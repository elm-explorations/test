# Throughput benchmarks for elm-test

How fast elm-test gets through a test. For whether it's any *good* at finding
bugs, see `f-metric/`.

The two are complementary and neither substitutes for the other. The F-metric
corpus is entirely *failing* tests, so it structurally cannot measure the case
that dominates a real suite: a fuzz test that passes and therefore runs its whole
budget. That's also where exhaustive checking should pay off, by stopping as soon
as the input domain is covered.

## Running

```sh
cd benchmarks
./build.sh
node runner.mjs --runs 100
```

`./build.sh` copies `../src` into a local `ELM_HOME` and compiles against it, so
you always measure the working copy.

### Comparing two implementations

```sh
# on master
./build.sh && node runner.mjs --runs 1000 --label before --out /tmp/before.json

# on your branch
./build.sh && node runner.mjs --runs 1000 --label after --compare /tmp/before.json
```

Comparison is on `ms/test`, which is defined for passing and failing benchmarks
alike, and reports a geometric mean so one slow benchmark can't dominate. Use the
same `--runs` on both sides.

Add `--markdown` for tables you can paste into a PR.

## Reading the output

```
benchmark               runs/sec  ms/test  iters  spread
small/bool                 7.48M   0.0134   4096     14%
simplify/int                 n/a   0.0070   8192     13%
```

- `runs/sec` — fuzz runs per second, so it stays comparable across `--runs`
  settings. **`n/a` for failing benchmarks**: they stop at the first value
  instead of executing the whole budget, so dividing by `--runs` would overstate
  them by orders of magnitude. Read `ms/test` for those.
- `ms/test` — wall time for one whole test, budget included.
- `iters` — how many times the test was run per timed repetition, grown
  automatically until a repetition clears `--min-ms` so the timer isn't what's
  being measured. Doubles as warmup.
- `spread` — max-to-min across repetitions, as a percentage of the median.
  Currently 1–20%; treat anything under ~1.2x as noise.

## The benchmark groups

| group | why |
| :--- | :--- |
| `small/` | domains small enough to enumerate exhaustively. **The early-termination opportunity.** |
| `mid/` | finite but not trivially so — these decide where a budget boundary should sit |
| `large/` | unbounded domains. **The regression guard**: an enumeration probe or deduplication bookkeeping is pure overhead here |
| `filter/` | the rejection path, which deduplication and enumeration both have to handle |
| `simplify/` | failing tests, so simplification cost rather than generation throughput |

## The measurement that motivates exhaustive checking

Cost scales linearly with the budget even when the domain has a single element:

| benchmark | domain size | ms/test @ `--runs 100` | @ `--runs 1000` | scaling |
| :--- | ---: | ---: | ---: | ---: |
| `small/unit` | 1 | 0.0069 | 0.0492 | 7.1x |
| `small/bool` | 2 | 0.0134 | 0.1077 | 8.0x |
| `small/order` | 3 | 0.0207 | 0.1972 | 9.5x |
| `small/pair-bool` | 4 | 0.0243 | 0.2094 | 8.6x |
| `small/intRange-0-20` | 21 | 0.0166 | 0.1341 | 8.1x |
| `large/string` | unbounded | 0.3016 | 2.9618 | 9.8x |
| `large/list-int` | unbounded | 0.7599 | 8.2087 | 10.8x |

`Fuzz.unit` has exactly one possible input and we test it a thousand times. The
recoverable factor is `runs / domainSize`, so it grows without bound in whatever
`--fuzz` the user passes — and unlike deduplicating already-tested runs, stopping
early avoids *generating* the redundant values too, which is where the cost
actually is.

The `large/` rows scale linearly for real reasons and have nothing to recover.
They're the rows to watch for regressions.

## Notes on the implementation

- **Headless, not browser-based.** This used to be an
  [elm-explorations/benchmark](https://github.com/elm-explorations/benchmark)
  browser program, which meant it ran in CI never and produced numbers only when
  a human clicked something. The statistics here are simple enough to do in
  `runner.mjs`.
- **No library internals.** It used to destructure
  `Test.Internal.ElmTestVariant__FuzzTest`, which broke when that variant changed
  shape. It now uses the public `Test.Runner.fromTest`, whose
  `run : () -> List Expectation` is a plain function — which is what makes it
  benchmarkable, since `Test.RunnerV2.runFuzzTest` returns a `Task` and a Task
  can't be timed from inside Elm.
- **`source-directories` can't be used to pull in `../src`.** Applications aren't
  allowed to import `Elm.Kernel.*` modules, so that approach stopped compiling as
  soon as `RandomRun` moved into the kernel. Hence the local `ELM_HOME`, the same
  trick `tests/` and `f-metric/` use.
- `build.sh` clears `elm-stuff`, because Elm caches compiled dependency artifacts
  per package *version* and the version never changes as `../src` is edited.
  Without that, a build silently reuses a stale copy of the library.

## Benchmarking a real test suite

These microbenchmarks won't tell you what a fifty percent improvement here does
to a real suite, where the test bodies do real work. To check that, point a real
suite at your modified `src`:

- In the suite's `elm.json`, remove the `elm-explorations/test` dependency, add
  elm-test's own dependencies, and add the path to your `src` to
  `source-directories`.
- Run the suite once to compile, then time it.

The library's own `tests/` suite works as an independent workload too: build once
with `bash tests/run-tests.sh`, then time `node tests/elm.js` repeatedly. Don't
use `NOCLEANUP=1` for that — same stale-artifact trap as above.
