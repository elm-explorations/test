# F-metric harness

Measures how good elm-test is at **finding bugs**, as opposed to how fast it runs
(that's `benchmarks/`).

The corpus is a set of fuzz tests with deliberately injected defects. For each
one, across many seeds, we record:

| metric | why |
| --- | --- |
| **time to a minimal counterexample** | the headline. What the developer actually waits for: detection _and_ simplification. |
| **cases to the defect** | the F-metric in its original sense: how many values the fuzzer generated before the property broke. Exact integers, and fully deterministic given the fixed seed list, so this is the axis with no measurement noise at all. |
| **detection rate at the budget** | whether the defect was found at all within `runs`. A strategy that finds a bug in 3 cases instead of 10 000 has won nothing if it's slower in wall-clock terms — but a strategy that stops finding the bug at all has lost something real. Only `cache/truncated-key` is tuned to sit below 100%, which is what makes this column informative. |
| **counterexample quality** | whether the value we report is the best possible one, plus the length and sum of the underlying `RandomRun` (the two components of the shortlex order that simplifying minimises). |

Time is the headline because it's what a developer actually experiences, and the
corpus shows why the two axes can't substitute for each other:
`needle/int-in-range` takes 370 cases but 0.12 ms, while `challenge/deletion`
takes 4 cases but 9 ms. Cases measure how well generation searches; time measures
what that search costs. Read them together — and prefer cases when they
disagree, because the case count is exact and the timings are not.

## Running

```sh
cd f-metric
./build.sh
node runner.mjs --seeds 100
```

`./build.sh` copies `../src` into a local `ELM_HOME` and compiles against it, so
you're always measuring the working copy. (The trick `benchmarks/` uses — putting
`../src` on `source-directories` — cannot work, because applications aren't
allowed to import `Elm.Kernel.*` modules. That's also why `benchmarks/` currently
doesn't compile.)

A 300-seed run over the default corpus takes about 30 seconds. `npm run f-metric`
from the repo root does the build and run in one step. Adding `--shrink-only`
makes it minutes rather than seconds, because `challenge/length-list` spends over
a second per measurement simplifying a 100-element list.

### Comparing two implementations

```sh
# on master
./build.sh && node runner.mjs --seeds 100 --label before --save-summary /tmp/before.json

# on your branch
./build.sh && node runner.mjs --seeds 100 --label after --compare /tmp/before.json
```

The comparison prints per-case detection-rate and time deltas, plus a geometric
mean of the time ratios (geometric, so that one slow case can't dominate).

Add `--markdown` to any report or comparison to get GitHub-flavoured tables for
pasting into a PR or issue.

`baseline/` holds committed summaries (the small per-case aggregates, not the raw
measurements) so you can diff against a known state without rerunning the other
side. `pr260-random.json` is today's purely-random generation, 300 seeds, budget
10 000, measured on an M-series Mac; `pr260-random-with-shrink.json` is the same
with `--shrink-only` included. **Absolute times depend on the machine**, so
for a real before/after, measure both sides yourself on one machine and use the
committed baseline only as a sanity check on the shape of the results — mainly
`found` and `optimal`, which are machine-independent.

## Reading the output

```
case                        found  med cases  p75   med ms   p75 ms  optimal  |run|  sum(run)
needle/int-in-range          100%        370  628     0.13     0.21     100%      2        43
```

- `found` — fraction of seeds that detected the defect within the budget.
- `med cases` / `p75` — values generated before the property broke. **The
  noise-free axis**: for a fixed seed list this is deterministic, so a change
  here is entirely attributable to the change under test, with no need to argue
  about measurement error.
- `med ms` / `p75 ms` — time to a minimal counterexample, **among the seeds that
  detected it**. A censored seed spent the entire budget and produced no
  counterexample; averaging that in as if it were a time-to-failure would be
  meaningless, so it isn't. Watch `found` and `med ms` together: a change that
  halves the time while dropping `found` from 100% to 60% is a regression.
- `optimal` — fraction of detections that reported the best possible
  counterexample. `-` means we haven't written that case's optimum down.
- `|run|`, `sum(run)` — median length and sum of the reported `RandomRun`. Lower
  is a simpler counterexample. These work even for cases with no known optimum,
  and they're directly comparable across strategies because they're the very
  quantities simplifying tries to minimise.

A case with `med cases` of 1 or 2 was falsified by the first value or two the
fuzzer produced, so it measures *only* simplification cost, not search quality.
Those cases are tagged `shrinkOnly` in the corpus and **excluded by default**;
`--shrink-only` puts them back. Removing them took the default run from 204
seconds to about 10 (at 100 seeds), almost all of it `challenge/length-list`.

## Methodology, and how much to trust a number

**The noise floor is measured, not guessed**, and the two axes are in completely
different regimes.

The **case count carries no noise at all**. Re-running an identical
configuration gives a case ratio of exactly 1.000x on every single case, at 100
and at 500 seeds — the seed list is fixed and generation is deterministic. Any
movement in `cases` is real, with no error bars to argue about. This is the axis
to compare on.

The **detection rate carries no run-to-run noise either**, for the same reason:
re-running gives byte-identical `found` values. But read it with one caveat the
case ratio doesn't need — it's a proportion over 300 seeds, so its sampling error
as an estimate of the *true* detection rate is about ±3 percentage points
(1 s.e.). A 2-3 point difference between two implementations is real on this seed
list but may not generalise; a 10 point difference is a result.

The **times cannot be compared across process invocations to better than about
±7%**, and more seeds do not fix it. Two back-to-back 500-seed runs of the
identical configuration came out uniformly 0.86x–1.03x, clustered around 0.93x:
not per-case scatter but a systematic whole-run drift, the second process simply
running faster than the first. Within a run the times are fine; between runs
they carry a bias you can't average away. Treat a time change under about 1.15x
per case, or 1.1x in aggregate, as nothing.

Cases whose median is under 0.5 ms are **excluded from the aggregate time ratio**
and flagged with `*`, because a single `performance.now()` tick is a double-digit
percentage of them — a 0.02 ms case reading 1.29x is not information. With the
shrink-only cases excluded that removes 11 of 18 cases from the time aggregate,
which leaves it resting on few enough cases to be fragile. Two consequences:
compare on `cases`, and if you specifically care about *simplification* time, run
with `--shrink-only`, since the slow cases all live there.

Things the harness does to keep comparisons honest, and which you shouldn't
undo:

- **Fixed seeds.** Seeds come from a fixed xorshift32 sequence, so every run
  measures the same seeds. Consecutive integers would be a poor choice —
  `Random.initialSeed` does little mixing, so nearby seeds can correlate.
- **Warmup.** The first few measurements of each case are discarded
  (`--warmup`), so a case isn't penalised for being the one that triggered JIT
  compilation.
- **Seed-major ordering.** Cases are interleaved rather than run to completion
  one at a time, so drift over a long run (thermal throttling, GC pressure) is
  spread across all cases instead of landing on whichever case ran last.
- **Same everything on both sides.** `--filter`, `--seeds`, `--warmup` and the
  budget all affect measured times — a filtered run measures each case as much as
  2.5x slower than the same case in a full run, purely because of how much other
  code the JIT saw. The comparison warns about mismatched budgets and seed
  counts, but it can't detect a mismatched `--filter`.
- **Dev mode, which is also production.** We don't compile with `--optimize`,
  and there's no option to. That isn't a readability compromise — it's what
  node-test-runner does: `lib/ElmCompiler.js` only ever passes `--output` and
  `--report json`. It has to, because under `--optimize` `Debug.toString`
  returns `"<internals>"` for _every_ value, plain `Int` included, so every
  counterexample elm-test reports would be unreadable. Dev mode is the
  configuration real test suites run in, so these timings are representative of
  it.

Known quirks of the current corpus:

- `cache/truncated-key` sits at **55% found** by design; everything else is at
  100%. Without it the `found` column would be a constant and tell you nothing.
  Its mask is tuned so a run collides with probability ~`28 / 2^19`, which over a
  10 000-run budget lands about half the seeds either side — so the axis responds
  in both directions. One mask bit is worth roughly a factor of two in `found`
  (18 bits measured 80%, 19 bits 55%), so retune the mask, never this case's
  budget, if it drifts to an extreme.
- Everything else detects on every seed at the default budget. That's deliberate
  headroom: `found` starts at 100% and has somewhere to fall if a change makes
  generation worse.
- `bst/delete-two-children` reports **1% optimal**. That's not a broken
  expectation — simplifying reliably finds the right four-node *shape* but never
  reduces the keys, stopping at `([5,0,7,6],5)` instead of `([1,0,3,2],1)`. It's
  the most sensitive simplification signal in the default corpus, and the 1%
  proves the target is reachable.
- With `--shrink-only`, `challenge/coupling` (53% optimal) and
  `structural/adjacent-dedupe` (32%) are the other two places simplifying
  reliably stops short.

## The corpus

Every case is a fuzz test with one deliberately injected defect. Cases are tagged
with a **role**: `Search` cases need the fuzzer to work to find the defect, so
their case counts and times mean something; `ShrinkOnly` cases are falsified by
the first value or two and only measure simplification, so they're excluded
unless you pass `--shrink-only`.

`src/Corpus/KnownBugs.elm` — data structures with one realistic mistake, and the
cases that carry most of the F signal. Each needs a *sequence* of operations that
interact, so no single value falsifies them:

| case | the defect | source |
| --- | --- | --- |
| `bst/delete-two-children` | deleting a two-child node replaces it with the right *child's* key instead of the right *subtree's minimum* — the same thing exactly when that child has no left subtree | the BST suite in John Hughes, [_How to Specify It!_](https://research.chalmers.se/publication/508940) (2019) |
| `rbt/missing-rotation` | Okasaki's `balance` with the left-right case removed; the other three still rebalance most insertions | Okasaki's red-black tree with the `balance` mutation set from [Etna](https://dl.acm.org/doi/10.1145/3591283) (Shi et al., PLDI 2023) |
| `intervals/touching-merge` | interval merge tests `lo < previousHi` where it needs `<=`, so intervals meeting at exactly one point aren't merged | classic bug class, not one named incident |
| `queue/missing-invariant` | Okasaki's batched queue where `pop` forgets to restore "front is empty only if the queue is", so a non-empty queue reports itself empty | classic bug class; the textbook motivation for model-based testing |
| `cache/truncated-key` | a cache keys entries on the low 19 bits of a 24-bit id, so two distinct ids collide and one silently overwrites the other | classic bug class. Also the corpus's **only case that isn't always found** — see below |

Two of these needed their input length bounded (`listOfLengthBetween 0 6`) to be
worth measuring. With `Fuzz.list`'s ~16 elements, a random 16-key sequence almost
always contains the trigger somewhere and the case is falsified by the very first
value — bounding the size makes *finding* the trigger the work, which is the
point, and it's how Etna's suites bound their inputs too.

`src/Corpus/Combined.elm` — cases where part of the domain is tiny and part is
huge, with the defect in the huge part. This is
[#188](https://github.com/elm-explorations/test/issues/188)'s motivating shape:
given `oneOf [map Err string, map Ok bool]`, half of every run is spent
re-testing `Ok True` and `Ok False`, so a defect in the `Err` branch is found at
half the rate it could be.

The three cases vary exactly one thing each, so a measurement says *which*
mechanism paid off rather than just that something did:

| case | `Err` payload | choice tree | helped by |
| :--- | :--- | :--- | :--- |
| `combined/open-payload` | `Fuzz.string` | infinite | duplicate rejection, or a per-subtree hybrid. **Not** all-or-nothing enumeration, which abandons an infinite tree and changes nothing. |
| `combined/finite-payload` | uniform `0..1023` | finite, 1026 leaves | plain exhaustive checking: guaranteed detection with zero variance |
| `combined/wide-easy-branch` | same, but `Ok` has 201 leaves | finite, 1225 leaves | separates leaf-level dedup from subtree-level exhaustion — dedup must *observe* all 201 `Ok` runs first (~`201*H(201)` ≈ 1200 draws) where an enumerator knows when the branch is covered |

`finite-payload` and `wide-easy-branch` have identical baselines by construction
(`Fuzz.bool` and `intRange 0 200` both consume exactly one draw, so the `Err`
sequence is bit-identical). They only diverge once a mechanism lands, which makes
them a matched pair.

`src/Corpus/Synthetic.elm` — cases shaped to probe one weakness each:

| category | what it rewards |
| --- | --- |
| `needle` | systematically covering a small domain |
| `boundary` | trying range edges and size edges deliberately |
| `structural` | producing a required *shape* (a repeat, a nesting) rather than a value |
| `deep` | reaching *large* inputs. **The category to watch** — anything that biases generation towards small inputs will look wonderful everywhere else and regress here. |
| `filter` | the rejection path in the fuzz loop |

`src/Corpus/ShrinkingChallenge.elm` — the
[shrinking-challenge](https://github.com/jlink/shrinking-challenge) suite, ported
from `tests/src/ShrinkingChallengeTests.elm`, which already encodes each
challenge as (fuzzer, falsifiable property, known-optimal counterexample). Keep
the two in sync. Seven of the twelve are `shrinkOnly`: they were designed to
stress *simplifying*, not detection, so the first generated value falsifies them.
The five that survive as search cases are `calculator`, `deletion` and the three
`difference` variants.

### Candidates not taken

Worth recording so nobody re-derives the dead ends:

- **Case-folding bugs** (the Turkish dotless `i`, German `ß` changing length
  under `toUpper`) are a famous, security-adjacent class, but unreachable here:
  `Fuzz.char` only draws from the full Unicode range 1/10 of the time, so a
  specific code point is about 1e-7 per character. The case would be censored
  forever and discriminate nothing.
- **The TimSort `mergeCollapse` bug** (de Gouw et al. 2015, which broke Java and
  Python) needs a specific run-length pattern across 60+ elements. Faithful
  replication is a lot of code for an input no random fuzzer will reach.

### Adding a case

See the docs on `Corpus.Case.fails`. Briefly: the property must be falsifiable,
the defect should be reachable but not trivially so, and `minima` should list the
counterexamples you'd consider optimal (as `Debug.toString` renders them). Leave
`minima` empty, run once, and read the `given` values out of the report to fill
it in.

Two traps, both of which this corpus hit:

- **Make sure the defect is reachable at all.** `Fuzz.string` is
  `stringOfLengthBetween 0 10`, so a property that only fails for strings longer
  than 10 characters is never falsifiable — it looks like a hard case but it's an
  impossible one. The report calls out cases that were never detected.
- **Keep `Fuzz.filter` predicates generous.** `Fuzz.filter` fails the whole test
  after 16 consecutive rejections, and over thousands of runs even a mildly
  selective predicate will hit that. A predicate rejecting 76% of values fails
  spuriously within a couple of thousand runs. Such a failure is _not_ a
  detection, and the harness reports it separately as "failed for the wrong
  reason" — that's what the `F-METRIC-VIOLATION` marker is for.

## Where the case count comes from

Reporting cases needs one number out of the library that wasn't previously
reachable: `Test/Fuzz.elm` tracks `runsElapsed` in its loop state, but it only
ever escaped to a runner inside a `DistributionReport`, i.e. only for tests that
asked for a distribution. This branch threads it through `RunResult` and
`Test.Expectation.FuzzTestFail` and exposes it as
`Test.RunnerV2.getFuzzTestFailRunsElapsed`.

That is a change to `src/`, and an addition to PR #260's public API, so it has to
land with or after #260. It's deliberately minimal — one field, one getter, no
behaviour change — and the existing 727-test suite passes unchanged.

Two things worth knowing about the number:

- It counts values *generated*, which includes ones the fuzzer rejected, so for
  cases using `Fuzz.filter` it's larger than the number of values the property
  actually saw. That's the right denominator for "how much work did the test
  system do".
- It can exceed the configured `runs`, because an `expectDistribution` test keeps
  generating until its statistical check settles.

An obvious follow-up, not done here: expose the same count for *passing* tests.
It's free now that `RunResult` carries it, and it would let a runner say "this
test actually ran 3072 times" for distribution-heavy tests.
