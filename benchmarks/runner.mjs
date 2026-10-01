#!/usr/bin/env node

// Drives the throughput benchmarks and reports runs/second.
//
// Usage:
//   ./build.sh && node runner.mjs [options]
//
//   --runs N           fuzz runs per test (default 100, elm-test's own default).
//                      Try 1000 too: for a small-domain fuzzer the cost should
//                      grow with this, and shouldn't once exhaustive checking
//                      can stop early.
//   --reps N           timed repetitions per benchmark (default 5); the median
//                      is reported
//   --min-ms N         each repetition runs the test enough times to take at
//                      least this long, so the timer isn't the limiting factor
//                      (default 50)
//   --filter REGEX     only run benchmarks whose name matches
//   --seed N           fuzz seed (default 1)
//   --label NAME       recorded in the output
//   --out FILE         write the results as JSON
//   --compare FILE     diff against a saved result file
//   --markdown         emit markdown tables
//
// Reported as runs/sec (fuzz runs, not test invocations) so numbers stay
// comparable across different --runs settings. For a fuzzer whose domain is
// smaller than --runs, a future exhaustive implementation should show runs/sec
// going *up* with --runs, because the extra budget costs nothing.

import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));

function fail(message) {
  console.error(`benchmarks: ${message}`);
  process.exit(1);
}

function parseArgs(argv) {
  const options = {
    runs: 100,
    reps: 5,
    minMs: 50,
    filter: null,
    seed: 1,
    label: "current",
    out: null,
    compare: null,
    markdown: false,
  };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    const next = () => {
      const v = argv[++i];
      if (v === undefined) fail(`${arg} needs a value`);
      return v;
    };
    const nextInt = () => {
      const v = Number(next());
      if (!Number.isInteger(v) || v < 0) fail(`${arg} needs a non-negative integer`);
      return v;
    };
    switch (arg) {
      case "--runs": options.runs = nextInt(); break;
      case "--reps": options.reps = nextInt(); break;
      case "--min-ms": options.minMs = nextInt(); break;
      case "--seed": options.seed = nextInt(); break;
      case "--filter": options.filter = new RegExp(next()); break;
      case "--label": options.label = next(); break;
      case "--out": options.out = next(); break;
      case "--compare": options.compare = next(); break;
      case "--markdown": options.markdown = true; break;
      default: fail(`unknown option: ${arg}`);
    }
  }
  return options;
}

function startElm() {
  const require = createRequire(import.meta.url);
  const elmPath = path.join(here, "elm.js");
  if (!fs.existsSync(elmPath)) fail("elm.js not found -- run ./build.sh first");
  const { Elm } = require(elmPath);
  const app = Elm.Main.init();

  let pending = null;
  app.ports.toJs.subscribe((message) => {
    const resolve = pending;
    pending = null;
    if (resolve === null) fail(`unexpected message: ${JSON.stringify(message)}`);
    resolve(message);
  });

  return (request) =>
    new Promise((resolve) => {
      if (pending !== null) fail("request sent while another was in flight");
      pending = resolve;
      app.ports.fromJs.send(request);
    });
}

// One timed repetition: `iterations` whole test executions, returning ms elapsed.
// The Elm work happens synchronously inside the port handler, so the round trip
// is the work plus a microtask -- negligible once a repetition takes >= minMs.
async function timeOnce(send, index, options, iterations) {
  const started = performance.now();
  const result = await send({ tag: "run", index, runs: options.runs, seed: options.seed, iterations });
  const elapsed = performance.now() - started;
  if (result.tag !== "done") fail(`expected done, got ${JSON.stringify(result)}`);
  return elapsed;
}

async function measure(options) {
  const send = startElm();
  const manifest = await send({ tag: "manifest" });
  if (manifest.tag !== "manifest") fail(`expected a manifest, got ${JSON.stringify(manifest)}`);

  const selected = manifest.benchmarks
    .map((b, index) => ({ ...b, index }))
    .filter(({ name }) => options.filter === null || options.filter.test(name));
  if (selected.length === 0) fail("no benchmarks to run");

  const results = [];
  const startedAt = Date.now();
  for (const { name, passes, index } of selected) {
    // Grow the iteration count until a repetition clears --min-ms, so the timer
    // resolution isn't what we're measuring. Also serves as warmup.
    let iterations = 1;
    while ((await timeOnce(send, index, options, iterations)) < options.minMs) {
      iterations *= 2;
      if (iterations > 1 << 22) break;
    }

    const samples = [];
    for (let r = 0; r < options.reps; r++) {
      samples.push(await timeOnce(send, index, options, iterations));
    }
    samples.sort((a, b) => a - b);
    const medianMs = samples[Math.floor(samples.length / 2)];

    results.push({
      name,
      passes,
      iterations,
      medianMs,
      msPerTest: medianMs / iterations,
      // Fuzz runs per second, comparable across --runs settings. Null for a
      // failing test, which never executes its whole budget.
      runsPerSec: passes ? (iterations * options.runs) / (medianMs / 1000) : null,
      spreadPct: ((samples[samples.length - 1] - samples[0]) / medianMs) * 100,
    });
  }

  return {
    meta: {
      label: options.label,
      runs: options.runs,
      reps: options.reps,
      minMs: options.minMs,
      seed: options.seed,
      node: process.version,
      startedAt: new Date(startedAt).toISOString(),
      wallMs: Date.now() - startedAt,
    },
    results,
  };
}

let MARKDOWN = false;

function table(rows, headers, aligns) {
  if (MARKDOWN) {
    const esc = (c) => String(c).replace(/\|/g, "\\|");
    const row = (cells) => `| ${cells.map(esc).join(" | ")} |`;
    return [row(headers), `| ${aligns.map((a) => (a === "r" ? "---:" : ":---")).join(" | ")} |`, ...rows.map(row)].join("\n");
  }
  const widths = headers.map((h, i) => Math.max(h.length, ...rows.map((r) => String(r[i]).length)));
  const line = (cells) =>
    cells.map((c, i) => (aligns[i] === "r" ? String(c).padStart(widths[i]) : String(c).padEnd(widths[i]))).join("  ");
  return [line(headers), line(widths.map((w) => "-".repeat(w))), ...rows.map(line)].join("\n");
}

function si(n) {
  if (n >= 1e6) return `${(n / 1e6).toFixed(2)}M`;
  if (n >= 1e3) return `${(n / 1e3).toFixed(1)}k`;
  return n.toFixed(0);
}

function report(run) {
  const lines = [];
  lines.push(
    MARKDOWN
      ? `## Throughput: ${run.meta.label}\n\n--runs ${run.meta.runs}, ${run.meta.reps} reps, ${(run.meta.wallMs / 1000).toFixed(1)}s wall.`
      : `Throughput: ${run.meta.label} -- --runs ${run.meta.runs}, ${run.meta.reps} reps, ${(run.meta.wallMs / 1000).toFixed(1)}s wall`,
  );
  lines.push("");
  lines.push(
    table(
      run.results.map((r) => [
        r.name,
        r.runsPerSec === null ? "n/a" : si(r.runsPerSec),
        r.msPerTest.toFixed(4),
        String(r.iterations),
        `${r.spreadPct.toFixed(0)}%`,
      ]),
      ["benchmark", "runs/sec", "ms/test", "iters", "spread"],
      ["l", "r", "r", "r", "r"],
    ),
  );
  return lines.join("\n");
}

function compareReport(current, baseline) {
  const byName = new Map(baseline.results.map((r) => [r.name, r]));
  const rows = [];
  const ratios = [];
  for (const r of current.results) {
    const b = byName.get(r.name);
    if (b === undefined) {
      rows.push([r.name, "new", r.msPerTest.toFixed(4), "-"]);
      continue;
    }
    // Compare on ms/test, which is defined for passing and failing alike.
    const ratio = b.msPerTest / r.msPerTest;
    ratios.push(ratio);
    rows.push([r.name, b.msPerTest.toFixed(4), r.msPerTest.toFixed(4), `${ratio.toFixed(2)}x`]);
  }
  const geo = ratios.length === 0 ? null : Math.exp(ratios.reduce((s, v) => s + Math.log(v), 0) / ratios.length);
  const lines = [];
  lines.push(MARKDOWN ? `## Throughput: ${current.meta.label} vs ${baseline.meta.label}` : `Throughput: ${current.meta.label} vs ${baseline.meta.label}`);
  if (current.meta.runs !== baseline.meta.runs) {
    lines.push(`WARNING: --runs differs (${current.meta.runs} vs ${baseline.meta.runs}); runs/sec is comparable but ms/test is not.`);
  }
  lines.push("");
  lines.push(table(rows, ["benchmark", "ms/test (base)", "ms/test", "speedup"], ["l", "r", "r", "r"]));
  lines.push("");
  lines.push(`Geometric mean throughput ratio: ${geo === null ? "-" : geo.toFixed(3) + "x"} (> 1 is faster)`);
  return lines.join("\n");
}

const options = parseArgs(process.argv.slice(2));
MARKDOWN = options.markdown;

const run = await measure(options);
console.log(report(run));
if (options.out !== null) {
  fs.mkdirSync(path.dirname(options.out), { recursive: true });
  fs.writeFileSync(options.out, JSON.stringify(run, null, 2) + "\n");
  console.log(`\nWrote ${path.relative(process.cwd(), options.out)}`);
}
if (options.compare !== null) {
  console.log("");
  console.log(compareReport(run, JSON.parse(fs.readFileSync(options.compare, "utf8"))));
}
