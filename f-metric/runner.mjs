#!/usr/bin/env node

// Drives the compiled Elm harness (elm.js) and turns raw measurements into a
// report. All statistics live here rather than in Elm, so re-analysing a saved
// run never needs a recompile.
//
// Usage:
//   ./build.sh && node runner.mjs [options]
//
//   --seeds N          measurements per case (default 300)
//   --budget N         max fuzz runs per measurement (default 10000).
//                      A case can raise this for itself; both sides of a
//                      comparison must use the same value.
//   --filter REGEX     only run cases whose name matches
//   --shrink-only      also run the cases that measure only simplification
//                      (falsified by the first generated value, so they say
//                      nothing about search quality and are off by default)
//   --warmup N         discarded measurements per case, to let the JIT settle
//                      (default 3)
//   --label NAME       recorded in the output, shown in comparisons
//   --out FILE         write JSONL here (default results/<label>-<timestamp>.jsonl)
//   --save-summary F   also write the per-case aggregates as JSON. This is what
//                      a committed baseline should be -- it's small, and
//                      --compare accepts it.
//   --summarize FILE   don't measure, just report on a saved JSONL or summary
//   --compare FILE     report on this run and diff it against a saved
//                      JSONL or summary
//   --markdown         emit the report as GitHub-flavoured markdown tables,
//                      for pasting into a PR or issue
//   --quiet            no progress output

import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));

// ---------------------------------------------------------------- CLI

function parseArgs(argv) {
  const options = {
    seeds: 300,
    budget: 10000,
    warmup: 3,
    filter: null,
    shrinkOnly: false,
    label: "current",
    out: null,
    saveSummary: null,
    summarize: null,
    compare: null,
    markdown: false,
    quiet: false,
  };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    const next = () => {
      const value = argv[++i];
      if (value === undefined) {
        fail(`${arg} needs a value`);
      }
      return value;
    };
    const nextInt = () => {
      const value = Number(next());
      if (!Number.isInteger(value) || value < 0) {
        fail(`${arg} needs a non-negative integer`);
      }
      return value;
    };
    switch (arg) {
      case "--seeds":
        options.seeds = nextInt();
        break;
      case "--budget":
        options.budget = nextInt();
        break;
      case "--warmup":
        options.warmup = nextInt();
        break;
      case "--filter":
        options.filter = new RegExp(next());
        break;
      case "--shrink-only":
        options.shrinkOnly = true;
        break;
      case "--label":
        options.label = next();
        break;
      case "--out":
        options.out = next();
        break;
      case "--save-summary":
        options.saveSummary = next();
        break;
      case "--summarize":
        options.summarize = next();
        break;
      case "--compare":
        options.compare = next();
        break;
      case "--markdown":
        options.markdown = true;
        break;
      case "--quiet":
        options.quiet = true;
        break;
      case "--help":
      case "-h":
        console.log(usage());
        process.exit(0);
        break;
      default:
        fail(`unknown option: ${arg}`);
    }
  }
  return options;
}

function fail(message) {
  console.error(`f-metric: ${message}`);
  process.exit(1);
}

// The header comment block of this file (skipping the shebang).
function usage() {
  const body = [];
  for (const line of fs.readFileSync(fileURLToPath(import.meta.url), "utf8").split("\n")) {
    if (line.startsWith("#!")) continue;
    if (line.startsWith("//")) {
      body.push(line.replace(/^\/\/ ?/, ""));
    } else if (body.length > 0) {
      break;
    }
  }
  return body.join("\n").trim();
}

// ---------------------------------------------------------------- seeds

// A fixed xorshift32 sequence, so every run measures the same seeds and two
// runs are directly comparable. Consecutive integers would be a bad choice:
// elm/random's initialSeed does little mixing, so nearby seeds can correlate.
function seedList(count) {
  let state = 0x9e3779b9;
  const seeds = [];
  for (let i = 0; i < count; i++) {
    state ^= state << 13;
    state >>>= 0;
    state ^= state >>> 17;
    state ^= state << 5;
    state >>>= 0;
    seeds.push(state);
  }
  return seeds;
}

// ---------------------------------------------------------------- Elm bridge

function startElm() {
  const require = createRequire(import.meta.url);
  const elmPath = path.join(here, "elm.js");
  if (!fs.existsSync(elmPath)) {
    fail("elm.js not found -- run ./build.sh first");
  }
  const { Elm } = require(elmPath);
  const app = Elm.Main.init();

  let pending = null;
  app.ports.toJs.subscribe((message) => {
    const resolve = pending;
    pending = null;
    if (resolve === null) {
      fail(`unexpected message from Elm: ${JSON.stringify(message)}`);
    }
    resolve(message);
  });

  return (request) =>
    new Promise((resolve) => {
      if (pending !== null) {
        fail("sent a request while another was still in flight");
      }
      pending = resolve;
      app.ports.fromJs.send(request);
    });
}

// ---------------------------------------------------------------- measuring

async function measureAll(options) {
  const send = startElm();

  const manifest = await send({ tag: "manifest" });
  if (manifest.tag !== "manifest") {
    fail(`expected a manifest, got ${JSON.stringify(manifest)}`);
  }

  const broken = manifest.cases.filter((c) => c.error !== null);
  for (const c of broken) {
    console.error(`f-metric: corpus case "${c.name}" is broken: ${c.error}`);
  }

  const cases = manifest.cases.filter(
    (c) =>
      c.error === null &&
      (options.filter === null || options.filter.test(c.name)) &&
      (options.shrinkOnly || c.role !== "shrinkOnly"),
  );
  const skipped = manifest.cases.filter((c) => c.error === null && c.role === "shrinkOnly").length;
  if (!options.shrinkOnly && skipped > 0) {
    console.error(
      `f-metric: skipping ${skipped} shrink-only case(s); pass --shrink-only to include them`,
    );
  }
  if (cases.length === 0) {
    fail("no cases to measure");
  }

  const budgetFor = (c) => c.budget ?? options.budget;
  const seeds = seedList(options.seeds);
  const warmupSeeds = seedList(options.warmup + options.seeds).slice(options.seeds);

  const total = cases.length * (options.seeds + options.warmup);
  const showProgress = !options.quiet && process.stderr.isTTY;
  let done = 0;
  const progress = (what) => {
    done++;
    if (showProgress) {
      process.stderr.write(`\r\u001b[Kf-metric: ${done}/${total} ${what}`);
    }
  };

  // Warm up case by case, so each case's JIT state is settled before it is
  // measured. Discard the results.
  for (const c of cases) {
    for (const seed of warmupSeeds) {
      await send({ tag: "measure", index: c.index, seed, budget: budgetFor(c) });
      progress(`warmup ${c.name}`);
    }
  }

  // Measure seed-major, so that any drift over the run (thermal throttling, GC
  // pressure) is spread across all cases instead of penalising whichever case
  // happens to run last.
  const measurements = [];
  const startedAt = Date.now();
  for (const seed of seeds) {
    for (const c of cases) {
      const result = await send({
        tag: "measure",
        index: c.index,
        seed,
        budget: budgetFor(c),
      });
      if (result.tag !== "measurement") {
        fail(`expected a measurement, got ${JSON.stringify(result)}`);
      }
      measurements.push(result);
      progress(c.name);
    }
  }
  if (showProgress) process.stderr.write("\r\u001b[K");

  return {
    meta: {
      label: options.label,
      startedAt: new Date(startedAt).toISOString(),
      wallMs: Date.now() - startedAt,
      seeds: options.seeds,
      warmup: options.warmup,
      budget: options.budget,
      shrinkOnly: options.shrinkOnly,
      node: process.version,
      cases: cases.map((c) => c.name),
      brokenCases: broken.map((c) => ({ name: c.name, error: c.error })),
    },
    measurements,
  };
}

// ---------------------------------------------------------------- statistics

function quantile(sorted, q) {
  if (sorted.length === 0) return null;
  const position = (sorted.length - 1) * q;
  const low = Math.floor(position);
  const high = Math.ceil(position);
  if (low === high) return sorted[low];
  return sorted[low] + (sorted[high] - sorted[low]) * (position - low);
}

function summarize(run) {
  const byCase = new Map();
  for (const m of run.measurements) {
    if (!byCase.has(m.name)) {
      byCase.set(m.name, { name: m.name, category: m.category, all: [] });
    }
    byCase.get(m.name).all.push(m);
  }

  const cases = [...byCase.values()].map((entry) => {
    const detected = entry.all.filter((m) => m.status === "detected");
    const errors = entry.all.filter((m) => m.status === "error");
    const times = detected.map((m) => m.durationMs).sort((a, b) => a - b);
    const caseCounts = detected.map((m) => m.cases).sort((a, b) => a - b);
    const lengths = detected.map((m) => m.runLength).sort((a, b) => a - b);
    const sums = detected.map((m) => m.runSum).sort((a, b) => a - b);
    const scored = detected.filter((m) => m.optimal !== null && m.optimal !== undefined);
    return {
      name: entry.name,
      category: entry.category,
      n: entry.all.length,
      detected: detected.length,
      errors: errors.length,
      errorDescription: errors.length > 0 ? errors[0].description : null,
      detectionRate: detected.length / entry.all.length,
      // Only among detections: the time a censored run took is the budget, not
      // a time-to-failure, so averaging it in would be meaningless.
      medianMs: quantile(times, 0.5),
      p25Ms: quantile(times, 0.25),
      p75Ms: quantile(times, 0.75),
      // The F-metric proper: values generated before the defect surfaced.
      // Exact integers, so unlike the times these carry no timer noise.
      medianCases: quantile(caseCounts, 0.5),
      p75Cases: quantile(caseCounts, 0.75),
      medianRunLength: quantile(lengths, 0.5),
      medianRunSum: quantile(sums, 0.5),
      optimalRate: scored.length === 0 ? null : scored.filter((m) => m.optimal).length / scored.length,
      // What a censored run costs the developer: the full budget, spent for nothing.
      censoredMs: quantile(
        entry.all.filter((m) => m.status === "notDetected").map((m) => m.durationMs).sort((a, b) => a - b),
        0.5,
      ),
    };
  });

  cases.sort((a, b) => a.name.localeCompare(b.name));

  return withCategories({ meta: run.meta, cases });
}

function withCategories(summary) {
  const byCategory = new Map();
  for (const c of summary.cases) {
    if (!byCategory.has(c.category)) byCategory.set(c.category, []);
    byCategory.get(c.category).push(c);
  }
  return { ...summary, byCategory };
}

function geometricMean(values) {
  const usable = values.filter((v) => Number.isFinite(v) && v > 0);
  if (usable.length === 0) return null;
  return Math.exp(usable.reduce((sum, v) => sum + Math.log(v), 0) / usable.length);
}

// ---------------------------------------------------------------- reporting

function fmt(value, digits = 2) {
  if (value === null || value === undefined) return "-";
  return value.toFixed(digits);
}

function pct(value) {
  if (value === null || value === undefined) return "-";
  return `${(value * 100).toFixed(0)}%`;
}

// Set by --markdown. Affects table rendering and section headings only; the
// numbers are identical either way.
let MARKDOWN = false;

function table(rows, headers, aligns) {
  if (MARKDOWN) {
    // `|run|` as a header would otherwise end the cell early.
    const escape = (cell) => String(cell).replace(/\|/g, "\\|");
    const row = (cells) => `| ${cells.map(escape).join(" | ")} |`;
    return [
      row(headers),
      `| ${aligns.map((a) => (a === "r" ? "---:" : ":---")).join(" | ")} |`,
      ...rows.map(row),
    ].join("\n");
  }
  const widths = headers.map((h, i) =>
    Math.max(h.length, ...rows.map((r) => String(r[i]).length)),
  );
  const line = (cells) =>
    cells
      .map((cell, i) => (aligns[i] === "r" ? String(cell).padStart(widths[i]) : String(cell).padEnd(widths[i])))
      .join("  ");
  return [line(headers), line(widths.map((w) => "-".repeat(w))), ...rows.map(line)].join("\n");
}

function heading(text) {
  return MARKDOWN ? `### ${text}` : `${text}:`;
}

function note(text) {
  return MARKDOWN ? `> ${text}` : text;
}

function report(summary) {
  const lines = [];
  lines.push(
    MARKDOWN
      ? `## F-metric: ${summary.meta.label}\n\n${summary.meta.seeds} seeds/case, budget ${summary.meta.budget}, ${(summary.meta.wallMs / 1000).toFixed(1)}s wall${summary.meta.shrinkOnly ? ", shrink-only cases included" : ""}.`
      : `F-metric: ${summary.meta.label} -- ${summary.meta.seeds} seeds/case, budget ${summary.meta.budget}, ${(summary.meta.wallMs / 1000).toFixed(1)}s wall`,
  );
  lines.push("");
  lines.push(
    table(
      summary.cases.map((c) => [
        c.name,
        pct(c.detectionRate),
        fmt(c.medianCases, 0),
        fmt(c.p75Cases, 0),
        fmt(c.medianMs),
        fmt(c.p75Ms),
        c.optimalRate === null ? "-" : pct(c.optimalRate),
        fmt(c.medianRunLength, 0),
        fmt(c.medianRunSum, 0),
      ]),
      ["case", "found", "med cases", "p75", "med ms", "p75 ms", "optimal", "|run|", "sum(run)"],
      ["l", "r", "r", "r", "r", "r", "r", "r", "r"],
    ),
  );
  lines.push("");
  lines.push(heading("By category"));
  lines.push(
    table(
      [...summary.byCategory.entries()].map(([category, cases]) => [
        category,
        String(cases.length),
        pct(cases.reduce((sum, c) => sum + c.detectionRate, 0) / cases.length),
        fmt(geometricMean(cases.map((c) => c.medianCases)), 0),
        fmt(geometricMean(cases.map((c) => c.medianMs))),
      ]),
      ["category", "cases", "mean found", "geomean cases", "geomean med ms"],
      ["l", "r", "r", "r", "r"],
    ),
  );

  const useless = summary.cases.filter((c) => c.detected === 0);
  if (useless.length > 0) {
    lines.push("");
    lines.push(
      note(
        `Never detected at this budget (these cases can't distinguish anything -- raise the budget or make them easier): ${useless.map((c) => c.name).join(", ")}`,
      ),
    );
  }
  const errored = summary.cases.filter((c) => c.errors > 0);
  if (errored.length > 0) {
    lines.push("");
    lines.push(heading("Cases that failed for the wrong reason (not a detection -- fix the case)"));
    for (const c of errored) {
      lines.push(`  ${c.name}: ${c.errors}/${c.n} -- ${c.errorDescription}`);
    }
  }
  return lines.join("\n");
}

// Medians below this are at the resolution limit of performance.now(): a single
// timer tick is a double-digit percentage of them, so they get excluded from the
// aggregate time ratio (and flagged in the table) rather than being allowed to
// masquerade as signal. Their case-count ratios are still exact and still count.
//
// The test is on the *baseline* median alone, deliberately. Gating on either side
// would let the excluded set shift as the variant's own timings move, and then
// two variants measured against the same baseline would have their geomeans
// computed over different sets of cases -- which is not a comparison.
const TIME_FLOOR_MS = 0.5;

function compareReport(current, baseline) {
  const baselineByName = new Map(baseline.cases.map((c) => [c.name, c]));
  const rows = [];
  const timeRatios = [];
  const caseRatios = [];
  let belowFloor = 0;
  for (const c of current.cases) {
    const b = baselineByName.get(c.name);
    if (b === undefined) {
      rows.push([c.name, "new", "-", "-", "-", "-", "-", "-"]);
      continue;
    }
    const comparable = c.detected > 0 && b.detected > 0;
    const ratioOf = (now, before) => (before !== null && now !== null && before > 0 ? now / before : null);

    const caseRatio = ratioOf(c.medianCases, b.medianCases);
    if (caseRatio !== null && comparable) caseRatios.push(caseRatio);

    const timeRatio = ratioOf(c.medianMs, b.medianMs);
    const atFloor = (b.medianMs ?? 0) < TIME_FLOOR_MS;
    if (timeRatio !== null && comparable) {
      if (atFloor) belowFloor++;
      else timeRatios.push(timeRatio);
    }

    rows.push([
      c.name,
      pct(b.detectionRate),
      pct(c.detectionRate),
      fmt(b.medianCases, 0),
      fmt(c.medianCases, 0),
      caseRatio === null ? "-" : `${caseRatio.toFixed(2)}x`,
      `${fmt(b.medianMs)} -> ${fmt(c.medianMs)}`,
      timeRatio === null ? "-" : `${timeRatio.toFixed(2)}x${atFloor ? " *" : ""}`,
    ]);
  }

  const currentNames = new Set(current.cases.map((c) => c.name));
  const gone = baseline.cases.filter((c) => !currentNames.has(c.name)).map((c) => c.name);

  const lines = [];
  lines.push(
    MARKDOWN
      ? `## Comparison: ${current.meta.label} vs ${baseline.meta.label}`
      : `Comparison: ${current.meta.label} vs ${baseline.meta.label}`,
  );
  if (current.meta.budget !== baseline.meta.budget) {
    lines.push(
      `WARNING: budgets differ (${current.meta.budget} vs ${baseline.meta.budget}). Detection rates are not comparable.`,
    );
  }
  if (current.meta.seeds !== baseline.meta.seeds) {
    lines.push(`NOTE: seed counts differ (${current.meta.seeds} vs ${baseline.meta.seeds}).`);
  }
  if (gone.length > 0) {
    lines.push(`NOTE: in the baseline but not measured here: ${gone.join(", ")}`);
  }
  lines.push("");
  lines.push(
    table(
      rows,
      ["case", "found (base)", "found", "cases (base)", "cases", "cases ratio", "ms (base -> now)", "time"],
      ["l", "r", "r", "r", "r", "r", "r", "r"],
    ),
  );
  lines.push("");

  const caseGeo = geometricMean(caseRatios);
  lines.push(
    `Geometric mean CASE ratio over ${caseRatios.length} comparable cases: ${caseGeo === null ? "-" : caseGeo.toFixed(3) + "x"} (< 1 means fewer values generated to find the bug)`,
  );
  const geo = geometricMean(timeRatios);
  lines.push(
    `Geometric mean TIME ratio over ${timeRatios.length} comparable cases: ${geo === null ? "-" : geo.toFixed(3) + "x"} (< 1 is faster)`,
  );
  if (belowFloor > 0) {
    lines.push(
      `  (* ${belowFloor} case(s) excluded from the time ratio: median under ${TIME_FLOOR_MS}ms is at the timer's resolution limit. Their case ratios are still included.)`,
    );
  }
  const detectionDelta =
    current.cases.reduce((sum, c) => sum + c.detectionRate, 0) / current.cases.length -
    baseline.cases.reduce((sum, c) => sum + c.detectionRate, 0) / baseline.cases.length;
  lines.push(`Mean detection rate change: ${(detectionDelta * 100).toFixed(1)} percentage points (> 0 is better)`);
  return lines.join("\n");
}

// ---------------------------------------------------------------- JSONL

function writeJsonl(file, run) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  const lines = [JSON.stringify({ tag: "meta", ...run.meta })];
  for (const m of run.measurements) lines.push(JSON.stringify(m));
  fs.writeFileSync(file, lines.join("\n") + "\n");
}

function readJsonl(file) {
  const parsed = fs
    .readFileSync(file, "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "")
    .map((line) => JSON.parse(line));
  const meta = parsed.find((entry) => entry.tag === "meta");
  if (meta === undefined) fail(`${file} has no meta line`);
  return { meta, measurements: parsed.filter((entry) => entry.tag === "measurement") };
}

function writeSummary(file, summary) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(
    file,
    JSON.stringify({ tag: "summary", meta: summary.meta, cases: summary.cases }, null, 2) + "\n",
  );
}

// Accepts either a raw JSONL run or a saved summary, so a committed baseline can
// be the small summary rather than thousands of measurement lines.
function loadSummary(file) {
  const text = fs.readFileSync(file, "utf8");
  if (text.trimStart().startsWith("{") && text.includes('"tag": "summary"')) {
    return withCategories(JSON.parse(text));
  }
  return summarize(readJsonl(file));
}

// ---------------------------------------------------------------- main

const options = parseArgs(process.argv.slice(2));
MARKDOWN = options.markdown;

if (options.summarize !== null) {
  const summary = loadSummary(options.summarize);
  console.log(report(summary));
  if (options.saveSummary !== null) writeSummary(options.saveSummary, summary);
  if (options.compare !== null) {
    console.log("");
    console.log(compareReport(summary, loadSummary(options.compare)));
  }
} else {
  const run = await measureAll(options);
  const out =
    options.out ??
    path.join(here, "results", `${options.label}-${run.meta.startedAt.replace(/[:.]/g, "-")}.jsonl`);
  writeJsonl(out, run);
  const summary = summarize(run);
  console.log(report(summary));
  console.log("");
  console.log(`Wrote ${path.relative(process.cwd(), out)}`);
  if (options.saveSummary !== null) {
    writeSummary(options.saveSummary, summary);
    console.log(`Wrote ${path.relative(process.cwd(), options.saveSummary)}`);
  }
  if (options.compare !== null) {
    console.log("");
    console.log(compareReport(summary, loadSummary(options.compare)));
  }
}
