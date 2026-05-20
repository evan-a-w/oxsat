const fs = require("fs");
const path = require("path");
const vm = require("vm");

function argValue(name, fallback) {
  const idx = process.argv.indexOf(name);
  if (idx < 0) return fallback;
  if (idx + 1 >= process.argv.length) {
    throw new Error(`missing value for ${name}`);
  }
  return process.argv[idx + 1];
}

function parseDimacs(data) {
  const clauses = [];
  let size = 0;

  for (const raw of data.split(/\n/)) {
    const line = raw.trim();
    if (line === "" || line[0] === "c") continue;
    if (line[0] === "p") {
      const parts = line.split(/\s+/);
      size = Number(parts[2]);
      continue;
    }

    const literals = (line.match(/-?\d+/g) || [])
      .map(Number)
      .filter((n) => n !== 0);
    if (literals.length !== 0) clauses.push(literals);
  }

  return { size, clauses };
}

function argInt(name, fallback) {
  return Number(argValue(name, String(fallback)));
}

function timeOnce(f) {
  const start = process.hrtime.bigint();
  f();
  return Number(process.hrtime.bigint() - start);
}

function estimateIterations(config, f) {
  const elapsedNs = timeOnce(f);
  if (elapsedNs <= 0) return config.maxIterations;

  const estimated = Math.floor(config.targetTimeNs / elapsedNs);
  return Math.min(
    config.maxIterations,
    Math.max(config.minIterations, estimated)
  );
}

function timeIterations(iterations, f) {
  const start = process.hrtime.bigint();
  for (let i = 0; i < iterations; i++) {
    f();
  }
  return Number(process.hrtime.bigint() - start);
}

function calculateStats(samples) {
  samples.sort((a, b) => a - b);
  const n = samples.length;
  const mean = samples.reduce((acc, x) => acc + x, 0) / n;
  const variance =
    samples.reduce((acc, x) => {
      const diff = x - mean;
      return acc + diff * diff;
    }, 0) / n;
  const percentile = (p) => {
    const idx = Math.min(n - 1, Math.floor((n * p) / 100));
    return samples[idx];
  };

  return {
    mean,
    stddev: Math.sqrt(variance),
    p50: percentile(50),
    p75: percentile(75),
    p90: percentile(90),
    p95: percentile(95),
    p99: percentile(99),
    samples: n,
  };
}

function formatTime(ns) {
  if (ns < 1000) return `${ns.toFixed(2)} ns`;
  if (ns < 1000000) return `${(ns / 1000).toFixed(2)} μs`;
  if (ns < 1000000000) return `${(ns / 1000000).toFixed(2)} ms`;
  return `${(ns / 1000000000).toFixed(2)} s`;
}

function statsToString(stats) {
  return [
    `Mean: ${formatTime(stats.mean)}`,
    `StdDev: ${formatTime(stats.stddev)}`,
    `p50: ${formatTime(stats.p50)}`,
    `p75: ${formatTime(stats.p75)}`,
    `p90: ${formatTime(stats.p90)}`,
    `p95: ${formatTime(stats.p95)}`,
    `p99: ${formatTime(stats.p99)}`,
    `(n=${stats.samples})`,
  ].join("  ");
}

function benchmarkFunction(config, f) {
  for (let i = 0; i < config.warmupRuns; i++) {
    timeOnce(f);
  }

  const iterations = estimateIterations(config, f);
  const samples = [];
  for (let i = 0; i < config.sampleRuns; i++) {
    samples.push(timeIterations(iterations, f) / iterations);
  }

  return calculateStats(samples);
}

const solverPath = argValue("--solver", "bench/sat.js");
const dimacsPath = argValue("--dimacs", null);
const instanceName = argValue(
  "--name",
  dimacsPath == null ? "DIMACS" : path.basename(dimacsPath)
);
const printStats = process.argv.includes("--stats");
const benchmark = process.argv.includes("--benchmark");

if (dimacsPath == null) {
  throw new Error("missing --dimacs");
}

vm.runInThisContext(fs.readFileSync(solverPath, "utf8"), { filename: solverPath });

let decisions = 0;
let conflicts = 0;
let learned = 0;
let learnedClauseLiterals = 0;
let assignments = 0;

if (printStats) {
  const oldSelect = satSelectLiteral;
  satSelectLiteral = function (state) {
    const literal = oldSelect(state);
    if (literal !== 0) decisions++;
    return literal;
  };

  const oldBacktrack = satBacktrack;
  satBacktrack = function (state, reason) {
    conflicts++;
    const learnedClause = oldBacktrack(state, reason);
    if (learnedClause != null) {
      learned++;
      learnedClauseLiterals += learnedClause.length;
    }
    return learnedClause;
  };

  const oldSet = literalSet;
  literalSet = function (state, literal, reason) {
    assignments++;
    return oldSet(state, literal, reason);
  };
}

const { size, clauses } = parseDimacs(fs.readFileSync(dimacsPath, "utf8"));

if (benchmark) {
  const config = {
    warmupRuns: argInt("--warmup-runs", 1),
    sampleRuns: argInt("--sample-runs", 5),
    minIterations: argInt("--min-iterations", 1),
    maxIterations: argInt("--max-iterations", 1000000000),
    targetTimeNs: argInt("--target-time-ns", 1000000000),
  };
  const stats = benchmarkFunction(config, () => {
    satSolve(size, clauses);
  });
  console.log(`${instanceName}: ${statsToString(stats)}`);
  process.exit(0);
}

const start = process.hrtime.bigint();
const result = satSolve(size, clauses);
const elapsedMs = Number(process.hrtime.bigint() - start) / 1e6;

if (printStats) {
  console.log(
    JSON.stringify({
      result,
      elapsedMs,
      decisions,
      conflicts,
      learned,
      learnedClauseLiterals,
      assignments,
    })
  );
}
