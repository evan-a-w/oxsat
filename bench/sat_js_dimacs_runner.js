const fs = require("fs");
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

const solverPath = argValue("--solver", "sat.js");
const dimacsPath = argValue("--dimacs", null);
const printStats = process.argv.includes("--stats");

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
