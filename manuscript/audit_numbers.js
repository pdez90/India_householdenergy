#!/usr/bin/env node
/* ---------------------------------------------------------------------------
 * audit_numbers.js -- find numbers in the manuscript that the analysis outputs
 * do not account for.
 *
 *   cd <outputs> && node audit_numbers.js            # audits ./build_manuscript.js
 *   node audit_numbers.js --dir /path/to/outputs
 *
 * Two independent passes, because there are two distinct failure modes and a
 * single check catches neither well.
 *
 *   PASS 1 -- HARD-CODED LITERALS.  Every number typed into a prose string in
 *   build_manuscript.js. These are the dangerous ones: they render perfectly,
 *   they never warn, and they stop being true the moment an analysis re-runs.
 *   A number that arrives through " + expr + " is by construction current, so
 *   only literals are reported. Section numbers, years, reference markers,
 *   thresholds that are definitions rather than results, and a short list of
 *   deliberate pins are excluded -- each with a reason, below.
 *
 *   PASS 2 -- TRACEABILITY.  Every number of three or more significant digits
 *   printed in the built .docx, checked against the union of every value in
 *   every CSV the pipeline wrote (at each rounding from 0 to 4 decimals, and
 *   allowing the x10 per-10-unit rescaling the health tables use). A number
 *   that matches nothing is either derived by arithmetic in the builder, which
 *   is legitimate, or stale, which is not -- so this pass RANKS rather than
 *   judges, and the output is meant to be read.
 *
 * Exit status is 1 if PASS 1 finds an unexplained literal, so this can gate a
 * publication run; PASS 2 never fails the build.
 * ------------------------------------------------------------------------- */
const fs = require("fs"), path = require("path");

const argDir = (() => {
  const i = process.argv.indexOf("--dir");
  return i > 0 ? process.argv[i + 1] : process.cwd();
})();
const DIR = path.resolve(argDir);
const JS  = path.join(DIR, "build_manuscript.js");
if (!fs.existsSync(JS)) { console.error("no build_manuscript.js in " + DIR); process.exit(2); }

/* --------------------------------------------------------------------------
 * Deliberate pins. Each entry must say WHY the number cannot come from a CSV.
 * Adding one is a decision, so it is recorded here rather than in a comment
 * beside the sentence, where it would be invisible to this audit.
 * ----------------------------------------------------------------------- */
const PINNED = [
  { re: /burn-in 1,000|burn-in 4,000,?|thinning 10\b|thinning 40\b/, why: "prespecified run lengths of the abandoned imputation pilot (docs/mime_v2_spec.md Section 4.2), settings rather than results" },
  { re: /R-hat (at most|<=) 1\.05|ESS at least 100/, why: "prespecified convergence thresholds (docs/mime_v2_spec.md Sections 4-5), definitions rather than results" },
  { re: /\b24,000\b|\bfour Hamiltonian\b/, why: "MCMC sampler settings -- now DERIVED from calibration_sampler.csv; this entry covers only the fallback literal for an output tree written before 05 exported them" },
  { re: /\b86%\b/,   why: "ACCESS 2018 re-interview rate, attributed to Jain et al. 2018 (ACCESS 2018 report), not derivable from the released panel" },
  { re: /\b93 kg\b/, why: "prior ACCESS analysis (Gould et al. 2020), an external published figure" },
  { re: /\b140 mmHg\b|\b90 mmHg\b|\bJNC7\b/, why: "JNC7 hypertension definition, a protocol constant" },
  { re: /\b126 mg\/dL\b|\b200 mg\/dL\b|\b15-49\b|\b15-54\b/, why: "adult-outcome definitions carried over from the cvd_load.RData construction code (glucose cut-offs, respondent age ranges); definitions, not results" },
  { re: /\b15-49\b|\b15-54\b/, why: "NFHS eligibility ages, a survey design constant" },
  { re: /\b2 km\b|\b5 km\b|\b10 km\b/, why: "DHS cluster displacement policy, an external data-release constant" },
  { re: /\b120 months\b|\b36-month\b|\bten years\b/, why: "analysis window definitions, chosen not estimated" },
  { re: /\b640 districts\b|\b707 districts\b/, why: "administrative frame sizes, fixed by the census/NSS frames" },
  { re: /\b25-30 households\b/, why: "NFHS sampling protocol, a design constant" },
  { re: /\b103 million\b|\bUjjwala\b/, why: "PMUY connections released, an external government figure with a citation" },
  { re: /\b111 million\b/, why: "household-air-pollution DALYs in 2021 from the GBD 2021 analysis (Bennitt et al. 2025), an external published figure with a citation" },
  { re: /\b500 million individuals\b/, why: "population of the six ACCESS states, an external census figure" },
  { re: /8,568|8,566/, why: "household counts in the published ACCESS report and the standalone microdata release; the sentence exists to CONTRAST them with our derived count, so they must be external" },
  { re: /_prep_access\.R|_prep_ires\.R|_correction\.R|_health_models\.R|_design_analysis\.R/, why: "script filenames, not quantities" },
  { re: /the reference-side logit standard error to the interval|bounded away from 0 and 1 by/, why: "bounds chosen in 05_correction.R, configuration rather than result (their effect IS derived: n_x_at_floor is exported)" },
  { re: /worked example|whose IRES rural prevalence is|the log odds of|divided by the product of|Substituting the effective sample size/, why: "the step-by-step worked arithmetic of SI Methods S1, whose purpose is to let a reader follow one district's delta-method calculation by hand" },
  { re: /\b168 households per district\b|\babout 96 in IRES\b/, why: "ACCESS and IRES per-district design targets; the REALIZED counts beside them are derived" },
  { re: /\b200 draws\b|\b200 imputations\b|\bsame 200 draws\b/, why: "M, the number of imputations, set in H2_health_models.R and 22_design_analysis.R; an analysis setting, not an estimate" },
];

/* Numbers that are never results, wherever they appear. */
const STRUCTURAL = [
  /^(19|20)\d\d$/,              // years
  /^\d{1,2}\.\d(\.\d)?$/,       // section numbers: 2.4.3, S9.20
  /^(0|1)$/, /^0\.05$/, /^1\.96$/, /^95$/, /^100$/, /^0\.01$/,
  /^[1-9]\d?$/,                 // small integers: counts of panels, list positions
  /^\d{1,2}\.\d\.\d$/,          // deeper section numbers
];

const text = fs.readFileSync(JS, "utf8");

/* ---- PASS 1 ------------------------------------------------------------- */
/* Prose strings only: the first argument of p(), r() and caption(). Table cell
 * builders and column widths are code, not prose, and their integers are
 * layout, not results. */
const proseStrings = [];
const CALL = /\b(?:p|r|caption|h1|h2|h3)\(\s*"/g;
let m;
while ((m = CALL.exec(text)) !== null) {
  // Walk the string literal from the opening quote, honouring escapes, and
  // keep collecting across " + ... + " concatenation so a sentence split by an
  // interpolation is audited as one sentence.
  let i = CALL.lastIndex, depth = 0, buf = "", inStr = true, out = "";
  for (; i < text.length; i++) {
    const c = text[i];
    if (inStr) {
      // Escapes: \uXXXX must be decoded, or \u0027 leaks "0027" into the prose
      // and is then reported as a hard-coded number.
      if (c === "\\") {
        const nx = text[i + 1];
        if (nx === "u") { buf += String.fromCharCode(parseInt(text.substr(i + 2, 4), 16)); i += 5; }
        else { buf += nx === "n" ? " " : nx; i++; }
        continue;
      }
      if (c === '"') { inStr = false; out += buf; buf = ""; continue; }
      buf += c;
    } else {
      // depth > 0 means we are inside a nested call such as
      // iresWin("pct_before_march_2020", "98.0") -- those strings are argument
      // values and identifiers, not prose, and collecting them reported both
      // the fallback constant and the quantity name as typed numbers.
      if (c === '"') {
        if (depth > 0) { for (i++; i < text.length; i++) { if (text[i] === "\\") { i++; continue; } if (text[i] === '"') break; } continue; }
        inStr = true; continue;
      }
      if (c === "(" || c === "[") depth++;
      else if (c === ")" || c === "]") { if (depth === 0) break; depth--; }
      else if (c === "," && depth === 0) break;
      // everything between strings is an expression: mark it so its digits are
      // not mistaken for typed prose
      out += (c === "+") ? "" : "";
    }
  }
  proseStrings.push({ index: CALL.lastIndex, text: out });
}

// A number token, but NOT one welded to a word or to another number by a
// hyphen. "NFHS-4", "2014-15", "PM2.5", "COVID-19" and "Sections 2.4.5-2.4.6"
// are names and ranges, not results, and matching a bare /-?\d+/ inside them
// produced a page of false positives on the first run of this audit.
const NUM = /(?<![A-Za-z0-9.\-])\d[\d,]*(?:\.\d+)?(?![A-Za-z0-9.\-])/g;
const lineOf = (idx) => text.slice(0, idx).split("\n").length;
const findings = [];
for (const ps of proseStrings) {
  const seen = new Set();
  let n;
  while ((n = NUM.exec(ps.text)) !== null) {
    const tok = n[0];
    if (seen.has(tok + "@" + n.index)) continue;
    const bare = tok.replace(/,/g, "");
    // A number right after the word "version" is a VERSION, not a section
    // reference, however much "4.5" looks like one. The structural rule below
    // exempts x.y as a section number, which is how a wrong R version survived
    // in a document whose every run used a different one.
    //
    // These two MUST be declared above the STRUCTURAL test that reads
    // isVersion. They were not, and `const` bindings are in the temporal dead
    // zone until their declaration, so the audit threw a ReferenceError on the
    // first number it examined. `node --check` cannot see that -- the program
    // is syntactically valid -- and build_docx.R reads a non-zero exit as
    // "hard-coded literals found", so under STRICT_CHECKS this would have
    // stopped a clean pipeline run at its very last step. Run the audit, do not
    // just parse it.
    const before = ps.text.slice(Math.max(0, n.index - 24), n.index).toLowerCase();
    const isVersion = /\bversion\s*$/.test(before) || /\bv\.?\s*$/.test(before);
    if (!isVersion && STRUCTURAL.some((r) => r.test(bare))) continue;
    const ctxt = ps.text.slice(Math.max(0, n.index - 70), n.index + 70);
    // Match a pin against the local context FIRST (so a narrow pin stays
    // narrow), then against the whole paragraph -- a worked arithmetic example
    // declares itself in its opening sentence and every number after it in that
    // paragraph belongs to the same example.
    const pin = PINNED.find((p) => p.re.test(ctxt)) || PINNED.find((p) => p.re.test(ps.text));
    findings.push({ line: lineOf(ps.index), num: tok, pinned: pin ? pin.why : null,
                    context: ctxt.replace(/\s+/g, " ").trim() });
  }
}
const unexplained = findings.filter((f) => !f.pinned);

console.log("=".repeat(78));
console.log("PASS 1 -- hard-coded numeric literals in manuscript prose");
console.log("=".repeat(78));
if (!unexplained.length) {
  console.log("  none: every number in prose is either interpolated from an output "
            + "file,\n  structural (a year, a section number, a definitional threshold), "
            + "or a\n  declared pin.");
} else {
  for (const f of unexplained)
    console.log(`  [LITERAL] build_manuscript.js:${f.line}  ${f.num}\n            ...${f.context}...`);
}
const pins = findings.filter((f) => f.pinned);
if (pins.length) {
  console.log("\n  Declared pins (intentional, each with a stated reason):");
  const byWhy = new Map();
  for (const f of pins) byWhy.set(f.why || f.pinned, (byWhy.get(f.pinned) || 0) + 1);
  for (const [why, k] of byWhy) console.log(`    x${k}  ${why}`);
}

/* ---- PASS 2 ------------------------------------------------------------- */
const csvValues = new Set();
const addVal = (v) => {
  const x = Number(v);
  if (!isFinite(x)) return;
  for (const s of [x, x * 10, x * 100, x / 10]) {
    for (let d = 0; d <= 4; d++) csvValues.add(Math.abs(s).toFixed(d));
  }
};
const walk = (dir, depth) => {
  if (depth > 2) return;
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const q = path.join(dir, e.name);
    // public/ holds the redistributable repackaging of these same outputs
    // (make_public_district_estimates.R). Every value in it is a copy or a
    // transformation of something already in the pool, except the
    // design-weighted-input calibration, which the manuscript does not print.
    // Indexing it can therefore only manufacture false matches for PASS 2 --
    // never supply a true one -- so the pool stays strictly upstream of the
    // document, as it does for release_build.csv below.
    if (e.isDirectory()) {
      if (depth === 0 && /^public$/i.test(e.name)) continue;
      walk(q, depth + 1); continue;
    }
    if (!/\.csv$/i.test(e.name)) continue;
    // release_build.csv DESCRIBES the document this pass is auditing -- it holds
    // the .docx md5s and byte sizes, written moments earlier by build_docx.R.
    // Indexing it would let the document be validated against a file derived
    // FROM the document, which is the same shape as the stale-builder bug: an
    // artifact certifying itself. Nothing in it is currently printed in the
    // manuscript, so this excludes no legitimate match; it keeps the pool
    // strictly upstream of the build.
    if (/^release_build\.csv$/i.test(e.name)) continue;
    const rows = fs.readFileSync(q, "utf8").split("\n");
    for (const row of rows) for (const cell of row.split(",")) addVal(cell.replace(/"/g, ""));
  }
};
walk(DIR, 0);

const docxText = (file) => {
  if (!fs.existsSync(file)) return null;
  // Minimal docx reader: the package's own XML, no dependency.
  const buf = fs.readFileSync(file);
  const { execSync } = require("child_process");
  try {
    return execSync(`unzip -p ${JSON.stringify(file)} word/document.xml`,
                    { maxBuffer: 1 << 28 }).toString()
      .replace(/<w:p\b[^>]*>/g, "\n").replace(/<[^>]+>/g, "")
      .replace(/&#(\d+);/g, (_, d) => String.fromCharCode(+d))
      .replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">");
  } catch (e) { return null; }
};

console.log("\n" + "=".repeat(78));
console.log("PASS 2 -- numbers printed in the .docx with no matching output value");
console.log("=".repeat(78));
console.log(`  (${csvValues.size.toLocaleString()} distinct values indexed from the CSVs in ${DIR})`);
let checked = 0, unmatched = [];
for (const f of ["ACCESS_Health_main.docx", "ACCESS_Health_SI.docx"]) {
  const t = docxText(path.join(DIR, f));
  if (t === null) { console.log(`  ${f}: not built, skipped`); continue; }
  const seen = new Set();
  let n;
  // The reference list is bibliography, not results: volume, issue, page and
  // article-ID numbers match nothing in the outputs and never will.
  const refAt = t.search(/\n\s*References\s*\n/);
  const body = refAt > 0 ? t.slice(0, refAt) : t;
  const RX = /-?\d[\d,]*\.\d+|\b\d{3,}\b/g;
  while ((n = RX.exec(body)) !== null) {
    const bare = n[0].replace(/,/g, "");
    const x = Math.abs(Number(bare));
    if (!isFinite(x)) continue;
    const digits = bare.replace(/[-.]/g, "").replace(/^0+/, "").length;
    if (digits < 3) continue;
    if (/^(19|20)\d\d$/.test(bare)) continue;
    if (seen.has(bare)) continue; seen.add(bare);
    checked++;
    const dp = (bare.split(".")[1] || "").length;
    if (csvValues.has(x.toFixed(dp))) continue;
    const i = n.index;
    unmatched.push({ file: f, num: n[0], context: body.slice(Math.max(0, i - 60), i + 60).replace(/\s+/g, " ").trim() });
  }
}
console.log(`  ${checked} distinct 3+-significant-digit numbers checked; `
          + `${unmatched.length} without a direct match.`);
for (const u of unmatched)
  console.log(`  [TRACE] ${u.file}  ${u.num}\n          ...${u.context}...`);
console.log("\n  A number here is not necessarily wrong: the builder computes ratios,");
console.log("  percentages and differences that no CSV stores directly. Read each one");
console.log("  and confirm it is derived rather than typed.");

process.exit(unexplained.length ? 1 : 0);
