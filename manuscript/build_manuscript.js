// Builds ACCESS_Health_draft_v3.docx = v2 (Intro + Data & Methods) with
// updated Methods details + full Results section with actual numbers/figures.
// Strategy: re-run the v2 builder content with amendments, then Results.
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, ImageRun,
} = require("docx");
const fs = require("fs");

// Where figures are read from, and where the two .docx files are written.
// BOTH default to the folder this builder lives in -- which is the analysis
// output folder, since build_manuscript.js is kept alongside the outputs it
// documents. Neither is ever a hardcoded absolute path: a literal path is how
// this build came to write into a directory that existed only on the machine
// the script was authored on, failing with ENOENT everywhere else. __dirname is
// correct wherever the file is run from and does not depend on the caller's
// working directory. FIG and OUT can still be overridden by environment
// variables (build_docx.R sets FIG) for the case where figures or outputs are
// deliberately kept somewhere else.
const withSlash = (d) => String(d).replace(/\/*$/, "/");
const FIG = withSlash(process.env.FIG || __dirname);
const OUT = withSlash(process.env.OUT || __dirname);
const FONT = "Times New Roman";
const SZ = 24, LINE = 300;

// ---- Reference numbering: order of first citation in the main text ----------
// The reference list below (refs) is kept in its historical order, and the prose
// cites those historical numbers. Journals using numbered (Vancouver-style)
// citations require numbering by order of first citation, so every citation is
// renumbered as its text run is created, and the list is emitted in REF_ORDER.
// REF_ORDER lists the historical numbers in order of first citation in the main
// text, then references cited only in the SI. After writing the main document
// the build re-reads it and FAILS if first citations are not 1, 2, 3, ... in
// order, so a new or moved citation cannot silently break the numbering.
const REF_ORDER = [5, 6, 7, 1, 46, 2, 3, 4, 38, 35, 36, 9, 30, 31, 8, 32, 34, 37, 10, 11, 12,
  13, 14, 15, 16, 17, 44, 45, 23, 18, 21, 22, 41, 24, 25, 19, 26, 43, 20, 42, 27, 40, 28,
  29, 47, 33, 39];
const REF_NEW = {};
REF_ORDER.forEach((o, i) => { REF_NEW[o] = i + 1; });
const CITE = (t) => (typeof t !== "string" ? t
  : t.replace(/\[(\d+(?:\s*,\s*\d+)*)\]/g, (m, inner) => {
      const ns = inner.split(/\s*,\s*/).map(Number);
      if (!ns.every((n) => REF_NEW[n])) return m;
      return "[" + ns.map((n) => REF_NEW[n]).sort((a, b) => a - b).join(", ") + "]";
    }));

const p = (text, opts = {}) =>
  new Paragraph({
    spacing: { line: LINE, after: 160 },
    alignment: AlignmentType.JUSTIFIED, ...opts.par,
    children: Array.isArray(text) ? text
      : HAS_EM.test(text) ? emphasize(text, opts.run)
      : [new TextRun({ text: CITE(text), font: FONT, size: SZ, ...opts.run })],
  });
const r = (t, e = {}) => new TextRun({ text: CITE(t), font: FONT, size: SZ, ...e });

// Emphasis is written inline as {{word}} and rendered in italics. It used to be
// written in capitals, which reads as shouting in a journal and cannot be
// mirrored into the reformatted paper/ documents as a formatting change. p()
// and everything built on it (caption(), the SI note paragraphs) pick the
// markers up automatically, so the emphasis lives in the sentence rather than
// in the call structure. HAS_EM is deliberately not /g: a global regex carries
// lastIndex across .test() calls and would match every other paragraph.
const HAS_EM = /\{\{/;
const emphasize = (t, e = {}) =>
  t.split(/\{\{(.*?)\}\}/)
   .map((piece, i) => (piece === "" ? null
                       : r(piece, i % 2 ? { ...e, italics: true } : e)))
   .filter(Boolean);
const h1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 }, children: [r(t, { bold: true, size: 28 })] });
const h2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 240, after: 120 }, children: [r(t, { bold: true, size: 26 })] });
const h3 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 200, after: 100 }, children: [r(t, { bold: true, italics: true, size: SZ })] });
const caption = (t) => p(t, { par: { alignment: AlignmentType.LEFT, spacing: { line: 240, after: 240 } }, run: { size: 20, bold: true } });
const img = (file, w, h) => new Paragraph({
  alignment: AlignmentType.CENTER, spacing: { before: 120, after: 60 },
  children: [new ImageRun({ type: "jpg", data: fs.readFileSync(FIG + file),
    transformation: { width: w, height: h } })],
});

// ---------- generic table builder ----------
function mkTable(colw, rows, { headerShade = true } = {}) {
  const cell = (t, { bold = false, shade = false, w = 0 } = {}) =>
    new TableCell({
      width: { size: colw[w], type: WidthType.DXA },
      shading: shade ? { type: ShadingType.CLEAR, fill: "E8EDF3" } : undefined,
      margins: { top: 50, bottom: 50, left: 90, right: 90 },
      children: [new Paragraph({ spacing: { line: 230 }, children: [r(String(t), { bold, size: 19 })] })],
    });
  return new Table({
    columnWidths: colw,
    width: { size: colw.reduce((a, b) => a + b), type: WidthType.DXA },
    rows: rows.map((cells, i) => new TableRow({
      children: cells.map((t, j) => cell(t, { bold: i === 0, shade: headerShade && i === 0, w: j })),
    })),
  });
}

// ---------- read numbers back out of the analysis outputs ----------
// A number that the analysis already wrote to disk should never be re-typed
// into the manuscript by hand. Hand-copied numbers are how a document comes to
// quote a correlation the current code no longer produces: the figure is
// regenerated, the caption is not, and nothing in either file knows they have
// come apart. Everything below reads the CSV the analysis script wrote, so a
// re-run changes the document, and a missing or degenerate value shows up in
// the text as "NA" instead of silently keeping a stale number alive.
//
// FIG is the analysis output folder (DIR_OUT); the CSVs sit in it or in its
// diagnostics/ subfolder.
function splitCsv(line) {
  const out = []; let cur = "", q = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (q) {
      if (ch === '"') { if (line[i + 1] === '"') { cur += '"'; i++; } else q = false; }
      else cur += ch;
    } else if (ch === '"') q = true;
    else if (ch === ",") { out.push(cur); cur = ""; }
    else cur += ch;
  }
  out.push(cur);
  return out;
}
// A CSV that an ANALYSIS NUMBER comes from is not optional. The builder used to
// fall back to constants transcribed from one run's console output whenever such
// a file was absent, announcing each one as "[build] PINNED". That was honest but
// it is not an archival property: a missing CSV would produce a complete,
// plausible manuscript containing numbers from a different run. Every one of
// those CSVs is now written by the pipeline, so a missing one means something
// upstream did not run, and the build stops instead of substituting.
//
// Constants transcribed from a PUBLISHED source (the ACCESS panel retention
// rate, for instance) are a different thing and stay literal: no run of this
// pipeline produces them, so there is nothing to derive them from.
function requireCsv(rel, what) {
  const rows = readCsv(rel, { quiet: true });
  if (!rows.length)
    throw new Error("[build] " + rel + " is missing or empty, and " + what
      + " comes from it.\n  The manuscript was NOT built: without that file the "
      + "only alternative is to\n  print numbers from some other run. Re-run the "
      + "step that writes it, then rebuild.");
  return rows;
}

function readCsv(rel, opt) {
  // Outputs of the abandoned outcome-conditioned imputation were moved to an
  // archive folder marked "failed convergence -- not for inference". Nothing in
  // that folder may ever reach the manuscript, so a read from it is fatal.
  if (/archive/i.test(rel))
    throw new Error("[build] refusing to read " + rel + ": archived outputs are not for inference");
  const f = FIG + rel;
  if (!fs.existsSync(f)) {
    // `quiet` is for outputs whose absence is already reported by an explicit
    // "[build] PINNED" message; everything else must shout.
    if (opt && opt.quiet) return [];
    console.warn("[build] MISSING ANALYSIS OUTPUT: " + rel +
                 " -- any table built from it will be empty and any number "
                 + "drawn from it will read NA.");
    return [];
  }
  const lines = fs.readFileSync(f, "utf8").replace(/﻿/, "").trim().split(/\r?\n/);
  if (lines.length < 2) return [];
  const head = splitCsv(lines[0]);
  return lines.slice(1).map((ln) => {
    const c = splitCsv(ln), o = {};
    head.forEach((h, i) => { o[h] = c[i]; });
    return o;
  });
}
// Format a value read from a CSV. R writes missing values as "NA"; keep them
// visible rather than coercing them to 0, which would read as a real result.
const nfmt = (x, d = 2) => {
  const v = Number(x);
  if (x === undefined || x === "" || x === "NA" || !isFinite(v)) return "NA";
  // toFixed inherits binary-float surprises: (0.845).toFixed(2) is "0.84",
  // because 0.845 is stored slightly below the decimal half. Analysis CSVs
  // that are themselves rounded (e.g. si_benchmark_agreement.csv at 3 dp)
  // therefore produced display values one unit low. Round half away from
  // zero on the decimal string instead, which is what a reader checking the
  // CSV by hand will do.
  const shifted = Number((v * Math.pow(10, d)).toPrecision(12));
  const rounded = Math.sign(shifted) * Math.round(Math.abs(shifted));
  return (rounded / Math.pow(10, d)).toFixed(d);
};
const pfmt = (x) => {
  const v = Number(x);
  if (x === undefined || x === "" || x === "NA" || !isFinite(v)) return "NA";
  // Same decimal rounding as nfmt, so a p-value is rounded half away from
  // zero on its decimal value rather than on its binary representation.
  return v < 0.001 ? "<0.001" : nfmt(v, 3);
};

// ================== S3: ERA vs INSTRUMENT TRANSFER DIAGNOSTIC ================
// Everything in this block is read from what 21_transfer_diagnostics.R wrote.
// The script's design question: the headline "stacking models do not transfer"
// result changes the era AND the measuring instrument at the same time, so it
// cannot say which of the two is responsible. The 2x2 below separates them --
// ACCESS Wave 1 to Wave 2 changes the era with the instrument held fixed (both
// are ACCESS); IRES to ACCESS Wave 2 changes the instrument with the era held
// approximately fixed (2018 vs 2019-20).
const trPred  = readCsv("diagnostics/transfer_predictive.csv");
const trCoef  = readCsv("diagnostics/transfer_coefficients.csv");
const trShift = readCsv("diagnostics/transfer_covariate_shift.csv");
const trLoo   = readCsv("diagnostics/transfer_loo_predictor.csv");

// ---- Drop-one transfer contribution (Table S14) ----------------------------
// transfer_loo_predictor.csv reports, for each predictor, the district-level
// correlation obtained when the stacking model is refitted without it.
// contrib_* is the LOSS relative to the full model, so a positive contribution
// means the predictor helps out-of-sample transfer and a negative one means
// that dropping it improves transfer. The SI Methods S3 sentence reads its
// magnitudes and its ordering from this file rather than restating them, so a
// re-run that reorders the predictors reorders the sentence with it.
const LOO_LABEL = {
  caste3: "caste", religion3: "religion", hhsize_c: "household size",
  bpl_bin: "the ration-card indicator",
  elec_bin: "the electrification indicator", wealth_q: "the wealth quintile",
};
const looLab = (k) => LOO_LABEL[k] || k;
const looSort = (col, desc) => trLoo.slice().sort((a, b) =>
  (desc ? 1 : -1) * (Number(b[col]) - Number(a[col])));
const looTop = looSort("contrib_W1", true)[0] || {};
// Predictors whose removal improves transfer in BOTH waves, largest gain first.
const looHarmfulList = looSort("contrib_W1", false)
  .filter((x) => Number(x.contrib_W1) < 0 && Number(x.contrib_W2) < 0)
  .map((x) => looLab(x.dropped));
const orList = (a) =>
  a.length === 0 ? "none"
  : a.length === 1 ? a[0]
  : a.slice(0, -1).join(", ") + (a.length > 2 ? ", or " : " or ") + a[a.length - 1];
if (looTop.dropped && looTop.dropped !== "religion3") {
  console.warn("[build] CHECK: the largest positive transfer contributor is now "
    + looTop.dropped + ", not religion3; the SI Methods S3 sentence names religion.");
}

// Pull one predictive row by its `kind` label, which 21_transfer_diagnostics.R
// writes as the human-readable name of the contrast.
const trRow = (kind) => trPred.find((x) => x.kind === kind) || {};
const trR   = (kind, d = 2) => nfmt(trRow(kind).district_r, d);
const ERA   = trRow("ERA"), INST = trRow("INSTRUMENT"), BOTH = trRow("ERA+INSTRUMENT");

// The headline cross-era transfer failure quoted in SI Methods S7, SI Methods S2
// and the SI Figure S6 caption is the ORIGINAL 06_stacking_prediction.R test:
// an IRES-trained, context-free model applied to ACCESS Wave 1, with IRES on
// its full rural frame rather than restricted to the six ACCESS states. That is
// the "ERA+INSTRUMENT, unrestricted IRES" row, NOT the plain "ERA+INSTRUMENT"
// row (which restricts IRES to the common states and gives a different, less
// negative value). The two were previously conflated and the text carried a
// stale -0.01.
const CROSS_ERA_R = trR("ERA+INSTRUMENT, unrestricted IRES");
// The era/instrument decomposition is read from the numbers, not asserted, and
// kept descriptive: the three correlations share data and models, so no SE-based
// comparison between them is offered.
const trNum = (k) => Number(trRow(k).district_r);
const TR_E = trNum("ERA"), TR_I = trNum("INSTRUMENT"), TR_B = trNum("ERA+INSTRUMENT");
const TR_ALL_WEAK = [TR_E, TR_I, TR_B].every((v) => isFinite(v) && v < 0.5);
const TR_BOTH_GE = TR_B >= Math.max(TR_E, TR_I);
const trNdist = () => { const v = ["ERA", "INSTRUMENT", "ERA+INSTRUMENT"].map((k) => Number(trRow(k).n_districts)).filter(isFinite);
  return v.length ? (Math.min(...v) === Math.max(...v) ? String(Math.min(...v)) : Math.min(...v) + "-" + Math.max(...v)) : "NA"; };
const trWeakTxt = () => TR_ALL_WEAK
  ? "transfer was weak across the evaluated contrasts (" + trNdist() + " test districts each)"
    + (TR_BOTH_GE ? ", and the contrast that changes both era and instrument transferred no worse than either single change" : "")
    + ". These comparisons do not isolate separate era and instrument effects"
  : "the era contrast gives r = " + trR("ERA") + " and the instrument contrast r = " + trR("INSTRUMENT") + ", on " + trNdist() + " districts each";

// Predictors whose coefficient is statistically indistinguishable between the
// two training surveys of a contrast AND keeps its sign. 21 writes `transfers`
// as an R logical, so it arrives as the string "TRUE".
const subsetTxt = () => {
  const sub = trPred.filter((x) => x.kind === "transferable subset").map((x) => Number(x.district_r));
  const full = [TR_B, TR_I];
  if (sub.length < 2 || !full.every(isFinite)) return ".";
  const d = [sub[0] - full[0], sub[1] - full[1]];
  if (d.every((v) => Math.abs(v) < 0.1)) return " - no material improvement; little of the relationship transfers in any direction.";
  if (Math.max(d[0], d[1]) < 0.1) return " - no better, and against " + (d[0] < d[1] ? "Wave 1" : "Wave 2") + " worse; little of the relationship transfers in any direction.";
  return " - " + (d[1] > d[0] ? "closer against Wave 2" : "closer against Wave 1") + ", but still weak"
    + ".";
};
const trStable = (contrastPrefix) => {
  const s = trCoef.filter((x) => (x.contrast || "").indexOf(contrastPrefix) === 0
                                 && x.transfers === "TRUE").map((x) => x.term);
  return s.length ? joinEng(s.map(termLab)) : "none";
};
// Covariate-shift lookup: the three columns are the predictor's mean (or, for a
// categorical, the share in that level) in each of the three frames.
const shiftRow = (v, lev) =>
  trShift.find((x) => x.variable === v && (lev === undefined || x.level === lev)) || {};
// Model terms are stored under their R names; the prose needs English. Any term
// not in this map falls through unchanged so a new predictor is visible rather
// than silently mislabelled.
const TERM_LAB = {
  wealth_q: "the within-state wealth quintile",
  elec_bin: "household electrification",
  hhsize_c: "household size",
  bpl_bin: "the BPL/Antyodaya ration card",
  caste3: "caste category",
  religion3: "religion",
  caste3General: "caste (General)",
  "caste3Scheduled Caste": "caste (Scheduled Caste)",
  "caste3Scheduled Tribe": "caste (Scheduled Tribe)",
  "caste3Other Backward Class": "caste (Other Backward Class)",
  religion3Other: "religion (Other)",
  religion3Muslim: "religion (Muslim)",
  religion3Hindu: "religion (Hindu)",
};
const termLab = (t) => TERM_LAB[t] || t;
const capFirst = (t) => t ? t.charAt(0).toUpperCase() + t.slice(1) : t;
const joinEng = (a) => a.length > 1
  ? a.slice(0, -1).join(", ") + " and " + a[a.length - 1] : (a[0] || "none");

// Which predictors the pipeline actually refit the transferable-subset model
// on, taken from the fit log rather than restated from memory.
// Same rotation hazard as the KG check below: run_everything.R renames
// model_fits.csv to prev_model_fits.csv at the START of every run, including a
// START_FROM= partial re-run, so a run that did not execute
// 21_transfer_diagnostics.R has no transferable_subset row at all, and reading
// only the current log would silently push "NA predictors: NA" into SI Section
// S3. We fall back to the archived log in that case, and say loudly that we
// did, so the failure is a build-time warning rather than an NA nobody traced.
const SUB_RE = /transferable_subset/;
const modelFits = readCsv("diagnostics/model_fits.csv");
// Preferred source: the file 21_transfer_diagnostics.R writes for this purpose,
// which survives partial re-runs; the fit logs are the fallback.
const subFile = readCsv("diagnostics/transfer_subset_vars.csv", { quiet: true })[0];
let subFit = subFile ? { extra: "vars: " + subFile.vars } : modelFits.find((r) => SUB_RE.test(r.model || ""));
if (!subFit) {
  const prevFits = readCsv("diagnostics/prev_model_fits.csv", { quiet: true });
  subFit = prevFits.find((r) => SUB_RE.test(r.model || ""));
  console.warn("[build] FALLBACK: the transferable-subset predictor list in SI "
    + "Section S3 comes from diagnostics/prev_model_fits.csv, not the current "
    + "log. The current run did not execute 21_transfer_diagnostics.R (a "
    + "START_FROM= partial re-run), so its fit row is absent. That row is a "
    + "property of the transfer diagnostic and is unaffected by downstream steps"
    + (subFit ? "." : ", but it was NOT found in the archived log either -- SI "
        + "Section S3 will read 'NA predictors: NA'. Re-run "
        + "21_transfer_diagnostics.R, then build_docx_only.R."));
}
subFit = subFit || {};
const SUB_M = /vars:\s*(\S+)/.exec(subFit.extra || "");
if (!SUB_M) {
  console.warn("[build] MISSING: the transferable_subset row carries no `vars:` "
    + "field, so SI Section S3 will read 'NA predictors: NA'. Re-run "
    + "21_transfer_diagnostics.R, then build_docx_only.R.");
}
const SUB_VARS = SUB_M ? joinEng(SUB_M[1].split("+").map(termLab)) : "NA";
const SUB_NVAR = SUB_M ? String(SUB_M[1].split("+").length) : "NA";

const shiftDelta = (v, lev, which, d = 3) => nfmt(shiftRow(v, lev)[which], d);

// ---- leave-district-out validation of the PRODUCTION stacking models ----
// These are NOT the same models as the drop-one importance table (Table S9),
// which refits a household-only mixed model; quoting one set where the other is
// meant is exactly the drift this file exists to prevent, so they are read from
// separate sources. 06_stacking_prediction.R now writes
// stacking_ldo_validation.csv, but the outputs currently on disk predate that
// writer, so the values that run printed are pinned here and the build says so
// out loud instead of passing them off as CSV-derived. Re-running 06 makes the
// CSV branch live and the pin inert.
const LDO_FILE = "stacking_ldo_validation.csv";
const ldoCsv = fs.existsSync(FIG + LDO_FILE) ? readCsv(LDO_FILE) : null;
const LDO_PINNED = {
  "ACCESS W1":  { auc: "0.61", district_r: "0.24" },
  "IRES rural": { auc: "0.65", district_r: "0.43" },
};
if (!ldoCsv) requireCsv("stacking_ldo_validation.csv", "the context-free transport validation (AUC, district r)");
if (false) console.warn("[build] superseded: production stacking "
  + "leave-district-out AUC and district-r. Re-run 06_stacking_prediction.R to "
  + "generate " + LDO_FILE + " and source them automatically.");
const ldo = (survey, field, d = 2) => {
  if (ldoCsv) {
    const x = ldoCsv.find((r) => r.survey === survey);
    return x ? nfmt(x[field], d) : "NA";
  }
  return (LDO_PINNED[survey] || {})[field] || "NA";
};
const shiftTriple = (v, lev, d = 2) => {
  const x = shiftRow(v, lev);
  return `${nfmt(x.ACCESS_W1_2015, d)} to ${nfmt(x.ACCESS_W2_2018, d)} to ${nfmt(x.IRES_rural_common, d)}`;
};



// ================= NUMBERS READ BACK OUT OF THE HEALTH OUTPUTS ==============
// Same rule as above: nothing here is typed. Every health estimate, every N,
// every covariate-importance value and every sampler diagnostic is read from
// the CSV the analysis wrote, so a re-run of the pipeline rewrites the table
// instead of leaving a literal behind that no longer matches the model.
// p-values are rounded once, at display. health_effects_table*.csv carry p and
// p_cr2 at full precision; health_small_cluster_inference*.csv carry the same
// quantities for the adjusted, all-stratum FIXED-SURFACE rows they cover (not
// the MI rows, which come from the effects table only). Where both files have a row, take p (CR0, t on residual df) and p_cr2
// from the small-cluster file -- but only if its estimate matches, so a stale
// small-cluster file can never overwrite the p-values of a different fit.
const fullP = (rows, file) => {
  const sc = readCsv(file, { quiet: true });
  rows.forEach((r) => {
    if (String(r.adjusted).toUpperCase() !== "TRUE" || (r.support_stratum || "all") !== "all") return;
    const m = sc.find((x) => x.outcome === r.outcome && x.term === r.term && x.weighting === r.weighting);
    if (!m) return;
    // est_per10 is stored at full precision; estimate itself is rounded to 4 dp.
    const rEst = (r.est_per10 !== undefined && r.est_per10 !== "") ? Number(r.est_per10) / 10 : Number(r.estimate);
    const tol  = (r.est_per10 !== undefined && r.est_per10 !== "") ? 1e-9 : 5.1e-5;
    if (m.estimate !== undefined && !(Math.abs(Number(m.estimate) - rEst) < tol)) {
      throw new Error("fullP: estimate mismatch between " + file + " and the effects table for "
                      + r.outcome + "/" + r.term + "/" + r.weighting + " (" + m.estimate + " vs " + r.estimate + ")");
    }
    if (m.p_cr0 !== undefined && m.p_cr0 !== "") r.p = m.p_cr0;
    if (m.p_cr2 !== undefined && m.p_cr2 !== "") r.p_cr2 = m.p_cr2;
  });
  return rows;
};
const heRural = fullP(readCsv("health_effects_table.csv"), "health_small_cluster_inference.csv");
const heAll   = fullP(readCsv("health_effects_table_all.csv"), "health_small_cluster_inference_all.csv");

// The six exposure specifications, in the order the text discusses them.
const EXPO_SPECS = [
  ["change_lpg_raw",               "Raw NFHS"],
  ["change_lpg_rc",                "Regression-calibrated"],
  ["change_lpg_bayes",             "Bayesian-corrected, fixed surface"],
  ["change_lpg_bayes_MI",          "Bayesian, exposure-only MI (sensitivity)"],
  ["change_lpg_bayes_iresonly",    "Bayesian, one instrument (IRES) both rounds"],
  ["change_lpg_bayes_iresonly_MI", "Bayesian, one instrument, exposure-only MI"],
];
const OUTCOME_LAB = { neonatal: "Neonatal", infant: "Infant",
                      hypertension: "Hypertension", diabetes: "Diabetes",
                      "diabetes (self-report)": "Diabetes (self-report)" };

// One row of a health-effects CSV, selected on the full key. Selecting on all
// four of outcome/term/weighting/support_stratum matters: the same CSV holds
// weighted and unweighted fits and three support strata, and a partial key
// would silently return whichever happened to be written first.
const heRow = (rows, oc, term, o) => {
  o = o || {};
  return rows.find((x) => x.outcome === oc && x.term === term
    && x.adjusted        === (o.adjusted  || "TRUE")
    && x.weighting       === (o.weighting || "birth_hmean")
    && x.support_stratum === (o.support   || "all")) || null;
};
// "est [lo, hi]" on the per-10-unit scale the tables and the text both use.
const ciCell = (r, d) => r
  ? nfmt(r.est_per10, d === undefined ? 3 : d) + " [" + nfmt(r.lo_per10, d === undefined ? 3 : d)
    + ", " + nfmt(r.hi_per10, d === undefined ? 3 : d) + "]"
  : "NA";
const est10 = (r, d) => r ? nfmt(r.est_per10, d === undefined ? 3 : d) : "NA";

// ---- small-cluster inference (CR2 / Satterthwaite), the primary rule ---------
// H2_health_models.R writes se_cr2 / df_satt / p_cr2 / ci_cr2_* on the RAW
// coefficient scale for every row of the effects table, including the
// Rubin-pooled MI rows (whose df is Barnard-Rubin on a Satterthwaite nu_com).
// The manuscript reports everything per 10 percentage points, so the CI bounds
// are scaled here rather than in R -- one place, one factor.
const hasCr2 = (r) => r && r.se_cr2 !== undefined && r.se_cr2 !== ""
                        && isFinite(Number(r.se_cr2));
const d3 = (d) => (d === undefined ? 3 : d);
const ci10cr2 = (r, d) => hasCr2(r)
  ? nfmt(10 * Number(r.ci_cr2_low), d3(d)) + " to " + nfmt(10 * Number(r.ci_cr2_high), d3(d))
  : "NA";
const ciCellCr2 = (r, d) => hasCr2(r)
  ? nfmt(r.est_per10, d3(d)) + " [" + nfmt(10 * Number(r.ci_cr2_low), d3(d))
    + ", " + nfmt(10 * Number(r.ci_cr2_high), d3(d)) + "]"
  : "NA";
// Negated orientation, to match nest10/nci10: a sentence that says "0.144
// FEWER deaths" needs its interval stated in the same direction, so the bounds
// are negated and swapped. Mixing nest10 with a signed interval reads as a
// contradiction.
const nci10cr2 = (r, sep, d) => hasCr2(r)
  ? nfmt(-10 * Number(r.ci_cr2_high), d3(d)) + (sep || " to ")
    + nfmt(-10 * Number(r.ci_cr2_low), d3(d))
  : "NA";
const pCr2  = (r) => hasCr2(r) ? pfmt(r.p_cr2)  : "NA";
const dfCr2 = (r) => hasCr2(r) ? nfmt(r.df_satt, 1) : "NA";
const nClus = (r) => (r && r.n_clusters) ? String(r.n_clusters) : "NA";
// Does a CR2 interval exclude the null? Derived, never asserted in prose: the
// Table S7 caption bug (a hard-coded "reaches p =" that outlived its number)
// is the reason every significance verb in this document is computed.
const cr2Excludes = (r) => hasCr2(r)
  && Number(r.ci_cr2_low) * Number(r.ci_cr2_high) > 0;

// The wild cluster bootstrap and the null-calibration simulation.
const wcbCsv = readCsv("health_small_cluster_inference.csv", { quiet: true });
const wcb = (outcome, term) => wcbCsv.find(
  (r) => r.outcome === outcome && r.term === term) || null;
const pWild = (outcome, term) => {
  const r = wcb(outcome, term);
  return r && r.p_wild !== "" && isFinite(Number(r.p_wild)) ? pfmt(r.p_wild) : "NA";
};
const wildB = () => {
  const r = wcbCsv.find((x) => x.wild_B && isFinite(Number(x.wild_B)));
  return r ? Number(r.wild_B).toLocaleString("en-US") : "NA";
};
const calibCsv = readCsv("health_cluster_inference_calibration.csv", { quiet: true });
// Rows are found by their rule label, not their position: 0 = conventional
// CR0/CR1, 1 = CR2/Satterthwaite, 2 = wild cluster bootstrap.
const CALIB_RX = [/^conventional/i, /^cr2/i, /^wild/i];
const calibRow = (i) => calibCsv.find((x) => CALIB_RX[i].test(String(x.rule || "")));
const calibRate = (i, d) => {
  const r = calibRow(i);
  return r ? nfmt(100 * Number(r.rejection_rate_at_nominal_05), d === undefined ? 1 : d) : "NA";
};
const calibReps = () => calibCsv.length
  ? Number(calibCsv[0].replications).toLocaleString("en-US") : "NA";
const HAVE_CALIB = [0, 1, 2].every((i) => !!calibRow(i));
// Is CR2 itself calibrated on this design? Judged against the Monte Carlo error
// of the null simulation, not asserted: a rejection rate more than two Monte
// Carlo SEs above 5% means CR2 is better than CR1 here but still anticonservative.
const CR2_RATE = HAVE_CALIB ? Number(calibRow(1).rejection_rate_at_nominal_05) : NaN;
// Two Monte Carlo SEs, each labelled where used: under the nominal 5% rate
// (the yardstick for "above nominal") and at the observed rate (the precision of
// the reported rate itself).
const CR2_NREP = HAVE_CALIB ? Number(calibRow(1).replications) : NaN;
const CR2_MCSE = HAVE_CALIB ? Math.sqrt(0.05 * 0.95 / CR2_NREP) : NaN;
const CR2_MCSE_OBS = HAVE_CALIB ? Math.sqrt(CR2_RATE * (1 - CR2_RATE) / CR2_NREP) : NaN;
const CR2_OVER = isFinite(CR2_RATE) && CR2_RATE > 0.05 + 2 * CR2_MCSE;
const cr2LimTxt = () => !HAVE_CALIB ? ""
  : CR2_OVER
    ? "CR2 is a substantial improvement but is not fully calibrated on this design: in this simulation it rejected a true null in " + calibRate(1) + "% of datasets (Monte Carlo SE " + nfmt(100 * CR2_MCSE_OBS, 1) + " points at the observed rate), more than two Monte Carlo SEs above the nominal 5% (SE " + nfmt(100 * CR2_MCSE, 1) + " points under the nominal rate), and the wild cluster bootstrap rejected in " + calibRate(2) + "%. CR2 p-values and intervals were therefore somewhat optimistic in this simulation and should be read as approximate."
    : "CR2 rejected a true null in " + calibRate(1) + "% of simulated datasets, within Monte Carlo error of the nominal 5%.";
// ---- the R that actually produced these outputs -----------------------------
// This used to be a typed version number, and it was wrong: the document
// named a release no run had ever used.
// audit_numbers.js did not catch it because a bare "4.5" is indistinguishable
// from a section reference, so the literal is gone and the value is derived,
// written by 20_pipeline_checks.R from the session that produced the outputs.
const envSnap = readCsv("environment_snapshot.csv", { quiet: true });
const envVal = (q) => {
  const r = envSnap.find((x) => x.quantity === q);
  return r && r.value ? String(r.value) : null;
};
// r_version, not r_version_short. environment_snapshot.csv records both; the
// full patch release is what ran and what renv.lock pins, and reporting "4.3"
// when 4.3.3 is on record is a needless loss of exactly the specificity this
// value was made derivable for. Falls back to the short form if only that is
// present in an older snapshot.
const R_VERSION = envVal("r_version") || envVal("r_version_short");
if (!R_VERSION)
  console.warn("[build] FALLBACK: environment_snapshot.csv is absent, so the R "
    + "version in the Methods is omitted rather than guessed. Re-run "
    + "20_pipeline_checks.R to derive it.");
// No pinned default on purpose: a wrong version number is worse than none.
const R_VERSION_TXT = R_VERSION
  ? "R version " + R_VERSION + " [20]"
  : "R [20]";
// IRES fieldwork window. 03_prep_ires.R derives these from the interview dates
// when it can find the date column, which is not named consistently across IRES
// releases. They were previously typed into SI Methods S1. The fallback keeps
// an older output tree building, and says loudly that it is a fallback.
const iresWinCsv = readCsv("ires_fieldwork_window.csv", { quiet: true });
let _iresWinWarned = false;
const iresWin = (quantity, fallback) => {
  const r = iresWinCsv.find((x) => x.quantity === quantity);
  if (r && r.value !== undefined && r.value !== "") {
    const n = Number(r.value);
    return isFinite(n) ? String(n) : r.value;
  }
  if (!_iresWinWarned) {
    _iresWinWarned = true;
    console.warn("[build] FALLBACK: ires_fieldwork_window.csv is absent, so the IRES "
      + "fieldwork-window percentages in SI Methods S1 are the PINNED values from "
      + "the 2026-09 analysis, not this run's. Re-run 03_prep_ires.R to derive them.");
  }
  return fallback;
};
// Body of Table S6a. Driven by health_small_cluster_inference.csv, which H2
// writes for exactly the fixed-surface rows the bootstrap is defined on, in the
// order the manuscript reports them.
const SMALL_CL_SPECS = [
  ["change_lpg_raw",                "Raw NFHS"],
  ["change_lpg_rc",                 "Regression-calibrated"],
  ["change_lpg_bayes",              "Bayesian-corrected (era-matched)"],
  ["change_lpg_bayes_iresonly",     "Bayesian, one instrument (IRES)"],
  ["change_lpg_bayes_accesswt",     "Bayesian, design-weighted ACCESS leg"],
];
const smallClusterRows = () => {
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    SMALL_CL_SPECS.forEach((sp) => {
      const r = wcb(oc, sp[0]);
      if (!r) return;
      out.push([OUTCOME_LAB[oc], sp[1], nfmt(r.estimate_per10, 3),
                pfmt(r.p_cr0), pfmt(r.p_cr2),
                (r.p_wild !== "" && isFinite(Number(r.p_wild))) ? pfmt(r.p_wild) : "NA",
                nfmt(r.df_satt, 1)]);
    });
  });
  return out;
};
// "inflates the standard error by about X%" -- a percentage INCREASE, not a
// ratio. Written as a function so the sign and the base are readable here
// rather than inferred from a division buried inside a sentence.
const seInflation = (base, wider) => {
  const a = Number((base || {}).se_cr2), b = Number((wider || {}).se_cr2);
  if (!isFinite(a) || !isFinite(b) || a === 0) return "an amount that could not be computed";
  return nfmt(100 * (b / a - 1), 0) + "%";
};
// What propagating the correction's uncertainty does to the infant estimate,
// stated from the CSV rows. Under CR2 the change can be mostly in degrees of
// freedom and in the point estimate rather than in the standard error, so the
// prose may not assume the standard error grows "materially".
const seRatio = (base, wider, col) => {
  const a = Number((base || {})[col]), b = Number((wider || {})[col]);
  return (isFinite(a) && isFinite(b) && a > 0) ? b / a - 1 : NaN;
};
const pctTxt = (x) => isFinite(x) ? nfmt(100 * x, 0) + "%" : "an amount that could not be computed";
const ci10  = (r, d) => r
  ? nfmt(r.lo_per10, d === undefined ? 3 : d) + " to " + nfmt(r.hi_per10, d === undefined ? 3 : d)
  : "NA";
// The Abstract phrases these effects as "fewer deaths", i.e. the sign-flipped
// coefficient. These wrappers read the same CSV rows the Results tables use, so
// the Abstract cannot drift away from the tables; note that flipping the sign
// also swaps the interval bounds.
const negv = (v) => (isFinite(Number(v)) ? -Number(v) : NaN);
const nest10 = (r, d) => r ? nfmt(negv(r.est_per10), d === undefined ? 3 : d) : "NA";
const nci10  = (r, sep, d) => r
  ? nfmt(negv(r.hi_per10), d === undefined ? 3 : d) + (sep || " to ")
    + nfmt(negv(r.lo_per10), d === undefined ? 3 : d)
  : "NA";

// Body of a 2-outcome x 6-specification health table. A specification the run
// did not produce is dropped rather than rendered as a row of NAs.
const healthRows = (rows, o) => {
  o = o || {};
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    EXPO_SPECS.forEach((sp) => {
      const r = heRow(rows, oc, sp[0], o);
      if (r) out.push([OUTCOME_LAB[oc], sp[1],
                       ciCellCr2(r), pCr2(r), ciCell(r), pfmt(r.p)]);
    });
  });
  return out;
};
// Unweighted sensitivity. The main models weight districts by the harmonic mean
// of eligible births across the two rounds; the unweighted fit is reported
// alongside rather than left implicit. Only the four specifications the
// pipeline fits unweighted are available, so the list is separate.
const UNW_SPECS = [
  ["change_lpg_raw",            "Raw NFHS, unweighted"],
  ["change_lpg_rc",             "Regression-calibrated, unweighted"],
  ["change_lpg_bayes",          "Bayesian-corrected, unweighted"],
  ["change_lpg_bayes_iresonly", "Bayesian, one instrument (IRES), unweighted"],
];
const healthRowsUnw = (rows) => {
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    UNW_SPECS.forEach((sp) => {
      const r = heRow(rows, oc, sp[0], { weighting: "unweighted" });
      if (r) out.push([OUTCOME_LAB[oc], sp[1],
                       ciCellCr2(r), pCr2(r), ciCell(r), pfmt(r.p)]);
    });
  });
  return out;
};

// The N the fitted models actually used, so "N = 606" cannot drift.
const heN = (rows, o) => {
  const r = heRow(rows, "infant", "change_lpg_bayes", o);
  return r ? r.n : "NA";
};
const N_RURAL = heN(heRural), N_ALL = heN(heAll);

// Support-stratum sensitivity (districts inside both calibration surveys'
// footprints, and inside an ACCESS state). Reported because restricting support
// is a check that can fail, not a check that confirms.
const supFull  = heRow(heRural, "infant", "change_lpg_bayes_MI", { support: "full_support" });
const supState = heRow(heRural, "infant", "change_lpg_bayes_MI", { support: "state_support" });

// Non-overlapping recent-cohort sensitivity, quoted in the Table S6 caption.
const heNonov = fullP(readCsv("health_effects_table_nonoverlap.csv"), "health_small_cluster_inference_nonoverlap.csv");

// ONLY THE APPROVED HEALTH OUTPUTS. The manuscript reports two treatments of the
// corrected exposure: the fixed corrected surface (conditional and exploratory)
// and the exposure-only multiple imputation (a sensitivity analysis). Every MI
// row in the effects tables must say so in its mi_method column; a table written
// before that column existed, or one carrying an outcome-conditioned row, stops
// the build rather than entering the text.
const assertApprovedMI = (rows, file) => {
  const mi = rows.filter((x) => /_MI$/.test(x.term || ""));
  if (!mi.length) return;
  const bad = mi.filter((x) => x.mi_method !== "exposure_only");
  if (bad.length)
    throw new Error("[build] " + file + " has " + bad.length + " MI row(s) that are not the "
      + "approved exposure-only imputation (mi_method = " + (bad[0].mi_method || "missing")
      + "). Re-run H2_health_models.R; the outcome-conditioned imputation is not for inference.");
};
assertApprovedMI(heRural, "health_effects_table.csv");
assertApprovedMI(heAll, "health_effects_table_all.csv");
assertApprovedMI(heNonov, "health_effects_table_nonoverlap.csv");

// Named handles for the estimates the Results text quotes sentence by sentence.
const R_N_RAW = heRow(heRural, "neonatal", "change_lpg_raw");
const R_I_RAW = heRow(heRural, "infant",   "change_lpg_raw");
const R_N_RC  = heRow(heRural, "neonatal", "change_lpg_rc");
const R_I_RC  = heRow(heRural, "infant",   "change_lpg_rc");
const R_N_BAY = heRow(heRural, "neonatal", "change_lpg_bayes");
const R_I_BAY = heRow(heRural, "infant",   "change_lpg_bayes");
const R_I_BAY_UNW = heRow(heRural, "infant", "change_lpg_bayes", { weighting: "unweighted" });
const R_I_IRES = heRow(heRural, "infant", "change_lpg_bayes_iresonly");
const R_N_IRES = heRow(heRural, "neonatal", "change_lpg_bayes_iresonly");
const R_I_MI   = heRow(heRural, "infant", "change_lpg_bayes_MI");
const R_N_MI   = heRow(heRural, "neonatal", "change_lpg_bayes_MI");
// What propagation does to the infant estimate, from the same rows (see seRatio).
const PROP_CR2  = seRatio(R_I_BAY, R_I_MI, "se_cr2");
const PROP_CONV = seRatio(R_I_BAY, R_I_MI, "se");
const PROP_SMALL = isFinite(PROP_CR2) && PROP_CR2 < 0.10;
const propMoveTxt = () => "the CR2 standard error is " + pctTxt(PROP_CR2) + " larger than with the surface held fixed, the point estimate moves from " + est10(R_I_BAY) + " to " + est10(R_I_MI) + ", and the Satterthwaite degrees of freedom fall from " + dfCr2(R_I_BAY) + " to " + dfCr2(R_I_MI) + "; under the conventional clustered standard error the increase is " + pctTxt(PROP_CONV);
const convPTxt = (subj) => (R_I_BAY && Number(R_I_BAY.p) < 0.05)
  ? "Under that rule " + subj + " reaches p = " + pfmt((R_I_BAY || {}).p) + " and the propagated one does not."
  : "Even under that rule " + subj + " has p = " + pfmt((R_I_BAY || {}).p) + ", so neither estimate is significant under any rule we report.";
// "about N-fold" claims are ratios of two CSV rows, rounded to the nearest half
// so the qualitative wording cannot outrun what a re-run actually produced.
const foldHalf = (a, b, d) => (a && b && Number(b.est_per10) !== 0)
  ? nfmt(Math.round((2 * Number(a.est_per10)) / Number(b.est_per10)) / 2, d === undefined ? 1 : d)
  : "NA";
const A_N_BAY = heRow(heAll,   "neonatal", "change_lpg_bayes");
const A_I_BAY = heRow(heAll,   "infant",   "change_lpg_bayes");
const NO_I_BAY  = heRow(heNonov, "infant", "change_lpg_bayes");
const NO_I_IRES = heRow(heNonov, "infant", "change_lpg_bayes_iresonly");
// Support-stratum sensitivity in the all-household frame, for the contrast the
// reviewer asked us to state plainly rather than gloss as "robust".
const supFullAll  = heRow(heAll, "infant", "change_lpg_bayes_MI", { support: "full_support" });
const supStateAll = heRow(heAll, "infant", "change_lpg_bayes_MI", { support: "state_support" });

// ---- ACCESS LPG consumption ------------------------------------------------
// Read from access_lpg_consumption.csv, which 02_prep_access.R writes for this
// purpose. Preferring a FILE to the scraped check row below is the whole point
// of the fix: a check row exists only for a script the current run executed, so
// a partial re-run (START_FROM=05_correction.R, say) leaves no step-02 rows and
// the scrape silently yields "NA kg/year" for a statistic that had not changed.
// That is exactly what reached the 2026-09-15 SI build.
//
// The scrape is kept as a fallback for output trees written before 02 gained
// the export, and the archived check log as a fallback to that, so an older
// tree still builds. Every downgrade says so; only a total miss reads NA.
const KG_RE = /physically plausible band/;
// Module-level: the check log is read ONCE and used by two consumers -- the KG
// fallback just below, and chkDetail() further down for the 22_design_analysis
// agreement figures. It was briefly scoped inside the KG fallback branch, which
// left chkDetail() referencing an undefined binding on every tree that HAS
// access_lpg_consumption.csv (i.e. every correct one). Keep it at module scope.
const pipeChecks = readCsv("diagnostics/pipeline_checks.csv");
// Empirical-Bayes shrinkage of the NFHS input to the corrected surface (05).
// "a-b" for a range of two rounded values, a single value when they round alike.
const rng2 = (a, b, d) => { const x = nfmt(Math.min(Number(a), Number(b)), d), y = nfmt(Math.max(Number(a), Number(b)), d); return x === y ? x : x + "-" + y; };
const shrinkTab = readCsv("me_shrinkage.csv", { quiet: true });
// Joint (two-round) shrinkage summary (05_correction.R), and the state-cluster
// bootstrap of the between-round correlation that 31_postrun_intervals.R adds:
// it keeps every replicate's UNCAPPED estimate and the share at the +/-0.95 cap.
const jointTab = readCsv("me_shrinkage_joint.csv", { quiet: true });
const jQ = (k, d) => { const r = jointTab.find((x) => x.quantity === k); return r ? nfmt(r.value, d === undefined ? 2 : d) : "NA"; };
const jBoot = readCsv("me_shrinkage_joint_bootstrap_summary.csv", { quiet: true })[0] || null;
if (!jBoot) console.warn("[build] me_shrinkage_joint_bootstrap_summary.csv absent -- the between-round "
  + "correlation interval is 05's capped bootstrap only. Run 31_postrun_intervals.R.");
// Interval sentence for the between-round correlation: 31's uncapped interval
// and the share of replicates at the cap when available, 05's otherwise.
const jRhoCI = () => jBoot
  ? "state-bootstrap 95% interval " + nfmt(jBoot.ci_uncapped_lo, 2) + " to " + nfmt(jBoot.ci_uncapped_hi, 2)
    + " from the uncapped replicates, with " + nfmt(100 * Number(jBoot.share_at_cap), 0) + "% of " + (Number(jBoot.replicates) - (Number(jBoot.failed) || 0)).toLocaleString("en-US")
    + " successful replicates at the 0.95 cap"
  : "state-bootstrap 95% interval " + jQ("between_round_correlation_ci_lo", 2) + " to " + jQ("between_round_correlation_ci_hi", 2);
const shrinkQ = (surf, f) => {
  const r = shrinkTab.filter((x) => x.surface === surf);
  if (!r.length) return "NA";
  if (f === "tau") return nfmt(Math.sqrt(Number(r[0].tau2)), 2);
  const v = r.map((x) => Number(x.lambda)).filter((z) => isFinite(z)).sort((p, q) => p - q);
  return nfmt(v[Math.floor((v.length - 1) / 2)], 2);
};
// Within-sample state-effect check on the regression-calibration slope
// (05_correction.R). slope_applied is written by 05 directly; for an output tree
// that predates that column, recover it from the check's own detail string
// ("slope difference (with-state minus applied): d1, d2").
const stChk = readCsv("calibration_state_effect_check.csv", { quiet: true });
const stApplied = (() => {
  if (stChk.length && stChk[0].slope_applied !== undefined) return stChk.map((r) => Number(r.slope_applied));
  const c = pipeChecks.find((x) => /state-effect calibration slope agrees/.test(x.check || x.label || x.name || ""));
  const m = c && /applied\):\s*([-+0-9.]+),\s*([-+0-9.]+)/.exec(c.detail || "");
  return (m && stChk.length === 2) ? [Number(stChk[0].slope_state) - Number(m[1]), Number(stChk[1].slope_state) - Number(m[2])] : [NaN, NaN];
})();
const stSlope = (i) => stChk[i] ? nfmt(stChk[i].slope_state, 2) : "NA";
const kgCsv  = readCsv("access_lpg_consumption.csv", { quiet: true });
const kgCell = (q) => {
  const r = kgCsv.find((x) => (x.quantity || "") === q);
  return r && r.value !== undefined && r.value !== "" && r.value !== "NA"
    ? Number(r.value) : undefined;
};
let KG_N_RAW, KG_MEDIAN, KG_MEAN;
if (kgCell("n") !== undefined) {
  KG_N_RAW  = String(Math.round(kgCell("n")));
  KG_MEDIAN = nfmt(kgCell("median_kg_yr"), 1);
  KG_MEAN   = nfmt(kgCell("mean_kg_yr"), 1);
} else {
  let kgRow = pipeChecks.find((r) => KG_RE.test(r.check || ""));
  if (!kgRow) {
    const prevChecks = readCsv("diagnostics/prev_pipeline_checks.csv", { quiet: true });
    kgRow = prevChecks.find((r) => KG_RE.test(r.check || ""));
  }
  console.warn("[build] FALLBACK: access_lpg_consumption.csv is absent, so the "
    + "ACCESS LPG-consumption figures come from the check log"
    + (kgRow ? ". Re-run 02_prep_access.R to write the file."
             : ", and they were NOT found there either -- they will read NA. "
               + "Re-run 02_prep_access.R."));
  kgRow = kgRow || {};
  const kgNum = (re) => { const m = re.exec(kgRow.detail || ""); return m ? m[1] : "NA"; };
  KG_N_RAW  = kgNum(/n = ([0-9,]+)/);
  KG_MEDIAN = kgNum(/median = ([0-9.]+)/);
  KG_MEAN   = kgNum(/mean = ([0-9.]+)/);
}
const KG_N = KG_N_RAW === "NA" ? "NA"
  : Number(String(KG_N_RAW).replace(/,/g, "")).toLocaleString("en-US");

// ---- adult cardiometabolic outcomes (Table S5) ----
const adultCsv = readCsv("si_adult_health_effects.csv");
const ADULT_SPECS = [["change_lpg_raw", "Raw NFHS"],
                     ["change_lpg_rc", "Regression-calibrated"],
                     ["change_lpg_bayes", "Bayesian-corrected"]];
const adultRows = () => {
  const out = [];
  ["hypertension", "diabetes", "diabetes (self-report)"].forEach((oc) => {
    ADULT_SPECS.forEach((sp) => {
      const r = adultCsv.find((x) => x.outcome === oc && x.term === sp[0]
                                     && x.adjusted === "TRUE");
      // H4 writes lo_cr2_per10/hi_cr2_per10 already on the per-10 scale, so
      // these cells do not go through ciCellCr2 (which scales from the raw
      // coefficient columns that H2's table carries).
      if (r) out.push([OUTCOME_LAB[oc], sp[1],
                       adultCr2Cell(r), (r.p_cr2 ? pfmt(r.p_cr2) : "NA"),
                       ciCell(r), pfmt(r.p)]);
    });
  });
  return out;
};
const adultCr2Cell = (r) =>
  (r.lo_cr2_per10 !== undefined && r.lo_cr2_per10 !== ""
   && isFinite(Number(r.lo_cr2_per10)))
    ? nfmt(r.est_per10, 3) + " [" + nfmt(r.lo_cr2_per10, 3) + ", "
      + nfmt(r.hi_cr2_per10, 3) + "]"
    : "NA";
const adultPrevCsv = readCsv("si_adult_health_prevalence.csv");
// Hypertension self-report verification (verify_hypertension_selfreport.R):
// links the raw "told high BP" / "taking BP medication" items to the analysis
// file person by person. Verified only if, in BOTH rounds, every respondent
// flagged despite normal measured BP was told or medicated and no unflagged
// respondent with normal BP was. Until that file exists the wording says
// "not verified" and the adult results stay provisional.
const htnVer = readCsv("diagnostics/adult_hypertension_verification.csv", { quiet: true });
const htnRow = (yr, rule) => htnVer.find((r) => String(r.year) === String(yr) && r.rule === rule) || null;
// The gate requires (i) the run's overall status file to say passed = TRUE
// (it covers linkage and unique keys, which the per-rule CSV does not), and
// (ii) for each round, from the per-rule CSV: zero strict-rule classification
// disagreements and missing-value mismatches, both normal-BP subgroup checks,
// and a frame size and missing count that are present and consistent
// (missing = rows - rows classified). A failed run leaves the per-rule CSV
// behind, so the status file is what keeps a later standalone build honest.
const htnStatus = readCsv("diagnostics/adult_hypertension_verification_status.csv", { quiet: true })[0] || null;
const HTN_STRICT = "BP or told or medicated (NA if any component missing)";
const HTN_VERIFIED = !!htnStatus && String(htnStatus.passed).toUpperCase() === "TRUE"
  && Number(htnStatus.n_failures) === 0
  // both files must come from the same verification run
  && !!htnStatus.run_id && htnVer.length > 0 && htnVer.every((r) => r.run_id === htnStatus.run_id)
  && ["2015", "2019"].every((yr) => {
    const st = htnRow(yr, HTN_STRICT),
          a = htnRow(yr, "flagged with normal BP: told or medicated"),
          b = htnRow(yr, "unflagged with normal BP: told or medicated");
    if (!st || !a || !b) return false;
    const nRows = Number(st.n_rows), nMiss = Number(st.n_stored_missing), nBoth = Number(st.n_both);
    return Number(st.disagree) === 0 && Number(st.na_mismatch) === 0
      && nRows > 0 && isFinite(nMiss) && nMiss >= 0 && nBoth + nMiss === nRows
      && Number(a.n_both) > 0 && Number(a.disagree) === 0 && Number(b.disagree) === 0;
  });
if (htnVer.length && !HTN_VERIFIED)
  console.warn("[build] hypertension verification NOT accepted (status file missing/failed, or a per-round check fails): adult results are labelled provisional. Re-run scripts/verify_hypertension_selfreport.R.");
const htnVerTxt = () => HTN_VERIFIED
  ? "Linking the raw recodes' self-reported diagnosis and medication items to the analysis file person by person confirms, in both rounds, that every respondent classified as hypertensive despite normal measured blood pressure reported a diagnosis or medication, and that no respondent with normal blood pressure who reported either was left unclassified (" + ["2015", "2019"].map((yr) => grp(Number(htnRow(yr, "flagged with normal BP: told or medicated").n_both))).join(" and ") + " such respondents in NFHS-4 and NFHS-5)."
  : "The self-reported diagnosis and medication items are not stored in the analysis file, and that part of the hypertension classification has not been verified against the raw recodes; the adult results are therefore provisional.";
// Share of each round's analysis frame with the hypertension composite missing
// under the complete-component rule (verify_hypertension_selfreport.R writes
// n_rows and n_stored_missing).
const htnMissPct = (yr) => {
  const r = htnRow(yr, "BP or told or medicated (NA if any component missing)");
  return r && isFinite(Number(r.n_rows)) && Number(r.n_rows) > 0
    ? nfmt(100 * Number(r.n_stored_missing) / Number(r.n_rows), 1) : "NA";
};
if (HTN_VERIFIED && htnMissPct("2015") === "NA")
  throw new Error("[build] diagnostics/adult_hypertension_verification.csv has no n_rows / n_stored_missing columns; re-run scripts/verify_hypertension_selfreport.R.");
const htnMissTxt = () => htnMissPct("2015") === "NA" ? "" : "The composite is missing for " + htnMissPct("2015") + "% of respondents in the NFHS-4 frame and " + htnMissPct("2019") + "% in the NFHS-5 frame. " + (HTN_VERIFIED ? "Exact reconstruction establishes that the coding is consistent across rounds, not that this missingness is ignorable:" : "This missingness may not be ignorable:") + " if respondents without complete measurements or answers differ in hypertension risk, and the difference changed between rounds, the district changes in prevalence could be affected, so the hypertension results are subject to possible selection effects.";
const htnShort = () => HTN_VERIFIED
  ? "recomputation from stored components and the raw recodes reproduces both outcome definitions in both rounds"
  : "recomputation reproduces diabetes and the measured blood-pressure part of hypertension in both rounds, but the self-reported part of hypertension is not yet verified, so these results are provisional";
// Unweighted district-mean prevalence, in percent, so the Table S5 note cannot
// drift away from the district file the adult models were actually fit on.
const adultPrev = (col) => {
  const v = adultPrevCsv.map((r) => Number(r[col])).filter((x) => isFinite(x));
  return v.length
    ? nfmt(100 * v.reduce((a, b) => a + b, 0) / v.length, 1) : "NA";
};
const N_ADULT = (() => {
  const r = adultCsv.find((x) => x.adjusted === "TRUE");
  return r ? r.n : "NA";
})();

// ---- predicted fuel-use composition metrics (Table S8) ----
const nuanced = readCsv("health_nuanced_effects.csv");
const NUANCE_ORDER = ["Primary LPG (Bayes-corrected)",
                      "LPG, no solid fuel reported (share)",
                      "LPG and solid fuel reported (share)",
                      "Any solid fuel reported (share)",
                      "Predicted LPG consumption"];
const nuancedRows = (analysis) => {
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    NUANCE_ORDER.forEach((ex) => {
      const r = nuanced.find((x) => x.analysis === analysis && x.outcome === oc
                                    && x.exposure === ex);
      if (!r) return;
      const per = r["per(+)"] === "10 kg/yr" ? " (per 10 kg/yr)" : "";
      const cr2 = (r.lo_cr2 !== undefined && r.lo_cr2 !== "" && isFinite(Number(r.lo_cr2)))
        ? nfmt(r.est_per10, 3) + " [" + nfmt(r.lo_cr2, 3) + ", " + nfmt(r.hi_cr2, 3) + "]"
        : "NA";
      out.push([OUTCOME_LAB[oc], r.exposure + per,
                cr2, (r.p_cr2 !== undefined && r.p_cr2 !== "") ? pfmt(r.p_cr2) : "NA",
                nfmt(r.est_per10, 3) + " [" + nfmt(r.lo, 3) + ", " + nfmt(r.hi, 3) + "]",
                pfmt(r.p)]);
    });
  });
  return out;
};
// Which rows of Table S8 have a CR2 interval excluding the null? Derived, not
// asserted: the caption used to state "the only interval excluding the null is
// the stacking share", which was true of the conventional intervals at the time
// it was written and is not a fact the caption can be trusted to keep.
const nuanceExcludingTxt = () => {
  const hits = [];
  ["neonatal", "infant"].forEach((oc) => {
    NUANCE_ORDER.forEach((ex) => {
      const r = nuanced.find((x) => x.analysis === "point" && x.outcome === oc
                                    && x.exposure === ex);
      if (!r) return;
      const lo = Number(r.lo_cr2), hi = Number(r.hi_cr2);
      if (isFinite(lo) && isFinite(hi) && lo * hi > 0)
        hits.push(OUTCOME_LAB[oc].toLowerCase() + " / " + ex);
    });
  });
  if (!hits.length) return "no CR2 interval in the table excludes the null";
  if (hits.length === 1) return "the only CR2 interval excluding the null is " + hits[0];
  return "the CR2 intervals excluding the null are " + joinEng(hits);
};
// Which predicted metrics point in the hypothesized direction, by outcome. The
// stacking share is excluded: more stacking can mean either more LPG among
// solid-fuel users or more solid fuel among LPG users.
const nuCorrTab = readCsv("health_nuanced_exposure_correlations.csv", { quiet: true });
const nuCorr = (a, b) => { const r = nuCorrTab.find((x) => x.metric === a); return r && r[b] !== undefined ? nfmt(r[b], 3) : "NA"; };
const NUANCE_EXPECT = { "Primary LPG (Bayes-corrected)": -1, "LPG, no solid fuel reported (share)": -1,
                        "Any solid fuel reported (share)": 1, "Predicted LPG consumption": -1 };
const NUANCE_SHORT = { "Primary LPG (Bayes-corrected)": "primary LPG", "LPG, no solid fuel reported (share)": "the LPG-no-solid-reported share",
                       "Any solid fuel reported (share)": "the any-solid-fuel share", "Predicted LPG consumption": "predicted consumption" };
const nuanceSignTxt = () => {
  const off = [];
  Object.keys(NUANCE_EXPECT).forEach((ex) => {
    const bad = ["neonatal", "infant"].filter((oc) => {
      const r = nuanced.find((x) => x.analysis === "point" && x.outcome === oc && x.exposure === ex);
      return r && isFinite(Number(r.est_per10)) && Math.sign(Number(r.est_per10)) !== NUANCE_EXPECT[ex];
    });
    if (bad.length) off.push(NUANCE_SHORT[ex] + (bad.length === 2 ? " (both outcomes)" : " (" + bad[0] + " mortality)"));
  });
  return off.length ? "the point estimates have the opposite sign for " + joinEng(off) + " and the expected sign for the other metrics"
                    : "every point estimate has the expected sign";
};
const N_NUANCE = (() => {
  const r = nuanced.find((x) => x.analysis === "point" && x.n && x.n !== "NA");
  return r ? r.n : "NA";
})();

// ---- covariate importance in the stacking model (Table S9) ----
// The covariate list is taken from the CSV, not fixed here, so a predictor
// added to or dropped from 20_predict_stacking.R appears or disappears in the
// table automatically. Education was removed from the pipeline on 2026-08-01
// because the NFHS extracts carry no attainment item, so its row is simply
// absent from the CSV and no longer has to be suppressed here.
const viStack = readCsv("var_importance_stacking.csv");
const viZcsv  = readCsv("var_importance_effect.csv");
const viCtx   = readCsv("var_importance_context_gain.csv");
const viDrop = (sv, cov) => {
  const x = viStack.find((r) => r.survey === sv && r.covariate === cov);
  return x ? nfmt(x.auc_drop, 3) : "NA";
};
const viAbsZ = (sv, cov) => {
  const x = viZcsv.find((r) => r.survey === sv && r.covariate === cov);
  return x ? nfmt(x.abs_z, 2) : "NA";
};
const viCtxRow  = (sv) => viCtx.find((r) => r.survey === sv) || {};
const viCtxGain = (sv) => nfmt(viCtxRow(sv).context_auc_gain_leaky, 3);
const viAuc     = (sv) => nfmt(viCtxRow(sv).household_only_auc, 2);
const CTX_LAB = "District LPG share (context)";
const viCovs = viStack.filter((r) => r.survey === "ACCESS W1")
  .sort((a, b) => Number(b.auc_drop) - Number(a.auc_drop))
  .map((r) => r.covariate);
const tableS9rows = viCovs
  .map((c) => [c, viDrop("ACCESS W1", c), viAbsZ("ACCESS W1", c),
                  viDrop("IRES rural", c), viAbsZ("IRES rural", c)])
  .concat([[CTX_LAB + "*", viCtxGain("ACCESS W1"), viAbsZ("ACCESS W1", CTX_LAB),
            viCtxGain("IRES rural"), viAbsZ("IRES rural", CTX_LAB)]]);

// ---- item missingness (Table S11) ----
// This table carried the education row until 2026-08-01, and that row was the
// evidence for dropping education: 100% missing in both NFHS rounds against 0%
// in ACCESS and IRES. Having acted on the evidence, 17_missingness.R no longer
// measures a variable that 01_prep_nfhs.R no longer derives, so the row is gone
// from the CSV and disappears from this table on its own. The surviving
// disclosure is the Table 1 comparability row, which is typed rather than read
// because it states a fact about the extract, not a computed share. Everything
// else here stays CSV-read: a hand-copied "0.23" once hid a variable that was
// in fact 100% missing.
const missItems = readCsv("missingness_items.csv");
// 17_missingness.R computes the ACCESS column on the full two-wave panel frame
// (17,635 household-wave observations), not on Wave 1 alone (8,563). Older
// output files label that row "ACCESS W1", which is wrong; the script now
// writes "ACCESS (both waves)". Accept either key and label the column for the
// frame that was actually used.
const MISS_ACCESS = missItems.some((r) => r.survey === "ACCESS (both waves)")
  ? "ACCESS (both waves)" : "ACCESS W1";
const MISS_SURVEYS = ["NFHS-4", "NFHS-5", MISS_ACCESS, "IRES"];
const missPct = (sv, role) => {
  const x = missItems.find((r) => r.survey === sv && r.role === role);
  return x ? nfmt(x.pct_missing, 2) : "NA";
};
const missRoles = missItems.filter((r) => r.survey === "NFHS-4").map((r) => r.role);
const tableS11rows = missRoles.map((role) =>
  [role].concat(MISS_SURVEYS.map((sv) => missPct(sv, role))));
const missN = (sv) => {
  const x = missItems.find((r) => r.survey === sv);
  return x ? Number(x.n_total).toLocaleString("en-US") : "NA";
};

// ---- brms sampler diagnostics, disclosed rather than assumed clean ----
const bayesDiag = readCsv("diagnostics/bayes_fit_diagnostics.csv");
const bdTxt = (fit) => {
  const x = bayesDiag.find((z) => z.fit === fit);
  if (!x) return "NA";
  const dv = Number(x.divergences);
  return (isFinite(dv) ? dv : "NA") + " divergent transition" + (dv === 1 ? "" : "s")
    + ", maximum R-hat " + nfmt(x.max_rhat, 3)
    + ", minimum bulk effective sample size " + nfmt(x.min_ess_bulk, 0)
    + " and minimum tail effective sample size " + nfmt(x.min_ess_tail, 0);
};

// Convergence VERDICT, derived from the same CSV rather than asserted in prose.
// The previous version of this sentence was hard-coded ("... the single divergent
// transition is one draw in 12,000, but the low bulk effective sample size ...").
// When the 2026-08-01 re-fit cleared both problems the sentence silently became
// false while the numbers beside it updated correctly -- exactly the failure this
// builder exists to prevent. Deriving the claim from the data means a future
// re-fit rewrites the verdict instead of stranding a stale one.
const bdVerdict = () => {
  const rows = ["nfhs4_access", "nfhs5_ires"]
    .map((f) => bayesDiag.find((z) => z.fit === f)).filter(Boolean);
  if (!rows.length) return "NA";
  const totDv   = rows.reduce((a, r) => a + Number(r.divergences), 0);
  const maxRhat = Math.max.apply(null, rows.map((r) => Number(r.max_rhat)));
  const minBulk = Math.min.apply(null, rows.map((r) => Number(r.min_ess_bulk)));
  const minTail = Math.min.apply(null, rows.map((r) => Number(r.min_ess_tail)));
  const dvTxt = totDv === 0
    ? "Neither fit produced a divergent transition"
    : "The two fits produced " + totDv + " divergent transition"
      + (totDv === 1 ? "" : "s") + " between them";
  const rhTxt = maxRhat < 1.01
    ? "the maximum R-hat is " + nfmt(maxRhat, 3)
      + ", within the conventional 1.01 threshold"
    : "the maximum R-hat is " + nfmt(maxRhat, 3)
      + ", above the conventional 1.01 threshold";
  const essTxt = minBulk >= 400
    ? "and the smallest bulk and tail effective sample sizes across the two fits ("
      + nfmt(minBulk, 0) + " and " + nfmt(minTail, 0)
      + ") clear the 400-draw guideline, so the credible-interval endpoints reported "
      + "below rest on an adequately explored posterior"
    : "but the smallest bulk effective sample size across the two fits ("
      + nfmt(minBulk, 0) + ") falls below the 400-draw guideline, indicating "
      + "appreciable autocorrelation that is reported here rather than assumed away";
  if (totDv === 0 && maxRhat < 1.01 && minBulk >= 400)
    return "All of these are within conventional thresholds (no divergent transitions, R-hat below 1.01, effective sample sizes above 400), so the credible-interval endpoints reported below rest on an adequately explored posterior.";
  return dvTxt + ", " + rhTxt + ", " + essTxt + ".";
};

// The chain count and post-warmup draw total in Section 2.4.3 used to be PINNED
// here, because they are arguments to 05_correction.R's sampler call rather than
// values in any output file -- the one number in the document that the audit
// could not check. 05 now exports them, so they are derived like everything
// else; the pin survives only as a fallback for an output tree written before
// that export existed, and says so loudly.
const samplerCsv = readCsv("calibration_sampler.csv", { quiet: true });
const samplerVal = (q) => {
  const r = samplerCsv.find((x) => x.quantity === q);
  const v = r ? Number(r.value) : NaN;
  return isFinite(v) ? v : NaN;
};
const HAVE_SAMPLER = isFinite(samplerVal("chains"))
                  && isFinite(samplerVal("post_warmup_draws_total"));
const MCMC_CHAINS = HAVE_SAMPLER ? samplerVal("chains") : 4;
const MCMC_DRAWS  = HAVE_SAMPLER ? samplerVal("post_warmup_draws_total") : 24000;
const MCMC_CHAINS_WORD = ({1:"one",2:"two",3:"three",4:"four",5:"five",6:"six",
                           7:"seven",8:"eight"})[MCMC_CHAINS] || String(MCMC_CHAINS);
if (!HAVE_SAMPLER)
  console.warn("[build] FALLBACK: calibration_sampler.csv is absent, so the MCMC "
    + "chain count and post-warmup draw total in Section 2.4.3 are the PINNED "
    + "values (4 chains, 24,000 draws) rather than this run's. Re-run "
    + "05_correction.R to derive them.");

// ---- head-to-head level differences, in percentage points ----
const cmpTab = readCsv("comparison_table.csv");
// External three-category composition check (SI Methods S2).
const habibTab = readCsv("si_habib_comparison.csv");
const hb = (cat, col, d = 1) => {
  const x = habibTab.find((r) => r.use3cat === cat);
  return x ? nfmt(Number(x[col]) * 100, d) : "NA";
};
const cmpGap = (name, d) => {
  const x = cmpTab.find((r) => r.comparison === name);
  return x ? nfmt(Math.abs(Number(x.mean_diff)) * 100, d === undefined ? 0 : d) : "NA";
};
// Generic accessors for the same table, so the Section 3.1 correlations move
// with a re-run instead of being pinned to the values of one run.
// District coverage counts. missingness_coverage.csv is the pipeline's own
// tally of how many districts carry an estimate, so the counts in Section 3.1
// cannot drift away from what the analysis actually produced.
const covTab = readCsv("missingness_coverage.csv");
const covN = (q) => {
  const x = covTab.find((r) => r.quantity === q);
  return x ? String(x.n_districts) : "NA";
};
const N_D_NFHS4 = covN("NFHS-4 districts with a raw multilevel estimate");
const N_D_NFHS5 = covN("NFHS-5 districts with a raw multilevel estimate");

// Benchmark (falsification) mean differences, in percentage points, signed.
const bmTab = readCsv("si_benchmark_agreement.csv");
const bmDiff = (pair, v, d = 1) => {
  const x = bmTab.find((r) => r.pair === pair && r.variable === v);
  if (!x) return "NA";
  const val = Number(x.mean_diff) * 100;
  if (!isFinite(val)) return "NA";
  return (val > 0 ? "+" : "") + nfmt(val, d);
};
const BM5 = "NFHS-5 vs IRES (rural)";
const bmR = (pair, v) => { const x = bmTab.find((r) => r.pair === pair && r.variable === v); return x ? nfmt(x.pearson, 2) : "NA"; };

// Leave-one-state-out cross-validation of the calibration.
const losoTab = readCsv("calibration_loso.csv");
const losoSum = readCsv("calibration_loso_summary.csv")[0] || {};
const losoDefined = losoTab.filter((r) => r.rmse_raw !== "NA" && r.rmse_raw !== "");
const med = (a) => {
  const v = a.slice().sort((x, y) => x - y);
  if (!v.length) return NaN;
  const m = Math.floor(v.length / 2);
  return v.length % 2 ? v[m] : (v[m - 1] + v[m]) / 2;
};
const LOSO_IMPROVED = losoSum.n_improved || "NA";
const LOSO_TESTED = losoSum.n_states_tested || "NA";
const LOSO_MED_RAW = nfmt(med(losoDefined.map((r) => Number(r.rmse_raw))), 2);
const LOSO_MED_CORR = nfmt(med(losoDefined.map((r) => Number(r.rmse_corr))), 2);

// NFHS-5 cluster-to-district linkage. Every count in the spatial-assignment
// paragraph comes from the diagnostics table the linkage script wrote.
const linkTab = readCsv("nfhs5_linkage_diagnostics.csv");
const lnk = (m) => {
  const x = linkTab.find((r) => r.metric === m);
  return x ? Number(x.value) : NaN;
};
const grp = (x) => (isFinite(x) ? Math.round(x).toLocaleString("en-US") : "NA");
const L_CL5    = lnk("NFHS-5 clusters");
const L_D5     = lnk("NFHS-5 districts (2019 frame)");
const L_NAME   = lnk("NFHS-5 clusters assigned by district name");
const L_DUP    = lnk("NFHS-5 clusters assigned by name, resolved by location (duplicate names)");
const L_PARENT = lnk("NFHS-5 clusters assigned to the 2011 parent by location");
const L_SPLIT  = lnk("NFHS-5 clusters assigned cluster by cluster by location");
const L_OVER   = lnk("NFHS-5 clusters whose name match was overridden by location");
const L_UNASS  = lnk("NFHS-5 clusters unassigned");
const L_AGREE  = nfmt(lnk("agreement of location vote with name match (name-matched clusters, before override)") * 100, 1);
const L_NOGPS  = lnk("households with unrecorded GPS (lat = lon = 0 in DHS release)");
const L_SHARE  = nfmt(lnk("share of households assigned") * 100, 1);
const L_POLY4  = nfmt(lnk("share of those in the polygon of their own DHS district") * 100, 0);
const L_LOC    = (isFinite(L_PARENT) ? L_PARENT : 0) + (isFinite(L_SPLIT) ? L_SPLIT : 0) + (isFinite(L_OVER) ? L_OVER : 0);
const pctOf5 = (a, d = 1) => nfmt((a / L_CL5) * 100, d);

// Deterministic linkage of the two reference surveys.
const accLnk = readCsv("access_linkage_diagnostics.csv");
const iresLnk = readCsv("ires_linkage_diagnostics.csv");
const dg = (tab, m) => {
  const x = tab.find((r) => r.metric === m);
  return x ? grp(Number(x.value)) : "NA";
};
// Raw numeric read of a linkage metric, for values that are neither counts nor
// need thousands separators (shares, ratios).
const dgN = (tab, m) => {
  const x = tab.find((r) => r.metric === m);
  return x ? Number(x.value) : NaN;
};
// A share stored as a proportion, rendered as a percentage.
const dgPct = (tab, m, d = 0) => nfmt(dgN(tab, m) * 100, d);

// The survey descriptions in Methods 2.2.2 and 2.2.3 used to carry typed
// household, district and state counts. That is precisely how a Wave 1 count of
// 8,568 -- correct for the published 2015 report, wrong for the appended panel
// file analysed here -- survived into the draft: it was checked against the
// report rather than against the data. 02_prep_access.R and 03_prep_ires.R now
// derive these counts and write them to their linkage CSVs; everything below
// reads them, so a re-run of the pipeline moves the manuscript and a missing
// metric reads "NA" instead of quietly keeping a stale number alive.
const A_W1_HH   = dg(accLnk, "ACCESS Wave 1 households");
const A_W2_HH   = dg(accLnk, "ACCESS Wave 2 households");
const A_PANEL   = dg(accLnk, "ACCESS households (both waves)");
const A_W1_D    = dg(accLnk, "ACCESS Wave 1 districts");
const A_W2_D    = dg(accLnk, "ACCESS Wave 2 districts");
const I_HH      = dg(iresLnk, "IRES households, total");
const I_SU      = dg(iresLnk, "IRES district sampling units (state x district name)");
const I_DIST    = dg(iresLnk, "IRES distinct NFHS-4 districts");
const I_STATES  = dg(iresLnk, "IRES states");
const I_RURAL   = dgPct(iresLnk, "share of households rural", 0);
if ([A_W1_HH, A_W2_HH, A_PANEL, A_W1_D, A_W2_D].includes("NA"))
  console.warn("[build] MISSING: ACCESS frame counts absent from "
    + "access_linkage_diagnostics.csv. Re-run 02_prep_access.R before building; "
    + "the Methods survey description will read NA until you do.");
if ([I_HH, I_SU, I_DIST, I_STATES].includes("NA") || I_RURAL === "NA")
  console.warn("[build] MISSING: IRES frame counts absent from "
    + "ires_linkage_diagnostics.csv. Re-run 03_prep_ires.R before building; "
    + "the Methods survey description will read NA until you do.");
// The 86% retention figure is the ONE number in the ACCESS description that the
// analytic file cannot produce. Every Wave 1 household identifier recurs in
// Wave 2 in the appended release (file overlap = 100%), because that release
// re-lists the Wave 1 sample (households that could not be re-interviewed were
// replaced by neighbours); 86% is the share of 2015 households re-interviewed in
// 2018, reported in the ACCESS 2018 survey report (Jain et al. 2018). It is
// therefore pinned to that published source, not derived, and says so here
// rather than passing silently as if it came from the data.
const A_RETENTION = "86%";
console.warn("[build] PINNED (not CSV-derived): ACCESS panel retention ("
  + A_RETENTION + "). This is the fielded-sample retention reported in the ACCESS "
  + "2018 survey report (Jain et al. 2018) and the "
  + "manuscript attributes it to that source rather than to the data. It cannot be derived here: the appended panel release "
  + "re-lists every Wave 1 household, so the file overlap written to "
  + "access_linkage_diagnostics.csv ("
  + nfmt(dgN(accLnk, "Wave 1 households re-listed in Wave 2 (share, this file)") * 100, 0)
  + "%) is a different quantity and cannot substitute for it.");

// Extrapolation footprint of the NFHS-5 fuel-use predictions. The pipeline's
// own diagnostic prints an unweighted mean of a district-level 0/1 flag and
// labels it a household share; it is a DISTRICT share, so both quantities are
// computed here from the proxy table and named for what they are.
const proxyTab = readCsv("district_exposure_proxy.csv");
// State ranking of NFHS-5 predicted stacking (SI Figure S6 caption), derived.
const stackStatesTxt = (high) => {
  const by = {};
  proxyTab.filter((x) => x.survey === "NFHS5" && isFinite(Number(x.p_stack))).forEach((x) => {
    (by[x.state] = by[x.state] || []).push(Number(x.p_stack)); });
  const st = Object.keys(by).filter((k) => by[k].length >= 3)
    .map((k) => [k, by[k].reduce((a, b) => a + b, 0) / by[k].length])
    .sort((a, b) => high ? b[1] - a[1] : a[1] - b[1]).slice(0, 3);
  return st.length ? joinEng(st.map((z) => z[0] + " (" + nfmt(z[1], 2) + ")")) : "NA";
};
const proxy5 = proxyTab.filter((r) => r.survey === "NFHS5");
const supDistPct = (() => {
  const v = proxy5.map((r) => Number(r.in_support)).filter((x) => isFinite(x));
  return v.length ? nfmt((100 * v.reduce((a, b) => a + b, 0)) / v.length, 1) : "NA";
})();
const supHhPct = (() => {
  let num = 0, den = 0;
  proxy5.forEach((r) => {
    const w = Number(r.n_hh), f = Number(r.in_support);
    if (isFinite(w) && isFinite(f)) { num += w * f; den += w; }
  });
  return den ? nfmt((100 * num) / den, 1) : "NA";
})();

// Birth counts behind the child-mortality outcomes (Methods 2.4.5).
// H1_prep_mortality.R now writes mortality_birth_counts.csv; the outputs on disk
// predate that writer, so the counts that run printed are pinned and flagged.
// The number of births IN THE WINDOW and the number that reached a district are
// different quantities and are reported as such.
const bcTab = readCsv("mortality_birth_counts.csv", { quiet: true });
const BC_PIN = {
  "rural|2015": 197106, "rural|2019": 185387,
  "all|2015":   258434, "all|2019":   232198,
};
if (!bcTab.length) requireCsv("mortality_birth_counts.csv", "the eligible-birth counts");
if (false) console.warn("[build] superseded: eligible-birth "
  + "counts in Section 2.4.5. Re-run H1_prep_mortality.R to generate "
  + "mortality_birth_counts.csv and source them automatically.");
// Outcome-eligible births (exposed for the full age interval), the counts the
// outcome-specific health weights are built from (H1). The health tables are
// only consistent with the weighting text if the district frame carries those
// weights, so their absence stops the build.
const eligB = (pop, yr, oc) => {
  const x = bcTab.find((r) => r.variant === pop && String(r.year) === String(yr));
  return x && x["n_eligible_" + oc] !== undefined ? grp(Number(x["n_eligible_" + oc])) : "NA";
};
const births = (pop, yr) => {
  const x = bcTab.find((r) => r.variant === pop && String(r.year) === String(yr));
  if (x) return grp(Number(x.n_births_window));
  const v = BC_PIN[pop + "|" + yr];
  return v === undefined ? "NA" : grp(v);
};
// Births that a district assignment actually reached, summed from the analysis
// frame the health models were fitted on.
const sumCol = (tab, col) => grp(tab
  .map((r) => Number(r[col])).filter((v) => isFinite(v))
  .reduce((a, b) => a + b, 0));
const hdwRural = readCsv("health_district_wide.csv");
const hdwAll   = readCsv("health_district_wide_all.csv");
[["health_district_wide.csv", hdwRural], ["health_district_wide_all.csv", hdwAll]].forEach(([f, t]) => {
  if (t.length && !("w_hmean_infant" in t[0] && "w_hmean_neonatal" in t[0]))
    throw new Error("[build] " + f + " has no outcome-specific birth weights (w_hmean_neonatal, w_hmean_infant). "
      + "The text describes outcome-eligible birth weights; re-run H1_prep_mortality.R and the health models (H2, 22, 23), then rebuild.");
});

// Geographic support of each corrected surface.
const supp = readCsv("calibration_support_summary.csv")[0] || {};
const SUPP15 = supp.n_state_support_2015 || "NA";
const SUPP19 = supp.n_state_support_2019 || "NA";

// Item-level missingness at one decimal, for prose (Table S11 uses two).
const missPct1 = (sv, role) => {
  const x = missItems.find((r) => r.survey === sv && r.role === role);
  return x ? nfmt(x.pct_missing, 1) : "NA";
};

// The eduMiss helper lived here until 2026-08-01. It read the "Low education"
// row of missingness_items.csv, a row 17_missingness.R no longer writes because
// 01_prep_nfhs.R no longer derives the variable. Nothing may interpolate an
// education share any more, and there is deliberately no helper left that could:
// the surviving disclosure is the typed Table 1 comparability row ("Education of
// respondent / head - not populated in extract"), which is a statement about the
// extract rather than a number that could go stale.

// Multilevel vs design-weighted agreement, across the four survey frames.
const mlwtTab = readCsv("ml_vs_designwt_agreement.csv");
// Per-pair mean difference (multilevel minus design-weighted), in percentage
// points, for the SI Figure S19 caption.
const mlwtDiffPP = (pair, d = 1) => {
  const x = mlwtTab.find((r) => r.pair === pair);
  return x ? nfmt(Number(x.mean_diff) * 100, d) : "NA";
};
const mlwtRange = (col, d = 2) => {
  const v = mlwtTab.map((r) => Number(r[col])).filter((x) => isFinite(x));
  if (!v.length) return "NA";
  return nfmt(Math.min.apply(null, v), d) + "-" + nfmt(Math.max.apply(null, v), d);
};

// Cross-estimator district correlations (Table S10 / SI Figure S20).
const emCorTab = readCsv("estimator_mix_correlations.csv");
const emCor = (yr, a, b, d = 2) => {
  const x = emCorTab.find((r) => String(r.year) === String(yr) && r.estimator === a);
  return x ? nfmt(x[b], d) : "NA";
};

// Season / COVID sensitivity of the NFHS-5-IRES rural gap.
const seasTab = readCsv("season_sensitivity.csv");
const seasDiff = (prefix, d = 1) => {
  const x = seasTab.find((r) => (r.variant || "").indexOf(prefix) === 0);
  return x ? nfmt(Number(x.mean_diff) * 100, d) : "NA";
};
const SEAS_ALL     = seasDiff("NFHS-5 all interviews");
const SEAS_PRECOV  = seasDiff("NFHS-5 interviews through Feb 2020");
const SEAS_WINTER  = seasDiff("NFHS-5 winter interviews only");

const cmpV = (name, col, d = 2) => {
  const x = cmpTab.find((r) => r.comparison === name);
  return x ? nfmt(x[col], d) : "NA";
};
const cmpN = (name) => {
  const x = cmpTab.find((r) => r.comparison === name);
  return x ? String(x.n_districts) : "NA";
};
const cmpDiffPP = (name, d = 1) => {
  const x = cmpTab.find((r) => r.comparison === name);
  return x ? nfmt(Number(x.mean_diff) * 100, d) : "NA";
};
const C4R  = "NFHS-4 (rural) vs ACCESS W1";
const C4RD = "NFHS-4 (rural, design-wt) vs ACCESS W1 (design-wt)";
const C4A  = "NFHS-4 (all) vs ACCESS W1";
const C5A  = "NFHS-5 (all) vs IRES (all)";
const C5R  = "NFHS-5 (rural) vs IRES (rural)";
const C5RD = "NFHS-5 (rural, design-wt) vs IRES (rural, design-wt)";
const GAP_ML = cmpGap("NFHS-5 (rural) vs IRES (rural)");
const GAP_DW = cmpGap(C5RD);

// ---- estimator / correction level ladder ----
// Table S10, and the correction magnitude quoted in the Abstract, Section 3.2
// and the Discussion, all come out of estimator_mix_levels.csv so that a re-run
// moves the number in every place it is stated.
const estMix = readCsv("estimator_mix_levels.csv");
const emx = (yr, est) => {
  const x = estMix.find((r) => String(r.year) === String(yr) && r.estimator === est);
  return x ? Number(x.mean_prev) : NaN;
};
const RAW19 = emx(2019, "Raw multilevel");
const BAY19 = emx(2019, "Bayesian-corrected");
const CORR_PP = nfmt(BAY19 - RAW19, 0);
const RAW19_0 = nfmt(RAW19, 0);
const BAY19_0 = nfmt(BAY19, 0);
const RC19_0 = nfmt(emx(2019, "Reg.-calibrated (multilevel input)"), 0);
// These means are unweighted averages over districts (estimator_mix_levels.csv):
// each district counts once whatever its population, so they are never called a
// national household prevalence.
// Where the NFHS-5 correction is largest in percentage points, from
// estimator_mix_revision_bands.csv (18_correction_comparison.R). The calibration
// slope is a logit-scale quantity, so the percentage-point pattern is read from
// the district outputs rather than inferred from the slope.
const revBands = requireCsv("estimator_mix_revision_bands.csv", "the percentage-point revision pattern")
  .filter((r) => String(r.year) === "2019" && r.estimator === "Bayesian-corrected" && Number(r.n) > 0)
  .sort((a, b) => Number(a.band_lo) - Number(b.band_lo));
const bandLbl = (r) => Number(r.band_lo) === 0 ? "below " + r.band_hi + "%"
  : Number(r.band_hi) >= 100 ? r.band_lo + "% or above" : r.band_lo + "-" + r.band_hi + "%";
const revTxt = () => {
  if (!revBands.length) return "NA";
  const pk = revBands.reduce((a, b) => Number(b.mean_rev_pp) > Number(a.mean_rev_pp) ? b : a);
  const lo = revBands[0], hi = revBands[revBands.length - 1];
  const pp = (r) => (Number(r.mean_rev_pp) >= 0 ? "+" : "") + nfmt(r.mean_rev_pp, 1);
  if (pk === lo) return "in percentage points the district revisions are largest where the raw NFHS-5 estimate is lowest (mean " + pp(lo) + " points " + bandLbl(lo) + ", " + pp(hi) + " points at " + bandLbl(hi) + ")";
  if (pk === hi) return "in percentage points the district revisions are largest where the raw NFHS-5 estimate is highest (mean " + pp(hi) + " points at " + bandLbl(hi) + ", " + pp(lo) + " points " + bandLbl(lo) + ")";
  return "in percentage points the district revisions are largest for districts with raw estimates of " + bandLbl(pk) + " (mean " + pp(pk) + " points) and smaller at both ends (" + pp(lo) + " points " + bandLbl(lo) + ", " + pp(hi) + " points at " + bandLbl(hi) + ")";
};

// Levels behind each head-to-head gap (18_correction_comparison.R). Section 3.1
// states the two district-mean prevalences a gap is the difference of, so the
// reader sees the levels as well as the percentage-point difference.
const cmpLvlTab = readCsv("comparison_levels.csv");
const cmpLvl = (name) => {
  const x = cmpLvlTab.find((r) => r.comparison === name);
  return x ? nfmt(x.mean_nfhs_pct, 0) + "% in NFHS against " + nfmt(x.mean_ref_pct, 0) + "%" : "NA";
};
// Geography of the 2019 correction, by state (18_correction_comparison.R).
// State labels in the outputs follow the NFHS-4 spelling; map the few that
// would read as typos in prose.
const ST_LABEL = { "Uttarakhaand": "Uttarakhand", "Chattisgarh": "Chhattisgarh",
  "Nct Of Delhi": "Delhi", "Jammu & Kashmir": "Jammu and Kashmir" };
const stRevTab = readCsv("correction_revision_by_state_2019.csv");
const stRevTxt = () => {
  if (!stRevTab.length) return "NA";
  const tot = stRevTab.reduce((a, r) => a + Number(r.n), 0);
  const up = stRevTab.reduce((a, r) => a + Number(r.n_up), 0);
  const downSt = stRevTab.filter((r) => Number(r.n_down) > 0).map((r) => ST_LABEL[r.state] || r.state);
  const big = stRevTab.filter((r) => Number(r.n) >= 10)
    .sort((a, b) => Number(b.mean_rev_pp) - Number(a.mean_rev_pp));
  const nm = (rs) => { const l = rs.map((r) => ST_LABEL[r.state] || r.state);
    return l.length > 1 ? l.slice(0, -1).join(", ") + " and " + l[l.length - 1] : l[0]; };
  const rng = (rs) => { const v = rs.map((r) => Number(r.mean_rev_pp));
    return nfmt(Math.min(...v), 0) + " to " + nfmt(Math.max(...v), 0); };
  const top = big.slice(0, 5), bot = big.slice(-4).reverse();
  return "The revision is upward in " + up + " of " + tot + " districts"
    + (downSt.length ? " (all but those in " + (downSt.length > 1 ? downSt.slice(0, -1).join(", ") + " and " + downSt[downSt.length - 1] : downSt[0]) + ", which are revised slightly downward)" : "")
    + ". Among states with at least ten districts it is largest in " + nm(top) + " (state means of " + rng(top) + " points) and smallest in " + nm(bot) + " (" + rng(bot) + " points). Because the measurement-error model carries state effects, every district in a given state is revised in the same direction, and these differences partly reflect fitted state-level departures from the common calibration line (between-state SD " + calE(CAL5, "sd_state") + " on the logit scale) rather than district-level evidence";
};

// Side-by-side spatial benchmark agreement (SI Figures S17-S18), written by
// 14_benchmark_side_by_side.R. These are the map-panel statistics; they are
// computed on a different estimator basis from si_benchmark_agreement.csv
// (Table S4), so these captions must read this file and not that one.
const sbsTab = readCsv("benchmark_sidebyside_agreement.csv");
const sbs = (pair, v, col, d = 2) => {
  const x = sbsTab.find((r) => r.pair === pair && r.variable === v);
  return x ? nfmt(x[col], d) : "NA";
};
const sbsDiff = (pair, v, d = 1) => sbs(pair, v, "mean_diff_pp", d);
const S10_ORDER = [
  "Raw multilevel",
  "Raw design-weighted",
  "Reg.-calibrated (multilevel input)",
  "Reg.-calibrated (design-wt input)",
  "Bayesian-corrected",
];
const S10_LAB = {
  "Reg.-calibrated (multilevel input)": "Regression-calibrated (multilevel input)",
  "Reg.-calibrated (design-wt input)": "Regression-calibrated (design-weighted input)",
};
const tableS10rows = S10_ORDER.map((e) =>
  [S10_LAB[e] || e, nfmt(emx(2015, e), 1), nfmt(emx(2019, e), 1)]);

// ============================ TABLE 1 (measures) ============================
const COLW1 = [3400, 1900, 1900, 2160];
const table1 = mkTable(COLW1, [
  ["Measure", "NFHS-4/5", "ACCESS (W1/W2)", "IRES"],
  ["Main cooking fuel (LPG / electricity / solid)", "Yes", "Yes", "Yes (PNG coded separately)"],
  ["Use of multiple fuels (firewood, dung, agricultural residue)", "No", "Yes", "Yes"],
  ["LPG named with no solid fuel named", "No", "Derived", "Yes (direct item; derived measure used)"],
  ["LPG cylinder refills (quantity of use)", "No", "Yes", "Yes"],
  ["PMUY beneficiary status", "No", "Wave 2 only", "Yes"],
  ["Caste (SC / ST / OBC / General)", "Yes", "Yes", "Yes"],
  ["Religion (Hindu / Muslim / Other)", "Yes", "Yes", "Yes"],
  ["Education of respondent / head", "Not populated in extract", "Yes", "Yes"],
  ["Wealth index (NFHS) / monthly expenditure (ACCESS, IRES)", "Yes", "Yes", "Yes"],
  ["Household size", "Yes", "Yes", "Yes"],
  ["BPL / ration card", "Yes", "Yes", "Yes"],
  ["Household electricity", "Yes", "Yes", "Yes"],
  ["Urban and rural households", "Yes", "Rural only", "Yes"],
  ["Health and anthropometric outcomes", "Yes", "No", "No"],
  ["Coverage", "National, all districts",
    "6 states, " + A_W1_D + "/" + A_W2_D + " districts",
    I_STATES + " states, " + I_DIST + " districts (" + I_SU + " sampling units)"],
]);

// ============================ TABLE 2 (agreement) ===========================
const COLW2 = [3550, 800, 950, 950, 1050, 800, 1000];
// Table 2 is generated row-for-row from comparison_table.csv, in the order the
// analysis wrote it, so a re-run that adds, drops, or moves a comparison is
// reflected in the table instead of leaving a stale row behind. The row label
// is the CSV's own comparison string.
const sgn3 = (v) => {
  const x = Number(v);
  return isFinite(x) ? (x > 0 ? "+" : "") + nfmt(x, 3) : "NA";
};
const table2 = mkTable(COLW2, [
  ["Comparison", "N", "Pearson r", "Spearman", "Sample-wt r", "CCC", "Mean diff"],
].concat(cmpTab.map((rw) => [
  rw.comparison,
  String(rw.n_districts),
  nfmt(rw.pearson, 2),
  nfmt(rw.spearman, 2),
  nfmt(rw.pearson_ref_samplewt, 2),
  nfmt(rw.ccc, 2),
  sgn3(rw.mean_diff),
])));

// ============================ TABLE 3 (calibration) =========================
// The two brms measurement-error fits. 05_correction.R now writes its own
// summary to calibration_parameters.csv; until that script is re-run the file
// does not exist, so the values verified against the 2026-07-31 run log are
// pinned here and the build says so out loud rather than pretending they were
// sourced. Every pinned number below is transcribed from that log's
// summary(bA) / summary(bB) blocks.

// ---- Measurement-error SE construction (Section 2.4.2/2.4.3, SI Method S1) ---
// Which districts get a design-based reference SE and which fall back to the
// binomial form, where the pre-fit bounds actually bind, and how large the
// IRES rural subsample really is. These are properties of the data prep inside
// 05_correction.R's prep_me(), not of the posterior, so they were verified
// directly against compare_pairs.rds. 05_correction.R now writes them to
// me_se_diagnostics.csv; until it is re-run they are pinned here and the build
// says so, in line with every other pinned value in this file.
const meTab = readCsv("me_se_diagnostics.csv", { quiet: true });
const ME_PIN = {
  b_design: "142", b_n: "144", b_fallback: "2", b_allyes_n: "17", a_n: "51",
  clamp_raw: "7.67", y_lo: "0.16", y_hi: "0.97", x_lo: "0.09", x_hi: "0.46",
  ires_rural_med: "79", ires_rural_min: "16", ires_rural_max: "128",
};
if (!meTab.length) {
  requireCsv("me_se_diagnostics.csv", "the measurement-error standard-error diagnostics");
  console.warn("[build] superseded: measurement-error SE "
    + "construction counts and bounds (Section 2.4.2, SI Method S1). Re-run "
    + "05_correction.R to generate me_se_diagnostics.csv and source them "
    + "automatically.");
}
const meQ = (key) => {
  const x = meTab.find((r) => r.quantity === key);
  return x ? String(x.value) : (ME_PIN[key] !== undefined ? ME_PIN[key] : "NA");
};
const ME_B_DESIGN = meQ("b_design"), ME_B_N = meQ("b_n");
const ME_B_FALLBACK = meQ("b_fallback"), ME_B_ALLYES_N = meQ("b_allyes_n");
const ME_A_N = meQ("a_n"), ME_CLAMP_RAW = meQ("clamp_raw");
const ME_Y_LO = meQ("y_lo"), ME_Y_HI = meQ("y_hi");
const ME_X_LO = meQ("x_lo"), ME_X_HI = meQ("x_hi");
const ME_IRES_RURAL_MED = meQ("ires_rural_med");
const ME_IRES_RURAL_MIN = meQ("ires_rural_min");
const ME_IRES_RURAL_MAX = meQ("ires_rural_max");
// Which bounds bind, read from the diagnostics rather than asserted. The
// district-specific wording is used only when the counts single out the one
// reference-side district clamped at the cap (with its prevalence at 1).
const seBoundTxt = () => {
  const numWord = (k) => (["no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"][k] || String(k));
  const n = (k) => { const v = Number(meQ(k)); return isFinite(v) ? v : NaN; };
  const fl = n("n_y_at_floor") + n("n_x_at_floor"), hi = n("n_clamped_hi"),
        xc = n("n_x_at_cap"), pc = n("n_prevalence_clamped");
  if (![fl, hi, xc, pc].every(isFinite)) {
    console.warn("[build] me_se_diagnostics.csv lacks the bound counts; using the pinned wording in Section 2.4.2.");
    return "The floor binds for no district. The upper bound binds for exactly one - the Andhra Pradesh district whose IRES rural prevalence is 1.000, which is moved to 0.999 and whose resulting binomial logit standard error of " + ME_CLAMP_RAW + " is reduced to " + meQ("se_cap") + ".";
  }
  const floorTxt = fl === 0 ? "The floor binds for no district." : "The floor binds for " + numWord(fl) + " district standard error" + (fl === 1 ? "." : "s.");
  if (hi === 1 && xc === 0 && pc === 1)
    return floorTxt + " The upper bound binds for exactly one - the Andhra Pradesh district whose IRES rural prevalence is 1.000, which is moved to 0.999 and whose resulting binomial logit standard error of " + ME_CLAMP_RAW + " is reduced to " + meQ("se_cap") + ".";
  console.warn("[build] SE-bound counts changed (reference-side at cap " + hi + ", NFHS-side at cap " + xc + ", prevalences clamped " + pc + "); review Section 2.4.2 wording.");
  return floorTxt + " The upper bound binds for " + numWord(hi + xc) + " district standard error" + (hi + xc === 1 ? "" : "s") + " (" + numWord(hi) + " reference-side, " + numWord(xc) + " NFHS-side), and " + numWord(pc) + " district prevalence" + (pc === 1 ? " is" : "s are") + " moved off the boundary.";
};

const CAL4 = "NFHS-4 ~ ACCESS W1 (rural)";
const CAL5 = "NFHS-5 ~ IRES (rural)";
const calTab = readCsv("calibration_parameters.csv", { quiet: true });
const CAL_PIN = {
  "NFHS-4 ~ ACCESS W1 (rural)": {
    slope: [0.79, 0.62, 0.96], intercept: [-0.47, -0.94, -0.05],
    sd_state: [0.2, 0.01, 0.56], sd_residual: [0.1, 0.0, 0.24], n_districts: [51],
  },
  "NFHS-5 ~ IRES (rural)": {
    slope: [0.63, 0.43, 0.83], intercept: [0.83, 0.52, 1.13],
    sd_state: [0.56, 0.29, 0.9], sd_residual: [0.7, 0.58, 0.85], n_districts: [144],
  },
};
if (!calTab.length) {
  requireCsv("calibration_parameters.csv", "the Bayesian calibration parameters");
  console.warn("[build] superseded: Bayesian calibration slope, "
    + "intercept and SDs (Table S3, Section 3.2). Re-run 05_correction.R to "
    + "generate calibration_parameters.csv and source them automatically.");
}
// One accessor for both paths, so the pinned and sourced documents are built by
// the same code and cannot diverge in formatting.
const calP = (cal, param) => {
  const x = calTab.find((r) => r.calibration === cal && r.parameter === param);
  if (x) return [Number(x.estimate), Number(x.lo95), Number(x.hi95)];
  const pin = CAL_PIN[cal] || {};
  return pin[param] || [NaN, NaN, NaN];
};
const calE = (cal, param, d = 2) => nfmt(calP(cal, param)[0], d);

// ---- Leg-A symmetry sensitivity (05_correction.R + H2_health_models.R) ------
// The NFHS-4 calibration leg uses the ACCESS MULTILEVEL district estimate as its
// reference, and no Taylor design standard error exists for that estimate, so the
// reference-side measurement error falls back to the simple-binomial form. The
// NFHS-5 leg has no such fallback. These three CSVs are the refit that removes
// the asymmetry: the same leg fitted with the ACCESS DESIGN-WEIGHTED estimate and
// its Taylor standard error, i.e. exactly the NFHS-5 treatment. Nothing below is
// typed; if the CSVs are absent the build says so and the sentence reads NA.
const CAL4WT = "NFHS-4 ~ ACCESS W1 design-weighted (rural, SENSITIVITY)";
const calWtTab = readCsv("calibration_parameters_legA_wt.csv");
const calWtE = (param, d = 2) => {
  const x = calWtTab.find((r) => r.parameter === param);
  return x ? nfmt(x.estimate, d) : "NA";
};
const calWtCI = (param, d = 2) => {
  const x = calWtTab.find((r) => r.parameter === param);
  return x ? "[" + nfmt(x.lo95, d) + ", " + nfmt(x.hi95, d) + "]" : "NA";
};
if (calWtTab.length && !calWtTab.some((r) => r.calibration === CAL4WT)) {
  console.warn("[build] LABEL MISMATCH: calibration_parameters_legA_wt.csv does "
    + "not contain the expected calibration label. Found: "
    + Array.from(new Set(calWtTab.map((r) => r.calibration))).join(" | "));
}
const legASensTab = readCsv("legA_symmetry_sensitivity.csv");
const legAQ = (key, d) => {
  const x = legASensTab.find((r) => r.quantity === key);
  return x ? (d === undefined ? String(x.value) : nfmt(x.value, d)) : "NA";
};
const legAMoveTab = readCsv("legA_symmetry_health_movement.csv");
const legAMoveRow = (cmp, oc) =>
  legAMoveTab.find((r) => r.comparison === cmp && r.outcome === oc) || null;
// The verdict word is COMPUTED, not asserted: the largest movement of an
// uncertainty-propagated coefficient, expressed in units of that coefficient's
// own standard error, decides which of three phrasings the sentence uses. A
// re-run that moved the result would therefore change the wording too, instead
// of leaving a favourable adjective standing over an unfavourable number.
const legAMaxZ = (() => {
  const rows = legAMoveTab.filter((r) => r.comparison === "point surface" || r.comparison === "exposure-only MI (sensitivity)");
  const zs = rows.map((r) => Math.abs(Number(r.diff_in_primary_se)))
                 .filter((z) => isFinite(z));
  return zs.length ? Math.max.apply(null, zs) : NaN;
})();
const legAVerDICT = !isFinite(legAMaxZ) ? "could not be evaluated"
  : legAMaxZ < 0.25 ? "leaves the child-mortality estimates essentially unchanged"
  : legAMaxZ < 0.5  ? "moves the child-mortality estimates by well under half of their own standard errors"
  : "moves the child-mortality estimates materially, and the fixed-surface specification should be read with that in mind";
const legAAnyFlip = legAMoveTab.some((r) => String(r.sign_flip) === "TRUE") ||
                    legAMoveTab.some((r) => String(r.sig_flip) === "TRUE");
if (legASensTab.length && legAAnyFlip) {
  console.warn("[build] ATTENTION: the leg-A symmetry sensitivity changes a sign "
    + "or a significance verdict in legA_symmetry_health_movement.csv. The SI "
    + "sentence reports this, but the Discussion should address it explicitly.");
}
const calCI = (cal, param, d = 2) => {
  const v = calP(cal, param);
  return "[" + nfmt(v[1], d) + ", " + nfmt(v[2], d) + "]";
};
const calEC = (cal, param, d = 2) => calE(cal, param, d) + " " + calCI(cal, param, d);
const calCrI = (cal, param, d = 2) => {
  const v = calP(cal, param);
  return nfmt(v[1], d) + "-" + nfmt(v[2], d);
};
const calN = (cal) => nfmt(calP(cal, "n_districts")[0], 0);

const COLW3 = [3100, 3000, 3000];
const table3 = mkTable(COLW3, [
  ["Parameter (logit scale)", CAL4, CAL5],
  ["Slope on logit(NFHS) [95% CrI]", calEC(CAL4, "slope"), calEC(CAL5, "slope")],
  ["Intercept [95% CrI]", calEC(CAL4, "intercept"), calEC(CAL5, "intercept")],
  ["Between-state SD", calEC(CAL4, "sd_state"), calEC(CAL5, "sd_state")],
  ["Residual SD (posterior mean)", calEC(CAL4, "sd_residual"), calEC(CAL5, "sd_residual")],
  ["Calibration districts", calN(CAL4), calN(CAL5)],
  ["Held-out states with RMSE reduced (LOSO)", "-", LOSO_IMPROVED + " of " + LOSO_TESTED],
  ["Median held-out RMSE, raw -> corrected", "-", LOSO_MED_RAW + " -> " + LOSO_MED_CORR],
]);

// ---- Table S4: benchmark (falsification) agreement, generated from CSV ------
// Every cell comes from si_benchmark_agreement.csv, written by 08_si_benchmarks.R,
// in that file's own row order. The variable keys are mapped to display labels
// here; an unrecognized key is printed as-is so a new benchmark variable shows
// up in the table rather than being silently dropped.
const S4_PAIR = {
  "NFHS-4 vs ACCESS W1 (rural)": "NFHS-4 vs ACCESS W1",
  "NFHS-5 vs IRES (rural)": "NFHS-5 vs IRES",
};
const S4_VAR = {
  lpg: "Primary LPG", sc: "SC", st: "ST", scst: "SC/ST", hindu: "Hindu",
  muslim: "Muslim", electricity: "Electricity", bpl: "BPL card",
};
// The ration-card row carries the footnote marker, on the NFHS-5/IRES pair only,
// exactly where the note below the table applies.
const benchTab = readCsv("si_benchmark_agreement.csv");
const tableS4 = mkTable([3000, 1900, 900, 1000, 1000, 1300], [
  ["Pair", "Variable", "N", "Pearson r", "CCC", "Mean diff"],
].concat(benchTab.map((rw) => [
  S4_PAIR[rw.pair] || rw.pair,
  (S4_VAR[rw.variable] || rw.variable)
    + (rw.variable === "bpl" && String(rw.pair).indexOf("IRES") >= 0 ? "*" : ""),
  String(rw.n),
  nfmt(rw.pearson, 2),
  nfmt(rw.ccc, 2),
  sgn3(rw.mean_diff),
])));

// ============================ TEXT BLOCKS ====================================
const intro = [
  p("The Demographic and Health Surveys (DHS), including India's National Family Health Survey (NFHS), were designed to measure maternal and child health, but their national scale and their linkage of exposures to health outcomes have made them one of the few instruments available for population health research in low- and middle-income countries, and researchers have repurposed them well beyond their original intent. Environmental epidemiology is a prominent example: studies of household air pollution routinely assign exposure from the NFHS cooking-fuel item, which records only a household's primary cooking fuel. No secondary fuel is recorded, so a household cooking with both liquefied petroleum gas (LPG) and firewood is counted as a clean-fuel user, and what the item measures therefore depends on how common such fuel stacking is. Whether such a simplified item adequately represents environmental exposure - and, if not, whether it can be corrected using better-measured data - is a general question for exposure assessment; clean cooking in India, where a single survey item defines cooking exposure for 1.4 billion people amid rapid policy change, provides an unusually clear test case."),
  p("India is undergoing the world's largest clean-cooking transition. The flagship Pradhan Mantri Ujjwala Yojana (PMUY) has released more than 103 million subsidized LPG connections to poor households since 2016 [5], and both NFHS and the independent 78th round of the National Sample Survey register the shift: primary clean-fuel use rose from 43.8% in NFHS-4 (2015-16) to 58.6% in NFHS-5 (2019-21) [6], and the National Sample Survey round reported 62% of households using LPG as their primary fuel in 2020-21 [7]; both surveys, built on different questionnaires, show LPG as the majority primary fuel by 2019-21, although they differ on its level. Household air pollution (HAP) from solid cooking fuels remains one of the largest environmental health risks worldwide - responsible for roughly 2.9 million premature deaths [1] and an estimated 111 million disability-adjusted life-years in 2021, with the highest burden rates in sub-Saharan Africa and South Asia [46] and the largest absolute burden in India [2]; kitchen PM2.5 far exceeds air-quality guidelines, and household combustion is a major source of ambient particulate matter across South Asia [3, 4] - so whether this expansion has improved health is a first-order question. Demonstrating such gains, however, has proved difficult: reviews of the epidemiological evidence, including randomized trials of clean-fuel interventions, report largely null or modest effects [38]."),
  p("That difficulty is, in part, a measurement problem: a single clean-versus-polluting classification cannot represent how the transition actually unfolds. The programs drove adoption far more than sustained use, so binary access measures overstate health-relevant progress [35]; a difference-in-differences analysis finds India's below-poverty-line policies, including PMUY, raised the probability of any LPG use by about eight percentage points but left the quantity consumed by using households essentially unchanged [36], and PMUY beneficiaries in particular use roughly 30% less LPG than general customers and take few refills a year [9]. Rather than climbing a clean rung of an energy ladder, households occupy an overlapping 'energy stack' [30, 31]: even as LPG ownership expanded across energy-poor northern states, most kept burning solid fuels, the share stacking LPG and biomass rose from 17% to 38% between 2015 and 2018, and exclusive LPG use reached only 17% [8]; a third to a half of households reporting LPG as their primary fuel still burn a solid fuel at least daily and often change which fuel they call primary across seasons [32]. Stacking is also dynamic rather than a fixed household trait: sustained LPG use depends on the affordability of refills and access to distributors [9], and fuel use responds to external shocks - during the COVID-19 pandemic some rural north Indian households relied more on polluting fuels after job losses, even as most used the free LPG cylinders the government provided [34]. Clean-fuel use also rises with socioeconomic status, while many rural households never switch cleanly [37]. Because household PM2.5 responds nonlinearly to residual solid-fuel use, such partial displacement yields far smaller exposure reductions than the binary label implies, so a household recorded as clean may be exposed much like a solid-fuel household [10, 11]. The NFHS item compounds this: reported by a single respondent without probing of secondary fuels or quantities, it may misclassify households in ways that vary with wealth, caste, and geography, attenuating and biasing estimated health effects of clean cooking and of programs such as PMUY [12]."),
  p("The measurements that reveal these patterns come from dedicated household energy surveys, which capture the full fuel portfolio but only in limited areas. The Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS) surveyed " + A_W1_HH + " rural households in six energy-poor northern and eastern states in 2014-15 and " + A_W2_HH + " in 2018 [13, 14], and the India Residential Energy Survey (IRES, 2019-20) covered " + I_HH + " households across " + I_DIST + " districts in " + I_STATES + " states [15]. Both record fuel stacking, cylinder refills, and consumption in detail, but neither collects health outcomes or supports nationwide district-level health analysis - the very strengths of NFHS."),
  p("Correcting the NFHS cooking-fuel measure is not an end in itself but a prerequisite for using the survey to evaluate the transition: if the item misclassifies the households the program most affects, then every downstream quantity built on it - the national count of clean-cooking households, the modeled burden of household air pollution, and the estimated health return to programs such as PMUY - inherits that error, and the direction and magnitude of the resulting bias cannot be assumed without knowing how the item fails. In this study we therefore use these energy surveys to evaluate, calibrate, and augment the NFHS exposure, and the paper is organized around three questions. First, does the NFHS cooking-fuel measure remain accurate across the transition? We estimate district-level primary-LPG prevalence in NFHS-4, NFHS-5, ACCESS, and IRES with a common multilevel small-area framework, test where NFHS and the energy surveys agree for the two temporally overlapping pairs - NFHS-4 with ACCESS, NFHS-5 with IRES - and, where they disagree, ask whether the gap can be attributed to sampling, timing, geography, or demographics rather than to the fuel question itself. Second, can the energy surveys correct the error? Treating them as higher-fidelity references - they establish each household's full fuel portfolio within a dedicated energy module before asking which fuel is primary - we correct the NFHS district estimates by regression calibration and a Bayesian measurement-error model, producing corrected, uncertainty-quantified prevalence for every district, checking by leave-one-state-out cross-validation of the regression calibration whether the calibration relationship transports across states, and decomposing the corrected value's uncertainty into the part that comes from the calibration and the part that comes from the health survey itself. Third, what do the energy surveys reveal about fuel stacking that the NFHS item cannot record, and can stacking be recovered from the characteristics NFHS does measure? We ask where stacking varies, whether household characteristics predict it in places where it was not observed and across survey instruments, and whether the NFHS main-fuel answer shifts with the season. Finally, as an exploratory illustration, we carry the corrected exposure into a district change-on-change analysis of child mortality, and report what that analysis can and cannot claim when the exposure is itself an estimate. The paper's contribution is principally empirical: a systematic district-level comparison of NFHS clean-cooking indicators with purpose-built energy surveys, which shows that a single, heavily used survey item changed its relationship to household fuel use over the period a national program was changing that use, shows by falsification against comparably measured characteristics that the change is far larger for the fuel question than for who was surveyed, delivers corrected, uncertainty-quantified exposure surfaces, and characterizes the fuel stacking the item misses. It also documents, for any analysis that adopts a calibrated exposure product, which parts of that product's uncertainty belong to the calibration and which to the health survey, and why carrying that uncertainty into a health model is not a routine step."),
];

const methodsOverview = [
  p("We combine four household surveys in three analytic stages: (i) common district-level estimation of primary-LPG prevalence in each survey; (ii) cross-survey comparison and measurement-error correction of the NFHS estimates; and (iii) prediction of fuel stacking and LPG consumption in NFHS from models trained in the energy surveys."),
  p("All analyses use the NFHS-4 district frame (the 640 districts of the 2011 Census). NFHS-4 households, births and women are assigned to districts by the district code DHS releases with every NFHS-4 cluster (DHSREGCO), which is the NFHS-4 district. NFHS-5 reports districts on a later administrative frame (" + grp(L_D5) + " districts), so each NFHS-5 district was mapped to the 2011 district it belongs to. Of " + grp(L_CL5) + " NFHS-5 clusters, " + grp(L_NAME) + " (" + pctOf5(L_NAME) + "%) lie in districts whose name is that of a 2011 district in the same state and were assigned to it. The remaining " + grp(L_LOC) + " (" + pctOf5(L_LOC) + "%) - in districts created after 2011 or renamed, plus " + grp(L_OVER) + " name-matched clusters whose name match was overridden by location - were assigned by location: each cluster's published GPS location was compared with the " + "seven nearest NFHS-4 clusters in the same state, whose district codes are known. Pooling these neighbour votes over all of the NFHS-5 district's clusters, the district was assigned to the 2011 district receiving at least 60% of them (its 2011 parent); where no 2011 district reached 60% - the " + grp(L_SPLIT) + " clusters in districts formed from parts of several 2011 districts - each cluster went to its own nearest-neighbour majority. A name match was overridden in favour of the location vote only where at least 80% of the district's pooled neighbour votes pointed to one other district (" + grp(L_OVER) + " clusters). As a check, before any override the district's pooled neighbour vote agrees with the name-based assignment for " + L_AGREE + "% of clusters in name-matched districts. Clusters released without GPS coordinates are assigned through their district like any other, so " + L_SHARE + "% of NFHS-5 households were assigned (" + grp(L_UNASS) + " clusters unassigned). We did not assign clusters by overlaying their GPS locations on a district boundary file: for NFHS-4, where the true district is known, point-in-polygon on the boundary file available to us placed only " + L_POLY4 + "% of clusters in their own district (SI Figure S3). ACCESS households were linked to NFHS-4 districts deterministically through the 2011 Census district codes carried in the ACCESS data (" + dg(accLnk, "households matched to an NFHS-4 district (by census code)") + " of " + dg(accLnk, "ACCESS households (both waves)") + " households; all " + dg(accLnk, "ACCESS districts, total") + " districts). IRES households were linked by census district code, with state-aware district-name matching as a fallback; the survey\u0027s " + I_SU + " sampling units map to " + I_DIST + " census districts (Nadia, West Bengal, was sampled as two units), and all " + dg(iresLnk, "IRES households, total") + " households were linked. State names were harmonized across sources. Linkage diagnostics are tabulated in SI Table S21 and mapped in SI Figures S1-S4."),
  p("Because ACCESS sampled only rural households, all head-to-head comparisons restrict NFHS to rural clusters (the NFHS-5/IRES comparison is additionally reported for all households). For the same reason, the measurement-error correction (Section 2.4.3) and the child-mortality and adult-outcome analyses (Sections 2.4.5 and 2.4.7) are conducted on rural households, which is the main specification throughout, and the fuel-use predictions (Section 2.4.4) enter the health analysis as rural district aggregates; all-household versions are reported in the Supplementary Materials as sensitivity analyses. Throughout, \"rural\" therefore denotes the main analysis and \"all-household\" the supplementary sensitivity. ACCESS Wave 2 (2018) is not used for calibration, as no NFHS round overlaps it in time; it enters only the descriptive maps and the fuel-stacking analyses (Section 2.4.4)."),
];

const nfhsText = [
  p("Data from the NFHS-4 survey conducted between January 2015 and December 2016 [16] and the NFHS-5 survey conducted between June 2019 and April 2021 [6] were used. NFHS are nationally representative household sample surveys measuring indicators of socioeconomic status, demographics, health, and nutrition, with special emphasis on maternal and child health. They have a two-stage design, in which clusters (villages in rural areas and census enumeration blocks in urban areas) are first selected from each district (640 districts at the time of the 2011 census for NFHS-4; 707 districts as of 2017 for NFHS-5), and then 22 households are selected by equal-probability systematic sampling in each selected cluster; women of reproductive age (15-49 years) in every selected household, and men (15-54 years) in a subsample of households, are then selected for in-depth interviews [17]. We use the household recode (fuel, covariates and household weights), the births recode (child mortality), the women's recode (health-model covariates) and, for the adult outcomes, women's and men's records, together with the cluster GPS locations."),
];

const accessText = [
  p("The Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS) surveys were conducted in 2014-15 and 2018 [13, 14]. The two waves were administered in six energy-poor contiguous states of India: Bihar, Jharkhand, Uttar Pradesh, Odisha, Madhya Pradesh, and West Bengal; these northern and eastern states have historically been energy-poor and combined account for about 500 million individuals, roughly 40% of the country's population [44]. Wave 1 was administered to " + A_W1_HH + " households between November 2014 and May 2015, and Wave 2 to " + A_W2_HH + " households between March 2018 and September 2018 (interview dates in the released panel file; the survey reports give December 2014 to March 2015, with a re-survey in Odisha in May 2015, and March to June 2018 [44]); the survey documentation reports " + A_RETENTION + " retention of Wave 1 households across the two waves, with households that could not be re-interviewed replaced by a neighbouring household [44]. The panel includes " + A_PANEL + " household-wave observations. The appended two-wave panel release analysed here contains " + A_W1_HH + " Wave 1 households, marginally fewer than the 8,568 reported in the published ACCESS 2015 report [13] and the 8,566 records in the standalone 2015 microdata release [45], of which they are a subset; the counts reported throughout describe the file analysed."),
  p("Sampling followed a three-stage stratified design. One district was sampled from each administrative division of each state (two per division in West Bengal), for " + A_W1_D + " districts in Wave 1 and " + A_W2_D + " in Wave 2 (Wave 2 added three districts in Odisha), the district being chosen at random within its division. Within each district, villages were split into two strata (large and small villages) comprising equal numbers of rural households; seven villages were sampled from each stratum with probability proportional to their household count, and twelve households were sampled at random within each village. This stratified design is approximately self-weighting within districts; village-level sampling weights from the survey's replication materials [14] are applied in design-weighted sensitivity analyses. The 45-minute questionnaire covered socioeconomic information, electricity access and satisfaction, cooking energy access and satisfaction, energy policy preferences, and willingness to pay; the household head (or another adult) was interviewed, and in Wave 2 the primary cook was asked to be present for the cooking-energy section [44]."),
];

// Section 2.2.4. Kept free of derived numbers on purpose: it is assembled
// before the NSSO outputs are read (SI Methods S8 carries the counts).
const nssoDataText = [
  p("The Multiple Indicator Survey (MIS) of the 78th round of the National Sample Survey [7] is an official household survey fielded by the National Statistical Office between January 2020 and August 2021, a window extended by the COVID-19 pandemic. Its design treats districts as strata, so it samples the whole country, and, like NFHS, it records a single primary source of energy for cooking, with LPG coded separately from piped natural gas. It has no dedicated cooking-energy module and collects none of the health outcomes analysed here, so it is used neither for calibration nor in the health analysis: it serves only as an independent external check on the NFHS-IRES comparison, processed with the same multilevel and design-weighted estimators as the other surveys (SI Methods S8)."),
];
const iresText = [
  p("The India Residential Energy Survey (IRES), conducted by the Council on Energy, Environment and Water (CEEW), is a large-scale, nationally representative household survey of energy access and consumption [15, 23]. Fieldwork was conducted between November 2019 and March 2020, with the majority of interviews administered in December 2019 through February 2020 [23]; interview dates in the analytic file confirm this window (" + iresWin("pct_before_march_2020", "98.0") + "% of interviews predate March 2020 and " + iresWin("pct_in_nov2019_mar2020", "99.98") + "% fall within November 2019-March 2020, with " + iresWin("n_out_of_window", "three") + " records carrying out-of-window dates). The entire IRES fieldwork therefore precedes the COVID-19 lockdown and the NFHS-5 fieldwork that resumed after it (November 2020-April 2021). The survey covered " + I_HH + " urban and rural households (" + I_RURAL + "% rural) spanning " + I_DIST + " districts (" + I_SU + " district sampling units) in " + I_STATES + " states, together comprising 97% of India's population. IRES employs a stratified multistage probability design [23]: each state was allocated a number of districts in proportion to its population (between 4 and 16, rounded up to an even number), its districts were ordered by population and grouped into strata of roughly equal population, and two districts were sampled per stratum with probability proportional to size without replacement; within each district, 12 villages/urban wards were sampled in proportion to the rural and urban population (at least two urban wards), and eight households were sampled per village/ward by random walk (96 households per district sampling unit). Interviews were conducted in person by trained enumerators using handheld tablets. The average non-response rate was 26% (34% urban, 21% rural) and was higher in districts with more wealthy households; CEEW's survey weights are design (base) weights reflecting selection probabilities, without non-response or post-stratification adjustment [23]. District-level design weights are applied in our design-weighted analyses."),
];

const measuresText = [
  p("Table S1 (Supplementary Materials) summarizes the measures used from each survey. The primary exposure indicator common to all surveys is use of LPG as the main cooking fuel. In NFHS this is derived from the household main-fuel item; in ACCESS from the primary cooking fuel module; and in IRES from the primary cooking fuel item, counting LPG only (piped natural gas has its own code in IRES and is not counted, matching ACCESS; the NFHS code groups LPG with natural gas, which reaches few rural households)."),
  p([
    r("From the energy surveys we construct three fuel-use outcomes unavailable in NFHS. "),
    r("Fuel stacking", { italics: true }),
    r(" is defined among households whose main fuel is LPG as continued use of any solid fuel (firewood, dung cake, or agricultural residue; IRES also records coal, but ACCESS does not, so coal is excluded in both). "),
    r("Four-category fuel use", { italics: true }),
    r(" classifies all households into four groups according to which fuels they name, not according to verified exclusivity: LPG with no solid fuel reported; LPG and solid fuel reported (stacking); solid fuel reported with no LPG; and a small residual group reporting neither LPG nor any solid fuel (e.g. electricity, piped natural gas, or kerosene as the primary fuel), so that this last group is not miscounted among solid-fuel burners. We deliberately avoid the term 'exclusive LPG' for the first group. The surveys establish only that LPG was named among the fuels the household uses and that no solid fuel was named; that is a fact about reporting, not evidence that no solid fuel is ever burned, and the category also admits households that additionally use kerosene or another non-solid fuel. The analysis code labels the four levels 'LPG, no solid fuel reported', 'LPG and solid fuel reported', 'Solid fuel reported, no LPG' and 'Neither LPG nor solid fuel reported' (variable use3cat, constructed identically in 02_prep_access.R and 03_prep_ires.R); the first of these is an abbreviation of 'LPG reported, no solid fuel reported' and carries exactly that meaning. "),
    r("Annual LPG consumption", { italics: true }),
    r(" (kg/year) is computed from self-reported cylinder use, valuing large cylinders at 14.2 kg: in ACCESS from annual purchases of large and small cylinders from distributors and the market, with small cylinders valued at 5 kg; in IRES from annual refills of large and small cylinders, with small cylinders valued at their reported size (5 kg where unrecorded) [8]. IRES additionally includes a direct self-report of whether LPG meets all cooking needs; because many households answering that LPG meets all their cooking needs also report burning a solid fuel, so the item over-identifies exclusive users, we do not use it to define fuel use and rely on the reported use of individual fuels, retaining it only as a cross-check."),
  ]),
  p("Covariates were harmonized across surveys to the set available in all three sources: caste category (Scheduled Caste, Scheduled Tribe, Other Backward Class, General), religion (Hindu, Muslim, Other), household size, below-poverty-line/Antyodaya ration card (whose question differs between the instruments; Section 3.1), household electricity, a within-state wealth quintile (constructed from the NFHS wealth-index factor score and from monthly household expenditure in ACCESS and IRES, and therefore interpretable as relative within-state economic rank rather than absolute wealth), urban/rural residence, and state. A complete variable inventory with source item codes for all three surveys is provided in the variable inventory section at the end of the Supplementary Materials."),
];

const statText = [
  p("Precision-weighted predicted probabilities of the exposures were derived at the district level from household-level data using four-level multilevel models. As the district frames of NFHS-4 and NFHS-5 differ, the prevalence of each outcome was estimated on the districts defined in the NFHS-4 survey (the same frame used by ACCESS); NFHS-5 clusters were mapped onto NFHS-4 districts with the cluster-to-district crosswalk described in Section 2.1. The four levels are households at level 1 (i), clusters (NFHS) or villages (ACCESS/IRES) at level 2 (j), districts at level 3 (k), and states at level 4 (l):"),
  p("logit(y_ijkl) = a0 + u_0jkl + v_0kl + f_0l          (Equation 1)", { par: { alignment: AlignmentType.CENTER }, run: { italics: true } }),
  p("where a0 is the constant, representing the median log-odds across the study area, and u_0jkl, v_0kl, and f_0l are cluster/village-, district-, and state-level residuals, assumed normally distributed with mean 0 and variances that capture within-district between-cluster variation, within-state between-district variation, and between-state variation, respectively. District-specific probabilities - the expected share of the district\u0027s households (or births) - are obtained by averaging expit(a0 + v_0kl + f_0l + u) over the cluster-level distribution u ~ N(0, sigma_u^2), by Gauss-Hermite quadrature. Evaluating expit at u = 0 instead would give the probability for a median cluster, which on the logit scale lies further from one half than the district average, by an amount that depends on the survey\u0027s own between-cluster variance; comparing two surveys at u = 0 would therefore mix a difference in prevalence with a difference in clustering. Models were fit with the lme4 package in R [18]. Sampling weights were not incorporated into the multilevel models: weighted estimation of mixed models requires pseudo-maximum-likelihood with scaled, level-specific weights [21, 22], and the NFHS releases a single household-level weight (hv005/10^6) with no separate cluster- or district-level components, so unweighted multilevel estimation - standard in DHS-based small-area applications, including precision-weighted mapping of PM2.5 exposure and its socioeconomic patterning across Indian districts [41] - was used, with design-based direct estimates (weighted proportions with Taylor-linearized standard errors, computed in the survey/srvyr packages [24, 25]) serving as a robustness check. The three surveys' sampling weights differ in construction and are used as their designers intended; their construction, the design-weighted direct-estimation specification for each survey, and the Taylor-linearization procedure used to obtain each district's design-based standard error (which the measurement-error model of Section 2.4.3 requires) are detailed in Supplementary Methods S1. The unweighted multilevel and design-weighted direct estimates agree closely within each survey (district correlation " + mlwtRange("pearson_r") + ", Lin's CCC " + mlwtRange("ccc") + "; SI Figure S19), so weighting choices do not drive the comparison results; the multilevel estimates are retained as the primary specification because partial pooling stabilizes district estimates based on small within-district samples. Regression calibration is likewise insensitive to which raw estimate it is fit on - calibrating the design-weighted rather than the multilevel NFHS estimate yields a near-identical corrected surface (district correlation " + emCor(2015, "rc_ml", "rc_wt") + " in 2015 and " + emCor(2019, "rc_ml", "rc_wt") + " in 2019; SI Figure S20 and Table S10) - and all corrections raise the post-PMUY district mean (each district weighted equally) to about " + rng2(BAY19, emx(2019, "Reg.-calibrated (design-wt input)"), 0) + "% (from a raw " + rng2(RAW19, emx(2019, "Raw design-weighted"), 0) + "%), agreeing on the level shift while the Bayesian surface redistributes somewhat more across districts than the calibrated ones (district correlations in SI Table S10)."),
];

// ---- Supplementary Methods S1: survey weights and design-based SEs (moved from 2.4.1) ----
const weightsSI = [
  p("Each survey's sampling weights differ in construction and are used as their designers intended. The NFHS household weight (hv005, released multiplied by 10^6) is the design weight - the inverse of the household's selection probability under the two-stage stratified design - adjusted for household non-response and normalized so that weights average one over the national sample (ICF 2012; IIPS 2021); it makes estimates representative at the national, state, and district level at which NFHS is designed to report. The ACCESS weights are village-level weights from the survey's replication materials (Pelz et al. 2021): they reflect each village's selection probability under the probability-proportional-to-size design (seven villages per size stratum per district), are shared by all households within a village (twelve households were sampled per village regardless of size), and carry no non-response or post-stratification adjustment; because districts were sampled one per administrative division and households equally within villages, the design is approximately self-weighting within districts and the weights chiefly correct across-village selection. The IRES weights are design (base) weights constructed by CEEW as the reciprocal of each household's selection probability through the full multistage design (districts sampled two per within-state stratum of roughly equal population, proportional to size [Section 2.2.3] x village/ward sampled proportional-to-size within district x household within village/ward), corrected for the ratio of planned to actually completed interviews in each village/ward, and provided at two levels of representativeness; we use the district-level weights (sw_dist) because our estimand is district prevalence. Beyond that village-level completion ratio, IRES weights, like ACCESS's, carry no non-response or post-stratification adjustment [23]."),
  p("Design-weighted direct district estimates (weighted proportions with Taylor-linearized standard errors) were computed using each survey's full sample design in the survey and srvyr packages for R (Lumley 2004): for NFHS, household weights (hv005/10^6) with clusters (hv021) as primary sampling units and sampling strata (hv022), with rural households analyzed as a subpopulation domain within the full design rather than by subsetting, and single-PSU strata handled by centering at the grand mean; for ACCESS, the village-level weights from the survey's replication materials, with the sampled villages as the primary sampling units; and for IRES, the CEEW-provided district analysis weights (sw_dist), with the sampled villages/wards as the primary sampling units. In both energy surveys the village (or ward) is the unit sampled below the district and households are enumerated within it, so the village is declared as the primary sampling unit - which captures the within-village clustering of fuel use - rather than mis-specifying households as the sampling unit and the village as a stratum, and no further within-village stratification is imposed."),
  p("The design-based standard error of each weighted district proportion is computed by Taylor-series linearization [25]: the weighted proportion is a ratio of weighted totals, which is linearized into a per-observation score contribution, and the variance is then estimated from the variability of the summed score contributions across primary sampling units within each sampling stratum, under the standard with-replacement approximation for the first stage. This variance estimator accounts simultaneously for the unequal weights, for the clustering of households within PSUs (households in the same cluster are more alike than a simple random sample would be, which inflates the variance relative to the naive p(1-p)/n formula), and, where present, for the stratification (variability is assessed within, not across, strata, which reduces it). For NFHS this includes the hv022 sampling strata, and strata contributing a single PSU to a domain, which cannot supply a within-stratum contrast, are handled by centering that PSU at the overall mean; the two energy surveys are treated as single-stratum village-PSU designs (ignoring, for ACCESS, the large/small village strata within districts, which if anything overstates the standard errors), so their district standard errors reflect village-level clustering alone. For the measurement-error model in Section 2.4.3, these standard errors are transported to the logit scale by the delta method (Equation S1), so that each district's known sampling variance enters the model on the scale on which the calibration is estimated."),
  p("Two further details of how these standard errors enter the measurement-error model of Section 2.4.3 are recorded here. First, the reference-side standard error is the reference survey's Taylor-linearized design standard error wherever one is available, and the code falls back, district by district, to the binomial form 1/sqrt(n p (1 - p)) only where it is not. In the NFHS-5/IRES calibration " + ME_B_DESIGN + " of the " + ME_B_N + " districts use the design standard error; the " + ME_B_FALLBACK + " exceptions are districts whose linearized standard error is exactly zero because their sampled rural villages show no between-village variation in the estimate - in one case because all " + ME_B_ALLYES_N + " sampled rural households reported LPG as the primary cooking fuel, in the other because the sampled villages' proportions are identical. The NFHS-4/ACCESS calibration uses the binomial form for all " + ME_A_N + " of its districts, because no design-based standard error was carried through for the multilevel ACCESS reference; Section 2.4.3 states what that costs."),
  p("Second, both logit standard errors are bounded before fitting: below by a positivity floor of " + nfmt(meQ("se_floor"), 4) + " (the se() term in brms requires strictly positive values) and above by " + meQ("se_cap") + " logits, and district proportions are bounded away from 0 and 1 by 0.001 before the logit transformation, so that a district whose estimated prevalence sits at the boundary cannot contribute an infinite measurement-error variance. " + seBoundTxt() + " Because a smaller standard error carries more weight in the likelihood, this bound increases rather than decreases that district's influence - the less conservative direction - though a logit standard error of " + meQ("se_cap") + " leaves the district weakly informative. No other bound binds anywhere in either panel: the remaining reference-side standard errors span " + ME_Y_LO + " to " + ME_Y_HI + " and the NFHS-side standard errors " + ME_X_LO + " to " + ME_X_HI + ", all far inside the limits."),
  p("Applying the fitted calibration to a district requires the district's true NFHS value, of which the NFHS estimate is a noisy measurement. The corrected surface is therefore computed from the posterior of that true value rather than from the raw estimate: on the logit scale each district's NFHS estimate x with standard error s is combined with the distribution of true district values in its state, estimated from all NFHS districts of the round (state mean m and between-district SD tau, a DerSimonian-Laird moment estimate pooled within states), giving a posterior mean m + lambda (x - m) and SD sqrt(lambda) s with lambda = tau^2 / (tau^2 + s^2). Each posterior draw of the corrected prevalence applies the calibration draw to a draw of the true value from that posterior. tau is " + shrinkQ("lpg_2015", "tau") + " for NFHS-4 and " + shrinkQ("lpg_2019", "tau") + " for NFHS-5, and the median shrinkage weight lambda is " + shrinkQ("lpg_2015", "lambda") + " and " + shrinkQ("lpg_2019", "lambda") + " respectively, so precisely estimated districts move little. Because the health analysis uses the change between rounds, the two rounds are shrunk jointly rather than one at a time. A district's true values in the two rounds are correlated (an estimated correlation of the latent values of " + jQ("between_round_correlation_true_values", 2) + " within states, " + jRhoCI() + ", estimated from the cross-products of within-state residuals, to which the two surveys' independent sampling errors do not contribute), so under this model the change is less reliably estimated than either level and its posterior is pulled further toward the state-mean change. Shrinking each round on its own can leave the change under-shrunk; whether that matters for the health association depends on the data-generating model, so both versions are reported (SI Methods S6). The joint posterior is bivariate normal, with the between-district covariance estimated from the within-state residuals of the " + jQ("n_districts_both_rounds", 0) + " districts observed in both rounds; the median posterior correlation of a district's two true values is " + jQ("median_posterior_correlation", 2) + ", and the posterior draws of the two rounds carry that correlation. The shrinkage moments are released with the replication materials (me_shrinkage.csv, me_shrinkage_joint.csv)."),
  p("The regression calibration applied nationally pools all calibration districts without state effects, because a state fixed effect cannot be evaluated in states the reference survey did not sample. As a within-sample check we refit it with state fixed effects. The slope is " + stSlope(0) + " with state effects against " + nfmt(stApplied[0], 2) + " without on the NFHS-4/ACCESS leg, and " + stSlope(1) + " against " + nfmt(stApplied[1], 2) + " on the NFHS-5/IRES leg. Within states, therefore, the NFHS-5 and IRES district estimates are related more weakly than across the pooled sample, and part of the pooled 2019 relation reflects differences between states. The Bayesian model represents these through its state random effects (Table S3), which are estimated within the calibration states and drawn from their fitted distribution elsewhere."),
  p("Because the binomial reference-side standard error described above applies to the whole NFHS-4 leg, and because the era-matched corrected change takes its 2015 endpoint from that leg, we refit the NFHS-4 calibration a second time under exactly the treatment the NFHS-5 leg receives: the ACCESS district value is the design-weighted direct estimate rather than the multilevel one, and its Taylor-linearized standard error is passed to the model as the reference-side measurement error, so neither leg relies on the binomial approximation. The calibration slope is " + calWtE("slope") + " " + calWtCI("slope") + " under the symmetric fit against " + calE(CAL4, "slope") + " " + calCI(CAL4, "slope") + " in the primary specification, and the residual standard deviation is " + calWtE("sd_residual") + " against " + calE(CAL4, "sd_residual") + "; because the response also changes, from the multilevel to the direct ACCESS estimate, the two fits differ in more than the standard error passed to the model. The corrected 2015 surface keeps its spatial pattern but shifts upward: across " + legAQ("n_districts") + " districts the two surfaces correlate at " + legAQ("corr_primary_sens", 3) + ", the mean corrected prevalence is " + legAQ("mean_sens_pc", 1) + "% against " + legAQ("mean_primary_pc", 1) + "% in the primary specification, and the surfaces differ by " + legAQ("mean_abs_diff_pp", 2) + " percentage points on average, with a largest single-district difference of " + legAQ("max_abs_diff_pp", 2) + " percentage points. Carried through the child-mortality models of SI Methods S6, the symmetric fit " + legAVerDICT + " (SI Table S6 reports the primary specification, and Table S6a and SI Figure S21 the design-weighted ACCESS-leg rows; the movement of each coefficient - fixed surface and exposure-only imputation - in units of its own standard error is given in legA_symmetry_health_movement.csv in the replication archive). We retain the multilevel reference as primary because it is the more precise district estimate, and report the symmetric refit so that the effect of the standard-error asymmetry can be judged directly."),
  p("A worked example makes the arithmetic concrete. Consider a hypothetical district in which four rural households are sampled from two villages, and suppose their released household weights are 1, 1, 3 and 3 - that is, the first two households each stand for one household in the district population and the last two each stand for three, because they were drawn from a stratum that the design deliberately under-sampled relative to its size. Suppose one of the two households carrying weight 1 reports LPG as its primary cooking fuel and the other three do not. The unweighted proportion is 1 of 4, or 25.0 percent. The design-weighted proportion is the ratio of the weighted total of LPG households to the weighted total of all households, 1 divided by 8, or 12.5 percent. Weighting has halved the estimate, and for the right reason: the single LPG household stands for fewer real households than its raw presence in the sample suggests. Every design-weighted district estimate in this paper is this calculation, carried out over the households of one district."),
  p("The standard error attached to that 12.5 percent has to reflect the same design, and two features of the design reduce the information those four households carry. Unequal weights reduce it directly: the Kish approximation to the effective sample size, the square of the summed weights divided by the sum of the squared weights, gives 8 squared over 1 + 1 + 9 + 9, that is 64 over 20, or 3.2 households of effective information rather than 4 - an unequal-weighting design effect of 1.25. Clustering reduces it further, because the four households come from only two villages and households in the same village resemble one another in fuel use. A binomial standard error that ignores both, the square root of p (1 - p) / n, would be 0.165. Substituting the effective sample size for n gives about 0.185, and the Taylor-linearized estimator actually used here adds the clustering component on top of that by accumulating score contributions within primary sampling units rather than within households, so the full design-based standard error would be larger still. For simplicity the example carries 0.185 forward."),
  p("Finally the estimate and its standard error are moved to the logit scale, on which the measurement-error model of Section 2.4.3 is fitted. The point estimate becomes the log odds of 0.125, that is the natural logarithm of 0.125 divided by 0.875, or -1.946. The standard error is transformed by the delta method given as Equation S1 below, which divides by p (1 - p): 0.185 divided by the product of 0.125 and 0.875 is 0.185 divided by 0.1094, or 1.69. That logit standard error is large, but only because the example has four households; it still falls inside the bounds described above, and real districts contribute hundreds of households and correspondingly tighter standard errors. If the reference survey placed the same district at 20.0 percent, its logit value would be the natural logarithm of 0.2 divided by 0.8, or -1.386, and the model would compare -1.946 with -1.386 while treating both survey standard errors as known measurement error and letting the residual standard deviation absorb what is left."),
];

const compareText = [
  p("We compare district-level primary-LPG prevalence for the two temporally overlapping survey pairs: NFHS-4 (rural clusters) versus ACCESS Wave 1, and NFHS-5 versus IRES (all households and rural-only). Agreement is summarized by Pearson and Spearman correlations, correlations on the logit scale, Lin's concordance correlation coefficient, and mean differences; sample-size-weighted correlations (weighting districts by surveyed reference-survey household counts, not by district population) address the sensitivity of unweighted district-level correlations to small, sparsely sampled districts. Scatterplots against the identity line and Bland-Altman plots are used to assess whether agreement diverges at higher or lower prevalence levels and by state. Because the reference surveys estimate district prevalence from about 168 households per district in ACCESS and about 96 in IRES - the design targets, with realized counts of 167-168 per ACCESS district and 94-110 per IRES district sampling unit - a substantial share of observed disagreement is expected from sampling variability alone. The rural subsample that the primary NFHS-5 calibration actually uses is smaller still, a median of " + ME_IRES_RURAL_MED + " rural households per 2011 district (range " + ME_IRES_RURAL_MIN + "-" + ME_IRES_RURAL_MAX + "; a 2011 district assembled from two IRES sampling units carries roughly twice the sample), so district-level noise on the IRES side is correspondingly larger than the full-sample figure implies; the measurement-error model of Section 2.4.3 propagates it rather than ignoring it. We also note fieldwork-timing differences: NFHS-5 interviews straddle the IRES fieldwork, with a substantial share conducted before it, after it, and during it, so fieldwork timing could in principle contribute to the gap in either direction and is tested directly below; PMUY's 2016 launch falls between ACCESS Wave 1 and the end of NFHS-4 fieldwork; and part of NFHS-5 fieldwork occurred during the COVID-19 period. Two sensitivity analyses address timing: restricting NFHS-5 to pre-COVID interviews (2019 through February 2020), and restricting NFHS-5 to winter interviews (December-February), the season in which IRES was fielded and in which stacking households are most likely to report a solid fuel as primary [32, 34]."),
  p("Finally, we conduct a falsification (benchmark) analysis: if NFHS and the reference surveys disagree on LPG because of fuel measurement rather than sample composition, they should still agree on benchmark characteristics asked in both. We therefore estimate district prevalences of Scheduled Caste, Scheduled Tribe, Hindu, Muslim, household electricity, BPL/Antyodaya ration card, and the combined Scheduled Caste/Scheduled Tribe share with the same multilevel specification in each member of each survey pair, and compare their agreement (correlations and mean differences) with that of primary LPG. Caste and religion serve as demographic benchmarks; electricity and the ration card are programme-linked items whose questions may differ between instruments, and are reported but not treated as clean benchmarks."),
];

// ============ VARIANCE DECOMPOSITION (22_design_analysis.R) ===================
// Section 3.4, SI Section S4 and Table S17 are built from the descriptive
// variance decomposition 22_design_analysis.R writes. The precision-frontier and
// validation-design parts of that script are disabled (DESIGN_FRONTIER = 0):
// they rested on the abandoned outcome-conditioned imputation, and their
// outputs are archived as failed convergence. Nothing here reads them.
const dSum    = requireCsv("design_summary.csv", "the NFHS-side variance shares");
const dVdec   = requireCsv("design_variance_decomposition.csv", "the variance decomposition (Table S17)");
const dsRaw = (k) => { const x = dSum.find((z) => z.quantity === k); return x ? x.value : undefined; };
const DS    = (k, d) => nfmt(dsRaw(k), d === undefined ? 2 : d);
const DSn   = (k) => { const v = Number(dsRaw(k)); return isFinite(v) ? v : NaN; };
if (dSum.some((z) => /kappa|frontier|floor|direct|nfhs_multiplier/.test(z.quantity)))
  throw new Error("[build] design_summary.csv carries precision-frontier quantities; it comes from a "
    + "frontier run (DESIGN_FRONTIER=1), whose results are not for inference. Re-run 22_design_analysis.R.");
const DECOMP_VALID = DSn("decomposition_valid") !== 0;

// ---- Outcome-side reliability, for the Discussion --------------------------
// The exposure side of the regression is corrected and its uncertainty is
// propagated; the outcome side is not. These helpers quantify how much of the
// across-district variance in the CHANGE in district mortality is genuine
// between-district variation rather than sampling error, using the same
// decomposition applied to the exposure. Everything comes from
// health_district_wide.csv, which the health pipeline already writes, so no
// re-run is required and nothing here is typed by hand.
//
// SCALE: the change_* columns of that file are in deaths per 100 births (they
// are 100 x the difference of the two design-weighted probabilities), whereas
// the *_se_* columns are on the probability scale. The standard errors are
// therefore rescaled by OUT_SCALE before they are compared with the variance of
// the change. Getting this wrong inflates the signal share to ~1.00, which is
// the failure mode the assertion at the end of this block guards against.
const OUT_SCALE = 100;
const hdwNum = (r, k) => { const v = Number(r[k]); return isFinite(v) ? v : null; };
const hdwVec = (col) => hdwRural.map((r) => hdwNum(r, col))
                                .filter((v) => v !== null);
const medOf = (v) => {
  if (!v.length) return NaN;
  const s = v.slice().sort((a, b) => a - b), m = s.length >> 1;
  return (s.length % 2) ? s[m] : (s[m - 1] + s[m]) / 2;
};
const varOf = (v) => {
  if (v.length < 2) return NaN;
  const mu = v.reduce((a, b) => a + b, 0) / v.length;
  return v.reduce((a, b) => a + (b - mu) * (b - mu), 0) / (v.length - 1);
};
// share = (var(change) - mean(se_2015^2 + se_2019^2)) / var(change)
const outcomeSignalShare = (outcome) => {
  const ch = [], nz = [];
  hdwRural.forEach((r) => {
    const c   = hdwNum(r, "change_" + outcome + "_death_dw");
    const s15 = hdwNum(r, outcome + "_death_dw_se_2015");
    const s19 = hdwNum(r, outcome + "_death_dw_se_2019");
    if (c === null || s15 === null || s19 === null) return;
    ch.push(c);
    nz.push(OUT_SCALE * OUT_SCALE * (s15 * s15 + s19 * s19));
  });
  if (ch.length < 2) return NaN;
  const v = varOf(ch);
  const n = nz.reduce((a, b) => a + b, 0) / nz.length;
  return (isFinite(v) && v > 0) ? (v - n) / v : NaN;
};
const OSS_INF = outcomeSignalShare("infant");
const OSS_NEO = outcomeSignalShare("neonatal");
const OSS_N   = hdwRural.filter((r) =>
  hdwNum(r, "change_infant_death_dw") !== null &&
  hdwNum(r, "infant_death_dw_se_2015") !== null &&
  hdwNum(r, "infant_death_dw_se_2019") !== null).length;
const medBirths = (yr) => nfmt(medOf(hdwVec("n_births_" + yr)), 0);
const medSE100  = (outcome, yr) =>
  nfmt(OUT_SCALE * medOf(hdwVec(outcome + "_death_dw_se_" + yr)), 1);

// Diagnostics. A silent NA here would put "NA%" into the Discussion; a share
// pinned at ~1.00 would mean the standard errors are being compared on the
// wrong scale. Both must shout rather than render.
console.log("[build] outcome reliability: n = " + OSS_N + " districts, "
  + "infant signal share = " + nfmt(100 * OSS_INF, 1) + "%, "
  + "neonatal = " + nfmt(100 * OSS_NEO, 1) + "%; median rural births "
  + medBirths("2015") + " (2015) and " + medBirths("2019") + " (2019); "
  + "median design-based infant SE " + medSE100("infant", "2015") + " and "
  + medSE100("infant", "2019") + " per 100 births.");
if (!isFinite(OSS_INF) || !isFinite(OSS_NEO) || OSS_N < 100) {
  console.warn("[build] OUTCOME RELIABILITY NOT COMPUTED: "
    + "health_district_wide.csv is missing the design-weighted district "
    + "mortality standard errors or the change columns; the Discussion "
    + "paragraph on outcome measurement error will read NA.");
}
if (OSS_INF > 0.95 || OSS_NEO > 0.95) {
  console.warn("[build] OUTCOME RELIABILITY IMPLAUSIBLE (> 0.95): the design-"
    + "based standard errors and the change columns are probably no longer on "
    + "the scales OUT_SCALE assumes. Check H1_prep_mortality.R before trusting "
    + "the Discussion paragraph on outcome measurement error.");
}

// ---- SI Figure S11 correlations, read from H6's output rather than typed -----
// H6_si_health_energy_scatter.R writes one row per (pair, energy metric,
// outcome). A metric that is constant across districts yields an NA correlation
// -- which must appear in the caption as NA, not be quietly omitted, because a
// caption that still quotes a number the figure no longer shows is worse than
// one that admits the value is unavailable.
//
// Defined here, well above the SI assembly that first used them, because the
// main-text Discussion now quotes the same correlations and const declarations
// are not hoisted.
const heCorr = readCsv("si_health_energy_corr.csv");
const heR = (pair, energy, outcome) => {
  const x = heCorr.find((z) => z.pair === pair && z.energy === energy
                               && z.outcome === outcome);
  return x ? nfmt(x.pearson, 2) : "NA";
};
const hePair = (energy) =>
  "neonatal " + heR("NFHS-4 vs ACCESS (rural)", energy, "Neonatal") + " and infant " +
  heR("NFHS-4 vs ACCESS (rural)", energy, "Infant") + " for ACCESS/NFHS-4; neonatal " +
  heR("NFHS-5 vs IRES (rural)", energy, "Neonatal") + " and infant " +
  heR("NFHS-5 vs IRES (rural)", energy, "Infant") + " for IRES/NFHS-5";

// The SIGN of these correlations is described in the main-text Discussion, in
// the SI Figure S11 caption and in the calibration-support paragraph. A typed
// "positively associated" would be a literal claim about data that a re-run can
// move -- exactly the failure mode this builder exists to prevent -- so the
// adjective is derived from the CSV rather than written into the sentence.
// heDir() returns an adjective that reads correctly in "the association is ___".
const heRaw = (pair, energy, outcome) => {
  const x = heCorr.find((z) => z.pair === pair && z.energy === energy
                               && z.outcome === outcome);
  const v = x ? Number(x.pearson) : NaN;
  return isFinite(v) ? v : NaN;
};
const HE_NEAR_ZERO = 0.10;   // |r| below this is not called a direction
const heDir = (pair, energy) => {
  const a = heRaw(pair, energy, "Neonatal");
  const b = heRaw(pair, energy, "Infant");
  if (!isFinite(a) || !isFinite(b)) return "unavailable";
  const lo = Math.min(a, b), hi = Math.max(a, b);
  if (lo >= HE_NEAR_ZERO) return "positive";
  if (hi <= -HE_NEAR_ZERO) return "negative";
  if (lo > -HE_NEAR_ZERO && hi < HE_NEAR_ZERO) return "close to zero";
  return "mixed in sign across the two mortality outcomes";
};
const HE_A = "NFHS-4 vs ACCESS (rural)";
const HE_I = "NFHS-5 vs IRES (rural)";
console.log("[build] SI Figure S11 directions (CSV-derived, not typed): "
  + "solid fuel " + heDir(HE_A, "Any solid-fuel burning") + " (ACCESS/NFHS-4), "
  + heDir(HE_I, "Any solid-fuel burning") + " (IRES/NFHS-5); primary LPG "
  + heDir(HE_A, "Primary LPG") + " (ACCESS/NFHS-4), "
  + heDir(HE_I, "Primary LPG") + " (IRES/NFHS-5).");
if ([heDir(HE_A, "Any solid-fuel burning"), heDir(HE_I, "Any solid-fuel burning"),
     heDir(HE_A, "Primary LPG"), heDir(HE_I, "Primary LPG")]
      .some((d) => d === "unavailable")) {
  console.warn("[build] SI Figure S11 DIRECTIONS UNAVAILABLE: "
    + "si_health_energy_corr.csv is missing rows. Re-run "
    + "H6_si_health_energy_scatter.R; the Discussion, the Figure S11 caption "
    + "and the calibration-support paragraph will read 'unavailable'.");
}
const vdRow = (yr) => dVdec.find((x) => String(x.leg).indexOf(yr) === 0) || {};
// Variance-decomposition accessors. Defined here rather than beside Table S17
// because Section 3.3 of the main text now quotes the fitted residual SDs, and
// these const initializers are evaluated in source order.
const vd = (yr, col, d) => nfmt(vdRow(yr)[col], d === undefined ? 3 : d);
const sigRatio = (d) => {
  const a = Number(vdRow("2019").sigma), b = Number(vdRow("2015").sigma);
  return (isFinite(a) && isFinite(b) && b !== 0) ? nfmt(a / b, d === undefined ? 1 : d) : "NA";
};

// Comparative read off the ratio, not typed.
const sigNoisierTxt = () => {
  const a = Number(vdRow("2019").sigma), b = Number(vdRow("2015").sigma);
  if (!(isFinite(a) && isFinite(b) && b > 0)) return "noisier";
  const r = a / b;
  if (r <= 1) { console.warn("[build] 2019 residual SD is not larger than 2015; review the Section 4 variance-split sentence."); return "no less noisy"; }
  return r >= 2 ? "far noisier" : r >= 1.25 ? "noisier" : "slightly noisier";
};
// What does the closed form lose by conditioning on the posterior MEDIAN of the
// variance components instead of averaging over their draws? This used to be a
// remembered constant in the S4 prose. It is not a constant: it is near zero at
// the calibration districts and shows up mainly at target districts in states
// the calibration never visited. 22_design_analysis.R now computes it per leg
// and exports it; if that file is absent the clause DISAPPEARS rather than
// printing a figure no output on disk supports.
const dVcPen = readCsv("design_vc_plugin_penalty.csv", { quiet: true });
const vcPenTxt = () => {
  const row  = (yr) => dVcPen.find((x) => String(x.leg).indexOf(yr) === 0);
  const a = row("2015"), i = row("2019");
  const va = a ? Number(a.pct_under_sd) : NaN;
  const vi = i ? Number(i.pct_under_sd) : NaN;
  if (!isFinite(va) || !isFinite(vi)) {
    console.warn("[build] design_vc_plugin_penalty.csv absent or unreadable -- "
      + "omitting the plug-in-versus-averaged magnitude from SI Section S4. "
      + "Re-run 22_design_analysis.R to restore it.");
    return "";
  }
  // Sign-aware and grammatical either way: the plug-in shortcut is not
  // guaranteed to err downward, so the verb is read off the exported number.
  const mag  = (v) => (Math.abs(v) < 0.5 ? "by under 1%"
                                         : "by " + nfmt(Math.abs(v), 0) + "%");
  const full = (v, yr, first) => (v < 0 ? "overstates" : "understates")
             + (first ? " the predicted SD " : " it ") + mag(v)
             + " on the " + yr + " leg";
  if (Math.abs(va) < 0.5 && Math.abs(vi) < 0.5)
    return " (a choice that moves the predicted SD by under 1% on either leg "
         + "here, though it is not guaranteed to be so small)";
  // The mechanism clause is asserted only when the exported covered/uncovered
  // split actually supports it on both legs.
  const conf = (r) => {
    const c = Number(r.pct_under_sd_covered_states);
    const u = Number(r.pct_under_sd_uncovered_states);
    return isFinite(c) && isFinite(u) && u > c && Math.abs(c) < 3;
  };
  const why = (conf(a) && conf(i))
    ? "; the gap sits almost entirely in target districts in states the "
      + "calibration survey never visited, where a fresh state effect enters "
      + "the variance as psi^2 and the right skew of psi's posterior makes its "
      + "median a poor summary"
    : "";
  const second = ((va < 0) === (vi < 0)) ? mag(vi) + " on the 2019 leg"
                                        : full(vi, "2019", false);
  return " (which " + full(va, "2015", true) + " and " + second + why + ")";
};

// ---- Section 2.4.6: the four agreement checks on the analytic calibration
// variance, read from the pipeline's own check log ----
// 22_design_analysis.R compares its analytic reconstruction of the
// calibration-side variance with each fitted measurement-error model on four
// criteria, and records the realized value of each in
// diagnostics/pipeline_checks.csv. The main text now names those criteria and
// quotes those values, so they are parsed from the log for exactly the reason
// the ACCESS consumption figures above are: a re-run that moves a number must
// move the sentence with it. Same prev_ fallback, same loud warning, and a
// wording fallback that drops the numbers rather than printing NA.
let _prevChecks = null;
// LAST match, not first. checks.R appends, and a script that emits the same
// check label more than once in a run (H2 does, once per exposure family) or a
// tree whose stale rows were not cleared both leave several rows under one
// label. 20_pipeline_checks.R consolidates at the LATEST row for exactly this
// reason; taking .find() here read the earliest instead, and the 2026-09-15
// build quoted a superseded run's agreement figures in Section 2.4.6 while
// every CSV-derived number beside them was current.
const findLast = (arr, pred) => { for (let i = arr.length - 1; i >= 0; i--) if (pred(arr[i])) return arr[i]; return undefined; };
const chkDetail = (re) => {
  let r = findLast(pipeChecks, (x) => re.test(x.check || ""));
  if (!r) {
    if (_prevChecks === null)
      _prevChecks = readCsv("diagnostics/prev_pipeline_checks.csv", { quiet: true });
    r = findLast(_prevChecks, (x) => re.test(x.check || ""));
    if (r) console.warn("[build] FALLBACK: a 22_design_analysis.R check figure "
      + "comes from diagnostics/prev_pipeline_checks.csv, not the current log ("
      + re + ").");
  }
  return r ? (r.detail || "") : "";
};
const chkNum = (re, pat) => {
  const m = pat.exec(chkDetail(re));
  return m ? Number(m[1]) : NaN;
};
const DCK = {
  sign15: chkNum(/closed-form calibration variance <= observed, 2015/, /([0-9.]+)% of target/),
  sign19: chkNum(/closed-form calibration variance <= observed, 2019/, /([0-9.]+)% of target/),
  nfall15: chkNum(/NFHS-side residual rises as district n falls, 2015/, /=\s*\+?(-?[0-9.]+)/),
  nfall19: chkNum(/NFHS-side residual rises as district n falls, 2019/, /=\s*\+?(-?[0-9.]+)/),
  trk15: chkNum(/closed-form tracks the real posterior, 2015/, /=\s*\+?(-?[0-9.]+)/),
  trk19: chkNum(/closed-form tracks the real posterior, 2019/, /=\s*\+?(-?[0-9.]+)/),
  rat15: chkNum(/NFHS-side floor matches .* (from NFHS SEs|of the NFHS input), 2015/, /ratio ([0-9.]+)/),
  rat19: chkNum(/NFHS-side floor matches .* (from NFHS SEs|of the NFHS input), 2019/, /ratio ([0-9.]+)/)
};

// What conditioning on the posterior MEDIAN of the variance components would
// have cost, in main-text form. Sign-aware in both directions: the plug-in
// shortcut is not guaranteed to err downward, and the mechanism clause is
// asserted only when the exported covered/uncovered split supports it.
const vcPenMain = () => {
  const row = (yr) => dVcPen.find((x) => String(x.leg).indexOf(yr) === 0);
  const a = row("2015"), i = row("2019");
  const va = a ? Number(a.pct_under_sd) : NaN;
  const vi = i ? Number(i.pct_under_sd) : NaN;
  if (!isFinite(va) || !isFinite(vi)) {
    console.warn("[build] design_vc_plugin_penalty.csv absent or unreadable -- "
      + "omitting the plug-in-versus-averaged magnitude from Section 2.4.6.");
    return "";
  }
  const verb = (v) => (v > 0 ? "understate" : "overstate");
  const first = Math.abs(va) < 0.5
    ? "leave the calibration-side standard deviation essentially unchanged on the 2015 leg"
    : verb(va) + " the calibration-side standard deviation by " + nfmt(Math.abs(va), 0)
      + " per cent on the 2015 leg";
  const second = Math.abs(vi) < 0.5
    ? "leave it essentially unchanged on the 2019 leg"
    : verb(vi) + " it by " + nfmt(Math.abs(vi), 0) + " per cent on the 2019 leg";
  const cov = Number(a.pct_under_sd_covered_states);
  const unc = Number(a.pct_under_sd_uncovered_states);
  const why = (isFinite(cov) && isFinite(unc) && unc > cov && Math.abs(cov) < 3)
    ? ", and what difference there is sits almost entirely in target districts in "
      + "states the reference survey never entered, where a fresh state effect "
      + "enters the variance as psi squared and the right skew of psi's posterior "
      + "makes its median a poor summary"
    : "";
  return "Conditioning on the medians instead would " + first + " and " + second + why + ".";
};

// The four checks, named, with their realized values.
const fourChecksTxt = () => {
  const v = DCK;
  const need = [v.sign15, v.sign19, v.trk15, v.trk19, v.nfall15, v.nfall19,
                v.rat15, v.rat19];
  if (!need.every((z) => isFinite(z))) {
    console.warn("[build] one or more of 22_design_analysis.R's four agreement "
      + "checks could not be read from the check log; Section 2.4.6 falls back to "
      + "the unquantified wording. Re-run 22_design_analysis.R to restore the "
      + "realized values.");
    return "Rather than assume that reconstruction reproduces the fitted models, we "
      + "compared it with them on four criteria, reported with their realized values "
      + "in SI Methods S4. We treat the reconstruction as an approximate apportionment "
      + "of the fitted posterior variance rather than as an exact reproduction of it.";
  }
  const pct = (r) => nfmt(Math.abs(r - 1) * 100, 0) + " per cent";
  return "Rather than assume that reconstruction reproduces the fitted models, we "
    + "compared it with them on four criteria. Three are internal to that comparison: "
    + "the reconstructed calibration variance lies below the fitted posterior variance "
    + (Number(v.sign15) === 100 && Number(v.sign19) === 100
        ? "in every target district on both legs"
        : "in " + nfmt(v.sign15, 0) + " per cent and " + nfmt(v.sign19, 0) + " per cent of target districts on the two legs")
    + ", as it must if the remainder is to be a genuine "
    + "NFHS-side term; it tracks the fitted posterior across districts, with "
    + "correlations of " + nfmt(v.trk15, 2) + " and " + nfmt(v.trk19, 2) + "; and the "
    + "remainder's correlation with the inverse of the district's NFHS household count (all households, "
    + "including on the 2019 leg, whose remainder concerns the rural estimate), which should be "
    + "positive, is " + nfmt(v.nfall15, 2) + " and " + nfmt(v.nfall19, 2)
    + (Math.min(v.nfall15, v.nfall19) < 0
        ? " (the wrong sign on the " + (v.nfall19 < v.nfall15 ? "2019" : "2015") + " leg, so that check fails there)"
        : Math.min(v.nfall15, v.nfall19) < 0.2 ? " (close to zero on the " + (v.nfall19 < v.nfall15 ? "2019" : "2015") + " leg, where the calibration-side part dominates the variance, so that check is weak there)" : "")
    + ". The fourth compares the remainder with b squared times the posterior variance of "
    + "the district's true NFHS value from the shrinkage step, which is how the NFHS-side "
    + "term enters the posterior draws; they agree to within " + pct(v.rat15) + " and "
    + pct(v.rat19) + ". Because that term is built into the draws, the fourth criterion "
    + "checks the reconstruction's internal consistency rather than validating it "
    + "independently. We therefore treat the reconstruction "
    + "as an approximate apportionment of the fitted posterior variance rather than as "
    + "an exact reproduction of it.";
};

// Assembled middle of the variance-split paragraph. Empty components drop out
// rather than leaving a doubled space.
const p246Checks = () => [vcPenMain(), fourChecksTxt()]
  .filter((s) => s && s.length).map((s) => " " + s).join("");

// ================= ESTIMAND SENSITIVITY (23_ppd_sensitivity.R) ===============
// SI Section S5 and Table S18. Reported quietly: if 23 has not been run yet the
// section is omitted rather than rendered as a wall of NAs, and the omission is
// announced on the build console so it cannot pass unnoticed.
const ppdSens = readCsv("ppd_sensitivity.csv", { quiet: true });
const ppdSurf = readCsv("ppd_surface_comparison.csv", { quiet: true });
const ppdSum  = readCsv("ppd_summary.csv", { quiet: true });
const HAVE_PPD = ppdSens.length > 0 && ppdSurf.length > 0 && ppdSum.length > 0;
if (!HAVE_PPD)
  console.warn("[build] 23_ppd_sensitivity.R outputs not found -- SI Section S5 "
    + "and Table S18 are OMITTED and the Methods cross-reference degrades. "
    + "Run 23_ppd_sensitivity.R to restore them.");
const pqRaw = (k) => { const x = ppdSum.find((z) => z.quantity === k); return x ? x.value : undefined; };
const PQ    = (k, d) => nfmt(pqRaw(k), d === undefined ? 3 : d);
const psRow = (o, v) => ppdSens.find((x) => x.outcome === o && x.variant === v) || {};
const psE   = (o, v, d) => nfmt(psRow(o, v).estimate, d === undefined ? 3 : d);
const psSE  = (o, v, d) => nfmt(psRow(o, v).se, d === undefined ? 3 : d);
// Primary inference rule (CR2 + Satterthwaite, Barnard-Rubin across
// imputations), as in Table S6; conventional values only if CR2 is absent.
const psHasCR2 = (o, v) => isFinite(Number(psRow(o, v).p_cr2)) && isFinite(Number(psRow(o, v).se_cr2));
const psPn  = (o, v) => psHasCR2(o, v) ? Number(psRow(o, v).p_cr2) : Number(psRow(o, v).p);
const psP   = (o, v) => pfmt(psPn(o, v));
const psSEn = (o, v) => psHasCR2(o, v) ? Number(psRow(o, v).se_cr2) : Number(psRow(o, v).se);
const psCI  = (o, v, d) => {
  const r = psRow(o, v), e = Number(r.estimate);
  const cr2 = psHasCR2(o, v);
  const se = cr2 ? Number(r.se_cr2) : Number(r.se), crit = cr2 ? Number(r.crit_cr2) : 1.96;
  if (!isFinite(e) || !isFinite(se) || !isFinite(crit)) return "NA";
  const k = d === undefined ? 3 : d;
  return nfmt(e - crit * se, k) + ", " + nfmt(e + crit * se, k);
};
const pvRow = (yr) => ppdSurf.find((x) => String(x.leg).indexOf(yr) === 0) || {};
const ppdVerdict = (o) => {
  const a = psPn(o, "epred_MI"), b = psPn(o, "ppd_MI");
  if (!isFinite(a) || !isFinite(b)) return "the comparison could not be made";
  if (a < 0.05 && b < 0.05) return "both versions are conventionally significant";
  if (a >= 0.05 && b >= 0.05) return "neither version is conventionally significant";
  return "the two versions fall on opposite sides of the conventional 0.05 threshold";
};
const ppdDirTxt = (o) => {
  const a = Number(psRow(o, "epred_MI").estimate), b = Number(psRow(o, "ppd_MI").estimate);
  if (!isFinite(a) || !isFinite(b) || a === 0) return "moves by an amount that could not be computed";
  const f = b / a;
  return f < 1 ? "shrinks toward the null by a factor of " + nfmt(f, 2) + ", as classical measurement error predicts"
               : "moves away from the null by a factor of " + nfmt(f, 2) + ", which classical measurement error does not predict and which we flag rather than interpret";
};
// The standard error under the posterior-predictive estimand is NOT expected to
// stay put. Adding the residual back inflates the variance of the regressor
// itself, so the coefficient attenuates roughly like Var(x)/Var(x*) while its
// standard error falls roughly like 1/sd(x*). What matters is whether the two
// move together, because if they do the test statistic - and the conclusion -
// is left largely unchanged. That is computed here, not asserted.
const ppdSeTxt = (o) => {
  const a = psSEn(o, "epred_MI"), b = psSEn(o, "ppd_MI");
  if (!isFinite(a) || !isFinite(b) || a === 0) return "the standard error could not be compared";
  const f = b / a;
  // The inflation factor itself is quoted in the sentence that introduces this
  // one, so it is explained here but not repeated.
  const inf = Number(pqRaw("sd_inflation_2019"));
  const why = isFinite(inf) && inf > 1.05
    ? ", which is what inflating the exposure's own variance does: the regressor is measured on a wider scale, so the coefficient attached to it is both smaller and more tightly pinned"
    : "";
  return f < 1
    ? "its pooled standard error falls with it, by a factor of " + nfmt(f, 2) + why
    : "its pooled standard error rises, by a factor of " + nfmt(f, 2) + ", so the two do not move together and we report the comparison rather than interpret it";
};
// Whether the attenuation of the coefficient and the fall in its standard error
// roughly cancel in the test statistic.
const ppdZTxt = (o) => {
  const e1 = Number(psRow(o, "epred_MI").estimate), s1 = psSEn(o, "epred_MI");
  const e2 = Number(psRow(o, "ppd_MI").estimate),   s2 = psSEn(o, "ppd_MI");
  if (!isFinite(e1) || !isFinite(e2) || !isFinite(s1) || !isFinite(s2) || s1 === 0 || s2 === 0)
    return "the test statistic could not be compared";
  const z1 = Math.abs(e1 / s1), z2 = Math.abs(e2 / s2);
  return "the two changes largely cancel in the test statistic, which moves from |t| = "
    + nfmt(z1, 2) + " to " + nfmt(z2, 2);
};
// Was the alternative estimand actually conservative here - smaller in
// magnitude AND less significant - for both outcomes? Stated as a finding, not
// as an expectation, because a future run could reverse it.
const ppdConsTxt = () => {
  const os = ["infant", "neonatal"];
  const ok = os.every((o) => {
    const e1 = Math.abs(Number(psRow(o, "epred_MI").estimate)),
          e2 = Math.abs(Number(psRow(o, "ppd_MI").estimate)),
          p1 = psPn(o, "epred_MI"), p2 = psPn(o, "ppd_MI");
    return isFinite(e1) && isFinite(e2) && isFinite(p1) && isFinite(p2) && e2 < e1 && p2 > p1;
  });
  return ok
    ? "In this application the posterior-predictive imputation is smaller in magnitude and less significant than the calibrated-expectation imputation for both outcomes (the point-surface versions in Table S18 do not show this pattern)"
    : "In this application the posterior-predictive imputation is not uniformly conservative across the two outcomes, which is why we report it in full rather than as a one-line reassurance";
};
const jensenVerdict = () => {
  const a = Number(pqRaw("jensen_rel_shift_infant_pct")),
        b = Number(pqRaw("jensen_rel_shift_neonatal_pct"));
  if (!isFinite(a) || !isFinite(b)) return "The size of the shift could not be computed.";
  const m = Math.max(a, b);
  return m < 5
    ? "The scale on which the posterior is averaged is therefore immaterial at the precision at which these coefficients are reported."
    : "The scale on which the posterior is averaged therefore moves the coefficient by up to " + nfmt(m, 1) + "%, which is not negligible; both versions are reported.";
};
// Residual-inclusive comparison (posterior-predictive point surface vs the
// fixed surface), quoted beside the residual-excluding Jensen check so the two
// are never conflated. Signed as in the main text (est10), per 10 points.
const psEst10 = (o, v) => nfmt(10 * Number(psRow(o, v).estimate), 3);
const psSe10 = (o, v) => nfmt(10 * psSEn(o, v), 3);
const psRelTxt = (o) => {
  const a = Number(psRow(o, "epred_point").estimate), b = Number(psRow(o, "ppd_point").estimate);
  return (isFinite(a) && isFinite(b) && a !== 0) ? nfmt(100 * Math.abs(Math.abs(b) - Math.abs(a)) / Math.abs(a), 0) + "%" : "NA";
};
// Endpoint shifts behind the change-scale Jensen gap, from ppd_surface_comparison.csv.
const jensenEndTxt = () => {
  const r15 = ppdSurf.find((x) => /^2015/.test(x.leg || "")), r19 = ppdSurf.find((x) => /^2019/.test(x.leg || ""));
  if (!r15 || !r19) return "the endpoint shifts could not be read";
  const a = Number(r15.jensen_gap_mean), b = Number(r19.jensen_gap_mean);
  if (!isFinite(a) || !isFinite(b)) return "the endpoint shifts could not be read";
  const sg = (v) => (v >= 0 ? "+" : "") + nfmt(v, 3);
  return "the mean endpoint shifts are " + sg(a) + " (2015) and " + sg(b) + " (2019) percentage points, "
    + (Math.sign(a) !== Math.sign(b) ? "of opposite sign, so they add rather than cancel in the 2015-2019 change" : "of the same sign, so they partly cancel in the 2015-2019 change");
};
const PPD_REF = HAVE_PPD ? "SI Methods S5 and Table S18"
                         : "the Supplementary Materials (pending: 23_ppd_sensitivity.R)";
// How much larger the un-propagated estimate is than the primary one. Stated as
// a computed ratio rather than as the word "doubles", which was true of one run
// and need not be true of the next.
const foldTxt = (a, b) => {
  const x = Number((a || {}).est_per10), y = Number((b || {}).est_per10);
  if (!isFinite(x) || !isFinite(y) || y === 0) return "changes the estimate by a factor that could not be computed";
  return "is " + nfmt(x / y, 1) + " times larger";
};
const miSigTxt = (rr) => {
  const v = Number((rr || {}).p);
  if (!isFinite(v)) return "p = NA";
  return v < 0.05 ? "p = " + pfmt(v) : "p = " + pfmt(v) + ", not statistically significant";
};

// Meng (1994) is cited exactly once -- in the estimand paragraph of Section
// 2.4.3, for the uncongeniality of an imputation model estimated independently
// of the analysis model. `refs` is defined further down, so the marker cannot be
// derived from refs.length at this point; it is declared once here and
// cross-checked against `refs` immediately after that array is built, so the
// in-text marker and the list entry cannot drift apart silently.
const MENG_REF = "Meng XL. Multiple-imputation inferences with uncongenial sources of input. Stat Sci. 1994;9(4):538-558.";
const MENG_NO  = 43;

const correctionText = [
  p("We correct the NFHS district estimates by calibrating them to the energy-survey estimates, and it is worth being explicit about why the latter serve as the reference. All three surveys measure cooking fuel by household self-report, and each asks a similar question about the household's primary cooking fuel, so the energy surveys are not a verified gold standard. They differ from NFHS in how the question is posed: NFHS records a single 'main fuel used for cooking' from a general household respondent, whereas ACCESS and IRES ask the primary-fuel question within a dedicated cooking-energy module that first enumerates every fuel the household uses and its quantity, and (in the 2018 ACCESS round) asks the primary cook to be present for that section. A module that establishes the full fuel portfolio before asking which fuel is primary is designed to elicit more complete and consistent reporting, and is less prone than a single undifferentiated item to misclassify the stacking households that dominate rural India. The energy surveys also link households to districts by census code, whereas NFHS-5 clusters are assigned to the 2011 district frame mainly by district name, using their displaced locations for districts created or renamed after 2011 and where location overrides a name match (Section 2.1), a further source of error in the NFHS estimates - though one too small and non-systematic to explain the level gap we observe. We therefore treat the energy-survey estimates as a higher-fidelity, though not infallible, reference. This interpretation is not assumed but is supported by the benchmark analysis, in which the NFHS-reference discrepancy is far larger for the fuel item than for comparably measured demographic characteristics (Section 3), and by an independent official survey (the NSSO 78th-round Multiple Indicator Survey; Section 2.2.4) that also places primary-LPG use above NFHS-5, though below IRES - closing only part of the rural NFHS-IRES gap, which suggests the rural magnitude of the IRES-anchored correction may be too large (SI Methods S8); the corrected estimates are accordingly best understood as calibrated to a better-instrumented reference rather than as validated measurements of true exposure, a limitation we return to in the Discussion. We correct the NFHS estimates in two ways. First, regression calibration: within the overlapping districts we regress the logit of the reference prevalence on the logit of the NFHS prevalence, weighted by reference-survey household counts, and apply the fitted calibration, estimated without state effects so that it can be applied in states the reference survey did not sample, to all NFHS districts (a within-sample refit with state effects is reported in Supplementary Methods S1). Second, a Bayesian hierarchical measurement-error model estimated with the brms package in R [19]: the logit-scale NFHS district estimate enters as a covariate measured with known error (its design-based standard error, delta-method transformed to the logit scale), with state-level random effects and weakly informative priors. The reference logit-prevalence is the response and itself carries a known standard error - for NFHS-5, the Taylor-linearized standard error of the rural design-weighted IRES estimate; for NFHS-4, where no design-based standard error was carried through for the multilevel ACCESS reference, a binomial approximation computed from the district household count. That approximation treats the district sample as an independent draw and so ignores ACCESS's village clustering and weighting; because clustered designs have design effects above one, it understates reference-side uncertainty, and the NFHS-4 fit correspondingly over-trusts the ACCESS points. We therefore treat the NFHS-4 calibration as the less well characterized of the two rather than as its equal, and note that a small fitted residual variance for that pair does not bound the consequences of understating the reference-side sampling variance; the relevant evidence is the refit in Supplementary Methods S1 that gives ACCESS its design-weighted estimate and Taylor-linearized standard error, the same treatment the NFHS-5 leg receives. Both fits add such a residual term to the known standard error - estimated, not assumed - absorbing questionnaire and design non-comparability beyond what sampling error explains; the fitted residual SD is itself a result and is reported in Table S3. Full details of which districts use which standard error, and of the bounds imposed on both, are given in Supplementary Methods S1. Models were estimated with " + MCMC_CHAINS_WORD + " Hamiltonian Monte Carlo chains (" + MCMC_DRAWS.toLocaleString("en-US") + " post-warmup draws; convergence assessed by R-hat and effective sample size). The NFHS-4/ACCESS fit returned " + bdTxt("nfhs4_access") + "; the NFHS-5/IRES fit returned " + bdTxt("nfhs5_ires") + ". " + bdVerdict() + " Corrected district prevalences with 95% credible intervals (CrI, the Bayesian analogue of a confidence interval: the range containing 95% of the posterior probability for the quantity) for all districts were obtained from the posterior distribution of the model's fitted (expected) district prevalence, retaining the estimated state effect for districts in calibration states and drawing a state effect from the fitted state-level distribution for districts in states outside the calibration sample. Each NFHS round is corrected using the reference survey from its own era, and on rural households in both cases: the NFHS-4 rural district estimates are calibrated against ACCESS Wave 1 (rural by design), and the NFHS-5 rural district estimates against the rural design-weighted IRES estimate - so that, for the primary national product, both sides of the calibration are the rural design-weighted direct estimate (like against like). The corrected surface is accordingly the district prevalence of rural primary-LPG use; the all-household NFHS-5/IRES comparison of Section 2.4.2 is reported for completeness but is not used for correction. The two calibrations differ in geographic support. IRES samples districts in " + I_STATES + " states covering 97% of India's population, so applying the NFHS-5 calibration to all Indian districts largely interpolates within the range of settings on which it was estimated; ACCESS covers " + A_W1_D + " districts in six poor northern and eastern states, so applying the NFHS-4 calibration outside those states assumes that the NFHS-reference relationship estimated there also holds elsewhere - an extrapolation that these data cannot verify. We therefore treat the corrected NFHS-5 surface as the primary national product, report the corrected NFHS-4 surface with an explicit extrapolation caveat outside the six ACCESS states, and note that the ACCESS calibration, like the IRES one, has a slope below 1 on the logit scale, although its correction is mild and lowers rather than raises the mean district prevalence. Transportability of the NFHS-5 calibration relationship is assessed by leave-one-state-out cross-validation, refitting the regression calibration without each state in turn (the Bayesian model is not refitted in the folds) and comparing root-mean-square error of corrected versus uncorrected estimates in the held-out state. The regression calibration is estimated on the multilevel district estimates; the Bayesian measurement-error model instead takes the design-weighted direct NFHS estimates as the error-prone input, because these carry valid design-based standard errors (delta-method transformed to the logit scale) that quantify the known measurement-error variance for each district."),
  p("We use both correction methods deliberately. Regression calibration is transparent, easily replicated, and standard in exposure epidemiology, but it treats the error-prone NFHS estimates as fixed: sampling noise in the NFHS values attenuates the fitted calibration slope, and the method yields point predictions without uncertainty. The Bayesian measurement-error model addresses both limitations: each district's true prevalence is treated as latent and estimated jointly with the calibration using that district's known design-based sampling variance, so districts are shrunk toward the calibration line in proportion to their own imprecision, and every corrected estimate carries a credible interval. Agreement between the two approaches indicates robustness of the correction to the treatment of sampling error [12, 26]."),
  p("The estimand for the corrected exposure has to be stated explicitly before that exposure is used in a health model. The value we carry forward is a calibrated point summary of the district's primary-LPG prevalence conditional on its NFHS estimate and the calibration data - the logistic transform of the posterior mean of the district's latent logit prevalence - and not a draw of the district's true prevalence. Because the logistic transform is nonlinear, this is {{not}} the posterior mean prevalence; the multiple-imputation draws described below carry the posterior of the calibration parameters and the NFHS-side error, though not the residual equation error. Two checks of this choice are reported (" + PPD_REF + "): averaging the calibration posterior on the probability scale with the residual equation error excluded, and integrating over the residual equation error as well, which is the posterior-predictive surface. Uncertainty in the calibration parameters is carried into a sensitivity analysis of the health model by exposure-only multiple imputation (Section 2.4.5); the calibration's residual equation error is treated as Berkson-type error and therefore enters through the outcome model rather than through the exposure. The distinction matters because the two components play different roles. Writing the district's latent logit prevalence as the value used plus an error, the first part of that error is posterior uncertainty in the intercept, slope and state effects. It is systematic: the same estimated calibration is applied to every district, so uncertainty in the slope or intercept shifts or rescales the entire exposure surface and passes directly into the estimated health effect. An interval that ignores it is conditional on the fitted calibration; the exposure-only imputation over calibration posterior draws carries it, at the cost of the attenuation described in Section 2.4.5. The second part is the residual equation error. Because the model already carries each reference district's own sampling standard error as a known variance (Supplementary Methods S1), the fitted residual SD is not reference-survey sampling noise but genuine equation error: the district-level mismatch between NFHS and the reference survey that remains after calibration and state effects. Conditional on the fitted calibration model, that residual is district-specific and represents unexplained departures of individual districts from the calibration relationship. Under the usual Berkson assumptions it contributes additional variability to the outcome but does not induce the attenuation associated with classical measurement error: in a linear outcome model it is absorbed into the composite residual, leaving the coefficient unbiased, and the cluster-robust variance estimator captures the resulting increase in residual variability. As with any Berkson argument, this relies on the assumed calibration model and is exact only under its conditions. Imputing instead from the full posterior predictive would add the residual back into every imputed exposure surface, which no longer corresponds to estimating the conditional expectation of district exposure but treats irreducible district-to-district mismatch as if it were uncertainty about the district's underlying exposure; the practical consequence is typically attenuation of the estimated health effect, and because adding the residual also inflates the variance of the exposure itself the standard error need not widen in step. Because the imputation model is estimated independently of the mortality model - the two are uncongenial in Meng's sense [" + MENG_NO + "] - that attenuation is in any case not guaranteed to be conservative. We therefore report the posterior-predictive version as a clearly labelled sensitivity analysis, alongside the probability-scale check described above (" + PPD_REF + ")."),
];

// ---- Fuel stacking: where it varies, whether household covariates predict it,
// and seasonality of the NFHS main-fuel answer (27 and 28). Defined here, ahead
// of the Results, because Section 3.3 quotes them and const is not hoisted; SI
// Section S11 and Tables S22-S23 use the same objects.
const vpTab = readCsv("stacking_variance_partition.csv", { quiet: true }) || [];
const seasTabN = readCsv("nfhs_main_fuel_seasonality.csv", { quiet: true }) || [];
const seasCells = readCsv("nfhs_main_fuel_seasonality_cells.csv", { quiet: true }) || [];
const HAVE_PS = vpTab.length > 0 || seasTabN.length > 0;
if (!vpTab.length || !seasTabN.length)
  console.warn("[build] 28_stacking_place_and_season.R outputs missing -- Section 3.3 will read NA.");
const vpRow = (k) => vpTab.find((x) => String(x.survey).indexOf(k) === 0) || {};
const vpRange = (col, d) => {
  const v = vpTab.map((x) => Number(x[col])).filter(isFinite);
  if (!v.length) return "NA";
  const lo = Math.min(...v), hi = Math.max(...v);
  return nfmt(lo, d) === nfmt(hi, d) ? nfmt(lo, d) : nfmt(lo, d) + "-" + nfmt(hi, d);
};
const fint0 = (v) => (v === undefined || v === "" || v === "NA" || !isFinite(Number(v)))
  ? "NA" : Number(v).toLocaleString("en-US");
// District counts behind the seasonality comparison, read from the output so the
// methods sentence cannot drift: districts in the sample, and those with clusters
// in more than one season (which identify the within-district contrast).
const seasDistTxt = () => {
  const r = (svy) => seasTabN.find((x) => x.survey === svy) || {};
  const one = (svy) => r(svy).n_districts_total ? r(svy).n_districts_total + " districts in " + svy.replace(/^NFHS(\d)$/, "NFHS-$1") + ", " + r(svy).n_districts_multi_season + " of them with clusters interviewed in more than one season" : null;
  const t = ["NFHS4", "NFHS5"].map(one).filter(Boolean);
  return t.length ? t.join("; ") : "district counts NA";
};
const seasRows = (svy, spec) => seasTabN.filter((x) => x.survey === svy && x.specification === spec);
const SPEC_FE = "district fixed effects", SPEC_FT = "district fixed effects + linear time trend";
const seasPick = (svy) => seasRows(svy, SPEC_FT).length ? seasRows(svy, SPEC_FT) : seasRows(svy, SPEC_FE);
const seasSentence = (svy, label) => {
  const r = seasPick(svy);
  if (!r.length) return label + ": not estimable.";
  const sig = r.filter((x) => Number(x.p) < 0.05);
  const nD = r[0].n_districts_multi_season;
  const parts = r.map((x) => x.term.replace(/ \(.*\)/, "").toLowerCase() + " " + (Number(x.pp_vs_winter) >= 0 ? "+" : "") + nfmt(Number(x.pp_vs_winter), 1)
    + " pp (SE " + nfmt(Number(x.se_pp), 1) + ")");
  return label + " (" + fint0(nD) + " districts interviewed in more than one season): relative to winter, " + parts.join(", ") + ". "
    + (sig.length ? "The " + sig.map((x) => x.term.replace(/ \(.*\)/, "").toLowerCase()).join(" and ") + " difference" + (sig.length > 1 ? "s are" : " is") + " distinguishable from zero at the 5% level."
                  : "No seasonal difference is statistically detectable at the 5% level, although the 95% intervals remain consistent with differences of up to about " + nfmt(Math.max(...r.map((x) => Math.abs(Number(x.pp_vs_winter)) + 1.96 * Number(x.se_pp))), 1) + " points.");
};
// Main-text summary across both rounds, wording derived from the estimates.
const seasMain = () => {
  const r = seasPick("NFHS5").concat(seasPick("NFHS4"));
  if (!r.length) return "the seasonal comparison could not be estimated";
  const sig = r.filter((x) => Number(x.p) < 0.05);
  const bound = nfmt(Math.max(...r.map((x) => Math.abs(Number(x.pp_vs_winter)) + 1.96 * Number(x.se_pp))), 1);
  return sig.length
    ? "some seasonal differences are statistically detectable (" + sig.map((x) => x.survey.replace(/^NFHS(\d)$/, "NFHS-$1") + " " + x.term.replace(/ \(.*\)/, "").toLowerCase() + " " + nfmt(Number(x.pp_vs_winter), 1) + " points").join("; ") + ")"
    : "there are no statistically detectable seasonal differences in either round, although the 95% intervals allow differences of up to about " + bound + " percentage points";
};
const ssvTab = readCsv("stacking_state_validation_summary.csv", { quiet: true });
if (!ssvTab.length) console.warn("[build] stacking_state_validation_summary.csv missing -- Section 3.3 state validation reads NA. Run 27_stacking_state_validation.R.");
const ssvChk = (re) => ssvTab.find((x) => re.test(x.check || "")) || {};
const SSV_LOSO = ssvChk(/leave-one-state-out/), SSV_ACC = ssvChk(/ACCESS Wave 1 states/);
const skillTxt = (r) => !isFinite(Number(r.skill_vs_benchmark)) ? "a skill that could not be computed"
  : Number(r.skill_vs_benchmark) <= 0
    ? "a skill of " + nfmt(r.skill_vs_benchmark, 2) + " - no better than assigning every state the overall share"
    : "a skill of " + nfmt(r.skill_vs_benchmark, 2) + " relative to assigning every state the overall share";
const vpShare = (k) => isFinite(Number(vpRow(k).stacking_share)) ? nfmt(100 * Number(vpRow(k).stacking_share), 0) : "NA";
const PLACE_GT_HH = vpTab.length > 0 && vpTab.every((x) => Number(x.marginal_r2_household_pct) < Number(x.pct_place_total));
const stackShareRange = () => { const v = vpTab.map((x) => 100 * Number(x.stacking_share)).filter(isFinite);
  return v.length ? nfmt(Math.min(...v), 0) + " and " + nfmt(Math.max(...v), 0) : "NA"; };
const stWeakTxt = () => {
  const w = Number((stChk[1] || {}).slope_state), a = stApplied[1];
  if (!isFinite(w) || !isFinite(a)) return "";
  return w < a ? "The calibration also has a weakness the cross-validation does not remove: on the NFHS-5 leg the regression-calibration slope estimated within states (" + nfmt(w, 2) + ") is weaker than the pooled slope applied (" + nfmt(a, 2) + "; SI Methods S1), so part of the pooled relation reflects differences between states." : "";
};
const absorbedMax = () => { const v = vpTab.map((x) => Number(x.place_variance_absorbed_pct)).filter(isFinite);
  return v.length ? nfmt(Math.max(...v), 0) : "NA"; };

// ---- Health helpers: every comparative word below is derived from the rows ----
const absE = (r) => Math.abs(Number((r || {}).est_per10));
const sgnE = (r) => Math.sign(Number((r || {}).est_per10));
const MONO = (raw, rc, bay) => sgnE(raw) !== 0 && sgnE(raw) === sgnE(rc) && sgnE(rc) === sgnE(bay)
  && absE(raw) < absE(rc) && absE(rc) < absE(bay);
const monoTxt = (oc, raw, rc, bay) => "the " + oc + " estimate moves from " + est10(raw) + " (raw) to " + est10(rc)
  + " (regression-calibrated) and " + est10(bay) + " (Bayesian-corrected)"
  + (MONO(raw, rc, bay) ? ", the monotone strengthening that classical measurement-error theory predicts as error is removed"
                        : ", which is not the monotone strengthening classical measurement-error theory would predict as error is removed");
// Two outcomes in one sentence without repeating the verdict clause.
const monoMove = (oc, raw, rc, bay) => "the " + oc + " estimate moves from " + est10(raw) + " (raw) to " + est10(rc)
  + " (regression-calibrated) and " + est10(bay) + " (Bayesian-corrected)";
const monoPairTxt = () => {
  const mi = MONO(R_I_RAW, R_I_RC, R_I_BAY), mn = MONO(R_N_RAW, R_N_RC, R_N_BAY);
  const both = monoMove("infant", R_I_RAW, R_I_RC, R_I_BAY) + ", and " + monoMove("neonatal", R_N_RAW, R_N_RC, R_N_BAY);
  if (mi && mn) return both + "; both show the monotone strengthening that classical measurement-error theory predicts as error is removed";
  if (!mi && !mn) return both + "; neither is the monotone strengthening classical measurement-error theory would predict as error is removed";
  return both + "; only the " + (mi ? "infant" : "neonatal") + " estimate shows the monotone strengthening classical measurement-error theory predicts as error is removed";
};
const relPct = (base, other) => {
  const a = absE(base), b = absE(other);
  if (!isFinite(a) || !isFinite(b) || a === 0) return "an amount that could not be computed";
  const d = 100 * (b / a - 1);
  return (d >= 0 ? "a " + nfmt(d, 0) + "% larger" : "a " + nfmt(-d, 0) + "% smaller") + " magnitude";
};
const sameSizeTxt = (base, other) => {
  const a = absE(base), b = absE(other);
  if (!isFinite(a) || !isFinite(b) || a === 0) return "could not be compared";
  if (Math.sign(Number(base.est_per10)) !== Math.sign(Number(other.est_per10)))
    return "opposite in sign to the era-matched estimate";
  const f = b / a;
  return f > 0.9 && f < 1.1 ? "essentially the same size as the era-matched estimate" : (f < 1 ? "smaller in magnitude (" + nfmt(f, 2) + " times the era-matched estimate)"
                                                                 : "larger in magnitude (" + nfmt(f, 2) + " times the era-matched estimate)");
};
const nullTxt = (r) => !hasCr2(r) ? "has no CR2 interval" : (cr2Excludes(r) ? "excludes the null" : "includes the null");
const MI_ATT = (R_I_BAY && R_I_MI) ? absE(R_I_MI) / absE(R_I_BAY) : NaN;
const MI_FLIP = (R_I_BAY && R_I_MI) && Math.sign(Number(R_I_BAY.est_per10)) !== Math.sign(Number(R_I_MI.est_per10));
const miAttTxt = () => !isFinite(MI_ATT) ? "a ratio that could not be computed"
  : MI_FLIP ? "opposite in sign to the fixed-surface estimate"
  : nfmt(MI_ATT, 2) + " times the fixed-surface estimate" + (MI_ATT < 1 ? ", a move toward the null of the kind the attenuation concern predicts" : ", so no attenuation is visible here, although the concern does not depend on it being visible");
const adultSigTxt = () => {
  const hits = adultCsv.filter((x) => x.adjusted === "TRUE" && isFinite(Number(x.p_cr2)) && Number(x.p_cr2) < 0.05);
  if (!hits.length) return "no adult association has a CR2 p below 0.05";
  return "under CR2, " + joinEng(hits.map((x) => (OUTCOME_LAB[x.outcome] || x.outcome).toLowerCase() + " with the "
    + ((ADULT_SPECS.find((s) => s[0] === x.term) || [, x.term])[1]).replace(/^Raw/, "raw").replace(/^Regression/, "regression") + " exposure (p = " + pfmt(x.p_cr2) + ")"))
    + (hits.length > 1 ? " have" : " has") + " p below 0.05, without a consistent pattern across outcomes or exposure specifications";
};
const slope1Txt = (cal) => {
  const v = calP(cal, "slope");
  if (!isFinite(v[1]) || !isFinite(v[2])) return "";
  return v[1] <= 1 && v[2] >= 1 ? "statistically indistinguishable from 1" : (v[2] < 1 ? "below 1" : "above 1");
};
// Fixed surface vs exposure-only imputation, separate vs joint shrinkage (H2).
const miCmp = readCsv("health_mi_comparison.csv", { quiet: true });
const miCmpRow = (oc, shr, prop) => miCmp.find((x) => x.outcome === oc && x.shrinkage === shr && x.propagation === prop) || null;
if (miCmp.some((x) => /outcome-conditional/i.test(x.propagation || "")))
  throw new Error("[build] health_mi_comparison.csv contains outcome-conditioned rows; re-run H2_health_models.R.");

// Separate- versus joint-round shrinkage, with and without the exposure-only
// imputation (health_mi_comparison.csv). Omitted if the file is absent.
const miCmpTxt = () => {
  const r = (shr, prop) => miCmpRow("infant", shr, prop);
  const fs = r("separate rounds", "none (point surface)"), fj = r("joint rounds", "none (point surface)");
  const es = r("separate rounds", "exposure-only MI"), ej = r("joint rounds", "exposure-only MI");
  if (!fs || !fj || !es || !ej) return "";
  return "Shrinking the two NFHS rounds separately rather than jointly (SI Methods S1) moves the infant fixed-surface estimate from "
    + nfmt(fj.est_per10, 3) + " to " + nfmt(fs.est_per10, 3) + " and the exposure-only estimate from " + nfmt(ej.est_per10, 3)
    + " to " + nfmt(es.est_per10, 3) + " (health_mi_comparison.csv; that comparison draws its own set of imputations, so its joint-rounds exposure-only value differs slightly from the estimate above).";
};
const predictText = [
  p("We model three energy-survey outcomes that NFHS does not collect - fuel stacking among primary-LPG households, the four-category composition of fuel use (LPG with no solid fuel reported / stacking / solid fuel with no LPG reported / other non-solid primary fuel), and annual LPG consumption - as functions of the harmonized covariate set - caste category, religion, household size, below-poverty-line/Antyodaya ration-card status, household electricity, and within-state wealth quintile - plus a district-context covariate (the corrected district LPG prevalence), using multilevel logistic, multinomial, and two-part hurdle models respectively. Models are trained era-matched (ACCESS Wave 1 for NFHS-4, IRES for NFHS-5), validated by leave-district-out cross-validation (ten folds of whole districts) and a stricter cross-survey, cross-era test, and applied to NFHS households - rural NFHS-4 households with the ACCESS models, all NFHS-5 households with the all-household IRES models - then aggregated to districts with NFHS sampling weights; combined with the corrected primary-LPG prevalence, they yield district-level exposure proxies (expected LPG-with-no-solid-fuel-reported, stacking, and solid-fuel-with-no-LPG shares, and expected consumption). The model specifications, out-of-sample validation, and era-matched design are detailed in Supplementary Methods S2, and the transfer diagnostic that separates a change of era from a change of survey instrument in SI Methods S3. Item missingness for the exposure and every covariate, together with the sample-linkage exclusions and district coverage, is documented in SI Table S11; the primary-LPG exposure is complete in all four surveys, and covariate missingness is low (the main exceptions being caste, " + missPct1("NFHS-4", "Caste") + "% in NFHS-4 and " + missPct1("NFHS-5", "Caste") + "% in NFHS-5, and the IRES expenditure item, " + missPct1("IRES", "Wealth (index/expenditure)") + "%, which feeds only the wealth quintile). Educational attainment could not be harmonized at all - it is unpopulated in the NFHS household extracts used for the prediction models (the women's recode, used for the health-model covariates, carries women's own schooling) while both energy surveys measure it completely (Table S1) - and is therefore excluded from the covariate set and from the benchmark comparison."),
  p("Three further analyses describe fuel stacking directly in the energy surveys. First, to ask where stacking varies, we modelled stacking among primary-LPG households in each reference survey with state, district and village random intercepts and no household characteristics, and then with the harmonized household characteristics added; the place-level shares are variance shares on the latent logistic scale, whose household-level variance is fixed at pi^2/3, so they are latent-scale intraclass correlations rather than shares of observed variation, and the household characteristics are summarized by their marginal R2. Second, to ask whether those characteristics predict stacking where it was not observed, we refitted the household model in IRES leaving out one state at a time, compared each held-out state's predicted and observed stacking share, and expressed the result as a skill score against a benchmark that assigns the held-out state the overall stacking share of the remaining states (one minus the ratio of the model's root-mean-square error to the benchmark's, so that a value at or below zero means no state-level signal); states with fewer than 20 rural primary-LPG households were not scored, and the all-state IRES model was also applied to the six ACCESS Wave 1 states. Third, because stacking households may change which fuel they call primary across seasons, we compared, within the same district, NFHS clusters interviewed in different seasons (district fixed effects, with and without a linear trend in interview date, conventional cluster-robust standard errors clustered on district; " + seasDistTxt() + "). All analyses were conducted in " + R_VERSION_TXT + "; the analysis code, including the script that generates this manuscript from the analysis outputs, is available at https://github.com/pdez90/India_householdenergy."),

  h3("2.4.5 Exploratory illustration: corrected exposure and child mortality (rural districts; all-household analysis in the Supplementary Materials)"),
  p("As an exploratory illustration of how the corrected exposure behaves in a health analysis, we estimated district-level change-on-change associations between clean-cooking prevalence and child mortality, following the design of prior analyses linking the clean-cooking transition to child health in India [42]. District child-mortality probabilities were estimated from the NFHS-4 and NFHS-5 births recode (full birth-history) files as recent-cohort probabilities over the full birth history in the ten years (120 months) preceding each survey. For each birth we computed age at interview from the century-month birth and interview dates, and counted a birth toward an outcome - neonatal (death before one month) or infant (before twelve months) - only once it had been exposed for the full age interval, whether it survived or died; more recent births not yet old enough to have completed the interval were treated as censored (missing) for that outcome, whether alive or dead, so each estimate is a completed-exposure cohort probability rather than a raw death share. We used the full birth history reported by interviewed mothers, including children no longer resident in the household, rather than restricting living children to those currently co-resident: because deceased children have no co-residence status, a co-residence restriction would retain all deaths but drop surviving children who had moved away, biasing mortality upward. This yielded " + births("rural", 2015) + " (NFHS-4) and " + births("rural", 2019) + " (NFHS-5) rural births in the observation window, and " + births("all", 2015) + " and " + births("all", 2019) + " births across all households; of the rural births, " + eligB("rural", 2015, "infant") + " and " + eligB("rural", 2019, "infant") + " had been exposed for the full first year and enter the infant outcome, and " + eligB("rural", 2015, "neonatal") + " and " + eligB("rural", 2019, "neonatal") + " the neonatal outcome. Births were assigned to NFHS-4 districts through the same cluster-to-district assignment as the exposure (Section 2.1: the DHS district code for NFHS-4 clusters, the name and location crosswalk for NFHS-5 clusters); births whose cluster had no district in the crosswalk are dropped before these counts, and the crosswalk leaves no cluster unassigned. District mortality prevalences for each round were obtained from the same four-level multilevel specification as Equation 1 (the primary estimator); DHS design-weighted direct district mortality (sampling weights, PSU-clustered standard errors) was also computed and is used only to describe the outcome's sampling error (Discussion). The socioeconomic covariates used for adjustment - the share in the lowest national wealth quintile, the share of women aged 15-49 with no schooling, household electricity, Muslim share, improved sanitation, and improved water - were estimated separately from the women's recode (individual-record) files, one record per woman rather than per birth, so that they represent woman/household composition rather than birth-weighted quantities, and were merged to districts. Analyses used the change (2019 minus 2015, per 100 births) as the outcome. Because the survey rounds are about four years apart, the two ten-year birth-history windows overlap substantially in calendar time, so this change is best read as the difference between two overlapping retrospective cohort estimates rather than a clean four-year mortality change; a shorter 36-month recent-cohort window, chosen to be shorter than the roughly four-year gap between the rounds so that the two cohorts overlap far less in calendar time (non-overlap is not enforced birth by birth), is reported as a sensitivity (SI Table S6b). Because the measurement-error correction is estimated on rural households (Section 2.4.3), the primary mortality analysis restricts district mortality to rural births as well, so that the exposure and the outcome describe the same population; an all-household version, which uses the identical exposure, is reported in the Supplementary Materials as a sensitivity analysis."),
  p("The exposure was the district change in primary-LPG prevalence, measured six ways: raw NFHS rural estimates; regression-calibrated; the era-matched Bayesian correction held fixed at the logistic transform of its posterior-mean logit (ACCESS calibrates the 2015 estimate and IRES the 2019 estimate); the same correction with calibration uncertainty carried by exposure-only multiple imputation (200 imputations of the corrected 2015 and 2019 district prevalence drawn from the calibration posterior - a shared calibration draw plus each district's NFHS-side error, without the outcome, correlated between rounds - forming the change on each, refitting, and pooling by Rubin's rules); a Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both NFHS rounds, so the corrected change does not switch calibration instrument between the two endpoints); and that instrument-consistent version with exposure-only imputation. Models were weighted by the harmonic mean, across the two rounds, of the number of births in each district that are eligible for the outcome being modelled - those in the ten-year window that had been exposed for the outcome's full age interval, counted separately for neonatal and infant mortality (a births-based analytic weight, not a population denominator; an unweighted specification is reported as a sensitivity). The harmonic mean is an approximate precision weight: if the variance of each round's rate were proportional to the reciprocal of its birth count, the variance of the difference would be proportional to the sum of the reciprocal counts, and the harmonic mean weights each district by the reciprocal of that sum. It is not an exact inverse-variance weight for mortality estimates that differ in prevalence, are clustered and are partially pooled. Models were also adjusted for concurrent district changes in poverty (lowest national wealth quintile), women's lack of schooling, household electricity, Muslim share, improved sanitation, and improved water, and for concurrent changes in ambient environmental conditions - annual mean fine particulate matter (PM2.5), mean near-surface temperature, relative humidity, and drought (the Palmer Drought Severity Index) - together with region fixed effects, and clustered on state. Inference on a district-level regression clustered on " + nClus(R_I_BAY) + " states is a small-cluster problem, and the conventional cluster-robust sandwich (CR1, the default with a G/(G-1) small-sample factor) is known to be anticonservative there. We therefore report the bias-reduced CR2 estimator of Bell and McCaffrey with Satterthwaite degrees of freedom as the primary inference for every clustered regression in the health analyses, and a wild cluster bootstrap with the null imposed (Rademacher weights applied at the state level, " + wildB() + " replications) as a robustness check; the conventional CR1 result is reported alongside so that the two can be compared. For the imputation-based estimates the CR2 standard error is pooled across imputations by Rubin's rules and its degrees of freedom by the Barnard-Rubin correction. How much the three rules differ on this design, calibrated by simulation against a known null, is reported in SI Methods S6. " + cr2LimTxt() + " Because the era-matched correction uses different reference instruments at the two endpoints, we report the single-instrument version to separate any change in the association from an artifact of switching calibration instrument between rounds."),
  p("Neither treatment of the corrected exposure gives a validated estimate, and we interpret them accordingly. The fixed-surface estimate is conditional and exploratory: it is conditional on the fitted calibration, and its interval does not include calibration uncertainty. The exposure-only imputation includes calibration uncertainty, but because each imputed exposure is drawn without reference to mortality it adds district-level noise unrelated to the outcome, which in a linear model attenuates the coefficient toward zero by an amount Rubin's rules cannot detect; we report it as a sensitivity analysis with that concern stated, not as a corrected estimate. Imputing conditional on the outcome as well as on the calibration posterior would address the attenuation. We attempted it twice, and both attempts failed convergence checks; the second was governed by a specification and stopping rule fixed before its pilot, and development stopped when the pilot failed (SI Methods S12). No estimate from either attempt is reported."),
  p("Ambient covariates were constructed at the district level for the two survey windows (2011-2015 for NFHS-4 and 2016-2020 for NFHS-5) and differenced. District mean PM2.5 was extracted from the satellite-derived Atmospheric Composition Analysis Group V5.GL.03 hybrid product (0.01-degree annual surfaces combining multiple satellite aerosol products with chemical-transport modeling and ground-monitor calibration) [27]; ambient PM2.5 is a robust district-level predictor of infant mortality in India and is therefore an important confounder to adjust for in this design [40]. Temperature, relative humidity, and drought were extracted from the TerraClimate 1/24-degree monthly climate reanalysis [28]; relative humidity was derived from TerraClimate's actual vapor pressure and vapor-pressure deficit as 100 x vap / (vap + vpd). All ambient surfaces were summarized to district means by averaging the gridded values at the district's NFHS-4 and NFHS-5 cluster locations (assigned to districts as in Section 2.1), which places each district's exposure where its survey households live; the district boundary file was used only for the few districts with no located cluster."),
  p("Two features of the design bound the influence of survey timing on the corrected exposure. First, the calibration is era-matched: NFHS-4 is corrected against ACCESS (2014-15) and NFHS-5 against IRES (2019-20), so no estimate is calibrated against a temporally distant reference. Second, because NFHS-5 fieldwork is state-staggered and extends beyond the concentrated IRES window while LPG use was still rising under PMUY, calibrating late-surveyed districts toward IRES could in principle pull the corrected 2019 prevalence slightly downward; but restricting NFHS-5 to season-matched or pre-COVID interviews leaves the gap essentially unchanged (Section 2.4.2; the calibration slope was not refitted on those restricted samples), so field timing does not appear to explain the discrepancy. Any residual downward bias would compress the estimated 2015-2019 exposure change."),
  p("Two exploratory extensions are reported in the Supplementary Materials. First, we substituted the predicted district composition metrics from Section 2.4.4 - the LPG-no-solid-reported share, the share of households reporting any solid fuel, the fuel-stacking share, and predicted LPG consumption - for primary-LPG prevalence in the same change-on-change design, using a model-consistent version in which the IRES composition models score the rural households of both rounds, so that the change is not confounded by switching models between eras; because these metrics are model-predicted rather than measured and their between-round change inherits the prediction models' limited transportability, they are treated as hypothesis-generating only. Second, as an external check that does not depend on the correction, we examined simple cross-sectional (level-on-level) associations within each era-matched pair between NFHS district mortality and the reference-survey energy metrics measured directly in ACCESS (with NFHS-4) and IRES (with NFHS-5)."),
  h3("2.4.6 Decomposing the corrected exposure's uncertainty"),
  p("Each corrected district value is a + b x + u_s, where x is the NFHS district estimate entering the measurement-error model as a covariate observed with known error, so its posterior variance separates into two parts: a calibration-side part, carried by the posterior of the intercept, slope and state effect, which depends on the reference (energy) survey; and an NFHS-side part equal to b squared times the posterior variance of the district's true NFHS value (its sampling variance, reduced by shrinkage toward the state mean), which is a property of the health survey being corrected. We reconstructed the calibration-side part analytically, in closed form, as a Bayesian linear mixed model with known within-district variances, averaging over the posterior draws of the residual and state standard deviations rather than conditioning on their posterior medians." + p246Checks() + " The decomposition is descriptive: it apportions the uncertainty of the correction as fitted, and we do not use it to predict what a differently designed reference survey would achieve (SI Methods S4)."),

  h3("2.4.7 Supplementary analysis: adult cardiometabolic outcomes (rural districts)"),
  p("As a secondary analysis we applied the same district change-on-change framework to two adult cardiovascular risk factors measured in NFHS, hypertension and diabetes, following the design of a prior study of clean cooking and hypertension in India [29] and using person-level indicators carried over from that study's analysis file. District prevalences among rural adult respondents (women aged 15-49 and men aged 15-54) were estimated for each round from the same four-level specification as Equation 1 and regressed on the district change in primary-LPG prevalence with the same adjustment set and CR2/Satterthwaite inference as the child-mortality models, unweighted. Because these risk factors reflect decades of cumulative exposure, a single-decade district design is poorly suited to detecting change in them, and we report them as supplementary: the outcome definitions, a person-level verification of their coding against the raw recodes, and the results are given in SI Methods S7."),
];

// ============================ TABLE 4 (health) ==============================
// Six columns, not four. CR2/Satterthwaite is the primary inference and comes
// first; the conventional CR1 rule follows so a reader can see exactly how
// much of any threshold crossing is the inference rule rather than the
// exposure. The point estimate is shared, so it is printed once, in the CR2
// interval cell.
const COLW4 = [1200, 2500, 1900, 800, 1900, 800];
const HEALTH_HDR = ["Outcome", "Exposure measurement",
                    "Est. per 10-pp LPG [95% CI], CR2", "p (CR2)",
                    "95% CI, conventional", "p (conv.)"];
const table4 = mkTable(COLW4, [HEALTH_HDR]
  .concat(healthRows(heRural)).concat(healthRowsUnw(heRural)));
const table4all = mkTable(COLW4, [HEALTH_HDR]
  .concat(healthRows(heAll)).concat(healthRowsUnw(heAll)));
// Whether the shorter window actually widened the fixed-surface infant CR2
// interval is read from the rows, not assumed (it did not in the 2026-10-21 run).
const ciW = (r) => hasCr2(r) ? 10 * (Number(r.ci_cr2_high) - Number(r.ci_cr2_low)) : NaN;
const s6bPrecTxt = () => {
  const a = ciW(NO_I_BAY), b = ciW(R_I_BAY);
  if (!isFinite(a) || !isFinite(b)) return "interval widths could not be compared";
  return a < b
    ? "the fixed-surface infant CR2 interval is nevertheless narrower than in Table S6 (" + ci10cr2(NO_I_BAY) + " against " + ci10cr2(R_I_BAY) + "), so precision does not simply track the birth count here"
    : "the fixed-surface infant CR2 interval is correspondingly wider than in Table S6 (" + ci10cr2(NO_I_BAY) + " against " + ci10cr2(R_I_BAY) + ")";
};
// Table S6b: the shorter-window 36-month recent-cohort sensitivity, which the
// main text (Section 2.4.5) and SI Methods S6 cite by table number.
const table4nonov = mkTable(COLW4, [HEALTH_HDR].concat(healthRows(heNonov)));
const N_NONOV = heN(heNonov);
// Quoted in the Table S7 caption; read back so the caption cannot outlive it.
const P_INF_ALL_BAYES = pfmt((heRow(heAll, "infant", "change_lpg_bayes") || {}).p);
// The Table S7 caption used to read "reaches p = " + P_INF_ALL_BAYES, which
// asserts a threshold crossing. When the all-household infant p moved from
// 0.046 to 0.051 the sentence became false while the number beside it updated
// correctly -- the same failure mode as the hard-coded MCMC verdict. Derive
// the verb from the value instead.
// Reported on the PRIMARY rule (CR2), with the conventional value beside it,
// and the verb derived from the value rather than typed -- the hard-coded
// "reaches p =" in an earlier draft outlived the number it described.
const allBayesPTxt = () => {
  const r  = heRow(heAll, "infant", "change_lpg_bayes") || {};
  const v2 = Number(r.p_cr2), v0 = Number(r.p);
  if (!isFinite(v2))
    return isFinite(v0)
      ? "has a conventional p of " + P_INF_ALL_BAYES + " (CR2 unavailable)"
      : "has p = NA";
  const c1 = Number(r.p);
  if (v2 >= 0.05 && isFinite(c1) && c1 >= 0.05)
    return "is not significant under either rule (CR2 p = " + pfmt(v2) + "; conventional CR1 p = " + P_INF_ALL_BAYES + ")";
  return (v2 < 0.05 ? "reaches p = " : "does not cross the conventional 0.05 threshold (p = ")
    + pfmt(v2) + (v2 < 0.05 ? "" : ")")
    + " under CR2, against " + P_INF_ALL_BAYES + " under the conventional CR1 rule";
};

// ============================ RESULTS ========================================
const results = [
  h2("3.1 NFHS and the specialized surveys agree before the policy expansion and diverge sharply after it"),
  p("District-level estimates of primary LPG use were produced for " + N_D_NFHS4 + " (NFHS-4) and " + N_D_NFHS5 + " (NFHS-5) districts on the NFHS-4 geography, for the " + cmpN(C4R) + " ACCESS Wave 1 districts, and for " + cmpN(C5A) + " IRES districts (Figure 1; agreement statistics in Table S2). In the pre-PMUY period the national health survey and the specialized survey agree closely: NFHS-4 rural clusters versus the all-rural ACCESS sample give r = " + cmpV(C4R, "pearson") + " (logit-scale r = " + cmpV(C4R, "pearson_logit") + "), and r = " + cmpV(C4RD, "pearson") + " with design-weighted estimates on both sides (Lin's CCC = " + cmpV(C4RD, "ccc") + ") and a mean difference of only " + cmpDiffPP(C4RD) + " percentage points (district means of " + cmpLvl(C4RD) + " in ACCESS; Figure 1a, c). The weaker correlation obtained when all NFHS-4 households are compared with rural ACCESS (r = " + cmpV(C4A, "pearson") + ") is an artifact of urban/rural composition rather than of measurement, and weighting districts by their reference-survey sample sizes changes correlations negligibly (Table S2)."),
  p("After PMUY the picture changes. Agreement between NFHS-5 and IRES remains high in correlation terms (all-household r = " + cmpV(C5A, "pearson") + "; rural r = " + cmpV(C5R, "pearson") + ") but now carries a large, systematic level difference: NFHS-5 rural estimates average " + cmpGap("NFHS-5 (rural) vs IRES (rural)", 1) + " percentage points lower than IRES (district means of " + cmpLvl(C5R) + " in IRES, over the " + cmpN(C5R) + " districts with rural households in both surveys; multilevel; " + cmpGap(C5RD, 1) + " points design-weighted, rural-vs-rural), across nearly the entire prevalence range (Figure 1b, d). Nor is the gap a composition artifact: comparing all households on both sides - urban and rural together, over all " + cmpN(C5A) + " common districts (r = " + cmpV(C5A, "pearson") + ", against " + cmpV(C5R, "pearson") + " rural and " + cmpV(C5RD, "pearson") + " rural design-weighted) - still leaves a large level difference, " + cmpGap(C5A, 1) + " percentage points (" + cmpLvl(C5A) + " in IRES), so broadening the frame narrows but does not close the gap (Table S2). Nor is it a timing artifact: NFHS-5 interviews straddle the IRES fieldwork, but restricting NFHS-5 to pre-COVID or to winter interviews leaves the gap essentially unchanged (below), and the corresponding NFHS-4/ACCESS difference is small (" + cmpDiffPP(C4R) + " points rural; " + cmpDiffPP(C4RD) + " design-weighted). The divergence is thus specific to the post-PMUY NFHS-5/IRES comparison, the period in which tens of millions of marginal LPG-owning households entered the denominator. Because the earlier and later comparisons use different reference surveys, the contrast between eras could in principle reflect a difference between ACCESS and IRES rather than a change in the NFHS item; we return to this in the limitations (Section 4.1)."),
  p("The disagreement is far larger for the fuel item than for the caste and religion composition of who was surveyed. In a benchmark (falsification) analysis (Table S4; SI Figure S5), district prevalences of caste and religion agree closely in level between NFHS-5 and IRES - mean differences of " + bmDiff(BM5, "st") + " points for Scheduled Tribe, " + bmDiff(BM5, "hindu") + " for Hindu, " + bmDiff(BM5, "muslim") + " for Muslim, and " + bmDiff(BM5, "sc") + " for Scheduled Caste, against " + bmDiff(BM5, "lpg") + " points for primary LPG - so on these benchmarks the two surveys sample similarly composed rural populations, and differences in them are far too small to account for the LPG gap. Timing does not account for it either: restricting NFHS-5 to pre-COVID interviews changes the rural gap from " + SEAS_ALL + " to " + SEAS_PRECOV + " points, and restricting to winter interviews (season-matched to IRES fielding) gives " + SEAS_WINTER + " points. The same falsification holds district by district when the NFHS and reference surfaces are mapped side by side on a common scale (SI Figures S17-S18), and the same table shows that not every item asked in both surveys agrees: household electricity agrees in level (mean difference " + bmDiff(BM5, "electricity") + " points) but only weakly across districts (r = " + bmR(BM5, "electricity") + "), and possession of a below-poverty-line ration card, whose question differs between the instruments, differs by " + bmDiff(BM5, "bpl") + " points, at least as much as primary LPG (Table S4). These two programme-linked items are therefore not clean benchmarks, and the comparison that bears on sample composition is the caste and religion one. The discrepancy persisted after the examined restrictions and was much larger than the differences in caste and religion composition, supporting a fuel-specific measurement explanation; these checks cannot exclude differences in unmeasured characteristics, non-response or field procedures (see the limitations in Section 4)."),
  img("paper_figs/fig1_agreement.jpeg", 440, 422),
  caption("Figure 1. District-level agreement between NFHS and energy-survey estimates of primary LPG use. (a) NFHS-4 rural vs ACCESS Wave 1 and (b) NFHS-5 rural vs IRES rural, against the identity line (dashed); (c, d) corresponding Bland-Altman plots (solid line = mean difference; dashed = 95% limits of agreement). Point size proportional to reference-survey households; r = Pearson correlation. (e) Mean district gap (NFHS minus energy survey, percentage points) for primary LPG and for each benchmark item before PMUY (NFHS-4 vs ACCESS) and after it (NFHS-5 vs IRES), from the multilevel benchmark estimates of Table S4 (six ACCESS states for the earlier pair, hence slightly different primary-LPG values from Table S2); items whose gap changes by more than 10 percentage points are highlighted. Electricity and the ration card are programme-linked items rather than clean benchmarks (Section 3.1)."),

  h2("3.2 The divergence can be calibrated, and a regression calibration held up in leave-one-state-out tests"),
  p("Because the evidence points to how cooking fuel is measured rather than to sample composition or timing, we calibrated the NFHS district estimates to the energy surveys with a Bayesian measurement-error model (Table S3). Calibrating NFHS-4 against ACCESS yields a slope on the logit scale of " + calE(CAL4, "slope") + " (95% CrI " + calCrI(CAL4, "slope") + ", " + slope1Txt(CAL4) + ") with a small intercept and little between-state variation (SD " + calE(CAL4, "sd_state") + "); over the observed prevalence range the calibrated values stay close to the NFHS-4 estimates, and the correction is correspondingly mild. Calibrating NFHS-5 against the rural design-weighted IRES estimate (with its Taylor-linearized standard error as the reference-side measurement error) yields a slope of " + calE(CAL5, "slope") + " (95% CrI " + calCrI(CAL5, "slope") + ") with a large intercept (" + calE(CAL5, "intercept") + ") and substantial between-state heterogeneity (SD " + calE(CAL5, "sd_state") + "): NFHS-5 understates prevalence across nearly all of the observed range. The slope below 1 is a statement on the logit scale, conditional on the state effect - NFHS-5 spreads districts further apart in log-odds than IRES does - and does not by itself say where the revision is largest in percentage points; " + revTxt() + " (SI Table S10). The corrected surface is on average roughly " + CORR_PP + " percentage points higher than the raw rural estimates (the mean across districts, each weighted equally, rising from " + RAW19_0 + "% to " + BAY19_0 + "%). " + stRevTxt() + " (Figure 2a, b); district-level corrections against the raw estimates are shown in Figure 2c."),
  p("In leave-one-state-out cross-validation, a regression calibration estimated without a given state reduces the root-mean-square error of that state's district estimates against the held-out reference in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states, with the median held-out RMSE falling from " + LOSO_MED_RAW + " to " + LOSO_MED_CORR + " (summary in SI Table S3). This supports the transportability of the calibration relationship; because the folds refit the regression calibration rather than the Bayesian model, it does not directly validate the Bayesian corrected surface or its credible intervals. Given the magnitude of the implied correction, verification of the IRES primary-fuel coding and of possible survey-mode effects (a detailed energy module eliciting fuller LPG reporting than a single item) are priorities we return to in the Discussion; the direction of the correction, however, is robust to the timing argument above."),
  img("paper_figs/fig2_correction.jpeg", 460, 161),
  caption("Figure 2. Correcting the NFHS-5 district estimates. (a) Corrected district prevalence of rural primary LPG use (Bayesian measurement-error model, IRES-calibrated; proportion scale); (b) correction shift relative to raw NFHS-5 rural estimates, as a proportion (red = corrected higher); grey areas have no district estimate; (c) corrected versus raw district estimates (red = Bayesian, blue = regression calibration; dashed = identity)."),

  h2("3.3 Fuel stacking varies with place more than with household, and the characteristics NFHS records did not predict it in the settings tested"),
  p("The energy surveys record what the NFHS item cannot. Among households reporting LPG as their primary cooking fuel, " + vpShare("IRES") + "% in rural IRES, " + vpShare("ACCESS Wave 1") + "% in ACCESS Wave 1 and " + vpShare("ACCESS Wave 2") + "% in ACCESS Wave 2 also reported burning a solid fuel (Table S22). That behaviour is concentrated by place. On the latent logistic scale, state, district and village intercepts together account for " + nfmt(Number(vpRow("IRES").pct_place_total), 1) + "% of the variation in stacking in rural IRES and " + nfmt(Number(vpRow("ACCESS Wave 2").pct_place_total), 0) + "-" + nfmt(Number(vpRow("ACCESS Wave 1").pct_place_total), 0) + "% in the two ACCESS waves, whereas the household characteristics a national survey records - caste, religion, household size, ration card, electricity and within-state wealth - account for " + vpRange("marginal_r2_household_pct", 1) + "% (marginal R2) and absorb at most " + absorbedMax() + "% of the place-level variance. In every survey, therefore, the characteristics explain " + (PLACE_GT_HH ? "less of stacking than where a household lives does" : "a share of stacking comparable to where a household lives") + "."),
  p("Consistent with that, household characteristics predict stacking poorly. In leave-district-out cross-validation the household model reached an area under the ROC curve of " + ldo("ACCESS W1", "auc") + " in ACCESS and " + ldo("IRES rural", "auc") + " in IRES, with district-level correlations between predicted and observed stacking of " + ldo("ACCESS W1", "district_r") + " and " + ldo("IRES rural", "district_r") + ". Predicting each IRES state from a model fitted to the others gave a state-level correlation of " + nfmt(SSV_LOSO.r_state, 2) + " across " + (SSV_LOSO.n_states || "NA") + " states whose observed stacking shares range from " + String(SSV_LOSO.observed_range_pp || "NA").replace("-", " to ") + "%, and " + skillTxt(SSV_LOSO) + ". Applied to the six ACCESS Wave 1 states the all-state IRES model fared better (state-level r = " + nfmt(SSV_ACC.r_state, 2) + ", " + skillTxt(SSV_ACC) + "), a comparison resting on six states. Models also transferred poorly between surveys. Changing only the era (ACCESS Wave 1 to Wave 2) gave a district-level correlation of " + trR("ERA") + ", changing only the instrument at nearly constant era (IRES to ACCESS Wave 2) " + trR("INSTRUMENT") + ", and changing both (IRES to ACCESS Wave 1) " + trR("ERA+INSTRUMENT") + "; " + trWeakTxt() + " (SI Methods S3; SI Tables S12-S15)."),
  p("Season does not account for the NFHS answer either. Comparing NFHS clusters interviewed in different seasons within the same district, " + seasMain() + " (Table S23; SI Methods S11). Together these results mean that the models we tested did not recover fuel stacking reliably from the NFHS primary-fuel item and the covariates NFHS already collects, in the transfer settings we evaluated: where households live explains more of stacking than those covariates do, at a finer geographic scale than they describe, and the relationship that does exist did not transfer reliably between the surveys tested. Stacking has to be asked about to be measured."),

  h2("3.4 Part of the corrected exposure's uncertainty belongs to the health survey"),
  p("Downstream analyses often treat a corrected exposure surface as though it were a measurement, although some propagate its uncertainty. Its posterior variance has two sources (Section 2.4.6): the calibration, carried by the posterior of the intercept, slope and state effects, and the NFHS district estimate that the calibration is applied to, which enters as b^2 times the posterior variance of the district's true NFHS value. The decomposition (SI Table S17) puts the NFHS-side part at " + DS("var_nfhs_side_share_2015", 0) + "% of the total on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg. The two legs differ because the calibration relationship is itself " + sigNoisierTxt() + " after the policy expansion: the fitted residual SD (posterior root-mean-square) is " + vd("2019", "sigma", 3) + " on the logit scale in 2019 against " + vd("2015", "sigma", 3) + " in 2015, roughly " + sigRatio(1) + " times larger, so the calibration accounts for " + (DSn("var_nfhs_side_share_2019") < DSn("var_nfhs_side_share_2015") ? "more" : "less") + " of the variance on the 2019 leg than on the 2015 leg. Because the model already carries each reference district's sampling variance as a known quantity, that post-expansion residual is equation error - district-level disagreement between NFHS-5 and IRES that the calibration leaves unexplained, whatever its source - rather than reference-survey sampling noise. The decomposition is descriptive; we do not use it to predict what a differently designed reference survey would achieve (SI Methods S4)."),

  h2("3.5 Exploratory illustration: the corrected exposure and child mortality"),
  p("We carried the corrected exposure into a district change-on-change model of child mortality across the " + N_RURAL + " rural districts with mortality estimates, corrected exposure and complete covariates. Because the attempts to impute the exposure conditional on the outcome failed their convergence checks (SI Methods S12), the association is reported in two forms, neither of which is a validated estimate. Holding the corrected surface fixed at its point summary - as an analyst adopting a published surface would - the infant association is " + est10(R_I_BAY) + " deaths per 100 births per 10-percentage-point rise in primary-LPG prevalence (95% CI " + ci10cr2(R_I_BAY) + ", p = " + pCr2(R_I_BAY) + ", " + dfCr2(R_I_BAY) + " degrees of freedom on " + nClus(R_I_BAY) + " state clusters; wild cluster bootstrap p = " + pWild("infant", "change_lpg_bayes") + "), and the neonatal association " + est10(R_N_BAY) + " (95% CI " + ci10cr2(R_N_BAY) + "). These estimates are conditional on the fitted calibration, and their intervals do not include calibration uncertainty. In the exposure-only imputation, which includes calibration uncertainty but draws each imputed exposure without reference to mortality, the infant estimate is " + est10(R_I_MI) + " (95% CI " + ci10cr2(R_I_MI) + "), " + miAttTxt() + "; the neonatal estimate is " + est10(R_N_MI) + " (95% CI " + ci10cr2(R_N_MI) + "). Because the imputation adds district-level noise unrelated to mortality, its interval cannot be read as the fixed-surface interval correctly widened, and its point estimate is at risk of being too close to zero."),
  p("Under small-cluster-corrected inference " + (nullTxt(R_I_BAY) === nullTxt(R_I_MI) ? "both the fixed-surface and the exposure-only infant intervals " + (hasCr2(R_I_BAY) ? nullTxt(R_I_BAY).replace(/^excludes/, "exclude").replace(/^includes/, "include") : "lack a CR2 interval") : "the fixed-surface infant interval " + nullTxt(R_I_BAY) + " and the exposure-only interval " + nullTxt(R_I_MI)) + ". The conventional clustered rule is anticonservative at " + nClus(R_I_BAY) + " clusters" + (HAVE_CALIB ? ", rejecting a true null in " + calibRate(0) + "% of simulated datasets on this design against a nominal 5%, where CR2 rejects in " + calibRate(1) + "% and the wild bootstrap in " + calibRate(2) + "% (SI Methods S6)" : "") + "; under it the fixed-surface infant estimate has p = " + pfmt((R_I_BAY || {}).p) + "." + (CR2_OVER ? " CR2 is therefore the better rule here but not an exact one: in this simulation its p-values were somewhat optimistic." : "") + " Across exposure specifications " + monoTxt("infant", R_I_RAW, R_I_RC, R_I_BAY) + ". Applying a single calibration instrument at both endpoints gives an infant estimate of " + est10(R_I_IRES) + ", " + sameSizeTxt(R_I_BAY, R_I_IRES) + ". The full set of specifications, the model and weighting details, the inference-rule comparison and the restrictions to districts with calibration support are given in SI Methods S6 (Table S6; SI Figure S21)."),
  p("We therefore read these data as neither establishing nor excluding a mortality benefit of the clean-cooking transition at the district level. The illustration shows two questions that any analysis adopting a calibrated exposure surface has to answer - whether its result is conditional on the calibration, and how calibration uncertainty is carried - and, in this application, that the second question did not have a satisfactory answer: the method that ignores the outcome is at risk of attenuation, and the methods that condition on it did not converge. The secondary analyses - predicted fuel composition and adult cardiometabolic outcomes - are reported in SI Methods S7."),
];

const discussion = [
  p("A single item in one of the world's largest health surveys changed its relationship to household fuel use over exactly the period a national program was changing that use. Before India's LPG expansion, NFHS-4 and ACCESS agreed on rural primary-LPG prevalence to about one percentage point on design-weighted estimates; after it, NFHS-5 recorded roughly " + GAP_DW + " percentage points less primary LPG use than IRES (district means of " + cmpLvl(C5RD) + " in IRES), while the same surveys continued to agree closely on the demographic benchmarks compared and neither field timing, season, weighting nor rural-urban composition accounted for the difference. A specialized survey could detect that change, point to the question rather than the sample as its likeliest source, and calibrate it: calibrating against the energy surveys materially changed the exposure surface, and a regression calibration refitted without each state reduced held-out error in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states. The energy surveys also show what the item cannot record. Between " + stackShareRange() + "% of primary-LPG households also burned a solid fuel, stacking varied far more with place than with the household characteristics a national survey records, those characteristics did not predict it in held-out IRES states, and the relationship transferred poorly between surveys. These are the paper's findings, and none of them depends on a health model."),

  p("The exposure measures that make national health surveys so valuable are optimized for breadth and low marginal cost, not for fidelity to any one exposure. NFHS, like the Demographic and Health Surveys worldwide, must capture dozens of domains in a single visit, and cooking energy is reduced to one primary-fuel item with no probing of secondary fuels, cylinder refills, or quantities - reported by a general household respondent rather than the primary cook, and within a general questionnaire rather than a dedicated energy module. Our central empirical finding is consistent with this design choice mattering: an energy survey that asks about cooking fuel within such a module disagrees with NFHS-5 by about " + GAP_DW + " percentage points while agreeing closely with it on caste and religion composition, and an independent official survey also places LPG use above NFHS-5, though below IRES, closing only part of the rural gap (SI Methods S8). The households the NFHS item misclassifies are unlikely to be a random subset: the marginal LPG adopters created by policy are precisely the households a single 'main fuel' question cannot resolve, because they stack fuels. Even a non-fuel item in our data (the ration-card question) shifts with how it is asked, showing that instrument design can shift answers to items other than cooking fuel. The tension between scale and fidelity is general: drinking-water source, sanitation, secondhand smoke, and dietary intake are each commonly captured by single survey items whose error structure is rarely characterized, and specialized surveys are the natural instrument for characterizing it."),

  p("Detecting a discrepancy is less useful than correcting it. Treating the energy surveys as higher-fidelity - though not infallible - references, the Bayesian measurement-error model raises estimated primary-LPG prevalence by roughly " + CORR_PP + " percentage points relative to raw NFHS-5 (the mean across districts, each weighted equally, rising from " + RAW19_0 + "% to " + BAY19_0 + "%), with the revision varying several-fold between states (Section 3.2); a regression calibration of the same relationship, refitted without each state, reduced held-out error in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states, which supports transporting the relationship though it does not directly validate the Bayesian surface or its credible intervals. The two eras behave differently in an interpretable way: the NFHS-4/ACCESS calibration is mild (the pre-PMUY item performed well), whereas the NFHS-5/IRES calibration implies substantial understatement (the post-PMUY item performed poorly, in the exact period when marginal adopters entered). We are explicit that this yields an exposure surface calibrated to a better-instrumented reference, not a validated measurement of true exposure; the evidence that it is closer to the truth rests on measurement-error theory, cross-validation of the regression calibration, and the benchmark falsification test, and would be strengthened by a dataset with measured household air pollution or a solid-fuel-smoke biomarker. " + stWeakTxt() + ""),

  p("The stacking results bear directly on what NFHS would need to ask. Because stacking is concentrated by place rather than by the household characteristics the survey records, and because the prediction models transferred poorly between surveys, the models we tested did not predict stacking reliably in the transfer settings we evaluated, so it cannot at present be imputed into NFHS with confidence; it has to be measured. The NFHS-4 and NFHS-5 seasonal comparisons show no statistically detectable shift in the main-fuel answer across seasons, although the intervals cannot exclude differences of a few points, so a seasonal re-interview is a lower priority than the content of the question. Three additions would address the gap at modest cost: asking which other fuels the household used for cooking in a reference period, and how often solid fuel was burned; recording LPG refills, which index sustained use rather than ownership; and addressing the cooking module to the primary cook. Harmonized questions of this kind already exist: the World Health Organization's Core Questions on Household Energy Use, developed with the World Bank for inclusion in national surveys and censuses, ask about secondary as well as primary cooking fuels so that fuel stacking is recorded [47], and adopting them in NFHS would make its fuel item comparable with surveys elsewhere. Administering such a module to a subsample of NFHS households would in addition convert the external calibration used here into an internal validation sample, which identifies how the main-fuel item errs at the household level rather than only its district-level average."),

  p("The corrected surface is an estimate, and its use in health research needs to say so. Part of its uncertainty belongs to the health survey itself - the NFHS district estimate it is applied to accounts for " + DS("var_nfhs_side_share_2015", 0) + "% of the corrected value's posterior variance on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg - and part to the calibration. An analysis that holds the surface fixed, as the fixed-surface child-mortality estimate here does, is conditional on the calibration and its interval omits calibration uncertainty. Carrying that uncertainty correctly turned out to be harder than drawing imputations: imputing from the calibration posterior alone is at risk of attenuating the association, and two implementations of the standard remedy, imputation conditional on the outcome, failed their convergence checks - the second against a prespecified stopping rule - and were abandoned (SI Methods S12). We therefore present the child-mortality association as conditional and exploratory, report the exposure-only imputation as a sensitivity analysis with its attenuation concern stated, and draw no conclusion about the health return to LPG. Two lessons generalize. Calibrated, modeled and predicted exposure surfaces - satellite and chemical-transport estimates of fine particulate matter, land-use regression and noise models, machine-learning exposure predictions, statistically downscaled climate fields - should be released with their posterior draws, so that a downstream analysis can see what holding them fixed assumes. And the method for carrying those draws into a health model needs to be chosen and checked before results are interpreted, because the obvious choice can bias the association it is meant to make honest. The same limited information bounds what can be asked of these estimates in subgroups: the models cluster on " + nClus(R_I_BAY) + " states, the region term partitions those same clusters into six groups, and any within-region contrast would rest on a handful of clusters each, so we report no regional subgroup estimates."),

  p("Everything in this analysis lives at the district: the exposure, the correction, the outcome, and the association between them. That is a consequence of the data rather than a modelling preference. NFHS and the energy surveys share no households, so the transfer function is identifiable only between district aggregates, and the corrected surface accordingly says how many households in a district the NFHS item misclassifies, not which ones. The district-level association therefore carries the standard ecological caveat, and here there is a specific reason to expect the household-level relationship to differ rather than merely to be noisier: the households the correction moves are the marginal ones - connected but stacking, or using LPG intermittently - and they are unlikely to carry either the exposure or the mortality risk of the average household in their district. Aggregation is not purely a threat. District means average away much of the household-level noise in the fuel item, and district-level supply is the scale at which the policy under study actually operates, so the district is the right unit for some questions; it is simply not a household-level dose-response. An internal validation subsample of NFHS households, as above, would permit individual-level correction by regression calibration, simulation-extrapolation or a Bayesian latent-exposure model with mortality analysed per birth."),

  p("A second asymmetry runs the other way: we correct one side of the regression and treat the other as measured. The district mortality estimates are themselves survey estimates built on modest numbers of births. The median study district contributes " + medBirths("2015") + " rural births to the ten-year window in the earlier round and " + medBirths("2019") + " in the later one, and the design-based standard error of a district infant mortality probability has a median of " + medSE100("infant", "2015") + " and " + medSE100("infant", "2019") + " deaths per 100 births in the two rounds. Differencing two such estimates roughly doubles the sampling variance while cancelling whatever is stable about a district, so the change-on-change outcome is considerably noisier than either level. Applying to the outcome the same decomposition we applied to the exposure, only about " + nfmt(100 * OSS_INF, 1) + "% of the across-district variance in the change in infant mortality, and " + nfmt(100 * OSS_NEO, 1) + "% of the variance in the change in neonatal mortality, is genuine between-district variation rather than sampling error. Classical nondifferential error in the dependent variable of a linear regression does not bias the slope; it inflates the residual variance and widens the interval. Two caveats qualify that benign reading. Error in the outcome that is correlated with error in the exposure - both are produced by the same NFHS interview in the same district - would bias the slope, and nothing in this design rules it out. And the primary district mortality estimates come from the same partial-pooling multilevel model used for the exposure, whose shrinkage suppresses sampling noise and genuine between-district contrast together, and the design-weighted direct mortality estimates are used here only to describe the outcome's sampling error, not as an alternative outcome in the health models. Birth-history recall error, omission of early neonatal deaths, and age heaping at twelve months are further sources this design does not address."),

  p("It is worth stating plainly that the health illustration may have no district-level relationship to detect. The one check that does not depend on the correction is the association between the energy surveys' own directly measured fuel use and district mortality, and it corroborates the expected directions - positive for solid fuel, negative for primary LPG - unevenly across the two eras. For solid-fuel burning that association is " + heDir(HE_A, "Any solid-fuel burning") + " in the ACCESS/NFHS-4 era and " + heDir(HE_I, "Any solid-fuel burning") + " in the IRES/NFHS-5 era (" + hePair("Any solid-fuel burning") + "); for primary-LPG prevalence it is " + heDir(HE_A, "Primary LPG") + " and " + heDir(HE_I, "Primary LPG") + " respectively (" + hePair("Primary LPG") + "). The ACCESS-era correlations rest on only " + cmpN(C4R) + " districts in six northern and eastern states, so they are weak evidence in either direction. Several considerations would make a small or absent district-level relationship unsurprising: personal exposure to fine particulate matter is only weakly proxied by the prevalence of a primary cooking fuel; the change in district prevalence over a single inter-survey interval is modest and confounded with everything else that improved between the rounds; and infant mortality has many causes, of which household air pollution is only one. These data cannot distinguish an imprecision-limited null from a substantive one. If the district-level relationship is genuinely small, the informative design is individual exposure linked to individual outcomes, or an outcome nearer the exposure than infant mortality, such as measured personal fine particulate exposure, respiratory symptoms or birthweight; roughly a third of the global burden attributed to household air pollution is estimated to act through short gestation and low birthweight [46]."),

  p("The procedure generalizes beyond cooking fuel: estimate a shared, simplified proxy at common spatial resolution in both surveys, calibrate the health-survey estimate against the specialized survey with a measurement-error model that carries its sampling uncertainty, and - where the health survey omits a construct entirely - characterize that construct in the specialized survey before attempting to transfer it. Calibration of a measured item is the more dependable core, though its magnitude rests on the reference survey; augmentation of a missing construct is more fragile, as the stacking results show, so behavioural models should not be moved across survey instruments or policy regimes without revalidation. The same design could improve exposure assessment for electrification, water quality, sanitation, secondhand smoke, and dietary intake wherever a large survey carries a crude item and a specialized survey a richer one."),

  p("These results carry implications for survey design, for exposure assessment, and for how corrected exposures are used. For survey practice, the cheapest fix lies inside the health survey: a small number of secondary-fuel, frequency and refill items asked of the primary cook, or periodic linkage to a specialized exposure survey on a matched frame, would improve exposure fidelity at low marginal cost and would measure stacking rather than leave it to be modelled. For exposure assessment, corrected and uncertainty-quantified district surfaces are a more defensible input to household-air-pollution burden estimation than a raw primary-fuel classification, provided the uncertainty travels with the surface. For epidemiological practice, analyses that adopt an externally calibrated exposure product should state that a fixed-surface result is conditional on the calibration, and should check, rather than assume, that the method used to carry calibration uncertainty does not bias the association. For clean-cooking policy, the finding that bears on monitoring is a measurement one: a single primary-fuel item stopped tracking household fuel use in exactly the period when the policy was doing most of its work, so stacking, rather than connection counts or a main-fuel classification, is what the transition needs to be monitored on. Most broadly, the study argues for treating a survey exposure item not as the exposure itself but as one noisy measurement to be calibrated against better instruments - and for treating the calibrated result, in its turn, as an estimate rather than a measurement."),

  h3("4.1 Limitations"),
  p("Several limitations bound these conclusions. First, the energy surveys are higher-fidelity references, not infallible gold standards: they have their own sampling designs, recall limitations, and non-response (IRES non-response averaged 26% and was higher among wealthier households, and neither survey's weights carry a non-response adjustment). We therefore frame them as better-measured references rather than truth; we note that wealthier-household non-response in IRES, given that LPG use rises with wealth, would if anything bias IRES prevalence downward, which on its own would make the observed NFHS-IRES gap conservative; against that, the independent NSSO survey places rural primary-LPG use between NFHS-5 and IRES, closing only part of the gap, so the rural magnitude of the IRES-anchored correction may be too large (SI Methods S8). Second, and most important for the correction, we have no independent validation that the corrected exposure is closer to the true household exposure; the case is theoretical and internal, and a dataset with measured household air pollution or a biomarker of solid-fuel smoke would be required to establish it. Third, we characterize but do not prove the mechanism of the NFHS-IRES discrepancy: differential questionnaire design is a leading candidate, but interviewer effects, item wording, the identity of the respondent (household members answer energy questions differently [33]; IRES, like NFHS, interviews the household head or another adult, whereas the 2018 ACCESS round asked the primary cook to be present for the cooking section), survey mode, and differential non-response could each contribute, and our own ration-card benchmark shows that even a non-fuel item is sensitive to how it is asked. Relatedly, the before-and-after contrast compares NFHS with two different reference surveys, so it cannot by itself separate a change in how the NFHS item behaves from a difference between ACCESS and IRES in how they elicit primary-fuel use; that both pairs agree closely on caste and religion, and that the independent NSSO survey also places LPG use above NFHS-5, bear on this but do not settle it. Fourth, the health analysis is limited in what it can claim. It is ecological, using district change-on-change with a rural exposure against rural-birth mortality across " + N_RURAL + " districts; it is associational rather than causal; its fixed-surface estimate is conditional on the fitted calibration and its interval excludes calibration uncertainty; the exposure-only imputation that includes that uncertainty is at risk of attenuation;" + (CR2_OVER ? " with " + nClus(R_I_BAY) + " state clusters even the small-cluster CR2 rule rejected a true null in " + calibRate(1) + "% of simulations rather than 5%;" : "") + " and the two methods that would have addressed the attenuation failed their convergence checks and were abandoned, the second under a prespecified stopping rule (SI Methods S12). We therefore treat the health analysis as exploratory and not as an estimate of the health return to LPG. The predicted fuel-composition metrics (LPG-no-solid-reported share, stacking, consumption) do not yield coherent health associations and are reported only as exploratory (SI Table S8, SI Figure S10). Fifth, the two national corrected surfaces rest on very different amounts of geographic support, and the NFHS-4 surface should be read accordingly. Only " + SUPP15 + " of the " + N_D_NFHS4 + " NFHS-4 districts we correct fall inside one of the six ACCESS states, so roughly two-thirds of the corrected NFHS-4 surface is an extrapolation of a calibration relationship estimated in six poor northern and eastern states to states where it was never observed; by contrast, " + SUPP19 + " of the " + N_D_NFHS5 + " NFHS-5 districts fall inside an IRES state, so the corrected NFHS-5 surface is very largely an interpolation within the range on which it was fit. We therefore treat the corrected NFHS-5/IRES surface as the defensible national product and read the corrected NFHS-4 surface outside the six ACCESS states as illustrative extrapolation; the ACCESS calibration's principal value is that it provides an era-matched check in the one region where such a reference exists; it shares the NFHS-5/IRES calibration's logit-scale slope below 1 but implies only a mild, mostly downward correction. Sixth, the stacking analyses rest on the energy surveys' own fuel-use items, which record whether a fuel was used rather than how much, and reported use of both LPG and solid fuels does not establish the frequency or intensity of stacking; the state-level validation draws on " + (SSV_LOSO.n_states || "NA") + " IRES states and six ACCESS states, and the variance shares are on the latent logistic scale. Seventh, the adult outcomes reported in the Supplementary Materials use hypertension and diabetes classifications that were verified against the raw recodes, but the hypertension composite is missing for " + htnMissPct("2015") + "% of respondents in the NFHS-4 frame and " + htnMissPct("2019") + "% in the NFHS-5 frame; if that missingness is related to hypertension risk and changed between rounds, the district changes in prevalence are subject to selection."),
];

const refs = [
  "World Health Organization. Household air pollution and health. Fact sheet, 16 December 2025. Geneva: WHO; 2025. https://www.who.int/news-room/fact-sheets/detail/household-air-pollution-and-health (accessed October 2026)",
  "Pandey A, Brauer M, Cropper ML, et al. Health and economic impact of air pollution in the states of India: the Global Burden of Disease Study 2019. Lancet Planet Health. 2021;5(1):e25-e38.",
  "Balakrishnan K, Ghosh S, Ganguli B, et al. State and national household concentrations of PM2.5 from solid cookfuel use: results from measurements and modeling in India for estimation of the global burden of disease. Environ Health. 2013;12:77.",
  "Chowdhury S, Dey S, Guttikunda S, et al. Indian annual ambient air quality standard is achievable by completely mitigating emissions from household sources. Proc Natl Acad Sci USA. 2019;116(22):10711-10716.",
  "Ministry of Petroleum and Natural Gas, Government of India. Pradhan Mantri Ujjwala Yojana. https://www.pmuy.gov.in/about.html (accessed July 2026).",
  "International Institute for Population Sciences (IIPS) and ICF. National Family Health Survey (NFHS-5), 2019-21: India. Mumbai: IIPS; 2021.",
  "National Statistical Office. Multiple Indicator Survey in India, NSS 78th Round (2020-21). New Delhi: MoSPI; 2023.",
  "Gould CF, Hou X, Richmond J, Sharma A, Urpelainen J. Jointly modeling the adoption and use of clean cooking fuels in rural India. Environ Res Commun. 2020;2:085004.",
  "Mani S, Jain A, Tripathi S, Gould CF. The drivers of sustained use of liquified petroleum gas in India. Nat Energy. 2020;5:450-457.",
  "Johnson MA, Chiang RA. Quantitative guidance for stove usage and performance to achieve health and environmental targets. Environ Health Perspect. 2015;123(8):820-826.",
  "World Health Organization. WHO guidelines for indoor air quality: household fuel combustion. Geneva: WHO; 2014.",
  "Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM. Measurement Error in Nonlinear Models: A Modern Perspective. 2nd ed. Boca Raton: Chapman & Hall/CRC; 2006.",
  "Jain A, Ray S, Ganesan K, Aklin M, Cheng C-Y, Urpelainen J. Access to Clean Cooking Energy and Electricity: Survey of States. New Delhi: Council on Energy, Environment and Water; 2015.",
  "Pelz S, Chindarkar N, Urpelainen J. Energy access for marginalized communities: evidence from rural North India, 2015-2018. World Dev. 2021;137:105204.",
  "Agrawal S, Mani S, Jain A, Ganesan K. State of Electricity Access in India: Insights from the India Residential Energy Survey (IRES) 2020. New Delhi: Council on Energy, Environment and Water; 2020.",
  "International Institute for Population Sciences (IIPS) and ICF. National Family Health Survey (NFHS-4), 2015-16: India. Mumbai: IIPS; 2017.",
  "ICF. Demographic and Health Surveys Sampling and Household Listing Manual. Rockville, MD: ICF; 2012.",
  "Bates D, Maechler M, Bolker B, Walker S. Fitting linear mixed-effects models using lme4. J Stat Softw. 2015;67(1):1-48.",
  "Buerkner P-C. brms: An R package for Bayesian multilevel models using Stan. J Stat Softw. 2017;80(1):1-28.",
  "R Core Team. R: A Language and Environment for Statistical Computing. Vienna: R Foundation for Statistical Computing; 2024.",
  "Rabe-Hesketh S, Skrondal A. Multilevel modelling of complex survey data. J R Stat Soc A. 2006;169(4):805-827.",
  "Carle AC. Fitting multilevel models in complex survey data with design weights: recommendations. BMC Med Res Methodol. 2009;9:49.",
  "Agrawal S, Mani S, Jain A, Ganesan K, Urpelainen J. India Residential Energy Survey (IRES) 2020: Design and data quality. Technical Document. New Delhi: Council on Energy, Environment and Water; 2020.",
  "Lumley T. Analysis of complex survey samples. J Stat Softw. 2004;9(8):1-19.",
  "Korn EL, Graubard BI. Analysis of Health Surveys. New York: Wiley; 1999.",
  "Keogh RH, White IR. A toolkit for measurement error correction, with a focus on nutritional epidemiology. Stat Med. 2014;33(12):2137-2155.",
  "van Donkelaar A, Hammer MS, Bindle L, et al. Monthly global estimates of fine particulate matter and their uncertainty. Environ Sci Technol. 2021;55(22):15287-15300. (Atmospheric Composition Analysis Group, V5.GL.03 product).",
  "Abatzoglou JT, Dobrowski SZ, Parks SA, Hegewisch KC. TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015. Sci Data. 2018;5:170191.",
  "deSouza P, Lee JJ, Nemeth J, et al. Evaluating associations between the transition to cleaner cooking energy use and hypertension in India. Environ Res Health. 2025;3:045008.",
  "Rehman IH, Kar A, Banerjee M, et al. Understanding the political economy and key drivers of energy access in addressing national energy access priorities and policies: India. Energy Policy. 2012;47(Suppl 1):27-37.",
  "Kar A, Zerriffi H. From cookstove acquisition to cooking transition: framing the behavioural aspects of cookstove interventions. Energy Res Soc Sci. 2018;42:23-33.",
  "Gould CF, Jha S, Patnaik S, et al. Variability in the household use of cooking fuels: the importance of dishes cooked, non-cooking end uses, and seasonality in understanding fuel stacking in rural and urban slum communities in six north Indian states. World Dev. 2022;159:106051.",
  "Zhang AT, Patnaik S, Jha S, et al. Evidence of multidimensional gender inequality in energy services from a large-scale household survey in India. Nat Energy. 2022;7:698-707.",
  "Gould CF, Pillarisetti A, Thompson LM, et al. Using high-frequency household surveys to describe energy use in rural North India during the COVID-19 pandemic. Nat Energy. 2023;8:169-178.",
  "Gill-Wiehl A, Gould CF, Jeuland M, et al. Beyond access: clean energy use in low-income and middle-income countries. Lancet Glob Health. 2026;14:e598-e611.",
  "Gill-Wiehl A, Brown T, Smith K. The need to prioritize consumption: a difference-in-differences approach to analyze the total effect of India's below-the-poverty-line policies on LPG use. Energy Policy. 2022;164:112915.",
  "Kumar R. Validity of energy ladder hypothesis through types of residence: an ordered probit analysis for Indian households. Indian Econ J. 2026;74(1):168-184.",
  "Pillarisetti A, Daouda M, Gould CF, et al. Household energy use and health in low-income and middle-income countries. Lancet Glob Health. 2026;14:e612-e625.",
  "Habib G, Kumari J, Khan M, et al. Estimating shifts in fuel stacking among solid biomass fuels and liquefied petroleum gas in rural households: a pan-India analysis. Research Square; 2023 (preprint, not peer-reviewed).",
  "deSouza PN, Dey S, Mwenda KM, Kim R, Subramanian SV, Kinney P. Robust relationship between ambient air pollution and infant mortality in India. Sci Total Environ. 2022;815:152755.",
  "deSouza PN, Chaudhary E, Dey S, et al. An environmental justice analysis of air pollution in India. Sci Rep. 2023;13:16690.",
  "deSouza P, Lee J, Longkumer I, et al. Associations between the transition to cleaner cooking energy use and child health outcomes in India. SSRN; 2025 (preprint, not peer-reviewed). https://ssrn.com/abstract=6129466",
  MENG_REF,
  "Jain A, Tripathi S, Mani S, Patnaik S, Shahidi T, Ganesan K. Access to Clean Cooking Energy and Electricity: Survey of States 2018. New Delhi: Council on Energy, Environment and Water; 2018.",
  "Aklin M, Cheng C-Y, Urpelainen J, Ganesan K, Jain A. Factors affecting household satisfaction with electricity supply in rural India. Nat Energy. 2016;1:16170.",
  "Bennitt FB, Wozniak S, Causey K, et al. Global, regional, and national burden of household air pollution, 1990-2021: a systematic analysis for the Global Burden of Disease Study 2021. Lancet. 2025;405(10485):1167-1181.",
  "World Health Organization. Core questions on household energy use. Clean Household Energy Solutions Toolkit (CHEST). Geneva: WHO. https://www.who.int/tools/core-questions-for-household-energy-use (accessed October 2026).",
];

// The Meng marker in Section 2.4.3 is a literal number in prose. This makes any
// drift between that number and the reference list a build-time failure rather
// than a wrong citation in a submitted manuscript.
if (refs[MENG_NO - 1] !== MENG_REF) {
  throw new Error("reference numbering drift: [" + MENG_NO + "] is not the Meng (1994) entry (refs has " + refs.length + " entries)");
}
{
  const perm = REF_ORDER.slice().sort((a, b) => a - b);
  if (perm.length !== refs.length || perm.some((v, i) => v !== i + 1))
    throw new Error("REF_ORDER is not a permutation of the " + refs.length + " references");
}
console.log("[build] reference [" + REF_NEW[MENG_NO] + "] = Meng (1994) after renumbering by first citation; cited once, Section 2.4.3 estimand paragraph; refs has " + refs.length + " entries");

// NFHS variable inventory. Names and code values are taken from 01_prep_nfhs.R,
// which builds every NFHS quantity used in the paper, so this list and the
// analysis code cannot drift apart silently.
const suppNfhs = [
  "Type of cooking fuel hv226 (carried as fuel in the analysis extract): 1 Electricity, 2 LPG/natural gas (labelled LPG in NFHS-5, whose separate natural-gas code is unused), 4 Biogas, 5 Kerosene, 6 Coal or lignite, 7 Charcoal, 8 Wood, 9 Straw, shrubs or grass, 10 Agricultural crop, 11 Animal dung, 95 No food cooked in the household, 96 Other. The primary-LPG indicator analysed throughout is code 2.",
  "Urban or rural residence (1: Urban, 2: Rural) hv025; rural households are hv025 = 2",
  "State hv024; district DHSREGCO from the DHS geographic dataset, merged to the household file on cluster hv001",
  "Number of household members hv012",
  "Household electricity (0: No, 1: Yes; any other code set to missing) hv206",
  "Wealth: the continuous wealth-index factor score hv271, with the quintile hv270 as fallback. Converted to within-state quintiles so that it aligns with the within-state monthly-expenditure quintiles used in ACCESS and IRES, and is therefore interpretable as relative economic rank within a state rather than absolute wealth.",
  "Below-poverty-line or Antyodaya ration card: sh58 in NFHS-4 and sh75 in NFHS-5 (code 8 set to missing)",
  "Caste of the household head (1: Scheduled Caste, 2: Scheduled Tribe, 3: Other Backward Class, 4: General): sh36 in NFHS-4 and sh49 in NFHS-5.",
  "Religion of the household head (1: Hindu, 2: Muslim, remaining codes: Other) hh_religion. Missing and unknown values are kept as missing rather than absorbed into the Other category.",
  "Bank account (code 8 set to missing) hv247",
  "Survey design: household weight hv005 divided by 1,000,000; primary sampling unit hv021; sampling stratum hv022",
];

const suppAccessEnergy = [
  "Use grid electricity (0: No, 1: Yes) m2_q55_grid",
  "LPG for cooking (0: No, 1: Yes) m4_q103_lpg", "Firewood for cooking (0: No, 1: Yes) m4_q109_firewood",
  "Dung cake for cooking (0: No, 1: Yes) m4_q113_dungcake", "Agricultural residue for cooking (0: No, 1: Yes) m4_q114_agro",
  "Main cooking fuel (1: Firewood and Chips, 2: Dung cakes, 3: LPG, 4: Others) m5_q118_main_cookfuel",
];
const suppAccess = [
  "Age m1_q19_age", "Gender (0: Female, 1: Male) m1_q20_gender", "Religion (Hindu, Muslim, Others) m1_q24_religion",
  "Education (1: No formal schooling, 2: Up to 5th standard, 3: Up to 10th standard, 4: 12th standard or diploma, 5: Graduate and above) m1_q23_edu",
  "Caste (1: Scheduled Caste, 2: Scheduled Tribe, 3: Other Backward Class, 4: General) m1_q25_caste",
  "Ration card (0: None, 1: APL, 2: BPL, 3: Antyodaya) m1_q26_ration", "Adults in household m1_q27_no_adults",
  "Number of children in household m1_q29_no_children", "Monthly expenditure m1_q32_month_expenditure",
  "Bank account (0: No, 1: Yes) m1_q34_bank_acc",
  "LPG cylinders: large (m4_q103_4_lpg_lcyl) and small (m4_q103_7_lpg_scyl) cylinder use; large cylinders bought from the distributor or market (m4_q103_5_lpg_lcyl_dist, m4_q103_6_lpg_lcyl_mkt); small cylinders bought from the distributor or market (m4_q103_8_lpg_scyl_dist, m4_q103_9_lpg_scyl_mkt)",
  "LPG cylinder prices, large and small, from the distributor and the market (m4_q103_10_lpg_lcyl_dist_price, m4_q103_11_lpg_scyl_dist_price, m4_q103_12_lpg_lcyl_mkt_price, m4_q103_13_lpg_scyl_mkt_price)",
];

// IRES variable inventory. Names and code values are taken from 03_prep_ires.R,
// which is the script that constructs every IRES quantity used in the paper, so
// this list and the analysis code cannot drift apart silently.
const suppIres = [
  "Primary cooking fuel (1: LPG, 2: PNG, 3: Electricity, 4: Firewood, 5: Agricultural residue, 6: Dung cake, 7: Coal or charcoal, 8: Kerosene, 9: Biogas, 10: Other) q529_prim_cook_fuel. The primary-LPG indicator analysed throughout is code 1 only: PNG is a separate code and is not counted as LPG, matching the NFHS and ACCESS definitions. The clean-primary-fuel variant used in sensitivity analysis is codes 1, 2, 3 and 9.",
  "Household uses LPG at all (1: Yes) q502_lpg_use_yn",
  "LPG meets all cooking needs (1: Yes) q514_lpg_use_all_needs_yn. This direct exclusivity item was judged unreliable in the metadata review and is retained for inspection only; the analytic categories are derived from the fuel-by-fuel use items below.",
  "Any solid fuel used for cooking (1: Yes) q514_a_solidfuel_yn",
  "Fuel-by-fuel cooking use (1: Yes): firewood q515_firewood_cook_use_yn; dung cake q519_dung_cook_use_yn; agricultural residue q523_agro_cook_use_yn; coal or charcoal q523_coal_cook_use_yn; kerosene q523_kero_cook_use_yn",
  "LPG cylinder type (1: Large, 2: Small, 3: Both) q505_lpg_large_small_both; large (14.2 kg) refills per year q505_1_lpg_large_n; small refills per year q506_lpg_small_n; weight of the small cylinder in kg q506_1_lpg_small_kg; amount paid at the last refill q508_lpg_last_refill_pay",
  "PMUY beneficiary (99 recoded to missing) q504_lpg_pmuy_yn",
  "Grid electricity supply (1: Yes) q301_grid_yn",
  "Survey type (1: Rural, 2: Urban) q103_survey_type",
  "Education of the primary income earner q208_priminc_earner_edu",
  "Monthly household expenditure (Rs) q234_month_exp",
  "Ration card q212_ration_card; caste q211_caste; religion q210_religion; number of household members q213_no_members; respondent age q202_resp_age; respondent gender q201_resp_gender",
];

// ============================ ASSEMBLE (MAIN TEXT) ===========================
const TITLE = "Measuring clean cooking through India's LPG transition: survey disagreement, calibration and fuel stacking in the National Family Health Survey";
const mainChildren = [
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120 },
    children: [r(TITLE, { bold: true, size: 30 })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240 },
    children: [r("Supplementary Materials are provided in a companion document.", { italics: true, size: 22 })] }),

  h1("Abstract"),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Background: ", { bold: true }),
    r("The Demographic and Health Surveys, designed to measure maternal and child health, are among the few nationally representative data sources in low- and middle-income countries, and are widely repurposed for environmental exposure assessment. Studies of household air pollution in India derive cooking-related exposure from a single National Family Health Survey (NFHS) primary-fuel item that is blind to fuel stacking. Whether such an item remains comparable across a period in which policy changes which households own which fuels, whether specialized energy surveys can correct it, and what they reveal about the stacking it cannot record, has not been established.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Objectives: ", { bold: true }),
    r("We asked whether the NFHS cooking-fuel item retained a stable relationship to household fuel use across India's clean-cooking transition, whether specialized energy surveys can calibrate the resulting error, and whether fuel stacking can be recovered from the household characteristics NFHS does record.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Methods: ", { bold: true }),
    r("We estimated district-level primary-liquefied petroleum gas (LPG) prevalence in NFHS-4 (2015-16), NFHS-5 (2019-21), the ACCESS survey (2014-15), and the India Residential Energy Survey (IRES, 2019-20) with a common multilevel small-area model, and compared them on like-for-like design-weighted rural estimates alongside demographic characteristics measured comparably in both. Treating the energy surveys as higher-fidelity references, we corrected the NFHS estimates by regression calibration and a Bayesian measurement-error model, assessed the transportability of the calibration relationship by leave-one-state-out cross-validation of the regression calibration, and decomposed the corrected exposure's posterior variance into calibration-side and health-survey-side components. In the energy surveys we partitioned the variation in fuel stacking between place and household characteristics, tested whether those characteristics predict stacking in held-out states and across survey instruments, and tested whether the NFHS main-fuel answer shifts with season. As an exploratory illustration we estimated district change-on-change associations between primary-LPG prevalence and child mortality, holding the corrected exposure fixed and, as a sensitivity analysis, carrying calibration uncertainty by exposure-only multiple imputation.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Results: ", { bold: true }),
    r("NFHS-4 and ACCESS agreed closely for rural districts before the policy expansion (mean difference under one percentage point), whereas NFHS-5 afterwards recorded roughly " + GAP_DW + " percentage points less primary LPG use than IRES on like-for-like design-weighted rural estimates. The same survey pairs agreed closely on the demographic benchmarks compared, and neither field timing, season, weighting, nor rural-urban composition accounted for the difference, consistent with a fuel-specific measurement explanation. Calibration materially changed the exposure surface (the mean of district rural primary-LPG prevalence, each district weighted equally, rose from " + RAW19_0 + "% to " + BAY19_0 + "%), and a regression calibration estimated without each state reduced held-out error in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states; " + DS("var_nfhs_side_share_2015", 0) + "% (2015) and " + DS("var_nfhs_side_share_2019", 0) + "% (2019) of the corrected value's posterior variance came from the NFHS district estimate itself. Between " + stackShareRange() + "% of primary-LPG households in the energy surveys also burned a solid fuel. Place - state, district and village - accounted for up to " + nfmt(Math.max(...vpTab.map((x) => Number(x.pct_place_total)).filter(isFinite)), 0) + "% of the latent-scale variation in stacking, household characteristics for " + vpRange("marginal_r2_household_pct", 0) + "%; a household model predicted held-out IRES states " + (Number(SSV_LOSO.skill_vs_benchmark) <= 0 ? "no better than" : "only modestly better than") + " assigning every state the overall share (state-level r = " + nfmt(SSV_LOSO.r_state, 2) + ") and transferred poorly between surveys (district r between " + nfmt(Math.min(TR_E, TR_I, TR_B), 2) + " and " + nfmt(Math.max(TR_E, TR_I, TR_B), 2) + " across era and instrument changes), and " + seasMain() + ". In the exploratory health analysis the fixed-surface infant association was " + est10(R_I_BAY) + " deaths per 100 births per 10-percentage-point rise in primary-LPG prevalence (95% CI " + ci10cr2(R_I_BAY) + ", small-cluster corrected; negative values mean fewer deaths), an estimate conditional on the fitted calibration whose interval excludes calibration uncertainty; the exposure-only imputation, which is at risk of attenuation, gave " + est10(R_I_MI) + " (95% CI " + ci10cr2(R_I_MI) + "). Two implementations of outcome-conditioned imputation failed their convergence checks (the second against a prespecified stopping rule) and were abandoned."), ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Discussion: ", { bold: true }),
    r("A single survey item can lose comparability when policy changes the population it is asked of, and a specialized survey can detect that, point to the question as its likeliest source by comparison with comparably measured characteristics, and calibrate the resulting level bias. Fuel stacking, which the item cannot record, varies with place far more than with the household characteristics the survey records, and the models we tested did not predict it reliably in the transfer settings we evaluated; measuring it requires secondary-fuel, frequency and refill questions addressed to the primary cook. Health analyses that adopt a calibrated exposure surface inherit uncertainty from both the calibration and the health survey, and the child-mortality association here remains conditional and exploratory."), ] }),
  new Paragraph({ spacing: { after: 200 }, children: [
    r("Keywords: ", { bold: true, italics: true }),
    r("exposure assessment; measurement error; survey validation; calibration; fuel stacking; clean cooking; household air pollution; small-area estimation; India", { italics: true }) ] }),

  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 60, after: 60 },
    children: [new ImageRun({ type: "png",
      data: fs.readFileSync(FIG + "fig_overview.png"),
      transformation: { width: 415, height: 537 } })] }),
  caption("Graphical overview. The national health survey (NFHS) and two specialized household energy surveys (ACCESS, IRES) are estimated on a common district frame and compared, era by era, on primary LPG and on benchmark items. The comparison motivates a measurement-error calibration of the NFHS estimate, which yields a calibrated district surface of rural primary-LPG prevalence with its uncertainty apportioned between the calibration and the health survey. Fuel stacking is characterized directly in the energy surveys; models predicting it from NFHS covariates did not transfer reliably. The calibrated surface is carried into an exploratory, conditional health illustration. The same procedure applies to other exposures captured by single survey items."),

  h1("1. Introduction"), ...intro,

  h1("2. Data and Methods"),
  h2("2.1 Study design overview"), ...methodsOverview,
  h2("2.2 Data sources"),
  h3("2.2.1 National Family Health Surveys (NFHS)"), ...nfhsText,
  h3("2.2.2 Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS)"), ...accessText,
  h3("2.2.3 India Residential Energy Survey (IRES)"), ...iresText,
  h3("2.2.4 NSSO Multiple Indicator Survey (NSS 78th round)"), ...nssoDataText,
  h2("2.3 Cooking-fuel exposure indicators and harmonized covariates"), ...measuresText,
  h2("2.4 Statistical analysis"),
  h3("2.4.1 Deriving district-level estimates of exposure"), ...statText,
  h3("2.4.2 Cross-survey comparison"), ...compareText,
  h3("2.4.3 Correcting the rural NFHS estimates (measurement-error models)"), ...correctionText,
  h3("2.4.4 Fuel stacking: prediction, place and season"), ...predictText,

  h1("3. Results"), ...results,

  h1("4. Discussion"), ...discussion,

  h1("References"),
  ...REF_ORDER.map((o) => refs[o - 1]).map((t, i) => p(`${i + 1}. ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 80 } }, run: { size: 22 } })),
];

const transferSI = [
  p("The secondary stacking analysis reports that a model trained in IRES (2019-20) has only a weak association with observed 2015 ACCESS district stacking (r = " + CROSS_ERA_R + "). That comparison is uninformative about mechanism, because it changes two things at once: the calendar period, and the survey instrument that measured the outcome. A model can fail to transfer because the behaviour it describes genuinely changed between the pre- and post-PMUY periods, or because the two surveys operationalize fuel stacking differently enough that the quantity being predicted is not the same quantity - and the two have opposite implications. The first is a substantive finding about the clean-cooking transition; the second is a further instance of the measurement problem this paper is about."),
  p("We separate them with three contrasts, two of which vary one factor at a time. ACCESS was fielded twice, in 2014-15 (Wave 1) and 2018 (Wave 2), with the same instrument, so training in Wave 1 and testing in Wave 2 changes the ERA with the instrument held fixed. IRES (2019-20) and ACCESS Wave 2 (2018) are one to two years apart, so training in IRES and testing in Wave 2 changes the INSTRUMENT with the era held approximately fixed. Training in IRES and testing in ACCESS Wave 1 changes {{both}}; it is the six-state counterpart of the cross-era test reported in SI Methods S2 and S7, which trains on all IRES states (r = " + CROSS_ERA_R + "; the 'unrestricted IRES' rows of Table S12). All three are estimated on the six states ACCESS and IRES share (Bihar, Jharkhand, Madhya Pradesh, Odisha, Uttar Pradesh, and West Bengal), on rural households only, and with the district-context covariate removed from the predictor set so that no era-specific quantity enters the model; IRES is additionally restricted to its rural subsample, ACCESS being rural by design. Performance is summarized at the district level, as the correlation between the predicted and the observed district share of stacking households, because that is the level at which the predictions are used."),
  p([
    r("Holding the instrument fixed and changing only the era gives a district-level correlation of "),
    r(trR("ERA"), { bold: true }),
    r(" (ACCESS Wave 1 to Wave 2; AUC " + nfmt(ERA.auc) + ", calibration slope " + nfmt(ERA.cal_slope) + "). Holding the era approximately fixed and changing only the instrument gives "),
    r(trR("INSTRUMENT"), { bold: true }),
    r(" (IRES to ACCESS Wave 2; AUC " + nfmt(INST.auc) + ", calibration slope " + nfmt(INST.cal_slope) + "). Changing both gives "),
    r(trR("ERA+INSTRUMENT"), { bold: true }),
    r(" (IRES to ACCESS Wave 1; AUC " + nfmt(BOTH.auc) + ", calibration slope " + nfmt(BOTH.cal_slope) + "). For reference, the same models evaluated in the data they were trained on, with their fitted district effects, reach district correlations of " +
      nfmt(trPred.filter((x) => x.kind === "in-sample (reference only)").map((x) => x.district_r)[0]) +
      " to " +
      nfmt(trPred.filter((x) => x.kind === "in-sample (reference only)").map((x) => x.district_r).slice(-1)[0]) +
      ", so the degradation is not a ceiling imposed by the outcome's own predictability. Read together, " + trWeakTxt() + ". What they do show is that none of the three transfers retains much of the within-survey signal."),
  ]),
  p("Two further caveats apply. The instrument contrast is not era-pure: ACCESS Wave 2 (2018) and IRES (2019-20) are one to two years apart during a period of continuing rapid change, so it mixes residual era drift with the change of instrument. And ACCESS is a panel, so the Wave 1 and Wave 2 coefficient estimates share households; the two-sample z tests below ignore any covariance between them, so the era contrast's comparisons are approximate, and their direction of error depends on the sign of that covariance, which is not estimated."),
  p([
    r("Restricting the model to the covariates that do transfer does not rescue it. Taking the predictors whose coefficients are statistically indistinguishable across the instrument contrast, keep their sign and differ by less than half the larger coefficient (" + trStable("INSTRUMENT") + ") and refitting on the subset retained from them (" + SUB_NVAR + " predictors: " + SUB_VARS + " - those among the instrument-stable variables whose removal does not by itself improve transfer), the IRES-trained model reaches district correlations of " +
      nfmt(trPred.filter((x) => x.kind === "transferable subset").map((x) => x.district_r)[0]) +
      " and " +
      nfmt(trPred.filter((x) => x.kind === "transferable subset").map((x) => x.district_r)[1]) +
      " against ACCESS Wave 1 and Wave 2, against " + nfmt(trPred.filter((x) => x.kind === "ERA+INSTRUMENT").map((x) => x.district_r)[0]) + " and " + nfmt(trPred.filter((x) => x.kind === "INSTRUMENT").map((x) => x.district_r)[0]) + " for the full model" + subsetTxt() + " "),
    r("Predictors stable across the era contrast are " + trStable("ERA (") + "; across the instrument contrast, " + trStable("INSTRUMENT") + "; and across the combined era-and-instrument contrast, only " + trStable("ERA+INSTRUMENT") + "."),
  ]),
  p("The covariate distributions themselves move in a way that explains part, but not all, of this. Comparing ACCESS Wave 1, ACCESS Wave 2, and rural IRES in the common states, the share of households holding a below-poverty-line or Antyodaya ration card rises monotonically (" + shiftTriple("bpl_bin", "mean") + "), mean household size falls monotonically (" + shiftTriple("hhsize_c", "mean") + "), and household electrification rises (" + shiftTriple("elec_bin", "mean") + "). Two shifts, however, are harder to read as population change. The Other Backward Class share does not move monotonically - it rises across the era contrast and then falls across the instrument contrast (" + shiftTriple("caste3", "Other Backward Class") + ") - and the ration-card indicator moves further across the instrument contrast than across the era contrast (" + shiftDelta("bpl_bin", "mean", "shift_instrument") + " versus " + shiftDelta("bpl_bin", "mean", "shift_era") + "), even though the instrument contrast spans the shorter interval (covariate means in Table S13). A covariate that shifts smoothly with time is consistent with population change; one that reverses, or that moves most where the instrument changes rather than where the years do, is more consistent with the two surveys classifying the same households differently, which is the pattern a measurement explanation predicts. Household-head education is not examined here: it is excluded from the prediction models because it is not populated in the NFHS extracts to which they are applied (Table S1)."),
  p("Finally, the contribution of each predictor to out-of-sample transfer, measured by refitting with that predictor dropped, is reported in Table S14. It leaves one tension unresolved: religion is simultaneously the least coefficient-stable term in the model (its 'Other' category carries the single largest cross-survey difference, and the only one significant at the 5% level on the instrument contrast; Table S15) and the largest positive contributor to transfer against Wave 1, its removal costing " + nfmt(looTop.contrib_W1, 2) + " of district-level correlation there" + (Number(looTop.contrib_W2) < 0 ? " while improving transfer to Wave 2 by " + nfmt(-Number(looTop.contrib_W2), 2) : " and " + nfmt(looTop.contrib_W2, 2) + " against Wave 2") + ". Several predictors are actively harmful to transfer - dropping " + orList(looHarmfulList) + " improves it. We do not read the drop-one ordering as a causal decomposition; with correlations this low the ranking is unstable, and we report it to document that no reweighting of this covariate set produces a transportable model, not to identify which covariate to keep."),
  p("The practical consequence for this paper is contained. The transfer diagnostic concerns the secondary augmentation analysis - predicting fuel-use detail that NFHS does not collect - and not the primary correction, which calibrates a measure both surveys do collect against a contemporaneous reference and is estimated within era-matched pairs throughout. The diagnostic is nevertheless the reason the augmentation models are trained separately by era and reported as exploratory: a behavioural model moved across survey instruments or eras may not be measuring the same construct, and this diagnostic cannot say which of the two changes matters more."),
];

// Tables S12-S15, all built row-by-row from the analysis CSVs.
const tableS12 = mkTable([2100, 2100, 1500, 800, 900, 900, 1000], [
  ["Train", "Test", "Contrast", "N hh", "AUC", "Cal. slope", "District r"],
  ...trPred.map((x) => [x.train, x.test, x.kind, x.n, nfmt(x.auc, 3),
                        nfmt(x.cal_slope, 3), nfmt(x.district_r, 3)]),
]);
const tableS13 = mkTable([2600, 1900, 1500, 1500, 1800], [
  ["Predictor", "Level", "ACCESS W1 (2015)", "ACCESS W2 (2018)", "IRES rural (2019-20)"],
  ...trShift.map((x) => [x.variable, x.level, nfmt(x.ACCESS_W1_2015, 3),
                         nfmt(x.ACCESS_W2_2018, 3), nfmt(x.IRES_rural_common, 3)]),
]);
const tableS14 = mkTable([2400, 2600, 1400, 1400, 1500], [
  ["Predictor dropped", "District r without it (W1 / W2)", "AUC W1", "AUC W2",
   "Contribution (W1 / W2)"],
  ...trLoo.map((x) => [x.dropped,
                       nfmt(x.district_r_W1, 3) + " / " + nfmt(x.district_r_W2, 3),
                       nfmt(x.auc_W1, 3), nfmt(x.auc_W2, 3),
                       nfmt(x.contrib_W1, 3) + " / " + nfmt(x.contrib_W2, 3)]),
]);
const tableS15 = mkTable([2000, 2000, 1200, 1200, 1200, 900, 900], [
  ["Contrast", "Term", "Est. (survey A)", "Est. (survey B)", "Difference", "z", "p"],
  ...trCoef.map((x) => [x.contrast, x.term, nfmt(x.e1, 3), nfmt(x.e2, 3),
                        nfmt(x.diff, 3), nfmt(x.z, 2), pfmt(x.p)]),
]);

// heCorr, heR and hePair (the SI Figure S11 correlations) are defined near the
// design accessors above, because the main-text Discussion now quotes the same
// correlations and const declarations are not hoisted. They are NOT redefined
// here; this comment marks where they used to live.

// ============================ ASSEMBLE (SUPPLEMENTARY) =======================
// ============ HEALTH-IMPUTATION FAILURE RECORD (SI Section S12) ===============
// The two outcome-conditioned imputation methods failed their convergence
// checks and were abandoned (docs/mime_v2_spec.md, Sections 5, 8, 9). Their
// record lives in the REPOSITORY, under records/health_imputation/, and is
// copied in beside the builder by build_docx.R. It is read here explicitly and
// only for this documentation section: no estimate from either method enters
// the text, and nothing is read from the archived output folder.
const REC = "records/health_imputation/";
const recV1   = requireCsv(REC + "v1_convergence_record.csv", "the first imputation version's convergence record");
const recSum  = requireCsv(REC + "v2_stageA_summary.csv", "the Stage A convergence table (Table S16)");
const recVer  = requireCsv(REC + "v2_stageA_verdict.csv", "the Stage A verdict");
const recCell = requireCsv(REC + "v2_stageA_cells.csv", "the Stage A per-cell counts (Table S16a)");
const recTune = requireCsv(REC + "v2_tuning_log.csv", "the tuning log (Table S16c)");
const recAmd  = requireCsv(REC + "v2_spec_amendments.csv", "the specification amendment record (Table S16d)");
const isT = (v) => String(v).toUpperCase() === "TRUE";
const sumRow = (re, eta, len) => recSum.find((x) => isT(x.state_re) === re && x.eta_update === eta && x.length === len) || {};
const PRIM = sumRow(true, "statewise", "long");
const PRIM_SHORT = sumRow(true, "statewise", "short");
const NORE_SW = sumRow(false, "statewise", "long"), NORE_BL = sumRow(false, "block", "long");
const verdictV = (re) => { const x = recVer.find((z) => re.test(z.criterion)); return x ? Number(x.value) : NaN; };
const V_MIXED = verdictV(/share of datasets mixed/), V_STAB = verdictV(/stable between run lengths/);
const STAGEA_N = Number(PRIM.datasets);
const STAGEA_NEED = Math.ceil(0.95 * STAGEA_N);
const cellMixed = (pred) => recCell.filter((x) => isT(x.state_re) && x.eta_update === "statewise" && x.length === "long" && pred(x))
  .reduce((a, x) => [a[0] + Number(x.mixed), a[1] + Number(x.datasets)], [0, 0]);
const NULL_MIX = cellMixed((x) => String(x.beta) === "0");
const EFF_MIX  = cellMixed((x) => String(x.beta) !== "0");
const fmtRhat = (v) => (String(v) === "Inf" || Number(v) === Infinity) ? "infinite" : nfmt(v, 2);
const pctN = (v, d) => isFinite(Number(v)) ? nfmt(100 * Number(v), d === undefined ? 0 : d) + "%" : "NA";
// Version 1, from the archived diagnostics, summarised in the committed record.
const v1Row = (re) => recV1.find((x) => re.test(x.analysis)) || {};
const V1_HEALTH = recV1.filter((x) => /^health models/.test(x.analysis));
const V1_H_RUNS = V1_HEALTH.reduce((a, x) => a + Number(x.runs), 0);
const V1_H_PASS = V1_HEALTH.reduce((a, x) => a + Number(x.runs_passing), 0);
const V1_ACC_MIN = Math.min(...recV1.map((x) => Number(x.acc_idx_min)).filter(isFinite));
const V1_DIST_MIN = Math.min(...V1_HEALTH.map((x) => Number(x.distinct_draws_min)).filter(isFinite));
const V1_ESS_MIN = Math.min(...recV1.map((x) => Number(x.ess_min_worst)).filter(isFinite));
const V1_RHAT_INF = recV1.some((x) => String(x.rhat_max_worst) === "Inf");
const V1_FRONT = v1Row(/frontier/);
const tune = (i) => recTune.find((x) => String(x.attempt) === String(i)) || {};
const semi = (v) => String(v || "NA").split(";").join(" and ");
const amd = (i) => recAmd[i] || {};
const SCEN_LAB = { S1: "S1, correct model", S2: "S2, calibration bias", S3a: "S3a, nonlinear dose-response",
                   S3b: "S3b, heterogeneous state slopes (non-zero mean)" };
const BETA_LAB = (b) => String(b) === "0" ? "0" : "beta_1";
if (!(V_MIXED < 0.95) && !(V_STAB < 0.95))
  console.warn("[build] the Stage A record no longer shows a failed verdict; SI S12 prose assumes it failed.");

// Table S16: Stage A, all four algorithm variants at both run lengths.
const tableS16 = mkTable([1500, 1300, 1000, 1500, 1200, 1200, 1500], [
  ["State intercept", "eta update", "Run length", "Datasets mixed", "Median max R-hat", "Median min ESS", "Median index acceptance"],
].concat(recSum.slice().sort((a, b) => (isT(b.state_re) - isT(a.state_re)) || a.eta_update.localeCompare(b.eta_update) || a.length.localeCompare(b.length))
  .map((x) => [isT(x.state_re) ? "yes" : "no", x.eta_update, x.length,
    x.mixed + " / " + x.datasets, fmtRhat(x.median_rhat_max), nfmt(x.median_ess_min, 0), pctN(x.median_acc_idx, 1)])));
// Table S16a: per-cell mixing, four long-run variants side by side.
const CELL_KEYS = [];
recCell.forEach((x) => { const k = x.scenario + "|" + x.beta; if (!CELL_KEYS.includes(k)) CELL_KEYS.push(k); });
const cellOf = (k, re, eta, len) => {
  const [sc, b] = k.split("|");
  const x = recCell.find((z) => z.scenario === sc && z.beta === b && isT(z.state_re) === re && z.eta_update === eta && z.length === len);
  return x ? x.mixed + " / " + x.datasets : "-";
};
const tableS16a = mkTable([3300, 1300, 1300, 1300, 1300], [
  ["Scenario, effect", "Primary, long", "Primary, short", "Block eta, long", "No intercept, long"],
].concat(CELL_KEYS.map((k) => [(SCEN_LAB[k.split("|")[0]] || k.split("|")[0]) + ", beta = " + BETA_LAB(k.split("|")[1]),
  cellOf(k, true, "statewise", "long"), cellOf(k, true, "statewise", "short"),
  cellOf(k, true, "block", "long"), cellOf(k, false, "statewise", "long")])));
// Table S16b: version 1 on the real outcome (and on the planned frontier).
const tableS16b = mkTable([2900, 900, 1000, 1700, 1300, 1200, 1000], [
  ["Analysis", "Runs", "Runs passing", "Index acceptance (min-max)", "Fewest distinct draws", "Worst max R-hat", "Smallest ESS"],
].concat(recV1.map((x) => [x.analysis.replace("non-overlap population", "36-month recent-cohort population"), x.runs, x.runs_passing,
  pctN(x.acc_idx_min, 1) + " - " + pctN(x.acc_idx_max, 1), x.distinct_draws_min, fmtRhat(x.rhat_max_worst), nfmt(x.ess_min_worst, 0)])));
// Table S16c: tuning log.
const tableS16c = mkTable([3900, 1000, 1200, 1100, 1000, 1200], [
  ["Change after Stage A", "Cell", "Datasets", "Max R-hat", "Min ESS", "Verdict"],
].concat(recTune.map((x) => [x.change + (x.within_approved_sampler === "no" ? " (outside the approved sampler)" : ""),
  x.cell.replace("beta1", "beta_1"), semi(x.datasets), semi(x.max_rhat), semi(x.min_ess), x.verdict])));
// Table S16d: specification history.
const tableS16d = mkTable([1400, 1900, 4300, 900, 1000], [
  ["Commit", "Time", "Event", "Stage A datasets", "Pass threshold"],
].concat(recAmd.map((x) => [x.commit, x.date, x.event, x.stage_a_datasets, x.pass_threshold])));

// Variance decomposition, transposed: one row per quantity, one column per leg,
// because two columns of fourteen numbers read far better than fourteen columns
// of two. The `vd` and `sigRatio` accessors are defined above, next to vdRow.
const VD_ROWS = [
  ["Calibration districts (overlap)", "calib_districts", 0],
  ["Calibration states", "calib_states", 0],
  ["Residual SD sigma (logit, posterior RMS)", "sigma", 3],
  ["State SD psi (logit, posterior RMS)", "psi", 3],
  ["Calibration slope b (logit-logit)", "slope_b", 3],
  ["Reference-survey SE, mean (logit)", "yse_ref", 3],
  ["Effective reference households per district", "neff_hh_per_district", 0],
  ["Target districts corrected", "targets", 0],
  ["... of which lie in a calibration state", "targets_covered_state", 0],
  ["Posterior variance of the corrected value (logit^2)", "var_observed", 4],
  ["... calibration-side component (a, b, u_s)", "var_calibration_side", 4],
  ["... NFHS-side component (b^2 x posterior variance of the true NFHS value)", "var_nfhs_side", 4],
  ["NFHS-side share (%)", "pct_nfhs_side", 1],
];
const tableS17 = mkTable([4300, 2200, 2200], [
  ["Quantity", "2015 leg (NFHS-4 ~ ACCESS)", "2019 leg (NFHS-5 ~ IRES)"],
].concat(VD_ROWS.map((rw) => [rw[0], vd("2015", rw[1], rw[2]), vd("2019", rw[1], rw[2])])));

const designSI = [
  h3("S4. Where the corrected exposure's uncertainty comes from"),
  p("Each corrected district value is a + b x + u_s, where x is the NFHS district estimate entering the measurement-error model as a covariate observed with known error. Its posterior variance therefore has two parts: a calibration-side part, carried by the posterior of the intercept, slope and state effect, which depends on the reference (energy) survey; and an NFHS-side part, b^2 times the posterior variance of the district's true NFHS value (its sampling variance, reduced by shrinkage toward the state mean), which is a property of the health survey being corrected rather than of the survey used to correct it. Table S17 reports the decomposition, averaged over the study districts on the logit scale. The NFHS-side part is " + DS("var_nfhs_side_share_2015", 0) + "% of the total on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg." + (DECOMP_VALID ? "" : " In this run the closed-form reconstruction failed its internal check on at least one leg (the reconstructed calibration-side variance exceeded the observed posterior variance in more than 10% of districts), so these shares are reported descriptively and not interpreted.")),
  p("The calibration-side part was not read off the fitted models but reconstructed analytically, in closed form, as a Bayesian linear mixed model with known within-district variances, averaging over the posterior draws of the residual and state standard deviations rather than conditioning on their posterior medians" + vcPenTxt() + ". " + fourChecksTxt()),
  p("The decomposition is descriptive. It apportions the uncertainty of the correction as fitted; it does not say what a differently designed reference survey would deliver, because a survey that measured the calibration relationship differently could also reveal a different relationship. One feature bears on interpretation. On the 2019 leg the fitted residual SD is " + vd("2019", "sigma", 3) + " on the logit scale (the posterior root-mean-square, which is the summary that enters the variance budget because sigma appears there only as sigma^2; SI Table S3 reports the posterior mean of the same parameter, a slightly smaller number), roughly " + sigRatio(1) + " times the " + vd("2015", "sigma", 3) + " fitted on the 2015 leg. Because the model already carries each reference district's own sampling error as a known variance, that sigma is equation error - systematic district-level disagreement between NFHS-5 and IRES in questionnaire wording, respondent, field timing or measurement - rather than reference-survey sampling noise."),
];

const failureSI = [
  h3("S12. Outcome-conditioned imputation: two unsuccessful attempts (failed convergence; not used for inference)"),
  p("Imputing the corrected exposure from the calibration posterior alone - the exposure-only imputation reported as a sensitivity analysis in SI Methods S6 - ignores the outcome, and in a linear model that adds district-level noise unrelated to mortality to every imputed surface and can attenuate the pooled coefficient toward zero by an amount Rubin's rules cannot detect. The standard remedy is to impute each district's exposure from its distribution given both the calibration posterior and the outcome model (measurement-error multiple imputation; Cole, Chu and Greenland 2006; Freedman and colleagues 2008). We attempted this twice. Neither attempt produced a sampler whose chains reliably agreed, and under a stopping rule fixed before the second attempt was piloted, development stopped. No estimate from either attempt is reported or used for inference. This section documents what was tried so that the decision can be judged; the records are released with the replication materials (records/health_imputation/), and the outputs of the first attempt are kept in an archive marked 'failed convergence - not for inference'."),
  p("The first version used a Metropolis-within-Gibbs sampler with three steps: the outcome model (the analysis model with a normal likelihood), drawn from its conjugate posterior given the current exposure; each district's NFHS-side sampling error given the outcome model; and the shared calibration, proposed as a whole saved posterior draw and accepted with the ratio of outcome likelihoods. Its outcome model had no state effect. Four independently initialised chains per run were compared by rank-normalised split R-hat and bulk effective sample size (ESS), and a run passed only if every monitored quantity, including the completed-data coefficient, had R-hat at most 1.05 and ESS at least 100. Of the " + V1_H_RUNS + " child-mortality runs across the three analysis populations, " + V1_H_PASS + " passed (Table S16b). Acceptance of moves between calibration draws fell as low as " + pctN(V1_ACC_MIN, 1) + ", some runs used only " + V1_DIST_MIN + " distinct calibration draws across their imputations, the smallest ESS was " + nfmt(V1_ESS_MIN, 0) + (V1_RHAT_INF ? ", and the worst R-hat was infinite, the chains having settled on different calibration draws" : "") + ". The same sampler underlay the precision-frontier evaluations planned for this paper, of which " + V1_FRONT.runs_failing + " of " + V1_FRONT.runs + " failed the rule; those evaluations are not reported." + (() => { const r = recV1.find((x) => /reference-design/.test(x.analysis)); return r ? " In a reference-design sensitivity analysis " + r.runs_passing + " of " + r.runs + " runs met the rule (Table S16b)." : ""; })()),
  p("Before a second attempt we wrote a specification fixing the model, the sampler, a two-stage pilot on synthetic outcomes and a stopping rule (docs/mime_v2_spec.md, released with the code). The redesign added a state random intercept to the imputation model, so that its outcome likelihood carries the state dependence that the CR2 analysis allows for; updated the effects of states outside the calibration sample on a non-centred scale; and used the empirical joint posterior draws of the fitted measurement-error models, extracted without refitting. Stage A tested convergence on synthetic outcomes generated on the real design (the study districts, states, covariates, weights and calibration posterior) under four scenarios: a correct model (S1), calibration bias (S2), a nonlinear dose-response (S3a) and heterogeneous state slopes (S3b), each with no exposure effect and with an effect equal to twice the fixed-surface estimate, except S3b, which has a non-zero mean effect only. Each dataset was fitted with four chains at two run lengths (burn-in 1,000 sweeps and thinning 10; burn-in 4,000 and thinning 40) and in a 2 x 2 algorithm comparison: state intercept present or absent, and the non-calibrated state effects updated state by state or as one block. Stage A passed only if, for the primary variant (state intercept, statewise updates) at the long run length, at least 95% of datasets met the convergence rule and the pooled estimate was stable between run lengths, differing by less than a quarter of its standard error (the specification stated the stability criterion without a dataset allowance; the pilot script applied it per dataset and required it in at least 95% of datasets). Stage B (bias and coverage against the fixed-surface and exposure-only comparators) and any use on the real outcome were conditional on Stage A passing, and the specification stated that if any condition failed, development of outcome-conditioned imputation would stop for this paper."),
  p("Why Stage A has " + STAGEA_N + " datasets. The specification as first written (" + amd(0).commit + ", " + amd(0).date + ") had three scenarios, each at two effect sizes, with ten datasets per cell: " + amd(0).stage_a_datasets + " datasets, passing at " + amd(0).pass_threshold + ". On review, its third scenario added state-specific slopes in every cell, so its no-effect cell was not genuinely null. The specification was amended (" + amd(1).commit + ", " + amd(1).date + "), before any Stage A run: the nonlinear scenario (S3a) kept both effect sizes, and the heterogeneous slopes moved to separate scenarios - S3b with a non-zero mean effect, in Stages A and B, and S3c with a zero-mean effect, in Stage B only and not counted as a null. Adding S3b to Stage A as a seventh cell gave " + amd(1).stage_a_datasets + " datasets and a threshold of " + amd(1).pass_threshold + ". Stage A was run once, on the amended specification (Table S16d)."),
  p("Stage A failed (Table S16; Table S16a). For the primary variant at the long run length " + PRIM.mixed + " of " + STAGEA_N + " datasets (" + pctN(V_MIXED, 1) + ") met the convergence rule, against the " + STAGEA_NEED + " required, and the pooled estimate was stable between run lengths in " + pctN(V_STAB, 1) + " of datasets, against 95%; at the short run length " + PRIM_SHORT.mixed + " mixed. Failure concentrated where the outcome carries an exposure effect: " + NULL_MIX[0] + " of " + NULL_MIX[1] + " no-effect datasets mixed, against " + EFF_MIX[0] + " of " + EFF_MIX[1] + " with an effect. Including state dependence substantially improved mixing in the pilot, but convergence remained inadequate. Without the state intercept no dataset mixed (" + NORE_SW.mixed + " and " + NORE_BL.mixed + " of " + STAGEA_N + " under the two updates), and median acceptance of moves between calibration draws was " + pctN(NORE_SW.median_acc_idx, 1) + " and " + pctN(NORE_BL.median_acc_idx, 1) + ", against " + pctN(PRIM.median_acc_idx, 0) + " with it; statewise and block updates performed similarly. The quantities that failed were mostly the 2019 calibration parameters, the mean and SD of the imputed change, and the completed-data coefficient: when the outcome carries an exposure effect it informs the 2019 calibration, and the conditional posterior over the saved calibration draws concentrates on few of them. The comparison is consistent with the first version's failure arising mainly from the missing state dependence, but does not prove it."),
  p("The specification limited changes after Stage A to sampler tuning, each recorded (Table S16c). On the hardest cell, calibration bias with an exposure effect, raising the number of calibration-index proposals per sweep from 5 to 25 left the largest R-hat at " + semi(tune(1).max_rhat) + " and the smallest ESS at " + semi(tune(1).min_ess) + " on the two datasets checked; thinning more heavily gave R-hat " + semi(tune(2).max_rhat) + " and ESS " + semi(tune(2).min_ess) + "; and a diagnostic variant outside the approved sampler, an additional calibration move holding the non-calibrated state effects fixed, was worse (R-hat " + semi(tune(3).max_rhat) + "). None met the rule. Under the stopping rule development stopped, Stage B was not run, and the second version was not applied to the real mortality outcome beyond software smoke tests whose output was not used."),
  p("What this means for the paper. The health association is reported only in two forms: the fixed corrected surface, which is conditional on the fitted calibration and whose interval excludes calibration uncertainty, and the exposure-only imputation, which includes calibration uncertainty but is at risk of attenuation. Neither is a validated estimate with calibration uncertainty properly propagated, so the health analysis is presented as conditional and exploratory. The failure limits what this paper can claim about health; it does not bear on the survey comparison, the calibration or the fuel-stacking results, none of which uses an imputation."),
];

// ================= ESTIMAND SI (23_ppd_sensitivity.R) ========================
const PS_ORDER = [
  ["epred_point", "Calibrated expectation, point surface (= corrected, Bayesian)"],
  ["emean_point", "Calibrated expectation, averaged on the probability scale"],
  ["epred_MI",    "Calibrated expectation, exposure-only MI (sensitivity)"],
  ["ppd_MI",      "District true prevalence, posterior predictive, exposure-only MI"],
  ["ppd_point",   "District true prevalence, posterior predictive, point surface"],
];
const psRows = () => {
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    PS_ORDER.forEach((v) => {
      const rr = psRow(oc, v[0]);
      if (!rr.estimate) return;
      const e = Number(rr.estimate), se = psSEn(oc, v[0]);
      const crit = psHasCR2(oc, v[0]) ? Number(rr.crit_cr2) : 1.96;
      out.push([oc === "infant" ? "Infant" : "Neonatal", v[1],
        nfmt(-10 * e, 3) + " [" + nfmt(-10 * (e + crit * se), 3) + " to "
          + nfmt(-10 * (e - crit * se), 3) + "]",
        psP(oc, v[0]),
        isFinite(Number(rr.fmi)) ? nfmt(100 * Number(rr.fmi), 0) : "NA"]);
    });
  });
  return out;
};
const tableS18 = HAVE_PPD ? mkTable([1100, 3400, 2200, 900, 700], [
  ["Outcome", "Exposure estimand", "Reduction per 10 pp [95% CI]", "p", "FMI %"],
].concat(psRows())) : null;

const ppdSI = [
  h3("S5. Which estimand? Posterior expectation versus posterior predictive"),
  p("The corrected district exposure carried into the health models is the calibrated expectation of the district's prevalence - the posterior of the linear predictor - and not a draw of the district's true prevalence, which would additionally include the calibration model's residual variance. Section 2.4.3 sets out why: the residual is Berkson rather than classical error with respect to the value actually used as the regressor, so in a linear outcome model it passes into the outcome residual without biasing the coefficient or deflating a cluster-robust standard error, whereas adding it back would convert Berkson error into classical error and attenuate the association toward the null. Because the calibration model is estimated without reference to mortality - the two models are uncongenial in Meng's sense - that attenuation is not guaranteed to be conservative in general, so it is checked here rather than assumed. As with any Berkson argument this holds under the assumed calibration model and is exact only under its conditions, and a reader may reasonably prefer the other estimand, so this section reports the alternative as a clearly labelled sensitivity rather than only asserting that it is the wrong target. Nothing is refitted: the sensitivity is computed from the saved posterior draws and the saved model fits."),
  p("The Berkson argument is exact on the logit scale, but the exposure enters the health regression on the probability scale, as a change in percentage points, and the logistic function is nonlinear - so the mean of the transformed draws is not the transform of the mean. The size of that Jensen gap has to be checked rather than assumed, particularly on the 2019 leg where the fitted residual SD is " + nfmt(pvRow("2019").sigma, 3) + " on the logit scale. Averaging the posterior on the probability scale instead moves the mean corrected prevalence by " + PQ("jensen_gap_change_mean_pp", 3) + " percentage points on average across districts in the 2015-2019 change (maximum " + PQ("jensen_gap_change_maxabs_pp", 2) + " percentage points); " + jensenEndTxt() + ". Refitting the mortality models on the probability-scale average shifts the infant coefficient by " + PQ("jensen_rel_shift_infant_pct", 2) + "% and the neonatal coefficient by " + PQ("jensen_rel_shift_neonatal_pct", 2) + "%. " + jensenVerdict() + " This check averages the calibration-parameter posterior only: it excludes the residual equation error whose size motivates it, so it does not test integration over that residual. Integrating over the residual as well is what the posterior-predictive point surface does (Table S18; it averages a finite set of residual draws per district, so part of any difference is Monte Carlo noise); it moves the infant point estimate from " + psEst10("infant", "epred_point") + " to " + psEst10("infant", "ppd_point") + " deaths per 100 births per 10 percentage points, a change in magnitude of " + psRelTxt("infant") + ", against a CR2 standard error of " + psSe10("infant", "epred_point") + " on the same scale. Were the probability-scale shift material, the remedy would be to save the probability-scale posterior mean from the calibration step in place of the logistic transform of the logit-scale mean; that would change the calibration output, not the estimand."),
  p("Table S18 reports the health models under both estimands. Imputing from the posterior predictive - adding the residual back, so that each imputation is a draw of the district's putative true prevalence rather than of its calibrated expectation - widens the exposure surface itself: the across-draw standard deviation of the corrected district prevalence rises by a factor of " + PQ("sd_inflation_2015", 2) + " on the 2015 leg and " + PQ("sd_inflation_2019", 2) + " on the 2019 leg. Against that wider exposure the infant coefficient " + ppdDirTxt("infant") + ", and " + ppdSeTxt("infant") + ". The point to take from the pair is that " + ppdZTxt("infant") + ": a regressor carrying more noise attenuates the coefficient attached to it and, because that noise enlarges the regressor's variance, sharpens rather than loosens the standard error, and the two effects substantially offset in the test. Both rows of the comparison use the exposure-only imputation, so both carry its attenuation concern; the posterior-predictive version adds a second source of attenuation on top of it. Comparing the two at the 0.05 threshold under the primary CR2 rule, " + ppdVerdict("infant") + " for infant mortality and " + ppdVerdict("neonatal") + " for neonatal mortality. " + ppdConsTxt() + ". We carry the calibrated expectation on the reasoning of Section 2.4.3, and report this table so that a reader who holds the other estimand can see exactly what it implies."),
];

// Displaced main-text material: the full child-mortality analysis (was Section
// 3.3) and the secondary analyses. The main text keeps only
// the fixed-surface-versus-propagated contrast; everything else lives here.
const healthSI = [
  h3("S6. The child-mortality illustration in full"),
  p("Section 3.5 reports this illustration in compact form. This section reports the full set of exposure specifications, sensitivity analyses and restrictions behind it (Table S6; SI Figure S21). Two caveats govern every estimate here. Estimates that hold the corrected surface fixed are conditional on the fitted calibration, and their intervals do not include calibration uncertainty. Estimates from the exposure-only imputation include calibration uncertainty but are at risk of attenuation, because the imputations do not condition on the outcome. Neither is a validated estimate with calibration uncertainty properly propagated; the two attempts to provide one are documented in SI Methods S12. Intervals throughout are CR2/Satterthwaite (see 'Inference with a small number of clusters' below)."),
  p("Across the " + N_RURAL + " rural districts with mortality estimates, corrected exposure and complete covariates, the raw NFHS fuel item gave neonatal " + est10(R_N_RAW) + " deaths per 100 births per 10-percentage-point rise (95% CI " + ci10cr2(R_N_RAW) + ") and infant " + est10(R_I_RAW) + " (95% CI " + ci10cr2(R_I_RAW) + "). Regression calibration gave neonatal " + est10(R_N_RC) + " (95% CI " + ci10cr2(R_N_RC) + ") and infant " + est10(R_I_RC) + " (95% CI " + ci10cr2(R_I_RC) + "), " + relPct(R_N_RAW, R_N_RC) + " than raw for neonatal and " + relPct(R_I_RAW, R_I_RC) + " for infant mortality; the fixed Bayesian surface gave neonatal " + est10(R_N_BAY) + " (95% CI " + ci10cr2(R_N_BAY) + ") and infant " + est10(R_I_BAY) + " (95% CI " + ci10cr2(R_I_BAY) + ", p = " + pCr2(R_I_BAY) + "). Taken in order, " + monoPairTxt() + "."),
  p("Exposure-only imputation. Each imputation uses one saved posterior draw of the calibration, the draws taken at evenly spaced positions in the saved chain, (intercept, slope and state effects, with a fresh state effect from the fitted state distribution for states outside the calibration sample) and adds each district's NFHS-side error - the slope times the posterior SD of the district's true NFHS value times a standard normal, correlated between the two rounds as described in SI Methods S1 - to give the corrected 2015 and 2019 logit prevalence. The change is formed on the probability scale, the analysis model is refitted on each of 200 imputations, and the results are pooled by Rubin's rules, with the CR2 variance and Barnard-Rubin degrees of freedom. The imputations do not use the outcome. The district-level part of each draw is therefore noise with respect to mortality, which in a linear model attenuates the coefficient toward zero by an amount Rubin's rules cannot detect, and the pooled interval is not the fixed-surface interval correctly widened. On this design the infant estimate is " + est10(R_I_MI) + " (95% CI " + ci10cr2(R_I_MI) + ") and the neonatal estimate " + est10(R_N_MI) + " (95% CI " + ci10cr2(R_N_MI) + "). " + miCmpTxt()),
  p("Model specification and population. Models were weighted by the harmonic mean, across the two rounds, of the births eligible for each outcome - a births-based analytic weight, not a population denominator - and adjusted for concurrent district changes in socioeconomic composition and in ambient conditions (PM2.5, temperature, relative humidity, and drought), with region fixed effects, clustered on state. Because the correction is estimated on rural households, district mortality is restricted to rural births so that exposure and outcome refer to the same population; in the all-household version (Table S7; N = " + N_ALL + ") the fixed-surface infant estimate is " + est10(A_I_BAY) + " (95% CI " + ci10cr2(A_I_BAY) + ") and the neonatal estimate " + est10(A_N_BAY) + ". Unweighted fits appear in the lower block of the same tables (rural infant, fixed surface: " + est10(R_I_BAY_UNW) + ", 95% CI " + ci10cr2(R_I_BAY_UNW) + "). The shorter-window (36-month) sensitivity gives a fixed-surface infant estimate of " + est10(NO_I_BAY) + " (95% CI " + ci10cr2(NO_I_BAY) + "; Table S6b), on fewer births per district."),
  p("Inference with a small number of clusters. These are district-level regressions clustered on state, and the number of clusters is " + nClus(R_I_BAY) + " - well inside the range where the conventional cluster-robust (CR1) sandwich is known to reject too often. We therefore report the bias-reduced CR2 estimator of Bell and McCaffrey with Satterthwaite degrees of freedom as the primary inference throughout, and a wild cluster bootstrap with the null imposed (Rademacher weights at the state level, " + wildB() + " replications) as a robustness check; the conventional result appears beside it in every table except Table S18. For the imputation-based estimates the CR2 variance is pooled by Rubin's rules and its degrees of freedom by the Barnard-Rubin correction with the mean Satterthwaite df as the complete-data df, so both the cluster count and the finite number of imputations are carried."),

  (HAVE_CALIB ? p("How much the rules differ here is calibrated by simulation rather than argued. Using this design - the same regressors, the same analytic weights in the fitted models (the simulated errors are not scaled by them), the same " + nClus(R_I_BAY) + " state clusters - data were generated " + calibReps() + " times under a known null, with the exposure coefficient set to exactly zero and errors drawn as a state random effect plus independent noise whose variances are recovered from the null-imposed residuals. The Gaussian error draw is deliberate: generating the errors with the bootstrap's own Rademacher state flips would test the wild cluster bootstrap against data produced the way it resamples. Against a nominal 5%, the conventional rule - the CR1 sandwich referred to a t distribution on the residual degrees of freedom, which is the rule reported in the conventional columns of these tables - rejected in " + calibRate(0) + "% of replications, CR2 with Satterthwaite degrees of freedom in " + calibRate(1) + "%, and the wild cluster bootstrap in " + calibRate(2) + "%" + (calibCsv[2] && isFinite(Number(calibCsv[2].bootstrap_B)) ? " (inside the simulation the bootstrap uses " + Number(calibCsv[2].bootstrap_B).toLocaleString("en-US") + " replications per dataset, against " + wildB() + " for the reported analyses)" : "") + ". That is the basis for demoting the conventional rule. " + cr2LimTxt() + " The calibration concerns how these regressions are tested, not what they estimate.") : p("The inference-rule calibration simulation (health_cluster_inference_calibration.csv) was not available for this build; the comparison of rules in Table S6a is reported without it.")),

  p("Table S6a gives the three inferences side by side for each fixed-surface specification. The estimate is common to all three; what changes is the width attached to it. For the era-matched Bayesian correction and infant mortality the conventional rule - CR1 with t on the residual degrees of freedom - returns p = " + pfmt((R_I_BAY || {}).p) + ", CR2 returns p = " + pCr2(R_I_BAY) + " on " + dfCr2(R_I_BAY) + " degrees of freedom, and the wild cluster bootstrap returns p = " + pWild("infant", "change_lpg_bayes") + "."),

  caption("Table S6a. Inference-rule comparison for the fixed-surface child-mortality models (rural districts, N = " + N_RURAL + ", " + nClus(R_I_BAY) + " state clusters). Each row is one exposure specification and one outcome, with a single point estimate and three inferences: the conventional cluster-robust (CR1) standard error referred to a t distribution on the residual degrees of freedom; CR2 with Satterthwaite degrees of freedom, this paper's primary rule; and a wild cluster bootstrap with the null imposed and Rademacher weights applied at the state level (" + wildB() + " replications). Units are deaths per 100 births per 10-percentage-point rise in primary-LPG prevalence. Fixed-surface estimates are conditional on the fitted calibration, and their intervals exclude calibration uncertainty. Exposure-only imputation rows are not shown here because the bootstrap is defined for a single fitted model; their CR2 intervals are in Tables S6 and S7."),
  mkTable([1100, 2200, 1200, 1000, 1000, 1000, 800], [
    ["Outcome", "Exposure measurement", "Est. per 10-pp", "p (conv.)",
     "p (CR2)", "p (wild)", "df (Satt.)"],
  ].concat(smallClusterRows())),
  p("", {}),

  p("The calibration instrument. The era-matched correction applies a different reference instrument at each endpoint - ACCESS calibrates the 2015 estimate, IRES the 2019 estimate - so part of the corrected change could reflect the switch of instrument rather than true temporal change. Applying a single instrument, the IRES rural calibration, to both NFHS rounds gives an infant estimate of " + est10(R_I_IRES) + " (95% CI " + ci10cr2(R_I_IRES) + "), " + sameSizeTxt(R_I_BAY, R_I_IRES) + ", and a neonatal estimate of " + est10(R_N_IRES) + " against " + est10(R_N_BAY) + "."),
  p("Calibration support. Two restrictions test whether the pattern depends on districts the calibrations never saw: districts inside both calibration surveys' footprints whose raw NFHS prevalence in both rounds also lies inside the range each calibration was fitted on, and separately districts inside an ACCESS state. With the exposure-only imputation, the restricted infant estimates are " + est10(supFull) + " (95% CI " + ci10cr2(supFull) + ", N = " + ((supFull || {}).n || "NA") + ") and " + est10(supState) + " (95% CI " + ci10cr2(supState) + ", N = " + ((supState || {}).n || "NA") + "); in the all-population analysis the same restrictions give " + est10(supFullAll) + " (95% CI " + ci10cr2(supFullAll) + ") and " + est10(supStateAll) + " (95% CI " + ci10cr2(supStateAll) + "). Because the restricted samples hold " + nfmt(100 * Math.max(Number((supFull || {}).n), Number((supState || {}).n)) / Number(N_RURAL), 0) + "% or fewer of the full sample's districts and rest on six states, these comparisons cannot establish robustness to limited calibration support. One check does not depend on the correction at all, and it corroborates the expected directions unevenly across the two eras: across districts, the association between the energy surveys' directly measured solid-fuel burning and child mortality is " + heDir(HE_A, "Any solid-fuel burning") + " in the ACCESS/NFHS-4 era and " + heDir(HE_I, "Any solid-fuel burning") + " in the IRES/NFHS-5 era, while the association with their primary-LPG prevalence is " + heDir(HE_A, "Primary LPG") + " and " + heDir(HE_I, "Primary LPG") + " respectively, the ACCESS-era figures resting on " + cmpN(C4R) + " districts in six northern and eastern states (SI Figure S11). All of these remain ecological associations across rural districts, and none is validated against an independent health-based measure of true exposure."),
];

const secondarySI = [
  h3("S7. Secondary analyses: predicted fuel use and adult cardiometabolic outcomes"),
  p("Two analyses are reported as secondary. First, we trained models in the energy surveys to augment NFHS with fuel-use detail it does not collect - fuel stacking, the four-category composition of fuel use, and LPG consumption (SI Methods S2; SI Figure S6; SI Table S8). These predictions have only moderate accuracy: in leave-district-out cross-validation the household stacking model reached an area under the ROC curve of " + ldo("ACCESS W1", "auc") + " in ACCESS and " + ldo("IRES rural", "auc") + " in IRES, with only modest agreement between predicted and observed district-level stacking (Pearson r = " + ldo("ACCESS W1", "district_r") + " and " + ldo("IRES rural", "district_r") + " respectively), with household wealth and size the largest standardized effects and no covariate individually strong (covariate contributions in SI Table S9). They surface one result that reinforces the measurement argument: the relationship between household characteristics and stacking transferred poorly - an IRES-era (2019-20) stacking model, even with era-specific context removed, showed only a weak association with observed 2015 ACCESS district stacking (r = " + CROSS_ERA_R + ", training on all IRES states). Because that test changes the calendar period and the measuring instrument at the same time, we decomposed it (SI Methods S3; SI Tables S12-S15). Holding the instrument fixed and changing only the era - training in ACCESS Wave 1 (2014-15) and testing in ACCESS Wave 2 (2018) - retains a district-level correlation of " + trR("ERA") + "; holding the era approximately fixed and changing only the instrument - training in IRES (2019-20) and testing in ACCESS Wave 2 - leaves " + trR("INSTRUMENT") + "; changing both leaves " + trR("ERA+INSTRUMENT") + ". Taken together, " + trWeakTxt() + ". Refitting on only the coefficients that are stable across surveys does not make the model transportable. Substituting these predicted composition metrics for corrected primary-LPG prevalence in the mortality model did not yield coherent associations and is reported only as exploratory (SI Table S8, SI Figure S10); corrected primary-LPG prevalence remains our exposure of record."),
  p("Second, we applied the same change-on-change design to two adult cardiometabolic outcomes (hypertension and diabetes, with definitions carried over from a prior study's analysis file; " + htnShort() + "; definitions below); " + adultSigTxt() + ". These cross-sectional risk factors integrate decades of cumulative exposure and are underpowered for a single-decade district design, so we report them only as a supplementary, exploratory analysis (SI Table S5; SI Figures S7-S9)."),
  p("Definitions and verification. Both outcomes are person-level indicators carried over from the analysis file of a prior study of clean cooking and hypertension in India [29] rather than rebuilt here. Hypertension is the mean of the second and third blood-pressure readings with systolic pressure at least 140 mmHg or diastolic at least 90 mmHg, or a self-reported diagnosis of high blood pressure, or current blood-pressure medication - a composite that is broader than the JNC7 measured-or-treated definition, because it also counts self-reported diagnosis, and that is defined only when all four components are non-missing; diabetes is a composite of self-reported diabetes or a measured glucose above 126 mg/dL after more than eight hours without food or drink and above 200 mg/dL otherwise, with the self-reported item alone as a sensitivity. Respondents are women aged 15-49 and men aged 15-54, excluding currently pregnant women and respondents who had lived at their current residence for under five years. The construction code is recovered for the NFHS-5 file only, so the definitions were also checked by recomputing the flags from the components stored in the analysis file for both rounds. That check reproduces the diabetes indicator exactly in both rounds (a respondent missing one component is classified only when the other is positive) and confirms that every respondent with raised measured blood pressure is classified as hypertensive. Hypertension is missing whenever the second and third readings, the diagnosis item or the medication item is missing (" + (HTN_VERIFIED ? "the rule that reproduces the stored indicator exactly in both rounds" : "as stated in the construction code") + "), so respondents missing any of these items are excluded from the hypertension analysis even if they reported a diagnosis or medication. " + htnVerTxt() + " " + htnMissTxt() + " District prevalences of each were estimated for NFHS-4 and NFHS-5 from the same four-level multilevel specification as Equation 1, among rural adult respondents (to match the rural corrected exposure), and regressed on the district change in primary-LPG prevalence (raw, regression-calibrated, and Bayesian-corrected) with the same socioeconomic and ambient adjustment set and the same state-clustered, CR2/Satterthwaite inference as the child-mortality models, but unweighted, each district counted once."),
];


// ================ SI SECTION S8: NSSO-78 EXTERNAL VALIDATION =================
// Built entirely from the outputs of 25_prep_nsso78.R / 26_compare_nsso78.R.
// If those scripts have not been run, the section, Table S19 and SI Figure S22
// are omitted with a warning (HAVE_NSSO), mirroring the HAVE_PPD pattern.
// REVISION CONVENTION: everything in this section is rendered in BLUE to mark
// it as new text. To fold it into the final manuscript, delete NSSO_BLUE's
// value (set it to undefined) and the section prints in black like the rest.
const NSSO_BLUE = "0000FF";
const rb = (t, e = {}) => r(t, { color: NSSO_BLUE, ...e });
const pb = (text, opts = {}) =>
  p(text, { ...opts, run: { color: NSSO_BLUE, ...(opts.run || {}) } });
const captionB = (t) => p(t, { par: { alignment: AlignmentType.LEFT,
  spacing: { line: 240, after: 240 } },
  run: { size: 20, bold: true, color: NSSO_BLUE } });
const h3b = (t) => new Paragraph({ heading: HeadingLevel.HEADING_3,
  spacing: { before: 200, after: 100 },
  children: [r(t, { bold: true, italics: true, size: SZ, color: NSSO_BLUE })] });
function mkTableB(colw, rows, { headerShade = true } = {}) {
  const cell = (t, { bold = false, shade = false, w = 0 } = {}) =>
    new TableCell({
      width: { size: colw[w], type: WidthType.DXA },
      shading: shade ? { type: ShadingType.CLEAR, fill: "E8EDF3" } : undefined,
      margins: { top: 50, bottom: 50, left: 90, right: 90 },
      children: [new Paragraph({ spacing: { line: 230 },
        children: [r(String(t), { bold, size: 19, color: NSSO_BLUE })] })],
    });
  return new Table({
    columnWidths: colw,
    width: { size: colw.reduce((a, b) => a + b), type: WidthType.DXA },
    rows: rows.map((cells, i) => new TableRow({
      children: cells.map((t, j) => cell(t, { bold: i === 0, shade: headerShade && i === 0, w: j })),
    })),
  });
}

const nssoCmp  = readCsv("nsso78_comparison_table.csv", { quiet: true });
const HAVE_NSSO = nssoCmp.length > 0;
// The main text cites SI Section S8, Table S19 and the sections and tables
// numbered after them (S9-S10, Tables S20-S21), so a build without the NSSO-78
// outputs would renumber or orphan those references. Refuse, unless the caller
// explicitly accepts an internally inconsistent draft.
if (!HAVE_NSSO) {
  const msg = "[build] NSSO-78 outputs not found (nsso78_comparison_table.csv). "
    + "SI Section S8, Table S19 and SI Figure S22 cannot be built, and the "
    + "main-text references to them would break. Run 25_prep_nsso78.R and "
    + "26_compare_nsso78.R, then rebuild (or set ALLOW_MISSING_NSSO=1 for a draft).";
  if (process.env.ALLOW_MISSING_NSSO === "1") console.warn(msg);
  else throw new Error(msg);
}

const nssoMeta = readCsv("nsso78_linkage_diagnostics.csv", { quiet: true });
const nmeta = (m) => { const row = nssoMeta.find((x) => x.metric === m);
                       return row ? row.value : "NA"; };
const nssoWide = readCsv("nsso78_nfhs_ires_district_lpg.csv", { quiet: true });
const nssoAbovePct = (a, b) => { const v = nssoWide.filter((x) => isFinite(Number(x[a])) && isFinite(Number(x[b])));
  return v.length ? nfmt(100 * v.filter((x) => Number(x[a]) > Number(x[b])).length / v.length, 0) : "NA"; };
const nssoNs = nssoWide.map((x) => Number(x.n_nsso)).filter((v) => isFinite(v))
                       .sort((a, b) => a - b);
const NSSO_MED_N = nssoNs.length ? nssoNs[Math.floor(nssoNs.length / 2)] : "NA";
const NSSO_MIN_N = nssoNs.length ? nssoNs[0] : "NA";

const nrow  = (lbl) => nssoCmp.find((x) => x.comparison === lbl) || {};
const NC = {
  n5all:  nrow("NSSO-78 (all) vs NFHS-5 (all)"),
  n5rur:  nrow("NSSO-78 (rural) vs NFHS-5 (rural)"),
  n5wt:   nrow("NSSO-78 (rural, design-wt) vs NFHS-5 (rural, design-wt)"),
  irall:  nrow("NSSO-78 (all) vs IRES (all)"),
  irrur:  nrow("NSSO-78 (rural) vs IRES (rural)"),
  irwt:   nrow("NSSO-78 (rural, design-wt) vs IRES (rural, design-wt)"),
  ctxall: nrow("NFHS-5 (all) vs IRES (all)  [context, IRES districts]"),
  ctxrur: nrow("NFHS-5 (rural) vs IRES (rural)  [context, IRES districts]"),
};
// mean_diff is on the proportion scale; render as signed percentage points.
const ppd = (x) => { const v = Number(x);
  return isFinite(v) ? (v >= 0 ? "+" : "−") + nfmt(Math.abs(100 * v), 1) : "NA"; };
// Share of the rural NFHS-IRES gap that the (later-fielded) MIS does NOT close:
const NSSO_GAP_CLOSED = (() => {
  const a = Number(NC.irrur.mean_diff), b = Number(NC.ctxrur.mean_diff);
  return (isFinite(a) && isFinite(b) && b !== 0)
    ? nfmt(100 * (1 - a / b), 0) : "NA";
})();

const tableS19rows = [
  ["Comparison", "Districts", "Pearson", "Spearman", "Sample-wt Pearson", "CCC", "Mean diff (pp)"],
].concat([
  ["NSSO-78 vs NFHS-5 (all households)",          NC.n5all],
  ["NSSO-78 vs NFHS-5 (rural)",                   NC.n5rur],
  ["NSSO-78 vs NFHS-5 (rural, design-weighted)",  NC.n5wt],
  ["NSSO-78 vs IRES (all households)",            NC.irall],
  ["NSSO-78 vs IRES (rural)",                     NC.irrur],
  ["NSSO-78 vs IRES (rural, design-weighted)",    NC.irwt],
  ["NFHS-5 vs IRES (all households; IRES footprint)", NC.ctxall],
  ["NFHS-5 vs IRES (rural; IRES footprint)",      NC.ctxrur],
].map(([lbl, d]) => [lbl, d.n_districts || "NA", nfmt(d.pearson), nfmt(d.spearman),
                     nfmt(d.pearson_ref_samplewt), nfmt(d.ccc), ppd(d.mean_diff)]));

const nssoMethodsSI = [
  h3b("S8. External validation against the NSSO 78th-round Multiple Indicator Survey (2020-21)"),
  pb("As a third, fully "
    + "independent check on the NFHS district exposure surface: the National "
    + "Sample Survey Office's 78th-round Multiple Indicator Survey (MIS), an "
    + "official household survey fielded January 2020 - August 2021 (a "
    + "COVID-extended window) that records each household's {{primary}} source "
    + "of energy for cooking, with LPG coded separately from piped natural gas. "
    + "Unlike ACCESS and IRES, the MIS samples the whole country on a design "
    + "in which districts are strata, and it is fielded by the same official "
    + "statistical system whose consumption rounds anchor much of the Indian "
    + "energy-access literature. We processed the MIS unit-level records ("
    + grp(nmeta("NSSO-78 households, total (Level 03)")) + " households; "
    + nmeta("distinct census-2011 districts") + " districts on the 2011 census "
    + "frame) with the estimator pair used for every other survey in this "
    + "pipeline: the unweighted three-level multilevel model (state, district, "
    + "first-stage sampling unit) and design-weighted direct estimates "
    + "(household multipliers, first-stage-unit clustering, full stratum "
    + "stratification), for all households and for rural households. The "
    + nmeta("NSS (state x district) code pairs in data") + " NSS state-district "
    + "code pairs in the data were mapped deterministically onto "
    + nmeta("distinct census-2011 districts") + " census-2011 districts through "
    + "a district key whose rows resolve exactly, by alias, or to the dominant "
    + "2011 parent of a district created after 2011 ("
    + nmeta("key rows resolved exact / alias / parent2011") + " key rows"
    + (() => { const k = String(nmeta("key rows resolved exact / alias / parent2011")).split("/").map((v) => Number(v)).reduce((x, y) => x + y, 0),
                   n = Number(nmeta("NSS (state x district) code pairs in data")), d = k - n;
               return isFinite(d) && d !== 0 ? "; " + Math.abs(d) + (d > 0 ? (Math.abs(d) === 1 ? " key row matches" : " key rows match") + " no code pair in the data" : (Math.abs(d) === 1 ? " code pair has" : " code pairs have") + " no key row") : ""; })()
    + ") - the same collapse applied to IRES's split sampling units - and "
    + "every household mapped. The weight rule and "
    + "fuel coding were verified by reproducing the published MIS national "
    + "clean-fuel shares (49.8% rural, 92.0% urban, 63.1% overall) to within "
    + "0.2 percentage points."),
  pb("Table S19 reports the agreement battery of Table S2 for the new pairs, "
    + "and SI Figure S22 the corresponding scatterplots. Against NFHS-5 on the "
    + (NC.n5all.n_districts || "NA") + "-district national overlap, the MIS "
    + "correlates r = " + nfmt(NC.n5all.pearson) + " (all households; rural "
    + "r = " + nfmt(NC.n5rur.pearson) + ") but sits " + ppd(NC.n5all.mean_diff)
    + " pp higher on average (" + ppd(NC.n5rur.mean_diff) + " pp rural). "
    + "Against IRES on its " + (NC.irall.n_districts || "NA") + "-district "
    + "footprint, the MIS shows r = " + nfmt(NC.irall.pearson) + " with a mean "
    + "difference of " + ppd(NC.irall.mean_diff) + " pp (all households) and "
    + ppd(NC.irrur.mean_diff) + " pp among rural households (" + (NC.irrur.n_districts || "NA") + " districts). On the IRES "
    + "footprint NFHS-5 sits " + ppd(NC.ctxall.mean_diff) + " pp (all, "
    + (NC.ctxall.n_districts || "NA") + " districts) and "
    + ppd(NC.ctxrur.mean_diff) + " pp (rural, " + (NC.ctxrur.n_districts || "NA")
    + " districts) relative to IRES"
    + (String(NC.ctxall.n_districts) === String(NC.irall.n_districts) ? "" : "; these "
    + "NFHS-IRES rows cover " + NC.ctxall.n_districts + " districts against "
    + (NC.irall.n_districts || "NA") + " for the NSSO-IRES rows, so comparisons "
    + "across rows are approximate") + "."),
  pb("Two features of this triangulation bear on the calibration. First, the "
    + "MIS - an official survey on an independent frame with an independent "
    + "field force - lands on the same side of NFHS-5 as IRES: in this period "
    + "NFHS-5 is the low outlier among the three data systems. Fieldwork timing "
    + "could explain part of the MIS-NFHS-5 difference, because the MIS window "
    + "(January 2020 - August 2021) is centred {{later}} than NFHS-5's while LPG use was still "
    + "rising; it is less able to explain why IRES, fielded within NFHS-5's window "
    + "(November 2019 - March 2020), is higher still, and restricting NFHS-5 to "
    + "pre-COVID interviews leaves the NFHS-IRES gap essentially unchanged (Section 3.1). Second, the MIS does not "
    + "reach IRES's levels either, particularly among rural households, where "
    + "it closes only about " + NSSO_GAP_CLOSED + "% of the rural NFHS-IRES "
    + "gap. Read jointly, the MIS is an independent comparison consistent with "
    + "NFHS-5 being low, but it does not by itself establish the correct "
    + "prevalence, and it suggests the rural {{magnitude}} of the IRES-anchored "
    + "correction may be too large: part of the residual NFHS-IRES "
    + "difference may sit on the IRES side (a 21-state design of 152 district sampling units "
    + "designed chiefly for state-level estimates, although district-level weights are provided) or reflect genuine "
    + "change across the surveys' staggered windows."),
  pb("Three caveats. MIS district samples are modest (median " + NSSO_MED_N
    + " households per district, minimum " + NSSO_MIN_N + "), so part of the "
    + "district-level scatter is sampling noise on the MIS side; the "
    + "multilevel estimates partially pool accordingly, and the design-"
    + "weighted rows of Table S19 tell the same story. The MIS fieldwork "
    + "spans the pandemic period, when refill subsidies and delivery "
    + "prioritization may have transiently raised primary-LPG reporting. And "
    + "the NFHS item groups LPG with piped natural gas while the MIS "
    + "separates them; piped gas reaches about half of one percent of "
    + "households nationally and is metro-concentrated, so this cannot move "
    + "rural district comparisons."),
];

// ---- S9: survey sample sizes and district linkage -------------------------
// Every number is read from 24_si_sample_size.R's summary and per-state tables
// and from the linkage diagnostics 01-03 and 25 write; nothing is typed.
const ssSum = readCsv("si_sample_size_summary.csv");
const ssSt  = readCsv("si_sample_size_state.csv");
const lnkAll = readCsv("missingness_linkage.csv");
const nssoLnk = readCsv("nsso78_linkage_diagnostics.csv", { quiet: true });
const ssv = (sv, dom, col) => (ssSum.find((x) => x.survey === sv && x.domain === dom) || {})[col];
const fint = (v) => (v === undefined || v === "" || v === "NA" || !isFinite(Number(v)))
  ? "-" : Number(v).toLocaleString("en-US");
const ssLine = (sv, dom) => fint(ssv(sv, dom, "households_total")) + " households in "
  + fint(ssv(sv, dom, "clusters_total")) + " clusters across " + fint(ssv(sv, dom, "districts"))
  + " districts (median " + fint(Math.round(Number(ssv(sv, dom, "hh_per_district_median")))) + " per district)";
// The linkage diagnostics count clusters in the DHS cluster-location (GPS)
// files; the sample-size summary counts clusters that contain interviewed
// households. With every household assigned, a difference between the two is
// location-file clusters with no household records in the analysed recode.
const clNoteTxt = () => {
  const a4 = lnk("NFHS-4 clusters"), b4 = Number(ssv("NFHS-4", "all", "clusters_total"));
  const a5 = L_CL5, b5 = Number(ssv("NFHS-5", "all", "clusters_total"));
  const unass = lnk("households unassigned");
  if (![a4, b4, a5, b5].every(isFinite) || (a4 === b4 && a5 === b5)) return "";
  if (unass !== 0)
    return " Cluster counts here (clusters containing interviewed households) differ from the linkage counts of Table S21 (" + grp(a4) + " NFHS-4 and " + grp(a5) + " NFHS-5 clusters), and " + grp(unass) + " households were not assigned to a district.";
  return " Cluster counts here are clusters that contain interviewed households in the household recode. The linkage counts in Table S21 (" + grp(a4) + " NFHS-4 and " + grp(a5) + " NFHS-5 clusters) are taken from the DHS cluster-location files, which list " + grp(a4 - b4) + " NFHS-4 and " + grp(a5 - b5) + " NFHS-5 clusters with no households in the analysed recode; no interviewed household was left unassigned, so no household is lost in the district linkage.";
};
const sampleSizeSI = [
  h3("S9. Survey sample sizes and district linkage"),
  p("Table S20 reports, for each state, the number of surveyed households and districts contributing to the district-level estimates in each survey, and Table S21 records how the households of each survey were linked to the NFHS-4 district frame. District prevalences in this paper are small-area estimates, so the number of households and clusters per district governs their precision and the degree of partial pooling toward the state mean."),
  p("All counts are households. NFHS-4 contributes " + ssLine("NFHS-4", "all") + ", of which the rural analytic sample is " + ssLine("NFHS-4", "rural")
    + ". NFHS-5 contributes " + ssLine("NFHS-5", "all") + " after assignment to the NFHS-4 district frame, of which the rural analytic sample is " + ssLine("NFHS-5", "rural")
    + ". The dedicated energy surveys are far smaller, and their district estimates correspondingly noisier: IRES covers " + ssLine("IRES", "all") + ", and its rural subsample, which the NFHS-5 calibration uses, " + ssLine("IRES", "rural")
    + "; ACCESS Wave 1 covers " + ssLine("ACCESS W1", "rural") + ", the clusters being villages." + (HAVE_NSSO ? " The NSSO-78 Multiple Indicator Survey (SI Methods S8) covers " + ssLine("NSSO-78", "all") + "." : "") + clNoteTxt()),
  h3("S10. The cooking-fuel question in each survey"),
  p("The surveys ask about cooking fuel in ways that are not interchangeable, and the difference is the likeliest origin of the measurement error this paper corrects. Table S1 lists the items used from each survey, and the variable inventory at the end of the Supplementary Materials gives their source codes."),
  p("NFHS records a single main cooking fuel per household, from the household questionnaire's type-of-cooking-fuel item, asked of a general household respondent. One response is permitted. There is no follow-up on additional fuels, no record of quantities or refill frequency, and no separate item for any fuel used alongside the one reported, and LPG is coded together with natural gas. A household that cooks on LPG and firewood is therefore represented by whichever of the two the respondent names."),
  p("ACCESS administers a dedicated cooking-energy module (in Wave 2 the primary cook was asked to be present for it; the household head or another adult is the respondent): the main cooking fuel (m5_q118_main_cookfuel), separate yes/no items for each fuel in use - LPG (m4_q103_lpg), firewood (m4_q109_firewood), dung cake (m4_q113_dungcake) and agricultural residue (m4_q114_agro) - and LPG cylinder purchases by size and source (m4_q103_5_lpg_lcyl_dist, m4_q103_6_lpg_lcyl_mkt, m4_q103_8_lpg_scyl_dist, m4_q103_9_lpg_scyl_mkt), from which annual consumption is computed."),
  p("IRES likewise records the primary cooking fuel (q529_prim_cook_fuel), with LPG coded separately from piped natural gas, and adds separate yes/no items for LPG use (q502_lpg_use_yn), firewood (q515_firewood_cook_use_yn), dung cake (q519_dung_cook_use_yn), agricultural residue, coal and kerosene (q523_*), whether LPG meets all cooking needs (q514_lpg_use_all_needs_yn), and cylinder refills by size (q505_1_lpg_large_n, q506_lpg_small_n, q506_1_lpg_small_kg). It is these additional items, absent from NFHS, that make fuel stacking observable at all."),
  p("The consequence is that the NFHS measure is not a noisy version of the energy-survey measure but a different quantity whose meaning depends on the composition of the population it is asked of. When few households have LPG, few are in a position to be classified differently by the two instruments, and the single response and the full portfolio nearly coincide, which is what we observe for NFHS-4 and ACCESS before PMUY. When many households have LPG and most of them also burn a solid fuel, the two diverge, and the size of the divergence depends on how respondents rank fuels they both use, which is what we observe for NFHS-5 and IRES afterwards. This is why comparability across survey rounds is treated here as a hypothesis to be tested rather than an assumption, and why the calibration is estimated separately for each era (Table S3)."),
];

// Table S20: households (districts) by state and survey. State names are
// shown in standard spelling; the source files carry a few legacy spellings.
const STATE_DISPLAY = { "Chattisgarh": "Chhattisgarh", "Nct Of Delhi": "NCT of Delhi",
                        "Uttarakhaand": "Uttarakhand" };
const stName = (x) => STATE_DISPLAY[x] || x;
const stCell = (row, pre) => {
  const h = row[pre + "_households"], d = row[pre + "_districts"];
  return (h === undefined || h === "" || h === "NA") ? "-" : fint(h) + " (" + fint(d) + ")";
};
const S20_COLS = [["NFHS4_rural", "NFHS-4 rural"], ["NFHS5_rural", "NFHS-5 rural"],
                  ["IRES_rural", "IRES rural"], ["ACCESSW1_rural", "ACCESS W1"],
                  ["NSSO78_rural", "NSSO-78 rural"]].filter((c) => HAVE_NSSO || c[0] !== "NSSO78_rural");
const S20_TOT = { NFHS4_rural: ["NFHS-4", "rural"], NFHS5_rural: ["NFHS-5", "rural"],
                  IRES_rural: ["IRES", "rural"], ACCESSW1_rural: ["ACCESS W1", "rural"],
                  NSSO78_rural: ["NSSO-78", "rural"] };
const tableS20 = mkTable([2160].concat(S20_COLS.map(() => 1440)), [
  ["State"].concat(S20_COLS.map((c) => c[1]))]
  .concat(ssSt.map((row) => [stName(row.state_name)].concat(S20_COLS.map((c) => stCell(row, c[0])))))
  .concat([["All study districts"].concat(S20_COLS.map((c) => {
    const t = S20_TOT[c[0]];
    return fint(ssv(t[0], t[1], "households_total")) + " (" + fint(ssv(t[0], t[1], "districts")) + ")";
  }))]));

// Table S21: linkage of each survey's households to the NFHS-4 district frame.
const lnkVal = (m, v) => /share|agreement/.test(m) ? nfmt(100 * Number(v), 1) + "%" : fint(v);
const tableS21 = mkTable([1700, 5960, 1700], [["Survey", "Linkage quantity", "Value"]]
  // The re-listing share is a property of the appended panel file (every Wave 1
  // id recurs), not the fielded retention rate, so it is not reported here.
  .concat(lnkAll.filter((x) => !/re-listed/.test(x.metric)).map((x) => [x.source.replace(/^NFHS(\d)$/, "NFHS-$1"), x.metric, lnkVal(x.metric, x.value)]))
  .concat(nssoLnk.filter((x) => isFinite(Number(x.value)))
    .map((x) => ["NSSO-78", x.metric, lnkVal(x.metric, x.value)])));

const nssoTablesSI = [
  captionB("Table S19. External validation against the NSSO 78th-round "
    + "Multiple Indicator Survey (SI Methods S8): district-level agreement in "
    + "the prevalence of primary-LPG use as the main cooking fuel, on the "
    + "NFHS-4 2015 district frame. Statistics as in Table S2: Pearson and "
    + "Spearman correlations, Pearson weighted by the reference survey's "
    + "district household count, Lin's concordance correlation coefficient "
    + "(CCC), and the mean difference (first-listed survey minus second, "
    + "percentage points). Design-weighted rows compare design-weighted "
    + "direct estimates; all other rows compare multilevel estimates. The "
    + "NFHS-5 vs IRES rows repeat the Table S2 comparison on the IRES "
    + "footprint for reference; their district counts can differ slightly "
    + "from the NSSO-IRES rows, so comparisons across rows are approximate."),
  mkTableB([2960, 950, 900, 950, 1250, 800, 1150], tableS19rows),
  pb("NSSO-78 estimates from 25_prep_nsso78.R (multilevel: state/district/"
    + "first-stage-unit random effects; design-weighted: MULT/100 household "
    + "multipliers, first-stage-unit clustering, stratum stratification). "
    + "Districts with fewer households appear in the multilevel rows with "
    + "partial pooling toward the state mean.",
    { par: { alignment: AlignmentType.LEFT, spacing: { line: 240, after: 240 } },
      run: { size: 20 } }),
  p("", {}),
];

const nssoFigsSI = [
  img("nsso78_scatter_45deg.jpeg", 430, 430),
  captionB("SI Figure S22. External validation against the NSSO 78th-round "
    + "Multiple Indicator Survey (SI Methods S8, Table S19). District "
    + "prevalence of primary-LPG use from the MIS (vertical axis, multilevel "
    + "estimates) against NFHS-5 (left column) and IRES (right column), for "
    + "all households (top row) and rural households (bottom row), on the "
    + "NFHS-4 2015 district frame. Dashed line is equality; the solid line is "
    + "the least-squares fit; points are sized by district household count and coloured by state (legend omitted) - "
    + "the MIS sample in the NFHS-5 panels and the IRES sample in the IRES "
    + "panels. "
    + "The MIS sits above NFHS-5 in " + nssoAbovePct("nsso_mainlpg", "lpg_2019") + "% of districts (all households; " + nssoAbovePct("nsso_mainlpg_rural", "lpg_2019_rural") + "% rural) but below IRES in most of "
    + "the IRES overlap, so the MIS lies between NFHS-5 and IRES (SI Methods S8)."),
];

const vpTableRows = [["Survey", "Households", "Villages", "Also burn solid fuel %", "State %", "District %", "Village %", "All place levels %", "Household characteristics (marginal R2) %", "Place variance absorbed by characteristics %"]]
  .concat(vpTab.map((x) => [x.survey, fint0(x.n_households), fint0(x.n_villages), isFinite(Number(x.stacking_share)) ? nfmt(100 * Number(x.stacking_share), 0) : "NA", nfmt(Number(x.pct_state), 1), nfmt(Number(x.pct_district), 1),
    nfmt(Number(x.pct_village), 1), nfmt(Number(x.pct_place_total), 1), nfmt(Number(x.marginal_r2_household_pct), 1),
    nfmt(Number(x.place_variance_absorbed_pct), 0)]));
const seasTableRows = [["Survey", "Specification", "Season (vs winter)", "Difference, pp", "SE, pp", "p", "Clusters", "Districts (>1 season)"]]
  .concat(seasTabN.map((x) => [x.survey.replace(/^NFHS(\d)$/, "NFHS-$1"), x.specification, x.term, (Number(x.pp_vs_winter) >= 0 ? "+" : "") + nfmt(Number(x.pp_vs_winter), 1),
    nfmt(Number(x.se_pp), 1), pfmt(Number(x.p)), fint0(x.n_clusters), fint0(x.n_districts_multi_season)]));
const placeSeasonSI = HAVE_PS ? [
  h3("S11. Where fuel stacking varies, and whether the NFHS main-fuel answer shifts with the season"),
  p("Two features of fuel stacking reported in the literature bear directly on this paper: that stacking is driven mainly by local supply, price and context rather than by household type, and that households which stack often change their primary fuel with the season. Both were examined in this study's own data (scripts/28_stacking_place_and_season.R)."),
  ...(vpTab.length ? [
    p("Where stacking varies. Among primary-LPG households in each reference survey, stacking (also burning solid fuel) was modelled with state, district and village random intercepts and no household characteristics, and then with the household characteristics the three surveys share (caste, religion, household size, BPL/Antyodaya card, electricity and within-state wealth quintile). On the latent logistic scale, " + vpRange("pct_place_total", 0) + "% of the variation in stacking lies between states, districts and villages (Table S22), while the shared household characteristics account for " + vpRange("marginal_r2_household_pct", 1) + "% (marginal R2). State-level validation: predicting each IRES state from a household model fitted to the other states gave a state-level correlation of " + nfmt(SSV_LOSO.r_state, 2) + " across " + (SSV_LOSO.n_states || "NA") + " states, and " + skillTxt(SSV_LOSO) + "; applied to the six ACCESS Wave 1 states, the all-state IRES model gave r = " + nfmt(SSV_ACC.r_state, 2) + " and " + skillTxt(SSV_ACC) + " (benchmark: each held-out state assigned the remaining states' overall share, states with fewer than 20 households not scored). Because the latent scale assigns a fixed household-level variance of pi^2/3, these place shares are latent-scale intraclass correlations, and the comparison that matters is between the place columns and the household column: the characteristics a national survey records explain " + (Math.max(...vpTab.map((x) => Number(x.marginal_r2_household_pct)).filter(isFinite)) < Math.min(...vpTab.map((x) => Number(x.pct_place_total)).filter(isFinite)) ? "less of stacking than where a household lives does" : "a share of stacking comparable to where a household lives") + ", consistent with the stacking model's failure to predict held-out states or to transfer between surveys (Section 3.3; SI Methods S3)."),
    captionB("Table S22. Where stacking varies (SI Methods S11). Primary-LPG households in each reference survey; outcome: household also reports burning solid fuel (the share doing so is the fourth column). Place columns are variance shares on the latent logistic scale from a model with state, district and village random intercepts only (residual variance pi^2/3). Household characteristics: marginal R2 when caste, religion, household size, BPL/Antyodaya card, electricity and within-state wealth quintile are added. Last column: reduction in the summed place-level variance when those characteristics are added (values are floored at zero, so 0 means none was absorbed)."),
    mkTableB([1700, 850, 750, 850, 650, 750, 700, 950, 1200, 1200], vpTableRows), p("", {})] : []),
  ...(seasTabN.length ? [
    p("Seasonality of the main-fuel answer. Within the same district, NFHS clusters interviewed in different seasons were compared (district fixed effects, design-weighted cluster shares of rural households reporting LPG as the main cooking fuel, standard errors clustered on district), with and without a linear trend in interview date so that a seasonal difference is not simply LPG rising over the fieldwork period. " + seasSentence("NFHS5", "NFHS-5") + " " + seasSentence("NFHS4", "NFHS-4") + " The design can only use districts whose clusters were interviewed in more than one season, and season is partly entangled with fieldwork phase, so these estimates inform rather than settle the question (Table S23). The comparison with IRES in the main text is in any case robust to restricting NFHS-5 to winter interviews (December-February, the season in which IRES was fielded) and to pre-COVID interviews (Section 3.1)."),
    captionB("Table S23. Seasonality of the NFHS main-cooking-fuel answer (SI Methods S11). Difference in the share of rural households reporting LPG as the main cooking fuel, relative to clusters of the same district interviewed in winter (December-February), in percentage points. Weighted least squares on design-weighted cluster shares with district fixed effects, standard errors clustered on district; the second specification adds a linear trend in interview date."),
    mkTableB([900, 2600, 1700, 1000, 800, 800, 900, 1000], seasTableRows), p("", {})] : []),
] : [];

const siChildren = [
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120 },
    children: [r("Supplementary Materials", { bold: true, size: 30 })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240 },
    children: [r("Companion to: " + TITLE, { italics: true, size: 22 })] }),

  h1("Supplementary Methods"),
  h3("S1. Survey sampling weights and design-based standard errors"),
  ...weightsSI,
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 80, after: 40 },
    children: [new ImageRun({ type: "png",
      data: fs.readFileSync(FIG + "fig_eq_selogit.png"),
      transformation: { width: 220, height: 74 } })] }),
  p("Equation S1. Delta-method transformation of a design-based standard error from the proportion scale to the logit scale, where p is the district proportion and SE_p its design-based standard error.", { par: { alignment: AlignmentType.CENTER }, run: { size: 20, italics: true } }),

  p("The prior specification of the measurement-error model is recorded here for the same reason. Weakly informative priors were placed on the calibration parameters: Normal(0, 2) on the intercept and on the slope relating the reference logit prevalence to the NFHS logit estimate, and Exponential(1) on the between-state and residual standard deviations; the latent district logit prevalences of the me() term receive the brms default hierarchical prior, a normal distribution with location and scale estimated from the data. The NFHS and reference-survey estimates enter only through the likelihood - the NFHS estimate as an error-prone measurement of the latent district value with known standard error, and the reference estimate as the response - so neither survey functions as a prior."),

  h3("S2. Fuel-use prediction models (secondary analysis)"),
  p("The rationale for imputing fuel-use detail into NFHS, rather than analyzing the energy surveys alone, is that NFHS is the only data system with all three properties that health analysis requires: national coverage at district resolution, repeated rounds that support longitudinal designs, and individual-level health and anthropometric outcomes measured in the same households as cooking fuel. The energy surveys measure the fuel portfolio credibly but cover limited geographies at single time points and collect no health outcomes; transferring their fuel-use detail onto NFHS households combines the strengths of both systems. To recover fuel-use detail absent from NFHS, we model the three energy-survey outcomes as functions of the harmonized covariate set (caste, religion, household size, ration card, electricity, within-state wealth quintile; education is omitted because the NFHS extracts carry no populated attainment item, Table S1) plus a district-context covariate, the district share of households using LPG as the main fuel: multilevel logistic regression (state and district random effects) for stacking among primary-LPG households; multinomial logistic regression for the four-category fuel-use outcome; and a two-part hurdle model for annual LPG consumption, combining logistic regression for any LPG use with a log-linear model for consumption among users, retransformed with Duan's smearing estimator (Duan 1983), following Gould et al. (2020). Models are trained separately in ACCESS Wave 1 (matching the NFHS-4 era and rural setting) and IRES (matching the NFHS-5 era, urban and rural). When applying the models to NFHS households, the district-context covariate is filled with the corrected district prevalence from the measurement-error model, which places it on the reference-survey scale on which the models were trained, matched to the corresponding era; the stacking model is fitted with state and district random effects but predicts every NFHS household from its fixed part (a conditional prediction at zero random effects, not a population average, the same estimand in every district), while for the composition and consumption models households in states outside a training survey's coverage receive predictions from a no-state-effect variant and are flagged as extrapolations."),
  p("Predictive validity is assessed two ways, both of them on a reduced, context-free specification rather than on the production model: leave-district-out cross-validation within each training survey (ten folds, each holding out whole districts; household predictors with state fixed effects), and a stricter cross-survey, cross-era test in which a multilevel model with the same household predictors and state and district random effects, fitted to rural IRES, is applied to ACCESS households from its fixed part and evaluated against observed ACCESS district-level stacking. Household-level performance is summarized by the area under the receiver-operating-characteristic curve (AUC). District-level performance is summarized by the correlation between observed and predicted district stacking prevalence. Neither validation fits the deployed model. Production adds the district-context covariate; the validated specifications drop it. The omission is forced rather than stylistic, because that covariate is a district mean computed within the training survey and is therefore either unavailable for a held-out district or leaks that district's own aggregate into its own prediction. The figures below accordingly measure how far the household-level predictors transport to unseen districts; they are not a performance estimate for the surface carried into NFHS. Both era-matched model sets are used in production - ACCESS-era models generate the NFHS-4 predictions and IRES-era models the NFHS-5 predictions (the exploratory health extension of Table S8 instead applies the IRES models to both rounds, so that the district change does not switch models); the primacy assigned to IRES in Section 2.4.3 concerns only the corrected prevalence surfaces and reflects geographic support (" + I_STATES + " states versus six), not a preference between the stacking models. In leave-district-out cross-validation the stacking models achieved AUC " + ldo("ACCESS W1", "auc") + " (ACCESS) and " + ldo("IRES rural", "auc") + " (rural IRES), with observed-vs-predicted district correlations of " + ldo("ACCESS W1", "district_r") + " (ACCESS) and " + ldo("IRES rural", "district_r") + " (IRES); within ACCESS, annual LPG consumption from cylinder purchases averaged " + KG_MEAN + " kg/year among LPG-using households (median " + KG_MEDIAN + " kg/year; n = " + KG_N + " LPG-using household-wave observations, both waves pooled), in the same range as the 93 kg/year reported in a prior analysis of ACCESS Wave 1 (Gould et al. 2020). The cross-era test is a negative result: an IRES-trained (2019-20) model, even excluding era-specific context, showed only a weak association with observed 2015 ACCESS district stacking (r = " + CROSS_ERA_R + "), motivating the era-matched design and cautioning against transporting stacking models across policy regimes. As an external check, the design-weighted IRES rural three-category composition (restricted, like the external estimates, to LPG-or-solid households) places the LPG-with-no-solid-fuel-reported share at " + hb("LPG, no solid fuel reported", "share") + "%, close to the " + hb("LPG, no solid fuel reported", "habib_2023", 0) + "% that an independent pan-India survey reports for rural India in 2019 under its own 'exclusive LPG' definition [39]; our stacking (" + hb("LPG and solid fuel reported", "share") + "%) and solid-fuel-with-no-LPG (" + hb("Solid fuel reported, no LPG", "share") + "%) shares differ from their " + hb("LPG and solid fuel reported", "habib_2023", 0) + "%/" + hb("Solid fuel reported, no LPG", "habib_2023", 0) + "% split because our classification counts any reported solid-fuel use as stacking whereas theirs reflects regular use, so the two stacking shares are defined differently; reported use of both LPG and solid fuels does not establish the frequency or intensity of stacking. For NFHS-5, " + supDistPct + "% of districts on the all-household district frame, holding " + supHhPct + "% of sampled households, lie in IRES-covered states; predictions elsewhere are extrapolations from a no-state-effect model variant and are flagged in the released estimates. The predictive contribution of each covariate, quantified by drop-one leave-district-out AUC and by standardized effect size, is reported in Table S9: the within-state wealth quintile and household size are the strongest predictors in both training surveys, the district-context covariate adds little, and no single covariate is strong - consistent with the models' modest discrimination and with fuel stacking being poorly captured by standard survey covariates."),

  h3("S3. Does the stacking model fail across eras or across instruments? (transfer diagnostic)"),
  ...transferSI,

  ...designSI,

  ...(HAVE_PPD ? ppdSI : []),

  ...healthSI,

  ...secondarySI,

  ...(HAVE_NSSO ? nssoMethodsSI : []),

  ...sampleSizeSI,

  ...placeSeasonSI,

  ...failureSI,

  h1("Supplementary Tables"),
  caption("Table S1. Measures used from each survey (NFHS-4, NFHS-5, ACCESS Wave 1, and IRES): the primary-LPG exposure indicator common to all four surveys, the fuel-use outcomes constructed only from the energy surveys (fuel stacking, four-category fuel use, and annual LPG consumption), and the harmonized socioeconomic covariates, together with the source item in each survey. A dash indicates a measure a survey does not collect."),
  table1, p("", {}),
  caption("Table S2. District-level agreement between NFHS and the reference energy surveys for the prevalence of primary LPG use as the main cooking fuel, for the two temporally overlapping survey pairs (NFHS-4 vs ACCESS Wave 1; NFHS-5 vs IRES) under several specifications (all households, rural only, and design-weighted direct estimates on both sides). Columns are: N, the number of districts common to both surveys; Pearson r and Spearman, the correlations between the two surveys' district estimates; Sample-wt r, the Pearson correlation with each district weighted by its number of reference-survey households, so that better-sampled districts count more (a sample-size weighting, not a population weighting); CCC, Lin's concordance correlation coefficient, which unlike a correlation also penalizes departures from the line of equality; and Mean diff, the average of (NFHS minus reference) on the 0-1 proportion scale, where a negative value means NFHS is lower. Restricting NFHS to rural clusters and using design-weighted estimates brings the NFHS-4/ACCESS pair into close agreement, whereas the NFHS-5/IRES pair retains a large negative mean difference - about " + GAP_DW + " percentage points on the design-weighted rural-versus-rural comparison and " + cmpGap("NFHS-5 (rural) vs IRES (rural)", 1) + " points on the multilevel estimates."),
  table2, p("", {}),
  caption("Table S3. Bayesian measurement-error calibration of the NFHS district primary-LPG estimates against the reference energy surveys, on the logit scale. For each calibration, estimated on rural households (NFHS-4 rural district estimates against ACCESS Wave 1, which sampled only rural households; NFHS-5 rural district estimates against the rural design-weighted IRES estimate), entries are the posterior mean and 95% credible interval (CrI) of the slope relating the reference logit-prevalence to the NFHS logit-estimate, the intercept, the between-state and residual standard deviations (SD), the number of calibration districts, and, for NFHS-5, the leave-one-state-out (LOSO) cross-validation result: the number of held-out states in which the calibration lowered the root-mean-square error (RMSE) against the reference, and the median held-out RMSE before and after correction. For NFHS-4/ACCESS the corrected values stay close to the raw ones on average (Table S10), indicating near-agreement; for NFHS-5/IRES the large positive intercept indicates that NFHS-5 understates primary-LPG prevalence over most of the observed range, and the slope below 1 that the gap narrows as NFHS-5 prevalence rises. The leave-one-state-out rows validate the calibration relation by regression calibration refit in each fold; the Bayesian model was not refit in the folds."),
  table3, p("", {}),
  caption("Table S4. Benchmark (falsification) analysis: district-level agreement between NFHS and the reference surveys, for rural districts, on characteristics measured comparably in both surveys, shown alongside primary LPG. Variables are Scheduled Caste (SC), Scheduled Tribe (ST), their union (SC/ST), the Hindu and Muslim population shares, household electricity, possession of a below-poverty-line (BPL) ration card, and primary LPG. Columns are: N, the number of overlapping districts; Pearson r; CCC, Lin's concordance correlation coefficient; and Mean diff, the average of (NFHS minus reference) on the proportion scale. Close agreement on religion and Scheduled Tribe - Scheduled Caste agrees less well across districts (r = " + bmR("NFHS-4 vs ACCESS W1 (rural)", "sc") + " and " + bmR(BM5, "sc") + "), and in level closely only on the NFHS-5/IRES pair (mean difference " + bmDiff(BM5, "sc") + " points, against " + bmDiff("NFHS-4 vs ACCESS W1 (rural)", "sc") + " on the NFHS-4/ACCESS pair) - together with the large negative mean difference for primary LPG is consistent with the two surveys sampling comparably composed populations, so that the LPG discrepancy reflects fuel measurement rather than who was surveyed. Household electricity and the ration card are programme-linked items: on the NFHS-5/IRES pair electricity agrees in level but weakly across districts, and the ration-card item, asked differently in the two instruments, differs as much as LPG, so neither is a clean benchmark. All rows, primary LPG included, use multilevel district estimates refit on the benchmark frame (for NFHS-4, the six ACCESS states only), so the primary-LPG row can differ in the third decimal from the national-frame estimates of Table S2."),
  tableS4,
  p("* After harmonizing the IRES ration-card response coding (Antyodaya + BPL vs none; don't-know set missing). The residual difference reflects instrument design: NFHS asks specifically about possession of a BPL card, whereas IRES elicits the card type held.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S5. Supplementary analysis: district change-on-change association between primary-LPG prevalence and adult cardiometabolic outcomes. For each outcome (" + (HTN_VERIFIED ? "" : "provisional definitions; ") + "Section 2.4.7) - hypertension (measured, self-reported diagnosis or medication), diabetes as a measured-or-self-reported composite, and diabetes by self-report only - the district change in prevalence between NFHS-4 (2015-16) and NFHS-5 (2019-21) is regressed on the district change in primary-LPG prevalence measured three ways: raw NFHS, regression-calibrated, and Bayesian-corrected. Entries are the estimate and 95% confidence interval of the change in adult prevalence (percentage points) per 10-percentage-point increase in primary-LPG prevalence, with the p-value; models adjust for the same socioeconomic and ambient covariates and region fixed effects as Table S6, clustered on state and unweighted, each district counted once (rural districts; N = " + N_ADULT + "). Two inference rules are shown for every row: CR2 with Satterthwaite degrees of freedom, which this paper reports as primary because the models are clustered on only " + nClus(R_I_BAY) + " states, and the conventional CR1 sandwich referred to t on the residual degrees of freedom, which is what a standard cluster-robust specification returns. The point estimate is identical under both and is printed once. Where the two disagree about a 0.05 threshold, the disagreement is a property of the inference rule, not of the exposure (SI Methods S6)."),
  mkTable([1400, 2000, 1900, 800, 1900, 800], [
    ["Outcome", "Exposure measurement",
     "Est. per 10-pp LPG [95% CI], CR2", "p (CR2)",
     "95% CI, conventional", "p (conv.)"],
  ].concat(adultRows())),
  p("Rural districts (N = " + N_ADULT + "); adult outcomes and corrected exposure both restricted to rural households. " + (HTN_VERIFIED ? "" : "Provisional: ") + "The outcome definitions are carried over from a prior study's analysis file; " + htnShort() + " (Section 2.4.7; definitions and verification in SI Methods S7). Respondents are women aged 15-49 and men aged 15-54, excluding currently pregnant women and respondents resident for under five years at their current address; hypertension is missing for respondents without the second and third blood-pressure readings or without an answer to the diagnosis or medication item. " + htnMissTxt() + " Unweighted district-mean rural prevalences of hypertension (measured blood pressure, self-reported diagnosis or medication), diabetes (composite: measured high glucose or self-report), and diabetes (self-report only) were " + adultPrev("hypertension_2015") + "%/" + adultPrev("hypertension_2019") + "%, " + adultPrev("diabetes_2015") + "%/" + adultPrev("diabetes_2019") + "%, and " + adultPrev("diabetes_sr_2015") + "%/" + adultPrev("diabetes_sr_2019") + "% at NFHS-4/NFHS-5 respectively. Diabetes is shown both as the composite indicator and, as a sensitivity, as the pure self-reported item; " + adultSigTxt() + ".", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S6. Exploratory analysis: district change-on-change association between primary-LPG prevalence and child mortality (2019 vs 2015), by exposure measurement, for rural districts (corrected exposure and mortality both restricted to rural births). These are the estimates plotted in SI Figure S21. Six exposure specifications are shown per outcome: raw NFHS; regression-calibrated; the era-matched Bayesian correction held fixed (ACCESS calibrates 2015, IRES calibrates 2019) - conditional on the fitted calibration, with an interval that excludes calibration uncertainty; that correction with calibration uncertainty carried by exposure-only multiple imputation (MI) - a sensitivity analysis at risk of attenuation, because the imputations do not condition on the outcome; a Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both rounds, so the corrected change does not switch instruments across rounds); and that instrument-consistent version with exposure-only MI. No row is a validated estimate with calibration uncertainty properly propagated (SI Methods S12). Models are weighted by the harmonic mean, across the two rounds, of the births eligible for each outcome; the corresponding unweighted fits are shown in the lower block of the same table. Models are adjusted for district changes in poverty (lowest national wealth quintile), women's lack of schooling, electricity, Muslim share, improved sanitation, improved water, ambient PM2.5, temperature, relative humidity, and drought, with region fixed effects, clustered on state; N = " + N_RURAL + " districts. Two inference rules are shown for every row: CR2 with Satterthwaite degrees of freedom, which this paper reports as primary because the models are clustered on only " + nClus(R_I_BAY) + " states, and the conventional CR1 sandwich referred to t on the residual degrees of freedom, which is what a standard cluster-robust specification returns. The point estimate is identical under both and is printed once. For the multiply-imputed (MI) rows the conventional column is the Rubin-pooled conventional clustered interval referred to a Normal distribution, and the CR2 column is pooled with Barnard-Rubin degrees of freedom. Where the two disagree about a 0.05 threshold, the disagreement is a property of the inference rule, not of the exposure (SI Methods S6). Units: deaths per 100 births per 10-percentage-point increase in LPG prevalence. As a further sensitivity, restricting each round's births to a shorter 36-month recent-cohort window (so the two mortality cohorts overlap far less in calendar time) gives a fixed-surface infant estimate of " + est10(NO_I_BAY) + " (95% CI " + ci10cr2(NO_I_BAY) + "; instrument-consistent " + est10(NO_I_IRES) + "; Table S6b). The corresponding all-household analysis is in Table S7."),
  table4,
  p("", {}),
  caption("Table S6b. Sensitivity analysis: the main child-mortality analysis of Table S6 with each round's births restricted to a shorter 36-month recent-cohort window, chosen to be shorter than the roughly four-year gap between rounds so that the 2015 and 2019 mortality cohorts overlap far less in calendar time; non-overlap is not enforced birth by birth (Section 2.4.5). Same exposure specifications, covariates, weighting and inference rules as Table S6; rural districts, N = " + N_NONOV + ". Units: deaths per 100 births per 10-percentage-point increase in LPG prevalence. The shorter window leaves fewer births per district; " + s6bPrecTxt() + "."),
  table4nonov,
  p("", {}),
  caption("Table S7. Sensitivity analysis: all-household version of the main child-mortality analysis (Table S6). District change-on-change association between primary-LPG prevalence and child mortality (2019 vs 2015), by exposure measurement, with district mortality estimated from all births (rather than rural births only) while the corrected exposure is unchanged. Adjusted for the same socioeconomic and ambient covariates and region fixed effects as Table S6, clustered on state; N = " + N_ALL + " districts. Two inference rules are shown for every row: CR2 with Satterthwaite degrees of freedom, which this paper reports as primary because the models are clustered on only " + nClus(R_I_BAY) + " states, and the conventional CR1 sandwich referred to t on the residual degrees of freedom, which is what a standard cluster-robust specification returns. The point estimate is identical under both and is printed once. For the multiply-imputed (MI) rows the conventional column is the Rubin-pooled conventional clustered interval referred to a Normal distribution, and the CR2 column is pooled with Barnard-Rubin degrees of freedom. Where the two disagree about a 0.05 threshold, the disagreement is a property of the inference rule, not of the exposure (SI Methods S6). Units: deaths per 100 births per 10-percentage-point increase in LPG prevalence. The same caveats as Table S6 apply: fixed-surface rows are conditional on the fitted calibration, and exposure-only MI rows are at risk of attenuation. The infant fixed-surface estimate " + allBayesPTxt() + " in this all-birth sample. The lower block of the table gives the corresponding unweighted fits."),
  table4all,
  p("", {}),

  caption("Table S8. Exploratory analysis: district change-on-change association between predicted fuel-use composition metrics and child mortality (2019 vs 2015), rural districts (N = " + N_NUANCE + "). For each mortality outcome, the corrected primary-LPG prevalence (the unweighted fit in the lower block of Table S6, shown for reference) is compared with the model-predicted district metrics from the augmentation step (Section 2.4.4): the LPG-no-solid-reported share, the LPG-and-solid-fuel (stacking) share, the any-solid-fuel share, and predicted annual LPG consumption. Entries are the point estimate and 95% CI per 10-percentage-point rise in each share, or per 10 kg/year for consumption, with the same covariate adjustment and state clustering as Table S6 but unweighted (each district counted once), under both inference rules. These metrics are model-predicted rather than measured and are strongly collinear (each is generated from the same corrected district-LPG context), so their individual coefficients are not separately interpretable; the table is hypothesis-generating only. Measured against the hypothesized directions (fewer deaths with more primary LPG, a larger LPG-no-solid-reported share and more LPG consumed; more deaths with a larger any-solid-fuel share; the stacking share has no single hypothesized direction), " + nuanceSignTxt() + ", and " + nuanceExcludingTxt() + ". We read that pattern as diagnostic of the limits of the prediction models rather than as evidence that fuel stacking is protective; together with the metrics' mutual collinearity it is why we draw no health conclusions from this table and retain corrected primary-LPG prevalence as the exposure of record."),
  mkTable([1200, 2200, 1900, 800, 1900, 800], [
    ["Outcome", "Predicted exposure metric",
     "Est. per 10 units [95% CI], CR2", "p (CR2)",
     "95% CI, conventional", "p (conv.)"],
  ].concat(nuancedRows("point"))),
  p("Units: deaths per 100 births per 10-percentage-point rise in each share (or per 10 kg/year for consumption). All metrics are estimated with a common specification (unweighted; the model-consistent proxy applies the IRES composition models to both rounds), so the primary-LPG reference row here differs from the birth-weighted main estimate in Table S6 (it is the unweighted Bayesian-corrected row of that table). With the four-category composition the any-solid-fuel share (solid-no-LPG + stacking) is not formally the complement of the LPG-no-solid-reported share, but the fourth 'other non-solid, non-LPG' category is so small that their district changes are almost exact complements (correlation " + nuCorr("change_excl_lpg", "change_any_solid") + "), so their coefficients are close to negations of each other and the two rows are effectively one metric.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S9. Predictive contribution of each covariate to the fuel-stacking prediction model (Section 2.4.4), computed separately in the two training surveys (ACCESS Wave 1; IRES rural). 'AUC drop' is the fall in leave-district-out cross-validated AUC when that covariate is removed from the household-covariate model - its unique out-of-sample contribution (larger = more important; a negative value means removal marginally improved held-out AUC). '|z|' is the absolute z-statistic of the covariate in the fitted logistic model with numeric predictors standardized (factors summarized by their largest-magnitude level), a measure of effect strength. The two columns are complementary: because the household covariates are correlated (wealth, household size, and electricity overlap), the drop-one AUC credits only each covariate's unique signal and so understates joint importance, whereas |z| reflects each covariate's fitted effect. The district LPG share is a district-level aggregate that cannot be honestly held out district-by-district, so its 'AUC drop' is the gain from adding it to the household-only model rather than a leave-one-out value. By standardized effect size (|z|), the within-state wealth quintile is the strongest single predictor in both surveys and household size the second (the drop-one AUC ranking differs slightly in IRES); no covariate is individually strong, consistent with the models' modest overall discrimination (leave-district-out AUC of the household-only model used for this table, " + viAuc("ACCESS W1") + " in ACCESS and " + viAuc("IRES rural") + " in IRES; the context-free leave-district-out validation model of SI Methods S2 is at " + ldo("ACCESS W1", "auc") + " and " + ldo("IRES rural", "auc") + "; the production model, which adds the district-context covariate and random effects, is not validated out of district) and with the argument that fuel stacking is poorly predicted by the standard covariates general health surveys carry."),
  mkTable([2500, 1450, 1150, 1450, 1150], [
    ["Covariate", "AUC drop (ACCESS)", "|z| (ACCESS)", "AUC drop (IRES)", "|z| (IRES)"],
  ].concat(tableS9rows)),
  p("* For the district LPG share the entry is the in-sample AUC gain from adding the district aggregate to the household-only model, not a leave-district-out drop; it is therefore optimistic and is not comparable with the rows above it. Education is absent from this table because the NFHS extract carries no populated attainment item (Table S1), so it is not a candidate predictor.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S10. Mean district primary-LPG prevalence for each estimator and correction, by round: an unweighted average across districts, each district counted once whatever its population, and so not a national household prevalence. For the NFHS-5 Bayesian correction (Bayesian-corrected minus raw multilevel estimate; the Bayesian model itself takes the design-weighted estimate as input), " + revTxt() + ". Regression calibration was fit two ways - on the multilevel and on the design-weighted raw NFHS estimate - to test sensitivity to the raw-estimator basis (Section 2.4.1; SI Figure S20)."),
  mkTable([4600, 1900, 1900], [
    ["Estimator / correction", "2015 (%)", "2019 (%)"],
  ].concat(tableS10rows)),
  p("Before PMUY (2015) the corrections are mild and all estimators lie within a few points; after PMUY (2019) the three corrections raise prevalence to about " + rng2(BAY19, emx(2019, "Reg.-calibrated (design-wt input)"), 0) + "%, from a raw " + rng2(RAW19, emx(2019, "Raw design-weighted"), 0) + "%. Across districts the two regression-calibration variants correlate r = " + emCor(2015, "rc_ml", "rc_wt") + " (2015) and " + emCor(2019, "rc_ml", "rc_wt") + " (2019); regression calibration and the Bayesian correction correlate r = " + emCor(2015, "rc_ml", "bayes") + " (2015) and " + emCor(2019, "rc_ml", "bayes") + " (2019), the lower post-PMUY value reflecting the Bayesian model's additional hierarchical shrinkage and propagated measurement error.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S11. Item missingness - the percent of households with a missing value - for the exposure and each harmonized covariate, in each survey's analytic household frame (NFHS-4 N = " + missN("NFHS-4") + "; NFHS-5 N = " + missN("NFHS-5") + "; ACCESS N = " + missN(MISS_ACCESS) + ", the pooled two-wave panel frame; IRES N = " + missN("IRES") + "). The primary-LPG exposure is complete in all four surveys."),
  mkTable([3400, 1450, 1450, 1350, 1000], [
    ["Variable", "NFHS-4", "NFHS-5", "ACCESS (both waves)", "IRES"],
  ].concat(tableS11rows)),
  p("Values are percent missing. Missingness is low throughout, the exceptions being caste (" + missPct1("NFHS-4", "Caste") + "% in NFHS-4 and " + missPct1("NFHS-5", "Caste") + "% in NFHS-5, from unresolved recode categories) and the IRES monthly-expenditure item (" + missPct1("IRES", "Wealth (index/expenditure)") + "%), which feeds only the within-state wealth quintile. Households missing a covariate still contribute to the primary-LPG exposure estimate (which uses the fuel item alone, complete for all households) and are handled by complete-case analysis in the covariate-based prediction models (Section 2.4.4). Beyond item missingness, " + L_SHARE + "% of NFHS-5 households were assigned to an NFHS-4 district (including the " + grp(L_NOGPS) + " in clusters released without GPS coordinates, which are assigned through their district), and all ACCESS and IRES households matched by census code. District-level estimates were produced for " + N_D_NFHS4 + " (NFHS-4) and " + N_D_NFHS5 + " (NFHS-5) districts. Recent-cohort censoring of the mortality outcome is by design (Section 2.4.5), not missing data.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S12. Transfer of the household fuel-stacking model across eras and across survey instruments (SI Methods S3), estimated on the six states ACCESS and IRES share, rural households only, with the district-context covariate removed so that no era-specific quantity enters the model. Each row trains the model in one survey and evaluates it in another. 'Contrast' names what changes between training and test: ERA holds the instrument fixed (ACCESS Wave 1 in 2014-15 to ACCESS Wave 2 in 2018), INSTRUMENT holds the era approximately fixed (IRES in 2019-20 to ACCESS Wave 2 in 2018), and ERA+INSTRUMENT changes both; it is the six-state counterpart of the cross-era test of SI Methods S2 and S7, which trains on all IRES states (the 'unrestricted IRES' rows). 'N hh' is the number of test-survey households; AUC is household-level discrimination; the calibration slope is the coefficient from regressing the observed outcome on the predicted log-odds, where 1 indicates predictions on the right scale and values near 0 indicate no usable signal; 'District r' is the Pearson correlation between predicted and observed district-level stacking share, the level at which the predictions are used. In-sample rows are shown only as a reference ceiling. 'Unrestricted IRES' rows drop the common-state restriction; 'transferable subset' rows refit using only the instrument-stable predictors whose removal does not by itself improve transfer (SI Methods S3)."),
  tableS12, p("", {}),

  caption("Table S13. Distribution of each transfer-model predictor across the three frames of SI Methods S3 (ACCESS Wave 1 2014-15, ACCESS Wave 2 2018, and rural IRES 2019-20), restricted to the six common states. Entries are means for continuous predictors and category shares for categorical ones. Reading left to right traces the era change and then the instrument change. Monotone movement across the three columns (ration-card holding, household size, electrification) is consistent with, but does not establish, population change over the transition; shifts as large as some of the caste shares, and a predictor that reverses direction precisely at the instrument change (the Other Backward Class share), are more consistent with the surveys classifying the same households differently."),
  tableS13, p("", {}),

  caption("Table S14. Contribution of each predictor to out-of-sample transfer of the stacking model (SI Methods S3), by refitting with that predictor dropped and re-evaluating in ACCESS Wave 1 (W1) and Wave 2 (W2). 'District r without it' is the district-level correlation the reduced model achieves; 'Contribution' is the loss relative to the full model, so a positive value means the predictor helps transfer and a negative value means dropping it improves transfer. With correlations this low the ordering is unstable and is reported to document that no reweighting of this covariate set produces a transportable model, not to identify which covariate to retain."),
  tableS14, p("", {}),

  caption("Table S15. Coefficient stability of the stacking model across the three contrasts of SI Methods S3. For each contrast and each model term, entries are the coefficient estimated in the first-named training survey (survey A) and in the second (survey B), their difference, and a two-sample z test of that difference on the log-odds scale. Because ACCESS is a panel, the Wave 1 and Wave 2 estimates in the ERA contrast share households and the test ignores any covariance between them, so these rows are approximate: overlapping households alone do not make them conservative, which would require a positive covariance between the two coefficient estimates, and that covariance is not estimated here. The instrument contrast is not era-pure - ACCESS Wave 2 (2018) and IRES (2019-20) are one to two years apart - so it will if anything overstate the instrument component."),
  tableS15, p("", {}),

  caption("Table S16. Stage A convergence pilot of the second outcome-conditioned imputation (SI Methods S12; failed convergence - not used for inference). Synthetic outcomes on the real design, " + STAGEA_N + " datasets; each fitted with four chains under each of four algorithm variants (state random intercept present or absent; effects of non-calibrated states updated state by state or as one block) at two run lengths (short: burn-in 1,000 sweeps, thinning 10; long: burn-in 4,000, thinning 40). A dataset is 'mixed' when every monitored quantity, including the completed-data coefficient, has rank-normalised split R-hat at most 1.05 and bulk ESS at least 100. The prespecified pass criterion, for the primary variant (state intercept, statewise updates, long run), was at least " + STAGEA_NEED + " of " + STAGEA_N + " datasets mixed and stability of the pooled estimate between run lengths in at least 95% of datasets; it failed on both (" + pctN(V_MIXED, 1) + " and " + pctN(V_STAB, 1) + "). Index acceptance is the share of proposed moves between saved calibration draws that were accepted."),
  tableS16, p("", {}),
  caption("Table S16a. Stage A mixed datasets by scenario cell (SI Methods S12). Columns: the primary variant at the long and short run lengths; block updates of the non-calibrated state effects, long run; no state intercept, statewise updates, long run. beta_1 is twice the fixed-surface infant estimate; S3b has a non-zero mean effect only."),
  tableS16a, p("", {}),
  caption("Table S16b. The first outcome-conditioned imputation (no state effect in the imputation model) on the real data: runs meeting the convergence rule (R-hat at most 1.05 and bulk ESS at least 100 on every monitored quantity, including the completed-data coefficient), by analysis (SI Methods S12; failed convergence - not used for inference). Index acceptance is the range across runs of the acceptance rate for moves between saved calibration draws; 'fewest distinct draws' is the smallest number of distinct calibration draws used across a run's imputations in either round. The precision-frontier row includes points at which calibration uncertainty is removed entirely, which pass trivially."),
  tableS16b, p("", {}),
  caption("Table S16c. Tuning attempts after Stage A (SI Methods S12), on synthetic data only, for the hardest cell (S2, calibration bias, with an exposure effect). Values for two datasets are separated by 'and'."),
  tableS16c, p("", {}),
  caption("Table S16d. History of the second-version specification (SI Methods S12): why Stage A has " + STAGEA_N + " datasets. Times are Mountain Daylight Time on 21 October 2026."),
  tableS16d, p("", {}),
  caption("Table S17. Where the posterior variance of a corrected district prevalence comes from (SI Methods S4), by calibration leg. Each corrected value is a + b x + u_s, with x the NFHS district estimate entering the measurement-error model as a covariate observed with known error; its posterior variance therefore splits into a calibration-side component, carried by the posterior of the intercept, slope and state effect, and an NFHS-side component equal to the squared calibration slope times the posterior variance of the district's true NFHS value (its sampling variance, reduced by empirical-Bayes shrinkage toward the state mean), which is a property of the health survey. Variances are on the logit scale and are averaged over the study districts. sigma is the fitted residual SD of the calibration; because the model also carries each reference district's own sampling standard error as a known variance, sigma is equation error - district-level disagreement left unexplained by the calibration, which may reflect instrument non-comparability among other sources - rather than reference-survey sampling noise. The decomposition is descriptive."),
  tableS17,
  p("", {}),

  ...(HAVE_PPD ? [
    caption("Table S18. Estimand sensitivity for the child-mortality models (SI Methods S5). The health models carry the calibrated {{expectation}} of each district's prevalence. Rows 1-3 of each block vary how that expectation is summarized: the point surface used for the fixed-surface rows of Table S6, the same posterior averaged on the probability rather than the logit scale (the Jensen check), and the exposure-only multiple imputation (a sensitivity analysis at risk of attenuation). Rows 4-5 replace the expectation with the posterior {{predictive}}, adding the calibration model's residual variance so that each imputation is a draw of the district's putative true prevalence. Entries are the reduction in deaths per 100 births per 10-percentage-point increase in LPG prevalence, with the 95% confidence interval and p-value under the primary inference rule of Table S6 (CR2 standard errors clustered on state with Satterthwaite degrees of freedom, pooled across imputations by Rubin's rules with Barnard-Rubin degrees of freedom), and the fraction of missing information. Rural districts, adjusted specification, weighted by the harmonic mean, across rounds, of the births eligible for each outcome. Nothing is refitted in the calibration: all rows are computed from the saved posterior draws."),
    tableS18,
    p("", {}),
  ] : []),

  ...(HAVE_NSSO ? nssoTablesSI : []),

  caption("Table S20. Survey sample sizes by state (SI Methods S9): the number of households, with the number of districts in parentheses, contributing to the rural district-level estimates in each survey, on the NFHS-4 (2011 Census) district frame. A dash indicates a state the survey did not sample. The last row gives the totals across all study districts."),
  tableS20, p("", {}),
  caption("Table S21. Linkage of each survey's households to the NFHS-4 district frame (SI Methods S9; main text Section 2.1). NFHS-4 households were assigned by DHS's own district code, NFHS-5 households through the name- and location-based crosswalk of Section 2.1; ACCESS, IRES and NSSO-78 households by census district code. Shares are percentages of the relevant household total."),
  tableS21, p("", {}),

  h1("Supplementary Figures"),
  img("maps/SI_1_reference_coverage.jpeg", 400, 355),
  caption("SI Figure S1. Geographic coverage of the reference energy surveys over the NFHS-4 district frame (the 640 districts of the 2011 Census). Each district is shaded by whether it was sampled by ACCESS only, IRES only, both, or neither; ACCESS covers " + cmpN(C4R) + " districts in six northern and eastern states and IRES " + cmpN(C5A) + " districts in " + I_STATES + " states. Districts sampled by neither survey receive a corrected exposure estimate only by extrapolation of the calibration and are flagged in the released data."),
  img("maps/SI_2_nfhs5_cluster_assignment.jpeg", 400, 355),
  caption("SI Figure S2. Assignment of NFHS-5 survey clusters to NFHS-4 (2011 Census) districts, urban and rural. Counts refer to all " + grp(L_CL5) + " NFHS-5 clusters; the map can show only the " + grp(lnk("NFHS-5 clusters with coordinates")) + " with published coordinates. Blue: clusters assigned by district name to a 2011 district of the same state (" + grp(L_NAME + (isFinite(L_DUP) ? L_DUP : 0)) + " clusters). Orange: clusters assigned by location - those in districts created after 2011, assigned to the 2011 district holding the majority of their nearest NFHS-4 neighbours in the same state, and the " + grp(L_OVER) + " name matches overridden by location (" + grp(L_LOC) + " clusters, " + pctOf5(L_LOC) + "%). Coordinates are displaced by the DHS Program by up to 2 km (urban) and 5 km (rural)."),
  img("maps/SI_2b_fallback_snap_distance.jpeg", 400, 355),
  caption("SI Figure S3. Why district boundaries were not used to assign clusters. Each NFHS-4 cluster with published coordinates is shown in the district polygon of the boundary file it falls in, coloured by whether that polygon is the cluster's own district as recorded by DHS (DHSREGCO). Only " + L_POLY4 + "% fall in their own district's polygon - far more disagreement than the DHS positional displacement (up to 2 km urban, 5 km rural) can explain - with whole districts misplaced in several states (for example Delhi, Manipur, Nagaland and the larger cities). Assignment therefore uses DHS's own district codes for NFHS-4 and a name- and neighbour-based crosswalk for NFHS-5 (Section 2.1)."),
  img("maps/SI_2d_fallback_share_by_district.jpeg", 400, 355),
  caption("SI Figure S4. District-level share of NFHS-5 households assigned by location rather than by district name (all households, urban and rural; grey areas have no district estimate). Districts with high shares are those split or renamed after 2011, whose NFHS-5 estimates combine the parts of the post-2011 districts assigned to them."),
  img("maps/SI_3_benchmark_scatter.jpeg", 460, 141),
  caption("SI Figure S5. Benchmark-variable agreement across surveys (rural districts). Each panel plots the NFHS district prevalence (vertical axis) against the reference-survey prevalence (horizontal axis) for a characteristic measured comparably in both surveys - Scheduled Caste, Scheduled Tribe, Scheduled Caste or Tribe combined, Hindu, Muslim, household electricity, and ration card - shown alongside primary LPG, for NFHS-4 versus ACCESS (top row) and NFHS-5 versus IRES (bottom row); the dashed line is the line of equality. On the NFHS-5/IRES pair, points close to the line for caste and religion, contrasted with the systematic off-line shift for primary LPG, indicate that the two surveys sample comparably composed populations and that the discrepancy is specific to the fuel item; electricity and the ration card are programme-linked items and are not clean benchmarks (Table S4)."),
  img("paper_figs/fig3_proxies.jpeg", 460, 212),
  caption("SI Figure S6. District exposure proxies from the fuel-use prediction models (secondary analysis, SI Methods S7), applied to all NFHS-5 households, urban and rural (this descriptive map is the one place the proxies are not restricted to the rural frame; the district composition proxies entering the health models of SI Methods S7 and Table S8 are rural in both rounds). (a) Predicted probability of fuel stacking among primary-LPG households, a conditional prediction at zero state and district random effects (so its levels sit well below the stacking shares observed in the energy surveys, Table S22, and the map shows relative rather than absolute stacking); (b) predicted share of households burning any solid fuel. These are model-predicted metrics with only moderate household-level discrimination (stacking AUC " + ldo("ACCESS W1", "auc") + " in ACCESS, " + ldo("IRES rural", "auc") + " in rural IRES) and limited temporal transportability (an IRES-trained stacking model shows r = " + CROSS_ERA_R + " against observed 2015 ACCESS district stacking); see SI Methods S2 and Table S8. Averaged over each state's districts (states with at least three districts), predicted stacking conditional on LPG is highest in " + stackStatesTxt(true) + " and lowest in " + stackStatesTxt(false) + ", and the expected share of households burning any solid fuel remains above one-half across much of central and eastern India even after correction of the primary-fuel prevalence."),
  img("fig_htn_rural.jpeg", 360, 320),
  caption("SI Figure S7. Rural district prevalence of hypertension at NFHS-5 (2019-21), from a null four-level multilevel model (respondent, cluster, district, and state). Hypertension is the mean of the second and third measured readings with systolic blood pressure at least 140 mmHg or diastolic at least 90 mmHg, or a self-reported diagnosis of high blood pressure, or current blood-pressure medication (SI Methods S7" + (HTN_VERIFIED ? "" : "; provisional") + "). This district surface is one of the outcomes for the change-on-change analysis in Table S5 and SI Figure S9."),
  img("fig_dm_rural.jpeg", 360, 320),
  caption("SI Figure S8. Rural district prevalence of diabetes at NFHS-5 (2019-21), estimated with the same four-level multilevel model as SI Figure S7. Diabetes is the composite item (measured high blood glucose or self-report of ever being told by a health professional of high blood sugar). This district surface is one of the outcomes for the change-on-change analysis in Table S5 and SI Figure S9."),
  img("fig_adult_rural.jpeg", 360, 383),
  caption("SI Figure S9. District change-on-change association between primary-LPG prevalence and adult cardiometabolic outcomes (change from NFHS-4 to NFHS-5), by exposure measurement, for rural districts. Points and horizontal bars are the point estimate and 95% CR2 confidence interval (Satterthwaite degrees of freedom, the primary inference of Table S5) of the change in adult prevalence (percentage points) per 10-percentage-point rise in primary-LPG prevalence, for hypertension, the diabetes composite, and self-reported diabetes, using raw, regression-calibrated, and Bayesian-corrected exposure; models adjust as in Table S5, clustered on state (" + N_ADULT + " districts). " + capFirst(adultSigTxt()) + "; with " + adultCsv.filter((x) => x.adjusted === "TRUE").length + " adult comparisons and no multiplicity adjustment, the figure is exploratory and is not read as evidence of an association."),
  img("fig_nuanced.jpeg", 400, 400),
  caption("SI Figure S10. Exploratory analysis (Table S8): district change-on-change association between predicted fuel-use composition metrics and child mortality, rural districts (N = " + N_NUANCE + "). For each mortality outcome the panel shows the corrected primary-LPG prevalence (the exposure of record) alongside the three model-predicted composition shares - the LPG-no-solid-fuel-reported share, the any-solid-fuel-reported share, and the LPG-and-solid-fuel (stacking) share - each oriented so that a move toward cleaner cooking points in the same direction (the stacking share, which has no single hypothesized direction in Table S8, is oriented so that less reported stacking counts as cleaner) (predicted LPG consumption is reported in Table S8 but is not plotted here because it is scaled per 10 kg/year rather than per 10 percentage points). Points and bars are point estimates and 95% CR2 confidence intervals (Satterthwaite degrees of freedom, the inference reported in Table S8) per 10-percentage-point move, with the same adjustment as Table S6, unweighted. Against the hypothesized directions, " + nuanceSignTxt() + " (Table S8). Because the shares are model-predicted rather than measured, and are mutually collinear, their individual coefficients are not separately interpretable; we read the pattern as diagnostic of the limits of the prediction models rather than as evidence that fuel stacking is protective, and the figure is hypothesis-generating only."),
  img("fig_health_energy.jpeg", 460, 300),
  caption("SI Figure S11. External check independent of the correction: cross-sectional (level-on-level) association between reference-survey energy metrics measured directly in the energy surveys and NFHS district child mortality, within each era-matched pair (ACCESS with NFHS-4; IRES with NFHS-5), rural districts. Each point is a district, sized by its reference-survey sample, with an unweighted least-squares line and 95% band; the vertical axis is NFHS district infant mortality (neonatal correlations are reported alongside) and the horizontal axis the reference-survey metric (primary-LPG prevalence, share burning any solid fuel, and fuel-stacking share). Pearson correlations are " + hePair("Any solid-fuel burning") + " for the share burning any solid fuel; " + hePair("Primary LPG") + " for primary-LPG prevalence; and " + hePair("Fuel stacking") + " for the fuel-stacking share. The expected directions are positive for solid-fuel burning and negative for primary-LPG prevalence; where a correlation above departs from that direction, the corroboration this check offers the corrected-exposure findings is correspondingly weaker. The ACCESS-era panels rest on " + cmpN(C4R) + " districts in six northern and eastern states. These associations are unadjusted and cross-sectional and are shown only as a directional corroboration."),
  h1("Supplementary district atlas"),
  p("To make every district-level quantity discussed in the paper visually inspectable, we provide a district atlas: choropleth maps of the NFHS covariates, the corrected and predicted exposure surfaces, the reference-survey prevalences, and a spatial version of the benchmark falsification check. All maps are drawn on the common NFHS-4 district geography by the released mapping scripts, which also generate the complete atlas - every covariate, exposure, composition, reference-survey, and health quantity, for each round and as the 2015-2019 change - beyond the representative panels reproduced here."),
  img("maps/atlas_panels/SES_levels_2019.jpeg", 460, 483),
  caption("SI Figure S12. District atlas of the NFHS-5 (2019-21) covariates entering the change-on-change health models, rural households: primary LPG and the socioeconomic adjustment covariates (poverty, women aged 15-49 with no schooling, household electricity, Muslim share, improved sanitation, improved water). Each panel is the null four-level multilevel district estimate; colour is the district prevalence (%). Analogous panels for NFHS-4 and for the 2015-2019 change are produced by the mapping scripts."),
  img("maps/atlas_panels/exposure_corrected.jpeg", 466, 326),
  caption("SI Figure S13. Corrected district surfaces of rural primary-LPG prevalence from both correction methods and both rounds: regression-calibrated and Bayesian measurement-error-corrected, for NFHS-4 (2015-16) and NFHS-5 (2019-21). The Bayesian NFHS-5 surface is the primary national product (also shown in main-text Figure 2); the corrected NFHS-4 surface outside the six ACCESS states is an extrapolation (Section 2.4.3)."),
  img("maps/atlas_panels/composition_2019.jpeg", 466, 326),
  caption("SI Figure S14. Predicted fuel-use composition for NFHS-5 (2019-21) rural households from the augmentation models (Section 2.4.4): the LPG-no-solid-reported share, the fuel-stacking share, the share of households reporting any solid fuel, and predicted annual LPG consumption (kg/yr). These are model-predicted district metrics and carry the caveats noted for SI Table S8."),
  img("maps/atlas_panels/reference_ACCESS.jpeg", 466, 326),
  caption("SI Figure S15. ACCESS reference-survey district prevalences (" + cmpN(C4R) + " districts in six northern and eastern states): primary LPG in Wave 1 (2014-15) and Wave 2 (2018), and Wave 1 fuel stacking, the LPG-no-solid-reported share, and annual LPG consumption (mean kg per household, non-users counted as zero). The mapped frame is the Wave 1 district set, so the three districts sampled only in Wave 2 do not appear. ACCESS sampled only rural households, so every panel is a rural estimate. Districts not sampled by ACCESS are shown in grey."),
  img("maps/atlas_panels/reference_IRES.jpeg", 466, 326),
  caption("SI Figure S16. IRES reference-survey district prevalences (2019-20; " + cmpN(C5A) + " districts in " + I_STATES + " states). Primary LPG is shown both for all sampled households and for the rural subsample (the rural estimate is the one used to calibrate NFHS-5, Section 2.4.3); fuel stacking, the LPG-no-solid-reported share, and annual LPG consumption (mean kg per household, non-users counted as zero) are estimated on all sampled IRES households. Districts not sampled by IRES are shown in grey."),
  img("fig_sbs_nfhs4_access.jpeg", 468, 370),
  caption("SI Figure S17. Spatial benchmark falsification, NFHS-4 versus ACCESS Wave 1 - the map analogue of SI Figure S5 - on the " + sbs("NFHS4_ACCESS", "lpg", "n_districts", 0) + " overlap districts, rural households in both surveys. Eight benchmark variables are shown as NFHS/reference pairs, two pairs per row, each pair on its own shared colour scale: primary LPG and Scheduled Caste (first row), Scheduled Tribe and Scheduled Caste or Tribe (second), Hindu share and Muslim share (third), household electricity and the BPL/Antyodaya ration card (fourth). Within every pair the NFHS panel is on the left and the reference panel on its right. Each NFHS panel is annotated with the Pearson correlation and the mean difference (NFHS minus reference, percentage points) across districts, computed from the design-weighted district estimates of both surveys, so they differ slightly from the multilevel-estimate values of SI Table S4. In this era religion and Scheduled Tribe composition agree closely (Scheduled Tribe r = " + sbs("NFHS4_ACCESS", "st", "pearson_r") + "), whereas the Scheduled Caste share and household electricity are higher in NFHS-4 (mean differences " + sbsDiff("NFHS4_ACCESS", "sc") + " and " + sbsDiff("NFHS4_ACCESS", "electricity") + " percentage points); primary LPG agrees well (r = " + sbs("NFHS4_ACCESS", "lpg", "pearson_r") + ", mean difference " + sbsDiff("NFHS4_ACCESS", "lpg") + " percentage points). This figure therefore establishes the benchmark baseline rather than the discrepancy; the divergence appears in the NFHS-5/IRES era (SI Figure S18). Scheduled Caste is the weakest of the demographic benchmarks here (r = " + sbs("NFHS4_ACCESS", "sc", "pearson_r") + "), reflecting the finer spatial texture of caste composition relative to the district samples."),
  img("fig_sbs_nfhs5_ires.jpeg", 468, 370),
  caption("SI Figure S18. Spatial benchmark falsification, NFHS-5 versus IRES, on the " + sbs("NFHS5_IRES", "lpg", "n_districts", 0) + " overlap districts, rural households in both surveys, in the same eight-variable layout as SI Figure S17 (NFHS/reference pairs, two pairs per row, in the same variable order; statistics from the design-weighted district estimates, as in SI Figure S17). Primary LPG shows NFHS-5 systematically below IRES (mean difference " + sbsDiff("NFHS5_IRES", "lpg") + " percentage points) (r = " + sbs("NFHS5_IRES", "lpg", "pearson_r") + "), whereas the census-like demographic benchmarks agree spatially and in level: Scheduled Tribe r = " + sbs("NFHS5_IRES", "st", "pearson_r") + " with a mean difference of " + sbsDiff("NFHS5_IRES", "st") + " points, Hindu share r = " + sbs("NFHS5_IRES", "hindu", "pearson_r") + ", Muslim share r = " + sbs("NFHS5_IRES", "muslim", "pearson_r") + ". That contrast is the visual form of the falsification argument: the surveys reach comparably composed populations and disagree specifically on the fuel item. Two benchmarks do not fit this clean pattern. The BPL/Antyodaya ration card differs by " + sbsDiff("NFHS5_IRES", "bpl") + " points (r = " + sbs("NFHS5_IRES", "bpl", "pearson_r") + ") and household electricity correlates only weakly across districts (r = " + sbs("NFHS5_IRES", "electricity", "pearson_r") + ") despite a small mean difference of " + sbsDiff("NFHS5_IRES", "electricity") + " points. Neither is a census-like attribute: both are programme-defined statuses whose measurement depends on how the question is posed, so they behave like the fuel item rather than like composition, and we treat them as instrument-sensitive benchmarks rather than as evidence of differing samples (Section 3.1)."),

  img("maps/SI_ml_vs_designwt.jpeg", 440, 440),
  caption("SI Figure S19. Multilevel versus design-weighted district primary-LPG prevalence, within each survey (NFHS-4 rural, NFHS-5 rural, ACCESS Wave 1, IRES). Each point is a district; the vertical axis is the unweighted multilevel small-area estimate (the primary specification) and the horizontal axis the design-weighted direct estimate (weighted proportion with Taylor-linearized standard error); the dashed line is equality. The two estimators agree closely (Pearson r " + mlwtRange("pearson_r") + "; Lin's CCC " + mlwtRange("ccc") + "), with a small mean difference between them in NFHS (" + mlwtDiffPP("NFHS-4 rural") + " percentage points for NFHS-4, " + mlwtDiffPP("NFHS-5 rural") + " for NFHS-5; multilevel minus design-weighted), partial pooling shrinking small-sample districts toward their state means. The close agreement confirms the NFHS-versus-reference comparison is not an artifact of the weighting or estimation choice."),

  img("maps/SI_estimator_mix.jpeg", 520, 312),
  caption("SI Figure S20. District primary-LPG prevalence from each alternative estimator and correction, plotted against the raw multilevel estimate, for NFHS-4 (top row) and NFHS-5 (bottom row): raw design-weighted; regression-calibrated fit on the multilevel input; regression-calibrated fit on the design-weighted input; and Bayesian measurement-error corrected. The dashed line is equality. Before PMUY (NFHS-4) all estimators lie near the line of equality (the correction is mild); after PMUY (NFHS-5) the three corrections lift district prevalence well above the raw estimate. Regression calibration is nearly identical whether fit on the multilevel or the design-weighted raw estimate (district correlation " + emCor(2015, "rc_ml", "rc_wt") + " in 2015 and " + emCor(2019, "rc_ml", "rc_wt") + " in 2019; Table S10), so the correction does not depend on the raw-estimator basis; the Bayesian correction lands at a similar unweighted district mean (" + BAY19_0 + "% in 2019, against " + RC19_0 + "% for regression calibration) but reorders districts somewhat more, owing to its hierarchical shrinkage and propagated measurement error."),

  img("fig_mort_rural.jpeg", 380, 380),
  caption("SI Figure S21. Exposure measurement and the LPG-child-mortality association (rural districts; exploratory, Section 3.5 and SI Methods S6). Points and horizontal bars are the point estimate and 95% CR2/Satterthwaite interval of the change in mortality (deaths per 100 births) per 10-percentage-point rise in primary-LPG prevalence, for neonatal and infant mortality, across eight exposure specifications: raw NFHS exposure; regression-calibrated; Bayesian-corrected (era-matched) held fixed; the Bayesian correction with exposure-only multiple imputation; the Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both rounds); that instrument-consistent version with exposure-only imputation; and the two corresponding variants of the symmetric NFHS-4 refit of SI Methods S1, in which the ACCESS reference is the design-weighted direct estimate carrying its own Taylor-linearized standard error. SI Table S6 reports the first six. Fixed-surface estimates are conditional on the fitted calibration and their intervals exclude calibration uncertainty; exposure-only imputation estimates include it but are at risk of attenuation (SI Methods S12). Models are weighted by the harmonic mean, across rounds, of the births eligible for each outcome and adjust for concurrent socioeconomic and ambient (PM2.5, temperature, relative humidity, drought) covariates with region fixed effects and state-clustered SEs (" + N_RURAL + " districts)."),

  ...(HAVE_NSSO ? nssoFigsSI : []),

  h1("References cited only in the Supplementary Materials"),
  p("All other works cited in the Supplementary Materials are listed in the main-text reference list.", { run: { italics: true } }),
  p("Duan N. 1983. Smearing estimate: a nonparametric retransformation method. Journal of the American Statistical Association 78:605-610."),
  p("Cole SR, Chu H, Greenland S. 2006. Multiple-imputation for measurement-error correction. International Journal of Epidemiology 35:1074-1081."),
  p("Freedman LS, Midthune D, Carroll RJ, Kipnis V. 2008. A comparison of regression calibration, moment reconstruction and imputation for adjusting for covariate measurement error in regression. Statistics in Medicine 27:5195-5216."),

  h1("Supplementary Materials: variable inventory"),
  p("NFHS-4 and NFHS-5: DHS Household Recode items", { run: { bold: true } }),
  ...suppNfhs.map((t, i) => p(`(${i + 1}) ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 60 } }, run: { size: 22 } })),
  p("ACCESS: energy and cooking-fuel items", { run: { bold: true } }),
  ...suppAccessEnergy.map((t, i) => p(`(${i + 1}) ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 60 } }, run: { size: 22 } })),
  p("ACCESS: socioeconomic and cylinder items", { run: { bold: true } }),
  ...suppAccess.map((t, i) => p(`(${i + 1}) ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 60 } }, run: { size: 22 } })),
  p("IRES: cooking-fuel and additional items", { run: { bold: true } }),
  ...suppIres.map((t, i) => p(`(${i + 1}) ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 60 } }, run: { size: 22 } })),
];

// Re-read the written main document and require first citations in order.
const checkCitationOrder = (buf) => {
  let JSZip;
  try { JSZip = require("jszip"); } catch (e) {
    console.warn("[build] jszip not available; citation order NOT verified.");
    return null;
  }
  return JSZip.loadAsync(buf).then((z) => z.file("word/document.xml").async("string")).then((xml) => {
    const txt = xml.replace(/<\/w:p>/g, "\n").replace(/<[^>]+>/g, "");
    const body = txt.slice(0, txt.lastIndexOf("\nReferences\n"));
    const seen = [];
    (body.match(/\[(\d+(?:\s*,\s*\d+)*)\]/g) || []).forEach((m) =>
      m.slice(1, -1).split(/\s*,\s*/).map(Number).forEach((n) => { if (!seen.includes(n)) seen.push(n); }));
    const bad = seen.findIndex((n, i) => n !== i + 1);
    if (bad >= 0) throw new Error("citation order: first citations in the main text run " + seen.slice(0, bad + 3).join(", ") + " ... (expected 1, 2, 3, ...); update REF_ORDER");
    console.log("[build] citation order verified: " + seen.length + " references first cited in order 1-" + seen.length + " in the main text; " + (refs.length - seen.length) + " cited only in the SI");
  });
};
const mkDoc = (children) => new Document({
  styles: { default: { document: { run: { font: FONT, size: SZ } } } },
  sections: [{ properties: { page: { size: { width: 12240, height: 15840 },
    margin: { top: 1440, bottom: 1440, left: 1440, right: 1440 } } }, children }],
});
// Write, then report the absolute path actually written. The caller verifies
// freshness by mtime, so an unhelpfully quiet success is worse than useless:
// naming the file lets a wrong-folder build be spotted in the log rather than
// three steps later.
Promise.all([
  Packer.toBuffer(mkDoc(mainChildren)).then((buf) => {
    fs.writeFileSync(OUT + "ACCESS_Health_main.docx", buf);
    console.log("Wrote " + OUT + "ACCESS_Health_main.docx");
    return checkCitationOrder(buf);
  }),
  Packer.toBuffer(mkDoc(siChildren)).then((buf) => {
    fs.writeFileSync(OUT + "ACCESS_Health_SI.docx", buf);
    console.log("Wrote " + OUT + "ACCESS_Health_SI.docx");
  }),
]).catch((e) => {                 // an unhandled rejection would exit 0 in some
  console.error("BUILD FAILED: " + e.message);   // Node versions -- exit nonzero
  process.exit(1);                               // so the R caller sees it.
});
