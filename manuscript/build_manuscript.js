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

const p = (text, opts = {}) =>
  new Paragraph({
    spacing: { line: LINE, after: 160 },
    alignment: AlignmentType.JUSTIFIED, ...opts.par,
    children: Array.isArray(text) ? text
      : HAS_EM.test(text) ? emphasize(text, opts.run)
      : [new TextRun({ text, font: FONT, size: SZ, ...opts.run })],
  });
const r = (t, e = {}) => new TextRun({ text: t, font: FONT, size: SZ, ...e });

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
function readCsv(rel, opt) {
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
  return v < 0.001 ? "<0.001" : v.toFixed(3);
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

// Predictors whose coefficient is statistically indistinguishable between the
// two training surveys of a contrast AND keeps its sign. 21 writes `transfers`
// as an R logical, so it arrives as the string "TRUE".
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
let subFit = modelFits.find((r) => SUB_RE.test(r.model || ""));
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
if (!ldoCsv) console.warn("[build] PINNED (not CSV-derived): production stacking "
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
const heRural = readCsv("health_effects_table.csv");
const heAll   = readCsv("health_effects_table_all.csv");

// The six exposure specifications, in the order the text discusses them.
const EXPO_SPECS = [
  ["change_lpg_raw",               "Raw NFHS"],
  ["change_lpg_rc",                "Regression-calibrated"],
  ["change_lpg_bayes",             "Bayesian-corrected"],
  ["change_lpg_bayes_MI",          "Bayesian + uncertainty (MI)"],
  ["change_lpg_bayes_iresonly",    "Bayesian, one instrument (IRES) both rounds"],
  ["change_lpg_bayes_iresonly_MI", "Bayesian, one instrument + uncertainty (MI)"],
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
      if (r) out.push([OUTCOME_LAB[oc], sp[1], ciCell(r), pfmt(r.p)]);
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
      if (r) out.push([OUTCOME_LAB[oc], sp[1], ciCell(r), pfmt(r.p)]);
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
const heNonov = readCsv("health_effects_table_nonoverlap.csv");

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

// ---- ACCESS LPG consumption, taken from the pipeline's own range check ----
// 02_prep_access.R writes the median/mean/n of annual consumption among LPG
// users into the check log; parsing it there is still reading the analysis
// output rather than re-typing a number that a re-run could change.
//
// The check log is truncated at the start of every run_everything.R run, and
// the previous run is kept as prev_pipeline_checks.csv. A partial re-run
// (START_FROM=05_correction.R, say) therefore produces a log with no step-02
// rows at all, and reading only the current log would silently push "NA" into
// the manuscript for numbers that never changed. We fall back to the archived
// log in that case, and say loudly that we did -- the alternative failure
// (a fresh full run that genuinely lost the check) would then be visible as a
// build-time warning rather than as an NA nobody traced back to its cause.
const KG_RE = /physically plausible band/;
const pipeChecks = readCsv("diagnostics/pipeline_checks.csv");
let kgRow = pipeChecks.find((r) => KG_RE.test(r.check || ""));
if (!kgRow) {
  const prevChecks = readCsv("diagnostics/prev_pipeline_checks.csv", { quiet: true });
  kgRow = prevChecks.find((r) => KG_RE.test(r.check || ""));
  console.warn("[build] FALLBACK: the ACCESS LPG-consumption figures come from "
    + "diagnostics/prev_pipeline_checks.csv, not the current log. The current "
    + "run did not execute 02_prep_access.R (a START_FROM= partial re-run), so "
    + "its check row is absent. These figures are ACCESS-side and unaffected by "
    + "downstream steps"
    + (kgRow ? "." : ", but they were NOT found in the archived log either -- "
        + "they will read NA. Re-run the pipeline from 02_prep_access.R."));
}
kgRow = kgRow || {};
const kgNum = (re) => { const m = re.exec(kgRow.detail || ""); return m ? m[1] : "NA"; };
const KG_N_RAW  = kgNum(/n = ([0-9,]+)/);
const KG_N      = KG_N_RAW === "NA" ? "NA"
  : Number(KG_N_RAW.replace(/,/g, "")).toLocaleString("en-US");
const KG_MEDIAN = kgNum(/median = ([0-9.]+)/);
const KG_MEAN   = kgNum(/mean = ([0-9.]+)/);

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
      if (r) out.push([OUTCOME_LAB[oc], sp[1], ciCell(r), pfmt(r.p)]);
    });
  });
  return out;
};
const adultPrevCsv = readCsv("si_adult_health_prevalence.csv");
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
      out.push([OUTCOME_LAB[oc], r.exposure + per,
                nfmt(r.est_per10, 3) + " [" + nfmt(r.lo, 3) + ", " + nfmt(r.hi, 3) + "]",
                pfmt(r.p)]);
    });
  });
  return out;
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
  return dvTxt + ", " + rhTxt + ", " + essTxt + ".";
};

// The chain count and post-warmup draw total in Section 2.4.3 are properties of
// 05_correction.R's sampler call (chains = 4, iter = 12000, warmup = iter/2), not
// of any CSV, so they are pinned here and must be updated alongside that script.
console.warn("[build] PINNED (not CSV-derived): MCMC chain count (4) and "
  + "post-warmup draw total (24,000). These are sampler settings read from "
  + "05_correction.R (chains = 4, iter = 12000, default warmup = iter/2), not "
  + "values written to disk. Change them there and they must be changed here.");

// ---- NFHS-5 clusters that could not be assigned to a 2011 district ----
const fbClust = readCsv("nfhs5_fallback_clusters.csv");
const fbExcl = fbClust
  .filter((r) => String(r.assigned).toUpperCase() === "FALSE")
  .map((r) => Number(r.snap_km)).filter((v) => isFinite(v))
  .sort((a, b) => a - b);
const fbAt = (q) => fbExcl.length
  ? fbExcl[Math.min(fbExcl.length - 1, Math.floor(q * (fbExcl.length - 1)))] : NaN;
const EXCL_MIN = nfmt(fbExcl[0], 1);
const EXCL_MAX = nfmt(fbExcl[fbExcl.length - 1], 1);
const EXCL_MED = nfmt(fbAt(0.5), 1);
// Median snap distance among the clusters that WERE assigned (<= 10 km).
const fbKeep = fbClust
  .filter((r) => String(r.assigned).toUpperCase() === "TRUE")
  .map((r) => Number(r.snap_km)).filter((v) => isFinite(v))
  .sort((a, b) => a - b);
const SNAP_MED = fbKeep.length
  ? nfmt(fbKeep[Math.floor((fbKeep.length - 1) / 2)], 1) : "NA";

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
const L_GPS = lnk("clusters with GPS, total");
const L_PIP = lnk("clusters assigned by point-in-polygon");
const L_SNAP = lnk("clusters snapped to nearest district (<= 10 km)");
const L_EXCL = lnk("clusters excluded (nearest district > 10 km)");
const L_SNAP_HH = lnk("households retained via nearest-district snap");
const L_EXCL_HH = lnk("households excluded (cluster > snap threshold)");
const L_NOGPS = lnk("households with unrecorded GPS (lat = lon = 0 in DHS release; excluded)");
const L_SHARE = nfmt(lnk("share of GPS households assigned") * 100, 1);
const pctOfGps = (a, d = 1) => nfmt((a / L_GPS) * 100, d);

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
// re-lists the Wave 1 sample; 86% is a fielded-sample retention rate reported by
// the survey team in Jain et al. 2015 (reference [13]), the ACCESS 2015 survey
// documentation. It is therefore pinned to that one published source, not
// derived, and the manuscript cites [13] alone for it,
// and says so here rather than passing silently as if it came from the data.
const A_RETENTION = "86%";
console.warn("[build] PINNED (not CSV-derived): ACCESS panel retention ("
  + A_RETENTION + "). This is the fielded-sample retention reported in the ACCESS "
  + "2015 survey documentation (Jain et al. 2015, reference [13]) and the "
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
if (!bcTab.length) console.warn("[build] PINNED (not CSV-derived): eligible-birth "
  + "counts in Section 2.4.5. Re-run H1_prep_mortality.R to generate "
  + "mortality_birth_counts.csv and source them automatically.");
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
  ["Use of multiple fuels (firewood, dung, agricultural residue, coal)", "No", "Yes", "Yes"],
  ["LPG named with no solid fuel named", "No", "Derived", "Yes (direct item; derived measure used)"],
  ["LPG cylinder refills (quantity of use)", "No", "Yes", "Yes"],
  ["PMUY beneficiary status", "No", "Yes", "Yes"],
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
  console.warn("[build] PINNED (not CSV-derived): measurement-error SE "
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
  console.warn("[build] PINNED (not CSV-derived): Bayesian calibration slope, "
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
  const rows = legAMoveTab.filter((r) => r.comparison === "MI (primary)");
  const zs = rows.map((r) => Math.abs(Number(r.diff_in_primary_se)))
                 .filter((z) => isFinite(z));
  return zs.length ? Math.max.apply(null, zs) : NaN;
})();
const legAVerDICT = !isFinite(legAMaxZ) ? "could not be evaluated"
  : legAMaxZ < 0.25 ? "leaves the child-mortality estimates essentially unchanged"
  : legAMaxZ < 0.5  ? "moves the child-mortality estimates by well under half of their own standard errors"
  : "moves the child-mortality estimates materially, and the primary specification should be read with that in mind";
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
  p("The Demographic and Health Surveys (DHS), including India's National Family Health Survey (NFHS), were designed to measure maternal and child health, but their national scale and their linkage of exposures to health outcomes have made them one of the few instruments available for population health research in low- and middle-income countries, and researchers have repurposed them well beyond their original intent. Environmental epidemiology is a prominent example: studies of household air pollution routinely assign exposure from the NFHS cooking-fuel item, which records only a household's primary cooking fuel. Whether such a simplified item adequately represents environmental exposure - and, if not, whether it can be corrected using better-measured data - is a general question for exposure assessment; clean cooking in India, where a single survey item defines cooking exposure for 1.4 billion people amid rapid policy change, provides an unusually clear test case."),
  p("India is undergoing the world's largest clean-cooking transition. The flagship Pradhan Mantri Ujjwala Yojana (PMUY) has released more than 106 million subsidized LPG connections to poor households since 2016 [5], and both NFHS and the independent 78th round of the National Sample Survey register the shift: primary clean-fuel use rose from 43.8% in NFHS-4 (2015-16) to 58.6% in NFHS-5 (2019-21) [6], and the latter reported 62% of households using LPG as their primary fuel in 2020-21 [7]; two national surveys built on different questionnaires agreeing on the trend indicates it is real, even if they differ on its level. Household air pollution (HAP) from solid cooking fuels remains one of the largest environmental health risks worldwide - responsible for roughly 2.9 million premature deaths in 2021 [1] and the largest absolute burden in India [2], with kitchen PM2.5 far exceeding air-quality guidelines and household combustion a major source of ambient particulate matter across South Asia [3, 4] - so whether this expansion has improved health is a first-order question. Demonstrating such gains, however, has proved difficult: reviews of the epidemiological evidence, including randomized trials of clean-fuel interventions, report largely null or modest effects [38]."),
  p("That difficulty is, in part, a measurement problem: a single clean-versus-polluting classification cannot represent how the transition actually unfolds. The programs drove adoption far more than sustained use, so binary access measures overstate health-relevant progress [35]; a difference-in-differences analysis finds India's below-poverty-line policies, including PMUY, raised the probability of any LPG use by about eight percentage points but left the quantity consumed by using households essentially unchanged [36], and PMUY beneficiaries in particular use roughly 30% less LPG than general customers and take few refills a year [9]. Rather than climbing a clean rung of an energy ladder, households occupy an overlapping 'energy stack': even as LPG ownership expanded across energy-poor northern states, most kept burning solid fuels, the share stacking LPG and biomass rose from 17% to 38% between 2015 and 2018, and exclusive clean-fuel use reached only 17% [8, 30, 31]; a third to a half of households reporting LPG as their primary fuel still burn a solid fuel at least daily and often change which fuel they call primary across seasons [32], and clean-fuel use rises with socioeconomic status while many rural households never switch cleanly [37]. Because household PM2.5 responds nonlinearly to residual solid-fuel use, such partial displacement yields far smaller exposure reductions than the binary label implies, so a household recorded as clean may be exposed much like a solid-fuel household [10, 11]. The NFHS item compounds this: reported by a single respondent without probing of secondary fuels or quantities, it may misclassify households in ways that vary with wealth, caste, and geography, attenuating and biasing estimated health effects of clean cooking and of programs such as PMUY [12]."),
  p("The measurements that reveal these patterns come from dedicated household energy surveys, which capture the full fuel portfolio but only in limited areas. The Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS) surveyed " + A_W1_HH + " rural households in six energy-poor northern states in 2014-15 and " + A_W2_HH + " in 2018 [13, 14], and the India Residential Energy Survey (IRES, 2019-20) covered " + I_HH + " households across " + I_DIST + " districts in " + I_STATES + " states [15]. Both record fuel stacking, cylinder refills, and consumption in detail, but neither collects health outcomes or supports nationwide district-level health analysis - the very strengths of NFHS."),
  p("Correcting the NFHS cooking-fuel measure is not an end in itself but a prerequisite for using the survey to evaluate the transition: if the item misclassifies the households the program most affects, then every downstream quantity built on it - the national count of clean-cooking households, the modeled burden of household air pollution, and the estimated health return to programs such as PMUY - inherits that error, and the direction and magnitude of the resulting bias cannot be assumed without knowing how the item fails. In this study we therefore use these energy surveys to evaluate, calibrate, and augment the NFHS exposure. This paper is organized around three questions: does NFHS mismeasure clean-cooking exposure, can specialized surveys correct that measurement error, and what happens when calibrated exposure products are analysed as though they were observed rather than estimated? The third question is not specific to cooking fuel. Calibrated, modeled and predicted exposure surfaces - satellite and model-based fine particulate matter fields, land-use regression and noise models, machine-learning exposure predictions, statistically downscaled climate reanalyses - are published as point estimates and routinely joined to health data as though they had been measured, and what that step costs the resulting inference is rarely examined. We address the three questions in turn. First, we ask whether the NFHS cooking-fuel measure is accurate: we estimate district-level primary-LPG prevalence in NFHS-4, NFHS-5, ACCESS, and IRES with a common multilevel small-area framework, test where NFHS and the energy surveys agree for the two temporally overlapping pairs - NFHS-4 with ACCESS, NFHS-5 with IRES - and, where they disagree, ask whether the gap can be attributed to sampling, timing, geography, or demographics rather than to the fuel question itself. Second, treating the energy surveys as higher-fidelity references - they establish each household's full fuel portfolio within a dedicated energy module before asking which fuel is primary - we correct the NFHS district estimates by regression calibration and a Bayesian measurement-error model, producing corrected, uncertainty-quantified prevalence for every district and validating the correction out of sample. Third, we decompose the corrected exposure's uncertainty into the part a better validation survey could reduce and the part belonging to the health survey itself, and we illustrate what the distinction costs by carrying the corrected exposure into a district change-on-change analysis of child mortality - once held fixed at its posterior mean, as a downstream user of a published surface would, and once with the correction's own uncertainty propagated. As a secondary analysis we also augment NFHS with fuel-use detail it does not collect, predicting fuel stacking and LPG consumption from the energy surveys. The paper therefore makes two contributions, and we regard them as of equal weight. The first is empirical: the first systematic district-level validation of NFHS clean-cooking indicators against purpose-built energy surveys, which shows that a single, heavily used survey item changed its relationship to household fuel use over the period a national program was changing that use, establishes by falsification that the change is confined to the fuel question rather than to who was surveyed, and delivers corrected, uncertainty-quantified exposure surfaces suitable for household air pollution epidemiology and burden estimation. The second is methodological: an account of what a calibrated exposure product is, how much of its uncertainty an external validation survey can and cannot remove, and what follows for any analysis that adopts one as an observation - a framework that transfers to electrification, water, sanitation and other environmental exposures, and to exposure products built without a validation survey at all."),
];

const methodsOverview = [
  p("We combine four household surveys in three analytic stages: (i) common district-level estimation of primary-LPG prevalence in each survey; (ii) cross-survey comparison and measurement-error correction of the NFHS estimates; and (iii) prediction of fuel stacking and LPG consumption in NFHS from models trained in the energy surveys. All analyses are conducted on the NFHS-4 district geography (the 640 districts of the 2011 Census, as delineated in the NFHS-4 district shapefile)."),
  p("Because NFHS-5 reports districts on a later administrative frame, NFHS-5 households were assigned to NFHS-4 districts spatially: each cluster's published GPS location (randomly displaced by the DHS Program by up to 2 km for urban clusters and 5 km for rural clusters, with 1% of rural clusters displaced up to 10 km) was overlaid on the NFHS-4 district polygons by point-in-polygon. Of " + grp(L_GPS) + " GPS-located NFHS-5 clusters, " + grp(L_PIP) + " (" + pctOfGps(L_PIP) + "%) fell within a district polygon. A further " + grp(L_SNAP) + " clusters (" + pctOfGps(L_SNAP) + "%; " + grp(L_SNAP_HH) + " households) fell within 10 km of a district polygon - a distance explainable by the DHS displacement - and were assigned to the nearest district (median snap distance " + SNAP_MED + " km; SI Figures S2-S3). The remaining " + grp(L_EXCL) + " clusters (" + pctOfGps(L_EXCL) + "%; " + grp(L_EXCL_HH) + " households) lay more than 10 km from any polygon and were excluded; their distances to the nearest polygon ranged from " + EXCL_MIN + " to " + EXCL_MAX + " km (median " + EXCL_MED + " km), far beyond what the DHS displacement can explain, and because India's districts tile the mainland these are necessarily locations offshore or beyond an international boundary relative to the 2011 frame. Households in the " + grp(L_NOGPS) + " records with unrecorded GPS (released as latitude = longitude = 0) were also excluded. In total, " + L_SHARE + "% of GPS-located NFHS-5 households were assigned. Cluster displacement may misassign a small number of border-adjacent clusters, a conservative source of noise in the district estimates. ACCESS households were linked to NFHS-4 districts deterministically through the 2011 Census district codes carried in the ACCESS data (" + dg(accLnk, "households matched to an NFHS-4 district (by census code)") + " of " + dg(accLnk, "ACCESS households (both waves)") + " households; all " + dg(accLnk, "ACCESS districts, total") + " districts). IRES households were linked by census district code, with state-aware district-name matching as a fallback; the survey\u0027s " + I_SU + " sampling units map to " + I_DIST + " census districts (Nadia, West Bengal, was sampled as two units), and all " + dg(iresLnk, "IRES households, total") + " households were linked. State names were harmonized across sources. Linkage diagnostics are tabulated in SI Table S1 and mapped in SI Figures S1-S3."),
  p("Because ACCESS sampled only rural households, all head-to-head comparisons restrict NFHS to rural clusters (the NFHS-5/IRES comparison is additionally reported for all households). For the same reason, the measurement-error correction (Section 2.4.3), the fuel-use prediction (Section 2.4.4), and the child-mortality and adult-outcome analyses (Sections 2.4.5-2.4.6) are all conducted on rural households and constitute the main analysis of this paper; all-household versions are reported in the Supplementary Materials as sensitivity analyses. Throughout, \"rural\" therefore denotes the main analysis and \"all-household\" the supplementary sensitivity. Analyses of ACCESS Wave 2 (2018) are reported descriptively only, as no NFHS round overlaps it in time."),
];

const nfhsText = [
  p("Data from the NFHS-4 survey conducted between January 2015 and November 2016 [16] and the NFHS-5 survey conducted between June 2019 and April 2021 [6] were used. NFHS are nationally representative household sample surveys measuring indicators of socioeconomic status, demographics, health, and nutrition, with special emphasis on maternal and child health. They have a two-stage design, in which clusters (villages in rural areas and census enumeration blocks in urban areas) are first selected from each district (640 districts at the time of the 2011 census for NFHS-4; 707 districts as of 2017 for NFHS-5), and then 25-30 households are selected by equal-probability systematic sampling in each selected cluster; women of reproductive age (15-49 years) and men (15-54 years) are then selected from those households for in-depth interviews [17]. We use the household recode files, household GPS cluster locations, and household sampling weights."),
];

const accessText = [
  p("The Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS) surveys were conducted in 2014-15 and 2018 [13, 14]. The two waves were administered in six energy-poor contiguous states of India: Bihar, Jharkhand, Uttar Pradesh, Odisha, Madhya Pradesh, and West Bengal; these northern states have historically been energy-poor and combined account for about 500 million individuals, roughly 40% of the country's population. Wave 1 was administered to " + A_W1_HH + " households between November 2014 and May 2015, and Wave 2 to " + A_W2_HH + " households between April 2018 and September 2018; the survey documentation reports " + A_RETENTION + " retention of Wave 1 households across the two waves [13]. The panel includes " + A_PANEL + " household-wave observations. The appended two-wave panel release analysed here contains " + A_W1_HH + " Wave 1 households, marginally fewer than the 8,568 reported in the published ACCESS 2015 report and the 8,566 records in the standalone 2015 microdata release; the counts reported throughout describe the file analysed."),
  p("Sampling followed a three-stage probability-proportional-to-size (PPS) design. One district was sampled from each administrative division of each state (two per division in West Bengal), for " + A_W1_D + " districts in Wave 1 and " + A_W2_D + " in Wave 2, each chosen with probability proportional to population. Within each district, villages were split into two strata (large and small villages) comprising equal numbers of rural households; seven villages were sampled from each stratum with probability proportional to population, and twelve households were sampled at random within each village. This stratified design is approximately self-weighting within districts; village-level sampling weights from the survey's replication materials [14] are applied in design-weighted sensitivity analyses. The 45-minute questionnaire covered socioeconomic information, electricity access and satisfaction, cooking energy access and satisfaction, energy policy preferences, and willingness to pay; primary cooks were interviewed or present for the cooking modules."),
];

const iresText = [
  p("The India Residential Energy Survey (IRES), conducted by the Council on Energy, Environment and Water (CEEW), is a large-scale, nationally representative household survey of energy access and consumption [15, 23]. Fieldwork was conducted between November 2019 and March 2020, with the majority of interviews administered in December 2019 through February 2020 [23]; interview dates in the analytic file confirm this window (98.0% of interviews predate March 2020 and 99.98% fall within November 2019-March 2020, with three records carrying out-of-window dates). The entire IRES fieldwork therefore precedes the COVID-19 lockdown and all of NFHS-5's phase-2 fieldwork (November 2020-April 2021). The survey covered " + I_HH + " urban and rural households (" + I_RURAL + "% rural) spanning " + I_DIST + " districts (" + I_SU + " district sampling units) in " + I_STATES + " states, together comprising 97% of India's population. IRES employs a stratified multistage probability design: within each state, districts were organized into two equal-population strata and two districts sampled per stratum with probability proportional to size; within each district, 12 villages/urban wards were sampled in proportion to the rural and urban population, and eight households were randomly sampled per village/ward (96 households per district). Interviews were conducted in person by trained enumerators using handheld tablets. The average non-response rate was 26% (34% urban, 21% rural) and was higher in districts with more wealthy households; CEEW's survey weights are design (base) weights reflecting selection probabilities, without non-response or post-stratification adjustment [23]. District-level design weights are applied in our design-weighted analyses."),
];

const measuresText = [
  p("Table S1 (Supplementary Materials) summarizes the measures used from each survey. The primary exposure indicator common to all surveys is use of LPG as the main cooking fuel. In NFHS this is derived from the household main-fuel item; in ACCESS from the primary cooking fuel module; and in IRES from the primary cooking fuel item (LPG or piped natural gas). A broader 'clean primary fuel' indicator (electricity, LPG/PNG, biogas) is used in sensitivity analyses."),
  p([
    r("From the energy surveys we construct three fuel-use outcomes unavailable in NFHS. "),
    r("Fuel stacking", { italics: true }),
    r(" is defined among households whose main fuel is LPG as continued use of any solid fuel (firewood, dung cake, agricultural residue, or coal). "),
    r("Three-category fuel use", { italics: true }),
    r(" classifies all households into four groups according to which fuels they name, not according to verified exclusivity: LPG with no solid fuel reported; LPG and solid fuel reported (stacking); solid fuel reported with no LPG; and a small residual group reporting neither LPG nor any solid fuel (e.g. electricity, piped natural gas, or kerosene as the primary fuel), so that this last group is not miscounted among solid-fuel burners. We deliberately avoid the term 'exclusive LPG' for the first group. The surveys establish only that LPG was named among the fuels the household uses and that no solid fuel was named; that is a fact about reporting, not evidence that no solid fuel is ever burned, and the category also admits households that additionally use kerosene or another non-solid fuel. The analysis code labels the four levels 'LPG, no solid fuel reported', 'LPG and solid fuel reported', 'Solid fuel reported, no LPG' and 'Neither LPG nor solid fuel reported' (variable use3cat, constructed identically in 02_prep_access.R and 03_prep_ires.R); the first of these is an abbreviation of 'LPG reported, no solid fuel reported' and carries exactly that meaning. "),
    r("Annual LPG consumption", { italics: true }),
    r(" (kg/year) is computed from self-reported cylinder purchases, counting both distributor and market purchases of each cylinder size and valuing large cylinders at 14.2 kg and small cylinders at their reported size (5 kg where unrecorded) [8]. IRES additionally includes a direct self-report of whether LPG meets all cooking needs; because the survey's own documentation cautions that this item over-identifies exclusive users, we do not use it to define fuel use and rely on the reported use of individual fuels, retaining it only as a cross-check."),
  ]),
  p("Covariates were harmonized across surveys to the set measured comparably in all three sources: caste category (Scheduled Caste, Scheduled Tribe, Other Backward Class, General), religion (Hindu, Muslim, Other), household size, below-poverty-line/Antyodaya ration card, household electricity, a within-state wealth quintile (constructed from the NFHS wealth-index factor score and from monthly household expenditure in ACCESS and IRES, and therefore interpretable as relative within-state economic rank rather than absolute wealth), urban/rural residence, and state. A complete variable inventory with source item codes for all three surveys is provided in the variable inventory section at the end of the Supplementary Materials."),
];

const statText = [
  p("Precision-weighted predicted probabilities of the exposures were derived at the district level from household-level data using four-level multilevel models. As the district frames of NFHS-4 and NFHS-5 differ, the prevalence of each outcome was estimated on the districts defined in the NFHS-4 survey (the same frame used by ACCESS); NFHS-5 clusters were mapped onto NFHS-4 districts based on the district in which each cluster's coordinates fall. The four levels are households at level 1 (i), clusters (NFHS) or villages (ACCESS/IRES) at level 2 (j), districts at level 3 (k), and states at level 4 (l):"),
  p("logit(y_ijkl) = a0 + u_0jkl + v_0kl + f_0l          (Equation 1)", { par: { alignment: AlignmentType.CENTER }, run: { italics: true } }),
  p("where a0 is the constant, representing the median log-odds across the study area, and u_0jkl, v_0kl, and f_0l are cluster/village-, district-, and state-level residuals, assumed normally distributed with mean 0 and variances that capture within-district between-cluster variation, within-state between-district variation, and between-state variation, respectively. District-specific probabilities are obtained as expit(a0 + v_0kl + f_0l). Models were fit with the lme4 package in R [18]. Sampling weights were not incorporated into the multilevel models: weighted estimation of mixed models requires pseudo-maximum-likelihood with scaled, level-specific weights [21, 22], and the NFHS releases a single household-level weight (hv005/10^6) with no separate cluster- or district-level components, so unweighted multilevel estimation - standard in DHS-based small-area applications, including precision-weighted mapping of PM2.5 exposure and its socioeconomic patterning across Indian districts [41] - was used, with design-based direct estimates (weighted proportions with Taylor-linearized standard errors, computed in the survey/srvyr packages [24, 25]) serving as a robustness check. The three surveys' sampling weights differ in construction and are used as their designers intended; their construction, the design-weighted direct-estimation specification for each survey, and the Taylor-linearization procedure used to obtain each district's design-based standard error (which the measurement-error model of Section 2.4.3 requires) are detailed in Supplementary Methods S1. The unweighted multilevel and design-weighted direct estimates agree closely within each survey (district correlation " + mlwtRange("pearson_r") + ", Lin's CCC " + mlwtRange("ccc") + "; SI Figure S19), so weighting choices do not drive the comparison results; the multilevel estimates are retained as the primary specification because partial pooling stabilizes district estimates based on small within-district samples. Regression calibration is likewise insensitive to which raw estimate it is fit on - calibrating the design-weighted rather than the multilevel NFHS estimate yields a near-identical corrected surface (district correlation " + emCor(2015, "rc_ml", "rc_wt") + " in 2015 and " + emCor(2019, "rc_ml", "rc_wt") + " in 2019; SI Figure S20 and Table S10) - and all corrections raise the post-PMUY national rural prevalence to about " + nfmt(BAY19, 0) + "-" + nfmt(emx(2019, "Reg.-calibrated (design-wt input)"), 0) + "% (from a raw " + nfmt(RAW19, 0) + "-" + nfmt(emx(2019, "Raw design-weighted"), 0) + "%), agreeing on the level shift while the Bayesian surface redistributes somewhat more across districts than the calibrated ones (Section 3.2)."),
];

// ---- Supplementary Methods S1: survey weights and design-based SEs (moved from 2.4.1) ----
const weightsSI = [
  p("Each survey's sampling weights differ in construction and are used as their designers intended. The NFHS household weight (hv005, released multiplied by 10^6) is the design weight - the inverse of the household's selection probability under the two-stage stratified design - adjusted for household non-response and normalized so that weights average one over the national sample [6, 17]; it makes estimates representative at the national, state, and district level at which NFHS is designed to report. The ACCESS weights are village-level weights from the survey's replication materials [14]: they reflect each village's selection probability under the probability-proportional-to-size design (seven villages per size stratum per district), are shared by all households within a village (twelve households were sampled per village regardless of size), and carry no non-response or post-stratification adjustment; because districts were sampled one per administrative division and households equally within villages, the design is approximately self-weighting within districts and the weights chiefly correct across-village selection. The IRES weights are design (base) weights constructed by CEEW as the reciprocal of each household's selection probability through the full multistage design (district sampled proportional-to-size within state strata x village/ward sampled proportional-to-size within district x household within village/ward), corrected for the ratio of planned to actually completed interviews in each village/ward, and provided at two levels of representativeness; we use the district-level weights (sw_dist) because our estimand is district prevalence. Like ACCESS, IRES weights carry no non-response or post-stratification adjustment [23]."),
  p("Design-weighted direct district estimates (weighted proportions with Taylor-linearized standard errors) were computed using each survey's full sample design in the survey/srvyr packages [24, 25]: for NFHS, household weights (hv005/10^6) with clusters (hv021) as primary sampling units and sampling strata (hv022), with rural households analyzed as a subpopulation domain within the full design rather than by subsetting, and single-PSU strata handled by centering at the grand mean; for ACCESS, the village-level weights from the survey's replication materials, with the sampled villages as the primary sampling units; and for IRES, the CEEW-provided district analysis weights (sw_dist), with the sampled villages/wards as the primary sampling units. In both energy surveys the village (or ward) is the unit sampled below the district and households are enumerated within it, so the village is declared as the primary sampling unit - which captures the within-village clustering of fuel use - rather than mis-specifying households as the sampling unit and the village as a stratum, and no further within-village stratification is imposed."),
  p("The design-based standard error of each weighted district proportion is computed by Taylor-series linearization [24, 25]: the weighted proportion is a ratio of weighted totals, which is linearized into a per-observation score contribution, and the variance is then estimated from the variability of the summed score contributions across primary sampling units within each sampling stratum, under the standard with-replacement approximation for the first stage. This variance estimator accounts simultaneously for the unequal weights, for the clustering of households within PSUs (households in the same cluster are more alike than a simple random sample would be, which inflates the variance relative to the naive p(1-p)/n formula), and, where present, for the stratification (variability is assessed within, not across, strata, which reduces it). For NFHS this includes the hv022 sampling strata, and strata contributing a single PSU to a domain, which cannot supply a within-stratum contrast, are handled by centering that PSU at the overall mean; the two energy surveys are treated as single-stratum village-PSU designs, so their district standard errors reflect village-level clustering alone. For the measurement-error model in Section 2.4.3, these standard errors are transported to the logit scale by the delta method (Equation S1), so that each district's known sampling variance enters the model on the scale on which the calibration is estimated."),
  p("Two further details of how these standard errors enter the measurement-error model of Section 2.4.3 are recorded here, because both are choices a replication would otherwise have to rediscover from the code. First, the reference-side standard error is the reference survey's Taylor-linearized design standard error wherever one is available, and the code falls back, district by district, to the binomial form 1/sqrt(n p (1 - p)) only where it is not. In the NFHS-5/IRES calibration " + ME_B_DESIGN + " of the " + ME_B_N + " districts use the design standard error; the " + ME_B_FALLBACK + " exceptions are districts whose linearized standard error is exactly zero because their rural subsample carries no within-district variation, in one case because all " + ME_B_ALLYES_N + " sampled rural households reported LPG as the primary cooking fuel. The NFHS-4/ACCESS calibration uses the binomial form for all " + ME_A_N + " of its districts, because no design-based standard error was carried through for the multilevel ACCESS reference; Section 2.4.3 states what that costs."),
  p("Second, both standard errors are bounded before fitting: the reference-side logit standard error to the interval [0.02, 2], the NFHS-side logit standard error to [0.01, 2], and district proportions are bounded away from 0 and 1 by 0.001 before the logit transformation. The bounds exist so that a district whose estimated prevalence sits at the boundary cannot contribute an infinite measurement-error variance. Across both fitted panels they bind for exactly one district - the Andhra Pradesh district whose IRES rural prevalence is 1.000, which is moved to 0.999 and whose resulting binomial logit standard error of " + ME_CLAMP_RAW + " is reduced to 2. Because a smaller standard error carries more weight in the likelihood, this bound increases rather than decreases that district's influence; we note the direction explicitly because it is the less conservative one, though a logit standard error of 2 leaves the district weakly informative in any case. No other bound binds anywhere in either panel: the remaining reference-side standard errors span " + ME_Y_LO + " to " + ME_Y_HI + " and the NFHS-side standard errors " + ME_X_LO + " to " + ME_X_HI + ", all far inside the limits."),
  p("Because that fallback applies to the whole NFHS-4 leg, and because the era-matched corrected change takes its 2015 endpoint from that leg, we refit the NFHS-4 calibration a second time under exactly the treatment the NFHS-5 leg receives: the ACCESS district value is the design-weighted direct estimate rather than the multilevel one, and its Taylor-linearized standard error is passed to the model as the reference-side measurement error, so neither leg relies on the binomial approximation. The calibration slope is " + calWtE("slope") + " " + calWtCI("slope") + " under the symmetric fit against " + calE(CAL4, "slope") + " " + calCI(CAL4, "slope") + " in the primary specification, and the residual standard deviation is " + calWtE("sd_residual") + " against " + calE(CAL4, "sd_residual") + ", the increase being the reference-side uncertainty that the binomial form had been leaving out. The corrected 2015 surface itself is barely affected: across " + legAQ("n_districts") + " districts the two surfaces correlate at " + legAQ("corr_primary_sens", 3) + " and differ by " + legAQ("mean_abs_diff_pp", 2) + " percentage points on average, with a largest single-district difference of " + legAQ("max_abs_diff_pp", 2) + " percentage points. Carried through the child-mortality models of SI Methods S6, the symmetric fit " + legAVerDICT + " (SI Table S6 reports the primary specification; the movement of each uncertainty-propagated coefficient, in units of its own standard error, is given in legA_symmetry_health_movement.csv in the replication archive). We retain the multilevel reference as primary because it is the more precise district estimate, and record the symmetric refit here so that the standard-error asymmetry it introduces is bounded rather than merely acknowledged."),
  p("A worked example makes the arithmetic concrete. Consider a hypothetical district in which four rural households are sampled from two villages, and suppose their released household weights are 1, 1, 3 and 3 - that is, the first two households each stand for one household in the district population and the last two each stand for three, because they were drawn from a stratum that the design deliberately under-sampled relative to its size. Suppose the one household carrying weight 1 reports LPG as its primary cooking fuel and the other three do not. The unweighted proportion is 1 of 4, or 25.0 percent. The design-weighted proportion is the ratio of the weighted total of LPG households to the weighted total of all households, 1 divided by 8, or 12.5 percent. Weighting has halved the estimate, and for the right reason: the single LPG household stands for fewer real households than its raw presence in the sample suggests. Every design-weighted district estimate in this paper is this calculation, carried out over the households of one district."),
  p("The standard error attached to that 12.5 percent has to reflect the same design, and two features of the design reduce the information those four households carry. Unequal weights reduce it directly: the Kish approximation to the effective sample size, the square of the summed weights divided by the sum of the squared weights, gives 8 squared over 1 + 1 + 9 + 9, that is 64 over 20, or 3.2 households of effective information rather than 4 - an unequal-weighting design effect of 1.25. Clustering reduces it further, because the four households come from only two villages and households in the same village resemble one another in fuel use. A binomial standard error that ignores both, the square root of p (1 - p) / n, would be 0.165. Substituting the effective sample size for n gives about 0.185, and the Taylor-linearized estimator actually used here adds the clustering component on top of that by accumulating score contributions within primary sampling units rather than within households. We carry 0.185 forward."),
  p("Finally the estimate and its standard error are moved to the logit scale, on which the measurement-error model of Section 2.4.3 is fitted. The point estimate becomes the log odds of 0.125, that is the natural logarithm of 0.125 divided by 0.875, or -1.946. The standard error is transformed by the delta method given as Equation S1 below, which divides by p (1 - p): 0.185 divided by the product of 0.125 and 0.875 is 0.185 divided by 0.1094, or 1.69. That logit standard error is large, but only because the example has four households; it still falls inside the bounds described in the preceding paragraph, and real districts contribute hundreds of households and correspondingly tighter standard errors. If the reference survey placed the same district at 20.0 percent, its logit value would be the natural logarithm of 0.2 divided by 0.8, or -1.386, and the model would compare -1.946 with -1.386 while treating both survey standard errors as known measurement error and letting the residual standard deviation absorb what is left."),
];

const compareText = [
  p("We compare district-level primary-LPG prevalence for the two temporally overlapping survey pairs: NFHS-4 (rural clusters) versus ACCESS Wave 1, and NFHS-5 versus IRES (all households and rural-only). Agreement is summarized by Pearson and Spearman correlations, correlations on the logit scale, Lin's concordance correlation coefficient, and mean differences; sample-size-weighted correlations (weighting districts by surveyed reference-survey household counts, not by district population) address the sensitivity of unweighted district-level correlations to small, sparsely sampled districts. Scatterplots against the identity line and Bland-Altman plots are used to assess whether agreement diverges at higher or lower prevalence levels and by state. Because the reference surveys estimate district prevalence from about 168 households per district in ACCESS and about 96 in IRES - the design targets, with realized counts of 167-168 and 94-110 respectively - a substantial share of observed disagreement is expected from sampling variability alone. The rural subsample that the primary NFHS-5 calibration actually uses is smaller still, a median of " + ME_IRES_RURAL_MED + " rural households per district (range " + ME_IRES_RURAL_MIN + "-" + ME_IRES_RURAL_MAX + "), so district-level noise on the IRES side is correspondingly larger than the full-sample figure implies; the measurement-error model of Section 2.4.3 propagates it rather than ignoring it. We also note fieldwork-timing differences: IRES was fielded before most NFHS-5 interviews, so a positive IRES-NFHS gap cannot be attributed to secular growth in LPG use; PMUY's 2016 launch falls between ACCESS Wave 1 and the end of NFHS-4 fieldwork; and part of NFHS-5 fieldwork occurred during the COVID-19 period. Two sensitivity analyses address timing: restricting NFHS-5 to pre-COVID interviews (2019 through February 2020), and restricting NFHS-5 to winter interviews (December-February), the season in which IRES was fielded and in which stacking households are most likely to report a solid fuel as primary [32, 34]."),
  p("Finally, we conduct a falsification (benchmark) analysis: if NFHS and the reference surveys disagree on LPG because of fuel measurement rather than sample composition, they should still agree on benchmark characteristics measured comparably in both. We therefore estimate district prevalences of Scheduled Caste, Scheduled Tribe, Hindu, Muslim, household electricity, BPL/Antyodaya ration card, and the combined Scheduled Caste/Scheduled Tribe share with the same multilevel specification in each member of each survey pair, and compare their agreement (correlations and mean differences) with that of primary LPG."),
];

// ============ VALIDATION-SURVEY DESIGN ANALYSIS (22_design_analysis.R) =======
// Discussion 4.5, SI Section S4 and Tables S16-S17 are built entirely from the
// CSVs 22_design_analysis.R writes. Nothing below is typed by hand: change the
// kappa grid, the design grid or the frontier and the prose moves with it, and
// a missing output reads as NA rather than as a stale number that no longer
// corresponds to any run.
const dSum    = readCsv("design_summary.csv");
const dFront  = readCsv("design_frontier.csv");
const dKstar  = readCsv("design_kappa_star.csv");
const dVdec   = readCsv("design_variance_decomposition.csv");
const dGrid   = readCsv("design_grid.csv");
const dDirect = readCsv("design_direct.csv");

const dsRaw = (k) => { const x = dSum.find((z) => z.quantity === k); return x ? x.value : undefined; };
const DS    = (k, d) => nfmt(dsRaw(k), d === undefined ? 2 : d);
const DSn   = (k) => { const v = Number(dsRaw(k)); return isFinite(v) ? v : NaN; };
const MILL  = (k, d) => { const v = DSn(k); return isFinite(v) ? nfmt(v / 1e6, d === undefined ? 1 : d) : "NA"; };

const dfRow = (outcome, kappa) => dFront.find((x) => x.outcome === outcome &&
                Math.abs(Number(x.kappa) - kappa) < 1e-9) || {};
const ksRow = (outcome) => dKstar.find((x) => x.outcome === outcome) || {};

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
  heR("NFHS-4 vs ACCESS (rural)", energy, "Neonatal") + " to " +
  heR("NFHS-4 vs ACCESS (rural)", energy, "Infant") + " for ACCESS/NFHS-4; " +
  heR("NFHS-5 vs IRES (rural)", energy, "Neonatal") + " to " +
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
const chkDetail = (re) => {
  let r = pipeChecks.find((x) => re.test(x.check || ""));
  if (!r) {
    if (_prevChecks === null)
      _prevChecks = readCsv("diagnostics/prev_pipeline_checks.csv", { quiet: true });
    r = _prevChecks.find((x) => re.test(x.check || ""));
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
  rat15: chkNum(/NFHS-side floor matches .* from NFHS SEs, 2015/, /ratio ([0-9.]+)/),
  rat19: chkNum(/NFHS-side floor matches .* from NFHS SEs, 2019/, /ratio ([0-9.]+)/),
  anch15: chkNum(/design simulator reproduces the actual designs/, /2015 ([0-9.]+)/),
  anch19: chkNum(/design simulator reproduces the actual designs/, /2019 ([0-9.]+)/)
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
  const first = Math.abs(va) < 1
    ? "leave the calibration-side standard deviation essentially unchanged on the 2015 leg"
    : verb(va) + " the calibration-side standard deviation by " + nfmt(Math.abs(va), 0)
      + " per cent on the 2015 leg";
  const second = Math.abs(vi) < 1
    ? "leave it essentially unchanged on the 2019 leg"
    : verb(vi) + " it by " + nfmt(Math.abs(vi), 0) + " per cent on the 2019 leg";
  const cov = Number(a.pct_under_sd_covered_states);
  const unc = Number(a.pct_under_sd_uncovered_states);
  const why = (isFinite(cov) && isFinite(unc) && unc > cov && Math.abs(cov) < 3)
    ? ", and what difference there is sits almost entirely in target districts in "
      + "states the validation survey never entered, where a fresh state effect "
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
      + "in SI Methods S4. We treat the reconstruction as accurate enough to rank "
      + "survey designs against one another rather than as an exact reproduction of "
      + "the fitted posterior, and we do not interpret small differences between "
      + "neighbouring designs.";
  }
  const pct = (r) => nfmt(Math.abs(r - 1) * 100, 0) + " per cent";
  return "Rather than assume that reconstruction reproduces the fitted models, we "
    + "compared it with them on four criteria. Three are internal to that comparison: "
    + "the reconstructed calibration variance lies below the fitted posterior variance "
    + "in " + nfmt(v.sign15, 0) + " per cent and " + nfmt(v.sign19, 0) + " per cent of "
    + "target districts on the two legs, as it must if the remainder is to be a genuine "
    + "NFHS-side term; it tracks the fitted posterior across districts, with "
    + "correlations of " + nfmt(v.trk15, 2) + " and " + nfmt(v.trk19, 2) + "; and the "
    + "remainder rises as district sample size falls, correlating with the inverse of "
    + "district sample size at " + nfmt(v.nfall15, 2) + " and " + nfmt(v.nfall19, 2)
    + ". The fourth uses information that never entered the reconstruction: the "
    + "remainder should equal b squared times the district sampling variance implied by "
    + "NFHS's own published standard errors, and it agrees with that quantity to within "
    + pct(v.rat15) + " and " + pct(v.rat19) + ". We therefore treat the reconstruction "
    + "as accurate enough to rank survey designs against one another rather than as an "
    + "exact reproduction of the fitted posterior, and we do not interpret small "
    + "differences between neighbouring designs.";
};

// Assembled middle of the variance-split paragraph. Empty components drop out
// rather than leaving a doubled space.
const p246Checks = () => [vcPenMain(), fourChecksTxt()]
  .filter((s) => s && s.length).map((s) => " " + s).join("");

// Scale anchor for the design grid: how big the two REALIZED validation samples
// actually were, so a reader can see what the top of the grid would mean.
const p246Scale = () => {
  const a = vdRow("2015"), i = vdRow("2019");
  const nd = Number(a.calib_districts), ns = Number(a.calib_states);
  const md = Number(i.calib_districts), ms = Number(i.calib_states);
  const N  = DSn("n_study_districts");
  if (![nd, ns, md, ms, N].every((z) => isFinite(z))) return "";
  return "For scale, the two realized validation samples entering the calibration "
    + "models cover " + nd + " districts in " + ns + " states and " + md + " districts "
    + "in " + ms + " states, so the largest cell in the grid describes a validation "
    + "survey with more districts than the whole " + N + "-district study frame, present "
    + "in every major state. ";
};

// The anchoring constants themselves, so that "anchored" cannot be read as
// "exact". A value of 1 would mean the simulator needed no anchoring at all.
const p246Anchor = () => (isFinite(DCK.anch15) && isFinite(DCK.anch19))
  ? "; those constants are " + nfmt(DCK.anch15, 2) + " and " + nfmt(DCK.anch19, 2)
    + ", so the simulator is close to self-consistent before it is anchored"
  : "";

// ---- Worked single-district illustration of the floor (SI Methods S4) ----
// Every quantity below is a function of kappa_floor and kappa_star_infant,
// which 22_design_analysis.R exports to design_summary.csv, so a re-run that
// moves the floor moves the worked example with it and the illustration can
// never drift out of agreement with Figure 3 or Table S17.
//
// Three numbers here are ILLUSTRATIVE and carry no analytic content: the round
// NFHS standard error the example is built around, and the pair of prevalences
// used to make it concrete. They are declared here rather than buried in the
// prose so that a reader of this file can see immediately which numbers are
// derived and which are chosen.
const TOY_SE = 3.0;   // percentage points, chosen round
const TOY_X  = 40;    // per cent, NFHS estimate in the illustrative district
const TOY_Y  = 52;    // per cent, its corrected value
const toy = () => {
  const kf = DSn("kappa_floor"), ks = DSn("kappa_star_infant");
  if (!isFinite(kf) || kf <= 0 || kf >= 1) {
    console.warn("[build] kappa_floor absent or out of range in "
      + "design_summary.csv -- omitting the worked single-district "
      + "illustration from SI Methods S4. Re-run 22_design_analysis.R.");
    return null;
  }
  const tot = TOY_SE / kf;
  const cal = Math.sqrt(Math.max(tot * tot - TOY_SE * TOY_SE, 0));
  return { se: nfmt(TOY_SE, 1), tot: nfmt(tot, 1), cal: nfmt(cal, 1),
           kf: nfmt(kf, 2),
           calShare: nfmt(100 * cal * cal / (tot * tot), 0),
           nfhsShare: nfmt(100 * TOY_SE * TOY_SE / (tot * tot), 0),
           sdCut: nfmt(100 * (1 - TOY_SE / tot), 0),
           ks: isFinite(ks) ? nfmt(ks, 2) : null,
           target: isFinite(ks) ? nfmt(ks * tot, 1) : null };
};

// One main-text sentence carrying the same point, with the two shares wired.
// These shares are properties of the fitted decomposition, not of the
// illustration: they do not depend on TOY_SE.
const toySqrt = () => {
  const t = toy();
  if (!t) return "";
  return "The reason the floor binds so early is arithmetic rather than "
    + "empirical: the two components add in variance, and a square root "
    + "compresses whatever the validation survey achieves, so removing "
    + t.calShare + " per cent of the total variance removes only " + t.sdCut
    + " per cent of the standard deviation. SI Methods S4 works this through "
    + "for a single district. ";
};

// The SI paragraphs themselves. Returned as an array so the whole illustration
// drops out cleanly if the design outputs are missing.
const toyBlock = () => {
  const t = toy();
  if (!t) return [];
  const out = [
    p("A single district makes the arithmetic concrete. The illustration below "
      + "uses percentage points for readability, although the calibration model "
      + "itself operates on the logit scale, and it sets the split between the "
      + "two variance components to the value this study actually estimates, so "
      + "it reproduces the reported floor exactly. Suppose NFHS estimates LPG "
      + "use in some district at " + TOY_X + " per cent with a standard error of "
      + t.se + " percentage points, and the fitted calibration maps that "
      + "estimate to a corrected value of " + TOY_Y + " per cent. Two "
      + "independent things are uncertain about that " + TOY_Y + ". First, the "
      + "calibration line itself - its intercept, slope and state effect - was "
      + "estimated from a validation survey of finite size, and at this "
      + "district's position on the line that contributes " + t.cal
      + " percentage points of uncertainty. Second, the NFHS estimate being fed "
      + "into the line is itself a sample estimate, and its " + t.se + "-point "
      + "standard error passes through the slope to contribute a further "
      + t.se + " points. Because the two sources are independent they combine in "
      + "variance rather than in standard deviation, so the corrected value "
      + "carries a posterior standard deviation of " + t.tot + " percentage "
      + "points, of which the validation survey is responsible for " + t.calShare
      + " per cent of the variance and NFHS for the remaining " + t.nfhsShare
      + " per cent. That " + t.nfhsShare + " per cent is the pooled NFHS-side share across both calibration legs, which is the quantity that fixes the floor; Table S17 gives the leg-specific shares of "
      + DS("var_nfhs_side_share_2015", 0) + " and "
      + DS("var_nfhs_side_share_2019", 0) + " per cent."),
    p("Now imagine the ideal validation survey: every district, every state, a "
      + "perfectly harmonized instrument administered to the same households. "
      + "The first component vanishes entirely. The corrected value still "
      + "carries " + t.se + " points, because that term is a property of the "
      + "health survey being corrected rather than of the survey used to correct "
      + "it. Uncertainty has fallen from " + t.tot + " points to " + t.se
      + " and can fall no further, which is exactly the floor: kappa_floor = "
      + t.se + " / " + t.tot + " = " + t.kf + ". Note what the square root has "
      + "done. Eliminating " + t.calShare + " per cent of the variance removed "
      + "only " + t.sdCut + " per cent of the standard deviation. This is why "
      + "the frontier flattens so early, and why the difference between a good "
      + "validation survey and a perfect one is so much smaller than intuition "
      + "suggests.")
  ];
  if (t.ks && t.target) out.push(
    p("The consequence for inference follows immediately. The infant-mortality "
      + "association reaches an absolute z of 1.96 at kappa* = " + t.ks
      + ", which in this district's terms means a corrected standard deviation "
      + "of about " + t.target + " percentage points. That is below the " + t.se
      + " points that survive even a perfect validation survey, so no validation "
      + "survey of any size, in any number of states, with any instrument can "
      + "deliver it. Only two levers remain, and both act on the health survey "
      + "rather than on the calibration: interviewing more households in each "
      + "NFHS district, which lowers that term directly, or fielding the "
      + "improved fuel module inside the NFHS districts, which removes the "
      + "calibration step altogether and replaces both components with the new "
      + "instrument's own sampling error. Both are quantified below, and the "
      + "sample-size multiplier reported there is larger than this simplified "
      + "illustration would imply, because it is evaluated on the grid's best "
      + "achievable transfer design, in which the calibration component is small "
      + "but not zero. The figures in this illustration are chosen for "
      + "arithmetic transparency; the reported floor is computed from the full "
      + "posterior across both calibration legs and all "
      + DS("n_study_districts", 0) + " study districts, not from a single "
      + "district."));
  return out;
};


// The three verdicts in this section are DERIVED, never asserted. A sentence
// that states a conclusion in words ("crosses", "never reaches", "above the
// floor") has to be computed from the same CSV as the number beside it, or it
// will silently outlive it -- the failure mode that produced bdVerdict() and
// allBayesPTxt() above.
const ksReach   = (o) => String(ksRow(o).reachable).toUpperCase() === "TRUE";
const gridHits  = dGrid.filter((x) => String(x.reaches_p05).toUpperCase() === "TRUE").length;
const gridBest  = dGrid.slice().sort((a, b) => Number(a.kappa) - Number(b.kappa))[0] || {};
const directStar = dDirect.find((x) => Number(x.m_ratio) === DSn("direct_m_star")) || {};
const direct1    = dDirect.find((x) => Number(x.m_ratio) === 1) || {};
const kappaStarTxt = () => ksReach("infant")
  ? "kappa* = " + DS("kappa_star_infant", 2)
  : "no attainable value of kappa";
const neoTxt = () => ksReach("neonatal")
  ? "the neonatal frontier crosses the same threshold at kappa* = " + nfmt(ksRow("neonatal").kappa_star, 2)
  : "the neonatal frontier never crosses it - even at kappa = 0, with the correction treated as if it were exact, |z| reaches only " + DS("z_at_kappa0_neonatal", 2);
const floorTxt = () => (isFinite(DSn("kappa_floor")) && isFinite(DSn("kappa_star_infant")))
  ? (DSn("kappa_floor") > DSn("kappa_star_infant") ? "above" : "below")
  : "at an unknown position relative to";
const floorVerdict = () => (isFinite(DSn("kappa_floor")) && isFinite(DSn("kappa_star_infant")) &&
                            DSn("kappa_floor") > DSn("kappa_star_infant"))
  ? "no transfer-function validation survey - of any size, in any number of states, with any instrument - is predicted to reach p < 0.05 for this design"
  : "a sufficiently precise transfer-function validation survey is predicted to reach p < 0.05, the floor lying below kappa*";
// floorTxt() answers "where is the FLOOR relative to kappa*". A sentence whose
// subject is kappa* needs the opposite orientation, and using floorTxt() there
// inverts the sense. Kept as a separate helper so neither can be used for the
// other by accident.
const starVsFloorTxt = () => (isFinite(DSn("kappa_floor")) && isFinite(DSn("kappa_star_infant")))
  ? (DSn("kappa_star_infant") < DSn("kappa_floor")
      ? "a precision beyond the floor set by the health survey's own district sampling error, which no validation survey can lower"
      : "a precision that lies within the floor set by the health survey's own district sampling error, and so is in principle attainable")
  : "a precision whose position relative to the floor set by the health survey's own district sampling error could not be determined";
const gridVerdict = () => gridHits === 0
  ? "none of the " + dGrid.length + " designs reached p < 0.05"
  : gridHits + " of the " + dGrid.length + " designs reached p < 0.05";

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
const psP   = (o, v) => pfmt(psRow(o, v).p);
const psCI  = (o, v, d) => {
  const e = Number(psRow(o, v).estimate), se = Number(psRow(o, v).se);
  if (!isFinite(e) || !isFinite(se)) return "NA";
  const k = d === undefined ? 3 : d;
  return nfmt(e - 1.96 * se, k) + ", " + nfmt(e + 1.96 * se, k);
};
const pvRow = (yr) => ppdSurf.find((x) => String(x.leg).indexOf(yr) === 0) || {};
const ppdVerdict = (o) => {
  const a = Number(psRow(o, "epred_MI").p), b = Number(psRow(o, "ppd_MI").p);
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
  const a = Number(psRow(o, "epred_MI").se), b = Number(psRow(o, "ppd_MI").se);
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
  const e1 = Number(psRow(o, "epred_MI").estimate), s1 = Number(psRow(o, "epred_MI").se);
  const e2 = Number(psRow(o, "ppd_MI").estimate),   s2 = Number(psRow(o, "ppd_MI").se);
  if (!isFinite(e1) || !isFinite(e2) || !isFinite(s1) || !isFinite(s2) || s1 === 0 || s2 === 0)
    return "the test statistic could not be compared";
  const z1 = Math.abs(e1 / s1), z2 = Math.abs(e2 / s2);
  return "the two changes largely cancel in the test statistic, which moves from |z| = "
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
          p1 = Number(psRow(o, "epred_MI").p), p2 = Number(psRow(o, "ppd_MI").p);
    return isFinite(e1) && isFinite(e2) && isFinite(p1) && isFinite(p2) && e2 < e1 && p2 > p1;
  });
  return ok
    ? "In this application the posterior-predictive version is conservative in both senses for both outcomes - smaller in magnitude and less significant than the version we report as primary - so nothing in the paper's conclusions rests on the choice between them"
    : "In this application the posterior-predictive version is not uniformly conservative across the two outcomes, which is why we report it in full rather than as a one-line reassurance";
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
  p("We correct the NFHS district estimates by calibrating them to the energy-survey estimates, and it is worth being explicit about why the latter serve as the reference. All three surveys measure cooking fuel by household self-report, and each asks a similar question about the household's primary cooking fuel, so the energy surveys are not a verified gold standard. They differ from NFHS in how the question is posed: NFHS records a single 'main fuel used for cooking' from a general household respondent, whereas ACCESS and IRES ask the primary-fuel question within a dedicated cooking-energy module that first enumerates every fuel the household uses and its quantity, and (in ACCESS) engages the primary cook. A module that establishes the full fuel portfolio before asking which fuel is primary is designed to elicit more complete and consistent reporting, and is less prone than a single undifferentiated item to misclassify the stacking households that dominate rural India. The energy surveys also link households to districts by census code, whereas NFHS cluster locations carry the DHS positional displacement and are assigned to districts spatially (Section 2.1), a further source of error in the NFHS estimates - though one too small and non-systematic to explain the level gap we observe. We therefore treat the energy-survey estimates as a higher-fidelity, though not infallible, reference. This interpretation is not assumed but is supported by the agreement of the two independent energy surveys with each other and with an external pan-India survey, and by the benchmark analysis that localizes the NFHS-reference discrepancy to the fuel item rather than to sample composition (Section 3); the corrected estimates are accordingly best understood as calibrated to a better-instrumented reference rather than as validated measurements of true exposure, a limitation we return to in the Discussion. We correct the NFHS estimates in two ways. First, regression calibration: within the overlapping districts we regress the logit of the reference prevalence on the logit of the NFHS prevalence, weighted by reference-survey household counts, with and without state effects, and apply the fitted calibration to all NFHS districts. Second, a Bayesian hierarchical measurement-error model estimated with brms [19]: the logit-scale NFHS district estimate enters as a covariate measured with known error (its design-based standard error, delta-method transformed to the logit scale), with state-level random effects and weakly informative priors. The reference logit-prevalence is the response and itself carries a known standard error - for NFHS-5, the Taylor-linearized standard error of the rural design-weighted IRES estimate; for NFHS-4, where no design-based standard error was carried through for the multilevel ACCESS reference, a binomial approximation computed from the district household count. That approximation treats the district sample as an independent draw and so ignores ACCESS's village clustering and weighting; because clustered designs have design effects above one, it understates reference-side uncertainty, and the NFHS-4 fit correspondingly over-trusts the ACCESS points. We therefore treat the NFHS-4 calibration as the less well characterized of the two rather than as its equal, while noting that the residual variance the same model estimates for that pair is near zero, which bounds how much the understatement can matter. Both fits add such a residual term to the known standard error - estimated, not assumed - absorbing questionnaire and design non-comparability beyond what sampling error explains; the fitted residual SD is itself a result and is reported in Table S3. Full details of which districts use which standard error, and of the bounds imposed on both, are given in Supplementary Method S1. Models were estimated with four Hamiltonian Monte Carlo chains (24,000 post-warmup draws; convergence assessed by R-hat and effective sample size). The NFHS-4/ACCESS fit returned " + bdTxt("nfhs4_access") + "; the NFHS-5/IRES fit returned " + bdTxt("nfhs5_ires") + ". " + bdVerdict() + " Corrected district prevalences with 95% credible intervals (CrI, the Bayesian analogue of a confidence interval: the range containing 95% of the posterior probability for the quantity) for all districts were obtained from the posterior distribution of the model's fitted (expected) district prevalence, retaining the estimated state effect for districts in calibration states and drawing a state effect from the fitted state-level distribution for districts in states outside the calibration sample. Each NFHS round is corrected using the reference survey from its own era, and on rural households in both cases: the NFHS-4 rural district estimates are calibrated against ACCESS Wave 1 (rural by design), and the NFHS-5 rural district estimates against the rural design-weighted IRES estimate - so that, for the primary national product, both sides of the calibration are the rural design-weighted direct estimate (like against like). The corrected surface is accordingly the district prevalence of rural primary-LPG use; the all-household NFHS-5/IRES comparison of Section 2.4.2 is reported for completeness but is not used for correction. The two calibrations differ in geographic support. IRES samples districts in " + I_STATES + " states covering 97% of India's population, so applying the NFHS-5 calibration to all Indian districts largely interpolates within the range of settings on which it was estimated; ACCESS covers " + A_W1_D + " districts in six poor northern states, so applying the NFHS-4 calibration outside those states assumes that the NFHS-reference relationship estimated there also holds elsewhere - an extrapolation that these data cannot verify. We therefore treat the corrected NFHS-5 surface as the primary national product, report the corrected NFHS-4 surface with an explicit extrapolation caveat outside the six ACCESS states, and note that the ACCESS calibration independently corroborates the existence and direction of the calibration relationship observed with IRES. Transportability of the NFHS-5 calibration is assessed by leave-one-state-out cross-validation, comparing root-mean-square error of corrected versus uncorrected estimates in held-out states. The regression calibration is estimated on the multilevel district estimates; the Bayesian measurement-error model instead takes the design-weighted direct NFHS estimates as the error-prone input, because these carry valid design-based standard errors (delta-method transformed to the logit scale) that quantify the known measurement-error variance for each district."),
  p("We use both correction methods deliberately. Regression calibration is transparent, easily replicated, and standard in exposure epidemiology, but it treats the error-prone NFHS estimates as fixed: sampling noise in the NFHS values attenuates the fitted calibration slope, and the method yields point predictions without uncertainty. The Bayesian measurement-error model addresses both limitations: each district's true prevalence is treated as latent and estimated jointly with the calibration using that district's known design-based sampling variance, so districts are shrunk toward the calibration line in proportion to their own imprecision, and every corrected estimate carries a credible interval that we propagate into downstream health models. Agreement between the two approaches indicates robustness of the correction to the treatment of sampling error [12, 26]."),
  p("The estimand for the corrected exposure has to be stated explicitly before that exposure is used in a health model. The value we carry forward is a posterior {{expectation}} of the district's primary-LPG prevalence conditional on its NFHS estimate and the calibration data - specifically the logistic of the posterior mean of the district's latent logit prevalence - and not a draw of the district's true prevalence. Because the logistic transform is nonlinear, this point summary is not identical to the posterior mean taken on the prevalence scale; the multiple-imputation draws described below carry the posterior itself, and the probability-scale check noted at the end of this section confirms that the choice of scale does not move the health coefficient. Uncertainty in the calibration parameters is propagated into the health model by multiple imputation (Section 2.4.5); the calibration's residual equation error is treated as Berkson-type error and therefore enters through the outcome model rather than through the exposure. The distinction matters because the two components play different roles. Writing the district's latent logit prevalence as the value used plus an error, the first part of that error is posterior uncertainty in the intercept, slope and state effects. It is systematic: the same estimated calibration is applied to every district, so uncertainty in the slope or intercept shifts or rescales the entire exposure surface and passes directly into the estimated health effect. It must be carried through, and the multiple imputation over calibration posterior draws does exactly that, capturing what is likely the dominant source of uncertainty. The second part is the residual equation error. Because the model already carries each reference district's own sampling standard error as a known variance (Supplementary Method S1), the fitted residual SD is not reference-survey sampling noise but genuine equation error: the district-level mismatch between NFHS and the reference survey that remains after calibration and state effects. Conditional on the fitted calibration model, that residual is district-specific and represents unexplained departures of individual districts from the calibration relationship. Under the usual Berkson assumptions it contributes additional variability to the outcome but does not induce the attenuation associated with classical measurement error: in a linear outcome model it is absorbed into the composite residual, leaving the coefficient unbiased, and the cluster-robust variance estimator captures the resulting increase in residual variability. As with any Berkson argument, this relies on the assumed calibration model and is exact only under its conditions. Imputing instead from the full posterior predictive would add the residual back into every imputed exposure surface, which no longer corresponds to estimating the conditional expectation of district exposure but treats irreducible district-to-district mismatch as if it were uncertainty about the district's underlying exposure; the practical consequence is typically attenuation of the estimated health effect, and because adding the residual also inflates the variance of the exposure itself the standard error need not widen in step. Because the imputation model is estimated independently of the mortality model - the two are uncongenial in Meng's sense [" + MENG_NO + "] - that attenuation is in any case not guaranteed to be conservative. We therefore report the posterior-predictive version as a clearly labelled sensitivity analysis rather than as the primary result, together with a check that averaging the posterior on the probability scale rather than on the logit scale does not move the coefficient (" + PPD_REF + ")."),
];

// Design-analysis grid levels and kappa-grid size, read back from the CSVs that
// 22_design_analysis.R writes, so the Methods description of the simulation
// cannot drift away from the code that ran it.
const dgLev = (k) => Array.from(new Set(dGrid.map((r) => Number(r[k]))))
  .filter((v) => isFinite(v)).sort((a, b) => a - b);
const dgTxt = (k, d) => dgLev(k).map((v) => nfmt(v, d === undefined ? 0 : d)).join(", ");
const kapN  = Array.from(new Set(dFront.map((x) => Number(x.kappa))))
  .filter((v) => isFinite(v)).length;

const predictText = [
  p("As a secondary analysis, we augment NFHS with fuel-use detail it does not collect. We model three energy-survey outcomes - fuel stacking among primary-LPG households, the four-category composition of fuel use (LPG with no solid fuel reported / stacking / solid fuel with no LPG reported / other non-solid primary fuel), and annual LPG consumption - as functions of the harmonized covariate set - caste category, religion, household size, below-poverty-line/Antyodaya ration-card status, household electricity, and within-state wealth quintile - plus a district-context covariate (the corrected district LPG prevalence), using multilevel logistic, multinomial, and two-part hurdle models respectively. Models are trained era-matched (ACCESS Wave 1 for NFHS-4, IRES for NFHS-5), validated by leave-district-out cross-validation and a stricter cross-survey, cross-era test, and applied to rural NFHS households, then aggregated to districts with NFHS sampling weights; combined with the corrected primary-LPG prevalence, they yield district-level exposure proxies (expected LPG-with-no-solid-fuel-reported, stacking, and solid-fuel-with-no-LPG shares, and expected consumption). Because the determinants of stacking proved unstable across policy eras (SI Methods S7), we treat these predictions as secondary; the model specifications, out-of-sample validation, and era-matched design are detailed in Supplementary Methods S2. Item missingness for the exposure and every covariate, together with the sample-linkage exclusions and district coverage, is documented in SI Table S11; the primary-LPG exposure is complete in all four surveys, and covariate missingness is low (the main exceptions being caste, " + missPct1("NFHS-5", "Caste") + "% in NFHS, and the IRES expenditure item, " + missPct1("IRES", "Wealth (index/expenditure)") + "%, which feeds only the wealth quintile). Educational attainment could not be harmonized at all - it is unpopulated in the NFHS extracts while both energy surveys measure it completely (Table S1) - and is therefore excluded from the covariate set and from the benchmark comparison. All analyses were conducted in R version 4.5 [20]; analysis code is available from the authors."),

  h3("2.4.5 Association with child mortality (rural districts; all-household analysis in the Supplementary Materials)"),
  p("To illustrate the consequences of exposure measurement error for health analysis, we estimated district-level change-on-change associations between clean-cooking prevalence and child mortality, following the design of prior analyses linking the clean-cooking transition to child health in India [42]. District child-mortality probabilities were estimated from the NFHS-4 and NFHS-5 children's recode (birth-history) files as recent-cohort probabilities over the full birth history in the ten years (120 months) preceding each survey. For each birth we computed age at interview from the century-month birth and interview dates, and counted a birth toward an outcome - neonatal (death before one month) or infant (before twelve months) - only once it had been observed for the full age interval or had already died within it; more recent births not yet old enough to have completed the interval were treated as censored (missing) for that outcome rather than counted as survivors, so each estimate is a completed-exposure cohort probability rather than a raw death share. We used the full birth history reported by interviewed mothers, including children no longer resident in the household, rather than restricting living children to those currently co-resident: because deceased children have no co-residence status, a co-residence restriction would retain all deaths but drop surviving children who had moved away, biasing mortality upward. This yielded " + births("rural", 2015) + " (NFHS-4) and " + births("rural", 2019) + " (NFHS-5) rural births in the observation window, and " + births("all", 2015) + " and " + births("all", 2019) + " births across all households. Births were assigned to NFHS-4 districts by cluster coordinates, with clusters lying more than ten kilometres from any district polygon left unassigned (matching the exposure pipeline, Section 2.1); " + sumCol(hdwRural, "n_births_2015") + " and " + sumCol(hdwRural, "n_births_2019") + " rural births (" + sumCol(hdwAll, "n_births_2015") + " and " + sumCol(hdwAll, "n_births_2019") + " across all households) reached a district and entered the district-level estimates. District mortality prevalences for each round were obtained from the same four-level multilevel specification as Equation 1 (the primary estimator); as a sensitivity we also computed DHS design-weighted direct district mortality (sampling weights, PSU-clustered standard errors), reported in the Supplementary Materials. The socioeconomic covariates used for adjustment - below-poverty-line/lowest wealth quintile, maternal education, household electricity, Muslim share, improved sanitation, and improved water - were estimated separately from the women's recode (individual-record) files, one record per woman rather than per birth, so that they represent woman/household composition rather than birth-weighted quantities, and were merged to districts. Analyses used the change (2019 minus 2015, per 100 births) as the outcome. Because the survey rounds are about four years apart, the two ten-year birth-history windows overlap substantially in calendar time, so this change is best read as the difference between two overlapping retrospective cohort estimates rather than a clean four-year mortality change; a short, non-overlapping 36-month recent-cohort window (whose two cohorts do not overlap in calendar time) is reported as a sensitivity (SI Table S6). Because the measurement-error correction is estimated on rural households (Section 2.4.3), the primary mortality analysis restricts district mortality to rural births as well, so that the exposure and the outcome describe the same population; an all-household version, which uses the identical exposure, is reported in the Supplementary Materials as a sensitivity analysis."),
  p("The exposure was the district change in primary-LPG prevalence, measured six ways: raw NFHS rural estimates; regression-calibrated; the era-matched Bayesian correction (the primary specification, in which ACCESS calibrates the 2015 estimate and IRES the 2019 estimate); the era-matched correction with its uncertainty propagated into the health model by multiple imputation (200 draws taken from the model's saved posterior of the corrected 2015 and 2019 district prevalence - the actual posterior draws, not a Normal approximation from the credible-interval endpoints - forming the change on each draw, refitting, and pooling by Rubin's rules); a Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both NFHS rounds, so the corrected change does not switch calibration instrument between the two endpoints); and that instrument-consistent version with its uncertainty likewise propagated. Models were weighted by the harmonic mean of the number of eligible births in each district across the two rounds (a births-based analytic weight, not a population denominator; an unweighted specification is reported as a sensitivity). The harmonic mean is the inverse-variance weight for this outcome: the variance of a difference of two round-specific rates is proportional to the sum of the reciprocal birth counts, so weighting by the harmonic mean weights each district by the reciprocal of that sum. It is therefore dominated by the sparser of the two rounds, which is the intended behaviour, because a change is estimated no more precisely than its noisier endpoint. Models were also adjusted for concurrent district changes in poverty (lowest wealth quintile), maternal education, household electricity, Muslim share, improved sanitation, and improved water, and for concurrent changes in ambient environmental conditions - annual mean fine particulate matter (PM2.5), mean near-surface temperature, relative humidity, and drought (the Palmer Drought Severity Index) - together with region fixed effects, with standard errors clustered on state. Under classical measurement error, attenuation of the raw-exposure coefficient toward the null is expected; a monotonic strengthening of the association as measurement error is removed is the diagnostic signature. Because the era-matched correction uses different reference instruments at the two endpoints, we report the single-instrument version to separate genuine strengthening from any artifact of switching calibration instrument between rounds."),
  p("Ambient covariates were constructed at the district level for the two survey windows (2011-2015 for NFHS-4 and 2016-2020 for NFHS-5) and differenced. District mean PM2.5 was extracted from the satellite-derived Atmospheric Composition Analysis Group V5.GL.03 hybrid product (0.01-degree annual surfaces combining multiple satellite aerosol products with chemical-transport modeling and ground-monitor calibration) [27]; ambient PM2.5 is a robust district-level predictor of infant mortality in India and is therefore an important confounder to adjust for in this design [40]. Temperature, relative humidity, and drought were extracted from the TerraClimate 1/24-degree monthly climate reanalysis [28]; relative humidity was derived from TerraClimate's actual vapor pressure and vapor-pressure deficit as 100 x vap / (vap + vpd). All ambient surfaces were summarized to district means by area-weighted zonal extraction over the NFHS-4 district polygons."),
  p("Two features of the design bound the influence of survey timing on the corrected exposure. First, the calibration is era-matched: NFHS-4 is corrected against ACCESS (2014-15) and NFHS-5 against IRES (2019-20), so no estimate is calibrated against a temporally distant reference. Second, because NFHS-5 fieldwork is state-staggered and extends beyond the concentrated IRES window while LPG use was still rising under PMUY, calibrating late-surveyed districts toward IRES could in principle pull the corrected 2019 prevalence slightly downward; but the season- and pre-COVID-restricted re-calibrations (Section 2.4.2) leave the slope and gap essentially unchanged, indicating the NFHS-IRES discrepancy reflects questionnaire design rather than timing. Any residual downward bias would compress the estimated 2015-2019 exposure change and therefore attenuate - not inflate - the health associations, making the corrected estimates conservative."),
  p("Two exploratory extensions are reported in the Supplementary Materials and interpreted cautiously. First, we substituted the predicted district composition metrics from Section 2.4.4 - the LPG-no-solid-reported share, the share of households reporting any solid fuel, the fuel-stacking share, and predicted LPG consumption - for primary-LPG prevalence in the same change-on-change design, to ask whether the nuanced predicted exposures add health information beyond the corrected primary-fuel prevalence; because these metrics are model-predicted rather than measured and their between-round change inherits the prediction models' limited temporal transportability (SI Methods S7), they are treated as hypothesis-generating only. Second, as an external check that does not depend on the correction, we examined simple cross-sectional (level-on-level) associations within each era-matched pair between NFHS district mortality and the reference-survey energy metrics measured directly in ACCESS (with NFHS-4) and IRES (with NFHS-5)."),
  h3("2.4.6 Precision-frontier and validation-survey design analysis"),
  p("The precision-frontier analysis asks what a better validation survey would have had to deliver for the propagated mortality association to be conclusive, and whether any feasible survey could deliver it. We index validation quality by a single scalar. Let L be the matrix of saved posterior draws of the corrected district prevalence on the logit scale and L-bar its across-draw mean, which is the point surface used in the fixed-surface analyses. For kappa in [0, 1] we form L(kappa) = L-bar + kappa (L - L-bar), map back through the logistic function, and refit the change-on-change model of Section 2.4.5 by multiple imputation on the shrunken draws, using the same 200 draws, the same covariates, the same weights and the same pooling by Rubin's rules. Shrinking on the logit scale is a pure variance operation on the scale on which the calibration model is linear, so kappa is interpretable as the fraction of the corrected exposure's posterior standard deviation that survives: kappa = 0.5 describes any validation design that halves it. The two endpoints are exact identities rather than approximations - kappa = 1 returns the saved draws and so reproduces the uncertainty-propagated estimate, and kappa = 0 collapses every draw onto the point surface and so reproduces the fixed-surface estimate - and both are verified numerically in the analysis script rather than assumed. We evaluated " + kapN + " values of kappa on [0, 1] and defined kappa* as the value at which the pooled test statistic crosses an absolute z of 1.96."),
  p("Whether a given kappa is attainable depends on where the posterior variance comes from. Each corrected district value is a + b x + u_s, where x is the NFHS district estimate entering the measurement-error model as a covariate observed with known error, so its posterior variance separates into two parts: a calibration-side part, carried by the posterior of the intercept, slope and state effect, which a larger or better-targeted validation survey shrinks; and an NFHS-side part equal to b squared times the NFHS district estimate's own sampling variance, which no validation survey can touch because it is a property of the health survey being corrected. Because the design analysis has to evaluate the calibration-side part under surveys that were never fielded, we did not read it off the fitted models: we reconstructed it analytically, in closed form, as a Bayesian linear mixed model with known within-district variances, averaging over the posterior draws of the residual and state standard deviations rather than conditioning on their posterior medians." + p246Checks() + " Setting the calibration-side part to zero, which is the limit of a validation survey of unlimited size and perfect coverage, leaves a floor kappa_floor, the square root of the NFHS-side share of the total variance across the two legs."),
  p("To check that the floor binds for realizable designs and not only in the limit, we mapped concrete survey designs onto predicted kappa. A grid of " + dGrid.length + " transfer-route designs crosses the number of validation districts (" + dgTxt("D") + "), the number of states covered (" + dgTxt("S") + "), the depth of the reference sample per district (" + dgTxt("m") + " times the realized effective sample), and an instrument-quality factor scaling the fitted residual standard deviation (" + dgTxt("sigma_factor", 2) + "), the last standing in for harmonized fuel-module wording, matched field periods or repeated measurement on the same households rather than for expenditure. " + p246Scale() + "Predicted kappa is computed from the analytic calibration posterior under each design, with a state-coverage schedule that approximately reproduces each real survey's district coverage and a per-leg anchoring constant chosen so that the realized ACCESS and IRES designs return their observed kappa" + p246Anchor() + ". Two levers outside the transfer route were evaluated on the same scale: multiplying the NFHS district sample sizes, evaluated on the grid's most favourable transfer design so that it says what the health survey would have to add on top of a best-case validation survey; and a direct route in which the better instrument is fielded in the health survey's own districts, which removes the calibration step altogether and is bounded only by the instrument's own sampling error. The exercise is a design and power calculation conditional on the fitted calibration and on the current point surface: it varies exposure precision alone, holding the mortality data, the covariate set and the model specification fixed, and it describes a survey that would estimate the same calibration relationship more precisely, not one that would reveal that relationship to be different. " + toySqrt() + "Full results, including the fitted frontier, the variance decomposition and the design grid, are reported in SI Methods S4, Tables S16 and S17, and main-text Figure 3."),

  h3("2.4.7 Supplementary analysis: adult cardiometabolic outcomes (rural districts)"),
  p("As a secondary analysis we applied the same district change-on-change framework to two adult cardiovascular risk factors measured in NFHS, following the design of a prior study of clean cooking and hypertension in India [29]. Hypertension was defined per the JNC7 criteria as a mean of the second and third measured readings with systolic blood pressure at least 140 mmHg or diastolic at least 90 mmHg, or current use of antihypertensive medication; diabetes was the self-reported item (ever told by a health professional of high blood sugar). District prevalences of each were estimated for NFHS-4 and NFHS-5 from the same four-level multilevel specification as Equation 1, among rural adult respondents (to match the rural corrected exposure), and regressed on the district change in primary-LPG prevalence (raw, regression-calibrated, and Bayesian-corrected) with the same socioeconomic and ambient adjustment set and state-clustered standard errors. These outcomes are reported in the Supplementary Materials."),
];

// ============================ TABLE 4 (health) ==============================
const COLW4 = [1500, 3100, 2400, 1000];
const table4 = mkTable(COLW4, [
  ["Outcome", "Exposure measurement", "Est. per 10-pp LPG [95% CI]", "p"],
].concat(healthRows(heRural)).concat(healthRowsUnw(heRural)));
const table4all = mkTable(COLW4, [
  ["Outcome", "Exposure measurement", "Est. per 10-pp LPG [95% CI]", "p"],
].concat(healthRows(heAll)).concat(healthRowsUnw(heAll)));
// Quoted in the Table S7 caption; read back so the caption cannot outlive it.
const P_INF_ALL_BAYES = pfmt((heRow(heAll, "infant", "change_lpg_bayes") || {}).p);
// The Table S7 caption used to read "reaches p = " + P_INF_ALL_BAYES, which
// asserts a threshold crossing. When the all-household infant p moved from
// 0.046 to 0.051 the sentence became false while the number beside it updated
// correctly -- the same failure mode as the hard-coded MCMC verdict. Derive
// the verb from the value instead.
const allBayesPTxt = () => {
  const v = Number((heRow(heAll, "infant", "change_lpg_bayes") || {}).p);
  if (!isFinite(v)) return "has p = NA";
  return v < 0.05
    ? "reaches p = " + P_INF_ALL_BAYES
    : "falls just short of the conventional 0.05 threshold (p = "
      + P_INF_ALL_BAYES + ")";
};

// ============================ RESULTS ========================================
const results = [
  h2("3.1 NFHS and the specialized surveys agree before the policy expansion and diverge sharply after it"),
  p("District-level estimates of primary LPG use were produced for " + N_D_NFHS4 + " (NFHS-4) and " + N_D_NFHS5 + " (NFHS-5) districts on the NFHS-4 geography, for the " + cmpN(C4R) + " ACCESS Wave 1 districts, and for " + cmpN(C5A) + " IRES districts (Figure 1; agreement statistics in Table S2). In the pre-PMUY period the national health survey and the specialized survey agree closely: NFHS-4 rural clusters versus the all-rural ACCESS sample give r = " + cmpV(C4R, "pearson") + " (logit-scale r = " + cmpV(C4R, "pearson_logit") + "), rising to r = " + cmpV(C4RD, "pearson") + " with design-weighted estimates on both sides (Lin's CCC = " + cmpV(C4RD, "ccc") + ") and a mean difference of only " + cmpDiffPP(C4RD) + " percentage points (Figure 1a, c). The weaker correlation obtained when all NFHS-4 households are compared with rural ACCESS (r = " + cmpV(C4A, "pearson") + ") is an artifact of urban/rural composition rather than of measurement, and population weighting changes correlations negligibly (Table S2)."),
  p("After PMUY the picture changes. Agreement between NFHS-5 and IRES remains high in correlation terms (all-household r = " + cmpV(C5A, "pearson") + "; rural r = " + cmpV(C5R, "pearson") + ") but now carries a large, systematic level difference: NFHS-5 rural estimates average " + GAP_ML + " percentage points lower than IRES (design-weighted, rural-vs-rural, " + GAP_DW + " points), across nearly the entire prevalence range (Figure 1b, d). Nor is the gap a composition artifact: comparing all households on both sides - urban and rural together, over all " + cmpN(C5A) + " common districts, the specification that gives the highest correlation of the three (r = " + cmpV(C5A, "pearson") + ", against " + cmpV(C5R, "pearson") + " rural and " + cmpV(C5RD, "pearson") + " rural design-weighted) - leaves the level difference essentially unchanged at " + cmpGap(C5A, 1) + " percentage points, so broadening the frame raises agreement in rank without closing the gap in level (Table S2). This is not a timing artifact - IRES was fielded before most NFHS-5 interviews, so secular growth in LPG use would move the gap the other way - and the corresponding NFHS-4/ACCESS difference is small (" + cmpDiffPP(C4R) + " points rural; " + cmpDiffPP(C4RD) + " design-weighted). The divergence is thus specific to the post-PMUY NFHS-5/IRES comparison, the period in which tens of millions of marginal LPG-owning households entered the denominator."),
  p("Crucially, the disagreement is confined to the fuel item and does not extend to who was surveyed. In a benchmark (falsification) analysis (Table S4; SI Figure S5), district prevalences of demographic characteristics measured comparably in both surveys agree closely in level between NFHS-5 and IRES - mean differences of " + bmDiff(BM5, "st") + " points for Scheduled Tribe, " + bmDiff(BM5, "hindu") + " for Hindu, " + bmDiff(BM5, "muslim") + " for Muslim, and " + bmDiff(BM5, "sc") + " for Scheduled Caste, against " + bmDiff(BM5, "lpg") + " points for primary LPG - so the two surveys sample comparably composed rural populations and the LPG gap does not arise from differential sample composition. Timing does not explain it either: restricting NFHS-5 to pre-COVID interviews barely changes the rural gap (" + SEAS_ALL + " to " + SEAS_PRECOV + " points), and restricting to winter interviews (season-matched to IRES fielding) slightly widens it (to " + SEAS_WINTER + " points), if anything making the gap conservative. The same falsification holds district by district when the NFHS and reference surfaces are mapped side by side on a common scale (SI Figures S17-S18), and the same table shows that even a non-fuel item - possession of a below-poverty-line ration card - shifts with question design (Table S4). Neither sampling, timing, geography, nor demographics explains the LPG gap - it is localized to the cooking-fuel question."),
  img("paper_figs/fig1_agreement.jpeg", 440, 422),
  caption("Figure 1. District-level agreement between NFHS and energy-survey estimates of primary LPG use. (a) NFHS-4 rural vs ACCESS Wave 1 and (b) NFHS-5 rural vs IRES rural, against the identity line (dashed); (c, d) corresponding Bland-Altman plots (solid line = mean difference; dashed = 95% limits of agreement). Point size proportional to reference-survey households; r = Pearson correlation."),

  h2("3.2 The divergence can be calibrated, and the calibration improves held-out prediction"),
  p("Because the discrepancy appears to arise from how cooking fuel is measured rather than from sample composition or timing, we calibrated the NFHS district estimates to the energy surveys with a Bayesian measurement-error model (Table S3). Calibrating NFHS-4 against ACCESS yields a slope near unity on the logit scale (" + calE(CAL4, "slope") + "; 95% CrI " + calCrI(CAL4, "slope") + ") with a small intercept and little between-state variation (SD " + calE(CAL4, "sd_state") + "): NFHS-4 rural estimates track the reference nearly one-for-one, and the correction is correspondingly mild. Calibrating NFHS-5 against the rural design-weighted IRES estimate (with its Taylor-linearized standard error as the reference-side measurement error) yields a slope of " + calE(CAL5, "slope") + " (95% CrI " + calCrI(CAL5, "slope") + ") with a large intercept (" + calE(CAL5, "intercept") + ") and substantial between-state heterogeneity (SD " + calE(CAL5, "sd_state") + "): NFHS-5 compresses true between-district variation and understates prevalence, and the corrected national surface is on average roughly " + CORR_PP + " percentage points higher than the raw rural estimates (national mean rising from " + RAW19_0 + "% to " + BAY19_0 + "%), with the largest upward revisions across the Indo-Gangetic plain and central India (Figure 2a, b); district-level corrections against the raw estimates are shown in Figure 2c."),
  p("The correction improves out-of-sample agreement, not merely in-sample fit. In leave-one-state-out cross-validation, a calibration estimated without a given state reduces the root-mean-square error of that state's district estimates against the held-out reference in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states, with the median held-out RMSE falling from " + LOSO_MED_RAW + " to " + LOSO_MED_CORR + " (exceptions: Chhattisgarh and Kerala, with Odisha essentially unchanged). Given the magnitude of the implied correction, verification of the IRES primary-fuel coding and of possible survey-mode effects (a detailed energy module eliciting fuller LPG reporting than a single item) are priorities we return to in the Discussion; the direction of the correction, however, is robust to the timing argument above."),
  img("paper_figs/fig2_correction.jpeg", 460, 161),
  caption("Figure 2. Correcting the NFHS-5 district estimates. (a) Corrected district prevalence of primary LPG use (Bayesian measurement-error model, IRES-calibrated); (b) correction shift relative to raw NFHS-5 rural estimates (red = corrected higher); (c) corrected versus raw district estimates (red = Bayesian, blue = regression calibration; dashed = identity)."),

  h2("3.3 Correction removes bias but does not create information"),
  p("A calibrated exposure surface is routinely treated, downstream, as though it were a measurement: adopted from its source, carried into a health model, and held fixed. That treatment is not safe here, and the reason is structural rather than particular to these surveys. Each corrected district value is a + b x + u_s, where x is the NFHS district estimate that enters the measurement-error model as a covariate observed with known error. Its posterior variance therefore separates into two parts. One is calibration-side, carried by the posterior of the intercept, slope and state effect, and a larger or better-targeted validation survey shrinks it. The other is b^2 times the NFHS district estimate's own sampling variance, and no validation survey can touch it at all, because it is a property of the health survey being corrected rather than of the survey used to correct it. The decomposition (SI Table S17) puts the NFHS-side part at " + DS("var_nfhs_side_share_2015", 0) + "% of the total on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg. The two legs differ because the calibration relationship is itself far noisier after the policy expansion: the fitted residual SD is " + vd("2019", "sigma", 3) + " on the logit scale in 2019 against " + vd("2015", "sigma", 3) + " in 2015, roughly " + sigRatio(1) + " times larger, so on the 2019 leg the calibration dominates the budget while on the 2015 leg the health survey's own noise is close to half of it."),
  p("What that irreducible part costs can be stated in the currency of the analysis it feeds. Index the precision of the correction by kappa, the fraction of the corrected exposure's posterior standard deviation (logit scale) that a hypothetical validation survey leaves in place: kappa = 1 is the correction as achieved, kappa = 0 treats it as exact. Refitting the change-on-change mortality model on posterior draws shrunk by kappa traces a precision frontier (Figure 3; SI Methods S4, Table S16); both endpoints are identities rather than approximations, and are checked numerically. The infant association reaches conventional significance at " + kappaStarTxt() + ", and " + neoTxt() + ". Setting the calibration-side variance to zero - a validation survey of unlimited size, perfect state coverage and a perfect instrument - still leaves kappa at a floor of " + DS("kappa_floor", 2) + ", " + floorTxt() + " the value the infant analysis requires, so " + floorVerdict() + ". The floor is the substantive point, and it is a statement about the health survey rather than about the validation survey: because it is set by the NFHS district estimates the calibration is applied to, the only levers that move it act on NFHS itself - a denser sample, a better exposure item, or a coarser analytic unit. A grid of " + dGrid.length + " concrete validation designs, and the NFHS-side sampling multipliers a fixed design would demand, are reported in SI Methods S4; none of them changes this conclusion."),
  p("One feature of the calibration points the same way. The post-expansion residual is equation error rather than sampling error - the model already carries each reference district's own sampling variance as a known quantity, so what remains is systematic disagreement between NFHS-5 and IRES in wording, respondent, or field timing - and sampling more reference households does not reduce it. What would improve a validation survey for this purpose is harmonized definitions, matched field periods and ideally repeated measurement on the same households, rather than simply a larger sample."),
  ...(fs.existsSync(FIG + "design_frontier.jpeg") ? [
    img("design_frontier.jpeg", 460, 320),
    caption("Figure 3. The precision frontier for the corrected-exposure child-mortality association, and the floor the health survey imposes on it (SI Methods S4, Table S16). The horizontal axis is kappa, the fraction of the corrected exposure's posterior standard deviation (logit scale) that a hypothetical validation survey leaves in place: kappa = 1 is the study as conducted, kappa = 0 treats the correction as exact. Panels show the pooled coefficient with its 95% confidence interval, the pooled p-value against the 0.05 threshold, and the fraction of missing information, for infant and neonatal mortality. " + (ksReach("infant") ? "The infant curve crosses p = 0.05 at kappa* = " + DS("kappa_star_infant", 2) + ", a precision gain of " + DS("precision_gain_infant", 1) + "-fold." : "The infant curve does not cross p = 0.05 at any attainable kappa.") + " The vertical reference line at kappa_floor = " + DS("kappa_floor", 2) + " marks the smallest kappa achievable by any transfer-function validation survey, however large, because the corrected value's posterior variance retains the NFHS district estimate's own sampling error scaled by the squared calibration slope (SI Table S17). Rural districts, adjusted specification, 200 imputations."),
  ] : []),

  h2("3.4 Demonstration: the corrected exposure and infant mortality"),
  p("What that distinction costs a downstream analysis can be shown directly, in the analysis the corrected surface was built for. Across the " + N_RURAL + " rural districts with mortality estimates, corrected exposure and complete covariates, we estimated the district change-on-change association between primary-LPG prevalence and child mortality twice: once in the way an analyst adopting a published corrected surface would, holding the exposure fixed at its posterior mean, and once propagating the correction's own uncertainty by multiple imputation (drawing each district's corrected 2015 and 2019 prevalence from its full posterior, forming the change, refitting on each of 200 draws, and pooling by Rubin's rules). The two analyses use the same districts, covariates and specification, and differ only in whether the corrected exposure is treated as a measurement or as an estimate (SI Methods S6)."),
  p("They do not support the same conclusion. Holding the corrected surface fixed, the infant association is " + est10(R_I_BAY) + " deaths per 100 births per 10-percentage-point rise in primary-LPG prevalence (95% CI " + ci10(R_I_BAY) + ", p = " + pfmt((R_I_BAY || {}).p) + ") - an interval that excludes zero. Propagating the correction's uncertainty, the same association is " + est10(R_I_MI) + " (95% CI " + ci10(R_I_MI) + "; " + miSigTxt(R_I_MI) + "), and the neonatal association is " + est10(R_N_MI) + " (95% CI " + ci10(R_N_MI) + "). The interval widens rather than narrows because the regressor is itself an estimate, and the fraction of missing information at the propagated estimate - " + nfmt(100 * DSn("fmi_infant_kappa1"), 0) + "% for infant and " + nfmt(100 * DSn("fmi_neonatal_kappa1"), 0) + "% for neonatal mortality - says that exposure uncertainty, not the number of districts, is what the width is made of. We report the propagated estimate as the result and the fixed-surface estimate explicitly as the analysis that ignores calibration uncertainty. The full set of exposure specifications, the model and weighting details, the sensitivity analyses and the restrictions to districts with calibration support are given in SI Methods S6 (Table S6; SI Figure S21); two of them - the monotone strengthening of the estimates as measurement error is removed, and the sensitivity of the era-matched estimate to which reference instrument calibrates each endpoint - point the same way as the contrast reported here."),
  p("The lesson of the contrast is not the mortality estimate. It is that two analyses of the same data, differing only in whether a calibrated exposure is treated as a measurement or as an estimate, disagree about whether an association is present - and that the disagreement is not one a better validation survey would resolve, because " + floorVerdict() + ". We therefore do not read these data as establishing a mortality benefit of the clean-cooking transition, and we do not read them as excluding one. Nothing in the mechanism is specific to cooking fuel or to child mortality: any exposure surface that was estimated rather than observed carries a dispersion of its own, and adopting one as though it had been measured can report a decisive result where the design supports none. The secondary analyses - predicted fuel stacking and consumption, and adult cardiometabolic outcomes - are reported in SI Methods S7."),
];

const discussion = [
  p("A specialized validation survey can identify and substantially correct systematic exposure error in a national health survey. It cannot recover district-level information that the health survey never measured precisely enough in the first place. Those two statements are this paper's methodological result. Its empirical result stands independently of them and is equally substantive: a single item in one of the world's largest health surveys changed its relationship to household fuel use over exactly the period a national program was changing that use, and a specialized survey can demonstrate that, localize it to the question rather than to the sample, and repair it. The analysis separates the two cleanly. Before India's LPG expansion, NFHS-4 and ACCESS agreed on rural primary-LPG prevalence to about one percentage point on design-weighted estimates; after it, NFHS-5 recorded roughly " + GAP_DW + " percentage points less primary LPG use than IRES, while the same surveys continued to agree on demographic composition and neither field timing, season, weighting nor rural-urban composition accounted for the difference. Calibrating against the energy surveys materially changed the national exposure surface and improved agreement with held-out reference estimates in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states. But the corrected exposure inherits an uncertainty floor from the NFHS district estimates the calibration is applied to - " + DS("var_nfhs_side_share_2015", 0) + "% of the corrected value's posterior variance on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg - and that floor lies " + floorTxt() + " the precision a district-level health analysis of this exposure would need. The methodological consequence is the paper's central claim: correction removes bias, but it does not create information, and an analysis that forgets the difference can turn an inconclusive result into an apparently significant one. Neither half of that statement is specific to cooking fuel. Any externally produced exposure surface is an estimate attached to a health dataset that was not designed around it, and inherits the same two-part structure: a component the exposure product's own design controls, and a component belonging to the health data it is joined to."),

  p("The exposure measures that make national health surveys so valuable are optimized for breadth and low marginal cost, not for fidelity to any one exposure. NFHS, like the Demographic and Health Surveys worldwide, must capture dozens of domains in a single visit, and cooking energy is reduced to one primary-fuel item with no probing of secondary fuels, cylinder refills, or quantities - reported by a general household respondent rather than the primary cook, and within a general questionnaire rather than a dedicated energy module. Our central empirical finding is that this design choice matters: two independent energy surveys that ask about cooking fuel within such a module agree with each other and disagree with NFHS-5 by about " + GAP_DW + " percentage points, while agreeing with NFHS on every demographic benchmark measured comparably. The households the NFHS item misclassifies are not a random subset - they are disproportionately the marginal LPG adopters created by policy, precisely the households a single 'main fuel' question cannot resolve because they stack fuels. Even a non-fuel item in our data (the ration-card question) shifts with how it is asked, underscoring that the effect is about instrument design, not about cooking fuel alone. The tension between scale and fidelity is general: drinking-water source, sanitation, secondhand smoke, and dietary intake are each commonly captured by single survey items whose error structure is rarely characterized, and specialized surveys are the natural instrument for characterizing it."),

  p("Detecting a discrepancy is less useful than correcting it. Treating the energy surveys as higher-fidelity - though not infallible - references, the Bayesian measurement-error model raises the estimated national rural prevalence of primary LPG use by roughly " + CORR_PP + " percentage points relative to raw NFHS-5 (a national rural mean rising from " + RAW19_0 + "% to " + BAY19_0 + "%), concentrated across the Indo-Gangetic plain and central India, and does so in a way that improves out-of-sample agreement in leave-one-state-out cross-validation rather than merely fitting the calibration data. The two eras behave differently in an interpretable way: the NFHS-4/ACCESS calibration is near-identity (the pre-PMUY item performed well), whereas the NFHS-5/IRES calibration implies substantial compression and understatement (the post-PMUY item performed poorly, in the exact period when marginal adopters entered). We are explicit that this yields an exposure surface calibrated to a better-instrumented reference, not a validated measurement of true exposure; the evidence that it is closer to the truth rests on measurement-error theory, internal cross-validation, and the benchmark falsification test, and would be strengthened by a dataset with measured household air pollution or a solid-fuel-smoke biomarker."),

  p("The corrected surface is an estimate, and the practice this paper is ultimately about - attaching an externally calibrated exposure surface to health data and analysing it as though it were measured - discards that fact. Here the consequence is not a confidence interval that is slightly too narrow. Holding the corrected surface fixed, the infant association is nominally significant - its interval excludes zero; propagating the correction's own uncertainty, it is not; and the fraction of missing information at the propagated estimate (" + nfmt(100 * DSn("fmi_infant_kappa1"), 0) + "%) shows that the exposure, rather than the number of districts, is what the interval is made of. The direction of movement across specifications is consistent with attenuation by measurement error and we report it as such, but we do not read it as evidence that correcting the exposure has revealed a health benefit: the same corrected estimate loses its significance when a single calibration instrument is applied at both endpoints, and again when the correction's uncertainty is carried through. The generalizable claim is about the analysis rather than about cooking fuel. Calibrated, modeled and predicted exposure surfaces are increasingly published and reused - satellite and chemical-transport estimates of fine particulate matter, land-use regression and noise models, machine-learning exposure predictions, statistically downscaled temperature and precipitation fields - and in each case the health analyst receives a point estimate per unit and, at best, an uncertainty layer that the analysis does not use. The incentive to treat such a surface as a fixed input is strong, because doing so is simple and makes results sharper. It is also wrong in a specific and hard-to-detect way, and the remedy is cheap: a calibrated exposure product should travel with its posterior draws, be analysed by multiple imputation or an equivalent, and be reported under both treatments so that a reader can see how much of the conclusion rests on holding the surface fixed."),

  p("The natural response to an inconclusive propagated estimate is to ask for a better validation survey, and the answer here is that no feasible validation survey would suffice. The corrected exposure's posterior variance is only partly calibration-side; the remainder is the NFHS district estimate's own sampling error propagated through the squared calibration slope, and it is invariant to how the validation survey is designed. Setting the calibration-side part to zero - a validation survey of unlimited size, perfect state coverage and a perfect instrument - still leaves the corrected exposure less precise than the infant analysis requires. A grid of concrete designs spanning validation districts, states covered, sampling depth per district and instrument quality, and the household counts a direct-measurement alternative would demand, is reported in SI Methods S4; none of them changes the conclusion, whose message is qualitative rather than a sample-size calculation: the remedy has to change the health survey's exposure measurement, or the scale at which the analysis is conducted, not the size of the external survey used to correct it. Two practical implications follow. A modest expansion of the health-survey instrument - a small number of secondary-fuel and consumption items, or a short fuel module administered to the primary cook - acts on the variance where it actually is. And where the instrument cannot be changed, analysis above the district, at a scale on which the health survey's own sampling error is small relative to the exposure contrast, is the honest alternative to a district design that cannot answer the question. There is a design lesson for the validation survey too, even though it cannot solve the problem alone: on the post-PMUY leg the fitted residual is equation error rather than sampling error, so what improves a validation survey for this purpose is harmonized definitions, matched field periods and ideally repeated measurement on the same households, rather than simply a larger sample."),

  p("Everything in this analysis lives at the district: the exposure, the correction, the outcome, and the association between them. That is a consequence of the data rather than a modelling preference. NFHS and the energy surveys share no households, so the transfer function is identifiable only between district aggregates, and the corrected surface accordingly says how many households in a district the NFHS item misclassifies, not which ones. The district-level association therefore carries the standard ecological caveat, and here there is a specific reason to expect the household-level relationship to differ rather than merely to be noisier: the households the correction moves are the marginal ones - connected but stacking, or using LPG intermittently - and they are unlikely to carry either the exposure or the mortality risk of the average household in their district. Aggregation is not purely a threat. District means average away much of the household-level noise in the fuel item, and district-level supply is the scale at which the policy under study actually operates, so the district is the right unit for some questions; it is simply not a household-level dose-response. The way forward is linkage rather than aggregation. Administering the richer fuel module to a subsample of NFHS households would convert a transfer problem into a classical measurement-error problem with an internal validation sample, which identifies the household-level misclassification structure rather than only its district-level average, permits individual-level correction by regression calibration, simulation-extrapolation or a Bayesian latent-exposure model with mortality analysed per birth, and removes the calibration variance from the analysis rather than shrinking it. It is also the cheaper design, because an internal validation sample need only characterize how the item errs, not estimate every district precisely - which is what both the external validation survey and the direct-measurement arm evaluated in SI Methods S4 are obliged to do."),

  p("A second asymmetry runs the other way: we correct one side of the regression and treat the other as measured. The district mortality estimates are themselves survey estimates built on modest numbers of births. The median study district contributes " + medBirths("2015") + " rural births to the ten-year window in the earlier round and " + medBirths("2019") + " in the later one, and the design-based standard error of a district infant mortality probability has a median of " + medSE100("infant", "2015") + " and " + medSE100("infant", "2019") + " deaths per 100 births in the two rounds. Differencing two such estimates roughly doubles the sampling variance while cancelling whatever is stable about a district, so the change-on-change outcome is considerably noisier than either level. Applying to the outcome the same decomposition we applied to the exposure, only about " + nfmt(100 * OSS_INF, 1) + "% of the across-district variance in the change in infant mortality, and " + nfmt(100 * OSS_NEO, 1) + "% of the variance in the change in neonatal mortality, is genuine between-district variation rather than sampling error. Classical nondifferential error in the dependent variable of a linear regression does not bias the slope; it inflates the residual variance and widens the interval. It is therefore not an alternative explanation for the movement of the estimate across specifications, but it does cap what exposure precision alone can buy, and the neonatal result shows that cap binding: with the calibration uncertainty set to zero the neonatal association still reaches only |z| = " + DS("z_at_kappa0_neonatal", 2) + ". Two caveats qualify the benign reading. Error in the outcome that is correlated with error in the exposure - both are produced by the same NFHS interview in the same district - would bias the slope, and nothing in this design rules it out. And the primary district mortality estimates come from the same partial-pooling multilevel model used for the exposure, whose shrinkage suppresses sampling noise and genuine between-district contrast together, which is why the design-weighted direct estimates are carried alongside as a sensitivity throughout. Birth-history recall error, omission of early neonatal deaths, and age heaping at twelve months are further sources this design does not address."),

  p("It is worth stating plainly what follows if there is no district-level relationship to detect. The design analysis asks how precise the exposure would have to be for the estimate we observe to reach conventional significance, which presumes that estimate is approximately right; if the true district-level association is null, kappa* is a target defined by noise, and a study built to reach it would deliver a precisely estimated null rather than a significant effect. The evidence bearing on this is not uniform. The one check that does not depend on the correction is the association between the energy surveys' own directly measured fuel use and district mortality, and it corroborates the expected directions - positive for solid fuel, negative for primary LPG - unevenly across the two eras. For solid-fuel burning that association is " + heDir(HE_A, "Any solid-fuel burning") + " in the ACCESS/NFHS-4 era and " + heDir(HE_I, "Any solid-fuel burning") + " in the IRES/NFHS-5 era (" + hePair("Any solid-fuel burning") + "); for primary-LPG prevalence it is " + heDir(HE_A, "Primary LPG") + " and " + heDir(HE_I, "Primary LPG") + " respectively (" + hePair("Primary LPG") + "). The ACCESS-era correlations rest on only " + cmpN(C4R) + " districts in six northern states, so they are weak evidence in either direction. Several considerations would make a small or absent district-level relationship unsurprising: personal exposure to fine particulate matter is only weakly proxied by the prevalence of a primary cooking fuel; the change in district prevalence over a single inter-survey interval is modest and confounded with everything else that improved between the rounds; and infant mortality has many causes, of which household air pollution is among the smaller. These data cannot distinguish an imprecision-limited null from a substantive one. What does not depend on the answer is the measurement result, which rests on the survey comparison and the calibration rather than on the health model. What does depend on it is the recommendation: if the district-level relationship is genuinely small, the response is neither a larger validation survey nor a more precisely corrected district exposure, but a different design - individual exposure linked to individual outcomes, or an outcome nearer the exposure than infant mortality, such as measured personal fine particulate exposure, respiratory symptoms or birthweight."),

  p("The procedure generalizes beyond cooking fuel: estimate a shared, simplified proxy at common spatial resolution in both surveys, calibrate the health-survey estimate against the specialized survey with a measurement-error model that carries its sampling uncertainty, and - where the health survey omits a construct entirely - transfer a prediction model trained in the specialized survey, validated out of sample. Calibration of a measured item is the robust core; augmentation of a missing construct is more fragile, as our secondary stacking analysis shows (moderate accuracy, and determinants that did not transfer across policy eras), so behavioural models should not be moved across policy regimes without revalidation. Even so, the same design could improve exposure assessment for electrification, water quality, sanitation, secondhand smoke, and dietary intake wherever a large survey carries a crude item and a specialized survey a richer one - producing an augmented, uncertainty-quantified exposure product from a scaled survey's coverage and a specialized survey's measurement depth."),

  p("The template has a limit that this study is unusually well placed to state, because it applies to the calibration step and not only to the augmentation step. Calibration transfers the level and the shape of a relationship from a better instrument to a worse one; it does not transfer precision. Wherever the health survey's own estimate at the analytic unit is noisy - and district-level estimates from a national health survey generally are - the corrected value inherits that noise, and the ceiling on what the design can detect is set before the specialized survey is fielded. The right time to ask whether an external validation survey will make a health analysis answerable is therefore before it is commissioned, by decomposing the corrected exposure's variance for the analysis actually intended, which costs nothing beyond the calibration model already being fitted."),

  p("These results carry implications for survey design, for exposure assessment, and for how corrected exposures are used. For survey practice, the cheapest fix lies inside the health survey: a small number of secondary-fuel and consumption items, or periodic linkage to a specialized exposure survey on a matched frame, would improve exposure fidelity at low marginal cost and, unlike a larger external validation survey, would act on the part of the uncertainty that binds. For exposure assessment, corrected and uncertainty-quantified district surfaces are a more defensible input to household-air-pollution burden estimation than a raw primary-fuel classification - provided the uncertainty travels with the surface and is used rather than discarded at the point of analysis. For epidemiological practice, the specific recommendation is that analyses adopting an externally calibrated exposure product should propagate its uncertainty and report the result under both treatments, as we do here, since the difference between them is not reliably small. For clean-cooking policy, the finding that bears on monitoring is a measurement one: a single primary-fuel item stopped tracking household fuel use in exactly the period when the policy was doing most of its work, so stacking, rather than connection counts or a main-fuel classification, is what the transition needs to be monitored on. Most broadly, the study argues for treating a survey exposure item not as the exposure itself but as one noisy measurement to be calibrated against better instruments - and for treating the calibrated result, in its turn, as an estimate rather than a measurement."),

  h3("4.1 Limitations"),
  p("Several limitations bound these conclusions. First, the energy surveys are higher-fidelity references, not infallible gold standards: they have their own sampling designs, recall limitations, and non-response (IRES non-response averaged 26% and was higher among wealthier households, and neither survey's weights carry a non-response adjustment). We therefore frame them as better-measured references rather than truth; we note that wealthier-household non-response in IRES, given that LPG use rises with wealth, would if anything bias IRES prevalence downward and thus makes the observed NFHS-IRES gap conservative. Second, and most important, we have no independent validation that the corrected exposure is closer to the true household exposure; the case is theoretical and internal, and a dataset with measured household air pollution or a biomarker of solid-fuel smoke would be required to establish it. Third, we characterize but do not prove the mechanism of the NFHS-IRES discrepancy: differential questionnaire design is a leading candidate, but interviewer effects, item wording, the identity of the respondent (dedicated energy surveys interview the primary cook, whereas NFHS interviews a household respondent, and household members answer energy questions differently) [33], survey mode, and differential non-response could each contribute, and our own ration-card benchmark shows that even a non-fuel item is sensitive to how it is asked. Fourth, the health analysis is ecological, using district change-on-change with a rural exposure against rural-birth mortality across " + N_RURAL + " districts (an all-birth version is nearly identical, SI Table S7); it is associational rather than causal, and remains imprecise because district-level exposure change over a single intersurvey interval carries limited variance - so that once the correction's own uncertainty is carried through, the interval is wide enough to remain consistent with a range of effect sizes. We therefore treat the health analysis as an illustration of what propagation does to inference, not as an estimate of the health return to LPG. The predicted fuel-composition metrics (LPG-no-solid-reported share, stacking, consumption) do not yield coherent health associations and are reported only as exploratory (SI Table S8, SI Figure S10), reflecting both their limited temporal transportability and their mutual collinearity; corrected primary-LPG prevalence remains our exposure of record. Fifth, the two national corrected surfaces rest on very different amounts of geographic support, and the NFHS-4 surface should be read accordingly. Only " + SUPP15 + " of the " + N_D_NFHS4 + " NFHS-4 districts we correct fall inside one of the six ACCESS states, so roughly two-thirds of the corrected NFHS-4 surface is an extrapolation of a calibration relationship estimated in six poor northern states to states where it was never observed; by contrast, " + SUPP19 + " of the " + N_D_NFHS5 + " NFHS-5 districts fall inside an IRES state, so the corrected NFHS-5 surface is very largely an interpolation within the 21-state, 97%-of-population range on which it was fit. We therefore treat the corrected NFHS-5/IRES surface as the defensible national product and read the corrected NFHS-4 surface outside the six ACCESS states as illustrative extrapolation that should carry little weight; the ACCESS calibration's principal value is that it independently reproduces the direction and approximate magnitude of the NFHS-5/IRES correction in the one region where an era-matched reference exists, not that it delivers a trustworthy national 2015 surface on its own. The near-identity of raw and corrected NFHS-4 estimates in the pre-PMUY period limits the practical harm of this extrapolation, but it remains an assumption these data cannot verify. Finally, the stacking models, consistent with the cross-era result, have only modest household-level discrimination and limited temporal transportability."),
];

const refs = [
  "World Health Organization. Household air pollution and health. Fact sheet. Geneva: WHO; 2024. https://www.who.int/news-room/fact-sheets/detail/household-air-pollution-and-health",
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
];

// The Meng marker in Section 2.4.3 is a literal number in prose. This makes any
// drift between that number and the reference list a build-time failure rather
// than a wrong citation in a submitted manuscript.
if (refs[MENG_NO - 1] !== MENG_REF) {
  throw new Error("reference numbering drift: [" + MENG_NO + "] is not the Meng (1994) entry (refs has " + refs.length + " entries)");
}
console.log("[build] reference [" + MENG_NO + "] = Meng (1994); cited once, Section 2.4.3 estimand paragraph; refs has " + refs.length + " entries");

// NFHS variable inventory. Names and code values are taken from 01_prep_nfhs.R,
// which builds every NFHS quantity used in the paper, so this list and the
// analysis code cannot drift apart silently.
const suppNfhs = [
  "Cooking fuel (1: Electricity, 2: LPG, 4: Natural gas, 5: Biogas, 95: No food cooked in the household) fuel. The primary-LPG indicator analysed throughout is code 2 only. The clean-primary-fuel variant used in sensitivity analysis is codes 1, 2, 4, 5 and 95. This is the harmonized cooking-fuel variable in the analysis extract; its coding is not the standard DHS hv226 coding and is documented in 01_prep_nfhs.R.",
  "Urban or rural residence (1: Urban, 2: Rural) hv025; rural households are hv025 = 2",
  "State hv024; district DHSREGCO from the DHS geographic dataset, merged to the household file on cluster hv001",
  "Number of household members hv012",
  "Household electricity (0: No, 1: Yes; any other code set to missing) hv206",
  "Wealth: the continuous wealth-index factor score hv271, with the quintile hv270 as fallback. Converted to within-state quintiles so that it aligns with the within-state monthly-expenditure quintiles used in ACCESS and IRES, and is therefore interpretable as relative economic rank within a state rather than absolute wealth.",
  "Below-poverty-line or Antyodaya ration card: sh58 in NFHS-4 and sh75 in NFHS-5 (code 8 set to missing)",
  "Caste of the household head (1: Scheduled Caste, 2: Scheduled Tribe, 3: Other Backward Class, 4: General): sh36 in NFHS-4 and sh49 in NFHS-5. Caste column names differ across Household Recode releases; 01_prep_nfhs.R warns and sets caste to missing if the named column is absent, rather than failing silently.",
  "Religion of the household head (1: Hindu, 2: Muslim, remaining codes: Other) hh_religion. Missing and unknown values are kept as missing rather than absorbed into the Other category.",
  "Bank account (code 8 set to missing) hv247",
  "Survey design: household weight hv005 divided by 1,000,000; primary sampling unit hv021; sampling stratum hv022",
];

const suppAccessEnergy = [
  "Use grid electricity (0: No, 1: Yes) m2_q55_grid","Electricity (0: No; 1: Yes) m2_q68_elec",
  "Hours of available electricity (hours/day) m2_q69_elec_hrs","Years of using grid electricity m2_q55_1_grid_years",
  "Fees for grid electricity connection m2_q55_2_grid_fees","Monthly spending for grid electricity m2_q55_3_grid_spending",
  "Amount of kerosene from PDS (l/month) m2_q61_4_kero_liter_PDS","Price of kerosene from PDS (Rs/l) m2_q61_5_kero_price_PDS",
  "Primary source of lighting (1: Grid electricity, 2: Kerosene lamp/lantern, 3: Microgrid, 4: Solar, 5: Others)",
  "LPG for cooking (0: No, 1: Yes) m4_q103_lpg","Firewood for cooking (0: No, 1: Yes) m4_q109_firewood",
  "Main cooking fuel (1: Firewood and Chips, 2: Dung cakes, 3: LPG, 4: Others) m5_q118_main_cookfuel",
  "Main cooking fuel, other m5_q118_main_cookfuel_other",
];
const suppAccess = [
  "Age m1_q19_age","Gender (0: Female, 1: Male) m1_q20_gender","Religion (Hindu, Muslim, Others) m1_q24_religion",
  "Education (1: No formal schooling, 2: Up to 5th standard, 3: Up to 10th standard, 4: 12th standard or diploma, 5: Graduate and above) m1_q23_edu",
  "Caste (1: Scheduled Caste, 2: Scheduled Tribe, 3: Other Backward Class, 4: General) m1_q25_caste",
  "Ration card (0: None, 1: APL, 2: BPL, 3: Antyodaya) m1_q26_ration","Adults in household m1_q27_no_adults",
  "Number of children in household m1_q29_no_children",
  "Primary source of income m1_q31_income_source","Monthly expenditure m1_q32_month_expenditure",
  "Savings per year m1_q33_year_save","Bank account (0: No, 1: Yes) m1_q34_bank_acc",
  "Amount of land (acres) m1_q36_land_final","Household decisionmakers m1_q38_decision_maker",
  "Household type (1: Pucca, 2: Mixed, 3: Kaccha)","Household owns the house (0: No, 1: Yes) m1_q40_house_own",
  "Household has toilet (0: No, 1: Yes) m1_q41_toilet","Household has piped water (0: No, 1: Yes) m1_q42_piped_water",
  "Number of rooms m1_q43_no_rooms","Electricity (0: No, 1: Yes) m2_q68_elec",
  "LPG cylinder purchases: large from distributor/market (m4_q103_5, m4_q103_6); small from distributor/market (m4_q103_8, m4_q103_9)",
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
const mainChildren = [
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120 },
    children: [r("Calibration removes bias but does not create information: evidence from India's clean-cooking transition", { bold: true, size: 30 })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240 },
    children: [r("Supplementary Materials are provided in a companion document.", { italics: true, size: 22 })] }),

  h1("Abstract"),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Background: ", { bold: true }),
    r("The Demographic and Health Surveys, designed to measure maternal and child health, are among the few nationally representative data sources in low- and middle-income countries, and are widely repurposed for environmental exposure assessment. Studies of household air pollution in India derive cooking-related exposure from a single National Family Health Survey (NFHS) primary-fuel item that is blind to fuel stacking. Whether such an item remains comparable across a period in which policy changes which households own which fuels, and what an external validation survey can do about it, has not been established.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Objectives: ", { bold: true }),
    r("We asked whether the NFHS cooking-fuel item retained a stable relationship to household fuel use across India's clean-cooking transition, whether specialized energy surveys can calibrate the resulting error, and - the general question that motivates the paper - what happens when a calibrated exposure surface is carried into a health analysis as a measurement rather than as the estimate it is.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Methods: ", { bold: true }),
    r("We estimated district-level primary-liquefied petroleum gas (LPG) prevalence in NFHS-4 (2015-16), NFHS-5 (2019-21), the ACCESS survey (2014-15), and the India Residential Energy Survey (IRES, 2019-20) with a common multilevel small-area model, and compared them on like-for-like design-weighted rural estimates alongside demographic characteristics measured comparably in both. Treating the energy surveys as higher-fidelity references, we corrected the NFHS estimates by regression calibration and a Bayesian measurement-error model, validated the correction by leave-one-state-out cross-validation, and decomposed the corrected exposure's posterior variance into a calibration-side component, which a larger or better validation survey reduces, and a health-survey-side component, which it cannot. As a demonstration we estimated district change-on-change associations between primary-LPG prevalence and child mortality, first holding the corrected exposure fixed and then propagating the correction's own uncertainty by multiple imputation.") ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Results: ", { bold: true }),
    r("NFHS-4 and ACCESS agreed closely for rural districts before the policy expansion (mean difference about one percentage point), whereas NFHS-5 afterwards recorded roughly " + GAP_DW + " percentage points less primary LPG use than IRES on like-for-like design-weighted rural estimates. The same survey pairs agreed on demographic composition, and neither field timing, season, weighting, nor rural-urban composition accounted for the difference, confining it to the cooking-fuel item. Calibration materially changed the national exposure surface (mean rural primary LPG rising from " + RAW19_0 + "% to " + BAY19_0 + "%) and improved agreement with held-out reference estimates in " + LOSO_IMPROVED + " of " + LOSO_TESTED + " states. Correction did not, however, create information. Only part of the corrected exposure's posterior variance is calibration-side: " + DS("var_nfhs_side_share_2015", 0) + "% of it on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg is the NFHS district estimate's own sampling error, which no validation survey can reduce. Eliminating the calibration-side component entirely would still leave the corrected exposure less precise than a district-level health analysis of this size requires. In the health demonstration, holding the corrected surface fixed gave a nominally significant infant association (" + nest10(R_I_BAY) + " fewer deaths per 100 births per 10-percentage-point rise in primary-LPG prevalence, p = " + pfmt((R_I_BAY || {}).p) + "), whereas propagating the correction's uncertainty left the same association inconclusive (" + nest10(R_I_MI) + ", 95% CI " + nci10(R_I_MI) + "; " + miSigTxt(R_I_MI) + ") - the same districts, covariates and specification, differing only in whether the corrected exposure was treated as fixed."), ] }),
  new Paragraph({ spacing: { after: 120 }, children: [
    r("Discussion: ", { bold: true }),
    r("A single survey item can lose comparability when policy changes the population it is asked of. That is an empirical finding of consequence for every use of these data, and a specialized survey can detect it, localize it to the question by falsification against comparably measured characteristics, and correct the resulting level and compression bias out of sample. But correction is not information: because the calibration is applied to district estimates the health survey itself measured imprecisely, the corrected exposure carries an uncertainty floor that no external validation design can lower, and an analysis that adopts a calibrated exposure surface as though it were fixed can report an apparently decisive health result that the design does not support. That last point is not specific to cooking fuel: exposure products of every kind - calibrated, modeled or predicted - are estimates carrying a dispersion of their own, and are routinely analysed as though they had been observed. Improving district-level environmental health inference from national health surveys therefore requires better exposure measurement within those surveys, or analysis at a coarser scale, rather than a larger external validation survey - and, whatever the exposure product, an inference that carries its uncertainty rather than discarding it."), ] }),
  new Paragraph({ spacing: { after: 200 }, children: [
    r("Keywords: ", { bold: true, italics: true }),
    r("exposure assessment; measurement error; survey validation; uncertainty propagation; clean cooking; household air pollution; small-area estimation; India", { italics: true }) ] }),

  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 60, after: 60 },
    children: [new ImageRun({ type: "png",
      data: fs.readFileSync(FIG + "fig_overview.png"),
      transformation: { width: 415, height: 537 } })] }),
  caption("Graphical overview. Two specialized household energy surveys (ACCESS, IRES) are used to evaluate, calibrate, and augment the simplified cooking-fuel exposure measured by a national health survey (NFHS): common district-level estimation of a shared exposure, cross-survey validation, measurement-error calibration, augmentation with predicted fuel stacking and consumption, and the resulting improved exposure metrics applied to health analyses. The framework generalizes to other environmental exposures captured by single survey items."),

  h1("1. Introduction"), ...intro,

  h1("2. Data and Methods"),
  h2("2.1 Study design overview"), ...methodsOverview,
  h2("2.2 Data sources"),
  h3("2.2.1 National Family Health Surveys (NFHS)"), ...nfhsText,
  h3("2.2.2 Access to Clean Cooking Energy and Electricity - Survey of States (ACCESS)"), ...accessText,
  h3("2.2.3 India Residential Energy Survey (IRES)"), ...iresText,
  h2("2.3 Cooking-fuel exposure indicators and harmonized covariates"), ...measuresText,
  h2("2.4 Statistical analysis"),
  h3("2.4.1 Deriving district-level estimates of exposure"), ...statText,
  h3("2.4.2 Cross-survey comparison"), ...compareText,
  h3("2.4.3 Correcting the rural NFHS estimates (measurement-error models)"), ...correctionText,
  h3("2.4.4 Augmenting NFHS with predicted fuel use (secondary analysis)"), ...predictText,

  h1("3. Results"), ...results,

  h1("4. Discussion"), ...discussion,

  h1("References"),
  ...refs.map((t, i) => p(`${i + 1}. ${t}`, { par: { alignment: AlignmentType.LEFT, spacing: { line: 260, after: 80 } }, run: { size: 22 } })),
];

const transferSI = [
  p("The secondary stacking analysis reports that a model trained in IRES (2019-20) has essentially no association with observed 2015 ACCESS district stacking. That comparison is uninformative about mechanism, because it changes two things at once: the calendar period, and the survey instrument that measured the outcome. A model can fail to transfer because the behaviour it describes genuinely changed between the pre- and post-PMUY periods, or because the two surveys operationalize fuel stacking differently enough that the quantity being predicted is not the same quantity - and the two have opposite implications. The first is a substantive finding about the clean-cooking transition; the second is a further instance of the measurement problem this paper is about."),
  p("We separate them with a 2x2 in which each contrast varies one factor. ACCESS was fielded twice, in 2014-15 (Wave 1) and 2018 (Wave 2), with the same instrument, so training in Wave 1 and testing in Wave 2 changes the ERA with the instrument held fixed. IRES (2019-20) and ACCESS Wave 2 (2018) are one to two years apart, so training in IRES and testing in Wave 2 changes the INSTRUMENT with the era held approximately fixed. Training in IRES and testing in ACCESS Wave 1 changes {{both}}, and is the comparison the secondary analysis reports. All three are estimated on the six states ACCESS and IRES share (Bihar, Jharkhand, Madhya Pradesh, Odisha, Uttar Pradesh, and West Bengal), on rural households only, and with the district-context covariate removed from the predictor set so that no era-specific quantity enters the model; IRES is additionally restricted to its rural subsample, ACCESS being rural by design. Performance is summarized at the district level, as the correlation between the predicted and the observed district share of stacking households, because that is the level at which the predictions are used."),
  p([
    r("The three contrasts separate cleanly. Holding the instrument fixed and changing only the era gives a district-level correlation of "),
    r(trR("ERA"), { bold: true }),
    r(" (ACCESS Wave 1 to Wave 2; AUC " + nfmt(ERA.auc) + ", calibration slope " + nfmt(ERA.cal_slope) + "). Holding the era approximately fixed and changing only the instrument gives "),
    r(trR("INSTRUMENT"), { bold: true }),
    r(" (IRES to ACCESS Wave 2; AUC " + nfmt(INST.auc) + ", calibration slope " + nfmt(INST.cal_slope) + "). Changing both gives "),
    r(trR("ERA+INSTRUMENT"), { bold: true }),
    r(" (IRES to ACCESS Wave 1; AUC " + nfmt(BOTH.auc) + ", calibration slope " + nfmt(BOTH.cal_slope) + "). For reference, the same models evaluated in the data they were trained on reach district correlations of " +
      nfmt(trPred.filter((x) => x.kind === "in-sample (reference only)").map((x) => x.district_r)[0]) +
      " to " +
      nfmt(trPred.filter((x) => x.kind === "in-sample (reference only)").map((x) => x.district_r).slice(-1)[0]) +
      ", so the degradation is not a ceiling imposed by the outcome's own predictability. Both channels therefore contribute, and they are not equal: three years of era change with one instrument retains a moderate signal, whereas a change of instrument at nearly constant era leaves almost none. The failure reported in the secondary analysis is predominantly an instrument failure, with a real but smaller era component."),
  ]),
  p("Two features of the design make this a conservative reading rather than an overstated one. The instrument contrast is not era-pure: ACCESS Wave 2 (2018) and IRES (2019-20) are one to two years apart during a period of continuing rapid change, so some of what we attribute to the instrument is residual era drift, and the instrument component is if anything overstated by that. The era contrast, by contrast, is clean. Working the other way, ACCESS is a panel, so Wave 1 and Wave 2 coefficient estimates are positively correlated across waves; the two-sample z tests below ignore that covariance and therefore overstate the variance of the difference, making the era contrast's coefficient comparisons under-reject. Neither caveat changes the ordering."),
  p([
    r("Restricting the model to the covariates that do transfer does not rescue it. Taking the predictors whose coefficients are statistically indistinguishable across the instrument contrast and keep their sign (" + trStable("INSTRUMENT") + ") and refitting on the subset the pipeline retains (" + SUB_NVAR + " predictors: " + SUB_VARS + " - the instrument-stable variables whose removal does not by itself improve transfer), the IRES-trained model reaches district correlations of " +
      nfmt(trPred.filter((x) => x.kind === "transferable subset").map((x) => x.district_r)[0]) +
      " and " +
      nfmt(trPred.filter((x) => x.kind === "transferable subset").map((x) => x.district_r)[1]) +
      " against ACCESS Wave 1 and Wave 2 - no better than the full model. The failure is global rather than attributable to a few unstable terms: it is not that most of the relationship transfers and a handful of coefficients spoil it, but that little of it transfers at all. "),
    r("Predictors stable across the era contrast are " + trStable("ERA (") + "; across the instrument contrast, " + trStable("INSTRUMENT") + "; and across the combined era-and-instrument contrast, only " + trStable("ERA+INSTRUMENT") + "."),
  ]),
  p("The covariate distributions themselves move in a way that explains part, but not all, of this. Comparing ACCESS Wave 1, ACCESS Wave 2, and rural IRES in the common states, the share of households holding a below-poverty-line or Antyodaya ration card rises monotonically (" + shiftTriple("bpl_bin", "mean") + "), mean household size falls monotonically (" + shiftTriple("hhsize_c", "mean") + "), and household electrification rises (" + shiftTriple("elec_bin", "mean") + "). Two shifts, however, are harder to read as population change. The Other Backward Class share does not move monotonically - it rises across the era contrast and then falls across the instrument contrast (" + shiftTriple("caste3", "Other Backward Class") + ") - and the ration-card indicator moves further across the instrument contrast than across the era contrast (" + shiftDelta("bpl_bin", "mean", "shift_instrument") + " versus " + shiftDelta("bpl_bin", "mean", "shift_era") + "), even though the instrument contrast spans the shorter interval. A covariate that shifts smoothly with time is consistent with genuine population change; one that reverses, or that moves most where the instrument changes rather than where the years do, is more consistent with the two surveys classifying the same households differently, which is the pattern a measurement explanation predicts. Household-head education cannot be examined here at all: it is not populated in the NFHS extracts and is excluded from the prediction models (Table S1)."),
  p("Finally, the contribution of each predictor to out-of-sample transfer, measured by refitting with that predictor dropped, is reported in Table S14. It leaves one tension we record rather than resolve: religion is simultaneously the least coefficient-stable term in the model (its 'Other' category carries the single largest cross-survey difference, and the only one significant at the 5% level on the instrument contrast; Table S15) and the largest positive contributor to transfer, its removal costing " + nfmt(looTop.contrib_W1, 2) + " of district-level correlation against Wave 1 and " + nfmt(looTop.contrib_W2, 2) + " against Wave 2. Several predictors are actively harmful to transfer - dropping " + orList(looHarmfulList) + " improves it. We do not read the drop-one ordering as a causal decomposition; with correlations this low the ranking is unstable, and we report it to document that no reweighting of this covariate set produces a transportable model, not to identify which covariate to keep."),
  p("The practical consequence for this paper is contained. The transfer diagnostic concerns the secondary augmentation analysis - predicting fuel-use detail that NFHS does not collect - and not the primary correction, which calibrates a measure both surveys do collect against a contemporaneous reference and is estimated within era-matched pairs throughout. The diagnostic is nevertheless the reason the augmentation models are trained separately by era and reported as exploratory: a behavioural model moved across survey instruments is not merely less precise, it is measuring a different construct, and the ordering above says the instrument is the larger of the two problems."),
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
// ================= DESIGN-ANALYSIS SI (22_design_analysis.R) =================
// Frontier table helpers. The health tables in this paper report a REDUCTION in
// deaths per 100 births per 10-pp rise in LPG prevalence, i.e. the negative of
// the fitted coefficient times ten (see nest10/nci10 above). The frontier table
// follows the same convention so the kappa = 1 row can be read straight across
// against Table S6, and so a reader is not asked to flip signs mid-document.
const kapList = Array.from(new Set(dFront.map((x) => Number(x.kappa))))
  .filter((v) => isFinite(v)).sort((a, b) => a - b);
const fRow = (o, k) => dFront.find((x) => x.outcome === o &&
              Math.abs(Number(x.kappa) - k) < 1e-9) || {};
const fE = (o, k, d) => {
  const e = Number(fRow(o, k).estimate);
  return isFinite(e) ? nfmt(-10 * e, d === undefined ? 3 : d) : "NA";
};
const fCI = (o, k, d) => {
  const e = Number(fRow(o, k).estimate), se = Number(fRow(o, k).se);
  if (!isFinite(e) || !isFinite(se)) return "NA";
  const q = d === undefined ? 3 : d;
  // the sign flip swaps the bounds
  return nfmt(-10 * (e + 1.96 * se), q) + " to " + nfmt(-10 * (e - 1.96 * se), q);
};
const fP    = (o, k) => pfmt(fRow(o, k).p);
const fFMI  = (o, k) => { const v = Number(fRow(o, k).fmi);
                          return isFinite(v) ? nfmt(100 * v, 0) : "NA"; };
const fN    = (o) => { const r0 = fRow(o, 1); return r0.n ? nfmt(r0.n, 0) : "NA"; };
// kappa* is reported to more places than the grid it sits on, so mark the row
// nearest to it rather than testing for equality.
const kapNearStar = (() => {
  const s = DSn("kappa_star_infant");
  if (!isFinite(s) || !kapList.length) return NaN;
  return kapList.slice().sort((a, b) => Math.abs(a - s) - Math.abs(b - s))[0];
})();
const kapMark = (k) => (isFinite(kapNearStar) && Math.abs(k - kapNearStar) < 1e-9
  ? nfmt(k, 2) + " *" : nfmt(k, 2));
const tableS16 = mkTable([760, 1900, 900, 700, 1900, 900, 700], [
  ["kappa", "Infant: reduction per 10 pp [95% CI]", "p", "FMI %",
   "Neonatal: reduction per 10 pp [95% CI]", "p", "FMI %"],
].concat(kapList.map((k) => [
  kapMark(k),
  fE("infant", k) + " [" + fCI("infant", k) + "]", fP("infant", k), fFMI("infant", k),
  fE("neonatal", k) + " [" + fCI("neonatal", k) + "]", fP("neonatal", k), fFMI("neonatal", k),
])));

// Variance decomposition, transposed: one row per quantity, one column per leg,
// because two columns of fourteen numbers read far better than fourteen columns
// of two. The `vd` and `sigRatio` accessors are defined above, next to vdRow.
const VD_ROWS = [
  ["Calibration districts (overlap)", "calib_districts", 0],
  ["Calibration states", "calib_states", 0],
  ["Residual SD sigma (logit, posterior RMS)", "sigma", 3],
  ["State SD psi (logit)", "psi", 3],
  ["Calibration slope b (logit-logit)", "slope_b", 3],
  ["Reference-survey SE, mean (logit)", "yse_ref", 3],
  ["Effective reference households per district", "neff_hh_per_district", 0],
  ["Target districts corrected", "targets", 0],
  ["... of which lie in a calibration state", "targets_covered_state", 0],
  ["Posterior variance of the corrected value (logit^2)", "var_observed", 4],
  ["... calibration-side component (a, b, u_s)", "var_calibration_side", 4],
  ["... NFHS-side component (b^2 x SE_x^2)", "var_nfhs_side", 4],
  ["NFHS-side share (%)", "pct_nfhs_side", 1],
];
const tableS17 = mkTable([4300, 2200, 2200], [
  ["Quantity", "2015 leg (NFHS-4 ~ ACCESS)", "2019 leg (NFHS-5 ~ IRES)"],
].concat(VD_ROWS.map((rw) => [rw[0], vd("2015", rw[1], rw[2]), vd("2019", rw[1], rw[2])])));

const designSI = [
  h3("S4. Design of a fit-for-purpose validation survey: what would it take?"),
  p("The health-analysis demonstration of Section 3.3 leaves an obvious question unanswered. The corrected exposure carries genuine uncertainty from the calibration, that uncertainty is propagated into the mortality models by multiple imputation, and the propagated estimate is not conventionally significant. Would a better validation survey have settled the matter? This section answers that question quantitatively rather than rhetorically, by asking how much of the imputation uncertainty a hypothetical future validation survey would have to remove before the propagated association crossed p < 0.05, and then asking what survey - if any - could remove that much."),
  p("We index the answer with a single quantity. Write L for the matrix of saved posterior draws of the corrected district prevalence on the logit scale, and L-bar for its across-draw mean, which is the point surface the paper uses. For kappa in [0, 1] define L(kappa) = L-bar + kappa (L - L-bar), and refit the change-on-change model by multiple imputation on the shrunken draws exactly as in Section 2.4.5. The two endpoints are not approximations: kappa = 1 returns the saved draws unchanged and therefore reproduces the paper's uncertainty-propagated estimate exactly, and kappa = 0 collapses every draw onto the point surface and therefore reproduces the paper's point-surface estimate exactly. Both identities are checked numerically in the analysis script rather than assumed. Intermediate kappa is the counterfactual of interest: kappa = 0.5 is a validation design that halves the posterior standard deviation of every corrected district value on the logit scale, whatever combination of sample size, state coverage and instrument quality would be needed to achieve it. Working on the logit scale matters, because that is the scale on which the calibration model is linear and on which shrinking draws toward their mean is a pure variance operation; the resulting prevalences are then mapped back through the logistic function before entering the health model, so the exposure the regression sees remains a prevalence change in percentage points."),
  p("Table S16 and main-text Figure 3 give the resulting precision frontier for both outcomes over " + kapList.length + " values of kappa, on the " + fN("infant") + " districts of the primary rural analysis. The infant association reaches conventional significance at " + kappaStarTxt() + ": a validation survey would have to remove about " + nfmt(100 * (1 - (isFinite(DSn("kappa_star_infant")) ? DSn("kappa_star_infant") : NaN)), 0) + "% of the posterior standard deviation of the corrected exposure, equivalently improve its precision by a factor of " + DS("precision_gain_infant", 1) + ", for the propagated estimate to cross the threshold. For neonatal mortality the picture is different: " + neoTxt() + ", so no amount of exposure precision would make that association conventionally significant; the limit there is the mortality outcome, not the exposure. The fraction of missing information at kappa = 1 is " + nfmt(100 * DSn("fmi_infant_kappa1"), 0) + "% for infant and " + nfmt(100 * DSn("fmi_neonatal_kappa1"), 0) + "% for neonatal mortality, which is the sense in which exposure uncertainty rather than sampling variability drives the width of the propagated intervals."),
  p("Whether kappa* is reachable is a question about where the posterior variance comes from, and the answer is not the one a naive reading suggests. Each corrected district value is a + b x + u_s, where x is the NFHS district estimate entering the measurement-error model as a covariate observed with known error. Its posterior variance therefore has two parts: a calibration-side part, carried by the posterior of the intercept, slope and state effect, which a larger or better-targeted validation survey shrinks; and an NFHS-side part, b^2 times the NFHS district estimate's own sampling variance, which the validation survey cannot touch at all because it is a property of the health survey being corrected. Table S17 reports the decomposition. The NFHS-side part is " + DS("var_nfhs_side_share_2015", 0) + "% of the total on the 2015 leg and " + DS("var_nfhs_side_share_2019", 0) + "% on the 2019 leg. Setting the calibration-side part to zero - a validation survey of infinite size and perfect coverage - leaves kappa_floor = " + DS("kappa_floor", 2) + ", which is " + floorTxt() + " kappa*. It follows that " + floorVerdict() + "."),
  ...toyBlock(),
  p("That conclusion is about the transfer-function route, in which a validation survey is used to estimate a calibration that is then applied to the NFHS estimates. To confirm that the floor binds in practice and not only in the limit, we evaluated a grid of " + dGrid.length + " concrete transfer-route designs crossing the number of validation districts, the number of states covered, the depth of the reference sample per district (expressed as a multiple of the realized effective sample), and an instrument-quality factor scaling the fitted residual SD - the last standing in for matched fuel-module wording, matched season, or a panel design rather than for money. Predicted kappa is computed from the closed-form posterior covariance of the calibration under each design, averaged over posterior draws of the variance components rather than plugged in at their medians" + vcPenTxt() + ", and anchored so that the realized ACCESS and IRES designs reproduce their observed kappa. Across the grid, " + gridVerdict() + "; the most favourable design reached kappa = " + nfmt(gridBest.kappa, 2) + " (" + nfmt(gridBest.D, 0) + " districts, " + nfmt(gridBest.S, 0) + " states, " + nfmt(gridBest.m, 0) + "x the realized effective reference sample per district - about " + nfmt(Number(gridBest.m) * Number(direct1.hh_per_district_2015), 0) + " and " + nfmt(Number(gridBest.m) * Number(direct1.hh_per_district_2019), 0) + " effective households per district on the 2015 and 2019 legs - residual SD scaled by " + nfmt(gridBest.sigma_factor, 2) + "). The residual SD is the reason: on the 2019 leg the fitted residual SD is " + vd("2019", "sigma", 3) + " on the logit scale (the posterior root-mean-square, which is the summary that enters the variance budget because sigma appears there only as sigma^2; SI Table S3 reports the posterior mean of the same parameter, a slightly smaller number), roughly " + sigRatio(1) + " times the " + vd("2015", "sigma", 3) + " fitted on the 2015 leg, and because the model already carries each reference district's own sampling error as a known variance, that sigma is equation error - systematic disagreement between NFHS-5 and IRES in questionnaire wording, field timing or measurement - rather than random sampling variation, so it is not reduced by sampling more reference households. The design implication is specific: improving validation for this purpose calls for surveys with harmonized definitions, matched field periods, and ideally repeated measurement on the same households, rather than simply a larger sample."),
  p("Two levers do exist, and both lie outside the transfer-function route. The first is the health survey itself: because the irreducible term is b^2 times the NFHS sampling variance, multiplying the NFHS district sample sizes by " + DS("nfhs_multiplier_needed", 0) + " brings predicted kappa below kappa*. That multiplier is evaluated on the grid's most favourable transfer design (" + nfmt(gridBest.D, 0) + " validation districts, " + nfmt(gridBest.S, 0) + " states, " + nfmt(gridBest.m, 0) + "x the realized reference sample per district, residual SD scaled by " + nfmt(gridBest.sigma_factor, 2) + "), not at the validation surveys' realized precision, so it says what the health survey would have to add on top of a best-case validation survey rather than on top of the surveys we actually have. The second is to abandon transfer altogether and measure the exposure directly in the health survey's own districts with the better instrument - a fuel module carried inside NFHS, or a matched-frame companion survey - which removes the calibration step and with it both variance components. Reaching kappa* that way requires about " + DS("direct_m_star", 0) + " times the realized per-district effective sample (from " + nfmt(direct1.hh_per_district_2015, 0) + " and " + nfmt(direct1.hh_per_district_2019, 0) + " households per district on the two legs to " + nfmt(directStar.hh_per_district_2015, 0) + " and " + nfmt(directStar.hh_per_district_2019, 0) + "), or roughly " + MILL("direct_hh_total", 1) + " million households across the " + DS("n_study_districts", 0) + " study districts - about " + DS("direct_hh_vs_nfhs5_rounds", 1) + " times the household sample of a full NFHS round. That is a statement about the size of the measurement problem, not a proposal."),
  p("Three limits on how this exercise should be read. It holds the mortality data, the covariate set and the model specification fixed and varies only exposure precision, so it bounds what exposure measurement alone can buy and not what a differently designed health study could achieve. The kappa device shrinks the posterior of the fitted calibration and so describes a survey that estimates the same calibration relationship more precisely; it does not describe a survey that would reveal the relationship to be different. And the grid's mapping from design features to kappa is a model-based prediction validated at two points - the realized ACCESS and IRES designs - so it should be read as an order-of-magnitude guide to which designs are in contention, not as a sample-size calculator. What survives all three caveats is the qualitative finding, which is robust to the anchoring and to the grid's resolution: for this outcome, in this design, the binding constraint is the precision of the health survey's own district estimates rather than the quality of the validation survey used to correct them."),
];

// ================= ESTIMAND SI (23_ppd_sensitivity.R) ========================
const PS_ORDER = [
  ["epred_point", "Calibrated expectation, point surface (= corrected, Bayesian)"],
  ["emean_point", "Calibrated expectation, averaged on the probability scale"],
  ["epred_MI",    "Calibrated expectation, uncertainty propagated (primary)"],
  ["ppd_MI",      "District true prevalence, posterior predictive, uncertainty propagated"],
  ["ppd_point",   "District true prevalence, posterior predictive, point surface"],
];
const psRows = () => {
  const out = [];
  ["neonatal", "infant"].forEach((oc) => {
    PS_ORDER.forEach((v) => {
      const rr = psRow(oc, v[0]);
      if (!rr.estimate) return;
      const e = Number(rr.estimate), se = Number(rr.se);
      out.push([oc === "infant" ? "Infant" : "Neonatal", v[1],
        nfmt(-10 * e, 3) + " [" + nfmt(-10 * (e + 1.96 * se), 3) + " to "
          + nfmt(-10 * (e - 1.96 * se), 3) + "]",
        pfmt(rr.p),
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
  p("The corrected district exposure carried into the health models is the calibrated expectation of the district's prevalence - the posterior of the linear predictor - and not a draw of the district's true prevalence, which would additionally include the calibration model's residual variance. Section 2.4.3 sets out why: the residual is Berkson rather than classical error with respect to the value actually used as the regressor, so in a linear outcome model it passes into the outcome residual without biasing the coefficient or deflating a cluster-robust standard error, whereas adding it back would convert Berkson error into classical error and attenuate the association toward the null. Because the calibration model is estimated without reference to mortality - the two models are uncongenial in Meng's sense - that attenuation is not guaranteed to be conservative in general, so it is checked here rather than assumed. As with any Berkson argument this holds under the assumed calibration model and is exact only under its conditions, and a reader may reasonably prefer the other estimand, so this section reports the alternative as a clearly labelled sensitivity - a conservative lower-bound analysis - rather than only asserting that it is the wrong target. Nothing is refitted: the sensitivity is computed from the saved posterior draws and the saved model fits."),
  p("The Berkson argument is exact on the logit scale, but the exposure enters the health regression on the probability scale, as a change in percentage points, and the logistic function is nonlinear - so the mean of the transformed draws is not the transform of the mean. The size of that Jensen gap has to be checked rather than assumed, particularly on the 2019 leg where the fitted residual SD is " + nfmt(pvRow("2019").sigma, 3) + " on the logit scale. Averaging the posterior on the probability scale instead moves the mean corrected prevalence by " + PQ("jensen_gap_change_mean_pp", 3) + " percentage points on average across districts in the 2015-2019 change (maximum " + PQ("jensen_gap_change_maxabs_pp", 2) + " percentage points), because a shift common to both endpoints cancels in a change-on-change design. Refitting the mortality models on the probability-scale average shifts the infant coefficient by " + PQ("jensen_rel_shift_infant_pct", 2) + "% and the neonatal coefficient by " + PQ("jensen_rel_shift_neonatal_pct", 2) + "%. " + jensenVerdict() + " Were the shift material, the remedy would be to save the probability-scale posterior mean from the calibration step in place of the logistic transform of the logit-scale mean; that would change the calibration output, not the estimand."),
  p("Table S18 reports the health models under both estimands. Imputing from the posterior predictive - adding the residual back, so that each imputation is a draw of the district's putative true prevalence rather than of its calibrated expectation - widens the exposure surface itself: the across-draw standard deviation of the corrected district prevalence rises by a factor of " + PQ("sd_inflation_2015", 2) + " on the 2015 leg and " + PQ("sd_inflation_2019", 2) + " on the 2019 leg. Against that wider exposure the infant coefficient " + ppdDirTxt("infant") + ", and " + ppdSeTxt("infant") + ". The point to take from the pair is that " + ppdZTxt("infant") + ": a regressor carrying more noise attenuates the coefficient attached to it and, because that noise enlarges the regressor's variance, sharpens rather than loosens the standard error, and the two effects substantially offset in the test. Comparing the two propagated versions on the conventional threshold, " + ppdVerdict("infant") + " for infant mortality and " + ppdVerdict("neonatal") + " for neonatal mortality. " + ppdConsTxt() + ". We report the calibrated-expectation version as primary on the reasoning of Section 2.4.3, and this table so that a reader who holds the other estimand can see exactly what it implies."),
];

// Displaced main-text material: the full child-mortality analysis (was Section
// 3.3) and the secondary analyses. The main text keeps only
// the fixed-surface-versus-propagated contrast; everything else lives here.
const healthSI = [
  h3("S6. The child-mortality demonstration in full"),
  p("Section 3.3 reports this demonstration in the form that bears on the paper's argument: a fixed corrected surface against the same surface with its uncertainty propagated. This section reports the full set of exposure specifications, sensitivity analyses and restrictions behind it, so that the compact main-text account can be checked against the complete result (Table S6; SI Figure S21). Across the " + N_RURAL + " rural districts with mortality estimates, corrected exposure and complete covariates, the raw NFHS fuel item gave little evidence that rising primary-LPG prevalence was associated with declining child mortality: adjusted associations were weak and their confidence intervals included no effect (neonatal " + est10(R_N_RAW) + " deaths per 100 births per 10-percentage-point rise, 95% CI " + ci10(R_N_RAW) + "; infant " + est10(R_I_RAW) + ", 95% CI " + ci10(R_I_RAW) + "). Substituting the corrected exposure moved both estimates toward a protective association: regression calibration roughly doubled the coefficients (neonatal " + est10(R_N_RC) + ", 95% CI " + ci10(R_N_RC) + "; infant " + est10(R_I_RC) + ", 95% CI " + ci10(R_I_RC) + "), and the Bayesian correction increased them further, to about " + foldHalf(R_I_BAY, R_I_RAW) + " times the raw infant estimate (neonatal " + est10(R_N_BAY) + ", 95% CI " + ci10(R_N_BAY) + "; infant " + est10(R_I_BAY) + ", 95% CI " + ci10(R_I_BAY) + ", p = " + pfmt((R_I_BAY || {}).p) + "). The monotone progression is what classical measurement-error theory predicts when error is removed from an exposure, and we report it as a consistency check on the correction. It is not, on its own, evidence that the correction has recovered a real association, for the two reasons given below and for the reason given in Section 3.3."),
  p("Model specification and population. Models were weighted by the harmonic mean of eligible births across the two rounds - a births-based analytic weight, not a population denominator - and adjusted for concurrent district changes in socioeconomic composition and in ambient conditions (PM2.5, temperature, relative humidity, and drought), with region fixed effects and state-clustered standard errors. Because the correction is estimated on rural households, district mortality is restricted to rural births so that exposure and outcome refer to the same population; the all-household version is closely similar (Table S7; N = " + N_ALL + "; infant Bayesian-corrected " + est10(A_I_BAY) + ", 95% CI " + ci10(A_I_BAY) + "; neonatal " + est10(A_N_BAY) + "). Unweighted fits appear in the lower block of the same tables and are of similar magnitude (rural infant, Bayesian-corrected: " + est10(R_I_BAY_UNW) + ", 95% CI " + ci10(R_I_BAY_UNW) + "). A short-window, non-overlapping-cohort sensitivity gives a similar corrected infant estimate (" + est10(NO_I_BAY) + "; Table S6)."),
  p("The calibration instrument. The era-matched correction applies a different reference instrument at each endpoint - ACCESS calibrates the 2015 estimate, IRES the 2019 estimate - so part of the corrected change could reflect the switch of instrument rather than true temporal change. Applying a single instrument, the IRES rural calibration, to both NFHS rounds leaves the infant association protective but smaller and no longer statistically distinguishable from no effect (" + est10(R_I_IRES) + ", 95% CI " + ci10(R_I_IRES) + "), which means that a substantial part of the apparently stronger era-matched infant result comes from changing calibration instruments between endpoints; the neonatal estimate is essentially unchanged (" + est10(R_N_IRES) + " against " + est10(R_N_BAY) + "). Calibration uncertainty, the second qualification, is the subject of Section 3.3: propagating it by multiple imputation returns both estimates partway toward the null while leaving them protective (infant " + est10(R_I_MI) + ", neonatal " + est10(R_N_MI) + ")."),
  p("Calibration support. Two restrictions test whether the pattern depends on districts the calibrations never saw. Restricting to districts inside both calibration surveys' footprints, and separately to districts inside an ACCESS state, retained the negative direction but produced less precise estimates (infant, Bayesian correction with its uncertainty propagated: " + est10(supFull) + ", 95% CI " + ci10(supFull) + ", N = " + ((supFull || {}).n || "NA") + "; and " + est10(supState) + ", 95% CI " + ci10(supState) + ", N = " + ((supState || {}).n || "NA") + "). Estimates were more sensitive in the all-population analysis, where the same restrictions moved the infant coefficient to " + est10(supFullAll) + " (95% CI " + ci10(supFullAll) + ") and " + est10(supStateAll) + " (95% CI " + ci10(supStateAll) + "). Because the restricted samples are less than half the size of the full one, these comparisons cannot establish that the main estimate is robust to limited calibration support; we report them as a sensitivity whose imprecision is itself the finding. One check does not depend on the correction at all, and it corroborates the expected directions unevenly across the two eras: across districts, the association between the energy surveys' directly measured solid-fuel burning and child mortality is " + heDir(HE_A, "Any solid-fuel burning") + " in the ACCESS/NFHS-4 era and " + heDir(HE_I, "Any solid-fuel burning") + " in the IRES/NFHS-5 era, while the association with their primary-LPG prevalence is " + heDir(HE_A, "Primary LPG") + " and " + heDir(HE_I, "Primary LPG") + " respectively, the ACCESS-era figures resting on " + cmpN(C4R) + " districts in six northern states (SI Figure S11). All of these remain ecological associations across " + N_RURAL + " rural districts, and none is validated against an independent health-based measure of true exposure."),
];

const secondarySI = [
  h3("S7. Secondary analyses: predicted fuel use and adult cardiometabolic outcomes"),
  p("Two analyses are reported as secondary rather than as part of the paper's argument. First, we trained models in the energy surveys to augment NFHS with fuel-use detail it does not collect - fuel stacking, the four-category composition of fuel use, and LPG consumption (SI Methods; SI Figure S6; SI Table S8). These predictions have only moderate accuracy: in leave-district-out cross-validation the household stacking model reached an area under the ROC curve of " + ldo("ACCESS W1", "auc") + " in ACCESS and " + ldo("IRES rural", "auc") + " in IRES, with only modest agreement between predicted and observed district-level stacking (Pearson r = " + ldo("ACCESS W1", "district_r") + " and " + ldo("IRES rural", "district_r") + " respectively), driven chiefly by household wealth and size with little added by caste, religion, or the district-LPG context (covariate contributions in SI Table S9). They surface one result that reinforces the measurement argument: the relationship between household characteristics and stacking did not transfer - an IRES-era (2019-20) stacking model, even with era-specific context removed, showed essentially no association with observed 2015 ACCESS district stacking (r = " + CROSS_ERA_R + "). Because that test changes the calendar period and the measuring instrument at the same time, we decomposed it (SI Methods S3; SI Tables S12-S15). Holding the instrument fixed and changing only the era - training in ACCESS Wave 1 (2014-15) and testing in ACCESS Wave 2 (2018) - retains a district-level correlation of " + trR("ERA") + "; holding the era approximately fixed and changing only the instrument - training in IRES (2019-20) and testing in ACCESS Wave 2 - leaves " + trR("INSTRUMENT") + "; changing both leaves " + trR("ERA+INSTRUMENT") + ". Refitting on only the coefficients that are stable across surveys does not recover it, so the failure is global rather than caused by a few unstable terms. Between the pre- and post-PMUY periods, then, not only did clean-fuel levels rise and the determinants of a household's fuel use shift, but the larger share of the failure lies with the instrument: a single fixed survey item is measuring a moving target in more than one sense. Substituting these predicted composition metrics for corrected primary-LPG prevalence in the mortality model did not yield coherent associations and is reported only as exploratory (SI Table S8, SI Figure S10); corrected primary-LPG prevalence remains our exposure of record."),
  p("Second, applying the same change-on-change design to two adult cardiometabolic outcomes (measured hypertension and diabetes) yielded null associations that did not follow the mortality pattern and did not strengthen under correction; these cross-sectional risk factors integrate decades of cumulative exposure and are underpowered for a single-decade district design, so we report them only as a bounding exercise (SI Table S5; SI Figures S7-S9). The contemporaneous child-mortality endpoints carry the substantive signal."),
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
if (!HAVE_NSSO)
  console.warn("[build] NSSO-78 outputs not found (nsso78_comparison_table.csv) "
    + "-- SI Section S8, Table S19 and SI Figure S22 will be OMITTED. Run "
    + "25_prep_nsso78.R and 26_compare_nsso78.R, then rebuild.");

const nssoMeta = readCsv("nsso78_linkage_diagnostics.csv", { quiet: true });
const nmeta = (m) => { const row = nssoMeta.find((x) => x.metric === m);
                       return row ? row.value : "NA"; };
const nssoWide = readCsv("nsso78_nfhs_ires_district_lpg.csv", { quiet: true });
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
  ["NFHS-5 vs IRES (all households; same districts)", NC.ctxall],
  ["NFHS-5 vs IRES (rural; same districts)",      NC.ctxrur],
].map(([lbl, d]) => [lbl, d.n_districts || "NA", nfmt(d.pearson), nfmt(d.spearman),
                     nfmt(d.pearson_ref_samplewt), nfmt(d.ccc), ppd(d.mean_diff)]));

const nssoMethodsSI = [
  h3b("S8. External validation against the NSSO 78th-round Multiple Indicator Survey (2020-21)"),
  pb("After the calibration analyses were specified, we added a third, fully "
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
    + "stratification), for all households and for rural households. The 685 "
    + "NSS frame districts were deterministically mapped onto the 640 "
    + "census-2011 districts, with the 47 districts created after 2011 folded "
    + "into their dominant 2011 parents - the same collapse applied to IRES's "
    + "split sampling units - and every household mapped. The weight rule and "
    + "fuel coding were verified by reproducing the published MIS national "
    + "clean-fuel shares (49.8% rural, 92.0% urban, 63.1% overall) to within "
    + "0.2 percentage points; the reproduction is asserted programmatically on "
    + "every pipeline run."),
  pb("Table S19 reports the agreement battery of Table S2 for the new pairs, "
    + "and SI Figure S22 the corresponding scatterplots. Against NFHS-5 on the "
    + (NC.n5all.n_districts || "NA") + "-district national overlap, the MIS "
    + "correlates r = " + nfmt(NC.n5all.pearson) + " (all households; rural "
    + "r = " + nfmt(NC.n5rur.pearson) + ") but sits " + ppd(NC.n5all.mean_diff)
    + " pp higher on average (" + ppd(NC.n5rur.mean_diff) + " pp rural). "
    + "Against IRES on its " + (NC.irall.n_districts || "NA") + "-district "
    + "footprint, the MIS shows r = " + nfmt(NC.irall.pearson) + " with a mean "
    + "difference of " + ppd(NC.irall.mean_diff) + " pp (all households) and "
    + ppd(NC.irrur.mean_diff) + " pp among rural households. On those same "
    + "districts NFHS-5 sits " + ppd(NC.ctxall.mean_diff) + " pp (all) and "
    + ppd(NC.ctxrur.mean_diff) + " pp (rural) relative to IRES."),
  pb("Two features of this triangulation bear on the calibration. First, the "
    + "MIS - an official survey on an independent frame with an independent "
    + "field force - lands on the same side of NFHS-5 as both reference energy "
    + "surveys: NFHS-5 is the low outlier among all four data systems. "
    + "Fieldwork timing cannot produce this pattern, since the MIS window is "
    + "centred roughly a year {{later}} than NFHS-5's, and PMUY-era growth "
    + "would raise, not depress, the later survey. Second, the MIS does not "
    + "reach IRES's levels either, particularly among rural households, where "
    + "it closes only about " + NSSO_GAP_CLOSED + "% of the rural NFHS-IRES "
    + "gap. Read jointly, the MIS confirms the {{direction}} of the "
    + "IRES-anchored correction while suggesting its rural {{magnitude}} is "
    + "better read as an upper bound: part of the residual NFHS-IRES "
    + "difference may sit on the IRES side (a 21-state, 152-district design "
    + "that is state- rather than district-representative) or reflect genuine "
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
    + "NFHS-5 vs IRES rows repeat the Table S2 comparison on the same "
    + "districts for reference."),
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
    + "the least-squares fit; points are sized by the household count of the "
    + "horizontal-axis survey's district sample (NFHS-5 panels: MIS counts). "
    + "The MIS sits above NFHS-5 nearly everywhere but below IRES in most of "
    + "the IRES overlap, bracketing NFHS-5 from above (SI Methods S8)."),
];

const siChildren = [
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120 },
    children: [r("Supplementary Materials", { bold: true, size: 30 })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240 },
    children: [r("Companion to: Calibration removes bias but does not create information: evidence from India's clean-cooking transition", { italics: true, size: 22 })] }),

  h1("Supplementary Methods"),
  h3("S1. Survey sampling weights and design-based standard errors"),
  ...weightsSI,
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 80, after: 40 },
    children: [new ImageRun({ type: "png",
      data: fs.readFileSync(FIG + "fig_eq_selogit.png"),
      transformation: { width: 220, height: 74 } })] }),
  p("Equation S1. Delta-method transformation of a design-based standard error from the proportion scale to the logit scale, where p is the district proportion and SE_p its design-based standard error.", { par: { alignment: AlignmentType.CENTER }, run: { size: 20, italics: true } }),

  h3("S2. Fuel-use prediction models (secondary analysis)"),
  p("The rationale for imputing fuel-use detail into NFHS, rather than analyzing the energy surveys alone, is that NFHS is the only data system with all three properties that health analysis requires: national coverage at district resolution, repeated rounds that support longitudinal designs, and individual-level health and anthropometric outcomes measured in the same households as cooking fuel. The energy surveys measure the fuel portfolio credibly but cover limited geographies at single time points and collect no health outcomes; transferring their fuel-use detail onto NFHS households combines the strengths of both systems. To recover fuel-use detail absent from NFHS, we model the three energy-survey outcomes as functions of the harmonized covariate set (caste, religion, household size, ration card, electricity, within-state wealth quintile; education is omitted because the NFHS extracts carry no populated attainment item, Table S1) plus a district-context covariate, the district share of households using LPG as the main fuel: multilevel logistic regression (state and district random effects) for stacking among primary-LPG households; multinomial logistic regression for the three-category fuel-use outcome; and a two-part hurdle model for annual LPG consumption, combining logistic regression for any LPG use with a log-linear model for consumption among users, retransformed with Duan smearing [8]. Models are trained separately in ACCESS Wave 1 (matching the NFHS-4 era and rural setting) and IRES (matching the NFHS-5 era, urban and rural). When applying the models to NFHS households, the district-context covariate is filled with the corrected district prevalence from the measurement-error model, which places it on the reference-survey scale on which the models were trained, matched to the corresponding era; households in states outside a training survey's coverage receive population-average predictions from a no-state-effect variant and are flagged as extrapolations."),
  p("Predictive validity is assessed two ways: leave-district-out cross-validation within each training survey, and a stricter cross-survey, cross-era test in which the IRES-trained model (excluding the era-specific district-context covariate) is applied to ACCESS households and evaluated against observed ACCESS district-level stacking. Household-level performance is summarized by the area under the receiver-operating-characteristic curve (AUC). District-level performance is summarized by the correlation between observed and predicted district stacking prevalence. Both era-matched model sets are used in production - ACCESS-era models generate the NFHS-4 predictions and IRES-era models the NFHS-5 predictions; the primacy assigned to IRES in Section 2.4.3 concerns only the corrected prevalence surfaces and reflects geographic support (" + I_STATES + " states versus six), not a preference between the stacking models. In leave-district-out cross-validation the stacking models achieved AUC " + ldo("ACCESS W1", "auc") + " (ACCESS) and " + ldo("IRES rural", "auc") + " (rural IRES), with observed-vs-predicted district correlations of " + ldo("ACCESS W1", "district_r") + " (ACCESS) and " + ldo("IRES rural", "district_r") + " (IRES); within ACCESS, annual LPG consumption from cylinder purchases averaged " + KG_MEAN + " kg/year among LPG-using households (median " + KG_MEDIAN + " kg/year; n = " + KG_N + " LPG-using households), in the same range as the 93 kg/year reported by prior ACCESS analyses [8]. The cross-era test is the decisive negative result: an IRES-trained (2019-20) model, even excluding era-specific context, showed essentially no association with observed 2015 ACCESS district stacking (r = " + CROSS_ERA_R + "), motivating the era-matched design and cautioning against transporting stacking models across policy regimes. As an external check, the design-weighted IRES rural three-category composition (restricted, like the external estimates, to LPG-or-solid households) places the LPG-with-no-solid-fuel-reported share at " + hb("LPG, no solid fuel reported", "share") + "%, close to the " + hb("LPG, no solid fuel reported", "habib_2023", 0) + "% that an independent pan-India survey reports for rural India in 2019 under its own 'exclusive LPG' definition [39]; our stacking (" + hb("LPG and solid fuel reported", "share") + "%) and solid-fuel-with-no-LPG (" + hb("Solid fuel reported, no LPG", "share") + "%) shares differ from their " + hb("LPG and solid fuel reported", "habib_2023", 0) + "%/" + hb("Solid fuel reported, no LPG", "habib_2023", 0) + "% split because our classification counts any reported solid-fuel use as stacking whereas theirs reflects regular use (a small further share of households use a non-solid, non-LPG fuel and fall outside this three-way split), so the HAP-relevant LPG-no-solid margin is definition-robust while our stacking share is, by construction, an upper bound. For NFHS-5, " + supDistPct + "% of districts, holding " + supHhPct + "% of sampled households, lie in IRES-covered states; predictions elsewhere are population-average extrapolations and are flagged in the released estimates. The predictive contribution of each covariate, quantified by drop-one leave-district-out AUC and by standardized effect size, is reported in Table S9: the within-state wealth quintile and household size are the strongest predictors in both training surveys, the district-context covariate adds little, and no single covariate is strong - consistent with the models' modest discrimination and with fuel stacking being poorly captured by standard survey covariates."),

  h3("S3. Does the stacking model fail across eras or across instruments? (transfer diagnostic)"),
  ...transferSI,

  ...designSI,

  ...(HAVE_PPD ? ppdSI : []),

  ...healthSI,

  ...secondarySI,

  ...(HAVE_NSSO ? nssoMethodsSI : []),

  h1("Supplementary Tables"),
  caption("Table S1. Measures used from each survey (NFHS-4, NFHS-5, ACCESS Wave 1, and IRES): the primary-LPG exposure indicator common to all four surveys, the fuel-use outcomes constructed only from the energy surveys (fuel stacking, three-category fuel use, and annual LPG consumption), and the harmonized socioeconomic covariates, together with the source item in each survey. A dash indicates a measure a survey does not collect."),
  table1, p("", {}),
  caption("Table S2. District-level agreement between NFHS and the reference energy surveys for the prevalence of primary LPG use as the main cooking fuel, for the two temporally overlapping survey pairs (NFHS-4 vs ACCESS Wave 1; NFHS-5 vs IRES) under several specifications (all households, rural only, and design-weighted direct estimates on both sides). Columns are: N, the number of districts common to both surveys; Pearson r and Spearman, the correlations between the two surveys' district estimates; Sample-wt r, the Pearson correlation with each district weighted by its number of reference-survey households, so that better-sampled districts count more (a sample-size weighting, not a population weighting); CCC, Lin's concordance correlation coefficient, which unlike a correlation also penalizes departures from the line of equality; and Mean diff, the average of (NFHS minus reference) on the 0-1 proportion scale, where a negative value means NFHS is lower. Restricting NFHS to rural clusters and using design-weighted estimates brings the NFHS-4/ACCESS pair into close agreement, whereas the NFHS-5/IRES pair retains a large negative mean difference - about " + GAP_DW + " percentage points on the design-weighted rural-versus-rural comparison and " + GAP_ML + " points on the multilevel estimates."),
  table2, p("", {}),
  caption("Table S3. Bayesian measurement-error calibration of the NFHS district primary-LPG estimates against the reference energy surveys, on the logit scale. For each calibration, estimated on rural households (NFHS-4 rural district estimates against ACCESS Wave 1, which sampled only rural households; NFHS-5 rural district estimates against the rural design-weighted IRES estimate), entries are the posterior mean and 95% credible interval (CrI) of the slope relating the reference logit-prevalence to the NFHS logit-estimate, the intercept, the between-state and residual standard deviations (SD), the number of calibration districts, and, for NFHS-5, the leave-one-state-out (LOSO) cross-validation result: the number of held-out states in which the calibration lowered the root-mean-square error (RMSE) against the reference, and the median held-out RMSE before and after correction. A slope near 1 with intercept near 0 (NFHS-4/ACCESS) indicates near-agreement; a slope well below 1 with a large positive intercept (NFHS-5/IRES) indicates that NFHS compresses between-district variation and understates prevalence."),
  table3, p("", {}),
  caption("Table S4. Benchmark (falsification) analysis: district-level agreement between NFHS and the reference surveys, for rural districts, on characteristics measured comparably in both surveys, shown alongside primary LPG. Variables are Scheduled Caste (SC), Scheduled Tribe (ST), their union (SC/ST), the Hindu and Muslim population shares, household electricity, possession of a below-poverty-line (BPL) ration card, and primary LPG. Columns are: N, the number of overlapping districts; Pearson r; CCC, Lin's concordance correlation coefficient; and Mean diff, the average of (NFHS minus reference) on the proportion scale. Close agreement on the demographic benchmarks together with the large negative mean difference for primary LPG indicates that the two surveys sample comparably composed populations, so the LPG discrepancy reflects the fuel measurement itself rather than differences in who was surveyed."),
  tableS4,
  p("* After harmonizing the IRES ration-card response coding (Antyodaya + BPL vs none; don't-know set missing). The residual difference reflects instrument design: NFHS asks specifically about possession of a BPL card, whereas IRES elicits the card type held.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S5. Supplementary analysis: district change-on-change association between primary-LPG prevalence and adult cardiometabolic outcomes. For each outcome - measured hypertension, diabetes as a measured-or-self-reported composite, and diabetes by self-report only - the district change in prevalence between NFHS-4 (2015-16) and NFHS-5 (2019-21) is regressed on the district change in primary-LPG prevalence measured three ways: raw NFHS, regression-calibrated, and Bayesian-corrected. Entries are the estimate and 95% confidence interval of the change in adult prevalence (percentage points) per 10-percentage-point increase in primary-LPG prevalence, with the p-value; models adjust for the same socioeconomic and ambient covariates and region fixed effects as Table S6, with standard errors clustered on state (rural districts; N = " + N_ADULT + ")."),
  mkTable([2200, 2600, 2400, 1000], [
    ["Outcome", "Exposure measurement", "Est. per 10-pp LPG [95% CI]", "p"],
  ].concat(adultRows())),
  p("Rural districts (N = " + N_ADULT + "); adult outcomes and corrected exposure both restricted to rural households. Unweighted district-mean rural prevalences of hypertension (measured, JNC7 definition), diabetes (composite: measured high glucose or self-report), and diabetes (self-report only) were " + adultPrev("hypertension_2015") + "%/" + adultPrev("hypertension_2019") + "%, " + adultPrev("diabetes_2015") + "%/" + adultPrev("diabetes_2019") + "%, and " + adultPrev("diabetes_sr_2015") + "%/" + adultPrev("diabetes_sr_2019") + "% at NFHS-4/NFHS-5 respectively. Diabetes is shown both as the composite indicator and, as a sensitivity, as the pure self-reported item; associations are null throughout and, unlike child mortality, the correction does not reveal a coherent protective association for adult cardiometabolic outcomes.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S6. Main analysis: district change-on-change association between primary-LPG prevalence and child mortality (2019 vs 2015), by exposure measurement, for rural districts (corrected exposure and mortality both restricted to rural births). These are the estimates plotted in main-text Figure 3. Six exposure specifications are shown per outcome: raw NFHS; regression-calibrated; the era-matched Bayesian correction (the primary; ACCESS calibrates 2015, IRES calibrates 2019); that correction with its uncertainty propagated by multiple imputation (MI); a Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both rounds, so the corrected change does not switch instruments across rounds); and that instrument-consistent version with MI. Models are weighted by the harmonic mean of eligible births across the two rounds; the corresponding unweighted fits are shown in the lower block of the same table. Models are adjusted for district changes in poverty, maternal education, electricity, Muslim share, improved sanitation, improved water, ambient PM2.5, temperature, relative humidity, and drought, with region fixed effects; state-clustered SEs; N = " + N_RURAL + " districts. Units: deaths per 100 births per 10-percentage-point increase in LPG prevalence. As a further sensitivity, restricting each round's births to a short, non-overlapping 36-month recent-cohort window (so the two mortality cohorts do not overlap in calendar time) gives a similar corrected infant estimate (Bayesian-corrected " + est10(NO_I_BAY) + ", 95% CI " + ci10(NO_I_BAY) + "; instrument-consistent " + est10(NO_I_IRES) + "). The corresponding all-household analysis is in Table S7."),
  table4,
  p("", {}),
  caption("Table S7. Sensitivity analysis: all-household version of the main child-mortality analysis (Table S6). District change-on-change association between primary-LPG prevalence and child mortality (2019 vs 2015), by exposure measurement, with district mortality estimated from all births (rather than rural births only) while the corrected exposure is unchanged. Adjusted for the same socioeconomic and ambient covariates and region fixed effects as Table S6, with state-clustered SEs; N = " + N_ALL + " districts. Units: deaths per 100 births per 10-percentage-point increase in LPG prevalence. Estimates are close to the rural main analysis (Table S6), and the same strengthening of the point association under correction is present; the infant Bayesian-corrected estimate " + allBayesPTxt() + " in this all-birth sample, and the instrument-consistent (single-instrument) version is again protective but not statistically significant. The lower block of the table gives the corresponding unweighted fits."),
  table4all,
  p("", {}),

  caption("Table S8. Exploratory analysis: district change-on-change association between predicted fuel-use composition metrics and child mortality (2019 vs 2015), rural districts (N = " + N_NUANCE + "). For each mortality outcome, the corrected primary-LPG prevalence (from Table S6, shown for reference) is compared with the model-predicted district metrics from the augmentation step (Section 2.4.4): the LPG-no-solid-reported share, the LPG-and-solid-fuel (stacking) share, the any-solid-fuel share, and predicted annual LPG consumption. Entries are the point estimate and 95% CI per 10-percentage-point rise in each share, or per 10 kg/year for consumption, with the same covariate adjustment and state-clustered SEs as Table S6. These metrics are model-predicted rather than measured and are strongly collinear (each is generated from the same corrected district-LPG context), so their individual coefficients are not separately interpretable; the table is hypothesis-generating only. Note that the predicted metrics do not behave coherently - the LPG-no-solid-reported share and predicted consumption, which should be protective if they carried real signal, carry a counterintuitive positive (harmful) sign, while the any-solid-fuel share, which should be harmful, carries a negative sign, and the only interval excluding the null is the LPG-and-solid-fuel (stacking) share. We read that pattern as diagnostic of the limits of the prediction models rather than as evidence that fuel stacking is protective; together with the metrics' mutual collinearity it is why we draw no health conclusions from this table and retain corrected primary-LPG prevalence as the exposure of record."),
  mkTable([1800, 3000, 2400, 1000], [
    ["Outcome", "Predicted exposure metric", "Est. per 10 units [95% CI]", "p"],
  ].concat(nuancedRows("point"))),
  p("Units: deaths per 100 births per 10-percentage-point rise in each share (or per 10 kg/year for consumption). All metrics are estimated with a common specification (unweighted; the model-consistent proxy applies the IRES composition models to both rounds), so the primary-LPG reference row here differs slightly from the birth-weighted main estimate in Table S6. With the four-category composition, the any-solid-fuel share (solid-no-LPG + stacking) is NOT the exact complement of the LPG-no-solid-reported share - a fourth 'other non-solid, non-LPG' category means the shares need not sum to one - so its coefficient is not simply the negation of the LPG-no-solid-reported row.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S9. Predictive contribution of each covariate to the fuel-stacking prediction model (Section 2.4.4), computed separately in the two training surveys (ACCESS Wave 1; IRES rural). 'AUC drop' is the fall in leave-district-out cross-validated AUC when that covariate is removed from the household-covariate model - its unique out-of-sample contribution (larger = more important; a negative value means removal marginally improved held-out AUC). '|z|' is the absolute z-statistic of the covariate in the fitted logistic model with numeric predictors standardized (factors summarized by their largest-magnitude level), a measure of effect strength. The two columns are complementary: because the household covariates are correlated (wealth, household size, and electricity overlap), the drop-one AUC credits only each covariate's unique signal and so understates joint importance, whereas |z| reflects each covariate's fitted effect. The district LPG share is a district-level aggregate that cannot be honestly held out district-by-district, so its 'AUC drop' is the gain from adding it to the household-only model rather than a leave-one-out value. In both surveys the within-state wealth quintile is the strongest single predictor and household size the second; no covariate is individually strong, consistent with the models' modest overall discrimination (leave-district-out AUC of the household-only model used for this table, " + viAuc("ACCESS W1") + " in ACCESS and " + viAuc("IRES rural") + " in IRES; the production stacking model, which differs only in carrying the district-context covariate, is at " + ldo("ACCESS W1", "auc") + " and " + ldo("IRES rural", "auc") + ") and with the argument that fuel stacking is poorly predicted by the standard covariates general health surveys carry."),
  mkTable([2500, 1450, 1150, 1450, 1150], [
    ["Covariate", "AUC drop (ACCESS)", "|z| (ACCESS)", "AUC drop (IRES)", "|z| (IRES)"],
  ].concat(tableS9rows)),
  p("* For the district LPG share the entry is the in-sample AUC gain from adding the district aggregate to the household-only model, not a leave-district-out drop; it is therefore optimistic and is not comparable with the rows above it. Education is absent from this table because the NFHS extract carries no populated attainment item (Table S1), so it is not a candidate predictor.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S10. Mean national district primary-LPG prevalence (unweighted average across districts) for each estimator and correction, by round. Regression calibration was fit two ways - on the multilevel and on the design-weighted raw NFHS estimate - to test sensitivity to the raw-estimator basis (Section 2.4.3; SI Figure S20)."),
  mkTable([4600, 1900, 1900], [
    ["Estimator / correction", "2015 (%)", "2019 (%)"],
  ].concat(tableS10rows)),
  p("Before PMUY (2015) the corrections are mild and all estimators lie within a few points; after PMUY (2019) the three corrections raise prevalence to about " + nfmt(BAY19, 0) + "-" + nfmt(emx(2019, "Reg.-calibrated (design-wt input)"), 0) + "%, from a raw " + nfmt(RAW19, 0) + "-" + nfmt(emx(2019, "Raw design-weighted"), 0) + "%. Across districts the two regression-calibration variants correlate r = " + emCor(2015, "rc_ml", "rc_wt") + " (2015) and " + emCor(2019, "rc_ml", "rc_wt") + " (2019); regression calibration and the Bayesian correction correlate r = " + emCor(2015, "rc_ml", "bayes") + " (2015) and " + emCor(2019, "rc_ml", "bayes") + " (2019), the lower post-PMUY value reflecting the Bayesian model's additional hierarchical shrinkage and propagated measurement error.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S11. Item missingness - the percent of households with a missing value - for the exposure and each harmonized covariate, in each survey's analytic household frame (NFHS-4 N = " + missN("NFHS-4") + "; NFHS-5 N = " + missN("NFHS-5") + "; ACCESS N = " + missN(MISS_ACCESS) + ", the pooled two-wave panel frame; IRES N = " + missN("IRES") + "). The primary-LPG exposure is complete in all four surveys."),
  mkTable([3400, 1450, 1450, 1350, 1000], [
    ["Variable", "NFHS-4", "NFHS-5", "ACCESS (both waves)", "IRES"],
  ].concat(tableS11rows)),
  p("Values are percent missing. Missingness is low throughout, the exceptions being caste (" + missPct1("NFHS-4", "Caste") + "% in NFHS-4 and " + missPct1("NFHS-5", "Caste") + "% in NFHS-5, from unresolved recode categories) and the IRES monthly-expenditure item (" + missPct1("IRES", "Wealth (index/expenditure)") + "%), which feeds only the within-state wealth quintile. Households missing a covariate still contribute to the primary-LPG exposure estimate (which uses the fuel item alone, complete for all households) and are handled by complete-case analysis in the covariate-based prediction models (Section 2.4.4). Beyond item missingness, cluster linkage excluded NFHS-5 households lacking GPS coordinates (" + grp(L_NOGPS) + ") and those in clusters more than 10 km from any district polygon (" + grp(L_EXCL_HH) + " households in " + grp(L_EXCL) + " clusters); " + L_SHARE + "% of GPS-located NFHS-5 households were assigned to a district, and all ACCESS and IRES households matched by census code. District-level estimates were produced for " + N_D_NFHS4 + " (NFHS-4) and " + N_D_NFHS5 + " (NFHS-5) districts. Recent-cohort censoring of the mortality outcome is by design (Section 2.4.5), not missing data.", { run: { size: 20, italics: true } }),
  p("", {}),

  caption("Table S12. Transfer of the household fuel-stacking model across eras and across survey instruments (SI Methods S3), estimated on the six states ACCESS and IRES share, rural households only, with the district-context covariate removed so that no era-specific quantity enters the model. Each row trains the model in one survey and evaluates it in another. 'Contrast' names what changes between training and test: ERA holds the instrument fixed (ACCESS Wave 1 in 2014-15 to ACCESS Wave 2 in 2018), INSTRUMENT holds the era approximately fixed (IRES in 2019-20 to ACCESS Wave 2 in 2018), and ERA+INSTRUMENT changes both and is the comparison the secondary analysis reports. 'N hh' is the number of test-survey households; AUC is household-level discrimination; the calibration slope is the coefficient from regressing the observed outcome on the predicted log-odds, where 1 indicates predictions on the right scale and values near 0 indicate no usable signal; 'District r' is the Pearson correlation between predicted and observed district-level stacking share, the level at which the predictions are used. In-sample rows are shown only as a reference ceiling. 'Unrestricted IRES' rows drop the common-state restriction; 'transferable subset' rows refit using only the predictors whose coefficients are stable across both contrasts."),
  tableS12, p("", {}),

  caption("Table S13. Distribution of each transfer-model predictor across the three frames of SI Methods S3 (ACCESS Wave 1 2014-15, ACCESS Wave 2 2018, and rural IRES 2019-20), restricted to the six common states. Entries are means for continuous predictors and category shares for categorical ones. Reading left to right traces the era change and then the instrument change. Predictors that move monotonically across the three columns (ration-card holding, household size, electrification) are consistent with genuine population change over the transition; a predictor that reverses direction precisely at the instrument change (the Other Backward Class share) is more consistent with the two surveys classifying the same households differently."),
  tableS13, p("", {}),

  caption("Table S14. Contribution of each predictor to out-of-sample transfer of the stacking model (SI Methods S3), by refitting with that predictor dropped and re-evaluating in ACCESS Wave 1 (W1) and Wave 2 (W2). 'District r without it' is the district-level correlation the reduced model achieves; 'Contribution' is the loss relative to the full model, so a positive value means the predictor helps transfer and a negative value means dropping it improves transfer. With correlations this low the ordering is unstable and is reported to document that no reweighting of this covariate set produces a transportable model, not to identify which covariate to retain."),
  tableS14, p("", {}),

  caption("Table S15. Coefficient stability of the stacking model across the three contrasts of SI Methods S3. For each contrast and each model term, entries are the coefficient estimated in the first-named training survey (survey A) and in the second (survey B), their difference, and a two-sample z test of that difference on the log-odds scale. Because ACCESS is a panel, the Wave 1 and Wave 2 estimates in the ERA contrast are positively correlated across waves and the test ignores that covariance, so the ERA rows are conservative (they under-reject). The instrument contrast is not era-pure - ACCESS Wave 2 (2018) and IRES (2019-20) are one to two years apart - so it will if anything overstate the instrument component."),
  tableS15, p("", {}),

  caption("Table S16. Precision frontier for the change-on-change child-mortality models (SI Methods S4): the association between the district change in corrected primary-LPG prevalence and the change in child mortality, as a function of kappa, the fraction of the corrected exposure's posterior standard deviation (on the logit scale) that a hypothetical validation survey leaves in place. kappa = 1 is the study as conducted and reproduces the uncertainty-propagated rows of Table S6 exactly; kappa = 0 treats the correction as if it were known without error and reproduces the corrected point-surface rows of Table S6 exactly. Entries are the reduction in deaths per 100 births per 10-percentage-point increase in LPG prevalence, with the 95% confidence interval from Rubin's rules, the pooled p-value, and the fraction of missing information (FMI, the share of the total variance contributed by uncertainty in the corrected exposure). Rural districts, adjusted specification, weighted by the harmonic mean of eligible births across rounds, standard errors clustered on state. All models use 200 imputations drawn from the saved posterior. Rows are asterisked at the grid value nearest kappa* = " + DS("kappa_star_infant", 2) + ", the smallest precision improvement at which the infant association reaches p < 0.05."),
  tableS16,
  p("Because the kappa device shrinks the saved draws toward their own mean, it describes a validation survey that estimates the same calibration relationship more precisely; it does not describe a survey that would reveal a different relationship. See SI Methods S4 for what kappa* implies about attainable designs."),
  p("", {}),

  caption("Table S17. Where the posterior variance of a corrected district prevalence comes from (SI Methods S4), by calibration leg. Each corrected value is a + b x + u_s, with x the NFHS district estimate entering the measurement-error model as a covariate observed with known error; its posterior variance therefore splits into a calibration-side component, carried by the posterior of the intercept, slope and state effect and reducible by a larger or better-targeted validation survey, and an NFHS-side component equal to the squared calibration slope times the NFHS district estimate's own sampling variance, which no validation survey can reduce. Variances are on the logit scale and are averaged over the study districts. sigma is the fitted residual SD of the calibration; because the model also carries each reference district's own sampling standard error as a known variance, sigma is equation error - instrument non-comparability - rather than reference-survey sampling noise, and is therefore not reduced by sampling more reference households."),
  tableS17,
  p("", {}),

  ...(HAVE_PPD ? [
    caption("Table S18. Estimand sensitivity for the child-mortality models (SI Methods S5). The primary analysis carries the calibrated {{expectation}} of each district's prevalence into the health model and propagates the posterior of the calibration by multiple imputation. Rows 1-3 of each block vary how that expectation is summarized: the point surface used for the corrected (non-propagated) rows of Table S6, the same posterior averaged on the probability rather than the logit scale (the Jensen check), and the uncertainty-propagated primary specification. Rows 4-5 replace the expectation with the posterior {{predictive}}, adding the calibration model's residual variance so that each imputation is a draw of the district's putative true prevalence. Entries are the reduction in deaths per 100 births per 10-percentage-point increase in LPG prevalence, 95% confidence interval, pooled p-value, and fraction of missing information. Rural districts, adjusted specification, weighted by the harmonic mean of eligible births across rounds, standard errors clustered on state. Nothing is refitted in the calibration: all rows are computed from the saved posterior draws."),
    tableS18,
    p("", {}),
  ] : []),

  ...(HAVE_NSSO ? nssoTablesSI : []),

  h1("Supplementary Figures"),
  img("maps/SI_1_reference_coverage.jpeg", 400, 355),
  caption("SI Figure S1. Geographic coverage of the reference energy surveys over the NFHS-4 district frame (the 640 districts of the 2011 Census). Each district is shaded by whether it was sampled by ACCESS only, IRES only, both, or neither; ACCESS covers " + cmpN(C4R) + " districts in six northern states and IRES " + cmpN(C5A) + " districts in " + I_STATES + " states. Districts sampled by neither survey receive a corrected exposure estimate only by extrapolation of the calibration and are flagged in the released data."),
  img("maps/SI_2_nfhs5_cluster_assignment.jpeg", 400, 355),
  caption("SI Figure S2. Assignment of NFHS-5 survey clusters to NFHS-4 districts (all NFHS-5 clusters, urban and rural, before the rural restriction applied in the analyses). Published NFHS-5 cluster coordinates (displaced by the DHS Program up to 2 km in urban and 5 km in rural areas) are overlaid on the NFHS-4 district boundaries and colored by how each cluster was assigned: blue, by point-in-polygon (" + grp(L_PIP) + " clusters, " + pctOfGps(L_PIP) + "% of the " + grp(L_GPS) + " located clusters); orange, to the nearest district because the point fell just outside every polygon (" + grp(L_SNAP) + " clusters, " + pctOfGps(L_SNAP) + "%). A further " + grp(L_EXCL) + " clusters lying more than 10 km outside all polygons - in the island territories and along the northeastern border - were excluded (red crosses), as were " + grp(L_NOGPS) + " records released with no GPS coordinates (not shown)."),
  img("maps/SI_2b_fallback_snap_distance.jpeg", 400, 355),
  caption("SI Figure S3. The " + grp(L_SNAP) + " nearest-district fallback clusters from SI Figure S2, colored by snap distance - the distance from the cluster point to the boundary of the district it was assigned to (median " + SNAP_MED + " km). Distances up to about 5-10 km are consistent with the DHS positional displacement of cluster coordinates; clusters more than 10 km from any polygon were excluded rather than snapped."),
  img("maps/SI_2d_fallback_share_by_district.jpeg", 400, 355),
  caption("SI Figure S4. District-level share of NFHS-5 households (all households, urban and rural, before the rural restriction) assigned by the nearest-district fallback rather than by point-in-polygon. Each district is shaded by the fraction of its NFHS-5 households that were snapped from just outside its polygon; districts with high shares (mostly small or coastal districts) are those whose corrected estimates are most sensitive to the assignment rule and are natural candidates for sensitivity checks."),
  img("maps/SI_3_benchmark_scatter.jpeg", 460, 141),
  caption("SI Figure S5. Benchmark-variable agreement across surveys (rural districts). Each panel plots the NFHS district prevalence (vertical axis) against the reference-survey prevalence (horizontal axis) for a characteristic measured comparably in both surveys - Scheduled Caste, Scheduled Tribe, Hindu, Muslim, household electricity, and ration card - shown alongside primary LPG, for NFHS-4 versus ACCESS (top row) and NFHS-5 versus IRES (bottom row); the dashed line is the line of equality. Points falling on the line for the demographic benchmarks, contrasted with the systematic off-line shift for primary LPG, indicate that the two surveys sample comparable populations and that the discrepancy is specific to the fuel item."),
  img("paper_figs/fig3_proxies.jpeg", 460, 212),
  caption("SI Figure S6. District exposure proxies from the fuel-use prediction models (secondary analysis, SI Methods S7), applied to all NFHS-5 households, urban and rural (this descriptive map is the one place the proxies are not restricted to the rural frame; the district composition proxies entering the health models of SI Methods S6 are rural in both rounds). (a) Predicted probability of fuel stacking among primary-LPG households; (b) predicted share of households burning any solid fuel. These are model-predicted metrics with only moderate household-level discrimination (stacking AUC " + ldo("ACCESS W1", "auc") + " in ACCESS, " + ldo("IRES rural", "auc") + " in rural IRES) and limited temporal transportability (an IRES-trained stacking model shows r = " + CROSS_ERA_R + " against observed 2015 ACCESS district stacking); see SI Methods S2 and Table S8. Predicted stacking conditional on LPG is highest across the northwestern and Indo-Gangetic states and lowest in the south, and the expected share of households burning any solid fuel remains above one-half across much of central and eastern India even after correction of the primary-fuel prevalence."),
  img("fig_htn_rural.jpeg", 360, 320),
  caption("SI Figure S7. Rural district prevalence of measured hypertension at NFHS-5 (2019-21), from a null four-level multilevel model (household, cluster, district, and state). Hypertension is defined (JNC7 criteria) as a mean of the second and third measured readings with systolic blood pressure at least 140 mmHg or diastolic at least 90 mmHg, or current use of antihypertensive medication. This district surface is one of the outcomes for the change-on-change analysis in Table S5 and SI Figure S9."),
  img("fig_dm_rural.jpeg", 360, 320),
  caption("SI Figure S8. Rural district prevalence of diabetes at NFHS-5 (2019-21), estimated with the same four-level multilevel model as SI Figure S7. Diabetes is the composite item (measured high blood glucose or self-report of ever being told by a health professional of high blood sugar). This district surface is one of the outcomes for the change-on-change analysis in Table S5 and SI Figure S9."),
  img("fig_adult_rural.jpeg", 360, 383),
  caption("SI Figure S9. District change-on-change association between primary-LPG prevalence and adult cardiometabolic outcomes (change from NFHS-4 to NFHS-5), by exposure measurement, for rural districts. Points and horizontal bars are the point estimate and 95% confidence interval of the change in adult prevalence (percentage points) per 10-percentage-point rise in primary-LPG prevalence, for hypertension, the diabetes composite, and self-reported diabetes, using raw, regression-calibrated, and Bayesian-corrected exposure; models adjust as in Table S5 with standard errors clustered on state (" + N_ADULT + " districts). Unlike child mortality (main-text Figure 3), these associations are null and do not strengthen coherently under correction."),
  img("fig_nuanced.jpeg", 400, 400),
  caption("SI Figure S10. Exploratory analysis (Table S8): district change-on-change association between predicted fuel-use composition metrics and child mortality, rural districts (N = " + N_NUANCE + "). For each mortality outcome the panel shows the corrected primary-LPG prevalence (the exposure of record) alongside the three model-predicted composition shares - the LPG-no-solid-fuel-reported share, the any-solid-fuel-reported share, and the LPG-and-solid-fuel (stacking) share - each oriented so that a move toward cleaner cooking points in the same direction (predicted LPG consumption is reported in Table S8 but is not plotted here because it is scaled per 10 kg/year rather than per 10 percentage points). Points and bars are point estimates and 95% confidence intervals per 10-percentage-point move, with the same adjustment as Table S6. Only the corrected primary-LPG exposure behaves as hypothesized: all three predicted composition shares point the opposite way once oriented, and the stacking contrast is the one that attains conventional significance (the pipeline flags this sign disagreement as a check warning). Because the shares sum to one, are model-predicted rather than measured, and are mutually collinear, their individual coefficients are not separately interpretable; we read the pattern as diagnostic of the limits of the prediction models rather than as evidence that fuel stacking is protective, and the figure is hypothesis-generating only."),
  img("fig_health_energy.jpeg", 460, 300),
  caption("SI Figure S11. External check independent of the correction: cross-sectional (level-on-level) association between reference-survey energy metrics measured directly in the energy surveys and NFHS district child mortality, within each era-matched pair (ACCESS with NFHS-4; IRES with NFHS-5), rural districts. Each point is a district; the vertical axis is NFHS district infant mortality and the horizontal axis the reference-survey metric (primary-LPG prevalence, share burning any solid fuel, and fuel-stacking share). Pearson correlations, over neonatal and infant mortality respectively, are " + hePair("Any solid-fuel burning") + " for the share burning any solid fuel; " + hePair("Primary LPG") + " for primary-LPG prevalence; and " + hePair("Fuel stacking") + " for the fuel-stacking share. The expected directions are positive for solid-fuel burning and negative for primary-LPG prevalence; where a correlation above departs from that direction, the corroboration this check offers the corrected-exposure findings is correspondingly weaker. The ACCESS-era panels rest on " + cmpN(C4R) + " districts in six northern states. These associations are unadjusted and cross-sectional and are shown only as a directional corroboration."),
  h1("Supplementary district atlas"),
  p("To make every district-level quantity discussed in the paper visually inspectable, we provide a district atlas: choropleth maps of the NFHS covariates, the corrected and predicted exposure surfaces, the reference-survey prevalences, and a spatial version of the benchmark falsification check. All maps are drawn on the common NFHS-4 district geography by the released mapping scripts, which also generate the complete atlas - every covariate, exposure, composition, reference-survey, and health quantity, for each round and as the 2015-2019 change - beyond the representative panels reproduced here."),
  img("maps/atlas_panels/SES_levels_2019.jpeg", 460, 483),
  caption("SI Figure S12. District atlas of the NFHS-5 (2019-21) covariates entering the change-on-change health models, rural households: primary LPG and the socioeconomic adjustment covariates (poverty, low maternal education, household electricity, Muslim share, improved sanitation, improved water). Each panel is the null four-level multilevel district estimate; colour is the district prevalence (%). Analogous panels for NFHS-4 and for the 2015-2019 change are produced by the mapping scripts."),
  img("maps/atlas_panels/exposure_corrected.jpeg", 466, 326),
  caption("SI Figure S13. Corrected district surfaces of rural primary-LPG prevalence from both correction methods and both rounds: regression-calibrated and Bayesian measurement-error-corrected, for NFHS-4 (2015-16) and NFHS-5 (2019-21). The Bayesian NFHS-5 surface is the primary national product (also shown in main-text Figure 2); the corrected NFHS-4 surface outside the six ACCESS states is an extrapolation (Section 2.4.3)."),
  img("maps/atlas_panels/composition_2019.jpeg", 466, 326),
  caption("SI Figure S14. Predicted fuel-use composition for NFHS-5 (2019-21) rural households from the augmentation models (Section 2.4.4): the LPG-no-solid-reported share, the fuel-stacking share, the share of households reporting any solid fuel, and predicted annual LPG consumption (kg/yr). These are model-predicted district metrics and carry the caveats noted for SI Table S8."),
  img("maps/atlas_panels/reference_ACCESS.jpeg", 466, 326),
  caption("SI Figure S15. ACCESS reference-survey district prevalences (" + cmpN(C4R) + " districts in six northern states): primary LPG in Wave 1 (2015) and Wave 2 (2018), and Wave 1 fuel stacking, the LPG-no-solid-reported share, and annual LPG consumption. The mapped frame is the Wave 1 district set, so the three districts sampled only in Wave 2 do not appear. ACCESS sampled only rural households, so every panel is a rural estimate. Districts not sampled by ACCESS are shown in grey."),
  img("maps/atlas_panels/reference_IRES.jpeg", 466, 326),
  caption("SI Figure S16. IRES reference-survey district prevalences (2019-20; " + cmpN(C5A) + " districts in " + I_STATES + " states). Primary LPG is shown both for all sampled households and for the rural subsample (the rural estimate is the one used to calibrate NFHS-5, Section 2.4.3); fuel stacking, the LPG-no-solid-reported share, and annual LPG consumption are estimated on all sampled IRES households. Districts not sampled by IRES are shown in grey."),
  img("fig_sbs_nfhs4_access.jpeg", 468, 370),
  caption("SI Figure S17. Spatial benchmark falsification, NFHS-4 versus ACCESS Wave 1 - the map analogue of SI Figure S5 - on the " + sbs("NFHS4_ACCESS", "lpg", "n_districts", 0) + " overlap districts, rural households in both surveys. Eight benchmark variables are shown as NFHS/reference pairs, two pairs per row, each pair on its own shared colour scale: primary LPG and Scheduled Caste (first row), Scheduled Tribe and Scheduled Caste or Tribe (second), Hindu share and Muslim share (third), household electricity and the BPL/Antyodaya ration card (fourth). Within every pair the NFHS panel is on the left and the reference panel on its right. Each NFHS panel is annotated with the Pearson correlation and the mean difference (NFHS minus reference, percentage points) across districts. In this era the two surveys agree closely on both composition and fuel: Scheduled Tribe surfaces are near-identical (r = " + sbs("NFHS4_ACCESS", "st", "pearson_r") + "), and primary LPG also agrees well (r = " + sbs("NFHS4_ACCESS", "lpg", "pearson_r") + ", mean difference " + sbsDiff("NFHS4_ACCESS", "lpg") + " percentage points). This figure therefore establishes the benchmark baseline rather than the discrepancy; the divergence appears in the NFHS-5/IRES era (SI Figure S18). Scheduled Caste is the weakest of the demographic benchmarks here (r = " + sbs("NFHS4_ACCESS", "sc", "pearson_r") + "), reflecting the finer spatial texture of caste composition relative to the district samples."),
  img("fig_sbs_nfhs5_ires.jpeg", 468, 370),
  caption("SI Figure S18. Spatial benchmark falsification, NFHS-5 versus IRES, on the " + sbs("NFHS5_IRES", "lpg", "n_districts", 0) + " overlap districts, rural households in both surveys, in the same eight-variable layout as SI Figure S17 (NFHS/reference pairs, two pairs per row, in the same variable order). Primary LPG shows NFHS-5 systematically lower than IRES by " + sbsDiff("NFHS5_IRES", "lpg") + " percentage points (r = " + sbs("NFHS5_IRES", "lpg", "pearson_r") + "), whereas the census-like demographic benchmarks agree spatially and in level: Scheduled Tribe r = " + sbs("NFHS5_IRES", "st", "pearson_r") + " with a mean difference of " + sbsDiff("NFHS5_IRES", "st") + " points, Hindu share r = " + sbs("NFHS5_IRES", "hindu", "pearson_r") + ", Muslim share r = " + sbs("NFHS5_IRES", "muslim", "pearson_r") + ". That contrast is the visual form of the falsification argument: the surveys reach comparably composed populations and disagree specifically on the fuel item. Two benchmarks do not fit this clean pattern and are shown rather than omitted. The BPL/Antyodaya ration card differs by " + sbsDiff("NFHS5_IRES", "bpl") + " points (r = " + sbs("NFHS5_IRES", "bpl", "pearson_r") + ") and household electricity correlates only weakly across districts (r = " + sbs("NFHS5_IRES", "electricity", "pearson_r") + ") despite a small mean difference of " + sbsDiff("NFHS5_IRES", "electricity") + " points. Neither is a census-like attribute: both are programme-defined statuses whose measurement depends on how the question is posed, so they behave like the fuel item rather than like composition, and we treat them as instrument-sensitive benchmarks rather than as evidence of differing samples (Section 3.1)."),

  img("maps/SI_ml_vs_designwt.jpeg", 440, 440),
  caption("SI Figure S19. Multilevel versus design-weighted district primary-LPG prevalence, within each survey (NFHS-4 rural, NFHS-5 rural, ACCESS Wave 1, IRES). Each point is a district; the vertical axis is the unweighted multilevel small-area estimate (the primary specification) and the horizontal axis the design-weighted direct estimate (weighted proportion with Taylor-linearized standard error); the dashed line is equality. The two estimators agree closely (Pearson r " + mlwtRange("pearson_r") + "; Lin's CCC " + mlwtRange("ccc") + "), the multilevel estimate averaging slightly lower than the design-weighted one in NFHS (mean difference " + mlwtDiffPP("NFHS-4 rural") + " percentage points for NFHS-4, " + mlwtDiffPP("NFHS-5 rural") + " for NFHS-5) because partial pooling shrinks small-sample districts toward the study-area mean. The close agreement confirms the NFHS-versus-reference comparison is not an artifact of the weighting or estimation choice."),

  img("maps/SI_estimator_mix.jpeg", 520, 312),
  caption("SI Figure S20. District primary-LPG prevalence from each alternative estimator and correction, plotted against the raw multilevel estimate, for NFHS-4 (top row) and NFHS-5 (bottom row): raw design-weighted; regression-calibrated fit on the multilevel input; regression-calibrated fit on the design-weighted input; and Bayesian measurement-error corrected. The dashed line is equality. Before PMUY (NFHS-4) all estimators lie near the line of equality (the correction is mild); after PMUY (NFHS-5) the three corrections lift district prevalence well above the raw estimate. Regression calibration is nearly identical whether fit on the multilevel or the design-weighted raw estimate (district correlation " + emCor(2015, "rc_ml", "rc_wt") + " in 2015 and " + emCor(2019, "rc_ml", "rc_wt") + " in 2019; Table S10), so the correction does not depend on the raw-estimator basis; the Bayesian correction lands at a similar national level (" + BAY19_0 + "% in 2019, against " + RC19_0 + "% for regression calibration) but reorders districts somewhat more, owing to its hierarchical shrinkage and propagated measurement error."),

  img("fig_mort_rural.jpeg", 380, 380),
  caption("SI Figure S21. Exposure measurement and the LPG-child-mortality association (rural districts, the demonstration population of Section 3.3 and SI Methods S6). Points and horizontal bars are the point estimate and 95% confidence interval of the change in mortality (deaths per 100 births) per 10-percentage-point rise in primary-LPG prevalence, for neonatal and infant mortality, across eight exposure specifications: raw NFHS exposure; regression-calibrated; Bayesian-corrected (era-matched); the Bayesian correction with its uncertainty propagated by multiple imputation (the specification reported as the result); the Bayesian correction using a single calibration instrument (the IRES rural calibration applied to both rounds); that instrument-consistent version with its uncertainty propagated; and the two corresponding variants of the symmetric NFHS-4 refit of SI Methods S1, in which the ACCESS reference is the design-weighted direct estimate carrying its own Taylor-linearized standard error, without and with uncertainty propagation. SI Table S6 reports the first six; the last two are shown here only, and are tabulated in the replication archive. Models are weighted by the harmonic mean of eligible births across rounds and adjust for concurrent socioeconomic and ambient (PM2.5, temperature, relative humidity, drought) covariates with region fixed effects and state-clustered SEs (" + N_RURAL + " districts). The estimates strengthen from raw to corrected exposure, but weaken once the correction's own uncertainty is carried through, or once a single calibration instrument is used for both rounds. The symmetric-refit variants sit within a fraction of a standard error of their primary counterparts."),

  ...(HAVE_NSSO ? nssoFigsSI : []),

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
  }),
  Packer.toBuffer(mkDoc(siChildren)).then((buf) => {
    fs.writeFileSync(OUT + "ACCESS_Health_SI.docx", buf);
    console.log("Wrote " + OUT + "ACCESS_Health_SI.docx");
  }),
]).catch((e) => {                 // an unhandled rejection would exit 0 in some
  console.error("BUILD FAILED: " + e.message);   // Node versions -- exit nonzero
  process.exit(1);                               // so the R caller sees it.
});
