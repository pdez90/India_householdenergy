## build_docx.R -- rebuild ACCESS_Health_main.docx + ACCESS_Health_SI.docx
##
## ONE definition of the manuscript build, used by TWO callers:
##   * run_everything.R, as the final step of a full pipeline run;
##   * build_docx_only.R, to rebuild the manuscript from analysis outputs that
##     are already current, without re-running (or disturbing) the analysis.
##
## Keeping this in a single sourced file is deliberate. The alternative -- the
## same forty lines copied into both callers -- is exactly how a standalone
## rebuild silently stops matching the pipeline's rebuild: someone adds a figure
## to the renames table in one copy and not the other, and the two routes start
## producing different documents from identical inputs. There is one table, one
## build command, and one set of pass conditions here.
##
## Sourcing this file defines functions only; it runs nothing and prints nothing.

## ---- is the Node 'docx' module resolvable for a build launched from DIR_OUT? --
##
## Two bugs lived in the one-liner this replaced, and both surfaced only AFTER
## the full analysis had run -- turning a 150-minute success into a hard stop on
## a correctly installed tree:
##
##  (1) Working directory. Node resolves modules relative to the process cwd,
##      walking upward through parent folders. run_everything.R setwd()s to the
##      scripts folder at startup, so a probe launched from there searches
##      scripts/node_modules and its parents -- never DIR_OUT/node_modules,
##      where `npm install docx` actually puts it. The real build runs from
##      DIR_OUT, so the probe must too.
##  (2) Shell quoting. system2() does NOT quote its arguments. The argument
##      require.resolve('docx') reached /bin/sh with bare parentheses and died
##      as "syntax error near unexpected token `('" -- a nonzero exit status
##      indistinguishable from a genuinely missing module.
##
## So: check the filesystem FIRST (no shell, nothing to quote, cannot be
## defeated by either bug), and fall back to a properly shQuote()d node probe
## only for the case where docx legitimately lives elsewhere on the resolution
## path -- a parent folder, a global install, or NODE_PATH.
docx_module_available <- function(node, dir_out) {
  if (file.exists(file.path(dir_out, "node_modules", "docx", "package.json")))
    return(TRUE)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dir_out)
  rc <- tryCatch(
    suppressWarnings(system2(node, c("-e", shQuote("require.resolve('docx')")),
                             stdout = FALSE, stderr = FALSE)),
    error = function(e) 1L)
  isTRUE(rc == 0)
}

## ---- the eight figures the builder embeds under fixed names ------------------
## Left column: the canonical output, written by the analysis script that owns
## it. Right column: the fixed name build_manuscript.js looks for. Refreshing
## the copies immediately before every build is what stops the docx embedding a
## figure from a previous run while quoting this run's numbers beside it.
DOCX_FIGURES <- matrix(byrow = TRUE, ncol = 2, c(
  "health_effects_coefplot.jpeg",                             "fig_mort_rural.jpeg",
  "si_adult_health_coefplot.jpeg",                            "fig_adult_rural.jpeg",
  "health_nuanced_coefplot.jpeg",                             "fig_nuanced.jpeg",
  "maps/SI_diabetes_2019.jpeg",                               "fig_dm_rural.jpeg",
  "maps/SI_hypertension_2019.jpeg",                           "fig_htn_rural.jpeg",
  "maps/SI_health_energy_scatter.jpeg",                       "fig_health_energy.jpeg",
  "maps/atlas_panels/benchmark_sidebyside_NFHS4_ACCESS.jpeg", "fig_sbs_nfhs4_access.jpeg",
  "maps/atlas_panels/benchmark_sidebyside_NFHS5_IRES.jpeg",   "fig_sbs_nfhs5_ires.jpeg"))

## ---- static artwork the builder embeds but no script produces ----------------
## fig_overview.png (the graphical abstract) and fig_eq_selogit.png (a typeset
## equation) are hand-made assets, not analysis output. They used to live loose
## in DIR_OUT, which worked only for as long as nobody started from a clean
## outputs directory -- and the 2026-09-16 canonical run did exactly that, so
## the build died on a bare fs.readFileSync ENOENT after every analysis step had
## already succeeded. They now live in the REPO, under manuscript/assets/, and
## are copied in beside the regenerated figures immediately before each build.
## Same rule as above: a missing one is fatal, and the stale destination is
## removed first so a forced build cannot quietly reuse it.
DOCX_STATIC_ASSETS <- c("fig_overview.png", "fig_eq_selogit.png")

## ---- the number audit that runs on every build -------------------------------
## audit_numbers.js re-reads the document that was just written and asks two
## questions the build itself cannot: is any number TYPED into prose rather than
## interpolated from an output file, and does every printed statistic trace to a
## value some CSV actually contains. It is copied in beside the builder for the
## same reason the builder is copied in -- so the version that runs is the
## version in the repository, not whatever happens to be sitting in DIR_OUT.
## A hard-coded literal exits non-zero, which is a FAIL under STRICT_CHECKS and
## a loud warning otherwise; the traceability pass reports and never fails.
DOCX_AUDIT_SCRIPT <- "audit_numbers.js"

## Where those assets live, resolved from THIS file's own location rather than
## from a caller-supplied variable. Both callers (run_everything.R and
## build_docx_only.R) sit in scripts/, so --file= points into scripts/ either
## way; ../manuscript/assets is then unambiguous no matter what the working
## directory is when the build runs.
DOCX_ASSET_DIR <- local({
  here <- tryCatch({
    a <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", a[grepl("^--file=", a)])
    if (length(f)) dirname(normalizePath(f[1])) else getwd()
  }, error = function(e) getwd())
  file.path(dirname(here), "manuscript", "assets")
})

## ---- build ------------------------------------------------------------------
## Returns TRUE on success; stop()s with an actionable message otherwise. It
## never returns FALSE: a manuscript that did not rebuild is a failure to
## surface, not a value to test and ignore.
##
## `resume_cmd` is the command quoted back to the user in every failure message,
## so each caller can name the way back into ITS own workflow.
build_manuscript_docx <- function(dir_out,
                                  resume_cmd = "Rscript build_docx_only.R") {
  node <- Sys.which("node")
  js   <- file.path(dir_out, "build_manuscript.js")

  ## STAGE THE BUILDER FIRST. This used to only CHECK that a build_manuscript.js
  ## was present in dir_out and stop if it was not -- the copy happened in
  ## run_pipeline.sh alone. So a full launcher run built from the current
  ## builder, while `Rscript build_docx_only.R` silently rebuilt from whatever
  ## copy the LAST full run had left behind, printed "built OK", passed the
  ## number audit (audit_numbers.js is copied fresh a few lines down, so a
  ## current auditor checked a stale builder and found it self-consistent) and
  ## claimed the documents reflected the current outputs. Editing the manuscript
  ## text and rebuilding therefore appeared to work and changed nothing. The
  ## numbers were never at risk -- they come from the CSVs either way -- but the
  ## PROSE around them could lag the repository indefinitely, which is the same
  ## staleness this file's figure handling already treats as fatal.
  ## Copy it here, from this file's own location, exactly as the assets and the
  ## audit script are copied, so every caller builds from the same source.
  js_src <- file.path(DOCX_ASSET_DIR, "..", "build_manuscript.js")
  if (file.exists(js_src)) {
    if (!isTRUE(file.copy(js_src, js, overwrite = TRUE)))
      stop("\n[docx] could not copy the manuscript builder into ", dir_out,
           "\n  from ", normalizePath(js_src), call. = FALSE)
    pkg_src <- file.path(DOCX_ASSET_DIR, "..", "package.json")
    if (file.exists(pkg_src))
      file.copy(pkg_src, file.path(dir_out, "package.json"), overwrite = TRUE)
  }

  if (!nzchar(node)) {
    stop("\n[docx] Node.js not found on PATH, so the manuscript cannot be rebuilt.\n",
         "  Any ACCESS_Health_main.docx / ACCESS_Health_SI.docx on disk predate\n",
         "  the current analysis outputs -- do not read them as current results.\n",
         "  Install Node (https://nodejs.org, or `brew install node`), then:\n",
         "      ", resume_cmd, call. = FALSE)
  }
  if (!file.exists(js)) {
    stop("\n[docx] build_manuscript.js not found in ", dir_out, ".\n",
         "  The manuscript cannot be rebuilt and the .docx files on disk are stale.\n",
         "  Restore build_manuscript.js, then:\n",
         "      ", resume_cmd, call. = FALSE)
  }
  if (!docx_module_available(node, dir_out)) {
    stop("\n[docx] the Node 'docx' module is not installed, so the manuscript was\n",
         "  NOT rebuilt. Install it once, in the output folder:\n",
         "      cd ", dir_out, " && npm init -y && npm install docx\n",
         "  then:  ", resume_cmd, call. = FALSE)
  }

  ## A missing figure source is FATAL. It used to warn and carry on, which is the
  ## worst of the three options: the destination copy from the PREVIOUS run was
  ## still sitting in dir_out, so the build succeeded, the freshness check below
  ## passed (it verifies that the .docx was rewritten, not that each embedded
  ## image was current), and the manuscript went out with one old figure beside
  ## this run's numbers. Nothing downstream could detect that. Fail here instead,
  ## and delete the stale destination first so a forced build cannot reuse it.
  missing_figs <- character()
  for (k in seq_len(nrow(DOCX_FIGURES))) {
    src <- file.path(dir_out, DOCX_FIGURES[k, 1])
    dst <- file.path(dir_out, DOCX_FIGURES[k, 2])
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
    } else {
      if (file.exists(dst)) unlink(dst)
      missing_figs <- c(missing_figs, DOCX_FIGURES[k, 1])
    }
  }
  ## The record of the abandoned outcome-conditioned imputation (SI Section S12)
  ## lives in the repository, not in the outputs: copy it in fresh before every
  ## build so the SI documents exactly the committed record. A missing record is
  ## fatal, like a missing figure.
  rec_src <- file.path(DOCX_ASSET_DIR, "..", "..", "records", "health_imputation")
  rec_dst <- file.path(dir_out, "records", "health_imputation")
  rec_files <- if (dir.exists(rec_src)) list.files(rec_src, pattern = "\\.csv$") else character()
  if (!length(rec_files))
    stop("\n[docx] records/health_imputation/*.csv not found in the repository (", rec_src, "),\n",
         "  so SI Section S12 cannot be built. Restore them from version control, then:\n",
         "      ", resume_cmd, call. = FALSE)
  dir.create(rec_dst, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(rec_dst, list.files(rec_dst)))
  for (f in rec_files) file.copy(file.path(rec_src, f), file.path(rec_dst, f), overwrite = TRUE)

  ## Static artwork, copied from the repo rather than regenerated.
  asset_dir <- DOCX_ASSET_DIR
  missing_assets <- character()
  for (a in DOCX_STATIC_ASSETS) {
    src <- file.path(asset_dir, a)
    dst <- file.path(dir_out, a)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
    } else {
      if (file.exists(dst)) unlink(dst)
      missing_assets <- c(missing_assets, file.path(asset_dir, a))
    }
  }
  if (length(missing_assets))
    stop("\n[docx] ", length(missing_assets), " static manuscript asset(s) are missing from the\n",
         "  repository, so the manuscript was NOT rebuilt:\n",
         paste0("    ", missing_assets, collapse = "\n"), "\n",
         "  These are hand-made artwork, not analysis output -- restore them from\n",
         "  version control (git checkout -- India_householdenergy/manuscript/assets), then:\n",
         "      ", resume_cmd, call. = FALSE)

  if (length(missing_figs))
    stop("\n[docx] ", length(missing_figs), " required figure source(s) are missing, so the\n",
         "  manuscript was NOT rebuilt (it would have embedded figures from an earlier run):\n",
         paste0("    ", missing_figs, collapse = "\n"), "\n",
         "  Re-run the script that writes each one, then:\n",
         "      ", resume_cmd, call. = FALSE)

  message("\n[docx] Building ACCESS_Health_main.docx + _SI.docx with Node ...")
  docx_paths <- file.path(dir_out, c("ACCESS_Health_main.docx",
                                     "ACCESS_Health_SI.docx"))
  t_build <- Sys.time()
  old_wd  <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dir_out)
  out <- tryCatch(
    system2(node, "build_manuscript.js",
            env = paste0("FIG=", dir_out, "/"), stdout = TRUE, stderr = TRUE),
    error = function(e) structure(paste("node failed:", conditionMessage(e)),
                                  status = 1L))
  setwd(old_wd)
  cat(paste(out, collapse = "\n"), "\n")

  ## Three independent conditions, all required. Exit status alone is not enough
  ## (a builder can exit 0 having written nothing), and existence alone is not
  ## enough (the files from the PREVIOUS run also exist, and look perfectly fine).
  status    <- attr(out, "status"); status <- if (is.null(status)) 0L else status
  info      <- file.info(docx_paths)
  exists_ok <- !is.na(info$size)
  size_ok   <- exists_ok & info$size > 0
  fresh_ok  <- exists_ok & info$mtime >= t_build
  if (status != 0L || !all(size_ok) || !all(fresh_ok)) {
    diag_lines <- paste0(
      "    ", basename(docx_paths), ": ",
      ifelse(!exists_ok, "MISSING",
      ifelse(!size_ok,   "zero bytes",
      ifelse(!fresh_ok,  paste0("STALE (not rewritten; mtime ", format(info$mtime), ")"),
                         paste0("ok, ", info$size, " bytes")))),
      collapse = "\n")
    stop("\n[docx] the manuscript build FAILED (node exit status ", status, ").\n",
         diag_lines, "\n",
         "  The analysis outputs are current but the .docx files are NOT.\n",
         "  Fix the builder, then:\n",
         "      ", resume_cmd, call. = FALSE)
  }
  message(sprintf("[docx] built OK: %s.",
                  paste(sprintf("%s (%.0f KB)", basename(docx_paths),
                                info$size / 1024), collapse = ", ")))

  ## ---- release receipt --------------------------------------------------------
  ## A document-only rebuild is legitimate -- the numbers come from the CSVs, so
  ## a wording correction does not need seven hours of analysis re-run -- but it
  ## leaves the .docx files postdating the run that certified the outputs, with
  ## nothing on disk connecting the two. The honest fix is not to pretend the
  ## whole thing was one computation; it is to record which analysis run the
  ## outputs come from, which builder turned them into a document, and what
  ## exactly came out, so the provenance statement in a release cites a file
  ## instead of asking a reader to take the history on trust.
  ##
  ## Everything here is read off disk at build time. The canonical run id and
  ## tally come from the check trail as it stands; if a later run replaces that
  ## trail, this receipt is superseded along with it, which is the correct
  ## coupling.
  .md5 <- function(f) if (file.exists(f) && requireNamespace("tools", quietly = TRUE))
    unname(tools::md5sum(f)) else NA_character_
  .rel <- local({
    trail <- file.path(dir_out, "diagnostics", "pipeline_checks.csv")
    rid <- tally <- NA_character_
    if (file.exists(trail)) {
      tr <- tryCatch(utils::read.csv(trail, stringsAsFactors = FALSE),
                     error = function(e) NULL)
      if (!is.null(tr) && nrow(tr)) {
        if ("run_id" %in% names(tr)) {
          u <- unique(tr$run_id[!is.na(tr$run_id)])
          # More than one id means the trail is a blend, and saying so is the
          # whole point of recording it.
          rid <- if (length(u) == 1L) u else paste0("MIXED(", length(u), "): ",
                                                    paste(u, collapse = " "))
        }
        st <- table(factor(tr$status, levels = c("PASS", "WARN", "FAIL")))
        # Semicolon, not a comma: this string is a CSV field, and a reader who
        # opens the receipt in a spreadsheet should not see a correctly quoted
        # value that nonetheless looks like it spilled into the next column.
        tally <- sprintf("%d PASS / %d WARN / %d FAIL over %d rows; %d scripts",
                         st[["PASS"]], st[["WARN"]], st[["FAIL"]], nrow(tr),
                         length(unique(tr$script)))
      }
    }
    env <- file.path(dir_out, "environment_snapshot.csv")
    rver <- NA_character_
    if (file.exists(env)) {
      e <- tryCatch(utils::read.csv(env, stringsAsFactors = FALSE),
                    error = function(e) NULL)
      if (!is.null(e) && "quantity" %in% names(e))
        rver <- e$value[match("r_version", e$quantity)]
    }
    data.frame(
      quantity = c("analysis_run_id", "analysis_check_tally", "analysis_r_version",
                   "document_built", "builder_md5", "builder_source",
                   basename(docx_paths), paste0(basename(docx_paths), "_bytes")),
      value = c(rid, tally, rver,
                format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                .md5(file.path(dir_out, "build_manuscript.js")),
                normalizePath(js_src, mustWork = FALSE),
                vapply(docx_paths, .md5, character(1)),
                as.character(info$size)),
      stringsAsFactors = FALSE)
  })
  utils::write.csv(.rel, file.path(dir_out, "release_build.csv"), row.names = FALSE)
  message("[docx] release receipt -> release_build.csv  (analysis run ",
          .rel$value[1], "; document built ", .rel$value[4], ")")

  ## ---- number audit ----------------------------------------------------------
  aud_src <- file.path(DOCX_ASSET_DIR, "..", DOCX_AUDIT_SCRIPT)
  if (file.exists(aud_src)) {
    file.copy(aud_src, file.path(dir_out, DOCX_AUDIT_SCRIPT), overwrite = TRUE)
    setwd(dir_out)
    aud <- tryCatch(system2(node, DOCX_AUDIT_SCRIPT, stdout = TRUE, stderr = TRUE),
                    error = function(e) structure(
                      paste("node failed:", conditionMessage(e)), status = 1L))
    setwd(old_wd)
    cat("\n", paste(aud, collapse = "\n"), "\n", sep = "")
    ast <- attr(aud, "status"); ast <- if (is.null(ast)) 0L else ast
    # NOT reported through chk(): the document is built AFTER
    # 20_pipeline_checks.R consolidates the trail, so a row written here would
    # land in the file without appearing in the summary anyone reads. It is
    # surfaced directly instead, and is fatal under the same STRICT_CHECKS flag
    # that governs a publication run.
    if (ast != 0L) {
      msg <- paste0("[docx] audit_numbers.js found hard-coded numeric literals in ",
                    "the manuscript prose (listed above). Each one is a number that ",
                    "will not move when the analysis does. Interpolate it from an ",
                    "output file, or -- if it genuinely cannot come from one -- add ",
                    "it to PINNED in manuscript/audit_numbers.js with the reason.")
      if (toupper(Sys.getenv("STRICT_CHECKS", "FALSE")) == "TRUE")
        stop("\n", msg, call. = FALSE)
      warning(msg, call. = FALSE)
    } else {
      message("[docx] number audit clean: no hard-coded literals in prose.")
    }
  } else {
    message("[docx] NOTE: ", DOCX_AUDIT_SCRIPT, " not found beside the builder; ",
            "the manuscript number audit did NOT run.")
  }
  invisible(TRUE)
}
