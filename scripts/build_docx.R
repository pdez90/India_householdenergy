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

  for (k in seq_len(nrow(DOCX_FIGURES))) {
    src <- file.path(dir_out, DOCX_FIGURES[k, 1])
    dst <- file.path(dir_out, DOCX_FIGURES[k, 2])
    if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)
    else message("[docx] WARNING: missing figure source ", DOCX_FIGURES[k, 1],
                 " -- the docx may embed an old copy.")
  }

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
  invisible(TRUE)
}
