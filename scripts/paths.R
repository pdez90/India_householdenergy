## paths.R -- resolve every input/output directory from the environment, or FAIL.
##
## WHY THIS FILE EXISTS
##   Sixteen scripts used to resolve their directories like this:
##
##       dir_out <- Sys.getenv("OUTPUT_DIR",
##                    Sys.getenv("ACCESS_DIR", "/Users/priyanka/Downloads/ACCESS_replica"))
##
##   With the environment set by run_pipeline.sh that is correct. With it unset
##   -- which is what happens to anyone who clones the repository and runs a
##   single script, and to anyone following an older README -- it silently
##   resolves to a directory on one author's laptop. Nothing errors. The script
##   either fails somewhere further down with a confusing message about a
##   missing .rds, or, on a machine that happens to have such a directory,
##   reads the wrong data and produces plausible wrong numbers.
##
##   That is the "undocumented environment-variable / fallback-path dependence"
##   this project set out to remove, and removing it from the README was not the
##   same as removing it from the code.
##
## WHY NOT JUST PUT THE DEFAULT IN 00_config.R
##   Five scripts (H1, H2, H3, 12, 22) are deliberately standalone and do not
##   source 00_config.R -- it attaches packages and does real work. This file
##   defines functions and nothing else, so a standalone script can source it
##   without inheriting a configuration it deliberately avoids.
##
## NOTE ON Sys.getenv()'s `unset` ARGUMENT
##   It is EAGERLY evaluated, so `Sys.getenv("X", stop("..."))` raises the error
##   even when X is set. That is why this is a function taking variable NAMES
##   rather than a default value.

## First of `...` that is set and non-empty; otherwise stop with the fix.
.env_dir <- function(..., what, must_exist = TRUE) {
  vars <- c(...)
  for (v in vars) {
    x <- Sys.getenv(v, "")
    if (nzchar(x)) {
      x <- path.expand(x)
      if (must_exist && !dir.exists(x))
        stop("\n[paths] ", v, " is set to a directory that does not exist:\n",
             "    ", x, "\n",
             "  (", what, ")\n", call. = FALSE)
      return(x)
    }
  }
  stop("\n[paths] ", what, " is not configured.\n",
       "  Set ", paste(vars, collapse = " or "), " in the environment.\n",
       "  The supported way to do that is the launcher, which sets every path:\n",
       "      cd <repository> && bash run_pipeline.sh\n",
       "  To run this one script on its own, export them first, e.g.\n",
       "      export ACCESS_DIR=/path/to/data DHS_DIR=/path/to/DHS_India\n",
       "      export OUTPUT_DIR=\"$ACCESS_DIR/outputs\"\n",
       "  See the 'Reproducing the analysis' section of README.md.\n",
       call. = FALSE)
}

## Named wrappers, so a call site says what it wants rather than which variable
## happens to carry it.
## OUTPUT_DIR or DIR_OUT if either is set. ACCESS_DIR is accepted only as the
## DATA ROOT, from which the documented layout puts outputs in $ACCESS_DIR/outputs
## -- it is NOT itself the output directory. The first version of this passed
## ACCESS_DIR straight through, so a standalone script run with only ACCESS_DIR
## set would have written its outputs into the data root beside the microdata.
## The launcher always sets OUTPUT_DIR, so this never fired in a supported run;
## it fired exactly where the whole point was to fail safely.
.dir_out <- function() {
  for (v in c("OUTPUT_DIR", "DIR_OUT")) {
    x <- Sys.getenv(v, "")
    if (nzchar(x)) {
      x <- path.expand(x)
      if (!dir.exists(x))
        stop("\n[paths] ", v, " is set to a directory that does not exist:\n",
             "    ", x, "\n  (the analysis output directory)\n", call. = FALSE)
      return(x)
    }
  }
  root <- Sys.getenv("ACCESS_DIR", "")
  if (nzchar(root)) {
    cand <- file.path(path.expand(root), "outputs")
    if (!dir.exists(cand))
      stop("\n[paths] OUTPUT_DIR is not set, and the documented default\n",
           "    ", cand, "\n  (ACCESS_DIR/outputs) does not exist.\n",
           "  Set OUTPUT_DIR explicitly, or run: bash run_pipeline.sh\n",
           call. = FALSE)
    return(cand)
  }
  .env_dir("OUTPUT_DIR", "DIR_OUT", what = "the analysis output directory")
}
.dir_access <- function() .env_dir("ACCESS_DIR",
                                   what = "the ACCESS/IRES data root")
.dir_dhs    <- function() .env_dir("DHS_DIR",
                                   what = "the DHS/NFHS data directory (RData images, GPS, recodes, shapefile)")
.dir_nsso   <- function() .env_dir("DIR_NSSO",
                                   what = "the NSS 78th-round unit-data directory")
