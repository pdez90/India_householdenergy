## required_packages.R -- ONE list of the packages this pipeline needs.
##
## WHY THIS FILE EXISTS
##   There used to be two lists: `required` in run_everything.R, which the
##   preflight stops on, and `PKGS` in 00_snapshot_env.R, which decides what
##   goes into renv.lock. They drifted. The lockfile ended up with no entry for
##   readstata13, WeMix, broom.mixed, naniar, exactextractr or ncdf4 -- all of
##   which the preflight requires -- so a reader following the README's
##   `renv::restore()` would have restored an environment that the very next
##   command refuses to start in. Two lists of the same thing is one list too
##   many; this is the list.
##
## It defines variables and nothing else, so a standalone script can source it
## without inheriting a configuration.

## The preflight set: run_everything.R refuses to start if any of these is
## missing, and every one of them must therefore appear in renv.lock.
REQUIRED_PKGS <- c(
  "tidyverse", "haven", "readstata13", "sf", "lme4", "survey", "srvyr",
  "WeMix", "broom", "broom.mixed", "brms", "nnet", "pROC", "cowplot",
  "naniar", "lmtest", "sandwich",
  # clubSandwich supplies CR2 (Bell-McCaffrey) with Satterthwaite degrees of
  # freedom, which IS the paper's primary inference for every state-clustered
  # regression and is read directly by the manuscript builder. Not optional: a
  # run without it cannot reproduce the published numbers.
  "clubSandwich",
  "terra", "exactextractr", "ncdf4", "patchwork")   # H3 ambient + atlas

## Attached or used by individual scripts but not worth failing the preflight
## over -- either they ship with a package already required (the tidyverse
## components, pinned here by name so the lockfile records their exact versions
## rather than only the meta-package's) or a single figure degrades without
## them. They are snapshotted, not enforced.
SNAPSHOT_EXTRA_PKGS <- c(
  "dplyr", "tidyr", "purrr", "readr", "stringr", "tibble", "ggplot2",
  "scales", "viridis", "knitr", "jsonlite")

## What 00_snapshot_env.R records.
SNAPSHOT_PKGS <- unique(c(REQUIRED_PKGS, SNAPSHOT_EXTRA_PKGS))
