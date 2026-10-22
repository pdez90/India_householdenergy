# ==============================================================================
# 00_snapshot_env.R -- record the software environment a run actually used.
#
#   Runs automatically at the end of every pipeline run (see run_everything.R).
#   To take a snapshot by hand:
#     cd <repository>/scripts && Rscript 00_snapshot_env.R
#
# WHY NOT A HAND-WRITTEN LOCKFILE
#   A lockfile is only worth anything if it describes the environment that
#   produced the outputs. Writing one by listing packages from memory produces a
#   file that looks authoritative and is not, which is worse than having none.
#   So this script snapshots what is loaded and installed HERE, and is meant to
#   be run once after a successful canonical run, with its outputs committed.
#
# It writes, at the repository root:
#   renv.lock      if renv is installed -- the real, resolvable lockfile
#   sessionInfo.txt  always -- R version, platform, and every attached and
#                    loaded namespace with its version. This is the fallback
#                    that always works, and the thing a reader can check against.
# ==============================================================================
# Run from the scripts folder, as documented; the repository root is one level up.
repo <- normalizePath("..", mustWork = FALSE)
if (!file.exists(file.path(repo, "README.md")))
  stop("Run this from the scripts/ folder: the repository root was not found at ",
       repo, call. = FALSE)

# The package list is NOT maintained here. It is the same vector run_everything.R
# stops on, sourced from one file, plus the extras named there. This script used
# to keep its own hand-written copy; the two drifted, and renv.lock ended up with
# no entry for readstata13, WeMix, broom.mixed, naniar, exactextractr or ncdf4 --
# every one of which the preflight requires. The README tells a reader to run
# renv::restore() to obtain the environment that produced the paper, so a
# lockfile missing preflight packages is not an incomplete record, it is a
# broken instruction.
source("required_packages.R")
PKGS <- SNAPSHOT_PKGS
present <- PKGS[vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
missing <- setdiff(PKGS, present)
for (p in present) suppressPackageStartupMessages(
  try(library(p, character.only = TRUE), silent = TRUE))
if (length(missing))
  message("[env] not installed, so not recorded: ", paste(missing, collapse = ", "))

# A lockfile that silently omits a package the preflight requires is the exact
# failure this file was rewritten to prevent, so say so rather than letting the
# snapshot quietly shrink. The pipeline's own preflight has already stopped for
# this case; the guard is here for a snapshot taken by hand in a bare session.
.req_missing <- setdiff(REQUIRED_PKGS, present)
if (length(.req_missing))
  warning("[env] THE LOCKFILE WILL BE INCOMPLETE. These packages are required ",
          "by run_everything.R but are not installed here, so renv cannot ",
          "record them: ", paste(.req_missing, collapse = ", "),
          ". Install them and re-run this script before committing renv.lock.",
          call. = FALSE, immediate. = TRUE)

si <- capture.output(sessionInfo())
writeLines(c(paste("# Recorded", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
             "# by scripts/00_snapshot_env.R after a pipeline run.", "", si),
           file.path(repo, "sessionInfo.txt"))
message("[env] wrote ", file.path(repo, "sessionInfo.txt"))

if (requireNamespace("renv", quietly = TRUE)) {
  message("[env] renv found; writing renv.lock for the packages above ...")
  try({
    # No `type =` argument. renv answers a call that passes `packages` with
    # "packages argument is set; type argument \"explicit\" will be ignored",
    # so naming a type here only makes the call look like it controls something
    # it does not. The package set IS the `packages` argument, and what actually
    # landed in the file is verified by reading it back below.
    renv::snapshot(project = repo, packages = present, prompt = FALSE)
    message("[env] wrote ", file.path(repo, "renv.lock"))
  }, silent = FALSE)

  # VERIFY THE FILE, do not assume the call did what was intended. Exactly how
  # renv resolves `packages=` together with `type=` is a detail of the installed
  # renv version, and the thing that matters -- does the lockfile contain every
  # package the preflight demands -- is cheap to read back and expensive to get
  # wrong: the README instructs a reader to reconstruct the analysis environment
  # with renv::restore(), and a lockfile missing a required package sends them
  # into a preflight failure on their first command. That is how the previous
  # lockfile came to omit readstata13, WeMix, broom.mixed, cowplot, naniar,
  # terra, exactextractr and ncdf4 without anyone noticing.
  .lock <- file.path(repo, "renv.lock")
  if (file.exists(.lock) && requireNamespace("jsonlite", quietly = TRUE)) {
    .have <- tryCatch(names(jsonlite::fromJSON(.lock)$Packages),
                      error = function(e) character(0))
    .gap  <- setdiff(REQUIRED_PKGS, .have)
    if (length(.gap))
      warning("[env] renv.lock is INCOMPLETE. It has ", length(.have),
              " packages but does not record: ", paste(.gap, collapse = ", "),
              ". renv::restore() from this lockfile will NOT satisfy ",
              "run_everything.R's preflight. Do not tag a release on it.",
              call. = FALSE, immediate. = TRUE)
    else
      message("[env] renv.lock verified: all ", length(REQUIRED_PKGS),
              " preflight packages recorded (", length(.have), " total).")
  } else if (!file.exists(.lock)) {
    warning("[env] renv::snapshot() did not produce ", .lock,
            call. = FALSE, immediate. = TRUE)
  }
} else {
  message("[env] renv is not installed, so no renv.lock was written. ",
          "install.packages('renv') and re-run this script to produce one; ",
          "sessionInfo.txt above records the versions either way.")
}
