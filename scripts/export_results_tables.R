## export_results_tables.R -- publish the derived result tables behind the
## manuscript into <repository>/results/, so every number in the paper can be
## checked against a file a reader can open.
##
##   cd <repository>/scripts
##   ACCESS_DIR=/path/to/data OUTPUT_DIR="$ACCESS_DIR/outputs" \
##     Rscript export_results_tables.R
##
## WHAT GOES IN, AND WHAT IS KEPT OUT -- by rule, not by hand
##   In:  every CSV at the top level of the output folder and in diagnostics/.
##        These are district-level aggregates and model summaries.
##   Out: nfhs5_fallback_clusters.csv -- DHS cluster IDs with GPS coordinates.
##          DHS GPS data is a restricted dataset that requires separate
##          permission and may not be redistributed, displaced or not.
##        ANY other CSV carrying both a latitude-like and a longitude-like
##          column, so a future location file cannot slip in unnoticed.
##        public/ -- already published in data/ by make_public_district_estimates.R.
##   Local filesystem paths (in the check trail and the release receipt) are
##   replaced with <OUTPUT_DIR>, <REPO> and <HOME>; the script refuses to finish
##   if any survive.
##
## results/ is GENERATED. It is cleared and rewritten on every run, so an
## output that the pipeline stops producing cannot linger as a stale copy.
##
## Like make_public_district_estimates.R this emits no checks and touches no
## diagnostics file, so it cannot blend the canonical run's check trail.
## Base R only.

if (!exists(".env_dir")) source("paths.R")
dir_out <- normalizePath(.dir_out())

repo <- local({
  here <- tryCatch({
    a <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", a[grepl("^--file=", a)])
    if (length(f)) dirname(normalizePath(f[1])) else getwd()
  }, error = function(e) getwd())
  dirname(here)
})
if (!file.exists(file.path(repo, "README.md")))
  stop("expected the repository root at ", repo, " -- run this from scripts/",
       call. = FALSE)
res <- file.path(repo, "results")

## ---- collect -------------------------------------------------------------
top  <- list.files(dir_out, pattern = "\\.csv$", full.names = TRUE)
diag <- list.files(file.path(dir_out, "diagnostics"), pattern = "\\.csv$",
                   full.names = TRUE)
src  <- c(top, diag)
rel  <- sub(paste0("^", gsub("([.\\\\+*?^$(){}|\\[\\]])", "\\\\\\1", dir_out), "/"),
            "", src)

## ---- exclusions ----------------------------------------------------------
DENY <- c("nfhs5_fallback_clusters.csv" =
          "DHS cluster IDs with GPS coordinates: restricted DHS data, not redistributable",
          "nfhs_cluster_district_xwalk.csv" =
          "DHS cluster IDs linked to districts: derived from restricted DHS GPS data")
reason <- rep(NA_character_, length(src))
reason[basename(src) %in% names(DENY)] <- DENY[basename(src)[basename(src) %in% names(DENY)]]
## Outputs of the abandoned outcome-conditioned imputation and of the precision
## frontier built on it: failed convergence, not for inference, never published.
FAILED_RE <- "^(design_(frontier|kappa_star|floor_direct|grid|direct|information_gain|mime_diagnostics)|mime_(diagnostics|validation)|ppd_mime_diagnostics)"
reason[is.na(reason) & grepl(FAILED_RE, basename(src))] <-
  "failed convergence (outcome-conditioned imputation / precision frontier): not for inference"

lat_re <- "^(lat|latitude|latnum|.*_lat|lat_.*|y_coord)$"   # latnum/longnum: the DHS GPS names
lon_re <- "^(lon|long|longitude|longnum|lng|.*_lon|lon_.*|x_coord)$"
for (i in which(is.na(reason))) {
  hdr <- tryCatch(tolower(names(utils::read.csv(src[i], nrows = 1,
                                                check.names = FALSE))),
                  error = function(e) character())
  if (any(grepl(lat_re, hdr)) && any(grepl(lon_re, hdr)))
    reason[i] <- "carries latitude and longitude columns: location data excluded by rule"
  ## DHS cluster or household identifiers mean the file is at sub-district
  ## level, i.e. derived from DHS microdata rather than a district aggregate.
  else if (any(hdr %in% c("hv001", "hv002", "v001", "v002", "caseid", "hhid")))
    reason[i] <- "carries DHS cluster/household identifiers: sub-district data excluded by rule"
}
## Content guard: an effects table whose MI rows are not the approved
## exposure-only imputation, or a comparison table with outcome-conditioned
## rows, is withheld whatever its name.
for (i in which(is.na(reason))) {
  b <- basename(src[i])
  if (grepl("^health_effects_table", b)) {
    h <- tryCatch(utils::read.csv(src[i], stringsAsFactors = FALSE), error = function(e) NULL)
    mi <- if (!is.null(h)) grepl("_MI$", h$term) else FALSE
    if (any(mi) && (!"mi_method" %in% names(h) || !all(h$mi_method[mi] %in% "exposure_only")))
      reason[i] <- "MI rows not the approved exposure-only imputation: not for inference"
  } else if (grepl("^health_mi_comparison", b)) {
    h <- tryCatch(utils::read.csv(src[i], stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(h) && any(grepl("outcome-condition", h$propagation, ignore.case = TRUE)))
      reason[i] <- "contains outcome-conditioned imputation rows: not for inference"
  }
}
keep <- is.na(reason)

## ---- path redaction --------------------------------------------------------
home <- normalizePath("~", mustWork = FALSE)
redact <- function(x) {
  x <- gsub(dir_out, "<OUTPUT_DIR>", x, fixed = TRUE)
  x <- gsub(repo,    "<REPO>",       x, fixed = TRUE)
  x <- gsub(home,    "<HOME>",       x, fixed = TRUE)
  x <- gsub("/Users/[^/\",]+",  "<HOME>", x)
  x <- gsub("/home/[^/\",]+",   "<HOME>", x)
  x <- gsub("/sessions/[^/\",]+", "<HOME>", x)
  x <- gsub("/Volumes/[^/\",]+",  "<VOLUME>", x)
  x <- gsub("/private/[^\",]+",   "<TMP>", x)
  x <- gsub("/mnt/[^/\",]+",      "<MNT>", x)
  x
}

## ---- write ---------------------------------------------------------------
if (dir.exists(res)) unlink(res, recursive = TRUE)
dir.create(res, recursive = TRUE)
man <- data.frame(file = character(), rows = integer(), bytes = numeric(),
                  md5 = character(), paths_redacted = logical(),
                  stringsAsFactors = FALSE)
for (i in which(keep)) {
  dst <- file.path(res, rel[i])
  dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
  txt <- readLines(src[i], warn = FALSE, encoding = "UTF-8")
  red <- redact(txt)
  writeLines(red, dst, useBytes = TRUE)
  man[nrow(man) + 1, ] <- list(rel[i], max(0L, length(red) - 1L),
                               file.size(dst), unname(tools::md5sum(dst)),
                               !identical(txt, red))
}

## Refuse to finish if a path survived. A published file that leaks a home
## directory is the failure this step exists to prevent, so it is fatal here
## rather than a warning to be scrolled past.
leaks <- character()
for (f in file.path(res, man$file)) {
  if (any(grepl("/Users/|/home/|/sessions/|/Volumes/|/private/|/mnt/|[A-Za-z]:\\\\", readLines(f, warn = FALSE))))
    leaks <- c(leaks, f)
}
if (length(leaks)) {
  unlink(res, recursive = TRUE)
  stop("local paths survived redaction in:\n  ", paste(leaks, collapse = "\n  "),
       "\nresults/ removed; nothing was published.", call. = FALSE)
}

utils::write.csv(man, file.path(res, "MANIFEST.csv"), row.names = FALSE)

rid <- NA_character_
trail <- file.path(dir_out, "diagnostics", "pipeline_checks.csv")
if (file.exists(trail)) {
  tr <- tryCatch(utils::read.csv(trail, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(tr) && "run_id" %in% names(tr)) {
    u <- unique(tr$run_id[!is.na(tr$run_id)])
    rid <- if (length(u) == 1L) u else paste0("MIXED(", length(u), ")")
  }
}
ex <- data.frame(file = rel[!keep], reason = reason[!keep], stringsAsFactors = FALSE)
prov <- data.frame(
  quantity = c("analysis_run_id", "generated", "files_published", "files_excluded",
               "generated_by",
               if (nrow(ex)) paste0("excluded: ", ex$file)),
  value    = c(rid, format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
               nrow(man), nrow(ex), "scripts/export_results_tables.R",
               if (nrow(ex)) ex$reason),
  stringsAsFactors = FALSE)
utils::write.csv(prov, file.path(res, "PROVENANCE.csv"), row.names = FALSE)

message(sprintf("published %d tables to %s (analysis run %s)", nrow(man), res, rid))
message(sprintf("  %d with local paths redacted: %s", sum(man$paths_redacted),
                paste(man$file[man$paths_redacted], collapse = ", ")))
message(sprintf("  %d excluded:", nrow(ex)))
for (k in seq_len(nrow(ex))) message("    ", ex$file[k], " -- ", ex$reason[k])
