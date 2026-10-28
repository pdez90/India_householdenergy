# ==============================================================================
# 34_crosswalk_variants_summary.R  (standalone; seconds)
#
# Collects the headline quantities of the crosswalk runs into one table for the
# manuscript (SI Methods S13): the original name/neighbour rule (version 1), the
# primary version-2 crosswalk, and its three sensitivity variants. Each run is a
# complete pipeline run in its own output folder.
#
#   Rscript 34_crosswalk_variants_summary.R
#     V1_DIR      default <ACCESS_DIR>/outputs_crosswalk_v1
#     VARIANT_DIRS default <ACCESS_DIR>/outputs_xw_{alternative_assignment,
#                  exclude_uncertain,alternative_history}
#
# Outputs (in OUTPUT_DIR, the primary run):
#   crosswalk_variants_headline.csv   run x quantity: estimate, lower, upper
#   crosswalk_history_table_summary.csv  documented-history rows by type and
#                                         confidence (data/district_history_nfhs5.csv)
#   crosswalk_method_summary.csv      assignment method x low-confidence history:
#                                     clusters and agreement with the 2011 overlay
# Run from scripts/ (it reads ../data/district_history_nfhs5.csv).
# ==============================================================================
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()
root <- dirname(dir_out)
runs <- c(version_1 = Sys.getenv("V1_DIR", file.path(root, "outputs_crosswalk_v1")),
          primary = dir_out)
vd <- strsplit(Sys.getenv("VARIANT_DIRS", ""), ",")[[1]]
if (!length(vd)) vd <- file.path(root, paste0("outputs_xw_", c("alternative_assignment", "exclude_uncertain", "alternative_history")))
names(vd) <- sub("^outputs_xw_", "", basename(vd))
runs <- c(runs, vd)
miss <- runs[!dir.exists(runs)]
if (length(miss)) stop("run folders not found: ", paste(miss, collapse = ", "))

rd <- function(d, f) utils::read.csv(file.path(d, f), stringsAsFactors = FALSE, check.names = FALSE)
one <- function(run, d) {
  ct <- rd(d, "comparison_table.csv")
  gap <- function(lab) 100 * ct$mean_diff[ct$comparison == lab]
  cp <- rd(d, "calibration_parameters.csv"); n5 <- cp[startsWith(cp$calibration, "NFHS-5"), ]
  par <- function(p, col = "estimate") n5[[col]][n5$parameter == p]
  lo <- rd(d, "calibration_loso_summary.csv")
  vdc <- rd(d, "design_variance_decomposition.csv")
  he <- rd(d, "health_effects_table.csv")
  hr <- function(term, oc) {
    z <- he[he$outcome == oc & he$term == term & as.character(he$adjusted) %in% c("TRUE", "True") &
              he$weighting == "birth_hmean" & he$support_stratum == "all", ]
    if (nrow(z) != 1) stop(run, ": expected one health row for ", oc, " ", term, ", found ", nrow(z))
    c(z$est_per10, 10 * z$ci_cr2_low, 10 * z$ci_cr2_high, z$p_cr2)
  }
  q <- list(
    gap_rural            = c(gap("NFHS-5 (rural) vs IRES (rural)"), NA, NA, NA),
    gap_rural_designwt   = c(gap("NFHS-5 (rural, design-wt) vs IRES (rural, design-wt)"), NA, NA, NA),
    gap_all_households   = c(gap("NFHS-5 (all) vs IRES (all)"), NA, NA, NA),
    calibration_slope_2019 = c(par("slope"), par("slope", "lo95"), par("slope", "hi95"), NA),
    residual_sd_rms_2019 = c(par("sd_residual_rms"), NA, NA, NA),
    loso_states_improved = c(lo$n_improved, lo$n_states_tested, NA, NA),
    nfhs_side_share_2019 = c(vdc$pct_nfhs_side[grepl("^2019", vdc$leg)], NA, NA, NA),
    infant_fixed_surface   = hr("change_lpg_bayes", "infant"),
    neonatal_fixed_surface = hr("change_lpg_bayes", "neonatal"),
    infant_exposure_only_mi = hr("change_lpg_bayes_MI", "infant"),
    infant_single_instrument = hr("change_lpg_bayes_iresonly", "infant"))
  do.call(rbind, lapply(names(q), function(n) data.frame(run = run, quantity = n, estimate = q[[n]][1],
                                                         lower = q[[n]][2], upper = q[[n]][3], p_cr2 = q[[n]][4])))
}
out <- do.call(rbind, Map(one, names(runs), runs))
# clusters assigned differently from the primary run, per variant
au <- function(d) { f <- file.path(d, "nfhs5_crosswalk_audit.csv"); if (file.exists(f)) rd(d, "nfhs5_crosswalk_audit.csv") else NULL }
ap <- au(dir_out)
for (r in names(vd)) {
  a <- au(vd[[r]]); if (is.null(a) || is.null(ap)) next
  m <- merge(ap[, c("cluster", "final_district")], a[, c("cluster", "final_district")], by = "cluster", all = TRUE, suffixes = c("_p", "_v"))
  out <- rbind(out, data.frame(run = r, quantity = c("clusters_reassigned_vs_primary", "clusters_excluded"),
                               estimate = c(sum(!is.na(m$final_district_v) & !is.na(m$final_district_p) & m$final_district_v != m$final_district_p),
                                            sum(is.na(m$final_district_v))), lower = NA, upper = NA, p_cr2 = NA))
}
utils::write.csv(out, file.path(dir_out, "crosswalk_variants_headline.csv"), row.names = FALSE)

# How each assignment method relates to the 2011 boundary overlay (primary run):
# clusters, clusters with GPS, clusters whose point lies in a different 2011
# polygon from their final district, the most such clusters in any one district,
# and clusters flagged uncertain.
lg <- function(x) as.logical(toupper(as.character(x)))
if (!is.null(ap)) {
  for (cn in c("has_gps", "overlay_disagrees_with_final", "uncertain", "history_low_confidence")) ap[[cn]] <- lg(ap[[cn]])
  ap$overlay_differs <- ap$has_gps %in% TRUE & ap$overlay_disagrees_with_final %in% TRUE
  ms <- do.call(rbind, lapply(split(ap, list(ap$method, ap$history_low_confidence), drop = TRUE), function(z)
    data.frame(method = z$method[1], low_confidence_history = z$history_low_confidence[1], clusters = nrow(z),
               districts = length(unique(paste(z$nfhs5_state, z$nfhs5_district))),
               with_gps = sum(z$has_gps %in% TRUE), overlay_differs = sum(z$overlay_differs),
               max_overlay_differs_one_district = max(tapply(z$overlay_differs, paste(z$nfhs5_state, z$nfhs5_district), sum)),
               uncertain = sum(z$uncertain %in% TRUE))))
  utils::write.csv(ms[order(ms$method, ms$low_confidence_history), ], file.path(dir_out, "crosswalk_method_summary.csv"), row.names = FALSE)
}

hist_path <- normalizePath(file.path(getwd(), "..", "data", "district_history_nfhs5.csv"), mustWork = TRUE)
h <- utils::read.csv(hist_path, stringsAsFactors = FALSE)
hs <- as.data.frame(table(type = h$type, confidence = h$confidence), stringsAsFactors = FALSE)
has_alt <- !is.na(h$alt_type) & h$alt_type != ""
# Verifying source of each entry (verified_source_url): a creating instrument
# (gazette notification, Act, or an RBI lead-bank circular quoting the gazette),
# a state or district government website, or anything else (press).
src <- if ("verified_source_url" %in% names(h)) tolower(h$verified_source_url) else rep("", nrow(h))
src_cat <- ifelse(grepl("rbi\\.org\\.in|megpns\\.gov\\.in|indiacode|prsindia", src), "source: gazette, Act or RBI circular",
           ifelse(grepl("\\.gov\\.in|\\.nic\\.in|negd", src), "source: government website",
           ifelse(nzchar(src), "source: other (press)", "source: none recorded")))
hs <- rbind(hs, data.frame(type = "all", confidence = "all", Freq = nrow(h)),
            data.frame(type = "with documented alternative", confidence = "low",
                       Freq = sum(has_alt & tolower(h$confidence) == "low")),
            as.data.frame(table(type = src_cat, confidence = h$confidence), stringsAsFactors = FALSE))
# What the cited source establishes for each entry, as against what the linkage
# assumes (column `basis`): states the relationship; names sub-units, parent
# inferred; documents a reorganisation only, candidates widened (analytic
# choice); no record of change found (absence of evidence).
if ("basis" %in% names(h)) {
  bcat <- ifelse(grepl("^source states", h$basis), "basis: source states the relationship",
          ifelse(grepl("^source names the sub-units", h$basis), "basis: parent inferred from named sub-units",
          ifelse(grepl("^source documents a reorganisation", h$basis), "basis: reorganisation documented, candidates widened",
          ifelse(grepl("^no record", h$basis), "basis: no record of change (absence of evidence)", "basis: other"))))
  hs <- rbind(hs, as.data.frame(table(type = bcat, confidence = h$confidence), stringsAsFactors = FALSE))
}
hs <- hs[hs$Freq > 0, ]
names(hs)[3] <- "districts"
utils::write.csv(hs, file.path(dir_out, "crosswalk_history_table_summary.csv"), row.names = FALSE)
w <- reshape(out[out$quantity %in% unique(out$quantity), c("run", "quantity", "estimate")],
             idvar = "quantity", timevar = "run", direction = "wide")
print(w, row.names = FALSE, digits = 4)
