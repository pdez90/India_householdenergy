# ==============================================================================
# 30_crosswalk_validation.R  (standalone; fast, deterministic)
#
# Validates the NFHS-5 -> 2011-district cluster crosswalk of geo_crosswalk.R
# beyond the 100% linkage rate it reports. Four questions, each answered from
# the same inputs 01_prep_nfhs.R used, with no analysis re-run:
#
#   A. Does the nearest-neighbour classifier recover district identity where it
#      is KNOWN? Leave-one-out on NFHS-4: each NFHS-4 cluster is classified from
#      its k nearest OTHER NFHS-4 clusters in the same state, cluster by cluster
#      and by the whole-district pooled vote the crosswalk actually uses, and the
#      prediction is compared with the cluster's own DHS district code.
#      Reported overall and separately for the 2011 districts that were later
#      split or renamed (the parents of NFHS-5 location-assigned districts),
#      which are the harder cases.
#   B. Is the crosswalk stable to its operational choices? The crosswalk is
#      rebuilt for every combination of k in {5, 7, 9, 11}, VOTE_MIN in
#      {0.5, 0.6, 0.7} and OVERRIDE in {0.7, 0.8, 0.9}; each alternative is
#      compared with the production crosswalk (k = 7, 0.6, 0.8) on clusters and
#      households reassigned, on the rural design-weighted district primary-LPG
#      estimate, and on the NFHS-5 - IRES rural gap that is the paper's first
#      result.
#   C. What exactly happened in the least straightforward assignments: the
#      NFHS-5 districts assigned cluster by cluster (split districts) and those
#      whose name match was overridden; how ties and sub-majority votes were
#      resolved; and how clusters without GPS coordinates were handled there.
#   D. Why point-in-polygon on the available boundary file disagreed with the
#      DHS district code for ~28% of NFHS-4 clusters: coverage, wrong-state
#      placements and the by-state pattern, so the manuscript can describe it as
#      a property of that file rather than of overlays in general.
#
# Inputs : DHS GPS files (path_gps_2015, path_gps_2019), the district shapefile
#          (names by code, as in 01), nfhs_cluster_district_xwalk.csv (01),
#          nfhs4_polygon_check.rds (01), nfhs_hh_covariates.rds (01),
#          ires_districts.rds (03), corrected_nfhs_districts.rds (05; for the
#          baseline-reproduction check only)
# Outputs: crosswalk_loo_validation.csv, crosswalk_loo_by_k.csv,
#          crosswalk_sensitivity.csv, crosswalk_location_districts.csv,
#          crosswalk_vote_resolution.csv, crosswalk_name_matched_districts.csv,
#          crosswalk_name_matched_summary.csv, crosswalk_history_districts.csv,
#          crosswalk_history_summary.csv, crosswalk_polygon_diagnostics.csv,
#          crosswalk_polygon_by_state.csv, crosswalk_polygon_by_district.csv
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_cluster_district_xwalk.csv" = "01_prep_nfhs.R",
              "nfhs4_polygon_check.rds"         = "01_prep_nfhs.R",
              "nfhs_hh_covariates.rds"          = "01_prep_nfhs.R",
              "ires_districts.rds"              = "03_prep_ires.R",
              "corrected_nfhs_districts.rds"    = "05_correction.R"))
source("geo_crosswalk.R")
SCRIPT <- "30"

# ---- inputs, exactly as 01_prep_nfhs.R reads them ------------------------------
.e15 <- new.env(); load(path_gps_2015, envir = .e15)
.e19 <- new.env(); load(path_gps_2019, envir = .e19)
gps15 <- .e15$gps; gps19 <- .e19$gps
districts_2015 <- sf::st_read(path_districts_shp, quiet = TRUE) |>
  dplyr::mutate(dist_code = as.numeric(dist_code)) |>
  (\(d) d[!sf::st_is_empty(d), ])() |> sf::st_make_valid()
names4 <- districts_2015 |> sf::st_drop_geometry() |>
  dplyr::transmute(code = as.numeric(dist_code), name = dist_name, state = state_name)
nm_by_code <- function(code) names4$name[match(code, names4$code)]
st_by_code <- function(code) names4$state[match(code, names4$code)]

K0 <- 7; VM0 <- 0.6; OV0 <- 0.8
base <- build_nfhs_xwalk(gps15, gps19, k = K0, VOTE_MIN = VM0, OVERRIDE = OV0, names4 = names4)
xw5_base <- base$nfhs5_detail
# The version-1 crosswalk is the baseline for parts A-C and E. When the run used
# version 2, 01_prep_nfhs.R saved the version-1 file alongside it.
V1_FILE <- if (file.exists(file.path(dir_out, "nfhs_cluster_district_xwalk_v1.csv")))
  "nfhs_cluster_district_xwalk_v1.csv" else "nfhs_cluster_district_xwalk.csv"
prod <- readr::read_csv(file.path(dir_out, V1_FILE),
                        show_col_types = FALSE) |> dplyr::filter(survey == "NFHS5")
m <- match(prod$cluster, xw5_base$cluster)
same_as_prod <- all(!is.na(m)) && all(prod$district == xw5_base$district[m])
chk(SCRIPT, paste0("rebuilt version-1 crosswalk reproduces ", V1_FILE),
    same_as_prod, sprintf("%d of %d NFHS-5 clusters identical", sum(prod$district == xw5_base$district[m], na.rm = TRUE), nrow(prod)))
if (!same_as_prod) stop("Baseline crosswalk does not reproduce the production file; inputs differ.")

# 2011 districts touched by reorganisation: parents of NFHS-5 districts that
# were assigned by location (created, split or renamed since 2011).
reorg_d4 <- unique(xw5_base$district[grepl("^location", xw5_base$method)])

# ---- A. leave-one-out classification of NFHS-4 clusters ------------------------
g4 <- data.frame(cluster = as.numeric(gps15$cluster), d4 = as.numeric(gps15$DHSREGCO),
                 st4 = geo_norm(gps15$ADM1NAME),
                 lon = as.numeric(gps15$LONGNUM), lat = as.numeric(gps15$LATNUM))
g4 <- g4[is.finite(g4$lat) & is.finite(g4$lon) & g4$lat != 0 & g4$lon != 0, ]

# Majority label with the production tie rule: sort(table()) keeps ties in the
# table's own (character) order of district codes, so an exact tie goes to the
# code that sorts first. Returned alongside: top share and whether a tie occurred.
vote_summary <- function(v) {
  tv <- sort(table(v), decreasing = TRUE)
  top <- as.numeric(names(tv)[1]); share <- as.numeric(tv[1]) / sum(tv)
  tie <- length(tv) > 1 && tv[2] == tv[1]
  c(top = top, share = share, tie = as.numeric(tie))
}

loo_labels <- function(g, k, chunk = 400) {
  # k nearest OTHER clusters in the same state, by equirectangular km distance
  cosl <- cos(22 * pi / 180)
  x <- g$lon * cosl * 111; y <- g$lat * 111
  out <- vector("list", nrow(g))
  for (st in unique(g$st4)) {
    idx <- which(g$st4 == st)
    kk <- min(k, length(idx) - 1)
    if (kk < 1) { for (i in idx) out[[i]] <- numeric(0); next }
    for (s in seq(1, length(idx), by = chunk)) {
      q <- idx[s:min(length(idx), s + chunk - 1)]
      d2 <- outer(x[q], x[idx], "-")^2 + outer(y[q], y[idx], "-")^2
      for (r in seq_along(q)) {
        d2[r, match(q[r], idx)] <- Inf          # exclude the cluster itself
        nn <- idx[order(d2[r, ])[seq_len(kk)]]
        out[[q[r]]] <- g$d4[nn]
      }
    }
  }
  out
}

loo_eval <- function(k) {
  lab <- loo_labels(g4, k)
  vs <- t(vapply(lab, function(v) if (length(v)) vote_summary(v) else c(NA, NA, NA), numeric(3)))
  cl <- data.frame(cluster = g4$cluster, d4 = g4$d4, pred = vs[, 1], share = vs[, 2],
                   tie = vs[, 3] == 1, reorg = g4$d4 %in% reorg_d4)
  cl$correct <- cl$pred == cl$d4
  # whole-district pooled vote, as the crosswalk uses for a created/renamed district
  pooled <- do.call(rbind, lapply(split(seq_len(nrow(g4)), g4$d4), function(ii) {
    v <- unlist(lab[ii]); s <- vote_summary(v)
    data.frame(d4 = g4$d4[ii[1]], n_clusters = length(ii), top = s[["top"]],
               share = s[["share"]], reorg = g4$d4[ii[1]] %in% reorg_d4)
  }))
  pooled$correct <- pooled$top == pooled$d4
  list(cluster = cl, pooled = pooled)
}

loo_rows <- list(); loo_k <- list()
for (k in c(5, 7, 9, 11)) {
  ev <- loo_eval(k); cl <- ev$cluster; po <- ev$pooled
  loo_k[[length(loo_k) + 1]] <- data.frame(
    k = k,
    cluster_accuracy = mean(cl$correct, na.rm = TRUE),
    cluster_accuracy_reorg = mean(cl$correct[cl$reorg], na.rm = TRUE),
    cluster_accuracy_other = mean(cl$correct[!cl$reorg], na.rm = TRUE),
    cluster_share_no_majority = mean(cl$share < 0.5 + 1e-9, na.rm = TRUE),
    cluster_share_tie = mean(cl$tie, na.rm = TRUE),
    district_pooled_accuracy = mean(po$correct),
    district_pooled_accuracy_reorg = mean(po$correct[po$reorg]),
    district_pooled_share_ge_votemin = mean(po$share >= VM0),
    district_pooled_wrong_and_ge_votemin = sum(!po$correct & po$share >= VM0),
    n_clusters = nrow(cl), n_districts = nrow(po), n_districts_reorg = sum(po$reorg))
  if (k == K0) loo_main <- ev
}
loo_k <- do.call(rbind, loo_k)
readr::write_csv(loo_k, file.path(dir_out, "crosswalk_loo_by_k.csv"))

cl <- loo_main$cluster; po <- loo_main$pooled
loo_tab <- data.frame(metric = c(
  "NFHS-4 clusters with coordinates classified",
  "cluster-by-cluster accuracy (k = 7, same state, self excluded)",
  "cluster-by-cluster accuracy, clusters in 2011 districts later split or renamed",
  "cluster-by-cluster accuracy, other clusters",
  "2011 districts later split or renamed (parents of location-assigned NFHS-5 districts)",
  "share of clusters whose top label lacks an absolute majority (< 4 of 7)",
  "share of clusters with an exact tie for the top label",
  "whole-district pooled vote: districts whose top label is their own code",
  "whole-district pooled vote: districts whose top label reaches VOTE_MIN (0.6)",
  "whole-district pooled vote: districts wrongly labelled at or above VOTE_MIN",
  "whole-district pooled vote: accuracy among districts later split or renamed",
  "median pooled top-vote share (correct districts)",
  "median pooled top-vote share (incorrect districts)"),
  value = c(nrow(cl), mean(cl$correct), mean(cl$correct[cl$reorg]), mean(cl$correct[!cl$reorg]),
            length(reorg_d4), mean(cl$share < 0.5 + 1e-9), mean(cl$tie),
            mean(po$correct), mean(po$share >= VM0), sum(!po$correct & po$share >= VM0),
            mean(po$correct[po$reorg]),
            stats::median(po$share[po$correct]), if (any(!po$correct)) stats::median(po$share[!po$correct]) else NA))
readr::write_csv(loo_tab, file.path(dir_out, "crosswalk_loo_validation.csv"))
cat("\n== A. Leave-one-out classification of NFHS-4 clusters ==\n")
print(loo_tab, row.names = FALSE, digits = 4)
chk(SCRIPT, "LOO: whole-district pooled vote recovers the known 2011 district",
    mean(po$correct) >= 0.95, sprintf("%.1f%% of %d districts", 100 * mean(po$correct), nrow(po)))
chk(SCRIPT, "LOO: cluster-by-cluster accuracy exceeds the 72% polygon overlay",
    mean(cl$correct) > 0.72, sprintf("%.1f%%", 100 * mean(cl$correct)))

# ---- B. sensitivity to k and thresholds ---------------------------------------
hh <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
hh5 <- hh[hh$survey == "NFHS5", ]
hh5$cluster <- as.numeric(sub("^([0-9]+).*$", "\\1", hh5$clust))
if (any(!is.finite(hh5$cluster)) || !all(hh5$cluster %in% xw5_base$cluster))
  stop("could not recover hv001 from clust for every NFHS-5 household")
hh5 <- hh5[as.logical(hh5$rural) %in% TRUE & is.finite(as.numeric(hh5$lpg)) & is.finite(as.numeric(hh5$wt)), ]
hh5$lpg <- as.numeric(hh5$lpg); hh5$wt <- as.numeric(hh5$wt)

dist_est <- function(xw5) {
  d <- xw5$district[match(hh5$cluster, xw5$cluster)]
  ok <- is.finite(d)
  num <- tapply(hh5$wt[ok] * hh5$lpg[ok], d[ok], sum)
  den <- tapply(hh5$wt[ok], d[ok], sum)
  data.frame(district = as.character(names(num)), est = as.numeric(num / den))
}
ires <- readRDS(file.path(dir_out, "ires_districts.rds"))
ires$district <- as.character(ires$district)
gap_vs_ires <- function(est) {
  r <- ires$ires_mainlpg_rural_wt[match(est$district, ires$district)]
  ok <- is.finite(r) & is.finite(est$est)
  c(gap = mean(est$est[ok] - r[ok]), n = sum(ok))
}
est0 <- dist_est(xw5_base)
# The production crosswalk (version 2 when that was run) must reproduce the
# pipeline's own rural design-weighted estimate; version 1 is the baseline here.
xw_prod <- readr::read_csv(file.path(dir_out, "nfhs_cluster_district_xwalk.csv"),
                           show_col_types = FALSE) |> dplyr::filter(survey == "NFHS5")
est_p <- dist_est(xw_prod)
corr5 <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
ref <- as.numeric(corr5$lpg_2019_rural_wt[match(est_p$district, as.character(corr5$district))])
r_base <- stats::cor(est_p$est, ref, use = "complete.obs")
chk(SCRIPT, "production crosswalk reproduces the pipeline's rural design-weighted estimate lpg_2019_rural_wt (05)",
    is.finite(r_base) && r_base > 0.999, sprintf("r = %.4f over %d districts", r_base, sum(is.finite(ref))))
g0 <- gap_vs_ires(est0)
cat(sprintf("\nBaseline NFHS-5 - IRES rural design-weighted gap: %.1f points over %d districts\n", 100 * g0[["gap"]], g0[["n"]]))

hh_per_cluster <- table(hh5$cluster)
grid <- expand.grid(k = c(5, 7, 9, 11), VOTE_MIN = c(0.5, 0.6, 0.7), OVERRIDE = c(0.7, 0.8, 0.9))
sens <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  k <- grid$k[i]; vm <- grid$VOTE_MIN[i]; ov <- grid$OVERRIDE[i]
  xw <- if (k == K0 && vm == VM0 && ov == OV0) xw5_base else
    build_nfhs_xwalk(gps15, gps19, k = k, VOTE_MIN = vm, OVERRIDE = ov, names4 = names4)$nfhs5_detail
  d_alt <- xw$district[match(xw5_base$cluster, xw$cluster)]
  changed <- d_alt != xw5_base$district
  est <- dist_est(xw)
  e0 <- est0$est[match(est$district, est0$district)]
  both <- is.finite(e0) & is.finite(est$est)
  dd <- 100 * (est$est[both] - e0[both])
  g <- gap_vs_ires(est)
  data.frame(k = k, VOTE_MIN = vm, OVERRIDE = ov,
             clusters_reassigned = sum(changed),
             households_reassigned = sum(as.numeric(hh_per_cluster[as.character(xw5_base$cluster[changed])]), na.rm = TRUE),
             n_overrides = sum(grepl("overridden", xw$method)),
             n_cluster_by_cluster = sum(grepl("cluster by cluster", xw$method)),
             n_whole_district_location = sum(grepl("2011 parent", xw$method)),
             districts_with_estimate = nrow(est),
             districts_estimate_changed_gt1pp = sum(abs(dd) > 1),
             max_abs_change_pp = if (length(dd)) max(abs(dd)) else 0,
             mean_abs_change_pp = if (length(dd)) mean(abs(dd)) else 0,
             correlation_with_baseline = if (sum(both) > 2) stats::cor(est$est[both], e0[both]) else NA,
             gap_vs_ires_pp = 100 * g[["gap"]], gap_n_districts = g[["n"]],
             gap_change_pp = 100 * (g[["gap"]] - g0[["gap"]]))
}))
# Version 2 against the version-1 baseline, on the same metrics.
if (V1_FILE != "nfhs_cluster_district_xwalk.csv") {
  d2 <- xw_prod$district[match(xw5_base$cluster, xw_prod$cluster)]
  ch2 <- d2 != xw5_base$district
  e0b <- est0$est[match(est_p$district, est0$district)]
  b2 <- is.finite(e0b) & is.finite(est_p$est); dd2 <- 100 * (est_p$est[b2] - e0b[b2]); g2 <- gap_vs_ires(est_p)
  sens <- rbind(sens, data.frame(k = NA, VOTE_MIN = NA, OVERRIDE = NA,
    clusters_reassigned = sum(ch2),
    households_reassigned = sum(as.numeric(hh_per_cluster[as.character(xw5_base$cluster[ch2])]), na.rm = TRUE),
    n_overrides = NA, n_cluster_by_cluster = NA, n_whole_district_location = NA,
    districts_with_estimate = nrow(est_p), districts_estimate_changed_gt1pp = sum(abs(dd2) > 1),
    max_abs_change_pp = if (length(dd2)) max(abs(dd2)) else 0,
    mean_abs_change_pp = if (length(dd2)) mean(abs(dd2)) else 0,
    correlation_with_baseline = stats::cor(est_p$est[b2], e0b[b2]),
    gap_vs_ires_pp = 100 * g2[["gap"]], gap_n_districts = g2[["n"]],
    gap_change_pp = 100 * (g2[["gap"]] - g0[["gap"]])))
  sens$variant <- c(rep("version 1 parameter grid", nrow(sens) - 1), "version 2 (history + 2011 boundaries)")
} else sens$variant <- "version 1 parameter grid"
# households_reassigned counts rural households with a valid LPG answer only
names(sens)[names(sens) == "households_reassigned"] <- "rural_households_reassigned"
readr::write_csv(sens, file.path(dir_out, "crosswalk_sensitivity.csv"))
cat("\n== B. Sensitivity of the crosswalk to k, VOTE_MIN and OVERRIDE ==\n")
print(sens[, c("variant", "k", "VOTE_MIN", "OVERRIDE", "clusters_reassigned", "rural_households_reassigned",
               "districts_estimate_changed_gt1pp", "max_abs_change_pp", "gap_vs_ires_pp")],
      row.names = FALSE, digits = 3)
chk(SCRIPT, "sensitivity: NFHS-5 - IRES rural gap moves by under 1 point across the grid",
    all(abs(sens$gap_change_pp[sens$variant == "version 1 parameter grid"]) < 1),
    sprintf("max |change| = %.2f points", max(abs(sens$gap_change_pp[sens$variant == "version 1 parameter grid"]))))
chk_warn(SCRIPT, "sensitivity: district estimates correlate > 0.99 with baseline under every alternative",
    all(sens$correlation_with_baseline[sens$variant == "version 1 parameter grid"] > 0.99, na.rm = TRUE),
    sprintf("min r = %.4f", min(sens$correlation_with_baseline[sens$variant == "version 1 parameter grid"], na.rm = TRUE)))

# ---- C. the location-assigned districts, ties and missing coordinates --------
d5_lab  <- as.character(gps19$DHSREGNA); d5_st <- as.character(gps19$ADM1NAME)
has5 <- is.finite(as.numeric(gps19$LATNUM)) & as.numeric(gps19$LATNUM) != 0 & as.numeric(gps19$LONGNUM) != 0
cl5 <- data.frame(cluster = as.numeric(gps19$cluster), name5 = d5_lab, state5 = d5_st, has_gps = has5)
xw5 <- merge(xw5_base, cl5, by = "cluster", all.x = TRUE)
loc <- xw5[grepl("^location", xw5$method), ]
loc_d <- do.call(rbind, lapply(split(loc, loc$d5), function(z) {
  data.frame(nfhs5_district = z$name5[1], state = z$state5[1], method = z$method[1],
             n_clusters = nrow(z), n_clusters_without_gps = sum(!z$has_gps),
             pooled_top_vote_share = z$top_vote_share[1],
             name_match_candidate = if (is.finite(z$name_cand[1])) nm_by_code(z$name_cand[1]) else "",
             assigned_2011_districts = paste(sort(unique(nm_by_code(z$district))), collapse = "; "),
             n_assigned_2011_districts = length(unique(z$district)),
             stringsAsFactors = FALSE)
}))
loc_d <- loc_d[order(loc_d$method, loc_d$state, loc_d$nfhs5_district), ]
readr::write_csv(loc_d, file.path(dir_out, "crosswalk_location_districts.csv"))
cat("\n== C. NFHS-5 districts assigned by location ==\n")
print(table(loc_d$method))

# Vote resolution for the cluster-by-cluster assignments: recompute each
# cluster's own k = 7 vote from the same reference set to record the share
# behind its assignment and whether the top label was tied.
split_cl <- xw5[grepl("cluster by cluster", xw5$method), ]
split_votes <- if (nrow(split_cl)) {
  g4ref <- data.frame(d4 = as.numeric(gps15$DHSREGCO), st4 = geo_norm(gps15$ADM1NAME),
                      lon = as.numeric(gps15$LONGNUM), lat = as.numeric(gps15$LATNUM))
  g4ref <- g4ref[is.finite(g4ref$lat) & g4ref$lat != 0 & g4ref$lon != 0, ]
  allowed <- geo_state_map(split_cl$state5)
  q_lon <- as.numeric(gps19$LONGNUM[match(split_cl$cluster, as.numeric(gps19$cluster))])
  q_lat <- as.numeric(gps19$LATNUM[match(split_cl$cluster, as.numeric(gps19$cluster))])
  t(vapply(seq_len(nrow(split_cl)), function(i) {
    if (!split_cl$has_gps[i]) return(c(NA, NA, NA))
    ref <- which(g4ref$st4 %in% allowed[[i]])
    v <- geo_knn_vote(q_lon[i], q_lat[i], g4ref$lon[ref], g4ref$lat[ref], g4ref$d4[ref], k = min(K0, length(ref)))[[1]]
    vote_summary(v)
  }, numeric(3)))
} else matrix(numeric(0), 0, 3)
vr <- data.frame(metric = c(
  "clusters assigned cluster by cluster (split districts)",
  "of which without GPS coordinates (assigned to the district-level top vote)",
  "of which with an absolute majority (>= 4 of 7 neighbours)",
  "of which with a plurality only (3 of 7 or fewer)",
  "of which with an exact tie for the top label (resolved to the lowest district code)",
  "clusters whose name match was overridden by location",
  "overrides: minimum pooled vote share for the alternative district",
  "clusters without GPS coordinates, all NFHS-5",
  "clusters without GPS coordinates in whole-district (parent) assignments",
  "clusters without GPS coordinates in name-matched districts"),
  value = c(nrow(split_cl), sum(!split_cl$has_gps),
            sum(split_votes[, 2] >= 4 / 7 - 1e-9, na.rm = TRUE),
            sum(split_votes[, 2] < 4 / 7 - 1e-9, na.rm = TRUE),
            sum(split_votes[, 3] == 1, na.rm = TRUE),
            sum(grepl("overridden", xw5$method)),
            if (any(grepl("overridden", xw5$method))) min(xw5$top_vote_share[grepl("overridden", xw5$method)]) else NA,
            sum(!xw5$has_gps), sum(!xw5$has_gps & grepl("2011 parent", xw5$method)),
            sum(!xw5$has_gps & grepl("^name match", xw5$method))))
readr::write_csv(vr, file.path(dir_out, "crosswalk_vote_resolution.csv"))
print(vr, row.names = FALSE, digits = 4)

# ---- C2. name-matched districts whose territory may have changed --------------
# A district can keep its 2011 name while its boundaries are redrawn (Delhi's
# 2012 reorganisation is the known case). The pooled 99.5% agreement figure
# hides such districts, so report every name-matched NFHS-5 district by the
# share of its pooled location votes that go to the 2011 district its name
# matched. Votes are recomputed exactly as geo_crosswalk.R draws them (k = 7
# nearest NFHS-4 clusters in the allowed 2011 state(s)).
g4all <- data.frame(d4 = as.numeric(gps15$DHSREGCO), st4 = geo_norm(gps15$ADM1NAME),
                    lon = as.numeric(gps15$LONGNUM), lat = as.numeric(gps15$LATNUM))
g4all <- g4all[is.finite(g4all$lat) & is.finite(g4all$lon) & g4all$lat != 0 & g4all$lon != 0, ]
allowed5 <- geo_state_map(gps19$ADM1NAME)
key5 <- vapply(allowed5, function(a) paste(sort(a), collapse = "|"), character(1))
votes5 <- vector("list", nrow(gps19))
for (kk in unique(key5[has5])) {
  q <- which(has5 & key5 == kk)
  ref <- which(g4all$st4 %in% allowed5[[q[1]]])
  if (!length(ref)) ref <- seq_len(nrow(g4all))
  votes5[q] <- geo_knn_vote(as.numeric(gps19$LONGNUM[q]), as.numeric(gps19$LATNUM[q]),
                            g4all$lon[ref], g4all$lat[ref], g4all$d4[ref], k = min(K0, length(ref)))
}
names(votes5) <- as.character(as.numeric(gps19$cluster))
nmd <- xw5[grepl("^name match", xw5$method), ]
nm_d <- do.call(rbind, lapply(split(nmd, nmd$d5), function(z) {
  v <- unlist(votes5[as.character(z$cluster)])
  data.frame(nfhs5_district = z$name5[1], state = z$state5[1], n_clusters = nrow(z),
             matched_2011_district = nm_by_code(z$district[1]),
             share_votes_to_matched = if (length(v)) mean(v == z$district[1]) else NA_real_,
             top_vote_2011_district = nm_by_code(z$top_vote[1]),
             top_vote_share = z$top_vote_share[1], stringsAsFactors = FALSE)
}))
nm_d <- nm_d[order(nm_d$share_votes_to_matched), ]
readr::write_csv(nm_d, file.path(dir_out, "crosswalk_name_matched_districts.csv"))
cat("\n== C2. Name-matched districts, lowest share of location votes for the matched district ==\n")
print(head(nm_d, 12), row.names = FALSE, digits = 3)
nm_sum <- data.frame(metric = c(
  "name-matched NFHS-5 districts",
  "name-matched districts with >= 80% of pooled votes for the matched district",
  "name-matched districts with 50-80% of pooled votes for the matched district",
  "name-matched districts with < 50% of pooled votes for the matched district",
  "clusters in name-matched districts with < 50% of votes for the matched district"),
  value = c(nrow(nm_d), sum(nm_d$share_votes_to_matched >= 0.8, na.rm = TRUE),
            sum(nm_d$share_votes_to_matched >= 0.5 & nm_d$share_votes_to_matched < 0.8, na.rm = TRUE),
            sum(nm_d$share_votes_to_matched < 0.5, na.rm = TRUE),
            sum(nm_d$n_clusters[nm_d$share_votes_to_matched < 0.5], na.rm = TRUE)))
readr::write_csv(nm_sum, file.path(dir_out, "crosswalk_name_matched_summary.csv"))
print(nm_sum, row.names = FALSE)

# ---- E. history-constrained variant -------------------------------------------
# The location rule uses only geography. Documented district histories add a
# constraint the rule ignores: a new district can only contain territory from
# the 2011 districts it was carved from. This variant re-assigns (i) every
# cluster in the split districts and (ii) every cluster in the three
# name-matched districts whose location vote gives under 50% to the matched
# district, by the k = 7 nearest NFHS-4 clusters drawn ONLY from the documented
# parent districts (ties broken by the single nearest neighbour), and reports
# how far the results move. Production assignments are unchanged.
# Parent districts (2011 codes) and sources:
#   Siang (2015): West Siang, East Siang -- Wikipedia, "Siang district"
#   West Karbi Anglong (2016): Karbi Anglong -- Wikipedia, "West Karbi Anglong district"
#   Gariyaband (2012): Raipur -- Wikipedia, "Gariaband district"
#   Botad (2013): Ahmadabad, Bhavnagar; Mahisagar (2013): Kheda, Panch Mahals;
#   Morbi (2013): Rajkot, Surendranagar -- Wikipedia, "List of districts of Gujarat"
#   Jangoan, Jayashankar Bhupalapally, Vikarabad, Ranga Reddy (2016) -- Reserve
#     Bank of India notification on the reorganised districts of Telangana (Id 2193)
#   New Delhi, Central (2012 redrawing): any 2011 Delhi district (no finer source)
HIST <- list(
  list(name = "Siang",                    state = "ARUNACHAL PRADESH", parents = c(250, 251)),
  list(name = "West Karbi Anglong",       state = "ASSAM",             parents = c(314)),
  list(name = "Gariyaband",               state = "CHHATTISGARH",      parents = c(410)),
  list(name = "Botad",                    state = "GUJARAT",           parents = c(474, 481)),
  list(name = "Mahisagar",                state = "GUJARAT",           parents = c(483, 484)),
  list(name = "Morbi",                    state = "GUJARAT",           parents = c(475, 476)),
  list(name = "Jangoan",                  state = "TELANGANA",         parents = c(539, 540)),
  list(name = "Jayashankar Bhupalapally", state = "TELANGANA",         parents = c(534, 540, 541)),
  list(name = "Vikarabad",                state = "TELANGANA",         parents = c(537, 538)),
  list(name = "Ranga Reddy",              state = "TELANGANA",         parents = c(537, 538)),
  list(name = "New Delhi",                state = "NCT OF DELHI",      parents = c(90:98)),
  list(name = "Central",                  state = "NCT OF DELHI",      parents = c(90:98)))
cosl <- cos(22 * pi / 180)
hist_rows <- list(); xw_hist <- xw5_base
for (h in HIST) {
  z <- which(toupper(xw5$name5) == toupper(h$name) & toupper(xw5$state5) == h$state)
  if (!length(z)) { warning("history variant: district not found: ", h$name); next }
  ref <- which(g4all$d4 %in% h$parents)
  lab <- rep(NA_real_, length(z)); allv <- c()
  for (j in seq_along(z)) {
    if (!xw5$has_gps[z[j]]) next
    i5 <- match(xw5$cluster[z[j]], as.numeric(gps19$cluster))
    dx <- (g4all$lon[ref] - as.numeric(gps19$LONGNUM[i5])) * cosl
    dy <-  g4all$lat[ref] - as.numeric(gps19$LATNUM[i5])
    o <- order(dx^2 + dy^2)[seq_len(min(K0, length(ref)))]
    v <- g4all$d4[ref][o]; allv <- c(allv, v)
    tv <- table(v); top <- as.numeric(names(tv)[tv == max(tv)])
    lab[j] <- if (length(top) == 1) top else v[v %in% top][1]   # tie -> nearest neighbour
  }
  if (any(is.na(lab))) lab[is.na(lab)] <- as.numeric(names(which.max(table(allv))))
  prod_lab <- xw5$district[z]
  hist_rows[[length(hist_rows) + 1]] <- data.frame(
    nfhs5_district = h$name, state = h$state, n_clusters = length(z),
    production_method = xw5$method[z[1]],
    documented_parents = paste(nm_by_code(h$parents), collapse = "; "),
    production_assigned = paste(sort(unique(nm_by_code(prod_lab))), collapse = "; "),
    production_clusters_outside_parents = sum(!prod_lab %in% h$parents),
    variant_assigned = paste(sort(unique(nm_by_code(lab))), collapse = "; "),
    clusters_reassigned_by_variant = sum(lab != prod_lab), stringsAsFactors = FALSE)
  xw_hist$district[match(xw5$cluster[z], xw_hist$cluster)] <- lab
}
hist_d <- if (length(hist_rows)) do.call(rbind, hist_rows) else
  data.frame(nfhs5_district = character(0), state = character(0), n_clusters = integer(0),
             production_method = character(0), documented_parents = character(0),
             production_assigned = character(0), production_clusters_outside_parents = integer(0),
             variant_assigned = character(0), clusters_reassigned_by_variant = integer(0))
readr::write_csv(hist_d, file.path(dir_out, "crosswalk_history_districts.csv"))
est_h <- dist_est(xw_hist)
e0h <- est0$est[match(est_h$district, est0$district)]
okh <- is.finite(e0h) & is.finite(est_h$est); ddh <- 100 * (est_h$est[okh] - e0h[okh])
gh <- gap_vs_ires(est_h)
chg_h <- xw_hist$district != xw5_base$district
hist_sum <- data.frame(metric = c(
  "split-district clusters assigned outside their documented parent districts (production)",
  "split-district clusters (production)",
  "clusters reassigned by the history-constrained variant",
  "rural households in reassigned clusters",
  "districts whose rural design-weighted LPG estimate moves by more than 1 point",
  "largest change in a district estimate (points)",
  "correlation of district estimates with production",
  "NFHS-5 - IRES rural design-weighted gap, production (points)",
  "NFHS-5 - IRES rural design-weighted gap, history-constrained (points)"),
  value = c(sum(hist_d$production_clusters_outside_parents[grepl("cluster by cluster", hist_d$production_method)]),
            sum(hist_d$n_clusters[grepl("cluster by cluster", hist_d$production_method)]),
            sum(chg_h), sum(as.numeric(hh_per_cluster[as.character(xw5_base$cluster[chg_h])]), na.rm = TRUE),
            sum(abs(ddh) > 1), if (length(ddh)) max(abs(ddh)) else 0,
            stats::cor(est_h$est[okh], e0h[okh]), 100 * g0[["gap"]], 100 * gh[["gap"]]))
readr::write_csv(hist_sum, file.path(dir_out, "crosswalk_history_summary.csv"))
cat("\n== E. History-constrained variant ==\n")
print(hist_d[, c("nfhs5_district", "n_clusters", "production_clusters_outside_parents", "clusters_reassigned_by_variant")], row.names = FALSE)
print(hist_sum, row.names = FALSE, digits = 4)
chk(SCRIPT, "history-constrained variant: NFHS-5 - IRES rural gap moves by under 1 point",
    abs(gh[["gap"]] - g0[["gap"]]) < 0.01, sprintf("%.2f -> %.2f points", 100 * g0[["gap"]], 100 * gh[["gap"]]))

# ---- D. the boundary-file overlay --------------------------------------------
pc <- readRDS(file.path(dir_out, "nfhs4_polygon_check.rds"))
pc$dhs_state <- st_by_code(pc$dhs_district); pc$poly_state <- st_by_code(pc$polygon_district)
pc$outside <- is.na(pc$polygon_district)
pc$wrong_state <- !pc$outside & !is.na(pc$poly_state) & pc$poly_state != pc$dhs_state
pc$wrong_district_same_state <- !pc$outside & !pc$agree & !pc$wrong_state
shp_codes <- unique(names4$code); dhs_codes <- unique(as.numeric(gps15$DHSREGCO))
pd <- data.frame(metric = c(
  "NFHS-4 clusters with coordinates", "share inside the polygon of their own DHS district",
  "share outside every polygon", "share inside a polygon of another state",
  "share inside another district's polygon in the same state",
  "boundary-file polygons (non-empty)", "distinct district codes in the boundary file",
  "DHS NFHS-4 district codes without a polygon", "boundary-file CRS",
  "boundary-file attribute fields"),
  value = c(nrow(pc), mean(pc$agree), mean(pc$outside), mean(pc$wrong_state),
            mean(pc$wrong_district_same_state), nrow(districts_2015), length(shp_codes),
            sum(!dhs_codes %in% shp_codes), sf::st_crs(districts_2015)$input,
            paste(setdiff(names(districts_2015), attr(districts_2015, "sf_column")), collapse = "; ")))
readr::write_csv(pd, file.path(dir_out, "crosswalk_polygon_diagnostics.csv"))
pbs <- do.call(rbind, lapply(split(pc, pc$dhs_state), function(z)
  data.frame(state = z$dhs_state[1], n_clusters = nrow(z), share_agree = mean(z$agree),
             share_outside = mean(z$outside), share_wrong_state = mean(z$wrong_state))))
pbs <- pbs[order(pbs$share_agree), ]
readr::write_csv(pbs, file.path(dir_out, "crosswalk_polygon_by_state.csv"))

# Per district: is the disagreement loose borders, or an identifier that does
# not belong to the geometry it labels? "identity" = under 30% of a district's
# clusters fall in its own polygon and at least 60% fall in one other polygon,
# i.e. the district's values would be drawn on another district's territory in
# any map that joins this file by code. Codes only; no coordinates are written.
inn <- pc[!pc$outside, ]
pbd <- do.call(rbind, lapply(split(inn, inn$dhs_district), function(z) {
  tv <- sort(table(z$polygon_district), decreasing = TRUE)
  modal <- as.numeric(names(tv)[1]); msh <- as.numeric(tv[1]) / nrow(z); own <- mean(z$agree)
  data.frame(district = z$dhs_district[1], district_name = nm_by_code(z$dhs_district[1]),
             state = z$dhs_state[1], n_clusters = nrow(z), share_in_own_polygon = own,
             modal_polygon = modal, modal_polygon_name = nm_by_code(modal),
             modal_polygon_state = st_by_code(modal), modal_share = msh,
             pattern = if (modal == z$dhs_district[1]) "own polygon is modal"
                       else if (own < 0.3 && msh >= 0.6) "identity: clusters sit in another district's polygon"
                       else "mixed or border", stringsAsFactors = FALSE)
}))
readr::write_csv(pbd, file.path(dir_out, "crosswalk_polygon_by_district.csv"))
cat("\nBoundary file, per district pattern:\n"); print(table(pbd$pattern))
idm <- pbd[grepl("^identity", pbd$pattern), ]
cat("identity mismatches by state:\n"); print(sort(table(idm$state), decreasing = TRUE))
chk_warn(SCRIPT, "boundary file: no district's clusters sit wholly in another district's polygon",
         nrow(idm) == 0, sprintf("%d districts (%d clusters) show an identity mismatch; maps that join this file by code misplace them",
                                 nrow(idm), sum(idm$n_clusters)))
cat("\n== D. Boundary-file overlay against the DHS district code ==\n")
print(pd, row.names = FALSE)
print(head(pbs, 8), row.names = FALSE, digits = 3)

for (f in c("crosswalk_loo_validation.csv", "crosswalk_loo_by_k.csv", "crosswalk_sensitivity.csv",
            "crosswalk_location_districts.csv", "crosswalk_vote_resolution.csv",
            "crosswalk_name_matched_districts.csv", "crosswalk_name_matched_summary.csv",
            "crosswalk_history_districts.csv", "crosswalk_history_summary.csv",
            "crosswalk_polygon_diagnostics.csv", "crosswalk_polygon_by_state.csv",
            "crosswalk_polygon_by_district.csv"))
  chk_file(SCRIPT, paste0(f, " written"), f)
rm(.e15, .e19); invisible(gc())
cat("\n30_crosswalk_validation.R done.\n")
