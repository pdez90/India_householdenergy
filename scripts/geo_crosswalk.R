# ==============================================================================
# geo_crosswalk.R -- assign every NFHS-4 and NFHS-5 cluster to an NFHS-4
# (2011 Census) district, WITHOUT the district shapefile's polygons.
#
# Why not polygons. Checked against DHS's own district code (DHSREGCO) for every
# NFHS-4 cluster, point-in-polygon on the pipeline's district shapefile puts only
# about 72% of clusters in their own district and about 13% in the wrong STATE
# (e.g. Delhi clusters in Ghaziabad, Chennai in Kancheepuram, the Imphal valley
# in Chandel). For NFHS-5 the polygon district agrees with the cluster's own
# district name only about 74% of the time. The shapefile is used for drawing
# maps only.
#
# Rule.
#   NFHS-4: the cluster's own DHSREGCO, which IS the NFHS-4 district code.
#   NFHS-5: by district, using NFHS-5's own district (DHSREGCO/DHSREGNA, 2019
#     frame, 707 districts):
#     (1) name match -- an NFHS-5 district whose (normalised) name is the name of
#         an NFHS-4 district in the same 2011 state maps to it (a name shared by
#         several districts of that state would be resolved by location);
#     (2) location -- otherwise (districts created after 2011), each cluster votes
#         with the NFHS-4 district labels of its k nearest NFHS-4 clusters; if one
#         NFHS-4 district gets at least VOTE_MIN of the district's votes, the
#         whole NFHS-5 district goes to it (its 2011 parent), else each cluster
#         goes to its own nearest-neighbour majority.
#     A name match is overridden only if the location vote points strongly
#     elsewhere (>= 0.8 of votes), which would mean a coincidental same name.
#   Clusters released without coordinates (lat = lon = 0) are assigned through
#   their district (steps 1-2) like any other cluster.
#
# Validation (leave-one-out on NFHS-4, where truth is known): the k = 7 nearest
# NFHS-4 clusters' majority label equals a cluster's own district 90% of the
# time even cluster by cluster, against 72% for the polygons; whole-district
# votes, as used here, are more accurate still. The script reports the same
# check on NFHS-5 name-matched districts.
# ==============================================================================

geo_norm <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  gsub("[^a-z]", "", tolower(x))
}

# k nearest NFHS-4 clusters for each query point, in base R (no extra package):
# equirectangular km coordinates, processed in chunks.
geo_knn_vote <- function(qlon, qlat, rlon, rlat, rlab, k = 7, chunk = 400) {
  cosl <- cos(22 * pi / 180)
  rx <- rlon * cosl * 111; ry <- rlat * 111
  qx <- qlon * cosl * 111; qy <- qlat * 111
  out <- vector("list", length(qx))
  for (s in seq(1, length(qx), by = chunk)) {
    i <- s:min(length(qx), s + chunk - 1)
    d2 <- outer(qx[i], rx, "-")^2 + outer(qy[i], ry, "-")^2
    for (r in seq_along(i)) {
      nn <- order(d2[r, ])[seq_len(k)]
      out[[i[r]]] <- rlab[nn]
    }
  }
  out   # list of length(qx), each a vector of k NFHS-4 district codes
}

# NFHS-5 state -> the NFHS-4 (2011) state(s) its territory belonged to.
geo_state_map <- function(s5) {
  s5 <- geo_norm(s5)      # e.g. "Jammu & Kashmir" -> "jammukashmir"
  lapply(s5, function(z) {
    if (z == "telangana") return(c("telangana", "andhrapradesh"))
    if (z == "ladakh")    return(c("ladakh", "jammukashmir", "jammuandkashmir"))
    if (grepl("dadra", z) || grepl("^daman", z))
      return(c(z, "dadranagarhaveli", "dadraandnagarhaveli", "damandiu", "damananddiu"))
    z
  })
}

# names4: optional data.frame(code, name) giving the 2011 district name for each
# NFHS-4 code (from the shapefile's attribute table, which follows the Census
# codes). The DHS GPS file's own names are swapped for two Punjab districts
# (codes 39 and 53: Shahid Bhagat Singh Nagar / Sangrur), while its codes and
# coordinates agree with the Census, so names are taken from the code.
build_nfhs_xwalk <- function(gps15, gps19, k = 7, VOTE_MIN = 0.6, OVERRIDE = 0.8,
                             names4 = NULL) {
  g4 <- data.frame(cluster = as.numeric(gps15$cluster),
                   d4 = as.numeric(gps15$DHSREGCO),
                   name4 = if (is.null(names4)) geo_norm(gps15$DHSREGNA) else
                     geo_norm(names4$name[match(as.numeric(gps15$DHSREGCO), as.numeric(names4$code))]),
                   st4 = geo_norm(gps15$ADM1NAME),
                   lon = as.numeric(gps15$LONGNUM), lat = as.numeric(gps15$LATNUM))
  g5 <- data.frame(cluster = as.numeric(gps19$cluster),
                   d5 = as.numeric(gps19$DHSREGCO),
                   name5 = geo_norm(gps19$DHSREGNA),
                   lon = as.numeric(gps19$LONGNUM), lat = as.numeric(gps19$LATNUM))
  allowed_st <- geo_state_map(gps19$ADM1NAME)
  has4 <- is.finite(g4$lat) & is.finite(g4$lon) & g4$lat != 0 & g4$lon != 0
  has5 <- is.finite(g5$lat) & is.finite(g5$lon) & g5$lat != 0 & g5$lon != 0

  xw4 <- data.frame(survey = "NFHS4", cluster = g4$cluster, district = g4$d4,
                    method = "DHS district code (DHSREGCO)", stringsAsFactors = FALSE)

  # location votes for every NFHS-5 cluster with coordinates.
  # Votes come from the k nearest NFHS-4 clusters IN THE SAME STATE (on the
  # 2011 frame): the reference set is restricted to the allowed 2011 state(s)
  # BEFORE the neighbour search, so a district near a state border cannot be
  # assigned across it. If an NFHS-5 state had no NFHS-4 reference cluster
  # under any of its allowed names (a naming mismatch), the search falls back to
  # all NFHS-4 clusters; such clusters are counted in the diagnostics (expected 0)
  # and 01_prep_nfhs.R fails the run if any occur.
  votes <- vector("list", nrow(g5))
  n_fallback <- 0L
  if (any(has5)) {
    key <- vapply(allowed_st, function(a) paste(sort(a), collapse = "|"), character(1))
    for (kk in unique(key[has5])) {
      q   <- which(has5 & key == kk)
      ref <- which(has4 & g4$st4 %in% allowed_st[[q[1]]])
      if (!length(ref)) { ref <- which(has4); n_fallback <- n_fallback + length(q) }
      vv <- geo_knn_vote(g5$lon[q], g5$lat[q], g4$lon[ref], g4$lat[ref], g4$d4[ref],
                         k = min(k, length(ref)))
      votes[q] <- vv
    }
  }

  name_tab <- unique(g4[, c("d4", "name4", "st4")])
  dist5 <- unique(g5[, c("d5", "name5")])
  res <- vector("list", nrow(dist5))
  for (r in seq_len(nrow(dist5))) {
    d5 <- dist5$d5[r]; rows <- which(g5$d5 == d5)
    v  <- unlist(votes[rows]); tv <- sort(table(v), decreasing = TRUE)
    top <- if (length(tv)) as.numeric(names(tv)[1]) else NA_real_
    top_share <- if (length(tv)) as.numeric(tv[1]) / sum(tv) else NA_real_
    # name candidates only among 2011 districts of the same (2011) state
    cand <- unique(name_tab$d4[name_tab$name4 == dist5$name5[r] &
                               name_tab$st4 %in% allowed_st[[rows[1]]]])
    if (length(cand) > 1) {
      cv <- table(factor(v, levels = cand))
      cand <- if (sum(cv) > 0) as.numeric(names(cv)[which.max(cv)]) else cand[1]
      meth0 <- "name match (same name in several states; resolved by location)"
    } else meth0 <- "name match"
    if (length(cand) == 1 && !(is.finite(top_share) && top_share >= OVERRIDE && top != cand)) {
      assign <- rep(cand, length(rows)); meth <- meth0
    } else if (length(cand) == 1) {
      assign <- rep(top, length(rows))
      meth <- "location (name match overridden: location vote points elsewhere)"
    } else if (is.finite(top_share) && top_share >= VOTE_MIN) {
      assign <- rep(top, length(rows)); meth <- "location: whole district to its 2011 parent"
    } else {
      # split district: each cluster to its own nearest-neighbour majority;
      # clusters without coordinates to the district-level majority
      assign <- vapply(rows, function(i) {
        vi <- votes[[i]]
        if (length(vi)) as.numeric(names(sort(table(vi), decreasing = TRUE))[1]) else top
      }, numeric(1))
      meth <- "location: cluster by cluster (district spans several 2011 districts)"
    }
    res[[r]] <- data.frame(survey = "NFHS5", cluster = g5$cluster[rows], district = assign,
                           method = meth, d5 = d5, top_vote = top,
                           top_vote_share = top_share,
                           name_cand = if (length(cand) == 1) cand else NA_real_,
                           stringsAsFactors = FALSE)
  }
  xw5 <- do.call(rbind, res)

  # Validation: for every cluster in a district with a single same-state name
  # match, does the location vote agree with the name -- computed BEFORE any
  # override, so the districts whose name match was overridden count as
  # disagreements here (they are also reported separately).
  nm <- is.finite(xw5$name_cand) & !grepl("several states", xw5$method)
  val <- if (any(nm)) mean(xw5$name_cand[nm] == xw5$top_vote[nm], na.rm = TRUE) else NA_real_
  diag <- data.frame(
    metric = c("NFHS-4 clusters", "NFHS-4 clusters with coordinates",
               "NFHS-5 clusters", "NFHS-5 clusters with coordinates",
               "NFHS-5 districts (2019 frame)",
               "NFHS-5 clusters assigned by district name",
               "NFHS-5 clusters assigned by name, resolved by location (duplicate names)",
               "NFHS-5 clusters assigned to the 2011 parent by location",
               "NFHS-5 clusters assigned cluster by cluster by location",
               "NFHS-5 clusters whose name match was overridden by location",
               "NFHS-5 clusters unassigned",
               "NFHS-5 clusters whose location vote had no same-state reference cluster",
               "agreement of location vote with name match (name-matched clusters, before override)"),
    value = c(nrow(g4), sum(has4), nrow(g5), sum(has5), nrow(dist5),
              sum(xw5$method == "name match"),
              sum(grepl("several states", xw5$method)),
              sum(grepl("2011 parent", xw5$method)),
              sum(grepl("cluster by cluster", xw5$method)),
              sum(grepl("overridden", xw5$method)),
              sum(is.na(xw5$district)), n_fallback, round(val, 4)))
  list(xwalk = rbind(xw4, xw5[, names(xw4)]), nfhs5_detail = xw5, diagnostics = diag)
}

# ==============================================================================
# Version 2 (primary from 2026-10-27): documented district history + 2011 Census
# district boundaries (SHRUG 2.2 PC11 district polygons).
#
# The location rule above uses geography only. Validation (30_crosswalk_
# validation.R) showed it assigns some clusters of split districts to 2011
# districts that contributed no territory, and that name matching keeps
# districts whose territory was redrawn (Delhi, 2012). Version 2 lets
# documented administrative history decide what a district may contain, and
# uses the validated 2011 boundaries (NFHS-4 overlay agreement with DHS's own
# district code ~98%) only where several documented parents are possible:
#
#   not listed in history   -> name match (territory not documented as changed)
#   type "unchanged"        -> name match; must equal the documented district
#   type "single_parent"    -> every cluster to that parent, with or without GPS
#   type "multi_parent"     -> the 2011 polygon the cluster falls in, if it is a
#                              documented parent; otherwise (outside the parents,
#                              outside all polygons) the nearest documented
#                              parent, flagged uncertain; clusters without GPS
#                              go to the district's majority parent, flagged
#   any location-assigned district missing from history -> the run stops.
#
# Version 1 is built first and returned alongside, so the two can be compared
# and every change traced in the per-cluster audit.
# ==============================================================================
# Uncertainty is flagged per cluster and carried to named variants, so its effect
# can be tested rather than assumed (each variant is a complete crosswalk; every
# NFHS-5 record of a cluster -- households, women, births -- follows it, with its
# original survey weight):
#   flag "fallback"           GPS point outside the documented parents or outside
#                             all polygons: nearest documented parent;
#                             alternative = the next-nearest documented parent
#   flag "no GPS"             no coordinates in a multi-parent district: the
#                             district's majority parent; alternative = its
#                             next most common parent (or the other parent)
#   flag "near boundary"      inside a documented parent but within DHS's
#                             displacement distance (urban 2 km, rural 5 km) of
#                             another documented parent; alternative = that parent
#   flag "low-confidence history" (district level) every cluster of a district
#                             whose history row has confidence "low"; the
#                             alternative is the row's alt_type/alt_parents_2011,
#                             applied to the whole district at once
# variant = "primary"                 best-supported assignment (default)
#           "alternative_assignment"  flagged clusters (first three flags) moved
#                                     to their alternative
#           "exclude_uncertain"       flagged clusters (first three flags) left
#                                     out of the crosswalk, so every NFHS-5 record
#                                     of those clusters drops out of the analysis
#           "alternative_history"     low-confidence history rows replaced by
#                                     their documented alternative, district-wide
build_nfhs_xwalk_v2 <- function(gps15, gps19, pc11, history, k = 7, VOTE_MIN = 0.6,
                                OVERRIDE = 0.8, names4 = NULL, variant = "primary",
                                near_km = c(U = 2, R = 5)) {
  VARIANTS <- c("primary", "alternative_assignment", "exclude_uncertain", "alternative_history")
  if (!variant %in% VARIANTS) stop("crosswalk variant must be one of: ", paste(VARIANTS, collapse = ", "))
  v1  <- build_nfhs_xwalk(gps15, gps19, k = k, VOTE_MIN = VOTE_MIN, OVERRIDE = OVERRIDE,
                          names4 = names4)
  det <- v1$nfhs5_detail
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  pc11 <- sf::st_make_valid(sf::st_transform(pc11, 4326))
  pid  <- as.numeric(pc11$pc11_d_id)

  overlay_pts <- function(lon, lat) {
    ok <- is.finite(lat) & is.finite(lon) & lat != 0 & lon != 0
    hits <- vector("list", length(lon)); n <- rep(NA_integer_, length(lon))
    if (any(ok)) {
      ij <- sf::st_intersects(sf::st_as_sf(data.frame(lon = lon[ok], lat = lat[ok]),
                                           coords = c("lon", "lat"), crs = 4326), pc11)
      hits[ok] <- lapply(ij, function(x) pid[x]); n[ok] <- lengths(ij)
    }
    list(ok = ok, hits = hits, n = n)
  }
  pt <- function(lon, lat) sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)

  # Validation on NFHS-4, where the true 2011 district is known.
  o4 <- overlay_pts(as.numeric(gps15$LONGNUM), as.numeric(gps15$LATNUM))
  ov4 <- vapply(o4$hits, function(x) if (length(x) == 1) x else NA_real_, numeric(1))
  nfhs4_agree <- mean((ov4 == as.numeric(gps15$DHSREGCO))[o4$ok] %in% TRUE)

  g5 <- data.frame(cluster = as.numeric(gps19$cluster), d5 = as.numeric(gps19$DHSREGCO),
                   name5 = trimws(as.character(gps19$DHSREGNA)),
                   state5 = trimws(as.character(gps19$ADM1NAME)),
                   lon = as.numeric(gps19$LONGNUM), lat = as.numeric(gps19$LATNUM),
                   urban = if ("URBAN_RURA" %in% names(gps19)) toupper(substr(as.character(gps19$URBAN_RURA), 1, 1)) == "U" else NA,
                   stringsAsFactors = FALSE)
  o5 <- overlay_pts(g5$lon, g5$lat)
  g5$has_gps <- o5$ok
  g5$overlay_status <- ifelse(!o5$ok, "no GPS", ifelse(o5$n == 0, "outside all polygons",
                        ifelse(o5$n > 1, "multiple polygons", "one polygon")))
  g5$overlay_district <- vapply(o5$hits, function(x) if (length(x) == 1) x else NA_real_, numeric(1))
  thr_km <- ifelse(g5$urban %in% TRUE, near_km[["U"]], near_km[["R"]])   # unknown -> rural (larger)

  key <- function(s, d) paste(toupper(trimws(s)), toupper(trimws(d)), sep = "|")
  history <- as.data.frame(history, stringsAsFactors = FALSE)
  for (cc in c("alt_type", "alt_parents_2011")) if (!cc %in% names(history)) history[[cc]] <- NA_character_
  history$key <- key(history$state5, history$district5)
  if (anyDuplicated(history$key)) stop("district history: duplicated state/district rows")
  TYPES <- c("unchanged", "single_parent", "multi_parent")
  if (!all(history$type %in% TYPES)) stop("district history: type must be one of ", paste(TYPES, collapse = ", "))
  alt_ok <- is.na(history$alt_type) | history$alt_type %in% c(TYPES, "")
  if (!all(alt_ok)) stop("district history: alt_type must be empty or one of ", paste(TYPES, collapse = ", "))
  history$low_conf <- tolower(history$confidence) %in% "low"
  history$alt_applied <- FALSE
  if (variant == "alternative_history") {
    sw <- history$low_conf & !is.na(history$alt_type) & history$alt_type != ""
    history$type[sw] <- history$alt_type[sw]; history$parents_2011[sw] <- history$alt_parents_2011[sw]
    history$alt_applied[sw] <- TRUE
  }
  hrow <- match(key(g5$state5, g5$name5), history$key)
  unused <- setdiff(history$key, key(g5$state5, g5$name5))
  if (length(unused)) stop("district history rows match no NFHS-5 district: ", paste(unused, collapse = "; "))

  m <- match(g5$cluster, det$cluster)
  g5$v1_district <- det$district[m]; g5$v1_method <- det$method[m]
  g5$history_type <- ifelse(is.na(hrow), "not listed", history$type[hrow])
  g5$history_confidence <- ifelse(is.na(hrow), NA_character_, history$confidence[hrow])
  g5$history_low_confidence <- !is.na(hrow) & history$low_conf[hrow]
  g5$history_alternative_applied <- !is.na(hrow) & history$alt_applied[hrow]
  bad <- is.na(hrow) & !grepl("^name match", g5$v1_method)
  if (any(bad))
    stop("v2 crosswalk: location-assigned NFHS-5 districts without a documented history row: ",
         paste(unique(paste(g5$state5[bad], g5$name5[bad], sep = " / ")), collapse = "; "))
  parents <- lapply(hrow, function(r) if (is.na(r)) numeric(0)
                    else as.numeric(strsplit(as.character(history$parents_2011[r]), ";")[[1]]))

  n5 <- nrow(g5)
  final <- rep(NA_real_, n5); method <- rep(NA_character_, n5); alt <- rep(NA_real_, n5)
  flag <- rep("", n5); km <- rep(NA_real_, n5); km_other <- rep(NA_real_, n5)

  i <- g5$history_type == "not listed"
  final[i] <- g5$v1_district[i]; method[i] <- "name match (territory not documented as changed)"

  i <- g5$history_type == "unchanged"
  if (any(i)) {
    mism <- i & !mapply(function(p, d) d %in% p, parents, g5$v1_district)
    if (any(mism)) stop("documented-unchanged districts whose name match is not the documented district: ",
                        paste(unique(g5$name5[mism]), collapse = "; "))
    final[i] <- g5$v1_district[i]; method[i] <- "name match (documented unchanged)"
  }

  i <- g5$history_type == "single_parent"
  if (any(i)) {
    final[i] <- vapply(parents[i], function(p) p[1], numeric(1))
    method[i] <- "location: single documented parent"
  }

  multi <- which(g5$history_type == "multi_parent" & g5$has_gps)
  for (j in multi) {
    p <- parents[[j]]; h <- intersect(o5$hits[[j]], p)
    pp <- pc11[pid %in% p, ]
    dd <- as.numeric(sf::st_distance(pt(g5$lon[j], g5$lat[j]), pp)) / 1000   # great-circle km
    ord <- order(dd); pcode <- as.numeric(pp$pc11_d_id)
    if (length(h) == 1) {
      final[j] <- h; method[j] <- "location: 2011 boundary within documented parents"
      other <- which(pcode != h)
      if (length(other)) {
        o <- other[which.min(dd[other])]; km_other[j] <- dd[o]
        if (dd[o] <= thr_km[j]) { flag[j] <- "near boundary"; alt[j] <- pcode[o] }
      }
    } else {
      final[j] <- pcode[ord[1]]; km[j] <- dd[ord[1]]
      method[j] <- paste0("location: fallback, nearest documented parent (",
                          if (length(h) > 1) "inside several parent polygons"
                          else if (g5$overlay_status[j] == "one polygon") "point in a non-parent district"
                          else "outside all 2011 polygons", ")")
      flag[j] <- "fallback"
      if (length(ord) > 1) { alt[j] <- pcode[ord[2]]; km_other[j] <- dd[ord[2]] }
    }
  }
  # clusters without GPS in multi-parent districts: the district's majority parent
  nog <- which(g5$history_type == "multi_parent" & !g5$has_gps)
  for (j in nog) {
    sib <- which(g5$d5 == g5$d5[j] & !is.na(final) & grepl("2011 boundary within", method))
    tv <- if (length(sib)) sort(table(final[sib]), decreasing = TRUE) else integer(0)
    final[j] <- if (length(tv)) as.numeric(names(tv)[1]) else parents[[j]][1]
    alt[j] <- if (length(tv) > 1) as.numeric(names(tv)[2]) else setdiff(parents[[j]], final[j])[1]
    method[j] <- "location: fallback, no GPS (district's majority documented parent)"
    flag[j] <- "no GPS"
  }
  if (any(is.na(final))) stop("v2 crosswalk left ", sum(is.na(final)), " NFHS-5 clusters unassigned")
  uncertain <- flag != ""

  primary <- final
  if (variant == "alternative_assignment") {
    sw <- uncertain & !is.na(alt); final[sw] <- alt[sw]
    method[sw] <- paste0(method[sw], " [variant: alternative parent]")
  }
  excluded <- if (variant == "exclude_uncertain") uncertain else rep(FALSE, n5)

  audit <- data.frame(cluster = g5$cluster, nfhs5_district_code = g5$d5, nfhs5_district = g5$name5,
                      nfhs5_state = g5$state5, urban = g5$urban, has_gps = g5$has_gps,
                      overlay_status = g5$overlay_status, overlay_district = g5$overlay_district,
                      v1_district = g5$v1_district, v1_method = g5$v1_method,
                      history_type = g5$history_type, history_confidence = g5$history_confidence,
                      history_low_confidence = g5$history_low_confidence,
                      history_alternative_applied = g5$history_alternative_applied,
                      primary_district = primary, alternative_district = alt,
                      final_district = ifelse(excluded, NA_real_, final), method = method,
                      uncertain = uncertain, uncertainty_flag = flag, excluded_by_variant = excluded,
                      km_to_assigned_parent = round(km, 2), km_to_other_parent = round(km_other, 2),
                      changed_from_v1 = final != g5$v1_district,
                      overlay_disagrees_with_final = g5$overlay_status == "one polygon" &
                        g5$overlay_district != final, variant = variant, stringsAsFactors = FALSE)

  det5 <- det; mm <- match(det5$cluster, audit$cluster)
  det5$district <- audit$final_district[mm]; det5$method <- audit$method[mm]
  det5$uncertain <- audit$uncertain[mm]
  det5 <- det5[!audit$excluded_by_variant[mm], , drop = FALSE]
  xw4 <- v1$xwalk[v1$xwalk$survey == "NFHS4", ]
  keep <- !excluded
  xw5 <- data.frame(survey = "NFHS5", cluster = audit$cluster[keep], district = audit$final_district[keep],
                    method = audit$method[keep], stringsAsFactors = FALSE)

  tab <- table(audit$method[keep]); ft <- table(factor(flag[uncertain], levels = c("fallback", "no GPS", "near boundary")))
  diag2 <- data.frame(
    metric = c("crosswalk version", "crosswalk variant",
               "NFHS-4 clusters inside the polygon of their own DHS district (2011 boundaries, SHRUG)",
               "NFHS-5 clusters inside exactly one 2011 polygon",
               "NFHS-5 clusters outside all 2011 polygons",
               "NFHS-5 clusters inside more than one 2011 polygon",
               "NFHS-5 clusters without GPS coordinates",
               paste0("v2 method: ", names(tab)),
               "v2: clusters flagged uncertain",
               paste0("v2: uncertainty flag: ", names(ft)),
               "v2: clusters in districts whose history has low confidence",
               "v2: clusters excluded by the variant",
               "v2: clusters assigned differently from version 1",
               "v2: NFHS-5 districts with a documented history row",
               "v2: clusters whose 2011 polygon differs from the final assignment (single-parent and unchanged districts kept by history)",
               "v2: NFHS-5 clusters unassigned"),
    value = c(2, match(variant, VARIANTS), round(nfhs4_agree, 4), sum(g5$overlay_status == "one polygon"),
              sum(g5$overlay_status == "outside all polygons"), sum(g5$overlay_status == "multiple polygons"),
              sum(!g5$has_gps), as.numeric(tab), sum(uncertain), as.numeric(ft),
              sum(g5$history_low_confidence), sum(excluded), sum(final != g5$v1_district & !excluded),
              length(unique(hrow[!is.na(hrow)])), sum(audit$overlay_disagrees_with_final & !excluded, na.rm = TRUE),
              sum(is.na(audit$final_district) & !excluded)))
  list(xwalk = rbind(xw4, xw5), nfhs5_detail = det5, audit = audit,
       diagnostics = rbind(v1$diagnostics, diag2), xwalk_v1 = v1$xwalk)
}
