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
