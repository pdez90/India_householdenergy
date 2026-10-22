# ==============================================================================
# plots.R -- every chart in the app. Palette: the validated reference palette
# (categorical slots in fixed order; one-hue blue sequential; blue/red diverging
# around a grey midpoint). Light theme.
# ==============================================================================

PAL <- list(
  s1 = "#2a78d6",   # slot 1 blue   : NFHS / raw / conventional
  s2 = "#eb6834",   # slot 2 orange : reference survey / corrected / CR2
  s3 = "#1baf7a",   # slot 3 aqua   : third series
  ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781",
  grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", mid = "#f0efec",
  seq = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"),
  red = "#e34948"
)

base_layout <- function(p, ...) {
  plotly::layout(p,
    paper_bgcolor = PAL$surface, plot_bgcolor = PAL$surface,
    font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                size = 13, color = PAL$ink2),
    hoverlabel = list(bgcolor = "white", bordercolor = PAL$axis,
                      font = list(color = PAL$ink, size = 12)),
    margin = list(l = 60, r = 20, t = 30, b = 55), ...) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                              "hoverCompareCartesian", "toggleSpikelines"))
}
ax <- function(title, ...) list(title = list(text = title, font = list(size = 12)),
                                gridcolor = PAL$grid, zerolinecolor = PAL$axis,
                                linecolor = PAL$axis, tickfont = list(color = PAL$muted), ...)

# ---- Step 1: district scatter, NFHS vs reference survey ---------------------
plot_agreement <- function(B, era = "2019") {
  d <- B$dist[, c("district", "dist_name", "state_name")]
  if (era == "2015") {
    x <- B$nfhs_b[, c("district", "lpg_2015")]; names(x)[2] <- "nfhs"
    y <- B$ref_b[, c("district", "lpg_access")]; names(y)[2] <- "ref"
    lab_ref <- "ACCESS Wave 1 (2014-15)"; lab_n <- "NFHS-4 (2015-16)"
  } else {
    x <- B$nfhs_b[, c("district", "lpg_2019")]; names(x)[2] <- "nfhs"
    y <- B$ref_b[, c("district", "lpg_ires")]; names(y)[2] <- "ref"
    lab_ref <- "IRES (2019-20)"; lab_n <- "NFHS-5 (2019-21)"
  }
  z <- merge(merge(x, y, by = "district"), d, by = "district", all.x = TRUE)
  z <- z[is.finite(z$nfhs) & is.finite(z$ref), ]
  z$txt <- sprintf("<b>%s</b>, %s<br>%s: %s%%<br>%s: %s%%<br>Gap: %s pp",
                   trimws(z$dist_name), z$state_name, lab_n, pp(z$nfhs, 0),
                   lab_ref, pp(z$ref, 0), signed(100 * (z$nfhs - z$ref), 0))
  plotly::plot_ly(z, x = ~100 * ref, y = ~100 * nfhs, type = "scatter", mode = "markers",
                  text = ~txt, hoverinfo = "text",
                  marker = list(size = 9, color = PAL$s1, opacity = 0.75,
                                line = list(color = PAL$surface, width = 1.5))) |>
    plotly::add_trace(x = c(0, 100), y = c(0, 100), mode = "lines", inherit = FALSE,
                      type = "scatter", hoverinfo = "skip",
                      line = list(color = PAL$muted, dash = "dash", width = 1.5)) |>
    plotly::add_annotations(x = 92, y = 97, text = "surveys agree", showarrow = FALSE,
                            font = list(color = PAL$muted, size = 11), xanchor = "right") |>
    base_layout(showlegend = FALSE,
      xaxis = ax(paste0("Reference survey: ", lab_ref, " (% of rural households)"), range = c(0, 100)),
      yaxis = ax(paste0("Health survey: ", lab_n, " (%)"), range = c(0, 100)))
}

VAR_LAB <- c(lpg = "Cooks mainly with LPG", sc = "Scheduled Caste", st = "Scheduled Tribe",
             scst = "SC or ST", hindu = "Hindu", muslim = "Muslim",
             electricity = "Has electricity", bpl = "Holds a BPL/Antyodaya ration card")

# ---- Step 1: falsification dot plot ------------------------------------------
plot_falsification <- function(B) {
  s <- B$side
  s$lab  <- VAR_LAB[s$variable]
  s$era  <- ifelse(s$pair == "NFHS4_ACCESS", "2015: NFHS-4 vs ACCESS", "2019: NFHS-5 vs IRES")
  ord <- s[s$pair == "NFHS5_IRES", ]; lv <- ord$lab[order(abs(ord$mean_diff_pp))]
  s$lab <- factor(s$lab, levels = lv)
  s$txt <- sprintf("<b>%s</b><br>%s<br>Average gap: %s pp<br>District correlation r = %s",
                   s$lab, s$era, signed(s$mean_diff_pp), num(s$pearson_r))
  plotly::plot_ly(s, x = ~mean_diff_pp, y = ~lab, color = ~era, colors = c(PAL$s1, PAL$s2),
                  type = "scatter", mode = "markers", text = ~txt, hoverinfo = "text",
                  marker = list(size = 11, line = list(color = PAL$surface, width = 1.5))) |>
    base_layout(
      xaxis = ax("Average district gap, health survey minus reference survey (percentage points)",
                 zeroline = TRUE, zerolinewidth = 1.5),
      yaxis = ax("", tickfont = list(color = PAL$ink2)),
      legend = list(orientation = "h", y = 1.08, x = 0),
      margin = list(l = 230, r = 20, t = 40, b = 55))
}

# ---- Maps -------------------------------------------------------------------
MAP_VARS <- list(
  "Raw NFHS, 2015-16"            = list(col = "raw_wt_2015", type = "level"),
  "Raw NFHS, 2019-21"            = list(col = "raw_wt_2019", type = "level"),
  "Corrected, 2015-16"           = list(col = "bayes_2015",  type = "level"),
  "Corrected, 2019-21"           = list(col = "bayes_2019",  type = "level"),
  "Regression-calibrated, 2015-16" = list(col = "rc_wt_2015", type = "level"),
  "Regression-calibrated, 2019-21" = list(col = "rc_wt_2019", type = "level"),
  "Change 2015 to 2019, raw (multilevel)" = list(col = "change_raw_ml", type = "change"),
  "Change 2015 to 2019, corrected" = list(col = "change_bayes", type = "change"),
  "Uncertainty: width of 95% interval, 2019-21" = list(col = "ciw_2019", type = "width")
)

# The map is drawn as plain SVG (one path per district): no map tiles, no
# WebGL and no external downloads, so it renders anywhere, including offline.
# Paths are projected once (equirectangular, scaled for India's latitude).
map_paths <- function(geo, width = 760) {
  if (is.null(geo)) return(NULL)
  k <- cos(22 * pi / 180)
  rings <- lapply(geo$features, function(f) {
    g <- f$geometry
    polys <- if (g$type == "Polygon") list(g$coordinates) else g$coordinates
    unlist(lapply(polys, function(pl) lapply(pl, function(r)
      matrix(unlist(r), ncol = 2, byrow = TRUE))), recursive = FALSE)
  })
  allxy <- do.call(rbind, unlist(rings, recursive = FALSE))
  xr <- range(allxy[, 1] * k); yr <- range(allxy[, 2])
  sc <- width / diff(xr); height <- diff(yr) * sc
  d <- vapply(rings, function(rs) paste(vapply(rs, function(m) {
    x <- (m[, 1] * k - xr[1]) * sc; y <- (yr[2] - m[, 2]) * sc
    paste0("M", paste(sprintf("%.1f,%.1f", x, y), collapse = "L"), "Z")
  }, character(1)), collapse = ""), character(1))
  list(id = vapply(geo$features, function(f) as.character(f$properties$district), character(1)),
       d = d, width = width, height = height)
}

ramp_hex <- function(cols, x) {
  f <- grDevices::colorRamp(cols, space = "Lab")
  out <- rep("#dcdbd5", length(x)); ok <- is.finite(x)
  if (any(ok)) { m <- f(pmin(pmax(x[ok], 0), 1)); out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255) }
  out
}

svg_map <- function(B, MP, var_label, grey_extrap = FALSE) {
  if (is.null(MP)) return(div(class = "muted", "District boundaries are not available in this bundle."))
  v <- MAP_VARS[[var_label]]
  d <- B$dist
  d$ciw_2019 <- d$bayes_hi_2019 - d$bayes_lo_2019
  val <- d[[v$col]]
  yr  <- if (grepl("2015", v$col)) "2015" else "2019"
  ext <- d[[paste0("extrapolated_", yr)]] %in% TRUE
  if (v$type == "change") ext <- (d$extrapolated_2015 %in% TRUE) | (d$extrapolated_2019 %in% TRUE)
  if (grey_extrap) val[ext] <- NA
  z <- 100 * val
  if (v$type == "change") {
    lim <- max(abs(z), na.rm = TRUE); cols <- c("#b2182b", "#ef8a62", PAL$mid, PAL$seq[3], PAL$seq[6])
    t <- (z + lim) / (2 * lim); lo <- -lim; hi <- lim; unit <- " pp"
  } else if (v$type == "width") {
    hi <- max(z, na.rm = TRUE); lo <- 0; cols <- PAL$seq; t <- z / hi; unit <- " pp"
  } else { lo <- 0; hi <- 100; cols <- PAL$seq; t <- z / 100; unit <- "%" }
  fill <- ramp_hex(cols, t)
  idx <- match(MP$id, as.character(d$district))
  nm  <- ifelse(is.na(idx), "", paste0(trimws(gsub("\\s+", " ", d$dist_name[idx])), ", ", d$state_name[idx]))
  vz  <- z[idx]
  lab <- ifelse(is.finite(vz), paste0(if (v$type == "change") signed(vz, 1) else num(vz, 1), unit), "no value")
  ex  <- ifelse(!is.na(idx) & ext[idx] & v$type != "width", " (state outside calibration sample)", "")
  paths <- lapply(seq_along(MP$id), function(i) tags$path(
    d = MP$d[i], fill = if (is.na(idx[i])) "#dcdbd5" else fill[idx[i]], class = "district",
    `data-id` = MP$id[i],
    onclick = sprintf("Shiny.setInputValue('map_click', '%s', {priority: 'event'})", MP$id[i]),
    tags$title(paste0(nm[i], ": ", lab[i], ex[i]))))
  # legend: gradient with end labels
  gid <- paste0("g", abs(sum(utf8ToInt(var_label))))
  stops <- lapply(seq(0, 1, length.out = 9), function(q)
    tags$stop(offset = paste0(100 * q, "%"), `stop-color` = ramp_hex(cols, q)))
  fmt <- function(x) if (v$type == "change") signed(x, 0) else num(x, 0)
  div(class = "svgmap",
    tags$svg(viewBox = sprintf("0 0 %.0f %.0f", MP$width, MP$height), role = "img",
             `aria-label` = paste("Map of", var_label, "by district"),
             tags$g(paths)),
    div(class = "map-legend",
      tags$svg(width = "260", height = "34",
        tags$defs(tags$linearGradient(id = gid, x1 = "0%", x2 = "100%", stops)),
        tags$rect(x = 0, y = 0, width = 260, height = 12, rx = 3, fill = sprintf("url(#%s)", gid)),
        tags$text(x = 0, y = 28, class = "lg", paste0(fmt(lo), unit)),
        tags$text(x = 260, y = 28, class = "lg", `text-anchor` = "end", paste0(fmt(hi), unit))),
      span(class = "lg-na", span(class = "sw"), if (grey_extrap) "no value / outside calibration states" else "no value")))
}

# ---- Step 2: raw vs corrected ------------------------------------------------
plot_raw_vs_corrected <- function(B, yr = "2019") {
  d <- B$dist
  x <- d[[paste0("raw_wt_", yr)]]; y <- d[[paste0("bayes_", yr)]]
  ext <- d[[paste0("extrapolated_", yr)]] %in% TRUE
  ok <- is.finite(x) & is.finite(y)
  z <- data.frame(x = 100 * x[ok], y = 100 * y[ok],
                  grp = ifelse(ext[ok], "State outside the calibration sample", "State in the calibration sample"),
                  lo = 100 * d[[paste0("bayes_lo_", yr)]][ok], hi = 100 * d[[paste0("bayes_hi_", yr)]][ok],
                  nm = paste0(trimws(gsub("\\s+", " ", d$dist_name[ok])), ", ", d$state_name[ok]))
  z$txt <- sprintf("<b>%s</b><br>Raw NFHS: %s%%<br>Corrected: %s%% (95%% interval %s to %s)",
                   z$nm, num(z$x, 0), num(z$y, 0), num(z$lo, 0), num(z$hi, 0))
  plotly::plot_ly(z, x = ~x, y = ~y, color = ~grp, colors = c(PAL$s2, PAL$s1),
                  type = "scatter", mode = "markers", text = ~txt, hoverinfo = "text",
                  marker = list(size = 7, opacity = 0.7, line = list(color = PAL$surface, width = 1))) |>
    plotly::add_trace(x = c(0, 100), y = c(0, 100), mode = "lines", inherit = FALSE, type = "scatter",
                      hoverinfo = "skip", showlegend = FALSE,
                      line = list(color = PAL$muted, dash = "dash", width = 1.5)) |>
    base_layout(xaxis = ax(paste0("Raw NFHS estimate, ", yr, " (% of rural households)"), range = c(0, 100)),
                yaxis = ax("Corrected estimate (%)", range = c(0, 100)),
                legend = list(orientation = "h", y = 1.1, x = 0))
}

# ---- Step 3: forest plot -------------------------------------------------------
TERM_LAB <- c(change_lpg_raw = "Raw NFHS exposure",
              change_lpg_rc = "Regression-calibrated",
              change_lpg_bayes = "Corrected surface held fixed (conditional)",
              change_lpg_bayes_MI = "Exposure-only imputation (sensitivity)")

plot_forest <- function(K, outcome = "infant", rule = "cr2") {
  h <- K$h
  h <- h[h$outcome == outcome & h$term %in% names(TERM_LAB), ]
  h$lab <- factor(TERM_LAB[h$term], levels = rev(TERM_LAB))
  if (rule == "cr2") { lo <- h$ci_cr2_low; hi <- h$ci_cr2_high; pv <- h$p_cr2; rl <- "small-cluster (CR2)" }
  else               { lo <- h$conf.low;   hi <- h$conf.high;   pv <- h$p;     rl <- "conventional" }
  h$e <- per10(h); h$lo <- 10 * lo; h$hi <- 10 * hi
  h$txt <- sprintf("<b>%s</b><br>%s deaths per 100 births<br>95%% interval %s to %s (%s)<br>p = %s",
                   h$lab, signed(h$e, 2), num(h$lo, 2), num(h$hi, 2), rl, pfmt(pv))
  h$col <- ifelse(h$term == "change_lpg_bayes_MI", PAL$s2, PAL$s1)
  rng <- range(c(h$lo, h$hi, 0), na.rm = TRUE); rng <- rng + c(-0.05, 0.05) * diff(rng)
  p <- plotly::plot_ly(h, y = ~lab)
  for (cc in unique(h$col))   # one segment trace per colour (plotly lines take a single colour)
    p <- plotly::add_segments(p, data = h[h$col == cc, ], x = ~lo, xend = ~hi, y = ~lab, yend = ~lab,
                              hoverinfo = "skip", line = list(color = cc, width = 3), showlegend = FALSE)
  p |>
    plotly::add_markers(data = h, x = ~e, y = ~lab, text = ~txt, hoverinfo = "text", showlegend = FALSE,
                        marker = list(size = 13, color = h$col, line = list(color = PAL$surface, width = 2))) |>
    base_layout(
      xaxis = ax("Deaths per 100 births per 10-point rise in LPG use (negative = fewer)",
                 range = rng, zeroline = TRUE, zerolinewidth = 2, zerolinecolor = PAL$ink2),
      yaxis = ax("", tickfont = list(color = PAL$ink2, size = 13)),
      margin = list(l = 260, r = 20, t = 20, b = 60))
}

# ---- Step 4: where the corrected exposure's uncertainty comes from ---------
plot_variance <- function(B) {
  v <- B$vdec
  v$leg <- ifelse(grepl("^2015", v$leg), "2015 correction (ACCESS)", "2019 correction (IRES)")
  v$cal <- 100 - v$pct_nfhs_side
  plotly::plot_ly(v, y = ~leg, x = ~cal, type = "bar", orientation = "h",
                  name = "Calibration side",
                  marker = list(color = PAL$s2), text = ~paste0(num(cal, 0), "%"),
                  textposition = "inside", insidetextfont = list(color = "white"),
                  hovertemplate = "%{y}<br>Calibration side: %{x:.0f}%<extra></extra>") |>
    plotly::add_trace(x = ~pct_nfhs_side, name = "Health-survey side (NFHS district sampling error)",
                      marker = list(color = PAL$s1), text = ~paste0(num(pct_nfhs_side, 0), "%"),
                      hovertemplate = "%{y}<br>Health-survey side: %{x:.0f}%<extra></extra>") |>
    base_layout(barmode = "stack", bargap = 0.45,
                xaxis = ax("Share of the corrected exposure's uncertainty (%)", range = c(0, 100)),
                yaxis = ax(""), legend = list(orientation = "h", y = 1.25, x = 0, traceorder = "normal"),
                margin = list(l = 170, r = 20, t = 40, b = 50))
}

# ---- Explore: one district ------------------------------------------------------
plot_district <- function(B, id) {
  d <- B$dist[as.character(B$dist$district) == id, ]
  if (!nrow(d)) return(plotly::plotly_empty())
  z <- data.frame(
    yr  = c("2015-16", "2019-21", "2015-16", "2019-21"),
    src = c("Raw NFHS", "Raw NFHS", "Corrected", "Corrected"),
    est = 100 * c(d$raw_wt_2015, d$raw_wt_2019, d$bayes_2015, d$bayes_2019),
    lo  = 100 * c(d$raw_wt_2015 - 1.96 * d$raw_wt_se_2015, d$raw_wt_2019 - 1.96 * d$raw_wt_se_2019,
                  d$bayes_lo_2015, d$bayes_lo_2019),
    hi  = 100 * c(d$raw_wt_2015 + 1.96 * d$raw_wt_se_2015, d$raw_wt_2019 + 1.96 * d$raw_wt_se_2019,
                  d$bayes_hi_2015, d$bayes_hi_2019))
  z$lo <- pmax(z$lo, 0); z$hi <- pmin(z$hi, 100)
  z$x  <- paste(z$src, z$yr, sep = "<br>")
  z$x  <- factor(z$x, levels = z$x[c(1, 3, 2, 4)])
  z$col <- ifelse(z$src == "Raw NFHS", PAL$s1, PAL$s2)
  z$txt <- sprintf("<b>%s, %s</b><br>%s%% (95%% interval %s to %s)", z$src, z$yr,
                   num(z$est, 0), num(z$lo, 0), num(z$hi, 0))
  p <- plotly::plot_ly(z, x = ~x)
  for (cc in unique(z$col))
    p <- plotly::add_segments(p, data = z[z$col == cc & is.finite(z$est), ], x = ~x, xend = ~x, y = ~lo, yend = ~hi,
                              line = list(width = 3, color = cc), hoverinfo = "skip", showlegend = FALSE)
  p |>
    plotly::add_markers(data = z, x = ~x, y = ~est, text = ~txt, hoverinfo = "text", showlegend = FALSE,
                        marker = list(size = 14, color = z$col, line = list(color = PAL$surface, width = 2))) |>
    base_layout(yaxis = ax("Rural households cooking mainly with LPG (%)", range = c(0, 100)),
                xaxis = ax(""), margin = list(l = 60, r = 10, t = 10, b = 60))
}
