# ==============================================================================
# sens.R -- "Under the hood": the two correction methods side by side, every
# corrected surface, every health specification, and the run's own checks.
# Everything is read from the bundle; nothing is re-estimated in the app.
# ==============================================================================

TERM_FULL <- c(
  change_lpg_raw               = "Raw NFHS (multilevel)",
  change_lpg_rc                = "Regression calibration",
  change_lpg_bayes             = "Bayesian correction, point surface",
  change_lpg_bayes_MI          = "Bayesian correction, exposure-only MI (sensitivity)",
  change_lpg_bayes_iresonly    = "Bayesian, IRES calibration both rounds, point",
  change_lpg_bayes_iresonly_MI = "Bayesian, IRES calibration both rounds, exposure-only MI",
  change_lpg_bayes_accesswt    = "Bayesian, design-weighted ACCESS reference, point",
  change_lpg_bayes_accesswt_MI = "Bayesian, design-weighted ACCESS reference, exposure-only MI")

SURFACES <- c(
  "Raw NFHS, multilevel"                 = "raw_ml",
  "Raw NFHS, design-weighted"            = "raw_wt",
  "Regression calibration (multilevel input)"      = "rc_ml",
  "Regression calibration (design-weighted input)" = "rc_wt",
  "Bayesian correction (primary)"        = "bayes",
  "Bayesian, IRES calibration (2015 only)" = "bayes_iresonly")

html_table <- function(df) tags$table(class = "table table-sm",
  tags$thead(tags$tr(lapply(names(df), tags$th))),
  tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v)))))))

cell_fmt <- function(v) {   # whole numbers as integers, others to 3 significant digits
  out <- as.character(v)
  num_v <- suppressWarnings(as.numeric(v))
  ok <- is.finite(num_v)
  whole <- ok & abs(num_v - round(num_v)) < 1e-9 & abs(num_v) < 1e9
  out[whole] <- format(round(num_v[whole]), big.mark = ",", trim = TRUE)
  out[ok & !whole] <- formatC(num_v[ok & !whole], format = "fg", digits = 3, flag = "#")
  out[!ok & !is.na(v) & !is.character(v)] <- ""
  out[is.na(v)] <- ""
  out
}
fmt_tab <- function(df) {
  for (j in seq_along(df)) if (is.numeric(df[[j]])) {
    df[[j]] <- if (all(!is.finite(df[[j]]) | abs(df[[j]] - round(df[[j]])) < 1e-9))
      ifelse(is.finite(df[[j]]), format(round(df[[j]]), big.mark = ",", trim = TRUE), "")
    else ifelse(is.finite(df[[j]]), formatC(df[[j]], format = "fg", digits = 3, flag = "#"), "")
  }
  df
}

# ---- UI ------------------------------------------------------------------------
ui_models <- function(B) div(class = "page",
  h2("The correction models"),
  p(class = "step-sub", "Two ways to correct the health survey's cooking-fuel estimate, and how they compare."),
  layout_columns(col_widths = c(6, 6),
    card(card_header("Regression calibration"),
      p("Fit, in the districts both surveys covered, a straight line on the logit scale from the NFHS estimate to the ",
        "energy survey's estimate, weighted by the energy survey's sample size, then apply the line to every district."),
      tags$ul(tags$li("Transparent and fast."),
              tags$li("Treats the NFHS estimate as if measured without error, which flattens the slope."),
              tags$li("Gives one number per district: no uncertainty to carry into the health model.")),
      p(class = "muted", "In the health models it appears as 'Regression calibration'.")),
    card(card_header("Bayesian measurement-error model (primary)"),
      p("The energy survey's district value (logit) = a + b \u00d7 (true NFHS value) + state effect + residual."),
      tags$ul(tags$li("The NFHS value enters with its own design-based standard error, so its noise does not flatten the slope."),
              tags$li("The energy survey's sampling error is a known variance; the residual SD is the remaining disagreement between instruments."),
              tags$li("States in the calibration sample keep their estimated effect; others get a draw from the between-state distribution."),
              tags$li("Before prediction, each NFHS district value is shrunk toward its state mean (empirical Bayes)."),
              tags$li("Output: posterior draws per district. In the health models they are used only in an exposure-only imputation sensitivity (at risk of attenuation); the outcome-conditioned imputation was abandoned after failing its convergence checks.")))),
  card(card_header("Fitted parameters"),
    p(class = "caption", "Logit scale; 95% posterior intervals for the Bayesian fits. The regression-calibration rows are the slope from the same pairs fitted by ordinary weighted regression with state fixed effects."),
    tableOutput("t_params")),
  layout_columns(col_widths = c(6, 6),
    card(card_header("Sampler diagnostics (Bayesian fits)"), tableOutput("t_sampler"), tableOutput("t_bayesdx"),
         p(class = "caption", "Targets: no divergent transitions; R-hat below 1.01; effective sample sizes above 400.")),
    card(card_header("Input standard errors and shrinkage"),
         p(class = "caption", "Bounds and counts used when the survey standard errors enter the measurement-error model (pipeline diagnostic names; see SI Methods S1)."),
         tableOutput("t_mese"), uiOutput("shrink_ui"))),
  card(card_header("Leave-one-state-out: does the regression calibration travel?"),
    plotlyOutput("p_loso", height = "460px"),
    p(class = "caption", "For each state: error of raw NFHS against the energy survey (grey) and of the regression calibration refitted without that state (orange). Where the orange dot sits left of the grey one, the calibration moved NFHS closer to the energy survey. Only the regression calibration is refitted; this does not validate the Bayesian correction or its intervals.")))

ui_surfaces <- function(B) div(class = "page",
  h2("Compare corrected surfaces"),
  p(class = "step-sub", "Every district-level surface the analysis produced, against any other."),
  layout_columns(col_widths = c(4, 8),
    card(radioButtons("sf_year", "Round", c("2015-16" = "2015", "2019-21" = "2019"), inline = TRUE),
         selectInput("sf_x", "Horizontal axis", names(SURFACES), selected = "Raw NFHS, design-weighted"),
         selectInput("sf_y", "Vertical axis", names(SURFACES), selected = "Bayesian correction (primary)"),
         uiOutput("sf_stats")),
    card(plotlyOutput("p_surf", height = "460px"))),
  layout_columns(col_widths = c(5, 7),
    card(card_header("Average prevalence by estimator (unweighted mean across districts)"), tableOutput("t_mixlev")),
    card(card_header("Correlation between estimators (districts)"), tableOutput("t_mixcor"))))

ui_health <- function(B) {
  ha <- B$health_all
  div(class = "page",
    h2("Health sensitivity explorer"),
    p(class = "step-sub", "Every specification of the child-mortality model the pipeline estimated. Filter, compare, and check."),
    layout_columns(col_widths = c(3, 9),
      card(
        radioButtons("hx_outcome", "Outcome", c("Infant" = "infant", "Neonatal" = "neonatal"), inline = TRUE),
        selectInput("hx_pop", "Population", unique(ha$population)),
        radioButtons("hx_adj", "Covariates", c("Adjusted" = "TRUE", "Unadjusted" = "FALSE"), inline = TRUE),
        radioButtons("hx_wt", "Weighting", c("Births (harmonic mean of outcome-eligible births across rounds)" = "birth_hmean", "None" = "unweighted"), inline = TRUE),
        radioButtons("hx_support", "Districts (MI rows)", c("All" = "all", "State in calibration" = "state_support",
                                                           "Full support" = "full_support"), selected = "all"),
        checkboxGroupInput("hx_terms", "Exposures", choices = setNames(names(TERM_FULL), TERM_FULL),
                           selected = names(TERM_FULL)),
        radioButtons("hx_rule", "Interval", c("CR2 (primary)" = "cr2", "Conventional" = "conv"), inline = TRUE)),
      div(card(card_header("Estimates"), plotlyOutput("p_hx", height = "460px"),
               p(class = "caption", "Deaths per 100 births per 10-point rise in primary-LPG prevalence. Orange: exposure-only imputation (MI) rows, a sensitivity analysis at risk of attenuation; other rows hold the corrected surface fixed and are conditional on the calibration. Rows the pipeline did not estimate for this combination are absent (for example, unweighted MI).")),
          card(card_header("Table"), div(class = "nowrap-num", tableOutput("t_hx"))))),
    navset_card_tab(
      nav_panel("Estimand: expectation vs predictive", tableOutput("t_ppd"),
        p(class = "caption", "Sensitivity analyses of the estimand. 'Calibrated expectation' rows use the calibrated expectation of the district's prevalence: the point-surface rows hold it fixed, and the MI row is the exposure-only imputation over the calibration posterior. ",
          "'District true prevalence' rows add the calibration residual, so each draw is a putative true prevalence; the predictive MI is also exposure-only and especially prone to attenuation. ",
          "None of these imputations conditions on the outcome.")),
      nav_panel("Leg-A reference: multilevel vs design-weighted ACCESS", tableOutput("t_legA")),
      nav_panel("Alternative exposure definitions", tableOutput("t_nuanced"),
        p(class = "caption", "Hypothesis-generating only. These models are unweighted (each district counted once), so the primary-LPG row here differs from the birth-weighted headline estimate. ",
          "The composition exposures are model-predicted from a stacking model that failed validation, and the pipeline flags that their signs are not mutually consistent. ",
          "Rows as in the manuscript's Table S8."))))
}

ui_checks <- function(B) div(class = "page",
  h2("Run checks and diagnostics"),
  p(class = "step-sub", "The pipeline checks itself as it runs. A FAIL stops a publication run; a WARN is reported and explained."),
  layout_columns(col_widths = c(5, 7),
    card(card_header("Check summary by script"), tableOutput("t_chk_sum")),
    card(card_header("Mixed-model fits"), tableOutput("t_fits"),
         p(class = "caption", "Singular fits are those where a variance component (usually the state level) is estimated at zero."))),
  card(card_header(div(class = "card-head-row", "All checks",
       radioButtons("chk_status", NULL, c("WARN and FAIL" = "nonpass", "All" = "all"), inline = TRUE))),
       tableOutput("t_chk")))

# ---- server ----------------------------------------------------------------------
sens_server <- function(input, output, session, B) {
  output$t_params <- renderTable({
    rows <- list()
    if (!is.null(B$calib)) rows[[1]] <- data.frame(Model = "Bayesian (primary)", Calibration = B$calib$calibration,
      Parameter = B$calib$parameter, Estimate = B$calib$estimate, Low = B$calib$lo95, High = B$calib$hi95)
    if (!is.null(B$calib_wt)) rows[[2]] <- data.frame(Model = "Bayesian, design-weighted ACCESS reference (sensitivity)",
      Calibration = B$calib_wt$calibration, Parameter = B$calib_wt$parameter, Estimate = B$calib_wt$estimate,
      Low = B$calib_wt$lo95, High = B$calib_wt$hi95)
    if (!is.null(B$rc_check)) {
      r <- B$rc_check
      rows[[3]] <- data.frame(Model = "Regression calibration (state fixed effects)", Calibration = r$leg,
        Parameter = "slope", Estimate = r$slope_state, Low = r$slope_state - 1.96 * r$se_state,
        High = r$slope_state + 1.96 * r$se_state)
      if ("slope_applied" %in% names(r))
        rows[[4]] <- data.frame(Model = "Regression calibration (as applied, no state effects)", Calibration = r$leg,
          Parameter = "slope", Estimate = r$slope_applied, Low = NA, High = NA)
    }
    d <- do.call(rbind, rows)
    d <- d[!d$Parameter %in% c("sd_residual_rms"), ]
    for (j in c("Estimate", "Low", "High")) d[[j]] <- cell_fmt(d[[j]])
    d
  })
  output$t_sampler <- renderTable(if (!is.null(B$sampler)) data.frame(Setting = gsub("_", " ", B$sampler$quantity), Value = cell_fmt(B$sampler$value)))
  output$t_bayesdx <- renderTable(if (!is.null(B$bayes_dx)) { x <- B$bayes_dx
    data.frame(Fit = x$fit, Divergences = as.integer(x$divergences), `Max R-hat` = num(x$max_rhat, 4),
               `Min bulk ESS` = format(round(x$min_ess_bulk), big.mark = ","),
               `Min tail ESS` = format(round(x$min_ess_tail), big.mark = ","), check.names = FALSE) })
  output$t_mese <- renderTable(if (!is.null(B$me_se)) data.frame(Quantity = B$me_se$quantity, Value = cell_fmt(B$me_se$value)))
  output$shrink_ui <- renderUI({
    s <- B$shrink
    if (is.null(s)) return(p(class = "muted", "Empirical-Bayes shrinkage summaries (tau and shrinkage weights per surface) appear here once the current run is published."))
    agg <- do.call(rbind, lapply(split(s, s$surface), function(z) data.frame(
      Surface = z$surface[1], Districts = nrow(z), tau = sqrt(z$tau2[1]),
      `Median weight on own estimate` = stats::median(z$lambda),
      `Degenerate SE (set to cap)` = sum(z$degenerate_se %in% TRUE), check.names = FALSE)))
    tagList(h6("Empirical-Bayes shrinkage of the NFHS input"), html_table(fmt_tab(agg)))
  })
  output$p_loso <- renderPlotly({
    l <- B$loso; req(l)
    l <- l[is.finite(l$rmse_raw) & is.finite(l$rmse_corr), ]
    l <- l[order(l$rmse_raw), ]; l$st <- factor(l$state_left_out, levels = l$state_left_out)
    plotly::plot_ly(l, y = ~st) |>
      plotly::add_segments(x = ~100 * rmse_corr, xend = ~100 * rmse_raw, yend = ~st, hoverinfo = "skip",
                           line = list(color = PAL$axis, width = 2), showlegend = FALSE) |>
      plotly::add_markers(x = ~100 * rmse_raw, name = "Raw NFHS", marker = list(color = PAL$muted, size = 10),
                          text = ~sprintf("%s: raw error %s pp", st, num(100 * rmse_raw, 1)), hoverinfo = "text") |>
      plotly::add_markers(x = ~100 * rmse_corr, name = "Corrected (state held out)", marker = list(color = PAL$s2, size = 10),
                          text = ~sprintf("%s: corrected error %s pp (%s districts)", st, num(100 * rmse_corr, 1), n_districts),
                          hoverinfo = "text") |>
      base_layout(xaxis = ax("Root-mean-square error against the energy survey (percentage points)", rangemode = "tozero"),
                  yaxis = ax(""), legend = list(orientation = "h", y = 1.06, x = 0),
                  margin = list(l = 140, r = 20, t = 30, b = 55))
  })

  # surfaces
  sf_data <- reactive({
    yr <- input$sf_year; d <- B$dist
    cx <- paste0(SURFACES[[input$sf_x]], "_", yr); cy <- paste0(SURFACES[[input$sf_y]], "_", yr)
    if (!cx %in% names(d) || !cy %in% names(d)) return(NULL)
    data.frame(x = 100 * d[[cx]], y = 100 * d[[cy]],
               nm = paste0(trimws(gsub("\\s+", " ", d$dist_name)), ", ", d$state_name),
               ext = d[[paste0("extrapolated_", yr)]] %in% TRUE)
  })
  output$p_surf <- renderPlotly({
    z <- sf_data()
    if (is.null(z)) return(plotly::plotly_empty() |> base_layout(title = list(text = "This surface is not available for this round")))
    z <- z[is.finite(z$x) & is.finite(z$y), ]
    z$grp <- ifelse(z$ext, "State outside calibration sample", "State in calibration sample")
    plotly::plot_ly(z, x = ~x, y = ~y, color = ~grp, colors = c(PAL$s2, PAL$s1), type = "scatter", mode = "markers",
                    text = ~sprintf("<b>%s</b><br>x: %s%%<br>y: %s%%", nm, num(x, 1), num(y, 1)), hoverinfo = "text",
                    marker = list(size = 7, opacity = 0.7, line = list(color = PAL$surface, width = 1))) |>
      plotly::add_trace(x = c(0, 100), y = c(0, 100), mode = "lines", type = "scatter", inherit = FALSE,
                        hoverinfo = "skip", showlegend = FALSE, line = list(color = PAL$muted, dash = "dash")) |>
      base_layout(xaxis = ax(paste0(input$sf_x, " (%)"), range = c(0, 100)),
                  yaxis = ax(paste0(input$sf_y, " (%)"), range = c(0, 100)),
                  legend = list(orientation = "h", y = 1.08, x = 0))
  })
  output$sf_stats <- renderUI({
    z <- sf_data(); if (is.null(z)) return(NULL)
    ok <- is.finite(z$x) & is.finite(z$y)
    div(class = "readout",
      div(tags$b(sum(ok), " districts")),
      div(paste0("Correlation r = ", num(stats::cor(z$x[ok], z$y[ok]), 3))),
      div(paste0("Mean difference (y - x) = ", signed(mean(z$y[ok] - z$x[ok]), 1), " pp")),
      div(paste0("Mean absolute difference = ", num(mean(abs(z$y[ok] - z$x[ok])), 1), " pp")))
  })
  output$t_mixlev <- renderTable(if (!is.null(B$mix_lev)) data.frame(Round = as.integer(B$mix_lev$year), Estimator = B$mix_lev$estimator, `Mean prevalence (%)` = num(B$mix_lev$mean_prev, 1), check.names = FALSE))
  output$t_mixcor <- renderTable(if (!is.null(B$mix_cor)) { m <- B$mix_cor; m$year <- as.integer(m$year)
    for (j in 3:ncol(m)) m[[j]] <- num(m[[j]], 3); setNames(m, c("Round", "Estimator", names(m)[-(1:2)])) })

  # health explorer
  hx <- reactive({
    h <- B$health_all; req(h)
    h <- h[h$outcome == input$hx_outcome & h$population == input$hx_pop &
           as.character(h$adjusted) == input$hx_adj & h$weighting == input$hx_wt &
           h$term %in% input$hx_terms, ]
    h <- h[h$support_stratum == "all" | (grepl("_MI$", h$term) & h$support_stratum == input$hx_support), ]
    if (input$hx_support != "all") h <- h[!(grepl("_MI$", h$term) & h$support_stratum == "all"), ]
    h$lab <- ifelse(h$support_stratum == "all", TERM_FULL[h$term], paste0(TERM_FULL[h$term], " [", h$support_stratum, "]"))
    h
  })
  output$p_hx <- renderPlotly({
    h <- hx()
    if (!nrow(h)) return(plotly::plotly_empty() |> base_layout(title = list(text = "No specification matches these filters")))
    cr2 <- input$hx_rule == "cr2"
    h$lo <- 10 * (if (cr2) h$ci_cr2_low else h$conf.low); h$hi <- 10 * (if (cr2) h$ci_cr2_high else h$conf.high)
    h$e <- per10(h); h$pv <- if (cr2) h$p_cr2 else h$p
    h$col <- ifelse(grepl("_MI$", h$term), PAL$s2, PAL$s1)
    h <- h[order(-match(h$term, names(TERM_FULL))), ]
    h$lab <- factor(h$lab, levels = unique(h$lab))
    h$txt <- sprintf("<b>%s</b><br>%s [%s, %s]<br>p = %s%s", h$lab, num(h$e, 3), num(h$lo, 3), num(h$hi, 3), pfmt(h$pv),
                     ifelse(is.finite(h$p_wild), paste0("<br>wild bootstrap p = ", pfmt(h$p_wild)), ""))
    p <- plotly::plot_ly(h, y = ~lab)
    for (cc in unique(h$col))
      p <- plotly::add_segments(p, data = h[h$col == cc, ], x = ~lo, xend = ~hi, y = ~lab, yend = ~lab,
                                line = list(color = cc, width = 3), hoverinfo = "skip", showlegend = FALSE)
    p |> plotly::add_markers(data = h, x = ~e, y = ~lab, text = ~txt, hoverinfo = "text", showlegend = FALSE,
                             marker = list(size = 12, color = h$col, line = list(color = PAL$surface, width = 2))) |>
      base_layout(xaxis = ax("Deaths per 100 births per 10-point rise (negative = fewer)", zeroline = TRUE,
                             zerolinewidth = 2, zerolinecolor = PAL$ink2),
                  yaxis = ax(""), margin = list(l = 360, r = 20, t = 10, b = 55))
  })
  output$t_hx <- renderTable({
    h <- hx(); if (!nrow(h)) return(NULL)
    data.frame(Exposure = h$lab, `Per 10 pp` = num(per10(h), 3),
      `95% CI (CR2)` = paste(num(10 * h$ci_cr2_low, 3), "to", num(10 * h$ci_cr2_high, 3)),
      `df (Satterthwaite / BR)` = num(h$df_satt, 1), `p (CR2)` = pfmt(h$p_cr2),
      `95% CI (conv.)` = paste(num(10 * h$conf.low, 3), "to", num(10 * h$conf.high, 3)), `p (conv.)` = pfmt(h$p),
      `p (wild bootstrap)` = ifelse(is.finite(h$p_wild), pfmt(h$p_wild), ""),
      Districts = as.integer(h$n), States = as.integer(h$n_clusters), check.names = FALSE)
  })
  output$t_ppd <- renderTable({
    s <- B$ppd; req(s)
    out <- data.frame(Outcome = s$outcome, Version = s$label, `Per 10 pp` = num(10 * s$estimate, 3),
                      SE = num(10 * s$se, 3), `p (conv.)` = pfmt(s$p), FMI = ifelse(is.finite(s$fmi), pp(s$fmi, 0), ""),
                      check.names = FALSE)
    if ("p_cr2" %in% names(s)) out$`p (CR2)` <- pfmt(s$p_cr2)
    out
  })
  output$t_legA <- renderTable({
    m <- B$legA_move; req(m)
    data.frame(Population = m$population, Comparison = m$comparison, Outcome = m$outcome,
               `Primary (per 10 pp)` = num(10 * m$estimate_primary, 3), `Sensitivity` = num(10 * m$estimate_sens, 3),
               `Change, in primary SEs` = num(m$diff_in_primary_se, 2), `Sign flip` = m$sign_flip,
               `Significance flip` = m$sig_flip, check.names = FALSE)
  })
  output$t_nuanced <- renderTable({
    n <- B$nuanced; req(n)
    # Only the rows the manuscript reports (Table S8): the point-surface fits,
    # which carry CR2 inference. The MI rows have conventional inference only.
    if ("analysis" %in% names(n) && any(n$analysis == "point")) n <- n[n$analysis == "point", ]
    data.frame(Analysis = n$analysis, Outcome = n$outcome, Exposure = n$exposure, `Per` = n$`per(+)`,
               Estimate = num(n$est_per10, 3), `95% CI (CR2)` = ifelse(is.finite(n$lo_cr2), paste(num(n$lo_cr2, 3), "to", num(n$hi_cr2, 3)), "\u2014"),
               `p (CR2)` = ifelse(is.finite(n$p_cr2), pfmt(n$p_cr2), "\u2014"), check.names = FALSE)
  })

  # checks
  output$t_chk_sum <- renderTable({
    c <- B$checks; req(c)
    t <- as.data.frame.matrix(table(c$script, c$status))
    cbind(Script = rownames(t), t)
  })
  output$t_fits <- renderTable(if (!is.null(B$fits_dx)) fmt_tab(setNames(B$fits_dx, c("Script", "Fits", "Singular", "Unknown", "% singular"))))
  output$t_chk <- renderTable({
    c <- B$checks; req(c)
    if (input$chk_status == "nonpass") c <- c[c$status != "PASS", ]
    data.frame(Script = c$script, Status = c$status, Check = c$check, Detail = c$detail)
  })
}

# ==============================================================================
# Stacking (experimental): validation first, then the reference surveys' own
# measured shares, then -- behind a warning -- the model-based national surface.
# ==============================================================================
svg_map_simple <- function(MP, ids, z, names_, unit = "%", lo = 0, hi = 100, cols = PAL$seq,
                           na_label = "not sampled") {
  if (is.null(MP)) return(div(class = "muted", "District boundaries are not available in this bundle."))
  idx  <- match(MP$id, as.character(ids))
  fill <- ramp_hex(cols, (z - lo) / (hi - lo))
  paths <- lapply(seq_along(MP$id), function(i) {
    j <- idx[i]
    tags$path(d = MP$d[i], class = "district nolink",
              fill = if (is.na(j) || !is.finite(z[j])) "#dcdbd5" else fill[j],
              tags$title(if (is.na(j) || !is.finite(z[j])) na_label else paste0(names_[j], ": ", num(z[j], 0), unit)))
  })
  gid <- paste0("gs", abs(sum(utf8ToInt(paste(cols, collapse = "")))) + round(hi))
  stops <- lapply(seq(0, 1, length.out = 9), function(q)
    tags$stop(offset = paste0(100 * q, "%"), `stop-color` = ramp_hex(cols, q)))
  div(class = "svgmap",
    tags$svg(viewBox = sprintf("0 0 %.0f %.0f", MP$width, MP$height), role = "img", tags$g(paths)),
    div(class = "map-legend",
      tags$svg(width = "260", height = "34",
        tags$defs(tags$linearGradient(id = gid, x1 = "0%", x2 = "100%", stops)),
        tags$rect(x = 0, y = 0, width = 260, height = 12, rx = 3, fill = sprintf("url(#%s)", gid)),
        tags$text(x = 0, y = 28, class = "lg", paste0(num(lo, 0), unit)),
        tags$text(x = 260, y = 28, class = "lg", `text-anchor` = "end", paste0(num(hi, 0), unit))),
      span(class = "lg-na", span(class = "sw"), na_label)))
}

stack_verdict <- function(B) {
  s <- B$stack_state_sum
  if (is.null(s)) return(p(class = "muted",
    "A state-level check (does the model beat simply assigning every state the national share?) appears here after scripts/27_stacking_state_validation.R has been run."))
  r <- s[grepl("leave-one-state-out", s$check), ]
  if (!nrow(r)) return(NULL)
  P(BB("State level: "), if (r$skill_vs_benchmark[1] > 0.2)
      paste0("predicting each held-out IRES state from its households' characteristics beats assigning it the overall share (skill ",
             num(r$skill_vs_benchmark[1], 2), ", r = ", num(r$r_state[1], 2), " across ", r$n_states[1],
             " states), so broad state-level patterns carry some signal.")
    else paste0("predicting each held-out IRES state from its households' characteristics does not meaningfully beat assigning it the overall share (skill ",
                num(r$skill_vs_benchmark[1], 2), ", r = ", num(r$r_state[1], 2), " across ", r$n_states[1],
                " states). Even state-level stacking cannot be inferred from what NFHS records."),
    { x <- s[grepl("ACCESS", s$check), ]
      if (nrow(x)) paste0(" The IRES model does better on the ", x$n_states[1], " ACCESS states (r = ", num(x$r_state[1], 2),
                          ", skill ", num(x$skill_vs_benchmark[1], 2), "), but those states were in its training data, so that is a test across survey instruments, not across places.")
      else "" })
}

ui_stacking <- function(B) div(class = "page",
  h2("Fuel stacking (experimental)"),
  p(class = "step-sub", "Households that cook mainly with LPG but still burn solid fuel. Can we map it nationally?"),
  div(class = "banner", span(class = "banner-icon", "\u26a0"),
      " The model-based national stacking surface fails its validation checks. Do not use it to rank or target districts. ",
      "The reference surveys' own measured shares (left map) are the reliable stacking data."),
  layout_columns(col_widths = c(5, 7),
    div(in_short(
      P("We trained a model of stacking on ACCESS and IRES households (wealth, caste, religion, ration card, household size, ",
        "electricity) and applied it to every NFHS household. Validation says it does not work: it barely separates stacking from ",
        "non-stacking households, and its district ranking largely disappears once it leaves the survey it was trained on."),
      stack_verdict(B),
      P(class = "why", "That is itself the finding: the models we tested, built on the characteristics a national survey records, did not reliably predict who keeps burning ",
        "solid fuel alongside LPG. Stacking has to be asked about directly, which is why we recommend a fuel-stacking module in NFHS.")),
      card(card_header("Validation"), tableOutput("t_stack_val"),
           p(class = "caption", "AUC 0.5 = no better than chance at the household level. District r: correlation between predicted and measured district stacking shares. In-sample rows are a ceiling, not a test."))),
    card(card_header(div(class = "card-head-row", "Measured stacking share, reference surveys",
           radioButtons("stk_survey", NULL, c("IRES 2019-20" = "IRES (2019-20)", "ACCESS 2014-15" = "ACCESS Wave 1 (2014-15)"), inline = TRUE))),
         uiOutput("m_stack_ref"),
         p(class = "caption", "Share of primary-LPG households that also reported burning solid fuel, estimated directly from each survey's own households (multilevel). Grey: district not sampled."))),
  card(card_header("Where does stacking vary? Place versus household"), uiOutput("vpart_ui")),
  ui_stacking_transfer(B),
  layout_columns(col_widths = c(5, 7),
    card(card_header("State-level check"), plotlyOutput("p_stack_state", height = "380px")),
    card(card_header("Model-based national surface (fails validation; shown for transparency)"),
         radioButtons("stk_round", NULL, c("NFHS-5 households" = "NFHS5", "NFHS-4 households" = "NFHS4"), inline = TRUE),
         checkboxInput("stk_show", "I understand this surface failed validation; show it", FALSE),
         uiOutput("m_stack_model"))))

stacking_server <- function(input, output, session, B, MP) {
  output$t_stack_val <- renderTable({
    rows <- list()
    if (!is.null(B$transfer)) {
      t <- B$transfer
      rows[[1]] <- data.frame(Test = paste(t$train, "to", t$test), Kind = t$kind,
                              AUC = num(t$auc, 2), `District r` = num(t$district_r, 2), check.names = FALSE)
    }
    if (!is.null(B$stack_ldo)) {
      s <- B$stack_ldo
      rows[[2]] <- data.frame(Test = paste(s$survey, "leave-district-out"), Kind = "within survey",
                              AUC = num(s$auc, 2), `District r` = num(s$district_r, 2), check.names = FALSE)
    }
    do.call(rbind, rows)
  })
  output$m_stack_ref <- renderUI({
    r <- B$ref_stack
    if (is.null(r)) return(p(class = "muted", "Measured reference-survey stacking shares appear after scripts/27_stacking_state_validation.R has been run."))
    r <- r[r$survey == input$stk_survey, ]
    nm <- B$dist$dist_name[match(as.character(r$district), as.character(B$dist$district))]
    svg_map_simple(MP, r$district, 100 * r$stacking_share,
                   paste0(trimws(gsub("\\s+", " ", nm)), ", ", r$state, " (n = ", r$n_households, ")"))
  })
  output$p_stack_state <- renderPlotly({
    s <- B$stack_state
    if (is.null(s)) return(plotly::plotly_empty() |> base_layout(title = list(text = "Run scripts/27_stacking_state_validation.R", font = list(size = 13))))
    s$col <- ifelse(grepl("leave-one-state-out", s$check), "IRES, state held out", "IRES model on ACCESS states")
    plotly::plot_ly(s, x = ~100 * observed, y = ~100 * predicted, color = ~col, colors = c(PAL$s2, PAL$s1),
                    type = "scatter", mode = "markers",
                    text = ~sprintf("<b>%s</b><br>measured %s%%, predicted %s%%<br>%s households", state,
                                    num(100 * observed, 0), num(100 * predicted, 0), n_households),
                    hoverinfo = "text", marker = list(size = 10, line = list(color = PAL$surface, width = 1.5))) |>
      plotly::add_trace(x = c(0, 100), y = c(0, 100), mode = "lines", type = "scatter", inherit = FALSE,
                        hoverinfo = "skip", showlegend = FALSE, line = list(color = PAL$muted, dash = "dash")) |>
      base_layout(xaxis = ax("Measured stacking share (%)", range = c(0, 100)),
                  yaxis = ax("Predicted from household characteristics (%)", range = c(0, 100)),
                  legend = list(orientation = "h", y = 1.1, x = 0))
  })
  output$m_stack_model <- renderUI({
    if (!isTRUE(input$stk_show)) return(p(class = "muted", "Hidden by default."))
    x <- B$proxy
    if (is.null(x) || !"p_stack" %in% names(x)) return(p(class = "muted", "Not in this bundle."))
    x <- x[x$survey == input$stk_round, ]
    nm <- B$dist$dist_name[match(as.character(x$district), as.character(B$dist$district))]
    tagList(div(class = "flag-note", "Fails validation: do not use for ranking or targeting districts."),
            svg_map_simple(MP, x$district, 100 * x$p_stack, paste0(trimws(gsub("\\s+", " ", nm)), ", ", x$state),
                           na_label = "no estimate"))
  })
}

# ---- What transfers: the same household model fitted in each survey -----------
TERM_LAB_STACK <- c(hhsize_c = "Household size (per person)", wealth_q = "Wealth quintile (per step)",
                    elec_bin = "Has electricity", bpl_bin = "Holds a BPL/Antyodaya card",
                    caste3General = "General caste (vs SC)", `caste3Other Backward Class` = "OBC (vs SC)",
                    `caste3Scheduled Tribe` = "Scheduled Tribe (vs SC)", religion3Muslim = "Muslim (vs Hindu)")

stack_coef_long <- function(B) {
  t <- B$transfer_coef
  if (is.null(t)) return(NULL)
  era <- t[t$contrast == "ERA (W1 vs W2)", ]; ei <- t[grepl("^ERA\\+INSTRUMENT", t$contrast), ]
  rbind(data.frame(term = era$term, survey = "ACCESS 2014-15", est = era$e1, se = era$s1),
        data.frame(term = era$term, survey = "ACCESS 2018",    est = era$e2, se = era$s2),
        data.frame(term = ei$term,  survey = "IRES 2019-20",   est = ei$e2,  se = ei$s2))
}

ui_stacking_transfer <- function(B) card(card_header("What does transfer: the same household model fitted in each survey"),
  layout_columns(col_widths = c(7, 5),
    plotlyOutput("p_stack_coef", height = "420px"),
    div(uiOutput("stack_coef_text"),
        p(class = "caption", "Log-odds of stacking among primary-LPG households, six states covered by all three surveys, rural households, ",
          "with 95% intervals. Positive = more stacking. 'Stable' means the coefficient does not differ significantly between ",
          "surveys and keeps its sign (SI Table S15)."))),
  tableOutput("t_stack_shift"))

stacking_transfer_server <- function(input, output, session, B) {
  output$p_stack_coef <- renderPlotly({
    L <- stack_coef_long(B); req(L)
    L <- L[L$term %in% c("hhsize_c", "wealth_q", "elec_bin", "bpl_bin", "caste3General"), ]
    L$lab <- factor(TERM_LAB_STACK[L$term], levels = rev(TERM_LAB_STACK[c("hhsize_c", "wealth_q", "elec_bin", "bpl_bin", "caste3General")]))
    cols <- c("ACCESS 2014-15" = PAL$s1, "ACCESS 2018" = PAL$s3, "IRES 2019-20" = PAL$s2)
    off <- c("ACCESS 2014-15" = 0.22, "ACCESS 2018" = 0, "IRES 2019-20" = -0.22)
    L$y <- as.numeric(L$lab) + off[L$survey]
    p <- plotly::plot_ly()
    for (sv in names(cols)) {
      z <- L[L$survey == sv, ]
      p <- p |>
        plotly::add_segments(data = z, x = ~est - 1.96 * se, xend = ~est + 1.96 * se, y = ~y, yend = ~y,
                             line = list(color = cols[[sv]], width = 2.5), hoverinfo = "skip", showlegend = FALSE) |>
        plotly::add_markers(data = z, x = ~est, y = ~y, name = sv,
                            marker = list(size = 10, color = cols[[sv]], line = list(color = PAL$surface, width = 1.5)),
                            text = ~sprintf("<b>%s</b><br>%s: %s (95%% %s to %s)", lab, sv, num(est, 2),
                                            num(est - 1.96 * se, 2), num(est + 1.96 * se, 2)), hoverinfo = "text")
    }
    lv <- levels(L$lab)
    base_layout(p, xaxis = ax("Log-odds of stacking", zeroline = TRUE, zerolinewidth = 2, zerolinecolor = PAL$ink2),
                yaxis = ax("", tickvals = seq_along(lv), ticktext = lv, tickfont = list(color = PAL$ink2)),
                legend = list(orientation = "h", y = 1.08, x = 0), margin = list(l = 220, r = 20, t = 30, b = 55))
  })
  output$stack_coef_text <- renderUI({
    L <- stack_coef_long(B); t <- B$transfer_coef; req(L, t)
    stable <- function(term) {   # stable on every contrast in which it was tested
      z <- t[t$term == term, ]; nrow(z) > 0 && all(z$transfers %in% TRUE)
    }
    g <- function(term, sv) L$est[L$term == term & L$survey == sv][1]
    bp <- t[t$term == "bpl_bin" & t$contrast == "ERA (W1 vs W2)", ]
    tagList(
      P(BB("Some household associations recur across surveys, but the stacking relationship did not transfer reliably. "),
        "Larger households stack more", if (stable("hhsize_c")) " (consistent across every contrast tested)" else "",
        "; wealthier households stack less", if (stable("wealth_q")) " (consistent across every contrast tested)" else "",
        "; households with electricity stack less in some surveys",
        if (stable("elec_bin")) " (consistent across every contrast tested)" else " (not consistent across every contrast tested)",
        ". 'Stacking' here means reporting any solid-fuel use alongside LPG; it does not establish how often or how much solid fuel is burned."),
      P(BB("BPL card holders: "), "the association with stacking rises from ", num(g("bpl_bin", "ACCESS 2014-15"), 2),
        " (2014-15) to ", num(g("bpl_bin", "ACCESS 2018"), 2), " (2018) and ", num(g("bpl_bin", "IRES 2019-20"), 2),
        " (2019-20). That fits newly connected Ujjwala households keeping solid fuel alongside LPG, but the 2014-15 to 2018 change is ",
        if (nrow(bp) && is.finite(bp$p) && bp$p >= 0.05) paste0("not statistically significant (p = ", pfmt(bp$p), "), so read it as suggestive.")
        else paste0("p = ", pfmt(bp$p), ".")),
      P(class = "muted", "What does not carry over is the level and geography of stacking: the district random effects, which stand in for local ",
        "fuel availability, prices and practice, carry most of the explained variation, and none of those drivers is in NFHS."))
  })
  output$t_stack_shift <- renderTable({
    s <- B$transfer_shift; req(s)
    vl <- c(caste3 = "Caste:", religion3 = "Religion:", hhsize_c = "Household size", bpl_bin = "BPL/Antyodaya card",
            elec_bin = "Electricity", wealth_q = "Wealth quintile")
    lab <- ifelse(s$level == "mean", vl[s$variable], paste(vl[s$variable], s$level))
    data.frame(Variable = lab, `ACCESS 2014-15` = num(s$ACCESS_W1_2015, 2),
               `ACCESS 2018` = num(s$ACCESS_W2_2018, 2), `IRES 2019-20` = num(s$IRES_rural_common, 2), check.names = FALSE)
  }, caption = "How the households themselves changed (shares, or means for household size and wealth quintile)", caption.placement = "top")
}


# ---- variance partition (script 28) and NFHS seasonality ----------------------
vpart_server <- function(input, output, session, B) {
  output$vpart_ui <- renderUI({
    v <- B$vpart
    if (is.null(v) || !nrow(v)) return(p(class = "muted", "Appears after scripts/28_stacking_place_and_season.R has been run."))
    tab <- data.frame(Survey = v$survey, Households = format(v$n_households, big.mark = ","),
                      `Between states, districts and villages (%)` = num(v$pct_place_total, 0),
                      `Of which: village (%)` = num(v$pct_village, 0),
                      `Household characteristics (%)` = num(v$marginal_r2_household_pct, 1),
                      check.names = FALSE)
    less <- all(v$marginal_r2_household_pct < v$pct_place_total, na.rm = TRUE)
    tagList(
      P(if (less) paste0("In every reference survey, more of the variation in stacking lies between places (",
                         num(min(v$pct_place_total), 0), "-", num(max(v$pct_place_total), 0),
                         "%) than is explained by the household characteristics NFHS records (",
                         num(min(v$marginal_r2_household_pct), 1), "-", num(max(v$marginal_r2_household_pct), 1), "%).")
        else "Place and household characteristics explain comparable shares of the variation in stacking."),
      html_table(tab),
      p(class = "caption", "Latent logistic scale (household-level variance fixed at pi^2/3), so the shares are intraclass correlations and modest by construction; compare the columns, not the absolute size. Primary-LPG households; outcome: also reports burning solid fuel (not its frequency or amount)."))
  })
}
