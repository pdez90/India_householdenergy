# ==============================================================================
# explain.R -- the "why it matters" and "how it works" layers:
#   * story_lessons(): what the analysis means for household-energy research
#   * correction explainer: regression calibration vs the Bayesian correction,
#     with an interactive translation curve
#   * CR1 vs CR2: why the conventional clustered standard error is optimistic
#   * marg_summary(): the cluster-averaging table, labelled
# Every number is read from the bundle. Maths is typeset with MathJax.
# ==============================================================================

mj <- function(...) HTML(paste0(...))   # a string that contains MathJax markup

# ---- small helpers -------------------------------------------------------------
cal_par <- function(B, leg, par) {
  c <- B$calib
  if (is.null(c)) return(NA_real_)
  v <- c$estimate[grepl(leg, c$calibration) & c$parameter == par]
  if (length(v)) as.numeric(v[1]) else NA_real_
}
# Regression calibration is deterministic: corrected = plogis(a + b * logit(raw)).
# Its two coefficients are recovered exactly from the published district pairs.
rc_coef <- function(B, yr) {
  d <- B$dist
  if (is.null(d)) return(c(NA_real_, NA_real_))
  x <- d[[paste0("raw_ml_", yr)]]; y <- d[[paste0("rc_ml_", yr)]]
  ok <- is.finite(x) & is.finite(y) & x > 0.001 & x < 0.999 & y > 0.001 & y < 0.999
  if (sum(ok) < 10) return(c(NA_real_, NA_real_))
  unname(coef(lm(qlogis(y[ok]) ~ qlogis(x[ok]))))
}
leg_of <- function(yr) if (yr == "2015") "NFHS-4" else "NFHS-5"
mix_lev <- function(B, yr, est) {
  m <- B$mix_lev
  if (is.null(m)) return(NA_real_)
  v <- m$mean_prev[m$year == as.integer(yr) & m$estimator == est]
  if (length(v)) as.numeric(v[1]) else NA_real_
}

# ==============================================================================
# Story: what this means for household-energy research
# ==============================================================================
story_lessons <- function(B, K) {
  vp <- B$vpart; ss <- B$stack_state_sum; ldo <- B$stack_ldo
  ires_vp <- if (!is.null(vp)) vp[grepl("IRES", vp$survey), ][1, ] else NULL
  loso <- if (!is.null(ss)) ss[grepl("leave-one-state-out", ss$check), ][1, ] else NULL
  sig19 <- cal_par(B, "NFHS-5", "sd_residual"); sig15 <- cal_par(B, "NFHS-4", "sd_residual")
  n19 <- cal_par(B, "NFHS-5", "n_districts")
  auc_rng <- if (!is.null(ldo)) paste0(num(min(ldo$auc), 2), "-", num(max(ldo$auc), 2)) else "NA"

  lesson <- function(title, ...) card(class = "lesson-card", card_header(title), ...)
  div(class = "lessons",
    h2("What this means for household-energy research"),
    p(class = "step-sub", "The four steps answer one narrow question. These are the broader lessons for anyone using NFHS to study household energy."),
    layout_columns(col_widths = c(6, 6),
      lesson("1. One 'main fuel' question cannot carry the weight put on it",
        P("NFHS asks a single question: the household's <i>main</i> cooking fuel. Many rural LPG users also report burning wood, dung or crop residue",
          if (!is.null(ires_vp)) paste0(" (", pp(ires_vp$stacking_share, 0), "% of IRES rural households whose main fuel is LPG also burn solid fuel)") else "",
          ", and the 'main' fuel can change with the season, with what is being cooked, and with whether a cylinder is in the house. ",
          "During the LPG scale-up the same question stopped lining up with dedicated energy surveys: NFHS-4 and ACCESS agreed to within ",
          BB(num(abs(K$d15), 1), " points"), " in 2015, while NFHS-5 recorded ", BB(num(abs(K$d19), 0), " points"),
          " less LPG than IRES in 2019, although the same surveys still agreed closely on caste and religion."),
        P(class = "why", "Consequence: trends in NFHS 'LPG as main fuel' after 2016 are not a clean measure of how households actually cook, ",
          "and exposure built from it carries an error that changed over exactly the period studied.")),
      lesson("2. Correcting it is possible on average, not precisely",
        P("The correction needs districts where both NFHS and an energy survey exist: ", BB(num(n19, 0)), " districts for 2019 and far fewer for 2015. ",
          "Everywhere else the relationship is borrowed. The 2019 relationship is also noisy: once each survey's own sampling error is accounted for, ",
          "the districts still scatter around the calibration line with a residual SD of ", BB(num(sig19, 2)), " on the logit scale, against ",
          num(sig15, 2), " in 2015. That scatter is <i>disagreement between the instruments</i>, not sampling noise."),
        P(class = "why", "Consequence: corrected district values come with wide intervals, and part of their uncertainty (",
          num(K$nfhs15, 0), "% in 2015, ", num(K$nfhs19, 0), "% in 2019) comes from NFHS's own district samples rather than from the calibration.")),
      lesson("3. NFHS needs better questions, not only outside validation",
        tags$ul(
          tags$li(HTML("<b>All fuels used</b>, how often, and for what (cooking, water and space heating), not just the main one.")),
          tags$li(HTML("<b>When</b> the fuel question refers to: the month or season, so seasonal switching is visible.")),
          tags$li(HTML("<b>LPG use, not just ownership</b>: refills in the past year, Ujjwala status and year of connection.")),
          tags$li(HTML("<b>Local context</b>: refill price paid, distance to the distributor, cattle ownership, main income source.")),
          tags$li(HTML("<b>An embedded validation sub-sample</b> with a detailed fuel module (ideally with stove-use sensors), so the main survey can be calibrated against itself."))),
        P(class = "why", "National surveys are for surveillance. Causal health effects of cooking fuel need measured-exposure cohorts or trials; ",
          "here, carrying the correction's uncertainty into a health model could not be done in a validated way, so the health result stays exploratory.")),
      lesson("4. A stacking variable cannot be built from NFHS",
        P("We trained a model of stacking on the energy surveys' households, using the characteristics all three surveys share (caste, religion, household size, ration card, electricity, wealth), and applied it to NFHS. ",
          "It barely tells stackers from non-stackers (AUC ", auc_rng, ")",
          if (!is.null(loso)) paste0(", and predicting a held-out state ",
                                     if (isTRUE(loso$skill_vs_benchmark <= 0)) "does no better than" else "improves only modestly on",
                                     " the national average (r = ", num(loso$r_state, 2),
                                     "; skill ", signed(loso$skill_vs_benchmark, 2), ")") else "", ". "),
        if (!is.null(ires_vp))
          P("The reason is that stacking is mostly about <i>place</i>: in IRES, ", BB(num(ires_vp$pct_place_total, 0), "%"),
            " of the variation (on the latent logit scale of a multilevel model, not a share of observed outcomes) lies between states, districts and villages, while those household characteristics explain ",
            BB(num(ires_vp$marginal_r2_household_pct, 0), "%"), ". Supply, price and local habit drive stacking, and NFHS records none of them."),
        P(class = "why", "Some household associations point the same way in more than one survey (for example, larger and poorer households stack more), but the stacking relationship as a whole did not transfer reliably between surveys, ",
          "and reported use of solid fuel says nothing about how often or how much it is burned. ",
          actionLink(nav_id("story", "stacking"), "See the stacking analysis \u2192"))))
  )
}

# ==============================================================================
# Step 2: how the two corrections work
# ==============================================================================
ui_corrections <- function(B) {
  tagList(
    P("Both corrections use the districts where NFHS and an energy survey were both fielded as a <b>translation table</b>: ",
      "for a district where NFHS says X% cook mainly with LPG, what does the energy survey say? Both then apply that translation to every district. ",
      "They differ in how much they trust each NFHS number and in whether they keep track of their own uncertainty."),
    layout_columns(col_widths = c(6, 6),
      div(class = "explain-box",
        h5("Regression calibration (simple)"),
        P("Draw the best straight line through the overlap districts (on the log-odds scale, so predictions stay between 0% and 100%) ",
          "and read every district's corrected value off that line. One number in, one number out."),
        P(class = "muted", "Weakness: it treats each NFHS district estimate as exact. Small district samples are noisy, and noise in the input ",
          "flattens the fitted line (regression dilution). It also gives no uncertainty for the corrected value."),
        mj("<p>\\( \\operatorname{logit}\\hat R_d = \\alpha + \\beta\\,\\operatorname{logit}\\hat N_d + e_d \\) (fitted on overlap districts)<br>",
           "\\( \\tilde p_d = \\operatorname{logit}^{-1}\\!\\big(\\hat\\alpha + \\hat\\beta\\,\\operatorname{logit}\\hat N_d\\big) \\)</p>",
           "<p class='muted'>\\(\\hat N_d\\): NFHS district estimate; \\(\\hat R_d\\): energy-survey (reference) estimate.</p>")),
      div(class = "explain-box",
        h5("Bayesian measurement-error correction (primary)"),
        P("The same idea, with three upgrades:"),
        tags$ol(
          tags$li(HTML("<b>Noisy inputs are discounted.</b> Each NFHS district value comes with its standard error; a noisy one is pulled toward its state's average before it is translated, which also stops the line being flattened. The two rounds are shrunk together, because a district's true values in 2015 and 2019 are strongly correlated and what the health analysis uses is the change.")),
          tags$li(HTML("<b>States differ.</b> Each calibrated state gets its own offset; for states no energy survey visited, the offset is drawn from the spread across states, which widens their intervals.")),
          tags$li(HTML("<b>Uncertainty is kept.</b> The model produces thousands of plausible versions of every district's value; the map shows their average and the interval shows their spread."))),
        mj("<p>\\( \\operatorname{logit}\\hat N_d \\sim \\mathcal N(x_d,\\, s_{N,d}^2) \\) &nbsp;(true NFHS value, measured with error)<br>",
           "\\( \\operatorname{logit}\\hat R_d \\sim \\mathcal N(\\mu_d,\\, s_{R,d}^2) \\), &nbsp; \\( \\mu_d = a + b\\,x_d + u_{s(d)} + \\varepsilon_d \\)<br>",
           "\\( u_s \\sim \\mathcal N(0,\\tau^2), \\quad \\varepsilon_d \\sim \\mathcal N(0,\\sigma^2) \\)<br>",
           "Corrected value: posterior draws of \\( \\operatorname{logit}^{-1}(a + b\\,x_d + u_{s(d)}) \\).</p>",
           "<p class='muted'>\\(\\sigma\\) is the instrument disagreement left after sampling error; \\(\\tau\\) the spread of state offsets.</p>"))),
    h5("Try it: translate a district"),
    layout_columns(col_widths = c(4, 8),
      div(radioButtons("cx_yr", NULL, c("2019-21 (NFHS-5 vs IRES)" = "2019", "2015-16 (NFHS-4 vs ACCESS)" = "2015")),
          sliderInput("cx_raw", "A district's raw NFHS value (% cooking mainly with LPG)", 1, 99, 40, step = 1),
          uiOutput("cx_out")),
      plotlyOutput("cx_plot", height = "380px")),
    p(class = "caption", "Lines: the regression-calibration line and the Bayesian line at its posterior-mean parameters for a state with an average offset. ",
      "Dots: every district's raw value against its Bayesian corrected value. They scatter around the line because each district is first pulled toward its state average ",
      "and then shifted by its state's own offset. Dashed: no correction."),
    uiOutput("cx_levels"))
}

corrections_server <- function(input, output, session, B) {
  pars <- reactive({
    yr <- input$cx_yr; leg <- leg_of(yr)
    list(yr = yr, rc = rc_coef(B, yr),
         a = cal_par(B, leg, "intercept"), b = cal_par(B, leg, "slope"))
  })
  output$cx_out <- renderUI({
    z <- pars(); x <- qlogis(input$cx_raw / 100)
    rc <- plogis(z$rc[1] + z$rc[2] * x); by <- plogis(z$a + z$b * x)
    div(class = "readout",
      div(HTML(paste0("Raw NFHS: <b>", input$cx_raw, "%</b>"))),
      div(HTML(paste0("Regression calibration: <b>", pp(rc, 0), "%</b>"))),
      div(HTML(paste0("Bayesian line (average state, before shrinkage): <b>", pp(by, 0), "%</b>"))),
      div(class = "muted", "A real district's Bayesian value also depends on how noisy its NFHS estimate is and on its state's offset."))
  })
  output$cx_plot <- renderPlotly({
    z <- pars(); g <- seq(0.01, 0.99, by = 0.01); x <- qlogis(g)
    d <- B$dist
    pts <- if (!is.null(d)) data.frame(raw = d[[paste0("raw_wt_", z$yr)]], cor = d[[paste0("bayes_", z$yr)]],
                                        nm = paste0(trimws(d$dist_name), ", ", d$state_name)) else NULL
    if (!is.null(pts)) pts <- pts[is.finite(pts$raw) & is.finite(pts$cor), ]
    p <- plotly::plot_ly()
    if (!is.null(pts) && nrow(pts))
      p <- plotly::add_markers(p, data = pts, x = ~100 * raw, y = ~100 * cor, text = ~nm, hoverinfo = "text",
                               marker = list(color = PAL$s2, opacity = 0.25, size = 5), name = "Districts (Bayesian)")
    p <- plotly::add_lines(p, x = 100 * g, y = 100 * g, line = list(color = PAL$muted, dash = "dash", width = 1), name = "No correction")
    if (all(is.finite(z$rc)))
      p <- plotly::add_lines(p, x = 100 * g, y = 100 * plogis(z$rc[1] + z$rc[2] * x), line = list(color = PAL$s1, width = 3), name = "Regression calibration")
    if (is.finite(z$a))
      p <- plotly::add_lines(p, x = 100 * g, y = 100 * plogis(z$a + z$b * x), line = list(color = PAL$s2, width = 3), name = "Bayesian (posterior-mean parameters, average state)")
    p <- plotly::add_markers(p, x = input$cx_raw, y = 100 * plogis(z$rc[1] + z$rc[2] * qlogis(input$cx_raw / 100)),
                             marker = list(color = PAL$s1, size = 12, line = list(color = "white", width = 2)), name = "Your district (RC)", showlegend = FALSE)
    base_layout(p, xaxis = ax("Raw NFHS (% cooking mainly with LPG)", range = c(0, 100)),
                yaxis = ax("Corrected (%)", range = c(0, 100)),
                legend = list(orientation = "h", y = -0.2))
  })
  output$cx_levels <- renderUI({
    yr <- input$cx_yr
    rows <- list(c("Raw NFHS, design-weighted", "Raw design-weighted"),
                 c("Regression calibration", "Reg.-calibrated (design-wt input)"),
                 c("Bayesian correction", "Bayesian-corrected"))
    tags$table(class = "table table-sm", style = "max-width: 520px",
      tags$thead(tags$tr(tags$th(paste0("Average across districts, ", yr)), tags$th("% cooking mainly with LPG"))),
      tags$tbody(lapply(rows, function(r) tags$tr(tags$td(r[1]), tags$td(num(mix_lev(B, yr, r[2]), 1))))))
  })
}

# ==============================================================================
# Step 3: CR1 vs CR2
# ==============================================================================
ui_cr <- function(B, K) {
  f <- K$inf_fix
  ok <- !is.null(f) && nrow(f) && all(c("se", "se_cr2", "df_satt", "p", "p_cr2") %in% names(f))
  tagList(
    P("Districts in the same state share policies, prices and survey teams, so they are not independent pieces of evidence. ",
      "Clustered standard errors handle this by letting the data say how much the ", K$n_clus, " states disagree with the fitted relationship. ",
      "The question is how well they do it with only ", K$n_clus, " states of very different sizes."),
    layout_columns(col_widths = c(6, 6),
      div(class = "explain-box",
        h5("CR1: the conventional clustered standard error"),
        P("Measures each state's leftover disagreement (its residuals) and adds them up, with a small-sample multiplier. ",
          "Two problems with few clusters: the residuals of a fitted line are <i>too small</i>, because each state pulls the line toward itself, most of all the large, ",
          "influential states; and the test is referred to a t distribution with hundreds of degrees of freedom (critical value about 1.96), ",
          "as if there were hundreds of independent pieces of evidence rather than about ", K$n_clus, ".")),
      div(class = "explain-box",
        h5("CR2 with Satterthwaite degrees of freedom (primary)"),
        P("Inflates each state's residuals by how much that state pulled the line toward itself (its leverage), which undoes most of the shrinkage. ",
          "It then asks how many <i>effective</i> clusters the estimate really rests on. When a few states carry most of the information, that number is small, ",
          "and the critical value grows accordingly."))),
    mj("<p>Both use the cluster-robust sandwich \\( \\hat V = (X^\\top W X)^{-1}\\Big[\\sum_{g=1}^{G} X_g^\\top W_g\\,\\tilde e_g \\tilde e_g^\\top W_g X_g\\Big](X^\\top W X)^{-1} \\) and differ in \\(\\tilde e_g\\):</p>",
       "<p>CR1: \\( \\tilde e_g = \\sqrt{c}\\,e_g,\\; c = \\tfrac{G}{G-1}\\tfrac{n-1}{n-k} \\), test against \\( t_{n-k} \\).<br>",
       "CR2: \\( \\tilde e_g = (I - H_{gg})^{-1/2} e_g,\\; H_{gg} = X_g (X^\\top W X)^{-1} X_g^\\top W_g \\), test against \\( t_{\\nu} \\) with Satterthwaite \\( \\nu \\).</p>"),
    if (ok) tags$table(class = "table table-sm", style = "max-width: 640px",
      tags$thead(tags$tr(tags$th("Infant estimate, corrected surface held fixed"), tags$th("CR1 (conventional)"), tags$th("CR2 (primary)"))),
      tags$tbody(
        tags$tr(tags$td("Standard error"), tags$td(num(f$se[1], 4)), tags$td(num(f$se_cr2[1], 4))),
        tags$tr(tags$td("Degrees of freedom"), tags$td("residual (hundreds)"), tags$td(num(f$df_satt[1], 1))),
        tags$tr(tags$td("5% critical value"), tags$td("about 1.96"), tags$td(num(qt(0.975, f$df_satt[1]), 2))),
        tags$tr(tags$td("p"), tags$td(pfmt(f$p[1])), tags$td(pfmt(f$p_cr2[1]))))),
    P("Does it matter? We simulated data with this exact design and <b>no</b> true association. A trustworthy 5% test rejects about 5% of the time:"),
    sim_table(B),
    p(class = "caption", "Same districts, covariates, weights and state clusters; ",
      { s <- B$simcal; n <- if (!is.null(s)) suppressWarnings(as.numeric(s$replications[1])) else NA
        if (is.finite(n)) paste0(format(n, big.mark = ","), " simulated datasets. ") else "" },
      "CR2 comes closest to 5%, which is why it is the primary rule",
      { s <- B$simcal; c2 <- if (!is.null(s)) s[grepl("CR2", s$rule), , drop = FALSE] else NULL
        r <- if (!is.null(c2) && nrow(c2)) c2$rejection_rate_at_nominal_05[1] else NA
        if (is.finite(r) && r > 0.05 + 2 * sqrt(0.05 * 0.95 / c2$replications[1]))
          paste0(", but in this simulation it still rejected ", pp(r, 1), "% of the time rather than 5% (Monte Carlo SE ", num(100 * sqrt(r * (1 - r) / c2$replications[1]), 1), " points at that rate): much better than the conventional rule, not fully calibrated. Its p-values were somewhat optimistic in this simulation; read them as approximate.")
        else "." }))
}

# ==============================================================================
# Step 1: cluster averaging, labelled
# ==============================================================================
SCRIPT_LAB <- c("01_prep_nfhs" = "NFHS (both rounds; all and rural)", "02_prep_access" = "ACCESS (both waves)",
                "03_prep_ires" = "IRES", "08_si_benchmarks" = "NFHS composition benchmarks",
                "09_season_sensitivity" = "NFHS-5 season restrictions", "25_prep_nsso78" = "NSS 78th round",
                "H1_prep_mortality" = "NFHS mortality-model covariates", "H4_si_adult_health" = "NFHS adult health")
marg_summary <- function(B) {
  m <- B$marg
  if (is.null(m)) return(NULL)
  m$shift <- 100 * (m$mean_marginal - m$mean_conditional)
  keep <- m$outcome %in% c("lpg", "main_fuel_lpg", "stack_binary")
  m <- m[keep, ]
  g <- split(m, paste(m$script, m$outcome, sep = "|"))
  rows <- lapply(g, function(z) data.frame(src = { l <- unname(SCRIPT_LAB[z$script[1]]); if (is.na(l)) z$script[1] else l }, outcome = z$outcome[1], n = nrow(z),
                                            sd = paste0(num(min(z$sd_cluster), 2), if (nrow(z) > 1) paste0("-", num(max(z$sd_cluster), 2)) else ""),
                                            sh = paste0(signed(min(z$shift)), if (nrow(z) > 1) paste0(" to ", signed(max(z$shift))) else "")))
  r <- do.call(rbind, rows)
  out_lab <- c(lpg = "LPG main fuel", main_fuel_lpg = "LPG main fuel", stack_binary = "Stacking (LPG users)")
  tagList(
    P("District values from the multilevel model need one more choice. The model gives each cluster (village or census block) its own level. ",
      "The value for a <i>typical</i> cluster (the model's linear predictor with the cluster effect set to zero) is not the same as the share of ",
      "<i>all</i> the district's households, which averages over clusters: because the logistic curve is S-shaped, averaging pulls the value toward 50%. ",
      "We report the cluster-averaged share. The <b>shift</b> below is cluster-averaged minus typical-cluster, averaged over districts:"),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th("Survey"), tags$th("Outcome"), tags$th("Model fits"), tags$th("Cluster SD (logit)"), tags$th("Shift (pp)"))),
      tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(
        tags$td(r$src[i]), tags$td(out_lab[[r$outcome[i]]]), tags$td(r$n[i]), tags$td(r$sd[i]), tags$td(r$sh[i]))))),
    p(class = "caption", "Several fits per survey (rounds, all vs rural). The full list, including covariates, is in glmer_cluster_marginalisation.csv in the repository's results folder."))
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
