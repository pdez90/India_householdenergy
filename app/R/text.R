# ==============================================================================
# text.R -- the narrative. Every sentence that states a result is built from the
# bundle (K = derive(B)), and sentences whose direction could change between
# runs are conditional on the numbers, so the app never asserts something the
# current results do not show.
# ==============================================================================
library(htmltools)

# Inline text is assembled as one HTML string, so no stray spaces appear
# around bold numbers or punctuation.
P  <- function(..., class = NULL) tags$p(class = class, HTML(paste0(...)))
BB <- function(...) paste0("<b>", ..., "</b>")

key_number <- function(value, label, sub = NULL) {
  div(class = "keynum",
      div(class = "keynum-value", value),
      div(class = "keynum-label", label),
      if (!is.null(sub)) div(class = "keynum-sub", sub))
}

in_short <- function(...) div(class = "in-short", tags$span(class = "eyebrow", "In short"), ...)

# ---- Landing ------------------------------------------------------------------
story_intro <- function(K) {
  tagList(
    p(class = "lede",
      "India's National Family Health Survey (NFHS) is the main source of district-level data on ",
      "health and household conditions, including how families cook. Between the 2015-16 and ",
      "2019-21 rounds, the government's LPG connection programme reached tens of millions of homes. ",
      "This site walks through what happened to the survey's cooking-fuel measure over that period, ",
      "how we corrected it, and what the correction means for questions about health."),
    P("Each step has a plain-language summary first. Open the ",
      BB("How we know"), " panels for the evidence and the ",
      BB("Technical details"), " panels for methods.")
  )
}

# ---- Step 1 -------------------------------------------------------------------
step1_short <- function(K) {
  in_short(
    P("In 2015, the health survey (NFHS-4) and a specialised energy survey (ACCESS) gave almost the same ",
      "picture of rural cooking fuel in the ", K$n15, " districts both covered: the average gap was ",
      BB(signed(K$d15), " percentage points"), " and district values tracked each other closely (r = ",
      num(K$r15), ")."),
    P("By 2019 the picture had changed. In the ", K$n19, " districts NFHS-5 shared with the IRES energy survey, ",
      "NFHS-5 recorded ", BB(num(abs(K$d19), 1), " points ", if (K$d19 < 0) "less" else "more",
      " LPG use"), " on average, and the district values agreed less well (r = ", num(K$r19), ")."),
    P(class = "why", "Why it matters: if a survey measures something differently in two rounds, ",
      "the change between rounds mixes real change with measurement change.")
  )
}

falsification_text <- function(B) {
  s <- B$side
  s19 <- s[s$pair == "NFHS5_IRES" & s$variable != "lpg", ]
  big <- s19[abs(s19$mean_diff_pp) >= 5, ]
  lpg19 <- s$mean_diff_pp[s$pair == "NFHS5_IRES" & s$variable == "lpg"]
  tagList(
    p("If the two 2019 surveys had simply reached different households, they should disagree on ",
      "other characteristics too. The chart compares the same districts on characteristics both surveys ask about."),
    if (nrow(big) == 0)
      P("Every other characteristic agrees within 5 percentage points on average, against a gap of ",
        num(abs(lpg19), 1), " points for LPG.")
    else
      P("Most other characteristics agree within 5 points on average, against ", num(abs(lpg19), 1),
        " points for LPG. The exception", if (nrow(big) > 1) "s are " else " is ",
        paste(sprintf("%s (%s points)", VAR_LAB_TXT[big$variable], signed(big$mean_diff_pp)), collapse = " and "),
        ". ", if (any(big$variable == "bpl"))
          "Ration-card status is itself linked to the welfare programmes that distributed LPG connections, so it is a weaker check than caste, religion and the other composition variables."),
    if (any(s19$variable == "electricity"))
      P(class = "muted", "Electricity access agrees on average but only weakly district by district, and it is also programme-linked, so it is not a clean benchmark either; caste and religion are the cleanest checks.")
  )
}

VAR_LAB_TXT <- c(lpg = "cooking mainly with LPG", sc = "Scheduled Caste share", st = "Scheduled Tribe share",
                 scst = "SC or ST share", hindu = "Hindu share", muslim = "Muslim share",
                 electricity = "electricity access", bpl = "holding a BPL/Antyodaya ration card")

season_note <- function(B) {
  s <- B$season
  if (is.null(s) || nrow(s) < 2) return(NULL)
  g <- 100 * s$mean_diff
  P(class = "muted", "Across these restrictions the average gap ranges from ", signed(min(g)), " to ",
    signed(max(g)), " points, against ", signed(g[1]), " with all interviews.")
}

season_within <- function(B) {
  s <- B$seas_nfhs
  if (is.null(s) || !nrow(s)) return(NULL)
  s <- s[s$specification == "district fixed effects + linear time trend" | !any(s$specification == "district fixed effects + linear time trend"), ]
  tagList(
    P("Does the answer itself shift with the season? Comparing clusters of the same district interviewed in different seasons ",
      "(with a linear trend in interview date):"),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th("Survey"), tags$th("Season vs winter"), tags$th("Difference (pp)"), tags$th("95% interval"), tags$th("p"), tags$th("Districts used"))),
      tags$tbody(lapply(seq_len(nrow(s)), function(i) tags$tr(
        tags$td(sub("^NFHS(\\d)$", "NFHS-\\1", s$survey[i])), tags$td(s$term[i]), tags$td(signed(s$pp_vs_winter[i])),
        tags$td(paste0(signed(s$pp_vs_winter[i] - 1.96 * s$se_pp[i]), " to ", signed(s$pp_vs_winter[i] + 1.96 * s$se_pp[i]))),
        tags$td(pfmt(s$p[i])), tags$td(s$n_districts_multi_season[i]))))),
    p(class = "caption", if (all(s$p >= 0.05, na.rm = TRUE)) "No seasonal difference is statistically detectable at the 5% level, but the intervals allow differences of a few points. "
                         else "Some seasonal differences reach the 5% level; see the table. ",
      "Only districts interviewed in more than one season contribute, and season is partly entangled with fieldwork phase."))
}

season_table <- function(B) {
  s <- B$season
  if (is.null(s)) return(NULL)
  tags$table(class = "table table-sm",
    tags$thead(tags$tr(tags$th("NFHS-5 interviews used"), tags$th("Districts"), tags$th("r"), tags$th("Average gap (pp)"))),
    tags$tbody(lapply(seq_len(nrow(s)), function(i) tags$tr(
      tags$td(s$variant[i]), tags$td(s$n_districts[i]), tags$td(num(s$pearson[i])),
      tags$td(signed(100 * s$mean_diff[i]))))))
}

marg_text <- function(B) {
  m <- B$marg
  if (is.null(m)) return(p(class = "muted",
    "District prevalences from the multilevel model are averaged over the survey's clusters ",
    "(villages or census blocks) within each district. A per-survey record of how much this matters ",
    "will appear here after the next results update."))
  m$diff <- 100 * (m$mean_marginal - m$mean_conditional)
  tagList(
    p("District prevalences from the multilevel model are the share of the district's households, ",
      "averaged over its clusters, not the value for a 'typical' cluster. The two differ on the logit scale, ",
      "and by how much depends on how clustered each survey is:"),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th("Script"), tags$th("Outcome"), tags$th("Cluster SD (logit)"),
                         tags$th("Average shift (pp)"))),
      tags$tbody(lapply(seq_len(nrow(m)), function(i) tags$tr(
        tags$td(m$script[i]), tags$td(m$outcome[i]), tags$td(num(m$sd_cluster[i])),
        tags$td(signed(m$diff[i])))))))
}

# ---- Step 2 -------------------------------------------------------------------
step2_short <- function(K) {
  in_short(
    p("We used the districts covered by both surveys to learn how NFHS answers line up with the ",
      "specialised surveys' answers, and applied that relationship to every district. Each corrected ",
      "value comes with a range that reflects how uncertain the correction is."),
    P("Averaged across districts (each district counted equally, so this is not national household prevalence), rural LPG use in 2019-21 moves from ", BB(pp(K$raw19, 0), "%"),
      " in the raw NFHS data to ", BB(pp(K$cor19, 0), "%"), " after correction (2015-16: ",
      pp(K$raw15, 0), "% to ", pp(K$cor15, 0), "%)."),
    P(class = "why", "A caution: in 2019, ", pp(K$ext19, 0), "% of districts (and ", pp(K$ext15, 0),
      "% in 2015) are in states outside that round's calibration sample. Their state-level adjustment is ",
      "borrowed from the spread across states, so read those district values with care. ",
      "Use the toggle to grey them out.")
  )
}

loso_text <- function(K) {
  if (!is.finite(K$loso_n)) return(NULL)
  tagList(
  P("Leave-one-state-out test: we refitted the ", BB("regression calibration"), " without each state in turn and checked it ",
    "against that state's energy-survey values. It moved NFHS closer to the energy survey in ",
    BB(K$loso_impr, " of ", K$loso_n, " states"), ", and the district-weighted root-mean-square error (logit scale) fell from ",
    num(K$loso_rmse_raw, 2), " to ", num(K$loso_rmse_cor, 2), " (", num(K$loso_rmse, 0), "%)."),
  P(class = "muted", "This tests the simpler regression calibration only. The Bayesian correction and its intervals were not refitted state by state, ",
    "so the test does not validate them, and it says nothing about states that no energy survey visited."))
}

calib_table <- function(B) {
  c <- B$calib
  if (is.null(c)) return(NULL)
  c <- c[c$parameter %in% c("slope", "intercept", "sd_state", "sd_residual", "n_districts"), ]
  lab <- c(slope = "Slope (logit scale)", intercept = "Intercept", sd_state = "Between-state SD",
           sd_residual = "Residual SD (instrument disagreement)", n_districts = "Districts")
  tags$table(class = "table table-sm",
    tags$thead(tags$tr(tags$th("Calibration"), tags$th("Parameter"), tags$th("Estimate"), tags$th("95% interval"))),
    tags$tbody(lapply(seq_len(nrow(c)), function(i) tags$tr(
      tags$td(c$calibration[i]), tags$td(lab[c$parameter[i]]),
      tags$td(if (c$parameter[i] == "n_districts") c$estimate[i] else num(c$estimate[i])),
      tags$td(if (is.finite(c$lo95[i])) paste(num(c$lo95[i]), "to", num(c$hi95[i])) else "")))))
}

# ---- Step 3 -------------------------------------------------------------------
step3_short <- function(K) {
  f <- K$inf_fix; m <- K$inf_mi
  has_m <- !is.null(m) && nrow(m) > 0
  dir <- function(e) if (e < 0) " fewer" else " more"
  in_short(
    P("Does the corrected exposure change what we learn about health? We asked whether districts where ",
      "LPG use rose more also saw infant and newborn mortality fall more between the two rounds (",
      K$n_health, " rural districts). This is an ", BB("exploratory illustration"), ", not an estimate of the health benefit of LPG."),
    P("Holding the corrected exposure fixed at its best estimate, a 10-point rise in LPG use goes with ",
      BB(num(abs(per10(f)), 2), dir(f$estimate), " infant deaths per 100 births"), " (p = ", pfmt(f$p_cr2),
      " under the small-cluster rule). That result is ", BB("conditional on the fitted correction"),
      ": its interval leaves out the uncertainty in the correction itself.",
      if (has_m) paste0(" Adding that uncertainty by drawing many versions of the corrected exposure gives ",
                        BB(num(abs(per10(m)), 2), dir(m$estimate)), " (p = ", pfmt(m$p_cr2), "), but this method ignores the mortality data when it draws the exposure, which tends to pull the estimate toward zero, so it is shown only as a sensitivity check.") else ""),
    P(class = "why", "Why no better answer? The standard fix draws the exposure in a way that is consistent with the mortality data as well. ",
      "We tried two versions; neither passed its convergence checks, and we stopped under a rule set in advance. ",
      "And with only ", K$n_clus, " states, the usual 'clustered' standard errors are too optimistic",
      if (is.finite(K$sim_conv)) paste0(": in simulations where the true association was zero, they declared it significant ",
                                        pp(K$sim_conv, 0), "% of the time instead of 5%") else "",
      ", so the results use a small-cluster correction (CR2)",
      if (is.finite(K$sim_cr2)) paste0(". CR2 is better but not perfect: it still rejected a true null ", pp(K$sim_cr2, 1),
                                       "% of the time, so its p-values are approximate") else "",
      ".")
  )
}

sim_table <- function(B) {
  s <- B$simcal
  if (is.null(s)) return(NULL)
  tags$table(class = "table table-sm",
    tags$thead(tags$tr(tags$th("Inference rule"), tags$th("Rejects a true null (nominal 5%)"), tags$th("Simulations"))),
    tags$tbody(lapply(seq_len(nrow(s)), function(i) tags$tr(
      tags$td(s$rule[i]), tags$td(paste0(pp(s$rejection_rate_at_nominal_05[i]), "%")), tags$td(s$replications[i])))))
}

# ---- Step 4 -------------------------------------------------------------------
step4_short <- function(K) {
  in_short(
    P("A corrected district value is an estimate, and its uncertainty has two sources. One is the ", BB("calibration"),
      ": the relationship between NFHS and the energy survey, learned from the districts where both exist. ",
      "The other is the ", BB("health survey itself"), ": the NFHS district estimate the calibration is applied to comes from a modest sample."),
    P("The health-survey part is ", BB(num(K$nfhs15, 0), "%"), " of the variance for the 2015 correction and ",
      BB(num(K$nfhs19, 0), "%"), " for 2019. The rest comes from the calibration, which is much noisier in 2019 because NFHS-5 and IRES disagree district by district by more than sampling error explains.",
      if (isFALSE(K$decomp_ok)) " (In this run the decomposition failed an internal check, so treat these shares as rough.)" else ""),
    P(class = "why", "Why it matters: anyone using a corrected exposure map inherits both kinds of uncertainty. ",
      "Holding the map fixed ignores the calibration part; this split is descriptive and does not say what a different validation survey would achieve.")
  )
}

failed_methods_note <- function() {
  tagList(
    P("The usual way to carry a corrected exposure's uncertainty into a health model is ", BB("multiple imputation"),
      ": rerun the analysis on many plausible versions of the exposure and pool the results. Drawing those versions from the correction alone ",
      "(the ", BB("exposure-only"), " imputation shown here) adds district-level noise unrelated to mortality, which tends to pull the association toward zero. ",
      "The standard remedy draws them conditional on the mortality data too."),
    P("We built that remedy twice. The first version's chains did not agree (some explored only two of thousands of calibration draws). ",
      "Before the second we fixed a specification, a pilot on simulated data and a stopping rule. Including state dependence substantially improved mixing in the pilot, but convergence remained inadequate: ",
      "about two-thirds of the pilot datasets mixed against the 95% required, and tuning did not fix it. Under the stopping rule we stopped."),
    P(class = "why", "So the health result is shown two ways, neither validated: the fixed corrected surface (conditional on the correction) and the exposure-only imputation (a sensitivity check at risk of attenuation). ",
      "The survey comparison, the correction and the stacking results do not depend on any of this."))
}

# ---- Glossary -----------------------------------------------------------------
GLOSSARY <- list(
  c("NFHS", "National Family Health Survey (India's Demographic and Health Survey). Rounds 4 (2015-16) and 5 (2019-21)."),
  c("ACCESS", "Access to Clean Cooking Energy and Electricity Survey of States: a specialised household energy survey in six states (2014-15, 2018)."),
  c("IRES", "India Residential Energy Survey (2019-20), 21 states."),
  c("Primary LPG", "The household cooks mainly with liquefied petroleum gas."),
  c("Design-weighted", "A direct estimate using the survey's own sampling weights."),
  c("Multilevel estimate", "A model-based estimate that borrows strength from the rest of the state for small district samples."),
  c("Calibration / correction", "Learning how one survey's answers map onto another's where both exist, then applying that mapping elsewhere."),
  c("CR2", "A small-sample correction to clustered standard errors (Bell-McCaffrey), with Satterthwaite degrees of freedom."),
  c("Multiple imputation", "Repeating the health analysis over many plausible versions of the corrected exposure and combining the results."),
  c("Exposure-only imputation", "Multiple imputation that draws the exposure from the correction alone, without the mortality data; shown as a sensitivity check because it tends to pull associations toward zero."),
  c("Fixed surface", "The corrected exposure held at its best estimate; results are conditional on the fitted correction.")
)
