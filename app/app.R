# ==============================================================================
# India household cooking fuel: survey disagreement, correction, stacking and an exploratory health illustration
# A guided walkthrough of the analysis, built on the published aggregate results.
#
#   Run locally (from this folder):   shiny::runApp()
#   Refresh after a new pipeline run: Rscript prepare_data.R
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly); library(htmltools)
})
for (f in list.files("R", full.names = TRUE)) source(f, local = TRUE, encoding = "UTF-8")

B <- load_bundle()
K <- derive(B)
MP <- map_paths(B$geo)

theme <- bs_theme(
  version = 5, bg = "#fcfcfb", fg = "#0b0b0b", primary = "#2a78d6", secondary = "#52514e",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  "border-radius" = "0.5rem")

step_header <- function(n, title, sub) div(class = "step-header",
  span(class = "step-num", n), div(h2(title), p(class = "step-sub", sub)))

how_we_know <- function(...) accordion(open = FALSE, multiple = TRUE, ...)

# Every navigation control gets a unique id "nav__<from>__<to>"; the server
# routes any input with that prefix to the target page.
NAV_IDS <- character()
nav_id <- function(from, to) { id <- paste0("nav__", from, "__", to); NAV_IDS <<- c(NAV_IDS, id); id }
nav_buttons <- function(page, prev = NULL, nxt = NULL) div(class = "step-nav",
  if (!is.null(prev)) actionButton(nav_id(page, prev[1]), paste("\u2190", prev[2]), class = "btn-outline-secondary"),
  if (!is.null(nxt))  actionButton(nav_id(page, nxt[1]),  paste(nxt[2], "\u2192"), class = "btn-primary"))

run_mix_note <- function(B) if (length(B$run_other)) tagList(
  " The exported files combine runs: everything comes from run ", tags$code(B$run_main),
  " except the checks of ", paste(vapply(B$run_other, function(o)
    paste0("script ", o$scripts, " (", o$n, " checks, targeted rerun ", o$id, ")"), ""), collapse = "; "),
  ", which do not feed any result shown in this app.") else NULL
provisional_banner <- div(class = "runinfo",
  "Results from pipeline run ", tags$code(B$run_main), " (exported ", B$run_date, ").", run_mix_note(B))

ui <- page_navbar(
  id = "nav", theme = theme, title = "Cooking fuel & health in India",
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(HTML("Shiny.addCustomMessageHandler('scrolltop', function(x){ window.scrollTo(0,0); });")), withMathJax()),
  fillable = FALSE,

  # ---- Landing ---------------------------------------------------------------
  nav_panel("The story", value = "story",
    div(class = "page", provisional_banner,
      h1("When a survey question stops meaning the same thing"),
      story_intro(K),
      layout_columns(col_widths = c(3, 3, 3, 3), class = "story-cards",
        card(class = "story-card", card_header(span(class = "step-num sm", "1"), "The surveys disagree"),
             key_number(paste0(signed(K$d19, 0), " pp"), "NFHS-5 vs IRES, rural LPG, 2019",
                        paste0("2015: ", signed(K$d15, 1), " pp")),
             actionLink(nav_id("story", "s1"), "See the evidence \u2192")),
        card(class = "story-card", card_header(span(class = "step-num sm", "2"), "We correct it"),
             key_number(paste0(pp(K$raw19, 0), "% \u2192 ", pp(K$cor19, 0), "%"),
                        "Rural LPG use, 2019-21, raw \u2192 corrected (unweighted mean across districts)"),
             actionLink(nav_id("story", "s2"), "See the correction \u2192")),
        card(class = "story-card", card_header(span(class = "step-num sm", "3"), "Health: exploratory only"),
             key_number(paste0(signed(per10(K$inf_fix), 2)),
                        paste0("Infant deaths per 100 births per 10-pp rise in LPG, corrected surface held fixed; 95% CI (CR2) ",
                               signed(10 * K$inf_fix$ci_cr2_low, 2), " to ", signed(10 * K$inf_fix$ci_cr2_high, 2)),
                        "conditional on the correction; p-values approximate (see step 3)"),
             actionLink(nav_id("story", "s3"), "See the health analysis \u2192")),
        card(class = "story-card", card_header(span(class = "step-num sm", "4"), "Where the uncertainty comes from"),
             key_number(paste0(num(K$nfhs19, 0), "%"),
                        "of 2019 corrected-exposure uncertainty comes from the health survey's own district samples",
                        paste0("2015: ", num(K$nfhs15, 0), "%")),
             actionLink(nav_id("story", "s4"), "See the breakdown \u2192"))
      ),
      div(class = "start", actionButton(nav_id("story", "s1b"), "Start the walkthrough \u2192", class = "btn-primary btn-lg")),
      story_lessons(B, K)
    )),

  # ---- Step 1 ----------------------------------------------------------------
  nav_panel("1 \u00b7 Surveys disagree", value = "s1",
    div(class = "page",
      step_header("1", "Two surveys, one question, diverging answers",
                  "Do the health survey and specialised energy surveys agree on how rural India cooks?"),
      layout_columns(col_widths = c(5, 7),
        step1_short(K),
        card(card_header(div(class = "card-head-row", "District by district",
               radioButtons("era", NULL, c("2019" = "2019", "2015" = "2015"), inline = TRUE))),
             plotlyOutput("p_agree", height = "430px"),
             p(class = "caption", "Each dot is a district both surveys covered (rural households, design-weighted). ",
               "Dots below the dashed line: the health survey reports less LPG use than the energy survey."))),
      how_we_know(
        accordion_panel("How we know it is the cooking-fuel question, not the sample",
          layout_columns(col_widths = c(5, 7), falsification_text(B), plotlyOutput("p_falsify", height = "360px"))),
        accordion_panel("How we know it is not timing or season", season_table(B), season_note(B), season_within(B)),
        accordion_panel("State by state (2019)", tableOutput("t_state")),
        accordion_panel("Technical details: estimators",
          p("The headline gap, the district scatter and the composition check use design-weighted direct estimates on both sides (rural households, same districts). ",
            "The season and state tables use the model-based (multilevel) rural comparison, the estimator of the manuscript's comparison table; ",
            "the composition-check gaps therefore differ somewhat from the manuscript's benchmark table, which uses its own estimator and frame."),
          marg_summary(B))),
      nav_buttons("s1", c("story", "Overview"), c("s2", "Next: the correction")))),

  # ---- Step 2 ----------------------------------------------------------------
  nav_panel("2 \u00b7 Correction", value = "s2",
    div(class = "page",
      step_header("2", "Correcting the exposure, with its uncertainty",
                  "Learn the relationship where both surveys exist; apply it everywhere; keep track of how sure we are."),
      layout_columns(col_widths = c(4, 8),
        div(step2_short(K),
            card(card_header("Map controls"),
                 selectInput("mapvar", "Show", names(MAP_VARS), selected = "Corrected, 2019-21"),
                 checkboxInput("grey", "Grey out districts in states outside the calibration sample", FALSE),
                 p(class = "caption", "Click a district to open it in 'Explore a district'."))),
        card(card_header("Rural households cooking mainly with LPG"),
             uiOutput("p_map"))),
      how_we_know(
        accordion_panel("How the two corrections work (intuition and maths)", ui_corrections(B)),
        accordion_panel("Does the calibration travel to new states?", loso_text(K)),
        accordion_panel("Raw versus corrected, district by district",
          radioButtons("rc_yr", NULL, c("2019" = "2019", "2015" = "2015"), inline = TRUE),
          plotlyOutput("p_rvc", height = "420px"),
          p(class = "caption", "Points above the dashed line were raised by the correction. Orange: districts whose state was in the calibration sample.")),
        accordion_panel("Technical details: the calibration model",
          p("For each round, the energy survey's district estimate (logit scale) is modelled as a linear function of the true NFHS district value plus a state effect and residual. ",
            "The NFHS value enters as a covariate measured with error (its own design-based standard error), and the energy survey's own sampling error is a known variance. ",
            "Before prediction, each NFHS district value is shrunk toward its state mean (empirical Bayes), so a noisy district estimate is not taken at face value. ",
            "Corrected values are posterior draws; the map shows the back-transformed posterior mean of the logit (not the mean of the probability draws) and the interval width."),
          calib_table(B),
          p(actionLink(nav_id("s2", "h_models"), "All parameters, sampler diagnostics and the state-by-state check \u2192"), br(),
            actionLink(nav_id("s2", "h_surfaces"), "Compare regression calibration and the Bayesian correction district by district \u2192")))),
      nav_buttons("s2", c("s1", "Back"), c("s3", "Next: health")))),

  # ---- Step 3 ----------------------------------------------------------------
  nav_panel("3 \u00b7 Health", value = "s3",
    div(class = "page",
      step_header("3", "An exploratory health illustration",
                  "District change in LPG use versus change in child mortality, 2015-16 to 2019-21. Not an estimate of the health benefit of LPG."),
      layout_columns(col_widths = c(5, 7),
        step3_short(K),
        card(card_header(div(class = "card-head-row", "Estimated association",
               div(class = "controls",
                 radioButtons("outcome", NULL, c("Infant" = "infant", "Neonatal" = "neonatal"), inline = TRUE),
                 radioButtons("rule", NULL, c("Small-cluster (CR2)" = "cr2", "Conventional" = "conv"), inline = TRUE)))),
             plotlyOutput("p_forest", height = "360px"),
             p(class = "caption", "Dots are estimates, lines are 95% intervals. Blue rows hold the exposure fixed: the corrected rows are conditional on the correction, ",
               "and the raw NFHS row is shown for context only (it is attenuated by the very error the correction addresses). ",
               "Orange: exposure-only imputation, a sensitivity check that tends to pull estimates toward zero. Switch the inference rule to see how much the interval depends on it.",
               if (is.finite(K$sim_cr2)) paste0(" Even the small-cluster rule (CR2) rejected a true null in ", pp(K$sim_cr2, 1),
                                                "% of simulated datasets against 5%, so read its p-values as approximate.") else ""))),
      how_we_know(
        accordion_panel("Why the health result is only exploratory", failed_methods_note()),
        accordion_panel("CR1 vs CR2: why the conventional standard errors are too optimistic here", ui_cr(B, K)),
        accordion_panel("Technical details: the health model",
          p("District change in neonatal or infant deaths per 100 births regressed on the change in primary-LPG prevalence (percentage points), ",
            "adjusted for changes in the share of poor households, the share of women aged 15-49 with no schooling, electricity, the Muslim share, sanitation and water, ",
            "and in ambient PM2.5, temperature, relative humidity and drought, with region fixed effects; ",
            "weighted by the harmonic mean across the two rounds of the district's outcome-eligible births (unweighted as a sensitivity); standard errors clustered by state. ",
            "Mortality is estimated from full birth histories with completed-interval censoring.")),
        accordion_panel("Main specifications (adjusted, birth-weighted, all districts)", tableOutput("t_health"),
          p(actionLink(nav_id("s3", "h_health"), "Open the sensitivity explorer (all populations, weightings, support strata and estimands) \u2192")))),
      nav_buttons("s3", c("s2", "Back"), c("s4", "Next: where the uncertainty comes from")))),

  # ---- Step 4 ----------------------------------------------------------------
  nav_panel("4 \u00b7 Uncertainty", value = "s4",
    div(class = "page",
      step_header("4", "Where the corrected exposure's uncertainty comes from",
                  "Part belongs to the calibration, part to the health survey's own district estimates."),
      layout_columns(col_widths = c(5, 7),
        step4_short(K),
        card(card_header("Share of each corrected value's posterior variance"),
             plotlyOutput("p_var", height = "250px"),
             p(class = "caption", "Averaged over districts, logit scale. Descriptive: it apportions the uncertainty of the correction as fitted."))),
      how_we_know(
        accordion_panel("Technical details: the decomposition",
          p("Each corrected value is a + b x + u_s, where x is the NFHS district estimate entering the calibration as a covariate measured with known error. ",
            "Its posterior variance splits into a calibration-side part (intercept, slope, state effect) and an NFHS-side part, b squared times the posterior variance of the district's true NFHS value. ",
            "The calibration-side part is reconstructed in closed form and checked against the fitted models; details are in the manuscript's Supplementary Methods S4."))),
      nav_buttons("s4", c("s3", "Back"), c("explore", "Explore a district")))),

  # ---- Explore ---------------------------------------------------------------
  nav_panel("Explore a district", value = "explore",
    div(class = "page",
      h2("Explore a district"),
      layout_columns(col_widths = c(4, 8),
        card(selectizeInput("district", "District", choices = district_choices(B), selected = "505",
                            options = list(placeholder = "Type a district name")),
             uiOutput("district_card")),
        card(card_header("Rural LPG use, raw and corrected, with 95% intervals"),
             plotlyOutput("p_district", height = "380px"))))),

  # ---- Under the hood -------------------------------------------------------------
  nav_panel("Fuel stacking", value = "stacking", ui_stacking(B)),
  nav_menu("Under the hood", value = "hood",
    nav_panel("Correction models", value = "h_models", ui_models(B)),
    nav_panel("Compare surfaces", value = "h_surfaces", ui_surfaces(B)),
    nav_panel("Health sensitivity explorer", value = "h_health", ui_health(B)),
    nav_panel("Run checks", value = "h_checks", ui_checks(B))),

  # ---- Data & methods ------------------------------------------------------------
  nav_panel("Data & methods", value = "data",
    div(class = "page",
      h2("Data, methods and provenance"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Provenance"),
             p("Pipeline run: ", tags$code(B$run_id), run_mix_note(B)), p("Generated: ", B$run_date),
             p("Every number and chart on this site is computed from the aggregate result files listed below, except the narrative description of the abandoned outcome-conditioned imputation, ",
               "which summarises the project's records rather than any exported result. ",
               "This app contains and distributes no household records, GPS coordinates or cluster identifiers."),
             p("Code: ", tags$a(href = "https://github.com/pdez90/India_householdenergy", target = "_blank",
                                 "github.com/pdez90/India_householdenergy")),
             p(class = "muted", "District boundaries: 2011 Census district polygons from SHRUG (Development Data Lab; Asher, Lunt, Matsuura and Novosad 2021, ",
               tags$em("World Bank Economic Review"), " 35(4)), simplified for display, licensed ",
               tags$a(href = "https://creativecommons.org/licenses/by-nc-sa/4.0/", target = "_blank", "CC BY-NC-SA 4.0"), "."),
             downloadButton("dl_district", "District estimates (CSV)"),
             downloadButton("dl_dict", "Data dictionary (CSV)")),
        card(card_header("Glossary"),
             tags$dl(lapply(GLOSSARY, function(g) tagList(tags$dt(g[1]), tags$dd(g[2])))))),
      card(card_header("Files in this bundle"), tableOutput("t_bundle")))),

  # ---- Contact -------------------------------------------------------------------
  nav_panel("Contact", value = "contact",
    div(class = "page",
      h2("Contact"),
      card(card_header("Dr Priyanka deSouza"),
           p("Department of Urban and Regional Planning", tags$br(), "University of Colorado Denver"),
           p("Email: ",
             tags$a(href = "mailto:priyanka.desouza@ucdenver.edu", "priyanka.desouza@ucdenver.edu"),
             " or ",
             tags$a(href = "mailto:priyankadesouza@gmail.com", "priyankadesouza@gmail.com"))))),

)

server <- function(input, output, session) {
  # navigation
  for (id in unique(NAV_IDS)) local({
    iid <- id; tgt <- sub("b$", "", strsplit(iid, "__")[[1]][3])
    observeEvent(input[[iid]], { nav_select("nav", tgt); session$sendCustomMessage("scrolltop", TRUE) },
                 ignoreInit = TRUE)
  })

  output$p_agree   <- renderPlotly(plot_agreement(B, input$era))
  output$p_falsify <- renderPlotly(plot_falsification(B))
  output$t_state   <- renderTable({
    s <- B$state_ag; if (is.null(s)) return(NULL)
    data.frame(State = s$state_name, Districts = as.integer(s$districts), r = num(s$r),
               `Average gap (pp)` = signed(100 * s$mean_diff), check.names = FALSE)
  })

  output$p_map <- renderUI(svg_map(B, MP, input$mapvar, input$grey))
  observeEvent(input$map_click, {
    updateSelectizeInput(session, "district", selected = as.character(input$map_click))
    nav_select("nav", "explore")
  })
  output$p_rvc <- renderPlotly(plot_raw_vs_corrected(B, input$rc_yr))

  output$p_forest <- renderPlotly(plot_forest(K, input$outcome, input$rule))
  output$t_health <- renderTable({
    h <- K$h; h <- h[order(h$outcome, h$evidence_rank), ]
    lab <- c(TERM_LAB, change_lpg_bayes_iresonly = "Corrected (IRES calibration both rounds), held fixed",
             change_lpg_bayes_iresonly_MI = "Corrected (IRES calibration both rounds), exposure-only imputation",
             change_lpg_bayes_accesswt = "Corrected (design-weighted ACCESS reference), held fixed",
             change_lpg_bayes_accesswt_MI = "Corrected (design-weighted ACCESS reference), exposure-only imputation")
    data.frame(Outcome = h$outcome, Exposure = ifelse(is.na(lab[h$term]), h$term, lab[h$term]), Role = sub("attenuated, not to be believed", "attenuated by measurement error", h$evidence_role, fixed = TRUE),
               `Per 10 pp` = num(per10(h), 3),
               `95% CI (CR2)` = paste(num(10 * h$ci_cr2_low, 3), "to", num(10 * h$ci_cr2_high, 3)),
               `p (CR2)` = pfmt(h$p_cr2), `p (conv.)` = pfmt(h$p), check.names = FALSE)
  })

  output$p_var    <- renderPlotly(plot_variance(B))

  output$district_card <- renderUI({
    req(input$district)
    d <- B$dist[as.character(B$dist$district) == input$district, ]
    if (!nrow(d)) return(NULL)
    flag <- function(x) if (isTRUE(x)) span(class = "flag", "outside calibration states") else NULL
    pc <- function(x) if (is.finite(x)) paste0(pp(x, 0), "%") else "no estimate"
    rg <- function(lo, hi) if (is.finite(lo) && is.finite(hi)) paste0(pp(lo, 0), "-", pp(hi, 0), "%") else ""
    ch <- function(x) if (is.finite(x)) paste0(signed(100 * x, 0), " pp") else "not available"
    tagList(
      h4(trimws(gsub("\\s+", " ", d$dist_name)), tags$small(class = "muted", paste0(" ", d$state_name))),
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(""), tags$th("2015-16"), tags$th("2019-21"))),
        tags$tbody(
          tags$tr(tags$td("Raw NFHS (design-weighted)"), tags$td(pc(d$raw_wt_2015)), tags$td(pc(d$raw_wt_2019))),
          tags$tr(tags$td("Corrected"), tags$td(pc(d$bayes_2015), flag(d$extrapolated_2015)),
                  tags$td(pc(d$bayes_2019), flag(d$extrapolated_2019))),
          tags$tr(tags$td("Corrected 95% interval"),
                  tags$td(rg(d$bayes_lo_2015, d$bayes_hi_2015)),
                  tags$td(rg(d$bayes_lo_2019, d$bayes_hi_2019))))),
      P("Change 2015 to 2019: raw ", BB(ch(d$raw_wt_2019 - d$raw_wt_2015)), ", corrected ", BB(ch(d$change_bayes)), "."),
      if (isTRUE(d$outside_calib_range_2019) || isTRUE(d$outside_calib_range_2015))
        p(class = "flag-note", "This district's raw value lies outside the range the correction was fitted on in at least one round, so its corrected value is an extrapolation."))
  })
  output$p_district <- renderPlotly({ req(input$district); plot_district(B, input$district) })

  sens_server(input, output, session, B)
  stacking_server(input, output, session, B, MP)
  stacking_transfer_server(input, output, session, B)
  vpart_server(input, output, session, B)
  corrections_server(input, output, session, B)
  output$t_bundle <- renderTable(B$bundle)
  output$dl_district <- downloadHandler("district_lpg_estimates.csv",
    function(file) file.copy(file.path(DATA_DIR, "district_lpg_estimates.csv"), file))
  output$dl_dict <- downloadHandler("district_lpg_estimates_dictionary.csv",
    function(file) file.copy(file.path(DATA_DIR, "district_lpg_estimates_dictionary.csv"), file))
}

shinyApp(ui, server)
