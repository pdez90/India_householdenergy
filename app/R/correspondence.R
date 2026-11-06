# ==============================================================================
# correspondence.R -- the "District correspondence" page: the study's NFHS-5 ->
# 2011 Census district correspondence file (data/district_history_nfhs5.csv,
# copied into the bundle by prepare_data.R), browsable with its sources and
# downloadable. District-level only: no cluster assignment is shown or shipped.
# ==============================================================================

CORR_FILE <- "district_history_nfhs5.csv"

load_corr <- function() {
  p <- file.path(DATA_DIR, CORR_FILE)
  if (!file.exists(p)) return(NULL)
  d <- readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  d[is.na(d)] <- ""
  d
}

corr_type_label <- function(type, residual) {
  out <- ifelse(type == "single_parent", "Single 2011 parent",
         ifelse(type == "multi_parent", "Several candidate 2011 parents",
         ifelse(type == "unchanged", "Unchanged", type)))
  ifelse(residual == "yes", paste0(out, " (residual)"), out)
}

corr_year <- function(y) sub("\\.0$", "", y)

corr_parents <- function(names, codes) {
  n <- trimws(strsplit(names, ";")[[1]]); c <- trimws(strsplit(codes, ";")[[1]])
  if (length(n) == length(c) && length(c)) paste0(n, " (", sprintf("%03d", as.integer(c)), ")", collapse = "; ")
  else names
}

# Links in a source field: every http(s) URL becomes a link; the rest is text.
corr_links <- function(x) {
  if (!nzchar(x)) return("")
  urls <- regmatches(x, gregexpr("https?://[^ ;,)]+", x))[[1]]
  if (!length(urls)) return(x)
  tagList(lapply(seq_along(urls), function(i) tagList(if (i > 1) tags$br(),
    tags$a(href = urls[i], target = "_blank", rel = "noopener",
           sub("^https?://(www\\.)?([^/]+).*$", "\\2", urls[i])))))
}

corr_source_kind <- function(u) {
  ifelse(grepl("lgdirectory", u), "Local Government Directory record",
  ifelse(grepl("rbi\\.org\\.in", u), "Reserve Bank of India lead-bank circular (quotes the state gazette)",
  ifelse(grepl("indiacode|prsindia|faolex|megpns|megpolice.*new_dist|\\.pdf$", u, ignore.case = TRUE),
         "Gazette notification or Act",
  ifelse(grepl("\\.gov\\.in|\\.nic\\.in", u), "State or district government website", "Other source"))))
}

corr_table <- function(d) {
  if (!nrow(d)) return(p(class = "muted", "No districts match these filters."))
  hdr <- c("State", "NFHS-5 district", "Relationship to the 2011 frame", "2011 district(s) (Census 2011 code)",
           "Year", "Confidence", "Source", "Details")
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    alt <- if (nzchar(r$alt_parents_2011_names))
      tags$p(tags$strong("Alternative used in a sensitivity rerun: "),
             corr_parents(r$alt_parents_2011_names, r$alt_parents_2011)) else NULL
    tags$tr(
      tags$td(tools::toTitleCase(tolower(r$state5))),
      tags$td(tags$strong(r$district5)),
      tags$td(corr_type_label(r$type, r$residual)),
      tags$td(corr_parents(r$parents_2011_names, r$parents_2011)),
      tags$td(corr_year(r$year)),
      tags$td(r$confidence),
      tags$td(div(class = "muted", corr_source_kind(r$verified_source_url)), corr_links(r$verified_source_url),
              if (!grepl("lgdirectory", r$verified_source_url) && grepl("\\bLGD\\b", r$verified_evidence))
                tagList(tags$br(), span(class = "muted", "also: "),
                        tags$a(href = "https://lgdirectory.gov.in/", target = "_blank", rel = "noopener", "lgdirectory.gov.in"))),
      tags$td(tags$details(tags$summary("Evidence and assumptions"),
        tags$p(tags$strong("What the source says: "), r$verified_evidence),
        tags$p(tags$strong("Basis: "), r$basis),
        if (nzchar(r$analytic_assumption)) tags$p(tags$strong("Assumption beyond the source: "), r$analytic_assumption),
        alt,
        if (nzchar(r$verification_status))
          tags$p(class = "muted", "Checking: ", gsub("verified_source_url", "the cited source", r$verification_status, fixed = TRUE)))))
  })
  div(style = "overflow-x:auto;",
    tags$table(class = "table table-sm corr-table",
      tags$thead(tags$tr(lapply(hdr, tags$th))), tags$tbody(rows)))
}

ui_corresp <- function(CR) div(class = "page",
  h2("NFHS-5 to 2011 Census district correspondence"),
  if (is.null(CR)) p(class = "muted", "The correspondence file is not in this bundle; run prepare_data.R.") else tagList(
  p("NFHS-5 (2019-21) reports districts on a frame of 707 districts, while the 2011 Census, NFHS-4 and the energy surveys use the 640 districts of 2011. ",
    "We did not find an official correspondence between the two, so we compiled one for this study. It covers the ", nrow(CR),
    " NFHS-5 districts that needed review: districts created, renamed or reorganised after 2011, and name-matched districts whose survey locations ",
    "suggested a boundary change. Every other NFHS-5 district has the same name as its 2011 district in the same state."),
  p("For each district the file records its 2011 predecessor district or districts, the year of the change, the source and the passage relied on, ",
    "a confidence grade, and any assumption that goes beyond what the source states. The most common sources are Reserve Bank of India lead-bank circulars, ",
    "which quote the state gazette when a state creates districts; others are the creating notification or Act, state or district government websites, ",
    "and the Local Government Directory (lgdirectory.gov.in), the government's register of administrative units."),
  tags$ul(
    tags$li(tags$strong("Single 2011 parent: "), "the district lies within one 2011 district. ",
            tags$em("Residual"), " districts kept their 2011 name after territory was carved out of them."),
    tags$li(tags$strong("Several candidate 2011 parents: "), "the district drew territory from more than one 2011 district; ",
            "in the study each survey cluster is assigned to the candidate whose 2011 boundary contains its location."),
    tags$li(tags$strong("Unchanged: "), "the district's territory did not change between 2011 and the NFHS-5 fieldwork.")),
  p(class = "muted", "Codes are Census 2011 district codes (as in SHRUG's pc11_d_id). The file is district-level: the cluster-level assignment uses ",
    "DHS GPS data, which are not redistributable, and is not part of it. Corrections are welcome (see Contact)."),
  layout_columns(col_widths = c(4, 4, 4),
    selectInput("corr_state", "State", c("All states" = "", sort(unique(tools::toTitleCase(tolower(CR$state5)))))),
    selectInput("corr_type", "Relationship", c("All" = "", "Single 2011 parent" = "single_parent",
                                               "Several candidate 2011 parents" = "multi_parent", "Unchanged" = "unchanged")),
    textInput("corr_q", "Search district or 2011 district", "")),
  div(class = "step-nav", style = "justify-content:flex-start; gap:8px;",
    downloadButton("dl_corr", "Correspondence file (CSV, all columns)"),
    span(class = "muted", textOutput("corr_n", inline = TRUE))),
  uiOutput("corr_tbl"),
  p(class = "muted", "Source: data/district_history_nfhs5.csv in ",
    tags$a(href = "https://github.com/pdez90/India_householdenergy", target = "_blank", "github.com/pdez90/India_householdenergy"),
    ". Method and sensitivity analyses: manuscript Section 2.1 and SI Methods S13.")))

corresp_server <- function(input, output, session, CR) {
  if (is.null(CR)) return(invisible())
  sel <- reactive({
    d <- CR
    if (nzchar(input$corr_state)) d <- d[tools::toTitleCase(tolower(d$state5)) == input$corr_state, ]
    if (nzchar(input$corr_type)) d <- d[d$type == input$corr_type, ]
    q <- trimws(input$corr_q)
    if (nzchar(q)) d <- d[grepl(tolower(q), tolower(paste(d$district5, d$parents_2011_names,
                                                          d$alt_parents_2011_names)), fixed = TRUE), ]
    d[order(d$state5, d$district5), ]
  })
  output$corr_n <- renderText(paste0(nrow(sel()), " of ", nrow(CR), " districts shown"))
  output$corr_tbl <- renderUI(corr_table(sel()))
  output$dl_corr <- downloadHandler(CORR_FILE, function(file) file.copy(file.path(DATA_DIR, CORR_FILE), file))
}
