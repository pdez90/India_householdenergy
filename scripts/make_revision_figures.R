## make_revision_figures.R -- the figures changed in the October 2026 co-author
## review, drawn from the SAVED outputs of the canonical run.
##
##   cd <repository>/scripts
##   ACCESS_DIR=/path/to/data OUTPUT_DIR="$ACCESS_DIR/outputs" \
##     Rscript make_revision_figures.R
##
## Writes into <OUTPUT_DIR>/paper_figs/:
##   fig3_precision_frontier.jpeg   Figure 3 with the estimate + CR2 CI panel and
##                                  a labelled kappa_floor (Jenny Lee)
##   fig1e_did_benchmarks.jpeg      the before/after gap for primary LPG beside
##                                  every benchmark item (Jenny Lee)
##   fig1_agreement_revised.jpeg    only with FIG1_COMPOSITE=TRUE (see below)
##
## These are presentation changes only. Every quantity plotted is read from a
## CSV the canonical run wrote; nothing is re-estimated. Like the other
## post-run scripts this emits no checks and touches no diagnostics file, so the
## canonical run's single-run check trail is left exactly as it was.

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(readr); library(patchwork)
})
if (!exists(".env_dir")) source("paths.R")
source("fig_precision_frontier.R")
dir_out <- .dir_out()
figs <- file.path(dir_out, "paper_figs")
dir.create(figs, showWarnings = FALSE, recursive = TRUE)
need <- function(f) {
  p <- file.path(dir_out, f)
  if (!file.exists(p)) stop("missing ", p, " -- run the pipeline first", call. = FALSE)
  readr::read_csv(p, show_col_types = FALSE)
}

## ---- Figure 3 ---------------------------------------------------------------
frontier <- need("design_frontier.csv")
direct   <- need("design_direct.csv")
summ     <- need("design_summary.csv")
ks_tab   <- need("design_kappa_star.csv")
val <- function(q) summ$value[summ$quantity == q]
KS          <- val("kappa_star_infant")
KAPPA_FLOOR <- val("kappa_floor")
KS2         <- ks_tab$kappa_star_cr2[ks_tab$outcome == "infant"]
fig3 <- plot_precision_frontier(frontier, direct, KS, KS2, KAPPA_FLOOR)
ggsave(file.path(figs, "fig3_precision_frontier.jpeg"), fig3,
       width = 7.2, height = 7.4, dpi = 300)
message(sprintf("wrote fig3_precision_frontier.jpeg (kappa* = %.4f, kappa_floor = %.4f, kappa*(CR2) = %s)",
                KS, KAPPA_FLOOR, ifelse(is.finite(KS2), sprintf("%.4f", KS2), "not reached")))

## ---- Figure 1e --------------------------------------------------------------
source("fig_did_benchmarks.R")
bm <- need("si_benchmark_agreement.csv")
fig1e <- plot_did_benchmarks(bm)
moved_items <- attr(fig1e, "moved_items")
ggsave(file.path(figs, "fig1e_did_benchmarks.jpeg"), fig1e,
       width = 5.6, height = 4.6, dpi = 300)
message("wrote fig1e_did_benchmarks.jpeg; items whose gap moved > 10 pp: ",
        paste(moved_items, collapse = ", "))

## ---- Figure 1, revised composite: canonical panels a-d + the new panel e ----
## Panels a-d are taken from the canonical run's own Figure 1 image, cropped at
## the white band that separates them from the old panel e, so they are exactly
## what that run drew. The band is located by scanning the image, not typed.
## (10_make_figures.R now draws this panel e natively, so the next full run
## produces the same composite without this step.)
## Only for outputs written BEFORE 10_make_figures.R drew the new panel e
## (i.e. the 2026-09-16 run). On any later run fig1_agreement.jpeg already
## carries it, so this step is off unless asked for: FIG1_COMPOSITE=TRUE.
fig1_src <- file.path(figs, "fig1_agreement.jpeg")
if (toupper(Sys.getenv("FIG1_COMPOSITE", "FALSE")) != "TRUE") {
  message("Figure 1 composite skipped: 10_make_figures.R draws panel e itself ",
          "(set FIG1_COMPOSITE=TRUE only for outputs from before that change)")
} else if (file.exists(fig1_src) && requireNamespace("jpeg", quietly = TRUE)) {
  img <- jpeg::readJPEG(fig1_src)
  grey <- if (length(dim(img)) == 3) rowMeans(img[, , 1:3, drop = FALSE], dims = 2) else img
  H <- nrow(grey)
  is_white <- apply(grey, 1, function(r) all(r[seq(1, length(r), 3)] > 0.96))
  rng <- seq(floor(H * 0.55), floor(H * 0.70))
  rl <- rle(is_white[rng]); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1
  k <- which(rl$values & rl$lengths >= 30)
  if (!length(k)) stop("could not find the white band above the old panel e in ", fig1_src,
                       call. = FALSE)
  k <- k[which.max(rl$lengths[k])]
  cut_row <- rng[1] - 1 + floor((starts[k] + ends[k]) / 2)
  abcd <- img[seq_len(cut_row), , , drop = FALSE]
  dpi <- 400   # 10_make_figures.R saves fig1_agreement.jpeg at 400 dpi
  w_in <- ncol(grey) / dpi; h_abcd <- cut_row / dpi; h_e <- 5.4
  panel_e <- plot_did_benchmarks(bm, text_scale = 1.45) + labs(tag = "e") +
    theme(plot.tag = element_text(face = "bold", size = 20))
  comp <- patchwork::wrap_elements(full = grid::rasterGrob(abcd, interpolate = TRUE)) /
    (patchwork::plot_spacer() + panel_e + patchwork::plot_spacer() +
       patchwork::plot_layout(widths = c(0.12, 0.76, 0.12))) +
    patchwork::plot_layout(heights = c(h_abcd, h_e))
  ggsave(file.path(figs, "fig1_agreement_revised.jpeg"), comp,
         width = w_in, height = h_abcd + h_e, dpi = dpi)
  message(sprintf("wrote fig1_agreement_revised.jpeg (panels a-d cropped at row %d of %d)",
                  cut_row, H))
} else message("canonical fig1_agreement.jpeg or the jpeg package absent -- composite skipped")
