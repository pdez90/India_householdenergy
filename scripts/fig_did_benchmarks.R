## fig_did_benchmarks.R -- Figure 1e, the before/after gap for primary LPG
## beside every benchmark item.
##
## ONE definition, used by 10_make_figures.R (Figure 1 composite, every run)
## and by make_revision_figures.R (from the saved CSV, no re-run).
##
## The paper's identification argument in one picture. If NFHS-5 understated
## LPG because of who it sampled, every item would drift with it; if the
## question is the problem, the composition items hold still while LPG moves.
## Every benchmark is shown -- including the ones that do NOT hold still --
## because a figure showing only the agreeing items would make the argument by
## selection. Which items are highlighted is decided by the data (a change in
## the gap of more than 10 percentage points), not by hand.
##
## `bm` is si_benchmark_agreement.csv from 08_si_benchmarks.R: one row per
## survey pair x item, with mean_diff = mean district NFHS-minus-reference.

## Uses ggplot2, dplyr and tidyr verbs unqualified: both callers attach them
## (10_make_figures.R through 00_config.R, the driver through library()).
plot_did_benchmarks <- function(bm, text_scale = 1) {
  ts <- text_scale   # multiplies every text size (a raster composite of Figure 1 needs ~1.45; 10_make_figures.R uses 1.1)
  stopifnot("ggplot2" %in% .packages(), "dplyr" %in% .packages(), "tidyr" %in% .packages())
  `%>%` <- dplyr::`%>%`
  bm <- bm %>%
    mutate(era = ifelse(grepl("^NFHS-4", pair), "before", "after"),
           gap = 100 * mean_diff)
  ## One survey pair per era: the before/after contrast below pivots on
  ## (variable, era), and a second pair in either era would turn the gap
  ## columns into lists. Fail loudly rather than draw a wrong figure.
  dup <- bm %>% dplyr::count(variable, era) %>% filter(n > 1)
  if (nrow(dup) > 0)
    stop("plot_did_benchmarks: more than one survey pair per era for ",
         paste(unique(dup$variable), collapse = ", "),
         " -- filter bm to one NFHS-4 and one NFHS-5 pair before calling.")
  item_lab <- c(lpg = "Primary LPG", bpl = "BPL/Antyodaya ration card",
                electricity = "Household electricity", sc = "Scheduled Caste",
                st = "Scheduled Tribe", scst = "SC/ST combined", hindu = "Hindu",
                muslim = "Muslim")
  ## Items whose before-after change in the gap exceeds 10 percentage points are
  ## highlighted and labelled; the threshold is stated in the caption, and which
  ## items cross it is decided by the data, not by hand.
  chg <- bm %>% select(variable, era, gap) %>%
    pivot_wider(names_from = era, values_from = gap) %>%
    mutate(moved = dplyr::coalesce(abs(after - before) > 10, FALSE))   # an item missing from one era cannot have "moved"
  bm <- bm %>% dplyr::left_join(chg %>% select(variable, moved), by = "variable") %>%
    mutate(item = item_lab[variable],
           grp = ifelse(moved, item, "Other benchmarks"),
           x = ifelse(era == "before", 1, 2))
  hi_cols <- c("#2a78d6", "#eb6834")   # validated pair; grey for everything else
  moved_items <- unique(bm$item[bm$moved])
  ## The highlight is decided by the data, so it must not depend on how many
  ## items happen to cross the threshold. Two have so far (LPG and BPL); if a
  ## re-run moves more, extend the palette rather than leave a series coloured
  ## NA (ggplot would silently draw it grey and the legend would misname it).
  if (length(moved_items) > length(hi_cols)) {
    message("[fig1e] ", length(moved_items), " items moved > 10 pp (",
            paste(moved_items, collapse = ", "), "); extending the highlight palette")
    ext <- c(hi_cols, "#1a9e77", "#b5179e", "#7a5c00")
    hi_cols <- if (length(moved_items) <= length(ext)) ext[seq_along(moved_items)] else
      grDevices::hcl.colors(length(moved_items), "Dark 3")
  }
  ## An item 08_si_benchmarks.R adds later would otherwise get an NA label and
  ## drop out of the legend and the grouped label; show its variable name.
  if (any(is.na(bm$item))) {
    message("[fig1e] no display label for: ",
            paste(unique(bm$variable[is.na(bm$item)]), collapse = ", "),
            " -- using the variable name; add it to item_lab")
    bm$item <- ifelse(is.na(bm$item), bm$variable, bm$item)
    bm$grp  <- ifelse(bm$moved, bm$item, "Other benchmarks")
    moved_items <- unique(bm$item[bm$moved])
  }
  cols <- setNames(c(hi_cols[seq_along(moved_items)], "grey60"),
                   c(moved_items, "Other benchmarks"))
  ## District counts per era; if items differ in coverage, show every count
  ## rather than let sprintf() return one axis label per value.
  n_lab <- function(e) paste(sort(unique(bm$n[bm$era == e])), collapse = "/")
  n_before <- n_lab("before"); n_after <- n_lab("after")
  ## Right-hand labels: the highlighted items individually (nudged apart, they end
  ## 0.7 pp from each other), and the rest as ONE group label -- the remaining
  ## items end within a few pp of each other and cannot be labelled individually
  ## without overprinting.
  lab_r <- bm %>% filter(era == "after", moved) %>% arrange(gap) %>%
    mutate(vj = ifelse(row_number() == 1, 1.5, -0.5))
  oth <- bm %>% filter(era == "after", !moved)
  oth_lab <- if (!nrow(oth))
    data.frame(x = numeric(), gap = numeric(), item = character(), grp = character(),
               lab = character()) else data.frame(x = 2, gap = mean(range(oth$gap)), item = "other", grp = "Other benchmarks",
    lab = sprintf("%d other benchmarks, all within\n%+.1f to %+.1f pp after PMUY:\n%s",
                  nrow(oth), min(oth$gap), max(oth$gap),
                  paste(strwrap(paste(sort(oth$item), collapse = ", "), 30),
                        collapse = "\n")))
  ## Left-hand labels: the highlighted items, and any other item whose gap was
  ## already large before PMUY, so no line in the plot is left unexplained.
  lab_l <- bm %>% filter(era == "before", moved | abs(gap) >= 5)
  fig1e <- ggplot(bm, aes(x, gap, group = item, colour = grp)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_line(aes(linewidth = moved)) + geom_point(aes(size = moved)) +
    scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.5) * ts, guide = "none") +
    scale_size_manual(values = c(`TRUE` = 2.4, `FALSE` = 1.6) * ts, guide = "none") +
    ## Labels are offset by a fixed nudge from the point, with hjust 0/1, so they
    ## clear the marker whatever its size (hjust alone left them under the dot).
    geom_text(data = lab_r, aes(label = sprintf("%s  %+.1f", item, gap), vjust = vj),
              hjust = 0, nudge_x = 0.05, size = 2.6 * ts, show.legend = FALSE) +
    geom_label(data = oth_lab, aes(label = lab), hjust = 0, nudge_x = 0.07, size = 2.4 * ts,
               lineheight = 0.95, colour = "grey35", fill = "white", label.size = 0,
               label.padding = grid::unit(0.1, "lines"), show.legend = FALSE) +
    geom_text(data = lab_l, aes(label = ifelse(moved, sprintf("%+.1f", gap),
                                               sprintf("%s %+.1f", item, gap))),
              hjust = 1, nudge_x = -0.05, size = 2.5 * ts, show.legend = FALSE) +
    scale_colour_manual(values = cols, breaks = c(moved_items, "Other benchmarks")) +
    scale_x_continuous(breaks = c(1, 2), limits = c(0.05, 3.3),
                       labels = c(sprintf("Before PMUY\nNFHS-4 vs ACCESS\n(%s districts)", n_before),
                                  sprintf("After PMUY\nNFHS-5 vs IRES\n(%s districts)", n_after))) +
    labs(x = NULL, y = "NFHS minus energy survey,\nmean district gap (percentage points)",
         colour = NULL) +
    theme_bw(base_size = 9 * ts) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  attr(fig1e, "moved_items") <- moved_items
  fig1e
}
