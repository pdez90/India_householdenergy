## fig_precision_frontier.R -- Figure 3, the precision frontier.
##
## ONE definition of the figure, used in two places:
##   * 22_design_analysis.R calls it at the end of every pipeline run;
##   * make_revision_figures.R calls it on the saved CSVs, so a change to the
##     figure's presentation never requires re-running the analysis (which would
##     also re-key the check trail).
## It reads nothing and writes nothing; it takes the frontier table and the
## scalars and returns a patchwork object. Callers decide where it goes.
##
## Layout (co-author review, Oct 2026):
##   (a) the pooled coefficient with its CR2/Satterthwaite 95% CI at each kappa,
##       one panel per outcome -- so a reader sees how wide the interval still is
##       at the floor, and where (if anywhere) it would first exclude zero;
##   (b) each rule's test statistic divided by its own 5% critical value, so the
##       threshold is the single line y = 1 and each curve's crossing is that
##       rule's kappa*. kappa_floor is labelled with its value.
##
## Required columns of `frontier`: outcome, kappa, estimate, z, se_cr2, t_cr2,
## crit_cr2. Estimates are per percentage point of LPG prevalence and are
## displayed per 10 percentage points, the unit used throughout the paper.

plot_precision_frontier <- function(frontier, direct, KS, KS2, KAPPA_FLOOR,
                                    Z_CRIT = qnorm(0.975)) {
  stopifnot(requireNamespace("ggplot2"), requireNamespace("patchwork"),
            requireNamespace("dplyr"), requireNamespace("tidyr"))
  `%>%` <- dplyr::`%>%`
  lab_out <- c(infant = "Infant mortality", neonatal = "Neonatal mortality")
  cols    <- c("Infant mortality" = "#F8766D", "Neonatal mortality" = "#00BFC4")
  # kappa_floor is NA when 22's variance decomposition failed its check; the
  # floor band and its labels are then simply not drawn.
  floor_ok <- length(KAPPA_FLOOR) == 1 && is.finite(KAPPA_FLOOR) && KAPPA_FLOOR > 0
  floor_lab <- sprintf("kappa[floor] == %.2f", KAPPA_FLOOR)

  ## ---- (a) estimate and CR2 interval -------------------------------------------
  est <- frontier %>%
    dplyr::mutate(outcome = factor(lab_out[outcome], lab_out),
                  est10 = 10 * estimate,
                  lo10  = 10 * (estimate - crit_cr2 * se_cr2),
                  hi10  = 10 * (estimate + crit_cr2 * se_cr2)) %>%
    dplyr::filter(is.finite(lo10), is.finite(hi10))
  pa <- ggplot2::ggplot(est, ggplot2::aes(kappa, est10, colour = outcome, fill = outcome)) +
    { if (floor_ok) ggplot2::annotate("rect", xmin = 0, xmax = KAPPA_FLOOR, ymin = -Inf, ymax = Inf,
                      fill = "grey85", alpha = 0.6) } +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    { if (floor_ok) ggplot2::geom_vline(xintercept = KAPPA_FLOOR, colour = "grey35",
                        linetype = "longdash", linewidth = 0.4) } +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo10, ymax = hi10), alpha = 0.18,
                         colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1.1) +
    ggplot2::facet_wrap(~ outcome, nrow = 1) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = NULL,
                  y = "change in deaths per 100 births\nper 10-pp rise in primary LPG",
                  tag = "a") +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())

  ## ---- (b) statistic relative to its own critical value ---------------------
  pl <- frontier %>%
    dplyr::mutate(outcome = factor(lab_out[outcome], lab_out),
                  `Conventional clustered z` = abs(z) / Z_CRIT,
                  `CR2 / Satterthwaite (primary)` = abs(t_cr2) / crit_cr2) %>%
    tidyr::pivot_longer(c(`Conventional clustered z`, `CR2 / Satterthwaite (primary)`),
                        names_to = "rule", values_to = "ratio") %>%
    dplyr::filter(is.finite(ratio)) %>%
    dplyr::mutate(rule = factor(rule, c("CR2 / Satterthwaite (primary)",
                                        "Conventional clustered z")))
  ymax <- max(pl$ratio, 1.05)
  ## kappa* is NA when the outcome never reaches p = 0.05 even at kappa = 0, and
  ## could in principle fall above the floor. Draw its line and label only when
  ## it exists, and keep the "unreachable" note inside the shaded region either
  ## way -- between kappa* and the floor when kappa* lies below the floor (the
  ## published case), otherwise centred in the region.
  ks_ok <- length(KS) == 1 && is.finite(KS)
  unreach_x <- if (!floor_ok) NA_real_ else
    if (ks_ok && KS < KAPPA_FLOOR) (KS + KAPPA_FLOOR) / 2 else KAPPA_FLOOR / 2
  if (floor_ok && ks_ok && KS >= KAPPA_FLOOR)
    message("[fig3] kappa* (", signif(KS, 3), ") is at or above kappa_floor (",
            signif(KAPPA_FLOOR, 3), "): the 'unreachable' framing no longer holds -- revisit the text")
  ## Direct labels at the right-hand end of each curve: the colour pair carries a
  ## contrast WARN against a light surface, so identity must not rest on colour.
  ends <- pl %>% dplyr::filter(kappa == max(kappa)) %>%
    dplyr::mutate(lab = paste0(sub(" mortality", "", outcome), ", ",
                               ifelse(grepl("^CR2", rule), "CR2", "z")))
  pb <- ggplot2::ggplot(pl, ggplot2::aes(kappa, ratio, colour = outcome, linetype = rule)) +
    { if (floor_ok) ggplot2::annotate("rect", xmin = 0, xmax = KAPPA_FLOOR, ymin = -Inf, ymax = Inf,
                      fill = "grey85", alpha = 0.6) } +
    ## Placed between kappa* and kappa_floor so it no longer runs into the
    ## dotted kappa* line, which it did in the first version.
    ## In the empty lower band of the shaded region, right of kappa*, where no
    ## curve passes; the first two placements ran into the kappa* line and then
    ## into the p = 0.05 line.
    { if (floor_ok) ggplot2::annotate("text", x = unreach_x, y = 0.20,
                      label = "below the NFHS-side\nerror floor (scalar\napproximation)",
                      size = 2.5, colour = "grey30", lineheight = 0.95) } +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    { if (floor_ok) ggplot2::geom_vline(xintercept = KAPPA_FLOOR, colour = "grey35",
                        linetype = "longdash", linewidth = 0.4) } +
    { if (floor_ok) ggplot2::annotate("text", x = KAPPA_FLOOR, y = 0.03, hjust = -0.06, size = 2.7,
                      label = floor_lab, parse = TRUE, colour = "grey15") } +
    { if (ks_ok) ggplot2::geom_vline(xintercept = KS, linetype = "dotted", colour = "grey20") } +
    { if (length(KS2) == 1 && is.finite(KS2))
        ggplot2::geom_vline(xintercept = KS2, linetype = "dotted", colour = "grey20") } +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1.2) +
    { if (ks_ok) ggplot2::annotate("text", x = KS, y = 0.03, hjust = 1.06, size = 2.7,
                      label = sprintf("kappa^'*' == %.2f~'(z)'", KS), parse = TRUE) } +
    { if (length(KS2) == 1 && is.finite(KS2))
        ggplot2::annotate("text", x = KS2, y = 0.12, hjust = 1.06, size = 2.7,
                          label = sprintf("kappa^'*' == %.2f~'(CR2)'", KS2), parse = TRUE) } +
    ggplot2::annotate("text", x = 1, y = 1, vjust = -0.6, hjust = 1, size = 2.6,
                      colour = "grey30", label = "p = 0.05") +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = lab), hjust = -0.08,
                       size = 2.4, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, 0.2), expand = ggplot2::expansion(mult = c(0.02, 0.16)),
      sec.axis = ggplot2::sec_axis(~ .,
        breaks = direct$kappa[direct$m_ratio %in% c(4, 9, 16, 25, 36)],
        labels = paste0(direct$m_ratio[direct$m_ratio %in% c(4, 9, 16, 25, 36)], "x"),
        name = "direct measurement in every study district (households per district, relative to existing surveys)")) +
    ggplot2::coord_cartesian(ylim = c(0, ymax), clip = "off") +
    ggplot2::labs(x = expression(paste(kappa, " = corrected-exposure posterior SD, relative to the study as conducted")),
                  y = "test statistic as a multiple of\nits own 5% critical value",
                  colour = NULL, linetype = NULL, tag = "b") +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    linetype = ggplot2::guide_legend(order = 2)) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())

  patchwork::wrap_plots(pa, pb, ncol = 1, heights = c(1, 1.45))
}
