# ==============================================================================
# 10_make_figures.R
# Composes the publication (main-text) figures as single multi-panel images
# with consistent styling, from pipeline outputs. Reproducible: re-run after
# any upstream change.
#
#   Figure 1  fig1_agreement.jpeg   A-B scatter vs identity; C-D Bland-Altman
#   Figure 2  fig2_correction.jpeg  A corrected 2019 map; B correction shift;
#                                   C corrected vs raw scatter
#   Figure 3  fig3_proxies.jpeg     A predicted stacking map; B any-solid map
#
# Inputs : compare_pairs.rds (04), corrected_nfhs_districts.rds (05),
#          district_exposure_proxy.rds (06), district shapefile
# Output : <dir_out>/paper_figs/*.jpeg
# ==============================================================================

source("00_config.R")
need_inputs(c("compare_pairs.rds"            = "04_compare.R",
              "corrected_nfhs_districts.rds" = "05_correction.R",
              "district_exposure_proxy.rds"  = "06_stacking_prediction.R"))
library(cowplot)

dir_figs <- file.path(dir_out, "paper_figs")
dir.create(dir_figs, showWarnings = FALSE)

pairs     <- readRDS(file.path(dir_out, "compare_pairs.rds"))
corrected <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
proxy     <- readRDS(file.path(dir_out, "district_exposure_proxy.rds"))
shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(district = as.character(as.numeric(dist_code))) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

theme_pub <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "plain"),
        legend.position = "right")

# ---------------------------------------------------------------- Figure 1 ----
pA <- pairs$pairA; pB <- pairs$pairB

sc_panel <- function(d, x, y, n, xlab, ylab) {
  r <- cor(d[[x]], d[[y]], use = "pairwise.complete.obs")
  ggplot(d, aes(.data[[x]], .data[[y]])) +
    geom_abline(linetype = 2, color = "grey55") +
    geom_point(aes(size = .data[[n]]), alpha = 0.55, color = "#2166AC",
               stroke = 0) +
    scale_size_area(max_size = 3.5, guide = "none") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    annotate("text", x = 0.03, y = 0.97, hjust = 0, vjust = 1, size = 3.2,
             label = sprintf("r = %.2f", r)) +
    labs(x = xlab, y = ylab) + theme_pub
}
ba_panel <- function(d, x, y, n, ylab) {
  dd <- d %>% mutate(avg = (.data[[x]] + .data[[y]]) / 2,
                     diff = .data[[x]] - .data[[y]])
  md <- mean(dd$diff, na.rm = TRUE); s <- sd(dd$diff, na.rm = TRUE)
  ggplot(dd, aes(avg, diff)) +
    geom_hline(yintercept = 0, color = "grey85") +
    geom_hline(yintercept = md, linewidth = 0.4) +
    geom_hline(yintercept = md + c(-1.96, 1.96) * s, linetype = 2,
               linewidth = 0.35) +
    geom_point(aes(size = .data[[n]]), alpha = 0.55, color = "#2166AC",
               stroke = 0) +
    scale_size_area(max_size = 3.5, guide = "none") +
    labs(x = "Mean of the two estimates", y = ylab) + theme_pub
}
fig1 <- plot_grid(
  sc_panel(pA, "access_w1_mainlpg", "lpg_2015_rural", "n_access_w1",
           "ACCESS Wave 1", "NFHS-4 rural"),
  sc_panel(pB, "ires_mainlpg_rural", "lpg_2019_rural", "n_ires_rural",
           "IRES rural", "NFHS-5 rural"),
  ba_panel(pA, "lpg_2015_rural", "access_w1_mainlpg", "n_access_w1",
           "NFHS-4 - ACCESS W1"),
  ba_panel(pB, "lpg_2019_rural", "ires_mainlpg_rural", "n_ires_rural",
           "NFHS-5 - IRES"),
  labels = c("a", "b", "c", "d"), label_size = 12, ncol = 2, align = "hv")
ggsave(file.path(dir_figs, "fig1_agreement.jpeg"), fig1,
       width = 7.5, height = 7.2, dpi = 400)

# ---------------------------------------------------------------- Figure 2 ----
map_theme <- theme_void(base_size = 9) +
  theme(legend.position = "right",
        legend.key.height = grid::unit(0.9, "lines"),
        legend.key.width = grid::unit(0.5, "lines"))

d2 <- shp %>% left_join(
  corrected %>% transmute(district = as.character(district),
                          raw = lpg_2019_rural,
                          bayes = .data[["lpg_2019_bayes"]],
                          rc = .data[["lpg_2019_rc"]]) , by = "district") %>%
  mutate(shift = bayes - raw)

m_corr <- ggplot(d2) + geom_sf(aes(fill = bayes), color = NA) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "grey90",
                       name = "P(LPG)") + map_theme
lim <- suppressWarnings(max(abs(range(d2$shift, na.rm = TRUE))))
if (!is.finite(lim) || lim == 0) lim <- 1   # all-NA/constant -> safe finite limit
m_shift <- ggplot(d2) + geom_sf(aes(fill = shift), color = NA) +
  scale_fill_gradient2(low = "#2166AC", mid = "grey96", high = "#B2182B",
                       midpoint = 0, limits = c(-lim, lim),
                       na.value = "grey90", name = "Shift") + map_theme
p_sc <- ggplot(d2, aes(raw, bayes)) +
  geom_abline(linetype = 2, color = "grey55") +
  geom_point(alpha = 0.35, size = 0.7, color = "#B2182B", stroke = 0) +
  geom_point(aes(y = rc), alpha = 0.35, size = 0.7, color = "#2166AC",
             stroke = 0) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Raw NFHS-5 rural estimate",
       y = "Corrected (red: Bayes, blue: RC)") + theme_pub
fig2 <- plot_grid(m_corr, m_shift, p_sc, labels = c("a", "b", "c"),
                  label_size = 12, ncol = 3, rel_widths = c(1.1, 1.1, 1))
ggsave(file.path(dir_figs, "fig2_correction.jpeg"), fig2,
       width = 12, height = 4.2, dpi = 400)

# ---------------------------------------------------------------- Figure 3 ----
p5 <- proxy %>% filter(survey == "NFHS5") %>%
  transmute(district = as.character(district), p_stack, p_any_solid_burning)
d3 <- shp %>% left_join(p5, by = "district")
m_stack <- ggplot(d3) + geom_sf(aes(fill = p_stack), color = NA) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "grey90",
                       name = "P(stack|LPG)") + map_theme
m_solid <- ggplot(d3) + geom_sf(aes(fill = p_any_solid_burning), color = NA) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "grey90",
                       name = "P(any solid)", option = "magma") + map_theme
fig3 <- plot_grid(m_stack, m_solid, labels = c("a", "b"), label_size = 12,
                  ncol = 2)
ggsave(file.path(dir_figs, "fig3_proxies.jpeg"), fig3,
       width = 10, height = 4.6, dpi = 400)

## ---- CHECKS ------------------------------------------------------------------
chk_header("10_make_figures")
for (fg in c("fig1_agreement.jpeg","fig2_correction.jpeg","fig3_proxies.jpeg"))
  chk("10", paste0(fg, " written"), file.exists(file.path(dir_figs, fg)))

message("10_make_figures.R done -> ", dir_figs,
        " (fig1_agreement, fig2_correction, fig3_proxies)")
