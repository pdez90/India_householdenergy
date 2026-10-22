# ==============================================================================
# 27_stacking_state_validation.R -- can the stacking model say anything at the
# STATE level, and what do the reference surveys themselves measure?
#
# 21_transfer_diagnostics.R shows that the household stacking model does not
# transfer at the district level. This script asks the coarser question that
# would decide whether national stacking maps have any policy use at all:
#
#   Leave-one-state-out within rural IRES (21 states): fit the context-free
#   stacking model on all other states, predict the held-out state's stacking
#   share from its households' characteristics alone (population-level
#   prediction, no state or district effect), and compare with the share IRES
#   actually measured there. A "no-skill" benchmark predicts every held-out
#   state with the training states' overall share. If the model does not beat
#   that benchmark, household characteristics carry no state-level signal.
#
#   Cross-instrument check at the state level: the all-state IRES model scored
#   on ACCESS Wave 1 states (6 states).
#
# It also writes the reference surveys' OWN measured district stacking shares
# (no model involved) for the districts they sampled, for the app.
#
# Standalone and read-only with respect to the pipeline: it reads
# harmonized_frames.rds (written by 06) and the reference district tables
# (02/03), writes three CSVs, and does not touch the pipeline check registry.
#
#   cd scripts && Rscript 27_stacking_state_validation.R
#
# OUTPUT  stacking_state_validation.csv          per held-out state
#         stacking_state_validation_summary.csv  r / RMSE / skill vs benchmark
#         reference_stacking_district.csv        ACCESS / IRES measured shares
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(lme4) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()

H <- readRDS(file.path(dir_out, "harmonized_frames.rds"))
pred_nc <- setdiff(H$predictors, "dist_lpg")
rhs_nc  <- H$rhs_nc

prep <- function(d) {
  d <- d %>% filter(!is.na(stack_binary))
  d <- d[complete.cases(d[, pred_nc, drop = FALSE]), , drop = FALSE]
  d$state <- droplevels(factor(d$state))
  d$district <- droplevels(factor(as.character(d$district)))
  d
}
requint <- function(d) {   # wealth quintiles within state over the rural frame (as in 21)
  if (!"month_exp" %in% names(d) || !"wealth_q" %in% names(d)) return(d)
  d %>% mutate(.w = suppressWarnings(as.numeric(month_exp))) %>% group_by(state) %>%
    mutate(wealth_q = ifelse(is.na(.w), NA_integer_, dplyr::ntile(.w, 5))) %>%
    ungroup() %>% select(-.w)
}
fit <- function(d) glmer(as.formula(paste("stack_binary ~", rhs_nc, "+ (1 | state) + (1 | district)")),
                         data = d, family = binomial, nAGQ = 0,
                         control = glmerControl(optimizer = "nloptwrap"))

IR <- prep(requint(H$ires %>% filter(rural == 1)))
A1 <- prep(H$access_w1)
message(sprintf("IRES rural: %d households, %d states; ACCESS W1: %d households, %d states",
                nrow(IR), nlevels(IR$state), nrow(A1), nlevels(A1$state)))

# The model's predictions are averaged over the state and district random
# effects (a held-out state's effect is unknown), so they estimate the share of
# HOUSEHOLDS that stack -- the same quantity as the observed share and the
# benchmark. predict(re.form = NA) alone would give the share for a "typical"
# district, which on the logit scale is pulled toward the extremes.
gh <- local({ n <- 40; i <- seq_len(n - 1); J <- matrix(0, n, n)
  J[cbind(i, i + 1)] <- J[cbind(i + 1, i)] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values * sqrt(2), w = e$vectors[1, ]^2) })   # nodes/weights for N(0, 1)
marg_pred <- function(m, newdata) {
  eta <- suppressWarnings(predict(m, newdata = newdata, type = "link", re.form = NA))
  s   <- sqrt(sum(vapply(lme4::VarCorr(m), function(v) as.numeric(v[1, 1]), numeric(1))))
  as.numeric(plogis(outer(eta, s * gh$x, `+`)) %*% gh$w)
}

# ---- leave-one-state-out within IRES -----------------------------------------
loso <- lapply(levels(IR$state), function(s) {
  tr <- IR[IR$state != s, , drop = FALSE]; te <- IR[IR$state == s, , drop = FALSE]
  tr$state <- droplevels(tr$state); tr$district <- droplevels(tr$district)
  m <- tryCatch(fit(tr), error = function(e) NULL)
  if (is.null(m) || nrow(te) < 20) return(NULL)
  p <- marg_pred(m, te)
  ok <- is.finite(p)
  data.frame(check = "IRES leave-one-state-out", state = s, n_households = sum(ok),
             observed = mean(te$stack_binary[ok]), predicted = mean(p[ok]),
             benchmark = mean(tr$stack_binary))
})
loso <- bind_rows(loso)

# ---- IRES (all states) -> ACCESS W1 states -------------------------------------
m_all <- fit(IR)
pA <- marg_pred(m_all, A1)
xs <- data.frame(state = A1$state, y = A1$stack_binary, p = pA) %>% filter(is.finite(p)) %>%
  group_by(state) %>% summarise(n_households = n(), observed = mean(y), predicted = mean(p), .groups = "drop") %>%
  mutate(check = "IRES model -> ACCESS Wave 1 states", benchmark = mean(IR$stack_binary), state = as.character(state))

by_state <- bind_rows(loso, xs) %>% select(check, state, n_households, observed, predicted, benchmark)
write.csv(by_state, file.path(dir_out, "stacking_state_validation.csv"), row.names = FALSE)

summ <- by_state %>% group_by(check) %>% summarise(
  n_states = n(),
  r_state = suppressWarnings(cor(observed, predicted)),
  rmse_model_pp = 100 * sqrt(mean((observed - predicted)^2)),
  rmse_benchmark_pp = 100 * sqrt(mean((observed - benchmark)^2)),
  mae_model_pp = 100 * mean(abs(observed - predicted)),
  skill_vs_benchmark = 1 - sqrt(mean((observed - predicted)^2)) / sqrt(mean((observed - benchmark)^2)),
  observed_range_pp = paste0(round(100 * min(observed)), "-", round(100 * max(observed))),
  .groups = "drop")
write.csv(summ, file.path(dir_out, "stacking_state_validation_summary.csv"), row.names = FALSE)
cat("\n== State-level validation of the stacking model ==\n"); print(as.data.frame(summ), digits = 3)
cat("\nskill_vs_benchmark > 0 means household characteristics predict state stacking better than\n",
    "assigning every state the overall share; <= 0 means no state-level signal.\n", sep = "")

# ---- the reference surveys' own measured district shares ------------------------
ad <- readRDS(file.path(dir_out, "access_districts.rds"))
id <- readRDS(file.path(dir_out, "ires_districts.rds"))
# n_households is the number of households the stacking share is computed on:
# those whose main fuel is LPG and whose solid-fuel use is recorded -- not the
# district's whole sample (n_access_w1 / n_ires), which is several times larger.
n_stack <- function(hh, keep = rep(TRUE, nrow(hh)))
  hh[keep & !is.na(hh$stack_binary), , drop = FALSE] %>%
    mutate(district = as.character(district)) %>% count(district, name = "n")
ahh <- readRDS(file.path(dir_out, "access_hh.rds"))
ihh <- readRDS(file.path(dir_out, "ires_hh.rds"))
na1 <- n_stack(ahh, ahh$wave %in% 0)   # wave 0 = ACCESS Wave 1 (2014-15)
ni  <- n_stack(ihh)
ref <- bind_rows(
  data.frame(survey = "ACCESS Wave 1 (2014-15)", state = ad$state, district = ad$district,
             n_households = na1$n[match(as.character(ad$district), na1$district)],
             stacking_share = ad$access_w1_stack),
  data.frame(survey = "IRES (2019-20)", state = id$state, district = id$district,
             n_households = ni$n[match(as.character(id$district), ni$district)],
             stacking_share = id$ires_stack))
write.csv(ref, file.path(dir_out, "reference_stacking_district.csv"), row.names = FALSE)
message("wrote stacking_state_validation.csv, stacking_state_validation_summary.csv, reference_stacking_district.csv")
