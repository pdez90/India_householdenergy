## check_nfhs_unit.R -- is the NFHS "household" frame one row per household,
## or one row per household MEMBER?
##
##   cd <repository>/scripts
##   DHS_DIR=~/Downloads/DHS_India Rscript check_nfhs_unit.R
##
## WHY THIS EXISTS
##   The frame 01_prep_nfhs.R builds has 2,869,043 rows for NFHS-4 and
##   2,843,917 for NFHS-5 (before GPS exclusions). DHS reports 601,509 and
##   636,699 interviewed households. Those row counts are close to the number of
##   household MEMBERS, and the frame averages ~100 rows per cluster where DHS
##   samples ~21 households. If the rows are people, every NFHS prevalence in the
##   paper is the share of PEOPLE living in LPG-primary households, compared
##   against ACCESS and IRES, which count HOUSEHOLDS.
##
## THE TEST
##   hv271 (the wealth-index factor score) is continuous and household-level:
##   every member of a household carries the same value, and two households in
##   one cluster almost never share it exactly. So the number of distinct
##   (cluster, hv271, hv012, hv005) keys counts households. If the rows are
##   households, keys == rows. If they are people, rows / keys is close to the
##   mean household size.
##
## It also reports how much the distinction moves rural primary-LPG prevalence:
## the share computed over rows (what the pipeline did before it collapsed the
## frame to households in 01_prep_nfhs.R) beside the share
## computed over distinct households, both with the DHS household weight.
##
## Reads the two slim caches only, one at a time. Writes nothing. Emits no
## checks, so it cannot touch the canonical run's check trail.

if (!exists(".env_dir")) source("paths.R")
dir_dhs <- .dir_dhs()
published <- c(`2015` = 601509, `2019` = 636699)   # DHS final reports, NFHS-4 and NFHS-5

for (yr in c(2015, 2019)) {
  f <- file.path(dir_dhs, sprintf("nfhs_hr_%d_slim.RData", yr))
  if (!file.exists(f)) { message("absent: ", f); next }
  e <- new.env(); load(f, envir = e)
  hr <- get(sprintf("india%d_hr", yr), envir = e); rm(e); invisible(gc())

  key  <- paste(hr$hv001, hr$hv271, hr$hv012, hr$hv005)
  hh1  <- !duplicated(key)
  ncl  <- length(unique(hr$hv001))
  fuel <- as.numeric(unclass(hr$fuel))
  lpg  <- as.integer(fuel == 2)
  rural <- hr$hv025 == 2
  w    <- hr$hv005 / 1e6
  wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

  cat(sprintf("\n================ NFHS %s ================\n", ifelse(yr == 2015, "-4 (2015-16)", "-5 (2019-21)")))
  cat(sprintf("rows in the frame                 %12s\n", format(nrow(hr), big.mark = ",")))
  cat(sprintf("distinct household keys           %12s   (DHS published: %s households)\n",
              format(sum(hh1), big.mark = ","), format(published[[as.character(yr)]], big.mark = ",")))
  cat(sprintf("rows per household key            %12.2f\n", nrow(hr) / sum(hh1)))
  cat(sprintf("mean hv012 (household size)       %12.2f   <- rows/key near this means rows are PEOPLE\n",
              mean(hr$hv012[hh1], na.rm = TRUE)))
  cat(sprintf("clusters                          %12s   rows per cluster %.1f\n",
              format(ncl, big.mark = ","), nrow(hr) / ncl))
  cat("\nrural primary-LPG prevalence, DHS household weight:\n")
  cat(sprintf("  over ROWS (person-level frame)     %6.2f%%\n", 100 * wmean(lpg[rural], w[rural])))
  cat(sprintf("  over distinct HOUSEHOLDS           %6.2f%%\n", 100 * wmean(lpg[rural & hh1], w[rural & hh1])))
  cat(sprintf("  difference (rows - households)     %+6.2f percentage points\n",
              100 * (wmean(lpg[rural], w[rural]) - wmean(lpg[rural & hh1], w[rural & hh1]))))
  rm(hr); invisible(gc())
}
cat("\nIf rows per key is ~1, the frame is households and nothing needs to change.\n",
    "If rows per key is close to mean hv012, the frame is household members: every\n",
    "NFHS prevalence is person-weighted, the multilevel models treat members of one\n",
    "household as independent observations, and the comparison with ACCESS and IRES\n",
    "(both household-level) mixes units. That would need fixing in 01_prep_nfhs.R and\n",
    "a full re-run before any number in the paper is final.\n", sep = "")
