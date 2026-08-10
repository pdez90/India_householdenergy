# NSSO-78 (MIS 2020-21) x NFHS-5 x IRES: district LPG comparison

Scripts 25 and 26 implement the NSSO-78 external-validation component of the
full pipeline: they produce district-level P(main cooking fuel = LPG) from the
NSSO 78th-round Multiple Indicator Survey on the same NFHS-4 2015 geography
(census-2011 `dist_code`) used everywhere else, and compare the surveys
head-to-head. `run_everything.R` runs them in their canonical place (after
04_compare.R), and the manuscript's external-validation section (SI S8,
Table S9.19, Figure S10.16) is built from their outputs. The two scripts add
to, and do not modify, the code of scripts 00-24.

## Files

| file | goes in | role |
|---|---|---|
| `25_prep_nsso78.R` | `scripts/` | NSSO-78 unit data -> `nsso78_hh.rds`, `nsso78_districts.rds` (+ linkage diagnostics) |
| `26_compare_nsso78.R` | `scripts/` | three-way spreadsheet, agreement table, scatter + Bland-Altman figures, auto-generated summary |
| `nsso78_district_key.csv` | `data/` (in this repository; resolved automatically by 25) | 685 NSS-frame districts -> 640 census-2011 district codes |

## Run

```
cd scripts
Rscript 25_prep_nsso78.R     # needs 00_config.R + the MoSPI unit data
Rscript 26_compare_nsso78.R  # needs nfhs_districts.rds (01) and ires_districts.rds (03)
```

Point 25 at your MoSPI download with environment variables (no script edits):
`DIR_NSSO` (folder), `NSSO_HH_CSV` (the Level-03 household CSV), and
optionally `NSSO_KEY_CSV`; by default the district key checked into this
repository's `data/` folder is used. Both scripts also run automatically in
their place inside `Rscript run_everything.R`.

Outputs land in `dir_out` alongside everything else:
`nsso78_nfhs_ires_district_lpg.csv` (the district spreadsheet),
`nsso78_comparison_table.csv`, `nsso78_scatter_45deg.jpeg`,
`nsso78_blandaltman.jpeg`, `nsso78_comparison_summary.md`.

## What was verified before shipping (and is re-asserted by CHECKS on every run)

- **Which file is the household record.** The MoSPI download has UUID names;
  `d8fcc080-...csv` is Level 03 (Questionnaire 5.1, Block 4 items 1-28), one row
  per household (276,409 rows), carrying "primary source of energy used for
  cooking (code)". Same records as `ms51l03.TXT` in the `.rar`.
- **Fuel codes** from the Vol-II field instructions (Schedule 5.1, item 16):
  LPG = 02; the clean-fuel set is {02, 03, 07, 08, 10, 11}.
- **Weight rule.** `wt = MULT/100` gives 265.1 M households and reproduces the
  MIS press-note clean-fuel shares (rural 49.8 / urban 92.0 / total 63.1; we get
  49.77 / 91.95 / 63.14 -- the press-note denominator excludes "no cooking
  arrangement" households, which 25 replicates for the check).
- **District key.** Built from Appendix I of the Vol-I instructions (parsed,
  then hand-checked). Counts, precisely: the key covers all 685 districts of
  the NSS 2020 frame; 684 (state, district) code pairs are actually observed
  in the unit data, and after collapsing post-2011 splits they map onto 639
  distinct census-2011 districts (of 640 -- Pondicherry district was not
  surveyed). 47 post-2011 districts are
  folded into their dominant 2011 parents (`match_type = "parent2011"`, with a
  `note` documenting every judgment call, incl. multi-parent splits like Morbi/
  Botad/Mahisagar/Siddipet, and the two Telangana districts missing from
  Appendix I whose codes were inferred from the NSS region in the data).

## Analytical conventions (mirror the existing pipeline)

- Main spec = unweighted glmer with state/district/FSU random effects
  (`district_estimates_glmer`), plus design-weighted direct estimates
  (FSU-clustered, stratum-stratified, Taylor SEs) as sensitivity -- exactly the
  01/03 pattern, so 26's comparisons are estimator-matched by construction.
- All households kept in the LPG denominator ("no cooking arrangement" = not
  LPG), matching how 01 treats NFHS.
- Rural = Sector 1; rural design-weighted estimates use domain estimation on
  the full design (no pre-subsetting), as in 01/03.

## Caveats to carry into the writeup

- **Timing.** NSSO-78 fieldwork Jan 2020 - Aug 2021 (COVID-extended) is centred
  about a year after NFHS-5 and IRES; with PMUY-era growth, part of any
  NSSO > NFHS-5 gap is real change, not measurement difference.
- **Estimand.** All three surveys ask the household's *primary/main* cooking
  fuel (good comparability; this is NOT the factsheet "clean fuel" indicator).
  One nuance: NFHS hv226 code 2 is "LPG/natural gas" in some phase-7 recodes,
  while NSSO separates PNG (code 03, 0.5% nationally, metro-concentrated).
- **District sample sizes.** MIS treats districts as strata (designed for
  district estimates), but the median district has ~320 households (min ~21) --
  report `n_nsso` alongside estimates and prefer the glmer partial-pooling
  estimates for maps, as elsewhere in the pipeline.
- **IRES comparisons** inherit IRES's limits: ~150 districts, state- (not
  district-) representative sampling.
