# Adult-outcome definitions (Table S5, SI Figures S7-S9)

The adult outcomes in `scripts/H4_si_adult_health.R` are flags stored in
`DHS_India/cvd_load.RData`, which this repository does not build.

## Provenance (traced 2026-10-21)
- `cvd_load.RData` is written by `DHS_India/India_CVD.Rmd` (`save.image`, ~line 355).
  It row-binds the women's and men's person files and keeps respondents who
  have lived at their current residence for at least five years (v104 >= 5 and
  != 96) and are not currently pregnant.
- NFHS-5 person files: `DHS_India/India2019.Rmd` (women ~1670-1790, men ~1930-2055).
  - hypertension1_23 = mean of BP readings 2 and 3 with systolic >= 140 or
    diastolic >= 90, OR told by a health worker of high BP (sb20/smb20), OR
    taking BP medication (sb21/smb21).
  - diabetes = self-reported diabetes (s728a/sm627a) OR glucose > 126 mg/dL if
    more than 8 hours since food or drink, > 200 mg/dL otherwise.
- NFHS-4 person files: generating script NOT recovered. The same logic, with
  NFHS-4 item names (sb18/sb19; s723a/sm622a; sb70/smb70), appears in
  `DHS_India/india_DHS_precisionwt.Rmd`, which saves to a different folder.

## Verification (`scripts/check_cvd_definitions.R`, run 2026-10-21)
Output: `check_cvd_definitions_output_2026-10-21.txt`.
- Sample: women 15-49, men 15-54; residence >= 5 years; no pregnant women. Both rounds.
- Diabetes: recomputed from the stored glucose, time-since-eating and
  self-report columns, it matches the stored flag in every row in both rounds
  (R `|` semantics: missing only when one component is missing and the other is
  not positive). Thresholds are strictly greater than 126 / 200 mg/dL.
- Hypertension, measured part: every respondent with raised measured BP
  (readings 2-3) is flagged.

## Self-report verification (`scripts/verify_hypertension_selfreport.R`, run 2026-10-21)
Output: `verify_hypertension_selfreport_output_2026-10-21.txt` and
`adult_hypertension_verification.csv` (counts only).
- Every analysis-file respondent was linked to the raw recodes by caseid /
  mcaseid: NFHS-4 644,852 of 644,852 (IAIR74FL, IAMR74FL); NFHS-5 685,435 of
  685,435 (IAIR7EFL, IAMR7DFL).
- Recomputing hypertension = (mean of readings 2-3 >= 140/90) OR told high BP
  (NFHS-4 sb18/smb18, NFHS-5 sb20/smb20) OR taking BP medication (sb19/smb19;
  sb21/smb21), with the flag missing if ANY of the four components is missing,
  reproduces the stored hypertension1_23 exactly in both rounds: agreement
  1.00000 on 622,005 (NFHS-4) and 617,700 (NFHS-5) rows, 0 missing-value
  mismatches. The coalesce rule of india_*_precisionwt.Rmd does NOT reproduce
  the missing pattern; the strict rule (as in India2019.Rmd) exactly reproduces
  both files. Matching outputs cannot identify the original generating script.
- All 41,746 (NFHS-4) and 30,738 (NFHS-5) respondents flagged despite normal
  measured BP reported a diagnosis or medication (100%); no respondent with
  normal BP who reported either was left unflagged (0 of 511,280 and 0 of 511,417).
- Consequence of the missing rule: respondents missing either BP reading or
  either self-report item are excluded (22,847 NFHS-4, 67,735 NFHS-5
  respondents, before the rural restriction), even if self-report alone would
  qualify them.

## Status
Both adult-outcome classifications are verified for both rounds. The NFHS-4
generating script itself is still not recovered. What was checked: hypertension
was recomputed from the blood-pressure averages stored in cvd_load.RData
(systolic23, diastolic23) plus the diagnosis and medication items retrieved
from the raw recodes; diabetes was recomputed from components stored in
cvd_load.RData (glucose level, time since eating/drinking, self-report). This
verifies the classification rules applied to those inputs; it does not
independently reconstruct every measurement (e.g. the BP averages or the
glucose values) from the raw files.

Limitation retained in the manuscript: the hypertension composite is missing
for 3.5% (NFHS-4) and 9.9% (NFHS-5) of respondents in these frames, before the
rural restriction. Exact reproduction confirms the coding, not that these
exclusions are unbiased.

## Residence flag and sex composition (checked 2026-10-21)
A code review flagged that H4 derived rural residence from `v025` only, which
would drop men from the rural frame if their residence were held elsewhere
(`mv025`/`hv025`). H4 now uses whichever of v025/mv025/hv025 exist, row by
row (first non-missing value per respondent), after checking the 1/2 coding
and that overlapping sources agree, and writes
`diagnostics/adult_sample_by_sex{,_all}.csv` (also in `results/diagnostics/`).
Rerun: session-20261021233620 via `scripts/run_adult_rerun.sh`; committed in
28ac8cd (release receipt "MIXED(2)", which documents the separate sessions).

Sample (rural analysis frame, respondents):

| Round | Men | Women | Men's share before / after residence restriction |
|---|---|---|---|
| NFHS-4 | 71,873 | 390,961 | 0.159 / 0.155 |
| NFHS-5 | 72,542 | 448,195 | 0.141 / 0.139 |

No respondent lacks residence. Outcome-specific non-missing counts by sex are
in the sample-by-sex CSVs.

Comparison with the outputs of commit c2aa7de (before the guard): the four
adult result files are byte-identical (same point estimates, CR2 intervals,
p-values and n).

| File (results/) | Git blob (c2aa7de = 28ac8cd) | SHA-256 |
|---|---|---|
| si_adult_health_effects.csv | 42cbc95f67a4499618ee3f8c355dedbbda5858f5 | e6972ee987993dceae8a1e1e15161a923790a9cea26409ce0c5c037b0676d647 |
| si_adult_health_effects_all.csv | 9f7344901ea361ff6930445edb6752b6ec129169 | e1735d64e9d687d7bfa1a1fe2faa06a17c2382fb28220fc03751be7368f18f42 |
| si_adult_health_prevalence.csv | df3af23205b9643645322938c64078115ff0b15b | 6495fa51d33866037479871b5405704661115f04a84d578dd63c665b33ab4b16 |
| si_adult_health_prevalence_all.csv | e963bec15438bf04155c26327e1e05972ec721b8 | f7cf518f0457cb19eedf32d7555a0b64409fb82da3740906b5a22bb8e1318b66 |

Conclusion: men were not excluded by the earlier code. The evidence is the
committed sample counts and the identical respondent selection implied by
byte-identical outputs. The guard is retained. The adult results keep their
exploratory interpretation and the hypertension missingness limitation.
