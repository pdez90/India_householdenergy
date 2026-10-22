# FAILED CONVERGENCE -- NOT FOR INFERENCE

Records of the two outcome-conditioned imputation methods that were abandoned for
this paper (prespecified stopping rule, docs/mime_v2_spec.md Sections 5, 8, 9).
They document convergence only. No estimate from these runs is a result.

- v1_convergence_record.csv  version 1 (no state effect in the imputation model):
  per-analysis counts of runs passing R-hat <= 1.05 and bulk ESS >= 100 on every
  monitored quantity, computed from the archived diagnostics
  (outputs/archive_failed_convergence_mime_run_20261021/).
- v2_stageA_summary.csv, v2_stageA_verdict.csv  version 2 Stage A pilot
  (scripts/32_mime2_stageA.R; synthetic outcomes only).
- v2_stageA_cells.csv  mixed datasets per scenario cell and algorithm variant.
- v2_tuning_log.csv  tuning attempts after Stage A (synthetic data only).
- v2_spec_amendments.csv  why Stage A has 70 datasets, not 60, and when the
  amendment was made.

The manuscript builder reads these files only for the SI section documenting the
failure. Nothing in outputs/archive_failed_convergence_* is read by the builder
or the app.
