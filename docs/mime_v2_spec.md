# Outcome-conditioned imputation, version 2: specification, pilot plan and stopping rule

Status: amended 21 Oct 2026 after the collaborator's review of 4ba8817 (Section 8 lists the amendments). The extraction script and Stage A are approved for implementation; the real-data health analysis stays on hold. Supersedes the sampler in `scripts/mime.R` (commits 0a92c7a–c521def). That sampler's results (the H2 MI rows, the script-22 frontier and the script-23 MI rows of run 147c80a) are rejected because they failed their convergence diagnostics:
- acceptance of moves between calibration draws was about 0.2%;
- each run used 2–8 distinct calibration draws;
- R-hat reached infinity and ESS was 5.

This document specifies one bounded redesign. If the redesign fails the stopping rule in Section 5, development stops for this paper and the health section is reported as in Section 6.

## 0. Corrections to the earlier assessment

- **The κ curve.** A stronger association as κ increases is not by itself evidence of error, because correcting measurement error can strengthen an association. What disqualifies the earlier results is the failed convergence: stuck chains, extreme R-hat and tiny ESS.
- **State-level selection** is a plausible explanation, not a demonstrated cause. Independence proposals of a whole high-dimensional calibration draw can produce near-zero acceptance even under a correctly specified model. Both the likelihood (no state dependence) and the proposal mechanism are addressed below, and the pilot is designed to tell them apart (Section 4.2).
- **Exposure-only imputation is not validated inference.** It stays as a comparison with a known attenuation concern. Good mixing in the fixed-calibration scenario is computational performance, not evidence of coverage or of correct specification.

## 1. Notation and data

- **Districts and states:** districts d = 1..n (n = 626 in the primary rural infant analysis), in states s(d), with S = 35 states.
- **Rounds:** t ∈ {15, 19}.
- **Calibration fits:** fit A calibrates the 2015 round (NFHS-4 against ACCESS W1; 6 calibrated states). Fit B calibrates the 2019 round (NFHS-5 against IRES; 19 calibrated states). For each fit f, C_f is the set of calibrated states and N_f = {1..S} \ C_f the rest.
- **NFHS inputs:** x̂_dt is the NFHS district logit and s_dt its standard error.
- **Outcome and covariates:** Y_d is the district change in infant (or neonatal) mortality per 100 births. Z_d holds the adjustment covariates and region indicators. w_d is the birth weight (harmonic mean), normalised to mean 1.

## 2. The model

### 2.1 Exposure (calibration) part: unchanged from the published correction

For fit f, the brms measurement-error model gives a joint posterior over

  φ_f = (a_f, b_f, τ_f, {u_fs : s ∈ C_f}, σ_f)

where:
- a_f is the intercept and b_f the slope on the true NFHS logit;
- τ_f is the SD of the state effects;
- u_fs are the estimated state effects of the calibrated states;
- σ_f is the residual (equation-error) SD.

This posterior is represented by K saved draws φ_f^(k), k = 1..K. Each draw keeps the dependence among intercept, slope, τ and the calibrated-state effects.

**States outside the calibration sample.** For s ∈ N_f, the state effect is not identified by the calibration data. Given φ_f, its distribution is u_fs | φ_f ~ N(0, τ_f²), independently across s. This is exactly what brms samples for new levels (`sample_new_levels = "gaussian"`). Write u_fs = τ_f · η_fs with η_fs ~ N(0, 1) independently; this non-centred form is what makes state-by-state updating legitimate (Section 3).

**True NFHS values (joint two-round shrinkage, as in 05).**
- (X_d,15, X_d,19) | x̂ ~ N(m_d, V_d), the bivariate empirical-Bayes posterior with the estimated between-round correlation ρ̂ = 0.835.
- Write X_dt = m_dt + v_dt^{1/2} z_dt, where (z_d,15, z_d,19) ~ N(0, R_d) and R_d has the posterior correlation r_d.
- ρ̂ is treated as known (empirical-Bayes approximation, stated in the paper).

**Corrected logit and exposure.**

  L_dt = a_f(t) + b_f(t) X_dt + u_f(t),s(d),  with f(15) = A, f(19) = B
  Δ_d = 100 · [logit⁻¹(L_d,19) − logit⁻¹(L_d,15)]

This is the same quantity the published corrected change uses: a transformed draw of the latent calibrated logit, a + b X + u, excluding the equation error σ_f. Its logistic transform is not an exact probability-scale conditional expectation; script 23 reports the probability-scale and posterior-predictive alternatives.

### 2.2 Mortality part: NEW, with state dependence

  Y_d = β Δ_d + Z_d γ + v_s(d) + ε_d
  v_s ~ N(0, ω²)
  ε_d ~ N(0, ψ² / w_d)

Priors:
- β, γ: flat;
- p(ψ²) ∝ 1/ψ²;
- ω² ~ scaled-inverse-χ²(ν₀ = 1, s₀² = Var(Y)/4), a weakly informative choice. The sensitivity of the results to s₀² is reported (×¼, ×4).

The state random intercept v_s is the change from version 1. A state-level pattern in mortality can now be absorbed by v_s, rather than only by selecting exposure draws whose state effects happen to match it. The model assumes the v_s are independent of exposure and covariates. It accommodates clustering; it does not by itself remove state-level confounding.

### 2.3 Target distribution of the imputations

  p(φ_A, φ_B, {η_As : s ∈ N_A}, {η_Bs : s ∈ N_B}, {z_d}, β, γ, ψ², ω², {v_s} | survey data, Y)
    ∝ p_cal(φ_A) p_cal(φ_B) ∏ N(η; 0, 1) ∏_d N(z_d; 0, R_d) × p(Y | Δ(·), β, γ, v, ψ²) p(v | ω²) p(β, γ, ψ², ω²)

Here p_cal is the calibration posterior, represented by the saved draws. Imputations are draws of Δ from this distribution's marginal.

**Analysis of each completed dataset is unchanged:** weighted lm with CR2/Satterthwaite inference, pooled by Rubin's rules with Barnard–Rubin degrees of freedom. The imputation model now contains state dependence, but it is still a parametric model, whereas the analysis is design-robust (CR2). Compatibility of the two is therefore checked empirically (Section 4), not assumed.

## 3. Sampler (Metropolis-within-Gibbs), one sweep

1. **(β, γ) | rest.** Exact conjugate normal draw. The response is Y − v_s(d) and the weights are w_d/ψ².
2. **ψ² | rest.** Exact scaled-inverse-χ² draw from the weighted residuals.
3. **v_s | rest, for each state.** Exact normal draw (conjugate); 35 independent draws.
4. **ω² | v.** Exact scaled-inverse-χ² draw.
5. **District errors (z_d,15, z_d,19) | rest, per district.**
   - Independence Metropolis, proposing from N(0, R_d) and accepting with the outcome-likelihood ratio.
   - Version 1 accepted about 95% of these moves.
   - Valid because the z_d are a priori independent across districts and the likelihood factorises given everything else.
6. **η_fs | rest, for s ∈ N_f, state by state.**
   - Random-walk Metropolis on η_fs, with target N(0, 1) × the outcome likelihood of the districts in state s. The acceptance ratio includes the normal prior ratio exp((η² − η'²)/2).
   - Because each η_fs enters only its own state's districts, all states can be proposed and accepted simultaneously from per-state log-likelihood sums.
   - Proposal SD tuned during burn-in toward roughly 40% acceptance, then frozen.
   - Valid because, given φ_f, the η_fs are a priori independent N(0, 1) and enter the likelihood only through their own state's districts.
7. **φ_f | rest (calibration draw index), for f ∈ {A, B}.**
   - Independence Metropolis over the K saved draws, with the η held fixed. In version 1 the new-state effects were part of the draw; here they move with the draw's own τ_f.
   - Several proposals per sweep.
   - Acceptance = outcome-likelihood ratio. This is valid because the proposal is the calibration posterior itself, the target's prior factor for φ_f.
   - For the instrument-consistent sensitivity, both rounds come from fit B, so a single shared φ_B is used.

Step 6 is the main algorithmic change. It removes the hundreds-of-dimensions independence proposal that version 1 used for the state effects of non-calibrated states, which were about 63% of districts in 2015. What remains for step 7 is a low-dimensional move: an intercept, a slope, a τ and the calibrated-state effects.

**Per-fit decomposition needed.** The sampler needs a_f, b_f, τ_f, the calibrated-state u_fs, and σ_f per saved draw, plus the district-to-state map and m_d, V_d, r_d. A new script, `05b_imputation_components.R`, extracts these from the cached brms fits (no refit) and from `me_shrinkage.csv`. It writes `imputation_components_v2.rds`. Script 05 itself is not rerun.

**κ (for script 22, later; not part of the pilot).**

  L_dt(κ) = L̄_dt + κ (L_dt − L̄_dt)

applied to the full exposure prior, with the imputations then conditioned on Y. κ = 0 holds the fitted corrected surface fixed. Intermediate values are a model-based sensitivity analysis, not a survey design.

## 4. Pilot: synthetic data first, no pipeline run

### 4.1 Data-generating process

- **Design:** the real design, with 626 districts, 35 states, the real covariate matrix Z, the real weights, and the real calibrated-state sets C_A and C_B.
- **Calibration posterior:** the empirical joint draws of the real fits (K = 4,000 per fit, extracted by `05b_imputation_components.R`). These keep every dependence among intercept, slope, state-effect SD, calibrated-state effects and σ.
  - In each replicate the "true" calibration is one of those draws, chosen at random (a conditional simulation).
  - The true effects of non-calibrated states are τ_f^(k)·η, with η ~ N(0, 1).
  - The methods then use the full set of saved draws, as on the real data.
  - (The values quoted earlier, 0.16 and 0.51, were posterior means of the state-effect SD τ_f, not posterior SDs. Using the empirical draws removes the need for synthetic values.)
- **NFHS inputs:** the true values are X = m + V^{1/2} z, with z ~ N(0, R_d), from the real joint-shrinkage moments.
- **Mortality:** Y = μ(Δ_true) + Z γ̂ + v_s + ε, with ω = 0.71 and ψ = 0.98 (fitted to the real outcome) and γ̂ fitted to the real outcome. β₁ = 2 × the fixed-surface estimate.
- **Scenarios:**

| Scenario | μ(Δ) | Values | Role |
|---|---|---|---|
| S1 correct | β Δ | β ∈ {0, β₁} | bias, coverage; false-positive rate at β = 0 |
| S2 calibration bias | β Δ, with the true logit deviations from each round's mean 1.2 times the posterior draw's | β ∈ {0, β₁} | as S1 |
| S3a nonlinear | 2β (Δ − median Δ)₊ | β ∈ {0, β₁} | as S1; at β = 0 every exposure slope is zero |
| S3b heterogeneous | (β₁ + δ_s) Δ, δ_s ~ N(0, (0.5 β₁)²) | non-zero mean only | recovery and coverage of the projection target; NOT used for false positives |
| S3c heterogeneous, zero mean | δ_s Δ, δ_s ~ N(0, (0.5 β₁)²) | — | reported separately, with its own projection target (generally non-zero); NOT counted as a false-positive experiment |

  - **False-positive experiments** use only S1, S2 and S3a at β = 0, where every exposure slope is exactly zero.
  - **Target in S3a, S3b and S3c:** in each replicate, the weighted, covariate-adjusted linear projection of the complete simulated exposure-effect function, including any state-specific slopes. That is, the coefficient on Δ_true in the weighted regression of μ(Δ_true) on Δ_true and Z, using the analysis weights.

### 4.2 Stage A: convergence

- **Datasets:** 10 per cell, for S1, S2 and S3a at β = 0 and β₁ and for S3b; 70 in all.
- **Run settings:** 4 independently initialised chains, 50 retained imputations per chain (M = 200), at two run lengths:
  - short: burn-in 1,000 sweeps, thin 10;
  - long: burn-in 4,000 sweeps, thin 40.
  - Each sweep makes 5 calibration-index proposals per fit and 2 district-error proposals per district.
- **Quantities monitored:**
  - β (imputation model);
  - a_f, b_f and τ_f in use;
  - ω and ψ;
  - the mean and SD of Δ;
  - mean |η| over the non-calibrated states;
  - the completed-data CR2 coefficient.
- **Pass criteria:** every monitored rank-normalised split R-hat ≤ 1.05 and every bulk ESS ≥ 100 at the long run length, in at least 95% of datasets (67 of 70). The pooled estimate must also be stable between the two lengths, differing by less than 0.25 of its SE.
- **Algorithm comparison (2 × 2):** each Stage-A dataset is also fitted with:
  - (i) the state random intercept present or absent;
  - (ii) η updated statewise (Section 3, step 6) or as one block. The block version proposes all η_fs jointly from their N(0, 1) prior and accepts with the joint outcome-likelihood ratio.

  All four combinations target their own model's posterior correctly. The two η algorithms target the same distribution, so their difference is purely computational; the state-intercept factor changes the model. The comparison indicates, but cannot prove, what caused version 1's failure.

### 4.3 Stage B: recovery and coverage

Run only if Stage A passes.

- **Replicates:** 400 per cell, for S1, S2 and S3a at β = 0 and β₁, plus S3b and S3c.
- **Methods:** fixed point surface; exposure-only MI (M = 200); version-2 conditioned MI. All are analysed with CR2 and Rubin pooling.
- **Version-2 settings:** the configuration that passed Stage A (by default the long run length), M = 200 (4 chains × 50).
- **Mixing threshold per replicate:** every monitored R-hat ≤ 1.05 and bulk ESS ≥ 100, including the completed-data coefficient. A missing value counts as a failure.
- **Reported per cell and method:**
  - attempted, failed and poorly-mixed counts, and the fraction unavailable;
  - for successful fits: bias with its Monte Carlo SE, empirical SD, mean SE, mean CI width, and coverage and rejection rate with Wilson 95% intervals;
  - for version 2, the same summaries among well-mixed fits, alongside the overall summaries and never instead of them.
- **Monte Carlo precision:** 400 replicates gives a Wilson half-width of about ±2.2 points at 95% coverage and ±2.1 at 5% rejection.
- **Checkpointing:** every replicate is written to its own file (results, failures, diagnostics, seed, configuration hash) as soon as it finishes. A resume mode skips completed replicates. Stage B runs as a standalone job with progress logging, separate from the pipeline and from document generation.

## 5. Stopping rule (prespecified)

Version 2 proceeds to the real data, starting with the primary infant model and then the remaining rows and the κ frontier, only if ALL of the following hold:

1. **Convergence:** Stage A passes (Section 4.2).
2. **S1 (correct model):**
   - |bias| < 10% of β₁, with the MC interval for the bias including 0 or lying within ±10%;
   - coverage: the Wilson 95% interval at β₁ overlaps [0.93, 0.97], and the point estimate is ≥ 0.92;
   - rejection at β = 0: the Wilson interval's lower bound is ≤ 0.05 and the point estimate is ≤ 0.075.
3. **S2, S3a and S3b (misspecified):**
   - coverage of the cell's target is no worse than the better of the two comparator methods by more than 5 points;
   - the false-positive rate at β = 0 (S2, S3a) is ≤ 0.10.
   - S3c is reported but not used in the rule.
4. **Failures:** for EACH method separately, fewer than 2% of replicates fail (error or non-finite result) in any cell. In addition, fewer than 2% of version-2 replicates mix poorly. Performance is judged among successful fits, reported alongside the fraction unavailable.

On the real data, the same convergence criteria (R-hat ≤ 1.05, ESS ≥ 100 on every monitored quantity, including the completed-data coefficient; missing values count as failures) must hold for every reported row. Any row that fails is not reported as a result.

**If any condition fails:** development of outcome-conditioned imputation stops for this paper. There is no version 3.

## 6. Reporting

- **If version 2 passes:**
  - It becomes the propagated estimate.
  - The fixed surface and exposure-only MI are reported alongside as comparisons with their known limitations.
  - The four-way shrinkage × propagation table and the validation results go in the SI.
- **If version 2 fails:**
  - The fixed-surface association is presented as a conditional, exploratory analysis.
  - Exposure-only MI is presented as a sensitivity analysis with a known attenuation concern.
  - The attempted conditioned imputation is described with its diagnostics.
  - Claims that the precision frontier establishes what better surveys could achieve are removed.
  - The methods are never presented as equally validated alternatives.

In both cases the survey-comparison, calibration and stacking sections proceed now. The health section stays provisional until this is resolved.

## 7. Scope limits

- **One redesign, as specified here.** Changes after Stage A are limited to sampler tuning (proposal SDs, number of index proposals, run length), and each change is recorded.
- **No real-data pipeline run** until Section 5's conditions 1–4 hold.
- **The published corrected surfaces and maps** come from the survey-only correction and are unaffected.

## 8. Amendments after review of 4ba8817

1. **Null scenarios are genuinely null.** Every exposure slope is exactly zero in the false-positive experiments. Heterogeneous effects are separate scenarios (S3b, S3c), with explicit projection targets.
2. **The algorithm comparison keeps the target distribution.** "Without step 6" is replaced by a block prior proposal for all η, with the correct acceptance ratio, inside a 2 × 2 comparison (state intercept × η algorithm).
3. **The simulation uses the empirical joint calibration draws,** preserving every dependence among the calibration parameters. τ values are clarified as posterior means of the state-effect SD.
4. **Run lengths, retained imputations, mixing thresholds and per-method failure rules are specified.** The heterogeneous-scenario target is defined as the weighted, covariate-adjusted linear projection, including state-specific slopes.
5. **Clarifications:**
   - the state intercept accommodates clustering but does not remove state-level confounding;
   - the exposure is a transformed latent-logit quantity excluding equation error, not an exact probability-scale expectation;
   - the η Metropolis ratio includes the normal prior ratio.

## 9. Stage A result and tuning log (21 Oct 2026)

**Stage A (commit 2caea13, run on the Mac, 560 runs, no failures): FAILED.**

| Variant | Run length | Datasets mixed | Median max R-hat | Median min ESS | Median calibration-index acceptance |
|---|---|---|---|---|---|
| state intercept, statewise η (primary) | long | 46 / 70 (65.7%) | 1.03 | 140 | 0.32 |
| state intercept, statewise η | short | 28 / 70 | 1.05 | 71 | 0.31 |
| state intercept, block η | long | 40 / 70 | 1.04 | 135 | 0.33 |
| state intercept, block η | short | 22 / 70 | 1.06 | 72 | 0.33 |
| no state intercept, statewise η | long | 0 / 70 | 6.6 | 5 | 0.002 |
| no state intercept, block η | long | 0 / 70 | Inf | 5 | 0.006 |

- **Verdict:**
  - mixed share 0.657 against a required ≥ 0.95;
  - stability between run lengths 0.943 against a required ≥ 0.95.
- **Where it fails (primary variant, long run):**

  | Cell | Mixed |
  |---|---|
  | S1 β = 0 | 10/10 |
  | S2 β = 0 | 10/10 |
  | S3a β = 0 | 8/10 |
  | S1 β₁ | 6/10 |
  | S2 β₁ | 3/10 |
  | S3a β₁ | 5/10 |
  | S3b | 4/10 |

  - The quantities that fail are mostly fit B's calibration parameters (τ_B, b_B, a_B), the SD and mean of the imputed change, and the completed-data coefficient.
  - Mixing fails mainly when the outcome carries an exposure effect. The outcome then informs the 2019 calibration, and the conditional posterior over the saved calibration draws concentrates.
- **2 × 2 reading:**
  - The state intercept is decisive: without it, no dataset mixes and index acceptance is 0.2–0.6%, as in version 1.
  - Statewise and block η updates perform similarly; statewise is slightly better.
  - This is consistent with version 1's failure arising mainly from the missing state dependence in the likelihood. It does not prove it.

**Tuning attempts (Section 7; synthetic data only; cloud spot checks on the hardest cell, S2 β₁, datasets 1–2):**

| Change | S2 β₁ result | Verdict |
|---|---|---|
| Index proposals per sweep 5 → 25 (long run) | max R-hat 1.10 / 1.07; min ESS 40 / 77 | not fixed |
| As above, with thinning 40 → 100 | max R-hat 1.11; min ESS 38 | not fixed |
| Diagnostic only, outside the approved sampler: an extra index move holding the non-calibrated state effects u fixed instead of η (with the Jacobian term) | max R-hat 1.26 / 1.13; min ESS 14 / 28 | worse |

The remaining failure is not resolved by the tuning the spec permits. Under Section 5 the conditions for proceeding are not met.
