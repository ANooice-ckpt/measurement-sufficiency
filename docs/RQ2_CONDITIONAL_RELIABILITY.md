# Active RQ2 conditional-reliability analysis

The scientific question is: **can available context identify when a candidate
representation is more or less reliable at a specified tolerance?** This extends
Fig.3's contextual structure to held-out reliability, rather than requiring
context to reconstruct signed exposure errors.

## Analysis

For each of 52 daily metrics and eight daily anchor contrasts, D=abs(z) retains
the frozen RQ1 scale and maximal metric-specific support. Two LIGHT-only targets
are unavailable: 414 of 416 tasks are estimable. No human-state outcome matching
is used. Context is the existing common 18 daily + 32 daypart dictionary;
measurement information is the candidate target plus 16 candidate-only hourly
signatures. Neither high-information exposure state nor participant/site/date
identifiers enter model matrices.

The primary target is Pr(D>epsilon), evaluated with Brier score at all six
prespecified tolerances 0.05/0.1/0.2/0.3/0.5/1. Scores average equally over these
slices and then over available metrics. This finite-grid score is not continuous
CRPS and the grid does not define universal adequate measurement. Mean D is fitted
alongside the probabilities to define common context-risk groups.

Each continuous predictor uses natural-spline knots at training quantiles 1/3
and 2/3; tied/boundary knots are removed. Binary/low-cardinality features use a
linear basis. Imputation, missingness indicators, centering and scaling are
training-only. Ridge complexity is fixed at ten effective degrees of freedom
excluding the intercept (capped at 80% of numerical rank). No outer-score tuning,
metric-specific features or transition-specific model choices are used.

The measurement fit remains unchanged when a second ten-df context block is
added. Both context features and response residuals are residualized against
the measurement fit on training data. This is regularized residualization, not
an exact conditional-independence test or causal orthogonal score. Extra-context
coefficients describe a tested prediction procedure, not mutual information.
Probabilities are clipped and projected by squared-distance isotonic projection
to decrease with increasing tolerance. Mean risk is nonnegative.

Controls are the outer-training configuration mean, outer-training site means,
and a twenty-df measurement-only model. The last control is important: optical
context gains over the ten-df baseline do not exceed a stronger measurement-only
decoder. A positive context increment alone must not be called proof of unique
information unavailable to every measurement-based estimator.

## Validation and interpretation

Three complete participant-grouped, site-stratified five-fold partitions are
evaluated. The primary partition is the frozen upstream recovery partition;
the two additional seeds are 20260915 and 20260916. Every information state uses
the same held-out rows. Bootstrap intervals use 1,000 paired participant draws
within site and preserve metric-specific supports. They condition on fitted
predictions; repeat partitions separately assess fitting/partition instability.
They are not independent dataset replications or confirmatory external validation.

Context groups use cutpoints in outer-training predicted mean D. The same
cutpoints label held-out participants' days as lower/middle/higher risk at every
tolerance. Groups are target/contrast-specific under one common fitting and
cutpoint rule, not one universal partition of dates across all metrics.
Actual group coverage is retained. Groups describe relative risk,
not universally acceptable/unacceptable days, and do not redefine the full cohort.
The validation domain is new participants in the observed site mix. Full-day
diaries/dayparts cannot support a claim to choose past sampling rates in real time.

Daily exceedance probabilities are distinct from RQ3's maximum mean absolute
change over all higher configurations, including duration. The bridge is that
the reliability of a specified target/tolerance depends on observable conditions;
RQ3 then determines burden in its own frozen configuration/support domain. Neither
estimated risk nor historical recovery residuals identify an irreducible floor.

## Running and artifacts

```sh
RQ2_RELIABILITY_WORKERS=12 Rscript scripts/12d_rq2_recovery.R --run
Rscript scripts/13b_plot_fig4.R
```

Workers default to 12 on Windows and 36 on Linux, bounded by physical cores and
environment overrides. Core/RQ1/Fig.3/RQ3 are not rebuilt. No XGBoost installation
is needed for the active analysis. The current run reuses the immutable input
export under `results/rq2/recovery/.../bc25abee872aed4cd609ce7233fcc7a5/inputs`.
Fresh builds can export identical inputs from frozen RQ1/Core/context without
fitting historical recovery models. `RQ2_RELIABILITY_INPUT_RUN` selects an explicit
export when several exist. Incompatible upstream scientific versions are rejected.

Versioned outputs live under `results/rq2/reliability/<rq1_version>/<run_id>/`.
The stable plot input is `results/rq2/rq2_conditional_reliability.rds`. Provenance
records source hashes, code hashes, all folds, dictionary, R/packages and rules.
Each task checkpoint is installed atomically and reused only for its run ID.
Completed fits can be summarized without refitting:

```sh
RQ2_RELIABILITY_RUN_DIR=<run_dir> Rscript scripts/12d_rq2_recovery.R --summarize
```

The manifest distinguishes fitting-code provenance from later summary-code hashes.
Outputs include task scores, participant loss sums, group profiles/coverage,
fold audits, repeated-partition scores and bootstrap intervals. `13b` only reads
frozen outputs and writes Fig.4 plus metric-heterogeneity and nuisance-control
supplements. Its filename and registry identity are unchanged.

Additional frozen-fit diagnostics:

```sh
Rscript --vanilla scripts/diagnose_rq2_reliability_support.R
```

This writes full context-composition profiles, group-contrast intervals and a
fixed first-paired-day calibration diagnostic. The latter uses later days only
and is a different information setting, not the primary new-participant result.

## Evidence and limitations of this revision

Across three partitions context-only Brier skill is positive for all eight
contrasts. Primary skill is 1.50–2.78%; all primary participant-bootstrap intervals
exclude zero. Context also beats site-mean controls (1.84–2.86% primary skill).
Lower-risk contexts have 17–29% less mean absolute distortion than unstratified
samples; higher-risk contexts have 14–37% more. These are OOS risk-stratification
effects, not distortion removed by calibration.

At epsilon=0.2, observed lower/higher context-group exceedance rates are 16.4/28.9%
for LIGHT, 36.6/46.6% for chest, 45.0/56.3% for wrist, and 22.2/30.1% for 120 s.
Predictions partly shrink the low/high separation; exact conditional calibration
or individual guarantees are not established.

Added context over the ten-df measured baseline has primary Brier gains 0.676%
(LIGHT), 0.340% (chest), 0.300% (wrist); temporal gains are near zero. LIGHT and
chest primary intervals exclude zero, wrist does not. With the stronger
twenty-df measured baseline, neither broad optical nor temporal superiority is
supported; placement point gains remain modest and intervals include zero.
Accordingly the supported claim is **stable contextual reliability stratification
with selective, model-dependent conditional increments**, not broadly large
post-acquisition gains or demonstrated adaptive burden savings.
