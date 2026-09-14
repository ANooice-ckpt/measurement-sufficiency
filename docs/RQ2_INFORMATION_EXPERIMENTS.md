# RQ2 information-value experiment log

## Scope and evaluation contract (2026-09-14)

The scientific target is usable information in configuration-induced distortion,
not a requirement that context must improve a particular score. Frozen Core,
RQ1, Fig.3 and RQ3 remain unchanged. All available 52 daily metrics and eight
single-axis anchor contrasts use the same dictionary and model rules; unavailable
LIGHT metrics remain unavailable. Historical recovery checkpoints are preserved.

The first experiments reuse the frozen participant fold map (five folds,
site-stratified). Every training/test division is by participant. Raw absolute
standardized distortion D=abs(z) retains the frozen RQ1 denominator. No outcomes,
target-side exposure summaries, identifiers, site or date enter a predictor matrix.
The 16 candidate-configuration signatures and 50 existing context features are
unchanged. Context-only, measurement-only (candidate target + signatures), and
their joint information set are scored on identical rows. A training-only null
is retained. Negative improvements are never clipped.

## Experiment 1: reuse historical observability predictions

`scripts/diagnose_rq2_information.R audit` reads the completed anchored recovery
run, and exports task-level risk-ranking curves and learner-selection frequencies.
Coverage levels are 0.2/0.4/0.6/0.8/1. Scores are ranked within test folds; tied
scores are fractionally included. This is explicitly a retrospective batch
ranking diagnostic, not a calibrated prospective acceptance policy. No test
outcome chooses a threshold or a model. At 40% coverage, adding context to the
historical signature state generally does not reduce retained absolute error;
the old predictions therefore do not alone establish independent context value.

## Experiment 2: matched information sets, different statistical targets

`scripts/diagnose_rq2_information.R experiment` uses an additive natural-spline
ridge model. Each numerical feature receives knots at its training 1/3 and 2/3
quantiles (duplicate/boundary knots removed), training median imputation, and a
missingness indicator where estimable. Complexity is fixed at ten effective
degrees of freedom, with the same rule for all tasks and information states.
There is no outcome-driven hyperparameter search. Binary features use their
linear basis. The repeated-quantile safeguard is a numerical implementation
correction, not feature selection.

One multiresponse fit evaluates mean D, log1p(D/training-mean-D), signed z, and
Pr(D>epsilon) for epsilon=0.05,0.1,0.2,0.3,0.5,1. These are diagnostic tolerance
slices, not universal adequacy cutoffs. Binary targets use squared/Brier loss;
predicted probabilities are projected to [0,1]. All targets are retained in the
outputs; no favorable tolerance or metric is selected for reporting. Log-risk
is only a ranking diagnostic and is not substituted for A or sufficiency.

Generated results and provenance are stored under
`results/rq2/information_diagnostics/<source_run_id>/`.

## Interpretation constraints

An OOS residual is not an identified irreducible information bound. A new
participant within the observed sites is not a new-site validation. Full-day
context supports retrospective quality assessment or planning for comparable
conditions, not a real-time claim to change past sampling. Selective acceptance
changes support; coverage and composition must accompany retained-risk results.
RQ3 remains max mean absolute change over all higher observed configurations;
daily reference-based risk is a bridge to its tolerance logic, not a replacement
for its temporal-by-duration sufficient region.

## Experiment 3: preserve the measured baseline

Directly concatenating all features under a fixed total complexity harmed
prediction (approximately -1.6% to -5.0% mean Brier gain relative to measurement).
An orthogonalized context block, fitted without changing the measurement model,
removed most of this competition. This structural comparison is retained under
`orthogonal_increment/`; it did not trigger metric-specific tuning. Its small
positive optical/placement increments motivated capacity and site controls,
not a claim that conditional information had been identified generally.

## Experiment 4: context-stratified cadence choice

`diagnose_rq2_policy.R` uses frozen OOS mean-risk predictions and a fixed
coarsest-passing-cadence rule at the full declared tolerance grid. It retains
metric-specific complete six-cadence support and audits the additional support
restriction. The study's actual daily context is available retrospectively;
this is not a prospective adaptive logger. Reference escalation has zero error
*relative to itself*, not resolved RQ3 sufficiency. The context policy did not
produce a compelling improvement in the error/sample-count frontier over
configuration-average risk. This route was not promoted into the main figure.

## Final fixed-rule reliability analysis and controls

The new `12d --run` fits mean risk plus tolerance-exceedance probabilities on
three grouped partitions (414 complete tasks, 2 unavailable). `13b` uses the
frozen manifest to show OOS context-risk strata, probability information value,
and tolerance-response curves. Monotone probability projection enforces a valid
ordering across tolerance. All six tolerances, eight contrasts and available
metrics remain in the output, including negative increments.

Context-only Brier skill is 1.50–2.78% in the primary partition, positive in all
three partitions for every contrast. Site-mean controls do not explain it away.
Conditional increments beyond measured exposure remain small. Stronger measured
decoders outperform the joint ten-df-plus-ten-df construction for optical and
temporal targets. Consequently the primary contribution is reliability
stratification, not a generalized recovery or acquisition-saving claim.

## Fixed first-day participant calibration

`diagnose_rq2_reliability_support.R` transfers the first paired day's signed
discrepancy to subsequent days, using the same rule for every target/contrast.
It also transfers the first-day residual of predicted mean risk. Both diagnostics
worsened pooled later-day errors in all eight contrasts. For example, chest
absolute error increased from 0.356 to 0.512 and LIGHT from 0.159 to 0.210 on the
same later-day support. This rejects the tested unshrunk one-day offset, not
all participant calibration or the existence of persistent heterogeneity.

## Numerical failures and reproducibility

R required a writable D:/Rtmp runtime directory outside its restrictive startup
sandbox. No scientific inputs were changed. Repeated spline quantiles, isotonic
projection orientation, a data.table validation selection, and a list/vector
checkpoint-path mismatch were corrected and tested. Completed model checkpoints
were reused for summary repair; model and summary code hashes are stored
separately. The original Fig.4 and plotting source are archived under
`results/rq2/information_diagnostics/`. Historical recovery, ranking diagnostics,
direct-concatenation scores, orthogonal-block scores and cadence-policy failures
remain available. This sequence is exploratory analysis development; repeated
splits are a stability assessment, not an untouched external test set.
