# Audit fixes and execution status — 2026-09-22

This revision separates defects in the current implementation from frozen
outputs that have not been regenerated. No production Core/RQ results or figures
were rebuilt as part of the code changes.

| Finding | Classification | Resolution |
| --- | --- | --- |
| 300-s duration values enter primary pairs and the shared scale anchor | Current code defect | Filter the frozen primary cadences before both operations; update the RQ1 cache version. Partitioned and in-memory inputs use the same rule. |
| Native 60-s source is labelled 10/30 s | Current code defect, confirmed on FUSPCEU S014 | Audit each required placement before alignment and completeness. Exclude an ineligible source only from supports requiring it, and preserve its reason in the source-sampling audit. |
| Fall-back hours are merged before duration IS/IV | Current code defect, reproduced against LightLogR 0.10.3 | Preserve exact hourly instants and local clock labels in the durable context; reconstruct IS/IV from that basis. Retain the 24 clock-hour profile for RQ2 signatures only. |
| Fig.4 numerical predictions differ from the corrected estimator | Frozen results not rerun | The SVD context projection and isotonic-before-clipping fixes already existed. Their numerical implementation is unchanged in this revision. |
| Fig.4 and summary-only execution accept the old estimator | Current compatibility-check defect | Introduce a shared semantic estimator version and validate the RQ1/core lineage. Old fits require `--run`; `--summarize` cannot upgrade them. |
| Duration resume reports missing/zero row counts | Current metadata defect | Persist row counts in completion markers and recover counts from same-version parts when necessary. |
| Adjacent higher window IDs refer to nonexistent windows | Current metadata defect | Emit the ID only when the longer window exists inside the run and configured duration domain. |
| Fig.2/Fig.5 old input versions and Fig.6 missing new inputs | Frozen results not rerun | Keep the existing rejection/missing-input behavior; no model or sufficiency change is needed for this status alone. |

## Scientific identities and rerun boundary

- Core: `v5_native10s_exact_hours__<core_design_id>`.
- RQ1: `rq1_v5_primary_duration_type_canonical__<core_version>__<analysis_design_id>`.
- Conditional reliability:
  `conditional_reliability_v2_orthogonal_bounded_projection__<analysis_design_id>`.
- Other downstream scientific versions incorporate the upstream RQ1/core identity,
  so their prior checkpoints cannot be reused as current results.

Native-source eligibility changes the available source cohort, and the exact
hourly basis adds information absent from the v4 context. These fixes therefore
require one versioned Core rebuild, followed by RQ1 and its inference extension,
RQ2 and conditional reliability, RQ3, and their figures. Merely rerunning plotting
or using `CORE_DURATION_ONLY=1` with v4 inputs cannot apply these fixes.
Existing v4 outputs remain historical; do not relabel them with the new version.

The established full entrypoint is `bash scripts/run_full_server.sh` on the pinned
R 4.5.0 server. `CORE_FORCE=0` is sufficient because the new version selects a
different cache directory. The source eligibility audit is emitted at
`results/diagnostics/core_source_sampling_audit.csv` and records each participant,
required placement, observed cadence, eligibility and reason by support.

## Bounded verification

The regression tests exercise production functions with synthetic observations
and temporary artifacts, without rebuilding production outputs:

- `validate_core_sampling_hours.R`: native cadence/support independence, exact
  sparse subsets, DST-aware IS/IV versus LightLogR, and resumed partition counts.
- `validate_duration_windows.R`: exhaustive contiguous windows and valid
  adjacency across short/long runs and alternative window limits.
- `validate_core_pipeline_contract.R`: support-audit persistence/resume and the
  production validator's version, source eligibility, participant cadence and
  window-adjacency checks.
- `validate_rq1_primary_duration.R`: partition/flat equivalence and exclusion of
  extreme 300-s values from both primary scales and duration changes.
- `validate_reliability_contract.R`: old estimator rejection, compatible frozen
  result acceptance, upstream consistency, and tolerance of source-only edits.

Numerical manuscript claims must be refreshed from the new production outputs
after that run; the pre-existing RQ2 numbers are labelled historical in
`RQ2_CONDITIONAL_RELIABILITY.md`.

The five new test scripts and five existing suites (`validate_engineering.R`,
`validate_inference_pipeline.R`, `validate_audit_fixes.R`,
`validate_rq3_support_pareto.R`, `validate_rq2_reliability.R`) passed under R 4.5.0
in an isolated temporary copy. Actual FUSPCEU S007/S014 checks also confirmed
the corrected eligibility and DST behavior; S007's IS/IV agreed with LightLogR
to less than 1e-12. No production result was overwritten.

An additional historical test, `validate_recovery_prototype.R`, still calls the
removed `recovery_predict()` interface and fails before reaching its signature
checks. This mismatch is already present in the pre-fix revision; the retired
prototype test was not changed or counted as a passing current-analysis check.
