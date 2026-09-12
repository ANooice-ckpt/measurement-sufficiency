# Independent RQ2 recovery prototype

`scripts/12d_rq2_recovery.R` is an opt-in downstream analysis. It never sources
`09`, `10`, `10b`, `12`, or `12c`, regenerates metrics/context, or changes figures.
Invocation without exactly `--check-inputs` or `--run` stops with usage help.

## Estimand and model

The eight participant-day contrasts and candidate/reference orientation come
from `rq1_inference_anchor_map()` and `rq1_inference_pairs()`. The 52 daily
representations retain their metric-specific maximal supports. LIGHT-only
MDER/nvRD remain unavailable. No duration or outcome matching is performed.

For each contrast/metric, the three predictions of the observed higher-information
representation are raw `Y_L`, calibration from `Y_L`, and context from `Y_L` plus
18 prespecified deployable context fields. The context whitelist is taken directly
from the external, microenvironment and behaviour families in
`rq2_context_features.R`. No RQ2 exposure-state predictor, other configuration's
representation, target-derived bin, participant ID, site ID, Date, distortion or
standardizer enters a design matrix. IDs/site are used solely for matching/CV.

Both fitted states use ridge regression with intercept unpenalized and fixed
`lambda=0.01`, minimizing mean squared training error plus the ridge penalty.
There is no full-data hyperparameter selection or fitted production model.
Linear metrics enter in their frozen representation units. Circular time uses
candidate sin/cos and a joint target sin/cos regression (86400-second period),
followed by atan2; predicted vectors with norm below 1e-10 fall back to raw and
are counted explicitly. No discontinuous clock-time regression is used.

Training-fold median imputation, missingness indicators, variance screening and
the existing `rq2_model_helpers()$scale_train_test` preserve held-out isolation.
Entirely missing context features and their constant indicators are removed
inside training folds. Missing context never removes a paired participant-day.
Model preprocessing and coefficients are stored per fold. Raw/calibration/context
have exactly the same held-out rows; an insufficient training fold makes the
whole task non-estimable, rather than silently selecting successful test folds.
Minimums: 20 eligible days/4 participants per task; 15 training days/3 training
participants in each realized fold. These are numerical prototype guards, not
scientific sufficiency thresholds. Small-site folds can be empty for a task.

One seeded, site-stratified participant fold map is shared across all tasks.
Every participant is held out as a whole. This evaluates new participants in
the observed site mix, not new-site transfer or prospective real-time prediction.
Rows are weighted equally in A, as in RQ1; participants with more days contribute
more to the descriptive loss. There are no iid-day confidence intervals.

Scoring uses absolute linear error or shortest circular distance divided by the
already-frozen RQ1 lattice/metric SD, never a refitted fold-specific denominator.
The scale is an evaluation anchor, not a predictor or tuning input. The code
checks per-row reconstruction of frozen z and checks that all-eligible raw A
reproduces the frozen RQ1 summary. `A_frozen_RQ1`, `A_raw_all_eligible`, and held-out
`A` remain separately named. Signed prediction error has prediction-minus-target
orientation, the reverse of canonical RQ1 delta; A is unchanged by this sign.

`delta_A = A_raw - A_rec`; positive means improvement. `G = 1-A_rec/A_raw` is
auxiliary, is not clipped (negative values indicate harm), and is NA when
`A_raw <= 1e-6` standardized units. The floor is configurable and recorded.
`context_increment = A_calibration - A_context` isolates extra context benefit.

## Required existing frozen inputs

| Input | Contract |
|---|---|
| `results/rq1/rq1_pairwise_change_long.rds` | Versioned partitioned manifest, current core/design identity, `part_manifest` with `part`/`dimension` |
| Manifest-declared `placement_optical_temporal` parts and their `.ok` markers | Fig.2 pair fields plus config IDs, lattice, scale anchor, availability, delta/z and versions; unique participant-day metric keys |
| `results/rq1/rq1_pairwise_summary.csv` | Same RQ1/core versions as manifest; unique anchor/metric summary and `A_mean_absolute` |
| `results/diagnostics/rq1_standardizer_audit.csv` | Unique lattice/metric/geometry scale, `scale_anchor_config`, `standardizer`; must numerically reproduce frozen z |
| `results/diagnostics/rq2_layered_context_day_features.csv` | Unique site/Id/Date and all 18 existing family fields; numeric or entirely missing |

Only non-duration parts are read. Their paths are taken exactly from the
manifest; a moved deployment must supply a coherent frozen artifact bundle.
No core cube, duration file, raw light series, diary RData, weather input,
Fig.2 inference result or old RQ2 model checkpoint is needed. Missing files,
incompatible versions, unsupported geometry, duplicate keys, unexpected support
or scale disagreement stop execution. No code path rebuilds an input.

The legacy context CSV lacks a scientific version field. Its content checksum
and the current producer/helper code checksums are recorded, but those cannot
prove which historic code produced the CSV. Deployment must use the existing
context export from the corresponding validated study run. The standardizer
CSV similarly lacks a version; numerical z and frozen-A reconciliation guard it.

## Outputs and restart

All outputs are isolated under
`results/rq2/recovery/<rq1_analysis_version>/<run_id>/`.
The run ID fingerprints input bytes, relevant code, R/package versions, seed,
fold count, penalty and denominator floor. Changing worker count does not
invalidate completed tasks. Upstream version changes cannot reuse old results.

| File | Purpose |
|---|---|
| `provenance.rds` | Input/code MD5, versions, parameters, runtime/session and provenance limitations |
| `participant_folds.csv` | Reproducible site-stratified participant assignment |
| `task_catalog.csv`, `unavailable_audit.csv` | Supports, eligibility/context coverage, frozen/all-eligible A and exclusions |
| `predictor_allowlist.csv` | Exact predictor/family audit |
| `inputs/task_*.rds` | Compact per-task input snapshots, shared by model workers through paths |
| `checkpoints/task_*.rds` | Atomic task result: metadata, all participant-day predictions/errors/folds, fold models/preprocessing, elapsed time, failure message |
| `task_status.csv` | Completed/non-estimable/failed/reused tasks and checkpoint locations |
| `heldout_errors.csv`, `fold_errors.csv` | Three-state MAE, RMSE and standardized A; fold A and test sizes |
| `recovery_comparison.csv` | Metric-level delta A, G, context increment, raw-adjusted delta A and raw-magnitude bin |
| `recovery_overview.csv` | Per-contrast/state improvement fraction, median gains, raw/recovered rank correlation and adjusted spread |
| `raw_magnitude_structure.csv` | Within-contrast raw-magnitude quartile ranges and gain distribution |
| `recovery_manifest.rds` | Run completeness, status references, errors and provenance |

Unestimable/failed tasks have three NA error rows, not zero distortion.
Gain summaries include only estimable tasks; use the status/catalog denominator
alongside them. The last three summary CSVs require at least one estimable task.
Prediction distributions are retained in checkpoints instead of duplicated
in a giant CSV. Read those for participant-level or matched-day diagnostics.

Workers catch task errors and atomically store the failure without discarding
other results. Restart the same command to reuse complete tasks and retry failed
ones. An interrupted PSOCK process may abort that invocation; already-installed
checkpoints survive. Input extraction is repeated on restart, but successful
model tasks are not refitted. Only one process should run a given run ID at once.

## Interpretation limits and leakage risks

* External weather, reported microenvironment, activity and prior sleep are
  deployable after the day is complete if those context sources are collected.
  They are not necessarily available to a real-time logger. ERA5 is retrospective;
  substituting forecasts or different sensors requires a separate validation.
* The existing context export's calendar originated from eye/MEDI/10-s support.
  Only its external/diary columns are used here. The recovery estimand remains
  conditional on observed paired support. This does not establish performance
  on days excluded by the high-information instrument's eligibility rules.
* Diary sleep/activity may share timing definitions with some exposure metrics;
  good recovery can reflect that structural information. Deployability assumes
  the same diary fields are actually collected independently of target exposure.
* Fixed ridge is a deliberately bounded prototype; poor performance is not a
  proof that a representation is fundamentally unrecoverable. Circular chordal
  training loss differs from circular absolute evaluation loss.
* Gain and raw error share terms. Raw-magnitude bins, rank changes and residuals
  from `delta_A ~ log1p(A_raw) + log1p(A_raw)^2` within each contrast/state are
  descriptive diagnostics of additional structure, not proof of statistical
  independence. Metric classes are not replicates. There are no inferential
  p-values or causal claims. Formal confirmation requires prespecified modeling
  and participant-level uncertainty beyond this prototype.

## ECS commands and resource expectations

Run at the repository root with the existing R 4.5.0 project environment:

```bash
Rscript scripts/12d_rq2_recovery.R --check-inputs
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
RQ2_RECOVERY_WORKERS=36 RQ2_RECOVERY_FOLDS=5 RQ2_RECOVERY_SEED=20260912 \
Rscript scripts/12d_rq2_recovery.R --run
```

Repeat the second command for restart. `RQ2_RECOVERY_LAMBDA=0.01` and
`RQ2_RECOVERY_G_FLOOR=0.000001` are optional explicit defaults; do not select
their values after inspecting held-out errors. Worker resolution reuses
`ms_resolve_workers`, whose physical-core cap may reduce a request of 36 to 24
on a 48-vCPU/24-physical-core host. The effective count is printed and recorded.
PSOCK workers use one BLAS/OpenMP thread. No server runner was modified.

There are at most 416 task slots (52 x 8), with the two LIGHT-only dual-channel
slots unavailable. Five folds require about 4,140 small matrix fits when all
414 remaining tasks are estimable, each with at most 39 predictor/intercept
columns. This is much cheaper than rebuilding metrics or mixed-model/bootstrap
analyses. A planning allowance is minutes to tens of minutes, several GiB to
low tens of GiB RAM for the coordinator and 24-36 R workers, and hundreds of MB
of recovery outputs; these are unbenchmarked estimates. Frozen part
decompression, PSOCK startup and I/O may dominate. The 48-vCPU/192-GiB machine
should have substantial headroom, but inspect the logged effective workers,
task sizes and elapsed times before increasing parallelism.

## Local validation record (2026-09-12)

Only parsing, small frozen-manifest/CSV reads and synthetic interface tests were
performed; no formal recovery task or upstream reconstruction was run.
`scripts/tests/validate_recovery_prototype.R` checks linear/circular behavior,
target-field isolation, missing context, identical test support, retry/reuse,
two-worker PSOCK parity, and zero-denominator summary behavior.

The local bundle contains 811 frozen context rows, 270 scale rows and 414
anchor-summary rows. It is not runnable: the manifest's non-duration
`rq1_pairwise_part_000.rds` and `.ok` are absent locally, and the stored part
directory is `/home/ecs-user/measurement-sufficiency/results/rq1/pairwise_parts/...`.
The manifest reports `rq1_v5_oriented_pairwise_config_keyed__...`, whereas the
summary reports `rq1_v5_duration_type_canonical__...`. This incompatibility is
reported rather than bridged or rewritten. The ECS preflight must pass with
its coherent frozen bundle before the user starts the formal run.
