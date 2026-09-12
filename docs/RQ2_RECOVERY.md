# Independent RQ2 recovery prototype

`scripts/12d_rq2_recovery.R` is an opt-in downstream analysis. It never sources
`09`, `10`, `10b`, `12`, or `12c`, regenerates metrics/context, or changes figures.
Entrypoints are `--build-context`, `--check-inputs`, `--smoke-test`, and `--run`.
Each validates/reuses the daypart cache, constructing it once if absent or stale.

## Estimand and model

The eight participant-day contrasts and candidate/reference orientation come
from `rq1_inference_anchor_map()` and `rq1_inference_pairs()`. The 52 daily
representations retain their metric-specific maximal supports. LIGHT-only
MDER/nvRD remain unavailable. No duration or outcome matching is performed.

For each contrast/metric, raw `Y_L` is compared with three fitted information layers:
`calibration = Y_L`, `signature = Y_L + S_L`, and `context = Y_L + S_L + C_T`.
There are four loss rows per learner, all on identical held-out participant-days.
The daily context whitelist is taken directly
from the external, microenvironment and behaviour families in
`rq2_context_features.R`. No RQ2 exposure-state predictor, other configuration's
representation, target-derived bin, participant ID, site ID, Date, distortion or
standardizer enters a design matrix. IDs/site are used solely for matching/CV.

Both learners now fit **identity-anchored residual correction**: linear training
target `delta = Y_H - Y_L`, prediction `Y_hat = Y_L + delta_hat`. XGBoost is the
primary nonlinear learner; ridge with unpenalized intercept and fixed
`lambda=0.01` is the sensitivity baseline. Zero predicted correction preserves
the actual measured `Y_L`, including when regularization shrinks slopes to zero.

For circular time, first form the shortest signed correction in [-43200,43200)
seconds. Train two correction coordinates `sin(theta)` and `cos(theta)-1`, with
`theta=2*pi*delta/86400`. Decode with `atan2(pred_sin, 1+pred_cos_minus_one)`,
take its shortest correction and add it to `Y_L` modulo 86400. Thus the model
origin (0,0) is identity; no absolute target-clock regression is used. An
undefined decoded direction (norm below 1e-10) falls back to zero correction
and is counted. Candidate clock predictors remain its own sin/cos coordinates.

Calibration, signature and context use identical XGBoost parameters, objective, inner split
and early-stopping procedure; only the predictor set differs. Context always
offers 16 signature fields, all 18 daily context fields and 12 daypart context
fields, plus their missingness indicators; no outcome-
driven context selection is performed. Defaults are depth 3, eta 0.05,
min_child_weight 5, lambda 1, alpha 0, full row/column sampling, histogram trees,
zero base_score and one XGBoost thread. The maximum is 500 rounds, with patience
30 and squared-error/RMSE training and inner validation. These settings are
fixed, not chosen using outer test performance.

Within each outer training fold, a seeded participant split holds out 20% of
training participants (at least one, leaving at least two for fitting).
Imputation/scaling for early stopping is fitted on inner-training participants
only. The minimum inner-validation RMSE selects the number of rounds for each
state/target coordinate; the same selection rule can yield different fitted
round counts. A fresh model then fits **all outer-training participants** for
exactly that number of rounds, without early stopping on the outer test fold.
No outer test target is passed to the boosting backend. The implementation uses
the official [xgb.train interface](https://xgboost.readthedocs.io/en/latest/r_docs/R-package/docs/reference/xgb.train.html),
accepting its `watchlist`/`evals` naming difference across R package versions.

Training-fold median imputation, missingness indicators, variance screening and
the existing `rq2_model_helpers()$scale_train_test` preserve held-out isolation.
Ridge removes entirely missing/constant fields inside training folds. XGBoost
preserves their schema as zero columns, retaining all prespecified candidates.
Missing context never removes a paired participant-day.
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
`signature_increment = A_calibration - A_signature` isolates information in other
low-configuration summaries. `context_increment = A_signature - A_context`
isolates extra context benefit; `context_vs_calibration` retains the wider
comparison. `loss_decomposition.csv` records
`A_raw = (A_raw-A_signature) + (A_signature-A_context) + A_context`.
The final term is **unrecovered residual under this model/input/CV design**, not
an identified irreducible error or proof that acquisition is indispensable.
Negative gains are retained; no monotone waterfall or zero clipping is imposed.

## Compact S_L and temporally structured C_T (v3)

Inspection found an existing exact-configuration hourly basis in
`results/core/unit_context.csv.gz`: `isiv_h00` through `isiv_h23`. The producer
`core_config_daily_context()` calls `core_make_series()` for that configuration
before forming hourly means of `log10(light + 0.1)`. For LIGHT configurations,
the internally named MEDI series is the observed LIGHT channel. Thus no faster
cadence, other placement, MEDI reconstruction or target-side exposure is needed.
Only this 24-column frozen basis is summarized; no raw sequence or vector of
the 52 scientific metrics is used. Candidate config AND support/site/Id/Date
must match exactly; existing support and config metadata checks remain strict.

The 16 shared signature features are:

| Family | Feature definition on finite stored hourly log-light values h |
|---|---|
| Level/distribution | mean, median, q10, q90, IQR (R type-7 quantiles), sample SD, min, max: eight fields |
| Daypart exposure | mean h in morning [06,11), midday [11,14), afternoon [14,18), evening [18,24): four fields |
| Bright/dark proxy | fraction of finite hours with h >= log10(250.1), and h <= log10(10.1): two fields |
| Simple dynamics | mean absolute adjacent-hour change; Pearson lag-1 correlation: two fields |

The bright/dark fractions refer to **hourly mean-log levels**, not sample-level
time above/below threshold or arithmetic hourly illuminance. The numerical
250/10 cutpoints are compact descriptive bins, not universal biological or
measurement-sufficiency thresholds. MEDI and LIGHT remain different operational
channels. Dynamics require adjacent finite clock hours; gaps are not bridged,
and 23:00 is not joined to 00:00. Correlation requires at least three finite
pairs with nonzero variation. Empty dayparts and undefined statistics remain
NA and use training-only imputation. `signature_valid_hours` is audit metadata,
not a predictor. Equal-hour weighting follows the already-frozen basis,
including its local-time/DST treatment; subhour distribution/dynamics cannot
be recovered from these summaries.

C_T retains all 18 daily external/microenvironment/behaviour fields and adds
only **three variables x four matching local dayparts**: log1p(mean radiation
W/m2), reported outdoor fraction, and reported work fraction. Existing daily
RQ2 CSVs and model shards lack these dayparts and cannot be disaggregated.
The sole additional frozen context artifact is
`results/rq2/recovery_inputs/context_dayparts.rds`. It must be supplied from the
existing validated core weather and harmonized diary context, without reading
light metrics, using low/high exposure to define segments, or filling a missing
daypart with a daily value. The recovery script now builds this artifact itself
from `results/core/weather_1min.csv.gz` and existing
`data/raw/melidos/<site>__lightexposurediary.RData`. It invokes no upstream job,
weather ingestion/interpolation or light-metric computation. The existing 18
daily context fields are unchanged.

`12d` reuses a valid cache only when core/RQ1 versions, every source MD5
(weather, core unit calendar, diaries), feature definition and builder/classifier
hash match. A changed fingerprint triggers reconstruction. A matching fingerprint
with invalid schema or content hash is reported as corruption, not silently
accepted. Source hashes are checked again before installation to reject inputs
that changed during the build. The complete RDS is closed in a temporary file
and installed by rename; no half-written destination is exposed. Metadata,
source paths/hashes, local daypart definition, generated timestamp, data hash,
invalid-interval and overlap/conflict audit are stored inside this same RDS.
No additional context manifest/CSV is produced. A repeat cache hit does not
rewrite the file or change its timestamp.

The RDS contract is a list:

```r
list(
  artifact_type = "recovery_deployable_dayparts_v1",
  daypart_contract = "local_06_11_14_18_24_v1",
  core_artifact_version = "<matching current core version>",
  rq1_analysis_version = "<matching pairwise/summary version>",
  sources = data.frame(role = ..., path = ..., md5 = ...),
  provenance = list(...), generated_at = ..., daypart_definition = ...,
  data_md5 = ..., audit = ...,
  data = data.frame(site = ..., Id = ..., Date = ..., timezone = ...,
    daypart = ..., radiation_mean_w_m2 = ..., outdoor_fraction = ...,
    work_fraction = ..., weather_hours = ..., environment_hours = ...,
    activity_hours = ...)
)
```

`sources` must identify `core_weather` and `harmonized_diary` with source paths
and MD5 records, verified against the current files before reuse. The core
calendar's MD5 and builder dependency hashes are in `provenance`. Dates are local
dates; timezone is an Olson name, identical
within day. There must be one row for every site/Id/Date/daypart, including
explicit NA/zero-coverage rows when unobserved. Fractions are in [0,1], radiation
is nonnegative, and each coverage denominator is measured in elapsed hours.
Radiation uses the frozen minute values on `[time_utc,time_utc+60s)`, weighted
by overlap seconds, without bridging missing minutes or interpolating. Outdoor uses the
existing harmonized `environment == outdoor` classification and work uses
`act_working_indoor | act_working_outdoor`, each weighted by interval overlap
with that local daypart on its reported-domain support. The builder retains
original half-open start/end boundaries; it does not assign an entire interval
to its start-time bin. It partitions overlapping diary intervals into segments:
identical reports count once, and conflicting reported flags exclude only that
segment/domain from its denominator. Unclassified reports remain missing.
Invalid intervals, overlap hours and per-domain conflict hours are audited in
the RDS. DST boundaries use local civil time
and elapsed durations, not a forced 24-hour UTC day. No observed denominator
means an NA feature, never a false zero. Night [00,06) is represented in daily
context and full-day signature summaries, not an extra daypart column.

Missing eligible-day signature/context rows stop the run rather than silently
shrinking the paired support or reverting to an earlier information layer.

## Required existing frozen inputs

| Input | Contract |
|---|---|
| `results/rq1/rq1_pairwise_change_long.rds` | Versioned partitioned manifest, current core/design identity, `part_manifest` with `part`/`dimension` |
| Manifest-declared `placement_optical_temporal` parts and their `.ok` markers | Fig.2 pair fields plus config IDs, lattice, scale anchor, availability, delta/z and versions; unique participant-day metric keys |
| `results/rq1/rq1_pairwise_summary.csv` | Same RQ1/core versions as manifest; unique anchor/metric summary and `A_mean_absolute` |
| `results/diagnostics/rq1_standardizer_audit.csv` | Unique lattice/metric/geometry scale, `scale_anchor_config`, `standardizer`; must numerically reproduce frozen z |
| `results/diagnostics/rq2_layered_context_day_features.csv` | Unique site/Id/Date and all 18 existing family fields; numeric or entirely missing |
| `results/core/unit_context.csv.gz` | Existing current-version configuration metadata and isiv_h00..23 basis for S_L |
| `results/core/weather_1min.csv.gz` | Current-version normalized minute radiation; required to build/validate the cache |
| `data/raw/melidos/<site>__lightexposurediary.RData` | Existing harmonized interval export for every frozen-calendar site |
| `results/rq2/recovery_inputs/context_dayparts.rds` | Automatically built/reused compact C_T artifact; not a manually prepared input |

Only non-duration parts are read. Original manifest part_dir takes precedence
if present; otherwise recovery resolves the current repository's same-version
pairwise_parts directory in memory and requires all declared parts and markers.
No core metric cube, duration file, raw light series, raw ERA5 payload,
Fig.2 inference result or old RQ2 model checkpoint is needed. Missing source files,
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
fold count, both learners' parameters and denominator floor. Version
`rq2_recovery_v3_low_signature_temporal_context` prevents reuse of earlier
information-layer checkpoints. Changing worker count does not
invalidate completed tasks. Upstream version changes cannot reuse old results.

| File | Purpose |
|---|---|
| `provenance.rds` | Input/code MD5, versions, parameters, runtime/session and provenance limitations |
| `participant_folds.csv` | Reproducible site-stratified participant assignment |
| `task_catalog.csv`, `unavailable_audit.csv` | Supports, eligibility/context coverage, frozen/all-eligible A and exclusions |
| `predictor_allowlist.csv` | Exact predictor/family audit |
| `inputs/task_*.rds` | Compact per-task input snapshots, shared by model workers through paths |
| `checkpoints/task_*.rds` | One metric/contrast/learner result: predictions/corrections/errors, fold preprocessing/models, inner participant maps, selected rounds/evaluation logs, serialized XGBoost boosters, failures |
| `task_status.csv` | Completed/non-estimable/failed/reused tasks and checkpoint locations |
| `heldout_errors.csv`, `fold_errors.csv` | Three-state MAE, RMSE and standardized A; fold A and test sizes |
| `recovery_comparison.csv` | Metric-level delta A, G, context increment, raw-adjusted delta A and raw-magnitude bin |
| `recovery_overview.csv` | Per-contrast/state improvement fraction, median gains, raw/recovered rank correlation and adjusted spread |
| `raw_magnitude_structure.csv` | Within-contrast raw-magnitude quartile ranges and gain distribution |
| `recovery_manifest.rds` | Run completeness, status references, errors and provenance |
| `low_signature_features.csv`, `temporal_context_features.csv` | Exact compact features used in the new layers |
| `information_support_audit.csv` | Config/support/day join presence and finite hourly counts |
| `loss_decomposition.csv` | Calibration/signature increments, total self/context recovery and unrecovered residual |

Unestimable/failed tasks have four NA error rows, not zero distortion.
Existing filenames are unchanged. Tables add `learner` (`xgboost` primary,
`ridge` sensitivity); each learner has its own three fitted layers and raw
baseline. `base_task_index` links shared scientific tasks/input shards, while
`task_index` identifies independently restartable learner tasks. All summaries
are stratified by learner, preventing accidental pooling or cross-learner
calibration/context comparisons. All learners use the same outer fold map.
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
* XGBoost with fixed architecture and ridge sensitivity remain bounded prototypes;
  poor performance is not a proof that a representation is fundamentally
  unrecoverable. Circular chordal
  training loss differs from circular absolute evaluation loss.
* Gain and raw error share terms. Raw-magnitude bins, rank changes and residuals
  from `delta_A ~ log1p(A_raw) + log1p(A_raw)^2` within each contrast/state are
  descriptive diagnostics of additional structure, not proof of statistical
  independence. Metric classes are not replicates. There are no inferential
  p-values or causal claims. Formal confirmation requires prespecified modeling
  and participant-level uncertainty beyond this prototype.

## ECS commands and resource expectations

Run at the repository root with the existing R 4.5.0 project environment and
the R `xgboost` package available. Preflight reports a missing package; the
script never installs packages or falls back silently to ridge-only analysis:

```bash
Rscript scripts/12d_rq2_recovery.R --build-context
Rscript scripts/12d_rq2_recovery.R --check-inputs
RQ2_RECOVERY_SMOKE_WORKERS=2 Rscript scripts/12d_rq2_recovery.R --smoke-test
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
RQ2_RECOVERY_WORKERS=36 RQ2_RECOVERY_FOLDS=5 RQ2_RECOVERY_SEED=20260912 \
Rscript scripts/12d_rq2_recovery.R --run
```

Repeat the `--run` command for restart. `RQ2_RECOVERY_LAMBDA=0.01` and
`RQ2_RECOVERY_G_FLOOR=0.000001` are optional explicit defaults; do not select
their values after inspecting held-out errors. Worker resolution reuses
`ms_resolve_workers`, whose physical-core cap may reduce a request of 36 to 24
on a 48-vCPU/24-physical-core host. The effective count is printed and recorded.
PSOCK workers use one BLAS/OpenMP thread. No server runner was modified.
`--smoke-test` validates all eight anchors, then fits one well-supported linear
and circular metric/contrast through both learners, all outer folds and all
three fitted layers with production model settings. It verifies held-out row
counts, predictor allowlists, inner/outer participant isolation and checkpoint
reuse. Its task inputs/checkpoints are temporary and cleaned up; only the
daypart context RDS persists. `RQ2_RECOVERY_CORE_ROOT` optionally points to an
existing core directory; all version/schema checks remain mandatory.

There are at most 832 independently checkpointed learner tasks (52 x 8 x 2),
with the two LIGHT-only dual-channel slots unavailable for each learner. Five
folds require 6,210 ridge matrix fits and at least 12,420 XGBoost training calls
(inner tuning plus outer refit), with twice as many boosting calls for circular
tasks' two correction coordinates. Training uses at most 95 columns including
the ridge intercept. Prior v1 time/output-size estimates do not apply: boosting
and stored fold boosters can materially increase runtime and disk usage. There
is no local formal benchmark; use elapsed-time/task-size audit on ECS. Continue
to request 36 workers (subject to the existing physical-core cap) with one
XGBoost/BLAS thread per worker, and inspect RAM before raising concurrency.

## Local validation record (2026-09-12)

Synthetic tests and a real daypart build were performed from the supplied
current-version frozen inputs; no upstream reconstruction was run.
`scripts/tests/validate_recovery_prototype.R` checks linear/circular behavior,
target-field isolation, missing context, identical test support, retry/reuse,
two-worker PSOCK parity, and zero-denominator summary behavior.
Residual-version checks additionally cover exact identity under zero correction,
constant circular correction, rejection of forbidden predictors, all 18 XGBoost
context fields, shared inner participant splits, inner-only preprocessing and
held-out target isolation using an instrumented boosting boundary. The real
XGBoost numerical smoke test is conditional on the installed R package. It was
skipped during v2 development when xgboost was absent. During v3 development,
the now-available XGBoost package passed the small synthetic numerical smoke
test, alongside exact low-config signature, daypart and decomposition checks.
No package installation or full recovery analysis was performed by this change.

The local bundle contains 811 frozen context rows, 270 scale rows and 414
anchor-summary rows. The current-version non-duration pairwise part and its
completion marker are available through the same-version repository fallback;
the Linux path in the frozen manifest remains unchanged.

The real `--build-context` produced 3,244 rows (811 participant-days, nine sites,
four dayparts), 31,084 bytes. A second invocation returned `reused=TRUE` with
identical file MD5 and modification time; `--check-inputs` passed. Radiation is
available in all dayparts; outdoor/work fractions are missing in 465/473 rows
respectively and retain explicit missingness for training-only preprocessing.
The RDS audit records 27 invalid diary intervals (RISE: 1; THUAS: 26), excluded
from duration integration. Missing context never changes frozen matched support.
Synthetic cache tests cover overlap weighting, cross-midnight intervals,
duplicate/conflict handling, DST, unchanged reuse, source/contract invalidation
and corruption rejection. No input artifact was rewritten.

The real `--smoke-test` passed in 43.4 seconds with two PSOCK workers: MDER and
brightest-10h midpoint, both on the 30-s versus 10-s contrast, each with XGBoost
and ridge. Each task retained 811 held-out participant-days in all four states
and fitted 15 models (five outer folds times three predictor layers). Predictor
allowlists, inner/outer participant separation, unchanged raw frozen A and
checkpoint reuse passed. Temporary smoke files were removed automatically;
the context RDS is the only persistent artifact generated by these checks.
The existing Windows locale, package-build-version and renv synchronization
warnings did not prevent tests from passing.
