# measurement-sufficiency

Analysis repository for **How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**.

The scientific object is:

```text
configuration state -> observed exposure process -> target representation
                    -> pairwise representation change
                    -> conditional / cross-dimensional structure
                    -> observed stability and measurement sufficiency
```

The published 54 Zauner/LightLogR target metrics and their definitions remain unchanged. Metric classes are descriptive groupings, not inferential replicates. Unavailable representations stay unavailable.

## Environment

Run from the repository root under R 4.5.0 with LightLogR 0.10.3 and melidosData 1.0.6.

```bash
Rscript scripts/00_setup.R
```

`data/` is reserved for raw MeLiDos and ERA5 inputs. Generated artifacts,
caches, diagnostics, checkpoints, tables and figures are written below
`results/`; execution manifests and session logs are written below `logs/`.

For a fresh Linux server, clone the project, place the raw inputs under `data/raw/`,
and run:

```bash
sudo bash scripts/bootstrap_ecs_ubuntu24.sh
CORE_WORKERS=48 bash scripts/run_all_server.sh
```

The full runner moves to the repository root, fetches and verifies the pinned
Zauner reference checkout, restores the R environment, and builds all core,
analysis, and figure artifacts. The first run needs outbound CRAN and GitHub
access for R packages and the pinned upstream checkout.

## Core

The current core artifact version is `v3_sparse_sampling_complete_days`.

Primary temporal states are:

```text
10 s, 20 s, 30 s, 60 s, 300 s, 900 s, 1800 s
```

Reserve extraction states are 120 s, 600 s and 3600 s. 15 s is prohibited. Every coarse state is a participant/source-grid-phase-anchored systematic sparse subsample of the harmonized 10-s grid; retained timestamps and values are exact source values.

Monitoring duration is the accumulation dimension. Core preprocessing identifies consecutive complete analysis-day runs and materializes every contiguous 1–6 day window within each run. Protocol `trial_times` metadata remains in `unit_context` for audit and sensitivity, but no seven-day protocol reference defines the primary duration domain.

Build the expensive core layer with:

```bash
CORE_WORKERS=48 CORE_FORCE=0 bash scripts/run_core_artifacts.sh
```

Durable core outputs:

```text
results/core/
  cache/<core_version>/{supports,metrics,context,weather}/
  metric_cube.csv.gz
  unit_context.csv.gz
  weather_1min.csv.gz
  duration_window_manifest.rds
  duration_metric_cube.rds
  duration_metric_cube_parts.csv
  core_manifest.csv
```

`duration_metric_cube` is the shared representation artifact for RQ1, RQ2 and RQ3. Daily targets are aggregated from the participant-day metric cube. IS/IV are rebuilt on exact selected window dates from the stored hourly basis.

## Downstream execution

```bash
# RQ1: general pairwise representation-change map
RQ1_BOOT=1000 RQ1_PART_WORKERS=24 RQ1_BOOT_WORKERS=8 RQ1_PART_COMPRESSION=gzip Rscript scripts/10_rq1_analysis.R
Rscript scripts/11_plot_fig1.R

# RQ2: local conditionality, CV models and circular-aware gamma
RQ2_WORKERS=12 RQ2_CV_FOLDS=5 RQ2_RUN_MODELS=1 Rscript scripts/12_rq2_analysis.R
Rscript scripts/13_plot_rq2.R

# RQ3: observed residual instability, sufficiency and Pareto occupancy
Rscript scripts/14_rq3_analysis.R
Rscript scripts/15_plot_rq3.R
```

On the Linux server, the resumable downstream chain can be launched with
`scripts/run_downstream_server.sh`. Its defaults are 16 RQ1 part workers,
8 bootstrap workers and 12 RQ2 model workers; override these environment
variables when storage bandwidth or memory requires a lower setting.

For a clean server that should produce the complete `results/` tree in one
ordered run, use `bash scripts/run_all_server.sh`. Set `CORE_WORKERS=48` on
the 96-vCPU ECS, and lower the RQ1/RQ2 worker variables if memory or storage
bandwidth is limiting.

The analysis stages emit versioned model/checkpoint and figure provenance tables alongside
their primary outputs: `results/rq2/rq2_model_artifact_manifest.csv` and
`results/rqX/figures/figure_artifact_manifest.csv`. These are presentation metadata, not a
second schema or hash contract.

RQ1's primary upstream artifact is `results/rq1/rq1_pairwise_change_long.rds`, a manifest for versioned canonical parts. RQ2 uses the manifest loader and selects only primary pairwise rows/columns. RQ3 reads the RQ1 pairwise summaries and actual `results/core/duration_metric_cube.rds`. Plot scripts read frozen RQ outputs only; they do not load raw MeLiDos series, call LightLogR metric operators, refit models, bootstrap, construct duration windows or calculate gamma/sufficiency.

Context analyses previously split across `10b`, `10c` and `12b` are no longer separate RQ1 entry points. Their reusable context operators belong in `scripts/utils/` or in the RQ2 checkpointed stages.

## Results hand-off

After a server run, copy the complete `results/` and `logs/` directories back.
`results/` contains core durable artifacts, analysis outputs, checkpoints,
diagnostics, tables and figures. `external/` remains upstream reproduction
material and is fetched by the core runner when absent.

See `docs/STUDY_SPEC.md`, `docs/CORE_ARTIFACTS.md`, `docs/RQ1_EXECUTION.md`,
and `docs/FIGURE_ARCHITECTURE.md`.
