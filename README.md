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

`data/` is reserved for raw MeLiDos and ERA5 inputs. All generated artifacts, caches, diagnostics, checkpoints, tables and figures are written below `results/`.

## Core

The current core is **v4 sparse sampling / complete analysis days**, with the frozen measurement-design identifier embedded in the artifact version and cache path.

Primary temporal states are:

```text
10 s, 20 s, 30 s, 40 s, 60 s, 120 s
```

The only active reserve state is **300 s (5 min)**, retained as an intentionally coarse sensitivity condition. Cadences above 5 min are no longer materialized. Fifteen seconds is excluded because it is not an equal-spacing subset of the harmonized 10-s source grid. Every coarse state is a participant/source-grid-phase-anchored systematic sparse subsample; retained timestamps and values are exact source values, with no averaging or interpolation.

The active temporal and duration domains are defined once in `scripts/utils/analysis_design.R`. RQ1-RQ3 artifact versions inherit the same design identifier so stale downstream caches cannot be silently reused after a lattice change.

Monitoring duration is the accumulation dimension. Core preprocessing identifies consecutive complete analysis-day runs and materializes every contiguous 1–6 day window within each run. Protocol `trial_times` metadata remains in `unit_context` for audit and sensitivity, but no seven-day protocol reference defines the primary duration domain.

Build the expensive core layer with:

```bash
CORE_WORKERS=48 CORE_FORCE=0 bash scripts/run_core_artifacts.sh
```

Durable core outputs:

```text
results/core/
  cache/<core_version>/{supports,metrics,context,weather,duration_parts}/
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

# RQ2: exposure-state conditionality, layered context models and circular-aware gamma
RQ2_WORKERS=12 RQ2_CV_FOLDS=5 RQ2_RUN_MODELS=1 Rscript scripts/12_rq2_analysis.R
Rscript scripts/13_plot_rq2.R

# RQ3: observed residual instability, sufficiency, joint design geometry and Pareto audit
Rscript scripts/14_rq3_analysis.R
Rscript scripts/15_plot_rq3.R
```

The canonical RQ2 entrypoint contains the corrected streamed analysis directly and then, in the same R process, adds the layered contextual models. Those models reuse existing ERA5 fields from `unit_context` and harmonized MeLiDos light-exposure, exercise and sleep diaries; they do not introduce an alternate core/weather preprocessing path.

On the Linux server, the resumable downstream chain can be launched with `scripts/run_downstream_server.sh`. The server runners define their own high-core-count defaults and every worker count remains environment-overridable; inspect the selected runner before changing concurrency for a different instance size. Downstream RQ2/RQ3 sources are canonical and are no longer generated through a v5 runtime-patch layer.

The analysis stages emit versioned model/checkpoint and figure provenance tables alongside their primary outputs. Main PNG figures are centralized under `results/figures`; RQ-specific figure manifests remain at the corresponding `results/rqX/` roots.

RQ1's primary upstream artifact is `results/rq1/rq1_pairwise_change_long.rds`, a manifest for versioned canonical parts. RQ2 uses the manifest loader and selects only primary pairwise rows/columns. RQ3 reads the RQ1 pairwise summaries and actual `results/core/duration_metric_cube.rds`. Plot scripts read frozen RQ outputs only; they do not load raw MeLiDos series, call LightLogR metric operators, refit models, bootstrap, construct duration windows, or recompute canonical gamma/sufficiency/Pareto estimands. Display-only summaries may be derived from frozen thresholds and never feed back into the analyses. Main Fig. 5 uses joint entry tolerance, class contrasts and fixed-tolerance confirmed-coverage slices; canonical Pareto occupancy remains an audit/supplementary RQ3 output.

Context analyses previously split across `10b`, `10c` and `12b` are no longer separate RQ1 entry points. Their reusable context operators belong in `scripts/utils/` or in the RQ2 checkpointed stages; `12c_rq2_context_models.R` is sourced internally by the canonical RQ2 entrypoint rather than run as a standalone stage.

## Results hand-off

After a server run, copy the complete `results/` directory back. It contains core durable artifacts, analysis outputs, checkpoints, diagnostics, tables and figures. `external/` remains upstream reproduction material and is not generated output.

See `docs/STUDY_SPEC.md`, `docs/CORE_ARTIFACTS.md`, `docs/RQ1_EXECUTION.md`, and `docs/FIGURE_ARCHITECTURE.md`.
