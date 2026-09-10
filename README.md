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
CORE_WORKERS=48 CORE_DURATION_WORKERS=32 CORE_FORCE=0 bash scripts/run_core_artifacts.sh
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
# RQ1: general pairwise representation-change map -> Fig. 1
RQ1_BOOT=1000 RQ1_STARTUP_WORKERS=36 RQ1_PART_WORKERS=44 RQ1_FRAGMENT_WORKERS=36 RQ1_BOOT_WORKERS=40 RQ1_PART_COMPRESSION=gzip Rscript scripts/10_rq1_analysis.R
Rscript scripts/11_plot_fig1.R

# RQ1 downstream inferential preservation -> Fig. 2
RQ1_INFERENCE_BOOT=1000 Rscript scripts/10b_rq1_inferential_preservation.R
Rscript scripts/11b_plot_fig2.R

# RQ2: exposure-state conditionality, layered context models and circular-aware gamma -> Figs. 3-4
RQ2_WORKERS=12 RQ2_CV_FOLDS=5 RQ2_RUN_MODELS=1 Rscript scripts/12_rq2_analysis.R
Rscript scripts/13a_plot_fig3.R
Rscript scripts/13b_plot_fig4.R

# RQ3: observed residual instability, sufficiency and Pareto occupancy -> Figs. 5-6
Rscript scripts/14_rq3_analysis.R
Rscript scripts/15a_plot_fig5.R
Rscript scripts/15b_plot_fig6.R

# Supplementary figures
Rscript scripts/16_plot_supplementary.R
```

RQ1-RQ3 remain the canonical scientific analysis sources. The inferential-preservation extension is an RQ1 downstream-consequence layer rather than a new research question. It uses 52 participant-day light representations and six day-level outcomes spanning **Sleep** (sleep quality, awakenings, awake duration), **Alertness** (daily KSS) and **Affect** (daily positive and negative MoodZoom composites), with exactly eight single-axis contrasts against eye/MEDI/10 s: chest, wrist, LIGHT, and 20/30/40/60/120 s. Sleep outcomes pair exposure day D with the following-morning diary; repeated current-conditions EMA responses are harmonized to nominal 11/14/17/20 h slots and summarized to same-day alertness/affect phenotypes. The analysis is descriptive association preservation rather than a temporally resolved causal health-effect model.

The active Fig. 2 artifact is `results/rq1/inference/rq1_inferential_preservation_domains_anchor8.rds`. Candidate/reference exposure values and their pair-specific support are read directly from the frozen RQ1 pairwise artifact; the main propagation analysis links the already-frozen RQ1 distortion to downstream association displacement. Reference and candidate associations use identical participant-day support, participant fixed effects and paired site-stratified participant bootstrap resampling. Eye/MEDI/10 s remains an empirical high-information anchor, not biological truth.

Main-figure identity is centralized in `scripts/utils/figure_registry.R`. The public Fig. 1–6 IDs and canonical numbered entrypoints live there once; the mature RQ2/RQ3 implementation filenames may retain their pre-insertion numbers internally. `scripts/utils/plot_contracts.R` converts only implementation-emitted legacy identities to current output identities and never renumbers an already-written current manifest. Existing historical `fig2_*`/`fig3_*`/`fig4_*`/`fig5_*` audit CSV filenames are retained for output compatibility.

RQ1 keeps concrete nested duration-window comparisons in its canonical pairwise artifact but projects them to generic 1–6 day comparison types before pooled summaries and bootstrap inference. The RQ2 entrypoint contains the streamed conditional analysis directly and, in the same R process, adds the layered contextual models. Those models reuse existing ERA5 fields from `unit_context` and harmonized MeLiDos light-exposure, exercise and sleep diaries; they do not introduce an alternate core/weather preprocessing path.

Linux execution has three maintained entrypoints: `scripts/run_core_artifacts.sh` for the expensive source-to-core build, `scripts/run_downstream_server.sh` for resumable RQ1-output-to-final-results execution, and `scripts/run_full_server.sh` for the complete pipeline. Production reruns are expected to use the validated 48-vCPU / 192-GiB ECS class. The repository defaults therefore target that machine directly; worker environment variables remain available only when hardware changes or for explicit diagnostic runs.

The analysis stages emit versioned model/checkpoint and figure provenance tables alongside their primary outputs. Main PNG figures are centralized under `results/figures`; RQ-specific figure manifests remain at the corresponding `results/rqX/` roots.

RQ1's primary upstream artifact is `results/rq1/rq1_pairwise_change_long.rds`, a manifest for versioned canonical parts. RQ2 uses the manifest loader and selects only primary pairwise rows/columns. RQ3 reads the RQ1 pairwise summaries and actual `results/core/duration_metric_cube.rds`. Plot scripts read frozen RQ outputs only; they do not load raw MeLiDos series, call LightLogR metric operators, refit models, bootstrap, construct duration windows or calculate gamma/sufficiency.

Context analyses previously split across historical `10b`, `10c` and `12b` entry points are no longer separate context-analysis stages. The current `10b_rq1_inferential_preservation.R` is a different, dedicated downstream-inference extension. Reusable context operators belong in `scripts/utils/` or in the RQ2 checkpointed stages; `12c_rq2_context_models.R` is sourced internally by the canonical RQ2 entrypoint rather than run as a standalone stage.

## Results hand-off

After a server run, copy the complete `results/` directory back. It contains core durable artifacts, analysis outputs, checkpoints, diagnostics, tables and figures. `external/` remains upstream reproduction material and is not generated output.

See `docs/STUDY_SPEC.md`, `docs/CORE_ARTIFACTS.md`, `docs/RQ1_EXECUTION.md`, and `docs/FIGURE_ARCHITECTURE.md`.
