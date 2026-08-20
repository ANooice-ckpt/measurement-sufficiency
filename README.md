# measurement-sufficiency

Analysis repository for **How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**.

The scientific object is the empirical mapping from measurement configuration to target exposure representation and its distortion relative to a high-information benchmark. RQ1 characterizes distortion distributions, RQ2 studies their conditionality and cross-dimensional separability, and RQ3 converts those empirical relationships into target-specific sufficient configuration sets and Pareto frontiers.

## Environment
Run from the repository root under **R 4.5.0**. LightLogR 0.10.3 and melidosData 1.0.6 are scientific pins.

```bash
Rscript scripts/00_setup.R
```

MeLiDos source files are stored under `data/raw/melidos/`; `scripts/01_download_melidos.R` downloads the harmonized light/wear/sleep inputs, site-level `trial_times` metadata used to anchor monitoring-duration references, and the light-exposure diaries used by the real-world context extension. ERA5 files remain under `data/raw/era5/`.

## Core v2: sparse temporal sampling + protocol duration metadata
The current core artifact version is `v2_sparse_sampling_protocol7`.

Primary temporal configurations are:

```text
10 s, 20 s, 30 s, 1 min, 5 min, 15 min, 30 min
```

Reserve extraction levels are `2 min, 10 min, 60 min`. **15 s is not constructed.** Every cadence coarser than 10 s is a deterministic clock-anchored systematic subset of the harmonized 10-s grid. Retained MEDI/LIGHT observations are copied exactly; there is no wider-bin averaging and no interpolation.

Monitoring-duration references are anchored by MeLiDos `trial_times`. Within each required support, protocol calendar Days 1–7 (the first seven calendar dates from trial start) form the fixed seven-day reference when all seven are valid on the required support. A later eighth valid calendar date in a Monday-to-Monday recording is treated as a return-day date rather than an arbitrary alternative seventh day. Candidate 1–6 d windows are all contiguous windows inside that fixed seven-day reference.

For a fresh Linux/ECS build use the single entry point:

```bash
CORE_WORKERS=48 CORE_FORCE=0 bash scripts/run_core_artifacts.sh
```

`CORE_FORCE=0` is safe because expensive interim blocks live under a **versioned** cache directory. Pre-v2 mean-binned caches cannot be reused by the v2 build.

Core outputs:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
data/derived/core/core_manifest.csv
```

## Downstream execution
After the current core completes, regenerate every downstream artifact because temporal, duration, and real-world-context estimands feed the manuscript figures.

```bash
# RQ1: whole-day distortion object and Fig. 1
RQ1_BOOT=1000 Rscript scripts/10_rq1_analysis.R
Rscript scripts/11_plot_fig1.R

# RQ1 context extension: civil day/night, diary indoor/outdoor, and activity
# (home / working / vehicle / outdoors), restricted to operator-valid metrics.
RQ1_CONTEXT_WORKERS=12 RQ1_BOOT=1000 Rscript scripts/10b_rq1_context_analysis.R

# RQ2: reference-exposure-state + external-context models and gamma interactions
RQ2_WORKERS=12 RQ2_CV_FOLDS=5 RQ2_BOOT=1000 Rscript scripts/12_rq2_analysis.R

# RQ2 context extension: state-specific context geometry and paired binary contrasts
RQ2_BOOT=1000 Rscript scripts/12b_rq2_context_analysis.R
Rscript scripts/13_plot_rq2.R

# RQ3: inverse sufficiency and multidimensional Pareto frontiers
Rscript scripts/14_rq3_analysis.R
Rscript scripts/15_plot_rq3.R
```

RQ2 checkpoints include the upstream RQ1 analysis version, so stale checkpoints from an older core are rejected automatically. The context extensions are post-core analyses: they read frozen support artifacts and do not redefine the whole-day RQ1 estimand.

## Manuscript figure architecture
The plotting layer uses a common 54-representation row order and deliberately separates orthogonal information channels:

- **position/facets** identify target representation, measurement dimension/configuration, context, or tolerance;
- **bubble area or x-position** carries distortion/interdependence magnitude (`A` or `Q`);
- **diverging fill** carries directionality (`B/A` or `R/Q`), not a second magnitude scale;
- **grey/white/x states** distinguish operator-valid support, structurally invalid context restriction, and valid-but-unestimated cells;
- metric classes organize the atlas visually but are never substituted for the metric-level inferential object.

Main outputs:

```text
results/figures/Fig1_RQ1.pdf
results/figures/Fig2_RQ2.pdf
results/figures/Fig3_RQ2.pdf
results/figures/Fig4_RQ3.pdf
results/figures/Fig5_RQ3.pdf
```

High-dimensional supporting views are exported separately, including the full real-world-context hypercube split by measurement dimension, the RQ1 retention atlas, and the RQ3 class-level coverage projection. See `docs/FIGURE_ARCHITECTURE.md` for the mapping between scientific estimands and visual channels.

See `docs/STUDY_SPEC.md`, `docs/CORE_ARTIFACTS.md`, and `docs/RQ1_EXECUTION.md` for the frozen definitions.
