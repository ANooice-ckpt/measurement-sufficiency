# measurement-sufficiency

Analysis repository for **How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**.

The scientific object is the empirical mapping from measurement configuration to target exposure representation and its distortion relative to a high-information benchmark. RQ1 characterizes distortion distributions, RQ2 studies their conditionality and cross-dimensional separability, and RQ3 converts those empirical relationships into target-specific sufficient configuration sets and Pareto frontiers.

## Environment
Run from the repository root under **R 4.5.0**. LightLogR 0.10.3 and melidosData 1.0.6 are scientific pins.

```bash
Rscript scripts/00_setup.R
```

MeLiDos source files are stored under `data/raw/melidos/`; `scripts/01_download_melidos.R` now downloads the harmonized light/wear/sleep inputs **and the site-level `trial_times` metadata** used to anchor monitoring-duration references. ERA5 files remain under `data/raw/era5/`.

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
After the new core completes, regenerate every downstream artifact because temporal and duration estimands changed:

```bash
RQ1_BOOT=1000 Rscript scripts/10_rq1_analysis.R
Rscript scripts/11_plot_fig1.R

RQ2_WORKERS=12 RQ2_CV_FOLDS=5 RQ2_BOOT=1000 Rscript scripts/12_rq2_analysis.R
Rscript scripts/13_plot_rq2.R

Rscript scripts/14_rq3_analysis.R
Rscript scripts/15_plot_rq3.R
```

RQ2 checkpoints include the upstream RQ1 analysis version, so stale checkpoints from an older core are rejected automatically.

See `docs/STUDY_SPEC.md`, `docs/CORE_ARTIFACTS.md`, and `docs/RQ1_EXECUTION.md` for the frozen definitions.
