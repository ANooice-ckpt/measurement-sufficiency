# measurement-sufficiency

Analysis repository for:

**How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**

## Planned workflow
1. Validate and inventory MeLiDos source data.
2. Reproduce upstream preprocessing and the published 54-metric pipeline.
3. Build the reusable configuration-level metric/context/weather artifacts.
4. Derive RQ1 representation distortion from the core artifacts.
5. Explain distortion heterogeneity and cross-dimensional dependence for RQ2.
6. Derive sufficiency functions and multidimensional Pareto frontiers for RQ3.

See `AGENTS.md` and `docs/`.

## Reproduction foundation
Run from the repository root under **R 4.5.0**. MeLiDos source files live under `data/raw/melidos/`. ERA5 site payloads live under `data/raw/era5/` and must include the nine site files named `BAUA.csv`, `FUSPCEU.csv`, `IZTECH.csv`, `KNUST.csv`, `MPI.csv`, `RISE.csv`, `THUAS.csv`, `TUM.csv`, and `UCR.csv`. The ERA5 reader accepts either plain CSV or ZIP-with-CSV payloads saved with a `.csv` extension.

## Core artifacts
The current production build on the 96-vCPU / 192-GiB Linux ECS uses 48 forked workers and checkpoint reuse:

```bash
CORE_WORKERS=48 CORE_FORCE=0 Rscript scripts/09_build_core_artifacts.R
```

For persistent console logging:

```bash
CORE_WORKERS=48 CORE_FORCE=0 \
Rscript scripts/09_build_core_artifacts.R 2>&1 | tee -a logs/core_build_console.log
```

`CORE_FORCE=0` is intentional: completed weather/support/metric/context blocks under `data/interim/core/` are reused after interruption. Do not delete checkpoints or force a rebuild merely because an SSH session was lost.

The build writes:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
```

The expensive high-resolution computation ends there. Later distortion definitions, duration-window construction, bootstrap choices, RQ2 models, RQ3 sufficiency calculations, and Pareto labels must read these artifacts rather than recomputing the 54 metrics from the 10-s source.

Primary temporal-resolution levels in the cube are 10 s, 15 s, 20 s, 30 s, 1 min, 5 min, 15 min, and 30 min. The cube additionally retains 2 min, 10 min, and 60 min as reserve extraction levels. Daily ERA5 summaries are merged into `unit_context`; `weather_1min` preserves reusable continuous context for later RQ2 window definitions.

See `docs/STUDY_SPEC.md` for frozen scientific definitions and `docs/CORE_ARTIFACTS.md` for the artifact schema, support-lattice rules, reserve levels, weather interpolation, and QC.
