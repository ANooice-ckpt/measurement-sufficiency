# measurement-sufficiency

Analysis repository for:

**How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**

## Planned workflow
1. Validate and inventory MeLiDos source data.
2. Reproduce upstream preprocessing and the published 54-metric pipeline.
3. Build the reusable configuration-level metric/context/weather artifacts.
4. Derive RQ1 representation distortion from the core artifacts.
5. Explain fidelity heterogeneity for RQ2.
6. Derive sufficiency functions and multidimensional Pareto frontiers for RQ3.

See `AGENTS.md` and `docs/`.

## Reproduction foundation

Run from the repository root under **R 4.5.0**. The one-command final build below runs the required setup, inventory, upstream reproduction/validation, ERA5 preflight, and artifact extraction in order.

MeLiDos source files live under `data/raw/melidos/`. ERA5 site payloads live under `data/raw/era5/` and must include the nine site files named `BAUA.csv`, `FUSPCEU.csv`, `IZTECH.csv`, `KNUST.csv`, `MPI.csv`, `RISE.csv`, `THUAS.csv`, `TUM.csv`, and `UCR.csv`. The ERA5 reader accepts either plain CSV or ZIP-with-CSV payloads saved with a `.csv` extension.

## Core artifacts

On Linux:

```bash
CORE_WORKERS=16 CORE_FORCE=1 bash scripts/run_core_artifacts.sh
```

This writes:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
```

The expensive high-resolution computation ends there. Later distortion definitions, bootstrap choices, RQ2 models, and RQ3 sufficiency calculations should read these artifacts rather than recomputing the 54 metrics from the 10-s source. Daily ERA5 summaries are already merged into `unit_context`; `weather_1min` preserves reusable continuous context for later window definitions.

See `docs/CORE_ARTIFACTS.md` for the schema, reserve extraction levels, support-lattice rules, weather interpolation, and QC.
