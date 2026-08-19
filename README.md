# measurement-sufficiency

Analysis repository for:

**How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**

## Planned workflow
1. Validate and inventory MeLiDos source data.
2. Reproduce upstream preprocessing and the published 54-metric pipeline.
3. Build the reusable configuration-level metric/context artifacts.
4. Derive RQ1 representation distortion from the core artifacts.
5. Explain fidelity heterogeneity for RQ2.
6. Derive sufficiency functions and multidimensional Pareto frontiers for RQ3.

See `AGENTS.md` and `docs/`.

## Reproduction foundation

Run from the repository root under **R 4.5.0**:

```bash
Rscript scripts/00_setup.R
Rscript scripts/01_download_melidos.R
Rscript scripts/02_inventory.R
Rscript scripts/03_reproduce_upstream.R
Rscript scripts/04_validate_reproduction.R
```

`scripts/01_download_melidos.R` never overwrites completed raw files. For a subset, set comma-separated `MELIDOS_SITES` and/or `MELIDOS_MODALITIES` environment variables before running it. The upstream reproduction intentionally implements Zauner's three-position primary scenario; that intersection is not imposed on the measurement-sufficiency support lattices.

## Core artifacts

After the MeLiDos source files are present and optional ERA5 site CSVs have been placed under `data/raw/era5/`, build the reusable analysis basis:

```bash
CORE_WORKERS=24 CORE_FORCE=1 Rscript scripts/09_build_core_artifacts.R
```

This writes:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
```

The expensive high-resolution computation ends there. Later distortion definitions, bootstrap choices, RQ2 models, and RQ3 sufficiency calculations should read these artifacts rather than recomputing the 54 metrics from the 10-s source. See `docs/CORE_ARTIFACTS.md` for the schema and support-lattice rules.
