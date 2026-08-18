# measurement-sufficiency

Analysis repository for:

**How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**

## Planned workflow
1. Validate and inventory MeLiDos source data.
2. Reproduce upstream preprocessing and the published 54-metric pipeline.
3. Implement RQ1 measurement-compression operators.
4. Explain fidelity heterogeneity for RQ2.
5. Derive single-dimension sufficiency curves and multidimensional Pareto frontiers for RQ3.

See `AGENTS.md` and `docs/`.

## Reproduction foundation

Run from the repository root:

```powershell
Rscript scripts/00_setup.R
Rscript scripts/01_download_melidos.R
Rscript scripts/02_inventory.R
Rscript scripts/03_reproduce_upstream.R
Rscript scripts/04_validate_reproduction.R
```

`scripts/01_download_melidos.R` never overwrites completed raw files. For a subset, set comma-separated `MELIDOS_SITES` and/or `MELIDOS_MODALITIES` environment variables before running it. The current upstream reproduction intentionally implements Zauner's three-position primary scenario; that intersection is not imposed on future measurement-sufficiency operators.
