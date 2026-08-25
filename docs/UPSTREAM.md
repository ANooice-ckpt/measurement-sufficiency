# Upstream sources

## MeLiDos
Data loader: melidosData 1.0.6

Use harmonized 10-s datasets as the source layer.

Core measurement modalities:
- light_glasses
- light_chest
- light_wrist
- wearlog
- sleepdiaries

RQ2 contextual covariates additionally use the harmonized continuous exports:
- lightexposurediary
- exercisediary
- sleepdiaries (work/free day and prior sleep duration)

The downloader validates these RData payloads with the same `melidos_io.R` loader used for the core inputs. The RQ2 contextual layer reuses the published MeLiDos field/category conventions; it does not reinterpret raw questionnaire codes independently of those exports.

Do not use pre-aggregated 1-minute datasets for primary analyses because temporal compression must be generated from the same 10-s reference.

## LightLogR
Installed reproduction version: 0.10.3.

Zauner release `v0.9.9` records LightLogR 0.10.3 in `renv.lock`; the earlier 0.10.0 entry was an unverified placeholder.

## Zauner et al.
Reference implementation:
`external/zauner_position/`

Checked-out reference: tag `v0.9.9`, commit `a74ec2acc84258ce87cc85b196f71b3a651522c4` (sparse checkout of reproduction inputs).

Use this repository to reproduce:
- preprocessing
- completeness rules
- definitions of the 54 metrics
- transformations
- metric classes

Do not modify files under `external/`.

## ERA5 context

ERA5 payloads are consumed only through the existing `weather_era5.R` ingestion and validation path. Daily RQ2 external-opportunity variables are read from the resulting core `unit_context` fields; RQ2 does not introduce a second file-format detector, time-zone convention, accumulation rule or daily weather aggregation.

## Environment boundary

The Zauner `v0.9.9` lockfile records **R 4.5.0**. New full reproductions, core-artifact builds, and final downstream analyses are therefore pinned to R 4.5.0.

The project was briefly snapshotted locally under R 4.4.2 during development. That stale runtime record is not part of the scientific specification. `scripts/00_setup.R` now handles this as a one-time migration: if the repository lockfile already records R 4.5.0 it performs a normal `renv::restore()`; if an older/unknown R runtime is recorded, it does not restore that stale library verbatim. Instead it installs an R-4.5-compatible project runtime, re-asserts LightLogR 0.10.3 and melidosData 1.0.6, and snapshots the resulting R 4.5.0 environment.

`renv/settings.json` explicitly encodes R 4.5.0 for future snapshots. After the one-time migration succeeds, the regenerated `renv.lock` should be committed and becomes the normal restore source for subsequent machines.

Earlier RQ1 exploratory outputs produced under R 4.4.2 are historical diagnostics only and should be replaced by the R 4.5.0 rebuild before final analysis.
