# Core analysis artifacts

## Purpose

The expensive boundary of this project is the conversion of harmonized high-resolution MeLiDos time series into exposure-representation values. The server build therefore stops at two durable compressed CSV artifacts:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
```

RQ1, RQ2, and RQ3 should be implemented downstream as table operations on these artifacts whenever possible. Distortion, bootstrap confidence intervals, model predictions, sufficiency thresholds, and Pareto labels are deliberately **not** stored in the core artifacts because they are analysis choices that may change.

## Runtime

The core build is pinned to **R 4.5.0**, matching the upstream Zauner `v0.9.9` runtime. Run `scripts/00_setup.R` under R 4.5.0 before rebuilding. The setup script restores the existing package environment, enforces `LightLogR 0.10.3` and `melidosData 1.0.6`, and then refreshes the lockfile under R 4.5.0.

## `metric_cube.csv.gz`

One row is one target metric value for one analysis unit, one measurement configuration, and one explicit support lattice.

Core columns:

```text
support_id
site
Id
analysis_unit_type
analysis_unit_id
Date
n_days
placement
optical
resolution_s
config_id
metric
metric_class
value
available
unavailable_reason
is_reference_config
```

The cube contains all observable combinations of:

- placement: eye, chest, wrist;
- optical channel: MEDI and, where a common dual-channel support exists, LIGHT as the one-channel proxy;
- temporal resolution: 10, 30, 60, 300, 900, and 1800 seconds;
- the published 54 metric definitions, with structurally unavailable representations retained as explicit unavailable rows.

### Support lattices

Support is part of the scientific object and must not be discarded. The cube uses six support families:

```text
eye_medi
eye_full
eye_chest_medi
eye_chest_full
eye_wrist_medi
eye_wrist_full
```

`*_medi` lattices preserve the maximal support needed for MEDI-based main effects. `*_full` lattices require all optical channels needed for joint optical configurations to be present on the same underlying time support. This prevents RQ1 main effects from being unnecessarily shrunk while still giving RQ2/RQ3 joint configurations a valid common support.

The same nominal configuration may therefore appear under more than one `support_id`. Never join or compare those rows while dropping `support_id`.

### Monitoring duration

Monitoring duration is intentionally not expanded into millions of redundant rows. For the 52 participant-day metrics, every `d`-day representation can be formed downstream by averaging the corresponding daily metric values over the selected day subset. This requires no return to the high-frequency time series.

IS and IV are different because they must be recomputed directly on the selected multi-day sequence. The cube includes their full observed multi-day value for each configuration, but it does **not** pre-materialize all duration-subset IS/IV values while the canonical 7-day reference-window issue remains unresolved. If duration is retained after that audit, only those two metrics require an additional targeted high-frequency computation.

## `unit_context.csv.gz`

One row is one support-specific participant-day. It contains the join key used by daily rows in `metric_cube` plus data-completeness/support information and site/date external context.

Core fields include:

- `support_id`, `site`, `Id`, `Date`, `analysis_unit_id`;
- site city, country, latitude, longitude, and timezone;
- expected and valid 10-s epochs, valid fraction, number of missing blocks, and largest missing gap;
- ERA5 daily summaries, when `data/raw/era5/<SITE>.csv` is present;
- calendar fields used for later modeling.

ERA5 is joined after converting the hourly UTC timestamps to the official MeLiDos site timezone and then assigning the local calendar date. The downloaded ERA5 CSVs remain the raw source; the context artifact stores daily summaries suitable for participant-day distortion models.

## Build

From the repository root after raw MeLiDos and ERA5 files are present:

```bash
Rscript scripts/00_setup.R
Rscript scripts/02_inventory.R
Rscript scripts/03_reproduce_upstream.R
Rscript scripts/04_validate_reproduction.R
CORE_WORKERS=24 CORE_FORCE=1 Rscript scripts/09_build_core_artifacts.R
```

On Windows, the core build falls back to one worker. On the intended Linux cloud worker, `CORE_WORKERS` controls process-level parallelism across independent support blocks. Completed support and metric blocks are persisted under `data/interim/core/`, so an interrupted run can be resumed with `CORE_FORCE=0`.

The two core artifacts are written only after all completed metric blocks are merged. A small factual build summary is written to `logs/core_artifact_summary.csv`, and the full R session is written to `logs/sessionInfo_core_artifacts.txt`.
