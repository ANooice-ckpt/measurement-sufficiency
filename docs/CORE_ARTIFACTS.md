# Core analysis artifacts

## Purpose

The expensive boundary of this project is the conversion of harmonized high-resolution MeLiDos time series into target representation values. The final server build therefore produces three durable compressed CSV artifacts:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
```

`metric_cube` and `unit_context` are the two main analysis artifacts. `weather_1min` is an auxiliary continuous meteorological artifact retained so that later RQ2 window definitions can change without redownloading or reprocessing ERA5.

Downstream distortion, bootstrap uncertainty, model predictions, sufficiency thresholds, and Pareto labels are deliberately not stored here; they remain cheap analysis choices.

## Runtime

The full build is pinned to **R 4.5.0**, matching the upstream Zauner `v0.9.9` runtime. Scientific package pins remain LightLogR 0.10.3 and melidosData 1.0.6.

Run `scripts/00_setup.R` under R 4.5.0 before rebuilding. It restores the recorded package environment, enforces the scientific package versions, and refreshes `renv.lock` under R 4.5.0.

## 1. `metric_cube.csv.gz`

One row is one target metric value for one analysis unit, one measurement configuration, and one explicit support lattice.

Core fields:

```text
support_id
support_role
site
Id
analysis_unit_type
analysis_unit_id
Date
n_days
placement
optical
resolution_s
is_primary_resolution
config_id
metric
metric_class
metric_scope
metric_geometry
value
available
unavailable_reason
is_reference_config
```

### Measurement configurations

The frozen primary temporal-resolution levels remain:

```text
10 s, 30 s, 60 s, 300 s, 900 s, 1800 s
```

For the one-time extraction run, the cube also retains inexpensive reserve levels:

```text
120 s, 600 s, 3600 s
```

`is_primary_resolution` distinguishes the frozen primary design from these reserve rows. The reserve rows do not redefine the primary RQs.

Optical rows use MEDI and, on dual-channel common supports, LIGHT as the operational one-channel proxy. Placement rows cover eye, chest, and wrist where the relevant ActLumus streams exist.

The 54 published metrics remain the target representation system. Structurally unavailable metric/configuration pairs are retained explicitly rather than silently dropped. MDER and nvRD require both MEDI and LIGHT and are therefore valid only on corresponding `*_full` supports. Pulse-family metrics use the upstream parameters; they are emitted only where those parameters are compatible with the configured epoch.

### Support lattices

Support is part of the scientific object and must never be dropped from joins/comparisons.

Primary/pairwise supports:

```text
eye_medi
eye_full
eye_chest_medi
eye_chest_full
eye_wrist_medi
eye_wrist_full
```

Reserve joint-placement supports:

```text
eye_chest_wrist_medi
eye_chest_wrist_full
```

`*_medi` preserves the maximal common support required for MEDI-based comparisons. `*_full` requires the relevant MEDI and LIGHT channels jointly. The all-position supports are retained only to preserve future multidimensional comparison options; they do not replace the pairwise maximal-support primary placement analyses.

## 2. `unit_context.csv.gz`

One row is one support-specific participant-day × measurement configuration. It joins directly to daily metric rows using:

```text
support_id + site + Id + Date + config_id
```

It contains:

- placement/optical/resolution configuration keys;
- valid-value fraction and missing-block structure after the configured measurement operation;
- participant support start/end, support span, number of valid days, and ordered valid-day index;
- site city/country/latitude/longitude/timezone;
- local calendar variables;
- ERA5 daily context summaries;
- `isiv_h00`–`isiv_h23`, the exact hourly transformed-light basis needed to reconstruct later day-subset IS/IV without returning to 10-s light data.

### Duration preservation

The artifact does not prematurely choose a canonical seven-day duration window. For the 52 daily metrics, arbitrary later `d`-day representations are obtained by averaging selected daily metric values. For IS/IV, the stored 24 hourly transformed-light values permit exact reconstruction of the LightLogR formulas for arbitrary selected days.

This means the current duration-reference ambiguity can be resolved scientifically later without another expensive high-resolution extraction.

## 3. `weather_1min.csv.gz`

ERA5 source files are expected at:

```text
data/raw/era5/<SITE>.csv
```

The reader accepts either a genuine CSV or a ZIP payload saved with a `.csv` filename by the CDS client, and accepts both long CDS variable names and the ERA5 short names (`t2m`, `d2m`, `ssrd`, `fdir`, etc.). `era5_manifest.csv` is not an analysis input.

ERA5 is hourly source data. The continuous artifact is therefore stored at **1-minute resolution**, which is ample interpolation density without multiplying a fundamentally hourly source to 10-second rows.

### Interpolation

Instantaneous variables use shape-preserving piecewise cubic Hermite interpolation (PCHIP) with no interpolation across source gaps longer than 90 minutes. This includes temperature, dew point, skin temperature, cloud cover, boundary/cloud-base height, pressure, and wind components. Wind speed is derived after interpolation of `u` and `v`.

Hourly accumulated radiation is first converted from J m-2 per source hour to mean W m-2 for that hour and located at the interval midpoint before PCHIP interpolation. Precipitation is **not** smoothed: the source hourly total is represented as a piecewise-constant hourly rate, avoiding invented rainfall timing.

Relative humidity and vapour-pressure deficit are derived from interpolated air/dew-point temperature. Diffuse solar radiation is derived as total downward solar minus direct solar when physically admissible. Direct fraction is constrained to [0,1].

Daily radiation and precipitation summaries in `unit_context` are always calculated directly from the original hourly accumulations, not from the interpolated minute table. ERA5 reanalysis accumulations are interpreted as the one-hour processing period ending at the validity time; daily attribution therefore uses the source-hour midpoint in the site's local timezone.

### ERA5 physical QC

The build applies deliberately broad plausibility bounds before interpolation. Values outside them are set to missing and recorded in `logs/era5_qc.csv`; these are QC guards, not claims about universal climatological limits.

Current bounds include:

```text
u/v wind components       -100 .. 100 m/s
2m temperature/dewpoint   -100 .. 70 C
skin temperature           -100 .. 80 C
boundary/cloud-base hgt       0 .. 20000 m
MSL pressure                800 .. 1100 hPa
surface pressure            500 .. 1100 hPa
SSRD / FDIR                   0 .. 5.5e6 J/m2/hour
STRD                          0 .. 3.0e6 J/m2/hour
total cloud cover             0 .. 1
precipitation                  0 .. 500 mm/hour
latitude                     -90 .. 90
longitude                   -180 .. 180
```

Tiny negative numerical noise in non-negative accumulated variables is coerced to zero. Dew point more than 0.5 C above air temperature is treated as invalid for derived humidity; direct radiation exceeding total downward solar is retained in the raw hourly fields but excluded from derived diffuse/direct-fraction quantities and counted in QC.

## Build and resumability

The server build writes persistent support, metric, context, and weather blocks under:

```text
data/interim/core/
```

An interrupted run can resume with `CORE_FORCE=0`.

From the repository root on Linux:

```bash
CORE_WORKERS=16 CORE_FORCE=1 bash scripts/run_core_artifacts.sh
```

For a 64-core / 128-GB machine, 16 workers is the conservative starting point because each worker can hold a large support block in memory. Increase only after observing real peak memory/CPU use.

Final diagnostics:

```text
logs/core_artifact_summary.csv
logs/era5_qc.csv
logs/era5_missing_study_dates.csv
logs/sessionInfo_core_artifacts.txt
```

The build stops on duplicate scientific keys or missing required ERA5 site files. Weather parsing/QC runs before the expensive light-metric stage so format errors fail early rather than after hours of computation.
