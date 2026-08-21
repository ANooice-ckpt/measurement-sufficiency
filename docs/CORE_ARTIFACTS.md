# Core artifact contract — v3 sparse sampling / complete analysis days

## Purpose

The core layer materializes expensive configuration-level target values once. RQ1–RQ3 consume those durable values and do not return to raw 10-s series for ordinary downstream changes.

Current version: v3_sparse_sampling_complete_days.

    results/core/
      cache/<core_artifact_version>/
        supports/
        metrics/
        context/
        weather/
        duration_parts/
      metric_cube.csv.gz
      unit_context.csv.gz
      weather_1min.csv.gz
      duration_window_manifest.rds
      duration_metric_cube.rds
      core_manifest.csv

data/ contains raw inputs only. All caches and generated artifacts are under results/.

## Support lattice

Primary/pairwise supports remain:

- eye_medi, eye_full
- eye_chest_medi, eye_chest_full
- eye_wrist_medi, eye_wrist_full

Reserve all-position supports remain available only for analyses that genuinely require all three placements. Pairwise supports retain their maximal comparison-specific support.

## Temporal extraction

The harmonized 10-s schedule is the source grid.

Primary states: 10, 20, 30, 60, 300, 900 and 1800 s. Reserve states: 120, 600 and 3600 s. 15 s is excluded.

core_make_series() performs deterministic participant/source-grid-phase-anchored systematic sparse subsampling. A retained row must have the exact source timestamp and MEDI/LIGHT value. No bin average, interpolation or hidden reconstruction is permitted.

## Duration artifact

The primary duration domain is 1–6 complete analysis days. A complete analysis day is a day retained by the common core completeness preprocessing. For each participant and support, consecutive complete-day runs are identified. Every contiguous window of 1–6 days inside every run is enumerated; a run longer than six days contributes all legal windows and no selected “best” subset.

trial_times metadata is still carried in unit_context for audit and descriptive/sensitivity analyses. It is not a primary eligibility criterion and does not create a seven-day reference.

duration_window_manifest.rds contains:

- support, site, participant and run identifiers;
- run start/end and complete-day count;
- window identifier, start/end, length and member dates;
- nesting and adjacent-window identifiers.

duration_metric_cube.rds is a compact manifest for partitioned RDS blocks under
results/core/cache/<core_artifact_version>/duration_parts/. The blocks contain the actual
window representation Y_ik(p,o,r,d,window), with support, participant, window, placement,
optical state, resolution, metric metadata, value, availability and unavailable reason.
Daily-defined metrics are aggregated from durable participant-day values using the existing
linear/circular semantics. IS/IV are rebuilt on exact selected dates from the stored hourly basis.

duration_metric_cube_parts.csv records the block-to-support/site mapping. A block is installed
atomically and receives a .ok marker only after the RDS is closed; CORE_DURATION_ONLY=1 can
therefore resume an interrupted duration build from completed blocks. The manifest is written
only after all expected blocks are available.

## Version and compatibility

Every main artifact carries core_artifact_version. A change to duration semantics, schema or output paths requires a normal core-version bump. No artifact hash or second contract system is used.
