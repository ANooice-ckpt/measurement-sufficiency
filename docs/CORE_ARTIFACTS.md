# Core artifact contract — v4 sparse sampling / complete analysis days

## Purpose

The core layer materializes expensive configuration-level target values once. RQ1–RQ3 consume those durable values and do not return to raw 10-s series for ordinary downstream changes.

The current core version is generated from the frozen measurement design and has the form:

    v4_sparse_sampling_complete_days__<core_design_id>

The design identifier is part of the cache key, so a temporal-lattice change cannot silently reuse an incompatible core cache.

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

Primary states: **10, 20, 30, 40, 60 and 120 s**. The only active reserve state is **300 s (5 min)**, retained as an intentionally coarse sensitivity condition. Cadences above 5 min are no longer materialized. Fifteen seconds remains excluded because it cannot be obtained by equal-spacing subsampling of the 10-s grid.

`core_make_series()` performs deterministic participant/source-grid-phase-anchored systematic sparse subsampling. A retained row must have the exact source timestamp and MEDI/LIGHT value. No bin average, interpolation or hidden reconstruction is permitted. The resulting temporal state is therefore a literal sparse-sampling interval.

All primary states are below the pulse-operator availability boundary and preserve the full primary target-representation set, subject to the normal optical/support restrictions. Pulse-derived metrics are unavailable at the 5-min reserve state.

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
atomically and receives a .ok marker only after the RDS has been closed; CORE_DURATION_ONLY=1 can
therefore resume an interrupted duration build from completed blocks. The manifest is written
only after all expected blocks are available.

## Version and compatibility

The single source of truth for the active temporal and duration domains is `scripts/utils/analysis_design.R`. `core_artifact_version()` includes the corresponding core-design identifier. RQ1, RQ2 and RQ3 also carry the analysis-design identifier in their artifact versions, preventing stale pairwise, model or sufficiency caches from being reused after a design change.

Every main artifact carries `core_artifact_version`. A change to measurement-state semantics, duration semantics, schema or output paths requires a normal core-version change. No parallel hash/contract system is used.
