# Core artifact contract — v2 sparse sampling / protocol metadata

## Purpose
The core layer materializes expensive configuration-level target values once. RQ1–RQ3 should not return to the harmonized 10-s source for ordinary downstream changes.

Current version: `v2_sparse_sampling_protocol7`.

Outputs:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
data/derived/core/core_manifest.csv
```

Every main artifact carries `core_artifact_version`.

## Support lattice
Primary/pairwise:
- `eye_medi`, `eye_full`
- `eye_chest_medi`, `eye_chest_full`
- `eye_wrist_medi`, `eye_wrist_full`

Reserve all-position:
- `eye_chest_wrist_medi`, `eye_chest_wrist_full`

Pairwise supports preserve maximal RQ1 samples. All-position supports are retained for analyses that genuinely require all three placements; they are not the default multidimensional RQ3 cohort.

## Temporal extraction
Source schedule: harmonized 10 s.

Primary: `10,20,30,60,300,900,1800 s`.
Reserve: `120,600,3600 s`.

`core_make_series()` implements clock-anchored systematic sparse subsampling. For `r>10`, rows are retained only when their integer epoch timestamp is divisible by `r`. No bin mean, interpolation, or value transformation occurs. The function stops if timestamps are not on the 10-s grid or a retained value differs from its source value.

Pulse-family metrics remain structurally unavailable for configured intervals >=300 s under the LightLogR operator contract. MDER/nvRD remain unavailable for LIGHT-only candidate representation.

## Protocol metadata
`scripts/01_download_melidos.R` obtains site `trial_times.RData`. `scripts/04c_prepare_raw_eye_spans.R` materializes:

```text
data/interim/core/protocol_participant_metadata.rds
logs/protocol_participant_metadata.csv
```

`unit_context` carries protocol start/end timestamps and dates, protocol day index, and flags for protocol days 1–7 / return date. Final selection remains downstream because validity is support-specific: participants need at least seven valid protocol-interval dates, and the first seven chronologically are fixed as the reference.

## Versioned cache
Interim blocks live under:

```text
data/interim/core/<core_artifact_version>/weather/
data/interim/core/<core_artifact_version>/supports/
data/interim/core/<core_artifact_version>/metrics/
data/interim/core/<core_artifact_version>/context/
```

A scientific operator change requires a new core version. `CORE_FORCE=0` may safely resume the current version; old-version blocks are not visible to the new build.

## metric_cube minimum fields
- version: `core_artifact_version`
- support/unit: `support_id`, `support_role`, `site`, `Id`, `analysis_unit_type`, `analysis_unit_id`, `Date`, `n_days`
- configuration: `placement`, `optical`, `resolution_s`, `is_primary_resolution`, `config_id`
- representation: `metric`, `metric_class`, `metric_scope`, `metric_geometry`, `value`, `available`, `unavailable_reason`, `is_reference_config`

## unit_context minimum fields
Alongside the scientific key `support_id + site + Id + Date + config_id`, retain:
- support validity / recording span;
- protocol metadata and protocol-day indices;
- configured missingness/completeness summaries;
- `isiv_h00..isiv_h23` hourly transformed-light basis;
- site metadata and local date fields;
- daily ERA5 context.

## Build
Fresh Linux build:

```bash
CORE_WORKERS=48 CORE_FORCE=0 bash scripts/run_core_artifacts.sh
```

The wrapper downloads/validates MeLiDos inputs including trial metadata, validates upstream reproduction and ERA5, prepares protocol metadata, and then runs the versioned core extraction.
