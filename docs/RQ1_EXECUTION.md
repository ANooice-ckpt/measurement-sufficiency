# RQ1 execution brief for Codex

## Objective
Run RQ1 entirely from the completed core artifacts, producing one canonical representation-distortion object plus stable tabular outputs, then render Fig. 1 from those frozen RQ1 outputs without rerunning the analysis.

This is an execution task, not a design task. Scientific definitions are frozen in `docs/STUDY_SPEC.md`.

## 0. Preconditions
The following core artifacts must exist and the core build must have completed without blocking diagnostics:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
logs/core_artifact_summary.csv
logs/era5_qc.csv
logs/era5_missing_study_dates.csv
logs/sessionInfo_core_artifacts.txt
```

The expensive high-resolution metric computation ends at the core-artifact layer. Do not return to the harmonized 10-s source for ordinary RQ1 changes.

## 1. Frozen downstream code structure
There are exactly two current downstream RQ1 scripts:

```text
scripts/10_rq1_analysis.R
scripts/11_plot_fig1.R
```

### `10_rq1_analysis.R`
Reads `metric_cube.csv.gz` and `unit_context.csv.gz`, constructs all primary RQ1 comparison lattices, monitoring-duration windows, standardized signed distortion, uncertainty, and supporting diagnostics.

It writes the canonical analysis artifact and all tables needed by the main text, supplement, and Fig. 1.

### `11_plot_fig1.R`
Reads only frozen RQ1 outputs from script `10`. It may estimate plotting densities and change visual styling, but must not recompute reference metric values, duration windows, standardizers, `e`, `A`, `B`, or bootstrap intervals.

Changing Fig. 1 aesthetics therefore never requires rerunning RQ1 analysis.

The superseded pre-core `05`–`08` RQ1 scripts are removed from `master`. `scripts/utils/rq1_metrics.R` remains an upstream core-extraction helper because `09_build_core_artifacts.R` depends on it; do not treat it as a downstream RQ1 executable.

## 2. Core input semantics
`metric_cube.csv.gz` contains one target metric value for one analysis unit × measurement configuration × explicit support lattice.

`unit_context.csv.gz` contains support/configuration-specific participant-day context and the stored `isiv_h00`–`isiv_h23` hourly transformed-light basis required to reconstruct monitoring-duration IS/IV without returning to high-resolution light records.

Never collapse across incompatible `support_id` values.

## 3. Primary RQ1 configurations
### Placement
- reference: eye / near-corneal, MEDI, 10 s
- alternatives: chest and wrist
- eye–chest and eye–wrist use separate maximal pairwise supports
- non-dual-channel metrics use the corresponding `*_medi` lattice
- MDER/nvRD use the corresponding `*_full` lattice

### Optical
- reference: eye MEDI, 10 s
- alternative: synchronous eye photopic illuminance (`LIGHT`)
- comparison uses `eye_full`
- MDER and nvRD are unavailable for the LIGHT-only candidate

### Temporal resolution
Reference: 10 s.

Primary candidates:

```text
15 s
20 s
30 s
1 min
5 min
15 min
30 min
```

Reserve 2 min, 10 min, and 60 min rows in the core cube are not primary RQ1 results.

Pulse-family representations are unavailable at epochs >=5 min under the frozen upstream operator definitions and must remain unavailable rather than be assigned extreme distortion.

### Monitoring duration
Reference: an unambiguous sequence of exactly seven consecutive valid eye-MEDI monitoring days.

For each eligible participant and each `d = 1..6`, enumerate every contiguous `d`-day window within that fixed seven-day sequence:

`W_(d,j) = {j, ..., j+d-1}, j = 1, ..., 8-d`.

Do not arbitrarily select seven days from participants with >7 valid days.

For daily metrics:
- linear representations use the arithmetic mean across selected days;
- circular-time representations use the circular mean on the 24-h clock.

For IS/IV, reconstruct directly from the stored hourly transformed-light basis. The reconstructed seven-day value must reproduce the corresponding core seven-day IS/IV value to numerical tolerance.

## 4. Distortion and comparison lattices
For analysis unit `i`, metric `k`, candidate configuration `c`, and comparison lattice `g`:

`e[i,k,c] = delta_k(candidate, reference) / s[k,0,g]`.

Rules:
- ordinary signed differences for linear metrics;
- shortest signed 24-h circular difference for circular-time metrics;
- `s[k,0,g]` is the reference dispersion on the comparison lattice and is fixed across candidate levels within that lattice;
- temporal levels share one standardizer within each metric;
- duration levels share one eligible seven-day reference standardizer within each metric;
- eye–chest and eye–wrist remain separate placement lattices;
- zero or undefined reference dispersion makes standardized distortion unavailable rather than triggering an arbitrary epsilon denominator.

The smallest-unit `e` distribution is the primary RQ1 object.

## 5. Frozen outputs from `10_rq1_analysis.R`
### Canonical distortion artifact

```text
data/derived/rq1/rq1_distortion_long.rds
```

It contains, at minimum:

```text
dimension
configuration
configuration_label
configuration_order
comparison_lattice
reference_configuration
support_id
site
Id
analysis_unit_type
analysis_unit_id
reference_unit_id
Date
window_id
window_index
window_start
window_end
n_days
metric
metric_class
metric_scope
metric_geometry
candidate_config_id
reference_config_id
reference_value
candidate_value
delta
standardizer
e
available
unavailable_reason
```

### Main/supporting tables

```text
results/rq1/rq1_summary.csv
results/rq1/rq1_configuration_manifest.csv
results/rq1/rq1_metric_availability.csv
results/rq1/rq1_sample_flow.csv
results/rq1/rq1_retention_diagnostics.csv
results/rq1/RQ1_RUN_REPORT.md
```

`rq1_summary.csv` is one row per metric × primary candidate configuration and includes:

```text
n_participants
n_units
median_e
q25_e
q75_e
p025_e
p975_e
B_mean_signed
A_mean_absolute
bootstrap_supported
B_ci_low
B_ci_high
A_ci_low
A_ci_high
uncertainty_method
```

Lin's CCC and Spearman correlation in `rq1_retention_diagnostics.csv` are descriptive supplementary retention diagnostics for linear representations only. They do not replace `D(e)`, `A`, or `B`.

### Diagnostics

```text
results/diagnostics/rq1_standardizer_audit.csv
results/diagnostics/rq1_geometry_invariant.csv
results/diagnostics/rq1_support_audit.csv
results/diagnostics/rq1_duration_cohort_audit.csv
results/diagnostics/rq1_duration_windows.csv
results/diagnostics/rq1_duration_isiv_reconstruction.csv
```

## 6. Uncertainty
For placement, optical, and temporal resolution, use participant-cluster bootstrap stratified by study site, preserving all smallest-unit rows belonging to a sampled participant. Default to 1000 replicates with a fixed seed; `RQ1_BOOT` may override the replicate count for deliberate rough runs.

The implementation checks whether site-stratified participant resampling has any non-degenerate stratum. If not, bootstrap intervals are left unavailable and the point estimate plus complete empirical unit distribution are retained.

Under the current strict duration cohort, each eligible site contributes only one participant, so no population-style duration bootstrap CI should be manufactured.

## 7. Required diagnostics
### Geometry
Every finite summary row must satisfy:

`A_mean_absolute >= abs(B_mean_signed)`

within numerical tolerance.

### Standardizer
Record metric × comparison lattice reference scale, reference-unit count, and zero/near-zero flag.

### Support
Record the actual `support_id`, candidate config, and reference config used by every metric × primary comparison so maximal pairwise supports cannot silently be replaced by stricter joint supports.

### Duration
Record exact eligible participants, ordered dates, every contiguous candidate window, and the seven-day IS/IV reconstruction check.

## 8. Frozen Fig. 1 grammar
`11_plot_fig1.R` generates:

```text
results/figures/Fig1_RQ1.pdf
results/figures/Fig1_RQ1.png
results/rq1/fig1_pooled_distribution_density.csv
```

Fig. 1 is a **2 × 4 matrix**. Columns are the four measurement dimensions in the same order throughout:

```text
Placement | Optical proxy | Temporal resolution | Monitoring duration
```

### Top row: empirical distortion distributions (`a`–`d`)
Each panel displays the empirical standardized signed-distortion distribution for all primary candidate states in that measurement dimension.

- x-axis: standardized signed distortion `e`
- y-axis: candidate configuration level/state
- placement: chest and wrist
- optical: photopic proxy
- temporal: all seven primary coarser resolutions
- duration: all contiguous 1–6 d candidate families relative to 7 d

For this top-row overview, smallest-unit `e` values are descriptively pooled across target metrics **after within-metric standardization**. Each metric × configuration contributes total weight 1, so metrics with more analysis units do not dominate the pooled density. This is a descriptive visualization of the distortion landscape, not a class-level inferential distribution.

The old four hand-picked archetypes (low distortion, positive directional, negative directional, cancellation) are not part of the main Fig. 1 grammar.

### Bottom row: A–B distortion geometry (`e`–`h`)
- x-axis: `B = mean(e)`
- y-axis: `A = mean(|e|)`
- show the boundaries `A = |B|`
- color encodes the six published metric classes
- each plotted state is one metric × candidate configuration

Placement displays chest and wrist in the same panel. Optical displays the photopic proxy. Temporal displays **all primary resolutions** for each metric as an ordered trajectory from finer to coarser measurement. Duration displays **all 1–6 d levels** for each metric as an ordered trajectory from longer to shorter monitoring.

Only a small number of extreme terminal metrics are labelled. Bootstrap error bars may be shown in sparse placement/optical panels where legible; the dense temporal/duration trajectory panels retain their uncertainty in `rq1_summary.csv` rather than crowding the main figure. Duration never displays a degenerate population bootstrap CI.

This figure is therefore a distortion-characterization figure, not a sufficiency figure: it shows `configuration -> D(e) -> (A,B)`. RQ3/Fig. 4 later performs the distinct inverse decision mapping from acceptable tolerance `epsilon` to sufficient measurement requirement.

## 9. Run sequence after the core artifacts complete
From the repository root:

```bash
Rscript scripts/10_rq1_analysis.R
Rscript scripts/11_plot_fig1.R
```

If only Fig. 1 aesthetics change, rerun `11_plot_fig1.R` alone.

Stop only for a scientific or structural failure: missing/invalid core artifacts, incompatible support joins, an unreconcilable metric definition, failed IS/IV reconstruction, degenerate standardizer for a requested standardized result, or violation of `A >= |B|`. Ordinary plotting changes are not reasons to rerun the core or RQ1 analysis.
