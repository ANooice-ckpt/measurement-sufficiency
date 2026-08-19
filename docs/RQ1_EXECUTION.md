# RQ1 execution brief for Codex

## Objective
Implement and **run** RQ1 from the completed core artifacts, producing the canonical representation-distortion dataset, unified RQ1 summaries, and a reproducible rough draft of Fig. 1.

This is an execution task, not a design task. Scientific definitions are frozen in `docs/STUDY_SPEC.md`. The expensive high-resolution metric computation has been moved upstream into the core-artifact build; do not rerun it for ordinary RQ1 changes.

## 0. Preconditions
Confirm that the following exist and were produced without blocking diagnostics:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
logs/core_artifact_summary.csv
logs/era5_qc.csv
logs/era5_missing_study_dates.csv
logs/sessionInfo_core_artifacts.txt
```

The upstream reproduction and structural validator must already have passed. Numerical differences above the explicitly authorized floating-point tolerance may be recorded, but ordered keys, row structure, 54 metrics, and six metric classes must agree with the upstream reference.

Do **not** use the old `scripts/05_rq1_reference.R`–`08_plot_fig1.R` or `06b_rq1_duration.R` as the final analysis source. They remain useful historical checks only. The final RQ1 must be reconstructed from the core artifacts.

## 1. Core input semantics
`metric_cube.csv.gz` contains one target metric value for one analysis unit × measurement configuration × explicit `support_id`.

`unit_context.csv.gz` contains the support/configuration-specific participant-day context and the hourly transformed-light basis required for later IS/IV duration reconstruction.

Preserve at minimum these scientific keys through downstream joins:

```text
support_id
site
Id
analysis_unit_type
analysis_unit_id
Date
placement
optical
resolution_s
config_id
metric
```

Never collapse across incompatible `support_id` values.

## 2. Primary configuration levels
### Placement
- reference: eye / near-corneal
- alternatives: chest, wrist
- use separate eye–chest and eye–wrist maximal pairwise supports
- do not replace them with the stricter all-position intersection

### Optical
- reference: eye `MEDI`
- alternative: synchronous eye `LIGHT`
- MDER and nvRD are unavailable for `LIGHT`-only proxy configurations

### Temporal resolution
Primary levels are:

```text
10 s reference
15 s
20 s
30 s
60 s
300 s
900 s
1800 s
```

Reserve extraction levels `120 s`, `600 s`, and `3600 s` remain in the cube but are not primary RQ1 results by default.

Pulse-family metrics are unavailable at epochs `>= 300 s` under the current upstream operator definitions. Treat them as unavailable, not as extreme distortion.

### Monitoring duration
Primary duration analysis uses an unambiguous reference of exactly seven consecutive valid eye-MEDI days. Under the current frozen rule, do not arbitrarily select seven days from a participant with more than seven valid days.

For each eligible participant and `d = 1..6`, enumerate every contiguous `d`-day window within the fixed seven-day reference:

`W_(d,j) = {j, ..., j+d-1}, j = 1, ..., 8-d`.

For the 52 daily metrics:
- arithmetic mean across selected days for linear metrics;
- circular mean across selected days for circular-time metrics.

For IS/IV, reconstruct the selected-window metric from the stored `isiv_h00`–`isiv_h23` basis in `unit_context`; do not return to 10-s light data.

The current strict cohort contains three eligible participants, one each from FUSPCEU, IZTECH, and RISE. THUAS has an eight-valid-day participant and is excluded under the frozen primary rule unless the canonical seven-day protocol window can later be resolved without arbitrary selection.

## 3. Canonical RQ1 data products
Create a unified configuration-level metric-value table from the core artifacts before distortion. Keep explicit availability and support fields.

Then create one canonical long-form distortion object, preferably:

`data/derived/rq1_distortion_long.rds`

Required semantics:

```text
dimension
configuration
reference_configuration
support_id / comparison_lattice
site
Id
analysis_unit_id
Date
window_id              # duration only
n_days                 # duration only
metric
metric_class
metric_geometry
reference_value
candidate_value
delta
standardizer
e
available
unavailable_reason
```

Do not store only aggregated `A`/`B`. The row-level `e` distribution is the primary object.

## 4. Comparison lattices and standardizers
For analysis unit `i`, metric `k`, candidate `c`, and comparison lattice `g`:

`e[i,k,c] = delta_k(candidate, reference) / s[k,0,g]`.

Rules:
- `s[k,0,g]` is estimated from the reference values on the comparison lattice and is fixed across all candidate levels within that lattice;
- temporal primary levels share one reference support/standardizer within a metric;
- duration levels share one eligible seven-day cohort/standardizer within a metric;
- eye–chest and eye–wrist are separate placement lattices and may have different standardizers;
- use shortest signed circular differences for circular-time metrics and compatible circular dispersion;
- if reference dispersion is zero or numerically degenerate, mark standardized distortion undefined and report it; do not insert an arbitrary epsilon.

## 5. RQ1 summary table
Create:

`results/rq1/rq1_summary.csv`

One row per metric × primary configuration with at least:

```text
dimension
configuration
metric
metric_class
n_participants
n_units
median_e
q25_e
q75_e
p025_e
p975_e
B_mean_signed
A_mean_absolute
B_ci_low
B_ci_high
A_ci_low
A_ci_high
```

For duration, CI fields may be left unavailable or separately flagged because the current site-stratified participant bootstrap degenerates with one eligible participant per site.

## 6. Bootstrap and uncertainty
For placement, optical, and temporal resolution:
- resample participants within study site;
- preserve every row belonging to a sampled participant;
- use a fixed seed recorded in code;
- 1000 replicates is the default unless runtime clearly requires a smaller named rough-run value.

For the current duration cohort, do not report a site-stratified participant bootstrap as population-level uncertainty. Preserve and report the complete participant × contiguous-window empirical distribution and point estimates. The small cohort is a data-support limitation, not a reason to relax the frozen eligibility rule.

Do not bootstrap metric classes as if metrics were independent observations.

## 7. Availability and sample-flow outputs
Create:

```text
results/rq1/rq1_sample_flow.csv
results/rq1/rq1_metric_availability.csv
```

The availability table must distinguish:
- structurally unavailable representations;
- metric undefined/missing on a specific analysis unit;
- zero/undefined standardizer.

Do not infer availability from failed function calls because the expensive metric functions have already run upstream in the core build.

## 8. Required diagnostics
Save under `results/diagnostics/`.

### 8.1 Geometry invariant
For every finite summary row verify:

`A_mean_absolute + tolerance >= abs(B_mean_signed)`.

Any violation is an implementation error.

### 8.2 Standardizer audit
Report metric × comparison lattice:
- standardizer;
- number of reference units;
- zero/near-zero flag.

### 8.3 Support audit
Verify that each candidate/reference pair uses the intended `support_id` and that no join silently mixes maximal pairwise supports with stricter joint supports.

### 8.4 Availability audit
Verify that optical MDER/nvRD and temporal pulse-family unavailability follow the frozen rules.

### 8.5 Duration cohort audit
Report:
- valid-day counts by participant;
- exact eligible seven-day cohort;
- ordered valid dates;
- all primary contiguous windows generated for each `d`;
- confirmation that participants with ambiguous >7-day windows were not arbitrarily truncated.

## 9. Fig. 1 rough draft
Generate:

```text
results/figures/Fig1_RQ1.pdf
results/figures/Fig1_RQ1.png
```

Panel a uses algorithmically selected representative empirical `D(e)` distributions showing low distortion, positive directional distortion, negative directional distortion, and high bidirectional distortion with cancellation.

Panels b–e use:
- x = `B_mean_signed`
- y = `A_mean_absolute`
- boundaries `y = |x|`
- one point per metric
- color = published metric class
- b = placement
- c = optical
- d = 30 min vs 10 s
- e = 1 d vs 7 d

Show participant-cluster bootstrap uncertainty where supported. Do not fabricate duration CIs from the degenerate three-stratum bootstrap.

Record panel-a example IDs in:

`results/rq1/fig1_panel_a_examples.csv`.

Do not spend time on journal-perfect styling before the numerical structure is validated.

## 10. Completion report
Write:

`results/rq1/RQ1_RUN_REPORT.md`

Keep it factual and short. Include:
- core artifact inputs used;
- downstream scripts run;
- sample sizes by dimension;
- unavailable metrics/configurations;
- whether diagnostics passed;
- duration data-support limitation;
- paths to canonical tables and Fig. 1;
- any unresolved issue that materially affects interpretation.

## 11. Stop conditions
Stop and report only if:
- the final core artifacts are absent or structurally invalid;
- a target metric cannot be reconciled with the upstream implementation;
- the intended support lattice cannot be identified without changing the estimand;
- standardized distortion requires an arbitrary denominator because reference dispersion is zero/near-zero;
- the duration seven-day reference requires an arbitrary selection not authorized by the frozen rule;
- a proposed workaround changes the scientific estimand.

Otherwise, make ordinary engineering decisions directly and continue execution.
