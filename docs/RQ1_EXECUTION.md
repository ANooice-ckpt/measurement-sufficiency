# RQ1 execution brief for Codex

## Objective
Implement and **run** RQ1 from the local MeLiDos data, producing the canonical representation-distortion dataset and a reproducible rough draft of Fig. 1.

This is an execution task, not a design task. Scientific definitions are frozen in `docs/STUDY_SPEC.md`. Prefer direct analysis progress over engineering scaffolding.

## 0. Preflight — do this first
From the repository root:

```powershell
Rscript scripts/02_inventory.R
Rscript scripts/03_reproduce_upstream.R
Rscript scripts/04_validate_reproduction.R
```

Requirements:
- `04_validate_reproduction.R` must pass ordered-key and numerical-value validation.
- Inspect `results/diagnostics/upstream_value_comparison.csv` if validation fails; do not silently relax tolerance.
- Regenerated `docs/DATA_INVENTORY.md` should report participant-level epoch spacing rather than cross-participant timestamp interleaving.

Then perform a short RQ1 data audit in code before computing distortion:
1. actual columns available in each light modality (`MEDI`, `LIGHT`, identifiers, timestamps);
2. site/device combinations relevant to eye–chest and eye–wrist comparison;
3. pairwise participant counts after cleaning and after common-support completeness rules;
4. number of valid near-corneal days per participant after the upstream cleaning/completeness logic;
5. number and site distribution of participants supporting an unambiguous 7-valid-day reference window;
6. metric-level optical availability under the rule in `STUDY_SPEC.md`.

Save the audit rather than only printing it.

## 1. Files to implement
Use the existing repository style. The preferred minimal structure is:

```text
scripts/
  05_rq1_reference.R
  06_rq1_configurations.R
  07_rq1_distortion.R
  08_plot_fig1.R
```

Add small helpers under `scripts/utils/` only when they remove real duplication (for example, the exact 54-metric computation or distortion calculation). Do not create a package or workflow framework.

The files above are a preferred organization, not a reason to duplicate heavy data processing. If one combined script is materially clearer/faster, keep the same numbered phase logic and required outputs.

## 2. Reference/cleaning implementation
Build RQ1 inputs from the harmonized source layer, **not** from `data/interim/zauner_primary_cleaned.rds`, because that file intentionally contains Zauner’s three-position intersection.

Reuse the exact upstream logic from `scripts/03_reproduce_upstream.R` for:
- state annotation,
- non-wear handling,
- range filtering,
- hour/day completeness,
- LightLogR metric definitions.

Generalize only the concurrency/sample restriction required by each RQ1 dimension.

Do not modify `scripts/03_reproduce_upstream.R` merely to make it serve RQ1. The reproduction script should remain an upstream reference implementation.

## 3. Canonical RQ1 data products
Create one canonical long-form distortion object, preferably:

`data/derived/rq1_distortion_long.rds`

Required fields (names may differ slightly but semantics must be explicit):

```text
dimension
configuration
reference_configuration
site
Id
analysis_unit_id
Date                  # when applicable
subset_id             # duration analysis when applicable
n_days                 # duration analysis when applicable
metric
metric_class
reference_value
candidate_value
delta
standardizer
e
available
unavailable_reason
```

Do not store only aggregated `A`/`B`. The row-level `e` distribution is the primary object.

Also save the configuration-specific metric values before distortion, either as an intermediate RDS or in a clearly reproducible script stage.

## 4. Sample and availability outputs
Create:

`results/rq1/rq1_sample_flow.csv`

At minimum report, by dimension/configuration/site:
- participants before cleaning,
- participants after cleaning,
- participant-days before/after completeness,
- smallest analysis units entering distortion,
- exclusions/unavailable cases.

Create:

`results/rq1/rq1_metric_availability.csv`

with one row per metric × configuration family, including whether the representation is computable and the reason when it is not.

For the optical axis, build the availability table explicitly before calculating optical distortion. Do not let failed function calls implicitly define availability.

## 5. RQ1 summary table
Create:

`results/rq1/rq1_summary.csv`

One row per metric × configuration with at least:

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

Bootstrap:
- resample participants within site;
- preserve all rows belonging to a sampled participant;
- use a fixed seed recorded in the script;
- use enough replicates for stable 95% intervals without creating unnecessary runtime. Start with 1000 unless local runtime makes this clearly impractical; if reduced for a rough run, make the number a named constant and record it.

Do not bootstrap metric classes as if metrics were independent observations.

## 6. Required diagnostics
Save explicit diagnostics under `results/diagnostics/`.

### 6.1 Reference reproduction check
On the overlapping sample/configuration, verify that the RQ1 metric engine reproduces the corresponding upstream metric values to numerical tolerance. This is especially important if the 54-metric computation is factored into a helper.

### 6.2 Geometry invariant
For every finite summary row, verify:

`A_mean_absolute + tolerance >= abs(B_mean_signed)`

Any violation is an implementation error.

### 6.3 Standardizer audit
Report metric × comparison lattice:
- reference standardizer,
- number of reference units,
- zero/near-zero dispersion flags.

Within a temporal-resolution metric, the standardizer must be identical across 30 s / 1 / 5 / 15 / 30 min. Within duration, it must be identical across 1–6 d. Placement pairwise lattices may have different standardizers because their maximal common supports may differ.

### 6.4 Missing-support audit
Check that temporal aggregation does not manufacture exposure values from fully missing bins and that candidate/reference comparison units are based on the intended common support.

### 6.5 Duration cohort audit
Report the distribution of valid near-corneal days per participant and the exact rule used to identify the canonical 7-day reference window. If an unambiguous window cannot be derived for some participants, exclude them transparently from duration analysis rather than inventing a hidden rule.

## 7. Dimension-specific implementation notes

### 7.1 Placement
Run eye–chest and eye–wrist separately on their maximum pairwise supports. Do not intersect them with the third position. The main Fig. 1 placement panel may combine the resulting summaries visually, but preserve their separate sample-flow metadata.

### 7.2 Optical
Use the near-corneal device only. Apply the same one-channel operator to `MEDI` and `LIGHT` with the same parameters when the target representation is operationally computable from either scalar channel. Mark intrinsically multi-optical representations unavailable.

Do not fit a conversion from `LIGHT` to `MEDI` in RQ1.

### 7.3 Temporal resolution
Use deterministic bins anchored consistently to clock time. Construct all levels from the same cleaned 10-s eye `MEDI` reference. Recompute metrics from the binned series.

Main-text Fig. 1 uses 30 min versus 10 s; nevertheless compute and save all specified levels because they are needed later for RQ3.

### 7.4 Monitoring duration
Run after the valid-day audit. For each eligible participant and `d=1..6`, enumerate all subsets of the canonical seven-day window when possible. For the 52 daily metrics, average the selected daily metric values. For IS/IV, recompute from the selected multi-day time series.

Main-text Fig. 1 uses 1 d versus 7 d; compute and save all 1–6 d levels.

## 8. Fig. 1 rough draft
Generate both vector and raster versions if the local graphics stack supports them without extra setup:

```text
results/figures/Fig1_RQ1.pdf
results/figures/Fig1_RQ1.png
```

Do not spend time on journal-perfect styling yet. The purpose of the first draft is to make the empirical structure visible and expose analytical mistakes.

### Panel a selection
Select examples from the finite configuration/metric results by explicit rules, then inspect them visually:

- **low distortion**: among metrics with adequate sample size, low `A` and `|B|`;
- **positive directional**: `B > 0`, high `A`, and high `|B|/A`;
- **negative directional**: `B < 0`, high `A`, and high `|B|/A`;
- **bidirectional/cancellation**: high `A` but low `|B|/A`.

Avoid choosing four examples from exactly the same configuration if equally clear examples exist across dimensions. Record the selected metric/configuration IDs in a CSV so the selection is reproducible.

Suggested output:

`results/rq1/fig1_panel_a_examples.csv`

### Panels b–e
- x = `B_mean_signed`
- y = `A_mean_absolute`
- show `y = |x|` boundaries
- one point per metric
- color = published metric class
- 95% bootstrap intervals for x and y
- b = placement; c = optical; d = 30 min vs 10 s; e = 1 d vs 7 d
- label only a small number of algorithmically extreme/representative metrics.

If error bars make the rough overview unreadable, retain them in the data and either lighten them substantially or produce a second diagnostic version without labels. Do not solve clutter by deleting uncertainty outputs.

## 9. Completion report
At the end of the run, write:

`results/rq1/RQ1_RUN_REPORT.md`

Keep it factual and short. Include:
- scripts run,
- package/session information path,
- sample sizes by dimension,
- unavailable metrics/configurations,
- whether all diagnostics passed,
- paths to canonical tables and Fig. 1,
- any unresolved issue that materially affects interpretation.

Do not write Discussion text and do not begin RQ2/RQ3.

## 10. Stop conditions
Stop and report rather than silently proceeding if:
- upstream numerical reproduction fails materially;
- a metric definition cannot be reconciled with the upstream implementation;
- the duration reference window is materially ambiguous after inspecting the actual protocol-cleaned days;
- standardized distortion requires an arbitrary denominator because the reference metric has zero/near-zero dispersion;
- a proposed workaround changes the scientific estimand.

Otherwise, make ordinary engineering decisions directly and continue execution.
