# Frozen study specification

## Status and purpose
This file is the human-readable scientific source of truth for the current analysis. It records the frozen framework for:

**How Much Measurement Is Enough? Measurement Sufficiency in Personal Light Exposure Assessment**

The present execution phase is RQ1. RQ2 and RQ3 are retained below only to preserve the logic of the full study.

## 1. Scientific object
A measurement configuration is represented as

`c = (p, o, r, d)`

where:
- `p` = measurement placement,
- `o` = optical representation/information,
- `r` = temporal resolution,
- `d` = monitoring duration.

For a target exposure representation `M_k`, the scientific object is the empirical mapping

`c -> E_c -> M_k(E_c)`

and the representation distortion induced by changing `c` relative to a high-information reference configuration. “Simpler” or “more detailed” measurement is an application-level direction of movement through this configuration space, not the scientific object itself.

The analysis is **distribution first**: the empirical distortion distribution is primary. Mean signed distortion, mean absolute distortion, interaction strength, sufficiency sets, and Pareto frontiers are secondary projections used for comparison or decision-making.

## 2. Target representations
Use the published Zauner et al. / LightLogR system of **54 personal light-exposure metrics** without creating a new taxonomy.

- 52 metrics are participant-day representations.
- Interdaily stability (IS) and intradaily variability (IV) are multi-day participant-level representations.
- The six published metric classes are: duration, exposure history, level, spectrum, temporal dynamics, and timing.

The **54 metrics are the inferential/analytical units**. Metric classes are descriptive grouping variables only; do not treat the metrics within a class as independent replicate observations for class-level hypothesis tests.

Metric definitions and LightLogR functions must follow the upstream reproduced pipeline. Model-response transformations used by Zauner et al. for their placement mixed models are not automatically part of our representation definition.

## 3. Data source and upstream boundary
Use the public MeLiDos harmonized field-study data.

The source layer for this project is the harmonized high-resolution time series distributed through `melidosData`, after the upstream import/harmonization steps. These are not raw device-export timestamps. The harmonized source explicitly retains missing intervals.

Use the package/repository versions recorded in `docs/UPSTREAM.md`.

Upstream preprocessing provides the reference for:
- wear/non-wear handling,
- implausible device-value filtering,
- sleep-state annotation,
- hour/day completeness rules,
- definitions of the 54 metrics.

The Zauner three-position concurrency rule belongs to the upstream placement-paper reproduction only. It is **not** a default sample restriction for this study.

## 4. Reference configuration and observable domains
The conceptual high-information reference configuration is:

- placement: near-corneal / glasses,
- optical quantity: melanopic EDI (`MEDI`),
- temporal resolution: harmonized 10-s grid,
- monitoring duration: 7 valid monitoring days.

Single-dimension RQ1 analyses vary one measurement dimension while holding the other dimensions at their reference state as far as the data permit.

For each target metric `k`, define a target-specific observable configuration domain `C_obs[k]`. A configuration for which the target representation cannot be computed is **unavailable**, not “high distortion” and not “insufficient”. Availability must therefore be recorded explicitly.

## 5. RQ1 — Representation distortion across measurement configurations

### 5.1 General workflow
For each measurement dimension:
1. construct the reference and alternative configurations on an explicitly defined common support;
2. compute the same target metric definition on the reference and candidate records;
3. calculate the smallest-unit signed representation distortion;
4. preserve the empirical distortion distribution;
5. derive summary projections and uncertainty only after the distribution exists.

### 5.2 Measurement placement
Reference: near-corneal / glasses.

Alternatives:
- chest,
- wrist.

Rules:
- eye–chest and eye–wrist analyses use their **pairwise maximum valid sample** separately;
- do not require the third placement;
- within each pair, retain only synchronized time support on which the two compared positions are jointly valid;
- apply the same upstream non-wear/range/completeness logic after constructing pairwise common support;
- only compare like-for-like ActLumus placement streams; do not introduce a device-type confound.

Because the protocol places devices near the bed during sleep, “placement” should be interpreted as the **protocol-defined measurement placement**, not as a literal anatomical placement throughout all 24 h.

### 5.3 Optical representation
Reference: near-corneal `MEDI`.

Alternative: synchronous near-corneal photopic illuminance (`LIGHT`).

Operational rule:
- for metrics defined by an operator acting on one scalar light channel, apply the same operator and the same numerical parameters to `MEDI` and `LIGHT`; the resulting difference is the empirical distortion induced by using photopic illuminance as the optical proxy;
- holding a numerical threshold fixed across `MEDI` and `LIGHT` is an operational proxy comparison and must not be described as physical equivalence of photopic and melanopic lux;
- metrics that intrinsically require information unavailable from a photopic-only channel are marked unavailable rather than assigned a numerical distortion. At minimum, any representation requiring both `MEDI` and `LIGHT` simultaneously (for example MDER or nvRD as defined upstream) must be checked explicitly in the availability table.

Do not invent a spectral conversion model in RQ1.

### 5.4 Temporal resolution
Reference: harmonized 10-s record.

Alternatives:
- 30 s,
- 1 min,
- 5 min,
- 15 min,
- 30 min.

Rules:
- construct every coarser series from the same cleaned 10-s reference record;
- use one explicit, deterministic time-binning rule across all sites and configurations;
- aggregate only observed values within bins and retain explicit missing support;
- ensure reference and candidate metrics are evaluated over the same admissible underlying support so that apparent fidelity is not driven by configuration-specific missingness;
- recompute all target metrics from each candidate time series rather than algebraically modifying the 10-s metric output.

### 5.5 Monitoring duration
Reference: a participant-level representation based on 7 valid near-corneal 10-s `MEDI` monitoring days.

Alternatives: 1–6 valid days.

For the 52 participant-day metrics, first compute the metric separately for each valid day, then define the participant-level `d`-day representation as the mean of the selected daily metric values:

`M_k^(d,S) = mean_{j in S} M_k(day_j)`

where `S` is a selected subset of `d` valid days.

The 7-day reference is:

`M_k^(7) = mean_{j=1..7} M_k(day_j)`

For IS and IV, recompute the metric directly from the corresponding selected multi-day time series; do not average daily values.

Day-window selection variability is part of the empirical distortion structure. With a seven-day reference, enumerate all `choose(7,d)` subsets for `d = 1..6` when the canonical seven-day reference window is unambiguous; this is preferred to Monte Carlo sampling because the finite subset space is small. If local data contain more than seven candidate valid days and the canonical protocol window cannot be resolved from the upstream metadata/cleaning logic, report that ambiguity before choosing an arbitrary window.

### 5.6 Smallest analysis units
For placement, optical, and temporal-resolution analyses:
- 52 daily metrics: participant-day;
- IS/IV: participant-level multi-day representation on the admissible records available for that comparison.

For monitoring duration:
- participant × selected day subset for the candidate representation, compared with the same participant’s fixed 7-day reference.

Do not average away the smallest-unit error before constructing the empirical distortion distribution.

## 6. Representation distortion definition
For analysis unit `i`, target metric `k`, configuration `c`, and its comparison lattice/support group `g`, define

`e[i,k,c] = delta_k(M_k(E_i,c), M_k(E_i,c0)) / s[k,0,g]`

where:
- `delta_k` is a signed difference on the metric’s natural representation scale;
- `s[k,0,g]` is the dispersion of the reference representation on the common comparison support and is fixed across all candidate levels within the same comparison lattice.

Use ordinary signed differences for linear metrics. For genuinely circular clock-time representations, use the shortest signed circular difference. The corresponding standardizer must be expressed on a compatible time scale; do not create midnight discontinuities.

For placement, eye–chest and eye–wrist are separate pairwise comparison lattices because their maximum valid supports may differ. For temporal resolution, all candidate levels share one reference support/standardizer within a metric. Optical has one alternative. Duration levels share the same eligible 7-day reference cohort/standardizer within a metric.

If the reference dispersion is zero or numerically degenerate, mark the standardized distortion undefined for that metric/comparison and report it; do not insert an arbitrary epsilon without inspection.

## 7. Primary RQ1 statistical objects
For every metric × configuration combination, preserve the full empirical distribution:

`D[k,c](e)`

Describe it using at least:
- median,
- interquartile range,
- empirical 2.5th and 97.5th percentiles,
- number of independent participants,
- number of smallest analysis units.

Then compute the two primary projections:

`A[k,c] = mean(|e|)`  — overall distortion magnitude

`B[k,c] = mean(e)`    — net directional distortion

By construction:

`A[k,c] >= |B[k,c]|`

This inequality is both a geometric property used in Fig. 1 and a required diagnostic.

Uncertainty for `A` and `B` is estimated with participant-cluster bootstrap stratified by study site. For duration, day-subset selection is already represented within participant; bootstrap resamples participants within site while preserving each selected participant’s subset structure.

Supplementary representation-retention diagnostics may include Lin’s CCC, Spearman correlation, and clearly meaningful classification agreement. They are secondary diagnostics and do not replace `D(e)`, `A`, or `B` as the RQ1 scientific object.

## 8. RQ1 Figure 1
Figure 1 is the first visualization of the RQ1 canonical distortion table.

Panel a: representative empirical `D(e)` distributions chosen by explicit rules after all results are computed, illustrating:
- low distortion,
- directional positive distortion,
- directional negative distortion,
- large bidirectional distortion with partial cancellation.

Panels b–e: `B` on the x-axis and `A` on the y-axis, with the boundaries `A = |B|`.

- b: placement — chest and wrist relative to eye;
- c: optical — photopic relative to eye `MEDI`;
- d: temporal resolution — main-text example uses 30 min relative to 10 s;
- e: monitoring duration — main-text example uses 1 d relative to 7 d.

Each point is one target metric. Color encodes the six published metric classes. Only representative/extreme metrics should be labelled. Use participant-cluster bootstrap 95% CIs for `A` and `B`. Other temporal-resolution and monitoring-duration levels belong in supplementary outputs rather than being crowded into the main figure.

Panel-a examples must be selected algorithmically from the computed results, not hand-picked to support a narrative. A practical selection rule is documented in `docs/RQ1_EXECUTION.md`.

## 9. RQ2 — retained for study continuity, not current execution
RQ2 asks whether the distortion distribution changes with the actual exposure process/external environment and whether the effects of configuration dimensions are separable.

The primary object remains the RQ1 smallest-unit distortion. Reference-exposure-state models are explanatory; external-only models assess pre-measurement predictability. Cross-dimensional dependence is represented by a second-order distortion contrast. Do not implement RQ2 during the current RQ1 phase.

## 10. RQ3 — retained for study continuity, not current execution
For a target metric `k` and acceptable mean absolute standardized distortion `epsilon`, define a target-specific sufficient set only within the empirically observable domain:

`S_k(epsilon) = {c in C_obs[k] : A_k(c) <= epsilon}`

Sufficiency is conditional on the target representation, the observed exposure-process distribution, the acceptable distortion requirement, and the available measurement constraints.

Do not assume monotonic configuration–distortion responses. For ordered dimensions, a “least-demanding sufficient observed level” can be reported; call it a minimum requirement only when the observed sufficient set is upward-closed. Placement has no artificial total burden ordering. Pareto dominance is defined only over dimensions with justified requirement orderings, with unordered states treated as incomparable/faceted.

Do not implement RQ3 during the current RQ1 phase.

## 11. Non-goals for the current phase
Do not:
- expand the 54-metric system;
- add machine-learning models;
- add new universal sufficiency thresholds;
- download ERA5 for RQ1;
- create a new taxonomy of exposure representations;
- add large sensitivity-analysis batteries;
- optimize figure aesthetics before the RQ1 numerical objects and diagnostics are validated.
