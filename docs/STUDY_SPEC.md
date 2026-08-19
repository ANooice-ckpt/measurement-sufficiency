# Frozen study specification

## 1. Scientific object and terminology
A measurement configuration is

`c = (p, o, r, d)`

for placement, optical representation/information, temporal sampling interval, and monitoring duration. For target exposure representation `M_k`, the scientific object is the empirical map

`c -> E_c -> M_k(E_c)`.

The analysis is **distribution first**. Full empirical distortion distributions are primary. Mean signed/absolute distortion, conditional models, interaction summaries, sufficient sets, and Pareto frontiers are projections of that object.

“Measurement sufficiency” is operational sufficiency relative to a target representation and a tolerance; it is not Fisher–Neyman statistical sufficiency. The high-information reference is a benchmark, not retinal/biological ground truth.

## 2. Target representations
Use the published Zauner/LightLogR system of 54 metrics unchanged:
- 52 participant-day representations;
- interdaily stability (IS) and intradaily variability (IV) as multiday participant representations;
- six published classes (duration, exposure history, level, spectrum, temporal dynamics, timing) for descriptive grouping only.

## 3. Reference configuration and observable domains
Conceptual reference:
- near-corneal / glasses placement;
- melanopic EDI (`MEDI`);
- harmonized 10-s source grid;
- protocol-anchored seven-day monitoring reference.

For each metric, define its empirically observable configuration domain `C_obs[k]`. A target/configuration that cannot be computed is **unavailable**; it is neither assigned extreme distortion nor classified insufficient.

Support is part of the estimand. Single-dimension main effects use the maximal common support required for that comparison. Stricter supports are used only for joint configurations that actually require them.

## 4. Temporal sampling configurations
The harmonized MeLiDos 10-s grid is the source observation schedule.

Primary intervals:
- 10 s reference;
- 20 s, 30 s, 60 s, 300 s, 900 s, 1800 s alternatives.

Reserve extraction:
- 120 s, 600 s, 3600 s.

15 s is excluded because it cannot be represented as a regular equal-spacing subset of a 10-s source schedule.

For every `r > 10 s`, construct the candidate as a deterministic, clock-anchored **systematic sparse subsample** of the 10-s grid. A retained candidate observation must have exactly the same timestamp and MEDI/LIGHT value as its corresponding source observation. Missing scheduled observations remain missing. Do not average observations within `r`-second bins, interpolate, or reconstruct hidden values.

This estimand represents a logger observing intermittently at a lower sampling frequency.

## 5. Protocol-anchored monitoring duration
MeLiDos `trial_times` (`datetime_trial_start`, `datetime_trial_end`) defines the intended participant-specific study interval. Site data are Monday-to-Monday in the standard protocol, which can span eight calendar dates while representing a seven-day study period.

For each comparison support:
1. restrict valid support dates to the participant's protocol interval;
2. define protocol calendar Days 1–7 from the local trial-start date and require all seven to be valid;
3. require those seven dates to be consecutive;
4. use them as the fixed seven-day reference;
5. any later eighth valid calendar date is recorded as an extra/return-day date and does not replace one of the selected seven days.

If the selected seven dates are not available/consecutive, the participant is ineligible for primary duration analysis on that support. Do not use Day 8 to repair a missing day and do not choose an arbitrary seven-of-eight subset.

For `d=1..6`, enumerate all contiguous windows within the selected seven dates. Window-selection variability remains part of the empirical distortion distribution.

For 52 daily metrics: calculate daily representations first, then aggregate within a window. Linear representations use arithmetic mean; circular clock-time representations use circular mean. IS/IV are rebuilt directly from the selected multiday hourly basis in `unit_context`.

## 6. RQ1 distortion
For smallest analysis unit `i`, metric `k`, candidate `c`, and comparison lattice/support group `g`:

`e[i,k,c] = delta_k(M_k(E_i,c), M_k(E_i,c0)) / s[k,0,g]`.

Linear metrics use ordinary signed difference. Circular-time metrics use the shortest signed circular difference. Primary standardizer `s` is the SD of reference representations on the same comparison lattice and remains fixed across candidate levels. Zero/degenerate reference dispersion gives undefined standardized distortion.

Primary distribution:
`D[k,c](e)`.

Primary summaries:
- `A[k,c] = mean(|e|)` overall distortion magnitude;
- `B[k,c] = mean(e)` net directional distortion;
- required invariant `A >= |B|`.

For placement, optical, temporal and duration whenever supported by the resulting cohort, uncertainty is site-stratified participant-cluster bootstrap. Preserve every row belonging to a resampled participant. If a particular comparison lacks a non-degenerate cluster structure, report empirical distribution + point estimate without manufactured CI.

Sensitivity diagnostics include robust reference scaling (`IQR/1.349`) and participant-balanced A/B; neither replaces the primary estimand.

### Placement
Eye reference vs chest and wrist separately. Eye–chest and eye–wrist retain separate pairwise maximum-valid-support lattices. Never impose the unused third position.

### Optical
Eye MEDI reference vs synchronous eye photopic illuminance (`LIGHT`) using the same scalar operator/numerical parameters when mathematically defined. This is an operational proxy comparison, not physical equivalence of photopic and melanopic lux. MDER/nvRD require both channels and are unavailable for LIGHT-only candidate representation.

## 7. RQ2 conditionality and separability
RQ2 asks how stable/transportable the configuration–representation map is.

### Conditional distortion
The primary object remains smallest-unit RQ1 `e`. Reference exposure state is explanatory; external weather/solar variables are **personal-measurement-independent context**, not assumed known before study start.

Exposure-state Low/Middle/High partitions are frozen on unique reference exposure units within each `dimension × configuration × support` and then shared across all target metrics. The exposure condition must not be re-ranked separately for each metric.

Compare external-only, exposure-state-only, and joint mixed models for signed distortion and absolute distortion. Prediction uses grouped-participant CV and leave-site-out validation; held-out predictions use population fixed effects only. Grouped-CV folds are fixed once per metric×configuration group and reused for all model families/outcomes.

### Cross-dimensional separability
For observable joint cells:

`gamma = [Delta_a(u|v) - Delta_a(u|b0)] / s_k0`

(equivalent to the discrete second-order difference on a linear scale). Preserve `D(gamma)` and summarize
- `R = E(gamma)`;
- `Q = E(|gamma|)`;
- invariant `Q >= |R|`.

Primary estimated pairs are placement×optical, placement×temporal, optical×temporal. Duration-containing second-order contrasts are not part of the primary population interaction estimand; duration enters multidimensional sufficiency directly in RQ3.

## 8. RQ3 sufficiency
For target `k` and acceptable expected absolute standardized distortion `epsilon`:

`S_k(epsilon) = {c in C_obs[k] : A_k(c) <= epsilon}`.

Sufficiency is representation-, exposure-process-, tolerance-, and constraint-dependent. Do not impose a universal epsilon.

Temporal resolution and duration have justified requirement orderings. Report the least-demanding sufficient observed level; call it a minimum requirement only when the observed sufficient set is threshold-like/upward-closed. Do not force nonmonotone responses to be monotone.

Placement and optical states have no universal total burden order and remain incomparable facets.

### Multidimensional frontier
Use actual joint configuration values. Never synthesize joint distortion by adding single-dimension A values.

Use **facet-specific maximal supports**. Within each fixed placement×optical facet, compare temporal resolution and monitoring duration and apply Pareto dominance only over those ordered dimensions. The reference and candidate must share the same support. Do not force all facets onto `eye_chest_wrist_full` solely to make a common plotting cohort.

Define the entry tolerance

`epsilon_entry,k(c) = A_k(c)`,

the smallest tolerance at which observed configuration `c` enters `S_k(epsilon)`. Fig.5 uses this value to display the full tolerance family without selecting an arbitrary single epsilon.

## 9. Figure grammar
- **Fig.1:** `configuration -> D(e) -> (A,B)`, 2×4 matrix; top empirical distribution landscape, bottom A–B geometry for placement/optical/temporal/duration.
- **Fig.2:** `X -> D(e|X)` and conditional A–B; panel f maps grouped-participant CV R² to leave-site-out R² to distinguish predictability from transportability.
- **Fig.3:** `D(gamma) -> (R,Q)` cross-dimensional separability; final panel explicitly shows which dimension pairs are estimated vs outside primary scope.
- **Fig.4:** inverse single-dimension mapping `epsilon -> sufficient requirement`; do not merely redraw Fig.1 with a horizontal threshold.
- **Fig.5:** actual multidimensional configurations; temporal×duration axes within placement×optical facets, fill by `epsilon_entry`, emphasize Pareto-efficient configurations.
