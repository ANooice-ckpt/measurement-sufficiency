# Manuscript figure architecture

This document fixes the plotting logic for the high-dimensional measurement-sufficiency analysis. It is a visualization specification only: figures may reorganize frozen outputs, but they must not redefine scientific estimands or introduce new metric-class averages upstream of metric-level estimates.

## 1. Scientific object and dimensional structure

The primary empirical object is the target-representation-specific distortion distribution

```text
D_k,c(e)
```

for target representation `k` and observed measurement configuration `c`, relative to the high-information empirical reference. The observed configuration space contains four measurement dimensions:

```text
placement × optical representation × temporal sampling × monitoring duration
```

RQ2 adds conditional axes:

```text
reference exposure state
real-world context: civil photoperiod × diary environment × diary activity
external environment: radiation / cloud / solar geometry
```

Cross-dimensional interaction adds a second-order dimension-pair axis, and RQ3 adds the acceptable-distortion tolerance `epsilon` plus Pareto status. The resulting object is too large for one literal hypercube rendering. Main figures therefore show mutually interpretable projections, while exhaustive slices are exported as supplementary atlases.

A plotting projection may expose only axes for which the upstream analysis defines an estimand. In particular, the real-world context axes are visualized in RQ2 because `10b/12b` estimate context-specific geometry and paired binary contrasts. They are not crossed into RQ3 sufficiency/Pareto figures unless a future analysis explicitly defines context-specific sufficiency estimands.

## 2. Invariants across figures

### Metric is the inferential unit

All 54 target representations remain visible whenever the figure is intended to support representation-specific claims. Metric classes are visual grouping variables only.

### Frozen metric order

`scripts/utils/figure_atlas.R::ms_metric_order()` defines one row order for the manuscript figures. Metrics are grouped by class; within each class, the display score is the median of dimension-specific median RQ1 absolute distortions. This two-stage summary gives placement, optical, temporal, and duration equal structural weight despite different numbers of sampled configurations. The same order is reused across RQ1, RQ2, and RQ3 so a reader can trace one representation vertically across figures.

This is a display order only. It is never used in estimation, testing, model fitting, sufficiency classification, or Pareto dominance.

### Orthogonal visual channels

Whenever possible:

| Visual channel | Scientific role |
| --- | --- |
| y-position | target representation |
| x-position | configuration, context, or tolerance |
| facet | measurement dimension / anchor configuration / dimension pair |
| point area | non-negative magnitude (`A`, `Q`) |
| diverging fill | directionality (`B/A`, `R/Q`) |
| continuous x-position in RQ3 | entry tolerance `epsilon_entry=A(c)` |
| grey cell | operator-valid support exists |
| white cell | context restriction is structurally invalid for that operator |
| grey x | operator-valid cell exists conceptually but no finite estimate is available |

Magnitude and direction are intentionally separated. `B/A` and `R/Q` are bounded display ratios and do not replace the underlying signed and absolute estimands.

## 3. Fig. 1 — RQ1 distortion structure

Fig. 1 follows three levels of description:

1. **Empirical distributions**: pooled `D(e)` views preserve the full distributional object and show central spread rather than only A/B summaries.
2. **All-representation distortion atlas**: all target representations × observed non-reference configurations. Bubble area is `A=E|e|`; fill is `B/A`.
3. **A-B geometry**: dimension-specific structural projection showing cancellation versus directional distortion, with trajectories for ordered temporal-resolution and duration dimensions.

Representation-retention diagnostics (`Lin CCC`, `Spearman rho`) are exported as a separate supplementary atlas. They are not encoded as a third within-cell channel in the main distortion atlas.

## 4. Fig. 2 — RQ2 conditionality

The main figure deliberately contains three distinct conditionality objects.

### Reference exposure state

Representative `D(e | X)` distributions and all-metric conditional A-B geometry for predeclared anchor configurations show how distortion changes across frozen Low / Middle / High reference-exposure states.

### Real-world context

The main context atlas uses the same RQ2 anchor configurations and all operator-valid metrics. Context columns are:

```text
Day | Night | Indoor | Outdoor | Home | Working | Vehicle | Outdoors
```

Bubble area is conditional `A`; fill is conditional `B/A`. The background explicitly distinguishes operator-valid from operator-invalid metric/context combinations.

A second, aligned atlas reports the paired smallest-unit contextual contrasts that are scientifically defined in `12b_rq2_context_analysis.R`:

```text
Day -> Night: delta A, delta B
Indoor -> Outdoor: delta A, delta B
```

Because `delta A` and `delta B` share standardized-distortion units, they use the same diverging fill scale. A small black dot marks a participant-cluster/site-stratified bootstrap interval excluding zero. Activity remains state-specific only; no arbitrary activity pairwise contrasts are introduced by plotting.

### External context

Grouped-participant CV versus leave-site-out R-squared retains individual metric/model results, with model-family medians and interquartile ranges superimposed. This separates within-sample predictability from geographic transportability.

### Exhaustive context hypercube

The main figure uses anchor configurations to remain readable. Full context slices are exported separately for each measurement dimension:

```text
FigS_RQ2_context_placement
FigS_RQ2_context_optical
FigS_RQ2_context_temporal
FigS_RQ2_context_duration
```

Each supplementary atlas contains all observed configurations in that dimension × all eight real-world context states × all operator-valid target representations.

## 5. Fig. 3 — RQ2 cross-dimensional separability

The main result is the full metric × joint-configuration interaction atlas for the estimable primary dimension pairs:

```text
placement × optical
placement × temporal
optical × temporal
```

Bubble area is `Q=E|gamma|`; fill is `R/Q`. The interaction atlas is complemented by:

- representative empirical `D(gamma)` distributions;
- the full R-Q geometry;
- dimension-pair medians/IQRs for Q plus median absolute R;
- the strongest observed non-separable marginal-shift profiles.

Definition schematics and the interaction-scope table do not occupy main-result panel area; scope remains encoded in the frozen analysis outputs and Methods/Supplementary material.

## 6. Fig. 4 — RQ3 single-dimension sufficiency

Fig. 4 is a 2 × 2 metric-level decision atlas.

### Ordered dimensions

Temporal resolution and monitoring duration are rendered as horizontal requirement strips over `epsilon`. Colour gives the least-demanding sufficient observed level. Hollow markers identify empirical epsilon states for which the sufficient set is explicitly non-threshold-like, so a scalar minimum requirement is not interpreted there.

### Unordered dimensions

Optical proxy and placement are represented by their metric-specific entry tolerance

```text
epsilon_entry(c) = A(c)
```

on the x-axis. Fill retains `B/A`. Placement uses paired chest/wrist points rather than imposing an artificial total order.

Class/all-metric coverage curves remain available as a supplementary projection.

## 7. Fig. 5 — RQ3 multidimensional sufficiency

Pareto dominance is applied only to temporal resolution and monitoring duration within fixed placement × optical facets, matching the frozen RQ3 estimand.

The figure contains:

1. a global empirical configuration map: point size = fraction of available metrics for which a configuration is ever Pareto-efficient; fill = median entry tolerance;
2. four representative metric-specific entry-tolerance landscapes with Pareto outlines;
3. a class-specific recurrence matrix for the globally most recurrent observed configurations.

This separates three questions that should not be collapsed:

```text
How much distortion tolerance is needed for a configuration to enter sufficiency?
When is that configuration Pareto-efficient?
Is its efficiency general or representation-class-specific?
```

## 8. What must remain separate

The following pairs are intentionally not encoded as interchangeable quantities:

- `A` versus `B`: magnitude versus signed direction;
- context-specific A/B versus paired context shifts: conditional state geometry versus within-unit contrast;
- grouped CV versus leave-site-out performance: predictability versus transportability;
- metric-level estimates versus metric-class summaries;
- unordered placement/optical alternatives versus ordered temporal/duration requirements;
- sufficiency entry tolerance versus Pareto recurrence.

This separation is the primary safeguard against information-dense figures becoming semantically overloaded.
