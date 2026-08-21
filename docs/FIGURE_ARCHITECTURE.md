# Current figure architecture

This document describes the plotting contract for the current frozen core and
RQ analysis chain. Plot scripts only reorganize written summaries; they do not
refit models, recompute bootstrap estimates, or recreate retired artifacts.

## Scientific visual grammar

The figures follow the object:

```text
configuration pair → observed exposure-process change → target-representation geometry
```

The 54 published exposure metrics remain the analytical units. Metric classes
are used for descriptive color and panel ordering only. Unavailable optical or
support-specific representations remain unavailable and are not plotted as
zero distortion.

The high-information states are empirical scale anchors. They are not treated
as biological truth, and no universal sufficiency threshold or placement/optical
burden order is implied.

## Frozen inputs and output locations

The current plot scripts read only these RQ outputs:

```text
scripts/10_rq1_analysis.R → results/rq1/rq1_pairwise_summary.csv
                          → results/rq1/rq1_metric_availability.csv
                          → results/rq1/rq1_local_transition_summary.csv

scripts/12_rq2_analysis.R → results/rq2/rq2_conditional_geometry.csv
                          → results/rq2/rq2_gamma_summary.csv
                          → results/rq2/rq2_model_performance.csv

scripts/14_rq3_analysis.R → results/rq3/rq3_sufficiency_long.csv
                          → results/rq3/rq3_convergence_profile.csv
                          → results/rq3/rq3_unordered_coverage_curves.csv
                          → results/rq3/rq3_pareto_frontiers.csv
                          → results/rq3/rq3_pareto_frequency.csv
```

Figures are written to `results/rq1/figures`, `results/rq2/figures`, and
`results/rq3/figures`. Each directory contains a
`figure_artifact_manifest.csv` with the core, upstream RQ, and figure input
versions.

Every plot script checks the current core version
`v3_sparse_sampling_complete_days` and the expected RQ analysis version before
plotting. The old `data/derived/rq*` paths are not valid plot inputs.

## Figure 1 — RQ1 pairwise representation atlas

`11_plot_fig1.R` presents:

- a–d: empirical quantile intervals of pairwise standardized distortion for
  placement, optical, temporal, and duration comparisons;
- e: the complete metric-by-pair atlas, with bubble area for `A = mean |z|`,
  fill for `B / A`, and explicit unavailable cells;
- f–g: placement and optical A/B geometry as unordered pairwise facets;
- h–i: A/B geometry for the explicitly flagged local temporal and duration
  transitions. All temporal pairwise comparisons are not forced into one path,
  and nested duration windows are not treated as a single artificial sequence.

Availability is exported separately as
`FigS_RQ1_availability_atlas`.

## Figures 2–3 — RQ2 conditional and interaction geometry

`13_plot_rq2.R` uses transition-local exposure-state bins frozen by RQ2. Fig. 2
shows conditional `A` and `B` across the observed pair and state-bin lattice.
Fig. 3 shows interaction gamma as signed `R` and absolute `Q` for the defined
placement/optical and adjacent temporal interaction lattices. Duration enters
RQ2 only through the current local transition features; no seven-day departure
predictor or retired context hypercube is plotted.

Model cross-validation output is supplementary and is shown only when the
current RQ2 model artifact contains rows.

## Figures 4–5 — RQ3 tolerance and Pareto projections

`15_plot_rq3.R` presents:

- Fig. 4a: metric-level tolerance projection of observed sufficient states;
- Fig. 4b: local adjacent-transition stability `G` and boundary proximity;
- Fig. 4c: unordered placement/optical substitutability coverage by pair;
- Fig. 5a: facet-specific Pareto occupancy over temporal resolution and
  monitoring duration;
- Fig. 5b: fraction of metrics ever Pareto across the same ordered dimensions.

The duration domain is the actual 1–6 complete-analysis-day domain. There is no
protocol seven-day reference configuration in the current figure logic.
Pareto dominance is applied only to temporal resolution and duration, inside
fixed placement × optical facets.

## Legacy artifacts

Files under `results/legacy/pre_refactor` are retained for audit only and are
not valid inputs to current plotting scripts. The compatibility entrypoints
`10b`, `10c`, and `12b` are inert migration notices; they must not be used to
recreate the retired context-specific artifact graph.
