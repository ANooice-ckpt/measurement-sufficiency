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

The active ordered design is read from `scripts/utils/analysis_design.R`:
primary temporal states are 10, 20, 30, 40, 60 and 120 s; monitoring duration
is 1–6 complete analysis days. Five minutes is a core sensitivity state only and
does not enter the primary figures.

## Frozen inputs and output locations

The current plot scripts read only frozen RQ outputs. Fig. 1 additionally derives a
lightweight Spearman rank-preservation diagnostic directly from the frozen RQ1
pairwise artifact and writes it as a separate figure-level audit CSV; it does not
change the canonical RQ1 estimand or any downstream sufficiency calculation. Main
figures are written as PNG files to the centralized `results/figures` directory.
RQ-specific artifact manifests remain under `results/rq1`, `results/rq2`, and
`results/rq3`.

Every RQ artifact version incorporates the current analysis-design identifier.
Canonical plotting wrappers check or reconstruct ordered-axis levels from the
same frozen design so a historical hard-coded temporal lattice cannot silently
survive a design change.

## Figure 1 — RQ1 configuration response

`11_plot_fig1.R` presents:

- Fig. 1a: absolute standardized distortion versus Spearman rank loss across
  placement, optical, temporal-resolution and monitoring-duration contrasts;
  ordinary rank preservation is not assigned to circular-time representations;
- Fig. 1b: target-aligned signed-versus-absolute distortion geometry for
  placement and optical representation on a shared linear A/B scale;
- Fig. 1c: the distribution of each metric's adjacent local-response share over
  the frozen temporal transitions and 1–6 d duration transitions.

The complete metric-by-pair atlas, pairwise distributions and availability atlas
are supplementary. Temporal transition ordering is generated from the frozen
analysis design rather than written manually into the plot source.

## Figures 2–3 — RQ2 conditionality and interactions

`13_plot_rq2.R` uses transition-local exposure-state bins frozen by RQ2.

- Fig. 2a shows conditional distortion magnitude across exposure state with
  linear within-dimension y scales;
- Fig. 2b shows standardized joint-model coefficients on one shared linear axis
  for a fixed, source-defined set of representatives spanning external
  opportunity, micro-environment, behaviour and exposure state. The selection
  is not data-driven; the complete layered coefficient table remains in the RQ2
  outputs. Only the central 99% of raw |beta| points are displayed, while the
  summaries use all estimates for the displayed terms;
- Fig. 2c shows the High-minus-Low change in distortion direction;
- Fig. 3 reports signed `R`, magnitude `Q`, and localized strongest
  cross-dimensional interactions.

The primary temporal interaction lattice is the same 10–120 s design used by
RQ1. Duration enters multidimensional stability directly in RQ3.

## Figures 4–5 — RQ3 sufficiency and joint minimum-burden projections

`15_plot_rq3.R` presents:

- Fig. 4a: tolerance-dependent minimum sufficient requirement rank for the two
  ordered dimensions, with a thin strip showing the fraction of evaluable metrics
  for which a sufficient resolved state is observed within the candidate domain;
- Fig. 4b: residual observed instability `R_obs` by requirement rank;
- Fig. 4c: target-aligned placement/optical substitutability as tolerance relaxes;
- Fig. 5a: overall Pareto occupancy across the temporal-resolution × duration
  grid inside the observed sufficient region;
- Fig. 5b: representation-class deviation from the overall Pareto occupancy.

Fig. 4a/c use one shared `log1p(epsilon)` display scale with explicit original-
epsilon tick labels. Fig. 4b remains linear. Fig. 5 uses tiles rather than the
previous saturated bubble encoding.

The conceptual object in Fig. 5 is a minimum-sufficient burden frontier within
the frozen candidate domain. It is not an unconstrained accuracy-versus-burden
optimum. The duration domain is 1–6 complete analysis days and the temporal main
domain is 10–120 s.

## Legacy artifacts

Files under `results/legacy/pre_refactor` are retained for audit only and are
not valid inputs to current plotting scripts. Compatibility entrypoints from the
retired context-specific graph are audit/migration aids only.
