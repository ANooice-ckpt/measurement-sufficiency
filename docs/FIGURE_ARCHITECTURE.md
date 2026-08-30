# Current figure architecture

This document describes the plotting contract for the current frozen core and
RQ analysis chain. Plot scripts consume frozen RQ artifacts and may derive
display-only summaries from already-frozen thresholds; they do not refit models,
reconstruct measurement windows, or redefine the canonical estimands.

## Scientific visual grammar

The 54 published exposure metrics remain the analytical units. Metric classes are
descriptive color/order groupings only. Unavailable representations remain
unavailable and are never coded as zero distortion.

The active ordered design is read from `scripts/utils/analysis_design.R`: primary
temporal states are 10, 20, 30, 40, 60 and 120 s; monitoring duration is 1–6
complete analysis days. Five minutes is a core sensitivity state only and does not
enter the primary figures. Main PNG figures are written to `results/figures`.

## Figure 1 — RQ1 configuration response

- Fig. 1a: typical absolute standardized distortion `A` versus Spearman rank loss
  `1-rho`; facet annotations give the descriptive metric-level association between
  these two quantities. Circular-time representations do not receive ordinary ranks.
- Fig. 1b: target-aligned signed-versus-absolute geometry for placement and optical
  representation; labels identify maximum `A` and maximum `|B|/A` metrics.
- Fig. 1c: adjacent local-response shares over temporal and duration transitions;
  the dotted line is the equal-share baseline.

## Figures 2–3 — RQ2 context dependence and non-separability

- Fig. 2a: conditional `A` trajectories across Low/Middle/High exposure state with
  a lower `B/A` coherence strip on the same state axis.
- Fig. 2b: standardized joint-model coefficients for a fixed representative set
  spanning external opportunity, micro-environment, behaviour and exposure state.
- Fig. 2c: participant-grouped held-out `R^2` from the joint contextual model;
  right annotations give the fraction of metrics with positive held-out `R^2`.
- Fig. 3a: signed cross-dimensional interaction `R`.
- Fig. 3b: interaction magnitude `Q`.
- Fig. 3c: strongest local transitions by `Q`, with fill encoding bounded signed
  coherence `R/Q`; values near zero indicate directional cancellation.

## Figures 4–5 — RQ3 sufficiency and joint design geometry

- Fig. 4a: tolerance-dependent minimum observed sufficient requirement rank for
  temporal resolution and monitoring duration, plus resolved-sufficient coverage.
- Fig. 4b: residual observed instability `R_obs` by burden rank; unresolved upper
  boundaries are omitted rather than assigned zero.
- Fig. 4c: target-aligned placement/optical substitutability; open points mark
  `epsilon50`, the first observed tolerance reaching at least 50% substitutability.
- Fig. 5a: joint temporal-resolution × duration entry-tolerance surface. Fill is the
  median `epsilon_entry = R_obs` among resolved joint-analysis target metrics.
- Fig. 5b: representation-class deviations from the pooled entry-tolerance surface,
  with compact class summaries in the strips.
- Fig. 5c: fixed-tolerance slices of confirmed joint sufficiency. Coverage uses a
  fixed all-target denominator within the joint-analysis metric universe, so
  missing/boundary-unresolved targets do not count as sufficient. The dark outline
  is a display-only coverage-efficient frontier, not the canonical Pareto estimand.

Fig. 4 uses tolerance as an axis; Fig. 5 uses tolerance only as fixed slices, so the
two figures answer different questions. Canonical RQ3 Pareto persistence/occupancy
remains available for audit and supplementary use, but is not the main Fig. 5 quantity.

## Cross-figure division of labor

- Fig. 1: what representation is lost when configuration changes?
- Fig. 2: how does distortion depend on exposure/context?
- Fig. 3: where do dimensions fail to separate additively?
- Fig. 4: at tolerance `epsilon`, what one-dimensional burden is observed sufficient?
- Fig. 5: what is the topology of joint temporal × duration sufficiency?

## Implementation invariants

Plot wrappers validate artifact/design identity before writing figures. Figure-level
inversions, coverage summaries and the Fig. 5c outline are display derivatives of
frozen `R_obs`/`epsilon_entry` thresholds and never feed back into RQ1–RQ3.

Files under `results/legacy/pre_refactor` are audit-only and are not valid inputs to
current plotting scripts.
