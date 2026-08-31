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

## Plot-script architecture

Each main-text figure has one canonical plotting entrypoint:

- `scripts/11_plot_fig1.R` → Fig. 1
- `scripts/13a_plot_fig2.R` → Fig. 2
- `scripts/13b_plot_fig3.R` → Fig. 3
- `scripts/15a_plot_fig4.R` → Fig. 4
- `scripts/15b_plot_fig5.R` → Fig. 5

All supplementary drawing code is centralized in:

- `scripts/16_plot_supplementary.R` → every `FigS_*` output from RQ1–RQ3

The supplementary script sources the relevant main-figure entrypoints inside
RQ-specific `local({ ... })` environments to reconstruct their frozen display
objects. `scripts/utils/plot_contracts.R` detects this prep-only sourcing path
and suppresses main-figure saves and main-only manifest writes while the sourced
script is on the call stack. Therefore running `16_plot_supplementary.R` does
not regenerate or overwrite Fig. 1–5; only its `FigS_*` blocks produce figures.

The retired combined plotting entrypoints `13_plot_rq2.R` and
`15_plot_rq3.R` are no longer part of the repository or canonical run graph.

## Main-figure visual composition contract

Fig. 2 and Fig. 4 define the current manuscript house style. The final export
pass in `scripts/utils/figure_polish.R` is strictly visual: it receives already
constructed panel objects and may change only composition, spacing, typography,
legend spacing and export dimensions. It cannot alter data, statistics, scales,
metric definitions, availability, model results or scientific estimands.

Across Fig. 1–5:

- panel labels and titles are left aligned, bold and placed at one consistent
  visual hierarchy; explanatory subtitles use one smaller neutral-grey level;
- metric-class color remains defined only by `MS_METRIC_COLORS`; neutral raw
  observations, reference guides and overall summaries remain visually
  subordinate to the class-level foreground;
- linetype is used only where it carries a categorical distinction already
  defined by the figure (for example chest versus wrist), rather than as
  decoration;
- nested layouts must form an approximately rectangular outer footprint: a
  full-width panel above a two-panel row shares the same left/right outer edges,
  and a full-width lower panel shares the same outer edges as the row above;
- subplot spacing is controlled at the composition level instead of by adding
  arbitrary whitespace inside individual plotting panels;
- Fig. 1 is exported as one full-width panel above two aligned lower panels;
  Fig. 3 uses the same full-width-above-two structure; Fig. 4 retains that
  reference structure; Fig. 5 retains two upper panels above one full-width
  lower panel, with extra width assigned to the multi-facet upper-right panel;
- Fig. 2 retains its established asymmetric predictor-atlas/reference layout and
  acts as the second visual reference rather than being forced into a symmetric
  grid.

## Frozen inputs and output locations

The plot scripts read only frozen RQ outputs. Figure-level display summaries may
be derived from those frozen artifacts and written as audit CSVs, but plotting
does not change canonical RQ estimands or downstream sufficiency calculations.

`ms_plot_save()` centralizes all raster figure output in:

```text
results/figures/
```

PDF calls retained in older plotting blocks are no-ops; PNG is the canonical
figure artifact. Figure manifests are normalized out of legacy per-RQ figure
folders and written at the corresponding RQ root:

- `results/rq1/figure_artifact_manifest.csv`
- `results/rq2/figure_artifact_manifest.csv`
- `results/rq3/figure_artifact_manifest.csv`

The downstream runner clears `results/figures/` before plotting so obsolete
figure files cannot survive a complete downstream rerun.

Every RQ artifact version incorporates the current analysis-design identifier.
Plotting wrappers check or reconstruct ordered-axis levels from the same frozen
design so a historical hard-coded temporal lattice cannot silently survive a
design change.

## Figure 1 — RQ1 configuration response

`11_plot_fig1.R` presents:

- Fig. 1a: absolute standardized distortion versus Spearman rank loss across
  placement, optical, temporal-resolution and monitoring-duration contrasts;
  ordinary rank preservation is not assigned to circular-time representations;
- Fig. 1b: target-aligned distortion magnitude and directional coherence for
  placement and optical representation;
- Fig. 1c: the distribution of each metric's adjacent local-response share over
  the frozen temporal transitions and 1–6 d duration transitions.

The complete metric-by-pair atlas, pairwise distributions and availability atlas
are supplementary and are drawn only in `16_plot_supplementary.R`.

## Figure 2 — RQ2 contextual dependence

`13a_plot_fig2.R` presents the current contextual figure:

- Fig. 2a: a contextual predictor atlas spanning the prespecified external
  opportunity, micro-environment, behaviour and exposure-state predictors,
  including standardized joint-model coefficient distributions across estimable
  tasks;
- Fig. 2b: conditional distortion geometry across transition-local exposure-state
  tertiles, retaining both magnitude `A` and directional coherence `B/A`;
- Fig. 2c: participant-grouped out-of-sample contextual predictability from the
  joint model, together with the fraction of metrics having positive held-out
  CV R².

The complete conditional geometry atlas, transition state-spread diagnostic and
incremental grouped-CV information diagnostic are supplementary.

## Figure 3 — RQ2 cross-dimensional non-additivity

`13b_plot_fig3.R` presents the active non-additivity composition:

- Fig. 3a: class-level distributions of metric-level non-additivity magnitude,
  using the display projection `Q_mp = median_t(Q_mpt)`;
- Fig. 3b: the ordered-transition backbone with overall and metric-class
  median/IQR overlays, retaining transition-level `Q = mean(|gamma|)`;
- Fig. 3c: the distribution of directional coherence
  `C = median_t(R_mpt / Q_mpt)` across dimension pairs.

The complete transition-level gamma atlas and model-validation diagnostics are
supplementary. Duration does not enter the primary RQ2 gamma interaction set;
it enters multidimensional stability directly in RQ3.

## Figure 4 — RQ3 single-dimension sufficiency

`15a_plot_fig4.R` presents:

- Fig. 4a: tolerance-dependent 100% stacked distributions of the minimum
  sufficient measurement state for the overall metric set and each metric
  class; non-threshold-like or otherwise unresolved states remain explicitly
  unresolved rather than being forced into a requirement rank;
- Fig. 4b: empirical residual instability `R_obs` across increasing ordered-axis
  measurement burden, with the unresolved upper boundary omitted;
- Fig. 4c: empirical placement/optical substitutability curves as tolerance
  relaxes, including the 50% substitutability entry point where observed.

The detailed adjacent-transition convergence and metric-level sufficiency
trajectories remain supplementary.

## Figure 5 — RQ3 joint temporal × duration sufficiency geometry

`15b_plot_fig5.R` presents the frozen 6 × 6 temporal-resolution × duration
candidate lattice:

- Fig. 5a: the joint entry-tolerance landscape based on metric-equal pooling of
  resolved `epsilon_entry`, with boundary-unresolved cells marked explicitly;
- Fig. 5b: Pareto occupancy at explicit tolerance slices using the frozen
  interval-level Pareto flags rather than a refitted optimization surface;
- Fig. 5c: metric-class sufficient-region geometry at a shared tolerance,
  showing the fraction of class metrics sufficient in each joint state.

The conceptual object in Fig. 5 is a minimum-sufficient burden frontier within
the frozen candidate domain. It is not an unconstrained accuracy-versus-burden
optimum. The duration domain is 1–6 complete analysis days and the temporal main
domain is 10–120 s.

## Canonical downstream execution order

`run_downstream_server.sh` executes the figure layer in this order:

```text
Fig. 1 from frozen RQ1
→ RQ2 analysis
→ Fig. 2
→ Fig. 3
→ RQ3 analysis
→ Fig. 4
→ Fig. 5
→ all supplementary figures and complete RQ-specific figure manifests
```

`run_full_server.sh` remains the full raw-data-to-results entrypoint and delegates
the downstream portion to `run_downstream_server.sh`.

## Legacy artifacts

Files under `results/legacy/pre_refactor` are retained for audit only and are
not valid inputs to current plotting scripts. Compatibility entrypoints from the
retired context-specific graph are audit/migration aids only.