# Current figure architecture

This document describes the plotting contract for the current frozen core and
RQ analysis chain. Plot scripts only reorganize written summaries; they do not
refit models, recompute bootstrap estimates, or recreate retired artifacts.

## Scientific visual grammar

The primary measurement object remains:

```text
configuration pair → observed exposure-process change → target-representation geometry
```

The RQ1 downstream-consequence extension adds one deliberately narrow link:

```text
frozen RQ1 representation distortion → day-level human-state association perturbation
```

The 54 published exposure metrics remain the analytical units. Metric classes
are used for descriptive color and panel ordering only. Unavailable optical or
support-specific representations remain unavailable and are not plotted as
zero distortion.

The high-information states are empirical scale anchors. They are not treated
as biological truth. In Fig. 2 specifically, eye/MEDI/10 s defines the observed
reference association landscape but is not a latent error-free exposure.

The active ordered design is read from `scripts/utils/analysis_design.R`:
primary temporal states are 10, 20, 30, 40, 60 and 120 s; monitoring duration
is 1–6 complete analysis days. Five minutes is a core sensitivity state only and
does not enter the primary figures.

## Plot-script architecture

Each main-text figure has one canonical numbered plotting entrypoint:

- `scripts/11_plot_fig1.R` → Fig. 1
- `scripts/11b_plot_fig2.R` → Fig. 2
- `scripts/13a_plot_fig3.R` → Fig. 3
- `scripts/13b_plot_fig4.R` → Fig. 4
- `scripts/15a_plot_fig5.R` → Fig. 5
- `scripts/15b_plot_fig6.R` → Fig. 6

Main-figure identity is defined once in `scripts/utils/figure_registry.R`. For
each figure the registry records its current manuscript/output ID, canonical
entrypoint and implementation source. RQ2/RQ3 implementations predate insertion
of the RQ1 inferential-preservation Fig. 2, so their internal figure IDs remain:

- `scripts/13a_plot_fig2.R`
- `scripts/13b_plot_fig3.R`
- `scripts/15a_plot_fig4.R`
- `scripts/15b_plot_fig5.R`

The numbered entrypoints do not duplicate this mapping; they request their
implementation from the registry. `scripts/utils/plot_contracts.R` converts an
ID emitted by a legacy implementation to the current public ID exactly once at
save/manifest-write time. The reverse current → legacy conversion is used only
to select the mature refinement/polish logic. These directions are deliberately
separate because a current ID can equal the legacy ID of another figure (for
example current `Fig3_RQ2` was also the pre-insertion ID of current Fig. 4).
Persisted manifests therefore contain current IDs and are never renumbered a
second time.

All active supplementary drawing code remains centralized in:

- `scripts/16_plot_supplementary.R` → `FigS_*` outputs from RQ1–RQ3

The historical RQ1 inference drawing block still present in that file reads only
the retired v1 artifact path `rq1_inferential_preservation.rds`. The current
v3 analysis writes `rq1_inferential_preservation_domains_anchor8.rds` and removes
both the v1 path and the intermediate sleep-only `rq1_inferential_preservation_anchor8.rds`,
so active inferential-preservation plotting belongs exclusively to `11b_plot_fig2.R`.

The supplementary script sources the mature RQ2/RQ3 implementations inside
RQ-specific local environments to reconstruct frozen display objects.
`scripts/utils/plot_contracts.R` detects this prep-only sourcing path from the
registry and suppresses main-figure saves and main-only manifest writes while
those sources are on the call stack. Therefore running
`16_plot_supplementary.R` does not regenerate or overwrite Fig. 1–6.

## Main-figure visual composition contract

Across Fig. 1–6:

- panel labels and titles are left aligned and bold with one consistent visual
  hierarchy; explanatory subtitles use a smaller neutral-grey level;
- metric-class color remains defined only by `MS_METRIC_COLORS`; neutral raw
  observations, reference guides and overall summaries remain visually
  subordinate to class-level structure;
- main panels should expose both a readable aggregate pattern and metric-level
  detail rather than collapsing the result to class means alone;
- linetype is used only where it carries a categorical distinction already
  defined by the figure, not as decoration;
- nested layouts should form an approximately rectangular outer footprint;
- subplot spacing is controlled at the composition level rather than by adding
  arbitrary whitespace inside individual plotting panels.

Fig. 1 retains its established preservation geometry. Fig. 2 intentionally uses
an asymmetric information-dense layout: a tall association landscape on the left
and two downstream-consequence panels on the right. Figs. 3–6 retain the mature
layouts of the pre-insertion RQ2/RQ3 figures through their historical refinement
and polish functions; numbering alone must not degrade those layouts.

## Frozen inputs and output locations

Plot scripts read only frozen RQ outputs. Figure-level display summaries may be
derived from those frozen artifacts and written as audit CSVs, but plotting does
not change canonical RQ estimands or downstream sufficiency calculations.

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
figure files cannot survive a complete downstream rerun. Existing audit/display
CSV names inside mature RQ2/RQ3 implementations retain their historical `fig2_`,
`fig3_`, `fig4_` and `fig5_` prefixes for output compatibility; those filenames
are implementation artifacts, not current manuscript figure identities.

Every RQ artifact version incorporates the current analysis-design identifier.
Ordered-axis levels are read or reconstructed from the same frozen design so a
historical hard-coded temporal lattice cannot silently survive a design change.

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
remain supplementary.

## Figure 2 — RQ1 downstream inferential preservation

`11b_plot_fig2.R` reads the frozen
`results/rq1/inference/rq1_inferential_preservation_domains_anchor8.rds` artifact.
It does not fit exposure–outcome models itself.

The analysis is intentionally restricted to the 52 participant-day exposure
representations, six day-level outcomes spanning **Sleep, Alertness and Affect**,
and eight single-axis contrasts against eye/MEDI/10 s. Duration, participant-level
IS/IV and placement × optical × cadence Cartesian combinations are not part of
this figure.

The six outcomes are sleep quality, awakenings, awake duration, daily KSS,
positive affect and negative affect. Sleep outcomes use exposure day D followed
by the next-morning diary. KSS and MoodZoom responses are harmonized to the
nominal 11/14/17/20 h EMA slots and summarized within day; these same-day
associations are descriptive day-level relationships rather than acute causal
response estimates.

Fig. 2 presents:

- Fig. 2a: the eye/MEDI/10-s reference association landscape across the 52 daily
  representations and all six outcomes, visibly grouped into the three human-state
  domains. Cell intensity is the magnitude of the within-participant association
  in reference-bootstrap uncertainty units; it is descriptive and is not a
  significance-screening heatmap;
- Fig. 2b: inferential degradation for each of the eight measurement contrasts,
  shown as horizontal metric–outcome point distributions with domain-level
  median and interquartile range. Placement, optical and temporal groups are
  separated visually; all eight contrasts retain the eye/MEDI/10-s reference;
- Fig. 2c: propagation from the already-frozen RQ1 representation distortion
  `A_mean_absolute` to downstream inferential deviation. Metric–outcome points
  remain visible, domain-specific binned median trajectories expose the overall
  tendency, and a Spearman association summarizes monotone propagation within
  each domain.

Fig. 2b's horizontal deviation axis and Fig. 2c's vertical deviation axis share
the same zero-preserving pseudo-log transform and limits. Fig. 2c also expands
small frozen A values using a pseudo-log x-axis. Ticks retain original units;
these are display transforms, not new inferential cutoffs. Reference-heatmap
labels are shortened only for display; full metric IDs, ordering, and all three
audit CSV contracts remain unchanged. The distortion–inference relationship is
descriptive and does not establish causal propagation.

The main x-axis in Fig. 2c must come from the frozen RQ1 summary. The distortion
recomputed on outcome-matched support is an audit quantity only and must never
silently replace it.

For linear metrics, inferential deviation is absolute paired coefficient
movement divided by reference-bootstrap SE. For circular-time metrics, the
sine/cosine coefficient pair is retained jointly and deviation is the
Mahalanobis norm using the reference-bootstrap covariance. Thus each
metric–outcome–contrast contributes one metric-level inferential-deviation
observation rather than two unrelated timing coefficients.

## Figure 3 — RQ2 contextual dependence

`13a_plot_fig3.R` is the canonical numbered entrypoint. It presents:

- Fig. 3a: a contextual predictor atlas spanning the prespecified external
  opportunity, micro-environment, behaviour and exposure-state predictors,
  including standardized joint-model coefficient distributions across estimable
  tasks;
- Fig. 3b: conditional distortion geometry across transition-local exposure-state
  tertiles, retaining both magnitude `A` and directional coherence `B/A`;
- Fig. 3c: participant-grouped out-of-sample contextual predictability from the
  joint model, together with the fraction of metrics having positive held-out
  CV R².

The complete conditional geometry atlas, transition state-spread diagnostic and
incremental grouped-CV information diagnostic remain supplementary.

## Figure 4 — RQ2 cross-dimensional non-additivity

`13b_plot_fig4.R` is the canonical numbered entrypoint. It presents:

- Fig. 4a: class-level distributions of metric-level non-additivity magnitude,
  using the display projection `Q_mp = median_t(Q_mpt)`;
- Fig. 4b: the ordered-transition backbone with overall and metric-class
  median/IQR overlays, retaining transition-level `Q = mean(|gamma|)`;
- Fig. 4c: the distribution of directional coherence
  `C = median_t(R_mpt / Q_mpt)` across dimension pairs.

The complete transition-level gamma atlas and model-validation diagnostics are
supplementary. Duration does not enter the primary RQ2 gamma interaction set;
it enters multidimensional stability directly in RQ3.

## Figure 5 — RQ3 single-dimension sufficiency

`15a_plot_fig5.R` is the canonical numbered entrypoint. It presents:

- Fig. 5a: tolerance-dependent 100% stacked distributions of the minimum
  sufficient measurement state for the overall metric set and each metric
  class; non-threshold-like or otherwise unresolved states remain explicitly
  unresolved rather than being forced into a requirement rank;
- Fig. 5b: empirical residual instability `R_obs` across increasing ordered-axis
  measurement burden, with the unresolved upper boundary omitted;
- Fig. 5c: empirical placement/optical substitutability curves as tolerance
  relaxes, including the 50% substitutability entry point where observed. Its
  vertical tolerance guides refer to the joint tolerance slices in Fig. 6.

The detailed adjacent-transition convergence and metric-level sufficiency
trajectories remain supplementary.

## Figure 6 — RQ3 joint temporal × duration sufficiency geometry

`15b_plot_fig6.R` is the canonical numbered entrypoint. It presents the frozen
6 × 6 temporal-resolution × duration candidate lattice:

- Fig. 6a: the joint entry-tolerance landscape based on metric-equal pooling of
  resolved `epsilon_entry`, with boundary-unresolved cells marked explicitly;
- Fig. 6b: Pareto occupancy at explicit tolerance slices using the frozen
  interval-level Pareto flags rather than a refitted optimization surface;
- Fig. 6c: metric-class sufficient-region geometry at a shared tolerance,
  showing the fraction of class metrics sufficient in each joint state.

The conceptual object in Fig. 6 is a minimum-sufficient burden frontier within
the frozen candidate domain. It is not an unconstrained accuracy-versus-burden
optimum. The duration domain is 1–6 complete analysis days and the temporal main
domain is 10–120 s.

## Canonical downstream execution order

`scripts/run_downstream_server.sh` executes the figure layer in this order:

```text
Fig. 1 from frozen RQ1
→ RQ1 inferential-preservation analysis
→ Fig. 2
→ RQ2 analysis
→ Fig. 3
→ Fig. 4
→ RQ3 analysis
→ Fig. 5
→ Fig. 6
→ all supplementary figures and RQ-specific figure manifests
```

`scripts/run_full_server.sh` remains the full raw-data-to-results entrypoint and
delegates the downstream portion to `scripts/run_downstream_server.sh`.

## Legacy artifacts

Files under `results/legacy/pre_refactor` are retained for audit only and are
not valid inputs to current plotting scripts. The old unversioned inference
artifact `results/rq1/inference/rq1_inferential_preservation.rds` and the
intermediate sleep-only `rq1_inferential_preservation_anchor8.rds` are retired.
Historical implementation filenames are internal compatibility details; the
numbered Fig. 1–6 graph and `figure_registry.R` are canonical.
