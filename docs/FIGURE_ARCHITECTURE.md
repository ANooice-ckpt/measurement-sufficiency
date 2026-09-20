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

Each main-text figure has exactly one canonical plotting script:

- `scripts/11_plot_fig1.R` → Fig. 1
- `scripts/11b_plot_fig2.R` → Fig. 2
- `scripts/13a_plot_fig3.R` → Fig. 3
- `scripts/13b_plot_fig4.R` → Fig. 4
- `scripts/15a_plot_fig5.R` → Fig. 5
- `scripts/15b_plot_fig6.R` → Fig. 6

Main-figure identity is defined once in `scripts/utils/figure_registry.R`. The
registry records the current manuscript/output ID and canonical script. RQ2/RQ3
plotting code still contains several pre-insertion figure IDs and component names
internally because the RQ1 inferential-preservation Fig. 2 was inserted after
those figures matured. `scripts/utils/plot_contracts.R` therefore retains an
explicit legacy-ID → current-ID mapping at the output boundary. This mapping is
for filenames, manifests and mature refinement/polish helpers only; there is no
second implementation script or wrapper layer.

Persisted manifests contain current IDs and are never renumbered a second time.
The previously duplicated pre-insertion scripts (`13a_plot_fig2.R`,
`13b_plot_fig3.R`, `15a_plot_fig4.R`, `15b_plot_fig5.R`) are retired. The former
`scripts/16_plot_supplementary.R` entrypoint is also retired and is not part of
the runner or plot-contract dispatch.

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
an asymmetric information-dense layout: a tall distortion-composition panel on the left
and two downstream-consequence panels on the right. Figs. 3–6 retain their mature
layouts and refinement/polish helpers; canonical file numbering is now identical
to manuscript numbering.

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
CSV names inside mature RQ2/RQ3 code retain their historical `fig2_`, `fig3_`,
`fig4_` and `fig5_` prefixes for output compatibility; those filenames are
internal artifacts, not current manuscript figure identities.

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
remain secondary outputs rather than main-figure panels.

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

- Fig. 2a: two aligned tracks show matched-support total RMS D_T and the
  within-person share f_W of squared candidate/reference distortion
  for each of the eight contrasts, on the exact outcome-matched fitting support.
  Its complement is the stable participant-offset share (including common bias).
  Metric-outcome points and median/IQR retain heterogeneity. Linear reference-SD
  and circular sin/cos design spaces are displayed separately;
- Fig. 2b: inferential degradation for each of the eight measurement contrasts,
  shown as horizontal metric–outcome point distributions with domain-level
  median and interquartile range. Placement, optical and temporal groups are
  separated visually; all eight contrasts retain the eye/MEDI/10-s reference;
- Fig. 2c: frozen coefficients and conditional participant-bootstrap intervals
  for the added within-person share predictor in a descriptive model of
  log1p(inferential deviation), adjusting for log1p(matched-support total RMS), log1p(frozen RQ1 A), contrast and
  outcome. Fits are separate for each domain and exposure geometry. Coefficients
  are expressed per primary-task SD of within share; missing/unreliable
  estimates are not represented as zero.

Fig. 2b retains its original pseudo-log deviation scale. The original reference
landscape and pooled A-displacement scatter move to supplementary figures,
together with within-contrast correlations stratified by exposure geometry.
The established tall-left/two-right composition is retained. Panel b's summary
lane offset is categorical spacing only. No plot refits the component model.
The left column is widened and the figure is 8.2 x 7.2 inches to accommodate
both tracks; total RMS uses a pseudo-log axis and geometry-specific ranges.
The conditional forest distinguishes unreliable-bootstrap estimates with crosses.

Frozen RQ1 A remains the upstream magnitude covariate and the pooled supplement's
x-axis. Matched-support squared-error components are newly declared explanatory
diagnostics, not a replacement for A or an additive decomposition of its absolute
loss. Within-person means interday variation of daily representations, not
intraday variation or an acute causal response.

For linear metrics, inferential deviation is absolute paired coefficient
movement divided by reference-bootstrap SE. For circular-time metrics, the
sine/cosine coefficient pair is retained jointly and deviation is the
Mahalanobis norm using the reference-bootstrap covariance. Thus each
metric–outcome–contrast contributes one metric-level inferential-deviation
observation rather than two unrelated timing coefficients.

## Figure 3 — RQ2 contextual dependence

`13a_plot_fig3.R` presents:

- Fig. 3a: a contextual predictor atlas spanning the prespecified external
  opportunity, micro-environment, behaviour and exposure-state predictors,
  including standardized joint-model coefficient distributions across estimable
  tasks;
- Fig. 3b: conditional distortion geometry across transition-local exposure-state
  tertiles, retaining both magnitude `A` and directional coherence `B/A`;
- Fig. 3c: cross-axis non-additivity, showing magnitude Q and coherence R/Q.
  The former contextual CV panel is retained in
  `FigS_RQ2_context_predictability.png`; Fig.4 owns the main predictive-value narrative.

The complete conditional geometry atlas, transition state-spread diagnostic and
incremental grouped-CV information diagnostic remain secondary outputs.

## Figure 4 — RQ2 context-conditioned measurement reliability

`12d_rq2_recovery.R --run` writes the versioned
`results/rq2/rq2_conditional_reliability.rds` manifest. `13b_plot_fig4.R` reads
this frozen artifact only. Its three panels present:

- Fig.4a: pooled lower/middle/higher contextual risk-group distortion ratios,
  shown as connected dots (dumbbells).
- Fig.4b: context's Brier-score value over configuration means and its increment
  beyond candidate measurements, with participant bootstrap intervals and repeated splits.
- Fig.4c: lower- versus higher-context observed exceedance probability scatterplots
  at all six frozen tolerances. Dots are metric–contrast pairs; dimension-specific
  diamonds and horizontal/vertical bars show marginal medians and IQRs. The
  identity line indicates equal risk. IQRs describe heterogeneity, not uncertainty.

Fig.3 is rendered at 7.4 × 6.88 inches (20% shorter than the earlier 8.6-inch version).

The 52 daily targets and eight anchor contrasts retain metric-specific supports.
There is no outcome matching. The figures do not claim universal sufficiency,
real-time adaptive acquisition, or an irreducible reconstruction error floor.
Supplementary outputs show site-mean and model-capacity controls and metric-level
heterogeneity. Cross-axis non-additivity remains in the renumbered Fig.3c supporting
panel and frozen gamma outputs. See `RQ2_CONDITIONAL_RELIABILITY.md` for the active
analysis and `RQ2_INFORMATION_EXPERIMENTS.md` for the experiment/failed-route log.

## Figure 5 — RQ3 single-dimension sufficiency

`15a_plot_fig5.R` presents:

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
trajectories remain secondary outputs.

## Figure 6 — RQ3 joint temporal × duration sufficiency geometry

`15b_plot_fig6.R` presents the frozen 6 × 6 temporal-resolution × duration
candidate lattice:

- Fig. 6a: the joint entry-tolerance landscape based on metric-equal pooling of
  resolved `epsilon_entry`, with boundary-unresolved cells marked explicitly;
- Fig. 6b: Pareto occupancy at explicit tolerance slices using the frozen
  interval-level Pareto flags rather than a refitted optimization surface;
- Fig. 6c: failure of single-axis sufficiency composition. A cell map at the
  existing epsilon=.25 slice shows joint failures / single-axis passes, alongside
  failure-rate profiles at .05/.10/.20/.25/.30/.50/1 by placement/optical facet.
  Both axes must have observed refinements. U denotes axis-unresolved and a dash
  denotes no single-axis passes; neither is a zero failure rate. Full failure
  intervals and pair-support counts are analysis outputs. The former class
  profiles and the failure map separated by facet are supplementary outputs.

Composition failure compares temporal-only and duration-only refinements from
the SAME joint starting configuration and standardizer with all higher joint
states. It does not concatenate Fig.5's separately defined single-axis summaries.
Original maximal pairwise supports remain in use; failure is a design-rule
disagreement in the observed comparison system, not proof of a statistical
interaction. Fig.3c's Q and R/Q remain a separate cross-axis description.

Display counts pool unique metric/configuration states across their respective
maximal supports within each placement/optical facet; support-specific counts
remain in the frozen summary for audit. They do not impose a common-support
intersection or treat metric classes as independent replicates.

The existing state-level composition CSV retains explicit R_T, R_D and R_J,
failure_interval_start/end/width with [start,end) bounds, worst_joint_refinement,
its cadence/duration and unit/participant counts. Temporal and duration maxima
also retain their worst configuration and corresponding counts, plus support
count ranges. These fields permit a representative case to be selected without
opening raw observations or creating another output file. Ties are deterministic;
pair-specific counts do not establish identical observation rows across pairs.

### Manuscript wording after execution

Results template (replace brackets only after inspecting the new outputs):
"Among [N] observed configurations for which temporal-only and duration-only
refinements both met the tolerance criterion, [n] failed the joint criterion
at epsilon = [value]. Failures occurred in [observed regions/facets], showing
that separately adequate temporal and duration choices did not necessarily
compose into a sufficient joint configuration." Report metric/configuration
counts and the evaluated tolerance range, not a population failure probability.
If no failures are observed, report that result and do not claim demonstrated
composition failure.

Discussion template when failures are observed: "Measurement sufficiency must
be assessed in the joint configuration space: passing separate refinement
checks does not guarantee preservation under combined refinement. This failure
can arise even through additive accumulation of discrepancies and therefore
does not, by itself, establish cross-axis interaction. The design implication
is to evaluate the selected cadence and monitoring duration together, within
the observed configuration domain and application-specific tolerance."

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
→ RQ2 conditional reliability
→ Fig. 4
→ RQ3 analysis
→ Fig. 5
→ Fig. 6
```

`scripts/run_full_server.sh` remains the full raw-data-to-results entrypoint and
delegates the downstream portion to `scripts/run_downstream_server.sh`.

## Legacy artifacts

Files under `results/legacy/pre_refactor` are retained for audit only and are
not valid inputs to current plotting scripts. The old unversioned inference
artifact `results/rq1/inference/rq1_inferential_preservation.rds` and the
intermediate sleep-only `rq1_inferential_preservation_anchor8.rds` are retired.
Pre-insertion figure IDs may still appear inside mature refinement helpers and
audit filenames, but there are no duplicate plotting entrypoint files.
