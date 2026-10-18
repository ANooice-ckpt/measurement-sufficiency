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

After the minimum-day IS correction, Fig. 1/Fig. 3 and the combined appendix
require RQ1 v7 rather than accepting stale duration results. Fig. 2 may retain
the unchanged v6 daily inference. Fig. 5/Fig. 6 require RQ3 v10 and task
projection v2. `14_rq3_analysis.R --refresh-frozen-availability` can refresh
the latter from compatible frozen v9/v10 pairs and state catalogues without
opening duration blocks: single-day IS is removed from pairs, retained as an
unavailable state, and all other R_obs/status values are checked unchanged.
The upstream RQ1 version is retained honestly; this is not a full RQ1/RQ2 rerun.

`14c_rq3_common_reference.R` separately freezes target-level loss to the same
placement/optical facet's 10-s/6-day reference and nominal-record budget
projections. These are descriptive projections of existing pair summaries;
the summaries cannot verify identical parent-window identities and weights
between candidate configurations. They neither replace R_obs nor establish
a controlled common-sample optimal design. Plot scripts must read these exports
if they display this new quantity, rather than deriving it inside a plot.

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

- panel labels and titles are bold and align to the left data-field / axis-frame
  edge, never to the outer axis-label gutter. Nested compositions pass their
  first aligned plot field as an explicit title anchor. There is no grey explanatory
  subtitle below individual panel titles; essential definitions belong in axes,
  legends or short dark figure notes;
- `figure_style.R` owns the shared font and physical sizes: 7.4-inch figure
  width, 8-pt panel titles, 7-pt facet/subpanel headings, 6.5-pt axes, 6.2-pt
  legends and 5.8-pt dense labels. `ms_panel_frame()` reserves a fixed 7-mm
  title band, independent of a panel's relative height;
- metric-class color remains defined only by `MS_METRIC_COLORS`; neutral raw
  observations, reference guides and overall summaries remain visually
  subordinate to class-level structure;
- main panels should expose both a readable aggregate pattern and metric-level
  detail rather than collapsing the result to class means alone;
- linetype is used only where it carries a categorical distinction already
  defined by the figure, not as decoration;
- nested layouts align actual plot fields before adding title bands: upper and
  lower edges within rows, and corresponding axis gutters within columns;
- subplot spacing is controlled at the composition level rather than by adding
  arbitrary whitespace inside individual plotting panels.

Fig. 1 retains its established preservation geometry. Fig. 2 places magnitude
(a) and inferential displacement (b) in an aligned top row. Pattern/signal (c)
and coefficient/signal (d) each span the full width, with aligned Sleep,
Alertness and Affect columns. Panel a retains total RMS only.
Figs. 3–5 retain their panel identities and existing refinement/polish helpers.
Fig. 6 uses four coordinated rows: joint designs, tolerance-dependent savings,
individual target constraints, and separate-axis composition failure. Canonical file
numbering is identical to manuscript numbering. Figure heights (Fig. 1–6) are
5.8, 7.6, 6.6, 7.0, 6.5 and 8.1 inches; height accommodates content density
without changing the series-wide physical font sizes.

## Frozen inputs and output locations

Plot scripts read only frozen RQ outputs. Figure-level display summaries may be
derived from those frozen artifacts and written as audit CSVs, but plotting does
not change canonical RQ estimands or downstream sufficiency calculations.

`ms_plot_save()` centralizes manuscript raster figure output in:

```text
results/figures/
```

Unnumbered historical supplementary layouts are diagnostic exports under
`results/diagnostics/figure_exploration/`; the six-item appendix is described below.

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

- Fig. 2a: matched-support total RMS D_T for each of the eight contrasts,
  on the exact outcome-matched fitting support. Metric-outcome points and
  median/IQR retain heterogeneity. Linear reference-SD and circular sin/cos
  design spaces are displayed separately, in the same order as panel c.
  The within-person fraction remains in the frozen analysis artifact but is
  not a second track in the main figure;
- Fig. 2b: inferential degradation for each of the eight measurement contrasts,
  shown as horizontal metric–outcome point distributions with domain-level
  median and interquartile range. Placement, optical and temporal groups are
  separated visually; all eight contrasts retain the eye/MEDI/10-s reference;
- Fig. 2c: within-person pattern loss versus the existing association-signal
  displacement, separately by domain and geometry. Linear loss is
  `1 - rho_within^2`; circular loss is the unexplained fraction of the demeaned
  reference sin/cos design after projection onto the candidate design. Signed
  linear correlation remains in the existing inference tables. Points show
  descriptive task-level relationships without another fitted model. The
  previous conditional composition model remains in the frozen analysis outputs.
- Fig. 2d: coefficient displacement versus association-signal displacement,
  with signal RMS normalized by within-outcome RMS. These are same-sample FE
  signals, not held-out predictions. Point size encodes reference within R²;
  the legend shows 0.1%, 1% and 5%, within the observed range. Point shapes
  identify placement, optical and temporal contrasts in both c and d.

The four panels respectively describe magnitude, coefficient sensitivity,
pattern/signal correspondence and coefficient/signal correspondence. All
finite task-level points in b/c/d are retained. Panels c/d share their full
vertical signal range and align the three domain columns. No significance
screen, new fit or smoothing is introduced. Total RMS and inferential
displacement retain pseudo-log axes; the linear and circular total-RMS facets
use their own ranges because their units differ. Panel b's summary lane offset
is categorical spacing only. The figure is 7.4 x 7.6 inches.

The participant-count continuation has been removed from Fig. 2. Its frozen
Monte Carlo artifact remains available but is no longer a plotting dependency;
`fig2_participant_count_display.csv`, if present from an earlier render, is
historical. The original reference landscape, pooled A-displacement scatter
and within-contrast correlation outputs retain their existing exports.

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

- Fig. 3a: horizontal bars show the overall predictor-strength pattern for
  all 20 predictors, grouped by contextual family. Bar length is the existing
  median absolute standardized coefficient across metric/dimension/response
  units. Two adjacent coefficient profiles separately retain the signed- and
  absolute-distortion responses: four small shapes show the median coefficient
  across metrics separately for each dimension. Each stem connects zero to
  that median; no interval or signed summary pooled across dimensions is drawn.
  Blue/orange points retain negative/positive signs even near zero; grey denotes
  numerical zero (absolute median coefficient <= 1e-12, not a substantive
  equivalence threshold). Response types are never pooled when assigning direction.
  All three fields use linear axes; ticks show 100 times the original coefficients.
  The strength bars keep a zero baseline. Both signed fields share symmetric
  limits containing every median, with no sign-based filtering.
  Day-to-day variability has a duration symbol only.
  These are coefficient-strength summaries, not additive explained-variance
  contributions. Metric-level values and signed coefficient quantiles remain
  in audit CSVs, including `fig3_predictor_direction_dimensions.csv` and
  `fig3_predictor_direction_pooled.csv` in raw coefficient units. The latter
  is audit-only: its pooled quantiles are not drawn and are not confidence intervals;
- Fig. 3b: conditional distortion geometry across transition-local exposure-state
  tertiles, retaining both magnitude `A` and directional coherence `B/A`, with
  an explicit low-origin / middle-circle / high-diamond key;
- Fig. 3c: cross-axis non-additivity, showing magnitude Q and coherence R/Q,
  with temporal intervals labelled directly in seconds rather than ranks.
  The former contextual CV panel is retained in
  `FigS_RQ2_context_predictability.png`; Fig.4 owns the main predictive-value narrative.

The complete conditional geometry atlas, transition state-spread diagnostic and
incremental grouped-CV information diagnostic remain secondary outputs.

## Figure 4 — RQ2 context-conditioned measurement reliability

`12d_rq2_recovery.R --run` writes the versioned
`results/rq2/rq2_conditional_reliability.rds` manifest. `13b_plot_fig4.R` reads
this frozen artifact only. Its three panels present:

- Fig.4a: pooled lower/middle/higher contextual risk-group distortion ratios,
  shown as connected dots (dumbbells), with explicit higher/lower group ratios
  on the right (1.50–1.88 in the current frozen artifact).
- Fig.4b: context's Brier-score value over configuration means and its increment
  beyond candidate measurements, with participant bootstrap intervals and repeated splits.
- Fig.4c: eight contrast-specific small multiples show predictive skill at
  all six frozen tolerances. The three bold profiles compare context only,
  measurement only and measurement plus context against the same training-mean
  baseline: `100 × (1 − mean model Brier loss / mean baseline Brier loss)`.
  Metric losses have equal weight before taking the ratio. The primary
  participant-grouped split is used. Open measurement markers remain visible
  around the smaller filled joint markers where their results nearly coincide.
  Fine grey trajectories retain all defined individual-metric joint skills.
  Higher values mean better held-out reliability prediction, not a percentage
  of recovered exposure. All facets share a signed pseudo-log skill axis
  (sigma = 5 percentage points) with a displayed window from −10% to 55%.
  This is coordinate clipping, not data filtering: 21 defined metric values
  below −10% (minimum −71.3%) are indicated by downward boundary triangles,
  with the total and minimum stated in the figure. Per-contrast/tolerance counts
  are exported in `fig4_skill_clipping_display.csv`. Tolerance is
  log-scaled. Zero-baseline task skills are undefined, with their number stated
  in the figure, and remain NA in the display data. Such tasks still contribute
  their losses to the pooled ratio. Panel b retains bootstrap intervals and
  repeated-split diagnostics; grey trajectories in c describe heterogeneity.

`results/rq2/fig4_skill_profile_display.csv` retains all 432 pooled summaries
(eight contrasts × six tolerances × three splits × three information states).
`fig4_skill_metric_display.csv` retains every individual metric-state-split
value, including undefined skills. These are display ratios of frozen losses;
no model is refitted. The main figure is 7.4 × 7.0 inches. Existing audit tables
and supplementary exports are retained without adding a new supplementary
solution to this redesign.

Fig.3 is rendered at 7.4 × 6.6 inches. Its compact strength bars expose the overall
pattern first; adjacent signed coefficient profiles recover direction and
dimension-specific detail without a numeric heatmap in panel a.
Fig.5 retains its established forms; long row names wrap, residual
instability facets have short titles, and legends have room for complete labels.

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
  measurement burden, labeled with temporal resolution / monitoring days and
  with the unresolved upper boundary omitted;
- Fig. 5c: empirical placement/optical substitutability curves as tolerance
  relaxes, including the 50% substitutability entry point where observed. Its
  vertical tolerance guides refer to the joint tolerance slices in Fig. 6.

The detailed adjacent-transition convergence and metric-level sufficiency
trajectories remain secondary outputs.

## Figure 6 — RQ3 tolerance-dependent joint design

`15b_plot_fig6.R` reads the frozen joint task projection, joint summary and
composition-failure summary. Four coordinated panels show joint choices,
tolerance trends, individual target constraints, and composition-rule failures.
Eye / Chest / Wrist columns align across the four rows. Fig.5 continues to own
single-axis requirements and substitutability. The figure is 7.4 × 8.1 inches.
The accepted figure has no bottom footnote block; the definitions below belong
in the manuscript caption. Axes, the shared target-class legend, direct numeric
annotations and the optical/count keys remain in the figure.

- Fig.6a: all seven MEDI target bundles at epsilon=.50 are shown on a cadence
  versus monitoring-days grid. The six observed cadence levels are categorical.
  Every frozen Pareto choice is retained (27 choices at 23 unique coordinates
  in the current artifact). Coincident designs share a marker and combined
  direct labels; All 54 is highlighted. Light leader lines associate labels with
  points and do not imply a connected frontier. No single winning design or
  universal order across placement/optical facets is introduced. Target counts
  are Level 9, Dynamics 7, Timing 23, Duration 12, History 2 and Spectrum 1;
  these are descriptive target sets, not inferential replicates.
- Fig.6b: all seven target bundles retain their continuous frozen tolerance
  intervals. Lines and bands show the minimum/maximum scheduled-record savings
  among actual Pareto alternatives, not uncertainty. Savings are
  `100 × [1 − (n_days / resolution_s) / (6 / 10)]`, relative to 10 s × 6 d.
  This is a descriptive projection of existing choices, not a new scalar burden
  ranking or sufficient-set calculation. All-54 profiles are emphasized; dots
  and percentages identify epsilon=.50, and downward triangles identify the
  first observed All-54 sufficient choice. No profile extends beyond its frozen
  domain or connects across a missing interval. Missing savings remain NA.
- Fig.6c: for each placement, the chosen All-54 design at epsilon=.50 is compared
  with its immediate one-day-shorter and next-coarser-cadence neighbours.
  Each row retains all 54 target-level residual-instability values from the
  frozen joint summary (486 points across nine states). Colour identifies metric
  class; jitter affects only the vertical display coordinate. Diamonds and
  three-decimal labels show the stored task maximum R_task; direct labels give
  the configuration and its maximum-attaining metric. The epsilon=.50 guide
  exposes why a neighbouring reduction fails. These are
  frozen decisions, not recomputed sufficiency tests. Maxima are checked against
  R_task only for input consistency. Named maxima are not an exhaustive list of
  failing targets and do not imply that removing those metrics alone would make
  the neighbour sufficient.
- Fig.6d: MEDI/LIGHT composition-failure profiles retain all seven evaluated
  tolerances in every placement (42 summary cells). Lines guide the eye between
  those evaluated slices. Numerator/denominator keys report exact counts at
  epsilon=.25. Hollow circles identify positive denominators below 10; the
  current chest 2/4 and wrist 0/5 cases are directly labelled. Crosses identify
  zero denominators; their failure rates remain NA and are never plotted as 0%.
  Counts refer to metric/configuration states, not participants, independent
  replicates or population probabilities. All observed rates remain visible.

Active display exports are `fig6_joint_design_states_display.csv`,
`fig6_joint_design_portfolio.csv`, `fig6_pareto_savings_by_tolerance.csv`,
`fig6_all_target_design_intervals.csv`, `fig6_constraint_states_display.csv`,
`fig6_constraint_targets_display.csv` and `fig6_composition_failure_profiles.csv`.
The constraint exports retain the original configuration keys and frozen values;
state roles 3 / 2 / 1 mean chosen / one day shorter / next coarser cadence.
`fig6b_eye_medi_epsilon_0.50_display_slice.csv` remains a reproducible audit export.
Earlier savings-matrix, day-block, design-table, composition-scatter and
limiting-target bar CSVs, if present, are historical and are not read by the
active figure. The canonical plot has no dependency on the review prototypes
and generates no additional supplementary layouts.

Composition failure compares temporal-only and duration-only refinements from
the SAME joint starting configuration and standardizer with all higher joint
states. It does not concatenate Fig.5's separately defined single-axis summaries.
Original maximal pairwise supports remain in use; failure is a design-rule
disagreement in the observed comparison system, not proof of a statistical
interaction. Fig.3c's Q and R/Q remain a separate cross-axis description.

The retained .50 audit-slice CSV also includes `R_task`, `limiting_targets`, their
metric classes and the union of limiting targets across each frozen frontier.
These identify the metrics attaining entry tolerance at those cells. Conditional
axis requirements report minimum sufficient days at each fixed interval and
the coarsest sufficient interval at each fixed duration. Differences compare
Temporal dynamics with Level (alternative goals), then All targets with Temporal
dynamics (an expanded target set). Positive extra days or a sampling-rate ratio
above one indicate the corresponding increased requirement. NA means no observed
sufficient choice on that slice (or no comparison bundle); the two conditional
minima must not be combined into an invented joint optimum. Full sufficient and
Pareto flags remain unchanged; no new bundle or sufficiency calculation is added.

The same CSV retains `shorter_*` and `coarser_*` neighbour coordinates, statuses,
frozen sufficient flags, R_task, blocking targets and figure keys. These identify
the largest residual-instability metrics at a rejected adjacent reduction; they
are not an exhaustive list of failing targets or a claim that removing the named
targets alone would enable that reduction. No step skips a missing lattice cell.

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
→ Configuration / participant-count analysis from frozen RQ1 (14b --compensation; separate output, not a Fig. 2 input)
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

## Frozen-decision display maintenance (2026-10-11)

RQ1 v6 excludes reserve/duplicate duration supports; RQ3 v9 exports unresolved
single-target sufficient/Pareto decisions as NA. Core v5 remains unchanged.
Fig. 5a queries the frozen single-dimension requirement table at its existing
breakpoints, and Fig. 5c reads frozen substitutability coverage. Neither panel
reimplements sufficiency. The original layout and display-table exports remain.

## Results appendix (2026-10-13)

`scripts/16_appendix_figures.R` is the independent appendix entrypoint. It
produces exactly six manuscript items, using frozen results only:

| Item | Evidence role | Frozen inputs |
| --- | --- | --- |
| Table S1 | Comparable support, units and availability | Core source/cohort/window audits, RQ1 availability, matched inference summaries |
| Fig. S1 | Standardization, participant weighting, and joint-decision scale dependence | RQ1 sensitivities plus RQ3 support-specific SD/IQR rescaling |
| Fig. S2 | Reference-strength dependence of preservation | Current three-domain, eight-anchor RQ1 inference artifact |
| Fig. S3 | Held-out contextual performance and group calibration | RQ2 layered grouped-CV performance and conditional-reliability risk profiles |
| Fig. S4 | Sampling phase at adjacent reduced joint designs | Both 20-s source-grid phases for the three prespecified MEDI neighbours |
| Fig. S5 | Target composition and participant sampling | Leave-one-target-out frontiers plus cluster-bootstrap All-54 stability at nine fixed states |

Fig. S2 is composed by `ms_fig2_appendix_strength()` in the canonical
`11b_plot_fig2.R`; the appendix imports that function without running the main
figure. Linear/circular geometries and the exact task-matched reference within
R² stay separate. Nested R² strata (all, ≥0.1%, ≥1%, ≥5%) are descriptive;
empty strata have zero task counts and undefined estimates. Medians and IQRs
describe task heterogeneity, not uncertainty intervals or independent replicates.

Fig. S3 retains negative held-out R². Calibration points are equal-target mean
predicted and observed probabilities within the three outer-training-defined
context-risk groups, for chest, LIGHT and 120-s contrasts at ε=.10/.50. These
are group calibration diagnostics, not individual probability-bin calibration
curves. The frozen export includes coverage and target counts. Grouped validation
supports new participants in the observed site mixture, not new study sites.

The focused phase experiment is prepared explicitly with
`Rscript scripts/14_rq3_analysis.R --appendix-phase`. It reconstructs the original
eligible supports, applies the alternate +10-s phase to the 20-s coarser neighbour
of each MEDI All-54 design at epsilon=.50, and recomputes all 54 targets and every
higher comparison at those three states. Both possible 20-s grid phases retain
exact source timestamps and values. Participant/day supports, all contiguous
windows and original support-specific SD anchors remain fixed. The chosen and
one-day-shorter 10-s configurations have no alternate grid phase. This is a
focused observation-operator sensitivity, not a complete phase-by-cadence or
participant-specific random-phase experiment. Source hashes, exact-row checks
and a primary-phase metric reproduction check are exported. Historical
state-coverage diagnostics remain CSV audit outputs, outside the numbered S4.

Before plotting, `Rscript scripts/14_rq3_analysis.R --appendix` explicitly
prepares `results/rq3/rq3_appendix_joint_target_sensitivity.rds` from frozen
`rq3_joint_summary.csv`. It reuses `rq3_task_projection()` for all 54 one-target
omissions under MEDI at ε=.50, retaining target-specific maximal supports and
NA boundary decisions. Fig. S5 reports the fraction of resolved omissions
changing sufficient/frontier membership, plus the largest target-specific
frontier changes. These are deterministic inventory perturbations, not
participant-bootstrap probabilities.

`Rscript scripts/14_rq3_analysis.R --appendix-robustness` reconstructs focused
10/20-s MEDI windows from the durable daily metric cube and exact hourly bases;
it does not rebuild Core or change canonical RQ3 results. Reconstructed pair
counts and A values must reproduce every corresponding frozen pair before
bootstrap. It retains participant-level sums and counts, resamples participants
within site, and shares each draw across targets, supports and comparisons
within a placement. All higher comparisons and all 54 target maxima are
recomputed in each draw. The main uncertainty analysis re-estimates the SD
anchor; a paired fixed-anchor analysis separates change-estimation uncertainty
from scale-estimation uncertainty. Undefined scales/comparisons remain
unresolved. Report resolved and sufficient draw counts explicitly. Percentages
at nine prespecified states are conditional sufficiency frequencies, not
whole-frontier selection probabilities or population-optimal design claims.

RQ3 robust-scale sensitivity uses the same support-specific six-day eye/MEDI/
10-s reference as primary joint scaling, never the pooled RQ1 duration scale.
Each target's frozen R_obs is rescaled by SD/(IQR/1.349), and the full All-54
decision inventory is projected again. Undefined scales cannot remove a
required target silently. Equal numerical epsilon values under two scales
express different absolute-change requirements. Fig. S1c displays the nine
prespecified states; complete scale and decision exports remain in
`results/rq3/appendix_robustness/`.

Table S1 is delivered as HTML and CSV under `results/appendix/`; Figs. S1–S5
are PNGs under `results/figures/`. All six identities and provenance live in
`results/appendix/appendix_artifact_manifest.csv`, with display CSVs alongside.
The appendix validates upstream versions and sensitivity source hashes; it
never silently prepares analysis. Historical unnumbered `FigS_*` layouts are
now routed by `ms_plot_save()` to `results/diagnostics/figure_exploration/`.
They remain reproducible diagnostic exports outside the six-item appendix.
`MS_PLOT_PREP_ONLY=1` builds the entire appendix in memory without writes.

RQ1 sensitivity exports now carry Core/RQ1 versions. Existing versionless
frozen sensitivity exports are accepted only with exact primary pair-key and
unit/participant-count agreement on available pairs; missing robust-scale pairs
must have an undefined/nonpositive audited robust scale. They remain unavailable
in display exports and are not relabelled as new analyses.
