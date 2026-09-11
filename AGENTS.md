# Project instructions

## Source of truth
Read in this order:
1. `docs/STUDY_SPEC.md`
2. `docs/CORE_ARTIFACTS.md`
3. `docs/UPSTREAM.md`
4. `docs/RQ1_EXECUTION.md`
5. `docs/FIGURE_ARCHITECTURE.md`

Scientific definitions in `STUDY_SPEC.md` override implementation notes elsewhere.

## Executable structure
The expensive source-to-core layer ends at `scripts/09_build_core_artifacts.R`. Downstream analysis/plotting is deliberately paired:

```text
10_rq1_analysis.R                    -> 11_plot_fig1.R
10b_rq1_inferential_preservation.R   -> 11b_plot_fig2.R
12_rq2_analysis.R                    -> 13a_plot_fig3.R + 13b_plot_fig4.R
14_rq3_analysis.R                    -> 15a_plot_fig5.R + 15b_plot_fig6.R
```

Main-figure identity has one source of truth: `scripts/utils/figure_registry.R`. It records the current manuscript ID and the single canonical plotting entrypoint for every main figure. RQ2/RQ3 plotting code still contains some pre-insertion figure IDs and component names internally because Fig. 2 was inserted after those figures matured; `scripts/utils/plot_contracts.R` converts those legacy output identities to current identities exactly once. There is no separate canonical-wrapper versus legacy-implementation script layer. A stored manifest is already current and must never be renumbered again.

Main plot scripts read frozen outputs only and must not silently refit/recompute their corresponding analysis. Active inferential-preservation plotting belongs exclusively to `11b_plot_fig2.R`. `scripts/16_plot_supplementary.R` has been retired and must not be referenced by runners, registries or plot-contract auto-detection.

## Frozen scientific rules
- Scientific object: `configuration -> observed exposure process -> target representation`.
- 54 published exposure metrics are analytical units; six metric classes are descriptive only.
- Distribution first: preserve smallest-unit distortion before A/B, models, sufficiency, or Pareto projections.
- High-information benchmark is eye / MEDI / 10 s; it is an empirical scale anchor, not biological truth.
- Support is part of the estimand. Pairwise placement analyses keep maximal eye–chest / eye–wrist supports.
- Optical LIGHT is an operational proxy. MDER/nvRD are unavailable when the candidate configuration has LIGHT only.
- Temporal primary levels: **10, 20, 30, 40, 60 and 120 s**. Reserve sensitivity: 300 s. 15 s is prohibited.
- Coarse temporal configurations are **systematic sparse subsamples of the 10-s grid**, not bin means. Retained source values cannot change.
- Monitoring duration uses consecutive complete analysis days and enumerates every contiguous 1–6 d window in each valid run. `trial_times` remains audit/descriptive metadata and does not define primary eligibility.
- Unavailable configurations are unavailable, not high-distortion/insufficient.
- RQ2 contextual models may consume the existing ERA5/unit-context variables and harmonized MeLiDos diaries, but must not redefine core weather ingestion, measurement configurations or target representations.
- RQ3 Pareto dominance applies only to justified ordered dimensions (temporal resolution, duration). Placement/optical are incomparable facets.
- Multidimensional RQ3 uses facet-specific maximal supports, not a gratuitous eye+chest+wrist full-support intersection.

## RQ1 downstream-inference extension
The human-state consequence layer remains part of RQ1; it is not a fourth research question and does not redefine the primary representation metrics.

- Eligible exposure representations are the 52 participant-day metrics only; participant-level IS/IV do not enter this daily outcome layer.
- Outcomes span three day-level domains: **Sleep** (next-morning sleep quality, awakenings, awake duration), **Alertness** (daily KSS), and **Affect** (daily positive affect = mean(elated, energetic); negative affect = mean(anxious, sad, angry, irritable)).
- Sleep pairs calendar exposure day D with local wake date D+1. Current-conditions EMA is harmonized to nominal 11/14/17/20 h slots; the nearest response within ±120 min is retained per slot and at least two valid slots are required per daily EMA outcome. Same-day exposure–EMA associations are descriptive day-level relationships, not acute causal-response estimates.
- Exactly eight single-axis contrasts are evaluated against eye/MEDI/10 s: chest and wrist placement, LIGHT optical representation, and 20/30/40/60/120 s temporal sampling. Duration and multi-axis Cartesian combinations are excluded.
- Candidate and reference exposure associations are fitted on exactly the same matched participant-days with participant fixed effects. Bootstrap resampling is by participant, stratified by site, with identical draws for the paired fits.
- The upstream distortion in the main downstream-consequence analysis is the already frozen RQ1 `A_mean_absolute` for the same metric/contrast. A distortion recomputed on outcome-matched support may be retained only as an audit quantity and must not replace the frozen RQ1 x-axis.
- Inferential deviation is standardized by reference-bootstrap uncertainty: absolute coefficient displacement divided by reference SE for linear metrics and the corresponding joint Mahalanobis displacement for circular sin/cos metrics.
- Observed exposure–human-state associations are descriptive association-preservation diagnostics. The reference configuration is not biological truth and coefficient displacement is not called causal bias.

## Artifact/cache rules
Current core version family: `v4_sparse_sampling_complete_days__<core_design_id>`.

Interim core blocks are versioned under the current results/core cache hierarchy. Do not point the active core at pre-v4 cache paths. Final core artifacts carry `core_artifact_version`, and the core manifest records the temporal operator and design identity.

RQ1 outputs carry `rq1_analysis_version`; the inferential extension additionally carries `rq1_inference_version`. RQ2 checkpoint paths include the upstream RQ1 version. Never reuse old RQ2 checkpoints after an upstream scientific version changes.

The active inferential-preservation artifact is `results/rq1/inference/rq1_inferential_preservation_domains_anchor8.rds`. Both historical `rq1_inferential_preservation.rds` and the intermediate sleep-only `rq1_inferential_preservation_anchor8.rds` are retired and must not be used for active plotting.

## Runtime
Full rebuilds use R 4.5.0, LightLogR 0.10.3, melidosData 1.0.6. On the large Linux ECS, worker counts are controlled by the server runners and remain environment-overridable. On a 16-core/32-thread local Windows machine, RQ2 defaults near 12 PSOCK workers; workers keep BLAS/OpenMP at one thread each.

## Do not
- Return to the raw 10-s source for ordinary RQ1–RQ3 changes after a validated core exists.
- Average hidden high-frequency observations to simulate a slower logger.
- Use Day 8 or protocol dates to manufacture a fixed-duration reference when complete-analysis-day eligibility does not support it.
- Treat metric classes as inferential replicates.
- Invent universal sufficiency thresholds or universal burden orders for placement/optical.
- Expand the inferential-preservation layer back into all 35 placement × optical × cadence combinations unless the scientific estimand is explicitly redesigned.
- Recompute an outcome-specific distortion and present it as the RQ1 distortion propagated downstream.
- Reinterpret same-day EMA associations as temporally ordered acute health effects without redesigning the exposure support.
- Reintroduce duplicate numbered plot wrappers or retired pre-insertion plotting filenames; each main figure has one canonical script.
