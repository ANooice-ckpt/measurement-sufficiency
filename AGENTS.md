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

Main-figure identity has one source of truth: `scripts/utils/figure_registry.R`. It records the current manuscript ID, canonical numbered entrypoint and, where needed, the pre-insertion implementation ID/source. The numbered RQ2/RQ3 entrypoints ask the registry for their implementation; they do not encode old-to-new figure numbers themselves. `scripts/utils/plot_contracts.R` consumes the registry when mature implementations emit historical filenames/manifests. Legacy -> current and current -> legacy conversions are intentionally separate because some strings overlap across generations (for example current `Fig3_RQ2` was also a historical implementation ID). A stored manifest is already current and must never be renumbered again.

Main plot scripts read frozen outputs only and must not silently refit/recompute their corresponding analysis. Active inferential-preservation plotting belongs exclusively to `11b_plot_fig2.R`. Supplementary drawing remains centralized in `scripts/16_plot_supplementary.R`; its historical inference block points only to the retired v1 artifact path and is not part of the active Fig. 2 graph.

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
The health-related consequence layer remains part of RQ1; it is not a fourth research question and does not redefine the primary representation metrics.

- Eligible exposure representations are the 52 participant-day metrics only; participant-level IS/IV do not enter this daily outcome layer.
- Outcomes are next-morning sleep quality, number of awakenings and awake duration from the harmonized sleep diary. Calendar exposure day D is paired with local wake date D+1.
- Exactly eight single-axis contrasts are evaluated against eye/MEDI/10 s: chest and wrist placement, LIGHT optical representation, and 20/30/40/60/120 s temporal sampling. Duration and multi-axis Cartesian combinations are excluded.
- Candidate and reference exposure associations are fitted on exactly the same matched participant-days with participant fixed effects. Bootstrap resampling is by participant, stratified by site, with identical draws for the paired fits.
- The upstream distortion in the main downstream-consequence analysis is the already frozen RQ1 `A_mean_absolute` for the same metric/contrast. A distortion recomputed on outcome-matched support may be retained only as an audit quantity and must not replace the frozen RQ1 x-axis.
- Inferential deviation is standardized by reference-bootstrap uncertainty: absolute coefficient displacement divided by reference SE for linear metrics and the corresponding joint Mahalanobis displacement for circular sin/cos metrics.
- Observed exposure–sleep associations are descriptive association-preservation diagnostics. The reference configuration is not biological truth and coefficient displacement is not called causal bias.

## Artifact/cache rules
Current core version family: `v4_sparse_sampling_complete_days__<core_design_id>`.

Interim core blocks are versioned under the current results/core cache hierarchy. Do not point the active core at pre-v4 cache paths. Final core artifacts carry `core_artifact_version`, and the core manifest records the temporal operator and design identity.

RQ1 outputs carry `rq1_analysis_version`; the inferential extension additionally carries `rq1_inference_version`. RQ2 checkpoint paths include the upstream RQ1 version. Never reuse old RQ2 checkpoints after an upstream scientific version changes.

The active inferential-preservation artifact is `results/rq1/inference/rq1_inferential_preservation_anchor8.rds`. The unversioned historical `rq1_inferential_preservation.rds` path is retired and must not be used for active plotting.

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
