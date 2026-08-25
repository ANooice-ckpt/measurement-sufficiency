# Project instructions

## Source of truth
Read in this order:
1. `docs/STUDY_SPEC.md`
2. `docs/CORE_ARTIFACTS.md`
3. `docs/UPSTREAM.md`
4. `docs/RQ1_EXECUTION.md`

Scientific definitions in `STUDY_SPEC.md` override implementation notes elsewhere.

## Executable structure
The expensive source-to-core layer ends at `scripts/09_build_core_artifacts.R`. Downstream analysis/plotting is deliberately paired:

```text
10_rq1_analysis.R -> 11_plot_fig1.R
12_rq2_analysis.R -> 13_plot_rq2.R
14_rq3_analysis.R -> 15_plot_rq3.R
```

Plot scripts read frozen outputs only and must not silently refit/recompute their corresponding analysis.

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

## Artifact/cache rules
Current core version family: `v4_sparse_sampling_complete_days__<core_design_id>`.

Interim core blocks are versioned under the current results/core cache hierarchy. Do not point the active core at pre-v4 cache paths. Final core artifacts carry `core_artifact_version`, and the core manifest records the temporal operator and design identity.

RQ1 outputs carry `rq1_analysis_version`; RQ2 checkpoint paths include that upstream version. Never reuse old RQ2 checkpoints after an upstream scientific version changes.

## Runtime
Full rebuilds use R 4.5.0, LightLogR 0.10.3, melidosData 1.0.6. On the large Linux ECS, worker counts are controlled by the server runners and remain environment-overridable. On a 16-core/32-thread local Windows machine, RQ2 defaults near 12 PSOCK workers; workers keep BLAS/OpenMP at one thread each.

## Do not
- Return to the raw 10-s source for ordinary RQ1–RQ3 changes after a validated core exists.
- Average hidden high-frequency observations to simulate a slower logger.
- Use Day 8 or protocol dates to manufacture a fixed-duration reference when complete-analysis-day eligibility does not support it.
- Treat metric classes as inferential replicates.
- Invent universal sufficiency thresholds or universal burden orders for placement/optical.
