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
- High-information benchmark is eye / MEDI / 10 s / protocol-anchored seven-day reference; it is an empirical benchmark, not biological truth.
- Support is part of the estimand. Pairwise placement analyses keep maximal eye–chest / eye–wrist supports.
- Optical LIGHT is an operational proxy. MDER/nvRD are unavailable when the candidate configuration has LIGHT only.
- Temporal primary levels: **10, 20, 30, 60, 300, 900, 1800 s**. Reserve: 120, 600, 3600 s. 15 s is prohibited.
- Coarse temporal configurations are **systematic sparse subsamples of the 10-s grid**, not bin means. Retained source values cannot change.
- Monitoring duration uses MeLiDos `trial_times` to anchor a fixed seven-day reference. Enumerate all contiguous 1–6 d candidate windows; do not substitute arbitrary non-contiguous subsets.
- Unavailable configurations are unavailable, not high-distortion/insufficient.
- RQ3 Pareto dominance applies only to justified ordered dimensions (temporal resolution, duration). Placement/optical are incomparable facets.
- Multidimensional RQ3 uses facet-specific maximal supports, not a gratuitous eye+chest+wrist full-support intersection.

## Artifact/cache rules
Current core version: `v2_sparse_sampling_protocol7`.

Interim core blocks are versioned under `data/interim/core/<core_version>/`. Do not point v2 at pre-v2 cache paths. Final core artifacts carry `core_artifact_version` and `data/derived/core/core_manifest.csv` records the temporal operator.

RQ1 outputs carry `rq1_analysis_version`; RQ2 checkpoint paths include that upstream version. Never reuse old RQ2 checkpoints after an upstream scientific version changes.

## Runtime
Full rebuilds use R 4.5.0, LightLogR 0.10.3, melidosData 1.0.6. On the 96-vCPU Linux ECS, `CORE_WORKERS=48` is the intended start. On a 16-core/32-thread local Windows machine, RQ2 defaults near 12 PSOCK workers; workers keep BLAS/OpenMP at one thread each.

## Do not
- Return to the raw 10-s source for ordinary RQ1–RQ3 changes after a validated core exists.
- Average hidden high-frequency observations to simulate a slower logger.
- Use Day 8 to replace a missing protocol reference day merely to reach seven days.
- Treat metric classes as inferential replicates.
- Invent universal sufficiency thresholds or universal burden orders for placement/optical.
