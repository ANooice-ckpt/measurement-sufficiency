# Project instructions

## Goal
Implement the analysis for **measurement sufficiency in personal light exposure** using the public MeLiDos field-study data.

## Source of truth
Read these files in order:
1. `docs/STUDY_SPEC.md` — frozen scientific specification and estimands.
2. `docs/CORE_ARTIFACTS.md` — current core-artifact schema, support lattice, extraction levels, and build contract.
3. `docs/UPSTREAM.md` — upstream package/repository versions and reproduction boundary.
4. `docs/RQ1_EXECUTION.md` — downstream RQ1 execution from completed core artifacts.
5. `docs/DATA_INVENTORY.md` — generated local data inventory; regenerate it when the inventory script changes.

If instructions conflict, scientific definitions in `docs/STUDY_SPEC.md` take priority. Extraction-only details belong to `docs/CORE_ARTIFACTS.md`. The old `scripts/05_rq1_reference.R` through `scripts/08_plot_fig1.R` and the earlier `06b_rq1_duration.R` are historical implementations and must not override the current artifact-first design.

## Current phase
The current task is **core artifact extraction**: convert the expensive high-resolution MeLiDos source layer and already-downloaded ERA5 site series into durable analysis tables that support later RQ1–RQ3 work without returning to the 10-s light records or reprocessing hourly weather for ordinary statistical changes.

Required outputs:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
```

`metric_cube` and `unit_context` are the two main analysis artifacts. `weather_1min` is an auxiliary reusable continuous-context artifact; daily ERA5 summaries are merged into `unit_context`.

After the core build completes, downstream RQ1 should read these artifacts rather than rerun high-resolution metric computation. RQ2 models, RQ3 sufficiency, bootstrap, duration-window selection, and manuscript outputs remain downstream choices and must not be written into the core artifacts.

## Working principles
- Preserve the scientific mapping `measurement configuration -> target representation` before any distortion/statistical projection.
- The 54 published exposure metrics are the analysis units; the six metric classes are descriptive grouping variables only.
- Support is part of the estimand. Preserve `support_id` and do not collapse rows across incompatible support lattices.
- RQ1 main effects retain their maximum valid support; stricter common supports are allowed only where joint configurations require them.
- Primary temporal-resolution levels are **10 s, 15 s, 20 s, 30 s, 1 min, 5 min, 15 min, and 30 min**. Additional 2 min, 10 min, and 60 min rows are reserve extraction levels only and remain identifiable by `is_primary_resolution`.
- Pairwise eye–chest and eye–wrist supports remain primary for placement. All-position common supports are reserve data for future multidimensional comparisons and must not silently replace pairwise maximal support.
- Primary monitoring-duration comparisons use contiguous `d`-day windows within an unambiguous seven-valid-day reference, not arbitrary non-contiguous subsets.
- Missing light support remains missing; never convert missing intervals to darkness/zero exposure.
- Do not add a missingness-simulation interface in the core build.
- ERA5 QC must be conservative: broad physical plausibility bounds may turn impossible values into missing, but weather observations must not be clipped merely to make later models convenient.
- ERA5 interpolation is an information-preservation convenience, not higher-resolution ground truth. Keep original-hour interpretation available through documented construction and diagnostics.

## Minimal engineering rule
Default to the smallest engineering solution that produces the scientific artifact. Do not add new hashes, frozen contracts, baselines, gates, or defensive infrastructure unless a concrete failure mode can be named and Git, version numbers, keys, types, ordinary tests, or existing checks are insufficient. Do not remove existing safety measures merely to simplify. Gates belong only at irreversible, cross-system, security, or formal-release boundaries. Preflight checks must not crowd out the actual computation.

The current duplicate-key stops and early ERA5-format/QC checks are permitted because a malformed final one-time artifact would otherwise silently contaminate all downstream RQs.

## Data rules
- `data/raw/` is immutable.
- `external/` is read-only.
- Do not modify `manuscript/methods_current.sdoc` during computation.
- Derived datasets must be reproducible from scripts.
- Use harmonized high-resolution MeLiDos time series as the source layer for the core extraction; do not use pre-aggregated 1-minute light data for primary temporal-resolution construction.
- Use like-for-like ActLumus placement streams; MPI has no comparable chest/wrist placement stream.
- Do not impose a three-position intersection unless an analysis specifically requires all three positions.
- ERA5 source files live under `data/raw/era5/` and are immutable. Accept CDS CSV payloads whether delivered as plain CSV or ZIP-with-CSV content.

## Runtime and reproduction
- Run from the repository root.
- Full rebuilds are pinned to **R 4.5.0**.
- Use LightLogR 0.10.3 and melidosData 1.0.6 as recorded in `docs/UPSTREAM.md`.
- Run the upstream reproduction and validator before the core build.
- Record `sessionInfo()` for the core-artifact build.
- On the current 96-vCPU / 192-GiB ECS, use `CORE_WORKERS=48` as the intended starting point and `CORE_FORCE=0` for checkpoint reuse unless a deliberate full rebuild is required.
- If a choice would materially change the scientific estimand and is not resolved by `docs/STUDY_SPEC.md`, report the ambiguity instead of silently inventing a method. Minor engineering choices should be resolved pragmatically and execution should continue.

## Completion condition
The current core phase is complete when all three CSV.GZ artifacts are generated; daily ERA5 context is successfully merged into `unit_context`; the artifact summary, ERA5 QC, missing-weather-date audit, and session information are written under `logs/`; and no duplicate scientific keys remain.
