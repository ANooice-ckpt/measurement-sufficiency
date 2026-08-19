# Project instructions

## Goal
Implement the analysis for **measurement sufficiency in personal light exposure** using the public MeLiDos field-study data.

## Source of truth
Before the current build, read these files in order:
1. `docs/STUDY_SPEC.md` — frozen scientific specification and estimands.
2. `docs/CORE_ARTIFACTS.md` — current core-artifact schema and build contract.
3. `docs/UPSTREAM.md` — upstream package/repository versions and reproduction boundary.
4. `docs/DATA_INVENTORY.md` — generated local data inventory; regenerate it when the inventory script changes.

`docs/RQ1_EXECUTION.md` remains the specification for the earlier RQ1 analysis layer, but it does not override the current core-artifact build. If instructions conflict, scientific definitions in `docs/STUDY_SPEC.md` take priority.

## Current phase
The current task is **core artifact extraction**: convert the expensive high-resolution MeLiDos source layer and already-downloaded ERA5 site series into durable analysis tables that support later RQ1–RQ3 work without returning to the 10-s light records or reprocessing hourly weather for ordinary statistical changes.

Required outputs:

```text
data/derived/core/metric_cube.csv.gz
data/derived/core/unit_context.csv.gz
data/derived/core/weather_1min.csv.gz
```

`metric_cube` and `unit_context` are the two main analysis artifacts. `weather_1min` is an auxiliary reusable continuous-context artifact; daily ERA5 summaries are merged into `unit_context`.

Do not begin RQ2 modeling, RQ3 sufficiency estimation, manuscript writing, or missingness simulation during this build.

## Working principles
- Preserve the scientific mapping `measurement configuration -> target representation` before any distortion/statistical projection.
- The 54 published exposure metrics are the analysis units; the six metric classes are descriptive grouping variables only.
- Support is part of the estimand. Preserve `support_id` and do not collapse rows across incompatible support lattices.
- RQ1 main effects retain their maximum valid support; stricter common supports are allowed only where joint configurations require them.
- The frozen primary temporal-resolution levels remain 10 s, 30 s, 1 min, 5 min, 15 min, and 30 min. Additional 2 min, 10 min, and 60 min rows are reserve extraction levels only and must remain identifiable by `is_primary_resolution`.
- Pairwise eye–chest and eye–wrist supports remain primary for placement. All-position common supports may be retained as reserve data for future multidimensional comparisons but must not silently replace pairwise maximal support.
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
- Use harmonized high-resolution MeLiDos time series as the source layer; do not use pre-aggregated 1-minute light data for the primary temporal-resolution construction.
- Use like-for-like ActLumus placement streams; MPI has no comparable chest/wrist placement stream.
- Do not impose a three-position intersection unless an analysis specifically requires all three positions.
- ERA5 source files live under `data/raw/era5/` and are immutable. Accept CDS CSV payloads whether delivered as plain CSV or ZIP-with-CSV content.

## Runtime and reproduction
- Run from the repository root.
- Full rebuilds are pinned to **R 4.5.0**.
- Use LightLogR 0.10.3 and melidosData 1.0.6 as recorded in `docs/UPSTREAM.md`.
- Run the upstream reproduction and validator before the core build.
- Record `sessionInfo()` for the core-artifact build.
- If a choice would materially change the scientific estimand and is not resolved by `docs/STUDY_SPEC.md`, report the ambiguity instead of silently inventing a method. Minor engineering choices should be resolved pragmatically and execution should continue.

## Completion condition
The current phase is complete when all three CSV.GZ artifacts are generated; daily ERA5 context is successfully merged into `unit_context`; the artifact summary, ERA5 QC, missing-weather-date audit, and session information are written under `logs/`; and no duplicate scientific keys remain.
