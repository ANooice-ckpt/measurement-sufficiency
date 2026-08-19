# Project instructions

## Goal
Implement the analysis for **measurement sufficiency in personal light exposure** using the public MeLiDos field-study data.

## Source of truth
Before analysis, read these files in order:
1. `docs/STUDY_SPEC.md` — frozen scientific specification and estimands.
2. `docs/RQ1_EXECUTION.md` — current implementation task and required outputs.
3. `docs/UPSTREAM.md` — upstream package/repository versions and reproduction boundary.
4. `docs/DATA_INVENTORY.md` — generated local data inventory; regenerate it when the inventory script changes.

If instructions conflict, scientific definitions in `docs/STUDY_SPEC.md` take priority. Upstream files define reproduction behavior only; they do not override the measurement-sufficiency estimand.

## Current phase
The current task is **RQ1 only**: construct single-dimension measurement configurations, compute representation distortion, validate the resulting canonical table, and generate a rough Fig. 1.

Do **not** begin RQ2, RQ3, ERA5 acquisition, multidimensional configuration analysis, or additional sensitivity analyses in this phase.

## Working principles
- First preserve and validate the upstream MeLiDos / LightLogR / Zauner pipeline; then build our analysis from the harmonized source layer.
- The scientific object is the mapping `measurement configuration -> target representation -> representation distortion`.
- Preserve the full empirical distortion distribution before computing summaries such as mean signed distortion or mean absolute distortion.
- The 54 published exposure metrics are the analysis units; the six metric classes are for grouping and interpretation only.
- Use the maximum valid sample for each measurement operator. Do not impose a three-position intersection unless a specific analysis requires all three positions.
- Prefer simple, readable R scripts and explicit intermediate tables. Do not build a new workflow framework, package, class hierarchy, or defensive abstraction unless it is required for the current analysis.
- Do not add new exposure metrics, new fidelity metrics, new thresholds, machine-learning models, or robustness analyses without a concrete scientific need.

## Data rules
- `data/raw/` is immutable.
- `external/` is read-only.
- Do not modify `manuscript/methods_current.sdoc` during analysis implementation.
- Derived datasets must be reproducible from scripts.
- Use harmonized high-resolution MeLiDos time series as the source layer; do not use pre-aggregated 1-minute data for the primary temporal-resolution analysis.
- Missing support must remain missing; never convert missing intervals to darkness/zero exposure.
- Record sample flow, availability, and exclusions for every RQ1 configuration dimension.

## Reproducibility and validation
- Run from the repository root.
- Run the upstream reproduction validator before starting RQ1. A numerical mismatch is a failed validation, not merely a diagnostic.
- Record package versions and `sessionInfo()` for RQ1.
- Every major RQ1 output must have a direct diagnostic check.
- If a choice would materially change the estimand and is not resolved by `docs/STUDY_SPEC.md`, report the ambiguity instead of silently inventing a method. Minor engineering choices should be resolved pragmatically and documented.

## RQ1 completion condition
RQ1 is complete only when the required canonical distortion table, sample/availability diagnostics, metric-level summaries, and a reproducible rough Fig. 1 described in `docs/RQ1_EXECUTION.md` have all been generated successfully.
