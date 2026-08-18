# Project instructions

## Goal
Implement the analysis for measurement sufficiency in personal light exposure.

## Source of truth
Before analysis, read:
- docs/STUDY_SPEC.md
- docs/UPSTREAM.md
- docs/DATA_INVENTORY.md

## Upstream
Files under external/ are read-only references.
Do not modify upstream repositories.

## Data rules
- data/raw/ is immutable.
- Never overwrite downloaded source data.
- Derived datasets must be reproducible from scripts.
- Do not force a three-position intersection unless the analysis explicitly requires all three positions.
- Use the maximum valid sample for each measurement operator.

## Scientific rules
- Preserve the published 54-metric system and six metric classes.
- Do not invent a new LEH taxonomy.
- Do not invent a universal fidelity threshold.
- RQ3 treats fidelity as a continuous requirement.
- Single-dimension analyses estimate fidelity curves and knee points.
- Multidimensional analyses estimate Pareto-optimal configurations.

## Reproducibility
- Run analyses from the project root.
- Use scripted workflows only.
- Record sample flow and exclusions.
- Save intermediate analytical tables.
- Record package versions and sessionInfo().
- Every major analysis must have a diagnostic or validation output.
