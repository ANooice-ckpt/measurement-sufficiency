# RQ1 downstream execution

RQ1 has exactly two entry points:

    scripts/10_rq1_analysis.R
    scripts/11_plot_fig1.R

10 reads the durable core metric and duration artifacts. 11 reads only frozen RQ1 outputs; main PNG figures are redirected by the shared plot contract to `results/figures/`, while the RQ1 figure manifest remains under `results/rq1/`.

## Canonical pairwise object

The primary artifact is:

    results/rq1/rq1_pairwise_change_long.rds

This file is a versioned manifest for immutable canonical parts under
`results/rq1/pairwise_parts/<rq1_analysis_version>/`; it is not required to
contain the full pairwise table in memory. Each part has an atomic `.ok`
marker, so interrupted runs reuse completed parts and rebuild only missing
parts. The analysis-design identifier is embedded in the RQ1 version, so parts
from an older temporal lattice cannot be reused after a design change.

Each row is a smallest-unit pairwise comparison and uses:

- config_a_id, config_b_id;
- value_a, value_b, delta, z;
- dimension, comparison lattice, pair role and requirement relation;
- support, participant, unit and window identifiers;
- metric geometry, scale-anchor identifier, standardized `z`/`robust_z` and availability.

For ordered dimensions, state_a is the less demanding state and state_b the more demanding state. **delta = value_b - value_a**. Placement/optical facets have a documented empirical orientation but no burden order. Candidate/reference terminology is retained only in historical compatibility outputs.

## Pair map

- Placement: eye–chest and eye–wrist on separate maximal supports.
- Optical: LIGHT–MEDI on the eye full support.
- Temporal: all `choose(6,2)=15` pairs among the frozen primary states **10, 20, 30, 40, 60 and 120 s**; adjacent transitions are flagged separately; 10-s anchor projections are a slice, not the canonical ontology. Five minutes is a core sensitivity state only and does not enter the primary RQ1 pair map.
- Duration: canonical rows retain every nested pair of 1–6 complete-day windows; inferential summaries project them to the 15 generic n-day comparison types, with adjacent d -> d+1 types flagged.

All pairs within a lattice join one standardizer. Primary scaling is SD; IQR/1.349 is sensitivity. The empirical distribution comes before A=mean(abs(z)) and B=mean(z); A >= |B| is checked.

## Outputs

    results/rq1/
      rq1_pairwise_change_long.rds
      pairwise_parts/<rq1_version>/      # canonical parts + .ok markers
      rq1_pairwise_summary.csv
      rq1_pairwise_bootstrap.csv
      rq1_anchor_projection.csv
      rq1_local_transition_summary.csv
      rq1_metric_availability.csv
      rq1_pair_type_counts.csv
      rq1_robust_scale_sensitivity.csv
      rq1_participant_balanced_sensitivity.csv
      figure_artifact_manifest.csv

    results/figures/
      Fig1_RQ1.png
      FigS_RQ1_*.png

Duration cohort/run/window audit is written under results/diagnostics/. RQ2
loads only selected primary pairwise columns/rows through the manifest loader;
RQ3 uses the frozen summary/local projections and manifest version. Plot
scripts read frozen outputs only.

## Production server defaults

Production reruns are expected to use the same validated **48-vCPU / 192-GiB ECS** class used for the full analysis. The repository therefore treats that machine as the default deployment target rather than a conservative generic server. Unless the hardware changes or a diagnostic run deliberately requires lower parallelism, use the checked-in defaults unchanged:

```text
RQ1_STARTUP_WORKERS=36
RQ1_PART_WORKERS=44
RQ1_FRAGMENT_WORKERS=36
RQ1_BOOT_WORKERS=40
RQ1_BOOT=1000
RQ1_PART_COMPRESSION=gzip
```

`RQ1_STARTUP_WORKERS` parallelizes duration-anchor startup scans, `RQ1_PART_WORKERS` parallelizes immutable canonical-part generation, `RQ1_FRAGMENT_WORKERS` parallelizes summary-fragment checkpoints, and `RQ1_BOOT_WORKERS` controls the participant-cluster/site-stratified bootstrap workers. The startup, duration-part and fragment schedules retain the validated large-task-first/LPT-style ordering. BLAS/OpenMP inner threading remains limited to one thread by the server runner so these worker counts do not create nested parallelism.

These values are production defaults, not per-run tuning suggestions. Environment overrides remain supported for a different machine or explicit troubleshooting. `RQ1_PART_COMPRESSION=gzip` is the speed-oriented default; `xz` remains available when storage is more constrained.
