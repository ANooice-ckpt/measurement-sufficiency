# RQ1 downstream execution

RQ1 has exactly two entry points:

    scripts/10_rq1_analysis.R
    scripts/11_plot_fig1.R

10 reads results/core/metric_cube.csv.gz, results/core/unit_context.csv.gz only for the core contract, and results/core/duration_metric_cube.rds. 11 reads only frozen RQ1 outputs and writes figures below results/rq1/figures/.

## Canonical pairwise object

The primary artifact is:

    results/rq1/rq1_pairwise_change_long.rds

This file is a versioned manifest for immutable canonical parts under
`results/rq1/pairwise_parts/<rq1_analysis_version>/`; it is not required to
contain the full pairwise table in memory. Each part has an atomic `.ok`
marker, so interrupted runs reuse completed parts and rebuild only missing
parts. The current implementation writes one non-duration part and one part
per core duration support/site block.

Each row is a smallest-unit pairwise comparison and uses:

- config_a_id, config_b_id;
- value_a, value_b, delta, z;
- dimension, comparison lattice, pair role and requirement relation;
- support, participant, unit and window identifiers;
- metric geometry, standardizer, robust standardizer and availability.

For ordered dimensions, state_a is the less demanding state and state_b the more demanding state. delta = value_a - value_b. Placement/optical facets have a documented empirical orientation but no burden order. Candidate/reference terminology is retained only in historical compatibility outputs.

## Pair map

- Placement: eye–chest and eye–wrist on separate maximal supports.
- Optical: LIGHT–MEDI on the eye full support.
- Temporal: all choose(7,2)=21 pairs among 10, 20, 30, 60, 300, 900 and 1800 s; adjacent transitions are flagged separately; 10-s anchor projections are a slice, not the canonical ontology.
- Duration: every nested pair of complete-day windows in the core manifest, with adjacent d -> d+1 pairs flagged.

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
      figures/

Duration cohort/run/window audit is written under results/diagnostics/. RQ2
loads only selected primary pairwise columns/rows through the manifest loader;
RQ3 uses the frozen summary/local projections and manifest version. Plot
scripts read frozen outputs only.

When RQ1_BOOT is positive, the analysis also computes participant-cluster/site-stratified
bootstrap intervals for A and B. RQ1_BOOT_WORKERS controls the cross-platform PSOCK worker
count. RQ1_PART_WORKERS controls parallel canonical-part generation. Both can
be set manually; a practical server starting point for a 100-GB node is
`RQ1_PART_WORKERS=16 RQ1_BOOT_WORKERS=8`; raise the part count to 24 only
after confirming memory headroom.
`RQ1_PART_COMPRESSION=gzip` is the speed-oriented default; `xz` remains
available when storage is more constrained.
