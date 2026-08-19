# RQ1 downstream execution

RQ1 has exactly two entry points:

```text
scripts/10_rq1_analysis.R
scripts/11_plot_fig1.R
```

`10` reads `metric_cube` + `unit_context`; `11` reads only RQ1 outputs.

## Temporal analysis
Reference 10 s; primary candidates 20 s, 30 s, 1 min, 5 min, 15 min, 30 min. These values already come from core v2 sparse systematic sampling. RQ1 never reconstructs/coarsens the source series.

## Duration analysis
For eye-MEDI reference supports (`eye_medi` for ordinary targets; `eye_full` for MDER/nvRD), use protocol metadata carried by `unit_context`:
- use protocol calendar Days 1–7 from the trial-start date and require all seven to be valid;
- require those seven dates consecutive;
- retain later eighth valid date only as an audit field;
- enumerate every contiguous 1–6 d window inside the fixed reference.

52 daily targets are aggregated day-first (circular mean for circular-time targets). IS/IV are rebuilt from `isiv_h00..h23` on the exact selected dates.

## Primary distortion
`e = delta/reference_SD` within comparison lattice, then empirical `D(e)`, `A=mean(abs(e))`, `B=mean(e)`. Participant-cluster/site-stratified bootstrap is used wherever the resulting cohort has a non-degenerate cluster structure.

Primary outputs:

```text
data/derived/rq1/rq1_distortion_long.rds
results/rq1/rq1_summary.csv
results/rq1/rq1_configuration_manifest.csv
results/rq1/rq1_metric_availability.csv
results/rq1/rq1_sample_flow.csv
results/rq1/rq1_retention_diagnostics.csv
```

Sensitivity outputs:

```text
results/rq1/rq1_robust_scale_sensitivity.csv
results/rq1/rq1_participant_balanced_sensitivity.csv
```

The canonical RDS and summary carry `core_artifact_version` and `rq1_analysis_version`. RQ2 uses the latter in checkpoint paths.

## Fig.1
Frozen 2×4 grammar:
- columns: placement, optical, temporal, duration;
- top row: pooled empirical `D(e)` landscapes with equal metric×configuration total weight;
- bottom row: A–B geometry; temporal/duration are trajectories.

All eight panel frames are square/aligned. Fig.1 is distortion characterization, not a sufficiency-threshold figure.
