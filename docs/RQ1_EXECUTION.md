# RQ1 downstream execution

RQ1 now has two paired analysis/plotting stages:

```text
scripts/10_rq1_analysis.R                  -> scripts/11_plot_fig1.R
scripts/10b_rq1_inferential_preservation.R -> scripts/11b_plot_fig2.R
```

The first pair defines the canonical representation-change analysis. The second is a downstream consequence extension that asks whether the already-observed representation distortion propagates into day-level human-state association estimates across Sleep, Alertness and Affect. It remains within RQ1 and does not create a new research question.

`10_rq1_analysis.R` reads the durable core metric and duration artifacts. `11_plot_fig1.R` reads only frozen RQ1 outputs. `10b_rq1_inferential_preservation.R` reuses participant-day candidate/reference values from the frozen RQ1 pairwise artifact, joins harmonized sleep diaries and current-conditions EMA, and writes an independent versioned inference artifact. It does not reopen or recompute Core. `11b_plot_fig2.R` reads only those frozen results and does not refit models.

Main PNG figures are redirected by the shared plot contract to `results/figures/`, while the RQ1 figure manifest remains under `results/rq1/`.

## Canonical pairwise object

The primary RQ1 representation artifact is:

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

## Downstream inferential-preservation extension

The downstream extension is intentionally narrower than the full RQ1 configuration lattice. It evaluates whether representation distortion has an observable consequence for association estimates without turning the paper into a separate health-effect study.

### Eligible representations and outcome domains

Only the 52 participant-day metrics enter this layer. Participant-level interdaily stability and intradaily variability are excluded because their temporal support does not match repeated daily outcomes.

Six day-level outcomes span three domains:

- **Sleep:** sleep quality (1–5, Very poor to Very good), number of awakenings, and awake duration in minutes;
- **Alertness:** daily Karolinska Sleepiness Scale (KSS; 1–10, higher = sleepier);
- **Affect:** daily positive affect = mean(elated, energetic) and negative affect = mean(anxious, sad, angry, irritable), using the 0–6 MoodZoom item scale.

Complete calendar-day exposure on day D is paired with the sleep diary whose local wake date is D+1. Sleep-onset time, sleep duration and other diary-derived timing outcomes are not used because they would be structurally coupled to exposure representations whose calculation windows already depend on diary sleep/wake timing.

Current-conditions EMA is treated as repeated sampling of day-level human state rather than as an acute causal-response design. Responses are assigned to the nearest nominal **11:00, 14:00, 17:00 and 20:00** slot within ±120 min, with at most one nearest response retained per slot; at least two valid slots are required to form each daily KSS or affect outcome. Same-day complete exposure and the resulting daily EMA phenotype are therefore interpreted only as a descriptive day-level association.

### Eight single-axis contrasts

The reference configuration is eye / MEDI / 10 s. Exactly eight alternative states are evaluated:

```text
placement:  chest / MEDI / 10 s
            wrist / MEDI / 10 s

optical:    eye / LIGHT / 10 s

temporal:   eye / MEDI / 20 s
            eye / MEDI / 30 s
            eye / MEDI / 40 s
            eye / MEDI / 60 s
            eye / MEDI / 120 s
```

These are one-axis projections, not the Cartesian product of placement × optical × cadence. Monitoring duration is not included because changing accumulation length changes the temporal support of the exposure–outcome pairing rather than merely changing the representation of a fixed participant-day.

### Matched-support association comparison

For each metric × outcome × contrast, the reference and candidate associations are fit on exactly the same eligible participant-days. Participants require at least two matched days. Participant fixed effects absorb site-invariant and person-invariant differences; participants are keyed by site + Id. Bootstrap draws resample complete participant blocks within site and apply the same draw to reference and candidate fits.

Linear exposure metrics are scaled by the reference SD on the matched support. Circular-time metrics enter jointly as sine and cosine terms and are never reduced to a discontinuous clock-time slope.

The principal downstream quantity is inferential deviation in reference-bootstrap uncertainty units:

```text
linear metric:   |beta_candidate - beta_reference| / SE_reference
circular metric: Mahalanobis norm of the paired sin/cos coefficient displacement
                 using the reference-bootstrap covariance
```

This quantity measures configuration sensitivity of the observed association estimate. It is not called causal bias because eye/MEDI/10 s is an empirical high-information reference, not latent biological truth.

### Frozen RQ1 distortion is the upstream predictor

The main Fig. 2 propagation analysis does **not** recompute RQ1 distortion on outcome-specific subsets. Instead, each eligible metric/contrast is joined to the already-frozen `A_mean_absolute` from `rq1_pairwise_summary.csv`.

An outcome-matched distortion is still calculated inside the paired model fit for support auditing, but it is stored as `matched_support_distortion_A/B` and must not replace the frozen RQ1 distortion in the main downstream-consequence result.

### Reference association landscape

A separate eye/MEDI/10-s association profile is estimated for each metric and outcome on that metric's own maximal eligible daily support. Its role is descriptive: it shows the association landscape whose stability is subsequently tested across three human-state domains. It is not a discovery screen and should not be presented as a multiple-testing health-effect atlas.

## Outputs

```text
results/rq1/
  rq1_pairwise_change_long.rds
  pairwise_parts/<rq1_version>/
  rq1_pairwise_summary.csv
  rq1_pairwise_bootstrap.csv
  rq1_anchor_projection.csv
  rq1_local_transition_summary.csv
  rq1_metric_availability.csv
  rq1_pair_type_counts.csv
  rq1_robust_scale_sensitivity.csv
  rq1_participant_balanced_sensitivity.csv
  figure_artifact_manifest.csv

results/rq1/inference/
  rq1_inferential_preservation_domains_anchor8.rds
  rq1_inferential_preservation_summary.csv
  rq1_inferential_preservation_term_summary.csv
  rq1_reference_association_summary.csv
  rq1_downstream_outcome_audit.csv
  fig2_reference_association_landscape.csv
  fig2_inferential_degradation.csv
  fig2_distortion_inference_link.csv

results/figures/
  Fig1_RQ1.png
  Fig2_RQ1_inferential_preservation.png
  FigS_RQ1_*.png
```

The historical `rq1_inferential_preservation.rds` all-configuration prototype and the intermediate sleep-only `rq1_inferential_preservation_anchor8.rds` artifact are retired. The v3 analysis removes both before writing the three-domain artifact so legacy drawing code cannot be mistaken for the active Fig. 2.

Duration cohort/run/window audit is written under `results/diagnostics/`. RQ2 loads only selected primary pairwise columns/rows through the manifest loader; RQ3 uses the frozen summary/local projections and manifest version. Plot scripts read frozen outputs only.

## Production server defaults

Production reruns are expected to use the same validated **48-vCPU / 192-GiB ECS** class used for the full analysis. The repository therefore treats that machine as the default deployment target rather than a conservative generic server. Unless the hardware changes or a diagnostic run deliberately requires lower parallelism, use the checked-in defaults unchanged:

```text
RQ1_STARTUP_WORKERS=36
RQ1_PART_WORKERS=44
RQ1_FRAGMENT_WORKERS=36
RQ1_BOOT_WORKERS=40
RQ1_BOOT=1000
RQ1_INFERENCE_BOOT=1000
RQ1_PART_COMPRESSION=gzip
```

`RQ1_STARTUP_WORKERS` parallelizes duration-anchor startup scans, `RQ1_PART_WORKERS` parallelizes immutable canonical-part generation, `RQ1_FRAGMENT_WORKERS` parallelizes both summary-fragment checkpoints and the canonical-part relational-preservation scan, and `RQ1_BOOT_WORKERS` controls the participant-cluster/site-stratified bootstrap workers. The inferential-preservation layer uses grouped sufficient statistics and vectorized bootstrap algebra but deliberately has no second worker pool. BLAS/OpenMP inner threading remains limited to one thread by the server runner.

These values are production defaults, not per-run tuning suggestions. Environment overrides remain supported for a different machine or explicit troubleshooting. `RQ1_PART_COMPRESSION=gzip` is the speed-oriented default; `xz` remains available when storage is more constrained.
