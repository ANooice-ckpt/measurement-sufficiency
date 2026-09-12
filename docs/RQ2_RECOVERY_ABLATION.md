# RQ2 recovery as a nested information-ablation experiment

This note describes the post-hoc analysis in `scripts/12e_rq2_recovery_ablation.R`
and the figure source in `scripts/13c_plot_fig4_recovery.R`.

The analysis does **not** refit the recovery models and therefore does not change
the frozen run ID or any checkpoint. It only reorganizes the already-held-out
errors from `12d_rq2_recovery.R`.

## Experimental interpretation

For each metric × degradation contrast, the same held-out participant-days are
compared across four nested information states:

1. `raw`: use the degraded representation `Y_L` as-is;
2. `self-calibration`: supervised residual correction from `Y_L` alone;
3. `+ measurement signature`: add the 16 compact summaries `S_L` extracted from
   the same low-burden measurement configuration;
4. `+ context`: add the prespecified daily and daypart external,
   microenvironmental and behavioural context `C`.

For the primary XGBoost learner this is a fixed-learner feature-group ablation.
Ridge is the matched linear reference. The comparison therefore separates two
questions:

- **information availability**: does adding `S_L` or `C` improve held-out
  reconstruction when learner family is fixed?
- **decoder flexibility**: are the same information increments recovered by a
  linear ridge model and a nonlinear tree model?

The stage-specific estimands are signed and are never clipped:

```text
calibration_increment = A_raw         - A_calibration
signature_increment   = A_calibration - A_signature
context_increment     = A_signature   - A_context
total_increment       = A_raw         - A_context
```

Positive values indicate recovery; negative values indicate that the added
model/information layer harmed held-out reconstruction. `A_context` remains the
unrecovered residual under the tested input/model/CV design, not an identified
irreducible error.

## Why the atlas is stratified

The pooled mean across all recovery tasks is not the primary scientific summary.
Temporal-sampling contrasts contain many tasks and several temporal-dynamics
representations are extremely self-calibratable, so a pooled average can make
self-calibration appear universally dominant and auxiliary context appear
negligible.

The primary atlas therefore uses

```text
configuration degradation × metric class × information step
```

with one cell for each contrast × metric-class group. Each cell reports:

- mean incremental `ΔA` for the information step;
- fraction of constituent metrics with positive incremental `ΔA`;
- number of metrics contributing to the cell.

Cells with fewer than three metrics remain in the CSV source but are omitted
from the main heatmap. This is a display rule only; no cell is removed from the
underlying recovery analysis.

The intended interpretation is mechanistic rather than a search for one globally
best recovery layer. Typical regimes to inspect include self-calibratable
sampling/dynamics losses, within-measurement redundancy, context-assisted
placement/level losses, and largely unrecovered timing/geometric losses.

## Outputs

Run after a completed `12d` recovery analysis:

```powershell
Rscript scripts/12e_rq2_recovery_ablation.R
Rscript scripts/13c_plot_fig4_recovery.R
```

If several completed recovery runs exist, set `RQ2_RECOVERY_RUN_DIR` or pass the
run directory as the sole command-line argument.

`12e` writes `<run_dir>/ablation/`:

- `recovery_metric_decomposition.csv`: one row per learner × metric × contrast;
- `recovery_information_atlas.csv`: contrast × metric-class summaries;
- `recovery_information_atlas_long.csv`: plotting-ready stage-specific atlas;
- `recovery_ablation_by_dimension.csv`: degradation-dimension summaries;
- `recovery_ablation_by_metric_class.csv`: metric-class summaries;
- `recovery_ablation_overall.csv`: pooled values retained only as a secondary audit;
- `recovery_decoder_capacity_atlas.csv`: matched XGBoost–ridge cell comparison;
- `context_gain_ranked.csv`: all XGBoost metric-level context increments, sorted
  for inspection without selecting or refitting tasks.

`13c` writes `<run_dir>/ablation/figures/`:

- `Fig4_recovery_information_ablation.pdf/.png`;
- `Fig4_recovery_information_ablation_source.csv`;
- `FigS_recovery_decoder_capacity.pdf/.png`;
- `FigS_recovery_decoder_capacity_source.csv`.

The three main heatmaps deliberately use separate symmetric `ΔA` colour scales,
because self-calibration gains can be orders of magnitude larger than signature
or context increments. Numeric colour-bar values, not hue intensity across
panels, must be used for cross-stage magnitude comparison. Cell text gives the
within-cell fraction of metrics improved.

## What this does not do

- It does not weaken self-calibration to make context look stronger.
- It does not select only favourable context tasks for model fitting.
- It does not introduce a new predictor family.
- It does not use target-derived exposure state as a recovery predictor.
- It does not claim that a negative or small context increment proves context is
  uninformative in general; the inference is conditional on `Y_L + S_L`, the
  prespecified context set, and the held-out participant design.
- It does not implement adaptive layer selection. Any future safe-selection
  analysis must choose the information layer using only data nested inside each
  outer-training fold; selecting from the existing outer-fold errors would leak
  held-out information.
