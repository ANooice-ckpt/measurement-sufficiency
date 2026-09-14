# RQ2 figure redesign, 2026-09-14

## Current revision

Following visual review, the long target atlas and numeric tolerance matrix were
removed from the main figure. Panel a restores the pooled dumbbell plot; b retains
paired information-value estimates; c uses six tolerance-faceted scatterplots of
lower versus higher context exceedance probability, with dimension-specific
marginal medians and IQRs. Every available metric–contrast pair is retained.
The diagonal communicates risk separation geometrically without reading cell
numbers. IQRs describe heterogeneity, not inferential uncertainty. Fig.3 height
is reduced from 8.60 to 6.88 inches, with width unchanged.

## Superseded atlas proposal (retained as design history)

The former Fig.3c and Fig.4b differ in targets, available information and supports,
but repeat the narrative question of out-of-participant predictive usefulness.
Move the former Fig.3c to a supplementary figure, preserving its frozen evidence.
Fig.3 now presents contextual coefficients, conditional geometry and cross-axis
non-additivity. Fig.4 presents operational information value.

The former Fig.4a and Fig.4c both summarized the same risk-group separation and
hid metric heterogeneity. The redesigned figure separates three questions:

1. **Which targets?** A complete 52 × 8 atlas of normalized mean-risk separation.
   Rows retain their analytical identity. Negative separation is shown, including
   temporal timing targets that the pooled summaries obscure.
2. **What additional information?** Paired predictive-score improvements and
   participant-bootstrap uncertainty distinguish contextual prediction from its
   increment beyond measurement. The small incremental estimates are unchanged.
3. **What does tolerance mean?** A six-tolerance matrix combines the absolute
   probability of exceedance with the context-dependent gap. This is a context
   of use description, not a new sufficiency threshold or burden-saving claim.

Panels a and c remain related through their common held-out groups, but display
different projections: target heterogeneity in mean distortion versus absolute
tolerance-specific probabilities. They should not be called independent evidence.
All groups and models remain frozen; no fitting, feature selection, threshold
selection, or significance screening was introduced for the redesign.

The previous Fig.4 pooled panels and predicted/observed curves are retained in
`FigS_RQ2_reliability_profiles.png`. Display CSVs expose every atlas and tolerance
matrix value. Whole-percentage labels are rounded, including values near zero.
