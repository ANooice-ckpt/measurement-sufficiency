# RQ2 manuscript replacement draft: context-conditioned measurement reliability

## Methods

We next tested whether contextual structure could inform the reliability of an
exposure representation at a specified tolerance. For each of the 52 daily
representations and eight single-axis contrasts, we retained the standardized
absolute configuration difference D=|z| from RQ1 on its original matched support.
We predicted the probability that D exceeded each of six fixed tolerance values
(0.05, 0.1, 0.2, 0.3, 0.5 and 1 RQ1 standardized units). These values sample a
tolerance domain and do not define universal adequacy thresholds. Predictions
were evaluated with Brier loss, averaged over tolerance values and then equally
over estimable representations. Mean absolute distortion was modelled alongside
the exceedance probabilities to define contextual reliability profiles.

All tasks used the same predictor dictionary: 18 daily and 32 daypart contextual
variables, and 16 signatures computed from the candidate configuration's frozen
hourly basis. Measurement-based models additionally included the candidate
representation itself, using sine/cosine coordinates for circular timing targets.
High-information exposure-state variables were excluded. Context-only predictions
were compared with training configuration means and, separately, training site
means. Added context was also evaluated conditional on candidate measurement.

Models used additive natural-spline bases and fixed-complexity ridge regression,
with all knots, imputation and scaling estimated within training participants.
The measured baseline was preserved when an additional residualized context
block was fitted. A more flexible measurement-only model provided a capacity
control. Predicted exceedance probabilities were projected to be nonincreasing
with increasing tolerance. No task-specific predictor selection or tuning on
held-out results was performed. Complete methods and the exploratory development
sequence are recorded in the analysis supplement.

Validation used three site-stratified, participant-grouped five-fold partitions.
All information sets were evaluated on identical held-out participant-days.
Uncertainty intervals used 1,000 paired participant bootstrap draws stratified
by site, conditional on the primary fitted predictions. Additional partitions
assessed split stability. Lower, middle and higher contextual risk groups were
defined by cutpoints in training-predicted mean distortion and applied unchanged
to held-out participants. Group composition and coverage were retained. The
analysis evaluates new participants within the observed site mix, not real-time
adaptive logging or transfer to unseen sites.

## Results

Contextual information identified reproducible differences in measurement
reliability among held-out participants. Across the eight contrasts, context-only
predictions improved Brier scores by 1.50–2.78% relative to configuration means;
all primary participant-bootstrap intervals were above zero, and the direction
was consistent across three participant partitions. Context also outperformed
site-mean predictions. Mean distortion in lower-context-risk groups was 17–29%
below the corresponding unstratified mean, whereas higher-risk groups were
14–37% above it (Fig.4a). Metric–contrast heterogeneity is shown in Fig.4c.

These differences translated into distinct tolerance-exceedance profiles. At a
tolerance of 0.2 standardized units, lower- and higher-risk groups had observed
exceedance rates of 16.4% and 28.9% for LIGHT versus MEDI, 36.6% and 46.6% for
chest versus eye, and 22.2% and 30.1% for 120 s versus 10 s (Fig.4c). These are
equal-representation descriptive summaries; target-specific profiles remain
available. Predicted probabilities captured the ordering but partially shrank
the separation between contextual groups.

Context's increment after conditioning on measured exposure was smaller and
selective (Fig.4b). Gains over the ten-df measured baseline were 0.676% for
optical representation and 0.340% for chest placement; temporal increments were
near zero. A stronger measurement-only decoder removed the apparent optical
advantage and outperformed the joint construction for temporal contrasts.
Thus, contextual reliability stratification was reproducible, while additional
post-acquisition gains depended on the information already expressed by the
measurement and on decoder capacity.

## Interpretation and bridge to RQ3

The practical contribution of context was to characterize when a specified
measurement was more or less reliable, rather than to recover every realized
exposure error. This supports evaluating measurement adequacy for an explicit
target, tolerance and context of use. It does not establish that contextual
prediction can generally replace more demanding acquisition: a retrospective
context-stratified cadence policy did not yield a compelling error–sampling
advantage. RQ3 therefore retains its observed-stability definition across higher
configurations and determines minimum sufficient burden within that frozen
domain. Daily anchor-based exceedance probabilities and RQ3 mean-change
sufficiency are complementary estimands, not interchangeable criteria.

## Figure 4 caption

**Context-conditioned measurement reliability.** (a) Actual held-out mean absolute
 distortion in lower, middle and higher contextual risk groups, relative to the
 unstratified mean. Groups use training-only predicted-risk cutpoints. (b) Brier
 score improvement from context relative to training configuration means and its
 increment beyond candidate measurements. Bars show 95% paired participant
 bootstrap intervals; smaller marks show two additional participant partitions.
 (c) Observed tolerance-exceedance probability in lower-risk (horizontal) versus
 higher-risk (vertical) contexts at all six prespecified tolerances. Each point
 represents one metric–contrast pair. Points above the identity line indicate
 successful risk ordering; points below it indicate reversed ordering. Colours
 identify configuration dimensions. Diamonds show dimension-specific marginal
 medians; horizontal and vertical bars show the respective interquartile ranges
 across metric–contrast pairs, not confidence intervals or a joint probability region.
 All 414 available pairs enter each slice: 104 placement, 50 optical and 260 temporal.
 Panels a–b weight metrics equally within contrasts. Risk groups do not certify
 RQ3 sufficiency. Predicted/observed curves and nuisance controls remain supplementary.
