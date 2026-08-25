# Frozen study specification

## 1. Scientific object and terminology

A measurement configuration is c = (placement, optical representation, temporal resolution, monitoring duration). For target representation M_k, the scientific object is the empirical map:

    configuration state -> observed exposure process -> M_k(E_c)

The primary comparison object is the representation change between two observed configuration states with common support. Distribution comes first. Mean signed/absolute change, conditional models, second differences, stability, sufficiency and Pareto projections are downstream summaries.

Measurement sufficiency is operational and target/tolerance dependent. The high-information eye / MEDI / 10-s benchmark is an empirical scale anchor, not biological truth.

## 2. Target representations

Use the published Zauner/LightLogR system of 54 metrics unchanged: 52 participant-day representations plus interdaily stability and intradaily variability as multiday participant representations. The six published metric classes are descriptive groupings only.

## 3. Temporal states and supports

The harmonized MeLiDos 10-s grid is the source schedule. The frozen primary temporal states are **10, 20, 30, 40, 60 and 120 s**. They span the practical high-frequency wearable-logging domain while resolving the operationally important sub-minute to minute-scale region. **300 s (5 min)** is retained only as an intentionally coarse sensitivity state and does not enter the primary RQ1-RQ3 burden lattice. Cadences coarser than 5 min are not materialized in the active core. Fifteen seconds is excluded because it cannot be represented as an equal-spacing subset of the 10-s source grid.

For every r > 10 s, construct a deterministic participant/source-grid-phase-anchored systematic sparse subsample. Retained timestamps and MEDI/LIGHT values must be exact source rows. No averaging, interpolation or hidden reconstruction. Temporal resolution therefore denotes a literal logger sampling interval in this study.

All primary temporal states retain the full target-representation system subject only to the ordinary optical/support availability rules. Pulse-derived representations are unavailable at the 5-min sensitivity state; this is one reason 5 min is excluded from the primary lattice.

Support is part of the estimand. Eye–chest and eye–wrist analyses retain their comparison-specific maximal supports. Unavailable representations are unavailable, not high distortion or insufficiency.

## 4. Pairwise representation change

For metric k and smallest analysis unit i:

    delta_ik(c_a,c_b) = value_ik(c_a) - value_ik(c_b)

for linear metrics. Circular-time metrics use the shortest signed circular difference. The canonical orientation is unique: for temporal resolution, state_a is coarser/less demanding and state_b is finer/more demanding; for duration, state_a is shorter and state_b is longer. Placement/optical pairs are explicit unordered facets, not burden-ordered candidate/reference states.

Within comparison lattice g:

    z_ik(c_a,c_b) = delta_ik(c_a,c_b) / s_kg

The comparison pair and scale anchor are different objects. One standardizer is joined to every pair in a lattice. Primary scale is SD; IQR/1.349 is sensitivity.

The complete empirical distribution D_k(c_a,c_b) is primary. A = mean(abs(z)) and B = mean(z) satisfy A >= |B|. For ordered adjacent states, G = A is the local configuration response.

## 5. Monitoring duration

The primary duration domain is 1–6 complete analysis days. After common core preprocessing, identify consecutive complete-analysis-day runs separately for every participant and support. Enumerate every contiguous 1–6 day window in every run. Runs longer than six days contribute all legal windows; no optimal subset is selected.

trial_times metadata remains in unit_context for audit, descriptive metadata and sensitivity. It does not define primary eligibility and does not create a protocol seven-day reference.

Daily-defined metric windows are aggregated from the durable daily metric cube using the existing semantics: arithmetic aggregation for linear metrics and circular aggregation for circular-time metrics. IS/IV are rebuilt on exact selected dates from the stored hourly basis.

## 6. RQ2 conditionality and separability

RQ2 reads the RQ1 pairwise representation-change artifact. Primary conditional transitions are placement, optical, adjacent primary temporal levels and adjacent nested duration windows. Exposure state is explanatory, not a metric-specific re-ranking.

Contextual explanation is organized into four prespecified layers without redefining measurement configurations or target representations:

- **External opportunity:** personal-measurement-independent weather/solar opportunity from the existing ERA5/unit-context pipeline, including radiation, direct fraction, cloud level/variability, solar-noon elevation, civil photoperiod, temperature and wet hours.
- **Micro-environment:** time-weighted reported outdoor exposure and primary daylight/display-light fractions from the harmonized MeLiDos light-exposure diary.
- **Behaviour:** time-weighted home/work/vehicle fractions plus the harmonized work/free-day, exercise-intensity and prior-sleep fields.
- **Exposure state:** the transition-local target-aligned exposure state already defined below, plus duration day-to-day variability where estimable.

Layer-specific models quantify each information source separately; the joint model includes the usable predictors from all four layers. Predictors are standardized on the fitting data. Participant-grouped cross-validation preserves all rows belonging to a held-out participant. The diary-derived context follows the existing MeLiDos interval and category conventions; ERA5 variables are consumed from the existing core context and are not re-derived under RQ2-specific weather rules.

Duration exposure state is local and low leakage: the longer-window exposure level, represented by log absolute longer-window mean MEDI, together with day-to-day variability in the shorter window before adding the new day. The target change itself is not used as a predictor. Duration contextual predictors are aggregated over the exact member dates of the longer observed window.

For actual four-cell cross-dimensional contrasts:

    gamma = Delta_b Delta_a M

linear metrics use ordinary second differences. Circular metrics use a circular-aware first difference and a circular-aware second difference. Summaries are R = mean(gamma), Q = mean(abs(gamma)), with Q >= |R|. Primary dimension pairs are placement x optical, placement x temporal and optical x temporal; duration enters multidimensional stability directly in RQ3.

## 7. RQ3 observed stability and sufficiency

For an ordered state c:

    R_obs_k(c) = max A_k(c,c')

over all higher observed states c'. If no higher observed state exists:

    n_higher_observed = 0
    R_obs = NA
    status = boundary_unresolved

No boundary is assigned zero. Boundary convergence is described by the adjacent G sequence.

For each epsilon, a resolved state is observed sufficient when R_obs <= epsilon. The full sufficient set is retained. A least-demanding requirement is reported only when the observed set is threshold-like; no monotonicity is imposed.

Placement and optical are empirical substitutability facets: a pair is substitutable at epsilon when its A <= epsilon.

## 8. Multidimensional stability and Pareto

Multidimensional states are actual rows from duration_metric_cube. Within fixed placement x optical facets, temporal resolution and duration are the only ordered burden dimensions. A higher joint state has no coarser temporal requirement and no shorter duration, with at least one strict increase in burden.

Joint R_obs is the maximum A over dominant higher observed joint configurations. Joint upper boundaries are boundary_unresolved with R_obs = NA.

Pareto dominance is calculated only inside the observed sufficient region. Finer temporal resolution is higher burden; longer monitoring is higher burden. In addition to ever_pareto and fraction_metrics_ever_pareto, report deterministic Pareto persistence/occupancy as tolerance-domain width or proportion over the pre-defined epsilon breakpoint domain. Pareto results are interpreted as a **minimum-sufficient burden frontier within the frozen candidate domain**, not as an unconstrained accuracy-versus-burden optimum. No subjective probability weighting is used.

## 9. Figures and implementation invariants

Plot scripts read frozen results artifacts only. They do not read raw series, call LightLogR operators, construct duration windows, bootstrap, refit models, calculate gamma or calculate sufficiency.

The primary temporal lattice, primary duration domain, labels and analysis-design identifier are defined once in `scripts/utils/analysis_design.R` and consumed by core, RQ1-RQ3 and canonical plot wrappers. A lattice change therefore changes artifact identities and cannot silently reuse stale downstream caches.

Core invariants include exact sparse source subsets, primary states restricted to 10–120 s, 5 min reserved for coarse sensitivity, no 15-s state, unique duration window membership, unique scientific keys, unavailable/high-distortion separation, A >= |B|, Q >= |R| and explicit unresolved boundaries.
