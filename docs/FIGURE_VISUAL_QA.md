# Fig. 2d participant-count continuation — 2026-10-11

The original 8.8-inch Fig. 2 composition is retained at its original physical
size. A 3.6-inch continuation below panel d compares frozen candidate total
signal displacement with reference sampling displacement, in eight configuration
rows and three domain columns. Panels a–c receive no new information layers.
Task-set consistency, rounded/repeated sample sizes, unavailable facets and the
10b → 14b --compensation → 11b execution order are covered by the existing
engineering test. A labelled synthetic Fig. 2d preview was rendered and inspected;
this checks layout, not real-data results. No production figures were overwritten.

# Main-figure visual review — 2026-10-02

The current pass standardizes Fig.1–6 typography, title bands, axes, legends and
aligned plot fields. It does not change scientific inputs, estimates, panel
identities or inferential rules. Existing plot helpers are reused; no new
production figure, analysis file or artifact is introduced.

- Shared physical sizes: 7.4-inch width; 8-pt panel titles in a 7-mm band;
  7-pt facet headings; 6.5-pt axes; 6.2-pt legends; 5.8-pt dense labels.
- Grey panel subtitles are removed. Necessary decoding information remains in
  axis labels, legends and dark notes; intentionally blank axes and legend
  titles stay blank.
- Fig.1: align both lower rows and their title baselines, plus the left axis
  gutter across the full-width and lower-left panels.
- Fig.2: assemble the two upper rows after aligning their actual fields; align
  the left plotting edges with panel d. Retain the two tracks within panel a.
- Fig.3: widen the predictor column, reduce strength-axis tick density and wrap
  colourbar titles; align the two non-additivity maps with the right column.
- Fig.4: align upper fields; enlarge atlas labels, replace display underscores
  with spaces and wrap metric-class labels without altering metric identities.
- Fig.5: share a compact interval/duration colour key; simplify half-width
  axis ticks and carry units in the axis title. Keep unresolved-boundary and
  open-marker explanations in short captions.
- Fig.6: use matching 30:70 column widths and aligned edges across rows; retain
  the existing frontier comparison and D/S blocker annotations.

Validation boundary: all six layouts were rendered with synthetic summaries
through the current plotting expressions/helpers and inspected visually. Every
temporary preview was explicitly labelled as a synthetic layout check, not a
study result. Real predictor and metric names were used to check long labels.
The local frozen analysis chain is incomplete/outdated (including missing
current core cubes and joint RQ3 input). Canonical production PNGs were therefore
not overwritten, and this is not a claim of final real-data figure validation.
Full R parsing, engineering/registry checks and RQ3 support/Pareto checks cover
the updated code. Production rendering still requires the current frozen inputs.

## Historical review — 2026-09-15

Scope: canonical Fig.1–6 plotting entrypoints and display helpers only. Frozen
analysis inputs, analytical supports, scores, uncertainty estimates and scientific
colour mappings are retained. No analysis entrypoint or model was run.

- Fig.1: reduce empty header space above the lower panels; reserve matching
  bottom space for their axis titles and the placement/optical symbol legend;
  increase panel-heading readability and separate title/subtitle baselines.
- Fig.2: align the long left plotting field with the right panels while retaining
  the two-line left x-axis title and a common top title baseline.
- Fig.3: retain the accepted a/c baseline and 6.88-inch height; reduce the narrow
  strength-axis tick font slightly to avoid collision with the next column.
- Fig.4: harmonize heading and subtitle sizes; preserve the aligned upper fields,
  full-width lower scatterplots and the accepted 6.56-inch height.
- Fig.5: enlarge small legend labels and panel headings; shorten the c heading;
  retain the requested removal of historical recovery curves.
- Fig.6: harmonize headings and neutral text colour; reduce oversized header
  bands and return that space to the plotting fields. All grid labels, Pareto
  bubble areas, unresolved states and cadence profiles are retained.

All six main figures are rendered from their canonical entrypoints. Windows
entrypoints explicitly request UTF-8 to render Greek letters and arrows
consistently. Final output is inspected for text collision, clipping, legend
readability and panel boundaries; the figure-registry contract is checked.
