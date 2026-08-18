# Upstream sources

## MeLiDos
Data loader: melidosData 1.0.6

Use harmonized 10-s datasets as the source layer.

Required modalities:
- light_glasses
- light_chest
- light_wrist
- wearlog
- sleepdiaries

Do not use pre-aggregated 1-minute datasets for primary analyses because temporal compression must be generated from the same 10-s reference.

## LightLogR
Primary version: 0.10.0

Reason: match the Zauner et al. analysis environment as closely as possible.

## Zauner et al.
Reference implementation:
`external/zauner_position/`

Use this repository to reproduce:
- preprocessing
- completeness rules
- definitions of the 54 metrics
- transformations
- metric classes

Do not modify files under `external/`.
