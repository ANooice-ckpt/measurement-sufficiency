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
Installed reproduction version: 0.10.3.

Zauner release `v0.9.9` records LightLogR 0.10.3 in `renv.lock`; the earlier 0.10.0 entry was an unverified placeholder.

## Zauner et al.
Reference implementation:
`external/zauner_position/`

Checked-out reference: tag `v0.9.9`, commit `a74ec2acc84258ce87cc85b196f71b3a651522c4` (sparse checkout of reproduction inputs).

Use this repository to reproduce:
- preprocessing
- completeness rules
- definitions of the 54 metrics
- transformations
- metric classes

Do not modify files under `external/`.

## Environment boundary

The Zauner `v0.9.9` lockfile records R 4.5.0. The available local runtime is R 4.4.2, so the project uses a local renv environment with the same core LightLogR and melidosData versions but is not represented as a byte-for-byte restoration of the upstream runtime.
