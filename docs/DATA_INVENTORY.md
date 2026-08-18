# Data inventory

Generated on 2026-08-19 02:08:30 by `scripts/02_inventory.R`.

Machine-readable outputs: `logs/data_inventory.csv` and `logs/sample_intersections.csv`.

## Site × modality

| site | modality | n_participants | n_rows | date_min | date_max | n_participant_days | missing_numeric_fraction | median_epoch_seconds |
|---|---|---|---|---|---|---|---|---|
| BAUA | light_glasses | 19 | 1252800 | 2025-06-10 00:00:02 | 2025-10-20 23:59:58 | 164 | 0.133171296 |  3 |
| BAUA | sleepdiaries | 22 |     149 | NA | NA | NA | 0.000000000 | NA |
| BAUA | wearlog | 20 |     234 | NA | NA | NA | 0.629629630 | NA |
| FUSPCEU | light_glasses | 23 | 1522800 | 2024-10-07 00:00:03 | 2025-02-10 23:59:55 | 205 | 0.131677830 |  5 |
| FUSPCEU | sleepdiaries | 23 |     149 | NA | NA | NA | 0.000000000 | NA |
| FUSPCEU | wearlog | 19 |     151 | NA | NA | NA | 0.600441501 | NA |
| IZTECH | sleepdiaries | 17 |     137 | NA | NA | NA | 0.000000000 | NA |
| IZTECH | wearlog | 17 |     199 | NA | NA | NA | 0.690117253 | NA |
| KNUST | light_glasses | 15 | 1028160 | 2024-10-07 00:00:02 | 2025-02-03 23:59:50 | 119 | 0.164859474 | 10 |
| KNUST | sleepdiaries | 15 |     104 | NA | NA | NA | 0.000000000 | NA |
| KNUST | wearlog | 15 |     164 | NA | NA | NA | 0.518292683 | NA |
| MPI | sleepdiaries | 26 |     182 | NA | NA | NA | 0.005494505 | NA |
| MPI | wearlog | 26 |     372 | NA | NA | NA | 0.629032258 | NA |
| RISE | sleepdiaries | 17 |     117 | NA | NA | NA | 0.002849003 | NA |
| RISE | wearlog | 16 |     185 | NA | NA | NA | 0.645045045 | NA |
| THUAS | sleepdiaries | 15 |     101 | NA | NA | NA | 0.000000000 | NA |
| THUAS | wearlog | 12 |     123 | NA | NA | NA | 0.579945799 | NA |
| TUM | light_chest | 10 |  691200 | 2024-05-13 00:00:05 | 2024-07-29 23:59:50 |  90 | 0.152264178 | 10 |
| TUM | light_glasses | 10 |  691200 | 2024-05-13 00:00:03 | 2024-07-29 23:59:57 |  90 | 0.152264178 | 10 |
| TUM | light_wrist | 10 |  691200 | 2024-05-13 | 2024-07-29 23:59:50 |  90 | 0.152274803 | 10 |
| TUM | sleepdiaries | 10 |      70 | NA | NA | NA | 0.000000000 | NA |
| TUM | wearlog | 10 |     155 | NA | NA | NA | 0.595698925 | NA |
| UCR | sleepdiaries | 39 |     267 | NA | NA | NA | 0.000000000 | NA |
| UCR | wearlog | 39 |     510 | NA | NA | NA | 0.616339869 | NA |

## Placement participant intersections

| site | E∩C | E∩W | E∩C∩W |
|---|---:|---:|---:|
| BAUA |  0 |  0 |  0 |
| FUSPCEU |  0 |  0 |  0 |
| IZTECH |  0 |  0 |  0 |
| KNUST |  0 |  0 |  0 |
| MPI |  0 |  0 |  0 |
| RISE |  0 |  0 |  0 |
| THUAS |  0 |  0 |  0 |
| TUM | 10 | 10 | 10 |
| UCR |  0 |  0 |  0 |

MPI has no comparable ActLumus chest/wrist modalities. Intersections are descriptive only and are not imposed on later measurement operators.
