# Data inventory

Generated on 2026-08-19 02:59:53 by `scripts/02_inventory.R`.

Machine-readable outputs: `logs/data_inventory.csv` and `logs/sample_intersections.csv`.

## Site × modality

| site | modality | n_participants | n_rows | date_min | date_max | n_participant_days | missing_numeric_fraction | median_epoch_seconds |
|---|---|---|---|---|---|---|---|---|
| BAUA | light_chest | 22 | 1408320 | 2025-06-10 | 2025-10-20 23:59:50 | 185 | 0.144577227 | 10 |
| BAUA | light_glasses | 19 | 1252800 | 2025-06-10 00:00:02 | 2025-10-20 23:59:58 | 164 | 0.133171296 |  3 |
| BAUA | light_wrist | 19 | 1252800 | 2025-06-10 | 2025-10-20 23:59:50 | 164 | 0.136610792 | 10 |
| BAUA | sleepdiaries | 22 |     149 | NA | NA | NA | 0.000000000 | NA |
| BAUA | wearlog | 20 |     234 | NA | NA | NA | 0.629629630 | NA |
| FUSPCEU | light_chest | 22 | 1504080 | 2024-10-07 00:00:05 | 2025-02-10 23:59:56 | 196 | 0.132270225 |  5 |
| FUSPCEU | light_glasses | 23 | 1522800 | 2024-10-07 00:00:03 | 2025-02-10 23:59:55 | 205 | 0.131677830 |  5 |
| FUSPCEU | light_wrist | 23 | 1555920 | 2024-10-07 00:00:04 | 2025-02-10 23:59:54 | 203 | 0.133411101 |  5 |
| FUSPCEU | sleepdiaries | 23 |     149 | NA | NA | NA | 0.000000000 | NA |
| FUSPCEU | wearlog | 19 |     151 | NA | NA | NA | 0.600441501 | NA |
| IZTECH | light_chest | 17 | 1183680 | 2024-12-23 00:00:06 | 2025-06-02 23:59:59 | 154 | 0.339075172 |  8 |
| IZTECH | light_glasses | 17 | 1183680 | 2024-12-23 | 2025-06-02 23:59:50 | 154 | 0.340938641 | 10 |
| IZTECH | light_wrist | 17 | 1120320 | 2024-12-23 00:00:06 | 2025-06-02 23:59:57 | 154 | 0.337886274 |  8 |
| IZTECH | sleepdiaries | 17 |     137 | NA | NA | NA | 0.000000000 | NA |
| IZTECH | wearlog | 17 |     199 | NA | NA | NA | 0.690117253 | NA |
| KNUST | light_chest | 15 | 1019520 | 2024-10-07 00:00:08 | 2025-02-03 23:59:50 | 118 | 0.160523580 | 10 |
| KNUST | light_glasses | 15 | 1028160 | 2024-10-07 00:00:02 | 2025-02-03 23:59:50 | 119 | 0.164859474 | 10 |
| KNUST | light_wrist | 15 | 1036800 | 2024-10-07 00:00:01 | 2025-02-03 23:59:55 | 120 | 0.151009838 | 10 |
| KNUST | sleepdiaries | 15 |     104 | NA | NA | NA | 0.000000000 | NA |
| KNUST | wearlog | 15 |     164 | NA | NA | NA | 0.518292683 | NA |
| MPI | light_glasses | 26 | 1797840 | 2023-08-14 00:00:01 | 2023-11-13 23:59:56 | 234 | 0.149339207 |  3 |
| MPI | sleepdiaries | 26 |     182 | NA | NA | NA | 0.005494505 | NA |
| MPI | wearlog | 26 |     372 | NA | NA | NA | 0.629032258 | NA |
| RISE | light_chest | 17 | 1183320 | 2025-03-03 00:00:06 | 2025-10-20 23:59:52 | 154 | 0.142269209 |  6 |
| RISE | light_glasses | 14 | 1079640 | 2025-03-03 00:00:07 | 2025-10-13 23:59:52 | 121 | 0.186485310 | 10 |
| RISE | light_wrist | 17 | 1166040 | 2025-03-03 | 2025-10-20 23:59:53 | 152 | 0.143990772 | 10 |
| RISE | sleepdiaries | 17 |     117 | NA | NA | NA | 0.002849003 | NA |
| RISE | wearlog | 16 |     185 | NA | NA | NA | 0.645045045 | NA |
| THUAS | light_chest | 15 | 1079640 | 2025-02-21 00:00:06 | 2025-10-14 23:59:54 | 140 | 0.121697047 | 10 |
| THUAS | light_glasses | 13 |  924120 | 2025-02-21 | 2025-09-28 23:59:50 | 120 | 0.130439770 | 10 |
| THUAS | light_wrist | 13 |  915480 | 2025-03-17 | 2025-10-14 23:59:50 | 119 | 0.123004326 | 10 |
| THUAS | sleepdiaries | 15 |     101 | NA | NA | NA | 0.000000000 | NA |
| THUAS | wearlog | 12 |     123 | NA | NA | NA | 0.579945799 | NA |
| TUM | light_chest | 10 |  691200 | 2024-05-13 00:00:05 | 2024-07-29 23:59:50 |  90 | 0.152264178 | 10 |
| TUM | light_glasses | 10 |  691200 | 2024-05-13 00:00:03 | 2024-07-29 23:59:57 |  90 | 0.152264178 | 10 |
| TUM | light_wrist | 10 |  691200 | 2024-05-13 | 2024-07-29 23:59:50 |  90 | 0.152274803 | 10 |
| TUM | sleepdiaries | 10 |      70 | NA | NA | NA | 0.000000000 | NA |
| TUM | wearlog | 10 |     155 | NA | NA | NA | 0.595698925 | NA |
| UCR | light_chest | 39 | 2695680 | 2025-06-16 00:00:02 | 2025-09-07 23:59:57 | 351 | 0.140828288 |  2 |
| UCR | light_glasses |  6 |  414720 | 2025-06-16 | 2025-06-30 23:59:50 |  54 | 0.148579765 | 10 |
| UCR | light_wrist | 39 | 2738881 | 2025-06-16 | 2025-09-07 23:59:50 | 356 | 0.131686992 | 10 |
| UCR | sleepdiaries | 39 |     267 | NA | NA | NA | 0.000000000 | NA |
| UCR | wearlog | 39 |     510 | NA | NA | NA | 0.616339869 | NA |

## Placement participant intersections

| site | E∩C | E∩W | E∩C∩W |
|---|---:|---:|---:|
| BAUA | 19 | 16 | 16 |
| FUSPCEU | 22 | 23 | 22 |
| IZTECH | 17 | 17 | 17 |
| KNUST | 15 | 15 | 15 |
| MPI |  0 |  0 |  0 |
| RISE | 14 | 14 | 14 |
| THUAS | 13 | 11 | 11 |
| TUM | 10 | 10 | 10 |
| UCR |  6 |  6 |  6 |

MPI has no comparable ActLumus chest/wrist modalities. Intersections are descriptive only and are not imposed on later measurement operators.
