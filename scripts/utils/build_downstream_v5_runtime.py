#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "results" / "runtime"
OUT.mkdir(parents=True, exist_ok=True)


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one anchor, found {n}")
    return text.replace(old, new, 1)


def copy_runtime(src_name: str, out_name: str, transform=None):
    src = ROOT / "scripts" / src_name
    text = src.read_text(encoding="utf-8")
    if transform is not None:
        text = transform(text)
    dest = OUT / out_name
    dest.write_text(text, encoding="utf-8")
    print(dest.relative_to(ROOT))


def patch_rq2(text: str) -> str:
    # ends_with("_b") would also remove scientific keys such as window_id_b,
    # n_days_b and analysis_unit_id_b. Drop only temporary predictor aliases.
    old = 'select(-longer_window_level, -longer_window_variability, -ends_with("_b"), -shorter_window_variability)'
    new = '''select(
      -longer_window_level, -longer_window_variability,
      -external_radiation_b, -external_direct_fraction_b, -external_cloud_b,
      -solar_noon_elevation_deg_b, -shorter_window_variability
    )'''
    return replace_once(text, old, new, "RQ2 temporary duration-column patch")


def patch_rq3(text: str) -> str:
    # Base merge() on tibbles does not accept data.table's allow.cartesian.
    # dplyr's explicit many-to-many join is the intended Cartesian product
    # inside each already-fixed participant/support/metric group.
    old = 'merge(a, b, by = c("support_id", "site", "Id"), allow.cartesian = TRUE) |>'
    new = 'inner_join(a, b, by = c("support_id", "site", "Id"), relationship = "many-to-many") |>'
    text = replace_once(text, old, new, "RQ3 Cartesian join patch")

    # Fig. 4d is a pair-level fraction across metrics. Grouping by metric_class
    # creates duplicate epsilon rows that the plot then incorrectly connects as
    # one line, so metric class must not define the coverage estimand.
    old_cov = 'group_by(dimension, comparison_pair_id, config_a_id, config_b_id, metric_class) |>'
    new_cov = 'group_by(dimension, comparison_pair_id, config_a_id, config_b_id) |>'
    text = replace_once(text, old_cov, new_cov, "RQ3 unordered coverage grouping patch")
    return text


copy_runtime("12_rq2_analysis_v5.R", "12_rq2_analysis_v5.runtime.R", patch_rq2)
copy_runtime("13_plot_rq2_v5.R", "13_plot_rq2_v5.runtime.R")
copy_runtime("14_rq3_analysis_v5.R", "14_rq3_analysis_v5.runtime.R", patch_rq3)
copy_runtime("15_plot_rq3_v5.R", "15_plot_rq3_v5.runtime.R")
print("Downstream v5 runtime generated")
