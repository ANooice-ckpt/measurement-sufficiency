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

    # The joint scale anchor must remain the frozen high-information state,
    # eye / MEDI / 10 s / 6 d. Pooling chest/wrist or LIGHT into the denominator
    # would make the standardized joint distortion depend on the alternatives.
    old_anchor = 'filter(resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>'
    new_anchor = 'filter(placement == "eye", optical == "MEDI", resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>'
    text = replace_once(text, old_anchor, new_anchor, "RQ3 joint anchor patch")

    # Unavailable state representations are not observed joint states.
    old_states = '''state_parts[[i]] <- z |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)'''
    new_states = '''state_parts[[i]] <- z |>
    filter(available, is.finite(value)) |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)'''
    text = replace_once(text, old_states, new_states, "RQ3 available joint-state catalogue patch")

    # A state is analysable only when its support/metric anchor dispersion is
    # defined. Otherwise it is unavailable, not a false upper boundary.
    old_catalog = '''joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))'''
    new_catalog = '''joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
  filter(is.finite(standardizer), standardizer > 0) |>
  select(-standardizer) |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))'''
    text = replace_once(text, old_catalog, new_catalog, "RQ3 analysable joint-state catalogue patch")

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
