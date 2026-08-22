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


def patch_rq3(text: str) -> str:
    # Base merge() on tibbles does not accept data.table's allow.cartesian.
    # dplyr's explicit many-to-many join is the intended Cartesian product
    # inside each already-fixed participant/support/metric group.
    old = 'merge(a, b, by = c("support_id", "site", "Id"), allow.cartesian = TRUE) |>'
    new = 'inner_join(a, b, by = c("support_id", "site", "Id"), relationship = "many-to-many") |>'
    return replace_once(text, old, new, "RQ3 Cartesian join patch")


copy_runtime("12_rq2_analysis_v5.R", "12_rq2_analysis_v5.runtime.R")
copy_runtime("13_plot_rq2_v5.R", "13_plot_rq2_v5.runtime.R")
copy_runtime("14_rq3_analysis_v5.R", "14_rq3_analysis_v5.runtime.R", patch_rq3)
copy_runtime("15_plot_rq3_v5.R", "15_plot_rq3_v5.runtime.R")
print("Downstream v5 runtime generated")
