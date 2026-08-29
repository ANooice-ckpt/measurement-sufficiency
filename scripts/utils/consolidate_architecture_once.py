#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import re
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
SCRIPTS = ROOT / "scripts"
RUNTIME = ROOT / "results" / "runtime"


def run(*args: str) -> None:
    subprocess.run(args, cwd=ROOT, check=True)


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write(path: Path, text: str) -> None:
    path.write_text(text, encoding="utf-8")


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one anchor, found {n}")
    return text.replace(old, new, 1)


def inline_plot(wrapper_name: str, v5_name: str) -> None:
    wrapper_path = SCRIPTS / wrapper_name
    v5_path = SCRIPTS / v5_name
    wrapper = read(wrapper_path)
    v5 = read(v5_path).rstrip() + "\n"
    source_line = f'source(file.path("scripts", "{v5_name}"), local = .GlobalEnv)'
    wrapper = replace_once(wrapper, source_line, v5, f"inline {v5_name}")
    # The canonical wrapper used the legacy v5 file only as a root sentinel.
    wrapper = wrapper.replace(
        f'file.exists(file.path(.ms_root, "scripts", "{v5_name}"))',
        'file.exists(file.path(.ms_root, "scripts", "utils", "figure_style.R"))',
    )
    wrapper = wrapper.replace(
        f'Could not resolve measurement-sufficiency repository root from ',
        'Could not resolve measurement-sufficiency repository root from ',
    )
    wrapper = wrapper.replace(
        "# Canonical RQ2 plotting entrypoint. Plotting consumes frozen v5 outputs only;\n# unlike the analysis runtime, the plot source requires no deployment-time patch.\n",
        "# Canonical RQ2 plotting source. All accepted display refinements are consolidated here.\n",
    )
    wrapper = wrapper.replace(
        "# Canonical RQ3 plotting entrypoint. Plotting consumes frozen v5 outputs only;\n# unlike the analysis runtime, the plot source requires no deployment-time patch.\n",
        "# Canonical RQ3 plotting source. All accepted display refinements are consolidated here.\n",
    )
    write(wrapper_path, wrapper)


# ---------------------------------------------------------------------------
# 1. Generate the exact corrected downstream runtimes from the currently
#    validated source + patch chain. These generated files are the behavioral
#    source of truth for consolidation; no scientific transformation is added.
# ---------------------------------------------------------------------------
run("python3", "scripts/utils/build_downstream_v5_runtime.py")
run("python3", "scripts/utils/patch_rq2_stream_runtime.py")
run("python3", "scripts/utils/patch_rq2_context_stream_runtime.py")

rq2_runtime = read(RUNTIME / "12_rq2_analysis_v5.runtime.R").rstrip() + "\n"
rq2_context_runtime = read(RUNTIME / "12c_rq2_context_models.runtime.R").rstrip() + "\n"
rq3_runtime = read(RUNTIME / "14_rq3_analysis_v5.runtime.R").rstrip() + "\n"

# ---------------------------------------------------------------------------
# 2. RQ2: preserve the current canonical orchestration exactly, but inline the
#    generated corrected base runtime and point the layered stage at its now-
#    patched canonical source. Base legacy fits remain intentionally disabled;
#    the layered contextual fits remain the sole model stage when requested.
# ---------------------------------------------------------------------------
rq2_wrapper_path = SCRIPTS / "12_rq2_analysis.R"
rq2_wrapper = read(rq2_wrapper_path)
marker = ".requested_rq2_models <-"
pos = rq2_wrapper.find(marker)
if pos < 0:
    raise RuntimeError("RQ2 wrapper orchestration marker not found")
rq2_canonical = rq2_wrapper[pos:]
rq2_canonical = replace_once(
    rq2_canonical,
    'source(file.path("results", "runtime", "12_rq2_analysis_v5.runtime.R"), local = .GlobalEnv)',
    rq2_runtime,
    "inline corrected RQ2 runtime",
)
rq2_canonical = replace_once(
    rq2_canonical,
    'source(file.path("results", "runtime", "12c_rq2_context_models.runtime.R"), local = .GlobalEnv)',
    'source(file.path("scripts", "12c_rq2_context_models.R"), local = .GlobalEnv)',
    "canonical layered RQ2 source",
)
rq2_header = """# Canonical RQ2 analysis.\n# The former v5 source and deployment-time patches have been consolidated into\n# this file. The layered contextual model remains a separate scientific module\n# and is sourced in the same R process so it reuses the validated transition\n# objects without reconstructing RQ1/core data.\n\n"""
write(rq2_wrapper_path, rq2_header + rq2_canonical)
write(SCRIPTS / "12c_rq2_context_models.R", rq2_context_runtime)

# RQ3 has no post-runtime orchestration; the generated corrected runtime is the
# exact canonical analysis source.
write(
    SCRIPTS / "14_rq3_analysis.R",
    "# Canonical RQ3 analysis; former v5 runtime patches are consolidated here.\n" + rq3_runtime,
)

# ---------------------------------------------------------------------------
# 3. Plotting: inline each historical v5 plotting source into the current
#    canonical wrapper, retaining all later display refinements verbatim.
# ---------------------------------------------------------------------------
inline_plot("13_plot_rq2.R", "13_plot_rq2_v5.R")
inline_plot("15_plot_rq3.R", "15_plot_rq3_v5.R")

# Rename the real RQ3 recovery utility; it is not a duplicate implementation.
resume_old = SCRIPTS / "resume_rq3_v5_after_joint.R"
resume_new = SCRIPTS / "resume_rq3_after_joint.R"
if resume_old.exists():
    shutil.copyfile(resume_old, resume_new)

# ---------------------------------------------------------------------------
# 4. Consolidate the server downstream runner. Start from the already-used v5
#    runner and replace only the runtime-generation/legacy calls that no longer
#    exist after source consolidation.
# ---------------------------------------------------------------------------
runner = read(SCRIPTS / "run_downstream_v5_server.sh")
start = runner.index('  echo "===== BUILD/PARSE CORRECTED ANALYSIS RUNTIMES + PLOT SOURCES ====="')
end = runner.index('  echo "===== STRUCTURAL PREFLIGHT ====="')
parse_block = '''  echo "===== PARSE CANONICAL DOWNSTREAM SOURCES ====="\n  Rscript --vanilla -e '\n    fs <- c(\n      "scripts/utils/analysis_design.R",\n      "scripts/utils/rq_context.R",\n      "scripts/utils/rq2_context_features.R",\n      "scripts/12_rq2_analysis.R",\n      "scripts/12c_rq2_context_models.R",\n      "scripts/13_plot_rq2.R",\n      "scripts/14_rq3_analysis.R",\n      "scripts/15_plot_rq3.R"\n    )\n    invisible(lapply(fs, parse))\n    cat("All canonical downstream analysis and plotting sources parse successfully\\n")\n  '\n\n'''
runner = runner[:start] + parse_block + runner[end:]
runner = runner.replace(
    '  echo "===== RQ2 V5 + LAYERED CONTEXT ====="',
    '  echo "===== RQ2 + LAYERED CONTEXT ====="',
)
runner = runner.replace('  echo "===== RQ2 V5 FIGURES ====="', '  echo "===== RQ2 FIGURES ====="')
runner = runner.replace('  echo "===== RQ3 V5 ====="', '  echo "===== RQ3 ====="')
runner = runner.replace(
    '  Rscript results/runtime/14_rq3_analysis_v5.runtime.R',
    '  Rscript scripts/14_rq3_analysis.R',
)
runner = runner.replace('  echo "===== RQ3 V5 FIGURES ====="', '  echo "===== RQ3 FIGURES ====="')
runner = runner.replace(
    '# Canonical server downstream entrypoint. The corrected v5 runner contains the\n# duration-type RQ2 projection, full-row grouped CV, streamed model inputs,\n# type-level RQ3 stability, nested joint duration comparisons and corrected\n# Pareto burden direction. Keep one public entrypoint so future runs cannot\n# accidentally fall back to the retired v4 scripts.\n',
    '# Canonical server downstream entrypoint. Downstream corrected implementations\n# are consolidated directly in the public RQ2/RQ3 sources; no generated v5 runtime\n# layer is required.\n',
)
write(SCRIPTS / "run_downstream_server.sh", runner)

# Optimized full runner still generates optimized core/RQ1 runtimes, but no
# longer generates or executes a second downstream runtime layer.
opt_path = SCRIPTS / "run_full_server_optimized.sh"
opt = read(opt_path)
opt = opt.replace(
    '  echo "Generate runtime-only optimized RQ1 and corrected downstream v5 entrypoints"\n',
    '  echo "Generate runtime-only optimized core/RQ1 entrypoints; downstream uses canonical sources"\n',
)
opt = opt.replace('  python3 scripts/utils/build_downstream_v5_runtime.py\n', '')
opt = opt.replace('      "results/runtime/12_rq2_analysis_v5.runtime.R",\n', '')
opt = opt.replace('      "results/runtime/14_rq3_analysis_v5.runtime.R",\n', '')
opt = opt.replace('      "scripts/13_plot_rq2_v5.R",\n', '')
opt = opt.replace('      "scripts/15_plot_rq3_v5.R"\n', '      "scripts/14_rq3_analysis.R",\n      "scripts/15_plot_rq3.R"\n')
opt = opt.replace('  echo "===== RQ2 V5 + LAYERED CONTEXT ====="', '  echo "===== RQ2 + LAYERED CONTEXT ====="')
opt = opt.replace(
    '  # Canonical wrapper sources the corrected runtime and the layered extension in\n  # one R process, so all context models reuse the same transition objects.\n',
    '  # Canonical RQ2 sources the layered extension in the same R process, so all\n  # context models reuse the same transition objects.\n',
)
opt = opt.replace('  echo "===== RQ2 V5 FIGURES ====="', '  echo "===== RQ2 FIGURES ====="')
opt = opt.replace('  echo "===== RQ3 V5 ====="', '  echo "===== RQ3 ====="')
opt = opt.replace('  Rscript results/runtime/14_rq3_analysis_v5.runtime.R', '  Rscript scripts/14_rq3_analysis.R')
opt = opt.replace('  echo "===== RQ3 V5 FIGURES ====="', '  echo "===== RQ3 FIGURES ====="')
write(opt_path, opt)

# ---------------------------------------------------------------------------
# 5. CI and lightweight documentation references.
# ---------------------------------------------------------------------------
ci_path = ROOT / ".github" / "workflows" / "r-syntax.yml"
ci = read(ci_path)
for line in [
    '            "scripts/10b_rq1_context_analysis.R",\n',
    '            "scripts/12_rq2_analysis_v5.R",\n',
    '            "scripts/12b_rq2_context_analysis.R",\n',
    '            "scripts/13_plot_rq2_v5.R",\n',
    '            "scripts/14_rq3_analysis_v5.R",\n',
    '            "scripts/15_plot_rq3_v5.R",\n',
    '            "scripts/resume_rq3_v5_after_joint.R"\n',
]:
    ci = ci.replace(line, '')
ci = ci.replace(
    '            "scripts/15_plot_rq3.R",\n',
    '            "scripts/15_plot_rq3.R",\n            "scripts/resume_rq3_after_joint.R"\n',
)
ci = re.sub(
    r'\n      - name: Parse runtime patch builder\n        run: python3 -m py_compile scripts/utils/build_downstream_v5_runtime\.py\n',
    '\n',
    ci,
)
ci = ci.replace('            scripts/run_downstream_v5_server.sh \\\n', '')
write(ci_path, ci)

readme_path = ROOT / "README.md"
readme = read(readme_path)
readme = readme.replace(
    "The canonical RQ2 entrypoint first executes the corrected v5 runtime and then, in the same R process, adds the layered contextual models.",
    "The canonical RQ2 entrypoint contains the corrected streamed analysis directly and then, in the same R process, adds the layered contextual models.",
)
readme = readme.replace(
    "The server runners define their own high-core-count defaults and every worker count remains environment-overridable; inspect the selected runner before changing concurrency for a different instance size.",
    "The server runners define their own high-core-count defaults and every worker count remains environment-overridable; inspect the selected runner before changing concurrency for a different instance size. Downstream RQ2/RQ3 sources are canonical and are no longer generated through a v5 runtime-patch layer.",
)
write(readme_path, readme)

# ---------------------------------------------------------------------------
# 6. Remove retired compatibility shells, duplicate v5 sources, and downstream
#    runtime patch machinery now represented exactly in canonical sources.
# ---------------------------------------------------------------------------
remove_paths = [
    "scripts/10b_rq1_context_analysis.R",
    "scripts/10b_rq1_context_analysis_windows_psock.R",
    "scripts/10c_rq1_context_finalize.R",
    "scripts/12b_rq2_context_analysis.R",
    "scripts/12_rq2_analysis_v5.R",
    "scripts/13_plot_rq2_v5.R",
    "scripts/14_rq3_analysis_v5.R",
    "scripts/15_plot_rq3_v5.R",
    "scripts/resume_rq3_v5_after_joint.R",
    "scripts/run_downstream_v5_server.sh",
    "scripts/utils/build_downstream_v5_runtime.py",
    "scripts/utils/patch_rq2_stream_runtime.py",
    "scripts/utils/patch_rq2_context_stream_runtime.py",
]
for rel in remove_paths:
    p = ROOT / rel
    if p.exists():
        p.unlink()

# The one-shot helper and workflow must not become part of the maintained tree.
for rel in [
    "scripts/utils/consolidate_architecture_once.py",
    ".github/workflows/architecture-cleanup-once.yml",
]:
    p = ROOT / rel
    if p.exists():
        p.unlink()

print("Architecture consolidation complete")
