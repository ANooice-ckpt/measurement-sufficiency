from pathlib import Path

fig1_path = Path("scripts/11_plot_fig1.R")
fig1 = fig1_path.read_text(encoding="utf-8")

if '"base_config_id"' not in fig1:
    old = '''rank_group_vars <- c(\n  "dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id",\n  "config_a_label", "config_b_label", "orientation_type", "orientation_basis",\n  "metric", "metric_class", "metric_geometry"\n)'''
    new = '''rank_group_vars <- c(\n  "dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id",\n  "config_a_label", "config_b_label", "orientation_type", "orientation_basis", "base_config_id",\n  "metric", "metric_class", "metric_geometry"\n)'''
    if old not in fig1:
        raise RuntimeError("rank_group_vars anchor not found")
    fig1 = fig1.replace(old, new, 1)

    old = '''    mutate(\n      dimension = as.character(dimension),\n      comparison_pair_id = if_else('''
    new = '''    mutate(\n      dimension = as.character(dimension),\n      base_config_id = if_else(\n        dimension == "duration",\n        sub("^(.*)__([^|]+\\\\|.*)$", "\\\\1", as.character(config_a_id)),\n        NA_character_\n      ),\n      comparison_pair_id = if_else('''
    if old not in fig1:
        raise RuntimeError("rank_fragment mutate anchor not found")
    fig1 = fig1.replace(old, new, 1)
    fig1 = fig1.replace(
        'rank_estimand = "Spearman correlation across paired RQ1 analysis units"',
        'rank_estimand = "Spearman correlation across paired RQ1 analysis units within comparison/base configuration"',
        1,
    )
    fig1_path.write_text(fig1, encoding="utf-8")

fig4_path = Path("scripts/15_plot_rq3.R")
fig4 = fig4_path.read_text(encoding="utf-8")
fig4 = fig4.replace('  geom_area(fill = "#D9DDE0", alpha = .62, position = "identity") +\n', '', 1)
fig4 = fig4.replace('labs(x = "tolerance ε", y = "resolved\\ncoverage")',
                    'labs(x = "tolerance ε", y = "sufficient\\ncoverage")', 1)
fig4 = fig4.replace('file.path("results", "rq3", "fig4_resolved_coverage.csv")',
                    'file.path("results", "rq3", "fig4_sufficient_metric_coverage.csv")', 1)
fig4_path.write_text(fig4, encoding="utf-8")

# Keep the figure contract precise about what the strip represents.
doc_path = Path("docs/FIGURE_ARCHITECTURE.md")
doc = doc_path.read_text(encoding="utf-8")
doc = doc.replace(
    "ordered dimensions, with a thin strip showing the fraction of metrics for which\n  a resolved sufficient state is observed within the candidate domain;",
    "ordered dimensions, with a thin strip showing the fraction of evaluable metrics\n  for which a sufficient resolved state is observed within the candidate domain;",
)
doc_path.write_text(doc, encoding="utf-8")

print("Figure correctness fix applied.")
