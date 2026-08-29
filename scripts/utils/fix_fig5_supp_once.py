from pathlib import Path

p = Path('scripts/15_plot_rq3.R')
text = p.read_text(encoding='utf-8')
old = '''pareto_base <- pareto |>
  filter(is.finite(resolution_s), is.finite(n_days)) |>
  mutate(
    ever_pareto = as.logical(ever_pareto),
    pareto_persistence = pmax(0, pmin(1, pareto_persistence)),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )
'''
new = '''pareto_base <- pareto |>
  filter(is.finite(resolution_s), is.finite(n_days)) |>
  mutate(
    ever_pareto = as.logical(ever_pareto),
    pareto_persistence = pmax(0, pmin(1, pareto_persistence)),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    resolution_rank = match(resolution_s, RES_LEVELS)
  ) |>
  filter(is.finite(resolution_rank))
'''
if old not in text:
    raise SystemExit('Fig5 pareto_base anchor not found')
p.write_text(text.replace(old, new, 1), encoding='utf-8')
