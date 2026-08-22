suppressPackageStartupMessages({library(tidyverse); library(nlme)})
source("scripts/utils/paths.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

# RQ2 consumes the general RQ1 pairwise representation-change object. Context
# is explanatory only: models never redefine the core representation values.
RQ1_PAIRWISE <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
CORE_METRICS <- file.path("results", "core", "metric_cube.csv.gz")
CORE_CONTEXT <- file.path("results", "core", "unit_context.csv.gz")
DURATION_CUBE <- file.path("results", "core", "duration_metric_cube.rds")
OUT <- file.path("results", "rq2")
DIAG <- file.path("results", "diagnostics")
CHECKPOINTS <- file.path(OUT, "checkpoints")
RUN_MODELS <- !identical(Sys.getenv("RQ2_RUN_MODELS", unset = "1"), "0")
RQ2_CV_FOLDS <- suppressWarnings(as.integer(Sys.getenv("RQ2_CV_FOLDS", unset = "5")))
if (!is.finite(RQ2_CV_FOLDS) || RQ2_CV_FOLDS < 2L) RQ2_CV_FOLDS <- 5L
RQ2_WORKERS <- ms_resolve_workers("RQ2_WORKERS", default = 1L, cap = 48L)
MODEL_SEED <- 20260821L
TEMPORAL_GAMMA_S <- c(10L, 20L, 30L, 60L, 300L, 900L, 1800L)
ensure_result_dirs(OUT, DIAG, CHECKPOINTS)
for (p in c(RQ1_PAIRWISE, CORE_METRICS, CORE_CONTEXT, DURATION_CUBE)) if (!file.exists(p)) stop("Missing RQ2 input: ", p)

rq1_artifact <- readRDS(RQ1_PAIRWISE)
rq1_columns <- c(
  "core_artifact_version", "rq1_analysis_version", "pair_key", "dimension", "comparison_lattice",
  "comparison_pair_id", "config_a_id", "config_b_id", "config_a_label", "config_b_label",
  "ordered_dimension", "adjacent_transition", "anchor_projection", "requirement_relation",
  "orientation_type", "orientation_basis", "scale_anchor_config", "support_id", "site", "Id", "analysis_unit_type",
  "analysis_unit_id_a", "analysis_unit_id_b", "Date", "window_id_a", "window_id_b",
  "n_days_a", "n_days_b", "metric", "metric_class", "metric_scope", "metric_geometry",
  "value_a", "value_b", "delta", "z", "robust_z", "available_a", "available_b",
  "pair_available", "available", "unavailable_reason"
)
rq1 <- rq1_pairwise_load(
  rq1_artifact, columns = rq1_columns,
  filter_fn = function(z) z |>
    filter(pair_available, dimension %in% c("placement", "optical", "temporal", "duration"),
           (dimension %in% c("placement", "optical") | adjacent_transition)) |>
    mutate(Date = as.Date(Date))
)
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
duration_cube <- load_duration_metric_cube(readRDS(DURATION_CUBE), columns = c(
  "site", "Id", "window_id", "window_start", "window_end", "metric", "placement", "optical",
  "resolution_s", "value"
))
RQ1_VERSION <- rq1_pairwise_version(rq1_artifact)
CORE_VERSION <- unique(na.omit(c(rq1_artifact$core_artifact_version, rq1$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
if (any(cube$resolution_s == 15L)) stop("15-s core state detected")
RQ2_VERSION <- paste0("rq2_v4_oriented_local_pairwise_gamma__", RQ1_VERSION)

safe_mean <- function(x) {x <- x[is.finite(x)]; if (length(x)) mean(x) else NA_real_}
safe_sd <- function(x) {x <- x[is.finite(x)]; if (length(x) >= 2L) sd(x) else NA_real_}
safe_q <- function(x, p) {x <- x[is.finite(x)]; if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_}
circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  th <- 2 * pi * x / period; (atan2(mean(sin(th)), mean(cos(th))) %% (2 * pi)) * period / (2 * pi)
}
standardize <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (geometry == "circular_time") sd(circular_delta(x, circular_mean(x))) else sd(x)
}

# Explicit branch-cut regression: two oriented marginal shifts straddling
# +/-period/2 differ by a small circular displacement, not by a full period.
branch_cut_test <- tibble(
  period = 86400, marginal_from = 43200 - 2, marginal_to = -43200 + 2,
  ordinary_difference = marginal_to - marginal_from,
  circular_second_difference = circular_delta(marginal_to, marginal_from),
  pass = abs(circular_delta(marginal_to, marginal_from)) < 10
)
if (!branch_cut_test$pass) stop("Circular gamma branch-cut regression failed")
readr::write_csv(branch_cut_test, file.path(DIAG, "rq2_circular_gamma_test.csv"), na = "")

# -----------------------------------------------------------------------------
# Exposure-state and external context features
# -----------------------------------------------------------------------------
state_cube <- cube |>
  filter(analysis_unit_type == "participant_day", support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L,
         metric %in% c("mean_MEDI", "frequency_crossing_250"), available, is.finite(value)) |>
  select(site, Id, Date, metric, value) |>
  distinct() |>
  pivot_wider(names_from = metric, values_from = value)
external <- context |>
  filter(support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
  select(site, Id, Date, era5_ssrd_daily_mean_w_m2, era5_direct_fraction, era5_total_cloud_cover_mean, latitude, day_of_year) |>
  distinct()
daily_features <- full_join(state_cube, external, by = c("site", "Id", "Date")) |>
  mutate(
    state_level = mean_MEDI,
    state_dynamic = if_else(is.finite(frequency_crossing_250) & frequency_crossing_250 >= 0, log1p(frequency_crossing_250), NA_real_),
    external_radiation = if_else(is.finite(era5_ssrd_daily_mean_w_m2) & era5_ssrd_daily_mean_w_m2 >= 0, log1p(era5_ssrd_daily_mean_w_m2), NA_real_),
    external_direct_fraction = era5_direct_fraction,
    external_cloud = era5_total_cloud_cover_mean,
    solar_noon_elevation_deg = pmax(0, 90 - abs(latitude - 23.44 * sin(2 * pi * (284 + day_of_year) / 365.25)))
  ) |>
  select(site, Id, Date, state_level, state_dynamic, external_radiation, external_direct_fraction, external_cloud, solar_noon_elevation_deg)

pair_features <- rq1 |>
  filter(pair_available) |>
  left_join(daily_features, by = c("site", "Id", "Date")) |>
  mutate(
    # The primary state is local to the transition. Duration deliberately uses
    # the longer-window MEDI level plus variability in the shorter window; it
    # does not use target departure from a seven-day mean.
    duration_state = NA_real_, duration_day_variability = NA_real_,
    primary_state_name = case_when(
      dimension == "placement" ~ "target-aligned daily MEDI level",
      dimension == "optical" ~ "target-aligned daily MEDI level",
      dimension == "temporal" ~ "higher-resolution short-term crossing dynamics",
      dimension == "duration" ~ "longer-window MEDI level plus pre-addition day variability",
      TRUE ~ "transition-local exposure state"
    ),
    primary_state_raw = case_when(
      dimension %in% c("placement", "optical") ~ state_level,
      dimension == "temporal" ~ state_dynamic,
      TRUE ~ duration_state
    ),
    abs_z = abs(z), participant_key = paste(site, Id, sep = "|")
  )

# Duration features use the actual member dates of each side. The longer window
# level is computed from duration_metric_cube; variability is calculated only on
# the shorter window before the added day.
if (any(rq1$dimension == "duration")) {
  duration_state_long <- duration_cube |>
    filter(metric == "mean_MEDI", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    select(site, Id, window_id, longer_window_level = value)
  duration_daily <- cube |>
    filter(analysis_unit_type == "participant_day", support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L, metric == "mean_MEDI") |>
    select(site, Id, Date, value) |>
    distinct()
  duration_state <- rq1 |>
    filter(dimension == "duration") |>
    select(pair_key, site, Id, window_id_a, window_id_b, n_days_a) |>
    distinct() |>
    left_join(duration_state_long, by = c("site", "Id", "window_id_b" = "window_id")) |>
    left_join(duration_cube |> filter(metric == "mean_MEDI", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
                select(site, Id, window_id = window_id_a, window_start, window_end),
              by = c("site", "Id", "window_id_a" = "window_id")) |>
    left_join(duration_cube |> filter(metric == "mean_MEDI", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
                select(site, Id, window_id = window_id_b, window_start_b = window_start, window_end_b = window_end),
              by = c("site", "Id", "window_id_b" = "window_id")) |>
    left_join(duration_daily, by = c("site", "Id")) |>
    filter(Date >= window_start, Date <= window_end) |>
    group_by(pair_key, longer_window_level) |>
    summarise(duration_state = log1p(abs(longer_window_level)), duration_day_variability = safe_sd(value), .groups = "drop")
  pair_features <- pair_features |>
    select(-duration_state, -duration_day_variability) |>
    left_join(duration_state, by = "pair_key") |>
    mutate(primary_state_raw = if_else(dimension == "duration", duration_state, primary_state_raw))
}

# Low/Middle/High bins are frozen by transition/support, then reused for every
# metric: state is an exposure-process property, not a metric-specific ranking.
state_units <- pair_features |>
  filter(is.finite(primary_state_raw)) |>
  distinct(dimension, comparison_pair_id, support_id, site, Id, analysis_unit_id_a, primary_state_raw)
state_bins <- state_units |>
  group_by(dimension, comparison_pair_id, support_id) |>
  mutate(state_bin = if (n() >= 6L && n_distinct(primary_state_raw) >= 3L) ntile(primary_state_raw, 3L) else NA_integer_) |>
  ungroup() |>
  mutate(state_bin_label = recode(as.character(state_bin), `1` = "Low", `2` = "Middle", `3` = "High", .default = NA_character_))
condition <- pair_features |>
  left_join(state_bins |> select(dimension, comparison_pair_id, support_id, site, Id, analysis_unit_id_a, state_bin, state_bin_label),
            by = c("dimension", "comparison_pair_id", "support_id", "site", "Id", "analysis_unit_id_a")) |>
  mutate(rq2_analysis_version = RQ2_VERSION)
saveRDS(condition, file.path(OUT, "rq2_condition_long.rds"), compress = "xz")
readr::write_csv(state_bins, file.path(DIAG, "rq2_reference_state_bins.csv"), na = "")

primary <- condition |>
  filter((dimension %in% c("placement", "optical")) | adjacent_transition, pair_available, available, is.finite(z))
conditional_geometry <- primary |>
  filter(!is.na(state_bin_label)) |>
  group_by(dimension, comparison_pair_id, config_a_label, config_b_label, metric, metric_class, metric_geometry,
           primary_state_name, state_bin, state_bin_label) |>
  summarise(n_participants = n_distinct(participant_key), n_units = n(),
            state_median = median(primary_state_raw), state_q25 = safe_q(primary_state_raw, .25), state_q75 = safe_q(primary_state_raw, .75),
            B_conditional = mean(z), A_conditional = mean(abs(z)), median_z = median(z),
            p025_z = safe_q(z, .025), p975_z = safe_q(z, .975), .groups = "drop") |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION)
readr::write_csv(conditional_geometry, file.path(OUT, "rq2_conditional_geometry.csv"), na = "")

# -----------------------------------------------------------------------------
# Mixed-model/CV engine, retained with checkpoints but limited to local primary
# transitions. Set RQ2_RUN_MODELS=0 for a structural smoke run.
# -----------------------------------------------------------------------------
EXTERNAL <- c("external_radiation", "external_direct_fraction", "external_cloud", "solar_noon_elevation_deg")
FAMILIES <- list(external_context = EXTERNAL, exposure_state = "primary_state_raw", joint = c("primary_state_raw", EXTERNAL))
OUTCOMES <- c(signed = "z", magnitude = "abs_z")
fit_task <- function(task) {
  dat <- task$data; meta <- task$meta
  scale_train_test <- function(tr, te, predictors) {
    keep <- character(); for (p in predictors) {mu <- mean(tr[[p]], na.rm = TRUE); s <- sd(tr[[p]], na.rm = TRUE); if (!is.finite(mu) || !is.finite(s) || s <= sqrt(.Machine$double.eps)) next; tr[[p]] <- (tr[[p]] - mu) / s; te[[p]] <- (te[[p]] - mu) / s; keep <- c(keep, p)}; list(tr = tr, te = te, keep = keep)
  }
  fit_one <- function(d, outcome, predictors) {
    if (!length(predictors)) return(list(fit = NULL, random_structure = NA_character_))
    d$site <- factor(d$site); d$participant_key <- factor(d$participant_key)
    f <- reformulate(predictors, response = outcome)
    ctrl <- nlme::lmeControl(opt = "optim", maxIter = 100L, msMaxIter = 100L, returnObject = TRUE)
    fit <- tryCatch(suppressWarnings(nlme::lme(fixed = f, random = ~1 | site/participant_key, data = d, method = "ML", na.action = na.omit, control = ctrl)), error = function(e) NULL)
    if (!is.null(fit)) return(list(fit = fit, random_structure = "site/participant"))
    fit <- tryCatch(suppressWarnings(nlme::lme(fixed = f, random = ~1 | participant_key, data = d, method = "ML", na.action = na.omit, control = ctrl)), error = function(e) NULL)
    list(fit = fit, random_structure = if (is.null(fit)) NA_character_ else "participant")
  }
  performance <- function(obs, pred) {ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]; if (length(obs) < 2L) return(tibble(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_)); sst <- sum((obs - mean(obs))^2); tibble(n_test = length(obs), rmse = sqrt(mean((obs - pred)^2)), mae = mean(abs(obs - pred)), r2 = if (sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_)}
  set.seed(task$seed); pm <- dat |> distinct(site, participant_key) |> group_by(site) |> mutate(fold = sample(rep(seq_len(task$folds), length.out = n()))) |> ungroup()
  coefs <- list(); perfs <- list(); ci <- 0L; pi <- 0L
  for (oname in names(OUTCOMES)) for (fname in names(FAMILIES)) {
    outcome <- OUTCOMES[[oname]]; preds <- FAMILIES[[fname]]; sc <- scale_train_test(dat, dat, preds); fit <- fit_one(sc$tr, outcome, sc$keep)
    if (!is.null(fit$fit)) {tt <- summary(fit$fit)$tTable; ci <- ci + 1L; coefs[[ci]] <- tibble(dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric, outcome = oname, model_family = fname, random_structure = fit$random_structure, term = rownames(tt), estimate = tt[, "Value"], std_error = tt[, "Std.Error"], df = tt[, "DF"], t_value = tt[, "t-value"], p_value = tt[, "p-value"])}
    for (scheme in c("participant_grouped", "leave_site_out")) {
      obs <- pred <- numeric()
      splits <- if (scheme == "participant_grouped") sort(unique(pm$fold)) else sort(unique(pm$site))
      for (sp in splits) {test <- if (scheme == "participant_grouped") pm$fold == sp else pm$site == sp; tr <- dat[match(paste(pm$site[!test], pm$participant_key[!test]), paste(dat$site, dat$participant_key)), , drop = FALSE]; te <- dat[match(paste(pm$site[test], pm$participant_key[test]), paste(dat$site, dat$participant_key)), , drop = FALSE]; if (nrow(tr) < 20L || nrow(te) < 2L) next; ss <- scale_train_test(tr, te, preds); ff <- fit_one(ss$tr, outcome, ss$keep); if (is.null(ff$fit)) next; pr <- tryCatch(as.numeric(predict(ff$fit, newdata = ss$te, level = 0)), error = function(e) rep(NA_real_, nrow(ss$te))); obs <- c(obs, ss$te[[outcome]]); pred <- c(pred, pr)}
      z <- performance(obs, pred); pi <- pi + 1L; perfs[[pi]] <- tibble(dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric, outcome = oname, model_family = fname, validation_scheme = scheme, n_participants = n_distinct(dat$participant_key), n_sites = n_distinct(dat$site), n_test = z$n_test, rmse = z$rmse, mae = z$mae, r2 = z$r2)
    }
  }
  list(checkpoint_version = task$checkpoint_version, complete = TRUE, coefficients = bind_rows(coefs), performance = bind_rows(perfs))
}
model_groups <- primary |> filter(is.finite(primary_state_raw), if_all(all_of(c("z", "abs_z", EXTERNAL)), is.finite)) |>
  group_by(dimension, comparison_pair_id, metric, metric_class) |>
  group_split(.keep = TRUE)
model_results <- vector("list", length(model_groups))
model_tasks <- map(seq_along(model_groups), function(i) {
  g <- model_groups[[i]]
  meta <- g |> slice(1) |> select(dimension, comparison_pair_id, metric, metric_class)
  token <- gsub("[^A-Za-z0-9_.-]", "_", paste(RQ1_VERSION, i, sep = "__"))
  path <- file.path(CHECKPOINTS, paste0("model_", token, ".rds"))
  version <- paste0(RQ2_VERSION, "__core__", CORE_VERSION, "__folds__", RQ2_CV_FOLDS)
  cached <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  list(index = i, data = g, meta = meta, folds = RQ2_CV_FOLDS,
       seed = 20260821L + i, checkpoint_version = version, checkpoint_path = path,
       cached = cached)
})
if (RUN_MODELS && length(model_tasks)) {
  pending <- keep(model_tasks, ~is.null(.x$cached) || !identical(.x$cached$checkpoint_version, .x$checkpoint_version))
  cached <- discard(model_tasks, ~is.null(.x$cached) || !identical(.x$cached$checkpoint_version, .x$checkpoint_version))
  if (length(pending)) {
    message("RQ2 models: ", length(pending), " pending tasks across ", RQ2_WORKERS, " PSOCK workers")
    fitted <- ms_parallel_map(
      pending,
      function(task) fit_task(task),
      workers = RQ2_WORKERS, seed = MODEL_SEED,
      packages = c("tidyverse", "nlme"),
      exports = c("fit_task", "OUTCOMES", "FAMILIES")
    )
    for (j in seq_along(pending)) {
      task <- pending[[j]]; obj <- fitted[[j]]
      saveRDS(obj, task$checkpoint_path, compress = FALSE)
      model_results[[task$index]] <- obj
    }
  }
  for (task in cached) model_results[[task$index]] <- task$cached
}
model_coefficients <- bind_rows(map(model_results, "coefficients"))
model_performance <- bind_rows(map(model_results, "performance"))
if (!ncol(model_coefficients)) model_coefficients <- tibble(
  dimension = character(), comparison_pair_id = character(), metric = character(), outcome = character(),
  model_family = character(), random_structure = character(), term = character(), estimate = double(),
  std_error = double(), df = double(), t_value = double(), p_value = double()
)
if (!ncol(model_performance)) model_performance <- tibble(
  dimension = character(), comparison_pair_id = character(), metric = character(), outcome = character(),
  model_family = character(), validation_scheme = character(), n_participants = integer(), n_sites = integer(),
  n_test = integer(), rmse = double(), mae = double(), r2 = double()
)
readr::write_csv(model_coefficients, file.path(OUT, "rq2_model_coefficients.csv"), na = "")
readr::write_csv(model_performance, file.path(OUT, "rq2_model_performance.csv"), na = "")

model_manifest <- dplyr::bind_rows(lapply(model_tasks, function(task) {
  obj <- model_results[[task$index]]
  tibble(
    artifact_type = "rq2_model_checkpoint_manifest_v1",
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    core_artifact_version = CORE_VERSION,
    task_index = task$index,
    dimension = task$meta$dimension[[1]],
    comparison_pair_id = task$meta$comparison_pair_id[[1]],
    metric = task$meta$metric[[1]],
    metric_class = task$meta$metric_class[[1]],
    checkpoint_path = normalizePath(task$checkpoint_path, winslash = "/", mustWork = FALSE),
    checkpoint_version = task$checkpoint_version,
    checkpoint_present = file.exists(task$checkpoint_path),
    checkpoint_complete = !is.null(obj) && isTRUE(obj$complete),
    coefficient_rows = if (!is.null(obj)) nrow(obj$coefficients) else 0L,
    performance_rows = if (!is.null(obj)) nrow(obj$performance) else 0L,
    run_models = RUN_MODELS
  )
}))
readr::write_csv(model_manifest, file.path(OUT, "rq2_model_artifact_manifest.csv"), na = "")

# -----------------------------------------------------------------------------
# Gamma: actual four-cell configuration values. Each first difference follows
# dimension_a's scientific orientation; the second difference follows
# dimension_b's scientific orientation. Circular metrics use circular geometry
# at both levels.
# -----------------------------------------------------------------------------
gamma_block <- function(cells, cell_names, dimension_a, dimension_b, transition, lattice, anchor_support) {
  key <- c("support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date", "metric")
  z <- cells |> filter(cell %in% cell_names) |> select(all_of(key), metric_class, metric_geometry, cell, value, available)
  wide <- z |> select(-available) |> pivot_wider(names_from = cell, values_from = value)
  if (!all(cell_names %in% names(wide))) return(tibble())
  wide |>
    mutate(
      dimension_a = dimension_a, dimension_b = dimension_b, transition = transition, comparison_lattice = lattice,
      delta_at_b_from = if_else(
        metric_geometry == "circular_time",
        circular_delta(.data[[cell_names[[2]]]], .data[[cell_names[[1]]]]),
        .data[[cell_names[[2]]]] - .data[[cell_names[[1]]]]
      ),
      delta_at_b_to = if_else(
        metric_geometry == "circular_time",
        circular_delta(.data[[cell_names[[4]]]], .data[[cell_names[[3]]]]),
        .data[[cell_names[[4]]]] - .data[[cell_names[[3]]]]
      ),
      gamma_delta = if_else(
        metric_geometry == "circular_time",
        circular_delta(delta_at_b_to, delta_at_b_from),
        delta_at_b_to - delta_at_b_from
      ),
      # compatibility aliases retained for downstream readers that only inspect
      # the long artifact; new interpretation is b_from -> b_to.
      delta_at_b = delta_at_b_from,
      delta_at_ref = delta_at_b_to,
      anchor_support = anchor_support
    )
}
gamma_blocks <- list()
# placement x optical: dimension_a chest/wrist -> eye; dimension_b LIGHT -> MEDI.
for (pos in c("chest", "wrist")) {
  sup <- paste0("eye_", pos, "_full")
  cells <- cube |> filter(support_id == sup, resolution_s == 10L, metric %in% unique(rq1$metric)) |>
    mutate(cell = paste(placement, optical, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste(pos, "LIGHT", sep = "__"), paste("eye", "LIGHT", sep = "__"),
      paste(pos, "MEDI", sep = "__"), paste("eye", "MEDI", sep = "__")),
    "placement", "optical", paste0(pos, "_LIGHT_to_MEDI"), paste0("placement_", pos, "_x_optical"), sup
  )
}
# placement x temporal and optical x temporal. Temporal orientation is coarse -> fine.
for (pos in c("chest", "wrist")) for (j in seq_len(length(TEMPORAL_GAMMA_S) - 1L)) {
  fine <- TEMPORAL_GAMMA_S[j]
  coarse <- TEMPORAL_GAMMA_S[j + 1L]
  sup <- paste0("eye_", pos, "_full")
  cells <- cube |> filter(support_id == sup, optical == "MEDI", resolution_s %in% c(fine, coarse)) |>
    mutate(cell = paste(placement, resolution_s, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste(pos, coarse, sep = "__"), paste("eye", coarse, sep = "__"),
      paste(pos, fine, sep = "__"), paste("eye", fine, sep = "__")),
    "placement", "temporal", paste0(pos, "_", coarse, "to", fine), paste0("placement_", pos, "_x_temporal"), sup
  )
}
for (j in seq_len(length(TEMPORAL_GAMMA_S) - 1L)) {
  fine <- TEMPORAL_GAMMA_S[j]
  coarse <- TEMPORAL_GAMMA_S[j + 1L]
  cells <- cube |> filter(support_id == "eye_full", placement == "eye", resolution_s %in% c(fine, coarse)) |>
    mutate(cell = paste(optical, resolution_s, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste("LIGHT", coarse, sep = "__"), paste("MEDI", coarse, sep = "__"),
      paste("LIGHT", fine, sep = "__"), paste("MEDI", fine, sep = "__")),
    "optical", "temporal", paste0(coarse, "to", fine), "optical_x_temporal", "eye_full"
  )
}
gamma_raw <- bind_rows(gamma_blocks)
if (nrow(gamma_raw)) {
  anchor <- cube |> filter(placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    select(support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, metric, anchor_value = value) |>
    filter(is.finite(anchor_value)) |>
    group_by(support_id, metric) |>
    summarise(standardizer = standardize(anchor_value, first(gamma_raw$metric_geometry[match(metric, gamma_raw$metric)])), .groups = "drop")
  gamma_long <- gamma_raw |> left_join(anchor, by = c("anchor_support" = "support_id", "metric")) |>
    mutate(available = is.finite(gamma_delta) & is.finite(standardizer), gamma = if_else(available, gamma_delta / standardizer, NA_real_),
           core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION) |>
    select(core_artifact_version, rq1_analysis_version, rq2_analysis_version, dimension_a, dimension_b, transition,
           comparison_lattice, anchor_support, site, Id, analysis_unit_type, analysis_unit_id, Date, metric, metric_class,
           metric_geometry, delta_at_b_from, delta_at_b_to, delta_at_b, delta_at_ref,
           gamma_delta, standardizer, gamma, available)
} else gamma_long <- tibble()
saveRDS(gamma_long, file.path(OUT, "rq2_gamma_long.rds"), compress = "xz")
gamma_summary <- if (nrow(gamma_long)) gamma_long |> filter(available, is.finite(gamma)) |> group_by(dimension_a, dimension_b, comparison_lattice, transition, metric, metric_class, metric_geometry) |> summarise(n_participants = n_distinct(paste(site, Id, sep = "|")), n_units = n(), R = mean(gamma), Q = mean(abs(gamma)), .groups = "drop") else tibble()
if (nrow(gamma_summary) && any(gamma_summary$Q + 1e-12 < abs(gamma_summary$R))) stop("RQ2 Q >= |R| invariant failed")
readr::write_csv(gamma_summary, file.path(OUT, "rq2_gamma_summary.csv"), na = "")
readr::write_csv(gamma_summary, file.path(OUT, "rq2_conditional_geometry_gamma.csv"), na = "")
readr::write_csv(tibble(
  dimension_pair = c("placement x optical", "placement x temporal", "optical x temporal", "duration-containing"),
  primary_scope = c(TRUE, TRUE, TRUE, FALSE), note = c("target-aligned local full-support four-cell contrast", "adjacent primary temporal transitions", "adjacent primary temporal transitions", "duration enters RQ3 joint stability")
), file.path(OUT, "rq2_interaction_scope.csv"), na = "")

writeLines(c(
  "# RQ2 run report", "", paste0("RQ1 upstream: ", RQ1_VERSION), paste0("RQ2 analysis version: ", RQ2_VERSION),
  "Primary conditional transitions inherit RQ1 scientific orientation: chest/wrist -> eye; LIGHT -> MEDI; coarse -> fine; short -> long.",
  "Duration state: log(absolute longer-window mean MEDI) plus SD of daily mean MEDI before the added day; no seven-day departure predictor.",
  paste0("Circular gamma branch-cut test passed: gamma_delta = ", format(branch_cut_test$circular_second_difference, digits = 8)),
  paste0("Mixed models executed: ", RUN_MODELS), "Checkpoint directory: results/rq2/checkpoints/"
), file.path(OUT, "RQ2_RUN_REPORT.md"))
message("RQ2 complete: ", RQ2_VERSION)