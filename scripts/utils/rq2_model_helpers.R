# Shared numerical helpers for legacy and layered RQ2 model workers.
# Kept in a factory so worker functions receive the same lexical scope.
rq2_model_helpers <- function() {
  scale_train_test <- function(tr, te, predictors) {
    keep <- character()
    for (p in predictors) {
      finite <- is.finite(tr[[p]])
      if (sum(finite) < 3L) next
      mu <- mean(tr[[p]][finite]); s <- sd(tr[[p]][finite])
      if (!is.finite(mu) || !is.finite(s) || s <= sqrt(.Machine$double.eps)) next
      tr[[p]] <- (tr[[p]] - mu) / s
      te[[p]] <- (te[[p]] - mu) / s
      keep <- c(keep, p)
    }
    list(tr = tr, te = te, keep = keep)
  }
  fit_one <- function(d, outcome, predictors) {
    if (!length(predictors) || nrow(d) < 20L || n_distinct(d$participant_key) < 3L) {
      return(list(fit = NULL, random_structure = NA_character_))
    }
    d$site <- factor(d$site); d$participant_key <- factor(d$participant_key)
    f <- reformulate(predictors, response = outcome)
    ctrl <- nlme::lmeControl(opt = "optim", maxIter = 100L, msMaxIter = 100L, returnObject = TRUE)
    fit <- tryCatch(
      suppressWarnings(nlme::lme(fixed = f, random = ~1 | site/participant_key, data = d,
                                 method = "ML", na.action = na.omit, control = ctrl)),
      error = function(e) NULL
    )
    if (!is.null(fit)) return(list(fit = fit, random_structure = "site/participant"))
    fit <- tryCatch(
      suppressWarnings(nlme::lme(fixed = f, random = ~1 | participant_key, data = d,
                                 method = "ML", na.action = na.omit, control = ctrl)),
      error = function(e) NULL
    )
    list(fit = fit, random_structure = if (is.null(fit)) NA_character_ else "participant")
  }
  performance <- function(obs, pred) {
    ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]
    if (length(obs) < 2L) return(tibble(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_))
    sst <- sum((obs - mean(obs))^2)
    tibble(n_test = length(obs), rmse = sqrt(mean((obs - pred)^2)), mae = mean(abs(obs - pred)),
           r2 = if (sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_)
  }

  list(scale_train_test = scale_train_test, fit_one = fit_one, performance = performance)
}
