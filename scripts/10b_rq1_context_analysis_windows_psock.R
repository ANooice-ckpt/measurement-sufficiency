# Windows PSOCK runner for scripts/10b_rq1_context_analysis.R
#
# This wrapper changes execution plumbing only. It rewrites one argument-passing
# detail in the Windows parLapplyLB call at runtime so the target function is
# passed through `...` positionally instead of being matched to parLapplyLB's
# own `fun` formal. Scientific operators, supports, metrics, standardization,
# windows, and bootstrap definitions remain those in 10b_rq1_context_analysis.R.

target <- "scripts/10b_rq1_context_analysis.R"
if (!file.exists(target)) stop("Missing target script: ", target)

code <- readLines(target, warn = FALSE, encoding = "UTF-8")
old <- "    ans <- parallel::parLapplyLB(cl, X, safe_runner, fun = FUN, chunk.size = 1L)"
new <- "    ans <- parallel::parLapplyLB(cl, X, safe_runner, FUN, chunk.size = 1L)"

idx_old <- which(code == old)
idx_new <- which(code == new)

if (length(idx_old) == 1L) {
  code[idx_old] <- new
  message("Applied Windows PSOCK argument-matching hotfix to RQ1 context execution.")
} else if (length(idx_new) == 1L) {
  message("RQ1 context already contains the Windows PSOCK argument-matching fix.")
} else {
  stop(
    "Could not identify the expected Windows PSOCK call in ", target,
    ". Refusing to modify scientific code implicitly."
  )
}

expr <- parse(text = code, keep.source = FALSE)
eval(expr, envir = .GlobalEnv)
