# Fast, base-R contract test for manuscript figure identity and routing.
# It uses no project data or optional packages and is safe to run in CI.
source("scripts/utils/figure_registry.R")

registry <- ms_main_figure_registry()
expected_current <- c(
  "Fig1_RQ1",
  "Fig2_RQ1_inferential_preservation",
  "Fig3_RQ2",
  "Fig4_RQ2",
  "Fig5_RQ3",
  "Fig6_RQ3"
)
expected_legacy <- c(
  "Fig1_RQ1",
  "Fig2_RQ1_inferential_preservation",
  "Fig2_RQ2",
  "Fig3_RQ2",
  "Fig4_RQ3",
  "Fig5_RQ3"
)
retired_plot_paths <- c(
  "scripts/13a_plot_fig2.R",
  "scripts/13b_plot_fig3.R",
  "scripts/15a_plot_fig4.R",
  "scripts/15b_plot_fig5.R",
  "scripts/16_plot_supplementary.R"
)

stopifnot(
  identical(registry$current_id, expected_current),
  identical(registry$legacy_id, expected_legacy),
  !anyDuplicated(registry$current_id),
  !anyDuplicated(registry$canonical_script),
  all(file.exists(registry$canonical_script)),
  !any(file.exists(retired_plot_paths)),
  identical(ms_current_main_figure_ids(), expected_current),
  identical(ms_main_figure_from_legacy_id(expected_legacy), expected_current),
  identical(ms_main_figure_to_legacy_id(expected_current), expected_legacy),
  identical(
    ms_main_figure_filename_from_legacy(paste0(expected_legacy, ".png")),
    paste0(expected_current, ".png")
  ),
  identical(ms_main_plot_scripts(), basename(registry$canonical_script))
)

# The overlap is intentional and is why conversion direction is explicit:
# current Fig3_RQ2 is also the legacy identity of current Fig4_RQ2, and current
# Fig5_RQ3 is also the legacy identity of current Fig6_RQ3. Stored current IDs
# therefore must never be fed back through the legacy->current conversion.
stopifnot(
  identical(ms_main_figure_from_legacy_id("Fig3_RQ2"), "Fig4_RQ2"),
  identical(ms_main_figure_from_legacy_id("Fig5_RQ3"), "Fig6_RQ3"),
  identical(ms_main_figure_to_legacy_id("Fig3_RQ2"), "Fig2_RQ2"),
  identical(ms_main_figure_to_legacy_id("Fig5_RQ3"), "Fig4_RQ3")
)

# Orchestration is explicit and must invoke every canonical entrypoint exactly
# through its current numbered path.
runner <- readLines("scripts/run_downstream_server.sh", warn = FALSE)
stopifnot(all(vapply(
  registry$canonical_script,
  function(path) any(grepl(path, runner, fixed = TRUE)),
  logical(1)
)))
stopifnot(!any(grepl("16_plot_supplementary.R", runner, fixed = TRUE)))

# Export dispatch must be independent of which main figure ran previously.
# Sentinel builders exercise routing without creating plots or reading results.
local({
  e <- new.env(parent = globalenv())
  sys.source("scripts/utils/figure_polish.R", envir = e)
  e$ms_polish_fig1 <- function(...) "fig1"
  e$ms_polish_fig4 <- function(...) "fig5"
  e$ms_polish_fig5 <- function(...) "fig6"
  untouched <- list(plot = "frozen plot", width = 7.4, height = 6.1)
  expected <- list("fig1", untouched, untouched, untouched, "fig5", "fig6")
  for (i in c(3L, 4L, 1L, 5L, 6L, 2L, 3L, 1L)) {
    actual <- e$ms_polish_main_figure("frozen plot", paste0(expected_legacy[i], ".png"),
                                     e, 7.4, 6.1)
    stopifnot(identical(actual, expected[[i]]))
  }
  stopifnot(identical(e$ms_polish_main_figure("frozen plot", "FigS_check.png", e, 7.4, 6.1),
                      untouched))
  for (path in registry$canonical_script) {
    definitions <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                            is.symbol(x[[2]]), parse(path))
    names <- vapply(definitions, function(x) as.character(x[[2]]), character(1))
    stopifnot(!any(names %in% c("ms_fig2_refine_main", "ms_fig3_refine_main",
                              "ms_fig3_atlas_refine_main", "ms_polish_main_figure")))
  }
})

# Inspect every direct writer, not just already-guarded blocks. Sentinels never
# force writer arguments, so missing plot/data objects cannot trigger real I/O.
local({
  writer_names <- c("write_csv", "write_csv2", "write_tsv", "write_delim", "write_lines",
                    "write.csv", "write.table", "writeLines", "writeBin", "saveRDS", "save",
                    "fwrite", "ggsave", "dir.create", "download.file", "file.create",
                    "file.copy", "file.rename", "unlink", "sink")
  call_name <- function(x) {
    if (!is.call(x)) return("")
    head <- x[[1]]
    if (is.symbol(head)) return(as.character(head))
    if (is.call(head) && is.symbol(head[[1]]) && as.character(head[[1]]) %in% c("::", ":::")) {
      return(as.character(head[[3]]))
    }
    ""
  }
  checks <- list()
  inspect <- function(x, path, guard = NULL, gate = NULL) {
    if (!is.call(x) && !is.expression(x)) return(invisible(NULL))
    name <- call_name(x)
    if (name %in% writer_names) {
      if (is.null(guard) && is.null(gate)) stop("Unguarded writer in ", path, ": ", name)
      checks[[length(checks) + 1L]] <<- list(
        code = if (!is.null(guard)) guard else as.call(list(as.name("{"), gate, x)),
        blocked = is.null(guard))
      return(invisible(NULL))
    }
    if (name == "if" && identical(x[[2]], quote(!ms_plot_prep_only()))) {
      inspect(x[[3]], path, as.call(list(as.name("if"), x[[2]], x[[3]])), gate)
      if (length(x) == 4L) inspect(x[[4]], path, guard, gate)
      return(invisible(NULL))
    }
    # The appendix rejects a missing basemap before reaching its download block.
    if (name == "{" || is.expression(x)) {
      statements <- if (is.expression(x)) as.list(x) else as.list(x)[-1L]
      for (statement in statements) {
        inspect(statement, path, guard, gate)
        if (call_name(statement) == "if" && length(statement) == 3L &&
            identical(statement[[2]], quote(ms_plot_prep_only())) &&
            call_name(statement[[3]]) == "stop") gate <- statement
      }
    } else invisible(lapply(as.list(x), inspect, path = path, guard = guard, gate = gate))
  }
  paths <- c(registry$canonical_script, "scripts/16_appendix_figures.R")
  invisible(lapply(paths, function(path) inspect(parse(path), path)))
  stopifnot(length(checks) > 0L,
    inherits(tryCatch(inspect(quote(readr::write_csv(missing_data, missing_path)),
                             "unguarded sentinel"), error = identity), "error"))

  e <- new.env(parent = baseenv())
  wanted <- c("ms_plot_prep_only", "ms_plot_save", "ms_plot_write_manifest")
  definitions <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    is.symbol(x[[2]]) && as.character(x[[2]]) %in% wanted,
    parse("scripts/utils/plot_contracts.R"))
  stopifnot(length(definitions) == length(wanted))
  invisible(lapply(definitions, eval, envir = e))
  e$stop <- function(...) base::stop("prep blocked")
  run_checks <- function() {
    old <- Sys.getenv("MS_PLOT_PREP_ONLY", unset = NA_character_)
    on.exit(if (is.na(old)) Sys.unsetenv("MS_PLOT_PREP_ONLY") else
      Sys.setenv(MS_PLOT_PREP_ONLY = old), add = TRUE)
    for (mode in c(NA_character_, "0", "1")) {
      if (is.na(mode)) Sys.unsetenv("MS_PLOT_PREP_ONLY") else Sys.setenv(MS_PLOT_PREP_ONLY = mode)
      prep <- identical(mode, "1")
      stopifnot(identical(e$ms_plot_prep_only(), prep))
      for (check in checks) {
        expected <- 0L
        stub <- function(x) {
          if (!is.call(x)) return(x)
          if (call_name(x) %in% writer_names) {
            expected <<- expected + 1L
            x[[1]] <- as.name(".writer")
            return(x)
          }
          as.call(lapply(as.list(x), stub))
        }
        code <- stub(check$code)
        calls <- 0L
        e$.writer <- function(...) { calls <<- calls + 1L; invisible(NULL) }
        result <- tryCatch(eval(code, envir = e), error = identity)
        stopifnot(calls == if (prep) 0L else expected,
                  inherits(result, "error") == (prep && check$blocked))
      }
    }
    # Both centralized exports must return before touching lazy plot/row inputs.
    stopifnot(identical(e$ms_plot_save(stop("forced plot"), "sentinel.png",
                                      stop("forced width"), stop("forced height")), "sentinel.png"),
              identical(e$ms_plot_write_manifest("sentinel.csv", stop("forced rows")), "sentinel.csv"))
  }
  run_checks()
})

cat("PASS: canonical main-figure identities, paths, legacy output mapping and runner entrypoints are consistent\n")
