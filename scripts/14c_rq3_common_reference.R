# Run from the repository root:
# Rscript scripts/14c_rq3_common_reference.R
# Reads frozen summaries only. No raw series, window reconstruction or RQ refit.
# Optional --output-dir=PATH and --validate-only; all paths are repository-relative
# unless absolute. This is separate from the canonical RQ3 sufficiency analysis.

rq3_common_reference_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  unknown <- args[!grepl("^--output-dir=", args) & args != "--validate-only"]
  if (length(unknown)) stop("Unknown argument: ", paste(unknown, collapse = ", "))
  source("scripts/utils/analysis_design.R")
  source("scripts/utils/rq3_common_reference.R")
  pair_path <- "results/rq3/rq3_joint_pair_summary.csv"
  state_path <- "results/rq3/rq3_joint_summary.csv"
  window_path <- "results/core/duration_window_manifest.csv"
  input_paths <- c(pair_summary = pair_path, state_summary = state_path)
  if (file.exists(window_path)) input_paths <- c(input_paths, window_manifest = window_path)
  if (!all(file.exists(input_paths))) stop("Required frozen RQ3 summaries are missing")
  hashes_before <- tools::md5sum(input_paths)
  pairs <- read.csv(pair_path, stringsAsFactors = FALSE, check.names = FALSE)
  states <- read.csv(state_path, stringsAsFactors = FALSE, check.names = FALSE)
  windows <- if (file.exists(window_path)) read.csv(window_path, stringsAsFactors = FALSE) else NULL
  message("Projecting common-reference loss from ", nrow(pairs), " frozen pairs")
  result <- rq3_cr_projection(pairs, states, windows)
  if (!identical(unname(hashes_before), unname(tools::md5sum(input_paths)))) {
    stop("Frozen inputs changed during projection; rerun after upstream work finishes")
  }
  targets <- result$tables$target_differences
  stopifnot(all(is.na(targets$D_reference[targets$metric == "interdaily_stability" & targets$n_days == 1L])),
            all(is.na(targets$D_reference[targets$optical == "LIGHT" & targets$metric %in% c("MDER", "nvRD")])),
            all(is.na(targets$source_R_obs[targets$status == "reference_self_identity"])),
            all(targets$D_reference[targets$status == "reference_self_identity"] == 0))
  if ("--validate-only" %in% args) {
    message("Validated ", nrow(targets), " target/configuration rows; no files written")
    return(invisible(result))
  }
  out <- "results/rq3/common_reference"
  output_arg <- grep("^--output-dir=", args, value = TRUE)
  if (length(output_arg) > 1L) stop("Use one --output-dir")
  if (length(output_arg)) out <- sub("^--output-dir=", "", output_arg)
  if (!nzchar(out)) stop("Empty output directory")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  definitions <- c(
    "Common-reference frozen-summary projection",
    paste("Projection version:", result$common_reference_version),
    "D_reference = frozen A(c,c0), where c0 is the SAME placement/optical facet at 10 s x 6 d.",
    "The standardizer remains the original support-specific eye/MEDI/10 s/6 d SD.",
    "D is not R_obs. No canonical R_obs, sufficient-set decision, or canonical Pareto result is changed.",
    "Self-reference D=0 is an identity only; its R_obs remains unresolved, not zero. Self pair counts are unknown.",
    "Every published target is retained. Single-day IS and LIGHT-only MDER/nvRD are unavailable, not poor fidelity.",
    "The target-level grid preserves one maximal support for each placement/optical/metric; no global intersection is imposed.",
    "The paired mean is over each frozen comparison's valid nested windows, with its original support and weighting.",
    "Parent-window identities and target-specific missingness weights cannot be recovered from these summary CSVs.",
    "Equal counts or manifest-compatible counts do not prove identical parent windows or equal weights across candidates.",
    "All minima/envelopes/non-dominance flags compare the available SUMMARY values within one support/placement/optical/metric.",
    "They are descriptive loss-versus-nominal-record projections, not controlled common-sample fidelity optima.",
    "No metric-class/All-54 loss is aggregated and no cross-placement or cross-optical dominance is imposed.",
    "Candidate_set_complete reports whether all states inside a budget have defined D; unavailable states are never silently zero.",
    "Nominal_records = 86400 * n_days / resolution_s: recording count only, not a composite cost or participant burden.",
    "The budget envelope is piecewise constant between OBSERVED record budgets; it does not interpolate configurations or extrapolate beyond the largest budget.",
    "Ties at the cheapest attaining record count are all retained. Exact equality uses the frozen double values.",
    "Adjacent summary_D_reduction = D_from - D_to, which can be negative. It is not A(from,to) or a verified matched-window gain.",
    "The optional window manifest is used only for potential parent/subwindow COUNTS; it does not establish metric availability.",
    "This projection cannot replace a row-level common-parent-window/weight audit or establish a population-optimal design.")
  result$definitions <- definitions
  result$source_manifest <- data.frame(
    role = names(input_paths), path = unname(input_paths), md5 = unname(hashes_before),
    bytes = unname(file.info(input_paths)$size), stringsAsFactors = FALSE)
  script_paths <- c("scripts/14c_rq3_common_reference.R", "scripts/utils/rq3_common_reference.R", "scripts/utils/analysis_design.R")
  result$implementation_md5 <- tools::md5sum(script_paths)
  for (name in names(result$tables)) {
    write.csv(result$tables[[name]], file.path(out, paste0("rq3_common_reference_", name, ".csv")),
              row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  write.csv(result$source_manifest, file.path(out, "source_manifest.csv"), row.names = FALSE)
  writeLines(definitions, file.path(out, "README.txt"), useBytes = TRUE)
  saveRDS(result, file.path(out, "rq3_common_reference_projection.rds"), compress = "xz")
  output_paths <- c(file.path(out, paste0("rq3_common_reference_", names(result$tables), ".csv")),
                    file.path(out, c("source_manifest.csv", "README.txt", "rq3_common_reference_projection.rds")))
  output_manifest <- data.frame(path = output_paths, md5 = unname(tools::md5sum(output_paths)),
                                common_reference_version = result$common_reference_version)
  write.csv(output_manifest, file.path(out, "output_manifest.csv"), row.names = FALSE)
  message("Wrote ", nrow(targets), " target rows and descriptive budget projections to ", out)
  print(table(targets$status))
  invisible(result)
}

if (sys.nframe() == 0L) rq3_common_reference_main()
