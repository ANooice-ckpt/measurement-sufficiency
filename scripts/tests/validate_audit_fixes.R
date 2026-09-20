# Bounded unit fixtures only. Never call a core/RQ analysis or plotting entrypoint.
# All filesystem fixtures live in the R session's temporary directory.
source("scripts/utils/analysis_design.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/rq2_risk_models.R")
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error=identity), "error"))

# Lossless design IDs, with the exact frozen design retaining its old identity.
stopifnot(ms_analysis_design_id()=="t6s1320__d6s91",
  ms_core_design_id()=="t6s1320__d6s91__r1s300")
local({
  e<-new.env(parent=globalenv()); sys.source("scripts/utils/analysis_design.R",e)
  old<-e$ms_analysis_design_id()
  e$ms_primary_temporal_s<-function()c(10L,20L,30L,60L,80L,90L)
  stopifnot(e$ms_analysis_design_id()!=old)
  combinations<-combn(seq(10L,120L,10L),6L,simplify=FALSE)
  ids<-vapply(combinations,function(x)ms_design_vector_id(x,ms_primary_temporal_s(),"6s1320"),character(1))
  stopifnot(!anyDuplicated(ids))
  e$ms_primary_temporal_s<-ms_primary_temporal_s
  e$ms_primary_duration_days<-function()c(1L,2L,3L,4L,6L,7L)
  stopifnot(e$ms_analysis_design_id()!=old)
  e$ms_primary_duration_days<-ms_primary_duration_days
  e$ms_reserve_temporal_s<-function()integer()
  stopifnot(e$ms_core_design_id()!=ms_core_design_id())
  for(x in list(c(10,20.5),c(10,NA),c(10,Inf),c(10,10),c(20,10),0,"10",2^31)) {
    expect_error(ms_design_vector_id(x,300L,"1s300"))
  }
})

# Independent small-dimensional oracle: enumerate every contiguous block
# partition, fit its bounded block means, and minimize squared distance over
# feasible nonincreasing fits. This does not repeat the production min/max formula.
bounded_oracle <- function(x) {
  n<-length(x); best<-NULL; loss<-Inf
  for(mask in 0:(2^(n-1L)-1L)) {
    cuts<-if(n>1L)as.integer(intToBits(mask))[seq_len(n-1L)] else integer()
    groups<-cumsum(c(1L,cuts))
    fitted<-ave(x,groups,FUN=function(v)max(0,min(1,mean(v))))
    if(any(diff(fitted)>1e-12))next
    score<-sum((x-fitted)^2)
    if(score<loss){loss<-score;best<-fitted}
  }
  best
}
local({
  stopifnot(identical(as.vector(rq2_risk_monotone(matrix(c(-1,1),1))),c(0,0)),
    identical(as.vector(rq2_risk_monotone(matrix(c(0,2),1))),c(1,1)))
  set.seed(20260921)
  for(n in 1:6) {
    p<-matrix(runif(30*n,-2,3),30,n)
    q<-rq2_risk_monotone(p)
    oracle<-matrix(unlist(lapply(seq_len(nrow(p)),function(i)bounded_oracle(p[i,]))),
                   nrow=nrow(p),ncol=n,byrow=TRUE)
    stopifnot(max(abs(q-oracle))<1e-10,all(q>=0 & q<=1),
      max(abs(rq2_risk_monotone(q)-q))<1e-10)
    if(n>1L)stopifnot(all(q[,-n,drop=FALSE]>=q[,-1,drop=FALSE]-1e-12))
    truth<-matrix(rep(as.numeric(seq_len(n)<=ceiling(n/2)),each=30),30,n)
    stopifnot(all(rowSums((q-truth)^2)<=rowSums((p-truth)^2)+1e-10))
  }
  p<-matrix(c(1,.5,0,1,.5,0),2,byrow=TRUE,dimnames=list(c("a","b"),c("x","y","z")))
  stopifnot(identical(rq2_risk_monotone(p),p),
    identical(dim(rq2_risk_monotone(matrix(numeric(),0,6))),c(0L,6L)),
    identical(dim(rq2_risk_monotone(matrix(numeric(),2,0))),c(2L,0L)))
  expect_error(rq2_risk_monotone(matrix(NA_real_,1)))
  expect_error(rq2_risk_monotone(matrix(Inf,1)))
  expect_error(rq2_risk_monotone(c(.2,.1)))
})

local({
  root<-tempfile("partition_portability_");dir.create(root)
  oldwd<-getwd();on.exit({setwd(oldwd);unlink(root,recursive=TRUE)})
  version<-"synthetic_version"
  rel<-file.path("results","rq1","pairwise_parts",version)
  dest<-file.path(root,rel);dir.create(dest,recursive=TRUE)
  m<-list(artifact_type="partitioned_rq1_pairwise_change",rq1_analysis_version=version,
    part_dir=rel,parts=c("first.rds","second.rds"))
  saveRDS(data.frame(value=1),file.path(dest,m$parts[1]))
  frozen<-file.path(root,"manifest.rds");saveRDS(m,frozen);before<-tools::md5sum(frozen)
  expected<-file.path(dest,m$parts)
  stopifnot(identical(rq1_pairwise_part_paths(m,root),expected))
  # Resolve and load a selected tiny part using the ordinary consumer API.
  setwd(root)
  stopifnot(identical(rq1_pairwise_part_paths(m),file.path(rel,m$parts)),
    identical(rq1_pairwise_load(m,parts=1L)$value,1))
  setwd(oldwd)
  for(stored in c("/missing-server/project/results/rq1/parts",
                 "Z:/missing-machine/project/parts","Z:\\missing-machine\\project\\parts",
                 "\\\\missing-machine\\share\\parts")) {
    moved<-m;moved$part_dir<-stored
    stopifnot(identical(rq1_pairwise_part_paths(moved,root),expected))
  }
  moved<-m;moved$part_dir<-file.path(root,"original");dir.create(moved$part_dir)
  stopifnot(identical(rq1_pairwise_part_paths(moved,root),file.path(moved$part_dir,m$parts)))
  # Missing files are exposed; do not combine original and relocated parts.
  stopifnot(!any(file.exists(rq1_pairwise_part_paths(moved,root))))
  moved$part_dir<-file.path(root,"absent");moved$rq1_analysis_version<-"other_version"
  stopifnot(!any(file.exists(rq1_pairwise_part_paths(moved,root))))
  moved$rq1_analysis_version<-NULL;expect_error(rq1_pairwise_part_paths(moved,root))
  moved$rq1_analysis_version<-"../wrong";expect_error(rq1_pairwise_part_paths(moved,root))
  stopifnot(identical(tools::md5sum(frozen),before),identical(readRDS(frozen),m))
})

# Exercise cache selection with dependency injection. Any attempt to assemble
# real inputs or fit a model is impossible in this isolated fixture environment.
local({
  e<-new.env(parent=globalenv())
  sys.source("scripts/utils/rq2_conditional_reliability.R",e)
  root<-tempfile("reliability_cache_");dir.create(root)
  oldwd<-getwd();on.exit({setwd(oldwd);unlink(root,recursive=TRUE)})
  input<-file.path(root,"source.txt");code<-file.path(root,"builder.R")
  writeLines("source-v1",input);writeLines("builder-v1",code)
  e$recovery_predictors<-function()paste0("daily",1:18)
  e$recovery_signature_predictors<-function()paste0("signature",1:16)
  e$recovery_temporal_predictors<-function()paste0("daypart",1:32)
  e$reliability_input_code<-function()code
  e$recovery_inputs<-function(allow_build=TRUE) {
    e$last_allow_build<-allow_build
    list(problems=character(),paths=input,core="core-test",version="rq1-test")
  }
  inputs<-e$recovery_inputs();p<-e$reliability_input_provenance(inputs)
  m<-list(complete=TRUE,provenance=p,statuses=data.frame(task_index=1L))
  stopifnot(e$reliability_input_cache_matches(m,p))
  for(field in c("core_artifact_version","rq1_analysis_version","predictors",
                "signature_predictors","temporal_predictors")) {
    bad<-m;bad$provenance[[field]][1]<-"changed"
    stopifnot(!e$reliability_input_cache_matches(bad,p))
  }
  bad<-m;bad$complete<-FALSE;stopifnot(!e$reliability_input_cache_matches(bad,p))
  bad<-m;bad$provenance$builder_version<-"unknown"
  stopifnot(!e$reliability_input_cache_matches(bad,p))
  bad<-m;bad$provenance$input_md5[]<-NA_character_
  stopifnot(!e$reliability_input_cache_matches(bad,p))
  writeLines("source-v2",input)
  stopifnot(!e$reliability_input_cache_matches(m,e$reliability_input_provenance(inputs)))
  writeLines("source-v1",input);writeLines("builder-v2",code)
  stopifnot(!e$reliability_input_cache_matches(m,e$reliability_input_provenance(inputs)))
  writeLines("builder-v1",code)
  legacy<-m;legacy$provenance$builder_version<-NULL;legacy$provenance$builder_code_md5<-NULL
  legacy$provenance$estimator_version<-"anchored_shared_split_candidate_library_v2"
  legacy$provenance$code_md5<-p$builder_code_md5
  stopifnot(e$reliability_input_cache_matches(legacy,p))
  legacy$provenance$code_md5[]<-strrep("a",32)
  stopifnot(!e$reliability_input_cache_matches(legacy,p))
  legacy$provenance$code_md5<-NULL
  stopifnot(!e$reliability_input_cache_matches(legacy,p))

  setwd(root)
  export<-file.path("results","rq2","reliability_inputs","rq1-test","fixture")
  dir.create(file.path(export,"inputs"),recursive=TRUE)
  saveRDS(data.frame(fixture=TRUE),file.path(export,"inputs","task_0001.rds"))
  writeLines("participant_key,fold",file.path(export,"participant_folds.csv"))
  saveRDS(m,file.path(export,"input_manifest.rds"))
  before<-tools::md5sum(file.path(export,"input_manifest.rds"))
  prepare<-e$reliability_prepare_inputs
  e$recovery_hash<-function(...)"fixture"
  e$recovery_pairs<-function(...)stop("ASSEMBLY_FORBIDDEN_IN_UNIT_TEST")
  stopifnot(identical(prepare(inputs),file.path("results/rq2/reliability_inputs","rq1-test","fixture")))
  e$reliability_prepare_inputs<-function(...)stop("BUILD_FORBIDDEN_IN_UNIT_TEST")
  stopifnot(identical(e$reliability_source()$path,export),e$last_allow_build,
    identical(e$reliability_source(export)$path,export),!e$last_allow_build)
  # Explicit selection never bypasses freshness validation; automatic selection
  # must reject the stale export and attempt the mocked preparation route.
  writeLines("source-v2",input)
  expect_error(e$reliability_source(export))
  err<-tryCatch(e$reliability_source(),error=identity)
  stopifnot(inherits(err,"error"),grepl("BUILD_FORBIDDEN",conditionMessage(err)))
  writeLines("source-v1",input)
  # Corrupt manifests are skipped during discovery, not reused or fatal to
  # discovery of a different valid export.
  corrupt<-file.path("results/rq2/reliability_inputs/rq1-test","corrupt")
  dir.create(corrupt);writeLines("not RDS",file.path(corrupt,"input_manifest.rds"))
  stopifnot(identical(e$reliability_source()$path,export))
  dup<-m;dup$statuses<-data.frame(task_index=c(1L,1L))
  stopifnot(is.null(e$reliability_input_files(dup,export)))
  unlink(file.path(export,"participant_folds.csv"))
  expect_error(e$reliability_source(export))
  expect_error(prepare(inputs))
  stopifnot(is.null(e$reliability_input_files(list(statuses=1),export)),
    identical(tools::md5sum(file.path(export,"input_manifest.rds")),before))
})
cat("PASS: cache freshness, bounded projection, injective design IDs, portable RQ1 paths (unit fixtures only)\n")

# Exercise only the actual CSV export statements, replacing the writer with an
# in-memory recorder. No analysis entrypoint or results directory is accessed.
local({
  previous <- Sys.getenv("MS_EXPORT_COMPAT_CSV", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("MS_EXPORT_COMPAT_CSV") else
            Sys.setenv(MS_EXPORT_COMPAT_CSV = previous))
  exports <- list(
    c("scripts/10_rq1_analysis.R", "summary", "rq1_pairwise_summary.csv", "rq1_summary.csv"),
    c("scripts/utils/rq1_relational_preservation.R", "rank_base",
      "rq1_relational_preservation.csv", "fig1_rank_preservation.csv"),
    c("scripts/utils/rq1_relational_preservation.R", "rank_dimension_summary",
      "rq1_relational_preservation_dimension_summary.csv", "fig1_panel_a_aggregated.csv"),
    c("scripts/utils/rq3_joint_projection.R", "pareto_summary",
      "rq3_pareto_frontiers.csv", "rq3_pareto_ever.csv")
  )
  record_writer <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(readr::write_csv))) x[[1]] <- as.name("write_csv")
    as.call(lapply(as.list(x), record_writer))
  }
  value <- data.frame(key = c("b", "a"), value = c(NA_real_, .125))
  for (spec in exports) {
    statements <- parse(spec[1])
    select_export <- function(filename) {
      hits <- Filter(function(x) {
        txt <- paste(deparse(x), collapse = " ")
        grepl("readr::write_csv", txt, fixed = TRUE) && grepl(paste0('"', filename, '"'), txt, fixed = TRUE)
      }, statements)
      stopifnot(length(hits) == 1L)
      record_writer(hits[[1]])
    }
    canonical <- select_export(spec[3]); alias <- select_export(spec[4])
    for (flag in c(NA_character_, "0", "1")) {
      if (is.na(flag)) Sys.unsetenv("MS_EXPORT_COMPAT_CSV") else Sys.setenv(MS_EXPORT_COMPAT_CSV = flag)
      e <- new.env(parent = baseenv()); e$OUT <- "unused"
      assign(spec[2], value, envir = e)
      writes <- list()
      e$write_csv <- function(x, file, na) writes[[basename(file)]] <<- list(data = x, na = na)
      eval(canonical, e); eval(alias, e)
      expected <- if (identical(flag, "1")) spec[3:4] else spec[3]
      stopifnot(identical(names(writes), expected),
                all(vapply(writes, function(x) identical(x, list(data = value, na = "")), logical(1))))
    }
  }
})
cat("PASS: canonical CSV exports are unchanged; four compatibility aliases are opt-in\n")
