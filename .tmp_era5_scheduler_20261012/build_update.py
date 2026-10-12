from pathlib import Path

root = Path(__file__).parent / 'bundle'
code = root / 'code'

def replace(text, old, new):
    assert text.count(old) == 1, (old[:100], text.count(old))
    return text.replace(old, new)

p = code / '02_stream_overrides.R'
s = p.read_text(encoding='utf-8-sig')
timer = '''# Elapsed (wall) seconds, never CPU seconds: stages are mutually exclusive.
# Timing sidecars are operational metadata and are not scientific checkpoints.
fast_stream_timer <- function() {
  e <- new.env(parent=emptyenv())
  e$started_at <- Sys.time()
  e$start <- e$last <- unname(proc.time()[["elapsed"]])
  e$stage <- "initialize_tile"
  stages <- c("initialize_tile","recover_checkpoint","validate_existing_checkpoint",
    "open_annual_history_files","read_history_chunk","evaluate_support","support_counts",
    "read_temperature_chunk","accumulate_all_weather_cdd","predict_supported_ab",
    "transform_warm_posterior","accumulate_supported_cdd","accumulate_warm_sums",
    "summarize_warm_tile","write_partition","checkpoint_write_validate","close_annual_files")
  e$seconds <- stats::setNames(numeric(length(stages)),stages)
  e
}

fast_stream_timer_switch <- function(timer,stage) {
  now <- unname(proc.time()[["elapsed"]])
  if (!timer$stage %in% names(timer$seconds)) timer$seconds[timer$stage] <- 0
  timer$seconds[timer$stage] <- timer$seconds[timer$stage] + now-timer$last
  timer$last <- now
  timer$stage <- stage
  invisible(NULL)
}

fast_stream_timer_snapshot <- function(timer) {
  fast_stream_timer_switch(timer,timer$stage)
  list(schema="ERA5-stream-timing-v1",started_at=timer$started_at,
    finished_at=Sys.time(),pid=Sys.getpid(),total_seconds=timer$last-timer$start,
    stage_seconds=timer$seconds)
}

'''
s = replace(s, '# Adapted original tile accumulator; original warm-curve definitions unchanged.\n', timer+'# Adapted original tile accumulator; original warm-curve definitions unchanged.\n')
s = replace(s, '  started <- Sys.time(); context <- list(stage = "initialize_tile", tile = task$tile_id)\n  set_context <- function(stage, ...) context <<- c(list(stage = stage, tile = task$tile_id), list(...))', '''  started <- Sys.time(); timer <- fast_stream_timer()
  context <- list(stage = "initialize_tile", tile = task$tile_id)
  set_context <- function(stage, ...) {
    fast_stream_timer_switch(timer,stage)
    context <<- c(list(stage = stage, tile = task$tile_id), list(...))
  }''')
s = replace(s, '    fast_daily_recover_warm(task, bundle, fingerprint, checkpoint_path, part_dir)', '    set_context("recover_checkpoint")\n    fast_daily_recover_warm(task, bundle, fingerprint, checkpoint_path, part_dir)')
s = replace(s, '        checkpoint = checkpoint_path, partition = filename, resumed = TRUE))', '        checkpoint = checkpoint_path, partition = filename, resumed = TRUE,\n        timing = fast_stream_timer_snapshot(timer)))')
s = replace(s, '    g <- bundle$g; fixed_set', '    set_context("initialize_tile")\n    g <- bundle$g; fixed_set')
s = replace(s, '        needed <- c("finite",', '        set_context("support_counts", chunk = info)\n        needed <- c("finite",')
s = replace(s, '        ta_mat <- read_rectangular_history', '        set_context("read_temperature_chunk", chunk = info)\n        ta_mat <- read_rectangular_history')
s = replace(s, '        if (any(finite_t)) for (si in seq_len(ns)) {', '        set_context("accumulate_all_weather_cdd", chunk = info)\n        if (any(finite_t)) for (si in seq_len(ns)) {')
s = replace(s, '        set_context("accumulate_warm_posterior", chunk = info)', '        set_context("transform_warm_posterior", chunk = info)')
s = replace(s, '        supported_cells <- cells[primary]', '        set_context("accumulate_supported_cdd", chunk = info)\n        supported_cells <- cells[primary]')
s = replace(s, '        selected <- rep(TRUE, sum(primary)); selected_cells <- cells[primary]', '        set_context("accumulate_warm_sums", chunk = info)\n        selected <- rep(TRUE, sum(primary)); selected_cells <- cells[primary]')
s = replace(s, '    set_context("commit_warm_tile")', '    set_context("write_partition")')
s = replace(s, '    fast_atomic_publish(checkpoint_path, function(temporary) {', '    set_context("checkpoint_write_validate")\n    fast_atomic_publish(checkpoint_path, function(temporary) {')
s = replace(s, '''    list(tile_id = tile_id, ncells = ncells, elapsed_seconds = cp$elapsed_seconds,
      checkpoint = checkpoint_path, partition = filename, resumed = FALSE)''', '''    set_context("close_annual_files")
    invisible(lapply(ncs,ncdf4::nc_close)); ncs <- list()
    timing <- fast_stream_timer_snapshot(timer)
    timing$tile_id <- tile_id; timing$fingerprint <- fingerprint
    timing$ncells <- ncells; timing$history_row_counts <- cp$history_row_counts
    timing$checkpoint_bytes <- unname(file.info(checkpoint_path)$size)
    timing_dir <- file.path(dirname(checkpoint_dir),"tile_timings")
    dir.create(timing_dir,recursive=TRUE,showWarnings=FALSE)
    fast_atomic_save_rds(timing,file.path(timing_dir,sprintf("tile_%05d.rds",tile_id)),replace=TRUE)
    list(tile_id = tile_id, ncells = ncells, elapsed_seconds = timing$total_seconds,
      checkpoint = checkpoint_path, partition = filename, resumed = FALSE, timing = timing)''')
s = replace(s, '      tile_id = task$tile_id, time = Sys.time(), context = context, error = conditionMessage(e),', '      tile_id = task$tile_id, time = Sys.time(), context = context, error = conditionMessage(e),\n      timing = fast_stream_timer_snapshot(timer),')
p.write_text(s,encoding='utf-8',newline='\n')

# Keep the supplied scientific reference byte-for-byte unchanged. This explicit
# runner override retains its setup/validation/final reduction, replacing only
# dispatch, progress and operational metadata. No string patching at run time.
s = (code/'warm02_reference.R').read_text(encoding='utf-8-sig')
s = s[s.index('run_global_era5 <- function(config, analysis = NULL) {'):]
s = s[:s.index('\n\n\n# ======================================================================')].rstrip()+'\n'
s = replace(s, '  g <- global_defaults(config); f <- fast_defaults(config)', '  g <- global_defaults(config); f <- fast_defaults(config)\n  scheduling <- fast_stream_scheduling(config$fast)')
s = replace(s, '  rate <- NA_real_; elapsed_new <- 0; cells_new <- 0L', '''  rate <- NA_real_; elapsed_new <- 0; cells_new <- 0L
  progress_interval <- scheduling$progress_seconds
  last_progress <- -Inf
  session_id <- paste0(format(Sys.time(),"%Y%m%dT%H%M%S"),"_",Sys.getpid())
  dispatch_file <- file.path(out_dir,paste0("dispatch_",session_id,".csv"))
  timing_file <- file.path(out_dir,paste0("tile_timings_",session_id,".csv"))''')
s = replace(s, '    utils::write.csv(do.call(rbind, progress), file.path(out_dir, "progress.csv"), row.names = FALSE)', '''    utils::write.csv(do.call(rbind, progress), file.path(out_dir, "progress.csv"), row.names = FALSE)
    last_progress <<- unname(proc.time()[["elapsed"]])''')
s = replace(s, '    message("All ", length(cl), " workers passed prediction QC; backend=", backend,', '    message("All ", length(cl), " workers passed prediction QC; backend=", backend,') # anchor assertion
begin = s.index('    run_wave <- function(ids, workers, phase) {')
end = s.index('    benchmark_file <- ',begin)
s = s[:begin]+'''    run_wave <- function(ids, workers, phase) {
      # IDs remain canonical; only the dispatch permutation changes.
      ids <- fast_stream_order_tiles(ids,cell_counts,scheduling$method)
      .set_analysis_stage(phase,tile_ids=ids,workers=workers,announce=TRUE)
      message("Continuous ",scheduling$method," queue: ",length(ids)," tiles (",
        sum(cell_counts[ids])," cells), ",workers," workers; no 2W batch barrier.")
      t0 <- unname(proc.time()[["elapsed"]]); completed_cells <- 0L
      fast_stream_append_csv(data.frame(session_id=session_id,phase=phase,
        dispatch_rank=seq_along(ids),tile_id=ids,n_cells=cell_counts[ids],
        cost_proxy=cell_counts[ids],method=scheduling$method),dispatch_file)
      on_result <- function(answer,task,worker,rank) {
        i <- as.integer(task$tile_id)
        if (!identical(as.integer(answer$tile_id),i) ||
            !identical(as.integer(answer$ncells),as.integer(cell_counts[i])) ||
            !file.exists(checkpoint_paths[i])) stop("Worker returned invalid tile identity/commit: ",i)
        done[i] <<- TRUE
        completed_cells <<- completed_cells+cell_counts[i]
        seconds <- unname(proc.time()[["elapsed"]])-t0
        rate <<- (cells_new+completed_cells)/max(elapsed_new+seconds,.001)
        fast_stream_append_csv(fast_stream_timing_row(answer,session_id,phase,worker,rank),timing_file)
        if (unname(proc.time()[["elapsed"]])-last_progress >= progress_interval || all(done))
          record_progress(phase,workers,seconds,completed_cells)
      }
      fast_stream_queue(cl[seq_len(workers)],tasks[ids],fast_worker_task,on_result)
      seconds <- unname(proc.time()[["elapsed"]])-t0
      list(seconds=seconds,cells=completed_cells,rate=completed_cells/max(seconds,.001))
    }
'''+s[end:]
s = replace(s, '      message("Reusing completed 8/12-worker selection from this unchanged run: ", chosen_workers, " workers.")', '      message("Reusing completed worker selection from this unchanged run: ", chosen_workers, " workers.")')
s = replace(s, '''    while (any(!done)) {
      remaining <- which(!done)
      ids <- head(remaining, 2L * chosen_workers)
      wave <- run_wave(ids, chosen_workers, "parallel_global_projection")''', '''    if (any(!done)) {
      wave <- run_wave(which(!done), chosen_workers, "parallel_global_projection")''')
s = replace(s, '    daily_cache_completion = cache_completion,', '''    scheduling = scheduling, scheduling_cost_proxy = "selected cells per tile; not measured runtimes",
    dispatch_log = dispatch_file, tile_timing_log = timing_file,
    daily_cache_completion = cache_completion,''')
header = '''# Continuous PSOCK scheduling for the streamed server runner.
# Source AFTER 02_stream_overrides.R. Scientific setup, checkpoint validation,
# fixed draw IDs and final tile-ID reduction follow warm02_reference.R unchanged.
# R parallel's sendCall/recvOneResult are internal APIs, isolated and exercised
# by 10_test_parallel_runtime.R on the installed R version before production.

fast_stream_scheduling <- function(fast) {
  method <- if (is.null(fast$scheduling)) "lpt" else fast$scheduling
  interval <- if (is.null(fast$progress_seconds)) 30 else fast$progress_seconds
  if (!is.character(method) || length(method)!=1L || is.na(method) ||
      !method %in% c("lpt","fifo")) stop("fast$scheduling must be 'lpt' or 'fifo'.")
  if (!is.numeric(interval) || length(interval)!=1L || !is.finite(interval) || interval<=0)
    stop("fast$progress_seconds must be positive.")
  list(method=method,progress_seconds=interval,version="continuous-psock-v1")
}

fast_stream_order_tiles <- function(ids,cell_counts,method="lpt") {
  if (!is.numeric(ids) || anyNA(ids) || any(ids!=as.integer(ids)) ||
      any(ids<1L | ids>length(cell_counts)) || anyDuplicated(ids) ||
      any(!is.finite(cell_counts[ids]) | cell_counts[ids]<=0)) stop("Invalid scheduling IDs/costs.")
  ids <- as.integer(ids)
  if (!method %in% c("lpt","fifo")) stop("Unknown scheduling method.")
  if (method=="lpt") ids[order(-cell_counts[ids],ids)] else ids
}

fast_stream_append_csv <- function(row,path) {
  exists <- file.exists(path) && file.info(path)$size>0
  utils::write.table(row,path,sep=",",row.names=FALSE,col.names=!exists,
    append=exists,quote=TRUE,na="NA")
  invisible(NULL)
}

fast_stream_timing_row <- function(answer,session_id,phase,worker,rank) {
  t <- answer$timing
  if (!is.list(t) || !identical(t$schema,"ERA5-stream-timing-v1") ||
      any(!is.finite(t$stage_seconds) | t$stage_seconds<0) ||
      !is.finite(t$total_seconds) || t$total_seconds<0) stop("Missing/invalid worker timing.")
  stages <- fast_stream_timer()$seconds
  if (any(!names(t$stage_seconds) %in% names(stages))) stop("Unknown timing stage.")
  stages[names(t$stage_seconds)] <- t$stage_seconds
  rows <- t$history_row_counts
  row <- data.frame(session_id=session_id,phase=phase,tile_id=answer$tile_id,
    n_cells=answer$ncells,worker=worker,pid=t$pid,dispatch_rank=rank,
    resumed=isTRUE(answer$resumed),started_at=as.character(t$started_at),
    finished_at=as.character(t$finished_at),total_seconds=t$total_seconds,
    history_rows=if(is.null(rows)) NA_real_ else unname(rows["all"]),
    predicted_rows=if(is.null(rows)) NA_real_ else unname(rows["predicted"]),
    checkpoint_bytes=if(is.null(t$checkpoint_bytes)) NA_real_ else t$checkpoint_bytes)
  for (nm in names(stages)) row[[paste0(nm,"_seconds")]] <- unname(stages[nm])
  row
}

fast_stream_task_envelope <- function(task,fun) {
  tryCatch(list(ok=TRUE,value=fun(task)),error=function(e)
    list(ok=FALSE,tile_id=task$tile_id,message=conditionMessage(e)))
}

fast_stream_queue <- function(cl,tasks,fun,on_result=function(...) NULL) {
  n <- length(tasks); workers <- length(cl)
  if (!n) return(invisible(list()))
  if (!workers) stop("Continuous queue requires at least one worker.")
  ids <- vapply(tasks,function(x) as.integer(x$tile_id),integer(1))
  if (anyNA(ids) || anyDuplicated(ids)) stop("Queue task IDs must be unique.")
  send <- getFromNamespace("sendCall","parallel")
  receive <- getFromNamespace("recvOneResult","parallel")
  active <- rep(NA_integer_,workers); next_job <- 1L
  result <- vector("list",n); failures <- character()
  submit <- function(node) {
    j <- next_job; next_job <<- next_job+1L
    send(cl[[node]],fast_stream_task_envelope,list(tasks[[j]],fun),tag=j)
    active[node] <<- j
  }
  for (node in seq_len(min(workers,n))) submit(node)
  while (any(!is.na(active))) {
    received <- receive(cl)
    node <- received$node; j <- received$tag
    if (length(node)!=1L || is.na(node) || node<1L || node>workers ||
        length(j)!=1L || is.na(j) || is.na(active[node]) || active[node]!=j)
      stop("Continuous queue received an invalid worker/tag.")
    active[node] <- NA_integer_
    value <- received$value
    if (is.list(value) && isTRUE(value$ok)) {
      error <- tryCatch({
        on_result(value$value,tasks[[j]],node,j)
        NULL
      },error=function(e) conditionMessage(e))
      if (is.null(error)) result[j] <- list(value$value) else
        failures <- c(failures,paste0("tile ",ids[j]," callback: ",error))
    } else {
      msg <- if (is.list(value) && is.character(value$message)) value$message else as.character(value)
      failures <- c(failures,paste0("tile ",ids[j],": ",paste(msg,collapse=" ")))
    }
    if (length(failures)==1L && any(!is.na(active)))
      message("Queue stopped dispatching after an error; waiting for running tiles to commit.")
    # On error, drain outstanding work and retain all successful commits. Never
    # silently drop an error, retry within this session, or enqueue fresh tasks.
    if (!length(failures) && next_job<=n) submit(node)
  }
  if (length(failures)) stop("Continuous queue failed; committed tiles can be resumed. ",
    paste(unique(failures),collapse="; "),call.=FALSE)
  invisible(result)
}

'''
(code/'09_parallel_runtime.R').write_text(header+s,encoding='utf-8',newline='\n')

p=code/'03_run_ecs.R'; s=p.read_text(encoding='utf-8-sig')
s=replace(s,'  source(file.path(base,"02_stream_overrides.R"),local=.GlobalEnv)', '  source(file.path(base,"02_stream_overrides.R"),local=.GlobalEnv)\n  source(file.path(base,"09_parallel_runtime.R"),local=.GlobalEnv)')
s=replace(s,'      resume=r$resume,runtime_selftest=FALSE,target_total_hours=30),', '      resume=r$resume,runtime_selftest=FALSE,target_total_hours=30,\n      scheduling=r$scheduling,progress_seconds=r$progress_seconds),')
s=replace(s,'      engine_md5=unname(tools::md5sum(file.path(base,"02_stream_overrides.R"))),', '      engine_md5=unname(tools::md5sum(file.path(base,"02_stream_overrides.R"))),\n      parallel_runtime_md5=unname(tools::md5sum(file.path(base,"09_parallel_runtime.R"))),')
p.write_text(s,encoding='utf-8',newline='\n')
p=root/'config.R'; s=p.read_text(encoding='utf-8-sig')
s=replace(s,'    workers = 32L,','    workers = 32L,\n    scheduling = "lpt", # Continuous queue; longest estimated tile first.\n    progress_seconds = 30, # Update on completed tiles, at most this often.')
p.write_text(s,encoding='utf-8',newline='\n')
p=code/'07_check_server.R'; s=p.read_text(encoding='utf-8-sig')
s=replace(s,'source(file.path(base,"02_stream_overrides.R"))','source(file.path(base,"02_stream_overrides.R"))\nsource(file.path(base,"09_parallel_runtime.R"))\nfast_stream_scheduling(cfg$run)')
p.write_text(s,encoding='utf-8',newline='\n')
p=root/'run_server.sh'; s=p.read_text(encoding='utf-8-sig')
s=replace(s,'    Rscript --vanilla code/06_test_regressions.R','    Rscript --vanilla code/06_test_regressions.R\n    Rscript --vanilla code/10_test_parallel_runtime.R')
p.write_text(s,encoding='utf-8',newline='\n')
