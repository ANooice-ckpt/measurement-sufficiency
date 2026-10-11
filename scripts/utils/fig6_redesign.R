# Fig. 6 display only. Inputs are frozen joint/task decisions and display grids.
# No new pooling, sufficiency classification, Pareto calculation or fitting.
# Explain a single frozen facet/tolerance slice in its existing display table.
# Axis requirements are conditional slices, not coordinates of one optimum.
ms_fig6_task_explanation <- function(tasks, inventory) {
  classes <- stats::setNames(inventory$metric_class, inventory$metric)
  collect_targets <- function(x) paste(sort(unique(unlist(strsplit(x[nzchar(x)], ";", fixed = TRUE)))), collapse = ";")
  target_classes <- function(x) paste(sort(unique(unname(classes[strsplit(x, ";", fixed = TRUE)[[1L]]]))), collapse = ";")
  by_interval <- tasks |> dplyr::group_by(task_id, resolution_s) |>
    dplyr::summarise(required_days_at_interval = if (any(sufficient %in% TRUE))
      min(n_days[sufficient %in% TRUE]) else NA_real_, .groups = "drop")
  by_days <- tasks |> dplyr::group_by(task_id, n_days) |>
    dplyr::summarise(required_interval_at_days_s = if (any(sufficient %in% TRUE))
      max(resolution_s[sufficient %in% TRUE]) else NA_real_, .groups = "drop")
  limits <- tasks |> dplyr::group_by(task_id) |>
    dplyr::summarise(frontier_limiting_targets = collect_targets(limiting_targets[pareto %in% TRUE]),
                     .groups = "drop")
  tasks <- tasks |>
    dplyr::mutate(
      limiting_target_classes = vapply(limiting_targets, target_classes, character(1)),
      # Level and dynamics are alternative goals, not nested target sets.
      comparison_task = unname(c(`temporal dynamics` = "level", all_targets = "temporal dynamics")[as.character(task_id)]),
      comparison_relation = dplyr::case_when(task_id == "temporal dynamics" ~ "alternative_target_bundle",
        task_id == "all_targets" ~ "expanded_target_bundle", TRUE ~ NA_character_)
    ) |>
    dplyr::left_join(limits, by = "task_id") |>
    dplyr::left_join(by_interval, by = c("task_id", "resolution_s")) |>
    dplyr::left_join(by_days, by = c("task_id", "n_days")) |>
    dplyr::left_join(dplyr::rename(by_interval, comparison_days = required_days_at_interval),
      by = c("comparison_task" = "task_id", "resolution_s")) |>
    dplyr::left_join(dplyr::rename(by_days, comparison_interval_s = required_interval_at_days_s),
      by = c("comparison_task" = "task_id", "n_days")) |>
    dplyr::mutate(
      extra_days_vs_comparison_at_interval = required_days_at_interval - comparison_days,
      sampling_rate_ratio_vs_comparison_at_days = comparison_interval_s / required_interval_at_days_s
    ) |>
    dplyr::select(-comparison_days, -comparison_interval_s)
  # Read the immediate lower-burden neighbours on the frozen lattice. Missing
  # cells are never skipped. Only a frozen FALSE decision identifies a blocker;
  # the current sufficient cell's largest R_task need not identify that metric.
  intervals <- sort(ms_primary_temporal_s())
  tasks <- tasks |>
    dplyr::mutate(shorter_n_days = n_days - 1L,
      coarser_resolution_s = c(intervals[-1L], NA_real_)[match(resolution_s, intervals)]) |>
    dplyr::left_join(tasks |> dplyr::transmute(task_id, resolution_s, shorter_n_days = n_days,
      shorter_status = status, shorter_sufficient = sufficient, shorter_R_task = R_task,
      shorter_limiting_targets = limiting_targets), by = c("task_id", "resolution_s", "shorter_n_days")) |>
    dplyr::left_join(tasks |> dplyr::transmute(task_id, n_days, coarser_resolution_s = resolution_s,
      coarser_status = status, coarser_sufficient = sufficient, coarser_R_task = R_task,
      coarser_limiting_targets = limiting_targets), by = c("task_id", "n_days", "coarser_resolution_s")) |>
    dplyr::mutate(
      shorter_status = dplyr::coalesce(shorter_status, ifelse(shorter_n_days < 1L, "outside_domain", "not_observed")),
      coarser_status = dplyr::coalesce(coarser_status, ifelse(is.na(coarser_resolution_s), "outside_domain", "not_observed")),
      shorter_blocking_targets = ifelse(pareto %in% TRUE & shorter_status == "resolved" &
        shorter_sufficient %in% FALSE, shorter_limiting_targets, NA_character_),
      coarser_blocking_targets = ifelse(pareto %in% TRUE & coarser_status == "resolved" &
        coarser_sufficient %in% FALSE, coarser_limiting_targets, NA_character_)) |>
    dplyr::select(-shorter_limiting_targets, -coarser_limiting_targets)
  # One compact key per exact limiting set retains all ties without long cell
  # labels. The key and full identities travel in the existing display CSV.
  blocker_sets <- sort(unique(stats::na.omit(c(tasks$shorter_blocking_targets, tasks$coarser_blocking_targets))))
  tasks |>
    dplyr::mutate(shorter_blocker_key = match(shorter_blocking_targets, blocker_sets),
                  coarser_blocker_key = match(coarser_blocking_targets, blocker_sets))
}

ms_fig6_redesign <- function(entry, tasks, classes, resolution_labels, days, composition,
                             prep_only = FALSE) {
  ink <- "#30363B"
  muted <- "#657078"
  unresolved <- "#E1E5E7"
  base <- theme_minimal(base_size = 7, base_family = MS_FONT) +
    theme(
      panel.grid = element_blank(), axis.ticks = element_blank(),
      axis.text = element_text(size = 6.5, colour = ink),
      axis.title = element_text(size = 7, colour = ink),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      strip.text = element_text(size = 7, face = "bold", colour = ink, margin = margin(b = 5)),
      strip.background = element_blank(),
      panel.spacing.x = grid::unit(2.4, "mm"),
      legend.position = "bottom", legend.title = element_text(size = 6.5),
      legend.text = element_text(size = 6), legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = margin(3, 5, 3, 3)
    )
  lattice_axes <- list(
    scale_x_continuous(breaks = seq_along(resolution_labels), labels = resolution_labels,
                       limits = c(.5, length(resolution_labels) + .5), expand = expansion(0)),
    scale_y_continuous(breaks = days, limits = c(min(days) - .5, max(days) + .5), expand = expansion(0))
  )

  # a: global joint geometry. Use the range of the displayed medians, not the
  # unplotted individual-metric tail; numbers retain exact reading precision.
  entry$display_label <- ifelse(entry$cell_unresolved, "U",
                                 ifelse(is.finite(entry$epsilon_entry_median), sprintf("%.2f", entry$epsilon_entry_median), "NA"))
  entry$heading <- "Median entry tolerance"
  entry_max <- max(entry$epsilon_entry_median[!entry$cell_unresolved], na.rm = TRUE)
  a <- ggplot(entry, aes(resolution_rank, n_days)) +
    geom_tile(aes(fill = epsilon_entry_median), width = .97, height = .97, colour = "white", linewidth = .35) +
    geom_tile(data = entry[entry$cell_unresolved, ], fill = unresolved, width = .97, height = .97) +
    geom_text(aes(label = display_label,
                  colour = !cell_unresolved & is.finite(epsilon_entry_median) & epsilon_entry_median > .63 * entry_max),
              size = 2.25, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = "white")) +
    scale_fill_gradientn(colours = c("#F5F1E9", "#DCC8A3", "#AA7E45", "#704B2B"),
                          limits = c(0, entry_max), na.value = unresolved,
                          breaks = c(0, .25, .50, .75), name = "Entry tolerance") +
    facet_wrap(~heading) + lattice_axes +
    labs(x = "Sampling interval", y = "Monitoring duration (days)") + base +
    guides(fill = guide_colorbar(title.position = "top", barwidth = grid::unit(32, "mm"),
                                 barheight = grid::unit(2, "mm")))

  # b: preserve every required target, rather than counting how frequently an
  # individually optimal state occurs. Sufficiency and Pareto flags are frozen.
  tasks$decision <- factor(ifelse(is.na(tasks$sufficient), NA_character_,
    ifelse(tasks$pareto, "Pareto", ifelse(tasks$sufficient, "Sufficient", "Not sufficient"))),
    levels = c("Not sufficient", "Sufficient", "Pareto"))
  task_names <- c(level = "Level", `temporal dynamics` = "Temporal dynamics", all_targets = "All targets")
  tasks$task_label <- paste0(unname(task_names[as.character(tasks$task_id)]), "\n", tasks$n_required, " targets",
    ifelse(tasks$task_id == "level", "\nStarting goal",
           ifelse(tasks$task_id == "temporal dynamics", "\nvs Level", "\nvs Temporal dynamics")))
  tasks$task_label <- factor(tasks$task_label, levels = unique(tasks$task_label[order(tasks$task_id)]))
  # Overlay every frozen comparison-frontier cell; no point matching or implied
  # unique optimum. Level -> dynamics changes goals; dynamics -> all adds targets.
  comparison_frontier <- tasks |>
    dplyr::distinct(task_id, task_label, comparison_task) |>
    dplyr::filter(!is.na(comparison_task)) |>
    dplyr::inner_join(tasks |> dplyr::filter(pareto %in% TRUE) |>
      dplyr::transmute(comparison_task = as.character(task_id), resolution_rank, n_days),
      by = "comparison_task", relationship = "many-to-many")
  blocker_key <- dplyr::bind_rows(
    tasks |> dplyr::transmute(key = shorter_blocker_key, targets = shorter_blocking_targets),
    tasks |> dplyr::transmute(key = coarser_blocker_key, targets = coarser_blocking_targets)) |>
    dplyr::filter(!is.na(key)) |> dplyr::distinct() |> dplyr::arrange(key)
  blocker_caption <- if (nrow(blocker_key)) paste0(
    "D: one fewer day; S: next slower interval; +: tied maxima.\n",
    stringr::str_wrap(paste0(blocker_key$key, ": ", gsub("_", " ", gsub(";", " + ", blocker_key$targets)),
                            collapse = ";  "), width = 100)) else NULL
  b <- ggplot(tasks, aes(resolution_rank, n_days)) +
    geom_tile(aes(fill = decision), colour = "white", width = .97, height = .97, linewidth = .3) +
    geom_text(data = tasks[which(tasks$sufficient & tasks$pareto), ],
      aes(label = sprintf("%g\u00d7", nominal_sample_reduction)),
      colour = "white", size = 2, fontface = "bold") +
    geom_text(data = tasks[tasks$status != "resolved", ],
      aes(label = ifelse(status == "unavailable", "NA", "U")), size = 2, colour = muted) +
    geom_tile(data = comparison_frontier, fill = NA, colour = ink, linetype = "22",
              width = .89, height = .89, linewidth = .35) +
    geom_text(data = tasks[!is.na(tasks$shorter_blocker_key), ],
      aes(y = n_days - .30, label = paste0("D", shorter_blocker_key)),
      colour = "white", size = 1.85) +
    geom_text(data = tasks[!is.na(tasks$coarser_blocker_key), ],
      aes(y = n_days + .30, label = paste0("S", coarser_blocker_key)),
      colour = "white", size = 1.85) +
    scale_fill_manual(values = c(`Not sufficient` = "#FCFDFD", Sufficient = "#CCDDE0", Pareto = "#5F8B9E"),
                      na.value = unresolved, drop = FALSE, name = NULL) +
    facet_wrap(~task_label, nrow = 1) + lattice_axes +
    labs(x = "Sampling interval", y = NULL, caption = paste0(
      "Eye/MEDI; dashed boxes: comparison frontier.\n", blocker_caption)) + base +
    theme(legend.box = "vertical", legend.spacing.y = grid::unit(1, "mm"),
          plot.caption = element_text(size = 5.2, hjust = 0, colour = ink, lineheight = 1.05),
          legend.key.width = grid::unit(4, "mm"), legend.key.height = grid::unit(2.5, "mm")) +
    guides(fill = guide_legend(nrow = 1))

  # c: re-express all four original 6 x 6 class fields as discrete-day profiles.
  # Heavy endpoint cadences reveal sampling sensitivity; middle cadences remain
  # visible without four more redundant heatmaps. Segments only connect observed
  # states. No envelope, fitted trend or new class statistic is calculated.
  classes$cadence <- ifelse(classes$resolution_rank == 1, "120 s",
                            ifelse(classes$resolution_rank == length(resolution_labels), "10 s", "20-60 s"))
  classes$cadence <- factor(classes$cadence, levels = c("120 s", "10 s", "20-60 s"))
  c <- ggplot(classes, aes(n_days, suff_fraction, group = resolution_rank, colour = metric_class)) +
    geom_hline(yintercept = c(0, .5, 1), colour = "#E8ECEE", linewidth = .20) +
    geom_line(data = classes[classes$cadence == "20-60 s", ], linewidth = .35, alpha = .32, na.rm = TRUE) +
    geom_line(data = classes[classes$cadence != "20-60 s", ], aes(linetype = cadence), linewidth = .60, na.rm = TRUE) +
    geom_point(data = classes[classes$cadence != "20-60 s", ], aes(shape = cadence), size = 1.45, stroke = .45, fill = "white", na.rm = TRUE) +
    facet_wrap(~class_label, nrow = 1) +
    scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
    scale_linetype_manual(values = c(`120 s` = "solid", `10 s` = "22"), breaks = c("120 s", "10 s"), name = "Sampling interval") +
    scale_shape_manual(values = c(`120 s` = 16, `10 s` = 21), breaks = c("120 s", "10 s"), name = "Sampling interval") +
    scale_x_continuous(breaks = days, limits = range(days) + c(-.15, .15), expand = expansion(0)) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = scales::label_percent(accuracy = 1),
                       limits = c(0, 1), expand = expansion(add = .04)) +
    labs(x = "Monitoring duration (days)", y = "Metrics sufficient") + base +
    guides(linetype = guide_legend(title.position = "left", nrow = 1),
           shape = guide_legend(title.position = "left", nrow = 1))

  class_profiles <- c
  # Counts and sufficiency decisions are frozen by the analysis. This helper
  # only selects display slices; it does not reconstruct a joint decision.
  failure_cells <- composition |>
    dplyr::filter(summary_scope=="cell_all_facets",abs(epsilon-.25)<1e-12) |>
    dplyr::mutate(resolution_rank=match(resolution_s,rev(ms_primary_temporal_s())),
      cell_label=dplyr::case_when(n_resolved==0 ~ "U",n_axis_pass==0 ~ "-",
                                  TRUE ~ paste0(n_failure,"/",n_axis_pass)))
  failure_map <- ggplot(failure_cells,aes(resolution_rank,n_days)) +
    geom_tile(aes(fill=failure_rate),width=.97,height=.97,colour="white",linewidth=.2) +
    geom_text(aes(label=cell_label),size=1.9,colour=ink) +
    lattice_axes +
    scale_fill_gradient(low="#F1F5F5",high="#C78257",limits=c(0,1),na.value=unresolved,
      labels=scales::label_percent(),breaks=c(0,.5,1),name="Joint failures / axis passes") +
    labs(x="Sampling interval",y="Days",title="Composition failures at epsilon = 0.25") + base +
    theme(plot.title=element_text(size=6.5,face="bold")) +
    guides(fill=guide_colorbar(title.position="top",barwidth=grid::unit(30,"mm"),barheight=grid::unit(2,"mm")))
  failure_curves <- composition |>
    dplyr::filter(summary_scope=="placement_optical")
  failure_curve <- ggplot(failure_curves,
    aes(epsilon,failure_rate,colour=placement,linetype=optical,group=interaction(placement,optical))) +
    geom_line(linewidth=.45,na.rm=TRUE) + geom_point(size=1.2,na.rm=TRUE) +
    scale_colour_manual(values=c(eye="#31586A",chest="#B2774F",wrist="#528C87")) +
    scale_linetype_manual(values=c(MEDI="solid",LIGHT="22")) +
    scale_y_continuous(limits=c(0,1),breaks=c(0,.5,1),labels=scales::label_percent()) +
    scale_x_continuous(breaks=c(.05,.25,.5,1)) +
    labs(x="Tolerance epsilon",y="Joint failures / axis passes",colour="Placement",linetype="Optical",
         title="Failure across evaluated tolerances") + base +
    theme(plot.title=element_text(size=6.5,face="bold"))
  c <- cowplot::plot_grid(failure_map,failure_curve,nrow=1,rel_widths=c(.44,.56))
  facet_cells <- composition |>
    dplyr::filter(summary_scope=="cell_placement_optical",abs(epsilon-.25)<1e-12) |>
    dplyr::mutate(resolution_rank=match(resolution_s,rev(ms_primary_temporal_s())),
      cell_label=dplyr::case_when(n_resolved==0 ~ "U",n_axis_pass==0 ~ "-",
                                  TRUE ~ paste0(n_failure,"/",n_axis_pass)))
  composition_facets <- ggplot(facet_cells,aes(resolution_rank,n_days)) +
    geom_tile(aes(fill=failure_rate),colour="white") + geom_text(aes(label=cell_label),size=2) +
    facet_grid(optical~placement) + lattice_axes +
    scale_fill_gradient(low="#F1F5F5",high="#C78257",limits=c(0,1),na.value=unresolved,
      labels=scales::label_percent(),name="Failure rate") +
    labs(x="Sampling interval",y="Days",title="Single-axis composition at epsilon = 0.25",
      subtitle="Numbers: failures / single-axis passes; U: axis unresolved; -: no axis passes") + base

  # Align the plotting fields before adding fixed physical title bands.
  a <- ms_style_plot(a)
  b <- ms_style_plot(b)
  failure_map <- ms_style_plot(failure_map, subpanel=TRUE) +
    labs(title="At epsilon = 0.25")
  failure_curve <- ms_style_plot(failure_curve, subpanel=TRUE) +
    labs(title="Across tolerances")
  left <- cowplot::align_plots(a,failure_map,align="v",axis="l")
  right <- cowplot::align_plots(b,failure_curve,align="v",axis="lr")
  aligned <- cowplot::align_plots(left[[1]],right[[1]],align="h",axis="tb")
  lower <- cowplot::align_plots(left[[2]],right[[2]],align="h",axis="tb")
  top <- cowplot::plot_grid(ms_panel_frame(aligned[[1]],"a  Joint stability"),
    ms_panel_frame(aligned[[2]],"b  Target-specific designs (epsilon = 0.50)"),
    nrow=1,rel_widths=c(.30,.70))
  c <- cowplot::plot_grid(plotlist=lower,nrow=1,rel_widths=c(.30,.70))
  note <- cowplot::ggdraw()+cowplot::draw_label(
    paste0("b: multipliers = sample reduction vs 10 s x 6 d; Level / dynamics: alternative goals; All targets: expanded goal.\n",
      "U: unresolved; NA: unavailable. c: joint failures / axis passes; -: no axis passes; counts are metric-facet-states."),
    x=.012,hjust=0,fontfamily=MS_FONT,size=MS_FIG_DENSE_PT,colour=ink)
  final <- cowplot::plot_grid(top,ms_panel_frame(c,"c  Do single-axis rules compose?",style=FALSE),note,
    ncol=1,rel_heights=c(.59,.36,.05))
  list(plot=final,a=a,b=b,c=c,class_profiles=ms_style_plot(class_profiles),
       composition_facets=ms_style_plot(composition_facets))
}
