# Canonical RQ2 plotting entrypoint. Plotting consumes frozen v5 outputs only;
# unlike the analysis runtime, the plot source requires no deployment-time patch.
source(file.path("scripts", "13_plot_rq2_v5.R"), local = .GlobalEnv)
