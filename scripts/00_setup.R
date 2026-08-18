# Environment setup
# Match upstream package versions before beginning analysis.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize only if this is a fresh project.
# renv::init(bare = TRUE)

# Add package installation / restoration steps here.
# Finish with:
# renv::snapshot()
# writeLines(capture.output(sessionInfo()), "logs/sessionInfo_setup.txt")
