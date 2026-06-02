# requirements.R
# Documents the R packages this project needs. On this machine all of them are
# already installed; this script only installs what is missing.
#
# Run: Rscript requirements.R

cran <- c(
  "shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
  "scales", "here", "digest"
)

installed <- rownames(installed.packages())
missing <- setdiff(cran, installed)
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All CRAN dependencies already installed.")
}

# In-house PAQ packages (not on CRAN):
#   drukAPI – druk API client (provides API_connect/getDataAll/getFiltry)
#   paqr    – PAQ palette/theme helpers (optional; theme.R hardcodes the hexes)
# On a fresh machine install with:
#   remotes::install_github("paqresearch/drukAPI")
#   remotes::install_github("paqresearch/paqr")
if (!"drukAPI" %in% installed) {
  message("NOTE: 'drukAPI' is not installed — needed for pipeline/00_preprocess.R ",
          "and pipeline/01_extract.R. Install: remotes::install_github('paqresearch/drukAPI')")
}
