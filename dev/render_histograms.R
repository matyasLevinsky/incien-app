# dev/render_histograms.R
# Local-only helper (NOT bundled by shinylive — lives outside app/). Renders the
# "Explorace dat" histogram for every selectable variable to PNGs so they can be
# eyeballed before deploying. Usage: Rscript dev/render_histograms.R
#
# Output folder is printed at the end. Images are .gitignored.

options(encoding = "UTF-8")
suppressMessages({ library(ggplot2); library(dplyr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

# Resolve repo root whether run from repo root or its parent.
root <- if (file.exists("app/app.R")) "." else "incien-app"
source(file.path(root, "app", "R", "data.R"))
source(file.path(root, "app", "R", "index.R"))
source(file.path(root, "app", "R", "theme.R"))

bundle <- load_processed(file.path(root, "app", "data", "processed", "municipalities.rds"))
DATA  <- bundle$data
LABS  <- index_labels();  UNITS <- index_units();  COLS <- index_cols()
COST  <- COST_COMPONENT$col

EXP_COLS  <- c(COLS, COST, "population", "density")
EXP_LABS  <- c(LABS, setNames(COST_COMPONENT$label, COST),
               population = "Počet obyvatel", density = "Hustota")
EXP_UNITS <- c(UNITS, setNames(COST_COMPONENT$unit, COST),
               population = "obyv.", density = "obyv./km²")

# Same histogram as app/app.R's hist_plot() — log x-axis, positive values only.
log_breaks <- function(v) {
  lo <- floor(log10(min(v))); hi <- ceiling(log10(max(v)))
  10^(lo:hi)
}
hist_plot <- function(col) {
  v <- DATA[[col]]; v <- v[is.finite(v) & v > 0]
  if (length(v) == 0) return(ggplot() + theme_void())
  ggplot(data.frame(v = v), aes(v)) +
    geom_histogram(bins = 40, fill = PAQ$blue, colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = median(v), linetype = "dashed", colour = PAQ$merlot) +
    scale_x_log10(breaks = log_breaks(v),
                  labels = function(x) format(x, big.mark = " ",
                                              scientific = FALSE, trim = TRUE)) +
    labs(x = sprintf("%s (%s, log škála)", EXP_LABS[[col]], EXP_UNITS[[col]]),
         y = "Počet obcí") +
    theme_paq_app(13)
}

out_dir <- file.path(root, "render_check")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (col in EXP_COLS) {
  ggsave(file.path(out_dir, paste0("hist_", col, ".png")),
         hist_plot(col), width = 8, height = 4.5, dpi = 110)
}

message("Rendered ", length(EXP_COLS), " histogram PNGs to:")
message(normalizePath(out_dir))
