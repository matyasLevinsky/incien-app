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

# Same histogram as app/app.R's hist_plot(): scale = "lin" or "log".
log_breaks <- function(v) {
  lo <- floor(log10(min(v))); hi <- ceiling(log10(max(v)))
  10^(lo:hi)
}
na_subtitle <- function(col) {
  tot <- length(DATA[[col]]); n_na <- sum(is.na(DATA[[col]]))
  sprintf("Chybějící hodnoty (NA): %s z %s obcí (%.1f %%)",
          format(n_na, big.mark = " "), format(tot, big.mark = " "),
          100 * n_na / tot)
}
hist_plot <- function(col, scale = "log") {
  v <- DATA[[col]]
  v <- if (scale == "log") v[is.finite(v) & v > 0] else v[is.finite(v)]
  if (length(v) == 0) return(ggplot() + theme_void())
  suffix <- if (scale == "log") ", log škála" else ""
  p <- ggplot(data.frame(v = v), aes(v)) +
    geom_histogram(bins = 40, fill = PAQ$blue, colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = median(v), linetype = "dashed", colour = PAQ$merlot) +
    labs(x = sprintf("%s (%s%s)", EXP_LABS[[col]], EXP_UNITS[[col]], suffix),
         y = "Počet obcí", subtitle = na_subtitle(col)) +
    theme_paq_app(13) +
    theme(plot.subtitle = element_text(colour = PAQ$merlot, face = "bold"))
  if (scale == "log")
    p <- p + scale_x_log10(breaks = log_breaks(v),
                           labels = function(x) format(x, big.mark = " ",
                                                       scientific = FALSE, trim = TRUE))
  p
}

# Render BOTH a linear and a log version per variable so neither needs processing.
out_dir <- file.path(root, "render_check")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (col in EXP_COLS) for (scale in c("lin", "log")) {
  ggsave(file.path(out_dir, sprintf("hist_%s_%s.png", col, scale)),
         hist_plot(col, scale), width = 8, height = 4.5, dpi = 110)
}

message("Rendered ", length(EXP_COLS) * 2, " histogram PNGs (lin + log each) to:")
message(normalizePath(out_dir))
