# R/theme.R
# PAQ Research visual identity for the Shiny app. Hex values mirror the shared
# PAQ-Theme / paqr palette so this app looks consistent with other PAQ outputs.

# Core PAQ palette (see ~/PAQ-Theme/PAQ_brand.md).
PAQ <- list(
  night_blue  = "#001056",
  blue        = "#004ae7",
  green       = "#00cc74",
  pink        = "#ffd4d4",
  merlot      = "#720030",
  yellow      = "#f0d42e",
  light_blue  = "#cfe2ff",
  light_green = "#bdeecd",
  grid        = "#CDD1D9",
  caption     = "#39404D"
)

# bslib theme for page_sidebar(). Green primary, night-blue headings.
paq_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary    = PAQ$green,
    secondary  = PAQ$night_blue,
    success    = PAQ$green,
    base_font  = bslib::font_google("Source Sans 3", local = FALSE),
    heading_font = bslib::font_google("Source Sans 3", local = FALSE),
    "headings-color" = PAQ$night_blue
  )
}

# Minimal ggplot2 theme matching the report look (sans, subtle grid).
theme_paq_app <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = PAQ$grid, linewidth = 0.25),
      plot.title.position = "plot",
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(colour = PAQ$night_blue, face = "bold")
    )
}
