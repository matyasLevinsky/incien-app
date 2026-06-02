# app/app.R
# Incien waste-index explorer. Reads data/processed/municipalities.rds and lets
# the user weight the index components, filter by population/density, and see
# each municipality's score plus the top 10 and bottom 10.
#
# Run: Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'

options(encoding = "UTF-8")

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(here)

source(here::here("R", "data.R"))
source(here::here("R", "index.R"))
source(here::here("R", "theme.R"))

# ── Load data once at startup ────────────────────────────────────────────────
bundle <- tryCatch(load_processed(), error = function(e) e)
data_ok <- !inherits(bundle, "error")

if (data_ok) {
  DATA  <- bundle$data
  COMPS <- bundle$components
  DIRS  <- index_directions()
  LABS  <- index_labels()
  COLS  <- index_cols()

  rng <- function(col) {
    if (!col %in% names(DATA)) return(c(0, 1))
    v <- DATA[[col]][is.finite(DATA[[col]])]
    if (length(v) == 0) c(0, 1) else range(v)
  }
  pop_rng <- rng("population")
  den_rng <- rng("density")
}

# Slider label with direction hint.
slider_label <- function(col) {
  arrow <- if (identical(DIRS[[col]], "lower")) "↓ nižší = lepší" else "↑ vyšší = lepší"
  paste0(LABS[[col]], "  (", arrow, ")")
}

# ── UI ────────────────────────────────────────────────────────────────────────
if (!data_ok) {
  ui <- page_fillable(
    theme = paq_bs_theme(),
    card(
      card_header("Data nejsou připravena"),
      card_body(
        p("Zpracovaná data nebyla nalezena. Spusťte pipeline:"),
        tags$pre("Rscript pipeline/01_extract.R   # potřebuje přístup k druk API\nRscript pipeline/02_process.R"),
        p(tags$small(conditionMessage(bundle)))
      )
    )
  )
} else {
  ui <- page_sidebar(
    title = "Index odpadového hospodářství obcí",
    theme = paq_bs_theme(),
    sidebar = sidebar(
      width = 340,
      h6("Váhy komponent indexu"),
      lapply(COLS, function(col) {
        sliderInput(paste0("w_", col), slider_label(col),
                    min = 0, max = 100, value = 50, step = 5)
      }),
      hr(),
      h6("Filtry"),
      sliderInput("f_pop", "Počet obyvatel",
                  min = floor(pop_rng[1]), max = ceiling(pop_rng[2]),
                  value = c(floor(pop_rng[1]), ceiling(pop_rng[2]))),
      sliderInput("f_den", "Hustota (obyv./km²)",
                  min = floor(den_rng[1]), max = ceiling(den_rng[2]),
                  value = c(floor(den_rng[1]), ceiling(den_rng[2])))
    ),
    layout_columns(
      fill = FALSE,
      value_box("Obcí ve výběru", textOutput("n_munis"), theme = "secondary"),
      value_box("Hodnocených (se skóre)", textOutput("n_scored"), theme = "primary")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Top 10 obcí"),
           plotOutput("top_plot", height = "260px"),
           DTOutput("top_tbl")),
      card(card_header("Bottom 10 obcí"),
           plotOutput("bot_plot", height = "260px"),
           DTOutput("bot_tbl"))
    ),
    card(
      card_header("Celkové pořadí"),
      DTOutput("full_tbl")
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  if (!data_ok) return(invisible(NULL))

  weights <- reactive({
    setNames(vapply(COLS, function(col) input[[paste0("w_", col)]] %||% 0, numeric(1)), COLS)
  })

  filtered <- reactive({
    d <- DATA
    if ("population" %in% names(d)) {
      d <- d[!is.na(d$population) & d$population >= input$f_pop[1] & d$population <= input$f_pop[2], ]
    }
    if ("density" %in% names(d)) {
      d <- d[!is.na(d$density) & d$density >= input$f_den[1] & d$density <= input$f_den[2], ]
    }
    d
  })

  scored <- reactive({
    compute_index(filtered(), weights(), DIRS) %>%
      filter(!is.na(score)) %>%
      arrange(desc(score)) %>%
      mutate(rank = row_number())
  })

  output$n_munis  <- renderText(format(nrow(filtered()), big.mark = " "))
  output$n_scored <- renderText(format(nrow(scored()), big.mark = " "))

  # Display columns: rank, obec, score + raw component values present.
  display_cols <- function(df) {
    keep <- c("rank", "obec", "score", intersect(COLS, names(df)))
    out <- df[, keep, drop = FALSE]
    out$score <- round(out$score, 1)
    for (c in intersect(COLS, names(out))) out[[c]] <- round(out[[c]], 1)
    names(out) <- c("Pořadí", "Obec", "Skóre", unname(LABS[intersect(COLS, names(df))]))
    out
  }

  rank_table <- function(df) {
    datatable(display_cols(df), rownames = FALSE,
              options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
              selection = "none")
  }

  score_bars <- function(df, fill) {
    if (nrow(df) == 0) return(ggplot() + theme_void())
    df <- df %>% mutate(obec = factor(obec, levels = rev(obec)))
    ggplot(df, aes(x = score, y = obec)) +
      geom_col(fill = fill, width = 0.7) +
      geom_text(aes(label = round(score, 1)), hjust = -0.15, size = 3.2, colour = PAQ$caption) +
      scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
      labs(title = NULL) +
      theme_paq_app(11)
  }

  top10 <- reactive(head(scored(), 10))
  bot10 <- reactive({
    s <- scored()
    tail(s, min(10, nrow(s))) %>% arrange(desc(score))
  })

  output$top_tbl  <- renderDT(rank_table(top10()))
  output$bot_tbl  <- renderDT(rank_table(bot10()))
  output$full_tbl <- renderDT(
    datatable(display_cols(scored()), rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE), selection = "none")
  )
  output$top_plot <- renderPlot(score_bars(top10(), PAQ$green))
  output$bot_plot <- renderPlot(score_bars(bot10(), PAQ$merlot))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
