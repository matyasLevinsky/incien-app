# app/app.R
# Incien waste-index explorer. Reads data/processed/municipalities.rds and lets
# the user weight the index components (grouped by waste theme), filter by
# population/density, and see each municipality's score plus top 10 / bottom 10.
#
# Run: Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'

options(encoding = "UTF-8")

suppressMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(here)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Resolve project root on either machine layout, then load shared code.
.find <- function(...) { p <- here::here(...); if (file.exists(p)) return(p); here::here("incien-app", ...) }
source(.find("R", "data.R"))
source(.find("R", "index.R"))
source(.find("R", "theme.R"))

# ── Load data once at startup ────────────────────────────────────────────────
bundle <- tryCatch(load_processed(), error = function(e) e)
data_ok <- !inherits(bundle, "error")

if (data_ok) {
  DATA  <- bundle$data
  DIRS  <- index_directions()
  LABS  <- index_labels()
  COLS  <- index_cols()
  GROUPS <- index_groups()
  DEFAULTS <- index_defaults()
  UNITS <- index_units()
  NOTE  <- bundle$meta$note %||% ""

  rng <- function(col) {
    if (!col %in% names(DATA)) return(c(0, 1))
    v <- DATA[[col]][is.finite(DATA[[col]])]
    if (length(v) == 0) c(0, 1) else range(v)
  }
  pop_rng <- rng("population")
  den_rng <- rng("density")

  # One weight slider per component, labelled with direction.
  slider_for <- function(col) {
    arrow <- if (identical(DIRS[[col]], "lower")) "↓" else "↑"
    sliderInput(paste0("w_", col), paste0(arrow, " ", LABS[[col]]),
                min = 0, max = 100, value = DEFAULTS[[col]], step = 5)
  }
  # Accordion panels grouped by theme, preserving group order of appearance.
  group_order <- unique(unname(GROUPS))
  weight_accordion <- accordion(
    open = group_order,
    lapply(group_order, function(g) {
      cols_g <- COLS[GROUPS == g]
      accordion_panel(g, lapply(cols_g, slider_for))
    })
  )
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
      width = 360,
      h6("Váhy komponent indexu"),
      tags$small(class = "text-muted", "↑ vyšší = lepší · ↓ nižší = lepší. Váha 0 = vyřadit."),
      weight_accordion,
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
      value_box("Hodnocených (se skóre)", textOutput("n_scored"), theme = "primary"),
      value_box("Aktivních komponent", textOutput("n_comp"), theme = "light")
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
    ),
    tags$small(class = "text-muted", NOTE)
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  if (!data_ok) return(invisible(NULL))

  weights <- reactive({
    setNames(vapply(COLS, function(col) input[[paste0("w_", col)]] %||% 0, numeric(1)), COLS)
  })
  active_cols <- reactive({
    w <- weights(); names(w)[w > 0]
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
  output$n_comp   <- renderText(as.character(length(active_cols())))

  # Display: identity + filters + only the components with positive weight.
  display_cols <- function(df) {
    act <- active_cols()
    base <- c("rank", "obec", "score", "population", "density")
    keep <- c(base, act)
    out <- df[, intersect(keep, names(df)), drop = FALSE]
    num <- setdiff(names(out), c("rank", "obec"))
    out[num] <- lapply(out[num], function(x) round(x, 1))
    nm <- names(out)
    nm[nm == "rank"] <- "Pořadí"; nm[nm == "obec"] <- "Obec"
    nm[nm == "score"] <- "Skóre"; nm[nm == "population"] <- "Obyvatel"
    nm[nm == "density"] <- "Hustota"
    for (c in act) nm[nm == c] <- paste0(LABS[[c]], " (", UNITS[[c]], ")")
    names(out) <- nm
    out
  }

  rank_table <- function(df, pageLength = 10) {
    datatable(display_cols(df), rownames = FALSE,
              options = list(pageLength = pageLength, dom = "tip", scrollX = TRUE),
              selection = "none")
  }

  score_bars <- function(df, fill) {
    if (nrow(df) == 0) return(ggplot() + theme_void())
    df <- df %>% mutate(obec = factor(obec, levels = rev(obec)))
    ggplot(df, aes(x = score, y = obec)) +
      geom_col(fill = fill, width = 0.7) +
      geom_text(aes(label = round(score, 1)), hjust = -0.15, size = 3.2, colour = PAQ$caption) +
      scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
      theme_paq_app(11)
  }

  top10 <- reactive(head(scored(), 10))
  bot10 <- reactive({
    s <- scored(); tail(s, min(10, nrow(s))) %>% arrange(desc(score))
  })

  output$top_tbl  <- renderDT(rank_table(top10()))
  output$bot_tbl  <- renderDT(rank_table(bot10()))
  output$full_tbl <- renderDT(rank_table(scored(), pageLength = 25))
  output$top_plot <- renderPlot(score_bars(top10(), PAQ$green))
  output$bot_plot <- renderPlot(score_bars(bot10(), PAQ$merlot))
}

shinyApp(ui, server)
