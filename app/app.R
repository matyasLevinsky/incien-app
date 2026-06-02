# app/app.R
# Incien waste-index explorer. Four tabs:
#   1. Nastavení & info  – project explanation + index weight sliders
#   2. Přehled           – cost-vs-quality quadrant scatter + full ranking
#   3. Dobré obce        – low-cost / high-quality quadrant
#   4. Špatné obce       – high-cost / low-quality quadrant
# Cost is NOT part of the index; it is the accompanying dimension on the X axis.
#
# Run: Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'

options(encoding = "UTF-8")

suppressMessages({
  library(shiny); library(bslib); library(DT)
  library(ggplot2); library(dplyr); library(here)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

.find <- function(...) { p <- here::here(...); if (file.exists(p)) return(p); here::here("incien-app", ...) }
source(.find("R", "data.R"))
source(.find("R", "index.R"))
source(.find("R", "theme.R"))

bundle <- tryCatch(load_processed(), error = function(e) e)
data_ok <- !inherits(bundle, "error")

# ── Setup (data present) ─────────────────────────────────────────────────────
if (data_ok) {
  DATA     <- bundle$data
  DIRS     <- index_directions()
  LABS     <- index_labels()
  COLS     <- index_cols()
  GROUPS   <- index_groups()
  DEFAULTS <- index_defaults()
  UNITS    <- index_units()
  COST     <- COST_COMPONENT$col
  COST_LAB <- COST_COMPONENT$label
  COST_UNIT<- COST_COMPONENT$unit
  NOTE     <- bundle$meta$note %||% ""

  rng <- function(col) {
    v <- DATA[[col]][is.finite(DATA[[col]])]
    if (length(v) == 0) c(0, 1) else range(v)
  }
  pop_rng <- rng("population"); den_rng <- rng("density")

  slider_for <- function(col) {
    arrow <- if (identical(DIRS[[col]], "lower")) "↓" else "↑"
    sliderInput(paste0("w_", col), paste0(arrow, " ", LABS[[col]]),
                min = 0, max = 100, value = DEFAULTS[[col]], step = 5)
  }
  group_order <- unique(unname(GROUPS))
  weight_accordion <- accordion(
    open = group_order,
    lapply(group_order, function(g) {
      accordion_panel(g, lapply(COLS[GROUPS == g], slider_for))
    })
  )

  GOOD_LAB <- "Dobrá (nízké náklady, vysoká kvalita)"
  BAD_LAB  <- "Špatná (vysoké náklady, nízká kvalita)"
  OTHER_LAB<- "Ostatní"
  QUAD_COLS <- setNames(c(PAQ$green, PAQ$merlot, "#BABACF"),
                        c(GOOD_LAB, BAD_LAB, OTHER_LAB))

  intro_md <- sprintf("
### Index kvality odpadového hospodářství obcí

Tento nástroj sestavuje **index kvality** odpadové politiky pro všech %s obcí ČR a
staví jej proti **nákladům** na odpadové hospodářství. Cílem je odlišit obce, které
hospodaří kvalitně i levně, od těch, kde vysoké náklady nevedou k dobrým výsledkům.

**Jak index funguje**

- Index skládá několik dílčích ukazatelů (plnění cíle třídění, podíl separace,
  produkce a separace odpadu podle druhů – vše přepočteno na obyvatele).
- Každý ukazatel se převede na **percentilové pořadí (0–100)** v rámci aktuálního
  výběru obcí a otočí se tak, aby vyšší hodnota vždy znamenala lepší politiku.
- Výsledné skóre je **vážený průměr** podle vah níže. Váha 0 ukazatel vyřadí.
  Nastavením vah „testujete různé podoby indexu“.

**Náklady jako druhý rozměr**

Náklady **nejsou** součástí indexu. V záložce *Přehled* je graf: na ose X náklady
na obyvatele, na ose Y skóre kvality. Hledáme kvadrant **vlevo nahoře** (nízké
náklady, vysoká kvalita) – *dobré obce*; kvadrant **vpravo dole** (vysoké náklady,
nízká kvalita) je varovný příklad – *špatné obce*. Hranice kvadrantů jsou mediány
výběru.

*Poznámka k datům: %s*
", format(nrow(DATA), big.mark = " "), NOTE)
}

# ── UI ────────────────────────────────────────────────────────────────────────
if (!data_ok) {
  ui <- page_fillable(theme = paq_bs_theme(),
    card(card_header("Data nejsou připravena"),
      card_body(p("Spusťte pipeline:"),
        tags$pre("Rscript pipeline/01_extract.R\nRscript pipeline/02_process.R"),
        p(tags$small(conditionMessage(bundle))))))
} else {
  ui <- page_navbar(
    title = "Index odpadového hospodářství obcí",
    theme = paq_bs_theme(),
    sidebar = sidebar(
      width = 300,
      h6("Filtry výběru obcí"),
      sliderInput("f_pop", "Počet obyvatel",
                  min = floor(pop_rng[1]), max = ceiling(pop_rng[2]),
                  value = c(floor(pop_rng[1]), ceiling(pop_rng[2]))),
      sliderInput("f_den", "Hustota (obyv./km²)",
                  min = floor(den_rng[1]), max = ceiling(den_rng[2]),
                  value = c(floor(den_rng[1]), ceiling(den_rng[2]))),
      hr(),
      tags$small(class = "text-muted",
                 textOutput("side_summary", inline = TRUE))
    ),

    nav_panel(
      "Nastavení & info",
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("O projektu a indexu"), card_body(markdown(intro_md))),
        card(card_header("Váhy komponent indexu"),
             tags$small(class = "text-muted",
                        "↑ vyšší = lepší · ↓ nižší = lepší · váha 0 = vyřadit"),
             weight_accordion)
      )
    ),

    nav_panel(
      "Přehled",
      layout_columns(
        fill = FALSE,
        value_box("Obcí ve výběru", textOutput("n_munis"), theme = "secondary"),
        value_box("Hodnocených (se skóre)", textOutput("n_scored"), theme = "primary"),
        value_box("Aktivních komponent", textOutput("n_comp"), theme = "light")
      ),
      card(card_header("Náklady vs. kvalita"),
           card_body(plotOutput("scatter", height = "440px"))),
      card(card_header("Celkové pořadí"), DTOutput("full_tbl"))
    ),

    nav_panel(
      "Dobré obce",
      card(card_header("Nízké náklady, vysoká kvalita"),
           card_body(
             tags$p(class = "text-muted",
                    textOutput("good_caption", inline = TRUE)),
             plotOutput("good_plot", height = "300px"))),
      card(card_header("Seznam dobrých obcí"), DTOutput("good_tbl"))
    ),

    nav_panel(
      "Špatné obce",
      card(card_header("Vysoké náklady, nízká kvalita"),
           card_body(
             tags$p(class = "text-muted",
                    textOutput("bad_caption", inline = TRUE)),
             plotOutput("bad_plot", height = "300px"))),
      card(card_header("Seznam špatných obcí"), DTOutput("bad_tbl"))
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  if (!data_ok) return(invisible(NULL))

  weights     <- reactive(setNames(vapply(COLS, function(c) input[[paste0("w_", c)]] %||% 0, numeric(1)), COLS))
  active_cols <- reactive({ w <- weights(); names(w)[w > 0] })

  filtered <- reactive({
    d <- DATA
    d <- d[!is.na(d$population) & d$population >= input$f_pop[1] & d$population <= input$f_pop[2], ]
    d <- d[!is.na(d$density) & d$density >= input$f_den[1] & d$density <= input$f_den[2], ]
    d
  })

  scored <- reactive({
    compute_index(filtered(), weights(), DIRS) %>%
      filter(!is.na(score)) %>% arrange(desc(score)) %>% mutate(rank = row_number())
  })

  # Municipalities with both a score and a cost → placed into quadrants.
  quad <- reactive({
    s <- scored()
    s <- s[!is.na(s[[COST]]) & !is.na(s$score), ]
    if (nrow(s) == 0) { s$quadrant <- character(0); return(s) }
    q_thr <- median(s$score, na.rm = TRUE)
    c_thr <- median(s[[COST]], na.rm = TRUE)
    s$quadrant <- ifelse(s$score >= q_thr & s[[COST]] <= c_thr, GOOD_LAB,
                  ifelse(s$score <  q_thr & s[[COST]] >  c_thr, BAD_LAB, OTHER_LAB))
    attr(s, "q_thr") <- q_thr; attr(s, "c_thr") <- c_thr
    s
  })

  # ── Summaries ──
  output$side_summary <- renderText(sprintf("Obcí: %s · hodnoceno: %s",
    format(nrow(filtered()), big.mark = " "), format(nrow(scored()), big.mark = " ")))
  output$n_munis  <- renderText(format(nrow(filtered()), big.mark = " "))
  output$n_scored <- renderText(format(nrow(scored()), big.mark = " "))
  output$n_comp   <- renderText(as.character(length(active_cols())))

  # ── Scatter ──
  scatter_plot <- function(d, emphasise = NULL) {
    if (nrow(d) == 0) return(ggplot() + theme_void())
    q_thr <- attr(d, "q_thr"); c_thr <- attr(d, "c_thr")
    xmax <- as.numeric(quantile(d[[COST]], 0.98, na.rm = TRUE))
    alpha <- if (is.null(emphasise)) 0.5 else ifelse(d$quadrant == emphasise, 0.85, 0.12)
    ggplot(d, aes(.data[[COST]], score, colour = quadrant)) +
      geom_vline(xintercept = c_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_hline(yintercept = q_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_point(aes(alpha = I(alpha)), size = 1) +
      scale_colour_manual(values = QUAD_COLS, name = NULL) +
      coord_cartesian(xlim = c(0, xmax), ylim = c(0, 100)) +
      labs(x = paste0(COST_LAB, " (", COST_UNIT, ")"), y = "Index kvality (0–100)") +
      theme_paq_app(12) + theme(legend.position = "top")
  }
  output$scatter <- renderPlot(scatter_plot(quad()))
  output$good_plot <- renderPlot(scatter_plot(quad(), emphasise = GOOD_LAB))
  output$bad_plot  <- renderPlot(scatter_plot(quad(), emphasise = BAD_LAB))

  output$good_caption <- renderText({
    q <- quad(); n <- sum(q$quadrant == GOOD_LAB)
    sprintf("Kvadrant vlevo nahoře: %d obcí pod mediánem nákladů a nad mediánem kvality. Řazeno podle skóre kvality.", n)
  })
  output$bad_caption <- renderText({
    q <- quad(); n <- sum(q$quadrant == BAD_LAB)
    sprintf("Kvadrant vpravo dole: %d obcí nad mediánem nákladů a pod mediánem kvality. Řazeno od nejhoršího skóre.", n)
  })

  # ── Tables ──
  base_tbl <- function(df, cols, names_map, pageLength = 10) {
    out <- df[, intersect(cols, names(df)), drop = FALSE]
    num <- setdiff(names(out), c("rank", "obec"))
    out[num] <- lapply(out[num], function(x) round(x, 1))
    nm <- names(out); for (k in names(names_map)) nm[nm == k] <- names_map[[k]]
    names(out) <- nm
    datatable(out, rownames = FALSE, selection = "none",
              options = list(pageLength = pageLength, dom = "tip", scrollX = TRUE))
  }
  common_map <- c(rank = "Pořadí", obec = "Obec", score = "Skóre kvality",
                  population = "Obyvatel", density = "Hustota")
  cost_map <- setNames(paste0(COST_LAB, " (", COST_UNIT, ")"), COST)

  output$full_tbl <- renderDT({
    act <- active_cols()
    comp_map <- setNames(paste0(LABS[act], " (", UNITS[act], ")"), act)
    base_tbl(scored(),
             cols = c("rank", "obec", "score", COST, "population", "density", act),
             names_map = c(common_map, cost_map, comp_map), pageLength = 25)
  })

  quad_cols <- function() c("obec", "score", COST, "population", "density")
  output$good_tbl <- renderDT({
    g <- quad(); g <- g[g$quadrant == GOOD_LAB, ] %>% arrange(desc(score))
    base_tbl(g, quad_cols(), c(common_map, cost_map))
  })
  output$bad_tbl <- renderDT({
    b <- quad(); b <- b[b$quadrant == BAD_LAB, ] %>% arrange(score)
    base_tbl(b, quad_cols(), c(common_map, cost_map))
  })
}

shinyApp(ui, server)
