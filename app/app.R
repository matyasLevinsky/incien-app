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
  library(ggplot2); library(dplyr); library(shinyjs)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# app/ is self-contained: helpers and data live under the app dir, referenced
# relatively so it works both with runApp("app") and after shinylive::export().
source("R/data.R")
source("R/index.R")
source("R/theme.R")

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
  dir_hint <- tags$small(class = "text-muted",
                         "↑ vyšší = lepší · ↓ nižší = lepší · váha 0 = vyřadit")
  # One weight card per group; used in the 5-box Nastavení grid.
  weights_card_for <- function(title, g) {
    card(card_header(title),
         card_body(dir_hint, lapply(COLS[GROUPS == g], slider_for)))
  }
  weights_plnenicile <- weights_card_for("Váhy: Plnění cíle třídění", "Plnění cíle")
  weights_produkce   <- weights_card_for("Váhy: Produkce", "Produkce")
  weights_separace   <- weights_card_for("Váhy: Separace", "Separace")

  # "Optimum: Separace" — per category a toggle (linear ↔ beta) + the optimum
  # value slider. Beta scores closeness to the optimum (sweet spot) instead of
  # rewarding ever-higher separated amounts. Mirrors the Váhy: Separace card.
  opt_cell <- function(col) {
    d <- sep_optimum_default(col)
    omax <- ceiling(as.numeric(quantile(DATA[[col]], 0.99, na.rm = TRUE)))
    if (!is.finite(omax) || omax <= 0) omax <- 1
    step <- signif(omax / 100, 1)
    div(id = paste0("optwrap_", col), class = "opt-cell",
      tags$strong(LABS[[col]]),
      input_switch(paste0("opt_on_", col), "Optimum (beta)", value = d$on),
      sliderInput(paste0("opt_val_", col), sprintf("Optimum (%s)", UNITS[[col]]),
                  min = 0, max = omax, value = min(d$opt, omax), step = step)
    )
  }
  # Optimum column — mirrors the Separace weights, one cell per category. Cells
  # grey out (server-side) when the matching weight is 0, since they then do nothing.
  optimum_separace <- card(
    card_header("Optimum: Separace"),
    card_body(
      tags$small(class = "text-muted",
                 "◎ beta = příliš málo i příliš mnoho je horší. Šedé = váha 0 (neúčinkuje)."),
      lapply(SEP_CATEGORIES, opt_cell)
    )
  )

  filter_card <- card(
    card_header("Filtry výběru obcí"),
    card_body(
      sliderInput("f_pop", "Počet obyvatel",
                  min = floor(pop_rng[1]), max = ceiling(pop_rng[2]),
                  value = c(floor(pop_rng[1]), ceiling(pop_rng[2]))),
      sliderInput("f_den", "Hustota (obyv./km²)",
                  min = floor(den_rng[1]), max = ceiling(den_rng[2]),
                  value = c(floor(den_rng[1]), ceiling(den_rng[2])))
    )
  )

  # Outlier treatment (winsorization). Cost is clamped only from below (low
  # cost ≈ bad data); production/separation are clamped at both extremes.
  clamp_card <- card(
    card_header("Ošetření odlehlých hodnot (clamping)"),
    card_body(
      sliderInput("clamp_cost", "Náklady/obyv. – ořez obou konců (percentil)",
                  min = 0, max = 0.20, value = 0.01, step = 0.01),
      tags$small(class = "text-muted", textOutput("clamp_cost_czk", inline = TRUE)),
      sliderInput("clamp_prod", "Produkce – ořez obou konců (percentil)",
                  min = 0, max = 0.10, value = 0.01, step = 0.01),
      sliderInput("clamp_sep", "Separace – ořez obou konců (percentil)",
                  min = 0, max = 0.10, value = 0.01, step = 0.01)
    )
  )

  preset_choices <- setNames(
    vapply(PRESET_MUNICIPALITIES, `[[`, "", "kod_obec"),
    vapply(PRESET_MUNICIPALITIES, function(p) sprintf("%s (%s)", p$name, p$tier), "")
  )
  preset_default <- "519651"  # Troubky (~2 000 obyv.)

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
- Výsledné skóre je **vážený průměr** podle vah, které nastavíte v záložce
  *Nastavení*. Váha 0 ukazatel vyřadí. Nastavením vah „testujete různé podoby indexu“.

**Náklady jako druhý rozměr**

Náklady **nejsou** součástí indexu. V záložce *Přehled* je graf: na ose X náklady
na obyvatele, na ose Y skóre kvality. Hledáme kvadrant **vlevo nahoře** (nízké
náklady, vysoká kvalita) – *dobré obce*; kvadrant **vpravo dole** (vysoké náklady,
nízká kvalita) je varovný příklad – *špatné obce*. Hranice kvadrantů jsou mediány
výběru.

*Poznámka k datům: %s*
", format(nrow(DATA), big.mark = " "), NOTE)

  # ── Data-exploration variable list (Explorace dat tab) ──
  # Every analytically meaningful raw column, with a label + unit lookup so the
  # histogram can label its axis. Index components + cost + the two filter cols.
  EXP_COLS <- c(COLS, COST, "population", "density")
  EXP_LABS <- c(LABS, setNames(COST_LAB, COST),
                population = "Počet obyvatel", density = "Hustota")
  EXP_UNITS <- c(UNITS, setNames(COST_UNIT, COST),
                 population = "obyv.", density = "obyv./km²")
  explore_choices <- setNames(
    EXP_COLS, sprintf("%s (%s)", EXP_LABS[EXP_COLS], EXP_UNITS[EXP_COLS]))
}

# ── UI ────────────────────────────────────────────────────────────────────────
if (!data_ok) {
  ui <- page_fillable(theme = paq_bs_theme(),
    card(card_header("Data nejsou připravena"),
      card_body(p("Spusťte pipeline:"),
        tags$pre("Rscript pipeline/01_extract.R\nRscript pipeline/02_process.R"),
        p(tags$small(conditionMessage(bundle))))))
} else {
  # Sections live in the left sidebar (radio nav), not a top navbar. The sidebar
  # also carries a sticky, always-visible histogram of the current index scores.
  sections <- c("Info", "Nastavení", "Přehled", "Dobré obce", "Špatné obce", "Explorace dat")

  ui <- page_sidebar(
    title = "Index odpadového hospodářství obcí",
    theme = paq_bs_theme(),
    fillable = FALSE,  # natural content height; the page scrolls
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    sidebar = sidebar(
      width = 330,
      title = "Sekce",
      radioButtons("nav_sel", NULL, choices = sections, selected = "Info"),
      hr(),
      div(class = "side-dist",
          tags$strong("Rozdělení skóre indexu",
                      style = sprintf("color:%s", PAQ$night_blue)),
          plotOutput("side_hist", height = "170px"),
          tags$small(class = "text-muted", textOutput("side_hist_cap", inline = TRUE)))
    ),

    navset_hidden(
      id = "main_nav",

      nav_panel_hidden(
        "Info",
        card(
          card_header("O projektu a indexu"),
          card_body(
            markdown(intro_md),
            hr(),
            actionLink("go_nastaveni", "Pokračovat na Nastavení →",
                       class = "btn btn-primary")
          )
        )
      ),

      nav_panel_hidden(
        "Nastavení",
        layout_columns(
          fill = FALSE,
          value_box("Obcí ve výběru", textOutput("n_munis"), theme = "secondary"),
          value_box("Hodnocených (se skóre)", textOutput("n_scored"), theme = "primary"),
          value_box("Aktivních komponent", textOutput("n_comp"), theme = "light")
        ),
        card(card_header("Výsledný vzorec"), card_body(uiOutput("formula"))),
        card(card_header("Příklad výpočtu"),
             card_body(
               radioButtons("example_obec", "Modelová obec:",
                            choices = preset_choices, selected = preset_default, inline = TRUE),
               uiOutput("example"))),
        # 4 columns of sliders: Filtr+Plnění cíle | Clamping+Produkce |
        # Separace váhy | Separace optimum (the optimum sits next to its weights).
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(filter_card, weights_plnenicile),
          div(clamp_card, weights_produkce),
          weights_separace,
          optimum_separace
        )
      ),

      nav_panel_hidden(
        "Přehled",
        card(card_header("Náklady vs. kvalita"),
             card_body(
               plotOutput("scatter", height = "440px",
                          hover = hoverOpts("scatter_hover", delay = 80, delayType = "debounce"),
                          click = "scatter_click"),
               tags$small(class = "text-muted",
                          "Najeďte myší na bod (nebo klikněte) pro detail obce."),
               uiOutput("scatter_info"))),
        card(card_header("Celkové pořadí"), DTOutput("full_tbl"))
      ),

      nav_panel_hidden(
        "Dobré obce",
        card(card_header("Nízké náklady, vysoká kvalita"),
             card_body(
               tags$p(class = "text-muted",
                      textOutput("good_caption", inline = TRUE)),
               plotOutput("good_plot", height = "300px"))),
        card(card_header("Seznam dobrých obcí"), DTOutput("good_tbl"))
      ),

      nav_panel_hidden(
        "Špatné obce",
        card(card_header("Vysoké náklady, nízká kvalita"),
             card_body(
               tags$p(class = "text-muted",
                      textOutput("bad_caption", inline = TRUE)),
               plotOutput("bad_plot", height = "300px"))),
        card(card_header("Seznam špatných obcí"), DTOutput("bad_tbl"))
      ),

      nav_panel_hidden(
        "Explorace dat",
        card(
          card_header("Rozdělení hodnot ukazatele (všechny obce ČR)"),
          card_body(
            selectInput("explore_var", "Ukazatel:", choices = explore_choices,
                        selected = "plneni_cile", width = "420px"),
            radioButtons("explore_scale", "Škála osy X:",
                         choices = c("Lineární" = "lin", "Logaritmická" = "log"),
                         selected = "log", inline = TRUE),
            plotOutput("hist_plot", height = "440px"),
            tags$small(class = "text-muted", textOutput("hist_caption", inline = TRUE))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  if (!data_ok) return(invisible(NULL))

  # Sidebar radio drives which panel is shown in the hidden navset.
  observeEvent(input$nav_sel, nav_select("main_nav", input$nav_sel))
  # "Pokračovat na Nastavení" link on the Info page selects the Nastavení radio
  # (which in turn switches the panel via the observer above).
  observeEvent(input$go_nastaveni,
               updateRadioButtons(session, "nav_sel", selected = "Nastavení"))

  weights     <- reactive(setNames(vapply(COLS, function(c) input[[paste0("w_", c)]] %||% 0, numeric(1)), COLS))
  active_cols <- reactive({ w <- weights(); names(w)[w > 0] })

  filtered <- reactive({
    d <- DATA
    d <- d[!is.na(d$population) & d$population >= input$f_pop[1] & d$population <= input$f_pop[2], ]
    d <- d[!is.na(d$density) & d$density >= input$f_den[1] & d$density <= input$f_den[2], ]
    d
  })

  # Apply user-set clamping to the filtered cohort before scoring/plotting.
  prepared <- reactive({
    d <- filtered()
    if ((input$clamp_cost %||% 0) > 0) d[[COST]] <- clamp_winsor(d[[COST]], input$clamp_cost)
    pp <- input$clamp_prod %||% 0
    if (pp > 0) for (col in COLS[GROUPS == "Produkce"]) d[[col]] <- clamp_winsor(d[[col]], pp)
    ps <- input$clamp_sep %||% 0
    if (ps > 0) for (col in COLS[GROUPS == "Separace"]) d[[col]] <- clamp_winsor(d[[col]], ps)
    d
  })

  # Separation categories switched to beta scoring → named numeric (col → optimum).
  beta_optima <- reactive({
    on <- SEP_CATEGORIES[vapply(SEP_CATEGORIES,
            function(c) isTRUE(input[[paste0("opt_on_", c)]]), logical(1))]
    setNames(vapply(on, function(c) input[[paste0("opt_val_", c)]] %||% 0, numeric(1)), on)
  })

  # Direction marker for a component: ◎ if beta-scored, else ↑/↓ by direction.
  marker <- function(col, beta) {
    if (col %in% names(beta)) "◎" else if (identical(DIRS[[col]], "lower")) "↓" else "↑"
  }

  # Human-readable scoring type for the example table's "Typ" column.
  type_label <- function(col, beta) {
    if (col %in% names(beta)) {
      sprintf("◎ optimum, cíl je: %s %s",
              format(round(beta[[col]], 1), big.mark = " "), UNITS[[col]])
    } else if (identical(DIRS[[col]], "lower")) {
      "↓ lineární, nižší je lepší"
    } else {
      "↑ lineární, vyšší je lepší"
    }
  }

  # Grey out + disable a category's optimum controls when its weight is 0
  # (the optimum then has no effect on the index).
  lapply(SEP_CATEGORIES, function(cc) {
    observe({
      active <- (input[[paste0("w_", cc)]] %||% 0) > 0
      shinyjs::toggleState(paste0("opt_on_", cc), condition = active)
      shinyjs::toggleState(paste0("opt_val_", cc), condition = active)
      shinyjs::toggleClass(id = paste0("optwrap_", cc), class = "opt-disabled", condition = !active)
    })
  })

  scored <- reactive({
    compute_index(prepared(), weights(), DIRS, beta = beta_optima()) %>%
      filter(!is.na(score)) %>% arrange(desc(score)) %>% mutate(rank = row_number())
  })

  # Sidebar: live distribution of the current index scores (reflects weights,
  # filters and clamping). Always visible across all sections.
  output$side_hist <- renderPlot({
    s <- scored()
    if (nrow(s) == 0)
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 4, colour = PAQ$caption,
                        label = "Nastavte alespoň\njednu váhu > 0"))
    ggplot(s, aes(score)) +
      geom_histogram(bins = 25, fill = PAQ$green, colour = "white", linewidth = 0.2) +
      geom_vline(xintercept = median(s$score), linetype = "dashed", colour = PAQ$merlot) +
      coord_cartesian(xlim = c(0, 100)) +
      labs(x = "Skóre indexu (0–100)", y = NULL) +
      theme_paq_app(11)
  })
  output$side_hist_cap <- renderText({
    s <- scored()
    if (nrow(s) == 0) return("Bez aktivních komponent.")
    sprintf("%s obcí · medián %.1f (přerušovaná čára)",
            format(nrow(s), big.mark = " "), median(s$score))
  })

  output$clamp_cost_czk <- renderText({
    p <- input$clamp_cost %||% 0
    if (p <= 0) return("Bez ořezu nákladů")
    v <- filtered()[[COST]]
    lo <- clamp_cutoff(v, p); hi <- clamp_cutoff(v, 1 - p)
    if (is.na(lo)) return("Bez dat")
    fmt <- function(x) format(round(x), big.mark = " ")
    sprintf("Hranice: %s – %s Kč/obyv. — %d obcí ořezáno",
            fmt(lo), fmt(hi), sum(!is.na(v) & (v < lo | v > hi)))
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
  output$n_munis  <- renderText(format(nrow(filtered()), big.mark = " "))
  output$n_scored <- renderText(format(nrow(scored()), big.mark = " "))
  output$n_comp   <- renderText(as.character(length(active_cols())))

  # Live index formula reflecting the current weights.
  output$formula <- renderUI({
    w <- weights(); act <- active_cols()
    if (length(act) == 0)
      return(tags$em("Žádná aktivní komponenta — nastavte alespoň jednu váhu > 0."))
    terms <- paste0(w[act], " · ", LABS[act])
    tagList(
      tags$p(tags$strong("Obecně: "), "Skóre = Σ(váha", tags$sub("i"),
             " × orientovaný percentil", tags$sub("i"), ") / Σ(váha", tags$sub("i"), ")"),
      tags$p(tags$strong("Aktuálně: "),
             sprintf("Skóre = ( %s ) / %s", paste(terms, collapse = " + "), sum(w[act]))),
      tags$small(class = "text-muted",
        "Orientovaný percentil = percentilové pořadí 0–100 v rámci výběru; ",
        "u ukazatelů „nižší = lepší“ obráceno (100 − percentil).")
    )
  })

  # Clamping applied to ALL municipalities (no pop/density filter), so any fixed
  # model obec is always present and its percentiles are stable.
  prepared_full <- reactive({
    d <- DATA
    if ((input$clamp_cost %||% 0) > 0) d[[COST]] <- clamp_winsor(d[[COST]], input$clamp_cost)
    pp <- input$clamp_prod %||% 0
    if (pp > 0) for (col in COLS[GROUPS == "Produkce"]) d[[col]] <- clamp_winsor(d[[col]], pp)
    ps <- input$clamp_sep %||% 0
    if (ps > 0) for (col in COLS[GROUPS == "Separace"]) d[[col]] <- clamp_winsor(d[[col]], ps)
    d
  })

  # Worked example for a fixed model municipality (chosen by the user).
  output$example <- renderUI({
    act <- active_cols()
    if (length(act) == 0) return(tags$em("Nastavte alespoň jednu váhu > 0."))
    d <- prepared_full()
    beta <- beta_optima()
    # Per-component score (linear percentile or beta-optimum) over all obce.
    op <- lapply(act, function(col) {
      opt <- if (col %in% names(beta)) beta[[col]] else NULL
      score_component(d[[col]], DIRS[[col]], opt = opt)
    })
    names(op) <- act
    sel <- input$example_obec %||% preset_default
    mi <- which(d$kod_obec == sel)[1]
    if (is.na(mi)) return(tags$em("Obec není v datech."))
    m <- d[mi, ]
    w <- weights()
    rows <- lapply(act, function(col) {
      pct <- op[[col]][mi]; wt <- w[[col]]
      contrib <- if (is.na(pct)) NA_real_ else wt * pct
      tags$tr(
        tags$td(LABS[[col]]),
        tags$td(type_label(col, beta)),
        tags$td(if (is.na(m[[col]])) "—" else paste(format(round(m[[col]], 1), big.mark = " "), UNITS[[col]])),
        tags$td(if (is.na(pct)) "—" else round(pct, 1)),
        tags$td(wt),
        tags$td(if (is.na(contrib)) "0" else format(round(contrib, 1), big.mark = " "))
      )
    })
    eff_w <- sum(vapply(act, function(col) if (is.na(op[[col]][mi])) 0 else w[[col]], 0))
    csum  <- sum(vapply(act, function(col) { p <- op[[col]][mi]; if (is.na(p)) 0 else w[[col]] * p }, 0))
    score <- if (eff_w > 0) csum / eff_w else NA_real_
    tagList(
      tags$p(tags$strong(sprintf("%s", m$obec)),
             sprintf(" — %s obyv., %s Kč/obyv.",
                     format(round(m$population), big.mark = " "),
                     format(round(m[[COST]]), big.mark = " "))),
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(lapply(
          c("Ukazatel", "Typ", "Hodnota", "Skóre komp. (0–100)", "Váha", "Příspěvek (váha×skóre)"),
          tags$th))),
        tags$tbody(rows)),
      tags$p(tags$strong(sprintf("Skóre = %s / %s = %s",
             format(round(csum, 1), big.mark = " "), eff_w, round(score, 1)))),
      tags$small(class = "text-muted",
                 "Skóre komponent počítáno vůči všem obcím ČR (nezávisle na filtru).")
    )
  })

  # ── Scatter ──
  # mode = "all"  → all points, both median lines, full (capped) cost range.
  # mode = "good" → only the good quadrant, zoomed to its region.
  # mode = "bad"  → only the bad quadrant, zoomed to its region.
  scatter_plot <- function(d, mode = "all", highlight = NULL) {
    if (nrow(d) == 0) return(ggplot() + theme_void())
    q_thr <- attr(d, "q_thr"); c_thr <- attr(d, "c_thr")
    cost <- d[[COST]]
    xlo <- min(cost, na.rm = TRUE)
    xhi <- as.numeric(quantile(cost, 0.99, na.rm = TRUE))  # caps the empty gap when uncapped
    base <- labs(x = paste0(COST_LAB, " (", COST_UNIT, ")"), y = "Index kvality (0–100)")

    if (mode == "good") {
      d <- d[d$quadrant == GOOD_LAB, ]
      if (nrow(d) == 0) return(ggplot() + theme_void())
      return(ggplot(d, aes(.data[[COST]], score)) +
        geom_point(colour = PAQ$green, alpha = 0.55, size = 1.3) +
        coord_cartesian(xlim = c(xlo, c_thr), ylim = c(q_thr, 100)) +
        base + theme_paq_app(12))
    }
    if (mode == "bad") {
      d <- d[d$quadrant == BAD_LAB, ]
      if (nrow(d) == 0) return(ggplot() + theme_void())
      return(ggplot(d, aes(.data[[COST]], score)) +
        geom_point(colour = PAQ$merlot, alpha = 0.55, size = 1.3) +
        coord_cartesian(xlim = c(c_thr, xhi), ylim = c(0, q_thr)) +
        base + theme_paq_app(12))
    }
    p <- ggplot(d, aes(.data[[COST]], score, colour = quadrant)) +
      geom_vline(xintercept = c_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_hline(yintercept = q_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_point(alpha = 0.5, size = 1) +
      scale_colour_manual(values = QUAD_COLS, name = NULL)
    # Highlight the clicked municipality with a labelled yellow ring.
    if (!is.null(highlight)) {
      hl <- d[d$kod_obec == highlight, ]
      if (nrow(hl)) p <- p +
        geom_point(data = hl, inherit.aes = FALSE, aes(.data[[COST]], score),
                   shape = 21, size = 4, stroke = 1.4, colour = "black", fill = PAQ$yellow) +
        geom_text(data = hl, inherit.aes = FALSE, aes(.data[[COST]], score, label = obec),
                  vjust = -1.1, size = 3.6, fontface = "bold", colour = "black")
    }
    p + coord_cartesian(xlim = c(xlo, xhi), ylim = c(0, 100)) +
      base + theme_paq_app(12) + theme(legend.position = "top")
  }
  output$scatter   <- renderPlot(scatter_plot(quad(), "all", highlight = selected_obec()))
  output$good_plot <- renderPlot(scatter_plot(quad(), "good"))
  output$bad_plot  <- renderPlot(scatter_plot(quad(), "bad"))

  # ── Explorace dat: histogram of one raw variable over ALL municipalities ──
  # Full dataset (no pop/density filter, no clamping) → true distribution.
  # scale = "lin" (all finite values) or "log" (positive only, log x-axis — the
  # log scale compresses the long right tail; zeros can't be shown so they are
  # dropped and reported in the caption).
  log_breaks <- function(v) {
    lo <- floor(log10(min(v))); hi <- ceiling(log10(max(v)))
    10^(lo:hi)
  }
  # NA subtitle: how many municipalities lack any value for this metric (always
  # over the full dataset, independent of the linear/log scale choice).
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
  output$hist_plot <- renderPlot(
    hist_plot(input$explore_var %||% "plneni_cile", input$explore_scale %||% "log"))
  output$hist_caption <- renderText({
    col <- input$explore_var %||% "plneni_cile"
    scale <- input$explore_scale %||% "log"
    all_v <- DATA[[col]]; all_v <- all_v[is.finite(all_v)]
    v <- if (scale == "log") all_v[all_v > 0] else all_v
    if (length(v) == 0) return("Bez dat pro tento ukazatel.")
    f <- function(x) format(round(x, 1), big.mark = " ")
    n_zero <- sum(all_v <= 0)
    zero_txt <- if (scale == "log" && n_zero > 0)
      sprintf(" · %s obcí s nulovou hodnotou vynecháno (log škála)", format(n_zero, big.mark = " ")) else ""
    sprintf("%s obcí · min %s · medián %s · max %s %s%s (přerušovaná čára = medián)",
            format(length(v), big.mark = " "),
            f(min(v)), f(median(v)), f(max(v)), EXP_UNITS[[col]], zero_txt)
  })

  # Municipality nearest a plot event (data coords + axis domain only — no extra
  # packages, no pixel coordmap), so it survives WebAssembly compilation.
  nearest_row <- function(ev) {
    if (is.null(ev) || is.null(ev$x) || is.null(ev$y)) return(NULL)
    d <- quad(); d <- d[!is.na(d[[COST]]) & !is.na(d$score), ]
    if (nrow(d) == 0) return(NULL)
    xr <- (ev$domain$right - ev$domain$left); if (is.null(xr) || xr == 0) xr <- 1
    yr <- (ev$domain$top - ev$domain$bottom); if (is.null(yr) || yr == 0) yr <- 1
    dist <- sqrt(((d[[COST]] - ev$x) / xr)^2 + ((d$score - ev$y) / yr)^2)
    i <- which.min(dist)
    if (dist[i] > 0.05) return(NULL)  # only when actually near a point
    d[i, ]
  }

  # Clicking pins a municipality → it stays highlighted on the scatter.
  selected_obec <- reactiveVal(NULL)
  observeEvent(input$scatter_click, {
    r <- nearest_row(input$scatter_click)
    selected_obec(if (is.null(r)) NULL else r$kod_obec)
  })

  output$scatter_info <- renderUI({
    h <- nearest_row(input$scatter_hover %||% input$scatter_click)
    if (is.null(h)) return(NULL)
    tags$div(class = "border rounded p-2 mt-2",
      tags$strong(h$obec),
      sprintf(" — skóre %s · %s %s · %s obyv. · %s obyv./km²",
              round(h$score, 1),
              format(round(h[[COST]]), big.mark = " "), COST_UNIT,
              format(round(h$population), big.mark = " "),
              format(round(h$density), big.mark = " ")),
      tags$span(class = "text-muted", sprintf("  [%s]", h$quadrant)))
  })

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

  # Shared column set + header map so all three tables show the same data:
  # rank, obec, score, cost, pop, density, then every active component.
  rank_columns <- function() c("rank", "obec", "score", COST, "population", "density", active_cols())
  rank_names   <- function() {
    act <- active_cols(); beta <- beta_optima()
    comp_map <- setNames(
      vapply(act, function(c) paste0(marker(c, beta), " ", LABS[[c]], " (", UNITS[[c]], ")"), ""),
      act)
    c(common_map, cost_map, comp_map)
  }
  rank_tbl <- function(df, pageLength = 10) base_tbl(df, rank_columns(), rank_names(), pageLength)

  output$full_tbl <- renderDT(rank_tbl(scored(), pageLength = 25))
  output$good_tbl <- renderDT({
    g <- quad(); rank_tbl(g[g$quadrant == GOOD_LAB, ] %>% arrange(desc(score)))
  })
  output$bad_tbl <- renderDT({
    b <- quad(); rank_tbl(b[b$quadrant == BAD_LAB, ] %>% arrange(score))
  })
}

shinyApp(ui, server)
