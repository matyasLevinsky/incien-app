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
  library(ggplot2); library(dplyr); library(here); library(shinyjs)
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
    id = "main_nav",
    title = "Index odpadového hospodářství obcí",
    theme = paq_bs_theme(),
    fillable = FALSE,  # natural content height; the page scrolls

    nav_panel(
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

    nav_panel(
      "Nastavení",
      useShinyjs(),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
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

    nav_panel(
      "Přehled",
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

  # "Pokračovat na Nastavení" link on the Info page jumps to the Nastavení tab.
  observeEvent(input$go_nastaveni, nav_select("main_nav", "Nastavení"))

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
        tags$td(paste0(marker(col, beta), " ",
                       if (is.na(m[[col]])) "—" else paste(format(round(m[[col]], 1), big.mark = " "), UNITS[[col]]))),
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
          c("Ukazatel", "Hodnota", "Skóre komp. (0–100)", "Váha", "Příspěvek (váha×skóre)"),
          tags$th))),
        tags$tbody(rows)),
      tags$p(tags$strong(sprintf("Skóre = %s / %s = %s",
             format(round(csum, 1), big.mark = " "), eff_w, round(score, 1)))),
      tags$small(class = "text-muted",
                 "Skóre komponent počítáno vůči všem obcím ČR (nezávisle na filtru). ",
                 "◎ = hodnoceno dle optima (beta), ↑/↓ = lineární (vyšší/nižší = lepší).")
    )
  })

  # ── Scatter ──
  # mode = "all"  → all points, both median lines, full (capped) cost range.
  # mode = "good" → only the good quadrant, zoomed to its region.
  # mode = "bad"  → only the bad quadrant, zoomed to its region.
  scatter_plot <- function(d, mode = "all") {
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
    ggplot(d, aes(.data[[COST]], score, colour = quadrant)) +
      geom_vline(xintercept = c_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_hline(yintercept = q_thr, linetype = "dashed", colour = PAQ$caption) +
      geom_point(alpha = 0.5, size = 1) +
      scale_colour_manual(values = QUAD_COLS, name = NULL) +
      coord_cartesian(xlim = c(xlo, xhi), ylim = c(0, 100)) +
      base + theme_paq_app(12) + theme(legend.position = "top")
  }
  output$scatter   <- renderPlot(scatter_plot(quad(), "all"))
  output$good_plot <- renderPlot(scatter_plot(quad(), "good"))
  output$bad_plot  <- renderPlot(scatter_plot(quad(), "bad"))

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
    act <- active_cols(); beta <- beta_optima()
    comp_map <- setNames(
      vapply(act, function(c) paste0(marker(c, beta), " ", LABS[[c]], " (", UNITS[[c]], ")"), ""),
      act)
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
