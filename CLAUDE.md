# CLAUDE.md

Guidance for working in this repo. See `README.md` for the user-facing tutorial and feature docs.

## What this is

An **R + Shiny** app scoring all Czech municipalities on waste-management policy quality. It is
deployed publicly as a **Shinylive** (WebAssembly) static site on **GitHub Pages**:
<https://matyaslevinsky.github.io/incien-app/>. Pushing to `main` rebuilds and redeploys it.

## Hard constraints (do not violate)

- **Never read, print, or commit `~/.Renviron`** — it holds the druk API credentials
  (`DRUK_API_HOST/PORT/USERNAME/PASSWORD`). It is gitignored and only the pipeline (stages 00/01)
  needs it. The app never does.
- **`app/` must stay self-contained** — it uses relative paths (`source("R/data.R")`,
  `load_processed("data/processed/municipalities.rds")`) and **no `here`**, because
  `shinylive::export("app/", "docs/")` only bundles the `app/` directory. Don't reintroduce
  out-of-`app/` references or `library(here)` into the app.
- **webR safety for the app** — only use packages available as webR binaries and already in the
  app's library block: `shiny, bslib, DT, ggplot2, dplyr, shinyjs`. Do **not** add new visualization
  or interactivity packages (e.g. plotly, leaflet); the scatter hover/click is done with base Shiny
  plot events + manual nearest-point math precisely to survive WebAssembly.
- **Publishing data is intentional and approved.** The processed data (and `data/raw/`) are
  committed and shipped to every visitor; the repo is public on purpose. This is fine — don't treat
  it as a leak.

## Structure / single sources of truth

- `app/app.R` — the whole app (UI + server). Layout: `page_sidebar` with a left-sidebar radio nav
  driving a `navset_hidden`, plus a sticky live index-score histogram in the sidebar.
- `app/R/data.R` — **single source of truth** for scored columns: `INDEX_COMPONENTS` (col, label,
  group, direction, unit, default weight), `SEP_CATEGORIES` + `SEP_OPTIMUM_DEFAULTS` (beta toggle
  defaults, currently all `on = FALSE`), `PRESET_MUNICIPALITIES`, and `load_processed()`.
- `app/R/index.R` — pure scoring (no Shiny/IO): `percentile_rank`, `orient`, `beta_score`,
  `score_component`, `compute_index`. `clamp_*` helpers remain but are **unused** (the clamping UI
  was removed).
- `app/R/theme.R` — PAQ palette (`PAQ` list) + `paq_bs_theme()` + `theme_paq_app()`.
- `app/app.R` filter machinery: `FILTER_SPECS`/`FILTER_EXT` (per-value range sliders; population &
  density are log10-scaled), `FILTER_DEFAULTS` (filters that start narrowed/active).
- `pipeline/02_process.R` — builds `app/data/processed/municipalities.rds` from `data/raw/`. It runs
  on the credentialed machine and still uses a `.find()` helper for dual path layouts — that's
  intentional; leave it.
- `dev/render_histograms.R` — local-only; renders the Explorace histograms to `render_check/` PNGs
  (gitignored). Lives outside `app/` so Shinylive never bundles it.
- `.github/workflows/deploy.yml` — Shinylive build + splash inject (`ci/loading.html`) + Pages deploy.

## Conventions

- **UI text is Czech**; code/comments are English. Match the surrounding tone.
- Use the `PAQ` palette from `theme.R` for all plot colors; `%||%` is defined at the top of `app.R`.
- Keep the index logic in `index.R` pure (testable without Shiny).

## Run / verify / ship

- **Run locally:** `Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'` (from repo root).
- **Headless smoke test:** run on a fixed port in the background, `curl` `/`, expect HTTP 200 and no
  error in the log.
- **Refresh data (needs druk):** `Rscript pipeline/00_preprocess.R` → `01_extract.R` → `02_process.R`.
- **Deploy:** commit + push to `main`; watch `gh run watch <id>`. GitHub Pages serves the prior build
  from its CDN for ~1–2 min after the action finishes, so poll the live `app.json` until the new code
  appears. Note `app.json` contains R *source*, so dynamically-built input ids (e.g. `paste0("f_", col)`)
  won't grep as literals — check stable strings instead.
- End git commit messages with the `Co-Authored-By: Claude …` trailer.
