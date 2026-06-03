# Incien — Waste-Index App

A small, self-contained **R + [Shiny](https://shiny.posit.co/)** web app that scores
all Czech municipalities on the **quality of their waste-management policy**. The user
weights a handful of waste metrics, filters the cohort, and sees each municipality's
composite score plotted against cost, with sortable rankings and the good/bad exemplars.

**🌐 Live demo (no install needed):** <https://matyaslevinsky.github.io/incien-app/>
The live site is the same app compiled to WebAssembly (Shinylive) and hosted on GitHub
Pages — it runs entirely in your browser. The first load downloads the R runtime and
takes ~20–30 s; after that it is cached and fast.

It adds a *policy-quality* dimension alongside cost data. The data flow is **inspired by**
the sibling `municipal-reports` project (both pull from PAQ's internal **druk** API) but
this repo is fully independent.

---

## 🚀 Run it locally — a beginner-friendly tutorial

**You do *not* need to be a programmer, and you do *not* need any passwords or API access
to run the app.** The processed data is already committed to this repo (`app/data/processed/
municipalities.rds`), so you can launch the app straight away. (You only need credentials
if you want to *re-pull* fresh data — see the Pipeline section, which most people can skip.)

### Step 1 — Install R

- Download and install **R** (the programming language) from <https://cran.r-project.org/>.
- *(Optional but recommended for non-programmers)* also install **RStudio Desktop** (a
  friendly point-and-click editor for R) from <https://posit.co/download/rstudio-desktop/>.

### Step 2 — Get the project onto your computer

- If you have `git`: `git clone https://github.com/matyasLevinsky/incien-app.git`
- If you don't: on the GitHub page click the green **Code** button → **Download ZIP**, then
  unzip it.

### Step 3 — Install the R packages the app needs

Open a terminal **in the project folder** (or in RStudio: *Session → Set Working Directory →
To Project/Source File Location*) and run once:

```bash
Rscript requirements.R
```

This installs any missing packages (shiny, bslib, DT, ggplot2, dplyr, shinyjs, …). It can
take a few minutes the first time. *(In RStudio you can instead open `requirements.R` and
click **Source**.)*

### Step 4 — Start the app

```bash
Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
```

A browser tab opens with the app. To stop it, press **Ctrl-C** in the terminal (or click the
red stop sign in RStudio).

> **In RStudio (fully click-based):** open the file `app/app.R` and click the **▶ Run App**
> button in the top-right of the editor. That's it.

**Troubleshooting:** if you see *"Processed data not found"*, make sure you launched from the
project's root folder (the one containing `app/`, not from inside `app/`). If a package fails
to install, re-run `Rscript requirements.R`.

---

## App layout (left sidebar + 6 sections)

The **left sidebar** holds the section navigation **and** a small, always-visible histogram of
the *current index-score distribution* (it updates live as you change weights/filters).

1. **Info** — project explanation + a button jumping to Nastavení.
2. **Nastavení** (settings) — value boxes, the live **formula**, and a **worked example**
   (pick one of five fixed model municipalities spanning the size tiers — Arnolec ~200,
   Chotutice ~500, Troubky ~2 000, Žďár nad Sázavou ~20 000, Hradec Králové ~93 000 — scored
   step by step over all obce). Then a 4-column grid: the long **Filtry** card (left), the
   **Plnění-cíle + Produkce** weights, the **Separace** weights, and the matching **Optimum:
   Separace** sliders (each greys out when its weight is 0).
3. **Přehled** (overview) — the **cost-vs-quality scatter** (cost/capita on X, index on Y,
   median lines splitting four quadrants) + the full sortable ranking. The scatter is
   hover/click interactive; clicking pins a municipality.
4. **Dobré obce** (good) — the low-cost / high-quality quadrant; the plot zooms to it.
5. **Špatné obce** (bad) — the high-cost / low-quality quadrant; the plot zooms to it.
6. **Explorace dat** (data exploration) — a histogram of any single underlying variable over
   all municipalities, with a **linear/log X-axis toggle** and a flag showing how many
   municipalities have a **missing (NA)** value for that metric.

---

## The index

- **Cost is not in the index.** `naklady_per_capita` is the *accompanying* dimension plotted
  against the index on the Přehled scatter. Quadrants split at the median cost and median
  quality of the current selection.
- **Components** are defined once in `app/R/data.R` (`INDEX_COMPONENTS`) — the single source
  of truth for column names, UI labels, sidebar group, "good" direction and default weight.
  14 scored components in three themes:
  - **Plnění cíle** — statutory recycling-target compliance, plus a derived separation share
  - **Produkce** — waste production, 4 categories (municipal / mixed / bulky / construction), kg/capita
  - **Separace** — separated collection, 8 categories (paper, plastic, glass, metal, the
    combined PPSK, bio, textile, hazardous), kg/capita
- **kg per capita:** druk production/separation arrive in tonnes (scale with town size);
  `pipeline/02_process.R` divides by population so the index measures policy, not size.
- **Normalization:** each metric → **percentile rank (0–100)** across the currently-filtered
  cohort, oriented so higher always means better policy (`app/R/index.R`).
- **Score:** weighted mean of the oriented percentiles; components with weight 0 or all-missing
  data drop out. Result tables show only the components whose weight is currently > 0.

### Startup defaults (weights & active filters)

The app launches with an opinionated preset (edit `INDEX_COMPONENTS` defaults and
`FILTER_DEFAULTS` in `app/app.R` to change):

| Setting | Default |
|---|---|
| Weight: Plnění cíle třídění | 50 |
| Weight: Produkce směsný komunální | 50 |
| Weight: Separace PPSK (papír+plast+sklo+kov) | 50 |
| Weight: Separace bio / textil | 50 / 20 (linear) |
| Weight: Produkce komunální / Podíl separace | 0 / 0 |
| Active filter: Produkce komunální | 32–2016 kg/obyv. |
| Active filter: Produkce směsný | 15–1005 kg/obyv. |

Because two filters start narrowed, the opening cohort is a subset of all 6 258 obce — widen
the sliders to full range to see everything.

### Filters

The **Filtry** card has a range slider for **every value** (population, density, cost, and all
14 index components):
- **Population & density** use a **logarithmic** slider (most obce are small, so log gives finer
  control at the low end instead of being dominated by Prague). A live readout shows the real,
  un-logged selected range.
- **Rule:** a slider left at its **full extent does not filter** (keeps all rows, including NAs);
  **narrowing** one keeps only municipalities with a known value inside the range — so unused
  filters never silently drop data.

### Optimum (beta) scoring for separation

The 8 per-capita separation categories can be scored two ways, toggled per category on the
**Optimum: Separace** card:
- **Lineární** (↑ higher = better) — the percentile rank; the default for all categories.
- **Optimum (beta)** (◎) — score peaks (100) at a set **optimum** value and falls to 0 at the
  capped extremes, following a fixed-shape Beta curve (`beta_score` in `app/R/index.R`). Captures
  sweet-spot streams (e.g. bio composted at source means *less* collected bio is fine, so too
  little *and* too much are penalised). Preset optima are kept for **bio (60 kg/obyv.)** and
  **textile (3 kg/obyv.)**, but the toggle is **OFF (linear) by default**; `sep_share` is always
  linear.

Value displays carry a marker: `↑`/`↓` for linear components, `◎` for optimum-scored ones.

**Data caveat:** cost is 2020-only while compliance/production/separation are latest (2023) and
population is the latest year per municipality — so the index mixes years. Documented in the
processed file's `$meta$note` and shown as a footnote in the app.

---

## Pipeline (four stages) — only needed to refresh the data

Most users can skip this entirely; the processed data is committed. Run it only to re-pull
fresh numbers from druk.

| Stage | Script | Credentials? | What it does |
|---|---|---|---|
| Preprocess | `pipeline/00_preprocess.R` | yes | Verify waste variable names/filters/units against the druk catalogue → `data/meta/variable_catalog.rds` |
| Extract | `pipeline/01_extract.R` | yes | Pull each variable for all municipalities; sha256-cached → `data/raw/<id>.rds` |
| Process | `pipeline/02_process.R` | no | Reshape to one row per municipality, compute kg/capita & density → `app/data/processed/municipalities.rds` |
| App | `app/app.R` | no | Interactive index explorer reading the processed file |

Only the first two stages need druk API access. They read `DRUK_API_HOST/PORT/USERNAME/PASSWORD`
from `~/.Renviron` (**never committed**). They also need the in-house `drukAPI` package
(`remotes::install_github("paqresearch/drukAPI")`).

```bash
Rscript pipeline/00_preprocess.R    # confirm variable names (run once, needs credentials)
Rscript pipeline/01_extract.R       # pull raw data            (needs credentials)
Rscript pipeline/02_process.R       # build the app's data file (no credentials)
```

---

## Deployment

Every push to `main` triggers `.github/workflows/deploy.yml`, which compiles the app to
WebAssembly with **Shinylive**, injects a one-time loading splash (`ci/loading.html`), and
publishes the static site to the `gh-pages` branch → GitHub Pages. There is no server; the app
and its data ship to the browser. Because each redeploy changes the asset hashes, the next
visitor re-downloads the runtime once (the splash covers that wait).

## Project structure

```
app/            app.R · www/style.css
  R/            data.R (components, filters, loader) · index.R (scoring) · theme.R (PAQ look)
  data/processed/  municipalities.rds   ← committed; the app's only data input
pipeline/       00_preprocess.R · 01_extract.R · 02_process.R   (data refresh; needs druk)
data/           meta/ + raw/   (pipeline intermediates at repo root)
dev/            render_histograms.R   (local-only: renders the Explorace histograms to PNGs)
ci/             loading.html   (first-load splash, injected at deploy time)
.github/workflows/deploy.yml   (Shinylive build + GitHub Pages deploy)
requirements.R  (install the R packages)
```

The `app/` directory is **self-contained** (relative paths, no `here`) so Shinylive can bundle
it as-is. To change which metrics are scored, their direction, defaults, or filter presets, edit
`app/R/data.R` and `app/app.R`.
