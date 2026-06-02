# Incien — Waste-Index App

A small, self-contained R Shiny app that scores all Czech municipalities on the
**quality of their waste-management policy**. The user weights a handful of waste
metrics, filters by population and density, and sees each municipality's composite
score together with the **top 10** and **bottom 10**.

It adds a *policy-quality* dimension alongside the existing cost data. The data
flow is **inspired by** the sibling `municipal-reports` project (both pull from
PAQ's internal **druk** API) but this repo is fully independent.

## Pipeline (four stages)

| Stage | Script | Credentials? | What it does |
|---|---|---|---|
| Preprocess | `pipeline/00_preprocess.R` | yes | Verify the waste variable names / filters / units against the druk catalogue → `data/meta/variable_catalog.rds` |
| Extract | `pipeline/01_extract.R` | yes | Pull each variable for all municipalities; sha256-cached → `data/raw/<id>.rds` |
| Process | `pipeline/02_process.R` | no | Reshape to one row per municipality, compute density → `data/processed/municipalities.rds` |
| App | `app/app.R` | no | Interactive index explorer reading the processed file |

Only the first two stages need druk API access. They read
`DRUK_API_HOST/PORT/USERNAME/PASSWORD` from `~/.Renviron`.

## Quick start

```bash
Rscript requirements.R              # install any missing packages (most already present)

Rscript pipeline/00_preprocess.R    # confirm variable names (run once)
Rscript pipeline/01_extract.R       # pull raw data  (needs druk credentials)
Rscript pipeline/02_process.R       # build the app's data file

Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
```

## The index

- **Components** (the 5 waste metrics) are defined once in `R/data.R`
  (`INDEX_COMPONENTS`) — the single source of truth for column names, labels and
  the "good" direction of each metric.
- **Normalization:** each metric is converted to a **percentile rank (0–100)**
  across the currently-filtered cohort, then oriented so higher always means
  better policy (`R/index.R`).
- **Score:** weighted mean of the oriented percentiles using the slider weights;
  components with weight 0 or all-missing data drop out (`compute_index`).
- Population and density are **filters only**, never scored.

To change which metrics are scored or their direction, edit `INDEX_COMPONENTS`
in `R/data.R` (and the matching `EXTRACTS` in `pipeline/01_extract.R`).

## Layout

```
pipeline/   00_preprocess.R · 01_extract.R · 02_process.R
R/          data.R (components + loader) · index.R (scoring) · theme.R (PAQ look)
app/        app.R · www/style.css
data/       meta/ (committed) · raw/ + processed/ (gitignored, regenerated)
```
