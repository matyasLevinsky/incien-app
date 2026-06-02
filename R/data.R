# R/data.R
# Shared data definitions and loader for the Incien waste-index app.
# This file is the SINGLE SOURCE OF TRUTH for which columns are scored, how
# they are labelled in the UI, and which direction is "good". It is sourced by
# both pipeline/02_process.R (to stamp the metadata into the processed file)
# and app/app.R (to build the sliders).

# ── Index components ────────────────────────────────────────────────────────
# Each entry:
#   col       – column name in the processed municipalities table
#   raw_id    – druk id_promenna it originates from (for traceability)
#   label     – human label shown on the weight slider
#   direction – "lower" (lower raw value = better policy) or
#               "higher" (higher raw value = better policy)
#   unit      – display unit (confirm/adjust against preprocess output)
#
# NOTE: directions/units below are best-effort and should be reconciled with
# data/meta/variable_catalog.rds produced by pipeline/00_preprocess.R.
INDEX_COMPONENTS <- list(
  list(col = "naklady",            raw_id = "68_odpadyNaklady",
       label = "Celkové náklady na odpady",        direction = "lower",  unit = "Kč"),
  list(col = "naklady_per_capita", raw_id = "68_odpadyNakladyPerCapita",
       label = "Náklady na odpady na obyvatele",   direction = "lower",  unit = "Kč/obyv."),
  list(col = "plneni_cile",        raw_id = "68_odpadyPlneniCileTrideni",
       label = "Plnění cíle třídění",              direction = "higher", unit = "%"),
  list(col = "produkce",           raw_id = "68_odpadyProdukce",
       label = "Produkce odpadu",                   direction = "lower",  unit = "kg/obyv."),
  list(col = "separace",           raw_id = "68_odpadySeparace",
       label = "Míra separace",                     direction = "higher", unit = "%")
)

# Columns used only for filtering / display, never scored.
FILTER_COMPONENTS <- list(
  list(col = "population", raw_id = "02_pocetObyvatel", label = "Počet obyvatel", unit = "obyv."),
  list(col = "area_ha",    raw_id = "49_vymeraObce",    label = "Výměra",         unit = "ha")
)

# Convenience accessors -------------------------------------------------------
index_cols       <- function() vapply(INDEX_COMPONENTS, `[[`, "", "col")
index_labels     <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "label"), index_cols())
index_directions <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "direction"), index_cols())

# ── Loader ──────────────────────────────────────────────────────────────────
# Reads the processed bundle written by pipeline/02_process.R. Returns a list
# with $data (one row per municipality) and $components (INDEX_COMPONENTS as
# stamped at process time). `path` is overridable for tests/fixtures.
load_processed <- function(path = NULL) {
  if (is.null(path)) {
    root <- tryCatch(here::here(), error = function(e) getwd())
    path <- file.path(root, "data", "processed", "municipalities.rds")
  }
  if (!file.exists(path)) {
    stop("Processed data not found at '", path, "'.\n",
         "Run the pipeline first: pipeline/01_extract.R then pipeline/02_process.R.")
  }
  readRDS(path)
}
