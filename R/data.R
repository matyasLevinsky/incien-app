# R/data.R
# Shared data definitions and loader for the Incien waste-index app.
# SINGLE SOURCE OF TRUTH for which columns are scored, how they are labelled and
# grouped in the UI, which direction is "good", and the default slider weight.
# Sourced by pipeline/02_process.R and app/app.R.

# ── Cross-machine path resolver ─────────────────────────────────────────────
# The here-root differs between machines: on the extraction machine it is the
# parent of `incien-app`, here it is `incien-app` itself. proj_path() returns
# the first candidate that exists, so the same code works on both layouts.
proj_path <- function(...) {
  a <- here::here(...)
  if (file.exists(a)) return(a)
  b <- here::here("incien-app", ...)
  if (file.exists(b)) return(b)
  a  # default to the here-root candidate (for files about to be created)
}

# ── Index components ────────────────────────────────────────────────────────
# Each entry:
#   col       – column in the processed municipalities table
#   label     – slider label
#   group     – accordion group in the sidebar
#   direction – "lower" or "higher" (higher always = better policy after orient)
#   unit      – display unit
#   default   – default slider weight (0–100)
#
# Tonnage categories from druk (production ×4, separation ×8) are converted to
# kg per capita in 02_process.R so they are comparable across municipality size.
# `sep_share` is derived (separated ÷ total municipal waste).
# NOTE: cost (naklady_per_capita) is intentionally NOT an index component — it is
# the accompanying dimension plotted against the index (see COST_COMPONENT).
INDEX_COMPONENTS <- list(
  # ── Plnění cíle třídění (statutory recycling-target compliance) ──
  list(col = "plneni_cile",        label = "Plnění cíle třídění",  group = "Plnění cíle",
       direction = "higher", unit = "%",        default = 50),

  # ── Separační podíl (derived headline quality metric) ──
  list(col = "sep_share",          label = "Podíl separace (vytříděno / komunální odpad)",
       group = "Plnění cíle", direction = "higher", unit = "%", default = 50),

  # ── Produkce odpadu (production, kg/obyv., less = better) ──
  list(col = "prod_komunalni", label = "Produkce: komunální odpad",     group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 50),
  list(col = "prod_smesny",    label = "Produkce: směsný komunální",    group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),
  list(col = "prod_objemny",   label = "Produkce: objemný odpad",       group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),
  list(col = "prod_stavebni",  label = "Produkce: stavební a demoliční", group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),

  # ── Separace (separated collection, kg/obyv., more = better) ──
  list(col = "sep_ppsk",   label = "Separace: papír+plast+sklo+kov", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_papir",  label = "Separace: papír",   group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_plast",  label = "Separace: plast",   group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_sklo",   label = "Separace: sklo",    group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_kov",    label = "Separace: kov",     group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_bio",    label = "Separace: biologický odpad", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_textil", label = "Separace: textil",  group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_nebezp", label = "Separace: nebezpečný odpad", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0)
)

# The accompanying cost dimension — plotted against the index, never scored.
COST_COMPONENT <- list(col = "naklady_per_capita", label = "Náklady na obyvatele",
                       unit = "Kč/obyv.", direction = "lower")

# Columns used only for filtering / display, never scored.
FILTER_COMPONENTS <- list(
  list(col = "population", label = "Počet obyvatel", unit = "obyv."),
  list(col = "density",    label = "Hustota",        unit = "obyv./km²")
)

# Convenience accessors -------------------------------------------------------
index_cols       <- function() vapply(INDEX_COMPONENTS, `[[`, "", "col")
index_labels     <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "label"), index_cols())
index_directions <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "direction"), index_cols())
index_groups     <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "group"), index_cols())
index_defaults   <- function() setNames(vapply(INDEX_COMPONENTS, function(x) as.numeric(x$default), 0), index_cols())
index_units      <- function() setNames(vapply(INDEX_COMPONENTS, `[[`, "", "unit"), index_cols())

# ── Loader ──────────────────────────────────────────────────────────────────
# Reads the processed bundle written by pipeline/02_process.R.
load_processed <- function(path = NULL) {
  if (is.null(path)) path <- proj_path("data", "processed", "municipalities.rds")
  if (!file.exists(path)) {
    stop("Processed data not found at '", path, "'.\n",
         "Run the pipeline first: pipeline/01_extract.R then pipeline/02_process.R.")
  }
  readRDS(path)
}
