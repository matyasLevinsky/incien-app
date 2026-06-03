# R/data.R
# Shared data definitions and loader for the Incien waste-index app.
# SINGLE SOURCE OF TRUTH for which columns are scored, how they are labelled and
# grouped in the UI, which direction is "good", and the default slider weight.
# Sourced by pipeline/02_process.R and app/app.R.

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

  # ── Produkce odpadu (production, kg/obyv., less = better) ──
  list(col = "prod_komunalni", label = "Produkce: komunální odpad",     group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),
  list(col = "prod_smesny",    label = "Produkce: směsný komunální",    group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 50),
  list(col = "prod_objemny",   label = "Produkce: objemný odpad",       group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),
  list(col = "prod_stavebni",  label = "Produkce: stavební a demoliční", group = "Produkce",
       direction = "lower", unit = "kg/obyv.", default = 0),

  # ── Separace (separated collection, more = better) ──
  # Derived headline share first, then the per-capita category tonnages.
  list(col = "sep_share",  label = "Podíl separace (vytříděno / komunální odpad)",
       group = "Separace", direction = "higher", unit = "%", default = 0),
  list(col = "sep_ppsk",   label = "Separace: papír+plast+sklo+kov", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 50),
  list(col = "sep_papir",  label = "Separace: papír",   group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_plast",  label = "Separace: plast",   group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_sklo",   label = "Separace: sklo",    group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_kov",    label = "Separace: kov",     group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0),
  list(col = "sep_bio",    label = "Separace: biologický odpad", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 50),
  list(col = "sep_textil", label = "Separace: textil",  group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 20),
  list(col = "sep_nebezp", label = "Separace: nebezpečný odpad", group = "Separace",
       direction = "higher", unit = "kg/obyv.", default = 0)
)

# The accompanying cost dimension — plotted against the index, never scored.
COST_COMPONENT <- list(col = "naklady_per_capita", label = "Náklady na obyvatele",
                       unit = "Kč/obyv.", direction = "lower")

# Per-capita separation categories that can switch from linear (higher = better)
# to beta "closeness-to-optimum" scoring (everything in group "Separace" except
# the derived share). Beta is OFF (linear) by default for every category; bio and
# textile keep their preset optimum values for when the user enables the toggle.
SEP_CATEGORIES <- c("sep_ppsk", "sep_papir", "sep_plast", "sep_sklo",
                    "sep_kov", "sep_bio", "sep_textil", "sep_nebezp")
SEP_OPTIMUM_DEFAULTS <- list(
  sep_bio    = list(on = FALSE, opt = 60),
  sep_textil = list(on = FALSE, opt = 3)
)
sep_optimum_default <- function(col) {
  SEP_OPTIMUM_DEFAULTS[[col]] %||% list(on = FALSE, opt = 0)
}

# Fixed model municipalities for the worked example, spanning the size tiers.
# Real obce with complete data and typical values; identified by kód obce so
# the example stays stable when the index weights change.
PRESET_MUNICIPALITIES <- list(
  list(kod_obec = "586854", name = "Arnolec",          tier = "~200 obyv."),
  list(kod_obec = "533343", name = "Chotutice",        tier = "~500 obyv."),
  list(kod_obec = "519651", name = "Troubky",          tier = "~2 000 obyv."),
  list(kod_obec = "595209", name = "Žďár nad Sázavou", tier = "~20 000 obyv."),
  list(kod_obec = "569810", name = "Hradec Králové",   tier = "~93 000 obyv.")
)

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
# Reads the processed bundle written by pipeline/02_process.R. The default path
# is relative to the app directory (the working dir under runApp("app") and in
# shinylive), so it works both locally and in the WebAssembly build.
load_processed <- function(path = "data/processed/municipalities.rds") {
  if (!file.exists(path)) {
    stop("Processed data not found at '", path, "'.\n",
         "Run the pipeline first: pipeline/01_extract.R then pipeline/02_process.R.")
  }
  readRDS(path)
}
