# pipeline/01_extract.R
# Stage 1 — raw data extraction via the druk API.
# Fetches each variable for ALL municipalities in one call. One .rds per
# variable in data/raw/. Needs druk credentials (DRUK_API_* in ~/.Renviron).
#
# Usage:
#   Rscript pipeline/01_extract.R
#   FORCE_REFRESH=1 Rscript pipeline/01_extract.R   # ignore cache, refetch all
#
# Pattern (incl. the sha256 per-variable cache) mirrors the approach used in
# the sibling municipal-reports project, but this project is self-contained.

options(encoding = "UTF-8")

library(drukAPI)
library(dplyr)
library(here)

raw_dir <- here::here("data", "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

API_connect()

# 5-year window so the processing step can pick the latest available year.
date_from <- "2020-01-01"
date_to   <- "2025-12-31"

# Helper for the common obec-level, aggregate ("all") spec.
obec_all <- function(id_promenna) {
  list(
    id = id_promenna,
    promenne = list(
      list(filtry = list(
        id_promenna     = id_promenna,
        id_typ_objekt   = "obec",
        id_filtr        = "all",
        platne_v_obdobi = c(date_from, date_to)
      ))
    )
  )
}

# ── Variables ───────────────────────────────────────────────────────────────
# Waste index components + population + area (for density). Confirm the three
# unverified waste ids with pipeline/00_preprocess.R first; adjust here if the
# real names differ.
EXTRACTS <- list(
  obec_all("68_odpadyNaklady"),
  obec_all("68_odpadyNakladyPerCapita"),
  obec_all("68_odpadyPlneniCileTrideni"),
  obec_all("68_odpadyProdukce"),
  obec_all("68_odpadySeparace"),
  obec_all("02_pocetObyvatel"),
  obec_all("49_vymeraObce")
)

# ── Cache control ─────────────────────────────────────────────────────────────
# Per-variable cache: data/raw/.extract_cache.rds maps each id → sha256 of its
# $promenne spec. A variable is skipped iff its .rds exists AND the hash matches.
force_refresh <- isTRUE(as.logical(Sys.getenv("FORCE_REFRESH", "false")))

cache_path <- file.path(raw_dir, ".extract_cache.rds")
manifest <- if (!force_refresh && file.exists(cache_path)) readRDS(cache_path) else list()

message("Extracting ", length(EXTRACTS), " variable(s)...")
if (force_refresh) message("FORCE_REFRESH=1 – ignoring cache.\n")

results <- lapply(EXTRACTS, function(spec) {
  out_path <- file.path(raw_dir, paste0(spec$id, ".rds"))
  cur_hash <- digest::digest(spec$promenne, algo = "sha256")
  cached <- !force_refresh && file.exists(out_path) &&
    identical(manifest[[spec$id]], cur_hash)

  if (cached) {
    message("── ", spec$id, "  (cached)")
    return(TRUE)
  }

  message("── ", spec$id)
  tryCatch({
    df <- dplyr::bind_rows(getDataAll(spec$promenne))
    saveRDS(df, out_path)
    manifest[[spec$id]] <<- cur_hash
    saveRDS(manifest, cache_path)
    message("   ", nrow(df), " rows → ", out_path)
    TRUE
  }, error = function(e) {
    message("   FAILED: ", conditionMessage(e))  # manifest left untouched → retry next run
    FALSE
  })
})

n_ok <- sum(unlist(results))
message("\nDone. ", n_ok, "/", length(EXTRACTS), " variable(s) extracted.")
