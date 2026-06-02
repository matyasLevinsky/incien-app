# pipeline/00_preprocess.R
# Stage 0 — discover & verify the druk variables we intend to extract.
# Needs druk API credentials (DRUK_API_* in ~/.Renviron). Run once before
# editing 01_extract.R so the variable ids / filters / units are confirmed.
#
# Usage: Rscript pipeline/00_preprocess.R
#
# Output: data/meta/variable_catalog.rds  (+ a readable console summary)

options(encoding = "UTF-8")

library(drukAPI)
library(dplyr)
library(here)

meta_dir <- here::here("data", "meta")
dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)

API_connect()

# Variables we want for the waste index + filters. Two of the waste ids are
# confirmed (per-capita cost, separation-target compliance); the other three
# are the intended names and are what this step verifies.
WANT <- c(
  "68_odpadyNaklady",
  "68_odpadyNakladyPerCapita",
  "68_odpadyPlneniCileTrideni",
  "68_odpadyProdukce",
  "68_odpadySeparace",
  "02_pocetObyvatel",
  "49_vymeraObce"
)

# Full catalogue of available variables (id, label, unit, ...).
message("Fetching variable catalogue via dataAvailability()...")
catalog <- tryCatch(dataAvailability(), error = function(e) {
  message("  dataAvailability() failed: ", conditionMessage(e)); NULL
})

# Anything matching the waste family, so we can see real names if ours are off.
waste_like <- if (!is.null(catalog)) {
  id_col <- intersect(c("id_promenna", "promenna", "id"), names(catalog))[1]
  catalog[grepl("68_odpad", catalog[[id_col]], ignore.case = TRUE), , drop = FALSE]
} else NULL

# For each wanted variable, pull its filter list to confirm "all" + obec level.
filtry <- lapply(WANT, function(v) {
  message("── getFiltry('", v, "')")
  tryCatch(getFiltry(v), error = function(e) {
    message("   FAILED: ", conditionMessage(e)); NULL
  })
})
names(filtry) <- WANT

found <- vapply(filtry, function(x) !is.null(x) && NROW(x) > 0, logical(1))

out <- list(
  wanted     = WANT,
  found      = found,
  filtry     = filtry,
  waste_like = waste_like,
  catalog    = catalog
)
saveRDS(out, file.path(meta_dir, "variable_catalog.rds"))

message("\n── Summary ──────────────────────────────────────────────")
for (v in WANT) message(sprintf("  %-30s %s", v, if (found[[v]]) "OK" else "NOT FOUND"))
if (!is.null(waste_like) && nrow(waste_like) > 0) {
  message("\nAll '68_odpad*' variables present in the catalogue:")
  print(waste_like)
}
message("\nSaved → ", file.path(meta_dir, "variable_catalog.rds"))
message("If any waste id is NOT FOUND, copy its real name from the list above ",
        "into WANT here, into EXTRACTS in 01_extract.R, and into INDEX_COMPONENTS in R/data.R.")
