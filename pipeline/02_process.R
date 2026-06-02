# pipeline/02_process.R
# Stage 2 — turn the raw per-variable .rds files into one analysis-ready table,
# one row per municipality. Needs NO credentials; runs against data/raw/.
#
# Usage: Rscript pipeline/02_process.R
# Output: data/processed/municipalities.rds  (list: $data, $components)

options(encoding = "UTF-8")

library(dplyr)
library(here)

source(here::here("R", "data.R"))  # INDEX_COMPONENTS, FILTER_COMPONENTS

raw_dir <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

# raw_id → clean processed column name (covers index + filter inputs).
COLMAP <- c(
  setNames(vapply(INDEX_COMPONENTS,  `[[`, "", "col"),
           vapply(INDEX_COMPONENTS,  `[[`, "", "raw_id")),
  setNames(vapply(FILTER_COMPONENTS, `[[`, "", "col"),
           vapply(FILTER_COMPONENTS, `[[`, "", "raw_id"))
)

# Long druk table → latest-year snapshot, one row per obec.
latest_snapshot <- function(df, value_col) {
  df %>%
    mutate(
      .value = suppressWarnings(as.numeric(hodnota)),
      .date  = as.Date(platnost_od)
    ) %>%
    filter(!is.na(.value), !is.na(.date)) %>%
    group_by(kod_obec = as.character(id_objekt)) %>%
    slice_max(.date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      kod_obec,
      obec = as.character(label_objekt),
      !!value_col := .value
    )
}

snapshots <- list()
for (raw_id in names(COLMAP)) {
  path <- file.path(raw_dir, paste0(raw_id, ".rds"))
  if (!file.exists(path)) {
    warning("Missing raw file (skipped): ", path, call. = FALSE)
    next
  }
  message("── ", raw_id, " → ", COLMAP[[raw_id]])
  snapshots[[raw_id]] <- latest_snapshot(readRDS(path), COLMAP[[raw_id]])
}

if (length(snapshots) == 0L) stop("No raw files found in ", raw_dir, ". Run 01_extract.R first.")

# Full-join all snapshots on kod_obec; keep the first non-NA municipality name.
master <- Reduce(function(a, b) {
  full_join(a, b, by = c("kod_obec", "obec"))
}, lapply(snapshots, function(s) s)) %>%
  # full_join on (kod_obec, obec) can split rows where a name differs; collapse.
  group_by(kod_obec) %>%
  summarise(
    obec = dplyr::first(na.omit(obec)),
    across(where(is.numeric), ~ dplyr::first(na.omit(.x))),
    .groups = "drop"
  )

# Density: inhabitants per km² (area is in hectares; 1 km² = 100 ha).
if (all(c("population", "area_ha") %in% names(master))) {
  master <- master %>%
    mutate(density = ifelse(!is.na(area_ha) & area_ha > 0, population / (area_ha / 100), NA_real_))
}

# Ensure every declared index column exists (NA if its raw file was absent).
for (col in index_cols()) if (!col %in% names(master)) master[[col]] <- NA_real_

master <- master %>% arrange(obec)

saveRDS(
  list(data = master, components = INDEX_COMPONENTS),
  file.path(proc_dir, "municipalities.rds")
)

message("\nProcessed ", nrow(master), " municipalities → ",
        file.path(proc_dir, "municipalities.rds"))
message("Columns: ", paste(names(master), collapse = ", "))
