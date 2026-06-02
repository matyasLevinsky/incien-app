# pipeline/02_process.R
# Stage 2 — turn the raw per-variable druk pulls into one analysis-ready table,
# one row per municipality, USING EVERY CATEGORY. Needs NO credentials.
#
# Usage: Rscript pipeline/02_process.R
# Output: data/processed/municipalities.rds  (list: $data, $components, $meta)
#
# Tonnage categories (production ×4, separation ×8) are converted to kg per
# capita so they are comparable across municipality size. A derived separation
# share (separated / total municipal waste) is added as a headline quality
# metric. NOTE: cost is 2020-only while production/separation/compliance are
# latest available (2023); population uses the latest year per obec.

options(encoding = "UTF-8")

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(here)
})

# Resolve project root on either machine layout, then load shared defs.
# The app is self-contained: helpers + processed data live under app/.
.find <- function(...) { p <- here::here(...); if (file.exists(p)) return(p); here::here("incien-app", ...) }
source(.find("app", "R", "data.R"))

raw_dir  <- .find("data", "raw")                 # pipeline intermediate (root)
proc_dir <- .find("app", "data", "processed")    # app input (bundled by shinylive)
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

read_raw <- function(id) {
  p <- file.path(raw_dir, paste0(id, ".rds"))
  if (!file.exists(p)) { warning("Missing raw file: ", p, call. = FALSE); return(NULL) }
  readRDS(p)
}

# Latest-year single value per obec (for non-categorical variables).
one_val <- function(df, col) {
  if (is.null(df)) return(NULL)
  df %>%
    mutate(val_ = suppressWarnings(as.numeric(hodnota)), date_ = as.Date(platnost_od),
           kod_obec = as.character(id_objekt), obec = as.character(label_objekt)) %>%
    filter(!is.na(val_), !is.na(date_)) %>%
    group_by(kod_obec) %>% slice_max(date_, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(kod_obec, obec, !!col := val_)
}

# Latest-year value per (obec, category), pivoted wide with a column prefix.
cat_wide <- function(df, prefix, recode) {
  if (is.null(df)) return(NULL)
  df %>%
    mutate(val_ = suppressWarnings(as.numeric(hodnota)), date_ = as.Date(platnost_od),
           kod_obec = as.character(id_objekt), cat = recode[h_kat]) %>%
    filter(!is.na(val_), !is.na(date_), !is.na(cat)) %>%
    group_by(kod_obec, cat) %>% slice_max(date_, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(kod_obec, cat, val_) %>%
    pivot_wider(names_from = cat, names_prefix = prefix, values_from = val_)
}

# ── Load & shape each source ────────────────────────────────────────────────
pop   <- one_val(read_raw("02_pocetObyvatel"), "population")               # kod_obec, obec, population
area  <- one_val(read_raw("49_vymeraObce"), "area_ha")            %>% select(kod_obec, area_ha)
nakl  <- one_val(read_raw("68_odpadyNaklady"), "naklady")         %>% select(kod_obec, naklady)
naklp <- one_val(read_raw("68_odpadyNakladyPerCapita"), "naklady_per_capita") %>% select(kod_obec, naklady_per_capita)
compl <- one_val(read_raw("68_odpadyPlneniCileTrideni"), "plneni_cile")  %>%
  mutate(plneni_cile = plneni_cile * 100) %>% select(kod_obec, plneni_cile)  # fraction → %

prod_recode <- c(komunalniOdpad = "komunalni", smesnyKomunalniOdpad = "smesny",
                 objemnyOdpad = "objemny", stavebniOdpad = "stavebni")
prod_t <- cat_wide(read_raw("68_odpadyProdukce"), "prodT_", prod_recode)   # tonnes

sep_recode <- c(papirPlastSkloKov = "ppsk", papir = "papir", plast = "plast", sklo = "sklo",
                kov = "kov", biologickyOdpad = "bio", textilniOdpad = "textil",
                nebezpecnyOdpad = "nebezp")
sep_t <- cat_wide(read_raw("68_odpadySeparace"), "sepT_", sep_recode)      # tonnes

# ── Assemble ────────────────────────────────────────────────────────────────
master <- pop
for (df in list(area, nakl, naklp, compl, prod_t, sep_t)) {
  if (!is.null(df)) master <- left_join(master, df, by = "kod_obec")
}

# kg per capita = tonnes * 1000 / population
pc <- function(t) ifelse(!is.na(t) & !is.na(master$population) & master$population > 0,
                         t * 1000 / master$population, NA_real_)

master <- master %>% mutate(
  density = ifelse(!is.na(area_ha) & area_ha > 0, population / (area_ha / 100), NA_real_),
  # production per capita
  prod_komunalni = pc(prodT_komunalni), prod_smesny = pc(prodT_smesny),
  prod_objemny   = pc(prodT_objemny),   prod_stavebni = pc(prodT_stavebni),
  # separation per capita
  sep_ppsk  = pc(sepT_ppsk),  sep_papir = pc(sepT_papir), sep_plast = pc(sepT_plast),
  sep_sklo  = pc(sepT_sklo),  sep_kov   = pc(sepT_kov),   sep_bio   = pc(sepT_bio),
  sep_textil = pc(sepT_textil), sep_nebezp = pc(sepT_nebezp),
  # derived separation share (%): separated municipal / total municipal waste
  sep_share = ifelse(!is.na(sepT_ppsk) & !is.na(prodT_komunalni) & prodT_komunalni > 0,
                     sepT_ppsk / prodT_komunalni * 100, NA_real_)
)

# Keep identity + filters + cost dimension + scored columns; drop tonnage intermediates.
keep <- c("kod_obec", "obec", "population", "area_ha", "density",
          "naklady", COST_COMPONENT$col, index_cols())
master <- master %>% select(any_of(keep)) %>% arrange(obec)

# Ensure every declared index column exists.
for (col in index_cols()) if (!col %in% names(master)) master[[col]] <- NA_real_

meta <- list(
  note = "Tonnage categories converted to kg/capita; sep_share derived. Cost=2020, other waste=latest(2023), population=latest.",
  n = nrow(master)
)
saveRDS(list(data = master, components = INDEX_COMPONENTS, meta = meta),
        file.path(proc_dir, "municipalities.rds"))

message("Processed ", nrow(master), " municipalities → ",
        file.path(proc_dir, "municipalities.rds"))
message("Scored columns: ", paste(index_cols(), collapse = ", "))
