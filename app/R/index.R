# R/index.R
# Pure, side-effect-free index computation. No Shiny, no I/O — so it can be
# unit-tested in isolation and reused anywhere.

# Percentile rank of x on a 0–100 scale.
#   - NA values stay NA (a municipality missing a metric contributes nothing
#     for that component; the caller drops it from the weighted average).
#   - Ties share the average rank.
#   - With <2 non-NA values percentile is undefined → all non-NA get 50.
percentile_rank <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  n <- sum(ok)
  if (n == 0L) return(out)
  if (n == 1L) { out[ok] <- 50; return(out) }
  # rank() with ties.method = "average", scaled to (0, 100)
  r <- rank(x[ok], ties.method = "average")
  out[ok] <- (r - 1) / (n - 1) * 100
  out
}

# Value at percentile p of x (lower-tail cutoff), NA-safe. Returns NA if no data.
clamp_cutoff <- function(x, p) {
  if (is.null(p) || p <= 0) return(NA_real_)
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}

# Lower-tail winsorization: values below the p-quantile are raised to it.
# Used for cost — municipalities with implausibly low cost (bad data) are
# pulled up to the cutoff. p <= 0 (or NULL) returns x unchanged.
clamp_lower <- function(x, p) {
  cut <- clamp_cutoff(x, p)
  if (is.na(cut)) return(x)
  ifelse(!is.na(x) & x < cut, cut, x)
}

# Two-sided winsorization: clamp to [p-quantile, (1-p)-quantile]. Used for the
# production/separation groups, where bad data appears at both extremes.
clamp_winsor <- function(x, p) {
  if (is.null(p) || p <= 0 || all(is.na(x))) return(x)
  lo <- quantile(x, p,     na.rm = TRUE, names = FALSE, type = 7)
  hi <- quantile(x, 1 - p, na.rm = TRUE, names = FALSE, type = 7)
  ifelse(is.na(x), x, pmin(pmax(x, lo), hi))
}

# Orient a percentile so that higher always means "better policy".
#   direction == "lower"  → low raw value is good → invert.
#   direction == "higher" → high raw value is good → keep.
orient <- function(p, direction) {
  if (identical(direction, "lower")) 100 - p else p
}

# Fixed concentration (alpha+beta) of the optimum beta curve. Higher = sharper.
BETA_CONC <- 8

# "Closeness to optimum" score on 0–100, shaped by a Beta distribution.
#   x        – metric values.
#   opt      – the optimum (best) value; scores peak here.
#   lo, hi   – the value range that maps to the beta's [0,1] support (the capped
#              extremes). Scores fall to ~0 at lo and hi.
#   conc     – beta concentration (alpha+beta).
# Returns NA for NA x (and all-NA if the range is degenerate).
beta_score <- function(x, opt, lo, hi, conc = BETA_CONC) {
  out <- rep(NA_real_, length(x))
  if (is.na(lo) || is.na(hi) || hi <= lo) return(out)
  u  <- pmin(pmax((x - lo) / (hi - lo), 0), 1)
  uo <- min(max((opt - lo) / (hi - lo), 1e-3), 1 - 1e-3)
  a  <- uo * (conc - 2) + 1
  b  <- conc - a
  peak <- dbeta(uo, a, b)
  ok <- !is.na(x)
  out[ok] <- dbeta(u[ok], a, b) / peak * 100
  out
}

# Single source of per-component scoring on 0–100.
#   opt = NULL/NA  → linear: oriented percentile rank (higher = better policy).
#   opt is a value → beta: closeness to that optimum over range(x).
score_component <- function(x, direction, opt = NULL, conc = BETA_CONC) {
  if (is.null(opt) || is.na(opt)) return(orient(percentile_rank(x), direction))
  rng <- suppressWarnings(range(x, na.rm = TRUE))
  if (!all(is.finite(rng))) return(rep(NA_real_, length(x)))
  beta_score(x, opt, rng[1], rng[2], conc)
}

# Flat zero/NA penalty multiplier (0..1) per row. If ANY penalized column is
# zero-or-missing, that row's score is multiplied by (1 - strength); else 1.
# strength is a fraction in [0,1]. No cols / strength<=0 → all 1 (no penalty).
zero_penalty_multiplier <- function(df, cols, strength) {
  if (length(cols) == 0 || is.null(strength) || strength <= 0) return(rep(1, nrow(df)))
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(rep(1, nrow(df)))
  hit <- rep(FALSE, nrow(df))
  for (col in cols) { v <- df[[col]]; hit <- hit | is.na(v) | v == 0 }
  ifelse(hit, 1 - strength, 1)
}

# Compute the composite waste index for a data frame of municipalities.
#
#   df          – municipalities table (one row each); must contain the columns
#                 named in names(weights).
#   weights     – named numeric vector: component column name → weight (>= 0).
#                 Components with weight 0 (or NA) are excluded entirely.
#   directions  – named character vector: component column → "lower"/"higher".
#                 Defaults to index_directions() from R/data.R if available.
#   beta        – optional named numeric (column → optimum value). Components
#                 listed here are scored by closeness-to-optimum (beta) instead
#                 of the linear oriented percentile.
#
# Returns df with two added columns:
#   score    – weighted mean of per-component scores, 0–100 (NA if a row has no
#              scorable component among those with positive weight).
#   n_used   – how many components actually contributed to that row's score.
#
# Scores are computed over the ROWS PASSED IN, i.e. the current cohort.
compute_index <- function(df, weights, directions = NULL, beta = NULL) {
  if (is.null(directions)) directions <- index_directions()

  weights <- weights[!is.na(weights) & weights > 0]
  comps <- intersect(names(weights), names(df))

  if (length(comps) == 0L) {
    df$score <- NA_real_
    df$n_used <- 0L
    return(df)
  }

  # Per-component score matrix (linear percentile or beta-optimum per column).
  oriented <- vapply(comps, function(col) {
    opt <- if (col %in% names(beta)) beta[[col]] else NULL
    score_component(df[[col]], directions[[col]], opt = opt)
  }, numeric(nrow(df)))
  oriented <- matrix(oriented, nrow = nrow(df), dimnames = list(NULL, comps))

  w <- weights[comps]
  # Row-wise weighted mean ignoring NA components (weights renormalised per row).
  wmat <- matrix(w, nrow = nrow(df), ncol = length(w), byrow = TRUE)
  wmat[is.na(oriented)] <- 0
  num <- rowSums(oriented * wmat, na.rm = TRUE)
  den <- rowSums(wmat)

  df$score <- ifelse(den > 0, num / den, NA_real_)
  df$n_used <- rowSums(!is.na(oriented))
  df
}
