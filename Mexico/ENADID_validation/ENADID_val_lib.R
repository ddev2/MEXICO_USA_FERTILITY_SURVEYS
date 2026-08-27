# ENADID_val_lib.R ======
# Shared helper functions for the ENADID validation reimplementation.
# Independent of enadid_lib.R — written from scratch for cross-checking purposes.
# Mirrors the structure of NSFG_val_lib.R; differences are noted inline.

library(tidyverse)
library(foreign)   # read.dbf
library(bit64)     # integer64 keys (2018, 2023)


# CMC helpers ======
# (Identical to NSFG_val_lib.R — reproduced here so ENADID_val_lib.R is standalone)

val_cmc <- function(month, year) {
  month <- as.integer(month)
  year  <- as.integer(year)
  year  <- dplyr::case_when(
    year == 99L              ~ NA_integer_,
    year >= 1L & year <= 98L ~ year + 1900L,
    TRUE                     ~ year
  )
  month <- dplyr::if_else(month >= 1L & month <= 12L, month, NA_integer_)
  dplyr::if_else(!is.na(year) & !is.na(month), (year - 1900L) * 12L + month, NA_integer_)
}

val_year_from_cmc <- function(cmc) {
  dplyr::if_else(!is.na(cmc) & cmc < 9000L,
                 1900L + as.integer((cmc - 1L) %/% 12L),
                 NA_integer_)
}

# Imputation flag: 0 = exact, 1 = month imputed, 10 = year imputed, NA = missing
val_imputed <- function(month, year) {
  month <- as.integer(month)
  year  <- as.integer(year)
  dplyr::case_when(
    is.na(month) & is.na(year)              ~ NA_integer_,
    is.na(year)  | year >= 9000L            ~ 10L,
    is.na(month) | month < 1L | month > 12L ~  1L,
    TRUE                                    ~  0L
  )
}

rand_month <- function(n) sample(1L:12L, n, replace = TRUE)


# ENADID-specific year helpers ======

# Fix 2-digit years to 4-digit; set 99/9999 → NA_integer_ (treated as missing).
# lo/hi: the plausible 2-digit range to expand (e.g. lo=20, hi=97 for 1997 DOBs).
val_fix_year <- function(v, lo = 0L, hi = 99L) {
  x <- as.integer(v)
  dplyr::case_when(
    x == 99L | x == 9999L      ~ NA_integer_,
    x >= lo & x <= hi          ~ x + 1900L,
    TRUE                       ~ x
  )
}

# Build CMC from separate month + year columns, applying val_fix_year first.
# For cycles where years are already 4-digit, just pass lo > hi (e.g. lo=100L).
val_cmc_enadid <- function(month, year, lo = 100L, hi = 99L) {
  m <- as.integer(month)
  y <- val_fix_year(year, lo, hi)
  val_cmc(m, y)
}

val_imputed_enadid <- function(month, year, lo = 100L, hi = 99L) {
  m <- as.integer(month)
  y <- val_fix_year(year, lo, hi)
  val_imputed(m, y)
}


# Output schema ======
# Identical column set to NSFG_val_lib.R.
# DIFFERENCE: CaseID is kept as character (llave_muj composite key).

SCHEMA_FIXED <- c(
  "country", "survey", "CaseID",
  "surveyDate_cmc", "lastYear",
  "indiv_dob_cmc", "indiv_dob_cmc_I",
  "yBirth",
  "indiv_age_survey",
  "indiv_weight",
  "union_status",
  "pregnant", "want_another", "prob_want_another",
  "age_first_sex", "ever_contraception",
  "nUnion",
  "nBioKids"
)

MAX_UNIONS <- 10L
MAX_BIRTHS <- 20L

union_cols <- function(u) {
  c(paste0("union_start_type",     u),
    paste0("union_start_cmc",      u),
    paste0("union_start_cmc_I",    u),
    paste0("marriage_start_cmc",   u),
    paste0("marriage_start_cmc_I", u),
    paste0("union_end_cmc",        u),
    paste0("union_end_cmc_I",      u),
    paste0("union_end_motive",     u))
}

birth_cols <- function(b) {
  c(paste0("dob_cmc",   b),
    paste0("dob_cmc_I", b),
    paste0("sex",       b),
    paste0("dod_cmc",   b),
    paste0("dod_cmc_I", b))
}

SCHEMA_ALL <- c(
  SCHEMA_FIXED,
  unlist(lapply(1:MAX_UNIONS, union_cols)),
  unlist(lapply(1:MAX_BIRTHS, birth_cols))
)


# Factor levels / labels ======

# union_start_type: same as NSFG
UNION_START_TYPE_LEVELS <- c(1L, 2L, 3L)
UNION_START_TYPE_LABELS <- c("marriage", "cohabitation", "cohabitation before marriage")

# union_end_motive: same as NSFG
# ENADID "divorce" is mapped to "separation" (code 2)
UNION_END_MOTIVE_LEVELS <- c(0L, 1L, 2L, 9L)
UNION_END_MOTIVE_LABELS <- c("in union", "widowhood", "separation", "unknown")

# pregnant: same codes as NSFG
PREGNANT_LEVELS <- c(1L, 2L, 8L, 9L)
PREGNANT_LABELS <- c("yes", "no", "refused", "unknown")

# want_another: same codes as NSFG; ENADID "yes but can't" → 3 ("disagree")
WANT_ANOTHER_LEVELS <- c(1L, 2L, 3L, 8L, 9L)
WANT_ANOTHER_LABELS <- c("yes", "no", "disagree", "refused", "unknown")

# prob_want_another: not collected in ENADID; always NA
PROB_WANT_ANOTHER_LEVELS <- c(1L, 2L, 8L, 9L)
PROB_WANT_ANOTHER_LABELS <- c("probably yes", "probably no", "refused", "unknown")

# union_status: collapsed harmonised set (user choice)
# Mapping from ENADID raw codes is done per cycle in ENADID_val_import.R.
UNION_STATUS_LEVELS <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L)
UNION_STATUS_LABELS <- c("married", "cohabiting", "separated", "divorced",
                          "widowed", "single", "unknown")


# enforce_schema ======
enforce_schema <- function(df) {
  missing_cols <- setdiff(SCHEMA_ALL, names(df))
  df[missing_cols] <- NA
  df[, intersect(SCHEMA_ALL, names(df))]
}


# val_order_union_history ======
val_order_union_history <- function(df, n_unions) {
  if (n_unions <= 1L) return(df)

  cmc_cols    <- paste0("union_start_cmc",      1:n_unions)
  cmc_I_cols  <- paste0("union_start_cmc_I",    1:n_unions)
  type_cols   <- paste0("union_start_type",      1:n_unions)
  marr_cols   <- paste0("marriage_start_cmc",    1:n_unions)
  marr_I_cols <- paste0("marriage_start_cmc_I",  1:n_unions)
  end_cols    <- paste0("union_end_cmc",         1:n_unions)
  end_I_cols  <- paste0("union_end_cmc_I",       1:n_unions)
  mot_cols    <- paste0("union_end_motive",      1:n_unions)

  for (row in seq_len(nrow(df))) {
    cmcs <- as.integer(df[row, cmc_cols])
    if (all(is.na(cmcs))) next
    ord  <- order(cmcs, na.last = TRUE)
    if (identical(ord, seq_len(n_unions))) next

    for (col_group in list(cmc_cols, cmc_I_cols, type_cols, marr_cols,
                           marr_I_cols, end_cols, end_I_cols, mot_cols)) {
      df[row, col_group] <- df[row, col_group[ord]]
    }
  }
  df
}


# val_reorder_births ======
val_reorder_births <- function(df, n_births) {
  if (n_births <= 1L) return(df)

  cmc_cols   <- paste0("dob_cmc",   1:n_births)
  I_cols     <- paste0("dob_cmc_I", 1:n_births)
  sex_cols   <- paste0("sex",       1:n_births)
  dod_cols   <- paste0("dod_cmc",   1:n_births)
  dod_I_cols <- paste0("dod_cmc_I", 1:n_births)

  for (row in seq_len(nrow(df))) {
    cmcs <- as.integer(df[row, cmc_cols])
    if (all(is.na(cmcs))) next
    ord <- order(cmcs, na.last = TRUE)
    if (identical(ord, seq_len(n_births))) next

    df[row, cmc_cols] <- df[row, cmc_cols[ord]]
    if (all(I_cols     %in% names(df))) df[row, I_cols]     <- df[row, I_cols[ord]]
    if (all(sex_cols   %in% names(df))) df[row, sex_cols]   <- df[row, sex_cols[ord]]
    if (all(dod_cols   %in% names(df))) df[row, dod_cols]   <- df[row, dod_cols[ord]]
    if (all(dod_I_cols %in% names(df))) df[row, dod_I_cols] <- df[row, dod_I_cols[ord]]
  }
  df
}


# val_compute_lastYear ======
val_compute_lastYear <- function(df) {
  df |>
    dplyr::group_by(country, survey) |>
    dplyr::mutate(
      lastYear = min(1900L + floor((surveyDate_cmc - 1L) / 12L) - 1L, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}


# val_clean ======
val_clean <- function(df, checkOnly = FALSE) {
  nStart <- nrow(df)
  bad    <- rep(FALSE, nrow(df))

  flag_after_survey <- function(var) {
    if (!(var %in% names(df))) return(rep(FALSE, nrow(df)))
    v <- df[[var]]
    v[is.na(v)] <- 0L
    v > df$surveyDate_cmc
  }

  if ("indiv_dob_cmc" %in% names(df))
    bad <- bad | (!is.na(df$indiv_dob_cmc) & df$indiv_dob_cmc > df$surveyDate_cmc)

  n_unions_present <- sum(paste0("union_start_cmc", 1:MAX_UNIONS) %in% names(df))
  for (u in seq_len(n_unions_present)) {
    sc  <- paste0("union_start_cmc",    u)
    ec  <- paste0("union_end_cmc",      u)
    msc <- paste0("marriage_start_cmc", u)
    if (sc  %in% names(df)) bad <- bad | flag_after_survey(sc)
    if (ec  %in% names(df)) bad <- bad | flag_after_survey(ec)
    if (msc %in% names(df)) bad <- bad | flag_after_survey(msc)
    if (sc %in% names(df) & ec %in% names(df)) {
      bad <- bad | (!is.na(df[[ec]]) & !is.na(df[[sc]]) & df[[ec]] < df[[sc]])
    }
    if (u > 1) {
      ec_prev <- paste0("union_end_cmc", u - 1L)
      if (sc %in% names(df) & ec_prev %in% names(df))
        bad <- bad | (!is.na(df[[sc]]) & !is.na(df[[ec_prev]]) & df[[sc]] < df[[ec_prev]])
    }
  }

  maxB <- max(df$nBioKids, na.rm = TRUE)
  for (b in seq_len(maxB)) {
    bad <- bad | flag_after_survey(paste0("dob_cmc", b))
    dob_col <- paste0("dob_cmc", b)
    dod_col <- paste0("dod_cmc", b)
    if (dob_col %in% names(df) & dod_col %in% names(df))
      bad <- bad | (!is.na(df[[dob_col]]) & !is.na(df[[dod_col]]) & df[[dod_col]] < df[[dob_col]])
    bad <- bad | flag_after_survey(dod_col)
  }

  if (sum(bad) > 0) cat(sum(bad), "individuals flagged during cleaning\n")
  if (!checkOnly) df <- df[!bad, ]
  df$indiv_weight[is.na(df$indiv_weight)] <- 1L
  cat(nStart - nrow(df), "individuals removed of", nStart, "\n")
  df
}


# val_check_bind ======
val_check_bind <- function(...) {
  dfs   <- list(...)
  types <- lapply(dfs, function(d) sapply(d, class))
  all_cols <- unique(unlist(lapply(dfs, names)))
  issues <- character(0)
  for (col in all_cols) {
    present  <- sapply(types, function(t) col %in% names(t))
    if (sum(present) < 2) next
    col_types <- unique(unlist(lapply(types[present], `[[`, col)))
    if (length(col_types) > 1)
      issues <- c(issues, paste0(col, ": ", paste(col_types, collapse = " vs ")))
  }
  if (length(issues) == 0) cat("No column type conflicts found.\n")
  else { cat("Column type conflicts:\n"); cat(paste0("  ", issues, "\n"), sep = "") }
  invisible(issues)
}


# Union-status mapping for ENADID ======
# Maps raw ENADID union_status integer codes to the harmonised factor.
# Returns a factor with levels UNION_STATUS_LEVELS / UNION_STATUS_LABELS.

# 1997 (codes 1-11):
#   1=cohab, 2=sep-cohab, 3=sep-marr, 4-5=divorced, 6=wid-cohab,
#   7=wid-marr, 8-10=married, 11=single
val_union_status_1997 <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(
    x %in% 8:10  ~ 1L,  # married
    x == 1L      ~ 2L,  # cohabiting
    x %in% 2:3   ~ 3L,  # separated
    x %in% 4:5   ~ 4L,  # divorced
    x %in% 6:7   ~ 5L,  # widowed
    x == 11L     ~ 6L,  # single
    TRUE         ~ 7L   # unknown
  )
  factor(r, levels = UNION_STATUS_LEVELS, labels = UNION_STATUS_LABELS)
}

# 2006-2018 (codes 1-8):
#   1=cohab, 2=sep-cohab, 3=sep-marr, 4=divorced, 5=wid-cohab,
#   6=wid-marr, 7=married, 8=single
val_union_status_2006 <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(
    x == 7L      ~ 1L,  # married
    x == 1L      ~ 2L,  # cohabiting
    x %in% 2:3  ~ 3L,  # separated
    x == 4L      ~ 4L,  # divorced
    x %in% 5:6  ~ 5L,  # widowed
    x == 8L      ~ 6L,  # single
    TRUE         ~ 7L   # unknown
  )
  factor(r, levels = UNION_STATUS_LEVELS, labels = UNION_STATUS_LABELS)
}

# 2009 (codes 0-9, where 0="ever united" which is ambiguous):
#   0=ever united, 1=cohab, 2=sep-cohab, 3=sep-marr, 4=divorced,
#   5=wid-cohab, 6=wid-marr, 7=married, 8=single, 9=don't know
val_union_status_2009 <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(
    x == 7L      ~ 1L,  # married
    x == 1L      ~ 2L,  # cohabiting
    x %in% 2:3  ~ 3L,  # separated
    x == 4L      ~ 4L,  # divorced
    x %in% 5:6  ~ 5L,  # widowed
    x == 8L      ~ 6L,  # single
    TRUE         ~ 7L   # unknown (codes 0 and 9)
  )
  factor(r, levels = UNION_STATUS_LEVELS, labels = UNION_STATUS_LABELS)
}

# 2023 (codes 1-9, adds code 9=don't know vs 2006-2018's 8 levels):
#   1=cohab, 2=sep-cohab, 3=sep-marr, 4=divorced, 5=wid-cohab,
#   6=wid-marr, 7=married, 8=single, 9=don't know
val_union_status_2023 <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(
    x == 7L      ~ 1L,
    x == 1L      ~ 2L,
    x %in% 2:3  ~ 3L,
    x == 4L      ~ 4L,
    x %in% 5:6  ~ 5L,
    x == 8L      ~ 6L,
    TRUE         ~ 7L
  )
  factor(r, levels = UNION_STATUS_LEVELS, labels = UNION_STATUS_LABELS)
}


# val_union_end_motive_enadid ======
# Maps ENADID end-motive codes to the shared schema.
# ENADID codes: 1=separation, 2=widowhood, 3=divorce/separation, 9=don't know
# Schema codes: 0=in union, 1=widowhood, 2=separation, 9=unknown
val_union_end_motive_enadid <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(
    x == 2L ~ 1L,   # widowhood
    x %in% c(1L, 3L) ~ 2L,  # separation or divorce → separation
    x == 9L ~ 9L,   # unknown
    TRUE    ~ NA_integer_
  )
  factor(r, levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)
}

# val_union_end_motive_from_status ======
# Infer union end motive for the last union from union_status when the union
# has ended (woman is separated/divorced/widowed).
val_union_end_motive_from_status <- function(union_status_int) {
  r <- dplyr::case_when(
    union_status_int %in% c(5L, 6L, 7L) ~ 5L,  # widowed (cohab or marr) → widowhood
    union_status_int %in% c(2L, 3L)     ~ 4L,  # separated
    union_status_int == 4L              ~ 4L,  # divorced → separation
    TRUE                                ~ NA_integer_
  )
  # Translate to schema: widowhood=1, separation=2, unknown=9
  r2 <- dplyr::case_when(
    r == 5L ~ 1L,
    r == 4L ~ 2L,
    TRUE    ~ NA_integer_
  )
  factor(r2, levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)
}

# val_pregnant_enadid ======
# Map ENADID pregnant codes to schema codes (1=yes, 2=no, 8=refused, 9=unknown).
# 1992 uses codes 5=yes, 6=no, 8/9=dk; other cycles use 1=yes, 2=no, 9=dk.
val_pregnant_1992 <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(x == 5L ~ 1L, x == 6L ~ 2L, TRUE ~ 9L)
  factor(r, levels = PREGNANT_LEVELS, labels = PREGNANT_LABELS)
}

val_pregnant_std <- function(v) {
  x <- as.integer(v)
  r <- dplyr::case_when(x == 1L ~ 1L, x == 2L ~ 2L, x == 8L ~ 8L, TRUE ~ 9L)
  factor(r, levels = PREGNANT_LEVELS, labels = PREGNANT_LABELS)
}

# val_want_another_enadid ======
# Build want_another from mother/nullipar columns.
# ENADID 1997 codes: 5=yes, 6=no, 8/9=dk → schema 1/2/9
# ENADID 2006+ codes: 1=yes, 2=yes-but-can't, 3=no, 9=dk → schema 1/3/2/9
val_want_another_1997 <- function(mother_v, nullipar_v) {
  m <- as.integer(mother_v)
  n <- as.integer(nullipar_v)
  v <- dplyr::coalesce(m, n)
  r <- dplyr::case_when(
    v == 5L ~ 1L,
    v == 6L ~ 2L,
    TRUE    ~ 9L
  )
  factor(r, levels = WANT_ANOTHER_LEVELS, labels = WANT_ANOTHER_LABELS)
}

val_want_another_std <- function(mother_v, nullipar_v) {
  m <- as.integer(mother_v)
  n <- as.integer(nullipar_v)
  v <- dplyr::coalesce(m, n)
  r <- dplyr::case_when(
    v == 1L ~ 1L,   # yes
    v == 3L ~ 2L,   # no
    v == 2L ~ 3L,   # yes but can't → disagree
    v == 8L ~ 8L,   # refused
    v == 9L ~ 9L,   # unknown
    TRUE    ~ NA_integer_
  )
  factor(r, levels = WANT_ANOTHER_LEVELS, labels = WANT_ANOTHER_LABELS)
}
