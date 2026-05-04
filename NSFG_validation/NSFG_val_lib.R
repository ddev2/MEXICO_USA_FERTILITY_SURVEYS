# NSFG_val_lib.R ======
# Shared helper functions for the NSFG validation reimplementation.
# Independent of NSFG_lib.R and enadid_lib.R — written from scratch
# for cross-checking purposes.

library(tidyverse)
library(haven)


# CMC helpers ======

# Convert calendar month + year to Century Month Code
# month: integer 1-12, or vector thereof; year: 4-digit integer year
val_cmc <- function(month, year) {
  month <- as.integer(month)
  year  <- as.integer(year)
  # years given as 2-digit: 01-98 -> 1901-1998, 99 -> missing
  year  <- dplyr::case_when(
    year == 99L              ~ NA_integer_,
    year >= 1L & year <= 98L ~ year + 1900L,
    TRUE                     ~ year
  )
  month <- dplyr::if_else(month >= 1L & month <= 12L, month, NA_integer_)
  dplyr::if_else(!is.na(year) & !is.na(month), (year - 1900L) * 12L + month, NA_integer_)
}

# Extract year from CMC
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

# Random month imputation (vectorised: one draw per row, never a scalar)
rand_month <- function(n) sample(1:12, n, replace = TRUE)


# Output schema ======
# Ensures every per-cycle dataframe has exactly the required columns,
# in the right order, with the right types, before bind_rows.

# Fixed columns (always present, independent of union/birth count) =====
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

# Column name generators =====
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

# Factor levels/labels used consistently across all cycles =====
UNION_START_TYPE_LEVELS <- c(1L, 2L, 3L)
UNION_START_TYPE_LABELS <- c("marriage", "cohabitation", "cohabitation before marriage")

UNION_END_MOTIVE_LEVELS <- c(0L, 1L, 2L, 9L)
UNION_END_MOTIVE_LABELS <- c("in union", "widowhood", "separation", "unknown")

PREGNANT_LEVELS <- c(1L, 2L, 8L, 9L)
PREGNANT_LABELS <- c("yes", "no", "refused", "unknown")

WANT_ANOTHER_LEVELS <- c(1L, 2L, 3L, 8L, 9L)
WANT_ANOTHER_LABELS <- c("yes", "no", "disagree", "refused", "unknown")

PROB_WANT_ANOTHER_LEVELS <- c(1L, 2L, 8L, 9L)
PROB_WANT_ANOTHER_LABELS <- c("probably yes", "probably no", "refused", "unknown")

# Add any missing schema columns as NA, drop extras, reorder =====
enforce_schema <- function(df) {
  missing_cols <- setdiff(SCHEMA_ALL, names(df))
  df[missing_cols] <- NA
  df[, intersect(SCHEMA_ALL, names(df))]
}


# Union history ordering ======
# After building all union slots, sort them by union_start_cmc per individual
# so union 1 is always the chronologically first union.
# Equivalent to order_union_history() in NSFG_lib.R.

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
    if (identical(ord, seq_len(n_unions))) next  # already sorted

    for (col_group in list(cmc_cols, cmc_I_cols, type_cols, marr_cols,
                           marr_I_cols, end_cols, end_I_cols, mot_cols)) {
      df[row, col_group] <- df[row, col_group[ord]]
    }
  }
  df
}


# Birth history ordering ======
# Sort births chronologically (birth 1 = earliest) and carry dod fields.
# Replicates reorder_birthHistory() from enadid_lib.R.

val_reorder_births <- function(df, n_births) {
  if (n_births <= 1L) return(df)

  cmc_cols <- paste0("dob_cmc",   1:n_births)
  I_cols   <- paste0("dob_cmc_I", 1:n_births)
  sex_cols <- paste0("sex",       1:n_births)
  dod_cols <- paste0("dod_cmc",   1:n_births)
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


# lastYear ======
# Minimum survey year within each country x survey cell, minus 1.
# Replicates compute_lastYear() from enadid_lib.R.

val_compute_lastYear <- function(df) {
  df |>
    dplyr::group_by(country, survey) |>
    dplyr::mutate(
      lastYear = min(1900L + floor((surveyDate_cmc - 1L) / 12L) - 1L, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}


# val_clean ======
# Remove logical inconsistencies; replicate cleanENADID() from enadid_lib.R.

val_clean <- function(df, checkOnly = FALSE) {
  df <- haven::zap_labels(df)
  nStart <- nrow(df)
  bad <- rep(FALSE, nrow(df))

  flag_after_survey <- function(var) {
    if (!(var %in% names(df))) return(rep(FALSE, nrow(df)))
    v <- df[[var]]
    v[is.na(v)] <- 0L
    v > df$surveyDate_cmc
  }

  # Respondent DOB check =====
  # Flag respondents whose birth CMC is after the interview date.
  if ("indiv_dob_cmc" %in% names(df)) {
    bad <- bad | (!is.na(df$indiv_dob_cmc) & df$indiv_dob_cmc > df$surveyDate_cmc)
  }

  # Union date checks =====
  # Loop over every union slot present in this dataset, not just 1:3.
  n_unions_present <- sum(paste0("union_start_cmc", 1:MAX_UNIONS) %in% names(df))
  for (u in seq_len(n_unions_present)) {
    sc  <- paste0("union_start_cmc",    u)
    ec  <- paste0("union_end_cmc",      u)
    msc <- paste0("marriage_start_cmc", u)
    if (sc  %in% names(df)) bad <- bad | flag_after_survey(sc)
    if (ec  %in% names(df)) bad <- bad | flag_after_survey(ec)
    if (msc %in% names(df)) bad <- bad | flag_after_survey(msc)
    # Union end before union start
    if (sc %in% names(df) & ec %in% names(df)) {
      v <- !is.na(df[[ec]]) & !is.na(df[[sc]]) & df[[ec]] < df[[sc]]
      bad <- bad | v
    }
    # Union N starts before union N-1 ends (overlapping unions)
    if (u > 1) {
      ec_prev <- paste0("union_end_cmc", u - 1L)
      if (sc %in% names(df) & ec_prev %in% names(df)) {
        v <- !is.na(df[[sc]]) & !is.na(df[[ec_prev]]) & df[[sc]] < df[[ec_prev]]
        bad <- bad | v
      }
    }
  }

  # Birth date checks =====
  maxB <- max(df$nBioKids, na.rm = TRUE)
  for (b in seq_len(maxB)) {
    # Birth after survey date
    bad <- bad | flag_after_survey(paste0("dob_cmc", b))
    # Death before birth or death after survey date
    dob_col <- paste0("dob_cmc", b)
    dod_col <- paste0("dod_cmc", b)
    if (dob_col %in% names(df) & dod_col %in% names(df)) {
      v <- !is.na(df[[dob_col]]) & !is.na(df[[dod_col]]) &
           df[[dod_col]] < df[[dob_col]]
      bad <- bad | v
    }
    bad <- bad | flag_after_survey(dod_col)
  }

  if (sum(bad) > 0)
    cat(sum(bad), "individuals flagged during cleaning\n")

  if (!checkOnly) df <- df[!bad, ]

  # Fix missing weights
  df$indiv_weight[is.na(df$indiv_weight)] <- 1L

  cat(nStart - nrow(df), "individuals removed of", nStart, "\n")
  df
}

# val_check_bind ======
# Check column type compatibility before binding cycles.
# Replicates check_bind_conflicts() from NSFG_import.R.

val_check_bind <- function(...) {
  dfs   <- list(...)
  nms   <- lapply(dfs, names)
  types <- lapply(dfs, function(d) sapply(d, class))
  all_cols <- unique(unlist(nms))
  issues <- character(0)
  for (col in all_cols) {
    present <- sapply(types, function(t) col %in% names(t))
    if (sum(present) < 2) next
    col_types <- unique(unlist(lapply(types[present], `[[`, col)))
    if (length(col_types) > 1)
      issues <- c(issues, paste0(col, ": ", paste(col_types, collapse = " vs ")))
  }
  if (length(issues) == 0) {
    cat("No column type conflicts found.\n")
  } else {
    cat("Column type conflicts:\n")
    cat(paste0("  ", issues, "\n"), sep = "")
  }
  invisible(issues)
}
