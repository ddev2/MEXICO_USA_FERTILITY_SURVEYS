# ENADID_val_compare.R ======
# Row-by-row comparison of original vs validation reimplementation.
#
# Usage:
#   1. Load both datasets into R:
#        load("path/to/ENADID_1997.Rdat")      # original: e.g. ENADID1997_full
#        source("ENADID_val_import.R")          # validation functions
#   2. Run: compare_cycles(ENADID1997_full, getDatos_ENADID1997_val(), "1997")
#
# Note: CaseID is character (composite key) in all ENADID validation cycles.
#       Original data uses 'llave_muj' — normalise_ids() handles the rename and
#       any leading-zero difference (integer64 originals vs character validation).
#
# Cycles available: 1992, 1997, 2006, 2009, 2014, 2018, 2023.

library(tidyverse)
library(bit64)


# normalise_ids ======
# Internal helper used by every comparison function.
# Renames 'llave_muj' → 'CaseID' and reconciles key format:
#   - integer64 original  → strip leading zeros from both sides
#   - character original  → keep as-is (1992/2006/2009/2014)

normalise_ids <- function(orig, val) {
  # Rename llave_muj → CaseID in original
  if (!"CaseID" %in% names(orig) && "llave_muj" %in% names(orig)) {
    names(orig)[names(orig) == "llave_muj"] <- "CaseID"
  }

  if ("CaseID" %in% names(orig)) {
    if (bit64::is.integer64(orig$CaseID)) {
      # integer64: no leading zeros.  Strip leading zeros from validation key too.
      orig$CaseID <- as.character(orig$CaseID)
      val$CaseID  <- as.character(bit64::as.integer64(as.character(val$CaseID)))
    } else {
      orig$CaseID <- as.character(orig$CaseID)
      val$CaseID  <- as.character(val$CaseID)
    }
  }

  list(orig = orig, val = val)
}


# compare_cycles ======
# Main comparison function.
# Returns a list with a summary tibble and per-column discrepancy details.

compare_cycles <- function(orig, val, cycle_label = "") {

  cat("\n====", cycle_label, "====\n")

  # Normalise ID column name and format
  ids   <- normalise_ids(orig, val)
  orig  <- ids$orig
  val   <- ids$val

  # Also run val_clean on validation output before comparing, if not done yet.
  # (val_clean removes impossible dates and logs removed rows)

  # Match on CaseID =====
  common_ids <- intersect(orig$CaseID, val$CaseID)
  only_orig  <- setdiff(orig$CaseID, val$CaseID)
  only_val   <- setdiff(val$CaseID,  orig$CaseID)

  cat("Cases in original only:   ", length(only_orig),  "\n")
  cat("Cases in validation only: ", length(only_val),   "\n")
  cat("Cases in both:            ", length(common_ids), "\n")

  if (length(common_ids) == 0) {
    cat("WARNING: no common IDs found — check key format.\n")
    return(invisible(list(summary = NULL, details = NULL)))
  }

  orig2 <- orig[orig$CaseID %in% common_ids, ] |> dplyr::arrange(CaseID)
  val2  <- val [val$CaseID  %in% common_ids, ] |> dplyr::arrange(CaseID)

  # Common columns =====
  common_cols    <- intersect(names(orig2), names(val2))
  only_orig_cols <- setdiff(names(orig2), names(val2))
  only_val_cols  <- setdiff(names(val2),  names(orig2))

  if (length(only_orig_cols) > 0)
    cat("Columns only in original:  ", paste(only_orig_cols, collapse = ", "), "\n")
  if (length(only_val_cols) > 0)
    cat("Columns only in validation:", paste(only_val_cols,  collapse = ", "), "\n")

  # Column-by-column comparison =====
  results <- list()

  # Helper: find companion imputation-flag column for a CMC column.
  # e.g. "dob_cmc1" -> "dob_cmc_I1", "union_start_cmc1" -> "union_start_cmc_I1".
  # Returns NULL when col is itself an _I column or has no _cmc substring.
  flag_companion <- function(col) {
    if (grepl("_cmc_I", col) || !grepl("_cmc", col)) return(NULL)
    gsub("_cmc", "_cmc_I", col)
  }

  for (col in common_cols) {
    o <- orig2[[col]]
    v <- val2[[col]]

    # Harmonise factors to character for comparison
    if (is.factor(o)) o <- as.character(o)
    if (is.factor(v)) v <- as.character(v)

    # Rows where either implementation imputed a random month (flag == 1):
    # these will differ by construction and should not count as discrepancies.
    fc <- flag_companion(col)
    month_imputed <- if (!is.null(fc) && fc %in% names(orig2) && fc %in% names(val2)) {
      fi_o <- orig2[[fc]]
      fi_v <- val2[[fc]]
      (!is.na(fi_o) & fi_o == 1L) | (!is.na(fi_v) & fi_v == 1L)
    } else {
      rep(FALSE, length(o))
    }

    # NA-safe comparison, excluding imputed-month rows
    one_na   <- xor(is.na(o), is.na(v))          & !month_imputed
    both_val <- !is.na(o) & !is.na(v)             & !month_imputed
    differ   <- both_val & (o != v)

    n_disagree <- sum(one_na, na.rm = TRUE) + sum(differ, na.rm = TRUE)

    if (n_disagree > 0) {
      results[[col]] <- list(
        n_disagree        = n_disagree,
        pct_disagree      = round(100 * n_disagree / length(common_ids), 2),
        n_na_mismatch     = sum(one_na,  na.rm = TRUE),
        n_value_diff      = sum(differ,  na.rm = TRUE),
        n_imputed_skipped = sum(month_imputed, na.rm = TRUE),
        examples          = dplyr::tibble(
          CaseID     = orig2$CaseID[differ | one_na],
          original   = o[differ | one_na],
          validation = v[differ | one_na]
        ) |> head(10)
      )
    }
  }

  # Summary table =====
  if (length(results) == 0) {
    cat("All", length(common_cols), "common columns match perfectly.\n")
    summary_tbl <- dplyr::tibble(
      column = character(), n_disagree = integer(),
      pct_disagree = numeric(), na_mismatch = integer(), value_diff = integer()
    )
  } else {
    cat("\nColumns with discrepancies:\n")
    summary_tbl <- dplyr::tibble(
      column          = names(results),
      n_disagree      = sapply(results, `[[`, "n_disagree"),
      pct_disagree    = sapply(results, `[[`, "pct_disagree"),
      na_mismatch     = sapply(results, `[[`, "n_na_mismatch"),
      value_diff      = sapply(results, `[[`, "n_value_diff"),
      imputed_skipped = sapply(results, `[[`, "n_imputed_skipped")
    ) |> dplyr::arrange(dplyr::desc(n_disagree))
    print(summary_tbl, n = 50)
  }

  invisible(list(summary = summary_tbl, details = results))
}


# compare_union_histories ======
# Focused check on union timing and ordering.

compare_union_histories <- function(orig, val, n_unions = 3, cycle_label = "") {
  cat("\n=== Union history check:", cycle_label, "===\n")

  ids  <- normalise_ids(orig, val)
  orig <- ids$orig; val <- ids$val

  orig2 <- orig |> dplyr::arrange(CaseID)
  val2  <- val  |> dplyr::arrange(CaseID)

  for (u in 1:n_unions) {
    sc_col  <- paste0("union_start_cmc",   u)
    scI_col <- paste0("union_start_cmc_I", u)
    em_col  <- paste0("union_end_motive",  u)

    if (!(sc_col %in% names(orig2)) || !(sc_col %in% names(val2))) next

    sc_o  <- orig2[[sc_col]]
    sc_v  <- val2 [[sc_col]]
    sc_Io <- orig2[[scI_col]]
    sc_Iv <- val2 [[scI_col]]
    em_o  <- as.character(orig2[[em_col]])
    em_v  <- as.character(val2 [[em_col]])

    imp <- (!is.na(sc_Io) & sc_Io == 1L) | (!is.na(sc_Iv) & sc_Iv == 1L)
    n_sc_diff <- sum(!is.na(sc_o) & !is.na(sc_v) & sc_o != sc_v & !imp, na.rm = TRUE)
    n_sc_imp  <- sum(imp,                                                  na.rm = TRUE)
    n_em_diff <- sum(!is.na(em_o) & !is.na(em_v) & em_o != em_v,         na.rm = TRUE)
    n_sc_na   <- sum(xor(is.na(sc_o), is.na(sc_v)) & !imp,                na.rm = TRUE)

    cat(sprintf("Union %d  start_cmc: %d value diffs, %d NA mismatches, %d imputed-skipped | end_motive: %d diffs\n",
                u, n_sc_diff, n_sc_na, n_sc_imp, n_em_diff))
  }
}


# compare_birth_histories ======
# Focused check on birth timing and child death dates.

compare_birth_histories <- function(orig, val, n_births = 5, cycle_label = "") {
  cat("\n=== Birth history check:", cycle_label, "===\n")

  ids  <- normalise_ids(orig, val)
  orig <- ids$orig; val <- ids$val

  orig2 <- orig |> dplyr::arrange(CaseID)
  val2  <- val  |> dplyr::arrange(CaseID)

  nk_o <- orig2$nBioKids
  nk_v <- val2$nBioKids
  cat("nBioKids agree:", sum(nk_o == nk_v, na.rm = TRUE), "/", nrow(orig2), "\n")

  for (b in 1:n_births) {
    dob_col   <- paste0("dob_cmc",   b)
    dob_I_col <- paste0("dob_cmc_I", b)
    dod_col   <- paste0("dod_cmc",   b)
    dod_I_col <- paste0("dod_cmc_I", b)

    # dob_cmc =====
    if (dob_col %in% names(orig2) && dob_col %in% names(val2)) {
      o <- orig2[[dob_col]]; v <- val2[[dob_col]]
      imp <- if (dob_I_col %in% names(orig2) && dob_I_col %in% names(val2)) {
        fi_o <- orig2[[dob_I_col]]; fi_v <- val2[[dob_I_col]]
        (!is.na(fi_o) & fi_o == 1L) | (!is.na(fi_v) & fi_v == 1L)
      } else rep(FALSE, length(o))
      n_diff <- sum(!is.na(o) & !is.na(v) & o != v & !imp, na.rm = TRUE)
      n_na   <- sum(xor(is.na(o), is.na(v)) & !imp,         na.rm = TRUE)
      n_imp  <- sum(imp,                                     na.rm = TRUE)
      cat(sprintf("Birth %2d  dob_cmc: %d value diffs, %d NA mismatches, %d imputed-skipped\n",
                  b, n_diff, n_na, n_imp))
    }

    # dod_cmc =====
    if (dod_col %in% names(orig2) && dod_col %in% names(val2)) {
      o <- orig2[[dod_col]]; v <- val2[[dod_col]]
      imp <- if (dod_I_col %in% names(orig2) && dod_I_col %in% names(val2)) {
        fi_o <- orig2[[dod_I_col]]; fi_v <- val2[[dod_I_col]]
        (!is.na(fi_o) & fi_o == 1L) | (!is.na(fi_v) & fi_v == 1L)
      } else rep(FALSE, length(o))
      n_diff <- sum(!is.na(o) & !is.na(v) & o != v & !imp, na.rm = TRUE)
      n_na   <- sum(xor(is.na(o), is.na(v)) & !imp,         na.rm = TRUE)
      n_imp  <- sum(imp,                                     na.rm = TRUE)
      cat(sprintf("Birth %2d  dod_cmc: %d value diffs, %d NA mismatches, %d imputed-skipped\n",
                  b, n_diff, n_na, n_imp))
    }
  }
}


# Example usage ======
# Uncomment when both datasets are loaded.
# Replace ENADID_XXXX_full with whatever name the original data object uses.
#
# Note: the original pipeline saves objects named e.g. ENADID1997_full with
#       a 'llave_muj' key (integer64 for 1997/2018/2023, character for others).
#       normalise_ids() inside each function handles the rename and format
#       difference automatically.

# source("ENADID_val_import.R")

# --- 1992 ---
# ENADID_VAL_1992 <- getDatos_ENADID1992_val()
# ENADID_VAL_1992 <- val_clean(ENADID_VAL_1992)
# res <- compare_cycles(ENADID1992_full, ENADID_VAL_1992, "1992")
# compare_union_histories(ENADID1992_full, ENADID_VAL_1992, n_unions = 2,  cycle_label = "1992")
# compare_birth_histories(ENADID1992_full, ENADID_VAL_1992, n_births = 10, cycle_label = "1992")

# --- 1997 ---
# ENADID_VAL_1997 <- getDatos_ENADID1997_val()
# ENADID_VAL_1997 <- val_clean(ENADID_VAL_1997)
# res <- compare_cycles(ENADID1997_full, ENADID_VAL_1997, "1997")
# compare_union_histories(ENADID1997_full, ENADID_VAL_1997, n_unions = 2,  cycle_label = "1997")
# compare_birth_histories(ENADID1997_full, ENADID_VAL_1997, n_births = 10, cycle_label = "1997")

# --- 2006 ---
# ENADID_VAL_2006 <- getDatos_ENADID2006_val()
# ENADID_VAL_2006 <- val_clean(ENADID_VAL_2006)
# res <- compare_cycles(ENADID2006_full, ENADID_VAL_2006, "2006")
# compare_union_histories(ENADID2006_full, ENADID_VAL_2006, n_unions = 2,  cycle_label = "2006")
# compare_birth_histories(ENADID2006_full, ENADID_VAL_2006, n_births = 10, cycle_label = "2006")

# --- 2009 ---
# ENADID_VAL_2009 <- getDatos_ENADID2009_val()
# ENADID_VAL_2009 <- val_clean(ENADID_VAL_2009)
# res <- compare_cycles(ENADID2009_full, ENADID_VAL_2009, "2009")
# compare_union_histories(ENADID2009_full, ENADID_VAL_2009, n_unions = 2,  cycle_label = "2009")
# compare_birth_histories(ENADID2009_full, ENADID_VAL_2009, n_births = 10, cycle_label = "2009")

# --- 2014 ---
# ENADID_VAL_2014 <- getDatos_ENADID2014_val()
# ENADID_VAL_2014 <- val_clean(ENADID_VAL_2014)
# res <- compare_cycles(ENADID2014_full, ENADID_VAL_2014, "2014")
# compare_union_histories(ENADID2014_full, ENADID_VAL_2014, n_unions = 2,  cycle_label = "2014")
# compare_birth_histories(ENADID2014_full, ENADID_VAL_2014, n_births = 10, cycle_label = "2014")

# --- 2018 ---
# ENADID_VAL_2018 <- getDatos_ENADID2018_val()
# ENADID_VAL_2018 <- val_clean(ENADID_VAL_2018)
# res <- compare_cycles(ENADID2018_full, ENADID_VAL_2018, "2018")
# compare_union_histories(ENADID2018_full, ENADID_VAL_2018, n_unions = 2,  cycle_label = "2018")
# compare_birth_histories(ENADID2018_full, ENADID_VAL_2018, n_births = 10, cycle_label = "2018")

# --- 2023 ---
# ENADID_VAL_2023 <- getDatos_ENADID2023_val()
# ENADID_VAL_2023 <- val_clean(ENADID_VAL_2023)
# res <- compare_cycles(ENADID2023_full, ENADID_VAL_2023, "2023")
# compare_union_histories(ENADID2023_full, ENADID_VAL_2023, n_unions = 2,  cycle_label = "2023")
# compare_birth_histories(ENADID2023_full, ENADID_VAL_2023, n_births = 10, cycle_label = "2023")

# Inspect specific discrepancy details:
# res$details$union_start_cmc1$examples
# res$details$dob_cmc1$examples
# res$details$union_status$examples
