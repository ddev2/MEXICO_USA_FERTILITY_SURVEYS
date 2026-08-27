# NSFG_val_compare.R ======
# Row-by-row comparison of original vs validation reimplementation.
#
# Usage:
#   1. Load both datasets into R:
#        load("path/to/NSFG_ENADID_1973.Rdat")   # original: NSFG_ENADID_1973
#        source("NSFG_val_import.R")              # validation functions
#   2. Run: compare_cycles(NSFG_ENADID_1973, NSFG_VAL_1973, "1973")

library(tidyverse)


# compare_cycles ======
# Main comparison function.
# Returns a list with a summary tibble and per-column discrepancy details.

compare_cycles <- function(orig, val, cycle_label = "") {

  cat("\n====", cycle_label, "====\n")

  # Match on CaseID =====
  common_ids <- intersect(orig$CaseID, val$CaseID)
  only_orig  <- setdiff(orig$CaseID, val$CaseID)
  only_val   <- setdiff(val$CaseID,  orig$CaseID)

  cat("Cases in original only:   ", length(only_orig),  "\n")
  cat("Cases in validation only: ", length(only_val),   "\n")
  cat("Cases in both:            ", length(common_ids), "\n")

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
        n_disagree    = n_disagree,
        pct_disagree  = round(100 * n_disagree / length(common_ids), 2),
        n_na_mismatch = sum(one_na,  na.rm = TRUE),
        n_value_diff  = sum(differ,  na.rm = TRUE),
        n_imputed_skipped = sum(month_imputed, na.rm = TRUE),
        examples      = dplyr::tibble(
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

  orig2 <- orig |> dplyr::arrange(CaseID)
  val2  <- val  |> dplyr::arrange(CaseID)

  for (u in 1:n_unions) {
    sc_o  <- orig2[[paste0("union_start_cmc",    u)]]
    sc_v  <- val2 [[paste0("union_start_cmc",    u)]]
    sc_Io <- orig2[[paste0("union_start_cmc_I",  u)]]
    sc_Iv <- val2 [[paste0("union_start_cmc_I",  u)]]
    em_o  <- as.character(orig2[[paste0("union_end_motive", u)]])
    em_v  <- as.character(val2 [[paste0("union_end_motive", u)]])

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

  orig2 <- orig |> dplyr::arrange(CaseID)
  val2  <- val  |> dplyr::arrange(CaseID)

  nk_o <- orig2$nBioKids
  nk_v <- val2$nBioKids
  cat("nBioKids agree:", sum(nk_o == nk_v, na.rm = TRUE), "/", nrow(orig2), "\n")

  for (b in 1:n_births) {
    dob_col <- paste0("dob_cmc", b)
    dod_col <- paste0("dod_cmc", b)

    # dob_cmc =====
    dob_I_col <- paste0("dob_cmc_I", b)
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
    dod_I_col <- paste0("dod_cmc_I", b)
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
# Uncomment when both datasets are loaded:

# load("NSFG_ENADID_1973.Rdat")
# source("NSFG_val_import.R")
# NSFG_VAL_1973 <- getDatos_1973_val()
# NSFG_VAL_1973 <- val_clean(NSFG_VAL_1973)
# NSFG_VAL_1973 <- val_reorder_births(NSFG_VAL_1973, max(NSFG_VAL_1973$nBioKids))
#
res <- compare_cycles(NSFG_ENADID_1973, NSFG_VAL_1973, "1973")
compare_union_histories(NSFG_ENADID_1973, NSFG_VAL_1973, n_unions = 5,  cycle_label = "1973")
compare_birth_histories(NSFG_ENADID_1973, NSFG_VAL_1973, n_births = 10, cycle_label = "1973")
#
# # Inspect specific discrepancy details:
# res$details$union_start_cmc1$examples
# res$details$dod_cmc1$examples

res <- compare_cycles(NSFG_ENADID_1976, NSFG_VAL_1976, "1976")
res <- compare_cycles(NSFG_ENADID_2022_23, NSFG_VAL_2022_23, "2022-23")
