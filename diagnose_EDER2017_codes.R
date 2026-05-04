setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.r")
# Diagnostic: EDER 2017 union code sequence validation
# Run this after loading EDER (the historiavida data.frame) to check
# for incoherence in the succession of edo_civil codes before attempting
# any imputation.
library(tidyverse)
library(data.table)

# Code classification
# NOTE: code 0 = single / never in union (state). Excluded from all checks
# by the `code > 0` filter below. It is not a union event and carries no
# timing information.

TRANS_COHAB   <- c(1L)                        # transition: cohabitation starts
TRANS_MARR    <- c(2L, 3L, 4L)               # transition: marriage starts
TRANS_DISS    <- c(6L, 7L, 8L)               # transition: dissolution starts
TRANS_COMP_2  <- c(12L, 13L, 14L, 17L, 18L,       # compound: two events same year
                   26L, 27L, 28L, 37L,
                   46L, 47L, 48L)
TRANS_COMP_3  <- c(126L)                      # compound: three events same year
TRANS_ALL     <- c(TRANS_COHAB, TRANS_MARR, TRANS_DISS, TRANS_COMP_2, TRANS_COMP_3)

STATE_COHAB   <- c(10L)                       # state: in cohabitation
STATE_MARR    <- c(20L, 30L, 40L)            # state: in marriage
STATE_DISS    <- c(60L, 70L, 80L)            # state: dissolved
STATE_ALL     <- c(STATE_COHAB, STATE_MARR, STATE_DISS)

# Expected first state code following each transition code
EXPECTED_STATE <- list(
  "1"   = c(10L),
  "2"   = c(20L),
  "3"   = c(30L),
  "4"   = c(40L),
  "6"   = c(60L),
  "7"   = c(70L),
  "8"   = c(80L),
  "12"  = c(30L),        # cohab -> religious marriage
  "13"  = c(40L),        # cohab -> civil+religious marriage
  "14"  = c(20L),        # cohab -> civil marriage
  "17"  = c(70L),        # cohab -> separation
  "18"  = c(80L),        # cohab -> widowhood
  "26"  = c(60L),        # civil marriage -> divorce
  "27"  = c(70L),        # civil marriage -> separation
  "28"  = c(80L),        # civil marriage -> widowhood
  "37"  = c(70L),        # religious marriage -> separation
  "46"  = c(60L),        # civil+religious -> divorce
  "47"  = c(70L),        # civil+religious -> separation
  "48"  = c(80L),        # civil+religious -> widowhood
  "126" = c(60L)         # cohab -> civil marriage -> divorce
)


# ==== Diagnostic function ====

diagnose_union_codes <- function(EDER, n_unions = 5) {
  cat("\n========== UNION CODE SEQUENCE DIAGNOSTICS ==========\n")
  
  for (u in seq_len(n_unions)) {
    col <- paste0("edo_civil", u)
    cat(sprintf("\n--- Union slot %d ---\n", u))
    
    # Get one row per woman (codes are in long format by anio_retro)
    # We need the sequence of codes over time for each woman
    dt <- as.data.table(EDER)[, .(llave_muj, anio_retro, code = get(col))]
    dt <- dt[!is.na(code) & code > 0L][order(llave_muj, anio_retro)]
    
    if (nrow(dt) == 0) {
      cat("  No active codes found.\n")
      next
    }
    
    # --- Check 1: unknown codes ---
    all_known <- c(TRANS_ALL, STATE_ALL)
    unknown   <- dt[!(code %in% all_known), unique(code)]
    if (length(unknown) > 0) {
      cat(sprintf("  Unknown codes found: %s\n", paste(sort(unknown), collapse = ", ")))
    } else {
      cat("  All codes are known.\n")
    }
    
    # --- Check 2: state code not preceded by corresponding transition ---
    # For each woman, find the first active code. If it is a state code,
    # there was no transition code recorded -- the union was already ongoing
    # at the start of observation.
    first_codes <- dt[, .(first_code = code[1L]), by = llave_muj]
    
    n_state_first <- sum(first_codes$first_code %in% STATE_ALL)
    n_cohab_first <- sum(first_codes$first_code %in% STATE_COHAB)
    n_marr_first  <- sum(first_codes$first_code %in% STATE_MARR)
    n_diss_first  <- sum(first_codes$first_code %in% STATE_DISS)
    
    cat(sprintf("  Women whose first code is a STATE (no transition observed): %d\n",
                n_state_first))
    if (n_state_first > 0) {
      cat(sprintf("    of which: cohabitation state (10): %d\n",        n_cohab_first))
      cat(sprintf("              marriage state (20/30/40): %d\n",      n_marr_first))
      cat(sprintf("              dissolution state (60/70/80): %d\n",   n_diss_first))
    }
    
    # --- Check 3: transition code followed by wrong state code ---
    # Build pairs: (transition_code, next_code) for each woman
    dt_pairs <- dt[, {
      n <- .N
      if (n >= 2L) {
        list(
          trans_code = code[seq_len(n - 1L)],
          next_code  = code[seq(2L, n)]
        )
      } else {
        list(trans_code = integer(0), next_code = integer(0))
      }
    }, by = llave_muj]
    
    # Keep only pairs where the first is a transition code
    dt_pairs <- dt_pairs[trans_code %in% TRANS_ALL]
    
    wrong_state <- dt_pairs[mapply(function(tc, nc) {
      expected <- EXPECTED_STATE[[as.character(tc)]]
      !is.null(expected) && !(nc %in% c(expected, TRANS_ALL))
    }, trans_code, next_code)]
    
    if (nrow(wrong_state) > 0) {
      cat(sprintf("  Transition -> unexpected next code: %d cases\n", nrow(wrong_state)))
      tbl <- wrong_state[, .N, by = .(trans_code, next_code)][order(-N)]
      for (r in seq_len(min(nrow(tbl), 10L))) {
        cat(sprintf("    code %3d -> code %3d : %d women\n",
                    tbl$trans_code[r], tbl$next_code[r], tbl$N[r]))
      }
    } else {
      cat("  All transition -> next-code pairs are coherent.\n")
    }
    
    # --- Check 4: state code followed by incoherent next code ---
    # A dissolution state (60/70/80) should not be followed by a union state
    dt_state_pairs <- dt[, {
      n <- .N
      if (n >= 2L) {
        list(
          state_code = code[seq_len(n - 1L)],
          next_code  = code[seq(2L, n)]
        )
      } else {
        list(state_code = integer(0), next_code = integer(0))
      }
    }, by = llave_muj]
    
    # Dissolution state followed by in-union code (should be impossible within same union slot)
    incoherent <- dt_state_pairs[
      state_code %in% STATE_DISS &
        next_code  %in% c(STATE_COHAB, STATE_MARR, TRANS_COHAB, TRANS_MARR)
    ]
    if (nrow(incoherent) > 0) {
      cat(sprintf("  Dissolution state followed by in-union code: %d cases\n",
                  nrow(incoherent)))
      tbl <- incoherent[, .N, by = .(state_code, next_code)][order(-N)]
      for (r in seq_len(min(nrow(tbl), 10L))) {
        cat(sprintf("    code %3d -> code %3d : %d women\n",
                    tbl$state_code[r], tbl$next_code[r], tbl$N[r]))
      }
    } else {
      cat("  No dissolution state followed by in-union code.\n")
    }
    
    # --- Check 5: frequency table of first transition codes ---
    first_trans <- first_codes[first_code %in% TRANS_ALL, .N, by = first_code][order(-N)]
    if (nrow(first_trans) > 0) {
      cat("  First transition code frequencies:\n")
      for (r in seq_len(nrow(first_trans))) {
        cat(sprintf("    code %3d : %d\n", first_trans$first_code[r], first_trans$N[r]))
      }
    }
  }
  
  cat("\n=====================================================\n")
}

diagnose_union_codes(EDER, n_unions = 5)

# ==== Check 6: compound codes 12/13/14 context (run separately) ====

# Determines whether codes 12/13/14 always encode Ustart+Mstart in the same
# year (no prior code 1), or can also appear after a prior code 1 in an
# earlier year (making them a Mstart-only event in that later year).

diagnose_compound_marr_codes <- function(EDER, n_unions = 5) {
  comp_marr_codes <- c(12L, 13L, 14L)
  cat("\n========== COMPOUND MARRIAGE CODES 12/13/14 ==========\n")
  
  for (u in seq_len(n_unions)) {
    col <- paste0("edo_civil", u)
    dt  <- as.data.table(EDER)[, .(llave_muj, anio_retro, code = get(col))]
    dt  <- dt[!is.na(code) & code > 0L][order(llave_muj, anio_retro)]
    
    any_found <- FALSE
    for (comp_code in comp_marr_codes) {
      women_this <- dt[code == comp_code, unique(llave_muj)]
      if (length(women_this) == 0) next
      any_found <- TRUE
      
      result <- dt[llave_muj %in% women_this, {
        comp_yr <- anio_retro[code == comp_code][1L]
        .(has_prior_cohab  = any(code == 1L & anio_retro < comp_yr),
          has_same_yr_cohab = any(code == 1L & anio_retro == comp_yr))
      }, by = llave_muj]
      
      n_prior   <- sum( result$has_prior_cohab)
      n_same    <- sum( result$has_same_yr_cohab)
      n_neither <- sum(!result$has_prior_cohab & !result$has_same_yr_cohab)
      
      cat(sprintf("\nUnion slot %d, code %d (%d women):\n", u, comp_code, length(women_this)))
      cat(sprintf("  Prior code 1 in earlier year => Mstart-only event : %d\n", n_prior))
      cat(sprintf("  Code 1 in same year          => true compound      : %d\n", n_same))
      cat(sprintf("  No code 1 at all             => compound encodes both: %d\n", n_neither))
    }
    if (!any_found) cat(sprintf("\nUnion slot %d: no codes 12/13/14 found.\n", u))
  }
  cat("\n=====================================================\n")
}

diagnose_compound_marr_codes(EDER, n_unions = 5)

diagnose_dissolution_conflicts <- function(EDER, n_unions = 5) {
  SEP_CODES <- c(7L, 17L, 27L, 37L, 47L, 70L)
  DIV_CODES <- c(6L, 26L, 46L, 60L, 126L)
  WID_CODES <- c(8L, 18L, 28L, 48L, 80L)
  
  cat("\n========== DISSOLUTION CONFLICT DIAGNOSTICS ==========\n")
  
  for (u in seq_len(n_unions)) {
    col <- paste0("edo_civil", u)
    dt  <- as.data.table(EDER)[, .(llave_muj, anio_retro, code = get(col))]
    dt  <- dt[!is.na(code) & code > 0L][order(llave_muj, anio_retro)]
    if (nrow(dt) == 0) next
    
    diss <- dt[, .(
      has_sep = any(code %in% SEP_CODES),
      has_div = any(code %in% DIV_CODES),
      has_wid = any(code %in% WID_CODES),
      yr_sep  = if (any(code %in% SEP_CODES)) min(anio_retro[code %in% SEP_CODES]) else NA_integer_,
      yr_div  = if (any(code %in% DIV_CODES)) min(anio_retro[code %in% DIV_CODES]) else NA_integer_,
      yr_wid  = if (any(code %in% WID_CODES)) min(anio_retro[code %in% WID_CODES]) else NA_integer_
    ), by = llave_muj]
    
    n_sep_and_div <- sum(diss$has_sep & diss$has_div)
    n_sep_and_wid <- sum(diss$has_sep & diss$has_wid)
    n_div_and_wid <- sum(diss$has_div & diss$has_wid)
    n_all_three   <- sum(diss$has_sep & diss$has_div & diss$has_wid)
    
    cat(sprintf("\n--- Union slot %d ---\n", u))
    cat(sprintf("  Separation AND divorce:           %d\n", n_sep_and_div))
    cat(sprintf("  Separation AND widowhood:         %d\n", n_sep_and_wid))
    cat(sprintf("  Divorce AND widowhood:            %d\n", n_div_and_wid))
    cat(sprintf("  All three:                        %d\n", n_all_three))
    
    # For sep+div conflicts, show year ordering
    if (n_sep_and_div > 0) {
      conflict <- diss[has_sep == TRUE & has_div == TRUE]
      n_sep_first <- sum(conflict$yr_sep <= conflict$yr_div, na.rm = TRUE)
      n_div_first <- sum(conflict$yr_div <  conflict$yr_sep, na.rm = TRUE)
      cat(sprintf("    Sep+Div: separation year <= divorce year: %d\n", n_sep_first))
      cat(sprintf("    Sep+Div: divorce year < separation year:  %d\n", n_div_first))
    }
    
    # For sep+wid conflicts
    if (n_sep_and_wid > 0) {
      conflict <- diss[has_sep == TRUE & has_wid == TRUE]
      n_sep_first <- sum(conflict$yr_sep <= conflict$yr_wid, na.rm = TRUE)
      n_wid_first <- sum(conflict$yr_wid <  conflict$yr_sep, na.rm = TRUE)
      cat(sprintf("    Sep+Wid: separation year <= widowhood year: %d\n", n_sep_first))
      cat(sprintf("    Sep+Wid: widowhood year < separation year:  %d\n", n_wid_first))
    }
  }
  cat("\n=====================================================\n")
}

diagnose_dissolution_conflicts(EDER, n_unions = 5)
