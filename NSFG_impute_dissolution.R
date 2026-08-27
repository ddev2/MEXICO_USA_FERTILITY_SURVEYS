# NSFG 2002 dissolution-date imputation -- building blocks
#
# Context (Step 1, already in `NSFG import.R`): per-husband prior-children flags
#   husb_priorKids_1 .. husb_priorKids_K   (= KIDSHX_i, indexed by marriage order)
# In the 2002 cycle the marriage-end module (MARENDHX + stop-living / death
# dates) was skipped whenever a husband had children from a prior relationship,
# so those marriages carry union_end_cmc = NA "by skip". Confirmed empirically:
# among remarried women, KIDSHX(husband i)=="yes" => MARENDHX(husband i)=NA in
# 100% of cases (119/119), vs 0% when "no" (0/568).
#
# This file holds Step 2 building blocks. Source it AFTER the combined frame is
# built, then run on NSFG_ENADID:
#   source("NSFG import.R")
#   source("NSFG_impute_dissolution.R")
#   NSFG_ENADID <- add_union_husb_priorKids(NSFG_ENADID)
#   tabulate_union_husb_priorKids(NSFG_ENADID)


# ==== Map per-husband flags onto chronological union slots ====
#
# husb_priorKids_i is indexed by MARRIAGE order (husband i). The cleaned union
# history stores slots in chronological order (after order_union_history), with
# non-marital cohabitations interleaved. We need each husband's flag attached to
# the union slot that holds his marriage.
#
# A slot is a MARRIAGE iff it has a marriage date: marriage_start_cmc{s} is
# populated for marriages (types "marriage" and "cohabitation before marriage")
# and NA for non-marital cohabitations. We rank a woman's marriage slots by that
# date -- NOT by slot position -- so the i-th-married husband maps to the i-th
# marriage even when a later husband's premarital cohabitation predates an
# earlier marriage (which would otherwise reorder the slots). The >=9000
# missing-year sentinel is treated as "no usable date".
#
# Adds one column per slot:
#   union_husb_priorKids{s}  factor yes/no/refused/don't know
#                            (NA when slot s is not a mappable marriage)
# A woman is only mapped when her count of dated marriage slots equals her count
# of husband flags; the rest are left unmapped (NA) as ambiguous, and reported.
add_union_husb_priorKids <- function(df) {
  husb_cols <- grep("^husb_priorKids_[0-9]+$", names(df), value = TRUE)
  ms_cols   <- grep("^marriage_start_cmc[0-9]+$", names(df), value = TRUE)
  if (length(husb_cols) == 0L || length(ms_cols) == 0L) {
    warning("add_union_husb_priorKids(): husb_priorKids_* or ",
            "marriage_start_cmc* columns missing; df returned unchanged")
    return(df)
  }
  husb_cols <- husb_cols[order(as.integer(sub("^husb_priorKids_", "", husb_cols)))]
  ms_cols   <- ms_cols[order(as.integer(sub("^marriage_start_cmc", "", ms_cols)))]
  ms_slot   <- as.integer(sub("^marriage_start_cmc", "", ms_cols))

  n    <- nrow(df)
  K    <- length(husb_cols)
  S    <- length(ms_cols)
  lvls <- c("yes", "no", "refused", "don't know")

  # husband-flag values as an n x K character matrix (gathered per row by rank)
  husb_chr <- vapply(husb_cols, function(cn) as.character(df[[cn]]), character(n))
  if (is.null(dim(husb_chr))) husb_chr <- matrix(husb_chr, nrow = n)

  # marriage-date matrix; NA marks "not a (dated) marriage slot"
  mstart <- vapply(ms_cols, function(cn) {
    v <- suppressWarnings(as.numeric(df[[cn]]))
    v[!is.na(v) & v >= 9000] <- NA_real_
    v
  }, numeric(n))
  if (is.null(dim(mstart))) mstart <- matrix(mstart, nrow = n)

  n_marr <- rowSums(!is.na(mstart))   # dated marriages per woman

  # husband order of marriage slot s = 1 + (# of this woman's marriage slots
  # with an earlier marriage date); ties broken by slot index for stability
  husb_order <- matrix(NA_integer_, n, S)
  for (s in seq_len(S)) {
    ms_s    <- mstart[, s]
    earlier <- integer(n)
    for (s2 in seq_len(S)) {
      if (s2 == s) next
      ms_2 <- mstart[, s2]
      earlier <- earlier + (!is.na(ms_2) & !is.na(ms_s) &
                            (ms_2 < ms_s | (ms_2 == ms_s & s2 < s)))
    }
    has <- !is.na(ms_s)
    husb_order[has, s] <- earlier[has] + 1L
  }

  # map only women whose #dated-marriages matches #husband-flags
  n_flag    <- rowSums(!is.na(husb_chr))
  flagged   <- n_flag > 0L
  ok        <- flagged & (n_marr == n_flag)
  ambiguous <- flagged & !ok

  for (s in seq_len(S)) {
    ho   <- husb_order[, s]
    val  <- rep(NA_character_, n)
    rows <- which(ok & !is.na(ho) & ho <= K)
    if (length(rows) > 0L) val[rows] <- husb_chr[cbind(rows, ho[rows])]
    df[[paste0("union_husb_priorKids", ms_slot[s])]] <- factor(val, levels = lvls)
  }

  message(sum(ok), " women mapped (husband -> marriage slot); ",
          sum(ambiguous), " flagged women left unmapped as ambiguous ",
          "(#dated-marriages != #husband-flags)")
  df
}


# ==== Sanity tabulation of the mapped slot flags ====
#
# Cross-check: the total count of each flag value spread across the slot columns
# should match the per-husband totals (each mapped husband lands on exactly one
# marriage slot). Also reports, per survey, marriage slots whose husband had
# prior children and whose union_end_cmc is missing -- the pool the imputation
# will draw from. NOTE: that pool still mixes genuinely-ended marriages with
# currently-intact ones (both have union_end_cmc = NA); separating them is the
# next step, not done here.
tabulate_union_husb_priorKids <- function(df) {
  flag_cols <- grep("^union_husb_priorKids[0-9]+$", names(df), value = TRUE)
  if (length(flag_cols) == 0L) stop("run add_union_husb_priorKids() first")
  slot <- as.integer(sub("^union_husb_priorKids", "", flag_cols))

  all_flags <- unlist(lapply(flag_cols, function(cn) as.character(df[[cn]])))
  cat("Flag value across all mapped marriage slots:\n")
  print(table(all_flags, useNA = "no"))

  n_na_end_yes <- integer(nrow(df))
  for (k in seq_along(flag_cols)) {
    ue <- paste0("union_end_cmc", slot[k])
    if (!ue %in% names(df)) next
    is_yes <- !is.na(df[[flag_cols[k]]]) & as.character(df[[flag_cols[k]]]) == "yes"
    n_na_end_yes <- n_na_end_yes + (is_yes & is.na(df[[ue]]))
  }
  cat("\nPrior-kids marriage slots with missing union_end_cmc, by survey",
      "(includes still-intact unions):\n")
  tab <- tapply(n_na_end_yes, df$survey, sum)
  print(tab[!is.na(tab) & tab > 0])
  invisible(n_na_end_yes)
}


# ==== Flag the currently-separated union per slot (issue 1) ====
#
# Currently-separated 2002 respondents (sep_recent == TRUE) had their marriage-
# end module skipped, so the separation is real but its date is missing -- the
# union reads as ongoing (union_end_cmc NA, motive "in union"). This marks the
# single union slot the separation applies to: the woman's CURRENT union = her
# most recent occupied slot whose union_end_cmc is NA. The slot is located from
# the union history (NOT from RMARITAL), and works whether that union is a
# marriage or a cohabitation.
#
# Adds sep_recent_{s} (logical) per union slot, TRUE on that one slot. Its
# union_end_cmc{s} is the issue-1 imputation target, bounded
# [union_start_cmc{s}, surveyDate_cmc]. Separated women with NO open slot (both
# ends already filled -- e.g. separated, then cohabited, then that cohab ended,
# so cleanENADID back-filled the marriage end) are reported; they fall to the
# placeholder re-imputation path instead.
#
# NOTE on RMARITAL: a pure union-history derivation cannot identify these women,
# because the 2002 skip removed the separation from the history itself (the
# current marriage looks intact, NA end). sep_recent (RMARITAL == 5) is the only
# surviving signal of the separation and is used solely to identify WHO; the
# history still locates WHICH slot. Once the dates are imputed the history is
# complete and current union status can be re-derived from it without RMARITAL.
add_union_sep_recent <- function(df) {
  if (!"sep_recent" %in% names(df)) {
    warning("add_union_sep_recent(): sep_recent column not found; df unchanged")
    return(df)
  }
  us_cols <- grep("^union_start_cmc[0-9]+$", names(df), value = TRUE)
  if (length(us_cols) == 0L) {
    warning("add_union_sep_recent(): no union_start_cmc* columns; df unchanged")
    return(df)
  }
  slot    <- as.integer(sub("^union_start_cmc", "", us_cols))
  o       <- order(slot); us_cols <- us_cols[o]; slot <- slot[o]
  ue_cols <- paste0("union_end_cmc", slot)

  n   <- nrow(df)
  S   <- length(us_cols)
  sep <- !is.na(df$sep_recent) & df$sep_recent

  # current separated union = most recent occupied union with no recorded end
  target <- rep(NA_integer_, n)               # slot number of the flagged union
  for (k in seq_len(S)) {
    has_start <- !is.na(suppressWarnings(as.numeric(df[[us_cols[k]]])))
    end_na    <- if (ue_cols[k] %in% names(df)) is.na(df[[ue_cols[k]]]) else rep(TRUE, n)
    target[sep & has_start & end_na] <- slot[k]   # later (more recent) slot wins
  }

  for (k in seq_len(S))
    df[[paste0("sep_recent_", slot[k])]] <- !is.na(target) & target == slot[k]

  n_flag <- sum(!is.na(target))

  # Placeholder path: sep_recent women whose union end was already back-filled
  # by cleanENADID (flag=21, end = union_start_next - 1) have no open slot and
  # are missed by the loop above. Find their most recent slot with flag=21 and
  # mark it as sep_recent_placeholder_{s} so impute_sep_recent can re-draw
  # within [union_start, back-fill bound] rather than leaving the wrong date.
  uI_cols  <- paste0("union_end_cmc_I", slot)
  no_open  <- sep & is.na(target)
  ph_target <- rep(NA_integer_, n)
  for (k in seq_len(S)) {
    iI_col <- uI_cols[k]
    if (!iI_col %in% names(df)) next
    iI        <- suppressWarnings(as.integer(df[[iI_col]]))
    has_start <- !is.na(suppressWarnings(as.numeric(df[[us_cols[k]]])))
    # later (more recent) slot wins — loop is ascending so this is correct
    ph_target[no_open & has_start & !is.na(iI) & iI == 21L] <- slot[k]
  }
  for (k in seq_len(S))
    df[[paste0("sep_recent_placeholder_", slot[k])]] <-
      !is.na(ph_target) & ph_target == slot[k]

  n_ph <- sum(!is.na(ph_target))
  message(n_flag, " separated women flagged on their current open union; ",
          n_ph,   " flagged on a cleanENADID placeholder slot (will be re-imputed); ",
          sum(sep) - n_flag - n_ph, " unresolved (no open or placeholder slot)")

  # context: marriage vs cohabitation for the flagged union
  ftype <- rep(NA_character_, n)
  for (k in seq_len(S)) {
    tcol <- paste0("union_start_type", slot[k])
    if (!tcol %in% names(df)) next
    hit <- !is.na(target) & target == slot[k]
    if (any(hit)) ftype[hit] <- as.character(df[[tcol]][hit])
  }
  cat("Flagged separated-union type:\n")
  print(table(flagged_union_type = ftype, useNA = "no"))
  df
}
