# ENADID_val_import.R ======
# Independent reimplementation of all ENADID survey cycles (1992–2023).
# One getDatos_ENADIDXXXX_val() function per cycle.
# Reads raw data files directly; does NOT call any enadid_lib.R function.
# Uses only helpers from ENADID_val_lib.R and base R / tidyverse.

source("ENADID_val_lib.R")

# Data root — mirrors rootPath convention from the original scripts.
data_root <- file.path(rootPath, "INEGI/Encuestas/ENADID")


# ── Shared birth-history helper ──────────────────────────────────────────────
# Converts a long births dataframe to wide, with columns dob_cmcN, dob_cmc_IN,
# sexN, dod_cmcN, dod_cmc_IN (N = birth_num).
# order_col: integer birth-order column (NA or 99 = unknown).
val_births_wide <- function(births_long) {
  # Sort by birth order if available, else by dob_cmc.
  births_long <- births_long |>
    dplyr::arrange(CaseID, dplyr::desc(!is.na(order_col) & order_col != 99L),
                   order_col, dob_cmc) |>
    dplyr::group_by(CaseID) |>
    dplyr::mutate(birth_num = dplyr::row_number()) |>
    dplyr::ungroup()

  births_long |>
    tidyr::pivot_wider(
      id_cols    = CaseID,
      names_from = birth_num,
      values_from = c(dob_cmc, dob_cmc_I, sex, dod_cmc, dod_cmc_I),
      names_sep  = ""
    )
}


# ── Death-date helper ─────────────────────────────────────────────────────────
# Given parallel day / month / year vectors, compute dod_cmc and dod_cmc_I.
# Only one of days/months/years should be non-NA per row (ENADID convention).
# Returns a list(dod_cmc, dod_cmc_I).
val_dod <- function(dob_cmc, days_v, months_v, years_v, missing_thresh = 50L) {
  n    <- length(dob_cmc)
  dod  <- rep(NA_integer_, n)
  dod_I <- rep(NA_integer_, n)

  # days: child lived < 1 month
  idx <- !is.na(days_v)
  if (any(idx)) {
    d    <- as.integer(days_v[idx])
    prob <- pmin(d / 30.44, 1)
    dod[idx]   <- dob_cmc[idx] + stats::rbinom(sum(idx), 1L, prob)
    dod[idx]   <- dplyr::if_else(d > missing_thresh, 9999L, dod[idx])
    dod_I[idx] <- dplyr::if_else(d > missing_thresh, 10L, 1L)
  }

  # months: child lived < 1 year
  idx <- !is.na(months_v)
  if (any(idx)) {
    m   <- as.integer(months_v[idx])
    dod[idx]   <- dob_cmc[idx] + m + sample(c(0L, 1L), sum(idx), replace = TRUE)
    dod[idx]   <- dplyr::if_else(m > missing_thresh, 9999L, dod[idx])
    dod_I[idx] <- dplyr::if_else(m > missing_thresh, 10L, 1L)
  }

  # years: child lived ≥ 1 year
  idx <- !is.na(years_v)
  if (any(idx)) {
    y   <- as.integer(years_v[idx])
    dod[idx]   <- dob_cmc[idx] + y * 12L + sample(0L:11L, sum(idx), replace = TRUE)
    dod[idx]   <- dplyr::if_else(y > missing_thresh, 9999L, dod[idx])
    dod_I[idx] <- dplyr::if_else(y > missing_thresh, 10L, 1L)
  }

  list(dod_cmc = dod, dod_cmc_I = dod_I)
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1992 ======
# Files:  FECUNDIDAD_1.DBF  (respondent)
#         FECUNDIDAD_2.DBF  (births)
# Key:    composite from IENT+ZONA+ESTRATO+CONTROL+IVIV+HOGAR+REG_MUJER
# Union:  NOT collected (all union slots NA)
# DOB birth: P9_20A (month), P9_20B (year 2-digit 47-92 → +1900, 99 → NA)
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID1992_val <- function() {

  mujer_path <- file.path(data_root, "1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_1.DBF")
  emb_path   <- file.path(data_root, "1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_2.DBF")

  mujer  <- foreign::read.dbf(mujer_path, as.is = TRUE)
  emb    <- foreign::read.dbf(emb_path,   as.is = TRUE)

  # Convert all columns to integer (skip first 8 id columns)
  int_cols <- function(df, skip = 8L) {
    df[, seq(skip + 1L, ncol(df))] <- lapply(df[, seq(skip + 1L, ncol(df))],
                                               function(x) suppressWarnings(as.integer(as.character(x))))
    df
  }
  mujer <- int_cols(mujer)
  emb   <- int_cols(emb)

  # Build composite key
  make_key <- function(df) {
    k <- paste0(df$IENT, df$ZONA, df$ESTRATO, df$CONTROL, df$IVIV, df$HOGAR, df$REG_MUJER)
    gsub("[^0-9]", "", k)
  }
  mujer$CaseID <- make_key(mujer)
  emb$CaseID   <- make_key(emb)

  # Fix respondent birth year (2-digit 47-92 → +1900)
  dob_yr <- val_fix_year(mujer$P9_1B, lo = 47L, hi = 92L)
  dob_mo <- as.integer(mujer$P9_1A)

  datos <- tibble::tibble(
    country         = "Mexico",
    survey          = "ENADID1992",
    CaseID          = mujer$CaseID,
    surveyDate_cmc  = val_cmc(12L, 1992L),   # survey Aug–Nov 1992, cut Dec 1992
    lastYear        = NA_integer_,
    indiv_dob_cmc   = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I = val_imputed(dob_mo, dob_yr),
    yBirth          = dob_yr,
    indiv_age_survey = as.integer(mujer$P9_2),
    indiv_weight    = as.integer(mujer$FMUJ),
    union_status    = factor(NA_integer_,
                             levels = UNION_STATUS_LEVELS,
                             labels = UNION_STATUS_LABELS),
    pregnant        = val_pregnant_1992(mujer$P9_11),
    want_another    = factor(NA_integer_,
                             levels = WANT_ANOTHER_LEVELS,
                             labels = WANT_ANOTHER_LABELS),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex   = NA_integer_,
    ever_contraception = NA_integer_,
    nUnion          = NA_integer_,
    nBioKids        = as.integer(mujer$P9_10A)
  )

  # ── Births ──────────────────────────────────────────────────────────────────
  # Birth year: 2-digit 47-92 → +1900, 99 → NA
  emb$P9_20B <- val_fix_year(emb$P9_20B, lo = 47L, hi = 92L)
  emb$dob_cmc   <- val_cmc(as.integer(emb$P9_20A), emb$P9_20B)
  emb$dob_cmc_I <- val_imputed(as.integer(emb$P9_20A), emb$P9_20B)

  # Sex: P9_15 (living) or P9_18 (dead; mutually exclusive in original)
  stopifnot(!any(!is.na(emb$P9_18) & !is.na(emb$P9_15)))
  emb$sex <- dplyr::coalesce(as.integer(emb$P9_15), as.integer(emb$P9_18))

  # Filter: keep only live births
  #   mortinatos (P9_23 in 6:10 or 99) → exclude
  #   abortos    (P9_27 in 1:5  or 99) → exclude
  emb <- emb[!(as.integer(emb$P9_23) %in% c(6:10, 99)), ]
  emb <- emb[!(as.integer(emb$P9_27) %in% c(1:5,  99)), ]

  # Death dates
  dod <- val_dod(emb$dob_cmc,
                 days_v   = as.integer(emb$P9_19A),
                 months_v = as.integer(emb$P9_19B),
                 years_v  = as.integer(emb$P9_19C))
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I
  emb$order_col <- as.integer(emb$P9_29)

  births_long <- emb[, c("CaseID", "dob_cmc", "dob_cmc_I", "sex",
                          "dod_cmc", "dod_cmc_I", "order_col")]
  births_wide <- val_births_wide(births_long)

  # ── Join, finalise ───────────────────────────────────────────────────────────
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1997 ======
# Files:  E97CMU.DBF  (respondent)
#         E97DGE.DBF  (general — marital status supplement)
#         E97UNI.DBF  (union history for women with previous unions)
#         E97HEM.DBF  (births)
# Key:    composite from ENT+MUN+ZONA+UPM+F_VIV+HOGAR+P3_1
# Year 2-digit convention:
#   DOB:          20-92 → +1900, 99 → NA
#   Union dates:  50-97 → +1900, 99 → NA
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID1997_val <- function() {

  mujer_path <- file.path(data_root, "1997/base_datos_enadid97_dbf/E97CMU.DBF")
  gen_path   <- file.path(data_root, "1997/base_datos_enadid97_dbf/E97DGE.DBF")
  uni_path   <- file.path(data_root, "1997/base_datos_enadid97_dbf/E97UNI.DBF")
  emb_path   <- file.path(data_root, "1997/base_datos_enadid97_dbf/E97HEM.DBF")

  int_cols <- function(df, skip = 10L, not_cols = integer(0)) {
    idx <- setdiff(seq(skip + 1L, ncol(df)), not_cols)
    df[, idx] <- lapply(df[, idx],
                        function(x) suppressWarnings(as.integer(as.character(x))))
    df
  }
  mujer <- int_cols(foreign::read.dbf(mujer_path, as.is = TRUE),
                    not_cols = c(63L, 64L, 71L, 75L))
  general <- int_cols(foreign::read.dbf(gen_path, as.is = TRUE))
  uni     <- int_cols(foreign::read.dbf(uni_path, as.is = TRUE))
  emb     <- int_cols(foreign::read.dbf(emb_path, as.is = TRUE))

  # FAC_MUJ may be wider than integer — coerce safely
  mujer$FAC_MUJ <- suppressWarnings(as.integer(mujer$FAC_MUJ))

  # Composite key
  make_key <- function(df) {
    k <- paste0(df$ENT, df$MUN, df$ZONA, df$UPM, df$F_VIV, df$HOGAR, df$P3_1)
    gsub("[^0-9]", "", k)
  }
  mujer$CaseID <- make_key(mujer)
  general$CaseID <- make_key(general)
  uni$CaseID     <- make_key(uni)
  emb$CaseID     <- make_key(emb)

  # Add marital status from general table
  mujer <- dplyr::left_join(mujer, general[, c("CaseID", "P6_1")], by = "CaseID")

  # DOB (respondent): 2-digit 20-92 → +1900
  dob_yr <- val_fix_year(mujer$P9_1B, lo = 20L, hi = 92L)
  dob_mo <- as.integer(mujer$P9_1A)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID1997",
    CaseID           = mujer$CaseID,
    surveyDate_cmc   = val_cmc(1L, 1998L),  # Sep–Dec 1997, cut Jan 1998
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = as.integer(mujer$P9_2A),
    indiv_weight     = as.integer(mujer$FAC_MUJ),
    union_status     = val_union_status_1997(mujer$P14_1),
    pregnant         = val_pregnant_std(mujer$P11_1),
    want_another     = val_want_another_1997(mujer$P11_4, mujer$P11_4),
    prob_want_another = factor(NA_integer_,
                                levels = PROB_WANT_ANOTHER_LEVELS,
                                labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = NA_integer_,
    ever_contraception = dplyr::case_when(
      as.integer(mujer$P12_14) == 1L ~ 1L,  # currently using → ever used
      as.integer(mujer$P12_3)  == 2L ~ 1L,  # P12_3: 1=no, 2=yes
      as.integer(mujer$P12_3)  == 1L ~ 0L,
      TRUE                           ~ NA_integer_),
    nUnion           = NA_integer_,  # filled after union history
    nBioKids         = dplyr::coalesce(as.integer(mujer$P9_7), 0L)
  )

  # ── Union history ─────────────────────────────────────────────────────────
  # nUnionBeforeLast from P14_8 (number of previous unions recorded in E97UNI)
  datos$nUnion <- as.integer(mujer$P14_8) + 1L  # +1 for the last union
  datos$nUnion[is.na(mujer$P14_8)] <- NA_integer_

  # Helper: fix 2-digit union years (50-97 → +1900)
  fix_uni_yr <- function(v) val_fix_year(v, lo = 50L, hi = 97L)

  # ── Previous unions from E97UNI ───────────────────────────────────────────
  # Each row in uni is one previous union for a woman (P14_9 = union order number).
  # P14_11: type (1=cohab, 2/3/4=marriage)
  # P14_14: cohabited before? (1=yes)
  # P14_10A/B: start month/year; P14_15A/B: cohab-start month/year (if P14_14==1)
  # P14_13A/B: end month/year
  # P14_12: end motive (1=separation, 2=widowhood, 3=divorce→separation, 9=dk)
  # P14_15A/B: cohabitation-before-marriage start

  uni$start_yr  <- fix_uni_yr(uni$P14_10B)
  uni$start_cmc <- val_cmc(as.integer(uni$P14_10A), uni$start_yr)
  uni$start_I   <- val_imputed(as.integer(uni$P14_10A), uni$start_yr)

  uni$end_yr    <- fix_uni_yr(uni$P14_13B)
  uni$end_cmc   <- val_cmc(as.integer(uni$P14_13A), uni$end_yr)
  uni$end_I     <- val_imputed(as.integer(uni$P14_13A), uni$end_yr)

  uni$cohab_yr   <- fix_uni_yr(uni$P14_15B)
  uni$cohab_cmc  <- val_cmc(as.integer(uni$P14_15A), uni$cohab_yr)
  uni$cohab_I    <- val_imputed(as.integer(uni$P14_15A), uni$cohab_yr)

  uni$cohab_before <- (as.integer(uni$P14_14) == 1L)

  # Determine union_start_type:
  #   P14_11 1=cohab, 2/3/4=marriage.  If cohab_before & is marriage → type 3
  uni$raw_type <- as.integer(uni$P14_11)
  uni$type_int <- dplyr::case_when(
    uni$raw_type %in% 2:4 & uni$cohab_before ~ 3L,  # cohabitation before marriage
    uni$raw_type %in% 2:4                   ~ 1L,  # marriage
    uni$raw_type == 1L                      ~ 2L,  # cohabitation
    TRUE                                    ~ NA_integer_
  )

  # If cohab_before: union_start_cmc = cohab start, marriage_start_cmc = marriage start
  uni$union_start_cmc   <- dplyr::if_else(uni$cohab_before, uni$cohab_cmc,  uni$start_cmc)
  uni$union_start_cmc_I <- dplyr::if_else(uni$cohab_before, uni$cohab_I,    uni$start_I)
  uni$marriage_start_cmc   <- dplyr::if_else(uni$type_int %in% c(1L, 3L),
                                              uni$start_cmc, NA_integer_)
  uni$marriage_start_cmc_I <- dplyr::if_else(uni$type_int %in% c(1L, 3L),
                                              uni$start_I,  NA_integer_)
  uni$union_end_motive <- val_union_end_motive_enadid(uni$P14_12)

  # Sequential union number within woman
  uni <- uni |>
    dplyr::arrange(CaseID, as.integer(P14_9)) |>
    dplyr::group_by(CaseID) |>
    dplyr::mutate(union_num = dplyr::row_number()) |>
    dplyr::ungroup()

  max_prev_unions <- max(uni$union_num, na.rm = TRUE)

  # Pivot previous unions to wide
  uni_wide <- uni |>
    tidyr::pivot_wider(
      id_cols     = CaseID,
      names_from  = union_num,
      values_from = c(type_int, union_start_cmc, union_start_cmc_I,
                      marriage_start_cmc, marriage_start_cmc_I,
                      end_cmc, end_I, union_end_motive),
      names_sep   = ""
    )
  # Rename to schema names
  for (u in seq_len(max_prev_unions)) {
    if (paste0("type_int", u) %in% names(uni_wide)) {
      uni_wide <- uni_wide |>
        dplyr::rename(
          !!paste0("union_start_type",     u) := paste0("type_int",           u),
          !!paste0("union_end_cmc",        u) := paste0("end_cmc",            u),
          !!paste0("union_end_cmc_I",      u) := paste0("end_I",              u)
        )
    }
  }
  # Factorise type columns
  for (u in seq_len(max_prev_unions)) {
    tc <- paste0("union_start_type", u)
    if (tc %in% names(uni_wide))
      uni_wide[[tc]] <- factor(uni_wide[[tc]],
                               levels = UNION_START_TYPE_LEVELS,
                               labels = UNION_START_TYPE_LABELS)
  }

  datos <- dplyr::left_join(datos, uni_wide, by = "CaseID")

  # ── Last union (from mujeres table) → slot nUnion ───────────────────────────
  last_start_yr  <- fix_uni_yr(mujer$P14_3B)
  last_end_yr    <- fix_uni_yr(mujer$P14_2B)
  last_cohab_yr  <- fix_uni_yr(mujer$P14_6B)

  last_start_cmc   <- val_cmc(as.integer(mujer$P14_3A), last_start_yr)
  last_start_I     <- val_imputed(as.integer(mujer$P14_3A), last_start_yr)
  last_end_cmc     <- val_cmc(as.integer(mujer$P14_2A), last_end_yr)
  last_end_I       <- val_imputed(as.integer(mujer$P14_2A), last_end_yr)
  last_cohab_cmc   <- val_cmc(as.integer(mujer$P14_6A), last_cohab_yr)
  last_cohab_I     <- val_imputed(as.integer(mujer$P14_6A), last_cohab_yr)
  last_cohab_before <- (as.integer(mujer$P14_5) == 1L)

  # Infer last-union type from union_status
  raw_status <- as.integer(mujer$P14_1)
  # 1=cohab, 2=sep-cohab, 3=sep-marr, 4-5=divorced, 6=wid-cohab, 7=wid-marr, 8-10=married, 11=single
  last_was_marriage <- raw_status %in% c(3L, 4L, 5L, 7L, 8L, 9L, 10L)
  last_type_int <- dplyr::case_when(
    last_was_marriage & last_cohab_before ~ 3L,
    last_was_marriage                    ~ 1L,
    !is.na(raw_status)                   ~ 2L,
    TRUE                                 ~ NA_integer_
  )
  last_union_start_cmc <- dplyr::if_else(last_cohab_before, last_cohab_cmc, last_start_cmc)
  last_union_start_I   <- dplyr::if_else(last_cohab_before, last_cohab_I,   last_start_I)
  last_marr_start_cmc  <- dplyr::if_else(last_type_int %in% c(1L, 3L), last_start_cmc, NA_integer_)
  last_marr_start_I    <- dplyr::if_else(last_type_int %in% c(1L, 3L), last_start_I,  NA_integer_)

  # End motive: "in union" if currently united; otherwise from status
  currently_united <- raw_status %in% c(1L, 8L, 9L, 10L)  # cohab or married
  last_end_motive_int <- dplyr::case_when(
    currently_united            ~ 0L,  # in union
    raw_status %in% c(6L, 7L)  ~ 1L,  # widowhood
    raw_status %in% c(2L, 3L, 4L, 5L) ~ 2L,  # separation / divorce
    TRUE                        ~ NA_integer_
  )
  last_end_cmc_final <- dplyr::if_else(currently_united, NA_integer_, last_end_cmc)
  last_end_I_final   <- dplyr::if_else(currently_united, NA_integer_, last_end_I)

  # Write last-union data into the nUnion-th slot for each woman
  maxU <- max(datos$nUnion, na.rm = TRUE)
  for (u in seq_len(maxU)) {
    rows <- !is.na(datos$nUnion) & datos$nUnion == u
    if (!any(rows)) next
    for (col in c(paste0("union_start_type",     u),
                  paste0("union_start_cmc",       u),
                  paste0("union_start_cmc_I",     u),
                  paste0("marriage_start_cmc",    u),
                  paste0("marriage_start_cmc_I",  u),
                  paste0("union_end_cmc",         u),
                  paste0("union_end_cmc_I",       u),
                  paste0("union_end_motive",      u))) {
      if (!(col %in% names(datos))) datos[[col]] <- NA
    }
    datos[[paste0("union_start_type",    u)]][rows] <- factor(last_type_int[rows],
                                                               levels = UNION_START_TYPE_LEVELS,
                                                               labels = UNION_START_TYPE_LABELS)
    datos[[paste0("union_start_cmc",     u)]][rows] <- last_union_start_cmc[rows]
    datos[[paste0("union_start_cmc_I",   u)]][rows] <- last_union_start_I[rows]
    datos[[paste0("marriage_start_cmc",  u)]][rows] <- last_marr_start_cmc[rows]
    datos[[paste0("marriage_start_cmc_I",u)]][rows] <- last_marr_start_I[rows]
    datos[[paste0("union_end_cmc",       u)]][rows] <- last_end_cmc_final[rows]
    datos[[paste0("union_end_cmc_I",     u)]][rows] <- last_end_I_final[rows]
    datos[[paste0("union_end_motive",    u)]][rows] <- factor(
      last_end_motive_int[rows],
      levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)
  }

  n_slots <- maxU
  datos <- val_order_union_history(datos, n_slots)

  # ── Births ───────────────────────────────────────────────────────────────────
  # Filter: P9_40 in (1, 2) = live births
  emb <- emb[as.integer(emb$P9_40) %in% c(1L, 2L), ]
  emb$dob_cmc   <- val_cmc(as.integer(emb$P9_17A), as.integer(emb$P9_17B))
  emb$dob_cmc_I <- val_imputed(as.integer(emb$P9_17A), as.integer(emb$P9_17B))
  # Sex: P9_12 (living) or P9_15 (dead)
  emb$sex       <- dplyr::coalesce(as.integer(emb$P9_12), as.integer(emb$P9_15))
  emb$order_col <- as.integer(emb$P9_41)

  dod <- val_dod(emb$dob_cmc,
                 days_v   = as.integer(emb$P9_16A),
                 months_v = as.integer(emb$P9_16B),
                 years_v  = as.integer(emb$P9_16C))
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_long <- emb[, c("CaseID", "dob_cmc", "dob_cmc_I", "sex",
                          "dod_cmc", "dod_cmc_I", "order_col")]
  births_wide <- val_births_wide(births_long)
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# HELPER: build union slots from "last union" + "first non-last union" columns.
# Used by cycles 2006–2023 (no separate union table).
# Arguments are all vectors aligned to the women dataframe.
# Returns a wide tibble with union_start_typeN … union_end_motiveN columns.
# ═══════════════════════════════════════════════════════════════════════════════

val_build_union_slots_enadid <- function(
    CaseID,
    union_status_int,     # raw ENADID code, used to classify last union
    nUnionBeforeLast,
    # Last union fields (already CMC or NA):
    last_start_mo, last_start_yr,
    last_end_mo,   last_end_yr,
    last_cohab_before_flag,    # integer 1/2/9; 1 = yes
    last_cohab_mo, last_cohab_yr,
    # First-non-last union (if nUnionBeforeLast >= 1):
    first_start_mo, first_start_yr,
    first_end_mo,   first_end_yr,
    first_end_motive_raw,   # ENADID code 1/2/3/9
    first_type_raw,         # ENADID code 1=cohab, 2=marriage, 9=dk
    first_cohab_before_flag,
    first_cohab_mo, first_cohab_yr,
    # Year-fixing parameters for 2-digit years (lo > hi → already 4-digit)
    yr_lo = 100L, yr_hi = 99L
) {
  fix_yr <- function(v) val_fix_year(v, lo = yr_lo, hi = yr_hi)
  cmc    <- function(mo, yr) val_cmc(as.integer(mo), fix_yr(yr))
  cmc_I  <- function(mo, yr) val_imputed(as.integer(mo), fix_yr(yr))

  n <- length(CaseID)

  # Last union derived fields
  last_cohab_before <- (as.integer(last_cohab_before_flag) == 1L)
  # 2006-2023 union_status codes (p10_1 or p10d03):
  #   7=married, 1=cohab, 2=sep-cohab, 3=sep-marr, 4=divorced, 5=wid-cohab, 6=wid-marr, 8=single
  last_was_marriage <- as.integer(union_status_int) %in% c(3L, 4L, 6L, 7L)
  last_type_int <- dplyr::case_when(
    last_was_marriage & last_cohab_before ~ 3L,
    last_was_marriage                    ~ 1L,
    !is.na(union_status_int)             ~ 2L,
    TRUE                                 ~ NA_integer_
  )
  last_sc  <- cmc(last_start_mo, last_start_yr)
  last_sI  <- cmc_I(last_start_mo, last_start_yr)
  last_cc  <- cmc(last_cohab_mo, last_cohab_yr)
  last_cI  <- cmc_I(last_cohab_mo, last_cohab_yr)
  last_ec  <- cmc(last_end_mo, last_end_yr)
  last_eI  <- cmc_I(last_end_mo, last_end_yr)

  last_union_start_cmc   <- dplyr::if_else(last_cohab_before, last_cc, last_sc)
  last_union_start_cmc_I <- dplyr::if_else(last_cohab_before, last_cI, last_sI)
  last_marr_start_cmc    <- dplyr::if_else(last_type_int %in% c(1L, 3L), last_sc, NA_integer_)
  last_marr_start_cmc_I  <- dplyr::if_else(last_type_int %in% c(1L, 3L), last_sI, NA_integer_)

  currently_united <- as.integer(union_status_int) %in% c(1L, 7L)
  last_end_motive_int <- dplyr::case_when(
    currently_united                              ~ 0L,  # in union
    as.integer(union_status_int) %in% c(5L, 6L) ~ 1L,  # widowhood
    as.integer(union_status_int) %in% c(2L, 3L, 4L) ~ 2L,  # separation/divorce
    TRUE                                          ~ NA_integer_
  )
  last_end_cmc_final <- dplyr::if_else(currently_united, NA_integer_, last_ec)
  last_end_I_final   <- dplyr::if_else(currently_united, NA_integer_, last_eI)

  # First non-last union derived fields
  nU <- as.integer(nUnionBeforeLast)
  has_first <- !is.na(nU) & nU >= 1L

  first_cohab_before <- has_first & (as.integer(first_cohab_before_flag) == 1L)
  first_type_raw_int <- as.integer(first_type_raw)
  first_type_int <- dplyr::case_when(
    has_first & first_type_raw_int == 2L & first_cohab_before ~ 3L,
    has_first & first_type_raw_int == 2L                      ~ 1L,
    has_first & first_type_raw_int == 1L                      ~ 2L,
    has_first                                                  ~ NA_integer_,
    TRUE                                                       ~ NA_integer_
  )
  first_sc <- dplyr::if_else(has_first, cmc(first_start_mo, first_start_yr), NA_integer_)
  first_sI <- dplyr::if_else(has_first, cmc_I(first_start_mo, first_start_yr), NA_integer_)
  first_cc <- dplyr::if_else(has_first, cmc(first_cohab_mo, first_cohab_yr), NA_integer_)
  first_cI <- dplyr::if_else(has_first, cmc_I(first_cohab_mo, first_cohab_yr), NA_integer_)
  first_ec <- dplyr::if_else(has_first, cmc(first_end_mo, first_end_yr), NA_integer_)
  first_eI <- dplyr::if_else(has_first, cmc_I(first_end_mo, first_end_yr), NA_integer_)

  first_union_start_cmc   <- dplyr::if_else(first_cohab_before, first_cc, first_sc)
  first_union_start_cmc_I <- dplyr::if_else(first_cohab_before, first_cI, first_sI)
  first_marr_start_cmc    <- dplyr::if_else(first_type_int %in% c(1L, 3L), first_sc, NA_integer_)
  first_marr_start_cmc_I  <- dplyr::if_else(first_type_int %in% c(1L, 3L), first_sI, NA_integer_)
  first_end_motive <- dplyr::if_else(has_first,
    as.integer(val_union_end_motive_enadid(first_end_motive_raw)), NA_integer_)

  # nUnion
  nUnion <- dplyr::if_else(!is.na(nU), nU + 1L, NA_integer_)

  out <- tibble::tibble(CaseID = CaseID, nUnion = nUnion)

  # Slot 1: first non-last union (if nUnion >= 2)
  out$union_start_type1     <- factor(dplyr::if_else(nUnion >= 2L, first_type_int, NA_integer_),
                                      levels = UNION_START_TYPE_LEVELS, labels = UNION_START_TYPE_LABELS)
  out$union_start_cmc1      <- dplyr::if_else(nUnion >= 2L, first_union_start_cmc,   NA_integer_)
  out$union_start_cmc_I1    <- dplyr::if_else(nUnion >= 2L, first_union_start_cmc_I, NA_integer_)
  out$marriage_start_cmc1   <- dplyr::if_else(nUnion >= 2L, first_marr_start_cmc,    NA_integer_)
  out$marriage_start_cmc_I1 <- dplyr::if_else(nUnion >= 2L, first_marr_start_cmc_I,  NA_integer_)
  out$union_end_cmc1        <- dplyr::if_else(nUnion >= 2L, first_ec, NA_integer_)
  out$union_end_cmc_I1      <- dplyr::if_else(nUnion >= 2L, first_eI, NA_integer_)
  out$union_end_motive1     <- factor(dplyr::if_else(nUnion >= 2L, first_end_motive, NA_integer_),
                                      levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)

  # Slot 2: last union (when nUnion == 2)
  out$union_start_type2     <- factor(dplyr::if_else(nUnion == 2L, last_type_int, NA_integer_),
                                      levels = UNION_START_TYPE_LEVELS, labels = UNION_START_TYPE_LABELS)
  out$union_start_cmc2      <- dplyr::if_else(nUnion == 2L, last_union_start_cmc,   NA_integer_)
  out$union_start_cmc_I2    <- dplyr::if_else(nUnion == 2L, last_union_start_cmc_I, NA_integer_)
  out$marriage_start_cmc2   <- dplyr::if_else(nUnion == 2L, last_marr_start_cmc,    NA_integer_)
  out$marriage_start_cmc_I2 <- dplyr::if_else(nUnion == 2L, last_marr_start_cmc_I,  NA_integer_)
  out$union_end_cmc2        <- dplyr::if_else(nUnion == 2L, last_end_cmc_final,      NA_integer_)
  out$union_end_cmc_I2      <- dplyr::if_else(nUnion == 2L, last_end_I_final,        NA_integer_)
  out$union_end_motive2     <- factor(dplyr::if_else(nUnion == 2L, last_end_motive_int, NA_integer_),
                                      levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)

  # Slot 1 (only union): last union when nUnion == 1
  out$union_start_type1[nUnion == 1L & !is.na(nUnion)] <-
    factor(last_type_int[nUnion == 1L & !is.na(nUnion)],
           levels = UNION_START_TYPE_LEVELS, labels = UNION_START_TYPE_LABELS)
  out$union_start_cmc1[nUnion == 1L & !is.na(nUnion)]      <- last_union_start_cmc[nUnion == 1L & !is.na(nUnion)]
  out$union_start_cmc_I1[nUnion == 1L & !is.na(nUnion)]    <- last_union_start_cmc_I[nUnion == 1L & !is.na(nUnion)]
  out$marriage_start_cmc1[nUnion == 1L & !is.na(nUnion)]   <- last_marr_start_cmc[nUnion == 1L & !is.na(nUnion)]
  out$marriage_start_cmc_I1[nUnion == 1L & !is.na(nUnion)] <- last_marr_start_cmc_I[nUnion == 1L & !is.na(nUnion)]
  out$union_end_cmc1[nUnion == 1L & !is.na(nUnion)]        <- last_end_cmc_final[nUnion == 1L & !is.na(nUnion)]
  out$union_end_cmc_I1[nUnion == 1L & !is.na(nUnion)]      <- last_end_I_final[nUnion == 1L & !is.na(nUnion)]
  out$union_end_motive1[nUnion == 1L & !is.na(nUnion)] <-
    factor(last_end_motive_int[nUnion == 1L & !is.na(nUnion)],
           levels = UNION_END_MOTIVE_LEVELS, labels = UNION_END_MOTIVE_LABELS)

  out
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2006 ======
# Files: ENADID06_Mujer.csv, ENADID06_Fecundidad.csv
# Key:   claveres (already in file)
# Survey date: individual interview date from "fecha1" column
# Years: already 4-digit in CSV
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID2006_val <- function() {

  mujer_path <- file.path(data_root, "2006/ENADID06_Mujer.csv")
  emb_path   <- file.path(data_root, "2006/ENADID06_Fecundidad.csv")

  mujer <- read.csv(mujer_path)
  emb   <- read.csv(emb_path)

  mujer$CaseID <- as.character(mujer$claveres)
  emb$CaseID   <- as.character(emb$claveres)

  # Individual interview date
  inter_date  <- as.Date(mujer$fecha1, format = "%d/%m/%y")
  inter_month <- as.integer(format(inter_date, "%m"))
  inter_year  <- as.integer(format(inter_date, "%Y"))
  inter_month[is.na(inter_month)] <- 3L
  inter_year[is.na(inter_year)]   <- 2006L
  survey_cmc <- val_cmc(inter_month, inter_year)

  dob_mo <- as.integer(mujer$p05d01m)
  dob_yr <- as.integer(mujer$p05d01a)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID2006",
    CaseID           = mujer$CaseID,
    surveyDate_cmc   = survey_cmc,
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = as.integer(mujer$p05d0201),
    indiv_weight     = as.integer(round(mujer$fac_muje)),
    union_status     = val_union_status_2006(mujer$p10d03),
    pregnant         = val_pregnant_std(mujer$p07d02),
    want_another     = val_want_another_std(mujer$p07d04, mujer$p07d04),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = dplyr::if_else(as.integer(mujer$p10d01) %in% c(88L, 98L, 99L),
                                      NA_integer_, as.integer(mujer$p10d01)),
    ever_contraception = NA_integer_,
    nBioKids         = dplyr::coalesce(as.integer(mujer$p05d06), 0L)
  )

  # Union history
  uh <- val_build_union_slots_enadid(
    CaseID               = mujer$CaseID,
    union_status_int     = mujer$p10d03,
    nUnionBeforeLast     = mujer$p10d10,
    last_start_mo  = mujer$p10d05m, last_start_yr = mujer$p10d05a,
    last_end_mo    = mujer$p10d04m, last_end_yr   = mujer$p10d04a,
    last_cohab_before_flag = mujer$p10d07,
    last_cohab_mo  = mujer$p10d08m, last_cohab_yr = mujer$p10d08a,
    first_start_mo = mujer$p10d11m, first_start_yr = mujer$p10d11a,
    first_end_mo   = mujer$p10d13m, first_end_yr   = mujer$p10d13a,
    first_end_motive_raw    = mujer$p10d12,
    first_type_raw          = mujer$p10d14,
    first_cohab_before_flag = mujer$p10d15,
    first_cohab_mo = mujer$p10d16m, first_cohab_yr = mujer$p10d16a
  )
  datos$nUnion <- uh$nUnion
  datos <- dplyr::left_join(datos, uh[, setdiff(names(uh), "nUnion")], by = "CaseID")

  # Births
  emb$dob_cmc   <- val_cmc(as.integer(emb$p05d13m), as.integer(emb$p05d13a))
  emb$dob_cmc_I <- val_imputed(as.integer(emb$p05d13m), as.integer(emb$p05d13a))
  emb$sex       <- dplyr::coalesce(as.integer(emb$sexovivo), as.integer(emb$sexofac))
  emb <- emb[!is.na(emb$sex), ]
  emb$order_col <- as.integer(emb$renglon)

  dod <- val_dod(emb$dob_cmc,
                 days_v   = as.integer(emb$p05d12d),
                 months_v = as.integer(emb$p05d12m),
                 years_v  = as.integer(emb$p05d12a))
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_wide <- val_births_wide(emb[, c("CaseID","dob_cmc","dob_cmc_I","sex",
                                          "dod_cmc","dod_cmc_I","order_col")])
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2009 ======
# Files: tr_cmu.dbf, tr_smi.DBF, tr_viv_hog.dbf, tr_fec_hemb.dbf
# Key:   LLAVE (already present; standardise to llave_muj)
# Survey date: October 2009
# Birth death: P5_12_3=days, P5_12_2=months, P5_12_1=years (NOTE: reversed)
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID2009_val <- function() {

  mu1_path <- file.path(data_root, "2009/base_datos_enadid09_dbf/tr_cmu.dbf")
  mu2_path <- file.path(data_root, "2009/base_datos_enadid09_dbf/tr_smi.DBF")
  emb_path <- file.path(data_root, "2009/base_datos_enadid09_dbf/tr_fec_hemb.dbf")

  to_int <- function(x) suppressWarnings(as.integer(as.character(x)))
  fix_df <- function(df) {
    df$CaseID <- gsub("[^0-9]", "", as.character(df$LLAVE))
    df[] <- lapply(df, function(x) if (is.factor(x)) to_int(x) else if (is.character(x)) to_int(x) else x)
    df
  }

  mu1 <- fix_df(foreign::read.dbf(mu1_path))
  mu2 <- fix_df(foreign::read.dbf(mu2_path))
  emb <- fix_df(foreign::read.dbf(emb_path))

  drop_cols <- c("control","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog","LLAVE")
  mu1 <- mu1[, setdiff(names(mu1), drop_cols)]
  mu2 <- mu2[, setdiff(names(mu2), drop_cols)]
  emb <- emb[, setdiff(names(emb), drop_cols)]

  mujeres <- dplyr::full_join(mu1, mu2, by = "CaseID")

  dob_mo <- mujeres$P5_1_1
  dob_yr <- as.integer(mujeres$P5_1_2)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID2009",
    CaseID           = mujeres$CaseID,
    surveyDate_cmc   = val_cmc(10L, 2009L),
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = mujeres$P5_2_1.x,
    indiv_weight     = as.integer(round(mujeres$FAC_MUJER.x)),
    union_status     = val_union_status_2009(mujeres$P9_1),
    pregnant         = val_pregnant_std(mujeres$P6_1),
    want_another     = val_want_another_std(mujeres$P6_7, mujeres$P6_9),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = dplyr::if_else(as.integer(mujeres$P7_34) %in% c(88L, 98L, 99L),
                                      NA_integer_, as.integer(mujeres$P7_34)),
    ever_contraception = dplyr::case_when(
      as.integer(mujeres$P7_2) == 1L ~ 1L,
      as.integer(mujeres$P7_2) == 2L ~ 0L,
      TRUE ~ NA_integer_),
    nBioKids         = dplyr::coalesce(mujeres$P5_6, 0L)
  )

  uh <- val_build_union_slots_enadid(
    CaseID               = mujeres$CaseID,
    union_status_int     = mujeres$P9_1,
    nUnionBeforeLast     = mujeres$P9_8,
    last_start_mo  = mujeres$P9_3_1, last_start_yr = mujeres$P9_3_2,
    last_end_mo    = mujeres$P9_2_1, last_end_yr   = mujeres$P9_2_2,
    last_cohab_before_flag = mujeres$P9_5,
    last_cohab_mo  = mujeres$P9_6_1, last_cohab_yr = mujeres$P9_6_2,
    first_start_mo = mujeres$P9_9_1, first_start_yr = mujeres$P9_9_2,
    first_end_mo   = mujeres$P9_11_1, first_end_yr  = mujeres$P9_11_2,
    first_end_motive_raw    = mujeres$P9_10,
    first_type_raw          = mujeres$P9_12,
    first_cohab_before_flag = mujeres$P9_13,
    first_cohab_mo = mujeres$P9_14_1, first_cohab_yr = mujeres$P9_14_2
  )
  datos$nUnion <- uh$nUnion
  datos <- dplyr::left_join(datos, uh[, setdiff(names(uh), "nUnion")], by = "CaseID")

  # Births: filter !is.na(orden) = birth order column ORDENHNV
  emb <- emb[!is.na(emb$ORDENHNV), ]
  emb$dob_cmc   <- val_cmc(emb$P5_13_1, emb$P5_13_2)
  emb$dob_cmc_I <- val_imputed(emb$P5_13_1, emb$P5_13_2)
  emb$sex       <- dplyr::coalesce(as.integer(emb$P5_8), as.integer(emb$P5_11))
  emb$order_col <- as.integer(emb$ORDENHNV)

  # NOTE: P5_12_3=days, P5_12_2=months, P5_12_1=years (inverted relative to later cycles)
  dod <- val_dod(emb$dob_cmc,
                 days_v   = emb$P5_12_3,
                 months_v = emb$P5_12_2,
                 years_v  = emb$P5_12_1)
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_wide <- val_births_wide(emb[, c("CaseID","dob_cmc","dob_cmc_I","sex",
                                          "dod_cmc","dod_cmc_I","order_col")])
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2014 ======
# Files: tmmujer1.csv, tmmujer2.csv, tfec_hemb.csv
# Key:   llave_muj (already in file)
# Survey date: October 2014
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID2014_val <- function() {

  mu1_path <- file.path(data_root,
    "2014/enadid_2014_csv/tmmujer1_enadid2014/conjunto_de_datos/tmmujer1.csv")
  mu2_path <- file.path(data_root,
    "2014/enadid_2014_csv/tmmujer2_enadid2014/conjunto_de_datos/tmmujer2.csv")
  emb_path <- file.path(data_root,
    "2014/enadid_2014_csv/tfec_hemb_enadid2014/conjunto_de_datos/tfec_hemb.csv")

  read_csv_int <- function(path) {
    df <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    df <- dplyr::relocate(df, llave_muj, .before = 1)
    df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.integer(as.character(x)))))
    df$llave_muj <- as.character(df$llave_muj)
    df
  }
  mu1 <- read_csv_int(mu1_path)
  mu2 <- read_csv_int(mu2_path)
  emb <- read_csv_int(emb_path)
  drop_cols <- c("control","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog")
  mu1 <- mu1[, setdiff(names(mu1), drop_cols)]
  mu2 <- mu2[, setdiff(names(mu2), drop_cols)]
  emb <- emb[, setdiff(names(emb), drop_cols)]
  mujeres <- dplyr::full_join(mu1, mu2, by = "llave_muj")
  mujeres$CaseID <- mujeres$llave_muj
  emb$CaseID     <- as.character(emb$llave_muj)

  dob_mo <- mujeres$p5_1_1
  dob_yr <- as.integer(mujeres$p5_1_2)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID2014",
    CaseID           = mujeres$CaseID,
    surveyDate_cmc   = val_cmc(10L, 2014L),
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = mujeres$p5_2,
    indiv_weight     = as.integer(round(mujeres$fac_per.x)),
    union_status     = val_union_status_2006(mujeres$p10_1),
    pregnant         = val_pregnant_std(mujeres$p7_1),
    want_another     = val_want_another_std(mujeres$p7_11, mujeres$p7_7),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = dplyr::if_else(as.integer(mujeres$p8_36) %in% c(88L, 98L, 99L),
                                      NA_integer_, as.integer(mujeres$p8_36)),
    ever_contraception = dplyr::case_when(
      as.integer(mujeres$p8_3) == 1L ~ 1L,
      as.integer(mujeres$p8_3) == 2L ~ 0L,
      TRUE ~ NA_integer_),
    nBioKids         = dplyr::coalesce(as.integer(mujeres$p5_9), 0L)
  )

  uh <- val_build_union_slots_enadid(
    CaseID               = mujeres$CaseID,
    union_status_int     = mujeres$p10_1,
    nUnionBeforeLast     = mujeres$p10_8,
    last_start_mo  = mujeres$p10_3_1, last_start_yr = mujeres$p10_3_2,
    last_end_mo    = mujeres$p10_2_1, last_end_yr   = mujeres$p10_2_2,
    last_cohab_before_flag = mujeres$p10_5,
    last_cohab_mo  = mujeres$p10_6_1, last_cohab_yr = mujeres$p10_6_2,
    first_start_mo = mujeres$p10_9_1, first_start_yr = mujeres$p10_9_2,
    first_end_mo   = mujeres$p10_11_1, first_end_yr  = mujeres$p10_11_2,
    first_end_motive_raw    = mujeres$p10_10,
    first_type_raw          = mujeres$p10_12,
    first_cohab_before_flag = mujeres$p10_13,
    first_cohab_mo = mujeres$p10_14_1, first_cohab_yr = mujeres$p10_14_2
  )
  datos$nUnion <- uh$nUnion
  datos <- dplyr::left_join(datos, uh[, setdiff(names(uh), "nUnion")], by = "CaseID")

  emb <- emb[!is.na(emb$ordenhnv), ]
  emb$dob_cmc   <- val_cmc(emb$p5_17_1, emb$p5_17_2)
  emb$dob_cmc_I <- val_imputed(emb$p5_17_1, emb$p5_17_2)
  emb$sex       <- dplyr::coalesce(as.integer(emb$p5_12), as.integer(emb$p5_15))
  emb$order_col <- as.integer(emb$ordenhnv)

  dod <- val_dod(emb$dob_cmc,
                 days_v   = emb$p5_16_1,
                 months_v = emb$p5_16_2,
                 years_v  = emb$p5_16_3)
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_wide <- val_births_wide(emb[, c("CaseID","dob_cmc","dob_cmc_I","sex",
                                          "dod_cmc","dod_cmc_I","order_col")])
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2018 ======
# Files: conjunto_de_datos_tmujer1/2_enadid_2018.csv, tfechisemb_enadid_2018.csv
# Key:   reconstructed from upm+viv_sel+hogar+n_ren (integer64 in original)
#        We keep as character here.
# Survey date: August 2018
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID2018_val <- function() {

  mu1_path <- file.path(data_root,
    "2018/conjunto_de_datos_enadid_2018_csv/conjunto_de_datos_tmujer1_enadid_2018/conjunto_de_datos/conjunto_de_datos_tmujer1_enadid_2018.csv")
  mu2_path <- file.path(data_root,
    "2018/conjunto_de_datos_enadid_2018_csv/conjunto_de_datos_tmujer2_enadid_2018/conjunto_de_datos/conjunto_de_datos_tmujer2_enadid_2018.csv")
  emb_path <- file.path(data_root,
    "2018/conjunto_de_datos_enadid_2018_csv/conjunto_de_datos_tfechisemb_enadid_2018/conjunto_de_datos/conjunto_de_datos_tfechisemb_enadid_2018.csv")

  read_csv_int <- function(path) {
    df <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    key <- gsub("[^0-9]", "",
                paste0(df$upm, df$viv_sel, df$hogar, df$n_ren))
    df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.integer(x))))
    df$CaseID <- key
    df
  }
  mu1 <- read_csv_int(mu1_path)
  mu2 <- read_csv_int(mu2_path)
  emb <- read_csv_int(emb_path)
  drop_cols <- c("upm","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog")
  mu1 <- mu1[, setdiff(names(mu1), drop_cols)]
  mu2 <- mu2[, setdiff(names(mu2), drop_cols)]
  emb <- emb[, setdiff(names(emb), drop_cols)]
  mujeres <- dplyr::full_join(mu1, mu2, by = "CaseID")

  dob_mo <- mujeres$p5_1_1
  dob_yr <- as.integer(mujeres$p5_1_2)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID2018",
    CaseID           = mujeres$CaseID,
    surveyDate_cmc   = val_cmc(8L, 2018L),
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = mujeres$p5_2_1,
    indiv_weight     = as.integer(round(mujeres$fac_per.x)),
    union_status     = val_union_status_2006(mujeres$p10_1),
    pregnant         = val_pregnant_std(mujeres$p7_1),
    want_another     = val_want_another_std(mujeres$p7_11, mujeres$p7_7),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = dplyr::if_else(as.integer(mujeres$p8_38) %in% c(88L, 98L, 99L),
                                      NA_integer_, as.integer(mujeres$p8_38)),
    ever_contraception = dplyr::case_when(
      as.integer(mujeres$p8_3) == 1L ~ 1L,
      as.integer(mujeres$p8_3) == 2L ~ 0L,
      TRUE ~ NA_integer_),
    nBioKids         = dplyr::coalesce(as.integer(mujeres$p5_9), 0L)
  )

  uh <- val_build_union_slots_enadid(
    CaseID               = mujeres$CaseID,
    union_status_int     = mujeres$p10_1,
    nUnionBeforeLast     = mujeres$p10_8,
    last_start_mo  = mujeres$p10_3_1, last_start_yr = mujeres$p10_3_2,
    last_end_mo    = mujeres$p10_2_1, last_end_yr   = mujeres$p10_2_2,
    last_cohab_before_flag = mujeres$p10_5,
    last_cohab_mo  = mujeres$p10_6_1, last_cohab_yr = mujeres$p10_6_2,
    first_start_mo = mujeres$p10_9_1, first_start_yr = mujeres$p10_9_2,
    first_end_mo   = mujeres$p10_11_1, first_end_yr  = mujeres$p10_11_2,
    first_end_motive_raw    = mujeres$p10_10,
    first_type_raw          = mujeres$p10_12,
    first_cohab_before_flag = mujeres$p10_13,
    first_cohab_mo = mujeres$p10_14_1, first_cohab_yr = mujeres$p10_14_2
  )
  datos$nUnion <- uh$nUnion
  datos <- dplyr::left_join(datos, uh[, setdiff(names(uh), "nUnion")], by = "CaseID")

  emb <- emb[!is.na(emb$ordenhnv), ]
  emb$dob_cmc   <- val_cmc(emb$p5_17_1, emb$p5_17_2)
  emb$dob_cmc_I <- val_imputed(emb$p5_17_1, emb$p5_17_2)
  emb$sex       <- dplyr::coalesce(as.integer(emb$p5_12), as.integer(emb$p5_15))
  emb$order_col <- as.integer(emb$ordenhnv)

  dod <- val_dod(emb$dob_cmc,
                 days_v   = emb$p5_16_1,
                 months_v = emb$p5_16_2,
                 years_v  = emb$p5_16_3)
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_wide <- val_births_wide(emb[, c("CaseID","dob_cmc","dob_cmc_I","sex",
                                          "dod_cmc","dod_cmc_I","order_col")])
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2023 ======
# Files: conjunto_datos_tmujer1/2_enadid_2023.csv, tfechisemb_enadid_2023.csv
# Key:   reconstructed from upm+viv_sel+hogar+n_ren
# Survey date: November 2023
# union_status: 9 levels (code 9 = don't know, new vs 2018)
# p10_* numbering shifted by 1 vs 2018 for lastUnion and firstUnionNotLast
# ═══════════════════════════════════════════════════════════════════════════════

getDatos_ENADID2023_val <- function() {

  mu1_path <- file.path(data_root,
    "2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer1_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer1_enadid_2023.csv")
  mu2_path <- file.path(data_root,
    "2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer2_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer2_enadid_2023.csv")
  emb_path <- file.path(data_root,
    "2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tfechisemb_enadid_2023/conjunto_de_datos/conjunto_datos_tfechisemb_enadid_2023.csv")

  read_csv_int <- function(path) {
    df <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    key <- gsub("[^0-9]", "",
                paste0(df$upm, df$viv_sel, df$hogar, df$n_ren))
    df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.integer(x))))
    df$CaseID <- key
    df
  }
  mu1 <- read_csv_int(mu1_path)
  mu2 <- read_csv_int(mu2_path)
  emb <- read_csv_int(emb_path)
  drop_cols <- c("upm","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog")
  mu1 <- mu1[, setdiff(names(mu1), drop_cols)]
  mu2 <- mu2[, setdiff(names(mu2), drop_cols)]
  emb <- emb[, setdiff(names(emb), drop_cols)]
  mujeres <- dplyr::full_join(mu1, mu2, by = "CaseID")

  dob_mo <- mujeres$p5_1_1
  dob_yr <- as.integer(mujeres$p5_1_2)

  datos <- tibble::tibble(
    country          = "Mexico",
    survey           = "ENADID2023",
    CaseID           = mujeres$CaseID,
    surveyDate_cmc   = val_cmc(11L, 2023L),
    lastYear         = NA_integer_,
    indiv_dob_cmc    = val_cmc(dob_mo, dob_yr),
    indiv_dob_cmc_I  = val_imputed(dob_mo, dob_yr),
    yBirth           = dob_yr,
    indiv_age_survey = mujeres$p5_2_1,
    indiv_weight     = as.integer(round(mujeres$fac_mod.x)),
    union_status     = val_union_status_2023(mujeres$p10_1),
    pregnant         = val_pregnant_std(mujeres$p7_1),
    want_another     = val_want_another_std(mujeres$p7_11, mujeres$p7_7),
    prob_want_another = factor(NA_integer_,
                               levels = PROB_WANT_ANOTHER_LEVELS,
                               labels = PROB_WANT_ANOTHER_LABELS),
    age_first_sex    = dplyr::if_else(as.integer(mujeres$p8_39) %in% c(88L, 98L, 99L),
                                      NA_integer_, as.integer(mujeres$p8_39)),
    ever_contraception = dplyr::case_when(
      as.integer(mujeres$p8_3) == 1L ~ 1L,
      as.integer(mujeres$p8_3) == 2L ~ 0L,
      TRUE ~ NA_integer_),
    nBioKids         = dplyr::coalesce(as.integer(mujeres$p5_9), 0L)
  )

  # NOTE: In 2023 the union section numbering shifted vs 2018:
  #   last union start   = p10_4_1/2 (was p10_3_1/2 in 2018)
  #   last union end     = p10_3_1/2 (was p10_2_1/2)
  #   cohab before flag  = p10_6     (was p10_5)
  #   cohab start        = p10_7_1/2 (was p10_6_1/2)
  #   nUnionBeforeLast   = p10_9     (was p10_8)
  #   first non-last start = p10_10_1/2 (was p10_9_1/2)
  #   first non-last end   = p10_12_1/2 (was p10_11_1/2)
  #   first non-last motive = p10_11   (was p10_10)
  #   first type           = p10_13    (was p10_12)
  #   first cohab before   = p10_14    (was p10_13)
  #   first cohab start    = p10_15_1/2 (was p10_14_1/2)
  uh <- val_build_union_slots_enadid(
    CaseID               = mujeres$CaseID,
    union_status_int     = mujeres$p10_1,
    nUnionBeforeLast     = mujeres$p10_9,
    last_start_mo  = mujeres$p10_4_1, last_start_yr = mujeres$p10_4_2,
    last_end_mo    = mujeres$p10_3_1, last_end_yr   = mujeres$p10_3_2,
    last_cohab_before_flag = mujeres$p10_6,
    last_cohab_mo  = mujeres$p10_7_1, last_cohab_yr = mujeres$p10_7_2,
    first_start_mo = mujeres$p10_10_1, first_start_yr = mujeres$p10_10_2,
    first_end_mo   = mujeres$p10_12_1, first_end_yr   = mujeres$p10_12_2,
    first_end_motive_raw    = mujeres$p10_11,
    first_type_raw          = mujeres$p10_13,
    first_cohab_before_flag = mujeres$p10_14,
    first_cohab_mo = mujeres$p10_15_1, first_cohab_yr = mujeres$p10_15_2
  )
  datos$nUnion <- uh$nUnion
  datos <- dplyr::left_join(datos, uh[, setdiff(names(uh), "nUnion")], by = "CaseID")

  emb <- emb[!is.na(emb$ordenhnv), ]
  emb$dob_cmc   <- val_cmc(emb$p5_17_1, emb$p5_17_2)
  emb$dob_cmc_I <- val_imputed(emb$p5_17_1, emb$p5_17_2)
  emb$sex       <- dplyr::coalesce(as.integer(emb$p5_12), as.integer(emb$p5_15))
  emb$order_col <- as.integer(emb$ordenhnv)

  dod <- val_dod(emb$dob_cmc,
                 days_v   = emb$p5_16_1,
                 months_v = emb$p5_16_2,
                 years_v  = emb$p5_16_3)
  emb$dod_cmc   <- dod$dod_cmc
  emb$dod_cmc_I <- dod$dod_cmc_I

  births_wide <- val_births_wide(emb[, c("CaseID","dob_cmc","dob_cmc_I","sex",
                                          "dod_cmc","dod_cmc_I","order_col")])
  datos <- dplyr::left_join(datos, births_wide, by = "CaseID")
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  n_births <- sum(paste0("dob_cmc", 1:MAX_BIRTHS) %in% names(datos))
  datos <- val_reorder_births(datos, n_births)
  datos <- enforce_schema(datos)
  datos <- val_compute_lastYear(datos)
  datos
}
