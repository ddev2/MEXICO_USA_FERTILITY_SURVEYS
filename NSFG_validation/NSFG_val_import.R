# NSFG_val_import.R ======
# Independent reimplementation of all 12 NSFG survey cycles.
#
# HOW TO USE:
#   1. Copy the PDF codebook for each cycle into NSFG_validation/codebooks/
#      Named as: 1973_codebook.pdf, 1976_codebook.pdf, ..., 2022_23_codebook.pdf
#   2. Open this file in RStudio and run cycle by cycle.
#   3. For each cycle, a Claude session reads the corresponding PDF to verify
#      all byte positions independently before generating the reading code.
#   4. Compare results using NSFG_val_compare.R
#
# OUTPUT SCHEMA (same as original for direct rbind):
#   Fixed columns:    country, survey, CaseID, surveyDate_cmc, lastYear,
#                     indiv_dob_cmc, indiv_dob_cmc_I, yBirth, indiv_age_survey,
#                     indiv_weight, union_status, pregnant, want_another,
#                     prob_want_another, age_first_sex, ever_contraception,
#                     nUnion, nBioKids
#   Union history:    union_start_typeN, union_start_cmcN, union_start_cmc_IN,
#                     marriage_start_cmcN, marriage_start_cmc_IN,
#                     union_end_cmcN, union_end_cmc_IN, union_end_motiveN
#                     (N = 1 to max unions in cycle)
#   Birth history:    dob_cmcN, dob_cmc_IN, sexN  (N = 1 to max births)
#
# CODING CONVENTIONS (independent of original code):
#   - CMC dates:   val_cmc(month, year) → integer, NA if missing
#   - Imputation:  val_imputed(month, year) → 0/1/10/NA
#   - Missing CMC: 9999 reserved for "year unknown"; treated as NA downstream
#   - union_start_type: factor("marriage","cohabitation","cohabitation before marriage")
#   - union_end_motive: factor("in union","widowhood","separation","unknown")
#   - pregnant:         factor("yes","no","refused","unknown")
#   - want_another:     factor("yes","no","disagree","refused","unknown")
#   - All random month imputations: rand_month(n) — one draw per row

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("NSFG_val_lib.R")

# Data file paths — adjust root as needed
data_root <- path.expand(
  "~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/USA/NSFG"
)

codebook_root <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "codebooks")

# Cycle builder template ======
# Each getDatos_XXXX() follows this pattern:
#   1. Read raw data (readLines for .dat, read_sas for .sas7bdat)
#   2. Build datos: individual-level fixed columns
#   3. Build union history (datos_UH), order chronologically
#   4. Build birth history, order chronologically
#   5. Join, enforce schema, return


# 1973 ======
# File: fixed-width .dat
# Sample: ever-married women + single mothers, 15-44
# Union history: up to 6 unions (current + 5 past), NO cohabitation before marriage
# Birth history: up to 17 live births, individual record (no separate birth file)
# CMC coding: 301–900 valid; 901–996 = year known month missing (code - 900 = year offset);
#             997–999 = year unknown (→ 9999)
# Byte positions verified against 1973_codebook.pdf

getDatos_1973_val <- function(childSex = TRUE) {

  path <- file.path(data_root, "1973/1973NSFGData.dat")

  # Local CMC adjuster (1973 convention) =====
  # Positions 901–996: year-only code; last 2 digits = years since 1900
  # e.g. 973 → year 1973, random month assigned per row
  adj_cmc <- function(cmc) {
    n <- length(cmc)
    out <- cmc
    out[cmc < 301]                    <- NA_integer_
    yr_only <- (!is.na(cmc)) & (cmc >= 901 & cmc <= 996)
    out[yr_only] <- (cmc[yr_only] - 900L + 1900L - 1900L) * 12L +
                    rand_month(sum(yr_only, na.rm = TRUE))
    out[cmc >= 997 & cmc <= 999]      <- 9999L
    out
  }

  imp_cmc <- function(cmc) {
    out <- rep(0L, length(cmc))
    out[is.na(cmc)]                   <- NA_integer_
    out[cmc < 301]                    <- NA_integer_
    out[(!is.na(cmc)) & (cmc >= 901 & cmc <= 996)] <- 1L   # month imputed
    out[cmc >= 997 & cmc <= 999]      <- 10L  # year imputed
    out
  }

  raw <- readLines(path)
  n   <- length(raw)

  # Individual-level variables =====
  # Verified positions: CaseID=1-5, surveyDate=709-711 (interview date),
  # dob=31-33 (respondent birthdate month/year CMC), weight=734-739 (F6.0).
  datos <- tibble(
    country         = "USA",
    survey          = "NSFG1973",
    CaseID          = as.integer(substring(raw,   1,  5)),
    surveyDate_cmc  = as.integer(substring(raw, 709, 711)),
    lastYear        = NA_integer_,
    indiv_dob_cmc   = as.integer(substring(raw,  31, 33)),
    indiv_dob_cmc_I = 0L,                                   # exact in 1973
    indiv_weight    = as.integer(substring(raw, 734, 739))
  )
  datos$yBirth           <- val_year_from_cmc(datos$indiv_dob_cmc)
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12L)

  # Marital and pregnancy status =====
  # pos 9: marital status (1=married,2=informal,3=widowed,4=divorced,5=separated,6=SWOC)
  # pos 138: pregnant now (1=yes, 2=no)
  # pos 266: intend another baby (1=yes, 2=no, 3=disagree, 8=DK)
  datos$union_status <- factor(
    as.integer(substring(raw, 9, 9)),
    levels = 1:6,
    labels = c("married", "cohabiting", "widowed", "divorced", "separated",
               "single with own children")
  )
  datos$pregnant <- factor(
    as.integer(substring(raw, 138, 138)),
    levels = PREGNANT_LEVELS, labels = PREGNANT_LABELS
  )
  datos$want_another <- factor(
    as.integer(substring(raw, 266, 266)),
    levels = c(1L, 2L, 3L, 8L), labels = c("yes", "no", "disagree", "unknown")
  )
  datos$prob_want_another <- factor(NA_integer_,
    levels = PROB_WANT_ANOTHER_LEVELS, labels = PROB_WANT_ANOTHER_LABELS)
  datos$age_first_sex      <- NA_integer_
  datos$ever_contraception <- NA_integer_

  # nUnion: pos 45 = ever married before (0=SWOC, 1=multiple, 2=once); pos 46 = count
  tmp <- as.integer(substring(raw, 45, 45))
  datos$nUnion <- dplyr::case_when(
    tmp == 0L ~ 0L,
    tmp == 2L ~ 1L,
    tmp == 1L ~ as.integer(substring(raw, 46, 46)),
    TRUE      ~ NA_integer_
  )

  # Union history =====

  # Current union: pos 47-49 = marriage start month/year CMC; pos 52 = informal union
  # (0=formal/marriage, 1=informal); end date not applicable (emotive = 0, "in union")
  curr_start_cmc   <- adj_cmc(as.integer(substring(raw, 47, 49)))
  curr_start_cmc_I <- imp_cmc(as.integer(substring(raw, 47, 49)))
  curr_start_type  <- as.integer(substring(raw, 52, 52))  # 0=marriage, 1=informal
  curr_marr_cmc    <- dplyr::if_else(curr_start_type == 0L, curr_start_cmc,   NA_integer_)
  curr_marr_cmc_I  <- dplyr::if_else(curr_start_type == 0L, curr_start_cmc_I, NA_integer_)

  # datos_UH holds only union history columns — nUnion stays in datos to avoid
  # a name collision on the left_join that would make it all-NA after enforce_schema.
  datos_UH <- tibble(CaseID = datos$CaseID)

  # Past unions: 5 slots starting at pos 64, each 13 bytes wide.
  # Per-slot layout (verified against 1973_codebook.pdf):
  #   +0..2  : marriage start date (3-byte CMC)      -> pos 64-66 (slot 1)
  #   +3..4  : day of marriage (2 bytes, not read)
  #   +5     : informal union flag (0=formal, 1=informal)
  #   +6     : how marriage ended (1=divorce, 2=separation, 3=death)
  #   +7..9  : marriage end date, divorce/death (3-byte CMC)
  #   +10..12: date stopped living together (3-byte CMC)
  # For divorce(1) or separation(2), use stopLiving as end date.
  dpos <- 0L
  for (u in 1:5) {
    sc_raw  <- as.integer(substring(raw, 64 + dpos, 66 + dpos))
    sc      <- adj_cmc(sc_raw)
    sc_I    <- imp_cmc(sc_raw)
    stype   <- as.integer(substring(raw, 69 + dpos, 69 + dpos))
    emotive <- as.integer(substring(raw, 70 + dpos, 70 + dpos))
    ec_raw  <- as.integer(substring(raw, 71 + dpos, 73 + dpos))
    ec      <- adj_cmc(ec_raw)
    ec_I    <- imp_cmc(ec_raw)
    sl_raw  <- as.integer(substring(raw, 74 + dpos, 76 + dpos))
    sl      <- adj_cmc(sl_raw)
    sl_I    <- imp_cmc(sl_raw)

    # 1973 motive codes at pos +6: 1=divorce/annulment, 2=separation, 3=death.
    # pos +7..9 = legal/official end date (divorce decree or death date).
    # pos +10..12 = date stopped living together.
    # For divorce/annulment (1) and separation (2) the real union end is when
    # they stopped living together, not the later legal date. For widowhood (3)
    # the death date is the true end.
    ec   <- dplyr::if_else(emotive %in% c(1L, 2L), sl,   ec)
    ec_I <- dplyr::if_else(emotive %in% c(1L, 2L), sl_I, ec_I)

    # Remap to schema motive values before storing:
    #   1 (divorce/annulment) -> 2L "separation"
    #   2 (separation)        -> 2L "separation"
    #   3 (death of husband)  -> 1L "widowhood"
    emotive_schema <- dplyr::case_when(
      emotive == 1L ~ 2L,
      emotive == 2L ~ 2L,
      emotive == 3L ~ 1L,
      TRUE          ~ NA_integer_
    )

    datos_UH[[paste0("union_start_cmc",      u)]] <- sc
    datos_UH[[paste0("union_start_cmc_I",    u)]] <- sc_I
    datos_UH[[paste0("union_start_type",     u)]] <- stype
    datos_UH[[paste0("marriage_start_cmc",   u)]] <- dplyr::if_else(stype == 0L, sc,   NA_integer_)
    datos_UH[[paste0("marriage_start_cmc_I", u)]] <- dplyr::if_else(stype == 0L, sc_I, NA_integer_)
    datos_UH[[paste0("union_end_cmc",        u)]] <- ec
    datos_UH[[paste0("union_end_cmc_I",      u)]] <- ec_I
    datos_UH[[paste0("union_end_motive",     u)]] <- emotive_schema

    dpos <- dpos + 13L
  }

  # Place current union in its correct chronological slot.
  # Use datos$nUnion directly (not a column of datos_UH, which no longer carries it).
  nUnion_vec <- datos$nUnion
  for (i in 1:5) {
    sc_col <- paste0("union_start_cmc", i)
    idx <- !is.na(nUnion_vec) & nUnion_vec == i & is.na(datos_UH[[sc_col]])
    if (!any(idx, na.rm = TRUE)) next
    datos_UH[[paste0("union_start_type",     i)]][idx] <- curr_start_type[idx]
    datos_UH[[paste0("union_start_cmc",      i)]][idx] <- curr_start_cmc[idx]
    datos_UH[[paste0("union_start_cmc_I",    i)]][idx] <- curr_start_cmc_I[idx]
    datos_UH[[paste0("marriage_start_cmc",   i)]][idx] <- curr_marr_cmc[idx]
    datos_UH[[paste0("marriage_start_cmc_I", i)]][idx] <- curr_marr_cmc_I[idx]
    datos_UH[[paste0("union_end_cmc",        i)]][idx] <- NA_integer_
    datos_UH[[paste0("union_end_motive",     i)]][idx] <- 0L  # in union
  }

  # Handle 6th union slot if needed (nUnion = 6 means 5 past + 1 current)
  if (max(nUnion_vec, na.rm = TRUE) > 5L) {
    idx <- !is.na(nUnion_vec) & nUnion_vec == 6L
    datos_UH$union_start_type6    <- NA_integer_
    datos_UH$union_start_cmc6     <- NA_integer_
    datos_UH$union_start_cmc_I6   <- NA_integer_
    datos_UH$marriage_start_cmc6  <- NA_integer_
    datos_UH$marriage_start_cmc_I6 <- NA_integer_
    datos_UH$union_end_cmc6       <- NA_integer_
    datos_UH$union_end_cmc_I6     <- NA_integer_
    datos_UH$union_end_motive6    <- NA_integer_
    datos_UH$union_start_type6[idx]    <- curr_start_type[idx]
    datos_UH$union_start_cmc6[idx]     <- curr_start_cmc[idx]
    datos_UH$union_start_cmc_I6[idx]   <- curr_start_cmc_I[idx]
    datos_UH$marriage_start_cmc6[idx]  <- curr_marr_cmc[idx]
    datos_UH$marriage_start_cmc_I6[idx] <- curr_marr_cmc_I[idx]
  }

  # Determine actual max unions present; trim to that many slots
  n_unions <- max(nUnion_vec, na.rm = TRUE)

  # Set "in union" motive for unions with no end date; factorise
  for (u in 1:n_unions) {
    em  <- paste0("union_end_motive", u)
    sc  <- paste0("union_start_cmc",  u)
    ec  <- paste0("union_end_cmc",    u)
    idx <- !is.na(datos_UH[[sc]]) & is.na(datos_UH[[ec]])
    datos_UH[[em]][idx] <- 0L
    datos_UH[[em]] <- factor(datos_UH[[em]],
      levels = UNION_END_MOTIVE_LEVELS,
      labels = UNION_END_MOTIVE_LABELS)
    datos_UH[[paste0("union_start_type", u)]] <- factor(
      datos_UH[[paste0("union_start_type", u)]],
      levels = c(0L, 1L), labels = c("marriage", "cohabitation")
    )
  }

  datos_UH <- val_order_union_history(datos_UH, n_unions)

  # Birth history =====
  # Live births embedded in the main record (not a separate birth file).
  # pos 130-131: number of live births.
  # Birth slots start at pos 1105, each 25 bytes wide (stride = 25).
  # Per-slot layout (verified against 1973_codebook.pdf, pos 1105-1529):
  #   +2..4  : DOB month/year CMC (3-byte)         -> absolute: 1107 + dpos
  #   +8     : sex of child (1=male, 2=female)      -> absolute: 1113 + dpos
  #   +19..21: date child died, month/year CMC (3)  -> absolute: 1124 + dpos
  #            (BLANK when child still living)
  datos$nBioKids <- as.integer(substring(raw, 130, 131))
  datos$nBioKids[is.na(datos$nBioKids)] <- 0L

  maxB <- max(datos$nBioKids, na.rm = TRUE)   # 17 for 1973
  dpos <- 0L
  for (b in 1:maxB) {
    dob_raw <- as.integer(substring(raw, 1107 + dpos, 1109 + dpos))
    datos[[paste0("dob_cmc",   b)]] <- adj_cmc(dob_raw)
    datos[[paste0("dob_cmc_I", b)]] <- imp_cmc(dob_raw)
    if (isTRUE(childSex))
      datos[[paste0("sex", b)]] <- as.integer(substring(raw, 1113 + dpos, 1113 + dpos))
    dod_raw <- as.integer(substring(raw, 1124 + dpos, 1126 + dpos))
    dod_cmc  <- adj_cmc(dod_raw)
    dod_cmc[!is.na(dod_cmc) & dod_cmc == 0L] <- NA_integer_
    datos[[paste0("dod_cmc",   b)]] <- dod_cmc
    datos[[paste0("dod_cmc_I", b)]] <- dplyr::if_else(is.na(dod_cmc), NA_integer_, imp_cmc(dod_raw))
    dpos <- dpos + 25L
  }

  # Assemble =====
  datos <- datos |>
    dplyr::left_join(datos_UH, by = "CaseID")
  datos <- val_compute_lastYear(datos)
  datos <- enforce_schema(datos)

  # Drop all-NA union slots beyond n_unions and birth slots beyond maxB,
  # so the output contains exactly the columns that carry data for this cycle.
  union_drop <- unlist(lapply((n_unions + 1L):MAX_UNIONS, union_cols))
  birth_drop <- unlist(lapply((maxB + 1L):MAX_BIRTHS,    birth_cols))
  keep_cols  <- setdiff(names(datos), c(union_drop, birth_drop))
  datos <- datos[, keep_cols]

  datos
}


# 1976 ======
# File: fixed-width .dat with TWO record types in the same file
#   record_type == 5  → birth/pregnancy interval records (Deck 34-39)
#   record_type != 5  → individual women records (Decks 01-21)
# Union history: current union + 2 past (up to 3 total), NO cohabitation before marriage
# Birth history: in birth interval records (one row per pregnancy, up to 3 babies)
# CMC coding:
#   < 301            → invalid / NA
#   901–924 (close)  → year known, month missing (keep as-is if closeEvent = TRUE)
#   925–976          → year known, month missing → random month
#   997–999          → year unknown → 9999
# Byte positions verified against 1976_codebook.pdf (NCHS Cycle II Tape Contents Manual)

getDatos_1976_val <- function() {

  path <- file.path(data_root, "1976/1976NSFGData.dat")

  # Local CMC adjuster (1976 convention) =====
  adj_cmc <- function(cmc, closeEvent = TRUE) {
    out <- cmc
    out[!is.na(cmc) & cmc < 301]                                     <- NA_integer_
    early_yr_only <- !is.na(cmc) & cmc >= 901 & cmc <= 924 & !closeEvent
    out[early_yr_only] <- (cmc[early_yr_only] - 900L) * 12L +
                          rand_month(sum(early_yr_only))
    late_yr_only <- !is.na(cmc) & cmc >= 925 & cmc <= 976
    out[late_yr_only] <- (cmc[late_yr_only] - 900L) * 12L +
                         rand_month(sum(late_yr_only))
    out[!is.na(cmc) & cmc >= 997 & cmc <= 999]                       <- 9999L
    out
  }

  imp_cmc <- function(cmc, closeEvent = TRUE) {
    out <- rep(0L, length(cmc))
    out[is.na(cmc)]                                                    <- NA_integer_
    out[!is.na(cmc) & cmc < 301]                                      <- NA_integer_
    early_yr_only <- !is.na(cmc) & cmc >= 901 & cmc <= 924 & !closeEvent
    out[early_yr_only]                                                 <- 1L
    out[!is.na(cmc) & cmc >= 925 & cmc <= 976]                        <- 1L
    out[!is.na(cmc) & cmc >= 997 & cmc <= 999]                        <- 10L
    out
  }

  raw_all <- readLines(path)

  # Split record types =====
  # Codebook: tape position 6 = Record Type (per "Key to Documentation" section).
  # Record type codes: 1=CMQ formal, 2=CMQ informal, 3=PMQ, 4=SWOC, 5=B&P interval.
  record_type <- as.integer(substring(raw_all, 6, 6))
  raw_births  <- raw_all[which(record_type == 5L)]
  raw         <- raw_all[which(record_type != 5L)]
  n           <- length(raw)

  # Individual-level variables =====
  # Verified positions:
  #   CaseID:         1-5   (ID number, all decks)
  #   surveyDate_cmc: 709-711 (Deck 21: date interview completed, month CMC)
  #   indiv_dob_cmc:  13-15 (NOTE: see flag below — review against data)
  #   indiv_weight:   720-725 (computer-generated: post-stratified weight WTFINAL)
  #
  # FLAG — indiv_dob_cmc at 13-15: The codebook does not show an explicit
  # respondent birthdate CMC in the questionnaire decks. Tape pos 726-727
  # contains single years of age (AGE, computer-generated). If pos 13-15
  # produces implausible CMCs in practice, replace with an age-based estimate:
  #   indiv_dob_cmc = surveyDate_cmc - as.integer(substring(raw, 726, 727)) * 12L
  datos <- tibble(
    country        = "USA",
    survey         = "NSFG1976",
    CaseID         = as.integer(substring(raw,   1,  5)),
    surveyDate_cmc = adj_cmc(as.integer(substring(raw, 709, 711))),
    lastYear       = NA_integer_,
    indiv_dob_cmc  = adj_cmc(as.integer(substring(raw,  13, 15))),  # FLAG: verify
    indiv_weight   = as.integer(substring(raw, 720, 725))
  )
  datos$indiv_dob_cmc_I  <- imp_cmc(as.integer(substring(raw, 13, 15)))
  datos$yBirth           <- val_year_from_cmc(datos$indiv_dob_cmc)
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12L)

  # Marital and pregnancy status =====
  # union_status: col 10 = record type (1=CMQ formal, 2=CMQ informal, 3=PMQ, 4=SWOC).
  #   Verified: col 10, not col 6 (which is blank in all records).
  # pregnant: tape position 99.
  datos$union_status <- factor(
    as.integer(substring(raw, 6, 6)),
    levels = 1:4,
    labels = c("married", "cohabiting", "widowed/divorced/separated",
               "single with own children")
  )
  datos$pregnant <- factor(
    as.integer(substring(raw, 99, 99)),
    levels = PREGNANT_LEVELS, labels = PREGNANT_LABELS
  )
  datos$want_another      <- factor(NA_integer_, levels = WANT_ANOTHER_LEVELS,      labels = WANT_ANOTHER_LABELS)
  datos$prob_want_another <- factor(NA_integer_, levels = PROB_WANT_ANOTHER_LEVELS, labels = PROB_WANT_ANOTHER_LABELS)
  datos$age_first_sex     <- NA_integer_
  datos$ever_contraception <- NA_integer_

  # Union history =====

  # nUnion: tape pos 26 = A-11 "how many times married"
  datos$nUnion <- as.integer(substring(raw, 26, 26))

  # Current union: tape pos 27-36 (verified against 1976_codebook.pdf Deck 01):
  #   27-29: marriage start month/year CMC (A-12)
  #   30-31: day of marriage (not read)
  #   32:    informal union flag (1=Yes/informal, 2=No/formal marriage)
  #   33:    how most-recent/only marriage ended (3=death, 4=divorce, 5=sep;
  #          blank/0 = still together, i.e. record types 1 & 2)
  #   34-36: marriage end date, death/divorce CMC (A-15/A-16)
  #   37-39: date stopped living together CMC (A-17)
  #          For separation (code 5) this is the true union end; for currently
  #          married women (record types 1 & 2) this field is blank.
  curr_sc_raw      <- as.integer(substring(raw, 27, 29))
  curr_sc          <- adj_cmc(curr_sc_raw)
  curr_sc[curr_sc == 0L] <- NA_integer_
  curr_sc_I        <- imp_cmc(curr_sc_raw)
  curr_stype       <- as.integer(substring(raw, 32, 32))  # 1=informal, 2=formal marriage
  curr_marr        <- dplyr::if_else(curr_stype == 2L, curr_sc,   NA_integer_)
  curr_marr_I      <- dplyr::if_else(curr_stype == 2L, curr_sc_I, NA_integer_)
  curr_how_ended   <- as.integer(substring(raw, 33, 33))  # 3=death,4=divorce,5=sep,NA=together
  curr_ec_raw      <- as.integer(substring(raw, 34, 36))
  curr_ec          <- adj_cmc(curr_ec_raw)
  curr_ec[!is.na(curr_ec) & curr_ec == 0L] <- NA_integer_
  curr_ec_I        <- imp_cmc(curr_ec_raw)
  curr_sl_raw      <- as.integer(substring(raw, 37, 39))
  curr_sl          <- adj_cmc(curr_sl_raw)
  curr_sl[!is.na(curr_sl) & curr_sl == 0L] <- NA_integer_
  curr_sl_I        <- imp_cmc(curr_sl_raw)

  # Use stopped-living date as end date when motive is divorce (4) or separation (5).
  curr_ec   <- dplyr::if_else(curr_how_ended %in% c(4L, 5L), curr_sl,   curr_ec)
  curr_ec_I <- dplyr::if_else(curr_how_ended %in% c(4L, 5L), curr_sl_I, curr_ec_I)

  # Keep raw codes (3=death, 4=divorce, 5=sep); blank/NA → 0 (in union).
  # Do NOT pre-map to intermediate values: the factorisation loop below
  # expects the raw codebook codes c(0,3,4,5,9).
  curr_emotive <- dplyr::if_else(!is.na(curr_how_ended), curr_how_ended, 0L)

  datos_UH <- tibble(CaseID = datos$CaseID)

  # Past unions: 2 slots starting at tape pos 40, stride = 13 bytes.
  # Tape pos 37-39 = stopped-living date for current/most-recent marriage (A-17),
  # so the first past marriage slot starts at pos 40, NOT 37.
  # Per-slot layout (verified against 1976_codebook.pdf Deck 01):
  #   +0..2  : marriage start month/year CMC (3-byte)   → A-13, pos 40-42 (slot 1)
  #   +3..4  : day of marriage (2 bytes, not read)
  #   +5     : informal union flag (1=informal, 2=formal) → A-13, pos 45 (slot 1)
  #   +6     : how marriage ended (3=death, 4=divorce, 5=sep) → A-14, pos 46 (slot 1)
  #   +7..9  : marriage end date, death/divorce CMC      → A-15/A-16, pos 47-49 (slot 1)
  #   +10..12: date stopped living together CMC          → A-17, pos 50-52 (slot 1)
  # Slot 2: pos 53-65 (40 + 13 = 53).
  # For separation (code 5), use stopLiving as end date.
  pos <- 40L
  for (u in 1:2) {
    sc_raw  <- as.integer(substring(raw, pos,       pos + 2L))
    sc      <- adj_cmc(sc_raw)
    sc[sc == 0L] <- NA_integer_
    sc_I    <- imp_cmc(sc_raw)
    stype   <- as.integer(substring(raw, pos + 5L,  pos + 5L))  # 1=informal, 2=formal
    emotive <- as.integer(substring(raw, pos + 6L,  pos + 6L))  # 3=death,4=divorce,5=sep
    ec_raw  <- as.integer(substring(raw, pos + 7L,  pos + 9L))
    ec      <- adj_cmc(ec_raw)
    ec[ec == 0L] <- NA_integer_
    ec_I    <- imp_cmc(ec_raw)
    sl_raw  <- as.integer(substring(raw, pos + 10L, pos + 12L))
    sl      <- adj_cmc(sl_raw)
    sl[sl == 0L] <- NA_integer_
    sl_I    <- imp_cmc(sl_raw)

    # Use stopLiving when motive is divorce (4) or separation (5)
    ec   <- dplyr::if_else(emotive %in% c(4L, 5L), sl,   ec)
    ec_I <- dplyr::if_else(emotive %in% c(4L, 5L), sl_I, ec_I)
    # Set motive to 0 (in union) when start is known but motive is blank
    emotive[!is.na(sc) & is.na(emotive)] <- 0L

    datos_UH[[paste0("union_start_cmc",      u)]] <- sc
    datos_UH[[paste0("union_start_cmc_I",    u)]] <- sc_I
    datos_UH[[paste0("union_start_type",     u)]] <- stype
    datos_UH[[paste0("marriage_start_cmc",   u)]] <- dplyr::if_else(stype == 2L, sc,   NA_integer_)
    datos_UH[[paste0("marriage_start_cmc_I", u)]] <- dplyr::if_else(stype == 2L, sc_I, NA_integer_)
    datos_UH[[paste0("union_end_cmc",        u)]] <- ec
    datos_UH[[paste0("union_end_cmc_I",      u)]] <- ec_I
    datos_UH[[paste0("union_end_motive",     u)]] <- emotive

    pos <- pos + 13L
  }

  # Slot 3 initialised as NA (current union will fill it if nUnion == 3)
  for (col in c("union_start_cmc3", "union_start_cmc_I3", "union_start_type3",
                "marriage_start_cmc3", "marriage_start_cmc_I3",
                "union_end_cmc3", "union_end_cmc_I3", "union_end_motive3"))
    datos_UH[[col]] <- NA_integer_

  # Place current union in its correct chronological slot.
  # Use the count of actually-filled past-union slots (not the declared count from
  # tape pos 26, which may count only past marriages): current union belongs in
  # slot  n_past + 1.  This mirrors mycode's logic and correctly handles women
  # with 2 past unions + 1 active current union (n_past = 2 → slot 3).
  n_unions   <- 3L
  n_past_vec <- rowSums(!is.na(as.data.frame(datos_UH[, paste0("union_start_cmc", 1:2)])))
  for (i in 1:n_unions) {
    sc_col <- paste0("union_start_cmc", i)
    idx <- !is.na(curr_sc) & (n_past_vec + 1L) == i & is.na(datos_UH[[sc_col]])
    if (!any(idx, na.rm = TRUE)) next
    datos_UH[[paste0("union_start_type",     i)]][idx] <- curr_stype[idx]
    datos_UH[[paste0("union_start_cmc",      i)]][idx] <- curr_sc[idx]
    datos_UH[[paste0("union_start_cmc_I",    i)]][idx] <- curr_sc_I[idx]
    datos_UH[[paste0("marriage_start_cmc",   i)]][idx] <- curr_marr[idx]
    datos_UH[[paste0("marriage_start_cmc_I", i)]][idx] <- curr_marr_I[idx]
    datos_UH[[paste0("union_end_cmc",        i)]][idx] <- curr_ec[idx]
    datos_UH[[paste0("union_end_cmc_I",      i)]][idx] <- curr_ec_I[idx]
    datos_UH[[paste0("union_end_motive",     i)]][idx] <- curr_emotive[idx]
  }

  # Recompute nUnion from data (store back into datos to avoid join collision)
  cmc_cols <- paste0("union_start_cmc", 1:n_unions)
  datos$nUnion <- rowSums(!is.na(as.data.frame(datos_UH[, cmc_cols])))

  # Factorise union_end_motive and union_start_type
  # Motive to 0 (in union) when start is known but motive still NA (current-union rows)
  for (u in 1:n_unions) {
    em  <- paste0("union_end_motive", u)
    sc  <- paste0("union_start_cmc",  u)
    ec  <- paste0("union_end_cmc",    u)
    idx <- !is.na(datos_UH[[sc]]) & is.na(datos_UH[[em]])
    datos_UH[[em]][idx] <- 0L
    datos_UH[[em]] <- factor(datos_UH[[em]],
      levels = c(0L, 3L, 4L, 5L, 9L),
      labels = c("in union", "widowhood", "separation", "separation", "unknown"))
    datos_UH[[paste0("union_start_type", u)]] <- factor(
      datos_UH[[paste0("union_start_type", u)]],
      levels = c(1L, 2L, 9L), labels = c("cohabitation", "marriage", "unknown"))
  }

  datos_UH <- val_order_union_history(datos_UH, n_unions)

  # Birth history (from B&P interval records) =====
  # Each B&P record (record_type == 5) covers one pregnancy and may contain
  # up to 3 children. Verified positions (Deck 35/36/37 of the Interval File):
  #   1-5:   CaseID
  #   7-8:   pregnancy number (01-19)
  #   9-11:  date of birth/loss month/year CMC
  #   18-19: child number within pregnancy, 1st child
  #   20:    sex of 1st child (1=male, 2=female)
  #   29-31: date 1st child died, month CMC (0 = still living)
  #   35-36: child number, 2nd child
  #   37:    sex of 2nd child
  #   46-48: date 2nd child died, month CMC
  #   52-53: child number, 3rd child
  #   54:    sex of 3rd child
  #   63-65: date 3rd child died, month CMC
  births_raw <- tibble(
    CaseID        = as.integer(substring(raw_births,  1,  5)),
    pregNumber    = as.integer(substring(raw_births,  7,  8)),
    dob_raw       = as.integer(substring(raw_births,  9, 11)),
    childNumber1  = as.integer(substring(raw_births, 18, 19)),
    babysex1      = as.integer(substring(raw_births, 20, 20)),
    dod_raw1      = as.integer(substring(raw_births, 29, 31)),
    childNumber2  = as.integer(substring(raw_births, 35, 36)),
    babysex2      = as.integer(substring(raw_births, 37, 37)),
    dod_raw2      = as.integer(substring(raw_births, 46, 48)),
    childNumber3  = as.integer(substring(raw_births, 52, 53)),
    babysex3      = as.integer(substring(raw_births, 54, 54)),
    dod_raw3      = as.integer(substring(raw_births, 63, 65))
  )
  births_raw$dob_cmc   <- adj_cmc(births_raw$dob_raw)
  births_raw$dob_cmc_I <- imp_cmc(births_raw$dob_raw)
  # Compute and clean per-child DOD (0 means "still living" in this file)
  dod1 <- adj_cmc(births_raw$dod_raw1); dod1[!is.na(dod1) & dod1 == 0L] <- NA_integer_
  dod2 <- adj_cmc(births_raw$dod_raw2); dod2[!is.na(dod2) & dod2 == 0L] <- NA_integer_
  dod3 <- adj_cmc(births_raw$dod_raw3); dod3[!is.na(dod3) & dod3 == 0L] <- NA_integer_
  births_raw$dod_cmc1   <- dod1
  births_raw$dod_cmc_I1 <- dplyr::if_else(is.na(dod1), NA_integer_, imp_cmc(births_raw$dod_raw1))
  births_raw$dod_cmc2   <- dod2
  births_raw$dod_cmc_I2 <- dplyr::if_else(is.na(dod2), NA_integer_, imp_cmc(births_raw$dod_raw2))
  births_raw$dod_cmc3   <- dod3
  births_raw$dod_cmc_I3 <- dplyr::if_else(is.na(dod3), NA_integer_, imp_cmc(births_raw$dod_raw3))

  # Use childNumber (not babysex) to detect occupied birth slots — more reliable
  # since sex could theoretically be missing even when a child is recorded.
  births_raw$nLiveBirths <- (!is.na(births_raw$childNumber1)) +
                            (!is.na(births_raw$childNumber2)) +
                            (!is.na(births_raw$childNumber3))

  births_raw <- dplyr::filter(births_raw, nLiveBirths > 0L)

  # Pivot to one row per child, then back to wide.
  # babysexN and dod_cmcN / dod_cmc_IN share the same N suffix, so we pivot
  # all three column families simultaneously using a two-name spec.
  births_long <- births_raw |>
    tidyr::pivot_longer(
      cols      = dplyr::matches("^(babysex|dod_cmc|dod_cmc_I)[123]$"),
      names_to  = c(".value", "baby_n"),
      names_pattern = "^(babysex|dod_cmc|dod_cmc_I)([123])$"
    ) |>
    dplyr::rename(sex = babysex) |>
    dplyr::filter(sex %in% 1:2) |>
    dplyr::mutate(
      dob_cmc   = .data$dob_cmc,
      dob_cmc_I = .data$dob_cmc_I
    )

  births_wide <- births_long |>
    dplyr::arrange(CaseID, dob_cmc) |>
    dplyr::group_by(CaseID) |>
    dplyr::mutate(order = dplyr::row_number(), nBioKids = dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols     = c(CaseID, nBioKids),
      names_from  = order,
      values_from = c(dob_cmc, dob_cmc_I, sex, dod_cmc, dod_cmc_I),
      names_sep   = ""
    )

  # Assemble =====
  datos <- datos |>
    dplyr::left_join(births_wide, by = "CaseID") |>
    dplyr::mutate(nBioKids = dplyr::coalesce(nBioKids, 0L)) |>
    dplyr::left_join(datos_UH, by = "CaseID")

  datos <- val_compute_lastYear(datos)
  datos <- enforce_schema(datos)

  # Drop all-NA union/birth slots beyond what this cycle contains
  maxB <- max(datos$nBioKids, na.rm = TRUE)
  union_drop <- unlist(lapply((n_unions + 1L):MAX_UNIONS, union_cols))
  birth_drop <- unlist(lapply((maxB + 1L):MAX_BIRTHS,    birth_cols))
  keep_cols  <- setdiff(names(datos), c(union_drop, birth_drop))
  datos <- datos[, keep_cols]

  datos
}


# Stubs for cycles 1982-2022-23 ======
# Each stub documents what the full implementation needs.
# Upload the corresponding codebook PDF and ask Claude to complete the body.

getDatos_1982_val    <- function() stop("Upload 1982_codebook.pdf to complete this function")
getDatos_1988_val    <- function() stop("Upload 1988_codebook.pdf to complete this function")
getDatos_1995_val    <- function() stop("Upload 1995_codebook.pdf to complete this function")
getDatos_2002_val    <- function() stop("Upload 2002_codebook.pdf to complete this function")
getDatos_2006_10_val <- function() stop("Upload 2006_10_codebook.pdf to complete this function")
getDatos_2011_13_val <- function() stop("Upload 2011_13_codebook.pdf to complete this function")
getDatos_2013_15_val <- function() stop("Upload 2013_15_codebook.pdf to complete this function")
getDatos_2015_17_val <- function() stop("Upload 2015_17_codebook.pdf to complete this function")
getDatos_2017_19_val <- function() stop("Upload 2017_19_codebook.pdf to complete this function")
# 2022-2023 ======
# Files: SAS .sas7bdat (female respondent + pregnancy)
# Sample: all women 15-50 at interview
# Union history: up to 5 marriages + non-marital cohabitations
# Birth history: from pregnancy file (OUTCOME=1), year-only dates
# CMC coding:
#   All event dates are YEAR-ONLY in the public file (month suppressed).
#   → Every CMC is imputed with a random month (imputation flag = 1).
#   → DOB suppressed; approximated from AGER + CMINTVW (indiv_dob_cmc_I = 1).
#   → Child death dates (dod_cmc) not available in public file → all NA.
#   → Current non-marital cohab start year not located in public file → NA.
#   → Only 1st former non-marital cohab has year data (STRTOTH1_Y/STPTOGC1_Y).
# NOTE: LMARSTAT value codes (1=widowed,2=divorced,3=separated,6=never married)
#       inferred from codebook text; verify against actual data on first run.
# Byte/variable positions verified against 2022_23_fem_codebook.txt
#                                      and 2022_23_preg_codebook.txt

getDatos_2022_23_val <- function() {

  # NOTE: adjust filenames below to match the actual downloaded files.
  fem_path  <- file.path(data_root, "2022-23", "NSFG-2022-2023-FemRespPUFData.sas7bdat")
  preg_path <- file.path(data_root, "2022-23", "NSFG-2022-2023-FemPregPUFData.sas7bdat")

  # Year → CMC helpers (public file suppresses month; random imputation) =====
  yr_ok <- function(yr) !is.na(yr) & yr >= 1900L & yr <= 2030L

  yr_to_cmc <- function(yr) {
    out    <- rep(NA_integer_, length(yr))
    ok     <- yr_ok(yr)
    if (any(ok)) out[ok] <- (yr[ok] - 1900L) * 12L + rand_month(sum(ok))
    out
  }

  yr_imp <- function(yr) {
    dplyr::if_else(yr_ok(yr), 1L, NA_integer_)   # 1 = month imputed
  }

  clean_yr <- function(v) {
    x <- as.integer(v)
    dplyr::if_else(yr_ok(x), x, NA_integer_)
  }

  # Read female respondent file =====
  fem <- haven::read_sas(fem_path) |> haven::zap_labels()
  n   <- nrow(fem)

  # Individual-level variables =====
  # DOB is suppressed; approximate indiv_dob_cmc = CMINTVW - AGER*12.
  # This places the birthday in the same month as the interview, AGER years ago.
  # On average overestimates DOB by ~6 months; imputation flag = 1 (month imputed).
  datos <- tibble(
    country         = "USA",
    survey          = "NSFG2022_23",
    CaseID          = as.integer(fem$CaseID),
    surveyDate_cmc  = as.integer(fem$CMINTVW),
    lastYear        = NA_integer_,
    indiv_dob_cmc   = as.integer(fem$CMINTVW) - as.integer(fem$AGER) * 12L,
    indiv_dob_cmc_I = 1L,
    indiv_weight    = as.integer(round(fem$WGT2022_2023))
  )
  datos$yBirth           <- val_year_from_cmc(datos$indiv_dob_cmc)
  datos$indiv_age_survey <- as.integer(fem$AGER)

  # Marital/union status =====
  # MARSTAT (AD-7b): 1=married, 3=cohabiting, 5=neither, 8=refused, 9=dk
  # LMARSTAT (AD-7c, if MARSTAT≠1): 1=widowed, 2=divorced/annulled,
  #   3=separated, 6=never married, 8=refused, 9=dk
  # NOTE: LMARSTAT codes inferred — verify on first run.
  marstat_int  <- as.integer(fem$MARSTAT)
  lmarstat_int <- as.integer(fem$LMARSTAT)
  datos$union_status <- factor(
    dplyr::case_when(
      marstat_int  == 1L ~ 1L,
      marstat_int  == 3L ~ 2L,
      lmarstat_int == 1L ~ 3L,
      lmarstat_int == 2L ~ 4L,
      lmarstat_int == 3L ~ 5L,
      lmarstat_int == 6L ~ 6L,
      TRUE               ~ NA_integer_
    ),
    levels = 1:6,
    labels = c("married", "cohabiting", "widowed", "divorced", "separated",
               "never married")
  )

  # Pregnant =====
  # CURRPREG: 1=yes, 5=no
  datos$pregnant <- factor(
    dplyr::case_when(
      as.integer(fem$CURRPREG) == 1L ~ 1L,
      as.integer(fem$CURRPREG) == 5L ~ 2L,
      TRUE                           ~ NA_integer_
    ),
    levels = PREGNANT_LEVELS, labels = PREGNANT_LABELS
  )

  # Want another child =====
  # RWANT (GA-1): 1=yes, 5=no, 8=refused, 9=dk; inapplicable → NA
  datos$want_another <- factor(
    dplyr::case_when(
      as.integer(fem$RWANT) == 1L ~ 1L,
      as.integer(fem$RWANT) == 5L ~ 2L,
      as.integer(fem$RWANT) == 8L ~ 4L,
      as.integer(fem$RWANT) == 9L ~ 5L,
      TRUE                        ~ NA_integer_
    ),
    levels = WANT_ANOTHER_LEVELS, labels = WANT_ANOTHER_LABELS
  )

  # Prob want another =====
  # PROBWANT (GA-1a): 1=prob want, 2=prob not, 8=refused, 9=dk
  # Applicable if RWANT=9 (don't know)
  datos$prob_want_another <- factor(
    dplyr::case_when(
      as.integer(fem$PROBWANT) == 1L ~ 1L,
      as.integer(fem$PROBWANT) == 2L ~ 2L,
      as.integer(fem$PROBWANT) == 8L ~ 8L,
      as.integer(fem$PROBWANT) == 9L ~ 9L,
      TRUE                           ~ NA_integer_
    ),
    levels = PROB_WANT_ANOTHER_LEVELS, labels = PROB_WANT_ANOTHER_LABELS
  )

  # Age at first sex =====
  # vry1stag: bottom-coded recode (10 = "10 or younger"; 97 = not ascertained)
  # NA if inapplicable (never had sex with a male partner)
  datos$age_first_sex <- dplyr::case_when(
    is.na(fem$vry1stag)                    ~ NA_integer_,
    as.integer(fem$vry1stag) >= 97L        ~ NA_integer_,
    TRUE                                   ~ as.integer(fem$vry1stag)
  )

  # Ever used contraception =====
  # EVERUSED: 1=yes, 5=no, 7=not ascertained
  datos$ever_contraception <- dplyr::case_when(
    as.integer(fem$EVERUSED) == 1L ~ 1L,
    as.integer(fem$EVERUSED) == 5L ~ 0L,
    TRUE                           ~ NA_integer_
  )

  # Number of live births =====
  # PARITY: number of live births (0 if none; NA if not applicable)
  datos$nBioKids <- dplyr::coalesce(as.integer(fem$PARITY), 0L)

  # Number of unions =====
  # TIMESMAR (CB-1) is not released in the public file; infer from the number
  # of non-missing WHMARHX_Y slots (year of marriage to husband 1..5).
  # PREVCOHB (FC C-34): former non-marital male cohab partners (0..4+; 98/99=dk/rf)
  # RMARIT==2: currently in non-marital cohabitation → add 1
  timesmar        <- rowSums(!is.na(
    data.frame(lapply(1:5, function(m) clean_yr(fem[[paste0("WHMARHX_Y_", m)]])))
  ))
  prevcohb_raw    <- as.integer(fem$PREVCOHB)
  prevcohb        <- dplyr::if_else(prevcohb_raw %in% c(98L, 99L),
                                    NA_integer_, prevcohb_raw)
  prevcohb        <- dplyr::coalesce(prevcohb, 0L)
  rmarit_int      <- as.integer(fem$RMARIT)
  curr_cohab_flag <- as.integer(!is.na(rmarit_int) & rmarit_int == 2L)
  datos$nUnion    <- timesmar + prevcohb + curr_cohab_flag

  # Union history =====
  # Marriage slots 1-5:
  #   WHMARHX_Y_m  : year married to mth husband (CB-3y; year only)
  #   STRTOGHX_Y_m : year began premarital cohabitation with mth husband (CB-7y)
  #   MARENDHX_m   : how marriage ended (CB-17; 1=death, 2=divorce, 3=annulment,
  #                   8=refused, 9=dk; NA/inapplicable = ongoing/current)
  #   WNSTPHX_Y_m  : year stopped living together with mth husband (CB-19y)
  #                   (asked for divorce/separation; NA for widowhood)
  # Non-marital cohabitation slots:
  #   1st former cohab (PREVCOHB>=1): STRTOTH1_Y, STPTOGC1_Y
  #   Current cohab (RMARIT==2): start year from WNSTRTCP_Y
  #   2nd+ former cohab: dates not in public file → NA

  datos_UH <- tibble(CaseID = datos$CaseID)

  # Marriage slots 1-5
  for (m in 1:5) {
    mar_y <- clean_yr(fem[[paste0("WHMARHX_Y_", m)]])
    coh_y <- clean_yr(fem[[paste0("STRTOGHX_Y_", m)]])
    stp_y <- if (m <= 4L)
               clean_yr(fem[[paste0("WNSTPHX_Y_", m)]])
             else
               rep(NA_integer_, n)   # 5th husband stop-year not in public file
    how   <- as.integer(fem[[paste0("MARENDHX_", m)]])

    has_mar <- !is.na(mar_y)
    has_coh <- !is.na(coh_y)

    # Union start: cohabitation year if premarital cohab present, else marriage year
    start_y <- dplyr::if_else(has_coh, coh_y, mar_y)
    sc      <- yr_to_cmc(start_y)
    sc_I    <- yr_imp(start_y)
    mc      <- yr_to_cmc(mar_y)
    mc_I    <- yr_imp(mar_y)

    # Union start type: 1=marriage, 3=cohabitation before marriage
    utype <- dplyr::case_when(
      has_mar & has_coh ~ 3L,
      has_mar           ~ 1L,
      TRUE              ~ NA_integer_
    )

    # Union end: WNSTPHX_Y (year stopped living together).
    # NOTE: for widowed unions (MARENDHX=1), WNSTPHX_Y is NA (not asked);
    #       union_end_cmc will be NA for those rows — a known public-file limitation.
    ec   <- yr_to_cmc(stp_y)
    ec_I <- yr_imp(stp_y)

    # End motive (from MARENDHX raw codes):
    #   1=death of husband → widowhood, 2=divorce → separation, 3=annulment → separation
    #   NA with end date   → separation (union ended, reason not recorded)
    #   NA without end date → "in union" (ongoing)
    emotive <- dplyr::case_when(
      !has_mar              ~ NA_integer_,
      how == 1L             ~  1L,   # widowhood
      how %in% c(2L, 3L)   ~  2L,   # separation (divorce / annulment)
      !is.na(ec)            ~  2L,   # end date present but motive blank → separation
      is.na(how)            ~  0L,   # no end date, no motive → in union (ongoing)
      TRUE                  ~  9L    # unknown (refused / dk, no end date)
    )

    datos_UH[[paste0("union_start_type",      m)]] <- utype
    datos_UH[[paste0("union_start_cmc",       m)]] <- sc
    datos_UH[[paste0("union_start_cmc_I",     m)]] <- sc_I
    datos_UH[[paste0("marriage_start_cmc",    m)]] <- mc
    datos_UH[[paste0("marriage_start_cmc_I",  m)]] <- mc_I
    datos_UH[[paste0("union_end_cmc",         m)]] <- ec
    datos_UH[[paste0("union_end_cmc_I",       m)]] <- ec_I
    datos_UH[[paste0("union_end_motive",      m)]] <- emotive
  }

  # 1st former non-marital cohabitation (slot 6; chronological sort will reorder)
  has_cohb1 <- prevcohb >= 1L
  c1_sc     <- yr_to_cmc(clean_yr(fem$STRTOTH1_Y))
  c1_sc_I   <- yr_imp(as.integer(fem$STRTOTH1_Y))
  c1_ec     <- yr_to_cmc(clean_yr(fem$STPTOGC1_Y))
  c1_ec_I   <- yr_imp(as.integer(fem$STPTOGC1_Y))

  datos_UH$union_start_type6     <- dplyr::if_else(has_cohb1, 2L,    NA_integer_)
  datos_UH$union_start_cmc6      <- dplyr::if_else(has_cohb1, c1_sc, NA_integer_)
  datos_UH$union_start_cmc_I6    <- dplyr::if_else(has_cohb1, c1_sc_I, NA_integer_)
  datos_UH$marriage_start_cmc6   <- NA_integer_
  datos_UH$marriage_start_cmc_I6 <- NA_integer_
  datos_UH$union_end_cmc6        <- dplyr::if_else(has_cohb1, c1_ec, NA_integer_)
  datos_UH$union_end_cmc_I6      <- dplyr::if_else(has_cohb1, c1_ec_I, NA_integer_)
  datos_UH$union_end_motive6     <- dplyr::if_else(has_cohb1, 2L,    NA_integer_)  # ended

  # NOTE: 2nd+ former non-marital cohab dates absent from public file → slots omitted.

  # Current non-marital cohabitation (slot 7)
  # WNSTRTCP_Y: year began cohabiting with current male partner.
  curr_cohab_yr  <- clean_yr(fem$WNSTRTCP_Y)
  datos_UH$union_start_type7     <- dplyr::if_else(curr_cohab_flag == 1L, 2L, NA_integer_)
  datos_UH$union_start_cmc7      <- dplyr::if_else(curr_cohab_flag == 1L, yr_to_cmc(curr_cohab_yr), NA_integer_)
  datos_UH$union_start_cmc_I7    <- dplyr::if_else(curr_cohab_flag == 1L, yr_imp(curr_cohab_yr),    NA_integer_)
  datos_UH$marriage_start_cmc7   <- NA_integer_
  datos_UH$marriage_start_cmc_I7 <- NA_integer_
  datos_UH$union_end_cmc7        <- NA_integer_   # ongoing
  datos_UH$union_end_cmc_I7      <- NA_integer_
  datos_UH$union_end_motive7     <- dplyr::if_else(curr_cohab_flag == 1L, 0L, NA_integer_)

  n_slots <- 7L

  # Factorise union columns; assign "in union" / "separation" where motive is still NA
  for (u in 1:n_slots) {
    em  <- paste0("union_end_motive", u)
    sc  <- paste0("union_start_cmc",  u)
    ec  <- paste0("union_end_cmc",    u)
    has_start  <- !is.na(datos_UH[[sc]])
    has_end    <- ec %in% names(datos_UH) & !is.na(datos_UH[[ec]])
    still_na   <- is.na(datos_UH[[em]])
    # Union started, no end date, motive unknown → ongoing ("in union")
    datos_UH[[em]][has_start &  has_end & still_na] <- 2L  # separation
    datos_UH[[em]][has_start & !has_end & still_na] <- 0L  # in union
    datos_UH[[em]] <- factor(
      datos_UH[[em]],
      levels = UNION_END_MOTIVE_LEVELS,
      labels = UNION_END_MOTIVE_LABELS
    )
    datos_UH[[paste0("union_start_type", u)]] <- factor(
      datos_UH[[paste0("union_start_type", u)]],
      levels = UNION_START_TYPE_LEVELS,
      labels = UNION_START_TYPE_LABELS
    )
  }

  datos_UH <- val_order_union_history(datos_UH, n_slots)

  # Recompute nUnion from filled slots after ordering.
  # timesmar + prevcohb + curr_cohab_flag overcounts when prevcohb >= 2 because
  # the public file only releases dates for the 1st former non-marital cohab
  # (STRTOTH1_Y / STPTOGC1_Y); 2nd+ former cohabs have no history data.
  # Counting non-NA union_start_cmc columns gives the true number of slots with data.
  cmc_cols       <- paste0("union_start_cmc", 1:n_slots)
  datos$nUnion   <- rowSums(!is.na(as.data.frame(datos_UH[, cmc_cols])))

  # Birth history (from pregnancy file) =====
  # OUTCOME=1: live birth. DATEND: year of birth (year-only in public file).
  # BABYSEX: 1=male, 2=female (NA for multiple births / refused / dk).
  # dod_cmc: child death dates not available in public file → all NA.
  preg <- haven::read_sas(preg_path) |> haven::zap_labels()

  births <- preg |>
    dplyr::filter(as.integer(OUTCOME) == 1L) |>
    dplyr::transmute(
      CaseID    = as.integer(CaseID),
      birth_yr  = clean_yr(as.integer(DATEND)),
      dob_cmc   = yr_to_cmc(birth_yr),
      dob_cmc_I = dplyr::if_else(!is.na(birth_yr), 1L, NA_integer_),
      sex       = dplyr::if_else(
        as.integer(BABYSEX) %in% c(1L, 2L),
        as.integer(BABYSEX), NA_integer_
      )
    ) |>
    dplyr::arrange(CaseID, dob_cmc) |>
    dplyr::group_by(CaseID) |>
    dplyr::mutate(birth_order = dplyr::row_number()) |>
    dplyr::ungroup()

  maxB <- max(datos$nBioKids, na.rm = TRUE)

  births_wide <- births |>
    tidyr::pivot_wider(
      id_cols     = CaseID,
      names_from  = birth_order,
      values_from = c(dob_cmc, dob_cmc_I, sex),
      names_sep   = ""
    )

  # dod_cmc not available in 2022-23 public file
  for (b in seq_len(maxB)) {
    births_wide[[paste0("dod_cmc",   b)]] <- NA_integer_
    births_wide[[paste0("dod_cmc_I", b)]] <- NA_integer_
  }

  # Assemble =====
  datos <- datos |>
    dplyr::left_join(births_wide, by = "CaseID") |>
    dplyr::left_join(datos_UH,   by = "CaseID")

  datos <- val_compute_lastYear(datos)
  datos <- enforce_schema(datos)

  n_unions_out <- min(max(datos$nUnion, na.rm = TRUE), MAX_UNIONS)
  maxB_out     <- max(datos$nBioKids,  na.rm = TRUE)
  union_drop   <- unlist(lapply((n_unions_out + 1L):MAX_UNIONS, union_cols))
  birth_drop   <- unlist(lapply((maxB_out    + 1L):MAX_BIRTHS,  birth_cols))
  keep_cols    <- setdiff(names(datos), c(union_drop, birth_drop))
  datos        <- datos[, keep_cols]

  datos
}


# Run and save ======
# Uncomment as each cycle is completed and validated

NSFG_VAL_1973 <- getDatos_1973_val()
NSFG_VAL_1973 <- val_clean(NSFG_VAL_1973)
NSFG_VAL_1973 <- val_reorder_births(NSFG_VAL_1973, max(NSFG_VAL_1973$nBioKids))

NSFG_VAL_1976 <- getDatos_1976_val()
NSFG_VAL_2022_23 <- getDatos_2022_23_val()
# NSFG_VAL_1976 <- val_clean(NSFG_VAL_1976)
# NSFG_VAL_1976 <- val_reorder_births(NSFG_VAL_1976, max(NSFG_VAL_1976$nBioKids))

# val_check_bind(NSFG_VAL_1973, NSFG_VAL_1976)
# NSFG_VAL <- dplyr::bind_rows(NSFG_VAL_1973, NSFG_VAL_1976)
# save(NSFG_VAL, file = "NSFG_VAL.Rdat")
