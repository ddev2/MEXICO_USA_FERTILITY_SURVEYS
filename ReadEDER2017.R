setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.r")
# Read EDER 2017 file
# imputed_month_capped() is defined in enadid_lib.r
library(tidyverse)
library(haven)
library(purrr)
library(data.table)

if (exists("DEBUG")) browser()

path_EDER2017              <- path.expand(paste0(rootPath, "/INEGI/Encuestas/EDER/2017/eder2017_bases_sav/historiavida.sav"))
path_EDER2017_antecedentes <- path.expand(paste0(rootPath, "/INEGI/Encuestas/EDER/2017/eder2017_bases_sav/antecedentes.sav"))
path_EDER_ENADID2017       <- path.expand(paste0(rootPath, "/INEGI/Encuestas/ENADID/EDER_ENADID2017.Rdat"))


# ==== 1. Load & merge historiavida + antecedentes ====

if (!exists("EDER")) {
  EDER <- haven::read_sav(path_EDER2017)
} else {
  EDER$factor_per <- NULL
}

EDER$llave_muj <- paste0(EDER$folioviv, EDER$foliohog, EDER$id_pobla)

antecedentes <- haven::read_sav(path_EDER2017_antecedentes)
antecedentes$llave_muj <- paste0(antecedentes$folioviv, antecedentes$foliohog, antecedentes$id_pobla)

EDER <- EDER %>%
  dplyr::left_join(
    dplyr::select(antecedentes, llave_muj, factor_per),
    by = "llave_muj"
  )
rm(antecedentes)

EDER <- haven::zap_labels(EDER)

# Keep name columns as character; convert everything else to integer
to_keep_char <- c(
  "llave_muj", "folioviv", "foliohog", "id_pobla", "geo_eder",
  "nom_cony1", "nom_cony2", "nom_cony3", "nom_cony4", "nom_cony5", "nom_cony6",
  paste0("hij_nom_", 1:15)
)

EDER <- EDER %>%
  dplyr::mutate(across(
    .cols = -all_of(to_keep_char),
    .fns  = as.integer
  ))

# Restrict to women
EDER <- subset(EDER, sexo == 2)

# One row per woman (for individual-level variables)
EDER_one <- EDER[!duplicated(EDER$llave_muj), ]


# ==== 2. Initialise output dataframe ====

# Survey field period: 3 Jul - 31 Dec 2017. We cut at December 2017 to
# retain all women, but year 2017 will not be used in exposure calculations.
n <- nrow(EDER_one)
survey_cmc_eder17 <- compute_cmc(12L, 2017L)

EDER_ENADID <- data.frame(country = rep("MEXICO", n), survey = rep("EDER2017", n))
EDER_ENADID$llave_muj          <- EDER_one$llave_muj
EDER_ENADID$surveyDate_cmc     <- survey_cmc_eder17
EDER_ENADID                    <- compute_lastYear(EDER_ENADID)
EDER_ENADID$indiv_dob_cmc      <- compute_cmc(EDER_one$mes_nac, EDER_one$anio_nac)
EDER_ENADID$yBirth             <- EDER_one$anio_nac
EDER_ENADID$indiv_age_survey   <- EDER_one$edad_act
EDER_ENADID$indiv_weight       <- EDER_one$factor_per
EDER_ENADID$pregnant           <- NA
EDER_ENADID$want_another       <- NA
EDER_ENADID$age_first_sex      <- EDER_one$edad_sex
EDER_ENADID$ever_contraception <- NA
EDER_ENADID$nUnion             <- EDER_one$matrimonio


# ==== 3. Union history ====

# edo_civil codes — 2017 uses compound codes when two or three events
# occur in the same calendar year. Verified against the data; correct
# meanings confirmed by diagnostic output.
#
# Simple codes:
#   0   Soltero / never in union (excluded by code > 0 filter)
#   1   Inicio union libre
#   2   Inicio matrimonio civil        -> next state 20
#   3   Inicio matrimonio religioso    -> next state 30
#   4   Inicio matrimonio civil y religioso -> next state 40
#   6   Inicio divorcio
#   7   Inicio separacion
#   8   Fallecimiento del conyuge
#  10   Union libre (ongoing)
#  20   Matrimonio civil (ongoing)
#  30   Matrimonio religioso (ongoing)
#  40   Matrimonio civil y religioso (ongoing)
#  60   Esta divorciado
#  70   Esta separado
#  80   Esta viudo
#
# Compound codes — two events same year (verified: no prior code 1 exists;
# both Ustart and Mstart encoded in this single row):
#  12   Inicio union libre + inicio matrimonio civil     -> next state 20
#  13   Inicio union libre + inicio matrimonio religioso -> next state 30
#  14   Inicio union libre + inicio matrim. civil y rel. -> next state 40
#  17   Inicio union libre + inicio separacion
#  18   Inicio union libre + fallecimiento del conyuge
#  26   Inicio matrimonio civil + inicio divorcio
#  27   Inicio matrimonio civil + inicio separacion
#  28   Inicio matrimonio civil + fallecimiento del conyuge
#  37   Matrimonio religioso (ongoing) + inicio separacion
#  46   Matrimonio civil y religioso (ongoing) + inicio divorcio
#  47   Matrimonio civil y religioso (ongoing) + inicio separacion
#  48   Matrimonio civil y religioso (ongoing) + fallecimiento
#
# Compound code — three events same year:
# 126   Inicio union libre + inicio matrimonio civil + inicio divorcio
#
# Imputation rules derived from the code semantics (verified by diagnostics):
#
#   Code 1        Ustart 1:12 independent; Mstart independent (different year or NA)
#   Code 2,3,4    Ustart = Mstart (single draw, same CMC)
#   Code 12,13,14 Ustart 1:11; Mstart Ustart_month+1:12 (same year, ordered)
#   Code 17,18    Ustart 1:11; Uend Ustart_month+1:12; Mstart NA
#   Code 26,27,28 Mstart 1:11; Uend Mstart_month+1:12;
#                 Ustart = Mstart if no prior union row, else Ustart from prior year
#   Code 37,46,   Uend independent; Mstart from prior year;
#        47,48    Ustart = Mstart if no prior union row, else Ustart from prior year
#   Code 126      Ustart 1:4; Mstart Ustart_month+1:8; Uend Mstart_month+1:12
#
# Dissolution type: separation, widowhood, and divorce are mutually exclusive
# in the data (confirmed by diagnostic). Type taken from whichever code appears.
# Divorce is used only when no separation or widowhood code is present.

# Code sets used in extract_union_data()
.COHAB_START  <- c(1L, 12L, 13L, 14L, 17L, 18L, 126L)  # union starts as cohabitation
.MARR_START   <- c(2L, 3L, 4L, 12L, 13L, 14L,           # marriage event present
                   26L, 27L, 28L, 37L, 40L,
                   46L, 47L, 48L, 126L)
.SEP_CODES    <- c(7L, 17L, 27L, 37L, 47L, 70L)         # separation
.WID_CODES    <- c(8L, 18L, 28L, 48L, 80L)              # widowhood
.DIV_CODES    <- c(6L, 26L, 46L, 60L, 126L)             # divorce
.DISS_CODES   <- c(.SEP_CODES, .WID_CODES, .DIV_CODES)

# Compound codes requiring same-year month-ordering in the CMC loop
.COMP_UM      <- c(12L, 13L, 14L)      # Ustart H1, Mstart H2
.COMP_UE      <- c(17L, 18L)           # Ustart H1, Uend H2
.COMP_ME      <- c(26L, 27L, 28L)      # Mstart H1, Uend H2; Ustart = Mstart if no prior row
.COMP_E_ONLY  <- c(37L, 46L, 47L, 48L) # Uend only; Ustart = Mstart if no prior row
.COMP_UME     <- c(126L)               # Ustart T1, Mstart T2, Uend T3

extract_union_data <- function(codes, years) {
  active_idx <- which(!is.na(codes) & codes > 0L)

  if (length(active_idx) == 0L) {
    return(list(
      type         = NA_character_,
      start_u      = NA_integer_,
      start_u_code = NA_integer_,
      start_m      = NA_integer_,
      start_m_code = NA_integer_,
      end_u        = NA_integer_,
      diss_type    = "None"
    ))
  }

  # --- Union start ---
  first_idx        <- min(active_idx)
  start_union_yr   <- years[first_idx]
  start_union_code <- codes[first_idx]

  # For codes 26/27/28/37/46/47/48 the union (and marriage) was already
  # ongoing before the compound event year — the compound code records only
  # a dissolution (and for 26/27/28 a marriage start), not a union start.
  # If no prior non-dissolution row exists before the compound code year,
  # the true union start year is unknown. We return start_u = NA so the
  # CMC loop's `is.na(Ustart)` check correctly sets Ustart = Mstart.
  .NO_USTART_CODES <- c(26L, 27L, 28L, 37L, 46L, 47L, 48L)
  if (start_union_code %in% .NO_USTART_CODES) {
    comp_yr         <- years[first_idx]
    prior_non_diss  <- which(!is.na(codes) & codes > 0L &
                               years < comp_yr &
                               !(codes %in% .DISS_CODES))
    if (length(prior_non_diss) == 0L) {
      start_union_yr <- NA_integer_
    }
  }

  # --- Marriage start ---
  # The marriage year is the year of the first code that implies a marriage.
  # For codes 2/3/4 the marriage starts the same year as the union (Mstart = Ustart).
  # For compound codes 12/13/14/26/27/28/37/46/47/48/126 a marriage also
  # starts or was already ongoing; take the first such year.
  marr_idx        <- which(codes %in% .MARR_START)
  start_marr_yr   <- if (length(marr_idx) > 0L) years[min(marr_idx)] else NA_integer_
  start_marr_code <- if (length(marr_idx) > 0L) codes[min(marr_idx)] else NA_integer_

  # --- Union type ---
  starts_with_cohab <- start_union_code %in% .COHAB_START
  has_marriage      <- !is.na(start_marr_yr)

  u_type <- dplyr::case_when(
    starts_with_cohab &  has_marriage ~ "cohabitation before marriage",
    starts_with_cohab & !has_marriage ~ "cohabitation",
    !starts_with_cohab & has_marriage ~ "marriage",
    TRUE                              ~ NA_character_
  )

  # --- Union end ---
  # Dissolution types are mutually exclusive in the data. Type is taken from
  # whichever dissolution code is present. end_union_yr is the year of the
  # first dissolution code regardless of type.
  diss_idx <- which(codes %in% .DISS_CODES)

  if (length(diss_idx) > 0L) {
    end_union_yr <- years[min(diss_idx)]
    # Determine type from all dissolution codes present (not just the first),
    # in case a state code (60/70/80) appears before the transition code.
    diss_codes_present <- codes[diss_idx]
    d_type <- dplyr::case_when(
      any(diss_codes_present %in% .SEP_CODES) ~ "separation",
      any(diss_codes_present %in% .WID_CODES) ~ "widowhood",
      any(diss_codes_present %in% .DIV_CODES) ~ "divorce",
      TRUE ~ "in union"
    )
  } else {
    end_union_yr <- NA_integer_
    d_type       <- "in union"
  }

  return(list(
    type         = u_type,
    start_u      = start_union_yr,
    start_u_code = start_union_code,
    start_m      = start_marr_yr,
    start_m_code = start_marr_code,
    end_u        = end_union_yr,
    diss_type    = d_type
  ))
}

df_unions_final <- EDER %>%
  dplyr::group_by(llave_muj) %>%
  dplyr::group_modify(~ {
    data_woman <- .x
    purrr::map_dfc(1:5, function(i) {
      col_name <- paste0("edo_civil", i)
      res <- extract_union_data(data_woman[[col_name]], data_woman$anio_retro)
      purrr::set_names(res, c(
        paste0("union_start_type",  i),
        paste0("yStartUnion_",      i),
        paste0("yStartUnionCode_",  i),
        paste0("yStartMarr_",       i),
        paste0("yStartMarrCode_",   i),
        paste0("yEndUnion_",        i),
        paste0("union_end_motive",  i)
      ))
    })
  }) %>%
  dplyr::ungroup()

# Join surveyDate_cmc so imputed_month_capped() can use it as upper bound
df_unions_final$surveyDate_cmc <- survey_cmc_eder17

# Convert year columns to CMC.
#
# The imputation is fully driven by the start-of-union code, which encodes
# exactly which events occurred and their temporal ordering within the year.
#
# General path (runs first for all women):
#   Ustart: draw 1:12 capped at survey_cmc
#   Mstart: independent draw 1:12 capped at survey_cmc
#   Uend:   computed AFTER compound blocks (see below)
#
# Compound-code overrides (same-year events — run after general path):
#   2/3/4     Mstart = Ustart (copy, same CMC)
#   12/13/14  Ustart 1:11; Mstart after Ustart
#   17/18     Ustart 1:11; Uend after Ustart
#   26/27/28  Mstart 1:11; Uend after Mstart;
#             if Ustart is NA (no prior row), set Ustart = Mstart
#   37/46/47/48  Uend via general path;
#             if Ustart is NA (no prior row), set Ustart = Mstart
#   126       Ustart 1:4; Mstart after Ustart; Uend after Mstart
#
# Uend general path (runs last, after all compound blocks):
#   Fills only cases not already set by a compound block.
#   after_cmc = pmax(Ustart, Mstart) — only binds in the same year.

for (u in seq_len(5)) {
  Ustart     <- paste0("union_start_cmc",      u)
  Ustart_I   <- paste0("union_start_cmc_I",    u)
  UstartType <- paste0("union_start_type",     u)
  Uend       <- paste0("union_end_cmc",        u)
  Uend_I     <- paste0("union_end_cmc_I",      u)
  UendMotive <- paste0("union_end_motive",     u)
  Mstart     <- paste0("marriage_start_cmc",   u)
  Mstart_I   <- paste0("marriage_start_cmc_I", u)

  Ustart_year <- paste0("yStartUnion_",     u)
  Ustart_code <- paste0("yStartUnionCode_", u)
  Uend_year   <- paste0("yEndUnion_",       u)
  Mstart_year <- paste0("yStartMarr_",      u)
  Mstart_code <- paste0("yStartMarrCode_",  u)

  N        <- nrow(df_unions_final)
  u_yrs    <- df_unions_final[[Ustart_year]]
  e_yrs    <- df_unions_final[[Uend_year]]
  m_yrs    <- df_unions_final[[Mstart_year]]
  surv_cmc <- df_unions_final$surveyDate_cmc
  u_codes  <- df_unions_final[[Ustart_code]]

  # --- General path: independent draws for Ustart and Mstart ---

  df_unions_final[[Ustart]] <- ifelse(
    !is.na(u_yrs),
    compute_cmc(imputed_month_capped(N, u_yrs, survey_cmc = surv_cmc), u_yrs),
    NA_integer_
  )

  df_unions_final[[Mstart]] <- ifelse(
    !is.na(m_yrs),
    compute_cmc(imputed_month_capped(N, m_yrs, survey_cmc = surv_cmc), m_yrs),
    NA_integer_
  )

  # Codes 2/3/4: union starts as a marriage — Mstart gets same CMC as Ustart
  idx_marr_only <- !is.na(u_codes) & u_codes %in% c(2L, 3L, 4L)
  if (any(idx_marr_only))
    df_unions_final[[Mstart]][idx_marr_only] <- df_unions_final[[Ustart]][idx_marr_only]

  # --- Compound-code overrides ---
  # Uend general path runs AFTER these blocks so after_cmc uses corrected values.

  # 12/13/14: Ustart and Mstart same year, Ustart < Mstart
  idx <- !is.na(u_codes) & u_codes %in% .COMP_UM
  if (any(idx)) {
    n <- sum(idx)
    df_unions_final[[Ustart]][idx] <- compute_cmc(
      imputed_month_capped(n, u_yrs[idx], survey_cmc = surv_cmc[idx], range = 1:11),
      u_yrs[idx])
    df_unions_final[[Mstart]][idx] <- compute_cmc(
      imputed_month_capped(n, m_yrs[idx], survey_cmc = surv_cmc[idx],
                           after_cmc = df_unions_final[[Ustart]][idx], range = 2:12),
      m_yrs[idx])
  }

  # 17/18: Ustart and Uend same year, Ustart < Uend, Mstart NA
  idx <- !is.na(u_codes) & u_codes %in% .COMP_UE
  if (any(idx)) {
    n <- sum(idx)
    df_unions_final[[Ustart]][idx] <- compute_cmc(
      imputed_month_capped(n, u_yrs[idx], survey_cmc = surv_cmc[idx], range = 1:11),
      u_yrs[idx])
    df_unions_final[[Uend]][idx] <- compute_cmc(
      imputed_month_capped(n, e_yrs[idx], survey_cmc = surv_cmc[idx],
                           after_cmc = df_unions_final[[Ustart]][idx], range = 2:12),
      e_yrs[idx])
  }

  # 26/27/28: Mstart and Uend are both new events this year, Mstart < Uend.
  # Ustart: left untouched if it has a value from a prior year; set to Mstart
  # if NA (no prior union row — union starts as a marriage this year).
  idx <- !is.na(u_codes) & u_codes %in% .COMP_ME
  if (any(idx)) {
    n <- sum(idx)
    df_unions_final[[Mstart]][idx] <- compute_cmc(
      imputed_month_capped(n, m_yrs[idx], survey_cmc = surv_cmc[idx], range = 1:11),
      m_yrs[idx])
    df_unions_final[[Uend]][idx] <- compute_cmc(
      imputed_month_capped(n, e_yrs[idx], survey_cmc = surv_cmc[idx],
                           after_cmc = df_unions_final[[Mstart]][idx], range = 2:12),
      e_yrs[idx])
    idx_no_ustart <- idx & is.na(df_unions_final[[Ustart]])
    if (any(idx_no_ustart))
      df_unions_final[[Ustart]][idx_no_ustart] <- df_unions_final[[Mstart]][idx_no_ustart]
  }

  # 37/46/47/48: only Uend is a new event this year; Mstart from a prior year.
  # Ustart: set to Mstart if NA (no prior union row).
  # Uend: handled by the general path below (after_cmc = pmax(Ustart, Mstart)).
  idx <- !is.na(u_codes) & u_codes %in% .COMP_E_ONLY
  if (any(idx)) {
    idx_no_ustart <- idx & is.na(df_unions_final[[Ustart]])
    if (any(idx_no_ustart))
      df_unions_final[[Ustart]][idx_no_ustart] <- df_unions_final[[Mstart]][idx_no_ustart]
  }

  # 126: Ustart, Mstart, Uend all same year, strictly ordered in thirds
  idx <- !is.na(u_codes) & u_codes %in% .COMP_UME
  if (any(idx)) {
    n <- sum(idx)
    df_unions_final[[Ustart]][idx] <- compute_cmc(
      imputed_month_capped(n, u_yrs[idx], survey_cmc = surv_cmc[idx], range = 1:4),
      u_yrs[idx])
    df_unions_final[[Mstart]][idx] <- compute_cmc(
      imputed_month_capped(n, m_yrs[idx], survey_cmc = surv_cmc[idx],
                           after_cmc = df_unions_final[[Ustart]][idx], range = 5:8),
      m_yrs[idx])
    df_unions_final[[Uend]][idx] <- compute_cmc(
      imputed_month_capped(n, e_yrs[idx], survey_cmc = surv_cmc[idx],
                           after_cmc = df_unions_final[[Mstart]][idx], range = 9:12),
      e_yrs[idx])
  }

  # --- Uend general path ---
  # Runs after all compound blocks so after_cmc uses the corrected Ustart values.
  # Only fills Uend cases not already set by a compound block (17/18/26/27/28/126).
  end_after_cmc <- pmax(df_unions_final[[Ustart]], df_unions_final[[Mstart]], na.rm = TRUE)
  uend_missing  <- is.na(df_unions_final[[Uend]]) & !is.na(e_yrs)
  if (any(uend_missing)) {
    df_unions_final[[Uend]][uend_missing] <- compute_cmc(
      imputed_month_capped(sum(uend_missing),
                           e_yrs[uend_missing],
                           survey_cmc = surv_cmc[uend_missing],
                           after_cmc  = end_after_cmc[uend_missing]),
      e_yrs[uend_missing]
    )
  }

  # Set dissolution motive to NA for women who never entered union u
  df_unions_final[[UendMotive]] <- ifelse(
    !is.na(u_yrs), df_unions_final[[UendMotive]], NA_character_
  )

  # Imputation flags: 1 when CMC is non-missing (month was imputed)
  df_unions_final[[Ustart_I]] <- ifelse(is.na(df_unions_final[[Ustart]]), NA_integer_, 1L)
  df_unions_final[[Uend_I]]   <- ifelse(is.na(df_unions_final[[Uend]]),   NA_integer_, 1L)
  df_unions_final[[Mstart_I]] <- ifelse(is.na(df_unions_final[[Mstart]]), NA_integer_, 1L)

  # --- Diagnostic columns: imputed month extracted from CMC ---
  # Allows year / month / CMC / code to be inspected side by side.
  # Kept in when DEBUG exists; removed with NULL deletions below otherwise.
  Ustart_month <- paste0("union_start_month",    u)
  Mstart_month <- paste0("marriage_start_month", u)
  Uend_month   <- paste0("union_end_month",      u)

  df_unions_final[[Ustart_month]] <- ifelse(
    !is.na(df_unions_final[[Ustart]]) & !is.na(u_yrs),
    df_unions_final[[Ustart]] - (u_yrs - 1900L) * 12L,
    NA_integer_
  )
  df_unions_final[[Mstart_month]] <- ifelse(
    !is.na(df_unions_final[[Mstart]]) & !is.na(m_yrs),
    df_unions_final[[Mstart]] - (m_yrs - 1900L) * 12L,
    NA_integer_
  )
  df_unions_final[[Uend_month]] <- ifelse(
    !is.na(df_unions_final[[Uend]]) & !is.na(e_yrs),
    df_unions_final[[Uend]] - (e_yrs - 1900L) * 12L,
    NA_integer_
  )

  # Relocate: group year / month / CMC / code together per event for inspection
  df_unions_final <- dplyr::relocate(
    df_unions_final,
    all_of(c(
      UstartType,
      Ustart_year, Ustart_code, Ustart_month, Ustart,
      Mstart_year, Mstart_code, Mstart_month, Mstart,
      Uend_year,               Uend_month,   Uend,
      UendMotive
    )),
    .after = all_of(UstartType)
  )
}

df_unions_final$surveyDate_cmc <- NULL

# In production (no DEBUG): drop diagnostic columns and reorder for output.
# In debug mode: keep all year/code/month columns for inspection.
if (!exists("DEBUG")) {
  for (u in seq_len(5)) {
    Ustart_year  <- paste0("yStartUnion_",        u)
    Ustart_code  <- paste0("yStartUnionCode_",    u)
    Uend_year    <- paste0("yEndUnion_",          u)
    Mstart_year  <- paste0("yStartMarr_",         u)
    Mstart_code  <- paste0("yStartMarrCode_",     u)
    Ustart_month <- paste0("union_start_month",   u)
    Mstart_month <- paste0("marriage_start_month",u)
    Uend_month   <- paste0("union_end_month",     u)

    df_unions_final[[Ustart_year]]  <- NULL
    df_unions_final[[Ustart_code]]  <- NULL
    df_unions_final[[Mstart_year]]  <- NULL
    df_unions_final[[Mstart_code]]  <- NULL
    df_unions_final[[Uend_year]]    <- NULL
    df_unions_final[[Ustart_month]] <- NULL
    df_unions_final[[Mstart_month]] <- NULL
    df_unions_final[[Uend_month]]   <- NULL
  }
  # Reorder so main CMC variables appear together per union slot
  for (u in seq_len(5)) {
    Ustart     <- paste0("union_start_cmc",    u)
    Uend       <- paste0("union_end_cmc",      u)
    Mstart     <- paste0("marriage_start_cmc", u)
    UendMotive <- paste0("union_end_motive",   u)
    UstartType <- paste0("union_start_type",   u)
    df_unions_final <- dplyr::relocate(
      df_unions_final,
      all_of(c(Ustart, Uend, Mstart, UendMotive)),
      .after = all_of(UstartType)
    )
  }
}

EDER_ENADID <- EDER_ENADID %>%
  dplyr::left_join(df_unions_final, by = "llave_muj")
rm(df_unions_final)


# ==== 4. Birth history ====

# hij_vid_i row-type codes:
#   0  No ha nacido / not applicable
#   6  Anio de nacimiento -- birth year row
#   7  Anio de fallecimiento -- death year row
#  60  Sobrevive / Vive (summary status)
#  70  Fallecido (summary status)
#  90  No sabe
#
# Birth year <- anio_retro where hij_vid_i == 6
# Death year <- anio_retro where hij_vid_i == 7
# Sex        <- hij_gen_i (constant; take first non-zero value)
#
# CHANGES from original 2017 code:
#   (a) Birth row detection changed from `hij_gen_i != 0` (sex variable,
#       wrong column) to `hij_vid_i == 6` (row-type variable, correct).
#   (b) imputed_month(length(first_row)) was always 1 since first_row is
#       a scalar; corrected to imputed_month_capped(1L, ...).
#   (c) Death detection used length(idx) as n, potentially > 1 if multiple
#       rows carry code 7; corrected to take which.min(anio_retro) as in 2025.
#   (d) Plain imputed_month() replaced by imputed_month_capped() throughout,
#       with survey_cmc passed as upper bound.
#   (e) prev_dob chain added: within each woman, each birth CMC is passed as
#       after_cmc for the next birth, ensuring strict ordering when two births
#       share the same year (same-year twins or rapid successive births).
#   (f) surveyDate_cmc joined into EDER via data.table so it is available
#       inside the by-group expression.

EDER_ENADID$nBioKids <- EDER_one$hij_vivos

setDT(EDER)

# Join surveyDate_cmc into EDER so it is available inside the by-group
survey_cmc_dt <- data.table(
  llave_muj      = EDER_ENADID$llave_muj,
  surveyDate_cmc = EDER_ENADID$surveyDate_cmc
)
EDER <- survey_cmc_dt[EDER, on = "llave_muj"]
rm(survey_cmc_dt)

gen_cols <- paste0("hij_gen_", 1:15)
vid_cols <- paste0("hij_vid_", 1:15)

df_summary <- EDER[, {
  surv     <- surveyDate_cmc[1L]
  prev_dob <- NA_integer_   # tracks last imputed birth CMC for after_cmc chaining
  results  <- list()

  for (i in seq_len(15)) {
    gen_col <- gen_cols[i]
    vid_col <- vid_cols[i]

    # Birth year: row where hij_vid_i == 6
    birth_idx <- which(!is.na(get(vid_col)) & get(vid_col) == 6L)
    if (length(birth_idx) > 0) {
      first_birth_row <- birth_idx[which.min(anio_retro[birth_idx])]
      birth_yr <- anio_retro[first_birth_row]
      dob <- compute_cmc(
        imputed_month_capped(1L, birth_yr,
                             survey_cmc = surv,
                             after_cmc  = prev_dob),
        birth_yr
      )
      results[[paste0("dob_cmc",   i)]] <- dob
      results[[paste0("dob_cmc_I", i)]] <- 1L
      prev_dob <- dob   # pass forward for next child ordering

      sex_vals <- get(gen_col)
      results[[paste0("sex", i)]] <-
        if (any(!is.na(sex_vals) & sex_vals > 0L))
          sex_vals[which(!is.na(sex_vals) & sex_vals > 0L)[1]]
        else NA_integer_
    } else {
      results[[paste0("dob_cmc",   i)]] <- NA_integer_
      results[[paste0("dob_cmc_I", i)]] <- NA_integer_
      results[[paste0("sex", i)]]       <- NA_integer_
      prev_dob <- NA_integer_   # chain broken: no birth at position i
    }

    # Death year: row where hij_vid_i == 7
    death_idx <- which(!is.na(get(vid_col)) & get(vid_col) == 7L)
    if (length(death_idx) > 0) {
      first_death_row <- death_idx[which.min(anio_retro[death_idx])]
      death_yr <- anio_retro[first_death_row]
      results[[paste0("dod_cmc",   i)]] <-
        compute_cmc(imputed_month_capped(1L, death_yr, survey_cmc = surv), death_yr)
      results[[paste0("dod_cmc_I", i)]] <- 1L
    } else {
      results[[paste0("dod_cmc",   i)]] <- NA_integer_
      results[[paste0("dod_cmc_I", i)]] <- NA_integer_
    }
  }
  results
}, by = llave_muj]

EDER_ENADID <- EDER_ENADID %>%
  dplyr::left_join(df_summary, by = "llave_muj")
rm(df_summary)


# ==== 5. Finalise & save ====

if (!exists("DEBUG")) {
  EDER_ENADID <- cleanENADID(EDER_ENADID)
  EDER_ENADID <- reorder_birthHistory(EDER_ENADID)

  save(EDER_ENADID, file = path_EDER_ENADID2017)
  rm(EDER)
}
