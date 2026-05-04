setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.r")
# Read EDER 2025 file
# imputed_month_capped() is defined in enadid_lib.r
library(tidyverse)
library(haven)
library(purrr)
library(data.table)

if (exists("DEBUG25")) browser()

path_EDER2025            <- path.expand(paste0(rootPath, "/INEGI/Encuestas/EDER/2025/eder2025_bases_sav/historiavida.sav"))
path_EDER2025_informante <- path.expand(paste0(rootPath, "/INEGI/Encuestas/EDER/2025/eder2025_bases_sav/informante.sav"))
path_EDER_ENADID2025     <- path.expand(paste0(rootPath, "/INEGI/Encuestas/ENADID/EDER_ENADID2025.Rdat"))


# ==== 1. Load & merge historiavida + informante ====

if (!exists("EDER25")) {
  EDER25 <- haven::read_sav(path_EDER2025)
} else {
  EDER25$sexo_pe <- NULL
  EDER25$mes_nac <- NULL
  EDER25$anio_nac <- NULL
  EDER25$edad_act <- NULL
  EDER25$edad_rsex <- NULL
  EDER25$anticoncep <- NULL
  EDER25$matrimonio <- NULL
  EDER25$hij_vivos <- NULL
}

EDER25$llave_muj <- paste0(EDER25$folioviv, EDER25$foliohog, EDER25$id_pobla)

informante25 <- haven::read_sav(path_EDER2025_informante)
informante25$llave_muj <- paste0(informante25$folioviv, informante25$foliohog, informante25$id_pobla)

# Verify every EDER25 row has a matching informante row
keys_only_in_EDER25 <- setdiff(EDER25$llave_muj, informante25$llave_muj)
if (length(keys_only_in_EDER25) > 0) {
  stop(paste("FAILURE: EDER25 is missing", length(keys_only_in_EDER25), "keys in informante"))
}

# Join individual-level variables from informante
EDER25 <- EDER25 %>%
  dplyr::left_join(
    dplyr::select(informante25, llave_muj, sexo_pe, mes_nac, anio_nac,
                  edad_act, edad_rsex, anticoncep, matrimonio, hij_vivos),
    by = "llave_muj"
  )
rm(informante25)

EDER25 <- haven::zap_labels(EDER25)

# Keep key identifiers as character; convert everything else to integer
to_keep_char <- c("llave_muj", "folioviv", "foliohog", "id_pobla", "geo_eder", "geo_eder1")

EDER25 <- EDER25 %>%
  dplyr::mutate(across(
    .cols = -all_of(to_keep_char),
    .fns  = as.integer
  ))

# Restrict to women
EDER25 <- subset(EDER25, sexo_pe == 2)

# One row per woman (for individual-level variables)
EDER_one <- EDER25[!duplicated(EDER25$llave_muj), ]


# ==== 2. Initialise output dataframe ====

# Survey field period: 7 May - 1 Sep 2025. Survey CMC fixed at September 2025
# to include all the women, although we will not use year 2025 for computing indices...
# NOTE: surveyDate_cmc is per-individual here for consistency with other
#   surveys, even though EDER 2025 does not publish individual interview dates.
n <- nrow(EDER_one)
survey_cmc_eder25 <- compute_cmc(9L, 2025L)

EDER_ENADID25 <- data.frame(country = rep("MEXICO", n), survey = rep("EDER2025", n))
EDER_ENADID25$llave_muj          <- EDER_one$llave_muj
EDER_ENADID25$surveyDate_cmc     <- survey_cmc_eder25
EDER_ENADID25                    <- compute_lastYear(EDER_ENADID25)
EDER_ENADID25$indiv_dob_cmc      <- compute_cmc(EDER_one$mes_nac, EDER_one$anio_nac)
EDER_ENADID25$yBirth             <- EDER_one$anio_nac
EDER_ENADID25$indiv_age_survey   <- EDER_one$edad_act
EDER_ENADID25$indiv_weight       <- EDER_one$factor_per
EDER_ENADID25$pregnant           <- NA
EDER_ENADID25$want_another       <- NA
# NOTE: renamed edad_sex -> edad_rsex in 2025
EDER_ENADID25$age_first_sex      <- EDER_one$edad_rsex
EDER_ENADID25$ever_contraception <- EDER_one$anticoncep
EDER_ENADID25$ever_contraception <- factor(EDER_ENADID25$ever_contraception, levels=c(1,2),labels=c("Yes","No"))
# NOTE: variable name and semantics unchanged (number of unions/marriages)
EDER_ENADID25$nUnion             <- EDER_one$matrimonio


# ==== 3. Union history ====

# IMPORTANT CHANGE FROM 2017:
#   2017 used compound codes encoding simultaneous events in a single value
#   (e.g. 12 = cohabitation + civil marriage same year; 126 = three events).
#   2025 uses a clean, non-compound code set — one event per row.
#   2025 also has 5 union slots (edo_civil1..5); 2017 had 6.
#
#   TRANSITION codes (event starts in that year):
#     1  Inicia union libre
#     2  Inicia matrimonio civil
#     3  Inicia matrimonio religioso
#     4  Inicia matrimonio civil y religioso
#     5  Inicia separacion
#     6  Inicia divorcio
#     7  Fallecimiento del conyuge
#
#   STATE codes (continuing status in that year):
#    10  Union libre (ongoing)
#    20  Matrimonio civil (ongoing)
#    30  Matrimonio religioso (ongoing)
#    40  Matrimonio civil y religioso (ongoing)
#    50  Separada(o)
#    60  Divorciada(o)
#    70  Viuda(o)

UNION_START_CODES <- c(1L, 2L, 3L, 4L)     # transition: union begins
UNION_STATE_CODES <- c(10L, 20L, 30L, 40L)  # ongoing state: in union
MARR_START_CODES  <- c(2L, 3L, 4L)          # transition: marriage begins
MARR_STATE_CODES  <- c(20L, 30L, 40L)       # ongoing: married
DISS_TRANS_CODES  <- c(5L, 6L, 7L)          # transition: dissolution event
DISS_STATE_CODES  <- c(50L, 60L, 70L)       # ongoing: dissolved
COHAB_START_CODES <- c(1L)                   # transition: cohabitation only

extract_union_data_2025 <- function(codes, years) {
  # Returns a named list of 8 elements per union slot.
  na_result <- list(
    type           = NA_character_,
    start_u        = NA_integer_,
    start_u_code   = NA_integer_,
    start_m        = NA_integer_,
    start_m_code   = NA_integer_,
    end_u          = NA_integer_,
    diss_type      = "None",
    start_inferred = FALSE   # TRUE when start year came from state codes (diagnostic)
  )

  # --- Union start ---
  # Use transition codes (1-4) as primary signal; fall back to state codes
  # (10/20/30/40) only if no transition code is present. The old approach
  # of `codes > 0` risked picking up a state code when the transition-year
  # row was absent, pushing the apparent start year forward.
  trans_idx <- which(codes %in% UNION_START_CODES)

  if (length(trans_idx) > 0) {
    first_idx        <- trans_idx[which.min(years[trans_idx])]
    start_union_yr   <- years[first_idx]
    start_union_code <- codes[first_idx]
    start_inferred   <- FALSE
  } else {
    state_idx <- which(codes %in% UNION_STATE_CODES)
    if (length(state_idx) == 0) return(na_result)
    first_idx        <- state_idx[which.min(years[state_idx])]
    start_union_yr   <- years[first_idx]
    start_union_code <- codes[first_idx]
    start_inferred   <- TRUE
  }

  # --- Marriage start ---
  # Prefer transition codes (2/3/4); fall back to earliest state code (20/30/40).
  marr_trans_idx <- which(codes %in% MARR_START_CODES)
  if (length(marr_trans_idx) > 0) {
    mi              <- marr_trans_idx[which.min(years[marr_trans_idx])]
    start_marr_yr   <- years[mi]
    start_marr_code <- codes[mi]
  } else {
    marr_state_idx <- which(codes %in% MARR_STATE_CODES)
    if (length(marr_state_idx) > 0) {
      mi              <- marr_state_idx[which.min(years[marr_state_idx])]
      start_marr_yr   <- years[mi]
      start_marr_code <- codes[mi]
    } else {
      start_marr_yr   <- NA_integer_
      start_marr_code <- NA_integer_
    }
  }

  # --- Union type ---
  starts_with_cohab <- start_union_code %in% COHAB_START_CODES
  has_marriage      <- !is.na(start_marr_yr)

  u_type <- dplyr::case_when(
    starts_with_cohab &  has_marriage ~ "cohabitation before marriage",
    starts_with_cohab & !has_marriage ~ "cohabitation",
    !starts_with_cohab & has_marriage ~ "marriage",
    TRUE                              ~ NA_character_
  )

  # --- Union end ---
  # Prefer transition dissolution code (5/6/7) — this is the actual event year.
  # Fall back to ongoing dissolution state (50/60/70) when no transition row
  # exists; note this year may be >= 1 year after the true dissolution.
  diss_trans_idx <- which(codes %in% DISS_TRANS_CODES)
  diss_state_idx <- which(codes %in% DISS_STATE_CODES)

  if (length(diss_trans_idx) > 0) {
    di           <- min(diss_trans_idx)
    end_union_yr <- years[di]
    final_code   <- codes[di]
  } else if (length(diss_state_idx) > 0) {
    di           <- min(diss_state_idx)
    end_union_yr <- years[di]
    final_code   <- codes[di]
  } else {
    end_union_yr <- NA_integer_
    final_code   <- NA_integer_
  }

  d_type <- dplyr::case_when(
    final_code %in% c(6L, 60L) ~ "divorce",
    final_code %in% c(5L, 50L) ~ "separation",
    final_code %in% c(7L, 70L) ~ "widowhood",
    TRUE                        ~ "in union"
  )

  return(list(
    type           = u_type,
    start_u        = start_union_yr,
    start_u_code   = start_union_code,
    start_m        = start_marr_yr,
    start_m_code   = start_marr_code,
    end_u          = end_union_yr,
    diss_type      = d_type,
    start_inferred = start_inferred
  ))
}

df_unions_final <- EDER25 %>%
  dplyr::group_by(llave_muj) %>%
  dplyr::group_modify(~ {
    data_woman <- .x
    purrr::map_dfc(1:5, function(i) {
      col_name <- paste0("edo_civil", i)
      res <- extract_union_data_2025(data_woman[[col_name]], data_woman$anio_retro)
      purrr::set_names(res, c(
        paste0("union_start_type",     i),
        paste0("yStartUnion_",         i),
        paste0("yStartUnionCode_",     i),
        paste0("yStartMarr_",          i),
        paste0("yStartMarrCode_",      i),
        paste0("yEndUnion_",           i),
        paste0("union_end_motive",     i),
        paste0("union_start_inferred", i)   # diagnostic: TRUE = start year from state code
      ))
    })
  }) %>%
  dplyr::ungroup()

# Join surveyDate_cmc into df_unions_final so imputed_month_capped() can use
# it as an upper bound. All women share the same date in EDER 2025, but we
# pass it as a vector to match the interface expected by imputed_month_capped().
df_unions_final <- df_unions_final %>%
  dplyr::left_join(
    dplyr::select(EDER_ENADID25, llave_muj, surveyDate_cmc),
    by = "llave_muj"
  )

# Convert year columns to CMC.
# imputed_month_capped() handles all constraints in one call per event:
#   survey_cmc  caps the draw from above (no event after interview date)
#   after_cmc   caps the draw from below (used for marriage start when union
#               start falls in the same year, ensuring strict ordering)

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

  N          <- nrow(df_unions_final)
  u_yrs      <- df_unions_final[[Ustart_year]]
  e_yrs      <- df_unions_final[[Uend_year]]
  m_yrs      <- df_unions_final[[Mstart_year]]
  surv_cmc   <- df_unions_final$surveyDate_cmc

  # --- Union start: capped at survey date ---
  df_unions_final[[Ustart]] <- ifelse(
    !is.na(u_yrs),
    compute_cmc(imputed_month_capped(N, u_yrs, survey_cmc = surv_cmc), u_yrs),
    NA_integer_
  )

  # --- Marriage start: capped at survey date and after union start ---
  # When union start and marriage start fall in the same year (cohabitation
  # that formalised), after_cmc = union_start_cmc ensures the marriage month
  # is strictly later. In other years after_cmc has no effect (different year).
  df_unions_final[[Mstart]] <- ifelse(
    !is.na(m_yrs),
    compute_cmc(
      imputed_month_capped(N, m_yrs,
                           survey_cmc = surv_cmc,
                           after_cmc  = df_unions_final[[Ustart]]),
      m_yrs
    ),
    NA_integer_
  )

  # Lower bound for union end: the later of union start and marriage start
  # (both only bind when they fall in the same year as the union end)
  end_after_cmc <- pmax(
    df_unions_final[[Ustart]],
    df_unions_final[[Mstart]],   # NA-safe: pmax treats NA as -Inf by default
    na.rm = TRUE
  )
  
  # --- Union end: capped at survey date and max of union and marriage date ---
  df_unions_final[[Uend]] <- ifelse(
    !is.na(e_yrs),
    compute_cmc(
      imputed_month_capped(N, e_yrs,
                           survey_cmc = surv_cmc,
                           after_cmc  = end_after_cmc),
      e_yrs
    ),
    NA_integer_
  )
  
  # Set dissolution motive to NA for women who never entered union u
  df_unions_final[[UendMotive]] <- ifelse(
    !is.na(u_yrs), df_unions_final[[UendMotive]], NA_character_
  )

  # Drop year/code helper columns
  df_unions_final[[Ustart_year]]  <- NULL
  df_unions_final[[Ustart_code]]  <- NULL
  df_unions_final[[Mstart_year]]  <- NULL
  df_unions_final[[Mstart_code]]  <- NULL
  df_unions_final[[Uend_year]]    <- NULL

  # Imputation flags: 1 when CMC is non-missing (month was imputed)
  df_unions_final[[Ustart_I]] <- ifelse(is.na(df_unions_final[[Ustart]]), NA_integer_, 1L)
  df_unions_final[[Uend_I]]   <- ifelse(is.na(df_unions_final[[Uend]]),   NA_integer_, 1L)
  df_unions_final[[Mstart_I]] <- ifelse(is.na(df_unions_final[[Mstart]]), NA_integer_, 1L)

  df_unions_final <- dplyr::relocate(
    df_unions_final,
    all_of(c(Ustart, Uend, Mstart, UendMotive)),
    .after = all_of(UstartType)
  )
}

df_unions_final$surveyDate_cmc <- NULL   # drop join helper before merging

EDER_ENADID25 <- EDER_ENADID25 %>%
  dplyr::left_join(df_unions_final, by = "llave_muj")
rm(df_unions_final)


# ==== Diagnostic: union extraction (remove after validation) ====

# Profiles extraction quality before cleanENADID(). The key output is the
# cross-tab at the bottom: if most overlaps are Inferred=TRUE, the extraction
# logic was picking up the wrong start year; if Inferred=FALSE, the overlaps
# reflect real sequencing problems in the data.

diagnose_union_extraction <- function(df) {
  cat("\n========== UNION EXTRACTION DIAGNOSTICS ==========\n")

  for (u in 1:5) {
    Ustart    <- paste0("union_start_cmc",      u)
    Uend      <- paste0("union_end_cmc",        u)
    Mstart    <- paste0("marriage_start_cmc",   u)
    Uinferred <- paste0("union_start_inferred", u)
    survey    <- df$surveyDate_cmc

    if (!(Ustart %in% names(df))) next
    has_u <- !is.na(df[[Ustart]])
    cat(sprintf("\n--- Union %d (n with start = %d) ---\n", u, sum(has_u)))

    if (Uinferred %in% names(df))
      cat(sprintf("  Start inferred from state code (no transition code): %d\n",
                  sum(df[[Uinferred]] & has_u, na.rm = TRUE)))

    cat(sprintf("  End before start: %d\n",
                sum(!is.na(df[[Uend]]) & !is.na(df[[Ustart]]) &
                      df[[Uend]] < df[[Ustart]], na.rm = TRUE)))
    cat(sprintf("  Start after survey date: %d\n",
                sum(has_u & !is.na(survey) & df[[Ustart]] > survey, na.rm = TRUE)))

    if (Uend %in% names(df))
      cat(sprintf("  End after survey date: %d\n",
                  sum(!is.na(df[[Uend]]) & !is.na(survey) &
                        df[[Uend]] > survey, na.rm = TRUE)))

    if (Mstart %in% names(df))
      cat(sprintf("  Marriage start after survey date: %d\n",
                  sum(!is.na(df[[Mstart]]) & !is.na(survey) &
                        df[[Mstart]] > survey, na.rm = TRUE)))
  }

  for (u in 1:4) {
    Uend_prev  <- paste0("union_end_cmc",   u)
    Ustart_cur <- paste0("union_start_cmc", u + 1)
    if (!(Ustart_cur %in% names(df))) next
    has_both <- !is.na(df[[Uend_prev]]) & !is.na(df[[Ustart_cur]])
    cat(sprintf("\n--- Union %d->%d ---\n", u, u + 1))
    cat(sprintf("  Union %d starts before union %d ends: %d\n", u + 1, u,
                sum(has_both & df[[Ustart_cur]] < df[[Uend_prev]], na.rm = TRUE)))
    cat(sprintf("  Union %d starts but union %d has no end: %d\n", u + 1, u,
                sum(!is.na(df[[Ustart_cur]]) & is.na(df[[Uend_prev]]), na.rm = TRUE)))
  }

  cat("\n--- Inferred starts by union ---\n")
  for (u in 1:5) {
    Uinferred <- paste0("union_start_inferred", u)
    Ustart    <- paste0("union_start_cmc",      u)
    if (!(Uinferred %in% names(df))) next
    n_u   <- sum(!is.na(df[[Ustart]]))
    n_inf <- sum(df[[Uinferred]], na.rm = TRUE)
    cat(sprintf("  Union %d: %d inferred out of %d (%.1f%%)\n",
                u, n_inf, n_u, 100 * n_inf / max(n_u, 1)))
  }

  vars2 <- c("union_start_inferred2", "union_start_cmc2", "union_end_cmc1")
  if (all(vars2 %in% names(df))) {
    cat("\n--- Union 2: inferred start x overlap with union 1 end ---\n")
    has_u2  <- !is.na(df$union_start_cmc2)
    overlap <- has_u2 & !is.na(df$union_end_cmc1) &
               df$union_start_cmc2 < df$union_end_cmc1
    inf2    <- df$union_start_inferred2
    cat(sprintf("  Inferred=TRUE  & overlap:    %d\n", sum( inf2 &  overlap, na.rm = TRUE)))
    cat(sprintf("  Inferred=TRUE  & no overlap: %d\n", sum( inf2 & !overlap, na.rm = TRUE)))
    cat(sprintf("  Inferred=FALSE & overlap:    %d\n", sum(!inf2 &  overlap, na.rm = TRUE)))
    cat(sprintf("  Inferred=FALSE & no overlap: %d\n", sum(!inf2 & !overlap, na.rm = TRUE)))
  }

  cat("\n===================================================\n")
}

diagnose_union_extraction(EDER_ENADID25)


# ==== 4. Birth history ====

# hij_vid_i row-type codes (identical in 2017 and 2025):
#   0  No ha nacido / not applicable
#   6  Anio de nacimiento  -- birth year row
#   7  Anio de fallecimiento -- death year row
#   9  Anio en que dejo de saber (2025 only)
#  60  Sobrevive / Vive (summary status)
#  70  Fallecido (summary status)
#  90  No sabe
#
# Birth year <- anio_retro where hij_vid_i == 6
# Death year <- anio_retro where hij_vid_i == 7
# Sex        <- hij_gen_i (constant; take first non-zero value)
#
# NOTE: birth filter changed from `!= 0` (too broad) to `== 6`.
# NOTE: imputed_month_capped() prevents births in the survey year from
#   being imputed to months after the survey date. survey_cmc is joined
#   into EDER25 (via llave_muj) so it is available inside the data.table
#   by-group expression.

EDER_ENADID25$nBioKids <- EDER_one$hij_vivos

setDT(EDER25)

# Join surveyDate_cmc into EDER25 so it is available inside the by-group
survey_cmc_dt <- data.table(
  llave_muj      = EDER_ENADID25$llave_muj,
  surveyDate_cmc = EDER_ENADID25$surveyDate_cmc
)
EDER25 <- survey_cmc_dt[EDER25, on = "llave_muj"]
rm(survey_cmc_dt)

gen_cols <- paste0("hij_gen_", 1:15)
vid_cols <- paste0("hij_vid_", 1:15)

df_summary <- EDER25[, {
  surv     <- surveyDate_cmc[1L]
  prev_dob <- NA_integer_   # tracks last imputed birth CMC within this woman
  results  <- list()
  
  for (i in seq_len(15)) {
    gen_col <- gen_cols[i]
    vid_col <- vid_cols[i]
    
    birth_idx <- which(!is.na(get(vid_col)) & get(vid_col) == 6L)
    if (length(birth_idx) > 0) {
      first_birth_row <- birth_idx[which.min(anio_retro[birth_idx])]
      birth_yr <- anio_retro[first_birth_row]
      
      # after_cmc = prev_dob only binds when birth_yr == year of prev_dob;
      # imputed_month_capped() ignores it otherwise
      dob <- compute_cmc(
        imputed_month_capped(1L, birth_yr,
                             survey_cmc = surv,
                             after_cmc  = prev_dob),
        birth_yr
      )
      results[[paste0("dob_cmc",   i)]] <- dob
      results[[paste0("dob_cmc_I", i)]] <- 1L
      prev_dob <- dob   # pass forward to next child
      
      sex_vals <- get(gen_col)
      results[[paste0("sex", i)]] <-
        if (any(!is.na(sex_vals) & sex_vals > 0L))
          sex_vals[which(!is.na(sex_vals) & sex_vals > 0L)[1]]
      else NA_integer_
    } else {
      results[[paste0("dob_cmc",   i)]] <- NA_integer_
      results[[paste0("dob_cmc_I", i)]] <- NA_integer_
      results[[paste0("sex", i)]]       <- NA_integer_
      prev_dob <- NA_integer_   # reset: no birth at position i, chain broken
    }
    
    # Death year (unchanged)
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

EDER_ENADID25 <- EDER_ENADID25 %>%
  dplyr::left_join(df_summary, by = "llave_muj")
rm(df_summary)


# ==== 5. Finalise & save ====

if (!(exists("DEBUG25"))) {
  EDER_ENADID25 <- cleanENADID(EDER_ENADID25)
  EDER_ENADID25 <- reorder_birthHistory(EDER_ENADID25)
  
  save(EDER_ENADID25, file = path_EDER_ENADID2025)
  rm(EDER25)
}