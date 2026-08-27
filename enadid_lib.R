setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rootPath <- sub("/INEGI/.*", "", getwd())
rootPath <- "/Users/daniel/My Drive (ddevolder@ced.uab.es)/Pachuca"
mainPath <- "/INEGI/Encuestas/"
dataPath <- paste0(rootPath, "/INEGI/Encuestas/ENADID/")

library (tidyverse)
library (haven)

# ==== Utilities ====

century_year <- function (year) {
  year <- ifelse(year == 0, NA, year)
  year <- ifelse(year == 99, 9999, year)
  year <- ifelse(year %in% seq (1,98,1),1900 + year, year)
  return (year)
}

compute_cmc <- function(month, year, survey_cmc = NULL, capped = FALSE) {
  month <- as.integer(month)
  year  <- as.integer(year)
  year  <- century_year(year)
  
  # Recycle scalar to match the longer vector, then check lengths
  if (length(month) == 1L) month <- rep(month, length(year))
  if (length(year)  == 1L) year  <- rep(year,  length(month))
  
  nm <- length(month)
  ny <- length(year)
  if (nm != ny) {
    warning(sprintf("compute_cmc: month (n=%d) and year (n=%d) have different lengths", nm, ny))
    browser()
  }
  
  # Impute month when missing or out of range. Routed through the unified
  # imputed_month_capped() so the sporadic (<10%) cases obey the same survey-
  # date cap as the systematic no-month surveys. No after_cmc context exists
  # here, so only the survey cap can bind (no event ordering).
  # Check is.na first: NA %in% 1:12 returns FALSE.
  need <- !is.na(year) & (is.na(month) | !(month %in% 1:12))
  if (any(need)) {
    sc <- if (is.null(survey_cmc)) NULL
    else if (length(survey_cmc) == 1L) survey_cmc
    else survey_cmc[need]
    month[need] <- imputed_month_capped(sum(need), year[need],
                                        survey_cmc = sc, capped = capped)
  }
  
  idx <- !is.na(year)
  cmc <- rep(NA_integer_, nm)
  cmc[idx] <- ifelse(year[idx] > 2100, 9999L, (year[idx] - 1900L) * 12L + month[idx])
  return(as.integer(cmc))
}

cmc_to_year <- function(cmc) (cmc - 1L) %/% 12L + 1900L

imputed_date <- function (month, year) {
  month <- as.integer(month)
  year <- as.integer(year)
  year <- century_year (year)
  imputed <- ifelse (is.na(month)&is.na(year),NA,0)
  # NA-safe: an NA month/year IS imputed (NA >= 1 evaluates to NA, which used
  # to propagate and make the whole flag NA, so downstream ordering checks
  # silently skipped those rows)
  imputed_month <- ifelse(!is.na(month) & (month >= 1) & (month <= 12), 0, 1)
  imputed_year <- ifelse(!is.na(year) & (year < 2100), 0, 10)
  return (imputed + imputed_month + imputed_year)
}

count_cmc <- function(vec) {
  rangeVal <- seq(10000,30000,1)
  return (sum(table (
    subset(vec, vec %in% rangeVal)
  )))
}

convertToInteger <- function(df=NULL, nColStart=NULL, colNotToConvert=NULL, check=TRUE) {
  is_int_string <- function(v) {
    v_na <- is.na(v)
    v_chr <- trimws(iconv(as.character(v), to="ASCII", sub=""))
    # matches optional sign + digits only
    ok_num <- grepl("^[+-]?[0-9]+$", v_chr)
    v_na | ok_num
  }
  if (is.null(nColStart)) nColStart <- 1
  # remove columns that are all text
  if (!is.null(colNotToConvert)) df[, colNotToConvert] <- list(NULL)
  #check everything is integer or NA
  if (check) {
    n <- ncol(df)
    for (i in (nColStart:n)) {
      if (!(colnames(df)[i] %in% colNotToConvert)) {
        bad <- !is_int_string(df[, i])
        if (any(bad)) {
          cat(paste0("Column", colnames(df)[i], "has non-integer values at position(s):\n"))
          cat(head(which(bad)),"\n")
          cat(head(as.character(as.data.frame(df)[which(bad), i])), "\n")
        }
      }
    }
  }
  df[, nColStart:ncol(df)] <- lapply(names(df)[nColStart:ncol(df)], function(col_name) {
    withCallingHandlers(
      as.integer(df[[col_name]]),
      warning = function(w) {
        if (grepl("NAs introduced by coercion", w$message)) {
          cat(paste("Warning in column:", col_name, "\n"))
        }
        invokeRestart("muffleWarning") # Optional: stops the warning from flooding the console
      }
    )
  })
  
  return (df)
}

listDatos <- function(datos) {
  na <- names(datos)
  for (nc in (1:ncol(datos))) {
    if (class(datos[,na[nc]])=="factor") {
      cat (na[nc],"\n")
      print(table(datos[,na[nc]],useNA = "always"))
    }
  }
}

loadENADID_data <- function() {
  load(file=paste0(dataPath, "MEXICO_ENADID.Rdat"), envir = globalenv())
  load(file=paste0(dataPath, "NSFG_ENADID.Rdat"), envir = globalenv())
}

check_union <- function (ENADID) {
  df <- data.frame(
    type=c("first union","first marriage","cohab before marriage"),
    separated=c(0,0,0),
    separated_end=c(0,0,0),
    widowed=c(0,0,0),
    widowed_end=c(0,0,0),
    inUnion=c(0,0,0),
    total=c(0,0,0)
  )
  df[df$type=="first union",]$total <- count_cmc(ENADID$union_start_cmc1)
  df[df$type=="first union",]$separated <- count_cmc(subset(ENADID,union_end_motive1=="separation")$union_start_cmc1)
  df[df$type=="first union",]$separated_end <- count_cmc(subset(ENADID,union_end_motive1=="separation")$union_end_cmc1)
  df[df$type=="first union",]$widowed <- count_cmc(subset(ENADID,union_end_motive1=="widowhood")$union_start_cmc1)
  df[df$type=="first union",]$widowed_end <- count_cmc(subset(ENADID,union_end_motive1=="widowhood")$union_end_cmc1)
  df[df$type=="first union",]$inUnion <- count_cmc(subset(ENADID,union_end_motive1=="in union")$union_start_cmc1)
  
  df[df$type=="first marriage",]$total <- count_cmc(ENADID$marriage_start_cmc1)
  df[df$type=="first marriage",]$separated <- count_cmc(subset(ENADID,union_end_motive1=="separation")$marriage_start_cmc1)
  df[df$type=="first marriage",]$separated_end <- count_cmc(subset(ENADID,union_end_motive1=="separation")$marriage_end_cmc1)
  df[df$type=="first marriage",]$widowed <- count_cmc(subset(ENADID,union_end_motive1=="widowhood")$marriage_start_cmc1)
  df[df$type=="first marriage",]$widowed_end <- count_cmc(subset(ENADID,union_end_motive1=="widowhood")$marriage_end_cmc1)
  df[df$type=="first marriage",]$inUnion <- count_cmc(subset(ENADID,union_end_motive1=="in union")$marriage_start_cmc1)
  
  cohabBeforeMarriage <- subset(ENADID, union_start_type1=="cohabitation before marriage")
  df[df$type=="cohab before marriage",]$total <- count_cmc(cohabBeforeMarriage$marriage_start_cmc1)
  df[df$type=="cohab before marriage",]$separated <- count_cmc(subset(cohabBeforeMarriage,union_end_motive1=="separation")$marriage_start_cmc1)
  df[df$type=="cohab before marriage",]$separated_end <- count_cmc(subset(cohabBeforeMarriage,union_end_motive1=="separation")$marriage_end_cmc1)
  df[df$type=="cohab before marriage",]$widowed <- count_cmc(subset(cohabBeforeMarriage,union_end_motive1=="widowhood")$marriage_start_cmc1)
  df[df$type=="cohab before marriage",]$widowed_end <- count_cmc(subset(cohabBeforeMarriage,union_end_motive1=="widowhood")$marriage_end_cmc1)
  df[df$type=="cohab before marriage",]$inUnion <- count_cmc(subset(cohabBeforeMarriage,union_end_motive1=="in union")$marriage_start_cmc1)
  
  return (df)
}


# Redraw the month of the LATER event so it falls on/after the EARLIER event
# within the same year (same month only when the earlier event is December, as
# no later month exists that year), when the later month was imputed and still
# respecting the survey-date cap. Reuses the strict-after branch of
# imputed_month_capped() (after_cmc_imp = 0). Shared by NSFG (raw_union_history)
# and ENADID/Mujeres (bigDataWomen) for orderings such as:
#   marriage start >= union start ; union end >= union start ; union end >= marriage
# `later`, `earlier`, `later_flag` are field-name stems; the slot is appended.
# Real months, different-year pairs, and equal-date pairs (e.g. a union that
# starts as a marriage, where union start == marriage start) are left untouched.
enforce_month_order <- function (datos, survey_cmc, later, earlier, later_flag, slots) {
  yr_of <- function (cmc) (cmc - 1L) %/% 12L + 1900L
  for (s in slots) {
    lf <- paste0(later,      s)
    ef <- paste0(earlier,    s)
    fl <- paste0(later_flag, s)
    if (!all(c(lf, ef, fl) %in% names(datos))) next
    e <- datos[[ef]]; l <- datos[[lf]]
    later_imputed <- !is.na(datos[[fl]]) & (datos[[fl]] %% 10L == 1L)   # month imputed
    idx <- !is.na(e) & !is.na(l) & later_imputed & (e != l) &
      (e < 9000L) & (l < 9000L) & (yr_of(e) == yr_of(l))
    if (any(idx)) {
      sc <- if (is.null(survey_cmc)) NULL else survey_cmc[idx]
      m  <- imputed_month_capped(sum(idx), yr_of(l[idx]),
                                 survey_cmc = sc,
                                 after_cmc = e[idx], after_cmc_imp = 0,
                                 capped = TRUE)
      datos[[lf]][idx] <- compute_cmc(m, yr_of(l[idx]))
    }
  }
  datos
}

split_birth_vs_union <- function (df, propBefore = 0.2, capMonth = FALSE,
                                  nBirths = 10, nUnions = 3, verbose = TRUE) {
  # Birth-vs-union probabilistic split (the EDER method, gated by capMonth).
  # When an IMPUTED birth month falls in the same year as a union start, the
  # uniform draw says nothing about their order. Re-draw it with
  # after_cmc_imp = 1 so a propBefore share of these births lands BEFORE the
  # union month and the rest on-or-after. Only imputed birth months are
  # touched (ENADID carries real months; dob_cmc_I %% 10 == 1 marks imputed).
  # The death date, computed as dob + lived offset BEFORE the join, is shifted
  # by the same delta so children never die before their birth.
  if (!isTRUE(capMonth)) return(df)
  for (b in seq_len(nBirths)) {
    dob  <- paste0("dob_cmc",   b)
    dobI <- paste0("dob_cmc_I", b)
    dod  <- paste0("dod_cmc",   b)
    if (!all(c(dob, dobI) %in% names(df))) next
    month_imp  <- !is.na(df[[dobI]]) & (df[[dobI]] %% 10L == 1L)
    for (u in seq_len(nUnions)) {
      Ustart <- paste0("union_start_cmc", u)
      if (!(Ustart %in% names(df))) next
      idx <- month_imp & !is.na(df[[dob]]) & !is.na(df[[Ustart]]) &
        (df[[dob]] < 9000L) & (df[[Ustart]] < 9000L) &
        (cmc_to_year(df[[dob]]) == cmc_to_year(df[[Ustart]]))
      if (any(idx)) {
        yr  <- cmc_to_year(df[[dob]][idx])
        new <- compute_cmc(
          imputed_month_capped(sum(idx), yr,
                               survey_cmc    = df$surveyDate_cmc[idx],
                               after_cmc     = df[[Ustart]][idx],
                               after_cmc_imp = 1,
                               propBefore    = propBefore,
                               capped        = TRUE),
          yr)
        delta <- new - df[[dob]][idx]
        df[[dob]][idx] <- new
        if (dod %in% names(df)) {
          ok <- !is.na(df[[dod]][idx]) & (df[[dod]][idx] < 9000L)
          df[[dod]][idx][ok] <- df[[dod]][idx][ok] + delta[ok]
        }
      }
    }
  }
  if (verbose && all(c("dob_cmc1", "union_start_cmc1") %in% names(df))) {
    same <- !is.na(df$dob_cmc1) & !is.na(df$union_start_cmc1) &
      (df$dob_cmc1 < 9000L) & (df$union_start_cmc1 < 9000L) &
      (cmc_to_year(df$dob_cmc1) == cmc_to_year(df$union_start_cmc1))
    cat("birth-vs-union split: n same-year (1st birth/1st union):", sum(same),
        " | share before union start:",
        round(mean(df$dob_cmc1[same] < df$union_start_cmc1[same]), 4), "\n")
  }
  df
}

bigDataWomen <- function (datos=NULL, hasFullUnionHistory=FALSE, countryName=NULL, capMonth=FALSE) {
  if (is.null(countryName)) countryName <- "MEXICO"
  ENADID <- data.frame(country=countryName, survey=datos$survey, llave_muj=datos$llave_muj)
  ENADID$surveyDate_cmc <- datos$surveyDate_cmc
  # survey-date cap for imputed months (NULL unless this survey opts in)
  sc_cap <- if (isTRUE(capMonth)) datos$surveyDate_cmc else NULL
  ENADID$lastYear <- 1900 + trunc (datos$surveyDate_cmc / 12) - 1
  ENADID$indiv_dob_cmc <- compute_cmc(datos$monthBirth, datos$yearBirth)
  ENADID$indiv_dob_cmc_I <- imputed_date(datos$monthBirth, datos$yearBirth)
  ENADID$yBirth <- century_year (datos$yearBirth)
  ENADID$indiv_age <- datos$age
  ENADID$indiv_age_survey <- trunc ((datos$surveyDate_cmc - ENADID$indiv_dob_cmc)/12)
  ENADID$indiv_weight <- datos$indiv_weight
  ENADID$nBioKids <- datos$nLiveBirths
  ENADID$nBioKids[is.na(ENADID$nBioKids)] <- 0
  ENADID$pregnant <- datos$pregnant
  ENADID$pregnant_wanted <- datos$pregnant_wanted
  ENADID$pregnant_want_another <- datos$pregnant_want_another
  ENADID$pregnant_ideal_number <- datos$pregnant_ideal_number
  ENADID$nullipar_want_another <- datos$nullipar_want_another
  ENADID$nullipar_fecund <- datos$nullipar_fecund
  ENADID$nullipar_ideal_number <- datos$nullipar_ideal_number
  ENADID$mother_want_another <- datos$mother_want_another
  ENADID$mother_fecund <- datos$mother_fecund
  ENADID$mother_ideal_number <- datos$mother_ideal_number
  ENADID$mother_unwanted <- datos$mother_unwanted
  ENADID$mother_less <- datos$mother_less
  ENADID$want_another <- datos$want_another
  ENADID$want_another <- ifelse(is.na(ENADID$want_another),ENADID$pregnant_want_another,ENADID$want_another)
  ENADID$want_another <- ifelse(is.na(ENADID$want_another),ENADID$nullipar_want_another,ENADID$want_another)
  ENADID$want_another <- ifelse(is.na(ENADID$want_another),ENADID$mother_want_another,ENADID$want_another)
  ENADID$ideal_number <- datos$ideal_number
  ENADID$ideal_number <- ifelse(is.na(ENADID$ideal_number),ENADID$pregnant_ideal_number,ENADID$ideal_number)
  ENADID$ideal_number <- ifelse(is.na(ENADID$ideal_number),ENADID$nullipar_ideal_number,ENADID$ideal_number)
  ENADID$ideal_number <- ifelse(is.na(ENADID$ideal_number),ENADID$mother_ideal_number,ENADID$ideal_number)
  ENADID$motive_no_child <- datos$motive_no_child
  ENADID$ever_contraception <- datos$ever_contraception
  ENADID$age_first_sex <- datos$age_first_sex
  ENADID$ever_had_sex <- ifelse(ENADID$age_first_sex == 88,1,2)
  ENADID$ever_had_sex <- ifelse(ENADID$age_first_sex == 99,9,ENADID$ever_had_sex)
  #ENADID$sexual_intercourse_last_month <- mujeres$p8_43 # will need recodification
  ENADID$union_status <- datos$union_status
  ENADID$lastUnion <- ifelse(ENADID$union_status == "single", 0, 1)
  ENADID$lastUnion_status <- ifelse(ENADID$union_status == "single", 0, NA)
  ENADID$lastUnion_status <- ifelse(ENADID$union_status %in% c("cohabitation", "married"), 1, ENADID$lastUnion_status)
  ENADID$lastUnion_status <- ifelse(ENADID$union_status %in% c("separated cohabitation", "separated marriage", "divorced"), 2, ENADID$lastUnion_status)
  ENADID$lastUnion_status <- ifelse(ENADID$union_status %in% c("widow cohabitation", "widow marriage"), 3, ENADID$lastUnion_status)
  ENADID$lastUnion_status <- factor(ENADID$lastUnion_status, levels = c(0, 1, 2, 3),
                                    labels = c("single", "in union", "separated", "widow"))
  ENADID$lastUnion_start_cmc <- ifelse(ENADID$union_status != "single",
                                       compute_cmc(datos$lastUnion_month_start, datos$lastUnion_year_start, survey_cmc = sc_cap, capped = capMonth),
                                       NA)
  ENADID$lastUnion_start_cmc_I <- imputed_date(datos$lastUnion_month_start, datos$lastUnion_year_start)
  ENADID$lastUnion_end_cmc <- ifelse(ENADID$union_status != "single",
                                     compute_cmc(datos$lastUnion_month_end, datos$lastUnion_year_end, survey_cmc = sc_cap, capped = capMonth),
                                     NA)
  ENADID$lastUnion_end_cmc_I <- imputed_date(datos$lastUnion_month_end, datos$lastUnion_year_end)
  ENADID$lastUnion_end_motive <- ifelse(ENADID$union_status %in% c("cohabitation", "married"), 0, NA)
  ENADID$lastUnion_end_motive <- ifelse(ENADID$union_status %in% c("separated cohabitation", "separated marriage", "divorced"), 1,
                                        ENADID$lastUnion_end_motive)
  ENADID$lastUnion_end_motive <- ifelse(ENADID$union_status %in% c("widow cohabitation", "widow marriage"), 2, ENADID$lastUnion_end_motive)
  ENADID$lastMarriage_start_cmc <- ifelse(ENADID$union_status %in% c("separated marriage", "divorced", "widow marriage", "married"),
                                          ENADID$lastUnion_start_cmc, NA)
  ENADID$lastMarriage_start_cmc_I <- ifelse(ENADID$union_status %in% c("separated marriage", "divorced", "widow marriage", "married"),
                                            ENADID$lastUnion_start_cmc_I, NA)
  ENADID$lastMarriage_end_cmc <- ifelse(ENADID$union_status %in% c("separated marriage", "divorced", "widow marriage", "married"),
                                        ENADID$lastUnion_end_cmc, NA)
  ENADID$lastMarriage_end_cmc_I <- ifelse(ENADID$union_status %in% c("separated marriage", "divorced", "widow marriage", "married"),
                                          ENADID$lastUnion_end_cmc_I, NA)
  # Cap for an imputed cohab-before month: not after the survey AND not after
  # the marriage/union date it precedes (both bind in the same year only).
  # Without this, an imputed cohab month can land after a KNOWN marriage month.
  cohab_cap <- function(union_cmc) {
    if (!isTRUE(capMonth)) return(NULL)
    u <- ifelse(!is.na(union_cmc) & union_cmc < 9000L, union_cmc, NA_integer_)
    pmin(datos$surveyDate_cmc, u, na.rm = TRUE)
  }
  ENADID$lastUnion_start_cmc <- ifelse((!is.na(datos$lastUnion_cohab_before))&(datos$lastUnion_cohab_before == "yes"),
                                       compute_cmc(datos$lastUnion_month_cohab_before, datos$lastUnion_year_cohab_before, survey_cmc = cohab_cap(ENADID$lastUnion_start_cmc), capped = capMonth),
                                       ENADID$lastUnion_start_cmc)
  ENADID$lastUnion_start_cmc_I <- ifelse((!is.na(datos$lastUnion_cohab_before))&(datos$lastUnion_cohab_before == "yes"),
                                         imputed_date(datos$lastUnion_month_cohab_before, datos$lastUnion_year_cohab_before),
                                         ENADID$lastUnion_start_cmc_I)
  
  ENADID$lastUnion_start_type <- NA
  idx <- (!is.na(ENADID$lastMarriage_start_cmc))
  ENADID$lastUnion_start_type [idx] <- 1
  idx <- (is.na(ENADID$lastMarriage_start_cmc))&(!is.na(ENADID$lastUnion_start_cmc))
  ENADID$lastUnion_start_type [idx] <- 2
  idx <- (ENADID$lastUnion_start_cmc < ENADID$lastMarriage_start_cmc)
  ENADID$lastUnion_start_type [idx] <- 3
  
  ENADID$lastUnion_start_type <- factor(ENADID$lastUnion_start_type, levels = c(1,2,3),
                                        labels = c("marriage", "cohabitation", "cohabitation before marriage"))
  
  ENADID$nUnion <- ifelse((!is.na(datos$nUnionBeforeLast))&(datos$nUnionBeforeLast %in% seq(1,20)),
                          ENADID$lastUnion + datos$nUnionBeforeLast,
                          ENADID$lastUnion)
  ENADID$lastUnion <- NULL
  
  if (!hasFullUnionHistory) {
    ENADID$union_start_cmc1 <- ifelse(ENADID$nUnion == 1,
                                      ENADID$lastUnion_start_cmc,
                                      compute_cmc(datos$firstUnionNotLast_month_start, datos$firstUnionNotLast_year_start, survey_cmc = sc_cap, capped = capMonth))
    ENADID$union_start_cmc_I1 <- ifelse(ENADID$nUnion == 1,
                                        ENADID$lastUnion_start_cmc_I,
                                        imputed_date(datos$firstUnionNotLast_month_start, datos$firstUnionNotLast_year_start))
    ENADID$union_end_cmc1 <- ifelse(ENADID$nUnion == 1,
                                    ENADID$lastUnion_end_cmc,
                                    compute_cmc(datos$firstUnionNotLast_month_end, datos$firstUnionNotLast_year_end, survey_cmc = sc_cap, capped = capMonth))
    ENADID$union_end_cmc_I1 <- ifelse(ENADID$nUnion == 1,
                                      ENADID$lastUnion_end_cmc_I,
                                      imputed_date(datos$firstUnionNotLast_month_end, datos$firstUnionNotLast_year_end))
    ENADID$union_end_motive1 <- ifelse(ENADID$nUnion == 1, as.numeric(ENADID$lastUnion_end_motive), datos$firstUnionNotLast_end_motive)
    ENADID$union_end_motive1 <- ifelse(ENADID$union_end_motive1 == 3, 1, ENADID$union_end_motive1)
    ENADID$union_end_motive1 <- factor(ENADID$union_end_motive1, levels = c(0,1,2),
                                       labels = c("in union", "separation", "widowhood"))
    ENADID$lastUnion_end_motive <- factor(ENADID$lastUnion_end_motive, levels = c(0,1,2),
                                          labels = c("in union", "separation", "widowhood"))
    ENADID$last_isFirstMarriage_start_cmc <- ifelse(ENADID$nUnion==1, ENADID$lastMarriage_start_cmc, NA)
    ENADID$last_isFirstMarriage_start_cmc_I <- ifelse(ENADID$nUnion==1, ENADID$lastMarriage_start_cmc_I, NA)
    ENADID$firstMarriageWithSep_start_cmc <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                                    ENADID$union_start_cmc1, NA)
    ENADID$firstMarriageWithSep_start_cmc_I <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                                      ENADID$union_start_cmc_I1, NA)
    ENADID$marriage_start_cmc1 <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                         ENADID$union_start_cmc1, ENADID$last_isFirstMarriage_start_cmc)
    ENADID$marriage_start_cmc_I1 <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                           ENADID$union_start_cmc_I1, ENADID$last_isFirstMarriage_start_cmc_I)
    ENADID$marriage_end_cmc1 <- ifelse(ENADID$nUnion == 1, ENADID$lastMarriage_end_cmc, NA)
    ENADID$marriage_end_cmc_I1 <- ifelse(ENADID$nUnion == 1, ENADID$lastMarriage_end_cmc_I, NA)
    ENADID$marriage_end_cmc1 <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                       ENADID$union_end_cmc1, ENADID$marriage_end_cmc1)
    ENADID$marriage_end_cmc_I1 <- ifelse((!is.na(datos$firstUnionNotLast_type))&(datos$firstUnionNotLast_type == "marriage"),
                                         ENADID$union_end_cmc_I1, ENADID$marriage_end_cmc_I1)
    ENADID$union_start_cmc1 <- ifelse((!is.na(datos$firstUnionNotLast_cohab_before))&(datos$firstUnionNotLast_cohab_before == "yes"),
                                      compute_cmc(datos$firstUnionNotLast_cohab_month, datos$firstUnionNotLast_cohab_year, survey_cmc = cohab_cap(ENADID$union_start_cmc1), capped = capMonth),
                                      ENADID$union_start_cmc1)
    ENADID$union_start_cmc_I1 <- ifelse((!is.na(datos$firstUnionNotLast_cohab_before))&(datos$firstUnionNotLast_cohab_before == "yes"),
                                        imputed_date(datos$firstUnionNotLast_cohab_month, datos$firstUnionNotLast_cohab_year),
                                        ENADID$union_start_cmc_I1)
    idx <- (is.na(ENADID$union_start_cmc1)&(!is.na(ENADID$marriage_start_cmc1)))
    ENADID$union_start_cmc1[idx] <- ENADID$marriage_start_cmc1 [idx]
    ENADID$union_start_cmc_I1[idx] <- ENADID$marriage_start_cmc_I1 [idx]
    
    ENADID$last_isFirstMarriage_start_cmc <- NULL
    ENADID$last_isFirstMarriage_start_cmc_I <- NULL
    ENADID$firstMarriageWithSep_start_cmc <- NULL
    ENADID$firstMarriageWithSep_start_cmc_I <- NULL
    
    ENADID$union_start_type1 <- NA
    idx <- (!is.na(ENADID$marriage_start_cmc1))
    ENADID$union_start_type1 [idx] <- 1
    idx <- (is.na(ENADID$marriage_start_cmc1))&(!is.na(ENADID$union_start_cmc1))
    ENADID$union_start_type1 [idx] <- 2
    idx <- (ENADID$union_start_cmc1 < ENADID$marriage_start_cmc1)
    ENADID$union_start_type1 [idx] <- 3
    
    ENADID$union_start_type1 <- factor(ENADID$union_start_type1, levels = c(1,2,3),
                                       labels = c("marriage", "cohabitation", "cohabitation before marriage"))
  }
  
  # Same-year ordering of imputed months (gated by capMonth): within a union,
  # marriage start >= union start, union end >= union start, union end >= marriage.
  if (isTRUE(capMonth) && !hasFullUnionHistory) {
    .sl <- c("1", "2", "3")
    ENADID <- enforce_month_order(ENADID, datos$surveyDate_cmc,
                                  "marriage_start_cmc", "union_start_cmc", "marriage_start_cmc_I", .sl)
    ENADID <- enforce_month_order(ENADID, datos$surveyDate_cmc,
                                  "union_end_cmc", "union_start_cmc", "union_end_cmc_I", .sl)
    ENADID <- enforce_month_order(ENADID, datos$surveyDate_cmc,
                                  "union_end_cmc", "marriage_start_cmc", "union_end_cmc_I", .sl)
  }
  
  return (ENADID)
  
}

addID <- function (df) {
  if (!("ID" %in% names(df))) {df <- cbind(ID = 1:nrow(df), df)}
  return (df)
}

# ==== Imputation-flag (_I) code registry ====
#
# Every *_cmc_I field (union_start_cmc_I, marriage_start_cmc_I, union_end_cmc_I,
# dob_cmc_I, ...) carries one of these codes describing how its CMC was obtained
# OR why it is bad. Codes are produced in several places; keep this list in sync
# when adding any.
#
# Codes 15/50/51 are BAD-DATA flags: cleanENADID no longer removes women, it
# only sets these flags and reports the problem. Actual removal is done later,
# on a case-by-case basis, by filterDateQuality() (see that function). So a
# reader that calls cleanENADID() alone keeps every woman; call filterDateQuality()
# afterwards to drop the flagged ones.
#
#    0   exact date (month and year reported)
#    1   month imputed, year reported            -> compute_cmc_fields (NSFG_lib.R)
#   10   year missing / out of range             -> compute_cmc_fields
#   11   month and year both missing             -> compute_cmc_fields
#   15   date is AFTER the survey date (well-formed value, chronologically
#          impossible -- not a DK/9999 artifact) -> cleanENADID (this file).
#          BAD DATA: dropped by filterDateQuality(dropBadUnionDates = TRUE).
#   20   cmc was 9999 (DK/refused), nulled to NA by union_end_9999_to_NA,
#          then back-filled to union_start_next - 1 by cleanENADID
#   21   end was genuinely NA (no date recorded), back-filled to union_start_next - 1
#          by cleanENADID
#   22   cleanENADID back-fill: recorded end was AFTER next union start, capped to it
#          -> cleanENADID (this file)
#   30   union end taken from the divorce date    -> compute_marriage_otherFields
#          (used when the stop-living date is missing)   (NSFG_lib.R)
#   31     divorce date, month imputed
#   32     divorce date, year missing
#   33     divorce date, month and year missing
#   40   model-imputed date (2006-10 model), reserved/unattributed -- should not appear
#   41     model-imputed: recently-separated women (issue 1)
#   42     model-imputed: husband-prior-kids marriages (issue 2)
#          -> NSFG_impute_dissolution_model.R
#   50   ordering violation on marriage_start_cmc: marriage starts BEFORE its own
#          union starts (marriage_start < union_start) -> cleanENADID (this file).
#          BAD DATA: dropped by filterDateQuality(dropBadUnionDates = TRUE).
#   51   ordering violation on union_end_cmc: union ends BEFORE it starts, or
#          before the marriage inside it (union_end < union_start, or
#          union_end < marriage_start) -> cleanENADID (this file).
#          BAD DATA: dropped by filterDateQuality(dropBadUnionDates = TRUE).
#   NA  no date / not applicable
#
# For union_end_cmc after cleanENADID (survival / dissolution analysis):
#
#   Treat as event (real observed dissolution date):
#     0, 1, 30, 31
#
#   Bad, un-repaired (separation known but date DK; window [union_start, survey]
#   too wide to be informative). REMOVED by filterDateQuality(dropBadUnionDates):
#     10
#
#   Exclude from donor-model fitting; optionally interval-censor
#   [union_start, union_start_next - 1] if the model supports it:
#     20, 21, 22
#
#   Exclude from donor-model fitting (these are model outputs, not observed
#   dates); treat as events in downstream fertility / union-duration analysis:
#     40, 41, 42
#
#   Bad data -- removed by filterDateQuality(dropBadUnionDates = TRUE):
#     11 (unknown), 15 (after survey), 51 (ordering). On marriage_start, 50
#     is the ordering analogue.
#
#   Right-censor at survey (union ongoing at interview):
#     NA
#
#   Note: codes 11, 32, 33 do not survive on union_end_cmc as such -- a
#   separation with a 9999 stop-living date is converted to 10 (by
#   union_end_9999_to_NA); a non-separation 9999 (e.g. widowhood, DK year)
#   keeps its cmc = 9999 / flag 10-11 and is dropped later by
#   filterDateQuality(dropBadUnionDates = TRUE).

checkImputedMonth <- function (df=NSFG_ENADID) {
  for (u in (1:3)) {
    us <- paste0("union_start_cmc", u)
    ue <- paste0("union_end_cmc", u)
    ue_I <- paste0("union_end_cmc_I", u)
    idx <- (df[[ue_I]] %in% c(1, 31)) & (yearFrom_cmc(df[[us]]) == yearFrom_cmc(df[[ue]]))
    idx[is.na(idx)] <- FALSE
    idx2 <- (df[[ue]][idx] < df[[us]][idx])
    idx2[is.na(idx2)] <- FALSE
    cat (sum(idx2),"date union end before start, out of",sum(idx),"imputed union end month, union order",u,"\n")
  }
  for (u in (1:3)) {
    us <- paste0("union_start_cmc", u)
    ms <- paste0("marriage_start_cmc", u)
    ms_I <- paste0("marriage_start_cmc_I", u)
    idx <- (df[[ms_I]] == 1) & (yearFrom_cmc(df[[us]]) == yearFrom_cmc(df[[ms]]))
    idx[is.na(idx)] <- FALSE
    idx2 <- (df[[ms]][idx] < df[[us]][idx])
    idx2[is.na(idx2)] <- FALSE
    cat (sum(idx2),"marriage start before union start, out of",sum(idx),"imputed marriage start month, union order",u,"\n")
  }
  for (u in (1:3)) {
    ms <- paste0("marriage_start_cmc", u)
    ue <- paste0("union_end_cmc", u)
    ue_I <- paste0("union_end_cmc_I", u)
    idx <- (df[[ue_I]] %in% c(1, 31)) & (yearFrom_cmc(df[[ms]]) == yearFrom_cmc(df[[ue]]))
    idx[is.na(idx)] <- FALSE
    idx2 <- (df[[ue]][idx] < df[[ms]][idx])
    idx2[is.na(idx2)] <- FALSE
    cat (sum(idx2),"date union end before marriage start, out of",sum(idx),"imputed union end month, union order",u,"\n")
  }
  for (b in (1:5)) {
    sd <- paste0("surveyDate_cmc")
    dob <- paste0("dob_cmc", b)
    dob_I <- paste0("dob_cmc_I", b)
    idx <- (df[[dob_I]] == 1) & (yearFrom_cmc(df[[sd]]) == yearFrom_cmc(df[[dob]]))
    idx[is.na(idx)] <- FALSE
    idx2 <- (df[[dob]][idx] > df[[sd]][idx])
    idx2[is.na(idx2)] <- FALSE
    cat (sum(idx2),"birth date after survey date, out of",sum(idx),"imputed, birth order",b,"\n")
  }
  idx <- !is.na(df$marriage_start_cmc1) & (df$marriage_start_cmc1 < df$union_start_cmc1)
  cat("check whether marriage date before union start is due to imputation")
  print(table(df$union_start_type1[idx]))       # expect all "cohabitation before marriage"
  cat ("everything should be either FALSE or no TRUE here\n")
  print(table(yearFrom_cmc(df$marriage_start_cmc1[idx]) == yearFrom_cmc(df$union_start_cmc1[idx])))  # TRUE = same-year (fixable)
}

# The fix below is a SEPARATE, smaller case: marriages that DID get a
# "separation" motive but whose stop-living date is refused/DK (9999) -> set
# the date to NA (keep the motive) so cleanENADID right-censors at the survey
# instead of removing them (cmc > 9900).

# Refused/DK separation date: union_end_cmc >= 9999 with motive "separation" -> NA
union_end_9999_to_NA <- function(datos) {
  ue_cols <- grep("^union_end_cmc\\d+$", names(datos), value = TRUE)
  for (ue in ue_cols) {
    suffix  <- sub("^union_end_cmc", "", ue)
    um      <- paste0("union_end_motive", suffix)
    ue_I    <- paste0("union_end_cmc_I",  suffix)
    if (!um %in% names(datos)) next
    idx_9999 <- !is.na(datos[[ue]]) & datos[[ue]] >= 9999L &
      !is.na(datos[[um]]) & as.character(datos[[um]]) == "separation"
    if (any(idx_9999)) {
      message(sum(idx_9999), " union_end_cmc", suffix,
              " = 9999 (refused/DK stop-living date) converted to NA",
              " (motive kept as separation)")
      datos[[ue]][idx_9999] <- NA_integer_
      if (ue_I %in% names(datos))
        datos[[ue_I]][idx_9999] <- 10L  # 10 = date unknown
    }
  }
  return (datos)
}

# check/flag dataset: FLAG (do not remove) individuals with incoherence in union
# and marriage dates, clean weight, and correct some missing or bad union ends.
# Bad-date women are marked with _I flags (15/50/51) and reported; the actual
# removal is done afterwards, case by case, by filterDateQuality().
checkENADID <- function(df) {
  return (cleanENADID(df, checkOnly = TRUE))
}

cleanENADID <- function(df, checkOnly = FALSE, correctUnionHistory = TRUE,
                        correctBirthHistory = FALSE, n_show = 10L) {
  
  df <- zap_labels(df)
  
  df <- union_end_9999_to_NA (df)
  
  if (isTRUE(checkOnly)) {
    listErrors <- list()
  }
  
  CORRECTION <- 0
  BAD_AGE <- 0
  BIRTHS_BADDATE <- 0
  
  # ==== Flag-reason audit trail ====
  #
  # Diagnostic only: reasonLog is never read by the flagging logic itself, so
  # writing to it cannot change which women get flagged. One row per
  # (woman, reason) -- a woman flagged for two reasons gets two rows, so
  # more than one row per ID marks a woman hit by more than one problem
  # at once. add_reason() self-gates on the LIVE value of checkOnly, so it
  # only logs a reason at the moment that reason is actually acted on by a
  # flag_bad() call a few lines below it -- including the temporary checkOnly
  # overrides used when correctUnionHistory / correctBirthHistory is FALSE.
  #
  #   category "ordering"      impossible sequence between two REPORTED
  #                            events (e.g. marriage before union start,
  #                            union end before union start/marriage). A
  #                            genuine logical inconsistency, not just a
  #                            missing/unknown date. Flagged 50/51; removed by
  #                            filterDateQuality(dropBadUnionDates = TRUE).
  #   category "date_quality"  single-field DK/implausible date (cmc > 9900,
  #                            i.e. year refused/unknown). Already carries flag
  #                            10/11 from compute_cmc_fields, so flag_bad() only
  #                            registers the ID; removed by filterDateQuality
  #                            (dropBadUnionDates).
  #   category "after_survey"  event after the interview date, with a value
  #                            that is NOT already >9900 -- i.e. NOT an
  #                            artifact of a DK year, a genuinely
  #                            inconsistent (but well-formed) date. Flagged 15;
  #                            removed by filterDateQuality(dropBadUnionDates).
  #   category "age"           bad age at survey. No date field to flag; removed
  #                            unconditionally (impossible age always dropped).
  #
  # NOTE: section 4 re-derives "after survey date" ids from union_start /
  # marriage_start / union_end cmc for ALL union orders. Because cmc = 9999
  # is always > any real surveyDate_cmc, most of its hits are really
  # "date_quality" cases surfacing a second time under a different label,
  # not a distinct interaction problem -- tag_end_survey() below splits
  # these back apart so the two are not conflated in the summary.
  reasonLog <- data.frame(ID = integer(0), reason = character(0),
                          category = character(0), order = integer(0))

  add_reason <- function(ids, reason, category, order = NA_integer_) {
    if (isTRUE(checkOnly) || length(ids) == 0L) return(invisible(NULL))
    reasonLog <<- rbind(reasonLog,
                        data.frame(ID = ids, reason = reason,
                                   category = category, order = order))
  }

  # ==== Flagging (replaces removal) ====
  #
  # cleanENADID no longer deletes rows. Every problem is recorded two ways:
  #   * bad_ids -- IDs of women with at least one bad date. The union/overlap
  #                CORRECTIONS below skip these women, exactly as the old code
  #                did by removing them first, so the corrections keep operating
  #                on clean data.
  #   * an _I flag written on the offending field (15 after-survey, 50/51
  #                ordering) so filterDateQuality() can drop the woman later.
  # date-quality problems (cmc > 9900) already carry flag 10/11, so for them
  # flag_bad() is called with no code -- it only registers the ID in bad_ids.
  bad_ids <- integer(0)

  # Register ids as bad and, when field_I + code are given, stamp the code on
  # that field. A complete no-op while checkOnly is TRUE (report only): bad_ids
  # then stays empty, so the checkOnly overlap report sees the full sample, just
  # as it did before this refactor.
  flag_bad <- function(ids, field_I = NULL, code = NULL) {
    if (isTRUE(checkOnly) || length(ids) == 0L) return(invisible(NULL))
    bad_ids <<- union(bad_ids, ids)
    if (is.null(field_I) || is.null(code)) return(invisible(NULL))
    if (field_I %in% names(df)) {
      col <- df[[field_I]]
      col[df$ID %in% ids] <- code
      df[[field_I]] <<- col
    }
  }

  # ==== Helpers ====
  
  # Print a compact sample of problematic rows (checkOnly mode only).
  # Shows llave_muj + the relevant CMC columns for the first n_show cases.
  show_cases <- function(xxx, ids, cols, label) {
    if (!checkOnly || length(ids) == 0L) return(invisible(NULL))
    listErrors[[label]] <<- subset(df, ID %in% ids)
    n     <- min(n_show, length(ids))
    shown <- dplyr::filter(xxx, ID %in% ids[seq_len(n)])
    shown <- dplyr::select(shown, dplyr::any_of(c("llave_muj", cols)))
    cat(sprintf("First %d of %d (%s):\n", n, length(ids), label))
    print(as.data.frame(shown), row.names = FALSE)
    cat("\n")
  }
  
  # Wrapper around check_end_survey(): logs WHY each hit fired ("date_quality"
  # vs genuine "after_survey", see registry above) AND stamps the flags.
  #   - DK/9999 after-survey hits are really year-missing cases: they keep their
  #     existing 10/11 flag, we only register them as bad (flag_bad, no code).
  #   - well-formed after-survey dates get flag 15 on their _I field.
  tag_end_survey <- function(df, varEvent, fieldLabel, order) {
    ids <- check_end_survey(df, varEvent)
    if (length(ids) == 0L) return(ids)
    val     <- df[[varEvent]][match(ids, df$ID)]
    isDK    <- !is.na(val) & val > 9900
    field_I <- paste0(sub("[0-9]+$", "", varEvent), "_I", order)
    add_reason(ids[isDK],  paste0(fieldLabel, "_after_survey_dk_year"), "date_quality", order)
    flag_bad(ids[isDK])
    add_reason(ids[!isDK], paste0(fieldLabel, "_after_survey"),         "after_survey",  order)
    flag_bad(ids[!isDK], field_I, 15L)
    return(ids)
  }
  
  check_end_survey <- function(df, varEvent) {
    if (!(varEvent %in% names(df))) return(c())
    df[[varEvent]][is.na(df[[varEvent]])] <- 0
    idx <- !is.na(df$surveyDate_cmc) & (df[[varEvent]] > df$surveyDate_cmc)
    if (sum(idx) > 0) {
      cat(sum(idx), "individuals with ", varEvent, "after survey date\n")
      idx2 <- df[[varEvent]][idx] >= 9999L
      if (any(idx2))
        cat(" ", sum(idx2), "due to missing year (cmc >= 9999)\n")
    }
    return(df$ID[idx])
  }
  
  if (exists("DEBUG_cleanENADID") && isTRUE(DEBUG_cleanENADID)) browser()
  cat("=============================== clean survey:",as.character(df$survey[1]), "===============================\n")
  checkImputedMonth(df)
  
  df <- addID(df)
  nIndiv <- nrow(df)
  # Minimal snapshot for the flag-reason audit trail below: a stable ID ->
  # llave_muj lookup for the summary (kept for symmetry with the old code).
  dfOrig <- df[, intersect(c("ID", "llave_muj"), names(df)), drop = FALSE]
  
  if (isFALSE(correctUnionHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly    <- TRUE
  }
  
  action <- if (checkOnly) "We FOUND" else "We FLAG"

  haveUnion1 <- "union_start_cmc1" %in% names(df) &&
    any(!is.na(df[["union_start_cmc1"]]))
  haveUnion2 <- "union_start_cmc2" %in% names(df) &&
    any(!is.na(df[["union_start_cmc2"]]))
  haveUnion3 <- "union_start_cmc3" %in% names(df) &&
    any(!is.na(df[["union_start_cmc3"]]))
  
  # ==== 1. Check / correct union 1 ====
  
  if (haveUnion1) {
    xxx <- df[, intersect(c("ID", "llave_muj",
                            "union_start_cmc1", "marriage_start_cmc1",
                            "union_end_cmc1", "surveyDate_cmc"), names(df))]
    
    # Marriage start before union start (impossible: marriage >= union start)
    xxx1 <- dplyr::filter(xxx, marriage_start_cmc1 < union_start_cmc1)
    if (nrow(xxx1) > 0) {
      cat(nrow(xxx1), "1st marriage before union\n")
      show_cases(xxx1, xxx1$ID,
                 c("union_start_cmc1", "marriage_start_cmc1"),
                 "marriage_start < union_start")
      add_reason(xxx1$ID, "marriage_before_union_start1", "ordering", 1)
      flag_bad(xxx1$ID, "marriage_start_cmc_I1", 50L)
    }

    # Union end before union start
    xxx2 <- dplyr::filter(xxx, union_end_cmc1 < union_start_cmc1)
    if (nrow(xxx2) > 0) {
      cat(nrow(xxx2), "1st union end before start\n")
      show_cases(xxx2, xxx2$ID,
                 c("union_start_cmc1", "union_end_cmc1"),
                 "union_end < union_start")
      add_reason(xxx2$ID, "union_end_before_union_start1", "ordering", 1)
      flag_bad(xxx2$ID, "union_end_cmc_I1", 51L)
    }

    # Union end before marriage start (marriage started after the union ended)
    xxx3 <- dplyr::filter(xxx, union_end_cmc1 < marriage_start_cmc1)
    if (nrow(xxx3) > 0) {
      cat(nrow(xxx3), "1st marriage start after union end\n")
      show_cases(xxx3, xxx3$ID,
                 c("marriage_start_cmc1", "union_end_cmc1"),
                 "union_end < marriage_start")
      add_reason(xxx3$ID, "union_end_before_marriage_start1", "ordering", 1)
      flag_bad(xxx3$ID, "union_end_cmc_I1", 51L)
    }

    # Implausible CMC values (>9900 signals a missing/unknown year). These
    # already carry flag 10/11 from compute_cmc_fields, so flag_bad() only
    # registers them as bad (removed later by filterDateQuality dropBadUnionDates).
    xxx4 <- dplyr::filter(xxx, union_start_cmc1 > 9900)
    if (nrow(xxx4) > 0) {
      cat(nrow(xxx4), "1st union start cmc more than 9900\n")
      show_cases(xxx4, xxx4$ID, c("union_start_cmc1"), "cmc > 9900")
      add_reason(xxx4$ID, "union_start1_dk_year", "date_quality", 1)
      flag_bad(xxx4$ID)
    }

    # union_end_9999_to_NA pre-empts separation cases (cmc=9999 -> NA);
    # non-separation 9999s (e.g. widowhood with DK year) keep cmc=9999/flag 10-11
    # and are removed later by filterDateQuality(dropBadUnionDates).
    xxx5 <- dplyr::filter(xxx, union_end_cmc1 > 9900)
    if (nrow(xxx5) > 0) {
      cat(nrow(xxx5), "1st union end cmc more than 9900\n")
      show_cases(xxx5, xxx5$ID, c("union_end_cmc1"), "cmc > 9900")
      add_reason(xxx5$ID, "union_end1_dk_year_non_separation", "date_quality", 1)
      flag_bad(xxx5$ID)
    }

    xxx6 <- dplyr::filter(xxx, marriage_start_cmc1 > 9900)
    if (nrow(xxx6) > 0) {
      cat(nrow(xxx6), "1st marriage start cmc more than 9900\n")
      show_cases(xxx6, xxx6$ID, c("marriage_start_cmc1"), "cmc > 9900")
      add_reason(xxx6$ID, "marriage_start1_dk_year", "date_quality", 1)
      flag_bad(xxx6$ID)
    }

    # Union start after survey date
    xxx7 <- dplyr::filter(xxx, union_start_cmc1 > surveyDate_cmc)
    if (nrow(xxx7) > 0) {
      cat(nrow(xxx7), "1st union start cmc after survey cmc\n")
      show_cases(xxx7, xxx7$ID,
                 c("union_start_cmc1", "surveyDate_cmc"),
                 "union_start > survey")
      # union_start1 > 9900 already logged as date_quality via xxx4 (keeps its
      # 10/11 flag); only the residual well-formed cases get flag 15 here.
      add_reason(setdiff(xxx7$ID, xxx4$ID), "union_start1_after_survey", "after_survey", 1)
      flag_bad(setdiff(xxx7$ID, xxx4$ID), "union_start_cmc_I1", 15L)
    }

    ids_flagged <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID,
                            xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID))

    if (length(ids_flagged) > 0)
      cat(action, length(ids_flagged),
          "individuals with problems in date of first union\n")
  }
  
  # ==== 2. Check / correct union 2 ====
  
  if (haveUnion2) {
    xxx <- df[, intersect(c("ID", "llave_muj",
                            "union_start_cmc1", "union_end_cmc1",
                            "union_start_cmc2", "marriage_start_cmc2",
                            "union_end_cmc2", "surveyDate_cmc"), names(df))]
    
    # Marriage start before union start
    xxx1 <- dplyr::filter(xxx, marriage_start_cmc2 < union_start_cmc2)
    if (nrow(xxx1) > 0) {
      cat(nrow(xxx1), "2nd marriage before union\n")
      show_cases(xxx1, xxx1$ID,
                 c("union_start_cmc2", "marriage_start_cmc2"),
                 "marriage_start2 < union_start2")
      add_reason(xxx1$ID, "marriage_before_union_start2", "ordering", 2)
      flag_bad(xxx1$ID, "marriage_start_cmc_I2", 50L)
    }

    # Union end before union start
    xxx2 <- dplyr::filter(xxx, union_end_cmc2 < union_start_cmc2)
    if (nrow(xxx2) > 0) {
      cat(nrow(xxx2), "2nd union end before start\n")
      show_cases(xxx2, xxx2$ID,
                 c("union_start_cmc2", "union_end_cmc2"),
                 "union_end2 < union_start2")
      add_reason(xxx2$ID, "union_end_before_union_start2", "ordering", 2)
      flag_bad(xxx2$ID, "union_end_cmc_I2", 51L)
    }

    # Union end before marriage start
    xxx3 <- dplyr::filter(xxx, union_end_cmc2 < marriage_start_cmc2)
    if (nrow(xxx3) > 0) {
      cat(nrow(xxx3), "2nd marriage start after union end\n")
      show_cases(xxx3, xxx3$ID,
                 c("marriage_start_cmc2", "union_end_cmc2"),
                 "union_end2 < marriage_start2")
      add_reason(xxx3$ID, "union_end_before_marriage_start2", "ordering", 2)
      flag_bad(xxx3$ID, "union_end_cmc_I2", 51L)
    }

    # Implausible CMC values (already flag 10/11: register only)
    xxx4 <- dplyr::filter(xxx, union_start_cmc2 > 9900)
    if (nrow(xxx4) > 0) {
      cat(nrow(xxx4), "2nd union start cmc more than 9900\n")
      show_cases(xxx4, xxx4$ID, c("union_start_cmc2"), "cmc > 9900")
      add_reason(xxx4$ID, "union_start2_dk_year", "date_quality", 2)
      flag_bad(xxx4$ID)
    }

    # union_end_9999_to_NA pre-empts separation cases (cmc=9999 -> NA);
    # non-separation 9999s (e.g. widowhood with DK year) keep cmc=9999/flag 10-11
    # and are removed later by filterDateQuality(dropBadUnionDates).
    xxx5 <- dplyr::filter(xxx, union_end_cmc2 > 9900)
    if (nrow(xxx5) > 0) {
      cat(nrow(xxx5), "2nd union end cmc more than 9900\n")
      show_cases(xxx5, xxx5$ID, c("union_end_cmc2"), "cmc > 9900")
      add_reason(xxx5$ID, "union_end2_dk_year_non_separation", "date_quality", 2)
      flag_bad(xxx5$ID)
    }

    xxx6 <- dplyr::filter(xxx, marriage_start_cmc2 > 9900)
    if (nrow(xxx6) > 0) {
      cat(nrow(xxx6), "2nd marriage start cmc more than 9900\n")
      show_cases(xxx6, xxx6$ID, c("marriage_start_cmc2"), "cmc > 9900")
      add_reason(xxx6$ID, "marriage_start2_dk_year", "date_quality", 2)
      flag_bad(xxx6$ID)
    }

    # Union start after survey date
    xxx7 <- dplyr::filter(xxx, union_start_cmc2 > surveyDate_cmc)
    if (nrow(xxx7) > 0) {
      cat(nrow(xxx7), "2nd union start cmc after survey cmc\n")
      show_cases(xxx7, xxx7$ID,
                 c("union_start_cmc2", "surveyDate_cmc"),
                 "union_start2 > survey")
      add_reason(setdiff(xxx7$ID, xxx4$ID), "union_start2_after_survey", "after_survey", 2)
      flag_bad(setdiff(xxx7$ID, xxx4$ID), "union_start_cmc_I2", 15L)
    }

    # NOTE: women with union_start_cmc2 but no union_end_cmc1 are NOT flagged
    # here — they are corrected below in the overlap block (union_end_cmc1 is
    # set to union_start_cmc2 - 1), consistent with the union 3 correction.
    ids_flagged <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID,
                            xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID))

    if (length(ids_flagged) > 0)
      cat(action, length(ids_flagged),
          "individuals with problems in date of second union\n")


    # --- Overlap between union 1 end and union 2 start ---
    #
    # Checks only union_start/end dates — marriage dates are irrelevant here
    # since we are correcting the boundary between two consecutive unions.
    #
    # When correctUnionHistory = TRUE: corrected rather than removed.
    # When checkOnly = TRUE: reported with sample cases, not corrected.
    #
    # Correction rules:
    #   (a) union_end_cmc1 is NA               -> set to union_start_cmc2 - 1
    #   (b) union_end_cmc1 > union_start_cmc2  -> set to union_start_cmc2 - 1
    
    # Skip women already flagged bad: the old code removed them before this
    # correction ran, so they never reached the overlap logic (and must not be
    # counted in CORRECTION now).
    xxxOK <- dplyr::filter(xxx, !(ID %in% bad_ids))
    overlap_no_end <- dplyr::filter(xxxOK, !is.na(union_start_cmc2) &
                                      is.na(union_end_cmc1))
    overlap_bad    <- dplyr::filter(xxxOK, !is.na(union_start_cmc2) &
                                      !is.na(union_end_cmc1) &
                                      union_start_cmc2 < union_end_cmc1)
    
    if (nrow(overlap_no_end) > 0) {
      cat(nrow(overlap_no_end), "2nd union start with no end of 1st union\n")
      show_cases(overlap_no_end, overlap_no_end$ID,
                 c("union_end_cmc1", "union_start_cmc2"),
                 "no union_end1, union_start2 present")
    }
    if (nrow(overlap_bad) > 0) {
      cat(nrow(overlap_bad), "2nd union before end of 1st union\n")
      show_cases(overlap_bad, overlap_bad$ID,
                 c("union_end_cmc1", "union_start_cmc2"),
                 "union_start2 < union_end1")
    }
    
    ids_no_end  <- overlap_no_end$ID
    ids_overlap <- overlap_bad$ID
    ids_fix     <- unique(c(ids_no_end, ids_overlap))
    
    if (length(ids_fix) > 0) {
      message <-"individuals with date of second union, 1. before the end of first union, or 2. no end of first union\n"
      if (checkOnly) {
        cat("We FOUND", length(ids_fix),
            message)
      } else {
        CORRECTION <- CORRECTION + length(ids_fix)
        cat("We CORRECT", length(ids_fix),
            message)
        df$union_end_cmc1[df$ID %in% ids_no_end]  <-
          df$union_start_cmc2[df$ID %in% ids_no_end]  - 1L
        df$union_end_cmc1[df$ID %in% ids_overlap] <-
          df$union_start_cmc2[df$ID %in% ids_overlap] - 1L
        if ("union_end_cmc_I1" %in% names(df)) {
          # 20 = was cmc=9999 DK (flag 10), nulled then back-filled
          # 21 = was genuinely NA (flag NA or other), back-filled
          # ifelse(NA == 10L, ...) returns NA, so guard with !is.na()
          prior_I1 <- df$union_end_cmc_I1[df$ID %in% ids_no_end]
          df$union_end_cmc_I1[df$ID %in% ids_no_end]  <- ifelse(!is.na(prior_I1) & prior_I1 == 10L, 20L, 21L)
          df$union_end_cmc_I1[df$ID %in% ids_overlap] <- 22L # recorded end after next union start
        }
      }
    }
  }
  
  
  # ==== 3. Check / correct union 3 ====
  
  if (haveUnion3) {
    xxx <- df[, intersect(c("ID", "llave_muj",
                            "union_end_cmc2", "union_start_cmc3",
                            "surveyDate_cmc"), names(df))]
    
    # --- Overlap between union 2 end and union 3 start ---
    #
    # Same correction logic as union 1/2 overlap above.
    # Marriage dates are not relevant for the boundary correction.
    # Skip women already flagged bad (they were removed before this ran before).

    xxxOK <- dplyr::filter(xxx, !(ID %in% bad_ids))

    xxx1 <- dplyr::filter(xxxOK, union_end_cmc2 > union_start_cmc3)
    if (nrow(xxx1) > 0) {
      cat(nrow(xxx1), "3rd union start before 2nd union end\n")
      show_cases(xxx1, xxx1$ID,
                 c("union_end_cmc2", "union_start_cmc3"),
                 "union_start3 < union_end2")
    }

    xxx2 <- dplyr::filter(xxxOK, is.na(union_end_cmc2) & !is.na(union_start_cmc3))
    if (nrow(xxx2) > 0) {
      cat(nrow(xxx2), "3rd union start and no 2nd union end\n")
      show_cases(xxx2, xxx2$ID,
                 c("union_end_cmc2", "union_start_cmc3"),
                 "no union_end2 but union_start3 present")
    }
    
    ids_fix <- unique(c(xxx1$ID, xxx2$ID))
    
    if (length(ids_fix) > 0) {
      message <-"individuals with date of third union, 1. before the end of second union, or 2. no end of second union\n"
      if (checkOnly) {
        cat("We FOUND", length(ids_fix),
            message)
      } else {
        CORRECTION <- CORRECTION + length(ids_fix)
        cat("We CORRECT", length(ids_fix),
            message)
        ids_no_end2  <- xxx2$ID
        ids_overlap2 <- xxx1$ID
        df$union_end_cmc2[df$ID %in% ids_no_end2]  <-
          df$union_start_cmc3[df$ID %in% ids_no_end2]  - 1L
        df$union_end_cmc2[df$ID %in% ids_overlap2] <-
          df$union_start_cmc3[df$ID %in% ids_overlap2] - 1L
        if ("union_end_cmc_I2" %in% names(df)) {
          # 20 = was cmc=9999 DK (flag 10), nulled then back-filled
          # 21 = was genuinely NA (flag NA or other), back-filled
          prior_I2 <- df$union_end_cmc_I2[df$ID %in% ids_no_end2]
          df$union_end_cmc_I2[df$ID %in% ids_no_end2]  <- ifelse(!is.na(prior_I2) & prior_I2 == 10L, 20L, 21L)
          df$union_end_cmc_I2[df$ID %in% ids_overlap2] <- 22L # recorded end after next union start
        }
      }
    }
  }
  
  
  # ==== 4. Check dates against survey date ====
  
  if (haveUnion1) {
    if (!all(is.na(df$nUnion))) {
      maxU        <- max(df$nUnion, na.rm = TRUE)
      ids_flagged <- c()

      # tag_end_survey() sets flag 15 on the offending field as a side effect
      # (well-formed after-survey dates only; DK/9999 keep their 10/11 flag).
      for (u in seq_len(maxU)) {
        ids_flagged <- c(ids_flagged,
                         tag_end_survey(df, paste0("union_start_cmc",    u), "union_start",    u),
                         tag_end_survey(df, paste0("marriage_start_cmc", u), "marriage_start", u),
                         tag_end_survey(df, paste0("union_end_cmc",      u), "union_end",      u))
      }

      ids_flagged <- unique(ids_flagged)
      if (length(ids_flagged) > 0)
        cat(action, length(ids_flagged),
            "women with date of union events after date of survey\n")
    }
  }
  
  if (isFALSE(correctUnionHistory)) checkOnly <- checkOnlyMem
  
  
  # ==== 5. Check / correct birth history ====
  
  if (isFALSE(correctBirthHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly    <- TRUE
  }
  
  action <- if (checkOnly) "We FOUND" else "We FLAG"

  maxB        <- max(df$nBioKids)
  ids_flagged <- c()

  # With the default correctBirthHistory = FALSE, checkOnly is TRUE here so the
  # dob dates are only reported (not flagged 15) -- births are dropped elsewhere
  # (cleanBH in "ENADID fertility.R"). Pass correctBirthHistory = TRUE to also
  # set flag 15 on dob dates after the survey.
  for (b in seq_len(maxB)) {
    ids_flagged <- c(ids_flagged,
                     tag_end_survey(df, paste0("dob_cmc", b), "dob", b))
  }

  ids_flagged <- unique(ids_flagged)
  if (length(ids_flagged) > 0)
    BIRTHS_BADDATE <- length(ids_flagged)
  cat(action, length(ids_flagged),
      "women with date of births after date of survey\n")

  if (isFALSE(correctBirthHistory)) checkOnly <- checkOnlyMem


  # ==== 6. Finalise ====
  # Bad ages: reported/logged here, but NOT removed here. filterDateQuality drops
  # them unconditionally. (indiv_age_survey is not a date field, no _I flag.)
  bad_ages <- subset(df, (indiv_age_survey < 0) | (indiv_age_survey > 100))
  if (nrow(bad_ages) > 0) {
    BAD_AGE <- nrow(bad_ages)
    cat(if (checkOnly) "We FOUND" else "We FLAG", nrow(bad_ages),
        "women with age negative or greater than 100\n")
    add_reason(bad_ages$ID, "bad_age", "age")
  }

  df$ID <- NULL

  # ---- Concise per-survey report ----
  # cleanENADID only SETS flags. The canonical statistics come from
  # summarizeDateQuality() on the FINAL data (run it after the 2006-10 model so
  # its flags 41/42 are visible). Here we just echo what this pass produced,
  # using the SAME shared detectors so the numbers are consistent. The model
  # corrections (41/42) are not set yet, so n_corrected here reflects only the
  # back-fills (20/21/22) and divorce-date substitutions (30/31).
  n_bad_union <- sum(rowsBadUnion(df))
  n_corrected <- sum(rowsWithFlag(df, "union_end_cmc", CORRECTED_FLAGS))
  n_bad_birth <- sum(rowsBadBirth(df))

  cat("=== survey", as.character(df$survey[1]), "-", nIndiv, "women ===\n")
  cat("  bad union dates (would be removed):", n_bad_union, "\n")
  cat("  union end corrected so far (20/21/22/30/31):", n_corrected, "\n")
  cat("  bad birth dates (removed only for birth analysis):", n_bad_birth, "\n")
  cat("  bad age, always removed (<0 or >100):", BAD_AGE, "\n")

  idx <- is.na(df$indiv_weight)
  if (sum(idx) > 0) {
    df$indiv_weight[idx] <- 1
    cat(sum(idx), "weights were NA and set to 1\n")
  }
  
  cat("=============================== finished survey:",as.character(df$survey[1]), "===============================\n")
  
  if (checkOnly) {
    return(listErrors)
  } else {
    return(df)
  }
}


# ==== Date-quality flag groups + shared row detectors ====
#
# One place defining which _I flag codes mean what, used by BOTH the removal
# step (filterDateQuality) and the summary (summarizeDateQuality) so the two can
# never drift apart. See the "_I code registry" comment above for every code.
#
#   BAD_UNION_FLAGS  bad union date, NOT repaired: 10/11 (unknown), 15 (after
#                    survey), 50/51 (ordering). Removed by Option 1.
#   BACKFILL_FLAGS   union end imputed as next-union-start - 1: 20/21/22.
#                    Removed by Option 2.
#   CORRECTED_FLAGS  union end repaired/substituted/imputed: 20/21/22 (later
#                    union), 30/31 (divorce date), 41/42 (2006-10 model).
#                    Counted as "corrected" in the summary.
#   BAD_BIRTH_FLAGS  bad birth (dob) date: 10/11 (unknown), 15 (after survey).
#                    Removed by Option 3, which ALSO catches dob after the survey
#                    by value (births are not flagged 15 when the default
#                    correctBirthHistory = FALSE). See rowsBadBirth().

UNION_DATE_BASES <- c("union_start_cmc", "marriage_start_cmc", "union_end_cmc")
BIRTH_DATE_BASE  <- "dob_cmc"
BAD_UNION_FLAGS  <- c(10L, 11L, 15L, 50L, 51L)
BACKFILL_FLAGS   <- c(20L, 21L, 22L)
CORRECTED_FLAGS  <- c(20L, 21L, 22L, 30L, 31L, 41L, 42L)
BAD_BIRTH_FLAGS  <- c(10L, 11L, 15L)

# numeric order suffixes of a {base}{n} family actually present in df
ordersPresent <- function(df, base) {
  cols <- grep(paste0("^", base, "[0-9]+$"), names(df), value = TRUE)
  sort(as.integer(sub(paste0("^", base), "", cols)))
}

# logical row mask: TRUE where any {base}_I{order} flag is in `codes`.
# `orders = NULL` scans every order present; pass e.g. 1 for first-union-only.
rowsWithFlag <- function(df, bases, codes, orders = NULL) {
  mask <- rep(FALSE, nrow(df))
  for (base in bases) {
    ords <- if (is.null(orders)) ordersPresent(df, base) else orders
    cols <- intersect(paste0(base, "_I", ords), names(df))
    for (cc in cols) mask <- mask | (df[[cc]] %in% codes)
  }
  mask
}

# logical row mask: TRUE where any raw {base}{order} cmc > 9900 (year unknown).
# Belt-and-braces for cases where the _I flag was not set (e.g. non-separation
# DK-year ends that keep cmc = 9999).
rowsWithRaw9999 <- function(df, bases, orders = NULL) {
  mask <- rep(FALSE, nrow(df))
  for (base in bases) {
    ords <- if (is.null(orders)) ordersPresent(df, base) else orders
    cols <- intersect(paste0(base, ords), names(df))
    for (cc in cols) mask <- mask | (!is.na(df[[cc]]) & df[[cc]] > 9900L)
  }
  mask
}

# logical row mask: TRUE where any raw {base}{order} cmc is after the survey.
# Value-based detection, independent of whether flag 15 was set. Needed for birth
# dates: they are NOT flagged when correctBirthHistory = FALSE (the default),
# whereas union dates ARE (correctUnionHistory defaults TRUE). Matches the
# dob_cmc > surveyDate_cmc test in NSFG_validation/NSFG_val_lib.R.
rowsAfterSurvey <- function(df, bases, orders = NULL) {
  mask <- rep(FALSE, nrow(df))
  if (!("surveyDate_cmc" %in% names(df))) return(mask)
  sd <- df$surveyDate_cmc
  for (base in bases) {
    ords <- if (is.null(orders)) ordersPresent(df, base) else orders
    cols <- intersect(paste0(base, ords), names(df))
    for (cc in cols) mask <- mask | (!is.na(df[[cc]]) & !is.na(sd) & df[[cc]] > sd)
  }
  mask
}

# --- the two target populations, defined once and reused everywhere ---

# bad, un-repaired UNION date (filterDateQuality Option 1 / summary removed_union)
rowsBadUnion <- function(df, orders = NULL) {
  rowsWithFlag(df, UNION_DATE_BASES, BAD_UNION_FLAGS, orders) |
    rowsWithRaw9999(df, UNION_DATE_BASES, orders)
}

# bad BIRTH date (filterDateQuality Option 3 / summary removed_birth). Includes
# the value-based after-survey test so births found-but-not-flagged (default
# correctBirthHistory = FALSE) are still counted.
rowsBadBirth <- function(df) {
  rowsWithFlag(df, BIRTH_DATE_BASE, BAD_BIRTH_FLAGS) |
    rowsWithRaw9999(df, BIRTH_DATE_BASE) |
    rowsAfterSurvey(df, BIRTH_DATE_BASE)
}


# ==== Removal step: filterDateQuality (three switches) ====
#
# Call after cleanENADID(). Exactly three removal options, plus an unconditional
# bad-age drop (impossible ages are unusable for any analysis):
#
#   1. dropBadUnionDates (TRUE)  bad, un-repaired union dates: flags 10/11/15/
#        50/51 or raw cmc>9900 on union_start / marriage_start / union_end.
#        NB this now also drops flag-10 DK separation ends (previously kept).
#   2. dropBackfilledEnd (FALSE) union end imputed as next-union-start - 1
#        (flags 20/21/22).
#   3. dropBadBirthDates (FALSE) bad birth dates: flags 10/11/15, raw cmc>9900,
#        or dob after the survey (by value). OFF by default so union-history
#        analysis keeps these women.
#
#   # union-history analysis (default): drop bad union dates + bad age
#   ENADID <- filterDateQuality(ENADID)
#
#   # also require a usable, non-imputed union end (survival / dissolution):
#   pooled_dur <- filterDateQuality(pooled, dropBackfilledEnd = TRUE)
#
#   # birth-history analysis: also drop bad birth dates
#   births <- filterDateQuality(pooled, dropBadBirthDates = TRUE)
#
#   # restrict the union checks to first unions only
#   pooled1 <- filterDateQuality(pooled, unionOrders = 1)

filterDateQuality <- function(df,
                              dropAllBad        = FALSE,
                              dropBadUnionDates = TRUE,
                              dropBackfilledEnd = FALSE,
                              dropBadBirthDates = FALSE,
                              unionOrders       = NULL,
                              verbose           = TRUE) {

  nStart <- nrow(df)
  keep   <- rep(TRUE, nStart)

  if (isTRUE(dropAllBad)) {
    dropBadUnionDates <- TRUE
    dropBackfilledEnd <- FALSE
    dropBadBirthDates <- FALSE
  }
  
  # Drop rows in `mask`; report only those not already removed, so the printed
  # counts are non-overlapping and sum to the total.
  report <- function(mask, msg) {
    n <- sum(mask & keep)
    if (verbose && n > 0L) cat(n, "women removed:", msg, "\n")
    keep <<- keep & !mask
  }

  # 1. bad, un-repaired union dates (flags 10/11/15/50/51 or raw cmc>9900)
  if (isTRUE(dropBadUnionDates)) {
    report(rowsBadUnion(df, unionOrders),
           "bad union dates (flag 10/11/15/50/51 or cmc>9900)")
  }

  # 2. union end imputed as next-union-start - 1 (flags 20/21/22)
  if (isTRUE(dropBackfilledEnd)) {
    report(rowsWithFlag(df, "union_end_cmc", BACKFILL_FLAGS, unionOrders),
           "union end back-filled to next union start - 1 (flag 20/21/22)")
  }

  # 3. bad birth dates (flags 10/11/15, raw cmc>9900, or dob after survey)
  if (isTRUE(dropBadBirthDates)) {
    report(rowsBadBirth(df),
           "bad birth dates (flag 10/11/15, cmc>9900, or after survey)")
  }

  # Always: impossible age at survey (< 0 or > 100) -- unusable for any analysis
  if ("indiv_age_survey" %in% names(df)) {
    report(!is.na(df$indiv_age_survey) &
             (df$indiv_age_survey < 0 | df$indiv_age_survey > 100),
           "age negative or greater than 100")
  }

  if (verbose)
    cat(nStart - sum(keep), "women removed in total of", nStart,
        "by filterDateQuality()\n")

  df[keep, , drop = FALSE]
}

convertToFlag10 <- function (df) {
  # for statistics: use flag 10 for all bad data (flags )
}

# ==== Summary: summarizeDateQuality (three headline stats) ====
#
# Run on the FINAL processed data (after cleanENADID AND the 2006-10 model, so
# the model flags 41/42 are visible). Returns one row per survey plus a Total
# row:
#
#   removed_union    women who would be dropped for bad, un-repaired union dates
#                    (= filterDateQuality(dropBadUnionDates = TRUE), the default)
#   corrected_union  women whose union end was repaired: later union (20/21/22),
#                    divorce date (30/31) or the 2006-10 model (41/42)
#   removed_birth    women who would be dropped for bad birth dates
#                    (= filterDateQuality(dropBadBirthDates = TRUE))
#   removed_bad_age  women with impossible age (always dropped)
#
# The columns answer different questions and CAN overlap (a woman may have both
# a bad union date and a corrected end on another union), so do not add them.

summarizeDateQuality <- function(df, by = "survey") {
  grp <- if (by %in% names(df)) as.character(df[[by]]) else rep("all", nrow(df))
  grp[is.na(grp)] <- "<NA>"

  m <- data.frame(
    grp             = grp,
    nWomen          = 1L,
    removed_union   = as.integer(rowsBadUnion(df)),
    corrected_union = as.integer(rowsWithFlag(df, "union_end_cmc", CORRECTED_FLAGS)),
    removed_birth   = as.integer(rowsBadBirth(df)),
    removed_bad_age = as.integer(
      if ("indiv_age_survey" %in% names(df))
        !is.na(df$indiv_age_survey) &
          (df$indiv_age_survey < 0 | df$indiv_age_survey > 100)
      else rep(FALSE, nrow(df))),
    stringsAsFactors = FALSE
  )

  out <- aggregate(cbind(nWomen, removed_union, corrected_union,
                         removed_birth, removed_bad_age) ~ grp,
                   data = m, FUN = sum)
  out <- rbind(out, data.frame(
    grp             = "Total",
    nWomen          = nrow(df),
    removed_union   = sum(m$removed_union),
    corrected_union = sum(m$corrected_union),
    removed_birth   = sum(m$removed_birth),
    removed_bad_age = sum(m$removed_bad_age),
    stringsAsFactors = FALSE))
  names(out)[names(out) == "grp"] <- by
  out
}


imputed_month <- function(n, range = 1:12, year_vec = NULL,
                          survey_cmc = NULL, capped = FALSE) {
  # Backward-compatible wrapper. With no year context it behaves exactly as
  # before (a pure random month). Pass year_vec (+ survey_cmc) to opt into the
  # survey-date cap; pass capped = TRUE to also enable the probabilistic layer
  # via imputed_month_capped().
  if (is.null(year_vec)) return(sample(range, n, replace = TRUE))
  imputed_month_capped(n, year_vec, range = range,
                       survey_cmc = survey_cmc, capped = capped)
}

# ==== Helper: month imputation capped to the survey period or constrained by a previous event ====
imputed_month_capped <- function(n, year_vec,
                                 range         = 1:12,
                                 survey_cmc    = NULL,
                                 after_cmc     = NULL,
                                 after_cmc_imp = 0,    # 0 = strict-after, 1 = probabilistic split
                                 propBefore    = 0.2,
                                 capped        = TRUE) {
  # Impute a random month for each of n events, with optional constraints.
  #
  # capped: master switch for the PROBABILISTIC layer only.
  #   TRUE  -> full behaviour: survey cap + strict-after ordering + the
  #            propBefore birth-vs-union split.
  #   FALSE -> "simple" mode: the survey cap and strict-after ordering still
  #            apply (both are HARD constraints, never toggled), but the
  #            probabilistic split is disabled. With no constraints supplied at
  #            all this is just a uniform draw, i.e. the legacy imputed_month().
  #
  # survey_cmc: per-individual survey CMC. Caps the draw so the result does
  #   not exceed it. Binds only when year_vec[i] == year of survey_cmc[i].
  #
  # after_cmc: per-individual prior-event CMC (e.g. union start). Behaviour
  #   depends on after_cmc_imp[i], and binds only in the same year:
  #
  #     after_cmc_imp == 0  -> the prior month is KNOWN and trusted. The birth
  #       is drawn STRICTLY AFTER it (original behaviour). The prior date is
  #       used as-is.
  #
  #     after_cmc_imp == 1  -> the prior month was itself IMPUTED. Across all
  #       same-year cases of this kind, a propBefore share of births is placed
  #       BEFORE the (fixed) union month, the rest ON-OR-AFTER it (same month
  #       counts as "during", not before). The union month is NOT touched.
  #
  # FEASIBILITY: "before" needs at least one month earlier in the year than the
  #   union month, so cases where the union was imputed to the first month of
  #   the range cannot be "before" and fall to "on-or-after". This nudges the
  #   realised share just under propBefore (by the fraction of joint cases with
  #   union month > min(range); ~11/12 if union months are uniform). See note
  #   below the function for an exact-target variant.
  #
  # GOTCHAS (unchanged): `range` captured as `valid_range` to avoid base::range;
  #   sample_one() guards against sample(scalar, 1) being read as sample(1:x, 1).
  
  if (length(year_vec) == 1L) year_vec <- rep(year_vec, n)
  
  sample_one <- function(x) {
    if (length(x) == 1L) return(x)
    sample(x, 1L)
  }
  
  valid_range <- range
  
  lo <- rep(min(valid_range), n)
  hi <- rep(max(valid_range), n)
  
  # --- Upper bound from survey date (same year only) ---
  if (!is.null(survey_cmc)) {
    if (length(survey_cmc) == 1L) survey_cmc <- rep(survey_cmc, n)
    upper_idx <- !is.na(year_vec) & !is.na(survey_cmc) &
      (year_vec == cmc_to_year(survey_cmc))
    if (any(upper_idx)) {
      hi[upper_idx] <- survey_cmc[upper_idx] -
        compute_cmc(1L, year_vec[upper_idx]) + 1L
    }
  }
  
  # --- Lower bound / ordering from the prior event (same year only) ---
  if (!is.null(after_cmc)) {
    if (length(after_cmc)     == 1L) after_cmc     <- rep(after_cmc,     n)
    if (length(after_cmc_imp) == 1L) after_cmc_imp <- rep(after_cmc_imp, n)
    if (length(propBefore)    == 1L) propBefore    <- rep(propBefore,    n)
    
    same_year <- !is.na(year_vec) & !is.na(after_cmc) &
      (year_vec == cmc_to_year(after_cmc))
    
    # (a) Prior month known -> strictly after (original logic, relaxed if the
    #     prior event took the last month of the range).
    strict_idx <- same_year & (after_cmc_imp == 0)
    if (any(strict_idx)) {
      lo_strict    <- after_cmc[strict_idx] -
        compute_cmc(1L, year_vec[strict_idx]) + 2L
      lo_nonstrict <- lo_strict - 1L
      lo[strict_idx] <- ifelse(lo_strict <= hi[strict_idx],
                               lo_strict, lo_nonstrict)
    }
    
    # (b) Prior month imputed -> propBefore split around the fixed union month.
    #     This probabilistic layer is the ONLY part gated by `capped`. In simple
    #     mode (capped = FALSE) these cases fall through to a uniform draw within
    #     [range], capped by the survey date, with no before/after preference.
    if (isTRUE(capped)) {
      joint_idx <- same_year & (after_cmc_imp == 1)
      if (any(joint_idx)) {
        um <- after_cmc[joint_idx] -
          compute_cmc(1L, year_vec[joint_idx]) + 1L        # union month, 1..K
        
        before_feasible <- um > min(valid_range)           # room for a month before
        pick_before <- (runif(length(um)) < propBefore[joint_idx]) & before_feasible
        
        lo[joint_idx] <- ifelse(pick_before, min(valid_range), um)
        hi[joint_idx] <- ifelse(pick_before, um - 1L,         hi[joint_idx])
      }
    }
  }
  
  # --- Draw within [lo, hi], redrawing only where the first draw misses ---
  months <- sample(valid_range, size = n, replace = TRUE)
  needs_redraw <- (months < lo) | (months > hi)
  
  if (any(needs_redraw)) {
    months[needs_redraw] <- mapply(
      function(l, h, yr) {
        valid <- valid_range[valid_range >= l & valid_range <= h]
        if (length(valid) > 0L) {
          sample_one(valid)
        } else {
          warning(sprintf(
            "imputed_month_capped: no valid month (year %d, lo %d, hi %d); using nearest boundary",
            yr, l, h))
          sample_one(valid_range[which.min(abs(valid_range - (l + h) / 2))])
        }
      },
      lo[needs_redraw], hi[needs_redraw], year_vec[needs_redraw]
    )
  }
  
  months
}

imputed_month_capped_old <- function(n, year_vec,
                                     range      = 1:12,
                                     survey_cmc = NULL,
                                     after_cmc  = NULL) {
  # Impute a random month for each of n events, with two optional constraints:
  #
  # survey_cmc: per-individual survey date CMC (vector of length n, or scalar).
  #   For case i, the draw is capped so the resulting CMC does not exceed
  #   survey_cmc[i]. Only binds when year_vec[i] == year of survey_cmc[i].
  #
  # after_cmc: per-individual lower-bound CMC (vector of length n, or scalar).
  #   For case i, the draw is strictly greater than after_cmc[i]. Only binds
  #   when year_vec[i] == year of after_cmc[i]. NA entries are unconstrained.
  #   If strict ordering is impossible (after_cmc month == max of valid range),
  #   the constraint is relaxed to allow equality rather than emitting a warning.
  #
  # IMPORTANT NOTES ON R GOTCHAS FIXED HERE:
  #
  # (1) `range` is captured as `valid_range` before mapply to prevent R from
  #     resolving `range` to base::range() inside the anonymous function scope.
  #
  # (2) sample(x, 1L) where x is a length-1 integer is dangerous in R: if x
  #     is a scalar n, R interprets it as sample(1:n, 1) rather than returning
  #     x. Fixed by using sample_one() which returns scalar values directly.
  
  if (length(year_vec) == 1L) year_vec <- rep(year_vec, n)
  
  # Safe single-draw: avoids sample(n, 1) being interpreted as sample(1:n, 1)
  sample_one <- function(x) {
    if (length(x) == 1L) return(x)
    sample(x, 1L)
  }
  
  # Capture the range parameter before any scope issues in mapply
  valid_range <- range
  
  cmc_to_year <- function(cmc) (cmc - 1L) %/% 12L + 1900L
  
  # Initialise bounds from valid_range for every individual
  lo <- rep(min(valid_range), n)
  hi <- rep(max(valid_range), n)
  
  # Tighten upper bound where year_vec[i] == year of survey_cmc[i]
  if (!is.null(survey_cmc)) {
    if (length(survey_cmc) == 1L) survey_cmc <- rep(survey_cmc, n)
    upper_idx <- !is.na(year_vec) & !is.na(survey_cmc) &
      (year_vec == cmc_to_year(survey_cmc))
    if (any(upper_idx)) {
      hi[upper_idx] <- survey_cmc[upper_idx] -
        compute_cmc(1L, year_vec[upper_idx]) + 1L
    }
  }
  
  # Tighten lower bound where year_vec[i] == year of after_cmc[i].
  # Strict: lo = after_cmc month + 1.
  # If strict lo would exceed hi, relax to non-strict (lo = after_cmc month)
  # so at least the same month is allowed rather than producing an impossible
  # constraint. This covers the edge case where a prior event drew the last
  # available month in the year.
  if (!is.null(after_cmc)) {
    if (length(after_cmc) == 1L) after_cmc <- rep(after_cmc, n)
    lower_idx <- !is.na(year_vec) & !is.na(after_cmc) &
      (year_vec == cmc_to_year(after_cmc))
    if (any(lower_idx)) {
      lo_strict    <- after_cmc[lower_idx] -
        compute_cmc(1L, year_vec[lower_idx]) + 2L
      lo_nonstrict <- lo_strict - 1L
      lo[lower_idx] <- ifelse(lo_strict <= hi[lower_idx], lo_strict, lo_nonstrict)
    }
  }
  
  # Initial unconstrained draw within valid_range
  months <- sample(valid_range, size = n, replace = TRUE)
  
  # Redraw cases where initial draw falls outside [lo, hi]
  needs_redraw <- (months < lo) | (months > hi)
  
  if (any(needs_redraw)) {
    months[needs_redraw] <- mapply(
      function(l, h, yr) {
        valid <- valid_range[valid_range >= l & valid_range <= h]
        if (length(valid) > 0L) {
          sample_one(valid)
        } else {
          warning(sprintf(
            "imputed_month_capped: no valid month (year %d, lo %d, hi %d); using nearest boundary",
            yr, l, h))
          sample_one(valid_range[which.min(abs(valid_range - (l + h) / 2))])
        }
      },
      lo[needs_redraw], hi[needs_redraw], year_vec[needs_redraw]
    )
  }
  
  months
}


reweight <- function(df, varWeight="weight") {
  # divide weights by their mean in order to get relative weights only
  df$country <- factor(df$country)
  countries <- names(table(df$country))
  df$survey <- factor(df$survey)
  surveys <- names(table(df$survey))
  # if any weight is NA, set to 1
  df$indiv_weight [is.na(df$indiv_weight)] <- 1
  if (!(varWeight %in% names(df))) df[[varWeight]] <- NA
  for (aCountry in countries) {
    for (aSurvey in surveys) {
      idx <- (df$country==aCountry) & (df$survey==aSurvey)
      sumWeights <- sum(df$indiv_weight[idx])
      nObs <- length(df$indiv_weight[idx])
      df[[varWeight]][idx] <- df$indiv_weight[idx] * nObs / sumWeights
    }
  }
  df <- relocate(df, weight, .after = indiv_weight)
  return (df)
}

addWeights <- function (df, popData) {
  # Pivot the population reference table to a long format for joining
  pop_long <- popData %>%
    tidyr::pivot_longer(
      cols      = -Age,
      names_to  = "survey",
      values_to = "pop_count"
    )
  names(pop_long)[1] <- "indiv_age_survey"
  
  # For each survey x age cell, compute the sum of individual weights
  weight_sums <- df %>%
    dplyr::group_by(survey, indiv_age_survey) %>%
    dplyr::summarise(weight_sum = sum(weight), .groups = "drop")
  
  # Join both onto the individual-level data and compute popWeight
  df <- df %>%
    dplyr::left_join(weight_sums, by = c("survey", "indiv_age_survey")) %>%
    dplyr::left_join(pop_long,    by = c("survey", "indiv_age_survey")) %>%
    dplyr::mutate(
      popWeight = weight * (pop_count / weight_sum)
    ) %>%
    dplyr::select(-weight_sum, -pop_count)
  
  df <- relocate(df, popWeight, .after = weight)
  
  #### check popWeight ####
  check_popWeight <- df %>%
    dplyr::group_by(survey, indiv_age_survey) %>%
    dplyr::summarise(pop_count = sum(popWeight), .groups = "drop")
  
  compared <- comparePopCount(pop_long, check_popWeight)
  result   <- summariseError(compared) 
  
  # if the age group has very few women, then popWeight will be very big
  # We set popWeight to the mean value of popWeight if it exceeds 6 times the mean
  mean_popWeight <- df %>%
    dplyr::group_by(survey) %>%
    dplyr::summarise(mean = mean(popWeight), .groups = "drop")
  
  df <- df %>%
    dplyr::left_join(mean_popWeight, by = "survey") %>%
    dplyr::mutate(
      popWeight = ifelse(popWeight > 6 * mean, 6 * mean, popWeight)
    ) %>%
    dplyr::select(-mean)
  
  return (df)
}

compute_lastYear <- function (df) {
  library (tidyverse)
  # column with last complete year of data (for computing PPRs...)
  df$country <- factor(df$country)
  df$survey <- factor(df$survey)
  Countries <- names(table(df$country))
  Surveys <- names(table(df$survey))
  for (aCountry in Countries) {
    for (aSurvey in Surveys) {
      idx <- (df$country==aCountry)&(df$survey==aSurvey)
      # correct out-of-range surveyDate_cmc
      tab <- table(df$surveyDate_cmc[idx])
      meanCmc <- trunc (weighted.mean(as.integer(names(tab)), w=tab))
      if ((as.integer(names(tab))[1] < meanCmc - 12*6) |
          (as.integer(names(tab))[length(tab)] > meanCmc + 12*6)) {
        cat("Some survey date cmc for",aCountry,aSurvey,"seems to be wrong, we set it to the mean cmc of the survey\n")
        too_high <- idx & (df$surveyDate_cmc > meanCmc + 12*6)
        df$surveyDate_cmc[too_high] <- meanCmc
        too_low <- idx & (df$surveyDate_cmc < meanCmc - 12*6)
        df$surveyDate_cmc[too_low] <- meanCmc
      }
      df[idx,"lastYear"] <- 1900 + floor ((df$surveyDate_cmc[idx] - 1) / 12) - 1
      df[idx,"lastYear"] <- min (df[idx,"lastYear"])
    }
  }
  df <- relocate(df, lastYear, .after = surveyDate_cmc)
  return (df)
}

reorder_birthHistory <- function (df=GGS_ENADID) {
  library(dplyr)
  library(tidyr)
  
  if (exists("DEBUG_reorder") && isTRUE(DEBUG_reorder)) browser()
  # 0. Count the number of cases
  df2=subset(df,(dob_cmc2<dob_cmc1)|(dob_cmc3<dob_cmc2)|(dob_cmc4<dob_cmc3))
  if (nrow(df2)==0) {
    cat("Birth histories already ordered...\n")
    return (df)
  } else {
    cat(nrow(df2),"birth histories not ordered...\n")
  }
  # 1. We create a temporary ID if it does not exist so as not to lose the woman's reference.
  df <- df %>%
    dplyr::mutate(temp_id = row_number())
  
  # 2. We transform into long format
  df_reordered <- df %>%
    # We select the child columns (sex1, dob_cmc1, sex2, dob_cmc2...)
    pivot_longer(
      cols = matches("^(sex|dob_cmc|dob_cmc_I|dod_cmc|dod_cmc_I)\\d+$"),
      names_to = c(".value", "old_order"),
      names_pattern = "(sex|dob_cmc|dob_cmc_I|dod_cmc|dod_cmc_I)(\\d+)",
      values_drop_na = TRUE  # We remove the NAs so that the order is clean.
    ) %>%
    # 3. We sort by woman and date of birth (CMC)
    group_by(temp_id) %>%
    arrange(dob_cmc, .by_group = TRUE) %>%
    # 4. We create the NEW order (1 for the first, 2 for the second...)
    mutate(new_order = row_number()) %>%
    ungroup() %>%
    # 5. We return to wide format with the new names
    pivot_wider(
      id_cols = c(temp_id, nBioKids), # Keep other variables you don't want to lose here.
      names_from = new_order,
      values_from = c(sex, dob_cmc, dob_cmc_I, dod_cmc, dod_cmc_I),
      names_glue = "{.value}{new_order}"
    )
  
  # 6. Join back with the rest of the original variables if necessary.
  final_df <- df %>%
    dplyr::select(-matches("^(sex|dob_cmc|dob_cmc_I|dod_cmc|dod_cmc_I)\\d+$")) %>%
    # Add nBioKids to the 'by' vector
    left_join(df_reordered, by = c("temp_id", "nBioKids"))
  
  return (final_df)
}

homoFactor <- function (df1=NULL, df2=NULL) {
  # for the columns with the same name which are factor variable, homogeneize, then return both in a list
  nc <- names (df1)
  for (c in nc) {
    if (is.factor (df1[[c]]) & is.factor (df2[[c]])) {
      # homogeneize the levels of the factor variable
      levs <- union (levels (df1[[c]]), levels (df2[[c]]))
      df1[[c]] <- factor (df1[[c]], levels=levs)
      df2[[c]] <- factor (df2[[c]], levels=levs)
    }
  }
  return (list(a=df1, b=df2))
}

join_with_harmonized <- function (df1=GGS_ENADID, df2=all_data, aSurvey="GGS2") {
  if ((exists("DEBUG1")) && isTRUE(DEBUG1)) browser()
  df1$llave_muj <- as.character(df1$llave_muj)
  if (class(df2)=="list") {
    countries <- names(df2)
  } else {
    countries <- names(table(df2$country))
  }
  for (aCountry in countries) {
    already <- subset(df1, (country==toupper(aCountry))&(survey==aSurvey))
    if (nrow(already)==0) {
      if (class(df2)=="list") {
        dfCountry <- df2[[aCountry]]
      } else {
        dfCountry <- subset(df2, country==aCountry)
      }
      
      aList <- homoFactor(df1, dfCountry)
      df1 <- aList$a
      dfCountry <- aList$b
      df1 <- dfCountry %>%
        select(any_of(names(df1))) %>%  # Select only columns present in df1
        bind_rows(df1, .)
      cat (paste(aCountry,"added..."))
    }
  }
  return (df1)
}

createFirstUnionFirstBirth <- function(df = NULL, ageMother=45L, ageChild=10L) {
  
  library(dplyr)
  
  AGE_MOTHER_MONTHS <- ageMother * 12L   # maximum mother's age, offset from her DOB
  CAP_CHILD_MONTHS <- ageChild * 12L   # age cap on the child's life
  
  
  # ==== 1. Assemble working dataframe ====
  
  union1_birth1 <- data.frame(
    surveyName = df$survey,
    country    = df$country,
    cmc_birth  = df$indiv_dob_cmc,    # mother's date of birth
    yBirth     = df$yBirth,
    cmc_survey = df$surveyDate_cmc,
    cmc_union1 = df$union_start_cmc1,
    cmc_sep1   = df$union_end_cmc1,   # NA = union still ongoing at survey
    cmc_union2 = df$union_start_cmc2,
    cmc_sep2 = df$union_end_cmc2,   # NA = union still ongoing at survey
    cmc_union3 = df$union_start_cmc3,
    cmc_sep3   = df$union_end_cmc3,   # NA = union still ongoing at survey
    cmc_union4 = df$union_start_cmc4,
    cmc_sep4   = df$union_end_cmc4,   # NA = union still ongoing at survey
    cmc_birth1 = df$dob_cmc1,         # first child's date of birth
    cmc_death1 = df$dod_cmc1,         # NA = child still alive at survey
    weight     = df$weight,
    popWeight  = df$popWeight,
    yBirth1    = yearFrom_cmc(df$dob_cmc1),
    yUnion1    = yearFrom_cmc(df$union_start_cmc1)
  )
  
  # ==== 2. Birth status relative to the union ====
  
  union1_birth1 <- union1_birth1 %>%
    mutate(firstBirthStatus = case_when(
      is.na(cmc_birth1)        ~ "no birth",
      is.na(cmc_union1)        ~ "no union",
      cmc_birth1 <  cmc_union1 ~ "before",
      is.na(cmc_sep1)          ~ "during ongoing",
      cmc_birth1 <  cmc_sep1   ~ "during before sep",
      cmc_birth1 >= cmc_sep1   ~ "after",
      .default = NA_character_
    ))
  
  
  # ==== 3. Observed life span of the child ====
  
  # End of observation = death if it occurred, else the survey (right-censored).
  union1_birth1 <- union1_birth1 %>%
    mutate(
      cmc_end = if_else(is.na(cmc_death1), cmc_survey, cmc_death1),
      firstBirth_durLife = if_else(is.na(cmc_birth1),
                                   NA_real_,
                                   pmax(0, cmc_end - cmc_birth1))
    )
  
  
  # ==== 4. Partition life by union regime (interval overlap) ====
  
  # Each component is the overlap (in months) of the child's observed life
  # [cmc_birth1, cmc_end] with one regime on the calendar:
  #   before : (-inf, cmc_union1)
  #   during : [cmc_union1, cmc_sep1)   (sep = +Inf when the union is ongoing)
  #   after  : [cmc_sep1, +inf)         (only when separated / widowed)
  # By construction before + during + after = firstBirth_durLife for any
  # child with a union, regardless of where the birth itself falls.
  
  union1_birth1 <- union1_birth1 %>%
    mutate(
      has_birth = !is.na(cmc_birth1),
      has_union = !is.na(cmc_union1),
      sep_eff   = if_else(is.na(cmc_sep1), Inf, as.double(cmc_sep1)),
      
      firstBirthLife_noUnion = if_else(has_birth & !has_union,
                                       firstBirth_durLife, NA_real_),
      
      firstBirthLife_BeforeUnion = if_else(
        has_birth & has_union,
        pmax(0, pmin(cmc_end, cmc_union1) - cmc_birth1),
        NA_real_),
      
      firstBirthLife_DuringUnion = if_else(
        has_birth & has_union,
        pmax(0, pmin(cmc_end, sep_eff) - pmax(cmc_birth1, cmc_union1)),
        NA_real_),
      
      firstBirthLife_AfterUnion = case_when(
        !has_birth | !has_union ~ NA_real_,
        is.na(cmc_sep1)         ~ 0,                                  # ongoing
        .default = pmax(0, cmc_end - pmax(cmc_birth1, cmc_sep1))
      )
    )
  
  
  # ==== 5. Life capped at mother  limit age / child limit age or death ====
  
  # End = earliest of: mother's limit age birthday, child's limit age birthday, the
  # survey, and the child's death. na.rm drops caps that don't apply (death
  # for a surviving child, or a missing maternal DOB).
  union1_birth1 <- union1_birth1 %>%
    mutate(
      cmc_mother_cap   = cmc_birth  + AGE_MOTHER_MONTHS,
      cmc_children_cap = cmc_birth1 + CAP_CHILD_MONTHS,
      cmc_end_capped = pmin(cmc_mother_cap, cmc_children_cap, cmc_survey, cmc_death1,
                            na.rm = TRUE),
      firstBirth_durLife_capped = if_else(
        is.na(cmc_birth1), NA_real_,
        pmax(0, cmc_end_capped - cmc_birth1))
    ) %>%
    select(-has_birth, -has_union, -sep_eff)
  
  return(union1_birth1)
}

# reasons why children live less than ageCap in a file created by createFirstUnionFirstBirth function
reasonIncomplete <- function(df, ageCap = 10) {
  cap_m <- ageCap * 12L
  
  df$ageMotherAtSurvey <- floor((df$cmc_survey - df$cmc_birth) / 12)
  
  # Reason = which date bound the window. case_when picks the FIRST match,
  # so order encodes priority. NA comparisons count as no-match (so the
  # !is.na guards keep death/mother branches from misfiring).
  df$reasonExcluded <- dplyr::case_when(
    is.na(df$cmc_birth1)                                       ~ "no first birth",
    df$firstBirth_durLife_capped >= cap_m                      ~ "included",
    !is.na(df$cmc_death1)   & df$cmc_end_capped == df$cmc_death1   ~ "child death",
    !is.na(df$cmc_mother_cap) & df$cmc_end_capped == df$cmc_mother_cap ~ "mother cap",
    df$cmc_end_capped == df$cmc_survey                         ~ "capped by survey",
    TRUE                                                       ~ "unknown"
  )
  
  print (tNA(df,surveyName,reasonExcluded))
  
  si <- subset(df,reasonExcluded=="included")
  ss <- si %>%
    dplyr::group_by(yBirth1, country) %>%
    summarize(mother_minAge=min(ageMotherAtSurvey),
              mother_maxAge=max(ageMotherAtSurvey),
              meanAge=weighted.mean(ageMotherAtSurvey,popWeight),
              .groups = "drop")
}

createUnionSep <- function (df=NULL) {
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  union1_sep1 <- data.frame(surveyName=df$survey, country=df$country, cmc_birth=df$indiv_dob_cmc,
                            yBirth=df$yBirth, cmc_survey=df$surveyDate_cmc,
                            cmc_union1=df$union_start_cmc1,
                            cmc_sep1=df$union_end_cmc1,
                            sep1_motive=df$union_end_motive1,
                            weight=df$weight,
                            popWeight=df$popWeight)
  
  union1_sep1$ageSurvey <- trunc((union1_sep1$cmc_survey - union1_sep1$cmc_birth) / 12)
  union1_sep1$sex <- 2
  df <- compute_lastYear (df)
  union1_sep1$lastYear <- df$lastYear
  
  #### clean union1_sep1
  # only women who enter a first union
  union1_sep1 <- subset (union1_sep1, (!is.na(cmc_union1)))
  # first separation cannot occur before first union
  befAll <- nrow(union1_sep1)
  union1_sep1 <- subset ( union1_sep1, (is.na(cmc_sep1)) | (cmc_sep1 >= cmc_union1) )
  if (befAll > nrow(union1_sep1)) cat (paste0("Excluded ", befAll - nrow(union1_sep1), " observations with first separation before union\n"))
  # exclude bad date of survey, or first union and first separation that occurs after the date of survey
  bef <- nrow(union1_sep1)
  union1_sep1 <- subset (union1_sep1, (!is.na(cmc_survey)))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with NA as date of survey \n"))
  bef <- nrow(union1_sep1)
  union1_sep1 <- subset (union1_sep1, (cmc_union1 <= cmc_survey))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with first union after date survey \n"))
  bef <- nrow(union1_sep1)
  union1_sep1 <- subset (union1_sep1, (is.na(cmc_sep1)) | (cmc_sep1 <= cmc_survey))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with first separation after date survey \n"))
  
  # end of first union unknown motive are allocated to separation
  levels(union1_sep1$sep1_motive)[levels(union1_sep1$sep1_motive) == "unknown"] <- "separation"
  # widowhood is treated as censoring
  union1_sep1$cmc_survey[union1_sep1$sep1_motive %in% "widowhood"] <- union1_sep1$cmc_sep1[union1_sep1$sep1_motive %in% "widowhood"]
  #union1_sep1$cmc_survey <- ifelse((!is.na(union1_sep1$sep1_motive))&(union1_sep1$sep1_motive=="widowhood"),union1_sep1$cmc_sep1,union1_sep1$cmc_survey)
  # cleaning...
  # second round of cmc_survey: take a look at dates of widowhood which are NA
  bef <- nrow(union1_sep1)
  union1_sep1 <- subset(union1_sep1, !is.na(union1_sep1$cmc_survey))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with NA as date of first widowhood \n"))
  bef <- nrow(union1_sep1)
  # if there is a separation, we should have a cmc date for it
  union1_sep1 <- dplyr::filter (union1_sep1, !(sep1_motive %in% "separation" & is.na(cmc_sep1)))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with NA as date of first separation \n"))
  # we consider only separation as event
  union1_sep1$cmc_sep1 <- ifelse((!is.na(union1_sep1$sep1_motive))&(union1_sep1$sep1_motive=="separation"),union1_sep1$cmc_sep1,NA)
  #year of first union
  union1_sep1$yUnion1 <- yearFrom_cmc(union1_sep1$cmc_union1)
  union1_sep1$ageAtRisk <- (union1_sep1$cmc_union1 - union1_sep1$cmc_birth) / 12
  bef <- nrow(union1_sep1)
  union1_sep1 <- subset (union1_sep1, (!is.na(yUnion1)))
  if (bef > nrow(union1_sep1)) cat (paste0("Excluded ", bef - nrow(union1_sep1), " observations with NA as date of first union \n"))
  #year of first separation
  union1_sep1$ySep1 <- yearFrom_cmc(union1_sep1$cmc_sep1)
  union1_sep1$ageEvent <- (union1_sep1$cmc_sep1 - union1_sep1$cmc_birth) / 12
  union1_sep1$durationEvent <- union1_sep1$cmc_sep1 - union1_sep1$cmc_union1
  
  if (befAll > nrow(union1_sep1)) cat (paste0("Excluded ", befAll - nrow(union1_sep1), " observations of a total of ", befAll, "\n"))
  
  union1_sep1$surveyName <- factor (union1_sep1$surveyName)
  
  return (union1_sep1)
}

createBirthBirths <- function (df) {
  birth_births <- data.frame(surveyName=df$survey, country=df$country, cmc_birth=df$indiv_dob_cmc,
                             yBirth=df$yBirth, ageSurvey=df$indiv_age_survey, cmc_survey=df$surveyDate_cmc,
                             cmc_birth1=df$dob_cmc1,cmc_birth2=df$dob_cmc2,
                             cmc_birth3=df$dob_cmc3,cmc_birth4=df$dob_cmc4,
                             cmc_birth5=df$dob_cmc5,cmc_birth6=df$dob_cmc6,
                             cmc_birth7=df$dob_cmc7,cmc_birth8=df$dob_cmc8,
                             cmc_birth9=df$dob_cmc9,cmc_birth10=df$dob_cmc10,
                             cmc_birth11=df$dob_cmc11,cmc_birth12=df$dob_cmc12,
                             cmc_birth13=df$dob_cmc13,cmc_birth14=df$dob_cmc14,
                             cmc_birth15=df$dob_cmc15,cmc_birth16=df$dob_cmc16,
                             weight=df$weight, popWeight=df$popWeight, last_year=df$lastYear)
  
  #clean birth_births
  nRows <- nrow(birth_births)
  birth_births <- subset (birth_births, (!is.na(cmc_birth)))
  birth_births <- subset (birth_births, (!is.na(cmc_survey)))
  birth_births <- subset (birth_births, (!is.na(yBirth)))
  birth_births <- subset (birth_births, (yBirth < 2100))
  #year birth children
  for (b in (1:16)) {
    yBirthChild <- paste0("yBirth", b)
    cmcBirthChild <- paste0("cmc_birth", b)
    birth_births[[yBirthChild]] <- yearFrom_cmc(birth_births[[cmcBirthChild]])
    birth_births <- subset (birth_births, (is.na(birth_births[[yBirthChild]])) | (birth_births[[yBirthChild]] < 2100))
    birth_births[[cmcBirthChild]] <-NULL
  }
  
  if (nRows > nrow(birth_births)) {
    cat ("Removed", nRows-nrow(birth_births), "rows with missing or implausible birth dates\n")
  }
  return (birth_births)
}

createUnionBirths <- function (df) {
  union_births <- data.frame(surveyName=df$survey, country=df$country, cmc_birth=df$indiv_dob_cmc,
                             yBirth=df$yBirth, ageSurvey=df$indiv_age_survey, cmc_survey=df$surveyDate_cmc,
                             cmc_union1=df$union_start_cmc1,
                             cmc_sep1=df$union_end_cmc1,
                             sep1_motive=df$union_end_motive1,
                             cmc_birth1=df$dob_cmc1,cmc_birth2=df$dob_cmc2,
                             cmc_birth3=df$dob_cmc3,cmc_birth4=df$dob_cmc4,
                             cmc_birth5=df$dob_cmc5,cmc_birth6=df$dob_cmc6,
                             cmc_birth7=df$dob_cmc7,cmc_birth8=df$dob_cmc8,
                             cmc_birth9=df$dob_cmc9,cmc_birth10=df$dob_cmc10,
                             cmc_birth11=df$dob_cmc11,cmc_birth12=df$dob_cmc12,
                             cmc_birth13=df$dob_cmc13,cmc_birth14=df$dob_cmc14,
                             cmc_birth15=df$dob_cmc15,cmc_birth16=df$dob_cmc16,
                             weight=df$indiv_weight, last_year=df$lastYear)
  
  #clean birth_births
  nRows <- nrow(birth_births)
  birth_births <- subset (birth_births, (!is.na(cmc_birth)))
  birth_births <- subset (birth_births, (!is.na(cmc_survey)))
  birth_births <- subset (birth_births, (!is.na(yBirth)))
  birth_births <- subset (birth_births, (yBirth<2100))
  #year of first birth
  birth_births$yBirth1 <- yearFrom_cmc(birth_births$cmc_birth1)
  birth_births <- subset (birth_births,(is.na(yBirth1))|(yBirth1<2100))
  birth_births$yBirth2 <- yearFrom_cmc(birth_births$cmc_birth2)
  birth_births <- subset (birth_births,(is.na(yBirth2))|(yBirth2<2100))
  
  if (nRows > nrow(birth_births)) {
    cat ("Removed", nRows-nrow(birth_births), "rows with missing or implausible birth dates\n")
  }
  return (birth_births)
}

# ==== 1. Join & compute relative error ====
comparePopCount <- function(df1, df2) {
  
  dplyr::inner_join(df1, df2,
                    by     = c("survey", "indiv_age_survey"),
                    suffix = c("_1", "_2")) %>%
    dplyr::mutate(
      rel_error = abs(pop_count_1 - pop_count_2) /
        ((pop_count_1 + pop_count_2) / 2)
    )
}

# ==== 2. Summarise ====
summariseError <- function(compared) {
  
  # Overall mean relative error
  overall <- mean(compared$rel_error, na.rm = TRUE)
  message("Mean relative error of popWeight (all surveys): ", round(overall, 4))
  
  # By survey
  by_survey <- compared %>%
    dplyr::group_by(survey) %>%
    dplyr::summarise(mean_rel_error = mean(rel_error, na.rm = TRUE),
                     n_ages         = dplyr::n(),
                     .groups        = "drop") %>%
    dplyr::mutate(mean_rel_error = dplyr::if_else(mean_rel_error < 1e-10, 0, mean_rel_error))
  
  message(paste(capture.output(print(by_survey)), collapse = "\n"))
  
  invisible(list(overall = overall, by_survey = by_survey))
}

