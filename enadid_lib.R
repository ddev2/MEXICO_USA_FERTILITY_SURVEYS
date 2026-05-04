setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rootPath <- sub("/INEGI/.*", "", getwd())
library (tidyverse)
library (haven)

century_year <- function (year) {
  year <- ifelse(year == 0, NA, year)
  year <- ifelse(year == 99, 9999, year)
  year <- ifelse(year %in% seq (1,98,1),1900 + year, year)
  return (year)
}

compute_cmc <- function(month, year) {
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
  
  # Impute month when missing or out of range.
  # Check is.na first: NA %in% 1:12 returns FALSE, so without this guard
  # NAs would be silently replaced by the range check below.
  month <- ifelse(is.na(month) & !is.na(year),  sample(1:12, nm, replace = TRUE), month)
  month <- ifelse(!is.na(month) & !(month %in% 1:12), sample(1:12, nm, replace = TRUE), month)
  
  idx <- !is.na(year)
  cmc <- rep(NA_integer_, nm)
  cmc[idx] <- ifelse(year[idx] > 2100, 9999L, (year[idx] - 1900L) * 12L + month[idx])
  return(as.integer(cmc))
}

imputed_date <- function (month, year) {
  month <- as.integer(month)
  year <- as.integer(year)
  year <- century_year (year)
  imputed <- ifelse (is.na(month)&is.na(year),NA,0)
  imputed_month <- ifelse((month >= 1)&(month <= 12), 0, 1)
  imputed_year <- ifelse((year < 2100), 0, 10)
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
          cat(paste0("Column ", colnames(df)[i], " has non-integer values at position(s):\n"))
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
          message(paste("Warning in column:", col_name))
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


bigDataWomen <- function (datos=NULL, hasFullUnionHistory=FALSE, countryName=NULL) {
  if (is.null(countryName)) countryName <- "MEXICO"
  ENADID <- data.frame(country=countryName, survey=datos$survey, llave_muj=datos$llave_muj)
  ENADID$surveyDate_cmc <- datos$surveyDate_cmc
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
                                       compute_cmc(datos$lastUnion_month_start, datos$lastUnion_year_start),
                                       NA)
  ENADID$lastUnion_start_cmc_I <- imputed_date(datos$lastUnion_month_start, datos$lastUnion_year_start)
  ENADID$lastUnion_end_cmc <- ifelse(ENADID$union_status != "single",
                                     compute_cmc(datos$lastUnion_month_end, datos$lastUnion_year_end),
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
  ENADID$lastUnion_start_cmc <- ifelse((!is.na(datos$lastUnion_cohab_before))&(datos$lastUnion_cohab_before == "yes"),
                                       compute_cmc(datos$lastUnion_month_cohab_before, datos$lastUnion_year_cohab_before),
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
                                      compute_cmc(datos$firstUnionNotLast_month_start, datos$firstUnionNotLast_year_start))
    ENADID$union_start_cmc_I1 <- ifelse(ENADID$nUnion == 1,
                                      ENADID$lastUnion_start_cmc_I,
                                      imputed_date(datos$firstUnionNotLast_month_start, datos$firstUnionNotLast_year_start))
    ENADID$union_end_cmc1 <- ifelse(ENADID$nUnion == 1,
                                    ENADID$lastUnion_end_cmc,
                                    compute_cmc(datos$firstUnionNotLast_month_end, datos$firstUnionNotLast_year_end))
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
                                      compute_cmc(datos$firstUnionNotLast_cohab_month, datos$firstUnionNotLast_cohab_year),
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
  
  return (ENADID)
  
}

addID <- function (df) {
  if (!("ID" %in% names(df))) {df <- cbind(ID = 1:nrow(df), df)}
  return (df)
}

# clean dataset: remove individuals with incoherence in union and marriage dates
# also clean weight
cleanENADID_old <- function (df, checkOnly=FALSE, correctUnionHistory=TRUE, correctBirthHistory=FALSE) {
  df <- zap_labels(df)
  check_end_survey <- function (df, varEvent) {
    if (!(varEvent %in% names (df))) return (c())
    df[[varEvent]][is.na(df[[varEvent]])] <- 0
    idx <- !is.na(df$surveyDate_cmc) & (df[[varEvent]] > df$surveyDate_cmc)
    if (sum(idx) > 0) {
      cat(sum(idx), "individuals with", varEvent, "after survey date\n")
    }
    return (df$ID[idx])
  }
  
  if (exists("DEBUG_cleanENADID") && isTRUE(DEBUG_cleanENADID)) browser()
  df <- addID(df)
  nIndiv <- nrow(df)
  if (isFALSE(correctUnionHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly <- TRUE
  }

  if (checkOnly) {
    action <- "We found"
  } else {
    action <- "We remove"
  }
  ids_to_remove <- c()
  # only the first two unions / marriages
  haveMarriage <- "marriage_start_cmc1" %in% names(df)
  haveUnion1 <- "union_start_cmc1" %in% names(df)
  haveUnion2 <- "union_start_cmc2" %in% names(df)
  haveUnion3 <- "union_start_cmc3" %in% names(df)
  if (haveMarriage) {
    xxx=df[,c("ID","union_start_cmc1","marriage_start_cmc1","union_end_cmc1", "surveyDate_cmc")]
    xxx1 <- dplyr::filter(xxx, marriage_start_cmc1 < union_start_cmc1)
    if (nrow(xxx1)>0) cat(nrow(xxx1),"1st marriage before union\n")
    xxx2 <- dplyr::filter(xxx,(union_end_cmc1<union_start_cmc1))
    if (nrow(xxx2)>0) cat(nrow(xxx2),"1st union end before start\n")
    xxx3 <- dplyr::filter(xxx,(union_end_cmc1<marriage_start_cmc1))
    if (nrow(xxx3)>0) cat(nrow(xxx3),"1st marriage end before start\n")
    xxx4 <- dplyr::filter(xxx,(union_start_cmc1>9900))
    if (nrow(xxx4)>0) cat(nrow(xxx4),"1st union start cmc more than 9900\n")
    xxx5 <- dplyr::filter(xxx,(union_end_cmc1>9900))
    if (nrow(xxx5)>0) cat(nrow(xxx5),"1st union end cmc more than 9900\n")
    xxx6 <- dplyr::filter(xxx,(marriage_start_cmc1>9900))
    if (nrow(xxx6)>0) cat(nrow(xxx6),"1st marriage start cmc more than 9900\n")
    xxx7 <- dplyr::filter(xxx,(union_start_cmc1 > surveyDate_cmc))
    if (nrow(xxx7)>0) cat(nrow(xxx7),"1st union start cmc after survey cmc\n")
    ids_to_remove <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID, xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID))
  } else {
    if (haveUnion1) {
      xxx=df[,c("ID","union_start_cmc1","union_end_cmc1", "surveyDate_cmc")]
      xxx2 <- dplyr::filter(xxx,(union_end_cmc1<union_start_cmc1))
      if (nrow(xxx2)>0) cat(nrow(xxx2),"1st union end before start\n")
      ids_to_remove <- xxx2$ID
    }
  }
  if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"individuals with problems in date of first union\n")
  if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  
  if (haveUnion2) {
    if (haveMarriage) {
      xxx=df[,c("ID","union_start_cmc1","marriage_start_cmc1","union_end_cmc1","union_start_cmc2","marriage_start_cmc2","union_end_cmc2", "surveyDate_cmc")]
      xxx1 <- dplyr::filter(xxx,(marriage_start_cmc2<union_start_cmc2))
      if (nrow(xxx1)>0) cat(nrow(xxx1),"2nd marriage before union\n")
      xxx2 <- dplyr::filter(xxx,(union_end_cmc2<union_start_cmc2))
      if (nrow(xxx2)>0) cat(nrow(xxx2),"2nd union end before start\n")
      xxx3 <- dplyr::filter(xxx,(union_end_cmc2<marriage_start_cmc2))
      if (nrow(xxx3)>0) cat(nrow(xxx3),"2nd marriage end before start\n")
      xxx4 <- dplyr::filter(xxx,(union_start_cmc2>9900))
      if (nrow(xxx4)>0) cat(nrow(xxx4),"2nd union start cmc more than 9900\n")
      xxx5 <- dplyr::filter(xxx,(union_end_cmc2>9900))
      if (nrow(xxx5)>0) cat(nrow(xxx5),"2nd union end cmc more than 9900\n")
      xxx6 <- dplyr::filter(xxx,(marriage_start_cmc2>9900))
      if (nrow(xxx6)>0) cat(nrow(xxx6),"2nd marriage start cmc more than 9900\n")
      xxx7 <- dplyr::filter(xxx,(union_start_cmc2 > surveyDate_cmc))
      if (nrow(xxx7)>0) cat(nrow(xxx7),"2nd union start cmc after survey cmc\n")
      xxx8 <- dplyr::filter(xxx,!is.na(union_start_cmc2)&(is.na(union_end_cmc1)))
      if (nrow(xxx8)>0) cat(nrow(xxx8),"2nd union start with no 1st union end\n")
      ids_to_remove <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID, xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID, xxx8$ID))
    } else {
      xxx=df[,c("ID","union_start_cmc1","union_end_cmc1","union_start_cmc2","union_end_cmc2", "surveyDate_cmc")]
      xxx2 <- dplyr::filter(xxx,(union_end_cmc2<union_start_cmc2))
      if (nrow(xxx2)>0) cat(nrow(xxx2),"2nd union end before start\n")
      ids_to_remove <- xxx2$ID
    }
    if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"individuals with problems in date of second union\n")
    if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
    
    if (haveMarriage) {
      xxx1 <- dplyr::filter(xxx,(marriage_start_cmc2<union_end_cmc1))
      if (nrow(xxx1)>0) cat(nrow(xxx1),"2nd marriage before end 1st union\n")
      xxx2 <- dplyr::filter(xxx,(union_start_cmc2<union_end_cmc1))
      if (nrow(xxx2)>0) cat(nrow(xxx2),"2nd union before end 1st union\n")
      ids_to_remove <- unique(c(xxx1$ID, xxx2$ID))
    } else {
      xxx2 <- dplyr::filter(xxx,(union_start_cmc2<union_end_cmc1))
      if (nrow(xxx2)>0) cat(nrow(xxx2),"2nd union before end 1st union\n")
      ids_to_remove <- xxx2$ID
    }
    if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"individuals with date of second union before the end of first union\n")
    if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  }
  
  if (haveUnion3) {
    # have start union 3 and no end union 2
    xxx=df[,c("ID","union_end_cmc2","union_start_cmc3", "surveyDate_cmc")]
    xxx1 <- dplyr::filter(xxx,(union_end_cmc2>union_start_cmc3))
    if (nrow(xxx1)>0) cat(nrow(xxx1),"3th union start before 2nd union end\n")
    xxx2 <- dplyr::filter(xxx,(is.na(union_end_cmc2))&(!is.na(union_start_cmc3)))
    if (nrow(xxx2)>0) cat(nrow(xxx2),"3th union start and no 2nd union end\n")
    ids_to_remove <- unique(c(xxx1$ID, xxx2$ID))
    if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"individuals with bad date of second union when there is a third union\n")
    if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  }
  
  if (haveUnion1) {
    if (!all(is.na(df$nUnion))) {
      maxU <- max (df$nUnion, na.rm=TRUE)
      ids_to_remove <- c()
      for (u in (1:maxU)) {
        varEvent <- paste("union_start_cmc",u,sep="")
        ids_to_remove <- c(ids_to_remove, check_end_survey (df, varEvent))
        varEvent <- paste("marriage_start_cmc",u,sep="")
        ids_to_remove <- c(ids_to_remove, check_end_survey (df, varEvent))
        varEvent <- paste("union_end_cmc",u,sep="")
        ids_to_remove <- c(ids_to_remove, check_end_survey (df, varEvent))
      }
      ids_to_remove <- unique(ids_to_remove)
      if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"women with date of union events after date of survey\n")
      if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
    }
  }

  if (isFALSE(correctUnionHistory)) {
    checkOnly <- checkOnlyMem
  }
  if (isFALSE(correctBirthHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly <- TRUE
  }
  if (checkOnly) {
    action <- "We found"
  } else {
    action <- "We remove"
  }
  maxB <- max (df$nBioKids)
  ids_to_remove <- c()
  for (b in (1:maxB)) {
    varEvent <- paste("dob_cmc",b,sep="")
    ids_to_remove <- c(ids_to_remove, check_end_survey (df, varEvent))
  }
  ids_to_remove <- unique(ids_to_remove)
  if (length(ids_to_remove) > 0) cat(action, length(ids_to_remove),"women with date of births after date of survey\n")
  if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  
  if (isFALSE(correctBirthHistory)) {
    checkOnly <- checkOnlyMem
  }
  df$ID <- NULL
  
  nRemoved <- nIndiv - nrow(df)
  
  cat(nRemoved,"individuals removed in total of",nIndiv,"during cleaning up\n")

  # clean weight
  idx <- is.na(df$indiv_weight)
  if (sum(idx)>0) {
    df$indiv_weight[idx] <- 1
    cat(sum(idx),"weights were NA and set to 1\n")
  }
  
  if (!checkOnly) return (df)
}

# modified to correct union history instead of removing individuals with incoherence in union history
checkENADID <- function(df) {
  return (cleanENADID(df, checkOnly = TRUE))  
}

cleanENADID <- function(df, checkOnly = FALSE, correctUnionHistory = TRUE,
                        correctBirthHistory = FALSE, n_show = 10L) {
  
  df <- zap_labels(df)
  
  if (isTRUE(checkOnly)) {
    listErrors <- list()
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
    cat(sprintf("  First %d of %d (%s):\n", n, length(ids), label))
    print(as.data.frame(shown), row.names = FALSE)
    cat("\n")
  }
  
  check_end_survey <- function(df, varEvent) {
    if (!(varEvent %in% names(df))) return(c())
    df[[varEvent]][is.na(df[[varEvent]])] <- 0
    idx <- !is.na(df$surveyDate_cmc) & (df[[varEvent]] > df$surveyDate_cmc)
    if (sum(idx) > 0) {
      cat(sum(idx), "individuals with", varEvent, "after survey date\n")
      idx2 <- df[[varEvent]][idx] >= 9999L
      if (any(idx2))
        cat(" ", sum(idx2), "due to missing year (cmc >= 9999)\n")
    }
    return(df$ID[idx])
  }
  
  if (exists("DEBUG_cleanENADID") && isTRUE(DEBUG_cleanENADID)) browser()
  
  df <- addID(df)
  nIndiv <- nrow(df)
  
  if (isFALSE(correctUnionHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly    <- TRUE
  }
  
  action <- if (checkOnly) "We FOUND" else "We REMOVE"
  
  ids_to_remove <- c()
  
  haveMarriage <- "marriage_start_cmc1" %in% names(df)
  haveUnion1   <- "union_start_cmc1"    %in% names(df)
  haveUnion2   <- "union_start_cmc2"    %in% names(df)
  haveUnion3   <- "union_start_cmc3"    %in% names(df)
  
  
  # ==== 1. Check / correct union 1 ====
  
  if (haveMarriage) {
    xxx <- df[, intersect(c("ID", "llave_muj",
                            "union_start_cmc1", "marriage_start_cmc1",
                            "union_end_cmc1", "surveyDate_cmc"), names(df))]
    
    xxx1 <- dplyr::filter(xxx, marriage_start_cmc1 < union_start_cmc1)
    if (nrow(xxx1) > 0) {
      cat(nrow(xxx1), "1st marriage before union\n")
      show_cases(xxx1, xxx1$ID,
                 c("union_start_cmc1", "marriage_start_cmc1"),
                 "marriage_start < union_start")
    }
    
    xxx2 <- dplyr::filter(xxx, union_end_cmc1 < union_start_cmc1)
    if (nrow(xxx2) > 0) {
      cat(nrow(xxx2), "1st union end before start\n")
      show_cases(xxx2, xxx2$ID,
                 c("union_start_cmc1", "union_end_cmc1"),
                 "union_end < union_start")
    }
    
    xxx3 <- dplyr::filter(xxx, union_end_cmc1 < marriage_start_cmc1)
    if (nrow(xxx3) > 0) {
      cat(nrow(xxx3), "1st marriage end before start\n")
      show_cases(xxx3, xxx3$ID,
                 c("marriage_start_cmc1", "union_end_cmc1"),
                 "union_end < marriage_start")
    }
    
    xxx4 <- dplyr::filter(xxx, union_start_cmc1 > 9900)
    if (nrow(xxx4) > 0) {
      cat(nrow(xxx4), "1st union start cmc more than 9900\n")
      show_cases(xxx4, xxx4$ID, c("union_start_cmc1"), "cmc > 9900")
    }
    
    xxx5 <- dplyr::filter(xxx, union_end_cmc1 > 9900)
    if (nrow(xxx5) > 0) {
      cat(nrow(xxx5), "1st union end cmc more than 9900\n")
      show_cases(xxx5, xxx5$ID, c("union_end_cmc1"), "cmc > 9900")
    }
    
    xxx6 <- dplyr::filter(xxx, marriage_start_cmc1 > 9900)
    if (nrow(xxx6) > 0) {
      cat(nrow(xxx6), "1st marriage start cmc more than 9900\n")
      show_cases(xxx6, xxx6$ID, c("marriage_start_cmc1"), "cmc > 9900")
    }
    
    xxx7 <- dplyr::filter(xxx, union_start_cmc1 > surveyDate_cmc)
    if (nrow(xxx7) > 0) {
      cat(nrow(xxx7), "1st union start cmc after survey cmc\n")
      show_cases(xxx7, xxx7$ID,
                 c("union_start_cmc1", "surveyDate_cmc"),
                 "union_start > survey")
    }
    
    ids_to_remove <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID,
                              xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID))
  } else {
    if (haveUnion1) {
      xxx  <- df[, intersect(c("ID", "llave_muj",
                               "union_start_cmc1", "union_end_cmc1",
                               "surveyDate_cmc"), names(df))]
      xxx2 <- dplyr::filter(xxx, union_end_cmc1 < union_start_cmc1)
      if (nrow(xxx2) > 0) {
        cat(nrow(xxx2), "1st union end before start\n")
        show_cases(xxx2, xxx2$ID,
                   c("union_start_cmc1", "union_end_cmc1"),
                   "union_end < union_start")
      }
      ids_to_remove <- xxx2$ID
    }
  }
  
  if (length(ids_to_remove) > 0)
    cat(action, length(ids_to_remove),
        "individuals with problems in date of first union\n")
  if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  
  
  # ==== 2. Check / correct union 2 ====
  
  if (haveUnion2) {
    
    if (haveMarriage) {
      xxx <- df[, intersect(c("ID", "llave_muj",
                              "union_start_cmc1", "marriage_start_cmc1",
                              "union_end_cmc1",
                              "union_start_cmc2", "marriage_start_cmc2",
                              "union_end_cmc2", "surveyDate_cmc"), names(df))]
      
      xxx1 <- dplyr::filter(xxx, marriage_start_cmc2 < union_start_cmc2)
      if (nrow(xxx1) > 0) {
        cat(nrow(xxx1), "2nd marriage before union\n")
        show_cases(xxx1, xxx1$ID,
                   c("union_start_cmc2", "marriage_start_cmc2"),
                   "marriage_start2 < union_start2")
      }
      
      xxx2 <- dplyr::filter(xxx, union_end_cmc2 < union_start_cmc2)
      if (nrow(xxx2) > 0) {
        cat(nrow(xxx2), "2nd union end before start\n")
        show_cases(xxx2, xxx2$ID,
                   c("union_start_cmc2", "union_end_cmc2"),
                   "union_end2 < union_start2")
      }
      
      xxx3 <- dplyr::filter(xxx, union_end_cmc2 < marriage_start_cmc2)
      if (nrow(xxx3) > 0) {
        cat(nrow(xxx3), "2nd marriage end before start\n")
        show_cases(xxx3, xxx3$ID,
                   c("marriage_start_cmc2", "union_end_cmc2"),
                   "union_end2 < marriage_start2")
      }
      
      xxx4 <- dplyr::filter(xxx, union_start_cmc2 > 9900)
      if (nrow(xxx4) > 0) {
        cat(nrow(xxx4), "2nd union start cmc more than 9900\n")
        show_cases(xxx4, xxx4$ID, c("union_start_cmc2"), "cmc > 9900")
      }
      
      xxx5 <- dplyr::filter(xxx, union_end_cmc2 > 9900)
      if (nrow(xxx5) > 0) {
        cat(nrow(xxx5), "2nd union end cmc more than 9900\n")
        show_cases(xxx5, xxx5$ID, c("union_end_cmc2"), "cmc > 9900")
      }
      
      xxx6 <- dplyr::filter(xxx, marriage_start_cmc2 > 9900)
      if (nrow(xxx6) > 0) {
        cat(nrow(xxx6), "2nd marriage start cmc more than 9900\n")
        show_cases(xxx6, xxx6$ID, c("marriage_start_cmc2"), "cmc > 9900")
      }
      
      xxx7 <- dplyr::filter(xxx, union_start_cmc2 > surveyDate_cmc)
      if (nrow(xxx7) > 0) {
        cat(nrow(xxx7), "2nd union start cmc after survey cmc\n")
        show_cases(xxx7, xxx7$ID,
                   c("union_start_cmc2", "surveyDate_cmc"),
                   "union_start2 > survey")
      }
      
      xxx8 <- dplyr::filter(xxx, !is.na(union_start_cmc2) & is.na(union_end_cmc1))
      if (nrow(xxx8) > 0) {
        cat(nrow(xxx8), "2nd union start with no 1st union end\n")
        show_cases(xxx8, xxx8$ID,
                   c("union_end_cmc1", "union_start_cmc2"),
                   "no union_end1 but union_start2 present")
      }
      
      ids_to_remove <- unique(c(xxx1$ID, xxx2$ID, xxx3$ID,
                                xxx4$ID, xxx5$ID, xxx6$ID, xxx7$ID, xxx8$ID))
    } else {
      xxx  <- df[, intersect(c("ID", "llave_muj",
                               "union_start_cmc1", "union_end_cmc1",
                               "union_start_cmc2", "union_end_cmc2",
                               "surveyDate_cmc"), names(df))]
      xxx2 <- dplyr::filter(xxx, union_end_cmc2 < union_start_cmc2)
      if (nrow(xxx2) > 0) {
        cat(nrow(xxx2), "2nd union end before start\n")
        show_cases(xxx2, xxx2$ID,
                   c("union_start_cmc2", "union_end_cmc2"),
                   "union_end2 < union_start2")
      }
      ids_to_remove <- xxx2$ID
    }
    
    if (length(ids_to_remove) > 0)
      cat(action, length(ids_to_remove),
          "individuals with problems in date of second union\n")
    if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
    
    
    # --- Overlap between union 1 end and union 2 start ---
    #
    # When correctUnionHistory = TRUE: corrected rather than removed.
    # When checkOnly = TRUE: reported with sample cases, not corrected.
    #
    # Correction rules:
    #   (a) union_end_cmc1 is NA  -> set to union_start_cmc2 - 1
    #   (b) union_end_cmc1 > union_start_cmc2 -> set to union_start_cmc2 - 1
    
    if (haveMarriage) {
      overlap_no_end <- dplyr::filter(xxx, !is.na(union_start_cmc2) &
                                        is.na(union_end_cmc1))
      overlap_bad    <- dplyr::filter(xxx, !is.na(union_start_cmc2) &
                                        !is.na(union_end_cmc1) &
                                        union_start_cmc2 < union_end_cmc1)
      if (nrow(overlap_no_end) > 0) {
        cat(nrow(overlap_no_end), "2nd marriage before end 1st union\n")
        show_cases(overlap_no_end, overlap_no_end$ID,
                   c("union_end_cmc1", "union_start_cmc2"),
                   "no union_end1, union_start2 present")
      }
      if (nrow(overlap_bad) > 0) {
        cat(nrow(overlap_bad), "2nd union before end 1st union\n")
        show_cases(overlap_bad, overlap_bad$ID,
                   c("union_end_cmc1", "union_start_cmc2"),
                   "union_start2 < union_end1")
      }
    } else {
      overlap_no_end <- dplyr::filter(xxx, !is.na(union_start_cmc2) &
                                        is.na(union_end_cmc1))
      overlap_bad    <- dplyr::filter(xxx, !is.na(union_start_cmc2) &
                                        !is.na(union_end_cmc1) &
                                        union_start_cmc2 < union_end_cmc1)
      if (nrow(overlap_bad) > 0) {
        cat(nrow(overlap_bad), "2nd union before end 1st union\n")
        show_cases(overlap_bad, overlap_bad$ID,
                   c("union_end_cmc1", "union_start_cmc2"),
                   "union_start2 < union_end1")
      }
    }
    
    ids_no_end  <- overlap_no_end$ID
    ids_overlap <- overlap_bad$ID
    ids_fix     <- unique(c(ids_no_end, ids_overlap))
    
    if (length(ids_fix) > 0) {
      if (checkOnly) {
        cat("We FOUND", length(ids_fix),
            "individuals with date of second union before the end of first union\n")
      } else {
        cat("We CORRECT", length(ids_fix),
            "individuals with date of second union before the end of first union\n")
        df$union_end_cmc1[df$ID %in% ids_no_end]  <-
          df$union_start_cmc2[df$ID %in% ids_no_end]  - 1L
        df$union_end_cmc1[df$ID %in% ids_overlap] <-
          df$union_start_cmc2[df$ID %in% ids_overlap] - 1L
        if ("union_end_cmc_I1" %in% names(df))
          df$union_end_cmc_I1[df$ID %in% ids_fix] <- 1L
      }
    }
  }
  
  
  # ==== 3. Check / correct union 3 ====
  
  if (haveUnion3) {
    xxx <- df[, intersect(c("ID", "llave_muj",
                            "union_end_cmc2", "union_start_cmc3",
                            "surveyDate_cmc"), names(df))]
    
    xxx1 <- dplyr::filter(xxx, union_end_cmc2 > union_start_cmc3)
    if (nrow(xxx1) > 0) {
      cat(nrow(xxx1), "3th union start before 2nd union end\n")
      show_cases(xxx1, xxx1$ID,
                 c("union_end_cmc2", "union_start_cmc3"),
                 "union_start3 < union_end2")
    }
    
    xxx2 <- dplyr::filter(xxx, is.na(union_end_cmc2) & !is.na(union_start_cmc3))
    if (nrow(xxx2) > 0) {
      cat(nrow(xxx2), "3th union start and no 2nd union end\n")
      show_cases(xxx2, xxx2$ID,
                 c("union_end_cmc2", "union_start_cmc3"),
                 "no union_end2 but union_start3 present")
    }
    
    ids_fix <- unique(c(xxx1$ID, xxx2$ID))
    
    if (length(ids_fix) > 0) {
      if (checkOnly) {
        cat("We FOUND", length(ids_fix),
            "individuals with bad date of second union when there is a third union\n")
      } else {
        cat("We CORRECT", length(ids_fix),
            "individuals with bad date of second union when there is a third union\n")
        ids_no_end2  <- xxx2$ID
        ids_overlap2 <- xxx1$ID
        df$union_end_cmc2[df$ID %in% ids_no_end2]  <-
          df$union_start_cmc3[df$ID %in% ids_no_end2]  - 1L
        df$union_end_cmc2[df$ID %in% ids_overlap2] <-
          df$union_start_cmc3[df$ID %in% ids_overlap2] - 1L
        if ("union_end_cmc_I2" %in% names(df))
          df$union_end_cmc_I2[df$ID %in% ids_fix] <- 1L
      }
    }
  }
  
  
  # ==== 4. Check dates against survey date ====
  
  if (haveUnion1) {
    if (!all(is.na(df$nUnion))) {
      maxU          <- max(df$nUnion, na.rm = TRUE)
      ids_to_remove <- c()
      
      for (u in seq_len(maxU)) {
        ids_to_remove <- c(ids_to_remove,
                           check_end_survey(df, paste0("union_start_cmc",    u)),
                           check_end_survey(df, paste0("marriage_start_cmc", u)),
                           check_end_survey(df, paste0("union_end_cmc",      u)))
      }
      
      ids_to_remove <- unique(ids_to_remove)
      if (length(ids_to_remove) > 0)
        cat(action, length(ids_to_remove),
            "women with date of union events after date of survey\n")
      if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
    }
  }
  
  if (isFALSE(correctUnionHistory)) checkOnly <- checkOnlyMem
  
  
  # ==== 5. Check / correct birth history ====
  
  if (isFALSE(correctBirthHistory)) {
    checkOnlyMem <- checkOnly
    checkOnly    <- TRUE
  }
  
  action <- if (checkOnly) "We FOUND" else "We REMOVE"
  
  maxB          <- max(df$nBioKids)
  ids_to_remove <- c()
  
  for (b in seq_len(maxB)) {
    ids_to_remove <- c(ids_to_remove,
                       check_end_survey(df, paste0("dob_cmc", b)))
  }
  
  ids_to_remove <- unique(ids_to_remove)
  if (length(ids_to_remove) > 0)
    cat(action, length(ids_to_remove),
        "women with date of births after date of survey\n")
  if (!checkOnly) df <- subset(df, !(ID %in% ids_to_remove))
  
  if (isFALSE(correctBirthHistory)) checkOnly <- checkOnlyMem
  
  
  # ==== 6. Finalise ====
  
  df$ID <- NULL
  
  nRemoved <- nIndiv - nrow(df)
  cat(nRemoved, "individuals REMOVED in total of", nIndiv, "during cleaning up\n")
  
  idx <- is.na(df$indiv_weight)
  if (sum(idx) > 0) {
    df$indiv_weight[idx] <- 1
    cat(sum(idx), "weights were NA and set to 1\n")
  }
  
  if (checkOnly) {
    return (listErrors)
  } else {
    return(df)
  }
}

imputed_month <- function (n, range=(1:12)) {
  sample(range, n, replace=TRUE)
}

# ==== Helper: month imputation capped to the survey period or constrained by a previous event ====
imputed_month_capped <- function(n, year_vec,
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

imputed_month_capped_old <- function(n,
                                 range      = 1:12,
                                 year_vec   = NULL,  # vector of years for each event (length n)
                                 survey_cmc = NULL,
                                 after_cmc  = NULL) {
  # Impute a random month for each of n events.
  #
  # survey_cmc: per-individual survey date CMC (vector length n).
  #   Caps the draw from above: result CMC <= survey_cmc[i].
  #   Only binds when year_vec[i] == year of survey_cmc[i].
  #
  # after_cmc: per-individual lower-bound CMC (vector length n).
  #   Caps the draw from below: result CMC > after_cmc[i].
  #   Only binds when year_vec[i] == year of after_cmc[i].
  # after_cmc enforces a STRICT lower bound: imputed CMC > after_cmc[i].
  # This is correct for same-year event ordering (e.g. Ustart < Mstart).
  # For non-strict cases, handle by direct CMC assignment rather than
  # passing through this function.
  
  # Single draw for unconstrained cases (the vast majority)
  months <- sample(range, size = n, replace = TRUE)
  
  if (is.null(year_vec)) return (months)  # no constraints if no year vector provided
  if (length(year_vec) == 1L) year_vec <- rep(year_vec, n)
  
  # Helper: extract year from CMC
  cmc_to_year <- function(cmc) (cmc - 1L) %/% 12L + 1900L
  
  # Initialise bounds to the full range for everyone
  lo <- rep(min(range), n)   # minimum valid month
  hi <- rep(max(range), n)   # maximum valid month
  
  # Tighten upper bound where year_vec[i] == survey year
  if (!is.null(survey_cmc)) {
    upper_idx    <- !is.na(year_vec) & !is.na(survey_cmc) &
      (year_vec == cmc_to_year(survey_cmc))
    hi[upper_idx] <- survey_cmc[upper_idx] -
      compute_cmc(1L, year_vec[upper_idx]) + 1L
  }
  
  # Tighten lower bound where year_vec[i] == year of after_cmc
  if (!is.null(after_cmc)) {
    lower_idx    <- !is.na(year_vec) & !is.na(after_cmc) &
      (year_vec == cmc_to_year(after_cmc))
    lo[lower_idx] <- after_cmc[lower_idx] -
      compute_cmc(1L, year_vec[lower_idx]) + 2L
  }
  
  # Redraw only cases where at least one bound is active and
  # the unconstrained draw falls outside [lo, hi]
  needs_redraw <- (months < lo) | (months > hi)
  
  if (any(needs_redraw)) {
    months[needs_redraw] <- mapply(
      function(l, h, yr) {
        valid <- range[range >= l & range <= h]
        if (length(valid) > 0L) {
          sample(valid, 1L)
        } else {
          warning(sprintf(
            "imputed_month_capped: no valid month (year %d, lo %d, hi %d); using nearest",
            yr, l, h))
          # Return the closest valid month rather than a random bad one
          if (h < min(range)) min(range) else max(range)
        }
      },
      lo[needs_redraw], hi[needs_redraw], year_vec[needs_redraw]
    )
  }
  
  return (months)
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
    cat("Birth histories already ordered...")
    return (df)
  } else {
    cat(nrow(df2),"birth histories not ordered...")
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
      message (paste(aCountry,"added..."))
    }
  }
  return (df1)
}

createUnionSep <- function (df=NULL) {
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  if ("reweight"  %in% names(df)) {
    union1_sep1 <- data.frame(surveyName=df$survey, country=df$country, cmc_birth=df$indiv_dob_cmc,
                              yBirth=df$yBirth, cmc_survey=df$surveyDate_cmc,
                              cmc_union1=df$union_start_cmc1,
                              cmc_sep1=df$union_end_cmc1,
                              sep1_motive=df$union_end_motive1,
                              weight=df$reweight)
  } else {
    union1_sep1 <- data.frame(surveyName=df$survey, country=df$country, cmc_birth=df$indiv_dob_cmc,
                              yBirth=df$yBirth, cmc_survey=df$surveyDate_cmc,
                              cmc_union1=df$union_start_cmc1,
                              cmc_sep1=df$union_end_cmc1,
                              sep1_motive=df$union_end_motive1,
                              weight=df$indiv_weight)
  }
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

