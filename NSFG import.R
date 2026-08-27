setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.R")
source ("NSFG_harmonize_types.R")
source ("NSFG_lib.R")
source ("NSFG_impute_dissolution.R")
source ("NSFG_impute_dissolution_model.R")
source("lib/KaplanMeierLib.R")
# Read US NSFG surveys
# Some have sps import files
# Some not
library(tidyverse)
library (haven)

# ==== Month-imputation mode (per-reader switch) ====
# Controls how a MISSING month is imputed for year-only event dates. The most
# recent NSFG public-use files (2015-17 onward) give year only, so every month
# is imputed; older files impute only the rare (<10%) out-of-range month.
#   FALSE = legacy: uniform random month, no constraint (current behaviour).
#   TRUE  = constrained: month capped to the survey date so a year-only event
#           cannot land after the interview. Passed to raw_union_history().
# No event-ordering/probabilistic layer is wired for NSFG, so TRUE adds only
# the survey-date cap. Flip to compare old vs new imputation.
capMonth <- TRUE

#### SUMMARY ####
# 1973: complete UH (up to 6), no CohabBefMar ever married women or single with coresiding children
# 1976: complete UH (up to 3), no CohabBefMar ever married women or single with coresiding children
# 1982: complete UH (up to 4), no CohabBefMar
# 1988: only first union, CohabBefMar
# 1995: complete UH (but cohab no widowhood), CohabBefMar
# 2002: complete UH (up to 10 unions), CohabBefMar
# 2006-10: complete UH (up to 10 unions), CohabBefMar
# 2011-13: complete UH (up to 10 unions), CohabBefMar
# 2013-15: complete UH (up to 10 unions), CohabBefMar
# 2015-17: complete UH (up to 10 unions), CohabBefMar, year not cmc for dates
# 2017-19: incomplete UH, year not cmc for dates
# 2022-23: only first union, CohabBefMar, year not cmc for dates

#### 1973 ####
# "9,797 women 15-44 years of age who were married, previously married, or single with children of their own in the household"
# Union history:
# up to 6 unions
# NO info on cohabitation before marriage
# complete date (cmc)
getDatos_1973 <- function (childSex=TRUE) {
  path_1973 <- paste0(nsfgPath, "1973/1973NSFGData.dat")
  
  adjust_cmc <- function (cmc) {
    cmcRes <- cmc
    cmcRes <- ifelse (cmc < 301,NA,cmcRes)
    # missing month, cmc contains the year in the last two digits
    cmcRes <- ifelse ((cmc >= 901)&(cmc <= 996), cmc - 900 + imputed_month (length(cmc)), cmcRes)
    # missing year
    cmcRes <- ifelse ((cmc >= 997)&(cmc <= 999), 9999, cmcRes)
    return (cmcRes)
  }
  imputed_date <- function (cmc) {
    imp <- cmc
    imp <- ifelse (is.na(cmc), NA, 0)
    imp <- ifelse ((cmc < 301), NA, imp)
    # missing month
    imp <- ifelse ((cmc >= 901)&(cmc <= 996),1,imp)
    # missing date or year
    imp <- ifelse ((cmc >= 997)&(cmc <= 999),10,imp)
    
    return (imp)
  }
  
  raw <- readLines(path_1973)
  n <- length(raw)
  datos <- data.frame(country=rep("USA",n), survey="NSFG1973")
  datos$CaseID <- as.integer(substring(raw,1,5)) # OK
  datos$surveyDate_cmc <- as.integer(substring(raw,709,711))
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- as.integer(substring(raw,31,33)) # OK
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12)
  datos$union_status <- as.integer(substring(raw,9,9)) # OK
  datos$union_status <- factor(datos$union_status, levels=c(1,2,3,4,5,6),
                               labels=c("married","cohabiting","widowed","divorced","separated","single with own children"))
  datos$yBirth <- 1900 + floor ((datos$indiv_dob_cmc-1) / 12)
  datos$indiv_weight <- as.integer(substring(raw,734,739))
  datos$pregnant <- as.integer(substring(raw,138,138))
  datos$pregnant <- factor (datos$pregnant, levels=c(1,2,8),labels=c("yes","no","unknown"))
  datos$want_another <- as.integer(substring(raw,266,266))
  datos$want_another <- factor(datos$want_another, levels=c(1,2,3,8),labels=c("yes","no","disagree","unknown"))
  datos$prob_want_another <- NA
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  datos$nUnion_tmp <- as.integer(substring(raw,45,45)) # OK
  datos$nUnion_decl <- ifelse(datos$nUnion_tmp==0,0,NA)
  datos$nUnion_decl <- ifelse(datos$nUnion_tmp==2,1,datos$nUnion_decl)
  datos$nUnion_decl <- ifelse(datos$nUnion_tmp==1,as.integer(substring(raw,46,46)),datos$nUnion_decl) # OK
  datos$nUnion_tmp <- NULL
  
  ##### Union histories #####
  datos_UH <- data.frame(CaseID=datos$CaseID, nUnion=datos$nUnion)
  datos_UH$currUnion_start_type <- as.integer(substring(raw,52,52)) # OK
  datos_UH$currUnion_start_cmc <- adjust_cmc (as.integer(substring(raw,47,49))) # OK
  datos_UH$currUnion_start_cmc_I <- imputed_date (as.integer(substring(raw,47,49))) # OK
  datos_UH$currMarriage_start_cmc <- ifelse(datos_UH$currUnion_start_type==0, datos_UH$currUnion_start_cmc, NA)
  datos_UH$currMarriage_start_cmc_I <- ifelse(datos_UH$currUnion_start_type==0, datos_UH$currUnion_start_cmc_I, NA)
  
  dpos <- 0
  for (u in (1:5)) {
    union_start_type <- paste0("union_start_type",u)
    union_start_cmc <- paste0("union_start_cmc",u)
    union_start_cmc_I <- paste0("union_start_cmc_I",u)
    marriage_start_cmc <- paste0("marriage_start_cmc",u)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I",u)
    union_end_motive <- paste0("union_end_motive",u)
    union_end_cmc <- paste0("union_end_cmc",u)
    union_end_cmc_I <- paste0("union_end_cmc_I",u)
    union_stopLiving_cmc <- paste0("union_stopLiving_cmc",u)
    union_stopLiving_cmc_I <- paste0("union_stopLiving_cmc_I",u)
    
    datos_UH[[union_start_type]] <- as.integer(substring(raw,69+dpos,69+dpos))
    datos_UH[[union_start_cmc]] <-  adjust_cmc (as.integer(substring(raw,64+dpos,66+dpos)))
    datos_UH[[union_start_cmc_I]] <-  imputed_date (as.integer(substring(raw,64+dpos,66+dpos)))
    datos_UH[[marriage_start_cmc]] <- ifelse(datos_UH[[union_start_type]]==0, datos_UH[[union_start_cmc]], NA)
    datos_UH[[marriage_start_cmc_I]] <- ifelse(datos_UH[[union_start_type]]==0, datos_UH[[union_start_cmc_I]], NA)
    datos_UH[[union_end_motive]] <- as.integer(substring(raw,70+dpos,70+dpos))
    datos_UH[[union_end_cmc]] <- adjust_cmc (as.integer(substring(raw,71+dpos,73+dpos)))
    datos_UH[[union_end_cmc_I]] <- imputed_date (as.integer(substring(raw,71+dpos,73+dpos)))
    datos_UH[[union_stopLiving_cmc]] <- adjust_cmc (as.integer(substring(raw,74+dpos,76+dpos)))
    datos_UH[[union_stopLiving_cmc_I]] <- imputed_date (as.integer(substring(raw,74+dpos,76+dpos)))
    
    dpos <- dpos + 13
  }

  dsum <- data.frame(
    curr=!is.na(datos_UH$currUnion_start_cmc),
    u1=!is.na(datos_UH$union_start_cmc1),
    u2=!is.na(datos_UH$union_start_cmc2),
    u3=!is.na(datos_UH$union_start_cmc3),
    u4=!is.na(datos_UH$union_start_cmc4),
    u5=!is.na(datos_UH$union_start_cmc5)
  )
  
  datos_UH$nUnion_computed <- rowSums (dsum)
  datos_UH <- relocate (datos_UH, nUnion_computed, .after= nUnion)
  datos_UH$nUnion <- NULL
  datos_UH <- dplyr::rename (datos_UH, nUnion = nUnion_computed)

  datos_UH$union_end_cmc1 <- ifelse(datos_UH$union_end_motive1 %in% c(1,2),datos_UH$union_stopLiving_cmc1,datos_UH$union_end_cmc1)
  datos_UH$union_end_cmc2 <- ifelse(datos_UH$union_end_motive2 %in% c(1,2),datos_UH$union_stopLiving_cmc2,datos_UH$union_end_cmc2)
  datos_UH$union_end_cmc3 <- ifelse(datos_UH$union_end_motive3 %in% c(1,2),datos_UH$union_stopLiving_cmc3,datos_UH$union_end_cmc3)
  datos_UH$union_end_cmc4 <- ifelse(datos_UH$union_end_motive4 %in% c(1,2),datos_UH$union_stopLiving_cmc4,datos_UH$union_end_cmc4)
  datos_UH$union_end_cmc5 <- ifelse(datos_UH$union_end_motive5 %in% c(1,2),datos_UH$union_stopLiving_cmc5,datos_UH$union_end_cmc5)

  datos_UH$union_end_cmc_I1 <- ifelse(datos_UH$union_end_motive1 %in% c(1,2),datos_UH$union_stopLiving_cmc_I1,datos_UH$union_end_cmc_I1)
  datos_UH$union_end_cmc_I2 <- ifelse(datos_UH$union_end_motive2 %in% c(1,2),datos_UH$union_stopLiving_cmc_I2,datos_UH$union_end_cmc_I2)
  datos_UH$union_end_cmc_I3 <- ifelse(datos_UH$union_end_motive3 %in% c(1,2),datos_UH$union_stopLiving_cmc_I3,datos_UH$union_end_cmc_I3)
  datos_UH$union_end_cmc_I4 <- ifelse(datos_UH$union_end_motive4 %in% c(1,2),datos_UH$union_stopLiving_cmc_I4,datos_UH$union_end_cmc_I4)
  datos_UH$union_end_cmc_I5 <- ifelse(datos_UH$union_end_motive5 %in% c(1,2),datos_UH$union_stopLiving_cmc_I5,datos_UH$union_end_cmc_I5)
  
  maxU <- max (datos_UH$nUnion, na.rm=TRUE)
  for (i in (1:5)) {
    union_start_cmc <- paste0("union_start_cmc", i)
    union_start_cmc_I <- paste0("union_start_cmc_I", i)
    union_start_type <- paste0("union_start_type", i)
    marriage_start_cmc <- paste0("marriage_start_cmc", i)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", i)
    union_end_cmc <- paste0("union_end_cmc", i)
    union_end_cmc_I <- paste0("union_end_cmc_I", i)
    union_end_motive <- paste0("union_end_motive", i)
    # if union_start_cmc is NA at union order i, and nUnion == i, we recopy data from current union / marriage
    idx <- ((datos_UH$nUnion == i))&(is.na(datos_UH [[union_start_cmc]]))
    datos_UH [[union_start_type]][idx] <- datos_UH$currUnion_start_type[idx]
    datos_UH [[marriage_start_cmc]][idx] <- datos_UH$currMarriage_start_cmc[idx]
    datos_UH [[marriage_start_cmc_I]][idx] <- datos_UH$currMarriage_start_cmc_I[idx]
    datos_UH [[union_start_cmc]][idx] <- datos_UH$currUnion_start_cmc[idx]
    datos_UH [[union_start_cmc_I]][idx] <- datos_UH$currUnion_start_cmc_I[idx]
  }
  if (maxU > 5) {
    idx <- (datos_UH$nUnion == 6)
    datos_UH$union_start_type6[idx] <- datos_UH$currUnion_start_type[idx]
    datos_UH$union_start_cmc6[idx] <- datos_UH$currUnion_start_cmc[idx]
    datos_UH$union_start_cmc_I6[idx] <- datos_UH$currUnion_start_cmc_I[idx]
    datos_UH$marriage_start_cmc6[idx] <- datos_UH$currMarriage_start_cmc[idx]
    datos_UH$marriage_start_cmc_I6[idx] <- datos_UH$currMarriage_start_cmc_I[idx]
    datos_UH$union_end_motive6 <- NA
    datos_UH$union_end_cmc6 <- NA
    datos_UH$union_end_cmc_I6 <- NA
  }
  for (i in (1:maxU)) {
    union_start_cmc <- paste0("union_start_cmc", i)
    union_start_cmc_I <- paste0("union_start_cmc_I", i)
    union_end_cmc <- paste0("union_end_cmc", i)
    union_end_cmc_I <- paste0("union_end_cmc_I", i)
    marriage_start_cmc <- paste0("marriage_start_cmc", i)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", i)
    union_start_type <- paste0("union_start_type", i)
    union_end_motive <- paste0("union_end_motive", i)
    datos_UH [[union_start_type]] <- factor (datos_UH [[union_start_type]], levels=c(0,1,9), labels=(c("marriage", "cohabitation","unknown")))
    datos_UH [[union_end_motive]] <- ifelse ((is.na(datos_UH [[union_end_cmc]]))&(!is.na(datos_UH [[union_start_cmc]])), 0, datos_UH [[union_end_motive]])
    datos_UH [[union_end_motive]] <- factor (datos_UH [[union_end_motive]], levels=c(0,1,2,3), labels=(c("in union", "separation", "separation","widowhood")))
  }
  
  datos_UH$currUnion_start_type <- NULL
  datos_UH$currUnion_start_cmc <- NULL
  datos_UH$currUnion_start_cmc_I <- NULL
  datos_UH$currMarriage_start_cmc <- NULL
  datos_UH$currMarriage_start_cmc_I <- NULL
  
  datos_UH$union_stopLiving_cmc1 <- NULL
  datos_UH$union_stopLiving_cmc2 <- NULL
  datos_UH$union_stopLiving_cmc3 <- NULL
  datos_UH$union_stopLiving_cmc4 <- NULL
  datos_UH$union_stopLiving_cmc5 <- NULL

  datos_UH$union_stopLiving_cmc_I1 <- NULL
  datos_UH$union_stopLiving_cmc_I2 <- NULL
  datos_UH$union_stopLiving_cmc_I3 <- NULL
  datos_UH$union_stopLiving_cmc_I4 <- NULL
  datos_UH$union_stopLiving_cmc_I5 <- NULL
  
  datos <- datos %>%
    left_join(datos_UH, by = "CaseID")

  if (all.equal(datos$nUnion,datos$nUnion_decl)) {
    datos$nUnion_decl <- NULL
  } else {
    stop ("discrepancy between nUnion and nUnion_decl")
  }
  
  ##### Live births histories #####
  datos$nBioKids <- as.integer(substring(raw,130,131))
  datos$nBioKids [is.na(datos$nBioKids)] <- 0

  maxB <- max (datos$nBioKids, na.rm=TRUE)
  dpos <- 0
  for (b in (1:maxB)) {
    dob_cmc <- paste0("dob_cmc",b)
    dob_cmc_I <- paste0("dob_cmc_I",b)
    dod_cmc <- paste0("dod_cmc",b)
    dod_cmc_I <- paste0("dod_cmc_I",b)
    plurality <- paste0("plurality",b)
    sex <- paste0("sex",b)
    datos [[dob_cmc]] <- adjust_cmc (as.integer(substring(raw,1107+dpos,1109+dpos)))
    datos [[dob_cmc_I]] <- imputed_date (as.integer(substring(raw,1107+dpos,1109+dpos)))
    datos [[dod_cmc]] <- adjust_cmc (as.integer(substring(raw,1124+dpos,1126+dpos)))
    datos [[dod_cmc_I]] <- imputed_date (as.integer(substring(raw,1124+dpos,1126+dpos)))
    #datos [[plurality]] <- as.integer(substring(raw,1112+dpos,1112+dpos))
    if (isTRUE(childSex)) datos [[sex]] <- as.integer(substring(raw,1113+dpos,1113+dpos))
    dpos <- dpos + 25
  }

  # count live births again
  cols <- paste0("dob_cmc", 1:17)
  idx <- !is.na(datos[, cols])
  datos$birth_count <- rowSums(!is.na(datos[, cols]))
  datos <- relocate (datos, birth_count, .after=nBioKids)
  
  if (all.equal(datos$birth_count,datos$nBioKids)) {
    datos$birth_count <- NULL
  } else {
    stop ("discrepancy between nBioKids and birth_count")
  }

  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_1973 <- getDatos_1973()
NSFG_ENADID_1973 <- cleanENADID(NSFG_ENADID_1973)
NSFG_ENADID_1973 <-  reorder_birthHistory(NSFG_ENADID_1973)

pathNSFG_ENADID_1973 <- path.expand(paste0(dataPath, "NSFG_ENADID_1973.Rdat"))
save(NSFG_ENADID_1973, file = pathNSFG_ENADID_1973)

#### 1976 ####
# "8,611 women 15-44 years of age who were married, previously married, or single with children of their own in the household"
# Union history:
# up to 3 unions, including the current one
# NO info on cohabitation before marriage
# complete date (cmc)
getDatos_1976 <- function () {
  path_1976 <- paste0(nsfgPath, "1976/1976NSFGData.dat")

  adjust_cmc <- function (cmc, closeEvent=TRUE) {
    cmcRes <- cmc
    cmcRes <- ifelse (cmc < 301,NA,cmcRes)
    # missing month, cmc contains the year in the last two digits
    cmcRes <- ifelse ((cmc >= 901)&(cmc <= 924)&isFALSE(closeEvent),(cmc - 900) * 12 + imputed_month (length(cmc)),cmcRes)
    cmcRes <- ifelse ((cmc >= 925)&(cmc <= 976), (cmc - 900) * 12 + imputed_month (length(cmc)), cmcRes)
    # missing year
    cmcRes <- ifelse ((cmc >= 997)&(cmc <= 999), 9999, cmcRes)
    return (cmcRes)
  }
  imputed_date <- function (cmc, closeEvent=TRUE) {
    imp <- cmc
    imp <- ifelse (is.na(cmc), NA, 0)
    imp <- ifelse ((cmc < 301), NA, imp)
    # missing month
    imp <- ifelse ((cmc >= 901)&(cmc <= 924)&isFALSE(closeEvent),1,imp)
    imp <- ifelse ((cmc >= 925)&(cmc <= 976),1,imp)
    # missing date or year
    imp <- ifelse ((cmc >= 997)&(cmc <= 999),10,imp)
    
    return (imp)
  }
  
  raw <- readLines(path_1976)
  n <- length(raw)
  datos <- data.frame(country=rep("USA",n), survey="NSFG1976")
  datos$CaseID <- as.integer(substring(raw,1,5)) # OK
  datos$union_status <- as.integer(substring(raw,6,6)) # OK
  datos$raw <- substring(raw,1,1000)
  #rm (raw)
  # separate between individual and interval (birth record)
  data_interval <- subset (datos, union_status==5)
  datos <- subset (datos, union_status!=5)
  
  # individuals
  datos$union_status <- factor(datos$union_status, levels=c(1,2,3,4,5),
                               labels=c("married","cohabiting","widowed/divorced/separated","single with own children","unknown"))
  datos$surveyDate_cmc <- adjust_cmc (as.integer(substring(datos$raw,709,711))) # OK
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- adjust_cmc (as.integer(substring(datos$raw,13,15))) # OK
  datos$indiv_dob_cmc_I <- imputed_date (as.integer(substring(datos$raw,13,15))) # OK
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12)
  datos$yBirth <- yearFrom_cmc(datos$indiv_dob_cmc)
  datos$indiv_weight <- as.integer(substring(datos$raw,720,725)) # OK
  datos$pregnant <- as.integer(substring(datos$raw,99,99))
  datos$pregnant <- factor (datos$pregnant, levels=c(1,2,8,9),labels=c("yes","no","unknown","NA"))
  datos$want_another <- NA
  datos$want_another <- factor(datos$want_another, levels=c(1,2,3,8),labels=c("yes","no","disagree","unknown"))
  datos$prob_want_another <- NA
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union histories #####
  datos$nUnion <- as.integer(substring(datos$raw,26,26)) # OK
  datos$currUnion_start_type <- as.integer(substring(datos$raw,32,32)) # OK
  datos$currUnion_start_cmc <- adjust_cmc (as.integer(substring(datos$raw,27,29))) # OK
  datos$currUnion_start_cmc_I <- imputed_date (as.integer(substring(datos$raw,27,29))) # OK
  datos$currUnion_start_cmc <- ifelse(datos$currUnion_start_cmc==0,NA,datos$currUnion_start_cmc)
  datos$currMarriage_start_cmc <- ifelse(datos$currUnion_start_type==2, datos$currUnion_start_cmc, NA) #marriage
  datos$currMarriage_start_cmc_I <- ifelse(datos$currUnion_start_type==2, datos$currUnion_start_cmc_I, NA) #marriage
  datos$currUnion_end_cmc <- adjust_cmc (as.integer(substring(datos$raw,34,36))) # OK
  datos$currUnion_end_cmc_I <- imputed_date (as.integer(substring(datos$raw,34,36))) # OK
  datos$currUnion_end_cmc <- ifelse(datos$currUnion_end_cmc==0,NA,datos$currUnion_end_cmc)
  datos$currUnion_stopLiving_cmc <- adjust_cmc (as.integer(substring(datos$raw,37,39))) # OK
  datos$currUnion_stopLiving_cmc_I <- imputed_date (as.integer(substring(datos$raw,37,39))) # OK
  datos$currUnion_stopLiving_cmc <- ifelse(datos$currUnion_stopLiving_cmc==0,NA,datos$currUnion_stopLiving_cmc)
  datos$currUnion_end_motive <- as.integer(substring(datos$raw,33,33)) # OK
  datos$currUnion_end_motive[!is.na(datos$currUnion_start_cmc)&is.na(datos$currUnion_end_motive)] <- 0
  datos$currUnion_end_cmc [(datos$currUnion_end_motive %in% c(4,5))] <-
    datos$currUnion_stopLiving_cmc [(datos$currUnion_end_motive %in% c(4,5))]
  datos$currUnion_end_cmc_I [(datos$currUnion_end_motive %in% c(4,5))] <-
    datos$currUnion_stopLiving_cmc_I [(datos$currUnion_end_motive %in% c(4,5))]
  
  pos <- 40
  datos$union_start_type1 <- as.integer(substring(datos$raw,pos+5,pos+5)) # OK
  datos$union_start_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,pos,pos+2))) # OK
  datos$union_start_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,pos,pos+2))) # OK
  datos$union_start_cmc1 <- ifelse(datos$union_start_cmc1==0,NA,datos$union_start_cmc1)
  datos$marriage_start_cmc1 <- ifelse(datos$union_start_type1==2, datos$union_start_cmc1, NA) # marriage
  datos$marriage_start_cmc_I1 <- ifelse(datos$union_start_type1==2, datos$union_start_cmc_I1, NA) # marriage
  datos$union_end_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,pos+7,pos+9))) # OK
  datos$union_end_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,pos+7,pos+9))) # OK
  datos$union_end_cmc1 <- ifelse(datos$union_end_cmc1==0,NA,datos$union_end_cmc1)
  datos$union_stopLiving_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,pos+10,pos+12))) # OK
  datos$union_stopLiving_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,pos+10,pos+12))) # OK
  datos$union_stopLiving_cmc1 <- ifelse(datos$union_stopLiving_cmc1==0,NA,datos$union_stopLiving_cmc1)
  datos$union_end_motive1 <- as.integer(substring(datos$raw,pos+6,pos+6)) # OK
  datos$union_end_motive1[!is.na(datos$union_start_cmc1)&is.na(datos$union_end_motive1)] <- 0
  datos$union_end_cmc1 [(datos$union_end_motive1 %in% c(4,5))] <- datos$union_stopLiving_cmc1 [(datos$union_end_motive1 %in% c(4,5))]
  datos$union_end_cmc_I1 [(datos$union_end_motive1 %in% c(4,5))] <- datos$union_stopLiving_cmc_I1 [(datos$union_end_motive1 %in% c(4,5))]
  
  pos <- pos + 13
  datos$union_start_type2 <- as.integer(substring(datos$raw,pos+5,pos+5)) # OK
  datos$union_start_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,pos,pos+2))) # OK
  datos$union_start_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,pos,pos+2))) # OK
  datos$union_start_cmc2 <- ifelse(datos$union_start_cmc2==0,NA,datos$union_start_cmc2)
  datos$marriage_start_cmc2 <- ifelse(datos$union_start_type2==2, datos$union_start_cmc2, NA) # marriage
  datos$marriage_start_cmc_I2 <- ifelse(datos$union_start_type2==2, datos$union_start_cmc_I2, NA) # marriage
  datos$union_end_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,pos+7,pos+9))) # OK
  datos$union_end_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,pos+7,pos+9))) # OK
  datos$union_end_cmc2 <- ifelse(datos$union_end_cmc2==0,NA,datos$union_end_cmc2)
  datos$union_stopLiving_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,pos+10,pos+12))) # OK
  datos$union_stopLiving_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,pos+10,pos+12))) # OK
  datos$union_stopLiving_cmc2 <- ifelse(datos$union_stopLiving_cmc2==0,NA,datos$union_stopLiving_cmc2)
  datos$union_end_motive2 <- as.integer(substring(datos$raw,pos+6,pos+6)) # OK
  datos$union_end_motive2[!is.na(datos$union_start_cmc2)&is.na(datos$union_end_motive2)] <- 0
  datos$union_end_cmc2 [(datos$union_end_motive2 %in% c(4,5))] <- datos$union_stopLiving_cmc2 [(datos$union_end_motive2 %in% c(4,5))]
  datos$union_end_cmc_I2 [(datos$union_end_motive2 %in% c(4,5))] <- datos$union_stopLiving_cmc_I2 [(datos$union_end_motive2 %in% c(4,5))]
  
  dsum <- data.frame(
    curr=!is.na(datos$currUnion_start_cmc),
    u1=!is.na(datos$union_start_cmc1),
    u2=!is.na(datos$union_start_cmc2)
  )
  
  datos$nUnion_computed <- rowSums (dsum)
  datos <- relocate (datos, nUnion_computed, .after= nUnion)
  datos$nUnion <- NULL
  datos <- dplyr::rename (datos, nUnion = nUnion_computed)
  
  datos$union_start_type3 <- NA
  datos$union_start_cmc3 <- NA
  datos$union_start_cmc_I3 <- NA
  datos$marriage_start_cmc3 <- NA
  datos$marriage_start_cmc_I3 <- NA
  datos$union_end_cmc3 <- NA
  datos$union_end_cmc_I3 <- NA
  datos$union_stopLiving_cmc3 <- NA
  datos$union_stopLiving_cmc_I3 <- NA
  datos$union_end_motive3 <- NA

  # put currUnion in its place in the union history (but if current union is the fourth or more, it will be misplaced, but we have no choice)
  maxU <- max (datos$nUnion, na.rm=TRUE)
  for (i in (1:maxU)) {
    union_start_cmc <- paste0("union_start_cmc", i)
    union_start_cmc_I <- paste0("union_start_cmc_I", i)
    union_start_type <- paste0("union_start_type", i)
    marriage_start_cmc <- paste0("marriage_start_cmc", i)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", i)
    union_end_cmc <- paste0("union_end_cmc", i)
    union_end_cmc_I <- paste0("union_end_cmc_I", i)
    union_stopLiving_cmc <- paste0("union_stopLiving_cmc", i)
    union_stopLiving_cmc_I <- paste0("union_stopLiving_cmc_I", i)
    union_end_motive <- paste0("union_end_motive", i)
    # if union_start_cmc is NA at union order i, and nUnion == i, we recopy data from current union / marriage
    idx <- ((datos$nUnion == i))&(is.na(datos [[union_start_cmc]]))
    datos [[union_start_type]][idx] <- datos$currUnion_start_type[idx]
    datos [[union_start_cmc]][idx] <- datos$currUnion_start_cmc[idx]
    datos [[union_start_cmc_I]][idx] <- datos$currUnion_start_cmc_I[idx]
    datos [[marriage_start_cmc]][idx] <- datos$currMarriage_start_cmc[idx]
    datos [[marriage_start_cmc_I]][idx] <- datos$currMarriage_start_cmc_I[idx]
    datos [[union_end_cmc]][idx] <- datos$currUnion_end_cmc[idx]
    datos [[union_end_cmc_I]][idx] <- datos$currUnion_end_cmc_I[idx]
    datos [[union_stopLiving_cmc]][idx] <- datos$currUnion_stopLiving_cmc[idx]
    datos [[union_stopLiving_cmc_I]][idx] <- datos$currUnion_stopLiving_cmc_I[idx]
    datos [[union_end_motive]][idx] <- datos$currUnion_end_motive[idx]
    datos[[union_start_type]] <- factor (datos[[union_start_type]], levels=c(1,2,9), labels=c("cohabitation","marriage","unknown"))
    datos[[union_end_motive]] <- factor (datos[[union_end_motive]], levels=c(0,3,4,5,9),
                                          labels=c("in union","widowhood","separation","separation","unknown"))
  }

  datos$currUnion_start_type <- NULL
  datos$currUnion_start_cmc <- NULL
  datos$currUnion_start_cmc_I <- NULL
  datos$currMarriage_start_cmc <- NULL
  datos$currMarriage_start_cmc_I <- NULL
  datos$currUnion_end_cmc <- NULL
  datos$currUnion_end_cmc_I <- NULL
  datos$currUnion_stopLiving_cmc <- NULL
  datos$currUnion_stopLiving_cmc_I <- NULL
  datos$currUnion_end_motive <- NULL
  datos$raw <- NULL
  
  ##### Live births histories #####
  births <- data.frame (CaseID=data_interval$CaseID)
  
  births$pregNumber <- as.integer(substring(data_interval$raw,7,8)) # OK
  births$dob_cmc <- adjust_cmc (as.integer(substring(data_interval$raw,9,11))) # OK
  births$dob_cmc_I <- imputed_date (as.integer(substring(data_interval$raw,9,11))) # OK
  
  births$childNumber1 <- as.integer(substring(data_interval$raw,18,19)) # OK
  births$babysex1 <- as.integer(substring(data_interval$raw,20,20)) # OK
  births$dod_cmc1 <- adjust_cmc (as.integer(substring(data_interval$raw,29,31))) # OK
  births$dod_cmc_I1 <- imputed_date (as.integer(substring(data_interval$raw,29,31))) # OK
  births$dod_cmc1 [births$dod_cmc1==0] <- NA
  births$childNumber2 <- as.integer(substring(data_interval$raw,35,36)) # OK
  births$babysex2 <- as.integer(substring(data_interval$raw,37,37)) # OK
  births$dod_cmc2 <- adjust_cmc (as.integer(substring(data_interval$raw,46,48))) # OK
  births$dod_cmc_I2 <- imputed_date (as.integer(substring(data_interval$raw,46,48))) # OK
  births$dod_cmc2 [births$dod_cmc2==0] <- NA
  births$childNumber3 <- as.integer(substring(data_interval$raw,52,53)) # OK
  births$babysex3 <- as.integer(substring(data_interval$raw,54,54)) # OK
  births$dod_cmc3 <- adjust_cmc (as.integer(substring(data_interval$raw,63,65))) # OK
  births$dod_cmc_I3 <- imputed_date (as.integer(substring(data_interval$raw,63,65))) # OK
  births$dod_cmc3 [births$dod_cmc3==0] <- NA
  
  births$nLiveBirths <- ifelse(!is.na(births$childNumber1),1,0) +
    ifelse(!is.na(births$childNumber2),1,0) +
    ifelse(!is.na(births$childNumber3),1,0)
  
  births <- subset(births, nLiveBirths > 0)
  
  births <- live_birth_history2(births)
  
  ##### final women dataframe with live births #####
  datos <- datos %>%
    left_join(births, by = "CaseID")
  
  datos$nBioKids [is.na(datos$nBioKids)] <- 0
  
  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_1976 <- getDatos_1976()
NSFG_ENADID_1976 <- cleanENADID(NSFG_ENADID_1976)
NSFG_ENADID_1976 <-  reorder_birthHistory(NSFG_ENADID_1976)

pathNSFG_ENADID_1976 <- path.expand(paste0(dataPath, "NSFG_ENADID_1976.Rdat"))
save(NSFG_ENADID_1976, file = pathNSFG_ENADID_1976)

#### 1982 ####
# Union history:
# up to 4 unions, info on first, second, previous and last union. This leaves out 2 women with 5 unions
# NO info on cohabitation before marriage
# complete date (cmc)
getDatos_1982 <- function () {
  path_1982 <- paste0(nsfgPath, "1982/1982NSFGData.dat")
  
  adjust_cmc <- function (cmc) {
    cmc <- ifelse(cmc %in% c(9797,9898,9999), 9999, cmc)
    # unknow months are coded as 9400-9997, we recode them with random month number
    cmc <- ifelse((cmc >= 9400) & (cmc <= 9997), (cmc - 9000) - 6 + imputed_month (length(cmc)), cmc)
    return (cmc)
  }
  imputed_date <- function (cmc) {
    imp <- 0
    imp <- ifelse((cmc >= 9400) & (cmc <= 9997), 1, imp)
    imp <- ifelse(cmc %in% c(9797,9898,9999), 10, imp)
    return (imp)
  }
  
  raw <- readLines(path_1982)
  n <- length(raw)
  datos <- data.frame(country=rep("USA",n), survey="NSFG1982")
  datos$CaseID <- as.integer(substring(raw,1494,1498)) # OK
  datos$record_type <- as.integer(substring(raw,1499,1500)) # OK
  datos$raw <- substring(raw,1,1500) # OK
  #rm (raw)
  # separate between individual and interval (birth record)
  data_interval <- subset (datos, record_type>0) # OK
  datos <- subset (datos, record_type==0) # OK
  
  datos$surveyDate_cmc <- adjust_cmc (as.integer(substring(datos$raw,841,844))) # OK
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- adjust_cmc (as.integer(substring(datos$raw,12,15))) # OK
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12)
  datos$union_status <- as.integer(substring(datos$raw,1006,1006)) #OK
  datos$union_status <- factor(datos$union_status, levels=c(1,2,3,4,5,6),
                               labels=c("married","cohabiting","widowed","divorced","separated","single"))
  datos$yBirth <- 1900 + floor ((datos$indiv_dob_cmc-1) / 12)
  datos$indiv_weight <- as.integer(substring(datos$raw,976,982)) # OK
  datos$pregnant <- as.integer(substring(datos$raw,63,63)) # OK
  datos$pregnant <- factor (datos$pregnant, levels=c(1,2,7,8),labels=c("yes","no","refused","unknown"))
  datos$want_another <- NA
  datos$prob_want_another <- NA
  datos$age_first_sex <- floor ((adjust_cmc (as.integer(substring(datos$raw,1292,1295))) - datos$indiv_dob_cmc) / 12) # OK
  datos$ever_contraception <- NA
  
  datos$nUnion <- as.integer(substring(datos$raw,590,591)) # OK
  datos$nUnion <- ifelse(is.na(datos$nUnion),0,datos$nUnion)
  datos$currUnion_start_cmc <- adjust_cmc (as.integer(substring(datos$raw,592,595))) # OK
  datos$currUnion_start_cmc_I <- imputed_date (as.integer(substring(datos$raw,592,595))) # OK
  datos$currUnion_start_type <- as.integer(substring(datos$raw,598,598)) # OK
  datos$currUnion_start_type <- factor(datos$currUnion_start_type, levels=c(1,2,7,8,9),
                                       labels=c("cohabitation","marriage","refused","don't remember","unknown"))
  datos$currMarriage_start_cmc <- ifelse(datos$currUnion_start_type=="marriage", datos$currUnion_start_cmc, NA)
  datos$currMarriage_start_cmc_I <- ifelse(datos$currUnion_start_type=="marriage", datos$currUnion_start_cmc_I, NA)
  datos$currUnion_end_cmc <- NA
  datos$currUnion_end_cmc_I <- NA
  datos$currUnion_end_motive <- 0
  datos$currUnion_end_motive <- factor(datos$currUnion_end_motive, levels=c(0,3,4,5,7,8,9),
                                    labels=c("in union","widowhood","separation","separation","refused","don't remember","unknown"))
  
  ##### Union histories #####
  # first marriage
  datos$union_start_type1 <- as.integer(substring(datos$raw,605,605)) # OK
  datos$union_start_type1 <- factor(datos$union_start_type1, levels=c(1,2,7,8,9),
                                    labels=c("cohabitation","marriage","refused","don't remember","unknown"))
  datos$union_start_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,599,602))) # OK
  datos$union_start_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,599,602))) # OK
  datos$marriage_start_cmc1 <- ifelse(datos$union_start_type1=="marriage", datos$union_start_cmc1, NA)
  datos$union_end_motive1 <- as.integer(substring(datos$raw,606,606)) # OK
  datos$union_husbandDeath_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,607,610))) # OK
  datos$union_husbandDeath_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,607,610))) # OK
  datos$union_stopLiving_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,613,616))) # OK
  datos$union_stopLiving_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,613,616))) # OK
  datos$union_divorce_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,619,622))) # OK
  datos$union_divorce_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,619,622))) # OK
  datos$union_stopLiving2_cmc1 <- adjust_cmc (as.integer(substring(datos$raw,625,628))) # OK
  datos$union_stopLiving2_cmc_I1 <- imputed_date (as.integer(substring(datos$raw,625,628))) # OK
  
  datos <- datos %>%
    dplyr::mutate(union_end_cmc1 = case_when(
      union_end_motive1 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc1)&(union_stopLiving_cmc1!=9999) ~ union_stopLiving_cmc1,
        !is.na(union_divorce_cmc1) ~ union_divorce_cmc1,
        !is.na(union_stopLiving2_cmc1) ~ union_stopLiving2_cmc1,
        TRUE ~ NA_integer_
      ),
      union_end_motive1 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc1) ~ union_husbandDeath_cmc1,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))
  datos <- datos %>%
    dplyr::mutate(union_end_cmc_I1 = case_when(
      union_end_motive1 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc1)&(union_stopLiving_cmc1!=9999) ~ union_stopLiving_cmc_I1,
        !is.na(union_divorce_cmc1) ~ union_divorce_cmc_I1,
        !is.na(union_stopLiving2_cmc1) ~ union_stopLiving2_cmc_I1,
        TRUE ~ NA_integer_
      ),
      union_end_motive1 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc1) ~ union_husbandDeath_cmc_I1,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))
    
  datos$union_end_motive1 <- ifelse (!is.na(datos$union_start_cmc1)&is.na(datos$union_end_cmc1),0,datos$union_end_motive1)
  datos$union_end_motive1 <- factor(datos$union_end_motive1, levels=c(0,3,4,5,7,8,9),
                                    labels=c("in union","widowhood","separation","separation","refused","don't remember","unknown"))

  datos$union_husbandDeath_cmc1 <- NULL
  datos$union_stopLiving_cmc1 <- NULL
  datos$union_divorce_cmc1 <- NULL
  datos$union_stopLiving2_cmc1 <- NULL

  datos$union_husbandDeath_cmc_I1 <- NULL
  datos$union_stopLiving_cmc_I1 <- NULL
  datos$union_divorce_cmc_I1 <- NULL
  datos$union_stopLiving2_cmc_I1 <- NULL
  
  # second marriage
  datos$union_start_type2 <- as.integer(substring(datos$raw,637,637)) # OK
  datos$union_start_type2 <- factor(datos$union_start_type2, levels=c(1,2,7,8,9),
                                    labels=c("cohabitation","marriage","refused","don't remember","unknown"))
  datos$union_start_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,631,634))) # OK
  datos$union_start_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,631,634))) # OK
  datos$marriage_start_cmc2 <- ifelse(datos$union_start_type2=="marriage", datos$union_start_cmc2, NA)
  datos$marriage_start_cmc_I2 <- ifelse(datos$union_start_type2=="marriage", datos$union_start_cmc_I2, NA)
  datos$union_end_motive2 <- as.integer(substring(datos$raw,638,638)) # OK
  datos$union_husbandDeath_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,639,642))) # OK
  datos$union_husbandDeath_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,639,642))) # OK
  datos$union_stopLiving_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,645,648))) # OK
  datos$union_stopLiving_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,645,648))) # OK
  datos$union_divorce_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,651,654))) # OK
  datos$union_divorce_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,651,654))) # OK
  datos$union_stopLiving2_cmc2 <- adjust_cmc (as.integer(substring(datos$raw,657,660))) # OK
  datos$union_stopLiving2_cmc_I2 <- imputed_date (as.integer(substring(datos$raw,657,660))) # OK
  
  datos <- datos %>%
    dplyr::mutate(union_end_cmc2 = case_when(
      union_end_motive2 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc2)&(union_stopLiving_cmc2!=9999) ~ union_stopLiving_cmc2,
        !is.na(union_divorce_cmc2) ~ union_divorce_cmc2,
        !is.na(union_stopLiving2_cmc2) ~ union_stopLiving2_cmc2,
        TRUE ~ NA_integer_
      ),
      union_end_motive2 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc2) ~ union_husbandDeath_cmc2,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))
  datos <- datos %>%
    dplyr::mutate(union_end_cmc_I2 = case_when(
      union_end_motive2 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc2)&(union_stopLiving_cmc2!=9999) ~ union_stopLiving_cmc_I2,
        !is.na(union_divorce_cmc2) ~ union_divorce_cmc_I2,
        !is.na(union_stopLiving2_cmc2) ~ union_stopLiving2_cmc_I2,
        TRUE ~ NA_integer_
      ),
      union_end_motive2 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc2) ~ union_husbandDeath_cmc_I2,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))

  datos$union_end_motive2 <- ifelse (!is.na(datos$union_start_cmc2)&is.na(datos$union_end_cmc2),0,datos$union_end_motive2)
  datos$union_end_motive2 <- factor(datos$union_end_motive2, levels=c(0,3,4,5,7,8,9),
                                    labels=c("in union","widowhood","separation","separation","refused","don't remember","unknown"))

  datos$union_husbandDeath_cmc2 <- NULL
  datos$union_stopLiving_cmc2 <- NULL
  datos$union_divorce_cmc2 <- NULL
  datos$union_stopLiving2_cmc2 <- NULL

  datos$union_husbandDeath_cmc_I2 <- NULL
  datos$union_stopLiving_cmc_I2 <- NULL
  datos$union_divorce_cmc_I2 <- NULL
  datos$union_stopLiving2_cmc_I2 <- NULL
  
  # previous marriage is considered the third, to simplify
  datos$union_start_type3 <- as.integer(substring(datos$raw,669,669)) # OK
  datos$union_start_type3 <- factor(datos$union_start_type3, levels=c(1,2,7,8,9),
                                    labels=c("cohabitation","marriage","refused","don't remember","unknown"))
  datos$union_start_cmc3 <- adjust_cmc (as.integer(substring(datos$raw,663,666))) # OK
  datos$union_start_cmc_I3 <- imputed_date (as.integer(substring(datos$raw,663,666))) # OK
  datos$marriage_start_cmc3 <- ifelse(datos$union_start_type3=="marriage", datos$union_start_cmc3, NA)
  datos$marriage_start_cmc_I3 <- ifelse(datos$union_start_type3=="marriage", datos$union_start_cmc_I3, NA)
  datos$union_end_motive3 <- as.integer(substring(datos$raw,670,670)) # OK
  datos$union_husbandDeath_cmc3 <- adjust_cmc (as.integer(substring(datos$raw,671,674))) # OK
  datos$union_husbandDeath_cmc_I3 <- imputed_date (as.integer(substring(datos$raw,671,674))) # OK
  datos$union_stopLiving_cmc3 <- adjust_cmc (as.integer(substring(datos$raw,677,680))) # OK
  datos$union_stopLiving_cmc_I3 <- imputed_date (as.integer(substring(datos$raw,677,680))) # OK
  datos$union_divorce_cmc3 <- adjust_cmc (as.integer(substring(datos$raw,683,686))) # OK
  datos$union_divorce_cmc_I3 <- imputed_date (as.integer(substring(datos$raw,683,686))) # OK
  datos$union_stopLiving2_cmc3 <- adjust_cmc (as.integer(substring(datos$raw,689,692))) # OK
  datos$union_stopLiving2_cmc_I3 <- imputed_date (as.integer(substring(datos$raw,689,692))) # OK
  
  datos <- datos %>%
    dplyr::mutate(union_end_cmc3 = case_when(
      union_end_motive3 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc3)&(union_stopLiving_cmc3!=9999) ~ union_stopLiving_cmc3,
        !is.na(union_divorce_cmc3) ~ union_divorce_cmc3,
        !is.na(union_stopLiving2_cmc3) ~ union_stopLiving2_cmc3,
        TRUE ~ NA_integer_
      ),
      union_end_motive3 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc3) ~ union_husbandDeath_cmc3,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))
  datos <- datos %>%
    dplyr::mutate(union_end_cmc_I3 = case_when(
      union_end_motive3 %in% c(4,5) ~ case_when( # separation
        !is.na(union_stopLiving_cmc3)&(union_stopLiving_cmc3!=9999) ~ union_stopLiving_cmc_I3,
        !is.na(union_divorce_cmc3) ~ union_divorce_cmc_I3,
        !is.na(union_stopLiving2_cmc3) ~ union_stopLiving2_cmc_I3,
        TRUE ~ NA_integer_
      ),
      union_end_motive3 == 3 ~ case_when( # widowhood
        !is.na(union_husbandDeath_cmc3) ~ union_husbandDeath_cmc_I3,
        TRUE ~ NA_integer_
      ),
      .default = NA_integer_
    ))
  
  datos$union_end_motive3 <- ifelse (!is.na(datos$union_start_cmc3)&is.na(datos$union_end_cmc3),0,datos$union_end_motive3)
  datos$union_end_motive3 <- factor(datos$union_end_motive3, levels=c(0,3,4,5,7,8,9),
                                    labels=c("in union","widowhood","separation","separation","refused","don't remember","unknown"))

  datos$union_husbandDeath_cmc3 <- NULL
  datos$union_stopLiving_cmc3 <- NULL
  datos$union_divorce_cmc3 <- NULL
  datos$union_stopLiving2_cmc3 <- NULL

  datos$union_husbandDeath_cmc_I3 <- NULL
  datos$union_stopLiving_cmc_I3 <- NULL
  datos$union_divorce_cmc_I3 <- NULL
  datos$union_stopLiving2_cmc_I3 <- NULL
  
  dsum <- data.frame(
    curr=!is.na(datos$currUnion_start_cmc),
    u1=!is.na(datos$union_start_cmc1),
    u2=!is.na(datos$union_start_cmc2),
    u3=!is.na(datos$union_start_cmc3)
  )
  
  datos$nUnion_computed <- rowSums (dsum)
  datos <- relocate (datos, nUnion_computed, .after= nUnion)

  datos$union_start_type4 <- NA
  datos$union_start_type4 <- factor(datos$union_start_type4, levels=c(1,2,7,8,9),
                                    labels=c("cohabitation","marriage","refused","don't remember","unknown"))
  datos$union_start_cmc4 <- NA
  datos$union_start_cmc_I4 <- NA
  datos$marriage_start_cmc4 <- NA
  datos$marriage_start_cmc_I4 <- NA
  datos$union_end_cmc4 <- NA
  datos$union_end_cmc_I4 <- NA
  datos$union_end_motive4 <- NA
  datos$union_end_motive4 <- factor(datos$union_end_motive4, levels=c(0,3,4,5,7,8,9),
                                    labels=c("in union","widowhood","separation","separation","refused","don't remember","unknown"))

  # put currUnion in its place in the union history (but if current union is the fifth or more, it will be misplaced, but we have no choice)
  maxU <- max (datos$nUnion_computed, na.rm=TRUE)
  for (i in (1:maxU)) {
    union_start_cmc <- paste0("union_start_cmc", i)
    union_start_cmc_I <- paste0("union_start_cmc_I", i)
    union_start_type <- paste0("union_start_type", i)
    marriage_start_cmc <- paste0("marriage_start_cmc", i)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", i)
    union_end_cmc <- paste0("union_end_cmc", i)
    union_end_cmc_I <- paste0("union_end_cmc_I", i)
    union_end_motive <- paste0("union_end_motive", i)
    # if union_start_cmc is NA at union order i, and nUnion == i, we recopy data from current union / marriage
    idx <- ((datos$nUnion_computed == i))&(is.na(datos [[union_start_cmc]]))
    datos [[union_start_type]][idx] <- datos$currUnion_start_type[idx]
    datos [[union_start_cmc]][idx] <- datos$currUnion_start_cmc[idx]
    datos [[union_start_cmc_I]][idx] <- datos$currUnion_start_cmc_I[idx]
    datos [[marriage_start_cmc]][idx] <- datos$currMarriage_start_cmc[idx]
    datos [[marriage_start_cmc_I]][idx] <- datos$currMarriage_start_cmc_I[idx]
    datos [[union_end_cmc]][idx] <- datos$currUnion_end_cmc[idx]
    datos [[union_end_cmc_I]][idx] <- datos$currUnion_end_cmc_I[idx]
    datos [[union_end_motive]][idx] <- "in union"
  }
  
  datos$currUnion_start_type <- NULL
  datos$currUnion_start_cmc <- NULL
  datos$currUnion_start_cmc_I <- NULL
  datos$currMarriage_start_cmc <- NULL
  datos$currMarriage_start_cmc_I <- NULL
  datos$currUnion_end_cmc <- NULL
  datos$currUnion_end_cmc_I <- NULL
  datos$currUnion_end_motive <- NULL
  datos$record_type <- NULL
  datos$raw <- NULL
  
  ##### Live births histories #####
  births <- data.frame (CaseID=data_interval$CaseID)
  births$nLiveBirths <- as.integer(substring(data_interval$raw,15,15)) # OK
  births$dob_cmc <- adjust_cmc (as.integer(substring(data_interval$raw,20,23))) # OK
  births$dob_cmc_I <- imputed_date (as.integer(substring(data_interval$raw,20,23))) # OK
  births$babysex1 <- as.integer(substring(data_interval$raw,35,35)) # OK
  births$babysex2 <- as.integer(substring(data_interval$raw,70,70)) # OK
  births$babysex3 <- as.integer(substring(data_interval$raw,105,105)) # OK
  births$dod_cmc1 <- adjust_cmc (as.integer(substring(data_interval$raw,63,66))) # OK
  births$dod_cmc_I1 <- imputed_date (as.integer(substring(data_interval$raw,63,66))) # OK
  births$dod_cmc2 <- adjust_cmc (as.integer(substring(data_interval$raw,98,101))) # OK
  births$dod_cmc_I2 <- imputed_date (as.integer(substring(data_interval$raw,98,101))) # OK
  births$dod_cmc3 <- adjust_cmc (as.integer(substring(data_interval$raw,133,136))) # OK
  births$dod_cmc_I3 <- imputed_date (as.integer(substring(data_interval$raw,133,136))) # OK
  
  births <- subset(births, nLiveBirths > 0) # only live births
  
  births <- live_birth_history2(births)
  
  ##### final women dataframe with live births #####
  datos <- datos %>%
    left_join(births, by = "CaseID")
  
  datos$nBioKids [is.na(datos$nBioKids)] <- 0
  
  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_1982 <- getDatos_1982()
NSFG_ENADID_1982 <- cleanENADID(NSFG_ENADID_1982)

pathNSFG_ENADID_1982 <- path.expand(paste0(dataPath, "NSFG_ENADID_1982.Rdat"))
save(NSFG_ENADID_1982, file = pathNSFG_ENADID_1982)

readNoSep <- function (pathFile, line_length) {
  file_size <- file.info(pathFile)$size
  
  # Calculate how many "lines" are in the file
  num_lines <- file_size / line_length
  
  # Open a connection
  con <- file(pathFile, "rb")
  
  # Read the file in chunks of your known length
  # This creates a character vector where each element is one "line"
  raw <- readChar(con, nchars = rep(line_length, num_lines))
  
  close(con)
  
  return (raw)
  
}

#### 1988 ####
# Union history:
# info on first union only (complete info only on the first and the last cohabitation)
# no transition sep1 -> union2
# info on cohabitation before marriage for the first union only
getDatos_1988 <- function () {
  path_1988 <- paste0(nsfgPath, "1988/")
  
  wd <- getwd()
  setwd(path_1988)
  raw <- readNoSep(paste0(path_1988,"1988FemRespData.dat"), line_length=3553)
  raw_preg <- readNoSep(paste0(path_1988,"1988PregData.dat"), line_length=3553)
  setwd(wd)

  adjust_cmc <- function (cmc) {
    # unknown month is coded as in 90000-99000, we re-code with month number at random
    cmc <- ifelse((cmc >= 90000) & (cmc <= 99000), (cmc - 90000) * 12 + imputed_month (length(cmc)), cmc)
    cmc <- ifelse(cmc==0,NA,cmc)
    cmc <- ifelse(cmc %in% c(9797,9898,9999),9999,cmc)
    return (cmc)
  }
  imputed_date <- function (cmc) {
    imp = cmc
    imp <- 0
    imp <- ifelse(is.na(cmc)|(cmc==0), NA, imp)
    # unknown month is coded as in 90000-99000
    imp <- ifelse((cmc >= 90000) & (cmc <= 99000), 1, imp)
    imp <- ifelse(cmc %in% c(9797,9898,9999),10,imp)
    return (imp)
  }
  
  n <- length(raw)
  datos <- data.frame(country=rep("USA",n), survey="NSFG1988")
  datos$CaseID <- as.integer(substring(raw,1,5)) #OK
  datos$surveyDate_cmc <- adjust_cmc (as.integer(substring(raw,12,16))) #OK
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- adjust_cmc (as.integer(substring(raw,26,30))) #OK
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12)
  datos$union_status <- as.integer(substring(raw,24,24)) #OK
  datos$union_status <- factor(datos$union_status, levels=c(1,2,3,4,5,6,7,9),
                               labels=c("married","cohabiting","widowed","divorced","separated","single","refused","unknown"))
  datos$yBirth <- 1900 + floor ((datos$indiv_dob_cmc-1) / 12)
  datos$indiv_weight <- as.integer(substring(raw,2568,2574)) # OK
  datos$pregnant <- as.integer(substring(raw,256,256)) # OK
  datos$pregnant <- factor (datos$pregnant, levels=c(1,2),labels=c("yes","no"))
  datos$want_another <- NA
  datos$want_another <- factor(datos$want_another, levels=c(1,2,3,8),labels=c("yes","no","disagree","unknown"))
  datos$prob_want_another <- NA
  datos$age_first_sex <- adjust_cmc (as.integer(substring(raw,338,342))) # OK
  datos <- datos %>%
    dplyr::mutate(age_first_sex = case_when(
      age_first_sex == 0 ~ NA_integer_,
      ((age_first_sex > 500) & (age_first_sex < 90000)) ~ floor ((age_first_sex - indiv_dob_cmc) / 12),
      ((age_first_sex > 90000) & (age_first_sex < 99000)) ~ floor ((adjust_cmc(age_first_sex) - indiv_dob_cmc) / 12),
      .default = NA_integer_
    ))
  datos$ever_contraception <- NA # OK
  
  ##### Union histories #####
  datos_UH <- data.frame(CaseID=datos$CaseID)
  datos_UH$currUnion_status <- as.integer(substring(raw,1516,1516)) # OK
  datos_UH$currUnion_status <- factor(datos_UH$currUnion_status, levels=c(1,2,3,4,5),
                               labels=c("married","widowed/separated/divorced","cohabiting","single","single"))
  datos_UH$nUnion <- NULL
  datos_UH$nMarriage <- as.integer(substring(raw,1517,1518)) # OK
  datos_UH$nMarriage <- ifelse(datos_UH$nMarriage > 96,NA,datos_UH$nMarriage)
  datos_UH$nMarriage [is.na(datos_UH$nMarriage)] <- 0
  # if current "marriage" is cohabitation, we check the number of previous marriages only
  datos_UH$nMarriage <- ifelse(datos_UH$currUnion_status=="cohabiting",as.integer(substring(raw,1519,1519)),datos_UH$nMarriage) # OK
  datos_UH$nMarriage <- ifelse(datos_UH$nMarriage > 6,0,datos_UH$nMarriage)
  
  # current marriage
  datos_UH$currUnion_start_type <- NA
  datos_UH$currUnion_start_cmc <- adjust_cmc (as.integer(substring(raw,1521,1525))) # OK
  datos_UH$currUnion_start_cmc_I <- imputed_date (as.integer(substring(raw,1521,1525))) # OK
  datos_UH$currMarriage_start_cmc <- datos_UH$currUnion_start_cmc
  datos_UH$currMarriage_start_cmc_I <- datos_UH$currUnion_start_cmc_I
  datos_UH$currUnion_start_type <- ifelse (!is.na(datos_UH$currMarriage_start_cmc),1,NA) # marriage
  datos_UH$currUnion_startLiving_cmc <- adjust_cmc (as.integer(substring(raw,1529,1533))) # OK
  datos_UH$currUnion_startLiving_cmc_I <- imputed_date (as.integer(substring(raw,1529,1533))) # OK
  datos_UH$currUnion_start_type <- ifelse (!is.na(datos_UH$currUnion_startLiving_cmc),3,datos_UH$currUnion_start_type) # cohabitation before marriage
  datos_UH$currUnion_start_cmc <- ifelse(!is.na(datos_UH$currUnion_startLiving_cmc),
                                         datos_UH$currUnion_startLiving_cmc,
                                         datos_UH$currUnion_start_cmc)
  datos_UH$currUnion_start_cmc_I <- ifelse(!is.na(datos_UH$currUnion_startLiving_cmc),
                                         datos_UH$currUnion_startLiving_cmc_I,
                                         datos_UH$currUnion_start_cmc_I)
  datos_UH$currUnion_start_type <- factor (datos_UH$currUnion_start_type, levels=c(1,2,3),
                                        labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$currUnion_startLiving_cmc <- NULL
  datos_UH$currUnion_startLiving_cmc_I <- NULL
  
  # first marriage
  datos_UH$union_start_type1 <- NA
  datos_UH$union_start_cmc1 <- adjust_cmc (as.integer(substring(raw,1538,1542))) # OK
  datos_UH$union_start_cmc_I1 <- imputed_date (as.integer(substring(raw,1538,1542))) # OK
  datos_UH$marriage_start_cmc1 <- datos_UH$union_start_cmc1
  datos_UH$marriage_start_cmc_I1 <- datos_UH$union_start_cmc_I1
  datos_UH$union_start_type1 <- ifelse (!is.na(datos_UH$union_start_cmc1),1,datos_UH$union_start_type1) # marriage
  datos_UH$union_startLiving_cmc1 <- adjust_cmc (as.integer(substring(raw,1546,1550))) # OK
  datos_UH$union_startLiving_cmc_I1 <- imputed_date (as.integer(substring(raw,1546,1550))) # OK
  datos_UH$union_start_type1 <- ifelse (!is.na(datos_UH$union_startLiving_cmc1),3,datos_UH$union_start_type1) # cohabitation before marriage
  datos_UH$union_start_cmc1 <- ifelse(!is.na(datos_UH$union_startLiving_cmc1),
                                      datos_UH$union_startLiving_cmc1,
                                      datos_UH$union_start_cmc1)
  datos_UH$union_start_cmc_I1 <- ifelse(!is.na(datos_UH$union_startLiving_cmc1),
                                      datos_UH$union_startLiving_cmc_I1,
                                      datos_UH$union_start_cmc_I1)
  datos_UH$union_start_type1 <- factor (datos_UH$union_start_type1, levels=c(1,2,3),
                                     labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$union_end_motive1 <- as.integer(substring(raw,1554,1554)) # OK
  datos_UH$union_deathHusband_cmc1 <- adjust_cmc (as.integer(substring(raw,1555,1559))) # OK
  datos_UH$union_stopLiving_cmc1 <- adjust_cmc (as.integer(substring(raw,1560,1564))) # OK
  datos_UH$union_stopLiving2_cmc1 <- adjust_cmc (as.integer(substring(raw,1570,1574))) # OK
  datos_UH$union_deathHusband_cmc_I1 <- imputed_date (as.integer(substring(raw,1555,1559))) # OK
  datos_UH$union_stopLiving_cmc_I1 <- imputed_date (as.integer(substring(raw,1560,1564))) # OK
  datos_UH$union_stopLiving2_cmc_I1 <- imputed_date (as.integer(substring(raw,1570,1574))) # OK
  datos_UH$union_end_cmc1 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc1), datos_UH$union_stopLiving_cmc1, NA)
  datos_UH$union_end_cmc_I1 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc1), datos_UH$union_stopLiving_cmc_I1, NA)
  datos_UH$union_end_cmc1 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc1), datos_UH$union_stopLiving2_cmc1, datos_UH$union_end_cmc1)
  datos_UH$union_end_cmc_I1 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc1), datos_UH$union_stopLiving2_cmc_I1, datos_UH$union_end_cmc_I1)
  datos_UH$union_end_cmc1 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc1), datos_UH$union_deathHusband_cmc1, datos_UH$union_end_cmc1)
  datos_UH$union_end_cmc_I1 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc1), datos_UH$union_deathHusband_cmc_I1, datos_UH$union_end_cmc_I1)
  datos_UH$union_end_motive1 <- ifelse(!is.na(datos_UH$union_start_cmc1)&is.na(datos_UH$union_end_cmc1), 0, datos_UH$union_end_motive1)

  datos_UH$union_startLiving_cmc1 <- NULL
  datos_UH$union_deathHusband_cmc1 <- NULL
  datos_UH$union_stopLiving_cmc1 <- NULL
  datos_UH$union_stopLiving2_cmc1 <- NULL
  
  datos_UH$union_startLiving_cmc_I1 <- NULL
  datos_UH$union_deathHusband_cmc_I1 <- NULL
  datos_UH$union_stopLiving_cmc_I1 <- NULL
  datos_UH$union_stopLiving2_cmc_I1 <- NULL
  
  # second marriage (no info on cohabitation before marriage)
  datos_UH$union_start_type2 <- NA
  datos_UH$union_start_cmc2 <- adjust_cmc (as.integer(substring(raw,1575,1579))) # OK
  datos_UH$union_start_cmc_I2 <- imputed_date (as.integer(substring(raw,1575,1579))) # OK
  datos_UH$marriage_start_cmc2 <- datos_UH$union_start_cmc2
  datos_UH$marriage_start_cmc_I2 <- datos_UH$union_start_cmc_I2
  datos_UH$union_start_type2 <- ifelse (!is.na(datos_UH$union_start_cmc2),1,datos_UH$union_start_type2) # marriage
  datos_UH$union_startLiving_cmc2 <- NA # that information is missing, therefore we cannot identify cohabitation before second marriage
  datos_UH$union_start_type2 <- ifelse (!is.na(datos_UH$union_startLiving_cmc2),3,datos_UH$union_start_type2) # cohabitation before marriage
  datos_UH$union_start_cmc2 <- ifelse(!is.na(datos_UH$union_startLiving_cmc2),datos_UH$union_startLiving_cmc2,datos_UH$union_start_cmc2)
  datos_UH$union_start_cmc_I2 <- ifelse(!is.na(datos_UH$union_startLiving_cmc2),datos_UH$union_startLiving_cmc_I2,datos_UH$union_start_cmc_I2)
  datos_UH$union_start_type2 <- factor (datos_UH$union_start_type2, levels=c(1,2,3),
                                     labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$union_end_motive2 <- as.integer(substring(raw,1582,1582)) # OK
  datos_UH$union_deathHusband_cmc2 <- adjust_cmc (as.integer(substring(raw,1583,1587))) # OK
  datos_UH$union_stopLiving_cmc2 <- adjust_cmc (as.integer(substring(raw,1588,1592))) # OK
  datos_UH$union_stopLiving2_cmc2 <- adjust_cmc (as.integer(substring(raw,1598,1602))) # OK
  datos_UH$union_deathHusband_cmc_I2 <- imputed_date (as.integer(substring(raw,1583,1587))) # OK
  datos_UH$union_stopLiving_cmc_I2 <- imputed_date (as.integer(substring(raw,1588,1592))) # OK
  datos_UH$union_stopLiving2_cmc_I2 <- imputed_date (as.integer(substring(raw,1598,1602))) # OK
  datos_UH$union_end_cmc2 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc2), datos_UH$union_stopLiving_cmc2, NA)
  datos_UH$union_end_cmc_I2 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc2), datos_UH$union_stopLiving_cmc_I2, NA)
  datos_UH$union_end_cmc2 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc2), datos_UH$union_stopLiving2_cmc2, datos_UH$union_end_cmc2)
  datos_UH$union_end_cmc_I2 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc2), datos_UH$union_stopLiving2_cmc_I2, datos_UH$union_end_cmc_I2)
  datos_UH$union_end_cmc2 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc2), datos_UH$union_deathHusband_cmc2, datos_UH$union_end_cmc2)
  datos_UH$union_end_cmc_I2 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc2), datos_UH$union_deathHusband_cmc_I2, datos_UH$union_end_cmc_I2)
  datos_UH$union_end_motive2 <- ifelse(!is.na(datos_UH$union_start_cmc2)&is.na(datos_UH$union_end_cmc2), 0, datos_UH$union_end_motive2)

  datos_UH$union_startLiving_cmc2 <- NULL
  datos_UH$union_deathHusband_cmc2 <- NULL
  datos_UH$union_stopLiving_cmc2 <- NULL
  datos_UH$union_stopLiving2_cmc2 <- NULL

  datos_UH$union_startLiving_cmc_I2 <- NULL
  datos_UH$union_deathHusband_cmc_I2 <- NULL
  datos_UH$union_stopLiving_cmc_I2 <- NULL
  datos_UH$union_stopLiving2_cmc_I2 <- NULL
  
  # previous marriage: we consider it at the third one, to simplify, but it could be fourth in 10 cases
  datos_UH$union_start_type3 <- NA
  datos_UH$union_start_cmc3 <- adjust_cmc (as.integer(substring(raw,1603,1607))) # OK
  datos_UH$union_start_cmc_I3 <- imputed_date (as.integer(substring(raw,1603,1607))) # OK
  datos_UH$marriage_start_cmc3 <- datos_UH$union_start_cmc3
  datos_UH$marriage_start_cmc_I3 <- datos_UH$union_start_cmc_I3
  datos_UH$union_start_type3 <- ifelse (!is.na(datos_UH$union_start_cmc3),1,datos_UH$union_start_type3) # marriage
  datos_UH$union_startLiving_cmc3 <- NA # that information is missing, therefore we cannot identify cohabitation before previous marriage
  datos_UH$union_start_type3 <- ifelse (!is.na(datos_UH$union_startLiving_cmc3),3,datos_UH$union_start_type3) # cohabitation before marriage
  datos_UH$union_start_cmc3 <- ifelse(!is.na(datos_UH$union_startLiving_cmc3),datos_UH$union_startLiving_cmc3,datos_UH$union_start_cmc3)
  datos_UH$union_start_cmc_I3 <- ifelse(!is.na(datos_UH$union_startLiving_cmc3),datos_UH$union_startLiving_cmc_I3,datos_UH$union_start_cmc_I3)
  datos_UH$union_start_type3 <- factor (datos_UH$union_start_type3, levels=c(1,2,3),
                                     labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$union_end_motive3 <- as.integer(substring(raw,1610,1610)) # OK
  datos_UH$union_deathHusband_cmc3 <- adjust_cmc (as.integer(substring(raw,1611,1615))) # OK
  datos_UH$union_stopLiving_cmc3 <- adjust_cmc (as.integer(substring(raw,1616,1620))) # OK
  datos_UH$union_stopLiving2_cmc3 <- adjust_cmc (as.integer(substring(raw,1626,1630))) # OK
  datos_UH$union_deathHusband_cmc_I3 <- imputed_date (as.integer(substring(raw,1611,1615))) # OK
  datos_UH$union_stopLiving_cmc_I3 <- imputed_date (as.integer(substring(raw,1616,1620))) # OK
  datos_UH$union_stopLiving2_cmc_I3 <- imputed_date (as.integer(substring(raw,1626,1630))) # OK
  datos_UH$union_end_cmc3 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc3), datos_UH$union_stopLiving_cmc3, NA)
  datos_UH$union_end_cmc_I3 <- ifelse(!is.na(datos_UH$union_stopLiving_cmc3), datos_UH$union_stopLiving_cmc_I3, NA)
  datos_UH$union_end_cmc3 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc3), datos_UH$union_stopLiving2_cmc3, datos_UH$union_end_cmc3)
  datos_UH$union_end_cmc_I3 <- ifelse(!is.na(datos_UH$union_stopLiving2_cmc3), datos_UH$union_stopLiving2_cmc_I3, datos_UH$union_end_cmc_I3)
  datos_UH$union_end_cmc3 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc3), datos_UH$union_deathHusband_cmc3, datos_UH$union_end_cmc3)
  datos_UH$union_end_cmc_I3 <- ifelse(!is.na(datos_UH$union_deathHusband_cmc3), datos_UH$union_deathHusband_cmc_I3, datos_UH$union_end_cmc_I3)
  datos_UH$union_end_motive3 <- ifelse(!is.na(datos_UH$union_start_cmc3)&is.na(datos_UH$union_end_cmc3), 0, datos_UH$union_end_motive3)

  datos_UH$union_startLiving_cmc3 <- NULL
  datos_UH$union_deathHusband_cmc3 <- NULL
  datos_UH$union_stopLiving_cmc3 <- NULL
  datos_UH$union_stopLiving2_cmc3 <- NULL
  
  datos_UH$union_startLiving_cmc_I3 <- NULL
  datos_UH$union_deathHusband_cmc_I3 <- NULL
  datos_UH$union_stopLiving_cmc_I3 <- NULL
  datos_UH$union_stopLiving2_cmc_I3 <- NULL
  
  # current marriage placed in the marriage list
  datos_UH$union_start_type4 <- NA
  datos_UH$union_start_type4 <- factor (datos_UH$union_start_type4, levels=c(1,2,3),
                                     labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$union_start_cmc4 <- NA
  datos_UH$marriage_start_cmc4 <- NA
  datos_UH$union_end_motive4 <- NA
  datos_UH$union_end_cmc4 <- NA
  datos_UH$union_start_cmc_I4 <- NA
  datos_UH$marriage_start_cmc_I4 <- NA
  datos_UH$union_end_motive_I4 <- NA
  datos_UH$union_end_cmc_I4 <- NA
  
  dsum <- data.frame(
    curr=!is.na(datos_UH$currUnion_start_cmc),
    u1=!is.na(datos_UH$union_start_cmc1),
    u2=!is.na(datos_UH$union_start_cmc2),
    u3=!is.na(datos_UH$union_start_cmc3)
  )
  
  datos_UH$nMarriage_computed <- rowSums (dsum)
  datos_UH <- relocate (datos_UH, nMarriage_computed, .after= nMarriage)
  
  for (m in (1:4)) {
    union_start_cmc <- paste0("union_start_cmc", m)
    union_start_cmc_I <- paste0("union_start_cmc_I", m)
    union_start_type <- paste0("union_start_type", m)
    marriage_start_cmc <- paste0("marriage_start_cmc", m)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", m)
    union_end_cmc <- paste0("union_end_cmc", m)
    union_end_cmc_I <- paste0("union_end_cmc_I", m)
    union_end_motive <- paste0("union_end_motive", m)
    idx <- ((m==datos_UH$nMarriage_computed) & (is.na(datos_UH [[union_start_cmc]])))
    datos_UH [[union_start_type]][idx] <- datos_UH$currUnion_start_type[idx]
    datos_UH [[union_start_cmc]][idx] <- datos_UH$currUnion_start_cmc[idx]
    datos_UH [[union_start_cmc_I]][idx] <- datos_UH$currUnion_start_cmc_I[idx]
    datos_UH [[marriage_start_cmc]][idx] <- datos_UH$currMarriage_start_cmc[idx]
    datos_UH [[marriage_start_cmc_I]][idx] <- datos_UH$currMarriage_start_cmc_I[idx]
    datos_UH [[union_end_motive]][idx] <- 0 # in union
    datos_UH [[union_end_cmc]][idx] <- NA
    datos_UH [[union_end_cmc_I]][idx] <- NA
  }
  datos_UH$currUnion_start_type <- NULL
  datos_UH$currUnion_start_cmc <- NULL
  datos_UH$currUnion_start_cmc_I <- NULL
  datos_UH$currMarriage_start_cmc <- NULL
  datos_UH$currMarriage_start_cmc_I <- NULL
  datos_UH$currUnion_end_cmc <- NULL
  datos_UH$currUnion_end_cmc_I <- NULL
  datos_UH$currUnion_stopLiving_cmc <- NULL
  datos_UH$currUnion_end_motive <- NULL
  
  # current cohabitation......
  datos_UH$union_start_type5 <- NA
  datos_UH$union_start_cmc5 <- adjust_cmc (as.integer(substring(raw,1632,1636))) # OK
  datos_UH$union_start_cmc_I5 <- imputed_date (as.integer(substring(raw,1632,1636))) # OK
  datos_UH$union_start_type5 <- ifelse(!is.na(datos_UH$union_start_cmc5),2,datos_UH$union_start_type5) # cohabitation
  datos_UH$union_start_type5 <- factor (datos_UH$union_start_type5, levels=c(1,2,3),
                                        labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$marriage_start_cmc5 <- NA
  datos_UH$marriage_start_cmc_I5 <- NA
  datos_UH$union_end_motive5 <- NA
  datos_UH$union_end_cmc5 <- NA
  datos_UH$union_end_cmc_I5 <- NA
  
  # first cohabitation
  datos_UH$union_start_type6 <- NA
  datos_UH$union_start_cmc6 <- adjust_cmc (as.integer(substring(raw,1641,1645))) # OK
  datos_UH$union_start_cmc_I6 <- imputed_date (as.integer(substring(raw,1641,1645))) # OK
  datos_UH$union_start_type6 <- ifelse(!is.na(datos_UH$union_start_cmc6),2,datos_UH$union_start_type6) # cohabitation
  datos_UH$union_start_type6 <- factor (datos_UH$union_start_type6, levels=c(1,2,3),
                                        labels=c("marriage","cohabitation","cohabitation before marriage"))
  datos_UH$marriage_start_cmc6 <- NA
  datos_UH$marriage_start_cmc_I6 <- NA
  datos_UH$union_end_motive6 <- NA
  datos_UH$union_end_motive6 <- ifelse(!is.na(datos_UH$union_start_cmc6),3,datos_UH$union_end_motive6) # separation
  # length of cohabitation instead of date of end
  datos_UH$lengthCohab <- as.integer(substring(raw,1646,1648)) # OK
  datos_UH$union_end_cmc6 <- ifelse ((!is.na(datos_UH$lengthCohab))&(datos_UH$lengthCohab<997),datos_UH$union_start_cmc6 + datos_UH$lengthCohab,9999)
  datos_UH$union_end_cmc6 <- ifelse (is.na(datos_UH$lengthCohab),NA,datos_UH$union_end_cmc6)
  datos_UH$union_end_cmc_I6 <- ifelse(is.na(datos_UH$lengthCohab), NA,
                                      ifelse(datos_UH$lengthCohab >= 997, 10, 0))
  datos_UH$union_end_motive6 <- ifelse((!is.na(datos_UH$union_start_cmc6))&(!is.na(datos_UH$union_end_cmc6)),3,datos_UH$union_end_motive6) # separation
  datos_UH$lengthCohab <- NULL
  
  datos_UH2 <- order_union_history (datos_UH)

  maxU <- max(datos_UH2$total_unions, na.rm=TRUE)
  for (u in (1:maxU)) {
    union_start_cmc <- paste0("union_start_cmc", u)
    union_end_cmc <- paste0("union_end_cmc", u)
    union_end_motive <- paste0("union_end_motive", u)
    datos_UH2 [[union_end_motive]] <- ifelse((!is.na(datos_UH2 [[union_start_cmc]])) & (is.na(datos_UH2 [[union_end_cmc]])), 0, datos_UH2 [[union_end_motive]])
    datos_UH2 [[union_end_motive]] <- factor (datos_UH2 [[union_end_motive]], levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
  }

  datos <- datos %>%
    left_join(datos_UH2, by = "CaseID") %>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0)) %>%
    rename(nUnion = total_unions)
  
  ##### Live births histories #####
  births <- data.frame (CaseID=as.integer(substring(raw_preg,1,5)))
  births$nLiveBirths <- as.integer(substring(raw_preg,15,15)) # OK
  births$dob_cmc <- adjust_cmc (as.integer(substring(raw_preg,21,25))) # OK
  births$dob_cmc_I <- imputed_date (as.integer(substring(raw_preg,21,25))) # OK
  births$babysex1 <- as.integer(substring(raw_preg,80,80)) # OK
  births$dod_cmc1 <- adjust_cmc (as.integer(substring(raw_preg,110,114))) # OK
  births$dod_cmc_I1 <- imputed_date (as.integer(substring(raw_preg,110,114))) # OK
  births$babysex2 <- as.integer(substring(raw_preg,121,121)) # OK
  births$dod_cmc2 <- adjust_cmc (as.integer(substring(raw_preg,151,155))) # OK
  births$dod_cmc_I2 <- imputed_date (as.integer(substring(raw_preg,151,155))) # OK
  births$babysex3 <- as.integer(substring(raw_preg,162,162)) # OK
  births$dod_cmc3 <- adjust_cmc (as.integer(substring(raw_preg,192,196))) # OK
  births$dod_cmc_I3 <- imputed_date (as.integer(substring(raw_preg,192,196))) # OK
  
  births <- subset(births, nLiveBirths > 0) # only live births
  
  births <- live_birth_history2(births)
  
  ##### final women dataframe with live births #####
  datos <- datos %>%
    left_join(births, by = "CaseID")
  
  datos$nBioKids [is.na(datos$nBioKids)] <- 0
  
  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_1988 <- getDatos_1988()
NSFG_ENADID_1988 <- cleanENADID(NSFG_ENADID_1988)
NSFG_ENADID_1988 <-  reorder_birthHistory(NSFG_ENADID_1988)

pathNSFG_ENADID_1988 <- path.expand(paste0(dataPath, "NSFG_ENADID_1988.Rdat"))
save(NSFG_ENADID_1988, file = pathNSFG_ENADID_1988)

#### 1995 ####
# Union history:
# complete marriage and cohabitation histories (but no information for cohabitations on death of partner, only on separation)
# info on cohabitation before marriage
getDatos_1995 <- function () {
  path_1995 <- paste0(nsfgPath, "1995/")
  
  raw <- readLines(paste0(path_1995,"1995FemRespData.dat"))
  raw_preg <- readLines(paste0(path_1995,"1995PregData.dat"))
  
  adjust_cmc <- function (cmc) {
    cmc <- ifelse(cmc==0,NA,cmc)
    cmc <- ifelse(cmc %in% c(9997,9998,9999),9999,cmc)
    cmc <- ifelse((cmc > 300) & (cmc < 1200) & (cmc != 9999), cmc, NA)
    return (cmc)
  }
  
  imputed_date <- function (cmc) {
    imp <- ifelse(cmc==0,NA,0)
    imp <- ifelse((cmc > 300) & (cmc < 1200), 0,
                  ifelse(cmc %in% c(9997,9998,9999), 10, NA))
    return (imp)
  }
  
  n <- length(raw)
  datos <- data.frame(country=rep("USA",n), survey="NSFG1995")
  datos$CaseID <- as.integer(substring(raw,1,8)) #OK
  datos$surveyDate_cmc <- adjust_cmc (as.integer(substring(raw,12360,12363))) #OK
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- adjust_cmc (as.integer(substring(raw,13,16))) #OK
  datos$indiv_age_survey <- floor((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12)
  datos$union_status <- as.integer(substring(raw,17,17)) #OK
  datos$union_status <- factor(datos$union_status, levels=c(1,2,3,4,5,7,8,9),
                               labels=c("married","widowed","divorced","separated","single","NA","refused","unknown"))
  datos$yBirth <- 1900 + floor ((datos$indiv_dob_cmc-1) / 12)
  datos$indiv_weight <- as.integer(substring(raw,12350,12359)) #OK
  datos$pregnant <- as.integer(substring(raw,3815,3815)) #OK
  datos$pregnant <- factor (datos$pregnant, levels=c(1,2,7,8,9),labels=c("yes","no","NA","refused","unknown"))
  datos$want_another <- as.integer(substring(raw,9360,9360)) #OK (look at the question)
  datos$want_another <- factor(datos$want_another, levels=c(1,2,3,7,8,9),labels=c("yes","no","up to God", "NA","refused","unknown"))
  datos$pregnant_want_another <- as.integer(substring(raw,9360,9360)) #OK (look 9380 for childless?)
  datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels=c(1,2,3,7,8,9),labels=c("yes","no","up to god","NA","refused","unknown"))
  datos$prob_want_another <- NA
  datos$age_first_sex <- as.integer(substring(raw,5649,5650)) #OK
  datos <- datos %>%
    dplyr::mutate(age_first_sex = case_when(
      ((age_first_sex > 7) & (age_first_sex < 90)) ~ age_first_sex,
       .default = NA_integer_
    ))
  datos$ever_contraception <- as.integer(substring(raw,6833,6833)) #OK
  datos$ever_contraception <- factor(datos$ever_contraception, levels=c(1,2,7,8,9),labels=c("yes","no","NA","refused","unknown"))
  
  ##### Union histories #####
  datos_UH <- data.frame(CaseID=datos$CaseID)
  
  marriageHistory <- function (raw, datos_UH) {
    datos_UH$nMarriage <- as.integer(substring(raw,4637,4638)) #OK
    datos_UH$nMarriage <- ifelse(datos_UH$nMarriage > 96,NA,datos_UH$nMarriage)
    datos_UH$nMarriage [is.na(datos_UH$nMarriage)] <- 0
    
    # current marriage place holder
    datos_UH$currUnion_start_type <- NA
    datos_UH$currUnion_start_cmc <- NA
    datos_UH$currUnion_start_cmc_I <- NA
    datos_UH$currMarriage_start_cmc <- NA
    datos_UH$currMarriage_start_cmc_I <- NA
    datos_UH$currUnion_end_motive <- NA
    datos_UH$currUnion_deathHusband_cmc <- NA
    datos_UH$currUnion_deathHusband_cmc_I <- NA
    datos_UH$currUnion_stopLiving_cmc <- NA
    datos_UH$currUnion_stopLiving_cmc_I <- NA
    datos_UH$currUnion_end_cmc <- NA
    datos_UH$currUnion_end_cmc_I <- NA
    
    # first four marriages
    dpos <- 0
    for (u in (1:4)) {
      union_start_cmc <- paste0("union_start_cmc", u)
      union_start_cmc_I <- paste0("union_start_cmc_I", u)
      union_start_type <- paste0("union_start_type", u)
      marriage_start_cmc <- paste0("marriage_start_cmc", u)
      marriage_start_cmc_I <- paste0("marriage_start_cmc_I", u)
      union_startLiving_cmc <- paste0("union_startLiving_cmc", u)
      union_startLiving_cmc_I <- paste0("union_startLiving_cmc_I", u)
      union_end_motive <- paste0("union_end_motive", u)
      union_deathHusband_cmc <- paste0("union_deathHusband_cmc", u)
      union_deathHusband_cmc_I <- paste0("union_deathHusband_cmc_I", u)
      union_stopLiving_cmc <- paste0("union_stopLiving_cmc", u)
      union_stopLiving_cmc_I <- paste0("union_stopLiving_cmc_I", u)
      union_end_cmc <- paste0("union_end_cmc", u)
      union_end_cmc_I <- paste0("union_end_cmc_I", u)
      
      datos_UH [[union_start_type]] <- NA
      datos_UH [[union_start_cmc]] <- adjust_cmc (as.integer(substring(raw,4659+dpos,4662+dpos))) #OK   
      datos_UH [[union_start_cmc_I]] <- imputed_date (as.integer(substring(raw,4659+dpos,4662+dpos))) #OK
      datos_UH [[marriage_start_cmc]] <- datos_UH [[union_start_cmc]]
      datos_UH [[marriage_start_cmc_I]] <- datos_UH [[union_start_cmc_I]]
      
      datos_UH [[union_start_type]] <- ifelse (!is.na(datos_UH [[union_start_cmc]]),1,datos_UH [[union_start_type]]) # marriage
      datos_UH [[union_startLiving_cmc]] <- adjust_cmc (as.integer(substring(raw,4664+dpos,4667+dpos))) #OK
      datos_UH [[union_startLiving_cmc_I]] <- imputed_date (as.integer(substring(raw,4664+dpos,4667+dpos))) #OK
      datos_UH [[union_start_type]] <- ifelse (!is.na(datos_UH [[union_startLiving_cmc]]),3,datos_UH [[union_start_type]]) # cohabitation before marriage
      datos_UH [[union_start_cmc]] <- ifelse(!is.na(datos_UH [[union_startLiving_cmc]]),
                                             datos_UH [[union_startLiving_cmc]],
                                             datos_UH [[union_start_cmc]])
      datos_UH [[union_start_cmc_I]] <- ifelse(!is.na(datos_UH [[union_startLiving_cmc]]),
                                               datos_UH [[union_startLiving_cmc_I]],
                                               datos_UH [[union_start_cmc_I]])
      datos_UH [[union_start_type]] <- factor (datos_UH [[union_start_type]], levels=c(1,2,3),
                                               labels=c("marriage","cohabitation","cohabitation before marriage"))
      
      datos_UH [[union_end_motive]] <- as.integer(substring(raw,4713+dpos,4713+dpos)) #OK
      datos_UH [[union_deathHusband_cmc]] <- adjust_cmc (as.integer(substring(raw,4714+dpos,4717+dpos))) #OK
      datos_UH [[union_deathHusband_cmc_I]] <- imputed_date (as.integer(substring(raw,4714+dpos,4717+dpos))) #OK
      datos_UH [[union_stopLiving_cmc]] <- adjust_cmc (as.integer(substring(raw,4722+dpos,4725+dpos))) #OK
      datos_UH [[union_stopLiving_cmc_I]] <- imputed_date (as.integer(substring(raw,4722+dpos,4725+dpos))) #OK
      datos_UH [[union_end_cmc]] <- ifelse(!is.na(datos_UH [[union_stopLiving_cmc]]), datos_UH [[union_stopLiving_cmc]], NA)
      datos_UH [[union_end_cmc_I]] <- ifelse(!is.na(datos_UH [[union_stopLiving_cmc]]), datos_UH [[union_stopLiving_cmc_I]], NA)
      datos_UH [[union_end_cmc]] <- ifelse(!is.na(datos_UH [[union_deathHusband_cmc]]),
                                           datos_UH [[union_deathHusband_cmc]],
                                           datos_UH [[union_end_cmc]])
      datos_UH [[union_end_cmc_I]] <- ifelse(!is.na(datos_UH [[union_deathHusband_cmc]]),
                                             datos_UH [[union_deathHusband_cmc_I]],
                                             datos_UH [[union_end_cmc_I]])
      datos_UH [[union_end_motive]] <- factor (datos_UH [[union_end_motive]], levels=c(0,1,2,3,7,8,9),
                                               labels=c("in union","widowhood","separation","separation","NA","refused","unknown"))
      
      datos_UH [[union_startLiving_cmc]] <- NULL
      datos_UH [[union_deathHusband_cmc]] <- NULL
      datos_UH [[union_stopLiving_cmc]] <- NULL
      
      datos_UH [[union_startLiving_cmc_I]] <- NULL
      datos_UH [[union_deathHusband_cmc_I]] <- NULL
      datos_UH [[union_stopLiving_cmc_I]] <- NULL
      
      dpos <- dpos + 76
    }

    # current marriage (at most the fifth)
    datos_UH$currUnion_start_type <- NA
    datos_UH$currUnion_start_cmc <- adjust_cmc (as.integer(substring(raw,4985,4988))) #OK
    datos_UH$currUnion_start_cmc_I <- imputed_date (as.integer(substring(raw,4985,4988))) #OK
    datos_UH$currMarriage_start_cmc <- datos_UH$currUnion_start_cmc
    datos_UH$currMarriage_start_cmc_I <- datos_UH$currUnion_start_cmc_I
    datos_UH$currUnion_start_type <- ifelse (!is.na(datos_UH$currUnion_start_cmc),1,datos_UH$currUnion_start_type) # marriage
    datos_UH$currUnion_startLiving_cmc <- adjust_cmc (as.integer(substring(raw,4990,4993))) #OK
    datos_UH$currUnion_startLiving_cmc_I <- imputed_date (as.integer(substring(raw,4990,4993))) #OK
    datos_UH$currUnion_start_type <- ifelse (!is.na(datos_UH$currUnion_startLiving_cmc),3,datos_UH$currUnion_start_type) # cohabitation before marriage
    datos_UH$currUnion_start_cmc <- ifelse(!is.na(datos_UH$currUnion_startLiving_cmc),
                                           datos_UH$currUnion_startLiving_cmc,
                                           datos_UH$currUnion_start_cmc)
    datos_UH$currUnion_start_cmc_I <- ifelse(!is.na(datos_UH$currUnion_startLiving_cmc),
                                           datos_UH$currUnion_startLiving_cmc_I,
                                           datos_UH$currUnion_start_cmc_I)
    datos_UH$currUnion_start_type <- factor (datos_UH$currUnion_start_type, levels=c(1,2,3),
                                             labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$currUnion_end_motive <- NA
    datos_UH$currUnion_deathHusband_cmc <- adjust_cmc (as.integer(substring(raw,5039,5042))) #OK
    datos_UH$currUnion_stopLiving_cmc <- adjust_cmc (as.integer(substring(raw,5043,5046))) #OK
    datos_UH$currUnion_deathHusband_cmc_I <- imputed_date (as.integer(substring(raw,5039,5042))) #OK
    datos_UH$currUnion_stopLiving_cmc_I <- imputed_date (as.integer(substring(raw,5043,5046))) #OK
    datos_UH$currUnion_end_cmc <- ifelse(!is.na(datos_UH$currUnion_stopLiving_cmc), datos_UH$currUnion_stopLiving_cmc, NA)
    datos_UH$currUnion_end_cmc_I <- ifelse(!is.na(datos_UH$currUnion_stopLiving_cmc), datos_UH$currUnion_stopLiving_cmc_I, NA)
    datos_UH$currUnion_end_cmc <- ifelse(!is.na(datos_UH$currUnion_deathHusband_cmc),
                                         datos_UH$currUnion_deathHusband_cmc,
                                         datos_UH$currUnion_end_cmc)
    datos_UH$currUnion_end_cmc_I <- ifelse(!is.na(datos_UH$currUnion_deathHusband_cmc),
                                           datos_UH$currUnion_deathHusband_cmc_I,
                                           datos_UH$currUnion_end_cmc_I)
    datos_UH$currUnion_end_motive <- ifelse(!is.na(datos_UH$currUnion_start_cmc)&(is.na(datos_UH$currUnion_end_cmc)),
                                            0,
                                            datos_UH$currUnion_end_motive) # in union
    datos_UH$currUnion_end_motive <- ifelse(!is.na(datos_UH$currUnion_deathHusband_cmc),
                                            1,
                                            datos_UH$currUnion_end_motive) # widowed
    datos_UH$currUnion_end_motive <- ifelse(!is.na(datos_UH$currUnion_stopLiving_cmc),
                                            2,
                                            datos_UH$currUnion_end_motive) # separated
    datos_UH$currUnion_end_motive <- factor (datos_UH$currUnion_end_motive, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","NA","refused","unknown"))
    datos_UH$currUnion_startLiving_cmc <- NULL
    datos_UH$currUnion_deathHusband_cmc <- NULL
    datos_UH$currUnion_stopLiving_cmc <- NULL
    datos_UH$currUnion_startLiving_cmc_I <- NULL
    datos_UH$currUnion_deathHusband_cmc_I <- NULL
    datos_UH$currUnion_stopLiving_cmc_I <- NULL
    
    dsum <- data.frame(
      curr=!is.na(datos_UH$currUnion_start_cmc),
      u1=!is.na(datos_UH$union_start_cmc1),
      u2=!is.na(datos_UH$union_start_cmc2),
      u3=!is.na(datos_UH$union_start_cmc3),
      u4=!is.na(datos_UH$union_start_cmc4)
    )
    
    # place current marriage in the marriage list
    datos_UH$nMarriage_computed <- rowSums (dsum)
    datos_UH <- relocate (datos_UH, nMarriage_computed, .after= nMarriage)
    
    # possible fifth marriage
    datos_UH$union_start_type5 <- NA
    datos_UH$union_start_type5 <- factor (datos_UH$union_start_type5, levels=c(1,2,3),
                                          labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$union_start_cmc5 <- NA
    datos_UH$union_start_cmc_I5 <- NA
    datos_UH$marriage_start_cmc5 <- NA
    datos_UH$marriage_start_cmc_I5 <- NA
    datos_UH$union_end_cmc5 <- NA
    datos_UH$union_end_cmc_I5 <- NA
    datos_UH$union_end_motive5 <- NA
    datos_UH$union_end_motive5 <- factor (datos_UH$union_end_motive5, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","NA","refused","unknown"))
    
    for (m in (1:5)) {
      union_start_cmc <- paste0("union_start_cmc", m)
      union_start_cmc_I <- paste0("union_start_cmc_I", m)
      union_start_type <- paste0("union_start_type", m)
      marriage_start_cmc <- paste0("marriage_start_cmc", m)
      marriage_start_cmc_I <- paste0("marriage_start_cmc_I", m)
      union_end_cmc <- paste0("union_end_cmc", m)
      union_end_cmc_I <- paste0("union_end_cmc_I", m)
      union_end_motive <- paste0("union_end_motive", m)
      idx <- ((m==datos_UH$nMarriage_computed) & (is.na(datos_UH [[union_start_cmc]])))
      datos_UH [[union_start_type]][idx] <- datos_UH$currUnion_start_type[idx]
      datos_UH [[union_start_cmc]][idx] <- datos_UH$currUnion_start_cmc[idx]
      datos_UH [[union_start_cmc_I]][idx] <- datos_UH$currUnion_start_cmc_I[idx]
      datos_UH [[marriage_start_cmc]][idx] <- datos_UH$currMarriage_start_cmc[idx]
      datos_UH [[marriage_start_cmc_I]][idx] <- datos_UH$currMarriage_start_cmc_I[idx]
      datos_UH [[union_end_motive]][idx] <- datos_UH$currUnion_end_motive[idx]
      datos_UH [[union_end_cmc]][idx] <- datos_UH$currUnion_end_cmc[idx]
      datos_UH [[union_end_cmc_I]][idx] <- datos_UH$currUnion_end_cmc_I[idx]
    }
    
    datos_UH$currUnion_start_type <- NULL
    datos_UH$currUnion_start_cmc <- NULL
    datos_UH$currUnion_start_cmc_I <- NULL
    datos_UH$currMarriage_start_cmc <- NULL
    datos_UH$currMarriage_start_cmc_I <- NULL
    datos_UH$currUnion_end_motive <- NULL
    datos_UH$currUnion_end_cmc <- NULL
    datos_UH$currUnion_end_cmc_I <- NULL

    return (datos_UH)
  }
  
  datos_UH <- marriageHistory (raw, datos_UH)
  
  cohabitationHistory <- function (raw, datos_UH) {
    datos_UH$nCohabitation <- NA
    # current cohabitation (we do not bother to put it in order, as we will merge all the unions list later and have it in order)
    datos_UH$union_start_cmc6 <- adjust_cmc (as.integer(substring(raw,5060,5063)))
    datos_UH$union_start_cmc_I6 <- imputed_date (as.integer(substring(raw,5060,5063)))
    datos_UH$union_start_type6 <- ifelse(!is.na(datos_UH$union_start_cmc6),2,NA) # cohabitation
    datos_UH$union_start_type6 <- factor (datos_UH$union_start_type6, levels=c(1,2,3),
                                          labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc6 <- NA
    datos_UH$marriage_start_cmc_I6 <- NA
    datos_UH$union_end_motive6 <- NA
    datos_UH$union_end_cmc6 <- NA
    datos_UH$union_end_cmc_I6 <- NA
    datos_UH$union_end_motive6 <- ifelse((!is.na(datos_UH$union_start_cmc6))&(is.na(datos_UH$union_end_cmc6)),
                                         0,
                                         datos_UH$union_end_motive6) # in union
    datos_UH$union_end_motive6 <- factor (datos_UH$union_end_motive6, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # clean some cohabitations (first unions still in union)
    idx <- (datos_UH$union_start_type6 %in% "cohabitation")&(datos_UH$union_end_motive1 %in% "in union")
    datos_UH$union_start_cmc6[idx] <- NA
    datos_UH$union_start_type6[idx] <- NA
    
    # other cohabitations
    datos_UH$nCohabitation <- as.integer(substring(raw,5093,5094))
    datos_UH$nCohabitation [is.na(datos_UH$nCohabitation)] <- 0
    datos_UH$nCohabitation <- datos_UH$nCohabitation + ifelse(!is.na(datos_UH$union_start_cmc6),1,0)
    
    # first cohabitation (before current one)
    datos_UH$union_start_type7 <- NA
    datos_UH$union_start_cmc7 <- adjust_cmc (as.integer(substring(raw,5095,5098)))
    datos_UH$union_start_cmc_I7 <- imputed_date (as.integer(substring(raw,5095,5098)))
    datos_UH$union_start_type7 <- ifelse(!is.na(datos_UH$union_start_cmc7),2,datos_UH$union_start_type7) # cohabitation
    datos_UH$union_start_type7 <- factor (datos_UH$union_start_type7, levels=c(1,2,3),
                                          labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc7 <- NA
    datos_UH$marriage_start_cmc_I7 <- NA
    datos_UH$union_end_motive7 <- NA
    datos_UH$union_end_cmc7 <- adjust_cmc (as.integer(substring(raw,5099,5102)))
    datos_UH$union_end_cmc_I7 <- imputed_date (as.integer(substring(raw,5099,5102)))
    datos_UH$union_end_motive7 <- ifelse((!is.na(datos_UH$union_start_cmc7))&(is.na(datos_UH$union_end_cmc7)),0,datos_UH$union_end_motive7) # in union
    datos_UH$union_end_motive7 <- ifelse((!is.na(datos_UH$union_start_cmc7))&(!is.na(datos_UH$union_end_cmc7)),2,datos_UH$union_end_motive7) # separation
    datos_UH$union_end_motive7 <- factor (datos_UH$union_end_motive7, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    # second cohabitation (before current one)
    datos_UH$union_start_type8 <- NA
    datos_UH$union_start_cmc8 <- adjust_cmc (as.integer(substring(raw,5162,5165)))
    datos_UH$union_start_cmc_I8 <- imputed_date (as.integer(substring(raw,5162,5165)))
    datos_UH$union_start_type8 <- ifelse(!is.na(datos_UH$union_start_cmc8),2,datos_UH$union_start_type8) # cohabitation
    datos_UH$union_start_type8 <- factor (datos_UH$union_start_type8, levels=c(1,2,3),
                                          labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc8 <- NA
    datos_UH$marriage_start_cmc_I8 <- NA
    datos_UH$union_end_motive8 <- NA
    datos_UH$union_end_cmc8 <- adjust_cmc (as.integer(substring(raw,5166,5169)))
    datos_UH$union_end_cmc_I8 <- imputed_date (as.integer(substring(raw,5166,5169)))
    datos_UH$union_end_motive8 <- ifelse((!is.na(datos_UH$union_start_cmc8))&(is.na(datos_UH$union_end_cmc8)),0,datos_UH$union_end_motive8) # in union
    datos_UH$union_end_motive8 <- ifelse((!is.na(datos_UH$union_start_cmc8))&(!is.na(datos_UH$union_end_cmc8)),2,datos_UH$union_end_motive8) # separation
    datos_UH$union_end_motive8 <- factor (datos_UH$union_end_motive8, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    # third cohabitation (before current one)
    datos_UH$union_start_type9 <- NA
    datos_UH$union_start_cmc9 <- adjust_cmc (as.integer(substring(raw,5229,5232)))
    datos_UH$union_start_cmc_I9 <- imputed_date (as.integer(substring(raw,5229,5232)))
    datos_UH$union_start_type9 <- ifelse(!is.na(datos_UH$union_start_cmc9),2,datos_UH$union_start_type9) # cohabitation
    datos_UH$union_start_type9 <- factor (datos_UH$union_start_type9, levels=c(1,2,3),
                                          labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc9 <- NA
    datos_UH$marriage_start_cmc_I9 <- NA
    datos_UH$union_end_motive9 <- NA
    datos_UH$union_end_cmc9 <- adjust_cmc (as.integer(substring(raw,5233,5236)))
    datos_UH$union_end_cmc_I9 <- imputed_date (as.integer(substring(raw,5233,5236)))
    datos_UH$union_end_motive9 <- ifelse((!is.na(datos_UH$union_start_cmc9))&(is.na(datos_UH$union_end_cmc9)),0,datos_UH$union_end_motive9) # in union
    datos_UH$union_end_motive9 <- ifelse((!is.na(datos_UH$union_start_cmc9))&(!is.na(datos_UH$union_end_cmc9)),2,datos_UH$union_end_motive9) # separation
    datos_UH$union_end_motive9 <- factor (datos_UH$union_end_motive9, levels=c(0,1,2,3,7,8,9),
                                          labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # fourth cohabitation (before current one)
    datos_UH$union_start_type10 <- NA
    datos_UH$union_start_cmc10 <- adjust_cmc (as.integer(substring(raw,5296,5299)))
    datos_UH$union_start_cmc_I10 <- imputed_date (as.integer(substring(raw,5296,5299)))
    datos_UH$union_start_type10 <- ifelse(!is.na(datos_UH$union_start_cmc10),2,datos_UH$union_start_type10) # cohabitation
    datos_UH$union_start_type10 <- factor (datos_UH$union_start_type10, levels=c(1,2,3),
                                           labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc10 <- NA
    datos_UH$marriage_start_cmc_I10 <- NA
    datos_UH$union_end_motive10 <- NA
    datos_UH$union_end_cmc10 <- adjust_cmc (as.integer(substring(raw,5300,5303)))
    datos_UH$union_end_cmc_I10 <- imputed_date (as.integer(substring(raw,5300,5303)))
    datos_UH$union_end_motive10 <- ifelse((!is.na(datos_UH$union_start_cmc10))&(is.na(datos_UH$union_end_cmc10)),0,datos_UH$union_end_motive10) # in union
    datos_UH$union_end_motive10 <- ifelse((!is.na(datos_UH$union_start_cmc10))&(!is.na(datos_UH$union_end_cmc10)),2,datos_UH$union_end_motive10) # separation
    datos_UH$union_end_motive10 <- factor (datos_UH$union_end_motive10, levels=c(0,1,2,3,7,8,9),
                                           labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # fifth cohabitation (before current one)
    datos_UH$union_start_type11 <- NA
    datos_UH$union_start_cmc11 <- adjust_cmc (as.integer(substring(raw,5363,5366)))
    datos_UH$union_start_cmc_I11 <- imputed_date (as.integer(substring(raw,5363,5366)))
    datos_UH$union_start_type11 <- ifelse(!is.na(datos_UH$union_start_cmc11),2,datos_UH$union_start_type11) # cohabitation
    datos_UH$union_start_type11 <- factor (datos_UH$union_start_type11, levels=c(1,2,3),
                                           labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc11 <- NA
    datos_UH$marriage_start_cmc_I11 <- NA
    datos_UH$union_end_motive11 <- NA
    datos_UH$union_end_cmc11 <- adjust_cmc (as.integer(substring(raw,5367,5370)))
    datos_UH$union_end_cmc_I11 <- imputed_date (as.integer(substring(raw,5367,5370)))
    datos_UH$union_end_motive11 <- ifelse((!is.na(datos_UH$union_start_cmc11))&(is.na(datos_UH$union_end_cmc11)),0,datos_UH$union_end_motive11) # in union
    datos_UH$union_end_motive11 <- ifelse((!is.na(datos_UH$union_start_cmc11))&(!is.na(datos_UH$union_end_cmc11)),2,datos_UH$union_end_motive11) # separation
    datos_UH$union_end_motive11 <- factor (datos_UH$union_end_motive11, levels=c(0,1,2,3,7,8,9),
                                           labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # sixth cohabitation (before current one)
    datos_UH$union_start_type12 <- NA
    datos_UH$union_start_cmc12 <- adjust_cmc (as.integer(substring(raw,5430,5433)))
    datos_UH$union_start_cmc_I12 <- imputed_date (as.integer(substring(raw,5430,5433)))
    datos_UH$union_start_type12 <- ifelse(!is.na(datos_UH$union_start_cmc12),2,datos_UH$union_start_type12) # cohabitation
    datos_UH$union_start_type12 <- factor (datos_UH$union_start_type12, levels=c(1,2,3),
                                           labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc12 <- NA
    datos_UH$marriage_start_cmc_I12 <- NA
    datos_UH$union_end_motive12 <- NA
    datos_UH$union_end_cmc12 <- adjust_cmc (as.integer(substring(raw,5434,5437)))
    datos_UH$union_end_cmc_I12 <- imputed_date (as.integer(substring(raw,5434,5437)))
    datos_UH$union_end_motive12 <- ifelse((!is.na(datos_UH$union_start_cmc12))&(is.na(datos_UH$union_end_cmc12)),0,datos_UH$union_end_motive12) # in union
    datos_UH$union_end_motive12 <- ifelse((!is.na(datos_UH$union_start_cmc12))&(!is.na(datos_UH$union_end_cmc12)),2,datos_UH$union_end_motive12) # separation
    datos_UH$union_end_motive12 <- factor (datos_UH$union_end_motive12, levels=c(0,1,2,3,7,8,9),
                                           labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # seventh cohabitation (before current one)
    datos_UH$union_start_type13 <- NA
    datos_UH$union_start_cmc13 <- adjust_cmc (as.integer(substring(raw,5497,5500)))
    datos_UH$union_start_cmc_I13 <- imputed_date (as.integer(substring(raw,5497,5500)))
    datos_UH$union_start_type13 <- ifelse(!is.na(datos_UH$union_start_cmc13),2,datos_UH$union_start_type13) # cohabitation
    datos_UH$union_start_type13 <- factor (datos_UH$union_start_type13, levels=c(1,2,3),
                                           labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc13 <- NA
    datos_UH$marriage_start_cmc_I13 <- NA
    datos_UH$union_end_motive13 <- NA
    datos_UH$union_end_cmc13 <- adjust_cmc (as.integer(substring(raw,5501,5504)))
    datos_UH$union_end_cmc_I13 <- imputed_date (as.integer(substring(raw,5501,5504)))
    datos_UH$union_end_motive13 <- ifelse((!is.na(datos_UH$union_start_cmc13))&(is.na(datos_UH$union_end_cmc13)),0,datos_UH$union_end_motive13) # in union
    datos_UH$union_end_motive13 <- ifelse((!is.na(datos_UH$union_start_cmc13))&(!is.na(datos_UH$union_end_cmc13)),2,datos_UH$union_end_motive13) # separation
    datos_UH$union_end_motive13 <- factor (datos_UH$union_end_motive13, levels=c(0,1,2,3,7,8,9),
                                           labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    # eight cohabitation (before current one)
    datos_UH$union_start_type14 <- NA
    datos_UH$union_start_cmc14 <- adjust_cmc (as.integer(substring(raw,5564,5567)))
    datos_UH$union_start_cmc_I14 <- imputed_date (as.integer(substring(raw,5564,5567)))
    datos_UH$union_start_type14 <- ifelse(!is.na(datos_UH$union_start_cmc14),2,datos_UH$union_start_type14) # cohabitation
    datos_UH$union_start_type14 <- factor (datos_UH$union_start_type14, levels=c(1,2,3),
                                           labels=c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$marriage_start_cmc14 <- NA
    datos_UH$marriage_start_cmc_I14 <- NA
    datos_UH$union_end_motive14 <- NA
    datos_UH$union_end_cmc14 <- adjust_cmc (as.integer(substring(raw,5568,5571)))
    datos_UH$union_end_cmc_I14 <- imputed_date (as.integer(substring(raw,5568,5571)))
    datos_UH$union_end_motive14 <- ifelse((!is.na(datos_UH$union_start_cmc14))&(is.na(datos_UH$union_end_cmc14)),0,datos_UH$union_end_motive14) # in union
    datos_UH$union_end_motive14 <- ifelse((!is.na(datos_UH$union_start_cmc14))&(!is.na(datos_UH$union_end_cmc14)),2,datos_UH$union_end_motive14) # separation
    datos_UH$union_end_motive14 <- factor (datos_UH$union_end_motive14, levels=c(0,1,2,3,7,8,9),
                                           labels=c("in union","widowhood","separation","separation","refused","unknown","NA"))
    
    dsum <- data.frame(
      u6=!is.na(datos_UH$union_start_cmc6),
      u7=!is.na(datos_UH$union_start_cmc7),
      u8=!is.na(datos_UH$union_start_cmc8),
      u9=!is.na(datos_UH$union_start_cmc9),
      u10=!is.na(datos_UH$union_start_cmc10),
      u11=!is.na(datos_UH$union_start_cmc11),
      u12=!is.na(datos_UH$union_start_cmc12),
      u13=!is.na(datos_UH$union_start_cmc13),
      u14=!is.na(datos_UH$union_start_cmc14)
    )
    
    # place current marriage in the marriage list
    datos_UH$nCohabitation_computed <- rowSums (dsum)
    datos_UH <- relocate (datos_UH, nCohabitation, .after= nMarriage_computed)
    datos_UH <- relocate (datos_UH, nCohabitation_computed, .after= nCohabitation)
    
    return (datos_UH)
  }
  
  datos_UH <- cohabitationHistory(raw, datos_UH)
  
  datos_UH2 <- order_union_history (datos_UH)
  
  datos <- datos %>%
    left_join(datos_UH2, by = "CaseID")
  
  datos$total_unions [is.na(datos$total_unions)] <- 0
  datos <- rename(datos,nUnion=total_unions)
  
  datos$nBioKids_decl <- as.integer(substring(raw,11290,11291))
  
  ##### Live births histories #####
  births <- data.frame (CaseID=as.integer(substring(raw_preg,1,8)))
  births$nLiveBirths <- as.integer(substring(raw_preg,14,14))
  births$dob_cmc <- adjust_cmc (as.integer(substring(raw_preg,27,30)))
  births$dob_cmc_I <- imputed_date (as.integer(substring(raw_preg,27,30)))
  births$babysex1 <- as.integer(substring(raw_preg,95,95))
  births$dod_cmc1 <- adjust_cmc (as.integer(substring(raw_preg,103,106)))
  births$dod_cmc_I1 <- imputed_date (as.integer(substring(raw_preg,103,106)))
  births$babysex2 <- as.integer(substring(raw_preg,151,151))
  births$dod_cmc2 <- adjust_cmc (as.integer(substring(raw_preg,159,162)))
  births$dod_cmc_I2 <- imputed_date (as.integer(substring(raw_preg,159,162)))
  births$babysex3 <- ifelse(births$nLiveBirths>2,sample(c(1,2), sum(births$nLiveBirths > 2, na.rm = TRUE), replace=TRUE),NA) # we do an imputation (no info)
  births$dod_cmc3 <- NA
  births$dod_cmc_I3 <- NA
  births$babysex4 <- ifelse(births$nLiveBirths>3,sample(c(1,2), sum(births$nLiveBirths > 3, na.rm = TRUE), replace=TRUE),NA) # we do an imputation (no info)
  births$dod_cmc4 <- NA
  births$dod_cmc_I4 <- NA
  
  births <- subset(births, nLiveBirths > 0) # only live births
  
  births <- live_birth_history2(births)
  
  ##### final women dataframe with live births #####
  datos <- datos %>%
    left_join(births, by = "CaseID")
  
  datos$nBioKids [is.na(datos$nBioKids)] <- 0
  
  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_1995 <- getDatos_1995()
NSFG_ENADID_1995 <- cleanENADID(NSFG_ENADID_1995)
NSFG_ENADID_1995 <-  reorder_birthHistory(NSFG_ENADID_1995)

pathNSFG_ENADID_1995 <- path.expand(paste0(dataPath, "NSFG_ENADID_1995.Rdat"))
save(NSFG_ENADID_1995, file = pathNSFG_ENADID_1995)

adjust_cmc_2002_after <- function (cmc) {
  cmc <- ifelse(cmc==0,NA,cmc)
  cmc <- ifelse(cmc %in% c(9997,9998,9999),9999,cmc)
  cmc <- ifelse((cmc > 300) & (cmc < 1800) & (cmc != 9999), cmc, NA)
  return (cmc)
}

imputed_date_2002_after <- function (cmc) {
  imp <- ifelse(cmc==0,NA,0)
  imp <- ifelse(cmc %in% c(9997,9998,9999),10,imp)
  imp <- ifelse((cmc > 300) & (cmc < 1800) & (cmc != 9999), 0, NA)
  return (imp)
}

#### 2002 ####
# Union history:
# complete up to 10 unions
# info on cohabitation before marriage
# complete date (cmc)

# Per-husband "did this husband have children from a prior relationship?" flags.
# Works for all NSFG cycles from 2002 onward.
# df          : raw survey dataframe (must contain FMARITAL and TIMESMAR)
# kidshx_names: character vector where kidshx_names[i] is the KIDSHX column for
#               the i-th husband (e.g. c("KIDSHX","KIDSHX2") or
#               c("KIDSHX_1","KIDSHX_2"))
# Returns a data.frame with one factor column per husband,
#   husb_priorKids_1 .. husb_priorKids_K  (K = length(kidshx_names)).
# Universe follows the NSFG codebook: KIDSHX_i is asked of every ever-married
# woman (FMARITAL %in% 1:4) with at least i marriages (TIMESMAR >= i); NA
# otherwise. NOTE: in 2002 the marriage-end module (MARENDHX + stop-living /
# death dates) was skipped whenever KIDSHX_i == "yes", so husb_priorKids_i ==
# "yes" marks the i-th marriage whose union_end is missing-by-skip -- the
# target for dissolution-date imputation.
nsfg_husb_priorKids <- function(df, kidshx_names) {
  n  <- nrow(df)
  nm <- names(df)
  # Marriage-count and marital-status variables are renamed in 2022-23:
  #   count : TIMESMAR (<=2019)  / FMARNO (2022-23)
  #   status: FMARITAL (<=2019)  / FMARIT (2022-23)
  cnt_var <- if ("TIMESMAR" %in% nm) "TIMESMAR" else if ("FMARNO" %in% nm) "FMARNO" else NA_character_
  mar_var <- if ("FMARITAL" %in% nm) "FMARITAL" else if ("FMARIT"  %in% nm) "FMARIT"  else NA_character_
  if (is.na(cnt_var) || is.na(mar_var)) {
    warning("nsfg_husb_priorKids(): missing marriage-count (TIMESMAR/FMARNO) ",
            "or marital-status (FMARITAL/FMARIT) column; all flags returned NA")
    timesmar     <- rep(NA_real_, n)
    ever_married <- rep(FALSE, n)
  } else {
    timesmar     <- df[[cnt_var]]
    ever_married <- !is.na(df[[mar_var]]) & df[[mar_var]] %in% 1:4
  }
  out        <- vector("list", length(kidshx_names))
  names(out) <- paste0("husb_priorKids_", seq_along(kidshx_names))
  for (i in seq_along(kidshx_names)) {
    nm  <- kidshx_names[i]
    val <- rep(NA_real_, n)
    if (nm %in% names(df)) {
      idx      <- ever_married & !is.na(timesmar) & timesmar >= i
      val[idx] <- df[[nm]][idx]
    }
    out[[i]] <- factor(
      val, levels = c(1, 5, 8, 9),
      labels = c("yes", "no", "refused", "don't know")
    )
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

getDatos_2002 <- function (correctDissolution=TRUE) {
  path_2002 <- paste0(nsfgPath, "2002/")

  wd <- getwd()
  setwd(path_2002)
  if (!exists("df_NSFG_2002")) df_NSFG_2002 <<- read_nsfg_data("2002FemResp.dat", "2002FemRespSetup.sps")
  if (!exists("df_NSFG_2002_preg")) df_NSFG_2002_preg <<- read_nsfg_data("2002FemPreg.dat", "2002PregSetup.sps")
  setwd(wd)

  if ("CASEID" %in% names(df_NSFG_2002)) {
    df_NSFG_2002 <- rename(df_NSFG_2002, CaseID=CASEID)
  }
  if ("CASEID" %in% names(df_NSFG_2002_preg)) {
    df_NSFG_2002_preg <- rename(df_NSFG_2002_preg, CaseID=CASEID)
  }
  
  n <- nrow(df_NSFG_2002)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2002")
  nms <- names (df_NSFG_2002)
  isCaseID <- ("CaseID" %in% nms)
  datos$CaseID <- if (isTRUE (isCaseID)) df_NSFG_2002$CaseID else df_NSFG_2002$CASEID
  datos$surveyDate_cmc <- adjust_cmc_2002_after (df_NSFG_2002$CMINTVW)
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- adjust_cmc_2002_after (df_NSFG_2002$CMBIRTH)
  datos$indiv_age_survey <- df_NSFG_2002$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$indiv_dob_cmc - 1) / 12)
  datos$indiv_weight <- df_NSFG_2002$FINALWGT
  datos$pregnant <- df_NSFG_2002$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2002$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2002$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  # compute number of unions (sum of marriage and cohabitations)
  datos$union_status <- factor(df_NSFG_2002$RMARITAL,
                               levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                               labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos$currMarr_cmc <- adjust_cmc_2002_after (df_NSFG_2002$CMMARRCH)
  datos$currCohab_cmc <- adjust_cmc_2002_after (df_NSFG_2002$CMSTRTCP)
  datos$nUnion <- NA
  datos$nMarriage <- ifelse(!is.na(datos$currMarr_cmc), df_NSFG_2002$PREVHUSB + 1, df_NSFG_2002$PREVHUSB)
  datos$nMarriage_decl <- df_NSFG_2002$TIMESMAR
  datos$nMarriage_decl [is.na(datos$nMarriage_decl)] <- 0
  datos$nCohab <- ifelse(!is.na(datos$currCohab_cmc), df_NSFG_2002$PREVCOHB + 1, df_NSFG_2002$PREVCOHB)
  datos$nUnion <- datos$nMarriage_decl + datos$nCohab
  
  #datos_UH <- datos[, c("CaseID", "nUnion")]
  #df_UH1 <- complete_union_history (datos_UH, df_NSFG_2002, 5) this one has only cmc, not month and year
  df_UH2 <- raw_union_history (df_NSFG_2002, survey_cmc = datos$surveyDate_cmc, capped = capMonth) # use cmc or month and year or year to compute cmc
  
  datos <- datos %>%
    left_join(df_UH2, by = "CaseID")%>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  # clean
  datos$CurrMarr_cmc <- NULL
  datos$yCurrCohab <- NULL
  datos$nUnion <- NULL
  datos$nMarriage <- NULL
  datos$nMarriage_decl <- NULL
  datos$nCohab <- NULL
  datos <- rename(datos, nUnion = total_unions)

  # ---- 2002 dissolution data gaps ----------------------------------------
  #
  # Two questionnaire skips leave marriage-end dates missing in 2002, both
  # confirmed against the raw file (2026-06):
  #
  # 1. Currently SEPARATED respondents (RMARITAL = 5): the marriage-end module
  #    is skipped ENTIRELY -- both MARENDHX and cmstphsbx are missing (229/229
  #    first-marriage separated women have BOTH NA). With no stop-living date,
  #    union_end_cmc stays NA and union_end_motive stays "in union", so these
  #    women look STILL MARRIED and their (recent) separation is invisible --
  #    biasing separation rates low near the survey. They are NOT caught by the
  #    refused/DK fix below (they never get a "separation" motive); they are
  #    tagged by sep_recent (built below) and are imputation targets, with the
  #    separation date bounded [marriage_start, survey].
  #
  # 2. Past marriages where the husband had prior children (KIDSHX = 1): CB-19
  #    (MARENDHX) and cmstphsbx are skipped for that marriage even when it is in
  #    the past (confirmed: remarried women with KIDSHX="yes" have MARENDHX=NA,
  #    119/119), leaving union_end_cmc = NA for marriages that DID end. When the
  #    woman later entered another union, cleanENADID fills union_end_cmc1 =
  #    union_start_cmc2 - 1 (the main driver of the elevated NSFG 2002 CORRECTION
  #    count). The real date is unrecoverable; the husb_priorKids_i flags (built
  #    below) identify the affected marriages.

  # Per-husband prior-children flags (husb_priorKids_1..5). Drives the 2002
  # dissolution-skip handling: husb_priorKids_i == "yes" => marriage i's
  # union_end was skipped in the questionnaire and is the imputation target.
  # Universe: all ever-married women (FMARITAL 1-4); NA for never-married.
  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2002,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4", "KIDSHX5")
  ))

  # Issue-1 flag: currently separated (RMARITAL = 5). The marriage-end module
  # was skipped for these women (confirmed all-NA above), so the separation is
  # real but its date is missing -> imputation target (bounded by the survey).
  datos$sep_recent <- !is.na(df_NSFG_2002$RMARITAL) & df_NSFG_2002$RMARITAL == 5

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2002$PARITY
  ###### count of live births #####
  
  NBRNALIV <- "NBRNALIV"
  if (!("NBRNALIV" %in% names(df_NSFG_2002_preg))) {
    NBRNALIV <- "BORNALIV"
  }
  df_birth_counts <- df_NSFG_2002_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(replace(.data [[NBRNALIV]], .data [[NBRNALIV]] == 9, 0), na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some women. We update to the new value
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  if (("BABYSEX" %in% names (df_NSFG_2002_preg))) df_NSFG_2002_preg <- rename (df_NSFG_2002_preg, BABYSEX1=BABYSEX)
  if (("NBRNALIV" %in% names (df_NSFG_2002_preg))) df_NSFG_2002_preg <- rename (df_NSFG_2002_preg, BORNALIV=NBRNALIV)
  df_NSFG_2002_preg <- subset(df_NSFG_2002_preg, BORNALIV != 9)
  children <- live_birth_history (df_NSFG_2002_preg, capped = capMonth) 

  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  datos$total_births [is.na(datos$total_births)] <- 0
  
  # Multiple births of 4 or 5 are ignored, the maximum being 3, so we update again nBioKids
  datos$nBioKids <- datos$total_births
  datos$total_live_births <- NULL
  
  datos <- compute_lastYear(datos)
  
  return (datos)
}

NSFG_ENADID_2002 <- getDatos_2002()
NSFG_ENADID_2002 <- cleanENADID(NSFG_ENADID_2002)
NSFG_ENADID_2002 <-  reorder_birthHistory(NSFG_ENADID_2002)

pathNSFG_ENADID_2002 <- path.expand(paste0(dataPath, "NSFG_ENADID_2002.Rdat"))
save(NSFG_ENADID_2002, file = pathNSFG_ENADID_2002)

#### 2006-10 ####
# Union history:
# up to 10 unions
# info on cohabitation before marriage
# complete date (cmc)
getDatos_2006_10 <- function () {
  path_2006_10 <- paste0(nsfgPath, "2006-10/")

  wd <- getwd()
  setwd(path_2006_10)
  if (!exists("df_NSFG_2006_10")) df_NSFG_2006_10 <<- read_nsfg_data("2006_2010_FemResp.dat", "2006_2010_FemRespSetup.sps")
  if (!exists("df_NSFG_2006_10_preg")) df_NSFG_2006_10_preg <<- read_nsfg_data("2006_2010_FemPreg.dat", "2006_2010_FemPregSetup.sps")
  setwd(wd)

  if ("CASEID" %in% names(df_NSFG_2006_10)) {
    df_NSFG_2006_10 <- rename(df_NSFG_2006_10, CaseID=CASEID)
  }
  if ("CASEID" %in% names(df_NSFG_2006_10_preg)) {
    df_NSFG_2006_10_preg <- rename(df_NSFG_2006_10_preg, CaseID=CASEID)
  }
  
  n <- nrow(df_NSFG_2006_10)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2006_10")
  nms <- names (df_NSFG_2006_10)
  isCaseID <- ("CaseID" %in% nms)
  datos$CaseID <- if (isTRUE (isCaseID)) df_NSFG_2006_10$CaseID else df_NSFG_2006_10$CASEID
  datos$surveyDate_cmc <- df_NSFG_2006_10$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- df_NSFG_2006_10$CMBIRTH
  datos$indiv_age_survey <- df_NSFG_2006_10$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$indiv_dob_cmc - 1) / 12)
  datos$indiv_weight <- df_NSFG_2006_10$WGTQ1Q16
  datos$pregnant <- df_NSFG_2006_10$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2006_10$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2006_10$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  # compute number of unions (sum of marriage and cohabitations)
  datos$union_status <- factor(df_NSFG_2006_10$RMARITAL,
                               levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                               labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos$currMarr_cmc <- df_NSFG_2006_10$CMMARRCH
  datos$yCurrCohab <- df_NSFG_2006_10$WNSTRTCP_Y
  datos$nUnion <- NA
  datos$nMarriage <- ifelse(!is.na(datos$currMarr_cmc), df_NSFG_2006_10$PREVHUSB + 1, df_NSFG_2006_10$PREVHUSB)
  datos$nMarriage_decl <- df_NSFG_2006_10$TIMESMAR
  datos$nMarriage_decl [is.na(datos$nMarriage_decl)] <- 0
  datos$nCohab <- ifelse(!is.na(datos$yCurrCohab), df_NSFG_2006_10$PREVCOHB + 1, df_NSFG_2006_10$PREVCOHB)
  datos$nUnion <- datos$nMarriage_decl + datos$nCohab
  
  #datos_UH <- datos[, c("CaseID", "nUnion")]
  #df_UH1 <- complete_union_history (datos_UH, df_NSFG_2006_10, 5)
  df_UH2 <- raw_union_history (df_NSFG_2006_10, survey_cmc = datos$surveyDate_cmc, capped = capMonth)
  
  datos <- datos %>%
    left_join(df_UH2, by = "CaseID") %>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  # clean
  datos$currMarr_cmc <- NULL
  datos$yCurrCohab <- NULL
  datos$nUnion <- NULL
  datos$nMarriage <- NULL
  datos$nMarriage_decl <- NULL
  datos$nCohab <- NULL
  datos <- rename(datos, nUnion = total_unions)

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2006_10,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4", "KIDSHX5", "KIDSHX6")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2006_10$PARITY
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2006_10_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(BORNALIV, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some 55 women. We update
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  children <- live_birth_history (df_NSFG_2006_10_preg, capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
}

NSFG_ENADID_2006_10 <- getDatos_2006_10()
NSFG_ENADID_2006_10 <- cleanENADID(NSFG_ENADID_2006_10)
NSFG_ENADID_2006_10 <-  reorder_birthHistory(NSFG_ENADID_2006_10)

pathNSFG_ENADID_2006_10 <- path.expand(paste0(dataPath, "NSFG_ENADID_2006_10.Rdat"))
save(NSFG_ENADID_2006_10, file = pathNSFG_ENADID_2006_10)

#### 2011-13 ####
# Union history:
# complete union history (both marriage and cohabitation)
# up to 10 unions
# info on cohabitation before marriage
# complete date (cmc)
getDatos_2011_13 <- function () {
  path_2011_13 <- paste0(nsfgPath, "2011-13/")
  
  wd <- getwd()
  setwd(path_2011_13)
  if (!exists("df_NSFG_2011_13")) df_NSFG_2011_13 <<- read_nsfg_data("2011_2013_FemRespData.dat", "2011_2013_FemRespSetup.sps")
  if (!exists("df_NSFG_2011_13_preg")) df_NSFG_2011_13_preg <<- read_nsfg_data("2011_2013_FemPregData.dat", "2011_2013_FemPregSetup.sps")
  setwd(wd)
  
  if ("CASEID" %in% names(df_NSFG_2011_13)) {
    df_NSFG_2011_13 <- rename(df_NSFG_2011_13, CaseID=CASEID)
  }
  if ("CASEID" %in% names(df_NSFG_2011_13_preg)) {
    df_NSFG_2011_13_preg <- rename(df_NSFG_2011_13_preg, CaseID=CASEID)
  }
  
  n <- nrow(df_NSFG_2011_13)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2011_13")
  nms <- names (df_NSFG_2011_13)
  isCaseID <- ("CaseID" %in% nms)
  datos$CaseID <- if (isTRUE (isCaseID)) df_NSFG_2011_13$CaseID else df_NSFG_2011_13$CASEID
  datos$surveyDate_cmc <- df_NSFG_2011_13$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- df_NSFG_2011_13$CMBIRTH
  datos$indiv_age_survey <- df_NSFG_2011_13$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$indiv_dob_cmc - 1) / 12)
  datos$indiv_weight <- df_NSFG_2011_13$WGT2011_2013
  datos$pregnant <- df_NSFG_2011_13$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2011_13$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2011_13$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  # compute number of unions (sum of marriage and cohabitations)
  datos$union_status <- factor(df_NSFG_2011_13$RMARITAL,
                               levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                               labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos$currMarr_cmc <- df_NSFG_2011_13$CMMARRCH
  datos$yCurrCohab <- df_NSFG_2011_13$WNSTRTCP_Y
  datos$nUnion <- NA
  datos$nMarriage <- ifelse(!is.na(datos$currMarr_cmc), df_NSFG_2011_13$PREVHUSB + 1, df_NSFG_2011_13$PREVHUSB)
  datos$nMarriage_decl <- df_NSFG_2011_13$TIMESMAR
  datos$nMarriage_decl [is.na(datos$nMarriage_decl)] <- 0
  datos$nCohab <- ifelse(!is.na(datos$yCurrCohab), df_NSFG_2011_13$PREVCOHB + 1, df_NSFG_2011_13$PREVCOHB)
  datos$nUnion <- datos$nMarriage_decl + datos$nCohab
  
  #datos_UH <- datos[, c("CaseID", "nUnion")]
  #df_UH1 <- complete_union_history (datos_UH, df_NSFG_2011_13, 5)
  df_UH2 <- raw_union_history (df_NSFG_2011_13, survey_cmc = datos$surveyDate_cmc, capped = capMonth)
  
  datos <- datos %>%
    left_join(df_UH2, by = "CaseID")%>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  # clean
  datos$currMarr_cmc <- NULL
  datos$yCurrCohab <- NULL
  datos$nUnion <- NULL
  datos$nMarriage <- NULL
  datos$nMarriage_decl <- NULL
  datos$nCohab <- NULL
  datos <- rename(datos, nUnion = total_unions)

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2011_13,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4", "KIDSHX5")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2011_13$PARITY
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2011_13_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(BORNALIV, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some 55 women. We update
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  children <- live_birth_history (df_NSFG_2011_13_preg, capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
  
}

NSFG_ENADID_2011_13 <- getDatos_2011_13()
NSFG_ENADID_2011_13 <- cleanENADID(NSFG_ENADID_2011_13)
NSFG_ENADID_2011_13 <-  reorder_birthHistory(NSFG_ENADID_2011_13)

pathNSFG_ENADID_2011_13 <- path.expand(paste0(dataPath, "NSFG_ENADID_2011_13.Rdat"))
save(NSFG_ENADID_2011_13, file = pathNSFG_ENADID_2011_13)


#### 2013-15 ####
# Union history:
# complete union history (both marriage and cohabitation)
# up to 10 unions
# info on cohabitation before marriage
# complete date (cmc)
getDatos_2013_15 <- function () {
  path_2013_15 <- paste0(nsfgPath, "2013-15/")
  
  wd <- getwd()
  setwd(path_2013_15)
  if (!exists("df_NSFG_2013_15")) df_NSFG_2013_15 <<- read_nsfg_data("2013_2015_FemRespData.dat", "2013_2015_FemRespSetup.sps")
  if (!exists("df_NSFG_2013_15_preg")) df_NSFG_2013_15_preg <<- read_nsfg_data("2013_2015_FemPregData.dat", "2013_2015_FemPregSetup.sps")
  setwd(wd)
  
  if ("CASEID" %in% names (df_NSFG_2013_15)) df_NSFG_2013_15 <- rename(df_NSFG_2013_15, CaseID=CASEID)
  if ("CASEID" %in% names (df_NSFG_2013_15_preg)) df_NSFG_2013_15_preg <- rename(df_NSFG_2013_15_preg, CaseID=CASEID)
  
  n <- nrow(df_NSFG_2013_15)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2013_15")
  nms <- names (df_NSFG_2013_15)
  isCaseID <- ("CaseID" %in% nms)
  datos$CaseID <- if (isTRUE (isCaseID)) df_NSFG_2013_15$CaseID else df_NSFG_2013_15$CASEID
  datos$surveyDate_cmc <- df_NSFG_2013_15$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- df_NSFG_2013_15$CMBIRTH
  datos$indiv_age_survey <- df_NSFG_2013_15$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$indiv_dob_cmc - 1) / 12)
  datos$indiv_weight <- df_NSFG_2013_15$WGT2013_2015
  datos$pregnant <- df_NSFG_2013_15$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2013_15$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2013_15$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  # compute number of unions (sum of marriage and cohabitations)
  datos$union_status <- factor(df_NSFG_2013_15$RMARITAL,
                               levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                               labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos$currMarr_cmc <- df_NSFG_2013_15$CMMARRCH
  datos$yCurrCohab <- df_NSFG_2013_15$WNSTRTCP_Y
  datos$nUnion <- NA
  datos$nMarriage <- ifelse(!is.na(datos$currMarr_cmc), df_NSFG_2013_15$PREVHUSB + 1, df_NSFG_2013_15$PREVHUSB)
  datos$nMarriage_decl <- df_NSFG_2013_15$TIMESMAR
  datos$nMarriage_decl [is.na(datos$nMarriage_decl)] <- 0
  datos$nCohab <- ifelse(!is.na(datos$yCurrCohab), df_NSFG_2013_15$PREVCOHB + 1, df_NSFG_2013_15$PREVCOHB)
  datos$nUnion <- datos$nMarriage_decl + datos$nCohab
  
  #datos_UH <- datos[, c("CaseID", "nUnion")]
  #df_UH <- complete_union_history (datos_UH, df_NSFG_2013_15, 5)
  df_rawUH <- raw_union_history (df_NSFG_2013_15, survey_cmc = datos$surveyDate_cmc, capped = capMonth)
  
  datos <- datos %>%
    left_join(df_rawUH, by = "CaseID") %>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  # clean
  datos$currMarr_cmc <- NULL
  datos$yCurrCohab <- NULL
  datos$nUnion <- NULL
  datos$nMarriage <- NULL
  datos$nMarriage_decl <- NULL
  datos$nCohab <- NULL
  datos <- rename(datos, nUnion=total_unions)

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2013_15,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4", "KIDSHX5")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2013_15$PARITY
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2013_15_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(BORNALIV, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some women. We update
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  children <- live_birth_history (df_NSFG_2013_15_preg, capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
  
}

NSFG_ENADID_2013_15 <- getDatos_2013_15()
NSFG_ENADID_2013_15 <- cleanENADID(NSFG_ENADID_2013_15)
NSFG_ENADID_2013_15 <-  reorder_birthHistory(NSFG_ENADID_2013_15)

pathNSFG_ENADID_2013_15 <- path.expand(paste0(dataPath, "NSFG_ENADID_2013_15.Rdat"))
save(NSFG_ENADID_2013_15, file = pathNSFG_ENADID_2013_15)


#### 2015-17 ####
# Union history:
# complete union history (both marriage and cohabitation)
# up to 10 unions
# info on cohabitation before marriage
# year, not cmc, for dates
getDatos_2015_17 <- function () {
  path_2015_17 <- paste0(nsfgPath, "2015-17/")

  wd <- getwd()
  setwd(path_2015_17)
  if (!exists("df_NSFG_2015_17")) df_NSFG_2015_17 <<- read_nsfg_data("2015_2017_FemRespData.dat", "2015_2017_FemRespSetup.sps")
  if (!exists("df_NSFG_2015_17_preg")) df_NSFG_2015_17_preg <<- read_nsfg_data("2015_2017_FemPregData.dat", "2015_2017_FemPregSetup.sps")
  setwd(wd)
  
  if ("CASEID" %in% names (df_NSFG_2015_17)) df_NSFG_2015_17 <- rename(df_NSFG_2015_17, CaseID=CASEID)
  if ("CASEID" %in% names (df_NSFG_2015_17_preg)) df_NSFG_2015_17_preg <- rename(df_NSFG_2015_17_preg, CaseID=CASEID)
  
  n <- nrow(df_NSFG_2015_17)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2015_17")
  datos$CaseID <- df_NSFG_2015_17$CaseID
  datos$surveyDate_cmc <- df_NSFG_2015_17$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- NA
  datos$indiv_age_survey <- df_NSFG_2015_17$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$surveyDate_cmc - 1) / 12) - datos$indiv_age_survey
  datos$indiv_dob_cmc <- compute_cmc(imputed_month (nrow(datos)), datos$yBirth)
  datos$indiv_weight <- df_NSFG_2015_17$WGT2015_2017
  datos$pregnant <- df_NSFG_2015_17$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2015_17$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2015_17$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  # compute number of unions (sum of marriage and cohabitations)
  datos$union_status <- factor(df_NSFG_2015_17$RMARITAL,
                               levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                               labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos$currMarr_cmc <- df_NSFG_2015_17$CMMARRCH
  datos$yCurrCohab <- df_NSFG_2015_17$WNSTRTCP_Y
  datos$nUnion <- NA
  datos$nMarriage <- ifelse(!is.na(datos$currMarr_cmc), df_NSFG_2015_17$PREVHUSB + 1, df_NSFG_2015_17$PREVHUSB)
  datos$nMarriage_decl <- df_NSFG_2015_17$TIMESMAR
  datos$nMarriage_decl [is.na(datos$nMarriage_decl)] <- 0
  datos$nCohab <- ifelse(!is.na(datos$yCurrCohab), df_NSFG_2015_17$PREVCOHB + 1, df_NSFG_2015_17$PREVCOHB)
  datos$nUnion <- datos$nMarriage_decl + datos$nCohab
  
  #datos_UH <- datos[, c("CaseID", "nUnion")]
  #df_UH <- complete_union_history (datos_UH, df_NSFG_2015_17)
  df_rawUH <- raw_union_history (df_NSFG_2015_17, survey_cmc = datos$surveyDate_cmc, capped = capMonth)
  
  datos <- datos %>%
    left_join(df_rawUH, by = "CaseID") %>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  # clean
  datos$currMarr_cmc <- NULL
  datos$yCurrCohab <- NULL
  datos$nUnion <- NULL
  datos$nMarriage <- NULL
  datos$nMarriage_decl <- NULL
  datos$nCohab <- NULL
  datos <- rename(datos, nUnion=total_unions)

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2015_17,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4", "KIDSHX5", "KIDSHX6")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2015_17$PARITY
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2015_17_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(BORNALIV, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some 55 women. We update
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  children <- live_birth_history (df_NSFG_2015_17_preg, capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
  
}

NSFG_ENADID_2015_17 <- getDatos_2015_17()
NSFG_ENADID_2015_17 <- cleanENADID(NSFG_ENADID_2015_17)
NSFG_ENADID_2015_17 <-  reorder_birthHistory(NSFG_ENADID_2015_17)

pathNSFG_ENADID_2015_17 <- path.expand(paste0(dataPath, "NSFG_ENADID_2015_17.Rdat"))
save(NSFG_ENADID_2015_17, file = pathNSFG_ENADID_2015_17)

#### 2017-19 ####
# Union history:
# NO year of marriage start or end
# year of cohabitation start or end
# year, not cmc, for dates
# therefore NO information on first union NOR Union History
# the information necessary for a complete Union History is in Restricted Use files...
getDatos_2017_19 <- function () {
  path_2017_19 <- paste0(nsfgPath, "2017-19/")

  wd <- getwd()
  setwd(path_2017_19)
  if (!exists("df_NSFG_2017_19")) df_NSFG_2017_19 <<- read_nsfg_data("2017_2019_FemRespData.dat", "2017_2019_FemRespSetup.sps")
  if (!exists("df_NSFG_2017_19_preg")) df_NSFG_2017_19_preg <<- read_nsfg_data("2017_2019_FemPregData.dat", "2017_2019_FemPregSetup.sps")
  setwd(wd)
  
  if ("CASEID" %in% names (df_NSFG_2017_19)) df_NSFG_2017_19 <- rename(df_NSFG_2017_19, CaseID=CASEID)
  if ("CASEID" %in% names (df_NSFG_2017_19_preg)) df_NSFG_2017_19_preg <- rename(df_NSFG_2017_19_preg, CaseID=CASEID)
  
  n <- nrow(df_NSFG_2017_19)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2017_19")
  datos$CaseID <- df_NSFG_2017_19$CaseID
  datos$surveyDate_cmc <- df_NSFG_2017_19$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- NA
  datos$indiv_age_survey <- df_NSFG_2017_19$AGE_R
  # we no longer have the cmc data of birht in the public file!!
  datos$yBirth <- 1900 + floor((datos$surveyDate_cmc - 1) / 12) - datos$indiv_age_survey
  datos$indiv_dob_cmc <- compute_cmc(imputed_month (nrow(datos)), datos$yBirth) 
  datos$indiv_weight <- df_NSFG_2017_19$WGT2017_2019
  datos$pregnant <- df_NSFG_2017_19$PREGNOWQ
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2017_19$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2017_19$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA
  
  ##### Union history #####
  datos_UH <- datos[,c("CaseID","ever_contraception")]
  # compute number of unions (sum of marriage and cohabitations)
  datos_UH$union_status <- factor(df_NSFG_2017_19$RMARITAL,
                                  levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                                  labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos_UH$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos_UH$currMarr_cmc <- df_NSFG_2017_19$CMMARRCH
  datos_UH$yCurrCohab <- df_NSFG_2017_19$WNSTRTCP_Y
  datos_UH$yStartCurrMarr <- df_NSFG_2017_19$CMSTRTHP
  datos_UH$nUnion <- NA
  datos_UH$nMarriage <- ifelse(!is.na(datos_UH$currMarr_cmc), df_NSFG_2017_19$PREVHUSB + 1, df_NSFG_2017_19$PREVHUSB)
  datos_UH$nMarriageDecl <- df_NSFG_2017_19$TIMESMAR
  datos_UH$nCohab <- ifelse(!is.na(datos_UH$yCurrCohab), df_NSFG_2017_19$PREVCOHB + 1, df_NSFG_2017_19$PREVCOHB)
  datos_UH$nUnion <- datos_UH$nMarriage + datos_UH$nCohab
  
  # we susbtitute "other" for "single" if no union. Other cases could be "separation" or "widow", but we don't handle them
  # are we cannot be exhaustive (we have the complete marriage history, but not the cohabitation history)
  datos_UH$union_status <- ifelse(datos_UH$nUnion==0, "single", datos_UH$union_status)
  
  # join datos and datos_BH
  datos <- datos %>%
    left_join(datos_UH, by = "CaseID")

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2017_19,
    kidshx_names = c("KIDSHX", "KIDSHX2", "KIDSHX3", "KIDSHX4")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2017_19$PARITY
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2017_19_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(NBRNLV_S, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  
  # nBioKids is incorrect for some 55 women. We update
  datos$nBioKids <- datos$total_live_births
  datos$total_live_births <- NULL
  
  ###### Pregnancies ######
  df_NSFG_2017_19_preg$BORNALIV <- df_NSFG_2017_19_preg$NBRNLV_S
  children <- live_birth_history (subset(df_NSFG_2017_19_preg, OUTCOME==1), capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
  
}

NSFG_ENADID_2017_19 <- getDatos_2017_19()
NSFG_ENADID_2017_19 <- cleanENADID(NSFG_ENADID_2017_19)
NSFG_ENADID_2017_19 <-  reorder_birthHistory(NSFG_ENADID_2017_19)

pathNSFG_ENADID_2017_19 <- path.expand(paste0(dataPath, "NSFG_ENADID_2017_19.Rdat"))
save(NSFG_ENADID_2017_19, file = pathNSFG_ENADID_2017_19)

#### 2022-23 ####
# Union history: incomplete (only the first previous cohabitation and the last one)
# has complete marriage history, but NOT the cohabitation history
# info on cohabitation before marriage
# year, not cmc, for dates
# therefore information only on first union NOR Union History
getDatos_2022_23 <- function () {
  firstMarriage_or_firstCohabitation <- function (datos_UH, df_NSFG_2022_23) {
    # this function is used to determine whether first marriage or first cohabitation occurs first, and to determine the date of first union (marriage or cohabitation)
    # 0. First marriage or first cohabitation: which one is first?
    datos_UH$yMarr1 <- df_NSFG_2022_23$WHMARHX_Y_1
    datos_UH$yMarr1 <- ifelse((datos_UH$nMarriage==1)&(is.na(datos_UH$yMarr1)), yearFrom_cmc(datos_UH$CurrMarr_cmc), datos_UH$yMarr1)
    datos_UH$yMarrEnd1 <- df_NSFG_2022_23$ENDMARRX_Y_1
    datos_UH$yStopLiving1 <- df_NSFG_2022_23$WNSTPHX_Y_1
    datos_UH$cohabBef1 <- df_NSFG_2022_23$LVTOGHX_1
    datos_UH$yCohabBef1 <- df_NSFG_2022_23$STRTOGHX_Y_1
    
    datos_UH$yCohab1 <- df_NSFG_2022_23$STRTOTH1_Y
    datos_UH$yCohab1 <- ifelse((datos_UH$nCohab==1)&(is.na(datos_UH$yCohab1)), datos_UH$yCurrCohab, datos_UH$yCohab1)
    datos_UH$yCohabEnd1 <- df_NSFG_2022_23$STPTOGC1_Y
    
    ###### First Marriage ######
    # 1. we start supposing first marriage is the first union
    datos_UH$marriage_start_cmc1 <- compute_cmc (imputed_month (nrow(datos_UH)), datos_UH$yMarr1)
    datos_UH$union_start_cmc1 <- datos_UH$marriage_start_cmc1
    datos_UH$union_start_cmc1 <- ifelse(!is.na(datos_UH$yCohabBef1),compute_cmc (imputed_month (nrow(datos_UH)), datos_UH$yCohabBef1),datos_UH$union_start_cmc1)
    
    # if cohabitation before first marriage occurs in the same year than the marriage, we suppose there is a 3 months lag
    # with marriage occurring in the middle of year (this leaves place for separation, if it occurs the same year than marriage)
    idx <- (datos_UH$yCohabBef1 == datos_UH$yMarr1)
    idx [is.na(idx)] <- FALSE
    datos_UH$marriage_start_cmc1 [idx] <- compute_cmc (imputed_month (nrow(datos_UH)), datos_UH$yMarr1) [idx]
    datos_UH$union_start_cmc1 [idx] <- compute_cmc (3, datos_UH$yCohabBef1) [idx]
    
    # date of marriage separation (stop living together) and motive (separation or widowhood)
    datos_UH$MarrEndMotive1 <- df_NSFG_2022_23$MARENDHX_1
    datos_UH$MarrEndMotive1 <- factor (datos_UH$MarrEndMotive1, levels=c(1,2,3,8,9),
                                       labels=c("widowhood","separation","separation","refused","unknown"))
    datos_UH$union_end_cmc1 <- compute_cmc (imputed_month (nrow(datos_UH)), datos_UH$yStopLiving1)
    datos_UH$union_end_cmc1 <-ifelse (datos_UH$MarrEndMotive1=="widowhood",compute_cmc (imputed_month (nrow(datos_UH)), datos_UH$yMarrEnd1),datos_UH$union_end_cmc1)  
    
    datos_UH <- datos_UH %>%
      dplyr::mutate(union_end_motive1 = case_when(
        MarrEndMotive1 == "widowhood" ~ 1,
        # 2. If it has an end date (but wasn't widowhood), code as 2
        !is.na(union_end_cmc1)        ~ 2,
        # 3. If it has a start date (but no end date), code as 0
        !is.na(union_start_cmc1)      ~ 0,
        # 4. Everything else (no start date) stays NA
        .default = NA_integer_
      ))
    
    datos_UH <- zap_label(datos_UH)
    
    # if marriage and separation occurs in the same year, we suppose there is a 3 months lag
    # and separation occurs in the second half of the year
    idx <- (datos_UH$yStopLiving1 == datos_UH$yMarr1)
    idx [is.na(idx)] <- FALSE
    datos_UH$union_end_cmc1 [idx] <- compute_cmc (9, datos_UH$yStopLiving1) [idx]
    
    # union start type
    datos_UH <- datos_UH %>%
      dplyr::mutate(union_start_type1 = case_when(
        (cohabBef1 == 1) ~ 3, # cohabitation before marriage
        !is.na(yMarr1) ~ 1, # marriage
        # Everything else (no start date) stays NA
        .default = NA_integer_
      ))
    
    # 2. we check now whether first cohabitation occurred before first marriage and adjust accordingly
    datos_UH$marr1_and_cohab1_present <- (!is.na(datos_UH$yMarr1) & !is.na(datos_UH$yCohab1))
    datos_UH$marr1_no_cohab1 <- (!is.na(datos_UH$yMarr1) & is.na(datos_UH$yCohab1))
    datos_UH$no_marr1_cohab1 <- (is.na(datos_UH$yMarr1) & !is.na(datos_UH$yCohab1))
    datos_UH$marr1_bef_cohab1 <- datos_UH$marr1_and_cohab1_present & (datos_UH$yMarr1 < datos_UH$yCohab1)
    datos_UH$marr1_after_cohab1 <- datos_UH$marr1_and_cohab1_present & (datos_UH$yMarr1 > datos_UH$yCohab1)
    datos_UH$marr1_equal_cohab1 <- datos_UH$marr1_and_cohab1_present & (datos_UH$yMarr1 == datos_UH$yCohab1)
    
    # check everything is OK
    if (anyNA(datos_UH$marr1_and_cohab1_present)) cat("NA in marr1_and_cohab1_present")
    if (anyNA(datos_UH$marr1_no_cohab1)) cat("NA in marr1_no_cohab1")
    if (anyNA(datos_UH$no_marr1_cohab1)) cat("NA in no_marr1_cohab1")
    if (anyNA(datos_UH$marr1_bef_cohab1)) cat("NA in marr1_bef_cohab1")
    if (anyNA(datos_UH$marr1_after_cohab1)) cat("NA in marr1_after_cohab1")
    if (anyNA(datos_UH$marr1_equal_cohab1)) cat("NA in marr1_equal_cohab1")
    
    # 3. the important case of first cohab been the first union
    datos_UH$firstUnion_isCohab <- (datos_UH$no_marr1_cohab1) | (datos_UH$marr1_after_cohab1)
    datos_UH$firstUnion_isCohab_withSep <- datos_UH$firstUnion_isCohab & (!is.na(datos_UH$yCohabEnd1))
    datos_UH$firstUnion_isCohab_withSep [is.na(datos_UH$firstUnion_isCohab_withSep)] <- FALSE
    
    datos_UH$union_start_cmc1 [datos_UH$firstUnion_isCohab] <- compute_cmc(imputed_month (nrow(datos_UH)), datos_UH$yCohab1) [datos_UH$firstUnion_isCohab]
    datos_UH$marriage_start_cmc1 [datos_UH$firstUnion_isCohab] <- NA
    datos_UH$union_end_cmc1 [datos_UH$firstUnion_isCohab] <- compute_cmc(imputed_month (nrow(datos_UH)), datos_UH$yCohabEnd1) [datos_UH$firstUnion_isCohab]
    # there is no motive in the survey for end of cohabitation, and we suppose this is separation
    datos_UH$union_end_motive1 [datos_UH$firstUnion_isCohab_withSep] <- 2
    datos_UH$union_start_type1 [datos_UH$firstUnion_isCohab] <- 2
    
    # if first cohabitation and separation occurs in the same year, we suppose there is a 6 months lag
    idx <- datos_UH$firstUnion_isCohab & (datos_UH$yCohab1 == datos_UH$yCohabEnd1)
    idx [is.na(idx)] <- FALSE
    datos_UH$union_start_cmc1 [idx] <- compute_cmc(3, datos_UH$yCohab1) [idx]
    datos_UH$union_end_cmc1 [idx] <- compute_cmc(9, datos_UH$yCohabEnd1) [idx]
    
    # 3. union starts but not ends, therefore still in union
    idx <- (!is.na(datos_UH$union_start_cmc1)&is.na(datos_UH$union_end_cmc1))
    datos_UH$union_end_motive1 [idx] <- 0
    
    # 4. union start type and end motive as factor
    datos_UH$union_start_type1 <- factor (datos_UH$union_start_type1, levels=c(1,2,3),
                                          labels = c("marriage","cohabitation","cohabitation before marriage"))
    datos_UH$union_end_motive1 <- factor (datos_UH$union_end_motive1, levels=c(0,1,2,9),
                                          labels=c("in union", "widowhood", "separation", "unknown"))
    
    # we substitute "other" for "single" if no union. Other cases could be "separation" or "widow", but we don't handle them
    # are we cannot be exhaustive (we have the complete marriage history, but not the cohabitation history)
    datos_UH$union_status <- ifelse(datos_UH$nUnion==0, "single", datos_UH$union_status)
    
    # delete raw columns
    datos_UH$yMarr1 <- NULL
    datos_UH$yCohab1 <- NULL
    datos_UH$yMarrEnd1 <- NULL
    datos_UH$yCohabEnd1 <- NULL
    datos_UH$cohabBef1 <- NULL
    datos_UH$yCohabBef1 <- NULL
    datos_UH$MarrEndMotive1 <- NULL
    datos_UH$yStopLiving1 <- NULL
    datos_UH$CurrMarr_cmc <- NULL
    datos_UH$yCurrCohab <- NULL
    
    # delete boolean columns
    datos_UH$marr1_and_cohab1_present <- NULL
    datos_UH$marr1_no_cohab1 <- NULL
    datos_UH$no_marr1_cohab1 <- NULL
    datos_UH$marr1_bef_cohab1 <- NULL
    datos_UH$marr1_after_cohab1 <- NULL
    datos_UH$marr1_equal_cohab1 <- NULL
    datos_UH$firstUnion_isCohab <- NULL
    datos_UH$firstUnion_isCohab_withSep <- NULL
    
    return (datos_UH)
  }

  path_2022_23 <- paste0(nsfgPath, "2022-23/NSFG-2022-2023-FemRespPUFData.sas7bdat")
  path_2022_23_preg <- paste0(nsfgPath, "2022-23/NSFG-2022-2023-FemPregPUFData.sas7bdat")
  if (!exists("df_NSFG_2022_23")) df_NSFG_2022_23 <<- haven::read_sas(path_2022_23)
  if (!exists("df_NSFG_2022_23_preg")) df_NSFG_2022_23_preg <<- haven::read_sas(path_2022_23_preg)
  
  n <- nrow(df_NSFG_2022_23)
  datos <- data.frame(country=rep("USA",n), survey="NSFG2022_23")
  datos$CaseID <- df_NSFG_2022_23$CaseID
  datos$surveyDate_cmc <- df_NSFG_2022_23$CMINTVW
  datos$lastYear <- NA
  datos$indiv_dob_cmc <- NA
  datos$indiv_age_survey <- df_NSFG_2022_23$AGE_R
  # we no longer have the cmc data of birth in the public file!!
  survey_year  <- 1900 + floor((datos$surveyDate_cmc - 1) / 12)
  survey_month <- datos$surveyDate_cmc - (survey_year - 1900) * 12
  
  # Randomly assign birth month first, then derive yBirth from it
  set.seed(42)  # or wherever you set your seed
  birth_month <- imputed_month (nrow(datos))
  
  # If imputed birth month <= survey month, birthday has passed: born in survey_year - age
  # If imputed birth month >  survey month, birthday hasn't passed: born in survey_year - age - 1
  datos$yBirth <- survey_year - datos$indiv_age_survey -
    ifelse(birth_month > survey_month, 1L, 0L)
  datos$indiv_dob_cmc <- compute_cmc(birth_month, datos$yBirth)
  
  datos$indiv_weight <- df_NSFG_2022_23$WGT2022_2023
  datos$pregnant <- df_NSFG_2022_23$CURRPREG
  datos$pregnant <- factor (datos$pregnant, levels=c(1,5,8,9), labels=c("yes","no","refused","unknown"))
  datos$want_another <- df_NSFG_2022_23$RWANT
  datos$want_another <- factor(datos$want_another, levels=c(1,5,8,9),
                               labels=c("yes","no","refused","unknown"))
  datos$prob_want_another <- df_NSFG_2022_23$PROBWANT # if want_another is unknown
  datos$prob_want_another <- factor(datos$prob_want_another, levels=c(1,2,8,9),
                                    labels=c("probably yes","probably no","refused","unknown"))
  datos$age_first_sex <- NA
  datos$ever_contraception <- NA

  ##### Union history #####
  datos_UH <- datos[,c("CaseID","ever_contraception")]
  # compute number of unions (sum of marriage and cohabitations)
  datos_UH$union_status <- factor(df_NSFG_2022_23$RMARITAL,
                                  levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                                  labels = c("married", "cohabiting", "widowed", "divorced", "separated", "single", "refused", "unknown"))
  datos_UH$ever_contraception <-NULL # we had it only to have a dataframe at the start...
  datos_UH$currMarr_cmc <- df_NSFG_2022_23$CMMARRCH
  datos_UH$yCurrCohab <- df_NSFG_2022_23$WNSTRTCP_Y
  datos_UH$nUnion <- NA
  datos_UH$nMarriage <- ifelse(!is.na(datos_UH$currMarr_cmc), df_NSFG_2022_23$PREVHUSB + 1, df_NSFG_2022_23$PREVHUSB)
  datos_UH$nCohab <- ifelse(!is.na(datos_UH$yCurrCohab), df_NSFG_2022_23$PREVCOHB + 1, df_NSFG_2022_23$PREVCOHB)
  datos_UH$nUnion <- datos_UH$nMarriage + datos_UH$nCohab
  datos$nUnion <- datos_UH$nUnion
  
  useCompleteUH_algorithm = TRUE
  if (isTRUE (useCompleteUH_algorithm)) {
    datos_UH1 <- raw_union_history(df=df_NSFG_2022_23, names2022_23=TRUE, datos=datos_UH, survey_cmc = datos$surveyDate_cmc, capped = capMonth)
  } else {
    datos_UH1 <- firstMarriage_or_firstCohabitation (datos_UH, df_NSFG_2022_23)
   }
  # join datos and datos_BH
  datos <- datos %>%
    left_join(datos_UH1, by = "CaseID") %>%
    # Replace NA in total_unions with 0 for women who never had a union
    mutate(total_unions = replace_na(total_unions, 0))
  
  #clean
  datos$nUnion <- datos$total_unions
  datos$total_unions <- NULL

  datos <- dplyr::bind_cols(datos, nsfg_husb_priorKids(
    df_NSFG_2022_23,
    kidshx_names = c("KIDSHX_1", "KIDSHX_2", "KIDSHX_3", "KIDSHX_4", "KIDSHX_5")
  ))

  ##### Birth History #####
  datos$nBioKids <- df_NSFG_2022_23$PARITY
  # check the parity and the number of live births is equal
  ###### count of live births #####
  df_birth_counts <- df_NSFG_2022_23_preg %>%
    filter(OUTCOME == 1) %>%
    group_by(CaseID) %>%
    summarize(total_live_births = sum(BORNALIV, na.rm = TRUE))
  
  datos <-  datos %>%
    left_join(df_birth_counts, by="CaseID") %>%
    mutate(total_live_births = replace_na(total_live_births, 0))
  
  datos <- relocate(datos, total_live_births, .after=nBioKids)
  datos$nBioKids <- NULL
  datos <- rename (datos, nBioKids=total_live_births)
  
  ###### Pregnancies ######
  children <- live_birth_history (df_NSFG_2022_23_preg, capped = capMonth) 
  
  ###### final women dataframe with live births ######
  datos <- datos %>%
    left_join(children, by = "CaseID")
  
  datos <- compute_lastYear(datos)
  
  return (zap_label (datos))
}

NSFG_ENADID_2022_23 <- getDatos_2022_23()
NSFG_ENADID_2022_23 <- cleanENADID(NSFG_ENADID_2022_23)
NSFG_ENADID_2022_23 <-  reorder_birthHistory(NSFG_ENADID_2022_23)

pathNSFG_ENADID_2022_23 <- path.expand(paste0(dataPath, "NSFG_ENADID_2022_23.Rdat"))
save(NSFG_ENADID_2022_23, file = pathNSFG_ENADID_2022_23)

surveys_list <- list(
  NSFG_ENADID_1973, NSFG_ENADID_1976, NSFG_ENADID_1982, NSFG_ENADID_1988,
  NSFG_ENADID_1995, NSFG_ENADID_2002, NSFG_ENADID_2006_10,
  NSFG_ENADID_2011_13, NSFG_ENADID_2013_15, NSFG_ENADID_2015_17,
  NSFG_ENADID_2017_19, NSFG_ENADID_2022_23
)

#### harmonize fields ####
surveys_list <- lapply(surveys_list, harmonize_survey_types)

#This one was to unify with the MEXICO which has "don't know" (2 cases) when NSFG has "unknown" "refused" "don't remember" (0 case)
#NSFG_ENADID <- harm_union_type (NSFG_ENADID, 10)

res <- do.call(check_bind_conflicts, surveys_list)
NSFG_ENADID <- dplyr::bind_rows(surveys_list)

checkImputedMonth (NSFG_ENADID)

#### reweight the surveys ####
# We use two kinds of weights:
# 1. *weight* is the original weight of each individual in the survey DIVIDED by the mean value of the weights in the survey,
# so that the mean of indiv_weight is 1 for each survey. This way, we keep the relative weights of individuals within each survey
# We use indiv_weight when computing variance and confidence intervals of indicators, as it reflects the sampling design of the survey.
# 2. *popWeigth* is the multiplier that we apply to each individual weight,
# so that the sum of the reweighted weights (weight * popWeigth) for each survey matches
# the population counts by age in the year of the survey.
# This way, we ensure that the total weighted count of individuals in each age group matches the population counts.
# We use popWeigth for computing the point estimates of indicators, as a pooling mechanism: it ensures that smaller surveys effectively balance larger ones

# 1. Calculate *weight* by dividing the original weight (variable indiv_weight) by the mean weight of the survey
NSFG_ENADID <- reweight (NSFG_ENADID)

# 2. Calculate *popWeigth* by multiplying *weight* by the division of the population counts by age in the year of the survey by the sum of *weight* for each age group in the survey
PopAgeUSA <- function () {
  return (
    structure(list(
      Age = 14:54,
      NSFG1973 = c(2120455, 2122105, 2106999, 
                   2090124, 2059452.5, 2015098, 1967575, 1911203, 1858779.5, 1833648, 
                   1833982, 1857469, 1820539, 1620074, 1484594.5, 1516379.5, 1518572, 
                   1428170, 1322869.5, 1273046.5, 1243285, 1211726, 1185179.5, 1182733.5, 
                   1184383.5, 1162237.5, 1158203, 1179306.5, 1196631.5, 1209044.5, 
                   1217076.5, 1240899, 1251665, 1264562, 1284753, 1279636, 1276590, 
                   1275743, 1247082.5, 1211688.5, 1192191),
      NSFG1976 = c(2102110.5, 
                   2132402.5, 2134751.5, 2146494.5, 2149496, 2135743.5, 2119697.5, 
                   2089488, 2045186, 1997971, 1941940.5, 1889899, 1863984.5, 1862508, 
                   1883660.5, 1844396, 1641676, 1503583.5, 1531976, 1531057, 1438250, 
                   1331134.5, 1279754.5, 1248436, 1215103.5, 1187108, 1183372.5, 
                   1184014.5, 1161069.5, 1156169, 1176127.5, 1192448.5, 1204451.5, 
                   1212447, 1235852.5, 1246130, 1258145.5, 1277100, 1270495, 1265447.5, 
                   1262465),
      NSFG1982 = c(1743300.5, 1792339, 1869101, 1973909.5, 
                   2054481, 2097560.5, 2151502, 2182179.5, 2184098.5, 2195227.5, 
                   2198131.5, 2184321.5, 2167883, 2136876, 2090894.5, 2040967.5, 
                   1981891.5, 1925705.5, 1895773, 1889657, 1905779.5, 1861431.5, 
                   1654853.5, 1513534, 1538305.5, 1534188.5, 1439025.5, 1329534, 
                   1276636, 1243918, 1209317, 1180089, 1174269, 1173721.5, 1150709, 
                   1145065.5, 1163749.5, 1178591, 1188893, 1194185, 1213622),
      NSFG1988 = c(1579281, 
                   1612838, 1720427, 1839481.5, 1855823, 1813130.5, 1812801, 1861101.5, 
                   1934998, 2035985.5, 2112339.5, 2151662.5, 2202219, 2229629, 2228890, 
                   2237356.5, 2237150, 2219363, 2199039, 2163617.5, 2113483, 2059207, 
                   1995541.5, 1935183, 1900387.5, 1889808.5, 1901658.5, 1853545.5, 
                   1645262, 1502094, 1524155, 1518043.5, 1421762.5, 1312484.5, 1259029.5, 
                   1225795, 1190661.5, 1160169, 1152912, 1149952, 1124375),
      NSFG1995 = c(1850788, 
                   1835843.5, 1794011.5, 1767710.5, 1736442, 1704145.5, 1710769.5, 
                   1718852, 1749239.5, 1851315.5, 1963673.5, 1973464, 1924232, 1916843.5, 
                   1957801, 2025105.5, 2119801.5, 2189621, 2222580, 2266511.5, 2286615, 
                   2277857, 2278070, 2268565.5, 2241555, 2212174.5, 2169018.5, 2112887, 
                   2053094.5, 1984986.5, 1920370, 1881165.5, 1867289, 1876325, 1826234, 
                   1618260, 1474975, 1494597, 1486084, 1390243, 1281317),
      NSFG2002 = c(2020307, 
                   2070647.5, 2124634, 2172683, 2172633.5, 2135632.5, 2105993, 2085678, 
                   2087763.5, 2086202.5, 2078094, 2092617, 2098579.5, 2091265, 2072464.5, 
                   2024275, 1989719, 1948580.5, 1905897.5, 1901725.5, 1898925, 1918175, 
                   2007906.5, 2107779.5, 2105936.5, 2045638, 2026745.5, 2055968.5, 
                   2111340, 2192573.5, 2248739, 2268583, 2299627, 2308328.5, 2290429, 
                   2281806.5, 2264060, 2229883, 2194189.5, 2145209.5, 2083546), 
      NSFG2006_10 = c(2049500.5, 2029417, 2022979.5, 2028301, 2050944, 
                      2085638, 2132040.5, 2184138.5, 2239150.5, 2287124, 2285972.5, 
                      2247326.5, 2215953.5, 2193568, 2193173, 2188341, 2176144.5, 
                      2185589.5, 2186155, 2173630.5, 2149844, 2097009, 2057754, 
                      2011728, 1963847, 1954253.5, 1946223, 1960401, 2044784, 2139215.5, 
                      2132506.5, 2068141.5, 2045507.5, 2070760, 2121855, 2198552, 
                      2250418.5, 2266403, 2292990.5, 2297131, 2274592),
      NSFG2011_13 = c(2068926.5, 
                      2092201, 2086630, 2067202.5, 2061329.5, 2067208.5, 2090256.5, 
                      2125143, 2171555.5, 2223582.5, 2278598.5, 2326350.5, 2324548, 
                      2284918, 2252270, 2228333, 2226186, 2219484.5, 2205392, 2213038.5, 
                      2211670.5, 2197116, 2171301.5, 2116408.5, 2075197, 2027627, 
                      1978557.5, 1967498, 1957401, 1969457, 2051963.5, 2144618.5, 
                      2136683, 2071325, 2047518.5, 2071375.5, 2120661, 2195274.5, 
                      2244935, 2258719, 2282926),
      NSFG2013_15 = c(2111446, 2104195, 
                      2108006, 2131998.5, 2127053.5, 2108210.5, 2102775, 2108886, 
                      2131961, 2166761, 2213113.5, 2264909, 2319223.5, 2365822.5, 
                      2362545, 2321219, 2286814, 2260980, 2256788, 2248190, 2232104, 
                      2237542.5, 2234050.5, 2217293.5, 2189420, 2132889, 2090270.5, 
                      2041316.5, 1990412, 1977313, 1965611.5, 1976266.5, 2057156, 
                      2148021, 2138935, 2072862.5, 2047711, 2069735.5, 2116977, 
                      2189188, 2236209),
      NSFG2015_17 = c(2208333, 2216412, 2183850.5, 
                      2156823, 2153262.5, 2146420.5, 2150565.5, 2174793, 2169968.5, 
                      2151115.5, 2145526.5, 2151416.5, 2174098.5, 2208335, 2253672.5, 
                      2303967.5, 2356381, 2400792.5, 2395188, 2351573.5, 2314827, 
                      2286821.5, 2280358, 2269279.5, 2250714, 2253531.5, 2247544.5, 
                      2228617, 2198609.5, 2140170.5, 2095592.5, 2044455.5, 1991673, 
                      1976504.5, 1962822.5, 1971290, 2049211.5, 2136816, 2125059.5, 
                      2056906, 2029010),
      NSFG2017_19 = c(2171850.5, 2216541, 2224421, 
                      2191677, 2164483, 2160765, 2153870, 2158004.5, 2182131.5, 
                      2177118.5, 2158156.5, 2152546, 2158421.5, 2181127, 2215432, 
                      2260842.5, 2311146, 2363524.5, 2407774.5, 2401834.5, 2358024.5, 
                      2321157.5, 2293024, 2286405.5, 2275071, 2256292, 2258935.5, 
                      2252780.5, 2233628, 2203202, 2144396, 2099365.5, 2047733, 
                      1994622, 1978998, 1964899.5, 1973138.5, 2050619.5, 2137275, 
                      2124591, 2055785.5),
      NSFG2022_23 = c(2208333, 2216412, 2183850.5, 
                      2156823, 2153262.5, 2146420.5, 2150565.5, 2174793, 2169968.5, 
                      2151115.5, 2145526.5, 2151416.5, 2174098.5, 2208335, 2253672.5, 
                      2303967.5, 2356381, 2400792.5, 2395188, 2351573.5, 2314827, 
                      2286821.5, 2280358, 2269279.5, 2250714, 2253531.5, 2247544.5, 
                      2228617, 2198609.5, 2140170.5, 2095592.5, 2044455.5, 1991673, 
                      1976504.5, 1962822.5, 1971290, 2049211.5, 2136816, 2125059.5, 
                      2056906, 2029010)),
      class = "data.frame", row.names = c(NA,  -41L))
    )
}

popUSA <- PopAgeUSA()

NSFG_ENADID <- addWeights (NSFG_ENADID, popUSA)
rm(popUSA)

#### IMPUTATION OF DATE OF END OF UNION FOR THE 2000 SURVEY ####
# As noted by Ruggles and Kennedy (2015) two sets of women had no end of union date:
# "Marital dissolution data are missing entirely for currently separated respondents,
# resulting in very high rates of missing data for marriages that dissolved in periods close to the survey administration.
# In addition, marriages in which the male partner had children from a prior union were also skipped out of the marriage dissolution questions."
# The women in the first set are 229 cases where RMARITAL = 5 (separated) and no date of end of union 
# The women in the second set

##### imputation-prep: per-slot target flags for the 2002 dissolution gaps #####
# Run AFTER select() (they add new columns) and AFTER cleanENADID (they read the
# finalized union_end NA pattern). Defined in NSFG_impute_dissolution.R (sourced
# at the top of this file).
NSFG_ENADID <- add_union_husb_priorKids(NSFG_ENADID)   # issue 2: husband had prior children
NSFG_ENADID <- add_union_sep_recent(NSFG_ENADID)       # issue 1: currently-separated unions

##### imputation of missing dates of end of union for the 2002 survey fitting a model to the 2006-10 survey data #####
train <- build_dissolution_training(NSFG_ENADID)
fit   <- fit_dissolution_model(train, dist = "loglogistic")
set.seed(1)
NSFG_ENADID <- impute_sep_recent(NSFG_ENADID, fit)
NSFG_ENADID <- impute_husb_priorKids(NSFG_ENADID, fit)

chk <- check_imputation(NSFG_ENADID, fit)   # n_rep, propagate, seed all optional
chk$summary

# --- Date-quality summary over the FINAL data (3 headline stats) ---
# Runs AFTER the 2006-10 model so its flags 41/42 are visible. Columns:
#   removed_union   women with bad, un-repaired union dates
#                   (= filterDateQuality(dropBadUnionDates = TRUE), the default)
#   corrected_union union end repaired by a later union (20/21/22), the divorce
#                   date (30/31) or the 2006-10 model (41/42)
#   removed_birth   women with bad birth dates
#                   (= filterDateQuality(dropBadBirthDates = TRUE))
#   removed_bad_age impossible age (always removed)
DATE_QUALITY_SUMMARY <- summarizeDateQuality(NSFG_ENADID)

#### clean dataframe and reorder fields ####
selColumns <- c(
  "country","survey","surveyDate_cmc","indiv_dob_cmc","indiv_dob_cmc_I","indiv_age_survey","yBirth",
  "indiv_weight", "weight", "popWeight",
  "nBioKids","nUnion","union_status","lastYear","llave_muj",
  "pregnant","pregnant_wanted","pregnant_want_another","pregnant_ideal_number",
  "nullipar_fecund","nullipar_want_another","nullipar_ideal_number",
  "mother_fecund","mother_want_another","mother_ideal_number","mother_less","mother_unwanted",
  "motive_no_child","ideal_number","want_another",
  "age_first_sex","ever_had_sex","ever_contraception"
)

for (u in (1:10)) {
  selColumns <- c(selColumns, paste0("union_start_type",u),
                  paste0("union_start_cmc",u),
                  paste0("union_start_cmc_I",u),
                  paste0("marriage_start_cmc",u),
                  paste0("marriage_start_cmc_I",u),
                  paste0("union_end_cmc",u),
                  paste0("union_end_cmc_I",u),
                  paste0("union_end_motive",u)
  )
}
for (b in (1:17)) {
  selColumns <- c(selColumns,
                  paste0("dob_cmc",b),
                  paste0("dob_cmc_I",b),
                  paste0("dod_cmc",b),
                  paste0("sex",b)
  )
}

NSFG_ENADID <- NSFG_ENADID %>%
  select(any_of(selColumns))


#### save NSFG_ENADID ####
pathNSFG_ENADID <- path.expand(paste0(dataPath, "NSFG_ENADID.Rdat"))
save(NSFG_ENADID, file = pathNSFG_ENADID)

print (DATE_QUALITY_SUMMARY)
