setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#### Read ENADID 2006 ####
library (tidyverse)
library (foreign)
library(janitor)
library(bit64) # Necessary for the integer64 type

path_mujer <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2006/ENADID06_Mujer.csv"))
path_embarazos <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2006/ENADID06_Fecundidad.csv"))
path_ENADID2006 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2006.Rdat"))
path_ENADID2006_mujer <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2006_mujer.Rdat"))
# read everything as character
mujeres2006 <- read.csv (path_mujer)
embarazos <- read.csv (path_embarazos)

# check the llave_muj key is OK
if (any(is.na(mujeres2006$claveres))) stop ("mujer has NA in LLAVE")
if (any(is.na(embarazos$claveres))) stop ("embarazos has NA in LLAVE")

dup_mujer <- mujeres2006 %>% get_dupes(claveres)
if (nrow(dup_mujer) > 0) stop ("mujer has duplicate keys")
rm(dup_mujer)

# all births have corresponding mother
if (!all(embarazos$claveres %in% mujeres2006$claveres)) {
  print ("some births have no mother...")
  print (setdiff(embarazos$claveres, mujeres2006$claveres))
} 

getDatos2006 <- function (mujer) {
  datos <- data.frame(llave_muj=mujer$claveres)
  datos$survey <- "ENADID2006"
  # date of interview
  inter_date <- as.Date(mujer$fecha1, format = "%d/%m/%y")
  # Extract as integers
  mujer$month <- as.integer(format(inter_date, "%m"))
  mujer$month [is.na(mujer$month)] <- 3 # for the few missing values, we impute with the month of the survey (March))]
  mujer$year  <- as.integer(format(inter_date, "%Y"))
  mujer$year [is.na(mujer$year)] <- 2006 # for the few missing values, we impute with the year of the survey (2006))
  datos$surveyDate_cmc <- compute_cmc(mujer$month, mujer$year) # date of survey is from "marzo de 2006". 
  datos$monthBirth <- mujer$p05d01m
  datos$yearBirth <- mujer$p05d01a
  datos$age <- mujer$p05d0201
  datos$nLiveBirths <- mujer$p05d06
  datos$nLiveBirths <- ifelse(is.na(datos$nLiveBirths),0,datos$nLiveBirths)
  datos$indiv_weight <- mujer$fac_muje
  datos$pregnant <- mujer$p07d02
  datos$pregnant <- factor(datos$pregnant, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  datos$pregnant_wanted <- mujer$p07d03
  datos$pregnant_wanted <- factor(datos$pregnant_wanted, levels = c(1, 2, 3, 9), labels = c("yes", "yes but later", "no","don't know"))
  datos$pregnant_want_another <- ifelse((datos$pregnant=="yes"),mujer$p07d04,NA) 
  datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  datos$pregnant_ideal_number <- ifelse((datos$pregnant=="yes"),mujer$p07d06 ,NA) 

  datos$nullipar_ideal_number <- ifelse((datos$nLiveBirths==0)&(datos$pregnant=="no"),mujer$p07d06,NA)
  datos$mother_ideal_number <- ifelse((datos$nLiveBirths>0)&(datos$pregnant=="no"),mujer$p07d06,NA)
  datos$nullipar_want_another <-ifelse((datos$nLiveBirths==0)&(datos$pregnant=="no"),mujer$p07d04,NA)
  datos$nullipar_want_another <- factor(datos$nullipar_want_another, levels = c(1, 2, 9),
                                        labels = c("yes", "no", "don't know"))
  datos$nullipar_fecund <- NA
  datos$mother_want_another <- ifelse((datos$nLiveBirths>0)&(datos$pregnant=="no"),mujer$p07d04,NA) 
  datos$mother_want_another <- factor(datos$mother_want_another, levels = c(1, 2, 9),
                                      labels = c("yes", "no", "don't know"))
  datos$mother_fecund <- NA
  datos$mother_unwanted <- NA
  datos$mother_less <- NA
  datos$want_another <- NA
  datos$ideal_number <- NA
  datos$motive_no_child <- NA

  datos$ever_contraception <- NA
  datos$ever_contraception <- factor(datos$ever_contraception, levels = c(1, 2, 9),
                                     labels = c("yes", "no","don't know"))
  datos$age_first_sex <- mujer$p10d01
  
  datos$ever_had_sex <- ifelse(datos$age_first_sex == 88,1,2)
  datos$ever_had_sex <- ifelse(datos$age_first_sex %in% c(98,99),9,datos$ever_had_sex)
  
  datos$union_status <- mujer$p10d03
  datos$union_status <- factor(datos$union_status, levels = seq(1,8,1),
                               labels = c("cohabitation", "separated cohabitation", "separated marriage", "divorced", "widow cohabitation", "widow marriage", "married", "single"))
  datos$lastUnion_month_start <- mujer$p10d05m
  datos$lastUnion_year_start <- mujer$p10d05a
  datos$lastUnion_month_end <- mujer$p10d04m
  datos$lastUnion_year_end <- mujer$p10d04a
  datos$lastUnion_cohab_before <- mujer$p10d07
  datos$lastUnion_cohab_before <- factor(datos$lastUnion_cohab_before, levels = c(1,2),
                                         labels = c("yes", "no"))
  datos$lastUnion_month_cohab_before <- mujer$p10d08m
  datos$lastUnion_year_cohab_before <- mujer$p10d08a
  datos$nUnionBeforeLast <- mujer$p10d10
  datos$nUnionBeforeLast[is.na(datos$nUnionBeforeLast)] <- 0
  datos$firstUnionNotLast_month_start <- mujer$p10d11m
  datos$firstUnionNotLast_year_start <- mujer$p10d11a
  datos$firstUnionNotLast_month_end <- mujer$p10d13m
  datos$firstUnionNotLast_year_end <- mujer$p10d13a
  datos$firstUnionNotLast_end_motive <- mujer$p10d12
  datos$firstUnionNotLast_end_motive <- factor(datos$firstUnionNotLast_end_motive, levels = c(1,2,3,9),
                                               labels = c("separation", "widowhood", "divorce","don't know"))
  datos$firstUnionNotLast_type <- mujer$p10d14
  datos$firstUnionNotLast_type <- factor(datos$firstUnionNotLast_type, levels = c(1,2,9),
                                         labels = c("cohabitation", "marriage","don't know"))
  datos$firstUnionNotLast_cohab_before <- mujer$p10d15
  datos$firstUnionNotLast_cohab_before <- factor(datos$firstUnionNotLast_cohab_before, levels = c(1,2,9),
                                                 labels = c("yes", "no","don't know"))
  datos$firstUnionNotLast_cohab_month <- mujer$p10d16m
  datos$firstUnionNotLast_cohab_year <- mujer$p10d16a
  
  return (datos)
}

# create a big dataframe
ENADID2006 <- bigDataWomen( getDatos2006(mujeres2006))

# Childbirths
bigDataChildbirths <- function (embarazos) {
  ENADID_P <- data.frame(llave_muj=embarazos$claveres, sex=embarazos$sexovivo)
  ENADID_P$sex <- ifelse(!is.na(embarazos$sexofac), embarazos$sexofac, ENADID_P$sex)
  ENADID_P$dob_cmc <- compute_cmc(embarazos$p05d13m, embarazos$p05d13a)
  ENADID_P$dob_cmc_I <- imputed_date(embarazos$p05d13m, embarazos$p05d13a)
  ENADID_P$orden <- embarazos$renglon
  ##### date of death #####
  # number of days lived for children who lived less than one month
  ENADID_P$dod_cmc <- NA
  ENADID_P$dod_cmc_I <- NA
  idx <- !is.na(embarazos$p05d12d)
  n_dead <- sum(idx)
  probs <- pmin(embarazos$p05d12d[idx] / 30.44, 1)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + rbinom(n_dead, 1, probs)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p05d12d[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p05d12d[idx] == 99, 10L, 1L)
  # number of months lived for children who lived less than one year
  idx <- !is.na(embarazos$p05d12m)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$p05d12m[idx] + sample(c(0,1), n_dead, replace = TRUE) # we add 0.5 month on average to avoid all children dying at the end of the month
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p05d12m[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p05d12m[idx] == 99, 10L, 1L)
  # number of years lived for children who lived more than one year
  idx <- !is.na(embarazos$p05d12a)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$p05d12a[idx] * 12 + sample(0:11, n_dead, replace = TRUE)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p05d12a[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p05d12a[idx] == 99, 10L, 1L)
  
  ENADID_P <- subset(ENADID_P,!is.na(sex))
  
  ENADID_P_wide <- ENADID_P %>%
    dplyr::arrange (llave_muj) %>%
    dplyr::group_by(llave_muj) %>%
    dplyr::mutate(
      # Check if all orden are 99 and all dob_cmc are non-negative
      use_dob = all(dob_cmc >= 0),
      
      # Create temporary sorting variable
      sort_key = if_else(use_dob, dob_cmc, orden * 10000)
      # Multiply orden by 10000 to ensure it dominates when used
    ) %>%
    
    # Step 2: Sort by the sorting key
    dplyr::arrange(llave_muj, sort_key) %>%
    
    # Step 3: Create sequential birth number
    dplyr::mutate(birth_num = row_number()) %>%
    dplyr::ungroup()
  
  # Step 4: Remove temporary variables
  ENADID_P_wide <- ENADID_P_wide %>%
    select(-use_dob, -sort_key, -orden)  
  # Pivot to wide format
  ENADID_P_wide <- ENADID_P_wide %>%
    tidyr::pivot_wider(
      id_cols = llave_muj,
      names_from = birth_num,
      values_from = c(sex, dob_cmc, dob_cmc_I, dod_cmc, dod_cmc_I),
      names_sep = ""
    ) %>%
    
    # Count number of children
    dplyr::mutate(
      nChildren = rowSums(!is.na(select(., starts_with("sex"))))
    ) %>%
    
    # Reorder columns: LLAVE, nChildren, then birth data
    dplyr::select(llave_muj, nChildren, everything())
  
  missing_keys <- setdiff(ENADID_P_wide$llave_muj, ENADID2006$llave_muj)
  if (length(missing_keys) > 0) stop (paste("ENADID_P_wide is missing", length(missing_keys), "keys from ENADID"))
  
  return (ENADID_P_wide)
}

ENADID_P <- bigDataChildbirths (embarazos)

#### append the birth histories ####
ENADID2006_full <- dplyr::left_join(ENADID2006, ENADID_P, by = "llave_muj")
ENADID2006_full$nChildren[is.na(ENADID2006_full$nChildren)] <- 0
if (!isTRUE(all.equal(ENADID2006_full$nChildren, ENADID2006_full$nBioKids))) {
  print("nBioKids and nChildren do not match")
  ENADID2006_full$nBioKids <- ENADID2006_full$nChildren
}
rm(embarazos)
rm(ENADID_P)
rm(ENADID2006)

ENADID2006_full <- cleanENADID(ENADID2006_full)
ENADID2006_full <- reorder_birthHistory(ENADID2006_full)

save(ENADID2006_full, file=path_ENADID2006)
save(mujeres2006, file=path_ENADID2006_mujer)
