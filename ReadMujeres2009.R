setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#### Read ENADID 2009 ####
library (tidyverse)
library (foreign)
library(janitor)
library(bit64) # Necessary for the integer64 type

path_mujer1 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2009/base_datos_enadid09_dbf/tr_cmu.dbf"))
path_mujer2 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2009/base_datos_enadid09_dbf/tr_smi.DBF"))
path_hogar <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2009/base_datos_enadid09_dbf/tr_viv_hog.dbf"))
path_embarazos <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2009/base_datos_enadid09_dbf/tr_fec_hemb.dbf"))
path_ENADID2009 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2009.Rdat"))
path_ENADID2009_mujeres <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2009_mujeres.Rdat"))
# read everything as character
mujer1 <- foreign::read.dbf (path_mujer1)
mujer2 <- foreign::read.dbf (path_mujer2)
embarazos <- foreign::read.dbf (path_embarazos)
hogar <- foreign::read.dbf (path_hogar)

llave_muj64 <- function(df) {
  # no key in ENADID2009. We build it here. We will leave it as character, no conversion to integer64
  #df$llave <- paste0(df$control, df$viv_sel, df$hogar, df$n_ren)
  return (
    df <- df %>%
      # 1. Clean the key column (LLAVE) as before
      dplyr::mutate(
        llave_muj = stringr::str_remove_all(LLAVE, "[^0-9]")
      ) %>%
      # 2. PERFORM TYPE-SPECIFIC CONVERSIONS
      dplyr::mutate(
        across(
          # Select all columns EXCEPT LLAVE
          .cols = -llave_muj,
          .fns = ~suppressWarnings(
            case_when(
              # RULE 1: If the column is a FACTOR
              is.factor(.) ~ as.integer(as.character(.)),
              
              # RULE 2: If the column is CHARACTER (and not factor)
              is.character(.) ~ as.integer(.),
              
              # RULE 3: For all other types (numeric, logical, etc.), keep the value
              TRUE ~ as.integer(.)
            )
          )
        ),
        # 3. Convert the key column (LLAVE)
        #llave_muj = bit64::as.integer64(LLAVE) 
      )
    )
}

mujer1 <- llave_muj64 (mujer1)
mujer2 <- llave_muj64 (mujer2)
embarazos <- llave_muj64 (embarazos)

# check the llave_muj key is OK
if (any(is.na(mujer1$llave_muj))) stop ("mujer1 has NA in LLAVE")
if (any(is.na(mujer2$llave_muj))) stop ("mujer2 has NA in LLAVE")
if (any(is.na(embarazos$llave_muj))) stop ("embarazos has NA in LLAVE")

dup_mujer1 <- mujer1 %>% get_dupes(llave_muj)
dup_mujer2 <- mujer2 %>% get_dupes(llave_muj)
if (nrow(dup_mujer1) > 0) stop ("mujer1 has duplicate keys")
if (nrow(dup_mujer2) > 0) stop ("mujer2 has duplicate keys")
rm(dup_mujer1)
rm(dup_mujer2)

# check each row of mujer1 has a corresponding row of mujer2
keys_only_in_mujer1 <- setdiff(mujer1$llave_muj, mujer2$llave_muj)
keys_only_in_mujer2 <- setdiff(mujer2$llave_muj, mujer1$llave_muj)
if (!((length(keys_only_in_mujer1) == 0 && length(keys_only_in_mujer2) == 0))) stop(paste("FAILURE: mujer1 is missing", length(keys_only_in_mujer2), "keys, and mujer2 is missing", length(keys_only_in_mujer1), "keys."))

# reduce columns
columns_to_drop <- c("control","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog")
mujer1 <- mujer1[,!(colnames(mujer1) %in% columns_to_drop)]
mujer2 <- mujer2[,!(colnames(mujer2) %in% columns_to_drop)]
embarazos <- embarazos[,!(colnames(embarazos) %in% columns_to_drop)]

mujeres2009 <- dplyr::full_join(mujer1, mujer2, by = "llave_muj")
rm(mujer1)
rm(mujer2)

getDatos2009 <- function (mujeres) {
  datos <- data.frame(llave_muj=mujeres$llave_muj, region=mujeres$ENT.x)
  datos$survey <- "ENADID2009"
  datos$surveyDate_cmc <- compute_cmc(10, 2009) # date of survey is from "del 4 de agosto al 26 de septiembre de 2009". We cut at October
  datos$monthBirth <- mujeres$P5_1_1
  datos$yearBirth <- mujeres$P5_1_2
  datos$age <- mujeres$P5_2_1.x
  datos$nLiveBirths <- mujeres$P5_6
  datos$nLiveBirths <- ifelse(is.na(datos$nLiveBirths),0,datos$nLiveBirths)
  datos$indiv_weight <- mujeres$FAC_MUJER.x
  datos$pregnant <- mujeres$P6_1
  datos$pregnant <- factor(datos$pregnant, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  datos$pregnant_wanted <- mujeres$P6_2
  datos$pregnant_wanted <- factor(datos$pregnant_wanted, levels = c(1, 2, 3, 9), labels = c("yes", "yes but later", "no","don't know"))
  datos$pregnant_want_another <- mujeres$P6_3
  datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths > 0), mujeres$P6_11, NA)
  datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths == 0), mujeres$P6_12, datos$pregnant_ideal_number)
  
  datos$nullipar_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$P6_12, NA)
  datos$mother_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$P6_11, NA)
  datos$nullipar_want_another <- mujeres$P6_9
  datos$nullipar_want_another <- factor(datos$nullipar_want_another, levels = c(1, 2, 9),
                                        labels = c("yes", "no", "don't know"))
  datos$nullipar_fecund <- NA
  datos$mother_want_another <- mujeres$P6_7
  datos$mother_want_another <- factor(datos$mother_want_another, levels = c(1, 2, 9),
                                      labels = c("yes", "no", "don't know"))
  datos$mother_fecund <- NA
  datos$mother_unwanted <- NA
  datos$mother_less <- NA
  datos$want_another <- NA
  datos$ideal_number <- NA
  datos$motive_no_child <- NA

  datos$ever_contraception <- mujeres$P7_2
  datos$ever_contraception <- factor(datos$ever_contraception, levels = c(1, 2, 9),
                                     labels = c("yes", "no","don't know"))
  datos$age_first_sex <- mujeres$P7_34
  
  datos$ever_had_sex <- ifelse(datos$age_first_sex == 88,1,2)
  datos$ever_had_sex <- ifelse(datos$age_first_sex %in% c(98,99),9,datos$ever_had_sex)
  
  datos$union_status <- mujeres$P9_1
  datos$union_status <- factor(datos$union_status, levels = seq(0,9,1),
                               labels = c("ever united", "cohabitation", "separated cohabitation", "separated marriage",
                                          "divorced", "widow cohabitation", "widow marriage", "married", "single", "don't know"))
  datos$lastUnion_month_start <- mujeres$P9_3_1
  datos$lastUnion_year_start <- mujeres$P9_3_2
  datos$lastUnion_month_end <- mujeres$P9_2_1
  datos$lastUnion_year_end <- mujeres$P9_2_2
  datos$lastUnion_cohab_before <- mujeres$P9_5
  datos$lastUnion_cohab_before <- factor(datos$lastUnion_cohab_before, levels = c(1,2,9),
                                         labels = c("yes", "no", "don't know"))
  datos$lastUnion_month_cohab_before <- mujeres$P9_6_1
  datos$lastUnion_year_cohab_before <- mujeres$P9_6_2
  datos$nUnionBeforeLast <- mujeres$P9_8
  datos$firstUnionNotLast_month_start <- mujeres$P9_9_1
  datos$firstUnionNotLast_year_start <- mujeres$P9_9_2
  datos$firstUnionNotLast_month_end <- mujeres$P9_11_1
  datos$firstUnionNotLast_year_end <- mujeres$P9_11_2
  datos$firstUnionNotLast_end_motive <- mujeres$P9_10
  datos$firstUnionNotLast_end_motive <- factor(datos$firstUnionNotLast_end_motive, levels = c(1,2,3,9),
                                               labels = c("separation", "widowhood", "divorce","don't know"))
  datos$firstUnionNotLast_type <- mujeres$P9_12
  datos$firstUnionNotLast_type <- factor(datos$firstUnionNotLast_type, levels = c(1,2,9),
                                         labels = c("cohabitation", "marriage","don't know"))
  datos$firstUnionNotLast_cohab_before <- mujeres$P9_13
  datos$firstUnionNotLast_cohab_before <- factor(datos$firstUnionNotLast_cohab_before, levels = c(1,2,9),
                                                 labels = c("yes", "no","don't know"))
  datos$firstUnionNotLast_cohab_month <- mujeres$P9_14_1
  datos$firstUnionNotLast_cohab_year <- mujeres$P9_14_2
  
  return (datos)
}

# create a big dataframe
ENADID2009 <- bigDataWomen( getDatos2009(mujeres2009))

# Childbirths
bigDataChildbirths <- function (embarazos) {
  ENADID_P <- data.frame(llave_muj=embarazos$llave_muj, sex=embarazos$P5_8)
  ENADID_P$dob_cmc <- compute_cmc(embarazos$P5_13_1, embarazos$P5_13_2)
  ENADID_P$dob_cmc_I <- imputed_date(embarazos$P5_13_1, embarazos$P5_13_2)
  ENADID_P$sex_dead <- embarazos$P5_11
  ENADID_P$orden <- embarazos$ORDENHNV
  ENADID_P$sex <- ifelse(is.na(ENADID_P$sex)&(!is.na(ENADID_P$orden)),ENADID_P$sex_dead,ENADID_P$sex)
  ENADID_P$sex_dead <- NULL
  ENADID_P$stillbirth_month <- embarazos$P5_16
  ENADID_P$signOfLife <- embarazos$P5_17
  ENADID_P$abortion_month <- embarazos$P5_20
  ##### date of death #####
  # number of days lived for children who lived less than one month
  ENADID_P$dod_cmc <- NA
  ENADID_P$dod_cmc_I <- NA
  idx <- !is.na(embarazos$P5_12_3)
  n_dead <- sum(idx)
  probs <- pmin(embarazos$P5_12_3[idx] / 30.44, 1)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + rbinom(n_dead, 1, probs)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P5_12_3[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P5_12_3[idx] == 99, 1L, 1L)
  # number of months lived for children who lived less than one year
  idx <- !is.na(embarazos$P5_12_2)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P5_12_2[idx] + sample(c(0,1), n_dead, replace = TRUE) # we add 0.5 month on average to avoid all children dying at the end of the month
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P5_12_2[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P5_12_2[idx] %in% c(88, 99), 1L, 1L)
  # number of years lived for children who lived more than one year
  idx <- !is.na(embarazos$P5_12_1)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P5_12_1[idx] * 12 + sample(0:11, n_dead, replace = TRUE)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P5_12_1[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P5_12_1[idx] %in% c(88, 99), 10L, 1L)
  
  ENADID_P <- subset(ENADID_P,!is.na(orden))
  
  ENADID_P_wide <- ENADID_P %>%
    dplyr::arrange (llave_muj) %>%
    dplyr::group_by(llave_muj) %>%
    dplyr::mutate(
      # Check if all orden are 99 and all dob_cmc are non-negative
      use_dob = all(orden == 99) & all(dob_cmc >= 0),
      
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
  
  missing_keys <- setdiff(ENADID_P_wide$llave_muj, ENADID2009$llave_muj)
  if (length(missing_keys) > 0) stop (paste("ENADID_P_wide is missing", length(missing_keys), "keys from ENADID"))
  
  return (ENADID_P_wide)
}

ENADID_P <- bigDataChildbirths (embarazos)

#### append the birth histories ####
ENADID2009_full <- dplyr::left_join(ENADID2009, ENADID_P, by = "llave_muj")
ENADID2009_full$nChildren[is.na(ENADID2009_full$nChildren)] <- 0
if (!all.equal(ENADID2009_full$nChildren, ENADID2009_full$nBioKids)) stop ("nBioKids and nChildren do not match")
rm(embarazos)
rm(ENADID_P)
rm(ENADID2009)

ENADID2009_full <- cleanENADID(ENADID2009_full)
ENADID2009_full <- reorder_birthHistory(ENADID2009_full)

save(ENADID2009_full, file=path_ENADID2009)
save(mujeres2009, file=path_ENADID2009_mujeres)

