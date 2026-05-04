setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#### Read ENADID 2023 ####
library (tidyverse)
library(janitor)
library(bit64) # Necessary for the integer64 type

path_mujer1 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer1_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer1_enadid_2023.csv"))
path_mujer2 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer2_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer2_enadid_2023.csv"))
path_embarazos <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tfechisemb_enadid_2023/conjunto_de_datos/conjunto_datos_tfechisemb_enadid_2023.csv"))
path_ENADID2023 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2023.Rdat"))
path_ENADID2023_mujeres <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID2023_mujeres.Rdat"))
# read everything as character
mujer1 <- readr::read_csv(path_mujer1, col_types = cols(.default = "c"))
mujer2 <- readr::read_csv(path_mujer2, col_types = cols(.default = "c"))
embarazos <- readr::read_csv(path_embarazos, col_types = cols(.default = "c"))

llave_muj64 <- function(df) {
  # there is a problem with big keys, so we reconstruct them
  df$llave_muj <- paste0(df$upm, df$viv_sel, df$hogar, df$n_ren)
  return (
    df %>%
      # 1. CLEAN THE KEY COLUMN FIRST
      dplyr::mutate(
        # Remove any character that is NOT a digit (0-9). 
        # This handles commas, spaces, etc., that might be present.
        llave_muj = stringr::str_remove_all(llave_muj, "[^0-9]")
      ) %>%
      # 2. PERFORM THE CONVERSION
      dplyr::mutate(
        across(
          .cols = -llave_muj,
          .fns = ~suppressWarnings(as.integer(.))
        ),
        llave_muj = as.integer64(llave_muj)
      )
  )
}

mujer1 <- llave_muj64 (mujer1)
mujer2 <- llave_muj64 (mujer2)
embarazos <- llave_muj64 (embarazos)

# check the llave_muj key is OK
if (any(is.na(mujer1$llave_muj))) stop ("mujer1 has NA in llave_muj")
if (any(is.na(mujer2$llave_muj))) stop ("mujer2 has NA in llave_muj")
if (any(is.na(embarazos$llave_muj))) stop ("embarazos has NA in llave_muj")

if (nrow(mujer1 %>% get_dupes(llave_muj)) > 0) stop ("mujer1 has duplicate keys")
if (nrow(mujer2 %>% get_dupes(llave_muj)) > 0) stop ("mujer2 has duplicate keys")

# check each row of mujer1 has a corresponding row of mujer2
keys_only_in_mujer1 <- setdiff(mujer1$llave_muj, mujer2$llave_muj)
keys_only_in_mujer2 <- setdiff(mujer2$llave_muj, mujer1$llave_muj)
if (!((length(keys_only_in_mujer1) == 0 && length(keys_only_in_mujer2) == 0))) stop(paste("FAILURE: mujer1 is missing", length(keys_only_in_mujer2), "keys, and mujer2 is missing", length(keys_only_in_mujer1), "keys."))

# reduce columns
columns_to_drop <- c("upm","viv_sel","hogar","n_ren","renglon","llave_viv","llave_hog")
mujer1 <- mujer1[,!(colnames(mujer1) %in% columns_to_drop)]
mujer2 <- mujer2[,!(colnames(mujer2) %in% columns_to_drop)]
embarazos <- embarazos[,!(colnames(embarazos) %in% columns_to_drop)]

# create a big dataframe
mujeres2023 <- dplyr::full_join(mujer1, mujer2, by = "llave_muj")
rm(mujer1)
rm(mujer2)

getDatos2023 <- function (mujeres) {
  datos <- data.frame(llave_muj=mujeres$llave_muj, region=mujeres$ent.x)
  datos$survey <- "ENADID2023"
  datos$surveyDate_cmc <- compute_cmc(11, 2023)# date of survey is from "14 de agosto al 6 de octubre de 2023". We cut at November 2023  datos$monthBirth <- mujeres$p5_1_1
  datos$monthBirth <- mujeres$p5_1_1
  datos$yearBirth <- mujeres$p5_1_2
  datos$age <- mujeres$p5_2_1
  datos$nLiveBirths <- mujeres$p5_9
  datos$nLiveBirths <- ifelse(is.na(datos$nLiveBirths),0,datos$nLiveBirths)
  datos$indiv_weight <- mujeres$fac_mod.x
  datos$pregnant <- mujeres$p7_1
  datos$pregnant <- factor(datos$pregnant, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  datos$pregnant_wanted <- mujeres$p7_2
  datos$pregnant_wanted <- factor(datos$pregnant_wanted, levels = c(1, 2, 3, 9), labels = c("yes", "yes but later", "no","don't know"))
  datos$pregnant_want_another <- mujeres$p7_3
  datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  
  datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths == 0), mujeres$p7_10, NA)
  datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths > 0), mujeres$p7_14, datos$pregnant_ideal_number)
  datos$nullipar_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$p7_10, NA)
  datos$mother_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$p7_14, NA)
  
  datos$nullipar_want_another <- mujeres$p7_7
  datos$nullipar_want_another <- factor(datos$nullipar_want_another, levels = c(1, 2, 3, 9),
                                        labels = c("yes", "yes but can't", "no", "don't know"))
  datos$nullipar_fecund <- mujeres$p7_9
  datos$nullipar_fecund <- factor(datos$nullipar_fecund, levels = c(1, 2, 3, 4, 5, 6, 9),
                                  labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  datos$mother_want_another <- mujeres$p7_11
  datos$mother_want_another <- factor(datos$mother_want_another, levels = c(1, 2, 3, 9),
                                      labels = c("yes", "yes but can't", "no", "don't know"))
  datos$mother_fecund <- mujeres$p7_13
  datos$mother_fecund <- factor(datos$mother_fecund, levels = c(1, 2, 3, 4, 5, 6, 9),
                                labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  datos$mother_unwanted <- mujeres$p7_17
  datos$mother_unwanted <- factor(datos$mother_unwanted, levels = c(seq(1,6,1),9),
                                  labels = c("no contraception", "contraception unknown", "contraception failure", "partner wanted", "religious", "other", "don't know"))
  datos$mother_less <- mujeres$p7_18
  datos$mother_less <- factor(datos$mother_less, levels = c(seq(1,7,1),9),
                              labels = c("want more", "no money", "studying", "health", "no partner", "sterilized", "other", "don't know"))
  datos$want_another <- mujeres$mashijos
  datos$want_another <- factor(datos$want_another, levels = c(1, 2, 3, 9), labels = c("yes", "yes but can't", "no", "don't know"))
  datos$ideal_number <- mujeres$idealh
  datos$motive_no_child <- mujeres$nohijos
  datos$motive_no_child <- factor(datos$motive_no_child, levels = c(1, 2, 3, 4, 5, 6, 9),
                                  labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  
  datos$ever_contraception <- mujeres$p8_3
  datos$ever_contraception <- factor(datos$ever_contraception, levels = c(1, 2, 9),
                                     labels = c("yes", "no","don't know"))
  datos$age_first_sex <- mujeres$p8_39
  
  datos$union_status <- mujeres$p10_1
  datos$union_status <- factor(datos$union_status, levels = seq(1,9,1),
                               labels = c("cohabitation", "separated cohabitation", "separated marriage", "divorced", "widow cohabitation", "widow marriage", "married", "single", "don't know"))
  datos$lastUnion_month_start <- mujeres$p10_4_1
  datos$lastUnion_year_start <- mujeres$p10_4_2
  datos$lastUnion_month_end <- mujeres$p10_3_1
  datos$lastUnion_year_end <- mujeres$p10_3_2
  datos$lastUnion_cohab_before <- mujeres$p10_6
  datos$lastUnion_cohab_before <- factor(datos$lastUnion_cohab_before, levels = c(1,2,9),
                                         labels = c("yes", "no", "don't know"))
  datos$lastUnion_month_cohab_before <- mujeres$p10_7_1
  datos$lastUnion_year_cohab_before <- mujeres$p10_7_2
  datos$nUnionBeforeLast <- mujeres$p10_9
  datos$firstUnionNotLast_month_start <- mujeres$p10_10_1
  datos$firstUnionNotLast_year_start <- mujeres$p10_10_2
  datos$firstUnionNotLast_month_end <- mujeres$p10_12_1
  datos$firstUnionNotLast_year_end <- mujeres$p10_12_2
  datos$firstUnionNotLast_end_motive <- mujeres$p10_11
  datos$firstUnionNotLast_end_motive <- factor(datos$firstUnionNotLast_end_motive, levels = c(1,2,3,9),
                                               labels = c("separation", "widowhood", "divorce","don't know"))
  datos$firstUnionNotLast_type <- mujeres$p10_13
  datos$firstUnionNotLast_type <- factor(datos$firstUnionNotLast_type, levels = c(1,2,9),
                                         labels = c("cohabitation", "marriage","don't know"))
  datos$firstUnionNotLast_cohab_before <- mujeres$p10_14
  datos$firstUnionNotLast_cohab_before <- factor(datos$firstUnionNotLast_cohab_before, levels = c(1,2,9),
                                                 labels = c("yes", "no","don't know"))
  datos$firstUnionNotLast_cohab_month <- mujeres$p10_15_1
  datos$firstUnionNotLast_cohab_year <- mujeres$p10_15_2
  
  return (datos)
}

ENADID2023 <- bigDataWomen ( getDatos2023 (mujeres2023) )

# Childbirths
bigDataChildbirths <- function (embarazos) {
  ENADID_P <- data.frame(llave_muj=embarazos$llave_muj, sex=embarazos$p5_12)
  ENADID_P$dob_cmc <- compute_cmc(embarazos$p5_17_1, embarazos$p5_17_2)
  ENADID_P$dob_cmc_I <- imputed_date(embarazos$p5_17_1, embarazos$p5_17_2)
  ENADID_P$sex_dead <- embarazos$p5_15
  ENADID_P$orden <- embarazos$ordenhnv
  ENADID_P$sex <- ifelse(is.na(ENADID_P$sex)&(!is.na(ENADID_P$orden)),ENADID_P$sex_dead,ENADID_P$sex)
  ENADID_P$sex_dead <- NULL
  ##### date of death #####
  # number of days lived for children who lived less than one month
  ENADID_P$dod_cmc <- NA
  ENADID_P$dod_cmc_I <- NA
  idx <- !is.na(embarazos$p5_16_1)
  n_dead <- sum(idx)
  probs <- pmin(embarazos$p5_16_1[idx] / 30.44, 1)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + rbinom(n_dead, 1, probs)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p5_16_1[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p5_16_1[idx] == 99, 10L, 1L)
  # number of months lived for children who lived less than one year
  idx <- !is.na(embarazos$p5_16_2)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$p5_16_2[idx] + sample(c(0,1), n_dead, replace = TRUE) # we add 0.5 month on average to avoid all children dying at the end of the month
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p5_16_2[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p5_16_2[idx] == 99, 10L, 1L)
  # number of years lived for children who lived more than one year
  idx <- !is.na(embarazos$p5_16_3)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$p5_16_3[idx] * 12 + sample(0:11, n_dead, replace = TRUE)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$p5_16_3[idx] > 50,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$p5_16_3[idx] == 99, 10L, 1L)
  
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
    
    # Reorder columns: llave_muj, nChildren, then birth data
    dplyr::select(llave_muj, nChildren, everything())
  
  missing_keys <- setdiff(ENADID_P_wide$llave_muj, ENADID2023$llave_muj)
  if (length(missing_keys) > 0) stop (paste("ENADID_P_wide is missing", length(missing_keys), "keys from ENADID2023"))
  
  return (ENADID_P_wide)
}

ENADID_P <- bigDataChildbirths (embarazos)

#### append the birth histories ####
ENADID2023_full <- dplyr::left_join(ENADID2023, ENADID_P, by = "llave_muj")
ENADID2023_full$nChildren[is.na(ENADID2023_full$nChildren)] <- 0
if (!all.equal(ENADID2023_full$nChildren, ENADID2023_full$nBioKids)) stop ("nBioKids and nChildren do not match")

ENADID2023_full <- cleanENADID(ENADID2023_full)
ENADID2023_full <- reorder_birthHistory(ENADID2023_full)

save(ENADID2023_full, file=path_ENADID2023)
rm(ENADID2023)
rm(ENADID_P)
save(mujeres2023, file=path_ENADID2023_mujeres)
