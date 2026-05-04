setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#### Read ENADID 1992 ####
library (tidyverse)
library (foreign)
library(janitor)
library(bit64) # Necessary for the integer64 type

path_mujer <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_1.DBF"))
path_embarazos <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_2.DBF"))
path_ENADID1992 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID1992.Rdat"))
path_ENADID1992_mujeres <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID1992_mujeres.Rdat"))
# read everything as character
mujeres1992 <- foreign::read.dbf (path_mujer, as.is = TRUE)
# Convert columns 9 to end to integer
mujeres1992 <- convertToInteger(mujeres1992, 9)
embarazos <- foreign::read.dbf (path_embarazos, as.is = TRUE)
# Convert columns 9 to end to integer
embarazos <- convertToInteger(embarazos, 9)

llave_muj64 <- function(df) {
  # no key in ENADID1992. We build it here. We will leave it as character, no conversion to integer64
  df$llave_muj <- paste0(df$IENT, df$ZONA, df$ESTRATO, df$CONTROL, df$IVIV, df$HOGAR, df$REG_MUJER)
  return (
    df <- df %>%
      # 1. Clean the key column (llave_muj) as before
      dplyr::mutate(
        llave_muj = stringr::str_remove_all(llave_muj, "[^0-9]")
      ) %>%
      # 2. PERFORM TYPE-SPECIFIC CONVERSIONS
      dplyr::mutate(
        across(
          # Select all columns EXCEPT llave_muj
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
        # 3. Convert the key column (llave_muj)
        #llave_muj = bit64::as.integer64(llave_muj) 
      )
    )
}

mujeres1992 <- llave_muj64 (mujeres1992)
embarazos <- llave_muj64 (embarazos)

# check the llave_muj key is OK
if (any(is.na(mujeres1992$llave_muj))) stop ("mujeres1992 has NA in llave_muj")
if (any(is.na(embarazos$llave_muj))) stop ("embarazos has NA in llave_muj")

dup_mujer <- mujeres1992 %>% get_dupes(llave_muj)
#dup_uniones <- uniones %>% get_dupes(llave_muj)
if (nrow(dup_mujer) > 0) stop ("mujeres1992 has duplicate keys")
rm(dup_mujer)
#if (nrow(dup_uniones) > 0) stop ("uniones has duplicate keys")

# check each row of embarazos has a corresponding row of mujeres
keys_only_in_embarazos <- setdiff(embarazos$llave_muj, mujeres1992$llave_muj)
if (!(length(keys_only_in_embarazos) == 0)) stop(paste("FAILURE: mujeres is missing", length(keys_only_in_embarazos), "keys"))

getDatos1992 <- function (mujeres) {
  datos <- data.frame(llave_muj=mujeres$llave_muj, region=mujeres$IENT)
  datos$survey <- "ENADID1992"
  datos$surveyDate_cmc <- compute_cmc(12, 1992)# date of survey is from "31 de agosto al 30 de noviembre de 1992". We cut at December 1992
  datos$monthBirth <- mujeres$P9_1A
  datos$yearBirth <- mujeres$P9_1B
  datos$age <- mujeres$P9_2
  datos$nLiveBirths <- mujeres$P9_10A
  datos$indiv_weight <- mujeres$FMUJ
  datos$pregnant <- mujeres$P9_11
  datos$pregnant <- factor(datos$pregnant, levels = c(5, 6, 8, 9), labels = c("yes", "no", "don't know", "Not specified"))
  datos$pregnant_wanted <- NA
  datos$pregnant_want_another <- NA
  datos$pregnant_ideal_number <- NA
  datos$nullipar_ideal_number <- NA
  datos$mother_ideal_number <- NA
  datos$nullipar_want_another <- NA
  datos$nullipar_fecund <- NA
  datos$mother_want_another <- NA
  datos$mother_fecund <- NA
  datos$mother_unwanted <- NA
  datos$mother_less <- NA
  datos$want_another <- NA
  datos$ideal_number <- NA
  datos$motive_no_child <- NA

  datos$ever_contraception <- NA
  datos$age_first_sex <- NA
  
  datos$union_status <- NA
  datos$lastUnion_month_start <- NA
  datos$lastUnion_year_start <- NA
  datos$lastUnion_month_end <- NA
  datos$lastUnion_year_end <- NA
  datos$lastUnion_cohab_before <- NA
  datos$lastUnion_month_cohab_before <- NA
  datos$lastUnion_year_cohab_before <- NA
  datos$nUnionBeforeLast <- NA
  datos$firstUnionNotLast_month_start <- NA
  datos$firstUnionNotLast_year_start <- NA
  datos$firstUnionNotLast_month_end <- NA
  datos$firstUnionNotLast_year_end <- NA
  datos$firstUnionNotLast_end_motive <- NA
  datos$firstUnionNotLast_type <- NA
  datos$firstUnionNotLast_cohab_before <- NA
  datos$firstUnionNotLast_cohab_month <- NA
  datos$firstUnionNotLast_cohab_year <- NA
  
  return (datos)
}

ENADID1992 <- bigDataWomen ( getDatos1992 (mujeres1992) )

# live births
bigDataChildbirths <- function (embarazos) {
  ENADID_P <- data.frame(llave_muj=embarazos$llave_muj, sex=embarazos$P9_15)
  # sex of dead children
  stopifnot(!any(!is.na(embarazos$P9_18) & !is.na(embarazos$P9_15)))
  ENADID_P$sex <- ifelse(!is.na(embarazos$P9_18), embarazos$P9_18, ENADID_P$sex)
  embarazos$P9_20B <- ifelse(embarazos$P9_20B == 99, 9999, embarazos$P9_20B)
  embarazos$P9_20B <- ifelse(embarazos$P9_20B %in% seq(47,92,1), embarazos$P9_20B + 1900, embarazos$P9_20B)
  ENADID_P$dob_cmc <- compute_cmc(embarazos$P9_20A, embarazos$P9_20B)
  ENADID_P$dob_cmc_I <- imputed_date(embarazos$P9_20A, embarazos$P9_20B)
  ENADID_P$orden <- embarazos$P9_29
  ##### date of death #####
  ENADID_P$dod_cmc <- NA
  ENADID_P$dod_cmc_I <- NA
  # number of days lived for children who lived less than one month
  idx <- !is.na(embarazos$P9_19A)
  # we don't know the day of birth so we randomly impute 0 or 1 month based on the number of days lived.
  # We add 0.5 month on average to avoid all children dying at the end of the month
  n_dead <- sum(idx)
  probs <- pmin(embarazos$P9_19A[idx] / 30.44, 1)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + rbinom(n_dead, 1, probs)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_19A[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_19A[idx] == 99, 10L, 1L)  # overwrite for 9999 case
  # number of months lived for children who lived less than one year
  idx <- !is.na(embarazos$P9_19B)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P9_19B[idx] +
    rbinom(n_dead, 1, 0.5) # we add 0.5 month on average to avoid all children dying at the end of the month
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_19B[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_19B[idx] == 99, 10L, 1L)  # overwrite for 9999 case
  # number of years lived for children who lived more than one year
  idx <- !is.na(embarazos$P9_19C)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P9_19C[idx] * 12 + sample(0:11, n_dead, replace = TRUE)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_19C[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_19C[idx] == 99, 10L, 1L)  # overwrite for 9999 case

  ENADID_P$mortinatos <- embarazos$P9_23
  ENADID_P$abortos <- embarazos$P9_27
  ENADID_P <- subset(ENADID_P, !(mortinatos %in% c(seq(6,10,1),99)))
  ENADID_P <- subset(ENADID_P, !(abortos %in% c(seq(1,5,1),99)))
  ENADID_P$mortinatos <- NULL
  ENADID_P$abortos <- NULL

  ENADID_P_wide <- ENADID_P %>%
    dplyr::arrange (llave_muj) %>%
    dplyr::group_by(llave_muj) %>%
    dplyr::mutate(
      # Create temporary sorting variable
      sort_key = if_else(orden != 99L, as.numeric(orden) * 100000, as.numeric(dob_cmc))
      # Multiply orden by 10000 to ensure it dominates when used
    )
    
    # Step 2: Sort by the sorting key
    ENADID_P_wide <- ENADID_P_wide %>%
    dplyr::arrange(llave_muj, sort_key) %>%
    
    # Step 3: Create sequential birth number
    dplyr::mutate(birth_num = row_number()) %>%
    dplyr::ungroup()
  
  ENADID_P_wide <- subset(ENADID_P_wide, !(is.na(ENADID_P_wide$llave_muj)))
  
  # Step 4: Remove temporary variables
  ENADID_P_wide <- ENADID_P_wide %>%
    select(-sort_key, -orden)  
  # Pivot to wide format
  ENADID_P_wide <- ENADID_P_wide %>%
    tidyr::pivot_wider(
      id_cols = llave_muj,
      names_from = birth_num,
      values_from = c(dob_cmc, dod_cmc, sex, dob_cmc_I, dod_cmc_I),
      names_sep = ""
    ) %>%
    
    # Count number of children
    dplyr::mutate(
      nChildren = rowSums(!is.na(select(., starts_with("sex"))))
    ) %>%
    
    # Reorder columns: llave_muj, nChildren, then birth data
    dplyr::select(llave_muj, nChildren, everything())
  
  missing_keys <- setdiff(ENADID_P_wide$llave_muj, ENADID1992$llave_muj)
  if (length(missing_keys) > 0) stop (paste("ENADID_P_wide is missing", length(missing_keys), "keys from ENADID"))
  
  return (ENADID_P_wide)
}

ENADID_P <- bigDataChildbirths (embarazos)

#### append the birth histories ####
ENADID1992_full <- dplyr::left_join(ENADID1992, ENADID_P, by = "llave_muj")
rm(ENADID_P)
rm(ENADID1992)

ENADID1992_full$nChildren[is.na(ENADID1992_full$nChildren)] <- 0
if (!isTRUE(
  all.equal(ENADID1992_full$nChildren, ENADID1992_full$nBioKids)
)) {
  ENADID1992_full$diff <- ENADID1992_full$nBioKids - ENADID1992_full$nChildren
  ENADID1992_full_diff <- subset(ENADID1992_full, diff!= 0)
  cat ("nBioKids and nChildren do not match for", nrow(ENADID1992_full_diff), "women\n")
  cat ("we correct based on the childbirths database\n")
  ENADID1992_full$nBioKids <- ENADID1992_full$nChildren
  ENADID1992_full$diff <- NULL
  }

ENADID1992_full <- cleanENADID(ENADID1992_full)
ENADID1992_full <- reorder_birthHistory(ENADID1992_full)

save(ENADID1992_full, file=path_ENADID1992)
rm(ENADID1992_full_diff)
save(mujeres1992, file=path_ENADID1992_mujeres)
