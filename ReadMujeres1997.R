setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#### Read ENADID 1997 ####
library (tidyverse)
library (foreign)
library(janitor)
library(bit64) # Necessary for the integer64 type

path_mujer <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1997/base_datos_enadid97_dbf/E97CMU.DBF"))
path_general <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1997/base_datos_enadid97_dbf/E97DGE.DBF"))
path_uniones <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1997/base_datos_enadid97_dbf/E97UNI.DBF"))
path_embarazos <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/1997/base_datos_enadid97_dbf/E97HEM.DBF"))
path_ENADID1997 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID1997.Rdat"))
path_ENADID1997_mujeres <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/ENADID1997_mujeres.Rdat"))
# read everything as character
mujeres1997 <- foreign::read.dbf (path_mujer, as.is = TRUE)
general <- foreign::read.dbf (path_general, as.is = TRUE)
uniones <- foreign::read.dbf (path_uniones, as.is = TRUE)
embarazos <- foreign::read.dbf (path_embarazos, as.is = TRUE)
# Convert columns 9 to end to integer
# remove columns with text only
mujeres1997 <- convertToInteger(mujeres1997, 11, colNotToConvert = c(63, 64, 71, 75))
mujeres1997$FAC_MUJ <- as.integer(mujeres1997$FAC_MUJ)
general <- convertToInteger(general, 11)
uniones <- convertToInteger(uniones, 11)
embarazos <- convertToInteger(embarazos, 11)

llave_muj64 <- function(df) {
  # no key in ENADID1997. We build it here. We will leave it as character, no conversion to integer64
  df$llave_muj <- paste0(df$ENT, df$MUN, df$ZONA, df$UPM, df$F_VIV, df$HOGAR, df$P3_1)
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

mujeres1997 <- llave_muj64 (mujeres1997)
general <- llave_muj64 (general)
uniones <- llave_muj64 (uniones)
embarazos <- llave_muj64 (embarazos)

# check the llave_muj key is OK
if (any(is.na(mujeres1997$llave_muj))) stop ("mujeres1997 has NA in llave_muj")
if (any(is.na(general$llave_muj))) stop ("mujeres1997 has NA in llave_muj")
if (any(is.na(uniones$llave_muj))) stop ("uniones has NA in llave_muj")
if (any(is.na(embarazos$llave_muj))) stop ("embarazos has NA in llave_muj")

dup_mujer <- mujeres1997 %>% get_dupes(llave_muj)
#dup_uniones <- uniones %>% get_dupes(llave_muj)
if (nrow(dup_mujer) > 0) stop ("mujeres1997 has duplicate keys")
rm(dup_mujer)
#if (nrow(dup_uniones) > 0) stop ("uniones has duplicate keys")

# check each row of uniones or embarazos has a corresponding row of mujeres
keys_only_in_uniones <- setdiff(uniones$llave_muj, mujeres1997$llave_muj)
if (!(length(keys_only_in_uniones) == 0 )) stop(paste("FAILURE: mujeres1997 is missing", length(keys_only_in_uniones), "keys"))
# check mujeres with previous unions have the adequate number of rows in uniones
checkCountUnions <- function (women=mujeres1997, unions=uniones, fieldName="P14_8") {
  # 1. Count how many unions actually exist for each woman in the 'uniones' table
  uniones_counts <- unions %>%
    count(llave_muj, name = "actual_count")
  # 2. Join this back to the 'mujeres1997' table
  comparison <- women %>%
    left_join(uniones_counts, by = "llave_muj") %>%
    # Replace NA with 0 for women who have no records in the 'uniones' table
    mutate(actual_count = coalesce(actual_count, 0)) %>%
    # Create a check field: TRUE if the counts match
    mutate(is_complete = (.data[[fieldName]] == actual_count))
  # 3. View only the discrepancies
  discrepancies <- comparison %>%
    filter(!is_complete)
  if (nrow(discrepancies)>0) cat ("missing unions in uniones\n")
  rm(comparison)
  rm(discrepancies)
}

checkCountUnions(mujeres1997, uniones, "P14_8")

keys_only_in_embarazos <- setdiff(embarazos$llave_muj, mujeres1997$llave_muj)
if (!(length(keys_only_in_embarazos) == 0)) stop(paste("FAILURE: mujeres1997 is missing", length(keys_only_in_embarazos), "keys"))
# check each row of mujeres has a corresponding row of general
keys_only_in_mujeres <- setdiff(mujeres1997$llave_muj, general$llave_muj)
if (!(length(keys_only_in_mujeres) == 0)) stop(paste("FAILURE: general is missing", length(keys_only_in_mujeres), "keys"))

# add marital status from general to mujeres (but we have P14_1 anyway)
mujeres1997 <- mujeres1997 %>%
  dplyr::left_join(general %>% select(llave_muj, P6_1), by = "llave_muj")

getDatos1997 <- function (mujeres) {
  datos <- data.frame(llave_muj=mujeres$llave_muj, region=mujeres$ENT)
  datos$survey <- "ENADID1997"
  datos$surveyDate_cmc <- compute_cmc(1, 1998) # date of survey is from "8 septiembre hasta 15 diciembre 1997". We cut at January 1998
  datos$monthBirth <- mujeres$P9_1A
  datos$yearBirth <- mujeres$P9_1B
  datos$yearBirth <- ifelse(datos$yearBirth == 99, 9999, datos$yearBirth)
  datos$yearBirth <- ifelse(datos$yearBirth %in% seq(20,92,1), datos$yearBirth + 1900, datos$yearBirth)
  datos$age <- mujeres$P9_2A
  datos$nLiveBirths <- mujeres$P9_7
  datos$nLiveBirths <- ifelse(is.na(datos$nLiveBirths),0,datos$nLiveBirths)
  datos$indiv_weight <- mujeres$FAC_MUJ
  datos$pregnant <- mujeres$P11_1
  datos$pregnant <- factor(datos$pregnant, levels = c(1, 2, 8, 9), labels = c("yes", "no", "don't know", "don't know"))
  datos$pregnant_want_another <- mujeres$P11_2
  datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels = c(3, 4, 8, 9), labels = c("yes", "no", "don't know", "don't know"))
  datos$pregnant_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant == "yes"), mujeres$P11_6A, NA)
  datos$pregnant_ideal_number <- ifelse ((datos$pregnant_ideal_number>50), 99, datos$pregnant_ideal_number)
  datos$nullipar_want_another <- ifelse((datos$nLiveBirths == 0), mujeres$P11_4, NA)
  datos$nullipar_want_another <- factor(datos$nullipar_want_another, levels = c(5, 6, 8, 9),
                                         labels = c("yes", "no", "don't know", "don't know"))
  datos$nullipar_ideal_number <- ifelse (((!is.na(datos$pregnant))&(datos$pregnant != "yes"))&(datos$nLiveBirths == 0), mujeres$P11_6A, NA)
  datos$nullipar_ideal_number <- ifelse ((datos$nullipar_ideal_number>50), 99, datos$nullipar_ideal_number)
  datos$mother_want_another <- ifelse((datos$nLiveBirths > 0), mujeres$P11_4, NA)
  datos$mother_want_another <- factor(datos$mother_want_another, levels = c(5, 6, 8, 9),
                                       labels = c("yes", "no", "don't know", "don't know"))
  datos$mother_ideal_number <- ifelse (((!is.na(datos$pregnant))&(datos$pregnant != "yes"))&(datos$nLiveBirths > 0), mujeres$P11_6A, NA)
  datos$mother_ideal_number <- ifelse ((datos$mother_ideal_number>50), 99, datos$mother_ideal_number)
  
  datos$nullipar_fecund <- NA
  datos$mother_fecund <- NA
  datos$mother_unwanted <- NA
  datos$mother_less <- NA
  datos$want_another <- NA
  datos$ideal_number <- NA
  datos$motive_no_child <- NA

  datos$ever_contraception <- mujeres$P12_3
  datos$ever_contraception <- factor(datos$ever_contraception, levels = c(1, 2),
                                     labels = c("no", "si"))
  datos$ever_contraception <- ifelse((datos$ever_contraception == "no")&(mujeres$P12_14 == 1), "si", datos$ever_contraception)
  datos$age_first_sex <- NA
  
  datos$union_status <- mujeres$P14_1
  datos$union_status <- factor(datos$union_status, levels = seq(1,11,1),
                                labels = c("cohabitation", "separated cohabitation", "separated marriage", "divorced", "divorced", "widow cohabitation", "widow marriage", "married", "married", "married", "single"))

  datos$lastUnion_month_start <- mujeres$P14_3A
  datos$lastUnion_year_start <- mujeres$P14_3B
  datos$lastUnion_year_start <- ifelse(datos$lastUnion_year_start==99,
                                       9999,
                                       ifelse(datos$lastUnion_year_start %in% seq(50,97),
                                              datos$lastUnion_year_start+1900,
                                              datos$lastUnion_year_start))
  datos$lastUnion_month_end <- mujeres$P14_2A
  datos$lastUnion_year_end <- mujeres$P14_2B
  datos$lastUnion_year_end <- ifelse(datos$lastUnion_year_end==99,
                                       9999,
                                       ifelse(datos$lastUnion_year_end %in% seq(50,97),
                                              datos$lastUnion_year_end+1900,
                                              datos$lastUnion_year_end))
  datos$lastUnion_cohab_before <- mujeres$P14_5
  datos$lastUnion_cohab_before <- factor(datos$lastUnion_cohab_before, levels = c(1,2,9),
                               labels = c("yes", "no", "dont' know"))
  datos$lastUnion_month_cohab_before <- mujeres$P14_6A
  datos$lastUnion_year_cohab_before <- mujeres$P14_6B
  datos$nUnionBeforeLast <- mujeres$P14_8
  
  return (datos)
}

ENADID1997 <- bigDataWomen ( getDatos1997 (mujeres1997), hasFullUnionHistory = TRUE )

#### Uniones ####
bigDataUniones1997 <- function(uniones) {
  ENADID_U <- data.frame(llave_muj = uniones$llave_muj)
  ENADID_U$nUnion <- uniones$P14_9
  
  # Process dates
  ENADID_U$union_start_cmc <- compute_cmc(uniones$P14_10A, uniones$P14_10B)
  ENADID_U$union_start_cmc_I <- imputed_date(uniones$P14_10A, uniones$P14_10B)
  
  ENADID_U$union_end_cmc <- compute_cmc(uniones$P14_13A, uniones$P14_13B)
  ENADID_U$union_end_cmc_I <- imputed_date(uniones$P14_13A, uniones$P14_13B)
  
  # Process union type
  ENADID_U$union_start_type <- uniones$P14_11
  ENADID_U$union_start_type <- ifelse((!is.na(uniones$P14_14)) & (uniones$P14_14 == 1) & (uniones$P14_11 %in% c(2, 3, 4)), 
                                      5, 
                                      ENADID_U$union_start_type)
  ENADID_U$union_start_type <- factor(ENADID_U$union_start_type, 
                                                 levels = c(1, 2, 3, 4, 5, 9),
                                                 labels = c("cohabitation", "marriage", "marriage", 
                                                            "marriage", "cohabitation before marriage", 
                                                            "don't know"))
  # Marriage dates
  ENADID_U$marriage_start_cmc <- ifelse(ENADID_U$union_start_type %in% c("marriage", "cohabitation before marriage"), 
                                        ENADID_U$union_start_cmc, NA)
  ENADID_U$marriage_start_cmc_I <- ifelse(ENADID_U$union_start_type %in% c("marriage", "cohabitation before marriage"), 
                                        ENADID_U$union_start_cmc_I, NA)
  ENADID_U$marriage_end_cmc <- ifelse(ENADID_U$union_start_type %in% c("marriage", "cohabitation before marriage"), 
                                      ENADID_U$union_end_cmc, NA)
  ENADID_U$marriage_end_cmc_I <- ifelse(ENADID_U$union_start_type %in% c("marriage", "cohabitation before marriage"), 
                                      ENADID_U$union_end_cmc_I, NA)
  
  # Union end motive
  ENADID_U$union_end_motive <- uniones$P14_12
  ENADID_U$union_end_motive <- factor(ENADID_U$union_end_motive, 
                                      levels = c(1, 2, 3, 9),
                                      labels = c("separation", "widowhood", 
                                                 "separation", "don't know"))
  
  # Cohabitation before
  ENADID_U$union_cohab_before <- uniones$P14_14
  ENADID_U$union_cohab_before <- factor(ENADID_U$union_cohab_before, 
                                        levels = c(1, 2, 9),
                                        labels = c("yes", "no", "don't know"))
  
  # Cohabitation start date
  ENADID_U$union_cohab_start_cmc <- ifelse((!is.na(ENADID_U$union_cohab_before)) & 
                                             (ENADID_U$union_cohab_before == "yes"), 
                                           compute_cmc(uniones$P14_15A, uniones$P14_15B), 
                                           NA)
  ENADID_U$union_cohab_start_cmc_I <- ifelse((!is.na(ENADID_U$union_cohab_before)) & 
                                             (ENADID_U$union_cohab_before == "yes"), 
                                           imputed_date(uniones$P14_15A, uniones$P14_15B), 
                                           NA)
  
  # Adjust union start if cohabitation before
  ENADID_U$union_start_cmc <- ifelse((!is.na(ENADID_U$union_cohab_before)) & 
                                       (ENADID_U$union_cohab_before == "yes"), 
                                     ENADID_U$union_cohab_start_cmc, 
                                     ENADID_U$union_start_cmc)
  ENADID_U$union_start_cmc_I <- ifelse((!is.na(ENADID_U$union_cohab_before)) & 
                                       (ENADID_U$union_cohab_before == "yes"), 
                                     ENADID_U$union_cohab_start_cmc_I, 
                                     ENADID_U$union_start_cmc_I)
  
  ENADID_U$union_cohab_before <- NULL
  ENADID_U$union_cohab_start_cmc <- NULL
  ENADID_U$union_cohab_start_cmc_I <- NULL
  
  # PIVOT TO WIDE FORMAT
  # Order by woman and union number
  ENADID_U <- ENADID_U %>%
    arrange(llave_muj, nUnion) %>%
    group_by(llave_muj) %>%
    mutate(union_number = row_number()) %>%
    ungroup()
  
  # create a fake union to make place for last union stored in mujeres dataframe
  mU <- max (ENADID_U$union_number)
  fakeUnion <- ENADID_U[1,]
  fakeUnion$llave_muj <- "123456789"
  fakeUnion$union_number <- mU + 1
  ENADID_U <- rbind(ENADID_U, fakeUnion)
  
  # Pivot wider with numbered columns
  ENADID_U_wide <- ENADID_U %>%
    select(-nUnion) %>%  # Remove original nUnion, use union_number instead
    pivot_wider(
      id_cols = llave_muj,
      names_from = union_number,
      values_from = c(union_start_cmc, union_start_cmc_I,
                      union_end_cmc, union_end_cmc_I,
                      marriage_start_cmc, marriage_start_cmc_I,
                      marriage_end_cmc, marriage_end_cmc_I,
                      union_end_motive, union_start_type),
      names_sep = ""
    ) %>%
    # Add total number of unions
    mutate(
      n_unions = rowSums(!is.na(select(., starts_with("union_start_cmc"))))
    ) %>%
    # Reorder columns: llave_muj, n_unions, then all union data
    select(llave_muj, n_unions, everything())
  
  # Remove the fake union
  ENADID_U_wide <- subset (ENADID_U_wide, llave_muj != "123456789")
  
  ENADID_U_wide$n_unions <- ENADID_U_wide$n_unions / 2
  
  return(ENADID_U_wide)
}

ENADID_U <- bigDataUniones1997 (uniones)

#### append the union histories ####
ENADID1997_full <- dplyr::left_join(ENADID1997, ENADID_U, by = "llave_muj")

# check the number of unions
womenWithPreviousUnions <- subset(ENADID1997, nUnion>1)
# 1. check each row of womenWithPreviousUnions has a corresponding row of uniones
keys_only_in_mujeres <- setdiff(womenWithPreviousUnions$llave_muj, ENADID_U$llave_muj)
if (!(length(keys_only_in_mujeres) == 0 )) stop(paste("FAILURE: unions is missing", length(keys_only_in_mujeres), "keys"))
rm (womenWithPreviousUnions)
# 2. check the number of unions match
ENADID1997_full$n_unions <- ENADID1997_full$n_unions + 1
if (any((!is.na(ENADID1997_full$n_unions))&(ENADID1997_full$n_unions != ENADID1997_full$nUnion))) {
  st <- sum((!is.na(ENADID1997_full$n_unions))&(ENADID1997_full$n_unions != ENADID1997_full$nUnion))
  cat ("we correct", st, "women with number of unions that do not match between mujeres and uniones\n")
  ENADID1997_full$nUnion <- ifelse ((!is.na(ENADID1997_full$n_unions)),ENADID1997_full$n_unions,ENADID1997_full$nUnion)
  ENADID1997_full$n_unions <- NULL
}

#### copy info last union into the union history ####
maxU  <- max(ENADID1997_full$nUnion, na.rm = TRUE)

# first convert to integer factor variables we will modify with last union
for (k in (1:maxU)) {
  rows <- (ENADID1997_full$nUnion == k)
  to   <- paste0("union_end_motive", k)
  ENADID1997_full[, to] <- as.integer(ENADID1997_full[, to])
  ENADID1997_full[rows, to] <- ifelse(ENADID1997_full[rows, to]==4, 9 , ENADID1997_full[rows, to])
}

# All variable "bases" that should be copied for the last union:
union_bases <- c("start_cmc", "end_cmc", "start_cmc_I", "end_cmc_I", "end_motive", "start_type")
marriage_bases <- c("start_cmc", "end_cmc", "start_cmc_I", "end_cmc_I")

# rows where this union number is the last union
for (k in (1:maxU)) {
  rows <- (ENADID1997_full$nUnion == k)
  
  for (b in union_bases) {
    from <- paste0("lastUnion_", b)
    to   <- paste0("union_", b, k)
    
    ENADID1997_full[rows, to] <- ENADID1997_full[rows, from]
  }
  for (b in marriage_bases) {
    from <- paste0("lastMarriage_", b)
    to   <- paste0("marriage_", b, k)
    
    ENADID1997_full[rows, to] <- ENADID1997_full[rows, from]
  }
}

# now convert back to factor
for (k in (1:maxU)) {
  rows <- (ENADID1997_full$nUnion == k)
  to   <- paste0("union_end_motive", k)
  ENADID1997_full[rows, to] <- ifelse((is.na(ENADID1997_full[rows, to])), 0, ENADID1997_full[rows, to])
  ENADID1997_full[, to] <- factor(ENADID1997_full[, to], levels = c(0,1,2,3),
                                      labels = c("in union", "separation", "widowhood", "don't know"))
}
ENADID1997_full$lastUnion_end_motive <- factor(ENADID1997_full$lastUnion_end_motive, levels = c(0,1,2,3),
                                labels = c("in union", "separation", "widowhood", "don't know"))

# Childbirths
bigDataChildbirths <- function (embarazos) {
  ENADID_P <- data.frame(llave_muj=embarazos$llave_muj, sex=embarazos$P9_12)
  ENADID_P$dob_cmc <- compute_cmc(embarazos$P9_17A, embarazos$P9_17B)
  ENADID_P$dob_cmc_I <- imputed_date(embarazos$P9_17A, embarazos$P9_17B)
  ENADID_P$sex_dead <- embarazos$P9_15
  ENADID_P$sex <- ifelse(is.na(ENADID_P$sex),ENADID_P$sex_dead,ENADID_P$sex)
  ENADID_P$sex_dead <- NULL
  ENADID_P$orden <- embarazos$P9_41
  ##### date of death #####
  # number of days lived for children who lived less than one month
  ENADID_P$dod_cmc <- NA
  ENADID_P$dod_cmc_I <- NA
  idx <- !is.na(embarazos$P9_16A)
  n_dead <- sum(idx)
  probs <- pmin(embarazos$P9_16A[idx] / 30.44, 1)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + rbinom(n_dead, 1, probs)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_16A[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_16A[idx] == 99, 10L, 1L)
  # number of months lived for children who lived less than one year
  idx <- !is.na(embarazos$P9_16B)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P9_16B[idx] + sample(c(0,1), n_dead, replace = TRUE) # we add 0.5 month on average to avoid all children dying at the end of the month
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_16B[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_16B[idx] == 99, 10L, 1L)  # overwrite for 9999 case
  # number of years lived for children who lived more than one year
  idx <- !is.na(embarazos$P9_16C)
  n_dead <- sum(idx)
  ENADID_P$dod_cmc[idx] <- ENADID_P$dob_cmc[idx] + embarazos$P9_16C[idx] * 12 + sample(0:11, n_dead, replace = TRUE)
  ENADID_P$dod_cmc[idx] <- ifelse(embarazos$P9_16C[idx] == 99,9999, ENADID_P$dod_cmc[idx])
  ENADID_P$dod_cmc_I[idx] <- ifelse(embarazos$P9_16C[idx] == 99, 10L, 1L)  # overwrite for 9999 case
  
  ENADID_P <- subset(ENADID_P,embarazos$P9_40 %in% c(1,2))
  
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
      values_from = c(dob_cmc, dob_cmc_I, dod_cmc, dod_cmc_I, sex),
      names_sep = ""
    ) %>%
    
    # Count number of children
    dplyr::mutate(
      nChildren = rowSums(!is.na(select(., starts_with("sex"))))
    ) %>%
    
    # Reorder columns: llave_muj, nChildren, then birth data
    dplyr::select(llave_muj, nChildren, everything())
  
  missing_keys <- setdiff(ENADID_P_wide$llave_muj, ENADID1997$llave_muj)
  if (length(missing_keys) > 0) stop (paste("ENADID_P_wide is missing", length(missing_keys), "keys from ENADID"))
  
  return (ENADID_P_wide)
}

ENADID_P <- bigDataChildbirths (embarazos)

#### append the birth histories ####
ENADID1997_full <- dplyr::left_join(ENADID1997_full, ENADID_P, by = "llave_muj")
rm(ENADID1997)
rm(ENADID_U)
rm(ENADID_P)
rm(embarazos)
rm(general)
rm(uniones)

ENADID1997_full$nChildren[is.na(ENADID1997_full$nChildren)] <- 0
# the count of kids from embarazos is better
cat ("differences between nBioKids and nChildren:", sum(ENADID1997_full$nBioKids != ENADID1997_full$nChildren), "\n")
ENADID1997_full$nBioKids <- ENADID1997_full$nChildren

ENADID1997_full <- cleanENADID(ENADID1997_full)
ENADID1997_full <- reorder_birthHistory(ENADID1997_full)

save(ENADID1997_full, file=path_ENADID1997)
save(mujeres1997, file=path_ENADID1997_mujeres)
