setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
# read GGS II file and convert to ENADID format
library (tidyverse)
library (haven)
library(foreign) # for Taiwan

path_Moldova <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_MLD_V_3_0/GGSII_Wave1_MLD_V_3_0.sav")
path_France <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_FR_V_1_0/GGSII_Wave1_FR_V_1_0.sav")
path_Austria <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_AT_V_1_1/GGSII_Wave1_AT_V_1_1.sav")
path_CzechRep <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_CZ_V_1_0/GGSII_Wave1_CZ_V_1_0.zsav")
path_Estonia <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_EE_V_1_0/GGSII_Wave1_EE_V_1_0.zsav")
path_Uruguay <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_UY_V_1_2/GGSII_Wave1_UY_V_1_2.zsav")
path_UK <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_UK_V_1_2/GGSII_Wave1_UK_V_1_2.sav")
path_Taiwan <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_TW_V_1_0/GGSII_Wave1_TW_V_1_0.sav")
path_Sweden <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_SW_V_1_1/GGSII_Wave1_SW_V_1_1.zsav")
path_Netherlands <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_NL_V_1_0/GGSII_Wave1_NL_V_1_0.sav")
path_Croatia <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_HR_V_1_1/GGSII_Wave1_HR_V_1_1.sav")
path_Finland <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_FI_V_1_0/GGSII_Wave1_FI_V_1_0.zsav")
path_Denmark <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGSII_Wave1_DK_V_1_0/GGSII_Wave1_DK_V_1_0.zsav")
path_Germany <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGS-II_Wave1_DE_V_1_0/GGSII_Wave1_DE_V_1_0.sav")
path_Norway <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGP2020_wave1_NOR_V_0_1/GGP2020_WAVE1_NOR_V_0_1.zsav")
path_Kazakhstan <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGP2020_WAVE1_KAZ_V_1_0/GGP2020_WAVE1_KAZ_V_1_0.zsav")
path_Belarus <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys/GGS/GGS II/GGP2020_WAVE1_BLR_V_1_0_P2e2z6r/GGP2020_WAVE1_BLR_V_1_0.ZSAV")

GGS_list <- list()
#GGS_list$moldova <- haven::read_sav(path_Moldova)
GGS_list$moldova <- read.spss(path_Moldova, to.data.frame = TRUE, use.value.labels = FALSE, reencode = NA)
GGS_list$france <- haven::read_sav(path_France)
GGS_list$austria <- haven::read_sav(path_Austria)
GGS_list[["czechia"]] <- haven::read_sav(path_CzechRep)
GGS_list$estonia <- haven::read_sav(path_Estonia)
GGS_list$uruguay <- haven::read_sav(path_Uruguay)
GGS_list$UK <- haven::read_sav(path_UK)
#GGS_list$taiwan <- haven::read_sav(path_Taiwan, encoding = "CP1252")
GGS_list$taiwan <- read.spss(path_Taiwan, to.data.frame = TRUE, use.value.labels = FALSE, reencode = NA)
GGS_list$sweden <- haven::read_sav(path_Sweden)
GGS_list$netherlands <- haven::read_sav(path_Netherlands)
GGS_list$croatia <- haven::read_sav(path_Croatia, encoding = "windows-1250")
GGS_list$finland <- haven::read_sav(path_Finland)
GGS_list$denmark <- haven::read_sav(path_Denmark)
GGS_list$germany <- haven::read_sav(path_Germany)
GGS_list$norway <- haven::read_sav(path_Norway)

correctMonthYear <- function (mujeres) {
  # Moldova has the number for months or years before the symbol "m" or "y"
  # Invert it there: for example lhiXX_XXm -> lhiXX_mXX
  mujeres <- mujeres %>%
    rename_with(
      ~ str_replace(., "(\\d+)m$", "m\\1"), 
      starts_with("lhi")
    ) %>%
    rename_with(
      ~ str_replace(., "(\\d+)y$", "y\\1"), 
      starts_with("lhi")
    )
  return (mujeres)
}

getDatosGGS2 <- function (mujeres, country=NULL) {
  message("Processing GGS2 data for country: ", country)
  mujeres <- subset(mujeres, dem01==2) # OK
  mujeres <- correctMonthYear (mujeres)
  # tibble can be a pain!!
  mujeres <- as.data.frame(mujeres)
  datos <- data.frame(llave_muj=mujeres$respid, indiv_weight=mujeres$weight) # OK
  datos$country <- toupper(country) # OK
  datos$survey <- "GGS2" # OK
  datos <- relocate(datos,survey)
  datos <- relocate(datos,country)
  datos$ind_original <- NA
  datos <- relocate(datos,indiv_weight,.after=ind_original)
  datos$surveyDate_cmc <- compute_cmc(mujeres$intdatem, mujeres$intdatey) # OK
  datos$indiv_dob_cmc <- compute_cmc(mujeres$dem02m, mujeres$dem02y) # OK
  datos$yBirth <- mujeres$dem02y # OK
  if ("age" %in% names (mujeres)) {
    datos$indiv_age_survey <- mujeres$age # OK
  } else {
    datos$indiv_age_survey <- trunc((datos$surveyDate_cmc - datos$indiv_dob_cmc) / 12) # OK
  }
  
  #datos$nLiveBirths <- mujeres$fer01a # OK
  #datos$nLiveBirths <- ifelse(is.na(datos$nLiveBirths),0,datos$nLiveBirths)
  datos$pregnant <- mujeres$fer01a # OK
  datos$pregnant <- factor(datos$pregnant, levels = c(1, 2, 3, 9), labels = c("yes", "no", "not sure", "don't know"))
  datos$pregnant_wanted <- mujeres$fer03 # OK
  datos$pregnant_wanted <- ifelse(mujeres$fer04==2,3,datos$pregnant_wanted)
  datos$pregnant_wanted <- factor(datos$pregnant_wanted, levels = c(1, 2, 3, 9), labels = c("yes", "no", "yes but later","don't know"))
  datos$lastBirth_wanted <- mujeres$fer04b # OK
  datos$lastBirth_wanted <- ifelse(mujeres$fer04c==2,3,datos$lastBirth_wanted)
  datos$lastBirth_wanted <- factor(datos$lastBirth_wanted, levels = c(1, 2, 3, 9), labels = c("yes", "no", "yes but later","don't know"))
  if ("fer05" %in% names(mujeres)) {
    datos$fecund <- mujeres$fer05 # OK
    datos$fecund <- factor(datos$fecund, levels = c(1, 2, 3, 4, 9),
                           labels = c("definitely not", "probably not", "probably yes", "definitely yes", "don't know"))
  } else {
    datos$fecund <- NA # OK
  }
  #datos$pregnant_want_another <- mujeres$p7_3
  # datos$pregnant_want_another <- factor(datos$pregnant_want_another, levels = c(1, 2, 9), labels = c("yes", "no", "don't know"))
  # 
  # datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths == 0), mujeres$p7_10, NA)
  # datos$pregnant_ideal_number <- ifelse ((datos$pregnant == "yes")&(datos$nLiveBirths > 0), mujeres$p7_14, datos$pregnant_ideal_number)
  # datos$nullipar_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$p7_10, NA)
  # datos$mother_ideal_number <- ifelse ((!is.na(datos$pregnant))&(datos$pregnant != "yes"), mujeres$p7_14, NA)
  # 
  # datos$nullipar_want_another <- mujeres$p7_7
  # datos$nullipar_want_another <- factor(datos$nullipar_want_another, levels = c(1, 2, 3, 9),
  #                                       labels = c("yes", "yes but can't", "no", "don't know"))
  # datos$nullipar_fecund <- mujeres$p7_9
  # datos$nullipar_fecund <- factor(datos$nullipar_fecund, levels = c(1, 2, 3, 4, 5, 6, 9),
  #                                 labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  # datos$mother_want_another <- mujeres$p7_11
  # datos$mother_want_another <- factor(datos$mother_want_another, levels = c(1, 2, 3, 9),
  #                                     labels = c("yes", "yes but can't", "no", "don't know"))
  # datos$mother_fecund <- mujeres$p7_13
  # datos$mother_fecund <- factor(datos$mother_fecund, levels = c(1, 2, 3, 4, 5, 6, 9),
  #                               labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  # datos$mother_unwanted <- mujeres$p7_17
  # datos$mother_unwanted <- factor(datos$mother_unwanted, levels = c(seq(1,6,1),9),
  #                                 labels = c("no contraception", "contraception unknown", "contraception failure", "partner wanted", "religious", "other", "don't know"))
  # datos$mother_less <- mujeres$p7_18
  # datos$mother_less <- factor(datos$mother_less, levels = c(seq(1,7,1),9),
  #                             labels = c("want more", "no money", "studying", "health", "no partner", "sterilized", "other", "don't know"))
  
  datos$want_another <- NA
  idx_yes <- (mujeres$fer14 %in% c(4, 5))|(mujeres$fer15 %in% c(4, 5))
  idx_unsure <- (mujeres$fer14 == 3)&(mujeres$fer15 == 3)
  idx_no <- (mujeres$fer14 %in% c(1, 2))&(mujeres$fer15 %in% c(1, 2))
  idx_yes[is.na(idx_yes)] <- FALSE
  idx_unsure[is.na(idx_unsure)] <- FALSE
  idx_no[is.na(idx_no)] <- FALSE
  datos$want_another[idx_yes] <- 1
  datos$want_another[idx_no] <- 2
  datos$want_another[idx_unsure] <- 3
  datos$want_another <- factor(datos$want_another, levels = c(1,2,3,9),
    labels = c("yes", "no", "unsure", "don't know"))
  
  datos$ideal_number <- NA
  # datos$motive_no_child <- mujeres$nohijos
  # datos$motive_no_child <- factor(datos$motive_no_child, levels = c(1, 2, 3, 4, 5, 6, 9),
  #                                 labels = c("health", "sterilized", "no partner", "menopause", "sterility", "other", "don't know"))
  # 
  datos$ever_contraception <- NA
  # datos$ever_contraception <- factor(datos$ever_contraception, levels = c(1, 2, 9),
  #                                    labels = c("yes", "no","don't know"))
  datos$age_first_sex <- mujeres$fer23
  
  #### Union history ####
  datos$union_status <- mujeres$dem21 # OK (Respondent has a partner (1=yes, 2=no))
  datos$union_status <- ifelse (mujeres$dem28a==1, 3, datos$union_status) # OK (mujeres$dem28a is "Married to partner")

  datos$lastMarriage_start_cmc <- compute_cmc(as.integer(mujeres$dem28bm),as.integer(mujeres$dem28by))
  datos$lastUnion_start_cmc <- compute_cmc(mujeres$dem30bm,mujeres$dem30by) # OK

  # if date of marriage is earlier than date of union, we correct the last
  idx <- datos$lastMarriage_start_cmc < datos$lastUnion_start_cmc # OK
  idx [is.na(idx)] <- FALSE
  datos$lastUnion_start_cmc [idx] <- datos$lastMarriage_start_cmc [idx] # OK

  # Unions are only the ones who are "living with partner"...   (mujeres$dem30a is "Living with partner")
  datos$lastMarriage_start_cmc <- ifelse(mujeres$dem30a!=1,NA,datos$lastMarriage_start_cmc) # OK
  datos$lastUnion_start_cmc <- ifelse(mujeres$dem30a!=1,NA,datos$lastUnion_start_cmc) # OK
  # At time we have date of marriage and not date of union
  datos$lastUnion_start_cmc <- ifelse((is.na(datos$lastUnion_start_cmc)),datos$lastMarriage_start_cmc,datos$lastUnion_start_cmc) # OK
  
  datos$union_status <- ifelse(mujeres$dem30a!=1,2,datos$union_status) # OK
  
  datos$union_status <- factor(datos$union_status, levels = c(1,2,3,4,5),
                               labels = c("cohabitation", "single","marriage","widowhood", "separated")) # OK
  
  datos$lastUnion <- ifelse (is.na(datos$lastUnion_start_cmc),0,1) # OK
  if (country == "germany") {
    datos$nUnionBeforeLast <- mujeres$lhi02_1401 # OK
  } else {
    datos$nUnionBeforeLast <- mujeres$lhi02 # OK
  }
  datos$nUnionBeforeLast <- ifelse(is.na(datos$nUnionBeforeLast),0,datos$nUnionBeforeLast) # OK
  datos$nUnion <- datos$nUnionBeforeLast + datos$lastUnion # OK
  
  maxU <- max (datos$nUnionBeforeLast, na.rm=T) # OK
  US_type <- "union_start_type"
  US_cmc <- "union_start_cmc" # OK
  UE_mot_cmc <- "union_end_motive" # OK
  UE_cmc <- "union_end_cmc" # OK
  MS_cmc <- "marriage_start_cmc" # OK
  US <- "lhi04_" # OK
  MS <- "lhi05b_" # OK
  UE_mot <- "lhi13_" # OK
  UE <- "lhi14_" # OK
  U_month <- "m" # OK
  U_year <- "y" # OK
  for (i in (1:maxU)) {
    unionStart_type <- paste0(US_type,as.character(i)) # OK
    unionStart_cmc <- paste0(US_cmc,as.character(i)) # OK
    unionStart_month <- paste0(US,U_month,as.character(i)) # OK
    unionStart_year <- paste0(US,U_year,as.character(i)) # OK
    marStart_cmc <- paste0(MS_cmc,as.character(i)) # OK
    marStart_month <- paste0(MS,U_month,as.character(i)) # OK
    marStart_year <- paste0(MS,U_year,as.character(i)) # OK
    if ((unionStart_month %in% names(mujeres))&(marStart_month %in% names(mujeres))&(marStart_year %in% names(mujeres))) { # some countries don't have all the fields
      datos[[unionStart_type]] <- NA # OK (placeholder)
      datos[[unionStart_month]] <- unlist(mujeres[[unionStart_month]])
      datos[[unionStart_year]] <- unlist(mujeres[[unionStart_year]])
      datos[[unionStart_cmc]] <- compute_cmc(datos[[unionStart_month]], datos[[unionStart_year]]) # OK
      datos[[marStart_month]] <- unlist(mujeres[[marStart_month]])
      datos[[marStart_year]] <- unlist(mujeres[[marStart_year]])
      datos[[marStart_cmc]] <- compute_cmc(unlist(mujeres[[marStart_month]]), unlist(mujeres[[marStart_year]])) # OK
      # set date of union backwards if later than date of marriage
      idx <- (datos[[unionStart_cmc]] > datos[[marStart_cmc]])&(!is.na(datos[[marStart_cmc]])) # OK
      idx [is.na(idx)] <- FALSE # OK
      datos[[unionStart_cmc]][idx] <- datos[[marStart_cmc]][idx] # OK
      # if date of union is NA, we set to date of marriage
      idx <- (is.na(datos[[unionStart_cmc]]))
      idx [is.na(idx)] <- FALSE # OK
      datos[[unionStart_cmc]][idx] <- datos[[marStart_cmc]][idx] # OK
      unionEndMotive_cmc <- paste0(UE_mot_cmc,as.character(i)) # OK
      UE_motive <- paste0(UE_mot,as.character(i)) # OK
      datos[[unionEndMotive_cmc]] <- mujeres[[UE_motive]] # OK
      unionEnd_cmc <- paste0(UE_cmc,as.character(i)) # OK
      unionEnd_month <- paste0(UE,U_month,as.character(i)) # OK
      unionEnd_year <- paste0(UE,U_year,as.character(i)) # OK
      datos[[unionEnd_month]] <- unlist(mujeres[[unionEnd_month]])
      datos[[unionEnd_year]] <- unlist(mujeres[[unionEnd_year]])
      datos[[unionEnd_cmc]] <- compute_cmc(unlist(mujeres[[unionEnd_month]]), unlist(mujeres[[unionEnd_year]])) # OK
      if (TRUE) { # for debugging set to FALSE
        datos[[unionStart_month]] <- NULL
        datos[[unionStart_year]] <- NULL
        datos[[marStart_month]] <- NULL
        datos[[marStart_year]] <- NULL
        datos[[unionEnd_month]] <- NULL
        datos[[unionEnd_year]] <- NULL
      }
    }
  }

  # add space for last union
  maxU <- maxU + 1
  unionStart_type <- paste0(US_type,as.character(maxU)) # OK
  datos[[unionStart_type]] <- NA # OK
  unionStart_cmc <- paste0(US_cmc,as.character(maxU)) # OK
  datos[[unionStart_cmc]] <- NA # OK
  marStart_cmc <- paste0(MS_cmc,as.character(maxU)) # OK
  datos[[marStart_cmc]] <- NA # OK
  unionEndMotive_cmc <- paste0(UE_mot_cmc,as.character(maxU)) # OK
  datos[[unionEndMotive_cmc]] <- NA # OK
  unionEnd_cmc <- paste0(UE_cmc,as.character(maxU)) # OK
  datos[[unionEnd_cmc]] <- NA # OK
  for (i in (1:maxU)) {
    idx <- (datos$lastUnion==1)&(datos$nUnion == i) # OK
    unionStart_cmc <- paste0(US_cmc,as.character(i)) # OK
    datos[idx,unionStart_cmc] <- datos$lastUnion_start_cmc [idx] # OK
    marStart_cmc <- paste0(MS_cmc,as.character(i)) # OK
    datos[idx,marStart_cmc] <- datos$lastMarriage_start_cmc [idx] # OK
    unionEndMotive_cmc <- paste0(UE_mot_cmc,as.character(i)) # OK
    datos[idx,unionEndMotive_cmc] <- 0 # OK
    unionEnd_cmc <- paste0(UE_cmc,as.character(i)) # OK
    datos[idx,unionEnd_cmc] <- NA # OK
    datos[[unionEndMotive_cmc]] <- factor (datos[[unionEndMotive_cmc]], levels=c(0,1,2),
                                          labels=c("in union", "separation", "widowhood"))
    unionStart_type <- paste0(US_type,as.character(i)) # OK
    datos[[unionStart_type]] <- NA
    idx <- (!is.na(datos[[unionStart_cmc]]))&(is.na(datos[[marStart_cmc]]))
    idx [is.na(idx)] <- FALSE
    datos[idx,unionStart_type] <- 1
    idx <- (datos[[unionStart_cmc]]==datos[[marStart_cmc]])
    idx [is.na(idx)] <- FALSE
    datos[idx,unionStart_type] <- 2
    idx <- (datos[[unionStart_cmc]] < datos[[marStart_cmc]])
    idx [is.na(idx)] <- FALSE
    datos[idx,unionStart_type] <- 3
    datos[[unionStart_type]] <- factor (datos[[unionStart_type]], levels=c(1,2,3),
                                          labels=c("cohabitation", "marriage", "cohabitation before marriage"))
  }
  
  #### clean union history ####
  # 1. Define the stems exactly as you want them
  stems <- c("union_start_type", "union_start_cmc", "marriage_start_cmc", 
             "union_end_motive", "union_end_cmc")
  
  # 2. Clean and Shift
  cleaned_unions <- datos %>%
    mutate(temp_id = row_number()) %>%
    # Select ONLY the ID and the columns that match your specific variables
    # This prevents unwanted variables from entering the pivot
    select(temp_id, matches("^(union_start|union_end|marriage).*\\d+$")) %>% 
    pivot_longer(
      cols = -temp_id,
      names_to = c(".value", "original_order"),
      names_pattern = "(.*?)(\\d+)$" 
    ) %>%
    # We removed the .value filter because the columns are now named after the stems
    filter(!is.na(union_start_cmc)) %>% 
    group_by(temp_id) %>%
    mutate(
      new_order = row_number(),
      total_unions = n()
    ) %>%
    ungroup() %>%
    select(-original_order) %>%
    # Pivot back
    pivot_wider(
      names_from = new_order,
      values_from = all_of(stems),
      names_glue = "{.value}{new_order}"
    )
  
  # 3. Join back to the original 'datos'
  datos_final <- datos %>%
    mutate(temp_id = row_number()) %>%
    # Dynamically drop whatever union/marriage columns exist in THIS country
    select(-matches("^(union_start|union_end|marriage).*\\d+$")) %>% 
    # Bring in the cleaned/shifted data
    left_join(cleaned_unions, by = "temp_id") %>%
    # Fix the count for women with zero unions
    mutate(total_unions = replace_na(total_unions, 0)) %>%
    select(-temp_id)
  
  # 4. Clean number of unions
  datos_final$nUnion <- NULL
  datos <- datos_final %>%
    rename(nUnion=total_unions)
  
  #### Birth History ####
  maxB <- 25
  if ((!is.null(country))&&(country=="UK")) {
    # lhi26_X variables name end with _4301. Delete that
    mujeres <- mujeres %>%
      rename_with(
        ~ gsub("_4301$", "", .x), 
        .cols = matches("^lhi26_.*_4301$")
      )  
  }
  childType_code <- "lhi26_"
  childSexe_code <- "lhi28_"
  childMonthBirth_code <- "lhi29_m"
  childYearBirth_code <- "lhi29_y"
  sex <- "sex"
  dob_cmc <- "dob_cmc"
  for (j in (1:maxB)) {
    child_type <- paste0(childType_code,as.character(j)) # OK
    if (child_type %in% names (mujeres)) {
      datos[[child_type]] <- as.integer (mujeres[[child_type]]) # OK
      child_sexe_N <- paste0(childSexe_code,as.character(j)) # OK
      child_month_birth <- paste0(childMonthBirth_code,as.character(j)) # OK
      child_year_birth <- paste0(childYearBirth_code,as.character(j)) # OK
      child_sex <- paste0(sex,as.character(j)) # OK
      datos[[child_sex]] <- as.integer (mujeres[[child_sexe_N]]) # OK
      child_dob <- paste0(dob_cmc,as.character(j)) # OK
      datos[[child_dob]] <- compute_cmc ( mujeres[[child_month_birth]], mujeres[[child_year_birth]] ) # OK
    }
  }
  
  #### clean birth history: only biological children, sorted on date of birth, and with the number of bio kids
  # First remove the variable labels that create warnings
  datos <- zap_label(datos)
  
  # 1. Define the variables we want to keep
  child_stems <- c("lhi26_", "sex", "dob_cmc")
  
  # 2. Process Birth History
  cleaned_births <- datos %>%
    mutate(temp_id = row_number()) %>%
    # Select ID and any column ending in a digit
    select(temp_id, matches("\\d+$")) %>% 
    # Specifically filter columns that match our birth variables
    select(temp_id, matches("^(lhi26_|sex|dob_cmc)")) %>%
    pivot_longer(
      cols = -temp_id,
      names_to = c(".value", "original_order"),
      names_pattern = "(.*?)(\\d+)$"
    ) %>%
    # STEP A: Keep only biological children (Type 1)
    # STEP B: Remove rows where dob_cmc is missing
    filter(lhi26_ == 1, !is.na(dob_cmc)) %>%
    # STEP C: Ensure chronological order
    arrange(temp_id, dob_cmc) %>%
    group_by(temp_id) %>%
    mutate(
      new_birth_order = row_number(),
      total_bio_children = n()
    ) %>%
    ungroup() %>%
    select(-original_order) %>%
    # Pivot back to wide
    pivot_wider(
      names_from = new_birth_order,
      values_from = all_of(child_stems),
      names_glue = "{.value}{new_birth_order}"
    )
  
  # 3. Join back to original data
  datos <- datos %>%
    mutate(temp_id = row_number()) %>%
    # Drop old birth history columns (any that start with our stems and end in a digit)
    select(-matches("^(lhi26_|sex|dob_cmc)\\d+$")) %>%
    left_join(cleaned_births, by = "temp_id") %>%
    # Set count to 0 for women with no biological children
    mutate(total_bio_children = replace_na(total_bio_children, 0)) %>%
    select(-temp_id)
  
  datos <- datos %>%
    rename(nBioKids=total_bio_children)
  # Remove child type variables
  datos <- datos %>%
    select(-matches("^(lhi26_)"))
  
  datos <- relocate (datos, indiv_weight, .after=indiv_age_survey)
  datos <- relocate (datos, nBioKids, .after=indiv_weight)
  datos$nUnionBeforeLast <- NULL
  
  datos <- cleanENADID(datos)
  
  return (datos)
}

all_data <- GGS_list %>%
  imap(~ getDatosGGS2(mujeres = .x, country = .y))

all_diff <- all_data %>%
  imap(function(processed_df, country_name) {
    
    # Message to console so you can see progress
    message("Checking consistency for: ", country_name)
    
    checkGGS(
      info_df  = GGS_ENADID,
      country  = country_name,
      dfDaniel = processed_df,              # From all_data (.x)
      dfGGS2   = GGS_list[[country_name]]    # From GGS_list
    )
  })

GGS_ENADID <- join_with_harmonized()
GGS_ENADID <- compute_lastYear(GGS_ENADID)

save (GGS_ENADID, file=pathGGS_ENADID)
