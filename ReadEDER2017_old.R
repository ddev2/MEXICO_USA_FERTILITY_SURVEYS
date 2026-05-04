setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
# Read EDER 2017 file
library (tidyverse)
library (haven)
library (purrr)

path_EDER2017 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/EDER/2017/eder2017_bases_sav/historiavida.sav"))
path_EDER2017_antecedentes <- path.expand(paste0(rootPath,"/INEGI/Encuestas/EDER/2017/eder2017_bases_sav/antecedentes.sav"))
path_EDER_ENADID2017 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/EDER_ENADID2017.Rdat"))

if (!exists("EDER")) {
  EDER <- haven::read_sav(path_EDER2017)
} else {
  EDER$factor_per <- NULL
}
EDER$llave_muj <- paste0(EDER$folioviv, EDER$foliohog, EDER$id_pobla)
antecedentes <- haven::read_sav(path_EDER2017_antecedentes)
antecedentes$llave_muj <- paste0(antecedentes$folioviv, antecedentes$foliohog, antecedentes$id_pobla)

# add the weight
EDER <- EDER %>%
  dplyr::left_join(select(antecedentes, llave_muj, factor_per), by = "llave_muj")
rm(antecedentes)

EDER <- haven::zap_labels(EDER)
to_keep_char <- c("llave_muj","folioviv","foliohog","id_pobla","geo_eder",
             "nom_cony1","nom_cony2","nom_cony3","nom_cony4","nom_cony5","nom_cony6",
             "hij_nom_1","hij_nom_2","hij_nom_3","hij_nom_4","hij_nom_5","hij_nom_6",
             "hij_nom_7","hij_nom_8","hij_nom_9","hij_nom_10","hij_nom_11","hij_nom_12",
             "hij_nom_13","hij_nom_14","hij_nom_15") 

EDER <- EDER %>%
  dplyr::mutate(across(
    .cols = -all_of(to_keep_char), 
    .fns = ~ {
      as.integer(.x)
    }
  ))
EDER <- subset(EDER, (sexo==2))
# first row for each woman
EDER_one <- EDER[!duplicated(EDER$llave_muj), ]

n = nrow(EDER_one)
EDER_ENADID <- data.frame(country=rep("MEXICO",n),survey=rep("EDER2017",n))
EDER_ENADID$llave_muj <- EDER_one$llave_muj
EDER_ENADID$surveyDate_cmc <- compute_cmc(1,2018) # "del 3 de julio al 31 de diciembre de 2017". We cut at January 2018
EDER_ENADID <- compute_lastYear(EDER_ENADID)
EDER_ENADID$indiv_dob_cmc <- compute_cmc(EDER_one$mes_nac, EDER_one$anio_nac)
EDER_ENADID$yBirth <- EDER_one$anio_nac
EDER_ENADID$indiv_age_survey <- EDER_one$edad_act
EDER_ENADID$indiv_weight <- EDER_one$factor_per
EDER_ENADID$pregnant <- NA
EDER_ENADID$want_another <- NA
EDER_ENADID$age_first_sex <- EDER_one$edad_sex
EDER_ENADID$ever_contraception <- NA

EDER_ENADID$nUnion <- EDER_one$matrimonio
#### Union History ####
extract_union_data <- function(codes, years) {
  # 1. Identify indices where the union is active (code > 0)
  active_idx <- which(!is.na(codes) & codes > 0)
  
  if (length(active_idx) == 0) {
    return(list(type = NA, start_u = NA, start_u_code = NA, start_m = NA, start_m_code = NA, end_u = NA, diss_type = "None"))
  }
  
  # --- START AND MARRIAGE DATES ---
  first_idx <- min(active_idx)
  start_union_yr <- years[first_idx]
  start_union_code <- codes[first_idx]
  
  # Identify if marriage ever occurs in this union
  marr_idx <- which(codes %in% c(2, 3, 4, 12, 13, 20, 26, 27, 28, 30, 37, 40, 46, 47, 48, 126))
  start_marr_yr <- if(length(marr_idx) > 0) years[min(marr_idx)] else NA
  start_marr_code <- if(length(marr_idx) > 0) codes[min(marr_idx)] else NA
  
  # --- UNION TYPE ---
  starts_with_cohab <- codes[first_idx] %in% c(1, 10, 12, 13, 17, 18, 126)
  has_marriage      <- !is.na(start_marr_yr)
  
  u_type <- case_when(
    starts_with_cohab & has_marriage ~ "cohabitation before marriage",
    starts_with_cohab & !has_marriage ~ "cohabitation ",
    !starts_with_cohab & has_marriage ~ "marriage",
    TRUE ~ NA
  )
  
  # --- DISSOLUTION ---
  # Find the first occurrence of a dissolution code
  diss_idx <- which(codes %in% c(6, 7, 8, 17, 18, 26, 27, 28, 37, 46, 47, 48, 60, 70, 80, 126))
  
  if (length(diss_idx) > 0) {
    first_diss_idx <- min(diss_idx)
    end_union_yr <- years[first_diss_idx]
    
    # Determine type based on the first dissolution code seen
    final_code <- codes[first_diss_idx]
    d_type <- case_when(
      final_code %in% c(6, 60, 26, 46, 126) ~ "separation", # Divorce codes
      final_code %in% c(7, 70, 17, 27, 37, 47) ~ "separation", # Separation codes
      final_code %in% c(8, 80, 18, 28, 48) ~ "widowhood",
      TRUE ~ "in union" # this one will be changed afterward, to set to NA the cases of women who didn't enter a union of order i
    )
  } else {
    end_union_yr <- NA
    d_type <- "in union" # idem
  }
  
  return(list(type = u_type, start_u = start_union_yr, start_u_code = start_union_code,
              start_m = start_marr_yr, start_m_code = start_marr_code, 
              end_u = end_union_yr, diss_type = d_type))
}

df_unions_final <- EDER %>%
  group_by(llave_muj) %>%
  group_modify(~ {
    # .x represents the data for the current woman
    data_woman <- .x 
    
    # Process the 5 unions and bind them into a single row
    map_dfc(1:5, function(i) {
      col_name <- paste0("edo_civil", i)
      
      # Now we pull the columns directly from data_woman
      res <- extract_union_data(data_woman[[col_name]], data_woman$anio_retro)
      
      # Set the names for the 5 resulting columns
      set_names(res, c(paste0("union_start_type", i), 
                       paste0("yStartUnion_", i), 
                       paste0("yStartUnionCode_", i), 
                       paste0("yStartMarr_", i), 
                       paste0("yStartMarrCode_", i), 
                       paste0("yEndUnion_", i), 
                       paste0("union_end_motive", i)))
    })
  }) %>%
  ungroup()

# now compute cmc for union, adjusting for the cases when there are two or three events the same year
# 1 Inicio Unión libre
# 2 Inicio Matrimonio civil
# 3 Inicio Matrimonio religioso
# 4 Inicio Matrimonio civil y religioso
# 6 Inicio Divorcio
# 7 Inicio Separación
# 8 Fallecimiento del cónyuge
# 10 Unión libre
# ==> 12 Inicio Matrimonio religioso posterior a Inicio de Unión libre
# ==> 13 Inicio Matrimonio civil y religioso posterior a Inicio de Unión libre
# ==> 17 Inicio Separación posterior a Inicio de Unión libre
# ==> 18 Fallecimiento del cónyuge posterior a Inicio de Unión libre
# 20 Matrimonio civil
# ==> 26 Inicio Matrimonio civil e Inicio Divorcio
# ==> 27 Inicio Matrimonio civil e Inicio Divorcio
# ==> 28 Inicio Matrimonio civil y Fallecimiento del cónyuge
# 30 Matrimonio religioso
# ==> 37 Matrimonio religioso e Inicio Separación
# 40 Matrimonio civil y religioso
# ==> 46 Matrimonio civil y religioso e Inicio Divorcio
# ==> 47 Matrimonio civil y religioso e Inicio Separación
# ==> 48 Matrimonio civil y religioso y Fallecimiento del cónyuge
# 60 Está divorciado
# 70 Está separado
# 80 Está viudo
# ==> 126 Inicio Matrimonio civil posterior a Inicio de Unión libre e Inicio Divorcio

for (u in (1:5)) {
  Ustart <- paste0("union_start_cmc",u)
  Ustart_I <- paste0("union_start_cmc_I",u)
  UstartType <- paste0("union_start_type",u)
  Uend <- paste0("union_end_cmc",u)
  Uend_I <- paste0("union_end_cmc_I",u)
  UendMotive <- paste0("union_end_motive",u)
  Mstart <- paste0("marriage_start_cmc",u)
  Mstart_I <- paste0("marriage_start_cmc_I",u)
  
  Ustart_year <- paste0("yStartUnion_",u)
  Ustart_code <- paste0("yStartUnionCode_",u)
  Uend_year <- paste0("yEndUnion_",u)
  Mstart_year <- paste0("yStartMarr_",u)
  Mstart_code <- paste0("yStartMarrCode_",u)
  
  df_unions_final[[Ustart]] <- ifelse(!is.na(df_unions_final[[Ustart_year]]), compute_cmc(imputed_month(nrow(df_unions_final)), df_unions_final[[Ustart_year]]), NA)
  df_unions_final[[Uend]] <- ifelse(!is.na(df_unions_final[[Uend_year]]), compute_cmc(imputed_month(nrow(df_unions_final)), df_unions_final[[Uend_year]]), NA)
  df_unions_final[[UendMotive]] <- ifelse(!is.na(df_unions_final[[Ustart_year]]), df_unions_final[[UendMotive]], NA)
  df_unions_final[[Mstart]] <- ifelse(!is.na(df_unions_final[[Mstart_year]]), compute_cmc(imputed_month(nrow(df_unions_final)), df_unions_final[[Mstart_year]]), NA)
  idx <- !is.na(df_unions_final[[Ustart_year]])&!is.na(df_unions_final[[Mstart_year]])&(df_unions_final[[Mstart_year]] == df_unions_final[[Ustart_year]])
  df_unions_final[[Mstart]][idx] <- df_unions_final[[Ustart]][idx]
  
  # cases of two events in the same year: we separate by 6 months
  # ==> 12 Inicio Matrimonio religioso posterior a Inicio de Unión libre
  df_unions_final[[Ustart]] <- ifelse(df_unions_final[[Ustart_code]] == 12, compute_cmc(imputed_month(nrow(df_unions_final),range=(1:6)), df_unions_final[[Ustart_year]]), df_unions_final[[Ustart]])
  df_unions_final[[Mstart]] <- ifelse(df_unions_final[[Ustart_code]] == 12, compute_cmc(imputed_month(nrow(df_unions_final),range=(7:12)), df_unions_final[[Mstart_year]]), df_unions_final[[Mstart]])
  # ==> 13 Inicio Matrimonio civil y religioso posterior a Inicio de Unión libre
  df_unions_final[[Ustart]] <- ifelse(df_unions_final[[Ustart_code]] == 13, compute_cmc(imputed_month(nrow(df_unions_final),range=(1:6)), df_unions_final[[Ustart_year]]), df_unions_final[[Ustart]])
  df_unions_final[[Mstart]] <- ifelse(df_unions_final[[Ustart_code]] == 13, compute_cmc(imputed_month(nrow(df_unions_final),range=(7:12)), df_unions_final[[Mstart_year]]), df_unions_final[[Mstart]])
  # ==> 17 Inicio Separación posterior a Inicio de Unión libre
  df_unions_final[[Ustart]] <- ifelse(df_unions_final[[Ustart_code]] == 17, compute_cmc(imputed_month(nrow(df_unions_final),range=(1:6)), df_unions_final[[Ustart_year]]), df_unions_final[[Ustart]])
  df_unions_final[[Uend]] <- ifelse(df_unions_final[[Ustart_code]] == 17, compute_cmc(imputed_month(nrow(df_unions_final),range=(7:12)), df_unions_final[[Uend_year]]), df_unions_final[[Uend]])
  # ==> 18 Fallecimiento del cónyuge posterior a Inicio de Unión libre
  df_unions_final[[Ustart]] <- ifelse(df_unions_final[[Ustart_code]] == 18, compute_cmc(imputed_month(nrow(df_unions_final),range=(1:6)), df_unions_final[[Ustart_year]]), df_unions_final[[Ustart]])
  df_unions_final[[Uend]] <- ifelse(df_unions_final[[Ustart_code]] == 18, compute_cmc(imputed_month(nrow(df_unions_final),range=(7:12)), df_unions_final[[Uend_year]]), df_unions_final[[Uend]])
  # ==> 26 Inicio Matrimonio civil e Inicio Divorcio
  # ==> 27 Inicio Matrimonio civil e Inicio Divorcio
  # ==> 28 Inicio Matrimonio civil y Fallecimiento del cónyuge
  # ==> 37 Matrimonio religioso e Inicio Separación
  # ==> 46 Matrimonio civil y religioso e Inicio Divorcio
  # ==> 47 Matrimonio civil y religioso e Inicio Separación
  # ==> 48 Matrimonio civil y religioso y Fallecimiento del cónyuge
  idx <- (df_unions_final[[Ustart_year]] == df_unions_final[[Mstart_year]]) & (df_unions_final[[Ustart_code]] %in% c(26,27,28,37,46,47,48))
  idx [is.na(idx)] <- FALSE
  df_unions_final[[Ustart]][idx] <- compute_cmc(imputed_month(sum(idx),range=(1:6)), df_unions_final[[Ustart_year]][idx])
  df_unions_final[[Mstart]] <- ifelse(df_unions_final[[Ustart_code]] %in% c(26,27,28,37,46,47,48), compute_cmc(imputed_month(nrow(df_unions_final),range=(1:6)), df_unions_final[[Mstart_year]]), df_unions_final[[Mstart]])
  df_unions_final[[Uend]] <- ifelse(df_unions_final[[Ustart_code]] %in% c(26,27,28,37,46,47,48), compute_cmc(imputed_month(nrow(df_unions_final),range=(7:12)), df_unions_final[[Uend_year]]), df_unions_final[[Uend]])
  
  # case of three events in the same year: we separate by 3 months
  # ==> 126 Inicio Matrimonio civil posterior a Inicio de Unión libre e Inicio Divorcio
  df_unions_final[[Ustart]] <- ifelse(df_unions_final[[Ustart_code]] == 126, compute_cmc(imputed_month(nrow(df_unions_final),range=(1:4)), df_unions_final[[Ustart_year]]), df_unions_final[[Ustart]])
  df_unions_final[[Mstart]] <- ifelse(df_unions_final[[Ustart_code]] == 126, compute_cmc(imputed_month(nrow(df_unions_final),range=(5:8)), df_unions_final[[Mstart_year]]), df_unions_final[[Mstart]])
  df_unions_final[[Uend]] <- ifelse(df_unions_final[[Ustart_code]] == 126, compute_cmc(imputed_month(nrow(df_unions_final),range=(9:12)), df_unions_final[[Uend_year]]), df_unions_final[[Uend]])
  
  # removing unneeded columns
  df_unions_final[[Ustart_year]] <- NULL
  df_unions_final[[Ustart_code]] <- NULL
  df_unions_final[[Mstart_year]] <- NULL
  df_unions_final[[Mstart_code]] <- NULL
  df_unions_final[[Uend_year]] <- NULL
  
  # month imputed
  df_unions_final[[Ustart_I]] <- ifelse(is.na(df_unions_final[[Ustart]]),NA,1)
  df_unions_final[[Uend_I]] <- ifelse(is.na(df_unions_final[[Uend]]),NA,1)
  df_unions_final[[Mstart_I]] <- ifelse(is.na(df_unions_final[[Mstart]]),NA,1)

    # relocate
  df_unions_final <- relocate(df_unions_final, all_of (c(Ustart, Uend, Mstart, UendMotive)), .after = all_of (UstartType))
}

EDER_ENADID <- EDER_ENADID %>%
  dplyr::left_join(df_unions_final, by = "llave_muj")
rm(df_unions_final)

EDER_ENADID$nBioKids <- EDER_one$hij_vivos
#### Birth History ####
library(data.table)
setDT(EDER)

# Define the columns we are looking at
gen_cols <- paste0("hij_gen_", 1:15)
vid_cols <- paste0("hij_vid_", 1:15)

df_summary <- EDER[, {
  # Create a list to hold the new variables
  results <- list()
  for (i in 1:15) {
    col <- gen_cols[i]
    # Find the first row where child i appears
    idx <- which(!is.na(get(col)) & get(col) != 0)
    
    if (length(idx) > 0) {
      first_row <- idx[which.min(anio_retro[idx])]
      results[[paste0("dob_cmc", i)]] <- compute_cmc(imputed_month(length(first_row)),anio_retro[first_row])
      results[[paste0("dod_cmc", i)]] <- NA_integer_
      results[[paste0("dob_cmc_I", i)]] <- NA_integer_
      results[[paste0("dod_cmc_I", i)]] <- NA_integer_
      results[[paste0("sex", i)]]    <- get(col)[first_row]
    } else {
      results[[paste0("dob_cmc", i)]] <- NA_integer_
      results[[paste0("dod_cmc", i)]] <- NA_integer_
      results[[paste0("dob_cmc_I", i)]] <- NA_integer_
      results[[paste0("dod_cmc_I", i)]] <- NA_integer_
      results[[paste0("sex", i)]]    <- NA_integer_
    }
    # Find the row where child i dies
    col <- vid_cols[i]
    idx <- which(!is.na(get(col)) & get(col) == 7)

    if (length(idx) > 0) {
      results[[paste0("dod_cmc", i)]] <- compute_cmc(imputed_month(length(idx)),anio_retro[idx])
    }
    results[[paste0("dob_cmc_I", i)]] <- ifelse(is.na(results[[paste0("dob_cmc", i)]]), NA_integer_, 1L)
    results[[paste0("dod_cmc_I", i)]] <- ifelse(is.na(results[[paste0("dod_cmc", i)]]), NA_integer_, 1L)    
  }
  results
}, by = llave_muj]

EDER_ENADID <- EDER_ENADID %>%
  dplyr::left_join(df_summary, by = "llave_muj")
rm(df_summary)

EDER_ENADID <- cleanENADID(EDER_ENADID)
EDER_ENADID <- reorder_birthHistory(EDER_ENADID)

save(EDER_ENADID, file=path_EDER_ENADID2017)
rm(EDER)
