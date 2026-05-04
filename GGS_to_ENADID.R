setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
# Read harmonized GGS (and others) files
library (tidyverse)
library (haven)
library (purrr)

rootPathGGS <- "~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Documents/Travail/Demographic Surveys"
pathHarmHist1 <- path.expand(paste0(rootPathGGS,"/GGS/Harmonized Histories/Harmonized_Histories/HARMONIZED-HISTORIES_ALL_GGSaccess.dta"))
pathHarmHist2 <- path.expand(paste0(rootPathGGS,"/GGS/Harmonized Histories/Harmonized_Histories_I/HARMONIZED-HISTORIES_I.dta"))
pathHarmHist3 <- path.expand(paste0(rootPathGGS,"/GGS/Harmonized Histories/HarmonizedHistories_II/HarmonizedHistoriesII_2025_09_01.dta"))

pathGGS_ENADID <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/GGS_ENADID.Rdat"))

GGS1 <- haven::read_dta(pathHarmHist1) # GGS
GGS2 <- haven::read_dta(pathHarmHist2) # GGS
GGS3 <- haven::read_dta(pathHarmHist3) # GGS2

GGS_to_ENADID <- function (GGS=GGS1, survey="GGS1") {
  GGS <- subset(GGS, SEX==2)
  n <- nrow(GGS)
  GGS_ENADID <- data.frame(country=rep("DUMMY",n),survey=rep(survey,n))
  if (survey=="GGS1") {
    GGS_ENADID$country <- factor(GGS$COUNTRY,
                                 levels=c(401,561,1001,2031,2331,2501,2681,2762,2761,3481,3801,4401,5281,5781,6162,6421,6431,7241,7521,
                                          8261,8401,8402),
                                 labels=c("AUSTRIA","BELGIUM","BULGARIA", "CZECHIA","ESTONIA","FRANCE","GEORGIA","GERMANY","GERMANY",
                                          "HUNGARY","ITALY","LITHUANIA","NETHERLANDS","NORWAY","POLAND","ROMANIA","RUSSIA","SPAIN","SWEDEN",
                                          "UK","USA","USA"))
    GGS_ENADID$survey <- "GGS"
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==2762,"PairFam",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==5271,"FFS",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==7241,"SFS2006",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==8261,"BHPS",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==8401,"NSFG1995",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==8402,"NSFG2007",GGS_ENADID$survey)
  } else if (survey=="GGS2") {
    GGS_ENADID$country <- factor(GGS$COUNTRY,
                                 levels=c(1121,1242,1241,8601,4981,5282,7242,8581),
                                 labels=c("BELARUS","CANADA","CANADA","KAZAKHSTAN","MOLDOVA","NETHERLANDS","SPAIN","URUGUAY"))
    GGS_ENADID$survey <- "GGS"
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==1242,"GSS2011",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==4981,"GGS2",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==5282,"OG2013",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==7242,"SFS2018",GGS_ENADID$survey)
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==8581,"ENCoR",GGS_ENADID$survey)
  } else if (survey=="GGS3") {
    GGS_ENADID$country <- factor(GGS$COUNTRY,
                                 levels=c(402,1911,2032,2081,2332,2461,2763,3802,5782,8262,8582),
                                 labels=c("AUSTRIA","CROATIA","CZECHIA","DENMARK","ESTONIA","FINLAND","GERMANY","ITALY","NORWAY","UK","URUGUAY"))
    GGS_ENADID$survey <- "GGS2"
    GGS_ENADID$survey <- ifelse(GGS$COUNTRY==3802,"FSS2016",GGS_ENADID$survey)
  }
  GGS_ENADID$llave_muj <- GGS$ARID # GGS id for individuals
  GGS_ENADID$ind_harmonized <- GGS$RESPID # Harmonized Histories id

  GGS_ENADID$surveyDate_cmc <- compute_cmc (GGS$IMONTH_S, GGS$YEAR_S)
  GGS_ENADID$indiv_dob_cmc <- compute_cmc (GGS$IBORN_M, GGS$BORN_Y)
  idxIta <- GGS$COUNTRY %in% c(3802) # Italy FSS2016
  GGS_ENADID$surveyDate_cmc[idxIta] <- compute_cmc (GGS$MONTH_S[idxIta], GGS$YEAR_S[idxIta])
  GGS_ENADID$indiv_dob_cmc[idxIta] <- compute_cmc (GGS$BORN_M[idxIta], GGS$BORN_Y[idxIta])
  idxIta <- GGS$COUNTRY %in% c(3801) # Italy GGS
  GGS_ENADID$indiv_dob_cmc[idxIta] <- GGS_ENADID$surveyDate_cmc[idxIta] - GGS$AGE_R[idxIta] * 12 - 6

  GGS_ENADID$yBirth <- GGS$BORN_Y
  GGS_ENADID$yBirth[idxIta] <- 1900 + trunc ((GGS_ENADID$indiv_dob_cmc[idxIta] - 1) / 12)
  GGS_ENADID$indiv_age_survey <- trunc ((GGS_ENADID$surveyDate_cmc - GGS_ENADID$indiv_dob_cmc)/12)
  GGS_ENADID$indiv_weight <- GGS$PERSWGT
  GGS <- GGS %>%
    dplyr::mutate(nBioKids = rowSums(!is.na(across(matches("^KID_Y\\d+$")))))
  GGS_ENADID$nBioKids <- GGS$nBioKids
  GGS_ENADID$pregnant <- NA # in original file
  GGS_ENADID$want_another <- NA # in original file
  GGS_ENADID$ideal_number <- NA # in original file
  GGS_ENADID$age_first_sex <- NA # in original file
  GGS_ENADID$ever_contraception <- NA # in original file
  GGS_ENADID$union_status <- NA # in original file
  GGS_ENADID$nUnion <- GGS$UNINUM

  #### Union Histories ####
  # copy info for union / marriage
  maxU  <- max(GGS_ENADID$nUnion, na.rm = TRUE)
  
  UnionErrorCases <- c("MBefU", #1
                       "MDateNoUDate", #2
                       "USBefPrecEnd", #3
                       "NoMDate", #4
                       "ImpMonthCorr", #5
                       "MBef14", #6
                       "UBef14", #7
                       "UDateNoMDate") #8#**
  for (u in 1:maxU) {
    # place holders
    to <- paste0("union_start_type", u)
    GGS_ENADID[[to]] <- NA
    checkIt <- paste0("checkUnionInfo", u)
    GGS_ENADID[[checkIt]] <- vector("list", nrow(GGS_ENADID))
    GGS_ENADID[[checkIt]] <- lapply(GGS_ENADID[[checkIt]], function(x) character(0))
    
    marr <- paste0("MARR_", u)
    union_m <- paste0("UNION_M", u)
    marr_m <- paste0("MARR_M", u)
    sep_m <- paste0("SEP_M", u)
    imp_union_m <- paste0("IUNION_M", u)
    imp_marr_m <- paste0("IMARR_M", u)
    imp_sep_m <- paste0("ISEP_M", u)
    union_y <- paste0("UNION_Y", u)
    marr_y <- paste0("MARR_Y", u)

    # if there is a month of union and no imputed month (case of Italy)
    # we update the imputed month
    idx <- (!is.na(GGS[[union_m]]))&(is.na(GGS[[imp_union_m]]))
    GGS[[imp_union_m]][idx] <- GGS[[union_m]][idx]
    # if there is a month of marriage and no imputed month (case of Italy)
    # we update the imputed month
    idx <- (!is.na(GGS[[marr_m]]))&(is.na(GGS[[imp_marr_m]]))
    GGS[[imp_marr_m]][idx] <- GGS[[marr_m]][idx]
    # if there is a month of union dissolution and no imputed month (case of Italy)
    # we update the imputed month
    idx <- (!is.na(GGS[[sep_m]]))&(is.na(GGS[[imp_sep_m]]))
    GGS[[imp_sep_m]][idx] <- GGS[[sep_m]][idx]
    
    # if there is a marriage but the date of marriage is missing, we recopy from the union
    idx <- (GGS[[marr]]==1)&(is.na(GGS[[marr_y]]))&(!is.na(GGS[[union_y]]))
    idx[is.na(idx)] <- FALSE
    GGS[[marr_y]][idx] <- GGS[[union_y]][idx]
    GGS[[imp_marr_m]][idx] <- GGS[[imp_union_m]][idx]
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[4]))
    
    # if both month of union and month of marriage are imputed, and they are in the same year,
    # they should have the same month value (which was imputed at random)!!
    # 1. case both month of union and marriage are NA and are imputed
    # idx1 <- ((is.na(GGS$UNION_M1))&(is.na(GGS$MARR_M1)))&(GGS$UNION_Y1==GGS$MARR_Y1)&((!is.na(GGS$IUNION_M1))&(!is.na(GGS$IMARR_M1)))
    idx1 <- ((is.na(GGS[[union_m]]))&(is.na(GGS[[marr_m]])))&
      (GGS[[union_y]]==GGS[[marr_y]])&
      ((!is.na(GGS[[imp_union_m]]))&(!is.na(GGS[[imp_marr_m]])))
    idx1[is.na(idx1)] <- FALSE
    # 2. case month of union is imputed and month of marriage exists
    # idx2 <- ((is.na(GGS$UNION_M1))&(!is.na(GGS$MARR_M1)))&(GGS$UNION_Y1==GGS$MARR_Y1)
    idx2 <- ((is.na(GGS[[union_m]]))&(!is.na(GGS[[marr_m]])))&(GGS[[union_y]]==GGS[[marr_y]])
    idx2[is.na(idx2)] <- FALSE
    # 3. case month of marriage is imputed and month of union exists
    # idx3 <- ((!is.na(GGS$UNION_M1))&(is.na(GGS$MARR_M1)))&(GGS$UNION_Y1==GGS$MARR_Y1)
    idx3 <- ((!is.na(GGS[[union_m]]))&(is.na(GGS[[marr_m]])))&(GGS[[union_y]]==GGS[[marr_y]])
    idx3[is.na(idx3)] <- FALSE
    idx <- idx1 | idx2 | idx3
    idx[is.na(idx)] <- FALSE
    GGS[[imp_union_m]][idx] <- GGS[[imp_marr_m]][idx]
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[5]))
    
    # GGS_ENADID$union_start_cmc1 <- compute_cmc(GGS$IUNION_M1, GGS$UNION_Y1)
    # GGS_ENADID$union_start_cmc1 <- ifelse((GGS$COUNTRY==8261)&(is.na(GGS_ENADID$union_start_cmc1)),
    #                                       compute_cmc(GGS$LTUNION_M1, GGS$LTUNION_Y1),GGS_ENADID$union_start_cmc1)
    cmc_Ustart    <- paste0("union_start_cmc", u)
    month_from <- paste0("IUNION_M", u)
    year_from  <- paste0("UNION_Y", u)
    ltmonth_from <- paste0("LTUNION_M", u)
    ltyear_from  <- paste0("LTUNION_Y", u)
    
    GGS_ENADID[[cmc_Ustart]] <- compute_cmc(GGS[[month_from]], GGS[[year_from]])
    GGS_ENADID[[cmc_Ustart]] <- ifelse((GGS$COUNTRY==8261)&(is.na(GGS_ENADID[[cmc_Ustart]])),
                                          compute_cmc(GGS[[ltmonth_from]], GGS[[ltyear_from]]),GGS_ENADID[[cmc_Ustart]])

    # GGS_ENADID$union_end_cmc1 <- compute_cmc(GGS$ISEP_M1, GGS$SEP_Y1)
    cmc_Uend    <- paste0("union_end_cmc", u)
    month_from <- paste0("ISEP_M", u)
    year_from  <- paste0("SEP_Y", u)

    GGS_ENADID[[cmc_Uend]] <- compute_cmc(GGS[[month_from]], GGS[[year_from]])
    
    # GGS_ENADID$union_end_motive1 <- GGS$SEP_1
    # GGS_ENADID$union_end_motive1 <- factor(GGS_ENADID$union_end_motive1,
    #                                        levels=c(0,1,2,2001,2901),
    #                                        labels=c("in union","separation","widowhood","unknown","unknown"))
    to <- paste0("union_end_motive", u)
    from <- paste0("SEP_", u)

    GGS_ENADID[[to]] <- GGS[[from]]
    # exclude non-union: if no date of union start, set to NA
    GGS_ENADID[[to]] <- ifelse(is.na(GGS_ENADID[[cmc_Ustart]]), NA, GGS_ENADID[[to]])
    GGS_ENADID[[to]] <- factor(GGS_ENADID[[to]],
                                           levels=c(0,1,2,2001,2901),
                                           labels=c("in union","separation","widowhood","unknown","unknown"))

    # GGS_ENADID$marriage_start_cmc1 <- compute_cmc(GGS$IMARR_M1, GGS$MARR_Y1)
    # GGS_ENADID$marriage_start_cmc1 <- ifelse((GGS$COUNTRY==8261)&(is.na(GGS_ENADID$marriage_start_cmc1)),
    #                                          compute_cmc(GGS$LTMARR_M1, GGS$LTMARR_Y1),GGS_ENADID$marriage_start_cmc1)
    cmc_Mstart    <- paste0("marriage_start_cmc", u)
    month_from <- paste0("IMARR_M", u)
    year_from  <- paste0("MARR_Y", u)
    ltmonth_from <- paste0("LTMARR_M", u)
    ltyear_from  <- paste0("LTMARR_Y", u)
    
    GGS_ENADID[[cmc_Mstart]] <- compute_cmc(GGS[[month_from]], GGS[[year_from]])
    GGS_ENADID[[cmc_Mstart]] <- ifelse((GGS$COUNTRY==8261)&(is.na(GGS_ENADID[[cmc_Mstart]])),
                                   compute_cmc(GGS[[ltmonth_from]], GGS[[ltyear_from]]),GGS_ENADID[[cmc_Mstart]])

    toMarr <- paste0("marriage_start_cmc", u)
    toUnion <- paste0("union_start_cmc", u)

    # 1. check whether  date of union or  date of marriage are too early (before age 14)
    idx <- (GGS_ENADID[[toMarr]] < GGS_ENADID$indiv_dob_cmc + 12 * 12)
    idx[is.na(idx)] <- FALSE
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[6]))
    idx <- (GGS_ENADID[[toUnion]] < GGS_ENADID$indiv_dob_cmc + 12 * 12)
    idx[is.na(idx)] <- FALSE
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[7]))
    
    # 2. marriage date and no union date
    idx <- ((!is.na(GGS_ENADID[[toMarr]]))&(is.na(GGS_ENADID[[toUnion]])))
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[2]))
    GGS_ENADID[[toUnion]][idx] <- GGS_ENADID[[toMarr]][idx] # copy marriage data...
    
    # Type of union
    to <- paste0("union_start_type", u)
    test1 <- paste0("marriage_start_cmc", u)
    test2 <- paste0("union_start_cmc", u)

    idx <- (!is.na(GGS_ENADID[[test1]]))
    GGS_ENADID[[to]][idx] <- 1
    idx <- (is.na(GGS_ENADID[[test1]]))&(!is.na(GGS_ENADID[[test2]]))
    GGS_ENADID[[to]][idx] <- 2
    idx <- (GGS_ENADID[[test2]] < GGS_ENADID[[test1]])
    GGS_ENADID[[to]][idx] <- 3

    GGS_ENADID[[to]] <- factor(GGS_ENADID[[to]], levels = c(1,2,3),
                                          labels = c("marriage", "cohabitation", "cohabitation before marriage"))

  }  
  
  #### Check the Union Histories ####
  for (u in 1:maxU) {
    checkIt <- paste0("checkUnionInfo", u)
    unionStart <- paste0("union_start_cmc", u)
    marrStart <- paste0("marriage_start_cmc", u)
    unionEnd <- paste0("union_end_cmc", u)
    unionEndBefore <- paste0("union_end_cmc", u-1)
    unionEndMot <- paste0("union_end_motive", u)
    
    # 1. marriage before union
    idx <- ((!is.na(GGS_ENADID[[marrStart]]))&(!is.na(GGS_ENADID[[unionStart]])))&
      (GGS_ENADID[[unionStart]]>GGS_ENADID[[marrStart]])
    idx[is.na(idx)] <- FALSE
    # Correct union date if the difference is less than one year
    idx2 <- (GGS_ENADID[[unionStart]] > GGS_ENADID[[marrStart]])&(GGS_ENADID[[unionStart]] < GGS_ENADID[[marrStart]] + 12)
    idx2[is.na(idx2)] <- FALSE
    GGS_ENADID[[unionStart]][idx2] <- GGS_ENADID[[marrStart]][idx2]
    idx <- idx & (!idx2)
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[1]))
    # 2. union start before previous union end
    idx <- (u>1)&
      ((!is.na(GGS_ENADID[[unionStart]]))&(!is.na(GGS_ENADID[[unionEndBefore]])))&
      (GGS_ENADID[[unionStart]]<GGS_ENADID[[unionEndBefore]])
    idx[is.na(idx)] <- FALSE
    GGS_ENADID[[checkIt]][idx] <- map(GGS_ENADID[[checkIt]][idx], ~ union(.x, UnionErrorCases[3]))  }
  
  #### Birth Histories ####
  # GGS_ENADID$sex1 <- GGS$KID_S1
  # GGS_ENADID$dob_cmc1 <- compute_cmc(GGS$IKID_M1, GGS$KID_Y1)
  maxK  <- max(GGS_ENADID$nBioKids, na.rm = TRUE)
  for (k in 1:maxK) {
    # Column names
    sex_to   <- paste0("sex", k)
    sex_from <- paste0("KID_S", k)
    
    cmc_dob    <- paste0("dob_cmc", k)
    month_from <- paste0("IKID_M", k)
    year_from  <- paste0("KID_Y", k)
    
    # 1. Direct assignment (Vectorized)
    GGS_ENADID[[sex_to]] <- GGS[[sex_from]]
    
    # 2. Compute CMC for the entire column at once
    # Ensure compute_cmc is vectorized (it should be if it uses simple math)
    GGS_ENADID[[cmc_dob]] <- compute_cmc(GGS[[month_from]], GGS[[year_from]])
  }
  
  return (GGS_ENADID)
}

GGS_ENADID1 <- GGS_to_ENADID()
GGS_ENADID2 <- GGS_to_ENADID(GGS=GGS2, survey="GGS2")
GGS_ENADID <- bind_rows(GGS_ENADID1, GGS_ENADID2)
GGS_ENADID3 <- GGS_to_ENADID(GGS=GGS3, survey="GGS3")
GGS_ENADID <- bind_rows(GGS_ENADID, GGS_ENADID3)
#GGS_ENADID <- chgLabels(GGS_ENADID)
GGS_ENADID <- reorder_birthHistory(GGS_ENADID)
GGS_ENADID$temp_id <- NULL
rm (GGS1)
rm (GGS2)
rm (GGS3)
rm (GGS_ENADID1)
rm (GGS_ENADID2)
rm (GGS_ENADID3)

GGS_ENADID <- cleanENADID(GGS_ENADID)
GGS_ENADID <- compute_lastYear(GGS_ENADID)

save(GGS_ENADID, file=pathGGS_ENADID)
#load(file=pathGGS_ENADID)