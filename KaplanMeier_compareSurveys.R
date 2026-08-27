# facetting using survey as variable: comparing across surveys
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
source("lib/KaplanMeierLib.R")
path_output_plots <- paste0(outputPath, "/")

#### MEXICO ####
# First union
MEX_union1_marr_comp <- subset(MEXICO_ENADID, !(survey %in% c("WFS","ENADID1992","ENADID2006")))
MEX_union1_marr_comp <- MEX_union1_marr_comp[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight", "popWeight")]
MEX_union1_marr_comp$yUnion1 <- yearFrom_cmc(MEX_union1_marr_comp$union_start_cmc1)

# Second union: only ENADID 1997, EDER 2017 and EDER 2025
MEX_union2_marr_comp <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017", "EDER2025")))
MEX_union2_marr_comp <- MEX_union2_marr_comp[,c("country","survey","surveyDate_cmc",
                                      "union_start_type2","union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","weight", "popWeight")]
MEX_union2_marr_comp$yUnion2 <- yearFrom_cmc(MEX_union2_marr_comp$union_start_cmc2)

##### 0. cleaning ... #####
# modify censored date for transition to union for marriage: if there is an end of union, it is the censored date
MEX_union1_marr_comp$varUnionCens1 <- ifelse(!is.na(MEX_union1_marr_comp$union_end_cmc1),
                                        MEX_union1_marr_comp$union_end_cmc1,MEX_union1_marr_comp$surveyDate_cmc)
MEX_union2_marr_comp$varUnionCens2 <- ifelse(!is.na(MEX_union2_marr_comp$union_end_cmc2),
                                        MEX_union2_marr_comp$union_end_cmc2,MEX_union2_marr_comp$surveyDate_cmc)

cohortsListMex_Marr_comp <- c(
  c(1990,1999),
  c(2000,2009)
)

##### 1. Union ==> Marriage #####
###### MEXICO WFS & ALL ENADID ######

plot_marriage_union1_mod_MEX_comp <- KaplanMeierPlot (df=MEX_union1_marr_comp, varEnter="union_start_cmc1",
                                                 varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1", 
                                                 varWeight=c("weight", "popWeight"),
                                                 varClass="survey", varCountry=NULL, vecCountry=NULL,
                                                 cohortsList=cohortsListMex_Marr_comp,
                                                 Title="Mexico: transition of first union to marriage, by union cohort",
                                                 xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
plot_marriage_union1_mod_MEX_comp
plot_marriage_union1_mod_MEX_comp + labs(title=NULL)

plot_marriage_union2_mod_MEX_comp <- KaplanMeierPlot (df=MEX_union2_marr_comp, varEnter="union_start_cmc2",
                                                 varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2",
                                                 varWeight=c("weight", "popWeight"),
                                                 varClass="survey", varCountry=NULL, vecCountry=NULL,
                                                 cohortsList=cohortsListMex_Marr_comp,
                                                 Title="Mexico: transition of second union to marriage, by union cohort",
                                                 xTitle = "Duration in years after second union", maxX = 20, confInt=TRUE)

plot_marriage_union2_mod_MEX_comp

##### 2. Union ==> Separation #####
# modify censored dates for widowhood
MEX_union1_sep_comp <- subset(MEXICO_ENADID, !(survey %in% c("ENADID1992",  "ENADID2006")))
MEX_union1_sep_comp <- MEX_union1_sep_comp[,c("country","survey","surveyDate_cmc",
                                    "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                    "yBirth","weight", "popWeight")]
MEX_union1_sep_comp$yUnion1 <- yearFrom_cmc(MEX_union1_sep_comp$union_start_cmc1)
MEX_union1_sep_comp$varSepCens1 <- ifelse((!is.na(MEX_union1_sep_comp$union_end_motive1)&(MEX_union1_sep_comp$union_end_motive1=="widowhood")),
                                          MEX_union1_sep_comp$union_end_cmc1,MEX_union1_sep_comp$surveyDate_cmc)
# widowhood is not the event, only separation is
MEX_union1_sep_comp$union_endBySep_cmc1 <- ifelse((!is.na(MEX_union1_sep_comp$union_end_motive1)&(MEX_union1_sep_comp$union_end_motive1=="widowhood")),
                                             NA,MEX_union1_sep_comp$union_end_cmc1)
# we have info for second unions only in ENADID 1997 and EDER 2017 and 2025...
# CREATE A NEW DATASET...
MEX_union2_sep_comp <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017", "EDER2025")))
MEX_union2_sep_comp <- MEX_union2_sep_comp[,c("country","survey","surveyDate_cmc",
                                    "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                    "yBirth","weight", "popWeight")]
MEX_union2_sep_comp$yUnion2 <- yearFrom_cmc(MEX_union2_sep_comp$union_start_cmc2)
MEX_union2_sep_comp$varSepCens2 <- ifelse((!is.na(MEX_union2_sep_comp$union_end_motive2)&(MEX_union2_sep_comp$union_end_motive2=="widowhood")),
                                     MEX_union2_sep_comp$union_end_cmc2,MEX_union2_sep_comp$surveyDate_cmc)
MEX_union2_sep_comp$union_endBySep_cmc2 <- ifelse((!is.na(MEX_union2_sep_comp$union_end_motive2)&(MEX_union2_sep_comp$union_end_motive2=="widowhood")),
                                             NA,MEX_union2_sep_comp$union_end_cmc2)

cohortsListMex_Sep_comp <- c(
  c(1990,1999),
  c(2000,2009)
)

plot_union_sep1_MEX_comp <- KaplanMeierPlot (df=MEX_union1_sep_comp, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", 
                                        varWeight=c("weight","popWeight"),
                                        varClass="survey", varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Sep_comp,
                                        Title="Mexico: Separation of first union, by union cohort",
                                        xTitle = "Duration in years after start of first union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep2_MEX_comp <- KaplanMeierPlot (df=MEX_union2_sep_comp, varEnter="union_start_cmc2",
                                        varEvent="union_endBySep_cmc2", varCens="varSepCens2", var_yBirth = "yUnion2",
                                        varWeight=c("weight","popWeight"),
                                        varClass="survey", varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Sep_comp,
                                        Title="Mexico: Separation of second union, by union cohort",
                                        xTitle = "Duration in years after start of second union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
