setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source (path.expand("~/Dropbox/RStudioData/TransitionsPPR/KaplanMeierLib.R"))
#### MEXICO ####
# First union
# WFS has no information on cohabitation before marriage
# ENADID 1992 has no information on unions
MEX_union1_marr <- subset(MEXICO_ENADID, !(survey %in% c("WFS", "ENADID1992",  "ENADID2006")))
MEX_union1_marr <- MEX_union1_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                     "yBirth","indiv_weight")]
MEX_union1_marr$yUnion1 <- yearFrom_cmc(MEX_union1_marr$union_start_cmc1)

# Second union: only ENADID 1997 and EDER 2017
MEX_union2_marr <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017")))
MEX_union2_marr <- MEX_union2_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type2","union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                     "yBirth","indiv_weight")]
MEX_union2_marr$yUnion2 <- yearFrom_cmc(MEX_union2_marr$union_start_cmc2)

##### 0. cleaning ... #####
# modify censored date for transition to union for marriage: if there is an end of union, it is the censored date
MEX_union1_marr$varUnionCens1 <- ifelse(!is.na(MEX_union1_marr$union_end_cmc1),
                                       MEX_union1_marr$union_end_cmc1,MEX_union1_marr$surveyDate_cmc)
MEX_union2_marr$varUnionCens2 <- ifelse(!is.na(MEX_union2_marr$union_end_cmc2),
                                       MEX_union2_marr$union_end_cmc2,MEX_union2_marr$surveyDate_cmc)

cohortsListMex <- c(
  c(1940,1949),
  c(1950,1959),
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009)
)

cohortsListMex_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

##### 1. Union ==> Marriage #####
###### MEXICO WFS & ALL ENADID ######

plot_marriage_union1_mod_MEX <- KaplanMeierPlot (df=MEX_union1_marr, varEnter="union_start_cmc1",
                                                 varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                                 varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                                 Title="Mexico: transition of first union to marriage, by union cohort",
                                                 xTitle = "Duration in years after first union", maxX = 20, popWeight = TRUE, confInt=TRUE)
plot_marriage_union1_mod_MEX

plot_marriage_union2_mod_MEX <- KaplanMeierPlot (df=MEX_union2_marr, varEnter="union_start_cmc2",
                                                 varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2", varWeight="indiv_weight",
                                                 varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                                 Title="Mexico: transition of second union to marriage, by union cohort",
                                                 xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE)

plot_marriage_union2_mod_MEX

#### MEXICO: EDER 2017 ####
# EDER_marr_union=EDER_ENADID[,c("country","survey","surveyDate_cmc","union_start_cmc1","union_end_cmc1","marriage_start_cmc1",
#                                  "union_start_cmc2","union_end_cmc2","union_end_motive1","marriage_start_cmc2","yBirth","indiv_weight")]
# 
# EDER_marr_union$varUnionCens1 <- ifelse(!is.na(EDER_marr_union$union_end_cmc1),
#                                         EDER_marr_union$union_end_cmc1,EDER_marr_union$surveyDate_cmc)
# EDER_marr_union$varUnionCens2 <- ifelse(!is.na(EDER_marr_union$union_end_cmc2),
#                                        EDER_marr_union$union_end_cmc2,EDER_marr_union$surveyDate_cmc)
# cohortsListMexEDER <- c(
#   c(1960,1969),
#   c(1970,1979),
#   c(1980,1989),
#   c(1990,1999)
# )
# 
# EDER_marr_union <- subset(EDER_marr_union, (is.na(union_start_cmc1))|(union_start_cmc1 != 9999))
# EDER_marr_union <- subset(EDER_marr_union, (is.na(marriage_start_cmc1))|(marriage_start_cmc1!=9999))
# EDER_marr_union <- subset(EDER_marr_union, (is.na(varUnionCens1))|(varUnionCens1!=9999))
# EDER_marr_union <- subset(EDER_marr_union, (is.na(union_start_cmc2))|(union_start_cmc2 != 9999))
# EDER_marr_union <- subset(EDER_marr_union, (is.na(marriage_start_cmc2))|(marriage_start_cmc2!=9999))
# EDER_marr_union <- subset(EDER_marr_union, (is.na(varUnionCens2))|(varUnionCens2!=9999))
# 
# plot_marriage_union1_mod_MEX_EDER <- KaplanMeierPlot (df=EDER_marr_union, varEnter="union_start_cmc1",
#                                                  varEvent="marriage_start_cmc1", varCens="varUnionCens1", varWeight="indiv_weight",
#                                                  varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMexEDER,
#                                                  xTitle = "Duration in years after first union", maxX = 20, popWeight = TRUE, confInt=TRUE)
# plot_marriage_union2_mod_MEX_EDER <- KaplanMeierPlot (df=EDER_marr_union, varEnter="union_start_cmc2",
#                                                  varEvent="marriage_start_cmc2", varCens="varUnionCens2", varWeight="indiv_weight",
#                                                  varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMexEDER,
#                                                  xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE)

##### 2. Union ==> Separation #####
# modify censored dates for widowhood
MEX_union1_sep <- subset(MEXICO_ENADID, !(survey %in% c("ENADID1992",  "ENADID2006")))
MEX_union1_sep <- MEX_union1_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","indiv_weight")]
MEX_union1_sep$yUnion1 <- yearFrom_cmc(MEX_union1_sep$union_start_cmc1)
MEX_union1_sep$varSepCens1 <- ifelse((!is.na(MEX_union1_sep$union_end_motive1)&(MEX_union1_sep$union_end_motive1=="widowhood")),
                                     MEX_union1_sep$union_end_cmc1,MEX_union1_sep$surveyDate_cmc)
# widowhood is not the event, only separation is
MEX_union1_sep$union_endBySep_cmc1 <- ifelse((!is.na(MEX_union1_sep$union_end_motive1)&(MEX_union1_sep$union_end_motive1=="widowhood")),
                                             NA,MEX_union1_sep$union_end_cmc1)
# we have info for second unions only in ENADID 1997 and EDER 2017...
# CREATE A NEW DATASET...
MEX_union2_sep <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017")))
MEX_union2_sep <- MEX_union2_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","indiv_weight")]
MEX_union2_sep$yUnion2 <- yearFrom_cmc(MEX_union2_sep$union_start_cmc2)
MEX_union2_sep$varSepCens2 <- ifelse((!is.na(MEX_union2_sep$union_end_motive2)&(MEX_union2_sep$union_end_motive2=="widowhood")),
                                     MEX_union2_sep$union_end_cmc2,MEX_union2_sep$surveyDate_cmc)
MEX_union2_sep$union_endBySep_cmc2 <- ifelse((!is.na(MEX_union2_sep$union_end_motive2)&(MEX_union2_sep$union_end_motive2=="widowhood")),
                                             NA,MEX_union2_sep$union_end_cmc2)

cohortsListMex_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_union_sep1_MEX <- KaplanMeierPlot (df=MEX_union1_sep, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: Separation after first union, by union cohort",
                                        xTitle = "Duration in years after first union", yTitle="Proportion of separation",
                                        maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep2_MEX <- KaplanMeierPlot (df=MEX_union2_sep, varEnter="union_start_cmc2",
                                        varEvent="union_endBySep_cmc2", varCens="varSepCens2", var_yBirth = "yUnion2", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex,
                                        Title="Mexico: Separation after second union, by union cohort",
                                        xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)

##### 3. First repartnering #####
MEX_sep1_union2 <- subset(MEXICO_ENADID, (survey %in% c( "WFS", "ENADID1997", "EDER2017")))
MEX_sep1_union2 <- MEX_sep1_union2[,c("country","survey","surveyDate_cmc",
                                      "union_end_cmc1","union_start_cmc2",
                                      "yBirth","indiv_weight")]
MEX_sep1_union2$ySep1 <- yearFrom_cmc(MEX_sep1_union2$union_end_cmc1)

cohortsListMex_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_sep1_union2_MEX <- KaplanMeierPlot (df=MEX_sep1_union2, varEnter="union_end_cmc1",
                                        varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: First repartnering, by union cohort",
                                        xTitle = "Duration in years after first separation", yTitle="Proportion of first separation",
                                        maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)

##### 4. First cohabitation ==> separation with marriage censored #####
MEX_cohab1_sep_no_marr <- subset(MEXICO_ENADID, !(survey %in% c("WFS", "ENADID1992", "ENADID2006")))
MEX_cohab1_sep_no_marr$survey <- factor (MEX_cohab1_sep_no_marr$survey)
MEX_cohab1_sep_no_marr <- MEX_cohab1_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","indiv_weight")]
MEX_cohab1_sep_no_marr$yUnion1 <- yearFrom_cmc(MEX_cohab1_sep_no_marr$union_start_cmc1)
MEX_cohab1_sep_no_marr <- subset (MEX_cohab1_sep_no_marr, union_start_type1 %in% c("cohabitation", "cohabitation before marriage"))

# modify censored dates for marriage and widowhood
MEX_cohab1_sep_no_marr$varSepCens1 <- ifelse((!is.na(MEX_cohab1_sep_no_marr$union_end_motive1)&(MEX_cohab1_sep_no_marr$union_end_motive1=="widowhood")),
                                             MEX_cohab1_sep_no_marr$union_end_cmc1,MEX_cohab1_sep_no_marr$surveyDate_cmc)
MEX_cohab1_sep_no_marr$varSepCens1 <- ifelse(!is.na(MEX_cohab1_sep_no_marr$marriage_start_cmc1),
                                             MEX_cohab1_sep_no_marr$marriage_start_cmc1,MEX_cohab1_sep_no_marr$varSepCens1)
# widowhood is not the event, only separation is
MEX_cohab1_sep_no_marr$union_endBySep_cmc1 <- ifelse((!is.na(MEX_cohab1_sep_no_marr$union_end_motive1)&(MEX_cohab1_sep_no_marr$union_end_motive1=="widowhood")),
                                                     NA,MEX_cohab1_sep_no_marr$union_end_cmc1)

cohortsListMex_Marr3 <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_cohab_sep1_MEX <- KaplanMeierPlot (df=MEX_cohab1_sep_no_marr, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr3,
                                        Title="Mexico: Separation of first cohabitation (marriage censored), by union cohort",
                                        xTitle = "Duration in years after first cohabitation", yTitle="Proportion of separation",
                                        maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_cohab_sep1_MEX

#### All countries: GGS and others ####
createCohortsList_Marr_Union <- function() {
  aList <-  list(
    "Austria"=c(c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Belarus"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Belgium"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Bulgaria"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Canada"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Colombia"=c(c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Croatia"=c(c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Czechia"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Denmark"=c(c(1970,1979),c(1980,1989),c(1990,1999)),
    "Estonia"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Finland"=c(c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "France"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Georgia"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Germany"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Hungary"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979)),
    "Italy"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Kazakhstan"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Lithuania"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Moldova"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Mexico"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999),c(2000,2009)),
    "Netherlands"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Norway"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Poland"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Romania"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Russia"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "Spain"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Sweden"=c(c(1930,1939),c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Taiwan"=c(c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "UK"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999)),
    "Uruguay"=c(c(1940,1949),c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989)),
    "USA"=c(c(1950,1959),c(1960,1969),c(1970,1979),c(1980,1989),c(1990,1999),c(2000,2009))
  )
  names(aList) <- toupper(names(aList))
  return (aList)
}
cohortsList <- createCohortsList_Marr_Union()
vecCountry <- names(cohortsList)

dd=GGS_ENADID[,c("country","survey","surveyDate_cmc","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                 "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2","yBirth","indiv_weight")]
idx <- dd$marriage_start_cmc1<dd$union_start_cmc1
idx[is.na(idx)] <- FALSE
cat (sum(idx), "marriage date before union date\n")
#dd$union_start_cmc1 <- ifelse((!is.na(dd$marriage_start_cmc1))&(is.na(dd$union_start_cmc1)),dd$marriage_start_cmc1,dd$union_start_cmc1)
idx <- (!is.na(dd$marriage_start_cmc1))&(is.na(dd$union_start_cmc1))
cat (sum(idx), "marriage date and no union date\n")

##### 1. Union ==> marriage #####
dd$varUnionCens1 <- ifelse(!is.na(dd$union_end_cmc1),
                          dd$union_end_cmc1,dd$surveyDate_cmc)
dd$varUnionCens2 <- ifelse(!is.na(dd$union_end_cmc2),
                           dd$union_end_cmc2,dd$surveyDate_cmc)
dd <- zap_labels (dd)
# we add Mexico
dd <- MEX_union1_marr %>%
  select(any_of(names(dd))) %>%  # Select only columns present in df1
  bind_rows(dd, .)
dd <- MEX_union2_marr %>%
  select(any_of(names(dd))) %>%  # Select only columns present in df1
  bind_rows(dd, .)

plot_marriage_union1_mod <- KaplanMeierPlot (df=dd, varEnter="union_start_cmc1",
                                             varEvent="marriage_start_cmc1", varCens="varUnionCens1", varWeight="indiv_weight",
                                             varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                             xTitle = "Duration in years after first union", maxX = 20, popWeight = TRUE, confInt=TRUE)

plot_marriage_union2_mod <- KaplanMeierPlot (df=dd, varEnter="union_start_cmc2",
                                             varEvent="marriage_start_cmc2", varCens="varUnionCens2", varWeight="indiv_weight",
                                             varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                             xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE)

##### 2. Union ==> Separation #####
# modify censored dates for widowhood
dd$varSepCens1 <- ifelse((!is.na(dd$union_end_motive1)&(dd$union_end_motive1=="widowhood")),
                                     dd$union_end_cmc1,dd$surveyDate_cmc)
# widowhood is not the event, only separation is
dd$union_endBySep_cmc1 <- ifelse((!is.na(dd$union_end_motive1)&(dd$union_end_motive1=="widowhood")),
                                             NA,dd$union_end_cmc1)
# we have info for second unions only in ENADID 1997 and EDER 2017...
# CREATE A NEW DATASET...
# MEX_marr_union$varSepCens2 <- ifelse((!is.na(MEX_marr_union$union_end_motive2)&(MEX_marr_union$union_end_motive2=="widowhood")),
#                                      MEX_marr_union$union_end_cmc2,MEX_marr_union$surveyDate_cmc)
# MEX_marr_union$union_endBySep_cmc2 <- ifelse((!is.na(MEX_marr_union$union_end_motive2)&(MEX_marr_union$union_end_motive2=="widowhood")),
#                                              NA,MEX_marr_union$union_end_cmc2)

plot_union_sep1_mod <- KaplanMeierPlot (df=dd, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                        xTitle = "Duration in years after first union", maxX = 20, popWeight = TRUE, confInt=TRUE)

#### USA: NSFG surveys ####
# 1973: complete UH (up to 6), no CohabBefMar
# 1976: complete UH (up to 3), no CohabBefMar
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
USA_union1_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982","NSFG2017_19")))
USA_union1_marr <- USA_union1_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","indiv_weight")]
USA_union1_marr$yUnion1 <- yearFrom_cmc(USA_union1_marr$union_start_cmc1)

# Second union
USA_union2_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988","NSFG2017_19","NSFG2022_23")))
USA_union2_marr <- USA_union2_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","indiv_weight")]
USA_union2_marr$yUnion2 <- yearFrom_cmc(USA_union2_marr$union_start_cmc2)

##### 0. cleaning ... #####
# modify censored date for transition to union for marriage: if there is an end of union, it is the censored date
USA_union1_marr$varUnionCens1 <- ifelse(!is.na(USA_union1_marr$union_end_cmc1),
                                        USA_union1_marr$union_end_cmc1,USA_union1_marr$surveyDate_cmc)
USA_union2_marr$varUnionCens2 <- ifelse(!is.na(USA_union2_marr$union_end_cmc2),
                                        USA_union2_marr$union_end_cmc2,USA_union2_marr$surveyDate_cmc)

cohortsListUSA <- c(
  c(1940,1949),
  c(1950,1959),
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009)
)

cohortsListUSA_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

##### 1. Union ==> Marriage #####

plot_marriage_union1_USA <- KaplanMeierPlot (df=USA_union1_marr, varEnter="union_start_cmc1",
                                             varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                             varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr,
                                             Title="USA: transition of first union to marriage, by union cohort",
                                             xTitle = "Duration in years after first union", maxX = 20, popWeight = TRUE, confInt=TRUE)
plot_marriage_union1_USA

plot_marriage_union2_USA <- KaplanMeierPlot (df=USA_union2_marr, varEnter="union_start_cmc2",
                                             varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2", varWeight="indiv_weight",
                                             varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr,
                                             Title="USA: transition of second union to marriage, by union cohort",
                                             xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE)

plot_marriage_union2_USA


##### 2. Union ==> Separation #####
# first union
USA_union1_sep <- subset(NSFG_ENADID, !(survey %in% c("NSFG2017_19")))
USA_union1_sep <- USA_union1_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","indiv_weight")]
USA_union1_sep$yUnion1 <- yearFrom_cmc(USA_union1_sep$union_start_cmc1)

# modify censored dates for widowhood
USA_union1_sep$varSepCens1 <- ifelse((!is.na(USA_union1_sep$union_end_motive1)&(USA_union1_sep$union_end_motive1=="widowhood")),
                                     USA_union1_sep$union_end_cmc1,USA_union1_sep$surveyDate_cmc)
# widowhood is not the event, only separation is
USA_union1_sep$union_endBySep_cmc1 <- ifelse((!is.na(USA_union1_sep$union_end_motive1)&(USA_union1_sep$union_end_motive1=="widowhood")),
                                             NA,USA_union1_sep$union_end_cmc1)
# Second union
USA_union2_sep <- subset(NSFG_ENADID, !(survey %in% c("NSFG1988","NSFG2017_19","NSFG2022_23")))
USA_union2_sep <- USA_union2_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","indiv_weight")]
USA_union2_sep$yUnion2 <- yearFrom_cmc(USA_union2_sep$union_start_cmc2)

# modify censored dates for widowhood
USA_union2_sep$varSepCens2 <- ifelse((!is.na(USA_union2_sep$union_end_motive2)&(USA_union2_sep$union_end_motive2=="widowhood")),
                                     USA_union2_sep$union_end_cmc2,USA_union2_sep$surveyDate_cmc)
# widowhood is not the event, only separation is
USA_union2_sep$union_endBySep_cmc2 <- ifelse((!is.na(USA_union2_sep$union_end_motive2)&(USA_union2_sep$union_end_motive2=="widowhood")),
                                             NA,USA_union2_sep$union_end_cmc2)

cohortsListUSA_Marr2 <- c(
  c(1945,1959),
  c(1950,1959),
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_union_sep1_USA <- KaplanMeierPlot (df=USA_union1_sep, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr2,
                                        Title="USA: Separation after first union, by union cohort",
                                        xTitle = "Duration in years after first union", yTitle="Proportion of separation",
                                        maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep1_USA
plot_union_sep2_USA <- KaplanMeierPlot (df=USA_union2_sep, varEnter="union_start_cmc2",
                                        varEvent="union_endBySep_cmc2", varCens="varSepCens2", var_yBirth = "yUnion2", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr2,
                                        Title="USA: Separation after second union, by union cohort",
                                        xTitle = "Duration in years after second union", maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep2_USA

##### 3. First repartnering #####
USA_sep1_union2 <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988","NSFG2017_19","NSFG2022_23")))
USA_sep1_union2 <- USA_sep1_union2[,c("country","survey","surveyDate_cmc",
                                      "union_end_cmc1","union_start_cmc2",
                                      "yBirth","indiv_weight")]
USA_sep1_union2$ySep1 <- yearFrom_cmc(USA_sep1_union2$union_end_cmc1)

cohortsListUSA_Marr3 <- c(
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_sep1_union2_USA <- KaplanMeierPlot (df=USA_sep1_union2, varEnter="union_end_cmc1",
                                         varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1", varWeight="indiv_weight",
                                         varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr3,
                                         Title="USA: First repartnering, by union cohort",
                                         xTitle = "Duration in years after first separation", yTitle="Proportion of first separation",
                                         maxX = 20, popWeight = TRUE, confInt=TRUE,
                                         inverseFunction = TRUE)
plot_sep1_union2_USA

##### 4. First cohabitation ==> separation with marriage as censored #####
USA_cohab1_sep_no_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982","NSFG2017_19")))
USA_cohab1_sep_no_marr <- USA_cohab1_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_start_type1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","indiv_weight")]
USA_cohab1_sep_no_marr$yUnion1 <- yearFrom_cmc(USA_union1_marr$union_start_cmc1)
USA_cohab1_sep_no_marr <- subset (USA_cohab1_sep_no_marr, union_start_type1 %in% c("cohabitation", "cohabitation before marriage"))

# modify censored dates for marriage and widowhood
USA_cohab1_sep_no_marr$varSepCens1 <- ifelse((!is.na(USA_cohab1_sep_no_marr$union_end_motive1)&(USA_cohab1_sep_no_marr$union_end_motive1=="widowhood")),
                                             USA_cohab1_sep_no_marr$union_end_cmc1,USA_cohab1_sep_no_marr$surveyDate_cmc)
USA_cohab1_sep_no_marr$varSepCens1 <- ifelse(!is.na(USA_cohab1_sep_no_marr$marriage_start_cmc1),
                                             USA_cohab1_sep_no_marr$marriage_start_cmc1,USA_cohab1_sep_no_marr$varSepCens1)
# widowhood is not the event, only separation is
USA_cohab1_sep_no_marr$union_endBySep_cmc1 <- ifelse((!is.na(USA_cohab1_sep_no_marr$union_end_motive1)&(USA_cohab1_sep_no_marr$union_end_motive1=="widowhood")),
                                             NA,USA_cohab1_sep_no_marr$union_end_cmc1)

cohortsListUSA_Marr3 <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_cohab_sep1_USA <- KaplanMeierPlot (df=USA_cohab1_sep_no_marr, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", varWeight="indiv_weight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr3,
                                        Title="USA: Separation of first cohabitation (marriage censored), by union cohort",
                                        xTitle = "Duration in years after first cohabitation", yTitle="Proportion of separation",
                                        maxX = 20, popWeight = TRUE, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_cohab_sep1_USA


#### Mexico and USA combined plots ####
##### 1. First union ==> marriage #####
library(patchwork)

# Combine plots side-by-side
combined_plot <-
  (plot_marriage_union1_mod_MEX + labs(title="MEXICO")) +
  (plot_marriage_union1_USA + labs(title="USA"))

# Apply font changes to EVERYTHING at once
MEX_USA_union1_marriage_plot <- combined_plot + 
  plot_annotation(
    #title = "Transition of first union to marriage, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/MEX_USA_Union1_marriage.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union1_marriage_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

##### 2. Second union ==> marriage #####
combined_plot <-
  (plot_marriage_union2_mod_MEX + labs(title="MEXICO")) +
  (plot_marriage_union2_USA + labs(title="USA"))

MEX_USA_union2_marriage_plot <- combined_plot + 
  plot_annotation(
    #title = "Transition of second union to marriage, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/MEX_USA_Union2_marriage.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union2_marriage_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

##### 3. First union ==> separation ######
combined_plot <-
  (plot_union_sep1_MEX + labs(title="MEXICO")) +
  (plot_union_sep1_USA + labs(title="USA"))

MEX_USA_union_sep1_plot <- combined_plot + 
  plot_annotation(
    #title = "Separation of first union, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/MEX_USA_Union_sep1.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union_sep1_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

##### 4. Separation of cohabitation, marriage censored #####
combined_plot <-
  (plot_cohab_sep1_MEX + labs(title="MEXICO")) +
  (plot_cohab_sep1_USA + labs(title="USA"))

MEX_USA_cohab_sep1_plot <- combined_plot + 
  plot_annotation(
    #title = "Separation of cohabitation, marriage censored, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/MEX_USA_Cohabitation_sep1.pdf")
ggsave(filename = pathFile, plot = MEX_USA_cohab_sep1_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

##### 5. First repartnering #####
combined_plot <-
  (plot_sep1_union2_MEX + labs(title="MEXICO")) +
  (plot_sep1_union2_USA + labs(title="USA"))

MEX_USA_sep1_union2_plot <- combined_plot + 
  plot_annotation(
    #title = "First repartnering, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/MEX_USA_sep1_union2.pdf")
ggsave(filename = pathFile, plot = MEX_USA_sep1_union2_plot, width = 29.7, height = 21, units = "cm", dpi = 300)
