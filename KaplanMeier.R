setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
source("lib/KaplanMeierLib.R")
# >>> Claude 2026-09-21
source("lib/unionEpisodes.R")   # buildUnionEpisodes, unionStateOccupancy, unionCompetingRisks
source("lib/adjustedSurv.R")    # adjustedSurv, adjustedSurv_note
library(survival)
# <<< Claude 2026-09-21
path_output_plots <- paste0(outputPath, "/")

MEXICO_ENADID <- filterDateQuality(MEXICO_ENADID)
NSFG_ENADID   <- filterDateQuality(NSFG_ENADID)

# >>> Claude 2026-09-22
# ======================================================================
# WHICH BLOCK TO USE, after the September 2026 revision
# ======================================================================
#
#   1. Union ==> Marriage .................. correct
#   2. Union ==> Separation ................ correct, NET measure
#   3. First repartnering .................. correct
#   4. Cohabitation ==> separation ......... WRONG, replaced by 4b.1b
#   4b. ................................... the replacements
#   Combined 1, 2, 6 ....................... correct
#   Combined 3, 4 .......................... correct, NET measure
#   Combined 5 ............................. WRONG, replaced by 4b.1b
#
# NET against CRUDE. A curve that censors a competing event estimates the
# associated single-decrement, or net, probability: what would happen if that
# event could not occur. Censoring WIDOWHOOD is defensible, since a partner's
# death is plausibly unrelated to the couple's propensity to separate, and it
# removes a mortality difference between the two countries. Censoring MARRIAGE
# is not, for the reason given at block 4. The crude probability, the share who
# actually experience the event, comes from the Aalen-Johansen blocks in 4b.
#
# EVERY FIGURE IN THIS FILE HAS MOVED, including the correct ones:
#   - the Kaplan-Meier estimator is right-continuous now, so a mass point at
#     duration 0 shows as a vertical drop instead of shifting the curve
#   - survival::survfit() supplies the estimate and the interval, with log-log
#     bounds and the infinitesimal jackknife variance in place of Greenwood
#   - filterDateQuality() removes the dateless separations that used to be
#     counted as women who never separated
# The intervals still ignore the sampling design: no strata, no clusters.
#
# See 'Union analysis: methods, status and plan' for the full argument.
# ======================================================================
# <<< Claude 2026-09-22

# >>> Claude 2026-09-22
# WRONG = TRUE skips the three blocks that censor at marriage, which 4b.1b
# replaces: Mexico section 4, USA section 4 and combined section 5. They are
# left in the file so the old specification can still be read and re-run.
# Set WRONG <- FALSE to produce them again, for instance to show in a
# robustness note how much censoring at marriage overstates the separation.
# Combined section 5 depends on the two country blocks, so the three switch
# together.
WRONG <- TRUE
# <<< Claude 2026-09-22

#### MEXICO ####
# First union
# WFS has no information on cohabitation before marriage
# ENADID 1992 has no information on unions
# EDER 2025 has incorrect information for marriages
MEX_union1_marr <- subset(MEXICO_ENADID, !(survey %in% c("WFS","ENADID1992","ENADID2006","EDER2025")))
MEX_union1_marr <- MEX_union1_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                     "yBirth","weight","popWeight")]
MEX_union1_marr$yUnion1 <- yearFrom_cmc(MEX_union1_marr$union_start_cmc1)

# Second union: only ENADID 1997, EDER 2017
MEX_union2_marr <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017")))
MEX_union2_marr <- MEX_union2_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type2","union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                     "yBirth","weight", "popWeight")]
MEX_union2_marr$yUnion2 <- yearFrom_cmc(MEX_union2_marr$union_start_cmc2)

##### 0. cleaning ... #####
# modify censored date for transition to union for marriage: if there is an end of union, it is the censored date
MEX_union1_marr$varUnionCens1 <- ifelse(!is.na(MEX_union1_marr$union_end_cmc1),
                                       MEX_union1_marr$union_end_cmc1,MEX_union1_marr$surveyDate_cmc)
MEX_union2_marr$varUnionCens2 <- ifelse(!is.na(MEX_union2_marr$union_end_cmc2),
                                       MEX_union2_marr$union_end_cmc2,MEX_union2_marr$surveyDate_cmc)

cohortsListMex_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

# >>> Claude 2026-09-22
# STATUS: correct. Keep.
# Measures: of all first unions, the share that have reached marriage by
# duration d. Direct marriages are included, so about half the unions marry at
# duration 0 and the curve starts near 0.46 after a vertical drop from 1.
# That is right for 'how does a union of any kind reach marriage'. For 'how
# fast do cohabiting unions convert', use plot_cohab_convert in 4b.1b instead.
# <<< Claude 2026-09-22

##### 1. Union ==> Marriage #####
###### MEXICO WFS & ALL ENADID ######

plot_marriage_union1_mod_MEX <- KaplanMeierPlot (df=MEX_union1_marr, varEnter="union_start_cmc1",
                                                 varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1", 
                                                 varWeight="popWeight",
                                                 varClass=NULL, varCountry=NULL, vecCountry=NULL,
                                                 cohortsList=cohortsListMex_Marr,
                                                 Title="Mexico: transition of first union to marriage, by union cohort",
                                                 xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
plot_marriage_union1_mod_MEX

plot_marriage_union2_mod_MEX <- KaplanMeierPlot (df=MEX_union2_marr, varEnter="union_start_cmc2",
                                                 varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2",
                                                 varWeight="popWeight",
                                                 varClass=NULL, varCountry=NULL, vecCountry=NULL,
                                                 cohortsList=cohortsListMex_Marr,
                                                 Title="Mexico: transition of second union to marriage, by union cohort",
                                                 xTitle = "Duration in years after second union", maxX = 20, confInt=TRUE)

plot_marriage_union2_mod_MEX

#### MEXICO: EDER ####
EDER_year <- "EDER2017"
EDER_year <- "EDER2025"
goEDER <- function (EDER_year, union=1) {
  if ((EDER_year == "EDER2025") & (exists("EDER_ENADID25"))) {
    EDER_union_marr=EDER_ENADID25[,c("country","survey","surveyDate_cmc",
                                     "union_start_type1","union_start_cmc1","union_end_cmc1","marriage_start_cmc1",
                                     "union_start_type2","union_start_cmc2","union_end_cmc2","union_end_motive1","marriage_start_cmc2",
                                     "yBirth","weight", "popWeight")]
  } else {
    EDER_union_marr=MEXICO_ENADID[(MEXICO_ENADID$survey==EDER_year),c("country","survey","surveyDate_cmc",
                                                                      "union_start_type1","union_start_cmc1","union_end_cmc1","marriage_start_cmc1",
                                                                      "union_start_type2","union_start_cmc2","union_end_cmc2","union_end_motive1","marriage_start_cmc2",
                                                                      "yBirth","weight", "popWeight")]
  }
  
  EDER_union_marr$yUnion1 <- yearFrom_cmc(EDER_union_marr$union_start_cmc1)
  EDER_union_marr$yUnion2 <- yearFrom_cmc(EDER_union_marr$union_start_cmc2)
  
  EDER_union_marr$varUnionCens1 <- ifelse(!is.na(EDER_union_marr$union_end_cmc1),
                                          EDER_union_marr$union_end_cmc1,EDER_union_marr$surveyDate_cmc)
  EDER_union_marr$varUnionCens2 <- ifelse(!is.na(EDER_union_marr$union_end_cmc2),
                                          EDER_union_marr$union_end_cmc2,EDER_union_marr$surveyDate_cmc)
  cohortsListMexEDER <- c(
    c(1980,1989),
    c(1990,1999),
    c(2000,2009),
    c(2010,2019),
    c(2020,2029)
  )
  
  plot_marriage_union1_mod_MEX_EDER <- KaplanMeierPlot (df=EDER_union_marr, varEnter="union_start_cmc1",
                                                        varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1", varWeight="weight",
                                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMexEDER,
                                                        xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
  plot_marriage_union2_mod_MEX_EDER <- KaplanMeierPlot (df=EDER_union_marr, varEnter="union_start_cmc2",
                                                        varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2", varWeight="weight",
                                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMexEDER,
                                                        xTitle = "Duration in years after second union", maxX = 20, confInt=TRUE)
  
  if (union==1) {
    plot_marriage_union1_mod_MEX_EDER
  } else {
    plot_marriage_union2_mod_MEX_EDER
  }
}

# >>> Claude 2026-09-22
# STATUS: correct, but read it as the NET measure.
# Widowhood is censored, so the woman leaves the risk set when her partner
# dies. The curve is therefore the share who would separate if no partner had
# died, that is, male mortality removed. It is computed correctly and it is a
# reasonable headline for a Mexico against USA comparison, since it takes out
# a mortality difference you are not measuring. It is NOT a share of women:
# net separation + net widowhood + still in union exceeds 1.
# For the CRUDE share, who actually separated, see 4b.1c:
#   plot_union_separate, plot_union_separate_origin
# <<< Claude 2026-09-22

##### 2. Union ==> Separation #####
MEX_union1_sep <- subset(MEXICO_ENADID, !(survey %in% c("ENADID1992",  "ENADID2006")))
MEX_union1_sep <- MEX_union1_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight", "popWeight")]
MEX_union1_sep$yUnion1 <- yearFrom_cmc(MEX_union1_sep$union_start_cmc1)
# include widowhood as censoring
MEX_union1_sep$varSepCens1 <- ifelse((!is.na(MEX_union1_sep$union_end_motive1)&(MEX_union1_sep$union_end_motive1=="widowhood")),
                                     MEX_union1_sep$union_end_cmc1,MEX_union1_sep$surveyDate_cmc)
# exclude widowhood from cause of end of union, retaining only separation
MEX_union1_sep$union_endBySep_cmc1 <- ifelse((!is.na(MEX_union1_sep$union_end_motive1)&(MEX_union1_sep$union_end_motive1=="widowhood")),
                                             NA,MEX_union1_sep$union_end_cmc1)
# we have info for second unions only in ENADID 1997 and EDER 2017 and 2025...
# CREATE A NEW DATASET...
MEX_union2_sep <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017", "EDER2025")))
MEX_union2_sep <- MEX_union2_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","weight", "popWeight")]
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
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1", 
                                        varWeight="popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: Separation of first union, by union cohort",
                                        xTitle = "Duration in years after start of first union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep1_MEX
plot_union_sep2_MEX <- KaplanMeierPlot (df=MEX_union2_sep, varEnter="union_start_cmc2",
                                        varEvent="union_endBySep_cmc2", varCens="varSepCens2", var_yBirth = "yUnion2",
                                        varWeight="popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: Separation of second union, by union cohort",
                                        xTitle = "Duration in years after start of second union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep2_MEX
# >>> Claude 2026-09-22
# STATUS: correct. One composition point.
# The women entering here are pooled regardless of how the first union ended,
# and roughly a fifth are widows. Repartnering after widowhood is a different
# process from repartnering after separation. Splitting on union_end_motive1
# is one subset() if you want the two apart.
# <<< Claude 2026-09-22

##### 3. First repartnering #####
MEX_sep1_union2 <- subset(MEXICO_ENADID, (survey %in% c("WFS", "ENADID1997", "EDER2017")))
MEX_sep1_union2 <- MEX_sep1_union2[,c("country","survey","surveyDate_cmc",
                                      "union_end_cmc1","union_start_cmc2",
                                      "yBirth","weight","popWeight")]
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
                                        varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1",
                                        varWeight="popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: First repartnering, by union cohort",
                                        xTitle = "Duration in years after first separation",
                                        yTitle="Proportion of first separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)

# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by 4b.1b.
# Censoring at marriage asks what would happen if a cohabiting couple could
# never marry. Unlike a partner's death, that is not a hazard you can imagine
# removing: conversion is an act of the same couple, and the couples who marry
# are exactly those least likely to separate, so the independence that
# censoring assumes fails. In simulation it overstated the ten-year figure by
# 83 per cent.
#
# Use instead, from one Aalen-Johansen fit, both countries, by union cohort:
#   plot_cohab_remain    still cohabiting
#   plot_cohab_convert   converted to marriage
#   plot_cohab_separate  separated before marrying  <- what this block wanted
# They sum to 1 with the widowed and the unknown endings at every duration.
# <<< Claude 2026-09-22

##### 4. Cohabitation ==> separation with marriage censored #####
# >>> Claude 2026-09-22
# Superseded by 4b.1b. Skipped while WRONG is TRUE; set WRONG <- FALSE
# at the top of the file to run it again for comparison.
if (!WRONG) {
###### First cohabitation ######
MEX_cohab1_sep_no_marr <- subset(MEXICO_ENADID, !(survey %in% c("WFS", "ENADID1992", "ENADID2006")))
MEX_cohab1_sep_no_marr$survey <- factor (MEX_cohab1_sep_no_marr$survey)
MEX_cohab1_sep_no_marr <- MEX_cohab1_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_type1","union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight","popWeight")]
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
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1",
                                        "popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr3,
                                        Title="Mexico: Separation of first cohabitation (marriage censored), by union cohort",
                                        xTitle = "Duration in years after first cohabitation", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_cohab_sep1_MEX

###### Second cohabitation ######
MEX_cohab2_sep_no_marr <- subset(MEXICO_ENADID, (survey %in% c("ENADID1997", "EDER2017")))
MEX_cohab2_sep_no_marr$survey <- factor (MEX_cohab2_sep_no_marr$survey)
MEX_cohab2_sep_no_marr <- MEX_cohab2_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                                    "union_start_type1","union_start_type1","union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                                    "yBirth","weight","popWeight")]
MEX_cohab2_sep_no_marr$yUnion1 <- yearFrom_cmc(MEX_cohab2_sep_no_marr$union_start_cmc1)
MEX_cohab2_sep_no_marr <- subset (MEX_cohab2_sep_no_marr, union_start_type1 %in% c("cohabitation", "cohabitation before marriage"))

# modify censored dates for marriage and widowhood
MEX_cohab2_sep_no_marr$varSepCens1 <- ifelse((!is.na(MEX_cohab2_sep_no_marr$union_end_motive1)&(MEX_cohab2_sep_no_marr$union_end_motive1=="widowhood")),
                                             MEX_cohab2_sep_no_marr$union_end_cmc1,MEX_cohab2_sep_no_marr$surveyDate_cmc)
MEX_cohab2_sep_no_marr$varSepCens1 <- ifelse(!is.na(MEX_cohab2_sep_no_marr$marriage_start_cmc1),
                                             MEX_cohab2_sep_no_marr$marriage_start_cmc1,MEX_cohab2_sep_no_marr$varSepCens1)
# widowhood is not the event, only separation is
MEX_cohab2_sep_no_marr$union_endBySep_cmc1 <- ifelse((!is.na(MEX_cohab2_sep_no_marr$union_end_motive1)&(MEX_cohab2_sep_no_marr$union_end_motive1=="widowhood")),
                                                     NA,MEX_cohab2_sep_no_marr$union_end_cmc1)

cohortsListMex_Marr3 <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_cohab_sep2_MEX <- KaplanMeierPlot (df=MEX_cohab2_sep_no_marr, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1",
                                        "popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr3,
                                        Title="Mexico: Separation of first cohabitation (marriage censored), by union cohort",
                                        xTitle = "Duration in years after first cohabitation", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_cohab_sep2_MEX
}   # end if (!WRONG)
# <<< Claude 2026-09-22

# >>> Claude 2026-09-21
##### 4b. The same question without censoring at marriage #####
#
# Section 4 above censors at marriage. That estimates the proportion who would
# separate IF a cohabiting union could never convert, which is a hypothetical
# world. In simulation it overstated the real figure by 83% at ten years.
# Marriage is not censoring: it is a COMPETING event, something that happened
# to the couple and removed them from the risk of separating while cohabiting.
# Only the survey date is censoring. The four blocks below replace that curve.
# See "Union analysis: methods, status and plan" for the full argument.
#
# NOTE ON ages: ageUnion and ageMarriage are the SAME number for a direct
# marriage, so controlling both is the same as controlling cohabDur. Block 4
# uses cohabDur because it makes that visible. Run adjustedSurv_note().

MEX_epi <- subset(MEXICO_ENADID, !(survey %in% c("WFS", "ENADID1992", "ENADID2006")))
MEX_epi$survey <- factor(MEX_epi$survey)
# >>> Claude 2026-09-21
# popWeight rather than weight, so several surveys pool on a common scale, and
# the cohort breaks are now the shared constants used by 4b.1b as well. Delete
# varWeight to go back to the individual weight.
MEX_epi <- buildUnionEpisodes(MEX_epi, u = 1, varWeight = "popWeight")

COHORT_BREAKS <- c(-Inf, 1949, 1959, 1969, 1979, 1989, 1999, 2009, Inf)
COHORT_LABELS <- c("<1950", "1950-59", "1960-69", "1970-79",
                   "1980-89", "1990-99", "2000-09", "2010+")
MIN_UNIONS    <- 200   # a (country, cohort) cell thinner than this is not plotted

MEX_epi$cohort <- cut(MEX_epi$yUnion, breaks = COHORT_BREAKS, labels = COHORT_LABELS)

# The USA counterpart, and the two stacked. Built here rather than inside a
# single sub-block, because every block from 4b.1 on now has a Mexico version,
# a USA version and a combined one.
USA_epi <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG2017_19")))
USA_epi$survey <- factor(USA_epi$survey)
USA_epi <- buildUnionEpisodes(USA_epi, u = 1, varWeight = "popWeight")
USA_epi$cohort <- cut(USA_epi$yUnion, breaks = COHORT_BREAKS, labels = COHORT_LABELS)
# ids must be distinct ACROSS countries: unionStateOccupancy() fits a multistate
# model with id =, and one id may not appear in two strata.
USA_epi$id <- USA_epi$id + max(MEX_epi$id)

BOTH_epi <- rbind(MEX_epi, USA_epi)
BOTH_epi <- BOTH_epi[!is.na(BOTH_epi$cohort), ]

# How many cohabiting first unions each (country, cohort) cell rests on. Thin
# cells are dropped rather than plotted as a noisy curve.
cohabFirst <- subset(BOTH_epi, (tstart == 0) & (istate == "cohabiting"))
cellN <- as.data.frame(table(country = cohabFirst$country, cohort = cohabFirst$cohort))
names(cellN)[3] <- "nUnions"
print(cellN)
keepCell <- subset(cellN, nUnions >= MIN_UNIONS)[, c("country", "cohort")]
BOTH_epi <- merge(BOTH_epi, keepCell, by = c("country", "cohort"))
BOTH_epi <- BOTH_epi[order(BOTH_epi$id, BOTH_epi$tstart), ]
# <<< Claude 2026-09-21


###### 4b.1 Where unions are, by duration (state occupancy) ######
# Replaces the single curve with the whole picture: still cohabiting, married,
# separated, widowed. The bands sum to 1 at every duration.
# >>> Claude 2026-09-21
# One helper, three figures: Mexico, the USA, and the two side by side.
# 'facets' is a formula so the combined version can split on origin AND country.
occupancyPlot <- function (occ, Title, facets = ~ group) {
  ggplot(occ, aes(x = timeYear, y = p, fill = state)) +
    geom_area() + facet_grid(facets) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    labs(title = Title, x = "Duration in years after the union began",
         y = "Share of the cohort", fill = "State") +
    theme_linedraw() + theme(legend.position = "bottom")
}

MEX_occ1 <- unionStateOccupancy(MEX_epi, by = "origin", maxYears = 25)
plot_occupancy_MEX <- occupancyPlot(
  MEX_occ1, "Mexico: where first unions are, by duration and by how they began")
plot_occupancy_MEX

USA_occ1 <- unionStateOccupancy(USA_epi, by = "origin", maxYears = 25)
plot_occupancy_USA <- occupancyPlot(
  USA_occ1, "USA: where first unions are, by duration and by how they began")
plot_occupancy_USA

# Combined. One fit, stratified on origin AND country, so the two panels of each
# row are directly comparable and the bands still sum to 1 within every panel.
BOTH_occ1 <- unionStateOccupancy(BOTH_epi, by = c("origin", "country"), maxYears = 25)
plot_occupancy_BOTH <- occupancyPlot(
  BOTH_occ1, "Mexico and the USA: where first unions are, by duration and by how they began",
  facets = origin ~ country)
plot_occupancy_BOTH
pathFile <- paste0(path_output_plots,"MEX_USA_union1_AJ.pdf")
ggsave(filename = pathFile, plot = plot_occupancy_BOTH, width = 29.7, height = 21, units = "cm", dpi = 300)

# <<< Claude 2026-09-21



###### 4b.1b Cohabiting first unions: remain, convert, separate ######
# Mexico against the USA, by union cohort.
#
# All three curves come from ONE Aalen-Johansen fit on the cohabiting spell of
# the first unions that BEGAN as cohabitation. At every duration
#     remain + convert + separate + widowed + ended (unknown) = 1
# so these are three readings of one decomposition, not three separate models.
#   remain   still cohabiting, neither married nor separated
#   convert  has married the same partner
#   separate has separated while still cohabiting
# Conversion and separation compete for the same unions, which is the point of
# 4b: censoring at marriage, as section 4 does, answers a different question.
#
# Direct marriages are excluded by construction: only spells whose istate is
# "cohabiting" at duration 0 enter.

# MEX_epi, USA_epi and BOTH_epi are built in the 4b preamble above.

CR_both <- unionCompetingRisks(BOTH_epi, from = "cohabiting",
                               by = c("country", "cohort"),
                               horizonYears = c(5, 10, 15))
print(CR_both$table)     # the numbers behind the three figures

# >>> Claude 2026-09-22
# Guard against a stale copy of the library in the session. The multi-column
# 'by' is recent; the older unionCompetingRisks() built its formula with
# sprintf() on the whole vector, and as.formula() then takes only the FIRST
# element with nothing worse than a deprecation warning. The fit comes back
# stratified on country alone and the split columns are missing, which surfaces
# much later as "replacement has 0 rows".
if (!all(c("country", "cohort") %in% names(CR_both$curves))) {
  stop("4b.1b: unionCompetingRisks() returned no 'country' / 'cohort' columns. ",
       "An older lib/unionEpisodes.R is still in memory: source it again.")
}
# <<< Claude 2026-09-22

curCohab <- subset(CR_both$curves, timeYear <= 25)
curCohab$cohort  <- factor(curCohab$cohort, levels = COHORT_LABELS)
curCohab$country <- factor(curCohab$country)

# >>> Claude 2026-09-22
# These now draw through KaplanMeierDraw(), the plotting half of
# KaplanMeierPlot(), so the Aalen-Johansen figures carry the same colour ramp,
# ribbons, cohort labels, facets and theme as every other figure in this file.
# asKMtable() only renames columns into the layout that function expects, with
# time back in MONTHS and the cohort as an ordered factor so the grey to blue to
# red ramp runs in chronological order.
asKMtable <- function (d, valueVar = "p", classVar = NULL) {
  out <- data.frame(
    time         = d$timeYear * 12,
    survFunction = d[[valueVar]],
    confIntMin   = if ("lower" %in% names(d)) d$lower else NA_real_,
    confIntMax   = if ("upper" %in% names(d)) d$upper else NA_real_,
    class        = if (is.null(classVar)) "All" else as.character(d[[classVar]]),
    country      = as.character(d$country),
    cohort       = factor(as.character(d$cohort), levels = COHORT_LABELS),
    branch       = "after",
    stringsAsFactors = FALSE)
  out$cohort      <- droplevels(out$cohort)
  out$cohortLabel <- as.character(out$cohort)
  # A bound that survfit could not compute falls back on the point estimate, so
  # the ribbon closes rather than breaking the polygon.
  out$confIntMin <- ifelse(is.na(out$confIntMin), out$survFunction, out$confIntMin)
  out$confIntMax <- ifelse(is.na(out$confIntMax), out$survFunction, out$confIntMax)
  out[order(out$class, out$country, out$cohort, out$time), ]
}

cohortPlot <- function (d, Title, yTitle, anchorAtOne = FALSE, classVar = NULL) {
  if (nrow(d) == 0) stop("4b.1: nothing to plot")
  KaplanMeierDraw(asKMtable(d, classVar = classVar),
                  Title = Title, yTitle = yTitle,
                  xTitle = "Duration in years after the union began",
                  minX = 0, maxX = 25, confInt = TRUE, hideLegend = TRUE,
                  anchorAtOne = anchorAtOne, legendTitle = "Union cohort")
}

cohabPlot <- function (st, Title, yTitle, anchorAtOne = FALSE) {
  d <- subset(curCohab, state == st)
  if (nrow(d) == 0) stop("4b.1b: no rows for state '", st, "'")
  cohortPlot(d, Title, yTitle, anchorAtOne = anchorAtOne)
}
# <<< Claude 2026-09-22

plot_cohab_remain <- cohabPlot(
  "(s0)",
  "Cohabiting first unions that are still cohabiting",
  "Still cohabiting, neither married nor separated",
  anchorAtOne = TRUE)     # a survival curve: it starts at 1

plot_cohab_convert <- cohabPlot(
  "married converted",
  "Cohabiting first unions that have converted to marriage",
  "Cumulative incidence of conversion")

plot_cohab_separate <- cohabPlot(
  "separation (cohabiting)",
  "Cohabiting first unions that have separated before marrying",
  "Cumulative incidence of separation")

plot_cohab_remain
plot_cohab_convert
plot_cohab_separate
pathFile <- paste0(path_output_plots,"MEX_USA_union1_cohab_remain_AJ.pdf")
ggsave(filename = pathFile, plot = plot_cohab_remain, width = 29.7, height = 21, units = "cm", dpi = 300)
pathFile <- paste0(path_output_plots,"MEX_USA_union1_cohab_convert_AJ.pdf")
ggsave(filename = pathFile, plot = plot_cohab_convert, width = 29.7, height = 21, units = "cm", dpi = 300)
pathFile <- paste0(path_output_plots,"MEX_USA_union1_cohab_separate_AJ.pdf")
ggsave(filename = pathFile, plot = plot_cohab_separate, width = 29.7, height = 21, units = "cm", dpi = 300)

###### 4b.1c All first unions that have separated ######
# The honest version of section 2, which censors at nothing but treats the
# union as a single spell. Here a union that converts to marriage CONTINUES:
# marriage is an intermediate state, not an exit. What is plotted is the
# probability of having separated by duration t, from either state, with
# widowhood competing rather than being censoring.
#
#   separated(t) = P(separation while cohabiting by t) + P(separation while married by t)
#
# Compare with the curve in section 2. That one counts widowhood as censoring,
# which in these data is 14 per cent of all endings, so it reads high.

OCC_both <- unionStateOccupancy(BOTH_epi, by = c("country", "cohort"), maxYears = 25)
if (!all(c("country", "cohort") %in% names(OCC_both))) {
  stop("4b.1c: unionStateOccupancy() returned no 'country' / 'cohort' columns. ",
       "An older lib/unionEpisodes.R is still in memory: source it again.")
}

# >>> Claude 2026-09-22
# The two separation states are merged BEFORE the fit rather than added after
# it. Adding two cumulative incidences gives the right point estimate but no
# usable interval, since the variance of a sum is not the sum of the variances.
# Merging the two absorbing states lets survfit return the interval itself.
mergeSeparations <- function (ep) {
  lv <- levels(ep$to)
  lv[lv %in% c("separation (cohabiting)", "separation (married)")] <- "separated"
  levels(ep$to) <- lv
  ep
}

OCC_sep <- unionStateOccupancy(mergeSeparations(BOTH_epi),
                               by = c("country", "cohort"), maxYears = 25)
sepBoth <- subset(OCC_sep, state == "separated")

plot_union_separate <- cohortPlot(
  sepBoth,
  "First unions that have separated, whether or not they married first",
  "Cumulative incidence of separation")
plot_union_separate

# The same split by how the union began, since the two are very different.
# 'origin' is passed as the class variable, which is what KaplanMeierDraw()
# facets on alongside country.
sepByOrigin <- lapply(c("cohabitation", "direct marriage"), function (og) {
  e <- mergeSeparations(subset(BOTH_epi, origin == og))
  o <- unionStateOccupancy(e, by = c("country", "cohort"), maxYears = 25)
  o <- subset(o, state == "separated")
  o$origin <- og
  o
})
sepByOrigin <- do.call(rbind, sepByOrigin)

plot_union_separate_origin <- cohortPlot(
  sepByOrigin,
  "First unions that have separated, by how the union began",
  "Cumulative incidence of separation",
  classVar = "origin")
plot_union_separate_origin
# <<< Claude 2026-09-22


###### 4b.2 Separation before marriage, against conversion to marriage ######
# The honest version of section 4: the cumulative incidence of separating while
# still cohabiting, with conversion as a competing event rather than censoring.
# >>> Claude 2026-09-21
# Mexico, the USA, and the two together.
crPlot <- function (curves, Title, extra = NULL) {
  d <- subset(curves, (state != "(s0)") & (timeYear <= 25))
  p <- ggplot(d, aes(x = timeYear, y = p, colour = state)) +
    geom_step(linewidth = 0.8) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1)) +
    labs(title = Title,
         subtitle = "Marriage treated as a competing event, not as censoring",
         x = "Duration in years after the union began",
         y = "Cumulative incidence", colour = NULL) +
    theme_linedraw() + theme(legend.position = "bottom")
  if (!is.null(extra)) p <- p + extra
  p
}

MEX_cr1 <- unionCompetingRisks(MEX_epi, from = "cohabiting", horizonYears = c(5, 10, 15))
print(MEX_cr1$table)     # <- compare these with the curve in section 4
plot_cr_MEX <- crPlot(MEX_cr1$curves,
                      "Mexico: what happens to a cohabiting first union (Aalen-Johansen)")
plot_cr_MEX

USA_cr1 <- unionCompetingRisks(USA_epi, from = "cohabiting", horizonYears = c(5, 10, 15))
print(USA_cr1$table)
plot_cr_USA <- crPlot(USA_cr1$curves,
                      "USA: what happens to a cohabiting first union (Aalen-Johansen)")
plot_cr_USA

# Combined, one panel per country so the same outcome can be read across.
BOTH_cr1 <- unionCompetingRisks(BOTH_epi, from = "cohabiting", by = "country",
                                horizonYears = c(5, 10, 15))
print(BOTH_cr1$table)
plot_cr_BOTH <- crPlot(BOTH_cr1$curves,
                       "Mexico and the USA: what happens to a cohabiting first union",
                       extra = facet_wrap(~ group))
plot_cr_BOTH

# The same three outcomes by union cohort as well, which is where the two
# countries diverge: CR_both in 4b.1b already holds country x cohort.
# <<< Claude 2026-09-21


###### 4b.3 Does the separation risk fall once a union converts? ######
# 'married' is TIME-VARYING: a union contributes cohabiting exposure until it
# converts and married exposure afterwards. Nothing about its future is used to
# place it, so this is not anticipatory analysis.
MEX_epi$sepEvent <- as.integer(MEX_epi$to %in% c("separation (cohabiting)", "separation (married)"))
MEX_epi$married  <- as.integer(MEX_epi$istate != "cohabiting")

fit_state_MEX <- coxph(Surv(tstart, tstop, sepEvent) ~ married + ageUnion + cluster(id),
                       data = MEX_epi, weights = w)
print(summary(fit_state_MEX))    # the 'married' coefficient IS the drop at conversion

# The same thing as a picture: separation accumulating in each state.
sf_state_MEX <- survfit(Surv(tstart, tstop, sepEvent) ~ married, data = MEX_epi, weights = w)
MEX_haz <- data.frame(timeYear = sf_state_MEX$time / 12,
                      cumhaz   = sf_state_MEX$cumhaz,
                      state    = rep(c("cohabiting", "married"), sf_state_MEX$strata))
plot_stateHaz_MEX <- ggplot(subset(MEX_haz, timeYear <= 25),
                            aes(timeYear, cumhaz, colour = state)) +
  geom_step(linewidth = 0.8) +
  labs(title = "Mexico: cumulative risk of separation, by the state the union is in",
       x = "Duration in years after the union began", y = "Cumulative hazard of separation",
       colour = "Currently") +
  theme_linedraw() + theme(legend.position = "bottom")
plot_stateHaz_MEX


###### 4b.4 Direct against converted marriages, clock reset at the wedding ######
# READ adjustedSurv_note() FIRST. Holding age at union and age at marriage
# fixed is the same as holding cohabDur fixed, and the only cohabitation a
# direct marriage has is zero, so the contrast extrapolates. The restricted
# version below needs much less extrapolation; report both.
MEX_mar <- subset(MEX_epi, istate %in% c("married direct", "married converted"))
MEX_mar$t     <- MEX_mar$tstop - MEX_mar$tstart
MEX_mar$sep   <- as.integer(MEX_mar$to == "separation (married)")
MEX_mar$route <- factor(ifelse(MEX_mar$istate == "married direct", "direct", "converted"),
                        levels = c("direct", "converted"))

adj_all_MEX  <- adjustedSurv(MEX_mar, "t", "sep", "route",
                             covariates = c("ageUnion", "cohabDur"),
                             weightVar = "w", maxTime = 25 * 12)
MEX_mar_short <- subset(MEX_mar, (route == "direct") | (cohabDur <= 2))
adj_short_MEX <- adjustedSurv(MEX_mar_short, "t", "sep", "route",
                              covariates = c("ageUnion", "cohabDur"),
                              weightVar = "w", maxTime = 25 * 12)
cat(sprintf("HR converted vs direct: all cohabitations %.3f | cohabitation under 2 y %.3f\n",
            adj_all_MEX$hr[["routeconverted"]], adj_short_MEX$hr[["routeconverted"]]))

MEX_adj <- rbind(transform(adj_all_MEX$curves,   spec = "all converted marriages"),
                 transform(adj_short_MEX$curves, spec = "cohabitation under 2 years"))
MEX_adj$timeYear <- MEX_adj$time / 12
plot_adj_MEX <- ggplot(MEX_adj, aes(timeYear, surv, colour = group)) +
  geom_line(linewidth = 0.8) + facet_wrap(~ spec) + coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Mexico: still married, direct against converted, standardised",
       subtitle = "Age at union and cohabitation duration held to one common distribution",
       x = "Duration in years after the marriage began", y = "Still married", colour = NULL) +
  theme_linedraw() + theme(legend.position = "bottom")
plot_adj_MEX

# To save any of them, following the pattern used further down this file:
# ggsave(filename = paste0(path_output_plots, "MEX_union_occupancy.pdf"),
#        plot = plot_occupancy_MEX, width = 29.7, height = 21, units = "cm", dpi = 300)
# <<< Claude 2026-09-21


#### All countries: GGS and others ####
goGGS <- function () {
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
                   "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2","yBirth","weight","popWeight")]
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
                                               varEvent="marriage_start_cmc1", varCens="varUnionCens1",
                                               "popWeight",
                                               varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                               xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
  
  plot_marriage_union2_mod <- KaplanMeierPlot (df=dd, varEnter="union_start_cmc2",
                                               varEvent="marriage_start_cmc2", varCens="varUnionCens2",
                                               "popWeight",
                                               varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                               xTitle = "Duration in years after second union", maxX = 20, confInt=TRUE)
  
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
                                          varEvent="union_endBySep_cmc1", varCens="varSepCens1",
                                          "popWeight",
                                          varClass=NULL, varCountry="country", vecCountry=vecCountry, cohortsList=cohortsList,
                                          xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
  
}


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
                                      "yBirth","weight","popWeight")]
USA_union1_marr$yUnion1 <- yearFrom_cmc(USA_union1_marr$union_start_cmc1)

# Second union
USA_union2_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988","NSFG2017_19","NSFG2022_23")))
USA_union2_marr <- USA_union2_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc2","union_end_cmc2","union_end_motive2","marriage_start_cmc2",
                                      "yBirth","weight","popWeight")]
USA_union2_marr$yUnion2 <- yearFrom_cmc(USA_union2_marr$union_start_cmc2)

##### 0. cleaning ... #####
# modify censored date for transition to union for marriage: if there is an end of union, it is the censored date
USA_union1_marr$varUnionCens1 <- ifelse(!is.na(USA_union1_marr$union_end_cmc1),
                                        USA_union1_marr$union_end_cmc1,USA_union1_marr$surveyDate_cmc)
USA_union2_marr$varUnionCens2 <- ifelse(!is.na(USA_union2_marr$union_end_cmc2),
                                        USA_union2_marr$union_end_cmc2,USA_union2_marr$surveyDate_cmc)

cohortsListUSA_Marr <- c(
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

# >>> Claude 2026-09-22
# STATUS: correct. Keep.
# Measures: of all first unions, the share that have reached marriage by
# duration d. Direct marriages are included, so about half the unions marry at
# duration 0 and the curve starts near 0.46 after a vertical drop from 1.
# That is right for 'how does a union of any kind reach marriage'. For 'how
# fast do cohabiting unions convert', use plot_cohab_convert in 4b.1b instead.
# <<< Claude 2026-09-22

##### 1. Union ==> Marriage #####

plot_marriage_union1_USA <- KaplanMeierPlot (df=USA_union1_marr, varEnter="union_start_cmc1",
                                             varEvent="marriage_start_cmc1", varCens="varUnionCens1", var_yBirth = "yUnion1",
                                             "popWeight",
                                             varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr,
                                             Title="USA: transition of first union to marriage, by union cohort",
                                             xTitle = "Duration in years after first union", maxX = 20, confInt=TRUE)
plot_marriage_union1_USA

plot_marriage_union2_USA <- KaplanMeierPlot (df=USA_union2_marr, varEnter="union_start_cmc2",
                                             varEvent="marriage_start_cmc2", varCens="varUnionCens2", var_yBirth = "yUnion2",
                                             "popWeight",
                                             varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr,
                                             Title="USA: transition of second union to marriage, by union cohort",
                                             xTitle = "Duration in years after second union", maxX = 20, confInt=TRUE)

plot_marriage_union2_USA


# >>> Claude 2026-09-22
# STATUS: correct, but read it as the NET measure.
# Widowhood is censored, so the woman leaves the risk set when her partner
# dies. The curve is therefore the share who would separate if no partner had
# died, that is, male mortality removed. It is computed correctly and it is a
# reasonable headline for a Mexico against USA comparison, since it takes out
# a mortality difference you are not measuring. It is NOT a share of women:
# net separation + net widowhood + still in union exceeds 1.
# For the CRUDE share, who actually separated, see 4b.1c:
#   plot_union_separate, plot_union_separate_origin
# <<< Claude 2026-09-22

##### 2. Union ==> Separation #####
# first union
USA_union1_sep <- subset(NSFG_ENADID, !(survey %in% c("NSFG2017_19")))
USA_union1_sep <- USA_union1_sep[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight","popWeight")]
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
                                      "yBirth","weight","popWeight")]
USA_union2_sep$yUnion2 <- yearFrom_cmc(USA_union2_sep$union_start_cmc2)

# modify censored dates for widowhood
USA_union2_sep$varSepCens2 <- ifelse((!is.na(USA_union2_sep$union_end_motive2)&(USA_union2_sep$union_end_motive2=="widowhood")),
                                     USA_union2_sep$union_end_cmc2,USA_union2_sep$surveyDate_cmc)
# widowhood is not the event, only separation is
USA_union2_sep$union_endBySep_cmc2 <- ifelse((!is.na(USA_union2_sep$union_end_motive2)&(USA_union2_sep$union_end_motive2=="widowhood")),
                                             NA,USA_union2_sep$union_end_cmc2)

cohortsListUSA_Marr2 <- c(
  c(1945,1949),
  c(1950,1959),
  c(1960,1969),
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_union_sep1_USA <- KaplanMeierPlot (df=USA_union1_sep, varEnter="union_start_cmc1",
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1",
                                        "popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr2,
                                        Title="USA: Separation after first union, by union cohort",
                                        xTitle = "Duration in years after first union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep1_USA
plot_union_sep2_USA <- KaplanMeierPlot (df=USA_union2_sep, varEnter="union_start_cmc2",
                                        varEvent="union_endBySep_cmc2", varCens="varSepCens2", var_yBirth = "yUnion2",
                                        "popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr2,
                                        Title="USA: Separation after second union, by union cohort",
                                        xTitle = "Duration in years after second union", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_union_sep2_USA

# >>> Claude 2026-09-22
# STATUS: correct. One composition point.
# The women entering here are pooled regardless of how the first union ended,
# and roughly a fifth are widows. Repartnering after widowhood is a different
# process from repartnering after separation. Splitting on union_end_motive1
# is one subset() if you want the two apart.
# <<< Claude 2026-09-22

##### 3. First repartnering #####
USA_sep1_union2 <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988","NSFG2017_19","NSFG2022_23")))
USA_sep1_union2 <- USA_sep1_union2[,c("country","survey","surveyDate_cmc",
                                      "union_end_cmc1","union_start_cmc2",
                                      "yBirth","weight","popWeight")]
USA_sep1_union2$ySep1 <- yearFrom_cmc(USA_sep1_union2$union_end_cmc1)

cohortsListUSA_Marr3 <- c(
  c(1970,1979),
  c(1980,1989),
  c(1990,1999),
  c(2000,2009),
  c(2010,2019)
)

plot_sep1_union2_USA <- KaplanMeierPlot (df=USA_sep1_union2, varEnter="union_end_cmc1",
                                         varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1",
                                         "popWeight",
                                         varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr3,
                                         Title="USA: First repartnering, by union cohort",
                                         xTitle = "Duration in years after first separation", yTitle="Proportion of first separation",
                                         maxX = 20, confInt=TRUE,
                                         inverseFunction = TRUE)
plot_sep1_union2_USA

# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by 4b.1b.
# Censoring at marriage asks what would happen if a cohabiting couple could
# never marry. Unlike a partner's death, that is not a hazard you can imagine
# removing: conversion is an act of the same couple, and the couples who marry
# are exactly those least likely to separate, so the independence that
# censoring assumes fails. In simulation it overstated the ten-year figure by
# 83 per cent.
#
# Use instead, from one Aalen-Johansen fit, both countries, by union cohort:
#   plot_cohab_remain    still cohabiting
#   plot_cohab_convert   converted to marriage
#   plot_cohab_separate  separated before marrying  <- what this block wanted
# They sum to 1 with the widowed and the unknown endings at every duration.
# <<< Claude 2026-09-22

##### 4. First cohabitation ==> separation with marriage as censored #####
# >>> Claude 2026-09-22
# Superseded by 4b.1b. Skipped while WRONG is TRUE; set WRONG <- FALSE
# at the top of the file to run it again for comparison.
if (!WRONG) {
USA_cohab1_sep_no_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982","NSFG2017_19")))
USA_cohab1_sep_no_marr <- USA_cohab1_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_start_type1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight","popWeight")]
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
                                        varEvent="union_endBySep_cmc1", varCens="varSepCens1", var_yBirth = "yUnion1",
                                        "popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr3,
                                        Title="USA: Separation of first cohabitation (marriage censored), by union cohort",
                                        xTitle = "Duration in years after first cohabitation", yTitle="Proportion of separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)
plot_cohab_sep1_USA
}   # end if (!WRONG)
# <<< Claude 2026-09-22


#### Mexico and USA combined plots ####
# >>> Claude 2026-09-22
# STATUS: correct. Keep.
# Measures: of all first unions, the share that have reached marriage by
# duration d. Direct marriages are included, so about half the unions marry at
# duration 0 and the curve starts near 0.46 after a vertical drop from 1.
# That is right for 'how does a union of any kind reach marriage'. For 'how
# fast do cohabiting unions convert', use plot_cohab_convert in 4b.1b instead.
# <<< Claude 2026-09-22

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

pathFile <- paste0(path_output_plots,"MEX_USA_Union1_marriage.pdf")
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

pathFile <- paste0(path_output_plots,"MEX_USA_Union2_marriage.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union2_marriage_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

# >>> Claude 2026-09-22
# STATUS: correct, but read it as the NET measure.
# Widowhood is censored, so the woman leaves the risk set when her partner
# dies. The curve is therefore the share who would separate if no partner had
# died, that is, male mortality removed. It is computed correctly and it is a
# reasonable headline for a Mexico against USA comparison, since it takes out
# a mortality difference you are not measuring. It is NOT a share of women:
# net separation + net widowhood + still in union exceeds 1.
# For the CRUDE share, who actually separated, see 4b.1c:
#   plot_union_separate, plot_union_separate_origin
# <<< Claude 2026-09-22

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

pathFile <- paste0(path_output_plots,"MEX_USA_Union_sep1.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union_sep1_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

##### 4. Second union ==> separation ######
combined_plot <-
  (plot_union_sep2_MEX + labs(title="MEXICO")) +
  (plot_union_sep2_USA + labs(title="USA"))

MEX_USA_union_sep2_plot <- combined_plot + 
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

pathFile <- paste0(path_output_plots,"MEX_USA_Union_sep2.pdf")
ggsave(filename = pathFile, plot = MEX_USA_union_sep2_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by 4b.1b.
# Censoring at marriage asks what would happen if a cohabiting couple could
# never marry. Unlike a partner's death, that is not a hazard you can imagine
# removing: conversion is an act of the same couple, and the couples who marry
# are exactly those least likely to separate, so the independence that
# censoring assumes fails. In simulation it overstated the ten-year figure by
# 83 per cent.
#
# Use instead, from one Aalen-Johansen fit, both countries, by union cohort:
#   plot_cohab_remain    still cohabiting
#   plot_cohab_convert   converted to marriage
#   plot_cohab_separate  separated before marrying  <- what this block wanted
# They sum to 1 with the widowed and the unknown endings at every duration.
# <<< Claude 2026-09-22

##### 5. Separation of first cohabitation, marriage censored #####
# >>> Claude 2026-09-22
# Superseded by 4b.1b. Skipped while WRONG is TRUE; set WRONG <- FALSE
# at the top of the file to run it again for comparison.
if (!WRONG) {
combined_plot <-
  (plot_cohab_sep1_MEX + labs(title="MEXICO")) +
  (plot_cohab_sep1_USA + labs(title="USA"))

MEX_USA_cohab_sep1_plot <- combined_plot + 
  plot_annotation(
    #title = "Separation of first cohabitation, marriage censored, by union cohort",
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

pathFile <- paste0(path_output_plots,"MEX_USA_Cohabitation_sep1.pdf")
ggsave(filename = pathFile, plot = MEX_USA_cohab_sep1_plot, width = 29.7, height = 21, units = "cm", dpi = 300)
}   # end if (!WRONG)
# <<< Claude 2026-09-22

# >>> Claude 2026-09-22
# STATUS: correct. One composition point.
# The women entering here are pooled regardless of how the first union ended,
# and roughly a fifth are widows. Repartnering after widowhood is a different
# process from repartnering after separation. Splitting on union_end_motive1
# is one subset() if you want the two apart.
# <<< Claude 2026-09-22

##### 6. First repartnering #####
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

pathFile <- paste0(path_output_plots,"MEX_USA_sep1_union2.pdf")
ggsave(filename = pathFile, plot = MEX_USA_sep1_union2_plot, width = 29.7, height = 21, units = "cm", dpi = 300)
