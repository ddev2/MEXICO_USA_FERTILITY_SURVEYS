# >>> Claude 2026-09-25
# ARCHIVED on 2026-09-25 (archive/KaplanMeier.R). This was the main script of
# union transitions. Its figures are now produced by MEX_USA_figures_cohort.R,
# and its further analyses (old 4b.2 to 4b.4) are in MEX_USA_union_models.R.
# What remains here are the per-country Kaplan-Meier curves and occupancy
# plots (the panels of the combined figures), and the unused goEDER() and
# goGGS() functions. It still runs: the line below moves to the repository root.
setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
# <<< Claude 2026-09-25
source("enadid_lib.R")
source("lib/KaplanMeierLib.R")
# >>> Claude 2026-09-21
source("lib/unionEpisodes.R")   # buildUnionEpisodes, unionStateOccupancy, unionCompetingRisks
source("lib/adjustedSurv.R")    # adjustedSurv, adjustedSurv_note
library(survival)
# <<< Claude 2026-09-21
path_output_plots <- paste0(outputPath, "/")

# >>> Claude 2026-09-25
if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
# <<< Claude 2026-09-25
MEXICO_ENADID <- filterDateQuality(MEXICO_ENADID)
NSFG_ENADID   <- filterDateQuality(NSFG_ENADID)

# >>> Claude 2026-09-25
# KaplanMeier.R: the country-by-country ANALYSIS of union transitions.
#
# The figures of the paper and of the presentation are NOT drawn here any
# more. They are in
#   MEX_USA_figures_cohort.R   by union cohort (Kaplan-Meier and Aalen-Johansen)
#   MEX_USA_figures_period.R   by calendar period (ppr_doIt)
# This file keeps the per-country curves those figures are made of, the EDER
# and GGS comparisons, and the Aalen-Johansen and Cox explorations of 4b.2 to
# 4b.4. The survey selections and the analysis samples come from
# lib/mexUsaFigures.R, so they are the same as in the figures.
#
# Retired: the three blocks that censored at marriage (old sections 4, and
# combined section 5) are in archive/KaplanMeier_marriage_censored.R.
#
# NET against CRUDE. A curve that censors a competing event estimates the net
# probability: what would happen if that event could not occur. Censoring
# WIDOWHOOD is defensible, since a partner's death is plausibly unrelated to the
# couple's propensity to separate. Censoring MARRIAGE is not. The crude
# probability comes from the Aalen-Johansen blocks in 4b. See docs/05_methods.md.
#
# The Kaplan-Meier estimator is right-continuous, survival::survfit() supplies
# the estimate and the interval (log-log bounds, infinitesimal jackknife
# variance), and filterDateQuality() removes the dateless separations. The
# intervals ignore the sampling design: no strata, no clusters.
# <<< Claude 2026-09-25

#### MEXICO ####
# >>> Claude 2026-09-25
# Samples and survey selections from lib/mexUsaFigures.R. The reasons for each
# selection are listed there (WFS: no cohabitation before marriage; ENADID
# 1992: no unions; ENADID 2006: too many missing first-union dates; EDER 2025: marriage dates not usable; ENADID 2009-2023:
# first and last union only, so second unions come from ENADID 1997 and EDER).
# varUnionCens: the union ends or the survey is reached, whichever comes first.
MEX_union1_marr <- sampleUnionMarriage(selectSurveys(MEXICO_ENADID, "union1_marriage", "MEXICO"), u = 1)
MEX_union2_marr <- sampleUnionMarriage(selectSurveys(MEXICO_ENADID, "union2_marriage", "MEXICO"), u = 2)
cohortsListMex_Marr <- COHORTS$MEXICO
# <<< Claude 2026-09-25

# >>> Claude 2026-09-22
# STATUS: correct. Keep.
# Measures: of all first unions, the share that have reached marriage by
# duration d. Direct marriages are included, so about half the unions marry at
# duration 0 and the curve starts near 0.46 after a vertical drop from 1.
# That is right for 'how does a union of any kind reach marriage'. For 'how
# fast do cohabiting unions convert', use block C6 of MEX_USA_figures_cohort.R instead.
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
# For the CRUDE share, who actually separated, see block C8 of
# MEX_USA_figures_cohort.R.
# <<< Claude 2026-09-22

##### 2. Union ==> Separation #####
# >>> Claude 2026-09-25
# Widowhood is censoring (varSepCens), and only a separation is an event
# (union_endBySep_cmc). Second unions: ENADID 1997, EDER 2017 and EDER 2025.
MEX_union1_sep <- sampleUnionSeparation(selectSurveys(MEXICO_ENADID, "union1_separation", "MEXICO"), u = 1)
MEX_union2_sep <- sampleUnionSeparation(selectSurveys(MEXICO_ENADID, "union2_separation", "MEXICO"), u = 2)
# <<< Claude 2026-09-25

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
# >>> Claude 2026-09-25
MEX_sep1_union2 <- sampleRepartnering(selectSurveys(MEXICO_ENADID, "repartnering", "MEXICO"))
# <<< Claude 2026-09-25

plot_sep1_union2_MEX <- KaplanMeierPlot (df=MEX_sep1_union2, varEnter="union_end_cmc1",
                                        varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1",
                                        varWeight="popWeight",
                                        varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListMex_Marr,
                                        Title="Mexico: First repartnering, by union cohort",
                                        xTitle = "Duration in years after first separation",
                                        yTitle="Proportion of first separation",
                                        maxX = 20, confInt=TRUE,
                                        inverseFunction = TRUE)

# >>> Claude 2026-09-25
# Old section 4 (separation of cohabitations, marriage censored) is retired:
# archive/KaplanMeier_marriage_censored.R. Its replacement is block C6 of
# MEX_USA_figures_cohort.R.
# <<< Claude 2026-09-25

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

# >>> Claude 2026-09-25
# The episodes, the cohorts and the thin-cell rule now come from
# buildBothEpisodes() in lib/mexUsaFigures.R, shared with the figure script.
EP <- buildBothEpisodes(MEXICO_ENADID, NSFG_ENADID, minUnions = 200)
MEX_epi  <- EP$MEX
USA_epi  <- EP$USA
BOTH_epi <- EP$BOTH
# <<< Claude 2026-09-25


###### 4b.1 Where unions are, by duration (state occupancy) ######
# Replaces the single curve with the whole picture: still cohabiting, married,
# separated, widowed. The bands sum to 1 at every duration.
# >>> Claude 2026-09-21
# >>> Claude 2026-09-25
# occupancyPlot() is now in lib/mexUsaFigures.R.
# <<< Claude 2026-09-25

MEX_occ1 <- unionStateOccupancy(MEX_epi, by = "origin", maxYears = 25)
plot_occupancy_MEX <- occupancyPlot(
  MEX_occ1, "Mexico: where first unions are, by duration and by how they began")
plot_occupancy_MEX

USA_occ1 <- unionStateOccupancy(USA_epi, by = "origin", maxYears = 25)
plot_occupancy_USA <- occupancyPlot(
  USA_occ1, "USA: where first unions are, by duration and by how they began")
plot_occupancy_USA

# >>> Claude 2026-09-25
# The combined Mexico-USA figures that followed here are in
# MEX_USA_figures_cohort.R:
#   old 4b.1 combined  -> C7  MEX_USA_union1_AJ.pdf
#   old 4b.1b          -> C6  cohabiting first unions: remain, convert, separate
#   old 4b.1c          -> C8  first unions that have separated, crude
# <<< Claude 2026-09-25


# >>> Claude 2026-09-25
# Sections 4b.2 to 4b.4 moved to MEX_USA_union_models.R (blocks M1 to M3).
# <<< Claude 2026-09-25
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
# >>> Claude 2026-09-25
# Samples and survey selections from lib/mexUsaFigures.R.
USA_union1_marr <- sampleUnionMarriage(selectSurveys(NSFG_ENADID, "union1_marriage", "USA"), u = 1)
USA_union2_marr <- sampleUnionMarriage(selectSurveys(NSFG_ENADID, "union2_marriage", "USA"), u = 2)
cohortsListUSA_Marr <- COHORTS$USA_marriage
# <<< Claude 2026-09-25

# >>> Claude 2026-09-22
# STATUS: correct. Keep.
# Measures: of all first unions, the share that have reached marriage by
# duration d. Direct marriages are included, so about half the unions marry at
# duration 0 and the curve starts near 0.46 after a vertical drop from 1.
# That is right for 'how does a union of any kind reach marriage'. For 'how
# fast do cohabiting unions convert', use block C6 of MEX_USA_figures_cohort.R instead.
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
# For the CRUDE share, who actually separated, see block C8 of
# MEX_USA_figures_cohort.R.
# <<< Claude 2026-09-22

##### 2. Union ==> Separation #####
# >>> Claude 2026-09-25
USA_union1_sep <- sampleUnionSeparation(selectSurveys(NSFG_ENADID, "union1_separation", "USA"), u = 1)
USA_union2_sep <- sampleUnionSeparation(selectSurveys(NSFG_ENADID, "union2_separation", "USA"), u = 2)
cohortsListUSA_Marr2 <- COHORTS$USA_separation
# <<< Claude 2026-09-25

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
# >>> Claude 2026-09-25
USA_sep1_union2 <- sampleRepartnering(selectSurveys(NSFG_ENADID, "repartnering", "USA"))
cohortsListUSA_Marr3 <- COHORTS$USA_repartnering
# <<< Claude 2026-09-25

plot_sep1_union2_USA <- KaplanMeierPlot (df=USA_sep1_union2, varEnter="union_end_cmc1",
                                         varEvent="union_start_cmc2", varCens="surveyDate_cmc", var_yBirth = "ySep1",
                                         "popWeight",
                                         varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=cohortsListUSA_Marr3,
                                         Title="USA: First repartnering, by union cohort",
                                         xTitle = "Duration in years after first separation", yTitle="Proportion of first separation",
                                         maxX = 20, confInt=TRUE,
                                         inverseFunction = TRUE)
plot_sep1_union2_USA

# >>> Claude 2026-09-25
# Old section 4 (marriage censored) is retired: archive/KaplanMeier_marriage_censored.R.
# <<< Claude 2026-09-25


#### Mexico and USA combined plots ####
# >>> Claude 2026-09-25
# Moved to MEX_USA_figures_cohort.R, blocks C1 to C5 (Figures 1, 2, 3, 4 and 7).
# The retired combined section 5 (marriage censored) is in
# archive/KaplanMeier_marriage_censored.R.
# <<< Claude 2026-09-25
