# >>> Claude 2026-09-25
# archive/KaplanMeier_marriage_censored.R
#
# RETIRED CODE, kept for the record. Separation of cohabiting first (and
# second) unions with MARRIAGE TREATED AS CENSORING, by union cohort. This is
# the specification of Figure 6 in the paper as first drafted
# ("Separation of cohabitation (marriage censored)").
#
# WHY IT WAS RETIRED. Censoring at marriage estimates what would happen if a
# cohabiting couple could never marry. The couples who marry are those least
# likely to separate, so the independence that censoring assumes fails and the
# curve overstates separation (by 83 per cent at ten years in a simulation).
# The replacement is block C6 of MEX_USA_figures_cohort.R: one Aalen-Johansen
# fit in which conversion to marriage competes with separation.
#
# It was moved here on 2026-09-25 from three blocks of KaplanMeier.R that had
# been switched off with 'if (!WRONG)'. It still runs, for a robustness note:
# open it in RStudio and Source. Its output file is MEX_USA_Cohabitation_sep1.pdf.

setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
source("enadid_lib.R")
library(patchwork)
path_output_plots <- paste0(outputPath, "/")
if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
MEXICO_ENADID <- filterDateQuality(MEXICO_ENADID)
NSFG_ENADID   <- filterDateQuality(NSFG_ENADID)
# <<< Claude 2026-09-25

#### MEXICO ####
# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by block C6 of MEX_USA_figures_cohort.R.
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
# <<< Claude 2026-09-22

#### USA ####
# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by block C6 of MEX_USA_figures_cohort.R.
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
USA_cohab1_sep_no_marr <- subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982","NSFG2017_19")))
USA_cohab1_sep_no_marr <- USA_cohab1_sep_no_marr[,c("country","survey","surveyDate_cmc",
                                      "union_start_cmc1","union_start_type1","union_end_cmc1","union_end_motive1","marriage_start_cmc1",
                                      "yBirth","weight","popWeight")]
# Corrected when archived: the original took the union year from USA_union1_marr,
# a different data frame, so the cohorts were misassigned.
USA_cohab1_sep_no_marr$yUnion1 <- yearFrom_cmc(USA_cohab1_sep_no_marr$union_start_cmc1)
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
# <<< Claude 2026-09-22

#### Mexico and USA combined ####
# >>> Claude 2026-09-22
# STATUS: WRONG. Do not use. Replaced by block C6 of MEX_USA_figures_cohort.R.
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
# <<< Claude 2026-09-22
