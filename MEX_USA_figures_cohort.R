# >>> Claude 2026-09-25
# MEX_USA_figures_cohort.R
#
# The Mexico-USA figures BY UNION COHORT, as they appear in the paper and in
# the September 2026 presentation. Each block builds the two national samples,
# estimates the curves, puts Mexico and the USA side by side and saves the
# figure to outputPath (see enadid_lib.R and config_local.R).
#
# HOW TO RUN. Build MEXICO_ENADID and NSFG_ENADID first (docs/01_build_data.md).
# Then open this file in RStudio and Source it. It loads the two data frames
# itself when they are not already in memory. Every block is independent once
# section 0 has run, so a single figure can be rebuilt by running section 0
# and that block.
#
# FIGURE INDEX
#
#   block  paper / slide              estimator       measure  output file
#   C1     Figure 1, slide 10         Kaplan-Meier    net      MEX_USA_Union1_marriage.pdf
#   C2     Figure 2, slide 11         Kaplan-Meier    net      MEX_USA_Union2_marriage.pdf
#   C3     Figure 3, slide 12         Kaplan-Meier    net      MEX_USA_Union_sep1.pdf
#   C4     Figure 4, slide 13         Kaplan-Meier    net      MEX_USA_Union_sep2.pdf
#   C5     Figure 7, slide 15         Kaplan-Meier    net      MEX_USA_sep1_union2.pdf
#   C6     replaces Figure 6          Aalen-Johansen  crude    MEX_USA_union1_cohab_remain_AJ.pdf
#                                                              MEX_USA_union1_cohab_convert_AJ.pdf
#                                                              MEX_USA_union1_cohab_separate_AJ.pdf
#   C7     state occupancy            Aalen-Johansen  crude    MEX_USA_union1_AJ.pdf
#   C8     separation, crude          Aalen-Johansen  crude    MEX_USA_union1_separated_AJ.pdf
#                                                              MEX_USA_union1_separated_origin_AJ.pdf
#
# Elsewhere:
#   slide 16, life course from 15 to 45 (AJ, crude)   MEX_USA_lifecourse_AJ.R
#   slides 7-8, KM against AJ for Mexico              presentation_KM_AJ.R
#   Figure 6 as printed (separation, marriage CENSORED) is no longer produced:
#     its code is in archive/KaplanMeier_marriage_censored.R. C6 replaces it.
#
# NET AND CRUDE. A Kaplan-Meier curve that censors a competing event gives the
# NET probability: what would happen if that event could not occur. That is
# defensible for widowhood (C3, C4) and for separation when the question is
# the pace of marriage (C1, C2). It is not defensible for marriage when the
# question is the separation of cohabitations, because the couples who marry
# are those least likely to separate. The CRUDE probability, the share who
# actually experience each outcome, comes from the Aalen-Johansen estimator
# (C6 to C8). docs/05_methods.md gives the argument and the references.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")          # also loads lib/mexUsaFigures.R and lib/unionEpisodes.R
library(survival)
library(patchwork)


# ==== 0. Data ====

if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
MEX <- filterDateQuality(MEXICO_ENADID)
USA <- filterDateQuality(NSFG_ENADID)

# One Kaplan-Meier figure: the same call for each country, then side by side.
kmFigure <- function (sampleMEX, sampleUSA, varEnter, varEvent, varCens, varCohort,
                      cohortsMEX, cohortsUSA, xTitle, yTitle = "Survival Probability",
                      inverseFunction = FALSE) {
  onePanel <- function (d, cohorts) {
    KaplanMeierPlot(df = d, varEnter = varEnter, varEvent = varEvent, varCens = varCens,
                    varWeight = "popWeight", var_yBirth = varCohort,
                    varClass = NULL, varCountry = NULL, vecCountry = NULL,
                    cohortsList = cohorts, xTitle = xTitle, yTitle = yTitle,
                    maxX = 20, confInt = TRUE, inverseFunction = inverseFunction)
  }
  panels <- list(MEXICO = onePanel(sampleMEX, cohortsMEX),
                 USA    = onePanel(sampleUSA, cohortsUSA))
  list(panels = panels, figure = combineCountries(panels$MEXICO, panels$USA))
}


# ==== C1. First union -> marriage (Figure 1) ====
#
# Of all first unions, the share not yet married by duration d. Direct
# marriages are included, so the curve drops at duration 0 by the share of
# unions that began as a marriage, and then declines as cohabitations convert.
# Separation and widowhood are censoring.

fig_C1 <- kmFigure(
  sampleUnionMarriage(selectSurveys(MEX, "union1_marriage", "MEXICO"), u = 1),
  sampleUnionMarriage(selectSurveys(USA, "union1_marriage", "USA"),    u = 1),
  varEnter = "union_start_cmc1", varEvent = "marriage_start_cmc1",
  varCens = "varUnionCens1", varCohort = "yUnion1",
  cohortsMEX = COHORTS$MEXICO, cohortsUSA = COHORTS$USA_marriage,
  xTitle = "Duration in years after first union")
fig_C1$figure
saveFigure(fig_C1$figure, "MEX_USA_Union1_marriage.pdf")


# ==== C2. Second union -> marriage (Figure 2) ====

fig_C2 <- kmFigure(
  sampleUnionMarriage(selectSurveys(MEX, "union2_marriage", "MEXICO"), u = 2),
  sampleUnionMarriage(selectSurveys(USA, "union2_marriage", "USA"),    u = 2),
  varEnter = "union_start_cmc2", varEvent = "marriage_start_cmc2",
  varCens = "varUnionCens2", varCohort = "yUnion2",
  cohortsMEX = COHORTS$MEXICO, cohortsUSA = COHORTS$USA_marriage,
  xTitle = "Duration in years after second union")
fig_C2$figure
saveFigure(fig_C2$figure, "MEX_USA_Union2_marriage.pdf")


# ==== C3. Separation of the first union (Figure 3) ====
#
# NET measure: widowhood is censoring, so the curve is the share who would
# separate if no partner died. It is not a share of women: net separation plus
# net widowhood plus still in union exceeds 1. For the crude share see C8.

fig_C3 <- kmFigure(
  sampleUnionSeparation(selectSurveys(MEX, "union1_separation", "MEXICO"), u = 1),
  sampleUnionSeparation(selectSurveys(USA, "union1_separation", "USA"),    u = 1),
  varEnter = "union_start_cmc1", varEvent = "union_endBySep_cmc1",
  varCens = "varSepCens1", varCohort = "yUnion1",
  cohortsMEX = COHORTS$MEXICO, cohortsUSA = COHORTS$USA_separation,
  xTitle = "Duration in years after start of first union",
  yTitle = "Proportion of separation", inverseFunction = TRUE)
fig_C3$figure
saveFigure(fig_C3$figure, "MEX_USA_Union_sep1.pdf")


# ==== C4. Separation of the second union (Figure 4) ====

fig_C4 <- kmFigure(
  sampleUnionSeparation(selectSurveys(MEX, "union2_separation", "MEXICO"), u = 2),
  sampleUnionSeparation(selectSurveys(USA, "union2_separation", "USA"),    u = 2),
  varEnter = "union_start_cmc2", varEvent = "union_endBySep_cmc2",
  varCens = "varSepCens2", varCohort = "yUnion2",
  cohortsMEX = COHORTS$MEXICO, cohortsUSA = COHORTS$USA_separation,
  xTitle = "Duration in years after start of second union",
  yTitle = "Proportion of separation", inverseFunction = TRUE)
fig_C4$figure
saveFigure(fig_C4$figure, "MEX_USA_Union_sep2.pdf")


# ==== C5. First re-partnering (Figure 7) ====
#
# From the end of the first union to the start of the second, by the cohort of
# the first union's end. Women whose first union ended by widowhood are
# included; splitting on union_end_motive1 separates the two processes.

fig_C5 <- kmFigure(
  sampleRepartnering(selectSurveys(MEX, "repartnering", "MEXICO")),
  sampleRepartnering(selectSurveys(USA, "repartnering", "USA")),
  varEnter = "union_end_cmc1", varEvent = "union_start_cmc2",
  varCens = "surveyDate_cmc", varCohort = "ySep1",
  cohortsMEX = COHORTS$MEXICO, cohortsUSA = COHORTS$USA_repartnering,
  xTitle = "Duration in years after first separation",
  yTitle = "Proportion who have re-partnered", inverseFunction = TRUE)
fig_C5$figure
saveFigure(fig_C5$figure, "MEX_USA_sep1_union2.pdf")


# ==== Aalen-Johansen figures: the union episodes ====
#
# A first union is split into episodes, one per state it passes through:
# cohabiting, then possibly married (converted), or married from the start
# (direct). Marriage is an intermediate state, not an exit, and only the
# survey date is censoring. See lib/unionEpisodes.R.

EP <- buildBothEpisodes(MEX, USA, minUnions = 200)


# ==== C6. Cohabiting first unions: remain, convert, separate (replaces Figure 6) ====
#
# One Aalen-Johansen fit on the cohabiting spell of the first unions that began
# as cohabitation. At every duration
#   still cohabiting + converted + separated + widowed + ended (unknown) = 1
# so the three figures are three readings of one decomposition.

CR_both <- unionCompetingRisks(EP$BOTH, from = "cohabiting", by = c("country", "cohort"),
                               horizonYears = c(5, 10, 15))
print(CR_both$table)     # the numbers behind the three figures

curCohab <- subset(CR_both$curves, timeYear <= 25)
curCohab$cohort  <- factor(curCohab$cohort, levels = COHORT_LABELS)
curCohab$country <- factor(curCohab$country)

cohabPlot <- function (st, Title, yTitle, anchorAtOne = FALSE) {
  d <- subset(curCohab, state == st)
  if (nrow(d) == 0) stop("C6: no rows for state '", st, "'")
  cohortPlot(d, Title, yTitle, anchorAtOne = anchorAtOne)
}

plot_cohab_remain <- cohabPlot(
  "(s0)", "Cohabiting first unions that are still cohabiting",
  "Still cohabiting, neither married nor separated", anchorAtOne = TRUE)
plot_cohab_convert <- cohabPlot(
  "married converted", "Cohabiting first unions that have converted to marriage",
  "Cumulative incidence of conversion")
plot_cohab_separate <- cohabPlot(
  "separation (cohabiting)", "Cohabiting first unions that have separated before marrying",
  "Cumulative incidence of separation")
plot_cohab_remain
plot_cohab_convert
plot_cohab_separate
saveFigure(plot_cohab_remain,   "MEX_USA_union1_cohab_remain_AJ.pdf")
saveFigure(plot_cohab_convert,  "MEX_USA_union1_cohab_convert_AJ.pdf")
saveFigure(plot_cohab_separate, "MEX_USA_union1_cohab_separate_AJ.pdf")


# ==== C7. Where first unions are, by duration and by how they began ====
#
# One fit stratified on origin AND country, so the bands sum to 1 in every
# panel and the two countries are directly comparable.

BOTH_occ1 <- unionStateOccupancy(EP$BOTH, by = c("origin", "country"), maxYears = 25)
plot_occupancy_BOTH <- occupancyPlot(
  BOTH_occ1, "Mexico and the USA: where first unions are, by duration and by how they began",
  facets = origin ~ country)
plot_occupancy_BOTH
saveFigure(plot_occupancy_BOTH, "MEX_USA_union1_AJ.pdf")


# ==== C8. First unions that have separated, crude ====
#
# The crude counterpart of C3: a union that converts to marriage continues,
# widowhood competes instead of being censoring, and what is plotted is the
# probability of having separated by duration t from either state.

OCC_sep <- unionStateOccupancy(mergeSeparations(EP$BOTH), by = c("country", "cohort"),
                               maxYears = 25)
plot_union_separate <- cohortPlot(
  subset(OCC_sep, state == "separated"),
  "First unions that have separated, whether or not they married first",
  "Cumulative incidence of separation")
plot_union_separate
saveFigure(plot_union_separate, "MEX_USA_union1_separated_AJ.pdf")

sepByOrigin <- do.call(rbind, lapply(c("cohabitation", "direct marriage"), function (og) {
  o <- unionStateOccupancy(mergeSeparations(subset(EP$BOTH, origin == og)),
                           by = c("country", "cohort"), maxYears = 25)
  o <- subset(o, state == "separated")
  o$origin <- og
  o
}))
plot_union_separate_origin <- cohortPlot(
  sepByOrigin, "First unions that have separated, by how the union began",
  "Cumulative incidence of separation", classVar = "origin")
plot_union_separate_origin
saveFigure(plot_union_separate_origin, "MEX_USA_union1_separated_origin_AJ.pdf")
# <<< Claude 2026-09-25
