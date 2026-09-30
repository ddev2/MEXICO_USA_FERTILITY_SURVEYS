# >>> Claude 2026-09-25
# MEX_USA_figures_mirrored.R
#
# Mirrored curves of first union and first birth, Mexico and the USA side by
# side, by birth cohort of the woman (Billari 2001).
#
# HOW TO READ THE FIGURE. Time 0 is the first of the two events.
#
#   right half  women whose first union came first. At x years, the height is
#               P(union first) x P(no first birth within x years of the union).
#               The curve falls as these women have their first child.
#   left half   women whose first birth came first. At -x years, the height is
#               1 - P(birth first) x P(no union within x years of the birth).
#               Read from 0 to the left: it rises as these mothers enter a
#               first union.
#   step at 0   women with both events in the same month, plus, in the
#               all-women version, women with neither event by age A.
#
# TWO VERSIONS, chosen by MIRROR_AT_LEAST_ONE. In both, the first event is
# counted only if it occurs before age A = HORIZON_AGE; a woman whose first
# event comes later is treated as having neither event by A.
#
#   TRUE   Billari's population: women with a first union or a first birth
#          before age A. P(union first) and P(birth first) are shares of
#          that population, and the y axis is a proportion of it.
#   FALSE  all women. The step at 0 also includes the women with neither
#          event by A, and the y axis is a proportion of all women.
#
# ESTIMATION. The probabilities of union first, birth first, both in the same
# month and neither by age A are Aalen-Johansen cumulative incidences, with the
# interview as censoring. For TRUE they are divided by 1 - P(neither by A).
# Billari (2001) used observed counts, which is exact when every woman of the
# cohort has reached A at the interview; the Aalen-Johansen estimator gives the
# same result in that case and remains valid for cohorts in which some women
# are younger than A. That is why all women are kept in the sample even in
# the TRUE version. Only the time between the two events is an ordinary
# Kaplan-Meier curve (clock reset at the first event). A cohort is drawn only
# if some of its women are observed at age A. docs/05_methods.md, section on
# the mirrored curve, gives the construction.
#
# ANTICIPATORY SELECTION. Each half is defined by which event came first,
# which is information about the future at the woman's birth. Neither half
# estimates an effect of one event on the other (Hoem and Kreyenfeld 2006).
#
# HOW TO RUN. Open this file in RStudio and Source it, as the other figure
# scripts. Section 0 loads MEXICO_ENADID and NSFG_ENADID when they are not in
# memory. The survey selection is "union1_birth1" in lib/mexUsaFigures.R.
#
# FIGURE INDEX
#
#   block  content                               output file
#   M1     first union and first birth, cohorts  MEX_USA_mirrored_union1_birth1.pdf

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")          # also loads lib/mirroredCurve.R and lib/mexUsaFigures.R
library(survival)
library(patchwork)
library(ggrepel)

# Settings
MIRROR_AT_LEAST_ONE <- TRUE                # TRUE: women with a first event before A (Billari);
                                           # FALSE: all women
HORIZON_AGE         <- 30L                 # age A; a cohort must be observed at A to be drawn
MIRROR_COHORTS      <- decades(1940, 1990) # birth cohorts 1940-49 ... 1990-99
MIRROR_BOOTSTRAP    <- 0L                  # 0: analytic intervals; 500 or more for the paper (slow)
MIRROR_TRUNCATE     <- 10                  # hide a tail with fewer unweighted events than this


# ==== 0. Data ====

if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
MEX <- filterDateQuality(MEXICO_ENADID)
USA <- filterDateQuality(NSFG_ENADID)

sampleUnionBirth <- function (df, ageMax = HORIZON_AGE) {
  #==> df: MEXICO_ENADID or NSFG_ENADID, after selectSurveys()
  #==> ageMax: a woman whose first event, union or birth, came at age ageMax
  #    or later is counted as having neither event by that age, so that the
  #    groups (union first, birth first, same month, neither) are all defined
  #    at the same age. Every woman is kept: those interviewed before ageMax
  #    are needed as censored observations.
  #<== one row per woman: her birth (entry), first union, first birth and
  #    interview (censoring), all CMC, with her birth year and the weights
  out <- data.frame(survey      = df$survey,
                    yBirth      = df$yBirth,
                    cmc_birth   = df$indiv_dob_cmc,
                    cmc_union1  = df$union_start_cmc1,
                    cmc_birth1  = df$dob_cmc1,
                    cmc_survey  = df$surveyDate_cmc,
                    weight      = df$weight,
                    popWeight   = df$popWeight,
                    stringsAsFactors = FALSE)
  out <- subset(out, !is.na(cmc_birth) & !is.na(cmc_survey) & !is.na(yBirth))
  firstEvent <- pmin(out$cmc_union1, out$cmc_birth1, na.rm = TRUE)
  late <- !is.na(firstEvent) & ((firstEvent - out$cmc_birth) >= ageMax * 12)
  out$cmc_union1[late] <- NA
  out$cmc_birth1[late] <- NA
  cat("sampleUnionBirth: ", nrow(out), " women; ", sum(late),
      " with a first event at age ", ageMax, " or later counted as having neither\n", sep = "")
  out
}

mirror_MEX <- sampleUnionBirth(selectSurveys(MEX, "union1_birth1", "MEXICO"))
mirror_USA <- sampleUnionBirth(selectSurveys(USA, "union1_birth1", "USA"))


# ==== M1. First union and first birth, by birth cohort ====

mirrorCohorts <- function (d) {
  #<== the cohorts of MIRROR_COHORTS with some women observed at HORIZON_AGE
  unlist(lapply(seq(1, length(MIRROR_COHORTS), by = 2), function (i) {
    dc <- subset(d, (yBirth >= MIRROR_COHORTS[i]) & (yBirth <= MIRROR_COHORTS[i + 1]))
    oldest <- if (nrow(dc) > 0) max(dc$cmc_survey - dc$cmc_birth) / 12 else 0
    if (oldest >= HORIZON_AGE) MIRROR_COHORTS[i:(i + 1)] else NULL
  }))
}

if (isTRUE(MIRROR_AT_LEAST_ONE)) {
  MIRROR_Y_TITLE <- paste0("Proportion of women with a first union or a first birth before ",
                           HORIZON_AGE)
  MIRROR_CAPTION <- paste0(MEX_USA_CAPTION, ". Women with a first union or a first birth before age ",
                           HORIZON_AGE, ". The step at 0 is the share with both in the same month.")
} else {
  MIRROR_Y_TITLE <- "Proportion of women"
  MIRROR_CAPTION <- paste0(MEX_USA_CAPTION, ". All women; first events counted up to age ",
                           HORIZON_AGE, ". The step at 0 is the share with neither event by that age",
                           " or both in the same month.")
}

mirroredPanel <- function (d) {
  #<== one country's panel. KaplanMeierDraw() labels each curve at its right
  #    end; the labels of the six cohorts pile up near 0 here, so they are
  #    replaced by a legend, shared by the two panels.
  p <- mirroredCurvePlot(df = d, varEnter = "cmc_birth", varEvent = "cmc_union1",
                    varEvent2 = "cmc_birth1", varCens = "cmc_survey",
                    varWeight = "popWeight", var_yBirth = "yBirth",
                    cohortsList = mirrorCohorts(d),
                    horizon = HORIZON_AGE * 12, conditional = MIRROR_AT_LEAST_ONE,
                    truncate = MIRROR_TRUNCATE, bootstrap = MIRROR_BOOTSTRAP,
                    minX = -10, maxX = 15, confInt = TRUE,
                    xTitle = "Years since first birth (left) and since first union (right)",
                    yTitle = MIRROR_Y_TITLE)
  p$layers <- p$layers[!vapply(p$layers, function (l) inherits(l$geom, "GeomTextRepel"), logical(1))]
  p
}

fig_M1_MEX <- mirroredPanel(mirror_MEX)
fig_M1_USA <- mirroredPanel(mirror_USA)
fig_M1 <- combineCountries(fig_M1_MEX, fig_M1_USA, caption = MIRROR_CAPTION) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
fig_M1
saveFigure(fig_M1, "MEX_USA_mirrored_union1_birth1.pdf")


# ==== M2. Branch probabilities, for the text ====
#
# By country and cohort: the probabilities of union first, birth first, both
# in the same month and neither by age A, for all women, and the shares of
# union first, birth first and same month among the women with an event
# before A (Billari's nS/n, nF/n and nFS/n). The latter are the heights of
# the two halves at 0 in the TRUE version.

branchTable <- function (d, country) {
  rows <- lapply(seq(1, length(MIRROR_COHORTS), by = 2), function (i) {
    from <- MIRROR_COHORTS[i]
    to   <- MIRROR_COHORTS[i + 1]
    dc <- subset(d, (yBirth >= from) & (yBirth <= to))
    oldest <- if (nrow(dc) > 0) floor(max(dc$cmc_survey - dc$cmc_birth) / 12) else 0
    if ((nrow(dc) <= 200) || (oldest < HORIZON_AGE)) return (NULL)
    r <- suppressMessages(mirroredCurve(dc, "cmc_birth", "cmc_union1", "cmc_survey",
                                        "popWeight", "cmc_birth1", truncate = NULL,
                                        horizon = HORIZON_AGE * 12))
    pU <- attr(r, "propEventBeforeEvent2")
    pB <- attr(r, "propEvent2BeforeEvent")
    pS <- attr(r, "propSimultaneous")
    p0 <- attr(r, "propNeither")
    data.frame(country = country, cohort = paste0(from, "-", to), n = nrow(dc),
               oldestAge = oldest,
               all_unionFirst = round(pU, 3), all_birthFirst = round(pB, 3),
               all_sameMonth  = round(pS, 3), all_neither    = round(p0, 3),
               atLeastOne_unionFirst = round(pU / (1 - p0), 3),
               atLeastOne_birthFirst = round(pB / (1 - p0), 3),
               atLeastOne_sameMonth  = round(pS / (1 - p0), 3))
  })
  do.call(rbind, rows)
}

mirror_branches <- rbind(branchTable(mirror_MEX, "MEXICO"), branchTable(mirror_USA, "USA"))
print(mirror_branches)
# <<< Claude 2026-09-25
