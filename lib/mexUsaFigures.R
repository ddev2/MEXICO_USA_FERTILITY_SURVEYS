# >>> Claude 2026-09-25
# lib/mexUsaFigures.R
#
# Building blocks shared by the Mexico-USA figure scripts:
#
#   MEX_USA_figures_cohort.R   Kaplan-Meier and Aalen-Johansen figures by union cohort
#   MEX_USA_figures_period.R   period indicators computed with ppr_doIt()
#   KaplanMeier.R              the country-by-country analysis behind them
#
# It keeps, in one place, the choices that used to be repeated in every block
# of KaplanMeier.R: which surveys enter each analysis, how each analysis sample
# is built from MEXICO_ENADID or NSFG_ENADID, which union cohorts are drawn, and
# how the Mexico and USA panels are put side by side. Change a survey selection
# here and every figure that uses it follows.
#
# Loaded by enadid_lib.R. Needs survival, ggplot2 and patchwork.


# ==== 1. Survey selections ====
#
# One entry per analysis and country. 'keep' lists the surveys used; when it
# is NULL, every survey is used except those in 'drop'. The reasons are the
# coverage of each questionnaire, documented in docs/02_common_format.md:
#
#   WFS 1976          no cohabitation before marriage
#   ENADID 1992       no union history
#   ENADID 2006       too many missing dates of first union
#   EDER 2025         marriage dates not usable for the union -> marriage transition
#   ENADID 2009-2023  first and last union only, so no second union
#   NSFG 1973, 1976   ever-married women or mothers only, no cohabitation
#   NSFG 1982         no cohabitation before marriage
#   NSFG 1988         first union only
#   NSFG 2017-19      incomplete union history
#   NSFG 2022-23      first union only

SURVEY_SELECTION <- list(
  union1_marriage = list(
    MEXICO = list(keep = NULL, drop = c("WFS", "ENADID1992", "ENADID2006", "EDER2025")),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG2017_19"))),
  union2_marriage = list(
    MEXICO = list(keep = c("ENADID1997", "EDER2017"), drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988",
                                        "NSFG2017_19", "NSFG2022_23"))),
  union1_separation = list(
    MEXICO = list(keep = NULL, drop = c("ENADID1992", "ENADID2006")),
    USA    = list(keep = NULL, drop = c("NSFG2017_19"))),
  union2_separation = list(
    MEXICO = list(keep = c("ENADID1997", "EDER2017", "EDER2025"), drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1988", "NSFG2017_19", "NSFG2022_23"))),
  repartnering = list(
    MEXICO = list(keep = c("WFS", "ENADID1997", "EDER2017"), drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988",
                                        "NSFG2017_19", "NSFG2022_23"))),
  # the union episodes behind every Aalen-Johansen figure
  union1_episodes = list(
    MEXICO = list(keep = NULL, drop = c("WFS", "ENADID1992", "ENADID2006")),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG2017_19"))),
  # period separation of the first union, ppr_doIt(). The USA drops 2022-23 as
  # well in the pooled comparison, because its union history stops at the
  # first union and its last years are still incomplete.
  union1_separation_period = list(
    MEXICO = list(keep = NULL, drop = c("ENADID1992", "ENADID2006")),
    USA    = list(keep = NULL, drop = c("NSFG2017_19", "NSFG2022_23"))),
  # the same, one survey at a time (Figures 15 and 16)
  union1_separation_bySurvey = list(
    MEXICO = list(keep = NULL, drop = c("ENADID1992", "ENADID2006")),
    USA    = list(keep = NULL, drop = c("NSFG2017_19"))),
  # birth of the woman -> first birth. NSFG 1973 and 1976 interviewed only
  # ever-married women and mothers, so childless single women are missing.
  birth_birth1 = list(
    MEXICO = list(keep = NULL, drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976"))),
  # NEW TRANSITIONS, period analysis. Check these selections before use.
  # birth of the woman -> first union: every survey with a first-union date
  union1_formation = list(
    MEXICO = list(keep = NULL, drop = c("ENADID1992", "ENADID2006")),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976"))),
  # separation of the first union -> second union: complete union histories only
  sep1_union2 = list(
    MEXICO = list(keep = c("WFS", "ENADID1997", "EDER2017", "EDER2025"), drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988",
                                        "NSFG2017_19", "NSFG2022_23"))),
  # second union -> separation
  union2_separation_period = list(
    MEXICO = list(keep = c("ENADID1997", "EDER2017", "EDER2025"), drop = NULL),
    USA    = list(keep = NULL, drop = c("NSFG1988", "NSFG2017_19", "NSFG2022_23")))
)

selectSurveys <- function (df, analysis, country) {
  #==> df: MEXICO_ENADID or NSFG_ENADID
  #==> analysis: a name in SURVEY_SELECTION
  #==> country: "MEXICO" or "USA"
  #<== the rows of df from the surveys that analysis uses
  sel <- SURVEY_SELECTION[[analysis]][[country]]
  if (is.null(sel)) stop("selectSurveys: no selection for '", analysis, "' / '", country, "'")
  if (!is.null(sel$keep)) {
    out <- df[df$survey %in% sel$keep, ]
  } else {
    out <- df[!(df$survey %in% sel$drop), ]
  }
  out
}


# ==== 2. Union cohorts drawn in each figure ====
#
# cohortsList as KaplanMeierPlot() expects it: a flat vector of (first, last)
# year pairs.

decades <- function (from, to) as.vector(rbind(seq(from, to, 10), seq(from, to, 10) + 9))

COHORTS <- list(
  MEXICO             = decades(1960, 2010),               # 1960-69 ... 2010-19
  USA_marriage       = decades(1960, 2010),
  USA_separation     = c(1945, 1949, decades(1950, 2010)),
  USA_repartnering   = decades(1970, 2010)
)


# ==== 3. Analysis samples ====
#
# Each function returns the columns KaplanMeierPlot() needs, with the entry
# date, the event date and the censoring date named as in the figure scripts.
# All dates are CMC.

sampleUnionMarriage <- function (df, u = 1) {
  #==> u: union order (1 or 2)
  #<== yUnion<u> (union cohort) and varUnionCens<u>: the union ends or the
  #    survey is reached, whichever comes first. Separation and widowhood are
  #    therefore CENSORING here: the curve is a NET measure.
  s <- function (v) paste0(v, u)
  out <- df[, c("country", "survey", "surveyDate_cmc",
                s("union_start_cmc"), s("union_end_cmc"), s("union_end_motive"),
                s("marriage_start_cmc"), "yBirth", "weight", "popWeight")]
  out[[s("yUnion")]]       <- yearFrom_cmc(out[[s("union_start_cmc")]])
  out[[s("varUnionCens")]] <- ifelse(!is.na(out[[s("union_end_cmc")]]),
                                     out[[s("union_end_cmc")]], out$surveyDate_cmc)
  out
}

sampleUnionSeparation <- function (df, u = 1) {
  #==> u: union order (1 or 2)
  #<== yUnion<u>, union_endBySep_cmc<u> (the end date when the union ended by
  #    separation, NA otherwise) and varSepCens<u> (the widowhood date for a
  #    widow, the survey date for everyone else). Widowhood is CENSORING: the
  #    curve is the NET probability of separating, male mortality removed.
  s <- function (v) paste0(v, u)
  out <- df[, c("country", "survey", "surveyDate_cmc",
                s("union_start_cmc"), s("union_end_cmc"), s("union_end_motive"),
                s("marriage_start_cmc"), "yBirth", "weight", "popWeight")]
  out[[s("yUnion")]] <- yearFrom_cmc(out[[s("union_start_cmc")]])
  widow <- !is.na(out[[s("union_end_motive")]]) & (out[[s("union_end_motive")]] == "widowhood")
  out[[s("varSepCens")]]         <- ifelse(widow, out[[s("union_end_cmc")]], out$surveyDate_cmc)
  out[[s("union_endBySep_cmc")]] <- ifelse(widow, NA, out[[s("union_end_cmc")]])
  out
}

sampleRepartnering <- function (df) {
  #<== women whose first union ended, from the end of the first union to the
  #    start of the second. ySep1 is the cohort of the first union's end.
  #    Unions that ended by widowhood are included.
  out <- df[, c("country", "survey", "surveyDate_cmc",
                "union_end_cmc1", "union_start_cmc2", "yBirth", "weight", "popWeight")]
  out$ySep1 <- yearFrom_cmc(out$union_end_cmc1)
  out
}


# ==== 3b. Period samples for ppr_doIt() ====
#
# ppr_doIt() works on YEARS: the year the woman became at risk (varEnter), the
# year of the event (varEvent), and the survey date (cmc_survey) as the
# censoring date. ageEvent is the age at the event, which ageTruncate uses.
# createUnionSep() and createBirthBirths() in enadid_lib.R build the two
# transitions already in the paper. The two functions below build new ones in
# the same layout.

createBirthUnion <- function (df, u = 1) {
  #==> u: union order counted from birth, normally 1
  #<== every woman, at risk of her first union from birth: yBirth (entry),
  #    yUnion (event, NA if never in union), ageEvent. Use with duration = FALSE,
  #    as for the first birth, so the life table runs by age.
  vStart <- paste0("union_start_cmc", u)
  out <- data.frame(surveyName = df$survey, country = df$country,
                    cmc_birth = df$indiv_dob_cmc, yBirth = df$yBirth,
                    ageSurvey = df$indiv_age_survey, cmc_survey = df$surveyDate_cmc,
                    cmc_union = df[[vStart]], weight = df$weight, popWeight = df$popWeight,
                    stringsAsFactors = FALSE)
  n0 <- nrow(out)
  out <- subset(out, !is.na(cmc_birth) & !is.na(cmc_survey) & !is.na(yBirth) & (yBirth < 2100))
  bad <- !is.na(out$cmc_union) & ((out$cmc_union > out$cmc_survey) | (out$cmc_union < out$cmc_birth))
  out <- out[!bad, ]
  out$yUnion   <- yearFrom_cmc(out$cmc_union)
  out$ageEvent <- (out$cmc_union - out$cmc_birth) / 12
  out$surveyName <- factor(out$surveyName)
  cat("createBirthUnion: ", nrow(out), " women kept of ", n0, ", ",
      sum(!is.na(out$yUnion)), " first unions\n", sep = "")
  out
}

createSepUnion <- function (df, motiveMap = NULL) {
  #<== women whose FIRST union ended by separation, at risk of a second union
  #    from the end of the first: ySep (entry), yUnion2 (event), ageEvent.
  #    Endings with no usable motive count as separations, as in
  #    createUnionSep(); widows are not at risk here. Use with duration = TRUE.
  if (is.null(motiveMap)) motiveMap <- UNION_MOTIVE_MAP
  out <- data.frame(surveyName = df$survey, country = df$country,
                    cmc_birth = df$indiv_dob_cmc, yBirth = df$yBirth,
                    cmc_survey = df$surveyDate_cmc,
                    cmc_end1 = df$union_end_cmc1, cmc_union2 = df$union_start_cmc2,
                    motive1 = tolower(trimws(as.character(df$union_end_motive1))),
                    weight = df$weight, popWeight = df$popWeight, stringsAsFactors = FALSE)
  n0 <- nrow(out)
  what <- unname(motiveMap[out$motive1])
  what[is.na(what)] <- "unknown"
  out <- out[!is.na(out$cmc_end1) & (what %in% c("separation", "unknown")), ]
  out <- subset(out, !is.na(cmc_survey) & (cmc_end1 <= cmc_survey))
  bad <- !is.na(out$cmc_union2) & ((out$cmc_union2 < out$cmc_end1) | (out$cmc_union2 > out$cmc_survey))
  out <- out[!bad, ]
  out$ySep     <- yearFrom_cmc(out$cmc_end1)
  out$yUnion2  <- yearFrom_cmc(out$cmc_union2)
  out$ageEvent <- (out$cmc_union2 - out$cmc_birth) / 12
  out$surveyName <- factor(out$surveyName)
  out$motive1  <- NULL
  cat("createSepUnion: ", nrow(out), " separated women kept of ", n0, ", ",
      sum(!is.na(out$yUnion2)), " second unions\n", sep = "")
  out
}


# ==== 4. Mexico and USA side by side ====

MEX_USA_CAPTION <- "Source: INEGI/ENADID-EDER and CDC/NSFG microdata"

combineCountries <- function (plotMEX, plotUSA, caption = MEX_USA_CAPTION) {
  #==> plotMEX, plotUSA: two ggplot objects, one per country
  #<== one patchwork figure, the two panels titled MEXICO and USA, with the
  #    font sizes used in the paper
  ((plotMEX + ggplot2::labs(title = "MEXICO")) +
     (plotUSA + ggplot2::labs(title = "USA"))) +
    patchwork::plot_annotation(title = NULL, caption = caption) &
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = 20, hjust = 0.5),
      axis.title   = ggplot2::element_text(size = 14),
      axis.text    = ggplot2::element_text(size = 12),
      legend.text  = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13),
      plot.caption = ggplot2::element_text(size = 10, face = "italic"))
}

saveFigure <- function (plot, fileName, width = 29.7, height = 21) {
  #==> fileName: written to outputPath, the folder set in enadid_lib.R or
  #    config_local.R. A4 landscape by default, as in the paper.
  pathFile <- file.path(outputPath, fileName)
  ggplot2::ggsave(filename = pathFile, plot = plot, width = width, height = height,
                  units = "cm", dpi = 300)
  message("saved ", pathFile)
  invisible(pathFile)
}


# ==== 5. Aalen-Johansen figures: union episodes and plotting helpers ====

COHORT_BREAKS <- c(-Inf, 1949, 1959, 1969, 1979, 1989, 1999, 2009, Inf)
COHORT_LABELS <- c("<1950", "1950-59", "1960-69", "1970-79",
                   "1980-89", "1990-99", "2000-09", "2010+")

buildBothEpisodes <- function (dfMEX, dfUSA, minUnions = 200, quiet = FALSE) {
  #==> dfMEX, dfUSA: MEXICO_ENADID and NSFG_ENADID (after filterDateQuality)
  #==> minUnions: a (country, cohort) cell with fewer COHABITING first unions
  #    than this is dropped rather than drawn as a noisy curve
  #<== list(MEX, USA, BOTH): first-union episodes from buildUnionEpisodes(),
  #    popWeight, cohort added, ids distinct across the two countries
  mex <- selectSurveys(dfMEX, "union1_episodes", "MEXICO")
  usa <- selectSurveys(dfUSA, "union1_episodes", "USA")
  mex$survey <- factor(mex$survey); usa$survey <- factor(usa$survey)
  MEX <- buildUnionEpisodes(mex, u = 1, varWeight = "popWeight", quiet = quiet)
  USA <- buildUnionEpisodes(usa, u = 1, varWeight = "popWeight", quiet = quiet)
  MEX$cohort <- cut(MEX$yUnion, breaks = COHORT_BREAKS, labels = COHORT_LABELS)
  USA$cohort <- cut(USA$yUnion, breaks = COHORT_BREAKS, labels = COHORT_LABELS)
  # survfit() with id = needs every id in one stratum only
  USA$id <- USA$id + max(MEX$id)

  BOTH <- rbind(MEX, USA)
  BOTH <- BOTH[!is.na(BOTH$cohort), ]
  cohabFirst <- subset(BOTH, (tstart == 0) & (istate == "cohabiting"))
  cellN <- as.data.frame(table(country = cohabFirst$country, cohort = cohabFirst$cohort))
  names(cellN)[3] <- "nUnions"
  if (!quiet) print(cellN)
  keepCell <- subset(cellN, nUnions >= minUnions)[, c("country", "cohort")]
  BOTH <- merge(BOTH, keepCell, by = c("country", "cohort"))
  BOTH <- BOTH[order(BOTH$id, BOTH$tstart), ]
  list(MEX = MEX, USA = USA, BOTH = BOTH, cellN = cellN)
}

occupancyPlot <- function (occ, Title, facets = ~ group) {
  #==> occ: output of unionStateOccupancy()
  #<== stacked areas, one band per state; the bands sum to 1 at every duration
  ggplot2::ggplot(occ, ggplot2::aes(x = timeYear, y = p, fill = state)) +
    ggplot2::geom_area() + ggplot2::facet_grid(facets) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = Title, x = "Duration in years after the union began",
                  y = "Share of the cohort", fill = "State") +
    ggplot2::theme_linedraw() + ggplot2::theme(legend.position = "bottom")
}

asKMtable <- function (d, valueVar = "p", classVar = NULL) {
  # Renames Aalen-Johansen curves into the layout KaplanMeierDraw() expects,
  # time back in MONTHS and the cohort an ordered factor, so these figures get
  # the same colour ramp, ribbons and facets as the Kaplan-Meier ones.
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
  # a bound survfit could not compute falls back on the estimate, so the ribbon closes
  out$confIntMin <- ifelse(is.na(out$confIntMin), out$survFunction, out$confIntMin)
  out$confIntMax <- ifelse(is.na(out$confIntMax), out$survFunction, out$confIntMax)
  out[order(out$class, out$country, out$cohort, out$time), ]
}

cohortPlot <- function (d, Title, yTitle, anchorAtOne = FALSE, classVar = NULL) {
  #==> d: Aalen-Johansen curves with timeYear, p, lower, upper, country, cohort
  if (nrow(d) == 0) stop("cohortPlot: nothing to plot")
  KaplanMeierDraw(asKMtable(d, classVar = classVar),
                  Title = Title, yTitle = yTitle,
                  xTitle = "Duration in years after the union began",
                  minX = 0, maxX = 25, confInt = TRUE, hideLegend = TRUE,
                  anchorAtOne = anchorAtOne, legendTitle = "Union cohort")
}

mergeSeparations <- function (ep) {
  # The two separation states are merged BEFORE the fit rather than added after
  # it: survfit then returns an interval for the sum, which adding two
  # cumulative incidences would not give.
  lv <- levels(ep$to)
  lv[lv %in% c("separation (cohabiting)", "separation (married)")] <- "separated"
  levels(ep$to) <- lv
  ep
}
# <<< Claude 2026-09-25
