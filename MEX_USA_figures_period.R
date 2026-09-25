# >>> Claude 2026-09-25
# MEX_USA_figures_period.R
#
# The Mexico-USA figures BY CALENDAR PERIOD. They use ppr_doIt(), through
# calc_ppr() and plotBySurvey() in lib/KaplanMeierLib.R: a life table built
# from the events and the exposure of each calendar year, the same machinery
# as a period parity progression ratio. For a transition A -> B it gives, for
# each year, the probability that a woman who entered state A would reach B
# if she experienced that year's rates throughout. Competing events (widowhood
# for separation) are censoring, so these are NET probabilities.
#
# HOW TO RUN. Build MEXICO_ENADID and NSFG_ENADID first (docs/01_build_data.md),
# open this file in RStudio and Source it. Section 0 must run first; each
# block after it is independent.
#
# FIGURE INDEX
#
#   block  paper / slide              output file
#   P1     Figure 5, slide 14         MEX_USA_period_sep1_40.pdf
#   P2     Figure 17                  MEX_USA_period_sep1_45.pdf
#   P3     Figures 15 and 16          MEX_period_sep1_bySurvey.pdf, USA_period_sep1_bySurvey.pdf
#   P4     Figure 10                  MEX_USA_maxAge.pdf
#   P5     Figure 19                  MEX_USA_mean_age_birth1.pdf (and the PPR of the first birth)
#   P6     NEW TRANSITIONS            one file per transition, see the table in P6
#
# Figures 11 to 14 (TFR) come from "ENADID fertility.R", which also has
# Figures 8 and 9 on children.
#
# BOOTSTRAP. calc_ppr() draws 200 bootstrap replicates by default for the
# confidence band, which takes a few minutes per country. Set PPR_REPLICATES
# to 0 while you are trying things out; the band then falls back on the
# Greenwood variance.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")          # also loads lib/mexUsaFigures.R
library(ggrepel)

PPR_REPLICATES <- 200L
RUN_CHECKS     <- FALSE         # P7, the bootstrap check on one survey

# the font sizes used in the paper
paperTheme <- theme(
  plot.title   = element_text(size = 20, hjust = 0.5),
  axis.title   = element_text(size = 14),
  axis.text    = element_text(size = 12),
  legend.text  = element_text(size = 12),
  legend.title = element_text(size = 13),
  plot.caption = element_text(size = 10, face = "italic"))


# ==== 0. Data ====

if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
MEX <- filterDateQuality(MEXICO_ENADID)
USA <- filterDateQuality(NSFG_ENADID)


# ==== P1. Period probability of separation of a first union, before age 40 (Figure 5) ====
#
# Separations after age 40 are not counted (ageTruncate), because most NSFG
# rounds stop at age 44 and the older ages would be seen only in the early
# years. The years kept are those with enough surveys behind them.

union1_sep1 <- rbind(
  createUnionSep(selectSurveys(MEX, "union1_separation_period", "MEXICO")),
  createUnionSep(selectSurveys(USA, "union1_separation_period", "USA")))

res_Union1Sep1 <- calc_ppr(df = union1_sep1,
                           varEnter = "yUnion", varEvent = "ySep", varCens = "cmc_survey",
                           varCountry = "country", vecCountry = NULL, varWeight = "weight",
                           res_numYears = 68, ageTruncate = 40, mySpan = 0.25,
                           replicates = PPR_REPLICATES)
res_Union1Sep1 <- subset(res_Union1Sep1,
                         ((country == "MEXICO") & (year >= 1965) & (year <= 2022)) |
                         ((country == "USA")    & (year >= 1968) & (year <= 2016)))

plot_sep1 <- plot_ppr(df_res = res_Union1Sep1, vecCountry = NULL, yLimit = c(0, 1),
                      yTitle = "probability of separation", facet = FALSE) +
  paperTheme + scale_x_continuous(breaks = seq(1960, 2020, by = 10))
plot_sep1
saveFigure(plot_sep1, "MEX_USA_period_sep1_40.pdf")

# the mean duration of the union at separation, from the same life table
plot_mean_sep1 <- plot_mean(df_res = res_Union1Sep1, vecCountry = NULL, yLimit = c(0, 15),
                            yTitle = "mean duration of union until separation", facet = FALSE)
plot_mean_sep1


# ==== P2. The same before age 45, for the recent period (Figure 17) ====
#
# Drawn over P1 so the effect of the age limit on the level can be judged.

res_Union1Sep1_45 <- calc_ppr(df = union1_sep1,
                              varEnter = "yUnion", varEvent = "ySep", varCens = "cmc_survey",
                              varCountry = "country", vecCountry = NULL, varWeight = "weight",
                              res_numYears = 20, ageTruncate = 45, mySpan = 0.25,
                              replicates = PPR_REPLICATES)
res_Union1Sep1_45 <- subset(res_Union1Sep1_45,
                            ((country == "MEXICO") & (year <= 2022)) |
                            ((country == "USA")    & (year <= 2016)))
res_Union1Sep1_45$country <- ifelse(res_Union1Sep1_45$country == "MEXICO", "MEXICO 45", "USA 45")
res_Union1Sep1_45 <- rbind(res_Union1Sep1_45, res_Union1Sep1)

cols <- c("MEXICO" = "#F8766D", "MEXICO 45" = "#F8766D", "USA" = "#00BFC4", "USA 45" = "#00BFC4")
ltys <- c("MEXICO" = "solid",   "MEXICO 45" = "dotted",  "USA" = "solid",   "USA 45" = "dotted")
plot_sep1_45 <- plot_ppr(df_res = res_Union1Sep1_45, vecCountry = NULL, yLimit = c(0, 1),
                         yTitle = "probability of separation", facet = FALSE) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_linetype_manual(values = ltys) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + paperTheme
plot_sep1_45
saveFigure(plot_sep1_45, "MEX_USA_period_sep1_45.pdf")


# ==== P3. The same, estimated from each survey separately (Figures 15 and 16) ====
#
# One segment per survey. Where the segments of neighbouring surveys meet, the
# surveys agree; where they diverge, one of them misreports dates.

bySurvey <- function (country) {
  d  <- createUnionSep(selectSurveys(if (country == "MEXICO") MEX else USA,
                                     "union1_separation_bySurvey", country))
  ny <- buildSpecificYears(d, "separation1", varEvent = "ySep")
  plotBySurvey(df_toPlot = d, varEnter = "yUnion", varEvent = "ySep", varWeight = "weight",
               res_countrySpecific_numYears = ny, ageTruncate = 40, mySpan = 0.5,
               yTitle = "probability of separation")
}
plot_sep1_all_Mex <- bySurvey("MEXICO")
plot_sep1_all_USA <- bySurvey("USA")
plot_sep1_all_Mex$plot
plot_sep1_all_USA$plot
saveFigure(plot_sep1_all_Mex$plot, "MEX_period_sep1_bySurvey.pdf")
saveFigure(plot_sep1_all_USA$plot, "USA_period_sep1_bySurvey.pdf")


# ==== P4. Maximum age of women in each calendar year (Figure 10) ====
#
# Back-projection of the pooled surveys: in a given past year, the oldest woman
# present is the oldest respondent minus the years since her survey. The dashed
# line is the age limit used in P1.

rangeAge <- rbind(
  agesByYear(df = createUnionSep(selectSurveys(MEX, "union1_separation_bySurvey", "MEXICO"),
                                 quiet = TRUE), varEvent = "ySep"),
  agesByYear(df = createUnionSep(selectSurveys(USA, "union1_separation_bySurvey", "USA"),
                                 quiet = TRUE), varEvent = "ySep"))
label_data <- subset(rangeAge, year == 1990)

pRange <- ggplot(rangeAge, aes(x = year, y = ageMax, color = country)) +
  geom_line() +
  geom_text_repel(data = label_data, aes(label = country), size = 4, direction = "y") +
  theme_bw() + xlab("year") + ylab("maximum age in dataset") +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "none") + paperTheme +
  geom_hline(yintercept = 40, linetype = "dashed", color = "gray40", linewidth = 0.7)
pRange
saveFigure(pRange, "MEX_USA_maxAge.pdf")


# ==== P5. First birth: period probability and mean age (Figure 19) ====

birth_births_Mex_USA <- rbind(
  createBirthBirths(selectSurveys(MEX, "birth_birth1", "MEXICO")),
  createBirthBirths(selectSurveys(USA, "birth_birth1", "USA")))

res_BirthBirth1_Mex_USA <- calc_ppr(df = birth_births_Mex_USA,
                                    varEnter = "yBirth", varEvent = "yBirth1", varCens = "cmc_survey",
                                    varCountry = "country", vecCountry = NULL, varWeight = "weight",
                                    res_finalYearsToDiscard = 5, res_numYears = 50, mySpan = 0.25,
                                    duration = FALSE, replicates = PPR_REPLICATES)
res_BirthBirth1_Mex_USA <- subset(res_BirthBirth1_Mex_USA, year <= 2016)

plot_birth1 <- plot_ppr(df_res = res_BirthBirth1_Mex_USA, vecCountry = NULL, yLimit = c(0, 1),
                        yTitle = "probability of first birth", facet = FALSE)
plot_mean_birth1 <- plot_mean(df_res = res_BirthBirth1_Mex_USA, vecCountry = NULL,
                              yLimit = c(15, 30), yTitle = "mean age at first birth", facet = FALSE)
plot_birth1
plot_mean_birth1
saveFigure(plot_mean_birth1, "MEX_USA_mean_age_birth1.pdf")


# ==== P6. New transitions ====
#
# One row per transition. To add one:
#   1. write a function that returns one row per woman at risk, with the year
#      of entry, the year of the event (NA if none), cmc_survey, weight, and
#      ageEvent if ageTruncate is used. createUnionSep(), createBirthBirths(),
#      createBirthUnion() and createSepUnion() are the models;
#   2. add the survey selection to SURVEY_SELECTION in lib/mexUsaFigures.R;
#   3. add a row here, with run = TRUE.
# duration = TRUE builds the life table by duration since entry (a union, a
# separation); duration = FALSE builds it by age (entry at birth).
#
# The three rows below are written and run on the pooled data, but their
# survey selections, age limits and year windows have NOT been reviewed yet.
# That is why they start with run = FALSE.

PERIOD_TRANSITIONS <- list(
  union1_formation = list(
    run = FALSE, build = createBirthUnion, selection = "union1_formation",
    varEnter = "yBirth", varEvent = "yUnion", duration = FALSE, ageTruncate = NULL,
    res_numYears = 50, res_finalYearsToDiscard = 1, yTitle = "probability of a first union",
    years = list(MEXICO = c(1965, 2022), USA = c(1968, 2021)),
    file = "MEX_USA_period_union1.pdf"),
  sep1_union2 = list(
    run = FALSE, build = createSepUnion, selection = "sep1_union2",
    varEnter = "ySep", varEvent = "yUnion2", duration = TRUE, ageTruncate = 45,
    res_numYears = 40, res_finalYearsToDiscard = 1, yTitle = "probability of a second union",
    years = list(MEXICO = c(1965, 2022), USA = c(1968, 2016)),
    file = "MEX_USA_period_sep1_union2.pdf"),
  union2_separation = list(
    run = FALSE, build = function (df) createUnionSep(df, u = 2), selection = "union2_separation_period",
    varEnter = "yUnion", varEvent = "ySep", duration = TRUE, ageTruncate = 45,
    res_numYears = 40, res_finalYearsToDiscard = 1, yTitle = "probability of separation, second union",
    years = list(MEXICO = c(1965, 2022), USA = c(1968, 2016)),
    file = "MEX_USA_period_sep2.pdf")
)

runPeriodTransition <- function (tr, replicates = PPR_REPLICATES) {
  d <- rbind(tr$build(selectSurveys(MEX, tr$selection, "MEXICO")),
             tr$build(selectSurveys(USA, tr$selection, "USA")))
  res <- calc_ppr(df = d, varEnter = tr$varEnter, varEvent = tr$varEvent, varCens = "cmc_survey",
                  varCountry = "country", vecCountry = NULL, varWeight = "weight",
                  duration = tr$duration, ageTruncate = tr$ageTruncate,
                  res_numYears = tr$res_numYears, res_finalYearsToDiscard = tr$res_finalYearsToDiscard,
                  mySpan = 0.25, replicates = replicates)
  keep <- rep(FALSE, nrow(res))
  for (cn in names(tr$years)) {
    keep <- keep | ((res$country == cn) & (res$year >= tr$years[[cn]][1]) & (res$year <= tr$years[[cn]][2]))
  }
  res <- res[keep, ]
  p <- plot_ppr(df_res = res, vecCountry = NULL, yLimit = c(0, 1), yTitle = tr$yTitle,
                facet = FALSE) + paperTheme
  list(results = res, plot = p)
}

period_results <- list()
for (nm in names(PERIOD_TRANSITIONS)) {
  tr <- PERIOD_TRANSITIONS[[nm]]
  if (!isTRUE(tr$run)) next
  cat("\n==== period transition:", nm, "====\n")
  period_results[[nm]] <- runPeriodTransition(tr)
  print(period_results[[nm]]$plot)
  saveFigure(period_results[[nm]]$plot, tr$file)
}


# ==== P7. Check: bootstrap against the analytic interval, one survey ====

if (RUN_CHECKS) {
  comparison_df <- validate_with_bootstrap(subset(union1_sep1, surveyName == "NSFG2006_10"),
                                           varEnter = "yUnion", varEvent = "ySep",
                                           varCens = "cmc_survey", varWeight = "weight",
                                           n_bootstrap = 200)
  print(comparison_df)
}
# <<< Claude 2026-09-25
