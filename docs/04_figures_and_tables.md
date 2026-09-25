# 4. Where each figure and table comes from

This page maps every figure and table of the paper *Union Dynamics and the
Union Context of Early Childhood in Mexico and the United States: A
Half-Century Comparison*, and every result slide of the September 2026
presentation, to the script and the block that produce it and to the file it
writes.

All scripts need `MEXICO_ENADID` and `NSFG_ENADID` (see
`docs/01_build_data.md`). Figures are written to `outputPath`, which is
`output/` beside `R/` unless `config_local.R` says otherwise. The figure
scripts load the two data frames themselves when they are not in memory.


## 4.1 The figure scripts

| Script | What it draws | Estimators |
|---|---|---|
| `MEX_USA_figures_cohort.R` | Mexico and the USA side by side, by union cohort | Kaplan-Meier (net) and Aalen-Johansen (crude) |
| `MEX_USA_figures_period.R` | Mexico and the USA overlaid, by calendar year | period life tables, `ppr_doIt()` |
| `ENADID fertility.R` | total fertility; the union setting of firstborn children | fertility rates; multistate life tables for children |
| `MEX_USA_lifecourse_AJ.R` | women from age 15 to 45, from single to the end of the first union | Aalen-Johansen, age scale |
| `presentation_KM_AJ.R` | Mexico only, Kaplan-Meier against Aalen-Johansen | both, for the methods slides |
| `KaplanMeier_compareSurveys.R` | Mexico, the same curve from each survey | Kaplan-Meier |
| `KaplanMeier.R` | the per-country curves behind the cohort figures, and further analyses (Cox model, standardised survival) | Kaplan-Meier, Aalen-Johansen, Cox |

The survey selection and the sample of every Mexico-USA figure are defined
once, in `lib/mexUsaFigures.R` (`SURVEY_SELECTION`, `sampleUnionMarriage()`,
`sampleUnionSeparation()`, `sampleRepartnering()`, `buildBothEpisodes()`).


## 4.2 Paper: figures

| Figure | Title (short) | Script, block | Output file | Measure |
|---|---|---|---|---|
| 1 | Transition of first unions to marriage, by union cohort | `MEX_USA_figures_cohort.R`, C1 | `MEX_USA_Union1_marriage.pdf` | net, Kaplan-Meier |
| 2 | Transition of second unions to marriage | `MEX_USA_figures_cohort.R`, C2 | `MEX_USA_Union2_marriage.pdf` | net, Kaplan-Meier |
| 3 | Separation of first unions | `MEX_USA_figures_cohort.R`, C3 | `MEX_USA_Union_sep1.pdf` | net (widowhood censored) |
| 4 | Separation of second unions | `MEX_USA_figures_cohort.R`, C4 | `MEX_USA_Union_sep2.pdf` | net (widowhood censored) |
| 5 | Period probability of separation of a first union, before 40 | `MEX_USA_figures_period.R`, P1 | `MEX_USA_period_sep1_40.pdf` | net, period life table |
| 6 | Separation of cohabitation, marriage censored | **retired**: `archive/KaplanMeier_marriage_censored.R`. Replacement: `MEX_USA_figures_cohort.R`, C6 | `MEX_USA_union1_cohab_separate_AJ.pdf` (and `_remain_`, `_convert_`) | crude, Aalen-Johansen |
| 7 | First re-partnering | `MEX_USA_figures_cohort.R`, C5 | `MEX_USA_sep1_union2.pdf` | net, Kaplan-Meier |
| 8 | How firstborn children spend their first ten years | `ENADID fertility.R`, "status of first birth / combining Mexico and the USA"; corrected version in "Figures 8 and 9 of the paper, corrected" | `MEX_USA_lifeChildren_plot.pdf`; corrected `MEX_USA_lifeChildren_plot_corrected.pdf` | crude, multistate life table |
| 9 | Firstborn children who spent their first ten years in the mother's first union | `ENADID fertility.R`, "Children born in mother's FIRST union ... / Mexico and USA"; corrected version as for Figure 8 | `MEX_USA_childIntact.pdf`; corrected `MEX_USA_childIntact_corrected.pdf` | crude |
| 10 | Maximum age of women in each calendar year | `MEX_USA_figures_period.R`, P4 | `MEX_USA_maxAge.pdf` | |
| 11 | TFR, Mexican surveys, pooled and UN series | `ENADID fertility.R`, "Total fertility / Mexico" | `MEX_TFR_bySurvey.pdf` | |
| 12 | TFR of first births, Mexico | same block | `MEX_TFR1_bySurvey.pdf` | |
| 13 | TFR, US surveys | `ENADID fertility.R`, "Total fertility / USA" | `USA_TFR_bySurvey.pdf` | |
| 14 | TFR of first births, USA | same block | `USA_TFR1_bySurvey.pdf` | |
| 15 | Period probability of separation, each Mexican survey | `MEX_USA_figures_period.R`, P3 | `MEX_period_sep1_bySurvey.pdf` | net |
| 16 | Period probability of separation, each US survey | `MEX_USA_figures_period.R`, P3 | `USA_period_sep1_bySurvey.pdf` | net |
| 17 | Period probability of separation before 45 | `MEX_USA_figures_period.R`, P2 | `MEX_USA_period_sep1_45.pdf` | net |
| 18 | EDER 2025 against the neighbouring Mexican surveys | `KaplanMeier_compareSurveys.R` | `MEX_union1_marriage_bySurvey.pdf` | net |
| 19 | Mean age at first birth | `MEX_USA_figures_period.R`, P5 | `MEX_USA_mean_age_birth1.pdf` | period life table |
| Annex | Lexis diagram of the selection by the survey age limit | `ENADID fertility.R`, "Annex figure: Lexis diagram" | `FigureA_Lexis_selection.pdf` | |

Every figure of the paper is now written to a file by `saveFigure()` (in
`lib/mexUsaFigures.R`). Use the same function for any new figure.

**Changes to note when the figures are rebuilt.** Figure 19 was computed from
a data frame that, by an error in the old code, included NSFG 1973 and 1976,
whose samples leave out childless single women; block P5 excludes them, as the
US-only version always did. The y-axis of Figure 7 now reads "Proportion who
have re-partnered" instead of "Proportion of first separation". The curves of
Figures 1 to 7 are otherwise unchanged: they were checked to be numerically
identical to those of the previous code.


## 4.3 Paper: tables

| Table | Content | Source |
|---|---|---|
| 1, 2 | Characteristics of the Mexican and US surveys | written by hand from the survey documentation; the numbers of women are `table(MEXICO_ENADID$survey)` and `table(NSFG_ENADID$survey)` |
| 3 | Separation dates for first marriages, NSFG 2002, by whether the husband had children | `NSFG document dissolution problems 2002.R` |
| 4 | Log-logistic AFT model of union duration, NSFG 2006-10 | `NSFG_impute_dissolution_model.R` (`fit_dissolution_model()`) |
| 5 | Quality of the end-of-union dates, NSFG | `checkFlag.R`: `checkFlag(NSFG_ENADID, "union_end_cmc_I")`, the `_I` flag by survey |
| 6 | Quality of the first end-of-union date, Mexico | `checkFlag.R` on `MEXICO_ENADID`, first union |


## 4.4 Presentation of September 2026 (UEAH, Pachuca)

Slide numbers are those of the restyled deck.

| Slide | Content | Source |
|---|---|---|
| 4 | Surveys used | Tables 1 and 2 |
| 5 | Pooling: TFR from each survey | Figures 11 and 13 |
| 7 | State diagrams | drawn with Mermaid, not produced by R |
| 8 | Kaplan-Meier against Aalen-Johansen, Mexico | `presentation_KM_AJ.R` |
| 10 to 15 | Figures 1, 2, 3, 4, 5 and 7 | as in section 4.2 |
| 16 | Women from age 15 to 45, first union | `MEX_USA_lifecourse_AJ.R` → `MEX_USA_lifecourse_AJ.pdf` |
| 17 | Conversion of cohabitations into marriage | `MEX_USA_figures_cohort.R`, C6 → `MEX_USA_union1_cohab_convert_AJ.pdf` |
| 19, 20 | Figures 8 and 9 | as in section 4.2 |


## 4.5 New period transitions

Block P6 of `MEX_USA_figures_period.R` contains a table of further period
transitions: entry into the first union, entry into a second union after a
separation, and separation of the second union. Each row names the function
that builds the sample, the survey selection, the age limit and the years to
keep, and is run only when its `run` field is `TRUE`. These three rows run on
the pooled data, but their settings have not been reviewed yet. The steps to
add a transition are written in the block itself.
