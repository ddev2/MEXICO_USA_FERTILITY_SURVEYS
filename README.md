# MEXICO_USA_FERTILITY_SURVEYS

R code that reads fifty years of Mexican and United States fertility surveys,
converts them into one common format, and compares union formation,
separation, re-partnering and the union setting of children in the two
countries.

It produces two harmonised data frames, one row per woman, with her union
history and her birth history dated in century-month code (CMC):

| Data frame | Country | Surveys |
|---|---|---|
| `MEXICO_ENADID` | Mexico | WFS 1976-77, ENADID 1992, 1997, 2006, 2009, 2014, 2018, 2023, EDER 2017, EDER 2025 |
| `NSFG_ENADID` | United States | NSFG 1973, 1976, 1982, 1988, 1995, 2002, 2006-10, 2011-13, 2013-15, 2015-17, 2017-19, 2022-23 |

The suffix `_ENADID` means "in the ENADID layout": the common format was
designed around the information collected by the Mexican ENADID, and every
other survey is converted into it. See `docs/02_common_format.md`.

The code accompanies the paper *Union Dynamics and the Union Context of Early
Childhood in Mexico and the United States: A Half-Century Comparison*
(Daniel Devolder, Centre d'Estudis Demogràfics, Universitat Autònoma de
Barcelona).


## Documentation

| Page | Content |
|---|---|
| [docs/01_build_data.md](docs/01_build_data.md) | downloading the survey files, where to put them, building the two data frames |
| [docs/02_common_format.md](docs/02_common_format.md) | why "_ENADID", the columns, the weights, what each survey provides, the date-quality flags |
| [docs/03_adding_variables.md](docs/03_adding_variables.md) | how to add a variable that is not yet in the data frames |
| [docs/04_figures_and_tables.md](docs/04_figures_and_tables.md) | which script produces each figure and table of the paper and of the presentation |
| [docs/05_methods.md](docs/05_methods.md) | Kaplan-Meier and Aalen-Johansen, net and crude measures, the mirrored curve, period indicators |
| [archive/README.md](archive/README.md) | retired code, and why it was retired |
| [NSFG_validation/README.md](NSFG_validation/README.md) | the independent re-reading of the NSFG cycles |


## Quick start

1. Install R 4.x, RStudio, and the packages listed in
   `docs/01_build_data.md`, section 1.2.
2. Download the survey files and put them in `data/` beside this folder, or
   point `config_local.R` (copied from `config_local.R.example`) at the folders
   where they already are.
3. In RStudio, open `ReadENADID.R` and Source it, then `NSFG import.R`. This
   writes `MEXICO_ENADID.Rdat` and `NSFG_ENADID.Rdat`.
4. Open `MEX_USA_figures_cohort.R` or `MEX_USA_figures_period.R` and Source
   it. The figures are written to `output/`.

Every script is meant to be opened in RStudio and run with **Source**: each
one locates the other files from its own position through
`rstudioapi::getActiveDocumentContext()`.


## Repository layout

### Building the data

| File | Role |
|---|---|
| `enadid_lib.R` | shared library: data paths, CMC arithmetic, month imputation, cleaning, weights, date-quality filter; loads the libraries in `lib/` |
| `config_local.R.example` | template for `config_local.R`, your own data locations (not in git) |
| `ReadENADID.R` | builds `MEXICO_ENADID` (entry point) |
| `ReadMujeres1992.R` ... `ReadMujeres2023.R` | one reader per ENADID round |
| `ReadEDER2017.R`, `ReadEDER2025.R` | readers of the EDER life-history surveys |
| `WFS_to_ENADID.R`, `mxsr02.Rdat` | Mexico World Fertility Survey 1976-77, and its prepared data file |
| `NSFG import.R` | builds `NSFG_ENADID` (entry point) |
| `NSFG_lib.R` | SPSS setup-file parser and NSFG helpers |
| `NSFG_harmonize_types.R` | common factor levels across NSFG cycles |
| `NSFG_impute_dissolution.R`, `NSFG_impute_dissolution_model.R` | repair of the missing union-end dates of NSFG 2002 |
| `adjust WFS under 20.R` | one-off correction of the WFS women under 20, kept for the record |

### Libraries (`lib/`)

| File | Supplies |
|---|---|
| `lib/lib.R` | general helpers: `stripLabels`, `tabNA`, `check_bind_conflicts`, `library2`, ggplot themes |
| `lib/KaplanMeierLib.R` | `KaplanMeier()`, `KaplanMeierPlot()`, `KaplanMeierDraw()`, period life tables `ppr_doIt()`, `calc_ppr()`, `plot_ppr()`, `plotBySurvey()`, `yearFrom_cmc()` |
| `lib/unionEpisodes.R` | union episodes and the Aalen-Johansen estimators: `buildUnionEpisodes()`, `unionStateOccupancy()`, `unionCompetingRisks()` |
| `lib/mexUsaFigures.R` | survey selections, analysis samples and plotting helpers shared by the Mexico-USA figures |
| `lib/mirroredCurve.R` | the two-event "mirrored" curve: `mirroredCurve()`, `mirroredCurveBootstrap()`, `mirroredCurvePlot()` |
| `lib/pprCompetingRisks.R` | crude (competing-risk) version of the period indicators: `ppr_cr_doIt()` |
| `lib/lifeCourseAJ_lib.R` | women from age 15 to 45 on the age scale, Aalen-Johansen |
| `lib/childUnionContext.R` | union setting of firstborn children, stratified Aalen-Johansen |
| `lib/adjustedSurv.R` | standardised survival curves after a Cox model |
| `lib/unionType_lib.R` | harmonised union types: `harm_union_type()` |
| `lib/DHS_lib.R`, `lib/FFS_Lib.R` | fertility rates and parity progression routines, written for DHS files |

### Analyses and figures

| File | Produces |
|---|---|
| `MEX_USA_figures_cohort.R` | the Mexico-USA figures by union cohort (paper Figures 1 to 4 and 7, and the Aalen-Johansen figures that replace Figure 6) |
| `MEX_USA_figures_period.R` | the Mexico-USA figures by calendar period (Figures 5, 10, 15 to 17, 19) and the new period transitions |
| `ENADID fertility.R` | total fertility rates (Figures 11 to 14) and the union setting of children (Figures 8 and 9, and their corrected versions) |
| `MEX_USA_lifecourse_AJ.R` | women from age 15 to 45: single, in union, separated, widowed |
| `presentation_KM_AJ.R` | Kaplan-Meier against Aalen-Johansen, Mexico (methods slides) |
| `KaplanMeier.R` | the per-country curves, and further analyses of union transitions (Cox model, standardised curves, EDER and GGS comparisons) |
| `KaplanMeier_compareSurveys.R` | the same curves for Mexico, one per survey (Figure 18) |
| `union_birth_life_expectancy.R`, `union_birth_lifeexp_calc.R` | years lived between two ages in each union and birth state |
| `women_births.R` | weighted counts of women and births by year and age |

See `docs/04_figures_and_tables.md` for the full mapping.

### Other survey families

| File | Source |
|---|---|
| `GGS_to_ENADID.R`, `GGS_true_to_ENADID.R` | Generations and Gender Survey (Harmonized Histories; GGS II) |
| `DHS_to_ENADID.R` | Colombia DHS 2015 |
| `Spain_CIS2006_to_ENADID.R` | Spanish fertility survey of 2006 (reads the raw file only) |

### Checks, validation and tests

| File or folder | Role |
|---|---|
| `filterDateQuality_audit.R` | what the date filter removes |
| `checkFlag.R` | tabulates any `_I` date-quality flag by survey (paper Tables 5 and 6) |
| `diagnose_EDER2017_codes.R` | contradictions in the EDER 2017 union codes |
| `NSFG document dissolution problems 2002.R` | the tabulations behind the 2002 repair (paper Table 3) |
| `Mexico/ENADID_validation/`, `NSFG_validation/` | a second, independent reading of each survey from its codebook, compared row by row with the first |
| `tests/` | tests on simulated data, no survey file needed: open one and Source it |
| `archive/` | retired code |


## Validation

Both reading pipelines are checked by a second, independent version of each
cycle written straight from the published codebooks and compared row by row
with the first. All seven ENADID rounds are done; for the NSFG, 1973, 1976 and
2022-23 are done and the other cycles are stubs that stop with the name of the
codebook to work from. See `NSFG_validation/README.md`. The 2002 dissolution
problem was found this way: among remarried women, all 119 whose husband had
children from a previous relationship have no end date for the marriage,
against none of the 568 whose husband had none.


## Tests

The files in `tests/` build small simulated data sets and check the
estimators against `survival::survfit()` or against values computed by hand.
They need no survey file. Open one in RStudio and Source it; each ends by
printing that all its tests passed.

| Test | Checks |
|---|---|
| `tests_kaplanMeier.R` | `KaplanMeier()`, weighted and unweighted, both variance paths, truncation |
| `tests_mirroredCurve.R` | the mirrored curve and its bootstrap |
| `tests_unionEpisodes.R` | union episodes and the Aalen-Johansen occupancy |
| `tests_childUnionContext.R` | the child union-context estimator |
| `tests_adjustedSurv.R` | standardised survival curves |
| `tests_harmUnionType.R` | harmonisation of union types |
| `tests_imputed_month.R` | month imputation under the `capMonth` constraints |


## Use of AI

Anthropic's Claude was used to check the coding of every survey against its
codebook, to write the code that reads the Mexican EDER surveys, and to help
with the analysis. The changes made with Claude are marked in the source with
comments naming Claude and the date.
