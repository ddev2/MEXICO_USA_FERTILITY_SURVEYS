# 2. The common format, and why the files are called "_ENADID"

## 2.1 Why "_ENADID"

The surveys used here were designed by different agencies over fifty years.
Their questionnaires ask about unions and births in different ways: some
record every union, some only the first and the last; some give the month of
each event, some only the year; some distinguish the start of co-residence
from the date of marriage, others do not.

To compare them, every survey is converted into **one common layout**. That
layout was designed first for the Mexican ENADID (Encuesta Nacional de la
Dinámica Demográfica), the survey with the largest samples and the longest
series in this project, and it is built from the information the ENADID
collects: the woman's date of birth and weight, the dates of start of each
union and of the marriage when it differs, the date and the reason of each
union's end, and the date of birth (and death) of each child.

Every other survey is then converted **into the ENADID layout**, which is what
the suffix means:

| Object or script | Meaning |
|---|---|
| `MEXICO_ENADID` | all Mexican surveys, in the ENADID layout |
| `NSFG_ENADID` | all US NSFG cycles, in the ENADID layout |
| `EDER_ENADID`, `EDER_ENADID25` | the EDER life-history surveys, before they are stacked into `MEXICO_ENADID` |
| `GGS_ENADID` | Generations and Gender Survey histories, in the same layout |
| `WFS_to_ENADID.R`, `DHS_to_ENADID.R`, `GGS_to_ENADID.R` ... | converters from another survey family into the layout |
| `bigDataWomen()` in `enadid_lib.R` | the function that builds the layout from the variables a reader has extracted |

Because the layout is the same, `dplyr::bind_rows(MEXICO_ENADID, NSFG_ENADID)`
gives one pooled file, and every analysis function (Kaplan-Meier, Aalen-Johansen,
period life tables, fertility rates) works on any survey without change.


## 2.2 Conventions

- **One row per woman.** Unions and births are in numbered columns, not in
  separate rows: `union_start_cmc1`, `union_start_cmc2`, ... and `dob_cmc1`,
  `dob_cmc2`, ...
- **Dates are in century-month code (CMC):** month 1 is January 1900, so
  `cmc = (year - 1900) * 12 + month`. Year of a CMC: `yearFrom_cmc()`.
- **Every date has a quality flag** in the column of the same name with `_I`
  added (`union_start_cmc_I1` for `union_start_cmc1`). See section 2.5.
- **Unions are numbered in the order they began**, and births in order of
  birth date (`reorder_birthHistory()`).
- **Factor levels are harmonised** across surveys, in English
  (`harm_union_type()` for union types, `harmonize_survey_types()` for the
  NSFG).


## 2.3 Columns

Mexico keeps 7 union slots and 25 birth slots, the USA 10 and 17. Columns that
a survey does not collect are `NA` for its women.

### The woman

| Column | Content |
|---|---|
| `country` | `MEXICO` or `USA` |
| `survey` | survey name: `WFS`, `ENADID1992` ... `ENADID2023`, `EDER2017`, `EDER2025`, `NSFG1973` ... `NSFG2022_23` |
| `surveyDate_cmc` | date of the interview |
| `indiv_dob_cmc`, `indiv_dob_cmc_I` | date of birth of the woman, and its flag |
| `indiv_age_survey` | age at interview, completed years |
| `yBirth` | year of birth |
| `llave_muj` | the survey's own identifier of the woman (ENADID only) |
| `lastYear` | last complete calendar year of observation, used by the period life tables |

### Weights

| Column | Content |
|---|---|
| `indiv_weight` | the weight published with the survey (an expansion factor) |
| `weight` | `indiv_weight` divided by its mean in the survey: mean 1 in every survey. Keeps the relative weights of the sampling design; used for variances and for the period life tables (`ppr_doIt()`). |
| `popWeight` | `weight` calibrated, survey by survey, so that the weighted number of women at each age equals the population at that age in the survey year (`addWeights()`). Used for point estimates when surveys are pooled, so that a large survey does not outweigh a small one covering other years. |

### Unions, for each slot `u` = 1, 2, ...

| Column | Content |
|---|---|
| `union_start_type<u>` | how the union began: `cohabitation`, `cohabitation before marriage` (cohabitation later converted), `marriage` (direct marriage), `don't know` |
| `union_start_cmc<u>`, `_I<u>` | start of co-residence |
| `marriage_start_cmc<u>`, `_I<u>` | date of the marriage, if the union became one (equal to the start for a direct marriage) |
| `union_end_cmc<u>`, `_I<u>` | end of the union, `NA` if still in union at the interview |
| `union_end_motive<u>` | reason of the end: `separation`, `divorce`, `widowhood`, `in union`, `unknown` ... (resolved into separation / widowed / unknown by `UNION_MOTIVE_MAP` in `lib/unionEpisodes.R`) |
| `nUnion` | number of unions reported |
| `union_status` | marital status at the interview |

### Births, for each slot `k` = 1, 2, ...

| Column | Content |
|---|---|
| `dob_cmc<k>`, `dob_cmc_I<k>` | date of birth of the k-th live birth |
| `dod_cmc<k>` | date of death of that child, if deceased |
| `sex<k>` | sex of that child (1 male, 2 female) |
| `nBioKids` | number of live births |

### Fertility intentions and sexual history (fewer surveys)

`pregnant`, `pregnant_wanted`, `pregnant_want_another`, `pregnant_ideal_number`,
`nullipar_fecund`, `nullipar_want_another`, `nullipar_ideal_number`,
`mother_fecund`, `mother_want_another`, `mother_ideal_number`, `mother_less`,
`mother_unwanted`, `motive_no_child`, `ideal_number`, `want_another`,
`age_first_sex`, `ever_had_sex`, `ever_contraception`. Several of these are
collected only by the ENADID; they are kept for completeness and are not used
in the union analyses.


## 2.4 What each survey provides

The union history differs from one survey to the next, which is why every
analysis uses its own list of surveys (`SURVEY_SELECTION` in
`lib/mexUsaFigures.R`).

| Survey | Women | Ages | Union history |
|---|---|---|---|
| WFS 1976 | 7,310 | 15-49 | complete, but no cohabitation before marriage |
| ENADID 1992 | 69,538 | 15-54 | none |
| ENADID 1997 | 88,022 | 15-54 | complete |
| ENADID 2006 | 38,923 | 15-54 | first union |
| ENADID 2009, 2014, 2018, 2023 | 98,711 to 108,439 | 15-54 | first and last union |
| EDER 2017 | 13,082 | 20-54 | complete (life history) |
| EDER 2025 | 14,094 | 18-64 | complete (life history) |
| NSFG 1973 | 9,797 | 15-44 | complete; ever-married women and mothers only |
| NSFG 1976 | 8,611 | 15-44 | up to 3 unions; ever-married women and mothers only |
| NSFG 1982 | 7,969 | 15-44 | complete, no cohabitation before marriage |
| NSFG 1988 | 8,450 | 15-44 | first union |
| NSFG 1995 to 2015-17 | 5,554 to 12,279 | 15-44 (15-49 from 2015) | complete |
| NSFG 2017-19 | 6,141 | 15-49 | incomplete |
| NSFG 2022-23 | 5,586 | 15-49 | first union |

The NSFG cycles from 2015-17 give dates as years only; the month is imputed
(section 2.5).


## 2.5 Date-quality flags (`_I` columns)

A date that is missing, partly missing or impossible is not dropped silently.
It is recoded, and the flag says what was done. The full list is the "_I code
registry" comment in `enadid_lib.R`; the main codes are:

| Code | Meaning |
|---|---|
| `0` | exact date, as reported |
| `1` | month missing or out of range, imputed (see `capMonth`) |
| `10`, `11` | year unknown ("don't know", refused); the date is not usable |
| `15` | date after the interview |
| `20`, `21`, `22` | union end missing, set to the start of the next union minus one month |
| `30`, `31` | end date taken from the divorce date |
| `41`, `42` | end date imputed by the 2006-10 model (NSFG 2002, see `NSFG_impute_dissolution.R`) |
| `50`, `51` | dates of one union out of order |

`filterDateQuality()` uses these codes to remove women whose union or birth
dates cannot be used (section 1.7 of `docs/01_build_data.md`).
