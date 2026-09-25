# 3. Adding a variable to MEXICO_ENADID or NSFG_ENADID

The two data frames keep only the columns listed in `docs/02_common_format.md`.
A variable that a survey collects but that is not in that list (the state of
residence, education, a question on contraception...) has to be carried
through every stage of the build. This page lists the stages, then works
through an example.


## 3.1 The path a variable follows

**Mexico** (`ReadENADID.R`):

```
raw file (DBF, CSV, SAV)
  -> getDatosYYYY() in ReadMujeresYYYY.R      picks and recodes the survey's own variables into 'datos'
  -> bigDataWomen() in enadid_lib.R           copies 'datos' into the common layout, one row per woman
  -> ENADIDYYYY_full                          joined with the birth history, cleaned, reordered
  -> bind_rows() in ReadENADID.R              all surveys stacked into MEXICO_ENADID
  -> selColumns in ReadENADID.R               ONLY the columns listed here are kept
  -> MEXICO_ENADID.Rdat
```

The WFS (`WFS_to_ENADID.R`) and the EDER (`ReadEDER2017.R`,
`ReadEDER2025.R`) build their frame directly, without `bigDataWomen()`.

**United States** (`NSFG import.R`):

```
raw file (.dat with an SPSS setup file, or .sas7bdat)
  -> getDatos_YYYY() in "NSFG import.R"       reads the file and builds 'datos' in the common layout
  -> bind_rows(surveys_list)                  all cycles stacked into NSFG_ENADID
  -> selColumns near the end of the file      ONLY the columns listed here are kept
  -> NSFG_ENADID.Rdat
```

Two rules follow from this:

1. A column that is not in `selColumns` is **silently dropped** at the end of
   the build, even if every reader creates it. This is the most common reason
   why a new variable "disappears".
2. A survey that does not create the column simply gets `NA` for it when the
   frames are stacked (`bind_rows()` fills the gaps). You do not need to touch
   the readers of surveys that lack the question.


## 3.2 Where to find the variable in the raw files

| Survey | Where the variable names and codes are |
|---|---|
| ENADID 1992 to 2023 | the INEGI data dictionary ("descriptor de archivos") of each round; the readers use the names as INEGI publishes them, for example `p10_1` for question 10.1 in 2018. `Mexico/ENADID_validation/` has small CSV dictionaries |
| EDER 2017, 2025 | the INEGI dictionary of the `.sav` files; the labels are also readable with `haven::read_sav()` |
| WFS 1976 | the variable names of `mxsr02.Rdat` (`V617` and so on), listed at the top of `WFS_to_ENADID.R` |
| NSFG 1973 to 1995 | the printed codebooks: these cycles are read BY BYTE POSITION, and the positions are written in the reader. The OCR text of the codebooks is in `NSFG_validation/codebooks/` |
| NSFG 2002 to 2017-19 | the SPSS setup file (`...Setup.sps`) gives name and position; `read_nsfg_data()` in `NSFG_lib.R` reads it, so a variable is available by its CDC name, for example `df_NSFG_2006_10$RMARITAL` |
| NSFG 2022-23 | the SAS file; variables by their CDC name |


## 3.3 Worked example: the state of residence in the ENADID

The 2018 reader already reads the state (`ent`) but does not pass it on:

```r
# ReadMujeres2018.R, inside getDatos2018()
datos <- data.frame(llave_muj=mujeres$llave_muj, region=mujeres$ent.x)
```

To make `region` a column of `MEXICO_ENADID`:

**Step 1. Every reader that has the variable puts it in `datos`**, with the
same name and the same coding. In each `getDatosYYYY()` of the ENADID rounds
that collect it:

```r
datos$region <- as.integer(mujeres$ent)          # the INEGI state code, 1 to 32
```

(In 2018 the column is called `ent.x`, because the reader joins two tables
that both have it. Look at `names(mujeres)` in each reader.)

Check the dictionary of each round: the same question can have a different
name or different codes from one round to the next. Recode here, in the
reader, so that the value means the same thing in every survey.

**Step 2. `bigDataWomen()` copies it into the common layout.** In
`enadid_lib.R`, near the other copies (`ENADID$ever_contraception <- datos$ever_contraception`):

```r
ENADID$region <- datos$region
```

If a reader does not create `datos$region`, `datos$region` is `NULL` and the
line creates nothing, which is what you want: the column will be `NA` for that
survey.

**Step 3. The converters that do not use `bigDataWomen()`** (WFS, EDER) create
the column themselves if their survey has the information, or do nothing.

**Step 4. Add the name to `selColumns` in `ReadENADID.R`**, otherwise it is
dropped:

```r
selColumns <- c(
  "country","survey","surveyDate_cmc", ...,
  "age_first_sex","ever_had_sex","ever_contraception",
  "region"
)
```

**Step 5. If it is a factor, give it the same levels in every survey.**
`bind_rows()` does not check meanings: it turns a factor stacked with a
character column into character, and two factors with different labels into
one factor that keeps both sets, so that "married" and "casada" become two
categories. Either code the variable as an integer, as above, or set identical
`levels` and `labels` in every reader.
`check_bind_conflicts()` (in `lib/lib.R`) lists the columns whose types
disagree before stacking.

**Step 6. Rebuild and check.** Source `ReadENADID.R`, then:

```r
loadENADID_data()
table(MEXICO_ENADID$survey, is.na(MEXICO_ENADID$region))   # which surveys have it
```

**Step 7. Document it** in the column list of `docs/02_common_format.md`.


## 3.4 The same for the NSFG

1. In each `getDatos_YYYY()` of `NSFG import.R` whose cycle has the question,
   add the column to `datos`, for example
   `datos$education <- df_NSFG_2006_10$HIEDUC`, recoded to a common coding.
   For the cycles read by position (1973 to 1995), add the byte positions from
   the codebook to the reader's column specification first.
2. If it is a factor, check `harmonize_survey_types()` (`NSFG_harmonize_types.R`),
   which sets the common levels before the cycles are stacked.
3. Add the name to `selColumns` near the end of `NSFG import.R`.
4. Rebuild, and tabulate `table(NSFG_ENADID$survey, is.na(NSFG_ENADID$education))`.

If the variable should be compared across the two countries, use the same
name and the same coding in both builds, so that
`bind_rows(MEXICO_ENADID, NSFG_ENADID)` stacks it cleanly.


## 3.5 A new union or birth slot, or a new date

Union and birth dates follow a stricter pattern, because the cleaning and the
imputation functions look for them by name:

- a date is stored in CMC, with its `_I` flag, and is computed with
  `compute_cmc(month, year)` and `imputed_date(month, year)` (in
  `enadid_lib.R`), never by hand;
- union columns follow the pattern `<base><u>`, with the bases
  `union_start_type`, `union_start_cmc`, `marriage_start_cmc`, `union_end_cmc`,
  `union_end_motive`;
- the number of slots is set by the loops `for (u in 1:7)` and
  `for (b in 1:25)` that build `selColumns` (1:10 and 1:17 for the NSFG), and
  by `harm_union_type(MEXICO_ENADID, 7)`.

A new kind of date (for example the date of first sexual intercourse) should
get both columns, the CMC and its flag, so that `filterDateQuality()` and the
analysis functions can treat it like the others.
