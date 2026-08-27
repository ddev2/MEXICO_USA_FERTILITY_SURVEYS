# MEXICO_USA_FERTILITY_SURVEYS

R code that reads Mexican and United States fertility surveys and converts
them into a single harmonised format, so that union histories and birth
histories collected over fifty years by different questionnaires can be
compared directly.

The code produces two data frames:

| Data frame | Country | Surveys |
|---|---|---|
| `MEXICO_ENADID` | Mexico | WFS 1976-77, ENADID 1992, 1997, 2006, 2009, 2014, 2018, 2023, EDER 2017, EDER 2025 |
| `NSFG_ENADID` | United States | NSFG 1973, 1976, 1982, 1988, 1995, 2002, 2006-10, 2011-13, 2013-15, 2015-17, 2017-19, 2022-23 |

Both use the same column names and the same conventions, so
`bind_rows(MEXICO_ENADID, NSFG_ENADID)` gives one pooled file of women with,
for each of them, up to ten unions and up to seventeen live births, all dated
in century-month code (CMC, month 1 = January 1900).

Converters for other survey families (WFS, GGS, DHS, the 2006 Spanish
fertility survey) write into the same format and are included as well.


## 1. What the code does

The work has four stages.

**Reading.** One script per survey reads the raw file published by the
statistical agency, locates each variable by its position or its name in the
codebook, and rebuilds the woman's union and birth history in the common
column layout. Missing months and missing years are recoded and flagged
rather than dropped.

**Harmonising.** Factor levels, weights and identifiers are made consistent
across surveys, the per-survey frames are stacked, and date quality is
audited. Women whose dates cannot be repaired are flagged, and the caller
decides whether to remove them.

**Validating.** A second, independent reading was written from the published
codebooks and compared row by row against the first one. Any difference points
either to a mistake in the original reading or to a misreading of the
codebook. All seven ENADID cycles have been done; three of the twelve NSFG
cycles have, and the rest are stubs waiting to be written. See section 6.

**Analysing.** Kaplan-Meier estimates of entry into first and second union,
fertility rates and parity progression ratios, state-occupancy life
expectancy by union and birth status, and the plots that go with them.


## 2. Requirements

R 4.x with the following packages:

```r
install.packages(c("tidyverse", "haven", "foreign", "data.table", "bit64",
                   "janitor", "survival", "patchwork", "ggrepel", "scales",
                   "scam", "directlabels", "MASS", "rstudioapi"))
```

The scripts are written to be run from RStudio, which they use to locate
themselves (`rstudioapi::getActiveDocumentContext()`). Open a script and use
Source, rather than calling `Rscript` from a terminal.

Install the packages before the first run. `lib/KaplanMeierLib.R` calls
`library2("scam")` as it loads, and `library2()` installs a package that is
not yet present, so a missing package turns into a silent install.

Four shared libraries that this code depends on are bundled in `lib/`, so no
external folder is needed:

| File | Supplies |
|---|---|
| `lib/lib.R` | general helpers: `stripLabels`, `tabNA`, `library2`, ggplot themes |
| `lib/KaplanMeierLib.R` | survival curve estimation and plotting, `yearFrom_cmc` |
| `lib/DHS_lib.R` | fertility rate and parity progression code written for DHS files |
| `lib/FFS_Lib.R` | older Fertility and Family Survey routines, loaded by `DHS_lib.R` |

`enadid_lib.R` loads `lib/lib.R` for you. Until August 2026 these four files
sat in personal Dropbox and Google Drive folders, one of them reachable only
through a personal `.Rprofile`, so the repository could not run anywhere else.


## 3. Repository layout

```
Mexico-USA/
├── R/                          this repository
│   ├── enadid_lib.R            shared library: CMC arithmetic, cleaning, weights
│   ├── ReadENADID.R            builds MEXICO_ENADID  (entry point)
│   ├── NSFG import.R           builds NSFG_ENADID    (entry point)
│   ├── ReadMujeres*.R          one reader per ENADID cycle
│   ├── ReadEDER2017.R          EDER life-history surveys
│   ├── ReadEDER2025.R
│   ├── WFS_to_ENADID.R         Mexico World Fertility Survey 1976-77
│   ├── NSFG_lib.R              SPSS setup-file parser and NSFG helpers
│   ├── NSFG_harmonize_types.R  common factor levels across NSFG cycles
│   ├── NSFG_impute_dissolution*.R  repair of the 2002 union-end gap
│   ├── KaplanMeier*.R          survival analysis and plots
│   ├── ENADID fertility.R      fertility rates, parity progression, plots
│   ├── union_birth_life_expectancy.R   state-occupancy life expectancy
│   ├── NSFG_validation/        independent re-reading of the NSFG cycles
│   ├── Mexico/ENADID_validation/  independent re-reading of the ENADID cycles
│   ├── lib/                    bundled shared libraries
│   └── config_local.R          your own data locations (not in git)
├── data/                       survey files, see section 4
└── output/                     plots written by the analysis scripts
```

### Where the data is looked for

`enadid_lib.R` works out the project folder and derives every data path from
it. It starts from the document open in the RStudio editor and walks up at
most four levels looking for `enadid_lib.R`, so keep a script from this
repository active when you Source. The default is a `data/` folder beside
`R/`:

```
data/
├── Mexico/          INEGI files; derived .Rdat files go in ENADID/,
│                    except WFS_ENADID1977.Rdat which sits at this level
│   ├── ENADID/
│   └── EDER/
├── USA/
│   └── NSFG/        CDC files, one folder per cycle
└── other/           optional GGS, DHS and Spanish files
```

If the surveys already live somewhere else on your disk, do not move them.
Copy `config_local.R.example` to `config_local.R` and set the roots there:

```r
mexicoRoot <- "~/Surveys/Mexico"
nsfgRoot   <- "~/Surveys/USA/NSFG"
outputPath <- "~/Surveys/output"
```

`config_local.R` is ignored by git, so each machine keeps its own. Restart R
after changing it. The variables it can set are `dataRoot`, `mexicoRoot`,
`nsfgRoot`, `otherRoot`, `dhsRoot` and `outputPath`; anything left unset falls
back to the layout above.


## 4. Downloading the survey files

### Mexico, from INEGI

Each survey has a programme page with a "Microdatos" section. Download the
database in the format named in the table and unpack it so that the folder
names below appear under `data/Mexico/`.

| Survey | Expected location under `data/Mexico/` |
|---|---|
| ENADID 1992 | `ENADID/1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_1.DBF`, `FECUNDIDAD_2.DBF` |
| ENADID 1997 | `ENADID/1997/base_datos_enadid97_dbf/E97CMU.DBF`, `E97DGE.DBF`, `E97UNI.DBF`, `E97HEM.DBF` |
| ENADID 2006 | `ENADID/2006/ENADID06_Mujer.csv`, `ENADID06_Fecundidad.csv` |
| ENADID 2009 | `ENADID/2009/base_datos_enadid09_dbf/tr_cmu.dbf`, `tr_smi.DBF`, `tr_viv_hog.dbf`, `tr_fec_hemb.dbf` |
| ENADID 2014 | `ENADID/2014/enadid_2014_csv/tmmujer1_enadid2014/conjunto_de_datos/tmmujer1.csv`, and the same pattern for `tmmujer2` and `tfec_hemb` |
| ENADID 2018 | `ENADID/2018/conjunto_de_datos_enadid_2018_csv/conjunto_de_datos_tmujer1_enadid_2018/conjunto_de_datos/conjunto_de_datos_tmujer1_enadid_2018.csv`, and the same pattern for `tmujer2` and `tfechisemb` |
| ENADID 2023 | `ENADID/2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer1_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer1_enadid_2023.csv`, and the same for `tmujer2` and `tfechisemb`. Note that the CSV names drop the `de_` that 2018 has |
| EDER 2017 | `EDER/2017/eder2017_bases_sav/historiavida.sav`, `antecedentes.sav` |
| EDER 2025 | `EDER/2025/eder2025_bases_sav/historiavida.sav`, `informante.sav` |

Programme pages: [ENADID](https://www.inegi.org.mx/programas/enadid/2023/) and
[EDER](https://www.inegi.org.mx/programas/eder/2017/), changing the year in the
address for the other rounds.

The Mexico World Fertility Survey of 1976-77 is the one exception. Its
prepared file `mxsr02.Rdat` is included in this repository, and
`WFS_to_ENADID.R` asks for its location if it cannot find it in the working
directory.

### United States, from the CDC

The National Survey of Family Growth publishes each cycle on its own page
under [cdc.gov/nchs/nsfg](https://www.cdc.gov/nchs/nsfg/). Download the female
respondent file, the pregnancy file, and for 2002 onwards the SPSS setup
files, then place them in a folder named for the cycle under `data/USA/NSFG/`.

| Cycle | Files expected in `USA/NSFG/<folder>/` |
|---|---|
| `1973` | `1973NSFGData.dat` |
| `1976` | `1976NSFGData.dat` |
| `1982` | `1982NSFGData.dat` |
| `1988` | `1988FemRespData.dat`, `1988PregData.dat` |
| `1995` | `1995FemRespData.dat`, `1995PregData.dat` |
| `2002` | `2002FemResp.dat`, `2002FemRespSetup.sps`, `2002FemPreg.dat`, `2002PregSetup.sps` |
| `2006-10` | `2006_2010_FemResp.dat`, `2006_2010_FemRespSetup.sps`, `2006_2010_FemPreg.dat`, `2006_2010_FemPregSetup.sps` |
| `2011-13` | `2011_2013_FemRespData.dat`, `2011_2013_FemRespSetup.sps`, `2011_2013_FemPregData.dat`, `2011_2013_FemPregSetup.sps` |
| `2013-15` | `2013_2015_...` , same four names with the year changed |
| `2015-17` | `2015_2017_...` |
| `2017-19` | `2017_2019_...` |
| `2022-23` | `NSFG-2022-2023-FemRespPUFData.sas7bdat`, `NSFG-2022-2023-FemPregPUFData.sas7bdat` |

The first five cycles are read by byte position, taken from the printed
codebooks. From 2002 the SPSS setup file supplies the positions, and
`NSFG_lib.R` parses it. The 2022-23 cycle is distributed as SAS files and is
read with `haven`.


## 5. Building the two data frames

### MEXICO_ENADID

Open `ReadENADID.R` in RStudio and Source it. It sources `enadid_lib.R`, then
each Mexican reader in turn, then stacks the results, harmonises the factor
levels, computes the weights and saves the result:

```
data/Mexico/ENADID/MEXICO_ENADID.Rdat
```

Each reader also saves its own intermediate file (`ENADID1997.Rdat` and so on)
in the same folder. These are written for inspection, not reused: every run
re-reads the raw DBF and CSV files from the start.

The switch at the top of `ReadENADID.R` controls month imputation:

```r
capMonth <- TRUE
```

With `TRUE`, an imputed month can never fall after the interview date, and
dates within one union are kept in order, so a marriage cannot precede the
start of the union that contains it. With `FALSE` the earlier behaviour is
restored, a uniform draw with no constraint. Each reader sets its own default
only when it is run on its own, so the value set in `ReadENADID.R` governs the
whole run.

### NSFG_ENADID

Open `NSFG import.R` and Source it. It reads the twelve cycles, harmonises
their types, then repairs the two gaps in the 2002 questionnaire described in
`NSFG_impute_dissolution.R`: unions of women who had recently separated, and
marriages to a husband who already had children, both of which the 2002
interview skipped. A single dissolution-timing model estimated on the 2006-10
cycle supplies the missing dates. The result is saved as:

```
data/Mexico/ENADID/NSFG_ENADID.Rdat
```

together with one file per cycle (`NSFG_ENADID_1995.Rdat` and so on). At the
end the script prints `DATE_QUALITY_SUMMARY`, the output of
`summarizeDateQuality()` described in the next section.

### Loading them again

Once both files exist:

```r
source("enadid_lib.R")
loadENADID_data()          # loads MEXICO_ENADID and NSFG_ENADID
pooled <- dplyr::bind_rows(MEXICO_ENADID, NSFG_ENADID)
pooled <- filterDateQuality(pooled)
```

`filterDateQuality()` has three independent switches: `dropBadUnionDates`
(on by default), `dropBackfilledEnd` and `dropBadBirthDates` (both off).
Impossible ages are always removed. Note that `dropAllBad = TRUE` does not
mean what its name suggests: it forces the first switch on and the other two
off, which is the same as the default call.

`summarizeDateQuality()` reports what each switch would remove without
removing anything. It returns one row per survey plus a total, with columns
`nWomen`, `removed_union`, `corrected_union`, `removed_birth` and
`removed_bad_age`.


## 6. Validation against the codebooks

Both reading pipelines are checked by writing a second, independent version of
each cycle straight from the published codebooks, without looking at the first
version, and comparing the two row by row. This work is done with Claude, one
cycle at a time.

| Folder | Cycles written | Still to write |
|---|---|---|
| `Mexico/ENADID_validation/` | 1992, 1997, 2006, 2009, 2014, 2018, 2023 | none |
| `NSFG_validation/` | 1973, 1976, 2022-23 | 1982, 1988, 1995, 2002, 2006-10, 2011-13, 2013-15, 2015-17, 2017-19 |

The cycles not yet written are present as stubs that call `stop()` with the
name of the codebook to work from, so the gap is visible rather than silent.
The workflow for filling one in is in `NSFG_validation/README.md`.

Each folder has the same three files: `*_val_lib.R` with its own helpers,
`*_val_import.R` with one `getDatos_..._val()` function per cycle, and
`*_val_compare.R` with the comparison functions. The validation code borrows
no reading or recoding function from `enadid_lib.R`, which is the point: an
error common to both would otherwise go unseen. It takes only the data
locations from it, so run it with the project library already loaded.

`NSFG_validation/README.md` records the output schema every cycle must
produce, the CMC conventions used by each questionnaire, and the coding rules
the validation follows. `NSFG_validation/codebooks/` and
`questionnaire/` hold the text of the codebooks and of the marital history
sections, obtained by running OCR on the scanned PDFs
(`codebooks/ocr_scanned_codebooks.sh`). The scans themselves are about 360 MB
and are not in the repository; the `PDF/` subfolders are in `.gitignore`.

The 2002 dissolution problem was found this way. `NSFG document dissolution
problems 2002.R` holds the tabulations that establish it, and the result is
recorded at the top of `NSFG_impute_dissolution.R`: among remarried women, all
119 whose husband had children from a previous relationship have `MARENDHX`
missing, against none of the 568 whose husband had not. The 2002 interview
skipped the whole marriage-end module for those women, and the published
universe statement does not mention it.


## 7. Analysis and plots

| Script | Produces | Needs the data frames loaded first |
|---|---|---|
| `KaplanMeier.R` | survival curves by country and cohort for entry into the first and second union, for separation from each, for separation from a cohabiting union, and for re-partnering after a first separation; saves six plots | yes |
| `KaplanMeier_compareSurveys.R` | the same curves for Mexico only, faceted by survey; builds the plots on screen but saves none | yes |
| `ENADID fertility.R` | total fertility rates, age-specific rates and parity progression ratios, using the DHS routines | yes |
| `union_birth_life_expectancy.R` | defines the state-occupancy functions: years lived between two ages in each of fourteen union and birth states | yes |
| `union_birth_lifeexp_calc.R` | runs those functions on the pooled file and prints the five-state summary | no, it calls `loadENADID_data()` itself |
| `women_births.R` | weighted counts of women aged 15 to 49, and of births, by calendar year and age. Defines `women_by_age_year()` and `births_by_age_year()`, then runs both on `MEXICO_ENADID` | yes |

Saved plots go to the folder given by `outputPath`, which is `output/` beside
`R/` unless `config_local.R` says otherwise.

Diagnostic scripts: `tests_imputed_month.R` checks the month imputation
without needing any survey file, `filterDateQuality_audit.R` reports what the
date filter removes, `checkFlag.R` tabulates any imputation flag by survey,
and `diagnose_EDER2017_codes.R` checks the EDER 2017 union code sequences for
contradictions.


## 8. Other survey families

These are not part of the two main pipelines. They read from `otherRoot` and
`dhsRoot`.

| Script | Source | State |
|---|---|---|
| `WFS_to_ENADID.R` | Mexico World Fertility Survey 1976-77 | converts, and is sourced by `ReadENADID.R` |
| `GGS_to_ENADID.R` | Generations and Gender Survey, Harmonized Histories | converts |
| `GGS_true_to_ENADID.R` | Generations and Gender Survey II, national files | converts |
| `DHS_to_ENADID.R` | Colombia DHS 2015 | converts |
| `Spain_CIS2006_to_ENADID.R` | Spanish fertility survey, CIS study 2639 of 2006 | six lines, reads the raw file only; no conversion written yet |

`adjust WFS under 20.R` is a one-off correction applied to the WFS women aged
under 20, kept for the record.
