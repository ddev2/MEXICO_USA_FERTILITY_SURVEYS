# 1. Building MEXICO_ENADID and NSFG_ENADID

This page explains, step by step, how to go from the files published by INEGI
and by the CDC to the two harmonised data frames that every analysis in this
repository uses:

| Data frame | Country | Surveys | Built by |
|---|---|---|---|
| `MEXICO_ENADID` | Mexico | WFS 1976-77, ENADID 1992, 1997, 2006, 2009, 2014, 2018, 2023, EDER 2017, EDER 2025 | `ReadENADID.R` |
| `NSFG_ENADID` | United States | NSFG 1973, 1976, 1982, 1988, 1995, 2002, 2006-10, 2011-13, 2013-15, 2015-17, 2017-19, 2022-23 | `NSFG import.R` |

The survey files themselves are not in the repository: they belong to the
agencies that publish them and they are large. You download them once, put
them in the folders described below, and run the two scripts.


## 1.1 Overview

1. Install R and the packages (section 1.2).
2. Decide where the survey files will live on your disk (section 1.3).
3. Download the Mexican files from INEGI (section 1.4) and the NSFG files
   from the CDC (section 1.5), and unpack them into the expected folders.
4. Open `ReadENADID.R` in RStudio and Source it. This writes
   `MEXICO_ENADID.Rdat`.
5. Open `NSFG import.R` in RStudio and Source it. This writes
   `NSFG_ENADID.Rdat`.
6. Check the result (section 1.7).

Steps 4 and 5 re-read the raw files from the start every time. They are only
needed again when a reader changes or a new survey is added.


## 1.2 Software

R 4.x and RStudio, with these packages:

```r
install.packages(c("tidyverse", "haven", "foreign", "data.table", "bit64",
                   "janitor", "survival", "patchwork", "ggrepel", "scales",
                   "directlabels", "MASS", "rstudioapi"))
```

`scam` is needed only for `KaplanMeierPlot(plotType = "smooth")`, and the
function says so if it is missing.

The scripts are written to be run from RStudio. Each one starts with

```r
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
```

so it finds the other files from its own location. Open the script in the
editor and click **Source**; do not call it with `Rscript` from a terminal.


## 1.3 Where the files go

`enadid_lib.R` finds the folder that contains it (the `R/` folder of the
repository) and derives every data path from it. The default layout is a
`data/` folder beside `R/`:

```
<project>/
├── R/                      this repository
├── data/
│   ├── Mexico/             INEGI files
│   │   ├── ENADID/         one sub-folder per round; the .Rdat files are written here
│   │   └── EDER/           2017/ and 2025/
│   ├── USA/
│   │   └── NSFG/           one sub-folder per cycle
│   └── other/              optional: GGS, DHS, Spanish survey
└── output/                 figures written by the analysis scripts
```

If the files already live somewhere else, do not move them. Copy
`config_local.R.example` to `config_local.R` in the `R/` folder and set the
roots there, for example:

```r
mexicoRoot <- "~/Surveys/Mexico"
nsfgRoot   <- "~/Surveys/USA/NSFG"
outputPath <- "~/Surveys/output"
```

The variables that can be set are `dataRoot`, `mexicoRoot`, `nsfgRoot`,
`otherRoot`, `dhsRoot` and `outputPath`. Anything left unset falls back to the
layout above. `config_local.R` is ignored by git, so each machine keeps its
own. Restart R after changing it.

The two harmonised files, and the intermediate file of each survey, are
written to `<mexicoRoot>/ENADID/`. This is also where the analysis scripts
look for them.


## 1.4 Mexican files, from INEGI

Each survey has a programme page with a "Microdatos" section:
[ENADID](https://www.inegi.org.mx/programas/enadid/2023/) and
[EDER](https://www.inegi.org.mx/programas/eder/2017/) (change the year in the
address for the other rounds). Download the database in the format named in
the table and unpack it so that these paths exist under `<mexicoRoot>/`:

| Survey | Files expected |
|---|---|
| ENADID 1992 | `ENADID/1992/base_datos_enadid92_dbf/BASESDBF/FECUNDIDAD_1.DBF`, `FECUNDIDAD_2.DBF` |
| ENADID 1997 | `ENADID/1997/base_datos_enadid97_dbf/E97CMU.DBF`, `E97DGE.DBF`, `E97UNI.DBF`, `E97HEM.DBF` |
| ENADID 2006 | `ENADID/2006/ENADID06_Mujer.csv`, `ENADID06_Fecundidad.csv` |
| ENADID 2009 | `ENADID/2009/base_datos_enadid09_dbf/tr_cmu.dbf`, `tr_smi.DBF`, `tr_viv_hog.dbf`, `tr_fec_hemb.dbf` |
| ENADID 2014 | `ENADID/2014/enadid_2014_csv/tmmujer1_enadid2014/conjunto_de_datos/tmmujer1.csv`, and the same pattern for `tmmujer2` and `tfec_hemb` |
| ENADID 2018 | `ENADID/2018/conjunto_de_datos_enadid_2018_csv/conjunto_de_datos_tmujer1_enadid_2018/conjunto_de_datos/conjunto_de_datos_tmujer1_enadid_2018.csv`, and the same pattern for `tmujer2` and `tfechisemb` |
| ENADID 2023 | `ENADID/2023/conjunto_de_datos_enadid_2023_csv/conjunto_de_datos_tmujer1_enadid_2023/conjunto_de_datos/conjunto_datos_tmujer1_enadid_2023.csv`, and the same for `tmujer2` and `tfechisemb`. The CSV names drop the `de_` that 2018 has |
| EDER 2017 | `EDER/2017/eder2017_bases_sav/historiavida.sav`, `antecedentes.sav` |
| EDER 2025 | `EDER/2025/eder2025_bases_sav/historiavida.sav`, `informante.sav` |

The **Mexico World Fertility Survey of 1976-77** is the exception. Its
prepared file `mxsr02.Rdat` is in this repository. `WFS_to_ENADID.R` looks for
it in the working directory and asks for its location if it cannot find it.
It writes `WFS_ENADID1977.Rdat` to `<mexicoRoot>/`.

The exact path of every file is written near the top of each reader
(`ReadMujeres1992.R` ... `ReadMujeres2023.R`, `ReadEDER2017.R`,
`ReadEDER2025.R`). If INEGI renames a folder, that is the line to change.


## 1.5 United States files, from the CDC

The National Survey of Family Growth publishes each cycle on its own page
under [cdc.gov/nchs/nsfg](https://www.cdc.gov/nchs/nsfg/). Download the female
respondent file, the pregnancy file, and from 2002 onwards the SPSS setup
files, then put them in a folder named for the cycle under `<nsfgRoot>/`:

| Cycle | Files expected in `<nsfgRoot>/<folder>/` |
|---|---|
| `1973` | `1973NSFGData.dat` |
| `1976` | `1976NSFGData.dat` |
| `1982` | `1982NSFGData.dat` |
| `1988` | `1988FemRespData.dat`, `1988PregData.dat` |
| `1995` | `1995FemRespData.dat`, `1995PregData.dat` |
| `2002` | `2002FemResp.dat`, `2002FemRespSetup.sps`, `2002FemPreg.dat`, `2002PregSetup.sps` |
| `2006-10` | `2006_2010_FemResp.dat`, `2006_2010_FemRespSetup.sps`, `2006_2010_FemPreg.dat`, `2006_2010_FemPregSetup.sps` |
| `2011-13` | `2011_2013_FemRespData.dat`, `2011_2013_FemRespSetup.sps`, `2011_2013_FemPregData.dat`, `2011_2013_FemPregSetup.sps` |
| `2013-15` | `2013_2015_...`, the same four names with the years changed |
| `2015-17` | `2015_2017_...` |
| `2017-19` | `2017_2019_...` |
| `2022-23` | `NSFG-2022-2023-FemRespPUFData.sas7bdat`, `NSFG-2022-2023-FemPregPUFData.sas7bdat` |

The cycles up to 1995 are read by byte position, taken from the printed
codebooks. From 2002 the SPSS setup file gives the positions and `NSFG_lib.R`
parses it. The 2022-23 cycle is distributed as SAS files and is read with
`haven`.


## 1.6 Running the two builds

### MEXICO_ENADID

Open `ReadENADID.R` and Source it. It sources `enadid_lib.R`, then each
Mexican reader in turn (WFS, ENADID 1992 to 2023, EDER 2017 and 2025), stacks
the results, harmonises the union types, keeps the common columns, computes
the two weights and saves

```
<mexicoRoot>/ENADID/MEXICO_ENADID.Rdat
```

Each reader also saves its own file (`ENADID1997.Rdat` and so on) for
inspection; these are not reused by the next run.

One switch at the top of `ReadENADID.R` governs the imputation of missing
months for the whole run:

```r
capMonth <- TRUE
```

With `TRUE`, an imputed month never falls after the interview, and dates
within one union stay in order (a marriage cannot precede the start of the
union that contains it). With `FALSE`, the older behaviour: a uniform draw
with no constraint. A reader run on its own sets its own default only when
`capMonth` does not exist yet.

### NSFG_ENADID

Open `NSFG import.R` and Source it. It reads the twelve cycles, harmonises
their types, reweights them and repairs the two gaps of the 2002 questionnaire
described in `NSFG_impute_dissolution.R` (unions of recently separated women,
and marriages to a husband who already had children, both skipped by the
2002 interview). A dissolution-timing model estimated on the 2006-10 cycle
supplies the missing dates. The result is saved as

```
<mexicoRoot>/ENADID/NSFG_ENADID.Rdat
```

with one file per cycle (`NSFG_ENADID_1995.Rdat` and so on). The script ends
by printing `DATE_QUALITY_SUMMARY`.

The NSFG file is saved in the Mexican folder on purpose: the analysis scripts
load both files from the same place.


## 1.7 Checking and loading the result

```r
source("enadid_lib.R")
loadENADID_data()                              # loads MEXICO_ENADID and NSFG_ENADID
table(MEXICO_ENADID$survey)                    # one count per survey, see docs/02
summarizeDateQuality(MEXICO_ENADID)            # what the date filter would remove
MEX <- filterDateQuality(MEXICO_ENADID)        # the version every analysis uses
```

`filterDateQuality()` has three independent switches: `dropBadUnionDates` (on
by default), `dropBackfilledEnd` and `dropBadBirthDates` (both off).
Impossible ages are always removed. `dropAllBad = TRUE` does not mean what its
name suggests: it sets the first switch on and the other two off, which is the
default call.

`summarizeDateQuality()` reports, without removing anything, one row per
survey and a total, with the columns `nWomen`, `removed_union`,
`corrected_union`, `removed_birth` and `removed_bad_age`.

The two frames can be stacked, since they share their column names and
conventions:

```r
pooled <- dplyr::bind_rows(MEXICO_ENADID, NSFG_ENADID)
```


## 1.8 Common problems

| Symptom | Cause and remedy |
|---|---|
| `cannot open file '.../ENADID/2018/...csv'` | The file is not where the reader expects it. Compare the path in the message with section 1.4, or set `mexicoRoot` in `config_local.R`. |
| `object 'MEXICO_ENADID' not found` in an analysis script | The data frames have not been built yet, or not loaded. Run section 1.6, or `loadENADID_data()`. |
| `rstudioapi::getActiveDocumentContext()` fails | The script was run outside RStudio. Open it in RStudio and click Source. |
| The paths in `config_local.R` seem ignored | R was not restarted after the file was edited. |
| `harm_union_type()` stops on an unknown label | A reader produced a union-type wording that is not yet in `UNION_TYPE_ALIASES` (`lib/unionType_lib.R`). Add it there; the stop is deliberate. |
