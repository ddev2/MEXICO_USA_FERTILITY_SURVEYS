# NSFG Validation Project

## Purpose

Independent reimplementation of the 12 NSFG survey cycles (1973–2022-23) for
cross-validation against the original `NSFG_import.R` code. The goal is to
produce an **identical output schema** so results can be compared row-by-row
using `NSFG_val_compare.R`.

---

## Folder structure

```
NSFG_validation/
├── README.md
├── NSFG_val_lib.R       # shared helpers (CMC, schema, clean, order)
├── NSFG_val_import.R    # one getDatos_XXXX_val() per cycle
├── NSFG_val_compare.R   # comparison functions
└── codebooks/           # copy PDF codebooks here before running each cycle
    ├── 1973_codebook.pdf
    ├── 1976_codebook.pdf
    ├── 1982_codebook.pdf
    ├── 1988_codebook.pdf
    ├── 1995_codebook.pdf
    ├── 2002_codebook.pdf
    ├── 2006_10_codebook.pdf
    ├── 2011_13_codebook.pdf
    ├── 2013_15_codebook.pdf
    ├── 2015_17_codebook.pdf
    ├── 2017_19_codebook.pdf
    └── 2022_23_codebook.pdf
```

---

## Workflow for each cycle

1. Copy the PDF codebook into `codebooks/`
2. Upload it to a Claude session
3. Ask Claude to complete the `getDatos_XXXX_val()` stub in `NSFG_val_import.R`,
   reading all byte positions from the PDF
4. Run the function and `val_clean()`
5. Compare with the original using `NSFG_val_compare.R`
6. Investigate any discrepancies — they indicate either a bug in the original
   or a misread position in the validation

---

## Output schema (all cycles must produce these columns)

### Fixed columns

| Column | Type | Notes |
|---|---|---|
| country | character | always "USA" |
| survey | character | e.g. "NSFG1973" |
| CaseID | integer | |
| surveyDate_cmc | integer | CMC of interview date |
| lastYear | integer | min survey year in cycle - 1 |
| indiv_dob_cmc | integer | CMC of date of birth |
| indiv_dob_cmc_I | integer | 0=exact, 1=month imputed, 10=year imputed |
| yBirth | integer | calendar year of birth |
| indiv_age_survey | integer | age in completed years at interview |
| indiv_weight | integer | survey weight |
| union_status | factor | current marital/union status at interview |
| pregnant | factor | "yes"/"no"/"refused"/"unknown" |
| want_another | factor | "yes"/"no"/"disagree"/"refused"/"unknown" |
| prob_want_another | factor | "probably yes"/"probably no"/"refused"/"unknown" |
| age_first_sex | integer | age at first sexual intercourse (NA if not collected) |
| ever_contraception | integer | NA if not collected |
| nUnion | integer | total number of unions (marriages + cohabitations) |
| nBioKids | integer | number of live births |

### Union history columns (N = 1 to max_unions in cycle)

| Column | Type | Notes |
|---|---|---|
| union_start_typeN | factor | "marriage" / "cohabitation" / "cohabitation before marriage" |
| union_start_cmcN | integer | CMC of union start (cohabitation start if applicable) |
| union_start_cmc_IN | integer | imputation flag |
| marriage_start_cmcN | integer | CMC of marriage (NA if union is cohabitation only) |
| marriage_start_cmc_IN | integer | imputation flag |
| union_end_cmcN | integer | CMC of union end (NA if ongoing) |
| union_end_cmc_IN | integer | imputation flag |
| union_end_motiveN | factor | "in union" / "widowhood" / "separation" / "unknown" |

### Birth history columns (N = 1 to max_births in cycle)

| Column | Type | Notes |
|---|---|---|
| dob_cmcN | integer | CMC of Nth child's date of birth |
| dob_cmc_IN | integer | imputation flag |
| sexN | integer | 1=male, 2=female (NA if not collected or randomised) |
| dod_cmcN | integer | CMC of Nth child's date of death (NA if child still living) |
| dod_cmc_IN | integer | imputation flag for date of death (NA if child still living) |

`dod_cmcN` and `dod_cmc_IN` follow the same ordering as `dob_cmcN`: child 1
is the chronologically earliest live birth, so `dod_cmc1` is the death date
of that same child, not necessarily the first child to die.

---

## Coding conventions (independent of original)

- CMC formula: `(year - 1900) * 12 + month`
- Missing year → 9999 (treated as NA by `val_clean()`)
- Random month imputation: always `rand_month(n)` — one draw per row, never scalar
- Union ordering: always chronological (earliest start = union 1)
- Birth ordering: always chronological (earliest birth = birth 1); `dod_cmc`
  columns are reordered alongside `dob_cmc` by `val_reorder_births()`
- Unions ongoing at survey date: `union_end_motive = "in union"`, `union_end_cmc = NA`
- Child still living at survey date: `dod_cmc = NA`, `dod_cmc_I = NA`
- Imputation flag values: 1 for missing or out of range month, 10 for missing or out of range year, 11 for both, 0 for correct values 

---

## CMC coding by cycle

| Cycle | Format | Missing month convention | Missing year |
|---|---|---|---|
| 1973 | FWF .dat | 901–996: code−900 = year offset | 997–999 → 9999 |
| 1976 | FWF .dat | 901–924 (close event): keep; 925–976: random month | 997–999 → 9999 |
| 1982 | FWF .dat | 9400–9997: code−9000 = cmc base | 9797/9898/9999 → 9999 |
| 1988 | FWF .dat (via SPS) | 5-digit CMC, special codes per codebook | per codebook |
| 1995–2013-15 | FWF .dat (via SPS) | 4-digit CMC, special codes per codebook | per codebook |
| 2015-17 onwards | SAS .sas7bdat | Year only (no month CMC in public file) | per codebook |
| 2022-23 | SAS .sas7bdat | Age + survey CMC only for DOB | n/a |
