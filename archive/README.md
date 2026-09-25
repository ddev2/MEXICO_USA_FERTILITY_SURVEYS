# archive/

Code that is no longer used by the analyses, kept so that earlier results can
be reproduced and the reasons for each change can be read. Nothing outside
this folder sources these files.

| File | What it was | Why it was retired |
|---|---|---|
| `KaplanMeier_marriage_censored.R` | Separation of cohabiting unions with marriage treated as censoring (Figure 6 as first drafted), Mexico, USA and combined. Moved here from three `if (!WRONG)` blocks of `KaplanMeier.R`. | Censoring at marriage assumes that marrying is unrelated to separating, which is false; the curve overstates separation. Replaced by block C6 of `MEX_USA_figures_cohort.R` (Aalen-Johansen). See `docs/05_methods.md`, section 5.3. When it was archived, one error was corrected: the US block took the union year from another data frame. |
| `KaplanMeierMstate.R` | First version of the mirrored two-event curve, built on the `mstate` package. | Replaced by `lib/mirroredCurve.R`, which computes the same quantities with `survival` alone and one dependency fewer. |
| `tests_kaplanMeierMstate.R` | Its tests. | As above. |
| `tests_kaplanMeier_old_copy.R` | An older copy of `tests/tests_kaplanMeier.R`. | Superseded by the current test file. |
| `ReadEDER2017_old.R` | The first reader of the EDER 2017 survey. | Replaced by `ReadEDER2017.R`. |

The scripts here were not updated when the rest of the code changed. They
may need small path edits to run: work from the repository root, and
`source("enadid_lib.R")` first.
