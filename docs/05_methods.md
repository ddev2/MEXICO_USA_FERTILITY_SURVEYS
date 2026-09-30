# 5. Methods

This page explains the estimators used in the figures, what each curve
measures, and why the choices were made. The code references are to
`lib/KaplanMeierLib.R`, `lib/unionEpisodes.R`, `lib/mirroredCurve.R` and
`lib/lifeCourseAJ_lib.R`.


## 5.1 Pooled surveys, cohorts and periods

Each country's surveys are stacked into one file (`docs/02_common_format.md`).
A woman interviewed in 2018 reports unions that began in the 1990s; a woman
interviewed in 1997 reports unions of the 1970s. Pooling therefore gives, for
every cohort of unions and every calendar year, the experience of all the
women who lived it, whatever the survey that recorded it. It also averages
out the dating errors of any single survey (Figures 11 to 16 compare each
survey with the pooled result).

Two weights are used (`docs/02_common_format.md`, section 2.3). `popWeight`
gives each survey the weight of the population it represents, and is used for
the point estimates of the cohort figures. `weight`, which has mean 1 in every
survey, is used by the period life tables.

The results are shown as curves of change over time, by cohort or by period,
rather than as tables of coefficients from statistical models.


## 5.2 Kaplan-Meier: net probabilities

For a transition such as "first union to separation", the Kaplan-Meier
estimator follows each union from its start until the event (separation) or
until observation stops (censoring), and estimates the probability of not yet
having experienced the event at each duration:

S(t) = product over event times t_i <= t of (1 - d_i / n_i),

where d_i is the weighted number of events at t_i and n_i the weighted number
of unions still at risk just before it (Kaplan and Meier 1958).
`KaplanMeier()` computes it with `survival::survfit()`, with log-log
confidence bounds and the infinitesimal jackknife variance, which is valid for
survey weights. The intervals do not account for the strata and clusters of
the survey designs.

When a **competing event** (widowhood, for separation) is treated as
censoring, the curve estimates a **net** probability: the probability of
separating in a hypothetical population in which partners did not die. This
is the associated single-decrement probability of the classic multiple
decrement life table (Chiang 1968; Preston, Heuveline and Guillot 2001,
chapter 4). It rests on the assumption that the competing event is independent
of the event of interest, an assumption the data cannot test (Tsiatis 1975).

Net measures are used in the paper where the assumption is reasonable:

- separation, with widowhood censored (Figures 3 and 4): a partner's death has
  little to do with the couple's propensity to separate, and censoring it
  removes a difference in male mortality between the two countries;
- transition to marriage, with the end of the union censored (Figures 1 and 2);
- re-partnering (Figure 7).


## 5.3 Aalen-Johansen: crude probabilities

When the competing event is frequent and linked to the event of interest, the
net probability describes a population that cannot exist. The clearest case
here is the separation of cohabitations: censoring at marriage asks what would
happen if cohabiting couples could never marry. The couples who marry are
those least likely to separate, so the independence assumption fails and the
net curve overstates separation. (In a simulation, by 83 per cent at ten
years.)

The **crude** probability, the share of unions that actually reach each
outcome when all outcomes compete, is estimated by the Aalen-Johansen
estimator (Aalen and Johansen 1978; Putter, Fiocco and Geskus 2007). For a
single starting state it is the cumulative incidence function; for several
states it gives the probability of being in each state at each duration, and
these probabilities add up to one.

In the code, a first union is split into **episodes**, one per state it goes
through (`buildUnionEpisodes()` in `lib/unionEpisodes.R`):

```
cohabiting --> married (converted) --> separated or widowed
    |
    +--> separated or widowed
married (direct) --> separated or widowed
```

Marriage is then an intermediate state, not an exit; only the survey date is
censoring. `unionStateOccupancy()` and `unionCompetingRisks()` fit the model
with `survival::survfit()` on these episodes.

Figures built this way: C6, C7 and C8 in `MEX_USA_figures_cohort.R`, the life
course from age 15 to 45 (`MEX_USA_lifecourse_AJ.R`, where the clock is age
rather than union duration, so that single women and women in union share it),
and the child figures corrected with a stratified Aalen-Johansen estimator
(`lib/childUnionContext.R`).

For all first unions and widowhood, the two estimators give almost the same
curve, because widowhood is rare in the first thirty years of a union (0.67
and 0.68 not separated after 30 years, Mexico). For cohabitations and
marriage they do not (0.57 and 0.68 after 20 years). `presentation_KM_AJ.R`
draws the two comparisons.


## 5.4 The "mirrored" curve (formerly "mirrored Kaplan-Meier")

This figure shows the timing of one event relative to another, for example
the first birth relative to the first union. Time 0 is the first of the two
events. The right half follows the women whose first event was `varEvent` and
shows how long they wait for `varEvent2`; the left half follows those whose
first event was `varEvent2`, drawn on a reversed axis. The figure was
introduced in the demographic literature as a "mirrored Kaplan-Meier"
(Billari 2001).

**The name no longer describes the estimate**, which is why the functions were
renamed in September 2026 (`mirroredCurve()`, `mirroredCurveBootstrap()`,
`mirroredCurvePlot()` in `lib/mirroredCurve.R`; the old names
`KaplanMeierSurvfit()`, `KaplanMeierBootstrap()` and
`KaplanMeierPlot(..., varEvent2 = ...)` still work). Each half is the product
of two pieces:

- the probability that a given event comes first, by a horizon tau: an
  **Aalen-Johansen cumulative incidence**, with the two possible first events
  as competing causes;
- the probability of not yet having experienced the second event, t months
  after the first: an ordinary **Kaplan-Meier** curve with the clock reset at
  the first event.

```
right half, at t:   P(event first by tau) x P(no event2 within t | event first)
left half,  at -t:  1 - P(event2 first by tau) x P(no event within t | event2 first)
```

Only the second piece is a Kaplan-Meier estimate. The older construction,
still available as `KaplanMeierPlot(..., estimator = "classic")`, scaled the
halves by the raw observed shares of women with each order of events; those
shares are biased downward as soon as some women are censored before either
event, which is why the Aalen-Johansen probabilities replaced them.

**Billari's population.** Billari (2001) divides the counts by the number of
persons with at least one event before a fixed age A, in cohorts whose members
have all reached A. `mirroredCurve(..., horizon = A, conditional = TRUE)` gives
the same shares (checked: identical to four decimals for Mexico, 1950-59,
A = 30) and stays valid for cohorts in which some women are younger than A,
because the Aalen-Johansen probabilities are divided by 1 - P(neither by A).
The classic construction divided by all women, with no common age, which is
where the old figures departed from Billari. `MEX_USA_figures_mirrored.R`
draws both versions (`MIRROR_AT_LEAST_ONE`).

Three points belong in the caption of any such figure:

- **the horizon tau** at which the two probabilities are read (by default the
  largest observed time to the first event; returned in
  `attr(result, "horizon")`);
- **the gap at time 0**, the share of women with neither event, or with both
  in the same month (`attr(result, "gapAtZero")`, and the `ties` argument);
- **the reading of the halves**: which half a woman belongs to depends on the
  order of her two events, which is not known at time 0. The halves describe
  the groups defined by that order, not a cohort followed forward in time
  (Hoem and Kreyenfeld 2006).

The analytic intervals are conditional on the two branch probabilities; for
intervals that include their sampling error and the survey design, use
`mirroredCurveBootstrap()` (or `KaplanMeierPlot(..., bootstrap = 500)`).
`tests/tests_mirroredCurve.R` checks the construction on simulated data.


## 5.5 Period indicators: `ppr_doIt()`

The period figures (`MEX_USA_figures_period.R`) apply to union transitions the
method of period parity progression ratios used for fertility (Feeney and Yu
1987). For a transition A to B, and for each calendar year:

1. women who entered state A in each earlier year are grouped by the year of
   entry (the rows of a Lexis table), and their events and exposure in the
   current year give a probability of transition at each duration since entry
   (or at each age, when entry is birth: `duration = FALSE`);
2. these probabilities are chained into a synthetic cohort, which gives the
   probability of ever reaching B under that year's rates (the "quantum") and
   the mean duration (or age) at the event.

The results are smoothed across years with a loess curve (`mySpan`). The
confidence band comes from a bootstrap of the women (200 replicates by
default, `replicates`), or from the Greenwood variance when `replicates = 0`.
Competing events (widowhood) remove the woman from the risk set, so the
period indicators are net probabilities. `ppr_cr_doIt()` in
`lib/pprCompetingRisks.R` gives the crude version.

**Age limit.** Most NSFG cycles stop at age 44, so women over 40 are seen only
in the most recent years of a survey. Events after an age limit are not
counted (`ageTruncate = 40`, and 45 for Figure 17); Figure 10 shows the
maximum age present in each year.


## 5.6 Confidence intervals

Three variance methods are used, chosen by the estimator:

| Estimate | Interval |
|---|---|
| Kaplan-Meier cohort curves (`KaplanMeier()`, `useSurvfit = TRUE`) | infinitesimal jackknife, from `survival::survfit()` |
| Aalen-Johansen curves (`unionStateOccupancy()`, `unionCompetingRisks()`) | infinitesimal jackknife, from `survival::survfit()` |
| Period indicators (`ppr_doIt()`) | bootstrap of the women, 200 replicates (Greenwood when `replicates = 0`) |
| Children, stratified and standardised (`childStratifiedAJ()`) | bootstrap |
| Mirrored curve | analytic, conditional on the branch probabilities; bootstrap with `mirroredCurveBootstrap()` |

**Why not Greenwood for the weighted Kaplan-Meier curves.** Greenwood's
formula treats each weight as a number of people. With `popWeight`, where one
woman represents thousands, the formula believes the sample is enormous and
the interval is far too narrow; with `weight` (mean 1) the scale problem
disappears, but the loss of precision caused by unequal weights is still
ignored. The infinitesimal jackknife does not depend on the scale of the
weights and accounts for their inequality. `tests/tests_kaplanMeier.R` checks
both properties: multiplying every weight by 10 changes the Greenwood standard
error and leaves the jackknife one unchanged. Greenwood remains available
(`useSurvfit = FALSE`) and is correct for unweighted data. The change was made
for the weights; the Aalen-Johansen estimator did not require it.

**What the infinitesimal jackknife is.** Despite its name, it deletes no
observation. It measures the influence of each woman on the estimate (how much
the estimate moves when her weight changes slightly) and sums the squared
influences. It is the limit of the delete-one jackknife and, for survey data,
the same as the Taylor linearization (sandwich) variance (Efron 1982; Lumley
2010). It is computed once, is the same at every run, and for smooth
estimators such as Kaplan-Meier and Aalen-Johansen gives practically the same
interval as the bootstrap.

**When the bootstrap is used instead.** For estimators that are complicated
functions of the data, with no simple variance formula: the period life table,
whose quantum is smoothed by loess; the child estimate, which is stratified and
then standardised; the product of two estimated pieces in the mirrored curve.

**The survey design.** Neither method, as used by default, accounts for the
strata and clusters of the samples. `survfit()` accepts clusters through its
`cluster` argument, and the bootstraps can resample primary sampling units
within strata (`varStrata`, `varCluster`) once those variables are carried in
the data frames.


## 5.7 Date quality

Women whose union dates cannot be used (unknown year, a date after the
interview, dates out of order) are removed by `filterDateQuality()` before
every analysis; missing months are imputed within the constraints set by
`capMonth` (`docs/01_build_data.md`, `docs/02_common_format.md`).


## 5.8 References

- Aalen, O. O. and Johansen, S. (1978). An empirical transition matrix for
  non-homogeneous Markov chains based on censored observations. *Scandinavian
  Journal of Statistics* 5(3): 141-150.
- Billari, F. C. (2001). The analysis of early life courses: complex
  descriptions of the transition to adulthood. *Journal of Population
  Research* 18(2): 119-142.
- Chiang, C. L. (1968). *Introduction to Stochastic Processes in Biostatistics.*
  New York: Wiley.
- Efron, B. (1982). *The Jackknife, the Bootstrap and Other Resampling
  Plans.* Philadelphia: SIAM.
- Efron, B. and Tibshirani, R. J. (1993). *An Introduction to the Bootstrap.*
  New York: Chapman and Hall.
- Feeney, G. and Yu, J. (1987). Period parity progression measures of
  fertility in China. *Population Studies* 41(1): 77-102.
- Hoem, J. M. and Kreyenfeld, M. (2006). Anticipatory analysis and its
  alternatives in life-course research. *Demographic Research* 15.
- Kaplan, E. L. and Meier, P. (1958). Nonparametric estimation from incomplete
  observations. *Journal of the American Statistical Association* 53(282):
  457-481.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R.* Hoboken:
  Wiley.
- Preston, S. H., Heuveline, P. and Guillot, M. (2001). *Demography: Measuring
  and Modeling Population Processes.* Oxford: Blackwell. Chapter 4.
- Putter, H., Fiocco, M. and Geskus, R. B. (2007). Tutorial in biostatistics:
  competing risks and multi-state models. *Statistics in Medicine* 26(11):
  2389-2430.
- Therneau, T. M. and Grambsch, P. M. (2000). *Modeling Survival Data:
  Extending the Cox Model.* New York: Springer.
- Tsiatis, A. (1975). A nonidentifiability aspect of the problem of competing
  risks. *Proceedings of the National Academy of Sciences* 72(1): 20-22.
- Wolter, K. M. (2007). *Introduction to Variance Estimation*, 2nd ed. New
  York: Springer.
