setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.r")
library(tidyverse)

# Prototype: state-occupancy "life expectancy" (years lived in each union /
# birth state between ageStart and ageEnd) from a pooled ENADID-type dataset
# (MEXICO_ENADID, NSFG_ENADID, or bind_rows(MEXICO_ENADID, NSFG_ENADID)).
#
# States (7), each split by whether the woman has had >=1 live birth so far
# (14 cells in total):
#   1  out_union            before her first union ever starts
#   2  union1_cohab         living in her first union, cohabiting (not yet married)
#   3  union1_married       living in her first union, married
#   4  sep1                 separated/widowed from her first union, not yet repartnered
#   5  union2plus_cohab     living in a second-or-later union, cohabiting
#   6  union2plus_married   living in a second-or-later union, married
#   7  sep2plus             separated/widowed from a second-or-later union, not yet repartnered
#
# The cohab/married split is WITHIN a union spell: a union that starts as
# cohabitation and later converts to marriage ("cohabitation before marriage",
# union_start_type1) contributes months to BOTH union1_cohab and
# union1_married, split at marriage_start_cmc{u}. A union that starts directly
# as a marriage contributes only to *_married; a union with no marriage_start_cmc{u}
# at all (never married) contributes only to *_cohab.
#
# State is otherwise defined purely from union_start_cmc{u} / union_end_cmc{u}
# spells -- union_end_motive (separation vs widowhood) is NOT used to
# distinguish sep1/sep2plus, since the requested categories only need
# "separated", not why. The motive columns (union_end_motive{u}) are still in
# the input data if you later want to split widowhood out as its own state.
#
# Because we have no mortality information (only interviewed, i.e. surviving,
# women), this is NOT a standard multistate life table: it is the observed
# split of a FIXED 25-year window (ageStart to ageEnd) among women who lived
# to at least ageEnd, so the 14 state x birth-status years always sum to
# exactly (ageEnd - ageStart) for a fully-observed woman, and the weighted
# average across women also sums to (ageEnd - ageStart) -- see the "check"
# value returned by state_life_expectancy().
#
# ASSUMPTIONS / prototype caveats (verify before trusting results):
#   - Input should already be cleaned: cleanENADID() + filterDateQuality().
#     This function does not repair bad dates.
#   - union_start_cmc{u} slots are assumed chronological (u=1 is the first
#     union, u=2 the second, ...). This holds for surveys read with
#     hasFullUnionHistory = TRUE. Surveys that only capture the first and last
#     union in detail will under-count intermediate union/separation spells --
#     check ordersPresent(df, "union_start_cmc") against df$nUnion per survey
#     before pooling if this matters for your analysis.
#   - A union spell covers months [union_start_cmc, union_end_cmc); i.e. the
#     end month itself already counts as "separated". Within the spell, months
#     before marriage_start_cmc{u} count as cohab, months at/after count as
#     married. Adjust in .womanStateMonths() if your convention is different.
#   - If marriage_start_cmc{u} is reported OUTSIDE [union_start_cmc{u},
#     union_end_cmc{u}) (a data-quality issue that filterDateQuality/cleanENADID
#     should normally have caught), the spell falls back to all-cohab (if the
#     marriage date is at/after the union end) or all-married (if at/before the
#     union start) -- it does not error.
#   - Only women observed through at least ageEnd (surveyDate_cmc >= dob +
#     ageEnd*12) are used by default (requireFullObs = TRUE), so every
#     included woman's 15-40 window is fully retrospective, not censored.


# ==== 1. Per-woman state-month accumulator ====

STATE_LABELS <- c("out_union", "union1_cohab", "union1_married", "sep1",
                  "union2plus_cohab", "union2plus_married", "sep2plus")
STATE_CELL_NAMES <- as.vector(outer(STATE_LABELS, c("no_births", "with_births"), paste, sep = "."))

# us, ue, ms: numeric vectors of union_start_cmc / union_end_cmc / marriage_start_cmc,
#         same length and order as unionOrders (position i = union order
#         unionOrders[i]); ue may be NA (union still ongoing at the survey),
#         ms may be NA (that union was never a marriage, cohab throughout).
# unionOrders: integer vector of the actual union-order numbers behind us/ue/ms
#         (e.g. c(1,2,3)), so "is this her first union" is judged on the real
#         order number, not just vector position.
# dob:    numeric vector of dob_cmc for all live births (NA where no birth).
# winStart, winEnd: cmc bounds of the observation window (ageStart, ageEnd).
# Returns a named numeric vector of length 14 (months), one per
# state x birth-status cell, e.g. "union1_married.with_births".
.womanStateMonths <- function(us, ue, ms, unionOrders, dob, winStart, winEnd) {

  out <- setNames(numeric(length(STATE_CELL_NAMES)), STATE_CELL_NAMES)

  if (is.na(winStart) || is.na(winEnd) || winEnd <= winStart) return(out)

  # "unions started by time t" must count EVERY union she has ever entered,
  # including one that started (and even ended) before winStart -- e.g. a
  # union at age 12-14 still means she enters the 15-40 window already
  # separated-from-first-union, not out_union. So this uses the RAW us
  # (unclipped, not restricted to unions overlapping the window).
  us_all <- us

  # clip union spells to the observation window; drop unions that don't
  # overlap it at all (started at/after winEnd, or already fully over by
  # winStart) -- those still count in us_all above, just not as a segment.
  us_c <- pmax(us, winStart)
  ue_c <- ifelse(is.na(ue), winEnd, pmin(ue, winEnd))
  validUnion <- !is.na(us) & (us < winEnd) & (ue_c > winStart)

  us_c   <- us_c[validUnion]
  ue_c   <- ue_c[validUnion]
  ms_c   <- ms[validUnion]              # marriage start for these clipped spells (NA = never married)
  ordVal <- unionOrders[validUnion]     # real union-order number per clipped spell

  dob_c <- dob[!is.na(dob) & dob >= winStart & dob < winEnd]

  # marriage-start breakpoints: only where the cohab -> married transition
  # actually falls strictly inside the clipped spell; ms at/before the spell
  # start means "married from the start" (no extra breakpoint needed), ms
  # at/after the spell end falls back to all-cohab (see file header).
  ms_break <- ms_c[!is.na(ms_c) & ms_c > us_c & ms_c < ue_c]

  breaks <- sort(unique(c(winStart, winEnd, us_c, ue_c, ms_break, dob_c)))
  breaks <- breaks[breaks >= winStart & breaks <= winEnd]
  if (length(breaks) < 2L) return(out)

  for (j in seq_len(length(breaks) - 1L)) {
    t   <- breaks[j]
    dur <- breaks[j + 1L] - t
    if (dur <= 0) next

    inUnion <- (us_c <= t) & (t < ue_c)
    if (any(inUnion)) {
      k       <- which(inUnion)[1L]
      ord     <- ordVal[k]
      married <- !is.na(ms_c[k]) && (t >= ms_c[k])
      base    <- if (ord == 1L) "union1" else "union2plus"
      state   <- paste0(base, if (married) "_married" else "_cohab")
    } else {
      startedSoFar <- sum(us_all <= t, na.rm = TRUE)
      state <- if (startedSoFar == 0L) "out_union"
      else if (startedSoFar == 1L) "sep1"
      else "sep2plus"
    }

    hasBirth <- any(dob_c <= t)
    col <- paste0(state, ".", if (hasBirth) "with_births" else "no_births")
    out[col] <- out[col] + dur
  }

  out
}


# ==== 2. Build the month matrix for a whole (pre-filtered) dataframe ====

# Returns a list(months = n x 14 matrix (months per state x birth-status),
#                df     = the matching filtered rows of the input df).
build_state_months <- function(df, ageStart = 15, ageEnd = 40,
                                maxUnionOrder = 7L, maxBirths = 25L,
                                requireFullObs = TRUE, verbose = TRUE) {

  unionOrders <- intersect(ordersPresent(df, "union_start_cmc"), seq_len(maxUnionOrder))
  birthOrders <- intersect(ordersPresent(df, "dob_cmc"),         seq_len(maxBirths))
  if (length(unionOrders) == 0L) stop("No union_start_cmc{u} columns found in df")

  usCols  <- paste0("union_start_cmc",    unionOrders)
  ueCols  <- paste0("union_end_cmc",      unionOrders)
  msCols  <- paste0("marriage_start_cmc", unionOrders)
  dobCols <- paste0("dob_cmc", birthOrders)
  ueCols <- ifelse(ueCols %in% names(df), ueCols, NA_character_)  # tolerate missing end col
  msCols <- ifelse(msCols %in% names(df), msCols, NA_character_)  # tolerate missing marriage col

  winStart <- df$indiv_dob_cmc + ageStart * 12L
  winEnd   <- df$indiv_dob_cmc + ageEnd   * 12L

  keep <- !is.na(df$indiv_dob_cmc)
  if (isTRUE(requireFullObs)) {
    keep <- keep & !is.na(df$surveyDate_cmc) & (df$surveyDate_cmc >= winEnd)
  }
  if (verbose) {
    cat(sum(!keep), "of", nrow(df),
        "women dropped (missing dob, or not yet observed through age", ageEnd, ")\n")
  }

  dfK      <- df[keep, , drop = FALSE]
  winStart <- winStart[keep]
  winEnd   <- winEnd[keep]

  usMat  <- as.matrix(dfK[, usCols, drop = FALSE])
  ueMat  <- matrix(NA_real_, nrow = nrow(dfK), ncol = length(ueCols))
  haveUe <- !is.na(ueCols)
  if (any(haveUe)) ueMat[, haveUe] <- as.matrix(dfK[, ueCols[haveUe], drop = FALSE])
  msMat  <- matrix(NA_real_, nrow = nrow(dfK), ncol = length(msCols))
  haveMs <- !is.na(msCols)
  if (any(haveMs)) msMat[, haveMs] <- as.matrix(dfK[, msCols[haveMs], drop = FALSE])
  dobMat <- as.matrix(dfK[, dobCols, drop = FALSE])

  n <- nrow(dfK)
  months <- matrix(0, nrow = n, ncol = length(STATE_CELL_NAMES),
                    dimnames = list(NULL, STATE_CELL_NAMES))

  for (i in seq_len(n)) {
    months[i, ] <- .womanStateMonths(usMat[i, ], ueMat[i, ], msMat[i, ], unionOrders,
                                      dobMat[i, ], winStart[i], winEnd[i])
  }

  list(months = months, df = dfK)
}


# ==== 3. Weighted summary: years per state x birth-status ====

# df:  pooled ENADID-type dataframe (already cleaned / filterDateQuality'd)
state_life_expectancy <- function(df, ageStart = 15, ageEnd = 40,
                                   weightVar = "popWeight",
                                   groupVars = "country",
                                   maxUnionOrder = 7L, maxBirths = 25L,
                                   requireFullObs = TRUE, verbose = TRUE) {

  built  <- build_state_months(df, ageStart, ageEnd, maxUnionOrder, maxBirths,
                                requireFullObs, verbose)
  months <- built$months
  dfK    <- built$df

  w <- if (weightVar %in% names(dfK)) dfK[[weightVar]] else rep(1, nrow(dfK))
  w[is.na(w)] <- 0

  grp <- if (is.null(groupVars) || length(groupVars) == 0L) {
    rep("all", nrow(dfK))
  } else {
    do.call(paste, c(as.list(dfK[, groupVars, drop = FALSE]), sep = " | "))
  }

  years <- as.data.frame(months / 12)
  years$w   <- w
  years$grp <- grp

  stateCols <- setdiff(names(years), c("w", "grp"))

  out <- do.call(rbind, lapply(split(years, years$grp), function(sub) {
    wsum <- sum(sub$w)
    means <- vapply(stateCols, function(cc) {
      if (wsum == 0) return(NA_real_)
      sum(sub[[cc]] * sub$w) / wsum
    }, numeric(1))
    data.frame(grp = sub$grp[1L], nWomen = nrow(sub), sumWeight = wsum,
               t(means), check.names = FALSE)
  }))
  rownames(out) <- NULL
  names(out)[1] <- if (is.null(groupVars)) "group" else paste(groupVars, collapse = "_")

  out$check_total_years <- rowSums(out[, stateCols, drop = FALSE])
  if (verbose) {
    cat("check_total_years should equal", ageEnd - ageStart, "for every row\n")
  }

  out
}


# ==== 4. Roll-ups ====

# Collapse with/without births only -- keep the 7 union/cohab/married states.
state_life_expectancy_7state <- function(out14) {
  base <- setdiff(names(out14), c(STATE_CELL_NAMES, "check_total_years"))
  res <- out14[, base, drop = FALSE]
  for (s in STATE_LABELS) {
    res[[s]] <- out14[[paste0(s, ".no_births")]] + out14[[paste0(s, ".with_births")]]
  }
  res$check_total_years <- rowSums(res[, STATE_LABELS, drop = FALSE])
  res
}

# Collapse both birth-status AND cohab/married -- reproduces the original
# 5-state headline view (out_union, union1, sep1, union2plus, sep2plus).
FIVE_STATE_LABELS <- c("out_union", "union1", "sep1", "union2plus", "sep2plus")

state_life_expectancy_5state <- function(out14) {
  base <- setdiff(names(out14), c(STATE_CELL_NAMES, "check_total_years"))
  res <- out14[, base, drop = FALSE]
  res$out_union  <- out14[["out_union.no_births"]]  + out14[["out_union.with_births"]]
  res$union1     <- out14[["union1_cohab.no_births"]]     + out14[["union1_cohab.with_births"]] +
    out14[["union1_married.no_births"]]   + out14[["union1_married.with_births"]]
  res$sep1       <- out14[["sep1.no_births"]] + out14[["sep1.with_births"]]
  res$union2plus <- out14[["union2plus_cohab.no_births"]]   + out14[["union2plus_cohab.with_births"]] +
    out14[["union2plus_married.no_births"]] + out14[["union2plus_married.with_births"]]
  res$sep2plus   <- out14[["sep2plus.no_births"]] + out14[["sep2plus.with_births"]]
  res$check_total_years <- rowSums(res[, FIVE_STATE_LABELS, drop = FALSE])
  res
}

# ==== PART B -- age-specific state counts, by birth cohort, ALL women ====
#
# Part A above only uses women already observed through ageEnd (a "completed
# cohort" summary that always sums to 25 years). This part instead:
#   - keeps every woman, right-censored at her own survey date (so recent
#     birth cohorts contribute only up to whatever age they'd reached by
#     interview -- older ages in those cohorts just have fewer women behind
#     them, same as any retrospective survey);
#   - stratifies by birth cohort (yBirth, grouped into cohortWidth-year bins)
#     as well as by groupVars (default "country"), instead of pooling every
#     birth cohort together;
#   - returns counts (weighted AND unweighted) for each of the 14 states at
#     EVERY SINGLE MONTH of age from ageStart to ageEnd, plus the total number
#     of women observed at that exact age (N_total) -- the denominator you'd
#     divide by to get a Sullivan-style age-specific prevalence, and also your
#     sample-size/attrition check by age within each cohort.
#
# N_total(age) falls off as age rises within a cohort still being fielded by
# more recent surveys (women not yet that old at interview drop out of the
# denominator for ages beyond their own survey age) -- that decline IS the
# censoring pattern, not a bug; use it to judge how far into each cohort's
# age range the estimates are trustworthy.


# ==== 6. Per-woman state segments, in age-months (not calendar cmc) ====

# us, ue, ms, dob_births are already expressed as AGE IN MONTHS (cmc minus the
# woman's own dob_cmc) by the caller (build_state_segments), so ageStartM is a
# constant across all women and only winEndAge (this woman's own censoring
# age -- min(ageEndM, age at survey)) varies.
# Returns a data.frame(state, age_start, age_end) -- an exhaustive, disjoint
# partition of [ageStartM, winEndAge) in age-months, 0 rows if winEndAge is
# at/before ageStartM (nothing observed in-window for this woman).
.womanStateSegments <- function(us, ue, ms, unionOrders, dob_births, ageStartM, winEndAge) {

  empty <- data.frame(state = character(0), age_start = integer(0), age_end = integer(0))
  if (is.na(winEndAge) || winEndAge <= ageStartM) return(empty)

  us_all <- us   # unclipped, for "unions started by age m" -- see Part A note

  us_c <- pmax(us, ageStartM)
  ue_c <- ifelse(is.na(ue), winEndAge, pmin(ue, winEndAge))
  validUnion <- !is.na(us) & (us < winEndAge) & (ue_c > ageStartM)

  us_c   <- us_c[validUnion]
  ue_c   <- ue_c[validUnion]
  ms_c   <- ms[validUnion]
  ordVal <- unionOrders[validUnion]

  dob_c <- dob_births[!is.na(dob_births) & dob_births >= ageStartM & dob_births < winEndAge]

  ms_break <- ms_c[!is.na(ms_c) & ms_c > us_c & ms_c < ue_c]

  breaks <- sort(unique(c(ageStartM, winEndAge, us_c, ue_c, ms_break, dob_c)))
  breaks <- breaks[breaks >= ageStartM & breaks <= winEndAge]
  if (length(breaks) < 2L) return(empty)

  nSeg   <- length(breaks) - 1L
  states <- character(nSeg)

  for (j in seq_len(nSeg)) {
    t <- breaks[j]

    inUnion <- (us_c <= t) & (t < ue_c)
    if (any(inUnion)) {
      k       <- which(inUnion)[1L]
      ord     <- ordVal[k]
      married <- !is.na(ms_c[k]) && (t >= ms_c[k])
      base    <- if (ord == 1L) "union1" else "union2plus"
      st      <- paste0(base, if (married) "_married" else "_cohab")
    } else {
      startedSoFar <- sum(us_all <= t, na.rm = TRUE)
      st <- if (startedSoFar == 0L) "out_union"
      else if (startedSoFar == 1L) "sep1"
      else "sep2plus"
    }

    hasBirth  <- any(dob_c <= t)
    states[j] <- paste0(st, ".", if (hasBirth) "with_births" else "no_births")
  }

  data.frame(state = states, age_start = breaks[-length(breaks)], age_end = breaks[-1],
             stringsAsFactors = FALSE)
}


# ==== 7. Build ALL women's segments, tagged by group / cohort / weight ====

# cohortVar: a numeric year column (default "yBirth"). cohortWidth = 5 groups
# it into 5-year bins labelled "1980-1984"; set cohortWidth = 1 for single
# birth-year cohorts, or cohortVar = NULL to skip cohort stratification.
# dropIncompleteCohorts: if TRUE, drop any (groupVars, cohort) combination in
#         which NO woman had reached ageEnd by her own survey date -- i.e. a
#         birth cohort still too young, across every survey it appears in, to
#         say anything about ages near ageEnd. Leaves cohorts that are simply
#         thin (few women, but at least one who reached ageEnd) untouched --
#         this only removes cohorts with zero information at the top of the
#         age range, not small-sample ones. Default FALSE preserves prior
#         behaviour (every cohort kept, N_total tapering to 0 at high ages).
build_state_segments <- function(df, ageStart = 15, ageEnd = 40,
                                 weightVar     = "popWeight",
                                 groupVars     = "country",
                                 cohortVar     = "yBirth",
                                 cohortWidth   = 5,
                                 maxUnionOrder = 7L, maxBirths = 25L,
                                 dropIncompleteCohorts = FALSE,
                                 verbose       = TRUE) {

  unionOrders <- intersect(ordersPresent(df, "union_start_cmc"), seq_len(maxUnionOrder))
  birthOrders <- intersect(ordersPresent(df, "dob_cmc"),         seq_len(maxBirths))
  if (length(unionOrders) == 0L) stop("No union_start_cmc{u} columns found in df")

  usCols  <- paste0("union_start_cmc",    unionOrders)
  ueCols  <- paste0("union_end_cmc",      unionOrders)
  msCols  <- paste0("marriage_start_cmc", unionOrders)
  dobCols <- paste0("dob_cmc", birthOrders)
  ueCols <- ifelse(ueCols %in% names(df), ueCols, NA_character_)
  msCols <- ifelse(msCols %in% names(df), msCols, NA_character_)

  ageStartM <- ageStart * 12L
  ageEndM   <- ageEnd   * 12L

  # NOTE the key difference from Part A: no requirement that surveyDate_cmc
  # reach ageEndM. Every woman with a usable dob and survey date, who had
  # already turned ageStart by the time of interview, is kept.
  keep <- !is.na(df$indiv_dob_cmc) & !is.na(df$surveyDate_cmc) &
    (df$surveyDate_cmc > df$indiv_dob_cmc + ageStartM)
  if (verbose) {
    cat(sum(!keep), "of", nrow(df),
        "women dropped (missing dob/survey date, or younger than", ageStart, "at survey)\n")
  }

  dfK <- df[keep, , drop = FALSE]
  dob <- dfK$indiv_dob_cmc

  cohort <- if (!is.null(cohortVar) && cohortVar %in% names(dfK)) {
    yr <- dfK[[cohortVar]]
    if (!is.null(cohortWidth) && cohortWidth > 1) {
      lo <- floor(yr / cohortWidth) * cohortWidth
      paste0(lo, "-", lo + cohortWidth - 1L)
    } else {
      as.character(yr)
    }
  } else {
    rep("all", nrow(dfK))
  }

  grp <- if (is.null(groupVars) || length(groupVars) == 0L) {
    rep("all", nrow(dfK))
  } else {
    do.call(paste, c(as.list(dfK[, groupVars, drop = FALSE]), sep = " | "))
  }

  # --- optional: drop birth cohorts nobody has aged past ageEnd in yet ---
  if (isTRUE(dropIncompleteCohorts)) {
    ageAtSurveyM <- dfK$surveyDate_cmc - dob   # UNCAPPED age at survey, in months
    key          <- paste(grp, cohort, sep = "###")
    hasReachedEnd <- tapply(ageAtSurveyM >= ageEndM, key, any)
    keepCohort    <- hasReachedEnd[key]
    keepCohort[is.na(keepCohort)] <- FALSE

    if (verbose) {
      cat(sum(!keepCohort), "of", nrow(dfK),
          "women dropped: their (group, cohort) has nobody who reached age",
          ageEnd, "by their own survey date\n")
    }

    dfK    <- dfK[keepCohort, , drop = FALSE]
    dob    <- dob[keepCohort]
    cohort <- cohort[keepCohort]
    grp    <- grp[keepCohort]
  }

  # this woman's own censoring age (age at survey, capped at ageEndM)
  winEndAge <- pmin(ageEndM, dfK$surveyDate_cmc - dob)

  # shift every date field from calendar cmc to age-in-months (cmc - dob)
  usMat <- as.matrix(dfK[, usCols, drop = FALSE]) - dob
  ueRaw <- matrix(NA_real_, nrow = nrow(dfK), ncol = length(ueCols))
  haveUe <- !is.na(ueCols)
  if (any(haveUe)) ueRaw[, haveUe] <- as.matrix(dfK[, ueCols[haveUe], drop = FALSE])
  ueMat <- ueRaw - dob
  msRaw <- matrix(NA_real_, nrow = nrow(dfK), ncol = length(msCols))
  haveMs <- !is.na(msCols)
  if (any(haveMs)) msRaw[, haveMs] <- as.matrix(dfK[, msCols[haveMs], drop = FALSE])
  msMat <- msRaw - dob
  dobBirthsMat <- as.matrix(dfK[, dobCols, drop = FALSE]) - dob

  w <- if (weightVar %in% names(dfK)) dfK[[weightVar]] else rep(1, nrow(dfK))
  w[is.na(w)] <- 0

  n <- nrow(dfK)
  segList <- vector("list", n)
  for (i in seq_len(n)) {
    seg <- .womanStateSegments(usMat[i, ], ueMat[i, ], msMat[i, ], unionOrders,
                               dobBirthsMat[i, ], ageStartM, winEndAge[i])
    if (nrow(seg) > 0L) {
      seg$group  <- grp[i]
      seg$cohort <- cohort[i]
      seg$w      <- w[i]
      segList[[i]] <- seg
    }
  }
  segs <- do.call(rbind, segList)
  rownames(segs) <- NULL

  list(segments = segs, ageStartM = ageStartM, ageEndM = ageEndM)
}


# ==== 8. Expand segments to per-month-of-age counts (weighted + unweighted) ====

# One row per (group, cohort, state, age_months): n_weighted / n_unweighted =
# number of women (weighted by weightVar, and a plain head count) in that
# state at that exact month of age; N_total_* = total women observed at that
# age in that (group, cohort) -- the sum across all 14 states, which is
# automatically consistent because the states partition each woman's window
# exactly (no separate pass needed to compute it).
#
# Implementation note: segments are turned into "+w at age_start / -w at
# age_end" events and cumulative-summed per (group, cohort, state) -- an
# O(n_segments) difference-array, not a per-month expansion, so this stays
# fast even at 300 ages x many cohorts x 14 states.
#
# dropIncompleteCohorts: passed straight through to build_state_segments().
#         Set TRUE to exclude any (groupVars, cohort) with no woman who had
#         reached ageEnd by her own survey date, e.g. to keep only cohorts
#         old enough to say something about age 40 when ageEnd = 40.
state_counts_by_age <- function(df, ageStart = 15, ageEnd = 40,
                                weightVar     = "popWeight",
                                groupVars     = "country",
                                cohortVar     = "yBirth",
                                cohortWidth   = 5,
                                maxUnionOrder = 7L, maxBirths = 25L,
                                dropIncompleteCohorts = FALSE,
                                verbose       = TRUE) {

  built <- build_state_segments(df, ageStart, ageEnd, weightVar, groupVars,
                                cohortVar, cohortWidth, maxUnionOrder, maxBirths,
                                dropIncompleteCohorts, verbose)
  segs      <- built$segments
  ageStartM <- built$ageStartM
  ageEndM   <- built$ageEndM
  nMonths   <- ageEndM - ageStartM

  if (is.null(segs) || nrow(segs) == 0L) stop("No usable women/segments -- check df and filters")

  key <- paste(segs$group, segs$cohort, segs$state, sep = "###")

  starts <- data.frame(key = key, ageIdx = segs$age_start - ageStartM, dw = segs$w,  dn = 1L)
  ends   <- data.frame(key = key, ageIdx = segs$age_end   - ageStartM, dw = -segs$w, dn = -1L)
  ends   <- ends[ends$ageIdx < nMonths, ]   # an end event exactly at the grid edge needs no removal

  events <- rbind(starts, ends)
  agg    <- stats::aggregate(cbind(dw, dn) ~ key + ageIdx, data = events, FUN = sum)
  # NOTE: for very large pooled datasets, swap this aggregate() for
  # data.table if it becomes a bottleneck -- logic is unchanged either way.

  keys     <- unique(agg$key)
  out_list <- vector("list", length(keys))
  for (i in seq_along(keys)) {
    sub <- agg[agg$key == keys[i], ]
    w_delta <- numeric(nMonths); w_delta[sub$ageIdx + 1L] <- sub$dw
    n_delta <- numeric(nMonths); n_delta[sub$ageIdx + 1L] <- sub$dn
    out_list[[i]] <- data.frame(key         = keys[i],
                                age_months  = ageStartM + seq_len(nMonths) - 1L,
                                n_weighted  = cumsum(w_delta),
                                n_unweighted = cumsum(n_delta))
  }
  res <- do.call(rbind, out_list)

  parts       <- strsplit(res$key, "###", fixed = TRUE)
  res$group   <- vapply(parts, `[`, character(1), 1L)
  res$cohort  <- vapply(parts, `[`, character(1), 2L)
  res$state   <- vapply(parts, `[`, character(1), 3L)
  res$key     <- NULL
  res$age_years <- res$age_months / 12

  totals <- stats::aggregate(cbind(n_weighted, n_unweighted) ~ group + cohort + age_months,
                             data = res, FUN = sum)
  names(totals)[names(totals) == "n_weighted"]   <- "N_total_weighted"
  names(totals)[names(totals) == "n_unweighted"] <- "N_total_unweighted"

  res <- merge(res, totals, by = c("group", "cohort", "age_months"), all.x = TRUE)
  res$prop_weighted <- ifelse(res$N_total_weighted > 0,
                              res$n_weighted / res$N_total_weighted, NA_real_)

  res <- res[order(res$group, res$cohort, res$age_months, res$state), ]
  rownames(res) <- NULL
  res
}


# ==== 9. Roll-ups (same idea as Part A, applied to the by-age table) ====

# N_total_* does not change when states are collapsed (same women, same age),
# so it is just carried over from the original 14-state table rather than
# recomputed.
.rollup_by_age <- function(res14, newState) {
  res <- res14
  res$state <- newState
  agg <- stats::aggregate(cbind(n_weighted, n_unweighted) ~ group + cohort + age_months + state,
                          data = res, FUN = sum)
  totals <- unique(res14[, c("group", "cohort", "age_months",
                             "N_total_weighted", "N_total_unweighted")])
  agg <- merge(agg, totals, by = c("group", "cohort", "age_months"), all.x = TRUE)
  agg$prop_weighted <- ifelse(agg$N_total_weighted > 0,
                              agg$n_weighted / agg$N_total_weighted, NA_real_)
  agg <- merge(agg, unique(res14[, c("group", "cohort", "age_months", "age_years")]),
              by = c("group", "cohort", "age_months"), all.x = TRUE)
  agg[order(agg$group, agg$cohort, agg$age_months, agg$state), ]
}

# Collapse with/without births -- keep the 7 union/cohab/married states.
state_counts_by_age_7state <- function(res14) {
  .rollup_by_age(res14, sub("\\.(no_births|with_births)$", "", res14$state))
}

# Collapse both birth-status AND cohab/married -- the 5 headline states.
state_counts_by_age_5state <- function(res14) {
  newState <- dplyr::case_when(
    startsWith(res14$state, "out_union")    ~ "out_union",
    startsWith(res14$state, "union1_")      ~ "union1",
    startsWith(res14$state, "sep1")         ~ "sep1",
    startsWith(res14$state, "union2plus_")  ~ "union2plus",
    startsWith(res14$state, "sep2plus")     ~ "sep2plus",
    .default = NA_character_
  )
  .rollup_by_age(res14, newState)
}

