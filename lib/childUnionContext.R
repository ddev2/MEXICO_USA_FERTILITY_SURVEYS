# >>> Claude 2026-09-23
# childUnionContext.R
#
# Union context of first-born children during their first ten years: the
# mother's union state at each month of the child's age, reconstructed from the
# union histories returned by createFirstUnionFirstBirth() (enadid_lib.R).
#
# WHY THIS FILE EXISTS
#   plot10Years_child() and plotFullUnion_child() in "ENADID fertility.R" average
#   over children observed for the full 120 months, pooling all surveys. A
#   survey taken g years after a birth observes only the mothers aged at most
#   Amax - g at that birth, Amax being the survey's upper age. Every survey
#   enters the pool with population weights, so it counts as the whole
#   population. Cohorts observed by several surveys long after birth therefore
#   receive extra slices made only of young mothers, while the most recent
#   cohorts, seen by one survey shortly after age 10, keep almost the full range
#   of ages. The NSFG upper age also rose from 44 to 49 in 2015. With a stable
#   population this alone draws a trough and a recovery (simulation, 2026-09-23).
#
# STATES (mother's situation at each month of the child's age)
#   1 noUnion     mother not yet in a union
#   2 union1      mother in her first union
#   3 outUnion    mother outside a union after a union ended (separation or
#                 widowhood, before re-partnering)
#   4 union2plus  mother in a second or later union
#
# CONTENTS
#   1. Preparation        childPrepare(), childEpisodes()
#   2. Estimator          .ajMonthly(), .cellEstimates(), .bootMult()
#   3. Step 1             childAgeProfile(), plotChildAgeProfile()
#   4. Step 2             childCommonSupport(), childDirect()
#   5. Step 3             childBySurvey(), plotChildBySurvey()
#   6. Stratified AJ      childStratifiedAJ()
#   7. Plots              plotChildCompare(), plotChildShares()
#   8. Paper figures      plotFigure8(), plotFigure9()
#   9. Lexis diagram      plotLexisSelection()
#
# ESTIMATOR
#   Aalen-Johansen on the monthly grid of the child's age. With every child
#   observed to 120 months it reproduces exactly the weighted average of months
#   that plot10Years_child() computes. With censoring it uses the
#   partial histories. State occupation probabilities from AJ do not require the
#   Markov assumption under independent censoring (Datta and Satten 2001,
#   Statistics & Probability Letters 55: 403-411). Censoring is NOT independent
#   of the outcome when mothers of different ages are observable for different
#   lengths of time, which is why childStratifiedAJ() estimates within groups of
#   mother's age at first birth and then standardises.
#
# TESTS in tests/tests_childUnionContext.R (simulated data, no survey needed):
#   1. complete windows: equal to the direct overlap calculation (1e-16)
#   2. censored, weighted data: equal to survival::survfit multistate (2e-16)
#   3. a population with no trend on the NSFG calendar: the current method
#      shows a trough and a recovery; common support and the stratified AJ
#      stay at the true value.
#
# CHILD DEATH is treated as censoring: the estimates describe children in the
#   absence of child mortality. The direct method (childDirect) keeps its
#   original definition and conditions on survival to age 10.
#
# LIMITS OF THE UNION HISTORIES
#   NSFG1988 and NSFG2022_23 record only the first union; argument firstUnionOnly
#   removes them from the four-state shares but keeps them for the measures
#   based on the first union alone. ENADID 2009, 2014, 2018 and 2023 record the
#   first and the last union: a mother with three or more unions has her
#   intermediate unions counted as time outside a union.

library(dplyr)
library(ggplot2)

CHILD_STATES       <- c("noUnion", "union1", "outUnion", "union2plus")
CHILD_STATE_LABELS <- c(noUnion    = "Mother not yet in a union",
                        union1     = "Mother in first union",
                        outUnion   = "Mother outside a union after a union ended",
                        union2plus = "Mother in a subsequent union")
CHILD_SERIES_COLS  <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                        "#e87ba4", "#008300", "#6b6a64", "#8a5cd6")


# ==== 1. Preparation ====

# One row per first child. `df` is the output of createFirstUnionFirstBirth(),
# which keeps ALL women, so the upper age of each survey can be read off the
# data before the women without a birth are dropped.
#
# maxAgeBirth = 35 reproduces the cap of the existing code: the window of a
# child is cut at the mother's 45th birthday, so a complete 10-year window
# requires an exact age at the birth of at most 35.
childPrepare <- function (df,
                          LifeLimit        = 120L,
                          maxAgeBirth      = 35,
                          cohortWidth      = 5L,
                          ageBreaks        = c(-Inf, 20, 25, 30, Inf),
                          ageLabels        = c("<20", "20-24", "25-29", "30-35"),
                          surveyMaxAge     = NULL,
                          firstUnionOnly   = c("NSFG1988", "NSFG2022_23"),
                          union_start_cols = paste0("cmc_union", 1:4),
                          union_end_cols   = paste0("cmc_sep", 1:4),
                          keepVars         = NULL,
                          quiet            = FALSE) {

  need <- c("surveyName", "country", "cmc_birth", "cmc_survey", "cmc_birth1",
            "cmc_death1", "popWeight", union_start_cols, union_end_cols)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("childPrepare: missing columns ", paste(miss, collapse = ", "))

  # --- upper age of each survey, from all women ---
  ageSurv <- floor((df$cmc_survey - df$cmc_birth) / 12)
  amax    <- tapply(ageSurv, as.character(df$surveyName), max, na.rm = TRUE)
  if (!is.null(surveyMaxAge)) amax[names(surveyMaxAge)] <- surveyMaxAge
  if (!quiet) {
    cat("childPrepare: upper age at survey (from the data; override with surveyMaxAge)\n")
    print(amax)
  }

  # --- one row per first child ---
  df <- df[!is.na(df$cmc_birth1) & !is.na(df$cmc_birth) & !is.na(df$popWeight), , drop = FALSE]
  nAll <- nrow(df)
  df <- df[(df$cmc_birth1 - df$cmc_birth) <= maxAgeBirth * 12, , drop = FALSE]
  if (!quiet) cat("childPrepare:", nAll - nrow(df), "first children dropped: mother older than",
                  maxAgeBirth, "at the birth\n")

  S <- as.matrix(df[, union_start_cols]); storage.mode(S) <- "double"
  E <- as.matrix(df[, union_end_cols]);   storage.mode(E) <- "double"
  K <- ncol(S)

  # --- repair the union intervals ---
  # A union with no start does not exist. A missing end is: the start of the
  # next union when there is one (the cleanENADID back-fill convention), and
  # otherwise an ongoing union. An end after the next start is cut there.
  nFill <- nClamp <- nNeg <- 0L
  E[is.na(S)] <- NA
  for (k in seq_len(K)) {
    nextS <- if (k < K) S[, k + 1] else rep(NA_real_, nrow(S))
    idx <- !is.na(S[, k]) & is.na(E[, k]) & !is.na(nextS)
    E[idx, k] <- nextS[idx]; nFill <- nFill + sum(idx)
    idx <- !is.na(S[, k]) & !is.na(E[, k]) & !is.na(nextS) & E[, k] > nextS
    E[idx, k] <- nextS[idx]; nClamp <- nClamp + sum(idx)
    idx <- !is.na(S[, k]) & is.na(E[, k])
    E[idx, k] <- Inf
    idx <- !is.na(S[, k]) & E[, k] < S[, k]
    E[idx, k] <- S[idx, k]; nNeg <- nNeg + sum(idx)
  }
  if (!quiet) cat("childPrepare: union ends set to the next start:", nFill,
                  "| cut at the next start:", nClamp, "| before their start:", nNeg, "\n")

  b     <- df$cmc_birth1
  death <- ifelse(is.na(df$cmc_death1), Inf, df$cmc_death1)
  cEnd  <- pmin(b + LifeLimit, df$cmc_survey, death)
  yB    <- (b - 1) %/% 12 + 1900
  coh   <- floor(yB / cohortWidth) * cohortWidth
  cohLab <- if (cohortWidth == 1) as.character(coh) else paste0(coh, "-", substr(coh + cohortWidth - 1, 3, 4))

  ch <- data.frame(
    id          = seq_len(nrow(df)),
    country     = as.character(df$country),
    surveyName  = as.character(df$surveyName),
    w           = df$popWeight,
    b           = b,
    cEnd        = cEnd,
    follow      = pmax(0, cEnd - b),                       # months observed
    complete    = (cEnd - b) >= LifeLimit,                 # full window observed
    diedBefore  = death < pmin(b + LifeLimit, df$cmc_survey),
    ageBirth    = (b - df$cmc_birth) / 12,
    gap         = (df$cmc_survey - b) / 12,                # years from birth to survey
    Amax        = as.numeric(amax[as.character(df$surveyName)]),
    yBirth1     = yB,
    cohort      = coh,
    cohortLabel = cohLab,
    firstUnionOnly = as.character(df$surveyName) %in% firstUnionOnly,
    stringsAsFactors = FALSE
  )
  for (v in intersect(keepVars, names(df))) ch[[v]] <- df[[v]]
  ch$ageGroup <- cut(ch$ageBirth, breaks = ageBreaks, labels = ageLabels, right = FALSE)
  ch$initState <- .stateAt(b, S, E)

  attr(ch, "S")         <- S
  attr(ch, "E")         <- E
  attr(ch, "LifeLimit") <- LifeLimit
  attr(ch, "maxAgeBirth") <- maxAgeBirth
  if (!quiet) {
    cat("childPrepare:", nrow(ch), "first children,", sum(ch$complete),
        "with a complete", LifeLimit, "month window\n")
    cat("childPrepare: state at birth\n")
    print(table(country = ch$country, state = CHILD_STATES[ch$initState]))
    unknownFU <- setdiff(firstUnionOnly, unique(ch$surveyName))
    if (length(unknownFU) > 0) cat("childPrepare: firstUnionOnly surveys not in the data:",
                                   paste(unknownFU, collapse = ", "), "\n")
  }
  ch
}

# State of the mother at CMC time `tm`, one value per row of S and E.
.stateAt <- function (tm, S, E) {
  inU <- (!is.na(S)) & (tm >= S) & (tm < E)
  inU[is.na(inU)] <- FALSE
  later <- if (ncol(S) > 1) rowSums(inU[, -1, drop = FALSE]) > 0 else rep(FALSE, length(tm))
  ifelse(is.na(S[, 1]) | tm < S[, 1], 1L,
         ifelse(inU[, 1], 2L, ifelse(later, 4L, 3L)))
}

# Segments of constant state on the child's clock, in months since birth.
# One row per segment: child row (idx), from state, tstart, tstop, and `to`,
# the next state or 0 when the segment ends by censoring (survey date, child's
# death, or the end of the window).
childEpisodes <- function (ch) {
  S  <- attr(ch, "S"); E <- attr(ch, "E")
  n  <- nrow(ch)
  pts <- cbind(ch$b, S, E, ch$cEnd)
  idx  <- rep(seq_len(n), times = ncol(pts))
  tm   <- as.vector(pts)
  keep <- !is.na(tm) & is.finite(tm) & tm >= ch$b[idx] & tm <= ch$cEnd[idx]
  idx  <- idx[keep]; tm <- tm[keep]
  o    <- order(idx, tm); idx <- idx[o]; tm <- tm[o]
  dup  <- c(FALSE, idx[-1] == idx[-length(idx)] & tm[-1] == tm[-length(tm)])
  idx  <- idx[!dup]; tm <- tm[!dup]

  last  <- c(idx[-1] != idx[-length(idx)], TRUE)
  sIdx  <- idx[!last]
  sFrom <- tm[!last]
  sTo   <- tm[which(!last) + 1]
  st    <- .stateAt(sFrom, S[sIdx, , drop = FALSE], E[sIdx, , drop = FALSE])

  # merge consecutive segments in the same state (e.g. union 2 -> union 3)
  newRun <- c(TRUE, sIdx[-1] != sIdx[-length(sIdx)] | st[-1] != st[-length(st)])
  ep <- data.frame(idx    = sIdx[newRun],
                   from   = st[newRun],
                   tstart = sFrom[newRun] - ch$b[sIdx[newRun]],
                   tstop  = sTo[c(which(newRun)[-1] - 1L, length(sTo))] - ch$b[sIdx[newRun]])
  nxtSame <- c(ep$idx[-1] == ep$idx[-nrow(ep)], FALSE)
  ep$to   <- ifelse(nxtSame, c(ep$from[-1], 0L), 0L)
  ep <- ep[ep$tstop > ep$tstart, , drop = FALSE]
  ep$initState <- ch$initState[ep$idx]
  ep
}


# ==== 2. Estimator ====

# Aalen-Johansen on the monthly grid t = 0..L. At time t the risk set of state
# j holds the segments with tstart < t <= tstop (a child censored at t is still
# at risk at t), and the transitions are the segments with tstop == t.
# P[t + 1, ] is the state distribution during month t of the child's life.
.ajMonthly <- function (from, to, tstart, tstop, w, L, nS = 4L, minRisk = 1) {
  first <- tstart == 0
  p0 <- vapply(seq_len(nS), function (j) sum(w[first & from == j]), 0)
  if (sum(p0) <= 0) return (NULL)
  p0 <- p0 / sum(p0)

  tstop <- pmin(tstop, L)
  addAt <- function (val, j, tt) {                 # nS x (L + 1) matrix
    m <- matrix(0, nS, L + 1)
    s <- rowsum(val, (j - 1) * (L + 1) + tt, reorder = FALSE)
    k <- as.integer(rownames(s))
    m[cbind((k %/% (L + 1)) + 1, (k %% (L + 1)) + 1)] <- s[, 1]
    m
  }
  Enter <- addAt(w, from, tstart)
  Leave <- addAt(w, from, tstop)
  risk  <- apply(Enter, 1, cumsum) - apply(Leave, 1, cumsum)   # (L+1) x nS; row t = risk at t
  cntE  <- addAt(rep(1, length(w)), from, tstart)
  cntL  <- addAt(rep(1, length(w)), from, tstop)
  nRisk <- apply(cntE, 1, cumsum) - apply(cntL, 1, cumsum)

  tr   <- to > 0 & tstop <= L & tstop >= 1
  H    <- array(0, c(L, nS, nS))                   # H[t, j, k] = d_jk(t) / risk_j(t)
  if (any(tr)) {
    kk   <- ((from[tr] - 1) * nS + (to[tr] - 1)) * L + (tstop[tr] - 1)
    dSum <- rowsum(w[tr], kk, reorder = FALSE)
    key  <- as.integer(rownames(dSum))
    dT   <- key %% L + 1; key <- key %/% L
    dJ   <- key %/% nS + 1; dK <- key %% nS + 1
    rk   <- risk[cbind(dT, dJ)]
    H[cbind(dT, dJ, dK)] <- ifelse(rk > 0, dSum[, 1] / rk, 0)
  }
  exitRate <- apply(H, c(1, 2), sum)               # L x nS

  P <- matrix(NA_real_, L + 1, nS); P[1, ] <- p0
  for (t in seq_len(L)) {
    pt <- P[t, ]
    P[t + 1, ] <- pt + as.vector(pt %*% H[t, , ]) - pt * exitRate[t, ]
  }
  tot   <- rowSums(nRisk[seq_len(L), , drop = FALSE])
  lastT <- if (any(tot >= minRisk)) max(which(tot >= minRisk)) else 0L
  list(P = P, p0 = p0, lastT = lastT, nRiskEnd = sum(nRisk[L, ]))
}

# Estimates for one cell: shares of the L months by state, P(born in first
# union), P(first union intact through month L | born in it), their product.
.cellEstimates <- function (ep, wSeg, L, minRiskEnd = 20, useShares = TRUE) {
  na <- c(setNames(rep(NA_real_, 4), CHILD_STATES), bornU1 = NA, intactU1 = NA, joint = NA,
          nRiskEnd = 0)
  if (length(ep$from) == 0) return (na)
  aj <- .ajMonthly(ep$from, ep$to, ep$tstart, ep$tstop, wSeg, L)
  if (is.null(aj)) return (na)
  out <- na
  out["nRiskEnd"] <- aj$nRiskEnd
  out["bornU1"]   <- aj$p0[2]                      # needs no follow-up
  if (aj$nRiskEnd >= minRiskEnd) {
    if (useShares) out[CHILD_STATES] <- colSums(aj$P[1:L, , drop = FALSE]) / L
    s <- ep$initState == 2
    if (any(s)) {
      aj2 <- .ajMonthly(ep$from[s], ep$to[s], ep$tstart[s], ep$tstop[s], wSeg[s], L)
      if (!is.null(aj2) && aj2$nRiskEnd >= minRiskEnd) out["intactU1"] <- aj2$P[L, 2]
    }
    out["joint"] <- out["bornU1"] * out["intactU1"]
  }
  out
}

# Segment vectors of a subset of rows, as a plain list (fast to reuse).
.epList <- function (ep, rows) {
  list(idx = ep$idx[rows], from = ep$from[rows], to = ep$to[rows],
       tstart = ep$tstart[rows], tstop = ep$tstop[rows], initState = ep$initState[rows])
}

# Bootstrap multiplicities: resample clusters with replacement within strata.
# Default: women resampled within each survey, which are independent samples.
.bootMult <- function (unit, stratum) {
  mult <- numeric(length(unit))
  for (ii in split(seq_along(unit), stratum)) {
    u <- unit[ii]; lev <- unique(u); k <- length(lev)
    cnt <- tabulate(sample.int(k, k, replace = TRUE), nbins = k)
    mult[ii] <- cnt[match(u, lev)]
  }
  mult
}

# Point estimates and bootstrap intervals for each group of `groupVars`.
.estimateGroups <- function (ch, ep, groupVars, replicates, varStrata, varCluster,
                             confLevel, seed, minRiskEnd, dropFirstUnionOnlyShares = TRUE,
                             w = ch$w) {
  L   <- attr(ch, "LifeLimit")
  key <- do.call(paste, c(ch[groupVars], sep = "\r"))
  grp <- split(seq_len(nrow(ch)), key)
  segKey <- key[ep$idx]
  segGrp <- split(seq_len(nrow(ep)), factor(segKey, levels = names(grp)))
  # segment lists built once, re-weighted in every bootstrap replicate
  eAll <- lapply(segGrp, function (r) .epList(ep, r))
  eSh  <- lapply(segGrp, function (r) {
    if (!dropFirstUnionOnlyShares) return (NULL)
    fu <- ch$firstUnionOnly[ep$idx[r]]
    if (!any(fu)) NULL else .epList(ep, r[!fu])
  })

  run <- function (wChild) {
    t(vapply(seq_along(grp), function (g) {
      e    <- eAll[[g]]
      full <- .cellEstimates(e, wChild[e$idx], L, minRiskEnd)
      if (!is.null(eSh[[g]])) {
        sh <- .cellEstimates(eSh[[g]], wChild[eSh[[g]]$idx], L, minRiskEnd)
        full[CHILD_STATES] <- sh[CHILD_STATES]
      }
      full
    }, numeric(8)))
  }

  est <- run(w)
  info <- do.call(rbind, lapply(grp, function (ii) {
    wi <- w[ii]
    data.frame(n = length(ii), nComplete = sum(ch$complete[ii]),
               nEff = sum(wi)^2 / sum(wi^2),
               nSurveys = length(unique(ch$surveyName[ii])),
               meanAgeBirth = sum(wi * ch$ageBirth[ii]) / sum(wi))
  }))
  first <- ch[vapply(grp, `[`, 0L, 1), groupVars, drop = FALSE]
  res <- cbind(first, info, as.data.frame(est))
  rownames(res) <- NULL

  if (replicates > 0) {
    if (!is.null(seed)) set.seed(seed)
    unit    <- if (is.null(varCluster)) ch$id else paste(ch$surveyName, ch[[varCluster]])
    stratum <- if (is.null(varStrata)) rep("1", nrow(ch)) else as.character(ch[[varStrata]])
    B <- array(NA_real_, c(replicates, nrow(est), ncol(est)))
    for (r in seq_len(replicates)) {
      B[r, , ] <- run(w * .bootMult(unit, stratum))
      if (r %% 25 == 0) cat("  bootstrap replicate", r, "of", replicates, "\n")
    }
    a <- (1 - confLevel) / 2
    for (k in seq_len(ncol(est))) {
      nm <- colnames(est)[k]
      if (nm == "nRiskEnd") next
      res[[paste0(nm, "_lower")]] <- apply(B[, , k, drop = FALSE], 2, stats::quantile, a, na.rm = TRUE)
      res[[paste0(nm, "_upper")]] <- apply(B[, , k, drop = FALSE], 2, stats::quantile, 1 - a, na.rm = TRUE)
    }
  }
  res
}


# ==== 3. Step 1: mother's age at first birth by child cohort and survey ====

# Weighted mean age of the mother at the birth and distribution by age group,
# for the children the current method keeps (complete window), by survey and
# pooled, together with the "true" composition of first births: the one seen
# among children whose survey was close enough to the birth for every mother up
# to maxAgeBirth to be observable (gap <= Amax + 1 - maxAgeBirth). Those
# children need not have reached age 10.
childAgeProfile <- function (ch) {
  A <- attr(ch, "maxAgeBirth")
  summ <- function (d, by) {
    d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::summarise(n = dplyr::n(),
                       wsum = sum(w),
                       meanAgeBirth = sum(w * ageBirth) / sum(w),
                       share_lt20 = sum(w * (ageBirth < 20)) / sum(w),
                       share_2024 = sum(w * (ageBirth >= 20 & ageBirth < 25)) / sum(w),
                       share_2529 = sum(w * (ageBirth >= 25 & ageBirth < 30)) / sum(w),
                       share_30p  = sum(w * (ageBirth >= 30)) / sum(w),
                       gapMin = min(gap), gapMax = max(gap),
                       .groups = "drop")
  }
  cmpl <- ch[ch$complete, ]
  bySurvey <- summ(cmpl, c("country", "cohort", "cohortLabel", "surveyName")) %>%
    dplyr::group_by(country, cohort) %>%
    dplyr::mutate(surveyShare = wsum / sum(wsum)) %>%
    dplyr::ungroup()
  pooled <- summ(cmpl, c("country", "cohort", "cohortLabel")) %>%
    dplyr::mutate(sample = "Complete 10-year window (current method)")
  truth  <- summ(ch[ch$gap <= ch$Amax + 1 - A, ], c("country", "cohort", "cohortLabel")) %>%
    dplyr::mutate(sample = "All first births, surveys close to the birth")
  list(bySurvey = bySurvey, pooled = pooled, truth = truth,
       both = dplyr::bind_rows(pooled, truth))
}

plotChildAgeProfile <- function (prof) {
  ggplot(prof$both, aes(x = cohort, y = meanAgeBirth, colour = sample)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    facet_wrap(~ country) +
    scale_colour_manual(values = CHILD_SERIES_COLS[1:2]) +
    labs(x = "Birth cohort of the first child", y = "Mean age of the mother at the birth",
         colour = NULL) +
    theme(legend.position = "bottom")
}


# ==== 4. Step 2: common support ====

# Children whose mothers were at most maxAgeBirth (completed years) at the birth,
# each survey contributing only the children born few enough years before it
# for every such mother to be observable: gap <= Amax - maxAgeBirth. Within
# this population no survey is truncated by the mother's age, so the pooled
# estimate does not depend on the survey calendar.
childCommonSupport <- function (ch, maxAgeBirth = 28, quiet = FALSE) {
  keep <- floor(ch$ageBirth) <= maxAgeBirth & ch$gap <= ch$Amax - maxAgeBirth
  out  <- ch[keep, , drop = FALSE]
  for (a in c("S", "E")) attr(out, a) <- attr(ch, a)[keep, , drop = FALSE]
  for (a in c("LifeLimit", "maxAgeBirth")) attr(out, a) <- attr(ch, a)
  out$id <- seq_len(nrow(out))
  if (!quiet) {
    cov <- out[out$complete, ] %>%
      dplyr::group_by(country, cohortLabel) %>%
      dplyr::summarise(nSurveys = dplyr::n_distinct(surveyName), n = dplyr::n(), .groups = "drop")
    cat("childCommonSupport: mothers <=", maxAgeBirth, "at the birth,", sum(out$complete),
        "children with a complete window\n")
    print(as.data.frame(cov))
  }
  out
}

# The current definition: children with a complete window only (child alive at
# 10, survey after the 10th birthday), no stratification. With the fixes:
# births after the first union ended are their own case (state 3 or 4 at birth,
# never "born in first union"), and a missing end of a non-last union no longer
# runs to the end of the window.
childDirect <- function (ch, groupVars = c("country", "cohort", "cohortLabel"),
                         replicates = 200L, varStrata = "surveyName", varCluster = NULL,
                         confLevel = 0.95, seed = NULL, minRiskEnd = 20) {
  keep <- ch$complete
  cc   <- ch[keep, , drop = FALSE]
  for (a in c("S", "E")) attr(cc, a) <- attr(ch, a)[keep, , drop = FALSE]
  for (a in c("LifeLimit", "maxAgeBirth")) attr(cc, a) <- attr(ch, a)
  cc$id <- seq_len(nrow(cc))
  ep <- childEpisodes(cc)
  .estimateGroups(cc, ep, groupVars, replicates, varStrata, varCluster, confLevel, seed, minRiskEnd)
}


# ==== 5. Step 3: survey by survey, same cohorts ====

# The same cohorts estimated from each survey separately, on the common support
# so that the surveys cover the same range of mothers' ages. A survey effect
# shows as a level difference for the same cohort. Without the common support
# the comparison would mix survey effects with the age truncation.
childBySurvey <- function (ch, maxAgeBirth = 28, minSurveys = 2L, replicates = 200L,
                           confLevel = 0.95, seed = NULL, minRiskEnd = 20) {
  cs  <- childCommonSupport(ch, maxAgeBirth, quiet = TRUE)
  res <- childDirect(cs, groupVars = c("country", "cohort", "cohortLabel", "surveyName"),
                     replicates = replicates, varStrata = "surveyName",
                     confLevel = confLevel, seed = seed, minRiskEnd = minRiskEnd)
  res %>%
    dplyr::group_by(country, cohort) %>%
    dplyr::filter(sum(!is.na(joint)) >= minSurveys) %>%
    dplyr::ungroup()
}

plotChildBySurvey <- function (res, var = "joint", yLab = NULL) {
  lo <- paste0(var, "_lower"); hi <- paste0(var, "_upper")
  p <- ggplot(res, aes(x = cohort, y = .data[[var]], colour = surveyName)) +
    geom_point(size = 2, position = position_dodge(width = 2)) +
    facet_wrap(~ country) +
    labs(x = "Birth cohort of the first child", y = if (is.null(yLab)) var else yLab, colour = NULL) +
    theme(legend.position = "bottom")
  if (lo %in% names(res)) {
    p <- p + geom_errorbar(aes(ymin = .data[[lo]], ymax = .data[[hi]]), width = 0,
                           position = position_dodge(width = 2))
  }
  p
}


# ==== 6. Stratified Aalen-Johansen ====

# Aalen-Johansen within cells country x cohort x mother's age group, using all
# first children, complete or censored, then standardised over the age groups.
#
# target
#   "observed"  composition of first births by the mother's age for each
#               country x cohort, from the children whose survey was close
#               enough to the birth to observe every mother up to maxAgeBirth.
#               Removes the artifact and keeps the real change in the age at
#               first birth. Not available for cohorts born before any such
#               survey (early NSFG cohorts): those come out NA.
#   "fixed"     one composition per country, pooled over cohorts: removes all
#               change in the mothers' ages, real or artificial.
#   data.frame  columns country, cohort, ageGroup, share: an external
#               composition, e.g. NCHS natality or INEGI registrations.
#
# A cohort x age cell is estimable when at least minRiskEnd children remain
# observed in the last month. A standardised value is NA when any age group of
# its cohort is not estimable, rather than renormalising over the others.
childStratifiedAJ <- function (ch, target = "observed", replicates = 100L,
                               varStrata = "surveyName", varCluster = NULL,
                               confLevel = 0.95, seed = NULL, minRiskEnd = 20) {
  L  <- attr(ch, "LifeLimit")
  A  <- attr(ch, "maxAgeBirth")
  ch <- ch[!is.na(ch$ageGroup), , drop = FALSE]
  ep <- childEpisodes(ch)
  grpVars <- c("country", "cohort", "cohortLabel", "ageGroup")

  targetFun <- function (w) {
    if (is.data.frame(target)) return (target)
    near <- ch$gap <= ch$Amax + 1 - A
    d <- data.frame(country = ch$country, cohort = ch$cohort, ageGroup = ch$ageGroup, w = w)
    if (identical(target, "observed")) d <- d[near, ]
    if (identical(target, "fixed"))    d <- d[near, ] %>% dplyr::mutate(cohort = NA)
    d %>% dplyr::group_by(country, cohort, ageGroup) %>%
      dplyr::summarise(w = sum(w), .groups = "drop") %>%
      dplyr::group_by(country, cohort) %>%
      dplyr::mutate(share = w / sum(w)) %>% dplyr::ungroup() %>% dplyr::select(-w)
  }

  standardise <- function (cells, tg) {
    if (all(is.na(tg$cohort))) tg <- dplyr::select(tg, -cohort)
    x <- dplyr::left_join(cells, tg, by = intersect(c("country", "cohort", "ageGroup"), names(tg)))
    x$share[is.na(x$share)] <- 0
    x %>%
      dplyr::group_by(country, cohort, cohortLabel) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(CHILD_STATES),
                      ~ if (any(is.na(.x[share > 0]))) NA_real_ else sum(share * .x)),
        bornU1 = if (any(is.na(bornU1[share > 0]))) NA_real_ else sum(share * bornU1),
        joint  = if (any(is.na(joint[share > 0])))  NA_real_ else sum(share * joint),
        targetCovered = sum(share),
        .groups = "drop") %>%
      dplyr::mutate(intactU1 = joint / bornU1) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(c(CHILD_STATES, "bornU1", "intactU1", "joint")),
                                  ~ ifelse(targetCovered < 0.999, NA_real_, .x)))
  }

  cells <- .estimateGroups(ch, ep, grpVars, 0L, NULL, NULL, confLevel, NULL, minRiskEnd)
  bad   <- cells[is.na(cells$joint), c("country", "cohortLabel", "ageGroup", "n", "nRiskEnd")]
  if (nrow(bad) > 0) {
    cat("childStratifiedAJ: cells with fewer than", minRiskEnd,
        "children observed to the last month (their cohort is NA once standardised):\n")
    print(bad, row.names = FALSE)
  }
  tg    <- targetFun(ch$w)
  std   <- standardise(cells, tg)
  crude <- .estimateGroups(ch, ep, c("country", "cohort", "cohortLabel"), 0L, NULL, NULL,
                           confLevel, NULL, minRiskEnd)

  if (replicates > 0) {
    if (!is.null(seed)) set.seed(seed)
    unit    <- if (is.null(varCluster)) ch$id else paste(ch$surveyName, ch[[varCluster]])
    stratum <- if (is.null(varStrata)) rep("1", nrow(ch)) else as.character(ch[[varStrata]])
    vars <- c(CHILD_STATES, "bornU1", "intactU1", "joint")
    Bs <- array(NA_real_, c(replicates, nrow(std), length(vars)))
    Bc <- array(NA_real_, c(replicates, nrow(crude), length(vars)))
    for (r in seq_len(replicates)) {
      wB <- ch$w * .bootMult(unit, stratum)
      cb <- .estimateGroups(ch, ep, grpVars, 0L, NULL, NULL, confLevel, NULL, minRiskEnd, w = wB)
      sb <- standardise(cb, targetFun(wB))
      sb <- sb[match(paste(std$country, std$cohort), paste(sb$country, sb$cohort)), ]
      Bs[r, , ] <- as.matrix(sb[, vars])
      kb <- .estimateGroups(ch, ep, c("country", "cohort", "cohortLabel"), 0L, NULL, NULL,
                            confLevel, NULL, minRiskEnd, w = wB)
      Bc[r, , ] <- as.matrix(kb[, vars])
      if (r %% 10 == 0) cat("  bootstrap replicate", r, "of", replicates, "\n")
    }
    a <- (1 - confLevel) / 2
    for (k in seq_along(vars)) {
      std[[paste0(vars[k], "_lower")]]   <- apply(Bs[, , k, drop = FALSE], 2, stats::quantile, a, na.rm = TRUE)
      std[[paste0(vars[k], "_upper")]]   <- apply(Bs[, , k, drop = FALSE], 2, stats::quantile, 1 - a, na.rm = TRUE)
      crude[[paste0(vars[k], "_lower")]] <- apply(Bc[, , k, drop = FALSE], 2, stats::quantile, a, na.rm = TRUE)
      crude[[paste0(vars[k], "_upper")]] <- apply(Bc[, , k, drop = FALSE], 2, stats::quantile, 1 - a, na.rm = TRUE)
    }
  }
  list(cells = cells, target = tg, std = std, crude = crude)
}


# ==== 7. Plots ====

# Several result frames on one plot, one colour per series; `results` is a
# named list, each element a frame with country, cohort and the variable.
plotChildCompare <- function (results, var = "joint", yLab = NULL, dodge = 1.5) {
  d <- dplyr::bind_rows(lapply(names(results), function (nm) {
    x <- results[[nm]]
    cols <- intersect(c("country", "cohort", var, paste0(var, c("_lower", "_upper"))), names(x))
    x <- x[, cols, drop = FALSE]; x$series <- nm; x
  }))
  d$series <- factor(d$series, levels = names(results))
  lo <- paste0(var, "_lower"); hi <- paste0(var, "_upper")
  p <- ggplot(d, aes(x = cohort, y = .data[[var]], colour = series)) +
    geom_line(position = position_dodge(width = dodge), alpha = 0.5) +
    geom_point(size = 2, position = position_dodge(width = dodge)) +
    facet_wrap(~ country) +
    scale_colour_manual(values = CHILD_SERIES_COLS[seq_along(results)]) +
    labs(x = "Birth cohort of the first child", y = if (is.null(yLab)) var else yLab,
         colour = NULL) +
    theme(legend.position = "bottom")
  if (lo %in% names(d)) {
    p <- p + geom_errorbar(aes(ymin = .data[[lo]], ymax = .data[[hi]]), width = 0,
                           position = position_dodge(width = dodge))
  }
  p
}

# Stacked shares of the first ten years by the mother's state, by cohort.
plotChildShares <- function (res, title = NULL) {
  d <- res %>%
    dplyr::select(country, cohort, dplyr::all_of(CHILD_STATES)) %>%
    tidyr::pivot_longer(dplyr::all_of(CHILD_STATES), names_to = "state", values_to = "share") %>%
    dplyr::filter(!is.na(share)) %>%
    dplyr::mutate(state = factor(CHILD_STATE_LABELS[state], levels = CHILD_STATE_LABELS))
  ggplot(d, aes(x = cohort, y = share, fill = state)) +
    geom_col(position = position_stack(reverse = TRUE), width = 4) +
    facet_wrap(~ country) +
    scale_fill_manual(values = c("#cde2fb", "#2a78d6", "#eb6834", "#eda100")) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Birth cohort of the first child", y = "Share of the first ten years",
         fill = NULL, title = title) +
    theme(legend.position = "bottom")
}

# ==== 8. Figures 8 and 9 of the paper ====

# Figure 9: proportion of first-born children who were born in their mother's
# first union and were still living in it at age 10, standardised Aalen-Johansen
# estimates by five-year birth cohort, with 95 per cent bootstrap intervals.
plotFigure9 <- function (resAJ, cohortRange = c(1975, 2010),
                         colours = c(MEXICO = "#d62728", USA = "#1f77b4")) {
  d <- resAJ$std
  d <- d[!is.na(d$joint) & d$cohort >= cohortRange[1] & d$cohort <= cohortRange[2], ]
  d$mid <- d$cohort + 2
  p <- ggplot(d, aes(x = mid, y = joint, colour = country)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.2)
  if ("joint_lower" %in% names(d)) {
    p <- p + geom_errorbar(aes(ymin = joint_lower, ymax = joint_upper), width = 0.8)
  }
  p + scale_colour_manual(values = colours) +
    scale_y_continuous(labels = scales::percent, limits = c(0.3, 0.85)) +
    scale_x_continuous(breaks = seq(cohortRange[1], cohortRange[2] + 5, 5)) +
    labs(x = "Year of birth of the first child", y = "Proportion", colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# Figure 8: share of the first ten years of first-born children spent in each
# union state of the mother, standardised Aalen-Johansen estimates by five-year
# birth cohort, one bar per cohort. Same four states and colours as plot10Years_child(secondUnions = TRUE).
plotFigure8 <- function (resAJ, cohortRange = c(1975, 2010),
                         fills = c("#C6DBEF", "#6BAED6", "#2171B5", "#08306B")) {
  labs8 <- c(noUnion = "Before union", union1 = "During first union",
             outUnion = "Separated or widowed", union2plus = "During second+ unions")
  d <- resAJ$std %>%
    dplyr::filter(cohort >= cohortRange[1], cohort <= cohortRange[2], !is.na(union1)) %>%
    dplyr::mutate(mid = cohort + 2) %>%
    dplyr::select(country, mid, dplyr::all_of(CHILD_STATES)) %>%
    tidyr::pivot_longer(dplyr::all_of(CHILD_STATES), names_to = "state", values_to = "share") %>%
    dplyr::mutate(state = factor(labs8[state], levels = labs8))
  ggplot(d, aes(x = mid, y = share, fill = state)) +
    geom_col(position = position_stack(reverse = TRUE), width = 4.4) +
    facet_wrap(~ country) +
    scale_fill_manual(values = setNames(fills, labs8)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(cohortRange[1], cohortRange[2] + 5, 5)) +
    labs(x = "Year of birth of the first child", y = "Proportion", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# ==== 9. Lexis diagram of the selection (Annex figure) ====

# Two panels.
# A: one survey (2012, women 15-44) and five women who had their first child in
#    1990 at ages 18 to 34. Their life lines climb at 45 degrees; only those
#    still aged 44 or less in 2012 are interviewed, i.e. those who were at most
#    22 at the birth. The shaded triangle is the set of births (year, age of the
#    mother) that the 2012 survey can see.
# B: every survey used, as the line of the oldest mother at the birth that it
#    can include, min(35, A - (s - year)), for children born at least ten years
#    before the survey. For a given birth cohort (vertical band), each line
#    crossing it is one survey; everything above the line is missing from it.
plotLexisSelection <- function (
    surveysUSA = data.frame(label = c("1973", "1976", "1982", "1988", "1995", "2002", "2006-10",
                                      "2011-13", "2013-15", "2015-17", "2022-23"),
                            year  = c(1973, 1976, 1982, 1988, 1995, 2002, 2008, 2012, 2014,
                                      2016, 2022.5),
                            amax  = c(rep(44, 9), 49, 49)),
    surveysMEX = data.frame(label = c("ENADID 1997", "ENADID 2009", "ENADID 2014", "EDER 2017",
                                      "ENADID 2018", "ENADID 2023", "EDER 2025"),
                            year  = c(1997, 2009, 2014, 2017, 2018, 2023, 2025),
                            amax  = rep(54, 7)),
    maxAgeBirth = 35, minAgeBirth = 15, window = 10,
    bands = data.frame(x0 = c(1985, 2005), x1 = c(1990, 2010), lab = c("1985-89", "2005-09"))) {

  cA <- c(obs = "#2a78d6", miss = "#eb6834")

  # --- panel A ---
  s <- 2012; A <- 44; t0 <- 1990
  ages  <- c(18, 22, 26, 30, 34)
  lines <- data.frame(a = ages, x0 = t0, y0 = ages, x1 = s, y1 = ages + (s - t0))
  lines$status <- ifelse(lines$y1 <= A, "obs", "miss")
  tri <- data.frame(x = c(1985, 1985, s, s), y = c(minAgeBirth, A - (s - 1985), A, minAgeBirth))
  pA <- ggplot() +
    geom_polygon(data = tri, aes(x, y), fill = "#cde2fb", alpha = 0.7) +
    geom_segment(aes(x = s, xend = s, y = minAgeBirth, yend = A), linewidth = 2.2, colour = "#1c1c1a") +
    geom_segment(data = lines, aes(x = x0, y = y0, xend = x1, yend = y1, colour = status,
                                   linetype = status), linewidth = 0.9) +
    geom_point(data = lines, aes(x = x0, y = y0), size = 2.6, colour = "#1c1c1a") +
    geom_point(data = lines, aes(x = x1, y = y1, colour = status), size = 2.6) +
    geom_text(data = lines, aes(x = x0 - 0.6, y = y0, label = paste0(a, " at the birth")),
              hjust = 1, size = 3.2) +
    geom_text(data = lines, aes(x = x1 + 0.6, y = y1,
                                label = paste0(y1, ifelse(status == "obs", ": interviewed",
                                                          ": not interviewed"))),
              hjust = 0, size = 3.2) +
    annotate("text", x = s - 0.6, y = 30, label = "Survey 2012:\nwomen aged 15-44",
             hjust = 1, size = 3.2, fontface = "bold") +
    annotate("text", x = 2002, y = 16.6, label = "Births the 2012\nsurvey can see", size = 3.2,
             colour = "#1c5cab") +
    annotate("text", x = 1978.5, y = 67.5, hjust = 0, vjust = 1, size = 3.2,
             label = paste0("2012 - 1990 = 22 years. Women interviewed in 2012\n",
                            "are at most 44, so they were at most 44 - 22 = 22\n",
                            "when their child was born in 1990. Mothers who were\n",
                            "older at the birth are over 44 in 2012.")) +
    scale_colour_manual(values = cA, guide = "none") +
    scale_linetype_manual(values = c(obs = "solid", miss = "dashed"), guide = "none") +
    scale_x_continuous(breaks = seq(1985, 2015, 5), limits = c(1978, 2027)) +
    scale_y_continuous(breaks = seq(15, 60, 5), limits = c(12, 68)) +
    coord_fixed(ratio = 1) +
    labs(title = "A. Women who had their first child in 1990, seen from the 2012 survey",
         x = "Calendar year", y = "Age of the mother") +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())

  # --- panel B ---
  lim <- function (sv, country) {
    do.call(rbind, lapply(seq_len(nrow(sv)), function (i) {
      x <- seq(sv$year[i] - (sv$amax[i] - minAgeBirth), sv$year[i] - window, by = 0.25)
      data.frame(country = country, survey = sv$label[i], upper = paste0("women up to ", sv$amax[i]),
                 x = x, y = pmin(maxAgeBirth, sv$amax[i] - (sv$year[i] - x)))
    }))
  }
  d <- rbind(lim(surveysUSA, "United States (NSFG)"), lim(surveysMEX, "Mexico (ENADID, EDER)"))
  d$country <- factor(d$country, levels = c("United States (NSFG)", "Mexico (ENADID, EDER)"))
  ends <- d %>% dplyr::group_by(country, survey) %>% dplyr::slice_max(x, n = 1) %>% dplyr::ungroup() %>%
    dplyr::group_by(country) %>% dplyr::arrange(x, .by_group = TRUE) %>%
    dplyr::mutate(ylab = maxAgeBirth + 0.8 + 1.15 * ((dplyr::row_number() - 1) %% 4)) %>% dplyr::ungroup()
  bd <- merge(bands, data.frame(country = levels(d$country)))
  bd$country <- factor(bd$country, levels = levels(d$country))
  pB <- ggplot() +
    geom_rect(data = bd, aes(xmin = x0, xmax = x1, ymin = minAgeBirth, ymax = maxAgeBirth),
              fill = "#e6e5df", alpha = 0.8) +
    geom_text(data = bd, aes(x = (x0 + x1) / 2, y = minAgeBirth - 0.9, label = lab), size = 3, fontface = "bold") +
    geom_hline(yintercept = maxAgeBirth, linetype = "dotted", colour = "#6b6a64") +
    geom_line(data = d, aes(x, y, group = survey, colour = upper), linewidth = 0.9) +
    geom_segment(data = ends, aes(x = x, xend = x, y = y, yend = ylab - 0.4), colour = "#b5b4ad",
                 linewidth = 0.3) +
    geom_label(data = ends, aes(x = x, y = ylab, label = survey), size = 2.6, label.size = 0,
               label.padding = unit(0.08, "lines"), fill = "white") +
    facet_wrap(~ country, ncol = 1) +
    scale_colour_manual(values = c("women up to 44" = "#2a78d6", "women up to 49" = "#eb6834",
                                   "women up to 54" = "#1baf7a"), name = "Survey age limit") +
    scale_x_continuous(breaks = seq(1950, 2015, 5), limits = c(1943, 2016)) +
    scale_y_continuous(breaks = seq(15, 35, 5), limits = c(minAgeBirth - 1.5, maxAgeBirth + 4.6)) +
    labs(title = "B. Oldest mother at the birth that each survey can include",
         x = "Year of birth of the first child", y = "Age of the mother at the birth") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom",
          strip.text = element_text(face = "bold", hjust = 0))

  list(A = pA, B = pB)
}
# <<< Claude 2026-09-23
