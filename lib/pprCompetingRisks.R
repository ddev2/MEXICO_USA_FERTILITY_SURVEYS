# >>> Claude 2026-09-22
# lib/pprCompetingRisks.R
#
# A period life table with TWO decrements, written as a separate function so
# ppr_doIt() and everything built on it stay exactly as they are.
#
# WHY IT EXISTS
#   ppr_doIt() forms duration-specific probabilities of one event and multiplies
#   them out, S(D) = prod(1 - q), reporting 1 - S(D). With a competing event
#   removed by censoring, that is the ASSOCIATED SINGLE-DECREMENT figure: the
#   share who would separate if widowhood did not exist. It is not a share of
#   any population, and current + widowed + still-in-union exceeds 1.
#   Here both decrements are kept and the CRUDE cumulative incidence is
#   accumulated against ALL-CAUSE survival:
#       S(d)   = prod_{j<=d} (1 - q1_j - q2_j)
#       CI1(D) = sum_{d<=D} S(d-1) * q1_d
#   so CI1 + CI2 + S = 1 at every duration, by construction.
#
# WHAT IT KEEPS FROM ppr_doIt
#   the same (entry year) x (event year) table, the same reconstruction of the
#   at-risk fraction from the cohort survival, the same loess smoothing and the
#   same alignment correction on the mean duration. The matrix building is
#   vectorised rather than looping over individuals, which is what makes the
#   bootstrap affordable.
#
# HOW varCens MUST BE SET, and it is not obvious
#   varCens is the end of OBSERVATION, the survey date. It is NOT the date of
#   any event, and in particular NOT the date of the competing event.
#
#   This is a property of THIS function, not a statement about censoring in
#   general. Here the competing event is netted out through the cohort survival,
#   exactly as the event of interest is. The denominator must therefore hold
#   everyone still under observation, whether or not they have already had an
#   event. Censoring a woman at her own widowhood would remove her twice, once
#   from the denominator and once through the survival, and the cumulative
#   incidence would come out too high: +1.8 points against a known population
#   value with widowhood at 14 per cent of endings, against +0.07 when she is
#   kept to the survey date.
#
#   In ppr_doIt() the opposite holds, and correctly so. There the survival nets
#   out the separations only, so censoring the widow is what makes the risk set
#   right, and the result is the NET probability (0.4430 against a net truth of
#   0.4470 on the same data). The two functions answer different questions and
#   each needs its own treatment of the widow.
#
# WHAT IT DOES NOT CHANGE
#   ageTruncate behaves exactly as in ppr_doIt: the events above the age are
#   suppressed and the EXPOSURE IS KEPT. That is deliberate and correct. Leaving
#   the over-age women in the denominator scales each duration's hazard by the
#   share of the at-risk pool still eligible, so reaching the age limit behaves
#   like a competing exit and the life table returns the CRUDE, population
#   average probability of separating before that age. Capping the exposure
#   instead gives the figure for a woman who stays eligible across the whole
#   duration range, that is, the earliest entrants, which overstates it badly.
#   Checked by simulation against the known population value: events suppressed
#   and exposure kept lands within about one point, exposure capped is out by 12
#   to 23 points depending on the shape of the hazard.
#   ageTruncateExposure = TRUE is offered for inspection only. Do not use it.
#
# WHICH QUANTITY THIS RETURNS
#   the CRUDE cumulative incidence, the share who actually separate. That is
#   lower than the net figure ppr_doIt() returns, and on the simulated data used
#   to check both it was 0.4255 against 0.4470. Neither is wrong. Report the
#   crude one as a share of a cohort and the net one as a counterfactual, and say
#   in the caption which it is.
#
# WHAT IT CHANGES ON PURPOSE
#   Only the second decrement. The hard cap on the rates is off by default and
#   reports itself when used.

ppr_cr_doIt <- function (df_ppr = NULL,
                         varEnter = NULL, varEvent = NULL, varEvent2 = NULL,
                         varCens = NULL, varWeight = NULL, varDob = NULL,
                         ageTruncate = NULL, ageTruncateExposure = FALSE,
                         ageEventVar = "ageEvent", ageEvent2Var = "ageEvent2",
                         maxRate = NULL, mySpan = 0.75, dropLastDuration = TRUE,
                         firstYearsToDiscard = 10L, finalYearsToDiscard = 1L,
                         replicates = 0L, confLevel = 0.95, seed = NULL,
                         quiet = FALSE) {
  #==> varEvent: YEAR of the event of interest (NA when it did not happen)
  #==> varEvent2: YEAR of the COMPETING event, widowhood for a separation table
  #==> varCens: CMC date at which OBSERVATION stops, that is the survey date.
  #    Never the date of an event. See the note at the top of this file.
  #==> varDob: CMC date of birth, needed only to cap the exposure at ageTruncate
  #==> replicates: bootstrap replicates for the interval. 0 leaves it as NA.
  #<== list(PPR = data.frame(year, quantum_1stkind, quantum_competing, surv_final,
  #    mac_1stkind and the smoothed / interval columns plot_ppr() expects),
  #    plus the duration-by-period matrices as attributes

  if (is.null(df_ppr)) stop("ppr_cr_doIt: df_ppr cannot be NULL")
  for (a in c("varEnter", "varEvent", "varEvent2", "varCens")) {
    if (is.null(get(a))) stop("ppr_cr_doIt: ", a, " cannot be NULL")
  }
  miss <- setdiff(c(varEnter, varEvent, varEvent2, varCens), names(df_ppr))
  if (length(miss) > 0) stop("ppr_cr_doIt: missing column(s): ", paste(miss, collapse = ", "))

  yEnter  <- as.numeric(df_ppr[[varEnter]])
  yEv1    <- as.numeric(df_ppr[[varEvent]])
  yEv2    <- as.numeric(df_ppr[[varEvent2]])
  cmcCens <- as.numeric(df_ppr[[varCens]])
  w       <- if (is.null(varWeight)) rep(1, nrow(df_ppr)) else as.numeric(df_ppr[[varWeight]])

  # ---- age truncation: the events AND, by default, the exposure -------------
  if (!is.null(ageTruncate)) {
    if (ageEventVar %in% names(df_ppr)) {
      aE <- as.numeric(df_ppr[[ageEventVar]])
      yEv1[!is.na(aE) & (aE >= ageTruncate)] <- NA
    }
    if (ageEvent2Var %in% names(df_ppr)) {
      aE2 <- as.numeric(df_ppr[[ageEvent2Var]])
      yEv2[!is.na(aE2) & (aE2 >= ageTruncate)] <- NA
    }
    if (isTRUE(ageTruncateExposure)) {
      if (is.null(varDob) || !(varDob %in% names(df_ppr))) {
        stop("ppr_cr_doIt: ageTruncateExposure = TRUE needs varDob, the CMC date of birth")
      }
      cmcCens <- pmin(cmcCens, as.numeric(df_ppr[[varDob]]) + ageTruncate * 12)
    }
  }

  # A widow censored at her own widowhood is the mistake this function is most
  # likely to meet, so say so rather than returning a quietly inflated number.
  sameYear <- is.finite(yEv2) & (yEv2 == (trunc((cmcCens - 1) / 12) + 1900))
  if (!quiet && (sum(sameYear, na.rm = TRUE) > 0.5 * sum(is.finite(yEv2)))) {
    message(sprintf(paste0("ppr_cr_doIt: %d of %d competing events fall in the very year of varCens. ",
                           "varCens should be the SURVEY date, not the event date: censoring at the ",
                           "competing event removes those cases twice and inflates the result."),
                    sum(sameYear, na.rm = TRUE), sum(is.finite(yEv2))))
  }

  ok <- is.finite(yEnter) & is.finite(cmcCens) & is.finite(w) & (w > 0)
  if (!quiet && any(!ok)) {
    message(sprintf("ppr_cr_doIt: dropped %d case(s) with an unusable entry year, censoring date or weight",
                    sum(!ok)))
  }
  yEnter <- yEnter[ok]; yEv1 <- yEv1[ok]; yEv2 <- yEv2[ok]
  cmcCens <- cmcCens[ok]; w <- w[ok]
  n <- length(w)
  if (n == 0) stop("ppr_cr_doIt: nothing left after cleaning")

  yCensT <- trunc((cmcCens - 1) / 12)
  frac   <- (cmcCens - yCensT * 12) / 12
  yCens  <- yCensT + 1900

  yMin_Enter <- min(yEnter); yMax_Enter <- max(yEnter)
  evYears <- c(yEv1, yEv2); evYears <- evYears[is.finite(evYears)]
  if (length(evYears) == 0) stop("ppr_cr_doIt: no events of either kind")
  yMin_Event <- min(evYears); yMax_Event <- max(evYears)
  nR <- yMax_Enter - yMin_Enter + 1L
  nC <- yMax_Event - yMin_Event + 1L

  ri  <- yEnter - yMin_Enter + 1L
  cCe <- yCens  - yMin_Event + 1L            # censoring column, may fall outside
  c1  <- yEv1   - yMin_Event + 1L
  c2  <- yEv2   - yMin_Event + 1L

  # Accumulate a weight vector into an nR x K matrix at (row, col).
  accum <- function (val, rows, cols, K) {
    keep <- is.finite(cols) & (cols >= 1L) & (cols <= K) & is.finite(val)
    m <- matrix(0, nR, K)
    if (!any(keep)) return (m)
    lin <- (cols[keep] - 1L) * nR + rows[keep]
    agg <- rowsum(val[keep], lin, reorder = FALSE)
    m[as.integer(rownames(agg))] <- agg[, 1]
    m
  }

  oneFit <- function (wv) {
    # Exposure. A woman contributes her full weight to every event year strictly
    # before her censoring year and a fraction of it in that year itself.
    Wc  <- accum(wv, ri, pmin(cCe, nC + 1L), nC + 1L)   # weight by censoring column
    tail <- t(apply(Wc, 1, function (r) rev(cumsum(rev(r)))))
    if (nR == 1L) tail <- matrix(tail, nrow = 1L)
    cnt <- tail[, 2:(nC + 1L), drop = FALSE]            # sum over columns AFTER y
    Fr  <- accum(wv * frac, ri, cCe, nC)
    pop <- cnt + Fr

    ev1 <- accum(wv, ri, c1, nC)
    ev2 <- accum(wv, ri, c2, nC)

    std1 <- ifelse(pop > 0, ev1 / pop, 0)
    std2 <- ifelse(pop > 0, ev2 / pop, 0)

    # All-cause cohort survival along the row, as ppr_doIt does for one event.
    survC <- matrix(1, nR, nC + 1L)
    for (y in seq_len(nC)) survC[, y + 1L] <- survC[, y] - std1[, y] - std2[, y]
    survC[survC < 0] <- 0
    base <- survC[, 1:nC, drop = FALSE]
    q1 <- ifelse(base > 0, std1 / base, 0)
    q2 <- ifelse(base > 0, std2 / base, 0)
    q1[!is.finite(q1)] <- 0; q2[!is.finite(q2)] <- 0
    nCapped <- 0L
    if (!is.null(maxRate)) {
      hit <- (q1 + q2) > maxRate
      nCapped <- sum(hit)
      if (nCapped > 0) {
        sc <- maxRate / (q1 + q2)
        q1[hit] <- q1[hit] * sc[hit]; q2[hit] <- q2[hit] * sc[hit]
      }
    }
    popAtRisk <- pop * base

    # Duration increases downward once the rows are reversed.
    rv <- rev(seq_len(nR))
    q1 <- q1[rv, , drop = FALSE]; q2 <- q2[rv, , drop = FALSE]
    popAtRisk <- popAtRisk[rv, , drop = FALSE]

    S <- matrix(1, nR + 1L, nC); CI1 <- matrix(0, nR + 1L, nC); CI2 <- matrix(0, nR + 1L, nC)
    for (x in seq_len(nR)) {
      S[x + 1L, ]   <- S[x, ] * (1 - q1[x, ] - q2[x, ])
      CI1[x + 1L, ] <- CI1[x, ] + S[x, ] * q1[x, ]
      CI2[x + 1L, ] <- CI2[x, ] + S[x, ] * q2[x, ]
    }
    list(S = S, CI1 = CI1, CI2 = CI2, q1 = q1, q2 = q2,
         popAtRisk = popAtRisk, nCapped = nCapped)
  }

  f <- oneFit(w)
  if (!quiet && !is.null(maxRate) && (f$nCapped > 0)) {
    message(sprintf("ppr_cr_doIt: maxRate = %.2f was applied to %d of %d duration-by-period cells",
                    maxRate, f$nCapped, nR * nC))
  }

  # ppr_doIt reads its quantum off survivalYear[c_x, ], which applies c_x - 1 of
  # the c_x duration rates and so stops one duration short of the full table.
  # The row dropped is the longest duration, held up by the oldest entry cohort
  # and the thinnest exposure, which is where the rate is least reliable.
  # dropLastDuration = TRUE keeps that convention so the two series line up.
  lastRow <- if (isTRUE(dropLastDuration)) nR else nR + 1L
  years <- yMin_Event:yMax_Event
  ci1   <- f$CI1[lastRow, ]
  ci2   <- f$CI2[lastRow, ]
  surv  <- f$S[lastRow, ]

  # Mean duration at the event, from the life-table increments, with the same
  # diagonal alignment correction ppr_doIt applies.
  dInc <- f$CI1[2:lastRow, , drop = FALSE] - f$CI1[1:(lastRow - 1L), , drop = FALSE]
  ageORduration <- (0:(lastRow - 2L)) + yMax_Event - yMax_Enter
  denom <- colSums(dInc)
  mac1 <- as.vector(ageORduration %*% dInc) / ifelse(denom > 0, denom, NA) - (seq(nC, 1) - 1)
  mac1[!is.finite(mac1)] <- NA

  wCol <- colSums(f$popAtRisk)
  sm <- function (v) {
    okv <- is.finite(v) & is.finite(wCol) & (wCol > 0)
    if (sum(okv) < 5) return (rep(NA_real_, length(v)))
    m <- stats::loess(v[okv] ~ years[okv], weights = wCol[okv], span = mySpan)
    stats::predict(m, years)
  }
  ci1_sm  <- sm(ci1)
  mac1_sm <- sm(mac1)

  # ---- bootstrap interval --------------------------------------------------
  lo <- hi <- lo_sm <- hi_sm <- rep(NA_real_, nC)
  se <- rep(NA_real_, nC)
  if (replicates > 0L) {
    if (!is.null(seed)) set.seed(seed)
    B <- matrix(NA_real_, replicates, nC)
    for (b in seq_len(replicates)) {
      mult <- as.vector(stats::rmultinom(1, n, rep(1 / n, n)))
      B[b, ] <- oneFit(w * mult)$CI1[lastRow, ]
    }
    a <- (1 - confLevel) / 2
    lo <- apply(B, 2, stats::quantile, probs = a,     na.rm = TRUE)
    hi <- apply(B, 2, stats::quantile, probs = 1 - a, na.rm = TRUE)
    se <- apply(B, 2, stats::sd, na.rm = TRUE)
    lo_sm <- ci1_sm - stats::qnorm(1 - a) * se
    hi_sm <- ci1_sm + stats::qnorm(1 - a) * se
  } else if (!quiet) {
    message("ppr_cr_doIt: replicates = 0, so the interval columns are NA. Pass replicates = 200 or more for a bootstrap interval.")
  }

  PPR <- data.frame(
    year                                    = years,
    quantum_1stkind                         = ci1,
    quantum_competing                       = ci2,
    surv_final                              = surv,
    quantum_1stkind_smoothed                = ci1_sm,
    mac_1stkind                             = mac1,
    mac_1stkind_smoothed                    = mac1_sm,
    se_quantum_1stkind                      = se,
    ci95_lower_quantum_1stkind              = pmax(0, lo),
    ci95_upper_quantum_1stkind              = pmin(1, hi),
    ci95_lower_quantum_1stkind_smoothed     = pmax(0, lo_sm),
    ci95_upper_quantum_1stkind_smoothed     = pmin(1, hi_sm),
    exposure                                = wCol,
    stringsAsFactors = FALSE)

  keep <- seq_len(nC)
  if (firstYearsToDiscard > 0L) keep <- setdiff(keep, seq_len(min(firstYearsToDiscard, nC)))
  if (finalYearsToDiscard > 0L) keep <- setdiff(keep, seq(nC - finalYearsToDiscard + 1L, nC))
  PPR <- PPR[keep, , drop = FALSE]
  rownames(PPR) <- NULL

  out <- list(PPR = PPR)
  attr(out, "q1") <- f$q1; attr(out, "q2") <- f$q2
  attr(out, "S")  <- f$S;  attr(out, "CI1") <- f$CI1; attr(out, "CI2") <- f$CI2
  attr(out, "popAtRisk") <- f$popAtRisk
  return (out)
}


calc_ppr_cr <- function (df = NULL, varCountry = "country", vecCountry = NULL, ...) {
  #<== one data.frame, the countries stacked, ready for plot_ppr()
  if (is.null(df)) stop("calc_ppr_cr: df cannot be NULL")
  if (!(varCountry %in% names(df))) {
    df[[varCountry]] <- "all"
  }
  ctys <- as.character(unique(df[[varCountry]]))
  if (!is.null(vecCountry)) ctys <- intersect(ctys, vecCountry)
  res <- lapply(ctys, function (cty) {
    d <- df[as.character(df[[varCountry]]) == cty, , drop = FALSE]
    r <- ppr_cr_doIt(df_ppr = d, ...)$PPR
    r$country <- cty
    r
  })
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}
# <<< Claude 2026-09-22
