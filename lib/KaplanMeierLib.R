library(tidyverse)
library2 ("scam")
library(ggrepel)
library(scales)
library(haven)

yearFrom_cmc <- function (cmc) {
  return (1900+trunc((cmc-1)/12))
}
monthFrom_cmc <- function (cmc) {
  return (cmc-trunc((cmc-1)/12)*12)
}
calc_cmc <- function (month, year) {
  return ((year-1900)*12+month)
}

do_useRelativeWeights <- function (df=NULL, varWeight=NULL) {
  library(dplyr)
  # we standardize the weights by the mean, so that the sum of weights is equal to the number of cases
  # we take care of separating by country and survey
  if (nrow(df)==0) return()
  if ("country" %in% names (df)) {
    countries <- names (table(df$country))
    deleteCountry <- FALSE
  } else {
    df$country <- "all"
    countries <- c("all")
    deleteCountry <- TRUE
  }
  if ("surveyName" %in% names (df)) {
    df <- dplyr::rename(df, survey=surveyName)
    renameSurvey <- TRUE
  } else {
    renameSurvey <- FALSE
  }
  deleteSurvey <- FALSE
  
  df2 <- data.frame()
  for (country in countries) {
    dfCountry <- subset (df, df$country==country)
    dfCountry$survey <- factor(dfCountry$survey)
    if ("survey" %in% names (dfCountry)) {
      surveys <- names (table(dfCountry$survey))
      deleteSurvey <- FALSE
    } else {
      dfCountry$survey <- "all"
      surveys <- c("all")
      deleteSurvey <- TRUE
    }
    for (survey in surveys) {
      mWeight <- mean(dfCountry[dfCountry$survey==survey, varWeight])
      if (is.na(mWeight)) stop (paste0("weights bad in survey: ", survey, ", for :", country))
      dfCountry[dfCountry$survey==survey, varWeight] <- dfCountry[dfCountry$survey==survey, varWeight] / mWeight
    }
    df2 <- rbind(df2, dfCountry)
  }
  rm (df)
  
  if (isTRUE(deleteSurvey)) df2$survey <- NULL
  if (isTRUE(deleteCountry)) df2$country <- NULL
  if (isTRUE(renameSurvey)) df2 <- dplyr::rename(df2, surveyName=survey)
  
  return (df2)
}
# >>> Claude 2026-09-19
# REVIEW AND REWRITE OF THE KAPLAN-MEIER CORE. Read before accepting.
#
# Corrections
#   (1) survFunction is now the standard RIGHT-continuous estimator,
#       S(t_i) = prod_{j<=i} (1 - rate_j). The old loop used rate[i-1], so the
#       returned table was shifted by one event time and the last drop was lost.
#       An explicit origin row (duration 0, S = 1) is inserted when duration 0 is
#       not already an observed time, so the curve still starts at 1.
#   (2) The Greenwood sum now spans the same index set as the product above. The
#       old code paired S(t_{i-1}) with a variance that already included t_i.
#   (3) Truncation is applied BEFORE mirroring and on the UNWEIGHTED count
#       (eventRaw). On the mirrored side the old mask counted the dense mass near
#       zero for every row, so it never removed the sparse far tail and could
#       instead drop the rows next to zero.
#   (4) The mirrored branch is an AFFINE transform, 1 - p*S, whose delta-method
#       variance is p^2 Var(S), not ((1 - p*S)/S)^2 Var(S). The old form diverged
#       as S approached 0; in a simulation the standard error was inflated by a
#       factor above 600 in the tail.
#   (5) The adjusted bounds are now written back into confIntMax / confIntMin.
#       The old lines assigned confIntMaxAdj to itself, so the returned interval
#       belonged to the unadjusted curve.
#   (6) Sampling error in the two mixing proportions is now included (binomial,
#       Kish effective n). Set includePropVariance = FALSE to reproduce the old
#       behaviour of conditioning on them.
#   (7) varWeight = NULL is detected correctly. df_KM[, NULL] is a zero-column
#       data.frame, not NULL, so the old test made useWeights always TRUE and
#       unweighted calls failed.
#   (8) The small-sample fallback no longer invents points at -100 and -1 with a
#       column set that made rbind() fail. It returns an empty, correctly shaped
#       frame, and the left branch is simply absent.
#   (9) dfE_before_E2$cens_after_event resolved only through partial matching of
#       cens_after_event2. The intermediate columns are gone; the differences are
#       passed straight to computeKM().
#  (10) The "rate > 0.95 and fewer than 3 events -> 0.5" override is kept but is
#       now switchable, compares against the UNWEIGHTED count, reports itself, and
#       DEFAULTS TO FALSE. It is not Kaplan-Meier estimation.
#  (11) useSurvfit = TRUE delegates the estimator to survival::survfit(), with
#       confType and robustVar controlling the interval. See the argument notes.
#  (12) Missing weights are no longer replaced by 1 without a word: naWeight
#       chooses between "one", "drop" and "error", and every case is reported.
#  (13) A 'branch' column ("before" / "after") is returned, because both halves
#       of a mirrored curve now carry a row at time 0 and the sign of 'time'
#       alone no longer separates them.
#
# Known limitations, deliberately NOT patched here
#   - propEventBeforeEvent2 and propEvent2BeforeEvent are raw observed shares.
#     Anyone censored before either event lowers both, so the vertical gap at
#     time 0 widens with censoring. The correct estimator is a competing-risks
#     (Aalen-Johansen) cumulative incidence. A message is printed when the
#     censored-before-either share exceeds censWarnThreshold.
#   - Greenwood with survey weights treats weights as frequency counts and is not
#     design based. Use validate_with_bootstrap() for intervals that respect
#     strata and clusters, and rescale weights to mean 1 before trusting any
#     interval reported here.
#   - Ties (varEvent2 == varEvent) are assigned to the "event first" group. With
#     monthly CMC data these are frequent; a third simultaneous outcome at time 0
#     would be more faithful.
#   - ANTICIPATORY SELECTION. Neither half estimates anything backwards in time.
#     Each is an ordinary forward-time Kaplan-Meier, and the left one is merely
#     drawn on a reversed axis. The exposure lies in the SAMPLE SPLIT: which half
#     a woman belongs to is decided by which event came first, and that is
#     information about her future as of time 0. A woman still at risk of both
#     events belongs to neither half until her order is observed. Read the two
#     halves as descriptions of the sub-populations defined by that order, not as
#     the experience of a cohort followed forward from time 0. On the general
#     problem see Hoem and Kreyenfeld (2006), Demographic Research 15:17.
KaplanMeier <- function (df_KM=NULL, varEnter=NULL, varEvent=NULL, varCens=NULL, varWeight=NULL, varEvent2=NULL,
                         truncate=10, fixHighRates=FALSE, includePropVariance=TRUE, censWarnThreshold=0.10,
                         useSurvfit=TRUE, confType="log-log", robustVar=NULL, naWeight="drop") {
  #==> df_KM: data.frame with the dataset
  #==> varEnter: name of the column with the starting dates of being at risk of events (from example date of birth of individuals)
  #==> varEvent: name of the column with the dates of the event
  #==> varCens: name of the column with the dates at censoring
  #ALL THE DATES SHOULD BE IN CMC FORMAT, WITH 1/1/1900 AS STARTING DAY
  #==> varWeight: name of column with weights (optional: if not NULL, then we will use them)
  #==> varEvent2: name of the column with the dates of the second event (optional)
  #   If there are two events, then we will try to built a mirrored Kaplan&Meier (Billari, 2001)
  #==> truncate: hide the tail once fewer than 'truncate' UNWEIGHTED events remain (NULL to keep everything)
  #==> fixHighRates: TRUE restores the legacy override of hazards above 0.95 based on fewer than
  #    3 raw events. It is NOT Kaplan-Meier estimation: it replaces a valid hazard with 0.5 and
  #    so moves the curve. It now defaults to FALSE. With truncate at its default the override
  #    only ever fired on rows the truncation then removed, so turning it off changes nothing
  #    in practice; set it to TRUE only to reproduce an older figure exactly.
  #==> naWeight: what to do with cases whose weight is missing, one of "one", "drop", "error".
  # >>> Claude 2026-09-21
  #    Corrected note: the DEFAULT IS "drop". "one" is the OLD behaviour; it sets the weight
  #    to 1, which is reasonable when the weights are already scaled around 1 but is close to
  #    deleting the case when they are expansion factors, since a weight of 1 sits beside
  #    weights in the thousands. "drop" removes the case outright, which is usually what a
  #    missing survey weight means, and is the honest version of what "one" did in practice.
  #    "error" stops. All three report how many cases were affected.
  # <<< Claude 2026-09-21
  #==> includePropVariance: add the binomial variance of the two mixing proportions
  #==> censWarnThreshold: report when this share of the sample is censored before either event
  #==> useSurvfit: TRUE delegates the estimator to survival::survfit(). Everything else in this
  #    function (cleaning, the origin row, truncation, mirroring, the scaling of the two branches)
  #    is unchanged, so the two paths are directly comparable. With confType = "plain" and
  #    robustVar = FALSE the two agree to numerical precision; that is asserted in the tests.
  #==> confType: interval scale passed to survfit, one of "plain", "log", "log-log".
  #    "log-log" keeps the bounds inside [0, 1] by construction and behaves far better in the
  #    tails, which on a mirrored curve is most of both halves. Ignored when useSurvfit is FALSE.
  #==> robustVar: TRUE asks survfit for the infinitesimal jackknife variance instead of Greenwood.
  #    NULL (the default) resolves to TRUE whenever weights are supplied, because survey weights
  #    are not frequency counts and Greenwood is not the right variance for them. Note that this
  #    still ignores strata and clusters: for a design-based interval use survey::svykm().
  #    Ignored when useSurvfit is FALSE.
  #<== return a table with Kaplan&Meier columns

  if (exists("DEBUG2") && isTRUE(DEBUG2)) browser()

  if (is.null (df_KM)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")

  useWeights <- !is.null(varWeight)

  if (!(naWeight %in% c("one", "drop", "error"))) {
    stop("naWeight must be one of \"one\", \"drop\", \"error\"")
  }

  # Missing weights. The old code replaced them by 1 with no record, which is
  # close to deleting the case when the weights are expansion factors.
  if (useWeights) {
    nNAw <- sum(is.na(df_KM[,varWeight]))
    if (nNAw > 0) {
      if (naWeight == "error") {
        stop(sprintf("KaplanMeier: %d case(s) have a missing weight in '%s'", nNAw, varWeight))
      } else if (naWeight == "drop") {
        message(sprintf("KaplanMeier: dropped %d case(s) with a missing weight in '%s'", nNAw, varWeight))
        df_KM <- df_KM[!is.na(df_KM[,varWeight]), ]
      } else {
        message(sprintf(paste0("KaplanMeier: %d case(s) have a missing weight in '%s' and were set to 1. ",
                               "If '%s' holds expansion factors this all but removes them from the ",
                               "estimate; naWeight = \"drop\" or \"error\" may be what you want."),
                        nNAw, varWeight, varWeight))
        df_KM[is.na(df_KM[,varWeight]), varWeight] <- 1
      }
    }
    if (nrow(df_KM) > 0) {
      nBadW <- sum(df_KM[,varWeight] <= 0, na.rm = TRUE)
      if (nBadW > 0) {
        message(sprintf(paste0("KaplanMeier: %d case(s) have a weight of zero or less in '%s'; ",
                               "survfit() treats a zero weight as ambiguous and the risk set here ",
                               "will not reflect them either"), nBadW, varWeight))
      }
    }
  }

  # >>> Claude 2026-09-21
  # Cases with no entry date or no censoring date carry no exposure. survfit()
  # and aggregate() both drop them without a word, so count them here. A large
  # number usually means the frame still holds women who never entered the state
  # at all, or that a cohort was selected with df[cond, ] rather than subset():
  # base '[' returns one row of NA for every NA in 'cond'.
  nNAenter <- sum(is.na(df_KM[[varEnter]]))
  nNAcens  <- sum(is.na(df_KM[[varCens]]))
  if ((nNAenter > 0) || (nNAcens > 0)) {
    message(sprintf("KaplanMeier: %d case(s) without an entry date in '%s', %d without a censoring date in '%s'; both contribute no exposure",
                    nNAenter, varEnter, nNAcens, varCens))
  }
  # <<< Claude 2026-09-21

  # Shape of an empty result, so the return type never depends on the data.
  emptyKM <- function () {
    data.frame(time=numeric(0), event=numeric(0), eventRaw=numeric(0), number=numeric(0),
               numberRaw=numeric(0), surv=numeric(0), rate=numeric(0), survFunction=numeric(0),
               variance=numeric(0), stdErr=numeric(0), confIntMax=numeric(0), confIntMin=numeric(0),
               branch=character(0), stringsAsFactors=FALSE)
  }

  # Kish effective sample size, used for the variance of the mixing proportions.
  kishN <- function (w, n) {
    if (is.null(w) || (length(w) == 0)) return (n)
    s2 <- sum(w^2, na.rm=TRUE)
    if (!is.finite(s2) || (s2 <= 0)) return (n)
    return (sum(w, na.rm=TRUE)^2 / s2)
  }

  if (isTRUE(useSurvfit)) {
    if (!requireNamespace("survival", quietly = TRUE)) {
      stop("useSurvfit = TRUE requires the 'survival' package (it ships with R)")
    }
    if (!(confType %in% c("plain", "log", "log-log"))) {
      stop("confType must be one of \"plain\", \"log\", \"log-log\"")
    }
    if (isTRUE(fixHighRates)) {
      message("KaplanMeier: fixHighRates has no equivalent in survfit() and is ignored while useSurvfit = TRUE")
    }
  }
  useRobust <- if (is.null(robustVar)) useWeights else isTRUE(robustVar)

  # Two-sided 95% normal quantile. The old code hard-coded 1.96; survfit() uses
  # qnorm(0.975) = 1.959964, and the difference is what kept the two paths from
  # agreeing exactly. Using the quantile lets the equivalence test be exact.
  zCrit <- stats::qnorm(0.975)

  computeKM <- function (vecEnter=NULL, vecEvent=NULL, vecCens=NULL, vecWeight=NULL,
                         mirror=FALSE, truncate=10) {
    if (exists("DEBUG3") && isTRUE(DEBUG3)) browser()

    sz <- length(vecEvent)
    if (sz == 0) return (emptyKM())

    w <- if (!is.null(vecWeight)) as.numeric(vecWeight) else rep(1, sz)

    data <- data.frame(
      time      = ifelse(is.na(vecEvent), vecCens - vecEnter, vecEvent - vecEnter),
      event     = ifelse(is.na(vecEvent), 0, w),
      eventRaw  = ifelse(is.na(vecEvent), 0, 1),
      number    = w,
      numberRaw = 1
    )
    data <- aggregate(data[, c("event", "eventRaw", "number", "numberRaw")],
                      by = list(time = data$time), FUN = sum)
    data <- data[order(data$time),]
    rownames(data) <- NULL

    # Number still at risk: anyone censored exactly at t is at risk at t.
    totN <- sum(data$number)
    data$surv <- totN - c(0, cumsum(data$number)[-nrow(data)])
    data$rate <- data$event / data$surv

    if (isTRUE(useSurvfit)) {

      # survival::survfit() supplies the estimator and the interval. Everything
      # around it (the risk set above, the origin row, truncation, mirroring)
      # stays as it is, so the two paths differ only in these five columns.
      #
      # NOTE ON confType AND MIRRORING: for a mirrored curve the two halves are
      # rescaled afterwards and their bounds are rebuilt from stdErr on the plain
      # scale, so that the uncertainty in the mixing proportions can be added.
      # confType therefore shapes the interval only on the unmirrored path.
      dfFit <- data.frame(tt     = ifelse(is.na(vecEvent), vecCens - vecEnter, vecEvent - vecEnter),
                          status = as.integer(!is.na(vecEvent)),
                          wt     = w)
      fit <- survival::survfit(survival::Surv(tt, status) ~ 1, data = dfFit, weights = wt,
                               robust = useRobust, conf.type = confType)
      sf <- summary(fit, times = fit$time, extend = TRUE)

      k <- match(data$time, sf$time)
      data$rate <- ifelse(is.finite(data$rate), data$rate, 0)   # kept for diagnostics only
      data$survFunction <- sf$surv[k]
      data$stdErr       <- sf$std.err[k]
      data$variance     <- data$stdErr * data$stdErr
      data$confIntMax   <- sf$upper[k]
      data$confIntMin   <- sf$lower[k]
      # "log" and "log-log" return NA where the curve touches 1 or 0. Fill the
      # bound with the limit it is approaching, so the ribbon stays drawable.
      data$confIntMax <- ifelse(is.na(data$confIntMax), 1, data$confIntMax)
      data$confIntMin <- ifelse(is.na(data$confIntMin), 0, data$confIntMin)
      data$confIntMax <- pmin(1, data$confIntMax)
      data$confIntMin <- pmax(0, data$confIntMin)

    } else {

      # Legacy override, kept for continuity but now explicit and unweighted.
      if (isTRUE(fixHighRates)) {
        hit <- (!is.na(data$rate)) & (data$rate > 0.95) & (data$eventRaw < 3)
        if (any(hit)) {
          message(sprintf("KaplanMeier: fixHighRates replaced %d hazard(s) above 0.95 by 0.5", sum(hit)))
          data$rate[hit] <- 0.5
        }
      }
      data$rate <- ifelse(is.finite(data$rate), data$rate, 0)

      # Right-continuous Kaplan-Meier.
      data$survFunction <- cumprod(1 - data$rate)

      # Greenwood over the same index set as the product above.
      gwTerm <- data$event / (data$surv * (data$surv - data$event))
      gwTerm <- ifelse(is.finite(gwTerm), gwTerm, 0)   # n_i == d_i: S is already 0 there
      data$variance <- data$survFunction * data$survFunction * cumsum(gwTerm)
      data$stdErr <- sqrt(data$variance)
      data$confIntMax <- pmin(1, data$survFunction + zCrit * data$stdErr)
      data$confIntMin <- pmax(0, data$survFunction - zCrit * data$stdErr)
    }

    # Explicit origin, so the curve starts at 1 without shifting the estimator.
    if (!is.na(min(data$time)) && (min(data$time) > 0)) {
      origin <- data[1,]
      origin$time <- 0
      origin$event <- 0
      origin$eventRaw <- 0
      origin$number <- 0
      origin$numberRaw <- 0
      origin$surv <- totN
      origin$rate <- 0
      origin$survFunction <- 1
      origin$variance <- 0
      origin$stdErr <- 0
      origin$confIntMax <- 1
      origin$confIntMin <- 1
      data <- rbind(origin, data)
    }

    # Truncate while the frame is still ordered by increasing duration, so the
    # part removed is always the sparse long-duration tail.
    if (!is.null(truncate)) {
      keep_mask <- rev(cumsum(rev(data$eventRaw))) >= truncate
      data <- data[keep_mask, ]
    }
    if (nrow(data) == 0) return (emptyKM())

    if (mirror) {
      data$time <- -data$time
      data <- data[order(data$time),]
    }
    # 'branch' names the half of a mirrored curve each row belongs to. Both
    # halves carry a row at time 0 (the left one is the limit from below), so
    # the sign of 'time' alone does not separate them.
    data$branch <- if (mirror) "before" else "after"
    rownames(data) <- NULL

    return (data)
  }

  #clean the dataset
  df_KM <- subset(df_KM, !is.na(df_KM[,varEnter])) #'varEnter' cannot be empty
  if (nrow(df_KM) == 0) return (emptyKM())
  df_KM <- subset(df_KM, (df_KM[,varEnter] < df_KM[,varCens])) #'varEnter' cannot be after 'varCens'
  df_KM <- subset(df_KM, is.na(df_KM[,varEvent])|(df_KM[,varEvent] >= df_KM[,varEnter])) #'varEvent' should be after 'varEnter'
  df_KM <- subset(df_KM, is.na(df_KM[,varEvent])|(df_KM[,varEvent] < df_KM[,varCens]))  #'varEvent' cannot be after 'varCens'
  if (!is.null(varEvent2)) {
    df_KM <- subset(df_KM, is.na(df_KM[,varEvent2])|(df_KM[,varEvent2] < df_KM[,varCens]))
  }
  # NOTE: a subset() on !is.na(weight) stood here. It could never remove anything,
  # because the missing weights had already been replaced above. Missing weights
  # are now handled once, by naWeight, at the top of the function.
  if (nrow(df_KM) == 0) return (emptyKM())

  vecW   <- if (useWeights) df_KM[,varWeight] else NULL
  totInd <- if (useWeights) sum(df_KM[,varWeight]) else nrow(df_KM)
  if (is.na(totInd) || (totInd <= 0)) totInd <- nrow(df_KM)
  nEffTot <- kishN(vecW, nrow(df_KM))

  #mirroring?
  if (!is.null(varEvent2)) {

    # 'event' first. Ties (varEvent2 == varEvent) are counted in this group.
    dfE_before_E2 <- subset(df_KM, (!is.na(df_KM[,varEvent])) &
                              ((is.na(df_KM[,varEvent2])) | (df_KM[,varEvent2] >= df_KM[,varEvent])))
    # 'event2' strictly first.
    dfE2_before_E <- subset(df_KM, (!is.na(df_KM[,varEvent2])) &
                              ((is.na(df_KM[,varEvent])) | (df_KM[,varEvent2] < df_KM[,varEvent])))

    wA <- if (useWeights) dfE_before_E2[,varWeight] else NULL
    wB <- if (useWeights) dfE2_before_E[,varWeight] else NULL

    totA <- if (useWeights) sum(wA) else nrow(dfE_before_E2)
    totB <- if (useWeights) sum(wB) else nrow(dfE2_before_E)
    if (is.na(totA)) totA <- nrow(dfE_before_E2)
    if (is.na(totB)) totB <- nrow(dfE2_before_E)

    propEventBeforeEvent2 <- totA / totInd
    propEvent2BeforeEvent <- totB / totInd
    propNeither <- 1 - propEventBeforeEvent2 - propEvent2BeforeEvent

    if (is.finite(propNeither) && (propNeither > censWarnThreshold)) {
      message(sprintf(paste0("KaplanMeier: %.1f%% of the sample is censored before either event. ",
                             "The raw proportions scaling the two branches are biased downward and ",
                             "the gap at time 0 is widened. A competing-risks cumulative incidence ",
                             "would be the correct estimator."),
                      100 * propNeither))
    }

    # Binomial variance of the two mixing proportions. This assumes the
    # proportion and the conditional survival are independent, which they are
    # not exactly; bootstrap the whole construction for a defensible interval.
    varPropA <- 0
    varPropB <- 0
    if (isTRUE(includePropVariance) && is.finite(nEffTot) && (nEffTot > 0)) {
      varPropA <- propEventBeforeEvent2 * (1 - propEventBeforeEvent2) / nEffTot
      varPropB <- propEvent2BeforeEvent * (1 - propEvent2BeforeEvent) / nEffTot
    }

    # LEFT branch: K&M of 'event' AFTER 'event2', plotted on negative durations.
    if (nrow(dfE2_before_E) > 3) {
      dataMirror <- computeKM (rep(0, nrow(dfE2_before_E)),
                               dfE2_before_E[,varEvent] - dfE2_before_E[,varEvent2],
                               dfE2_before_E[,varCens]  - dfE2_before_E[,varEvent2],
                               wB, mirror=TRUE, truncate=truncate)
    } else {
      dataMirror <- emptyKM()
    }
    # Affine transform 1 - p*S: Var = p^2 Var(S) + S^2 Var(p).
    dataMirror$survFunctionAdj <- 1 - dataMirror$survFunction * propEvent2BeforeEvent
    dataMirror$varianceAdj <- propEvent2BeforeEvent * propEvent2BeforeEvent * dataMirror$variance +
      dataMirror$survFunction * dataMirror$survFunction * varPropB

    # RIGHT branch: K&M of 'event2' AFTER 'event'.
    if (nrow(dfE_before_E2) > 0) {
      data <- computeKM (rep(0, nrow(dfE_before_E2)),
                         dfE_before_E2[,varEvent2] - dfE_before_E2[,varEvent],
                         dfE_before_E2[,varCens]   - dfE_before_E2[,varEvent],
                         wA, mirror=FALSE, truncate=truncate)
    } else {
      data <- emptyKM()
    }
    # Product transform p*S: Var = p^2 Var(S) + S^2 Var(p).
    data$survFunctionAdj <- data$survFunction * propEventBeforeEvent2
    data$varianceAdj <- propEventBeforeEvent2 * propEventBeforeEvent2 * data$variance +
      data$survFunction * data$survFunction * varPropA

    data <- rbind(dataMirror, data)

    data$survFunction <- data$survFunctionAdj
    data$variance     <- data$varianceAdj
    data$stdErr       <- sqrt(data$varianceAdj)
    data$confIntMax   <- pmin(1, data$survFunction + zCrit * data$stdErr)
    data$confIntMin   <- pmax(0, data$survFunction - zCrit * data$stdErr)
    data$survFunctionAdj <- NULL
    data$varianceAdj     <- NULL
    rownames(data) <- NULL

    attr(data, "propEventBeforeEvent2") <- propEventBeforeEvent2
    attr(data, "propEvent2BeforeEvent") <- propEvent2BeforeEvent
    attr(data, "propNeither")           <- propNeither

  } else {
    data <- computeKM (df_KM[,varEnter], df_KM[,varEvent], df_KM[,varCens], vecW,
                       mirror=FALSE, truncate=truncate)
  }

  return (data)
}
# <<< Claude 2026-09-19

KaplanMeierPlot <- function(
    df = NULL, varEnter = NULL, varEvent = NULL, varCens = NULL,
    varWeight = NULL, varEvent2 = NULL,
    varClass = NULL, varCountry = NULL, vecCountry = NULL,
    cohortsList = NULL, var_yBirth = "yBirth",
    plotType = "step", minX = NULL, maxX = NULL,
    Title = ggplot2::waiver(),
    xTitle = "duration before / after",
    yTitle = "Survival Probability",
    inverseFunction = FALSE, confInt = TRUE,
    truncate = 10, hideLegend = TRUE,
    fixHighRates = FALSE, includePropVariance = TRUE,
    minCases = 200, minPoints = 10,
    useSurvfit = TRUE, confType = "log-log",
    robustVar = NULL, naWeight = "drop",
    estimator = if (is.null(varEvent2)) "classic" else "survfit",
    horizon = NULL, ties = "simultaneous",
    bootstrap = 0L, varStrata = NULL, varCluster = NULL,
    # >>> Claude 2026-09-21
    anchorAtOne = TRUE
    # <<< Claude 2026-09-21
    ) {
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  #==> df: data.frame with the dataset
  #==> varEnter: name of the column with the starting dates of being at risk of events (for example date of birth of individuals)
  #==> varEvent: name of the column with the dates of the event (for example date of first childbearing)
  #==> varCens: name of the column with the dates at censoring (either the survey date or when the individual exit the state of being at risk)
  #ALL THE DATES SHOULD BE IN CMC FORMAT, (USUALLY WITH 1/1/1900 AS STARTING DAY), which means that the time unit is the month
  #==> varWeight: name of one or two columns with weights (optional: if not NULL, then we will use them)
  # We consider two type of WEIGHTS:
  # 1. weights that equalizes people in the sample (for example when we have oversampling of some groups, like young or old age)
  # 2. weights that allows to retrieve numbers for the whole population of the country or the region
  # if varWeight contains two types of weight, we assume than the first are individual ones and the second population one
  # individual weights are used to compute variance and deduce confidence intervals, while population weight are used for the point estimates
  #==> varEvent2: name of the column with the dates of the second event (optional, for example date of first union)
  #=============> if there are two events, then we will try to built a mirrored Kaplan&Meier (Billare, 2001)
  #==> varClass: name of a variable for facetting (optional): for example survey name
  #==> varCountry: name of the variable containing country names for facetting (optional)
  #==> vecCountry: vector of countries to plot, selected from column 'varCountry' (optional: if not NULL, then 'varCountry' cannot be NULL)
  #==> cohortsList: list with limits for creating cohort plots (optional)
  #Example: list((c(2000, 2004))) will create a group of individuals at risk starting between year 2000 and 2004
  #Example: list((c(2000, 2004), c(2005, 2009))) will create two groups of individuals at risk...
  #Example: if 'varCountry' allows building a vector of countries, then cohortsList can contain specific limits for each country:
  #list("Austria"=(c(2000, 2004), c(2005, 2009)), ...) will create two groups of individuals at risk for 'Austria'
  # and we can add other limits for other countries afterward
  #==> var_yBirth: name of the column with the year of birth of individuals (used only if cohortsList is not NULL)
  #==============> this variable can also be year of union start cohort, and cohortsList values should be adapted
  #==> plotType: one of c('step', 'smooth') (optional: if NULL, will default to 'step')
  #==> inverseFunction: if TRUE, then we plot the function 1 - K&M curve (which increase from 0 instead of decreasing from 1)
  #==> confInf to TRUE to plot the confidence interval
  #==> truncate: we truncate the K&M curve when there are less than 'truncate' events (pass NULL if no truncation)
  # >>> Claude 2026-09-21
  #==> anchorAtOne: TRUE prepends a row at duration 0 with S = 1 to every plain
  #    (non mirrored) curve whose first observed duration is 0. Without it the step
  #    function starts at S(0), which for union -> marriage in Mexico is about 0.46,
  #    and the drop caused by the direct marriages is invisible. Set FALSE to plot
  #    the estimator exactly as KaplanMeier() returns it.
  # <<< Claude 2026-09-21
  #==> useSurvfit / confType / robustVar: passed straight to KaplanMeier(). Set useSurvfit=TRUE,
  #    confType="log-log" to get bounds that stay inside [0,1] and a variance valid for
  #    non-integer weights. See the argument notes on KaplanMeier() for the caveats.
  #==> estimator: "classic" uses KaplanMeier() from this file. "survfit" uses
  #    KaplanMeierSurvfit() from lib/KaplanMeierSurvfit.R, which scales the two halves by
  #    Aalen-Johansen branch probabilities instead of raw observed shares. Only meaningful
  #    with varEvent2; source lib/KaplanMeierSurvfit.R first.
  #==> horizon / ties: passed to KaplanMeierSurvfit(). 'horizon' is the time at which the
  #    branch probabilities are read and belongs in the figure caption; 'ties' is one of
  #    "simultaneous", "event", "event2" and decides what happens when both events share a
  #    month. Ignored when estimator = "classic".
  #==> bootstrap / varStrata / varCluster: with estimator = "survfit" and bootstrap > 0, the
  #    intervals come from that many bootstrap replicates of the complete estimator, resampling
  #    clusters within strata when those columns are named. This is the only interval here that
  #    accounts for the sampling design. It is slow; 500 replicates is a sensible floor.

  if (is.null (df)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")
  if (!is.null (vecCountry)&is.null (varCountry)) stop("varCountry cannot been NULL if vecCountry is used")
  if (!(estimator %in% c("classic", "survfit"))) stop("estimator must be \"classic\" or \"survfit\"")
  if ((estimator == "survfit") && (length(varWeight) == 2)) {
    message("KaplanMeierPlot: estimator = \"survfit\" uses only the first weight column")
  }

  twoWeights <- (length(varWeight) == 2)
  if (is.null(varWeight)) {
    varWeight <- "weight"
    df$weight <- 1
  }
  
  df2 <- df[,c(varEnter, varEvent, varCens)]
  # IMPORTANT: if date of event is the same than starting date of being at risk, we add 1 unit of time to the former in order
  # to compute a risk (WE DON'T DO IT, BUT LEFT THE CODE IN CASE NEEDED)
  # df2[[varEvent]] <- ifelse (df2[[varEvent]] == df2[[varEnter]], df2[[varEvent]] + 1, df2[[varEvent]])
  if (!is.null(var_yBirth)) df2[[var_yBirth]] <- df[[var_yBirth]]
  if (!is.null(varWeight)) df2[[varWeight[1]]] <- df[[varWeight[1]]]
  if (twoWeights) df2[[varWeight[2]]] <- df[[varWeight[2]]]
  if (!is.null(varEvent2)) df2[[varEvent2]] <- df[[varEvent2]]
  if (!is.null(varClass)) df2[[varClass]] <- df[[varClass]]
  if (!is.null(varCountry)) df2[[varCountry]] <- df[[varCountry]]
  
  df <- df2
  rm (df2)
  
  if (!is.null(names(cohortsList))) {
    df <- subset(df, df$country %in% names(cohortsList))
  }
  
  # Identify Class and Country Names
  varClassNames <- if (!is.null(varClass)) names(table(df[,varClass])) else "All"
  varCountryNames <- if (!is.null(varCountry)) names(table(df[,varCountry])) else "All"

  if (is.null(cohortsList)) cohortsList <- c(0, 10000)

  dataTot <- data.frame(time=NULL, event=NULL, number=NULL, numberRaw=NULL,
                        surv=NULL, rate=NULL, survFunction=NULL, variance=NULL,
                        stdErr=NULL, confIntMax=NULL, confIntMin=NULL, class=NULL, country=NULL, cohort=NULL)
  for (indClass in (1:length(varClassNames))) {
    dfClass <- df
    if (varClassNames[indClass] != "All") dfClass <- subset(df, df[,varClass]==varClassNames[indClass])
    for (indCountry in (1:length(varCountryNames))) {
      dfCountry <- dfClass
      if (varCountryNames[indCountry] != "All") dfCountry <- subset(dfClass, dfClass[,varCountry]==varCountryNames[indCountry])
      if ( !is.null(vecCountry) & (!(varCountryNames[indCountry] %in% vecCountry)) ) next
      if (is.null(names(cohortsList))) {
        vecCohorts <- as.vector(cohortsList)
      } else {
        vecCohorts <- cohortsList[varCountryNames[indCountry]][[1]]
        if (is.null(vecCohorts)) vecCohorts <- c(0, 10000)
      }
      for (indCohort in (1:(length(vecCohorts)/2))) {
        dfCountryCohort <- dfCountry
        startYear <- vecCohorts[(indCohort-1)*2+1]
        endYear <- vecCohorts[indCohort*2]
        if ((vecCohorts[2]!=10000)) {
          dfCountryCohort <- subset (dfCountry,
                                     (dfCountry[,var_yBirth] >= startYear) &
                                       (dfCountry[,var_yBirth] <= endYear))
        }
        # >>> Claude 2026-09-19
        # The hard-coded thresholds 200 and 10 are now the arguments minCases
        # and minPoints, so the groups that get dropped are visible to caller.
        enoughCases <- (dim(dfCountryCohort)[1] > minCases)
        # <<< Claude 2026-09-19
        if (enoughCases) {
          print (paste(varCountryNames[indCountry], "cohort", indCohort))
          # >>> Claude 2026-09-19
          # The two weight runs are now joined on 'time'. Truncation depends on
          # the event counts, so the two tables need not hold the same set of
          # event times and aligning them by row position was silently wrong.
          # The relative width of the individual-weight interval is carried over
          # to the population-weight point estimate, as before.
          if (estimator == "survfit") {
            if (is.null(varEvent2)) stop("estimator = \"survfit\" needs varEvent2")
            if (!exists("KaplanMeierSurvfit")) stop("source(\"lib/KaplanMeierSurvfit.R\") first")
            if (bootstrap > 0) {
              data <- KaplanMeierBootstrap (dfCountryCohort, varEnter, varEvent, varCens,
                                            varWeight[1], varEvent2, truncate=truncate,
                                            horizon=horizon, ties=ties, naWeight=naWeight,
                                            replicates=bootstrap, varStrata=varStrata,
                                            varCluster=varCluster)
            } else {
              data <- KaplanMeierSurvfit (dfCountryCohort, varEnter, varEvent, varCens,
                                          varWeight[1], varEvent2, truncate=truncate,
                                          horizon=horizon, ties=ties, naWeight=naWeight)
            }
          } else {
          data <- KaplanMeier (dfCountryCohort, varEnter, varEvent, varCens, varWeight[1], varEvent2,
                               truncate=truncate, fixHighRates=fixHighRates,
                               includePropVariance=includePropVariance,
                               useSurvfit=useSurvfit, confType=confType, robustVar=robustVar,
                               naWeight=naWeight)
          }
          if (twoWeights && (nrow(data) > 0) && (estimator == "classic")) {
            data2 <- KaplanMeier (dfCountryCohort, varEnter, varEvent, varCens, varWeight[2], varEvent2,
                                  truncate=truncate, fixHighRates=fixHighRates,
                                  includePropVariance=includePropVariance,
                                  useSurvfit=useSurvfit, confType=confType, robustVar=robustVar,
                                  naWeight=naWeight)
            # Join on time AND branch: a mirrored curve holds two rows at
            # time 0, one per branch, so 'time' alone is not a unique key.
            ind <- data[, c("time", "branch", "survFunction", "stdErr", "confIntMax", "confIntMin")]
            names(ind) <- c("time", "branch", "survInd", "stdErrInd", "ciMaxInd", "ciMinInd")
            data2 <- merge(data2, ind, by = c("time", "branch"), all.x = FALSE, all.y = FALSE)
            data2 <- data2[order(match(data2$branch, c("before", "after")), data2$time), ]
            ratioMax <- ifelse(data2$survInd > 0, data2$ciMaxInd / data2$survInd, 1)
            ratioMin <- ifelse(data2$survInd > 0, data2$ciMinInd / data2$survInd, 1)
            data2$stdErr <- data2$stdErrInd
            data2$confIntMax <- pmin(1, data2$survFunction * ratioMax)
            data2$confIntMin <- pmax(0, data2$survFunction * ratioMin)
            data2[, c("survInd", "stdErrInd", "ciMaxInd", "ciMinInd")] <- NULL
            rownames(data2) <- NULL
            data <- data2
          }
          # <<< Claude 2026-09-19
          nData <- nrow(data)
          if (nData > minPoints) {
            data$class <- varClassNames[indClass]
            data$country <- varCountryNames[indCountry]
            data$cohort <- paste(vecCohorts[(indCohort-1)*2+1],"-",vecCohorts[indCohort*2],sep="")
            if (vecCohorts[(indCohort-1)*2+1] >= 2010) {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1],"-",vecCohorts[indCohort*2]-2000,sep="")
            } else if (vecCohorts[(indCohort-1)*2+1] >= 2000) {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1],"-0",vecCohorts[indCohort*2]-2000,sep="")
            } else {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1]-1900,"-",vecCohorts[indCohort*2]-1900,sep="")
            }
            # >>> Claude 2026-09-19
            # With the default cohortsList c(0, 10000) the labels above read
            # "0-10000" and "-1900-8100" in the legend. Name that group "All".
            if ((startYear == 0) && (endYear == 10000)) {
              data$cohort <- "All"
              data$cohortLabel <- "All"
            }
            # <<< Claude 2026-09-19
            dataTot <- rbind(dataTot,data)
          }
        } else {
          cat (
            paste("too few cases for", varCountryNames[indCountry], "cohort", paste0(startYear,"-",endYear), "\n")
            )
        }
      }
    }
  }

  # >>> Claude 2026-09-22
  # The drawing half of this function now lives in KaplanMeierDraw(), moved
  # verbatim so nothing about the existing figures changes. Splitting it out
  # lets the Aalen-Johansen curves of section 4b be drawn in exactly the same
  # format, colours, ribbons, labels and facets, from a table this function
  # never computed.
  return (KaplanMeierDraw(dataTot,
                          plotType = plotType, minX = minX, maxX = maxX,
                          Title = Title, xTitle = xTitle, yTitle = yTitle,
                          inverseFunction = inverseFunction, confInt = confInt,
                          hideLegend = hideLegend, anchorAtOne = anchorAtOne,
                          varClassNames = varClassNames,
                          varCountryNames = varCountryNames))
  # <<< Claude 2026-09-22
}

# >>> Claude 2026-09-22
# KaplanMeierDraw: everything KaplanMeierPlot() used to do after the estimation.
# Give it a table with one row per plotted point and it returns the figure.
#==> dataTot: needs time (MONTHS), survFunction, confIntMin, confIntMax, class,
#    country and cohort. 'cohortLabel' and 'branch' are filled in when absent.
#    'cohort' should be an ordered factor, since the grey to blue to red ramp is
#    assigned in level order.
#==> anchorAtOne: see KaplanMeierPlot(). Set FALSE for a cumulative incidence,
#    which rises from 0 and must not be anchored at 1.
#==> legendTitle: "Birth Cohort" keeps the old wording; pass "Union cohort" or
#    anything else when that is what the groups are.
KaplanMeierDraw <- function (dataTot,
                             plotType = "step", minX = NULL, maxX = NULL,
                             Title = ggplot2::waiver(),
                             xTitle = "duration before / after",
                             yTitle = "Survival Probability",
                             inverseFunction = FALSE, confInt = TRUE,
                             hideLegend = TRUE, anchorAtOne = TRUE,
                             legendTitle = "Birth Cohort",
                             varClassNames = NULL, varCountryNames = NULL) {

  need <- c("time", "survFunction", "class", "country", "cohort")
  miss <- setdiff(need, names(dataTot))
  if (length(miss) > 0) stop("KaplanMeierDraw: missing column(s): ", paste(miss, collapse = ", "))
  if (!("branch" %in% names(dataTot)))      dataTot$branch <- "after"
  if (!("cohortLabel" %in% names(dataTot))) dataTot$cohortLabel <- as.character(dataTot$cohort)
  if (!("confIntMin" %in% names(dataTot)))  dataTot$confIntMin <- dataTot$survFunction
  if (!("confIntMax" %in% names(dataTot)))  dataTot$confIntMax <- dataTot$survFunction
  if (is.null(varClassNames))   varClassNames   <- as.character(unique(dataTot$class))
  if (is.null(varCountryNames)) varCountryNames <- as.character(unique(dataTot$country))

  # >>> Claude 2026-09-21
  # Restore the vertical segment at duration 0.
  #
  # KaplanMeier() inserts an origin row (duration 0, S = 1) only when duration 0
  # is not itself an observed event time. When it is, which is the case whenever a
  # sizeable share of the events happen at duration 0 (in Mexico roughly half of
  # first unions begin with the marriage, so S(0) is near 0.46), the first row of
  # the table already carries the post-event value and geom_step starts the curve
  # at that height. The mass point at duration 0 is then hidden.
  #
  # The block below prepends, for each curve, a row at the same duration with
  # S = 1 and a degenerate confidence interval. geom_step(direction = "hv") draws
  # the horizontal piece over a zero-length interval and then the vertical piece,
  # so the segment from 1 down to S(0) appears. The estimator is untouched: the
  # added row is the left limit S(0-), which equals 1 by construction.
  #
  # Mirrored curves are left alone. There the height at duration 0 is fixed by the
  # branch probability, not by 1, and the two branches already share that point.
  if (isTRUE(anchorAtOne) && (nrow(dataTot) > 0) &&
      all(c("branch", "time", "survFunction", "class", "country", "cohort") %in% names(dataTot))) {
    grpKey <- do.call(paste, c(dataTot[, c("class", "country", "cohort")], sep = "\r"))
    anchors <- do.call(rbind, lapply(split(seq_len(nrow(dataTot)), grpKey), function (ii) {
      d <- dataTot[ii, , drop = FALSE]
      if (any(d$branch == "before")) return (NULL)        # mirrored curve
      d <- d[order(d$time), , drop = FALSE]
      if ((d$time[1] > 0) || (d$survFunction[1] >= 1)) return (NULL)
      a <- d[1, , drop = FALSE]
      a$survFunction <- 1
      for (nm in c("stdErr", "variance", "event", "eventRaw", "rate")) {
        if (nm %in% names(a)) a[[nm]] <- 0
      }
      for (nm in c("confIntMax", "confIntMin")) {
        if (nm %in% names(a)) a[[nm]] <- 1
      }
      a
    }))
    if (!is.null(anchors) && (nrow(anchors) > 0)) {
      dataTot <- rbind(anchors, dataTot)
      dataTot <- dataTot[order(dataTot$class, dataTot$country, dataTot$cohort,
                               match(dataTot$branch, c("before", "after")),
                               dataTot$time, -dataTot$survFunction), ]
      rownames(dataTot) <- NULL
    }
  }
  # <<< Claude 2026-09-21

  dataTot$timeYear <- dataTot$time / 12
  
  if (is.null(minX)) {
    minX <- max(-6, trunc (min (dataTot$timeYear)))
  }
  if (is.null(maxX)) {
    maxX <- 10
  }
  
  # Plotting
  # if we want to put the labels on the right, we need to cut off the data on the right of the maxX value
  dataTot <- subset(dataTot, timeYear <= maxX)
  
  if (varCountryNames[1] != "All") {
    label_data <- dataTot %>%
      group_by(country, cohort) %>%
      dplyr::filter(timeYear == max(timeYear)) %>%
      ungroup()
  } else {
    # Get the point for each line to position the labels (in case only one country)
    label_data <- dataTot %>%
      group_by(country, cohort) %>%
      dplyr::filter(timeYear == max(timeYear)) %>%
      ungroup()
  }
  
  if (isTRUE(inverseFunction)) {
    dataTot$survFunction <- 1 - dataTot$survFunction
    dataTot$tmp <- dataTot$confIntMax
    dataTot$confIntMax <- 1 - dataTot$confIntMin
    dataTot$confIntMin <- 1 - dataTot$tmp
    dataTot$tmp <- NULL
    label_data$survFunction <- 1 - label_data$survFunction
  }
  
  nCohorts <- length(table(dataTot$cohort))
  my_colors <- colorRampPalette(c("gray85", "steelblue", "darkred"))(nCohorts)
  
  p <- ggplot(dataTot, aes(x=timeYear, y=survFunction, group=cohort, colour=cohort)) +
    geom_vline(xintercept = 0, linetype="dotted")
  
  p <- p + scale_color_manual(values=my_colors)
  p <- p + scale_x_continuous(expand = expansion(mult = 0.02)) +  # 2% padding
    scale_y_continuous(expand = expansion(mult = 0.02))  # 2% padding
    
  if (isTRUE(confInt)) {
    p <- p + geom_ribbon(aes(ymin = confIntMin, ymax = confIntMax, group=cohort, fill=cohort), 
                         alpha = 0.2, color=NA,
                         outline.type = "both")
    p <- p + scale_fill_manual(values=my_colors)
  }

  if (plotType=="step") p <- p + geom_step(direction = "hv")
  if (plotType=="smooth") p <- p + geom_smooth(method = "scam", formula = y ~ s(x, k = 15, bs = "mpd"), se = FALSE)
  
  p <- p + theme_linedraw() + labs(title=Title, y=yTitle, x=xTitle, colour=legendTitle,
                                   fill = legendTitle)
  p <- p + coord_cartesian(ylim = c(0, 1), xlim=c(minX, maxX))
  
  # Faceting
  if (varClassNames[1] != "All" && varCountryNames[1] != "All") {
    p <- p + facet_wrap(~class + country)
  } else if (varClassNames[1] != "All") {
    p <- p + facet_wrap(~class)
  } else if (varCountryNames[1] != "All") {
    # force ggrepel to show the labels outside of the lines
    p <- p + geom_point(
      data = dataTot,
      size = 0.1,
      alpha = 0  # Invisible
    )
    p <- p + facet_wrap(~country)
    p <- p + geom_text_repel(
      data = label_data,
      aes(label = cohortLabel),
      show.legend = FALSE,
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      size = 3,
      segment.size = 0.2,
      segment.color = "grey50",
      force = 8,              # Increase repulsion force (default is 1)
      force_pull = 0.5,       # How strongly labels are pulled toward points
      box.padding = 0.5,      # Padding around each label box (default 0.25)
      point.padding = 0.8,    # Padding around the data point
      min.segment.length = 0, # Always show connector segments
      max.overlaps = Inf      # Allow all labels to show
    )
    if (hideLegend) {p <- p + theme(legend.position = "none")}
  } else {
    # no faceting: only one plot
    # we associate the labels with the lines
    p <- p + geom_text_repel(data = label_data, 
                             aes(label = cohort, color = cohort),
                             direction = "y", 
                             hjust = 0,
                             force = 2,
                             nudge_x = 1,
                             segment.linetype = "dotted", # Optional: adds a small guide line
                             min.segment.length = 0, xlim=c(0, maxX)) +
      theme(legend.position = "none") +
    theme(
      panel.border = element_blank(),           # Remove panel border
      axis.line = element_line(color = "black") # Keep only bottom and left axes
    )
    p <- p + theme(plot.margin = margin(5, 5, 5, 5))
  }

  p
  
  # p <- ggplot(dataTot, aes(x=timeYear, y=survFunction, group=cohort, colour=cohort))
  # p <- p + geom_vline(xintercept = 0)
  # if (plotType=="step") p <- p + geom_step()
  # if (plotType=="smooth") p <- p + geom_smooth(method = "scam", formula = y ~ s(x, k = 50, bs = "mpd"), se = FALSE)
  # p <- p + geom_smooth(span=0.5, se = TRUE)
  # p <- p + theme_linedraw() + labs(y="Survival Probability", x="first conception: duration before / after start of first union", colour="Birth Cohort")
  # p <- p + coord_cartesian(ylim = c(0, 1), xlim=c(-6, 10))
  # p <- p + scale_x_continuous(breaks=seq(-6, 10, 2))
  # if ((varClassNames[1]!="All")&(varCountryNames[1]!="All")) {
  #   p <- p + facet_wrap(vars(class)~vars(country))
  # } else if (varClassNames[1]!="All") {
  #   p <- p + facet_wrap(vars(class))
  # } else if (varCountryNames[1]!="All") {
  #   p <- p + facet_wrap(vars(country))
  # }
 
}
# <<< Claude 2026-09-22

buildMatrix <- function(rowMin, rowMax, colMin, colMax, defaultValue=0) {
  mat <- matrix(defaultValue, ncol=(colMax - colMin + 1), nrow=(rowMax - rowMin + 1))
  colnames(mat) <- as.character(colMin:colMax)
  rownames(mat) <- as.character(rowMin:rowMax)
  return (mat)
}

buildVector <- function(colMin, colMax, defaultValue=0, numYear=NULL) {
  if (is.null(numYear)) {
    nYears <- colMax-colMin+1
  } else {
    nYears <- numYear
  }
  vec <- rep(defaultValue, nYears)
  names(vec) <- as.character((colMax-nYears+1):colMax)
  return (vec)
}

initLifeTable <- function (lg=30) {
  lifeTable <- data.frame(l0=0)
  for (ind in (1:(lg-1))) {
    lt <- data.frame(l=0)
    names(lt)[1] <- paste("l", ind, sep="")
    lifeTable <- cbind(lifeTable, lt)
  }
  return (lifeTable)
}

plotDeaths <- function (df=deathsTablePeriod, lastCol=50) {
  nRows <- dim(df)[1]
  nCols <- dim(df)[2]
  df <- df[,((nCols-lastCol+1):nCols)]
  dfPlot <- data.frame(duration=NULL,deaths=NULL,year=NULL)
  for (x in (1:lastCol)) {
    tmp <- data.frame(duration=as.integer(rownames(df)))
    tmp$deaths <- 0
    tmp$deaths[(1:(nRows-(lastCol-x)))] <- df[((lastCol-x+1):nRows),x]
    tmp$year <- colnames(df)[x]
    dfPlot <- rbind(dfPlot, tmp)
  }
  colours <- rep("grey80", lastCol)
  colours[10] <- "blue"
  colours[20] <- "green"
  colours[30] <- "black"
  colours[40] <- "orange"
  colours[49] <- "red"
  names(colours) <- colnames(df)
  small_dfPlot <- subset (dfPlot, (year %in% c(1970, 1980, 1990, 2000, 2009)))
  p <- ggplot(dfPlot, aes(x=duration, y=deaths, group=year, color=year)) + geom_smooth(se=FALSE, span=0.3)
  p <- p + scale_y_continuous(limit=c(0,NA),oob=squish)
  p <- p + scale_color_manual(values=colours)
  p <- p + theme_linedraw() + theme_text()
  p <- p + coord_cartesian(ylim=c(0, 0.05))
  p <- p + directlabels::geom_dl(data=small_dfPlot, aes(label = year), method = "first.bumpup")
  p <- p + directlabels::geom_dl(aes(label = year), method = "first.bumpup")
  p <- p + theme(legend.position="none")
  p
}

GreenwoodVar <- function (events, popAtRisk, iStart, iEnd, Sx) {
  var <- 0
  for (ind in (iStart:iEnd)) {
    var <- var + events[ind] / (popAtRisk[ind] * (popAtRisk[ind] - events[ind]))
  }
  return ((Sx^2)*var)
}

# Bootstrap validation
# Compare your variance estimates with bootstrap confidence intervals

validate_with_bootstrap <- function(df_ppr, varEnter, varEvent, varCens, 
                                    varWeight = NULL, n_bootstrap = 200) {
  cat("=== Method 2: Bootstrap Validation ===\n\n")
  cat("This compares analytical variance (Greenwood) with bootstrap SE\n")
  cat(sprintf("Running %d bootstrap iterations...\n\n", n_bootstrap))
  
  # Run your function once to get the analytical results
  # (Assuming ppr_doIt is available)
  original_result <- ppr_doIt(df_ppr, varEnter = varEnter, varEvent = varEvent, 
                              varCens = varCens, varWeight = varWeight,
                              computeVariance = TRUE)
  
  # Bootstrap resampling
  bootstrap_means <- matrix(NA, nrow = n_bootstrap, 
                            ncol = length(original_result$PPR$year))
  bootstrap_quanta <- matrix(NA, nrow = n_bootstrap, 
                             ncol = length(original_result$PPR$year))
  
  for (b in 1:n_bootstrap) {
    # Resample with replacement
    indices <- sample(1:nrow(df_ppr), nrow(df_ppr), replace = TRUE)
    df_boot <- df_ppr[indices, ]
    
    # Run analysis on bootstrap sample
    tryCatch({
      boot_result <- ppr_doIt(df_boot, varEnter = varEnter, varEvent = varEvent,
                              varCens = varCens, varWeight = varWeight,
                              computeVariance = FALSE)  # Skip variance for speed
      
      bootstrap_means[b, ] <- boot_result$PPR$mac_1stkind
      bootstrap_quanta[b, ] <- boot_result$PPR$quantum_1stkind
    }, error = function(e) {
      # Skip failed bootstrap samples
    })
    
    if (b %% 50 == 0) cat(sprintf("  Completed %d iterations\n", b))
  }
  
  # Calculate bootstrap standard errors
  boot_se_mean <- apply(bootstrap_means, 2, sd, na.rm = TRUE)
  boot_se_quantum <- apply(bootstrap_quanta, 2, sd, na.rm = TRUE)
  
  # Compare with analytical SE
  comparison_df <- data.frame(
    year = original_result$PPR$year,
    analytical_SE_mean = original_result$PPR$se_mac_1stkind,
    bootstrap_SE_mean = boot_se_mean,
    ratio_mean = original_result$PPR$se_mac_1stkind / boot_se_mean,
    analytical_SE_quantum = original_result$PPR$se_quantum_1stkind,
    bootstrap_SE_quantum = boot_se_quantum,
    ratio_quantum = original_result$PPR$se_quantum_1stkind / boot_se_quantum
  )
  
  cat("\nComparison of Analytical vs Bootstrap Standard Errors:\n")
  print(comparison_df)
  cat("\nRatio should be close to 1.0 if analytical variance is correct\n")
  cat(sprintf("Mean ratio for MAC: %.3f (SD: %.3f)\n", 
              mean(comparison_df$ratio_mean, na.rm = TRUE),
              sd(comparison_df$ratio_mean, na.rm = TRUE)))
  cat(sprintf("Mean ratio for Quantum: %.3f (SD: %.3f)\n\n", 
              mean(comparison_df$ratio_quantum, na.rm = TRUE),
              sd(comparison_df$ratio_quantum, na.rm = TRUE)))
  
  return(comparison_df)
}

ppr_doIt <- function (df_ppr=dfCountry, debugFunction=FALSE,
                      varEnter, varEvent, varCens, varWeight=NULL, country="all",
                      duration=TRUE, res_numYears=20, res_finalYearsToDiscard=1, res_firstYearsToDiscard=10,
                      useRelativeWeights=FALSE,ageTruncate=NULL,mySpan=0.75,
                      computeVariance=TRUE,
                      # >>> Claude 2026-09-22
                      replicates=200L, varStrata=NULL, varCluster=NULL,
                      confLevel=0.95, seed=NULL) {
                      # <<< Claude 2026-09-22
  
  useWeights <- !is.null(varWeight)
  if (isTRUE(useWeights) & isTRUE(useRelativeWeights)) {
    df_ppr <- do_useRelativeWeights (df_ppr, varWeight)
  }
  
  if (!is.null (ageTruncate)) {
    # if ageTruncate has a value, is not NULL, we use it to truncate the events that happen after that age
    # (we set them to NA, so that they are not counted as events, but the individuals are still at risk of events until the censoring date)
    df_ppr[,varEvent] <- ifelse ((!is.na(df_ppr$ageEvent))&(df_ppr$ageEvent >= ageTruncate), NA, df_ppr[,varEvent])
  }
  
  yMin_Enter <- min(df_ppr[,varEnter])
  yMax_Enter <- max(df_ppr[,varEnter])
  c_x <- yMax_Enter - yMin_Enter + 1
  cat <- table(df_ppr[,varEvent])
  yMin_Event <- as.integer(names(cat)[1])
  yMax_Event <- as.integer(tail(names(cat),1))
  c_y <- yMax_Event - yMin_Event + 1
  rownames(df_ppr) <- (1:dim(df_ppr)[1])
  res_numYears <- min (res_numYears, c_y-1)
  
  buildPopRow <- function(yMin_Event, yMax_Event, yCens, fracYear, weight) {
    popRow <- rep(0, (yMax_Event - yMin_Event + 1))
    dd = (yCens < yMin_Event)
    if (yCens < yMin_Event) {return (popRow)}
    popRow <- rep(weight, (yMax_Event - yMin_Event + 1))
    names (popRow) <- as.character(yMin_Event:yMax_Event)
    if (yCens < yMax_Event) {
      popRow[(yCens:yMax_Event) - yMin_Event + 1] <- 0
      popRow[yCens - yMin_Event + 1] <- fracYear * weight
    }
    return (popRow)
  }
  buildEventRow <- function(yMin_Event, yMax_Event, yEvent, weight) {
    eventRow <- rep(0, (yMax_Event - yMin_Event + 1))
    names (eventRow) <- as.character(yMin_Event:yMax_Event)
    if (!is.na(yEvent)) {eventRow[yEvent - yMin_Event + 1 ] <- weight}
    return (eventRow)
  }
  
  # >>> Claude 2026-09-22
  # The per-individual loop that used to fill these four matrices is replaced by
  # the vectorised build below. It reproduces buildPopRow() and buildEventRow()
  # exactly, EDGE CASE INCLUDED: when the censoring year is at or after the last
  # event year, buildPopRow() gives the full weight in every column and applies
  # no fraction, which is what 'fullTo' encodes. The two are asserted equal in
  # tests_pprBootstrap.R.
  #
  # The point of vectorising is speed. The loop took about 27 seconds on 400,000
  # rows, which makes a bootstrap of 200 replicates an hour and a half per
  # country. Below it is roughly two orders of magnitude faster, which is what
  # makes the interval affordable.
  nR <- yMax_Enter - yMin_Enter + 1L
  nC <- yMax_Event - yMin_Event + 1L

  vEnter <- as.numeric(df_ppr[, varEnter])
  vEvent <- as.numeric(df_ppr[, varEvent])
  vCens  <- as.numeric(df_ppr[, varCens])
  wAll   <- if (useWeights) as.numeric(df_ppr[, varWeight]) else rep(1, nrow(df_ppr))
  if (any(!is.finite(vEnter))) stop("ppr_doIt: ", sum(!is.finite(vEnter)),
                                    " case(s) have no entry year in '", varEnter, "'")
  if (any(!is.finite(vCens)))  stop("ppr_doIt: ", sum(!is.finite(vCens)),
                                    " case(s) have no censoring date in '", varCens, "'")

  yCensT   <- trunc((vCens - 1) / 12)
  fracYear <- (vCens - yCensT * 12) / 12
  yCens    <- yCensT + 1900

  riAll  <- as.integer(vEnter - yMin_Enter + 1L)
  ciAll  <- as.integer(yCens  - yMin_Event + 1L)
  ceAll  <- as.integer(vEvent - yMin_Event + 1L)
  fullTo <- ifelse(ciAll >= nC, nC, ciAll - 1L)          # last column at full weight
  ciFrac <- ifelse(ciAll <= (nC - 1L), ciAll, NA_integer_)  # column carrying the fraction

  accumCell <- function (val, rows, cols, K) {
    keep <- is.finite(val) & is.finite(rows) & is.finite(cols) & (cols >= 1L) & (cols <= K)
    m <- matrix(0, nR, K)
    if (!any(keep)) return (m)
    lin <- (cols[keep] - 1L) * nR + rows[keep]
    agg <- rowsum(val[keep], lin, reorder = FALSE)
    m[as.integer(rownames(agg))] <- agg[, 1]
    m
  }
  revCum <- function (m) {
    out <- t(apply(m, 1, function (r) rev(cumsum(rev(r)))))
    if (nR == 1L) out <- matrix(out, nrow = 1L)
    out
  }
  buildCells <- function (wv) {
    pop <- revCum(accumCell(wv, riAll, fullTo, nC)) + accumCell(wv * fracYear, riAll, ciFrac, nC)
    ev  <- accumCell(wv, riAll, ceAll, nC)
    dimnames(pop) <- dimnames(ev) <-
      list(as.character(yMin_Enter:yMax_Enter), as.character(yMin_Event:yMax_Event))
    list(pop = pop, ev = ev)
  }

  cw  <- buildCells(wAll)
  cnw <- buildCells(rep(1, length(wAll)))
  popYear       <- cw$pop
  eventYear     <- cw$ev
  popYear_noW   <- cnw$pop
  eventYear_noW <- cnw$ev
  # <<< Claude 2026-09-22
  # >>> Claude 2026-09-22
  # Everything from the standardised events to the mean duration is wrapped
  # here UNCHANGED, so the bootstrap recomputes the estimate through exactly
  # the same arithmetic as the point estimate. Nothing inside was edited; only
  # the wrapper and the return are new.
  coreFromCells <- function (popYear, eventYear, computeVariance = FALSE) {
  #standardize the event count by the population count (this way we will obtain later rates of the first and the second kind)
  #these are the d(x) of the life table
  stdEventYear <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  tmp <- which(popYear != 0)
  stdEventYear[tmp] <- eventYear[tmp] / popYear[tmp]
  #survival function by cohort
  survivalCohort <- buildMatrix(yMin_Enter, yMax_Enter, 0, c_y, defaultValue=1)
  for (y in (1:c_y)) {
    survivalCohort [,y+1] <- survivalCohort [,y] - stdEventYear[,y]
  }
  #rates of first kind
  ratesFirstKind <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  for (y in (1:c_y)) {
    ratesFirstKind [,y] <- (survivalCohort [,y] - survivalCohort [,y+1]) / survivalCohort [,y]
  }
  ratesFirstKind[is.na(ratesFirstKind)] = 0
  #avoid cases where nearly all the surviving persons make a transition to the event
  #in these cases the probability has value which can reach 1
  #we put here 0.25 instead of 1...
  ratesFirstKind[which(ratesFirstKind>0.25)] <- 0.25
  
  # Hadamard or element-wise product
  popAtRisk <- popYear * survivalCohort[,(1:(c_y))]
  
  # if (isTRUE(smoothRates)) {
  #   library(mgcv)
  #   ratesFirstKind_loess <- ratesFirstKind + 1e-10
  #   ratesFirstKind_gam <- ratesFirstKind
  #   for (y in (1:c_y)) {
  #     mod <- loess(log(ratesFirstKind_loess[,y]) ~ as.integer(rownames(ratesFirstKind)), weights = popAtRisk[,y], span=0.3)
  #     ratesFirstKind_loess[,y] <- exp(predict(mod, as.integer(rownames(ratesFirstKind))))
  #     
  #     # Use family = Gamma(link = "log") to enforce positivity
  #     model <- gam(ratesFirstKind[,y] ~ s(as.integer(rownames(ratesFirstKind))), weights = popAtRisk[,y], family = quasipoisson(link = "log"))
  #     ratesFirstKind_gam[,y] <- predict(model, type = "response")    
  #   }
  # }
  
  #survival function for each period
  survivalYear <- buildMatrix(yMin_Enter-1, yMax_Enter, yMin_Event, yMax_Event, defaultValue=1)
  #order arrays upside down in order to have age or duration increasing rowwise
  stdEventYear <- stdEventYear[order(row.names(stdEventYear),decreasing=TRUE),] 
  ratesFirstKind <- ratesFirstKind[order(row.names(ratesFirstKind),decreasing=TRUE),] 
  popAtRisk <- popAtRisk[order(row.names(popAtRisk),decreasing=TRUE),]
  survivalYear <- survivalYear[order(row.names(survivalYear),decreasing=TRUE),]
  for (x in (1:c_x)) {
    survivalYear[x+1,] <- survivalYear[x,] * (1 - ratesFirstKind[x,])
  }
  if (duration) {rownames(survivalYear) <- as.character(0:(c_x))}
  
  ##
  # VARIANCE OF SURVIVAL - Greenwood's formula ====
  ##
  
  if (computeVariance) {
    # Greenwood's formula for variance of survival function
    # Var(S(x)) = S(x)^2 * sum_{i=0}^{x-1} [ q_i / (n_i * (1 - q_i)) ]
    # where q_i = probability of event at age/duration i
    #       n_i = population at risk at age/duration i
    #       S(x) = survival probability at age/duration x
    
    varianceSurvival <- buildMatrix(yMin_Enter-1, yMax_Enter, yMin_Event, yMax_Event, defaultValue=0)
    varianceSurvival <- varianceSurvival[order(row.names(varianceSurvival),decreasing=TRUE),]
    if (duration) {rownames(varianceSurvival) <- as.character(0:(c_x))}
    
    # For each period (year/column)
    for (y in (1:c_y)) {
      cumulative_var_term <- 0
      
      # For each age/duration (row)
      for (x in (1:c_x)) {
        q_x <- ratesFirstKind[x, y]
        n_x <- popAtRisk[x, y]
        
        # Avoid division by zero and numerical issues
        if (n_x > 0 && q_x < 1 && q_x > 0) {
          # Greenwood variance component for this age/duration
          var_component <- q_x / (n_x * (1 - q_x))
          cumulative_var_term <- cumulative_var_term + var_component
        }
        
        # Variance of survival at age/duration x+1
        S_x <- survivalYear[x+1, y]
        varianceSurvival[x+1, y] <- (S_x^2) * cumulative_var_term
      }
    }
    
    # Standard error of survival
    seSurvival <- sqrt(varianceSurvival)
  }

  #deaths counts from the life table
  deathsTablePeriod <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event) 
  deathsTablePeriod <- deathsTablePeriod[order(row.names(deathsTablePeriod),decreasing=TRUE),]
  if (duration) {rownames(deathsTablePeriod) <- as.character(0:(c_x-1))}
  for (x in (1:c_x)) {
    deathsTablePeriod[x,] <- (survivalYear [x,] - survivalYear [x+1,])
  }
  
  #age (first birth) or duration (subsequent) for the last year
  # ageORduration <- yMax_Enter - as.integer(rownames(stdEventYear)[1:(length(rownames(stdEventYear)))])
  ageORduration <- c(0:(c_x-1)) + yMax_Event - yMax_Enter
  
  #mean age or duration computed from the life table
  meanFirstKind <- buildVector(yMin_Event, yMax_Event)
  meanFirstKind <- ( as.vector((ageORduration) %*% deathsTablePeriod) / colSums(deathsTablePeriod) ) - (seq(c_y, 1)-1)
  meanFirstKind[is.nan(meanFirstKind)] <- 0
  
  #mean age or duration computed from the life table
  meanSecondKind <- buildVector(yMin_Event, yMax_Event)
  meanSecondKind <- ( as.vector((ageORduration) %*% stdEventYear) / colSums(stdEventYear) ) - (seq(c_y, 1)-1)
  meanSecondKind[is.nan(meanSecondKind)] <- 0
    return (mget(c("stdEventYear", "ratesFirstKind", "popAtRisk", "survivalYear",
                   "deathsTablePeriod", "meanFirstKind", "meanSecondKind",
                   if (isTRUE(computeVariance)) c("varianceSurvival", "seSurvival")
                   else character(0)),
                 envir = environment()))
  }
  list2env(coreFromCells(popYear, eventYear, computeVariance), envir = environment())
  # <<< Claude 2026-09-22

  
  # >>> Claude 2026-09-22
  # BOOTSTRAP INTERVAL.
  #
  # The Greenwood interval further down assumes the weights are frequency counts
  # and that the sample is a simple random sample. Neither is true here: the
  # survey weights vary, so the effective sample size is well below the number
  # of women, and the design has strata and clusters. The bootstrap replaces
  # both assumptions with resampling.
  #
  # With neither varStrata nor varCluster it resamples WOMEN, which captures the
  # unequal weighting and nothing else. Name a cluster column and it resamples
  # clusters; name a stratum column as well and it resamples clusters WITHIN
  # each stratum, which is the design-based interval. Adding the design later is
  # therefore one argument, not a rewrite.
  #
  # A replicate is a reweighting rather than a re-subset: each sampling unit is
  # drawn with replacement and its multiplicity multiplies its weight. That is
  # equivalent and much faster, because the index vectors never change.
  bootQuantum <- bootMean <- NULL
  if (isTRUE(replicates > 0L)) {
    for (v in c(varStrata, varCluster)) {
      if (!(v %in% names(df_ppr))) stop("ppr_doIt: column '", v, "' not found for the bootstrap")
    }
    unitId <- if (is.null(varCluster)) seq_len(nrow(df_ppr)) else as.character(df_ppr[[varCluster]])
    strId  <- if (is.null(varStrata))  rep("1", nrow(df_ppr)) else as.character(df_ppr[[varStrata]])
    byStratum <- split(seq_along(strId), strId)
    if (!is.null(seed)) set.seed(seed)
    Q <- matrix(NA_real_, replicates, c_y)
    M <- matrix(NA_real_, replicates, c_y)
    for (bRep in seq_len(replicates)) {
      mult <- numeric(length(unitId))
      for (idx in byStratum) {
        u   <- unitId[idx]
        lev <- unique(u)
        k   <- length(lev)
        cnt <- tabulate(sample.int(k, k, replace = TRUE), nbins = k)
        mult[idx] <- cnt[match(u, lev)]
      }
      cb <- buildCells(wAll * mult)
      kb <- try(coreFromCells(cb$pop, cb$ev, computeVariance = FALSE), silent = TRUE)
      if (inherits(kb, "try-error")) next
      Q[bRep, ] <- 1 - kb$survivalYear[c_x, ]
      M[bRep, ] <- kb$meanFirstKind
    }
    alphaB <- (1 - confLevel) / 2
    qt <- function (m, p) apply(m, 2, stats::quantile, probs = p, na.rm = TRUE)
    bootQuantum <- list(se = apply(Q, 2, stats::sd, na.rm = TRUE),
                        lo = qt(Q, alphaB), hi = qt(Q, 1 - alphaB))
    bootMean    <- list(se = apply(M, 2, stats::sd, na.rm = TRUE),
                        lo = qt(M, alphaB), hi = qt(M, 1 - alphaB))
  }
  # <<< Claude 2026-09-22

  ##### loess smoothing of the results ====
  for (y in (1:c_y)) {
    # Smooth mean of first kind
    mod_mean1 <- loess(meanFirstKind ~ as.integer(names(meanFirstKind)), weights = colSums(popAtRisk), span=mySpan)
    meanFirstKind_smoothed <- predict(mod_mean1, as.integer(names(meanFirstKind)))
    
    # Smooth mean of second kind
    mod_mean2 <- loess(meanSecondKind ~ as.integer(names(meanSecondKind)), weights = colSums(popAtRisk), span=mySpan)
    meanSecondKind_smoothed <- predict(mod_mean2, as.integer(names(meanSecondKind)))
    
    # Smooth quantum of first kind
    mod_quantum1 <- loess(survivalYear[c_x,] ~ as.integer(colnames(survivalYear)), weights = colSums(popAtRisk), span=mySpan)
    survivalYear_smoothed <- predict(mod_quantum1, as.integer(colnames(survivalYear)))
    
    # Smooth quantum of second kind
    mod_quantum2 <- loess(colSums(stdEventYear) ~ as.integer(colnames(stdEventYear)), weights = colSums(popAtRisk), span=mySpan)
    stdEventYear_smoothed <- predict(mod_quantum2, as.integer(colnames(stdEventYear)))
  }
  
  ##
  # VARIANCE OF MEAN DURATION - Chiang's formula ====
  ##
  
  if (computeVariance) {
    # Chiang's formula for variance of life expectancy (mean duration)
    # Var(e_0) = sum_{x=0}^{omega} [ (A_x) * Var(q_x) ]
    # where A_x = (l_x)^2 * (e_{x+1} + 0.5)^2
    #       e_x = life expectancy at age x, computed as sum_{i=0}^{x-1} (l_i + l_{i+1})/2 for unit intervals
    #       l_x = survival probability from birth at age i
    #       l_0 = radix (initial cohort size, typically 1)
    #       Var(q_x) = variance of probability of dying at age x, estimated as q_x^2 * (1 - q_x) * D for binomial distribution,
    #                  where D is the number of events, that can be computed as D = q_x * n_x, the population at risk at age x
    #                  which explains why we can also compute Var(q_x) = q_x * (1 - q_x) / n_x

    varianceMeanFirstKind <- buildVector(yMin_Event, yMax_Event)
    varianceMeanSecondKind <- buildVector(yMin_Event, yMax_Event)

    # For each period (year/column)
    for (y in (1:c_y)) {
      # Calculate L_x (person-years lived) for each interval
      # For a unit interval: L_x = (l_x + l_{x+1}) / 2
      L_x <- numeric(c_x)
      for (x in (1:(c_x-1))) {
        L_x[x] <- (survivalYear[x, y] + survivalYear[x+1, y]) / 2
      }
      L_x[c_x] <- 0
      
      # Calculate e_x
      # Since our radix is 1 (survivalYear[1,y] = 1), we just need cumulative sum from end
      e_x <- numeric(c_x)
      for (x in (c_x:1)) {
        e_x[x] <- sum(L_x[x:c_x])
      }
      e_x[c_x] <- 0 # not necessary, but just in case

      # Apply Chiang's formula
      var_sum <- 0
      D_x <- ratesFirstKind[,y] * popAtRisk[,y]
      for (x in (1:(c_x-1))) {
        q_x <- ratesFirstKind[x, y]
        n_x <- popAtRisk[x, y]
        if (n_x > 0) var_q_x <- q_x * (1 - q_x) / n_x else var_q_x <- 0
        A_x <- survivalYear[x, y]^2 * (e_x[x+1] + 0.5)^2
        var_sum <- var_sum + (A_x) * var_q_x
      }
      
       varianceMeanFirstKind[y] <- var_sum

      # For second kind, we can use a similar approach but based on observed events
      # This is more approximate as rates of second kind don't form a proper life table
      # We'll use the same variance structure scaled by the ratio of means
      if (meanFirstKind[y] > 0) {
        scale_factor <- (meanSecondKind[y] / meanFirstKind[y])^2
        varianceMeanSecondKind[y] <- varianceMeanFirstKind[y] * scale_factor
      } else {
        varianceMeanSecondKind[y] <- NA
      }
    }
    
    # Standard errors
    seMeanFirstKind <- sqrt(varianceMeanFirstKind)
    seMeanSecondKind <- sqrt(varianceMeanSecondKind)
    
    # 95% Confidence intervals
    ci95_lower_meanFirstKind <- meanFirstKind - 1.96 * seMeanFirstKind
    ci95_upper_meanFirstKind <- meanFirstKind + 1.96 * seMeanFirstKind
    ci95_lower_meanSecondKind <- meanSecondKind - 1.96 * seMeanSecondKind
    ci95_upper_meanSecondKind <- meanSecondKind + 1.96 * seMeanSecondKind

    ci95_lower_meanFirstKind_smoothed <- meanFirstKind_smoothed - 1.96 * seMeanFirstKind
    ci95_upper_meanFirstKind_smoothed <- meanFirstKind_smoothed + 1.96 * seMeanFirstKind
    ci95_lower_meanSecondKind_smoothed <- meanSecondKind_smoothed - 1.96 * seMeanSecondKind
    ci95_upper_meanSecondKind_smoothed <- meanSecondKind_smoothed + 1.96 * seMeanSecondKind
  }
  
  
  #results: ppr from rates of first kind (2) mean age at childbearing from rates of first kind (3)
  #results: ppr or tfr from rates of second kind (4) mean age at childbearing from rates of second kind (5) number of events each year in column (6)
  #results: the first column is the year...
  k_ind_year <- 1
  k_ind_quantum_1st_kind <- 2
  k_ind_mac_1st_kind <- 3
  k_ind_quantum_2nd_kind <- 4
  k_ind_mac_2nd_kind <- 5
  k_ind_quantum_1st_kind_smoothed <- 6
  k_ind_mac_1st_kind_smoothed <- 7
  k_ind_quantum_2nd_kind_smoothed <- 8
  k_ind_mac_2nd_kind_smoothed <- 9
  k_ind_quantum_1st_kind_corrected <- 10
  k_ind_quantum_2nd_kind_corrected <- 11
  k_ind_number_events <- 12
  k_ind_pop_at_risk <- 13
  c_res <- array(0,c(res_numYears,k_ind_pop_at_risk))
  
  firstYear <- yMin_Event + res_firstYearsToDiscard
  lastYear <- yMax_Event - res_finalYearsToDiscard
  range <- lastYear - firstYear + 1
  range <- min (range, res_numYears)
  lastInd <- c_y - res_finalYearsToDiscard
  firstInd <- lastInd - range + 1
  firstYear <- lastYear - range + 1
  rangeYear <- (firstYear:lastYear)
  rangeInd <- (firstInd:lastInd)
  rangeRes <- ((res_numYears - range + 1):res_numYears)
  c_res[rangeRes, k_ind_year] <- rangeYear
  c_res[rangeRes, k_ind_quantum_1st_kind] <- 1 - survivalYear[c_x,rangeInd]
  c_res[rangeRes, k_ind_mac_1st_kind] <- meanFirstKind[rangeInd]
  c_res[rangeRes, k_ind_quantum_2nd_kind] <- colSums(stdEventYear[,rangeInd])
  c_res[rangeRes, k_ind_mac_2nd_kind] <- meanSecondKind[rangeInd]
  c_res[rangeRes, k_ind_quantum_1st_kind_smoothed] <- 1 - survivalYear_smoothed[rangeInd]
  c_res[rangeRes, k_ind_mac_1st_kind_smoothed] <- meanFirstKind_smoothed[rangeInd]
  c_res[rangeRes, k_ind_quantum_2nd_kind_smoothed] <- stdEventYear_smoothed[rangeInd]
  c_res[rangeRes, k_ind_mac_2nd_kind_smoothed] <- meanSecondKind_smoothed[rangeInd]
  c_res[rangeRes, k_ind_number_events] <- colSums(eventYear_noW[,rangeInd])
  c_res[rangeRes, k_ind_pop_at_risk] <- colSums(popAtRisk[,rangeInd])
  colnames(c_res) <- c("year", "quantum_1st_kind", "mac_1st_kind", "quantum_2nd_kind", "mac_2nd_kind",
                       "quantum_1st_kind_smoothed", "mac_1st_kind_smoothed", "quantum_2nd_kind_smoothed", "mac_2nd_kind_smoothed",
                       "quantum_1st_kind_corrected", "quantum_2nd_kind_corrected",
                       "number_events","pop_at_risk")
  
  # Prepare output list
  output_list <- list(
    PPR=data.frame(
      country=rep(country, length(rangeRes)),
      year=c_res[rangeRes,k_ind_year],
      quantum_1stkind=c_res[rangeRes, k_ind_quantum_1st_kind],
      mac_1stkind=c_res[rangeRes, k_ind_mac_1st_kind],
      quantum_2ndkind=c_res[rangeRes, k_ind_quantum_2nd_kind],
      mac_2ndkind=c_res[rangeRes, k_ind_mac_2nd_kind],
      quantum_1stkind_smoothed=c_res[rangeRes, k_ind_quantum_1st_kind_smoothed],
      mac_1stkind_smoothed=c_res[rangeRes, k_ind_mac_1st_kind_smoothed],
      quantum_2ndkind_smoothed=c_res[rangeRes, k_ind_quantum_2nd_kind_smoothed],
      mac_2ndkind_smoothed=c_res[rangeRes, k_ind_mac_2nd_kind_smoothed],
      quantum_1stkind_corrected=c_res[rangeRes, k_ind_quantum_1st_kind_corrected],
      quantum_2ndkind_corrected=c_res[rangeRes, k_ind_quantum_2nd_kind_corrected],
      nEvents=c_res[rangeRes, k_ind_number_events]
    ),
    rates1=ratesFirstKind, # rates of first kind (from the life table)
    rates2=stdEventYear # rates of second kind (standardized by the population at risk)
  )
  
  # Add variance-related outputs if computed
  if (computeVariance) {
    # Add variance and confidence intervals to the main PPR dataframe
    output_list$PPR$se_mac_1stkind <- seMeanFirstKind[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind <- ci95_lower_meanFirstKind[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind <- ci95_upper_meanFirstKind[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind_smoothed <- ci95_lower_meanFirstKind_smoothed[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind_smoothed <- ci95_upper_meanFirstKind_smoothed[rangeInd]
    output_list$PPR$se_mac_2ndkind <- seMeanSecondKind[rangeInd]
    output_list$PPR$ci95_lower_mac_2ndkind <- ci95_lower_meanSecondKind[rangeInd]
    output_list$PPR$ci95_upper_mac_2ndkind <- ci95_upper_meanSecondKind[rangeInd]
    output_list$PPR$ci95_lower_mac_2ndkind_smoothed <- ci95_lower_meanSecondKind_smoothed[rangeInd]
    output_list$PPR$ci95_upper_mac_2ndkind_smoothed <- ci95_upper_meanSecondKind_smoothed[rangeInd]
    
    # Add variance of quantum (from final survival)
    output_list$PPR$se_quantum_1stkind <- seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_lower_quantum_1stkind <- (1 - survivalYear[c_x,rangeInd]) - 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_upper_quantum_1stkind <- (1 - survivalYear[c_x,rangeInd]) + 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_lower_quantum_1stkind_smoothed <- (1 - survivalYear_smoothed[rangeInd]) - 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_upper_quantum_1stkind_smoothed <- (1 - survivalYear_smoothed[rangeInd]) + 1.96 * seSurvival[c_x, rangeInd]
    
    # >>> Claude 2026-09-22
    # Keep the Greenwood numbers under their own names so the two can be
    # compared. The columns plot_ppr() draws are replaced below.
    output_list$PPR$se_quantum_1stkind_greenwood <- seSurvival[c_x, rangeInd]
    output_list$PPR$se_mac_1stkind_greenwood     <- seMeanFirstKind[rangeInd]
    # <<< Claude 2026-09-22

    # Add full variance matrices to output
    output_list$varianceSurvival <- varianceSurvival
    output_list$seSurvival <- seSurvival
    output_list$varianceMeanFirstKind <- varianceMeanFirstKind
    output_list$varianceMeanSecondKind <- varianceMeanSecondKind
    output_list$seMeanFirstKind <- seMeanFirstKind
    output_list$seMeanSecondKind <- seMeanSecondKind
  }
  
  # >>> Claude 2026-09-22
  # The reported interval is the bootstrap one whenever replicates > 0. The
  # percentile bounds go on the raw quantum; the smoothed curve gets the
  # bootstrap standard error around itself, since a percentile of the raw
  # replicates is not a bound for a smoothed value.
  if (!is.null(bootQuantum)) {
    zB <- stats::qnorm(1 - (1 - confLevel) / 2)
    output_list$PPR$se_quantum_1stkind         <- bootQuantum$se[rangeInd]
    output_list$PPR$ci95_lower_quantum_1stkind <- pmax(0, bootQuantum$lo[rangeInd])
    output_list$PPR$ci95_upper_quantum_1stkind <- pmin(1, bootQuantum$hi[rangeInd])
    output_list$PPR$ci95_lower_quantum_1stkind_smoothed <-
      pmax(0, (1 - survivalYear_smoothed[rangeInd]) - zB * bootQuantum$se[rangeInd])
    output_list$PPR$ci95_upper_quantum_1stkind_smoothed <-
      pmin(1, (1 - survivalYear_smoothed[rangeInd]) + zB * bootQuantum$se[rangeInd])
    output_list$PPR$se_mac_1stkind             <- bootMean$se[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind     <- bootMean$lo[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind     <- bootMean$hi[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind_smoothed <-
      meanFirstKind_smoothed[rangeInd] - zB * bootMean$se[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind_smoothed <-
      meanFirstKind_smoothed[rangeInd] + zB * bootMean$se[rangeInd]
    output_list$bootstrap <- list(replicates = replicates, confLevel = confLevel,
                                  varStrata = varStrata, varCluster = varCluster)
  }
  # <<< Claude 2026-09-22

  return(output_list)
}


calc_ppr <- function (df=NULL,
                      varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL, varWeight=NULL,
                      duration=TRUE, res_numYears=30, res_finalYearsToDiscard=1, res_firstYearsToDiscard=10, res_countrySpecific_numYears=NULL,
                      useRelativeWeights=TRUE,ageTruncate=NULL,mySpan=0.75,
                      # >>> Claude 2026-09-22
                      # Passed straight to ppr_doIt(). replicates = 200 gives a
                      # bootstrap interval in place of Greenwood; name varStrata
                      # and varCluster when you have the design variables and it
                      # becomes a design-based interval. replicates = 0 keeps
                      # the old Greenwood columns.
                      replicates=200L, varStrata=NULL, varCluster=NULL,
                      confLevel=0.95, seed=NULL) {
                      # <<< Claude 2026-09-22
  if (is.null (df)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")
  if (!is.null (vecCountry)&is.null (varCountry)) stop("varCountry cannot been NULL if vecCountry is used")

  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  varCountryNames <- c("All")
  if (!is.null (varCountry)) {
    cat <- table(df[, varCountry])
    varCountryNames <- names(cat)
  }
  
  # pprsTot <- data.frame(country=NULL, year=NULL, quantum_1stkind=NULL, mac_1stkind=NULL, quantum_2ndkind=NULL, mac_2ndkind=NULL,
  #                    quantum_1stkind_smoothed=NULL, mac_1stkind_smoothed=NULL, quantum_2ndkind_smoothed=NULL, mac_2ndkind_smoothed=NULL,
  #                    quantum_1stkind_corrected=NULL, quantum_2ndkind_corrected=NULL,
  #                    nEvents=NULL)
  pprsTot <- data.frame()
  
  for (indCountry in (1:length(varCountryNames))) {
    countrySel <- varCountryNames[indCountry]
    if (countrySel != "All") {
      print (countrySel)
      dfCountry <- subset(df, country==countrySel)
      #specific number of years for this country?
      if (!is.null(res_countrySpecific_numYears)) {
        if (countrySel %in% res_countrySpecific_numYears$country)
        {
          #info for the current country found...
          dfSel <- subset(res_countrySpecific_numYears, country==countrySel)
          dfCountry$surveyName <- factor(dfCountry$surveyName)
          surveys <- names(table(dfCountry$surveyName))
          #we will use it if the main data.frame contains info for only ONE survey
          #and the new number of years to discard corresponds to that survey...
          if ((length(surveys)==1)&(surveys[1] %in% dfSel$surveyName)) {
            dfSel <- subset(dfSel, surveyName==surveys[1])
            res_numYears <- dfSel$years
          }
        }
      }
    }
    if ( !is.null(vecCountry) & (!(countrySel %in% vecCountry)) ) next
    pprs <- ppr_doIt (dfCountry, varEnter=varEnter, varEvent=varEvent, varCens=varCens, varWeight=varWeight, country=countrySel,
                       duration=duration,
                      res_numYears=res_numYears,
                      res_finalYearsToDiscard=res_finalYearsToDiscard,
                      res_firstYearsToDiscard=res_firstYearsToDiscard,
                      useRelativeWeights=useRelativeWeights,
                      ageTruncate=ageTruncate,
                      # >>> Claude 2026-09-22
                      replicates=replicates, varStrata=varStrata, varCluster=varCluster,
                      confLevel=confLevel, seed=seed,
                      # <<< Claude 2026-09-22
                      mySpan=mySpan)
    pprsTot <- rbind(pprsTot, pprs$PPR)
  }
  
  return (pprsTot)
}

plot_ppr <- function(df_res=res_BirthBirth1, vecCountry=NULL, facet=TRUE, yLimit=NULL, Title=NULL, yTitle="PPR", xTitle=NULL,
                     yVar="quantum_1stkind_smoothed", yVar_min="ci95_lower_quantum_1stkind_smoothed", yVar_max="ci95_upper_quantum_1stkind_smoothed") {
  require (ggplot2)
  require (ggrepel)
  require (scales)
  
  if (!is.null(vecCountry)) {
    df_res <- subset(df_res, country %in% vecCountry)
  }
  
  if ((exists("DEBUG2")) && (isTRUE(DEBUG2))) browser()
  
  bySurvey <- ("surveyName" %in% colnames(df_res))
  
  if (facet) {
    # 1. Get the last points for labeling
    if (isTRUE(bySurvey)) {
      df_ends <- df_res %>% 
        group_by(country, surveyName) %>% 
        dplyr::filter(year == max(year))
    } else {
      df_ends <- df_res %>% 
        group_by(country) %>% 
        dplyr::filter(year == max(year))
    }
  } else {
    df_labels <- df_res %>%
      group_by(country) %>%
      slice(ceiling(n() / 2))
    }
  # 2. Get the max year to know where the "edge" is
  max_x <- max(df_res$year)
  max_y <- min(0.75,max(df_res[[yVar]]))
  
  # >>> Claude 2026-09-22
  # 'linetype' is now MAPPED, to the same variable as the colour, and every
  # level is set to "solid" straight afterwards. Nothing about the existing
  # figures changes, but the aesthetic exists, so a caller can restyle the
  # lines by adding one scale to the returned plot:
  #
  #   plot_sep1_45 + scale_linetype_manual(values = c(MEXICO = "solid", USA = "22"))
  #
  # Without the mapping a linetype scale has nothing to act on, which is why
  # adding one to the old version did nothing. Because all three aesthetics
  # carry the same variable and the same default legend title, they merge into
  # a single key rather than producing a second one.
  #
  # The facetted branch without surveys maps no grouping variable at all, so it
  # gets no linetype either.
  ltVar <- NULL
  if (facet) {
    if (bySurvey) {
      ltVar <- "surveyName"
      p <- ggplot(df_res, aes(x=year, y=.data[[yVar]], group=surveyName, color=surveyName,
                              linetype=surveyName))
      p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]], fill=surveyName),
                           alpha=0.1,
                           color = NA)
    } else {
      p <- ggplot(df_res, aes(x=year, y=.data[[yVar]]),
                  color = NA)
      p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]]),
                           alpha=0.1,
                           color = NA)
    }
  } else {
    ltVar <- "country"
    p <- ggplot(df_res, aes(x=year, y=.data[[yVar]], group=country, color=country,
                            linetype=country))
    p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]], fill=country), alpha=0.1, color = NA)
  }
  p <- p  + geom_line()
  if (!is.null(ltVar) && (ltVar %in% names(df_res))) {
    ltLev <- unique(as.character(df_res[[ltVar]]))
    ltLev <- ltLev[!is.na(ltLev)]
    if (length(ltLev) > 0) {
      p <- p + scale_linetype_manual(values = stats::setNames(rep("solid", length(ltLev)), ltLev))
    }
  }
  # <<< Claude 2026-09-22
  if(facet) {p <- p + facet_wrap (vars(country))}
  p <- p + scale_y_continuous(limit=c(0,NA),oob=squish)
  p <- p + theme_linedraw()
  if (!is.null(yLimit)) {p <- p + coord_cartesian(ylim=yLimit)}
  p <- p + theme(legend.position="none")
  if (facet) {
    if (bySurvey) {
      # p <- p + directlabels::geom_dl(aes(label = surveyName), method = "chull.grid")
      p <- p + geom_text_repel(
        data = df_ends,
        aes(label = surveyName), 
        size = 4,
        fontface = "bold",
        hjust = 0,
        direction = "y",           # Stack them vertically to avoid overlap
        nudge_x = -1,               # Force them 5 units to the right of the last point
        xlim = c(NA, max_x),   # Don't let labels go the right
        ylim = c(max_y, NA),   # Don't let labels cross over the curves
        segment.color ="grey50",
        segment.linetype ="dotted",
        segment.size = 0.5,        # Thickness of the line
        #segment.curvature = -0.1,  # Add a slight "wiggle" or curve to the line
        segment.ncp = 3,
        min.segment.length = 0     # Force the line to show even if the label is close
      )
    }
  } else {
    y_range <- diff(range(df_res$quantum_1stkind_smoothed))
    
    p <- p + geom_text_repel(
      data = df_labels,
      aes(label = country),
      size = 4,
      fontface = "bold",
      nudge_y = y_range * 0.05,    # 5% of the data range, adapts automatically
      min.segment.length = 0
    )
  }
  
  p + labs(title=Title, y=yTitle, x=xTitle) + theme_text()
}

plot_mean <- function(df_res=res_BirthBirth1, vecCountry=NULL, facet=TRUE, yLimit=NULL, Title=NULL, yTitle="mean duration", xTitle=NULL) {
   return (plot_ppr(df_res=df_res, vecCountry=vecCountry, facet=facet, yLimit=yLimit, Title=Title, yTitle=yTitle, xTitle=xTitle,
                    yVar="mac_1stkind_smoothed", yVar_min="ci95_lower_mac_1stkind_smoothed", yVar_max="ci95_upper_mac_1stkind_smoothed"))
}

plotBySurvey <- function (df_toPlot=NULL, varEnter=NULL, varEvent=NULL, varCens="cmc_survey", varCountry="country",
                          varWeight="weight",
                          vecCountry=NULL, duration=FALSE,
                          res_firstYearsToDiscard=15, res_finalYearsToDiscard=1, res_countrySpecific_numYears=NULL,
                          yLimit=c(0,1), mySpan=0.75,
                          useRelativeWeights=FALSE,ageTruncate=NULL,
                          # >>> Claude 2026-09-22
                          replicates=200L, varStrata=NULL, varCluster=NULL,
                          confLevel=0.95, seed=NULL,
                          # <<< Claude 2026-09-22
                          Title=NULL, yTitle="PPR", xTitle=NULL) {
  
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  surveys <- names(table(df_toPlot$surveyName))
  if (exists("res_all")) {rm(res_all)}
  for (indSurvey in (1:length(surveys))) {
    print (paste("Survey:", surveys[indSurvey]))
    df_toPlot_survey <- subset(df_toPlot, surveyName==surveys[indSurvey])
    res_survey <- calc_ppr(df=df_toPlot_survey, varEnter=varEnter, varEvent=varEvent, varCens=varCens, varCountry=varCountry,
                           vecCountry=vecCountry, varWeight=varWeight, duration=duration,
                           res_firstYearsToDiscard=res_firstYearsToDiscard, res_finalYearsToDiscard=res_finalYearsToDiscard,
                           res_countrySpecific_numYears=res_countrySpecific_numYears,
                           useRelativeWeights=useRelativeWeights,ageTruncate=ageTruncate,mySpan=mySpan,
                           # >>> Claude 2026-09-22
                           replicates=replicates, varStrata=varStrata, varCluster=varCluster,
                           confLevel=confLevel, seed=seed)
                           # <<< Claude 2026-09-22
    res_survey$surveyName=surveys[indSurvey]
    if (exists("res_all")) {
      res_all <- rbind(res_all, res_survey)
    } else {
      res_all <- res_survey
    }
  }
  plotIt <- plot_ppr(df_res=res_all, vecCountry=NULL, yLimit=yLimit, Title=Title, yTitle=yTitle, xTitle=xTitle)
  
  return (list(plot=plotIt,results=res_all))
}

#determine the maximum number of years for computing PPRs with a survey
pprNumYears <- function (df=NULL, type="fertility", varEvent=NULL, mute=FALSE) {
  #df is a fertility survey which should have a variable named "ageSurvey"
  #type is one of c("fertility", "separation1", "separation2")
  #if type is "fertility", the minimum age is 40 years
  #if type is "separation1", the minimum age is 40 years
  #if type is "separation2", the minimum age is 40 years
  #if 'mute' is FALSE, then the function prints the limit for the survey, if there is a variable named 'country'
  ageLimit <- list("fertility"=40, "separation1"=40, "separation2"=40)
  tab <- table(df$ageSurvey)
  n30_39 <- mean(tab[which(names(tab) %in% (30:39))])
  tab <- subset(tab, tab > n30_39 * 0.5)
  maxTab <- as.numeric(max(names(tab)))
  numYears <- maxTab - ageLimit[[type]]
  if (!is.null(varEvent)&("lastYear" %in% names(df))) {
    tabEvent <- table(df[, varEvent])
    maxTabEvent <- as.numeric(max(names(tabEvent)))
    lastYear <- min(df$lastYear)
    numYears <- numYears + maxTabEvent - lastYear - 1
  }
  if (numYears < 2) {numYears <- 3}
  if (!is.null(df$country)) {
    aCountry <- df$country[1]
  } else {
    aCountry <- "country"
  }
  if (!is.null(df$surveyName)) {
    name <- df$surveyName[1]
  } else {
    name <- "survey"
  }
  if (!mute) {
    print (paste(aCountry, ", survey:", name, ", last age is", maxTab, ", num years is", numYears))
  }
  
  return (data.frame(country=aCountry, surveyName=name, years=numYears))
}

buildSpecificYears <- function (dfsurveys=NULL, type=NULL, varEvent=NULL) {
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  res <- data.frame(country=NULL, surveyName=NULL, years=NULL)
  countries <- names (table (dfsurveys$country))
  for (indCountry in (1:length(countries))) {
    dfCountry <- subset (dfsurveys, country==countries[indCountry])
    dfCountry$surveyName <- factor(dfCountry$surveyName)
    surveys <- names (table(dfCountry$surveyName))
    for (indSurvey in (1:length(surveys))) {
      dfSurvey <- subset(dfCountry, surveyName==surveys[indSurvey])
      resSurvey <- pprNumYears(dfSurvey, type, varEvent=varEvent)
      res <- rbind(res, resSurvey)
    }
  }
  return (res)
}

agesByYear <- function(df=union1_sep1, varEvent="ySep1") {
  # compute the range of ages for the range of year of events
  if ("country" %in% names(df)) {
    df$country <- factor (df$country)
    cat <- table(df$country)
    countries <- names(cat)
  } else {
    countries <- "country"
  }
  agesByYear_tot <- data.frame()
  for (aCountry in countries) {
    dfC <- subset (df, country==aCountry)
    rangeYearEvents <- range(dfC[[varEvent]], na.rm=TRUE)
    rangeYears <- rangeYearEvents[1]:rangeYearEvents[2]
    agesByYear <- data.frame(country=rep(aCountry,length(rangeYears)),year=rangeYears, ageMin=NA, ageMax=NA)
    # create columns for each year in the range and put the age of individuals in each column
    cols <- as.character(rangeYears)
    dfC[, cols] <- NA
    dfC[, cols] <- sapply(rangeYears,
                         function(y) {
                           age <- y - dfC$yBirth
                           age <- ifelse(age < 0, NA, age)
                           age <- ifelse(age > dfC$ageSurvey, NA, age)
                           return (age)
                         })
    # loop
    # for (year in rangeYears) {
    #   agesByYear$ageMin[agesByYear$year == year] <- min(dfC[[as.character(year)]], na.rm=TRUE)
    #   agesByYear$ageMax[agesByYear$year == year] <- max(dfC[[as.character(year)]], na.rm=TRUE)
    # }
    
    # the vectorized version of the loop
    # 1. Calculate all mins and maxes at once (returns a named vector)
    all_mins <- sapply(dfC[cols], min, na.rm = TRUE)
    all_maxs <- sapply(dfC[cols], max, na.rm = TRUE)
    
    # 2. Map them into your summary table using the 'year' as an index
    agesByYear$ageMin <- all_mins[as.character(agesByYear$year)]
    agesByYear$ageMax <- all_maxs[as.character(agesByYear$year)]
    
    agesByYear_tot <- rbind(agesByYear_tot, agesByYear)
  }
  
  return (agesByYear_tot)
}
