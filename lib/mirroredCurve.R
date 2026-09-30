# >>> Claude 2026-09-25
# lib/mirroredCurve.R  (was lib/KaplanMeierSurvfit.R)
#
# THE NAME. This figure has long been called a "mirrored Kaplan-Meier curve"
# (Billari 2001). The name no longer fits what is computed. The two halves are
# scaled by Aalen-Johansen cumulative incidences of the FIRST event, and only
# the time from the first to the second event is an ordinary Kaplan-Meier
# curve. The functions are therefore renamed:
#
#   mirroredCurve()           was KaplanMeierSurvfit()
#   mirroredCurveBootstrap()  was KaplanMeierBootstrap()
#   mirroredCurvePlot()       new: the figure, a wrapper of KaplanMeierPlot()
#
# The old names still work (aliases at the end of this file), and so does
# KaplanMeierPlot(..., varEvent2 = ...), so no existing script breaks. The
# older estimator that scales the halves by RAW observed shares is still
# available as KaplanMeierPlot(..., varEvent2 = ..., estimator = "classic");
# it is biased whenever women are censored before either event and is kept
# only to reproduce old figures. docs/05_methods.md explains the construction.
# <<< Claude 2026-09-25

# >>> Claude 2026-09-20
# Mirrored event-order curve estimated with survival:: alone.
#
# This replaces archive/KaplanMeierMstate.R (formerly lib/) as the production implementation of the
# mirrored figure. It keeps that prototype's structure and its two good ideas
# (Aalen-Johansen branch probabilities, and transforming the interval bounds
# rather than rebuilding them from a standard error), and drops mstate.
#
# WHY NO mstate. The prototype already had two probability engines and used the
# survival one whenever the weights were not constant, which is every real call
# in this project. The mstate path also needed a counting-process layout with an
# epsilon nudge on zero-length intervals, and coxph + msfit + probtrans to reach
# a quantity that survfit() computes in one call from a factor status. Removing
# it deletes that machinery, one dependency, and the epsilon.
#
# WHAT THE CURVE IS. Two probabilities and two conditional survival curves:
#
#   right half, at gap t:  P(event first by tau) * P(no event2 within t | event first)
#   left half,  at gap -t: 1 - P(event2 first by tau) * P(no event within t | event2 first)
#
# The two probabilities are the Aalen-Johansen cumulative incidences of the two
# causes of the FIRST event, evaluated at the horizon tau. They replace the raw
# observed shares used by KaplanMeier() in KaplanMeierLib.R, which are biased
# downward whenever anyone is censored before either event.
#
# TAU IS PART OF THE ESTIMATE. A cumulative incidence is always read at a finite
# time. The default is the largest observed first-event time, and the value used
# is returned in attr(result, "horizon"). Put it in the figure caption.
#
# THE GAP AT TIME 0 has two components, both returned: propNeither (never
# observed to experience either event) and, when ties = "simultaneous",
# propSimultaneous (both events in the same month). Neither is drawn, so the
# visible gap is their sum. attr(result, "gapAtZero") reports it.
#
# ANTICIPATORY SELECTION. As in KaplanMeierLib.R, neither half estimates
# anything backwards in time. Which half a woman belongs to is decided by which
# event came first, which is information about her future as of time 0. See Hoem
# and Kreyenfeld (2006), Demographic Research 15:17.
#
# INFERENCE. The analytic intervals returned here are CONDITIONAL on the two
# Aalen-Johansen probabilities and ignore strata and clusters. For an interval
# that is defensible in a paper, use mirroredCurveBootstrap(), which resamples the
# whole construction, PSUs within strata when you supply them.

library2 <- if (exists("library2")) library2 else function (pkg) library(pkg, character.only = TRUE)


# ==== 1. Internal helpers ====

.kmsEmpty <- function () {
  data.frame(time=numeric(0), event=numeric(0), eventRaw=numeric(0), number=numeric(0),
             numberRaw=numeric(0), surv=numeric(0), rate=numeric(0), survFunction=numeric(0),
             variance=numeric(0), stdErr=numeric(0), confIntMax=numeric(0), confIntMin=numeric(0),
             branch=character(0), stringsAsFactors=FALSE)
}

# Conditional clock-reset Kaplan-Meier for one half, from survfit().
.kmsBranch <- function (gap, status, weight, branch, truncate, confLevel, confType) {
  if (length(gap) == 0) return (.kmsEmpty())

  fitData <- data.frame(gap = as.numeric(gap),
                        status = as.integer(status),
                        wt = as.numeric(weight))
  fit <- survival::survfit(survival::Surv(gap, status) ~ 1, data = fitData, weights = wt,
                           robust = TRUE, conf.int = confLevel, conf.type = confType)
  sf <- summary(fit, times = fit$time, extend = TRUE)
  if (length(sf$time) == 0) return (.kmsEmpty())

  # Unweighted and weighted counts per gap, for truncation and for diagnostics.
  agg <- stats::aggregate(
    data.frame(event = weight * status, eventRaw = status,
               number = weight, numberRaw = rep(1, length(gap))),
    by = list(time = as.numeric(gap)), FUN = sum)

  res <- data.frame(time         = sf$time,
                    surv         = sf$n.risk,
                    rate         = ifelse(sf$n.risk > 0, sf$n.event / sf$n.risk, 0),
                    survFunction = sf$surv,
                    stdErr       = sf$std.err,
                    variance     = sf$std.err^2,
                    confIntMax   = sf$upper,
                    confIntMin   = sf$lower)
  res <- merge(res, agg, by = "time", all.x = TRUE)
  for (cc in c("event", "eventRaw", "number", "numberRaw")) res[[cc]][is.na(res[[cc]])] <- 0
  res <- res[order(res$time), ]

  # log and log-log return NA where the curve touches 1 or 0.
  res$confIntMax <- pmin(1, ifelse(is.na(res$confIntMax), 1, res$confIntMax))
  res$confIntMin <- pmax(0, ifelse(is.na(res$confIntMin), 0, res$confIntMin))

  # Explicit origin, so the conditional curve starts at 1.
  if (min(res$time) > 0) {
    origin <- res[1, ]
    origin$time <- 0
    origin[, c("event", "eventRaw", "number", "numberRaw", "rate", "variance", "stdErr")] <- 0
    origin$surv <- sum(weight)
    origin$survFunction <- 1
    origin$confIntMax <- 1
    origin$confIntMin <- 1
    res <- rbind(origin, res)
  }

  # Truncate on the UNWEIGHTED count, while still ordered by increasing gap.
  if (!is.null(truncate)) {
    res <- res[rev(cumsum(rev(res$eventRaw))) >= truncate, ]
  }
  if (nrow(res) == 0) return (.kmsEmpty())

  if (branch == "before") {
    res$time <- -res$time
    res <- res[order(res$time), ]
  }
  res$branch <- branch
  res <- res[, names(.kmsEmpty())]
  rownames(res) <- NULL
  return (res)
}

# Weighted Aalen-Johansen cumulative incidence of each cause of the FIRST event.
.kmsFirstProbs <- function (firstTime, firstStatus, weight, horizon, causes) {
  lv <- c("censor", causes)
  fitData <- data.frame(time = as.numeric(firstTime),
                        st   = factor(lv[firstStatus + 1L], levels = lv),
                        wt   = as.numeric(weight))
  fit <- suppressWarnings(
    survival::survfit(survival::Surv(time, st) ~ 1, data = fitData, weights = wt))
  ps <- suppressWarnings(summary(fit, times = horizon, extend = TRUE)$pstate)
  ps <- as.numeric(ps[1, ])
  names(ps) <- fit$states
  pick <- function (nm) if (nm %in% names(ps)) ps[[nm]] else 0
  out <- list(eventFirst   = pick("event_first"),
              event2First  = pick("event2_first"),
              simultaneous = pick("simultaneous"),
              neither      = pick("(s0)"))
  return (out)
}

# Shared preparation: cleaning, the order of the two events, and the two gaps.
.kmsPrepare <- function (df_KM, varEnter, varEvent, varCens, varEvent2, varWeight,
                         ties, naWeight) {
  d <- data.frame(enter  = as.numeric(df_KM[[varEnter]]),
                  event  = as.numeric(df_KM[[varEvent]]),
                  event2 = as.numeric(df_KM[[varEvent2]]),
                  cens   = as.numeric(df_KM[[varCens]]),
                  weight = if (is.null(varWeight)) 1 else as.numeric(df_KM[[varWeight]]))

  # Missing weights, with the same three policies as KaplanMeier().
  if (!is.null(varWeight)) {
    nNAw <- sum(is.na(d$weight))
    if (nNAw > 0) {
      if (naWeight == "error") {
        stop(sprintf("mirroredCurve: %d case(s) have a missing weight in '%s'", nNAw, varWeight))
      } else if (naWeight == "drop") {
        message(sprintf("mirroredCurve: dropped %d case(s) with a missing weight in '%s'", nNAw, varWeight))
        d <- d[!is.na(d$weight), ]
      } else {
        message(sprintf(paste0("mirroredCurve: %d case(s) have a missing weight in '%s' and were ",
                               "set to 1; naWeight = \"drop\" or \"error\" may be what you want"), nNAw, varWeight))
        d$weight[is.na(d$weight)] <- 1
      }
    }
  }

  ok <- is.finite(d$enter) & is.finite(d$cens) & (d$cens > d$enter) &
    is.finite(d$weight) & (d$weight > 0)
  d <- d[ok, ]
  if (nrow(d) == 0) return (NULL)

  ok <- (is.na(d$event)  | ((d$event  >= d$enter) & (d$event  < d$cens))) &
        (is.na(d$event2) | ((d$event2 >= d$enter) & (d$event2 < d$cens)))
  d <- d[ok, ]
  if (nrow(d) == 0) return (NULL)

  isE  <- !is.na(d$event)  & (is.na(d$event2) | (d$event  < d$event2))
  isE2 <- !is.na(d$event2) & (is.na(d$event)  | (d$event2 < d$event))
  isTie <- !is.na(d$event) & !is.na(d$event2) & (d$event == d$event2)

  # Simultaneous events. "simultaneous" keeps them as a third outcome of the
  # first event, which is the honest reading with monthly CMC dates. "event" and
  # "event2" fold them into one of the two halves; "event" reproduces the
  # convention used by KaplanMeier() in KaplanMeierLib.R.
  if (ties == "event") {
    isE <- isE | isTie
    isTie <- rep(FALSE, nrow(d))
  } else if (ties == "event2") {
    isE2 <- isE2 | isTie
    isTie <- rep(FALSE, nrow(d))
  }

  firstStatus <- ifelse(isE, 1L, ifelse(isE2, 2L, ifelse(isTie, 3L, 0L)))
  firstDate   <- ifelse(firstStatus == 1L, d$event,
                 ifelse(firstStatus == 2L, d$event2,
                 ifelse(firstStatus == 3L, d$event, d$cens)))
  firstTime <- firstDate - d$enter
  # survfit() will not accept a transition at time 0. A tiny positive value
  # keeps such a case in its correct state without moving it to another month.
  firstTime <- pmax(firstTime, 1e-6)

  # Second event, measured from the first one.
  secondDate   <- d$cens
  secondStatus <- integer(nrow(d))
  seenE2 <- (firstStatus == 1L) & !is.na(d$event2) & (d$event2 >= d$event) & (d$event2 < d$cens)
  secondDate[seenE2]   <- d$event2[seenE2]
  secondStatus[seenE2] <- 1L
  seenE  <- (firstStatus == 2L) & !is.na(d$event) & (d$event >= d$event2) & (d$event < d$cens)
  secondDate[seenE]   <- d$event[seenE]
  secondStatus[seenE] <- 1L

  gap <- secondDate - firstDate

  return (list(d = d, firstTime = firstTime, firstStatus = firstStatus,
               gap = gap, secondStatus = secondStatus))
}


# ==== 2. The estimator ====

mirroredCurve <- function (df_KM=NULL, varEnter=NULL, varEvent=NULL, varCens=NULL,
                                varWeight=NULL, varEvent2=NULL, truncate=10,
                                horizon=NULL, confLevel=0.95, confType="log-log",
                                ties=c("simultaneous", "event", "event2"),
                                naWeight="one",
                                # >>> Claude 2026-09-25
                                conditional=FALSE
                                # <<< Claude 2026-09-25
                                ) {
  #==> df_KM, varEnter, varEvent, varCens, varWeight: as in KaplanMeier()
  #==> varEvent2: the second event. Required here; without it use KaplanMeier().
  #==> truncate: hide a half's tail once fewer than 'truncate' UNWEIGHTED events remain
  #==> horizon: the time tau at which the two branch probabilities are read,
  #    measured from varEnter in the same units as the dates. NULL uses the
  #    largest observed first-event time. The value used is returned as an
  #    attribute and belongs in the figure caption.
  #==> confType: "log-log" (default), "log" or "plain", passed to survfit
  #==> ties: how to treat varEvent2 == varEvent. See .kmsPrepare().
  # >>> Claude 2026-09-25
  #==> conditional: TRUE divides the branch probabilities (and the same-month
  #    share) by 1 - P(neither by horizon), so that the figure describes the
  #    women with at least one event by the horizon, as in Billari (2001).
  #    With observed counts this is exact only when every woman has reached
  #    the horizon; the Aalen-Johansen ratio stays valid when some have not.
  # <<< Claude 2026-09-25
  #<== a table with the same columns as KaplanMeier(), so KaplanMeierPlot() and
  #    anything else downstream needs no change.

  ties <- match.arg(ties)
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("mirroredCurve requires the 'survival' package (it ships with R)")
  }
  if (is.null(df_KM)) stop("df_KM cannot be NULL")
  if (is.null(varEnter) || is.null(varEvent) || is.null(varCens) || is.null(varEvent2)) {
    stop("varEnter, varEvent, varEvent2 and varCens are all required")
  }
  need <- c(varEnter, varEvent, varEvent2, varCens, varWeight)
  miss <- setdiff(need, names(df_KM))
  if (length(miss) > 0) stop("missing column(s): ", paste(miss, collapse = ", "))

  prep <- .kmsPrepare(df_KM, varEnter, varEvent, varCens, varEvent2, varWeight, ties, naWeight)
  if (is.null(prep)) return (.kmsEmpty())

  causes <- if (any(prep$firstStatus == 3L)) c("event_first", "event2_first", "simultaneous")
            else c("event_first", "event2_first")
  if (is.null(horizon)) horizon <- max(prep$firstTime)

  p <- .kmsFirstProbs(prep$firstTime, prep$firstStatus, prep$d$weight, horizon, causes)
  # >>> Claude 2026-09-25
  propNeitherAll <- p$neither
  if (isTRUE(conditional)) {
    withEvent <- 1 - p$neither
    if (!is.finite(withEvent) || (withEvent <= 0)) return (.kmsEmpty())
    p$eventFirst   <- p$eventFirst / withEvent
    p$event2First  <- p$event2First / withEvent
    p$simultaneous <- p$simultaneous / withEvent
    p$neither      <- 0
  }
  # <<< Claude 2026-09-25

  iE  <- which(prep$firstStatus == 1L)
  iE2 <- which(prep$firstStatus == 2L)

  after <- .kmsBranch(prep$gap[iE], prep$secondStatus[iE], prep$d$weight[iE],
                      "after", truncate, confLevel, confType)
  before <- .kmsBranch(prep$gap[iE2], prep$secondStatus[iE2], prep$d$weight[iE2],
                       "before", truncate, confLevel, confType)

  # Scale each half by its branch probability. The bounds are transformed
  # directly rather than rebuilt from stdErr, so the log-log shape survives.
  # Both transforms are monotone in S for a fixed p, so the bounds only swap
  # on the left half.
  if (nrow(after) > 0) {
    after$survFunction <- p$eventFirst * after$survFunction
    after$variance     <- p$eventFirst^2 * after$variance
    after$stdErr       <- sqrt(after$variance)
    after$confIntMax   <- p$eventFirst * after$confIntMax
    after$confIntMin   <- p$eventFirst * after$confIntMin
  }
  if (nrow(before) > 0) {
    loCond <- before$confIntMin
    hiCond <- before$confIntMax
    before$survFunction <- 1 - p$event2First * before$survFunction
    before$variance     <- p$event2First^2 * before$variance
    before$stdErr       <- sqrt(before$variance)
    before$confIntMin   <- 1 - p$event2First * hiCond
    before$confIntMax   <- 1 - p$event2First * loCond
  }

  res <- rbind(before, after)
  res$confIntMax <- pmin(1, res$confIntMax)
  res$confIntMin <- pmax(0, res$confIntMin)
  rownames(res) <- NULL

  attr(res, "propEventBeforeEvent2") <- p$eventFirst
  attr(res, "propEvent2BeforeEvent") <- p$event2First
  attr(res, "propSimultaneous")      <- p$simultaneous
  attr(res, "propNeither")           <- p$neither
  attr(res, "gapAtZero")             <- p$neither + p$simultaneous
  attr(res, "horizon")               <- horizon
  attr(res, "ties")                  <- ties
  # >>> Claude 2026-09-25
  attr(res, "conditional")           <- isTRUE(conditional)
  attr(res, "propNeitherAll")        <- propNeitherAll
  # <<< Claude 2026-09-25
  attr(res, "ciMethod") <- paste0(
    "conditional ", confType, " intervals from survfit(), scaled by the ",
    "Aalen-Johansen branch probabilities. Uncertainty in those probabilities, ",
    "and the sampling design, are NOT included. Use mirroredCurveBootstrap().")
  return (res)
}


# ==== 3. Bootstrap inference for the complete curve ====

mirroredCurveBootstrap <- function (df_KM=NULL, varEnter=NULL, varEvent=NULL, varCens=NULL,
                                  varWeight=NULL, varEvent2=NULL, truncate=10,
                                  horizon=NULL, confLevel=0.95,
                                  ties=c("simultaneous", "event", "event2"),
                                  naWeight="one",
                                  replicates=500, varStrata=NULL, varCluster=NULL,
                                  seed=NULL, progress=TRUE,
                                  # >>> Claude 2026-09-25
                                  conditional=FALSE
                                  # <<< Claude 2026-09-25
                                  ) {
  #==> replicates: number of bootstrap replicates. 500 is a reasonable floor for
  #    a percentile interval; 1000 if the figure goes in a paper.
  #==> varStrata / varCluster: names of the design columns. When varCluster is
  #    given, whole PSUs are resampled with replacement, within strata when
  #    varStrata is also given. This is what makes the interval design based.
  #    With neither, rows are resampled, which handles the weights and the
  #    branch probabilities but not the clustering.
  #<== the point estimate, with confIntMin / confIntMax replaced by percentile
  #    bounds over the replicates, and stdErr by the bootstrap standard
  #    deviation. attr(.., "bootReplicates") reports how many replicates
  #    actually produced a curve.

  ties <- match.arg(ties)
  if (!is.null(seed)) set.seed(seed)

  point <- mirroredCurve(df_KM, varEnter, varEvent, varCens, varWeight, varEvent2,
                              truncate = truncate, horizon = horizon,
                              confLevel = confLevel, ties = ties, naWeight = naWeight,
                              conditional = conditional)   # Claude 2026-09-25
  if (nrow(point) == 0) return (point)
  if (is.null(horizon)) horizon <- attr(point, "horizon")   # hold tau fixed across replicates

  # Resampling unit
  if (!is.null(varCluster)) {
    clusterId <- as.character(df_KM[[varCluster]])
    stratumId <- if (is.null(varStrata)) rep("all", nrow(df_KM)) else as.character(df_KM[[varStrata]])
    key <- paste(stratumId, clusterId, sep = "\r")
    byStratum <- split(unique(key), sub("\r.*$", "", unique(key)))
    rowsOfKey <- split(seq_len(nrow(df_KM)), key)
  }

  # Evaluate a replicate's step function on the point estimate's own grid,
  # separately per half, right-continuous.
  evalOn <- function (rep, grid, br) {
    r <- rep[rep$branch == br, ]
    g <- grid[grid$branch == br, "time"]
    if (length(g) == 0) return (numeric(0))
    if (nrow(r) == 0) return (rep(NA_real_, length(g)))
    stats::approx(x = r$time, y = r$survFunction, xout = g,
                  method = "constant", f = 0, rule = 2)$y
  }

  gridBefore <- sum(point$branch == "before")
  gridAfter  <- sum(point$branch == "after")
  boot <- matrix(NA_real_, nrow = replicates, ncol = nrow(point))
  nOK <- 0L

  for (b in seq_len(replicates)) {
    if (!is.null(varCluster)) {
      picked <- unlist(lapply(byStratum, function (ks) sample(ks, length(ks), replace = TRUE)),
                       use.names = FALSE)
      idx <- unlist(rowsOfKey[picked], use.names = FALSE)
    } else {
      idx <- sample.int(nrow(df_KM), nrow(df_KM), replace = TRUE)
    }
    rep_b <- try(suppressMessages(suppressWarnings(
      mirroredCurve(df_KM[idx, , drop = FALSE], varEnter, varEvent, varCens,
                         varWeight, varEvent2, truncate = NULL, horizon = horizon,
                         confLevel = confLevel, ties = ties, naWeight = naWeight,
                         conditional = conditional))),   # Claude 2026-09-25
      silent = TRUE)
    if (inherits(rep_b, "try-error") || nrow(rep_b) == 0) next
    vB <- if (gridBefore > 0) evalOn(rep_b, point, "before") else numeric(0)
    vA <- if (gridAfter  > 0) evalOn(rep_b, point, "after")  else numeric(0)
    boot[b, ] <- c(vB, vA)
    nOK <- nOK + 1L
    if (isTRUE(progress) && (b %% 100 == 0)) cat("  bootstrap", b, "/", replicates, "\n")
  }

  a <- (1 - confLevel) / 2
  lo <- apply(boot, 2, stats::quantile, probs = a,     na.rm = TRUE)
  hi <- apply(boot, 2, stats::quantile, probs = 1 - a, na.rm = TRUE)
  sd <- apply(boot, 2, stats::sd, na.rm = TRUE)

  out <- point
  out$confIntMin <- pmax(0, as.numeric(lo))
  out$confIntMax <- pmin(1, as.numeric(hi))
  out$stdErr     <- as.numeric(sd)
  out$variance   <- out$stdErr^2

  attr(out, "bootReplicates") <- nOK
  attr(out, "bootRequested")  <- replicates
  attr(out, "bootUnit") <- if (is.null(varCluster)) "individual rows" else
    paste0("clusters in '", varCluster, "'",
           if (is.null(varStrata)) "" else paste0(" within strata '", varStrata, "'"))
  attr(out, "ciMethod") <- paste0(
    "percentile bootstrap of the complete estimator over ", nOK, " replicates, ",
    "resampling ", attr(out, "bootUnit"), ". Includes uncertainty in the ",
    "Aalen-Johansen branch probabilities and in both conditional curves.")
  return (out)
}
# <<< Claude 2026-09-20


# >>> Claude 2026-09-25
# ==== 4. The figure, and the old names ====

mirroredCurvePlot <- function (df = NULL, varEnter = NULL, varEvent = NULL, varEvent2 = NULL,
                               varCens = NULL, varWeight = NULL, ...) {
  #==> as KaplanMeierPlot(); varEvent2 is required. The right half follows the
  #    women whose 'varEvent' came first and shows the time until 'varEvent2';
  #    the left half follows those whose 'varEvent2' came first, drawn on a
  #    reversed axis. Any other KaplanMeierPlot() argument (cohortsList,
  #    horizon, ties, bootstrap, ...) is passed through.
  if (is.null(varEvent2)) stop("mirroredCurvePlot: varEvent2 is required")
  KaplanMeierPlot(df = df, varEnter = varEnter, varEvent = varEvent, varCens = varCens,
                  varWeight = varWeight, varEvent2 = varEvent2, estimator = "survfit", ...)
}

# old names, kept so existing scripts run unchanged
KaplanMeierSurvfit   <- mirroredCurve
KaplanMeierBootstrap <- mirroredCurveBootstrap
# <<< Claude 2026-09-25
