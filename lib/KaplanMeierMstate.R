# Mirrored Kaplan-Meier estimator using a multistate model
#
# The signed-gap curve combines:
#   1. Aalen-Johansen probabilities for which event occurs first.
#   2. Clock-reset Kaplan-Meier curves for the time from the first event to
#      the second event.
#
# The multistate model preserves the order of the two events:
#
#   neither -> event first  -> both, event first
#           -> event2 first -> both, event2 first
#           -> simultaneous
#
# `mstate` estimates the state probabilities. `survival::survfit()` estimates
# the two clock-reset conditional survival curves because they are ordinary
# two-state processes after entry into the intermediate state.


# ==== Internal helpers ====

.kmMstateEmpty <- function () {
  data.frame(
    time         = numeric(0),
    event        = numeric(0),
    eventRaw     = numeric(0),
    number       = numeric(0),
    numberRaw    = numeric(0),
    surv         = numeric(0),
    rate         = numeric(0),
    survFunction = numeric(0),
    variance     = numeric(0),
    stdErr       = numeric(0),
    confIntMax   = numeric(0),
    confIntMin   = numeric(0),
    branch       = character(0),
    stringsAsFactors = FALSE
  )
}


.kmMstateBranch <- function (gap, status, weight, branch, truncate,
                             confLevel) {
  if (length(gap) == 0) return(.kmMstateEmpty())

  fitData <- data.frame(
    gap    = as.numeric(gap),
    status = as.integer(status),
    weight = as.numeric(weight)
  )

  fit <- survival::survfit(
    survival::Surv(gap, status) ~ 1,
    data      = fitData,
    weights   = weight,
    conf.int  = confLevel,
    conf.type = "log-log"
  )
  fitSummary <- summary(fit, censored = TRUE)

  if (length(fitSummary$time) == 0) return(.kmMstateEmpty())

  rawAtTime <- stats::aggregate(
    cbind(eventRaw = status, numberRaw = rep(1, length(status))),
    by = list(time = gap),
    FUN = sum
  )

  weightedAtTime <- stats::aggregate(
    cbind(
      event  = weight * status,
      number = weight
    ),
    by = list(time = gap),
    FUN = sum
  )

  result <- data.frame(
    time         = fitSummary$time,
    surv         = fitSummary$n.risk,
    rate         = ifelse(
      fitSummary$n.risk > 0,
      fitSummary$n.event / fitSummary$n.risk,
      0
    ),
    survFunction = fitSummary$surv,
    variance     = fitSummary$std.err^2,
    stdErr       = fitSummary$std.err,
    confIntMax   = fitSummary$upper,
    confIntMin   = fitSummary$lower
  )
  result <- merge(result, weightedAtTime, by = "time", all.x = TRUE)
  result <- merge(result, rawAtTime, by = "time", all.x = TRUE)
  result <- result[order(result$time), ]

  result$event[is.na(result$event)]       <- 0
  result$eventRaw[is.na(result$eventRaw)] <- 0
  result$number[is.na(result$number)]     <- 0
  result$numberRaw[is.na(result$numberRaw)] <- 0

  if (min(result$time) > 0) {
    origin <- data.frame(
      time         = 0,
      surv         = sum(weight),
      rate         = 0,
      survFunction = 1,
      variance     = 0,
      stdErr       = 0,
      confIntMax   = 1,
      confIntMin   = 1,
      event        = 0,
      number       = 0,
      eventRaw     = 0,
      numberRaw    = 0
    )
    result <- rbind(origin, result)
  }

  if (!is.null(truncate)) {
    keep <- rev(cumsum(rev(result$eventRaw))) >= truncate
    result <- result[keep, ]
  }
  if (nrow(result) == 0) return(.kmMstateEmpty())

  if (branch == "before") {
    result$time <- -result$time
    result <- result[order(result$time), ]
  }

  result$branch <- branch
  result <- result[, names(.kmMstateEmpty())]
  rownames(result) <- NULL
  attr(result, "survfit") <- fit
  result
}


.kmMstateLong <- function (firstTime, firstStatus, secondTime,
                           secondStatus, weight, hasTies) {
  n <- length(firstTime)
  # Counting-process intervals must have positive length. A small fraction of
  # a CMC month represents transitions observed at entry without moving them
  # to a substantively different month.
  epsilon <- 1e-4
  firstStop <- pmax(firstTime, epsilon)
  secondStop <- pmax(secondTime, firstStop + epsilon)

  if (hasTies) {
    transitionMatrix <- mstate::transMat(
      x = list(c(2, 3, 6), c(4), c(5), c(), c(), c()),
      names = c(
        "neither",
        "event_first",
        "event2_first",
        "both_event_first",
        "both_event2_first",
        "simultaneous"
      )
    )
    initialTransitions <- 1:3
    secondEventTrans   <- 4L
    secondEvent2Trans  <- 5L
  } else {
    transitionMatrix <- mstate::transMat(
      x = list(c(2, 3), c(4), c(5), c(), c()),
      names = c(
        "neither",
        "event_first",
        "event2_first",
        "both_event_first",
        "both_event2_first"
      )
    )
    initialTransitions <- 1:2
    secondEventTrans   <- 3L
    secondEvent2Trans  <- 4L
  }

  longParts <- vector("list", length(initialTransitions) + 2L)

  for (j in seq_along(initialTransitions)) {
    transition <- initialTransitions[j]
    longParts[[j]] <- data.frame(
      id      = seq_len(n),
      from    = 1L,
      to      = if (hasTies) c(2L, 3L, 6L)[j] else c(2L, 3L)[j],
      trans   = transition,
      Tstart  = 0,
      Tstop   = firstStop,
      time    = firstStop,
      status  = as.integer(firstStatus == j),
      weight  = weight
    )
  }

  eventFirst <- which(firstStatus == 1L)
  longParts[[length(initialTransitions) + 1L]] <- data.frame(
    id      = eventFirst,
    from    = 2L,
    to      = 4L,
    trans   = secondEventTrans,
    Tstart  = firstStop[eventFirst],
    Tstop   = secondStop[eventFirst],
    time    = secondStop[eventFirst] - firstStop[eventFirst],
    status  = secondStatus[eventFirst],
    weight  = weight[eventFirst]
  )

  event2First <- which(firstStatus == 2L)
  longParts[[length(initialTransitions) + 2L]] <- data.frame(
    id      = event2First,
    from    = 3L,
    to      = 5L,
    trans   = secondEvent2Trans,
    Tstart  = firstStop[event2First],
    Tstop   = secondStop[event2First],
    time    = secondStop[event2First] - firstStop[event2First],
    status  = secondStatus[event2First],
    weight  = weight[event2First]
  )

  longData <- do.call(rbind, longParts)
  longData <- longData[is.finite(longData$Tstop) &
                         (longData$Tstop >= longData$Tstart), ]
  rownames(longData) <- NULL
  class(longData) <- c("msdata", "data.frame")
  attr(longData, "trans") <- transitionMatrix

  list(data = longData, transitions = transitionMatrix)
}


.kmFirstProbabilities <- function (firstTime, firstStatus, weight,
                                   horizon, hasTies) {
  statusLevels <- c("censor", "event_first", "event2_first")
  if (hasTies) statusLevels <- c(statusLevels, "simultaneous")

  statusLabel <- statusLevels[firstStatus + 1L]
  fitData <- data.frame(
    time   = firstTime,
    status = factor(statusLabel, levels = statusLevels),
    weight = weight
  )
  fit <- suppressWarnings(
    survival::survfit(
      survival::Surv(time, status) ~ 1,
      data = fitData,
      weights = weight
    )
  )

  stateSummary <- suppressWarnings(
    summary(fit, times = horizon, extend = TRUE)$pstate
  )
  stateSummary <- as.numeric(stateSummary[1, ])
  names(stateSummary) <- fit$states

  value <- function (name) {
    if (name %in% names(stateSummary)) stateSummary[[name]] else 0
  }

  list(
    eventFirst  = value("event_first"),
    event2First = value("event2_first"),
    simultaneous = value("simultaneous"),
    neither = value("(s0)"),
    fit = fit
  )
}


# ==== Mirrored multistate estimator ====

#' Estimate a mirrored event-order curve with a multistate model.
#'
#' `varEvent` is plotted on the left when it occurs after `varEvent2`.
#' `varEvent2` is plotted on the right when it occurs after `varEvent`, matching
#' the orientation of `KaplanMeier()` in KaplanMeierLib.R.
#'
#' The returned confidence intervals are conditional on the Aalen-Johansen
#' branch probabilities. Bootstrap the complete estimator when uncertainty in
#' those probabilities, survey strata, or clusters must be included.
KaplanMeierMstate <- function (df_KM=NULL, varEnter=NULL, varEvent=NULL,
                               varCens=NULL, varWeight=NULL, varEvent2=NULL,
                               truncate=10, horizon=NULL, confLevel=0.95,
                               ties=c("simultaneous", "event", "event2"),
                               returnModels=FALSE) {
  ties <- match.arg(ties)

  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("KaplanMeierMstate requires the survival package")
  }
  if (!requireNamespace("mstate", quietly = TRUE)) {
    stop("KaplanMeierMstate requires the mstate package")
  }
  if (is.null(df_KM)) stop("df_KM cannot be NULL")
  if (is.null(varEnter) || is.null(varEvent) || is.null(varCens) ||
      is.null(varEvent2)) {
    stop("varEnter, varEvent, varEvent2, and varCens are required")
  }

  required <- c(varEnter, varEvent, varEvent2, varCens)
  if (!is.null(varWeight)) required <- c(required, varWeight)
  missingColumns <- setdiff(required, names(df_KM))
  if (length(missingColumns) > 0) {
    stop("Missing column(s): ", paste(missingColumns, collapse = ", "))
  }

  data <- data.frame(
    enter  = as.numeric(df_KM[[varEnter]]),
    event  = as.numeric(df_KM[[varEvent]]),
    event2 = as.numeric(df_KM[[varEvent2]]),
    cens   = as.numeric(df_KM[[varCens]]),
    weight = if (is.null(varWeight)) 1 else as.numeric(df_KM[[varWeight]])
  )

  valid <- is.finite(data$enter) & is.finite(data$cens) &
    (data$cens > data$enter) & is.finite(data$weight) & (data$weight > 0)
  data <- data[valid, ]
  if (nrow(data) == 0) return(.kmMstateEmpty())

  validEvent <- is.na(data$event) |
    ((data$event >= data$enter) & (data$event <= data$cens))
  validEvent2 <- is.na(data$event2) |
    ((data$event2 >= data$enter) & (data$event2 <= data$cens))
  data <- data[validEvent & validEvent2, ]
  if (nrow(data) == 0) return(.kmMstateEmpty())

  eventFirst <- !is.na(data$event) &
    (is.na(data$event2) | (data$event < data$event2))
  event2First <- !is.na(data$event2) &
    (is.na(data$event) | (data$event2 < data$event))
  simultaneous <- !is.na(data$event) & !is.na(data$event2) &
    (data$event == data$event2)

  if (ties == "event") {
    eventFirst  <- eventFirst | simultaneous
    simultaneous <- rep(FALSE, nrow(data))
  } else if (ties == "event2") {
    event2First <- event2First | simultaneous
    simultaneous <- rep(FALSE, nrow(data))
  }

  firstStatus <- ifelse(
    eventFirst, 1L,
    ifelse(event2First, 2L, ifelse(simultaneous, 3L, 0L))
  )
  firstDate <- ifelse(
    firstStatus == 1L,
    data$event,
    ifelse(firstStatus == 2L, data$event2,
           ifelse(firstStatus == 3L, data$event, data$cens))
  )
  firstTime <- firstDate - data$enter

  secondDate <- data$cens
  secondStatus <- integer(nrow(data))

  observedAfterEvent <- (firstStatus == 1L) & !is.na(data$event2) &
    (data$event2 >= data$event) & (data$event2 <= data$cens)
  secondDate[observedAfterEvent] <- data$event2[observedAfterEvent]
  secondStatus[observedAfterEvent] <- 1L

  observedAfterEvent2 <- (firstStatus == 2L) & !is.na(data$event) &
    (data$event >= data$event2) & (data$event <= data$cens)
  secondDate[observedAfterEvent2] <- data$event[observedAfterEvent2]
  secondStatus[observedAfterEvent2] <- 1L

  secondTime <- secondDate - data$enter
  hasTies <- any(firstStatus == 3L)
  ms <- .kmMstateLong(
    firstTime    = firstTime,
    firstStatus  = firstStatus,
    secondTime   = secondTime,
    secondStatus = secondStatus,
    weight       = data$weight,
    hasTies      = hasTies
  )

  if (is.null(horizon)) horizon <- max(firstTime)

  # mstate::msfit() does not support case-weighted Cox models. Use the full
  # mstate Aalen-Johansen path for unweighted data and survival's equivalent
  # weighted Aalen-Johansen estimator when non-constant weights are supplied.
  useMstateProbabilities <- is.null(varWeight) ||
    (max(data$weight) - min(data$weight) < sqrt(.Machine$double.eps))

  stateFit <- NULL
  stateHazards <- NULL
  stateProbabilities <- NULL
  firstFit <- NULL

  if (useMstateProbabilities) {
    stateFit <- survival::coxph(
      survival::Surv(Tstart, Tstop, status) ~ strata(trans),
      data   = ms$data,
      method = "breslow",
      x      = TRUE
    )
    stateHazards <- mstate::msfit(
      stateFit,
      trans = ms$transitions,
      variance = FALSE
    )
    allStateProbabilities <- mstate::probtrans(
      stateHazards,
      predt = 0,
      variance = FALSE
    )
    stateProbabilities <- allStateProbabilities[[1]]

    probabilityRow <- stateProbabilities[
      stateProbabilities$time <= horizon,
      ,
      drop = FALSE
    ]
    if (nrow(probabilityRow) == 0) {
      probabilityRow <- stateProbabilities[1, , drop = FALSE]
    }
    probabilityRow <- probabilityRow[nrow(probabilityRow), , drop = FALSE]

    pEventFirst <- probabilityRow$pstate2 + probabilityRow$pstate4
    pEvent2First <- probabilityRow$pstate3 + probabilityRow$pstate5
    pSimultaneous <- if (hasTies) probabilityRow$pstate6 else 0
    pNeither <- probabilityRow$pstate1
    probabilityEngine <- "mstate::msfit + mstate::probtrans"
  } else {
    firstProbabilities <- .kmFirstProbabilities(
      firstTime = firstTime,
      firstStatus = firstStatus,
      weight = data$weight,
      horizon = horizon,
      hasTies = hasTies
    )
    pEventFirst <- firstProbabilities$eventFirst
    pEvent2First <- firstProbabilities$event2First
    pSimultaneous <- firstProbabilities$simultaneous
    pNeither <- firstProbabilities$neither
    firstFit <- firstProbabilities$fit
    probabilityEngine <- "survival::survfit weighted Aalen-Johansen"
  }

  idxEventFirst <- which(firstStatus == 1L)
  after <- .kmMstateBranch(
    gap = secondTime[idxEventFirst] - firstTime[idxEventFirst],
    status = secondStatus[idxEventFirst],
    weight = data$weight[idxEventFirst],
    branch = "after",
    truncate = truncate,
    confLevel = confLevel
  )

  idxEvent2First <- which(firstStatus == 2L)
  before <- .kmMstateBranch(
    gap = secondTime[idxEvent2First] - firstTime[idxEvent2First],
    status = secondStatus[idxEvent2First],
    weight = data$weight[idxEvent2First],
    branch = "before",
    truncate = truncate,
    confLevel = confLevel
  )

  if (nrow(before) > 0) {
    conditionalLower <- before$confIntMin
    conditionalUpper <- before$confIntMax
    before$survFunction <- 1 - pEvent2First * before$survFunction
    before$variance <- pEvent2First^2 * before$variance
    before$stdErr <- sqrt(before$variance)
    before$confIntMin <- 1 - pEvent2First * conditionalUpper
    before$confIntMax <- 1 - pEvent2First * conditionalLower
  }

  if (nrow(after) > 0) {
    after$survFunction <- pEventFirst * after$survFunction
    after$variance <- pEventFirst^2 * after$variance
    after$stdErr <- sqrt(after$variance)
    after$confIntMin <- pEventFirst * after$confIntMin
    after$confIntMax <- pEventFirst * after$confIntMax
  }

  result <- rbind(before, after)
  rownames(result) <- NULL

  attr(result, "propEventBeforeEvent2") <- pEventFirst
  attr(result, "propEvent2BeforeEvent") <- pEvent2First
  attr(result, "propSimultaneous")      <- pSimultaneous
  attr(result, "propNeither")           <- pNeither
  attr(result, "horizon")               <- horizon
  attr(result, "transitionMatrix")       <- ms$transitions
  attr(result, "probabilityEngine")      <- probabilityEngine
  attr(result, "ciMethod") <- paste(
    "Conditional log-log intervals from survfit();",
    "uncertainty in Aalen-Johansen branch probabilities is excluded"
  )

  if (isTRUE(returnModels)) {
    attr(result, "models") <- list(
      longData          = ms$data,
      stateFit          = stateFit,
      stateHazards      = stateHazards,
      stateProbabilities = stateProbabilities,
      firstFit          = firstFit
    )
  }

  result
}
