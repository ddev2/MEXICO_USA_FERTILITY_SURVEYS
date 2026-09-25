source("lib/KaplanMeierMstate.R")


# ==== 1. Complete histories reproduce the repaired estimator ====

set.seed(19)
n <- 1500
origin <- 1200L
first <- sample(c("event", "event2"), n, replace = TRUE, prob = c(0.4, 0.6))
anchor <- origin + sample(1:100, n, replace = TRUE)
gap <- rgeom(n, 0.08) + 1

completeData <- data.frame(
  enter  = rep(origin, n),
  cens   = rep(origin + 500, n),
  event  = NA_real_,
  event2 = NA_real_
)
eventFirst <- first == "event"
completeData$event[eventFirst] <- anchor[eventFirst]
completeData$event2[eventFirst] <- anchor[eventFirst] + gap[eventFirst]
completeData$event2[!eventFirst] <- anchor[!eventFirst]
completeData$event[!eventFirst] <- anchor[!eventFirst] + gap[!eventFirst]

completeResult <- KaplanMeierMstate(
  completeData,
  varEnter = "enter",
  varEvent = "event",
  varCens = "cens",
  varEvent2 = "event2",
  truncate = NULL
)

stopifnot(
  abs(attr(completeResult, "propEventBeforeEvent2") - mean(eventFirst)) < 1e-12,
  abs(attr(completeResult, "propEvent2BeforeEvent") - mean(!eventFirst)) < 1e-12,
  abs(attr(completeResult, "propNeither")) < 1e-12,
  all(diff(completeResult$survFunction) <= 1e-12)
)


# ==== 2. Censoring and simultaneous events ====

set.seed(20)
n <- 3000
first <- sample(
  c("event", "event2", "simultaneous", "none"),
  n,
  replace = TRUE,
  prob = c(0.45, 0.30, 0.05, 0.20)
)
anchor <- origin + sample(0:180, n, replace = TRUE)
gap <- rgeom(n, 0.06) + 1
cens <- origin + sample(80:240, n, replace = TRUE)

censoredData <- data.frame(
  enter  = rep(origin, n),
  cens   = cens,
  event  = NA_real_,
  event2 = NA_real_,
  weight = runif(n, 0.5, 2)
)
eventFirst <- first == "event"
event2First <- first == "event2"
simultaneous <- first == "simultaneous"

censoredData$event[eventFirst] <- anchor[eventFirst]
censoredData$event2[eventFirst] <- anchor[eventFirst] + gap[eventFirst]
censoredData$event2[event2First] <- anchor[event2First]
censoredData$event[event2First] <- anchor[event2First] + gap[event2First]
censoredData$event[simultaneous] <- anchor[simultaneous]
censoredData$event2[simultaneous] <- anchor[simultaneous]
censoredData$event[censoredData$event > censoredData$cens] <- NA
censoredData$event2[censoredData$event2 > censoredData$cens] <- NA

censoredResult <- KaplanMeierMstate(
  censoredData,
  varEnter = "enter",
  varEvent = "event",
  varCens = "cens",
  varWeight = "weight",
  varEvent2 = "event2",
  truncate = 10,
  ties = "simultaneous"
)

probabilities <- c(
  attr(censoredResult, "propEventBeforeEvent2"),
  attr(censoredResult, "propEvent2BeforeEvent"),
  attr(censoredResult, "propSimultaneous"),
  attr(censoredResult, "propNeither")
)
stopifnot(
  abs(sum(probabilities) - 1) < 1e-8,
  all(probabilities >= 0),
  all(probabilities <= 1),
  all(is.finite(censoredResult$survFunction)),
  all(censoredResult$survFunction >= 0),
  all(censoredResult$survFunction <= 1),
  all(censoredResult$confIntMin <= censoredResult$survFunction),
  all(censoredResult$confIntMax >= censoredResult$survFunction)
)


# ==== 3. Weight scaling does not change point estimates ====

scaledData <- censoredData
scaledData$weight <- scaledData$weight * 100

scaledResult <- KaplanMeierMstate(
  scaledData,
  varEnter = "enter",
  varEvent = "event",
  varCens = "cens",
  varWeight = "weight",
  varEvent2 = "event2",
  truncate = 10,
  ties = "simultaneous"
)

stopifnot(
  isTRUE(all.equal(
    censoredResult$survFunction,
    scaledResult$survFunction,
    tolerance = 1e-10
  )),
  abs(
    attr(censoredResult, "propEventBeforeEvent2") -
      attr(scaledResult, "propEventBeforeEvent2")
  ) < 1e-10
)

cat("ALL KAPLAN-MEIER MSTATE TESTS PASSED\n")
