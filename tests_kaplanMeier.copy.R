# >>> Claude 2026-09-19
# Regression tests for KaplanMeierLib.R. No survey data required.
# Open in RStudio and Source. Every check uses stopifnot(), so the script stops
# at the first failure and prints "ALL KAPLAN-MEIER TESTS PASSED" at the end.
#
# The ordinary branch is checked against survival::survfit(), which is the
# reference implementation. The mirrored branch (varEvent2) has no external
# reference, so it is checked against its own definition and against the
# invariants a mirrored curve must satisfy.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
source("lib/KaplanMeierLib.R")
library(survival)

ORIGIN <- 1200L   # an arbitrary CMC entry date, so durations are not the dates


# ==== 1. Helpers ====

# Build a df in the shape KaplanMeier() expects, from durations and status.
makeDf <- function (dur, status, weight=NULL, censAt=NULL) {
  n <- length(dur)
  if (is.null(censAt)) censAt <- max(dur) + 1
  df <- data.frame(
    enter = rep(ORIGIN, n),
    event = ifelse(status == 1, ORIGIN + dur, NA_real_),
    cens  = ifelse(status == 1, ORIGIN + censAt, ORIGIN + dur)
  )
  if (!is.null(weight)) df$w <- weight
  return (df)
}

# survfit reference. NOTE: fit$std.err is the standard error of the CUMULATIVE
# HAZARD. summary()$std.err is the standard error of the survival, which is what
# KaplanMeier() returns in stdErr, so the comparison goes through summary().
refKM <- function (dur, status) {
  fit <- survfit(Surv(dur, status) ~ 1)
  sf  <- summary(fit, times = fit$time, extend = TRUE)
  data.frame(time = sf$time, surv = sf$surv, se = sf$std.err)
}

approxEq <- function (a, b, tol=1e-8) isTRUE(all.equal(a, b, tolerance=tol))


# ==== 2. Ordinary branch against survfit() ====

set.seed(42)
n      <- 500
dur    <- rgeom(n, 0.05) + 1
status <- as.integer(dur < 40)
dur    <- pmin(dur, 40)

df  <- makeDf(dur, status, censAt = 40)
res <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                   truncate = NULL, fixHighRates = FALSE)
ref <- refKM(dur, status)

m <- merge(res[, c("time", "survFunction", "stdErr")], ref, by = "time")
stopifnot(nrow(m) == nrow(ref))
stopifnot(approxEq(m$survFunction, m$surv, tol = 1e-10))
stopifnot(approxEq(m$stdErr, m$se, tol = 1e-8))
cat("2. ordinary branch matches survfit() on", nrow(m), "event times\n")

# The curve must start at 1 and never increase.
stopifnot(res$survFunction[1] == 1 || min(res$time) == min(dur))
stopifnot(all(diff(res$survFunction) <= 1e-12))
stopifnot(all(res$confIntMin >= 0), all(res$confIntMax <= 1))


# ==== 3. Ties, and an event at duration 0 ====

dur    <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 6, 6, 9, 9, 9, 14, 22)
status <- c(1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0,  1,  0)
df     <- makeDf(dur, status, censAt = 30)
res    <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                      truncate = NULL, fixHighRates = FALSE)
ref    <- refKM(dur, status)
m      <- merge(res[, c("time", "survFunction")], ref, by = "time")
stopifnot(nrow(m) == nrow(ref))
stopifnot(approxEq(m$survFunction, m$surv, tol = 1e-10))
# The drop at the LAST event time must be present (the old code lost it).
lastEvent <- max(dur[status == 1])
stopifnot(res$survFunction[res$time == lastEvent] < res$survFunction[res$time == 9])

# An event at duration 0 (for instance marriage in the month the union starts)
# must produce its drop AT 0, with no artificial origin row above it.
dur0    <- c(0, 0, 0, 1, 1, 2, 2, 5, 5, 5)
status0 <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0)
res0    <- KaplanMeier(makeDf(dur0, status0, censAt = 20), "enter", "event", "cens",
                       varWeight = NULL, truncate = NULL, fixHighRates = FALSE)
stopifnot(min(res0$time) == 0)
stopifnot(sum(res0$time == 0) == 1)
stopifnot(approxEq(res0$survFunction[res0$time == 0], 0.7))
cat("3. ties, the final drop and a duration-0 event are handled\n")


# ==== 4. Degenerate samples ====

# 4a. every observation censored: S stays at 1
df  <- makeDf(c(3, 5, 7, 9), c(0, 0, 0, 0))
res <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL, truncate = NULL)
stopifnot(all(res$survFunction == 1))

# 4b. a single distinct time (the old 2:length() loop broke here)
df  <- makeDf(c(4, 4, 4), c(1, 1, 1))
res <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                   truncate = NULL, fixHighRates = FALSE)
stopifnot(nrow(res) >= 1, all(is.finite(res$survFunction)))
stopifnot(approxEq(min(res$survFunction), 0))

# 4c. varEnter after varCens everywhere: empty, correctly shaped result
df <- data.frame(enter = c(ORIGIN, ORIGIN), event = c(NA, NA), cens = c(ORIGIN - 5, ORIGIN - 9))
res <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL, truncate = NULL)
stopifnot(nrow(res) == 0, "survFunction" %in% names(res), "eventRaw" %in% names(res))
cat("4. degenerate samples return sane, correctly shaped results\n")


# ==== 5. Weights ====

set.seed(7)
n      <- 400
dur    <- rgeom(n, 0.06) + 1
status <- as.integer(dur < 35)
dur    <- pmin(dur, 35)

# 5a. a constant weight of 1 must reproduce the unweighted result
dfU <- makeDf(dur, status, censAt = 35)
dfW <- makeDf(dur, status, weight = rep(1, n), censAt = 35)
resU <- KaplanMeier(dfU, "enter", "event", "cens", varWeight = NULL,
                    truncate = NULL, fixHighRates = FALSE)
resW <- KaplanMeier(dfW, "enter", "event", "cens", varWeight = "w",
                    truncate = NULL, fixHighRates = FALSE)
stopifnot(approxEq(resU$survFunction, resW$survFunction))
stopifnot(approxEq(resU$stdErr, resW$stdErr))

# 5b. rescaling all weights by a constant must not move the point estimate,
#     but it DOES move the Greenwood interval. This is the documented
#     limitation, asserted here so it cannot change unnoticed.
dfW10 <- makeDf(dur, status, weight = rep(10, n), censAt = 35)
resW10 <- KaplanMeier(dfW10, "enter", "event", "cens", varWeight = "w",
                      truncate = NULL, fixHighRates = FALSE)
stopifnot(approxEq(resW$survFunction, resW10$survFunction))
stopifnot(max(resW10$stdErr) < max(resW$stdErr))
cat("5. weighted and unweighted paths agree; weight scale affects only the CI\n")


# ==== 6. Truncation ====

set.seed(11)
n      <- 600
dur    <- rgeom(n, 0.04) + 1
status <- as.integer(dur < 90)
dur    <- pmin(dur, 90)
df     <- makeDf(dur, status, censAt = 90)

resFull <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL, truncate = NULL)
resTrunc <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL, truncate = 10)
# Truncation must remove the LONG-duration tail, never the early times.
stopifnot(min(resTrunc$time) == min(resFull$time))
stopifnot(max(resTrunc$time) < max(resFull$time))
cat("6. truncation removes the long-duration tail only\n")


# ==== 7. Mirrored branch ====

set.seed(2026)
n <- 3000
# event  = "first birth", event2 = "first union", both as CMC dates
gapA   <- rgeom(n, 0.05) + 1              # union -> birth
gapB   <- rgeom(n, 0.07) + 1              # birth -> union
first  <- sample(c("A", "B", "none"), n, replace = TRUE, prob = c(0.55, 0.35, 0.10))
anchor <- ORIGIN + sample(0:120, n, replace = TRUE)
censAt <- ORIGIN + 400

dfM <- data.frame(enter = ORIGIN, cens = censAt,
                  event = NA_real_, event2 = NA_real_,
                  w = runif(n, 0.5, 2))
isA <- first == "A"     # event2 (union) first, then event (birth)
isB <- first == "B"     # event (birth) first, then event2 (union)
dfM$event2[isA]  <- anchor[isA]
dfM$event[isA]   <- anchor[isA] + gapA[isA]
dfM$event[isB]   <- anchor[isB]
dfM$event2[isB]  <- anchor[isB] + gapB[isB]

resM <- KaplanMeier(dfM, "enter", "event", "cens", varWeight = "w", varEvent2 = "event2",
                    truncate = 10)

pA <- attr(resM, "propEventBeforeEvent2")   # 'event' (birth) first  -> right half
pB <- attr(resM, "propEvent2BeforeEvent")   # 'event2' (union) first -> left half
stopifnot(is.finite(pA), is.finite(pB), pA > 0, pB > 0, (pA + pB) <= 1)

# Both halves carry a row at time 0, so split on 'branch', not on the sign.
stopifnot("branch" %in% names(resM))
left  <- subset(resM, branch == "before")
right <- subset(resM, branch == "after")
stopifnot(nrow(left) > 5, nrow(right) > 5)
stopifnot(all(left$time <= 0), all(right$time >= 0))

# 7a. every value is a probability, and the intervals bracket the curve
stopifnot(all(resM$survFunction >= 0), all(resM$survFunction <= 1))
stopifnot(all(resM$confIntMin >= 0), all(resM$confIntMax <= 1))
stopifnot(all(resM$confIntMin <= resM$survFunction + 1e-12))
stopifnot(all(resM$confIntMax >= resM$survFunction - 1e-12))

# 7b. the intervals must belong to the ADJUSTED curve, not to the raw one.
#     Before the fix, confIntMax was the unadjusted bound and this failed.
inner <- subset(resM, (confIntMax < 1 - 1e-9) & (confIntMin > 1e-9))
stopifnot(nrow(inner) > 10)
stopifnot(approxEq(inner$confIntMax - inner$survFunction,
                   inner$survFunction - inner$confIntMin, tol = 1e-6))

# 7c. the left branch approaches 1 at long negative durations and 1 - pB at 0-
left <- left[order(left$time), ]
stopifnot(all(diff(left$survFunction) <= 1e-9))          # decreasing towards 0
stopifnot(abs(left$survFunction[nrow(left)] - (1 - pB)) < 0.05)

# 7d. the right branch starts near pA and decreases
right <- right[order(right$time), ]
stopifnot(all(diff(right$survFunction) <= 1e-9))
stopifnot(abs(right$survFunction[1] - pA) < 0.05)

# 7e. the left tail must be truncated, not retained. Before the fix the
#     mirrored mask kept every row, including points with one event.
resMfull <- KaplanMeier(dfM, "enter", "event", "cens", varWeight = "w",
                        varEvent2 = "event2", truncate = NULL)
stopifnot(min(resM$time) > min(resMfull$time))

# 7f. standard errors must stay finite on the left branch. The old variance
#     transformation diverged as the conditional survival approached 0.
stopifnot(all(is.finite(resM$stdErr)))
stopifnot(max(resM$stdErr) < 0.5)
cat("7. mirrored branch: scaling, intervals and truncation are consistent\n")


# ==== 8. Tiny or empty left group ====

# Three or fewer cases with 'event2' first: the left branch must be absent,
# and rbind() must not fail (the old fallback frame lacked eventRaw).
dfTiny <- dfM[1:400, ]
dfTiny$event2[which(dfTiny$event2 < dfTiny$event)] <- NA   # drop the union-first cases
res <- KaplanMeier(dfTiny, "enter", "event", "cens", varWeight = "w",
                   varEvent2 = "event2", truncate = 10)
stopifnot(is.data.frame(res))
stopifnot(sum(res$branch == "before") == 0)
stopifnot(all(res$time >= 0))

# Entirely empty second event
dfNoE2 <- dfM
dfNoE2$event2 <- NA_real_
res <- KaplanMeier(dfNoE2, "enter", "event", "cens", varWeight = "w",
                   varEvent2 = "event2", truncate = 10)
stopifnot(is.data.frame(res), all(res$time >= 0))
stopifnot(sum(res$branch == "before") == 0)
cat("8. tiny and empty mirrored groups no longer break rbind()\n")


# ==== 9. fixHighRates switch ====

dur    <- c(1, 2, 3, 3)
status <- c(0, 0, 1, 1)     # both remaining cases fail at t = 3, so rate = 1
df     <- makeDf(dur, status, censAt = 20)
resOn  <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                      truncate = NULL, fixHighRates = TRUE)
resOff <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                      truncate = NULL, fixHighRates = FALSE)
stopifnot(approxEq(min(resOff$survFunction), 0))     # the honest estimate
stopifnot(min(resOn$survFunction) > 0)               # the legacy override
stopifnot(all(is.finite(resOn$stdErr)), all(is.finite(resOff$stdErr)))
cat("9. fixHighRates behaves as documented and leaves the variance finite\n")



# ==== 10. useSurvfit: equivalence, robust variance, log-log bounds ====

set.seed(42)
n      <- 500
dur    <- rgeom(n, 0.05) + 1
status <- as.integer(dur < 40)
dur    <- pmin(dur, 40)
df     <- makeDf(dur, status, censAt = 40)

# 10a. With confType = "plain" and robustVar = FALSE the survfit path must
#      reproduce the hand-written estimator exactly. This is the anchor: if it
#      ever fails, one of the two paths has drifted.
own <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                   truncate = NULL, fixHighRates = FALSE)
sfv <- KaplanMeier(df, "enter", "event", "cens", varWeight = NULL,
                   truncate = NULL, fixHighRates = FALSE,
                   useSurvfit = TRUE, confType = "plain", robustVar = FALSE)
stopifnot(approxEq(own$time, sfv$time))
stopifnot(approxEq(own$survFunction, sfv$survFunction))
stopifnot(approxEq(own$stdErr, sfv$stdErr))
stopifnot(approxEq(own$confIntMax, sfv$confIntMax))
stopifnot(approxEq(own$confIntMin, sfv$confIntMin))

set.seed(3)
w    <- runif(n, 0.5, 2)
dfW  <- makeDf(dur, status, weight = w, censAt = 40)
ownW <- KaplanMeier(dfW, "enter", "event", "cens", varWeight = "w",
                    truncate = NULL, fixHighRates = FALSE)
sfvW <- KaplanMeier(dfW, "enter", "event", "cens", varWeight = "w",
                    truncate = NULL, fixHighRates = FALSE,
                    useSurvfit = TRUE, confType = "plain", robustVar = FALSE)
stopifnot(approxEq(ownW$survFunction, sfvW$survFunction))
stopifnot(approxEq(ownW$stdErr, sfvW$stdErr))

# 10b. The robust (infinitesimal jackknife) variance does not depend on the
#      SCALE of the weights. Greenwood does, which is the defect it fixes.
dfW10 <- makeDf(dur, status, weight = w * 10, censAt = 40)
rob   <- KaplanMeier(dfW,   "enter", "event", "cens", varWeight = "w", truncate = NULL,
                     fixHighRates = FALSE, useSurvfit = TRUE, robustVar = TRUE)
rob10 <- KaplanMeier(dfW10, "enter", "event", "cens", varWeight = "w", truncate = NULL,
                     fixHighRates = FALSE, useSurvfit = TRUE, robustVar = TRUE)
own10 <- KaplanMeier(dfW10, "enter", "event", "cens", varWeight = "w", truncate = NULL,
                     fixHighRates = FALSE)
stopifnot(approxEq(max(rob$stdErr), max(rob10$stdErr), tol = 1e-6))   # invariant
stopifnot(!approxEq(max(ownW$stdErr), max(own10$stdErr)))             # not invariant
stopifnot(max(rob$stdErr) > max(ownW$stdErr))   # Greenwood understates here

# 10c. log-log bounds stay inside [0, 1], carry no NA, and are asymmetric.
ll <- KaplanMeier(dfW, "enter", "event", "cens", varWeight = "w", truncate = NULL,
                  fixHighRates = FALSE, useSurvfit = TRUE,
                  confType = "log-log", robustVar = TRUE)
stopifnot(all(ll$confIntMin >= 0), all(ll$confIntMax <= 1))
stopifnot(all(is.finite(ll$confIntMax)), all(is.finite(ll$confIntMin)))
stopifnot(!approxEq(ll$confIntMax - ll$survFunction, ll$survFunction - ll$confIntMin))

# 10d. The mirrored branch runs through survfit and gives the same point
#      estimates (only the intervals may differ).
m1 <- KaplanMeier(dfM, "enter", "event", "cens", varWeight = "w",
                  varEvent2 = "event2", truncate = 10)
m2 <- KaplanMeier(dfM, "enter", "event", "cens", varWeight = "w",
                  varEvent2 = "event2", truncate = 10, fixHighRates = FALSE,
                  useSurvfit = TRUE, confType = "log-log", robustVar = TRUE)
stopifnot(nrow(m1) == nrow(m2), approxEq(m1$time, m2$time))
stopifnot(approxEq(m1$survFunction, m2$survFunction, tol = 1e-10))
stopifnot(all(m2$confIntMin >= 0), all(m2$confIntMax <= 1))

# 10e. Degenerate cases must survive the survfit path too.
r <- KaplanMeier(makeDf(c(3, 5, 7, 9), c(0, 0, 0, 0)), "enter", "event", "cens",
                 varWeight = NULL, truncate = NULL, fixHighRates = FALSE,
                 useSurvfit = TRUE, confType = "log-log")
stopifnot(all(r$survFunction == 1))
r <- KaplanMeier(makeDf(c(4, 4, 4), c(1, 1, 1)), "enter", "event", "cens",
                 varWeight = NULL, truncate = NULL, fixHighRates = FALSE,
                 useSurvfit = TRUE, confType = "log-log")
stopifnot(approxEq(min(r$survFunction), 0), all(is.finite(c(r$confIntMin, r$confIntMax))))
cat("10. useSurvfit reproduces the hand-written path, and fixes the weight scale\n")


cat("\nALL KAPLAN-MEIER TESTS PASSED\n")
# <<< Claude 2026-09-19
