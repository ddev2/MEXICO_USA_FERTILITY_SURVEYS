# >>> Claude 2026-09-23
# Tests for lib/childUnionContext.R on simulated data. Run from the R folder:
#   source("tests/tests_childUnionContext.R")
# No survey data needed.

if (!exists("childPrepare")) source(file.path("lib", "childUnionContext.R"))
library(survival)


# ==== Simulated createFirstUnionFirstBirth() output ====

# A population that does NOT change over calendar time: the same distribution of
# the mother's age at first birth and the same union behaviour in every cohort.
# Stability rises with the mother's age at the birth. Surveys follow the NSFG
# calendar with its upper ages, so any trend in the estimates is an artifact.
simFirstBirths <- function (surveys, nPerSurvey = 6000, country = "USA", seed = 1) {
  set.seed(seed)
  out <- lapply(seq_len(nrow(surveys)), function (i) {
    sc   <- (surveys$year[i] - 1900) * 12 + 6
    amax <- surveys$amax[i]
    n    <- nPerSurvey
    age  <- runif(n, 15, amax + 1)
    dob  <- round(sc - age * 12)
    hasB <- runif(n) < 0.85
    aB   <- pmin(pmax(rnorm(n, 24, 5), 15), 42)
    b    <- round(dob + aB * 12)
    hasB <- hasB & b <= sc
    lg   <- function (x) 1 / (1 + exp(-x))
    # state at birth: before any union, or in the first union
    bornBefore <- runif(n) < lg(-0.4 - 0.15 * (aB - 24))
    u1 <- ifelse(bornBefore, b + round(rexp(n, 1 / 40)), b - round(runif(n, 0, 60)))
    # first union dissolution: rate falls with the mother's age at the birth
    rate1 <- 0.012 * exp(-0.09 * (aB - 24))
    e1    <- u1 + pmax(1, round(rexp(n, rate1)))
    u2    <- e1 + round(rexp(n, 1 / 30))
    e2    <- u2 + pmax(1, round(rexp(n, 1 / 90)))
    # censor at the survey
    u1[u1 > sc] <- NA
    e1[is.na(u1) | e1 > sc] <- NA
    u2[is.na(e1) | u2 > sc] <- NA
    e2[is.na(u2) | e2 > sc] <- NA
    death <- ifelse(runif(n) < 0.03, b + round(runif(n, 0, 120)), NA)
    death[!is.na(death) & death > sc] <- NA
    data.frame(surveyName = surveys$name[i], country = country,
               cmc_birth = dob, cmc_survey = sc,
               cmc_union1 = u1, cmc_sep1 = e1, cmc_union2 = u2, cmc_sep2 = e2,
               cmc_union3 = NA_real_, cmc_sep3 = NA_real_,
               cmc_union4 = NA_real_, cmc_sep4 = NA_real_,
               cmc_birth1 = ifelse(hasB, b, NA), cmc_death1 = ifelse(hasB, death, NA),
               popWeight = 1)
  })
  do.call(rbind, out)
}

NSFG_CAL <- data.frame(
  name = c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG1988", "NSFG1995", "NSFG2002",
           "NSFG2006_10", "NSFG2011_13", "NSFG2013_15", "NSFG2015_17", "NSFG2022_23"),
  year = c(1973, 1976, 1982, 1988, 1995, 2002, 2008, 2012, 2014, 2016, 2022),
  amax = c(rep(44, 9), 49, 49))

sim   <- simFirstBirths(NSFG_CAL)
chSim <- childPrepare(sim, firstUnionOnly = character(0))


# ==== Test 1: complete windows reproduce the direct overlap calculation ====

direct4 <- function (ch) {
  cc <- ch[ch$complete, ]
  S  <- attr(ch, "S")[ch$complete, ]; E <- attr(ch, "E")[ch$complete, ]
  L  <- attr(ch, "LifeLimit")
  lo <- cc$b; hi <- cc$b + L
  ov <- function (s, e) ifelse(is.na(s), 0, pmax(0, pmin(hi, e) - pmax(lo, s)))
  before <- ifelse(is.na(S[, 1]), L, pmax(0, pmin(hi, S[, 1]) - lo))
  d1 <- ov(S[, 1], E[, 1]); d2 <- ov(S[, 2], E[, 2])
  sep <- L - before - d1 - d2
  tot <- sum(cc$w) * L
  c(noUnion = sum(cc$w * before) / tot, union1 = sum(cc$w * d1) / tot,
    outUnion = sum(cc$w * sep) / tot, union2plus = sum(cc$w * d2) / tot)
}
r1 <- childDirect(chSim, groupVars = "country", replicates = 0)
t1 <- max(abs(unlist(r1[1, CHILD_STATES]) - direct4(chSim)))
cat("Test 1, AJ on complete windows vs direct overlap, max abs diff:", signif(t1, 3), "\n")
stopifnot(t1 < 1e-10)


# ==== Test 2: monthly AJ equals survfit() on censored, weighted data ====

sub <- chSim[chSim$surveyName == "NSFG2002", ]
for (a in c("S", "E")) attr(sub, a) <- attr(chSim, a)[chSim$surveyName == "NSFG2002", , drop = FALSE]
attr(sub, "LifeLimit") <- 120L
ep  <- childEpisodes(sub)
set.seed(2); wv <- runif(nrow(sub), 0.5, 2)
aj  <- .ajMonthly(ep$from, ep$to, ep$tstart, ep$tstop, wv[ep$idx], 120)

sd  <- ep
sd$event <- factor(ifelse(sd$to == 0, "censor", CHILD_STATES[pmax(sd$to, 1)]),
                   levels = c("censor", CHILD_STATES))
sd$istate <- factor(CHILD_STATES[sd$from], levels = CHILD_STATES)
sd$wt <- wv[sd$idx]
fit <- survfit(Surv(tstart, tstop, event) ~ 1, data = sd, id = idx, istate = istate, weights = wt)
# Read the step function directly from the fit rather than through summary(),
# which in some versions of survival drops time 0 (119 rows instead of 120).
pst <- fit$pstate
if (length(dim(pst)) == 3) pst <- pst[, 1, ]
pos <- findInterval(0:119, fit$time)                 # last event time <= t
ps  <- rbind(fit$p0, pst)[pos + 1, match(CHILD_STATES, fit$states), drop = FALSE]
t2  <- max(abs(ps - aj$P[1:120, ]))
cat("Test 2, monthly AJ vs survfit multistate, max abs diff over months 0-119:", signif(t2, 3), "\n")
stopifnot(t2 < 1e-8)


# ==== Test 3: the artifact and its removal ====

truthCh <- childPrepare(simFirstBirths(data.frame(name = "TRUTH", year = 2090, amax = 100),
                                       nPerSurvey = 60000, seed = 3),
                        firstUnionOnly = character(0), quiet = TRUE)
truth <- childDirect(truthCh, groupVars = "country", replicates = 0)
cat("\nTrue values (no truncation):\n"); print(truth[, c(CHILD_STATES, "bornU1", "intactU1", "joint")])

cur <- childDirect(chSim, replicates = 0)
cs  <- childDirect(childCommonSupport(chSim, 28, quiet = TRUE), replicates = 0)
truth28 <- childDirect(childCommonSupport(truthCh, 28, quiet = TRUE), groupVars = "country", replicates = 0)
aj  <- childStratifiedAJ(chSim, replicates = 0)

tab <- data.frame(cohort = sort(unique(cur$cohort)))
tab$current     <- cur$joint[match(tab$cohort, cur$cohort)]
tab$nCurrent    <- cur$n[match(tab$cohort, cur$cohort)]
tab$common28    <- cs$joint[match(tab$cohort, cs$cohort)]
tab$ajStd       <- aj$std$joint[match(tab$cohort, aj$std$cohort)]
aj3 <- childStratifiedAJ(childPrepare(sim, firstUnionOnly = character(0), quiet = TRUE,
                                      ageBreaks = c(-Inf, 20, 25, Inf),
                                      ageLabels = c("<20", "20-24", "25-35")),
                         replicates = 0, minRiskEnd = 10)
tab$ajStd3groups <- aj3$std$joint[match(tab$cohort, aj3$std$cohort)]
cat("\nJoint P(born in first union and intact at 10) by cohort.",
    "True value: all mothers", round(truth$joint, 3), "| mothers <= 28", round(truth28$joint, 3), "\n")
print(tab, digits = 3, row.names = FALSE)
# <<< Claude 2026-09-23
