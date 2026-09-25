# >>> Claude 2026-09-20
# Tests for lib/KaplanMeierSurvfit.R. No survey data required.
# Open in RStudio and Source, or set KM_SURVFIT_PATH and run headless.

if (nzchar(Sys.getenv("KM_SURVFIT_PATH"))) {
  source(Sys.getenv("KM_SURVFIT_PATH"))
} else {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source("enadid_lib.R")
  source("lib/KaplanMeierSurvfit.R")
}
library(survival)

ORIGIN <- 1200
approxEq <- function (a, b, tol=1e-8) isTRUE(all.equal(a, b, tolerance=tol))

# Simulate two ordered events with a controllable amount of censoring.
# An event at or after the survey date is simply NOT OBSERVED, which is what
# real survey data looks like and what the cleaning in the estimator assumes.
# censRange gives the censoring time, drawn per woman. A censoring time that
# overlaps the event times is what makes the raw shares biased; an
# administrative cut-off after every event leaves them unbiased and there is
# then nothing for Aalen-Johansen to correct.
makeTwo <- function (n, pE=0.35, pE2=0.55, pTie=0.00, censRange=c(30, 400), seed=1) {
  set.seed(seed)
  gA <- rgeom(n, 0.05) + 1          # event2 -> event
  gB <- rgeom(n, 0.07) + 1          # event  -> event2
  pNone <- max(0, 1 - pE - pE2 - pTie)
  first <- sample(c("E", "E2", "TIE", "none"), n, replace = TRUE,
                  prob = c(pE, pE2, pTie, pNone))
  anchor <- ORIGIN + sample(0:120, n, replace = TRUE)
  censTime <- if (censRange[1] == censRange[2]) rep(censRange[1], n) else
    sample(censRange[1]:censRange[2], n, replace = TRUE)
  d <- data.frame(enter = ORIGIN, cens = ORIGIN + censTime,
                  event = NA_real_, event2 = NA_real_, w = runif(n, 0.5, 2))
  iE <- first == "E"; iE2 <- first == "E2"; iT <- first == "TIE"
  d$event[iE]   <- anchor[iE];             d$event2[iE]  <- anchor[iE] + gB[iE]
  d$event2[iE2] <- anchor[iE2];            d$event[iE2]  <- anchor[iE2] + gA[iE2]
  d$event[iT]   <- anchor[iT];             d$event2[iT]  <- anchor[iT]
  d$event  <- ifelse(!is.na(d$event)  & (d$event  >= d$cens), NA, d$event)
  d$event2 <- ifelse(!is.na(d$event2) & (d$event2 >= d$cens), NA, d$event2)
  d
}

# The raw observed share, which is what KaplanMeierLib.R uses.
rawShare <- function (d, which) {
  tot <- sum(d$w)
  isE  <- !is.na(d$event)  & (is.na(d$event2) | (d$event  < d$event2))
  isE2 <- !is.na(d$event2) & (is.na(d$event)  | (d$event2 < d$event))
  if (which == "E") sum(d$w[isE]) / tot else sum(d$w[isE2]) / tot
}


# ==== 1. Output structure matches KaplanMeier() ====

d <- makeTwo(3000, pE = 0.35, pE2 = 0.55, censRange = c(30, 400), seed = 11)
r <- KaplanMeierSurvfit(d, "enter", "event", "cens", "w", "event2", truncate = 10)

expected <- c("time", "event", "eventRaw", "number", "numberRaw", "surv", "rate",
              "survFunction", "variance", "stdErr", "confIntMax", "confIntMin", "branch")
stopifnot(identical(names(r), expected))
stopifnot(all(r$branch %in% c("before", "after")))
stopifnot(sum(r$branch == "before") > 5, sum(r$branch == "after") > 5)
cat("1. returns the same columns as KaplanMeier(), so the plot code is unchanged\n")


# ==== 2. With no censoring, Aalen-Johansen reduces to the raw shares ====

# Everyone is followed until both events happen, so the order is always known
# and there is nothing for the competing-risks estimator to correct.
dFull <- makeTwo(4000, pE = 0.45, pE2 = 0.55, censRange = c(1e5, 1e5), seed = 21)
stopifnot(!any(is.na(dFull$event)), !any(is.na(dFull$event2)))   # nothing censored
rFull <- KaplanMeierSurvfit(dFull, "enter", "event", "cens", "w", "event2",
                            truncate = NULL, ties = "event")
stopifnot(approxEq(attr(rFull, "propEventBeforeEvent2"), rawShare(dFull, "E"),  tol = 1e-6))
stopifnot(approxEq(attr(rFull, "propEvent2BeforeEvent"), rawShare(dFull, "E2"), tol = 1e-6))
stopifnot(attr(rFull, "propNeither") < 1e-6)
cat("2. with complete observation the branch probabilities equal the raw shares\n")


# ==== 3. With censoring, Aalen-Johansen corrects the raw shares upward ====

dCens <- makeTwo(4000, pE = 0.35, pE2 = 0.55, censRange = c(20, 150), seed = 31)
rCens <- KaplanMeierSurvfit(dCens, "enter", "event", "cens", "w", "event2", truncate = 10)
pE  <- attr(rCens, "propEventBeforeEvent2")
pE2 <- attr(rCens, "propEvent2BeforeEvent")
stopifnot(pE  > rawShare(dCens, "E"))
stopifnot(pE2 > rawShare(dCens, "E2"))
# and the gap at time 0 is therefore narrower than the raw construction gives
gapAJ  <- attr(rCens, "gapAtZero")
gapRaw <- 1 - rawShare(dCens, "E") - rawShare(dCens, "E2")
stopifnot(gapAJ < gapRaw)
# The simulation knows the truth, so check the correction goes the right way and
# lands near it: 0.35 of the sample has 'event' first, 0.55 'event2' first.
stopifnot(abs(pE  - 0.35) < abs(rawShare(dCens, "E")  - 0.35))
stopifnot(abs(pE2 - 0.55) < abs(rawShare(dCens, "E2") - 0.55))
stopifnot(abs(pE  - 0.35) < 0.03, abs(pE2 - 0.55) < 0.03)
cat(sprintf("3. censoring correction: P(event first) %.3f raw -> %.3f AJ (truth 0.35);\n", 
            rawShare(dCens, "E"), pE))
cat(sprintf("   gap at time 0 falls from %.3f to %.3f (truth 0.10)\n", gapRaw, gapAJ))


# ==== 4. The curve is a pair of probabilities with sane bounds ====

stopifnot(all(r$survFunction >= 0), all(r$survFunction <= 1))
stopifnot(all(r$confIntMin >= 0), all(r$confIntMax <= 1))
stopifnot(all(r$confIntMin <= r$survFunction + 1e-12))
stopifnot(all(r$confIntMax >= r$survFunction - 1e-12))

L <- r[r$branch == "before", ]; L <- L[order(L$time), ]
R <- r[r$branch == "after",  ]; R <- R[order(R$time), ]
stopifnot(all(L$time <= 0), all(R$time >= 0))
stopifnot(all(diff(L$survFunction) <= 1e-9))
stopifnot(all(diff(R$survFunction) <= 1e-9))
# Each half meets time 0 at its own branch probability
stopifnot(approxEq(L$survFunction[nrow(L)], 1 - attr(r, "propEvent2BeforeEvent")))
stopifnot(approxEq(R$survFunction[1],           attr(r, "propEventBeforeEvent2")))
# The intervals are asymmetric, so the log-log shape survived the scaling
stopifnot(!approxEq(R$confIntMax - R$survFunction, R$survFunction - R$confIntMin))
cat("4. both halves are monotone probabilities meeting time 0 at their own p\n")


# ==== 5. The four probabilities account for everything ====

stopifnot(approxEq(attr(r, "gapAtZero"),
                   attr(r, "propNeither") + attr(r, "propSimultaneous"), tol = 1e-10))
total <- attr(r, "propEventBeforeEvent2") + attr(r, "propEvent2BeforeEvent") +
  attr(r, "propSimultaneous") + attr(r, "propNeither")
stopifnot(approxEq(total, 1, tol = 1e-6))
cat("5. the four state probabilities sum to 1 at the horizon\n")


# ==== 6. Simultaneous events ====

dTie <- makeTwo(3000, pE = 0.25, pE2 = 0.55, pTie = 0.12, censRange = c(60, 400), seed = 41)
rSim <- KaplanMeierSurvfit(dTie, "enter", "event", "cens", "w", "event2",
                           truncate = 10, ties = "simultaneous")
rEv  <- KaplanMeierSurvfit(dTie, "enter", "event", "cens", "w", "event2",
                           truncate = 10, ties = "event")
stopifnot(attr(rSim, "propSimultaneous") > 0.05)
stopifnot(attr(rEv,  "propSimultaneous") == 0)
# Folding the ties into 'event' moves exactly that mass to the right half
stopifnot(attr(rEv, "propEventBeforeEvent2") >
            attr(rSim, "propEventBeforeEvent2") + 0.05)
stopifnot(approxEq(attr(rEv, "propEventBeforeEvent2"),
                   attr(rSim, "propEventBeforeEvent2") + attr(rSim, "propSimultaneous"),
                   tol = 0.02))
# With ties kept separate, the visible gap at 0 is wider by that mass
stopifnot(attr(rSim, "gapAtZero") > attr(rEv, "gapAtZero"))
cat(sprintf("6. ties: %.3f simultaneous, gap at 0 is %.3f keeping them vs %.3f folding them\n",
            attr(rSim, "propSimultaneous"), attr(rSim, "gapAtZero"), attr(rEv, "gapAtZero")))


# ==== 7. The horizon is explicit and it matters ====

stopifnot(is.finite(attr(r, "horizon")))
rShort <- KaplanMeierSurvfit(d, "enter", "event", "cens", "w", "event2",
                             truncate = 10, horizon = 60)
stopifnot(attr(rShort, "horizon") == 60)
stopifnot(attr(rShort, "propEventBeforeEvent2") < attr(r, "propEventBeforeEvent2"))
stopifnot(attr(rShort, "gapAtZero") > attr(r, "gapAtZero"))
cat("7. a shorter horizon lowers both branch probabilities, as it must\n")


# ==== 8. Bootstrap: wider than the conditional interval ====

dB <- makeTwo(1500, pE = 0.35, pE2 = 0.55, censRange = c(20, 150), seed = 51)
cond <- KaplanMeierSurvfit(dB, "enter", "event", "cens", "w", "event2", truncate = 10)
boot <- KaplanMeierBootstrap(dB, "enter", "event", "cens", "w", "event2", truncate = 10,
                             replicates = 200, seed = 7, progress = FALSE)
stopifnot(nrow(boot) == nrow(cond))
stopifnot(approxEq(boot$survFunction, cond$survFunction))
stopifnot(attr(boot, "bootReplicates") >= 190)
widthCond <- mean(cond$confIntMax - cond$confIntMin)
widthBoot <- mean(boot$confIntMax - boot$confIntMin)
stopifnot(widthBoot > widthCond)
cat(sprintf("8. bootstrap interval is wider than the conditional one (%.4f vs %.4f)\n",
            widthBoot, widthCond))


# ==== 9. Clustering widens it further ====

# 100 PSUs of 30 women. The PSU shifts the chance that 'event' comes first, so
# the outcome is correlated within cluster and a row bootstrap understates.
set.seed(61)
nPSU <- 100; perPSU <- 30
psuShift <- rbeta(nPSU, 2, 2)
rows <- do.call(rbind, lapply(seq_len(nPSU), function (k) {
  dk <- makeTwo(perPSU, pE = 0.15 + 0.5 * psuShift[k],
                pE2 = 0.80 - (0.15 + 0.5 * psuShift[k]),
                censRange = c(30, 200), seed = 1000 + k)
  dk$psu <- k
  dk$stratum <- (k %% 5) + 1
  dk
}))
bRow <- KaplanMeierBootstrap(rows, "enter", "event", "cens", "w", "event2", truncate = 10,
                             replicates = 200, seed = 3, progress = FALSE)
bClu <- KaplanMeierBootstrap(rows, "enter", "event", "cens", "w", "event2", truncate = 10,
                             replicates = 200, seed = 3, progress = FALSE,
                             varCluster = "psu", varStrata = "stratum")
wRow <- mean(bRow$confIntMax - bRow$confIntMin)
wClu <- mean(bClu$confIntMax - bClu$confIntMin)
stopifnot(wClu > wRow)
stopifnot(grepl("clusters in 'psu'", attr(bClu, "bootUnit")))
cat(sprintf("9. clustered bootstrap is wider than the row bootstrap (%.4f vs %.4f, ratio %.2f)\n",
            wClu, wRow, wClu / wRow))


# ==== 10. Degenerate inputs ====

dNo2 <- d; dNo2$event2 <- NA_real_
r0 <- KaplanMeierSurvfit(dNo2, "enter", "event", "cens", "w", "event2", truncate = 10)
stopifnot(is.data.frame(r0), sum(r0$branch == "before") == 0)

dEmpty <- data.frame(enter = c(ORIGIN, ORIGIN), event = c(NA, NA), event2 = c(NA, NA),
                     cens = c(ORIGIN - 5, ORIGIN - 9), w = c(1, 1))
rE <- KaplanMeierSurvfit(dEmpty, "enter", "event", "cens", "w", "event2", truncate = 10)
stopifnot(nrow(rE) == 0, identical(names(rE), expected))

stopifnot(inherits(try(KaplanMeierSurvfit(d, "enter", "event", "cens", "w", NULL),
                       silent = TRUE), "try-error"))
stopifnot(inherits(try(KaplanMeierSurvfit(d, "enter", "nope", "cens", "w", "event2"),
                       silent = TRUE), "try-error"))
cat("10. degenerate and malformed inputs behave\n")


cat("\nALL KAPLAN-MEIER SURVFIT TESTS PASSED\n")
# <<< Claude 2026-09-20
