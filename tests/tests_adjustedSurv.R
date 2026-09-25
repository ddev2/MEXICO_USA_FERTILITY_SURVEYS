# >>> Claude 2026-09-21
# Tests for lib/adjustedSurv.R. No survey data required.
# Open in RStudio and Source, or set ADJ_LIB_PATH and run headless.

if (nzchar(Sys.getenv("ADJ_LIB_PATH"))) {
  source(Sys.getenv("ADJ_LIB_PATH"))
} else {
  # >>> Claude 2026-09-25: tests now live in tests/; work from the repository root
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
# <<< Claude 2026-09-25
  source("enadid_lib.R")
  source("lib/adjustedSurv.R")
}
library(survival)

approxEq <- function (a, b, tol=1e-8) isTRUE(all.equal(a, b, tolerance=tol))

# A data set where the group has a KNOWN effect and one covariate confounds it.
set.seed(404)
n <- 20000
x <- rnorm(n)                                  # the confounder
g <- rbinom(n, 1, plogis(0.9 * x))             # group depends on x
lp <- log(0.70) * g + 0.55 * x                 # true group effect: HR 0.70
tt <- rexp(n, rate = 0.08 * exp(lp))
cc <- runif(n, 0.5, 20)
d <- data.frame(t = pmin(tt, cc), sep = as.integer(tt <= cc),
                grp = factor(ifelse(g == 1, "treated", "control"), levels = c("control","treated")),
                x = x, w = runif(n, 0.5, 2))


# ==== 1. The adjustment recovers the truth; no adjustment does not ====

a0 <- adjustedSurv(d, "t", "sep", "grp", covariates = NULL, weightVar = "w")
a1 <- adjustedSurv(d, "t", "sep", "grp", covariates = "x",  weightVar = "w")
stopifnot(abs(a1$hr[["grptreated"]] - 0.70) < 0.03)
stopifnot(abs(a0$hr[["grptreated"]] - 0.70) > 0.05)
cat(sprintf("1. unadjusted HR %.3f, adjusted HR %.3f, truth 0.700\n",
            a0$hr[["grptreated"]], a1$hr[["grptreated"]]))


# ==== 2. The curves are proper survival functions ====

for (a in list(a0, a1)) {
  stopifnot(all(a$curves$surv >= 0), all(a$curves$surv <= 1))
  for (lv in levels(d$grp)) {
    s <- a$curves$surv[a$curves$group == lv]
    stopifnot(all(diff(s) <= 1e-12))          # non-increasing
    stopifnot(approxEq(s[1], 1, tol = 1e-6))  # starts at 1
  }
  stopifnot(setequal(as.character(unique(a$curves$group)), levels(d$grp)))
}
cat("2. every curve starts at 1, is non-increasing and stays in [0,1]\n")


# ==== 3. Standardisation: both curves use the SAME covariate distribution ====

# With the covariate given zero effect, adjusting must not move the curves.
d2 <- d
set.seed(7); d2$z <- rnorm(nrow(d2))            # pure noise, unrelated to anything
b0 <- adjustedSurv(d2, "t", "sep", "grp", covariates = NULL, weightVar = "w")
bz <- adjustedSurv(d2, "t", "sep", "grp", covariates = "z",  weightVar = "w")
stopifnot(abs(b0$hr[["grptreated"]] - bz$hr[["grptreated"]]) < 0.01)
stopifnot(max(abs(b0$curves$surv - bz$curves$surv)) < 0.01)
cat("3. adjusting for a covariate with no effect leaves the curves alone\n")


# ==== 4. Weights are honoured ====

dw <- d; dw$w <- ifelse(dw$grp == "treated", 5, 1)
aw <- adjustedSurv(dw, "t", "sep", "grp", covariates = "x", weightVar = "w")
au <- adjustedSurv(d,  "t", "sep", "grp", covariates = "x", weightVar = NULL)
stopifnot(!approxEq(aw$curves$surv, au$curves$surv))
stopifnot(abs(aw$hr[["grptreated"]] - 0.70) < 0.05)   # still near the truth
cat("4. weights change the standardisation and the estimate stays sane\n")


# ==== 5. It agrees with survfit(newdata=) on a small sample ====

# survfit(fit, newdata = whole sample) is the textbook route. It builds one
# curve per row, so it only works on a small data set, but it is the reference.
small <- d[1:400, ]
f <- coxph(Surv(t, sep) ~ grp + x, data = small, weights = w)
ref <- sapply(levels(small$grp), function (lv) {
  nd <- small; nd$grp <- factor(lv, levels = levels(small$grp))
  s  <- survfit(f, newdata = nd)
  approx(s$time, apply(s$surv, 1, weighted.mean, w = nd$w),
         xout = 5, method = "constant", f = 0, rule = 2)$y
})
mine <- adjustedSurv(small, "t", "sep", "grp", "x", "w", times = c(0, 5))
got <- sapply(levels(small$grp), function (lv)
  mine$curves$surv[(mine$curves$group == lv) & (mine$curves$time == 5)])
stopifnot(max(abs(ref - got)) < 1e-6)
cat(sprintf("5. matches survfit(newdata=) at t=5: %.6f vs %.6f, %.6f vs %.6f\n",
            ref[1], got[1], ref[2], got[2]))


# ==== 6. Three groups, and argument checking ====

d3 <- d; d3$grp3 <- factor(sample(c("a","b","c"), nrow(d3), TRUE))
a3 <- adjustedSurv(d3, "t", "sep", "grp3", "x", "w")
stopifnot(length(unique(a3$curves$group)) == 3, length(a3$hr) == 2)

stopifnot(inherits(try(adjustedSurv(NULL), silent = TRUE), "try-error"))
stopifnot(inherits(try(adjustedSurv(d, "t", "sep", "nope", "x", "w"), silent = TRUE), "try-error"))
bad <- d; bad$w[1] <- -1
stopifnot(inherits(try(adjustedSurv(bad, "t", "sep", "grp", "x", "w"), silent = TRUE), "try-error"))
one <- d; one$grp <- factor("only")
stopifnot(inherits(try(adjustedSurv(one, "t", "sep", "grp", "x", "w"), silent = TRUE), "try-error"))
cat("6. three groups work; bad arguments are refused\n")


cat("\nALL adjustedSurv TESTS PASSED\n")
adjustedSurv_note()
# <<< Claude 2026-09-21
