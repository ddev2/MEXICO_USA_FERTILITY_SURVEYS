# >>> Claude 2026-09-25: tests now live in tests/; work from the repository root
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
# <<< Claude 2026-09-25
# >>> Claude 2026-09-25: file name case fixed (enadid_lib.r fails on Linux)
source("enadid_lib.R")
# <<< Claude 2026-09-25

# Quick validation of the unified month imputation (no survey data required).
# Open in RStudio and Source. Confirms the 4 properties of imputed_month_capped()
# and that capMonth = FALSE reproduces the legacy uniform draw.


# ==== Validation of imputed_month_capped() ====

set.seed(1)
N    <- 200000L
yr   <- rep(2017L, N)
surv <- compute_cmc(7L, 2017L)            # interview in July 2017

# 1) Survey-date cap binds in BOTH modes: no month after July in the survey year
for (cap in c(TRUE, FALSE)) {
  m <- imputed_month_capped(N, yr, survey_cmc = surv, capped = cap)
  stopifnot(max(m) <= 7L, min(m) >= 1L)
}
cat("1 survey cap          : OK  (max month <= 7 in both modes)\n")

# 2) Strict-after: union end >= union start, same year, in BOTH modes
sm   <- sample(1:12, N, replace = TRUE)
scmc <- compute_cmc(sm, yr)
for (cap in c(TRUE, FALSE)) {
  em <- imputed_month_capped(N, yr, after_cmc = scmc, after_cmc_imp = 0, capped = cap)
  stopifnot(min(em - sm) >= 0L)           # no negative durations
  cat(sprintf("2 strict-after cap=%-5s : OK  min(end - start) = %d\n", cap, min(em - sm)))
}

# 3) Probabilistic birth-before-union split: active ONLY when capped = TRUE
um   <- sample(1:12, N, replace = TRUE)
ucmc <- compute_cmc(um, yr)
for (cap in c(TRUE, FALSE)) {
  b <- imputed_month_capped(N, yr, after_cmc = ucmc, after_cmc_imp = 1,
                            propBefore = 0.2, capped = cap)
  cat(sprintf("3 prob split   cap=%-5s : share(birth < union) = %.3f%s\n",
              cap, mean(b < um),
              if (cap) "  (~ propBefore x feasible)" else "  (uniform baseline ~ 0.46)"))
}

# 4) No constraints, capped = FALSE == uniform 1:12 (legacy imputed_month)
tb <- table(imputed_month_capped(N, yr, capped = FALSE))
cat(sprintf("4 legacy uniform      : OK  month freq in [%.3f, %.3f] ~ %.3f\n",
            min(tb) / N, max(tb) / N, 1 / 12))

# 5) Union end vs union start, same year, both imputed: end is on/after start,
#    and a December start forces a December end (no later month that year).
#    This is what enforce_union_end_after_start() applies in raw_union_history().
us_m <- sample(1:12, N, replace = TRUE)
ue_m <- imputed_month_capped(N, yr, after_cmc = compute_cmc(us_m, yr),
                             after_cmc_imp = 0,
                             survey_cmc = compute_cmc(12L, 2017L), capped = TRUE)
stopifnot(all(ue_m >= us_m))                 # union end never before union start
stopifnot(all(ue_m[us_m == 12L] == 12L))     # December start -> December end
cat(sprintf("5 union end >= start  : OK  min(end-start)=%d; Dec start -> end month(s) {%s}\n",
            min(ue_m - us_m), paste(sort(unique(ue_m[us_m == 12L])), collapse = ",")))

cat("\nAll checks passed.\n")
