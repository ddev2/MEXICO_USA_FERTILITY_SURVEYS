# >>> Claude 2026-09-21
# Tests for lib/unionEpisodes.R. No survey data required: the fixtures are
# built in ENADID/NSFG column shape so the reshaping is tested as it will run.
# Open in RStudio and Source, or set EPI_LIB_PATH and run headless.

if (nzchar(Sys.getenv("EPI_LIB_PATH"))) {
  source(Sys.getenv("EPI_LIB_PATH"))
} else {
  # >>> Claude 2026-09-25: tests now live in tests/; work from the repository root
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
# <<< Claude 2026-09-25
  source("enadid_lib.R")
  source("lib/unionEpisodes.R")
}
library(survival)

cmc <- function (m, y) (y - 1900) * 12 + m
SURV <- cmc(1, 2020)

# A hand-built set of unions, one per situation that has to be handled.
fx <- data.frame(
  who = c("direct, still married", "direct, separated", "direct, widowed",
          "cohab, still cohabiting", "cohab, separated", "cohab, widowed",
          "converted, still married", "converted, separated",
          "cohab-before-marriage, no marriage date", "marriage in the union's own month",
          "union type missing", "union starts after the survey"),
  indiv_dob_cmc      = cmc(1, 1985),
  surveyDate_cmc     = SURV,
  union_start_cmc1   = c(cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2005),
                         cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2005), cmc(1,2030)),
  union_start_type1  = c("marriage","marriage","marriage","cohabitation","cohabitation","cohabitation",
                         "cohabitation before marriage","cohabitation before marriage",
                         "cohabitation before marriage","cohabitation", NA, "marriage"),
  marriage_start_cmc1= c(cmc(1,2005), cmc(1,2005), cmc(1,2005), NA, NA, NA,
                         cmc(1,2008), cmc(1,2008), NA, cmc(1,2005), NA, cmc(1,2030)),
  union_end_cmc1     = c(NA, cmc(1,2015), cmc(1,2012), NA, cmc(1,2010), cmc(1,2011),
                         NA, cmc(1,2016), NA, NA, NA, NA),
  union_end_motive1  = c("in union","separation","widowhood","in union","separation","widowhood",
                         "in union","separation","in union","in union","in union","in union"),
  weight = 1, country = "Mexico", survey = "TEST", stringsAsFactors = FALSE)

ep <- buildUnionEpisodes(fx, u = 1, quiet = TRUE)
row <- function (w, ...) ep[ep$id == which(fx$who == w), c("istate","to","tstart","tstop", ...)]


# ==== 1. Every situation lands in the right state ====

stopifnot(nrow(row("union type missing")) == 0)          # dropped
stopifnot(nrow(row("union starts after the survey")) == 0)

d1 <- row("direct, still married")
stopifnot(nrow(d1) == 1, as.character(d1$istate) == "married direct", as.character(d1$to) == "censor")
stopifnot(as.character(row("direct, separated")$to) == "separation (married)")
stopifnot(as.character(row("direct, widowed")$to)   == "widowed")

c1 <- row("cohab, still cohabiting")
stopifnot(nrow(c1) == 1, as.character(c1$istate) == "cohabiting", as.character(c1$to) == "censor")
stopifnot(as.character(row("cohab, separated")$to) == "separation (cohabiting)")
stopifnot(as.character(row("cohab, widowed")$to)   == "widowed")

# a marriage dated in the union's own month is a direct marriage
stopifnot(as.character(row("marriage in the union's own month")$istate) == "married direct")
# cohabitation-before-marriage with no marriage date cannot convert
nm <- row("cohab-before-marriage, no marriage date")
stopifnot(nrow(nm) == 1, as.character(nm$istate) == "cohabiting")
cat("1. all twelve situations land in the right state\n")


# ==== 1b. Every motive wording the readers produce ====

mot <- data.frame(
  who = c("divorce","don't know","unknown","in union with an end date","a wording nobody uses"),
  indiv_dob_cmc = cmc(1,1985), surveyDate_cmc = SURV,
  union_start_cmc1 = cmc(1,2005), union_start_type1 = "marriage",
  marriage_start_cmc1 = cmc(1,2005), union_end_cmc1 = cmc(1,2012),
  union_end_motive1 = c("divorce","don't know","unknown","in union","se fue a Marte"),
  weight = 1, country = "Mexico", survey = "TEST", stringsAsFactors = FALSE)

# A divorce is a separation: that is what sections 2 to 4 of KaplanMeier.R assume.
m1 <- buildUnionEpisodes(mot, u = 1, quiet = TRUE)
stopifnot(as.character(m1$to[m1$id == 1]) == "separation (married)")
# Everything else that ended for an unclear reason gets its own absorbing state
stopifnot(all(as.character(m1$to[m1$id %in% 2:5]) == "ended (unknown reason)"))

# ... and can be folded into separation, to match sections 2 to 4 exactly
m2 <- buildUnionEpisodes(mot, u = 1, quiet = TRUE, unknownEndAs = "separation")
stopifnot(all(as.character(m2$to) == "separation (married)"))
# ... or treated as still observed, which it is not, but the option exists
m3 <- buildUnionEpisodes(mot, u = 1, quiet = TRUE, unknownEndAs = "censor")
stopifnot(all(as.character(m3$to[m3$id %in% 2:5]) == "censor"))
stopifnot(as.character(m3$to[m3$id == 1]) == "separation (married)")
cat("1b. divorce counts as separation; unknown reasons are handled three ways\n")


# ==== 2. A converting union produces two contiguous spells ====

cv <- row("converted, separated")
stopifnot(nrow(cv) == 2)
stopifnot(as.character(cv$istate) == c("cohabiting", "married converted"))
stopifnot(as.character(cv$to)     == c("married converted", "separation (married)"))
stopifnot(cv$tstart[1] == 0, cv$tstop[1] == cv$tstart[2])           # contiguous, no gap
stopifnot(cv$tstop[1] == 36)                                         # 2005-01 to 2008-01
stopifnot(cv$tstop[2] == (cmc(1,2016) - cmc(1,2005)))
cat("2. a converting union gives two contiguous spells with the right durations\n")


# ==== 3. Ages and cohabitation duration ====

a <- ep[ep$id == which(fx$who == "converted, separated"), ][1, ]
stopifnot(abs(a$ageUnion    - 20) < 1e-8)     # born 1985-01, union 2005-01
stopifnot(abs(a$ageMarriage - 23) < 1e-8)     # marriage 2008-01
stopifnot(abs(a$cohabDur    -  3) < 1e-8)
b <- ep[ep$id == which(fx$who == "direct, separated"), ][1, ]
stopifnot(abs(b$ageUnion - b$ageMarriage) < 1e-8)   # identical for a direct marriage
stopifnot(b$cohabDur == 0)
d <- ep[ep$id == which(fx$who == "cohab, separated"), ][1, ]
stopifnot(is.na(d$ageMarriage), is.na(d$cohabDur))
stopifnot(a$yUnion == 2005)
cat("3. ageUnion == ageMarriage for direct marriages; cohabDur is their difference\n")


# ==== 4. Nothing is censored later than the survey ====

stopifnot(all(ep$tstop <= (SURV - cmc(1, 2005))))
stopifnot(all(ep$tstop > ep$tstart))
cat("4. no spell runs past the survey date and none has zero length\n")


# ==== 5. State occupancy sums to 1 and survfit accepts the shape ====

set.seed(5)
n <- 4000
sim <- data.frame(
  indiv_dob_cmc  = cmc(1, 1980) + sample(0:180, n, TRUE),
  surveyDate_cmc = SURV,
  weight = runif(n, .5, 2), country = "Mexico", survey = "SIM", stringsAsFactors = FALSE)
sim$union_start_cmc1  <- sim$indiv_dob_cmc + round(runif(n, 18, 32) * 12)
sim$union_start_type1 <- sample(c("cohabitation","marriage"), n, TRUE, prob = c(.6,.4))
convAt <- sim$union_start_cmc1 + round(rexp(n, 1/60))
sim$marriage_start_cmc1 <- ifelse(sim$union_start_type1 == "marriage", sim$union_start_cmc1,
                           ifelse(runif(n) < .5 & convAt < SURV, convAt, NA))
endAt <- sim$union_start_cmc1 + round(rexp(n, 1/140))
sim$union_end_cmc1    <- ifelse(endAt < SURV & runif(n) < .6, endAt, NA)
sim$union_end_motive1 <- ifelse(is.na(sim$union_end_cmc1), "in union",
                         ifelse(runif(n) < .9, "separation", "widowhood"))
sim <- subset(sim, union_start_cmc1 < SURV)

epS <- buildUnionEpisodes(sim, u = 1, quiet = TRUE)
occ <- unionStateOccupancy(epS, by = "origin", maxYears = 25)
tot <- tapply(occ$p, paste(occ$group, occ$timeYear), sum)
stopifnot(max(abs(tot - 1)) < 1e-8)
stopifnot(all(occ$p >= -1e-12), all(occ$p <= 1 + 1e-12))
stopifnot(setequal(unique(occ$group), c("cohabitation", "direct marriage")))
# a direct-marriage union is never in a cohabiting or converted state
dm <- subset(occ, group == "direct marriage" & state %in% c("cohabiting", "married converted"))
stopifnot(max(dm$p) < 1e-12)
cat("5. state occupancy sums to 1 at every duration, in both strata\n")


# ==== 6. Competing risks from the cohabiting state ====

cr <- unionCompetingRisks(epS, from = "cohabiting", horizonYears = c(5, 10))
stopifnot(all(c("horizonYears","group") %in% names(cr$table)))
num <- setdiff(names(cr$table), c("horizonYears","group"))
stopifnot(all(abs(rowSums(cr$table[num]) - 1) < 1e-6))     # everyone is somewhere
stopifnot(all(cr$curves$p >= -1e-12), all(cr$curves$p <= 1 + 1e-12))
# the cumulative incidences only grow
for (st in unique(cr$curves$state)) {
  s <- cr$curves$p[cr$curves$state == st]
  if (st != "(s0)") stopifnot(all(diff(s) >= -1e-9))
}
cat("6. cumulative incidences are monotone and sum to 1 with the survivors\n")
print(cr$table, row.names = FALSE)


# ==== 7. Argument checking ====

stopifnot(inherits(try(buildUnionEpisodes(NULL), silent = TRUE), "try-error"))
stopifnot(inherits(try(buildUnionEpisodes(fx, u = 9, quiet = TRUE), silent = TRUE), "try-error"))
stopifnot(inherits(try(unionCompetingRisks(epS, from = "nowhere"), silent = TRUE), "try-error"))
cat("7. missing columns and impossible arguments are refused\n")


cat("\nALL unionEpisodes TESTS PASSED\n")
# <<< Claude 2026-09-21
