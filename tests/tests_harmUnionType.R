# >>> Claude 2026-09-21
# Tests for lib/unionType_lib.R. No survey data required.
# Open in RStudio and Source, or set HARM_LIB_PATH and run headless.

if (nzchar(Sys.getenv("HARM_LIB_PATH"))) {
  source(Sys.getenv("HARM_LIB_PATH"))
} else {
  # >>> Claude 2026-09-25: tests now live in tests/; work from the repository root
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
# <<< Claude 2026-09-25
  source("enadid_lib.R")
  source("lib/unionType_lib.R")
}

# The two label sets exactly as they stand in MEXICO_ENADID and NSFG_ENADID.
mexLev  <- c("cohabitation", "cohabitation before marriage", "don't know", "marriage")
nsfgLev <- c("marriage", "cohabitation", "unknown", "refused", "don't remember",
             "cohabitation before marriage")


# ==== 1. Both sides end on the same four levels, "don't know" last ====

set.seed(1)
mex <- data.frame(
  survey = "ENADID1997",
  union_start_type1 = factor(sample(c(mexLev, NA), 400, TRUE,
                                    prob = c(.20, .13, .01, .46, .20)), levels = mexLev),
  union_start_type2 = factor(sample(c("cohabitation", "marriage", NA), 400, TRUE,
                                    prob = c(.1, .1, .8)), levels = mexLev))
nsfg <- data.frame(
  survey = "NSFG2011_13",
  union_start_type1 = factor(sample(c(nsfgLev, NA), 300, TRUE,
                                    prob = c(.16, .30, 0, 0, 0, .21, .33)), levels = nsfgLev),
  union_start_type2 = factor(sample(c("marriage", "cohabitation", NA), 300, TRUE,
                                    prob = c(.1, .1, .8)), levels = nsfgLev))

mexH  <- harm_union_type(mex,  maxUnions = 7,  quiet = TRUE)
nsfgH <- harm_union_type(nsfg, maxUnions = 10, quiet = TRUE)

stopifnot(identical(levels(mexH$union_start_type1),  UNION_TYPE_LEVELS))
stopifnot(identical(levels(nsfgH$union_start_type1), UNION_TYPE_LEVELS))
stopifnot(identical(levels(mexH$union_start_type1), levels(nsfgH$union_start_type1)))
stopifnot(tail(UNION_TYPE_LEVELS, 1) == "don't know")
cat("1. both sides carry:", paste(UNION_TYPE_LEVELS, collapse = " | "), "\n")


# ==== 2. Nothing is lost ====

for (nm in c("union_start_type1", "union_start_type2")) {
  stopifnot(sum(is.na(mex[[nm]]))  == sum(is.na(mexH[[nm]])))
  stopifnot(sum(is.na(nsfg[[nm]])) == sum(is.na(nsfgH[[nm]])))
  stopifnot(sum(!is.na(mexH[[nm]])) == sum(!is.na(mex[[nm]])))
}
cat("2. counts and NA preserved on every slot\n")


# ==== 3. The three NSFG missing-labels fold into "don't know" ====

nsfg2 <- nsfg
nsfg2$union_start_type1 <- factor(c("unknown", "refused", "don't remember", "marriage",
                                    rep(NA, nrow(nsfg) - 4)), levels = nsfgLev)
n2 <- harm_union_type(nsfg2, maxUnions = 10, quiet = TRUE)
stopifnot(sum(n2$union_start_type1 == "don't know", na.rm = TRUE) == 3)
stopifnot(sum(n2$union_start_type1 == "marriage",   na.rm = TRUE) == 1)
cat("3. unknown / refused / don't remember all fold into don't know\n")


# ==== 4. The point of the exercise: the two sides bind ====

both <- rbind(mexH[, c("survey", "union_start_type1")],
              nsfgH[, c("survey", "union_start_type1")])
stopifnot(identical(levels(both$union_start_type1), UNION_TYPE_LEVELS))
stopifnot(sum(is.na(both$union_start_type1)) ==
            sum(is.na(mexH$union_start_type1)) + sum(is.na(nsfgH$union_start_type1)))
cat("4. rbind gives one factor and no level was coerced to NA\n")
print(table(both$survey, both$union_start_type1, useNA = "ifany"))


# ==== 5. An unseen label stops by default ====

odd <- mex
levels(odd$union_start_type1) <- c(levels(odd$union_start_type1), "civil union")
odd$union_start_type1[1:5] <- "civil union"
stopifnot(inherits(try(harm_union_type(odd, 7, quiet = TRUE), silent = TRUE), "try-error"))
o2 <- harm_union_type(odd, 7, strict = FALSE, quiet = TRUE)
stopifnot(sum(o2$union_start_type1 == "don't know", na.rm = TRUE) >= 5)
cat("5. unseen label stops with strict = TRUE, folds with strict = FALSE\n")


# ==== 6. Numeric codes are refused ====

# "NSFG import.R" line 194 codes c(0,1,9) as marriage/cohabitation/unknown and
# line 437 codes c(1,2,9) as cohabitation/marriage/unknown. Harmonizing numbers
# would silently swap the first two categories between cycles.
num <- data.frame(union_start_type1 = c(0L, 1L, 9L, 1L))
stopifnot(inherits(try(harm_union_type(num, 1, quiet = TRUE), silent = TRUE), "try-error"))
cat("6. numeric codes refused, because the codings differ between NSFG cycles\n")


# ==== 7. Absent slots are skipped, a wrong prefix is an error ====

few <- data.frame(union_start_type1 = factor(c("marriage", "cohabitation"), levels = mexLev))
f <- harm_union_type(few, maxUnions = 7, quiet = TRUE)
stopifnot(nrow(f) == 2, identical(levels(f$union_start_type1), UNION_TYPE_LEVELS))
stopifnot(inherits(try(harm_union_type(few, 7, varPrefix = "nope", quiet = TRUE),
                       silent = TRUE), "try-error"))
cat("7. missing slots skipped; a wrong prefix errors\n")


cat("\nALL harm_union_type TESTS PASSED\n")
# <<< Claude 2026-09-21
