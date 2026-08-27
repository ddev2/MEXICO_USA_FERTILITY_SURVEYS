# ==== Harmonize column types across NSFG surveys ====

# ---- Canonical factor level definitions ----
# Keep these in ONE place. If you ever add a new level in a single survey,
# add it here so every survey-year is rebuilt with the same level set.

LEVELS_UNION_STATUS <- c(
  "married",
  "cohabiting",
  "widowed",
  "divorced",
  "separated",
  "widowed/divorced/separated",
  "single",
  "single with own children",
  "other",
  "refused",
  "unknown",
  "NA"
)

LEVELS_PREGNANT <- c("yes", "no", "refused", "unknown", "NA")

LEVELS_WANT_ANOTHER <- c(
  "yes", "no", "disagree", "up to god", "up to God",
  "refused", "unknown", "NA"
)

LEVELS_PROB_WANT_ANOTHER <- c(
  "probably yes", "probably no", "refused", "unknown", "NA"
)

LEVELS_EVER_CONTRACEPTION <- c("yes", "no", "refused", "unknown", "NA")

# Raw union_status codes -> canonical labels. These are the NSFG RMARITAL codes
# (identical to the union_status factor's own code order): 1 married,
# 2 cohabiting, 3 widowed, 4 divorced, 5 separated, 6 never-married/single.
# The 2002+ cycles reach harmonization with union_status reduced to these
# integer codes; pre-2002 surveys arrive already labelled and pass through.
# (Previously 3->other, 4->single, and 5 unmapped, which sent every 2002+
# widowed/divorced/separated woman to NA -- the "unexpected value 5" warning.)
UNION_STATUS_RECODE <- c(
  "1" = "married",
  "2" = "cohabiting",
  "3" = "widowed",
  "4" = "divorced",
  "5" = "separated",
  "6" = "single"
)

# ---- Helper: force a column to factor with given levels ----
# Accepts logical-NA, character, or existing factor input and returns
# a factor with the canonical level set. Values not in `levels` become NA
# (with a warning so you catch unexpected codes).

force_factor <- function(x, levels, col_name = NULL) {
  if (is.factor(x)) {
    cur <- as.character(x)
  } else if (is.character(x)) {
    cur <- x
  } else if (is.logical(x) && all(is.na(x))) {
    cur <- rep(NA_character_, length(x))
  } else if (all(is.na(x))) {
    cur <- rep(NA_character_, length(x))
  } else {
    cur <- as.character(x)
  }

  bad <- !is.na(cur) & !(cur %in% levels)
  if (any(bad)) {
    warning(sprintf(
      "force_factor(): %d unexpected value(s) in column '%s': %s",
      sum(bad), col_name %||% "?",
      paste(unique(cur[bad]), collapse = ", ")
    ))
    cur[bad] <- NA_character_
  }

  factor(cur, levels = levels)
}

# null-coalescing helper (in case purrr isn't loaded)
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- Main harmonizer ----
# Applies the type contract to one survey-year dataframe.

harmonize_survey_types <- function(d) {
  # Plain text columns: always character, never factor
  if ("country" %in% names(d))  d$country  <- as.character(d$country)
  if ("survey"  %in% names(d))  d$survey   <- as.character(d$survey)

  if ("union_status" %in% names(d)) {
    us <- as.character(d$union_status)
    # Recode any raw integer codes unconditionally; string labels pass through untouched
    is_raw_code <- us %in% names(UNION_STATUS_RECODE) & !is.na(us)
    us[is_raw_code] <- UNION_STATUS_RECODE[us[is_raw_code]]
    d$union_status <- force_factor(us, LEVELS_UNION_STATUS, "union_status")
  }
  
  # Categorical columns: always factor with the canonical levels above
  if ("pregnant" %in% names(d)) {
    d$pregnant <- force_factor(d$pregnant, LEVELS_PREGNANT, "pregnant")
  }
  if ("want_another" %in% names(d)) {
    d$want_another <- force_factor(d$want_another, LEVELS_WANT_ANOTHER, "want_another")
  }
  if ("prob_want_another" %in% names(d)) {
    d$prob_want_another <- force_factor(
      d$prob_want_another, LEVELS_PROB_WANT_ANOTHER, "prob_want_another"
    )
  }
  if ("ever_contraception" %in% names(d)) {
    d$ever_contraception <- force_factor(
      d$ever_contraception, LEVELS_EVER_CONTRACEPTION, "ever_contraception"
    )
  }

  d
}
