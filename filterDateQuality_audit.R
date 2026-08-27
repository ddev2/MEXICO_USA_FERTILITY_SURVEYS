setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")

# ==== filterDateQuality removal audit ====
#
# Answers, without changing any filterDateQuality() default:
#   1. How many women does the DEFAULT filterDateQuality() actually remove?
#   2. Do the cleanENADID summary columns "Flagged" and "DK_end" overlap?
#
# Key point: with defaults, DK_end (union_end flag 10) is NOT removed
#   - dropUnknownEnd = FALSE           -> separation DK-ends (cmc = NA) kept
#   - dropEndYear9999 keys on raw cmc>9900 -> NA cases are skipped
# so "women removed" is NOT Flagged + DK_end. It is close to Flagged alone.
#
# Run on the frame cleanENADID() returned (AFTER cleanENADID, BEFORE filtering).

filterQualityAudit <- function(df) {

  # stable per-row key: survives filterDateQuality()'s internal addID + ID drop
  df$.rowkey <- seq_len(nrow(df))

  # rows the DEFAULT call keeps vs removes (defaults untouched, quiet)
  kept_keys <- filterDateQuality(df, verbose = FALSE)$.rowkey
  removed   <- setdiff(df$.rowkey, kept_keys)

  # DK_end membership: any union_end_cmc_I == 10 (same rule as the summary)
  ue_I  <- grep("^union_end_cmc_I", names(df), value = TRUE)
  is_DK <- if (length(ue_I) > 0L)
    apply(df[ue_I], 1L, function(x) any(!is.na(x) & x == 10L))
  else rep(FALSE, nrow(df))
  dk_keys <- df$.rowkey[is_DK]

  data.frame(
    survey          = as.character(df$survey[1]),
    nWomen          = nrow(df),
    removed_default = length(removed),                    # <-- the number you asked for
    DK_end          = length(dk_keys),
    DK_end_removed  = length(intersect(dk_keys, removed)),# overlap: DK_end the default DOES drop
    DK_end_kept     = length(setdiff(dk_keys, removed))   # DK_end the default KEEPS (not removed)
  )
}

# --- usage ---
# single survey frame:
#   filterQualityAudit(ENADID)
#
# pooled frame with a `survey` column:
#   do.call(rbind, lapply(split(ENADID, ENADID$survey), filterQualityAudit))
