# >>> Claude 2026-09-21
# harm_union_type(): one canonical level set for the union_start_type columns.
#
# WHY THIS EXISTS. The readers build union_start_typeN independently per survey,
# and they do not agree. Mexico ends up with
#     cohabitation | cohabitation before marriage | don't know | marriage
# while the NSFG side ends up with
#     marriage | cohabitation | unknown | refused | don't remember | cohabitation before marriage
# Worse, the numeric codes behind those labels differ BETWEEN NSFG cycles:
# "NSFG import.R" line 194 uses levels c(0,1,9) -> marriage, cohabitation, unknown,
# and line 437 uses c(1,2,9) -> cohabitation, marriage, unknown. So the codes are
# not comparable and only the LABELS are. This function therefore refuses a
# numeric or integer column outright rather than guessing which coding it is.
#
# WHAT IT DOES. For union_start_type1 .. union_start_type<maxUnions>, it maps the
# labels onto four canonical levels, in this order, "don't know" last:
#
#     cohabitation
#     cohabitation before marriage
#     marriage
#     don't know
#
# "unknown", "refused" and "don't remember" all fold into "don't know". Those
# three carry 0 cases in the NSFG data as of 2026-09, and "don't know" carries 2
# cases in ENADID 1997, so the fold costs nothing and makes the two sides bind.
# Real NA stays NA: not knowing the type of a union that exists is a different
# thing from there being no union.
#
# NOTE ON maxUnions. It is the number of union slots the dataset carries, which
# is 7 for MEXICO_ENADID (see the 1:7 loop in ReadENADID.R) and 10 for
# NSFG_ENADID. Slots that do not exist in the data frame are skipped with a note.
#
# NOTE ON NSFG 2017-19. Every union_start_type1 is NA for that cycle. This is
# expected, not a defect: "NSFG import.R" records "2017-19: incomplete UH".


UNION_TYPE_LEVELS <- c("cohabitation",
                       "cohabitation before marriage",
                       "marriage",
                       "don't know")

# Lower-case label -> canonical level. Add a row here when a reader invents a
# new wording; do not add mappings anywhere else.
UNION_TYPE_ALIASES <- c(
  "cohabitation"                 = "cohabitation",
  "cohabiting"                   = "cohabitation",
  "consensual union"             = "cohabitation",
  "union libre"                  = "cohabitation",
  "cohabitation before marriage" = "cohabitation before marriage",
  "cohabitation then marriage"   = "cohabitation before marriage",
  "cohab before marriage"        = "cohabitation before marriage",
  "marriage"                     = "marriage",
  "married"                      = "marriage",
  "matrimonio"                   = "marriage",
  "don't know"                   = "don't know",
  "dont know"                    = "don't know",
  "do not know"                  = "don't know",
  "unknown"                      = "don't know",
  "refused"                      = "don't know",
  "don't remember"               = "don't know",
  "dont remember"                = "don't know",
  "no sabe"                      = "don't know",
  "not specified"                = "don't know"
)


harm_union_type <- function (df = NULL, maxUnions = 1, varPrefix = "union_start_type",
                             strict = TRUE, quiet = FALSE) {
  #==> df: the data frame to harmonize
  #==> maxUnions: how many union slots the data frame carries (7 for MEXICO_ENADID, 10 for NSFG_ENADID)
  #==> varPrefix: column stem, so the same function can serve another family of columns
  #==> strict: TRUE stops on a label that is not in UNION_TYPE_ALIASES, naming it and its count.
  #    FALSE maps it to "don't know" and reports. Leave it TRUE unless you have looked at the label.
  #==> quiet: TRUE suppresses the per-column report
  #<== df, with every union_start_typeN a factor on UNION_TYPE_LEVELS

  if (is.null(df)) stop("harm_union_type: df cannot be NULL")
  if (!is.data.frame(df)) stop("harm_union_type: df must be a data.frame")
  if (!is.numeric(maxUnions) || (length(maxUnions) != 1) || (maxUnions < 1)) {
    stop("harm_union_type: maxUnions must be a single positive number")
  }

  cols    <- paste0(varPrefix, seq_len(maxUnions))
  present <- cols[cols %in% names(df)]
  absent  <- setdiff(cols, present)
  if (length(present) == 0) {
    stop(sprintf("harm_union_type: none of %s1..%s%d are in the data frame",
                 varPrefix, varPrefix, maxUnions))
  }
  if (!quiet && (length(absent) > 0)) {
    message(sprintf("harm_union_type: %d slot(s) not in the data and skipped: %s",
                    length(absent), paste(absent, collapse = ", ")))
  }

  unknownAll <- character(0)
  foldedAll  <- 0L

  for (cc in present) {
    v <- df[[cc]]

    # Numeric codes are NOT comparable across surveys or across NSFG cycles.
    if (is.numeric(v) && !is.factor(v)) {
      stop(sprintf(paste0("harm_union_type: '%s' is numeric. The codes behind these labels differ ",
                          "between surveys and between NSFG cycles, so they cannot be harmonized ",
                          "safely. Label it in its reader first, then call this function."), cc))
    }

    v <- as.character(v)
    v <- trimws(v)
    v[v %in% c("", "NA", "<NA>")] <- NA_character_

    key    <- tolower(v)
    mapped <- unname(UNION_TYPE_ALIASES[key])

    bad <- (!is.na(v)) & is.na(mapped)
    if (any(bad)) {
      unknownAll <- c(unknownAll, v[bad])
      if (!strict) mapped[bad] <- "don't know"
    }

    # How many labels actually changed meaning, for the report
    folded <- sum((!is.na(v)) & (!is.na(mapped)) & (tolower(v) != mapped), na.rm = TRUE)
    foldedAll <- foldedAll + folded

    df[[cc]] <- factor(mapped, levels = UNION_TYPE_LEVELS)

    if (!quiet && (folded > 0)) {
      changed <- sort(unique(v[(!is.na(v)) & (!is.na(mapped)) & (tolower(v) != mapped)]))
      message(sprintf("harm_union_type: %s, %d value(s) relabelled (%s)",
                      cc, folded, paste(changed, collapse = ", ")))
    }
  }

  if (length(unknownAll) > 0) {
    tab <- table(unknownAll)
    txt <- paste(sprintf("\"%s\" (%d)", names(tab), as.integer(tab)), collapse = ", ")
    if (strict) {
      stop(sprintf(paste0("harm_union_type: label(s) not in UNION_TYPE_ALIASES: %s. ",
                          "Add them to UNION_TYPE_ALIASES, or pass strict = FALSE to fold ",
                          "them into \"don't know\"."), txt))
    }
    message(sprintf("harm_union_type: %s folded into \"don't know\" (strict = FALSE)", txt))
  }

  if (!quiet) {
    message(sprintf("harm_union_type: %d column(s) harmonized, %d value(s) relabelled in total",
                    length(present), foldedAll))
  }

  return (df)
}
# <<< Claude 2026-09-21
