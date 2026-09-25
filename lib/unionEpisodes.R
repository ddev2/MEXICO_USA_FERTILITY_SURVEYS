# >>> Claude 2026-09-21
# Union histories reshaped into episodes, and the two descriptive estimators
# that read off them. This is the input for the four plots discussed in
# "Union analysis: methods, status and plan".
#
# WHY EPISODES. A union that begins as cohabitation and later converts occupies
# two states in turn. Splitting it into one row per state lets survfit() follow
# it through them, and lets a hazard model treat "currently married" as a
# time-varying property rather than a label attached at time 0. Labelling a
# union at time 0 by whether it will eventually convert uses the future to
# define the present, which is the anticipatory analysis problem. Episodes
# avoid it: a union contributes cohabiting exposure until it converts, and
# married exposure afterwards, and nothing about its future was used.
#
# The two marriages ARE kept apart, as "married direct" and "married
# converted". That is legitimate, because at the moment a couple enters the
# married state you already know how their union began. It is past information.
#
# TIME UNIT: months (CMC), matching the rest of the project. AGES in years.

UNION_ISTATES <- c("cohabiting", "married direct", "married converted")
UNION_TO      <- c("censor", "married converted",
                   "separation (cohabiting)", "separation (married)",
                   "widowed", "ended (unknown reason)")

# union_end_motive is labelled differently by different readers. Every wording
# in the project, lower-cased, mapped onto what it means for the episode:
#   ReadMujeres2006/2018/2023 : separation | widowhood | divorce | don't know
#   ReadMujeres1997           : separation | widowhood | don't know
#   WFS_to_ENADID             : in union | widowhood | separation
#   NSFG import               : in union | separation | widowhood | unknown
#   ReadEDER2017 / 2025       : ... | divorce
# A divorce ends the union by the couple's decision, so it counts as a
# separation, which is what sections 2 to 4 of KaplanMeier.R already assume.
UNION_MOTIVE_MAP <- c(
  "separation"  = "separation",
  "separated"   = "separation",
  "divorce"     = "separation",
  "divorced"    = "separation",
  "widowhood"   = "widowed",
  "widowed"     = "widowed",
  "in union"    = "unknown",
  "don't know"  = "unknown",
  "dont know"   = "unknown",
  "unknown"     = "unknown",
  "no sabe"     = "unknown")


buildUnionEpisodes <- function (df = NULL, u = 1,
                                varDob = "indiv_dob_cmc", varCens = "surveyDate_cmc",
                                varWeight = "weight", idVar = NULL,
                                carry = c("country", "survey"), quiet = FALSE,
                                unknownEndAs = c("own state", "separation", "censor")) {
  #==> df: MEXICO_ENADID, NSFG_ENADID or a subset of either
  #==> u: which union slot, so union_start_cmc<u> and friends
  #==> varDob: date of birth, for the ages. NULL skips them
  #==> carry: extra columns copied onto every episode
  #==> unknownEndAs: a union that ended for a reason the data does not give.
  #    "own state" keeps it as its own absorbing state, which is the honest
  #    default and shows up as a band in the occupancy plot. "separation" folds
  #    it in, matching what sections 2 to 4 of KaplanMeier.R do. "censor"
  #    treats it as still under observation, which it is not.
  #<== one row per union-state spell, with tstart / tstop in months since the
  #    union began, istate, to, origin, ageUnion, ageMarriage, cohabDur, yUnion

  if (is.null(df)) stop("buildUnionEpisodes: df cannot be NULL")
  unknownEndAs <- match.arg(unknownEndAs)
  vStart <- paste0("union_start_cmc", u); vType <- paste0("union_start_type", u)
  vEnd   <- paste0("union_end_cmc", u);   vMot  <- paste0("union_end_motive", u)
  vMarr  <- paste0("marriage_start_cmc", u)
  need <- c(vStart, vType, vEnd, vMot, vMarr, varCens, varWeight)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("buildUnionEpisodes: missing column(s): ", paste(miss, collapse = ", "))

  n0    <- nrow(df)
  start <- as.numeric(df[[vStart]]);  cens <- as.numeric(df[[varCens]])
  endD  <- as.numeric(df[[vEnd]]);    marr <- as.numeric(df[[vMarr]])
  type  <- as.character(df[[vType]]); mot  <- as.character(df[[vMot]])
  w     <- as.numeric(df[[varWeight]])
  id    <- if (is.null(idVar)) seq_len(n0) else df[[idVar]]

  origin <- ifelse(type %in% c("cohabitation", "cohabitation before marriage"), "cohabitation",
            ifelse(type == "marriage", "direct marriage", NA_character_))

  ok <- !is.na(start) & !is.na(cens) & (cens > start) & !is.na(origin) & is.finite(w) & (w > 0)
  nNoType <- sum(is.na(origin) & !is.na(start))
  nBadDate <- sum(!is.na(origin) & (is.na(start) | is.na(cens) | (cens <= start)))

  # A union that began as cohabitation but whose marriage is dated in the very
  # month it started is a direct marriage in all but the label.
  tie <- ok & (origin == "cohabitation") & !is.na(marr) & (marr == start)
  origin[tie] <- "direct marriage"

  # End of the union, never after the survey
  ended  <- ok & !is.na(endD) & (endD > start) & (endD <= cens)
  endObs <- ifelse(ended, endD, cens)

  motKey  <- tolower(trimws(as.character(mot)))
  motWhat <- unname(UNION_MOTIVE_MAP[motKey])
  nUnmapped <- sum(ended & !is.na(motKey) & is.na(motWhat))
  nInUnion  <- sum(ended & (motKey == "in union"), na.rm = TRUE)
  motWhat[is.na(motWhat)] <- "unknown"
  if (unknownEndAs == "separation") motWhat[motWhat == "unknown"] <- "separation"

  endTo <- ifelse(!ended, "censor",
           ifelse(motWhat == "separation", "separation",
           ifelse(motWhat == "widowed", "widowed",
                  if (unknownEndAs == "censor") "censor" else "ended (unknown reason)")))
  nOddMot <- sum(ended & (motWhat == "unknown"), na.rm = TRUE)

  conv <- ok & (origin == "cohabitation") & !is.na(marr) & (marr > start) & (marr <= endObs)
  nNoMarrDate <- sum(ok & (type == "cohabitation before marriage") & is.na(marr))

  ageU <- if (is.null(varDob)) NA_real_ else (start - as.numeric(df[[varDob]])) / 12
  ageM <- if (is.null(varDob)) NA_real_ else
    ifelse(conv | (origin == "direct marriage"),
           (ifelse(origin == "direct marriage", start, marr) - as.numeric(df[[varDob]])) / 12, NA_real_)
  cDur <- ifelse(origin == "direct marriage", 0,
          ifelse(conv, (marr - start) / 12, NA_real_))
  yU   <- 1900 + trunc((start - 1) / 12)

  base <- function (idx, ts, tp, st, to) {
    # rep(..., length.out=) so a group with no members yields 0 rows rather
    # than a "differing number of rows" error from the scalar tstart.
    n <- length(idx)
    ts <- rep(ts, length.out = n); tp <- rep(tp, length.out = n)
    st <- rep(st, length.out = n); to <- rep(to, length.out = n)
    d <- data.frame(id = id[idx], tstart = ts, tstop = tp,
                    istate = st, to = to, origin = origin[idx],
                    ageUnion = if (length(ageU) == 1) NA_real_ else ageU[idx],
                    ageMarriage = if (length(ageM) == 1) NA_real_ else ageM[idx],
                    cohabDur = cDur[idx], yUnion = yU[idx], w = w[idx],
                    stringsAsFactors = FALSE)
    for (cc in intersect(carry, names(df))) d[[cc]] <- df[[cc]][idx]
    d
  }
  sepFrom <- function (to, where) ifelse(to == "separation", paste0("separation (", where, ")"), to)

  iD <- which(ok & (origin == "direct marriage"))
  iC <- which(ok & (origin == "cohabitation") & !conv)
  iV <- which(conv)

  ep <- rbind(
    base(iD, 0, endObs[iD] - start[iD], "married direct",    sepFrom(endTo[iD], "married")),
    base(iC, 0, endObs[iC] - start[iC], "cohabiting",        sepFrom(endTo[iC], "cohabiting")),
    base(iV, 0, marr[iV] - start[iV],   "cohabiting",        "married converted"),
    base(iV, marr[iV] - start[iV], endObs[iV] - start[iV],
                                        "married converted", sepFrom(endTo[iV], "married")))

  ep <- ep[is.finite(ep$tstart) & is.finite(ep$tstop) & (ep$tstop > ep$tstart), ]
  ep$istate <- factor(ep$istate, levels = UNION_ISTATES)
  ep$to     <- factor(ep$to,     levels = UNION_TO)
  ep <- ep[order(ep$id, ep$tstart), ]
  rownames(ep) <- NULL

  if (!quiet) {
    message(sprintf(paste0("buildUnionEpisodes(u = %d): %d unions in, %d kept, %d episodes.\n",
                           "  %d direct marriages, %d cohabitations of which %d converted.\n",
                           "  dropped: %d without a usable union type, %d with unusable dates.\n",
                           "  %d 'cohabitation before marriage' without a marriage date (kept as never converted).\n",
                           "  %d ended for a reason that is neither separation nor widowhood (handled as '%s'),\n",
                           "    of which %d are labelled 'in union' despite having an end date, and %d carry a wording\n",
                           "    not in UNION_MOTIVE_MAP.\n",
                           "  %d marriages dated in the union's own first month, counted as direct."),
                    u, n0, sum(ok), nrow(ep), length(iD), length(iC) + length(iV), length(iV),
                    nNoType, nBadDate, nNoMarrDate, nOddMot, unknownEndAs,
                    nInUnion, nUnmapped, sum(tie)))
  }
  return (ep)
}


unionStateOccupancy <- function (ep = NULL, by = NULL, maxYears = NULL) {
  #==> ep: output of buildUnionEpisodes()
  #==> by: a column to stratify on, for instance "origin" or "country"
  #<== tidy data.frame(timeYear, state, p, <by>) ready for geom_area()
  if (!requireNamespace("survival", quietly = TRUE)) stop("needs 'survival'")
  # >>> Claude 2026-09-21
  # 'by' may name several columns, for instance c("country", "cohort"): they are
  # pasted into one key here and split apart again below. Note that every id must
  # sit in a single stratum, so ids have to be distinct ACROSS countries.
  byCols <- by
  if (!is.null(by) && (length(by) > 1)) {
    miss <- setdiff(by, names(ep))
    if (length(miss) > 0) stop("unionStateOccupancy: missing column(s): ", paste(miss, collapse = ", "))
    ep$.key <- do.call(paste, c(lapply(by, function (b) as.character(ep[[b]])), sep = "\r"))
    by <- ".key"
  }
  # <<< Claude 2026-09-21
  fml <- stats::as.formula(if (is.null(by)) "survival::Surv(tstart, tstop, to) ~ 1"
                           else sprintf("survival::Surv(tstart, tstop, to) ~ %s", by))
  f <- survival::survfit(fml, data = ep, id = id, istate = istate, weights = w)
  grp <- if (is.null(f$strata)) rep("all", length(f$time))
         else rep(sub("^[^=]*=", "", names(f$strata)), f$strata)
  out <- data.frame(timeYear = rep(f$time, length(f$states)) / 12,
                    state    = rep(f$states, each = length(f$time)),
                    p        = as.vector(f$pstate),
                    # >>> Claude 2026-09-22
                    # survfit returns pointwise bounds for pstate on a multistate
                    # fit as well. They are returned here so the curves can be
                    # drawn with a ribbon like every other figure in the project.
                    lower    = if (is.null(f$lower)) NA_real_ else as.vector(f$lower),
                    upper    = if (is.null(f$upper)) NA_real_ else as.vector(f$upper),
                    # <<< Claude 2026-09-22
                    group    = rep(grp, length(f$states)),
                    stringsAsFactors = FALSE)
  # >>> Claude 2026-09-21
  if (!is.null(byCols) && (length(byCols) > 1)) {
    parts <- do.call(rbind, strsplit(out$group, "\r", fixed = TRUE))
    colnames(parts) <- byCols
    out <- cbind(out, as.data.frame(parts, stringsAsFactors = FALSE))
    out$group <- NULL
  }
  # <<< Claude 2026-09-21
  if (!is.null(maxYears)) out <- out[out$timeYear <= maxYears, ]
  attr(out, "fit") <- f
  return (out)
}


unionCompetingRisks <- function (ep = NULL, from = "cohabiting", by = NULL,
                                 horizonYears = c(5, 10)) {
  #==> from: which starting state to follow, "cohabiting" or "married direct"
  #==> horizonYears: where to read the cumulative incidences off
  #<== list(curves = tidy CIFs over time, table = the values at each horizon)
  if (!requireNamespace("survival", quietly = TRUE)) stop("needs 'survival'")
  first <- ep[(ep$tstart == 0) & (ep$istate == from), ]
  if (nrow(first) == 0) stop("unionCompetingRisks: no spells start in state '", from, "'")
  first$to <- droplevels(first$to)
  # >>> Claude 2026-09-21
  # 'by' may now name SEVERAL columns, for instance c("country", "cohort").
  # They are pasted into one stratifying key and split apart again below, which
  # keeps the survfit formula a single term and the strata names parsable.
  byCols <- by
  if (!is.null(by) && (length(by) > 1)) {
    miss <- setdiff(by, names(first))
    if (length(miss) > 0) stop("unionCompetingRisks: missing column(s): ", paste(miss, collapse = ", "))
    first$.key <- do.call(paste, c(lapply(by, function (b) as.character(first[[b]])), sep = "\r"))
    by <- ".key"
  }
  # <<< Claude 2026-09-21
  fml <- stats::as.formula(if (is.null(by)) "survival::Surv(tstop, to) ~ 1"
                           else sprintf("survival::Surv(tstop, to) ~ %s", by))
  f <- survival::survfit(fml, data = first, weights = w)
  grp <- if (is.null(f$strata)) rep("all", length(f$time))
         else rep(sub("^[^=]*=", "", names(f$strata)), f$strata)
  curves <- data.frame(timeYear = rep(f$time, length(f$states)) / 12,
                       state    = rep(f$states, each = length(f$time)),
                       p        = as.vector(f$pstate),
                       # >>> Claude 2026-09-22
                       lower    = if (is.null(f$lower)) NA_real_ else as.vector(f$lower),
                       upper    = if (is.null(f$upper)) NA_real_ else as.vector(f$upper),
                       # <<< Claude 2026-09-22
                       group    = rep(grp, length(f$states)),
                       stringsAsFactors = FALSE)
  sm <- summary(f, times = horizonYears * 12, extend = TRUE)
  ps <- matrix(as.numeric(sm$pstate), ncol = length(f$states),
               dimnames = list(NULL, f$states))
  tab <- data.frame(horizonYears = sm$time / 12,
                    group = if (is.null(f$strata)) "all" else sub("^[^=]*=", "", as.character(sm$strata)),
                    round(as.data.frame(ps), 4), check.names = FALSE)
  # >>> Claude 2026-09-21
  # Put the pasted key back into one column per 'by' variable.
  if (!is.null(byCols) && (length(byCols) > 1)) {
    splitKey <- function (d) {
      parts <- do.call(rbind, strsplit(d$group, "\r", fixed = TRUE))
      colnames(parts) <- byCols
      d <- cbind(d, as.data.frame(parts, stringsAsFactors = FALSE))
      d$group <- NULL
      d
    }
    curves <- splitKey(curves)
    tab    <- splitKey(tab)
  }
  # <<< Claude 2026-09-21
  attr(curves, "fit") <- f
  return (list(curves = curves, table = tab))
}
# <<< Claude 2026-09-21
