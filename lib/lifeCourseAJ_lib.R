# >>> Claude 2026-09-24
# Life course of women from age 15 to 45 in one multistate model: single,
# first union (by cohabitation or by direct marriage), conversion of the
# cohabitation into a marriage, and the end of the first union by separation
# or widowhood. Estimated with the Aalen-Johansen estimator on the AGE scale,
# so single women and women in union share one clock.
#
# Unlike unionStateOccupancy(), whose clock is the duration of the union and
# whose population is the unions, the population here is ALL women, and the
# share in each state at age a is P(state at age a). The bands sum to 1.
#
# Only the first union is followed: separation and widowhood are absorbing.
# A union that ends for a reason that is neither separation nor widowhood is
# counted as a separation (as in presentation_KM_AJ.R).
#
# TIME UNIT: months since age 15 (CMC arithmetic), ages in years on output.

# ==== 1. States ====

LIFE_STATES <- c("single", "cohabiting", "married (after cohabitation)", "married (direct)",
                 "separated (from cohabitation)", "separated (after cohabitation and marriage)",
                 "separated (from direct marriage)", "widowed")


# ==== 2. Episodes on the age scale ====

buildLifeEpisodes <- function (df = NULL, u = 1, ageFrom = 15, ageTo = 45,
                               varDob = "indiv_dob_cmc", varCens = "surveyDate_cmc",
                               varWeight = "popWeight", carry = c("country", "survey"),
                               quiet = FALSE) {
  #==> df: MEXICO_ENADID, NSFG_ENADID or a subset of either
  #==> u: union slot followed (1 = first union)
  #==> ageFrom, ageTo: observation window in years of age
  #<== one row per woman-state spell, tstart / tstop in months since ageFrom,
  #    istate, to (a LIFE_STATES value or "censor"), w, and the carry columns

  if (is.null(df)) stop("buildLifeEpisodes: df cannot be NULL")
  vStart <- paste0("union_start_cmc", u); vType <- paste0("union_start_type", u)
  vEnd   <- paste0("union_end_cmc", u);   vMot  <- paste0("union_end_motive", u)
  vMarr  <- paste0("marriage_start_cmc", u)
  need <- c(vStart, vType, vEnd, vMot, vMarr, varDob, varCens, varWeight)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("buildLifeEpisodes: missing column(s): ", paste(miss, collapse = ", "))

  n0   <- nrow(df)
  dob  <- as.numeric(df[[varDob]]);  cens <- as.numeric(df[[varCens]])
  U    <- as.numeric(df[[vStart]]);  E    <- as.numeric(df[[vEnd]])
  M    <- as.numeric(df[[vMarr]]);   w    <- as.numeric(df[[varWeight]])
  type <- as.character(df[[vType]]); mot  <- tolower(trimws(as.character(df[[vMot]])))

  # observation window of each woman, in CMC
  entry <- dob + 12 * ageFrom
  exitW <- pmin(cens, dob + 12 * ageTo)

  origin <- ifelse(type %in% c("cohabitation", "cohabitation before marriage"), "cohabitation",
            ifelse(type == "marriage", "direct marriage", NA_character_))
  hasU   <- !is.na(U) & (U < 9900)

  okW   <- !is.na(dob) & (dob < 9900) & !is.na(cens) & (exitW > entry) & is.finite(w) & (w > 0)
  badU  <- okW & hasU & (is.na(origin) | (U > cens) | (U < dob))
  ok    <- okW & !badU
  hasU  <- ok & hasU

  # a cohabitation married in its own first month is a direct marriage
  tie <- hasU & (origin == "cohabitation") & !is.na(M) & (M == U)
  origin[tie] <- "direct marriage"

  ended  <- hasU & !is.na(E) & (E < 9900) & (E > U) & (E <= cens)
  endObs <- ifelse(ended, E, cens)
  conv   <- hasU & (origin == "cohabitation") & !is.na(M) & (M > U) & (M <= endObs)
  wid    <- ended & (mot %in% c("widowhood", "widowed"))

  # the state entered when the union ends, from the state the union is in
  sepState <- ifelse(origin == "direct marriage", LIFE_STATES[7],
              ifelse(conv, LIFE_STATES[6], LIFE_STATES[5]))
  endTo <- ifelse(!ended, "censor", ifelse(wid, LIFE_STATES[8], sepState))

  # spells on the calendar, before clipping to the window
  sp <- function (idx, ts, tp, st, to) {
    n <- length(idx)
    data.frame(i = idx, ts = rep(ts, length.out = n), tp = rep(tp, length.out = n),
               st = rep(st, length.out = n), to = rep(to, length.out = n),
               stringsAsFactors = FALSE)
  }
  iN <- which(ok & !hasU)
  iU <- which(hasU)
  iD <- which(hasU & (origin == "direct marriage"))
  iC <- which(hasU & (origin == "cohabitation") & !conv)
  iV <- which(conv)
  firstUnion <- ifelse(origin == "direct marriage", LIFE_STATES[4], LIFE_STATES[2])
  cal <- rbind(
    sp(iN, -Inf, cens[iN], LIFE_STATES[1], "censor"),
    sp(iU, -Inf, U[iU],    LIFE_STATES[1], firstUnion[iU]),
    sp(iD, U[iD], endObs[iD], LIFE_STATES[4], endTo[iD]),
    sp(iC, U[iC], endObs[iC], LIFE_STATES[2], endTo[iC]),
    sp(iV, U[iV], M[iV],      LIFE_STATES[2], LIFE_STATES[3]),
    sp(iV, M[iV], endObs[iV], LIFE_STATES[3], endTo[iV]))
  # women whose union ended before the window opens start in the end state
  iA <- which(ended & (E <= entry))
  cal <- rbind(cal, sp(iA, E[iA], Inf, endTo[iA], "censor"))

  # clip to [entry, exitW)
  cal$a0 <- entry[cal$i]; cal$a1 <- exitW[cal$i]
  cal$to[cal$tp > cal$a1] <- "censor"
  cal$ts <- pmax(cal$ts, cal$a0); cal$tp <- pmin(cal$tp, cal$a1)
  cal <- cal[cal$tp > cal$ts, ]

  ep <- data.frame(id = cal$i, tstart = cal$ts - cal$a0, tstop = cal$tp - cal$a0,
                   istate = factor(cal$st, levels = LIFE_STATES),
                   to = factor(cal$to, levels = c("censor", LIFE_STATES)),
                   w = w[cal$i], stringsAsFactors = FALSE)
  for (cc in intersect(carry, names(df))) ep[[cc]] <- df[[cc]][cal$i]
  ep <- ep[order(ep$id, ep$tstart), ]
  rownames(ep) <- NULL

  if (!quiet) {
    message(sprintf(paste0("buildLifeEpisodes(u = %d, ages %g-%g): %d women in, %d kept, %d spells.\n",
                           "  dropped: %d with no usable birth or survey date or no time in the window,\n",
                           "           %d with a union of unknown type or impossible start date.\n",
                           "  kept: %d never in union, %d direct marriages, %d cohabitations (%d converted),\n",
                           "        %d unions ended (%d by widowhood), %d of them before age %g."),
                    u, ageFrom, ageTo, n0, sum(ok), nrow(ep), sum(!okW), sum(badU),
                    length(iN), length(iD), length(iC) + length(iV), length(iV),
                    sum(ended), sum(wid), length(iA), ageFrom))
  }
  return (ep)
}



# ==== 3. Aalen-Johansen state occupancy ====

lifeStateOccupancy <- function (ep = NULL, by = NULL, ageFrom = 15) {
  #==> ep: output of buildLifeEpisodes()
  #==> by: a column to stratify on, for instance "country"
  #<== tidy data.frame(age, state, p, group) ready for geom_area()
  if (!requireNamespace("survival", quietly = TRUE)) stop("needs 'survival'")
  fml <- stats::as.formula(if (is.null(by)) "survival::Surv(tstart, tstop, to) ~ 1"
                           else sprintf("survival::Surv(tstart, tstop, to) ~ %s", by))
  # No standard errors: they are not drawn, and on half a million women the
  # influence matrix of an eight-state fit would take a long time.
  f <- survival::survfit(fml, data = ep, id = id, istate = istate, weights = w,
                         se.fit = FALSE)
  grp  <- if (is.null(f$strata)) rep("all", length(f$time))
          else rep(sub("^[^=]*=", "", names(f$strata)), f$strata)
  ps   <- f$pstate
  # add the starting distribution p0 at age ageFrom for every stratum
  p0   <- if (is.null(dim(f$p0))) matrix(f$p0, nrow = 1) else f$p0
  g0   <- if (is.null(f$strata)) "all" else sub("^[^=]*=", "", names(f$strata))
  out  <- rbind(
    data.frame(age = ageFrom, state = rep(f$states, each = length(g0)),
               p = as.vector(p0), group = rep(g0, length(f$states)), stringsAsFactors = FALSE),
    data.frame(age = ageFrom + rep(f$time, length(f$states)) / 12,
               state = rep(f$states, each = length(f$time)),
               p = as.vector(ps), group = rep(grp, length(f$states)), stringsAsFactors = FALSE))
  out <- out[out$state %in% LIFE_STATES, ]
  out$state <- factor(out$state, levels = LIFE_STATES)
  out <- out[order(out$group, out$state, out$age), ]
  attr(out, "fit") <- f
  return (out)
}


# ==== 4. Stacked area plot ====

# stack order, top to bottom: the single state on top feeds the unions below it;
# each union state sits directly above the separations that come out of it
LIFE_ORDER <- c("single", "cohabiting", "separated (from cohabitation)",
                "married (after cohabitation)", "separated (after cohabitation and marriage)",
                "married (direct)", "separated (from direct marriage)", "widowed")
LIFE_COLOURS <- c("single" = "#cfcfcf",
                  "cohabiting" = "#1b9e77", "separated (from cohabitation)" = "#9fd8c3",
                  "married (after cohabitation)" = "#2f67b1",
                  "separated (after cohabitation and marriage)" = "#a3c0ea",
                  "married (direct)" = "#c2477f", "separated (from direct marriage)" = "#f0b0cb",
                  "widowed" = "#8c6d46")
LIFE_SHORT <- c("Single", "Cohabiting", "Separated\n(cohabitation)", "Married after\ncohabitation",
                "Separated (after\ncohab. + marriage)", "Married\ndirectly",
                "Separated\n(direct marriage)", "Widowed")
names(LIFE_SHORT) <- LIFE_ORDER
# legend in two rows: each union state above the separation that comes out of it
LIFE_LEGEND <- c("single", "cohabiting", "married (after cohabitation)", "married (direct)",
                 "widowed", "separated (from cohabitation)",
                 "separated (after cohabitation and marriage)", "separated (from direct marriage)")
LIFE_LEGEND_LABELS <- c("Single", "Cohabiting", "Married after cohabitation", "Married directly",
                        "Widowed", "Separated from cohabitation",
                        "Separated after cohabitation and marriage", "Separated from direct marriage")

lifeCoursePlot <- function (occ, Title, labelAge = 40, minBand = 0.045) {
  occ$state <- factor(as.character(occ$state), levels = LIFE_ORDER)
  occ <- occ[occ$age <= 45, ]
  # labels in the middle of each band at labelAge, only where the band is thick
  lab <- occ %>% group_by(group, state) %>%
    summarise(p = p[findInterval(labelAge, age)], .groups = "drop") %>%
    arrange(group, desc(state)) %>% group_by(group) %>%
    mutate(y = cumsum(p) - p / 2) %>% ungroup() %>% filter(p >= minBand)
  lab$label <- LIFE_SHORT[as.character(lab$state)]
  lab$ink   <- ifelse(lab$state %in% c("cohabiting", "married (after cohabitation)",
                                       "married (direct)", "widowed"), "white", "grey15")
  ggplot(occ, aes(x = age, y = p, fill = state)) +
    geom_area(colour = "white", linewidth = 0.25) +
    geom_text(data = lab, aes(x = labelAge, y = y, label = label, colour = ink),
              size = 3.1, lineheight = 0.9, inherit.aes = FALSE) +
    scale_colour_identity() +
    facet_grid(~ group) +
    scale_fill_manual(values = LIFE_COLOURS, breaks = LIFE_LEGEND, labels = LIFE_LEGEND_LABELS,
                      name = NULL) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(15, 45, 5)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    labs(title = Title, x = "Age of the woman (years)", y = "Share of all women") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_linedraw() + theme(legend.position = "bottom", panel.grid = element_blank(),
                             panel.spacing = unit(1.2, "lines"),
                             legend.text = element_text(size = 9))
}
# <<< Claude 2026-09-24
