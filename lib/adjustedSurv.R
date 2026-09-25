# >>> Claude 2026-09-21
# adjustedSurv(): survival curves for two or more groups, standardised to a
# common covariate distribution, so the curves can be compared directly.
#
# METHOD. Fit a Cox model with the group and the covariates. Then, for each
# group in turn, force the group variable to that value FOR THE WHOLE SAMPLE,
# predict, and average. Every curve is therefore standardised to the same
# covariate distribution (the observed one), so what is left between them is
# the group contrast and not composition. This is direct standardisation, also
# called the g-formula.
#
#     S_g(t) = weighted mean over everyone of exp( -H0(t) * exp(lp_i | group = g) )
#
# The obvious route, survfit(fit, newdata = wholeSample), builds one survival
# curve per respondent and falls over above a few thousand rows. This computes
# the same thing from the baseline hazard instead.
#
# READ THE WARNING IN adjustedSurv_note() BEFORE USING IT ON
# direct-versus-converted MARRIAGES. Holding age at union AND age at marriage
# fixed is the same as holding the cohabitation duration fixed, and the only
# duration a direct marriage has is zero.

adjustedSurv <- function (df = NULL, timeVar = "t", statusVar = "sep", groupVar = "route",
                          covariates = NULL, weightVar = NULL,
                          times = NULL, nTimes = 250, maxTime = NULL) {
  #==> df: one row per spell
  #==> timeVar / statusVar: duration and a 0/1 event indicator
  #==> groupVar: the factor whose levels are compared. Its first level is the reference
  #==> covariates: character vector of terms for the right-hand side, e.g.
  #    c("ageUnion", "ageMarriage") or c("ns(ageUnion, 3)", "ns(cohabDur, 3)")
  #==> weightVar: survey weight column, or NULL
  #==> times / nTimes / maxTime: the grid the curves are evaluated on
  #<== list(curves, hr, model, formula)

  if (!requireNamespace("survival", quietly = TRUE)) stop("adjustedSurv needs 'survival'")
  if (is.null(df)) stop("adjustedSurv: df cannot be NULL")
  need <- c(timeVar, statusVar, groupVar, weightVar)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("adjustedSurv: missing column(s): ", paste(miss, collapse = ", "))

  g <- df[[groupVar]]
  if (!is.factor(g)) g <- factor(g)
  if (nlevels(g) < 2) stop("adjustedSurv: groupVar needs at least two levels")
  df[[groupVar]] <- g

  w <- if (is.null(weightVar)) rep(1, nrow(df)) else as.numeric(df[[weightVar]])
  if (any(!is.finite(w)) || any(w <= 0)) stop("adjustedSurv: weights must be finite and positive")

  rhs <- paste(c(groupVar, covariates), collapse = " + ")
  fml <- stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s", timeVar, statusVar, rhs))
  fit <- survival::coxph(fml, data = df, weights = w)

  if (is.null(times)) {
    hi <- if (is.null(maxTime)) stats::quantile(df[[timeVar]], 0.95, na.rm = TRUE) else maxTime
    times <- seq(0, hi, length.out = nTimes)
  }
  bh <- survival::basehaz(fit, centered = FALSE)
  # basehaz() starts at the first event time, so interpolating below it would
  # extrapolate the first hazard backwards and the curves would not start at 1.
  # Anchor the cumulative hazard at (0, 0).
  bh <- rbind(data.frame(hazard = 0, time = 0), bh[, c("hazard", "time")])
  bh <- bh[order(bh$time), ]
  H0 <- stats::approx(bh$time, bh$hazard, xout = times,
                      method = "constant", f = 0, rule = 2, ties = "ordered")$y

  curves <- do.call(rbind, lapply(levels(g), function (lv) {
    nd <- df
    nd[[groupVar]] <- factor(lv, levels = levels(g))
    risk <- stats::predict(fit, newdata = nd, type = "risk", reference = "zero")
    data.frame(time  = times,
               surv  = vapply(H0, function (h) stats::weighted.mean(exp(-h * risk), w), numeric(1)),
               group = lv, stringsAsFactors = FALSE)
  }))
  curves$group <- factor(curves$group, levels = levels(g))

  cf <- stats::coef(fit)
  hr <- exp(cf[grepl(paste0("^", groupVar), names(cf))])

  return (list(curves = curves, hr = hr, model = fit, formula = fml))
}


adjustedSurv_note <- function () {
  cat(paste0(
  "WHEN THE GROUPS ARE DIRECT vs CONVERTED MARRIAGES\n\n",
  "For a direct marriage, age at union and age at marriage are the SAME number:\n",
  "moving in together and marrying are one event. So\n\n",
  "    age at marriage - age at union = cohabitation duration\n\n",
  "which is exactly 0 for every direct marriage and positive for every converted\n",
  "one. Controlling BOTH ages is therefore algebraically the same as controlling\n",
  "the cohabitation duration: 'route + ageUnion + ageMarriage' and\n",
  "'route + ageUnion + cohabDur' are the same model and give the same coefficient.\n\n",
  "That means the group contrast asks: how do a direct marriage and a converted\n",
  "marriage differ AT THE SAME COHABITATION DURATION? The only duration a direct\n",
  "marriage has is zero, and no converted marriage has zero. The estimate is\n",
  "obtained by extrapolating the duration effect down to zero.\n\n",
  "In simulation, when the duration effect is linear the estimate is exact. When\n",
  "it saturates (the first months matter more than later ones, which is likely),\n",
  "a linear specification returned 0.49 against a truth of 0.80.\n\n",
  "SO: fit it as 'route + ageUnion + cohabDur', plot the fitted duration effect,\n",
  "and if it is steep near zero report a version restricted to short\n",
  "cohabitations as well. The restricted estimate is local but needs no\n",
  "extrapolation.\n"))
}
# <<< Claude 2026-09-21
