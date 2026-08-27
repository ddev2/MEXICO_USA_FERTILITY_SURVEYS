# NSFG 2002 dissolution-date imputation -- model + imputation (Step 2)
#
# Estimates ONE union-dissolution timing model on the 2006-10 cycle (which has
# complete dissolution dates) and uses it to impute the union-end dates the 2002
# questionnaire skipped, for BOTH gaps:
#   issue 1  recently-separated women, current union reads as open (sep_recent_*)
#   issue 2  marriages whose husband had prior children (union_husb_priorKids*)
#
# WHY ONE MODEL (not two):
#   Both gaps need the same quantity -- the time from union start to dissolution
#   (separation/divorce). That process is estimated once, on all 2006-10 union
#   spells, so the smaller gap borrows strength from the larger sample. The two
#   cases differ only at DRAW time, in (a) the truncation bounds and (b) the
#   covariate values of each target union -- not in the underlying hazard. The
#   one substantive split that DOES matter, marriage vs cohabitation dissolution,
#   is carried as a covariate (unionType), so a single model spans both gaps.
#
# Workflow (run AFTER NSFG_ENADID is built and the flags are added):
#   source("NSFG import.R")
#   source("NSFG_impute_dissolution.R")     # add_union_sep_recent / _husb_priorKids
#   source("NSFG_impute_dissolution_model.R")
#   train <- build_dissolution_training(NSFG_ENADID)
#   fit   <- fit_dissolution_model(train, dist = "loglogistic")
#   set.seed(1)
#   NSFG_ENADID <- impute_sep_recent(NSFG_ENADID, fit)
#   NSFG_ENADID <- impute_husb_priorKids(NSFG_ENADID, fit)
#
# Depends on: survival (survreg), MASS (mvrnorm, only when propagate = TRUE).

library(survival)

# union_end_cmc_I codes for model-imputed dissolution dates. The sub-digit marks
# the imputation SOURCE (not date precision, unlike 30-33): 41 = separated women
# (issue 1), 42 = husband-prior-kids marriages (issue 2). 40 is a reserved generic
# that should never appear. Distinct from 1 (imputed month), 10/11 (missing year /
# month+year), 20 (cleanENADID back-fill / refused), and 30-33 (divorce date).
IMPUTED_CODE           <- 40L   # reserved / unattributed (should never appear)
IMPUTED_CODE_SEP       <- 41L   # issue 1: recently-separated women
IMPUTED_CODE_PRIORKIDS <- 42L   # issue 2: husband had prior children

# Default parametric family. log-logistic gives a non-monotonic hazard, which
# fits divorce/separation timing better than Weibull's monotone hazard.
DEFAULT_DIST <- "loglogistic"


# ==== 1. Union-level covariates (shared by training and imputation) ====

# Identical covariate construction MUST be used when fitting and when drawing,
# so factor levels are fixed here rather than inferred from the data.
union_model_covariates <- function(start_cmc, dob_cmc, is_marriage, union_order,
                                   prior_kids = FALSE) {
  age <- (start_cmc - dob_cmc) / 12
  data.frame(
    ageStart   = age,
    ageStart2  = age^2,
    startYear  = 1900 + (start_cmc - 1) / 12,
    unionType  = factor(ifelse(is_marriage, "marriage", "cohab"),
                        levels = c("cohab", "marriage")),
    unionOrder = factor(ifelse(union_order >= 3L, "3+", as.character(union_order)),
                        levels = c("1", "2", "3+")),
    priorKids  = factor(ifelse(!is.na(prior_kids) & prior_kids, "yes", "no"),
                        levels = c("no", "yes"))
  )
}

# Chronological order of the union holding reference start `start_ref`, counted
# as the number of the woman's occupied unions that began no later than it.
union_order_at <- function(df, idx, start_ref, max_slots = 10L) {
  us  <- paste0("union_start_cmc", 1:max_slots)
  us  <- us[us %in% names(df)]
  ord <- integer(length(idx))
  for (v in us) {
    st  <- suppressWarnings(as.numeric(df[[v]][idx]))
    ord <- ord + (!is.na(st) & st < 9000 & !is.na(start_ref) & st <= start_ref)
  }
  pmax(ord, 1L)
}


# ==== 2. Build 2006-10 training data ====

# One row per observed union spell. event = 1 iff the union ended by separation
# (the harmonised motive collapses divorce into "separation"); unions still
# intact ("in union") are right-censored at the survey, and widowhood-ended
# unions are censored at the death date (cause-specific dissolution hazard).
build_dissolution_training <- function(df, survey = "NSFG2006_10",
                                       max_slots = 10L,
                                       drop_unknown_motive = TRUE) {
  d <- df[df$survey == survey, , drop = FALSE]
  if (nrow(d) == 0L) stop("no rows for survey ", survey)

  slots <- which(paste0("union_start_cmc", 1:max_slots) %in% names(d))
  rows  <- list()

  for (s in slots) {
    start  <- suppressWarnings(as.numeric(d[[paste0("union_start_cmc", s)]]))
    endcol <- paste0("union_end_cmc", s)
    end    <- if (endcol %in% names(d)) suppressWarnings(as.numeric(d[[endcol]])) else NA_real_
    mot    <- as.character(d[[paste0("union_end_motive", s)]])
    mstart <- suppressWarnings(as.numeric(d[[paste0("marriage_start_cmc", s)]]))

    occ <- !is.na(start) & start < 9000

    # chronological order of this slot among the woman's occupied unions
    ord <- integer(nrow(d))
    for (s2 in slots) {
      st2 <- suppressWarnings(as.numeric(d[[paste0("union_start_cmc", s2)]]))
      ord <- ord + (!is.na(st2) & st2 < 9000 & !is.na(start) & st2 <= start)
    }

    # event time: separation/widowhood at observed end, otherwise survey
    tend  <- d$surveyDate_cmc
    tend  <- ifelse(mot %in% c("separation", "widowhood") & !is.na(end), end, tend)
    event <- as.integer(mot == "separation" & !is.na(end) & end < 9000)
    dur   <- tend - start

    keep <- occ & !is.na(dur) & dur >= 1 & (is.na(end) | end < 9000)
    if (drop_unknown_motive)
      keep <- keep & mot %in% c("in union", "separation", "widowhood")
    keep <- keep & !(mot == "separation" & (is.na(end) | end >= 9000))
    endI <- if (paste0("union_end_cmc_I", s) %in% names(d))
              suppressWarnings(as.numeric(d[[paste0("union_end_cmc_I", s)]])) else rep(NA_real_, nrow(d))
    keep <- keep & !(endI %in% c(20L, 21L, 22L, 40L, 41L, 42L))   # drop back-fill placeholders / model-imputed ends from the donor sample
    if (!any(keep)) next

    pkcol <- paste0("union_husb_priorKids", s)
    pk    <- if (pkcol %in% names(d)) as.character(d[[pkcol]]) else rep(NA_character_, nrow(d))
    cov <- union_model_covariates(start[keep], d$indiv_dob_cmc[keep],
                                  is_marriage = !is.na(mstart[keep]) & mstart[keep] < 9000,
                                  union_order = ord[keep],
                                  prior_kids  = pk[keep] == "yes")
    rows[[length(rows) + 1L]] <- cbind(
      data.frame(
        dur    = dur[keep],
        event  = event[keep],
        weight = if ("indiv_weight" %in% names(d)) d$indiv_weight[keep] else 1
      ),
      cov)
  }

  out <- do.call(rbind, rows)
  out <- out[is.finite(out$ageStart) & out$ageStart >= 10 & out$ageStart <= 70, ]
  message(nrow(out), " union spells from ", survey, ": ",
          sum(out$event), " separations, ", nrow(out) - sum(out$event), " censored")
  out
}


# ==== 3. Fit the shared AFT dissolution model ====

fit_dissolution_model <- function(train, dist = DEFAULT_DIST,
                                  formula = Surv(dur, event) ~ ageStart + ageStart2 +
                                            startYear + unionType + unionOrder + priorKids,
                                  weights = TRUE) {
  if (isTRUE(weights) && "weight" %in% names(train)) {
    train <- train[is.finite(train$weight) & train$weight > 0, ]
    w     <- train$weight * nrow(train) / sum(train$weight)   # sum(w) = n: avoid survey-weight SE inflation
  } else {
    w <- NULL
  }
  fit <- survival::survreg(formula, data = train, dist = dist, weights = w)
  message("AFT (", dist, ") fitted on ", nrow(train), " spells; scale = ",
          signif(fit$scale, 4))
  fit
}


# ==== 4. Truncated end-date draw (core) ====

# AFT location-scale CDF / quantile of the survival time, given linear predictor
# lp (= location of log T) and scale sigma.
aft_cdf <- function(d, lp, scale, dist) {
  d <- pmax(d, .Machine$double.eps)
  switch(dist,
    weibull     = pweibull(d, shape = 1 / scale, scale = exp(lp)),
    lognormal   = plnorm(d, meanlog = lp, sdlog = scale),
    loglogistic = plogis((log(d) - lp) / scale),
    stop("unsupported dist: ", dist))
}

aft_qf <- function(u, lp, scale, dist) {
  switch(dist,
    weibull     = qweibull(u, shape = 1 / scale, scale = exp(lp)),
    lognormal   = qlnorm(u, meanlog = lp, sdlog = scale),
    loglogistic = exp(lp + scale * qlogis(u)),
    stop("unsupported dist: ", dist))
}

# Draw one union-end CMC per row from the model, truncated so the END falls in
# [lower_cmc, upper_cmc]. start_cmc is the duration clock origin (= union start);
# lower_cmc may differ (e.g. a marriage cannot end before marriage_start).
# propagate = TRUE redraws (beta, sigma) from their sampling distribution so a
# loop over seeds yields PROPER multiple imputation (parameter + sampling error).
draw_truncated_end <- function(fit, newdata, start_cmc, lower_cmc, upper_cmc,
                               dist = fit$dist, propagate = TRUE, min_dur = 1L) {
  stopifnot(nrow(newdata) == length(start_cmc))
  beta  <- coef(fit)
  scale <- fit$scale
  p     <- length(beta)

  if (isTRUE(propagate)) {
    if (!requireNamespace("MASS", quietly = TRUE))
      stop("propagate = TRUE needs install.packages('MASS')")
    V    <- vcov(fit)                       # [coef ... , Log(scale)]
    has_scale <- nrow(V) == p + 1L
    mu   <- if (has_scale) c(beta, log(scale)) else beta
    draw <- MASS::mvrnorm(1, mu, V)
    beta <- draw[seq_len(p)]
    if (has_scale) scale <- exp(draw[p + 1L])
  }

  X  <- model.matrix(delete.response(terms(fit)), data = newdata, xlev = fit$xlevels)
  X  <- X[, names(beta), drop = FALSE]       # guarantee column/coef alignment
  lp <- as.numeric(X %*% beta)

  dlo <- pmax(min_dur, lower_cmc - start_cmc)
  dhi <- upper_cmc - start_cmc
  ok  <- is.finite(lp) & is.finite(dlo) & is.finite(dhi) & dhi >= dlo

  end <- rep(NA_integer_, length(start_cmc))
  if (any(ok)) {
    Flo <- aft_cdf(dlo[ok], lp[ok], scale, dist)
    Fhi <- aft_cdf(dhi[ok], lp[ok], scale, dist)
    Fhi <- pmax(Fhi, Flo + 1e-9)             # guard a zero-width interval
    u   <- runif(sum(ok), Flo, Fhi)
    dur <- aft_qf(u, lp[ok], scale, dist)
    dur <- pmin(pmax(round(dur), dlo[ok]), dhi[ok])
    end[ok] <- as.integer(start_cmc[ok] + dur)
  }
  attr(end, "n_unbounded") <- sum(!ok)
  end
}

# Write imputed ends back into slot s (end + _I flag + motive "separation").
.write_union_end <- function(df, s, idx, end, code, set_separation = TRUE) {
  wr <- !is.na(end)
  if (!any(wr)) return(df)
  df[[paste0("union_end_cmc", s)]][idx[wr]] <- end[wr]
  ic <- paste0("union_end_cmc_I", s)
  if (ic %in% names(df)) df[[ic]][idx[wr]] <- code
  mc <- paste0("union_end_motive", s)
  if (set_separation && mc %in% names(df)) {
    lev <- union(levels(as.factor(df[[mc]])), "separation")
    m   <- as.character(df[[mc]]); m[idx[wr]] <- "separation"
    df[[mc]] <- factor(m, levels = lev)
  }
  df
}


# ==== 5. Impute issue 1: recently-separated women ====

# Target = the slot flagged by sep_recent_{s} (one per woman). The separation
# happened by the survey, so the end is bounded [union_start_cmc{s}, survey].
impute_sep_recent <- function(df, fit, dist = fit$dist, propagate = TRUE,
                              imputed_code = IMPUTED_CODE_SEP, max_slots = 10L,
                              survey = "NSFG2002") {
  flag_cols <- grep("^sep_recent_[0-9]+$", names(df), value = TRUE)
  if (length(flag_cols) == 0L) {
    warning("impute_sep_recent(): no sep_recent_* flags; run add_union_sep_recent()")
    return(df)
  }
  slots <- as.integer(sub("^sep_recent_", "", flag_cols))
  n_imp <- 0L

  for (s in slots) {
    fl  <- df[[paste0("sep_recent_", s)]]
    idx <- which(!is.na(fl) & fl & df$survey == survey)
    if (!length(idx)) next

    start   <- suppressWarnings(as.numeric(df[[paste0("union_start_cmc", s)]][idx]))
    upper   <- df$surveyDate_cmc[idx]
    is_marr <- {
      ms <- suppressWarnings(as.numeric(df[[paste0("marriage_start_cmc", s)]][idx]))
      !is.na(ms) & ms < 9000
    }
    pkcol <- paste0("union_husb_priorKids", s)
    pk    <- if (pkcol %in% names(df)) as.character(df[[pkcol]][idx]) else rep(NA_character_, length(idx))
    nd  <- union_model_covariates(start, df$indiv_dob_cmc[idx], is_marr,
                                  union_order_at(df, idx, start, max_slots),
                                  prior_kids = pk == "yes")
    end <- draw_truncated_end(fit, nd, start_cmc = start,
                              lower_cmc = start, upper_cmc = upper,
                              dist = dist, propagate = propagate)
    df    <- .write_union_end(df, s, idx, end, imputed_code)
    n_imp <- n_imp + sum(!is.na(end))
  }
  message("issue 1: imputed ", n_imp, " separation end dates")

  # Placeholder re-imputation: sep_recent women whose end was already back-filled
  # by cleanENADID to union_start_next - 1 (flag=21). The back-fill bound is a
  # correct upper bound but not the actual separation date. Re-draw within
  # [union_start, back-fill bound] — the same logic as husb_priorKids placeholder.
  ph_cols  <- grep("^sep_recent_placeholder_[0-9]+$", names(df), value = TRUE)
  ph_slots <- as.integer(sub("^sep_recent_placeholder_", "", ph_cols))
  n_ph <- 0L
  for (s in ph_slots) {
    fl  <- df[[paste0("sep_recent_placeholder_", s)]]
    idx <- which(!is.na(fl) & fl & df$survey == survey)
    if (!length(idx)) next

    start  <- suppressWarnings(as.numeric(df[[paste0("union_start_cmc",   s)]][idx]))
    upper  <- suppressWarnings(as.numeric(df[[paste0("union_end_cmc",     s)]][idx]))  # back-fill bound
    is_marr <- {
      ms <- suppressWarnings(as.numeric(df[[paste0("marriage_start_cmc", s)]][idx]))
      !is.na(ms) & ms < 9000
    }
    pkcol <- paste0("union_husb_priorKids", s)
    pk    <- if (pkcol %in% names(df)) as.character(df[[pkcol]][idx])
             else rep(NA_character_, length(idx))
    nd  <- union_model_covariates(start, df$indiv_dob_cmc[idx], is_marr,
                                  union_order_at(df, idx, start, max_slots),
                                  prior_kids = pk == "yes")
    end <- draw_truncated_end(fit, nd, start_cmc = start,
                              lower_cmc = start, upper_cmc = upper,
                              dist = dist, propagate = propagate)
    df    <- .write_union_end(df, s, idx, end, imputed_code)
    n_ph  <- n_ph + sum(!is.na(end))
  }
  if (length(ph_slots) > 0L)
    message("issue 1 placeholder: re-imputed ", n_ph,
            " back-filled separation end dates (flag 21 -> 41)")
  df
}


# ==== 6. Impute issue 2: husband had prior children ====

# Select, per marriage slot s, which prior-kids marriages need a date. NA-end
# marriages still mix genuinely-ended unions with currently-intact ones, so the
# split here is a JUDGEMENT CALL -- review the reported counts before trusting:
#   placeholder  end is a cleanENADID back-fill (_I == 21, end = next_start-1):
#                ended for sure, re-impute inside [marriage_start, end].
#   na_ended     end is NA AND there is a later union OR union_status indicates
#                dissolution -> ended last marriage, impute [marriage_start, survey].
#   intact       end is NA, no later union, still "married" -> ongoing, SKIP.
# Widowhood-ended marriages are excluded by default (not a dissolution event).
select_issue2_targets <- function(df, s, exclude_widowed = TRUE, max_slots = 10L) {
  fl  <- df[[paste0("union_husb_priorKids", s)]]
  yes <- !is.na(fl) & as.character(fl) == "yes"
  ms  <- suppressWarnings(as.numeric(df[[paste0("marriage_start_cmc", s)]]))
  is_marr <- !is.na(ms) & ms < 9000
  end <- suppressWarnings(as.numeric(df[[paste0("union_end_cmc", s)]]))
  iI  <- if (paste0("union_end_cmc_I", s) %in% names(df))
           suppressWarnings(as.numeric(df[[paste0("union_end_cmc_I", s)]]))
         else rep(NA_real_, nrow(df))

  has_later <- rep(FALSE, nrow(df))
  if (s < max_slots) for (s2 in (s + 1L):max_slots) {
    v <- paste0("union_start_cmc", s2)
    if (v %in% names(df)) {
      st2 <- suppressWarnings(as.numeric(df[[v]]))
      has_later <- has_later | (!is.na(st2) & st2 < 9000)
    }
  }
  ended_status <- df$union_status %in% c("divorced", "separated")
  if (!exclude_widowed) ended_status <- ended_status | df$union_status %in% "widowed"

  list(
    placeholder = which(yes & is_marr & !is.na(iI) & iI == 21 & !is.na(end)),
    na_ended    = which(yes & is_marr & is.na(end) & (has_later | ended_status)),
    intact      = which(yes & is_marr & is.na(end) & !has_later & !ended_status)
  )
}

impute_husb_priorKids <- function(df, fit, dist = fit$dist, propagate = TRUE,
                                  imputed_code = IMPUTED_CODE_PRIORKIDS,
                                  reimpute_placeholder = TRUE,
                                  exclude_widowed = TRUE, max_slots = 10L,
                                  survey = "NSFG2002") {
  flag_cols <- grep("^union_husb_priorKids[0-9]+$", names(df), value = TRUE)
  if (length(flag_cols) == 0L) {
    warning("impute_husb_priorKids(): no union_husb_priorKids* flags; ",
            "run add_union_husb_priorKids()")
    return(df)
  }
  slots <- as.integer(sub("^union_husb_priorKids", "", flag_cols))
  tally <- c(placeholder = 0L, na_ended = 0L, intact_skipped = 0L)

  for (s in slots) {
    tg <- select_issue2_targets(df, s, exclude_widowed, max_slots)
    tally["intact_skipped"] <- tally["intact_skipped"] + length(tg$intact)

    do_idx <- sort(unique(c(if (reimpute_placeholder) tg$placeholder, tg$na_ended)))
    do_idx <- do_idx[df$survey[do_idx] == survey]   # restrict imputation to the skip-affected survey
    if (!length(do_idx)) next

    start  <- suppressWarnings(as.numeric(df[[paste0("union_start_cmc", s)]][do_idx]))
    mstart <- suppressWarnings(as.numeric(df[[paste0("marriage_start_cmc", s)]][do_idx]))
    endcur <- suppressWarnings(as.numeric(df[[paste0("union_end_cmc", s)]][do_idx]))
    upper  <- ifelse(!is.na(endcur), endcur, df$surveyDate_cmc[do_idx])  # placeholder vs last-union
    lower  <- ifelse(!is.na(mstart), mstart, start)

    nd  <- union_model_covariates(start, df$indiv_dob_cmc[do_idx],
                                  is_marriage = TRUE,
                                  union_order = union_order_at(df, do_idx, start, max_slots),
                                  prior_kids  = TRUE)
    end <- draw_truncated_end(fit, nd, start_cmc = start,
                              lower_cmc = lower, upper_cmc = upper,
                              dist = dist, propagate = propagate)
    df  <- .write_union_end(df, s, do_idx, end, imputed_code)

    wr <- !is.na(end)
    tally["placeholder"] <- tally["placeholder"] + sum(do_idx %in% tg$placeholder & wr)
    tally["na_ended"]    <- tally["na_ended"]    + sum(do_idx %in% tg$na_ended & wr)
  }
  message("issue 2: re-imputed ", tally["placeholder"], " back-fill placeholders, ",
          tally["na_ended"], " NA-end ended marriages; ",
          tally["intact_skipped"], " intact marriages left ongoing")
  df
}


# ==== 7. Check: imputed vs observed durations (2006-10) ====

# Posterior-predictive self-check on the cycle the model was trained on. For the
# 2006-10 spells that actually ended in separation, re-impute the end date AS IF
# it were missing (same truncated draw, bounded [start, survey]) and compare the
# imputed duration distribution with the observed one (left panel). The right
# panel overlays the model survival curve on the Kaplan-Meier of all 2006-10
# spells -- a censoring-correct fit check. Good model => densities overlap and
# the curves track each other. Returns the obs/imputed durations invisibly.
check_imputation <- function(df, fit, survey = "NSFG2006_10", dist = fit$dist,
                             max_slots = 10L, n_rep = 5L, seed = 1L,
                             propagate = FALSE, plot = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  d <- df[df$survey == survey, , drop = FALSE]
  if (nrow(d) == 0L) stop("no rows for survey ", survey)

  slots <- which(paste0("union_start_cmc", 1:max_slots) %in% names(d))

  # gather observed spells with the fields the draw needs
  S <- list()
  for (s in slots) {
    start  <- suppressWarnings(as.numeric(d[[paste0("union_start_cmc", s)]]))
    end    <- suppressWarnings(as.numeric(d[[paste0("union_end_cmc", s)]]))
    mot    <- as.character(d[[paste0("union_end_motive", s)]])
    mstart <- suppressWarnings(as.numeric(d[[paste0("marriage_start_cmc", s)]]))

    occ <- !is.na(start) & start < 9000
    ord <- integer(nrow(d))
    for (s2 in slots) {
      st2 <- suppressWarnings(as.numeric(d[[paste0("union_start_cmc", s2)]]))
      ord <- ord + (!is.na(st2) & st2 < 9000 & !is.na(start) & st2 <= start)
    }
    use <- occ & (mot %in% c("in union", "separation", "widowhood"))
    if (!any(use)) next

    tend <- ifelse(mot %in% c("separation", "widowhood") & !is.na(end),
                   end, d$surveyDate_cmc)
    S[[length(S) + 1L]] <- data.frame(
      start   = start[use],
      survey  = d$surveyDate_cmc[use],
      dur     = (tend - start)[use],
      event   = as.integer((mot == "separation" & !is.na(end) & end < 9000)[use]),
      is_marr = (!is.na(mstart) & mstart < 9000)[use],
      order   = ord[use],
      dob     = d$indiv_dob_cmc[use],
      weight  = if ("indiv_weight" %in% names(d)) d$indiv_weight[use] else 1
    )
  }
  S <- do.call(rbind, S)
  S <- S[is.finite(S$dur) & S$dur >= 1 & is.finite(S$weight), ]

  # impute-the-observed: redraw the end for separated spells, bounded [start, survey]
  ev  <- S[S$event == 1L, ]
  nd  <- union_model_covariates(ev$start, ev$dob, ev$is_marr, ev$order)
  imp <- numeric(0)
  for (r in seq_len(n_rep)) {
    e   <- draw_truncated_end(fit, nd, start_cmc = ev$start,
                              lower_cmc = ev$start, upper_cmc = ev$survey,
                              dist = dist, propagate = propagate)
    imp <- c(imp, e - ev$start)
  }
  imp <- imp[is.finite(imp)]
  obs <- ev$dur

  if (isTRUE(plot)) {
    op <- par(mfrow = c(1, 2)); on.exit(par(op))

    do <- density(obs / 12); di <- density(imp / 12)
    plot(do, col = "black", lwd = 2, lty = 1, ylim = c(0, max(do$y, di$y)),
         xlab = "duration to separation (years)",
         main = paste0("Observed vs imputed (", survey, ")"))
    lines(di, col = "red", lwd = 2, lty = 2)
    legend("topright", c("observed", "model-imputed"),
           col = c("black", "red"), lwd = 2, lty = c(1, 2), bty = "n")

    km <- survival::survfit(Surv(dur, event) ~ 1, data = S, weights = S$weight)
    tg <- seq(1, max(S$dur), length.out = 200)
    cov_all <- union_model_covariates(S$start, S$dob, S$is_marr, S$order)
    X   <- model.matrix(delete.response(terms(fit)), cov_all, xlev = fit$xlevels)
    X   <- X[, names(coef(fit)), drop = FALSE]
    lp  <- as.numeric(X %*% coef(fit))
    w   <- S$weight / sum(S$weight)
    Sft <- vapply(tg, function(t) sum(w * (1 - aft_cdf(t, lp, fit$scale, dist))), numeric(1))
    plot(km$time / 12, km$surv, type = "s", ylim = c(0, 1),
         xlab = "duration (years)", ylab = "S(t): still in union",
         main = "KM vs fitted survival")
    lines(tg / 12, Sft, col = "red", lwd = 2, lty = 2)
    legend("topright", c("Kaplan-Meier", "model"),
           col = c("black", "red"), lwd = 2, lty = c(1, 2), bty = "n")
  }

  summ <- data.frame(
    quantity = c("n_separations", "median_obs_yr", "median_imp_yr",
                 "mean_obs_yr", "mean_imp_yr"),
    value = c(nrow(ev),
              round(median(obs) / 12, 2), round(median(imp) / 12, 2),
              round(mean(obs)  / 12, 2), round(mean(imp)  / 12, 2)))
  message("check_imputation: ", nrow(ev), " observed separations; median obs ",
          round(median(obs) / 12, 1), " yr vs imputed ",
          round(median(imp) / 12, 1), " yr")
  invisible(list(observed = obs, imputed = imp, summary = summ))
}


# ==== 8. Driver / multiple imputation (example, commented) ====

# Single completed dataset:
#   train <- build_dissolution_training(NSFG_ENADID)
#   fit   <- fit_dissolution_model(train, dist = "loglogistic")
#   set.seed(1)
#   NSFG_ENADID <- impute_sep_recent(NSFG_ENADID, fit)
#   NSFG_ENADID <- impute_husb_priorKids(NSFG_ENADID, fit)
#
# Proper multiple imputation (M completed datasets; pool with Rubin's rules):
#   train <- build_dissolution_training(NSFG_ENADID)
#   fit   <- fit_dissolution_model(train)
#   imps  <- lapply(1:20, function(m) {
#     set.seed(m)
#     d <- impute_sep_recent(NSFG_ENADID, fit, propagate = TRUE)
#     impute_husb_priorKids(d, fit, propagate = TRUE)
#   })
#
# NOTE: imputed ends carry union_end_cmc_I 41 (separated) or 42 (prior-kids). They are bounded
# [start, survey] (and >= marriage_start for issue 2), so they satisfy every
# cleanENADID date check by construction.


# ==== 9. Document the raw separated-women dissolution gap (2002) ====

# Standalone diagnostic on getDatos_2002() output (BEFORE cleanENADID): counts
# currently-separated women whose union-end date is missing and reconciles the
# sub-groups, reproducing the by-number-of-unions breakdown.
#
# The gap is an NSFG UNIVERSE feature, not a counting/routing error. The marriage
# stop-living / end date is collected inside the marriage-DISSOLUTION sequence,
# whose universe is marriages ENDED by divorce / annulment / widowhood. A current
# separated marriage has not legally ended, so its date is out of universe --
# missing for 100% of first-union separated women. The dates seen for some
# higher-order separated women belong to a DIFFERENT, genuinely-ended union (a
# prior divorce or a broken cohabitation), NOT to the current separation: the
# dated_with_open count below shows their separation slot is still open.
document_sep_gap <- function(df, status = "separated", max_slots = 10L) {
  has <- function(p, s) paste0(p, s) %in% names(df)
  num <- function(p, s) suppressWarnings(as.numeric(df[[paste0(p, s)]]))
  slots <- (1:max_slots)[paste0("union_start_cmc", 1:max_slots) %in% names(df)]

  sep <- !is.na(df$union_status) & as.character(df$union_status) == status

  occ <- sapply(slots, function(s) !is.na(num("union_start_cmc", s)) &
                                   num("union_start_cmc", s) < 9000)
  if (is.null(dim(occ))) occ <- matrix(occ, ncol = length(slots))
  nUnion    <- rowSums(occ)
  last_slot <- apply(occ, 1L, function(r) if (any(r)) max(slots[r]) else NA_integer_)

  endM <- sapply(slots, function(s) if (has("union_end_cmc", s)) num("union_end_cmc", s)
                                    else rep(NA_real_, nrow(df)))
  if (is.null(dim(endM))) endM <- matrix(endM, ncol = length(slots))
  col_of   <- match(last_slot, slots)
  end_last <- rep(NA_real_, nrow(df))
  ok       <- !is.na(col_of)
  end_last[ok] <- endM[cbind(which(ok), col_of[ok])]

  any_open <- rowSums(occ & is.na(endM)) > 0           # >=1 occupied slot, no end

  no_union   <- sep & nUnion == 0
  last_na    <- sep & nUnion >= 1 & is.na(end_last)
  last_dated <- sep & nUnion >= 1 & !is.na(end_last)

  cat("==== Separated dissolution gap (raw getDatos_2002, pre-clean) ====\n")
  cat("separated women (union_status == '", status, "'): ", sum(sep), "\n\n", sep = "")
  cat("No end on LAST union, by number of unions:\n")
  print(table(nUnion = nUnion[last_na]))
  cat("\nReconciliation:\n")
  cat("  no end on last union :", sum(last_na),    " (the documented gap)\n")
  cat("  end recorded on last :", sum(last_dated), " (date is from an ended union)\n")
  cat("  no union at all      :", sum(no_union),   "\n")
  cat("  total separated      :",
      sum(last_na) + sum(last_dated) + sum(no_union), "\n\n")
  cat("Why the 'recorded' ones are not exceptions to the skip:\n")
  cat("  of those", sum(last_dated), "dated-last-union women,",
      sum(last_dated & any_open), "still have an EARLIER open union\n")
  cat("  (their separation itself -- the dated union is a different one).\n")
  cat("Separated women with >=1 open union slot (true separation gap):",
      sum(sep & any_open), "\n")

  invisible(list(
    separated = sum(sep), last_na = sum(last_na), last_dated = sum(last_dated),
    no_union = sum(no_union), dated_with_open = sum(last_dated & any_open),
    any_open = sum(sep & any_open)))
}
