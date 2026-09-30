# >>> Claude 2026-09-25
# archive/ENADID_fertility_unused.R
#
# Code removed from "ENADID fertility.R" on 2026-09-25 because nothing used it:
# three alternative TFR calculations for Mexico and one for the USA, all
# commented out, and the function plot10Years_child_1(), defined but never
# called (the children figures use plot10Years_child()). Kept for the record.
# To use a piece, copy it back into "ENADID fertility.R" at the place named.
# <<< Claude 2026-09-25


# ==== Mexico TFR: population weights, loess variance, smoothed ====
# (was in "ENADID fertility.R", section "Total fertility")

# using population weights, loess variance, smooth
# MEX_TFR_2w_s_1992 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1992",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_1997 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1997",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2006 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2006",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2009 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2009",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2014 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2014",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2017 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2017",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2018 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2018",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2023 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2023",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_2025 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2025",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_s_pooled <- computeTFR_2weights(df_fert_MEX, numLastYears=44, orderPlus=5, removeLastYear=TRUE)
# 
# dfPlot_Mex_2w_s <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_smooth_min", "tfr_smooth_max"), pattern="MEX_TFR_2w_s_")
# tfrSmoothPlot_2w_s <- plotTFR_mean (dfPlot_Mex_2w_s,
#                                x="year", y="tfr_smooth", ymin="tfr_smooth_min", ymax="tfr_smooth_max",
#                                xTitle="year", yTitle="TFR",
#                                dfObserved=TFR_Mex, country="MEXICO")
# tfrSmoothPlot_2w_s

# ==== Mexico TFR: population weights, not smoothed ====
# (was in "ENADID fertility.R", section "Total fertility")

# using population weights, non smooth
# MEX_TFR_2w_ns_1992 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1992",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_1997 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1997",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2006 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2006",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2009 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2009",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2014 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2014",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2017 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2017",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2018 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2018",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2023 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2023",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_2025 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2025",], orderPlus=5, smooth=FALSE, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_2w_ns_pooled <- computeTFR_2weights(df_fert_MEX, numLastYears=44, orderPlus=5, smooth=FALSE, removeLastYear=TRUE)
# 
# dfPlot_Mex_2w_ns <- prepareDataPlotGen (keeps=c("year", "tfr", "tfr_min", "tfr_max"), pattern="MEX_TFR_2w_ns_")
# tfrSmoothPlot_2w_ns <- plotTFR_mean (dfPlot_Mex_2w_ns,
#                                   x="year", y="tfr", ymin="tfr_min", ymax="tfr_max",
#                                   xTitle="year", yTitle="TFR",
#                                   dfObserved=TFR_Mex, country="MEXICO")
# tfrSmoothPlot_2w_ns

# ==== Mexico TFR: individual weights, loess variance ====
# (was in "ENADID fertility.R", section "Total fertility")

# # using individual weights, loess var
# MEX_TFR_1w_1992 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID1992",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_1997 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID1997",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2006 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2006",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2009 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2009",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2014 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2014",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2017 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="EDER2017",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2018 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2018",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2023 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2023",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_2025 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="EDER2025",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
# MEX_TFR_1w_pooled <- computeTFR(df_fert_MEX, numLastYears=44, orderPlus=5, removeLastYear=TRUE)
# 
# dfPlot_Mex <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_min", "tfr_max"), pattern="MEX_TFR_1w_")
# tfrSmoothPlot <- plotTFR_mean (dfPlot_Mex,
#                                   x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max",
#                                   xTitle="year", yTitle="TFR",
#                                   dfObserved=TFR_Mex, country="MEXICO")
# tfrSmoothPlot

# ==== USA TFR: cohort version (computeTFR_cohort) ====
# (was in "ENADID fertility.R", section "Total fertility")

# USA2_TFR1982 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG1982",], maxOrder=5, numYear=6, loessSpan=0.95)
# USA2_TFR1988 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG1988",], maxOrder=5, numYear=6, loessSpan=0.95)
# USA2_TFR1995 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG1995",], maxOrder=5, numYear=6, loessSpan=0.95)
# USA2_TFR2002 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2002",], maxOrder=5, numYear=6, loessSpan=0.95)
# USA2_TFR2006 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2006_10",], maxOrder=5, numYear=6, loessSpan=0.95)
# USA2_TFR2011 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2011_13",], maxOrder=5, numYear=7, loessSpan=0.95)
# USA2_TFR2013 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2013_15",], maxOrder=5, numYear=7, loessSpan=0.95)
# USA2_TFR2015 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2015_17",], maxOrder=5, numYear=7, loessSpan=0.95)
# USA2_TFR2017 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2017_19",], maxOrder=5, numYear=12, loessSpan=0.95)
# USA2_TFR2022 <- computeTFR_cohort(df_fert_USA[df_fert_USA$survey=="NSFG2022_23",], maxOrder=5, numYear=12, loessSpan=0.95)
# USA2_TFRpooled <- computeTFR_cohort(df_fert_USA, numYear=45, maxOrder=5, loessSpan=0.95)
# 
# dfPlot <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_min", "tfr_max"), pattern="USA2_TFR")
# tfrSmoothPlot <- plotTFR_mean (dfPlot, x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max", xTitle="year", yTitle="TFR", yLim=c(1.5,2.5))
# tfrSmoothPlot

# ==== plot10Years_child_1(): first union only, never called ====
# (was in "ENADID fertility.R", section "10 first years of first children")

plot10Years_child_1 <- function(df, LifeLimit=120, smooth=TRUE) {

  df <- subset(df,firstBirth_durLife_capped==LifeLimit)
  df$yBirth1 <- yearFrom_cmc(df$cmc_birth1)
  df$firstBirthLife_noUnion[is.na(df$firstBirthLife_noUnion)] <- 0
  df$firstBirthLife_BeforeUnion[is.na(df$firstBirthLife_BeforeUnion)] <- 0
  df$firstBirthLife_DuringUnion[is.na(df$firstBirthLife_DuringUnion)] <- 0
  df$firstBirthLife_AfterUnion[is.na(df$firstBirthLife_AfterUnion)] <- 0
  df$firstBirth_befUnion <- df$firstBirthLife_noUnion + df$firstBirthLife_BeforeUnion
  df$firstBirth_befUnion <- ifelse(df$firstBirth_befUnion > LifeLimit, LifeLimit, df$firstBirth_befUnion)
  df$firstBirth_durUnion <- ifelse(df$firstBirth_befUnion == LifeLimit, 0,
                                                  pmin(LifeLimit - df$firstBirth_befUnion,
                                                       df$firstBirthLife_DuringUnion))
  df$firstBirth_afterUnion <- ifelse((df$firstBirth_durUnion + df$firstBirth_befUnion) == LifeLimit, 0,
                                                    pmin(LifeLimit -
                                                           (df$firstBirth_durUnion + df$firstBirth_befUnion),
                                                         df$firstBirthLife_AfterUnion))
  df$firstBirth_befUnion_w <- df$firstBirth_befUnion * df$popWeight
  df$firstBirth_durUnion_w <- df$firstBirth_durUnion * df$popWeight
  df$firstBirth_afterUnion_w <- df$firstBirth_afterUnion * df$popWeight
  #compute proportions

  df_yBirth <- df %>%
    group_by(yBirth1) %>%
    summarise(
      p1 = sum(firstBirth_befUnion_w,   na.rm = TRUE),
      p2 = sum(firstBirth_durUnion_w,   na.rm = TRUE),
      p3 = sum(firstBirth_afterUnion_w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      wsum       = p1 + p2 + p3,
      propBef    = p1 / wsum,
      propDuring = p2 / wsum,
      propAfter  = p3 / wsum
    ) %>%
    select(-c(wsum, p1, p2, p3))

  df_yBirth %>%
    pivot_longer(
      cols      = c(propBef, propDuring, propAfter),
      names_to  = "regime",
      values_to = "prop"
    ) %>%
    mutate(regime = factor(
      regime,
      levels = c("propBef", "propDuring", "propAfter"),
      labels = c("Before union", "During union", "After union")
    )) %>%
    ggplot(aes(x = yBirth1, y = prop, fill = regime)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(
      floor(min(df_yBirth$yBirth1) / 10) * 10,
      ceiling(max(df_yBirth$yBirth1) / 10) * 10,
      by = 10
    )) +
    labs(x = "Year of birth", y = "Share", fill = NULL)

  ### plot with smoothing ###
  # Smoothing parameters
  SPAN <- 0.9          # loess span; raise toward 1 when cohort years are few
  EPS  <- 1e-6         # floor to guard log(0) in empty categories


  # Weighted loess smoother: one fitted value per input x.
  # surface = "direct" keeps prediction safe at the range edges.
  smooth_one <- function(y, x, w = rep(1, length(x)), span = SPAN) {
    fit <- loess(y ~ x, weights = w, span = span,
                 control = loess.control(surface = "direct"))
    as.numeric(predict(fit, newdata = data.frame(x = x)))
  }


  # === 1. Weighted shares by year of birth ===

  df_yBirth <- df %>%
    group_by(yBirth1) %>%
    summarise(
      p1   = sum(firstBirth_befUnion_w,   na.rm = TRUE),
      p2   = sum(firstBirth_durUnion_w,   na.rm = TRUE),
      p3   = sum(firstBirth_afterUnion_w, na.rm = TRUE),
      wpop = sum(popWeight,               na.rm = TRUE),   # smoothing weight per year
      .groups = "drop"
    ) %>%
    mutate(
      wsum       = p1 + p2 + p3,
      propBef    = p1 / wsum,
      propDuring = p2 / wsum,
      propAfter  = p3 / wsum
    ) %>%
    select(-c(wsum, p1, p2, p3))   # keep yBirth1, the three shares, and wpop


  # === 2. Log-ratio smoothing (sum-to-1 preserved) ===

  # Smooth two additive log-ratios (reference = After union), weighted by the
  # per-year population weight, then back-transform via softmax. The result is
  # positive and sums to 1 at every year by construction.

  df_smooth <- df_yBirth %>%
    mutate(
      lr1 = log(pmax(propBef,    EPS) / pmax(propAfter, EPS)),
      lr2 = log(pmax(propDuring, EPS) / pmax(propAfter, EPS)),
      s1  = smooth_one(lr1, yBirth1, w = wpop),
      s2  = smooth_one(lr2, yBirth1, w = wpop),
      denom      = 1 + exp(s1) + exp(s2),
      propBef    = exp(s1) / denom,
      propDuring = exp(s2) / denom,
      propAfter  = 1       / denom
    ) %>%
    select(-lr1, -lr2, -s1, -s2, -denom)


  # === 3. Stacked area plot ===

  df_smooth %>%
    pivot_longer(
      cols      = c(propBef, propDuring, propAfter),
      names_to  = "regime",
      values_to = "prop"
    ) %>%
    mutate(regime = factor(
      regime,
      levels = c("propBef", "propDuring", "propAfter"),
      labels = c("Before union", "During union", "After union")
    )) %>%
    ggplot(aes(x = yBirth1, y = prop, fill = regime)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(
      floor(min(df_smooth$yBirth1)   / 10) * 10,
      ceiling(max(df_smooth$yBirth1) / 10) * 10,
      by = 10
    )) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Year of birth", y = "Share", fill = NULL)

}
