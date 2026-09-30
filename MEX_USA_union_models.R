# >>> Claude 2026-09-25
# MEX_USA_union_models.R
#
# Further analyses of first unions, not in the paper, moved here from
# KaplanMeier.R (its sections 4b.2 to 4b.4; the rest of that file is in
# archive/KaplanMeier.R):
#
#   M1 (old 4b.2)  what happens to a cohabiting first union: remain, marry,
#                  separate, Aalen-Johansen, per country and combined
#   M2 (old 4b.3)  does the risk of separation fall once a cohabitation turns
#                  into marriage? Cox model with marriage as a time-varying state
#   M3 (old 4b.4)  direct against converted marriages, standardised survival
#                  after the wedding (read adjustedSurv_note() first)
#
# The union episodes are the same as in MEX_USA_figures_cohort.R
# (buildBothEpisodes() in lib/mexUsaFigures.R). Figures are drawn on screen;
# use saveFigure(<plot>, "<file>.pdf") to keep one.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")          # also loads lib/mexUsaFigures.R and lib/unionEpisodes.R
source("lib/adjustedSurv.R")    # adjustedSurv, adjustedSurv_note
library(survival)


# ==== 0. Data and union episodes ====

if (!exists("MEXICO_ENADID") || !exists("NSFG_ENADID")) loadENADID_data()
EP <- buildBothEpisodes(filterDateQuality(MEXICO_ENADID), filterDateQuality(NSFG_ENADID),
                        minUnions = 200)
MEX_epi  <- EP$MEX
USA_epi  <- EP$USA
BOTH_epi <- EP$BOTH
# <<< Claude 2026-09-25


# ==== M1. Separation before marriage, against conversion to marriage ====
# The cumulative incidence of separating while still cohabiting, with
# conversion to marriage as a competing event rather than censoring.
# >>> Claude 2026-09-21
# Mexico, the USA, and the two together.
crPlot <- function (curves, Title, extra = NULL) {
  d <- subset(curves, (state != "(s0)") & (timeYear <= 25))
  p <- ggplot(d, aes(x = timeYear, y = p, colour = state)) +
    geom_step(linewidth = 0.8) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1)) +
    labs(title = Title,
         subtitle = "Marriage treated as a competing event, not as censoring",
         x = "Duration in years after the union began",
         y = "Cumulative incidence", colour = NULL) +
    theme_linedraw() + theme(legend.position = "bottom")
  if (!is.null(extra)) p <- p + extra
  p
}

MEX_cr1 <- unionCompetingRisks(MEX_epi, from = "cohabiting", horizonYears = c(5, 10, 15))
print(MEX_cr1$table)
plot_cr_MEX <- crPlot(MEX_cr1$curves,
                      "Mexico: what happens to a cohabiting first union (Aalen-Johansen)")
plot_cr_MEX

USA_cr1 <- unionCompetingRisks(USA_epi, from = "cohabiting", horizonYears = c(5, 10, 15))
print(USA_cr1$table)
plot_cr_USA <- crPlot(USA_cr1$curves,
                      "USA: what happens to a cohabiting first union (Aalen-Johansen)")
plot_cr_USA

# Combined, one panel per country so the same outcome can be read across.
BOTH_cr1 <- unionCompetingRisks(BOTH_epi, from = "cohabiting", by = "country",
                                horizonYears = c(5, 10, 15))
print(BOTH_cr1$table)
plot_cr_BOTH <- crPlot(BOTH_cr1$curves,
                       "Mexico and the USA: what happens to a cohabiting first union",
                       extra = facet_wrap(~ group))
plot_cr_BOTH

# The same three outcomes by union cohort are block C6 of MEX_USA_figures_cohort.R.
# <<< Claude 2026-09-21


# ==== M2. Does the separation risk fall once a union converts? ====
# 'married' is TIME-VARYING: a union contributes cohabiting exposure until it
# converts and married exposure afterwards. Nothing about its future is used to
# place it, so this is not anticipatory analysis.
MEX_epi$sepEvent <- as.integer(MEX_epi$to %in% c("separation (cohabiting)", "separation (married)"))
MEX_epi$married  <- as.integer(MEX_epi$istate != "cohabiting")

fit_state_MEX <- coxph(Surv(tstart, tstop, sepEvent) ~ married + ageUnion + cluster(id),
                       data = MEX_epi, weights = w)
print(summary(fit_state_MEX))    # the 'married' coefficient IS the drop at conversion

# The same thing as a picture: separation accumulating in each state.
sf_state_MEX <- survfit(Surv(tstart, tstop, sepEvent) ~ married, data = MEX_epi, weights = w)
MEX_haz <- data.frame(timeYear = sf_state_MEX$time / 12,
                      cumhaz   = sf_state_MEX$cumhaz,
                      state    = rep(c("cohabiting", "married"), sf_state_MEX$strata))
plot_stateHaz_MEX <- ggplot(subset(MEX_haz, timeYear <= 25),
                            aes(timeYear, cumhaz, colour = state)) +
  geom_step(linewidth = 0.8) +
  labs(title = "Mexico: cumulative risk of separation, by the state the union is in",
       x = "Duration in years after the union began", y = "Cumulative hazard of separation",
       colour = "Currently") +
  theme_linedraw() + theme(legend.position = "bottom")
plot_stateHaz_MEX


# ==== M3. Direct against converted marriages, clock reset at the wedding ====
# READ adjustedSurv_note() FIRST. Holding age at union and age at marriage
# fixed is the same as holding cohabDur fixed, and the only cohabitation a
# direct marriage has is zero, so the contrast extrapolates. The restricted
# version below needs much less extrapolation; report both.
MEX_mar <- subset(MEX_epi, istate %in% c("married direct", "married converted"))
MEX_mar$t     <- MEX_mar$tstop - MEX_mar$tstart
MEX_mar$sep   <- as.integer(MEX_mar$to == "separation (married)")
MEX_mar$route <- factor(ifelse(MEX_mar$istate == "married direct", "direct", "converted"),
                        levels = c("direct", "converted"))

adj_all_MEX  <- adjustedSurv(MEX_mar, "t", "sep", "route",
                             covariates = c("ageUnion", "cohabDur"),
                             weightVar = "w", maxTime = 25 * 12)
MEX_mar_short <- subset(MEX_mar, (route == "direct") | (cohabDur <= 2))
adj_short_MEX <- adjustedSurv(MEX_mar_short, "t", "sep", "route",
                              covariates = c("ageUnion", "cohabDur"),
                              weightVar = "w", maxTime = 25 * 12)
cat(sprintf("HR converted vs direct: all cohabitations %.3f | cohabitation under 2 y %.3f\n",
            adj_all_MEX$hr[["routeconverted"]], adj_short_MEX$hr[["routeconverted"]]))

MEX_adj <- rbind(transform(adj_all_MEX$curves,   spec = "all converted marriages"),
                 transform(adj_short_MEX$curves, spec = "cohabitation under 2 years"))
MEX_adj$timeYear <- MEX_adj$time / 12
plot_adj_MEX <- ggplot(MEX_adj, aes(timeYear, surv, colour = group)) +
  geom_line(linewidth = 0.8) + facet_wrap(~ spec) + coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Mexico: still married, direct against converted, standardised",
       subtitle = "Age at union and cohabitation duration held to one common distribution",
       x = "Duration in years after the marriage began", y = "Still married", colour = NULL) +
  theme_linedraw() + theme(legend.position = "bottom")
plot_adj_MEX
