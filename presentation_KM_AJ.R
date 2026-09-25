# >>> Claude 2026-09-24
# presentation_KM_AJ.R
#
# Kaplan-Meier against Aalen-Johansen for Mexican first unions (presentation of
# 2026-09-24). Plot 1: all first unions, survival to separation, widowhood
# censored (KM, net) or competing (AJ, crude). Plot 2: unions that start as
# cohabitation, survival to separation, marriage and widowhood censored (KM) or
# competing (AJ). Plot 3: AJ state occupancy of those cohabitations.
# Surveys excluded as in the paper: ENADID1992 and ENADID2006 for plot 1, plus
# WFS for plots 2 and 3. Population weights; KM bands use the infinitesimal
# jackknife on log-log bounds, AJ bands come from survfit.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
suppressMessages({library(dplyr); library(ggplot2); library(survival); library(tidyr)})
load(paste0(dataPath, "MEXICO_ENADID.Rdat"))
MEX <- filterDateQuality(MEXICO_ENADID, verbose = FALSE)
path_output <- paste0(outputPath, "/presentation_2026-09-24/")
dir.create(path_output, showWarnings = FALSE)


# step function of a survfit object read on a grid of years
onGrid <- function (tt, x, grid, x0) { pos <- findInterval(grid, tt); c(x0, x)[pos + 1] }
grid <- seq(0, 30, by = 1/12)

# ---------- 1. all first unions ----------
d1 <- subset(MEX, !(survey %in% c("ENADID1992", "ENADID2006")))
ep1 <- buildUnionEpisodes(d1, u = 1, varWeight = "popWeight", unknownEndAs = "separation", quiet = TRUE)
last <- ep1 %>% group_by(id) %>% slice_max(tstop, n = 1, with_ties = FALSE) %>% ungroup() %>%
  mutate(T = tstop / 12,
         ev = ifelse(grepl("^separation", as.character(to)), "separation",
              ifelse(as.character(to) == "widowed", "widowed", "censor")))
cat("All first unions:", nrow(last), "| separations", sum(last$ev == "separation"),
    "| widowhoods", sum(last$ev == "widowed"), "\n")

km1 <- survfit(Surv(T, ev == "separation") ~ 1, data = last, weights = w, id = id,
               robust = TRUE, conf.type = "log-log")
aj1 <- survfit(Surv(T, factor(ev, levels = c("censor", "separation", "widowed"))) ~ 1,
               data = last, weights = w, id = id)
k  <- match("separation", aj1$states)
df1 <- bind_rows(
  data.frame(t = grid, est = onGrid(km1$time, km1$surv, grid, 1),
             lo = onGrid(km1$time, km1$lower, grid, 1), hi = onGrid(km1$time, km1$upper, grid, 1),
             method = "Kaplan-Meier: widowhood censored (net)"),
  data.frame(t = grid, est = 1 - onGrid(aj1$time, aj1$pstate[, k], grid, 0),
             lo = 1 - onGrid(aj1$time, aj1$upper[, k], grid, 0), hi = 1 - onGrid(aj1$time, aj1$lower[, k], grid, 0),
             method = "Aalen-Johansen: widowhood as competing risk (crude)"))

# ---------- 2. cohabitations ----------
d2 <- subset(MEX, !(survey %in% c("WFS", "ENADID1992", "ENADID2006")))
ep2 <- buildUnionEpisodes(d2, u = 1, varWeight = "popWeight", unknownEndAs = "separation", quiet = TRUE)
co <- ep2 %>% filter(as.character(istate) == "cohabiting") %>%
  mutate(T = tstop / 12,
         ev = case_when(grepl("^separation", as.character(to)) ~ "separation",
                        as.character(to) == "married converted" ~ "married",
                        as.character(to) == "widowed" ~ "widowed",
                        TRUE ~ "censor"))
cat("Cohabitations:", nrow(co), "| separations", sum(co$ev == "separation"), "| marriages",
    sum(co$ev == "married"), "| widowhoods", sum(co$ev == "widowed"), "\n")

km2 <- survfit(Surv(T, ev == "separation") ~ 1, data = co, weights = w, id = id,
               robust = TRUE, conf.type = "log-log")
aj2 <- survfit(Surv(T, factor(ev, levels = c("censor", "separation", "married", "widowed"))) ~ 1,
               data = co, weights = w, id = id)
k2 <- match("separation", aj2$states)
g2 <- seq(0, 20, by = 1/12)
df2 <- bind_rows(
  data.frame(t = g2, est = onGrid(km2$time, km2$surv, g2, 1),
             lo = onGrid(km2$time, km2$lower, g2, 1), hi = onGrid(km2$time, km2$upper, g2, 1),
             method = "Kaplan-Meier: marriage and widowhood censored (net)"),
  data.frame(t = g2, est = 1 - onGrid(aj2$time, aj2$pstate[, k2], g2, 0),
             lo = 1 - onGrid(aj2$time, aj2$upper[, k2], g2, 0), hi = 1 - onGrid(aj2$time, aj2$lower[, k2], g2, 0),
             method = "Aalen-Johansen: marriage and widowhood as competing risks (crude)"))
occ <- as.data.frame(sapply(c("(s0)", "married", "separation", "widowed"), function (s) {
  j <- match(s, aj2$states); onGrid(aj2$time, aj2$pstate[, j], g2, ifelse(s == "(s0)", 1, 0)) }))
names(occ) <- c("Still cohabiting", "Married", "Separated", "Widowed")
occ$t <- g2

for (yy in c(10, 20, 30)) {
  i <- which.min(abs(grid - yy))
  cat(sprintf("Unions, %d years: KM not separated %.3f | AJ not separated %.3f\n", yy,
              df1$est[df1$method == unique(df1$method)[1]][i], df1$est[df1$method == unique(df1$method)[2]][i]))
}
for (yy in c(5, 10, 20)) {
  i <- which.min(abs(g2 - yy))
  cat(sprintf("Cohab, %d years: KM %.3f | AJ %.3f | occupancy: cohab %.3f married %.3f separated %.3f widowed %.3f\n", yy,
              df2$est[df2$method == unique(df2$method)[1]][i], df2$est[df2$method == unique(df2$method)[2]][i],
              occ[i, 1], occ[i, 2], occ[i, 3], occ[i, 4]))
}


# ==== Plots ====

cols <- c("#2a78d6", "#eb6834")
th <- theme_minimal(base_size = 17) +
  theme(legend.position = "bottom", legend.direction = "vertical", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "#52514e"))

lab <- function (d, at) {
  d %>% group_by(method) %>% slice(sapply(at, function (a) which.min(abs(t - a)))) %>% ungroup() %>%
    mutate(txt = sprintf("%.2f", est))
}
plotKMAJ <- function (d, at, xmax, title, subtitle) {
  d$method <- factor(d$method, levels = unique(d$method))
  L <- lab(d, at)
  L$vj <- ifelse(as.integer(L$method) == 1, 1.6, -0.8)
  ggplot(d, aes(t, est, colour = method, fill = method)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
    geom_step(linewidth = 1.2) +
    geom_point(data = L, size = 2.8) +
    geom_text(data = L, aes(label = txt, vjust = vj), size = 5.5, show.legend = FALSE) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = seq(0, xmax, 5), limits = c(0, xmax)) +
    scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
    labs(title = title, subtitle = subtitle, x = "Years since the start of the union",
         y = "Proportion not separated", colour = NULL, fill = NULL) + th
}
p1 <- plotKMAJ(df1, c(10, 20, 30), 30, "Mexico: first unions, survival to separation",
               "Kaplan-Meier (widowhood censored) and Aalen-Johansen (widowhood as competing risk)")
p2 <- plotKMAJ(df2, c(5, 10, 20), 20, "Mexico: unions that start as cohabitation, survival to separation",
               "Kaplan-Meier (marriage and widowhood censored) and Aalen-Johansen (competing risks)")
ggsave(paste0(path_output, "KM_AJ_unions_Mexico.png"), p1, width = 13.33, height = 7.5, dpi = 150)
ggsave(paste0(path_output, "KM_AJ_cohabitations_Mexico.png"), p2, width = 13.33, height = 7.5, dpi = 150)

o <- occ %>% pivot_longer(-t, names_to = "state", values_to = "p") %>%
  mutate(state = factor(state, levels = c("Separated", "Widowed", "Married", "Still cohabiting")))
p3 <- ggplot(o, aes(t, p, fill = state)) + geom_area() +
  scale_fill_manual(values = c("Separated" = "#eb6834", "Widowed" = "#6b6a64",
                               "Married" = "#1baf7a", "Still cohabiting" = "#cde2fb")) +
  scale_x_continuous(breaks = seq(0, 20, 5)) + scale_y_continuous(labels = scales::percent) +
  labs(title = "Mexico: what becomes of unions that start as cohabitation",
       subtitle = "Aalen-Johansen state occupancy by duration (crude proportions, sum to 100%)",
       x = "Years since the start of the union", y = NULL, fill = NULL) +
  th + theme(legend.direction = "horizontal")
ggsave(paste0(path_output, "AJ_cohabitation_states_Mexico.png"), p3, width = 13.33, height = 7.5, dpi = 150)
# <<< Claude 2026-09-24
