# >>> Claude 2026-09-24
# MEX_USA_lifecourse_AJ.R
#
# Where women are between age 15 and 45: single, cohabiting, married after a
# cohabitation, married directly, or out of their first union by separation
# (three origins) or widowhood. One Aalen-Johansen fit on the age scale,
# stratified by country, with all surveys pooled and population weights.
# Only the first union is followed; separation and widowhood are absorbing.
#
# Surveys excluded, as in section 4b of KaplanMeier.R:
#   Mexico: WFS, ENADID1992, ENADID2006
#   USA:    NSFG1973, NSFG1976 (ever-married or mothers only), NSFG1982, NSFG2017_19

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("enadid_lib.R")
source("lib/lifeCourseAJ_lib.R")
suppressMessages({library(dplyr); library(ggplot2); library(survival)})

AGE_FROM <- 15
AGE_TO   <- 45
path_output <- paste0(outputPath, "/presentation_2026-09-24/")
dir.create(path_output, showWarnings = FALSE)


# ==== 1. Data ====

load(paste0(dataPath, "MEXICO_ENADID.Rdat"))
load(paste0(dataPath, "NSFG_ENADID.Rdat"))
MEX <- filterDateQuality(MEXICO_ENADID, verbose = FALSE)
USA <- filterDateQuality(NSFG_ENADID, verbose = FALSE)
MEX <- subset(MEX, !(survey %in% c("WFS", "ENADID1992", "ENADID2006")))
USA <- subset(USA, !(survey %in% c("NSFG1973", "NSFG1976", "NSFG1982", "NSFG2017_19")))
MEX$country <- "MEXICO"
USA$country <- "USA"


# ==== 2. Episodes and fit ====

life_ep_MEX <- buildLifeEpisodes(MEX, ageFrom = AGE_FROM, ageTo = AGE_TO)
life_ep_USA <- buildLifeEpisodes(USA, ageFrom = AGE_FROM, ageTo = AGE_TO)
# ids must be distinct across countries, one id per stratum
life_ep_USA$id <- life_ep_USA$id + max(life_ep_MEX$id)
life_ep <- rbind(life_ep_MEX, life_ep_USA)

life_occ <- lifeStateOccupancy(life_ep, by = "country", ageFrom = AGE_FROM)

# shares at selected ages, for the text
life_tab <- life_occ %>% group_by(group, state) %>%
  summarise(age20 = p[findInterval(20, age)], age25 = p[findInterval(25, age)],
            age30 = p[findInterval(30, age)], age35 = p[findInterval(35, age)],
            age40 = p[findInterval(40, age)], age45 = p[findInterval(45, age)],
            .groups = "drop")
print(as.data.frame(life_tab), digits = 2)
write.csv(life_tab, paste0(path_output, "MEX_USA_lifecourse_AJ.csv"), row.names = FALSE)


# ==== 3. Plot ====

plot_lifecourse <- lifeCoursePlot(life_occ,
                                  "Mexico and the USA: women's first union from age 15 to 45")
plot_lifecourse
ggsave(paste0(path_output, "MEX_USA_lifecourse_AJ.pdf"), plot_lifecourse,
       width = 29.7, height = 21, units = "cm")
ggsave(paste0(path_output, "MEX_USA_lifecourse_AJ.png"), plot_lifecourse,
       width = 13.33, height = 7.5, dpi = 150)
# <<< Claude 2026-09-24
