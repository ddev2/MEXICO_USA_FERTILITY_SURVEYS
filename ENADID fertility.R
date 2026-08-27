setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
library (tidyverse)
library (haven)
library (purrr)
source (path.expand("~/Dropbox/RStudioData/TransitionsPPR/KaplanMeierLib.R"))
path_output_plots <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/")

pathColDHS <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Angela/Trabajo DANE 2022/DHS")
source(paste0(pathColDHS,"/DHS_lib.r"))

convert_ENADID_DHStype <- function(dfIN=NULL) {
  # create a data.frame with the fields necessaries to compute TFR and PPRs using code for DHS
  if ("popWeight" %in% colnames(dfIN)) {
    dfOUT <- dfIN[,c("country", "survey", "surveyDate_cmc", "indiv_dob_cmc", "yBirth", "weight", "popWeight", "nBioKids")]
    names(dfOUT) <- c("country", "survey", "cmcSurvey", "cmcBirthEgo", "yBirth", "weight", "popWeight", "nBirthsTot")
  } else {
    dfOUT <- dfIN[,c("country", "survey", "surveyDate_cmc", "indiv_dob_cmc", "yBirth", "weight", "nBioKids")]
    names(dfOUT) <- c("country", "survey", "cmcSurvey", "cmcBirthEgo", "yBirth", "weight", "nBirthsTot")
  }
  
  maxB <- max(dfOUT$nBirthsTot)
  for (b in (1:maxB)) {
    cmcBirth <- paste0("cmcBirthChild",b)
    dob_cmc <- paste0("dob_cmc",b)
    dfOUT[[cmcBirth]] <- dfIN[[dob_cmc]]
  }
  dfOUT <- computeYearBirth (dfOUT)
  return (dfOUT)
}

cleanBH <- function(df) {
  # remove women who have births that are after the survey (or more than 5 months after the survey, as they are likely to be errors or missing year)
  n <- nrow(df)
  maxB <- max(df$nBirthsTot)
  df$ID <- (1:nrow(df))
  delVec <- c(integer(0))
  for (b in (1:maxB)) {
    cmcBirth <- paste0("cmcBirthChild",b)
    d <- subset(df, (df[[cmcBirth]] > df$cmcSurvey+5))
    delVec <- c(delVec,d$ID)
  }
  delVec <- unique(delVec)
  df <- subset(df, !(ID %in% delVec))
  df$ID <- NULL
  newN <- nrow(df)
  if (newN < n) cat ("Removed", n-newN,"women with births after the survey\n")
  return (df)
}

birthSeries <- function (df=df_fert, order=1) {
  maxB <- max(df$nBirthsTot)
  if (order > 0) {
    ftab_agg <- table(df$survey,df[[paste0("yBirthChild",order)]])
  } else {
    # Define fixed levels once, outside the loop
    survey_levels <- sort(unique(df$survey))  # or hardcode: c("2015-2019", "2020-2022", ...)
    year_levels   <- sort(unique(unlist(
      lapply(1:maxB, function(b) df[[paste0("yBirthChild", b)]])
    )), na.last = TRUE)  # collect all years across all b columns, plus NA
    
    # Initialize accumulator
    ftab_agg <- NULL
    
    for (b in 1:maxB) {
      col <- factor(df[[paste0("yBirthChild", b)]], levels = year_levels, exclude = NULL)
      sur <- factor(df$survey,                      levels = survey_levels, exclude = NULL)
      
      ftab <- table(sur, col, useNA = "always")
      
      if (is.null(ftab_agg)) {
        ftab_agg <- ftab
      } else {
        ftab_agg <- ftab_agg + ftab
      }
    }
  }
  ftab_agg
}

prepareDataPlotGen <- function(keeps=NULL, order=0, pattern="MEX_TFR") {
  # Takes dataframe in memory with names starting with pattern, and creates a dataframe for plotting
  # order=0 corresponds to TFR Total
  # order=1 corresponds to TFR1
  # ...
  # order=5 corresponds to TFR5+ (order can be higher than 5, but then it will be TFR order+)
  dfPattern = paste0("^", pattern)
  tfr_names <- ls(pattern=dfPattern, envir = .GlobalEnv)
  firstList <- get(tfr_names[1])
  if (is.null(keeps) | !all(keeps %in% colnames(firstList[[order+1]]))) stop ("Keeps is null or bad")
  
  dfPlot <- data.frame()
  for (i in (1:length(tfr_names))) {
    aList <- get(tfr_names[i])
    data <- aList[[order+1]][keeps]
    data$yearSurvey <- substring(tfr_names[i], nchar(pattern)+1)
    dfPlot <- rbind(dfPlot, data)
  }
  return (dfPlot)
}

plotTFR_mean <- function(dfPlot,
                         x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max",
                         xTitle="year", yTitle="TFR", yLim=NULL,
                         dfObserved=NULL, country=NULL) {
  require(ggrepel)
  nSurveys <- length(table(dfPlot$yearSurvey))
  dfPlot$yearSurvey <- factor(dfPlot$yearSurvey, levels=unique(dfPlot$yearSurvey))
  lastIsPooled <- ("pooled" %in% levels(dfPlot$yearSurvey))
  
  # --- End-of-line labels ---
  df_line_ends <- dfPlot %>%
    dplyr::group_by(yearSurvey) %>%
    dplyr::slice_max(order_by = .data[[x]], n = 1) %>%
    dplyr::ungroup()
  
  lineWidths <- rep(0.5,length(levels(dfPlot$yearSurvey)))
  if (lastIsPooled) lineWidths[length(lineWidths)] <- 1
  p <- ggplot(dfPlot, aes(x=.data[[x]], y=.data[[y]], color=yearSurvey, linewidth=yearSurvey))
  p <- p + geom_ribbon(aes(ymin=.data[[ymin]], ymax=.data[[ymax]], fill=yearSurvey), alpha=0.1, colour=NA)
  if (!is.null(dfObserved)) {
    df_ends <- dfObserved %>%
      filter(year == max(year))
    if (is.null(country)) {
      df_ends$label <- "National\nTFR"
    } else {
      df_ends$label <- paste0(country, "\nTFR")
    }
    p <- p + geom_line(data=dfObserved, aes(x=year, y=TFR), color="red", linewidth=2.5)
    df_ends <- df_ends %>% mutate(y_label = TFR + 0.5)
    p <- p +
      geom_segment(
        data = df_ends,
        aes(x=year, xend=year, y=TFR, yend=y_label),
        inherit.aes = FALSE,
        color = "red",
        linetype = "dashed"
      ) +
      geom_text(
        data = df_ends,
        aes(x=year, y=y_label, label=label),
        inherit.aes = FALSE,
        color = "red",
        size = 6,
        fontface = "bold",
        hjust = 0.5,
        vjust = 0
      )
  }
  
  p <- p + geom_line()
  
  colors <- hue_pal()(nSurveys)
  colors[nSurveys] <- "#000000"
  p <- p + scale_color_manual(name=NULL, values=colors)
  p <- p + scale_linewidth_manual(name=NULL, values=lineWidths)
  p <- p + scale_fill_manual(name=NULL, values=colors)
  
  # --- End labels (repelled to avoid overlap) ---
  p <- p + ggrepel::geom_label_repel(
    data        = df_line_ends,
    aes(x=.data[[x]], y=.data[[y]], label=yearSurvey, color=yearSurvey),
    linewidth=0.5,
    nudge_x     = 1,
    direction   = "y",
    hjust       = 0,
    segment.size = 0.3,
    show.legend = FALSE
  )
  
  p <- p + theme_minimal() + theme_bw() + theme_text(1.5)
  p <- p + labs(x=xTitle, y=yTitle)
  
  # --- Hide legend ---
  p <- p + theme(legend.position = "none")
  
  return(p + coord_cartesian(ylim=yLim))
}

#### Total fertility ####
##### Mexico #####
path_MEXICO_ENADID <- paste0(dataPath, "MEXICO_ENADID.Rdat")
load(file=path_MEXICO_ENADID)
# remove bad date of union
MEXICO_ENADID <- filterDateQuality (MEXICO_ENADID)

df_fert_MEX <- convert_ENADID_DHStype (MEXICO_ENADID)
df_fert_MEX <- cleanBH (df_fert_MEX)

# UN Population Prospects
TFR_Mex <- structure(list(
  year = 1975:2025,
  TFR = c(5.739337, 5.542503, 
          5.361235, 5.201952, 4.964136, 4.73899, 4.573655, 4.424913, 4.296177, 
          4.183615, 4.087398, 3.971162, 3.80263, 3.640279, 3.532133, 3.443583, 
          3.363117, 3.285287, 3.21142, 3.131734, 3.043559, 2.963915, 2.885847, 
          2.814643, 2.762953, 2.714001, 2.670764, 2.632673, 2.579335, 2.536202, 
          2.494697, 2.456018, 2.421225, 2.39073, 2.364095, 2.340178, 2.317411, 
          2.294256, 2.269204, 2.210554, 2.136778, 2.085608, 2.0406, 2.03, 
          2.02, 1.99, 1.97, 1.94, 1.91, 1.89, 1.87)),
  class = "data.frame", row.names = c(NA, -51L))

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

# using population weight, Poisson variance, loess smoothing
MEX_TFR_2w_P_1992 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1992",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_1997 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID1997",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2006 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2006",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2009 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2009",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2014 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2014",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2017 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2017",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2018 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2018",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2023 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="ENADID2023",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_2025 <- computeTFR_2weights(df_fert_MEX[df_fert_MEX$survey=="EDER2025",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=15, removeLastYear=TRUE)
MEX_TFR_2w_P_pooled <- computeTFR_2weights(df_fert_MEX, numLastYears=44, confidenceBand_loess=FALSE, orderPlus=5, removeLastYear=TRUE)

order <- 0
dfPlot_Mex_2w_P <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_smooth_min", "tfr_smooth_max"), pattern="MEX_TFR_2w_P_", order=order)
if (order == 0) {
  tfrSmoothPlot_2w_P <- plotTFR_mean (dfPlot_Mex_2w_P,
                                      x="year", y="tfr_smooth", ymin="tfr_smooth_min", ymax="tfr_smooth_max",
                                      xTitle="year", yTitle="TFR",
                                      dfObserved=TFR_Mex, country="MEXICO")
} else {
  tfrSmoothPlot_2w_P <- plotTFR_mean (dfPlot_Mex_2w_P,
                                      x="year", y="tfr_smooth", ymin="tfr_smooth_min", ymax="tfr_smooth_max",
                                      xTitle="year", yTitle="TFR", country="MEXICO")
}
tfrSmoothPlot_2w_P

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

##### USA #####
path_NSFG_ENADID <- paste0(dataPath, "NSFG_ENADID.Rdat")
load(file=path_NSFG_ENADID)
# remove bad date of union
NSFG_ENADID <- filterDateQuality (NSFG_ENADID)

df_fert_USA <- convert_ENADID_DHStype ( subset(NSFG_ENADID,!(survey %in% c("NSFG1973", "NSFG1976") )) )
df_fert_USA <- cleanBH (df_fert_USA)

# using population weight, Poisson variance, loess smoothing
#USA_TFR1973 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG1973",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, removeLastYear=TRUE)
#USA_TFR1976 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG1976",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, removeLastYear=TRUE)
USA_TFR1982 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG1982",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, numLastYears_toRemove=1)
USA_TFR1988 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG1988",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, numLastYears_toRemove=1)
USA_TFR1995 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG1995",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, numLastYears_toRemove=1)
USA_TFR2002 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2002",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=7, numLastYears_toRemove=1)
USA_TFR2006 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2006_10",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=12, numLastYears_toRemove=4)
USA_TFR2011 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2011_13",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=9, numLastYears_toRemove=2)
USA_TFR2013 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2013_15",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=9, numLastYears_toRemove=2)
USA_TFR2015 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2015_17",], orderPlus=5, confidenceBand_loess=FALSE, numLastYears=12, numLastYears_toRemove=2)
USA_TFR2017 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2017_19",], orderPlus=10, confidenceBand_loess=FALSE, numLastYears=12, numLastYears_toRemove=2)
USA_TFR2022 <- computeTFR_2weights(df_fert_USA[df_fert_USA$survey=="NSFG2022_23",], orderPlus=10, confidenceBand_loess=FALSE, numLastYears=11, numLastYears_toRemove=2)
USA_TFRpooled <- computeTFR_2weights(df_fert_USA, numLastYears=46, orderPlus=5, confidenceBand_loess=FALSE, numLastYears_toRemove=2)

#UN Population prospects
TFR_USA <- structure(list(
  year = 1978:2021,
  TFR = c(1.804295, 1.842943, 1.861143, 1.847063, 1.855037, 1.827088, 
          1.82674, 1.854495, 1.852466, 1.885172, 1.941965, 2.013895, 2.071944, 
          2.058829, 2.039379, 2.010094, 1.987981, 1.965016, 1.960261, 1.955789, 
          1.979864, 1.990404, 2.030073, 2.010819, 2.002778, 2.02495, 2.031377, 
          2.040237, 2.087032, 2.096203, 2.052786, 1.986704, 1.91557, 1.879428, 
          1.861815, 1.839564, 1.848337, 1.832191, 1.804146, 1.753184, 1.714576, 
          1.683919, 1.615593, 1.633919)),
  class = "data.frame", row.names = c(NA,-44L))


order <- 0
dfPlot_USA <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_smooth_min", "tfr_smooth_max"), pattern="USA_TFR", order=order)
if (order == 0) {
  tfrSmoothPlot <- plotTFR_mean (dfPlot_USA,
                                      x="year", y="tfr_smooth", ymin="tfr_smooth_min", ymax="tfr_smooth_max",
                                      xTitle="year", yTitle="TFR",
                                      dfObserved=TFR_USA, country="USA")
} else {
  tfrSmoothPlot <- plotTFR_mean (dfPlot_USA,
                                      x="year", y="tfr_smooth", ymin="tfr_smooth_min", ymax="tfr_smooth_max",
                                      xTitle="year", yTitle="TFR", country="USA")
}
tfrSmoothPlot

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

s#### union1 ==> sep1 ####
# Mexico
union1_sep1_Mex <- createUnionSep (df=subset(MEXICO_ENADID,!(survey %in% c("ENADID1992","ENADID2006"))))
rangeAge_Mex <- agesByYear (df=union1_sep1_Mex, varEvent="ySep1")

df_specificNumYears <-  buildSpecificYears(union1_sep1_Mex, "separation1", varEvent="ySep1")

plot_sep1_all_Mex <- plotBySurvey (df_toPlot=union1_sep1_Mex, varEnter="yUnion1", varEvent="ySep1", varWeight="weight",
                               res_countrySpecific_numYears=df_specificNumYears, ageTruncate = 40, mySpan=0.5, yTitle="probability of separation")
plot_sep1_all_Mex$plot

# Europe GGS & similar
# union1_sep1 <- createUnionSep (df=subset (GGS_ENADID, survey!="ENCoR"))
# rangeAge_GGS <- agesByYear (df=union1_sep1, varEvent="ySep1")

# merge NSFG surveys from GGS_ENADID
# us <- subset(GGS_ENADID, survey %in% c("NSFG1995", "NSFG2007"))
# NSFG_ENADID <- dplyr::bind_rows (us, NSFG_ENADID)

# USA
union1_sep1_USA <- createUnionSep (df=subset (NSFG_ENADID,!(survey %in% c("NSFG2017_19"))))
rangeAge_USA <- agesByYear (df=union1_sep1_USA, varEvent="ySep1")

df_specificNumYears <-  buildSpecificYears(union1_sep1_USA, "separation1", varEvent="ySep1")

plot_sep1_all_USA <- plotBySurvey (df_toPlot=union1_sep1_USA, varEnter="yUnion1", varEvent="ySep1", varWeight="weight",
                               res_countrySpecific_numYears=df_specificNumYears, ageTruncate = 40, mySpan=0.5, yTitle="probability of separation")
plot_sep1_all_USA$plot

# join Mexico and USA
union1_sep1 <- createUnionSep (df=subset(MEXICO_ENADID,!(survey %in% c("ENADID1992","ENADID2006"))))
union1_sep1 <- rbind (union1_sep1, createUnionSep (df=subset (NSFG_ENADID,!(survey %in% c("NSFG2017_19","NSFG2022_23")))))

# compute probability of the event (PPR)
res_Union1Sep1 <- calc_ppr(df=union1_sep1,
                           varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_numYears=68, ageTruncate = 40, mySpan=0.25)
# reduce the time span
res_Union1Sep1 <- subset(res_Union1Sep1, ((country=="MEXICO")&(year <= 2022))|((country=="USA")&(year <= 2016)))
res_Union1Sep1 <- subset(res_Union1Sep1, ((country=="MEXICO")&(year >= 1965))|((country=="USA")&(year >= 1968)))

plot_sep1 <- plot_ppr(df_res=res_Union1Sep1, vecCountry=NULL, yLimit=c(0, 1), yTitle="probability of separation", facet=FALSE)
plot_sep1 + theme(
  plot.title = element_text(size = 20, hjust=0.5),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 13),
  plot.caption = element_text(size = 10, face = "italic")
) + scale_x_continuous(breaks = seq(1960, 2020, by = 10))

plot_mean_sep1 <- plot_mean(df_res=res_Union1Sep1, vecCountry=NULL, yLimit=c(0, 15), yTitle="mean duration of union until separation", facet=FALSE)

#ageTruncate 45, for testing...
res_Union1Sep1_45 <- calc_ppr(df=union1_sep1,
                           varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_numYears=20, ageTruncate = 45, mySpan=0.25)
# reduce the time span
res_Union1Sep1_45 <- subset(res_Union1Sep1_45, ((country=="MEXICO")&(year <= 2022))|((country=="USA")&(year <= 2016)))
res_Union1Sep1_45$country <- ifelse(res_Union1Sep1_45$country=="MEXICO","MEXICO 45","USA 45")
res_Union1Sep1_45 <- rbind(res_Union1Sep1_45,res_Union1Sep1)

plot_sep1_45 <- plot_ppr(df_res=res_Union1Sep1_45, vecCountry=NULL, yLimit=c(0, 1), yTitle="probability of separation", facet=FALSE)
plot_sep1_45 + theme(
  plot.title = element_text(size = 20, hjust=0.5),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 13),
  plot.caption = element_text(size = 10, face = "italic")
) + scale_x_continuous(breaks = seq(1960, 2020, by = 10))

#### plot max age USA & Mexico ####
rangeAge <- rbind (rangeAge_Mex, rangeAge_USA)

# Calculate midpoint positions for labels
ageMaxMex <- rangeAge[(rangeAge$country == "MEXICO")&(rangeAge$year==1990), ]$ageMax
ageMaxUSA <- rangeAge[(rangeAge$country == "USA")&(rangeAge$year==1990), ]$ageMax
label_data <- data.frame(country=c("MEXICO", "USA"),year=c(1990,1990), ageMax=c(ageMaxMex, ageMaxUSA))

library(ggrepel)

pRange <- ggplot(rangeAge, aes(x=year, y=ageMax, color=country)) +
  geom_line() +
  geom_text_repel(data = label_data, 
                  aes(label = country),
                  size = 4,
                  direction = "y") +
  theme_bw() +
  xlab("year") + ylab("maximum age in dataset") +
  scale_color_manual(values=c("red","blue")) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "gray40", linewidth = 0.7)

pRange

# validate
df <- subset (union1_sep1, surveyName=="NSFG2006_10")
comparison_df <- validate_with_bootstrap (df, varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", 
                                    varWeight = "weight", n_bootstrap = 200)
  
#### birth ego -> first birth ####
# Mexico
birth_births_Mex <- createBirthBirths(MEXICO_ENADID)

df_specificNumYears <-  buildSpecificYears(birth_births_Mex, "fertility")

res_birth1_Mex <- plotBySurvey (df_toPlot=birth_births_Mex, varEnter="yBirth", varEvent="yBirth1", varWeight="weight",
                                res_countrySpecific_numYears=df_specificNumYears, res_finalYearsToDiscard=1, mySpan=0.5, yTitle="probability of birth", yLimit=c(0.6,1))
res_birth1_Mex$plot
view (res_birth1_Mex$results)

# USA
# first two surveys have no childless single women, so we exclude them
birth_births_USA <- createBirthBirths(subset(NSFG_ENADID, !(survey %in% c("NSFG1973", "NSFG1976"))))

df_specificNumYears <-  buildSpecificYears(birth_births_USA, "fertility")

res_birth1_USA <- plotBySurvey (df_toPlot=birth_births_USA, varEnter="yBirth", varEvent="yBirth1", varWeight="weight",
                                res_countrySpecific_numYears=df_specificNumYears, res_finalYearsToDiscard=1, mySpan=0.5, yTitle="probability of birth", yLimit=c(0.6,1))
res_birth1_USA$plot
view (res_birth1_USA$results)

# join Mexico and USA
birth_births_Mex_USA <- createBirthBirths(MEXICO_ENADID)
birth_births_Mex_USA <- rbind (birth_births, createBirthBirths(NSFG_ENADID))

res_BirthBirth1_Mex_USA <- calc_ppr(df=birth_births_Mex_USA,
                           varEnter="yBirth", varEvent="yBirth1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_finalYearsToDiscard=5, res_numYears=50, mySpan=0.25, duration=FALSE)

res_BirthBirth1_Mex_USA <- subset(res_BirthBirth1_Mex_USA, ((country=="MEXICO")&(year <= 2016))|((country=="USA")&(year <= 2016)))

plot_birth1 <- plot_ppr(df_res=res_BirthBirth1_Mex_USA, vecCountry=NULL, yLimit=c(0, 1), yTitle="probability of first birth", facet=FALSE)
plot_mean_birth1 <- plot_mean(df_res=res_BirthBirth1_Mex_USA, vecCountry=NULL, yLimit=c(15, 30), yTitle="mean age at first birth", facet=FALSE)


#### 10 first years of first children ####
##### first union only #####
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

# Partition the first `LifeLimit` months of a first child's life by the
# mother's union status at each moment, then plot weighted, loess-smoothed
# shares by the child's year of birth.
#
# secondUnions = FALSE (default) reproduces the original 3-way split:
#     Before union / During union / After union
# secondUnions = TRUE splits the post-first-union time further:
#     Before union / During first union / During second+ unions / Separated
#
# Allocation method
#   The child's observation window is [b, b + LifeLimit) in CMC months,
#   with b = cmc_birth1. Each union i contributes its overlap with that
#   window. Overlaps are disjoint and sum to LifeLimit, so the separation
#   total is just the residual: L - before - sum(during_i). This replaces
#   the old sequential capping, which could not preserve the chronological
#   ordering of separation periods between successive unions.
#
# Data requirement
#   This needs per-union start/end CMC columns, in chronological order,
#   passed via union_start_cols / union_end_cols. A union i that does not
#   exist for a woman must have an NA start. A missing END is treated as an
#   ongoing union (extends to the window end) -- see the NOTE below if a
#   non-last union can have a missing end in your data.

##### first four unions #####

plot10Years_child <- function(df,
                              LifeLimit        = 120, # default of 10 years
                              secondUnions     = FALSE,
                              union_start_cols = c("cmc_union1", "cmc_union2",
                                                   "cmc_union3", "cmc_union4"),
                              union_end_cols   = c("cmc_sep1",   "cmc_sep2",
                                                   "cmc_sep3",   "cmc_sep4"),
                              span             = 0.9,
                              smooth           = TRUE,
                              palette          = "cold",
                              palette_reverse  = FALSE,
                              label_year       = 1990,   # NULL -> median cohort
                              label_size       = 3.5) {
  
  stopifnot(length(union_start_cols) == length(union_end_cols))
  
  
  # ==== 1. Keep only fully observed windows ====
  t <- tNA (df, surveyName, firstBirthStatus)
  print (t)
  nChildren <- nrow (df)
  df <- subset(df, firstBirth_durLife_capped == LifeLimit)
  cat (nChildren - nrow(df), "children dropped due to incomplete first-child observation window\n")
  cat (nrow(df), "children kept\n")
  t <- tNA (df, surveyName, firstBirthStatus)
  print (t)
  
  if (!("yBirth1" %in% names(df))) df$yBirth1 <- cmc_to_year(df$cmc_birth1)
  
  
  # ==== 2. Allocate months by union status (interval intersection) ====
  
  b   <- df$cmc_birth1
  whi <- b + LifeLimit               # window end (exclusive), in CMC months
  
  # Overlap of an interval [lo, hi) with the window [b, whi), per row.
  overlap_win <- function(lo, hi) pmax(0, pmin(whi, hi) - pmax(b, lo))
  
  K      <- length(union_start_cols)
  during <- matrix(0, nrow = nrow(df), ncol = K)   # months "during union i"
  
  for (i in seq_len(K)) {
    s <- df[[union_start_cols[i]]]
    e <- df[[union_end_cols[i]]]
    # NOTE: NA end = ongoing/unknown -> extend to window end (counts as
    #   "during"). Safe for the LAST union; for a non-last union with a
    #   genuinely missing separation date this over-counts "during". Adjust
    #   upstream if your data has that case (your DHS misclassification bug).
    e <- ifelse(is.na(e), whi, e)
    d <- overlap_win(s, e)
    d[is.na(s)] <- 0                 # union i does not exist for this woman
    during[, i] <- d
  }
  
  # Before any union: window start up to the first union start.
  # No union at all (NA first start) -> entire window is "before".
  s1     <- df[[union_start_cols[1]]]
  before <- ifelse(is.na(s1), LifeLimit, pmax(0, pmin(whi, s1) - b))
  
  during1     <- during[, 1]
  during2plus <- if (K >= 2) rowSums(during[, -1, drop = FALSE]) else rep(0, nrow(df))
  
  
  # ==== 3. Build category set (depends on secondUnions) ====
  
  # NOTE: "Separated" now precedes "During second+ unions" in the level order,
  #   so it stacks (and appears in the legend) before second+ unions.
  if (secondUnions) {
    separation <- pmax(0, LifeLimit - before - during1 - during2plus)
    alloc <- tibble::tibble(
      yBirth1     = df$yBirth1,
      popWeight   = df$popWeight,
      before      = before,
      during1     = during1,
      separation  = separation,
      during2plus = during2plus
    )
    cat_levels <- c("before", "during1", "separation", "during2plus")
    cat_labels <- c("Before union", "During first union",
                    "Separated", "During second+ unions")
  } else {
    after <- pmax(0, LifeLimit - before - during1)   # everything post first union
    alloc <- tibble::tibble(
      yBirth1   = df$yBirth1,
      popWeight = df$popWeight,
      before    = before,
      during1   = during1,
      after     = after
    )
    cat_levels <- c("before", "during1", "after")
    cat_labels <- c("Before union", "During union", "After union")
  }
  
  
  # ==== 4. Weighted shares by year of birth ====
  
  shares <- alloc %>%
    dplyr::group_by(yBirth1) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cat_levels), ~ sum(.x * popWeight, na.rm = TRUE)),
      wpop = sum(popWeight, na.rm = TRUE),       # per-year smoothing weight
      .groups = "drop"
    ) %>%
    dplyr::mutate(tot = rowSums(dplyr::across(dplyr::all_of(cat_levels)))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cat_levels), ~ .x / tot)) %>%
    dplyr::select(-tot)
  
  
  # ==== 5. Log-ratio loess smoothing (shares stay positive, sum to 1) ====
  
  # Smooth the additive log-ratios of each category against a reference,
  # weighted by per-year population, then softmax back.
  #
  # NOTE: the reference is the category with the largest mean share, not the
  #   last level. This decouples the smoother from the plotting order, so
  #   reordering categories (e.g. Separated before second+ unions) cannot push
  #   a tiny, noisy category into the reference slot and destabilise the fit.
  smooth_shares <- function(data, cat_cols, x, w, span = 0.9, eps = 1e-6) {
    means  <- vapply(cat_cols, function(cc) mean(data[[cc]], na.rm = TRUE), numeric(1))
    ref    <- cat_cols[which.max(means)]
    others <- setdiff(cat_cols, ref)
    xx <- data[[x]]
    ww <- data[[w]]
    
    if (length(unique(xx)) < 5) return(data)   # too few cohorts to smooth
    
    s <- lapply(others, function(cc) {
      y   <- log(pmax(data[[cc]], eps) / pmax(data[[ref]], eps))
      fit <- loess(y ~ xx, weights = ww, span = span,
                   control = loess.control(surface = "direct"))
      as.numeric(predict(fit, newdata = data.frame(xx = xx)))
    })
    
    denom <- 1 + Reduce(`+`, lapply(s, exp))
    for (k in seq_along(others)) data[[others[k]]] <- exp(s[[k]]) / denom
    data[[ref]] <- 1 / denom
    data
  }
  
  if (smooth) {
    shares <- smooth_shares(shares, cat_levels, x = "yBirth1", w = "wpop", span = span)
  }
  
  
  # ==== 6. Stacked area plot with inline labels (no legend) ====
  
  fill_cols        <- make_fill(length(cat_levels), palette, palette_reverse)
  names(fill_cols) <- cat_labels
  
  # --- Label position: centre of each band at the chosen year of birth ---
  if (is.null(label_year)) label_year <- stats::median(shares$yBirth1)
  
  # Snap to the nearest available cohort to read off the band heights.
  ly    <- shares$yBirth1[which.min(abs(shares$yBirth1 - label_year))]
  props <- unlist(shares[shares$yBirth1 == ly, cat_levels][1, ], use.names = FALSE)
  
  # geom_area(position_stack(reverse = TRUE)) puts level 1 at the bottom and
  # stacks upward in cat_levels order, so cumulative sums give band tops.
  band_centres <- cumsum(props) - props / 2
  
  # White or black text per band, chosen from fill luminance (Rec. 601).
  lum      <- colSums(grDevices::col2rgb(fill_cols) * c(0.299, 0.587, 0.114))
  text_col <- ifelse(lum < 140, "white", "black")
  
  label_df <- tibble::tibble(
    yBirth1 = label_year,
    prop    = band_centres,
    label   = cat_labels,
    colour  = text_col
  )
  
  shares %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(cat_levels),
      names_to  = "regime",
      values_to = "prop"
    ) %>%
    dplyr::mutate(regime = factor(regime, levels = cat_levels, labels = cat_labels)) %>%
    ggplot2::ggplot(ggplot2::aes(x = yBirth1, y = prop, fill = regime)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      data        = label_df,
      mapping     = ggplot2::aes(x = yBirth1, y = prop, label = label, colour = colour),
      inherit.aes = FALSE,
      fontface    = "bold",
      lineheight  = 0.9,
      size        = label_size
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_continuous(breaks = seq(
      floor(min(shares$yBirth1)   / 10) * 10,
      ceiling(max(shares$yBirth1) / 10) * 10,
      by = 10
    )) +
    ggplot2::scale_fill_manual(values = fill_cols) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(x = "Year of birth", y = "Proportion") +
    ggplot2::theme(legend.position = "none")
}

# ==== Colour palette helper ====

# Build `n` ordered fill colours by interpolating between anchor colours.
# `palette` is either a preset name (below) or your own vector of 2+ colours.
# `reverse` flips the direction (e.g. dark -> light for the mono ramps).
#
# Multi-hue (good for "hot to cold" style transitions):
#   "hot"       yellow -> red          (warm heat ramp)
#   "cold"      light  -> dark blue    (cool ramp)
#   "hotcold"   red -> white -> blue   (diverging, warm to cool)
#   "redyellow" pale yellow -> dark red
#
# Mono-hue (single colour, light -> dark):
#   "blues" "reds" "greens" "purples" "oranges" "greys"
make_fill <- function(n, palette = "hot", reverse = FALSE) {
  presets <- list(
    hot       = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#B10026"),
    cold      = c("#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    hotcold   = c("#67001F", "#D6604D", "#FDDBC7", "#D1E5F0", "#4393C3", "#053061"),
    redyellow = c("#FFFFCC", "#FFEDA0", "#FED976", "#FD8D3C", "#E31A1C", "#800026"),
    blues     = c("#F7FBFF", "#6BAED6", "#08306B"),
    reds      = c("#FFF5F0", "#FB6A4A", "#67000D"),
    greens    = c("#F7FCF5", "#74C476", "#00441B"),
    purples   = c("#FCFBFD", "#9E9AC8", "#3F007D"),
    oranges   = c("#FFF5EB", "#FD8D3C", "#7F2704"),
    greys     = c("#FFFFFF", "#969696", "#252525")
  )
  
  anchors <- if (length(palette) > 1) palette else presets[[palette]]
  if (is.null(anchors)) {
    stop("Unknown palette '", palette, "'. Use one of: ",
         paste(names(presets), collapse = ", "),
         " - or pass a vector of 2+ colours.")
  }
  if (reverse) anchors <- rev(anchors)
  
  grDevices::colorRampPalette(anchors)(n)
}


#### status of first birth ####
##### Mexico #####
Mex_firstBirthStatus_union1 <- subset(MEXICO_ENADID,!(survey %in% c("WFS","ENADID1992","ENADID2006")))
fBirthStatus_Mex_union1 <- createFirstUnionFirstBirth (Mex_firstBirthStatus_union1)
cat (nrow(Mex_firstBirthStatus_union1)-nrow(fBirthStatus_Mex_union1), "children dropped\n")
Mex_firstBirthStatus_union1_4 <- subset(MEXICO_ENADID,!(survey %in% c("WFS","ENADID1992","ENADID2006")))
fBirthStatus_Mex_union1_4 <- createFirstUnionFirstBirth (Mex_firstBirthStatus_union1_4)
cat (nrow(Mex_firstBirthStatus_union1_4)-nrow(fBirthStatus_Mex_union1_4), "children dropped\n")

ggplot_fill_panel(plot10Years_child(fBirthStatus_Mex_union1)) + theme_text()
pMex <- ggplot_fill_panel(plot10Years_child(fBirthStatus_Mex_union1_4,secondUnions = TRUE)) + theme_text()

##### USA #####
fBirthStatus_USA_union <- createFirstUnionFirstBirth (subset(NSFG_ENADID,!(survey %in% c("NSFG2017_19"))))

ggplot_fill_panel(plot10Years_child(fBirthStatus_USA_union)) + theme_text() + theme_noGrid()
pUSA <- ggplot_fill_panel(plot10Years_child(fBirthStatus_USA_union,secondUnions = TRUE)) + theme_text() + theme_noGrid("none")

##### combining Mexico and the USA #####
library(patchwork)

# Combine plots side-by-side
combined_plot <-
  (pMex + labs(title="MEXICO")) +
  (pUSA + labs(title="USA") + labs(y = NULL))

# Apply font changes to EVERYTHING at once
MEX_USA_lifeChildren_plot <- combined_plot + 
  plot_annotation(
    #title = "Transition of first union to marriage, by union cohort",
    title = NULL,
    caption = "Source: INEGI/ENADID-EDER and CDC/NSFG microdata",
    theme = theme(
      # Styling ONLY the NEW Main Title
      plot.title = element_text(size = 24, family = "serif", face = "bold")
    )    ) &
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )

pathFile <- paste0(path_output_plots,"MEX_USA_lifeChildren_plot.pdf")
ggsave(filename = pathFile, plot = MEX_USA_lifeChildren_plot, width = 29.7, height = 21, units = "cm", dpi = 300)

# ==== Children born in mother's FIRST union, intact to age 10, by country ====

# ==== Shared classifier: WHY a first child did not have a full-union first 10 years ====
#
# Reconstructs the original fullUnion indicator (born_in_union1 & union1_to_10)
# but decomposed into an exhaustive `reason` factor, so the two (or three, with
# motive) factors that pull the fullUnion proportion down are visible instead
# of collapsing straight to 0/1:
#
#   full_union            born at/after union1 start, union1 still intact at
#                          the child's LifeLimit-th month (the old "1")
#   born_before_union     child was born BEFORE union1 started
#   no_union               mother never entered a (first) union
#   separated_widowhood    union1 ended (widowhood) before the child's LifeLimit
#   separated_other        union1 ended (separation, or motive unknown) before
#                          the child's LifeLimit
#
# NOTE: born_before_union and no_union were already counted as failures in the
# original plotFullUnion_child (born_in_union1 was FALSE for them, so they
# were never excluded from the denominator) -- this only makes that visible
# and lets you exclude them via includeBornBefore if you want the narrower
# "children born during the first union" population instead.
.classifyFullUnionReason <- function(df,
                                     LifeLimit    = 120,
                                     union1_start = "cmc_union1",
                                     union1_end   = "cmc_sep1",
                                     motive_col   = "sep1_motive") {

  df <- subset(df, firstBirth_durLife_capped == LifeLimit)
  if (!("yBirth1" %in% names(df))) df$yBirth1 <- cmc_to_year(df$cmc_birth1)

  b    <- df$cmc_birth1
  whi  <- b + LifeLimit               # window end (exclusive), in CMC months
  u1   <- df[[union1_start]]          # first union start
  sep1 <- df[[union1_end]]            # first union end (NA = ongoing)
  motive <- if (motive_col %in% names(df)) as.character(df[[motive_col]]) else NA_character_

  df$reason <- dplyr::case_when(
    is.na(u1)                       ~ "no_union",
    b < u1                          ~ "born_before_union",
    is.na(sep1) | sep1 >= whi       ~ "full_union",
    !is.na(motive) & motive == "widowhood" ~ "separated_widowhood",
    .default                        = "separated_other"
  )
  df$reason <- factor(df$reason, levels = c("full_union", "born_before_union", "no_union",
                                            "separated_widowhood", "separated_other"))
  df$fullUnion <- as.integer(df$reason == "full_union")

  df
}

# Row-level reason breakdown (no plot), for whoever wants the raw data --
# e.g. to tabulate, or feed a different chart than plotFullUnion_reasons_child().
# includeBornBefore = FALSE restricts to children actually born during union1
# (drops "born_before_union" and "no_union"), i.e. the narrower population.
fullUnion_reasons <- function(df,
                              LifeLimit         = 120,
                              country_col       = "country",
                              union1_start      = "cmc_union1",
                              union1_end        = "cmc_sep1",
                              motive_col        = "sep1_motive",
                              includeBornBefore = TRUE) {

  df <- .classifyFullUnionReason(df, LifeLimit, union1_start, union1_end, motive_col)
  df$country <- df[[country_col]]

  if (!isTRUE(includeBornBefore)) {
    df <- subset(df, !(reason %in% c("born_before_union", "no_union")))
  }

  df
}

# Proportion of FIRST children whose mother's FIRST union (a) had already begun
# at the child's birth and (b) was still ongoing when the child reached
# `LifeLimit` months (default 120 = 10 years). Plotted as one line per country.
#
# Built on `cmc_birth1` (the mother's first birth, one row per mother), so the
# "first child" condition holds by construction. Expects the merged Mexico +
# USA data with a `country` column (name set via `country_col`).
#
# includeBornBefore = TRUE (default) reproduces the original numbers exactly:
# children born before union1, or to a mother with no union, stay in the
# denominator as automatic failures. Set to FALSE to restrict the population
# to children actually born during (or after) union1 -- see fullUnion_reasons()
# for the row-level data, or plotFullUnion_reasons_child() for a breakdown of
# WHY the proportion falls short of 100%.
plotFullUnion_child <- function(df,
                                LifeLimit    = 120,
                                country_col  = "country",
                                union1_start = "cmc_union1",
                                union1_end   = "cmc_sep1",
                                span         = 0.9,
                                smooth       = TRUE,
                                colours      = NULL,   # named/ordered vector
                                includeBornBefore = TRUE) {

  df <- fullUnion_reasons(df, LifeLimit, country_col, union1_start, union1_end,
                          motive_col = "sep1_motive", includeBornBefore = includeBornBefore)


  # ==== 3. Weighted proportion by year of birth and country ====

  prop <- df %>%
    dplyr::group_by(country, yBirth1) %>%
    dplyr::summarise(
      p    = stats::weighted.mean(fullUnion, popWeight, na.rm = TRUE),
      wpop = sum(popWeight, na.rm = TRUE),       # per-year smoothing weight
      .groups = "drop"
    )
  
  
  # ==== 4. Smooth each country's series in logit space (stays in 0-1) ====
  
  if (smooth) {
    prop <- prop %>%
      dplyr::group_by(country) %>%
      dplyr::group_modify(~ {
        d <- .x
        if (length(unique(d$yBirth1)) >= 5) {
          eps <- 1e-6
          y   <- stats::qlogis(pmin(pmax(d$p, eps), 1 - eps))
          fit <- loess(y ~ yBirth1, data = d, weights = d$wpop, span = span,
                       control = loess.control(surface = "direct"))
          d$p <- stats::plogis(
            as.numeric(predict(fit, newdata = data.frame(yBirth1 = d$yBirth1)))
          )
        }
        d
      }) %>%
      dplyr::ungroup()
  }
  
  
  # ==== 5. Line plot, one line per country ====
  
  p <- ggplot2::ggplot(prop, ggplot2::aes(x = yBirth1, y = p, colour = country)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = seq(
      floor(min(prop$yBirth1)   / 10) * 10,
      ceiling(max(prop$yBirth1) / 10) * 10,
      by = 10
    )) +
    ggplot2::labs(x = "Year of birth",
                  y = "Proportion",
                  colour = NULL)
  
  if (!is.null(colours)) p <- p + ggplot2::scale_colour_manual(values = colours)
  p
}

# 100%-stacked-area decomposition of WHY first children fall short of a full
# intact first-union first 10 years: the complement of plotFullUnion_child()'s
# single line, split into born_before_union / no_union / separated (optionally
# split by widowhood vs separation) -- these bands plus "full_union" sum to 1
# at every year, so the shrinking "full_union" band matches plotFullUnion_child()
# and the bands above it show what's pulling it down.
#
# splitMotive = FALSE (default) collapses separated_widowhood/separated_other
# into one "separated_before_10" band. Set TRUE for the widowhood-vs-separation
# bonus breakdown.
plotFullUnion_reasons_child <- function(df,
                                        LifeLimit         = 120,
                                        country_col       = "country",
                                        union1_start      = "cmc_union1",
                                        union1_end        = "cmc_sep1",
                                        motive_col        = "sep1_motive",
                                        includeBornBefore = TRUE,
                                        splitMotive       = FALSE,
                                        facetCountry      = TRUE) {

  df <- fullUnion_reasons(df, LifeLimit, country_col, union1_start, union1_end,
                          motive_col, includeBornBefore)

  if (!isTRUE(splitMotive)) {
    df$reason <- forcats::fct_collapse(
      df$reason,
      separated_before_10 = c("separated_widowhood", "separated_other"))
  }

  labs_map <- c(full_union           = "Full union to age 10",
               born_before_union    = "Born before union",
               no_union              = "No union",
               separated_before_10   = "Separated before age 10",
               separated_widowhood   = "Widowed before age 10",
               separated_other       = "Separated before age 10")

  shares <- df %>%
    dplyr::group_by(country, yBirth1, reason) %>%
    dplyr::summarise(w = sum(popWeight, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(country, yBirth1, reason, fill = list(w = 0)) %>%   # every
    # (country, year, reason) combo present, so no gaps in the stacked area
    # for a reason that happens to be absent in a given cohort-year
    dplyr::group_by(country, yBirth1) %>%
    dplyr::mutate(prop = w / sum(w)) %>%
    dplyr::ungroup()

  presentLevels <- levels(droplevels(df$reason))
  shares$reasonLabel <- factor(labs_map[as.character(shares$reason)],
                               levels = unique(labs_map[presentLevels]))

  p <- ggplot2::ggplot(shares, ggplot2::aes(x = yBirth1, y = prop, fill = reasonLabel)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_continuous(breaks = seq(
      floor(min(shares$yBirth1)   / 10) * 10,
      ceiling(max(shares$yBirth1) / 10) * 10,
      by = 10
    )) +
    ggplot2::labs(x = "Year of birth", y = "Share", fill = NULL)

  if (isTRUE(facetCountry) && length(unique(shares$country)) > 1) {
    p <- p + ggplot2::facet_wrap(~country)
  }

  p
}

##### Mexico and USA #####
fBirthStatus_Both_union <- rbind(fBirthStatus_Mex_union1, fBirthStatus_USA_union)
(ggplot_fill_panel(plotFullUnion_child(fBirthStatus_Both_union, colours = c("MEXICO" = "red", "USA" = "blue")), top=0.1) +
  theme_text() + theme_noGrid()) %>% ggplot_set_scale(breaks=seq(0,1,0.1))

rm(Mex_firstBirthStatus_union1)
rm(Mex_firstBirthStatus_union1_4)
rm(fBirthStatus_USA_union)

