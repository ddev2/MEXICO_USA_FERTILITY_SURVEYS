setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
# Read harmonized GGS (and others) files
library (tidyverse)
library (haven)
library (purrr)
source (path.expand("~/Dropbox/RStudioData/TransitionsPPR/KaplanMeierLib.R"))
path_plots <- path.expand("~/My Drive (ddevolder@ced.uab.es)/Pachuca/INEGI/Encuestas/USA-MEXICO/")

pathColDHS <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Angela/Trabajo DANE 2022/DHS")
source(paste0(pathColDHS,"/DHS_lib.r"))

path_MEXICO_ENADID <- paste0(rootPath, "/INEGI/Encuestas/ENADID/MEXICO_ENADID.Rdat")
load(file=path_MEXICO_ENADID)

convert_ENADID_DHStype <- function(dfIN=NULL) {
  # create a data.frame with the fields necessaries to compute TFR and PPRs using code for DHS
  dfOUT <- dfIN[,c("country", "survey", "surveyDate_cmc", "indiv_dob_cmc", "yBirth", "indiv_weight", "nBioKids")]
  names(dfOUT) <- c("country", "survey", "cmcSurvey", "cmcBirthEgo", "yBirth", "indiv_weight", "nBirthsTot")
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

birthSeries <- function (df=df_fert) {
  maxB <- max(df$nBirthsTot)
  ftab1 <- table(df$survey,df$yBirthChild1)
}
prepareDataPlotGen <- function(keeps=NULL, ind=1, pattern="MEX_TFR") {
  # Takes dataframe in memory with names starting with pattern, and creates a dataframe for plotting
  # ind=1 corresponds to TFR Total
  # ind=2 corresponds to TFR1
  # ...
  # ind=6 corresponds to TFR5+
  dfPattern = paste0("^", pattern)
  tfr_names <- ls(pattern=dfPattern, envir = .GlobalEnv)
  firstList <- get(tfr_names[1])
  if (is.null(keeps) | !all(keeps %in% colnames(firstList[[ind]]))) stop ("Keeps is null or bad")
  
  dfPlot <- data.frame()
  for (i in (1:length(tfr_names))) {
    aList <- get(tfr_names[i])
    data <- aList[[ind]][keeps]
    data$yearSurvey <- substring(tfr_names[i], nchar(pattern)+1)
    dfPlot <- rbind(dfPlot, data)
  }
  return (dfPlot)
}

plotTFR_mean <- function(dfPlot,
                         x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max",
                         xTitle="year", yTitle="TFR", yLim=NULL,
                         dfObserved=NULL, country=NULL) {
  require (ggrepel)
  nSurveys <- length(table(dfPlot$yearSurvey))
  dfPlot$yearSurvey <- factor(dfPlot$yearSurvey, levels=unique(dfPlot$yearSurvey))
  
  p <- ggplot(dfPlot, aes(x=.data [[x]], y=.data [[y]], color=yearSurvey))
  p <- p + geom_ribbon (aes(ymin=.data [[ymin]], ymax=.data [[ymax]], fill=yearSurvey), alpha=0.2, colour=NA)
  if (!is.null(dfObserved)) {
    df_ends <- dfObserved %>% 
      filter(year == max(year))
    if (is.null(country)) {
      df_ends$label <- "National\nTFR"
    } else {
      df_ends$label <- paste0(country,"\nTFR")
    }
    p <- p + geom_line(data=dfObserved, aes(x=year, y=TFR),color="red",linewidth=2.5)
    df_ends <- df_ends |> mutate(y_label = TFR + 0.5)  # adjust 0.1 to taste

    p <- p + 
      geom_segment(
        data = df_ends,
        aes(x = year, xend = year, y = TFR, yend = y_label),
        inherit.aes = FALSE,
        color = "red",
        linetype = "dashed"
      ) +
      geom_text(
        data = df_ends,
        aes(x = year, y = y_label, label = label),
        inherit.aes = FALSE,
        color = "red",
        size = 6,
        fontface = "bold",
        hjust = 0.5,  # centered above the segment
        vjust = 0     # sits just above the segment end
      )
    }
  p <- p + geom_line()
  colors <- hue_pal()(nSurveys) 
  colors[nSurveys] <- "#000000" # pooled
  p <- p + scale_color_manual(name=NULL,values=colors)
  p <- p + scale_fill_manual(name=NULL,values=colors)
  p <- p + theme_minimal() + theme_bw() + theme_text(1.5)
  p <- p + labs(x=xTitle, y=yTitle)
  return (p + coord_cartesian(ylim=yLim))
}

#### Total fertility ####
##### Mexico #####
df_fert_MEX <- convert_ENADID_DHStype (MEXICO_ENADID)
df_fert_MEX <- reweight(df_fert_MEX)
df_fert_MEX <- cleanBH (df_fert_MEX)

MEX_TFR1976 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="WFS",], orderPlus=5, numLastYears=10, removeLastYear=TRUE)
MEX_TFR1992 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID1992",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR1997 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID1997",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2006 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2006",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2009 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2009",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2014 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2014",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2017 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="EDER2017",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2018 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2018",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFR2023 <- computeTFR(df_fert_MEX[df_fert_MEX$survey=="ENADID2023",], orderPlus=5, numLastYears=15, removeLastYear=TRUE)
MEX_TFRpooled <- computeTFR(df_fert_MEX, numLastYears=44, orderPlus=5, removeLastYear=TRUE)
rm(MEX_TFR1976)

# UN Population Prospects
TFR_Mex <- structure(list(
  year = 1975:2023,
  TFR = c(5.739337, 5.542503, 
          5.361235, 5.201952, 4.964136, 4.73899, 4.573655, 4.424913, 4.296177, 
          4.183615, 4.087398, 3.971162, 3.80263, 3.640279, 3.532133, 3.443583, 
          3.363117, 3.285287, 3.21142, 3.131734, 3.043559, 2.963915, 2.885847, 
          2.814643, 2.762953, 2.714001, 2.670764, 2.632673, 2.579335, 2.536202, 
          2.494697, 2.456018, 2.421225, 2.39073, 2.364095, 2.340178, 2.317411, 
          2.294256, 2.269204, 2.210554, 2.136778, 2.085608, 2.0406, 2.03, 
          2.02, 1.99, 1.97, 1.94, 1.91)),
  class = "data.frame", row.names = c(NA, -49L))

dfPlot_Mex <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_min", "tfr_max"), pattern="MEX_TFR")
tfrSmoothPlot <- plotTFR_mean (dfPlot_Mex,
                               x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max",
                               xTitle="year", yTitle="TFR",
                               dfObserved=TFR_Mex, country="MEXICO")
tfrSmoothPlot

##### USA #####
df_fert_USA <- convert_ENADID_DHStype ( NSFG_ENADID )
df_fert_USA <- convert_ENADID_DHStype ( subset(NSFG_ENADID,!(survey %in% c("NSFG1973", "NSFG1976") )) )
df_fert_USA <- reweight(df_fert_USA)
df_fert_USA <- cleanBH (df_fert_USA)

#USA_TFR1973 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG1973",], orderPlus=5, numLastYears=7, removeLastYear=TRUE)
#USA_TFR1976 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG1976",], orderPlus=5, numLastYears=7, removeLastYear=TRUE)
USA_TFR1982 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG1982",], orderPlus=5, numLastYears=7, numLastYears_toRemove=1)
USA_TFR1988 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG1988",], orderPlus=5, numLastYears=7, numLastYears_toRemove=1)
USA_TFR1995 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG1995",], orderPlus=5, numLastYears=7, numLastYears_toRemove=1)
USA_TFR2002 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2002",], orderPlus=5, numLastYears=7, numLastYears_toRemove=1)
USA_TFR2006 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2006_10",], orderPlus=5, numLastYears=12, numLastYears_toRemove=4)
USA_TFR2011 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2011_13",], orderPlus=5, numLastYears=9, numLastYears_toRemove=2)
USA_TFR2013 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2013_15",], orderPlus=5, numLastYears=9, numLastYears_toRemove=2)
USA_TFR2015 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2015_17",], orderPlus=5, numLastYears=12, numLastYears_toRemove=2)
USA_TFR2017 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2017_19",], orderPlus=10, numLastYears=12, numLastYears_toRemove=2)
USA_TFR2022 <- computeTFR(df_fert_USA[df_fert_USA$survey=="NSFG2022_23",], orderPlus=10, numLastYears=11, numLastYears_toRemove=2)
USA_TFRpooled <- computeTFR(df_fert_USA, numLastYears=46, orderPlus=5, numLastYears_toRemove=2)

TFR_USA <- structure(list(
  year = 1975:2023,
  TFR = c(1.814622, 1.785184, 
          1.826474, 1.804295, 1.842943, 1.861143, 1.847063, 1.855037, 1.827088, 
          1.82674, 1.854495, 1.852466, 1.885172, 1.941965, 2.013895, 2.071944, 
          2.058829, 2.039379, 2.010094, 1.987981, 1.965016, 1.960261, 1.955789, 
          1.979864, 1.990404, 2.030073, 2.010819, 2.002778, 2.02495, 2.031377, 
          2.040237, 2.087032, 2.096203, 2.052786, 1.986704, 1.91557, 1.879428, 
          1.861815, 1.839564, 1.848337, 1.832191, 1.804146, 1.753184, 1.714576, 
          1.683919, 1.615593, 1.633919, 1.665, 1.623609)),
  class = "data.frame", row.names = c(NA,-49L))

dfPlot_USA <- prepareDataPlotGen (keeps=c("year", "tfr_smooth", "tfr_min", "tfr_max"), pattern="USA_TFR")
tfrSmoothPlot <- plotTFR_mean (dfPlot_USA,
                               x="year", y="tfr_smooth", ymin="tfr_min", ymax="tfr_max",
                               xTitle="year", yTitle="TFR",
                               dfObserved = TFR_USA, country="USA")
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
union1_sep1 <- createUnionSep (df=subset(MEXICO_ENADID,!(survey %in% c("ENADID1992","ENADID2006"))))
union1_sep1 <- createUnionSep (df=subset(MEXICO_ENADID,!(survey %in% c("ENADID1992","ENADID2006","EDER2017"))))
rangeAge_Mex <- agesByYear (df=union1_sep1, varEvent="ySep1")

# Europe GGS & similar
union1_sep1 <- createUnionSep (df=subset (GGS_ENADID, survey!="ENCoR"))
rangeAge_GGS <- agesByYear (df=union1_sep1, varEvent="ySep1")

# merge NSFG surveys from GGS_ENADID
# us <- subset(GGS_ENADID, survey %in% c("NSFG1995", "NSFG2007"))
# NSFG_ENADID <- dplyr::bind_rows (us, NSFG_ENADID)

# USA
union1_sep1 <- createUnionSep (df=subset (NSFG_ENADID,!(survey %in% c("NSFG2017_19"))))
rangeAge_USA <- agesByYear (df=union1_sep1, varEvent="ySep1")

df_specificNumYears <-  buildSpecificYears(union1_sep1, "separation1", varEvent="ySep1")

plot_sep1_all <- plotBySurvey (df_toPlot=union1_sep1, varEnter="yUnion1", varEvent="ySep1", varWeight="weight",
                               res_countrySpecific_numYears=df_specificNumYears, ageTruncate = 40, mySpan=0.5, yTitle="probability of separation")
plot_sep1_all$plot

# join Mexico and USA
union1_sep1 <- createUnionSep (df=subset(MEXICO_ENADID,!(survey %in% c("ENADID1992","ENADID2006","EDER2017"))))
union1_sep1 <- rbind (union1_sep1, createUnionSep (df=subset (NSFG_ENADID,!(survey %in% c("NSFG2017_19")))))

# compute probability of the event (PPR)
res_Union1Sep1 <- calc_ppr(df=union1_sep1,
                           varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_numYears=68, ageTruncate = 40, mySpan=0.25)
res_Union1Sep1 <- calc_ppr(df=union1_sep1,
                           varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_numYears=20, ageTruncate = 45, mySpan=0.25)

plot_sep1 <- plot_ppr(df_res=res_Union1Sep1, vecCountry=NULL, yLimit=c(0, 1), yTitle="probability of separation", facet=FALSE)
plot_sep1 + theme(
  plot.title = element_text(size = 20, hjust=0.5),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 13),
  plot.caption = element_text(size = 10, face = "italic")
)

plot_mean_sep1 <- plot_mean(df_res=res_Union1Sep1, vecCountry=NULL, yLimit=c(0, 15), yTitle="mean duration of union until separation", facet=FALSE)

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
  xlab("year") + ylab("maximum age") +
  scale_color_manual(values=c("red","blue")) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 10, face = "italic")
  )


# validate
df <- subset (union1_sep1, surveyName=="NSFG2006_10")
comparison_df <- validate_with_bootstrap (df, varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", 
                                    varWeight = "weight", n_bootstrap = 200)
  
#### birth ego -> first birth ####
# Mexico
birth_births <- createBirthBirths(MEXICO_ENADID)
# USA
birth_births <- createBirthBirths(NSFG_ENADID)
# join Mexico and USA
birth_births <- createBirthBirths(MEXICO_ENADID)
birth_births <- rbind (birth_births, createBirthBirths(NSFG_ENADID))

df_specificNumYears <-  buildSpecificYears(birth_births, "fertility")

res_birth1_all <- plotBySurvey (df_toPlot=birth_births, varEnter="yBirth", varEvent="yBirth1", varWeight="weight",
                               res_countrySpecific_numYears=df_specificNumYears, res_finalYearsToDiscard=3, mySpan=0.5, yTitle="probability of birth", yLimit=c(0.6,1))
res_birth1_all$plot
res_birth1_all$results

res_BirthBirth1 <- calc_ppr(df=birth_births,
                           varEnter="yBirth", varEvent="yBirth1", varCens="cmc_survey", varCountry="country", vecCountry=NULL,
                           varWeight="weight", res_finalYearsToDiscard=5, res_numYears=50, mySpan=0.25, duration=FALSE)

plot_birth1 <- plot_ppr(df_res=res_BirthBirth1, vecCountry=NULL, yLimit=c(0, 1), yTitle="probability of first birth", facet=FALSE)
plot_mean_birth1 <- plot_mean(df_res=res_BirthBirth1, vecCountry=NULL, yLimit=c(15, 30), yTitle="mean age at first birth", facet=FALSE)
