library(tidyverse)
library2 ("scam")
library(ggrepel)
library(scales)
library(haven)

yearFrom_cmc <- function (cmc) {
  return (1900+trunc((cmc-1)/12))
}
monthFrom_cmc <- function (cmc) {
  return (cmc-trunc((cmc-1)/12)*12)
}
calc_cmc <- function (month, year) {
  return ((year-1900)*12+month)
}

do_useRelativeWeights <- function (df=NULL, varWeight=NULL) {
  library(dplyr)
  # we standardize the weights by the mean, so that the sum of weights is equal to the number of cases
  # we take care of separating by country and survey
  if (nrow(df)==0) return()
  if ("country" %in% names (df)) {
    countries <- names (table(df$country))
    deleteCountry <- FALSE
  } else {
    df$country <- "all"
    countries <- c("all")
    deleteCountry <- TRUE
  }
  if ("surveyName" %in% names (df)) {
    df <- dplyr::rename(df, survey=surveyName)
    renameSurvey <- TRUE
  } else {
    renameSurvey <- FALSE
  }
  deleteSurvey <- FALSE
  
  df2 <- data.frame()
  for (country in countries) {
    dfCountry <- subset (df, df$country==country)
    dfCountry$survey <- factor(dfCountry$survey)
    if ("survey" %in% names (dfCountry)) {
      surveys <- names (table(dfCountry$survey))
      deleteSurvey <- FALSE
    } else {
      dfCountry$survey <- "all"
      surveys <- c("all")
      deleteSurvey <- TRUE
    }
    for (survey in surveys) {
      mWeight <- mean(dfCountry[dfCountry$survey==survey, varWeight])
      if (is.na(mWeight)) stop (paste0("weights bad in survey: ", survey, ", for :", country))
      dfCountry[dfCountry$survey==survey, varWeight] <- dfCountry[dfCountry$survey==survey, varWeight] / mWeight
    }
    df2 <- rbind(df2, dfCountry)
  }
  rm (df)
  
  if (isTRUE(deleteSurvey)) df2$survey <- NULL
  if (isTRUE(deleteCountry)) df2$country <- NULL
  if (isTRUE(renameSurvey)) df2 <- dplyr::rename(df2, surveyName=survey)
  
  return (df2)
}
KaplanMeier <- function (df_KM=NULL, varEnter=NULL, varEvent=NULL, varCens=NULL, varWeight=NULL, varEvent2=NULL, truncate=10) {
  #==> df_KM: data.frame with the dataset
  #==> varEnter: name of the column with the starting dates of being at risk of events (from example date of birth of individuals)
  #==> varEvent: name of the column with the dates of the event
  #==> varCens: name of the column with the dates at censoring
  #ALL THE DATES SHOULD BE IN CMC FORMAT, WITH 1/1/1900 AS STARTING DAY
  #==> varWeight: name of column with weights (optional: if not NULL, then we will use them)
  #==> varEvent2: name of the column with the dates of the second event (optional)
  #   If there are two events, then we will try to built a mirrored Kaplan&Meier (Billari, 2001)
  #<== return a table with Kaplan&Meier columns
  
  if (exists("DEBUG2") && isTRUE(DEBUG2)) browser()
  
  if (is.null (df_KM)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")
  
  if (!is.null(varWeight)) {
    df_KM[,varWeight] <- ifelse(is.na(df_KM[,varWeight]),1,df_KM[,varWeight])
  }
  
  computeKM <- function (sz=NULL, vecEnter=NULL, vecEvent=NULL, vecCens=NULL, vecWeight=NULL, mirror=FALSE, truncate=10) {
    # ==> truncate: hide the tail when the total number of remaining events is less then 10
    if (exists("DEBUG3") && isTRUE(DEBUG3)) browser()
    
    useWeights <- !is.null(vecWeight)
    data <- data.frame(time=(1:sz), event=(1:sz))
    data$time <- ifelse(is.na(vecEvent), vecCens-vecEnter, vecEvent-vecEnter)
    if (useWeights) {
      data$event <- ifelse(is.na(vecEvent), 0, vecWeight)
      data$eventRaw <- ifelse(is.na(vecEvent), 0, 1)
      data$number <- vecWeight
    } else {
      data$event <- ifelse(is.na(vecEvent), 0, 1)
      data$eventRaw <- ifelse(is.na(vecEvent), 0, 1)
      data$number <- 1
    }
    #count the number of unweighted cases, for Loess smoothing...
    data$numberRaw <- 1
    data <- data[order(data$time),]
    data <- aggregate(data, by = list(data$time), FUN = sum)
    data$time <- NULL
    colnames(data)[1] <- "time"
    data$surv <- sum(data$number)
    data$surv[2:length(data$surv)] <- data$surv[2:length(data$surv)] - cumsum (data$number[1:(length(data$surv)-1)])
    data$rate <- data$event / data$surv
    data$rate <- ifelse((data$rate > 0.95)&(data$event < 3), 0.5, data$rate) # avoid case of few events and rate equal 1
    data$survFunction <- 1
    for (i in (2:length(data$surv))) {
      data$survFunction[i] = data$survFunction[i-1] * (1 - data$rate[i-1])
    }
    data$variance <- data$event / (data$surv * (data$surv - data$event))
    data$variance <- cumsum (data$variance)
    data$variance <- data$survFunction * data$survFunction * data$variance
    data$stdErr <- sqrt(data$variance)
    data$confIntMax <- data$survFunction + 1.96 * data$stdErr
    data$confIntMin <- data$survFunction - 1.96 * data$stdErr
 
    # start at time 0
    if (mirror) {
      data$time <- -data$time
    }
    data <- data[order(data$time),]
    
    if (!is.null(truncate)) {
      # Exclude rows in which the cumulative number of events from that point to the final observation is below the truncate value
      keep_mask <- rev(cumsum(rev(data$event))) >= truncate
      
      # Apply the mask to the dataframe
      data <- data[keep_mask, ]
    }
    
    return (data)
  }
  
  #clean the dataset
  df_KM <- subset(df_KM, !is.na(df_KM[,varEnter])) #'varEnter' cannot be empty
  if (nrow(df_KM) == 0) return (df_KM)
  df_KM <- subset(df_KM, (df_KM[,varEnter] < df_KM[,varCens])) #'varEnter' cannot be after 'varCens'
  df_KM <- subset(df_KM, is.na(df_KM[,varEvent])|(df_KM[,varEvent] >= df_KM[,varEnter])) #'varEvent' should be after 'varEnter'
  df_KM <- subset(df_KM, is.na(df_KM[,varEvent])|(df_KM[,varEvent] < df_KM[,varCens]))  #'varEvent' cannot be after 'varCens'
  if (!is.null(varEvent2)) {
    df_KM <- subset(df_KM, is.na(df_KM[,varEvent2])|(df_KM[,varEvent2] < df_KM[,varCens]))  #'varEvent' cannot be after 'varCens'
  }
  useWeights <- !is.null(df_KM[,varWeight])
  if (useWeights) {
    df_KM <- subset (df_KM, (!is.na(df_KM[,varWeight]))) #if we use weights, they cannot be empty
    totInd <- sum(df_KM[,varWeight])
  } else {
    totInd <- dim(df_KM)[1]
  }
  if (is.na(totInd)) totInd <- nrow(df_KM)
  
  #mirroring?
  if (!is.null(varEvent2)) {
    #mirroring...
    dfE_before_E2 <- subset(df_KM, (!is.na(df_KM[,varEvent]))&((is.na(df_KM[,varEvent2]))|(df_KM[,varEvent2]>=df_KM[,varEvent])))
    if (useWeights) {
      totEvent_before_Event2 <- sum(dfE_before_E2[,varWeight])
    } else {
      totEvent_before_Event2 <- dim(dfE_before_E2)[1]
    }
    if (is.na(totEvent_before_Event2)) totEvent_before_Event2 <- nrow(dfE_before_E2)
    
    #proportion of persons who experimented 'event2' before/after 'event'
    propEventBeforeEvent2 <- totEvent_before_Event2 / totInd
    dfE2_before_E <- subset(df_KM, (!is.na(df_KM[,varEvent2]))&((is.na(df_KM[,varEvent]))|(df_KM[,varEvent2]<df_KM[,varEvent])))
    if (dim(dfE2_before_E)[1] > 3) {
      if (useWeights) {
        totEvent2_before_Event <- sum(dfE2_before_E[,varWeight])
      } else {
        totEvent2_before_Event <- dim(dfE2_before_E)[1]
      }
      if (is.na(totEvent2_before_Event)) totEvent2_before_Event <- nrow(dfE2_before_E)
      #K&M of 'event' AFTER 'event2'
      dfE2_before_E$event_after_event2 <- dfE2_before_E[,varEvent] - dfE2_before_E[,varEvent2]
      dfE2_before_E$cens_after_event2 <- dfE2_before_E[,varCens] - dfE2_before_E[,varEvent2]
      dfE2_before_E$event2_origin <- 0
      #sz=dim(dfE2_before_E)[1]
      #vecEnter=dfE2_before_E$event2_origin
      #vecEvent=dfE2_before_E$event_after_event2
      #vecCens=dfE2_before_E$cens_after_event2
      #vecWeight=dfE2_before_E[,varWeight]
      dataMirror <- computeKM (dim(dfE2_before_E)[1],
                               dfE2_before_E$event2_origin,
                               dfE2_before_E$event_after_event2,
                               dfE2_before_E$cens_after_event2,
                               dfE2_before_E[,varWeight], mirror=TRUE, truncate=truncate)
      #proportion of persons who experimented 'event2' before/after 'event'
      propEvent2BeforeEvent <- totEvent2_before_Event / totInd
      #adjust dataMirror$survFunction and associated parameters using preceding proportion
      endVal <- tail(dataMirror$survFunction, 1)
      dataMirror$survFunctionAdj <- 1 - dataMirror$survFunction * propEvent2BeforeEvent
      dataMirror$varianceAdj <- dataMirror$survFunctionAdj * dataMirror$survFunctionAdj * dataMirror$variance /
        (dataMirror$survFunction * dataMirror$survFunction)
      dataMirror$stdErrAdj <- sqrt(dataMirror$varianceAdj)
      dataMirror$confIntMaxAdj <- dataMirror$survFunctionAdj + 1.96 * dataMirror$stdErrAdj
      dataMirror$confIntMinAdj <- dataMirror$survFunctionAdj - 1.96 * dataMirror$stdErrAdj
    } else {
      propEvent2BeforeEvent <- 0
      dataMirror <- data.frame(time=c(-100,-1), event=c(1,1), number=c(1,1), numberRaw=c(1,1),
                            surv=c(1,1), rate=c(0,0), survFunction=c(1,1), variance=c(0,0),
                            stdErr=c(0,0), confIntMax=c(0,0), confIntMin=c(0,0), survFunctionAdj=c(1,1), varianceAdj=c(0,0),
                            stdErrAdj=c(0,0), confIntMaxAdj=c(0,0), confIntMinAdj=c(0,0))
    }
    #K&M of 'event2' AFTER 'event'
    dfE_before_E2$event2_after_event <- dfE_before_E2[,varEvent2] - dfE_before_E2[,varEvent]
    dfE_before_E2$cens_after_event2 <- dfE_before_E2[,varCens] - dfE_before_E2[,varEvent]
    dfE_before_E2$event_origin <- 0
    #sz=dim(dfE2_before_E)[1]
    #vecEnter=dfE2_before_E$event2_origin
    #vecEvent=dfE2_before_E$event_after_event2
    #vecCens=dfE2_before_E$cens_after_event2
    #vecWeight=dfE2_before_E[,varWeight]
    data <- computeKM (dim(dfE_before_E2)[1],
                       dfE_before_E2$event_origin,
                       dfE_before_E2$event2_after_event,
                       dfE_before_E2$cens_after_event,
                       dfE_before_E2[,varWeight], mirror=FALSE, truncate=truncate)
    #adjust data$survFunction and associated parameters using preceding proportion
    data$survFunctionAdj <- data$survFunction * propEventBeforeEvent2
    data$varianceAdj <- data$survFunctionAdj * data$survFunctionAdj * data$variance /
      (data$survFunction * data$survFunction)
    data$stdErrAdj <- sqrt(data$varianceAdj)
    data$confIntMaxAdj <- data$survFunctionAdj + 1.96 * data$stdErrAdj
    data$confIntMinAdj <- data$survFunctionAdj - 1.96 * data$stdErrAdj
    
    data <- rbind(dataMirror, data)
    data$survFunction <- data$survFunctionAdj
    data$variance <- data$varianceAdj
    data$stdErr <- data$stdErrAdj
    data$confIntMaxAdj <- data$confIntMaxAdj
    data$confIntMinAdj <- data$confIntMinAdj
    data$survFunctionAdj <- NULL
    data$varianceAdj <- NULL
    data$stdErrAdj <- NULL
    data$confIntMaxAdj <- NULL
    data$confIntMinAdj <- NULL
  } else {
    data <- computeKM (dim(df_KM)[1], df_KM[,varEnter], df_KM[,varEvent], df_KM[,varCens], df_KM[,varWeight], mirror=FALSE, truncate=truncate)
  }
  
  return (data)
}

KaplanMeierPlot <- function (df=NULL, varEnter=NULL, varEvent=NULL, varCens=NULL, varWeight=NULL, varEvent2=NULL,
                             varClass=NULL, varCountry=NULL, vecCountry=NULL, cohortsList=NULL, var_yBirth="yBirth",
                             plotType="step", minX=NULL, maxX=NULL,
                             Title=waiver(), xTitle="duration before / after", yTitle="Survival Probability",
                             inverseFunction=FALSE, confInt=FALSE, truncate=10, hideLegend=TRUE) {
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  #==> df: data.frame with the dataset
  #==> varEnter: name of the column with the starting dates of being at risk of events (for example date of birth of individuals)
  #==> varEvent: name of the column with the dates of the event (for example date of first childbearing)
  #==> varCens: name of the column with the dates at censoring (either the survey date or when the individual exit the state of being at risk)
  #ALL THE DATES SHOULD BE IN CMC FORMAT, (USUALLY WITH 1/1/1900 AS STARTING DAY), which means that the time unit is the month
  #==> varWeight: name of one or two columns with weights (optional: if not NULL, then we will use them)
  # We consider two type of WEIGHTS:
  # 1. weights that equalizes people in the sample (for example when we have oversampling of some groups, like young or old age)
  # 2. weights that allows to retrieve numbers for the whole population of the country or the region
  # if varWeight contains two types of weight, we assume than the first are individual ones and the second population one
  # individual weights are used to compute variance and deduce confidence intervals, while population weight are used for the point estimates
  #==> varEvent2: name of the column with the dates of the second event (optional, for example date of first union)
  #=============> if there are two events, then we will try to built a mirrored Kaplan&Meier (Billare, 2001)
  #==> varClass: name of a variable for facetting (optional): for example survey name
  #==> varCountry: name of the variable containing country names for facetting (optional)
  #==> vecCountry: vector of countries to plot, selected from column 'varCountry' (optional: if not NULL, then 'varCountry' cannot be NULL)
  #==> cohortsList: list with limits for creating cohort plots (optional)
  #Example: list((c(2000, 2004))) will create a group of individuals at risk starting between year 2000 and 2004
  #Example: list((c(2000, 2004), c(2005, 2009))) will create two groups of individuals at risk...
  #Example: if 'varCountry' allows building a vector of countries, then cohortsList can contain specific limits for each country:
  #list("Austria"=(c(2000, 2004), c(2005, 2009)), ...) will create two groups of individuals at risk for 'Austria'
  # and we can add other limits for other countries afterward
  #==> var_yBirth: name of the column with the year of birth of individuals (used only if cohortsList is not NULL)
  #==============> this variable can also be year of union start cohort, and cohortsList values should be adapted
  #==> plotType: one of c('step', 'smooth') (optional: if NULL, will default to 'step')
  #==> inverseFunction: if TRUE, then we plot the function 1 - K&M curve (which increase from 0 instead of decreasing from 1)
  #==> confInf to TRUE to plot the confidence interval
  #==> truncate: we truncate the K&M curve when there are less than 'truncate' events (pass NULL if no truncation)

  if (is.null (df)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")
  if (!is.null (vecCountry)&is.null (varCountry)) stop("varCountry cannot been NULL if vecCountry is used")
  
  twoWeights <- (length(varWeight) == 2)
  if (is.null(varWeight)) {
    varWeight <- "weight"
    df$weight <- 1
  }
  
  df2 <- df[,c(varEnter, varEvent, varCens)]
  # IMPORTANT: if date of event is the same than starting date of being at risk, we add 1 unit of time to the former in order
  # to compute a risk (WE DON'T DO IT, BUT LEFT THE CODE IN CASE NEEDED)
  # df2[[varEvent]] <- ifelse (df2[[varEvent]] == df2[[varEnter]], df2[[varEvent]] + 1, df2[[varEvent]])
  if (!is.null(var_yBirth)) df2[[var_yBirth]] <- df[[var_yBirth]]
  if (!is.null(varWeight)) df2[[varWeight[1]]] <- df[[varWeight[1]]]
  if (twoWeights) df2[[varWeight[2]]] <- df[[varWeight[2]]]
  if (!is.null(varEvent2)) df2[[varEvent2]] <- df[[varEvent2]]
  if (!is.null(varClass)) df2[[varClass]] <- df[[varClass]]
  if (!is.null(varCountry)) df2[[varCountry]] <- df[[varCountry]]
  
  df <- df2
  rm (df2)
  
  if (!is.null(names(cohortsList))) {
    df <- subset(df, df$country %in% names(cohortsList))
  }
  
  # Identify Class and Country Names
  varClassNames <- if (!is.null(varClass)) names(table(df[,varClass])) else "All"
  varCountryNames <- if (!is.null(varCountry)) names(table(df[,varCountry])) else "All"

  if (is.null(cohortsList)) cohortsList <- c(0, 10000)

  dataTot <- data.frame(time=NULL, event=NULL, number=NULL, numberRaw=NULL,
                        surv=NULL, rate=NULL, survFunction=NULL, variance=NULL,
                        stdErr=NULL, confIntMax=NULL, confIntMin=NULL, class=NULL, country=NULL, cohort=NULL)
  for (indClass in (1:length(varClassNames))) {
    dfClass <- df
    if (varClassNames[indClass] != "All") dfClass <- subset(df, df[,varClass]==varClassNames[indClass])
    for (indCountry in (1:length(varCountryNames))) {
      dfCountry <- dfClass
      if (varCountryNames[indCountry] != "All") dfCountry <- subset(dfClass, dfClass[,varCountry]==varCountryNames[indCountry])
      if ( !is.null(vecCountry) & (!(varCountryNames[indCountry] %in% vecCountry)) ) next
      if (is.null(names(cohortsList))) {
        vecCohorts <- as.vector(cohortsList)
      } else {
        vecCohorts <- cohortsList[varCountryNames[indCountry]][[1]]
        if (is.null(vecCohorts)) vecCohorts <- c(0, 10000)
      }
      for (indCohort in (1:(length(vecCohorts)/2))) {
        dfCountryCohort <- dfCountry
        startYear <- vecCohorts[(indCohort-1)*2+1]
        endYear <- vecCohorts[indCohort*2]
        if ((vecCohorts[2]!=10000)) {
          dfCountryCohort <- subset (dfCountry,
                                     (dfCountry[,var_yBirth] >= startYear) &
                                       (dfCountry[,var_yBirth] <= endYear))
        }
        if (dim(dfCountryCohort)[1] > 200) {
          print (paste(varCountryNames[indCountry], "cohort", indCohort))
          data <- KaplanMeier (dfCountryCohort, varEnter, varEvent, varCens, varWeight[1], varEvent2, truncate=truncate)
          if (twoWeights) {
            data2 <- KaplanMeier (dfCountryCohort, varEnter, varEvent, varCens, varWeight[2], varEvent2, truncate=truncate)
            data2 <- data2[(1:nrow(data)),]
            data2$stdErr <- data$stdErr
            data2$confIntMax <- data2$survFunction * data$confIntMax / data$survFunction
            data2$confIntMin <- data2$survFunction * data$confIntMin / data$survFunction
            data <- data2
          }
          nData <- nrow(data)
          if (nData > 10) {
            data$class <- varClassNames[indClass]
            data$country <- varCountryNames[indCountry]
            data$cohort <- paste(vecCohorts[(indCohort-1)*2+1],"-",vecCohorts[indCohort*2],sep="")
            if (vecCohorts[(indCohort-1)*2+1] >= 2010) {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1],"-",vecCohorts[indCohort*2]-2000,sep="")
            } else if (vecCohorts[(indCohort-1)*2+1] >= 2000) {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1],"-0",vecCohorts[indCohort*2]-2000,sep="")
            } else {
              data$cohortLabel <- paste(vecCohorts[(indCohort-1)*2+1]-1900,"-",vecCohorts[indCohort*2]-1900,sep="")
            }
            dataTot <- rbind(dataTot,data)
          }
        } else {
          cat (
            paste("too few cases for", varCountryNames[indCountry], "cohort", paste0(startYear,"-",endYear), "\n")
            )
        }
      }
    }
  }

  dataTot$timeYear <- dataTot$time / 12
  
  if (is.null(minX)) {
    minX <- max(-6, trunc (min (dataTot$timeYear)))
  }
  if (is.null(maxX)) {
    maxX <- 10
  }
  
  # Plotting
  # if we want to put the labels on the right, we need to cut off the data on the right of the maxX value
  dataTot <- subset(dataTot, timeYear <= maxX)
  
  if (varCountryNames[1] != "All") {
    label_data <- dataTot %>%
      group_by(country, cohort) %>%
      filter(timeYear == max(timeYear)) %>%
      ungroup()
  } else {
    # Get the point for each line to position the labels (in case only one country)
    label_data <- dataTot %>%
      group_by(cohort) %>%
      # approx() takes the existing x (timeYear) and y (survFunction) 
      # and calculates what 'y' would be at exactly xout (maxX)
      summarise(
        survFunction = approx(x = timeYear, y = survFunction, xout = maxX)$y
      ) %>%
      mutate(timeYear = maxX) # Ensure the X coordinate for the label is exactly maxX
    label_data <- dataTot %>%
      group_by(country, cohort) %>%
      filter(timeYear == max(timeYear)) %>%
      ungroup()
  }
  
  if (isTRUE(inverseFunction)) {
    dataTot$survFunction <- 1 - dataTot$survFunction
    dataTot$tmp <- dataTot$confIntMax
    dataTot$confIntMax <- 1 - dataTot$confIntMin
    dataTot$confIntMin <- 1 - dataTot$tmp
    dataTot$tmp <- NULL
    label_data$survFunction <- 1 - label_data$survFunction
  }
  
  nCohorts <- length(table(dataTot$cohort))
  my_colors <- colorRampPalette(c("gray85", "steelblue", "darkred"))(nCohorts)
  
  p <- ggplot(dataTot, aes(x=timeYear, y=survFunction, group=cohort, colour=cohort)) +
    geom_vline(xintercept = 0, linetype="dotted")
  
  p <- p + scale_color_manual(values=my_colors)
  p <- p + scale_x_continuous(expand = expansion(mult = 0.02)) +  # 2% padding
    scale_y_continuous(expand = expansion(mult = 0.02))  # 2% padding
    
  if (isTRUE(confInt)) {
    p <- p + geom_ribbon(aes(ymin = confIntMin, ymax = confIntMax, group=cohort, fill=cohort), 
                         alpha = 0.2, color=NA,
                         outline.type = "both")
    p <- p + scale_fill_manual(values=my_colors)
  }

  if (plotType=="step") p <- p + geom_step(direction = "vh")
  if (plotType=="smooth") p <- p + geom_smooth(method = "scam", formula = y ~ s(x, k = 15, bs = "mpd"), se = FALSE)
  
  p <- p + theme_linedraw() + labs(title=Title, y=yTitle, x=xTitle, colour="Birth Cohort",
                                   fill = "Birth Cohort")
  p <- p + coord_cartesian(ylim = c(0, 1), xlim=c(minX, maxX))
  
  # Faceting
  if (varClassNames[1] != "All" && varCountryNames[1] != "All") {
    p <- p + facet_wrap(~class + country)
  } else if (varClassNames[1] != "All") {
    p <- p + facet_wrap(~class)
  } else if (varCountryNames[1] != "All") {
    # force ggrepel to show the labels outside of the lines
    p <- p + geom_point(
      data = dataTot,
      size = 0.1,
      alpha = 0  # Invisible
    )
    p <- p + facet_wrap(~country)
    p <- p + geom_text_repel(
      data = label_data,
      aes(label = cohortLabel),
      show.legend = FALSE,
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      size = 3,
      segment.size = 0.2,
      segment.color = "grey50",
      force = 8,              # Increase repulsion force (default is 1)
      force_pull = 0.5,       # How strongly labels are pulled toward points
      box.padding = 0.5,      # Padding around each label box (default 0.25)
      point.padding = 0.8,    # Padding around the data point
      min.segment.length = 0, # Always show connector segments
      max.overlaps = Inf      # Allow all labels to show
    )
    if (hideLegend) {p <- p + theme(legend.position = "none")}
  } else {
    # no faceting: only one plot
    # we associate the labels with the lines
    p <- p + geom_text_repel(data = label_data, 
                             aes(label = cohort, color = cohort),
                             direction = "y", 
                             hjust = 0,
                             force = 2,
                             nudge_x = 1,
                             segment.linetype = "dotted", # Optional: adds a small guide line
                             min.segment.length = 0, xlim=c(0, maxX)) +
      theme(legend.position = "none") +
    theme(
      panel.border = element_blank(),           # Remove panel border
      axis.line = element_line(color = "black") # Keep only bottom and left axes
    )
    p <- p + theme(plot.margin = margin(5, 5, 5, 5))
  }

  p
  
  # p <- ggplot(dataTot, aes(x=timeYear, y=survFunction, group=cohort, colour=cohort))
  # p <- p + geom_vline(xintercept = 0)
  # if (plotType=="step") p <- p + geom_step()
  # if (plotType=="smooth") p <- p + geom_smooth(method = "scam", formula = y ~ s(x, k = 50, bs = "mpd"), se = FALSE)
  # p <- p + geom_smooth(span=0.5, se = TRUE)
  # p <- p + theme_linedraw() + labs(y="Survival Probability", x="first conception: duration before / after start of first union", colour="Birth Cohort")
  # p <- p + coord_cartesian(ylim = c(0, 1), xlim=c(-6, 10))
  # p <- p + scale_x_continuous(breaks=seq(-6, 10, 2))
  # if ((varClassNames[1]!="All")&(varCountryNames[1]!="All")) {
  #   p <- p + facet_wrap(vars(class)~vars(country))
  # } else if (varClassNames[1]!="All") {
  #   p <- p + facet_wrap(vars(class))
  # } else if (varCountryNames[1]!="All") {
  #   p <- p + facet_wrap(vars(country))
  # }
 
}

buildMatrix <- function(rowMin, rowMax, colMin, colMax, defaultValue=0) {
  mat <- matrix(defaultValue, ncol=(colMax - colMin + 1), nrow=(rowMax - rowMin + 1))
  colnames(mat) <- as.character(colMin:colMax)
  rownames(mat) <- as.character(rowMin:rowMax)
  return (mat)
}

buildVector <- function(colMin, colMax, defaultValue=0, numYear=NULL) {
  if (is.null(numYear)) {
    nYears <- colMax-colMin+1
  } else {
    nYears <- numYear
  }
  vec <- rep(defaultValue, nYears)
  names(vec) <- as.character((colMax-nYears+1):colMax)
  return (vec)
}

initLifeTable <- function (lg=30) {
  lifeTable <- data.frame(l0=0)
  for (ind in (1:(lg-1))) {
    lt <- data.frame(l=0)
    names(lt)[1] <- paste("l", ind, sep="")
    lifeTable <- cbind(lifeTable, lt)
  }
  return (lifeTable)
}

plotDeaths <- function (df=deathsTablePeriod, lastCol=50) {
  nRows <- dim(df)[1]
  nCols <- dim(df)[2]
  df <- df[,((nCols-lastCol+1):nCols)]
  dfPlot <- data.frame(duration=NULL,deaths=NULL,year=NULL)
  for (x in (1:lastCol)) {
    tmp <- data.frame(duration=as.integer(rownames(df)))
    tmp$deaths <- 0
    tmp$deaths[(1:(nRows-(lastCol-x)))] <- df[((lastCol-x+1):nRows),x]
    tmp$year <- colnames(df)[x]
    dfPlot <- rbind(dfPlot, tmp)
  }
  colours <- rep("grey80", lastCol)
  colours[10] <- "blue"
  colours[20] <- "green"
  colours[30] <- "black"
  colours[40] <- "orange"
  colours[49] <- "red"
  names(colours) <- colnames(df)
  small_dfPlot <- subset (dfPlot, (year %in% c(1970, 1980, 1990, 2000, 2009)))
  p <- ggplot(dfPlot, aes(x=duration, y=deaths, group=year, color=year)) + geom_smooth(se=FALSE, span=0.3)
  p <- p + scale_y_continuous(limit=c(0,NA),oob=squish)
  p <- p + scale_color_manual(values=colours)
  p <- p + theme_linedraw() + theme_text()
  p <- p + coord_cartesian(ylim=c(0, 0.05))
  p <- p + directlabels::geom_dl(data=small_dfPlot, aes(label = year), method = "first.bumpup")
  p <- p + directlabels::geom_dl(aes(label = year), method = "first.bumpup")
  p <- p + theme(legend.position="none")
  p
}

GreenwoodVar <- function (events, popAtRisk, iStart, iEnd, Sx) {
  var <- 0
  for (ind in (iStart:iEnd)) {
    var <- var + events[ind] / (popAtRisk[ind] * (popAtRisk[ind] - events[ind]))
  }
  return ((Sx^2)*var)
}

# Bootstrap validation
# Compare your variance estimates with bootstrap confidence intervals

validate_with_bootstrap <- function(df_ppr, varEnter, varEvent, varCens, 
                                    varWeight = NULL, n_bootstrap = 200) {
  cat("=== Method 2: Bootstrap Validation ===\n\n")
  cat("This compares analytical variance (Greenwood) with bootstrap SE\n")
  cat(sprintf("Running %d bootstrap iterations...\n\n", n_bootstrap))
  
  # Run your function once to get the analytical results
  # (Assuming ppr_doIt is available)
  original_result <- ppr_doIt(df_ppr, varEnter = varEnter, varEvent = varEvent, 
                              varCens = varCens, varWeight = varWeight,
                              computeVariance = TRUE)
  
  # Bootstrap resampling
  bootstrap_means <- matrix(NA, nrow = n_bootstrap, 
                            ncol = length(original_result$PPR$year))
  bootstrap_quanta <- matrix(NA, nrow = n_bootstrap, 
                             ncol = length(original_result$PPR$year))
  
  for (b in 1:n_bootstrap) {
    # Resample with replacement
    indices <- sample(1:nrow(df_ppr), nrow(df_ppr), replace = TRUE)
    df_boot <- df_ppr[indices, ]
    
    # Run analysis on bootstrap sample
    tryCatch({
      boot_result <- ppr_doIt(df_boot, varEnter = varEnter, varEvent = varEvent,
                              varCens = varCens, varWeight = varWeight,
                              computeVariance = FALSE)  # Skip variance for speed
      
      bootstrap_means[b, ] <- boot_result$PPR$mac_1stkind
      bootstrap_quanta[b, ] <- boot_result$PPR$quantum_1stkind
    }, error = function(e) {
      # Skip failed bootstrap samples
    })
    
    if (b %% 50 == 0) cat(sprintf("  Completed %d iterations\n", b))
  }
  
  # Calculate bootstrap standard errors
  boot_se_mean <- apply(bootstrap_means, 2, sd, na.rm = TRUE)
  boot_se_quantum <- apply(bootstrap_quanta, 2, sd, na.rm = TRUE)
  
  # Compare with analytical SE
  comparison_df <- data.frame(
    year = original_result$PPR$year,
    analytical_SE_mean = original_result$PPR$se_mac_1stkind,
    bootstrap_SE_mean = boot_se_mean,
    ratio_mean = original_result$PPR$se_mac_1stkind / boot_se_mean,
    analytical_SE_quantum = original_result$PPR$se_quantum_1stkind,
    bootstrap_SE_quantum = boot_se_quantum,
    ratio_quantum = original_result$PPR$se_quantum_1stkind / boot_se_quantum
  )
  
  cat("\nComparison of Analytical vs Bootstrap Standard Errors:\n")
  print(comparison_df)
  cat("\nRatio should be close to 1.0 if analytical variance is correct\n")
  cat(sprintf("Mean ratio for MAC: %.3f (SD: %.3f)\n", 
              mean(comparison_df$ratio_mean, na.rm = TRUE),
              sd(comparison_df$ratio_mean, na.rm = TRUE)))
  cat(sprintf("Mean ratio for Quantum: %.3f (SD: %.3f)\n\n", 
              mean(comparison_df$ratio_quantum, na.rm = TRUE),
              sd(comparison_df$ratio_quantum, na.rm = TRUE)))
  
  return(comparison_df)
}

ppr_doIt <- function (df_ppr=dfCountry, debugFunction=FALSE,
                      varEnter, varEvent, varCens, varWeight=NULL, country="all",
                      duration=TRUE, res_numYears=20, res_finalYearsToDiscard=1, res_firstYearsToDiscard=10,
                      useRelativeWeights=FALSE,ageTruncate=NULL,mySpan=0.75,
                      computeVariance=TRUE) {
  
  useWeights <- !is.null(varWeight)
  if (isTRUE(useWeights) & isTRUE(useRelativeWeights)) {
    df_ppr <- do_useRelativeWeights (df_ppr, varWeight)
  }
  
  if (!is.null (ageTruncate)) {
    # if ageTruncate has a value, is not NULL, we use it to truncate the events that happen after that age
    # (we set them to NA, so that they are not counted as events, but the individuals are still at risk of events until the censoring date)
    df_ppr[,varEvent] <- ifelse ((!is.na(df_ppr$ageEvent))&(df_ppr$ageEvent >= ageTruncate), NA, df_ppr[,varEvent])
  }
  
  yMin_Enter <- min(df_ppr[,varEnter])
  yMax_Enter <- max(df_ppr[,varEnter])
  c_x <- yMax_Enter - yMin_Enter + 1
  cat <- table(df_ppr[,varEvent])
  yMin_Event <- as.integer(names(cat)[1])
  yMax_Event <- as.integer(tail(names(cat),1))
  c_y <- yMax_Event - yMin_Event + 1
  rownames(df_ppr) <- (1:dim(df_ppr)[1])
  res_numYears <- min (res_numYears, c_y-1)
  
  buildPopRow <- function(yMin_Event, yMax_Event, yCens, fracYear, weight) {
    popRow <- rep(0, (yMax_Event - yMin_Event + 1))
    dd = (yCens < yMin_Event)
    if (yCens < yMin_Event) {return (popRow)}
    popRow <- rep(weight, (yMax_Event - yMin_Event + 1))
    names (popRow) <- as.character(yMin_Event:yMax_Event)
    if (yCens < yMax_Event) {
      popRow[(yCens:yMax_Event) - yMin_Event + 1] <- 0
      popRow[yCens - yMin_Event + 1] <- fracYear * weight
    }
    return (popRow)
  }
  buildEventRow <- function(yMin_Event, yMax_Event, yEvent, weight) {
    eventRow <- rep(0, (yMax_Event - yMin_Event + 1))
    names (eventRow) <- as.character(yMin_Event:yMax_Event)
    if (!is.na(yEvent)) {eventRow[yEvent - yMin_Event + 1 ] <- weight}
    return (eventRow)
  }
  
  #build population count and event count matrices
  popYear <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  popYear_noW <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  eventYear <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  eventYear_noW <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  for (ind in (1:dim(df_ppr)[1])) {
    yEnter <- df_ppr[,varEnter][ind]
    yEvent <- df_ppr[,varEvent][ind]
    cmc_cens <- df_ppr[,varCens][ind]
    yCens <- trunc ((cmc_cens - 1) / 12)
    fracYear <- (cmc_cens - yCens * 12) / 12
    yCens <- yCens + 1900
    if (useWeights) {
      weight <- df_ppr[,varWeight][ind]
    } else {
      weight <- 1
    }
    
    popRow <- buildPopRow(yMin_Event, yMax_Event, yCens, fracYear, weight)
    popRow_noW <- buildPopRow(yMin_Event, yMax_Event, yCens, fracYear, 1)
    
    eventRow <- buildEventRow(yMin_Event, yMax_Event, yEvent, weight)
    eventRow_noW <- buildEventRow(yMin_Event, yMax_Event, yEvent, 1)
    popYear[yEnter - yMin_Enter + 1,] <- popYear[yEnter - yMin_Enter + 1,] + popRow
    popYear_noW[yEnter - yMin_Enter + 1,] <- popYear_noW[yEnter - yMin_Enter + 1,] + popRow_noW
    eventYear[yEnter - yMin_Enter + 1,] <- eventYear[yEnter - yMin_Enter + 1,] + eventRow
    eventYear_noW[yEnter - yMin_Enter + 1,] <- eventYear_noW[yEnter - yMin_Enter + 1,] + eventRow_noW
  }
  #standardize the event count by the population count (this way we will obtain later rates of the first and the second kind)
  #these are the d(x) of the life table
  stdEventYear <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  tmp <- which(popYear != 0)
  stdEventYear[tmp] <- eventYear[tmp] / popYear[tmp]
  #survival function by cohort
  survivalCohort <- buildMatrix(yMin_Enter, yMax_Enter, 0, c_y, defaultValue=1)
  for (y in (1:c_y)) {
    survivalCohort [,y+1] <- survivalCohort [,y] - stdEventYear[,y]
  }
  #rates of first kind
  ratesFirstKind <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event)
  for (y in (1:c_y)) {
    ratesFirstKind [,y] <- (survivalCohort [,y] - survivalCohort [,y+1]) / survivalCohort [,y]
  }
  ratesFirstKind[is.na(ratesFirstKind)] = 0
  #avoid cases where nearly all the surviving persons make a transition to the event
  #in these cases the probability has value which can reach 1
  #we put here 0.25 instead of 1...
  ratesFirstKind[which(ratesFirstKind>0.25)] <- 0.25
  
  # Hadamard or element-wise product
  popAtRisk <- popYear * survivalCohort[,(1:(c_y))]
  
  # if (isTRUE(smoothRates)) {
  #   library(mgcv)
  #   ratesFirstKind_loess <- ratesFirstKind + 1e-10
  #   ratesFirstKind_gam <- ratesFirstKind
  #   for (y in (1:c_y)) {
  #     mod <- loess(log(ratesFirstKind_loess[,y]) ~ as.integer(rownames(ratesFirstKind)), weights = popAtRisk[,y], span=0.3)
  #     ratesFirstKind_loess[,y] <- exp(predict(mod, as.integer(rownames(ratesFirstKind))))
  #     
  #     # Use family = Gamma(link = "log") to enforce positivity
  #     model <- gam(ratesFirstKind[,y] ~ s(as.integer(rownames(ratesFirstKind))), weights = popAtRisk[,y], family = quasipoisson(link = "log"))
  #     ratesFirstKind_gam[,y] <- predict(model, type = "response")    
  #   }
  # }
  
  #survival function for each period
  survivalYear <- buildMatrix(yMin_Enter-1, yMax_Enter, yMin_Event, yMax_Event, defaultValue=1)
  #order arrays upside down in order to have age or duration increasing rowwise
  stdEventYear <- stdEventYear[order(row.names(stdEventYear),decreasing=TRUE),] 
  ratesFirstKind <- ratesFirstKind[order(row.names(ratesFirstKind),decreasing=TRUE),] 
  popAtRisk <- popAtRisk[order(row.names(popAtRisk),decreasing=TRUE),]
  survivalYear <- survivalYear[order(row.names(survivalYear),decreasing=TRUE),]
  for (x in (1:c_x)) {
    survivalYear[x+1,] <- survivalYear[x,] * (1 - ratesFirstKind[x,])
  }
  if (duration) {rownames(survivalYear) <- as.character(0:(c_x))}
  
  ##
  # VARIANCE OF SURVIVAL - Greenwood's formula ====
  ##
  
  if (computeVariance) {
    # Greenwood's formula for variance of survival function
    # Var(S(x)) = S(x)^2 * sum_{i=0}^{x-1} [ q_i / (n_i * (1 - q_i)) ]
    # where q_i = probability of event at age/duration i
    #       n_i = population at risk at age/duration i
    #       S(x) = survival probability at age/duration x
    
    varianceSurvival <- buildMatrix(yMin_Enter-1, yMax_Enter, yMin_Event, yMax_Event, defaultValue=0)
    varianceSurvival <- varianceSurvival[order(row.names(varianceSurvival),decreasing=TRUE),]
    if (duration) {rownames(varianceSurvival) <- as.character(0:(c_x))}
    
    # For each period (year/column)
    for (y in (1:c_y)) {
      cumulative_var_term <- 0
      
      # For each age/duration (row)
      for (x in (1:c_x)) {
        q_x <- ratesFirstKind[x, y]
        n_x <- popAtRisk[x, y]
        
        # Avoid division by zero and numerical issues
        if (n_x > 0 && q_x < 1 && q_x > 0) {
          # Greenwood variance component for this age/duration
          var_component <- q_x / (n_x * (1 - q_x))
          cumulative_var_term <- cumulative_var_term + var_component
        }
        
        # Variance of survival at age/duration x+1
        S_x <- survivalYear[x+1, y]
        varianceSurvival[x+1, y] <- (S_x^2) * cumulative_var_term
      }
    }
    
    # Standard error of survival
    seSurvival <- sqrt(varianceSurvival)
  }

  #deaths counts from the life table
  deathsTablePeriod <- buildMatrix(yMin_Enter, yMax_Enter, yMin_Event, yMax_Event) 
  deathsTablePeriod <- deathsTablePeriod[order(row.names(deathsTablePeriod),decreasing=TRUE),]
  if (duration) {rownames(deathsTablePeriod) <- as.character(0:(c_x-1))}
  for (x in (1:c_x)) {
    deathsTablePeriod[x,] <- (survivalYear [x,] - survivalYear [x+1,])
  }
  
  #age (first birth) or duration (subsequent) for the last year
  # ageORduration <- yMax_Enter - as.integer(rownames(stdEventYear)[1:(length(rownames(stdEventYear)))])
  ageORduration <- c(0:(c_x-1)) + yMax_Event - yMax_Enter
  
  #mean age or duration computed from the life table
  meanFirstKind <- buildVector(yMin_Event, yMax_Event)
  meanFirstKind <- ( as.vector((ageORduration) %*% deathsTablePeriod) / colSums(deathsTablePeriod) ) - (seq(c_y, 1)-1)
  meanFirstKind[is.nan(meanFirstKind)] <- 0
  
  #mean age or duration computed from the life table
  meanSecondKind <- buildVector(yMin_Event, yMax_Event)
  meanSecondKind <- ( as.vector((ageORduration) %*% stdEventYear) / colSums(stdEventYear) ) - (seq(c_y, 1)-1)
  meanSecondKind[is.nan(meanSecondKind)] <- 0
  
  ##### loess smoothing of the results ====
  for (y in (1:c_y)) {
    # Smooth mean of first kind
    mod_mean1 <- loess(meanFirstKind ~ as.integer(names(meanFirstKind)), weights = colSums(popAtRisk), span=mySpan)
    meanFirstKind_smoothed <- predict(mod_mean1, as.integer(names(meanFirstKind)))
    
    # Smooth mean of second kind
    mod_mean2 <- loess(meanSecondKind ~ as.integer(names(meanSecondKind)), weights = colSums(popAtRisk), span=mySpan)
    meanSecondKind_smoothed <- predict(mod_mean2, as.integer(names(meanSecondKind)))
    
    # Smooth quantum of first kind
    mod_quantum1 <- loess(survivalYear[c_x,] ~ as.integer(colnames(survivalYear)), weights = colSums(popAtRisk), span=mySpan)
    survivalYear_smoothed <- predict(mod_quantum1, as.integer(colnames(survivalYear)))
    
    # Smooth quantum of second kind
    mod_quantum2 <- loess(colSums(stdEventYear) ~ as.integer(colnames(stdEventYear)), weights = colSums(popAtRisk), span=mySpan)
    stdEventYear_smoothed <- predict(mod_quantum2, as.integer(colnames(stdEventYear)))
  }
  
  ##
  # VARIANCE OF MEAN DURATION - Chiang's formula ====
  ##
  
  if (computeVariance) {
    # Chiang's formula for variance of life expectancy (mean duration)
    # Var(e_0) = sum_{x=0}^{omega} [ (A_x) * Var(q_x) ]
    # where A_x = (l_x)^2 * (e_{x+1} + 0.5)^2
    #       e_x = life expectancy at age x, computed as sum_{i=0}^{x-1} (l_i + l_{i+1})/2 for unit intervals
    #       l_x = survival probability from birth at age i
    #       l_0 = radix (initial cohort size, typically 1)
    #       Var(q_x) = variance of probability of dying at age x, estimated as q_x^2 * (1 - q_x) * D for binomial distribution,
    #                  where D is the number of events, that can be computed as D = q_x * n_x, the population at risk at age x
    #                  which explains why we can also compute Var(q_x) = q_x * (1 - q_x) / n_x

    varianceMeanFirstKind <- buildVector(yMin_Event, yMax_Event)
    varianceMeanSecondKind <- buildVector(yMin_Event, yMax_Event)

    # For each period (year/column)
    for (y in (1:c_y)) {
      # Calculate L_x (person-years lived) for each interval
      # For a unit interval: L_x = (l_x + l_{x+1}) / 2
      L_x <- numeric(c_x)
      for (x in (1:(c_x-1))) {
        L_x[x] <- (survivalYear[x, y] + survivalYear[x+1, y]) / 2
      }
      L_x[c_x] <- 0
      
      # Calculate e_x
      # Since our radix is 1 (survivalYear[1,y] = 1), we just need cumulative sum from end
      e_x <- numeric(c_x)
      for (x in (c_x:1)) {
        e_x[x] <- sum(L_x[x:c_x])
      }
      e_x[c_x] <- 0 # not necessary, but just in case

      # Apply Chiang's formula
      var_sum <- 0
      D_x <- ratesFirstKind[,y] * popAtRisk[,y]
      for (x in (1:(c_x-1))) {
        q_x <- ratesFirstKind[x, y]
        n_x <- popAtRisk[x, y]
        if (n_x > 0) var_q_x <- q_x * (1 - q_x) / n_x else var_q_x <- 0
        A_x <- survivalYear[x, y]^2 * (e_x[x+1] + 0.5)^2
        var_sum <- var_sum + (A_x) * var_q_x
      }
      
       varianceMeanFirstKind[y] <- var_sum

      # For second kind, we can use a similar approach but based on observed events
      # This is more approximate as rates of second kind don't form a proper life table
      # We'll use the same variance structure scaled by the ratio of means
      if (meanFirstKind[y] > 0) {
        scale_factor <- (meanSecondKind[y] / meanFirstKind[y])^2
        varianceMeanSecondKind[y] <- varianceMeanFirstKind[y] * scale_factor
      } else {
        varianceMeanSecondKind[y] <- NA
      }
    }
    
    # Standard errors
    seMeanFirstKind <- sqrt(varianceMeanFirstKind)
    seMeanSecondKind <- sqrt(varianceMeanSecondKind)
    
    # 95% Confidence intervals
    ci95_lower_meanFirstKind <- meanFirstKind - 1.96 * seMeanFirstKind
    ci95_upper_meanFirstKind <- meanFirstKind + 1.96 * seMeanFirstKind
    ci95_lower_meanSecondKind <- meanSecondKind - 1.96 * seMeanSecondKind
    ci95_upper_meanSecondKind <- meanSecondKind + 1.96 * seMeanSecondKind

    ci95_lower_meanFirstKind_smoothed <- meanFirstKind_smoothed - 1.96 * seMeanFirstKind
    ci95_upper_meanFirstKind_smoothed <- meanFirstKind_smoothed + 1.96 * seMeanFirstKind
    ci95_lower_meanSecondKind_smoothed <- meanSecondKind_smoothed - 1.96 * seMeanSecondKind
    ci95_upper_meanSecondKind_smoothed <- meanSecondKind_smoothed + 1.96 * seMeanSecondKind
  }
  
  
  #results: ppr from rates of first kind (2) mean age at childbearing from rates of first kind (3)
  #results: ppr or tfr from rates of second kind (4) mean age at childbearing from rates of second kind (5) number of events each year in column (6)
  #results: the first column is the year...
  k_ind_year <- 1
  k_ind_quantum_1st_kind <- 2
  k_ind_mac_1st_kind <- 3
  k_ind_quantum_2nd_kind <- 4
  k_ind_mac_2nd_kind <- 5
  k_ind_quantum_1st_kind_smoothed <- 6
  k_ind_mac_1st_kind_smoothed <- 7
  k_ind_quantum_2nd_kind_smoothed <- 8
  k_ind_mac_2nd_kind_smoothed <- 9
  k_ind_quantum_1st_kind_corrected <- 10
  k_ind_quantum_2nd_kind_corrected <- 11
  k_ind_number_events <- 12
  k_ind_pop_at_risk <- 13
  c_res <- array(0,c(res_numYears,k_ind_pop_at_risk))
  
  firstYear <- yMin_Event + res_firstYearsToDiscard
  lastYear <- yMax_Event - res_finalYearsToDiscard
  range <- lastYear - firstYear + 1
  range <- min (range, res_numYears)
  lastInd <- c_y - res_finalYearsToDiscard
  firstInd <- lastInd - range + 1
  firstYear <- lastYear - range + 1
  rangeYear <- (firstYear:lastYear)
  rangeInd <- (firstInd:lastInd)
  rangeRes <- ((res_numYears - range + 1):res_numYears)
  c_res[rangeRes, k_ind_year] <- rangeYear
  c_res[rangeRes, k_ind_quantum_1st_kind] <- 1 - survivalYear[c_x,rangeInd]
  c_res[rangeRes, k_ind_mac_1st_kind] <- meanFirstKind[rangeInd]
  c_res[rangeRes, k_ind_quantum_2nd_kind] <- colSums(stdEventYear[,rangeInd])
  c_res[rangeRes, k_ind_mac_2nd_kind] <- meanSecondKind[rangeInd]
  c_res[rangeRes, k_ind_quantum_1st_kind_smoothed] <- 1 - survivalYear_smoothed[rangeInd]
  c_res[rangeRes, k_ind_mac_1st_kind_smoothed] <- meanFirstKind_smoothed[rangeInd]
  c_res[rangeRes, k_ind_quantum_2nd_kind_smoothed] <- stdEventYear_smoothed[rangeInd]
  c_res[rangeRes, k_ind_mac_2nd_kind_smoothed] <- meanSecondKind_smoothed[rangeInd]
  c_res[rangeRes, k_ind_number_events] <- colSums(eventYear_noW[,rangeInd])
  c_res[rangeRes, k_ind_pop_at_risk] <- colSums(popAtRisk[,rangeInd])
  colnames(c_res) <- c("year", "quantum_1st_kind", "mac_1st_kind", "quantum_2nd_kind", "mac_2nd_kind",
                       "quantum_1st_kind_smoothed", "mac_1st_kind_smoothed", "quantum_2nd_kind_smoothed", "mac_2nd_kind_smoothed",
                       "quantum_1st_kind_corrected", "quantum_2nd_kind_corrected",
                       "number_events","pop_at_risk")
  
  # Prepare output list
  output_list <- list(
    PPR=data.frame(
      country=rep(country, length(rangeRes)),
      year=c_res[rangeRes,k_ind_year],
      quantum_1stkind=c_res[rangeRes, k_ind_quantum_1st_kind],
      mac_1stkind=c_res[rangeRes, k_ind_mac_1st_kind],
      quantum_2ndkind=c_res[rangeRes, k_ind_quantum_2nd_kind],
      mac_2ndkind=c_res[rangeRes, k_ind_mac_2nd_kind],
      quantum_1stkind_smoothed=c_res[rangeRes, k_ind_quantum_1st_kind_smoothed],
      mac_1stkind_smoothed=c_res[rangeRes, k_ind_mac_1st_kind_smoothed],
      quantum_2ndkind_smoothed=c_res[rangeRes, k_ind_quantum_2nd_kind_smoothed],
      mac_2ndkind_smoothed=c_res[rangeRes, k_ind_mac_2nd_kind_smoothed],
      quantum_1stkind_corrected=c_res[rangeRes, k_ind_quantum_1st_kind_corrected],
      quantum_2ndkind_corrected=c_res[rangeRes, k_ind_quantum_2nd_kind_corrected],
      nEvents=c_res[rangeRes, k_ind_number_events]
    ),
    rates1=ratesFirstKind, # rates of first kind (from the life table)
    rates2=stdEventYear # rates of second kind (standardized by the population at risk)
  )
  
  # Add variance-related outputs if computed
  if (computeVariance) {
    # Add variance and confidence intervals to the main PPR dataframe
    output_list$PPR$se_mac_1stkind <- seMeanFirstKind[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind <- ci95_lower_meanFirstKind[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind <- ci95_upper_meanFirstKind[rangeInd]
    output_list$PPR$ci95_lower_mac_1stkind_smoothed <- ci95_lower_meanFirstKind_smoothed[rangeInd]
    output_list$PPR$ci95_upper_mac_1stkind_smoothed <- ci95_upper_meanFirstKind_smoothed[rangeInd]
    output_list$PPR$se_mac_2ndkind <- seMeanSecondKind[rangeInd]
    output_list$PPR$ci95_lower_mac_2ndkind <- ci95_lower_meanSecondKind[rangeInd]
    output_list$PPR$ci95_upper_mac_2ndkind <- ci95_upper_meanSecondKind[rangeInd]
    output_list$PPR$ci95_lower_mac_2ndkind_smoothed <- ci95_lower_meanSecondKind_smoothed[rangeInd]
    output_list$PPR$ci95_upper_mac_2ndkind_smoothed <- ci95_upper_meanSecondKind_smoothed[rangeInd]
    
    # Add variance of quantum (from final survival)
    output_list$PPR$se_quantum_1stkind <- seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_lower_quantum_1stkind <- (1 - survivalYear[c_x,rangeInd]) - 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_upper_quantum_1stkind <- (1 - survivalYear[c_x,rangeInd]) + 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_lower_quantum_1stkind_smoothed <- (1 - survivalYear_smoothed[rangeInd]) - 1.96 * seSurvival[c_x, rangeInd]
    output_list$PPR$ci95_upper_quantum_1stkind_smoothed <- (1 - survivalYear_smoothed[rangeInd]) + 1.96 * seSurvival[c_x, rangeInd]
    
    # Add full variance matrices to output
    output_list$varianceSurvival <- varianceSurvival
    output_list$seSurvival <- seSurvival
    output_list$varianceMeanFirstKind <- varianceMeanFirstKind
    output_list$varianceMeanSecondKind <- varianceMeanSecondKind
    output_list$seMeanFirstKind <- seMeanFirstKind
    output_list$seMeanSecondKind <- seMeanSecondKind
  }
  
  return(output_list)
}


calc_ppr <- function (df=NULL,
                      varEnter="yUnion1", varEvent="ySep1", varCens="cmc_survey", varCountry="country", vecCountry=NULL, varWeight=NULL,
                      duration=TRUE, res_numYears=30, res_finalYearsToDiscard=1, res_firstYearsToDiscard=10, res_countrySpecific_numYears=NULL,
                      useRelativeWeights=TRUE,ageTruncate=NULL,mySpan=0.75) {
  if (is.null (df)) stop("the dataframe cannot been NULL")
  if (is.null (varEnter) | is.null(varEvent) | is.null(varCens)) stop("varEnter, varEvent and varCens cannot been NULL")
  if (!is.null (vecCountry)&is.null (varCountry)) stop("varCountry cannot been NULL if vecCountry is used")

  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  varCountryNames <- c("All")
  if (!is.null (varCountry)) {
    cat <- table(df[, varCountry])
    varCountryNames <- names(cat)
  }
  
  # pprsTot <- data.frame(country=NULL, year=NULL, quantum_1stkind=NULL, mac_1stkind=NULL, quantum_2ndkind=NULL, mac_2ndkind=NULL,
  #                    quantum_1stkind_smoothed=NULL, mac_1stkind_smoothed=NULL, quantum_2ndkind_smoothed=NULL, mac_2ndkind_smoothed=NULL,
  #                    quantum_1stkind_corrected=NULL, quantum_2ndkind_corrected=NULL,
  #                    nEvents=NULL)
  pprsTot <- data.frame()
  
  for (indCountry in (1:length(varCountryNames))) {
    countrySel <- varCountryNames[indCountry]
    if (countrySel != "All") {
      print (countrySel)
      dfCountry <- subset(df, country==countrySel)
      #specific number of years for this country?
      if (!is.null(res_countrySpecific_numYears)) {
        if (countrySel %in% res_countrySpecific_numYears$country)
        {
          #info for the current country found...
          dfSel <- subset(res_countrySpecific_numYears, country==countrySel)
          dfCountry$surveyName <- factor(dfCountry$surveyName)
          surveys <- names(table(dfCountry$surveyName))
          #we will use it if the main data.frame contains info for only ONE survey
          #and the new number of years to discard corresponds to that survey...
          if ((length(surveys)==1)&(surveys[1] %in% dfSel$surveyName)) {
            dfSel <- subset(dfSel, surveyName==surveys[1])
            res_numYears <- dfSel$years
          }
        }
      }
    }
    if ( !is.null(vecCountry) & (!(countrySel %in% vecCountry)) ) next
    pprs <- ppr_doIt (dfCountry, varEnter=varEnter, varEvent=varEvent, varCens=varCens, varWeight=varWeight, country=countrySel,
                       duration=duration,
                      res_numYears=res_numYears,
                      res_finalYearsToDiscard=res_finalYearsToDiscard,
                      res_firstYearsToDiscard=res_firstYearsToDiscard,
                      useRelativeWeights=useRelativeWeights,
                      ageTruncate=ageTruncate,
                      mySpan=mySpan)
    pprsTot <- rbind(pprsTot, pprs$PPR)
  }
  
  return (pprsTot)
}

plot_ppr <- function(df_res=res_BirthBirth1, vecCountry=NULL, facet=TRUE, yLimit=NULL, Title=NULL, yTitle="PPR", xTitle=NULL,
                     yVar="quantum_1stkind_smoothed", yVar_min="ci95_lower_quantum_1stkind_smoothed", yVar_max="ci95_upper_quantum_1stkind_smoothed") {
  require (ggplot2)
  require (ggrepel)
  require (scales)
  
  if (!is.null(vecCountry)) {
    df_res <- subset(df_res, country %in% vecCountry)
  }
  
  if ((exists("DEBUG2")) && (isTRUE(DEBUG2))) browser()
  
  bySurvey <- ("surveyName" %in% colnames(df_res))
  
  if (facet) {
    # 1. Get the last points for labeling
    if (isTRUE(bySurvey)) {
      df_ends <- df_res %>% 
        group_by(country, surveyName) %>% 
        filter(year == max(year))
    } else {
      df_ends <- df_res %>% 
        group_by(country) %>% 
        filter(year == max(year))
    }
  } else {
    df_labels <- df_res %>%
      group_by(country) %>%
      slice(ceiling(n() / 2))
    }
  # 2. Get the max year to know where the "edge" is
  max_x <- max(df_res$year)
  max_y <- min(0.75,max(df_res[[yVar]]))
  
  if (facet) {
    if (bySurvey) {
      p <- ggplot(df_res, aes(x=year, y=.data[[yVar]], group=surveyName, color=surveyName))
      p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]], fill=surveyName),
                           alpha=0.1,
                           color = NA)
    } else {
      p <- ggplot(df_res, aes(x=year, y=.data[[yVar]]),
                  color = NA)
      p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]]),
                           alpha=0.1,
                           color = NA)
    }
  } else {
    p <- ggplot(df_res, aes(x=year, y=.data[[yVar]], group=country, color=country))
    p <- p + geom_ribbon(aes(ymin=.data[[yVar_min]], ymax=.data[[yVar_max]], fill=country), alpha=0.1, color = NA)
  }
  p <- p  + geom_line()
  if(facet) {p <- p + facet_wrap (vars(country))}
  p <- p + scale_y_continuous(limit=c(0,NA),oob=squish)
  p <- p + theme_linedraw()
  if (!is.null(yLimit)) {p <- p + coord_cartesian(ylim=yLimit)}
  p <- p + theme(legend.position="none")
  if (facet) {
    if (bySurvey) {
      # p <- p + directlabels::geom_dl(aes(label = surveyName), method = "chull.grid")
      p <- p + geom_text_repel(
        data = df_ends,
        aes(label = surveyName), 
        size = 4,
        fontface = "bold",
        hjust = 0,
        direction = "y",           # Stack them vertically to avoid overlap
        nudge_x = -1,               # Force them 5 units to the right of the last point
        xlim = c(NA, max_x),   # Don't let labels go the right
        ylim = c(max_y, NA),   # Don't let labels cross over the curves
        segment.color ="grey50",
        segment.linetype ="dotted",
        segment.size = 0.5,        # Thickness of the line
        #segment.curvature = -0.1,  # Add a slight "wiggle" or curve to the line
        segment.ncp = 3,
        min.segment.length = 0     # Force the line to show even if the label is close
      )
    }
  } else {
    y_range <- diff(range(df_res$quantum_1stkind_smoothed))
    
    p <- p + geom_text_repel(
      data = df_labels,
      aes(label = country),
      size = 4,
      fontface = "bold",
      nudge_y = y_range * 0.05,    # 5% of the data range, adapts automatically
      min.segment.length = 0
    )
  }
  
  p + labs(title=Title, y=yTitle, x=xTitle) + theme_text()
}

plot_mean <- function(df_res=res_BirthBirth1, vecCountry=NULL, facet=TRUE, yLimit=NULL, Title=NULL, yTitle="mean duration", xTitle=NULL) {
   return (plot_ppr(df_res=df_res, vecCountry=vecCountry, facet=facet, yLimit=yLimit, Title=Title, yTitle=yTitle, xTitle=xTitle,
                    yVar="mac_1stkind_smoothed", yVar_min="ci95_lower_mac_1stkind_smoothed", yVar_max="ci95_upper_mac_1stkind_smoothed"))
}

plotBySurvey <- function (df_toPlot=NULL, varEnter=NULL, varEvent=NULL, varCens="cmc_survey", varCountry="country",
                          varWeight="weight",
                          vecCountry=NULL, duration=FALSE,
                          res_firstYearsToDiscard=15, res_finalYearsToDiscard=1, res_countrySpecific_numYears=NULL,
                          yLimit=c(0,1), mySpan=0.75,
                          useRelativeWeights=FALSE,ageTruncate=NULL,
                          Title=NULL, yTitle="PPR", xTitle=NULL) {
  
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  
  surveys <- names(table(df_toPlot$surveyName))
  if (exists("res_all")) {rm(res_all)}
  for (indSurvey in (1:length(surveys))) {
    print (paste("Survey:", surveys[indSurvey]))
    df_toPlot_survey <- subset(df_toPlot, surveyName==surveys[indSurvey])
    res_survey <- calc_ppr(df=df_toPlot_survey, varEnter=varEnter, varEvent=varEvent, varCens=varCens, varCountry=varCountry,
                           vecCountry=vecCountry, varWeight=varWeight, duration=duration,
                           res_firstYearsToDiscard=res_firstYearsToDiscard, res_finalYearsToDiscard=res_finalYearsToDiscard,
                           res_countrySpecific_numYears=res_countrySpecific_numYears,
                           useRelativeWeights=useRelativeWeights,ageTruncate=ageTruncate,mySpan=mySpan)
    res_survey$surveyName=surveys[indSurvey]
    if (exists("res_all")) {
      res_all <- rbind(res_all, res_survey)
    } else {
      res_all <- res_survey
    }
  }
  plotIt <- plot_ppr(df_res=res_all, vecCountry=NULL, yLimit=yLimit, Title=Title, yTitle=yTitle, xTitle=xTitle)
  
  return (list(plot=plotIt,results=res_all))
}

#determine the maximum number of years for computing PPRs with a survey
pprNumYears <- function (df=NULL, type="fertility", varEvent=NULL, mute=FALSE) {
  #df is a fertility survey which should have a variable named "ageSurvey"
  #type is one of c("fertility", "separation1", "separation2")
  #if type is "fertility", the minimum age is 40 years
  #if type is "separation1", the minimum age is 40 years
  #if type is "separation2", the minimum age is 40 years
  #if 'mute' is FALSE, then the function prints the limit for the survey, if there is a variable named 'country'
  ageLimit <- list("fertility"=40, "separation1"=40, "separation2"=40)
  tab <- table(df$ageSurvey)
  n30_39 <- mean(tab[which(names(tab) %in% (30:39))])
  tab <- subset(tab, tab > n30_39 * 0.5)
  maxTab <- as.numeric(max(names(tab)))
  numYears <- maxTab - ageLimit[[type]]
  if (!is.null(varEvent)&("lastYear" %in% names(df))) {
    tabEvent <- table(df[, varEvent])
    maxTabEvent <- as.numeric(max(names(tabEvent)))
    lastYear <- min(df$lastYear)
    numYears <- numYears + maxTabEvent - lastYear - 1
  }
  if (numYears < 2) {numYears <- 3}
  if (!is.null(df$country)) {
    aCountry <- df$country[1]
  } else {
    aCountry <- "country"
  }
  if (!is.null(df$surveyName)) {
    name <- df$surveyName[1]
  } else {
    name <- "survey"
  }
  if (!mute) {
    print (paste(aCountry, ", survey:", name, ", last age is", maxTab, ", num years is", numYears))
  }
  
  return (data.frame(country=aCountry, surveyName=name, years=numYears))
}

buildSpecificYears <- function (dfsurveys=NULL, type=NULL, varEvent=NULL) {
  if ((exists("DEBUG1")) && (isTRUE(DEBUG1))) browser()
  res <- data.frame(country=NULL, surveyName=NULL, years=NULL)
  countries <- names (table (dfsurveys$country))
  for (indCountry in (1:length(countries))) {
    dfCountry <- subset (dfsurveys, country==countries[indCountry])
    dfCountry$surveyName <- factor(dfCountry$surveyName)
    surveys <- names (table(dfCountry$surveyName))
    for (indSurvey in (1:length(surveys))) {
      dfSurvey <- subset(dfCountry, surveyName==surveys[indSurvey])
      resSurvey <- pprNumYears(dfSurvey, type, varEvent=varEvent)
      res <- rbind(res, resSurvey)
    }
  }
  return (res)
}

agesByYear <- function(df=union1_sep1, varEvent="ySep1") {
  # compute the range of ages for the range of year of events
  if ("country" %in% names(df)) {
    df$country <- factor (df$country)
    cat <- table(df$country)
    countries <- names(cat)
  } else {
    countries <- "country"
  }
  agesByYear_tot <- data.frame()
  for (aCountry in countries) {
    dfC <- subset (df, country==aCountry)
    rangeYearEvents <- range(dfC[[varEvent]], na.rm=TRUE)
    rangeYears <- rangeYearEvents[1]:rangeYearEvents[2]
    agesByYear <- data.frame(country=rep(aCountry,length(rangeYears)),year=rangeYears, ageMin=NA, ageMax=NA)
    # create columns for each year in the range and put the age of individuals in each column
    cols <- as.character(rangeYears)
    dfC[, cols] <- NA
    dfC[, cols] <- sapply(rangeYears,
                         function(y) {
                           age <- y - dfC$yBirth
                           age <- ifelse(age < 0, NA, age)
                           age <- ifelse(age > dfC$ageSurvey, NA, age)
                           return (age)
                         })
    # loop
    # for (year in rangeYears) {
    #   agesByYear$ageMin[agesByYear$year == year] <- min(dfC[[as.character(year)]], na.rm=TRUE)
    #   agesByYear$ageMax[agesByYear$year == year] <- max(dfC[[as.character(year)]], na.rm=TRUE)
    # }
    
    # the vectorized version of the loop
    # 1. Calculate all mins and maxes at once (returns a named vector)
    all_mins <- sapply(dfC[cols], min, na.rm = TRUE)
    all_maxs <- sapply(dfC[cols], max, na.rm = TRUE)
    
    # 2. Map them into your summary table using the 'year' as an index
    agesByYear$ageMin <- all_mins[as.character(agesByYear$year)]
    agesByYear$ageMax <- all_maxs[as.character(agesByYear$year)]
    
    agesByYear_tot <- rbind(agesByYear_tot, agesByYear)
  }
  
  return (agesByYear_tot)
}
