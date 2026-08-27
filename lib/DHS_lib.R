## compute indicators from DHS
library(dplyr)

getCmc <- function(month, year) {
  #vectorized
  if (length(month)!=length(year)) stop("month and year should have the same length")
  return ( (year-1900) * 12 + month )  
}

decmc <- function (cmcDate) {
  #vectorized
  year <- trunc ( (cmcDate-1) / 12 )
  month <- cmcDate - year * 12
  return (list(month=month, year=year+1900))
}

computeYearBirth <- function(df) {
  #some DHS files have no year of birth value, only the cmc value
  maxB <- max (df$nBirthsTot)
  for (ind in (1:maxB)) {
    varYear <- paste ("yBirthChild", ind, sep="")
    varCmc <- paste ("cmcBirthChild", ind, sep="")
    df[varYear] <- decmc(df[varCmc])$year
  }
  return (df)
}

kDurYear <- 12
kDurSemester <- 6
kDurQuarter <- 3
kDurMonth <- 1

#### naming conventions ####
#We compute from DHS recoded files
#We change the name of variables (not important)
#But the main change is that we REVERSE the order of the birth history
#(the DHS files use a reverse order, with the last birth as the first entry in the history)
#the most important changes for the variable names are the following:
varName_cmcSurvey <- "cmcSurvey" #corresponds to DHS variable V008
varName_Weight <- "weight" #corresponds to DHS variable V005

#### Utilies ####
cmcToDur <- function(cmc, lengthPeriod=12) {
  #vectorized
  date <- decmc (cmc)
  return ( (date$year * 12 / lengthPeriod) + trunc( (date$month-1) / lengthPeriod) )
}

calcSpan <- function (df, var, lengthPeriod=12) {
  endYear <- decmc(max(df[var], na.rm = TRUE))$year
  beginYear <- decmc(min(df[var], na.rm = TRUE))$year
  return ( (endYear - beginYear + 1) * 12 / lengthPeriod )
}

calcFirstDateEvent <- function (df, var, lengthPeriod=12) {
  return ( cmcToDur ( min(df[var], na.rm = TRUE), lengthPeriod ) )
}

calcLastDateEvent <- function (df, var, lengthPeriod=12) {
  return ( cmcToDur ( max(df[var], na.rm = TRUE), lengthPeriod ) )
}

#### new DHS algorithm ####
#constructs yearly time series of birth from DHS individual recode data file
getBirthSeries <- function(df, useWeights=TRUE, byOrder=TRUE, byRegion=TRUE, varWeight="weight") {
  nWomen <- dim(df)[1]
  maxB <- max (df$nBirthsTot)
  vecVariables <- c()
  for (order in (1:maxB)) {
    vecVariables <- c(vecVariables, paste ("yBirthChild", order, sep=""))
  }
  nVars <- length(vecVariables)
  minYear <- 9999
  maxYear <- 0
  for (ind in (1:nVars)) {
    #ind=1
    minYear <- min(minYear, min(df[vecVariables[ind]], na.rm=TRUE))
    maxYear <- max(maxYear, max(df[vecVariables[ind]], na.rm=TRUE))
  }
  resDF <- data.frame(year=seq(minYear, maxYear, 1))
  if (!byOrder) resDF$births <- 0
  if (byRegion) {
    nRegion <- length (table(df$regionResidence))
    resDF$region <- 1
    temp <- resDF
    for (ind in (2:nRegion)) {
      temp$region <- ind
      resDF <- rbind(resDF, temp)
    }
    rm (temp)
    myRegion <- 0
  } else {
    resDF$region <- 1
    myRegion <- 1
  }
  weightData <- df[[varWeight]]
  if (!useWeights) weightData[] <- 1
  for (ind in (1:nVars)) {
    #ind=1
    if (byOrder) {
      nSer <- paste("b", ind, sep="")
      resDF[nSer] <- 0
      colBirths <- ind+2
    } else {
      colBirths <- 3
    }
    vecData <- pull(df, vecVariables[ind])
    for (w in (1:nWomen)) {
      #w=1
      if (!is.na(vecData[w])) {
        year <- vecData[w]
        if (byRegion) {
          myRegion <- df$regionResidence[w]
        }
        resDF[(resDF$region==myRegion)&(resDF$year==year), colBirths] <- resDF[(resDF$region==myRegion)&(resDF$year==year), colBirths] + weightData [w]
      }
    }
  }
  return (resDF)
}

# vectorized version of womenCountByAgeByYear function
womenCountByAgeByYear2 <- function (df, 
                                   cmcDateBirth_str="cmcBirthEgo", 
                                   cmcSurvey_str="cmcSurvey", 
                                   weight_str="weight", 
                                   numYear=15, 
                                   useWeights=TRUE) {
  
  if ((exists("DEBUG2")) && (isTRUE(DEBUG2))) browser()
  
  base <- df[cmcDateBirth_str]
  base <- cbind(base, df[cmcSurvey_str])
  base <- cbind(base, df[weight_str])
  colnames(base) <- c("cmcBirth", "cmcSurvey", "weight")
  if (!useWeights) base$weight <- 1
  lastYear <- calcLastDateEvent(df, cmcSurvey_str)
  firstAge <- 10
  
  dateSurvey <- decmc(base$cmcSurvey)
  base$fractionLastYear   <- (dateSurvey$month - 0.5) / 12
  base$birthYear          <- decmc(base$cmcBirth)$year
  base$yearOfSurvey       <- dateSurvey$year
  base$ageAt_1st_Jan_survey <- dateSurvey$year - decmc(base$cmcBirth)$year
  
  maxAge   <- max(base$ageAt_1st_Jan_survey)
  ageSpan  <- firstAge:maxAge
  timeSpan <- (lastYear - numYear + 1):lastYear
  nAge  <- length(ageSpan)
  nYear <- length(timeSpan)
  
  womenCount <- matrix(0, nrow=nAge, ncol=nYear)
  rownames(womenCount) <- ageSpan
  colnames(womenCount) <- timeSpan
  
  nWomen <- nrow(base)
  for (w in 1:nWomen) {
    bYear           <- base$birthYear[w]
    lastCompleteAge <- base$ageAt_1st_Jan_survey[w] - 1
    
    # Complete years: woman lived through age `age` entirely in year (bYear + age)
    for (age in firstAge:lastCompleteAge) {
      ageInd  <- age - firstAge + 1
      # FIX: use bYear + age directly, not via yearReachingFirstAge + ageInd
      # which introduced a spurious extra +1
      yearInd <- bYear + age - timeSpan[1] + 1
      if (ageInd >= 1 & ageInd <= nAge & yearInd >= 1 & yearInd <= nYear) {
        womenCount[ageInd, yearInd] <- womenCount[ageInd, yearInd] + base$weight[w]
      }
    }
    
    # Partial year: age at Jan 1st of survey year, fraction of year up to interview
    age     <- base$ageAt_1st_Jan_survey[w]
    ageInd  <- age - firstAge + 1
    yearInd <- bYear + age - timeSpan[1] + 1
    if (ageInd >= 1 & ageInd <= nAge & yearInd >= 1 & yearInd <= nYear) {
      womenCount[ageInd, yearInd] <- womenCount[ageInd, yearInd] +
        base$weight[w] * base$fractionLastYear[w]
    }
  }
  
  return(womenCount)
}

# slightly vectorized version of birthsCountByAgeByYear function
birthsCountByAgeByYear2 <- function (df=NULL, tableOut=NULL,
                                     cmcBirthEgo_str="cmcBirthEgo",
                                     cmcBirthChild_str="cmcBirthChild1",
                                     weight_str="weight",
                                     numYear=15, useWeights=TRUE) {
  
  if (exists("DEBUG2") && isTRUE(DEBUG2)) browser()
  
  if (is.null(df) | is.null(tableOut)) stop("One parameter is null")
  
  tableOut[,] <- 0
  timeSpan <- as.numeric(colnames(tableOut))
  ageSpan  <- as.numeric(rownames(tableOut))
  nAge  <- length(ageSpan)
  nYear <- length(timeSpan)
  
  indColBirthEgo <- which(colnames(df) == cmcBirthEgo_str)
  indColWeight   <- which(colnames(df) == weight_str)
  
  if (!useWeights) df[, indColWeight] <- 1
  
  nWomen <- dim(df)[1]
  
  for (ind in (1:length(cmcBirthChild_str))) {
    indColChild <- which(colnames(df) == cmcBirthChild_str[ind])
    for (w in (1:nWomen)) {
      if (is.na(df[w, indColChild])) next
      yearBirthEgo   <- decmc(df[w, indColBirthEgo])$year
      yearBirthChild <- decmc(df[w, indColChild])$year
      # BUG 3 FIXED: age is now computed as age at January 1st of the birth year
      # (yearBirthChild - yearBirthEgo), consistent with how womenCount attributes
      # exposure: a woman born in year B is counted at age (y - B) during year y.
      # Previously this was identical numerically, but the comment and intent were
      # misleading; making the convention explicit guards against future drift.
      ageBirth <- yearBirthChild - yearBirthEgo
      ageInd  <- ageBirth - ageSpan[1] + 1
      yearInd <- yearBirthChild - timeSpan[1] + 1
      # BUG 2 FIXED (applied here too): guard against out-of-range indices
      if (ageInd > 0 & ageInd <= nAge & yearInd > 0 & yearInd <= nYear) {
        tableOut[ageInd, yearInd] <- tableOut[ageInd, yearInd] + df[w, indColWeight]
      }
    }
  }
  
  return(tableOut)
}


womenCountByAgeByYear <- function (df, 
                                   cmcDateBirth_str="cmcBirthEgo", 
                                   cmcSurvey_str="cmcSurvey", 
                                   weight_str="weight", 
                                   numYear=15, 
                                   useWeights=TRUE) {
  #Create a two dimensional table (by age and by year) of person-year for women (for computing fertility rates)
  #Age varies from 10 to maxAge (we determine maxAge at runtime from the info in the file)
  #Time varies from lastYear - numYear to lastYear, where lastYear is the lastYear when an interview took place
  #For lastYear, most of the women will spend less than one year, so we ponderate accordingly:
  #     if a woman was interviewed in april of lastYear, we will suppose she was interviewed at mid month and
  #     will suppose that she was present 3 months and half or a fraction of 105 / 365 year
  #Age is computed as age reached at mid year
  #We suppose that the interviews were conducted in at most two calendar years, for example 2001-2002
  
  if ((exists("DEBUG2")) && (isTRUE(DEBUG2))) browser()
  
  base <- df[cmcDateBirth_str]
  base <- cbind(base, df[cmcSurvey_str])
  base <- cbind(base, df[weight_str])
  colnames(base) <- c("cmcBirth", "cmcSurvey", "weight")
  if (!useWeights) base$weight <- 1
  lastYear <- calcLastDateEvent(df, cmcSurvey_str)
  firstAge <- 10
  
  #women info  
  dateSurvey <- decmc (base$cmcSurvey)
  base$fractionLastYear <- (dateSurvey$month - 0.5) / 12
  base$yearReachingFirstAge <- decmc(base$cmcBirth)$year + firstAge
  base$yearOfSurvey <- dateSurvey$year
  base$ageAt_1st_January_lastYear <- dateSurvey$year - decmc (base$cmcBirth)$year
  
  minAge <- min (base$ageAt_1st_January_lastYear)
  maxAge <- max (base$ageAt_1st_January_lastYear)
  ageSpan <- firstAge:maxAge
  timeSpan <- (lastYear-numYear+1):lastYear
  nAge <- length(ageSpan)
  nYear <- length(timeSpan)
  
  womenCount <- matrix(rep(0, nAge * nYear), c(nAge, nYear))
  rownames(womenCount) <- ageSpan
  colnames(womenCount) <- timeSpan
  
  nWomen <- dim(base)[1]
  for (w in (1:nWomen)) {
    #w=100000
    lastCompleteAge <- base$ageAt_1st_January_lastYear[w] - 1
    for (age in (firstAge:lastCompleteAge)) {
      ageInd <- age - firstAge + 1
      yearInd <- base$yearReachingFirstAge[w] + ageInd - timeSpan[1]
      if (yearInd > 0) {
        womenCount[ageInd, yearInd] <- womenCount[ageInd, yearInd] + base$weight[w]
      }
    }
    ageInd <- base$ageAt_1st_January_lastYear[w] - firstAge + 1
    yearInd <- base$yearReachingFirstAge[w] + base$ageAt_1st_January_lastYear[w] - firstAge - timeSpan[1] + 1
    womenCount[ageInd, yearInd] <- womenCount[ageInd, yearInd] + base$weight[w] * base$fractionLastYear[w]
  }
  
  return (womenCount)
}

birthsCountByAgeByYear <- function (df=NULL, tableOut=NULL, cmcBirthEgo_str="cmcBirthEgo", cmcBirthChild_str="cmcBirthChild1", weight_str="weight", numYear=15, useWeights=TRUE) {
  #cmcBirthChild_str can be a vector of 'cmcBirthChild' dates. If this is the case, it will add the births of all orders
  
  if (exists("DEBUG2") && isTRUE(DEBUG2)) browser()
  
  if (is.null(df) | is.null (tableOut)) stop ("One parameter is null")
  
  tableOut[,] <- 0
  timeSpan <- as.numeric (colnames (tableOut))
  ageSpan <- as.numeric (rownames (tableOut))
  
  indColBirthEgo <- which(colnames(df)==cmcBirthEgo_str)
  indColWeight <- which(colnames(df)==weight_str)
  
  if (!useWeights) df[,indColWeight] <- 1
  
  nWomen <- dim(df)[1]
  
  for (ind in (1:length(cmcBirthChild_str))) {
    indColChild <- which(colnames(df)==cmcBirthChild_str[ind])
    for (w in (1:nWomen)) {
      #w=2
      if (is.na(df[w, indColChild])) next
      yearBirthEgo <- decmc(df[w, indColBirthEgo])$year
      yearBirthChild <- decmc(df[w, indColChild])$year
      #age reached in the year of birth of the child
      ageBirth <- yearBirthChild - yearBirthEgo
      ageInd <- ageBirth - ageSpan[1] + 1
      yearInd <- yearBirthChild - timeSpan[1] + 1
      if ((ageInd > 0) & (yearInd > 0)) {
        result <- try({
          tableOut[ageInd, yearInd] <- tableOut[ageInd, yearInd] + df[w, indColWeight]
        }, silent = TRUE)
        if (inherits(result, "try-error")) {
          cat("Error with ageInd",ageInd,"or yearInd:",yearInd,"\n")
        }
      }
    }
  }
  
  return (tableOut)
}

computeTFRbyOrder <- function(df, womenCount, orders=1, useWeights=TRUE, numLastYears=15, weight_str="weight") {
  #'orders' can be a vector of birth orders. In that case we will add all the births of all orders
  
   if (is.null(df) | is.null (womenCount)) stop ("One parameter is null")
  
  timeSpan <- as.numeric ( colnames (womenCount) )
  ageSpan <- as.numeric ( rownames (womenCount) )
  
  childVars <- c()
  for (order in orders) {
    childVars <- c( childVars, paste ("cmcBirthChild", order, sep="") )
  }
  # we pass womenCount as tableOut, because we reuse the matrix structure and the row and column names, but we will fill it with births
  birthCount <- birthsCountByAgeByYear2(df, tableOut=womenCount, cmcBirthChild_str=childVars, numYear=numLastYears,
                                        useWeights=TRUE, weight_str=weight_str)

  tableRates <- birthCount / womenCount
  tableRates_Var <- birthCount / (womenCount^2) # variance formula for Poisson counts
  TFR <- colSums(tableRates, na.rm=TRUE)
  TFR_var <- colSums(tableRates_Var, na.rm=TRUE)
  meanAges <- c()
  meanAges_var <- c()
  for (yearInd in (1:length(timeSpan))) {
    if (TFR[yearInd] > 0) {
      mA <- sum(ageSpan * tableRates[, yearInd], na.rm = TRUE) / TFR[yearInd]
      mA_var <- sum((ageSpan - mA)^2 * tableRates_Var[, yearInd], na.rm = TRUE) / TFR[yearInd]^2
    } else {
      mA <- 0
      mA_var <- 0
    }
    meanAges <- c(meanAges, mA)
    meanAges_var <- c(meanAges_var, mA_var)
  }
  
  loessWeights <- ifelse(TFR_var > 0, 1 / TFR_var, 0)
  loessWeights_mac <- ifelse(meanAges_var > 0, 1 / meanAges_var, 0)
  
  # confidence interval for TFR
  EE <- TFR_var ^(1/2) / TFR
  EF <- exp(1.96 * EE * log(TFR))
  TFR_min <- TFR / EF
  TFR_max <- TFR * EF
  
  res <- data.frame(year=timeSpan, order=rep(orders[1], numLastYears),
                    tfr=TFR, tfr_min=TFR_min, tfr_max=TFR_max, tfr_var=TFR_var,
                    meanAge=meanAges, meanAge_min=meanAges - 1.96 * sqrt (meanAges_var), meanAge_max=meanAges + 1.96 * sqrt (meanAges_var),
                    meanAge_var=meanAges_var,
                    w_TFR=loessWeights, w_MAC=loessWeights_mac)
  
  return (res)
}

smoothTFR <- function (aListTFRbyOrder=NULL, cSpan=1) {
  
  for (ind in (1:length(aListTFRbyOrder))) {
    ser <- aListTFRbyOrder[[ind]]
    serOut <- data.frame(year=ser$year)
    serOut$order <- ser$order
    res <- aLoessSmoothedValues (data.frame(Year=ser$year, Value=ser$tfr, Weights=ser$w_TFR), cSpan=cSpan)
    serOut$tfr <- ser$tfr
    serOut$tfr_min <- ser$tfr_min
    serOut$tfr_max <- ser$tfr_max
    serOut$tfr_var <- ser$tfr_var
    serOut$tfr_smooth_min <- res$min
    serOut$tfr_smooth <- res$Value
    serOut$tfr_smooth_max <- res$max
    res <- aLoessSmoothedValues (data.frame(Year=ser$year, Value=ser$meanAge, Weights=ser$w_MAC), cSpan=cSpan)
    serOut$meanAge <- ser$meanAge
    serOut$meanAge_min <- ser$meanAge_min
    serOut$meanAge_max <- ser$meanAge_max
    serOut$meanAge_var <- ser$meanAge_var
    serOut$meanAge_smooth_min <- res$min
    serOut$meanAge_smooth <- res$Value
    serOut$meanAge_smooth_max <- res$max
    
    serOut$nBirths <- ser$nBirths
    aListTFRbyOrder[[ind]] <- serOut
  }
  
  return (aListTFRbyOrder)
}

# compute with two different weights: individual and population
# first weight: individual weights, used for variance and confidence intervals
# second weight: population weights, used for point estimates of TFR and mean age
# should have two weights in weight_str: we don't check that
computeTFR_2weights <- function(df=NULL,
                       useWeights=TRUE, weight_str=c("weight", "popWeight"),
                       numLastYears=15, orderPlus=10,
                       smooth=TRUE, cSpan=1, confidenceBand_loess=TRUE,
                       removeLastYear=FALSE, numLastYears_toRemove=0) {
  
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  
  aListTFRbyOrder_indivWeight <- computeTFR(df=df, useWeights=TRUE, weight_str=weight_str[1],
                                            numLastYears=numLastYears, orderPlus=orderPlus,
                                            smooth=smooth, cSpan=cSpan,
                                            removeLastYear=removeLastYear, numLastYears_toRemove=numLastYears_toRemove
  )
  aListTFRbyOrder_popWeight <- computeTFR(df=df, useWeights=TRUE, weight_str=weight_str[2],
                                            numLastYears=numLastYears, orderPlus=orderPlus,
                                            smooth=smooth, cSpan=cSpan,
                                            removeLastYear=removeLastYear, numLastYears_toRemove=numLastYears_toRemove
  )
  aListTFRbyOrder <- aListTFRbyOrder_popWeight
  if (smooth == FALSE) confidenceBand_loess <- FALSE
  if (isTRUE(smooth)) {
    if (isTRUE(confidenceBand_loess)) {
      for (ord in (1:length(aListTFRbyOrder_indivWeight))) {
        aListTFRbyOrder[[ord]]$tfr_smooth_min <- aListTFRbyOrder_popWeight[[ord]]$tfr_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$tfr_smooth_min /
          aListTFRbyOrder_indivWeight[[ord]]$tfr_smooth
        aListTFRbyOrder[[ord]]$tfr_smooth_max <- aListTFRbyOrder_popWeight[[ord]]$tfr_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$tfr_smooth_max /
          aListTFRbyOrder_indivWeight[[ord]]$tfr_smooth
        aListTFRbyOrder[[ord]]$meanAge_smooth_min <- aListTFRbyOrder_popWeight[[ord]]$meanAge_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_smooth_min /
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_smooth
        aListTFRbyOrder[[ord]]$meanAge_smooth_max <- aListTFRbyOrder_popWeight[[ord]]$meanAge_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_smooth_max /
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_smooth
      }
    } else {
      for (ord in (1:length(aListTFRbyOrder_indivWeight))) {
        aListTFRbyOrder[[ord]]$tfr_smooth_min <- aListTFRbyOrder_popWeight[[ord]]$tfr_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$tfr_min /
          aListTFRbyOrder_indivWeight[[ord]]$tfr
        aListTFRbyOrder[[ord]]$tfr_smooth_max <- aListTFRbyOrder_popWeight[[ord]]$tfr_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$tfr_max /
          aListTFRbyOrder_indivWeight[[ord]]$tfr
        aListTFRbyOrder[[ord]]$meanAge_smooth_min <- aListTFRbyOrder_popWeight[[ord]]$meanAge_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_min /
          aListTFRbyOrder_indivWeight[[ord]]$meanAge
        aListTFRbyOrder[[ord]]$meanAge_smooth_max <- aListTFRbyOrder_popWeight[[ord]]$meanAge_smooth *
          aListTFRbyOrder_indivWeight[[ord]]$meanAge_max /
          aListTFRbyOrder_indivWeight[[ord]]$meanAge
      }
    }
  }
  for (ord in (1:length(aListTFRbyOrder_indivWeight))) {
    aListTFRbyOrder[[ord]]$tfr_min <- aListTFRbyOrder_popWeight[[ord]]$tfr *
      aListTFRbyOrder_indivWeight[[ord]]$tfr_min /
      aListTFRbyOrder_indivWeight[[ord]]$tfr
    aListTFRbyOrder[[ord]]$tfr_max <- aListTFRbyOrder_popWeight[[ord]]$tfr *
      aListTFRbyOrder_indivWeight[[ord]]$tfr_max /
      aListTFRbyOrder_indivWeight[[ord]]$tfr
    aListTFRbyOrder[[ord]]$meanAge_min <- aListTFRbyOrder_popWeight[[ord]]$meanAge *
      aListTFRbyOrder_indivWeight[[ord]]$meanAge_min /
      aListTFRbyOrder_indivWeight[[ord]]$meanAge
    aListTFRbyOrder[[ord]]$meanAge_max <- aListTFRbyOrder_popWeight[[ord]]$meanAge *
      aListTFRbyOrder_indivWeight[[ord]]$meanAge_max /
      aListTFRbyOrder_indivWeight[[ord]]$meanAge
  }
  
  
  return (aListTFRbyOrder)
}

computeTFR <- function(df=NULL,
                       useWeights=TRUE, weight_str="weight",
                       numLastYears=15, orderPlus=10,
                       smooth=TRUE, cSpan=1,
                       removeLastYear=FALSE, numLastYears_toRemove=0) {
  #df<-temp
  #orderPlus is the last birth order, of type 'orderPlus and plus'
  if (is.null(df) ) stop ("One parameter is null")
  
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  
  if (isTRUE(removeLastYear)) {
    numLastYears <- numLastYears + 1
  }
  womenCount <- womenCountByAgeByYear2(df, numYear=numLastYears, useWeights=useWeights, weight_str=weight_str)
  maxB <- max(df$nBirthsTot)
  resTot <- computeTFRbyOrder (df, womenCount, orders=(1:maxB), useWeights=useWeights, weight_str=weight_str, numLastYears=numLastYears)
  resTot$order <- 0 # TFR total will have order 0
  if (isTRUE(removeLastYear)) {
    resTot <- head(resTot, -1)
  }
  aListTFRbyOrder <- list(resTot)
  
  for (order in (1:(orderPlus-1))) {
    #order=2
    res <- computeTFRbyOrder (df, womenCount, orders=order, useWeights=useWeights, weight_str=weight_str, numLastYears=numLastYears)
    if (isTRUE(removeLastYear)) {
      res <- head(res, -1)
    }
    aListTFRbyOrder <- append (aListTFRbyOrder, list(res))
  }
  resPlus <- computeTFRbyOrder (df, womenCount, orders=(orderPlus:maxB), useWeights=useWeights, weight_str=weight_str, numLastYears=numLastYears)
  resPlus$order <- orderPlus
  if (isTRUE(removeLastYear)) {
    resPlus <- head(resPlus, -1)
  }
  aListTFRbyOrder <- append (aListTFRbyOrder, list(resPlus))
  
  namesList <- c("Total")
  for (order in (1:(orderPlus-1))) {
    namesList <- c(namesList, paste("TFR", order, sep=""))
  }
  namesList <- c(namesList, paste("TFR", orderPlus, "plus", sep=""))
  names (aListTFRbyOrder) <- namesList
  
  if (smooth) {
    if (cSpan==1) cSpan=(15 / numLastYears)
    aListTFRbyOrder <- smoothTFR (aListTFRbyOrder, cSpan=cSpan)
  }
  
  numLastYears_toRemove <- min(numLastYears_toRemove, (numLastYears-6))
  if (numLastYears_toRemove > 0) {
    for (ind in (1:length(aListTFRbyOrder))) {
      aListTFRbyOrder[[ind]] <- head(aListTFRbyOrder[[ind]], -numLastYears_toRemove)
    }
  }
  return (aListTFRbyOrder)
}

computeAllRegions <- function (df) {
  TFRallRegions <- list()
  for (region in DHS_regions) {
    temp <- subset(df, regionResidence==region)
    TFRallByReg <- computeTFR(temp, numLastYears=50, orderPlus=5)
    nm <- c(names(TFRallRegions), paste("reg", region, sep=""))
    TFRallRegions <- append(TFRallRegions, list(TFRallByReg))
    names(TFRallRegions) <- nm
  }
  
  return (TFRallRegions)
}

#compute Period Parity Progression Ratio (or transition from one state to the following one) by the life table method
computePPR <- function(df, firstEvent_str, secondEvent_str, kDuration=12, useWeights=TRUE, weight_str="weight", maxRateAllowed=0.25, numLastYears=NULL, smooth=TRUE) {
  #df: a codified DHS survey file (or a subset of it)
  #firstEvent_str, secondEvent_str: string values with the name of two variables in DHS Century-Month-Code format
  #   from which we compute the transition probability
  #kDuration: 12 will compute yearly rates, 6 rates by semester, 3 rates by quarter, 1 monthly rates
  #useWeights: will use the 'weight' variable of DHS (observe that the name has been changed from 'V005' to 'weight')
  #also observe that we have changed the name of 'V008' to 'cmcSurvey'
  #maxRateAllowed: we put a ceiling on the rates in order to limit volatility when we compute rates with small numbers
  #numLastYears: return PPPRs value for the specified number of last years: numLastYears
  
  if (is.null(numLastYears)) {smooth=FALSE}
  
  computeEventsCounts <- function (base, useWeights=useWeights, weight_str="weight") {
    
    if (!useWeights) base[[weight_str]] <- 1
    
    eventsCount <- matrix(rep(0, firstEventSpan * secondEventSpan), c(firstEventSpan, secondEventSpan))
    rownames(eventsCount) <- seq(firstDateFirstEvent, firstDateFirstEvent + firstEventSpan - 1, 1)
    colnames(eventsCount) <- seq(firstDateSecondEvent, firstDateSecondEvent + secondEventSpan - 1, 1)
    for (ind in (1:nWomen)) {
      beginDate <- base$dateFirstEvent[ind]
      eventDate <- base$dateSecondEvent[ind]
      
      if (!is.na(eventDate)) {
        eventsCount [beginDate - firstDateFirstEvent + 1, eventDate - firstDateSecondEvent + 1] <-
          eventsCount [beginDate - firstDateFirstEvent + 1, eventDate - firstDateSecondEvent + 1] + base[[weight_str]][ind]
      }
    }
    
    return (eventsCount)
  }
  
  base <- df[firstEvent_str]
  base <- cbind(base, df[secondEvent_str])
  base <- cbind(base, df["cmcSurvey"])
  colnames(base) <- c("firstEvent", "secondEvent", "cmcSurvey")
  base$weight <- df[[weight_str]]
  #truncate date of survey as a multiple in months of kDuration: we retain only complete periods for individuals:
  #if a person was interviewed in April of the year of survey, then, we will retain that period ONLY if we compute monthly or quarterly rates
  #if kDuration=6 (computing for semesters) or kDuration=12 (computing yearly rate), we will set the month of survey to zero
  #if kDuration=1 (month) or kDuration=3 (quarter) we will set the month to a multiple of kDuration (in that case to 4 for months or 3 for quarters)
  dateSurvey <- decmc (base$cmcSurvey)
  base$oldCmcSurvey <- base$cmcSurvey
  base$cmcSurvey <- getCmc(trunc((dateSurvey$month-1)/kDuration)*kDuration, dateSurvey$year)
  #debugging: verify new date of survey is OK
  dateSurvey <- decmc (base$cmcSurvey)
  base$monthSurvey <- dateSurvey$month
  base$yearSurvey <- dateSurvey$year
  
  base$dateFirstEvent <- cmcToDur(base$firstEvent, kDuration)
  base$dateSecondEvent <- cmcToDur(base$secondEvent, kDuration)
  base$dateSurvey <- cmcToDur(base$cmcSurvey, kDuration)
  #if a birth (or an event) occurs after the new value of date of survey, then we discard it
  base$dateFirstEvent <- ifelse(is.na(base$dateFirstEvent), 0, base$dateFirstEvent)
  base$dateFirstEvent <- ifelse(base$dateFirstEvent>base$dateSurvey, 0, base$dateFirstEvent)
  base$dateFirstEvent <- ifelse(base$dateFirstEvent==0, NA, base$dateFirstEvent)
  base$dateSecondEvent <- ifelse(is.na(base$dateSecondEvent), 0, base$dateSecondEvent)
  base$dateSecondEvent <- ifelse(base$dateSecondEvent>base$dateSurvey, 0, base$dateSecondEvent)
  base$dateSecondEvent <- ifelse(base$dateSecondEvent==0, NA, base$dateSecondEvent)
  
  base$dateSecondEvent_cens <- ifelse(is.na(base$dateSecondEvent), base$dateSurvey, base$dateSecondEvent)
  if (!useWeights) base$weight <- 1
  
  firstDateFirstEvent <- calcFirstDateEvent (df, firstEvent_str, kDuration)
  firstDateSecondEvent <- calcFirstDateEvent (df, secondEvent_str, kDuration)
  firstEventSpan <- max(base$dateFirstEvent, na.rm=TRUE) - min(base$dateFirstEvent, na.rm=TRUE) + 1
  secondEventSpan  <- max(base$dateSecondEvent_cens, na.rm=TRUE) - min(base$dateSecondEvent, na.rm=TRUE) + 1
  lastDateFirstEvent <- firstDateFirstEvent + firstEventSpan - 1
  lastDateSecondEvent <- firstDateSecondEvent + secondEventSpan - 1
  
  #life table of women
  womenCount <- matrix(rep(0, firstEventSpan * secondEventSpan), c(firstEventSpan, secondEventSpan))
  rownames(womenCount) <- seq(firstDateFirstEvent, firstDateFirstEvent + firstEventSpan - 1, 1)
  colnames(womenCount) <- seq(firstDateSecondEvent, firstDateSecondEvent + secondEventSpan - 1, 1)
  
  nWomen <- dim(base)[1]
  for (ind in (1:nWomen)) {
    #ind = 138608
    beginDate <- base$dateFirstEvent[ind]
    endDate <- base$dateSecondEvent_cens[ind]
    
    #no first event: that person is not included in the denominator: a count of persons-period
    if (is.na(beginDate)) next
    
    if (beginDate > firstDateSecondEvent) {
      addVec <- c(rep(0, beginDate - firstDateSecondEvent), rep(base$weight[ind], endDate - beginDate + 1))
    } else {
      addVec <- rep(base$weight[ind], endDate - firstDateSecondEvent + 1)
    }
    
    if (endDate - firstDateSecondEvent + 1 < secondEventSpan) {
      addVec <- c(addVec, rep(0, secondEventSpan - (endDate - firstDateSecondEvent + 1)))
    }
    
    womenCount [beginDate - firstDateFirstEvent + 1, ] <- womenCount [beginDate - firstDateFirstEvent + 1, ] + addVec
  }
  
  #count of events
  eventsCount <- computeEventsCounts (base, useWeights)
  eventsCount_noW <- computeEventsCounts (base, FALSE)
  sumEvents <- colSums(eventsCount_noW)
  
  #compute rates
  rates <- matrix(rep(0, firstEventSpan * secondEventSpan), c(firstEventSpan, secondEventSpan))
  rownames(rates) <- seq(firstDateFirstEvent, firstDateFirstEvent + firstEventSpan - 1, 1)
  colnames(rates) <- seq(firstDateSecondEvent, firstDateSecondEvent + secondEventSpan - 1, 1)
  
  rates <- eventsCount / womenCount
  
  #as we use small numbers with a survey, limit the maximum value of rates computed
  rates <- ifelse (rates > maxRateAllowed, maxRateAllowed, rates)
  
  #compute standard errors for the period survival function
  #first we compute 'greenwoods' for cohorts
  greenwoods <- matrix(rep(0, firstEventSpan * (secondEventSpan)), c(firstEventSpan, secondEventSpan))
  rownames(greenwoods) <- seq(firstDateFirstEvent, firstDateFirstEvent + firstEventSpan - 1, 1)
  colnames(greenwoods) <- seq(firstDateSecondEvent, firstDateSecondEvent + secondEventSpan - 1, 1)
  #TO BE DONE...
  
  #second we use these 'greenwood' number for the period life table
  #the confidence interval for PPRs will be the value at the end of the survival function
  #TO BE DONE...
  
  #compute PPPRs
  PPPRs <- rep(1, secondEventSpan)
  for (indRow in (1:firstEventSpan)) {
    for (indCol in (1:(secondEventSpan))) {
      if (!is.na(rates[indRow, indCol])) {
        PPPRs[indCol] <- PPPRs[indCol] * (1 - rates[indRow, indCol])
      }
    }
  }
  PPPRs <- 1 - PPPRs
  dateSpan <- seq(firstDateSecondEvent, firstDateSecondEvent + secondEventSpan - 1, 1)
  names(PPPRs) <- dateSpan
  
  if (!is.null(numLastYears)) {
    #for the moment we suppose that all computations are done in unit-year (kDuration is 12)
    if (kDuration == 12) {
      timeSpan <- c(lastDateSecondEvent - numLastYears + 1, lastDateSecondEvent)
      PPPRs <- PPPRs[(timeSpan[1] - firstDateSecondEvent + 1):(timeSpan[2] - firstDateSecondEvent + 1)]
      if (smooth) {
        res <- aLoessSmoothedValues (data.frame(Year=as.numeric(names(PPPRs)), Value=PPPRs, Weights=sumEvents[(length(sumEvents)-numLastYears+1):length(sumEvents)]))
        PPPRs <- res$Value
        names(PPPRs) <- dateSpan[(length(dateSpan)-numLastYears+1):length(dateSpan)]
        return (list(women=womenCount, events=eventsCount, rates=rates, PPR=PPPRs, PPR_min=res$min, PPR_max=res$max))
      }
    }
  }
  
  return (list(women=womenCount, events=eventsCount, rates=rates, PPR=PPPRs))
}

#compute PPPR from parity n_plus to n+1_plus (makes sense only for fertility, not transition between other states like unions)
computePPR_plus <- function(df,
                            vecFirsts=c("cmcBirthChild3", "cmcBirthChild4"), 
                            vecSeconds=c("cmcBirthChild4", "cmcBirthChild5"),
                            kDuration=12, useWeights=TRUE, weight_str="weight", numLastYears=NULL) {
  nTrans <- length(vecFirsts)
  if (nTrans != length(vecSeconds)) {stop ("The two vectors of variable names should have equal length")}
  out <- data.frame(firstEvent=NULL, secondEvent=NULL, cmcSurvey=NULL, weight=NULL)
  for (ind in (1:nTrans)) {
    vecData1 <- pull(df, vecFirsts[ind])
    vecData2 <- pull(df, vecSeconds[ind])
    out <- rbind(out, data.frame(firstEvent=vecData1, secondEvent=vecData2, cmcSurvey=df$cmcSurvey, weight=df$weight))
  }
  return (computePPR(out, "firstEvent", "secondEvent", kDuration, useWeights, weight_str="weight", numLastYears=numLastYears))
}

truncateFirsts <- function (aVec, numYears) {
  lVec <- length(aVec)
  if (numYears > lVec) stop ("truncateFirsts bad number of years")
  return (aVec[(lVec-numYears+1):lVec])
}

computePPR_fromTFR <- function(listTFR, highestOrder=NULL, fromSmooth=TRUE) {
  #listTFR <- TFRall
  #highestOrder=NULL
  #fromSmooth: we compute ppr from smoothed TFR by order
  if (is.null(highestOrder)) {
    highestOrder <- length(listTFR) - 2
  }
  #first one is equal to TF1
  if (fromSmooth) {
    listTFR$TFR1$ppr <- listTFR$TFR1$tfr_smooth
  } else {
    listTFR$TFR1$ppr <- listTFR$TFR1$tfr
  }
  listTFR$Total$tfr_ppr <- listTFR$TFR1$ppr
  lastTFR <- listTFR$Total$tfr_ppr
  for (order in (2:(highestOrder))) {
    if (fromSmooth) {
      listTFR[[order+1]]$ppr <- listTFR[[order+1]]$tfr_smooth / listTFR[[order]]$tfr_smooth
    } else {
      listTFR[[order+1]]$ppr <- listTFR[[order+1]]$tfr / listTFR[[order]]$tfr
    }
    lastTFR <- lastTFR * listTFR[[order+1]]$ppr
    listTFR$Total$tfr_ppr <- listTFR$Total$tfr_ppr + lastTFR
  }
  #last one
  if (fromSmooth) {
    listTFR[[highestOrder+2]]$ppr <- listTFR[[highestOrder+2]]$tfr_smooth / (listTFR[[highestOrder+2]]$tfr_smooth + listTFR[[highestOrder+1]]$tfr_smooth)
  } else {
    listTFR[[highestOrder+2]]$ppr <- listTFR[[highestOrder+2]]$tfr / (listTFR[[highestOrder+2]]$tfr + listTFR[[highestOrder+1]]$tfr)
  }
  lastTFR <- lastTFR * listTFR[[highestOrder+2]]$ppr / ( 1 - listTFR[[highestOrder+2]]$ppr)
  listTFR$Total$tfr_ppr <- listTFR$Total$tfr_ppr + lastTFR
  
  return (listTFR)
}

#### Compute PPPRs ####
computePPPRs <- function (df, numLastYears=NULL, firstOrderPlus=10, kDuration=12, useWeights=TRUE, weight_str="weight") {
  #df<-col2015
  # Compute first PPPR
  result <- computePPR (df, "cmcBirthEgo", "cmcBirthChild1", kDuration, numLastYears=numLastYears, useWeights=useWeights, weight_str=weight_str)
  PPPRs <- list("P0_1"=result$PPR)
  minMaxValues <- "PPR_min" %in% names(result)
  if (minMaxValues) {
    PPPRs_min <- list("P0_1"=result$PPR_min)
    PPPRs_max <- list("P0_1"=result$PPR_max)
  }
  # Compute second PPPR up to last individual, before the aggregated one
  for (ind in (1:(firstOrderPlus-1))) {
    #ind=7
    firstChild <- paste("cmcBirthChild", ind, sep="")
    secondChild <- paste("cmcBirthChild", ind+1, sep="")
    trans <- paste("P", ind, "_",  ind+1, sep="")
    result <- computePPR (df, firstChild, secondChild, kDuration, numLastYears=numLastYears, useWeights=useWeights, weight_str=weight_str)
    oldNames <- names(PPPRs)
    PPPRs <- append (PPPRs, list(result$PPR))
    names(PPPRs) <- c(oldNames, trans)
    if (minMaxValues) {
      PPPRs_min <- append(PPPRs_min, list(result$PPR_min))
      names(PPPRs_min) <- names(PPPRs)
      PPPRs_max <- append(PPPRs_max, list(result$PPR_max))
      names(PPPRs_max) <- names(PPPRs)
    }
  }
  # Compute final PPPR
  vec1 <- c()
  vec2 <- c()
  for (ind in (firstOrderPlus:19)) {
    vec1 <- c(vec1, paste("cmcBirthChild", ind, sep=""))
    vec2 <- c(vec2, paste("cmcBirthChild", ind+1, sep=""))
  }
  result <- computePPR_plus(df, vec1, vec2, numLastYears=numLastYears, useWeights=useWeights, weight_str=weight_str)
  trans <- paste("P", firstOrderPlus, "plus_",  firstOrderPlus+1, "plus", sep="")
  oldNames <- names(PPPRs)
  PPPRs <- append (PPPRs, list(result$PPR))
  names(PPPRs) <- c(oldNames, trans)
  if (minMaxValues) {
    PPPRs_min <- append(PPPRs_min, list(result$PPR_min))
    names(PPPRs_min) <- names(PPPRs)
    PPPRs_max <- append(PPPRs_max, list(result$PPR_max))
    names(PPPRs_min) <- names(PPPRs)
  }
  
  minYears <- 10000 #big value!
  for (ind in (1:length(PPPRs))) {
    minYears <- min (minYears, length (PPPRs [[ind]]))
  }
  for (ind in (1:length(PPPRs))) {
    PPPRs [[ind]] <- truncateFirsts (PPPRs [[ind]], minYears)
    if (minMaxValues) {
      PPPRs_min [[ind]] <- truncateFirsts (PPPRs_min [[ind]], minYears)
      PPPRs_max [[ind]] <- truncateFirsts (PPPRs_max [[ind]], minYears)
    }
  }
  
  results <- data.frame(year=names(unlist(PPPRs [[1]])))
  
  completeResults <- function(res=NULL, PPPRs=PPPRs, addStr="") {
    trans <- "P0_1"
    res[paste(trans, addStr, sep="")] <- PPPRs [[1]]
    TFR <- PPPRs [[1]]
    lastTFR <- TFR
    for (ind in (2:(firstOrderPlus))) {
      #ind=2
      trans <- paste("P", ind-1, "_",  ind, sep="")
      res[paste(trans, addStr, sep="")] <- PPPRs [[ind]]
      lastTFR <- lastTFR * PPPRs [[ind]]
      TFR <- TFR + lastTFR
    }
    trans <- paste("P", firstOrderPlus, "+_",  firstOrderPlus+1, "+", sep="")
    res[paste(trans, addStr, sep="")] <- PPPRs [[firstOrderPlus]]
    TFR <- TFR + lastTFR * PPPRs[[firstOrderPlus]] / (1 - PPPRs[[firstOrderPlus]])
    res$TFR <- TFR
    
    return (res)
  }

  #==== PPPRs ====#
  trans <- "P0_1"
  results[trans] <- PPPRs [[1]]
  TFR <- PPPRs [[1]]
  lastTFR <- TFR
  for (ind in (2:(firstOrderPlus))) {
    #ind=2
    trans <- paste("P", ind-1, "_",  ind, sep="")
    results[trans] <- PPPRs [[ind]]
    lastTFR <- lastTFR * PPPRs [[ind]]
    TFR <- TFR + lastTFR
  }
  trans <- paste("P", firstOrderPlus, "+_",  firstOrderPlus+1, "+", sep="")
  results[trans] <- PPPRs [[firstOrderPlus]]
  TFR <- TFR + lastTFR * PPPRs[[firstOrderPlus]] / (1 - PPPRs[[firstOrderPlus]])
  results$TFR <- TFR
  #====#
  
  if (minMaxValues) {
    results <- completeResults(results, PPPRs_min, "_min")
    results <- completeResults(results, PPPRs_max, "_max")
  }
  
  return (results)
}

#### compute TFR in the cohort / year dimensions ====
computeTFR_cohort <- function(df,
                              cmcBirthEgo_str   = "cmcBirthEgo",
                              cmcSurvey_str     = "cmcSurvey",
                              cmcBirthChild_str = "cmcBirthChild", # prefix of the variables with the birth dates of children (cmcBirthChild1, cmcBirthChild2, ...)
                              weight_str        = "weight",
                              maxOrder          = 6,       # orders above this are grouped into "maxOrder+"
                              numYear           = 15,
                              numLastYearsToRemove = 1,
                              useWeights        = TRUE,
                              doLoess           = TRUE,
                              loessSpan         = 0.75,
                              vectorize         = TRUE
                              ) {
  #
  # Build working base
  #
  base <- data.frame(
    cmcBirth  = df[[cmcBirthEgo_str]],
    cmcSurvey = df[[cmcSurvey_str]],
    weight    = if (useWeights) df[[weight_str]] else rep(1, nrow(df))
  )

  base$birthYear  <- decmc(base$cmcBirth)$year
  base$surveyYear <- decmc(base$cmcSurvey)$year
  base$surveyMonth <- decmc(base$cmcSurvey)$month
  
  lastYear  <- max(base$surveyYear)
  firstYear <- lastYear - numYear + 1
  yearSpan  <- firstYear:lastYear
  
  # Birth cohorts present: women whose childbearing years overlap [firstYear, lastYear]
  # We keep all women whose birth year is in the data (filtering by yearInd later)
  cohortSpan <- sort(unique(base$birthYear))
  
  nYear   <- length(yearSpan)
  nCohort <- length(cohortSpan)
  
  #
  # Determine which child variables exist in df (cmcBirthChild1, cmcBirthChild2, ...)
  #
  allChildVars <- colnames(df)[grepl(paste0("^",cmcBirthChild_str,"[0-9]+$"), colnames(df))]
  # Extract order numbers
  allOrders <- as.integer(sub(cmcBirthChild_str, "", allChildVars))
  allOrders <- sort(allOrders)
  
  # Orders to treat individually: 1 to (maxOrder - 1)
  # Orders maxOrder and above are grouped
  individualOrders <- allOrders[allOrders < maxOrder]
  groupedOrders    <- allOrders[allOrders >= maxOrder]
  orderLabels      <- c(as.character(individualOrders),
                        if (length(groupedOrders) > 0) paste0(maxOrder, "+") else NULL)
  
  #
  # Helper: build women count matrix (cohort x year)
  # Each woman contributes 1.0 to all fully observed years, and a fraction
  # (surveyMonth - 0.5) / 12 to her survey year
  #
  buildWomenCount <- function() {
    wCount <- matrix(0, nrow = nCohort, ncol = nYear)
    rownames(wCount) <- cohortSpan
    colnames(wCount) <- yearSpan
    
    for (w in seq_len(nrow(base))) {
      B  <- base$birthYear[w]
      S  <- base$surveyYear[w]
      frac <- (base$surveyMonth[w] - 0.5) / 12
      wt   <- base$weight[w]
      
      cohortInd <- match(B, cohortSpan)
      if (is.na(cohortInd)) next
      
      # Full years: firstYear up to S-1 (capped to yearSpan)
      fullYears <- intersect(firstYear:(S - 1), yearSpan)
      if (length(fullYears) > 0) {
        yInds <- fullYears - firstYear + 1
        wCount[cohortInd, yInds] <- wCount[cohortInd, yInds] + wt
      }
      
      # Partial year: survey year
      if (S >= firstYear && S <= lastYear) {
        yInd <- S - firstYear + 1
        wCount[cohortInd, yInd] <- wCount[cohortInd, yInd] + wt * frac
      }
    }
    return(wCount)
  }
  
  buildWomenCount_vec <- function() {
    wCount <- matrix(0, nrow = nCohort, ncol = nYear)
    rownames(wCount) <- cohortSpan
    colnames(wCount) <- yearSpan
    
    cohortInds <- match(base$birthYear, cohortSpan)
    valid      <- !is.na(cohortInds)
    
    for (w in which(valid)) {
      B    <- base$birthYear[w]
      S    <- base$surveyYear[w]
      frac <- (base$surveyMonth[w] - 0.5) / 12
      wt   <- base$weight[w]
      
      cohortInd <- cohortInds[w]
      
      fullYears <- intersect(firstYear:(S - 1), yearSpan)
      if (length(fullYears) > 0) {
        yInds <- fullYears - firstYear + 1
        wCount[cohortInd, yInds] <- wCount[cohortInd, yInds] + wt
      }
      
      if (S >= firstYear && S <= lastYear) {
        yInd <- S - firstYear + 1
        wCount[cohortInd, yInd] <- wCount[cohortInd, yInd] + wt * frac
      }
    }
    return(wCount)
  }
  
  #
  # Helper: build births count matrix (cohort x year) for a set of orders
  #
  buildBirthCount <- function(orders) {
    bCount <- matrix(0, nrow = nCohort, ncol = nYear)
    rownames(bCount) <- cohortSpan
    colnames(bCount) <- yearSpan
    
    for (ord in orders) {
      varName <- paste0(cmcBirthChild_str, ord)
      if (!varName %in% colnames(df)) next
      cmcChild <- df[[varName]]
      
      for (w in seq_len(nrow(base))) {
        if (is.na(cmcChild[w])) next
        # Skip births after interview (safety check)
        if (cmcChild[w] > base$cmcSurvey[w]) next
        
        B         <- base$birthYear[w]
        yearChild <- decmc(cmcChild[w])$year
        wt        <- base$weight[w]
        
        cohortInd <- match(B, cohortSpan)
        if (is.na(cohortInd)) next
        
        yInd <- yearChild - firstYear + 1
        if (yInd < 1 || yInd > nYear) next
        
        bCount[cohortInd, yInd] <- bCount[cohortInd, yInd] + wt
      }
    }
    return(bCount)
  }
  
  #
  # Helper: build births count matrix WITHOUT weights (for mean age calculation)
  #
  buildBirthCountNoW <- function(orders) {
    bCount <- matrix(0, nrow = nCohort, ncol = nYear)
    rownames(bCount) <- cohortSpan
    colnames(bCount) <- yearSpan
    
    for (ord in orders) {
      varName <- paste0(cmcBirthChild_str, ord)
      if (!varName %in% colnames(df)) next
      cmcChild <- df[[varName]]
      
      for (w in seq_len(nrow(base))) {
        if (is.na(cmcChild[w])) next
        if (cmcChild[w] > base$cmcSurvey[w]) next
        
        B         <- base$birthYear[w]
        yearChild <- decmc(cmcChild[w])$year
        
        cohortInd <- match(B, cohortSpan)
        if (is.na(cohortInd)) next
        
        yInd <- yearChild - firstYear + 1
        if (yInd < 1 || yInd > nYear) next
        
        bCount[cohortInd, yInd] <- bCount[cohortInd, yInd] + 1
      }
    }
    return(bCount)
  }
  
  buildBirthCount_vec <- function(orders, useWeights = TRUE) {
    bCount <- matrix(0, nrow = nCohort, ncol = nYear)
    rownames(bCount) <- cohortSpan
    colnames(bCount) <- yearSpan

    for (ord in orders) {
      varName <- paste0(cmcBirthChild_str, ord)
      if (!varName %in% colnames(df)) next
      
      cmcChild <- df[[varName]]
      
      # Filter valid rows: non-missing birth, not after interview
      valid <- !is.na(cmcChild) & (cmcChild <= base$cmcSurvey)
      if (!any(valid)) next
      
      base_v     <- base[valid, ]
      cmcChild_v <- cmcChild[valid]
      
      # Cohort and year indices
      cohortInds <- match(base_v$birthYear, cohortSpan)
      yearChild  <- decmc(cmcChild_v)$year
      yInds      <- yearChild - firstYear + 1
      
      # Keep only in-range indices
      validInd   <- !is.na(cohortInds) & yInds >= 1 & yInds <= nYear
      if (!any(validInd)) next
      
      cohortInds <- cohortInds[validInd]
      yInds      <- yInds[validInd]
      wt         <- if (useWeights) base_v$weight[validInd] else rep(1, sum(validInd))
      
      # Create a one-row-per-birth matrix with nYear columns, all zero except
      # column yInd which holds wt, then rowsum by cohortInd
      contrib        <- matrix(0, nrow = length(wt), ncol = nYear)
      contrib[cbind(seq_along(wt), yInds)] <- wt
      accumulated    <- rowsum(contrib, group = cohortInds)
      
      # Align accumulated rows back to cohortSpan
      #matchInds      <- match(rownames(accumulated), as.character(cohortSpan))
      matchInds      <- match(as.integer (rownames(accumulated)),(1:length(cohortSpan)))
      bCount[matchInds, ] <- bCount[matchInds, ] + accumulated
    }
    
    return(bCount)
  }
  
  #
  # Helper: compute TFR, mean age and optionally loess from rate matrices
  #
  computeMetrics <- function(birthCount, womenCount, birthCountNoW, orderLabel) {
    
    # rates: births / women, per cohort-year cell
    rates <- birthCount / womenCount
    rates[!is.finite(rates)] <- NA
    
    # TFR per year: sum rates over cohorts
    TFR <- colSums(rates, na.rm = TRUE)
    # Mean age at birth per year: weighted average of mother's age (yearChild - birthCohort)
    # age in a given year y for cohort B is simply y - B
    ageMat <- outer(cohortSpan, yearSpan, function(B, y) y - B)  # nCohort x nYear
    meanAge <- rep(NA, nYear)
    for (yi in seq_len(nYear)) {
      denom <- TFR[yi]
      if (!is.na(denom) && denom > 0) {
        meanAge[yi] <- sum(ageMat[, yi] * rates[, yi], na.rm = TRUE) / denom
      }
    }
    
    # Unweighted births per year
    nBirths <- colSums(birthCountNoW, na.rm = TRUE)
    
    result <- data.frame(
      year    = yearSpan,
      order   = orderLabel,
      tfr     = TFR,
      meanAge = meanAge,
      nBirths = nBirths
    )

    # Loess smoothing
    if (doLoess && sum(!is.na(TFR)) >= 4) {
      TFR [TFR==0] <- 0.000000001
      nBirths [nBirths==0] <- 1
      wt_loess <- ifelse(is.na(TFR), 0, 1)  # uniform weights over years
      # Use nBirths as loess weights so years with more observations
      # have more influence, consistent with the survey weights already
      # embedded in TFR via womenCount
      loessW <- ifelse(nBirths > 0, nBirths, NA)
      validInd <- which(!is.na(TFR) & !is.na(loessW) & loessW > 0)
      
      if (length(validInd) >= 4) {
        loessFit <- loess(TFR[validInd] ~ yearSpan[validInd],
                          weights = loessW[validInd],
                          span    = loessSpan,
                          degree  = 2)
        pred <- predict(loessFit,
                        newdata = data.frame(yearSpan = yearSpan),  # predict all years
                        se      = TRUE)
        result$tfr_smooth <- pred$fit
        result$tfr_min <- pred$fit - 1.96 * pred$se.fit
        result$tfr_max <- pred$fit + 1.96 * pred$se.fit
      } else {
        result$tfr_smooth    <- NA
        result$tfr_min <- NA
        result$tfr_max <- NA
      }
    } else {
      result$tfr_smooth    <- NA
      result$tfr_min <- NA
      result$tfr_max <- NA
    }
    
    return(result)
  }
  
  #
  # Main computation
  #
  if (isTRUE(vectorize)) {
    womenCount <- buildWomenCount_vec()
  } else {
    womenCount <- buildWomenCount()
  }
  allResults <- list()
  
  # Individual orders
  for (ord in individualOrders) {
    if (isTRUE(vectorize)) {
      bCount   <- buildBirthCount_vec(ord, useWeights = TRUE)
      bCountNW <- buildBirthCount_vec(ord, useWeights = FALSE)
    } else {
      bCount   <- buildBirthCount(ord)
      bCountNW <- buildBirthCountNoW(ord)
    }
    allResults[[as.character(ord)]] <- computeMetrics(bCount, womenCount, bCountNW,
                                                      orderLabel = as.character(ord))
  }
  
  # Grouped orders (maxOrder+)
  if (length(groupedOrders) > 0) {
    if (isTRUE(vectorize)) {
      bCount   <- buildBirthCount_vec(groupedOrders, useWeights = TRUE)
      bCountNW <- buildBirthCount_vec(groupedOrders, useWeights = FALSE)
    } else {
      bCount   <- buildBirthCount(groupedOrders)
      bCountNW <- buildBirthCountNoW(groupedOrders)
    }
    label    <- paste0(maxOrder, "+")
    allResults[[label]] <- computeMetrics(bCount, womenCount, bCountNW,
                                          orderLabel = label)
  }
  
  # All orders combined (TFR total)
  if (isTRUE(vectorize)) {
    bCount   <- buildBirthCount_vec(allOrders, useWeights = TRUE)
    bCountNW <- buildBirthCount_vec(allOrders, useWeights = FALSE)
  } else {
    bCount   <- buildBirthCount(allOrders)
    bCountNW <- buildBirthCountNoW(allOrders)
  }
  allResults[["total"]] <- computeMetrics(bCount, womenCount, bCountNW,
                                          orderLabel = "total")
  
  keyAllOrders <- setdiff(names(allResults), "total")
  if (doLoess) {
    allResults[["total"]]$tfr_smooth <- 0
    allResults[["total"]]$tfr_min <- 0
    allResults[["total"]]$tfr_max <- 0
    for (key in keyAllOrders) {
      res <- allResults[[key]]
      allResults[["total"]]$tfr_smooth <- allResults[["total"]]$tfr_smooth + res$tfr_smooth
      allResults[["total"]]$tfr_min    <- allResults[["total"]]$tfr_min + res$tfr_min
      allResults[["total"]]$tfr_max    <- allResults[["total"]]$tfr_max + res$tfr_max
    }
  }

  # relocate "total" as first element of the list
  allResults <- allResults[c("total", setdiff(names(allResults), "total"))]
  
  # remove last years
  keyAllOrders <- names(allResults)
  
  return(allResults)
}

####

#### Old FFS algorithm functions ####
source("lib/FFS_Lib.R")

truncateSurveyEndOfYear <- function (df) {
  #df <- colAll
  #we set the date of survey for each survey at end of the last complete year and truncate the information which occurs afterwards
  #the trickiest thing is to recompute the correct number of children
  vecDHSyear <- as.numeric(names(table(df$DHSyear)))
  for (aYear in vecDHSyear) {
    df$cmcSurvey <- ifelse (df$DHSyear==aYear, min(subset (df, DHSyear==aYear)$cmcSurvey), df$cmcSurvey)
  }
  dateSurvey <- decmc (df$cmcSurvey)
  df$cmcSurvey <- getCmc(rep(12, length(df$cmcSurvey)), dateSurvey$year-1)
  dateSurvey <- decmc (df$cmcSurvey)
  df$ySurvey <- dateSurvey$year
  
  maxB <- max (df$nBirthsTot)
  for (ord in (1:maxB)) {
    #ord=1
    cmcChildOrd <- paste("cmcBirthChild", ord, sep="")
    temp <- as.numeric(pull(df, cmcChildOrd))
    temp <- ifelse(is.na(temp),0,temp)
    df$nBirthsTot <- ifelse (temp > df$cmcSurvey, df$nBirthsTot - 1, df$nBirthsTot)
    df$nBirthsBH <- df$nBirthsTot
    temp <- ifelse (temp > df$cmcSurvey, 0, temp)
    temp <- ifelse (temp==0, NA, temp)
    df[cmcChildOrd] <- temp
  }
  
  df <- computeYearBirth(df)
  
  return (df)
}

# compute with the FFS algorithm
computeFFS <- function (df, numYear=10, firstOrderPlus=10) {
  #df<-colAll
  df$YBIRTH <- df$yBirthEgo
  df$NBIRTH <- df$nBirthsTot
  df <- truncateSurveyEndOfYear(df)
  
  maxYear <- max(df$ySurvey)
  results <- defResults()
  
  results <- computeCountry (df=df, country="Colombia", results=results, sex="Female", maxYear=maxYear, numYear=numYear, firstOrderPlus=10)
  
  return (results)
}
