read.ffs <- function () {
  
  ffs <- read.csv("vectorFFS.csv", header=TRUE, na.strings=c("#NULL!"))
  
  for (i in 1:9) {
    colnames(ffs)[which(colnames(ffs)==paste("V314Y.0",i,sep=""))] <- paste("YBIRTHCHILD",i,sep="")
  }
  for (i in 10:13) {
    colnames(ffs)[which(colnames(ffs)==paste("V314Y.",i,sep=""))] <- paste("YBIRTHCHILD",i,sep="")
  }
  
  return (ffs)
}

compute.ppr.m <- function(datatable=NULL, datatable_noW=NULL, maxYear=NULL, numYear=NULL, country=NULL, sex=NULL, birthOrder=NULL, useNA=FALSE) {
  #datatable <- c1
  #datatable_noW <- c1_noW
  #maxYear <- 94
  #numYear <- 10
  #country <- "Austria"
  #sex <- "Female"
  #birthOrder <- 1
  maxYear_ind <- which(colnames(datatable)==as.character(maxYear))
  c_x <- dim(datatable)[1]
  c_y <- dim(datatable)[2]
  
  #order it upside down in order to have age or duration increasing rowwise
  datatable <- datatable[order(row.names(datatable),decreasing=TRUE),]  
  #age (first birth) or duration (subsequent) for the last year
  ageORduration <- maxYear - as.integer(rownames(datatable)[1:(length(rownames(datatable))-1)])
  
  #total by row (total of births of a specific birthOrder by cohort or total of women by year of birth)
  sum_x <- rowSums(datatable[1:(c_x-1),])
  #total by column (total of births of a specific birthOrder by year)
  sum_y_noW <- colSums(datatable_noW[,1:(c_y)])
  
  #take care of don't know (97) and missing values (98) <in fact we ignore them right now, because they represent less than 1% of the cases. so don't need to bother>
  sum_x_na <- rep(0, c_x-1)
  if (useNA) {
    ind97 <- which(colnames(datatable)==97)
    if ((length(ind97) > 0) && (ind97>0)) {
      sum_x_na <- sum_x_na + datatable[1:(c_x-1), ind97]
    }
    ind98 <- which(colnames(datatable)==98)
    if ((length(ind98) > 0) && (ind98>0)) {
      sum_x_na <- sum_x_na + datatable[1:(c_x-1), ind98]
    }
  }
  
  # array for rates of first kind (1), number surviving (2), event numbers in the table (3), rates of second kind (4)
  c_ind <- array(0,c(c_x,numYear,4))
  #results: ppr from rates of first kind (2) mean age at childbearing from rates of first kind (3)
  #results: ppr or tfr from rates of second kind (4) mean age at childbearing from rates of second kind (5) number of events each year in column (6)
  #results: the first column is the year...
  ind_year <- 1
  ind_quantum_1st_kind <- 2
  ind_mac_1st_kind <- 3
  ind_quantum_2nd_kind <- 4
  ind_mac_2nd_kind <- 5
  ind_number_events <- 6
  c_res <- array(0,c(numYear,6))
  
  for (year in (1:numYear)) {
    c_res[year, ind_year] <- maxYear - numYear + year
    c_res[year, ind_number_events] <- sum_y_noW [c_y - numYear + year - 1]
  }
  
  #rates of first kind
  for (year in (1:numYear)) {
    ind <- maxYear_ind-numYear+year
    sum_x_left <- rowSums(datatable[1:(c_x-1),1:(ind-1)])
    c_ind[1:(c_x-1),year,1] <- ifelse ((sum_x - sum_x_left - sum_x_na) > 0, datatable[1:(c_x-1), ind] / (sum_x - sum_x_left - sum_x_na), 0)
  }
  #avoid cases with nearly all the mothers at parity x having a child of parity x+1
  #in those case the probability has value which can reach 1 and the PPR is also 1
  #we put here 0.25 instead of 1...
  c_ind <- ifelse(c_ind > 0.25, 0.25, c_ind)

  #rates of second kind
  for (year in (1:numYear)) {
    ind <- maxYear_ind-numYear+year
    c_ind[1:(c_x-1),year,4] <- ifelse ((sum_x - sum_x_na) > 0, datatable[1:(c_x-1), ind] / (sum_x - sum_x_na), 0)
  }
  #PPR or TFR based on rates of second kind
  c_res[,ind_quantum_2nd_kind] <- colSums(c_ind[1:(c_x-1),,4])
  #mean age based on rates of second kind
  #we have to substract 0.5 year to the mean age because we have rates by age reached during the year
  #but we would have to add 0.5 year to the mean age because we compute at mid-year, so at the end we leave the mean age as they are
  c_res[,ind_mac_2nd_kind] <- ifelse (c_res[,ind_quantum_2nd_kind] > 0,
                                      colSums(c_ind[1:(c_x-1),,4] * ageORduration[1:(c_x-1)]) / c_res[,ind_quantum_2nd_kind],
                                      0)
  #the ages are computed for year maxYear, so this has to be adjusted downward for each year back in time
  c_res[,ind_mac_2nd_kind] <- ifelse (c_res[,ind_mac_2nd_kind] > 0,
                                      c_res[,ind_mac_2nd_kind] - c((numYear-1):0),
                                      0)

  #number surviving
  #start from 1
  c_ind[1,,2] <- 1
  #recursive computation...
  for (x in (2:c_x)) {
    c_ind[x,,2] <- c_ind[x-1,,2] * (1-c_ind[x-1,,1])
  }
  #PPR based on rates of first kind
  c_res[,ind_quantum_1st_kind] <- 1 - c_ind[c_x,,2]
  
  #number of "deaths" in the table
  c_ind[1:(c_x-1),,3] <- c_ind[1:(c_x-1),,2] - c_ind[2:(c_x),,2]
  #mean age based on rates of first kind
  c_res[,ind_mac_1st_kind] <- ifelse (c_res[,ind_quantum_1st_kind] > 0,
                                      colSums(c_ind[1:(c_x-1),,3] * ageORduration[1:(c_x-1)]) / c_res[,ind_quantum_1st_kind],
                                      0)
  c_res[,ind_mac_1st_kind] <- ifelse (c_res[,ind_mac_1st_kind] > 0,
                                      c_res[,ind_mac_1st_kind] - c((numYear-1):0),
                                      0)

  return (
    data.frame(
      country=rep(country, numYear),
      sex=rep(sex, numYear),
      order=rep(birthOrder, numYear),
      year=c_res[,1],
      quantum_1stkind=c_res[, ind_quantum_1st_kind],
      mac_1stkind=c_res[, ind_mac_1st_kind],
      quantum_2ndkind=c_res[, ind_quantum_2nd_kind],
      mac_2ndkind=c_res[, ind_mac_2nd_kind],
      nEvents=c_res[, ind_number_events],
      stringsAsFactors=FALSE
    )
  )
  
}

zero <- function (x) {
  if (length(dim(x)==1)) {
    x[] <- 0
  } else if (length(dim(x)==2)) {
    x[,] <- 0    
  }
  
  return (x)
}

add <- function(t1, t2) {
  #t1<-c4plus
  #t2<-table(ffs$YBIRTHCHILD3, ffs$YBIRTHCHILD4, exclude=NULL)
  #take care of NA values for year
  d <- dim(t2)
  rownames(t1)[dim(t1)[1]] <- 1000
  rownames(t2)[dim(t2)[1]] <- 1000
  colnames(t1)[dim(t1)[2]] <- 1000
  colnames(t2)[dim(t2)[2]] <- 1000
  
  for (i in 1:(d[1])) {
    for (j in 1:(d[2])) {
      ti <- which(rownames(t1)==rownames(t2)[i])
      tj <- which(colnames(t1)==colnames(t2)[j])
      t1[ti,tj] <- t1[ti,tj] + t2[i,j]
    }
  }
  
  #return the NA value...
  rownames(t1)[dim(t1)[1]] <- NA
  colnames(t1)[dim(t1)[2]] <- NA
  
  return (t1)
}

# non-vectorized code, very slow!
sortBH <- function (dat=NULL) {
  posYB1 <- 10
  posNC <- 9
  nbCol <- 22
  
  #sort reproductive history (for some women and some country it is in reverse order)
  # 623 cases mostly in Canada95, Portugal and Germany
  d <- dim(dat)[1]
  #we need to do it on a case by case basic, for nearly 120 000 cases...
  for (ind in 1:d) {
    if (max(dat[ind,(posYB1:nbCol)],na.rm=TRUE) < 97) {
      #we do it only if there are no "don't know" or "missing" values which are coded as 97, 98 or 99...
      nChild <- dat[ind,posNC]
      if ((nChild > 1) && (length(na.omit(unlist(dat[ind,posYB1:(posYB1+nChild-1)])))>0)) {
        if (nChild < 14) {
          dat[ind,posYB1:(posYB1+nChild-1)] <- sort(dat[ind,posYB1:(posYB1+nChild-1)])
        } else {
          x <- 1
        }
      }
    }
  }
  
  return (dat)
}

# vectorized code, very fast!
sortBH_tidyverse <- function(dat) {
  library(tidyverse)

  # position in dat of the first year of birth, and end of the birth history
  posYB1 <- 10
  nbCol  <- 22
  
  # 1. We create a unique ID for each row so as not to lose track of women
  # and we keep the original column names for the final step
  dat_with_id <- dat %>% 
    mutate(row_id = row_number())
  
  col_names <- names(dat)[posYB1:nbCol]
  
  # 2. We transform into Long Format
  # This puts all the years of birth in a single column.
  dat_long <- dat_with_id %>%
    pivot_longer(
      cols = all_of(posYB1:nbCol), 
      names_to = "child_col", 
      values_to = "birth_year"
    )
  
  # 3. We identify which women meet the condition (max < 97)
  # We use group_by to analyse each woman independently.
  dat_processed <- dat_long %>%
    group_by(row_id) %>%
    mutate(
      # We calculate the maximum ignoring NAs for the condition
      max_y = max(birth_year, na.rm = TRUE),
      
      # We only order if the maximum is valid (< 97)
      # Otherwise, we leave birth_year as it was.
      birth_year = if_else(
        !is.na(max_y) && max_y < 97,
        # La magia: sort() organiza los números y pone los NA al final
        sort(birth_year, na.last = TRUE), 
        birth_year
      )
    ) %>%
    # We remove the auxiliary column and remove the grouping.
    select(-max_y) %>%
    ungroup()
  
  # 4. We return to WIDE Format
  # It is important to ensure that the columns return to their original order.
  dat_final <- dat_processed %>%
    pivot_wider(
      names_from = child_col, 
      values_from = birth_year
    ) %>%
    # We remove the temporary ID and ensure that the columns 
    # are in the same order as the original dataframe
    select(names(dat))
  
  return(dat_final)
}

average_tempo_correct <- function(ppr_ffs=NULL, numYear=10) {
  #from the previous results, compute average for each indicators over a definite time span and correct for tempo variation
  timeSpan <- 4
  meanTimeSpan <- 3
  # we compute an average value of the PPR over the time span and compute the tempo variation with an average of meanTimeSpan years at the beginning and the end
  # for example if for the country of interest we have a 4 years time span for computing fertility indexes and the years are 1990 to 1999
  # we will compute the PPRs and TFR values taking the average over years 1995-1998
  # the tempo variation will be measured computing first the various mean ages at childbearing averaging over the years 1990-94 and 1995-99
  # and the annual variation will be the fifth part of the difference between these two values
  # as there is still some variability, we smooth everything with the loess algorithm, using a strong smoothing factor (span=2)
  quantumValues <- c((numYear-timeSpan):(numYear-1))
  macBegValues <- (numYear-timeSpan-1):(numYear-timeSpan+meanTimeSpan-2)
  macEndValues <- (numYear-meanTimeSpan+1):(numYear)
  diffYearMacs <- mean(macEndValues) - mean(macBegValues)
  
  results <- data.frame(country=character(), sex=character(), order=numeric(),
                        quantum_1stkind=numeric(), quantum_1stkind_corrected=numeric(), quantum_2ndkind=numeric(), quantum_2ndkind_corrected=numeric())
  
  
  birthOrders <- 1:(dim(table(ppr_ffs$order)))
  
  for (SEX in c("Male", "Female")) {
    #SEX = "Female"
    ppr_ffs_sex <- subset(ppr_ffs, sex==SEX)
    
    for (COUNTRY in countries) {
      #COUNTRY = "Spain"
      ppr <- subset(ppr_ffs_sex, country==COUNTRY)
      
      if (dim(ppr)[1] == 0) {
        #no data
        next
      }
      
      for (ORDER in birthOrders) {
        #ORDER=1
        ppr_order <- subset(ppr, order==ORDER)
        span_loess <- 2 # maximum for span is 1??
        
        loess1 <- loess(quantum_1stkind~year, data=ppr_order, span=span_loess)
        quantum_1stkind_smoothed <- predict(loess1, data.frame(year=ppr_order$year))
        quantum_1stkind = mean ( quantum_1stkind_smoothed[quantumValues] )
        loess1 <- loess(mac_1stkind~year, data=ppr_order, span=span_loess)
        mac_1stkind_smoothed <- predict(loess1, data.frame(year=ppr_order$year))
        mac_1stkind_beg = mean ( mac_1stkind_smoothed[macBegValues] )     
        mac_1stkind_end = mean ( mac_1stkind_smoothed[macEndValues] )
        mac_1stkind_var = ( mac_1stkind_end - mac_1stkind_beg ) / diffYearMacs
        quantum_1stkind_corrected = 1 - ( 1 - quantum_1stkind ) ^ ( 1 / ( 1 - mac_1stkind_var ) )
        loess1 <- loess(quantum_2ndkind~year, data=ppr_order, span=span_loess)
        quantum_2ndkind_smoothed <- predict(loess1, data.frame(year=ppr_order$year))     
        quantum_2ndkind = mean ( quantum_2ndkind_smoothed[quantumValues] )
        loess1 <- loess(mac_2ndkind~year, data=ppr_order, span=span_loess)
        mac_2ndkind_smoothed <- predict(loess1, data.frame(year=ppr_order$year))
        mac_2ndkind_beg = mean ( mac_2ndkind_smoothed[macBegValues] )     
        mac_2ndkind_end = mean ( mac_2ndkind_smoothed[macEndValues] )
        mac_2ndkind_var = ( mac_2ndkind_end - mac_2ndkind_beg ) / diffYearMacs
        quantum_2ndkind_corrected = quantum_2ndkind / ( 1 - mac_2ndkind_var )
        
        results=rbind(results,
                      data.frame(country=COUNTRY, sex=SEX, order=ORDER,
                                 quantum_1stkind=quantum_1stkind, quantum_1stkind_corrected=quantum_1stkind_corrected,
                                 quantum_2ndkind=quantum_2ndkind, quantum_2ndkind_corrected=quantum_2ndkind_corrected))
      }
      
    }
  }
  
  return (results)
}


personYear <- function (datatable=NULL) {
  persYear <- PLPPSimple (datatable, id="id", period="age", direction="period")
}

addNARow <- function (datatable) {
  #datatable <- c1
  c_x <- dim(datatable)[1]
  c_y <- dim(datatable)[2]
  if ( !(is.na(rownames(datatable)[c_x])) ) {
    rowNA <- rep(0, c_y)
    datatable <- rbind(datatable, rowNA)
    rownames(datatable) <- c(rownames(datatable)[1:c_x],0)
  }
  return (datatable)
}

computeCountry <- function(df=NULL, country=NULL, results=NULL, sex=NULL, maxYear=NULL, firstOrderPlus=4, numYear=10, useWeights=TRUE, useNA=FALSE, FFSTypeFile=FALSE, highestOrder=NULL) {
 #df<-ffs
  #useWeights=FALSE
  if (is.null(df) | is.null(country) | is.null (results) | is.null (sex) | is.null (maxYear)) stop ("At least one the required parameters is not defined")
  if (firstOrderPlus < 3) stop ("firstOrderPlus minimum value 3")
  
  colnames (df) <- toupper (colnames(df))
  
  df_noW <- df
  df_noW$WEIGHT <- 1
  if (!useWeights) df$WEIGHT <- 1
  
  #determine the higher birth order (if there is no predeterminated value)
  if (is.null(highestOrder)) {
    for (highestOrder in (2:50)) {
      if (!(paste("YBIRTHCHILD", highestOrder+1, sep="") %in% colnames(df))) break
    }
  }
  
  #first child
  c1 <- xtabs(WEIGHT~YBIRTH+YBIRTHCHILD1, exclude=NULL, na.action=na.pass, data=df)
  c1 <- addNARow (c1)
  c1_noW <- xtabs(WEIGHT~YBIRTH+YBIRTHCHILD1, exclude=NULL, na.action=na.pass, data=df_noW)
  c1_noW <- addNARow (c1_noW)
  
  if (country == "Norway" & sex == "Female") {
    c1 <- NorwayFemale1()
  }
  if (country == "Sweden" & sex == "Female") {
    c1 <- SwedenFemale1()
  }
  
  fert <- compute.ppr.m (c1, c1_noW, maxYear, numYear, country, sex, 1)
  results$pprs <- rbind(results$pprs, fert)
  
  #second child
  c2 <- xtabs(WEIGHT~YBIRTHCHILD1+YBIRTHCHILD2, exclude=NULL, na.action=na.pass, data=df)
  c2 <- addNARow (c2)
  c2_noW <- xtabs(WEIGHT~YBIRTHCHILD1+YBIRTHCHILD2, exclude=NULL, na.action=na.pass, data=df_noW)
  c2_noW <- addNARow (c2_noW)
  
  fert <- compute.ppr.m (c2, c2_noW, maxYear, numYear, country, sex, 2)
  results$pprs <- rbind(results$pprs, fert)
  
  #third child and more
  
  #we do that to avoid empty columns / years: recycle table for second order
  for (ind in (3:(firstOrderPlus))) {
    c3more <- zero(c2)
    firstChild <- paste("YBIRTHCHILD", ind-1, sep="")
    secondChild <- paste("YBIRTHCHILD", ind, sep="")
    formula <- paste ("WEIGHT~", firstChild, "+", secondChild, sep="")
    c3more <- add(c3more, xtabs(formula, exclude=NULL, na.action=na.pass, data=df))
    c3more <- addNARow (c3more)
    c3more_noW <- zero(c2_noW)
    c3more_noW <- add(c3more_noW, xtabs(formula, exclude=NULL, na.action=na.pass, data=df_noW))
    c3more_noW <- addNARow (c3more_noW)
    
    fert <- compute.ppr.m (c3more, c3more_noW, maxYear, numYear, country, sex, ind)
    results$pprs <- rbind(results$pprs, fert)
  }
  
  #fourth and more
  #first way to compute it
  
  #we do that to avoid empty columns / years: recycle table for second order
  cHighestOrderPlus <- zero(c2)
  cHighestOrderPlus_noW <- zero(c2_noW)
  for (ind in (firstOrderPlus:(highestOrder-1))) {
    formula <- paste(paste ("WEIGHT~YBIRTHCHILD", ind, "+YBIRTHCHILD", ind+1, sep=""))
    cHighestOrderPlus <- add (cHighestOrderPlus, xtabs(formula, exclude=NULL, na.action=na.pass, data=df))
    cHighestOrderPlus_noW <- add (cHighestOrderPlus_noW, xtabs(formula, exclude=NULL, na.action=na.pass, data=df_noW))
  }
  cHighestOrderPlus <- addNARow (cHighestOrderPlus)
  cHighestOrderPlus_noW <- addNARow (cHighestOrderPlus_noW)
  
  fert <- compute.ppr.m (cHighestOrderPlus, cHighestOrderPlus_noW, maxYear, numYear, country, sex, firstOrderPlus+1)
  results$pprs <- rbind(results$pprs, fert)
  
  #fourth and more
  #second way of computing it (it was for checking purpose, for debugging the program. Now the results are identicals...)
  # if (FFSTypeFile) {
  #   posOrderPlus <- which(colnames(df)==paste("YBIRTHCHILD", firstOrderPlus, sep=""))
  #   posLast <- which(colnames(df)==paste("YBIRTHCHILD", highestOrder, sep=""))
  #   ffsOrderPlus <- subset(df, NCHILD>=firstOrderPlus)
  #   ffsOrderPlus_noW <- subset(df_noW, NCHILD>=firstOrderPlus)
  #   ffsOrderPlusPlus <- data.frame(YBIRTHCHILDORDERPLUS=numeric(), YBIRTHCHILDORDERPLUSPLUS=numeric(), WEIGHT=numeric())
  #   ffsOrderPlusPlus_noW <- data.frame(YBIRTHCHILDORDERPLUS=numeric(), YBIRTHCHILDORDERPLUSPLUS=numeric(), WEIGHT=numeric())
  #   i <- 0
  #   for (ind in 1:dim(ffsOrderPlus)[1]) {
  #     for (child in firstOrderPlus:ffsOrderPlus[ind,]$NCHILD) {
  #       i <- i + 1
  #       if (child < highestOrder) {
  #         ffsOrderPlusPlus[i,] <- list(ffsOrderPlus[ind, posOrderPlus+child-firstOrderPlus], ffsOrderPlus[ind, posOrderPlus+child-firstOrderPlus-1], ffsOrderPlus[ind, posLast])
  #         ffsOrderPlusPlus_noW[i,] <- list(ffsOrderPlus_noW[ind, posOrderPlus+child-firstOrderPlus], ffsOrderPlus_noW[ind, posOrderPlus+child-firstOrderPlus-1], ffsOrderPlus_noW[ind, posLast])
  #       } else {
  #         ffsOrderPlusPlus[i,] <- list(ffsOrderPlus[ind, posOrderPlus+child-firstOrderPlus], NA, ffsOrderPlus[ind, posLast])
  #         ffsOrderPlusPlus_noW[i,] <- list(ffsOrderPlus_noW[ind, posOrderPlus+child-firstOrderPlus], NA, ffsOrderPlus_noW[ind, posLast])
  #       }
  #     }
  #   }
  #   
  #   #we do that to avoid empty columns / years: recycle table for second order
  #   cHighestOrderPlus2 <- zero(c2)
  #   cHighestOrderPlus2 <- add (cHighestOrderPlus2, xtabs(WEIGHT~YBIRTHCHILDORDERPLUS+YBIRTHCHILDORDERPLUSPLUS, exclude=NULL, na.action=na.pass, data=ffs3plus))
  #   cHighestOrderPlus2 <- addNARow (cHighestOrderPlus2)
  #   cHighestOrderPlus2_noW <- zero(c2_noW)
  #   cHighestOrderPlus2_noW <- add (cHighestOrderPlus2_noW, xtabs(WEIGHT~YBIRTHCHILDORDERPLUS+YBIRTHCHILDORDERPLUSPLUS, exclude=NULL, na.action=na.pass, data=ffs3plus_noW))
  #   cHighestOrderPlus2_noW <- addNARow (cHighestOrderPlus2_noW)
  #   
  #   fert <- compute.ppr.m (cHighestOrderPlus2, cHighestOrderPlus2_noW, maxYear, numYear, country, sex, firstOrderPlus+2)
  #   results$pprs <- rbind(results$pprs, fert)
  # }
  
  #now the TFR computed "recycling" the previous function. In that case only the results for the rates of the 2nd kind make sense
  c1 <- xtabs(WEIGHT~YBIRTH+YBIRTHCHILD1, exclude=NULL, na.action=na.pass, data=df)   
  c1 <- addNARow (c1)
  c1_noW <- xtabs(WEIGHT~YBIRTH+YBIRTHCHILD1, exclude=NULL, na.action=na.pass, data=df_noW)   
  c1_noW <- addNARow (c1_noW)
  
  fert <- compute.ppr.m (c1, c1_noW, maxYear, numYear, country, sex, 1)
  results$tfrs <- rbind(results$tfrs, fert)
  fertTot <- fert
  fertTot$order <- 0
  for (birthOrder in 2:(highestOrder)) {
    cc <- zero(c1)
    cc_noW <- zero(c1_noW)
    YBIRTH_VAR <- paste("YBIRTHCHILD", birthOrder, sep="")
    formula <- paste("WEIGHT ~ YBIRTH + ", YBIRTH_VAR, sep="")
    cc <- add(cc, xtabs(formula, exclude=NULL, na.action=na.pass, data=df))
    cc <- addNARow (cc)
    cc_noW <- add(cc_noW, xtabs(formula, exclude=NULL, na.action=na.pass, data=df_noW))
    cc_noW <- addNARow (cc_noW)
    
    fert <- compute.ppr.m (cc, cc_noW, maxYear, numYear, country, sex, birthOrder)
    results$tfrs <- rbind(results$tfrs, fert)
    fertTot$quantum_2ndkind <- fertTot$quantum_2ndkind + fert$quantum_2ndkind
  }
  results$tfrs <- rbind(results$tfrs, fertTot)
  
  return (results)
}

defResults <- function() {
  #duration based PPRs (for first order age is used instead)
  ppr_ffs <- data.frame(country=character(), sex=character(), order=numeric(), year=numeric(),
                        quantum_1stkind=numeric(), mac_1stkind=numeric(), quantum_2ndkind=numeric(), mac_2ndkind=numeric(), nEvents=numeric())
  
  #tfr computed from PPRs (rates of 1st kind) or from standard age-specific fertility rates (rates of 2nd kind)
  tfr_ffs <- data.frame(country=character(), sex=character(), order=numeric(), year=numeric(),
                        quantum_1stkind=numeric(), mac_1stkind=numeric(), quantum_2ndkind=numeric(), mac_2ndkind=numeric(), nEvents=numeric())
  
  results <- list(pprs=ppr_ffs, tfrs=tfr_ffs)
  
  return (results)
}

cleanFFS <- function (ffs, maxYear) {
  ffs$YBIRTHCHILD1 <- ifelse(ffs$YBIRTHCHILD1 > maxYear,NA,ffs$YBIRTHCHILD1)
  ffs$YBIRTHCHILD2 <- ifelse(ffs$YBIRTHCHILD2 > maxYear,NA,ffs$YBIRTHCHILD2)
  ffs$YBIRTHCHILD3 <- ifelse(ffs$YBIRTHCHILD3 > maxYear,NA,ffs$YBIRTHCHILD3)
  ffs$YBIRTHCHILD4 <- ifelse(ffs$YBIRTHCHILD4 > maxYear,NA,ffs$YBIRTHCHILD4)
  ffs$YBIRTHCHILD5 <- ifelse(ffs$YBIRTHCHILD5 > maxYear,NA,ffs$YBIRTHCHILD5)
  ffs$YBIRTHCHILD6 <- ifelse(ffs$YBIRTHCHILD6 > maxYear,NA,ffs$YBIRTHCHILD6)
  ffs$YBIRTHCHILD7 <- ifelse(ffs$YBIRTHCHILD7 > maxYear,NA,ffs$YBIRTHCHILD7)
  ffs$YBIRTHCHILD8 <- ifelse(ffs$YBIRTHCHILD8 > maxYear,NA,ffs$YBIRTHCHILD8)
  ffs$YBIRTHCHILD9 <- ifelse(ffs$YBIRTHCHILD9 > maxYear,NA,ffs$YBIRTHCHILD9)
  ffs$YBIRTHCHILD10 <- ifelse(ffs$YBIRTHCHILD10 > maxYear,NA,ffs$YBIRTHCHILD10)
  ffs$YBIRTHCHILD11 <- ifelse(ffs$YBIRTHCHILD11 > maxYear,NA,ffs$YBIRTHCHILD11)
  ffs$YBIRTHCHILD12 <- ifelse(ffs$YBIRTHCHILD12 > maxYear,NA,ffs$YBIRTHCHILD12)
  ffs$YBIRTHCHILD13 <- ifelse(ffs$YBIRTHCHILD13 > maxYear,NA,ffs$YBIRTHCHILD13)
  return (ffs)
}

computeAll <- function(ffs_all=NULL, countries=NULL, numYear=10, useWeights=TRUE, useNA=FALSE) {
  
  results <- defResults()
  
  for (sex in c("Male", "Female")) {
    #sex = "Male"
    #sex = "Female"
    ffs_sex <- subset(ffs_all, SEX==sex)
    
    for (country in countries) {
      print (paste(country,sex))
      if ((country=="Poland")&&(sex=="Male")){next}
      #country = "Austria"
      ffs <- subset(ffs_sex, COUNTRY==country)
      
      if (dim(ffs)[1] == 0) {
        #no data
        next
      }
      
      #lastyear surveyed
      maxYear <- min (ffs$YEAR)-1
      
      ffs <- cleanFFS (ffs, maxYear)
      
      results <- computeCountry(df=ffs, country=country, results=results, sex=sex, maxYear=maxYear, numYear=numYear, useWeights=useWeights, FFSTypeFile=TRUE)
    }
  }
  
  return (results)
}

NorwayFemale1 <- function () {
  return (
    structure(list(
      `62` = c(0, 11.8, 18.8, 21, 9.6, 12, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      `63` = c(0, 9.8, 23.6, 28.2, 28, 12, 11.2, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `64` = c(0, 
               9.2, 19.6, 35.4, 37.6, 35, 14.2, 10.4, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `65` = c(0, 12, 18.4, 29.4, 47.2, 47, 36.4, 16.4, 9.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `66` = c(0, 5.8, 24, 27.6, 
               39.2, 59, 51.6, 37.8, 18.6, 8.8, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `67` = c(0, 8.6, 11.6, 36, 
               36.8, 49, 60.6, 56.2, 39.2, 20.8, 8, 1, 0.4, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `68` = c(0, 5.2, 17.2, 17.4, 
               48, 46, 52.6, 62.2, 60.8, 40.6, 23, 8.2, 2, 0.6, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `69` = c(0, 4.4, 10.4, 25.8, 
               23.2, 60, 46.6, 56.2, 63.8, 65.4, 42, 25.6, 8.4, 3, 0.8, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `70` = c(0, 3.4, 8.8, 
               15.6, 34.4, 29, 55.6, 47.2, 59.8, 65.4, 70, 44.2, 28.2, 8.6, 
               4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `71` = c(0, 
               2.8, 6.8, 13.2, 20.8, 43, 31.8, 51.2, 47.8, 63.4, 67, 65.8, 46.4, 
               30.8, 8.8, 5, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `72` = c(0, 
               1.2, 5.6, 10.2, 17.6, 26, 42, 34.6, 46.8, 48.4, 67, 65.4, 61.6, 
               48.6, 33.4, 9, 4.4, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
      `73` = c(0, 1.4, 2.4, 8.4, 13.6, 22, 28.6, 41, 37.4, 42.4, 
               49, 63.4, 63.8, 57.4, 50.8, 36, 9.6, 3.8, 0.4, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0) ,
      `74` = c(0, 1.6, 2.8, 3.6, 11.2, 17, 
               20.8, 31.2, 40, 40.2, 38, 49, 59.8, 62.2, 53.2, 53, 33.4, 
               10.2, 3.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `75` = c(0, 
               0.8, 3.2, 4.2, 4.8, 14, 16.8, 19.6, 33.8, 39, 43, 36.8, 49, 
               56.2, 60.6, 49, 50.8, 30.8, 10.8, 2.6, 0, 0.2, 0, 0, 0, 0, 
               0, 0, 0, 0),
      `76` = c(0, 1.2, 1.6, 4.8, 5.6, 6, 14.2, 16.6, 
               18.4, 36.4, 38, 43.4, 35.6, 49, 52.6, 59, 48.2, 48.6, 28.2, 
               11.4, 2, 0, 0.4, 0, 0, 0, 0, 0, 0, 0) ,
      `77` = c(0, 0.4, 2.4, 
               2.4, 6.4, 7, 7.2, 14.4, 16.4, 17.2, 39, 37, 43.8, 34.4, 49, 
               49, 56.8, 47.4, 46.4, 25.6, 12, 2.4, 0, 0.6, 0, 0, 0, 0, 
               0, 0) ,
      `78` = c(0, 0.4, 0.8, 3.6, 3.2, 8, 6, 8.4, 14.6, 16.2, 
               16, 40.4, 36, 44.2, 33.2, 49, 50.8, 54.6, 46.6, 44.2, 23, 
               11, 2.8, 0, 0.8, 0, 0, 0, 0, 0) ,
      `79` = c(0, 0.8, 0.8, 1.2, 
               4.8, 4, 7.8, 5, 9.6, 14.8, 16, 18, 41.8, 35, 44.6, 32, 48.8, 
               52.6, 52.4, 45.8, 42, 21, 10, 3.2, 0, 1, 0, 0, 0, 0) ,
      `80` = c(0, 
               0.2, 1.6, 1.2, 1.6, 6, 4.2, 7.6, 4, 10.8, 15, 17.2, 20, 43.2, 
               34, 45, 35.2, 48.6, 54.4, 50.2, 45, 38.4, 19, 9, 3.6, 0, 
               0.666666667, 0, 0, 0) ,
      `81` = c(0, 0, 0.4, 2.4, 1.6, 2, 5.2, 
               4.4, 7.4, 3, 12, 16.8, 18.4, 22, 44.6, 33, 43, 38.4, 48.4, 
               56.2, 48, 44, 34.8, 17, 8, 4, 0.333333333, 0.333333333, 0, 
               0) ,
      `82` = c(0, 0, 0, 0.6, 3.2, 2, 2.2, 4.4, 4.6, 7.2, 2, 
               13.6, 18.6, 19.6, 24, 46, 33.6, 41, 41.6, 48.2, 58, 48.2, 
               43, 31.2, 15, 7, 4, 0.666666667, 0, 0) ,
      `83` = c(0, 0, 0, 
               0, 0.8, 4, 2.2, 2.4, 3.6, 4.8, 7, 5, 15.2, 20.4, 20.8, 26, 
               47, 34.2, 39, 44.8, 48, 54.8, 48.4, 42, 27.6, 13, 7.666666667, 
               4, 1, 0) ,
      `84` = c(0, 0, 0, 0, 0, 1, 3.8, 2.4, 2.6, 2.8, 
               5, 7, 8, 16.8, 22.2, 22, 28.2, 48, 34.8, 37, 48, 46.8, 51.6, 
               48.6, 41, 24, 14.33333333, 8.333333333, 4, 0) ,
      `85` = c(0, 
               0, 0, 0, 0, 0, 0.8, 3.6, 2.6, 2.8, 2, 4, 7, 11, 18.4, 24, 
               17.8, 30.4, 49, 35.4, 35, 38.8, 45.6, 48.4, 48.8, 40, 24.33333333, 
               15.66666667, 9, 0) ,
      `86` = c(0, 0, 0, 0, 0, 0, 0, 0.6, 3.4, 
               2.8, 3, 1.6, 3, 7, 14, 20, 19.2, 13.6, 32.6, 50, 36, 28, 
               29.6, 44.4, 45.2, 49, 37, 24.66666667, 17, 0) ,
      `87` = c(0, 
               0, 0, 0, 0, 0, 0, 0, 0.4, 3.2, 3, 2.4, 1.2, 2, 7, 17, 16, 
               14.4, 9.4, 34.8, 51, 28.8, 21, 20.4, 43.2, 42, 33, 34, 25, 
               0) ,
      `88` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 3, 2.4, 1.8, 
               0.8, 1, 7, 13.6, 12, 9.6, 5.2, 37, 40.8, 21.6, 14, 11.2, 
               42, 28, 17, 31, 0) ,
      `89` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 2.4, 1.8, 1.2, 0.4, 0, 5.6, 10.2, 8, 4.8, 1, 29.6, 30.6, 
               14.4, 7, 2, 28, 14, 1, 0) ,
      `NA` = c(0, 7.2, 14.4, 21.6, 28.8, 
               36, 40.6, 45.2, 49.8, 54.4, 59, 69, 80.8, 91.4, 100.8, 109, 
               137.4, 170, 203.8, 231.4, 251, 296, 362.2, 419.2, 452.8, 
               472, 527, 594, 633, 0)),
      .Names = c("62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", 
                                                   "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", 
                                                   "88", "89", NA),
      row.names = c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", 
                                                                                  "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", 
                                                                                  "67", "68", "0"),
      class = "data.frame")
  )
}

SwedenFemale1 <- function () {
  return (
    structure(list(
      `64` = c(8.8964, 13.1512, 10.4436, 1.5472, 1.934, 
               0.28672, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0) ,
      `65` = c(7.9294, 17.7928, 19.7268, 13.9248, 
               1.934, 1.9056, 0.3584, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `66` = c(8.8964, 15.8588, 26.6892, 
               26.3024, 17.406, 1.7264, 1.8772, 0.5376, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `67` = c(10.637, 
               17.7928, 23.7882, 35.5856, 32.878, 15.5376, 1.5188, 1.8488, 0.7168, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ) ,
      `68` = c(10.637, 21.274, 26.6892, 31.7176, 44.482, 28.4528, 
               13.6692, 1.3112, 1.8204, 0.896, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `69` = c(11.2172, 21.274, 31.911, 
               35.5856, 39.647, 41.8576, 24.0276, 11.8008, 1.1036, 1.792, 0.7168, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `70` = c(8.8964, 
               22.4344, 31.911, 42.548, 44.482, 38.7064, 39.2332, 19.6024, 9.9324, 
               0.896, 1.602, 0.5376, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0) ,
      `71` = c(6.9624, 17.7928, 33.6516, 42.548, 53.185, 
               44.008, 37.7658, 36.6088, 15.1772, 8.064, 1.3904, 1.412, 0.3584, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `72` = c(6.1888, 
               13.9248, 26.6892, 44.8688, 53.185, 51.508, 43.534, 36.8252, 33.9844, 
               10.752, 8.9772, 1.8848, 1.222, 0.1792, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0) ,
      `73` = c(4.4482, 12.3776, 20.8872, 35.5856, 
               56.086, 52.7624, 49.831, 43.06, 35.8846, 31.36, 10.7908, 9.8904, 
               2.3792, 1.032, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ) ,
      `74` = c(2.7076, 8.8964, 18.5664, 27.8496, 44.482, 51.4992, 
               52.3398, 48.154, 42.586, 34.944, 28.6244, 10.8296, 10.8036, 2.8736, 
               0.842, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `75` = c(4.0614, 
               5.4152, 13.3446, 24.7552, 34.812, 42.0368, 46.9124, 51.9172, 
               46.477, 42.112, 33.0072, 25.8888, 10.8684, 11.7168, 3.368, 0.6736, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `76` = c(3.4812, 8.1228, 
               8.1228, 17.7928, 30.944, 34.1216, 39.5916, 42.3256, 51.4946, 
               44.8, 41.2676, 31.0704, 23.1532, 10.9072, 12.63, 3.0944, 0.5052, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `77` = c(1.3538, 6.9624, 
               12.1842, 10.8304, 22.241, 30.848, 33.4312, 37.1464, 37.7388, 
               51.072, 41.9024, 40.4232, 29.1336, 20.4176, 10.946, 11.904, 2.8208, 
               0.3368, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `78` = c(2.3208, 
               2.7076, 10.4436, 16.2456, 13.538, 22.452, 30.752, 32.7408, 34.7012, 
               33.152, 49.1092, 39.0048, 39.5788, 27.1968, 17.682, 10.1568, 
               11.178, 2.5472, 0.1684, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `79` = c(1.5472, 
               4.6416, 4.0614, 13.9248, 20.307, 15.848, 22.663, 30.656, 32.0504, 
               32.256, 32.0788, 47.1464, 36.1072, 38.7344, 25.26, 16.3456, 9.3676, 
               10.452, 2.2736, 0, 0.169, 0, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `80` = c(1.1604, 
               3.0944, 6.9624, 5.4152, 17.406, 22.1592, 18.158, 22.874, 30.56, 
               31.36, 32.5408, 31.0056, 45.1836, 33.2096, 37.89, 26.408, 15.0092, 
               8.5784, 9.726, 2, 0, 0.338, 0, 0, 0, 0, 0, 0, 0, 0) ,
      `81` = c(0.7736, 
               2.3208, 4.6416, 9.2832, 6.769, 16.9712, 24.0114, 20.468, 23.085, 
               30.464, 32.1608, 32.8256, 29.9324, 43.2208, 30.312, 35.912, 27.556, 
               13.6728, 7.7892, 9, 1.769, 0, 0.507, 0, 0, 0, 0, 0, 0, 0) ,
      `82` = c(0.1934, 
               1.5472, 3.4812, 6.1888, 11.604, 7.7448, 16.5364, 25.8636, 22.778, 
               23.296, 32.6228, 32.9616, 33.1104, 28.8592, 41.258, 33.2496, 
               33.934, 28.704, 12.3364, 7, 7.538, 1.538, 0, 0.676, 0, 0, 0, 
               0, 0, 0) ,
      `83` = c(0.1934, 0.3868, 2.3208, 4.6416, 7.736, 11.792, 
               8.7206, 16.1016, 27.7158, 25.088, 25.7096, 34.7816, 33.7624, 
               33.3952, 27.786, 41.8064, 36.1872, 31.956, 29.852, 11, 6.783, 
               6.076, 1.307, 0, 0.845, 0, 0, 0, 0, 0) ,
      `84` = c(0.3868, 0.3868, 
               0.5802, 3.0944, 5.802, 7.6224, 11.98, 9.6964, 15.6668, 29.568, 
               27.3116, 28.1232, 36.9404, 34.5632, 33.68, 31.4288, 42.3548, 
               39.1248, 29.978, 31, 9.983, 6.566, 4.614, 1.076, 0, 0.676, 0, 
               0, 0, 0) ,
      `85` = c(0, 0.7736, 0.5802, 0.7736, 3.868, 6.9712, 
               7.5088, 12.168, 10.6722, 15.232, 29.2116, 29.5352, 30.5368, 39.0992, 
               35.364, 35.744, 35.0716, 42.9032, 42.0624, 28, 30.377, 8.966, 
               6.349, 3.152, 0.845, 0, 0.507, 0, 0, 0) ,
      `86` = c(0.1934, 0, 
               1.1604, 0.7736, 0.967, 5.0656, 8.1404, 7.3952, 12.356, 11.648, 
               16.3956, 28.8552, 31.7588, 32.9504, 41.258, 39.4912, 37.808, 
               38.7144, 43.4516, 45, 27.977, 29.754, 7.949, 6.132, 1.69, 0.676, 
               0, 0.338, 0, 0) ,
      `87` = c(0.1934, 0.3868, 0, 1.5472, 0.967, 1.8488, 
               6.2632, 9.3096, 7.2816, 12.544, 12.1812, 17.5592, 28.4988, 33.9824, 
               35.364, 42.4064, 43.6184, 39.872, 42.3572, 44, 42.591, 27.954, 
               29.131, 6.932, 5.915, 1.352, 0.507, 0, 0.169, 0) ,
      `88` = c(0, 
               0.3868, 0.5802, 0, 1.934, 1.6696, 2.7306, 7.4608, 10.4788, 7.168, 
               13.0664, 12.7144, 18.7228, 28.1424, 36.206, 35.6912, 43.5548, 
               47.7456, 41.936, 46, 43.312, 40.182, 27.931, 28.508, 5.915, 4.732, 
               1.014, 0.338, 0, 0) ,
      `89` = c(0, 0, 0.5802, 0.7736, 0, 1.5472, 
               2.3722, 3.6124, 8.6584, 11.648, 5.9028, 13.5888, 13.2476, 19.8864, 
               27.786, 28.9648, 36.0184, 44.7032, 51.8728, 44, 36.969, 42.624, 
               37.773, 27.908, 27.885, 4.732, 3.549, 0.676, 0.169, 0) ,
      `90` = c(0, 
               0, 0, 0.7736, 0.967, 0, 1.1604, 3.0748, 4.4942, 9.856, 9.3184, 
               4.6376, 14.1112, 13.7808, 21.05, 22.2288, 21.7236, 36.3456, 45.8516, 
               56, 35.2, 27.938, 41.936, 35.364, 27.885, 22.308, 3.549, 2.366, 
               0.338, 0) ,
      `91` = c(0, 0, 0, 0, 0.967, 0.7736, 0, 0.7736, 3.7774, 
               5.376, 7.8848, 6.9888, 3.3724, 14.6336, 14.314, 16.84, 16.6716, 
               14.4824, 36.6728, 47, 44.8, 26.4, 18.907, 41.248, 32.955, 22.308, 
               16.731, 2.366, 1.183, 0) ,
      `92` = c(0, 0, 0, 0, 0, 0.7736, 0.5802, 
               0, 0.3868, 4.48, 4.3008, 5.9136, 4.6592, 2.1072, 15.156, 11.4512, 
               12.63, 11.1144, 7.2412, 37, 37.6, 33.6, 17.6, 9.876, 40.56, 26.364, 
               16.731, 11.154, 1.183, 0) ,
      `93` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 3.584, 3.2256, 3.9424, 2.3296, 0.842, 12.1248, 8.5884, 8.42, 
               5.5572, 0, 29.6, 28.2, 22.4, 8.8, 0.845, 32.448, 19.773, 11.154, 
               5.577, 0) ,
      `NA` = c(14.8918, 29.7836, 44.6754, 59.5672, 74.459, 
               72.72048, 71.7772, 70.3396, 68.3218, 66.304, 73.588, 83.56, 92.0984, 
               99.3824, 102.724, 133.2528, 172.0332, 204.4144, 232.4172, 252, 
               285.762, 341.724, 386.886, 415.048, 420.81, 337.324, 277.329, 
               198.068, 104.611, 0)),
      .Names = c("64", "65", "66", "67", "68", 
                 "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", 
                 "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", 
                 "91", "92", "93", NA),
      row.names = c("45", "46", "47", "48", 
                    "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", 
                    "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", 
                    "71", "72", "73", 0),
      class = "data.frame")
  )
}