library(tidyverse)
# Function to parse NSFG SPSS syntax file and read fixed-width data
# Works with NSFG .sps files and .dat files
# Process only the DATA LIST
parse_spss_syntax_DATALIST <- function(sps_file) {
  # Read the SPSS syntax file
  sps_lines <- readLines(sps_file, warn = FALSE)
  
  # Find the DATA LIST section
  data_list_start <- grep("DATA LIST FILE", sps_lines, ignore.case = TRUE)
  
  if (length(data_list_start) == 0) {
    stop("Could not find DATA LIST section in the SPSS syntax file")
  }
  
  # Find where the data list ends (look for period on its own line or VARIABLE LABELS)
  data_list_end <- data_list_start
  for (i in (data_list_start + 1):length(sps_lines)) {
    line <- trimws(sps_lines[i])
    # Stop at period ending the DATA LIST or at VARIABLE LABELS or EXECUTE
    if (grepl("^\\.$", line) || grepl("VARIABLE LABELS", line, ignore.case = TRUE) || 
        grepl("EXECUTE", line, ignore.case = TRUE)) {
      data_list_end <- i - 1
      break
    }
  }
  
  # Extract the variable definition lines
  var_lines <- sps_lines[(data_list_start + 1):data_list_end]
  
  # Remove comments (lines starting with *) and empty lines
  var_lines <- var_lines[!grepl("^\\s*\\*", var_lines)]
  var_lines <- var_lines[!grepl("^\\s*$", var_lines)]
  
  # Combine all lines into one string
  var_text <- paste(var_lines, collapse = " ")
  
  # Parse variable definitions (format: VARNAME start-end)
  # The pattern matches variable names followed by column positions
  # NSFG format: CASEID 1-5  RSCRNINF 6  RSCRAGE 7-8
  pattern <- "([A-Za-z_][A-Za-z0-9_]*)\\s+(\\d+)(?:-(\\d+))?"
  
  # Find all matches
  matches <- gregexpr(pattern, var_text, perl = TRUE)
  match_list <- regmatches(var_text, matches)[[1]]
  
  if (length(match_list) == 0) {
    stop("Could not parse variable definitions from SPSS syntax file")
  }
  
  # Initialize data frame to store variable information
  var_info <- data.frame(
    varname = character(),
    start = integer(),
    end = integer(),
    width = integer(),
    stringsAsFactors = FALSE
  )
  
  # Parse each variable definition
  for (match_text in match_list) {
    # Extract variable name and positions
    parts <- regmatches(match_text, regexec(pattern, match_text, perl = TRUE))[[1]]
    
    if (length(parts) >= 3) {
      varname <- parts[2]
      start_pos <- as.integer(parts[3])
      # If no end position specified (e.g., "RSCRNINF 6"), end = start
      end_pos <- if (length(parts) >= 4 && parts[4] != "") {
        as.integer(parts[4])
      } else {
        start_pos
      }
      
      var_info <- rbind(var_info, data.frame(
        varname = varname,
        start = start_pos,
        end = end_pos,
        width = end_pos - start_pos + 1,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by start position to ensure correct order
  var_info <- var_info[order(var_info$start), ]
  
  return(var_info)
}

# Function to read NSFG data using parsed syntax
read_nsfg_data <- function(data_file, sps_file, nrows = -1) {
  cat("Parsing SPSS syntax file...\n")
  var_info <- parse_spss_syntax_DATALIST(sps_file)
  
  cat(sprintf("Found %d variables\n", nrow(var_info)))
  cat("Reading data file...\n")
  
  # Read the fixed-width file
  df <- read.fwf(
    data_file,
    widths = var_info$width,
    col.names = var_info$varname,
    n = nrows,
    buffersize = 2000,
    stringsAsFactors = FALSE
  )
  
  cat("Data successfully loaded!\n")
  cat(sprintf("Dimensions: %d rows x %d columns\n", nrow(df), ncol(df)))
  
  return(df)
}


order_union_history <- function (df=NULL, CaseID="CaseID") {
  # Merge all the unions in order of union_start_cmc value
  # 1. Pivot the multiple groups of columns into a Long format
  # We use names_pattern to separate the variable "base" from the union order
  df_long <- df %>%
    dplyr::select(CaseID,
                  matches("^(union_start_type|union_start_cmc|union_start_cmc_I|marriage_start_cmc|marriage_start_cmc_I|union_end_cmc|union_end_cmc_I|union_end_motive)\\d+$")) %>%
    
    # 2. Pivot to long format
    pivot_longer(
      cols = -CaseID,
      names_to = c(".value", "original_pos"),
      names_pattern = "(.*?)(\\d+)$"
    ) %>%
    
    # 3. Clean and Sort
    filter(!is.na(union_start_cmc)) %>%
    arrange(CaseID, union_start_cmc) %>%
    
    # 4. Create new sequence and total count
    group_by(CaseID) %>%
    mutate(
      union_order = row_number(),
      total_unions = n()
    ) %>%
    ungroup()
  
  # 5. Transform back to Wide
  df_unions_ordered_wide <- df_long %>%
    dplyr::select(-original_pos) %>%  # Drop the old unsorted index
    pivot_wider(
      id_cols = c(CaseID, total_unions),
      names_from = union_order,
      values_from = c(union_start_type, union_start_cmc, union_start_cmc_I,
                      marriage_start_cmc, marriage_start_cmc_I, union_end_cmc, union_end_cmc_I, union_end_motive),
      names_sep = ""
    )
  
  return (df_unions_ordered_wide)
}

complete_union_history <- function (df=NULL, df_NSFG=NULL, nMarr=6) {
  
  if (is.null (df)) {
    nms <- names (df_NSFG)
    isCaseID <- "CaseID" %in% nms
    df <- data.frame(CaseID=if (isTRUE(isCaseID)) df_NSFG$CaseID else df_NSFG$CASEID)
    df$nUnion <- 0
  }
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  # Marriage
  strnum <- c("","2","3","4","5","6")
  nm <- names (df_NSFG)
  for (i in (1:nMarr)) {
    yMarr <- paste0("yMarr",i)
    WHMARHX_M <- paste0("WHMARHX_M",strnum[i])
    WHMARHX_Y <- paste0("WHMARHX_Y",strnum[i])
    yStopLiving <- paste0("yStopLiving",i)
    WNSTPHX_M <- paste0("WNSTPHX_M",strnum[i])
    WNSTPHX_Y <- paste0("WNSTPHX_Y",strnum[i])
    MarrEndMotive <- paste0("MarrEndMotive",i)
    MARENDHX <- paste0("MARENDHX",strnum[i])
    yCohabBef <- paste0("yCohabBef",i)
    STRTOGHX_M <- paste0("STRTOGHX_M",strnum[i])
    STRTOGHX_Y <- paste0("STRTOGHX_Y",strnum[i])
    yDeathHusband <- paste0("yDeathHusband",i)
    WNDIEHX_M <- paste0("WNDIEHX_M",strnum[i])
    WNDIEHX_Y <- paste0("WNDIEHX_Y",strnum[i])
    marriage_start_cmc <- paste0("marriage_start_cmc",i)
    union_start_cmc <- paste0("union_start_cmc",i)
    union_end_cmc <- paste0("union_end_cmc",i)
    union_end_motive <- paste0("union_end_motive",i)
    union_start_type <- paste0("union_start_type",i)
    
    df [[yMarr]] <- df_NSFG [[WHMARHX_Y]]
    #df [[yMarr]] <- ifelse((df [["nMarriage_decl"]] == i)&(is.na(df [[yMarr]])), df [["yCurrMarr"]], df [[yMarr]])
    df [[yStopLiving]] <- df_NSFG [[WNSTPHX_Y]]
    df [[yDeathHusband]] <- df_NSFG [[WNDIEHX_Y]]
    df [[MarrEndMotive]] <- df_NSFG [[MARENDHX]]
    df [[MarrEndMotive]] <- factor (df [[MarrEndMotive]], levels=c(1,2,3,9),
                                          labels=c("widowhood", "separation", "separation", "unknown"))
    df [[yCohabBef]] <- df_NSFG [[STRTOGHX_Y]]
    
    df [[union_start_type]] <- NA
    if (WHMARHX_M %in% nm) {
      df [[marriage_start_cmc]] <- compute_cmc (df_NSFG [[WHMARHX_M]], df [[yMarr]])
    } else {
      df [[marriage_start_cmc]] <- compute_cmc (imputed_month(nrow(df)), df [[yMarr]])
    }
    df [[union_start_cmc]] <- df [[marriage_start_cmc]]
    if (STRTOGHX_M %in% nm) {
      df [[union_start_cmc]] <- ifelse(!is.na(df [[yCohabBef]]),compute_cmc (df_NSFG [[STRTOGHX_M]], df [[yCohabBef]]),df [[union_start_cmc]])
    } else {
      df [[union_start_cmc]] <- ifelse(!is.na(df [[yCohabBef]]),compute_cmc (5, df [[yCohabBef]]),df [[union_start_cmc]])
    }
    
    # date of marriage separation (stop living together) and motive (separation or widowhood)
    if (WNSTPHX_M %in% nm) {
      df [[union_end_cmc]] <- compute_cmc (df_NSFG [[WNSTPHX_M]], df [[yStopLiving]])
    } else {
      df [[union_end_cmc]] <- compute_cmc (7, df [[yStopLiving]])
    }
    df [[union_end_motive]] <- NA
    idx <- (!is.na(df [[union_start_cmc]]))&(is.na(df [[union_end_cmc]]))
    df [[union_end_motive]] [idx] <- 0 # in union
    idx <- !is.na(df [[yStopLiving]])
    df [[union_end_motive]] [idx] <- 2 # separation
    idx <- !is.na(df [[yDeathHusband]])
    df [[union_end_motive]] [idx] <- 1 # widowhood
    df [[union_end_motive]] <- factor(df [[union_end_motive]],
                                            levels=c(0,1,2),
                                            labels = c("in union","widowhood","separation")
    )
    
    # death of husband
    if (WNDIEHX_M %in% nm) {
      df [[union_end_cmc]][idx] <- compute_cmc (df_NSFG [[WNDIEHX_M]], df [[yDeathHusband]])[idx]
    } else {
      df [[union_end_cmc]][idx] <- compute_cmc (7, df [[yDeathHusband]])[idx]
    }
    
    # union start type
    df [[union_start_type]] <- ifelse(!is.na(df [[yMarr]]), 1, NA)
    df [[union_start_type]] <- ifelse(!is.na(df [[yCohabBef]]), 3, df [[union_start_type]])
    df [[union_start_type]] <- factor(df [[union_start_type]], levels=c(1,2,3),
                                            labels=c("marriage", "cohabitation", "cohabitation before marriage"))
  }
  
  # current cohabitation
  yCohab <- paste0("yCohab", 0)
  WNSTRTCP_M <- paste0("WNSTRTCP_M",strnum[1])
  WNSTRTCP_Y <- paste0("WNSTRTCP_Y",strnum[1])
  yCohabEnd <- paste0("yCohabEnd", 0)
  marriage_start_cmc <- paste0("marriage_start_cmc",1+nMarr)
  union_start_cmc <- paste0("union_start_cmc",1+nMarr)
  union_end_cmc <- paste0("union_end_cmc",1+nMarr)
  union_end_motive <- paste0("union_end_motive",1+nMarr)
  union_start_type <- paste0("union_start_type",1+nMarr)

  df [[yCohab]] <- df_NSFG [[WNSTRTCP_Y]]
  df [[yCohabEnd]] <- NA
  df [[union_start_type]] <- NA
  df [[marriage_start_cmc]] <- NA
  if (WNSTRTCP_M %in% nm) {
    df [[union_start_cmc]] <- compute_cmc (df_NSFG [[WNSTRTCP_M]], df [[yCohab]])
  } else {
    df [[union_start_cmc]] <- compute_cmc (imputed_month(nrow(df)), df [[yCohab]])
  }
  df [[union_end_cmc]] <- NA
  df [[union_end_motive]] <- NA
  
  df[[union_end_motive]] <- case_when(
    !is.na(df[[yCohabEnd]])                                          ~ 2L,  # separation (but current cohabitation is still going on!)
    !is.na(df[[union_start_cmc]]) & is.na(df[[union_end_cmc]])      ~ 0L,  # in union (this is always the case)
    TRUE                                                              ~ NA_integer_
  )

  df [[union_end_motive]] <- factor (df [[union_end_motive]], levels=c(0,1,2,9),
                                           labels=c("in union", "widowhood", "separation", "unknown"))
                                          
  df [[union_start_type]] <- ifelse(!is.na(df [[yCohab]]),2,NA)
  df [[union_start_type]] <- factor(df [[union_start_type]], levels=c(1,2,3),
                                          labels=c("marriage", "cohabitation", "cohabitation before marriage"))
  
  for (i in (1:4)) {
    yCohab <- paste0("yCohab", i)
    STRTOTHX_M <- paste0("STRTOTHX_M",strnum[i])
    STRTOTHX_Y <- paste0("STRTOTHX_Y",strnum[i])
    yCohabEnd <- paste0("yCohabEnd", i)
    STPTOGCX_M <- paste0("STPTOGCX_M",strnum[i])
    STPTOGCX_Y <- paste0("STPTOGCX_Y",strnum[i])
    marriage_start_cmc <- paste0("marriage_start_cmc",i+nMarr+1)
    union_start_cmc <- paste0("union_start_cmc",i+nMarr+1)
    union_end_cmc <- paste0("union_end_cmc",i+nMarr+1)
    union_end_motive <- paste0("union_end_motive",i+nMarr+1)
    union_start_type <- paste0("union_start_type",i+nMarr+1)
    
    df [[yCohab]] <- df_NSFG [[STRTOTHX_Y]]
    df [[yCohabEnd]] <- df_NSFG [[STPTOGCX_Y]]
    df [[union_start_type]] <- NA
    df [[marriage_start_cmc]] <- NA
    if (STRTOTHX_M %in% nm) {
      df [[union_start_cmc]] <- compute_cmc (df_NSFG [[STRTOTHX_M]], df [[yCohab]])
    } else {
      df [[union_start_cmc]] <- compute_cmc (imputed_month(nrow(df)), df [[yCohab]])
    }
    if (STPTOGCX_M %in% nm) {
      df [[union_end_cmc]] <- compute_cmc (df_NSFG [[STPTOGCX_M]], df [[yCohabEnd]])
    } else {
      df [[union_end_cmc]] <- compute_cmc (7, df [[yCohabEnd]])
    }
    df [[union_end_motive]] <- NA
    idx <- (!is.na(df [[union_start_cmc]]))&(is.na(df [[union_end_cmc]]))
    df [[union_end_motive]] [idx] <- 0 # in union
    df [[union_end_motive]] <- ifelse(!is.na(df [[yCohabEnd]]),2,df [[union_end_motive]])
    df [[union_end_motive]] <- factor (df [[union_end_motive]], levels=c(0,1,2,9),
                                             labels=c("in union", "widowhood", "separation", "unknown"))
    
    df [[union_start_type]] <- ifelse(!is.na(df [[yCohab]]),2,NA)
    df [[union_start_type]] <- factor(df [[union_start_type]], levels=c(1,2,3),
                                            labels=c("marriage", "cohabitation", "cohabitation before marriage"))
  }
  
  UH_ordered <- order_union_history (zap_label(df))
  
  return (UH_ordered)
}

raw_union_history <- function (df=df_NSFG_2011_13, names2022_23=FALSE, datos=NULL) {
  compute_cmc_fields <- function (dfIn, dfOut, fieldName, cmc, month, year, end, endTot) {
    isCMC <- grepl("_cmc",fieldName)
    nms <- names (dfIn)
    fieldName_cmc <- paste0(fieldName, endTot)
    if (isTRUE(isCMC)) fieldName_cmc_I <- paste0(fieldName, "_I", endTot)
    cmc <- paste0(cmc, end)
    month <- paste0(month, end)
    year <- paste0(year, end)

    # dfOut <- dfOut %>%
    #   dplyr::mutate(!!sym(fieldName) := case_when(
    #     (cmc %in% nms) ~ dfIn[[cmc]],
    #     (month %in% nms) & (year %in% nms) ~ compute_cmc (dfIn[[month]], dfIn[[year]]),
    #     (year %in% nms) ~ compute_cmc (imputed_month(nrow(dfIn)), dfIn[[year]]),
    #     .default = NA_integer_
    #   ))
    
    dfOut[[fieldName_cmc]] <- NA
    if (isTRUE(isCMC)) dfOut[[fieldName_cmc_I]] <- NA
    if (isTRUE(isCMC)) {
      if ((!(cmc %in% nms)) & (!(month %in% nms)) & (year %in% nms)) {
        # only the year, month and cmc are missing
        dfOut[[fieldName_cmc]] <- ifelse(is.na(dfIn[[year]]), NA, compute_cmc (imputed_month(nrow(dfIn)), dfIn[[year]]))
        dfOut[[fieldName_cmc]] <- ifelse(dfIn[[year]] > 9000, 9999, dfOut[[fieldName_cmc]])
        dfOut[[fieldName_cmc_I]] <- ifelse(is.na(dfIn[[year]]), NA, 1)
        dfOut[[fieldName_cmc_I]] <- ifelse(dfIn[[year]] > 9000, 11, dfOut[[fieldName_cmc_I]])
      }
      if ((!(cmc %in% nms)) & (month %in% nms) & (year %in% nms)) {
        # only the month and the year, not the cmc
        vecMonth <- ifelse((!is.na(dfIn[[month]]))&(!(dfIn[[month]] %in% (1:12))), imputed_month(nrow(dfIn)), dfIn[[month]])
        dfOut[[fieldName_cmc_I]] <- ifelse(is.na(dfIn[[month]]), NA, 0)
        dfOut[[fieldName_cmc_I]] <- ifelse(is.na(dfIn[[month]]) | !(dfIn[[month]] %in% 1:12), 1, 0)
        dfOut[[fieldName_cmc]] <- compute_cmc (vecMonth, dfIn[[year]])
        dfOut[[fieldName_cmc_I]] <- ifelse(dfIn[[year]] > 9000, dfOut[[fieldName_cmc_I]] + 10, dfOut[[fieldName_cmc_I]])
      }
      if (cmc %in% nms) {
        # we have the cmc date
        dfOut[[fieldName_cmc]] <- ifelse((!is.na(dfIn[[cmc]]))&(dfIn[[cmc]] > 200)&(dfIn[[cmc]] < 2000), dfIn[[cmc]], 9999)
        dfOut[[fieldName_cmc]] <- ifelse((is.na(dfIn[[cmc]])), NA, dfOut[[fieldName_cmc]])
        dfOut[[fieldName_cmc_I]] <- ifelse((!is.na(dfIn[[cmc]]))&(dfIn[[cmc]] > 200)&(dfIn[[cmc]] < 2000), 0, 10)
        dfOut[[fieldName_cmc_I]] <- ifelse((is.na(dfIn[[cmc]])), NA, dfOut[[fieldName_cmc_I]])
      }
    } else {
      dfOut[[fieldName_cmc]] <- dfIn[[cmc]]
    }
    
    return (dfOut)
  }
  
  compute_marriage_otherFields <- function (df, end) {
    marriage_start_cmc <- paste0("marriage_start_cmc", end)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", end)
    marriage_end_cmc <- paste0("marriage_end_cmc", end)
    marriage_end_cmc_I <- paste0("marriage_end_cmc_I", end)
    marriage_end_motive <- paste0("marriage_end_motive", end)
    marriage_cohabBef_cmc <- paste0("marriage_cohabBef_cmc", end)
    marriage_cohabBef_cmc_I <- paste0("marriage_cohabBef_cmc_I", end)
    marriage_deathHusband_cmc <- paste0("marriage_deathHusband_cmc", end)
    marriage_deathHusband_cmc_I <- paste0("marriage_deathHusband_cmc_I", end)
    marriage_stopLiving_cmc <- paste0("marriage_stopLiving_cmc", end)
    marriage_stopLiving_cmc_I <- paste0("marriage_stopLiving_cmc_I", end)
    union_start_type <- paste0("union_start_type", end)
    union_start_cmc <- paste0("union_start_cmc", end)
    union_start_cmc_I <- paste0("union_start_cmc_I", end)
    union_end_cmc <- paste0("union_end_cmc", end)
    union_end_cmc_I <- paste0("union_end_cmc_I", end)
    union_end_motive <- paste0("union_end_motive", end)

    if (isTRUE(names2022_23)) {
      df[[marriage_deathHusband_cmc]] <- ifelse (df[[marriage_end_motive]] %in% c(1),df[[marriage_end_cmc]],NA)
    }
    df[[union_start_type]] <- ifelse (is.na(df[[marriage_start_cmc]]),NA,1) # 1: marriage
    df[[union_start_type]] <- ifelse (is.na(df[[marriage_cohabBef_cmc]]),df[[union_start_type]],3) # 3: cohabitation before marriage
    df[[union_start_type]] <- factor(df[[union_start_type]], levels=c(1,2,3),
                                            labels=c("marriage", "cohabitation", "cohabitation before marriage"))
    df[[union_start_cmc]] <- ifelse (is.na(df[[marriage_cohabBef_cmc]]),df[[marriage_start_cmc]],df[[marriage_cohabBef_cmc]])
    df[[union_start_cmc_I]] <- ifelse (is.na(df[[marriage_cohabBef_cmc]]),df[[marriage_start_cmc_I]],df[[marriage_cohabBef_cmc_I]])
    df[[union_end_cmc]] <- ifelse (is.na(df[[marriage_stopLiving_cmc]]),df[[marriage_deathHusband_cmc]],df[[marriage_stopLiving_cmc]])
    df[[union_end_cmc_I]] <- ifelse (is.na(df[[marriage_stopLiving_cmc]]),df[[marriage_deathHusband_cmc_I]],df[[marriage_stopLiving_cmc_I]])
    df[[union_end_motive]] <- ifelse (is.na(df[[union_start_cmc]]),NA,0) # NA: no union, 0: union
    df[[union_end_motive]] <- ifelse (is.na(df[[marriage_stopLiving_cmc]]),df[[union_end_motive]],2) # 2: separation
    df[[union_end_motive]] <- ifelse (is.na(df[[marriage_deathHusband_cmc]]),df[[union_end_motive]],1) # 1: widowhood
    df[[union_end_motive]] <- factor (df[[union_end_motive]], levels=c(0,1,2,9),
                                             labels=c("in union", "widowhood", "separation", "unknown"))
    
    df$nUnion <- df$nUnion + ifelse (is.na(df[[marriage_start_cmc]]),0,1)

    return (df)
  }

  compute_cohabitation_otherFields <- function (df, end) {
    marriage_start_cmc <- paste0("marriage_start_cmc", end)
    marriage_start_cmc_I <- paste0("marriage_start_cmc_I", end)
    union_start_type <- paste0("union_start_type", end)
    union_start_cmc <- paste0("union_start_cmc", end)
    union_start_cmc_I <- paste0("union_start_cmc_I", end)
    union_end_cmc <- paste0("union_end_cmc", end)
    union_end_cmc_I <- paste0("union_end_cmc_I", end)
    union_end_motive <- paste0("union_end_motive", end)
    
    df[[union_start_type]] <- ifelse (is.na(df[[union_start_cmc]]),NA,2) # 2: cohabitation
    df[[union_start_type]] <- factor(df[[union_start_type]], levels=c(1,2,3),
                                     labels=c("marriage", "cohabitation", "cohabitation before marriage"))
    df[[union_end_motive]] <- ifelse (is.na(df[[union_start_cmc]]),NA,0) # NA: no union, 0: union
    df[[union_end_motive]] <- ifelse (is.na(df[[union_end_cmc]]),df[[union_end_motive]],2) # 2: separation
    df[[union_end_motive]] <- factor (df[[union_end_motive]], levels=c(0,1,2,9),
                                      labels=c("in union", "widowhood", "separation", "unknown"))
    
    df$nUnion <- df$nUnion + ifelse (is.na(df[[union_start_cmc]]),0,1)

    return (df)
  }
  
  nms <- names (df)
  if (is.null (datos)) {
    isCaseID <- "CaseID" %in% nms
    datos <- data.frame(CaseID=if (isTRUE(isCaseID)) df$CaseID else df$CASEID)
    datos$nUnion <- 0
  }
  
  if (isTRUE(names2022_23)) {
    nMar <- if ("WHMARHX_Y_7" %in% nms) 7 else if ("WHMARHX_Y_6" %in% nms) 6 else if ("WHMARHX_Y_5" %in% nms) 5 else 4
    vIn <- c("_1", "_2", "_3", "_4", "_5", "_6", "_7")
  } else {
    nMar <- if ("WHMARHX_Y7" %in% nms) 7 else if ("WHMARHX_Y6" %in% nms) 6 else if ("WHMARHX_Y5" %in% nms) 5 else 4
    vIn <- c("", "2", "3", "4", "5", "6", "7")
  }
  vOut <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  if ("TIMESMAR" %in% nms) datos$nMarriages <- df$TIMESMAR
  for (u in (1:nMar)) {
    datos <- compute_cmc_fields (df, datos, "marriage_start_cmc", "CMMARRHX", "WHMARHX_M", "WHMARHX_Y", vIn[u], vOut[u])
    datos <- compute_cmc_fields (df, datos, "marriage_end_cmc", "", "", "ENDMARRX_Y", vIn[u], vOut[u])
    datos <- compute_cmc_fields (df, datos, "marriage_end_motive", "MARENDHX", "", "", vIn[u], vOut[u])
    datos <- compute_cmc_fields (df, datos, "marriage_cohabBef_cmc", "CMPMCOHX", "STRTOGHX_M", "STRTOGHX_Y", vIn[u], vOut[u])
    datos <- compute_cmc_fields (df, datos, "marriage_deathHusband_cmc", "CMHSBDIEX", "WNDIEHX_M", "WNDIEHX_Y", vIn[u], vOut[u])
    datos <- compute_cmc_fields (df, datos, "marriage_stopLiving_cmc", "CMSTPHSBX", "WNSTPHX_M", "WNSTPHX_Y", vIn[u], vOut[u])
    datos <- compute_marriage_otherFields (datos, vOut[u])
  }
  # current cohabitation
  u <- nMar + 1
  datos <- compute_cmc_fields (df, datos, "marriage_start_cmc", "XXX", "XXX", "XXX", "", vOut[u])
  datos <- compute_cmc_fields (df, datos, "union_start_cmc", "CMSTRTCP", "WNSTRTCP_M", "WNSTRTCP_Y", "", vOut[u])
  datos <- compute_cmc_fields (df, datos, "union_end_cmc", "XXX", "XXX", "XXX", "", vOut[u])
  datos <- compute_cohabitation_otherFields (datos, vOut[u])

  if (isTRUE(names2022_23)) {
    nPrevCohab <- if ("STRTOTH4_Y" %in% nms) 4 else if ("STRTOTH3_Y" %in% nms) 3 else if ("STRTOTH2_Y" %in% nms) 2 else if ("STRTOTH1_Y" %in% nms) 1 else 0
  } else {
    nPrevCohab <- if ("STRTOTHX_Y5" %in% nms) 5 else
      if ("STRTOTHX_Y4" %in% nms) 4 else
        if ("STRTOTHX_Y3" %in% nms) 3 else
          if ("STRTOTHX_Y2" %in% nms) 2 else
            if ("STRTOTHX_Y" %in% nms) 1 else 0
    if (nPrevCohab == 0) {
      nPrevCohab <- if ("CMCOHSTX8" %in% nms) 8 else
        if ("CMCOHSTX7" %in% nms) 7 else
          if ("CMCOHSTX6" %in% nms) 6 else
            if ("CMCOHSTX5" %in% nms) 5 else
              if ("CMCOHSTX4" %in% nms) 4 else
                if ("CMCOHSTX3" %in% nms) 3 else
                  if ("CMCOHSTX2" %in% nms) 2 else
                    if ("CMCOHSTX" %in% nms) 1 else 0
    }
  }
  for (c in (1:nPrevCohab)) {
    if (isTRUE(names2022_23)) {
      datos <- compute_cmc_fields (df, datos, "marriage_start_cmc", "XXX", "XXX", "XXX", "", vOut[u+c])
      CMCOHST <- paste0("CMCOHST", c)
      STRTOTH_M <- paste0("STRTOTH", c, "_M")
      STRTOTH_Y <- paste0("STRTOTH", c, "_Y")
      datos <- compute_cmc_fields (df, datos, "union_start_cmc", CMCOHST, STRTOTH_M, STRTOTH_Y, "", vOut[u+c])
      CMSTPCOH <- paste0("CMSTPCOH", c)
      STPTOGC_M <- paste0("STPTOGC", c, "_M")
      STPTOGC_Y <- paste0("STPTOGC", c, "_Y")
      datos <- compute_cmc_fields (df, datos, "union_end_cmc", CMSTPCOH, STPTOGC_M, STPTOGC_Y, "", vOut[u+c])
      datos <- compute_cohabitation_otherFields (datos, vOut[u+c])
    } else {
      datos <- compute_cmc_fields (df, datos, "marriage_start_cmc", "XXX", "XXX", "XXX", vIn[c], vOut[u+c])
      datos <- compute_cmc_fields (df, datos, "union_start_cmc", "CMCOHSTX", "STRTOTHX_M", "STRTOTHX_Y", vIn[c], vOut[u+c])
      datos <- compute_cmc_fields (df, datos, "union_end_cmc", "CMSTPCOHX", "STPTOGCX_M", "STPTOGCX_Y", vIn[c], vOut[u+c])
      datos <- compute_cohabitation_otherFields (datos, vOut[u+c])
    }
  }
  
  UH_ordered <- order_union_history (zap_label(datos))

  return (UH_ordered)
}

live_birth_history <- function (df_NSFG_preg=NULL) {
  # for pregnancies file starting with year 2002
  # the variable OUTCOME should exist in the file
  if (!("OUTCOME" %in% names(df_NSFG_preg))) {
    stop("The variable OUTCOME is missing in the pregnancy file. Please check the file and try again.")
  }
  if (exists("DEBUG1") && isTRUE(DEBUG1)) browser()
  ###### sex of multiple pregnancies ######
  # the sex of the babies born in multiple pregnancy is hidden for anonymity reasons!
  # we give a random sex to all
  set.seed(42)
  
  nm <- names(df_NSFG_preg)
  hasBABYSEX <- ("BABYSEX" %in% nm)
  hasBABYSEX1 <- ("BABYSEX1" %in% nm)
  hasBABYSEX3 <- ("BABYSEX3" %in% nm)
  hasBABYSEX4 <- ("BABYSEX4" %in% nm)
  hasBORNALIV <- ("BORNALIV" %in% nm)
  if (isTRUE(hasBABYSEX)&isTRUE(hasBORNALIV)&!isTRUE(hasBABYSEX1)) {
    # If BABYSEX is NA (common in multiples), randomize it.
    missing_b1 <- which(is.na(df_NSFG_preg$BABYSEX) & df_NSFG_preg$OUTCOME == 1)
    # 1. BABYSEX1: Handle all live births
    df_NSFG_preg$BABYSEX1 <- df_NSFG_preg$BABYSEX
    df_NSFG_preg$BABYSEX1[missing_b1] <- sample(c(1, 2), length(missing_b1), replace = TRUE)
    
    # 2. BABYSEX2: Handle EXACTLY Twins and Triplets separately
    # Extract indices for exactly 2 and exactly 3 or 4 babies
    idx_twins    <- which(df_NSFG_preg$BORNALIV == 2)
    idx_triplets <- which(df_NSFG_preg$BORNALIV == 3)
    idx_quadruplets <- which(df_NSFG_preg$BORNALIV == 4)
    
    # Assign random sex for the second baby in twins, triplets and quadruplets
    df_NSFG_preg$BABYSEX2 <- NA # Initialize column
    df_NSFG_preg$BABYSEX2[idx_twins]    <- sample(c(1, 2), length(idx_twins), replace = TRUE)
    df_NSFG_preg$BABYSEX2[idx_triplets] <- sample(c(1, 2), length(idx_triplets), replace = TRUE)
    df_NSFG_preg$BABYSEX2[idx_quadruplets] <- sample(c(1, 2), length(idx_quadruplets), replace = TRUE)
    
    # 3. BABYSEX3: Handle  Triplets
    df_NSFG_preg$BABYSEX3 <- NA # Initialize column
    # Assign random sex for the third and fourth baby
    df_NSFG_preg$BABYSEX3[idx_triplets] <- sample(c(1, 2), length(idx_triplets), replace = TRUE)
    df_NSFG_preg$BABYSEX3[idx_quadruplets] <- sample(c(1, 2), length(idx_quadruplets), replace = TRUE)
    
    # 4. BABYSEX4: Handle Quadruplets
    df_NSFG_preg$BABYSEX4 <- NA # Initialize column
    # Assign random sex for the fourth baby only
    df_NSFG_preg$BABYSEX4[idx_quadruplets] <- sample(c(1, 2), length(idx_quadruplets), replace = TRUE)
    
  } else if (!isTRUE(hasBABYSEX1)) {
    if (isTRUE(hasBORNALIV)) {
      # If BABYSEX1 is missing but BORNALIV exists, create BABYSEX1,2,3,4 with random values for live births
      idx1 <- (df_NSFG_preg$BORNALIV >= 1)
      df_NSFG_preg$BABYSEX1 <- NA
      df_NSFG_preg$BABYSEX1[idx1] <- sample(c(1, 2), sum(idx1), replace = TRUE)
      df_NSFG_preg$BABYSEX2 <- NA
      idx2 <- (df_NSFG_preg$BORNALIV >= 2)
      df_NSFG_preg$BABYSEX2[idx2] <- sample(c(1, 2), sum(idx2), replace = TRUE)
      idx3 <- (df_NSFG_preg$BORNALIV == 3)
      df_NSFG_preg$BABYSEX3 <- NA
      df_NSFG_preg$BABYSEX3[idx3] <- sample(c(1, 2), sum(idx3), replace = TRUE)
      idx4 <- (df_NSFG_preg$BORNALIV == 4)
      df_NSFG_preg$BABYSEX4 <- NA
      df_NSFG_preg$BABYSEX4[idx4] <- sample(c(1, 2), sum(idx4), replace = TRUE)
    } else {
      # If neither BABYSEX1 nor BORNALIV exists, create BABYSEX1,2,3,4 with random values for all pregnancies with OUTCOME == 1
      idx_outcome <- (df_NSFG_preg$OUTCOME == 1)
      df_NSFG_preg$BABYSEX1 <- NA
      df_NSFG_preg$BABYSEX1 <- ifelse(df_NSFG_preg$OUTCOME == 1, sample(c(1, 2), sum(idx_outcome), replace = TRUE), NA)
      df_NSFG_preg$BABYSEX2 <- NA
      df_NSFG_preg$BABYSEX2 <- ifelse(df_NSFG_preg$OUTCOME == 1, sample(c(1, 2, NA), 
                                                                        size = sum(idx_outcome), 
                                                                        replace = TRUE, 
                                                                        prob = c(0.005, 0.005, 0.99)),
                                      NA)
      df_NSFG_preg$BABYSEX3 <- NA
      df_NSFG_preg$BABYSEX4 <- NA
    }
  }
  
  if (isFALSE(hasBABYSEX3)) df_NSFG_preg$BABYSEX3 <- NA
  if (isFALSE(hasBABYSEX4)) df_NSFG_preg$BABYSEX4 <- NA

  ###### dataframe of live births ######
  # complete the list of fields
  nm <- names(df_NSFG_preg)
  if (!("DATEND_I" %in% nm)) df_NSFG_preg$DATEND_I <- NA
  if ("CMKIDIED" %in% nm) df_NSFG_preg = rename(df_NSFG_preg, CMKIDIED1 = CMKIDIED)
  hasCMKIDIED1 <- ("CMKIDIED1" %in% nm)
  if (!hasCMKIDIED1) {
    df_NSFG_preg$CMKIDIED1 <- NA
    df_NSFG_preg$CMKIDIED2 <- NA
    df_NSFG_preg$CMKIDIED3 <- NA
    df_NSFG_preg$CMKIDIED4 <- NA
    hasWHENDIED_M1 <- ("WHENDIED_M1" %in% nm)
    if (isTRUE(hasWHENDIED_M1)) {
      df_NSFG_preg$CMKIDIED1 <- compute_cmc (df_NSFG_preg$WHENDIED_M1, df_NSFG_preg$WHENDIED_Y1)
      df_NSFG_preg$CMKIDIED2 <- compute_cmc (df_NSFG_preg$WHENDIED_M2, df_NSFG_preg$WHENDIED_Y2)
      df_NSFG_preg$CMKIDIED3 <- compute_cmc (df_NSFG_preg$WHENDIED_M3, df_NSFG_preg$WHENDIED_Y3)
    } else {
      hasWHENDIED_Y1 <- ("WHENDIED_Y1" %in% nm)
      if (isTRUE(hasWHENDIED_Y1)) {
        df_NSFG_preg$CMKIDIED1 <- compute_cmc (imputed_month(nrow(df_NSFG_preg)), df_NSFG_preg$WHENDIED_Y1)
        df_NSFG_preg$CMKIDIED2 <- compute_cmc (imputed_month(nrow(df_NSFG_preg)), df_NSFG_preg$WHENDIED_Y2)
        df_NSFG_preg$CMKIDIED3 <- compute_cmc (imputed_month(nrow(df_NSFG_preg)), df_NSFG_preg$WHENDIED_Y3)
      }
    }
  }
  if (!("CMKIDIED4" %in% nm)) df_NSFG_preg$CMKIDIED4 <- NA
  
  # 1. Selection and Filtering
  df_children_final <- df_NSFG_preg %>%
    filter(OUTCOME == 1) %>%
    dplyr::select(
      CaseID, PREGORDR, DATEND, DATEND_I,
      BABYSEX1, BABYSEX2, BABYSEX3, BABYSEX4,
      CMKIDIED1, CMKIDIED2, CMKIDIED3, CMKIDIED4
    ) %>%
    
    # 2. Multi-column Pivot
    # This matches the prefix (BABYSEX or CMKIDIED) to a column 
    # and the digit to the 'baby_number' row
    pivot_longer(
      cols = c(starts_with("BABYSEX"), starts_with("CMKIDIED")),
      names_to = c(".value", "baby_num"),
      names_pattern = "(BABYSEX|CMKIDIED)(\\d)"
    ) %>%
    
    # 3. Clean up and Rename
    # Filter to keep only valid births (1=Male, 2=Female)
    filter(BABYSEX %in% c(1, 2)) %>%
    rename(
      sex = BABYSEX,
      yBirth = DATEND,      # Renaming to your target variable name
      dod_cmc = CMKIDIED    # Date of death in CMC
    ) %>%
    
    # 4. Final Ordering and Indexing
    arrange(CaseID, PREGORDR) %>%
    group_by(CaseID) %>%
    mutate(order = row_number()) %>%
    ungroup() %>%
    
    # 5. Remove unnecessary helper columns
    dplyr::select(-baby_num)
  
  #cmc or year?
  df_children_final$dob_cmc <- ifelse((df_children_final$yBirth>0)&(df_children_final$yBirth<1560),
                                      df_children_final$yBirth, #cmc
                                      adjust_cmc_2002_after (compute_cmc(imputed_month(nrow(df_children_final)),df_children_final$yBirth)))
  df_children_final$dob_cmc_I <- ifelse((df_children_final$yBirth>0)&(df_children_final$yBirth<1560),
                                      0, #cmc
                                      1)
  df_children_final$dod_cmc <- ifelse((df_children_final$dod_cmc>0)&(df_children_final$dod_cmc<1560),
                                      df_children_final$dod_cmc, #cmc
                                      adjust_cmc_2002_after (compute_cmc(imputed_month(nrow(df_children_final)),df_children_final$dod_cmc)))
  df_children_final$dod_cmc_I <- ifelse((df_children_final$dod_cmc>0)&(df_children_final$dod_cmc<1560),
                                      0, #cmc
                                      1)
  
  children_wide <- df_children_final %>%
    # 1. Create the count column first
    add_count(CaseID, name = "total_births") %>% 
    # 2. Pivot, ensuring 'total_births' stays as an identifier
    pivot_wider(
      id_cols = c(CaseID, total_births), 
      names_from = order, 
      values_from = c(dob_cmc, dob_cmc_I, sex, dod_cmc, dod_cmc_I),
      names_sep = "" 
    )
  
  return (zap_label(children_wide))
}

live_birth_history2 <- function (births=NULL) {
  # live births from pregnancies. NSFG or year 1980-1990s
  # 'births' has the fields: "CaseID","dob_cmc","babysex1","babysex2","babysex3","dod_cmc1","dod_cmc2","dod_cmc"

  births_long <- births %>%
    tidyr::pivot_longer(
      cols = starts_with("babysex"),
      names_to = "baby_number",
      names_prefix = "babysex",
      values_to = "sex",
      values_drop_na = TRUE
    ) %>%
    mutate(
      dod_cmc = case_when(
        baby_number == "1" ~ dod_cmc1,
        baby_number == "2" ~ dod_cmc2,
        baby_number == "3" ~ dod_cmc3,
        TRUE ~ NA_real_
      ),
      dod_cmc_I = case_when(
        baby_number == "1" ~ dod_cmc_I1,
        baby_number == "2" ~ dod_cmc_I2,
        baby_number == "3" ~ dod_cmc_I3,
        TRUE ~ NA_real_
      )
    )
  
  # order by date of birth
  df_children_final <- births_long %>%
    arrange(CaseID, dob_cmc) %>% 
    group_by(CaseID) %>%
    mutate(order = row_number()) %>%
    ungroup()
  
  children_wide <- df_children_final %>%
    # 1. Calculate the count per mother
    group_by(CaseID) %>%
    mutate(nBioKids = n()) %>%
    ungroup() %>%
    # 2. Pivot to wide format
    # Note: Including nBioKids in id_cols keeps it in the final dataframe
    pivot_wider(
      id_cols = c(CaseID, nBioKids), 
      names_from = order, 
      values_from = c(dob_cmc, dob_cmc_I, dod_cmc, dod_cmc_I, sex),
      names_sep = ""
    ) %>%
    # 3. Ensure nBioKids is the second column
    relocate(nBioKids, .after = CaseID)
  
  return (children_wide)
  
}
