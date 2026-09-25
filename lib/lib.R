GUI <- function(df=NA) {
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(ggplotgui)){
    install.packages("ggplotgui")
    library(ggplotgui)
  }
  ggplot_shiny(df)
}

##### A LIST OF GLOBALS PARAMETER VALUES #####
# can be useful for coding
# the default name of the list is 'k' (can be modified)
# for example set a global variable with global ("varName", aValue) as:
# global ("toto", 1)
# and use it as k$toto
# if you want another name for the global list, instead of "k"
# you should call the function as:
# global ("toto", 1, "myGlobalList")
# and use it as:
# myGlobalList$toto
global <- function(varName=NULL, varValue=NULL, globalListName="k") {
  if (is.null(varName) || is.null(varValue)) stop("varName and varValue should not be null")
  if (class(varName)!="character") stop ("varName should be a string variable")
  if (class(globalListName)!="character") stop ("globalListName should be a string variable")
  
  if (!exists(globalListName)) {
    assign (globalListName, list())
  }
  globalList <- get(globalListName)
  if (class(globalList)!="list") stop (paste(globalListName, "should be a global list"))
  
  globalList[[varName]] <- varValue
  assign (globalListName, globalList, envir=.GlobalEnv)
}

#### Interpolation and smoothing ####
# convert to yearly age
smooth_interpolate <- function(data, age_col, value_col, 
                               method = "spline", smoothing = 0.5) {
  
  # Extract age and value vectors
  ages <- data[[age_col]]
  values <- data[[value_col]]
  
  # Create sequence of annual ages
  min_age <- min(ages)
  max_age <- max(ages)
  annual_ages <- min_age:max_age
  
  # Apply interpolation method
  if (method == "spline") {
    interpolated_values <- spline(x = ages, y = values, 
                                  xout = annual_ages, 
                                  method = "natural")$y
  } else if (method == "smooth.spline") {
    smooth_fit <- smooth.spline(x = ages, y = values, spar = smoothing)
    interpolated_values <- predict(smooth_fit, x = annual_ages)$y
  } else if (method == "linear") {
    interpolated_values <- approx(x = ages, y = values, 
                                  xout = annual_ages, 
                                  method = "linear")$y
  } else {
    stop("Method must be 'spline', 'smooth.spline', or 'linear'")
  }
  
  # Return result
  result <- data.frame(
    age = annual_ages,
    value = interpolated_values
  )
  names(result)[2] <- value_col
  
  return(result)
}

#test smooth interpolate
testSmoothInterpolate <- function(method="spline") {
  # Example usage
  sample_data <- data.frame(
    age = seq(20, 60, by = 5),
    income = c(30000, 35000, 42000, 48000, 52000, 50000, 45000, 40000, 35000)
  )
  sample_data$type <- "decennial"
  # Apply smooth interpolation
  annual_income <- smooth_interpolate(sample_data, "age", "income", 
                                      method = method)
  
  annual_income$type <- "yearly"
  data <- rbind(sample_data,annual_income)
  data$type <- factor(data$type)
  p <- ggplot(data, aes(x=age, y=income, color=type)) +
    geom_point(aes(size = type)) +
    geom_line() +
    scale_size_manual(values = c(3,1)) +
    labs(title = "Income by Age with Smooth Interpolation",
         x = "Age",
         y = "Income") +
    theme_minimal()
  print (p)
}

#### loess smoothing helpers ####
aLoessSmoothedValue <- function (df=NULL, yearSel=NULL, cSpan=1, smooth=TRUE) {
  #pass a dataframe with first column named "Year" and second column named "Value"
  #returns the loess smoothed value which corresponds to year yearSel
  #default value of span equal to 1
  #example:
  #df <- data.frame(Year=c(1978, 1979, 1980, 1981, 1982), Value=c(1,6,4,8,5))
  #yearSel <- 1981 will return an outVal of 6.31
  #smooth<-FALSE
  if (smooth) {
    mod <- loess(Value~Year, data=df, span=cSpan)
    outVal <- predict(mod, data.frame(Year=yearSel), se=FALSE)
  } else {
    outVal <- df$Value[df$Year==yearSel]
  }
  return (outVal)
}

aLoessSmoothedValues <- function (dfToSmooth=NULL, yearsSel=NULL, cSpan=1) {
  #pass a dataframe with first column named "Year" and second column named "Value"
  #minimum of 5 values...
  #if a column named "Weights" exists, then we use it and the return value include min and max for confidence interval
  #returns the loess smoothed values which corresponds to years yearsSel
  #default value of span equal to 1
  #example:
  #dfToSmooth <- data.frame(Year=c(1978, 1979, 1980, 1981, 1982), Value=c(1,6,4,8,5))
  #yearsSel <- c(1979, 1980, 1981) will return an outVal of c(4.30938, 4.00000, 6.30938)
  #example with weights:
  #dfToSmooth <- data.frame(Year=c(1978, 1979, 1980, 1981, 1982), Value=c(1,6,4,8,5), Weights=c(10, 8, 9, 15, 11))
  #yearsSel <- c(1979, 1980, 1981) will return outVal as a dataframe with three columns: Value, min, max
  #example too short:
  #dfToSmooth <- data.frame(Year=c(1978, 1979, 1980, 1981), Value=c(1,6,4,8), Weights=c(10, 8, 9, 15))
  #yearsSel <- c(1978, 1979, 1980, 1981) will return an outVal of c(4.30938, 4.00000, 6.30938)
  
  if (is.null(yearsSel)) {yearsSel<-dfToSmooth$Year}
  
  weightedResults <- ("Weights" %in% colnames(dfToSmooth))
  if (weightedResults) {
    if (all(is.na(dfToSmooth$Value))) {
      dfToSmooth$min <- NA
      dfToSmooth$max <- NA
      return (dfToSmooth)
    }
    sumW <- sum(dfToSmooth$Weight, na.rm=TRUE)
    dfToSmooth$Weights <- ifelse(dfToSmooth$Weights==0, sumW / 1000, dfToSmooth$Weights)
    mod <- loess(Value~Year, data=dfToSmooth, weights=Weights, span=cSpan, control=loess.control(surface="direct"))
  } else {
    if (all(is.na(dfToSmooth$Value))) {
      return (dfToSmooth$Value)
    }
    mod <- loess(Value~Year, data=dfToSmooth, span=cSpan)
  }
  res <- predict(mod, data.frame(Year=yearsSel), se=weightedResults)
  
  if (weightedResults) {
    outVal <- data.frame(Year=yearsSel)
    outVal$Value <- res$fit
    outVal$min <- outVal$Value - res$se.fit
    outVal$max <- outVal$Value + res$se.fit
  } else  {
    names(res) <- yearsSel
    outVal <- res
  }
  
  return (outVal)
}

compare_geom_smooth <- function (df1=sub, df2=outOne) {
  #DEBUG: to check whether the two series are similar, especially if df1 is already smoothed
  p <- ggplot(df1, aes(x=x, y=y)) + geom_line(color="red")
  if (SE_data) {
    p <- p + geom_line(aes(x=x, y=ymin), color="red")
    p <- p + geom_line(aes(x=x, y=ymax), color="red")
  }
  p <- p + geom_line(data=df2, aes(x=x, y=y), color="blue")
  if (SE_data) {
    p <- p + geom_line(data=df2, aes(x=x, y=ymin), color="blue")
    p <- p + geom_line(data=df2, aes(x=x, y=ymax), color="blue")
  }
  p
}

# get geom_smooth data from a plot with loess method, for each group and facet panel,
# and for a given x interval (if not specified, we use the range of x values in the plot)
get_geom_smooth_dataFromPlot <- function (a_ggplot, xInterval=NULL) {
  #internal ggplot values read in ggTable
  ggTable <- ggplot_build(a_ggplot)$data[[1]]
  #facet panels
  panels <- as.numeric(names(table(ggTable$PANEL)))
  nPanel <- length(panels)
  onePanel <- (nPanel==1)
  #number of series in each plot
  groups <- as.numeric(names(table(ggTable$group)))
  nGroup <- length(groups)
  oneGroup <- (nGroup==1)
  out <- data.frame()
  #are there 'ymin' and 'ymax' values?
  SE_data <- "ymin" %in% colnames(ggTable)
  for (pan in (1:nPanel)) {
    for (grp in (1:nGroup)) {
      sub <- subset(ggTable, (PANEL==panels[pan])&(group==groups[grp]))
      #no group series for this facet panel?
      if (dim(sub)[1] == 0) next
      if (is.null(xInterval)) {
        outOne <- data.frame(x=c(min(trunc(sub$x)):max(trunc(sub$x))))
      } else {
        outOne <- data.frame(x=xInterval)
      }
      nObs <- dim(outOne)[1]
      #hack to avoid problems with a small range for the x interval
      #  when there are more than 90 x values
      #  we use a span of 0.1, but
      #  we adjust on-the-fly up to a span of 0.5
      #  for 10 values of the x interval
      cSpan <- max (0.1, 0.5 * 10 / (nObs-(nObs-10)/2)) 
      if (!onePanel) outOne$panel <- pan
      if (!oneGroup) outOne$group <- grp
      mod <- loess(y~x, data=sub, span=cSpan)
      outOne$y <- predict(mod, outOne$x, se=FALSE)
      if (SE_data) {
        mod <- loess(ymin~x, data=sub, span=cSpan)
        outOne$ymin <- predict(mod, outOne$x, se=FALSE)
        mod <- loess(ymax~x, data=sub, span=cSpan)
        outOne$ymax <- predict(mod, outOne$x, se=FALSE)
      }
      #compare_geom_smooth(sub, outOne)
      out <- rbind(out, outOne)
    }
  }
  return (out)
}

#### My ggplot theme ####
theme_text <- function(mainRelSize = 2) {
  biggerRelSize <- mainRelSize * 4 / 3
  ggplot2::theme(
    # white everywhere (the only thing theme_linedraw was giving you)
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key        = ggplot2::element_rect(fill = "white", colour = NA),
    strip.background  = ggplot2::element_rect(fill = "white", colour = NA),
    # text sizing
    axis.text         = ggplot2::element_text(size = ggplot2::rel(mainRelSize)),
    axis.title        = ggplot2::element_text(size = ggplot2::rel(mainRelSize)),
    plot.title        = ggplot2::element_text(size = ggplot2::rel(biggerRelSize),
                                              hjust = 0.5),
    plot.subtitle     = ggplot2::element_text(hjust = 0.5),
    strip.text        = ggplot2::element_text(size = ggplot2::rel(mainRelSize)),
    legend.title      = ggplot2::element_text(size = ggplot2::rel(mainRelSize)),
    legend.text       = ggplot2::element_text(colour = "black",
                                              size = ggplot2::rel(mainRelSize)),
    axis.ticks.length = ggplot2::unit(0.18, "cm")
  )
}

# Grid control in ggplot
# Other possible options:
# Per-side variants
# Almost every axis element splits by side, which matters if you add a secondary axis: axis.text.x.top / .x.bottom, axis.text.y.left / .y.right,
# and likewise axis.line.x.top, axis.ticks.x.bottom, etc.
# Same for tick lengths: axis.ticks.length.x.bottom, axis.ticks.length.y.left.
# Ticks
# Beyond axis.ticks and axis.ticks.length: a negative length (unit(-0.15, "cm")) points the ticks inward.
# As of ggplot2 ≥ 3.5.0 there are also minor tick marks — axis.minor.ticks.x.bottom (and the other sides) plus
# axis.minor.ticks.length.* — which render at your scale's minor_breaks.
# Worth knowing if you want subdivided ticks without subdivided gridlines.
# Axis lines
# element_line for axis.line accepts lineend and an arrow = grid::arrow(...) argument, so you can put arrowheads on the axes if you want that style.
# Text orientation.
# Inside axis.text.x = element_text(...): angle, hjust, vjust, margin, face, family. Rotating long year labels is
# the usual element_text(angle = 45, hjust = 1).
# Panel
# panel.spacing (and .x/.y) for gaps between facets, panel.ontop = TRUE to draw the grid over the data (good for your filled areas), and at the coord level coord_cartesian(clip = "off") to let ticks/labels/annotations spill outside the panel.
# Scale/guide-level controls
# (not theme, but they govern the same visuals): scale_y_continuous(minor_breaks = ..., n.breaks = ...) controls where lines/ticks fall; guide_axis(angle = 45, n.dodge = 2, check.overlap = TRUE) handles crowded labels; and sec.axis = dup_axis() or sec_axis(~ ., ...) mirrors or transforms an axis on the opposite side.
# The two most likely to be useful for your charts: panel.ontop = TRUE (paired with a faint keep = "major.y") so the gridlines read through the country fills, and guide_axis(check.overlap = TRUE) if the decade breaks ever crowd. If you want, I can fold an inward_ticks and a panel.ontop toggle into the function signature so they're parameters rather than manual additions.

theme_noGrid <- function(keep      = "major",   # "none","all","major","minor",
                         # "x","y", or e.g. "major.y"
                         col       = "grey85",
                         linewidth = 0.3,
                         linetype  = "solid",
                         axis_line = TRUE,       # draw the x/y axis lines?
                         border    = FALSE) {    # draw the panel box?
  
  k <- tolower(keep)
  
  # "none" means a fully bare panel: no grid, no axis lines, no border.
  if ("none" %in% k) {
    axis_line <- FALSE
    border    <- FALSE
  }
  
  # Expand coarse tokens into the four concrete grid components.
  all4 <- c("major.x", "major.y", "minor.x", "minor.y")
  sel  <- character(0)
  if ("all"   %in% k) sel <- all4
  if ("major" %in% k) sel <- union(sel, c("major.x", "major.y"))
  if ("minor" %in% k) sel <- union(sel, c("minor.x", "minor.y"))
  if ("x"     %in% k) sel <- union(sel, c("major.x", "minor.x"))
  if ("y"     %in% k) sel <- union(sel, c("major.y", "minor.y"))
  sel <- union(sel, intersect(k, all4))   # explicit fine-grained tokens
  if ("none"  %in% k) sel <- character(0)
  sel <- intersect(all4, sel)             # canonical order, drop typos
  
  line <- ggplot2::element_line(colour = col, linewidth = linewidth,
                                linetype = linetype)
  
  # Start from a clean slate: blank grid, background, and conditionally the
  # axis lines and panel border.
  th <- ggplot2::theme(
    panel.grid       = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border     = if (border)    ggplot2::element_rect(fill = NA, colour = "black")
    else           ggplot2::element_blank(),
    axis.line        = if (axis_line) ggplot2::element_line(colour = "black")
    else           ggplot2::element_blank()
  )
  
  # Re-enable just the requested grid lines (a specific element overrides the
  # blanked parent).
  for (part in sel) {
    th <- th + switch(part,
                      major.x = ggplot2::theme(panel.grid.major.x = line),
                      major.y = ggplot2::theme(panel.grid.major.y = line),
                      minor.x = ggplot2::theme(panel.grid.minor.x = line),
                      minor.y = ggplot2::theme(panel.grid.minor.y = line)
    )
  }
  
  th
}

#### change the name of a variable (column) of a dataframe ####
#example: chgColName (df, "new", "old")
chgColName <- function(df, newName, oldName) {
  colnames(df)[which(colnames(df) == oldName)] <- newName
  
  return (df)
}


#### Attributes of dataframes ####
stripLabels <- function (df) {
  #haven::zap_label and labelled::remove_labels() don't work!
  strip <- function(x) { if (inherits(x, c("haven_labelled","labelled"))) { attr(x,"labels") <- NULL; attr(x,"label") <- NULL; class(x) <- NULL }; x }
  df[] <- lapply(df, strip)
  df
}

# Labels: if varName is NULL, then we delete everything
chgLabels <- function (df, varName=NULL, attrValue=NULL) {
  nVar <- ncol (df)
  if (length(varName) != length(attrValue) & !is.null(varName)) {
    stop ("varName and attrValue should have the same length")
  }
  if (length(varName)>nVar) {
    stop ("varName length is greater than the number of variables in df")
  }
  if (is.null(varName)) {
    varName <- names(df)
    attrValue <- rep(NULL, nVar)
  }
  for (i in (1:nVar)) {
    if (names(df)[i] %in% varName) {
      attr(df[,i], "label") <- attrValue[which(varName==names(df[,i]))]
    }
   }
  return (df)
}

showClass <- function (df) {
  for (i in 1:ncol(df)) {
    print(paste0(names(df)[i],": ",class(df[,i])))
  }
}

library(dplyr)
library(purrr)
library(tidyr)

# a function that spot type mismatch before doing a bind_rows() on a list of dataframes.
# It will return a dataframe with the columns that have type conflicts,
# the types they have in each dataframe, and which type is the majority vs the outlier.
# --- How to use it ---
# my_list <- list(df2017, df2019, df2022)
# check_bind_conflicts(my_list)
# >>> Claude 2026-09-21
# Two fixes.
#
# (1) The dplyr verbs are namespaced. Called with a stray 'filter' object in
#     .GlobalEnv, this function failed with "object 'type' not found", because
#     stats::filter evaluates n_distinct(type) in the calling environment
#     instead of inside the data frame. Reproduced exactly; dplyr:: makes it
#     immune. Run find("filter") if you see that message again.
#
# (2) substitute(list(...)) cannot recover argument names under do.call(). The
#     call in "NSFG import.R" is do.call(check_bind_conflicts, surveys_list),
#     and with an unnamed list the dataframe_name column came out holding the
#     DEPARSED CONTENTS of each data frame rather than its name, which defeats
#     the purpose of the function. It now prefers the list's own names when
#     they exist, so naming surveys_list is all that is needed.
check_bind_conflicts <- function(...) {
  # 1. Capture the names of the dataframes passed as arguments
  arg_list <- list(...)
  nm <- names(arg_list)
  if (is.null(nm) || any(!nzchar(nm))) {
    guess <- as.character(substitute(list(...)))[-1]
    # Under do.call() the "names" are deparsed objects, not symbols. Anything
    # that is not a plain name is replaced by a positional label.
    okName <- (length(guess) == length(arg_list)) & grepl("^[A-Za-z._][A-Za-z0-9._]*$", guess)
    if (length(okName) != length(arg_list)) okName <- rep(FALSE, length(arg_list))
    nm <- ifelse(okName, guess, paste0("arg", seq_along(arg_list)))
  }
  names(arg_list) <- nm

  # 2. Extract types for all columns
  type_summary <- purrr::map_df(arg_list, function(df) {
    dplyr::summarise(df, dplyr::across(dplyr::everything(), ~class(.x)[1]))
  }, .id = "dataframe_name") %>%
    tidyr::pivot_longer(-dataframe_name, names_to = "column_name", values_to = "type")

  # 3. Identify columns with inconsistent types
  conflicts <- type_summary %>%
    dplyr::group_by(column_name) %>%
    dplyr::filter(dplyr::n_distinct(type) > 1) %>%
    dplyr::ungroup()
  
  if (nrow(conflicts) == 0) {
    message("✅ No type conflicts found. bind_rows() should be safe.")
    return(NULL)
  }
  
  # 4. CRITICAL CHECK: Look for Factor vs. Non-Factor mixtures
  # bind_rows() fails specifically when factors meet numeric/integer/character
  factor_clashes <- conflicts %>%
    dplyr::group_by(column_name) %>%
    dplyr::summarise(
      has_factor = any(type == "factor"),
      has_non_factor = any(type != "factor"),
      types_present = paste(unique(type), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::filter(has_factor & has_non_factor)
  
  # 5. Output Warnings
  if (nrow(factor_clashes) > 0) {
    warning("\n‼️ CRITICAL BIND ERROR DETECTED:\n", 
            "The following columns contain a mix of 'factor' and other types. ",
            "This WILL cause bind_rows() to fail:\n", 
            paste("- ", factor_clashes$column_name, " (", factor_clashes$types_present, ")", collapse = "\n"), 
            call. = FALSE)
  }
  
  return(conflicts %>% dplyr::arrange(column_name, type))
}
# <<< Claude 2026-09-21

#### Utilities ####
sum_mismatched_tables <- function(..., colName = "Survey") {
  result <- list(...) %>%
    
    # 1. Force into a table, then to a data frame. 
    # This guarantees a strict long format with columns: Var1, Var2, Freq
    map(~ as.data.frame(as.table(as.matrix(.x)))) %>%
    bind_rows() %>%
    
    # 2. Catch true NAs in the row names (like your missing margin) 
    # and make them characters so pivot_wider handles them safely
    mutate(Var1 = replace_na(as.character(Var1), "<NA>")) %>%
    
    # 3. Pivot directly to the final wide format
    pivot_wider(
      names_from = Var2,             # The survey states (0, 1, 10, etc.)
      values_from = Freq,            # The counts
      values_fill = 0,               # Replaces missing columns with 0 natively
      values_fn = sum                # Automatically sums overlapping rows (Sum, <NA>)
    ) %>%
    
    # 4. Rename the Var1 column and set as rownames
    rename(!!colName := Var1)
  
  return(result)
}

# list fields name, type and other info
describe_df <- function(df) {
  result <- purrr::map_dfr(names(df), function(nm) {
    x      <- df[[nm]]
    n      <- length(x)
    prop_na <- mean(is.na(x))
    
    if (is.factor(x)) {
      list(
        name     = nm,
        type     = "factor",
        min      = NA_real_,
        max      = NA_real_,
        prop_na  = prop_na,
        details  = paste(levels(x), collapse = ", ")
      )
    } else if (is.numeric(x)) {
      list(
        name     = nm,
        type     = class(x)[[1]],
        min      = min(x, na.rm = TRUE),
        max      = max(x, na.rm = TRUE),
        prop_na  = prop_na,
        details  = NA_character_
      )
    } else {
      list(
        name     = nm,
        type     = class(x)[[1]],
        min      = NA_real_,
        max      = NA_real_,
        prop_na  = prop_na,
        details  = NA_character_
      )
    }
  })
  print(result, n = Inf)
}

# no output on source() except cat, print, message and errors
shutOff <- function() { 
  .con <- file(nullfile(), open = "wb")
  sink(.con)
  return (.con)
}
# restore the normal output
# use is, on entry:
# puke <- shutOff()
# at the end of the code:
# on exit(blabla (puke), add=TRUE)
blabla <- function(out) {
  sink()
  close(out)  
}

# loads a R data file in memory
loadFile <- function (aFile=NULL, path=dataPath) {
  if (is.null(aFile)) {
    fileName <- file.choose()
    print (fileName)
  } else {
    fileName <- paste(path, aFile, sep="")
  }
  load (file=fileName, envir = .GlobalEnv)
}

# Rename df1 to df2 and delete df1
renDel <- function (d2, d1) {
  assign(d2, get(d1, envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list=d1, envir = .GlobalEnv)
}

# Remove data objects (variables) from the global environment, keeping all
# functions and anything named in toKeep. Pass FALSE in the second parameter to keep the values as well
rmData <- function(toKeep = character(0), onlyDataLike=TRUE, Rfunctions=FALSE) {
  all_objs <- ls(envir = .GlobalEnv)
  
  is_data_like <- function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || is.matrix(obj) || is.array(obj) ||
      (is.list(obj) && !is.function(obj))
  }
  
   if (onlyDataLike) all_objs[vapply(all_objs, is_data_like, logical(1))]
  
  # TRUE for objects that are functions -> never removed
  is_fun <- vapply(all_objs,
                   function(x) is.function(get(x, envir = .GlobalEnv)),
                   logical(1))
  
  if (!Rfunctions) data_objs <- all_objs[!is_fun]
  to_remove <- setdiff(data_objs, toKeep)
  
  rm(list = to_remove, envir = .GlobalEnv)
  invisible(to_remove)
}

# Remove dataframes and lists from the global environment except what is in the character vector except
rmData <- function(except = NULL) {
  # 1. Get names of all objects in the Global Environment
  all_names <- ls(envir = .GlobalEnv)
  
  # 2. Identify which ones are data.frames or lists
  # We use inherits() because it handles objects with multiple classes well
  is_target <- sapply(all_names, function(x) {
    obj <- get(x, envir = .GlobalEnv)
    inherits(obj, c("data.frame", "list", "gg", "ggplot"))
  })
  
  target_names <- all_names[is_target]
  
  # 3. Remove the exceptions from our "hit list"
  to_remove <- setdiff(target_names, except)
  
  # 4. Execute removal
  if(length(to_remove) > 0) {
    rm(list = to_remove, envir = .GlobalEnv)
    message("Removed: ", paste(to_remove, collapse = ", "))
  } else {
    message("Nothing to remove!")
  }
}

tabNA <- function(... ) {
  t <- table(..., useNA = "always")
  return (addmargins(t))
}

tNA <- function(data, row_var, col_var) {
  library(janitor)
  library(dplyr)
  data %>%
    # Convert the column to a factor to ensure all levels are kept
    mutate({{ col_var }} := factor({{ col_var }})) %>%
    # Create the table
    tabyl({{ row_var }}, {{ col_var }}, show_na = TRUE) %>%
    # Format with percentages and counts
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
}

# How to use it:
# result <- create_custom_tabyl(NSFG_ENADID, survey, union_end_motive1)
OS <- function() {
  if (.Platform$OS.type == "windows") {
    return("Windows")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("Mac")
  } else {
    return("other_OS")
  }
}

#set working directory to current r script file path
setDirCurr <- function() {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

#create a subdirectory if it does not exist and switch to it if needed
checkSubDir <- function(sub_dir=NULL, changeDir=FALSE) {
  if (is.null(sub_dir)) stop ("sub_dir is NULL")
  main_dir <- getwd()
  good <- FALSE
  if (file.exists(sub_dir)){
    # specifying the working directory
    if (changeDir) {setwd(file.path(main_dir, sub_dir))}
    good <- TRUE
  } else {
    # create a new sub directory inside
    # the main path
    dir.create(file.path(main_dir, sub_dir))
    # specifying the working directory
    if (changeDir) {setwd(file.path(main_dir, sub_dir))}
    good <- TRUE
  }
  return (good)
}

library2 <- function(pack) {
  #pack<-"tcltk2"
  if( !(pack %in% utils::installed.packages()))
    {utils::install.packages(pack)}
  library(pack,character.only = TRUE)
}

library_github <- function(githubpack, forceDownload=FALSE) {
  #githubpack<-"josehcms/fertestr"
  pack <- gsub(".*/", "", githubpack)
  if( !(pack %in% utils::installed.packages()) | forceDownload ) {
    library2("devtools")
    devtools::install_github(githubpack)
  }
  library(pack, character.only = TRUE)
}

#get the environment of a variable. Useful when we want to modify inside a function without copying the object
getEnvOf <- function(what, which=rev(sys.parents())) {
  #get the name of object 'what'
  nWhat <- deparse(substitute(what))
  for (frame in which)
    if (exists(nWhat, frame=frame, inherits=FALSE)) 
      return(sys.frame(frame))
  return(NULL)
}

#ifelse with NA value filtered and converted to FALSE
ifelse2 <- function(x, a, b){
  falseifNA <- function(x){
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

#append an object to a list
lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

#append a list to a list
lappendlist <- function(lst1, lst2) {
  n = length(lst2)
  if (n > 0) {
    for (i in 1:n) {
      lst1 <- lappend(lst1, lst2[[i]])
    }
  }
  return (lst1)
}

#change name of factor in a level vector
levelFromTo <- function(levVect=NULL, from=NULL, to=NULL) {
  return (levels(levVect)[which(levVect == from)] <- to)
}

greyPlot <- function(p) {
  p <- p + scale_color_grey(start=0, end=0.9)
  p <- p + scale_fill_grey()
  return (p)
}

#antilogit
alogit <- function(x) {
  return (exp(x)/(1+exp(x)))
}

reOrderVar <- function (df, varName, newColPos) {
  #take data.frame df and put column 'varName' in column 'newColPos'
  #observe that 'newColPos' is the column number AFTER removing column 'varName' (important if we want to shift it rightward)
  ind <- which(colnames(df)==varName)
  vecOrder <- c(1:length(colnames(df)))
  vecOrder <- vecOrder[-ind]
  vecOrder <- append(vecOrder, ind, after=(newColPos-1))
  return (df[,vecOrder])
}

numbersFromStrings <- function (vecStrings) {
  result <- unlist(regmatches(vecStrings, gregexpr("\\d+", vecStrings)))
  
  return (result)
}

#### Fertility analysis ####
#create a reproductive history dataframe (INPUT: each child is in a different row, OUTPUT: all the children are in only one row)
#parameters: the input dataframe and the column number of the mother (or household) id
createReprodHist <- function (df=NULL, colId=NULL) {
  if(!"splitstackshape" %in% utils::installed.packages())
    install.packages("splitstackshape")
  library(splitstackshape)
  
  reshape(getanID(df, colId:colId), direction = "wide", idvar = names(df)[colId:colId], timevar = ".id")
}

## Person-Level Person-Period Converter Function
## https://stats.oarc.ucla.edu/r/faq/how-can-i-convert-from-person-level-to-person-period/
PLPP <- function(data, id, period, event, direction = c("period", "level")) {
  ## Data Checking and Verification Steps
  stopifnot(is.matrix(data) || is.data.frame(data))
  stopifnot(c(id, period, event) %in% c(colnames(data), 1:ncol(data)))
  
  if (any(is.na(data[, c(id, period, event)]))) {
    stop("PLPP cannot currently handle missing data in the id, period, or event variables")
  }
  
  ## Do the conversion
  switch(match.arg(direction),
         period = {
           index <- rep(1:nrow(data), data[, period])
           idmax <- cumsum(data[, period])
           reve <- data[, event]
           dat <- data[index, ]
           dat[, period] <- ave(dat[, period], dat[, id], FUN = seq_along)
           dat[, event] <- 0
           dat[idmax, event] <- reve},
         level = {
           tmp <- cbind(data[, c(period, id)], i = 1:nrow(data))
           index <- as.vector(by(tmp, tmp[, id],
                                 FUN = function(x) x[which.max(x[, period]), "i"]))
           dat <- data[index, ]
           dat[, event] <- as.integer(!dat[, event])
         })
  
  rownames(dat) <- NULL
  return(dat)
}


#### for benchmarking ####
timeThis <- function(...) {
  start.time <- Sys.time();
  eval.parent(...);
  end.time <- Sys.time();
  print(end.time - start.time);
}

#### Excel helpers function ####
#dataframetoExcel
te <- function (df, colFlag=NA, rowFlag=TRUE) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    write.table(df, pipe("pbcopy"), sep="\t", col.names=colFlag, row.names=rowFlag) 
  } else { # Windows OS
    write.table(df, file="clipboard-512", sep="\t", col.names=colFlag, row.names=rowFlag) 
  }
}

#dataframeFromExcel
fe <- function(headerFlag=TRUE, sepChar='\t') {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    y <- (read.table(pipe("pbpaste"), header=headerFlag, sep=sepChar)) 
  } else { # Windows OS
    y <- (read.table("clipboard", header=headerFlag, sep=sepChar))
  }
  if (length(y) == 1) {
    #if the data.frame has only one column, return the data as a vector
    y <- unlist(y)
  }
  return (y)
}

# copy a string to the clipboard
# Works on Mac, Windows, Linux
# Copy TO clipboard
copyTo <- function(text) {
  if (.Platform$OS.type == "windows") {
    # Windows
    writeClipboard(text)
  } else if (Sys.info()["sysname"] == "Darwin") {
    # Mac
    clip <- pipe("pbcopy", "w")
    writeLines(text, clip)
    close(clip)
  } else {
    # Linux (requires xclip)
    clip <- pipe("xclip -selection clipboard", "w")
    writeLines(text, clip)
    close(clip)
  }
  message("✓ Text copied to clipboard!")
}

# Paste FROM clipboard
pasteFrom <- function() {
  if (.Platform$OS.type == "windows") {
    # Windows
    text <- readClipboard()
  } else if (Sys.info()["sysname"] == "Darwin") {
    # Mac
    clip <- pipe("pbpaste", "r")
    text <- readLines(clip)
    close(clip)
  } else {
    # Linux (requires xclip)
    clip <- pipe("xclip -selection clipboard -o", "r")
    text <- readLines(clip)
    close(clip)
  }
  
  return(text)
}

#simple coding in regression for factor (categorical variable)
#use as this:
#contrasts(aFactor) <- simple.coding(aFactor)

simple.coding <- function(aFactor=NULL) {
  nf <- length(levels(aFactor))
  return (contr.treatment(nf)-matrix(rep(1/nf, nf*(nf-1)), ncol=nf-1))
}

#### GGPLOT HELPERS ####
# Expand the panel to fill the chart area, but keep a little headroom at the
# top of y so the highest value/label isn't clipped. Edits the existing
# scales in place, so percent labels, breaks and limits are preserved.
ggplot_fill_panel <- function(p, top = 0.05) {
  present <- unlist(lapply(p$scales$scales, `[[`, "aesthetics"))
  
  # Edit any scales that already exist.
  for (s in p$scales$scales) {
    if ("x" %in% s$aesthetics) s$expand <- ggplot2::expansion(0)
    if ("y" %in% s$aesthetics) s$expand <- ggplot2::expansion(mult = c(0, top))
  }
  # Add ones that don't, so the expansion actually takes effect.
  if (!"x" %in% present)
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(0))
  if (!"y" %in% present)
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, top)))
  
  p
}

# alter a ggplot scale without nullifying all previous settings
# use: # ggplot_set_scale(p, "y", breaks = seq(0, 1, 0.2), minor_breaks = NULL)
ggplot_set_scale <- function(p, aes = "y", ...) {
  props <- list(...)
  for (s in p$scales$scales) {
    if (aes %in% s$aesthetics) for (nm in names(props)) s[[nm]] <- props[[nm]]
  }
  p
}

ggplot_getColours <- function(p, kind="fill") {
  g <- ggplot_build(p)
  return ( unique(g$data[[1]][kind]) )
}

#construct the standard ggplot colors
gg_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Get the number of facets
get_n_facets <- function(plot) {
  build <- ggplot_build(plot)
  n_facets <- length(unique(build$data[[1]]$PANEL))
  return(n_facets)
}

##### plotSameSize #####
plotSameSize <- function(...) {
  aList = list(...)
  gl <- lapply(aList, ggplotGrob2)
  library(grid)
  widths <- do.call(unit.pmax, lapply(gl, "[[", "widths"))
  heights <- do.call(unit.pmax, lapply(gl, "[[", "heights"))
  lg <- lapply(gl, function(g) {g$widths <- widths; g$heights <- heights; g})
  
  return (lg)
}

plotSameSizeRow <- function(...) {
  library(gtable)
  library(grid)
  library(gridExtra)
  aList = list(...)
  gl <- lapply(aList, ggplotGrob)
  aList <- c(gl, list(size = "first"))
  g <- do.call (cbind, aList)

  grid.draw(g)
  
  return (g)
}

#Legend will be centered on all the graph, not only the plot area
#(Tested only with a legend in bottom position !!)
centerLegend <- function(p) {
  library(gtable)
  
  g <- ggplotGrob(p)
  id <- which(g$layout$name == "guide-box")
  g$layout[id, c("l","r")] <- c(1, ncol(g))
  grid.newpage()
  grid.draw(g)
}

## ggplot colors ##
gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##### MULTIPLOT: FUNCTION THAT PUT VARIOUS PLOTS TOGETHER #####
multiplot <-function(..., plotlist=NULL, cols=1,
                     layout=NULL, myWidths=NULL,
                     plotSameSize=FALSE) {
  if (is.null(myWidths)) {myWidths <- rep(1, cols)}
  
  # ... here goes the various ggplots
  # plotList is an optional list of extra ggplots
  # cols is the number of columns
  # and optional matrix layout
  require(grid)
  require(gridExtra)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  plots <- lapply(plots, ggplotGrob2)
  if (plotSameSize) {
    widths <- do.call(unit.pmax, lapply(plots, "[[", "widths"))
    heights <- do.call(unit.pmax, lapply(plots, "[[", "heights"))
    plots <- lapply(plots, function(g) {g$widths <- widths; g$heights <- heights; g})
  }
  
  numPlots <- length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    grid.newpage()
    grid.arrange(grobs=plots, ncol=1)
    
    #print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    grid.arrange(grobs=plots, nrow=trunc(numPlots / cols), ncol=cols, widths=myWidths)
    
    #pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    # for (i in 1:numPlots) {
    #   # Get the i,j matrix positions of the regions that contain this subplot
    #   matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    #   
    #   print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
    #                                   layout.pos.col = matchidx$col))
    # }
  }
}


#left over from a previous version of reshape2
guess_value <- function(df) {
  if ("value" %in% names(df)) return("value")
  if ("(all)" %in% names(df)) return("(all)")
  
  last <- names(df)[ncol(df)]
  message("Using ", last, " as value column: use value.var to override.")
  
  last
}

##### ggplot function for creating smooth steps and ribbons #####
#for not ribbon, set ribbon_alpha=0
geom_step_ribbon <- function (df=NULL, xmin=NULL, xmax=NULL, argPlot=NULL, ggPlot_curr=NULL, isLegend=TRUE,
                              ribbon_alpha=0.2, ...) {
  if ( is.null(df) || is.null(xmax) ) stop ("all arguments should be different from NULL")
  
  df_ribbon <- data.frame(
    x=c(df$x[1], rep(df$x[2:length(df$x)], each=2), xmax),
    y=rep(df$y, each=2),
    ymin=rep(df$ymin, each=2),
    ymax=rep(df$ymax, each=2)
    )
  df_ribbon$x <-df_ribbon$x - 0.5
  
  if (is.null(ggPlot_curr)) {
    p <- ggplot()
  } else {
    p <- ggPlot_curr
  }
  if ( !is.null(argPlot) ) {
    p <- p + argPlot
  }
  p <- p + geom_ribbon(data=df_ribbon, aes(x=x, ymin=ymin, ymax=ymax), alpha=ribbon_alpha)
  p <- p + geom_line(data=df_ribbon, aes(x=x, y=y), ...)
  if (!isLegend) {
    p <- p + theme(legend.position = 'none')
  }
  
  return (p)
}

geom_hsegment_sign <- function (df=NULL, xmax=NULL, ggplot=NULL, nullValue=NULL, ribbon=FALSE) {

  if ( is.null(df) || is.null(xmax) || is.null(ggplot) || is.null(nullValue)) stop ("all arguments should be different from NULL")
  
  df_segment <- data.frame(
    x=df$x,
    xend=c(df$x[2:length(df$x)], xmax),
    y=df$y,
    significant=sign(df$ymin-nullValue)==sign(df$ymax-nullValue)
  )
  df_segment$x <-df_segment$x - 0.5
  df_segment$xend <-df_segment$xend - 0.5
  df_segment$y_sign <- ifelse(df_segment$significant, df_segment$y, NA)
  df_segment$y_non.sign <- ifelse(!df_segment$significant, df_segment$y, NA)
  
  ggplot <- ggplot + geom_segment(data=df_segment, aes(x=x, xend=xend, y=y_sign, yend=y_sign))
  ggplot <- ggplot + geom_segment(data=df_segment, aes(x=x, xend=xend, y=y_non.sign, yend=y_non.sign), linetype="dotted")
  
  if (ribbon) {
    df_ribbon <- data.frame(
      x=c(df$x[1], rep(df$x[2:length(df$x)], each=2), xmax),
      y=rep(df$y, each=2),
      ymin=rep(df$ymin, each=2),
      ymax=rep(df$ymax, each=2)
    )
    df_ribbon$x <-df_ribbon$x - 0.5

    ggplot <- ggplot + geom_ribbon(aes(x=df_ribbon$x, y=df_ribbon$y, ymin=df_ribbon$ymin, ymax=df_ribbon$ymax, alpha=0.1))
    ggplot <- ggplot + theme(legend.position = 'none')
  }
  
  return (ggplot)
}

highRes <- function(df=NULL, yres=0.01) {
  #we will use the variable in the first column of df to create highRes intervals
  #this variable will be our y column
  #the other columns will use the number of intervals created for this first variable
  
  nVar <- ncol(df)
  yres <- 0.01
  
  for (var in 1:nVar) {
    df$tmp <- c(df[,var][-1], NA)
    colnames(df)[length(df)] <- paste(colnames(df)[var], "_2", sep="")
  }
  df <- df[1:(nrow(df)-1), ] # remove last row
  
  # new high-resolution y coordinates between each pair within each group
  y.new <- apply(df, 1, function(x) {
    seq(x[1], x[nVar+1], yres*sign(x[nVar+1] - x[1]))
  })
  
  df.out <- data.frame(y=unlist(y.new))
  colnames(df.out)[ncol(df.out)] <- colnames(df)[ncol(df.out)]
  
  df$len <- sapply(y.new, length) # length of each series of points
  for (var in 2:nVar) {
    var.new <- apply(df, 1, function(x) {
      seq(x[var], x[nVar+var], length.out=x['len'])
    })
    df.out$tmp <- unlist(var.new)
    colnames(df.out)[ncol(df.out)] <- colnames(df)[ncol(df.out)]
  }
  
  return (df.out)
}

# obsolete: two Y axes
two_y_axes <- function (p1, p2, mergeLegend=FALSE) {
  ## http://lehoangvan.com/posts/dual-y-axis-ggplot2/
  library(ggplot2)
  library(gtable)
  library(grid)
  
  # Get the plot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  
  # Get the locations of the plot panels in g1.
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  # Overlap panel for second plot on that of the first plot
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # ggplot contains many labels that are themselves complex grob; 
  # usually a text grob surrounded by margins.
  # When moving the grobs from, say, the left to the right of a plot,
  # make sure the margins and the justifications are swapped around.
  # The function below does the swapping.
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
  hinvert_title_grob <- function(grob){
    
    # Swap the widths
    widths <- grob$widths
    grob$widths[1] <- widths[3]
    grob$widths[3] <- widths[1]
    grob$vp[[1]]$layout$widths[1] <- widths[3]
    grob$vp[[1]]$layout$widths[3] <- widths[1]
    
    # Fix the justification
    grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
    grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
    grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
    grob
  }
  
  # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
  index <- which(g2$layout$name == "axis-l")  # Which grob
  yaxis <- g2$grobs[[index]]                  # Extract the grob
  
  # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
  # The relevant grobs are contained in axis$children:
  #   axis$children[[1]] contains the axis line;
  #   axis$children[[2]] contains the tick marks and tick mark labels.
  
  # Second, swap tick marks and tick mark labels
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  
  # Third, move the tick marks
  # Tick mark lengths can change. 
  # A function to get the original tick mark length
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
  plot_theme <- function(p) {
    plyr::defaults(p$theme, theme_get())
  }
  
  tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
  
  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  
  # Fifth, put ticks back into yaxis
  yaxis$children[[2]] <- ticks
  
  # Put the transformed yaxis on the right side of g1
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # # Labels grob
  # left = textGrob("Number in Russia", x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14, col =  "#68382C", fontfamily = "OfficinaSanITCMedium"))
  # right =  textGrob("Rest of World", x = 1, y = 0.9, just = c("right", "top"), gp = gpar(fontsize = 14, col =  "#00a4e6", fontfamily = "OfficinaSanITCMedium"))
  # labs = gTree("Labs", children = gList(left, right))
  # 
  # # New row in the gtable for labels
  # height = unit(3, "grobheight", left)
  # g1 <- gtable_add_rows(g1, height, 2)  
  # 
  # # Put the label in the new row
  # g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)
  # 
  # # Turn off clipping in the plot panel
  # g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"
  
  # right axis title
  g1 <- gtable_add_grob(g1, g2$grob[[7]], pp$t, length(g1$widths), pp$b)

    if (mergeLegend) {
    leg1 <- g_legend(g1)
    leg2 <- g_legend(g2)
    
    if (!is.null(leg1) && !is.null(leg2))
      g1$grobs[[which(g1$layout$name == "guide-box")]] <- 
      gtable:::cbind_gtable(leg1, leg2, "first")
  }
  return (g1)
}

two_y_axes_old <- function (p1, p2, mergeLegend=FALSE) {
  library(ggplot2)
  library(gtable)
  library(grid)
  
  # two plots
  
  p2 <- p2 +
    theme(panel.background = element_rect(fill = NA), panel.grid = element_blank())
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # right axis title
  g <- gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b)
  
  if (mergeLegend) {
    leg1 <- g_legend(g1)
    leg2 <- g_legend(g2)
    
    if (!is.null(leg1) && !is.null(leg2))
    g$grobs[[which(g$layout$name == "guide-box")]] <- 
      gtable:::cbind_gtable(leg1, leg2, "first")
  }
  return (g)
}

#Extract Legend
g_legend<-function(a.gplot){ 
  return(g_extract(a.gplot, "guide-box"))
}

#extract grid piece of a ggplot object
g_extract<-function(a.gplot, what, index=FALSE) {
  if (class(a.gplot)[1] == "gtable") {
    tmp <- a.gplot
  } else {
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  }
  leg <- which( grepl( what, sapply(tmp$grobs, function(x) x$name) ) )
  if (length(leg) > 0) {
    if (index) {
      return(leg)
    } else {
      legend <- tmp$grobs[[leg]] 
      return(legend)
    }
   } else {
    return(NULL)
  }
}

#row of ggplot or grid object with a shared legend
grid_arrange_legend <- function(plots=NULL, aLegend, nRows=1) {
  library(gridExtra)
  lheight <- sum(aLegend$height)
  for (i in (1:length(plots))) {
    ggPlotObject <- class(plots[[i]])[1] == "gg"
    if (ggPlotObject) {
      plots[[i]] <- plots[[i]] + theme(legend.position="none")
    }
  }
  
  return (
  grid.arrange(
    arrangeGrob(grobs=plots,
                nrow=nRows),
    aLegend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
  )
}

#helper function for plotSameSize
ggplotGrob2 <- function(p) {
  if (class(p)[1] == "gg") {
    return (ggplotGrob(p))
  } else {
    return (p)
  }
}

##### FACETADJUST #####
# have axis title for all when the number of facet is odd
# see: http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot/13316126#13316126

facetAdjust <- function(x, pos = c("up", "down"))
{
  if (packageVersion(ggplot2)>='2.2.1') {
    return (x)
  }
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

#helper function for above one
print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  library(grid)
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}

# pos - where to add new labels
# newpage, vp - see ?print.ggplot
facetAdjust2 <- function(x, pos = c("up", "down"), 
                        newpage = is.null(vp), vp = NULL)
{
  if (packageVersion(ggplot2)>='2.2.1') {
    return (x)
  }
  library(grid)
  # part of print.ggplot
  ggplot2:::set_last_plot(x)
  if(newpage)
    grid.newpage()
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p)
  # finding dimensions
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  # number of panels in the plot
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  # missing panels
  n <- space - panels
  # checking whether modifications are needed
  if(panels != space){
    # indices of panels to fix
    idx <- (space - ncol - n + 1):(space - ncol)
    # copying x-axis of the last existing panel to the chosen panels 
    # in the row above
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      # if pos == down then shifting labels down to the same level as 
      # the x-axis of last panel
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  # again part of print.ggplot, plotting adjusted version
  if(is.null(vp)){
    grid.draw(gtable)
  }
  else{
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }
  invisible(p)
}

##### UPDATING R #####
# updating R and insuring that we have our package list up to date...
# https://www.datascienceriot.com/how-to-upgrade-r-without-losing-your-packages/kris/
# also have a look at packages location on disk:
# http://stackoverflow.com/questions/13656699/update-r-using-rstudio
# there is also a complete package for Mac, similar to updateR for Windows, but I have not used it (but have installed it):
# https://andreacirilloblog.wordpress.com/2015/10/22/updater-package-update-r-version-with-a-function-on-mac-osx/

# before updating R...
beforeUp <- function() {
  tmp <- utils::installed.packages()
  installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
  save(installedpkgs, file="installed_old.rda")
}

# after updating R...
# (then we can delete the old version files located in /Library/Frameworks/R.framework/Versions/)
afterUp <- function() {
  load("installed_old.rda")
  tmp <- utils::installed.packages()
  installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
  missing <- setdiff(installedpkgs, installedpkgs.new)
  install.packages(missing)
  update.packages()
}

##### SECRET: for creating passwords: enter a string and a number is returned #####
secret <- function(s=NULL, nDigit=4) {
  #s<-"namecoins"
  #nDigit<-5
  res <- 33
  modValue <- 0
  for (i in 1:nDigit) {
    modValue <- modValue * 10 + 9
  }
  for (c in 1:nchar(s)) {
    res <- res + utf8ToInt (substr(s,c,c)) * (c+4)
  }
  return (res %% modValue)
}

