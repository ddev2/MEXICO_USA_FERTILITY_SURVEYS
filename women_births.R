# ==== Women by age and year ====

# For each survey, compute weighted count of women aged 15-49 at mid-year,
# for each calendar year back to the year the oldest woman was aged 39.
#
# CMC convention: month 1 = January 1900.
# Mid-year CMC for year Y = (Y - 1900) * 12 + 6  (June = month 6).

cmc_to_year  <- function(cmc) (cmc - 1L) %/% 12L + 1900L
mid_year_cmc <- function(y)   (y - 1900L) * 12L + 6L


women_by_age_year <- function(df, varWeight="popWeight") {
  
  surveys <- split(df, df$survey)
  
  result_list <- lapply(surveys, function(sdf) {
    
    # --- Survey reference year (mean interview date across respondents) ---
    survey_year <- cmc_to_year(as.integer(round(mean(sdf$surveyDate_cmc, na.rm = TRUE))))
    
    # --- How far back: oldest woman was 39 in year (survey_year - n_back) ---
    age_at_survey <- floor((mid_year_cmc(survey_year) - sdf$indiv_dob_cmc) / 12)
    max_age       <- max(age_at_survey, na.rm = TRUE)
    start_year    <- survey_year - (floor(max_age) - 39L)
    years         <- start_year:survey_year
    
    # --- Weighted count by age 15-49 for each year ---
    year_tabs <- lapply(years, function(y) {
      
      age_y    <- floor((mid_year_cmc(y) - sdf$indiv_dob_cmc) / 12)
      in_range <- !is.na(age_y) & age_y >= 15L & age_y <= 49L
      
      if (!any(in_range)) return(NULL)
      
      # Aggregate weights by age, then expand to guarantee all ages 15-49 present
      agg <- tapply(sdf[[varWeight]][in_range], age_y[in_range], sum, na.rm = TRUE)
      
      data.frame(
        survey  = sdf$survey[[1L]],
        year    = y,
        age     = 15L:49L,
        n_women = as.numeric(agg[as.character(15L:49L)])
      )
    })
    
    do.call(rbind, Filter(Negate(is.null), year_tabs))
  })
  
  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}

births_by_age_year <- function(df, varWeight="popWeight") {
  
  dob_cols <- sort(grep("^dob_cmc\\d+$", names(df), value = TRUE))
  
  surveys <- split(df, df$survey)
  
  result_list <- lapply(surveys, function(sdf) {
    
    survey_year   <- cmc_to_year(as.integer(round(mean(sdf$surveyDate_cmc, na.rm = TRUE))))
    age_at_survey <- floor((mid_year_cmc(survey_year) - sdf$indiv_dob_cmc) / 12)
    max_age       <- max(age_at_survey, na.rm = TRUE)
    start_year    <- survey_year - (floor(max_age) - 39L)
    
    birth_frames <- lapply(dob_cols, function(col) {
      dob   <- sdf[[col]]
      valid <- !is.na(dob)
      if (!any(valid)) return(NULL)
      data.frame(
        indiv_dob_cmc = sdf$indiv_dob_cmc[valid],
        weight        = sdf[[varWeight]][valid],
        birth_cmc     = dob[valid],
        birth_order   = as.integer(sub("^dob_cmc", "", col))  # extract X from dob_cmcX
      )
    })
    
    births <- do.call(rbind, Filter(Negate(is.null), birth_frames))
    if (is.null(births) || nrow(births) == 0L) return(NULL)
    
    births$year <- cmc_to_year(births$birth_cmc)
    births$age  <- floor((births$birth_cmc - births$indiv_dob_cmc) / 12)
    
    keep <- births$year >= start_year  &
      births$year <= survey_year &
      births$age  >= 15L         &
      births$age  <= 49L
    
    births <- births[keep, ]
    if (nrow(births) == 0L) return(NULL)
    
    weight_col <- "weight"
    agg <- aggregate(
      reformulate(c("year", "age", "birth_order"), response = weight_col),
      data = births, FUN = sum, na.rm = TRUE
    )
    names(agg)[names(agg) == weight_col] <- "n_births"
    
    agg$survey <- sdf$survey[[1L]]
    agg[, c("survey", "year", "age", "birth_order", "n_births")]
  })
  
  result <- do.call(rbind, Filter(Negate(is.null), result_list))
  rownames(result) <- NULL
  result
}

women_age_year <- women_by_age_year(MEXICO_ENADID)
births_age_year <- births_by_age_year(MEXICO_ENADID)
