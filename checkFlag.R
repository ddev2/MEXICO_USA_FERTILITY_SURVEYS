checkFlag <- function(df_surv=NSFG_ENADID, flag="union_end_cmc_I", order=0) {
  reorderTable <- function (df) {
    nm <- names(df)
    
    # integer-named columns, sorted by numeric value
    num_idx <- which(grepl("^-?[0-9]+$", nm) & !is.na(nm))
    num_idx <- num_idx[order(as.numeric(nm[num_idx]))]
    
    # keep these fixed
    first_idx <- which(nm == "Survey")
    na_idx    <- which(is.na(nm) | nm == "NA")   # the NA-named column
    sum_idx   <- which(nm == "Sum")
    
    df <- df[, c(first_idx, num_idx, na_idx, sum_idx)]
    
    return (df)
  }
  
  tables <- list()
  n <- 0
  for (i in 1:30) {
    col <- paste0(flag, i)
    if (n == 0 ) t1 <- tabNA(df_surv$survey, df_surv[[col]])
    if (col %in% names(df_surv)) {
      tables[[length(tables) + 1]] <- tabNA(df_surv$survey, df_surv[[col]])
      n <- n + 1
    }
    if (i == order) break
  }
  t <- as.data.frame(do.call(sum_mismatched_tables, tables))
  t <- reorderTable (t)
  nCol <- ncol(t)
  nCol1 <- ncol(t1)
  t[,nCol] <- t1[,nCol1]
  t[,nCol-1] <- t1[,nCol1-1]
  t
}
checkFlag(NSFG_ENADID)
checkFlag(MEXICO_ENADID,order=1)
checkFlag(NSFG_ENADID, "union_start_cmc_I")
checkFlag(NSFG_ENADID, "marriage_start_cmc_I")
checkFlag(MEXICO_ENADID, "union_start_cmc_I",order=1)
checkFlag(MEXICO_ENADID, "marriage_start_cmc_I",order=1)
