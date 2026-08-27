raw2002 = getDatos_2002()

#### problem with partners with kids in previous unions affect only marriages ####
idx=raw2002$union_start_type1=="marriage"
idx[is.na(idx)] <- NA
tabNA(raw2002$husb_priorKids_1[idx],is.na(raw2002$union_end_cmc1[idx]))
idx1=raw2002$union_start_type1=="cohabitation before marriage"
idx1[is.na(idx1)] <- NA
tabNA(raw2002$husb_priorKids_1[idx1],is.na(raw2002$union_end_cmc1[idx1]))
idx2=raw2002$union_start_type1=="cohabitation"
idx2[is.na(idx2)] <- NA
tabNA(raw2002$husb_priorKids_1[idx2],is.na(raw2002$union_end_cmc1[idx2]))

#### marriage start and end: how many have husbands with previous kids ####
##### marriage 1 #####
tabNA(df_NSFG_2002$KIDSHX,is.na(df_NSFG_2002$CMMARRHX)) # start
tabNA(df_NSFG_2002$KIDSHX,is.na(df_NSFG_2002$CMSTPHSBX)) # end
##### marriage 2 #####
tabNA(df_NSFG_2002$KIDSHX2,is.na(df_NSFG_2002$CMMARRHX2))
tabNA(df_NSFG_2002$KIDSHX2,is.na(df_NSFG_2002$CMSTPHSBX2)) # end
##### marriage 3 #####
tabNA(df_NSFG_2002$KIDSHX3,is.na(df_NSFG_2002$CMMARRHX3))
tabNA(df_NSFG_2002$KIDSHX3,is.na(df_NSFG_2002$CMSTPHSBX3)) # end
##### marriage 4 #####
tabNA(df_NSFG_2002$KIDSHX4,is.na(df_NSFG_2002$CMMARRHX4))
tabNA(df_NSFG_2002$KIDSHX4,is.na(df_NSFG_2002$CMSTPHSBX4)) # end


#### separated women still married with no date of end of union ####
idx1 <- (df_NSFG_2002$TIMESMAR==1)
tabNA(df_NSFG_2002$FMARIT[idx1],is.na(df_NSFG_2002$CMSTPHSBX[idx1]))
tabNA(df_NSFG_2002$MARSTAT[idx1],is.na(df_NSFG_2002$CMSTPHSBX[idx1]))
idx2 <- (df_NSFG_2002$TIMESMAR==2)
tabNA(df_NSFG_2002$FMARIT[idx2],is.na(df_NSFG_2002$CMSTPHSBX2[idx2]))
tabNA(df_NSFG_2002$MARSTAT[idx2],is.na(df_NSFG_2002$CMSTPHSBX2[idx2]))
idx3 <- (df_NSFG_2002$TIMESMAR==3)
tabNA(df_NSFG_2002$FMARIT[idx3],is.na(df_NSFG_2002$CMSTPHSBX3[idx3]))
tabNA(df_NSFG_2002$MARSTAT[idx3],is.na(df_NSFG_2002$CMSTPHSBX3[idx3]))
idx4 <- (df_NSFG_2002$TIMESMAR==4)
tabNA(df_NSFG_2002$FMARIT[idx4],is.na(df_NSFG_2002$CMSTPHSBX3[idx4]))
tabNA(df_NSFG_2002$MARSTAT[idx4],is.na(df_NSFG_2002$CMSTPHSBX3[idx4]))

raw2002 = getDatos_2002()
maxU <- max(raw2002$nUnion)
for (u in (1:maxU)) {
  union_start_type <- paste0("union_start_type",u)
  idx <- (raw2002$nUnion==u)&(raw2002[[union_start_type]] %in% c("marriage", "cohabitation before marriage"))
  union_end_cmc <- paste0("union_end_cmc",u)
  t<-tabNA(raw2002$union_status[idx],is.na(raw2002[[union_end_cmc]][idx]))
  print (t)
}


tabNA(NSFG_ENADID$survey,NSFG_ENADID$sep_recent)

idx2002=NSFG_ENADID$survey=="NSFG2002"
tabNA(is.na(NSFG_ENADID$union_end_cmc1[idx2002]),NSFG_ENADID$sep_recent_1[idx2002])
tabNA(is.na(NSFG_ENADID$union_end_cmc2[idx2002]),NSFG_ENADID$sep_recent_2[idx2002])
tabNA(is.na(NSFG_ENADID$union_end_cmc3[idx2002]),NSFG_ENADID$sep_recent_3[idx2002])
tabNA(is.na(NSFG_ENADID$union_end_cmc4[idx2002]),NSFG_ENADID$sep_recent_4[idx2002])
tabNA(is.na(NSFG_ENADID$union_end_cmc5[idx2002]),NSFG_ENADID$sep_recent_5[idx2002])
tabNA(is.na(NSFG_ENADID$union_end_cmc6[idx2002]),NSFG_ENADID$sep_recent_6[idx2002])

# ==== Diagnose sep_recent vs sep_recent_1..6 (NSFG2002) ====

sep_cols <- paste0("sep_recent_", 1:6)

# Per-slot TRUE counts and their sum
sapply(sep_cols, function(v) sum(NSFG_ENADID[[v]][idx2002], na.rm = TRUE))
sum(sapply(sep_cols, function(v) sum(NSFG_ENADID[[v]][idx2002], na.rm = TRUE)))  # 229
sum(NSFG_ENADID$sep_recent[idx2002], na.rm = TRUE)                              # 261

# OR across the six slots, per respondent (NA -> not flagged)
any_slot <- Reduce(`|`, lapply(sep_cols, function(v) NSFG_ENADID[[v]][idx2002] %in% TRUE))

# This cell is your 32: aggregate TRUE but flagged in no slot
tabNA(NSFG_ENADID$sep_recent[idx2002], any_slot)

# Pull those rows and inspect their union + separation variables
disc <- which(NSFG_ENADID$sep_recent[idx2002] %in% TRUE & !any_slot)
length(disc)                                                                    # expect ~32
cols_show <- c("nUnion", grep("^union_end_cmc", names(NSFG_ENADID), value = TRUE), sep_cols)
NSFG_ENADID[idx2002, cols_show][disc, ]

#### women separated and with no date of union
suf <- c("", "2", "3", "4", "5", "6")
suf <- suf[paste0("CMMARRHX", suf) %in% names(df_NSFG_2002)]      # husbands present
cmmar <- sapply(suf, function(s) df_NSFG_2002[[paste0("CMMARRHX",  s)]])
stopL <- sapply(suf, function(s) df_NSFG_2002[[paste0("CMSTPHSBX", s)]])
death <- sapply(suf, function(s) df_NSFG_2002[[paste0("CMHSBDIEX", s)]])

nHusb <- rowSums(!is.na(cmmar))                                    # dated marriages
gi    <- cbind(seq_len(nrow(df_NSFG_2002)), ifelse(nHusb >= 1, nHusb, NA))

sep <- df_NSFG_2002$RMARITAL == 5
gap <- sep & nHusb >= 1 & is.na(stopL[gi]) & is.na(death[gi])     # last husband: no end
sum(gap, na.rm = TRUE)

