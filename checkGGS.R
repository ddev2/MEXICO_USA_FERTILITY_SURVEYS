checkGGS <- function(info_df=GGS_ENADID, country="moldova", dfDaniel=getDatosGGS2(GGS_list$moldova, "moldova"), dfGGS2=GGS_list$moldova) {
  #country="moldova"
  #dfDaniel=getDatosGGS2(GGS_list$moldova)
  #dfGGS2=ggs_data_austria
  df <- subset(info_df, (country==toupper(country))&(survey=="GGS2"))
  if (nrow(df)==0) {
    cat (paste(country,"not found in Harmonized Histories\n"))
    return (df)
  }
  df <- df[,c("country","llave_muj","nBioKids","nUnion")]
  df <- rename(df, nBioKids_harm=nBioKids)
  df <- rename(df, nUnion_harm=nUnion)
  if (!is.null(dfDaniel)) {
    df <- merge(df, dfDaniel, by="llave_muj", all.x=TRUE)
    df <- df %>% relocate(nBioKids, .after=nBioKids_harm)
    df <- df %>% relocate(nUnion, .after=nUnion_harm)
  }
  if (!is.null(dfGGS2)) {
    dfGGS2 <- correctMonthYear (dfGGS2)
    if (country == "germany") {
      dfGGS2 <- rename(dfGGS2, lhi01=lhi01_1402)
      dfGGS2 <- rename(dfGGS2, lhi02=lhi02_1401)
    }
    dfGGS2 <- dfGGS2[,c("respid","dem21","dem28a","dem28bm","dem28by", "dem30a", "dem30bm", "dem30by", "lhi01", "lhi02",
                        "lhi04_m1", "lhi04_y1", "lhi04_m2", "lhi04_y2", "lhi04_m3", "lhi04_y3", "lhi04_m4", "lhi04_y4")]
    
    dfGGS2 <- rename(dfGGS2, llave_muj=respid)
    dfGGS2 <- rename(dfGGS2, hasPartner=dem21)
    dfGGS2 <- rename(dfGGS2, married=dem28a)
    dfGGS2 <- rename(dfGGS2, lastMarrMonth=dem28bm)
    dfGGS2 <- rename(dfGGS2, lastMarrYear=dem28by)
    dfGGS2 <- rename(dfGGS2, LivingPartner=dem30a)
    dfGGS2 <- rename(dfGGS2, lastUnionMonth=dem30bm)
    dfGGS2 <- rename(dfGGS2, lastUnionYear=dem30by)
    dfGGS2 <- rename(dfGGS2, EverLivedWithPartner=lhi01)
    dfGGS2 <- rename(dfGGS2, nUnionPrevious=lhi02)
    dfGGS2 <- rename(dfGGS2, union1Month=lhi04_m1)
    dfGGS2 <- rename(dfGGS2, union1Year=lhi04_y1)
    dfGGS2 <- rename(dfGGS2, union2Month=lhi04_m2)
    dfGGS2 <- rename(dfGGS2, union2Year=lhi04_y2)
    dfGGS2 <- rename(dfGGS2, union3Month=lhi04_m3)
    dfGGS2 <- rename(dfGGS2, union3Year=lhi04_y3)
    dfGGS2 <- rename(dfGGS2, union4Month=lhi04_m4)
    dfGGS2 <- rename(dfGGS2, union4Year=lhi04_y4)
    df <- merge(df, dfGGS2, by="llave_muj", all.x=TRUE)
  }
  dfBio <- subset(df, (nBioKids!=nBioKids_harm))
  dfUnion <- subset(df, (nUnion!=nUnion_harm))
  cat (nrow(dfBio), "differences in number of children and",nrow(dfUnion), "differences in number of unions\n")
  df <- rbind(dfBio,dfUnion)
  
  return(df)
}
