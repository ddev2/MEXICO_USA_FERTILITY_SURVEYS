setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
library(foreign)
# convert Colombia DHS 2015 to ENADID format

pathColDHS2015 <- path.expand("~/Library/CloudStorage/GoogleDrive-ddevolder@ced.uab.es/My Drive/Angela/Trabajo DANE 2022/DHS/co2015.csv")
pathCol2015_ENADID <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/DHS_Col2015_ENADID.Rdat"))

col2015 <- computeYearBirth (read.csv(pathColDHS2015))
col2015$weight <- col2015$weight / 1000000
col2015$DHSyear <- 2015

n <- nrow(col2015)
DHS_col <- data.frame(country=rep("COLOMBIA",n),survey=rep("DHS",n))

DHS_col$llave_muj <- NA
DHS_col$surveyDate_cmc <- col2015$cmcSurvey
DHS_col <- compute_lastYear(DHS_col)
DHS_col$indiv_dob_cmc <- col2015$cmcBirthEgo
DHS_col$yBirth <- col2015$yBirthEgo
DHS_col$indiv_weight <- col2015$weight
DHS_col$nBioKids <- col2015$nBirthsTot
DHS_col$pregnant <- NA
DHS_col$want_another <- NA
DHS_col$age_first_sex <- NA
DHS_col$ever_contraception <- NA
DHS_col$union_status <- col2015$maritalStatus
# 0 "Never in union"
# 1 "Married"
# 2 "Living with partner"
# 3 "Widowed"
# 4 "Divorced"
# 5 "No longer living together/separated"
DHS_col$union_status <- factor(DHS_col$union_status, levels=c(0,1,2,3,4,5),
                               labels=c("single","marriage","cohabitation","widowhood","separated","separated"))
DHS_col$nUnion <- col2015$nUnion

maxU <- max (DHS_col$nUnion)
for (u in (1:maxU)) {
  DHS_Utype <- paste0("unionType",u)
  DHS_Ustart <- paste0("cmcUnion",u)
  DHS_Uend <- paste0("cmcEndUnion",u)
  DHS_UendMotive <- paste0("reasonEnd",u)
  DHS_marriedLater <- paste0("marriedLater",u)
  DHS_Mstart <- paste0("cmcMarriage",u)
  Utype <- paste0("union_start_type",u)
  Ustart <- paste0("union_start_cmc",u)
  Uend <- paste0("union_end_cmc",u)
  UendMotive <- paste0("union_end_motive",u)
  marriedLater <- paste0("marriedLater",u)
  Mstart <- paste0("marriage_start_cmc",u)
  
  DHS_col[[Utype]] <- col2015[[DHS_Utype]] # (1: married, 2: cohabitation)
  DHS_col[[Ustart]] <- col2015[[DHS_Ustart]]
  DHS_col[[Uend]] <- col2015[[DHS_Uend]]
  DHS_col[[UendMotive]] <- col2015[[DHS_UendMotive]] # (1: widowed, 2: separated, 3: divorced)
  idx <- (!is.na(DHS_col[[Ustart]]))&(is.na(DHS_col[[UendMotive]]))
  idx[is.na(idx)] <- FALSE
  DHS_col[[UendMotive]][idx] <- 0 # if union start and no end motive, we consider that the union is still active (in union)
  DHS_col[[UendMotive]] <- factor(DHS_col[[UendMotive]], levels=c(0,1,2,3),
                                  labels=c("in union", "widowhood", "separation", "separation"))
  DHS_col[[marriedLater]] <- col2015[[DHS_marriedLater]] # serve to re-codify (0: no, 1: yes)
  DHS_col[[Mstart]] <- col2015[[DHS_Mstart]]

  # union type is marriage, we copy the cmc of union start  
  idx <- (DHS_col[[Utype]] == 1)
  idx[is.na(idx)] <- FALSE
  DHS_col[[Mstart]][idx] <- DHS_col[[Ustart]][idx]
  
  idx <- (DHS_col[[marriedLater]] == 1)&(is.na(DHS_col[[Mstart]]))
  idx[is.na(idx)] <- FALSE
  if (sum(idx) > 0) {
    cat(sum(idx),"marriage later have no marriage cmc, we copy from union cmc\n")
    DHS_col[[Mstart]][idx] <- col2015[[DHS_Ustart]][idx]
  }
  
  # union type
  idx <- (DHS_col[[Ustart]] == DHS_col[[Mstart]])
  idx[is.na(idx)] <- FALSE
  DHS_col[[Utype]][idx] <- 1
  idx <- (!is.na(DHS_col[[Ustart]]))&(is.na(DHS_col[[Mstart]]))
  idx[is.na(idx)] <- FALSE
  DHS_col[[Utype]][idx] <- 2
  idx <- (DHS_col[[Ustart]] < DHS_col[[Mstart]])
  idx[is.na(idx)] <- FALSE
  DHS_col[[Utype]][idx] <- 3
  #check
  idx <- (DHS_col[[Utype]] == 3) & (DHS_col[[marriedLater]] != 1)
  idx[is.na(idx)] <- FALSE
  if (sum(idx) > 0) cat ("mismatch between married later and type: cohabitation before marriage at order",u,"\n")
  
  DHS_col[[Utype]] <- factor (DHS_col[[Utype]],levels=c(1,2,3),labels=c("marriage","cohabitation","cohabitation before marriage"))
  
  DHS_col[[marriedLater]] <- NULL
}

maxB <- max (DHS_col$nBioKids)
for (b in (1:maxB)) {
  sex <- paste0("sex",b)
  dob_cmc <- paste0("dob_cmc",b)
  cmcBirthChild <- paste0("cmcBirthChild",b)
  DHS_col[[sex]] <- NA # no info
  DHS_col[[dob_cmc]] <- col2015[[cmcBirthChild]]
}

DHS_col <- cleanENADID(DHS_col)
DHS_col <- reorder_birthHistory(DHS_col)

save(DHS_col, file=pathCol2015_ENADID)
pathGGS_ENADID <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/GGS_ENADID.Rdat"))
GGS_ENADID <- subset(GGS_ENADID, (country!="COLOMBIA"))
GGS_ENADID <- join_with_harmonized(df1=GGS_ENADID, df2=DHS_col, aSurvey="DHS")

save(GGS_ENADID, file=pathGGS_ENADID)
