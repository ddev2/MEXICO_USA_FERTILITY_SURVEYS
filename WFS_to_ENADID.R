setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
# Convert Mexico WHS file to ENADID format
dataFileName <- file.choose()
load(file=dataFileName)

path_ENADID1977 <- path.expand(paste0(rootPath,"/INEGI/Encuestas/ENADID/WFS_ENADID1977.Rdat"))

keeps <- c(
  "region"="V701",
  "surveyDate_cmc"= "V007",
  "indiv_dob_cmc"="V008",
  "yBirth"="V009",
  "indiv_weight"="V006",
  "nBioKids"="V208",
  "pregnant"="V206",
  "want_another"="V501",
  "ideal_number"="V511",
  "ever_contraception"="V617",
  "union_status"="V107",
  "nUnion"="V101",
  "union_start_type1"="M011",
  "union_start_cmc1"="M012",
  "union_end_cmc1"="M014",
  "union_end_motive1"="M013",
  "union_start_type2"="M021",
  "union_start_cmc2"="M022",
  "union_end_cmc2"="M024",
  "union_end_motive2"="M023",
  "union_start_type3"="M031",
  "union_start_cmc3"="M032",
  "union_end_cmc3"="M034",
  "union_end_motive3"="M033",
  "union_start_type4"="M041",
  "union_start_cmc4"="M042",
  "union_end_cmc4"="M044",
  "union_end_motive4"="M043",
  "union_start_type5"="M051",
  "union_start_cmc5"="M052",
  "union_end_cmc5"="M054",
  "union_end_motive5"="M053",
  "sex1"="B013",
  "dob_cmc1"="B012",
  "sex2"="B023",
  "dob_cmc2"="B022",
  "sex3"="B033",
  "dob_cmc3"="B032",
  "sex4"="B043",
  "dob_cmc4"="B042",
  "sex5"="B053",
  "dob_cmc5"="B052",
  "sex6"="B063",
  "dob_cmc6"="B062",
  "sex7"="B073",
  "dob_cmc7"="B072",
  "sex8"="B083",
  "dob_cmc8"="B082",
  "sex9"="B093",
  "dob_cmc9"="B092",
  "sex10"="B103",
  "dob_cmc10"="B102",
  "sex11"="B113",
  "dob_cmc11"="B112",
  "sex12"="B123",
  "dob_cmc12"="B122",
  "sex13"="B133",
  "dob_cmc13"="B132",
  "sex14"="B143",
  "dob_cmc14"="B142",
  "sex15"="B153",
  "dob_cmc15"="B152",
  "sex16"="B163",
  "dob_cmc16"="B162",
  "sex17"="B173",
  "dob_cmc17"="B172",
  "sex18"="B183",
  "dob_cmc18"="B182",
  "sex19"="B193",
  "dob_cmc19"="B192",
  "sex20"="B203",
  "dob_cmc20"="B202",
  "sex21"="B213",
  "dob_cmc21"="B212",
  "sex22"="B223",
  "dob_cmc22"="B222",
  "sex23"="B233",
  "dob_cmc23"="B232",
  "sex24"="B243",
  "dob_cmc24"="B242"
)

mujeresWFS <- mxsr02[keeps]
remove(mxsr02)
colnames(mujeresWFS) <- names(keeps)

getDatos1977 <- function (mujeresWFS) {
  n <- nrow(mujeresWFS)
  datos <- data.frame(country=rep("MEXICO",n),survey=rep("WFS",n))
  datos <- cbind(datos,mujeresWFS)
  datos$region <- factor(datos$region,levels=seq(1,8,1),
                         labels=c("NORTH-WEST","NORTH-EAST","NORTH","WESTERN","CENTRAL","GULF","SOUTH-EAST","SOUTH-PACIFIC"))
  datos$yBirth <- 1900 + datos$yBirth
  datos$indiv_age_survey <- trunc ((datos$surveyDate_cmc - datos$indiv_dob_cmc)/12)
  datos$pregnant <- ifelse(is.na(datos$pregnant), 8, datos$pregnant)
  datos$pregnant <- factor (datos$pregnant, levels = c(0,1), labels = c("no","yes"))
  datos$want_another <- factor (datos$want_another, levels = c(1,2,88,99), labels = c("yes","no", "not marr or not fecund", "inapplicable"))
  datos$ever_contraception <- factor(datos$ever_contraception, levels = c(0,1), labels = c("no","yes"))
  datos$union_status <- factor(datos$union_status, levels = c(1,2,3,4,88,99), labels = c("in union","widow","separated","separated","single","not stated"))
  datos$union_start_cmc1 <- ifelse(datos$union_start_cmc1==8888, NA, datos$union_start_cmc1)
  datos$union_start_cmc2 <- ifelse(datos$union_start_cmc2==8888, NA, datos$union_start_cmc2)
  datos$union_start_cmc3 <- ifelse(datos$union_start_cmc3==8888, NA, datos$union_start_cmc3)
  datos$union_start_cmc4 <- ifelse(datos$union_start_cmc4==8888, NA, datos$union_start_cmc4)
  datos$union_start_cmc5 <- ifelse(datos$union_start_cmc5==8888, NA, datos$union_start_cmc5)
  datos$union_end_cmc1 <- ifelse(datos$union_end_cmc1==8888, NA, datos$union_end_cmc1)
  datos$union_end_cmc2 <- ifelse(datos$union_end_cmc2==8888, NA, datos$union_end_cmc2)
  datos$union_end_cmc3 <- ifelse(datos$union_end_cmc3==8888, NA, datos$union_end_cmc3)
  datos$union_end_cmc4 <- ifelse(datos$union_end_cmc4==8888, NA, datos$union_end_cmc4)
  datos$union_end_cmc5 <- ifelse(datos$union_end_cmc5==8888, NA, datos$union_end_cmc5)
  datos$union_start_type1 <- ifelse(datos$union_start_type1==8,NA,datos$union_start_type1)
  datos$union_start_type2 <- ifelse(datos$union_start_type2==8,NA,datos$union_start_type2)
  datos$union_start_type3 <- ifelse(datos$union_start_type3==8,NA,datos$union_start_type3)
  datos$union_start_type4 <- ifelse(datos$union_start_type4==8,NA,datos$union_start_type4)
  datos$union_start_type5 <- ifelse(datos$union_start_type5==8,NA,datos$union_start_type5)
  datos$union_start_type1 <- factor(datos$union_start_type1, levels = c(1,2), labels = c("marriage","cohabitation"))
  datos$union_start_type2 <- factor(datos$union_start_type2, levels = c(1,2), labels = c("marriage","cohabitation"))
  datos$union_start_type3 <- factor(datos$union_start_type3, levels = c(1,2), labels = c("marriage","cohabitation"))
  datos$union_start_type4 <- factor(datos$union_start_type4, levels = c(1,2), labels = c("marriage","cohabitation"))
  datos$union_start_type5 <- factor(datos$union_start_type5, levels = c(1,2), labels = c("marriage","cohabitation"))
  datos$union_end_motive1 <- ifelse(datos$union_end_motive1==8,NA,datos$union_end_motive1)
  datos$union_end_motive2 <- ifelse(datos$union_end_motive2==8,NA,datos$union_end_motive2)
  datos$union_end_motive3 <- ifelse(datos$union_end_motive3==8,NA,datos$union_end_motive3)
  datos$union_end_motive4 <- ifelse(datos$union_end_motive4==8,NA,datos$union_end_motive4)
  datos$union_end_motive5 <- ifelse(datos$union_end_motive5==8,NA,datos$union_end_motive5)
  datos$union_end_motive1 <- factor(datos$union_end_motive1, levels = c(1,2,3,4), labels = c("in union","widowhood","separation","separation"))
  datos$union_end_motive2 <- factor(datos$union_end_motive2, levels = c(1,2,3,4), labels = c("in union","widowhood","separation","separation"))
  datos$union_end_motive3 <- factor(datos$union_end_motive3, levels = c(1,2,3,4), labels = c("in union","widowhood","separation","separation"))
  datos$union_end_motive4 <- factor(datos$union_end_motive4, levels = c(1,2,3,4), labels = c("in union","widowhood","separation","separation"))
  datos$union_end_motive5 <- factor(datos$union_end_motive5, levels = c(1,2,3,4), labels = c("in union","widowhood","separation","separation"))

  for (b in (1:24)) {
    sex <- paste0("sex",b)
    datos[,sex] <- ifelse(datos[,sex]==8,NA,datos[,sex])
    dob_cmc <- paste0("dob_cmc",b)
    datos[,dob_cmc] <- ifelse(datos[,dob_cmc]==8888,NA,datos[,dob_cmc])
  }
  
  return (datos)
}

WFS_ENADID1977_full <- getDatos1977(mujeresWFS)
WFS_ENADID1977_full <- cleanENADID(WFS_ENADID1977_full)
WFS_ENADID1977_full <- reorder_birthHistory(WFS_ENADID1977_full)

save(WFS_ENADID1977_full, file=path_ENADID1977)
rm(mujeresWFS)
