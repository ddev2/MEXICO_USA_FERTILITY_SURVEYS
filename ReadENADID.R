setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
source ("WFS_to_ENADID.r")
source ("ReadMujeres1992.r")
source ("ReadMujeres1997.r")
source ("ReadMujeres2006.r")
source ("ReadMujeres2009.r")
source ("ReadMujeres2014.r")
source ("ReadEDER2017.r")
source ("ReadMujeres2018.r")
source ("ReadMujeres2023.r")
source ("ReadEDER2025.r")
rm(mujeres1992)
rm(mujeres1997)
rm(mujeres2006)
rm(mujeres2009)
rm(mujeres2014)
rm(mujeres2018)
rm(mujeres2023)
rm(embarazos)
rm(hogar)

MEXICO_ENADID <- WFS_ENADID1977_full
MEXICO_ENADID$region <- NULL

MEXICO_ENADID <- ENADID1992_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID1997_full$want_another <- factor (ENADID1997_full$want_another, levels=c(1,2,3),labels=c("yes", "no", "don't know"))
ENADID1997_full$ever_contraception <- factor (ENADID1997_full$ever_contraception, levels=c(1,2,3),labels=c("yes", "no", "don't know"))
MEXICO_ENADID <- ENADID1997_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID2006_full$want_another <- factor (ENADID2006_full$want_another, levels=c(1,2,3),labels=c("yes", "no", "don't know"))
MEXICO_ENADID <- ENADID2006_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID2009_full$want_another <- factor (ENADID2009_full$want_another, levels=c(1,2,3),labels=c("yes", "no", "don't know"))
MEXICO_ENADID <- ENADID2009_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID2014_full$want_another <- factor(ENADID2014_full$want_another, levels = c(1, 2, 3, 4), labels = c("yes", "yes but can't", "no", "don't know"))
MEXICO_ENADID <- ENADID2014_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

EDER_ENADID$want_another <- factor(EDER_ENADID$want_another, levels = c(1, 2, 3, 4), labels = c("yes", "yes but can't", "no", "don't know"))
MEXICO_ENADID <- EDER_ENADID %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID2018_full$want_another <- factor(ENADID2018_full$want_another, levels = c(1, 2, 3, 4), labels = c("yes", "yes but can't", "no", "don't know"))
ENADID2018_full$llave_muj <- as.character(ENADID2018_full$llave_muj)
MEXICO_ENADID <- ENADID2018_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

ENADID2023_full$want_another <- factor(ENADID2023_full$want_another, levels = c(1, 2, 3, 4), labels = c("yes", "yes but can't", "no", "don't know"))
ENADID2023_full$llave_muj <- as.character(ENADID2023_full$llave_muj)
MEXICO_ENADID <- ENADID2023_full %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

EDER_ENADID25$want_another <- factor(EDER_ENADID25$want_another, levels = c(1, 2, 3, 4), labels = c("yes", "yes but can't", "no", "don't know"))
MEXICO_ENADID <- EDER_ENADID25 %>%
  #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
  bind_rows(MEXICO_ENADID, .)

MEXICO_ENADID$survey <- factor (MEXICO_ENADID$survey)

#### reweight the surveys ####
PobEdadMex <- function () {
  return (
    structure(list(
      Age = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
              12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
              28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
              44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
              60, 61, 62, 63, 64, 65),
      WFS = c(1109598.5, 1079703, 1063531, 
              1045732, 1027303.5, 1006600, 983312.5, 958318, 930376, 900935.5, 
              871979.5, 842530.5, 811947.5, 780538, 749232, 719285.5, 691078, 
              663938, 637891, 612003.5, 586042, 561759, 538200.5, 514525.5, 
              491692.5, 468691.5, 448582, 431921.5, 410107.5, 386063.5, 367787.5, 
              353316.5, 340034, 326651.5, 311675.5, 293300.5, 275081.5, 260129, 
              248125, 241395.5, 237436, 233195.5, 230210, 226493.5, 220966, 
              214593.5, 208784, 203688.5, 196648.5, 187721.5, 178963, 171750.5, 
              164996, 156440, 146315, 136386.5, 128344.5, 122098.5, 116312.5, 
              111152.5, 107229, 104099, 100755, 96720.5, 92516, 88352),
      ENADID1992 = c(1161293.5, 
                     1141100.5, 1122136.5, 1106533.5, 1097529.5, 1087109.5, 1076002, 
                     1064208, 1053554.5, 1050276, 1051747, 1044350.5, 1028466, 1015311.5, 
                     1016057, 1023087.5, 1020656.5, 1006488.5, 987878.5, 970011.5, 
                     944091, 910240, 879933.5, 852777, 822994.5, 791082.5, 762174, 
                     738827, 716944.5, 692931, 667299.5, 640557.5, 615269, 593093, 
                     571228, 549423, 529054, 508564, 487777.5, 467564.5, 447287.5, 
                     427610, 409270.5, 391182.5, 373648.5, 357264.5, 341236, 325802.5, 
                     312334.5, 300206, 288271.5, 277962.5, 268264, 258116.5, 248831.5, 
                     239197.5, 228553.5, 218810, 210665.5, 202746.5, 194224.5, 185461.5, 
                     177106.5, 169036, 161107, 153494.5),
      ENADID1997 = c(1138903.5, 
                     1132884.5, 1136491.5, 1134480, 1132281.5, 1133355.5, 1131365.5, 
                     1116887, 1097068.5, 1084132.5, 1078388.5, 1071487.5, 1061917, 
                     1051129.5, 1045577.5, 1040827, 1026702, 1011688.5, 1000133.5, 
                     991015.5, 981132.5, 967527.5, 952712.5, 937265.5, 918603, 894430.5, 
                     866645.5, 839976, 816138, 792063, 767102, 745079.5, 725808.5, 
                     705506, 683782, 661236.5, 637413.5, 613718, 590340.5, 565846.5, 
                     541698, 518484, 495442, 473813.5, 453377.5, 432567.5, 413416, 
                     396168, 378920.5, 363093.5, 347961.5, 332018, 316643, 303264.5, 
                     291015, 279098, 268626, 258961.5, 249410.5, 240168, 230955, 221256.5, 
                     210794, 200713.5, 191958.5, 183175),
      ENADID2006 = c(1115629.5, 
                     1117622.5, 1121133.5, 1125723.5, 1128154.5, 1126723, 1115470.5, 
                     1101637.5, 1096797.5, 1098914, 1109238.5, 1116612.5, 1114301.5, 
                     1110989.5, 1110972.5, 1106930, 1089825.5, 1062991.5, 1044430, 
                     1036113, 1025915.5, 1012271, 996594.5, 985936, 974860, 956093, 
                     941479, 930985.5, 918357, 903735.5, 889720, 880254.5, 871479, 
                     857332.5, 839179.5, 818168.5, 796045.5, 775964.5, 755798, 734458, 
                     715291.5, 696848, 676233.5, 654716.5, 632927, 610149.5, 587213.5, 
                     563636, 538881.5, 514929, 491745, 469015.5, 448358, 428799, 408449.5, 
                     390069, 373418, 356631.5, 341605, 326840, 310835.5, 295381.5, 
                     281755, 269058.5, 256770, 245652),
      ENADID2009 = c(1113569, 1108092, 
                     1106605.5, 1108726, 1112787, 1116879, 1122226, 1125172, 1124088.5, 
                     1112987, 1099040.5, 1093876, 1095599.5, 1105516, 1112532.5, 1109772, 
                     1105869.5, 1105158, 1100338.5, 1082478, 1055016, 1035979.5, 1027281, 
                     1016727.5, 1002823.5, 987136, 976708.5, 966008, 947705.5, 933623, 
                     923765, 911788, 897780, 884307.5, 875225.5, 866653.5, 852544.5, 
                     834304, 813147.5, 790814.5, 770442, 749951.5, 728300.5, 708846, 
                     690183.5, 669426.5, 647822, 625989.5, 603187, 580225.5, 556629, 
                     531848.5, 507807.5, 484479.5, 461561.5, 440659, 420838, 400236.5, 
                     381553, 364539.5, 347360.5, 331861, 316620, 300224, 284435.5, 
                     270481), ENADID2014 = c(1110926, 1119415, 1117977, 1114522.5, 
                                             1110561, 1106662, 1103645.5, 1103198, 1106040.5, 1110564, 1114931.5, 
                                             1120391, 1123279, 1121954.5, 1110422, 1095836.5, 1089835.5, 1090564, 
                                             1099392, 1105343.5, 1101651, 1096989, 1095713, 1090543.5, 1072548, 
                                             1045107.5, 1026152, 1017571.5, 1007194, 993513.5, 978073.5, 967881.5, 
                                             957421, 939375.5, 925503, 915795, 903924.5, 889975.5, 876502.5, 
                                             867344.5, 858652.5, 844422, 826051, 804744.5, 782222.5, 761580.5, 
                                             740751.5, 718702.5, 698751.5, 679515.5, 658158.5, 635927.5, 613447.5, 
                                             590003.5, 566391.5, 542158.5, 516779.5, 492137, 468211, 444706.5, 
                                             423167.5, 402674.5, 381439.5, 362029.5, 344199.5, 326220.5), 
      EDER2017 = c(1039421, 1055587.5, 1079260, 1103715, 1114428.5, 
                   1114036, 1111350, 1107950.5, 1104458, 1101719.5, 1101440, 
                   1104331, 1108743, 1112768.5, 1117585, 1119504, 1116911, 1103905, 
                   1087762.5, 1080240, 1079598.5, 1087287.5, 1092393.5, 1088162.5, 
                   1083233.5, 1081914.5, 1076899, 1059229, 1032240.5, 1013794, 
                   1005737.5, 995903.5, 982766, 967848, 958128.5, 948092.5, 
                   930440, 916890.5, 907434.5, 895764.5, 881970, 868599, 859474, 
                   850764, 836500.5, 818079.5, 796699, 774064.5, 753243.5, 732178.5, 
                   709844, 689530.5, 669871, 648074, 625388.5, 602443, 578542, 
                   554475, 529803.5, 504022.5, 478981.5, 454661.5, 430784, 408843.5, 
                   387941.5, 366350),
      ENADID2018 = c(1030246.5, 1036537, 1053946.5, 
                     1077956, 1102666, 1113575, 1113329.5, 1110747.5, 1107418.5, 
                     1103967.5, 1101243, 1100939, 1103744.5, 1107992, 1111765.5, 
                     1116248.5, 1117777, 1114772.5, 1101375, 1084886.5, 1077078.5, 
                     1076222, 1083763, 1088788, 1084539.5, 1079639, 1078381.5, 
                     1073457, 1055906.5, 1029058, 1010748, 1002820, 993107.5, 
                     980083, 965266.5, 955625.5, 945650, 928048.5, 914528.5, 905078.5, 
                     893400.5, 879587, 866182, 857002.5, 848223, 833888.5, 815392.5, 
                     793930.5, 771203.5, 750267, 729070, 706593, 686115, 666274.5, 
                     644298, 621429, 598295.5, 574206.5, 549952, 525099.5, 499156.5, 
                     473963.5, 449511, 425522.5, 403475, 382470.5),
      ENADID2023 = c(996832, 
                     1006432.5, 1013919.5, 1020875, 1024776.5, 1023785, 1032014.5, 
                     1050264, 1074886, 1100038, 1111262, 1111205.5, 1108665, 1105206.5, 
                     1101417, 1098108.5, 1096950.5, 1098648, 1101594, 1103968, 
                     1107060, 1107311.5, 1103223.5, 1088996, 1071915.5, 1063695.5, 
                     1062581.5, 1069981, 1074995.5, 1070877.5, 1066193.5, 1065194, 
                     1060575, 1043390, 1016923.5, 998892, 991109.5, 981472, 968470.5, 
                     953630, 943871.5, 933725, 915951, 902128.5, 892210, 879940, 
                     865419, 851179.5, 841016, 831204, 815939, 796622.5, 774414.5, 
                     750928.5, 729078.5, 706805, 683098, 661144, 639665.5, 616065.5, 
                     591633, 567032.5, 541654.5, 516263.5, 490451.5, 463755), 
      EDER2025 = c(978858, 984557, 992715.5, 1003573.5, 1011509, 
                   1018808, 1022976, 1022187, 1030561.5, 1048910, 1073595.5, 
                   1098782, 1110013, 1109923.5, 1107278.5, 1103590, 1099423, 
                   1095637, 1093996, 1095288, 1097930, 1100060.5, 1102902, 1102884, 
                   1098547.5, 1084151, 1066999, 1058787.5, 1057743.5, 1065249, 
                   1070399.5, 1066439.5, 1061908.5, 1061036.5, 1056523.5, 1039436.5, 
                   1013055, 995063.5, 987266, 977588.5, 964527.5, 949605, 939719, 
                   929414.5, 911477.5, 897447.5, 887273, 874729.5, 859922.5, 
                   845375, 834855.5, 824655.5, 809005.5, 789295.5, 766678.5, 
                   742754, 720408.5, 697611, 673379, 650871, 628837.5, 604738, 
                   579851, 554836, 529096, 503369.5)),
      row.names = c(NA, -66L), class = "data.frame")
    )
}
pobMex <- PobEdadMex()
pobMex_l <- pobMex %>%
  as_tibble() %>%
  pivot_longer(cols = -Age, names_to = "survey", values_to = "Freq")
pobMex_l <- pobMex_l[order(pobMex_l$survey),]
pobMex_l <- subset (pobMex_l, Age >= 14)
pobMEX_ENADID <-  as.data.frame(xtabs(indiv_weight ~ ageSurvey + survey, data = MEXICO_ENADID))
pobMEX_ENADID <- pobMEX_ENADID[order(pobMEX_ENADID$survey),]

reweight <- pobMEX_ENADID
reweight[,3] <- pobMex_l[,3] / pobMEX_ENADID[,3]
reweight[,3] <- ifelse(is.infinite(reweight[,3]),0,reweight[,3])
reweight$ageSurvey <- as.numeric(as.character(reweight$ageSurvey))

MEXICO_ENADID <- MEXICO_ENADID %>%
  # Step 1: Join the multiplier to the individual data
  left_join(reweight, by = c("ageSurvey", "survey")) %>%

  # Step 2: Perform the vectorized multiplication
  mutate(reweight = indiv_weight * Freq) %>%

  # Step 3: Optional - remove the multiplier column to keep it clean
  select(-Freq)

path_MEXICO_ENADID <- paste0(rootPath, "/INEGI/Encuestas/ENADID/MEXICO_ENADID.Rdat")
save(MEXICO_ENADID, file = path_MEXICO_ENADID)

rm(WFS_ENADID1977_full)
rm(ENADID1992_full)
rm(ENADID1997_full)
rm(ENADID2006_full)
rm(ENADID2009_full)
rm(ENADID2014_full)
rm(EDER_ENADID)
rm(ENADID2018_full)
rm(ENADID2023_full)
rm(EDER_ENADID25)
