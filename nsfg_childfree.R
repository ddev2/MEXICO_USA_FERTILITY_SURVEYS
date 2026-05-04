#' Read and recode National Survey of Family Growth (NSFG) data
#'
#' @param years vector: a numeric vector containing the starting year of NSFG waves to include (2002, 2006, 2011, 2013, 2015, 2017, 2022)
#' @param nonbio boolean: should non-biological children be included
#' @param keep_source boolean: keep the raw variables used to construct \code{want_cf} and \code{famstat}
#' @param progress boolean: display a progress bar
#'
#' @details
#' The U.S. Centers for Disease Control \href{https://www.cdc.gov/nchs/nsfg/index.htm}{National Survey of Family Growth} (NSFG)
#'    regularly collects fertility and other health information from a population-representative sample of adults in the
#'    United States. Between 1973 and 2002, the NSFG was conducted periodically. Starting in 2006, the NSFG transitioned to
#'    continuous data collection, releasing data in multi-year waves (e.g., 2006-2010, 2011-2013). The `nsfg()` function reads
#'    the raw data from CDC's website, extracts and recodes selected variables useful for studying childfree adults and other family
#'    statuses, then returns an unweighted data frame.
#'
#' **Sampling weights**
#'
#' The NSFG is collected using a complex survey design. The \code{survey} package can be used to perform analyses that take these
#'    design features into account, and make it possible to obtain population-representative estimates. In most cases, a \link[survey]{svydesign}
#'    object for a single wave can be created using \code{survey::svydesign(data = data, ids = ~cluster, strata = ~strata, weights = ~weight, nest = TRUE)}.
#'    Additional information about analyzing DHS data using weights is available \href{https://www.cdc.gov/nchs/nsfg/index.htm}{here}.
#'
#' **Non-biological children**
#'
#' When \code{nonbio == TRUE} (default), non-biological children (e.g., adopted children, foster children, etc.) are treated the same as
#'    biological children when determining a respondent's family status. This matches the approach described by the ABC Framework
#'    (Neal & Neal, 2024), and should generally be used.However, non-biological children can be ignored by setting \code{nonbio = FALSE},
#'    which may be useful when comparing NSFG estimates to estimates derived from other data where information about non-biological children
#'    is not available.
#'
#' **Additional notes**
#'   * Starting in 2006, "hispanic" was a response option for race, however "hispanic" is not a racial category, but an ethnicity.
#'     When a respondent chose this option, their actual race is unknown.
#'   * Partnership status only describes a respondent's status with respect to an opposite-sex partner. Information about current
#'     or former same-sex partnerships is not available.
#'   * The NSFG manual explains that "sample sizes for a single year are too small to provide estimates with adequate levels of precision,"
#'     and therefore recommends avoiding analysis of data from single years. Instead, these data are designed to be analyzed by wave.
#'
#' @return A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
#'
#' @references NSFG Classification: {Neal, J. W. and Neal, Z. P. (2025). Tracking types of non-parents in the United States. *Journal of Marriage and Family*. \doi{10.1111/jomf.13097}}
#' @references ABC Framework: {Neal, Z. P. and Neal, J. W. (2024). A framework for studying adults who neither have nor want children. *The Family Journal, 32*, 121-130. \doi{10.1177/10664807231198869}}
#' @export
#'
#' @examples
#' \donttest{
#' unweighted <- nsfg(years = 2017)  #Unweighted data
#' table(unweighted$famstat) / nrow(unweighted)  #Fraction of respondents with each family status
#' }
nsfg <- function(years, nonbio = TRUE, keep_source = FALSE, progress = TRUE) {

  if (!all(years %in%c(2002, 2006, 2011, 2013, 2015, 2017, 2022))) {stop("Only the following NSFG years are available: 2002, 2006, 2011, 2013, 2015, 2017, 2022")}  #Check for valid years
  years <- sort(years)  #Put years in order

  if (progress) {message("Processing NSFG data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(years) * 2, initial = 0, style = 3)} #Initialize progress bar
  year.num <- 1

  #### FEMALE RESPONDENT LOOP ####
  for (year in years) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,year.num)}

    #Import raw data
    if (year==2002) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2002FemResp.dat")}
    if (year==2006) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2006_2010_FemResp.dat")}
    if (year==2011) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_FemRespData.dat")}
    if (year==2013) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_FemRespData.dat")}
    if (year==2015) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_FemRespData.dat")}
    if (year==2017) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat")}
    if (year==2022) {
      temp <- tempfile()
      nullcon <- file(nullfile(), open = "wb")
      sink(nullcon, type = "message")
      utils::download.file("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/NSFG/NSFG-2022-2023-FemRespPUFData.zip",temp)  #Download file
      sink(type = "message")
      close(nullcon)
      raw <- utils::read.csv(unz(temp, "NSFG_2022_2023_FemRespPUFData.csv"), header = TRUE)
      rm(temp)
      }

    #Initialize dataframe with id variable
    if (year==2002) {dat <- data.frame(id = as.character(substring(raw,1,12)))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat <- data.frame(id = as.character(substring(raw,1,5)))}
    if (year==2022) {dat <- data.frame(id = raw$CaseID)}

    #### Family Status ####
    #Source variables
    if (year==2002) {
      dat$parity <- as.numeric(substring(raw,3790,3791)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,103,103)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,307,307))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3512,3512)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1463,1463)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1464,1464)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,4786,4786)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2006) {
      dat$parity <- as.numeric(substring(raw,4915,4916)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,143,143)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,697,697))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,4539,4539)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1902,1902)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1903,1903)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,6078,6078)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2011) {
      dat$parity <- as.numeric(substring(raw,3810,3811)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,148,148)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,624,624))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3282,3282)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1754,1754)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1755,1755)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,4852,4852)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2013) {
      dat$parity <- as.numeric(substring(raw,3767,3768)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,143,143)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,552,552))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3420,3420)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1683,1683)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1684,1684)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,4978,4978)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
      }
    if (year==2015) {
      dat$parity <- as.numeric(substring(raw,3286,3287)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,121,121)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,401,401))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2785,2785)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1237,1237)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1238,1238)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,4416,4416)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
      }
    if (year==2017) {
      dat$parity <- as.numeric(substring(raw,2879,2880)) #Number of live births
      dat$otherkid <- as.numeric(substring(raw,102,102)) #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- as.numeric(substring(raw,214,214))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2410,2410)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,836,836)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,837,837)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- as.numeric(substring(raw,3734,3734)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2022) {
      dat$parity <- raw$PARITY #Number of live births
      dat$otherkid <- raw$OTHERKID #Any non-biological children: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$otachil <- NA
      dat$seekadpt <- raw$SEEKADPT  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- raw$RWANT #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- raw$RSTRSTAT #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- raw$PSTRSTAT #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$anykids <- NA
      dat$intent <- raw$INTENT #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }

    dat$seekadpt[which(is.na(dat$seekadpt))] <- 5  #Females under 18 not asked; impute no

    #If requested, exclude non-biological children
    if (nonbio == FALSE) {
      dat$otherkid <- 5
      dat$otachil <- 5
      dat$seekadpt <- 5
    }

    #Age in years (using AGER)
    if (year==2002) {dat$age <- as.numeric(substring(raw,3749,3750))}
    if (year==2006) {dat$age <- as.numeric(substring(raw,4853,4854))}
    if (year==2011) {dat$age <- as.numeric(substring(raw,3751,3752))}
    if (year==2013) {dat$age <- as.numeric(substring(raw,3707,3708))}
    if (year==2015) {dat$age <- as.numeric(substring(raw,3227,3228))}
    if (year==2017) {dat$age <- as.numeric(substring(raw,2822,2823))}
    if (year==2022) {dat$age <- raw$AGER}

    #Constructed variables
    dat$behavior <- NA
    dat$behavior[which(dat$age>=18 & (dat$parity>0 | dat$otherkid==1))] <- 1  #Age 18+, has biological or non-biological children
    dat$behavior[which(dat$age>=18 & (dat$parity==0 & dat$otherkid==5))] <- 0  #Age 18+, does not have biological or non-biological children
    dat$behavior[which(dat$age<18 & dat$parity>0)] <- 1  #Age 18+, has biological children
    dat$behavior[which(dat$age<18 & dat$parity==0)] <- 0  #Age 18+, does not have biological children

    dat$attitude <- NA
    dat$attitude[which(dat$rwant==5 & dat$seekadpt==5)] <- 0  #No, do not want biological or adopted children
    dat$attitude[which(dat$rwant==1 | dat$seekadpt==1)] <- 1  #Yes, want biological or adopted children
    dat$attitude[which((dat$rwant==9 | dat$seekadpt==9) & dat$rwant!=1 & dat$seekadpt!=1)] <- -1  #DK if want biological or adopted children

    dat$fecund <- NA
    dat$fecund[which(dat$rstrstat==1 | dat$rstrstat==2 | dat$pstrstat==1 | dat$pstrstat==2)] <- 0  #Self (or partner, if present) is sterile
    dat$fecund[which(dat$rstrstat==0 & dat$pstrstat==0)] <- 1 #Self and partner are not sterile
    dat$fecund[which(dat$rstrstat==0 & is.na(dat$pstrstat))] <- 1 #Self not sterile, no partner

    dat$circumstance <- 0  #No known barriers
    dat$circumstance[which(dat$fecund==0)] <- 1  #Infecund
    dat$circumstance[which(dat$fecund==1 & dat$intent==2)] <- 2  #Other barrier (fecund, but do not intend to have children)

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$behavior==0 & dat$attitude==0)] <- 1  #Childfree
    dat$cf_want[which(dat$behavior!=0 | dat$attitude!=0)] <- 0  #Not childfree

    #Childfree (expect) - Unknown because intention question only asked of single respondents if they wanted children

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$behavior==1)] <- 1  #Parent - Unclassified
    #Parent - Fulfilled: Unknown because parents who do not want another child could also be reluctant
    dat$famstat[which(dat$behavior==1 & dat$attitude==1)] <- 3  #Parent - Unfulfilled
    #Parent - Reluctant: Unknown because parents who do not want another child could also be fulfilled
    dat$famstat[which(dat$behavior==1 & dat$attitude==-1)] <- 5  #Parent - Ambivalent
    dat$famstat[which(dat$behavior==0 & dat$attitude==1)] <- 6  #Not yet parent
    #Childless - Unclassified: Not used because all can be classified
    dat$famstat[which(dat$behavior==0 & dat$attitude==1 & dat$circumstance==2 & dat$seekadpt==5)] <- 8  #Socially childless
    dat$famstat[which(dat$behavior==0 & dat$attitude==1 & dat$circumstance==1 & dat$seekadpt==5)] <- 9  #Biologically childless
    dat$famstat[which(dat$behavior==0 & dat$attitude==-1 & dat$circumstance!=0)] <- 10  #Ambivalent
    dat$famstat[which(dat$behavior==0 & dat$attitude==-1 & dat$circumstance==0)] <- 11  #Undecided
    dat$famstat[which(dat$behavior==0 & dat$attitude==0)] <- 12  #Childfree
    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex (based on source data file)
    dat$sex <- 1
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Sexual orientation (using ORIENT)
    dat$lgbt <- NA
    if (year==2002 | year==2006) {dat$orient <- NA}
    if (year==2011) {dat$orient <- as.numeric(substring(raw,3704,3704))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2013) {dat$orient <- as.numeric(substring(raw,3654,3654))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2015) {
      dat$a <- as.numeric(substring(raw,3184,3184))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3185,3185))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2017) {
      dat$a <- as.numeric(substring(raw,2779,2779))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,2780,2780))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2022) {
      dat$orient <- raw$ORIENT  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$orient[which(raw$ORIENT==1)] <- 2  #Recode gay as 2
      dat$orient[which(raw$ORIENT==2)] <- 1  #Recode straight as 1
    }
    dat$lgbt <- factor(dat$orient, levels = c(1,2,3,4), labels = c("Straight", "Gay/Lesbian", "Bisexual", "Something else"))

    #Race (using RSCRRACE)
    if (year==2002) {
      dat$race <- as.numeric(substring(raw,17,17))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,99,98), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2006 | year==2011 | year==2013) {  #Coding assumes that 1/2/3 mean the same here as in 2002, despite not being labeled in the provided Stata .do code
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,96,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2015 | year==2017) {
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2022) {
      dat$race <- raw$RSCRRACE
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }

    #Hispanic (using HISPANIC)
    if (year==2002) {dat$hispanic <- as.numeric(substring(raw,3756,3756))}  #1 hispanic, 2 non-hispanic
    if (year==2006) {dat$hispanic <- as.numeric(substring(raw,4861,4861))}  #1 hispanic, 2 non-hispanic
    if (year==2011) {dat$hispanic <- as.numeric(substring(raw,3759,3759))}  #1 hispanic, 2 non-hispanic
    if (year==2013) {dat$hispanic <- as.numeric(substring(raw,3715,3715))}  #1 hispanic, 2 non-hispanic
    if (year==2015) {dat$hispanic <- as.numeric(substring(raw,3235,3235))}  #1 hispanic, 2 non-hispanic
    if (year==2017) {dat$hispanic <- as.numeric(substring(raw,2830,2830))}  #1 hispanic, 2 non-hispanic
    if (year==2022) {dat$hispanic <- raw$HISPANIC}  #1 hispanic, 2 non-hispanic
    dat$hispanic[which(dat$hispanic==2)] <- 0  #Recode not-hispanic as 0

    #Age in years - Computed in family status section

    #Education (using HIEDUC)
    if (year==2002) {educ <- as.numeric(substring(raw,3754,3755))}
    if (year==2006) {educ <- as.numeric(substring(raw,4859,4860))}
    if (year==2011) {educ <- as.numeric(substring(raw,3757,3758))}
    if (year==2013) {educ <- as.numeric(substring(raw,3713,3714))}
    if (year==2015) {educ <- as.numeric(substring(raw,3233,3234))}
    if (year==2017) {educ <- as.numeric(substring(raw,2828,2829))}
    if (year==2002 | year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {
      dat$education <- NA
      dat$education[which(educ<=8)] <- 1  #No high school
      dat$education[which(educ==9)] <- 2  #High school or GED
      dat$education[which(educ==10)] <- 3  #Some college
      dat$education[which(educ==11)] <- 4  #Associate
      dat$education[which(educ==12)] <- 5  #Bachelor
      dat$education[which(educ==13)] <- 6  #Master
      dat$education[which(educ==14)] <- 7  #Doctor
      dat$education[which(educ==15)] <- 8  #Professional
    }
    if (year==2022) {
      educ <- raw$HIEDUC
      dat$education <- NA
      dat$education[which(educ<=2)] <- 1  #No high school
      dat$education[which(educ==3)] <- 2  #GED
      dat$education[which(educ==4)] <- 2  #High school
      dat$education[which(educ==5)] <- 3  #Some college
      dat$education[which(educ==6)] <- 4  #Associate
      dat$education[which(educ==7)] <- 4  #Associate
      dat$education[which(educ==8)] <- 5  #Bachelor
      dat$education[which(educ==9)] <- 6  #Master
      dat$education[which(educ==11)] <- 7  #Doctor
      dat$education[which(educ==10)] <- 8  #Professional
    }
    dat$education <- factor(dat$education,
                            levels = c(1:8),
                            labels = c("Did not finish high school",
                                       "High school graduate",
                                       "Some college",
                                       "Associates Degree",
                                       "Bachelor's Degree",
                                       "Master's Degree",
                                       "Doctorate Degree",
                                       "Professional Degree"),
                            ordered = TRUE)

    #Partnership status (using RMARITAL)
    if (year==2002) {partner <- as.numeric(substring(raw,4349,4349))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2006) {partner <- as.numeric(substring(raw,4856,4856))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2011) {partner <- as.numeric(substring(raw,3754,3754))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2013) {partner <- as.numeric(substring(raw,3710,3710))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2015) {partner <- as.numeric(substring(raw,3230,3230))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2017) {partner <- as.numeric(substring(raw,2825,2825))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2022) {partner <- raw$RMARITAL}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    dat$partnered <- NA
    dat$partnered[which(partner==6)] <- 1  #Single, never married
    dat$partnered[which(partner==1 | partner==2)] <- 2  #Currently partnered
    dat$partnered[which(partner==3 | partner==4 | partner==5)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence (using METRO)
    if (year==2002) {dat$metro <- as.numeric(substring(raw,4821,4821))}
    if (year==2006) {dat$metro <- as.numeric(substring(raw,6116,6116))}
    if (year==2011) {dat$metro <- as.numeric(substring(raw,4890,4890))}
    if (year==2013) {dat$metro <- as.numeric(substring(raw,5016,5016))}
    if (year==2015) {dat$metro <- as.numeric(substring(raw,4454,4454))}
    if (year==2017) {dat$metro <- as.numeric(substring(raw,3772,3772))}
    if (year==2022) {dat$metro <- raw$METRO}
    dat$residence <- NA
    dat$residence[which(dat$metro==1)] <- 4  #Principal city of MSA = Urban
    dat$residence[which(dat$metro==2)] <- 3  #Other part of MSA = Suburb
    dat$residence[which(dat$metro==3)] <- 1  #Not in MSA = Rural
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed (using RWRKST)
    if (year==2002) {dat$rwrkst <- as.numeric(substring(raw,3674,3674))}
    if (year==2006) {dat$rwrkst <- as.numeric(substring(raw,4758,4758))}
    if (year==2011) {dat$rwrkst <- as.numeric(substring(raw,3511,3511))}
    if (year==2013) {dat$rwrkst <- as.numeric(substring(raw,3461,3461))}
    if (year==2015) {dat$rwrkst <- as.numeric(substring(raw,3008,3008))}
    if (year==2017) {dat$rwrkst <- as.numeric(substring(raw,2663,2663))}
    if (year==2022) {dat$rwrkst <- raw$RWRKST}
    dat$employed <- NA
    dat$employed[which(dat$rwrkst==1)] <- 1  #Employed
    dat$employed[which(dat$rwrkst==5)] <- 0  #Not employed

    #In school (using GOSCHOL)
    if (year==2006) {dat$goschol <- as.numeric(substring(raw,37,37))}
    if (year==2002 | year==2011) {dat$goschol <- as.numeric(substring(raw,41,41))}
    if (year==2013) {dat$goschol <- as.numeric(substring(raw,38,38))}
    if (year==2015 | year==2017) {dat$goschol <- as.numeric(substring(raw,34,34))}
    if (year==2022) {dat$goscho <- NA}
    dat$inschool <- NA
    dat$inschool[which(dat$goschol==1)] <- 1  #In school
    dat$inschool[which(dat$goschol==5)] <- 0  #Not in school

    #### Attitude ####
    #Religion (using RELIGION)
    if (year==2002) {rel <- as.numeric(substring(raw,4822,4822))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2006) {rel <- as.numeric(substring(raw,6117,6117))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2011) {rel <- as.numeric(substring(raw,4891,4891))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2013) {rel <- as.numeric(substring(raw,5017,5017))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2015) {rel <- as.numeric(substring(raw,4455,4455))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2017) {rel <- as.numeric(substring(raw,3773,3773))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2022) {rel <- raw$RELIGION}  #1 none, 2 catholic, 3 protestant, 4 other
    dat$religion <- NA
    dat$religion[which(rel==1)] <- 1  #None
    dat$religion[which(rel==2)] <- 2  #Catholic
    dat$religion[which(rel==3)] <- 5  #Protestant
    dat$religion[which(rel==4)] <- 6  #Other
    dat$religion <- factor(dat$religion, levels = c(1:6), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #Bother (If it turns out that you do not have any children, how much would it bother you?)
    if (year==2002) {dat$bother <- as.numeric(substring(raw,3723,3723))}
    if (year==2006) {dat$bother <- as.numeric(substring(raw,4806,4806))}
    if (year==2011) {dat$bother <- as.numeric(substring(raw,3537,3537))}
    if (year==2013) {dat$bother <- as.numeric(substring(raw,3486,3486))}
    if (year==2015) {dat$bother <- as.numeric(substring(raw,3027,3027))}
    if (year==2017) {dat$bother <- as.numeric(substring(raw,2682,2682))}
    if (year==2022) {dat$bother <- raw$CHBOTHER}
    dat$bother <- factor(dat$bother, levels = c(4,3,2,1), labels = c("Not at all", "A little", "Some", "A great deal"), ordered = TRUE)

    #### Design ####
    #Identifier - This step is performed above, when initializing the data frame

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (year==2002) {dat$weight <- as.numeric(substring(raw,4873,4891))}
    if (year==2006) {dat$weight <- as.numeric(substring(raw,6150,6168))}
    if (year==2011) {dat$weight <- as.numeric(substring(raw,4906,4922))}
    if (year==2013) {dat$weight <- as.numeric(substring(raw,5032,5048))}
    if (year==2015) {dat$weight <- as.numeric(substring(raw,4470,4486))}
    if (year==2017) {dat$weight <- as.numeric(substring(raw,3787,3803))}
    if (year==2022) {dat$weight <- raw$WGT2022_2023}

    #Cluster
    if (year==2002) {dat$cluster <- as.numeric(substring(raw,4891,4891))}
    if (year==2006) {dat$cluster <- as.numeric(substring(raw,6222,6222))}
    if (year==2011) {dat$cluster <- as.numeric(substring(raw,4922,4922))}
    if (year==2013) {dat$cluster <- as.numeric(substring(raw,5048,5048))}
    if (year==2015) {dat$cluster <- as.numeric(substring(raw,4486,4486))}
    if (year==2017) {dat$cluster <- as.numeric(substring(raw,3803,3803))}
    if (year==2022) {dat$cluster <- raw$VECL}

    #Stratum
    if (year==2002) {dat$stratum <- as.numeric(substring(raw,4892,4893))}
    if (year==2006) {dat$stratum <- as.numeric(substring(raw,6223,6225))}
    if (year==2011) {dat$stratum <- as.numeric(substring(raw,4923,4923))}
    if (year==2013) {dat$stratum <- as.numeric(substring(raw,5049,5051))}
    if (year==2015) {dat$stratum <- as.numeric(substring(raw,4487,4489))}
    if (year==2017) {dat$stratum <- as.numeric(substring(raw,3804,3806))}
    if (year==2022) {dat$stratum <- raw$VEST}

    #Wave
    if (year==2002) {dat$wave <- "2002"}
    if (year==2006) {dat$wave <- "2006-2010"}
    if (year==2011) {dat$wave <- "2011-2013"}
    if (year==2013) {dat$wave <- "2013-2015"}
    if (year==2015) {dat$wave <- "2015-2017"}
    if (year==2017) {dat$wave <- "2017-2019"}
    if (year==2022) {dat$wave <- "2022-2023"}

    #Year of data collection
    if (year==2002) {dat$cmintvw <- as.numeric(substring(raw,4894,4897))}
    if (year==2006) {dat$cmintvw <- as.numeric(substring(raw,6226,6229))}
    if (year==2011) {dat$cmintvw <- as.numeric(substring(raw,4926,4929))}
    if (year==2013) {dat$cmintvw <- as.numeric(substring(raw,5052,5055))}
    if (year==2015) {dat$cmintvw <- as.numeric(substring(raw,4490,4493))}
    if (year==2017) {dat$cmintvw <- as.numeric(substring(raw,3807,3810))}
    if (year==2022) {dat$cmintvw <- raw$CMINTVW}
    dat$year <- 1900+floor((dat$cmintvw-1)/12)

    #Month of data collection
    dat$month <- dat$cmintvw - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (year==2002) {dat$file <- "2002FemResp.dat"}
    if (year==2006) {dat$file <- "2006_2010_FemRespData.dat"}
    if (year==2011) {dat$file <- "2011_2013_FemRespData.dat"}
    if (year==2013) {dat$file <- "2013_2015_FemRespData.dat"}
    if (year==2015) {dat$file <- "2015_2017_FemRespData.dat"}
    if (year==2017) {dat$file <- "2017_2019_FemRespData.dat"}
    if (year==2022) {dat$file <- "NSFG_2022_2023_FemRespPUFData.csv"}

    #Source survey
    dat$survey <- "NSFG"

    #### Clean up ####
    #Reduce data
    if (keep_source) {
      dat <- dat[,c("cf_want", "famstat", "parity", "otherkid", "seekadpt", "anykids", "otachil", "rwant", "rstrstat", "pstrstat", "intent",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "religion", "bother", #Attitude
                    "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design
    }

    if (!keep_source) {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "religion", "bother", #Attitude
                    "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Start data file, or append to existing data file
    if (year==min(years)) {data <- dat} else {data <- rbind(data, dat)}
    year.num <- year.num + 1
  }

  #### MALE RESPONDENT LOOP ####
  for (year in years) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,year.num)}

    #Import raw data
    if (year==2002) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2002Male.dat")}
    if (year==2006) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2006_2010_Male.dat")}
    if (year==2011) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_MaleData.dat")}
    if (year==2013) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_MaleData.dat")}
    if (year==2015) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_MaleData.dat")}
    if (year==2017) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_MaleData.dat")}
    if (year==2022) {
      temp <- tempfile()
      nullcon <- file(nullfile(), open = "wb")
      sink(nullcon, type = "message")
      utils::download.file("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/NSFG/NSFG-2022-2023-MaleRespPUFData.zip",temp)  #Download file
      sink(type = "message")
      close(nullcon)
      raw <- utils::read.csv(unz(temp, "NSFG_2022_2023_MaleRespPUFData.csv"), header = TRUE)
      rm(temp)
    }

    #Initialize dataframe with id variable
    if (year==2002) {dat <- data.frame(id = as.character(substring(raw,1,12)))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat <- data.frame(id = as.character(substring(raw,1,5)))}
    if (year==2022) {dat <- data.frame(id = raw$CaseID)}

    #### Family Status ####
    #Source variables
    if (year==2002) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,2136,2136)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,2081,2081)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2414,2414)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,114,114)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,220,220)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,2864,2864)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2006) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,3375,3375)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,3315,3315)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3745,3745)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,163,163)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,314,314)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,4401,4401)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2011) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,3370,3370)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,3255,3255)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3750,3750)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,179,179)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,328,328)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,4566,4566)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2013) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,3189,3189)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,3115,3115)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3592,3592)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,180,180)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,327,327)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,4409,4409)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2015) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,2902,2902)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,2802,2802)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3316,3316)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,152,152)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,280,280)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,4099,4099)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2017) {
      dat$parity <- NA
      dat$anykids <- as.numeric(substring(raw,2859,2859)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$otachil <- as.numeric(substring(raw,2740,2740)) #Any non-biological children: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3276,3276)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,167,167)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,290,290)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- as.numeric(substring(raw,4018,4018)) #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }
    if (year==2022) {
      dat$parity <- NA
      dat$anykids <- raw$ANYKIDS #Any biological or adopted children: 1 = Yes, 5 = No
      dat$anykids[which(dat$anykids==5)] <- 0  #Recode for consistency with past waves
      dat$otachil <- 5 #Any non-biological children: NOT AVAILABLE IN THIS WAVE, SO IMPUTED AS "NO"
      dat$rwant <- raw$RWANT #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- raw$RSTRSTAT #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- raw$PSTRSTAT #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$seekadpt <- NA
      dat$otherkid <- NA
      dat$intent <- raw$INTENT #Intent for children: 1 = Yes, 2 = No, 3 = Don't know
    }

    #If requested, exclude non-biological children
    if (nonbio == FALSE) {
      dat$otherkid <- 5
      dat$otachil <- 5
      dat$seekadpt <- 5
    }

    #Age in years (using AGER)
    if (year==2002) {dat$age <- as.numeric(substring(raw,2622,2623))}
    if (year==2006) {dat$age <- as.numeric(substring(raw,4007,4008))}
    if (year==2011) {dat$age <- as.numeric(substring(raw,4227,4228))}
    if (year==2013) {dat$age <- as.numeric(substring(raw,4074,4075))}
    if (year==2015) {dat$age <- as.numeric(substring(raw,3781,3782))}
    if (year==2017) {dat$age <- as.numeric(substring(raw,3692,3693))}
    if (year==2022) {dat$age <- raw$AGER}

    #Constructed variables
    dat$behavior <- NA
    dat$behavior[which(dat$age>=18 & (dat$anykids==1 | dat$otachil==1))] <- 1  #Age 18+, has biological/adopted or other non-biological children
    dat$behavior[which(dat$age>=18 & (dat$anykids==0 & dat$otachil==5))] <- 0  #Age 18+, does not have biological/adopted or other non-biological children
    dat$behavior[which(dat$age<18 & dat$anykids==1)] <- 1  #Age 18+, has biological/adopted children
    dat$behavior[which(dat$age<18 & dat$anykids==0)] <- 0  #Age 18+, does not have biological/adopted children

    dat$attitude <- NA
    dat$attitude[which(dat$rwant==5)] <- 0  #No, do not want biological children
    dat$attitude[which(dat$rwant==1)] <- 1  #Yes, want biological children
    dat$attitude[which(dat$rwant==9)] <- -1  #DK if want biological children

    dat$fecund <- NA
    dat$fecund[which(dat$rstrstat==1 | dat$rstrstat==2 | dat$pstrstat==1 | dat$pstrstat==2)] <- 0  #Self (or partner, if present) is sterile
    dat$fecund[which(dat$rstrstat==0 & dat$pstrstat==0)] <- 1 #Self and partner are not sterile
    dat$fecund[which(dat$rstrstat==0 & is.na(dat$pstrstat))] <- 1 #Self not sterile, no partner

    dat$circumstance <- 0  #No known barriers
    dat$circumstance[which(dat$fecund==0)] <- 1  #Infecund
    dat$circumstance[which(dat$fecund==1 & dat$intent==2)] <- 2  #Other barrier (fecund, but do not intend to have children)

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$behavior==0 & dat$attitude==0)] <- 1  #Childfree
    dat$cf_want[which(dat$behavior!=0 | dat$attitude!=0)] <- 0  #Not childfree

    #Childfree (expect) - Unknown because intention question only asked of single respondents if they wanted children

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$behavior==1)] <- 1  #Parent - Unclassified
    #Parent - Fulfilled: Unknown because parents who do not want another child could also be reluctant
    dat$famstat[which(dat$behavior==1 & dat$attitude==1)] <- 3  #Parent - Unfulfilled
    #Parent - Reluctant: Unknown because parents who do not want another child could also be fulfilled
    dat$famstat[which(dat$behavior==1 & dat$attitude==-1)] <- 5  #Parent - Ambivalent
    dat$famstat[which(dat$behavior==0 & dat$attitude==1)] <- 6  #Not yet parent
    #Childless - Unclassified: Not used because all can be classified
    dat$famstat[which(dat$behavior==0 & dat$attitude==1 & dat$circumstance==2)] <- 8  #Socially childless
    dat$famstat[which(dat$behavior==0 & dat$attitude==1 & dat$circumstance==1)] <- 9  #Biologically childless
    dat$famstat[which(dat$behavior==0 & dat$attitude==-1 & dat$circumstance!=0)] <- 10  #Ambivalent
    dat$famstat[which(dat$behavior==0 & dat$attitude==-1 & dat$circumstance==0)] <- 11  #Undecided
    dat$famstat[which(dat$behavior==0 & dat$attitude==0)] <- 12  #Childfree
    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    dat$sex <- 2
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Sexual orientation
    dat$lgbt <- NA
    if (year==2002 | year==2006) {dat$orient <- NA}
    if (year==2011) {dat$orient <- as.numeric(substring(raw,4175,4175))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2013) {dat$orient <- as.numeric(substring(raw,4020,4020))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2015) {
      dat$a <- as.numeric(substring(raw,3737,3737))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3738,3738))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2017) {
      dat$a <- as.numeric(substring(raw,3648,3648))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3649,3649))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2022) {
      dat$orient <- raw$ORIENT  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$orient[which(raw$ORIENT==1)] <- 2  #Recode gay as 2
      dat$orient[which(raw$ORIENT==2)] <- 1  #Recode straight as 1
    }
    dat$lgbt <- factor(dat$orient, levels = c(1,2,3,4), labels = c("Straight", "Gay/Lesbian", "Bisexual", "Something else"))

    #Race (using RSCRRACE)
    if (year==2002) {
      dat$race <- as.numeric(substring(raw,17,17))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,99,98), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2006 | year==2011 | year==2013) {  #Coding assumes that 1/2/3 mean the same here as in 2002, despite not being labeled in the provided Stata .do code
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,96,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2015 | year==2017) {  #Coding assumes values match female respondents (e.g., 3 = White); the provided Stata .do file seems incorrect
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2022) {
      dat$race <- raw$RSCRRACE
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }

    #Hispanic (using HISPANIC)
    if (year==2002) {dat$hispanic <- as.numeric(substring(raw,2629,2629))}  #1 hispanic, 2 non-hispanic
    if (year==2006) {dat$hispanic <- as.numeric(substring(raw,4015,4015))}  #1 hispanic, 2 non-hispanic
    if (year==2011) {dat$hispanic <- as.numeric(substring(raw,4235,4235))}  #1 hispanic, 2 non-hispanic
    if (year==2013) {dat$hispanic <- as.numeric(substring(raw,4082,4082))}  #1 hispanic, 2 non-hispanic
    if (year==2015) {dat$hispanic <- as.numeric(substring(raw,3789,3789))}  #1 hispanic, 2 non-hispanic
    if (year==2017) {dat$hispanic <- as.numeric(substring(raw,3700,3700))}  #1 hispanic, 2 non-hispanic
    if (year==2022) {dat$hispanic <- raw$HISPANIC}  #1 hispanic, 2 non-hispanic
    dat$hispanic[which(dat$hispanic==2)] <- 0  #Recode not-hispanic as 0

    #Age in years - Computed in family status section

    #Education (using HIEDUC)
    if (year==2002) {educ <- as.numeric(substring(raw,2627,2628))}
    if (year==2006) {educ <- as.numeric(substring(raw,4013,4014))}
    if (year==2011) {educ <- as.numeric(substring(raw,4233,4234))}
    if (year==2013) {educ <- as.numeric(substring(raw,4080,4081))}
    if (year==2015) {educ <- as.numeric(substring(raw,3787,3788))}
    if (year==2017) {educ <- as.numeric(substring(raw,3698,3699))}
    if (year==2002 | year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {
      dat$education <- NA
      dat$education[which(educ<=8)] <- 1  #No high school
      dat$education[which(educ==9)] <- 2  #High school or GED
      dat$education[which(educ==10)] <- 3  #Some college
      dat$education[which(educ==11)] <- 4  #Associate
      dat$education[which(educ==12)] <- 5  #Bachelor
      dat$education[which(educ==13)] <- 6  #Master
      dat$education[which(educ==14)] <- 7  #Doctor
      dat$education[which(educ==15)] <- 8  #Professional
    }
    if (year==2022) {
      educ <- raw$HIEDUC
      dat$education <- NA
      dat$education[which(educ<=2)] <- 1  #No high school
      dat$education[which(educ==3)] <- 2  #GED
      dat$education[which(educ==4)] <- 2  #High school
      dat$education[which(educ==5)] <- 3  #Some college
      dat$education[which(educ==6)] <- 4  #Associate
      dat$education[which(educ==7)] <- 4  #Associate
      dat$education[which(educ==8)] <- 5  #Bachelor
      dat$education[which(educ==9)] <- 6  #Master
      dat$education[which(educ==11)] <- 7  #Doctor
      dat$education[which(educ==10)] <- 8  #Professional
    }
    dat$education <- factor(dat$education,
                            levels = c(1:8),
                            labels = c("Did not finish high school",
                                       "High school graduate",
                                       "Some college",
                                       "Associates Degree",
                                       "Bachelor's Degree",
                                       "Master's Degree",
                                       "Doctorate Degree",
                                       "Professional Degree"),
                            ordered = TRUE)

    #Partnership status (using RMARITAL)
    if (year==2002) {partner <- as.numeric(substring(raw,2641,2641))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2006) {partner <- as.numeric(substring(raw,4010,4010))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2011) {partner <- as.numeric(substring(raw,4230,4230))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2013) {partner <- as.numeric(substring(raw,4077,4077))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2015) {partner <- as.numeric(substring(raw,3784,3784))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2017) {partner <- as.numeric(substring(raw,3695,3695))}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    if (year==2022) {partner <- raw$RMARITAL}  #1 currently, 2 partnered, 3 widowed, 4 divorced, 5 separated, 6 never
    dat$partnered <- NA
    dat$partnered[which(partner==6)] <- 1  #Single, never married
    dat$partnered[which(partner==1 | partner==2)] <- 2  #Currently partnered
    dat$partnered[which(partner==3 | partner==4 | partner==5)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence (using METRO)
    if (year==2002) {dat$metro <- as.numeric(substring(raw,2876,2876))}
    if (year==2006) {dat$metro <- as.numeric(substring(raw,4413,4413))}
    if (year==2011) {dat$metro <- as.numeric(substring(raw,4578,4578))}
    if (year==2013) {dat$metro <- as.numeric(substring(raw,4421,4421))}
    if (year==2015) {dat$metro <- as.numeric(substring(raw,4111,4111))}
    if (year==2017) {dat$metro <- as.numeric(substring(raw,4030,4030))}
    if (year==2022) {dat$metro <- raw$METRO}
    dat$residence <- NA
    dat$residence[which(dat$metro==1)] <- 4  #Principal city of MSA = Urban
    dat$residence[which(dat$metro==2)] <- 3  #Other part of MSA = Suburb
    dat$residence[which(dat$metro==3)] <- 1  #Not in MSA = Rural
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed (using RWRKST)
    if (year==2002) {dat$rwrkst <- as.numeric(substring(raw,2565,2565))}
    if (year==2006) {dat$rwrkst <- as.numeric(substring(raw,3924,3924))}
    if (year==2011) {dat$rwrkst <- as.numeric(substring(raw,3944,3944))}
    if (year==2013) {dat$rwrkst <- as.numeric(substring(raw,3787,3787))}
    if (year==2015) {dat$rwrkst <- as.numeric(substring(raw,3514,3514))}
    if (year==2017) {dat$rwrkst <- as.numeric(substring(raw,3488,3488))}
    if (year==2022) {dat$rwrkst <- raw$RWRKST}
    dat$employed <- NA
    dat$employed[which(dat$rwrkst==1)] <- 1  #Employed
    dat$employed[which(dat$rwrkst==5)] <- 0  #Not employed

    #In school (using GOSCHOL)
    if (year==2002 | year==2011) {dat$goschol <- as.numeric(substring(raw,36,36))}
    if (year==2006) {dat$goschol <- as.numeric(substring(raw,34,34))}
    if (year==2013) {dat$goschol <- as.numeric(substring(raw,33,33))}
    if (year==2015 | year==2017) {dat$goschol <- as.numeric(substring(raw,29,29))}
    if (year==2022) {dat$goschol <- NA}
    dat$inschool <- NA
    dat$inschool[which(dat$goschol==1)] <- 1  #In school
    dat$inschool[which(dat$goschol==5)] <- 0  #Not in school

    #### Attitude ####
    #Religion (using RELIGION)
    if (year==2002) {rel <- as.numeric(substring(raw,2877,2877))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2006) {rel <- as.numeric(substring(raw,4414,4414))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2011) {rel <- as.numeric(substring(raw,4579,4579))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2013) {rel <- as.numeric(substring(raw,4422,4422))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2015) {rel <- as.numeric(substring(raw,4112,4112))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2017) {rel <- as.numeric(substring(raw,4031,4031))}  #1 none, 2 catholic, 3 protestant, 4 other
    if (year==2022) {rel <- raw$RELIGION}  #1 none, 2 catholic, 3 protestant, 4 other
    dat$religion <- NA
    dat$religion[which(rel==1)] <- 1  #None
    dat$religion[which(rel==2)] <- 2  #Catholic
    dat$religion[which(rel==3)] <- 5  #Protestant
    dat$religion[which(rel==4)] <- 6  #Other
    dat$religion <- factor(dat$religion, levels = c(1:6), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #Bother (If it turns out that you do not have any children, how much would it bother you?)
    if (year==2002) {dat$bother <- as.numeric(substring(raw,2596,2596))}
    if (year==2006) {dat$bother <- as.numeric(substring(raw,3956,3956))}
    if (year==2011) {dat$bother <- as.numeric(substring(raw,3969,3969))}
    if (year==2013) {dat$bother <- as.numeric(substring(raw,3813,3813))}
    if (year==2015) {dat$bother <- as.numeric(substring(raw,3533,3533))}
    if (year==2017) {dat$bother <- as.numeric(substring(raw,3507,3507))}
    if (year==2022) {dat$bother <- raw$CHBOTHER}
    dat$bother <- factor(dat$bother, levels = c(4,3,2,1), labels = c("Not at all", "A little", "Some", "A great deal"), ordered = TRUE)

    #### Design ####
    #Identifier - This step is performed above, when initializing the data frame

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (year==2002) {dat$weight <- as.numeric(substring(raw,2927,2944))}
    if (year==2006) {dat$weight <- as.numeric(substring(raw,4446,4463))}
    if (year==2011) {dat$weight <- as.numeric(substring(raw,4593,4608))}
    if (year==2013) {dat$weight <- as.numeric(substring(raw,4436,4451))}
    if (year==2015) {dat$weight <- as.numeric(substring(raw,4126,4141))}
    if (year==2017) {dat$weight <- as.numeric(substring(raw,4044,4059))}
    if (year==2022) {dat$weight <- raw$WGT2022_2023}

    #Cluster
    if (year==2002) {dat$cluster <- as.numeric(substring(raw,2945,2945))}
    if (year==2006) {dat$cluster <- as.numeric(substring(raw,4518,4518))}
    if (year==2011) {dat$cluster <- as.numeric(substring(raw,4609,4609))}
    if (year==2013) {dat$cluster <- as.numeric(substring(raw,4452,4452))}
    if (year==2015) {dat$cluster <- as.numeric(substring(raw,4142,4142))}
    if (year==2017) {dat$cluster <- as.numeric(substring(raw,4060,4060))}
    if (year==2022) {dat$cluster <- raw$VECL}

    #Stratum
    if (year==2002) {dat$stratum <- as.numeric(substring(raw,2946,2947))}
    if (year==2006) {dat$stratum <- as.numeric(substring(raw,4519,4521))}
    if (year==2011) {dat$stratum <- as.numeric(substring(raw,4610,4612))}
    if (year==2013) {dat$stratum <- as.numeric(substring(raw,4453,4455))}
    if (year==2015) {dat$stratum <- as.numeric(substring(raw,4143,4145))}
    if (year==2017) {dat$stratum <- as.numeric(substring(raw,4061,4063))}
    if (year==2022) {dat$stratum <- raw$VEST}

    #Wave
    if (year==2002) {dat$wave <- "2002"}
    if (year==2006) {dat$wave <- "2006-2010"}
    if (year==2011) {dat$wave <- "2011-2013"}
    if (year==2013) {dat$wave <- "2013-2015"}
    if (year==2015) {dat$wave <- "2015-2017"}
    if (year==2017) {dat$wave <- "2017-2019"}
    if (year==2022) {dat$wave <- "2022-2023"}

    #Year of data collection
    if (year==2002) {dat$cmintvw <- as.numeric(substring(raw,2948,2951))}
    if (year==2006) {dat$cmintvw <- as.numeric(substring(raw,4522,4525))}
    if (year==2011) {dat$cmintvw <- as.numeric(substring(raw,4613,4616))}
    if (year==2013) {dat$cmintvw <- as.numeric(substring(raw,4456,4459))}
    if (year==2015) {dat$cmintvw <- as.numeric(substring(raw,4146,4149))}
    if (year==2017) {dat$cmintvw <- as.numeric(substring(raw,4064,4067))}
    if (year==2022) {dat$cmintvw <- raw$CMINTVW}
    dat$year <- 1900+floor((dat$cmintvw-1)/12)

    #Month of data collection
    dat$month <- dat$cmintvw - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (year==2002) {dat$file <- "2002Male.dat"}
    if (year==2006) {dat$file <- "2006_2010_Male.dat"}
    if (year==2011) {dat$file <- "2011_2013_MaleData.dat"}
    if (year==2013) {dat$file <- "2013_2015_MaleData.dat"}
    if (year==2015) {dat$file <- "2015_2017_MaleData.dat"}
    if (year==2017) {dat$file <- "2017_2019_MaleData.dat"}
    if (year==2022) {dat$file <- "NSFG_2022_2023_MaleRespPUFData.csv"}

    #Source survey
    dat$survey <- "NSFG"

    #### Clean up ####
    #Reduce data
    if (keep_source) {
      dat <- dat[,c("cf_want", "famstat", "parity", "otherkid", "seekadpt", "anykids", "otachil", "rwant", "rstrstat", "pstrstat", "intent",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "religion", "bother", #Attitude
                    "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design
    }

    if (!keep_source) {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "religion", "bother", #Attitude
                    "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Append to existing data file from female respondent loop
    data <- rbind(data, dat)
    year.num <- year.num + 1
  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)
}
