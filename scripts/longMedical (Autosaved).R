### This script creates demographic tables for each of the domains in the medical
### screener
###
### Bukola Ajanaku
### March 1, 2021
### Ignore this line. This line's code was used to visualize the graphs/data through overwriting testing.csv: testing.csv <- write.csv(date_df, "~/Documents/PennBBL/testing.csv")
# TO-DO: Give full path to csv, never setwd


# Load packages
library(gtsummary)
library(dbplyr)
library(dplyr)
library(reshape2)

# Load data
# setwd("~/Box Sync/medical_pnc")
demo_df <- read.csv(file = "~/Box Sync/medical_pnc/n9498_demo_sex_race_ethnicity_dob.csv")
screen_df <- read.csv(file = "~/Box Sync/medical_pnc/Longitudinal Medical/fullscreen_medhistory_oracle_n9498.csv")
date_df <- read.csv(file= "~/Box Sync/medical_pnc/pnc_longitudinal_diagnosis_n749_20210112.csv")

# Recalculate ntimepoints (a lot of erroneous zeros)
for (bblid in unique(date_df$bblid)) {
  date_df[date_df$bblid == bblid, 'ntimepoints'] <- length(date_df[date_df$bblid == bblid, 'ntimepoints'])
}

# Select only the first and last timepoints in date_df (think about how to use 'ntimepoints'!
#----- odd way to do it:
# date_df$timepoint <- as.factor(date_df$timepoint)
# date_df <- date_df %>%
#    group_by(bblid) %>%
#    filter(timepoint == min(timepoint) | timepoint == max(timepoint))
#-----
# Notes:
# doesn't work, matches to first column's valuedate_df3 <- date_df[grepl(date_df$ntimepoints, date_df$timepoint),]
# creates boolean string: date_df2 <- mapply(grepl, date_df$ntimepoints, date_df$timepoint)

date_df <- date_df[date_df$timepoint == "t1" | mapply(grepl, date_df$ntimepoints, date_df$timepoint),]

# recode() 'psy' as 'PS' in date_df$diagnosis
date_df[date_df$diagnosis == "psy", "diagnosis"] <- "PS"
# date_df$diagnosis <- recode(date_df$diagnosis, psy = "PS")


# reshape2::dcast() date_df so that the two values of 'timepoint' each get their own column
# SIMPLER WAY TO DO IT? Without typing it out, date_df would lose all previous columns.
date_df <- dcast(date_df, bblid + timepoint + ntimepoints + dodiagnosis + diagnosis + goassessDxpmr7 + CONSENSUS_TYPE + DOCONSENSUS + CONSENSUSBY + INTERVIEWER + PROTOCOL + VISITNUM + TYPE + INTERVIEWEE + HSTATUS + ASSESS_TYPE + PREV_DX_CHANGED + CONFIDENCE + FROM_CAPA + FROM_DIGS + FROM_SCID + FROM_FIGS + FROM_RECORDS + FROM_MINI + FROM_SIPS + DIAGNOSIS_DEFERRED + DEFERRED_REASON + AXIS1_DX1 + AXIS1_DESC1 + AXIS1_DX2 + AXIS1_DESC2 + AXIS1_DX3 + AXIS1_DESC3 + AXIS1_DX4 + AXIS1_DESC4 + AXIS1_DX5 + AXIS1_DESC5 + AXIS1_DX6 + AXIS1_DESC6 + AXIS1_DX7 + AXIS1_DESC7 + AXIS1_DX8 + AXIS1_DESC8 + AXIS1_DX9 + AXIS1_DESC9 + AXIS1_DX10 + AXIS1_DESC10 + AXIS2_DX1 + AXIS2_DESC1 + AXIS2_DX2 + AXIS2_DESC2 + AXIS2_DX3 + AXIS2_DESC3 + AXIS2_DX4 + AXIS2_DESC4 + AXIS3_DX1 + AXIS3_DESC1 + AXIS3_DX2 + AXIS3_DESC2 + AXIS3_DX3 + AXIS3_DESC3 + AXIS3_DX4 +  AXIS3_DESC4 + AXIS3_DX5 + AXIS3_DESC5 + AXIS3_DX6 + AXIS3_DESC6 + AXIS3_DX7 + AXIS3_DESC7 + AXIS3_DX8 + AXIS3_DESC8 + AXIS3_DX9 + AXIS3_DESC9 + AXIS3_DX10 + AXIS3_DESC10 + GASR_CURRENT + GAS_TYPE + DEFICIT + AXIS1_UK + AXIS1_LIFETIME + CLINICRISK_UK + CLINICRISK_LIFETIME + ENTBY + DOENT + DXSOURCE_PROJ + DXSOURCE_ID + AGEONSET_AXIS1 + AGEONSET_CLINICRISK + AXIS1_STAT1 + AXIS1_STAT2 + AXIS1_STAT3 + AXIS1_STAT4 + AXIS1_STAT5 + AXIS1_STAT6 + AXIS1_STAT7 + AXIS1_STAT8 + AXIS1_STAT9 + AXIS1_STAT10 + FROM_GOASSESS_AD + FROM_PRONIA + dx_none + dx_prodromal + dx_prodromal_remit + dx_psychosis + dx_scz + dx_moodnos + dx_mdd + dx_bp1 + dx_bpoth + dx_adhd + dx_anx + dx_ptsd + dx_cogdis + dx_other + dx_BrderPD + dx_sub_dep_can + dx_sub_dep_alc + dx_sub_dep_oth + dx_sub_abuse_can + dx_sub_abuse_alc + dx_sub_abuse_oth + dx_sub_dep + dx_sub_abuse + dxsum + dx_pscat ~ timepoint)


# Convert all date columns to be valid (as.Date())
#--- error
#date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)] <- as.factor(date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)])
#
#date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)] <- as.POSIXct(strptime(date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)], format = "%m/%d/%y"))
#
#date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)] <- as.Date(date_df2[,grepl("do", colnames(date_df2), ignore.case=TRUE)], format = "%m/%d/%y")
#---- error
#lapply(date_df2[,grep("do", colnames(date_df2), ignore.case=TRUE)], as.factor) 
#lapply(date_df2[,grep("do", colnames(date_df2), ignore.case=TRUE)], as.POSIXct, format = "%m/%d/%y") 
#lapply(date_df2[,grep("do", colnames(date_df2), ignore.case=TRUE)], as.numeric, format = "%m/%d/%y") 
#
#lapply(date_df2[,grep("do", colnames(date_df2), ignore.case=TRUE)], as.Date, origin="1970-01-01")
#
#as.Date(grep("do", colnames(date_df2), ignore.case=TRUE), origin="1970-01-01")
date_df$dodiagnosis <- as.Date(date_df$dodiagnosis, format = "%m/%d/%y")
date_df$DOCONSENSUS <- as.Date(date_df$DOCONSENSUS, format = "%m/%d/%y")
date_df$DOENT <- as.Date(date_df$DOENT, format = "%m/%d/%y")


# merge() date_df and demo_df so that you only get the bblids in date_df that are in demo_df
date_df <- merge(demo_df, date_df, by.x = "bblid")
date_df$dob <- as.Date(date_df$dob, format = "%m/%d/%y")

# Calculate age for each assessment
date_df$age <- floor((as.numeric(date_df$dodiagnosis - date_df$dob))/365.25)


################################################################################
# Write a function that returns the age at first and last diagnosis as an atomic vector of length two

ageFristLast <- function(bblid){
	forBblid <- subset(date_df, date_df$bblid == bblid) 
	forBblid <- forBblid$age
	aget1 <- forBblid[1]
	agetf <- forBblid[2]
	
	ages <- c(aget1, agetf)
	
	return(ages)
}



# Write a function that returns, based on the age at assessment that you will calculate,
# the assessment number (e.g., if a person comes in at ages 12, 15, and 17, they
# would be assigned assessment numbers 1, 2, and 3, respectively)



################################################################################

# sapply() your ageFirstLast() function


# Recode variables
final_df$Sex <- recode(final_df$sex, `1`='Male', `2`='Female')
final_df$Race <- recode(final_df$race, `1`='Caucasian', `2`='African American',
  `3`='US India/Alaska Native', `4`='Asian', `5`='More Than One Race')
final_df$Diagnosis <- recode(final_df$t1_tfinal, 'other_TD'='OP-TD',
  'other_other'='OP-OP', 'other_PS'='OP-PS', 'TD_other'='TD-OP', 'PS_other'='PS-OP',
  'TD_TD'='TD-TD', 'TD_PS'='TD-PS', 'PS_TD'='PS-TD', 'PS_PS'='PS-PS')
final_df$Diagnosis <- ordered(final_df$Diagnosis, c('TD-TD', 'TD-OP', 'TD-PS',
  'OP-TD', 'OP-OP', 'OP-PS', 'PS-TD', 'PS-OP', 'PS-PS'))







# ..........



# merge date_df and screen_df so that you only get the bblids in date_df that are
# in screen_df
