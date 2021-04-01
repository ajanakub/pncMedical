### This script creates demographic tables for each of the domains in the medical
### screener
###
### Bukola Ajanaku
### March 1, 2021
### Ignore this line. This line's code was used to visualize the graphs/data through overwriting testing.csv: testing.csv <- write.csv(date_df, "~/Documents/PennBBL/testing.csv")

# Load packages
library(gtsummary)
library(dbplyr)
library(dplyr)
library(reshape2)

# Load data
demo_df <- read.csv(file = "~/Box Sync/medical_pnc/n9498_demo_sex_race_ethnicity_dob.csv")
screen_df <- read.csv(file = "~/Box Sync/medical_pnc/Longitudinal Medical/fullscreen_medhistory_oracle_n9498.csv")
date_df <- read.csv(file= "~/Box Sync/medical_pnc/pnc_longitudinal_diagnosis_n749_20210112.csv")

# Recalculate ntimepoints (a lot of erroneous zeros)
for (bblid in unique(date_df$bblid)) {
  date_df[date_df$bblid == bblid, 'ntimepoints'] <- length(date_df[date_df$bblid
   == bblid, 'ntimepoints'])
}

# Select only the first and last timepoints in date_df (think about how to
# use 'ntimepoints'!
# How Ellyn did it: date_df <- date_df[date_df$timepoint == ‘t1’ |
# date_df$timepoint == paste0(’t’, date_df$ntimepoints), ]
clinical_df <- date_df[date_df$timepoint == "t1" | mapply(grepl,
date_df$ntimepoints, date_df$timepoint),]

# recode() 'psy' as 'PS' in date_df$diagnosis
clinical_df$diagnosis <- recode(clinical_df$diagnosis, "psy" = "PS")

# reshape2::dcast() date_df so that the two values of 'timepoint' each get
# their own column
# Step 1: recode the timepoint column | Step 2: reshape. need value.var
clinical_df$timepoint <- recode(clinical_df$timepoint, "t2" = "tfinal",
"t3" = "tfinal", "t4" = "tfinal", "t5" = "tfinal", "t6" = "tfinal")
clinical_df <- reshape2::dcast(clinical_df, bblid ~ timepoint,
value.var = "diagnosis")
clinical_df$t1_tfinal <- paste(clinical_df$t1, clinical_df$tfinal, sep = "_")

# merge() date_df and demo_df so that you only get the bblids in date_df that
# are in demo_df
date_df <- merge(demo_df, date_df, by.x = "bblid")

# Adding this line to help shorten date_df:
date_df <- date_df[, c("bblid", "dob", "timepoint", "ntimepoints", "dodiagnosis",
 "diagnosis")]

# Convert all date columns to be valid (as.Date())
date_df$dob <- as.Date(date_df$dob, format = "%m/%d/%y")
date_df$dodiagnosis <- as.Date(date_df$dodiagnosis, format = "%m/%d/%y")

# Calculate age for each assessment
date_df$age <- (as.numeric(date_df$dodiagnosis - date_df$dob))/365.25
date_df <- date_df[date_df$timepoint == "t1" | mapply(grepl, date_df$ntimepoints,
date_df$timepoint),]


################################################################################
# Write a function that returns the age at first and last diagnosis as an atomic
# vector of length two

ageFirstLast <- function(bblid){
#  bblid <- date_df[i,"bblid"]; This is only relevant for sapply but I for loop.
#  Ellyn: Anything written for a for loop can be written for a sapply.
	forBblid <- date_df[,c("bblid", "age")]
	forBblid <- forBblid[forBblid$bblid == bblid,]
	aget1 <- min(forBblid$age)
	agetf <- max(forBblid$age)
	ages <- c(aget1, agetf)

	return(ages)
}

# Write a function that returns, based on the age at assessment that you
# will calculate, the assessment number (e.g., if a person comes in at ages
# 12, 15, and 17, they would be assigned assessment numbers 1, 2, and 3,
# respectively).

colnames(screen_df)[colnames(screen_df) == "BBLID"] <- "bblid"
# Tries to get rid of the duplication error.
date_df2 <- date_df[duplicated(date_df$bblid),c("bblid", "dob")]
screen_df <- merge(date_df2, screen_df, by = "bblid")
screen_df$DOMHSCREEN <- as.Date(screen_df$DOMHSCREEN, format = "%m/%d/%y")
screen_df$age <- (as.numeric(screen_df$DOMHSCREEN - screen_df$dob))/365.25

# 1st method for assessNumber. Fail! Originally wanted to use it to edit the
# order of the ages which I would just go back to timestamp. Did not work
# because ages were manipulated, repeated, or just plain wrong. Next part was to
# add the "timpoint" part and edit in function but it would not sapply for
# everyone and would not save while in function form. I did not know how to
# sapply with specifications like in a for loop so I just foor looped and
# changed the function:
# assessNumber <- function(i){
#  bblid <- screen_df[i,"bblid"]
#  forBblid <- screen_df[,c("bblid", "age", "timepoint")]
#  forBblid <- forBblid[forBblid$bblid == bblid,]
#  forBblid$timepoint <- order(forBblid$age)
#  return(forBblid)
#}
# ahhh <- sapply(1:nrow(screen_df), assessNumber)


# Corrected but longer version:
screen_df$timepoint <- NA
assessNumber <- function(bblid){
  forBblid <- screen_df[screen_df$bblid == bblid, c("bblid", "age")]
  orderVals <- order(forBblid$age, )
  return(orderVals)
}
for(bblid in unique(screen_df$bblid)){
screen_df[screen_df$bblid == bblid, "timepoint"] <- assessNumber(bblid)
}

################################################################################

# sapply() your ageFirstLast() function
# Got an error for this line! But, tbh, I don't think its necessary. Can't I do
# this after the merge (final_df)
date_df[c("firstAge", "lastAge")] <- t(sapply(1:nrow(date_df), ageFirstLast))

# assessNumber use to filter screen_df for the first time point
# need to merge in dob to all of the dataframes in order to calc age
# if there is a single person that doesn't have a medical followup for one of the medical
# conditions, only put the first age in the tables that we are creating.
# Medical table that we are making will only have the first ages.
# Clinical table will look like Ellyn's with the first and last ages.
# Merge dob directly into screen_df and then do the assessNumber thingy
# gtsummary ** : need like 50 tables, use a for loop -> html table output

final_df <- date_df
# olddate_df <- read.csv(file= "~/Box Sync/medical_pnc/pnc_longitudinal_diagnosis_n749_20210112.csv")
# head(demo_df)
# head(clinical_df)
final_df <- merge(demo_df, clinical_df, by = "bblid")
# final_df[c("firstAge", "lastAge")] <- t(sapply(1:nrow(final_df), ageFirstLast))

for(bblid in final_df$bblid){
final_df[final_df$bblid == bblid,]$firstAge <- ageFirstLast(bblid)[1]
final_df[final_df$bblid == bblid,]$lastAge <- ageFirstLast(bblid)[2]
}
# Still wanna know why entering the c("firstAge", "lastAge") doesn't work if I
# return as an atomic length of 2. **

# Recode variables
final_df$sex <- recode(final_df$sex, `1`='Male', `2`='Female')
final_df$race <- recode(final_df$race, `1`='Caucasian', `2`='African American',
  `3`='US India/Alaska Native', `4`='Asian', `5`='More Than One Race')
final_df$Diagnosis <- recode(final_df$t1_tfinal, 'other_TD'='OP-TD',
  'other_other'='OP-OP', 'other_PS'='OP-PS', 'TD_other'='TD-OP', 'PS_other'='PS-OP',
  'TD_TD'='TD-TD', 'TD_PS'='TD-PS', 'PS_TD'='PS-TD', 'PS_PS'='PS-PS')
final_df$Diagnosis <- ordered(final_df$Diagnosis, c('TD-TD', 'TD-OP', 'TD-PS',
  'OP-TD', 'OP-OP', 'OP-PS', 'PS-TD', 'PS-OP', 'PS-PS'))
medical_Table <- final_df %>% select("sex", "race", "firstAge", "lastAge",
"Diagnosis")
plotMedical <- medical_Table %>% tbl_summary(by = Diagnosis, label =
list(sex ~ "Sex", race ~ "Race", firstAge ~ "First Age", lastAge ~ "Final Age"))

# ..........

# merge date_df and screen_df so that you only get the bblids in date_df that are
# in screen_df
tester <- merge(screen_df, final_df, by = "bblid")

# ..........
#for loop the screen_df for the diagnosis and include only the rows that have
# valid values (Y/N) and only use the first ages. A table per medical diagnosis.
