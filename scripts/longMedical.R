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
library(gt)

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
# How Ellyn did it: date_df <- date_df[date_df$timepoint == ‘t1’ |
clinical_df <- date_df[date_df$timepoint == "t1" | mapply(grepl,
date_df$ntimepoints, date_df$timepoint),]

# recode() 'psy' as 'PS' in date_df$diagnosis
clinical_df$diagnosis <- recode(clinical_df$diagnosis, "psy" = "PS")

# reshape2::dcast() date_df so that the two values of 'timepoint' each get
# their own column
clinical_df$timepoint <- recode(clinical_df$timepoint, "t2" = "tfinal",
"t3" = "tfinal", "t4" = "tfinal", "t5" = "tfinal", "t6" = "tfinal")
clinical_df <- reshape2::dcast(clinical_df, bblid ~ timepoint,
value.var = "diagnosis")
clinical_df$t1_tfinal <- paste(clinical_df$t1, clinical_df$tfinal, sep = "_")

# merge() date_df and demo_df so that you only get the bblids in date_df that
# are in demo_df
date_df <- merge(demo_df, date_df, by = "bblid")

# Adding this line to help shorten date_df:
date_df <- date_df[, c("bblid", "dob", "timepoint", "ntimepoints", "dodiagnosis",
 "diagnosis")]

# Convert all date columns to be valid (as.Date())
date_df$dob <- as.Date(date_df$dob, format = "%m/%d/%y")
date_df$dodiagnosis <- as.Date(date_df$dodiagnosis, format = "%m/%d/%y")
# age for TD-OP etc diagnosis.

# Calculate age for each assessment
date_df$age <- (as.numeric(date_df$dodiagnosis - date_df$dob))/365.25
date_df <- date_df[date_df$timepoint == "t1" | mapply(grepl, date_df$ntimepoints,
date_df$timepoint),]


################################################################################
# Write a function that returns the age at first and last diagnosis as an atomic
# vector of length two

ageFirstLast <- function(bblid){
	forBblid <- date_df[,c("bblid", "age")]
	forBblid <- forBblid[forBblid$bblid == bblid,]
	aget1 <- min(forBblid$age)
	agetf <- max(forBblid$age)
	ages <- c(aget1, agetf)

	return(ages)
}

names(screen_df)[names(screen_df) == "BBLID"] <- "bblid"
  # Tries to get rid of the duplication error.

date_df2 <- date_df[duplicated(date_df$bblid),c("bblid", "dob")]
screen_df <- merge(date_df2, screen_df, by = "bblid")
screen_df$DOMHSCREEN <- as.Date(screen_df$DOMHSCREEN, format = "%m/%d/%y")
screen_df$medicalAges <- (as.numeric(screen_df$DOMHSCREEN - screen_df$dob))/365.25
# DOMHSCREEN: age for medical health screen -> ages in screen_df
# dodiagnosis: age for psychopathology tests -> ages in date_df

screen_df$timepoint <- NA

assessNumber <- function(bblid){
  forBblid <- screen_df[screen_df$bblid == bblid, c("bblid", "medicalAges")]
  orderVals <- rank(forBblid$medicalAges)
  return(orderVals)
}

for(bblid in unique(screen_df$bblid)){
  screen_df[screen_df$bblid == bblid, "timepoint"] <- assessNumber(bblid)
}

################################################################################

# sapply() your ageFirstLast() function. ~Used merge and then a for loop instead.

final_df <- merge(demo_df, clinical_df, by = "bblid")
final_df[, c("t1", "tfinal")] <- NULL
final_df[c("firstPsychAge", "lastPsychAge")] <- NA

for(bblid in final_df$bblid){
  final_df[final_df$bblid == bblid,"firstPsychAge"] <- min(ageFirstLast(bblid))
  final_df[final_df$bblid == bblid,"lastPsychAge"] <- max(ageFirstLast(bblid))
}

# Recode variables
final_df$sex <- recode(final_df$sex, `1`='Male', `2`='Female')
final_df$ethnicity <- recode(final_df$race, `1`='Caucasian', `2`='African American',
  `3`='US India/Alaska Native', `4`='Asian', `5`='More Than One Race')
final_df$Diagnosis <- recode(final_df$t1_tfinal, 'other_TD'='OP-TD',
  'other_other'='OP-OP', 'other_PS'='OP-PS', 'TD_other'='TD-OP', 'PS_other'='PS-OP',
  'TD_TD'='TD-TD', 'TD_PS'='TD-PS', 'PS_TD'='PS-TD', 'PS_PS'='PS-PS')
final_df$Diagnosis <- ordered(final_df$Diagnosis, c('TD-TD', 'TD-OP', 'TD-PS',
  'OP-TD', 'OP-OP', 'OP-PS', 'PS-TD', 'PS-OP', 'PS-PS'))


medical_Table <- final_df %>% select("sex", "race", "firstPsychAge",
  "lastPsychAge", "Diagnosis")
tabClinical <- medical_Table %>% tbl_summary(by = Diagnosis, label =
  list(sex ~ "Sex", race ~ "Race", firstPsychAge ~ "First Age",
  lastPsychAge ~ "Final Age"))
tabClinical <- tabClinical %>% as_gt()
gtsave(tabClinical,"tabClinical.html", "~/Documents/PennBBL/pncMedical/tables")
# Figure out the alternative to deprecated group_by()

################################################################################

# Merge date_df and screen_df so that you only get the bblids in date_df that
# are in screen_df. Grab nothing but bblids from date_df because date_df is just
# psychopathology ages.
# newScreen <- merge(screen_df, final_df[,c("bblid", "ntimepoints")], by = "bblid")
newScreen <- screen_df[screen_df$bblid %in% date_df$bblid,]

# Dropping all bblids with NA ages.
newScreen <- newScreen[is.na(newScreen$medicalAges) == FALSE,]

# Subsetting screen_df for only the first timepoints.
newScreen <- newScreen[newScreen$timepoint == 1,]

# Cleaning up newScreen so that only relevant columns are in the for loop.
drop <- c("dob","SID","OTHERID","OTHER_SITE", "PROTOCOL", "DOMHSCREEN",
"timepoint")

newScreen <- newScreen[,!mapply(grepl,"NOTES", names(newScreen))]
newScreen <- newScreen[,!(names(newScreen) %in% drop)]

# Grabbing t1_tfinal fron final_df to newScreen by bblid.
newScreen <- merge(newScreen, final_df[, c("bblid", "t1_tfinal", "sex",
  "ethnicity")], by = "bblid")

# Recording variables for the table.
newScreen$t1_tfinal <- recode(newScreen$t1_tfinal, 'other_TD'='OP-TD',
  'other_other'='OP-OP', 'other_PS'='OP-PS', 'TD_other'='TD-OP', 'PS_other'='PS-OP',
  'TD_TD'='TD-TD', 'TD_PS'='TD-PS', 'PS_TD'='PS-TD', 'PS_PS'='PS-PS')
newScreen$t1_tfinal <- ordered(newScreen$t1_tfinal, c('TD-TD', 'TD-OP', 'TD-PS',
    'OP-TD', 'OP-OP', 'OP-PS', 'PS-TD', 'PS-OP', 'PS-PS'))

removeColumn <- c("TIMES_SEIZURE", "TIMES_HEADINJURY", "UNCONSCIOUS_LENGTH",
  "AGE_UNCONSCIOUS", "AGE_SEIZURE", "UNCONSCIOUS_UNIT")

newScreen <- newScreen[,!(names(newScreen) %in% removeColumn)]

# For loop the screen_df for the diagnosis and include only the rows that have
# valid values (Y/N) and only use the first ages. A table per medical diagnosis.
nonMedColumns <- c("bblid", "medicalAges", "t1_tfinal", "sex", "ethnicity")

names(newScreen)[names(newScreen) == "MEMORYLOSE"] <- "MEMORYLOSS"
newScreen[newScreen == "Y"] <- "Yes"
newScreen[newScreen == "N"] <- "No"

for(i in names(newScreen[,!(names(newScreen) %in% nonMedColumns)])){
  now <- newScreen[newScreen[,i] %in% c("Yes","No"),
    c("bblid", i, "sex", "ethnicity", "medicalAges", "t1_tfinal")]
  now <- now[,names(now) != "bblid"]
  show <- now %>% tbl_summary(by = names(now)[5], label =
  list(sex ~ "Sex", ethnicity ~ "Race", medicalAges ~ "First Age")) %>%
    modify_spanning_header(paste("stat", 1:9, sep = "_") ~ i)
  show <- show %>% as_gt()
  name <- paste0("print", i)
  namer <- paste(name, "html", sep = ".")
  gtsave(show, namer, "~/Documents/PennBBL/pncMedical/tables/medTables")
}

################ Cleaning up dataframe with longitudinal medical info. #########
# Creating a dataframe with the longitudinal medical history csv.
medHistory <- read.csv(file = "~/Box Sync/medical_pnc/Longitudinal Medical/CommonInterviewScale_Capa_MedHistory_n9498.csv")

# Creating a vector to drop the not needed columns to make it easier to manipulate.
bleh <- c("redcapid", "redcap_data_access_group", "interview_id", "site_id",
  "subject_id", "famid", "assessment_id", "protocol", "protocol_number",
  "assessment", "libi_id", "version", "assessor", "interview_type", "location",
  "location_other", "location_remote_type", "location_remote_method",
  "location_remote_av", "location_remote_notes", "date1_note", "date2",
  "date2_note", "previous_capa_date", "previous_capa_dayssince", "name",
  "coll_name", "starttime", "capa_admin_complete", "medical_history",
  "no_medical_history___2", "no_medical_history___9", "med001", "med002",
  "med_height_src", "med003", "med_weight_src", "med815b", "med813b", "med810b",
   "med809b", "med808b", "med804b", "med803b", "med800b", "med071", "med073",
   "med122", "med126", "med119", "med131", "med140", "med144", "med146",
   "med148", "med148a", "med149", "med151", "med151a", "med152", "med154",
   "med154a", "med801b", "med802b", "med805b", "med806b", "med807b", "med811b",
   "med812b", "med814b", "med816b", "med817b", "med750a", "med756", "med757",
   "med075", "med076", "med077", "med078", "med079", "med080", "med081",
   "med083", "med118", "med120", "med121", "med124", "med150", "med153",
   "med238", "med241", "med244", "med247", "med250", "med253", "med256",
   "med259", "med262", "med265", "med269", "med272", "med275", "med278",
   "med293", "med296", "med299", "med302", "med305", "med308", "med800c",
   "med801c", "med802c", "med803c", "med804c", "med805c", "med806c", "med807c",
   "med808c", "med809c", "med810c", "med811c", "med812c", "med813c", "med814c",
   "med815c", "med816c", "med817c", "med606", "med609", "med612", "med615",
   "med618", "med621", "med624", "med627", "med636", "med639", "med642",
   "med645", "med648", "med651", "med654", "med657", "med716", "med719",
   "med734", "med737", "med740", "med743", "med746", "med749", "med752",
   "med755", "med757b", "med757c")

# Dropping columns.
medHistory <- medHistory[,!(names(medHistory) %in% bleh)]
medHistory <- medHistory[,!(grepl("dur|notes|review|timestamp|feedback|summary|
  history|check|complete|admin", names(medHistory)))]

# Renaming each column according to key.
medHistory <- rename(medHistory, "AbnmBirth" = "med070", "AbnmDevelopment" =
  "med072", "Headaches" = "med074" , "Migraines" = "med082", "Epilepsy" =
  "med117", "ConvulsionsorSeizures" = "med116", "SeriousHeadInjury" = "med125",
  "LostConsciousnessfromHeadInjury" = "med127", "SkullFractureorBreak" = "med132",
  "SurgerytoHeadduetoInjury" = "med133", "AmnesiaorMemoryLoss" = "med134",
  "AbnormalHeadachesPostHeadSurgery" = "med138", "PostHeadacheMemoryLoss" =
  "med139", "UnexplainedUnconsciousEvent" = "med145",
  "UnconsciousnesstoOvernightHospitalization" = "med147", "BirthDefects" =
  "med236", "LeadPoisoning" = "med242", "SpeechProblem" = "med248",
  "VocalTics" = "med254", "MotorTics" = "med260", "ReadingProblems" = "med267",
  "LearningProblems" = "med273", "AutismorPervasiveDevelopmentDisorder" =
  "med291", "Sleepwalking" = "med297", "BedwettingAfter5" = "med303",
  "Allergies" = "med800", "CardiovascularComplications" = "med801",
  "EndocrinologyDisease" = "med802", "ENTComplications" = "med803",
  "GastrointestinalIssues" = "med804", "GeneticsDisorder" = "med805",
  "HematologyDisease" = "med806", "HepatologyDisease" = "med807",
  "ImmunologyDisorder" = "med808", "InfectiousDisease" = "med809",
  "MetabolicDisease" = "med810", "NephrologyDisease" = "med811",
  "OncologyStatus" = "med812", "OrthopedicCondition" = "med813",
  "PlasticSurgery" = "med814", "PulmonaryDisease" = "med815",
  "RheumatologyDisease" = "med816", "UrologyorGynecologyCondition" = "med817",
  "CorrectedVision" = "med598", "NearSightedness" = "med604", "FarSightedness" =
  "med610", "Astigmatism" = "med616", "OtherVisionProblems" = "med622",
  "HearingProblems" = "med628", "MildHearingProblems" = "med634",
  "SevereHearingProblems" = "med640", "CongenitalHearingProblems" = "med646",
  "AcquiredDeafness" = "med652", "HuntingtonsDisease" = "med714", "Meningitis" =
  "med732", "MotionSicknesswith5+episodes" = "med738", "MultipleSclerosis" =
  "med744", "NeurologicalorNeuromuscularProblems" = "med750",
  "OtherMedicalConditions" = "med757a"
)


# Recoding 1 and 0 as Y/N
medHistory[medHistory == "1"] <- "Yes"
medHistory[medHistory == "0"] <- "No"
# Dates for bblids are sketchy! Double check what is going on with dates.
# Issue with bblid 138950, and possibly others. Comb through dates and
# figure out what might be going on.


# Merging dataframes for only bblids with psychopathology and relevant data for
# creating tables.
medHistory <- merge(medHistory, final_df[, c("bblid", "t1_tfinal", "sex",
  "ethnicity", "dob")], by = "bblid")
# medHistory <- merge(medHistory, newScreen[, c("bblid", "medicalAges")], by = "bblid")


# Calculating age for date of diagnosis of these medical OtherMedicalConditions
medHistory$date <- as.Date(medHistory$date, format = "%m/%d/%y")
medHistory$dob <- as.Date(medHistory$dob, format = "%m/%d/%y")
medHistory <- medHistory[is.na(medHistory$date) == FALSE,]
medHistory$newMedAges <- (as.numeric(medHistory$date - medHistory$dob))/365.25

medHistory$t1_tfinal <- recode(medHistory$t1_tfinal, 'other_TD'='OP-TD',
  'other_other'='OP-OP', 'other_PS'='OP-PS', 'TD_other'='TD-OP', 'PS_other'='PS-OP',
  'TD_TD'='TD-TD', 'TD_PS'='TD-PS', 'PS_TD'='PS-TD', 'PS_PS'='PS-PS')
medHistory$t1_tfinal <- ordered(medHistory$t1_tfinal, c('TD-TD', 'TD-OP', 'TD-PS',
    'OP-TD', 'OP-OP', 'OP-PS', 'PS-TD', 'PS-OP', 'PS-PS'))

nonHistoryColumns <- c("bblid", "date", "t1_tfinal", "sex", "ethnicity",
  "dob", "newMedAges")

for(i in names(medHistory[,!(names(medHistory) %in% nonHistoryColumns)])){
  now <- medHistory[medHistory[,i] %in% c("Yes","No"),
    c("bblid", i, "sex", "ethnicity", "newMedAges", "t1_tfinal")]
  now <- now[,names(now) != "bblid"]
  show <- now %>% tbl_summary(by = names(now)[5], label =
  list(sex ~ "Sex", ethnicity ~ "Race", newMedAges ~ "First Age")) %>%
    modify_spanning_header(paste("stat", 1:9, sep = "_") ~ i)
  show <- show %>% as_gt()
  name <- paste0("print", i)
  namer <- paste(name, "html", sep = ".")
  gtsave(show, namer, "~/Documents/PennBBL/pncMedical/tables/testerTables")
}
# ls | wc -l

# Figure out the yes/nos.
# Both yes and nos and a total number of people. High-end of yes, low end-of yes, middle of yes/no.
# Get titles, descriptors, short data blurb
