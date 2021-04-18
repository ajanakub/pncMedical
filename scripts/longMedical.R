R### This script creates demographic tables for each of the domains in the medical
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

# Write a function that returns, based on the age at assessment that you
# will calculate, the assessment number (e.g., if a person comes in at ages
# 12, 15, and 17, they would be assigned assessment numbers 1, 2, and 3,
# respectively).

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
# Add back in the yes/no values for each medical diagnosis.

removeColumn <- c("TIMES_SEIZURE", "TIMES_HEADINJURY", "UNCONSCIOUS_LENGTH",
  "AGE_UNCONSCIOUS", "AGE_SEIZURE", "UNCONSCIOUS_UNIT")

newScreen <- newScreen[,!(names(newScreen) %in% removeColumn)]

# For loop the screen_df for the diagnosis and include only the rows that have
# valid values (Y/N) and only use the first ages. A table per medical diagnosis.
for(i in names(newScreen)[2:16]){
  now <- newScreen[!is.na(newScreen[,i]) & newScreen[,i] %in% c("Y","N"),
    c("bblid", i, "sex", "ethnicity", "medicalAges", "t1_tfinal")]
  now <- now[,names(now) != "bblid"]
  show <- now %>% tbl_summary(by = names(now)[5], label =
  list(sex ~ "Sex", ethnicity ~ "Race", medicalAges ~ "First Age")) %>%
    modify_spanning_header(paste("stat", 1:nrow(unique(now[5])), sep = "_") ~ i)
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
   "med812b", "med814b", "med816b", "med817b", "med750a", "med756", "med757")

# Dropping columns.
medHistory <- medHistory[,!(names(medHistory) %in% bleh)]
medHistory <- medHistory[,!(grepl("dur|notes|review|timestamp|feedback|summary|
  history|check|complete|admin", names(medHistory)))]

# Renaming each column according to key.
medHistory <- rename(medHistory, "AbnmBirth" = "med070", "AbnmDevelopment" =
  "med072", "Headaches" = "med074" , "CopresentingHeadaches" = "med075",
  "HeadacheSensitivitytoLightorSound" = "med076", "HeadacheswithNausea" =
  "med077", "UnilateralHeadache" = "med078", "ThrobbingHeadaches" = "med079",
  "HeadachesInterruptSchool" = "med080" , "DoctorVisitforHeadaches" = "med081",
  "DiagnosedwithMigraines" = "med082", "TreatedforMigraines" = "med083",
  "DiagnosedwithEpilepsy" = "med117", "ConvulsionsorSeizures" = "med116",
  "TreatedwithEpilepsySeizuresorConvulsions" = "med118",
  "AgeofLastSeizure" = "med120", "CauseofSeizuresFound" = "med121",
  "SeizuresOnlyDuringFever" = "med124", "HistoryofSeriousHeadInjury" = "med125",
  "LostConsciousnessfromHeadInjury" = "med127",
  "HistoryofFractureorBreakSkull" = "med132",
  "SurgerytoHeadduetoInjury" = "med133", "AmnesiaorMemoryLoss" = "med134",
  "AbnormalHeadachesPostHeadSurgery" = "med138",
  "PostHeadacheMemoryLoss" = "med139", "UnexplainedUnconsciousEvent" = "med145",
  "UnconsciousnesstoOvernightHospitalization(FirstInstance)" = "med147",
  "UnconsciousnesstoOvernightHospitalization(SecondInstance)" = "med150",
  "UnconsciousnesstoOvernightHospitalization(ThirdInstance)" = "med153",
  "BirthDefects" = "med236", "TreatedforBrithDefects" = "med238",
  "BirthDefectaCurrentCondition" = "med241", "LeadPoisoning" = "med242",
  "TreatedforLeadPoisoning" = "med244", "LeadPoisoningaCurrentCondition" =
  "med247", "SpeechProblem" = "med248", "TreatedforSpeechProblem" = "med250",
  "SpeechProblemaCurrentCondition" = "med253", "VocalTics" = "med254",
  "TreatedforVocalTics" = "med256", "VocalTicsaCurrentCondition" = "med259",
  "MotorTics" = "med260", "TreatmentforMotorTics" = "med262",
  "MotorTicsaCurrentCondition" = "med265", "ReadingProblems" = "med267",
  "TreatedforReadingProblems" = "med269", "ReadingProblemsaCurrentCondition" =
  "med272", "LearningProblems" = "med273", "TreatedforLearningProblems" =
  "med275", "LearningProblemsaCurrentCondition" = "med278",
  "AutismorPervasiveDevelopmentDisorder" = "med291",
  "TreatedforAutismorPervasiveDevelopmentDisorder" = "med293",
  "AutismorPervasiveDevelopmentDisorderaCurrentCondition" = "med296",
  "Sleepwalking" = "med297", "TreatedforSleepWalking" = "med299",
  "SleepwalkingaCurrentCondition" = "med302", "Bedwetting(after5)" = "med303",
  "TreatedforBedwetting(after5)" = "med305",
  "Bedwetting(after5)aCurrentCondition" = "med308", "Allergies" = "med800",
  "Allergies(withinlast6months)" = "med800c", "CardiovascularComplications" =
  "med801", "CardiovascularComplicationsaCurrentCondition" = "med801c",
  "EndocrinologyDisease" = "med802", "EndocrinologyDisease(withinlast6months)" =
  "med802c", "ENTComplications" = "med803",
  "ENTComplications(withinlast6months)" = "med803c", "GastrointestinalIssues" =
  "med804", "GastrointestinalIssues(withinlast6months)" = "med804c",
  "GeneticsDisorder" = "med805", "GeneticsDisorder(withinlast6months)" =
  "med805c", "HematologyDisease" = "med806",
  "HematologyDisease(withinlast6months)" = "med806c", "HepatologyDisease" =
  "med807", "HepatologyDisease(withinlast6months)" = "med807c",
  "ImmunologyDisorder" = "med808", "ImmunologyDisorder(withinlast6months)" =
  "med808c", "InfectiousDisease" = "med809",
  "InfectiousDisease(withinlast6months)" = "med809c", "MetabolicDisease" =
  "med810", "MetabolicDisease(withinlast6months)" = "med810c",
  "NephrologyDisease" = "med811", "NephrologyDisease(withinlast6months)" =
  "med811c", "OncologyStatus" = "med812", "OncologyStatus(withinlast6months)" =
  "med812c", "OrthopedicCondition" = "med813",
  "OrthopedicCondition(withinlast6months)" = "med813c", "PlasticSurgery" =
  "med814", "PlasticSurgery(withinlast6months)" = "med814c",
  "PulmonaryDisease" = "med815", "PulmonaryDisease(withinlast6months)" =
  "med815c", "RheumatologyDisease" = "med816",
  "RheumatologyDisease(withinlast6months)" = "med816c",
  "Urology/GynecologyCondition" = "med817",
  "Urology/GynecologyCondition(withinlast6months)" = "med817c",
  "CorrectedVision" = "med598", "CorrectedVision:NearSightedness" = "med604",
  "TreatedforNearSightedness" = "med606", "NearSightednessaCurrentCondition" =
  "med609", "CorrectedVision:FarSightedness" = "med610",
  "TreatedforFarSightedness" = "med612", "FarSightednessaCurrentCondition" =
  "med615", "CorrectedVision:Astigmatism" = "med616", "TreatedforAstigmatism" =
  "med618", "AstigmatismaCurrentCondition" = "med621", "OtherVisionProblems" =
  "med622", "TreatforOtherVisionProblems" = "med624",
  "OtherVisionProblemsaCurrentCondition" = "med627", "HearingProblems" =
  "med628", "MildHearingProblems" = "med634",
  "TreatedforMildHearingProblems" = "med636",
  "MildHearingProblemsaCurrentCondition" = "med639", "SevereHearingProblems" =
  "med640", "TreatedforSevereHearingProblems" = "med642",
  "SevereHearingProblemsaCurrentCondition" = "med645",
  "CongenitalHearingProblems" = "med646",
  "TreatedforCongenitalHearingProblems" = "med648",
  "CongenitalHearingProblemsaCurrentCondition" = "med651",
  "HearingProblems:AcquiredDeafness" = "med652", "TreatedforAquiredDeafness" =
  "med654", "AcquiredDeafnessaCurrentCondition" = "med657",
  "HuntingtonsDisease" = "med714", "TreatedforHuntingtonsDisease" = "med716",
  "HuntingtonsDiseaseaCurrentCondition" = "med719", "Meningitis" = "med732",
  "TreatedforMeningitis" = "med734", "MeningitisaCurrentCondition" = "med737",
  "MotionSickness(5+episodes)" = "med738",
  "TreatedforMotionSickness(5+episodes)" = "med740",
  "MotionSicknessaCurrentCondition"= "med743", "MultipleSclerosis" = "med744",
  "TreatedforMultipleSclerosis" = "med746",
  "MultipleSclerosisaCurrentCondition" = "med749",
  "NeurologicalorNeuromuscularProblems" = "med750",
  "TreatedforNeurologicalorNeuromuscularProblems" = "med752",
  "NeurologicalorNeuromuscularProblemsaCurrentCondition" = "med755",
  "OtherMedicalConditions" = "med757a", "TreatedforOtherMedicalConditions" =
  "med757b", "OtherMedicalConditionsaCurrentCondition" = "med757c"
)

# Recoding 1 and 0 as Y/N
medHistory[medHistory == "1"] <- "Y"
medHistory[medHistory == "0"] <- "N"


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

for(i in names(medHistory)[3:80]){
  now <- medHistory[!is.na(medHistory[,i]) & medHistory[,i] %in% c("Y","N"),
    c("bblid", i, "sex", "ethnicity", "newMedAges", "t1_tfinal")]
  now <- now[,names(now) != "bblid"]
  show <- now %>% tbl_summary(by = names(now)[5], label =
  list(sex ~ "Sex", ethnicity ~ "Race", newMedAges ~ "First Age")) %>%
    modify_spanning_header(paste("stat", 1:nrow(unique(now[5])), sep = "_") ~ i)
  show <- show %>% as_gt()
  name <- paste0("print", i)
  namer <- paste(name, "html", sep = ".")
  setwd("~/Documents/PennBBL/pncMedical/tables/testerTables")
  gtsave(show, namer, getwd())
}

for(i in names(medHistory)[81:141]){
  now <- medHistory[!is.na(medHistory[,i]) & medHistory[,i] %in% c("Y","N"),
    c("bblid", i, "sex", "ethnicity", "newMedAges", "t1_tfinal")]
  now <- now[,names(now) != "bblid"]
  show <- now %>% tbl_summary(by = names(now)[5], label =
  list(sex ~ "Sex", ethnicity ~ "Race", newMedAges ~ "First Age")) %>%
    modify_spanning_header(paste("stat", 1:nrow(unique(now[5])), sep = "_") ~ i)
  show <- show %>% as_gt()
  name <- paste0("print", i)
  namer <- paste(name, "html", sep = ".")
  setwd("~/Documents/PennBBL/pncMedical/tables/testerTables")
  gtsave(show, namer, getwd())
}
# ls | wc -l
