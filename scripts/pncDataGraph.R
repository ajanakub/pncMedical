# November 3, 2020
# "PNC Data Graphs" - Bukola Ajanaku

R
setwd("~/Box Sync/medical_pnc")
relevantDF <- read.csv(file = "pnc_longitudinal_diagnosis_n752_202007.csv")
head(relevantDF)
medicalRatings <- read.csv("n9498_medical_ratings_20160731.csv")
medicalTable<- read.csv("n9498_go1_medical_smry_ratings.csv")

shortenedMedicalTable <- subset(medicalTable, select = - c(smry_med_allergies_rtg, smry_med_asthma_rtg, smry_med_birth_abn_rtg, smry_med_birth_defects_rtg, smry_med_cardiovascular_rating, smry_med_devel_abn_rtg, smry_med_endocrine_rtg, smry_med_ent_rtg, smry_med_epilepsy_rtg,
smry_med_genetic_rtg, smry_med_gi_rtg, smry_med_head_inj_rtg, smry_med_hearing_severe_rtg, smry_med_hematology_rtg, smry_med_hepatology_rtg, smry_med_huntingtons_rtg, smry_med_immunology_rtg, smry_med_infectious_rtg, smry_med_lead_rtg,
smry_med_medication_rtg, smry_med_meningitis_rtg, smry_med_metabolic_rtg, smry_med_migraine_rtg, smry_med_motion_sickness_rtg, smry_med_ms_rtg, smry_med_nephrology_rtg, smry_med_oncology_rtg, smry_med_ortho_rtg, smry_med_other_neuro_rtg,
smry_med_other_vision_rtg, smry_med_plastics_rtg, smry_med_pulmonary_rtg, smry_psych_learning_prob_no_psy_illness_rtg, smry_med_rheumatology_rtg, smry_med_urogyn_rtg, smry_psych_bedwet_rtg, smry_psych_learning_prob_rtg, smry_psych_medication_rtg, smry_psych_pdd_rtg,
smry_psych_reading_prob_rtg, smry_psych_sleepwalk_rtg, smry_psych_speech_prob_rtg, smry_psych_tics_rtg, smry_med_total_all, smry_med_total_major, smry_med_total_psych))
head(shortenedMedicalTable)
# Creates a medical table that does not have rating values but has
# a value of whether they have the condition or not (0,1).
# **** I could have used " grep " to remove all the columns that ended in rtg. *

mainTable <- shortenedMedicalTable[ shortenedMedicalTable$bblid %in% relevantDF$bblid,]
# Creates a table for the bblids that are logitudinal (what we're looking for).

# Now the goal is to add the t1 and tfinal2 data to the table for graphing purposes.
mainTable <- cbind(mainTable, relevantDF$t1)
mainTable <- cbind(mainTable, relevantDF$tfinal2)

# Just going to go ahead and rename each of the column names.
library(data.table)
correctNames <- read.delim(file = "go1_medical_smry_ratings_dictionary.txt")

setnames(mainTable,
old = c("smry_med_allergies", "smry_med_asthma",
"smry_med_birth_abn", "smry_med_birth_defects",
"smry_med_cardiovascular", "smry_med_devel_abn",
"smry_med_endocrine", "smry_med_ent",
"smry_med_epilepsy", "smry_med_genetic",
"smry_med_gi", "smry_med_head_inj",
"smry_med_hearing_severe", "smry_med_hematology",
"smry_med_hepatology", "smry_med_huntingtons",
"smry_med_immunology", "smry_med_infectious",
"smry_med_lead", "smry_med_medication",
"smry_med_meningitis", "smry_med_metabolic",
"smry_med_migraine", "smry_med_motion_sickness",
"smry_med_ms", "smry_med_nephrology",
"smry_med_oncology", "smry_med_ortho",
"smry_med_other_neuro","smry_med_other_vision",
"smry_med_plastics", "smry_med_pulmonary",
"smry_med_rheumatology", "smry_med_urogyn",
"smry_psych_bedwet", "smry_psych_learning_prob",
"smry_psych_medication", "smry_psych_pdd",
"smry_psych_reading_prob", "smry_psych_sleepwalk",
"smry_psych_speech_prob", "smry_psych_tics",
"relevantDF$t1", "relevantDF$tfinal2"),

new = c("Allergies", "Asthma",
"Birth Abnormality", "Birth Defect",
"Cardiovascular problem", "Developmental Abnormality",
"Endocrine problem", "Ear/Nose/Throat problem",
"Epilepsy", "Genetic problem",
"Gastrointestinal problem", "Head Injury",
"Severe Hearing problem", "Hematology problem",
"Hepatology problem", "Huntington's Disease",
"Immunology problem", "Infectious Disease problem",
"Lead Poisoning", "At least one medication rated >=1",
"Meningitis", "Metabolic problem",
"Migraines", "Motion Sickness",
"Multiple Sclerosis", "Nephrology problem",
"Oncology problem", "Orthopedic problem",
"Other Neurological problem", "Other Vision problem",
"Plastic Surgery", "Pulmonary problem",
"Rheumatology problem", "Urology/Gynecology problem",
"Bedwetting  after age 5", "Learning Problem",
"At least one psychoactive medication rated >=1", "Autism or Pervasive Developmental Delay",
"Reading Problem", "Sleepwalking",
"Speech Problem", "Motor or Vocal Tics",
"Initial (t1)", "Final (t2)"))

mainTable[mainTable=="other"]<- "OP"
# Changing others to OP.

# I wanna clean up the NAs and set them equal to 0.
mainTable[is.na(mainTable)] <- 0

# Should I flip the tables or not???
# revMainTable <- t(mainTable)
# testing.csv <- write.csv(longitudinalData, "~/Documents/PennBBL/testing.csv")

# Working on percentages:
newmainTable[nrow(mainTable)+1,] <- NA
colnames(mainTable[nrow(mainTable),]) <- "Percents"

for(col in names(mainTable)[-1]) {
  ifelse(is.na(mainTable)){
  mainTable[paste0(col, "Percents")] = (sum(mainTable[col]) / ncol(mainTable[col]))
}}
##
library(dplyr)
Percentages <- summarize_all(mainTable[2:43], mean)
Percentages <- Percentages * 100

#Adding percentages to original dataframe for mainTable:
mainTable[nrow(mainTable)+1,] <- NA

# Time to graph:
library("ggplot2")
library("MASS")
library(extrafont)
library(wesanderson)
library("viridis")

flip <- t(mainTable)

slope <- cbind(colnames(mainTable), " ")
slope[,2] <- Percentages[,1:42]
slope[,2] <- summarize_all(mainTable[2:43], mean)
ifelse(is.na(slope[,2]) & !is.na(Percentages[,2]), combi$OD, combi$DT)

---

g <- ggplot(as.data.frame(slope), aes(x = rownames(graphable), y= as.data.frame(graphable[,1])) + geom_bar()
g + geom_bar(Percentages)





g <- ggplot(graphable, aes(as.data.frame(graphable[,1]))) + geom_bar()



### Use screenshots.
## Make a function that goes off each row.


----

----
forTDTD <- longitudinalData
forTDTD <- subset(longitudinalData, longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "TD")
forTDTDsums <- colSums(Filter(is.numeric, forTDTD[,-1]), na.rm=TRUE)
forTDTDsums <- as.data.frame(forTDTDsums)
forTDTDsums["First"] <- "TD"
forTDTDsums["Final"] <- "TD"
forTDTDsums <- t(forTDTDsums)
forTDTDsums <- as.data.frame(forTDTDsums)

forTDOP <- longitudinalData
forTDOP <- subset(longitudinalData, longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "OP")
forTDOPsums <- colSums(Filter(is.numeric, forTDOP), na.rm=TRUE)
forTDOPsums <- as.data.frame(forTDTDsums)
forTDOPsums["First"] <- "TD"
forTDOPsums["Final"] <- "OP"
forTDOPsums <- t(forTDTDsums)
forTDOPsums <- as.data.frame(forTDOPsums)

ella <- cbind(forTDTDsums, forTDOPsums)

forTDPS <- longitudinalData
forTDPS <- subset(longitudinalData, longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "PS")
forTDPSsums <- colSums(Filter(is.numeric, forTDPS), na.rm=TRUE)

forOPTD <- longitudinalData
forOPTD <- subset(longitudinalData, longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "TD")
forOPTDsums <- colSums(Filter(is.numeric, forOPTD), na.rm=TRUE)

forOPOP <- longitudinalData
forOPOP <- subset(longitudinalData, longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "OP")
forOPOPsums <- colSums(Filter(is.numeric, forOPOP), na.rm=TRUE)

forOPPS <- longitudinalData
forOPPS <- subset(longitudinalData, longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "PS")
forOPPSsums <- colSums(Filter(is.numeric, forOPPS), na.rm=TRUE)

forPSTD <- longitudinalData
forPSTD <- subset(longitudinalData, longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "TD")
forPSTDsums <- colSums(Filter(is.numeric, forPSTD), na.rm=TRUE)

forPSOP <- longitudinalData
forPSOP <- subset(longitudinalData, longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "OP")
forPSOPsums <- colSums(Filter(is.numeric, forPSOP), na.rm=TRUE)

forPSPS <- longitudinalData
forPSPS <- subset(longitudinalData, longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "PS")
forPSPSsums <- colSums(Filter(is.numeric, forPSPS), na.rm=TRUE)

# percent_DF$colname(val) <- print(val)
----



select = c(bblid, Allergies, Asthma, Birth Abnormality, Birth Defect,
Cardiovascular problem, Developmental Abnormality, Endocrine problem,
Ear/Nose/Throat problem, Epilepsy, Genetic problem, Gastrointestinal problem,
Head Injury, Severe Hearing problem, Hematology problem,
Hepatology problem, Huntington's Disease,
Immunology problem, Infectious Disease problem,
Lead Poisoning, At least one medication rated >=1,
Meningitis, Metabolic problem, Migraines,
Motion Sickness, Multiple Sclerosis, Nephrology problem,
Oncology problem, Orthopedic problem, Other Neurological problem,
Other Vision problem, Plastic Surgery, Pulmonary problem,
Rheumatology problem, Urology/Gynecology problem,
Bedwetting  after age 5, Learning Problem,
At least one psychoactive medication rated >=1, Autism or Pervasive Developmental Delay,
Reading Problem, Sleepwalking, Speech Problem, Motor or Vocal Tics))






library(dplyr)
getPercent <- function(x) {
  forTDTD <- subset(mainTable, relevantDF$t1 == "TD" & relevantDF$tfinal2 == "TD",
  select = smry_med_allergies)
  forTDTD <- colMeans(forTDTD, na.rm=TRUE) * 100
  (percent_DF["First Diagnosis"] == "TD" & percent_DF["Last Diagnosis"] == "TD" & percent_DF["Condition"] == "Allergies")[,3] <- as.data.frame(forTDTD)[,1]
  }

  subset(percent_DF, percent_DF[,1] == "TD" & percent_DF[,2] == "TD") <- percentagesForTDTD

  subset(percent_DF, percent_DF[,1] == "TD" & percent_DF[,2] == "TD")[,4] <- percentagesForTDTD
