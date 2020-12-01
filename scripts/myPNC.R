# Corrected Version
# "PNC Data Graphs" by Bukola Ajanaku
# November 10, 2020
# Ignore this line. This line's code was used to visualize the graphs through overwriting testing.csv: testing.csv <- write.csv(yay, "~/Documents/PennBBL/testing.csv")

setwd("~/Box Sync/medical_pnc")
relevantDF <- read.csv(file = "pnc_longitudinal_diagnosis_n752_202007.csv")
head(relevantDF)
medicalRatings <- read.csv("n9498_medical_ratings_20160731.csv")
medicalTable<- read.csv("n9498_go1_medical_smry_ratings.csv")

percent_DF <- expand.grid(c('TD', 'OP', 'PS'), c('TD', 'OP', 'PS'),
c("Allergies", "Asthma",
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
"Speech Problem", "Motor or Vocal Tics"))

percent_DF$Percent <- NA
names(percent_DF) <- c("First", "Final", "Diagnosis", "Percent")

#Merging the full medical table with data for the first (t1) and last (tfinal2) diagnosis.
fullMedicalTable <- merge(medicalTable, relevantDF, by = "bblid")
longitudinalData <- subset(fullMedicalTable, select = - c(smry_med_allergies_rtg, smry_med_asthma_rtg, smry_med_birth_abn_rtg, smry_med_birth_defects_rtg, smry_med_cardiovascular_rating, smry_med_devel_abn_rtg, smry_med_endocrine_rtg, smry_med_ent_rtg, smry_med_epilepsy_rtg,
smry_med_genetic_rtg, smry_med_gi_rtg, smry_med_head_inj_rtg, smry_med_hearing_severe_rtg, smry_med_hematology_rtg, smry_med_hepatology_rtg, smry_med_huntingtons_rtg, smry_med_immunology_rtg, smry_med_infectious_rtg, smry_med_lead_rtg,
smry_med_medication_rtg, smry_med_meningitis_rtg, smry_med_metabolic_rtg, smry_med_migraine_rtg, smry_med_motion_sickness_rtg, smry_med_ms_rtg, smry_med_nephrology_rtg, smry_med_oncology_rtg, smry_med_ortho_rtg, smry_med_other_neuro_rtg,
smry_med_other_vision_rtg, smry_med_plastics_rtg, smry_med_pulmonary_rtg, smry_psych_learning_prob_no_psy_illness_rtg, smry_med_rheumatology_rtg, smry_med_urogyn_rtg, smry_psych_bedwet_rtg, smry_psych_learning_prob_rtg, smry_psych_medication_rtg, smry_psych_pdd_rtg,
smry_psych_reading_prob_rtg, smry_psych_sleepwalk_rtg, smry_psych_speech_prob_rtg, smry_psych_tics_rtg, smry_med_total_all, smry_med_total_major, smry_med_total_psych, t2, t3, t4, t5, t6, tfinal, tfinal))
# use grep() to select columns, would shorten code


# longitudinalData[longitudinalData == "other"]<- "OP"
# Changing others to OP.
# Weird way to do it. Figure out a way to make it column-specific.
longitudinalData[longitudinalData$t1 == "other", "t1"] <- "OP"
longitudinalData[longitudinalData$tfinal2 == "other", "tfinal2"] <- "OP"

#Changing names of columns for longitudinalData.

library(data.table)
setnames(longitudinalData,
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
"smry_psych_speech_prob", "smry_psych_tics"),

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
"Speech Problem", "Motor or Vocal Tics"))

listen <- c("Allergies", "Asthma",
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
"Speech Problem", "Motor or Vocal Tics")

for (i in listen){
  # x <- as.character(i)

  percent_DF[percent_DF$First == "TD" & percent_DF$Final == "TD" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "TD", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "TD" & percent_DF$Final == "OP" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "OP", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "TD" & percent_DF$Final == "PS" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "TD" & longitudinalData$tfinal2 == "PS", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "OP" & percent_DF$Final == "TD" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "TD", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "OP" & percent_DF$Final == "OP" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "OP", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "OP" & percent_DF$Final == "PS" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "OP" & longitudinalData$tfinal2 == "PS", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "PS" & percent_DF$Final == "TD" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "TD", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "PS" & percent_DF$Final == "OP" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "OP", i], na.rm=TRUE) * 100

  percent_DF[percent_DF$First == "PS" & percent_DF$Final == "PS" & percent_DF$Diagnosis == i, "Percent"] <-
  mean(longitudinalData[longitudinalData$t1 == "PS" & longitudinalData$tfinal2 == "PS", i], na.rm=TRUE) * 100

  #yay <- percent_DF
}

library(ggplot2)

library(dplyr)
percent_DF <- percent_DF %>% mutate(First = recode(First, 'TD' = "TD - First Diagnosis", 'OP' = "OP - First Diagnosis", 'PS' = "PS - First Diagnosis"))
percent_DF <- percent_DF %>% mutate(Final = recode(Final, 'TD' = "TD - Last Diagnosis", 'OP' = "OP - Last Diagnosis", 'PS' = "PS - Last Diagnosis"))
# Used dplyr::recode() to rename the levels TD to TD-First Diagnosis ex: First Diagnosis - TD

percent_DF$First <- factor(percent_DF$First, levels = c('PS - First Diagnosis', 'OP - First Diagnosis', 'TD - First Diagnosis'), ordered = T)
percent_DF$Final <- factor(percent_DF$Final, levels = c('TD - Last Diagnosis', 'OP - Last Diagnosis', 'PS - Last Diagnosis'), ordered = T)

# Making some titles a bit shorter.
percent_DF$Diagnosis <- gsub("At least one psychoactive medication rated >=1", "1+ Psych Med Rated >=1", percent_DF$Diagnosis)
percent_DF$Diagnosis <- gsub("At least one medication rated >=1", "1+ Med Rated >=1", percent_DF$Diagnosis)
percent_DF$Diagnosis <- gsub("Autism or Pervasive Developmental Delay", "Autsm/Prvsv Dvlpmntl Delay", percent_DF$Diagnosis)

#Trying to reorder the Diagnoses on the x-axis to make them more readable.
percent_DF$Diagnosis <- factor(percent_DF$Diagnosis, levels =
c("Migraines", "Epilepsy", "Head Injury",
"Other Neurological problem",
"Motion Sickness",
"Plastic Surgery",
"Allergies",
"Asthma",
"Birth Abnormality", "Birth Defect",
"Genetic problem", "Developmental Abnormality",
"Autsm/Prvsv Dvlpmntl Delay",
"Reading Problem", "Learning Problem",
"Speech Problem", "Motor or Vocal Tics",
"Bedwetting  after age 5", "Sleepwalking",
"Ear/Nose/Throat problem",
"Other Vision problem",
"Severe Hearing problem",
"Orthopedic problem",
"Rheumatology problem",
"Meningitis",
"Hematology problem",
"Hepatology problem",
"Pulmonary problem",
"Cardiovascular problem",
"Nephrology problem",
"Urology/Gynecology problem",
"Gastrointestinal problem",
"Lead Poisoning",
"Oncology problem",
"Metabolic problem",
"Endocrine problem",
"Immunology problem",
"Infectious Disease problem",
"Huntington's Disease",
"Multiple Sclerosis",
"1+ Med Rated >=1",
"1+ Psych Med Rated >=1"), ordered = T)

percent_DF$colorGroup <- NA

groupOther <- c("Migraines", "Epilepsy", "Head Injury","Other Neurological problem","Motion Sickness","Plastic Surgery","Allergies","Asthma")
for(i in groupOther){
percent_DF[percent_DF$Diagnosis == i, "colorGroup"] <- "Other"
}

groupBirthDvlpProbs <- c("Birth Abnormality", "Birth Defect", "Genetic problem", "Developmental Abnormality", "Autsm/Prvsv Dvlpmntl Delay", "Reading Problem", "Learning Problem",  "Speech Problem", "Motor or Vocal Tics", "Bedwetting  after age 5", "Sleepwalking")
for(i in groupBirthDvlpProbs){
percent_DF[percent_DF$Diagnosis == i, "colorGroup"] <- "Birth/Developmental Issues"
}

groupOnGProbs <- c("Ear/Nose/Throat problem","Other Vision problem","Severe Hearing problem","Orthopedic problem","Rheumatology problem","Meningitis","Hematology problem",
"Hepatology problem","Pulmonary problem","Cardiovascular problem","Nephrology problem","Urology/Gynecology problem","Gastrointestinal problem")
for(i in groupOnGProbs){
percent_DF[percent_DF$Diagnosis == i, "colorGroup"] <- "Organ/Gland Issues"
}

groupSystProbs <- c("Lead Poisoning","Oncology problem","Metabolic problem","Endocrine problem","Immunology problem","Infectious Disease problem",
"Huntington's Disease","Multiple Sclerosis")
for(i in groupSystProbs){
percent_DF[percent_DF$Diagnosis == i, "colorGroup"] <- "Systemic Complications"
}

groupMeds <- c("1+ Med Rated >=1", "1+ Psych Med Rated >=1")
for(i in groupMeds){
percent_DF[percent_DF$Diagnosis == i, "colorGroup"] <- "Medication"
}

library(extrafont)
font_import()
loadfonts(device = "postscript")
percent_DF$colorGroup <- factor(percent_DF$colorGroup, levels = c("Other", "Birth/Developmental Issues", "Organ/Gland Issues", "Systemic Complications", "Medication"))

onlyPlot <- ggplot(percent_DF, aes(Diagnosis, Percent, fill = colorGroup)) +
geom_bar(stat="identity") +
facet_grid(First ~ Final) +
ylim(0, 100) +
theme_linedraw() +
ggtitle("Comorbidities at First Visit") +
scale_color_manual(values = c("red1", "yellow", "green1", "blue1", "violetred4")) +
theme(axis.text.x = element_text(angle=80, hjust=1, face = "italic"), legend.position = "bottom",
legend.title = element_blank(),
plot.title = element_text(size=14, face = "bold.italic", family = "Georgia"),
axis.title.x = element_text(size= 12, face = "bold.italic", family = "Georgia"),
axis.title.y = element_text(size= 12, face = "bold.italic", family = "Georgia"),
axis.text.y = element_text(face = "bold"))


pdf(file= "/Users/bukola/Documents/PennBBL/myFinalPNCGraph.pdf", family = "Georgia",
    width = 14,
    height = 7)

onlyPlot
dev.off()
