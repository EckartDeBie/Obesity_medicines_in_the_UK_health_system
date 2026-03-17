#scripts for GitHub for the Obesity Medicines in the UK Health System paper

# STEP 1 - COMORBIDITY DATAFILES PREPARATION
#' ---
#' title: uMed comorbs selection
#' author: Eckart De Bie
#' output:
#'    html_document:
#'     toc: true
#'         
#' ---

#' uMED CMD data analysis 

#setWD
setwd("C:/Users/Gebruiker/Documents/PhD/myfilepath")

#load libraries
library(tidyverse)
library(ggalluvial)
library(ggpubr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(purrr)
library(stringr)

Update_1_diagnosis_Umed <- read_excel("Update_1_diagnosis_Umed.xlsx")
comorbid_diagnoses_umed <- Update_1_diagnosis_Umed
SNOMED_CAD <- read_excel("SNOMED_codes_OSA_CAD.xlsx")
SNOMED_OSA <- read_excel("SNOMED_codes_OSA_CAD.xlsx", sheet = "OSA")
SNOMED_cerebro <- read_excel("SNOMED_codes_OSA_CAD.xlsx", sheet = "cerebrovascular")
SNOMED_PAD <- read_excel("SNOMED_codes_OSA_CAD.xlsx", sheet = "PAD")
SNOMED_HF <- read_excel("SNOMED_codes_OSA_CAD.xlsx", sheet = "HF")
SNOMED_T2DM <- read_excel("SNOMED_codes_OSA_CAD.xlsx", sheet = "T2DM")
SNOMED_HTN <- read_excel("HTN.xlsx")
HTN_lipids <- read.csv("~/PhD/myfilepath/HTN_lipids.csv", sep=";")
lipid_disorder_codes_eu <- read.csv("~/PhD/myfilepath/lipid_disorder_codes_eu.csv", sep=";")
SNOMED_lipids <- merge(lipid_disorder_codes_eu,HTN_lipids[, c("Disorder.Code.Description"), drop = FALSE],by = "Disorder.Code.Description")
SNOMED_lipids$Disorder.Code <- gsub("^=", "", SNOMED_lipids$Disorder.Code)


cad <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_CAD$Code) %>% select(patient_study_identifier) %>% unique()
osa <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_OSA$code) %>% select(patient_study_identifier) %>% unique()
cerebro <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_cerebro$Code) %>% select(patient_study_identifier) %>% unique()
pad <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_PAD$code) %>% select(patient_study_identifier) %>% unique()
hf <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_HF$Code) %>% select(patient_study_identifier) %>% unique()
dyslip <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_lipids$Disorder.Code) %>% select(patient_study_identifier) %>% unique()
htn <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_HTN$`Disorder Code`) %>% select(patient_study_identifier) %>% unique()

cardiovasc <- rbind(cad, cerebro, pad, hf) %>% unique()

t2dm <- comorbid_diagnoses_umed %>% filter(snomed_code %in% SNOMED_T2DM$code) %>% select(patient_study_identifier) %>% unique()
#Issue: 3420 now! --> seems more logical

write_rds(cardiovasc, file='Umed_patients_cardiovasc.rds')
write_rds(cerebro, file='Umed_patients_cerebro.rds')
write_rds(hf, file='Umed_patients_heartfailure.rds')
write_rds(osa, file='Umed_patients_OSA.rds')
write_rds(t2dm, file='Umed_patients_T2DM.rds')
write_rds(htn, file='Umed_patients_HTN.rds')
write_rds(dyslip, file='Umed_patients_dyslip.rds')

#=========================================================================
#EdB continued on 25/09/25
#load in the data
Update_1_medication_Umed <- read_excel("Update_1_medication_Umed.xlsx")
qt_medication_acmd <- Update_1_medication_Umed

#load in the drugs
uk_antidiabetic_drugs_with_combinations <- read_excel("uk_antidiabetic_drugs_with_combinations.xlsx")


#Match and see
qt_medication_acmd <- qt_medication_acmd %>%
  rowwise() %>%
  mutate(Class = {
    match_idx <- which(str_detect(
      tolower(substance), 
      tolower(uk_antidiabetic_drugs_with_combinations$Generic)
    ))
    if (length(match_idx) > 0) uk_antidiabetic_drugs_with_combinations$Class[match_idx[1]] else NA_character_
  }) %>%
  ungroup()

#add class 2 for brand name
qt_medication_acmd <- qt_medication_acmd %>%
  rowwise() %>%
  mutate(Class2 = {
    match_idx <- which(str_detect(
      tolower(substance), 
      tolower(uk_antidiabetic_drugs_with_combinations$`Brand name (UK)`)
    ))
    if (length(match_idx) > 0) uk_antidiabetic_drugs_with_combinations$Class[match_idx[1]] else NA_character_
  }) %>%
  ungroup()

meds <- qt_medication_acmd
summary(as.factor(meds$Class))
summary(as.factor(meds$Class2))

#great, all have generic names!
#not sure what to do with prescription dates --> check with Mark
meds$metformin <- ifelse(meds$Class == 'Metformin', 'Metformin use', 'No metformin use')
meds$sglt2 <- ifelse(meds$Class == 'SGLT2 inhibitors', 'SGLT2 use', 'No SGLT2 use')
meds$DPP4 <- ifelse(meds$Class == 'DPP-4 inhibitors', 'DPP4 use', 'No DPP4 use')
meds$pio <- ifelse(meds$Class == 'Pioglitazone', 'Piglitazone use', 'No pioglitazone use')
meds$DPP4 <- ifelse(meds$Class == 'Sulfonylurea', 'SU use', 'No SU use')

meds <- meds %>% filter(!is.na(Class))
meds$min_date <- as.Date(meds$min_date)
meds$max_date <- as.Date(meds$max_date)


#make sure only 1 row per med (if continuous usage) --> I did this per class as class change does not count for triple therapy
meds <- meds %>%
  group_by(patient_study_identifier, Class) %>%
  summarise(
    min_date = min(min_date, na.rm = TRUE),
    max_date = max(max_date, na.rm = TRUE),
    .groups = "drop")


#only select drugs taken at the same time (as triple meds = during same time)
meds_filtered <- meds %>%
  inner_join(meds, by = "patient_study_identifier", suffix = c("", "_other")) %>%
  filter(
    min_date <= max_date_other &
      max_date >= min_date_other &
      !(min_date == min_date_other & max_date == max_date_other) 
  ) %>%
  distinct(patient_study_identifier, min_date, max_date, .keep_all = TRUE)

#now only retain IDs that also have metformin
metformin <- meds_filtered %>% filter(Class == 'Metformin') %>% select(patient_study_identifier) %>% unique()

meds2 <- merge(meds_filtered, metformin, by='patient_study_identifier')
#655 rows for 276 patients with metformin with some overlapping rows


#make sure there are at least 3 overlapping rows
meds_collapsed <- meds2 %>%
  group_by(patient_study_identifier, Class) %>%
  summarise(
    min_date = min(min_date, na.rm = TRUE),
    max_date = max(max_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(patient_study_identifier) %>%
  filter(n() >= 3) %>%  
  ungroup()


#now double check dates actually overlap
meds_collapsed <- meds_collapsed %>%
  group_by(patient_study_identifier) %>%
  filter(
    n_distinct(Class) >= 3,  
    max(min_date) <= min(max_date) 
  ) %>%
  ungroup()

#now get their unique IDs
triple_therapy <- meds_collapsed %>% select(patient_study_identifier) %>% unique()
#39 people on triple therapy
write_rds(triple_therapy, 'Patients_on_3_antidiabetics_incl_metformin.rds')

glp1 <- qt_medication_acmd %>% filter(umed_drug_class %in% c('Glucagon-like Peptide-1 (GLP-1) Agonists', 'Glucose-dependent Insulinotropic Polypeptide (GIP) and Glucagon-like Peptide-1 (GLP-1) Receptor Agonist'))
glp1$glp1 <- 'yes'
glp2 <- glp1
write_rds(glp2, file='GLP_meds_umed.rds')
glp1 <- glp1 %>% select(patient_study_identifier, glp1) %>% unique()
write_rds(glp1, file='prescribed_GLP1_MedsOct25.rds')

#=================================================================================================
#STEP 2
#main analyses

#' ---
#' title: Associations of Selexipag with long term outcomes
#' author: Eckart De Bie
#' output:
#'    html_document:
#'     toc: true
#'         
#' ---

#'CMD data analysis 

#setWD
setwd("C:/Users/Gebruiker/Documents/PhD/myfilepath")

#load libraries
library(tidyverse)
library(ggalluvial)
library(ggpubr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(purrr)
library(stringr)

#import the data from the drive
googledrive::drive_auth()
dir = drive_find(pattern = 'GLP-1 External data sharing ', type='folder')

if (nrow(dir) > 0) {
  folder_id <- dir$id[1]   
  query <- sprintf("'%s' in parents", folder_id)
  a <- drive_find(q = query)
} else {
  stop("Folder not found! Check the pattern.")
}

#file needs to be google sheet! --> load in
meds <- gs4_get("mylink")
umed_medication <- read_sheet(meds)

diag <- gs4_get("mylink")
umed_diagnoses <- read_sheet(diag)

glp1 <- gs4_get("mylink")
umed_glp1 <- read_sheet(glp1)


#' data checks
ids <- unique(umed_glp1$patient_study_identifier)
#number of unique IDs = number of rows

#check age, height, weight
umed_glp1$age_ehr <- as.numeric(umed_glp1$age_ehr)
umed_glp1$latest_height_ehr <- as.numeric(umed_glp1$latest_height_ehr)
umed_glp1$latest_weight_ehr <- as.numeric(umed_glp1$latest_weight_ehr)

hist(umed_glp1$latest_height_ehr, breaks=150)
hist(umed_glp1$latest_weight_ehr, breaks=150)
hist(umed_glp1$age_ehr, breaks=150)
#this all looks okay
umed_glp1$bmi <- umed_glp1$latest_weight_ehr/((umed_glp1$latest_height_ehr/100)^2)
hist(umed_glp1$bmi, breaks=150)
#a few BMI's are in the 70s, which is very high but not totally implausible, check these
high_bmi <- umed_glp1 %>% filter(bmi >55)
#raw values all seem plausible 

#' visualise GLP1 question responses
glpq <- umed_glp1[,c(2,23:127, 175)]
glpq$obese <- ifelse(glpq$bmi >=30, 'Obese', 'Not Obese')

glpq$discussion_about_weight <- ifelse(glpq$Q1_Talk_about_weight__yes == "TRUE", 'yes', 'no')
summary(as.factor(glpq$Q1_Talk_about_weight__yes))

#problem, NA and lost recorded --> for purposes of data visualisation, this should be in the group NA
glpq[, 3:10] <- lapply(glpq[, 3:10], function(x) {
  x[x %in% c("lost", "NA")] <- NA  
  x
})

#Write a loop to iron out the questions
#problems with NAs in loop --> manual ifelse
glpq$ways_to_lose_weight <- NA
glpq$ways_to_lose_weight <- as.character(glpq$ways_to_lose_weight)
glpq[,3:10] <- lapply(glpq[,3:10], as.character)

#everything
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'All', NA)

#nothing
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'None', glpq$ways_to_lose_weight)

#mix
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Exercise', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Medication', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Exercise', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Medication', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Exercise, Medication', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Exercise, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Exercise, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Medication, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Medication, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Exercise, Medication, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Exercise, Medication, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Exercise, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Medication, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Exercise, Medication, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Exercise, Medication, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Exercise, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Medication, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Exercise, Medication', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Exercise, Medication, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='FALSE', 'Healthy eating, Exercise, Medication, Programme/Groups', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Exercise, Medication, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='FALSE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Exercise, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='FALSE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Healthy eating, Medication, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='FALSE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'Exercise, Medication, Programme/Groups, Surgery', glpq$ways_to_lose_weight)
glpq$ways_to_lose_weight <- ifelse(glpq$Q2_WT_SUPPORT__Healthy_eating=='TRUE' & glpq$Q2_WT_SUPPORT__Exercise=='TRUE' & glpq$Q2_WT_SUPPORT__Med=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_grps=='TRUE' & glpq$Q2_WT_SUPPORT__Wt_loss_surg=='TRUE', 'All', glpq$ways_to_lose_weight)

#Check responses
summary(as.factor(glpq$ways_to_lose_weight))

glpq$glp1_used <- ifelse(glpq$Q3_GLP1_USED__Yes == 'TRUE', 'GLP1 used', 'No GLP1 used')
glpq$prescribed <- ifelse(glpq$Q5_GLP1_SOURCE__Pres == 'TRUE', 'Prescribed', NA)
glpq$prescribed <- ifelse(glpq$Q5_GLP1_SOURCE__Bought == 'TRUE', 'Bought', glpq$prescribed)
glpq$prescribed <- ifelse(glpq$Q5_GLP1_SOURCE__Pres == 'TRUE' & glpq$Q5_GLP1_SOURCE__Bought == 'TRUE', 'Both bought and prescribed', glpq$prescribed)

summary(as.factor(glpq$prescribed))

#replace lost and NA with actual NA for brand
glpq[,13:20] <- lapply(glpq[,13:20], as.character)

drug_cols <- c("Q4_GLP1_NAMES__Weg", 
               "Q4_GLP1_NAMES__Ozem", 
               "Q4_GLP1_NAMES__Sax", 
               "Q4_GLP1_NAMES__Mounj", 
               "Q4_GLP1_NAMES__Tru")

drug_map <- c(
  "Weg"   = "Wegovy",
  "Ozem"  = "Ozempic",
  "Sax"   = "Saxenda",
  "Mounj" = "Mounjaro",
  "Tru"   = "Trulicity"
)

# Clean columns: trim spaces, uppercase, "lost"/"NA" -> NA
glpq[, drug_cols] <- lapply(glpq[, drug_cols], function(x) {
  x <- toupper(trimws(x))
  x[x %in% c("LOST", "NA")] <- NA
  x
})

# Aggregate into one column
glpq$GLP1_drugs <- apply(glpq[, drug_cols], 1, function(x) {
  selected <- names(x)[x == "TRUE"]
  if(length(selected) == 0) return(NA_character_)
  suffixes <- gsub("Q4_GLP1_NAMES__", "", selected)
  paste(drug_map[suffixes], collapse = ", ")
})

# Ensure any "NA" strings are converted to real NA
glpq$GLP1_drugs[glpq$GLP1_drugs == "NA"] <- NA

# Check result
head(glpq$GLP1_drugs)

glpq$GLP1_drugs <- ifelse(glpq$GLP1_drugs == 'NA, NA, NA, NA, NA', NA, glpq$GLP1_drugs)
summary(as.factor(glpq$GLP1_drugs))


#get number of different drugs (in questionnaire)
glpq$number_of_GLP1_drugs <- ifelse(
  is.na(glpq$GLP1_drugs), 
  0,  # or NA if you prefer
  sapply(strsplit(glpq$GLP1_drugs, ","), length))

#now select what is needed
glpq[,20:106] <- lapply(glpq[,20:106], as.character)
glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Bld_Sug == 'TRUE', 'Blood sugar', NA)
glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Weight == 'TRUE', 'Weight', glpq$reason_for_use)
glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Weight == 'TRUE' & glpq$Q6_GLP1_REASON__Bld_Sug == 'TRUE', 'Both blood sugar and weight', glpq$reason_for_use)


sankey1 <- glpq %>% select(patient_study_identifier, obese, discussion_about_weight, ways_to_lose_weight, glp1_used, prescribed, number_of_GLP1_drugs, GLP1_drugs, reason_for_use)

#format the data
sankey1$ways_to_lose_weight <- ifelse(is.na(sankey1$ways_to_lose_weight), 'Not discussed', sankey1$ways_to_lose_weight)
sankey1$number_of_GLP1_drugs <- ifelse(sankey1$number_of_GLP1_drugs == 0, NA, sankey1$number_of_GLP1_drugs)
sankey1$prescribed <- ifelse(is.na(sankey1$prescribed), 'No GLP1 used', sankey1$prescribed)
sankey1$number_of_GLP1_drugs <- ifelse(is.na(sankey1$number_of_GLP1_drugs), 'No GLP1 used', sankey1$number_of_GLP1_drugs)
sankey1$glp1_used <- ifelse(is.na(sankey1$glp1_used), 'No GLP1 used', sankey1$glp1_used)
sankey1$GLP1_drugs <- ifelse(is.na(sankey1$GLP1_drugs), 'No GLP1 used', sankey1$GLP1_drugs)
sankey1$reason_for_use <- ifelse(is.na(sankey1$reason_for_use) & sankey1$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey1$reason_for_use)
sankey1$reason_for_use <- ifelse(is.na(sankey1$reason_for_use), 'No reason given', sankey1$reason_for_use)
sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight == 'Not applicable', 'None', sankey1$ways_to_lose_weight)

summary(as.factor(sankey1$ways_to_lose_weight))

sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight %in% c('Exercise, Medication, Programme/Groups, Surgery', 'Exercise, Programme/Groups, Surgery', 'Exercise, Surgery', 'Healthy eating, Exercise, Medication, Surgery', 'Healthy eating, Exercise, Programme/Groups, Surgery', 'Healthy eating, Medication, Programme/Groups, Surgery', 'Healthy eating, Surgery', 'Medication, Programme/Groups, Surgery', 'Medication, Surgery', 'Programme/Groups, Surgery', 'Surgery'), 'Surgery with/without other interventions', sankey1$ways_to_lose_weight)
sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight %in% c('Medication, Programme/Groups','Healthy eating, Medication','Healthy eating, Exercise, Medication, Programme/Groups','Healthy eating, Exercise, Medication','Exercise, Medication, Programme/Groups','Exercise, Medication'), 'Medication with other lifestyle interventions', sankey1$ways_to_lose_weight)
sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight == 'All', 'All options discussed', sankey1$ways_to_lose_weight)
sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight %in% c("Programme/Groups","Healthy eating, Programme/Groups","Exercise, Programme/Groups"), 'Programme/groups with/without other lifestyle  interventions', sankey1$ways_to_lose_weight)

summary(as.factor(sankey1$GLP1_drugs))
sankey1$GLP1_drugs <- ifelse(sankey1$GLP1_drugs %in% c('Ozempic, Mounjaro, Trulicity','Ozempic, Saxenda, Mounjaro','Wegovy, Ozempic, Trulicity','Wegovy, Ozempic, Mounjaro, Trulicity','Wegovy, Saxenda, Mounjaro','Wegovy, Ozempic, Saxenda, Mounjaro','Wegovy, Ozempic, Mounjaro','Wegovy, Mounjaro, Trulicity','Saxenda, Mounjaro, Trulicity','Ozempic, Saxenda, Mounjaro, Trulicity'), 'Combination of 3 or 4 drugs', sankey1$GLP1_drugs)
sankey1$GLP1_drugs <- ifelse(sankey1$GLP1_drugs %in% c('Ozempic, Saxenda','Ozempic, Trulicity','Wegovy, Ozempic'), 'Omzempic and one other', sankey1$GLP1_drugs)
sankey1$GLP1_drugs <- ifelse(sankey1$GLP1_drugs %in% c('Saxenda, Mounjaro','Wegovy, Mounjaro','Mounjaro, Trulicity'), 'Moujaro and one other', sankey1$GLP1_drugs)

sankey1ab <- sankey1
sankey1 <- sankey1 %>% filter(obese == 'Obese')

sankey1$ways_to_lose_weight <- ifelse(sankey1$ways_to_lose_weight == 'None', 'Not discussed', sankey1$ways_to_lose_weight)

#' Get flowchart of first few questions
#prepare data for alluvial plot
lab_tab <- as.data.frame(table(sankey1$discussion_about_weight, sankey1$ways_to_lose_weight, sankey1$glp1_used, sankey1$reason_for_use))

names(lab_tab)[[1]] <-"Discussion about weight"
names(lab_tab)[[2]] <-"Ways to lose weight discussed"
names(lab_tab)[[3]] <-"GLP1-analogue self-reported use"
names(lab_tab)[[4]] <-"Reasons for GLP1-analogue use"
names(lab_tab)[[5]] <-"Frequencies"

lab_tab$`GLP1-analogue self-reported use` <- as.character(lab_tab$`GLP1-analogue self-reported use`)
lab_tab$`GLP1-analogue self-reported use` <- ifelse(lab_tab$`GLP1-analogue self-reported use` == '1', 'OM used', 'No OM used')
lab_tab$`GLP1-analogue self-reported use` <- as.factor(lab_tab$`GLP1-analogue self-reported use`)

summary(as.factor(lab_tab$`Ways to lose weight discussed`))

lab_tab$`Ways to lose weight discussed` <- factor(
  lab_tab$`Ways to lose weight discussed`,
  levels = c(
    "Not discussed",
    "Healthy eating",
    "Exercise",
    "Healthy eating, Exercise",
    "Programme/groups with/without other lifestyle  interventions",
    "Medication",
    "Medication with other lifestyle interventions",
    "Surgery with/without other interventions", 
    "All options discussed"
  )
)

lab_tab$`Reasons for GLP1-analogue use` <- factor(
  lab_tab$`Reasons for GLP1-analogue use`,
  levels = c(
    "No reason given",
    "Blood sugar",
    "Both blood sugar and weight",
    "Weight"
  )
)

lab_tab$`GLP1-analogue self-reported use` <- factor(
  lab_tab$`GLP1-analogue self-reported use`,
  levels = c("No OM used", "OM used")
)

#lab_tab$`Ways to lose weight discussed` <- factor(
#  lab_tab$`Ways to lose weight discussed`,
#  levels = c("Not discussed", setdiff(unique(lab_tab$`Ways to lose weight discussed`), "Not discussed")))

#lab_tab$`GLP1-analogue self-reported use` <- factor(
#  lab_tab$`GLP1-analogue self-reported use`,
#  levels = c("No GLP1 used", setdiff(unique(lab_tab$`GLP1-analogue self-reported use`), "No GLP1 used")))

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:4,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow(alpha=0.4) +
  geom_stratum(alpha = 0.4, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication questionnaire for patients with obesity") +
  theme_pubr() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question)) +
  
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +
  
  # flows + strata (lighter colours optional)
  geom_flow(alpha = 0.4) +
  geom_stratum(alpha = 0.4, width = 0.5, color = "white") +
  
  # dual y-axis
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  
  ggtitle("GLP1 medication questionnaire for patients with obesity") +
  theme_pubr() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#==================================================================
#EdB continued here on 05/09/2025
#continue with formatting of the following questions

#glpq[,20:106] <- lapply(glpq[,20:106], as.character)
#glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Bld_Sug == 'TRUE', 'Blood sugar', NA)
#glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Weight == 'TRUE', 'Weight', glpq$reason_for_use)
#glpq$reason_for_use <- ifelse(glpq$Q6_GLP1_REASON__Weight == 'TRUE' & glpq$Q6_GLP1_REASON__Bld_Sug == 'TRUE', 'Both blood sugar and weight', glpq$reason_for_use)


glpq$current_user <- ifelse(glpq$Q7_GLP1_CURRENT_USE__Never_used == 'TRUE', 'Never', NA)
glpq$current_user <- ifelse(glpq$Q7_GLP1_CURRENT_USE__Yes == 'TRUE', 'Current user', glpq$current_user)
glpq$current_user <- ifelse(glpq$Q7_GLP1_CURRENT_USE__Used_before == 'TRUE', 'Past user', glpq$current_user)

glpq$nervous_about_talking_to_dr <- ifelse(glpq$Q8_GLP1_UNSURE__No == 'TRUE', 'Not nervous to talk about GLP1', NA)
glpq$nervous_about_talking_to_dr <- ifelse(glpq$Q8_GLP1_UNSURE__Not_sure == 'TRUE', 'Unsure if nervous to talk about GLP1', glpq$nervous_about_talking_to_dr)
glpq$nervous_about_talking_to_dr <- ifelse(glpq$Q8_GLP1_UNSURE__yes == 'TRUE', 'Nervous to talk about GLP1', glpq$nervous_about_talking_to_dr)
glpq$nervous_about_talking_to_dr <- ifelse(is.na(glpq$nervous_about_talking_to_dr), 'Answer not recorded/lost', glpq$nervous_about_talking_to_dr)


glpq$dose_titration <- ifelse(glpq$Q9_TITRATION_DONE__No_stay_low == 'TRUE', 'Remained at low dose', NA)
glpq$dose_titration <- ifelse(glpq$Q9_TITRATION_DONE__Not_sure == 'TRUE', 'Unsure', glpq$dose_titration)
glpq$dose_titration <- ifelse(glpq$Q9_TITRATION_DONE__Yes_finish == 'TRUE', 'Finished at raised dose', glpq$dose_titration)
glpq$dose_titration <- ifelse(glpq$Q9_TITRATION_DONE__Yes_raising == 'TRUE', 'Currently uptitrating dose', glpq$dose_titration)

#why dose was not raised --> multiple options! Requires loop

glpq <- glpq %>%
  mutate(
    reasons_for_not_up_titrating_dose = pmap_chr(
      list(
        Q10_TITRATION_BARRIERS__Dose_raised,
        `Q10_TITRATION_BARRIERS__S/E`,
        Q10_TITRATION_BARRIERS__Not_avail,
        Q10_TITRATION_BARRIERS__Instructions,
        Q10_TITRATION_BARRIERS__Low_dose_ok,
        Q10_TITRATION_BARRIERS__Stopped
      ),
      ~ {
        reasons <- c()
        if(..1 == "TRUE") reasons <- c(reasons, "Dose was raised")
        if(..2 == "TRUE") reasons <- c(reasons, "Side effects")
        if(..3 == "TRUE") reasons <- c(reasons, "Drug not available")
        if(..4 == "TRUE") reasons <- c(reasons, "Instructions were confusing")
        if(..5 == "TRUE") reasons <- c(reasons, "Low dose felt fine")
        if(..6 == "TRUE") reasons <- c(reasons, "Drug discontinued")
        if(length(reasons) == 0) NA_character_ else paste(reasons, collapse = "; ")
      }
    )
  )
summary(as.factor(glpq$reasons_for_not_up_titrating_dose))

glpq$experience_with_raising_dose <- ifelse(glpq$Q11_TITRATION_EASE__Neutral == 'TRUE', 'Neutral', NA)
glpq$experience_with_raising_dose <- ifelse(glpq$Q11_TITRATION_EASE__V_Easy == 'TRUE', 'Very easy', glpq$experience_with_raising_dose)
glpq$experience_with_raising_dose <- ifelse(glpq$Q11_TITRATION_EASE__Somewhat_easy == 'TRUE', 'Somewhat easy', glpq$experience_with_raising_dose)
glpq$experience_with_raising_dose <- ifelse(glpq$Q11_TITRATION_EASE__Somewhat_hard == 'TRUE', 'Somewhat hard', glpq$experience_with_raising_dose)
glpq$experience_with_raising_dose <- ifelse(glpq$Q11_TITRATION_EASE__V_Hard == 'TRUE', 'Very hard', glpq$experience_with_raising_dose)

glpq$instructions_for_raising_dose <- ifelse(glpq$`Q12_TITRATION_INSTR__Don't_recall` == 'TRUE', 'Do not recall', NA)
glpq$instructions_for_raising_dose <- ifelse(glpq$Q12_TITRATION_INSTR__Not_al_all_clr == 'TRUE', 'Not at all clear', glpq$instructions_for_raising_dose)
glpq$instructions_for_raising_dose <- ifelse(glpq$Q12_TITRATION_INSTR__Not_very_clr == 'TRUE', 'Not very clear', glpq$instructions_for_raising_dose)
glpq$instructions_for_raising_dose <- ifelse(glpq$Q12_TITRATION_INSTR__Somewhat_clr == 'TRUE', 'Somewhat clear', glpq$instructions_for_raising_dose)
glpq$instructions_for_raising_dose <- ifelse(glpq$Q12_TITRATION_INSTR__V_Clear == 'TRUE', 'Very clear', glpq$instructions_for_raising_dose)

glpq <- glpq %>%
  mutate(
    side_effects = pmap_chr(
      list(
        Q13_TITRATION_SIDES__Constip,
        Q13_TITRATION_SIDES__Diarr,
        Q13_TITRATION_SIDES__Headache,
        Q13_TITRATION_SIDES__Naus,
        Q13_TITRATION_SIDES__Vomit,
        Q13_TITRATION_SIDES__Loss_app,
        Q13_TITRATION_SIDES__None
      ),
      ~ {
        effects <- c()
        if(..1 == "TRUE") effects <- c(effects, "Constipation")
        if(..2 == "TRUE") effects <- c(effects, "Diarrhea")
        if(..3 == "TRUE") effects <- c(effects, "Headache")
        if(..4 == "TRUE") effects <- c(effects, "Nausea")
        if(..5 == "TRUE") effects <- c(effects, "Vomiting")
        if(..6 == "TRUE") effects <- c(effects, "Loss of appetite")
        if(..7 == "TRUE") effects <- c(effects, "None")
        if(length(effects) == 0) NA_character_ else paste(effects, collapse = "; ")
      }
    )
  )

summary(as.factor(glpq$side_effects))

#' get everything ready for a second Sankey plot
sankey2 <- glpq[,c(1,111,115:122)]

#update Sankey
sankey2$reason_for_use <- ifelse(sankey2$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey2$reason_for_use)
sankey2$nervous_about_talking_to_dr <- ifelse(sankey2$glp1_used == 'GLP1 used', 'Current or past user', sankey2$nervous_about_talking_to_dr)
sankey2$current_user <- ifelse(sankey2$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey2$current_user)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey2$reasons_for_not_up_titrating_dose)

summary(as.factor(sankey2$reasons_for_not_up_titrating_dose))

sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose %in% c('Dose was raised; Drug not available','Drug not available; Drug discontinued','Drug not available', 'Drug not available; Low dose felt fine'), 'Drug not available', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose %in% c('Side effects; Low dose felt fine','Side effects; Low dose felt fine; Drug discontinued', 'Dose was raised; Side effects; Low dose felt fine'), 'Side effects & low dose felt fine', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose %in% c('Drug not available; Instructions were confusing','Instructions were confusing','Instructions were confusing; Low dose felt fine', 'Side effects; Instructions were confusing'), 'Confusing instruction +/- other reason', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose %in% c('Dose was raised; Side effects','Side effects; Drug discontinued','Side effects; Drug not available', 'Side effects; Drug not available; Drug discontinued'), 'Side effects', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose %in% c('Dose was raised; Low dose felt fine','Low dose felt fine'), 'Low dose felt fine', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(is.na(sankey2$reasons_for_not_up_titrating_dose) & sankey2$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(sankey2$reasons_for_not_up_titrating_dose == 'Drug discontinued', 'Drug disconinued, reason not given', sankey2$reasons_for_not_up_titrating_dose)
sankey2$reasons_for_not_up_titrating_dose <- ifelse(is.na(sankey2$reasons_for_not_up_titrating_dose), 'Question not completed', sankey2$reasons_for_not_up_titrating_dose)

#find people who stopped
discontinued_users <- glpq %>% filter(Q10_TITRATION_BARRIERS__Stopped == "TRUE") %>% select(patient_study_identifier)

sankey2$drug_discontinued <- ifelse(glpq$patient_study_identifier %in% discontinued_users$patient_study_identifier, 'Discontinued', NA)
sankey2$drug_discontinued <- ifelse(is.na(sankey2$drug_discontinued) & sankey2$glp1_used == 'GLP1 used', 'Not discontinued', sankey2$drug_discontinued)
sankey2$drug_discontinued <- ifelse(is.na(sankey2$drug_discontinued) & sankey2$glp1_used == 'No GLP1 used', 'No GLP1 used', sankey2$drug_discontinued)
summary(as.factor(sankey2$drug_discontinued))

summary(as.factor(sankey2$side_effects))
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Vomiting','Nausea; Vomiting','Nausea'), 'Nausea and/or vomiting', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Vomiting; Loss of appetite','Nausea; Vomiting; Loss of appetite','Nausea; Loss of appetite'), 'Loss of apetite with nausea and/or vomiting', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Headache; None','Headache; Nausea; Vomiting','Nausea; Loss of appetite', 'Headache; Nausea', 'Headache; Loss of appetite','Headache; Nausea; Loss of appetite','Headache; Nausea; Vomiting; Loss of appetite','Headache; Loss of appetite'), 'Headache with/without N/V and/or loss of apetite', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Diarrhea; Nausea','Diarrhea; Nausea; Vomiting','Diarrhea'), 'Diarrhea with/without N/V', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Diarrhea; Nausea; Vomiting; Loss of appetite','Diarrhea; Nausea; Loss of appetite','Diarrhea; Loss of appetite'), 'Diarrhea and loss of apetite with/without N/V', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Constipation; Diarrhea; Constipation; Diarrhea; Loss of appetite','Constipation; Diarrhea; Loss of appetite','Constipation; Diarrhea; Nausea', 'Constipation; Diarrhea; Nausea; Loss of appetite','Constipation; Diarrhea; Nausea; Vomiting','Constipation; Diarrhea; Nausea; Vomiting; Loss of appetite','Constipation; Diarrhea; Vomiting', 'Constipation; Diarrhea; Vomiting; Loss of appetite', 'Constipation; Diarrhea'), 'Diarrhea and constipation with/without N/V/loss of apetite', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Constipation; Nausea; Vomiting','Constipation; Nausea; Vomiting; Loss of appetite','Constipation; Nausea; Vomiting; Loss of appetite', 'Constipation; Vomiting','Constipation; Vomiting; Loss of appetite','Constipation; Nausea; Loss of appetite','Constipation; Loss of appetite','Constipation; Nausea'), 'Constipation and N/V/loss of apetite', sankey2$side_effects)
sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Diarrhea; Headache; Vomiting',' Diarrhea; Headache; Nausea; Vomiting; Loss of appetite','Diarrhea; Headache; Nausea; Vomiting', 'Diarrhea; Headache; Nausea; Loss of appetite','Diarrhea; Headache; Nausea','Diarrhea; Headache; Loss of appetite','Diarrhea; Headache', 'Diarrhea; Headache; Nausea; Vomiting; Loss of appetite'), "Diarrhea with headache with/without N/V/loss of apetite", sankey2$side_effects)

sankey2$side_effects <- ifelse(sankey2$side_effects %in% c('Constipation; Headache; Nausea; Vomiting; Loss of appetite','Constipation; Headache; Nausea; Vomiting','Constipation; Headache; Nausea; Loss of appetite', 'Constipation; Headache; Nausea','Constipation; Headache; Loss of appetite','Constipation; Headache','Constipation; Diarrhea; Headache; Nausea; Vomiting; Loss of appetite','Constipation; Diarrhea; Headache; Nausea; Vomiting','Constipation; Diarrhea; Headache; Nausea; Loss of appetite','Constipation; Diarrhea; Headache; Nausea','Constipation; Diarrhea; Headache; Loss of appetite','Constipation; Diarrhea; Headache'), 'Consitpation with headache and/or N/V/loss of apetitie/diarrhea', sankey2$side_effects)

#now time to plot the Sankey

#' Flowchart for barriers to care
#prepare data for alluvial plot
lab_tab <- as.data.frame(table(sankey2$glp1_used, sankey2$nervous_about_talking_to_dr))

names(lab_tab)[[1]] <-"GLP1 usage"
names(lab_tab)[[2]] <-"Nervous about speaking to HCP about GLP1 medication"
names(lab_tab)[[3]] <-"Frequencies"


#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:2,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication questionnaire - seeking help from a HCP") +
  theme_pubr() +
  theme(legend.position = "none")


#'second set
#prepare data for alluvial plot
sankey3 <- sankey2 %>% filter(glp1_used == 'GLP1 used')
#1099 retained

sankey3 <- merge(sankey3, sankey1ab, by='patient_study_identifier')


lab_tab <- as.data.frame(table(sankey3$current_user, sankey3$reason_for_use.y, sankey3$prescribed, sankey3$reasons_for_not_up_titrating_dose, sankey3$side_effects))


names(lab_tab)[[1]] <-"Current user"
names(lab_tab)[[2]] <-"Reason for GLP1 use"
names(lab_tab)[[3]] <-"Prescribed or bought"
names(lab_tab)[[4]] <-"Reasons for not uptitrating the dose"
names(lab_tab)[[5]] <-"Side effects"
names(lab_tab)[[6]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:5,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - side effects & uptitration") +
  theme_pubr() +
  theme(legend.position = "none")



#do another one
lab_tab <- as.data.frame(table(sankey3$reason_for_use.y, sankey3$number_of_GLP1_drugs, sankey3$GLP1_drugs))

names(lab_tab)[[1]] <-"Reason for use"
names(lab_tab)[[2]] <-"Number of GLP1-analogues used"
names(lab_tab)[[3]] <-"Which GLP1-analogues used"
names(lab_tab)[[4]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:3,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - number of drugs") +
  theme_pubr() +
  theme(legend.position = "none")


#EdB continued here on 08/09/2025
glpq$lose_weight <- ifelse(glpq$Q14_WEIGHT_LOSS__No == 'TRUE', 'no', NA)
glpq$lose_weight <- ifelse(glpq$Q14_WEIGHT_LOSS__Yes == 'TRUE', 'yes', glpq$lose_weight)

glpq$spent_less_money_on_food <-ifelse(glpq$Q15_SPEND_LESS_FOOD__No == 'TRUE', 'Not spent less', NA)
glpq$spent_less_money_on_food <-ifelse(glpq$Q15_SPEND_LESS_FOOD__Yes == 'TRUE', 'Spent less', glpq$spent_less_money_on_food)
glpq$spent_less_money_on_food <-ifelse(glpq$Q15_SPEND_LESS_FOOD__Not_sure == 'TRUE', 'Not sure', glpq$spent_less_money_on_food)

glpq$changed_daily_activity <- ifelse(glpq$Q16_ACTIVITY_CHANGE__No == 'TRUE', 'No change in daily activities', NA)
glpq$changed_daily_activity <- ifelse(glpq$Q16_ACTIVITY_CHANGE__Yes == 'TRUE', 'Change in daily activities', glpq$changed_daily_activity)
glpq$changed_daily_activity <- ifelse(glpq$Q16_ACTIVITY_CHANGE__Not_sure == 'TRUE', 'Not sure', glpq$changed_daily_activity)


activities_map <- c(
  Q17_ACTIVITY_TYPES__Confidence   = "Feeling more confident in public",
  Q17_ACTIVITY_TYPES__Move_more    = "Moving around more",
  Q17_ACTIVITY_TYPES__More_energy  = "More energy during the day",
  Q17_ACTIVITY_TYPES__Easier_chores= "Easier to do household chores",
  Q17_ACTIVITY_TYPES__Go_out_more  = "Going out more",
  Q17_ACTIVITY_TYPES__Sleep        = "Sleeping better"
)

#Run loop now
glpq$which_activities_changed <- apply(glpq, 1, function(row) {
  active <- names(activities_map)[row[names(activities_map)] == "TRUE"]
  if (length(active) == 0) {
    return(NA)
  } else {
    return(paste(activities_map[active], collapse = "; "))
  }
})

summary(as.factor(glpq$which_activities_changed))



helped_map <- c(
  Q21_SUPPORT_NEEDED__Easy_to_get   = "Easier to get next dose",
  Q21_SUPPORT_NEEDED__Help_from_Dr    = "More help from HCP",
  `Q21_SUPPORT_NEEDED__Help_w_s/e`  = "More help with side effects",
  Q21_SUPPORT_NEEDED__Instructions= "Clearer instructions needed",
  Q21_SUPPORT_NEEDED__Low_cost  = "Lower cost",
  Q21_SUPPORT_NEEDED__No_problems        = "No problems",
  Q21_SUPPORT_NEEDED__Support = "Support from others"
)

#Run loop now
glpq$what_would_have_helped_to_stay_on_meds_or_raise_dose <- apply(glpq, 1, function(row) {
  active <- names(helped_map)[row[names(helped_map)] == "TRUE"]
  if (length(active) == 0) {
    return(NA)
  } else {
    return(paste(helped_map[active], collapse = "; "))
  }
})

glpq$what_would_make_unsure_if_medication_offered_again <- ifelse(glpq$Q22_UNSURE_AGAIN__No == 'TRUE', 'Nothing', NA)
glpq$what_would_make_unsure_if_medication_offered_again <- ifelse(glpq$Q22_UNSURE_AGAIN__Yes == 'lost', 'Response lost', glpq$what_would_make_unsure_if_medication_offered_again)
glpq$what_would_make_unsure_if_medication_offered_again <- ifelse(glpq$Q22_UNSURE_AGAIN__Not_sure == 'TRUE', 'Not sure', glpq$what_would_make_unsure_if_medication_offered_again)

summary(as.factor(glpq$Q23_LIFE_IMPACT))
3774+1196 -6049
#1079 open ended replies still recorded! Not too sure what t do with this! 

summary(as.factor(glpq$Q24_OTHER_EXP))
3774+1200 - 6049
#so 1075 responses still available to see

#do question 23
cmd_reason <- c(
  Q25_END_CMD_REASON__Confidence   = "Have a sense of purpose about my condition",
  Q25_END_CMD_REASON__Healthcare_of_others    = "Contribute to medical advancement",
  Q25_END_CMD_REASON__New_treatment  = "Have early access to innovative therapies",
  Q25_END_CMD_REASON__Progress= "Be part of community & recieve information about research progress",
  Q25_END_CMD_REASON__Research  = "Opportunity to be part of research"
)

#Run loop now
glpq$CMD_reason <- apply(glpq, 1, function(row) {
  active <- names(cmd_reason)[row[names(cmd_reason)] == "TRUE"]
  if (length(active) == 0) {
    return(NA)
  } else {
    return(paste(cmd_reason[active], collapse = "; "))
  }
})

#visualise the data again

#'make sankey
sankey4 <- glpq[,c(1,111,116, 123:132)]

sankey4a <- sankey4 %>% filter(glp1_used == 'GLP1 used')

#recode to make sankey more managable
sankey4a$which_activities_changed <- ifelse(sankey4a$which_activities_changed %in% c('More energy during the day', 'Going out more', 'More energy during the day; Easier to do household chores; Going out more','More energy during the day; Easier to do household chores','Moving around more', 'Moving around more; More energy during the day; Easier to do household chores; Going out more','Moving around more; Easier to do household chores','Moving around more; Easier to do household chores; Going out more','Moving around more; Going out more','Moving around more; More energy during the day','Moving around more; More energy during the day; Easier to do household chores', ' Moving around more; More energy during the day; Easier to do household chores; Going out more','Moving around more; More energy during the day; Going out more', 'Easier to do household chores'), 'Combination of (some of): more energy in the day, going out more, easier to do household chores, and moving around more', sankey4a$which_activities_changed)
sankey4a$which_activities_changed <- ifelse(sankey4a$which_activities_changed %in% c('Feeling more confident in public; Moving around more; More energy during the day; Going out more','Feeling more confident in public; Moving around more; More energy during the day; Easier to do household chores; Going out more','Feeling more confident in public; Moving around more; More energy during the day; Easier to do household chores; Going out more','Feeling more confident in public; Moving around more; More energy during the day; Easier to do household chores','Feeling more confident in public; Moving around more; More energy during the day','Feeling more confident in public; Moving around more; Going out more','Feeling more confident in public; Moving around more; Easier to do household chores; Going out more','Feeling more confident in public; Moving around more; Easier to do household chores','Feeling more confident in public; Moving around more','Feeling more confident in public; More energy during the day; Going out more', 'Feeling more confident in public; More energy during the day; Easier to do household chores; Going out more','Feeling more confident in public; More energy during the day; Easier to do household chores','Feeling more confident in public; More energy during the day','Feeling more confident in public; Going out more','Easier to do household chores; Going out more; Sleeping better', 'Feeling more confident in public; Easier to do household chores; Going out more','Feeling more confident in public; Easier to do household chores'), 'Feeling more confident in public & more energy/activities', sankey4a$which_activities_changed)
sankey4a$which_activities_changed <- ifelse(sankey4a$which_activities_changed %in% c('Moving around more; More energy during the day; Sleeping better', 'Moving around more; Sleeping better','Sleeping better','oving around more; More energy during the day; Sleeping better','Moving around more; More energy during the day; Going out more; Sleeping better','Moving around more; More energy during the day; Easier to do household chores; Sleeping better','Moving around more; More energy during the day; Easier to do household chores; Going out more; Sleeping better','Moving around more; Going out more; Sleeping better','Moving around more; Easier to do household chores; Sleeping better','Moving around more; Easier to do household chores; Going out more; Sleeping better','Moving around more; Easier to do household chores; Sleeping better','Moving around more; Easier to do household chores; Going out more; Sleeping better','More energy during the day; Sleeping better','Easier to do household chores; Sleeping better',' Easier to do household chores; Going out more; Sleeping better'),'Sleeping better & increased activities/energy', sankey4a$which_activities_changed)
sankey4a$which_activities_changed <- ifelse(sankey4a$which_activities_changed %in% c('Feeling more confident in public; Moving around more; Sleeping better','Feeling more confident in public; Sleeping better','Feeling more confident in public; Moving around more; More energy during the day; Sleeping better','Feeling more confident in public; Moving around more; More energy during the day; Going out more; Sleeping better','Feeling more confident in public; Moving around more; More energy during the day; Easier to do household chores; Sleeping better','Feeling more confident in public; Moving around more; Going out more; Sleeping better','Feeling more confident in public; Moving around more; More energy during the day; Easier to do household chores; Going out more; Sleeping better','Feeling more confident in public; Moving around more; Easier to do household chores; Sleeping better','Feeling more confident in public; More energy during the day; Sleeping better','Feeling more confident in public; More energy during the day; Going out more; Sleeping better','Feeling more confident in public; More energy during the day; Easier to do household chores; Going out more; Sleeping better','Feeling more confident in public; Going out more; Sleeping better','Feeling more confident in public; Easier to do household chores; Sleeping better'), 'Feeling more confident in public & sleeping better and/or increased activities/energy' ,sankey4a$which_activities_changed)
sankey4a$which_activities_changed <- ifelse(is.na(sankey4a$which_activities_changed), 'No change reported', sankey4a$which_activities_changed)

summary(as.factor(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose))
sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose <- ifelse(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose %in% c('Clearer instructions needed','Clearer instructions needed; No problems','More help from HCP','More help from HCP; Clearer instructions needed','More help from HCP; Clearer instructions needed; Lower cost','More help from HCP; Clearer instructions needed; No problems','More help from HCP; No problems', 'More help from HCP; Clearer instructions needed; Support from others', 'Support from others','More help from HCP; Clearer instructions needed; Support from others', 'No problems; Support from others','More help from HCP; No problems; Support from others','More help from HCP; Support from others','Clearer instructions needed; No problems; Support from others','Clearer instructions needed; Support from others'), 'More help from HCP or others and/or clearer instructions needed', sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose)
sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose <- ifelse(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose %in% c('Easier to get next dose','Easier to get next dose; Clearer instructions needed; No problems','Easier to get next dose; Clearer instructions needed; Support from others','Easier to get next dose; Support from others', 'Easier to get next dose; More help from HCP','Easier to get next dose; More help from HCP; Clearer instructions needed','Easier to get next dose; More help from HCP; No problems','Easier to get next dose; More help from HCP; Support from others','Easier to get next dose; No problems', 'asier to get next dose; Support from others'), 'Making it easier to get next dose with/witout help from HCP/others/clearer instruction' ,sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose)
sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose <- ifelse(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose %in% c('More help with side effects; Support from others','More help with side effects; No problems','More help with side effects; Clearer instructions needed','More help with side effects','More help from HCP; More help with side effects; Support from others','More help from HCP; More help with side effects; No problems','More help from HCP; More help with side effects; Clearer instructions needed','More help from HCP; More help with side effects'), 'More help with side effects with/without support from HCP/others/clearer instruction',sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose)
sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose <- ifelse(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose %in% c('Easier to get next dose; Clearer instructions needed; Lower cost','Easier to get next dose; Lower cost','Easier to get next dose; Lower cost; No problems','Easier to get next dose; More help from HCP; Lower cost','Lower cost','Lower cost; No problems','Lower cost; No problems; Support from others','Lower cost; Support from others','More help from HCP; Lower cost','More help from HCP; Lower cost; No problems','More help from HCP; Lower cost; Support from others'), 'Lower cost with/without making it easier to get next dose/more support',sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose)
sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose <- ifelse(sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose %in% c('More help with side effects; Lower cost; No problems','More help with side effects; Lower cost; No problems','More help from HCP; More help with side effects; Lower cost','Easier to get next dose; More help with side effects; Support from others','Easier to get next dose; More help with side effects; No problems','Easier to get next dose; More help with side effects; Lower cost','Easier to get next dose; More help with side effects; Clearer instructions needed','Easier to get next dose; More help with side effects','Easier to get next dose; More help from HCP; More help with side effects', 'More help with side effects; Lower cost'),'More help with side effects & lower cost or easier to get next dose and/or more support' ,sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose)

summary(as.factor(sankey4a$what_would_make_unsure_if_medication_offered_again))

sankey4a <- sankey4a %>% mutate(across(everything(), ~replace_na(.x, "Question not completed")))

#1099 retained
lab_tab <- as.data.frame(table(sankey4a$current_user, sankey4a$lose_weight, sankey4a$spent_less_money_on_food, sankey4a$changed_daily_activity, sankey4a$which_activities_changed, sankey4a$what_would_have_helped_to_stay_on_meds_or_raise_dose))

names(lab_tab)[[1]] <-"Current user"
names(lab_tab)[[2]] <-"Lost weight"
names(lab_tab)[[3]] <-"Spent less money on food"
names(lab_tab)[[4]] <-"Changed daily activity"
names(lab_tab)[[5]] <-"Which daily activites changed"
names(lab_tab)[[6]] <-"What would have helped to stay on/uptitrate drug"
names(lab_tab)[[7]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:6,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

library(stringr)
wrap_words <- function(x, n = 10) {
  x <- as.character(x)   # ensure it's character
  sapply(strsplit(x, " "), function(words) {
    paste(sapply(seq(1, length(words), by = n), function(i) {
      paste(words[i:min(i+n-1, length(words))], collapse = " ")
    }), collapse = "\n")
  })
}

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(
    stat = "stratum",
    size = 3.5,
    aes(label = wrap_words(as.character(after_stat(stratum)), 10))
  ) +
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - effects") +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    legend.position = "none"
  )

#do bladder control plot
lab_tab <- as.data.frame(table(sankey4a$current_user, sankey4a$lose_weight, sankey4a$blader_control, sankey4a$frequency_of_bladder_issues, sankey4a$bladder_control_improved_with_GLP_weight_loss))

names(lab_tab)[[1]] <-"Current user"
names(lab_tab)[[2]] <-"Lost weight"
names(lab_tab)[[3]] <-"Bladder control issues"
names(lab_tab)[[4]] <-"Frequency of bladder control issues"
names(lab_tab)[[5]] <-"Did issues improve with GLP1 associated weight loss"
names(lab_tab)[[6]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:5,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - bladder control") +
  theme_pubr() +
  theme(legend.position = "none")


#repeat for last question
#plot last question
sankey4$count_cmd <- 1

cmd <-sankey4 %>% group_by(CMD_reason) %>% count()
cmd$percentage <- cmd$n/6049*100
cmd <- cmd %>% filter(!is.na(CMD_reason))
cmd <- cmd %>% filter(percentage >2)


cmd$CMD_reason <- str_wrap(cmd$CMD_reason, width = 20)

ggplot(cmd, aes(x=CMD_reason, y=percentage, fill=CMD_reason)) +
  geom_bar(stat = "identity") +
  ggtitle('Most common reasons for participation in accessCMD') +
  xlab('Reason(s)') +
  ylab('Percentage of responses') +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10))



#' do some basic stats first

#Current user vs past users on reasons for not uptitrating the dose
library(Publish)

df <- merge(sankey1, sankey2, by='patient_study_identifier')
df2 <- merge(sankey3, sankey4a, by='patient_study_identifier')

df <- merge(df, df2, by='patient_study_identifier')
df <- df %>% filter(!is.na(current_user))
df <- df %>% filter(current_user !='Never')

user <- summary(univariateTable(current_user ~ prescribed.x + reasons_for_not_up_titrating_dose.x + side_effects.x + lose_weight + changed_daily_activity + spent_less_money_on_food + what_would_have_helped_to_stay_on_meds_or_raise_dose, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=df))

#EdB continued here on 09/09/2025
#compare comorbid diagnoses between groups
bp <- umed_glp1
bp$systolic_bp_reading_patient_reported <- as.numeric(bp$systolic_bp_reading_patient_reported)
bp$diastolic_bp_reading_patient_reported <- as.numeric(bp$diastolic_bp_reading_patient_reported)

hist(bp$diastolic_bp_reading_patient_reported)
hist(bp$systolic_bp_reading_patient_reported)
#BPs make sense --> also quantify this (based on NHS guidance on home readings)
bp$hypertens <- ifelse(bp$systolic_bp_reading_patient_reported > 135 & bp$diastolic_bp_reading_patient_reported >85, 'HTN', 'No measured HTN')
summary(as.factor(bp$hypertens))

htn_pt <- bp %>% filter(hypertens == 'HTN') %>% select(patient_study_identifier) %>% unique()

diag <- umed_diagnoses %>% select(patient_study_identifier, diagnosis_label)
#add HTN here if not self reported
diag$disease_present <- 'Yes'
diag <- unique(diag)
diag <- diag %>% pivot_wider(names_from = diagnosis_label, values_from = disease_present)

diag$Hypertension <- ifelse(is.na(diag$Hypertension) & diag$patient_study_identifier %in% htn_pt$patient_study_identifier, 'Yes', diag$Hypertension) 

#change all NAs to no in this df
df_bl <- glpq %>% select(patient_study_identifier, obese, glp1_used)
diag <- merge(df_bl, diag, by='patient_study_identifier', all=T)
diag[is.na(diag)] <- "no"
diag$obese <- ifelse(diag$obese == 'no', NA, diag$obese)
diag$glp1_used <- ifelse(diag$glp1_used == 'no', NA, diag$glp1_used)

bl_dat <- umed_glp1 %>% select(patient_study_identifier, sex_ehr, age_ehr)

diag <- merge(bl_dat, diag, by='patient_study_identifier')

obl <- diag %>% filter(!is.na(obese))
glpbl <- diag %>% filter(!is.na(glp1_used))

obese_bl <- summary(univariateTable(obese ~ age_ehr + sex_ehr + glp1_used + Hypertension + Diabetes +Hypercholesterolemia + CVD + CHF, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=obl))
glp_bl <- summary(univariateTable(glp1_used ~ age_ehr + sex_ehr + obese + Hypertension + Diabetes +Hypercholesterolemia + CVD + CHF, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=glpbl))

#check comorbs for past vs present users
glp_past_present <- merge(glpbl, df, by='patient_study_identifier')

users_comorb <- summary(univariateTable(current_user ~ age_ehr + sex_ehr + obese.x + Hypertension + Diabetes +Hypercholesterolemia + CVD + CHF, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=glp_past_present))

#now check duration of GLP1 drugs
meds <- umed_medication %>% select(patient_study_identifier, min_date, max_date, substance)
summary(as.factor(meds$substance))

#check if unique and #switches per patient
meds <- unique(meds)
meds$min_date <- as.Date(meds$min_date)
meds$max_date <- as.Date(meds$max_date)

meds_clean <- meds %>% group_by(patient_study_identifier) %>%summarise(min_date = min(min_date, na.rm = TRUE),max_date = max(max_date, na.rm = TRUE),number_of_drugs = n_distinct(substance))
meds_clean$duration <- meds_clean$max_date - meds_clean$min_date
meds_clean$duration <- as.numeric(meds_clean$duration)
hist(meds_clean$duration)

meds_clean <- merge(meds_clean, df, by='patient_study_identifier')

meds_clean$current_user <- as.factor(meds_clean$current_user)

wilcox.test(meds_clean$duration ~ meds_clean$current_user)
kruskal.test(meds_clean$duration ~ meds_clean$side_effects.x)

meds_clean$se <- ifelse(meds_clean$side_effects.x == 'None', 'No', 'Yes')
meds_clean$se <- as.factor(meds_clean$se)
wilcox.test(meds_clean$duration ~ meds_clean$se)

bmi <- umed_glp1 %>% select(patient_study_identifier, bmi)
bladder_test <- merge(bladder_test, bmi, by='patient_study_identifier')
cor.test(bladder_test$duration, bladder_test$bmi, method='spearman')

scripts <- meds_clean 
scripts$ehr_drug_recorded <- ifelse(is.na(scripts$duration), 'Not recorded', 'EHR prescribed')
scripts <- scripts %>% select(patient_study_identifier, ehr_drug_recorded) %>% unique()

test2 <- merge(scripts, sankey1, by='patient_study_identifier', all=T)
test2$ehr_drug_recorded <- ifelse(is.na(test2$ehr_drug_recorded), 'Not in EHR', test2$ehr_drug_recorded)

scripts2 <- summary(univariateTable(ehr_drug_recorded ~ glp1_used + prescribed, show.totals=TRUE, column.percent = TRUE, compare.groups = TRUE, data=test2))



test3 <- merge(test2[,1:2], sankey4a, by='patient_study_identifier')

scripts3 <- summary(univariateTable(ehr_drug_recorded ~ lose_weight, show.totals=TRUE, column.percent = TRUE, compare.groups = TRUE, data=test3))

drug_bought <- test2 %>% filter(ehr_drug_recorded == 'Not in EHR' & prescribed == 'Bought') %>% select(patient_study_identifier) %>% unique()

#================================================================================
drug_bought$bought_drug <- 'yes'
glp_past_present <- merge(glp_past_present, drug_bought, by='patient_study_identifier', all=T)
glp_past_present$bought_drug <- ifelse(is.na(glp_past_present$bought_drug), 'Not bought', 'Drug bought')
users_comorb <- summary(univariateTable(bought_drug ~ current_user + age_ehr + sex_ehr + obese.x + Hypertension + Diabetes +Hypercholesterolemia + CVD + CHF, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=glp_past_present))

df_user <- merge(df, drug_bought, by='patient_study_identifier', all=T)
df_user$bought_drug <- ifelse(is.na(df_user$bought_drug), 'Not bought', 'Drug bought')

user <- summary(univariateTable(bought_drug ~ current_user + reasons_for_not_up_titrating_dose.x + side_effects.x + changed_daily_activity + what_would_have_helped_to_stay_on_meds_or_raise_dose, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=df_user))



#=========================================================================
#NOTE only Saxenda, Wegovy and Mounjaro are licensed for weight loss! 

#old NICE criteria
#link: https://www.nice.org.uk/guidance/ta875/resources/semaglutide-for-managing-overweight-and-obesity-pdf-82613674831813
#1) BMI >=35 OR BMI>=30 with comorbidities (HTN, dyslipidaemia)
#1a) IF ethnicity = South Asian, Chinese, Middle eastern, Black African, African Carribean --> >=32.5  >=27.5 with comorbs
#2) Failed to achieve/maintain weight loss with lifestyle interventions
#3) prescription via specialist or weight management service 
#recommended drug: liraglutide

#new NICE criteria
#overall guidance on: https://www.nice.org.uk/guidance/ng246/chapter/Medicines-and-surgery#medicines-for-overweight-and-obesity
#NOTE: all have to have reduced intake & increased activity

#Tizepatide:
#guidance from: https://www.england.nhs.uk/wp-content/uploads/2025/03/PRN01879-interim-commissioning-guidance-implementation-of-the-nice-technology-appraisal-ta1026-and-the-NICE-fu.pdf
#&: https://www.nice.org.uk/guidance/ta1026/chapter/1-Recommendations
#BMI >=35 (32.5 if aforementioned ethnic group)
#1 weight related comorbidity
#for funding in GP care at least >=4 of: HTN, dyslipidaemia, OSA, artherosclerotic cardiovascular disease, T2DM & BMI >=40


#Semaglutide
#1) BMI >=35 & 1 comorbidity OR BMI >=30 + refferal for specialist management:
#1a) surgery or medication considered, specalist intervention needed, less intensive medicine unsuccesul, undelying cause needs to be assessed, complex disease
#2) Failed to achieve/maintain weight loss with lifestyle intervention
#3) prescription via specialist or weight management service
#reccomended drug: semaglutide (liraglutide if semaglutide not available/tollerated)


#liraglutide
#BMI >=35 (or 32.5 if specific ethnic group) & non-diabetic hyperglycaemia
#high CVD risk (HTN, dyslipidaemia)
#needs secondary care script

#==========================================================================
#EdB continued here 16/09/25
#visualise use of licensed drugs
summary(as.factor(sankey1$GLP1_drugs))

glpq$drug_indicated_for_weight <- ifelse(glpq$Q4_GLP1_NAMES__Weg == 'TRUE'| glpq$Q4_GLP1_NAMES__Zep == 'TRUE'| glpq$Q4_GLP1_NAMES__Ozem == 'TRUE' | glpq$Q4_GLP1_NAMES__Mounj == 'TRUE' | glpq$Q4_GLP1_NAMES__Sax == 'TRUE', 'GLP1 drug licensed for weight loss', 'GLP1 not licensed for weight loss')

sankey5 <- glpq %>% select(patient_study_identifier, glp1_used, reason_for_use, drug_indicated_for_weight) %>% filter(glp1_used == 'GLP1 used')


#1099 retained
lab_tab <- as.data.frame(table(sankey5$reason_for_use, sankey5$drug_indicated_for_weight))

names(lab_tab)[[1]] <-"Reason for using GLP1"
names(lab_tab)[[2]] <-"Drug indicated for weight loss"
names(lab_tab)[[3]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:2,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - license") +
  theme_pubr() +
  theme(legend.position = "none")

#start with Moujaro
summary(as.factor(umed_glp1$ethnicity_group_16_ehr))
ethnicity <- umed_glp1 %>% filter(ethnicity_group_16_ehr %in% c('Asian or Asian British - Indian','Black or Black British - African','Asian or Asian British - Any other Asian background','Black or Black British - Caribbean','Asian or Asian British - Bangladeshi','Asian or Asian British - Pakistani','Black or Black British - Any other Black background','Other Ethnic Groups - Chinese')) %>% select(patient_study_identifier)
ethnicity <- unique(ethnicity)

#assume that if discussed = is done
glpq$participating_in_lifestyle_intervention <- ifelse(glpq$Q2_WT_SUPPORT__Exercise == 'TRUE' & glpq$Q2_WT_SUPPORT__Healthy_eating == 'TRUE' | glpq$Q2_WT_SUPPORT__Wt_loss_grps == 'TRUE', 'Lifestyle criteria satisfied', 'Lifestyle not discussed according to guideline')

#NOTE: we need coronary artery disease, not included CHF yet (as not specified in NICE guideline)
Umed_patients_cardiovasc <- readRDS("~/PhD/myfilepath/Umed_patients_cardiovasc.rds")
Umed_patients_cerebro <- readRDS("~/PhD/myfilepath/Umed_patients_cerebro.rds")
Umed_patients_heartfailure <- readRDS("~/PhD/myfilepath/Umed_patients_heartfailure.rds")
Umed_patients_OSA <- readRDS("~/PhD/myfilepath/Umed_patients_OSA.rds")
Umed_patients_T2DM <- readRDS("~/PhD/myfilepath/Umed_patients_T2DM.rds")
Umed_patients_dyslip <- readRDS("~/PhD/myfilepath/Umed_patients_dyslip.rds")
Umed_patients_HTN <- readRDS("~/PhD/myfilepath/Umed_patients_HTN.rds")


diag$cardiovasc <- ifelse(diag$patient_study_identifier %in% Umed_patients_cardiovasc$patient_study_identifier, 'Yes', 'no')
diag$osa <- ifelse(diag$patient_study_identifier %in% Umed_patients_OSA$patient_study_identifier, 'Yes', 'no')
diag$cerebrovasc_snomed <- ifelse(diag$patient_study_identifier %in% Umed_patients_cerebro$patient_study_identifier, 'Yes', 'no')
diag$hf_snomed <- ifelse(diag$patient_study_identifier %in% Umed_patients_heartfailure$patient_study_identifier, 'Yes', 'no')
diag$T2DM <- ifelse(diag$patient_study_identifier %in% Umed_patients_T2DM$patient_study_identifier, 'Yes', 'no')
diag$HTN <- ifelse(diag$patient_study_identifier %in% Umed_patients_HTN$patient_study_identifier, 'Yes', 'no')
diag$dyslip <- ifelse(diag$patient_study_identifier %in% Umed_patients_dyslip$patient_study_identifier, 'Yes', 'no')


diag$number_of_comorbs <- NA
diag$number_of_comorbs <- ifelse(diag$HTN == 'Yes', 1, 0)
diag$number_of_comorbs <- ifelse(diag$T2DM == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$dyslip == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$cardiovasc == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$osa == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)

four_comorbs <- diag %>% filter(number_of_comorbs >=4) %>% select(patient_study_identifier) %>% unique()
three_comorbs <- diag %>% filter(number_of_comorbs >=3) %>% select(patient_study_identifier) %>% unique()
one_or_more_comorbs <- diag %>% filter(number_of_comorbs >=1) %>% select(patient_study_identifier) %>% unique()

glpq$refferal <- ifelse(glpq$Q2_WT_SUPPORT__Med == 'TRUE' | glpq$Q2_WT_SUPPORT__Wt_loss_surg == 'TRUE', 'Refferal indicated', 'No refferal indicated')
htn_or_dyslip <- diag %>% filter(HTN == 'Yes' | dyslip == 'Yes') %>% select(patient_study_identifier) %>% unique()

no_dm <- diag %>% filter(Diabetes != 'TRUE') %>% select(patient_study_identifier) %>% unique()
glpq$use_for_blood_sugar_but_no_DM <- ifelse(glpq$reason_for_use %in% c('Blood sugar', 'Both blood sugar and weight') & glpq$patient_study_identifier %in% no_dm$patient_study_identifier, 'Hyperglycaemia without DM', 'No hyperglycaemia without DM')
summary(as.factor(glpq$use_for_blood_sugar_but_no_DM))


diag$htn_overlap <- diag$HTN == diag$Hypertension
diag$dyslip_overlap <- diag$dyslip == diag$Hypercholesterolemia
#==================================================================================================
#data is prepared, do checks 
glpq$BMI_for_tirzepatide_specialist <- ifelse(glpq$bmi >=35 | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')
glpq$BMI_for_tirzepatide_GP <- ifelse(glpq$bmi >=40 | glpq$bmi >=37.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')
glpq$BMI_for_tirzepatide_GP_phase2 <- ifelse(glpq$bmi >=35 | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')

glpq$comorbs_for_tirzepatide_specialist <- ifelse(glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier, 'yes', 'no')
glpq$comorbs_for_tirzepatide_gp <- ifelse(glpq$patient_study_identifier %in% four_comorbs$patient_study_identifier, 'yes', 'no')
glpq$comorbs_for_tirzepatide_gp_phase3 <- ifelse(glpq$patient_study_identifier %in% three_comorbs$patient_study_identifier, 'yes', 'no')

glpq$Tirzepatide_specialist_indicated <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_tirzepatide_specialist == 'BMI high enough' & glpq$comorbs_for_tirzepatide_specialist == 'yes', 'TRUE', 'Secondary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase2 <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_tirzepatide_GP_phase2 == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase3 <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp_phase3 == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')

glpq$Tirzepatide_GP_indicated_no_lifestyle <- ifelse(glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase2_no_lifestyle <- ifelse(glpq$BMI_for_tirzepatide_GP_phase2 == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase3_no_lifestyle <- ifelse(glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp_phase3 == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')

Patients_on_3_antidiabetics_incl_metformin <- readRDS("~/PhD/Projects/myfilepath/Patients_on_3_antidiabetics_incl_metformin.rds")

glpq$Tirzepatide_T2DM <- ifelse(glpq$BMI_for_tirzepatide_specialist == 'BMI high enough' & glpq$patient_study_identifier %in% Patients_on_3_antidiabetics_incl_metformin$patient_study_identifier, 'TRUE', 'T2DM Tirzepatide not indicated')
summary(as.factor(glpq$Tirzepatide_T2DM))
dm_glp <- glpq %>% filter(Tirzepatide_T2DM == 'TRUE') %>% select(patient_study_identifier) %>% unique()


#Add MHRA license criteria for Tirzepatide for obesity 
#from: https://mhraproducts4853.blob.core.windows.net/docs/8729e32b84978867881a40ec535bef40c9877ce8
#liraglutide & semaglutide only seem to have MHRA information on T2DM (?)
#from: https://mhraproducts4853.blob.core.windows.net/docs/c5257911ba77a5bf9dd81163de9d07c406e48720
#https://mhraproducts4853.blob.core.windows.net/docs/4a176520080ef5ac905594dc34f5727651d0ecef
#Wegovy is licensed for weight management if BMI >=30 OR >=27 + 1 weight related comorb
#Tirzepatide is licensed for weight management if BMI>=30 or >=27 +1 weight related comrob

glpq$MHRA_tirzepatide_or_semaglutide <- ifelse(glpq$bmi >=30 | glpq$bmi >=27 & glpq$patient_study_identifier %in% one_or_more_comorbs, 'MHRA license indication', 'No MHRA license indication')
#============================================================================
#move on to semaglutide
glpq$BMI_for_semaglutide_specialist <- ifelse(glpq$bmi >=35 & glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier & glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier | glpq$bmi >=27.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier & glpq$refferal == 'Refferal indicated' | glpq$bmi >=30 & glpq$refferal == 'Refferal indicated', 'BMI high enough', 'BMI not high enough')
glpq$semaglutide_specialist_indicated <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_semaglutide_specialist == 'BMI high enough', 'TRUE', 'Secondary care Semaglutide not indicated')

summary(as.factor(glpq$semaglutide_specialist_indicated))
#now liraglutide
glpq$BMI_for_liraglutide_specialist <- ifelse(glpq$use_for_blood_sugar_but_no_DM == 'Hyperglycaemia without DM' & glpq$patient_study_identifier %in% htn_or_dyslip$patient_study_identifier & (glpq$bmi >= 35 |(glpq$bmi >= 32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier)),"BMI high enough", "BMI not high enough")
glpq$liraglutide_specialist_indicated <- ifelse(glpq$participating_in_lifestyle_intervention == 'Lifestyle criteria satisfied' & glpq$BMI_for_liraglutide_specialist == 'BMI high enough', 'TRUE', 'Secondary care Liraglutide not indicated')

summary(as.factor(glpq$BMI_for_liraglutide_specialist))
summary(as.factor(glpq$liraglutide_specialist_indicated))

#get one col for indication
indication <- c(
  liraglutide_specialist_indicated   = "Liraglutide (specialist)",
  semaglutide_specialist_indicated    = "Semaglutide (specialist)",
  Tirzepatide_GP_indicated  = "Tirzepatide (GP)",
  Tirzepatide_GP_indicated_phase2  = "Tirzepatide (GP) - phase 2",
  Tirzepatide_GP_indicated_phase3  = "Tirzepatide (GP) - phase 3",
  Tirzepatide_specialist_indicated= "Tirzepatide (specialist)"
)

#Run loop now
glpq$indication <- apply(glpq, 1, function(row) {
  active <- names(indication)[row[names(indication)] == "TRUE"]
  if (length(active) == 0) {
    return(NA)
  } else {
    return(paste(indication[active], collapse = "; "))
  }
})

summary(as.factor(glpq$indication))

glpq$indication <- ifelse(glpq$indication %in% c('NA; NA','NA; NA','NA', 'NA; NA; NA', 'NA; NA; NA; NA', 'NA; NA; NA; NA; NA; NA ', 'NA; NA; NA; NA; NA', 'NA; NA; NA; NA; NA; NA'), NA, glpq$indication)
glpq$indication <- ifelse(is.na(glpq$indication), 'No NICE indication', glpq$indication)


#now plot 
glpq$indication <- ifelse(glpq$indication %in% c("Liraglutide (specialist); Semaglutide (specialist); Tirzepatide (GP) - phase 3; Tirzepatide (specialist)", "Semaglutide (specialist); Tirzepatide (GP) - phase 3; Tirzepatide (specialist)"), 'Stage 3 GP Tirzepatide & specialist drugs', glpq$indication)
glpq$indication <- ifelse(glpq$indication %in% c("Semaglutide (specialist); Tirzepatide (GP) - phase 2; Tirzepatide (specialist)", "Liraglutide (specialist); Semaglutide (specialist); Tirzepatide (GP) - phase 2; Tirzepatide (specialist)"), 'Stage 2 GP Tirzepatide & specialist drugs', glpq$indication)
glpq$indication <- ifelse(glpq$indication %in% c("Semaglutide (specialist); Tirzepatide (GP); Tirzepatide (GP) - phase 2; Tirzepatide (GP) - phase 3; Tirzepatide (specialist)", "Liraglutide (specialist); Semaglutide (specialist); Tirzepatide (GP); Tirzepatide (GP) - phase 2; Tirzepatide (GP) - phase 3; Tirzepatide (specialist)"),'All phases or phase 2/3 of GP Tirzepatide & specialist drugs' ,glpq$indication)

#order the plot
glpq$indication <- factor(glpq$indication, levels = c('All phases or phase 2/3 of GP Tirzepatide & specialist drugs', 
                                                      'Stage 2 GP Tirzepatide & specialist drugs',
                                                      'Stage 3 GP Tirzepatide & specialist drugs',
                                                      "Liraglutide (specialist); Semaglutide (specialist); Tirzepatide (specialist)",
                                                      "Semaglutide (specialist); Tirzepatide (specialist)", 
                                                      "Semaglutide (specialist)",
                                                      "No NICE indication"))
glpq$glp1_used <- factor(glpq$glp1_used,levels = c("GLP1 used", "No GLP1 used"))
glpq$obese <- factor(glpq$obese,levels = c("Obese", "Not Obese"))

summary(as.factor(glpq$indication))

glpq_obese <- glpq %>% filter(obese == 'Obese')
glpq_obese$reason_for_use <- ifelse(is.na(glpq_obese$reason_for_use) & glpq_obese$glp1_used == 'NO GLP1 used', 'not used', glpq_obese$reason_for_use)
glpq_obese$reason_for_use <- ifelse(is.na(glpq_obese$reason_for_use), 'not answered', glpq_obese$reason_for_use)

lab_tab <- as.data.frame(table(glpq_obese$obese, glpq_obese$reason_for_use, glpq_obese$indication, glpq_obese$glp1_used))

names(lab_tab)[[1]] <-"Obese"
names(lab_tab)[[2]] <-"Reason for use"
names(lab_tab)[[3]] <-"NICE indication"
names(lab_tab)[[4]] <-"GLP1 used"
names(lab_tab)[[5]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:4,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 questionnaire results") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - NICE indication") +
  theme_pubr() +
  theme(legend.position = "none")

summary(as.factor(glpq$indication))

glpq$indication_overall <- ifelse(glpq$indication == 'No NICE indication', 'No NICE indication', 'NICE indication')

chisq.test(glpq$indication, glpq$glp1_used)
chisq.test(glpq$indication_overall, glpq$glp1_used)



#=============================================================
#Sankey for those with indication
indic <- glpq %>% select(patient_study_identifier, Tirzepatide_GP_indicated, Tirzepatide_GP_indicated_phase2, Tirzepatide_GP_indicated_phase3, Tirzepatide_GP_indicated_no_lifestyle, Tirzepatide_GP_indicated_phase2_no_lifestyle, Tirzepatide_GP_indicated_phase3_no_lifestyle, Tirzepatide_specialist_indicated, semaglutide_specialist_indicated, liraglutide_specialist_indicated, glp1_used)

indic <-indic %>% filter(Tirzepatide_GP_indicated_no_lifestyle == 'TRUE' | Tirzepatide_GP_indicated_phase2_no_lifestyle == 'TRUE' | Tirzepatide_GP_indicated_phase3_no_lifestyle == 'TRUE')

lab_tab <- as.data.frame(table(indic$glp1_used, indic$Tirzepatide_GP_indicated, indic$Tirzepatide_GP_indicated_phase2, indic$Tirzepatide_GP_indicated_phase3, indic$Tirzepatide_specialist_indicated, indic$liraglutide_specialist_indicated, indic$semaglutide_specialist_indicated))

names(lab_tab)[[1]] <-"GLP1 used"
names(lab_tab)[[2]] <-"Phase 1 GP Tirzepatide"
names(lab_tab)[[3]] <-"Phase 2 GP Tirzepatide"
names(lab_tab)[[4]] <-"Phase 3 GP Tirzepatide"
names(lab_tab)[[5]] <-"Specialist Tirzepatide"
names(lab_tab)[[6]] <-"Specialist Liraglutide"
names(lab_tab)[[7]] <-"Specialist Semaglutide"
names(lab_tab)[[8]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:7,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - with lifestyle requirement") +
  theme_pubr() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))


lab_tab <- as.data.frame(table(indic$glp1_used, indic$Tirzepatide_GP_indicated_no_lifestyle, indic$Tirzepatide_GP_indicated_phase2_no_lifestyle, indic$Tirzepatide_GP_indicated_phase3_no_lifestyle, indic$Tirzepatide_specialist_indicated, indic$liraglutide_specialist_indicated, indic$semaglutide_specialist_indicated))

names(lab_tab)[[1]] <-"GLP1 used"
names(lab_tab)[[2]] <-"Phase 1 GP Tirzepatide"
names(lab_tab)[[3]] <-"Phase 2 GP Tirzepatide"
names(lab_tab)[[4]] <-"Phase 3 GP Tirzepatide"
names(lab_tab)[[5]] <-"Specialist Tirzepatide"
names(lab_tab)[[6]] <-"Specialist Liraglutide"
names(lab_tab)[[7]] <-"Specialist Semaglutide"
names(lab_tab)[[8]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:7,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - without lifestyle requirement") +
  theme_pubr() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))
#==============================================================================
#EdB continued here on 17/09/25
more_gp_support <- glpq %>% select(patient_study_identifier, Q10_TITRATION_BARRIERS__Instructions, instructions_for_raising_dose, Q21_SUPPORT_NEEDED__Instructions, Q21_SUPPORT_NEEDED__Help_from_Dr, glp1_used) %>% filter(glp1_used == 'GLP1 used')
sup1 <- merge(more_gp_support, glp_past_present, by='patient_study_identifier')

sup_bl <- summary(univariateTable(Q21_SUPPORT_NEEDED__Help_from_Dr ~ bought_drug + current_user + age_ehr + sex_ehr + obese.x + Hypertension + Diabetes +Hypercholesterolemia + CVD + CHF, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=sup1))

sup2 <- merge(df, sup1,by='patient_study_identifier')

user1a <- summary(univariateTable(Q21_SUPPORT_NEEDED__Help_from_Dr ~ prescribed.x.x + reasons_for_not_up_titrating_dose.x.x + side_effects.x.x + lose_weight.x + changed_daily_activity.x + spent_less_money_on_food.x + what_would_have_helped_to_stay_on_meds_or_raise_dose.x, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=sup2))
bladder1a <- summary(univariateTable(Q21_SUPPORT_NEEDED__Help_from_Dr ~ blader_control.x + frequency_of_bladder_issues.x + bladder_control_improved_with_GLP_weight_loss.x, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, data=sup2))
#===================================================================================
#EdB continued here on 24 sept 2025
diag$cvd_agree <- diag$CVD == diag$cerebrovasc_snomed
diag$hf_agree <- diag$CHF == diag$hf_snomed
summary(diag$cvd_agree) 
5575/6049
summary(diag$hf_agree)
6025/6049

#do some grouping
tab1 <- summary(univariateTable(Tirzepatide_GP_indicated ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))
tab2 <- summary(univariateTable(Tirzepatide_GP_indicated_phase2 ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))
tab3 <- summary(univariateTable(Tirzepatide_GP_indicated_phase3 ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))
tab1a <- summary(univariateTable(Tirzepatide_GP_indicated_no_lifestyle ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))
tab2a <- summary(univariateTable(Tirzepatide_GP_indicated_phase2_no_lifestyle ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))
tab3a <- summary(univariateTable(Tirzepatide_GP_indicated_phase3_no_lifestyle ~ glp1_used + Tirzepatide_specialist_indicated + liraglutide_specialist_indicated + semaglutide_specialist_indicated, data=glpq))

summary(as.factor(glpq$obese))
strict <- glpq %>% filter(Tirzepatide_GP_indicated == 'TRUE' | Tirzepatide_GP_indicated_phase2 == 'TRUE' | Tirzepatide_GP_indicated_phase3 == 'TRUE')
strict2 <- glpq %>% filter(Tirzepatide_GP_indicated_no_lifestyle == 'TRUE' | Tirzepatide_GP_indicated_phase2_no_lifestyle == 'TRUE' | Tirzepatide_GP_indicated_phase3_no_lifestyle == 'TRUE')

#MHRA table
mhra_df <- glpq %>% filter(!is.na(MHRA_tirzepatide_or_semaglutide))
mhra <- summary(univariateTable(MHRA_tirzepatide_or_semaglutide ~ glp1_used + Tirzepatide_GP_indicated_no_lifestyle + Tirzepatide_GP_indicated_phase2_no_lifestyle + Tirzepatide_GP_indicated_phase3_no_lifestyle + Tirzepatide_T2DM + Tirzepatide_specialist_indicated + semaglutide_specialist_indicated + liraglutide_specialist_indicated, data=mhra_df, compare.groups = TRUE, show.totals = TRUE))
write.csv2(mhra, file='grouping_by_MHRA_license.csv')

#check diabetes NICE indicatin:
#from: https://cks.nice.org.uk/topics/diabetes-type-2/management/management-adults/#antidiabetic-drugs
#GLP1 IF: triple therapy (which has to include metformin) ineffective/not tollerated/contraindicated
#AND BMI >=35 (or >=32.5 if BAME) OR insulin would have serious occupational implications

glpq$any_specialist_indication <- ifelse(glpq$Tirzepatide_specialist_indicated == 'TRUE' | glpq$liraglutide_specialist_indicated == 'TRUE' | glpq$semaglutide_specialist_indicated == 'TRUE' | glpq$Tirzepatide_T2DM == 'TRUE', 'Specialist indication', 'No specialist indication')

glpq$Tirzepatide_specialist_indicated_nl <- ifelse(glpq$BMI_for_tirzepatide_specialist == 'BMI high enough' & glpq$comorbs_for_tirzepatide_specialist == 'yes', 'TRUE', 'Secondary care Tirzepatide not indicated')
glpq$semaglutide_specialist_indicated_nl <- ifelse(glpq$BMI_for_semaglutide_specialist == 'BMI high enough', 'TRUE', 'Secondary care Semaglutide not indicated')
glpq$liraglutide_specialist_indicated_nl <- ifelse(glpq$BMI_for_liraglutide_specialist == 'BMI high enough', 'TRUE', 'Secondary care Liraglutide not indicated')

glpq$any_specialist_indication_nl <- ifelse(glpq$Tirzepatide_specialist_indicated_nl == 'TRUE' | glpq$liraglutide_specialist_indicated_nl == 'TRUE' | glpq$semaglutide_specialist_indicated_nl == 'TRUE' | glpq$Tirzepatide_T2DM == 'TRUE', 'Specialist indication', 'No specialist indication')

ph1 <- glpq %>% filter(Tirzepatide_GP_indicated_no_lifestyle == 'TRUE')
summary(as.factor(ph1$glp1_used))
summary(as.factor(ph1$prescribed))
summary(as.factor(ph1$any_specialist_indication_nl))

ph2 <- glpq %>% filter(Tirzepatide_GP_indicated_phase2_no_lifestyle == 'TRUE')
summary(as.factor(ph2$glp1_used))
summary(as.factor(ph2$prescribed))
summary(as.factor(ph2$any_specialist_indication_nl))

ph3 <- glpq %>% filter(Tirzepatide_GP_indicated_phase3_no_lifestyle == 'TRUE')
summary(as.factor(ph3$glp1_used))
summary(as.factor(ph3$prescribed))
summary(as.factor(ph3$any_specialist_indication_nl))

glpq$any_gp_indication_nl <- ifelse(glpq$Tirzepatide_GP_indicated_no_lifestyle == 'TRUE' | glpq$Tirzepatide_GP_indicated_phase2_no_lifestyle == 'TRUE' | glpq$Tirzepatide_GP_indicated_phase3_no_lifestyle == 'TRUE', 'GP indication', 'No indication')
ph4<- glpq %>% filter(any_gp_indication_nl == 'GP indication')
summary(as.factor(ph4$glp1_used))
summary(as.factor(ph4$any_specialist_indication_nl))



ph1 <- glpq %>% filter(Tirzepatide_GP_indicated == 'TRUE')
summary(as.factor(ph1$glp1_used))
summary(as.factor(ph1$any_specialist_indication))

ph2 <- glpq %>% filter(Tirzepatide_GP_indicated_phase2 == 'TRUE')
summary(as.factor(ph2$glp1_used))
summary(as.factor(ph2$any_specialist_indication))

ph3 <- glpq %>% filter(Tirzepatide_GP_indicated_phase3 == 'TRUE')
summary(as.factor(ph3$glp1_used))
summary(as.factor(ph3$any_specialist_indication))

glpq$any_gp_indication <- ifelse(glpq$Tirzepatide_GP_indicated == 'TRUE' | glpq$Tirzepatide_GP_indicated_phase2 == 'TRUE' | glpq$Tirzepatide_GP_indicated_phase3 == 'TRUE', 'GP indication', 'No indication')
ph4<- glpq %>% filter(any_gp_indication == 'GP indication')
summary(as.factor(ph4$glp1_used))
summary(as.factor(ph4$any_specialist_indication))

#=================================================================
#IMD codes
ACMD_Consented_Patient_IMD_Rank_09_19_2025_ <- read_excel("ACMD Consented Patient_IMD Rank (09.19.2025).xlsx")
imd <- ACMD_Consented_Patient_IMD_Rank_09_19_2025_
conv <- umed_glp1 %>% select(patientguid, patient_study_identifier)

GP_glp <- glpq %>% select(patient_study_identifier, Tirzepatide_GP_indicated_no_lifestyle, Tirzepatide_GP_indicated_phase2_no_lifestyle, Tirzepatide_GP_indicated_phase3_no_lifestyle, obese, bmi, MHRA_tirzepatide_or_semaglutide)
GP_glp$Tirzepatide_via_GP <- ifelse(GP_glp$Tirzepatide_GP_indicated_no_lifestyle == 'TRUE' | GP_glp$Tirzepatide_GP_indicated_phase2_no_lifestyle == 'TRUE' | GP_glp$Tirzepatide_GP_indicated_phase3_no_lifestyle == 'TRUE', 'GP indiciation for Tirzepatide', 'No GP indication for Tirzepatide')

imd <- merge(imd, conv, by='patientguid')
imd2 <- imd
imd_table <- imd2

imd <- merge(GP_glp, imd, by='patient_study_identifier')
glp_imd <- merge(glpq, imd2, by='patient_study_identifier')

imd$bmi_class <- NA
imd$bmi_class <- ifelse(imd$bmi < 18.5, 'underweight', 'normal weight')
imd$bmi_class <- ifelse(imd$bmi >= 25 & imd$bmi <30, 'overweight', imd$bmi_class)
imd$bmi_class <- ifelse(imd$bmi >=30 & imd$bmi <35, 'Obesity class I', imd$bmi_class)
imd$bmi_class <- ifelse(imd$bmi >=35 & imd$bmi <40, 'Obesity class II', imd$bmi_class)
imd$bmi_class <- ifelse(imd$bmi >=40, 'Obesity class III', imd$bmi_class)

hist(log(glp_imd$bmi))
#log tranformation normalises BMI
#see if IMD rank can be normalised
hist(glp_imd$`Index of Multiple Deprivation Rank`, breaks=150)
hist(log(glp_imd$`Index of Multiple Deprivation Rank` + 10), breaks=150)
glp_imd$log_imd <- log(glp_imd$`Index of Multiple Deprivation Rank` + 10)
hist(scale(glp_imd$log_imd), breaks=150)
hist(rank(glp_imd$log_imd), breaks=350)
glp_imd$log_bmi <- log(glp_imd$bmi)
plot(glp_imd$log_bmi, glp_imd$log_imd)
plot(glp_imd$log_bmi, glp_imd$`Index of Multiple Deprivation Rank`)

#so, try a LOESS model to residualise
glp_imd2 <- glp_imd %>% filter(!is.na(log_bmi) & !is.na(log_imd))

loess_fit <- loess(`Index of Multiple Deprivation Rank` ~ log_bmi, data = glp_imd2)
glp_imd2$imd_residuals <- loess_fit$residuals
plot(glp_imd2$log_bmi, glp_imd2$imd_residuals)

#also try with normalising IMD
glp_imd2$imd_norm=qnorm((glp_imd2$`Index of Multiple Deprivation Rank` +1)/(max(glp_imd2$`Index of Multiple Deprivation Rank`)+2))

loess_fit2 <- loess(imd_norm ~ log_bmi, data = glp_imd2)
glp_imd2$imd_residuals_norm <- loess_fit2$residuals

cor.test(glp_imd2$bmi, glp_imd2$imd_residuals_norm, method='spearman')

cor3 <- ggplot(glp_imd2, aes(x=imd_residuals_norm, y=bmi)) +
  geom_point() +
  xlab('residualised IMD') +
  ylab('BMI (kg/m^2') +
  ggtitle('Correlation of residualised IMD and BMI after normalisation') +
  theme_pubr()
#########################################################################3

cor.test(glp_imd2$bmi, glp_imd2$imd_residuals, method='spearman')
cor.test(glp_imd2$bmi, glp_imd2$`Index of Multiple Deprivation Rank`, method='spearman')


cor1 <- ggplot(glp_imd2, aes(x=`Index of Multiple Deprivation Rank`, y=bmi)) +
  geom_point() +
  xlab('IMD rank') +
  ylab('BMI (kg/m^2') +
  ggtitle('Correlation of IMD and BMI') +
  theme_pubr()

cor2 <- ggplot(glp_imd2, aes(x=imd_residuals, y=bmi)) +
  geom_point() +
  xlab('residualised IMD') +
  ylab('BMI (kg/m^2') +
  ggtitle('Correlation of residualised IMD and BMI') +
  theme_pubr()

ggarrange(cor1, cor3, ncol=2)

ggplot(imd, aes(x=obese, y=`Index of Multiple Deprivation Rank`, fill=obese)) +
  geom_boxplot() +
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD for obesity group') +
  xlab('Obese') +
  ylab('IMD rank') +
  theme_pubr()

ggplot(imd, aes(x=bmi_class, y=`Index of Multiple Deprivation Rank`, fill=bmi_class)) +
  geom_boxplot() +
  stat_compare_means(method='kruskal.test') +
  ggtitle('IMD for obesity group') +
  xlab('BMI group') +
  ylab('IMD rank') +
  theme_pubr()


imd_obese <- imd %>% filter(obese == 'Obese')
ggplot(imd_obese, aes(x=Tirzepatide_via_GP, y=`Index of Multiple Deprivation Rank`, fill=Tirzepatide_via_GP)) +
  geom_boxplot() +
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD for GP indication for Tirzepatide') +
  xlab('GP indication for Tirzepatide') +
  ylab('IMD rank') +
  theme_pubr()

ggplot(imd_obese, aes(x=MHRA_tirzepatide_or_semaglutide, y=`Index of Multiple Deprivation Rank`, fill=MHRA_tirzepatide_or_semaglutide)) +
  geom_boxplot() +
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD for MHRA GLP1 license') +
  xlab('MHRA GLP1 license indication') +
  ylab('IMD rank') +
  theme_pubr()

ggplot(imd, aes(x=MHRA_tirzepatide_or_semaglutide, y=`Index of Multiple Deprivation Rank`, fill=MHRA_tirzepatide_or_semaglutide)) +
  geom_boxplot() +
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD for MHRA GLP1 license') +
  xlab('MHRA GLP1 license indication') +
  ylab('IMD rank') +
  theme_pubr()

#==========================================================================================
#make sure NA is actual NA for IMD comparison
glp_imd[, 1:98] <- glp_imd[, 1:98] %>% mutate(across(everything(), ~ na_if(as.character(.), "NA")))
glp_imd[, 1:98] <- glp_imd[, 1:98] %>% mutate(across(everything(), ~ na_if(as.character(.), "lost")))

glp_imd <- glp_imd[,c(1,107:131,4:10,165)]
glp_imd[, 4:33] <- lapply(glp_imd[, 4:33], function(x) {
  if (is.character(x)) x[x == "NA"] <- NA
  x})

glp_imd$number_of_GLP1_drugs <- as.factor(glp_imd$number_of_GLP1_drugs)

# Clean NAs safely
glp_imd <- glp_imd %>%
  mutate(across(
    where(~is.character(.) || is.factor(.)),
    ~na_if(as.character(.), "NA")
  )) %>%
  mutate(across(
    where(~is.character(.) || is.factor(.)),
    ~na_if(as.character(.), "lost")
  ))

adj_imd <- glp_imd2 %>% select(patient_study_identifier, imd_residuals_norm)
glp_imd <- merge(glp_imd, adj_imd, by='patient_study_identifier')
# Ensure numeric IMD column
glp_imd[[35]] <- as.numeric(as.character(glp_imd[[35]]))
glp_imd <- glp_imd[,-34]

imd_results <- data.frame(
  variable = character(),
  test_type = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (col in setdiff(1:ncol(glp_imd), c(1,3, 34))) {
  
  var_name <- names(glp_imd)[col]
  var_data <- glp_imd[[col]]
  
  # Ensure IMD rank is numeric
  imd_rank <- as.numeric(glp_imd[[34]])
  
  # Skip if variable is all NA or constant
  if (length(unique(na.omit(var_data))) < 2) next
  
  # Drop NAs
  df_sub <- data.frame(
    var = var_data,
    imd = imd_rank
  ) %>% na.omit()
  
  if (nrow(df_sub) < 3) next
  
  # Decide which test to run
  if (is.numeric(df_sub$var)) {
    # Spearman correlation for numeric data
    test_type <- "Spearman correlation"
    p_val <- tryCatch({
      cor.test(df_sub$var, df_sub$imd, method = "spearman")$p.value
    }, error = function(e) NA)
    
  } else if (length(unique(df_sub$var)) == 2) {
    # Wilcoxon for binary categorical data
    test_type <- "Wilcoxon rank-sum"
    p_val <- tryCatch({
      wilcox.test(imd ~ var, data = df_sub)$p.value
    }, error = function(e) NA)
    
  } else {
    # Kruskal–Wallis for categorical with >2 levels
    test_type <- "Kruskal–Wallis"
    p_val <- tryCatch({
      kruskal.test(imd ~ var, data = df_sub)$p.value
    }, error = function(e) NA)
  }
  
  # Store results
  imd_results <- rbind(
    imd_results,
    data.frame(variable = var_name, test_type = test_type, p_value = p_val)
  )
}

imd_results$fdr <- p.adjust(imd_results$p_value, method='fdr')
imd_results$sig <- ifelse(imd_results$fdr <0.05, 'Significant', 'NS')

glp_imd$imd_quantile <- cut(
  glp_imd$imd_residuals_norm,
  breaks = quantile(glp_imd$imd_residuals_norm, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4"))
summary(glp_imd$imd_quantile)
#now do chi-squares

glp_imd$any_side_effects <- ifelse(glp_imd$side_effects == 'None', 'no', 'yes')
glp_imd$any_side_effects <- ifelse(glp_imd$glp1_used == 'No GLP1 used', NA, glp_imd$any_side_effects)

imd_results <- data.frame(
  variable = character(),
  test_type = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (col in setdiff(1:ncol(glp_imd), c(1, 3,5,8,14,17, 21,22,23,24,25, 32, 33, 34, 35))) {
  
  var_name <- names(glp_imd)[col]
  var_data <- glp_imd[[col]]
  
  # IMD quantiles must be a factor
  imd_q <- as.factor(glp_imd$imd_quantile)
  
  # Skip variables that are all NA or constant
  if (length(unique(na.omit(var_data))) < 2) next
  
  # Drop NAs
  df_sub <- data.frame(
    var = var_data,
    imd_q = imd_q
  ) %>% na.omit()
  
  if (nrow(df_sub) < 3) next
  
  # Only run chi-square if variable is categorical
  if (is.numeric(df_sub$var)) {
    # skip numeric variables entirely
    next
  }
  
  # Make sure categorical variables are factors
  df_sub$var <- as.factor(df_sub$var)
  
  test_type <- "Chi-square"
  p_val <- tryCatch({
    chisq.test(table(df_sub$var, df_sub$imd_q))$p.value
  }, error = function(e) NA)
  
  imd_results <- rbind(
    imd_results,
    data.frame(variable = var_name, test_type = test_type, p_value = p_val)
  )
}

imd_results$fdr <- p.adjust(imd_results$p_value, method='fdr')
imd_results$sig <- ifelse(imd_results$fdr <0.05, 'Significant', 'NS')

supl_tab <-  summary(univariateTable(imd_quantile ~ discussion_about_weight  + Q2_WT_SUPPORT__Healthy_eating + Q2_WT_SUPPORT__Exercise + Q2_WT_SUPPORT__Med + Q2_WT_SUPPORT__Wt_loss_surg + Q2_WT_SUPPORT__Wt_loss_grps + glp1_used + prescribed + number_of_GLP1_drugs + reason_for_use + current_user + nervous_about_talking_to_dr + dose_titration + experience_with_raising_dose + instructions_for_raising_dose  + lose_weight + spent_less_money_on_food + changed_daily_activity + what_would_make_unsure_if_medication_offered_again + any_side_effects, data=glp_imd, show.totals = TRUE, compare.groups = FALSE, column.percent = TRUE))
supl_tab$col_number <- 1:nrow(supl_tab)

#now merge with data for FDR
supl_tab <- merge(supl_tab, imd_results, by.x='Variable', by.y='variable', all=T)
supl_tab <- supl_tab[order(supl_tab$col_number), ]
write.csv2(supl_tab, file='qunatiled_IMD_for_questions.csv')

#summarise percentage of each reply here
vars <- c(
  "discussion_about_weight",
  "Q2_WT_SUPPORT__Healthy_eating",
  "Q2_WT_SUPPORT__Exercise",
  "Q2_WT_SUPPORT__Med",
  "Q2_WT_SUPPORT__Wt_loss_surg",
  "Q2_WT_SUPPORT__Wt_loss_grps",
  "glp1_used",
  "prescribed",
  "number_of_GLP1_drugs",
  "reason_for_use",
  "current_user",
  "nervous_about_talking_to_dr",
  "dose_titration",
  "experience_with_raising_dose",
  "instructions_for_raising_dose",
  "lose_weight",
  "spent_less_money_on_food",
  "changed_daily_activity",
  "what_would_make_unsure_if_medication_offered_again",
  "any_side_effects"
)


imd_for_plot <- glp_imd %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "response"
  ) %>%
  drop_na(response) %>%          # <-- remove NAs here
  group_by(imd_quantile, variable, response) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

imd_for_plot <- merge(imd_for_plot, imd_results, by = 'variable', all=T)


imd_for_plot <- imd_for_plot %>% filter(!is.na(response))


plot1 <- imd_for_plot %>% pivot_wider(names_from = variable, values_from = percent, id_cols = c(imd_quantile, response))

p1a <- ggplot(glp_imd %>% filter(!is.na(discussion_about_weight)), aes(x=discussion_about_weight, y=imd_residuals, fill=discussion_about_weight)) +
  geom_violin(width = 2) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD residuals for weight discussed') +
  xlab('Weight loss discussed') +
  ylab("IMD residuals") +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p1 <- ggplot(glp_imd %>% filter(!is.na(ways_to_lose_weight)), aes(x=ways_to_lose_weight, y=imd_residuals, fill=ways_to_lose_weight)) +
  geom_violin(width = 2) +
  #geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='kruskal.test') +
  ggtitle('IMD residuals for weight loss methods discussed') +
  xlab('Weight loss method') +
  ylab("IMD residuals") +
  theme_pubr() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

library(FSA)
dunn_test <- dunnTest(imd_residuals ~ ways_to_lose_weight, 
                      data = glp_imd %>% filter(!is.na(ways_to_lose_weight)),
                      method = "bonferroni")
dunn_test <- as.data.frame(dunn_test$res)

p2 <- ggplot(glp_imd %>% filter(!is.na(prescribed)), 
             aes(x=prescribed, y=imd_residuals, fill=prescribed)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='kruskal.test') +
  ggtitle('IMD residuals for self-reported GLP1 prescription') + 
  xlab('Prescription') + 
  ylab("IMD residuals") + 
  theme_pubr() + 
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

p3a <- ggplot(glp_imd %>% filter(!is.na(Q2_WT_SUPPORT__Exercise)), 
              aes(x=Q2_WT_SUPPORT__Exercise, y=imd_residuals, fill=Q2_WT_SUPPORT__Exercise)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD residuals for discussed exercise') + 
  xlab('Discussed exercise') + 
  ylab("IMD residuals") + 
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p3b <- ggplot(glp_imd %>% filter(!is.na(Q2_WT_SUPPORT__Med)), 
              aes(x=Q2_WT_SUPPORT__Med, y=imd_residuals, fill=Q2_WT_SUPPORT__Med)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD residuals for discussed medication') + 
  xlab('Discussed medication') + 
  ylab("IMD residuals") + 
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


p3c <- ggplot(glp_imd %>% filter(!is.na(Q2_WT_SUPPORT__Wt_loss_surg)), 
              aes(x=Q2_WT_SUPPORT__Wt_loss_surg, y=imd_residuals, fill=Q2_WT_SUPPORT__Wt_loss_surg)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='wilcox.test') +
  ggtitle('IMD residuals for discussed surgery') + 
  xlab('Discussed surgery') + 
  ylab("IMD residuals") + 
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p4 <- ggplot(glp_imd %>% filter(!is.na(glp1_used)), 
             aes(x=glp1_used, y=imd_residuals, fill=glp1_used)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +  
  stat_compare_means(method='kruskal.test') +
  ggtitle('IMD residuals for self-reported GLP1 use') + 
  xlab('Self reported GLP1 use') + 
  ylab("IMD residuals") + 
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggarrange(p1a,p2,p3a,p3b, p3c, p4, ncol=2, nrow = 3)

ggplot(glp_imd, aes(x=scale(bmi), y=scale(imd_residuals))) +
  geom_point() +
  theme_pubr()

#=====================================================================
#quantile plots
df_plot <- imd_for_plot %>% filter(variable == 'discussion_about_weight')
fdr_label <- df_plot$fdr[1]
df_plot <- df_plot %>% filter(!is.na(response))
p1 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Had a discussion about weight with primary care provider') +
  scale_fill_brewer(palette = "Blues") +  
  theme_bw()


#P2
df_plot <- imd_for_plot %>% filter(variable == 'Q2_WT_SUPPORT__Exercise')
df_plot$response <- ifelse(df_plot$response == 'TRUE', 'yes', 'no')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
p2 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  annotate("text",
           x = 0.7,  # adjust horizontal position (0.7 or 1)
           y = 95,  # slightly above top bar
           label = paste0("FDR = ", signif(fdr_label, 3)),
           hjust = 0,
           size = 4) +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Discussed exercise') +
  theme_bw()


#p3
df_plot <- imd_for_plot %>% filter(variable == 'Q2_WT_SUPPORT__Healthy_eating')
df_plot$response <- ifelse(df_plot$response == 'TRUE', 'yes', 'no')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
p3 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  annotate("text",
           x = 0.7,  # adjust horizontal position (0.7 or 1)
           y = 95,  # slightly above top bar
           label = paste0("FDR = ", signif(fdr_label, 3)),
           hjust = 0,
           size = 4) +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Discussion about healthy eating') +
  theme_bw()



#p4
df_plot <- imd_for_plot %>% filter(variable == 'Q2_WT_SUPPORT__Med')
df_plot$response <- ifelse(df_plot$response == 'TRUE', 'yes', 'no')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
p4 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Had a discussion about OMs with primary care provider') +
  scale_fill_brewer(palette = "Blues") +  
  theme_bw()



#p5
df_plot <- imd_for_plot %>% filter(variable == 'Q2_WT_SUPPORT__Wt_loss_surg')
df_plot$response <- ifelse(df_plot$response == 'TRUE', 'yes', 'no')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
p5 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  annotate("text",
           x = 0.7,  # adjust horizontal position (0.7 or 1)
           y = 95,  # slightly above top bar
           label = paste0("FDR = ", signif(fdr_label, 3)),
           hjust = 0,
           size = 4) +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Discussion about weight loss surgery') +
  theme_bw()

#p7
df_plot <- imd_for_plot %>% filter(variable == 'Q2_WT_SUPPORT__Wt_loss_grps')
df_plot$response <- ifelse(df_plot$response == 'TRUE', 'yes', 'no')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
p7 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  annotate("text",
           x = 0.7,  # adjust horizontal position (0.7 or 1)
           y = 95,  # slightly above top bar
           label = paste0("FDR = ", signif(fdr_label, 3)),
           hjust = 0,
           size = 4) +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Discussion about participation in weight loss groups') +
  theme_bw()


#p8
df_plot <- imd_for_plot %>% filter(variable == 'prescribed')
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
custom_colors <- c("Prescribed" = "#F8766D","Bought" = "#00BFC4", "Both bought and prescribed" = "#FDC068")
p8 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Purchased or NHS prescribed OMs (self-reported)') +
  scale_fill_brewer(palette = "Blues") +  
  theme_bw()


df_plot <- imd_for_plot %>% filter(variable == 'glp1_used')
df_plot <- df_plot %>% filter(!is.na(response))
df_plot$response <- ifelse(df_plot$response == 'GLP1 used', 'yes', 'no')
fdr_label <- df_plot$fdr[1]
p9 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Self-reported use of OMs') +
  scale_fill_brewer(palette = "Blues") +  
  theme_bw()

p8 <- p8 + theme(legend.position = "none")

ggarrange(p1,p4,p9,p8, common.legend = TRUE, legend = 'right')

df_plot <- imd_for_plot %>% filter(variable == 'nervous_about_talking_to_dr')
df_plot$response <- ifelse(df_plot$response == 'Answer not recorded/lost', 'yes', df_plot$response)
df_plot$response <- ifelse(df_plot$response == 'Not nervous to talk about GLP1', 'no', df_plot$response)
df_plot$response <- ifelse(df_plot$response == 'Unsure if nervous to talk about GLP1', 'unsure', df_plot$response)
df_plot <- df_plot %>% filter(!is.na(response))
fdr_label <- df_plot$fdr[1]
custom_colors <- c("yes" = "#F8766D","no" = "#00BFC4", "unsure" = "#FDC068")

p10 <- ggplot(df_plot, aes(x = imd_quantile, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  annotate("text",
           x = 0.7,  # adjust horizontal position (0.7 or 1)
           y = 95,  # slightly above top bar
           label = paste0("FDR = ", signif(fdr_label, 3)),
           hjust = 0,
           size = 4) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "IMD Quartile",
    y = "Percent",
    fill = "Response") +
  ggtitle('Nervous to talk to doctor about GLP1-RAs if offered again') +
  theme_bw()

ggarrange(p1,p2,p3,p4,p5,p7,p8,p9,p10, ncol=2, nrow=5)
#============================================================================
#make basline table
glpq[, 1:98] <- glpq[, 1:98] %>% mutate(across(everything(), ~ na_if(as.character(.), "NA")))
bmi_group <- glpq %>% select(patient_study_identifier, bmi, Q4_GLP1_NAMES__Weg, Q4_GLP1_NAMES__Ozem, Q4_GLP1_NAMES__Sax, Q4_GLP1_NAMES__Mounj, Q4_GLP1_NAMES__Zep, Q4_GLP1_NAMES__Tru)

bmi_group$bmi_class <- NA
bmi_group$bmi_class <- ifelse(bmi_group$bmi < 18.5, 'underweight', 'normal weight')
bmi_group$bmi_class <- ifelse(bmi_group$bmi >= 25 & bmi_group$bmi <30, 'overweight', bmi_group$bmi_class)
bmi_group$bmi_class <- ifelse(bmi_group$bmi >=30 & bmi_group$bmi <35, 'Obesity class I', bmi_group$bmi_class)
bmi_group$bmi_class <- ifelse(bmi_group$bmi >=35 & bmi_group$bmi <40, 'Obesity class II', bmi_group$bmi_class)
bmi_group$bmi_class <- ifelse(bmi_group$bmi >=40, 'Obesity class III', bmi_group$bmi_class)

bl <- merge(bmi_group, imd[,-c(5,6,11)], by='patient_study_identifier', all=T)
bl <- merge(bl, diag, by='patient_study_identifier', all=T)
names(bl)[[16]] <- "IMD_rank"

bl$Q4_GLP1_NAMES__Mounj <- ifelse(bl$Q4_GLP1_NAMES__Mounj == 'TRUE', 'yes', 'no')
bl$Q4_GLP1_NAMES__Weg <- ifelse(bl$Q4_GLP1_NAMES__Weg == 'TRUE', 'yes', 'no')
bl$Q4_GLP1_NAMES__Ozem <- ifelse(bl$Q4_GLP1_NAMES__Ozem == 'TRUE', 'yes', 'no')
bl$Q4_GLP1_NAMES__Sax <- ifelse(bl$Q4_GLP1_NAMES__Sax == 'TRUE', 'yes', 'no')
bl$Q4_GLP1_NAMES__Zep <- ifelse(bl$Q4_GLP1_NAMES__Zep == 'TRUE', 'yes', 'no')
bl$Q4_GLP1_NAMES__Tru <- ifelse(bl$Q4_GLP1_NAMES__Tru == 'TRUE', 'yes', 'no')


bl$Tirzepatide <- ifelse(bl$Q4_GLP1_NAMES__Mounj == 'yes' | bl$Q4_GLP1_NAMES__Zep == 'yes' , 'yes', 'no')
bl$Semaglutide <- ifelse(bl$Q4_GLP1_NAMES__Ozem == 'yes' | bl$Q4_GLP1_NAMES__Weg == 'yes', 'yes', 'no')
bl$Liraglutide <- ifelse(bl$Q4_GLP1_NAMES__Sax == 'yes', 'yes', 'no')
bl$Dulaglutide <- ifelse(bl$Q4_GLP1_NAMES__Tru == 'yes', 'yes', 'no')

bl$BAME <- ifelse(bl$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BAME', 'Not BAME')

tab1 <- summary(univariateTable(glp1_used ~ age_ehr + sex_ehr + BAME + bmi_class +Tirzepatide + Semaglutide+ Liraglutide + Dulaglutide + cardiovasc + HTN + dyslip + osa + T2DM + Q(IMD_rank), show.totals = TRUE, compare.groups = TRUE,  Q.format = "median(x) [iqr(x)]", data=bl))
tab1_df <- as.data.frame(tab1)
tab1_df$Row <- seq_len(nrow(tab1_df))

vars <- c(
  "age_ehr", "sex_ehr", "BAME", "bmi_class", "Tirzepatide", "Liraglutide", "Semaglutide", "Dulaglutide", "cardiovasc",
  "HTN", "dyslip", "osa", "T2DM", "IMD_rank"
)

test_results <- lapply(vars, function(v) {
  x <- bl[[v]]
  group <- bl$glp1_used
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
#finally sorted! 
write_rds(merged_tab, file='table1A_paper.rds')
write_rds(bl, file='discovery_cohort_baseline_data.rds')
#data_glp_license

#get data for Rajenki
Baseline_data_Rajenki <- glpq %>% select(patient_study_identifier, any_gp_indication_nl, any_gp_indication, Tirzepatide_GP_indicated_no_lifestyle, Tirzepatide_GP_indicated_phase2_no_lifestyle, Tirzepatide_GP_indicated_phase3_no_lifestyle, Tirzepatide_GP_indicated, Tirzepatide_GP_indicated_phase2, Tirzepatide_GP_indicated_phase3, Tirzepatide_specialist_indicated, Tirzepatide_specialist_indicated_nl, Tirzepatide_T2DM, semaglutide_specialist_indicated_nl, semaglutide_specialist_indicated, liraglutide_specialist_indicated, liraglutide_specialist_indicated_nl, glp1_used)
bl_rajenk2 <- bl %>% select(patient_study_identifier, bmi, age_ehr, sex_ehr, IMD_rank, BAME, cardiovasc, osa, HTN, T2DM, dyslip)

Baseline_data_Rajenki <- merge(bl_rajenk2, Baseline_data_Rajenki, by='patient_study_identifier', all=T)

names(Baseline_data_Rajenki) <- c('id','BMI','Age','Sex','IMD_rank','In_BAME_group','Cardiovascular_disease','OSA','Hypertension','Type_2_diabetes','Dyslipidaemia','GP_indication_for_Tirzepatide_no_lifestyle_requirement_overall','GP_indication_for_Tirzepatide_lifestyle_requirement_overall','GP_indicated_Tirzepatide_no_lifestyle_phase1','GP_indicated_Tirzepatide_no_lifestyle_phase2','GP_indicated_Tirzepatide_no_lifestyle_phase3','GP_indicated_Tirzepatide_lifestyle_phase1','GP_indicated_Tirzepatide_lifestyle_phase2','GP_indicated_Tirzepatide_lifestyle_phase3','Specialist_indication_for_tirzepatide_with_lifestyle','Specialist_indication_for_tirzepatide_no_lifestyle','Tirzepatide_indicated_for_T2DM','Semaglutide_specialist_indicated_no_lifestyle','Semaglutide_specialist_indicated_with_lifestyle','Liraglutide_specialist_indicated_with_lifestyle','Liraglutide_specialist_indicated_no_lifestyle','Self_reported_GLP1_use')
write_rds(Baseline_data_Rajenki, file='Discovery_cohort_individual_level_data_for_Rajenki.rds')


bl_for_model <- bl
bl_for_model$glp1_used <- as.factor(bl_for_model$glp1_used)
levels(bl_for_model$glp1_used)
bl_for_model$glp1_used <- factor(bl_for_model$glp1_used, 
                                 levels = c("No GLP1 used", "GLP1 used"))
mod1 <- glm(glp1_used ~ age_ehr + sex_ehr + BAME + bmi_class + IMD_rank + cardiovasc + HTN + dyslip + osa + T2DM, family = 'binomial', data=bl_for_model)
summary(mod1)
drop1(mod1, test = "Chisq")
library(gtsummary)
or_table_mod1 <- tbl_regression(mod1, exponentiate = TRUE)
or_table_mod1

bl_for_model$glp_ehr <- ifelse(bl_for_model$patient_study_identifier %in% meds$patient_study_identifier, 'OM', 'No OM')
bl_for_model$glp_ehr <- factor(bl_for_model$glp_ehr, 
                               levels = c("No OM", "OM"))

mod2 <- glm(glp_ehr ~ age_ehr + sex_ehr + BAME + bmi_class + IMD_rank + cardiovasc + HTN + dyslip + osa + T2DM, family = 'binomial', data=bl_for_model)
summary(mod2)
drop1(mod2, test = "Chisq")
or_table_mod2 <- tbl_regression(mod2, exponentiate = TRUE)
or_table_mod2
#========================================================
classes <- glpq %>% select(patient_study_identifier, any_gp_indication_nl, any_specialist_indication_nl, Tirzepatide_T2DM)
bl_elig <- merge(bl, classes, by='patient_study_identifier')
bl_elig$any_gp_indication_nl <- ifelse(is.na(bl_elig$any_gp_indication_nl), 'No indication', bl_elig$any_gp_indication_nl)
bl_elig$any_specialist_indication_nl <- ifelse(is.na(bl_elig$any_specialist_indication_nl), 'No specialist indication', bl_elig$any_specialist_indication_nl)
elig_tab <- summary(univariateTable(any_gp_indication_nl ~ age_ehr + sex_ehr + bmi_class + BAME + Q(IMD_rank) + any_specialist_indication_nl + Tirzepatide_T2DM + cardiovasc + osa +  T2DM + HTN + dyslip, Q.format = "median(x) [iqr(x)]", show.totals = TRUE, compare.groups = TRUE, column.percent = TRUE, data=bl_elig))

tab1_df <- as.data.frame(elig_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))

vars <- c("age_ehr", "sex_ehr", "bmi_class", "BAME", "IMD_rank",
          "any_specialist_indication_nl", "Tirzepatide_T2DM", "cardiovasc", "osa", "T2DM", "HTN", "dyslip")

test_results <- lapply(vars, function(v) {
  x <- bl_elig[[v]]
  group <- bl_elig$any_gp_indication_nl
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]

write.csv2(merged_tab, file='table_for_differences_between_eligible_populations.csv')


#========================================================================
#compare medication data
meds <- umed_medication %>% select(patient_study_identifier, min_date, max_date, substance)
summary(as.factor(meds$substance))

glp_bought <- glpq
glp_bought$Bought <- ifelse(!glp_bought$patient_study_identifier %in% meds$patient_study_identifier & glp_bought$prescribed == 'Bought', 'Drug bought', 'Not bought')
glp_bought$EHR_recorded <- ifelse(glp_bought$patient_study_identifier %in% meds$patient_study_identifier, 'EHR recorded', 'Not EHR recorded')
glp_bought <- merge(glp_bought, adj_imd, by='patient_study_identifier', all=T)

bl_comorb <- bl %>% select(patient_study_identifier, age_ehr, sex_ehr, bmi_class, cardiovasc, osa, HTN, T2DM, dyslip, BAME)
glp_bought <- merge(glp_bought, bl_comorb, by='patient_study_identifier', all=T)

#only in users
glp_bought <- glp_bought %>% filter(glp1_used == 'GLP1 used')
glp_bought$Bought <- ifelse(is.na(glp_bought$Bought), 'Not bought', glp_bought$Bought)
glp_bought$side_effect_any <- ifelse(glp_bought$side_effects == 'None', 'No', 'Side effects reported')
names(glp_bought)[89:95] <- c('Help_side_effects', 'Easier_to_get_next_dose', 'Help_from_dr','Lower_cost','Support_from_others','Clearer_instructions','No_problems')
imd_for_merge <- imd %>% select(patient_study_identifier, `Index of Multiple Deprivation Rank`)
names(imd_for_merge) <- c('patient_study_identifier', 'IMD_rank')
glp_bought <- merge(glp_bought, imd_for_merge, by='patient_study_identifier', all=T)
glp_bought <- glp_bought %>% filter(!is.na(Bought))
bought_tab <- summary(univariateTable(Bought ~ age_ehr + sex_ehr + bmi_class + BAME + imd_residuals_norm + Q(IMD_rank) + prescribed + current_user + reason_for_use + lose_weight + changed_daily_activity + side_effect_any + experience_with_raising_dose + instructions_for_raising_dose + Help_side_effects + Easier_to_get_next_dose + Help_from_dr + Lower_cost + Support_from_others + Clearer_instructions + No_problems+ cardiovasc + osa + HTN + T2DM + dyslip, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, Q.format = "median(x) [iqr(x)]", data=glp_bought))
recorded_tab <- summary(univariateTable(EHR_recorded ~ age_ehr + sex_ehr + bmi_class + BAME + imd_residuals_norm + Q(IMD_rank) + prescribed + current_user + reason_for_use + lose_weight + changed_daily_activity + side_effect_any + experience_with_raising_dose + instructions_for_raising_dose +  Help_side_effects + Easier_to_get_next_dose + Help_from_dr + Lower_cost + Support_from_others + Clearer_instructions + No_problems + cardiovasc + osa + HTN + T2DM + dyslip, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, Q.format = "median(x) [iqr(x)]", data=glp_bought))
glp_bought$Bought <- as.factor(glp_bought$Bought)
levels(glp_bought$Bought)


glp_bought$Bought <- factor(glp_bought$Bought, 
                            levels = c("Not bought", "Drug bought"))
glp_bought$experience_with_raising_dose <- as.factor(glp_bought$experience_with_raising_dose)
glp_bought$reason_for_use <- as.factor(glp_bought$reason_for_use)
mod3 <- glm(Bought ~ age_ehr + sex_ehr + BAME + bmi_class + IMD_rank + prescribed + current_user + reason_for_use + lose_weight + experience_with_raising_dose + instructions_for_raising_dose + Lower_cost + No_problems + T2DM, family = 'binomial', data=glp_bought)
summary(mod3)
glp_bought$experience_with_raising_dose <- as.factor(glp_bought$experience_with_raising_dose)
or_table_mod3 <- tbl_regression(mod3, exponentiate = TRUE)
or_table_mod3

#decide on follow up steps for this tomorrow! 

tab1_df <- as.data.frame(bought_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))


vars <- c("age_ehr","sex_ehr","bmi_class","BAME", "imd_residuals_norm","IMD_rank" ,"prescribed","current_user",
          "reason_for_use","lose_weight","changed_daily_activity","side_effect_any","experience_with_raising_dose",
          "instructions_for_raising_dose", 'Help_side_effects', 'Easier_to_get_next_dose', 'Help_from_dr','Lower_cost','Support_from_others','Clearer_instructions','No_problems', "cardiovasc","osa","HTN","T2DM","dyslip")

test_results <- lapply(vars, function(v) {
  x <- glp_bought[[v]]
  group <- glp_bought$Bought
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
write.csv2(merged_tab, file='bought_vs_prescribed_differences.csv')


tab1_df <- as.data.frame(recorded_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))
test_results <- lapply(vars, function(v) {
  x <- glp_bought[[v]]
  group <- glp_bought$EHR_recorded
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]

write.csv2(merged_tab, file='EHR_recorded_vs_not_recorded.csv')

glp_bought$Bought <- factor(glp_bought$Bought, 
                            levels = c("Not bought", "Drug bought"))
glp_bought$experience_with_raising_dose <- as.factor(glp_bought$experience_with_raising_dose)
glp_bought$reason_for_use <- as.factor(glp_bought$reason_for_use)
mod3 <- glm(Bought ~ age_ehr + sex_ehr + BAME + bmi_class + IMD_rank + prescribed + current_user + reason_for_use + lose_weight + experience_with_raising_dose + instructions_for_raising_dose + Lower_cost + No_problems + T2DM, family = 'binomial', data=glp_bought)
summary(mod3)
drop1(mod3, test = "Chisq")


glp_bought$experience_with_raising_dose <- as.factor(glp_bought$experience_with_raising_dose)
or_table_mod3 <- tbl_regression(mod3, exponentiate = TRUE)
or_table_mod3

#===========================================================================
#self reported bought analysis
srep <- glp_bought %>% filter(!is.na(prescribed))
srep_tab <- summary(univariateTable(prescribed ~ age_ehr + sex_ehr + bmi_class + BAME + imd_residuals_norm + Q(IMD_rank) + EHR_recorded + current_user + reason_for_use + lose_weight + changed_daily_activity + side_effect_any + experience_with_raising_dose + instructions_for_raising_dose +  Help_side_effects + Easier_to_get_next_dose + Help_from_dr + Lower_cost + Support_from_others + Clearer_instructions + No_problems + cardiovasc + osa + HTN + T2DM + dyslip, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, Q.format = "median(x) [iqr(x)]", data=srep))

#decide on follow up steps for this tomorrow! 

tab1_df <- as.data.frame(srep_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))


vars <- c("age_ehr","sex_ehr","bmi_class","BAME", "imd_residuals_norm","IMD_rank" ,"EHR_recorded","current_user",
          "reason_for_use","lose_weight","changed_daily_activity","side_effect_any","experience_with_raising_dose",
          "instructions_for_raising_dose", 'Help_side_effects', 'Easier_to_get_next_dose', 'Help_from_dr','Lower_cost','Support_from_others','Clearer_instructions','No_problems', "cardiovasc","osa","HTN","T2DM","dyslip")

test_results <- lapply(vars, function(v) {
  x <- srep[[v]]
  group <- srep$prescribed
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Kruskal–Wallis test
    test <- suppressWarnings(kruskal.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Kruskal–Wallis",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
write.csv2(merged_tab, file='self_reported_bought_vs_prescribed_differences.csv')


#==============================================================
#current vs past user analysis
user <- glp_bought %>% filter(current_user %in% c("Current user", "Past user"))
user_tab <- summary(univariateTable(current_user ~ age_ehr + sex_ehr + bmi_class + BAME + imd_residuals_norm + Q(IMD_rank) + prescribed + EHR_recorded + reason_for_use + lose_weight + changed_daily_activity + side_effect_any + experience_with_raising_dose + instructions_for_raising_dose +  Help_side_effects + Easier_to_get_next_dose + Help_from_dr + Lower_cost + Support_from_others + Clearer_instructions + No_problems + cardiovasc + osa + HTN + T2DM + dyslip, column.percent = TRUE, show.totals = TRUE, compare.groups = TRUE, Q.format = "median(x) [iqr(x)]", data=user))

#decide on follow up steps for this tomorrow! 

tab1_df <- as.data.frame(user_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))


vars <- c("age_ehr","sex_ehr","bmi_class","BAME", "imd_residuals_norm","IMD_rank" ,"prescribed","EHR_recorded",
          "reason_for_use","lose_weight","changed_daily_activity","side_effect_any","experience_with_raising_dose",
          "instructions_for_raising_dose", 'Help_side_effects', 'Easier_to_get_next_dose', 'Help_from_dr','Lower_cost','Support_from_others','Clearer_instructions','No_problems', "cardiovasc","osa","HTN","T2DM","dyslip")

test_results <- lapply(vars, function(v) {
  x <- user[[v]]
  group <- user$current_user
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
write.csv2(merged_tab, file='current_vs_past_user_differences.csv')

user$current_user <- as.factor(user$current_user)
levels(user$current_user)
user$current_user <- factor(user$current_user, 
                            levels = c("Past user", "Current user"))

mod4 <- glm(current_user ~ age_ehr + sex_ehr + BAME + bmi_class + IMD_rank + prescribed + EHR_recorded + reason_for_use + lose_weight + changed_daily_activity + experience_with_raising_dose + instructions_for_raising_dose + Help_side_effects + No_problems, family = 'binomial', data=user)
summary(mod4)
drop1(mod4, test = "Chisq")


or_table_mod4 <- tbl_regression(mod4, exponentiate = TRUE)
or_table_mod4

#=========================================================================================
#STEP 3
#validation of analyses
#' ---
#' title: uMED 4K check
#' author: Eckart De Bie
#' output:
#'    html_document:
#'     toc: true
#'         
#' ---

#' uMED CMD data analysis 

#setWD
setwd("C:/Users/Gebruiker/Documents/PhD/myfilepath")

#load libraries
library(tidyverse)
library(ggalluvial)
library(ggpubr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(purrr)
library(stringr)
library(Publish)

#import the data from the drive
googledrive::drive_auth()
dir = drive_find(pattern = 'GLP-1 External data sharing ', type='folder')

if (nrow(dir) > 0) {
  folder_id <- dir$id[1]   
  query <- sprintf("'%s' in parents", folder_id)
  a <- drive_find(q = query)
} else {
  stop("Folder not found! Check the pattern.")
}

#file needs to be google sheet! --> load in
glp1 <- gs4_get("mylink")
umed_glp1 <- read_sheet(glp1)

umed_glp1$Q6_GLP1_REASON__Both <- as.character(umed_glp1$Q6_GLP1_REASON__Both)
umed_glp1$Q6_GLP1_REASON__Weight <- as.character(umed_glp1$Q6_GLP1_REASON__Weight)

weight_use <- umed_glp1 %>% filter(umed_glp1$Q6_GLP1_REASON__Weight == 'TRUE' | umed_glp1$Q6_GLP1_REASON__Both == 'lost') %>% select(patient_study_identifier) %>% unique()

#load in the data for the 4K
qt_ethnicity_acmd_update1 <- read_excel("qt_ethnicity_acmd_update1.xlsx")
`Height_Weight_HbA1c_ACMD_Consented.(10.14.2025)` <- read.csv("~/PhD/myfilepath/Height_Weight_HbA1c_ACMD_Consented (10.14.2025).csv")

diag <- `Height_Weight_HbA1c_ACMD_Consented.(10.14.2025)` 
summary(as.factor(diag$height_unit))
diag$height <- ifelse(diag$height_unit == 'm', diag$latest_height*100, diag$latest_height)
diag$height <- ifelse(diag$height_unit == 'ft', diag$height*30.4, diag$height)
#assuming height below 20cm (as all converted is actually in meters)
diag$height <- ifelse(diag$height <20, diag$height*100, diag$height)

summary(as.factor(diag$weight_unit))
#exclude all weight <20kg --> not reliable (all in kg recorded)
diag <- diag %>% filter(latest_weight >20)

diag$bmi <- diag$latest_weight/(diag$height/100)^2
hist(diag$bmi, breaks=50)
#some implausible BMIs
diag <- diag %>% filter(bmi <100)
hist(diag$bmi, breaks=50)

#BMI <10 is unreliable
diag <- diag %>% filter(bmi >10)

#get numbers for uMed

diag$bmi_umed <- NA
diag$bmi_umed <- ifelse(diag$bmi < 18.5, 'underweight', 'Healthy weight')
diag$bmi_umed <- ifelse(diag$bmi >= 25 & diag$bmi <30, 'overweight', diag$bmi_umed)
diag$bmi_umed <- ifelse(diag$bmi >=30 & diag$bmi <40, 'Obese', diag$bmi_umed)
diag$bmi_umed <- ifelse(diag$bmi >=40, 'Severely obese', diag$bmi_umed)

summary(as.factor(diag$bmi_umed))

glps <- diag %>% filter(patient_study_identifier %in% umed_glp1$patient_study_identifier)
no_glps <- diag %>% filter(!patient_study_identifier %in% umed_glp1$patient_study_identifier)

summary(as.factor(glps$bmi_umed))
summary(as.factor(no_glps$bmi_umed))
#================================================================================
#start with Moujaro
summary(as.factor(qt_ethnicity_acmd_update1$ethnicity))

ethnicity <- qt_ethnicity_acmd_update1 %>% filter(ethnicity %in% c('Asian or Asian British','Black or Black British','Chinese or Other Ethnic Groups')) %>% select(patient_study_identifier)
ethnicity <- unique(ethnicity)

#NOTE: we need coronary artery disease, not included CHF yet (as not specified in NICE guideline)
Umed_patients_cardiovasc <- readRDS("~/PhD/myfilepath/Umed_patients_cardiovasc.rds")
Umed_patients_cerebro <- readRDS("~/PhD/myfilepath/Umed_patients_cerebro.rds")
Umed_patients_heartfailure <- readRDS("~/PhD/myfilepath/Umed_patients_heartfailure.rds")
Umed_patients_OSA <- readRDS("~/PhD/myfilepath/Umed_patients_OSA.rds")
Umed_patients_T2DM <- readRDS("~/PhD/myfilepath/Umed_patients_T2DM.rds")
Umed_patients_dyslip <- readRDS("~/PhD/myfilepath/Umed_patients_dyslip.rds")
Umed_patients_HTN <- readRDS("~/PhD/myfilepath/Umed_patients_HTN.rds")


diag$cardiovasc <- ifelse(diag$patient_study_identifier %in% Umed_patients_cardiovasc$patient_study_identifier, 'Yes', 'no')
diag$osa <- ifelse(diag$patient_study_identifier %in% Umed_patients_OSA$patient_study_identifier, 'Yes', 'no')
diag$cerebrovasc_snomed <- ifelse(diag$patient_study_identifier %in% Umed_patients_cerebro$patient_study_identifier, 'Yes', 'no')
diag$hf_snomed <- ifelse(diag$patient_study_identifier %in% Umed_patients_heartfailure$patient_study_identifier, 'Yes', 'no')
diag$T2DM <- ifelse(diag$patient_study_identifier %in% Umed_patients_T2DM$patient_study_identifier, 'Yes', 'no')
diag$HTN <- ifelse(diag$patient_study_identifier %in% Umed_patients_HTN$patient_study_identifier, 'Yes', 'no')
diag$dyslip <- ifelse(diag$patient_study_identifier %in% Umed_patients_dyslip$patient_study_identifier, 'Yes', 'no')


diag$number_of_comorbs <- NA
diag$number_of_comorbs <- ifelse(diag$HTN == 'Yes', 1, 0)
diag$number_of_comorbs <- ifelse(diag$T2DM == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$dyslip == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$cardiovasc == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)
diag$number_of_comorbs <- ifelse(diag$osa == 'Yes', diag$number_of_comorbs + 1, diag$number_of_comorbs)

four_comorbs <- diag %>% filter(number_of_comorbs >=4) %>% select(patient_study_identifier) %>% unique()
three_comorbs <- diag %>% filter(number_of_comorbs >=3) %>% select(patient_study_identifier) %>% unique()
one_or_more_comorbs <- diag %>% filter(number_of_comorbs >=1) %>% select(patient_study_identifier) %>% unique()

htn_or_dyslip <- diag %>% filter(HTN == 'Yes' | dyslip == 'Yes') %>% select(patient_study_identifier) %>% unique()
no_dm <- diag %>% filter(T2DM != 'TRUE') %>% select(patient_study_identifier) %>% unique()

glpq <- diag 
#==================================================================================================
#data is prepared, do checks 
glpq$BMI_for_tirzepatide_specialist <- ifelse(glpq$bmi >=35 | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')
glpq$BMI_for_tirzepatide_GP <- ifelse(glpq$bmi >=40 | glpq$bmi >=37.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')
glpq$BMI_for_tirzepatide_GP_phase2 <- ifelse(glpq$bmi >=35 | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BMI high enough', 'BMI not high enough')

glpq$comorbs_for_tirzepatide_specialist <- ifelse(glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier, 'yes', 'no')
glpq$comorbs_for_tirzepatide_gp <- ifelse(glpq$patient_study_identifier %in% four_comorbs$patient_study_identifier, 'yes', 'no')
glpq$comorbs_for_tirzepatide_gp_phase3 <- ifelse(glpq$patient_study_identifier %in% three_comorbs$patient_study_identifier, 'yes', 'no')

glpq$Tirzepatide_specialist_indicated <- ifelse(glpq$BMI_for_tirzepatide_specialist == 'BMI high enough' & glpq$comorbs_for_tirzepatide_specialist == 'yes', 'TRUE', 'Secondary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated <- ifelse(glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase2 <- ifelse(glpq$BMI_for_tirzepatide_GP_phase2 == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')
glpq$Tirzepatide_GP_indicated_phase3 <- ifelse(glpq$BMI_for_tirzepatide_GP == 'BMI high enough' & glpq$comorbs_for_tirzepatide_gp_phase3 == 'yes', 'TRUE', 'Primary care Tirzepatide not indicated')

Patients_on_3_antidiabetics_incl_metformin <- readRDS("~/PhD/myfilepath/Patients_on_3_antidiabetics_incl_metformin.rds")

glpq$Tirzepatide_T2DM <- ifelse(glpq$BMI_for_tirzepatide_specialist == 'BMI high enough' & glpq$patient_study_identifier %in% Patients_on_3_antidiabetics_incl_metformin$patient_study_identifier, 'TRUE', 'T2DM Tirzepatide not indicated')
summary(as.factor(glpq$Tirzepatide_T2DM))
dm_glp <- glpq %>% filter(Tirzepatide_T2DM == 'TRUE') %>% select(patient_study_identifier) %>% unique()


#Add MHRA license criteria for Tirzepatide for obesity 
#from: https://mhraproducts4853.blob.core.windows.net/docs/8729e32b84978867881a40ec535bef40c9877ce8
#liraglutide & semaglutide only seem to have MHRA information on T2DM (?)
#from: https://mhraproducts4853.blob.core.windows.net/docs/c5257911ba77a5bf9dd81163de9d07c406e48720
#https://mhraproducts4853.blob.core.windows.net/docs/4a176520080ef5ac905594dc34f5727651d0ecef
#Wegovy is licensed for weight management if BMI >=30 OR >=27 + 1 weight related comorb
#Tirzepatide is licensed for weight management if BMI>=30 or >=27 +1 weight related comrob

glpq$MHRA_tirzepatide_or_semaglutide <- ifelse(glpq$bmi >=30 | glpq$bmi >=27 & glpq$patient_study_identifier %in% one_or_more_comorbs, 'MHRA license indication', 'No MHRA license indication')
#============================================================================
#move on to semaglutide
glpq$BMI_for_semaglutide_specialist <- ifelse(glpq$bmi >=35 & glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier | glpq$bmi >=32.5 & glpq$patient_study_identifier %in% ethnicity$patient_study_identifier & glpq$patient_study_identifier %in% one_or_more_comorbs$patient_study_identifier, 'BMI high enough', 'BMI not high enough')
glpq$semaglutide_specialist_indicated <- ifelse(glpq$BMI_for_semaglutide_specialist == 'BMI high enough', 'TRUE', 'Secondary care Semaglutide not indicated')

summary(as.factor(glpq$semaglutide_specialist_indicated))
#now liraglutide
#lacking data to properly assess this! 

#get meds
prescribed_GLP1_MedsOct25 <- readRDS("~/PhD/myfilepath/prescribed_GLP1_MedsOct25.rds")

glpq$glp1_use <- ifelse(glpq$patient_study_identifier %in% prescribed_GLP1_MedsOct25$patient_study_identifier, 'Prescribed', 'Not prescribed')

#now only select those not already in the 6K
glpq <- glpq %>% filter(!patient_study_identifier %in% umed_glp1$patient_study_identifier)
#4166 retained

#plot now 
lab_tab <- as.data.frame(table(glpq$MHRA_tirzepatide_or_semaglutide, glpq$Tirzepatide_GP_indicated, glpq$Tirzepatide_GP_indicated_phase2, glpq$Tirzepatide_GP_indicated_phase3, glpq$Tirzepatide_specialist_indicated, glpq$Tirzepatide_T2DM, glpq$semaglutide_specialist_indicated, glpq$glp1_use))

names(lab_tab)[[1]] <-"MHRA license indicattion"
names(lab_tab)[[2]] <-"GP Tirzepatide - phase 1"
names(lab_tab)[[3]] <-"GP Tirzepatide - phase 2"
names(lab_tab)[[4]] <-"GP Tirzepatide - phase 3"
names(lab_tab)[[5]] <-"Tirzepatide - Specialist"
names(lab_tab)[[6]] <-"Tirzepatide - T2DM"
names(lab_tab)[[7]] <-"Semaglutide - Specialist"
names(lab_tab)[[8]] <-"GLP1 drugs - prescribed"
names(lab_tab)[[9]] <-"Frequencies"

#transform to lodes format
lab_lodes <- to_lodes_form(lab_tab,
                           axes = 1:8,
                           id = "Cohort")

names(lab_lodes)[[2]] <- "id"
names(lab_lodes)[[3]] <- "Answer"
names(lab_lodes)[[4]] <- "Question"

ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Frequencies,
           fill = Question, label = Question)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("GLP1 4K eligibility") +
  theme_pubr()

#now make sure plot conforms to layout
totals <- lab_lodes %>%
  group_by(Answer) %>%
  summarise(total_n = sum(Frequencies))

lab_lodes <- lab_lodes %>% left_join(totals, by = "Answer") %>% mutate(Percentage = Frequencies / total_n * 100)

avg_total <- round(mean(totals$total_n))

#plot! 
ggplot(lab_lodes,
       aes(x = Answer, stratum = Question, alluvium = id,
           y = Percentage,
           fill = Question, label = Question)) +
  scale_x_discrete(name = "Question", expand = c(.1, .1)) +  # X-axis label
  geom_flow() +
  geom_stratum(alpha = 0.8, width = 0.5, color = "white") +
  geom_text(stat = "stratum", size = 3.5) +
  
  scale_y_continuous(
    name = "Percentage (%)",
    sec.axis = sec_axis(~ . * avg_total / 100, name = "Count (n)")
  ) +
  ggtitle("GLP1 medication - NICE indication - 4K") +
  theme_pubr() +
  theme(legend.position = "none")

#=================================================================================================
#continue with BL table
glpq$bmi_class <- NA
glpq$bmi_class <- ifelse(glpq$bmi < 18.5, 'underweight', 'normal weight')
glpq$bmi_class <- ifelse(glpq$bmi >= 25 & glpq$bmi <30, 'overweight', glpq$bmi_class)
glpq$bmi_class <- ifelse(glpq$bmi >=30 & glpq$bmi <35, 'Obesity class I', glpq$bmi_class)
glpq$bmi_class <- ifelse(glpq$bmi >=35 & glpq$bmi <40, 'Obesity class II', glpq$bmi_class)
glpq$bmi_class <- ifelse(glpq$bmi >=40, 'Obesity class III', glpq$bmi_class)

#get drugs
GLP_meds_umed <- readRDS("~/PhD/myfilepath/GLP_meds_umed.rds")
summary(as.factor(GLP_meds_umed$substance))

glp_pat <- GLP_meds_umed %>% filter(patient_study_identifier %in% weight_use$patient_study_identifier)
glp_pat <- glp_pat %>%
  group_by(patient_study_identifier) %>%
  summarise(min_date = min(min_date, na.rm = TRUE)) %>%
  ungroup()

#now filter after date
june25 <- as.Date("23-6-2025", format='%d-%m-%Y')
glp_pat <- glp_pat %>% filter(min_date > june25)

generic_lookup <- c(
  "Dulaglutide (substance)" = "Dulaglutide",
  "Exenatide (substance)" = "Exenatide",
  "Liraglutide (substance)" = "Liraglutide",
  "Lixisenatide (substance)" = "Lixisenatide",
  "Semaglutide (substance)" = "Semaglutide",
  "Tirzepatide (substance)" = "Tirzepatide",
  "Substance with glucagon-like peptide 1 receptor agonist mechanism of action (substance)" = "GLP1_RA",
  "Long-acting insulin (substance)" = "Insulin"
)

GLP_meds_umed$substance <- generic_lookup[GLP_meds_umed$substance]

GLP_meds_wide <- GLP_meds_umed %>%
  distinct(patient_study_identifier, substance) %>%
  mutate(value = "Yes") %>%
  pivot_wider(
    names_from = substance,
    values_from = value,
    values_fill = "No")

glpq <- merge(glpq, GLP_meds_wide, by='patient_study_identifier', all=T)
glpq <- glpq %>% filter(!is.na(patientguid))
#number normal

glpq[, 37:43] <- lapply(glpq[, 37:43], function(x) {
  x[is.na(x)] <- "No"
  x
})

bl <- glpq

#======================================================================================
#overview table for eligibility
summary(as.factor(bl$bmi_class))
summary(as.factor(bl$MHRA_tirzepatide_or_semaglutide))
summary(as.factor(bl$glp1_use))

bl$specialist_indication <- ifelse(bl$semaglutide_specialist_indicated == 'TRUE' | bl$Tirzepatide_specialist_indicated == 'TRUE' | bl$Tirzepatide_T2DM == 'TRUE', 'Specialist indicated', 'Not specialist indicated')

ph1 <- bl %>% filter(Tirzepatide_GP_indicated == 'TRUE')
summary(as.factor(ph1$specialist_indication))
summary(as.factor(ph1$glp1_use))
ph2 <- bl %>% filter(Tirzepatide_GP_indicated_phase2 == 'TRUE')
summary(as.factor(ph2$specialist_indication))
summary(as.factor(ph2$glp1_use))
ph3 <- bl %>% filter(Tirzepatide_GP_indicated_phase3 == 'TRUE')
summary(as.factor(ph3$specialist_indication))
summary(as.factor(ph3$glp1_use))
bl$overall_indication <- ifelse(bl$Tirzepatide_GP_indicated_phase3 == 'TRUE' | bl$Tirzepatide_GP_indicated_phase2 == 'TRUE' | bl$Tirzepatide_GP_indicated == 'TRUE', 'indicated', 'not indicated')

ph_overall <- bl %>% filter(overall_indication == 'indicated')
summary(as.factor(ph_overall$specialist_indication))
summary(as.factor(ph_overall$glp1_use))
#=====================================================================================

ACMD_Consented_Patient_IMD_Rank_09_19_2025_ <- read_excel("ACMD Consented Patient_IMD Rank (09.19.2025).xlsx")
imd <- ACMD_Consented_Patient_IMD_Rank_09_19_2025_
#=============================================
#Check here
names(imd)[[2]] <- 'IMD_rank'
bl <- merge(imd, bl, all=T) %>% filter(!is.na(patient_study_identifier))

#add in age/sex AND ethnicity group
qt_age_sex_acmd_total_update2 <- read_excel("qt_age_sex_acmd_total_update2.xlsx")
age_sex <- qt_age_sex_acmd_total_update2[,c(1,3:4)]
bl <- merge(bl, age_sex, all=T) %>% filter(!is.na(patient_study_identifier))
bl$BAME <- ifelse(bl$patient_study_identifier %in% ethnicity$patient_study_identifier, 'BAME group', 'Not in BAME group')
bl$glp1_used <- bl$glp1_use

tab2 <- summary(univariateTable(glp1_used ~ age + sex + BAME + bmi_class +Tirzepatide + Semaglutide+ Liraglutide + Dulaglutide + Exenatide + cardiovasc + HTN + dyslip + osa + T2DM + Q(IMD_rank), show.totals = TRUE, compare.groups = TRUE,  Q.format = "median(x) [iqr(x)]", data=bl))
tab1_df <- as.data.frame(tab2)
tab1_df$Row <- seq_len(nrow(tab1_df))

vars <- c(
  "age", "sex", "BAME", "bmi_class", "Tirzepatide", "Liraglutide", "Semaglutide", "Dulaglutide", "Exenatide", "cardiovasc",
  "HTN", "dyslip", "osa", "T2DM", "IMD_rank"
)

test_results <- lapply(vars, function(v) {
  x <- bl[[v]]
  group <- bl$glp1_used
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
#finally sorted! 
write_rds(merged_tab, file='table1B_paper.rds')

table1B_paper <- readRDS("~/PhD/Projects/myfilepath/table1B_paper.rds")
table1B_paper$cohort <- 'Replication'
table1A_paper <- readRDS("~/PhD/Projects/myfilepath/table1A_paper.rds")
table1A_paper$cohort <- 'Discovery'

form1 <- table1A_paper[,c(1,11,2,3:5,10)]
form2 <- table1B_paper[,c(1,11,2,3:5,10)]

write.csv2(form1, file='bl_table1A.csv')
write.csv2(form2, file='bl_table1B.csv')

write_rds(bl, file ="Replication_cohort_baseline_data.rds")

#=============================================================================
#start with a new table 1
discovery_cohort_baseline_data <- readRDS("~/PhD/myfilepath/discovery_cohort_baseline_data.rds")
Replication_cohort_baseline_data <- readRDS("~/PhD/myfilepath/Replication_cohort_baseline_data.rds")



#import the data from the drive
googledrive::drive_auth()
dir = drive_find(pattern = 'GLP-1 External data sharing ', type='folder')

if (nrow(dir) > 0) {
  folder_id <- dir$id[1]   
  query <- sprintf("'%s' in parents", folder_id)
  a <- drive_find(q = query)
} else {
  stop("Folder not found! Check the pattern.")
}

#add in wheter or not EHR prescribed
meds <- gs4_get("mylink")
umed_medication <- read_sheet(meds)
meds <- umed_medication %>% select(patient_study_identifier, min_date, max_date, substance)
summary(as.factor(meds$substance))

meds <- unique(meds)
meds$min_date <- as.Date(meds$min_date)
meds$max_date <- as.Date(meds$max_date)

meds_clean <- meds %>% group_by(patient_study_identifier) %>%summarise(min_date = min(min_date, na.rm = TRUE),max_date = max(max_date, na.rm = TRUE),number_of_drugs = n_distinct(substance))
scripts <- meds_clean 
scripts$EHR_recorded <- 'yes'

meds$use <- 'yes'
meds <- meds %>% select(patient_study_identifier, substance, use) %>% unique()
meds <- meds %>% pivot_wider(id_cols = patient_study_identifier, names_from = substance, values_from = 'use')

meds[is.na(meds)] <- "no"
meds <- meds[,c(1:6)]
names(meds) <- c('patient_study_identifier', 'Tirzepatide_EHR', 'Semaglutide_EHR', 'Dulaglutide_EHR', 'Liraglutide_EHR', 'Exenatide_EHR')

discovery_cohort_baseline_data$EHR_prescribed <- ifelse(discovery_cohort_baseline_data$patient_study_identifier %in% scripts$patient_study_identifier, 'EHR recorded', 'Not EHR recorded')
bl1 <- merge(discovery_cohort_baseline_data, meds, by='patient_study_identifier', all=T)
bl1[, 45:49][is.na(bl1[, 45:49])] <- "no"

#get the data to merge properly! 
t1 <- bl1[,c(1,18,17,9, 43, 16,27,28,31:33, 44:49)]
t2 <- Replication_cohort_baseline_data[,c(3,50,51,38,52,2,15,16,19:21,37,39:43)]

names(t1) <- c('id', 'Age','Sex', 'BMI_class', 'BAME', 'IMD_rank', 'Artherosclerotic_cardiovascular_disease', 'OSA', 'T2DM', 'Hypertension', 'Dyslipidaemia', 'EHR_prescribed', 'Tirzepatide_EHR', 'Semaglutide_EHR', 'Dulaglutide_EHR','Liraglutide_EHR','Exenatide_EHR')
names(t2) <- c('id', 'Age','Sex', 'BMI_class', 'BAME', 'IMD_rank', 'Artherosclerotic_cardiovascular_disease', 'OSA', 'T2DM', 'Hypertension', 'Dyslipidaemia', 'EHR_prescribed', 'Tirzepatide_EHR', 'Semaglutide_EHR', 'Dulaglutide_EHR','Liraglutide_EHR','Exenatide_EHR')
t1$cohort <- 'Discovery'
t2$cohort <- 'Replication'

first_tab <- rbind(t1,t2)

first_tab <- first_tab %>%
  mutate(across(where(is.character), ~case_when(
    . == "Yes" ~ "yes",
    . == "No"  ~ "no",
    TRUE ~ .
  )))

first_tab$EHR_prescribed <- ifelse(first_tab$EHR_prescribed == 'EHR recorded' | first_tab$EHR_prescribed == 'Prescribed', 'EHR recorded', 'Not EHR recorded')
first_tab$BAME <- ifelse(first_tab$BAME == 'Not BAME' | first_tab$BAME == 'Not in BAME group', 'Not in BAME group', 'In BAME group')
first_tab$Sex <- ifelse(first_tab$Sex == 'U', NA, first_tab$Sex)

sum1 <- summary(univariateTable(cohort ~ Age + Sex + BMI_class + BAME + Q(IMD_rank) + Artherosclerotic_cardiovascular_disease + OSA + T2DM + Hypertension + Dyslipidaemia + EHR_prescribed + Tirzepatide_EHR + Semaglutide_EHR + Dulaglutide_EHR + Liraglutide_EHR + Exenatide_EHR, compare.groups = TRUE, show.totals = TRUE, column.percent = TRUE, data=first_tab, Q.format = "median(x) [iqr(x)]"))
sum1$Row <- seq_len(nrow(sum1))

#do loop 
vars <- c('Age','Sex', 'BMI_class', 'BAME', 'IMD_rank', 'Artherosclerotic_cardiovascular_disease', 'OSA', 'T2DM', 'Hypertension', 'Dyslipidaemia', 'EHR_prescribed', 'Tirzepatide_EHR', 'Semaglutide_EHR', 'Dulaglutide_EHR','Liraglutide_EHR','Exenatide_EHR')

test_results <- lapply(vars, function(v) {
  x <- first_tab[[v]]
  group <- first_tab$cohort
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(sum1, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
merged_tab$fdr <- p.adjust(merged_tab$P_value, method='fdr')
write.csv2(merged_tab, 'discovery_replication_cohort_bl_table.csv')

#============================================================================
#repeat analysis for EHR prescribed in discovery cohort
tab1 <- summary(univariateTable(EHR_prescribed ~ age_ehr + sex_ehr + BAME + bmi_class +Tirzepatide_EHR + Semaglutide_EHR+ Liraglutide_EHR + Dulaglutide_EHR + cardiovasc + HTN + dyslip + osa + T2DM + Q(IMD_rank), show.totals = TRUE, compare.groups = TRUE,  Q.format = "median(x) [iqr(x)]", data=bl1))
tab1_df <- as.data.frame(tab1)
tab1_df$Row <- seq_len(nrow(tab1_df))

vars <- c(
  "age_ehr", "sex_ehr", "BAME", "bmi_class", "Tirzepatide", "Liraglutide", "Semaglutide", "Dulaglutide", "cardiovasc",
  "HTN", "dyslip", "osa", "T2DM", "IMD_rank"
)

test_results <- lapply(vars, function(v) {
  x <- bl1[[v]]
  group <- bl1$EHR_prescribed
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]
write.csv2(merged_tab, file='EHR_recorded_GLP1_discovery_cohort.csv')

#final table
library(Publish)
library(tidyverse)
Replication_cohort_baseline_data <- readRDS("~/PhD/myfilepath/Replication_cohort_baseline_data.rds")

Replication_cohort_baseline_data$any_gp_indication <- ifelse(Replication_cohort_baseline_data$Tirzepatide_GP_indicated == 'TRUE' | Replication_cohort_baseline_data$Tirzepatide_GP_indicated_phase2 == 'TRUE' | Replication_cohort_baseline_data$Tirzepatide_GP_indicated_phase3 == 'TRUE', 'Indicated', 'Not indicated')
Replication_cohort_baseline_data$any_specialist_indication <- ifelse(Replication_cohort_baseline_data$Tirzepatide_specialist_indicated == 'TRUE' | Replication_cohort_baseline_data$Tirzepatide_T2DM == 'TRUE' | Replication_cohort_baseline_data$semaglutide_specialist_indicated == 'TRUE', 'Indicated', 'Not indicated')


bl_elig <- Replication_cohort_baseline_data

#now construct table! 
elig_tab <- summary(univariateTable(any_gp_indication ~ age + sex + bmi_class + BAME + Q(IMD_rank) + any_specialist_indication + Tirzepatide_T2DM + cardiovasc + osa +  T2DM + HTN + dyslip, Q.format = "median(x) [iqr(x)]", show.totals = TRUE, compare.groups = FALSE, column.percent = TRUE, data=bl_elig))

tab1_df <- as.data.frame(elig_tab)
tab1_df$Row <- seq_len(nrow(tab1_df))

vars <- c("age", "sex", "bmi_class", "BAME", "IMD_rank",
          "any_specialist_indication", "Tirzepatide_T2DM", "cardiovasc", "osa", "T2DM", "HTN", "dyslip")

test_results <- lapply(vars, function(v) {
  x <- bl_elig[[v]]
  group <- bl_elig$any_gp_indication
  
  valid <- complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  
  if (length(unique(group)) < 2 || length(x) == 0) return(NULL)
  if (is.numeric(x)) {
    # Wilcoxon (Mann–Whitney)
    test <- suppressWarnings(wilcox.test(x ~ group))
    data.frame(
      Variable = v,
      Test = "Wilcoxon rank-sum",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  } else {
    # Chi-square for categorical
    tbl <- table(x, group)
    test <- suppressWarnings(chisq.test(tbl))
    data.frame(
      Variable = v,
      Test = "Chi-square",
      Statistic = unname(test$statistic),
      P_value = test$p.value
    )
  }
})

test_df <- do.call(rbind, test_results)
merged_tab <- merge(tab1_df, test_df, by = "Variable", all.x = TRUE)
merged_tab <- merged_tab[order(merged_tab$Row), ]

write.csv2(merged_tab, file='table_for_differences_between_eligible_populations_validation.csv')

#==============================================
#get data for Rajenki
Baseline_data_Rajenki <- glpq %>% select(patient_study_identifier, bmi, cardiovasc, osa, HTN, T2DM, dyslip, Tirzepatide_GP_indicated, Tirzepatide_GP_indicated_phase2, Tirzepatide_GP_indicated_phase3, Tirzepatide_specialist_indicated, Tirzepatide_T2DM, semaglutide_specialist_indicated, glp1_use)
bl_rajenk2 <- Replication_cohort_baseline_data %>% select(patient_study_identifier, age, sex, IMD_rank, BAME)

Baseline_data_Rajenki <- merge(bl_rajenk2, Baseline_data_Rajenki, by='patient_study_identifier', all=T)

names(Baseline_data_Rajenki) <- c('id','Age','Sex','IMD_rank','In_BAME_group','BMI' ,'Cardiovascular_disease','OSA','Hypertension','Type_2_diabetes','Dyslipidaemia','GP_indicated_Tirzepatide_no_lifestyle_phase1','GP_indicated_Tirzepatide_no_lifestyle_phase2','GP_indicated_Tirzepatide_no_lifestyle_phase3','Specialist_indication_for_tirzepatide_no_lifestyle','Tirzepatide_indicated_for_T2DM','Semaglutide_specialist_indicated_no_lifestyle','EHR_recorded_GLP1_use')
Baseline_data_Rajenki$overall_GP_Tirzepatide_indication <- ifelse(Baseline_data_Rajenki$GP_indicated_Tirzepatide_no_lifestyle_phase1 == 'TRUE' | Baseline_data_Rajenki$GP_indicated_Tirzepatide_no_lifestyle_phase2 == 'TRUE' | Baseline_data_Rajenki$GP_indicated_Tirzepatide_no_lifestyle_phase3 == 'TRUE', 'Indicated', 'Not indicated')
summary(as.factor(Baseline_data_Rajenki$overall_GP_Tirzepatide_indication))
write_rds(Baseline_data_Rajenki, file='Replication_cohort_individual_level_data_for_Rajenki.rds')



#======================================================
#model
repl <- bl
repl$glp1_used <- as.factor(repl$glp1_used)
levels(repl$glp1_used)
repl <- repl %>% filter(!is.na(sex))
repl$sex <- as.factor(repl$sex)
levels(repl$sex)
repl <- repl %>% filter(sex != 'U')
repl$sex <- droplevels(repl$sex)

mod4 <- glm(glp1_used ~ age + sex + BAME + bmi_class + IMD_rank + cardiovasc + HTN + dyslip + osa + T2DM, family = 'binomial', data=repl)
summary(mod4)
drop1(mod4, test = "Chisq")
library(gtsummary)
or_table_mod4 <- tbl_regression(mod4, exponentiate = TRUE)
or_table_mod4


#=====================================================================================
#STEP 4 - plotting of OpenPrescribing data
#data on prescriptions
#setWD
setwd("C:/Users/Gebruiker/Documents/PhD/myfilepath")
library(tidyverse)
library(readxl)

#postcode files from: https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/ods-postcode-files
CMD_GLP1_postcodes <- read_excel("CMD_GLP1_postcodes.xlsx")

#ICB list mapped via: https://www.sbs.nhs.uk/supplier-information/ccg-icb-list/
pcodeall <- read.csv("~/PhD/myfilepath/pcodeall.csv")

#get mapping
ccg_icb_mapping <- read_excel("ICB postcodes/ccg_icb_mapping.xlsx")


pcodeall <- rbind(colnames(pcodeall), pcodeall)
colnames(pcodeall) <- c("post", "small_code", "ICB_code")
pcodeall$Postcode <- gsub(" ", "", pcodeall$post)

postcodes <- merge(CMD_GLP1_postcodes, pcodeall, by='Postcode')
#lost 12 postcodes --> acceptable

postcodes <- postcodes %>% group_by(small_code) %>% summarise(number_of_scripts = n())
#merge with ICB code
icb <- ccg_icb_mapping %>% select(ICBEntityCode, ICBNewName) %>% unique()

icb <- merge(icb, postcodes, by.x='ICBEntityCode', by.y='small_code')

icb$ICBNewName <- gsub("Integrated Care Board", "", icb$ICBNewName)
icb$ICBNewName <- gsub("[,\\-]+\\s*$", "", icb$ICBNewName)
icb$ICBNewName <- trimws(icb$ICBNewName)

write.csv2(icb, file='Scripts_mapped_to_icb.csv')

#===================================================================
#load in the Openprscribing data
Openprescribing_dulaglutide <- read_csv("~/PhD/myfilepath/ICB_dulaglutide.csv")
Openprescribing_Exenatidecsv <- read_csv("~/PhD/myfilepath/ICB_exenatide.csv")
Openprescribing_liragutide <- read_csv("~/PhD/myfilepath/ICB_liraglutide.csv")
Openprescribing_semaglutide <- read_csv("~/PhD/myfilepath/ICB_semaglutide.csv")
Openprescribing_tirzepatide <- read_csv("~/PhD/myfilepath/ICB_tirzepatide.csv")

glp <- rbind(Openprescribing_dulaglutide, Openprescribing_Exenatidecsv, Openprescribing_liragutide, Openprescribing_semaglutide, Openprescribing_tirzepatide)

glp_dates <- glp %>% group_by(date) %>% summarise(total_scripts = sum(quantity))

glp_dates$date <- as.Date(glp_dates$date, format = "%Y-%m-%d")
glp_dates$date <- floor_date(glp_dates$date, unit = "month")
glp_dates <- glp_dates %>% group_by(date) %>% summarise(total_scripts = sum(total_scripts))

#filter to only include up to November 2025
nov <- as.POSIXct("2025-12-01 00:00:00")
glp_dates <- glp_dates %>% filter(date < nov)
glp_dates$group <- 'OpenPrescribing'
feb <- as.POSIXct("2023-01-01 00:00:00")
glp_dates <- glp_dates %>% filter(date > feb)
Sys.setlocale("LC_TIME", "C")
library(scales)
library(ggpubr)

p_open <- ggplot(glp_dates, aes(x=date, y=total_scripts, colour=group)) +
  geom_point() +
  geom_line() +
  xlab('Month') +
  ylab('Number of scripts') +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) + 
  ggtitle('Prescribing trend of all GLP-1RAs - OpenPrescribing') +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ggarrange(p_tot, p_open, ncol=1)
#=========================================================
glp_tot <- glp %>% group_by(row_name) %>% summarise(scripts = sum(items))

icb_map <- ccg_icb_mapping %>%
  fill(ICBNewName, .direction = "down")

icb_map <- icb_map %>%
  mutate(CCGS = str_to_upper(CCGS),                 
         CCGS = str_replace_all(CCGS, "\\(.*?\\)", ""), 
         CCGS = str_replace_all(CCGS, "CCG", ""),   
         CCGS = str_squish(CCGS))

#add missing conversions here
#icb_map[108,1:4] <- list('x', 'NHS Bath and North East Somerset, Swindon and Wiltshire', 'x', 'NHS Bath and North East Somerset, Swindon and Wiltshire')
#icb_map[109,1:4] <- list('x', 'NHS Bedfordshire, Luton and Milton Keynes', 'x', 'NHS Bedfordshire, Luton and Milton Keynes')
#icb_map[110,1:4] <- list('x', 'NHS Black Country', 'x', 'NHS Black Country')
#icb_map[111,1:4] <- list('x', 'NHS Bristol, North Somerset and South Gloucestershire', 'x', 'NHS Bristol, North Somerset and South Gloucestershire')
icb_map[108,1:4] <- list('x', 'NHS Cornwall and The Isles of Scilly', 'x', 'NHS Cornwall and The Isles of Scilly')
#icb_map[113,1:4] <- list('x', 'NHS Shropshire, Telford and Wrekin', 'x', 'NHS Shropshire, Telford and Wrekin')

icb_map <- icb_map %>%
  mutate(CCGS = str_to_upper(CCGS),                 
         CCGS = str_replace_all(CCGS, "\\(.*?\\)", ""), 
         CCGS = str_replace_all(CCGS, "CCG", ""),   
         CCGS = str_squish(CCGS))

unmatched_glp <- glp_tot[!glp_tot$row_name %in% icb_map$CCGS, ]

glp_tot_clean <- glp_tot %>%
  mutate(row_name = case_when(
    row_name == "NHS BEDFORDSHIRE, LUTON AND MILTON KEYNES" ~ "NHS BEDFORDSHIRE, LUTON & MILTON KEYNES",
    row_name == "NHS BLACK COUNTRY" ~ "NHS BLACK COUNTRY AND WEST BIRMINGHAM",
    row_name == "NHS CASTLE POINT AND ROCHFORD" ~ "NHS CASTLEPOINT AND ROCHFORD",
    row_name == "NHS HERTS VALLEYS" ~ "NHS HERTS VALLEY",
    row_name == "NHS MID ESSEX" ~ "NHS MID-ESSEX",
    row_name == "NHS SHROPSHIRE, TELFORD AND WREKIN" ~ "NHS SHROPSHIRE TELFORD AND WREKIN",
    row_name == "NHS STOKE ON TRENT" ~ "NHS STOKE-ON-TRENT",
    # Using str_detect here in case "SEISDON PENINSULA" got cut off in your dataframe
    str_detect(row_name, "NHS SOUTH EAST STAFFORDSHIRE AND SEISDON") ~ "NHS SOUTH EAST STAFFORDSHIRE AND EASTERN PENINSULA",
    # Keep all other rows the same
    TRUE ~ row_name
  ))

glp_tot <- merge(glp_tot_clean, icb_map, by.x='row_name', by.y='CCGS')
glp_tot <- glp_tot %>% group_by(ICBNewName) %>% summarise(scripts = sum(scripts))

#load in population size
sapeicb20222024_1_ <- read_excel("sapeicb20222024 (1).xlsx", 
                                 +     sheet = "Mid-2024 ICB 2024")

icb_pop <- sapeicb20222024_1_
colnames(icb_pop) <- icb_pop[3,]
icb_pop <- icb_pop[-c(1:3),]
icb_pop <- icb_pop[,c(4,7)]
icb_pop$Total <- as.numeric(icb_pop$Total)

icb_pop_final <- icb_pop %>%
  mutate(`ICB 2024 Name` = str_remove(`ICB 2024 Name`, " Integrated Care Board")) %>%
  group_by(`ICB 2024 Name`) %>%
  summarise(Total_Population = sum(Total, na.rm = TRUE))

icb_pop_final$`ICB 2024 Name`[icb_pop_final$`ICB 2024 Name` == "NHS Cornwall and the Isles of Scilly"] <- "NHS Cornwall and The Isles of Scilly"

glp_icb <- merge(glp_tot, icb_pop_final, by.x='ICBNewName', by.y='ICB 2024 Name')
unmatched_in_glp <- anti_join(glp_tot, icb_pop_final, by = c("ICBNewName" = "ICB 2024 Name"))

#get population adjusted numbers
glp_icb$scripts_population_adjusted <- glp_icb$scripts/glp_icb$Total_Population

#also do Tirzepatide
Tirzepat <- Openprescribing_tirzepatide

Tirzepat <- Tirzepat %>%
  mutate(row_name = case_when(
    row_name == "NHS BEDFORDSHIRE, LUTON AND MILTON KEYNES" ~ "NHS BEDFORDSHIRE, LUTON & MILTON KEYNES",
    row_name == "NHS BLACK COUNTRY" ~ "NHS BLACK COUNTRY AND WEST BIRMINGHAM",
    row_name == "NHS CASTLE POINT AND ROCHFORD" ~ "NHS CASTLEPOINT AND ROCHFORD",
    row_name == "NHS HERTS VALLEYS" ~ "NHS HERTS VALLEY",
    row_name == "NHS MID ESSEX" ~ "NHS MID-ESSEX",
    row_name == "NHS SHROPSHIRE, TELFORD AND WREKIN" ~ "NHS SHROPSHIRE TELFORD AND WREKIN",
    row_name == "NHS STOKE ON TRENT" ~ "NHS STOKE-ON-TRENT",
    # Using str_detect here in case "SEISDON PENINSULA" got cut off in your dataframe
    str_detect(row_name, "NHS SOUTH EAST STAFFORDSHIRE AND SEISDON") ~ "NHS SOUTH EAST STAFFORDSHIRE AND EASTERN PENINSULA",
    # Keep all other rows the same
    TRUE ~ row_name
  ))

glp_tirz <- merge(Tirzepat, icb_map, by.x='row_name', by.y='CCGS')
glp_tirz <- glp_tirz %>% group_by(ICBNewName) %>% summarise(scripts = sum(items))
glp_tirz <- merge(glp_tirz, icb_pop_final, by.x='ICBNewName', by.y='ICB 2024 Name')
unmatched_in_glp <- anti_join(glp_tot, icb_pop_final, by = c("ICBNewName" = "ICB 2024 Name"))
glp_tirz$scripts_population_adjusted <- glp_tirz$scripts/glp_tirz$Total_Population

write.csv2(glp_icb, file='ICB_for_GLP1RA_OpenPrescribing.csv')
write.csv2(glp_tirz, file='ICB_for_Tirzepatide_OpenPrescribing.csv')
#note: unable to map: NHS Bath and North East Somerset, Swindon and Wiltshire
#NHS Bedfordshire, Luton and Milton Keynes
#NHS Bristol, North Somerset and South Gloucestershire
#NHS Shropshire, Telford and Wrekin
#these names do not occur in the GLP-1 data from OpenPrescribing?



#####################################################################################################
library(scales)
library(ggpubr)
#plot scripts for trizepatide
Openprescribing_dulaglutide_scripts <- read_csv("~/PhD/myfilepath/OpenPrescribing/overall_dulaglutide.csv")
Openprescribing_Exenatide_scripts <- read_csv("~/PhD/myfilepath/OpenPrescribing/overall_exenatide.csv")
Openprescribing_liragutide_scripts <- read_csv("~/PhD/myfilepath/OpenPrescribing/overall_liraglutide.csv")
Openprescribing_semaglutide_scripts <- read_csv("~/PhD/myfilepath/OpenPrescribing/overall_semaglutide.csv")
Openprescribing_Tirzepatide_scripts <- read_csv("~/PhD/myfilepath/OpenPrescribing/overall_tirzepatide.csv")

Update_1_medication_Umed <- read_excel("Umed_feb26_meds.xlsx")
tirzepatide <- Update_1_medication_Umed %>% filter(substance == 'Tirzepatide (substance)')

#load in the data from openprescribing
tirzepatide$date <- as.Date(tirzepatide$min_date, format = "%Y-%m-%d")
tirzepatide$date <- floor_date(tirzepatide$date, unit = "month")
tirzepatide <- tirzepatide %>% group_by(date) %>% summarise(number_of_scripts = n())
tirzepatide$group <- 'AccessCMD'


open_script <- Openprescribing_Tirzepatide_scripts
open_script$group <- 'OpenPrescribing'
open_script$date <- as.Date(open_script$date)
open_script <- open_script %>% select(date, items, group)
names(open_script) <- c('date', 'number_of_scripts', 'group')
s1 <- rbind(tirzepatide, open_script)

tirzepatide <- merge(tirzepatide, open_script[,c(1,3)], by='date', all=T)
tirzepatide$number_of_scripts <- ifelse(is.na(tirzepatide$number_of_scripts), 0, tirzepatide$number_of_scripts)

#filter to only include up to July 2025
nov <- as.POSIXct("2025-12-01 00:00:00")

#tirzepatide <- tirzepatide %>% filter(date < nov)
tirzepatide$group <- 'AccessCMD'
Sys.setlocale("LC_TIME", "C")

#combine in one script
max_uMed  <- max(tirzepatide$number_of_scripts, na.rm = TRUE)
max_open  <- max(open_script$number_of_scripts, na.rm = TRUE)

# Scale OpenPrescribing values to uMed range
scale_factor <- max_uMed / max_open

open_script <- open_script %>%
  mutate(scaled_scripts = number_of_scripts * scale_factor)


s_all <- bind_rows(
  tirzepatide %>% mutate(scaled_scripts = number_of_scripts),
  open_script
)


p <- ggplot() +
  geom_line(data = s_all,
            aes(x = date,
                y = scaled_scripts,
                colour = group)) +
  geom_point(data = s_all,
             aes(x = date,
                 y = scaled_scripts,
                 colour = group)) +
  
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  scale_y_continuous(
    name = "AccessCMD scripts",
    labels = comma,
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "OpenPrescribing scripts",
                        labels = comma)
  ) +
  
  ggtitle("Tirzepatide prescribing Trends: AccessCMD & OpenPrescribing") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#re-do the plot
tirzepatide <- tirzepatide %>%
  select(date, number_of_scripts, group)

open_script <- open_script %>%
  select(date, number_of_scripts, group)

max_uMed  <- max(tirzepatide$number_of_scripts, na.rm = TRUE)
max_open  <- max(open_script$number_of_scripts, na.rm = TRUE)

scale_factor <- max_uMed / max_open

tirzepatide <- tirzepatide %>%
  mutate(scaled_scripts = number_of_scripts)

open_script <- open_script %>%
  mutate(scaled_scripts = number_of_scripts * scale_factor)

combined <- bind_rows(tirzepatide, open_script)

p_combined_tirzepatide <- ggplot(combined, aes(x = date)) +
  
  geom_line(aes(y = scaled_scripts, colour = group)) +
  geom_point(aes(y = scaled_scripts, colour = group)) +
  
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  scale_y_continuous(
    name = "AccessCMD scripts",
    labels = comma,
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "OpenPrescribing scripts",
      labels = comma
    )
  ) +
  
  ggtitle("Tirzepatide Prescribing Trends - AccessCMD vs OpenPrescribing") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, , vjust = 0.5))

p_combined_tirzepatide


s1 <- rbind(tirzepatide[,c(1,2,3)], open_script[,1:3])
s1$date_num <- as.numeric(s1$date)
library(lme4)
model_lmm <- lmer(number_of_scripts ~ date_num * group + (1 | date), data = s1)
anova(model_lmm)
summary(model_lmm)


library(lmerTest)
model_lmm <- lmer(number_of_scripts ~ date_num * group + (1 | date), data = s1)
summary(model_lmm)
anova(model_lmm)


#medication plot of all GLP-1 agonists in uMed
meds_umed <- Update_1_medication_Umed %>% filter(umed_drug_class %in% c('Glucagon-like Peptide-1 (GLP-1) Agonists', 'Glucose-dependent Insulinotropic Polypeptide (GIP) and Glucagon-like Peptide-1 (GLP-1) Receptor Agonist'))
#summary(as.factor(umed_medication$umed_drug_class))
meds_umed$group <- 'uMed'
meds_umed$date <- as.Date(meds_umed$min_date, format = "%Y-%m-%d")
meds_umed$date <- floor_date(meds_umed$date, unit = "month")
meds_umed <- meds_umed %>% group_by(date) %>% summarise(number_of_scripts = n())

tirzepatide <- meds_umed

feb <- as.POSIXct("2023-02-01 00:00:00")
#tirzepatide <- tirzepatide %>% filter(date < nov)
tirzepatide <- tirzepatide %>% filter(date >= feb)
tirzepatide$group <- 'AccessCMD'
Sys.setlocale("LC_TIME", "C")


glp <- rbind(Openprescribing_dulaglutide_scripts, Openprescribing_Exenatide_scripts, Openprescribing_liragutide_scripts, Openprescribing_semaglutide_scripts, Openprescribing_Tirzepatide_scripts)
glp_dates <- glp %>% group_by(date) %>% summarise(total_scripts = sum(items))

glp_dates$date <- as.Date(glp_dates$date, format = "%Y-%m-%d")
glp_dates$date <- floor_date(glp_dates$date, unit = "month")
glp_dates <- glp_dates %>% group_by(date) %>% summarise(total_scripts = sum(total_scripts))

#filter to only include up to nov 2025
glp_dates <- glp_dates %>% filter(date < nov)
glp_dates$group <- 'OpenPrescribing'
feb <- as.POSIXct("2023-01-01 00:00:00")
glp_dates <- glp_dates %>% filter(date > feb)
Sys.setlocale("LC_TIME", "C")
library(scales)
library(ggpubr)

max_uMed  <- max(tirzepatide$number_of_scripts, na.rm = TRUE)
max_open  <- max(glp_dates$total_scripts, na.rm = TRUE)

scale_factor <- max_uMed / max_open

# Add scaled column to OpenPrescribing so both can be plotted on same axis
glp_dates <- glp_dates %>%
  mutate(scaled_scripts = total_scripts * scale_factor)

# uMed needs no scaling (scaled = original)
tirzepatide <- tirzepatide %>%
  mutate(scaled_scripts = number_of_scripts)

#--------------------------------------------
# 3. Combine for plotting
#--------------------------------------------

combined <- bind_rows(tirzepatide, glp_dates)

#--------------------------------------------
# 4. Plot with secondary axis
#--------------------------------------------

p_combined <- ggplot(combined, aes(x = date)) +
  
  geom_line(aes(y = scaled_scripts, colour = group)) +
  geom_point(aes(y = scaled_scripts, colour = group)) +
  
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  scale_y_continuous(
    name = "AccessCMD scripts",
    labels = comma,
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "OpenPrescribing scripts",
                        labels = comma)
  ) +
  
  ggtitle("OM Prescribing Trends – AccessCMD vs OpenPrescribing") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_combined
p_total_combind <- p_combined

ggarrange(p_combined_tirzepatide, p_total_combind)

#==========================================================================
#Get number of new uMed Tirzepatide scripts since guidance
n_unique <- Update_1_medication_Umed %>% select(patient_study_identifier) %>% unique()


Replication_cohort_baseline_data <- readRDS("~/PhD/myfilepath/Replication_cohort_baseline_data.rds")
discovery_cohort_baseline_data <- readRDS("~/PhD/myfilepath/discovery_cohort_baseline_data.rds")

cohorts <- rbind(Replication_cohort_baseline_data[,c(3,2)], discovery_cohort_baseline_data[,c(1,16)])
cohorts <- cohorts %>% select(patient_study_identifier) %>% unique()
n_unique <- n_unique %>% filter(patient_study_identifier %in% cohorts$patient_study_identifier)
#so, verified, all patients with medication data are also in the cohorts but no extra patients 

umed_tirz <- Update_1_medication_Umed %>% filter(substance == 'Tirzepatide (substance)')
umed_tirz$date <- as.Date(umed_tirz$min_date, format = "%Y-%m-%d")

umed_tirz_first <- umed_tirz %>%
  group_by(patient_study_identifier) %>%
  summarise(first_prescription_date = min(min_date, na.rm = TRUE)) %>%
  ungroup() 

umed_tirz_first2 <- umed_tirz_first

jun23 <- as.POSIXct("2025-06-23 00:00:00")

umed_tirz_first <- umed_tirz_first %>% filter(first_prescription_date > jun23)
umed_tirz_first <- unique(umed_tirz_first)

eligible_patients <- discovery_cohort_baseline_data %>% select(patient_study_identifier, Tirzepatide_via_GP) %>% filter(Tirzepatide_via_GP == 'GP indiciation for Tirzepatide')
eligible_patients2 <- Replication_cohort_baseline_data %>% select(patient_study_identifier, overall_indication) %>% filter(overall_indication == 'indicated')

elig_list <- c(eligible_patients$patient_study_identifier, eligible_patients2$patient_study_identifier)
#375 patients eligible
#of the users how many are eligible
umed_tirz_first$eligibility <- ifelse(umed_tirz_first$patient_study_identifier %in% elig_list, 'GP indicaiton', 'No GP indication')
summary(as.factor(umed_tirz_first$eligibility))


umed_tirz_first2$eligibility <- ifelse(umed_tirz_first2$patient_study_identifier %in% elig_list, 'GP indication', 'No GP indication')
Discovery_cohort_individual_level_data_for_Rajenki <- readRDS("~/PhD/myfilepath/Discovery_cohort_individual_level_data_for_Rajenki.rds")

specialist <- Replication_cohort_baseline_data %>% select(patient_study_identifier, specialist_indication, MHRA_tirzepatide_or_semaglutide)
spec2 <- Discovery_cohort_individual_level_data_for_Rajenki%>% select(id, Specialist_indication_for_tirzepatide_no_lifestyle, Semaglutide_specialist_indicated_no_lifestyle)

spec2 <- spec2 %>% filter(Specialist_indication_for_tirzepatide_no_lifestyle == 'TRUE' | Semaglutide_specialist_indicated_no_lifestyle == 'TRUE') %>% select(id) %>% unique()
spec1 <- specialist %>% filter(specialist_indication == 'Specialist indicated') %>% select(patient_study_identifier) %>% unique()

names(spec2) <- c('patient_study_identifier')

specialists <- c(spec1$patient_study_identifier, spec2$patient_study_identifier)

umed_tirz_first2$eligibility_spec <- ifelse(umed_tirz_first2$patient_study_identifier %in% specialists, 'Specialist indication', 'No specialist indication')



#plot number of people started per month
umed_tirz_first2$date <- floor_date(umed_tirz_first2$first_prescription_date, unit = "month")
umed_tirz_first2 <- umed_tirz_first2 %>% group_by(date, eligibility) %>% summarise(number_of_people_started = n())
Sys.setlocale("LC_TIME", "C")

ggplot() +
  geom_line(data = umed_tirz_first2,
            aes(x = date,
                y = number_of_people_started,
                colour = eligibility)) +
  geom_point(data = umed_tirz_first2,
             aes(x = date,
                 y = number_of_people_started,
                 colour = eligibility)) +
  scale_colour_manual(values = c("orange", "blue")) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y") +
  scale_y_continuous(
    name = "People initiated on Tirzepatide") +
  ggtitle("People started on Tirzepatide in AccessCMD") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(as.factor(umed_tirz_first2$eligibility_spec))

m2 <- merge(umed_tirz_first, umed_tirz_first2, by=c('patient_study_identifier', 'first_prescription_date', 'eligibility'))
summary(as.factor(m2$eligibility_spec))
