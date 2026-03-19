# Modelling eligibility for Tirzepatide - CLEAN VERSION
# Rajenki Das
# v1.0, 6th March 2026

# Libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)


# Values
N_england <- 46437085 # 18+ population in England (men and women population from ONS mid-2024)

# -----------------------------
# Data import
# -----------------------------
disc_cohort <- Discovery_cohort_individual_level_data_for_Rajenki
repl_cohort <- Replication_cohort_individual_level_data_for_Rajenki

# -----------------------------
# Cleaning and renaming
# -----------------------------
# -- Discovery
names(disc_cohort)[names(disc_cohort) == "GP_indication_for_Tirzepatide_no_lifestyle_requirement_overall"] <- "Tirzepatide_GP_noLR" # LR is lifestyle requirement
names(disc_cohort)[names(disc_cohort) == "GP_indication_for_Tirzepatide_lifestyle_requirement_overall"] <- "Tirzepatide_GP_LR"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase1"] <- "Tirzepatide_GP_noLR_ph1"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase2"] <- "Tirzepatide_GP_noLR_ph2"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase3"] <- "Tirzepatide_GP_noLR_ph3"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase1"] <- "Tirzepatide_GP_LR_ph1"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase2"] <- "Tirzepatide_GP_LR_ph2"
names(disc_cohort)[names(disc_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase3"] <- "Tirzepatide_GP_LR_ph3"
names(disc_cohort)[names(disc_cohort) == "Specialist_indication_for_tirzepatide_with_lifestyle"] <- "Tirzepatide_Special_LR"
names(disc_cohort)[names(disc_cohort) == "Specialist_indication_for_tirzepatide_no_lifestyle"] <- "Tirzepatide_Special_noLR"

disc_cohort <- within(disc_cohort,{
  In_BAME_group <- ifelse(In_BAME_group == "BAME", 1, 0)
  Cardiovascular_disease <- ifelse(Cardiovascular_disease == "Yes", 1, 0)
  OSA <- ifelse(OSA == "Yes", 1, 0)
  Hypertension <- ifelse(Hypertension == "Yes", 1, 0)
  Type_2_diabetes <- ifelse(Type_2_diabetes == "Yes", 1, 0)
  Dyslipidaemia <- ifelse(Dyslipidaemia == "Yes", 1, 0)
  Tirzepatide_GP_noLR <- ifelse(Tirzepatide_GP_noLR == "GP indication", 1, 0)
  Tirzepatide_GP_LR <- ifelse(Tirzepatide_GP_LR == "GP indication", 1, 0)
  Tirzepatide_GP_noLR_ph1 <- ifelse(Tirzepatide_GP_noLR_ph1 == "TRUE", 1, 0)
  Tirzepatide_GP_noLR_ph2 <- ifelse(Tirzepatide_GP_noLR_ph2 == "TRUE", 1, 0)
  Tirzepatide_GP_noLR_ph3 <- ifelse(Tirzepatide_GP_noLR_ph3 == "TRUE", 1, 0)
  Tirzepatide_GP_LR_ph1 <- ifelse(Tirzepatide_GP_LR_ph1 == "TRUE", 1, 0)
  Tirzepatide_GP_LR_ph2 <- ifelse(Tirzepatide_GP_LR_ph2 == "TRUE", 1, 0)
  Tirzepatide_GP_LR_ph3 <- ifelse(Tirzepatide_GP_LR_ph3 == "TRUE", 1, 0)
})

disc_cohort$BMI_category <- cut(
  disc_cohort$BMI,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese"),
  right = FALSE
)

table(disc_cohort$BMI_category, useNA = "ifany")

# -- Replication
# names(repl_cohort)[names(repl_cohort) == "GP_indication_for_Tirzepatide_no_lifestyle_requirement_overall"] <- "Tirzepatide_GP_noLR" # LR is lifestyle requirement
names(repl_cohort)[names(repl_cohort) == "GP_indication_for_Tirzepatide_lifestyle_requirement_overall"] <- "Tirzepatide_GP_LR"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase1"] <- "Tirzepatide_GP_noLR_ph1"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase2"] <- "Tirzepatide_GP_noLR_ph2"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_no_lifestyle_phase3"] <- "Tirzepatide_GP_noLR_ph3"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase1"] <- "Tirzepatide_GP_LR_ph1"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase2"] <- "Tirzepatide_GP_LR_ph2"
names(repl_cohort)[names(repl_cohort) == "GP_indicated_Tirzepatide_lifestyle_phase3"] <- "Tirzepatide_GP_LR_ph3"

names(repl_cohort)[names(repl_cohort) == "Specialist_indication_for_tirzepatide_no_lifestyle"] <- "Tirzepatide_Special_noLR"
names(repl_cohort)[names(repl_cohort) == "overall_GP_Tirzepatide_indication"] <- "Tirzepatide_GP_noLR"

repl_cohort <- within(repl_cohort,{
  In_BAME_group <- ifelse(In_BAME_group == "BAME", 1, 0)
  Cardiovascular_disease <- ifelse(Cardiovascular_disease == "Yes", 1, 0)
  OSA <- ifelse(OSA == "Yes", 1, 0)
  Hypertension <- ifelse(Hypertension == "Yes", 1, 0)
  Type_2_diabetes <- ifelse(Type_2_diabetes == "Yes", 1, 0)
  Dyslipidaemia <- ifelse(Dyslipidaemia == "Yes", 1, 0)
  Tirzepatide_GP_noLR <- ifelse(Tirzepatide_GP_noLR == "Indicated", 1, 0)
  # Tirzepatide_GP_LR <- ifelse(Tirzepatide_GP_LR == "GP indication", 1, 0)
  Tirzepatide_GP_noLR_ph1 <- ifelse(Tirzepatide_GP_noLR_ph1 == "TRUE", 1, 0)
  Tirzepatide_GP_noLR_ph2 <- ifelse(Tirzepatide_GP_noLR_ph2 == "TRUE", 1, 0)
  Tirzepatide_GP_noLR_ph3 <- ifelse(Tirzepatide_GP_noLR_ph3 == "TRUE", 1, 0)
})


# Additional cleaning of other columns
disc_cohort$Sex[disc_cohort$Sex %in% c("", "NA", "N/A")] <- NA
repl_cohort$Sex[repl_cohort$Sex %in% c("", "NA", "N/A")] <- NA

disc_cohort$Sex <- factor(disc_cohort$Sex, levels = c("F","M","U"))
repl_cohort$Sex <- factor(repl_cohort$Sex, levels = c("F","M","U"))

ons_age_band <- function(age){
  cut(
    age,
    breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,Inf),
    labels = c(
      "0 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
      "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
      "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89","90 and over"
    ),
    right = FALSE
  )
}

disc_cohort$age_band <- ons_age_band(disc_cohort$Age)
repl_cohort$age_band <- ons_age_band(repl_cohort$Age)

# --- Combining the two cohorts
common_cols <- intersect(colnames(disc_cohort), colnames(repl_cohort))

combined_cohort <- rbind(
  cbind(disc_cohort[common_cols], cohort = "disc"),
  cbind(repl_cohort[common_cols], cohort = "repl")
)

combined_cohort$cohort <- factor(combined_cohort$cohort)

summary(disc_cohort)
summary(repl_cohort)
summary(combined_cohort)

################################ INPUT DATA ####################################

comorb_names <- c("Cardiovascular_disease", 
                  "OSA", 
                  "Hypertension", 
                  "Type_2_diabetes", 
                  "Dyslipidaemia")

# England data

#p_eng <- c(10.96, 5.868, 23.4, 7.92, 23.6) # CVD_Source I
p_eng <- c(6.58, 5.868, 23.4, 7.92, 23.6)  # CVD_Source II

p_eng <- p_eng/100

p_eng_M <- c(12, 4.1, 26, 10, 45 )
p_eng_M <- p_eng_M/100

p_eng_F <- c(10, 7.5, 21, 6, 50)
p_eng_F <- p_eng_F/100

#########################################################################################
##########                           Estimation                                ##########
#########################################################################################

data <- combined_cohort %>%
  transmute(
    BMI = as.numeric(BMI),   
    X1 = as.integer(Cardiovascular_disease),
    X2 = as.integer(OSA),
    X3 = as.integer(Hypertension),
    X4 = as.integer(Type_2_diabetes),
    X5 = as.integer(Dyslipidaemia)
  ) #%>%
  # mutate(
  #   n_comorb = X1 + X2 + X3 + X4 + X5,
  #   eligible =
  #     (BMI > 35 & n_comorb >= 4) | 
  #     (BMI > 40 & n_comorb == 3)
#  )

##### ----------- Eligible patterns ------------------ #####

# PART I:  BMI > 35 with >=4 comorbS AND BMI > 40 with exactly 3 comorbs

all_patterns <- expand.grid(X1 = 0:1, X2 = 0:1, X3 = 0:1, X4 = 0:1, X5 = 0:1)
elig_patterns <- list()

for(i in 1:nrow(all_patterns)) {
  pat <- as.numeric(all_patterns[i, ])
  n_comorb <- sum(pat)
  
  if(n_comorb >= 4) {
    name <- paste0("35_", paste0(pat, collapse = ""))
    elig_patterns[[name]] <- pat
  }
  
  if(n_comorb == 3) {
    name <- paste0("40_", paste0(pat, collapse = ""))
    elig_patterns[[name]] <- pat
  }
}


# PART II: eligible patterns: BMI > 35 or > 40 with >=1 comorb ---

all_patterns <- expand.grid(X1 = 0:1, X2 = 0:1, X3 = 0:1, X4 = 0:1, X5 = 0:1)
all_patterns <- all_patterns[rowSums(all_patterns) >= 1, ] # at least one comorb

elig_patterns <- list()

for(i in 1:nrow(all_patterns)) {
  
  pat <- as.numeric(all_patterns[i, ])
  
  elig_patterns[[paste0("35_", paste0(pat, collapse=""))]] <- pat
  elig_patterns[[paste0("40_", paste0(pat, collapse=""))]] <- pat
}
################################################################################
# Estimation function
pattern_comorb_est <- function(pat, pat_name, j, data, p_eng) {
  
  if (pat[j] != 1) return(NA)
  
  bmi_check <- if (startsWith(pat_name, "35_")) 35 else if (startsWith(pat_name, "40_")) 40 else NA
  if (is.na(bmi_check)) stop("Pattern name must start with 35_ or 40_")
  
  Xj <- paste0("X", j)
  
  den <- sum(data[[Xj]] == 1, na.rm = TRUE)
  if (den == 0) return(NA)
  
  num <- sum(data$BMI > bmi_check &
               data$X1 == pat[1] &
               data$X2 == pat[2] &
               data$X3 == pat[3] &
               data$X4 == pat[4] &
               data$X5 == pat[5], na.rm = TRUE)
  
  p_eng[j] * num / den # formula
}

# Matrix of pattern × comorb estimates
est_mat <- matrix(NA, nrow = length(elig_patterns), ncol = 5)
colnames(est_mat) <- paste0("X", 1:5)
rownames(est_mat) <- names(elig_patterns)

i <- 1
for (nm in names(elig_patterns)) {
  pat <- elig_patterns[[nm]]
  for (j in 1:5) {
    est_mat[i, j] <- pattern_comorb_est(pat, 
                                        nm, j, 
                                        data = data, p_eng = p_eng)
  }
  i <- i + 1
}

est_mat

# Convert to data frame
est_df <- as.data.frame(est_mat)
colnames(est_df) <- paste0(comorb_names)
est_df$Pattern <- rownames(est_mat)
rownames(est_df) <-NULL
est_df <- est_df[, c("Pattern", comorb_names)] # moving Pattern to col 1
est_df[, -1] <- round(est_df[, -1]*100, 3) # converting to % and rounding off

#  Summarise
for (nm in comorb_names) est_df[[nm]] <- as.numeric(est_df[[nm]])
num_part <- est_df[, -1] # Removing Patterns
est_df$Median <- apply(num_part, 1, median, na.rm = TRUE)
est_df <- est_df[order(est_df$Pattern), ]

est_df

est35<- est_df[startsWith(est_df$Pattern, "35"), ]
sum(est35$Median)

est40<- est_df[startsWith(est_df$Pattern, "40"), ]
sum(est40$Median)

write.csv(est_df, "est_df_min1comorb_source2.csv", row.names = FALSE)


# -------------------- validation ---------------------------------------------#
bmi_check <- 35  
bmi_joint <- data.frame(Comorbidity = paste0("X",1:5), P_AccessCMD = NA,
  P_England = p_eng, Joint_Prob = NA, Joint_Percent = NA
)

for (j in 1:5){
  
  Xj <- paste0("X", j)
  
  num <- sum(data$BMI > bmi_check & data[[Xj]] == 1, na.rm = TRUE)
  den <- sum(data[[Xj]] == 1, na.rm = TRUE)
  p_cmd <- num / den
  joint <- p_cmd * p_eng[j]
  
  bmi_joint$P_AccessCMD[j] <- p_cmd
  bmi_joint$Joint_Prob[j] <- joint
  bmi_joint$Joint_Percent[j] <- 100 * joint
}

# Summarise
validation_table <- data.frame(
  BMI = paste0(">", bmi_check),
  t(bmi_joint$Joint_Percent),
  Median = median(bmi_joint$Joint_Percent, na.rm = TRUE)
)

colnames(validation_table) <- c("BMI", comorb_names, "Median")

validation_table


write.csv(bmi_joint, "bmi40_joint_validation.csv", row.names = FALSE)
write.csv(validation_table, "bmi40_validation_summary.csv", row.names = FALSE)
