# =========================================
# Section 1: SHARE Data Background
# =========================================

# Setting directory to where the downloaded SHARE files are
setwd("/Users/jacobellen/dropbox/Frailty/SHARE2")
# Example: "/Users/jacobellen/dropbox/Frailty/SHARE2"

# **Accessing Data**
# - Download SHARE Wave 1 and Wave 2 in SPSS format.
# - Place the data in a folder named "SHARE" on the desktop.
# - Use releases 8.0.0: https://releases.sharedataportal.eu/releases

# **Notes on Coding Frailty**
# - Frailty variables are distributed throughout the datasets and processed as follows:
#   1. Clean each variable as it is pulled from the respective dataset.
#   2. Compile and organize variables in the final dataset, ensuring they are aligned.

# **Frailty Definition Reference**
# - Based on Romero-Otuno et al. (2010) BMC Geriatrics (https://pubmed.ncbi.nlm.nih.gov/20731877/).
# - Variables selected as per Santos-Eggimann et al.

# =========================================
# Frailty Criteria
# =========================================

# 1. **Exhaustion**
#    - Question: "In the last month, have you had too little energy to do the things you wanted to do?"
#    - Coding:
#       - Yes = 1 (Presence of exhaustion)
#       - No = 0 (Absence of exhaustion)

# 2. **Weight Loss**
#    - Based on responses to two questions:
#      a) "What has your appetite been like?" (Look for "Diminution in desire for food")
#      b) "Have you been eating more or less than usual?" (Look for "Less")
#    - Coding:
#       - Criterion met = 1
#       - Criterion not met = 0

# 3. **Weakness**
#    - Measured by handgrip strength using a dynamometer.
#    - Two consecutive measurements taken for each hand; the highest value is selected.
#    - Variable is kept continuous.

# 4. **Slowness**
#    - Based on difficulty in two tasks:
#      a) Walking 100 meters
#      b) Climbing one flight of stairs without resting
#    - Coding:
#       - 1 = One or two positive answers (Presence of slowness)
#       - 0 = Both answers negative (Absence of slowness)

# 5. **Low Activity**
#    - Question: "How often do you engage in activities that require low/moderate energy (e.g., gardening, walking)?"
#    - Coding: Ordinal variable
#       - 1 = More than once a week
#       - 2 = Once a week
#       - 3 = One to three times a month
#       - 4 = Hardly ever or never

# =========================================
# Deficit Frailty Score (DFS) Calculations
# =========================================

# **Female DFS Formula**
# DFS = (2.077707 * Fatigue − 0.757295) * 0.4088 +
#       (3.341539 * Loss of appetite - 0.332289) * 0.3325 +
#       (0.132827 * Grip strength − 3.534515) * -0.4910 +
#       (2.627085 * Functional difficulties − 0.461808) * 0.6012 +
#       (0.918866 * Physical activity − 1.523633) * 0.4818

# **Male DFS Formula**
# DFS = (2.280336 * Fatigue − 0.592393) * 0.3762 +
#       (4.058274 * Loss of appetite - 0.263501) * 0.3130 +
#       (0.092326 * Grip strength − 3.986646) * 0.4653 +
#       (3.098226 * Functional difficulties − 0.365971) * 0.6146 +
#       (1.005942 * Physical activity − 1.571803) * 0.4680

# =========================================
# Frailty Classification
# =========================================

# **For Females**
# - NON-FRAIL: DFS < 0.3151
# - PRE-FRAIL: 0.3151 ≤ DFS < 2.1301
# - FRAIL: DFS ≥ 2.1301

# **For Males**
# - NON-FRAIL: DFS < 1.2119
# - PRE-FRAIL: 1.2119 ≤ DFS < 3.0053
# - FRAIL: DFS ≥ 3.0053



# =========================================
# Section 1: SHARE Data Processing
# =========================================

#Libraries
library(tidyverse)
library(sjlabelled)
library(haven)
library(ggplot2)
library(dplyr)

##### WAVE 1 #####
cv_r_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_cv_r.sav")
ph_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_ph.sav")
br_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_br.sav")
mh_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_mh.sav")
hc_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_hc.sav")
gs_backup <- read_sav("sharew1_rel8-0-0_ALL_datasets_spss/sharew1_rel8-0-0_gs.sav")

#getting age and gender from cover screen
cv <- cv_r_backup %>% select(mergeid, gender, mobirth, yrbirth, age_int, int_month, 
                             int_year)

#getting physical health data from physical health module
ph <- ph_backup %>% select(mergeid, starts_with("ph006"), 
                           starts_with("ph011"), ph012_, ph013_, ph048d1, ph048d5)

#coding functional difficulties frailty criteria 
ph <- ph %>% mutate(functional_difficulties = case_when(ph048d1 == 1 | ph048d5 == 1 ~ 1,
                                         ph048d1 == 0 & ph048d5 == 0 ~ 0))
set_label(ph$functional_difficulties) <- "Functional difficulties"

#removing values in weight that are less than or equal to 10 kg 
#this includes removing values for -1 or -2, which are the real missing values, and positive values that seem incompatible with life)
ph <- ph %>% mutate(ph012_ = ifelse(ph012_ <= 10, NA, ph012_))
set_label(ph$ph012_) <- "Weight of respondent"

#removing values in height that are -1 or -2
ph <- ph %>% mutate(ph013_ = ifelse(ph013_ %in% c(-1, -2), NA, ph013_))
set_label(ph$ph013_) <- "Height of respondent"

#creating BMI
ph$bmi <- (10000 * ph$ph012_)/(ph$ph013_ * ph$ph013_)
set_label(ph$bmi) <- "BMI"

#getting alcohol and tobacco from behavioral risk factor module, as well as physical activity frailty variable
br <- br_backup %>% select(mergeid, br001_, br010_, br016_) %>%
  mutate(br001_ = case_when(br001_ == 5 ~ 0, #changing 5's to 0's
                            br001_ == 1 ~ 1)) %>%
  mutate(br010_ = ifelse(br010_ %in% c(-1, -2), NA, br010_)) %>% #removing -1's and -2's from alcohol days in week
  mutate(physical_activity = case_when(br016_ == 1 ~ 1, #copying variable but in doing so removing -1's and -2's
                                       br016_ == 2 ~ 2,
                                       br016_ == 3 ~ 3,
                                       br016_ == 4 ~ 4)) %>%
  select(-br016_) #removing initial physical activity variable

#reapplying label because it falls off
set_label(br$br001_) <- "Ever smoked daily"
set_label(br$br010_) <- "Days a week consumed alcohol last 6 months"
set_label(br$physical_activity) <- "Physical activity"

#getting lifetime depression from mental health module, as well as fatigue and reduced appetite frailty variables
mh <- mh_backup %>% select(mergeid, mh011_, mh012_, mh013_, mh018_) %>%
  mutate(fatigue = case_when(mh013_ == 5 ~ 0, #changing 5's to 0's
                             mh013_ == 1 ~ 1)) %>%
  select(-mh013_) %>% #removing initial variable
  mutate(mh018_ = case_when(mh018_ == 5 ~ 0, #changing 5's to 0's
                            mh018_ == 1 ~ 1))
set_label(mh$fatigue) <- "Fatigue"
set_label(mh$mh018_) <- "Depression ever"

#cleaning reduced appetite variable as per frailty coding instructions below
mh <- mh %>% mutate(reduced_appetite = case_when(mh011_ == 1 | mh012_ == 1 ~ 1,
                                                 mh011_ == 2 ~ 0,
                                                 mh011_ == 3 & mh012_ != 1 ~ 0))
set_label(mh$reduced_appetite) <- "Reduced appetite"
  
#getting hospitalizations, encounters from healthcare utilization module
hc <- hc_backup %>% select(mergeid, hc002_, hc012_, hc023_, hc029_) %>%
  mutate(hc012_ = case_when(hc012_ == 5 ~ 0, #changing 5's to 0's
                            hc012_ == 1 ~ 1)) %>%
  mutate(hc023_ = case_when(hc023_ == 5 ~ 0, #changing 5's to 0's
                            hc023_ == 1 ~ 1)) %>%
  mutate(hc029_ = case_when(hc029_ == 5 ~ 0, #changing 5's to 0's,
                            hc029_ == 3 ~ 1, #3 and  1 represent temporary and permanent nursing home status
                            hc029_ == 1 ~ 1))
set_label(hc$hc012_) <- "Stayed overnight in hospital last 12 months"
set_label(hc$hc023_) <- "Had outpatient surgery in last 12 months"
set_label(hc$hc029_) <- "In a nursing home during last 12 months"

#getting grip strength
gs <- gs_backup %>% select(mergeid, gs006_:gs009_)

#finding highest value, as per frailty criteria
gs <- gs %>% mutate(grip_strength = pmax(gs006_, gs007_, gs008_, gs009_, na.rm = TRUE))
set_label(gs$grip_strength) <- "Maximum grip strength"

#merging all the wave 1 variables into one dataset
wave_one <- cv %>% 
  right_join(ph) %>% #doing right join for ph, because cv was the screening dataset and will have people who don't compete other surveys
  left_join(br) %>% 
  left_join(mh) %>% 
  left_join(hc) %>% 
  left_join(gs)

#getting rid of duplicate datasets now that we've merged
rm(br, br_backup,cv,cv_r_backup,gs,gs_backup,hc,hc_backup,mh,mh_backup,ph,ph_backup)

#removing all variables that won't be used as predictors (i.e. things used to calculate frailty variables)
wave_one <- wave_one %>% select(-ph048d1, -ph048d5, -mh011_, -mh012_, -gs006_, -gs007_, -gs008_, -gs009_)

#putting all frailty variables at the end of the dataset
wave_one <- wave_one %>% relocate(physical_activity, fatigue, reduced_appetite, functional_difficulties, grip_strength, .after = last_col())

#calculating frailty raw scores
wave_one <- wave_one %>% mutate(frailty_raw = case_when(
  gender == 1 ~ (2.280336 * fatigue - 0.592393) * 0.3762 + (4.058274 * reduced_appetite - 0.263501) * 0.3130 + (0.092326 * grip_strength - 3.986646) * 0.4653 + (3.098226 * functional_difficulties - 0.365971) * 0.6146 + (1.005942 * physical_activity - 1.571803) * 0.4680,
  gender == 2 ~ (2.077707 * fatigue - 0.757295) * 0.4088 + (3.341539 * reduced_appetite - 0.332289) * 0.3325 + (0.132827 * grip_strength - 3.534515) * -0.4910 + (2.627085 * functional_difficulties - 0.461808) * 0.6012 + (0.918866 * physical_activity - 1.523633) * 0.4818
))

#checking distribution
ggplot(wave_one, aes(x = frailty_raw)) + geom_density()

#grouping into frailty categories
wave_one <- wave_one %>%
  mutate(frailty = ifelse(gender == 2,
                          ifelse(frailty_raw < 0.3151361243, "Non-frail",
                                 ifelse(frailty_raw < 2.1301121973, "Pre-frail",
                                        ifelse(frailty_raw < 6, "Frail", NA))),
                          ifelse(gender == 1,
                                 ifelse(frailty_raw < 1.211878526, "Non-frail",
                                        ifelse(frailty_raw < 3.0052612772, "Pre-frail",
                                               ifelse(frailty_raw < 7, "Frail", NA))),
                                 NA)))

#checking distribution
#compared to Romero-Otuno, where "the prevalence of frailty was 
#7.3% in females and 3.1% in males," 
#we find: 8.1% in females and 1.8% in males
wave_one %>%
  group_by(gender) %>%
  count(frailty) %>%
  mutate(percentage = 100 * (n / sum(n)))

#This isn't that incredibly far off, but why do we see that?
#Romero-Otuno write that their subjects were "17,304 females and 13,811 males included in the first wave of the Survey of Health, Aging and Retirement in Europe (SHARE, release 2.3.0 of November 13th, 2009)"
#However, release 8.0.0 shows 30419 consistent across all datasets except the screener. 
#17304 + 13811 = 31115, thus in other words at some point ~700 subjects were dropped from the files from release 2.3.0 to 8.0.0
#This may not perfectly explain discrepancy, but provides a source


##### WAVE 2 #####
#load data
cv_r_backup2 <- read_sav("sharew2_rel8-0-0_ALL_datasets_spss/sharew2_rel8-0-0_cv_r.sav")
ph_backup2 <- read_sav("sharew2_rel8-0-0_ALL_datasets_spss/sharew2_rel8-0-0_ph.sav")
br_backup2 <- read_sav("sharew2_rel8-0-0_ALL_datasets_spss/sharew2_rel8-0-0_br.sav")
mh_backup2 <- read_sav("sharew2_rel8-0-0_ALL_datasets_spss/sharew2_rel8-0-0_mh.sav")
gs_backup2 <- read_sav("sharew2_rel8-0-0_ALL_datasets_spss/sharew2_rel8-0-0_gs.sav")

#getting dates of follow up interview
cv2 <- cv_r_backup2 %>% 
  select(mergeid, int_month, int_year) %>%
  mutate(int_month2 = ifelse(int_month %in% c(-9), NA, int_month)) %>% #removing missing -9
  mutate(int_year2 = ifelse(int_year %in% c(-9), NA, int_year)) %>%    #removing missing -9
  select(-int_month, -int_year)
set_label(cv2$int_month) <- "Interview month WAVE 2"
set_label(cv2$int_year) <- "Interview year WAVE 2"

#getting physical health data from physical health module
ph2 <- ph_backup2 %>% select(mergeid, ph048d1, ph048d5)

#coding functional difficulties frailty criteria 
ph2 <- ph2 %>% mutate(functional_difficulties2 = case_when(ph048d1 == 1 | ph048d5 == 1 ~ 1,
                                                           ph048d1 == 0 & ph048d5 == 0 ~ 0))
set_label(ph2$functional_difficulties2) <- "Functional difficulties WAVE 2"
ph2 <- ph2 %>% select(mergeid, functional_difficulties2)

#getting physical activity frailty criteria from behavioral risk module
br2 <- br_backup2 %>% select(mergeid, br016_) %>%
  mutate(physical_activity2 = case_when(br016_ == 1 ~ 1, #copying variable but in doing so removing -1's and -2's
                                        br016_ == 2 ~ 2,
                                        br016_ == 3 ~ 3,
                                        br016_ == 4 ~ 4)) %>%
  select(-br016_) #removing initial physical activity variable
set_label(br2$physical_activity2) <- "Physical activity WAVE 2"

#getting reduced appetite and fatigue frailty variables from mental health module
mh2 <- mh_backup2 %>% select(mergeid, mh011_, mh012_, mh013_) %>%
  mutate(reduced_appetite2 = case_when(mh011_ == 1 | mh012_ == 1 ~ 1, #cleaning reduced appetite variable as per frailty coding
                                       mh011_ == 2 ~ 0,
                                       mh011_ == 3 & mh012_ != 1 ~ 0)) %>%
  mutate(fatigue2 = case_when(mh013_ == 5 ~ 0, #changing 5's to 0's
                              mh013_ == 1 ~ 1)) %>%
  select(-mh011_, -mh012_, -mh013_) #removing initial fatigue and appetite variables
set_label(mh2$fatigue2) <- "Fatigue WAVE 2"
set_label(mh2$reduced_appetite2) <- "Reduced appetite WAVE 2"
mh2 <- mh2 %>% select(mergeid, fatigue2, reduced_appetite2)

#getting grip strength from grip strength module
gs2 <- gs_backup2 %>% select(mergeid, gs006_:gs009_)

#finding highest value, as per frailty criteria
gs2 <- gs2 %>% mutate(grip_strength2 = pmax(gs006_, gs007_, gs008_, gs009_, na.rm = TRUE))
set_label(gs2$grip_strength2) <- "Maximum grip strength WAVE 2"

#removing individual values
gs2 <- gs2 %>% select(mergeid, grip_strength2)

#joining all into the same table
wave_two <- ph2 %>%
  left_join(cv2) %>%
  left_join(br2) %>% 
  left_join(mh2) %>% 
  left_join(gs2)

#also need gender from wave one to calculate frailty score
#any missing cases are due to new participants in wave two
wave_two <- wave_two %>% left_join(wave_one %>% select(mergeid, gender))

#calculating frailty raw scores
wave_two <- wave_two %>% mutate(frailty_raw2 = case_when(
  gender == 1 ~ (2.280336 * fatigue2 - 0.592393) * 0.3762 + (4.058274 * reduced_appetite2 - 0.263501) * 0.3130 + (0.092326 * grip_strength2 - 3.986646) * 0.4653 + (3.098226 * functional_difficulties2 - 0.365971) * 0.6146 + (1.005942 * physical_activity2 - 1.571803) * 0.4680,
  gender == 2 ~ (2.077707 * fatigue2 - 0.757295) * 0.4088 + (3.341539 * reduced_appetite2 - 0.332289) * 0.3325 + (0.132827 * grip_strength2 - 3.534515) * -0.4910 + (2.627085 * functional_difficulties2 - 0.461808) * 0.6012 + (0.918866 * physical_activity2 - 1.523633) * 0.4818
))

#checking distribution
ggplot(wave_two, aes(x = frailty_raw2)) +
  geom_density()

#grouping into frailty categories
wave_two <- wave_two %>%
  mutate(frailty2 = ifelse(gender == 2,
                          ifelse(frailty_raw2 < 0.3151361243, "Non-frail",
                                 ifelse(frailty_raw2 < 2.1301121973, "Pre-frail",
                                        ifelse(frailty_raw2 < 6, "Frail", NA))),
                          ifelse(gender == 1,
                                 ifelse(frailty_raw2 < 1.211878526, "Non-frail",
                                        ifelse(frailty_raw2 < 3.0052612772, "Pre-frail",
                                               ifelse(frailty_raw2 < 7, "Frail", NA))),
                                 NA)))


#checking distribution again
#compared to Romero-Otuno, where "the prevalence of frailty [in Wave 1] was 
#7.3% in females and 3.1% in males," we find:
#8.1% in females and 1.8% in males in Wave 1
#7.5% in females and 1.6% in males in Wave 2
wave_two %>%
  group_by(gender) %>%
  count(frailty2) %>%
  mutate(freq = 100 * (n / sum(n)))

#getting rid of duplicate datasets now that we've created our final wave 2 dataset
rm(br2,br_backup2,gs2,gs_backup2,mh2,mh_backup2,ph2,ph_backup2,cv2,cv_r_backup2)


# =========================================
# Section 3: Combined Waves 1 and 2 Data
# =========================================

##### FINAL DATASET #####
#merging waves into final dataset
df <- wave_one %>% 
  left_join(wave_two %>% 
  select(-gender)) %>%                                       #don't need recopied gender from wave one
  relocate(c("int_month2","int_year2"), .after = int_year)   #just moving these to put next to wave one

#removing -1's and -2's in the physical health section to NA, have to save and reapply labels
labels <- df %>% get_label()
df <- df %>%
  mutate(across(ph006d1:ph011dot, ~ifelse(. %in% c(-1, -2), NA, .)))
set_label(df) <- labels

# KGs to pounds and cms to inches
df$ph012_ <- df$ph012_ * 2.20462
df$ph013_ <- df$ph013_/2.54

# Calculate time between interviews for each patient
df <- df %>%
  mutate(
    year_difference = (int_year2 - int_year) * 12,  # Convert year difference to months
    Months_Between_Apppointments = year_difference + (int_month2 - int_month)) 

# Removing non-specific, 'other' variables
df <- df %>% dplyr::select(-c(ph006dot))

# Final dataset processing
share_final <- df %>%
  filter(!is.na(frailty2)) %>% # Keep rows with outcome frailty data 
  filter(!is.na(frailty)) %>%        # Keep rows with frailty data at baseline
  filter(frailty != "Frail") %>%    # Exclude already Frail patients at baseline
  filter(Months_Between_Apppointments > 12 & Months_Between_Apppointments < 48
         ) %>% # Filtering for patients who had between 1 and 4 year follow-up
  dplyr::select( # Removing unused variables
    -mergeid, -mobirth, -yrbirth, -ph006dno, -int_month, -int_year,
    -int_month2, -int_year2, -frailty_raw, -frailty_raw2,
    -physical_activity, -fatigue, -reduced_appetite, -functional_difficulties,
    -grip_strength, -physical_activity2, -fatigue2, -reduced_appetite2,
    -functional_difficulties2, -grip_strength2, -year_difference
  )

# Moving months between appointments column to middle of dataframe
share_final <- share_final %>% relocate(Months_Between_Apppointments, 
                                        .before = frailty)

# Renaming some columns for interpretability of modeling
colnames(share_final) <- c("gender", "age_baseline", "pmh_heart_attack", "pmh_hypertension",
                           "pmh_cholesterol", "pmh_stroke", "pmh_diabetes", "pmh_emphysema",
                           "pmh_asthma", "pmh_arthritis", "pmh_osteoporosis", "pmh_any_cancer",
                           "pmh_ulcer", "pmh_dementia", "pmh_eye", "pmh_fracture_hip",
                           "high_cholesterol_drug", "high_bloodpressure_drug", "coronary_disease_drug",
                           "otherheart_disease_drug",
                           "asthma_drug", "diabetes_drug","joint_pain_drug", "other_pain_drug",
                           "sleep_drug", "anxiety_depression", "osteoporosis_hormone_drug",
                           "osteoporosis_other_drug", "stomach_burn_drug", "bronchitis_drug",
                           "no_drugs", "drugs_for_other",
                           "vitals_measured_weight", "vitals_height", "vitals_BMI",
                           "ever_smoked_daily", "day_per_week_alcohol", "depression",
                           "how_often_seen_doctor", "hospital_stays_lastyear",
                           "outpatient_surgery", "nursing_home", "Months_Between_Appointments",
                           "Baseline_Frailty", "Frailty_Score")
share_final$gender <- share_final$gender-1 # Keeping formatting consistent


# =========================================
# Section 4: Final Dataset Processing
# =========================================

## Correlated Features
 numeric_columns <- share_final[sapply(share_final, is.numeric)]  # Extract numeric columns
 correlation_matrix <- cor(numeric_columns, use = "complete.obs") # Compute the correlation matrix
 high_corr <- which(abs(correlation_matrix) > 0.84 & lower.tri(correlation_matrix), arr.ind = TRUE) # Find highly correlated pairs
 high_corr_pairs <- data.frame(
   Var1 = rownames(correlation_matrix)[high_corr[, 1]],  # Get variable names for highly correlated pairs
   Var2 = colnames(correlation_matrix)[high_corr[, 2]], 
   Correlation = correlation_matrix[high_corr]
 )

## Variances of each variable
variances <- sapply(numeric_columns, var, na.rm = TRUE)  # Calculate variances
low_variance <- which(variances < 0.01)                 # Identify low-variance columns
low_variance_vars <- names(numeric_columns)[low_variance] # Get names of low-variance columns

## Remove highly correlated and low variance variables
# Combine variables to remove: unique variables from high_corr_pairs and low_variance_vars
vars_to_remove <- unique(c(high_corr_pairs$Var1, low_variance_vars))
share_final <- share_final[ , !(names(share_final) %in% vars_to_remove)]

# Formating Final Baseline Frailty Variable
share_final$Baseline_Frailty <- as.numeric(as.factor(share_final$Baseline_Frailty)) -1

# Remove patients with both missing height and weight
share_final <- share_final %>% 
  filter(!is.na(vitals_height) | !is.na(vitals_measured_weight))


# Missing Data Analysis

# Calculate percentage of missing values per column
missing_per_column <- share_final %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Percentage")

# Display the variables with their missing percentages
ggplot(missing_per_column, aes(x = reorder(Variable, -Missing_Percentage), y = Missing_Percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Missing Data Percentage per Variable", x = "Variable", y = "Missing Percentage") +
  theme_minimal()

# Filter variables with more than 10% missing values - None!
variables_to_remove <- missing_per_column %>%
  filter(Missing_Percentage > 40) %>%
  pull(Variable)

# Calculate percentage of missing values per row (patients)
share_final2 <- share_final %>%
  mutate(Missing_Percentage = rowMeans(is.na(.)) * 100)

# Display distribution of missing data across patients
ggplot(share_final2, aes(x = Missing_Percentage)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Missing Data Across Patients", x = "Missing Percentage", y = "Frequency") +
  theme_minimal()

# Removing patients with more than 10% missing values
missing_40 <- as.data.frame(share_final[-c(which(share_final2$Missing_Percentage > 40)), ])

# Note: no patients or variables with more than 40% missing, so none removed

# ===============================================
# Section 5: Final Data Summarizing and Saving
# ===============================================

# Summary of Data After All of these Steps
summary(share_final)

# Outcome variable Analysis
#share_final$Frailty_Score <- share_final$Frailty
frequency_table <- table(share_final$Frailty_Score, useNA = "always")
proportions <- prop.table(frequency_table) * 100

# Combine counts and percentages into a data frame
summary_combined <- data.frame(
  Category = names(frequency_table),
  Count = as.vector(frequency_table),
  Percentage = as.vector(proportions)
)
print(summary_combined)

# Visualize the Frailty_Score distribution using ggplot2
ggplot(share_final, aes(x = Frailty_Score)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Frailty Score",
    x = "Frailty Score",
    y = "Count"
  ) +
  theme_minimal()

# Baseline Frailty Analysis
frequency_table_baseline <- table(share_final$Baseline_Frailty)
proportions_baseline <- prop.table(frequency_table_baseline ) * 100

# Combine counts and percentages into a data frame
summary_combined_baseline <- data.frame(
  Category = names(frequency_table_baseline),
  Count = as.vector(frequency_table_baseline),
  Percentage = as.vector(proportions_baseline)
)
print(summary_combined_baseline)

# Age
mean(share_final$age_baseline)
sd(share_final$age_baseline)

#Gender
gender.table <- table(share_final$gender)
gender.proportions <- prop.table(gender.table) * 100
print(gender.proportions)

#Follow-up
mean(share_final$Months_Between_Appointments)
sd(share_final$Months_Between_Appointments)

# SAVING FINAL FILE
write.csv(share_final, file="/Users/jacobellen/dropbox/Preprocessed_SHARE.csv",
          row.names=FALSE)
