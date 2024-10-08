#############################################
# MAM02 Assignment Data Science
#############################################

# Authors: Robin and Matias

library(dplyr)

# get the data
diabetes <- read.csv("diabetic_data_initial.csv", stringsAsFactors = FALSE)

# Replace missing values in 'race' and 'medical_specialty'
diabetes$race[diabetes$race == "?"] <- "Missing"
diabetes$medical_specialty[diabetes$medical_specialty == "?"] <- "Missing"

# Remove 'weight' and 'payer_code' columns due to high missing values
diabetes <- diabetes %>% select(-weight, -payer_code)

# Keep only the first encounter for each patient
diabetes <- diabetes %>%
  arrange(patient_nbr, encounter_id) %>%
  group_by(patient_nbr) %>%
  slice(1) %>%
  ungroup()

# Remove encounters that resulted in death or discharge to hospice.
# Note to myself: This needs another look. Not sure about the choices made.
exclude_discharge_ids <- c(11, 13, 14, 19, 20, 21)
diabetes <- diabetes %>%
  filter(!(discharge_disposition_id %in% exclude_discharge_ids))

# Create the readmitted yes-no variable
diabetes$readmitted_binary <- ifelse(diabetes$readmitted == "<30", "Yes", "No")

# Map 'A1Cresult' and 'change' to HbA1c groups
diabetes$HbA1c_group <- with(diabetes, ifelse(
  A1Cresult == "None", "No test was performed",
  ifelse(A1Cresult == "Norm" | A1Cresult == ">7", "Normal result",
         ifelse(A1Cresult == ">8" & change == "No", "Result was high but diabetic medication was not changed",
                ifelse(A1Cresult == ">8" & change == "Ch", "Result was high and diabetic medication was changed", "No test was performed")))
))

# The gender values
diabetes$gender[diabetes$gender == "Unknown/Invalid"] <- NA

# discharge disposition id to groups
diabetes$discharge_disposition_group <- ifelse(
  diabetes$discharge_disposition_id == 1, "Discharged to home", "Otherwise"
)

# admission_source_id to groups
diabetes$admission_source_group <- ifelse(
  diabetes$admission_source_id == 7, "Admitted from emergency room",
  ifelse(diabetes$admission_source_id %in% c(1, 2), "Admitted because of physician/clinic referral", "Otherwise")
)

# Map 'medical_specialty' to groups
diabetes$medical_specialty_group <- with(diabetes, ifelse(
  medical_specialty == "InternalMedicine", "Internal Medicine",
  ifelse(medical_specialty == "Cardiology", "Cardiology",
         ifelse(grepl("Surgery", medical_specialty), "Surgery",
                ifelse(medical_specialty == "Family/GeneralPractice", "Family/general practice",
                       ifelse(medical_specialty == "Missing", "Missing or unknown", "Other"))))
))

# Function to map 'diag_1' codes to disease categories
map_diag_to_category <- function(diag_code) {
  if (diag_code == "?") {
    return("Other")
  } else if (startsWith(diag_code, "V") || startsWith(diag_code, "E")) {
    return("Other")
  } else {
    code_num <- suppressWarnings(as.numeric(diag_code))
    if (is.na(code_num)) {
      return("Other")
    } else if (code_num >= 390 & code_num <= 459 | code_num == 785) {
      return("Circulatory system disease")
    } else if (code_num == 250) {
      return("Diabetes")
    } else if (code_num >= 460 & code_num <= 519 | code_num == 786) {
      return("Respiratory system disease")
    } else if (code_num >= 520 & code_num <= 579 | code_num == 787) {
      return("Digestive system disease")
    } else if (code_num >= 800 & code_num <= 999) {
      return("Injury and poisoning")
    } else if (code_num >= 710 & code_num <= 739) {
      return("Musculoskeletal/connective tissue disease")
    } else if (code_num >= 580 & code_num <= 629 | code_num == 788) {
      return("Genitourinary system disease")
    } else if (code_num >= 140 & code_num <= 239) {
      return("Neoplasms")
    } else {
      return("Other")
    }
  }
}

# Apply the function to 'diag_1'
diabetes$diag_1_category <- sapply(diabetes$diag_1, map_diag_to_category)

# Map 'race' to groups
diabetes$race_group <- with(diabetes, ifelse(
  race == "AfricanAmerican", "African American",
  ifelse(race == "Caucasian", "Caucasian",
         ifelse(race == "Missing", "Missing", "Other"))
))

# Map 'age' to groups
diabetes$age_group <- with(diabetes, ifelse(
  age %in% c("[0-10)", "[10-20)", "[20-30)"), "30 years old or younger",
  ifelse(age %in% c("[30-40)", "[40-50)", "[50-60)"), "30-60 years old", "Older than 60")
))

# compute counts and percentages for each variable
compute_table3 <- function(data, group_var) {
  data %>%
    group_by_at(group_var) %>%
    summarise(
      Number_of_Encounters = n(),
      Percent_of_Population = round((n() / nrow(diabetes)) * 100, 1),
      Readmitted_Number_of_Encounters = sum(readmitted_binary == "Yes"),
      Percent_in_Group = round((Readmitted_Number_of_Encounters / Number_of_Encounters) * 100, 1)
    ) %>%
    arrange(desc(Number_of_Encounters))
}

# HbA1c
HbA1c_table <- compute_table3(diabetes, "HbA1c_group")
print("HbA1c:")
print(HbA1c_table)

# Gender
gender_table <- compute_table3(diabetes, "gender")
print("Gender:")
print(gender_table)

# Discharge disposition
discharge_table <- compute_table3(diabetes, "discharge_disposition_group")
print("Discharge disposition:")
print(discharge_table)

# Admission source
admission_source_table <- compute_table3(diabetes, "admission_source_group")
print("Admission source:")
print(admission_source_table)

# Medical specialty
medical_specialty_table <- compute_table3(diabetes, "medical_specialty_group")
print("Specialty of the admitting physician:")
print(medical_specialty_table)

# Primary diagnosis
diag1_table <- compute_table3(diabetes, "diag_1_category")
print("Primary diagnosis:")
print(diag1_table)

# Race
race_table <- compute_table3(diabetes, "race_group")
print("Race:")
print(race_table)

# Age
age_table <- compute_table3(diabetes, "age_group")
print("Age:")
print(age_table)
