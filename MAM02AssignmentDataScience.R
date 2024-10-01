#############################################
# MAM02 Assignment Data Science
#############################################

#############################################
# Import the data
#############################################

# Set your working directory to change to the folder where you downloaded the data file
# Thus replace the pathname H:/Downloads/ with the folder containing the data file
# Note that in Windows you have to use '/' instead of the normal '\' in the folder pathname
setwd("H:/Downloads/")

# Import the data file
data_diabetic <- read.csv(file="diabetic_data_initial.csv", header=TRUE, sep=",")

# Let's quickly check the first few rows of the data
head(data_diabetic)

# Let's check the names of the variables (i.e., column names)
names(data_diabetic)

# How many rows are there?
dim(data_diabetic)[1]
# your answer: <fill in>

# Are these all unique encounters?
length(unique(data_diabetic$encounter_id))
# your answer: <fill in>

# And are they all unique patients?
length(unique(data_diabetic$patient_nbr))
# your answer: <fill in>

#############################################
# Select data
#############################################

# We have to remove some data according to the paper (read section 2.3 carefully)
# "... used only one encounter per patient ..."
# I found the way to do this on stackoverflow.com (remember this site because you can find a lot of answers on R)
# https://stackoverflow.com/questions/19944334/extract-rows-for-the-first-occurrence-of-a-variable-in-a-data-frame#19944458
data_diabetic_first_encounters <- data_diabetic[match(unique(data_diabetic$patient_nbr), data_diabetic$patient_nbr),]
# "... removed all encounters that resulted in either discharge to a hospice or patient death ..."
# We use the dplyr library to filter the data
library("dplyr")
# The discharge_disposition_id column contains info on the discharge
# In id_mapping.csv we find discharge IDs 11 (Expired), 13 (Hospice / home), 14 (Hospice / medical facility)
names_removed_discharge_disposition_id <- c(11,13,14)
# Thus we remove these rows from the data
data_diabetic_selected = filter(data_diabetic_first_encounters, !data_diabetic_first_encounters$discharge_disposition_id %in% names_removed_discharge_disposition_id)

# Do we now have 69,984 encounters as mentioned in the paper?
dim(data_diabetic_selected)[1]
# your answer: <fill in>

# And let's quickly check if we now only have unique patients
length(unique(data_diabetic_selected$patient_nbr))
# your answer: <fill in>

#############################################
# Calculate descriptive statistics
#############################################

# Assuming that we have selected the data properly, we will show you the calculation of a single row from Table 3
# We will calculate the numbers for: Gender = Female

# Female: Number of encounters
Female_Number_of_encounters <- table(data_diabetic_selected$gender)[1]

# Female: % of the population
Population_size <- dim(data_diabetic_selected)[1]
Female_Percentage_of_population <- round(Female_Number_of_encounters / Population_size * 100, 1)

# Female Readmitted: Number of encounters
Female_Readmitted_Number_of_encounters <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$gender)[1]

# Female Readmitted: % in group
round(Female_Readmitted_Number_of_encounters / Female_Number_of_encounters * 100, 1)

#############################################
# From here on, you're on your own!
# Complete the source file such that
#############################################

# We will calculate the numbers for: Gender = Male
# <include your code here>

# calculate numbers for: HbA1c
# <include your code here>

# calculate numbers for: Discharge disposition
# <include your code here>

# et cetera

# Good luck!
