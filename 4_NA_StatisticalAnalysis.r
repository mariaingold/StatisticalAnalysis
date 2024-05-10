# Statististical Analysis Assignment
# Numerical Analysis End of Module Assessment
# UoEO MSc AI
#
# Author: Maria Ingold
# Date: 2024-06-03 (Assignment due date)
#
# USAGE: Run script in RStudio, with working directory set to file location.
# INPUT: Loads dataset: Health Survey For England (HSE) 2011 (HSE_2011.sav)
# OUTPUT: Outputs descriptive statistics and visualisation of the dataset.
#
# HSE DATASET ANALYSIS
# Alcohol causes hospital admissions and deaths
# Analysis: gender, region, income, age, and alcohol consumption
#
# STEPS
# STEP 1: SETUP
# STEP 2: DESCRIPTIVE STATISTICS
#
# VERSIONS

##############################################################################
# STEP 1: SETUP
# a: Load libraries, set working directory, load dataset
# b: Basic dataset exploration
##############################################################################

# LIBRARIES

if (!require(pacman)) {install.packages("pacman")} # Package management
pacman::p_load(tidyverse,                          # To manipulate data
               haven,                              # To read sav, as_factor
               rstudioapi,                         # For file directory
               psych)                              # For describe

# Set working directory to this file location
# Ensure dataset in same directory
# Only works in RStudio
if (rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# DATA

# Load Dataset
hse_data <- read_sav("HSE_2011.sav")

# Basic dataset exploration
# Rows: 10,617
# Columns: 58
View(hse_data)       # View dataset
glimpse(hse_data)    # Structure of dataset
summary(hse_data)    # Summary of dataset

# Create list of names with thier labels for reference
hse_names <- names(hse_data)
hse_labels <- sapply(hse_data,
                 function(x) {
                   if (is.labelled(x))
                     attr(x, "label")
                   else
                     NA  # No label
                   }
                 )
names_labels_df <- data.frame(Variable = hse_names, Label = hse_labels, stringsAsFactors = FALSE)
rownames(names_labels_df) <- NULL # Otherwise Variable appears twice.
print(names_labels_df)

#FUNCTIONS

# Mode function
mode <- function(x) {
  ux <- unique(x)                 # Eliminate duplicates
  tab <- tabulate(match(x, ux))   # Create frequency table
  ux[tab == max(tab)]             # Return mode(s)
}

# Mode function for continuous random variables
mode_crv <- function(x) {
  den <- density(x, na.rm = TRUE) # Estimated density plot returns x and y
  den$x[which.max(den$y)]         # Return first mode (estimated x at max y)
}

##############################################################################
# STEP 2: DESCRIPTIVE STATISTICS
# a: How many people are included in the sample? (pserial)
# b: What is the percentage of people who drink alcohol? (dnnow)
# c: What is the percentage of women in the sample? (sex)
# d: What is the highest educational level? (topqual3)
# e: What is percentage of divorced and separated people? (marstatc)
# f: Find the mean, median, mode, minimum, maximum, range and
#    standard deviation of household size, BMI and age at last birthday.
#    (HHSize, bmival, Age)
#
# Variables
# pserial: Serial number of Individual
# dnnow: Whether drink nowadays: Yes (1), No (2): categorical
# Sex: Sex: Male (1), Female (2): categorical
# topqual3: Highest educational level: categorical
# marstatc: Marital status including cohabitees 4 = Separated, 5 = Divorced
# HHSize: Household size
# bmival: Body Mass Index
# Age: Age at last birthday (some ages equal 0, due to babies under 12 months)
##############################################################################

# a: How many people are included in the sample?
# ==> Outcome: 10,617 people are included in the sample.
# pserial is the "Serial number of Individual"
# There are no empty rows (NA)
# This matches the number of rows in the dataset.
any(is.na(hse_data$pserial))    # Check for empty rows (NA)
sum(!is.na(hse_data$pserial))   # Number of not empty rows

# b: What is the percentage of people who drink alcohol? (dnnow)
# ==> Outcome: 78.65% of people who responded yes or no drink alcohol.
# dnnow is "Whether drink nowadays"
# Yes (1), No (2)
# Number empty rows (NA) ==> 2083
# Frequency table ==> Yes = 6712, No = 1822
# Percentage ==> Yes = 78.65%, No = 21.35%
# Percentage Yes = (6712 / (6712 + 1822)) * 100
sum(is.na(hse_data$dnnow))                              # Number empty rows (NA)
table(as_factor(hse_data$dnnow))                        # Frequency table
table(as_factor(hse_data$dnnow)) %>% prop.table() * 100 # Percentage

# c: What is the percentage of women in the sample? (Sex)
# ==> Outcome: 54.30% of the sample are women.
# Male (1), Female (2)
# Number of empty rows (NA) ==> 0
# Frequency table ==> Male = 4852, Female = 5765
# Percentage Female = 54.29971
# Percentage Female = (5765 / (4852 + 5765)) * 100
sum(is.na(hse_data$Sex))                                # Number empty rows (NA)
table(as_factor(hse_data$Sex))                          # Frequency table
table(as_factor(hse_data$Sex)) %>% prop.table() * 100   # Percentage

# d: What is the highest educational level? (topqual3)
# ==> Outcome: The highest educational level is NVQ4/NVQ5/Degree or equiv (1)
# topqual3 is "Highest educational level"
# NVQ4/NVQ5/Degree or equiv (1)
# Higher ed below degree (2)
# NVQ3/GCE A Level equiv (3)
# NVQ2/GCE O Level equiv (4)
# NVQ1/CSE other grade equiv (5)
# Foreign/other (6)
# No qualification (7)
attr(hse_data$topqual3, "labels") # Labels

# e: What is percentage of divorced and separated people? (marstatc)
# ==> Outcome: 9.50 % of the sample are divorced and separated.
# marstatc is "Marital status including cohabitees"
# Single (1)
# Married (2)
# Civil partnership including spontaneous answers (3)
# Separated (4)
# Divorced (5)
# Widowed (6)
# Cohabitees (7)
attr(hse_data$marstatc, "labels") # Labels
sum(is.na(hse_data$marstatc))                              # NA
table(as_factor(hse_data$marstatc))                        # Frequency table
table(as_factor(hse_data$marstatc)) %>% prop.table() * 100 # Percentage
div_sep <- sum(hse_data$marstatc == 4 | hse_data$marstatc == 5, na.rm = TRUE)
div_sep                                                    # Total div and sep
div_sep / sum(table(as_factor(hse_data$marstatc))) * 100   # Percentage

# f: Find the mean, median, mode, minimum, maximum, range and standard deviation
#    of household size, BMI and age at last birthday (HHSize, bmival, Age)

# HHSize: Household size
# Number empty rows (NA) = 0
# ==> Outcome:
# Mean = 2.85
# Median = 3
# Mode = 2
# Min = 1
# Max = 10
# Range = 9
# Standard Deviation = 1.37
sum(is.na(hse_data$HHSize)) # Number empty rows (NA)
summary(hse_data$HHSize)    # Summary statistics
describe(hse_data$HHSize)   # Descriptive statistics
mode(hse_data$HHSize)       # Mode

# bmival: Body Mass Index
# Number empty rows (NA) = 2241
# ==> Outcome:
# Mean = 25.92
# Median = 25.59
# Mode = 25.00 (Continuous random variable, density plot estimate)
# Min = 8.34
# Max = 65.28
# Range = 56.94
# Standard Deviation = 6.14
sum(is.na(hse_data$bmival)) # Number empty rows (NA)
summary(hse_data$bmival)    # Summary statistics
describe(hse_data$bmival)   # Descriptive statistics
# mode() function does not work for continuous random variables, need PDF plot
# Plot density
# Creates a right-skewed, single mode plot
hse_data %>%
  ggplot(aes(x = bmival)) +
  geom_density(fill = "gray", alpha=0.5) +
  theme_bw() +
  labs(title = "BMI Density plot",
       x = "BMI",
       y = "Density")
# There is only one mode, so use the density plot to estimate it
mode_crv(hse_data$bmival)   # Estimated mode

# Age: Age at last birthday (some ages equal 0, due to babies under 12 months)
# Number empty rows (NA) = 0
# ==> Outcome:
# Mean = 41.56
# Median = 42
# Mode = 64 and 42 (validated with frequency table)
# Min = 0
# Max = 100
# Range = 100
# Standard Deviation = 23.83
sum(is.na(hse_data$Age)) # Number empty rows (NA)
summary(hse_data$Age)    # Summary statistics
describe(hse_data$Age)   # Descriptive statistics
mode(hse_data$Age)       # Mode

table(hse_data$Age)      # 2 modes, frequency table confirms

# There are some 0 ages (babies under 12 months), so let's plot outliers
# No outliers. And frequency table confirms. 
# Plot box plot 
hse_data %>%
  ggplot(aes(y = Age)) +
  geom_boxplot(fill = "gray", outlier.color = "red", na.rm = TRUE) +
  labs(title = "Age Boxplot",
       y = "Age") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels
