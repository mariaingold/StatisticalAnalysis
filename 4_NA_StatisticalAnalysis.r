##############################################################################
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
# STEP 3: INFERENTIAL STATISTICS
##############################################################################

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
               psych,                              # For describe
               ggVennDiagram)                      # For ggVennDiagram

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

# Create list of names with their label for reference
hse_names <- names(hse_data)
hse_labels <- sapply(hse_data,
                 function(x) {
                   if (is.labelled(x))
                     attr(x, "label")
                   else
                     NA  # No label
                   }
                 )
names_labels_df <- data.frame(Variable = hse_names,
                              Label = hse_labels,
                              stringsAsFactors = FALSE)
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

##############################################################################
# a: How many people are included in the sample?
# ==> Outcome: 10,617 people are included in the sample.
# pserial is the "Serial number of Individual"
# There are no empty rows (NA)
# There are no duplicates (TRUE means no duplicates)
# This matches the number of rows in the dataset.
##############################################################################
any(is.na(hse_data$pserial))       # Check for empty rows (NA)
all(!duplicated(hse_data$pserial)) # Check for duplicates
sum(!is.na(hse_data$pserial))      # Number of not empty rows

##############################################################################
# b: What is the percentage of people who drink alcohol? (dnnow)
# ==> Outcome: 78.65% of people who responded yes or no drink alcohol.
# dnnow is "Whether drink nowadays"
# Yes (1), No (2)
# Number empty rows (NA) ==> 2083
# Frequency table ==> Yes = 6712, No = 1822
# Percentage ==> Yes = 78.65%, No = 21.35%
# Percentage Yes = (6712 / (6712 + 1822)) * 100
##############################################################################
sum(is.na(hse_data$dnnow))                              # Number empty rows (NA)
table(as_factor(hse_data$dnnow))                        # Frequency table
dnnow_pct_tab <- table(as_factor(hse_data$dnnow)) %>%
  prop.table() * 100                                    # Percentage
dnnow_pct_tab

# Prep for pie chart
dnnow_pct_df <- dnnow_pct_tab %>%
  as.data.frame() %>%                        # Convert to data frame
  rename(name = Var1, percentage = Freq) %>% # Rename columns
  filter(percentage > 0)                     # Remove 0 values
dnnow_pct_df

# Y-axis label for pie chart
pct_yes <- dnnow_pct_df$percentage[1]     # Percentage people who drink
dnnow_responded <- sum(table(as_factor(hse_data$dnnow))) # Total responded
dnnow_y_label <- paste0(round(pct_yes, 1), "% of ",
                        dnnow_responded, " responents drink nowadays")

# Pie chart
dnnow_pct_df %>%
  ggplot(aes(x = "", y = percentage, fill = factor(name))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Alcohol Consumption",
       x = NULL,
       y = dnnow_y_label,
       fill = "Drink nowadays")

##############################################################################
# c: What is the percentage of women in the sample? (Sex)
# ==> Outcome: 54.30% of the sample are women.
# Male (1), Female (2)
# Number of empty rows (NA) ==> 0
# Frequency table ==> Male = 4852, Female = 5765
# Percentage Female = 54.29971
# Percentage Female = (5765 / (4852 + 5765)) * 100
##############################################################################
sum(is.na(hse_data$Sex))                                # Number empty rows (NA)
table(as_factor(hse_data$Sex))                          # Frequency table
table(as_factor(hse_data$Sex)) %>% prop.table() * 100   # Percentage

##############################################################################
# d: What is the highest educational level? (topqual3)
# ==> Outcome: The highest educational level is NVQ4/NVQ5/Degree or equiv (1)
# topqual3 is "Highest educational level"
# NVQ4/NVQ5/Degree or equiv (1) = 2008 people
# Higher ed below degree (2) = 948 people
# NVQ3/GCE A Level equiv (3) = 1248 people
# NVQ2/GCE O Level equiv (4) = 1803 people
# NVQ1/CSE other grade equiv (5) = 395 people
# Foreign/other (6) = 127 people
# No qualification (7) = 2037 people
# Number of empty rows (NA) ==> 2051
##############################################################################
attr(hse_data$topqual3, "labels")   # Labels
sum(is.na(hse_data$topqual3))       # Number empty rows (NA)
table(as_factor(hse_data$topqual3)) # Frequency table
topqual_pct_tab <- table(as_factor(hse_data$topqual3)) %>%
  prop.table() * 100                # Percentage
topqual_pct_tab

# Prep for pie chart
topqual_pct_df <- topqual_pct_tab %>%
  as.data.frame() %>%                        # Convert to data frame
  rename(name = Var1, percentage = Freq) %>% # Rename columns
  filter(percentage > 0)                     # Remove 0 values
topqual_pct_df

# Y-axis label for pie chart
pct_highest <- topqual_pct_df$percentage[1] # Percentage highest ed
topqual_responded <- sum(table(as_factor(hse_data$topqual3))) # Total responded
topqual_y_label <- paste0(round(pct_highest, 1), "% of ",
                  topqual_responded, " have NVQ4/NVQ/Degree/Equiv")

# Pie chart
topqual_pct_df %>%
  ggplot(aes(x = "", y = percentage, fill = factor(name))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Educational level",
       x = NULL,
       y = topqual_y_label,
       fill = "Educational level")

##############################################################################
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
# Number of empty rows (NA) ==> 2009
##############################################################################
attr(hse_data$marstatc, "labels") # Labels
sum(is.na(hse_data$marstatc))                              # NA
table(as_factor(hse_data$marstatc))                        # Frequency table
mar_pct_tab <- table(as_factor(hse_data$marstatc)) %>%
  prop.table() * 100                                       # Percentage
mar_pct_tab
div_sep <- sum(hse_data$marstatc == 4 | hse_data$marstatc == 5, na.rm = TRUE)
div_sep                                                    # Total div and sep
pct_div_sep <- div_sep / sum(table(as_factor(hse_data$marstatc))) * 100 # Pct

# Prep for pie chart
mar_pct_df <- mar_pct_tab %>%
  as.data.frame() %>%                        # Convert to data frame
  rename(name = Var1, percentage = Freq) %>% # Rename columns
  filter(percentage > 0)                     # Remove 0 values
mar_pct_df

# Y-axis label for pie chart
mar_responded <- sum(table(as_factor(hse_data$marstatc))) # Total responded
mar_y_label <- paste0(round(pct_div_sep, 1), "% of ",
                      mar_responded, " are divorced or separated")

# Pie chart
mar_pct_df %>%
  ggplot(aes(x = "", y = percentage, fill = factor(name))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Maritial status including cohabitees",
       x = NULL,
       y = mar_y_label,
       fill = "Marital status")

##############################################################################
# Try a Venn Diagram of dnnow = Yes (1), # topqual3 = NVQ4/NVQ5/Degree or equiv (1)
# and marstatc = Divirced (5) or Separated (4)
# Do one for Sex = Female (2)
# Do one for Sex = Male (1)
##############################################################################

# Create vectors for Venn Diagram
dnnow_yes_serials <- hse_data$pserial[hse_data$dnnow == 1]
dnnow_no_serials <- hse_data$pserial[hse_data$dnnow == 2]
female_serials <- hse_data$pserial[hse_data$Sex == 2]
male_serials <- hse_data$pserial[hse_data$Sex == 1] 
topqual_serials <- hse_data$pserial[hse_data$topqual3 == 1]
div_sep_serials <- hse_data$pserial[hse_data$marstatc %in% c(4, 5)]
serials_drink_female <- list("Drink" = dnnow_yes_serials,
                             "Female" = female_serials)
serials_drink_male <- list("Drink" = dnnow_yes_serials,
                            "Male" = male_serials)
serials_drink_mf <- list("Drink" = dnnow_yes_serials,
                         "Female" = female_serials,
                         "Male" = male_serials)
serials_nodrink_mf <- list ("No Drink" = dnnow_no_serials,
                            "Female" = female_serials,
                            "Male" = male_serials)
serials_list_female <- list("Drink" = dnnow_yes_serials,
                            "Female" = female_serials,
                            "Top Qualification" = topqual_serials,
                            "Div/Sep" = div_sep_serials)
serials_list_male <- list("Drink" = dnnow_yes_serials,
                          "Male" = male_serials,
                          "Top Qualification" = topqual_serials,
                          "Div/Sep" = div_sep_serials)

# Venn for male and female drinkers
vmfd <- ggVennDiagram(serials_drink_mf,
                     label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") + 
  ggtitle("Venn Diagram: Males and females who drink ")
vmfd + scale_x_continuous(expand = expansion(mult = .2)) # Show long labels

# Venn for male and female non-drinkers
vmfnd <- ggVennDiagram(serials_nodrink_mf,
                      label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") + 
  ggtitle("Venn Diagram: Males and females who do not drink ")
vmfnd + scale_x_continuous(expand = expansion(mult = .2)) # Show long labels

# Venn for females
vfd <- ggVennDiagram(serials_drink_female,
                     label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") + 
  ggtitle("Venn Diagram: Females who drink ")
vfd + scale_x_continuous(expand = expansion(mult = .2)) # Show long labels

ggVennDiagram(serials_list_female,
              label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") + 
  ggtitle("Venn Diagram for Females")

# Venn for males
vmd <- ggVennDiagram(serials_drink_male,
                     label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") + 
  ggtitle("Venn Diagram: Males who drink")
vmd + scale_x_continuous(expand = expansion(mult = .2)) # Show long labels

ggVennDiagram(serials_list_male,
              label_alpha = 0) +
  ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#4292c6") +
  ggtitle("Venn Diagram for Males")

##############################################################################
# f: Find the mean, median, mode, minimum, maximum, range and standard deviation
#    of household size, BMI and age at last birthday (HHSize, bmival, Age)
##############################################################################

##############################################################################
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
##############################################################################
sum(is.na(hse_data$HHSize)) # Number empty rows (NA)
summary(hse_data$HHSize)    # Summary statistics
describe(hse_data$HHSize)   # Descriptive statistics
mode(hse_data$HHSize)       # Mode
table(hse_data$HHSize)      # Frequency table

# Plot box plot (check for outliers)
hse_data %>% 
  ggplot(aes(y = HHSize)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "Household Size Box Plot",
       y = "Size") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

# Plot frequency table
hse_data %>% 
  mutate(HHSize = as.factor(HHSize)) %>% # Convert to factor for discrete
  ggplot(aes(x = HHSize)) +
  geom_bar(fill = "lightblue") +
  theme_bw() +
  labs(title = "Household Size Frequency Table",
       x = "Size",
       y = "Frequency")

##############################################################################
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
##############################################################################
sum(is.na(hse_data$bmival)) # Number empty rows (NA)
summary(hse_data$bmival)    # Summary statistics
describe(hse_data$bmival)   # Descriptive statistics

# Plot box plot (check for outliers)
hse_data %>%
  ggplot(aes(y = bmival)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "BMI Box Plot",
       y = "Size") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

# mode() function does not work for continuous random variables, need PDF plot
# Plot density
# Creates a right-skewed, single mode plot
hse_data %>%
  ggplot(aes(x = bmival)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  theme_bw() +
  labs(title = "BMI Density plot",
       x = "BMI",
       y = "Density")
# There is only one mode, so use the density plot to estimate it
mode_crv(hse_data$bmival)   # Estimated mode

##############################################################################
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
##############################################################################
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
  geom_boxplot(fill = "lightblue", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "Age Boxplot",
       y = "Age") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

# Plot histogram
hse_data %>%
  ggplot(aes(x = Age)) +
  geom_histogram(fill = "lightblue", binwidth = 1) +
  theme_bw() +
  labs(title = "Age Histogram",
       x = "Age",
       y = "Frequency") +
  scale_x_continuous(n.breaks = 10)

##############################################################################
# STEP 3: INFERENTIAL STATISTICS
# a.	Run a significance test to find out which gender drinks more alcohol.
# b.	Run a significance test to find out which region drinks the most alcohol.
# c.	Investigate whether there is a statistical difference between
#     men and women on the following variables:
#     I.	Valid height.
#     II.	Valid weight.
# d.	What is the correlation between whether a person drinks nowadays, total
#     household income, age at last birthday and gender?
##############################################################################

##############################################################################
# a.	Run a significance test to find out which gender drinks more alcohol.
# Variables
# dnnow: Whether drink nowadays: Yes (1), No (2): categorical
# Sex: Sex: Male (1), Female (2): categorical
# Both categorical so don't need to check for normality
# ==> Outcome:
# Chi-squared independence test
# H0: Gender and drinks now are independent
# Ha: Gender and drinks now are not independent
# p-value < 2.2e-16 < 0.05
# ==> Reject H0.
# There is a significant difference between genders in drinking habits
# Women have a higher percentage of drinkers
##############################################################################

# Contingency table
sexdr_cont_tab <- table(hse_data$Sex, hse_data$dnnow)
sexdr_cont_tab

# Contingency table with labels
sex_label <- factor(hse_data$Sex, levels = c(1,2), labels = c("Male", "Female"))
dnnow_label <- factor(hse_data$dnnow, levels = c(1,2), labels = c("Yes", "No"))
sexdr_labels_cont_tab <- table(sex_label, dnnow_label)
sexdr_labels_cont_tab
addmargins(sexdr_labels_cont_tab)

# Contingentcy table with labels and percentages
sexdr_pct_tab <- sexdr_labels_cont_tab %>%
  prop.table(margin = 2) * 100
sexdr_pct_tab
addmargins(sexdr_pct_tab) # Add row and column totals

# Chi-squared test of yes and no drink by gender
sexdr_labels_cont_tab %>%
  chisq.test() # Chi-squared test

# Bar chart of percentage men and women who do and do not drink
sexdr_pct_df <- as.data.frame.table(sexdr_pct_tab)  # Convert to data frame
colnames(sexdr_pct_df) <- c("Gender", "Drinks_Nowadays", "Percentage") # Rename
sexdr_pct_df %>%
  ggplot(aes(x = Gender, y = Percentage, fill = Drinks_Nowadays)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme_bw() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Percentage Men and Women who Drink Nowadays",
       x = "Gender", y = "Percentage", fill = "Drink Nowadays")

##############################################################################
# b.	Run a significance test to find out which region drinks the most alcohol.
# Variables
# dnnow: Whether drink nowadays: Yes (1), No (2): categorical
# gor1: Government Office Region: categorical
# Number empty gor1 rows (NA) = 0
# North East (1), North West (2), Yorkshire and the Humber (3),
# East Midlands (4), West Midlands (5), East of England (6),
# London (7), South East (8), South West (9)
# Wales (10), Scotland (11)
# Frequency table
#   1    2    3    4    5    6    7    8    9
# 880 1396 1082  966 1093 1169 1254 1733 1044
# Both categorical so don't need to check for normality
# ==> Outcome:
# Chi-squared independence test
# H0: Region and drinks now are independent
# Ha: Region and drinks now are not independent
# p-value < 2.2e-16 < 0.05
# ==> Reject H0.
# There is a significant difference between regions' drinking habits
# South East has the highest percentage of drinkers
##############################################################################

# Understand gor1
attr(hse_data$gor1, "labels") # 11 Labels
sum(is.na(hse_data$Age))      # Number empty rows (NA) = 0
table(hse_data$gor1)          # Frequency table (1st 9 regions used)
describe(hse_data$gor1)       # Descriptive statistics

# Contingency table (with 9 rows)
table(hse_data$gor1, hse_data$dnnow)

# Contingency table with labels
gor_label <- factor(hse_data$gor1, levels = 1:9,
                    labels = c("North East", "North West",
                               "Yorkshire and the Humber", "East Midlands",
                               "West Midlands", "East of England",
                               "London", "South East", "South West"))
dnnow_label <- factor(hse_data$dnnow, levels = c(1,2), labels = c("Yes", "No"))
gordr_labels_cont_tab <- table(gor_label, dnnow_label)
addmargins(gordr_labels_cont_tab)

# Contingency table with labels and percentages
gordr_pct_tab <- gordr_labels_cont_tab %>%
  prop.table(margin = 2) * 100
addmargins(gordr_pct_tab) # Add row and column totals

# Chi-squared test of yes and no drink by region
gordr_labels_cont_tab %>%
  chisq.test() # Chi-squared test

# Bar chart of regions percentages who do and do not drink
gordr_pct_df <- as.data.frame.table(gordr_pct_tab)  # Convert to data frame
colnames(gordr_pct_df) <- c("Region", "Drinks_Nowadays", "Percentage") # Rename
gordr_pct_df %>%
  ggplot(aes(x = Region, y = Percentage, fill = Drinks_Nowadays)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme_bw() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Percentage Regions who Drink Nowadays",
       x = "Region", y = "Percentage", fill = "Drink Nowadays")

##############################################################################
# c.	Investigate whether there is a statistical difference between
#     men and women on the following variables:
#     I.	Valid height.
#
# Variables
#
# Sex: Sex: Male (1), Female (2): categorical
# Sex descriptive:
#    vars     n mean  sd median trimmed mad min max range  skew kurtosis se
# X1    1 10617 1.54 0.5      2    1.54   0   1   2     1 -0.17    -1.97  0
# mode = 2
# mean < median = mode => Left skewed (negative)
#
# htval: Valid height (cm): continuous random variable
# Number empty htval rows (NA) = 1971
# htval summary:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   82.4   157.1   165.1   161.8   173.2   202.5    1971
# htval descriptive:
#     vars    n   mean    sd median trimmed   mad  min   max range  skew kurtosis  se
#  X1    1 8646 161.79 18.85  165.1  161.79 12.01 82.4 202.5 120.1 -1.75     3.58 0.2
# estimated mode = 166.618
# mean < median < mode => Left skewed (negative)
#
# Categorical and continuous so need to check for normality
# Normality tests for htval with NA removed:
# Shapiro-Wilk test for normality
# data:  heights_male$htval
# W = 0.73755, p-value < 2.2e-16
# Shapiro-Wilk normality test
# # data:  heights_female$htval
# W = 0.75777, p-value < 2.2e-16
# ==> Eveerything is non-normal
#
# ==> Mann-Whitney U test for non-normal data
# ==> Outcome:
# 	Wilcoxon rank sum test with continuity correction
# data:  heights_male$htval and heights_female$htval
# W = 14713021, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# ==> Reject H0
# There is a significant difference in height due to gender
##############################################################################

# Understand Sex
describe(hse_data$Sex)     # Descriptive statistics
mode(hse_data$Sex)         # Mode

# Understand htval
sum(is.na(hse_data$htval)) # Number empty rows (NA) = 1971
summary(hse_data$htval)    # Summary statistics
describe(hse_data$htval)   # Descriptive statistics ==> Left skewed (negative)
# Plot density
# Creates a single mode, left-skewed plot
hse_data %>%
  ggplot(aes(x = htval)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  theme_bw() +
  labs(title = "Height Density plot",
       x = "Height",
       y = "Density")
mode_crv(hse_data$htval)   # Estimated mode

# Plot htval box plot (check for outliers) => There are many outliers due to age range
hse_data %>%
  ggplot(aes(y = htval)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "htval Box Plot",
       y = "Height") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

# Normality tests
heights_male <- hse_data[hse_data$Sex == 1 & !is.na(hse_data$htval), ] # Remove NA
heights_female <- hse_data[hse_data$Sex == 2 & !is.na(hse_data$htval), ] # Remove NA  

# Male Q-Q plot =
heights_male %>%
  ggplot(aes(sample = htval)) +
  stat_qq() +
  theme_bw() +
  labs(title = "Height Q-Q for males")

# Female Q-Q plot
heights_female %>%
  ggplot(aes(sample = htval)) +
  stat_qq() +
  theme_bw() +
  labs(title = "Height Q-Q for females")

# Combined histogram - male and female
# Male and female data frame
heights_mf_df <- rbind(
  heights_male %>% mutate(Sex = "Male"),
  heights_female %>% mutate(Sex = "Female"))

# Combined histogram
heights_mf_df %>%
  ggplot(aes(x = htval, fill = factor(Sex))) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
#  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(title = "Height Histogram for males and females", 
       x = "Height (cm)", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("Male" = "#4292c6", "Female" = "#C51B7D")) +
  scale_x_continuous(n.breaks = 10)

# Shapiro-Wilk test for normality
shapiro.test(heights_male$htval)   # Shapiro-Wilk test
shapiro.test(heights_female$htval) # Shapiro-Wilk test

# Mann-Whitney U test for non-normal data
wilcox.test(heights_male$htval, heights_female$htval)

# Box plot Mann-Whitney U test
heights_mf_df %>%
  ggplot(aes(x = Sex, y = htval, fill = Sex)) +
  geom_boxplot(outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "Height Box Plot for Males and Females", 
        x = "Sex", 
        y = "Height") +
  scale_fill_manual(values = c("Male" = "#4292c6", "Female" = "#C51B7D"))

##############################################################################
# c.	Investigate whether there is a statistical difference between
#     men and women on the following variables:
#     II.	Valid weight.
#
# Variables
#
# Sex: Sex: Male (1), Female (2): categorical
# Sex descriptive:
#    vars     n mean  sd median trimmed mad min max range  skew kurtosis se
# X1    1 10617 1.54 0.5      2    1.54   0   1   2     1 -0.17    -1.97  0
# mode = 2
# mean < median = mode => Left skewed (negative)
#
# wtval: Valid weight (Kg) inc. estimated>130kg: continuous random variable
# Number empty wtval rows (NA) = 
##############################################################################

# Understand Sex
describe(hse_data$Sex)     # Descriptive statistics
mode(hse_data$Sex)         # Mode

# Understand htval
sum(is.na(hse_data$wtval)) # Number empty rows (NA) = 1971
summary(hse_data$wtval)    # Summary statistics
describe(hse_data$wtval)   # Descriptive statistics ==> Left skewed (negative)
# Plot density
# Creates a single mode, left-skewed plot
hse_data %>%
  ggplot(aes(x = wtval)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  theme_bw() +
  labs(title = "Weight Density plot",
       x = "Weight (kg)",
       y = "Density")
mode_crv(hse_data$wtval)   # Estimated mode

# Plot htval box plot (check for outliers) =>
hse_data %>%
  ggplot(aes(y = wtval)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "wtval Box Plot",
       y = "Weight (kg)") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

##############################################################################
# d.	What is the correlation between whether a person drinks nowadays, total
#     household income, age at last birthday and gender?
##############################################################################