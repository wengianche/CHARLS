#Project - MULTIMORBIDITY
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250822
#UPDATED: 20250827
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE CHARLS DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#CHARLS waves 1 to 5, plus life history survey in 2014 and harmonized data of waves 1 to 4
  
#Logbook
######################################################  
######################################################

#Things to pay attention
###################################################### 
#250825 According to harmonized data (waves 1 to 4) documentation (mBfW3A-Harmonized_CHARLS_D), there are five individuals with contradictory information on interview status (RwIWSTAT varaibles)
#250825 For missing values, check if missing coding is used or just plain missing (NA)
#250826 QC: birth year ranging from 1900 and 2000 which is not reasonable and at least those born after 1973 should be excluded
#250826 Variable: age at interview. At Waves 1 and 2, all respondents are asked to report their current age. Starting at Wave 3, respondents are not asked to report their current age. Then, create a new age at interview variable by (interview year-birth year)
#250826 Variable: Hukou (R3HUKOU). There are around 1,000 re-interview respondents in wave 3 with missing values for RwHUKOU who disputed the previous wave hukou type but do not have a corrected type available.
#250826 Variable: self-reported health (RwSHLT and RwSHLTA (preferable given it is available across the waves), with many missing values). CHARLS has two scales for respondents to self-report their current health condition. Starting at Wave 4, respondents were only asked to self-report their current health condition in the beginning of the health module. Respondents are asked the second scale.
#250827, Variable: ADL assessment only among to those >=50 years and reported good health. However, some individuals younger than 50y also had values for these variables
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Create a variable list 
#2. Participation of individuals across waves (using harmonized data)
#3. Drop out and death statuses across waves (using harmonized data)
#2. Missing values

#DATA PREPARATION
#1. Adding variables from wave 5
######################################################  
###################################################### 
#INSTALL AND LOAD LIBRARY
###################################################### 
library(pacman)
pacman::p_load(Hmisc, #attach labels to variables
               haven,
               tidyverse,
               ggalluvial,
               ggplot2,
               plotly,
               labelled,
               openxlsx
) 
###################################################### 

######################################################
#DATA EXPLORATION
#1. Create a variable list 
#2. Participation of individuals across waves (using harmonized data)
#3. Drop out and death statuses across waves (using harmonized data)
#2. Missing values
######################################################
#1. Create a variable list 
######################################################
#Load data
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hw14_full <- read_dta("H_CHARLS_D_Data.dta") #missing codes in the original dta file are all collapsed to "NA"

#Create a detailed table
hw14_variable_table <- data.frame(
  Variable = names(hw14_full),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(hw14_full, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(hw14_full)),
  Unique_Values = sapply(hw14_full, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
hw14_variable_table$Value_Labels <- sapply(names(hw14_full), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Basic information')
write.xlsx(hw14_variable_table, file = "hw14_variable_table_250825.xlsx", colNames=T, format_headers=T)
######################################################
#2. Participation of individuals across waves (using harmonized data)
######################################################
#Load data
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hw14 <- read_dta("H_CHARLS_D_Data.dta") %>% select(1,24:27)

#Wave 5_2020
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
w5 <- read_dta("Sample_Infor.dta") %>% select(1)

#Add participation indicator for wave 5_2020
hw15 <- hw14 %>% mutate(inw5=if_else(ID %in% w5$ID,1,0)) 

#Tabulate variable inw1 to inw4 to get distribution of participation across waves 1 to 4
hw15_inw_freq <- hw15 %>%   group_by(inw1, inw2, inw3, inw4, inw5) %>%
                            summarise(Frequency = n(), .groups = 'drop') %>%
                            arrange(desc(Frequency))

##Alluvial plot 
#Create a pattern identifier
hw15_inw_freq <- hw15_inw_freq %>%
  mutate(pattern_id = 1:n()) %>% relocate(pattern_id, Frequency)

#Remove labels for inw1 to inw4
hw15_inw_freq <- hw15_inw_freq  %>%
mutate(across(everything(), ~ {
  attr(., "labels") <- NULL
  attr(., "label") <- NULL
  class(.) <- setdiff(class(.), "labelled")
  return(.)
}))

#Transform data to long format for plotting
hw15_inw_freq_long <- hw15_inw_freq %>%
  pivot_longer(
    cols = starts_with("inw"),
    names_to = "Wave",
    values_to = "Participated"
  ) %>%
  mutate(Participated = factor(Participated, levels = c(1, 0), labels = c("Yes", "No")))

#Create the alluvial plot 
#Based on the data structure without information on drop out and death, the alluvial plot cannot clearly show the number of individuals included in each participation pattern across waves
#Graph to be updated after adding information on drop out and death 250825
ggplot(hw15_inw_freq_long,
       aes(x = Wave, 
           stratum = Participated, 
           alluvium = pattern_id,
           y = Frequency,
           fill = Participated,
           label = Participated)) +
  geom_flow(alpha = 0.7) +
  geom_stratum(alpha = 0.8) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(count)),
            size = 3, 
            color = "black",
            check_overlap = TRUE) +
  scale_fill_manual(values = c("Yes" = "#1f78b4", "No" = "#e31a1c")) +
  labs(title = "Participant Retention Patterns Across CHARLS Survey Waves",
       y = "Number of Participants",
       fill = "Participated") +
  theme_minimal() +
  theme(legend.position = "bottom")

##Cleveland plot
#Add pattern label
hw15_inw_freq <- hw15_inw_freq %>%
  unite("pattern_label", inw1:inw5, sep = "-", remove = FALSE)

#Create the cleveland plot
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Multimorbidity/Output/Graph')
tiff("hw15_inw_freq.tiff", width = 18, height = 12, units = "in", res = 300)
ggplot(hw15_inw_freq, aes(x = Frequency, y = reorder(pattern_label, pattern_label))) +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(xend = 0, yend = reorder(pattern_label, pattern_label)), color = "grey50") +
  geom_text(aes(label = paste0("n = ", Frequency)), 
            hjust = -0.2,
            size = 3.5,
            color = "black") +
  labs(title = "Frequency of Participation Patterns",
       x = "Number of Participants",
       y = "Pattern (Wave1-Wave2-Wave3-Wave4-Wave5)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())
dev.off()

##Participation pattern that should be dropped
#Participation in one wave only: 1106 (wave1) + 373 (wave2) + 430 (wave3) + 158 (wave4) 

##The number of individuals added if including individuals with at least two participations
#Wave1: 16602
#Wave2: 3053
#Wave3: 3394
#Wave4: 470
######################################################
#3. Drop out and death statuses across waves (using harmonized data)
######################################################
#Load data
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hw14_full <- read_dta("H_CHARLS_D_Data.dta") 

#Wave 5_2020
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
w5 <- read_dta("Sample_Infor.dta") %>% select(1)

#Add participation indicator for wave 5_2020
hw15 <- hw14 %>% mutate(inw5=if_else(ID %in% w5$ID,1,0)) 
######################################################



#Wave 1_2011
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2011r')

#Wave 5_2020
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
w5 <- read_dta("Sample_Infor.dta") %>% select(1,)
######################################################
#DATA PREPARATION
#1. Adding variables from wave 5
######################################################  
#1. Adding variables from wave 5
###################################################### 
#Variables to be added
#Weight (housefold and individual levels)
#Interview Dates
#Birth date: Month, and Year (compare to wave 5 variables and update, focus on RAIDBYEAR, RAIDBMONTH and RAIDBDAY)
#Death: Month, and Year, The Wave 3 and 4 exit interview files have not been released. Extract from wave 3 to 5?
#Age at interview (in months and years)
#Education (update needed?)
#Current Marital Status and Number of Marriage
#HuKou, Live in Urban or Rural, Lives in Institution at Interview
#Self-report of Health
#Activities of daily living (ADLs): Some difficulty, Instrumental activities of daily living (IADLs): Some difficulty, Other Functional Limitations: Some difficulty, ADL summary variable (use this only?)
#Doctor diagnosed health problems: Ever Had Condition, Doctor diagnosed health problems: Memory-related disease, Whether Receives Treatment or Medication for Disease (update as well after dispute disease diagnosis? should be as variables regarding disease record verification were included to build these variables)
#Health behaviors: Physical Activity or Exercise (with many missing as only half of sample were randomly selected to answer these questions in the first three waves, only included for wave 4?)
#Health behaviors: Drinking
#Health behaviors: Smoking (cigarettes)
#Medical Care Utilization: Inpatient/Hospital Stay, Medical Care Utilization: Outpatient/ doctor visit, Medical Care Utilization: Dental visit (exclude as data only available at waves 2 and 3?), Traditional medicine facility visit
#Medical Expenditure: hospitalization in the past year,  dental visit in the last year (also only for waves 2 and 3)
#Covered by Public Health Insurance Program (as covariates), Private Health Insurance Program, Other Health Insurance Program
#Self-reported Memory, Immediate Word Recall, Delayed Word Recall, Date Naming (with many missing values, esp for wave 2), Drawing Picture, Total Recall Summary Score
#Inflation Multiplier
#Individual value: cash and savings accounts, stocks and mutual funds (with many missing values at waves 2 to 4), government bonds, all other savings (with many missing at waves 2 to 4), other debt (with many missing at waves 2 to 4), Total non-housing financial wealth
#Household value: other house (not primary residence) (with many missing at waves 2 to 4), primary residence, total wealth (household level available for waves 1 and 2 only, as covariate/converted to couple level?)
#Earned Income (with many missing at waves 2 to 4), Total Capital Income (with many missing at waves 2 to 4), Pension Income
#Government/Public Transfer Income (with many missing at waves 2 to 4), Other Income (with many missing at waves 2 to 4), Income from Other Household Members, 
#Household food consumption (with many missing at waves 2 to 4), Household non-food consumption in last month (with many missing at waves 2 to 4), Household other non-food consumption in last year (with many missing at waves 2 to 4), Total household consumption
#Number of People Living in Household, Living Children, Deceased Children (as adverse psychological event?), Living Sibling, Deceased Sibling, Parental Mortality (dispute may happen after wave 2 but correction was not done, with many missing at waves 2 to 4), Parental SES: Education and Harmonized Education 
###################################################### 
###################################################### 


# Read the Stata file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
COVID_Module <- read_dta("COVID_Module.dta")
demo <- read_dta("Demographic_Background.dta")
exit <- read_dta("Exit_Module.dta")
family <- read_dta("Family_Information.dta")
health_func <- read_dta("Health_Status_and_Functioning.dta")
house_income <- read_dta("Household_Income.dta")
indi_income <- read_dta("Individual_Income.dta")
sample_info <- read_dta("Sample_Infor.dta")
weight <- read_dta("Weights.dta")
work_retire<- read_dta("Work_Retirement.dta")

# Convert to data.frame if needed (read_dta returns a tibble)
data <- as.data.frame(data)