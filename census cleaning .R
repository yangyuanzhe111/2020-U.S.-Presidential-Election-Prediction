#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 
# https://usa.ipums.org/usa/index.shtml
# Author: Siyi Ma, Heye Liu, Yan Wang, Yuanzhe Yang
# Data: 30 October 2020
# Contact: yangyuanzhe111@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from 
# https://usa.ipums.org/usa/index.shtml
# and save the folder that you're interested in to inputs/data 

library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("usa_00001.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)

names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(
    sex, 
    age, 
    race, 
    educd,
    region,
    labforce)

reduced_data[which(as.integer(reduced_data$age) < 18), ] <- NA 
  

reduced_data <- reduced_data %>% 
  mutate(gender = ifelse(sex=="male", "Male", 
                         ifelse(sex == "female", "Female", NA)),
         agegroup = ifelse(as.integer(age) >= 18 & as.integer(age) <= 29, "18-29",
                           ifelse(as.integer(age) >= 30 & as.integer(age) <= 44, "30-44",
                                  ifelse(as.integer(age) >= 45 & as.integer(age) <= 54, "45-54",
                                         ifelse(as.integer(age) >= 55 & as.integer(age) <= 64, "55-64",
                                                ifelse(as.integer(age) >= 65, "65+", NA))))),
         educationl = ifelse(educd == "some college, but less than 1 year"|educd == "associate's degree, type not specified", "Some College",
                                 ifelse(educd == "regular high school diploma", "High School Graduate",
                                        ifelse(educd == "professional degree beyond a bachelor's degree", "College Degree",
                                               ifelse(educd ==  "master's degree", "Masters degree",
                                                      ifelse(educd == "doctoral degree", "Doctorate degree", "Non-High School Graduate"))))),
         race = ifelse(as.integer(race) == 1, "White",
                       ifelse(as.integer(race) == 2, "Black", "Others")),
         workingstatus = ifelse(labforce == "yes, in the labor force", "Working",
                              ifelse(labforce == "no, not in the labor force", "Not working", NA)),)

census_data <- reduced_data %>% na.omit()

dim(census_data)
# Saving the sample data as a csv file in my
# working directory
write_csv(census_data, "census_data.csv")
                                             
         