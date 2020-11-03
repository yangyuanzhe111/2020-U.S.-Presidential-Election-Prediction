#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 
# https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Siyi Ma, Heye Liu, Yan Wang, Yuanzhe Yang
# Data: 30 October 2020
# Contact: yangyuanzhe111@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from 
# https://www.voterstudygroup.org/publication/nationscape-data-set 
# and save the folder that you're interested in to inputs/data 



#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("~/Desktop/Data")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

# filter dataset by registered persons
reduced_data <- 
  reduced_data %>% 
  filter(registration == "Registered")
# create a new binary variable of trump 
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020 =="Donald Trump", 1, 0))

reduced_data<-
  reduced_data %>%
  mutate(voteintention = 
           ifelse(vote_2020 =="Donald Trump", "Trump", "Biden"))
# create a age group 
reduced_data<-
  reduced_data %>%
  mutate(agegroup = ifelse(age >= 18 & age <= 29, "18-29",
                           ifelse(age >= 30 & age <= 44, "30-44",
                                  ifelse(age >= 45 & age <= 54, "45-54",
                                         ifelse(age >= 55 & age <= 64, "55-64",
                                                ifelse(age >= 65, "65+", NA))))))
reduced_data<-
  reduced_data %>%
  mutate(race = ifelse(race_ethnicity == "White", "White",
                       ifelse(race_ethnicity == "Black, or African American", "Black", "Others")))
                              

# create income categories 
reduced_data<-
  reduced_data %>%
  mutate(incomelevel = ifelse(household_income == "Less than $14,999" |
                                household_income == "$15,000 to $19,999"|
                                household_income == "$20,000 to $24,999"|
                                household_income == "$25,000 to $29,999 "|
                                household_income == "$30,000 to $34,999", "Lower income", 
                              ifelse(household_income == "$35,000 to $39,999 "|
                                       household_income =="$40,000 to $44,999"|
                                       household_income == "$45,000 to $49,999"|
                                       household_income == "$50,000 to $54,999"|
                                       household_income == "$55,000 to $59,999" |
                                       household_income == "$60,000 to $64,999"|
                                       household_income =="$65,000 to $69,999 "|
                                       household_income == "$70,000 to $74,999"|
                                       household_income == "$75,000 to $79,999"|
                                       household_income == "$80,000 to $84,999"|
                                       household_income == "$85,000 to $89,999"|
                                       household_income == "$90,000 to $94,999"|
                                       household_income == "$95,000 to $99,999"|
                                       household_income == "$100,000 to $124,999", "Middle income",
                                     ifelse(household_income == "$125,000 to $149,999"|
                                              household_income == "$150,000 to $174,999"|
                                              household_income == "$175,000 to $199,999"|
                                              household_income == "$200,000 to $249,999 "|
                                              household_income =="$250,000 and above", "Upper income", NA))))


reduced_data <- reduced_data %>% 
  mutate(educationl = ifelse(education == "No HS", "Non-High School Graduate",
                        ifelse(education == "High school graduate" |education == "Completed some high school", "High School Graduate",
                               ifelse(education == "Some college"|education == "Completed some college, but no degree"|education=="Associate Degree", "Some College",
                                      ifelse(education == "College Degree (such as B.A., B.S.)", "College Degree",
                                             ifelse(education == "Masters degree", "Masters degree",
                                                    ifelse(education == "Doctorate degree", "Doctorate degree", NA)))))))
reduced_data <- reduced_data %>% 
  mutate(workingstatus = ifelse(employment == "Full-time employed"|
                                  employment == "Self-employed"|
                                  employment =="Part-time employed","Working",
                                "Not working"))

# removing NA values
survey_data <- na.omit(reduced_data)
survey_data <- survey_data %>% 
  select(gender, 
         agegroup, 
         race, 
         incomelevel, 
         educationl, 
         workingstatus, 
         state, 
         vote_trump,
         voteintention)
dim(survey_data)
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(survey_data, "survey_data.csv")

survey_data %>% 
  ggplot(mapping = aes(x = state, fill = voteintention)) +
  geom_bar() + 
  labs(x = "state",
       y = "Count")
