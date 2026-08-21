# Labour Demand Project - Data Cleaning
library(tidyverse)
library(readxl)

# Import data
ivi_anzsco2 <- read_excel("data/raw/internet_vacancies_anzsco2_occupations_states_and_territories_-_july_2026.xlsx")
ivi_skill <- read_excel("data/raw/internet_vacancies_anzsco_skill_level_states_and_territories_-_july_2026.xlsx")
ivi_anzsco4 <- read_excel("data/raw/internet_vacancies_anzsco4_occupations_states_and_territories_-_july_2026.xlsx")