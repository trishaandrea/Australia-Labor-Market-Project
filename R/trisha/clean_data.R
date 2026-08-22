# Labour Demand Project - Data Cleaning
library(tidyverse)
library(readxl)

# Import data
ivi_anzsco2 <- read_excel("data/raw/internet_vacancies_anzsco2_occupations_states_and_territories_-_july_2026.xlsx")
ivi_skill <- read_excel("data/raw/internet_vacancies_anzsco_skill_level_states_and_territories_-_july_2026.xlsx")
ivi_anzsco4 <- read_excel("data/raw/internet_vacancies_anzsco4_occupations_states_and_territories_-_july_2026.xlsx")

# Clean ANZSCO 2-Digit Data
ivi_anzsco2 <- read_excel(
  "data/raw/internet_vacancies_anzsco2_occupations_states_and_territories_-_july_2026.xlsx",
  sheet = "Seasonally Adjusted"
)

class(ivi_anzsco2)
unique(ivi_anzsco2$Level)
unique(ivi_anzsco2$State)
ivi_anzsco2 |>
  count(State)
ivi_anzsco2 |>
  select(Level, ANZSCO_CODE, Title) |>
  distinct() |>
  print(n = 50)

# Convert monthly columns from wide to long format
# Convert the dataset to tidy format
ivi_anzsco2_clean <- ivi_anzsco2 |>
  pivot_longer(
    cols = -c(Level, ANZSCO_CODE, Title, State),
    names_to = "Month",
    values_to = "Vacancies"
  )

head(ivi_anzsco2_clean)
glimpse(ivi_anzsco2_clean)

# Fix the Month column
ivi_anzsco2_clean <- ivi_anzsco2_clean |>
  mutate(
    Month = as.Date(
      as.numeric(Month),
      origin = "1899-12-30"
    )
  )
head(ivi_anzsco2_clean)
min(ivi_anzsco2_clean$Month)
max(ivi_anzsco2_clean$Month)
unique(format(ivi_anzsco2_clean$Month, "%d"))

# Check missing value
colSums(is.na(ivi_anzsco2_clean))

# Check duplicates
ivi_anzsco2_clean |>
  count(Level, ANZSCO_CODE, Title, State, Month) |>
  filter(n > 1)

# Final dimensions
dim(ivi_anzsco2_clean)

# Save cleaned ANZSCO 2-digit dataset
write_csv(
  ivi_anzsco2_clean,
  "data/processed/ivi_anzsco2_clean.csv"
)

# Clean ANZSCO skill level
excel_sheets("data/raw/internet_vacancies_anzsco_skill_level_states_and_territories_-_july_2026.xlsx")
ivi_skill <- read_excel(
  "data/raw/internet_vacancies_anzsco_skill_level_states_and_territories_-_july_2026.xlsx",
  sheet = "Seasonally Adjusted"
)

dim(ivi_skill)
names(ivi_skill)
head(ivi_skill)
glimpse(ivi_skill)
unique(ivi_skill$State)

ivi_skill_clean <- ivi_skill |>
  pivot_longer(
    cols = -c(Level, Title, State, Skill_level),
    names_to = "Month",
    values_to = "Vacancies"
  ) |>
  mutate(
    Month = as.Date(
      as.numeric(Month),
      origin = "1899-12-30"
    )
  )
head(ivi_skill_clean)

min(ivi_skill_clean$Month)
max(ivi_skill_clean$Month)

colSums(is.na(ivi_skill_clean))

ivi_skill_clean |>
  count(Level, Title, State, Skill_level, Month) |>
  filter(n > 1)

dim(ivi_skill_clean)

ivi_skill_clean |>
  distinct(Level, Skill_level, Title) |>
  arrange(Level, Skill_level)

write_csv(
  ivi_skill_clean,
  "data/processed/ivi_skill_clean.csv"
)

# Clean ANZSCO 4
excel_sheets("data/raw/internet_vacancies_anzsco4_occupations_states_and_territories_-_july_2026.xlsx")

ivi_anzsco4 <- read_excel(
  "data/raw/internet_vacancies_anzsco4_occupations_states_and_territories_-_july_2026.xlsx",
  sheet = "4 digit 3 month average"
)

dim(ivi_anzsco4)
names(ivi_anzsco4)
head(ivi_anzsco4)
glimpse(ivi_anzsco4)

ivi_anzsco4_clean <- ivi_anzsco4 |>
  pivot_longer(
    cols = -c(ANZSCO_CODE, ANZSCO_TITLE, state),
    names_to = "Month",
    values_to = "Vacancies"
  )

head(ivi_anzsco4_clean)
glimpse(ivi_anzsco4_clean)

ivi_anzsco4_clean <- ivi_anzsco4_clean |>
  mutate(
    Vacancies = na_if(Vacancies, "."),
    Vacancies = as.numeric(Vacancies),
    Month = as.Date(
      as.numeric(Month),
      origin = "1899-12-30"
    )
  ) |>
  rename(State = state)

head(ivi_anzsco4_clean)

glimpse(ivi_anzsco4_clean)

min(ivi_anzsco4_clean$Month)
max(ivi_anzsco4_clean$Month)

colSums(is.na(ivi_anzsco4_clean))

ivi_anzsco4_clean |>
  count(ANZSCO_CODE, ANZSCO_TITLE, State, Month) |>
  filter(n > 1)

dim(ivi_anzsco4_clean)

ivi_anzsco4_clean |>
  group_by(State) |>
  summarise(
    Missing = sum(is.na(Vacancies)),
    Total = n(),
    Missing_pct = Missing / Total * 100
  )

ivi_anzsco4_clean |>
  filter(is.na(Vacancies)) |>
  count(ANZSCO_CODE, ANZSCO_TITLE, sort = TRUE)

ivi_anzsco4_clean |>
  group_by(Month) |>
  summarise(
    Missing = sum(is.na(Vacancies))
  ) |>
  filter(Missing > 0)

# Check missing values by occupation
ivi_anzsco4_clean |>
  filter(is.na(Vacancies)) |>
  count(ANZSCO_CODE, ANZSCO_TITLE, sort = TRUE)

# Check missing values over time
ivi_anzsco4_clean |>
  group_by(Month) |>
  summarise(
    Missing = sum(is.na(Vacancies))
  ) |>
  filter(Missing > 0)

# Missing values are structural and are retained as NA.
# No imputation is performed.

# Check duplicates
ivi_anzsco4_clean |>
  count(ANZSCO_CODE, ANZSCO_TITLE, State, Month) |>
  filter(n > 1)

# Check final structure
glimpse(ivi_anzsco4_clean)

# Check dimensions
dim(ivi_anzsco4_clean)

write_csv(
  ivi_anzsco4_clean,
  "data/processed/ivi_anzsco4_clean.csv"
)