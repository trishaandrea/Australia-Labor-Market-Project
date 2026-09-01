library(tidyverse)
library(readxl)
library(lubridate)

# --------------------------------------------------
# 1. Import Unemployed Persons
# --------------------------------------------------

unemployment <- read_excel(
  "data/raw/ABS_Table_001_labour_force.xlsx",
  sheet = "Data1",
  skip = 9
) |>
  select(
    date = `Series ID`,
    unemployed = A84423046K
  ) |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


# --------------------------------------------------
# 2. Import cleaned ABS Job Vacancies
# --------------------------------------------------

abs_job_vacancies <- read_csv(
  "data/processed/abs_job_vacancies_clean.csv",
  show_col_types = FALSE
) |>
  mutate(
    Quarter = as.Date(Quarter)
  )


# --------------------------------------------------
# 3. Create year and month
# --------------------------------------------------

unemployment <- unemployment |>
  mutate(
    year = year(date),
    month = month(date)
  )

abs_job_vacancies <- abs_job_vacancies |>
  mutate(
    year = year(Quarter),
    month = month(Quarter)
  )


# --------------------------------------------------
# 4. Combine datasets
# --------------------------------------------------

tightness <- abs_job_vacancies |>
  left_join(
    unemployment,
    by = c("year", "month")
  ) |>
  select(
    Quarter,
    Job_Vacancies,
    unemployed
  ) |>
  arrange(Quarter)


# --------------------------------------------------
# 5. Calculate tightness
# --------------------------------------------------

tightness <- tightness |>
  mutate(
    Tightness = Job_Vacancies / unemployed
  )


# --------------------------------------------------
# 6. Check result
# --------------------------------------------------

glimpse(tightness)

head(tightness)

tail(tightness)

sum(is.na(tightness$unemployed))

sum(is.na(tightness$Tightness))

sum(duplicated(abs_job_vacancies$Quarter))