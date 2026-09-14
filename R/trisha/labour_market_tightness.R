# ============================================================
# Labour-Market Tightness
# ABS Job Vacancies / Unemployed Persons
# ============================================================

library(tidyverse)
library(readxl)
library(lubridate)

# ------------------------------------------------------------
# 1. Load ABS Job Vacancies
# ------------------------------------------------------------

job_vacancies <- read_csv(
  "data/processed/abs_job_vacancies_clean.csv",
  show_col_types = FALSE
) |>
  mutate(
    Quarter = as.Date(Quarter),
    year = year(Quarter),
    month = month(Quarter)
  ) |>
  arrange(Quarter)

# Check
glimpse(job_vacancies)
head(job_vacancies)
tail(job_vacancies)


# ------------------------------------------------------------
# 2. Load ABS unemployment data
#
# A84423046K =
# Unemployed persons, Australia,
# Seasonally Adjusted ('000)
# ------------------------------------------------------------

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
    date = as.Date(date),
    unemployed = as.numeric(unemployed),
    year = year(date),
    month = month(date)
  ) |>
  arrange(date)

# Check
glimpse(unemployment)
head(unemployment)
tail(unemployment)


# ------------------------------------------------------------
# 3. Match quarterly vacancies with unemployment
#
# ABS Job Vacancies reference months are:
# February, May, August, November
#
# Match by year and month instead of exact date
# because the day can differ across ABS datasets.
# ------------------------------------------------------------

tightness <- job_vacancies |>
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


# ------------------------------------------------------------
# 4. Calculate labour-market tightness
#
# Tightness = Job Vacancies / Unemployed Persons
#
# Both are measured in '000, so the units cancel.
# ------------------------------------------------------------

tightness <- tightness |>
  mutate(
    Tightness = Job_Vacancies / unemployed
  )


# ------------------------------------------------------------
# 5. Check the result
# ------------------------------------------------------------

glimpse(tightness)

head(tightness, 10)

tail(tightness, 10)

# Missing values
sum(is.na(tightness$Job_Vacancies))
sum(is.na(tightness$unemployed))
sum(is.na(tightness$Tightness))

# Duplicate quarters
sum(duplicated(tightness$Quarter))

# Date range
range(tightness$Quarter, na.rm = TRUE)

# Summary statistics
summary(tightness$Tightness)


# ------------------------------------------------------------
# 6. Plot labour-market tightness
# ------------------------------------------------------------

tightness_plot <- ggplot(
  tightness,
  aes(
    x = Quarter,
    y = Tightness
  )
) +
  geom_line(
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  labs(
    title = "Labour-Market Tightness in Australia",
    subtitle = "ABS Job Vacancies / Unemployed Persons",
    x = NULL,
    y = "Vacancies per unemployed person",
    caption = "Sources: ABS Job Vacancies and ABS Labour Force"
  ) +
  theme_minimal()

tightness_plot


# ------------------------------------------------------------
# 7. Highest tightness periods
# ------------------------------------------------------------

tightness |>
  filter(!is.na(Tightness)) |>
  arrange(desc(Tightness)) |>
  slice_head(n = 10)


# ------------------------------------------------------------
# 8. Lowest tightness periods
# ------------------------------------------------------------

tightness |>
  filter(!is.na(Tightness)) |>
  arrange(Tightness) |>
  slice_head(n = 10)


# ------------------------------------------------------------
# 9. Recent observations
# ------------------------------------------------------------

tightness |>
  filter(!is.na(Tightness)) |>
  slice_tail(n = 12)


# ------------------------------------------------------------
# 10. Save processed tightness dataset
# ------------------------------------------------------------

write_csv(
  tightness,
  "data/processed/labour_market_tightness.csv"
)

summary(tightness$Tightness)
tail(tightness, 10)
tightness_plot