
# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)


# ------------------------------------------------------------
# 2. Set data folder
# ------------------------------------------------------------

raw_dir <- "data/raw"
processed_dir <- "data/processed"

dir.create(
  processed_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# ------------------------------------------------------------
# 3. Import ABS Labour Force data
# ------------------------------------------------------------

labour_force <- read_excel(
  file.path(
    raw_dir,
    "ABS_Table_001_labour_force.xlsx"
  ),
  sheet = "Data1",
  skip = 9
) |>
  select(
    date = `Series ID`,
    employed = A84423043C,
    unemployment_rate = A84423050A,
    participation_rate = A84423051C
  ) |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


# ------------------------------------------------------------
# 4. Import ABS Youth Labour Force data
# ------------------------------------------------------------

youth <- read_excel(
  file.path(
    raw_dir,
    "ABS_Table_013_youth_labour_force.xlsx"
  ),
  sheet = "Data1",
  skip = 9
) |>
  select(
    date = `Series ID`,
    youth_unemployment_rate = A84424185C
  ) |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


# ------------------------------------------------------------
# 5. Import ABS Underemployment data
# ------------------------------------------------------------

underemployment <- read_excel(
  file.path(
    raw_dir,
    "ABS_X29_underutilisation.xlsx"
  ),
  sheet = "Data2",
  skip = 9
) |>
  select(
    date = `Series ID`,
    underemployment_rate = A85255725J,
    youth_underemployment_rate = A85255677A
  ) |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)

# ------------------------------------------------------------
# 6. Import ABS Underutilisation data
# ------------------------------------------------------------

underutilisation <- read_excel(
  file.path(
    raw_dir,
    "ABS_X29_underutilisation.xlsx"
  ),
  sheet = "Data4",
  skip = 9
) |>
  select(
    date = `Series ID`,
    underutilisation_rate = A85255726K,
    youth_underutilisation_rate = A85255678C
  ) |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)



# ------------------------------------------------------------
# 7. Combine the ABS datasets
# ------------------------------------------------------------

labour_market_slack <- labour_force |>
  left_join(
    youth,
    by = "date"
  ) |>
  left_join(
    underemployment,
    by = "date"
  ) |>
  left_join(
    underutilisation,
    by = "date"
  ) |>
  arrange(date)


# ------------------------------------------------------------
# 8. Create youth unemployment gap
# ------------------------------------------------------------

labour_market_slack <- labour_market_slack |>
  mutate(
    youth_unemployment_gap =
      youth_unemployment_rate -
      unemployment_rate
  )

# ------------------------------------------------------------
# 9. Check the dataset
# ------------------------------------------------------------

# Number of observations
nrow(labour_market_slack)

# Number of unique months
n_distinct(labour_market_slack$date)

# Date range
range(
  labour_market_slack$date,
  na.rm = TRUE
)

# Missing values
colSums(
  is.na(labour_market_slack)
)

# Duplicate dates
sum(
  duplicated(labour_market_slack$date)
)


# ------------------------------------------------------------
# 10. Check missing values
# ------------------------------------------------------------

colSums(
  is.na(labour_market_slack)
)

# ------------------------------------------------------------
# 11. Check labour-market slack relationship
# ------------------------------------------------------------

underutilisation_check <- labour_market_slack |>
  summarise(
    largest_difference =
      max(
        abs(
          underutilisation_rate -
            (
              unemployment_rate +
                underemployment_rate
            )
        ),
        na.rm = TRUE
      )
  )

underutilisation_check

# ------------------------------------------------------------
# 12. Check final dataset
# ------------------------------------------------------------

glimpse(labour_market_slack)

# ------------------------------------------------------------
# 13. Save processed dataset
# ------------------------------------------------------------

write.csv(
  labour_market_slack,
  "data/processed/labour_market_slack.csv",
  row.names = FALSE
)