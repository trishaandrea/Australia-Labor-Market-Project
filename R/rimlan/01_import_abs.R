
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

data_dir <- "data"


# ------------------------------------------------------------
# 3. Import ABS Labour Force data
# ------------------------------------------------------------

labour_force <- read_excel(
  file.path(
    data_dir,
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
  )


# ------------------------------------------------------------
# 4. Import ABS Youth Labour Force data
# ------------------------------------------------------------

youth <- read_excel(
  file.path(
    data_dir,
    "ABS_Table_013_youth_labour_force.xlsx"
  ),
  sheet = "Data1",
  skip = 9
) |>
  select(
    date = `Series ID`,
    youth_unemployment_rate = A84424185C
  )


# ------------------------------------------------------------
# 5. Import ABS Underemployment data
# ------------------------------------------------------------

underemployment <- read_excel(
  file.path(
    data_dir,
    "ABS_X29_underutilisation.xlsx"
  ),
  sheet = "Data2",
  skip = 9
) |>
  select(
    date = `Series ID`,
    underemployment_rate = A85255725J,
    youth_underemployment_rate = A85255677A
  )


# ------------------------------------------------------------
# 6. Import ABS Underutilisation data
# ------------------------------------------------------------

underutilisation <- read_excel(
  file.path(
    data_dir,
    "ABS_X29_underutilisation.xlsx"
  ),
  sheet = "Data4",
  skip = 9
) |>
  select(
    date = `Series ID`,
    underutilisation_rate = A85255726K,
    youth_underutilisation_rate = A85255678C
  )


# ------------------------------------------------------------
# 7. Convert dates
# ------------------------------------------------------------

labour_force <- labour_force |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


youth <- youth |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


underemployment <- underemployment |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


underutilisation <- underutilisation |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(date)


# ------------------------------------------------------------
# 8. Combine all datasets
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
# 9. Create youth unemployment gap
# ------------------------------------------------------------

labour_market_slack <- labour_market_slack |>
  mutate(
    youth_unemployment_gap =
      youth_unemployment_rate -
      unemployment_rate
  )


# ------------------------------------------------------------
# 10. Check the dataset
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
# 11. Basic summary
# ------------------------------------------------------------

summary(labour_market_slack)


# ------------------------------------------------------------
# 12. Check labour-market slack relationship
# ------------------------------------------------------------

labour_market_slack |>
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


# ------------------------------------------------------------
# 13. Prepare data for the main graph
# ------------------------------------------------------------

plot_data <- labour_market_slack |>
  select(
    date,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  ) |>
  pivot_longer(
    cols = -date,
    names_to = "measure",
    values_to = "rate"
  ) |>
  mutate(
    measure = recode(
      measure,
      unemployment_rate =
        "Unemployment",

      underemployment_rate =
        "Underemployment",

      underutilisation_rate =
        "Underutilisation"
    )
  )


# ------------------------------------------------------------
# 14. Main graph - Labour-market slack
# ------------------------------------------------------------

labour_market_slack_pt <- ggplot(
  plot_data,
  aes(
    x = date,
    y = rate,
    colour = measure
  )) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title = "Labour-market slack in Australia",
    subtitle =
      "Unemployment, underemployment and underutilisation rates",
    x = NULL,
    y = "Rate (%)",
    colour = NULL,
    caption =
      "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 12
  )

labour_market_slack_pt


# ------------------------------------------------------------
# 15. Youth vs overall unemployment
# ------------------------------------------------------------

youth_plot_data <- labour_market_slack |>
  select(
    date,
    unemployment_rate,
    youth_unemployment_rate
  ) |>
  pivot_longer(
    cols = -date,
    names_to = "measure",
    values_to = "rate"
  ) |>
  mutate(
    measure = recode(
      measure,

      unemployment_rate =
        "Overall unemployment",

      youth_unemployment_rate =
        "Youth unemployment (15–24)"
    )
  )


youth_unemployment_pt <- ggplot(
  youth_plot_data,
  aes(
    x = date,
    y = rate,
    colour = measure
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title =
      "Youth and overall unemployment in Australia",

    subtitle =
      "Youth unemployment compared with the overall unemployment rate",

    x = NULL,

    y = "Rate (%)",

    colour = NULL,

    caption =
      "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 12
  )

youth_unemployment_pt

ggplot(
  labour_market_slack,
  aes(
    x = date,
    y = youth_unemployment_gap
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title = "Youth unemployment gap in Australia",
    subtitle =
      "Youth unemployment rate minus the overall unemployment rate",
    x = NULL,
    y = "Percentage points",
    caption =
      "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 12
  )

labour_market_slack |>
  slice_max(
    underutilisation_rate,
    n = 10,
    with_ties = FALSE
  ) |>
  select(
    date,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  )

labour_market_slack |>
  slice_max(
    unemployment_rate,
    n = 10,
    with_ties = FALSE
  ) |>
  select(
    date,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  )

labour_market_slack |>
  slice_max(
    youth_unemployment_rate,
    n = 10,
    with_ties = FALSE
  ) |>
  select(
    date,
    unemployment_rate,
    youth_unemployment_rate,
    youth_unemployment_gap
  )

labour_market_slack |>
  mutate(
    period = case_when(
      date < as.Date("1990-01-01") ~ "Pre-1990",
      date >= as.Date("1990-01-01") &
        date <= as.Date("1993-12-01") ~ "Early 1990s recession",
      date >= as.Date("1994-01-01") &
        date <= as.Date("2019-12-01") ~ "1994–2019",
      date >= as.Date("2020-01-01") &
        date <= as.Date("2021-12-01") ~ "COVID period",
      date >= as.Date("2022-01-01") ~ "2022 onwards"
    )
  ) |>
  group_by(period) |>
  summarise(
    mean_unemployment =
      mean(unemployment_rate, na.rm = TRUE),

    mean_underemployment =
      mean(underemployment_rate, na.rm = TRUE),

    mean_underutilisation =
      mean(underutilisation_rate, na.rm = TRUE),

    mean_youth_unemployment =
      mean(youth_unemployment_rate, na.rm = TRUE)
  )