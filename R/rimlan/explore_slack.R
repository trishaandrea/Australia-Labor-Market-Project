library(tidyverse)
library(lubridate)
library(scales)

labour_market_slack <- read_csv(
  "data/processed/labour_market_slack.csv"
)

labour_market_slack |> 
  summarise(
    mean_unemployment = 
      mean(unemployment_rate, na.rm = TRUE),

    mean_underemployment = 
      mean(underemployment_rate, na.rm = TRUE),

    mean_underutilisation = 
      mean(underutilisation_rate, na.rm = TRUE),

    mean_youth_unemployment =
      mean(youth_unemployment_rate, na.rm = TRUE),

    mean_youth_underemployment = 
      mean(youth_underemployment_rate, na.rm = TRUE),

    mean_youth_underutilisation =
      mean(youth_underutilisation_rate, na.rm = TRUE)
  )


slack_data <- labour_market_slack |> 
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
      unemployment_rate = "Unemployment",
      underemployment_rate = "Underemployment",
      underutilisation_rate = "Underutilisation"
    )
  )

ggplot(
  slack_data,
  aes(
    x = date,
    y = rate,
    colour = measure
  )
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Labour market slack in Australia",
    subtitle = "Unemployment, Underemployment and Underutilisation",
    x = NULL,
    y = "Rate(%)",
    colour = NULL,
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 12
  )