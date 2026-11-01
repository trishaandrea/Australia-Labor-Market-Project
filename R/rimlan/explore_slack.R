
# ============================================================
# Explore Labour-Market Slack
# ETC-5543 Australian Labour Market Project
#
# Author: Rimlan
#
# Purpose:
# Explore historical unemployment, underemployment and
# underutilisation rates in Australia.
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)


# ============================================================
# 2. Load data
# ============================================================

labour_market_slack <- read_csv(
  "data/processed/labour_market_slack.csv",
  show_col_types = FALSE
)


# ============================================================
# 3. Check required variables
# ============================================================

required_columns <- c(
  "date",
  "unemployment_rate",
  "underemployment_rate",
  "underutilisation_rate",
  "youth_unemployment_rate",
  "youth_underemployment_rate",
  "youth_underutilisation_rate"
)


missing_columns <- setdiff(
  required_columns,
  names(labour_market_slack)
)


if (length(missing_columns) > 0) {

  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )

}


# ============================================================
# 4. Prepare data
# ============================================================

labour_market_slack <- labour_market_slack |>

  mutate(
    date = as.Date(date)
  ) |>

  arrange(date)


# Check for duplicate dates

if (anyDuplicated(labour_market_slack$date) > 0) {

  stop("Duplicate dates found in labour-market slack data.")

}


# ============================================================
# 5. Summary statistics
# ============================================================

slack_summary <- labour_market_slack |>

  summarise(

    observations = n(),

    start_date = min(date, na.rm = TRUE),

    end_date = max(date, na.rm = TRUE),

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


print(slack_summary)


# ============================================================
# 6. Prepare data for visualisation
# ============================================================

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
    ),

    measure = factor(
      measure,
      levels = c(
        "Unemployment",
        "Underemployment",
        "Underutilisation"
      )
    )

  )


# ============================================================
# 7. Plot historical labour-market slack
# ============================================================

slack_plot <- ggplot(
  slack_data,
  aes(
    x = date,
    y = rate,
    colour = measure
  )
) +

  geom_line(
    linewidth = 0.8
  ) +

  scale_colour_manual(
    values = c(
      "Unemployment" = "#17365D",
      "Underemployment" = "#159D9A",
      "Underutilisation" = "#E67E22"
    )
  ) +

  scale_y_continuous(
    labels = label_number(suffix = "%")
  ) +

  labs(
    title = "Labour-market slack in Australia",
    subtitle = paste(
      "Monthly unemployment, underemployment",
      "and underutilisation rates"
    ),
    x = NULL,
    y = "Rate",
    colour = NULL,
    caption = "Source: Australian Bureau of Statistics"
  ) +

  theme_minimal(
    base_size = 12
  ) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


print(slack_plot)


# ============================================================
# 8. Save plot
# ============================================================

ggsave(
  "data/processed/labour_market_slack_trends.png",
  plot = slack_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 9. Analysis complete
# ============================================================

cat(
  "\nLabour-market slack exploration complete.\n"
)
