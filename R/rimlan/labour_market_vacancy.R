library(tidyverse)
library(lubridate)
library(scales)

# ============================================================
# 1. Load combined vacancy and labour-market slack data
# ============================================================

combined_data <- read_csv(
  "data/processed/combined_tightness_slack.csv"
)

# ============================================================
# 3. Check the data
# ============================================================

combined_data<- combined_data |> 
  mutate(
    Quarter = as.Date(Quarter)
  ) |> 
  arrange(Quarter)


# ============================================================
# 3. Check the data
# ============================================================

glimpse(combined_data)

combined_data |> 
  summarise(
    observations = n(),
    first_quarter = min(Quarter, na.rm = TRUE),
    last_quarter = max(Quarter, na.rm = TRUE)
  )

# ============================================================
# 4. Check missing vacancy observations
# ============================================================

combined_data |> 
  filter(is.na(Job_Vacancies)) |> 
  select(
    Quarter,
    Job_Vacancies,
    Tightness
  )


# ============================================================
# 5. Calculate quarter-to-quarter changes
# ============================================================

vacancy_changes <- combined_data |> 
  arrange(Quarter) |> 
  mutate(
    previous_quarter = lag(Quarter),

    change_vacancies = if_else(
      Quarter == previous_quarter %m+% months(3),
      Job_Vacancies - lag(Job_Vacancies),
      NA_real_
    ),

    change_unemployment = 
      unemployment_rate - lag(unemployment_rate),

    change_underemployment = 
      underemployment_rate - lag(underemployment_rate),

    change_underutilisation = 
      underutilisation_rate - lag(underutilisation_rate)
  )

# ============================================================
# 6. View the calculated changes
# ============================================================

vacancy_changes |> 
  select(
    Quarter,
    Job_Vacancies,
    change_vacancies,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  ) |> 
  head(10)

# ============================================================
# 7. Correlation: vacancies and unemployment
# ============================================================

vacancy_unemployment <- cor(
  vacancy_changes$change_vacancies,
  vacancy_changes$change_unemployment,
  use = "complete.obs"
)

vacancy_unemployment

# ============================================================
# 8. Correlation: vacancies and underemployment
# ============================================================

vacancy_underemployment <- cor(
  vacancy_changes$change_vacancies,
  vacancy_changes$change_underemployment,
  use = "complete.obs"
)

vacancy_underemployment

# ============================================================
# 9. Correlation: vacancies and underutilisation 
# ============================================================

vacancy_underutilisation <- cor(
  vacancy_changes$change_vacancies,
  vacancy_changes$change_underutilisation,
  use = "complete.obs"
)

vacancy_underutilisation

# ============================================================
# 10. Combine correlation results
# ============================================================

vacancy_correlation <- tibble(
  relationship = c(
    "Vacancy changes vs unemployment changes",
    "Vacancy changes vs underemployment changes",
    "Vacancy changes vs underutilisation changes"
  ),
  correlation = c(
    vacancy_unemployment,
    vacancy_underemployment,
    vacancy_underutilisation
  )
)

vacancy_correlation

# ============================================================
# 11. Prepare data for combined scatter plot
# ============================================================

vacancy_scatter_data <- vacancy_changes |> 
  select(
    Quarter,
    change_vacancies,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  ) |> 
  pivot_longer(
    cols = c(
      change_unemployment,
      change_underemployment,
      change_underutilisation
    ),
    names_to = "slack_measure",
    values_to = "change_slack"
  ) |> 
    mutate(
      slack_measure = recode(
        slack_measure, 
        change_unemployment = "Unemployment",
        change_underemployment = "Underemployment",
        change_underutilisation = "Underutilisation"
      )
    )

# ============================================================
# 12. Combined scatter plot
# ============================================================

ggplot(
  vacancy_scatter_data,
  aes(
    x = change_vacancies,
    y = change_slack
  )
) +
  geom_point(
    alpha = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(
    ~ slack_measure,
    scales = "free_y"
  ) +
  labs(
    title = "Changes in job vacancies and labour-market slack",
    subtitle = "Quarter-to-quarter changes",
    x = "Change in job vacancies",
    y = "Change in slack measure (%)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 12
  )

# ============================================================
# 13. Identify unusual movements
# ============================================================

unusual_vacancy_slack <- vacancy_changes |>
  filter(
    !is.na(change_vacancies),
    !is.na(change_underutilisation)
  ) |>
  filter(
    change_vacancies > 0,
    change_underutilisation > 0
  ) |>
  select(
    Quarter,
    Job_Vacancies,
    change_vacancies,
    unemployment_rate,
    change_unemployment,
    underemployment_rate,
    change_underemployment,
    underutilisation_rate,
    change_underutilisation
  ) |>
  arrange(desc(change_underutilisation))

unusual_vacancy_slack

# ============================================================
# 14. Vacancies and underutilisation moving
#     in opposite directions
# ============================================================

expected_relationship <- vacancy_changes |> 
  filter(
    !is.na(change_vacancies),
    !is.na(change_underutilisation)
  ) |> 
  filter(
    (change_vacancies > 0 & change_underutilisation < 0) |
    (change_vacancies < 0 & change_underutilisation > 0)
  ) |> 
  select(
    Quarter,
    change_vacancies,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  )

expected_relationship

# ============================================================
# 15. Create lagged vacancy changes
# ============================================================

 lag_data <- vacancy_changes |> 
  select(
    Quarter,
    change_vacancies
  ) |> 
  rename(
    vacancy_quarter = Quarter,
    vacancy_change = change_vacancies
  )

# ============================================================
# 16. One-quarter lag
# ============================================================

lag_1 <- vacancy_changes |> 
  select(
    Quarter,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  ) |>
  left_join(
    lag_data |> 
      mutate(
        Quarter = vacancy_quarter %m+% months(3)
      ) |>
      select(
        Quarter,
        vacancy_change
      ),
    by = "Quarter"
  )

# ============================================================
# 17. One-quarter lag correlations
# ============================================================

lag_1_cor <- tibble(
  relationship = c(
    "Vacancy changes vs next-quarter unemployment changes",
    "Vacancy changes vs next-quarter underemployment changes",
    "Vacancy changes vs next-quarter underutilisation changes"
  ),
  correlation = c(
    cor(
      lag_1$vacancy_change,
      lag_1$change_unemployment,
      use = "complete.obs"
    ),

    cor(
      lag_1$vacancy_change,
      lag_1$change_underemployment,
      use = "complete.obs"
    ),

    cor(
      lag_1$vacancy_change,
      lag_1$change_underutilisation,
      use = "complete.obs"
    )
  )
)

lag_1_cor

# ============================================================
# 18. Two-quarter lag
# ============================================================

lag_2 <- vacancy_changes |> 
  select(
    Quarter,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  ) |> 
  left_join(
    lag_data |> 
      mutate(
        Quarter = vacancy_quarter %m+% months(6)
      ) |> 
      select(
        Quarter,
        vacancy_change
      ),
      by = "Quarter"
  )

# ============================================================
# 19. Two-quarter lag correlations
# ============================================================
lag_2_cor <- tibble(
  relationship = c(
    "Vacancy changes vs two-quarter ahead unemployment changes",
    "Vacancy changes vs two-quarter ahead underemployment changes",
    "Vacancy changes vs two-quarter ahead underutilisation changes"
  ),
  correlation = c(
    cor(
      lag_2$vacancy_change,
      lag_2$change_unemployment,
      use = "complete.obs"
    ),

    cor(
      lag_2$vacancy_change,
      lag_2$change_underemployment,
      use = "complete.obs"
    ),

    cor(
      lag_2$vacancy_change,
      lag_2$change_underutilisation,
      use = "complete.obs"
    )
  )
)

lag_2_cor

# ============================================================
# 20. Compare same-quarter and lagged correlations
# ============================================================
lag_summary <- bind_rows(
  vacancy_correlation |> 
    mutate(lag = "Same quarter"),

  lag_1_cor |> 
    mutate(lag = "1 quarter ahead"),

  lag_2_cor |> 
    mutate(lag = "2 quarters ahead")
) |> 
  select(
    lag,
    relationship,
    correlation
  )

lag_summary



