# ============================================================
# COMBINED LABOUR-MARKET ANALYSIS
# Labour-Market Tightness and Labour-Market Slack
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)


# ============================================================
# 1. LOAD DATA
# ============================================================

# Quarterly labour-market tightness
tightness <- read_csv(
  "data/processed/labour_market_tightness.csv",
  show_col_types = FALSE
)

# Monthly labour-market slack
slack <- read_csv(
  "data/processed/labour_market_slack.csv",
  show_col_types = FALSE
)


# ============================================================
# 2. CHECK INPUT DATA
# ============================================================

glimpse(tightness)
glimpse(slack)

summary(tightness)
summary(slack)


# ============================================================
# 3. PREPARE DATES FOR MATCHING
#
# ABS Job Vacancies is quarterly:
# February, May, August, November.
#
# Labour-market slack is monthly.
# Therefore, slack observations are matched to the
# corresponding vacancy reference month.
# ============================================================

tightness <- tightness |>
  mutate(
    Quarter = as.Date(Quarter),
    year = year(Quarter),
    month = month(Quarter)
  )

slack <- slack |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )


# ============================================================
# 4. SELECT MAIN SLACK MEASURES
# ============================================================

slack_selected <- slack |>
  select(
    date,
    year,
    month,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  )


# ============================================================
# 5. COMBINE TIGHTNESS AND SLACK
#
# Match by year and month rather than exact date because
# the exact reference day can differ between ABS datasets.
# ============================================================

combined <- tightness |>
  left_join(
    slack_selected,
    by = c("year", "month")
  ) |>
  select(
    Quarter,
    Job_Vacancies,
    unemployed,
    Tightness,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  ) |>
  arrange(Quarter)


# ============================================================
# 6. VALIDATE COMBINED DATA
# ============================================================

glimpse(combined)

head(combined, 10)
tail(combined, 10)

# Number of observations
nrow(combined)

# Date coverage
range(combined$Quarter, na.rm = TRUE)

# Check missing values
combined |>
  summarise(
    missing_tightness =
      sum(is.na(Tightness)),
    
    missing_unemployment =
      sum(is.na(unemployment_rate)),
    
    missing_underemployment =
      sum(is.na(underemployment_rate)),
    
    missing_underutilisation =
      sum(is.na(underutilisation_rate))
  )

# Check duplicate quarters
sum(duplicated(combined$Quarter))


# ============================================================
# 7. SUMMARY STATISTICS
# ============================================================

combined |>
  summarise(
    mean_tightness =
      mean(Tightness, na.rm = TRUE),
    
    mean_unemployment =
      mean(unemployment_rate, na.rm = TRUE),
    
    mean_underemployment =
      mean(underemployment_rate, na.rm = TRUE),
    
    mean_underutilisation =
      mean(underutilisation_rate, na.rm = TRUE)
  )


# ============================================================
# 8. CORRELATION WITH LABOUR-MARKET TIGHTNESS
#
# This is descriptive correlation only.
# It does not establish causality.
# ============================================================

correlations <- combined |>
  summarise(
    
    tightness_unemployment =
      cor(
        Tightness,
        unemployment_rate,
        use = "complete.obs"
      ),
    
    tightness_underemployment =
      cor(
        Tightness,
        underemployment_rate,
        use = "complete.obs"
      ),
    
    tightness_underutilisation =
      cor(
        Tightness,
        underutilisation_rate,
        use = "complete.obs"
      )
  )

print(correlations)


# ============================================================
# 9. PLOT: TIGHTNESS OVER TIME
# ============================================================

ggplot(
  combined,
  aes(
    x = Quarter,
    y = Tightness
  )
) +
  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  labs(
    title = "Labour-Market Tightness in Australia",
    subtitle = "ABS Job Vacancies / Unemployed Persons",
    x = NULL,
    y = "Vacancies per unemployed person",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 10. PLOT: TIGHTNESS VS UNEMPLOYMENT
# ============================================================

ggplot(
  combined,
  aes(
    x = Tightness,
    y = unemployment_rate
  )
) +
  geom_point(
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    title = "Labour-Market Tightness and Unemployment",
    subtitle = "Quarterly observations",
    x = "Vacancies per unemployed person",
    y = "Unemployment rate (%)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 11. PLOT: TIGHTNESS VS UNDEREMPLOYMENT
# ============================================================

ggplot(
  combined,
  aes(
    x = Tightness,
    y = underemployment_rate
  )
) +
  geom_point(
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    title = "Labour-Market Tightness and Underemployment",
    subtitle = "Quarterly observations",
    x = "Vacancies per unemployed person",
    y = "Underemployment rate (%)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 12. PLOT: TIGHTNESS VS UNDERUTILISATION
# ============================================================

ggplot(
  combined,
  aes(
    x = Tightness,
    y = underutilisation_rate
  )
) +
  geom_point(
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    title = "Labour-Market Tightness and Underutilisation",
    subtitle = "Quarterly observations",
    x = "Vacancies per unemployed person",
    y = "Underutilisation rate (%)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 13. COMPARE ALL THREE SLACK MEASURES
# ============================================================

combined_long <- combined |>
  select(
    Quarter,
    Tightness,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  ) |>
  pivot_longer(
    cols = c(
      unemployment_rate,
      underemployment_rate,
      underutilisation_rate
    ),
    names_to = "measure",
    values_to = "slack_rate"
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


ggplot(
  combined_long,
  aes(
    x = Tightness,
    y = slack_rate
  )
) +
  geom_point(
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ measure,
    scales = "free_y"
  ) +
  labs(
    title = "Labour-Market Tightness and Labour-Market Slack",
    subtitle = "Quarterly comparison of three slack measures",
    x = "Vacancies per unemployed person",
    y = "Slack rate (%)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 14. CHANGES IN TIGHTNESS AND SLACK
#
# The research question focuses on changes.
# Because observations are quarterly here,
# these are quarter-to-quarter changes.
# ============================================================

combined_changes <- combined |>
  arrange(Quarter) |>
  mutate(
    
    tightness_change =
      Tightness - lag(Tightness),
    
    unemployment_change =
      unemployment_rate -
      lag(unemployment_rate),
    
    underemployment_change =
      underemployment_rate -
      lag(underemployment_rate),
    
    underutilisation_change =
      underutilisation_rate -
      lag(underutilisation_rate)
  )


# Check changes
combined_changes |>
  select(
    Quarter,
    tightness_change,
    unemployment_change,
    underemployment_change,
    underutilisation_change
  ) |>
  tail(20)


# ============================================================
# 15. CORRELATION BETWEEN CHANGES
# ============================================================

change_correlations <- combined_changes |>
  summarise(
    
    tightness_unemployment_change =
      cor(
        tightness_change,
        unemployment_change,
        use = "complete.obs"
      ),
    
    tightness_underemployment_change =
      cor(
        tightness_change,
        underemployment_change,
        use = "complete.obs"
      ),
    
    tightness_underutilisation_change =
      cor(
        tightness_change,
        underutilisation_change,
        use = "complete.obs"
      )
  )

print(change_correlations)


# ============================================================
# 16. PLOT: CHANGES IN TIGHTNESS VS
#     CHANGES IN UNDERUTILISATION
# ============================================================

ggplot(
  combined_changes,
  aes(
    x = tightness_change,
    y = underutilisation_change
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4
  ) +
  geom_point(
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    title = "Changes in Labour-Market Tightness and Slack",
    subtitle = "Quarter-to-quarter changes in tightness and underutilisation",
    x = "Change in labour-market tightness",
    y = "Change in underutilisation rate (percentage points)",
    caption = "Source: Australian Bureau of Statistics"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 18
      ),
    
    panel.grid.minor =
      element_blank()
  )


# ============================================================
# 17. SAVE COMBINED DATA
# ============================================================

write_csv(
  combined,
  "data/processed/combined_tightness_slack.csv"
)

write_csv(
  combined_changes,
  "data/processed/combined_tightness_slack_changes.csv"
)


# ============================================================
# FINAL CHECK
# ============================================================

message("")
message("============================================")
message("COMBINED LABOUR-MARKET ANALYSIS COMPLETED")
message("============================================")
message("")

message(
  "Dataset covers: ",
  min(combined$Quarter, na.rm = TRUE),
  " to ",
  max(combined$Quarter, na.rm = TRUE)
)

message("")

message(
  "Quarterly observations: ",
  nrow(combined)
)

message("")
message(
  "Next stage: investigate the relationship between ",
  "changes in labour-market tightness and labour-market slack."
)

message("")
message("============================================")

correlations
change_correlations

correlations |> print(width = Inf)

change_correlations |> print(width = Inf)