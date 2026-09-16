# ============================================================ 
# 1. LOAD PACKAGES 
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)

# ============================================================ 
#  2. LOAD COMBINED DATA 
# ============================================================

combined <- read_csv(
  "data/processed/combined_tightness_slack.csv",
  show_col_types = FALSE
) |> 
  mutate(
    Quarter = as.Date(Quarter)
  ) |> 
  arrange(Quarter)

# ============================================================ 
#  3. CHECK DATA 
# ============================================================

glimpse(combined)

head(combined)

tail(combined)

nrow(combined)

range(
  combined$Quarter,
  na.rm = TRUE
)

# ============================================================ 
# 4. CHECK MISSING VALUES 
# ============================================================

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

# ============================================================ 
# 5. DEFINE HIGH AND LOW LABOUR-MARKET CONDITIONS 
# ============================================================

tightness_median <- median(
  combined$Tightness,
  na.rm = TRUE
)

underutilisation_median <- median(
  combined$underutilisation_rate,
  na.rm = TRUE
)

unemployment_median <- median(
  combined$unemployment_rate,
  na.rm = TRUE
)

underemployment_median <- median(
  combined$underemployment_rate,
  na.rm = TRUE
)

print(tightness_median)
print(underutilisation_median)
print(unemployment_median)
print(underemployment_median)

# ============================================================ 
# 6. CLASSIFY LABOUR-MARKET CONDITIONS
# ============================================================

market_conditions <- combined |> 
  mutate(
    demand_level = case_when(
      Tightness >= tightness_median ~
        "High tightness",
      TRUE ~
        "Low tightness"
    ),
    
    slack_level = case_when(
      underutilisation_rate >= 
        underutilisation_median ~
        "High slack",
      TRUE ~
        "Low slack"
    ),

    labour_market_conditions = paste(
      demand_level,
      slack_level,
      sep = " + "
    )
  )

# ============================================================ 
# 7. COUNT OBSERVATIONS IN EACH CONDITION 
# ============================================================

condition_summary <- market_conditions |> 
  count(
    labour_market_conditions,
    name = "observations"
  ) |> 
  mutate(
    percentage = 
      observations / sum(observations) * 100
  ) |> 
  arrange(desc(observations))

print(condition_summary)

# ============================================================ 
# 8. IDENTIFY POTENTIAL MISMATCH PERIODS
# ============================================================ 

potential_conditions <- market_conditions |> 
  filter(
    demand_level == "High tightness",
    slack_level == "High slack"
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

print(potential_conditions)

# ============================================================ 
# 9. SUMMARISE POTENTIAL MISMATCH PERIODS 
# ============================================================

potential_summary <- potential_conditions |> 
  summarise(
    observations = n(),

    avg_tightness = 
      mean(Tightness, na.rm = TRUE),

    avg_unemployment =
      mean(unemployment_rate, na.rm = TRUE),

    avg_underemployment = 
      mean(underemployment_rate, na.rm = TRUE),

    avg_underutilisation = 
      mean(underutilisation_rate, na.rm = TRUE),

    min_tightness = 
      min(Tightness, na.rm = TRUE),

    max_tightness = 
      max(Tightness, na.rm = TRUE),

    min_underutilisation =
      min(underutilisation_rate,
      na.rm = TRUE
    ),

    
    max_underutilisation = 
      max(underutilisation_rate,
      na.rm = TRUE
    )
  )

print(potential_summary)


# ============================================================ 
# 10. COMPARE POTENTIAL MISMATCH PERIODS WITH OTHER PERIODS
# ============================================================ 

comparison_summary <- market_conditions |>
  mutate(
    potential_conditions = case_when(
      demand_level == "High tightness" &
        slack_level == "High slack" ~
        "Potential mismatch",

      TRUE ~
        "Other periods"
    )
  ) |>
  group_by(
    potential_conditions
  ) |>
  summarise(
    observations = n(),

    avg_tightness =
      mean(
        Tightness,
        na.rm = TRUE
      ),

    avg_unemployment =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),

    avg_underemployment =
      mean(
        underemployment_rate,
        na.rm = TRUE
      ),

    avg_underutilisation =
      mean(
        underutilisation_rate,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

print(comparison_summary)

# ============================================================ 
# 11. IDENTIFY POTENTIAL HIGH-TIGHTNESS/ HIGH-SLACK PERIODS
# ============================================================ 

potential_periods <- market_conditions |> 
  filter(
    demand_level == "High tightness",
    slack_level == "High slack"
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

print(potential_periods)

# ============================================================
# 12. VISUALISE POTENTIAL HIGH-TIGHTNESS / HIGH-SLACK PERIODS
# ============================================================

plot_data <- market_conditions |>
  mutate(
    potential_mismatch = case_when(
      demand_level == "High tightness" &
        slack_level == "High slack" ~
        "Potential high-tightness / high-slack",
      TRUE ~
        "Other period"
    )
  )

# Select potential mismatch periods
mismatch_points <- plot_data |>
  filter(
    potential_mismatch ==
      "Potential high-tightness / high-slack"
  ) |>
  mutate(
    label = format(
      Quarter,
      "%b %Y"
    )
  )


ggplot(
  plot_data,
  aes(
    x = Quarter,
    y = underutilisation_rate
  )
) +
  annotate(
    "rect",
    xmin = min(
      plot_data$Quarter,
      na.rm = TRUE
    ),
    xmax = max(
      plot_data$Quarter,
      na.rm = TRUE
    ),
    ymin = underutilisation_median,
    ymax = Inf,
    fill = "#FDE0DD",
    alpha = 0.35
  ) +
  geom_line(
    colour = "#2C7FB8",
    linewidth = 1.1
  ) +
  geom_point(
    aes(
      fill = potential_mismatch
    ),
    colour = "white",
    shape = 21,
    size = 3.2,
    stroke = 0.7,
    alpha = 0.9
  ) +
  geom_point(
    data = mismatch_points,
    aes(
      fill = potential_mismatch
    ),
    colour = "#7F0000",
    shape = 21,
    size = 5.2,
    stroke = 1.2
  ) +
  geom_hline(
    yintercept = underutilisation_median,
    colour = "#4D4D4D",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  annotate(
    "label",
    x = max(
      plot_data$Quarter,
      na.rm = TRUE
    ),
    y = underutilisation_median,
    label = paste0(
      "Median: ",
      number(
        underutilisation_median,
        accuracy = 0.1
      ),
      "%"
    ),
    hjust = 1,
    vjust = -0.45,
    size = 3.2,
    colour = "#4D4D4D",
    fill = "white",
    label.size = 0.2
  ) +
  geom_label_repel(
    data = mismatch_points,
    aes(
      label = label
    ),
    seed = 123,
    direction = "both",
    min.segment.length = 0,
    box.padding = 0.55,
    point.padding = 0.35,
    force = 1.2,
    max.overlaps = Inf,
    size = 3.1,
    fontface = "bold",
    colour = "#7F0000",
    fill = "#FFF5F5",
    label.size = 0.25,
    segment.colour = "#B2182B",
    segment.linewidth = 0.35
  ) +
  scale_fill_manual(
    values = c(
      "Other period" =
        "#8C96A0",

      "Potential high-tightness / high-slack" =
        "#D73027"
    )
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 1,
      suffix = "%"
    ),
    expand = expansion(
      mult = c(0.05, 0.12)
    )
  ) +
  labs(
    title =
      "Periods of high labour-market tightness and slack",

    subtitle = paste0(
      "Red points indicate quarters where both tightness and ",
      "underutilisation were above their historical medians"
    ),

    x = "Quarter",

    y = "Underutilisation rate (%)",

    fill = "Period type",

    caption = paste0(
      "Historical period: ",
      format(
        min(
          plot_data$Quarter,
          na.rm = TRUE
        ),
        "%Y"
      ),
      "–",
      format(
        max(
          plot_data$Quarter,
          na.rm = TRUE
        ),
        "%Y"
      ),
      " | Dashed line = median underutilisation | ",
      "Classification thresholds use sample medians"
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 16,
      colour = "#1F2933",
      margin = margin(
        b = 5
      )
    ),

    plot.subtitle = element_text(
      size = 10.5,
      colour = "#52606D",
      margin = margin(
        b = 15
      )
    ),

    axis.text = element_text(
      colour = "#52606D"
    ),

    axis.title.x = element_text(
      face = "bold",
      colour = "#1F2933"
    ),

    axis.title.y = element_text(
      face = "bold",
      colour = "#1F2933",
      margin = margin(
        r = 10
      )
    ),

    axis.ticks = element_blank(),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_line(
      colour = "#E5E7EB",
      linewidth = 0.35
    ),

    panel.grid.major.y = element_line(
      colour = "#D9E2EC",
      linewidth = 0.45
    ),

    legend.position = "top",

    legend.title = element_text(
      face = "bold"
    ),

    legend.text = element_text(
      colour = "#52606D"
    ),

    plot.caption = element_text(
      size = 9,
      colour = "#7B8794",
      hjust = 0,
      margin = margin(
        t = 12
      )
    ),

    plot.margin = margin(
      15,
      20,
      15,
      15
    )
  )

# ============================================================
# 13. CALCULATE QUARTER-TO-QUARTER CHANGES 
# ============================================================

changes <- combined |> 
  arrange(Quarter) |> 
  mutate(
    change_tightness =
      Tightness - lag(Tightness),

    change_unemployment =
      unemployment_rate - lag(unemployment_rate),

    change_underemployment =
      underemployment_rate - lag(underemployment_rate),

    change_underutilisation = 
      underutilisation_rate - lag(underutilisation_rate)
  ) |> 
  select(
    Quarter,
    change_tightness,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  ) |> 
  drop_na()

glimpse(changes)

# ============================================================
# 14. CORRELATION BETWEEN CHANGES
# ============================================================
change_correlations <- tibble(
  measure = c(
    "Unemployment",
    "Underemployment",
    "Underutilisation"
  ),
  correlation = c(
    cor(
      changes$change_tightness,
      changes$change_unemployment
    ),

    cor(
      changes$change_tightness,
      changes$change_underemployment
    ),

    cor(
      changes$change_tightness,
      changes$change_underutilisation
    )
  )
)

print(change_correlations)

# ============================================================
# 15. CHANGE IN TIGHTNESS VS CHANGE IN UNDERUTILISATION
# ============================================================

ggplot(
  changes,
  aes(
    x = change_tightness,
    y = change_underutilisation
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    alpha = 0.7,
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Changes in labour-market tightness and underutilisation",
    subtitle = "Quarter-to-quarter changes",
    x = "Change in labour-market tightness",
    y = "Change in underutilisation rate (percentage points)",
    caption = "Source: ABS Labour Force and Job Vacancies data"
  ) +
  theme_minimal(base_size = 13)

# ============================================================
# 16. IDENTIFY UNUSUAL QUARTERS
# ============================================================

unusual_quarters <- changes |> 
  filter(
    change_tightness > 0,
    change_underutilisation > 0
  ) |> 
  arrange(desc(change_underutilisation)) |> 
  select(
    Quarter,
    change_tightness,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  )

print(unusual_quarters)
 
# ============================================================
# 17. VISUALISE UNUSUAL QUARTERS
# ============================================================

plot_changes <- changes |> 
  mutate(
    unusual_quarters = case_when(
      change_tightness > 0 &
        change_underutilisation > 0 ~
        "Tightness and underutilisation",
      TRUE ~
        "Other quarter"
    )
  )

ggplot(
  plot_changes,
  aes(
    x = Quarter,
    y = change_underutilisation
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    data = plot_changes |> 
      filter(unusual_quarters == "Other quarter"),
    aes(
      size = abs(change_tightness)
    ),
    alpha = 0.5 
  ) +
  geom_point(
    data = plot_changes |> 
      filter(
        unusual_quarters == 
          "Tightness and underutilisation"
      ),
      aes(
        size = abs(change_tightness)
      ),
      shape = 21,
      fill = "red",
      colour = "black",
      stroke = 0.8
  ) +
  geom_text(
    data = plot_changes |> 
    filter(
      unusual_quarters ==
        "Tightness and underutilisation"
    ),
    aes(
      label = format(Quarter, "%b %Y")
    ),
    vjust = -1,
    size = 3
  ) +
  scale_size_continuous(
    name  = "Absolute change in tightness"
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Unusual quarters in the relationship between tightness and slack",
    subtitle = "Highlighted quarters experienced increases in both tightness and underutilisation",
    x = "Quarter",
    y = "Change in underutilisation rate (% points)",
    caption = "Quarter-to-Quarter changes; highlighted observations are descriptive and do not establish causation."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    legend.position = "top"
  )

# ============================================================
# 18. COMPARE UNUSUAL QUARTERS WITH OTHER QUARTERS
# ============================================================

unusual_comp <- changes |> 
  mutate(
    quarter_type = case_when(
      change_tightness > 0 & 
        change_underutilisation > 0 ~
        "Tightness and underutilisation",
      TRUE ~
        "Other quarters"
    )
  ) |> 
  group_by(quarter_type) |> 
  summarise(
    observations = n(), 

    avg_tightness = 
      mean(change_tightness, na.rm = TRUE),

    avg_unemployment = 
      mean(change_unemployment, na.rm = TRUE),

    avg_underemployment = 
      mean(change_underemployment, na.rm = TRUE),

    avg_underutilisation = 
      mean(change_underutilisation, na.rm = TRUE),

    .groups = "drop"
  )

print(unusual_comp)

# ============================================================
# 19. TOP UNUSUAL QUARTERS
# ============================================================

top_quarters <- unusual_quarters |> 
  slice_max(
    order_by = change_underutilisation,
    n = 10
  )

print(top_quarters)

# ============================================================
# 20. KEY RESULTS 
# ============================================================

key_results <- tibble(
  statistic = c(
    "Median tightness",
    "Median unemployment rate",
    "Median underemployment rate",
    "Median underutilisation rate",
    "Correlation: tightness vs unemployment changes",
    "Correlation: tightness vs underemployment changes",
    "Correlation: tightness vs underutilisation changes"
  ),
  value = c(
    tightness_median,
    unemployment_median,
    underemployment_median,
    underutilisation_median,
    change_correlations$correlation [
      change_correlations$measure == "Unemployment"
    ],
    change_correlations$correlation [
      change_correlations$measure == "Underemployment"
    ],
    change_correlations$correlation [
      change_correlations$measure == "Underutilisation"
    ]
  )
)

print(key_results)

