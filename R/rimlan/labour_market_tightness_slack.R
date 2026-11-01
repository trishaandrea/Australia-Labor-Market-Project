# ============================================================
# LABOUR MARKET TIGHTNESS AND SLACK ANALYSIS
# ETC-5543 Australian Labour Market Project
#
# Author: Rimlan
#
# Research question:
# To what extent are changes in job vacancies associated
# with changes in labour-market slack in Australia?
#
# Purpose:
# Examine the relationship between job vacancies,
# labour-market tightness and labour-market slack using
# quarterly Australian labour-market data.
#
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)


# ============================================================
# 2. LOAD COMBINED DATA
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
# 3. CHECK DATA
# ============================================================

cat("\n")
cat("============================================\n")
cat("COMBINED DATA OVERVIEW\n")
cat("============================================\n\n")

glimpse(combined)

print(head(combined))
print(tail(combined))

cat("\nNumber of observations:\n")
print(nrow(combined))

cat("\nDate range:\n")
print(range(combined$Quarter, na.rm = TRUE))


# Check required variables

required_columns <- c(
  "Quarter",
  "Job_Vacancies",
  "unemployed",
  "Tightness",
  "unemployment_rate",
  "underemployment_rate",
  "underutilisation_rate"
)

missing_columns <- setdiff(
  required_columns,
  names(combined)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# Check missing and invalid dates

if (any(is.na(combined$Quarter))) {
  stop("Missing or invalid quarterly dates detected.")
}


# Check duplicate quarters

if (anyDuplicated(combined$Quarter) > 0) {
  stop("Duplicate quarterly observations detected.")
}


# Check that the data are numeric

numeric_columns <- c(
  "Job_Vacancies",
  "unemployed",
  "Tightness",
  "unemployment_rate",
  "underemployment_rate",
  "underutilisation_rate"
)

non_numeric_columns <- numeric_columns[
  !vapply(combined[numeric_columns], is.numeric, logical(1))
]

if (length(non_numeric_columns) > 0) {
  stop(
    paste(
      "These variables must be numeric:",
      paste(non_numeric_columns, collapse = ", ")
    )
  )
}


# ============================================================
# 4. CHECK MISSING VALUES
# ============================================================

missing_summary <- combined |>
  summarise(
    observations = n(),
    missing_tightness = sum(is.na(Tightness)),
    missing_vacancies = sum(is.na(Job_Vacancies)),
    missing_unemployed = sum(is.na(unemployed)),
    missing_unemployment = sum(is.na(unemployment_rate)),
    missing_underemployment = sum(is.na(underemployment_rate)),
    missing_underutilisation = sum(is.na(underutilisation_rate))
  )

cat("\nMissing-value summary:\n")
print(missing_summary)


# ============================================================
# 5. DEFINE HIGH AND LOW LABOUR-MARKET CONDITIONS
# ============================================================

# Historical sample medians are used as descriptive
# classification thresholds.

safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}

tightness_median <- safe_median(combined$Tightness)

underutilisation_median <- safe_median(
  combined$underutilisation_rate
)

unemployment_median <- safe_median(
  combined$unemployment_rate
)

underemployment_median <- safe_median(
  combined$underemployment_rate
)


if (
  any(
    is.na(c(
      tightness_median,
      underutilisation_median,
      unemployment_median,
      underemployment_median
    ))
  )
) {
  stop("At least one classification median cannot be calculated.")
}


cat("\n")
cat("============================================\n")
cat("MEDIAN CLASSIFICATION THRESHOLDS\n")
cat("============================================\n\n")

median_summary <- tibble(
  measure = c(
    "Tightness",
    "Unemployment",
    "Underemployment",
    "Underutilisation"
  ),
  median = c(
    tightness_median,
    unemployment_median,
    underemployment_median,
    underutilisation_median
  )
)

print(median_summary)


# ============================================================
# 6. CLASSIFY LABOUR-MARKET CONDITIONS
# ============================================================

market_conditions <- combined |>
  mutate(
    demand_level = case_when(
      is.na(Tightness) ~ NA_character_,
      Tightness >= tightness_median ~ "High tightness",
      TRUE ~ "Low tightness"
    ),

    slack_level = case_when(
      is.na(underutilisation_rate) ~ NA_character_,
      underutilisation_rate >= underutilisation_median ~
        "High slack",
      TRUE ~ "Low slack"
    ),

    labour_market_conditions = case_when(
      is.na(demand_level) | is.na(slack_level) ~ NA_character_,
      TRUE ~ paste(
        demand_level,
        slack_level,
        sep = " + "
      )
    )
  )


# ============================================================
# 7. COUNT OBSERVATIONS IN EACH CONDITION
# ============================================================

condition_summary <- market_conditions |>
  filter(!is.na(labour_market_conditions)) |>
  count(
    labour_market_conditions,
    name = "observations"
  ) |>
  mutate(
    percentage = round(
      observations / sum(observations) * 100,
      2
    )
  ) |>
  arrange(desc(observations))

cat("\nCondition summary:\n")
print(condition_summary)


# ============================================================
# 8. IDENTIFY POTENTIAL HIGH-TIGHTNESS / HIGH-SLACK PERIODS
# ============================================================

# These observations identify quarters in which both
# tightness and underutilisation were above their
# respective historical sample medians.
#
# This is a descriptive classification, not proof of
# occupational or skills mismatch.

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

cat("\nPotential high-tightness / high-slack periods:\n")
print(potential_periods)


# ============================================================
# 9. SUMMARISE POTENTIAL HIGH-TIGHTNESS / HIGH-SLACK PERIODS
# ============================================================

potential_summary <- potential_periods |>
  summarise(
    observations = n(),

    avg_tightness = if (n() > 0) {
      mean(Tightness, na.rm = TRUE)
    } else {
      NA_real_
    },

    avg_unemployment = if (n() > 0) {
      mean(unemployment_rate, na.rm = TRUE)
    } else {
      NA_real_
    },

    avg_underemployment = if (n() > 0) {
      mean(underemployment_rate, na.rm = TRUE)
    } else {
      NA_real_
    },

    avg_underutilisation = if (n() > 0) {
      mean(underutilisation_rate, na.rm = TRUE)
    } else {
      NA_real_
    },

    min_tightness = if (n() > 0) {
      min(Tightness, na.rm = TRUE)
    } else {
      NA_real_
    },

    max_tightness = if (n() > 0) {
      max(Tightness, na.rm = TRUE)
    } else {
      NA_real_
    },

    min_underutilisation = if (n() > 0) {
      min(underutilisation_rate, na.rm = TRUE)
    } else {
      NA_real_
    },

    max_underutilisation = if (n() > 0) {
      max(underutilisation_rate, na.rm = TRUE)
    } else {
      NA_real_
    }
  )

cat("\nPotential-period summary:\n")
print(potential_summary)


# ============================================================
# 10. COMPARE POTENTIAL PERIODS WITH OTHER PERIODS
# ============================================================

comparison_summary <- market_conditions |>
  filter(
    !is.na(demand_level),
    !is.na(slack_level)
  ) |>
  mutate(
    potential_conditions = case_when(
      demand_level == "High tightness" &
        slack_level == "High slack" ~ "Potential high-tightness / high-slack",
      TRUE ~ "Other periods"
    )
  ) |>
  group_by(potential_conditions) |>
  summarise(
    observations = n(),

    avg_tightness = mean(Tightness, na.rm = TRUE),

    avg_unemployment = mean(
      unemployment_rate,
      na.rm = TRUE
    ),

    avg_underemployment = mean(
      underemployment_rate,
      na.rm = TRUE
    ),

    avg_underutilisation = mean(
      underutilisation_rate,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  mutate(
    percentage = round(
      observations / sum(observations) * 100,
      2
    )
  )

cat("\nComparison of potential periods and other periods:\n")
print(comparison_summary)


# ============================================================
# 11. VISUALISE POTENTIAL HIGH-TIGHTNESS / HIGH-SLACK PERIODS
# ============================================================

plot_data <- market_conditions |>
  mutate(
    potential_mismatch = case_when(
      is.na(Tightness) | is.na(underutilisation_rate) ~ NA_character_,

      demand_level == "High tightness" &
        slack_level == "High slack" ~
        "Potential high-tightness / high-slack",

      TRUE ~ "Other period"
    )
  )


mismatch_points <- plot_data |>
  filter(
    potential_mismatch ==
      "Potential high-tightness / high-slack"
  ) |>
  mutate(
    label = format(Quarter, "%b %Y")
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
    xmin = min(plot_data$Quarter, na.rm = TRUE),
    xmax = max(plot_data$Quarter, na.rm = TRUE),
    ymin = underutilisation_median,
    ymax = Inf,
    fill = "#FDE0DD",
    alpha = 0.35
  ) +

  geom_line(
    colour = "#2C7FB8",
    linewidth = 1.1,
    na.rm = TRUE
  ) +

  geom_point(
    aes(fill = potential_mismatch),
    colour = "white",
    shape = 21,
    size = 3.2,
    stroke = 0.7,
    alpha = 0.9,
    na.rm = TRUE
  ) +

  geom_point(
    data = mismatch_points,
    aes(fill = potential_mismatch),
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
    x = max(plot_data$Quarter, na.rm = TRUE),
    y = underutilisation_median,
    label = paste0(
      "Median: ",
      number(underutilisation_median, accuracy = 0.1),
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
    aes(label = label),
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
      "Other period" = "#8C96A0",
      "Potential high-tightness / high-slack" = "#D73027"
    ),
    na.value = "grey80"
  ) +

  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))
  ) +

  scale_y_continuous(
    labels = label_number(
      accuracy = 1,
      suffix = "%"
    ),
    expand = expansion(mult = c(0.05, 0.12))
  ) +

  labs(
    title = "Periods of High Labour-Market Tightness and Slack",

    subtitle = paste0(
      "Red points indicate quarters where both tightness and ",
      "underutilisation were above their historical medians"
    ),

    x = "Quarter",
    y = "Underutilisation rate (%)",
    fill = "Period type",

    caption = paste0(
      "Dashed line = median underutilisation. ",
      "Classification thresholds use full-sample medians."
    )
  ) +

  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      colour = "#1F2933",
      margin = margin(b = 5)
    ),

    plot.subtitle = element_text(
      size = 10.5,
      colour = "#52606D",
      margin = margin(b = 15)
    ),

    axis.text = element_text(
      colour = "#52606D"
    ),

    axis.title = element_text(
      face = "bold",
      colour = "#1F2933"
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

    legend.title = element_text(face = "bold"),

    legend.text = element_text(colour = "#52606D"),

    plot.caption = element_text(
      size = 9,
      colour = "#7B8794",
      hjust = 0,
      margin = margin(t = 12)
    ),

    plot.margin = margin(15, 20, 15, 15)
  )


# ============================================================
# 12. CHECK QUARTERLY CONTINUITY
# ============================================================

# Compare each date with the previous date plus three months.
# This avoids using %m-% with an incorrect argument order.
#
# A gap means the observations are not consecutive quarters.
# Changes across these gaps are excluded from the analysis.

quarter_check <- combined |>
  arrange(Quarter) |>
  mutate(
    previous_quarter = lag(Quarter),

    expected_quarter = previous_quarter %m+% months(3),

    consecutive_quarter = !is.na(previous_quarter) &
      Quarter == expected_quarter
  ) |>
  filter(
    !is.na(previous_quarter),
    !consecutive_quarter
  ) |>
  select(
    Quarter,
    previous_quarter,
    expected_quarter
  )

cat("\n")
cat("============================================\n")
cat("QUARTERLY CONTINUITY CHECK\n")
cat("============================================\n\n")

print(quarter_check)


# ============================================================
# 13. CALCULATE QUARTER-TO-QUARTER CHANGES
# ============================================================

# Calculate changes only when observations are exactly
# three months apart.
#
# Missing values remain missing. Changes across gaps are
# not treated as ordinary quarter-to-quarter movements.

changes <- combined |>
  arrange(Quarter) |>
  mutate(
    previous_quarter = lag(Quarter),

    expected_quarter = previous_quarter %m+% months(3),

    consecutive_quarter = !is.na(previous_quarter) &
      Quarter == expected_quarter,

    change_vacancies = if_else(
      consecutive_quarter,
      Job_Vacancies - lag(Job_Vacancies),
      NA_real_
    ),

    change_tightness = if_else(
      consecutive_quarter,
      Tightness - lag(Tightness),
      NA_real_
    ),

    change_unemployment = if_else(
      consecutive_quarter,
      unemployment_rate - lag(unemployment_rate),
      NA_real_
    ),

    change_underemployment = if_else(
      consecutive_quarter,
      underemployment_rate - lag(underemployment_rate),
      NA_real_
    ),

    change_underutilisation = if_else(
      consecutive_quarter,
      underutilisation_rate - lag(underutilisation_rate),
      NA_real_
    )
  ) |>
  select(
    Quarter,
    change_vacancies,
    change_tightness,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  )

cat("\nQuarter-to-quarter changes:\n")
glimpse(changes)


# ============================================================
# 14. CORRELATION FUNCTION
# ============================================================

# Calculate Pearson and Spearman correlations using complete
# observations. Return NA if there are too few observations
# or either variable has no variation.

calculate_correlation <- function(x, y, method = "pearson") {

  valid <- complete.cases(x, y)

  x_valid <- x[valid]
  y_valid <- y[valid]

  n_valid <- length(x_valid)

  if (
    n_valid < 3 ||
    dplyr::n_distinct(x_valid) < 2 ||
    dplyr::n_distinct(y_valid) < 2
  ) {
    return(NA_real_)
  }

  cor(
    x_valid,
    y_valid,
    method = method
  )
}


# ============================================================
# 15. CORRELATIONS BETWEEN CHANGES
# ============================================================

# First examine actual vacancy changes against changes
# in labour-market slack.
#
# Then examine changes in tightness against changes in slack.
# These are related but different measures.

correlation_results <- tibble(
  measure = c(
    "Unemployment",
    "Underemployment",
    "Underutilisation"
  ),

  vacancy_observations = c(
    sum(complete.cases(
      changes$change_vacancies,
      changes$change_unemployment
    )),
    sum(complete.cases(
      changes$change_vacancies,
      changes$change_underemployment
    )),
    sum(complete.cases(
      changes$change_vacancies,
      changes$change_underutilisation
    ))
  ),

  vacancy_pearson = c(
    calculate_correlation(
      changes$change_vacancies,
      changes$change_unemployment,
      "pearson"
    ),
    calculate_correlation(
      changes$change_vacancies,
      changes$change_underemployment,
      "pearson"
    ),
    calculate_correlation(
      changes$change_vacancies,
      changes$change_underutilisation,
      "pearson"
    )
  ),

  vacancy_spearman = c(
    calculate_correlation(
      changes$change_vacancies,
      changes$change_unemployment,
      "spearman"
    ),
    calculate_correlation(
      changes$change_vacancies,
      changes$change_underemployment,
      "spearman"
    ),
    calculate_correlation(
      changes$change_vacancies,
      changes$change_underutilisation,
      "spearman"
    )
  ),

  tightness_observations = c(
    sum(complete.cases(
      changes$change_tightness,
      changes$change_unemployment
    )),
    sum(complete.cases(
      changes$change_tightness,
      changes$change_underemployment
    )),
    sum(complete.cases(
      changes$change_tightness,
      changes$change_underutilisation
    ))
  ),

  tightness_pearson = c(
    calculate_correlation(
      changes$change_tightness,
      changes$change_unemployment,
      "pearson"
    ),
    calculate_correlation(
      changes$change_tightness,
      changes$change_underemployment,
      "pearson"
    ),
    calculate_correlation(
      changes$change_tightness,
      changes$change_underutilisation,
      "pearson"
    )
  ),

  tightness_spearman = c(
    calculate_correlation(
      changes$change_tightness,
      changes$change_unemployment,
      "spearman"
    ),
    calculate_correlation(
      changes$change_tightness,
      changes$change_underemployment,
      "spearman"
    ),
    calculate_correlation(
      changes$change_tightness,
      changes$change_underemployment,
      "spearman"
    )
  )
) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

cat("\n")
cat("============================================\n")
cat("CORRELATIONS BETWEEN QUARTERLY CHANGES\n")
cat("============================================\n\n")

print(correlation_results)


# ============================================================
# 16. CHANGE IN TIGHTNESS VS CHANGE IN UNDERUTILISATION
# ============================================================

scatter_data <- changes |>
  filter(
    !is.na(change_tightness),
    !is.na(change_underutilisation)
  )


ggplot(
  scatter_data,
  aes(
    x = change_tightness,
    y = change_underutilisation
  )
) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "#7B8794"
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "#7B8794"
  ) +

  geom_point(
    alpha = 0.7,
    size = 3,
    colour = "#2C7FB8"
  ) +

  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "#D95F02",
    fill = "#D95F02",
    alpha = 0.15
  ) +

  labs(
    title = "Changes in Labour-Market Tightness and Underutilisation",

    subtitle = "Quarter-to-quarter changes and their linear association",

    x = "Change in labour-market tightness",

    y = "Change in underutilisation rate (percentage points)",

    caption = paste(
      "Source: ABS Labour Force and Job Vacancies data.",
      "Only consecutive-quarter changes are included."
    )
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 17. CHANGE IN VACANCIES VS CHANGE IN UNDERUTILISATION
# ============================================================

vacancy_scatter_data <- changes |>
  filter(
    !is.na(change_vacancies),
    !is.na(change_underutilisation)
  )


ggplot(
  vacancy_scatter_data,
  aes(
    x = change_vacancies,
    y = change_underutilisation
  )
) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "#7B8794"
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "#7B8794"
  ) +

  geom_point(
    alpha = 0.7,
    size = 3,
    colour = "#2C7FB8"
  ) +

  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "#D95F02",
    fill = "#D95F02",
    alpha = 0.15
  ) +

  scale_x_continuous(
    labels = label_comma()
  ) +

  labs(
    title = "Changes in Job Vacancies and Labour-Market Slack",

    subtitle = "Quarter-to-quarter changes in vacancies and underutilisation",

    x = "Change in job vacancies",

    y = "Change in underutilisation rate (percentage points)",

    caption = paste(
      "Source: ABS Job Vacancies and Labour Force data.",
      "Only consecutive-quarter changes are included."
    )
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 18. IDENTIFY QUARTERS WITH SIMULTANEOUS INCREASES
# ============================================================

# These quarters experienced an increase in both tightness
# and underutilisation relative to the previous quarter.
#
# They are descriptive observations and should not be
# interpreted as evidence of causation.

simultaneous_increases <- changes |>
  filter(
    !is.na(change_tightness),
    !is.na(change_underutilisation),
    change_tightness > 0,
    change_underutilisation > 0
  ) |>
  arrange(desc(change_underutilisation)) |>
  select(
    Quarter,
    change_vacancies,
    change_tightness,
    change_unemployment,
    change_underemployment,
    change_underutilisation
  )

cat("\nQuarters with simultaneous increases in tightness and slack:\n")
print(simultaneous_increases)


# ============================================================
# 19. VISUALISE QUARTERS WITH SIMULTANEOUS INCREASES
# ============================================================

plot_changes <- changes |>
  mutate(
    quarter_type = case_when(
      is.na(change_tightness) |
        is.na(change_underutilisation) ~ NA_character_,

      change_tightness > 0 &
        change_underutilisation > 0 ~
        "Simultaneous increases",

      TRUE ~ "Other quarters"
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
      filter(quarter_type == "Other quarters"),
    aes(size = abs(change_tightness)),
    alpha = 0.5,
    colour = "#8C96A0"
  ) +

  geom_point(
    data = plot_changes |>
      filter(quarter_type == "Simultaneous increases"),
    aes(size = abs(change_tightness)),
    shape = 21,
    fill = "#D73027",
    colour = "black",
    stroke = 0.8
  ) +

  geom_text_repel(
    data = plot_changes |>
      filter(quarter_type == "Simultaneous increases"),
    aes(
      label = format(Quarter, "%b %Y")
    ),
    seed = 123,
    size = 3,
    max.overlaps = 15,
    min.segment.length = 0
  ) +

  scale_size_continuous(
    name = "Absolute change in tightness"
  ) +

  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +

  labs(
    title = "Quarters with Simultaneous Increases in Tightness and Slack",

    subtitle = "Highlighted quarters experienced increases in both measures",

    x = "Quarter",

    y = "Change in underutilisation rate (percentage points)",

    caption = paste(
      "Quarter-to-quarter changes.",
      "Highlighted observations are descriptive and do not establish causation."
    )
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    plot.subtitle = element_text(
      colour = "grey40"
    ),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


# ============================================================
# 20. COMPARE SIMULTANEOUS INCREASES WITH OTHER QUARTERS
# ============================================================

simultaneous_comparison <- changes |>
  filter(
    !is.na(change_tightness),
    !is.na(change_underutilisation)
  ) |>
  mutate(
    quarter_type = case_when(
      change_tightness > 0 &
        change_underutilisation > 0 ~
        "Simultaneous increases",

      TRUE ~ "Other quarters"
    )
  ) |>
  group_by(quarter_type) |>
  summarise(
    observations = n(),

    avg_vacancy_change = mean(
      change_vacancies,
      na.rm = TRUE
    ),

    avg_tightness_change = mean(
      change_tightness,
      na.rm = TRUE
    ),

    avg_unemployment_change = mean(
      change_unemployment,
      na.rm = TRUE
    ),

    avg_underemployment_change = mean(
      change_underemployment,
      na.rm = TRUE
    ),

    avg_underutilisation_change = mean(
      change_underutilisation,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

cat("\nComparison of simultaneous increases and other quarters:\n")
print(simultaneous_comparison)


# ============================================================
# 21. TOP QUARTERS WITH THE LARGEST SLACK INCREASES
# ============================================================

top_quarters <- simultaneous_increases |>
  slice_max(
    order_by = change_underutilisation,
    n = 10,
    with_ties = FALSE
  )

cat("\nTop quarters with the largest simultaneous slack increases:\n")
print(top_quarters)


# ============================================================
# 22. KEY RESULTS
# ============================================================

key_results <- tibble(
  statistic = c(
    "Median tightness",
    "Median unemployment rate",
    "Median underemployment rate",
    "Median underutilisation rate",

    "Pearson: vacancy changes vs unemployment changes",
    "Spearman: vacancy changes vs unemployment changes",

    "Pearson: vacancy changes vs underemployment changes",
    "Spearman: vacancy changes vs underemployment changes",

    "Pearson: vacancy changes vs underutilisation changes",
    "Spearman: vacancy changes vs underutilisation changes",

    "Pearson: tightness changes vs unemployment changes",
    "Spearman: tightness changes vs unemployment changes",

    "Pearson: tightness changes vs underemployment changes",
    "Spearman: tightness changes vs underemployment changes",

    "Pearson: tightness changes vs underutilisation changes",
    "Spearman: tightness changes vs underutilisation changes"
  ),

  value = c(
    tightness_median,
    unemployment_median,
    underemployment_median,
    underutilisation_median,

    correlation_results$vacancy_pearson[
      correlation_results$measure == "Unemployment"
    ],
    correlation_results$vacancy_spearman[
      correlation_results$measure == "Unemployment"
    ],

    correlation_results$vacancy_pearson[
      correlation_results$measure == "Underemployment"
    ],
    correlation_results$vacancy_spearman[
      correlation_results$measure == "Underemployment"
    ],

    correlation_results$vacancy_pearson[
      correlation_results$measure == "Underutilisation"
    ],
    correlation_results$vacancy_spearman[
      correlation_results$measure == "Underutilisation"
    ],

    correlation_results$tightness_pearson[
      correlation_results$measure == "Unemployment"
    ],
    correlation_results$tightness_spearman[
      correlation_results$measure == "Unemployment"
    ],

    correlation_results$tightness_pearson[
      correlation_results$measure == "Underemployment"
    ],
    correlation_results$tightness_spearman[
      correlation_results$measure == "Underemployment"
    ],

    correlation_results$tightness_pearson[
      correlation_results$measure == "Underutilisation"
    ],
    correlation_results$tightness_spearman[
      correlation_results$measure == "Underutilisation"
    ]
  )
) |>
  mutate(
    value = round(value, 3)
  )

cat("\n")
cat("============================================\n")
cat("KEY RESULTS\n")
cat("============================================\n\n")

print(key_results)


# ============================================================
# 23. FINAL DATA AND ANALYSIS CHECKS
# ============================================================

cat("\n")
cat("============================================\n")
cat("LABOUR-MARKET TIGHTNESS AND SLACK ANALYSIS\n")
cat("COMPLETED\n")
cat("============================================\n\n")

cat(
  "Data period: ",
  format(min(combined$Quarter), "%b %Y"),
  " to ",
  format(max(combined$Quarter), "%b %Y"),
  "\n",
  sep = ""
)

cat(
  "Total observations: ",
  nrow(combined),
  "\n",
  sep = ""
)

cat(
  "Valid observations for condition classification: ",
  sum(
    complete.cases(
      market_conditions$Tightness,
      market_conditions$underutilisation_rate
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Potential high-tightness / high-slack quarters: ",
  nrow(potential_periods),
  "\n",
  sep = ""
)

cat(
  "Quarters with simultaneous increases: ",
  nrow(simultaneous_increases),
  "\n",
  sep = ""
)

cat(
  "Quarters with continuity gaps: ",
  nrow(quarter_check),
  "\n",
  sep = ""
)

cat("\n")
cat("Next step: interpret the results alongside the\n")
cat("vacancy, slack and wage-price-index analyses.\n")
cat("\n")
cat("============================================\n")