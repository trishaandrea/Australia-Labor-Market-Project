# ============================================================
# LABOUR MARKET SLACK ANALYSIS
# ETC-5543 Australian Labour Market Project
#
# Key areas:
#   1. Historical labour-market slack
#   2. Unemployment and underemployment relationship
#   3. Youth labour-market gaps
#   4. Major economic shocks
#   5. Pearson and Spearman correlations
#   6. Monthly and annual changes
#   7. Recovery following COVID-19
#   8. Recent labour-market conditions
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "scales",
  "plotly"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {

  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them using install.packages()."
    )
  )

}

library(tidyverse)
library(lubridate)
library(scales)
library(plotly)


# ============================================================
# 2. GENERAL SETTINGS
# ============================================================

slack_file <- "data/processed/labour_market_slack.csv"


# ============================================================
# 3. LOAD DATA
# ============================================================

cat("\n")
cat("============================================\n")
cat("LOADING LABOUR-MARKET SLACK DATA\n")
cat("============================================\n\n")

if (!file.exists(slack_file)) {

  stop(
    paste0(
      "Cannot find: ",
      slack_file,
      "\nCurrent working directory: ",
      getwd()
    )
  )

}

labour_market_slack <- read_csv(
  slack_file,
  show_col_types = FALSE
)


# ============================================================
# 4. CHECK REQUIRED VARIABLES
# ============================================================

required_columns <- c(
  "date",
  "unemployment_rate",
  "underemployment_rate",
  "underutilisation_rate",
  "youth_unemployment_rate",
  "youth_underutilisation_rate",
  "youth_unemployment_gap",
  "youth_underemployment_gap",
  "youth_underutilisation_gap"
)

missing_columns <- setdiff(
  required_columns,
  names(labour_market_slack)
)

if (length(missing_columns) > 0) {

  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_columns, collapse = ", "),
      "\n\nCheck labour_market_slack.csv."
    )
  )

}

cat("Required variables found.\n")


# ============================================================
# 5. PREPARE DATA
# ============================================================

labour_market_slack <- labour_market_slack |>

  mutate(

    date = as.Date(date),

    across(
      where(is.character),
      identity
    )

  ) |>

  arrange(date)


# Check for missing dates

if (any(is.na(labour_market_slack$date))) {

  stop("Missing or invalid dates detected.")

}


# Check for duplicate dates

if (anyDuplicated(labour_market_slack$date) > 0) {

  stop(
    "Duplicate dates detected. Check the source dataset before analysis."
  )

}


# ============================================================
# 6. DATA OVERVIEW
# ============================================================

cat("\n")
cat("============================================\n")
cat("DATA OVERVIEW\n")
cat("============================================\n\n")

glimpse(labour_market_slack)

summary(labour_market_slack)

data_overview <- labour_market_slack |>

  summarise(

    observations = n(),

    unique_dates = n_distinct(date),

    first_date = min(date),

    last_date = max(date)

  )

print(data_overview)


# ============================================================
# 7. HISTORICAL PERIOD CLASSIFICATION
# ============================================================

labour_market_slack <- labour_market_slack |>

  mutate(

    period = case_when(

      date < as.Date("1990-01-01") ~
        "1978-1989",

      date < as.Date("1995-01-01") ~
        "1990-1994",

      date < as.Date("2008-01-01") ~
        "1995-2007",

      date < as.Date("2020-01-01") ~
        "2008-2019",

      date < as.Date("2022-01-01") ~
        "2020-2021",

      TRUE ~
        "2022-present"

    ),

    period = factor(

      period,

      levels = c(
        "1978-1989",
        "1990-1994",
        "1995-2007",
        "2008-2019",
        "2020-2021",
        "2022-present"
      )

    )

  )


# ============================================================
# 8. SUMMARY STATISTICS BY PERIOD
# ============================================================

period_sum <- labour_market_slack |>

  group_by(period) |>

  summarise(

    observations = n(),

    unemployment = mean(
      unemployment_rate,
      na.rm = TRUE
    ),

    underemployment = mean(
      underemployment_rate,
      na.rm = TRUE
    ),

    underutilisation = mean(
      underutilisation_rate,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) |>

  mutate(
    across(
      c(
        unemployment,
        underemployment,
        underutilisation
      ),
      ~ round(.x, 2)
    )
  )

print(period_sum)


# ============================================================
# PLOT 1: LABOUR-MARKET SLACK OVER TIME
# ============================================================

slack_long <- labour_market_slack |>

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
  slack_long,
  aes(
    x = date,
    y = rate,
    colour = measure
  )
) +

  annotate(
    "rect",
    xmin = as.Date("1990-07-01"),
    xmax = as.Date("1992-06-01"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.10,
    fill = "grey"
  ) +

  annotate(
    "rect",
    xmin = as.Date("2020-03-01"),
    xmax = as.Date("2021-12-01"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.10,
    fill = "grey"
  ) +

  geom_line(
    linewidth = 0.9
  ) +

  labs(

    title = "Labour-Market Slack in Australia",

    subtitle = "Unemployment, underemployment and underutilisation rates",

    x = NULL,

    y = "Rate (%)",

    colour = NULL,

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  scale_x_date(

    date_breaks = "5 years",

    date_labels = "%Y",

    expand = expansion(
      mult = c(0.01, 0.02)
    )

  ) +

  scale_y_continuous(

    labels = label_number(
      suffix = "%"
    ),

    expand = expansion(
      mult = c(0, 0.05)
    )

  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18
    ),

    plot.subtitle = element_text(
      size = 12
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),

    legend.position = "top",

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank(),

    plot.caption = element_text(
      hjust = 0,
      size = 9
    )

  )


# ============================================================
# ANALYSIS 1: UNEMPLOYMENT AND UNDEREMPLOYMENT CORRELATION
# ============================================================

slack_relationship <- labour_market_slack |>

  summarise(

    observations = sum(
      complete.cases(
        unemployment_rate,
        underemployment_rate
      )
    ),

    pearson_correlation = cor(
      unemployment_rate,
      underemployment_rate,
      method = "pearson",
      use = "complete.obs"
    ),

    spearman_correlation = cor(
      unemployment_rate,
      underemployment_rate,
      method = "spearman",
      use = "complete.obs"
    )

  ) |>

  mutate(

    across(
      c(
        pearson_correlation,
        spearman_correlation
      ),
      ~ round(.x, 3)
    )

  )

print(slack_relationship)


# ============================================================
# PLOT 2: UNEMPLOYMENT AND UNDEREMPLOYMENT RELATIONSHIP
# LOESS TREND
# ============================================================

ggplot(

  labour_market_slack,

  aes(
    x = unemployment_rate,
    y = underemployment_rate
  )

) +

  geom_point(
    alpha = 0.5,
    colour = "#17365D"
  ) +

  geom_smooth(

    method = "loess",

    formula = y ~ x,

    span = 0.75,

    se = TRUE,

    colour = "#E67E22",

    fill = "#E67E22",

    alpha = 0.15

  ) +

  labs(

    title = "Relationship Between Unemployment and Underemployment",

    subtitle = "LOESS trend illustrates the potentially nonlinear relationship",

    x = "Unemployment rate (%)",

    y = "Underemployment rate (%)",

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18
    ),

    plot.subtitle = element_text(
      size = 11
    ),

    panel.grid.minor = element_blank()

  )


# ============================================================
# PLOT 3: YOUTH VS OVERALL LABOUR-MARKET SLACK
# ============================================================

youth_long <- labour_market_slack |>

  select(

    date,

    unemployment_rate,

    youth_unemployment_rate,

    underutilisation_rate,

    youth_underutilisation_rate

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
        "Youth unemployment",

      underutilisation_rate =
        "Overall underutilisation",

      youth_underutilisation_rate =
        "Youth underutilisation"

    ),

    category = case_when(

      measure %in% c(
        "Overall unemployment",
        "Youth unemployment"
      ) ~ "Unemployment",

      measure %in% c(
        "Overall underutilisation",
        "Youth underutilisation"
      ) ~ "Underutilisation"

    )

  )


ggplot(

  youth_long,

  aes(
    x = date,
    y = rate,
    colour = measure
  )

) +

  geom_line(
    linewidth = 0.85
  ) +

  facet_wrap(
    ~ category,
    ncol = 1,
    scales = "free_y"
  ) +

  labs(

    title = "Youth Labour-Market Slack Is Consistently Higher",

    subtitle = "Comparison of youth and overall unemployment and underutilisation",

    x = NULL,

    y = "Rate (%)",

    colour = NULL,

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +

  scale_y_continuous(
    labels = label_number(suffix = "%")
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 19
    ),

    legend.position = "top",

    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank()

  )


# ============================================================
# PLOT 4: MAJOR ECONOMIC SHOCKS
# ============================================================

shock_data <- labour_market_slack |>

  filter(

    date >= as.Date("1988-01-01"),

    date <= as.Date("2022-12-01")

  ) |>

  mutate(

    shock_period = case_when(

      date >= as.Date("1988-01-01") &
        date <= as.Date("1994-12-01") ~
        "Early 1990s recession",

      date >= as.Date("2018-01-01") &
        date <= as.Date("2022-12-01") ~
        "COVID-19 period",

      TRUE ~ NA_character_

    )

  ) |>

  filter(
    !is.na(shock_period)
  )


shock_long <- shock_data |>

  select(

    date,

    shock_period,

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

  shock_long,

  aes(
    x = date,
    y = rate,
    colour = measure
  )

) +

  geom_line(
    linewidth = 0.9
  ) +

  facet_wrap(
    ~ shock_period,
    ncol = 1,
    scales = "free_x"
  ) +

  labs(

    title = "Labour-Market Slack During Major Economic Shocks",

    subtitle = "Comparison of the early 1990s recession and COVID-19 period",

    x = NULL,

    y = "Rate (%)",

    colour = NULL,

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +

  scale_y_continuous(
    labels = label_number(suffix = "%")
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 19
    ),

    legend.position = "top",

    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank()

  )


# ============================================================
# ANALYSIS 2: UNEMPLOYMENT AND UNDEREMPLOYMENT BY PERIOD
# PEARSON AND SPEARMAN
# ============================================================

period_corr <- labour_market_slack |>

  group_by(period) |>

  summarise(

    observations = sum(
      complete.cases(
        unemployment_rate,
        underemployment_rate
      )
    ),

    pearson = cor(
      unemployment_rate,
      underemployment_rate,
      method = "pearson",
      use = "complete.obs"
    ),

    spearman = cor(
      unemployment_rate,
      underemployment_rate,
      method = "spearman",
      use = "complete.obs"
    ),

    .groups = "drop"

  ) |>

  mutate(

    across(
      c(pearson, spearman),
      ~ round(.x, 3)
    )

  )

print(period_corr)


# ============================================================
# PLOT 5: PEARSON AND SPEARMAN CORRELATIONS BY PERIOD
# ============================================================

period_corr_long <- period_corr |>

  pivot_longer(

    cols = c(
      pearson,
      spearman
    ),

    names_to = "method",

    values_to = "correlation"

  ) |>

  mutate(

    method = recode(

      method,

      pearson = "Pearson",

      spearman = "Spearman"

    ),

    label = sprintf(
      "%.2f",
      correlation
    )

  )


ggplot(

  period_corr_long,

  aes(
    x = period,
    y = correlation,
    fill = method
  )

) +

  geom_col(

    position = position_dodge(
      width = 0.8
    ),

    width = 0.7

  ) +

  geom_text(

    aes(
      label = label
    ),

    position = position_dodge(
      width = 0.8
    ),

    vjust = -0.4,

    size = 3.5

  ) +

  scale_fill_manual(

    values = c(

      "Pearson" = "#17365D",

      "Spearman" = "#159D9A"

    )

  ) +

  scale_y_continuous(

    limits = c(

      min(
        -0.3,
        min(
          period_corr_long$correlation,
          na.rm = TRUE
        ) - 0.1
      ),

      1

    )

  ) +

  labs(

    title = "Relationship Between Unemployment and Underemployment Across Periods",

    subtitle = "Comparison of Pearson and Spearman correlation coefficients",

    x = NULL,

    y = "Correlation coefficient",

    fill = NULL,

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 17
    ),

    legend.position = "top",

    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),

    panel.grid.minor = element_blank()

  )


# ============================================================
# ANALYSIS 3: YOUTH LABOUR-MARKET GAPS BY PERIOD
# ============================================================

youth_gap <- labour_market_slack |>

  group_by(period) |>

  summarise(

    observations = n(),

    youth_unemployment_gap = mean(
      youth_unemployment_gap,
      na.rm = TRUE
    ),

    youth_underemployment_gap = mean(
      youth_underemployment_gap,
      na.rm = TRUE
    ),

    youth_underutilisation_gap = mean(
      youth_underutilisation_gap,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) |>

  mutate(

    across(
      ends_with("_gap"),
      ~ round(.x, 2)
    )

  )

print(youth_gap)


# ============================================================
# ANALYSIS 4: MONTH-TO-MONTH CHANGES
# ============================================================

labour_market <- labour_market_slack |>

  arrange(date) |>

  mutate(

    unemploy_change =
      unemployment_rate -
      lag(unemployment_rate),

    underemploy_change =
      underemployment_rate -
      lag(underemployment_rate),

    underutilise_change =
      underutilisation_rate -
      lag(underutilisation_rate)

  )


change_sum <- labour_market |>

  summarise(

    mean_unemploy_change =
      mean(
        unemploy_change,
        na.rm = TRUE
      ),

    sd_unemploy_change =
      sd(
        unemploy_change,
        na.rm = TRUE
      ),

    mean_underemploy_change =
      mean(
        underemploy_change,
        na.rm = TRUE
      ),

    sd_underemploy_change =
      sd(
        underemploy_change,
        na.rm = TRUE
      ),

    mean_underutilise_change =
      mean(
        underutilise_change,
        na.rm = TRUE
      ),

    sd_underutilise_change =
      sd(
        underutilise_change,
        na.rm = TRUE
      )

  ) |>

  mutate(
    across(
      everything(),
      ~ round(.x, 3)
    )
  )

print(change_sum)


# ============================================================
# ANALYSIS 5: LARGEST MONTHLY INCREASES IN UNDERUTILISATION
# ============================================================

larg_inc <- labour_market |>

  select(

    date,

    unemploy_change,

    underemploy_change,

    underutilise_change

  ) |>

  arrange(
    desc(underutilise_change)
  ) |>

  slice_head(
    n = 10
  )

print(larg_inc)


# ============================================================
# ANALYSIS 6: LARGEST MONTHLY DECREASES IN UNDERUTILISATION
# ============================================================

larg_dec <- labour_market |>

  select(

    date,

    unemploy_change,

    underemploy_change,

    underutilise_change

  ) |>

  arrange(
    underutilise_change
  ) |>

  slice_head(
    n = 10
  )

print(larg_dec)


# ============================================================
# ANALYSIS 7: YEAR-ON-YEAR CHANGES
# ============================================================

labour_market <- labour_market |>

  mutate(

    unemploy_yoy =
      unemployment_rate -
      lag(
        unemployment_rate,
        12
      ),

    underemploy_yoy =
      underemployment_rate -
      lag(
        underemployment_rate,
        12
      ),

    underutilise_yoy =
      underutilisation_rate -
      lag(
        underutilisation_rate,
        12
      )

  )


# ============================================================
# ANALYSIS 8: YEAR-ON-YEAR CHANGES IN RECENT PERIOD
# ============================================================

recent_change <- labour_market |>

  filter(
    date >= as.Date("2019-01-01")
  ) |>

  select(

    date,

    unemploy_yoy,

    underemploy_yoy,

    underutilise_yoy

  )

print(recent_change)


# ============================================================
# PLOT 6: YEAR-ON-YEAR CHANGES IN LABOUR-MARKET SLACK
# ============================================================

labour_change <- labour_market |>

  select(

    date,

    unemploy_yoy,

    underemploy_yoy,

    underutilise_yoy

  ) |>

  pivot_longer(

    cols = -date,

    names_to = "measure",

    values_to = "change"

  ) |>

  mutate(

    measure = recode(

      measure,

      unemploy_yoy = "Unemployment",

      underemploy_yoy = "Underemployment",

      underutilise_yoy = "Underutilisation"

    )

  )


ggplot(

  labour_change,

  aes(
    x = date,
    y = change,
    colour = measure
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

    title = "Year-on-Year Changes in Labour-Market Slack",

    subtitle = "Difference from the same month one year earlier",

    x = NULL,

    y = "Percentage-point change",

    colour = NULL,

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18
    ),

    legend.position = "top",

    panel.grid.minor = element_blank()

  )


# ============================================================
# ANALYSIS 9: YOUTH UNEMPLOYMENT GAP OVER TIME
# ============================================================

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
    linewidth = 0.9,
    colour = "#17365D"
  ) +

  labs(

    title = "Youth Unemployment Gap",

    subtitle = "Youth unemployment rate minus the overall unemployment rate",

    x = NULL,

    y = "Percentage-point gap",

    caption = "Source: Australian Bureau of Statistics, Labour Force Australia"

  ) +

  scale_y_continuous(
    labels = label_number(
      suffix = " pp"
    )
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18
    ),

    panel.grid.minor = element_blank()

  )


# ============================================================
# ANALYSIS 10: PRE-SHOCK VS SHOCK CONDITIONS
# ============================================================

shock_comp <- labour_market_slack |>

  mutate(

    period_type = case_when(

      date >= as.Date("1990-07-01") &
        date < as.Date("1992-06-01") ~
        "1990s recession",

      date >= as.Date("2018-01-01") &
        date < as.Date("2020-03-01") ~
        "Pre-COVID",

      date >= as.Date("2020-03-01") &
        date < as.Date("2021-12-01") ~
        "COVID-19",

      TRUE ~ NA_character_

    )

  ) |>

  filter(
    !is.na(period_type)
  ) |>

  group_by(period_type) |>

  summarise(

    observations = n(),

    unemployment = mean(
      unemployment_rate,
      na.rm = TRUE
    ),

    underemployment = mean(
      underemployment_rate,
      na.rm = TRUE
    ),

    underutilisation = mean(
      underutilisation_rate,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) |>

  mutate(

    across(
      c(
        unemployment,
        underemployment,
        underutilisation
      ),
      ~ round(.x, 2)
    )

  )

print(shock_comp)


# ============================================================
# ANALYSIS 11: POST-COVID COMPARISON WITH 2019 BASELINE
# ============================================================

pre_covid <- labour_market_slack |>

  filter(

    date >= as.Date("2019-01-01"),

    date <= as.Date("2019-12-01")

  ) |>

  summarise(

    unemployment = mean(
      unemployment_rate,
      na.rm = TRUE
    ),

    underemployment = mean(
      underemployment_rate,
      na.rm = TRUE
    ),

    underutilisation = mean(
      underutilisation_rate,
      na.rm = TRUE
    )

  )


post_covid_comparison <- labour_market_slack |>

  filter(

    date >= as.Date("2020-03-01")

  ) |>

  mutate(

    unemployment_difference =
      unemployment_rate -
      pre_covid$unemployment,

    underemployment_difference =
      underemployment_rate -
      pre_covid$underemployment,

    underutilisation_difference =
      underutilisation_rate -
      pre_covid$underutilisation

  )


print(

  post_covid_comparison |>

    select(

      date,

      unemployment_difference,

      underemployment_difference,

      underutilisation_difference

    ) |>

    tail(20)

)


# ============================================================
# ANALYSIS 12: RECENT LABOUR-MARKET CONDITIONS
# ============================================================

market_cond <- labour_market_slack |>

  filter(
    date >= as.Date("2022-01-01")
  ) |>

  summarise(

    observations = n(),

    mean_unemployment = mean(
      unemployment_rate,
      na.rm = TRUE
    ),

    mean_underemployment = mean(
      underemployment_rate,
      na.rm = TRUE
    ),

    mean_underutilisation = mean(
      underutilisation_rate,
      na.rm = TRUE
    ),

    latest_unemployment =
      unemployment_rate[
        which.max(date)
      ],

    latest_underemployment =
      underemployment_rate[
        which.max(date)
      ],

    latest_underutilisation =
      underutilisation_rate[
        which.max(date)
      ],

    latest_date = max(date)

  ) |>

  mutate(

    across(
      where(is.numeric),
      ~ round(.x, 2)
    )

  )

print(market_cond)


# ============================================================
# 13. FINAL CHECKS
# ============================================================

cat("\n")
cat("============================================\n")
cat("LABOUR-MARKET SLACK ANALYSIS COMPLETED\n")
cat("============================================\n\n")

cat(
  "Dataset covers: ",
  format(
    min(labour_market_slack$date),
    "%B %Y"
  ),
  " to ",
  format(
    max(labour_market_slack$date),
    "%B %Y"
  ),
  "\n",
  sep = ""
)

cat(
  "Total observations: ",
  nrow(labour_market_slack),
  "\n\n"
)

cat("Next stage:\n")

cat(
  "Combine validated labour-market slack measures with\n",
  "the labour-demand and vacancy dataset.\n"
)

cat("\n")
cat("============================================\n")
