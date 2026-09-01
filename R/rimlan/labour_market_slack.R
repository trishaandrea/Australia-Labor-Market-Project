library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(stringr)

labour_market_slack <- read_csv("data/processed/labour_market_slack.csv")

glimpse(labour_market_slack)

summary(labour_market_slack)

period_sum <- labour_market_slack |> 
  mutate(
    period = case_when(
      date < as.Date("1990-01-01") ~ "1978-1989",
      date < as.Date("1995-01-01") ~ "1990-1994",
      date < as.Date("2008-01-01") ~ "1995-2007",
      date < as.Date("2020-01-01") ~ "2008-2019",
      date < as.Date("2022-01-01") ~ "2020-2021",
      TRUE ~ "2022-present"
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
  ) |> 
  group_by(period) |> 
  summarise(
    obs = n(),
    unemployment = mean(unemployment_rate, na.rm = TRUE),
    underemployment = mean(underemployment_rate,na.rm = TRUE),
    underutilisation = mean(underutilisation_rate,na.rm = TRUE),
    .groups = "drop"
  )

print(period_sum)


period_change <- period_sum |> 
  mutate(
    unemployment_change = 
      unemployment - lag(unemployment),

    underemployment_change = 
      underemployment - lag(underemployment),

    underutilisation_change = 
      underutilisation - lag(underutilisation)
  )

print(period_change)

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
  slack_long,
  aes(
    x = date,
    y = rate,
    colour = measure
  )
) +

  # Major economic shock periods
  annotate(
    "rect",
    xmin = as.Date("1990-07-01"),
    xmax = as.Date("1992-06-01"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.10
  ) +

  annotate(
    "rect",
    xmin = as.Date("2020-03-01"),
    xmax = as.Date("2021-12-01"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.10
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
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
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

    legend.text = element_text(
      size = 11
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank(),

    plot.caption = element_text(
      hjust = 0,
      size = 9
    ),

    plot.margin = margin(
      10, 15, 10, 10
    )
  )


slack_relationship <- labour_market_slack |> 
  summarise(
    unemploy_underemploy_corr =
      cor(
        unemployment_rate,
        underemployment_rate,
        use = "complete.obs"
      )
  )

print(slack_relationship)

ggplot(
  labour_market_slack,
  aes(
    x = unemployment_rate,
    y = underemployment_rate
  )
) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Relation between Unemployment and Underemployment",
    subtitle = "Higher unemployment does not necessarily capture all available labour-market slack",
    x = "Unemployment rate (%)",
    y = "Underemployment rate (%)",
    caption = "Source : Australian Bureau of Statistics, Labour Force Australia"
  ) +
  theme_minimal(base_size = 13) +
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
# SHOCK SUMMARY: PEAK LABOUR-MARKET SLACK
# ============================================================

shock_summary <- labour_market_slack |>
  mutate(
    shock = case_when(
      date >= as.Date("1990-01-01") &
        date <= as.Date("1994-12-01") ~
        "Early 1990s recession",

      date >= as.Date("2020-01-01") &
        date <= as.Date("2021-12-01") ~
        "COVID-19",

      TRUE ~ NA_character_
    ),

    shock = factor(
      shock,
      levels = c(
        "Early 1990s recession",
        "COVID-19"
      )
    )
  ) |>
  filter(
    !is.na(shock)
  ) |>
  group_by(shock) |>
  summarise(
    
    obs = n(),

    mean_unemployment =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),

    peak_unemployment =
      max(
        unemployment_rate,
        na.rm = TRUE
      ),

    peak_unemployment_date =
      date[
        which.max(
          unemployment_rate
        )
      ],

    mean_underemployment =
      mean(
        underemployment_rate,
        na.rm = TRUE
      ),

    peak_underemployment =
      max(
        underemployment_rate,
        na.rm = TRUE
      ),

    peak_underemployment_date =
      date[
        which.max(
          underemployment_rate
        )
      ],

    mean_underutilisation =
      mean(
        underutilisation_rate,
        na.rm = TRUE
      ),

    peak_underutilisation =
      max(
        underutilisation_rate,
        na.rm = TRUE
      ),

    peak_underutilisation_date =
      date[
        which.max(
          underutilisation_rate
        )
      ],

    .groups = "drop"
  )

print(shock_summary)

# ============================================================
# PLOT 2: YOUTH VS OVERALL LABOUR-MARKET SLACK
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
    cols = c(
      unemployment_rate,
      youth_unemployment_rate,
      underutilisation_rate,
      youth_underutilisation_rate
    ),
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

      str_detect(
        measure,
        "unemployment"
      ) &
        !str_detect(
          measure,
          "underutilisation"
        ) ~
        "Unemployment",

      TRUE ~
        "Underutilisation"
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
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  scale_y_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 19
    ),

    plot.subtitle = element_text(
      size = 11,
      margin = margin(bottom = 12)
    ),

    legend.position = "top",

    legend.text = element_text(
      size = 10
    ),

    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank(),

    panel.spacing = unit(
      1.2,
      "lines"
    ),

    plot.caption = element_text(
      hjust = 0,
      size = 9,
      margin = margin(top = 10)
    )
  )

# ============================================================
# PLOT 3: MAJOR SHOCK ANALYSIS
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

      TRUE ~
        NA_character_
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

      unemployment_rate =
        "Unemployment",

      underemployment_rate =
        "Underemployment",

      underutilisation_rate =
        "Underutilisation"
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
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 19
    ),

    plot.subtitle = element_text(
      size = 11,
      margin = margin(bottom = 12)
    ),

    legend.position = "top",

    legend.text = element_text(
      size = 10
    ),

    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank(),

    panel.spacing = unit(
      1.3,
      "lines"
    ),

    plot.caption = element_text(
      hjust = 0,
      size = 9,
      margin = margin(top = 10)
    )
  )

# ============================================================
# ANALYSIS 1: UNEMPLOYMENT AND UNDEREMPLOYMENT BY PERIOD
# ============================================================

period_corr <- labour_market_slack |> 
  
  mutate(
    period = case_when(
      date < as.Date("1990-01-01") ~ "1978-1989",
      date < as.Date("1995-01-01") ~ "1990-1994",
      date < as.Date("2008-01-01") ~ "1995-2007",
      date < as.Date("2020-01-01") ~ "2008-2019",
      date < as.Date("2022-01-01") ~ "2020-2021",
      TRUE ~ "2022-present"
    )
  ) |> 
  
  group_by(period) |> 
  
  summarise(
    
    corr = cor(
      unemployment_rate,
      underemployment_rate,
      use = "complete.obs"
    ),
    
    obs = sum(
      complete.cases(
        unemployment_rate,
        underemployment_rate
      )
    ),
    
    .groups = "drop"
  )

print(period_corr)

# ============================================================
# PLOT 4: CORRELATION BETWEEN UNEMPLOYMENT AND
# UNDEREMPLOYMENT BY PERIOD
# ============================================================

ggplot(
  period_corr,
  aes(
    x = period,
    y = corr
  )
) +
  geom_col(
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.6
  ) +
  labs(
    title = "The relationship between Unemployment & Underemployment has Varied Over Time",
     subtitle = "Correlation between monthly unemployment & underemployment rates",
     x = NULL,
     y = "Correlation",
     caption = "Source: Australian Bureau OF Statistics, Labour Force Australia"
  ) +
  
  scale_y_continuous(
    limits = c(-0.3,1),
    breaks = seq(-0.2,1,0.2)
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),

    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

# ============================================================
# ANALYSIS 2: YOUTH LABOUR-MARKET GAPS BY PERIOD
# ============================================================

youth_gap <- labour_market_slack |> 
  mutate(
    period = case_when(
      date < as.Date("1990-01-01") ~ "1978-1989",
      date < as.Date("1995-01-01") ~ "1990-1994",
      date < as.Date("2008-01-01") ~ "1995-2007",
      date < as.Date("2020-01-01") ~ "2008-2019",
      date < as.Date("2022-01-01") ~ "2020-2021",
      TRUE ~ "2022-present"
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
  ) |> 
  group_by(period) |> 
  summarise(
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
    )
  )

print(youth_gap)


labour_market <- labour_market_slack |> 
  arrange(date) |> 
  mutate(
    unemploy_change = 
      unemployment_rate - lag(unemployment_rate),

    underemploy_change =
      underemployment_rate - lag(underemployment_rate),

    underutilise_change = 
      underutilisation_rate - lag(underutilisation_rate)
  )
summary(
  labour_market |> 
    select(
      unemploy_change,
      underemploy_change,
      underutilise_change
    )
)

larg_change <- labour_market |> 
  select(
    date,
    unemploy_change,
    underemploy_change,
    underutilise_change
  ) |> 
  arrange(
    desc(underutilise_change)
  ) |> 
  slice_head(n = 10)

print(larg_change)