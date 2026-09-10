library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(stringr)
library(plotly)

labour_market_slack <- read_csv("data/processed/labour_market_slack.csv")

glimpse(labour_market_slack)

summary(labour_market_slack)

labour_market_slack |> 
  summarise(
    observations = n(),
    unique_dates = n_distinct(date),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE)
  ) |> 
  print()

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
      "2022-present")
    )
  )

period_sum <- labour_market_slack |> 
  group_by(period) |> 
  summarise(
    obs = n(),
    unemployment = mean(unemployment_rate, na.rm = TRUE),
    underemployment = mean(underemployment_rate,na.rm = TRUE),
    underutilisation = mean(underutilisation_rate, na.rm = TRUE),
  .groups = "drop")

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
    subtitle = "Unemployment alone does not capture all available labour-market slack",
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
      measure%in%
        c(
          "Overall unemployment",
          "Youth unemployment"
        ) ~ "Unemployment",

        measure%in% c(
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

period_plt <- period_corr |>
  mutate(
    label = sprintf("%.2f", corr),
    hover_text = paste0(
      "<b>", period, "</b>",
      "<br><br>",
      "<br>Correlation: ", sprintf("%.2f", corr),
      "<br><b>Observations:</b", obs
    )
  )

plot_ly(
  data = period_plt,
  x = ~period,
  y = ~corr,
  type = "bar",
  text = ~label,
  
  textposition = "outside",
  textfont = list(
    size = 15,
    color = "#222222"
  ),
  hovertext = ~hover_text,
  hoverinfo = "text",

  showlegend = FALSE
) |>
  
  layout(
    title = list(
      text = paste0(
        "<b> Relationship Between Unemployment and Underemployment Across Varied Periods</b>",
        "<br>",
        "<span style='font-size:14px;font-weight:normal;'>",
        "Pearson correlation between monthly unemployment and underemployment rates",
        "</span>"
      ),
      x = 0.05,
      xanchor = "left"
    ),
    
    xaxis = list(
      title = "",
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(
        size = 12
      )
    ),
    
    yaxis = list(
      title = "<b>Correlation</b",
      range = c(-0.3, 1.05),
      dtick = 0.2,
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.08)",
      zeroline = TRUE,
      zerolinecolor = "grey",
      zerolinewidth = 1.5,
      tickfont = list(
        size = 11
      )
    ),

    bargap = 0.35,

    plot_bgcolor = "white",
    paper_bgcolor = "white",
    
    margin = list(
      l = 75,
      r = 35,
      t = 105,
      b = 65
    ),
    
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#333333",
      font = list(
        size = 13
      )
    )
  ) |> 
  config(
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d",
      "select2d"
    )
  )
  

# ============================================================
# ANALYSIS 2: YOUTH LABOUR-MARKET GAPS BY PERIOD
# ============================================================

youth_gap <- labour_market_slack |> 
  group_by(period) |> 
  summarise(
    youth_unemployment_gap = 
    mean(youth_unemployment_gap,
      na.rm = TRUE),

    youth_underemployment_gap = 
      mean(youth_underemployment_gap, 
        na.rm = TRUE),
    
    youth_underutilisation_gap =
      mean(youth_underutilisation_gap,
      na.rm = TRUE),

      .groups = "drop"
    )

print(youth_gap)
  
# ============================================================
# ANALYSIS 3: MONTH-TO-MONTH CHANGES
# ============================================================


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

change_sum <- labour_market |> 
  summarise(
    mean_unemploy_change =
      mean(unemploy_change,
      na.rm = TRUE),

    sd_unemploy_change =
      sd(unemploy_change, 
      na.rm = TRUE),

    mean_underemploy_change =
      mean(underemploy_change,
      na.rm = TRUE),

    sd_underemploy_change =
      sd(underemploy_change ,
      na.rm = TRUE),
    
    mean_underutilise_change =
      mean(underutilise_change,
      na.rm = TRUE),

    sd_underutilise_change =
      sd(underutilise_change,
      na.rm = TRUE)
  )

print(change_sum)

# ============================================================
# ANALYSIS 4
# LARGEST MONTHLY INCREASES IN UNDERUTILISATION
# ============================================================

larg_inc <- labour_market |> 
  select(date,
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
# ANALYIS 5 
# LARGEST MONTHLY DECREASES IN UNDERUTILISATION
# ============================================================

larg_dec <- labour_market |> 
  select(
    date,
    unemploy_change,
    underemploy_change,
    underutilise_change
  ) |> 
  arrange(underutilise_change) |> 
  slice_head( 
    n = 10)

print(larg_dec)

# ============================================================
# ANALYSIS 6 
# YEAR-ON-YEAR CHANGES
# ============================================================

labour_market <- labour_market |> 
  mutate(
    unemploy_yoy =
      unemployment_rate - 
      lag(unemployment_rate, 12
      ),
    
    underemploy_yoy =
      underemployment_rate - 
      lag(underemployment_rate, 12
      ),

    underutilise_yoy =
      underutilisation_rate - 
      lag(underutilisation_rate, 12
      )
  )

# ============================================================
# ANALYSIS 7
# YEAR-ON-YEAR CHANGE DURING RECENT PERIOD
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
# PLOT 6
# YEAR-ON-YEAR CHNAGE IN LABOUR MARKET SLACK
# ============================================================

labour_change <- labour_market |> 
  select(
    date, 
    unemploy_yoy,
    underemploy_yoy,
    underutilise_yoy
  ) |> 
  pivot_longer(
    cols = c(
      unemploy_yoy,
      underemploy_yoy,
      underutilise_yoy
    ),
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
    color = measure
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.8
  )+
  labs(
    title = "Year-On-Year Changes in Labour-Market Slack",
    subtitle = "Difference from the same month one year earlier",
    x = NULL,
    y = "Percentage point change",
    color = NULL,
    source = "Australian Bureau of Statistics, Labour Force Australia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold", 
      size = 18
    ),

    legend.position = "top",

    panel.grid.minor = element_blank()
  )

# ============================================================
# ANALYSIS 8 
# YOUTH EMPLOYMENT GAP OVER TIME
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
    linewidth = 0.9
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
      suffix = "pp"
    )
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
# ANALYSIS 9
# PRE-SHOCK VS SHOCK CONDITIONS
# ============================================================

shock_comp <- labour_market_slack |> 
  mutate(
    period_type = case_when(
      date >= as.Date("1990-07-01") &
        date < as.Date("1992-06-01") ~
        "1990s recession",
      date >= as.Date("2018-01-01") &
        date < as.Date("2020-03-01") ~
        "Pre Covid",
      date >= as.Date("2020-03-01") &
        date < as.Date("2021-12-01") ~
        "Covid-19",
      TRUE ~
        NA_character_
    )
  ) |> 
  filter(
    !is.na(period_type)
  ) |> 
  group_by(
    period_type
  ) |> 
  summarise(
    unemployment =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),
    underemployment =
      mean(
        underemployment_rate,
        na.rm = TRUE
      ),
    underutilisation =
      mean(
        underutilisation_rate,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(shock_comp)

# ============================================================
# ANALYSIS 10
# RECOVERY AFTER COVID-19
# ============================================================

pre_covid <- labour_market_slack |> 
  filter(
    date >= as.Date("2019-01-01"),
    date <= as.Date("2019-12-01")
  ) |> 
  summarise(
    unemployment =
      mean(unemployment_rate,
      na.rm = TRUE
    ),
    underemployment =
      mean(underemployment_rate,
      na.rm = TRUE
    ),
    underutilisation = 
      mean(underutilisation_rate,
      na.rm = TRUE
    )
  )

print(pre_covid)

covid <- labour_market_slack |> 
  filter(
    date >= as.Date("2020-03-01"),
    date <= as.Date("2026-07-01")
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

print(covid |> 
  select(
    date, 
    unemployment_difference,
    underemployment_difference,
    underutilisation_difference
  ) |> 
  tail(20)
)

# ============================================================
# ANALYSIS 11
# RECENT LABOUR MARKET CONDITIONS
# ============================================================

market_cond <- labour_market_slack |> 
  filter(
    date >= as.Date("2022-01-01")
  ) |> 
  summarise(
    mean_unemploy =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),
    mean_underemploy = 
      mean(
        underemployment_rate,
        na.rm = TRUE
      ),
    mean_underutilise = 
      mean(
        underutilisation_rate,
        na.rm = TRUE
      ),
    latest_unemploy = 
      unemployment_rate[
        which.max(date)
      ],
    latest_underemploy = 
      underemployment_rate[
        which.max(date)
      ],
    latest_underutilse = 
      underutilisation_rate[
        which.max(date)
      ]
    )

print(market_cond)
   
# ============================================================
# FINAL CHECK
# ============================================================

message("")
message("============================================")

message("LABOUR MARKET SLACK ANALYSIS COMPLETED")
message("============================================")
message("")

message(
  "Dataset covers: ",
  min(
    labour_market_slack$date,
    na.rm = TRUE
  ),
  "to",
  max(
    labour_market_slack$date,
    na.rm = TRUE
  )
)

message("")
message("Next stage:"
)
message(
  "Combined validated labour market slack measures with",
  "the labour demand/vacancy dataset."
)
message("")
message("============================================")

