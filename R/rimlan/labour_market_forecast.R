# ============================================================
# Labour Market Forecasting
# ETC-5543 Australian Labour Market Project
#
# Author: Rimlan
#
# Purpose:
# Explore whether Australian labour-market slack measures
# can be forecast over the next 12 months using historical data.
#
# Final forecast:
#   - Best-performing model refitted using all available data
#   - 12-month forecast
#   - 80% and 95% prediction intervals
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "fpp3",
  "scales"
)

missing_packages <- required_packages[
  !sapply(
    required_packages,
    requireNamespace,
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {

  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall them using:\n\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}


library(tidyverse)
library(lubridate)
library(fpp3)
library(scales)


# ============================================================
# 2. General settings
# ============================================================

forecast_horizon <- 12

test_size <- 12

primary_measure <- "Underutilisation"


# ============================================================
# 3. Find project data
# ============================================================

find_project_file <- function(filename) {

  candidates <- c(

    file.path(
      "data",
      "processed",
      filename
    ),

    file.path(
      "..",
      "data",
      "processed",
      filename
    ),

    file.path(
      "../..",
      "data",
      "processed",
      filename
    ),

    file.path(
      "../../..",
      "data",
      "processed",
      filename
    )

  )

  found <- candidates[
    file.exists(candidates)
  ]

  if (length(found) == 0) {
    return(NA_character_)
  }

  found[[1]]
}


slack_file <- find_project_file(
  "labour_market_slack.csv"
)


if (is.na(slack_file)) {

  stop(
    paste0(
      "\nCould not find labour_market_slack.csv.\n\n",
      "Expected location:\n",
      "data/processed/labour_market_slack.csv\n\n",
      "Current working directory:\n",
      getwd()
    )
  )
}


processed_dir <- dirname(
  slack_file
)


if (!dir.exists(processed_dir)) {

  dir.create(
    processed_dir,
    recursive = TRUE
  )

}


# ============================================================
# 4. Load data
# ============================================================

cat("\n")
cat("============================================\n")
cat("LOADING LABOUR-MARKET SLACK DATA\n")
cat("============================================\n\n")


labour_market_slack <- read_csv(
  slack_file,
  show_col_types = FALSE
)


# ============================================================
# 5. Check required variables
# ============================================================

required_columns <- c(

  "date",

  "unemployment_rate",

  "underemployment_rate",

  "underutilisation_rate"

)


missing_columns <- required_columns[
  !required_columns %in%
    names(labour_market_slack)
]


if (length(missing_columns) > 0) {

  stop(
    paste0(
      "Missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      "\n\nAvailable columns:\n",
      paste(
        names(labour_market_slack),
        collapse = ", "
      )
    )
  )

}


cat(
  "✓ Required variables found.\n"
)


# ============================================================
# 6. Prepare data
# ============================================================

forecast_data <- labour_market_slack |>

  select(
    date,
    unemployment_rate,
    underemployment_rate,
    underutilisation_rate
  ) |>

  mutate(

    date = as.Date(date),

    unemployment_rate =
      as.numeric(
        unemployment_rate
      ),

    underemployment_rate =
      as.numeric(
        underemployment_rate
      ),

    underutilisation_rate =
      as.numeric(
        underutilisation_rate
      )

  ) |>

  filter(
    !is.na(date)
  ) |>

  arrange(date)


# ------------------------------------------------------------
# Remove duplicate dates
# ------------------------------------------------------------

forecast_data <- forecast_data |>

  group_by(date) |>

  summarise(

    unemployment_rate =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),

    underemployment_rate =
      mean(
        underemployment_rate,
        na.rm = TRUE
      ),

    underutilisation_rate =
      mean(
        underutilisation_rate,
        na.rm = TRUE
      ),

    .groups = "drop"

  ) |>

  arrange(date)


# ------------------------------------------------------------
# Replace infinite values with NA
# ------------------------------------------------------------

forecast_data <- forecast_data |>

  mutate(

    across(

      c(
        unemployment_rate,
        underemployment_rate,
        underutilisation_rate
      ),

      ~ ifelse(
        is.infinite(.x),
        NA_real_,
        .x
      )

    )

  )


# ============================================================
# 7. Data quality report
# ============================================================

cat("\n")
cat("============================================\n")
cat("DATA QUALITY CHECK\n")
cat("============================================\n\n")


cat(
  "Data range: ",
  format(
    min(
      forecast_data$date,
      na.rm = TRUE
    ),
    "%B %Y"
  ),
  " to ",
  format(
    max(
      forecast_data$date,
      na.rm = TRUE
    ),
    "%B %Y"
  ),
  "\n"
)


cat(
  "Total observations: ",
  nrow(forecast_data),
  "\n\n"
)


missing_summary <- forecast_data |>

  summarise(

    unemployment_missing =
      sum(
        is.na(
          unemployment_rate
        )
      ),

    underemployment_missing =
      sum(
        is.na(
          underemployment_rate
        )
      ),

    underutilisation_missing =
      sum(
        is.na(
          underutilisation_rate
        )
      )

  )


print(
  missing_summary
)


# ============================================================
# 8. Forecasting function
# ============================================================

run_forecast_analysis <- function(

    data,

    value_column,

    measure_name,

    horizon = 12,

    test_size = 12

) {


  cat("\n")
  cat("============================================\n")

  cat(
    "FORECASTING:",
    measure_name,
    "\n"
  )

  cat("============================================\n\n")


  # ==========================================================
  # 8.1 Select series
  # ==========================================================

  series_data <- data |>

    select(

      date,

      value =
        all_of(
          value_column
        )

    ) |>

    filter(
      !is.na(value)
    ) |>

    arrange(date)


  if (
    nrow(series_data) <=
      test_size + 24
  ) {

    stop(
      paste0(
        "Not enough observations to forecast ",
        measure_name
      )
    )

  }


  # ==========================================================
  # 8.2 Convert to monthly tsibble
  # ==========================================================

  series_ts <- series_data |>

    mutate(
      date = yearmonth(date)
    ) |>

    as_tsibble(
      index = date
    )


  # ==========================================================
  # 8.3 Train/test split
  # ==========================================================

  total_n <- nrow(
    series_ts
  )


  train_n <- total_n -
    test_size


  train_data <- series_ts |>

    slice_head(
      n = train_n
    )


  test_data <- series_ts |>

    slice_tail(
      n = test_size
    )


  cat(
    "Training observations:",
    nrow(train_data),
    "\n"
  )


  cat(
    "Testing observations:",
    nrow(test_data),
    "\n\n"
  )


  # ==========================================================
  # 8.4 Fit forecasting models
  # ==========================================================

  cat(
    "Fitting models...\n\n"
  )


  models <- train_data |>

    model(

      Naive =
        NAIVE(value),

      Seasonal_Naive =
        SNAIVE(value),

      ETS =
        ETS(value),

      ARIMA =
        ARIMA(value)

    )


  # ==========================================================
  # 8.5 Forecast test period
  # ==========================================================

  test_forecasts <- models |>

    forecast(
      new_data = test_data
    )


  # ==========================================================
  # 8.6 Evaluate models
  # ==========================================================

  accuracy_results <- accuracy(

    test_forecasts,

    test_data

  ) |>

    as_tibble() |>

    select(

      .model,

      RMSE,

      MAE,

      MASE

    ) |>

    rename(

      model =
        .model

    ) |>

    mutate(

      measure =
        measure_name,

      .before = 1

    ) |>

    arrange(
      RMSE
    )


  cat(
    "\nModel performance:\n\n"
  )


  print(
    accuracy_results
  )


  # ==========================================================
  # 8.7 Select best model
  # ==========================================================

  best_model_name <- accuracy_results |>

    filter(
      !is.na(RMSE)
    ) |>

    slice_min(

      RMSE,

      n = 1,

      with_ties = FALSE

    ) |>

    pull(
      model
    )


  cat(
    "\nBest model:",
    best_model_name,
    "\n"
  )


  # ==========================================================
  # 8.8 Residual diagnostics
  # ==========================================================

  selected_model <- models |>

    select(
      all_of(best_model_name)
    )


  residuals <- selected_model |>

    augment()


  residual_ljung_box <- residuals |>

    features(
      .innov,
      ljung_box,
      lag = 12
    )


  # ==========================================================
  # 8.9 Refit all models using full dataset
  # ==========================================================

  cat(
    "\nRefitting models using all available observations...\n"
  )


  full_models <- series_ts |>

    model(

      Naive =
        NAIVE(value),

      Seasonal_Naive =
        SNAIVE(value),

      ETS =
        ETS(value),

      ARIMA =
        ARIMA(value)

    )


  # ==========================================================
  # 8.10 Select best full model
  # ==========================================================

  selected_full_model <- full_models |>

    select(
      all_of(best_model_name)
    )


  # ==========================================================
  # 8.11 Produce future forecast
  # ==========================================================

  final_forecast_raw <- selected_full_model |>

    forecast(
      h = horizon
    )


  # ==========================================================
  # 8.12 Extract forecast intervals
  # ==========================================================

  interval_80 <- final_forecast_raw |>

    hilo(
      level = 80
    ) |>

    as_tibble()


  interval_95 <- final_forecast_raw |>

    hilo(
      level = 95
    ) |>

    as_tibble()


  # ==========================================================
  # 8.13 Create forecast table
  # ==========================================================

  final_forecast <- final_forecast_raw |>

    as_tibble() |>

    select(
      date,
      .mean
    ) |>

    rename(
      forecast = .mean
    ) |>

    mutate(

      date =
        as.Date(date),

      measure =
        measure_name,

      model =
        best_model_name,

      type =
        "Forecast"

    )


  # ==========================================================
  # 8.14 Extract 80% intervals
  # ==========================================================

  interval_80_values <- interval_80 |>

    mutate(

      interval =
        as.character(
          `80%`
        ),

      lo80 =
        readr::parse_number(
          stringr::str_extract(
            interval,
            "(?<=\\[)[^,]+"
          )
        ),

      hi80 =
        readr::parse_number(
          stringr::str_extract(
            interval,
            "(?<=,)[^\\]]+"
          )
        )

    ) |>

    select(

      date,

      lo80,

      hi80

    )


  # ==========================================================
  # 8.15 Extract 95% intervals
  # ==========================================================

  interval_95_values <- interval_95 |>

    mutate(

      interval =
        as.character(
          `95%`
        ),

      lo95 =
        readr::parse_number(
          stringr::str_extract(
            interval,
            "(?<=\\[)[^,]+"
          )
        ),

      hi95 =
        readr::parse_number(
          stringr::str_extract(
            interval,
            "(?<=,)[^\\]]+"
          )
        )

    ) |>

    select(

      date,

      lo95,

      hi95

    )


  # ==========================================================
  # 8.16 Combine intervals
  # ==========================================================

  final_forecast <- final_forecast |>

    left_join(

      interval_80_values,

      by = "date"

    ) |>

    left_join(

      interval_95_values,

      by = "date"

    ) |>

    select(

      date,

      measure,

      model,

      type,

      forecast,

      lo80,

      hi80,

      lo95,

      hi95

    )


  # ==========================================================
  # 8.17 Historical data
  # ==========================================================

  historical <- series_data |>

    mutate(

      date =
        as.Date(date),

      measure =
        measure_name,

      type =
        "Historical"

    )


  # ==========================================================
  # 8.18 Forecast plot
  # ==========================================================

  forecast_plot <- ggplot() +

    geom_ribbon(

      data =
        final_forecast,

      aes(

        x =
          date,

        ymin =
          lo95,

        ymax =
          hi95

      ),

      alpha =
        0.15

    ) +

    geom_ribbon(

      data =
        final_forecast,

      aes(

        x =
          date,

        ymin =
          lo80,

        ymax =
          hi80

      ),

      alpha =
        0.25

    ) +

    geom_line(

      data =
        historical,

      aes(

        x =
          date,

        y =
          value

      ),

      linewidth =
        0.8

    ) +

    geom_line(

      data =
        final_forecast,

      aes(

        x =
          date,

        y =
          forecast

      ),

      linewidth =
        1.1

    ) +

    geom_vline(

      xintercept =
        max(
          historical$date,
          na.rm = TRUE
        ),

      linetype =
        "dashed",

      linewidth =
        0.6

    ) +

    labs(

      title =
        paste0(
          measure_name,
          ": 12-Month Forecast"
        ),

      subtitle =
        paste0(
          "Selected model: ",
          best_model_name,
          " | 80% and 95% prediction intervals"
        ),

      x =
        NULL,

      y =
        paste0(
          measure_name,
          " rate (%)"
        ),

      caption =
        "Historical data: ABS. Forecast is model-based and is not an official ABS forecast."

    ) +

    theme_minimal(
      base_size = 12
    ) +

    theme(

      plot.title =
        element_text(
          face = "bold",
          size = 15
        ),

      plot.subtitle =
        element_text(
          size = 10
        ),

      plot.caption =
        element_text(
          size = 8
        ),

      panel.grid.minor =
        element_blank(),

      legend.position =
        "none"

    )


  print(
    forecast_plot
  )


  # ==========================================================
  # 8.19 Forecast summary
  # ==========================================================

  latest_value <- series_data |>

    slice_tail(
      n = 1
    ) |>

    pull(
      value
    )


  first_forecast <- final_forecast |>

    slice_head(
      n = 1
    ) |>

    pull(
      forecast
    )


  last_forecast <- final_forecast |>

    slice_tail(
      n = 1
    ) |>

    pull(
      forecast
    )


  forecast_change <-
    last_forecast -
    latest_value


  forecast_summary <- tibble(

    measure =
      measure_name,

    latest_observed =
      latest_value,

    first_forecast =
      first_forecast,

    final_forecast =
      last_forecast,

    change_over_horizon =
      forecast_change,

    best_model =
      best_model_name

  )


  # ==========================================================
  # 8.20 Return results
  # ==========================================================

  list(

    measure =
      measure_name,

    accuracy =
      accuracy_results,

    best_model =
      best_model_name,

    diagnostics =
      residual_ljung_box,

    forecast =
      final_forecast,

    historical =
      historical,

    summary =
      forecast_summary,

    plot =
      forecast_plot

  )

}


# ============================================================
# 9. Run forecasts
# ============================================================

underutilisation_results <-
  run_forecast_analysis(

    data =
      forecast_data,

    value_column =
      "underutilisation_rate",

    measure_name =
      "Underutilisation",

    horizon =
      forecast_horizon,

    test_size =
      test_size

  )


unemployment_results <-
  run_forecast_analysis(

    data =
      forecast_data,

    value_column =
      "unemployment_rate",

    measure_name =
      "Unemployment",

    horizon =
      forecast_horizon,

    test_size =
      test_size

  )


underemployment_results <-
  run_forecast_analysis(

    data =
      forecast_data,

    value_column =
      "underemployment_rate",

    measure_name =
      "Underemployment",

    horizon =
      forecast_horizon,

    test_size =
      test_size

  )


# ============================================================
# 10. Combine forecast results
# ============================================================

all_forecasts <- bind_rows(

  underutilisation_results$forecast,

  unemployment_results$forecast,

  underemployment_results$forecast

)


# ============================================================
# 11. Combine model accuracy
# ============================================================

all_accuracy <- bind_rows(

  underutilisation_results$accuracy,

  unemployment_results$accuracy,

  underemployment_results$accuracy

)


# ============================================================
# 12. Combine diagnostics
# ============================================================

all_diagnostics <- bind_rows(

  underutilisation_results$diagnostics |>
    mutate(
      measure =
        "Underutilisation"
    ),

  unemployment_results$diagnostics |>
    mutate(
      measure =
        "Unemployment"
    ),

  underemployment_results$diagnostics |>
    mutate(
      measure =
        "Underemployment"
    )

)


# ============================================================
# 13. Forecast summaries
# ============================================================

forecast_summaries <- bind_rows(

  underutilisation_results$summary,

  unemployment_results$summary,

  underemployment_results$summary

)


# ============================================================
# 14. Best model summary
# ============================================================

best_models <- forecast_summaries |>

  select(

    measure,

    best_model

  )


cat("\n")
cat("============================================\n")
cat("BEST FORECASTING MODELS\n")
cat("============================================\n\n")


print(
  best_models
)


# ============================================================
# 15. Save forecast results
# ============================================================

forecast_output_file <- file.path(

  processed_dir,

  "labour_market_forecast.csv"

)


write_csv(

  all_forecasts,

  forecast_output_file

)


# ============================================================
# 16. Save model accuracy
# ============================================================

accuracy_output_file <- file.path(

  processed_dir,

  "labour_market_forecast_accuracy.csv"

)


write_csv(

  all_accuracy,

  accuracy_output_file

)


# ============================================================
# 17. Save diagnostics
# ============================================================

diagnostics_output_file <- file.path(

  processed_dir,

  "labour_market_forecast_diagnostics.csv"

)


write_csv(

  all_diagnostics,

  diagnostics_output_file

)


# ============================================================
# 18. Save forecast summaries
# ============================================================

summary_output_file <- file.path(

  processed_dir,

  "labour_market_forecast_summary.csv"

)


write_csv(

  forecast_summaries,

  summary_output_file

)


# ============================================================
# 19. Save best models
# ============================================================

best_model_output_file <- file.path(

  processed_dir,

  "labour_market_forecast_best_models.csv"

)


write_csv(

  best_models,

  best_model_output_file

)


# ============================================================
# 20. Print output files
# ============================================================

cat("\n")
cat("============================================\n")
cat("FILES CREATED\n")
cat("============================================\n\n")


cat(
  "Forecasts:\n",
  forecast_output_file,
  "\n\n"
)


cat(
  "Model accuracy:\n",
  accuracy_output_file,
  "\n\n"
)


cat(
  "Diagnostics:\n",
  diagnostics_output_file,
  "\n\n"
)


cat(
  "Forecast summary:\n",
  summary_output_file,
  "\n\n"
)


cat(
  "Best models:\n",
  best_model_output_file,
  "\n\n"
)


# ============================================================
# 21. Primary underutilisation results
# ============================================================

cat("\n")
cat("============================================\n")
cat("PRIMARY MEASURE: UNDERUTILISATION\n")
cat("============================================\n\n")


cat(
  "Selected model: ",
  underutilisation_results$best_model,
  "\n\n"
)


cat(
  "Model comparison:\n\n"
)


print(
  underutilisation_results$accuracy
)


cat(
  "\nForecast summary:\n\n"
)


print(
  underutilisation_results$summary
)


cat("\n")
cat("IMPORTANT:\n")
cat(
  "These are exploratory model-based forecasts using\n",
  "historical labour-market patterns.\n",
  "They are NOT official ABS forecasts.\n"
)


cat("\n")
cat(
  "Forecasting analysis complete.\n"
)