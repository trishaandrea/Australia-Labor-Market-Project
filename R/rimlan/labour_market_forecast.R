
# ============================================================
# Labour Market Forecasting
# ETC-5543 Australian Labour Market Project
# Measures:
#   - Unemployment
#   - Underemployment
#   - Underutilisation
#
# Models:
#   - Naive
#   - Seasonal Naive
#   - ETS
#   - ARIMA
#
# Final forecast:
#   - Best-performing model selected using a 12-month test set
#   - Selected model refitted using all available observations
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
  "Required variables found.\n"
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
      as.numeric(unemployment_rate),

    underemployment_rate =
      as.numeric(underemployment_rate),

    underutilisation_rate =
      as.numeric(underutilisation_rate)

  ) |>

  filter(
    !is.na(date)
  ) |>

  arrange(date)


# ------------------------------------------------------------
# 6.1 Remove duplicate dates
# ------------------------------------------------------------

if (anyDuplicated(forecast_data$date) > 0) {

  stop(
    "Duplicate dates detected. Check the source data."
  )

}


# ------------------------------------------------------------
# 6.2 Replace infinite values with NA
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


# ------------------------------------------------------------
# 6.3 Check monthly continuity
# ------------------------------------------------------------

forecast_data <- forecast_data |>

  mutate(
    date = yearmonth(date)
  )


expected_months <- seq(
  min(forecast_data$date),
  max(forecast_data$date),
  by = 1
)


if (!identical(
  as.character(forecast_data$date),
  as.character(expected_months)
)) {

  stop(
    paste0(
      "Missing monthly observations detected.\n",
      "Check the source data before forecasting."
    )
  )

}


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
    as.Date(min(forecast_data$date)),
    "%B %Y"
  ),
  " to ",
  format(
    as.Date(max(forecast_data$date)),
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
      sum(is.na(unemployment_rate)),

    underemployment_missing =
      sum(is.na(underemployment_rate)),

    underutilisation_missing =
      sum(is.na(underutilisation_rate))

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
        all_of(value_column)

    ) |>

    filter(
      !is.na(value)
    ) |>

    arrange(date)


  # Check for missing values

  if (anyNA(series_data$value)) {

    stop(
      paste0(
        "Missing values detected in ",
        measure_name,
        ". Check the source data."
      )
    )

  }


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


  cat(
    "Training period:",
    as.character(min(train_data$date)),
    "to",
    as.character(max(train_data$date)),
    "\n"
  )


  cat(
    "Testing period:",
    as.character(min(test_data$date)),
    "to",
    as.character(max(test_data$date)),
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


  if (length(best_model_name) == 0) {

    stop(
      paste(
        "No model produced a valid RMSE for",
        measure_name
      )
    )

  }


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


  # Determine the number of estimated AR and MA parameters
  # for an ARIMA model.

  model_report <- selected_model |>
    report()


  arima_df <- 0L

  if (best_model_name == "ARIMA") {

    arima_order <- selected_model |>
      tidy()

    arima_df <- sum(
      grepl(
        "^(ar|ma)[0-9]+$",
        arima_order$term
      )
    )

  }


  residual_ljung_box <- residuals |>

    features(

      .innov,

      ljung_box,

      lag = 12,

      dof = arima_df

    ) |>

    mutate(
      measure = measure_name,
      model = best_model_name,
      .before = 1
    )


  cat(
    "\nResidual Ljung-Box test:\n\n"
  )


  print(
    residual_ljung_box
  )


  # Residual ACF plot

  residual_acf_plot <- residuals |>

    ACF(.innov) |>

    autoplot() +

    labs(

      title =
        paste(
          "Residual ACF:",
          measure_name
        ),

      subtitle =
        paste(
          "Selected model:",
          best_model_name
        ),

      x =
        "Lag",

      y =
        "Autocorrelation"

    ) +

    theme_minimal(
      base_size = 12
    )


  print(
    residual_acf_plot
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

  final_forecast <- final_forecast_raw |>

    hilo(
      level = c(80, 95)
    ) |>

    unpack_hilo(
      cols = c(`80%`, `95%`)
    ) |>

    as_tibble() |>

    transmute(

      date = as.Date(date),

      forecast = .mean,

      lo80 = `80%_lower`,

      hi80 = `80%_upper`,

      lo95 = `95%_lower`,

      hi95 = `95%_upper`

    ) |>

    mutate(

      measure =
        measure_name,

      model =
        best_model_name,

      type =
        "Forecast"

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
  # 8.13 Validate forecast intervals
  # ==========================================================

  if (anyNA(
    final_forecast |>
      select(
        forecast,
        lo80,
        hi80,
        lo95,
        hi95
      )
  )) {

    stop(
      paste(
        "Missing forecast values or intervals for",
        measure_name
      )
    )

  }


  invalid_intervals <- final_forecast |>

    filter(

      lo95 > lo80 |

      lo80 > forecast |

      forecast > hi80 |

      hi80 > hi95

    )


  if (nrow(invalid_intervals) > 0) {

    stop(
      paste(
        "Forecast interval validation failed for",
        measure_name
      )
    )

  }


  # ==========================================================
  # 8.14 Historical data
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
  # 8.15 Forecast plot
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

      fill =
        "#E67E22",

      alpha =
        0.12

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

      fill =
        "#E67E22",

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
          value,

        colour =
          "Historical"

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
          forecast,

        colour =
          "Forecast"

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

    scale_colour_manual(

      values = c(

        "Historical" =
          "#17365D",

        "Forecast" =
          "#E67E22"

      )

    ) +

    scale_y_continuous(
      labels = label_number(suffix = "%")
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
        "Rate",

      colour =
        NULL,

      caption =
        paste(
          "Historical data: ABS.",
          "Forecasts are model-based exploratory projections,",
          "not official ABS forecasts."
        )

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
        "bottom"

    )


  print(
    forecast_plot
  )


  # ==========================================================
  # 8.16 Forecast summary
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
  # 8.17 Return results
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

    residual_acf =
      residual_acf_plot,

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

  underutilisation_results$diagnostics,

  unemployment_results$diagnostics,

  underemployment_results$diagnostics

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
# 15. Overall model performance comparison
# ============================================================

overall_accuracy <- all_accuracy |>

  select(

    measure,

    model,

    RMSE,

    MAE,

    MASE

  ) |>

  arrange(

    measure,

    RMSE

  )


cat("\n")
cat("============================================\n")
cat("OVERALL FORECAST ACCURACY COMPARISON\n")
cat("============================================\n\n")


print(
  overall_accuracy
)


# ============================================================
# 16. Save forecast results
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
# 17. Save model accuracy
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
# 18. Save diagnostics
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
# 19. Save forecast summaries
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
# 20. Save best models
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
# 21. Save overall model comparison
# ============================================================

overall_accuracy_file <- file.path(

  processed_dir,

  "labour_market_forecast_model_comparison.csv"

)


write_csv(

  overall_accuracy,

  overall_accuracy_file

)


# ============================================================
# 22. Print output files
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


cat(
  "Overall model comparison:\n",
  overall_accuracy_file,
  "\n\n"
)


# ============================================================
# 23. Primary underutilisation results
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
  "\nResidual diagnostics:\n\n"
)


print(
  underutilisation_results$diagnostics
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
