# ============================================================
# AUSTRALIA LABOUR-MARKET PROJECT
# Shiny Dashboard
#
# Research Question:
# To what extent are changes in job vacancies associated
# with changes in labour-market slack in Australia?
#
# IMPORTANT:
# app.R is inside the /shiny folder.
# Therefore all project data paths use ../data/...
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(scales)


# ============================================================
# 2. PROJECT PATHS
# ============================================================

# Because this app.R is inside:
# Australia-Labor-Market-Project/shiny/
#
# the data folder is one level above:
# ../data/processed/

data_path <- function(file) {
  file.path("..", "data", "processed", file)
}


# ============================================================
# 3. COLOUR PALETTE
# ============================================================

COLORS <- list(

  # Main brand colours
  navy = "#17324D",
  teal = "#176B87",
  orange = "#E76F51",
  gold = "#F4A261",
  purple = "#7B61A8",

  # Slack colours
  unemployment = "#E76F51",
  underemployment = "#F4A261",
  underutilisation = "#176B87",

  # Demand / tightness
  vacancies = "#2A9D8F",
  tightness = "#176B87",

  # Supporting colours
  grey = "#6C757D",
  light_grey = "#EEF3F7",
  dark = "#243447",
  white = "#FFFFFF"
)


# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================

safe_read_csv <- function(path) {

  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    read_csv(
      path,
      show_col_types = FALSE
    ),
    error = function(e) {
      NULL
    }
  )
}


parse_date_safe <- function(x) {

  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  suppressWarnings(as.Date(x))
}


safe_cor <- function(x, y) {

  complete <- complete.cases(x, y)

  if (sum(complete) < 3) {
    return(NA_real_)
  }

  cor(
    x[complete],
    y[complete],
    use = "complete.obs"
  )
}


fmt_number <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) {
    return("—")
  }

  number(
    x,
    accuracy = 10^-digits,
    big.mark = ","
  )
}


fmt_percent <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) {
    return("—")
  }

  paste0(
    number(
      x,
      accuracy = 10^-digits
    ),
    "%"
  )
}


plotly_clean <- function(p) {

  p |>
    config(
      displaylogo = FALSE,
      responsive = TRUE,
      modeBarButtonsToRemove = c(
        "lasso2d",
        "select2d"
      )
    )
}


# ============================================================
# 5. LOAD CORE DATA
# ============================================================

combined_data <- safe_read_csv(
  data_path("combined_tightness_slack.csv")
)

slack_data <- safe_read_csv(
  data_path("labour_market_slack.csv")
)

wpi_data <- safe_read_csv(
  data_path("labour_market_wpi.csv")
)

ivi_anzsco2 <- safe_read_csv(
  data_path("ivi_anzsco2_clean.csv")
)

ivi_anzsco4 <- safe_read_csv(
  data_path("ivi_anzsco4_clean.csv")
)

ivi_skill <- safe_read_csv(
  data_path("ivi_skill_clean.csv")
)


# ============================================================
# 6. PREPARE CORE DATA
# ============================================================

if (!is.null(combined_data)) {

  combined_data <- combined_data |>
    mutate(
      Quarter = parse_date_safe(Quarter)
    ) |>
    arrange(Quarter) |>
    mutate(

      vacancy_change =
        Job_Vacancies - lag(Job_Vacancies),

      vacancy_pct_change =
        (Job_Vacancies / lag(Job_Vacancies) - 1) * 100,

      tightness_change =
        Tightness - lag(Tightness),

      unemployment_change =
        unemployment_rate - lag(unemployment_rate),

      underemployment_change =
        underemployment_rate -
        lag(underemployment_rate),

      underutilisation_change =
        underutilisation_rate -
        lag(underutilisation_rate)
    )
}


# ============================================================
# 7. PREPARE SLACK DATA
# ============================================================

if (!is.null(slack_data)) {

  slack_data <- slack_data |>
    mutate(
      date = parse_date_safe(date)
    ) |>
    arrange(date)
}


# ============================================================
# 8. PREPARE IVI DATA
# ============================================================

if (!is.null(ivi_anzsco2)) {

  if ("Month" %in% names(ivi_anzsco2)) {

    ivi_anzsco2 <- ivi_anzsco2 |>
      mutate(
        Month = parse_date_safe(Month)
      ) |>
      arrange(Month)
  }
}


if (!is.null(ivi_anzsco4)) {

  if ("Month" %in% names(ivi_anzsco4)) {

    ivi_anzsco4 <- ivi_anzsco4 |>
      mutate(
        Month = parse_date_safe(Month)
      ) |>
      arrange(Month)
  }
}


if (!is.null(ivi_skill)) {

  if ("Month" %in% names(ivi_skill)) {

    ivi_skill <- ivi_skill |>
      mutate(
        Month = parse_date_safe(Month)
      ) |>
      arrange(Month)
  }
}


# ============================================================
# 9. NATIONAL IVI
# ============================================================

ivi_australia <- NULL

if (!is.null(ivi_anzsco2)) {

  required_cols <- c(
    "Level",
    "ANZSCO_CODE",
    "State",
    "Vacancies",
    "Month"
  )

  if (all(required_cols %in% names(ivi_anzsco2))) {

    ivi_australia <- ivi_anzsco2 |>
      filter(
        Level == 1,
        ANZSCO_CODE == "0",
        State == "AUST"
      ) |>
      arrange(Month) |>
      mutate(
        monthly_change =
          Vacancies - lag(Vacancies),

        monthly_pct_change =
          (Vacancies / lag(Vacancies) - 1) * 100,

        yoy_change =
          Vacancies - lag(Vacancies, 12),

        yoy_pct_change =
          (Vacancies / lag(Vacancies, 12) - 1) * 100
      )
  }
}


# ============================================================
# 10. OCCUPATION-LEVEL IVI
# ============================================================

ivi_occupation <- NULL

if (!is.null(ivi_anzsco2)) {

  if (
    all(
      c(
        "Level",
        "State",
        "Vacancies",
        "Month"
      ) %in% names(ivi_anzsco2)
    )
  ) {

    ivi_occupation <- ivi_anzsco2 |>
      filter(
        Level == 2,
        State == "AUST"
      ) |>
      arrange(Month)
  }
}


# ============================================================
# 11. PERIOD CLASSIFICATION
# ============================================================

if (!is.null(slack_data)) {

  slack_data <- slack_data |>
    mutate(

      period = case_when(

        year(date) >= 1978 &
          year(date) <= 1989 ~
          "1978–1989",

        year(date) >= 1990 &
          year(date) <= 1994 ~
          "1990–1994",

        year(date) >= 1995 &
          year(date) <= 2007 ~
          "1995–2007",

        year(date) >= 2008 &
          year(date) <= 2019 ~
          "2008–2019",

        year(date) >= 2020 &
          year(date) <= 2021 ~
          "2020–2021",

        year(date) >= 2022 ~
          "2022–present",

        TRUE ~ NA_character_
      )
    )
}


# ============================================================
# 12. MEDIAN CLASSIFICATION
# ============================================================

tightness_median <- NA_real_
underutilisation_median <- NA_real_

if (!is.null(combined_data)) {

  tightness_median <- median(
    combined_data$Tightness,
    na.rm = TRUE
  )

  underutilisation_median <- median(
    combined_data$underutilisation_rate,
    na.rm = TRUE
  )

  combined_data <- combined_data |>
    mutate(

      tightness_group =
        if_else(
          Tightness >= tightness_median,
          "High tightness",
          "Low tightness"
        ),

      slack_group =
        if_else(
          underutilisation_rate >=
            underutilisation_median,
          "High slack",
          "Low slack"
        ),

      quadrant =
        paste(
          tightness_group,
          slack_group,
          sep = " + "
        )
    )
}


# ============================================================
# 13. HISTORICAL SLACK SUMMARY
# ============================================================

period_summary <- NULL

if (!is.null(slack_data)) {

  period_summary <- slack_data |>
    filter(!is.na(period)) |>
    group_by(period) |>
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
}


# ============================================================
# 14. WPI PREPARATION
# ============================================================

wpi_plot_data <- NULL

if (!is.null(wpi_data)) {

  # Identify a date column
  possible_dates <- c(
    "date",
    "Date",
    "Quarter",
    "quarter"
  )

  date_col <- possible_dates[
    possible_dates %in% names(wpi_data)
  ][1]

  if (!is.na(date_col)) {

    wpi_data[[date_col]] <-
      parse_date_safe(wpi_data[[date_col]])

    # Find numeric columns
    numeric_cols <- names(
      wpi_data[
        vapply(
          wpi_data,
          is.numeric,
          logical(1)
        )
      ]
    )

    # Remove columns that are obviously not WPI
    numeric_cols <- numeric_cols[
      !numeric_cols %in% c(
        "year",
        "month",
        "quarter"
      )
    ]

    if (length(numeric_cols) > 0) {

      value_col <- numeric_cols[1]

      wpi_plot_data <- wpi_data |>
        transmute(
          date = .data[[date_col]],
          wpi = .data[[value_col]]
        ) |>
        filter(
          !is.na(date),
          !is.na(wpi)
        )
    }
  }
}


# ============================================================
# 15. USER INTERFACE
# ============================================================

ui <- fluidPage(

  # ----------------------------------------------------------
  # Custom CSS
  # ----------------------------------------------------------

  tags$head(

    tags$style(
      HTML(
        "
        /* ==================================================
           GLOBAL
           ================================================== */

        body {
          background-color: #EEF3F7;
          font-family:
            -apple-system,
            BlinkMacSystemFont,
            'Segoe UI',
            Roboto,
            Arial,
            sans-serif;
          color: #243447;
        }

        .container-fluid {
          padding-left: 28px;
          padding-right: 28px;
        }


        /* ==================================================
           HEADER
           ================================================== */

        .dashboard-header {
          background:
            linear-gradient(
              135deg,
              #17324D 0%,
              #176B87 100%
            );

          color: white;
          padding: 30px 35px;
          margin: -15px -15px 25px -15px;

          border-bottom:
            5px solid #F4A261;

          box-shadow:
            0 4px 12px rgba(0,0,0,0.12);
        }

        .dashboard-title {
          font-size: 30px;
          font-weight: 700;
          margin-bottom: 6px;
        }

        .dashboard-subtitle {
          font-size: 16px;
          opacity: 0.92;
          margin-bottom: 0;
        }


        /* ==================================================
           RESEARCH QUESTION
           ================================================== */

        .research-question {
          background: white;

          border-left:
            6px solid #E76F51;

          border-radius: 10px;

          padding: 22px 25px;
          margin-bottom: 25px;

          box-shadow:
            0 3px 12px rgba(0,0,0,0.07);
        }

        .research-question-title {
          color: #17324D;
          font-weight: 700;
          font-size: 15px;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          margin-bottom: 8px;
        }

        .research-question-text {
          font-size: 20px;
          font-weight: 600;
          color: #243447;
          line-height: 1.45;
        }


        /* ==================================================
           KPI CARDS
           ================================================== */

        .kpi-card {
          background: white;

          border-radius: 12px;

          padding: 20px;

          min-height: 130px;

          box-shadow:
            0 3px 12px rgba(0,0,0,0.07);

          border-top: 4px solid #176B87;

          transition:
            transform 0.2s ease,
            box-shadow 0.2s ease;

          margin-bottom: 20px;
        }

        .kpi-card:hover {
          transform: translateY(-3px);

          box-shadow:
            0 7px 18px rgba(0,0,0,0.11);
        }

        .kpi-card.orange {
          border-top-color: #E76F51;
        }

        .kpi-card.gold {
          border-top-color: #F4A261;
        }

        .kpi-card.purple {
          border-top-color: #7B61A8;
        }

        .kpi-label {
          font-size: 13px;
          color: #6C757D;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          font-weight: 600;
        }

        .kpi-value {
          font-size: 28px;
          font-weight: 750;
          color: #17324D;
          margin-top: 5px;
        }

        .kpi-description {
          font-size: 12px;
          color: #6C757D;
          margin-top: 4px;
        }


        /* ==================================================
           SECTION CARDS
           ================================================== */

        .section-card {
          background: white;

          border-radius: 12px;

          padding: 22px;

          margin-bottom: 25px;

          box-shadow:
            0 3px 12px rgba(0,0,0,0.06);
        }

        .section-title {
          color: #17324D;
          font-size: 20px;
          font-weight: 700;

          border-bottom:
            2px solid #EEF3F7;

          padding-bottom: 10px;

          margin-bottom: 18px;
        }


        /* ==================================================
           INSIGHT BOX
           ================================================== */

        .insight-box {
          background: #F7FAFC;

          border-left:
            5px solid #176B87;

          border-radius: 8px;

          padding: 17px 20px;

          margin: 15px 0;

          line-height: 1.6;
        }

        .insight-box.orange {
          border-left-color: #E76F51;
        }

        .insight-box.gold {
          border-left-color: #F4A261;
        }


        /* ==================================================
           METHODOLOGY
           ================================================== */

        .method-box {
          background: #F7FAFC;

          border: 1px solid #DCE5EC;

          border-radius: 10px;

          padding: 20px;

          margin-bottom: 15px;
        }

        .method-title {
          color: #176B87;
          font-weight: 700;
          margin-bottom: 8px;
        }


        /* ==================================================
           TABS
           ================================================== */

        .nav-tabs {
          border-bottom:
            2px solid #DCE5EC;
          margin-bottom: 25px;
        }

        .nav-tabs > li > a {
          color: #536777;
          font-weight: 600;
          border: none;
          padding: 13px 17px;
        }

        .nav-tabs > li > a:hover {
          background: transparent;
          color: #176B87;
        }

        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          color: #17324D;
          background: transparent;

          border: none;

          border-bottom:
            4px solid #E76F51;
        }


        /* ==================================================
           INPUTS
           ================================================== */

        .form-control {
          border:
            1px solid #CBD5DD;

          border-radius: 7px;
        }

        .form-control:focus {
          border-color: #176B87;

          box-shadow:
            0 0 0 2px rgba(23,107,135,0.12);
        }

        .selectize-input {
          border-radius: 7px;
          border-color: #CBD5DD;
        }


        /* ==================================================
           BUTTONS
           ================================================== */

        .btn-primary {
          background: #176B87;
          border-color: #176B87;
          border-radius: 7px;
        }

        .btn-primary:hover {
          background: #17324D;
          border-color: #17324D;
        }


        /* ==================================================
           DATA TABLE
           ================================================== */

        table.dataTable thead th {
          background: #17324D !important;
          color: white !important;
          border: none !important;
        }

        table.dataTable tbody tr:hover {
          background-color: #EEF7FA !important;
        }


        /* ==================================================
           FOOTER
           ================================================== */

        .dashboard-footer {
          margin-top: 35px;
          padding: 20px;

          text-align: center;

          color: #6C757D;

          border-top:
            1px solid #DCE5EC;
        }

        "
      )
    )
  ),


  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------

  div(
    class = "dashboard-header",

    div(
      class = "dashboard-title",
      "Australia Labour-Market Dashboard"
    ),

    div(
      class = "dashboard-subtitle",
      "Labour-market tightness, vacancies, slack and wage pressure"
    )
  ),


  # ----------------------------------------------------------
  # RESEARCH QUESTION
  # ----------------------------------------------------------

  div(
    class = "research-question",

    div(
      class = "research-question-title",
      "Research Question"
    ),

    div(
      class = "research-question-text",
      "To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?"
    )
  ),


  # ----------------------------------------------------------
  # NAVIGATION
  # ----------------------------------------------------------

  tabsetPanel(

    id = "main_tabs",

    type = "tabs",


    # ========================================================
    # OVERVIEW
    # ========================================================

    tabPanel(
      "Overview",

      br(),

      # KPI ROW 1
      fluidRow(

        column(
          3,

          div(
            class = "kpi-card",

            div(
              class = "kpi-label",
              "Observations"
            ),

            div(
              class = "kpi-value",
              textOutput("kpi_slack_obs")
            ),

            div(
              class = "kpi-description",
              "Monthly labour-force observations"
            )
          )
        ),

        column(
          3,

          div(
            class = "kpi-card orange",

            div(
              class = "kpi-label",
              "Median Tightness"
            ),

            div(
              class = "kpi-value",
              textOutput("kpi_tightness")
            ),

            div(
              class = "kpi-description",
              "Vacancies relative to unemployed persons"
            )
          )
        ),

        column(
          3,

          div(
            class = "kpi-card gold",

            div(
              class = "kpi-label",
              "Median Underutilisation"
            ),

            div(
              class = "kpi-value",
              textOutput("kpi_underutilisation")
            ),

            div(
              class = "kpi-description",
              "Broader measure of labour-market slack"
            )
          )
        ),

        column(
          3,

          div(
            class = "kpi-card purple",

            div(
              class = "kpi-label",
              "Quarterly Observations"
            ),

            div(
              class = "kpi-value",
              textOutput("kpi_quarters")
            ),

            div(
              class = "kpi-description",
              "Vacancy / tightness observations"
            )
          )
        )
      ),


      # SLACK OVERVIEW
      div(
        class = "section-card",

        div(
          class = "section-title",
          "Labour-Market Slack Over Time"
        ),

        plotlyOutput(
          "overview_slack_plot",
          height = "450px"
        )
      ),


      # TIGHTNESS RELATIONSHIP
      fluidRow(

        column(
          7,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "Tightness and Underutilisation"
            ),

            plotlyOutput(
              "overview_relationship_plot",
              height = "400px"
            )
          )
        ),

        column(
          5,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "What the Dashboard Examines"
            ),

            div(
              class = "insight-box",

              strong("Labour demand"),

              br(),

              "ABS Job Vacancies and the Internet Vacancy Index provide complementary measures of recruitment activity."
            ),

            div(
              class = "insight-box",

              strong("Labour-market slack"),

              br(),

              "Unemployment, underemployment and underutilisation are retained as separate measures rather than combined into an additional researcher-weighted index."
            ),

            div(
              class = "insight-box orange",

              strong("Core relationship"),

              br(),

              "The main analysis examines whether changes in vacancies are associated with changes in labour-market slack."
            )
          )
        )
      )
    ),


    # ========================================================
    # LABOUR DEMAND
    # ========================================================

    tabPanel(
      "Labour Demand",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "ABS Job Vacancies"
        ),

        plotlyOutput(
          "vacancy_plot",
          height = "450px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Internet Vacancy Index"
        ),

        plotlyOutput(
          "ivi_plot",
          height = "450px"
        )
      ),


      fluidRow(

        column(
          6,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "Monthly Change in Online Vacancies"
            ),

            plotlyOutput(
              "ivi_change_plot",
              height = "380px"
            )
          )
        ),

        column(
          6,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "Year-on-Year Change in Online Vacancies"
            ),

            plotlyOutput(
              "ivi_yoy_plot",
              height = "380px"
            )
          )
        )
      )
    ),


    # ========================================================
    # SLACK
    # ========================================================

    tabPanel(
      "Labour-Market Slack",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Explore Labour-Market Slack"
        ),

        fluidRow(

          column(
            4,

            selectInput(
              "slack_measure",
              "Measure:",
              choices = c(
                "Unemployment" =
                  "unemployment_rate",

                "Underemployment" =
                  "underemployment_rate",

                "Underutilisation" =
                  "underutilisation_rate"
              ),

              selected =
                "underutilisation_rate"
            )
          ),

          column(
            8,

            uiOutput(
              "slack_date_ui"
            )
          )
        ),

        plotlyOutput(
          "slack_plot",
          height = "500px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Historical Period Comparison"
        ),

        plotlyOutput(
          "period_slack_plot",
          height = "420px"
        ),

        br(),

        DTOutput(
          "period_summary_table"
        )
      )
    ),


    # ========================================================
    # TIGHTNESS
    # ========================================================

    tabPanel(
      "Labour-Market Tightness",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Labour-Market Tightness Over Time"
        ),

        div(
          class = "insight-box",

          HTML(
            paste0(
              "<strong>Definition:</strong> Labour-market tightness is measured as ",
              "seasonally adjusted job vacancies divided by seasonally adjusted ",
              "unemployed persons. A higher ratio indicates more vacancies relative ",
              "to unemployed persons."
            )
          )
        ),

        plotlyOutput(
          "tightness_plot",
          height = "450px"
        )
      ),


      fluidRow(

        column(
          6,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "Distribution of Tightness"
            ),

            plotlyOutput(
              "tightness_histogram",
              height = "380px"
            )
          )
        ),

        column(
          6,

          div(
            class = "section-card",

            div(
              class = "section-title",
              "Tightness and Underutilisation"
            ),

            plotlyOutput(
              "tightness_slack_scatter",
              height = "380px"
            )
          )
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Tightness / Slack Quadrants"
        ),

        plotlyOutput(
          "quadrant_plot",
          height = "430px"
        )
      )
    ),


    # ========================================================
    # RESEARCH QUESTION
    # ========================================================

    tabPanel(
      "Research Question",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Changes in Vacancies vs Changes in Slack"
        ),

        selectInput(
          "rq_measure",
          "Slack measure:",
          choices = c(
            "Unemployment" =
              "unemployment_change",

            "Underemployment" =
              "underemployment_change",

            "Underutilisation" =
              "underutilisation_change"
          ),

          selected =
            "underutilisation_change"
        ),

        plotlyOutput(
          "rq_change_plot",
          height = "480px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Relationship Across Slack Measures"
        ),

        plotlyOutput(
          "all_relationships_plot",
          height = "500px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Correlation Results"
        ),

        DTOutput(
          "correlation_table"
        ),

        br(),

        div(
          class = "insight-box",

          HTML(
            paste0(
              "<strong>Interpretation:</strong> Correlation measures the strength ",
              "and direction of linear association. It does not establish causation."
            )
          )
        )
      )
    ),


    # ========================================================
    # WAGE PRESSURE
    # ========================================================

    tabPanel(
      "Wage Pressure",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Wage Price Index"
        ),

        plotlyOutput(
          "wpi_plot",
          height = "450px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Timing Relationships"
        ),

        DTOutput(
          "wpi_correlation_table"
        ),

        br(),

        div(
          class = "insight-box gold",

          HTML(
            paste0(
              "<strong>Purpose:</strong> The WPI extension examines whether ",
              "wage growth is associated with labour-market tightness and slack ",
              "at the same quarter and subsequent quarters."
            )
          )
        )
      )
    ),


    # ========================================================
    # OCCUPATION DEMAND
    # ========================================================

    tabPanel(
      "Occupation Demand",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Internet Vacancy Index by Occupation"
        ),

        uiOutput(
          "occupation_controls"
        ),

        plotlyOutput(
          "occupation_plot",
          height = "500px"
        )
      ),


      div(
        class = "section-card",

        div(
          class = "section-title",
          "Skill-Level Vacancy Data"
        ),

        plotlyOutput(
          "skill_plot",
          height = "450px"
        )
      )
    ),


    # ========================================================
    # DATA EXPLORER
    # ========================================================

    tabPanel(
      "Data Explorer",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Select Dataset"
        ),

        selectInput(
          "dataset_choice",
          "Dataset:",
          choices = c(
            "Vacancy / Tightness / Slack" =
              "combined",

            "Labour-Market Slack" =
              "slack",

            "National IVI" =
              "ivi",

            "Occupation IVI" =
              "occupation",

            "Skill IVI" =
              "skill",

            "WPI" =
              "wpi"
          )
        ),

        DTOutput(
          "data_table"
        )
      )
    ),


    # ========================================================
    # METHODOLOGY
    # ========================================================

    tabPanel(
      "Methodology",

      br(),

      div(
        class = "section-card",

        div(
          class = "section-title",
          "Project Methodology"
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "1. Labour-Market Slack"
          ),

          p(
            "Labour-market slack is represented using three related but distinct measures: unemployment, underemployment and underutilisation."
          ),

          p(
            "Underutilisation is used as the broadest observable proxy for unused labour capacity, while unemployment and underemployment are retained separately to preserve their different information."
          )
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "2. Labour Demand"
          ),

          p(
            "Labour demand is examined using ABS Job Vacancies and the Jobs and Skills Australia Internet Vacancy Index."
          ),

          p(
            "The IVI measures newly lodged online job advertisements and therefore represents a flow of online recruitment activity. ABS Job Vacancies measures vacancies available for immediate filling at the survey reference date."
          )
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "3. Labour-Market Tightness"
          ),

          p(
            "Tightness is calculated as seasonally adjusted job vacancies divided by seasonally adjusted unemployed persons."
          ),

          p(
            "The measure provides an indicator of the balance between labour demand and the pool of unemployed workers."
          )
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "4. Research Question Analysis"
          ),

          p(
            "The central analysis examines whether changes in job vacancies are associated with changes in unemployment, underemployment and underutilisation."
          ),

          p(
            "The analysis uses correlations and visual comparisons. These results describe associations rather than causal effects."
          )
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "5. Frequency and Matching"
          ),

          p(
            "Labour Force data are monthly, while the ABS Job Vacancies Survey is quarterly. The tightness analysis therefore uses the Labour Force observations corresponding to the vacancy reference months."
          )
        ),


        div(
          class = "method-box",

          div(
            class = "method-title",
            "6. Important Limitations"
          ),

          tags$ul(

            tags$li(
              "Aggregate data do not identify individual worker-level matching."
            ),

            tags$li(
              "Vacancies and unemployed workers may differ in occupation, skills and location."
            ),

            tags$li(
              "The IVI does not capture every vacancy or recruitment channel."
            ),

            tags$li(
              "Underutilisation is an observable proxy for slack rather than a complete measure of all unused labour capacity."
            ),

            tags$li(
              "Correlation does not establish causation."
            ),

            tags$li(
              "Different datasets have different frequencies, definitions and coverage."
            )
          )
        )
      )
    )
  ),


  # ----------------------------------------------------------
  # FOOTER
  # ----------------------------------------------------------

  div(
    class = "dashboard-footer",

    "Australia Labour-Market Project | Monash University | Labour-Market Tightness and Slack in Australia"
  )
)


# ============================================================
# 16. SERVER
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # KPI OUTPUTS
  # ==========================================================

  output$kpi_slack_obs <- renderText({

    if (is.null(slack_data)) {
      return("—")
    }

    format(
      nrow(slack_data),
      big.mark = ","
    )
  })


  output$kpi_tightness <- renderText({

    if (is.na(tightness_median)) {
      return("—")
    }

    number(
      tightness_median,
      accuracy = 0.001
    )
  })


  output$kpi_underutilisation <- renderText({

    if (is.na(underutilisation_median)) {
      return("—")
    }

    paste0(
      number(
        underutilisation_median,
        accuracy = 0.1
      ),
      "%"
    )
  })


  output$kpi_quarters <- renderText({

    if (is.null(combined_data)) {
      return("—")
    }

    format(
      nrow(combined_data),
      big.mark = ","
    )
  })


  # ==========================================================
  # OVERVIEW SLACK
  # ==========================================================

  output$overview_slack_plot <- renderPlotly({

    validate(
      need(
        !is.null(slack_data),
        "Labour-market slack data could not be loaded."
      )
    )

    p <- ggplot(
      slack_data,
      aes(x = date)
    ) +

      geom_line(
        aes(
          y = unemployment_rate,
          colour = "Unemployment"
        ),
        linewidth = 0.8
      ) +

      geom_line(
        aes(
          y = underemployment_rate,
          colour = "Underemployment"
        ),
        linewidth = 0.8
      ) +

      geom_line(
        aes(
          y = underutilisation_rate,
          colour = "Underutilisation"
        ),
        linewidth = 1
      ) +

      scale_colour_manual(
        values = c(
          "Unemployment" =
            COLORS$unemployment,

          "Underemployment" =
            COLORS$underemployment,

          "Underutilisation" =
            COLORS$underutilisation
        )
      ) +

      labs(
        x = NULL,
        y = "Rate (%)",
        colour = NULL,
        title = "Australian Labour-Market Slack",
        subtitle =
          "Unemployment, underemployment and underutilisation"
      ) +

      theme_minimal(base_size = 13) +

      theme(
        legend.position = "top",
        plot.title =
          element_text(
            face = "bold",
            colour = COLORS$navy
          )
      )

    plotly_clean(p)
  })


  # ==========================================================
  # OVERVIEW RELATIONSHIP
  # ==========================================================

  output$overview_relationship_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Combined vacancy and slack data could not be loaded."
      )
    )

    p <- ggplot(
      combined_data,
      aes(
        x = Tightness,
        y = underutilisation_rate
      )
    ) +

      geom_point(
        alpha = 0.7,
        size = 3,
        colour = COLORS$teal
      ) +

      geom_smooth(
        method = "lm",
        se = TRUE,
        colour = COLORS$orange,
        linewidth = 1
      ) +

      labs(
        x = "Labour-market tightness",
        y = "Underutilisation (%)",
        title =
          "Tightness and Labour-Market Slack",
        subtitle =
          "Quarterly observations"
      ) +

      theme_minimal(base_size = 12)

    plotly_clean(p)
  })


  # ==========================================================
  # ABS JOB VACANCIES
  # ==========================================================

  output$vacancy_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Vacancy data could not be loaded."
      )
    )

    p <- ggplot(
      combined_data,
      aes(
        x = Quarter,
        y = Job_Vacancies
      )
    ) +

      geom_line(
        colour = COLORS$vacancies,
        linewidth = 1
      ) +

      labs(
        x = NULL,
        y = "Job vacancies",
        title =
          "ABS Job Vacancies in Australia",
        subtitle =
          "Seasonally adjusted quarterly series"
      ) +

      scale_y_continuous(
        labels = comma
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # NATIONAL IVI
  # ==========================================================

  output$ivi_plot <- renderPlotly({

    validate(
      need(
        !is.null(ivi_australia),
        "National IVI data could not be loaded."
      )
    )

    p <- ggplot(
      ivi_australia,
      aes(
        x = Month,
        y = Vacancies
      )
    ) +

      geom_line(
        colour = COLORS$purple,
        linewidth = 1
      ) +

      labs(
        x = NULL,
        y = "Online job advertisements",
        title =
          "Internet Vacancy Index in Australia",
        subtitle =
          "Seasonally adjusted national series"
      ) +

      scale_y_continuous(
        labels = comma
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # IVI MONTHLY CHANGE
  # ==========================================================

  output$ivi_change_plot <- renderPlotly({

    validate(
      need(
        !is.null(ivi_australia),
        "National IVI data could not be loaded."
      )
    )

    p <- ggplot(
      ivi_australia,
      aes(
        x = Month,
        y = monthly_pct_change
      )
    ) +

      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      geom_line(
        colour = COLORS$orange,
        linewidth = 0.8
      ) +

      labs(
        x = NULL,
        y = "Monthly change (%)",
        title =
          "Monthly Change in Online Vacancies"
      ) +

      theme_minimal(base_size = 12)

    plotly_clean(p)
  })


  # ==========================================================
  # IVI YEAR-ON-YEAR
  # ==========================================================

  output$ivi_yoy_plot <- renderPlotly({

    validate(
      need(
        !is.null(ivi_australia),
        "National IVI data could not be loaded."
      )
    )

    p <- ggplot(
      ivi_australia,
      aes(
        x = Month,
        y = yoy_pct_change
      )
    ) +

      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      geom_line(
        colour = COLORS$teal,
        linewidth = 0.8
      ) +

      labs(
        x = NULL,
        y = "Year-on-year change (%)",
        title =
          "Year-on-Year Change in Online Vacancies"
      ) +

      theme_minimal(base_size = 12)

    plotly_clean(p)
  })


  # ==========================================================
  # SLACK DATE UI
  # ==========================================================

  output$slack_date_ui <- renderUI({

    validate(
      need(
        !is.null(slack_data),
        "Slack data unavailable."
      )
    )

    dateRangeInput(
      "slack_date_range",
      "Date range:",
      start = min(
        slack_data$date,
        na.rm = TRUE
      ),
      end = max(
        slack_data$date,
        na.rm = TRUE
      ),

      min = min(
        slack_data$date,
        na.rm = TRUE
      ),

      max = max(
        slack_data$date,
        na.rm = TRUE
      )
    )
  })


  # ==========================================================
  # SLACK PLOT
  # ==========================================================

  output$slack_plot <- renderPlotly({

    validate(
      need(
        !is.null(slack_data),
        "Slack data unavailable."
      ),

      need(
        !is.null(input$slack_date_range),
        "Select a date range."
      )
    )

    selected_data <- slack_data |>
      filter(
        date >= input$slack_date_range[1],
        date <= input$slack_date_range[2]
      )

    measure <- input$slack_measure

    label <- c(
      unemployment_rate =
        "Unemployment",

      underemployment_rate =
        "Underemployment",

      underutilisation_rate =
        "Underutilisation"
    )[[measure]]

    colour <- c(
      unemployment_rate =
        COLORS$unemployment,

      underemployment_rate =
        COLORS$underemployment,

      underutilisation_rate =
        COLORS$underutilisation
    )[[measure]]

    p <- ggplot(
      selected_data,
      aes(
        x = date,
        y = .data[[measure]]
      )
    ) +

      geom_line(
        colour = colour,
        linewidth = 1
      ) +

      labs(
        x = NULL,
        y = "Rate (%)",
        title =
          paste(
            label,
            "over time"
          )
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # PERIOD SLACK PLOT
  # ==========================================================

  output$period_slack_plot <- renderPlotly({

    validate(
      need(
        !is.null(period_summary),
        "Period summary unavailable."
      )
    )

    plot_data <- period_summary |>
      pivot_longer(
        cols = c(
          unemployment,
          underemployment,
          underutilisation
        ),
        names_to = "measure",
        values_to = "rate"
      ) |>
      mutate(

        measure = recode(
          measure,

          unemployment =
            "Unemployment",

          underemployment =
            "Underemployment",

          underutilisation =
            "Underutilisation"
        )
      )

    p <- ggplot(
      plot_data,
      aes(
        x = period,
        y = rate,
        fill = measure
      )
    ) +

      geom_col(
        position = "dodge"
      ) +

      scale_fill_manual(
        values = c(
          "Unemployment" =
            COLORS$unemployment,

          "Underemployment" =
            COLORS$underemployment,

          "Underutilisation" =
            COLORS$underutilisation
        )
      ) +

      labs(
        x = NULL,
        y = "Average rate (%)",
        fill = NULL,
        title =
          "Average Labour-Market Slack by Historical Period"
      ) +

      theme_minimal(base_size = 12) +

      theme(
        legend.position = "top",
        axis.text.x =
          element_text(
            angle = 30,
            hjust = 1
          )
      )

    plotly_clean(p)
  })


  # ==========================================================
  # PERIOD TABLE
  # ==========================================================

  output$period_summary_table <- renderDT({

    validate(
      need(
        !is.null(period_summary),
        "Period summary unavailable."
      )
    )

    period_summary |>
      rename(
        Period = period,
        Unemployment = unemployment,
        Underemployment = underemployment,
        Underutilisation = underutilisation
      ) |>
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      ) |>
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })


  # ==========================================================
  # TIGHTNESS PLOT
  # ==========================================================

  output$tightness_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Tightness data unavailable."
      )
    )

    p <- ggplot(
      combined_data,
      aes(
        x = Quarter,
        y = Tightness
      )
    ) +

      geom_line(
        colour = COLORS$tightness,
        linewidth = 1
      ) +

      geom_hline(
        yintercept = tightness_median,
        linetype = "dashed",
        colour = COLORS$orange
      ) +

      labs(
        x = NULL,
        y = "Vacancies / unemployed persons",
        title =
          "Labour-Market Tightness",
        subtitle =
          "Dashed line indicates the sample median"
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # TIGHTNESS HISTOGRAM
  # ==========================================================

  output$tightness_histogram <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Tightness data unavailable."
      )
    )

    p <- ggplot(
      combined_data,
      aes(x = Tightness)
    ) +

      geom_histogram(
        bins = 18,
        fill = COLORS$teal,
        colour = "white"
      ) +

      geom_vline(
        xintercept = tightness_median,
        linetype = "dashed",
        colour = COLORS$orange,
        linewidth = 1
      ) +

      labs(
        x = "Tightness",
        y = "Number of observations",
        title =
          "Distribution of Labour-Market Tightness"
      ) +

      theme_minimal(base_size = 12)

    plotly_clean(p)
  })


  # ==========================================================
  # TIGHTNESS / SLACK SCATTER
  # ==========================================================

  output$tightness_slack_scatter <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Combined data unavailable."
      )
    )

    p <- ggplot(
      combined_data,
      aes(
        x = Tightness,
        y = underutilisation_rate
      )
    ) +

      geom_point(
        colour = COLORS$teal,
        alpha = 0.75,
        size = 3
      ) +

      geom_smooth(
        method = "lm",
        se = FALSE,
        colour = COLORS$orange
      ) +

      labs(
        x = "Labour-market tightness",
        y = "Underutilisation (%)",
        title =
          "Tightness vs Underutilisation"
      ) +

      theme_minimal(base_size = 12)

    plotly_clean(p)
  })


  # ==========================================================
  # QUADRANT PLOT
  # ==========================================================

  output$quadrant_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Combined data unavailable."
      )
    )

    p <- ggplot(
      combined_data,
      aes(
        x = Tightness,
        y = underutilisation_rate,
        colour = quadrant,
        text =
          paste0(
            "Quarter: ",
            format(Quarter, "%b %Y"),
            "<br>Tightness: ",
            round(Tightness, 3),
            "<br>Underutilisation: ",
            round(
              underutilisation_rate,
              2
            ),
            "%"
          )
      )
    ) +

      geom_point(
        size = 3,
        alpha = 0.8
      ) +

      geom_vline(
        xintercept = tightness_median,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      geom_hline(
        yintercept = underutilisation_median,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      labs(
        x = "Labour-market tightness",
        y = "Underutilisation (%)",
        colour = "Quadrant",
        title =
          "High / Low Tightness and Slack"
      ) +

      theme_minimal(base_size = 12) +

      theme(
        legend.position = "top"
      )

    ggplotly(
      p,
      tooltip = "text"
    ) |>
      config(
        displaylogo = FALSE,
        responsive = TRUE
      )
  })


  # ==========================================================
  # RESEARCH QUESTION CHANGE PLOT
  # ==========================================================

  output$rq_change_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Combined data unavailable."
      )
    )

    measure <- input$rq_measure

    label <- c(

      unemployment_change =
        "Change in unemployment",

      underemployment_change =
        "Change in underemployment",

      underutilisation_change =
        "Change in underutilisation"

    )[[measure]]

    plot_data <- combined_data |>
      filter(
        !is.na(vacancy_change),
        !is.na(.data[[measure]])
      )

    correlation <- safe_cor(
      plot_data$vacancy_change,
      plot_data[[measure]]
    )

    p <- ggplot(
      plot_data,
      aes(
        x = vacancy_change,
        y = .data[[measure]]
      )
    ) +

      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        colour = COLORS$grey
      ) +

      geom_point(
        colour = COLORS$orange,
        alpha = 0.8,
        size = 3
      ) +

      geom_smooth(
        method = "lm",
        se = TRUE,
        colour = COLORS$teal,
        linewidth = 1
      ) +

      labs(
        x = "Change in job vacancies",
        y = label,
        title =
          "Changes in Job Vacancies and Labour-Market Slack",
        subtitle =
          paste(
            "Correlation:",
            round(
              correlation,
              3
            )
          )
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # ALL RELATIONSHIPS
  # ==========================================================

  output$all_relationships_plot <- renderPlotly({

    validate(
      need(
        !is.null(combined_data),
        "Combined data unavailable."
      )
    )

    plot_data <- combined_data |>
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

    p <- ggplot(
      plot_data,
      aes(
        x = Tightness,
        y = rate,
        colour = measure
      )
    ) +

      geom_point(
        alpha = 0.65
      ) +

      geom_smooth(
        method = "lm",
        se = FALSE
      ) +

      scale_colour_manual(
        values = c(

          "Unemployment" =
            COLORS$unemployment,

          "Underemployment" =
            COLORS$underemployment,

          "Underutilisation" =
            COLORS$underutilisation
        )
      ) +

      labs(
        x = "Labour-market tightness",
        y = "Slack measure (%)",
        colour = NULL,
        title =
          "Tightness and Alternative Measures of Slack"
      ) +

      theme_minimal(base_size = 12) +

      theme(
        legend.position = "top"
      )

    plotly_clean(p)
  })


  # ==========================================================
  # CORRELATION TABLE
  # ==========================================================

  output$correlation_table <- renderDT({

    validate(
      need(
        !is.null(combined_data),
        "Combined data unavailable."
      )
    )

    tibble(

      Relationship = c(
        "Tightness vs unemployment",
        "Tightness vs underemployment",
        "Tightness vs underutilisation",
        "Vacancy change vs unemployment change",
        "Vacancy change vs underemployment change",
        "Vacancy change vs underutilisation change"
      ),

      Correlation = c(

        safe_cor(
          combined_data$Tightness,
          combined_data$unemployment_rate
        ),

        safe_cor(
          combined_data$Tightness,
          combined_data$underemployment_rate
        ),

        safe_cor(
          combined_data$Tightness,
          combined_data$underutilisation_rate
        ),

        safe_cor(
          combined_data$vacancy_change,
          combined_data$unemployment_change
        ),

        safe_cor(
          combined_data$vacancy_change,
          combined_data$underemployment_change
        ),

        safe_cor(
          combined_data$vacancy_change,
          combined_data$underutilisation_change
        )
      )
    ) |>

      mutate(
        Correlation =
          round(
            Correlation,
            3
          )
      ) |>

      datatable(
        options = list(
          pageLength = 10,
          searching = FALSE,
          lengthChange = FALSE
        ),
        rownames = FALSE
      )
  })


  # ==========================================================
  # WPI PLOT
  # ==========================================================

  output$wpi_plot <- renderPlotly({

    validate(
      need(
        !is.null(wpi_plot_data),
        "WPI data could not be loaded."
      )
    )

    p <- ggplot(
      wpi_plot_data,
      aes(
        x = date,
        y = wpi
      )
    ) +

      geom_line(
        colour = COLORS$purple,
        linewidth = 1
      ) +

      labs(
        x = NULL,
        y = "Wage Price Index",
        title =
          "Australian Wage Price Index",
        subtitle =
          "Seasonally adjusted series"
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # WPI CORRELATION TABLE
  # ==========================================================

  output$wpi_correlation_table <- renderDT({

    # These are the project's previously calculated
    # timing correlations.

    tibble(

      Relationship =
        c(
          "WPI growth vs Tightness — same quarter",
          "WPI growth vs Tightness — 1 quarter ahead",
          "WPI growth vs Tightness — 2 quarters ahead",
          "WPI growth vs Underutilisation — same quarter",
          "WPI growth vs Underutilisation — 1 quarter ahead",
          "WPI growth vs Underutilisation — 2 quarters ahead"
        ),

      Correlation =
        c(
          0.403,
          0.422,
          0.438,
          -0.774,
          -0.751,
          -0.649
        )
    ) |>

      datatable(
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          pageLength = 10
        ),
        rownames = FALSE
      ) |>

      formatRound(
        "Correlation",
        3
      )
  })


  # ==========================================================
  # OCCUPATION CONTROLS
  # ==========================================================

  output$occupation_controls <- renderUI({

    validate(
      need(
        !is.null(ivi_occupation),
        "Occupation IVI data unavailable."
      )
    )

    if (!"ANZSCO_CODE" %in% names(ivi_occupation)) {

      return(
        helpText(
          "Occupation code information is not available."
        )
      )
    }

    choices <- sort(
      unique(
        ivi_occupation$ANZSCO_CODE
      )
    )

    selectInput(
      "occupation_code",
      "Occupation:",
      choices = choices,
      selected = choices[1]
    )
  })


  # ==========================================================
  # OCCUPATION PLOT
  # ==========================================================

  output$occupation_plot <- renderPlotly({

    validate(
      need(
        !is.null(ivi_occupation),
        "Occupation IVI data unavailable."
      ),

      need(
        !is.null(input$occupation_code),
        "Select an occupation."
      )
    )

    plot_data <- ivi_occupation |>
      filter(
        ANZSCO_CODE ==
          input$occupation_code
      )

    validate(
      need(
        nrow(plot_data) > 0,
        "No observations available for this occupation."
      )
    )

    p <- ggplot(
      plot_data,
      aes(
        x = Month,
        y = Vacancies
      )
    ) +

      geom_line(
        colour = COLORS$teal,
        linewidth = 0.9
      ) +

      labs(
        x = NULL,
        y = "Online vacancies",
        title =
          paste(
            "Internet Vacancy Index:",
            input$occupation_code
          )
      ) +

      scale_y_continuous(
        labels = comma
      ) +

      theme_minimal(base_size = 13)

    plotly_clean(p)
  })


  # ==========================================================
  # SKILL PLOT
  # ==========================================================

  output$skill_plot <- renderPlotly({

    validate(
      need(
        !is.null(ivi_skill),
        "Skill-level IVI data unavailable."
      )
    )

    required <- c(
      "Month",
      "Vacancies"
    )

    validate(
      need(
        all(
          required %in%
            names(ivi_skill)
        ),
        "The skill-level IVI file does not contain the expected columns."
      )
    )

    # Try to identify a skill column
    possible_skill_cols <- c(
      "Skill_Level",
      "Skill",
      "skill_level",
      "skill"
    )

    skill_col <- possible_skill_cols[
      possible_skill_cols %in%
        names(ivi_skill)
    ][1]

    if (is.na(skill_col)) {

      p <- ggplot(
        ivi_skill,
        aes(
          x = Month,
          y = Vacancies
        )
      ) +

        geom_line(
          colour = COLORS$purple,
          linewidth = 0.8
        ) +

        labs(
          x = NULL,
          y = "Online vacancies",
          title =
            "Internet Vacancy Index by Skill Data"
        ) +

        theme_minimal(base_size = 12)

    } else {

      p <- ggplot(
        ivi_skill,
        aes(
          x = Month,
          y = Vacancies,
          colour = as.factor(
            .data[[skill_col]]
          )
        )
      ) +

        geom_line(
          linewidth = 0.8
        ) +

        labs(
          x = NULL,
          y = "Online vacancies",
          colour = "Skill level",
          title =
            "Internet Vacancy Index by Skill Level"
        ) +

        theme_minimal(base_size = 12) +

        theme(
          legend.position = "top"
        )
    }

    plotly_clean(p)
  })


  # ==========================================================
  # DATA EXPLORER
  # ==========================================================

  output$data_table <- renderDT({

    selected <- input$dataset_choice

    data_to_show <- switch(

      selected,

      combined =
        combined_data,

      slack =
        slack_data,

      ivi =
        ivi_australia,

      occupation =
        ivi_occupation,

      skill =
        ivi_skill,

      wpi =
        wpi_data,

      NULL
    )

    validate(
      need(
        !is.null(data_to_show),
        "This dataset is not available."
      )
    )

    datatable(
      data_to_show,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
}


# ============================================================
# 17. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)