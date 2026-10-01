# ============================================================
# AUSTRALIAN LABOUR MARKET DASHBOARD
# ETC-5543
#
# Research Question:
# To what extent are changes in job vacancies associated
# with changes in labour-market slack in Australia?
#
# Author: Rimlan
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)


# ============================================================
# 2. COLOUR PALETTE
# ============================================================

dashboard_palette <- c(
  "Unemployment" = "#2774C6",
  "Underemployment" = "#E67E22",
  "Underutilisation" = "#087F8C",
  "Youth" = "#7657A8",
  "Overall" = "#12355B",
  "Vacancies" = "#2774C6",
  "Tightness" = "#087F8C",
  "WPI Growth" = "#E67E22"
)

occupation_palette <- c(
  "#12355B",
  "#2774C6",
  "#087F8C",
  "#19947A",
  "#E67E22",
  "#D95F02",
  "#7657A8",
  "#8C6BB1",
  "#5C7AEA",
  "#4E9F8A"
)


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Find files regardless of whether app is run from project
# root or from shiny/
# ------------------------------------------------------------

find_project_file <- function(filename) {

  possible_paths <- c(
    file.path("..", "data", "processed", filename),
    file.path("data", "processed", filename),
    file.path("..", "..", "data", "processed", filename)
  )

  existing_paths <- possible_paths[file.exists(possible_paths)]

  if (length(existing_paths) == 0) {
    return(NA_character_)
  }

  existing_paths[1]
}


# ------------------------------------------------------------
# Safely read CSV
# ------------------------------------------------------------

safe_read_csv <- function(filename) {

  file_path <- find_project_file(filename)

  if (is.na(file_path)) {
    message("File not found: ", filename)
    return(tibble())
  }

  tryCatch(
    {
      read_csv(
        file_path,
        show_col_types = FALSE
      )
    },
    error = function(e) {
      message(
        "Could not read ",
        filename,
        ": ",
        e$message
      )
      tibble()
    }
  )
}


# ------------------------------------------------------------
# Safely parse dates
# ------------------------------------------------------------

safe_date <- function(x) {

  if (inherits(x, "Date")) {
    return(x)
  }

  x <- as.character(x)

  result <- suppressWarnings(
    parse_date_time(
      x,
      orders = c(
        "Y-m-d",
        "d/m/Y",
        "m/d/Y",
        "Y/m/d",
        "Y-m",
        "m-Y",
        "b Y",
        "B Y",
        "Y"
      )
    )
  )

  as.Date(result)
}


# ------------------------------------------------------------
# Safe numeric conversion
# ------------------------------------------------------------

safe_numeric <- function(x) {

  suppressWarnings(
    as.numeric(
      gsub(
        ",",
        "",
        as.character(x)
      )
    )
  )
}


# ------------------------------------------------------------
# Safe correlation
# ------------------------------------------------------------

safe_cor <- function(x, y) {

  x <- safe_numeric(x)
  y <- safe_numeric(y)

  valid <- complete.cases(x, y)

  if (sum(valid) < 3) {
    return(NA_real_)
  }

  suppressWarnings(
    cor(
      x[valid],
      y[valid]
    )
  )
}


# ------------------------------------------------------------
# Number formatting
# ------------------------------------------------------------

format_number <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) {
    return("—")
  }

  scales::number(
    x,
    accuracy = 10^(-digits),
    big.mark = ","
  )
}


# ------------------------------------------------------------
# Percentage formatting
# ------------------------------------------------------------

format_percent <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) {
    return("—")
  }

  paste0(
    scales::number(
      x,
      accuracy = 10^(-digits)
    ),
    "%"
  )
}


# ------------------------------------------------------------
# DT options
# ------------------------------------------------------------

datatable_options <- function(default = 10) {

  list(

    pageLength = default,

    lengthMenu = list(
      c(
        10,
        25,
        50,
        100,
        250,
        500,
        -1
      ),
      c(
        "10",
        "25",
        "50",
        "100",
        "250",
        "500",
        "All"
      )
    ),

    lengthChange = TRUE,
    searching = TRUE,
    ordering = TRUE,
    paging = TRUE,
    info = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE,

    language = list(
      lengthMenu = "Show _MENU_ entries",
      search = "Search:",
      info = "Showing _START_ to _END_ of _TOTAL_ entries",
      infoEmpty = "Showing 0 to 0 of 0 entries",
      infoFiltered = "(filtered from _MAX_ total entries)"
    )
  )
}


# ------------------------------------------------------------
# Empty Plotly chart
# ------------------------------------------------------------

empty_plot <- function(message = "No data available") {

  plot_ly() |> 
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(
            size = 16,
            color = "#718096"
          )
        )
      )
    )
}


# ------------------------------------------------------------
# Clean Plotly
# ------------------------------------------------------------

clean_plotly <- function(plot) {

  plot |> 

    layout(
      font = list(
        family = "Arial",
        size = 13,
        color = "#263648"
      ),

      paper_bgcolor = "white",
      plot_bgcolor = "white",

      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "#D9E2EC",
        font = list(
          color = "#263648"
        )
      ),

      margin = list(
        l = 65,
        r = 30,
        t = 30,
        b = 60
      )
    ) |> 

    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}


# ============================================================
# 4. LOAD DATA
# ============================================================

labour_market_slack <- safe_read_csv(
  "labour_market_slack.csv"
)

combined_data <- safe_read_csv(
  "combined_tightness_slack.csv"
)

wpi_data <- safe_read_csv(
  "labour_market_wpi.csv"
)

ivi_anzsco2 <- safe_read_csv(
  "ivi_anzsco2_clean.csv"
)

ivi_anzsco4 <- safe_read_csv(
  "ivi_anzsco4_clean.csv"
)

ivi_skill <- safe_read_csv(
  "ivi_skill_clean.csv"
)


# ============================================================
# 5. PREPARE SLACK DATA
# ============================================================

if (nrow(labour_market_slack) > 0) {

  if ("date" %in% names(labour_market_slack)) {

    labour_market_slack$date <-
      safe_date(
        labour_market_slack$date
      )

  } else {

    labour_market_slack <-
      tibble()
  }
}


if (nrow(labour_market_slack) > 0) {

  labour_market_slack <-
    labour_market_slack |> 

    arrange(date) |> 

    mutate(

      period = case_when(

        date < as.Date("1990-01-01") ~
          "1978–1989",

        date < as.Date("1995-01-01") ~
          "1990–1994",

        date < as.Date("2008-01-01") ~
          "1995–2007",

        date < as.Date("2020-01-01") ~
          "2008–2019",

        date < as.Date("2022-01-01") ~
          "2020–2021",

        TRUE ~
          "2022–present"
      ),

      unemploy_change =
        unemployment_rate -
        lag(unemployment_rate),

      underemploy_change =
        underemployment_rate -
        lag(underemployment_rate),

      underutilise_change =
        underutilisation_rate -
        lag(underutilisation_rate),

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
}


# ============================================================
# 6. PREPARE COMBINED DATA
# ============================================================

if (nrow(combined_data) > 0) {

  if ("Quarter" %in% names(combined_data)) {

    combined_data$Quarter <-
      safe_date(
        combined_data$Quarter
      )

  } else {

    combined_data <-
      tibble()
  }
}


if (nrow(combined_data) > 0) {

  combined_data <-
    combined_data |> 

    arrange(Quarter) |> 

    mutate(

      Job_Vacancies =
        safe_numeric(
          Job_Vacancies
        ),

      Tightness =
        safe_numeric(
          Tightness
        ),

      unemployment_rate =
        safe_numeric(
          unemployment_rate
        ),

      underemployment_rate =
        safe_numeric(
          underemployment_rate
        ),

      underutilisation_rate =
        safe_numeric(
          underutilisation_rate
        ),

      change_vacancies =
        Job_Vacancies -
        lag(Job_Vacancies),

      change_unemployment =
        unemployment_rate -
        lag(unemployment_rate),

      change_underemployment =
        underemployment_rate -
        lag(underemployment_rate),

      change_underutilisation =
        underutilisation_rate -
        lag(underutilisation_rate),

      change_tightness =
        Tightness -
        lag(Tightness)
    )
}


# ============================================================
# 7. PREPARE WPI DATA
# ============================================================

if (nrow(wpi_data) > 0) {

  wpi_date_candidates <-
    names(wpi_data)[
      grepl(
        "date|quarter|period",
        names(wpi_data),
        ignore.case = TRUE
      )
    ]

  wpi_value_candidates <-
    names(wpi_data)[
      grepl(
        "wpi",
        names(wpi_data),
        ignore.case = TRUE
      )
    ]

  if (
    length(wpi_date_candidates) > 0 &&
    length(wpi_value_candidates) > 0
  ) {

    wpi_date_col <-
      wpi_date_candidates[1]

    wpi_value_col <-
      wpi_value_candidates[1]

    wpi_data <-
      wpi_data |> 

      mutate(

        wpi_date =
          safe_date(
            .data[[wpi_date_col]]
          ),

        WPI_value =
          safe_numeric(
            .data[[wpi_value_col]]
          )
      ) |> 

      arrange(wpi_date) |> 

      mutate(

        WPI_growth =
          (
            WPI_value /
              lag(WPI_value) -
              1
          ) * 100,

        WPI_growth_lead1 =
          lead(
            WPI_growth,
            1
          ),

        WPI_growth_lead2 =
          lead(
            WPI_growth,
            2
          )
      )

  } else {

    wpi_data <-
      tibble()
  }
}


# ============================================================
# 8. PREPARE IVI DATA
# ============================================================

prepare_ivi <- function(data) {

  if (nrow(data) == 0) {
    return(tibble())
  }

  required_columns <- c(
    "Month",
    "Vacancies"
  )

  if (
    !all(
      required_columns %in%
        names(data)
    )
  ) {

    return(tibble())
  }

  data |> 

    mutate(

      Month =
        safe_date(
          Month
        ),

      Vacancies =
        safe_numeric(
          Vacancies
        )
    ) |> 

    arrange(Month)
}


ivi_anzsco2 <-
  prepare_ivi(
    ivi_anzsco2
  )

ivi_anzsco4 <-
  prepare_ivi(
    ivi_anzsco4
  )

ivi_skill <-
  prepare_ivi(
    ivi_skill
  )


# ============================================================
# 9. NATIONAL IVI
# ============================================================

national_ivi <- tibble()


if (
  nrow(ivi_anzsco2) > 0 &&
  all(
    c(
      "Level",
      "ANZSCO_CODE",
      "State"
    ) %in%
    names(ivi_anzsco2)
  )
) {

  national_ivi <-
    ivi_anzsco2 |> 

    filter(

      as.character(Level) == "1",

      as.character(ANZSCO_CODE) == "0",

      toupper(
        as.character(State)
      ) == "AUST"
    )
}


# Fallback if national rows cannot be found

if (
  nrow(national_ivi) == 0 &&
  nrow(ivi_anzsco2) > 0
) {

  national_ivi <-
    ivi_anzsco2 |> 

    group_by(Month) |> 

    summarise(
      Vacancies =
        sum(
          Vacancies,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
}


# ============================================================
# 10. OCCUPATION DATA
# ============================================================

occupation_data <- tibble()


if (
  nrow(ivi_anzsco2) > 0 &&
  "Level" %in% names(ivi_anzsco2)
) {

  occupation_data <-
    ivi_anzsco2 |> 

    filter(
      as.character(Level) == "2"
    )
}


# ============================================================
# 11. UI
# ============================================================

ui <- fluidPage(

  # ----------------------------------------------------------
  # CSS
  # ----------------------------------------------------------

  tags$head(

    tags$style(
      HTML("

      body {
        background:
          linear-gradient(
            180deg,
            #F4F7FB 0%,
            #F8FAFC 100%
          );

        font-family:
          -apple-system,
          BlinkMacSystemFont,
          'Segoe UI',
          Arial,
          sans-serif;

        color:
          #263648;
      }


      .container-fluid {
        padding-left: 28px;
        padding-right: 28px;
      }


      /* HEADER */

      .dashboard-header {

        background:
          linear-gradient(
            135deg,
            #102A43 0%,
            #164E63 50%,
            #087F8C 100%
          );

        color: white;

        padding: 34px 38px;

        border-radius: 18px;

        margin-top: 18px;
        margin-bottom: 25px;

        box-shadow:
          0 8px 25px
          rgba(
            16,
            42,
            67,
            0.15
          );
      }


      .dashboard-header h1 {

        margin: 0 0 8px 0;

        font-size: 30px;

        font-weight: 750;
      }


      .dashboard-header p {

        margin: 4px 0;

        color:
          rgba(
            255,
            255,
            255,
            0.88
          );
      }


      /* TABS */

      .nav-tabs {

        border-bottom:
          1px solid #DCE5ED;

        margin-bottom: 20px;
      }


      .nav-tabs > li > a {

        color:
          #526579;

        font-weight:
          600;

        border:
          none !important;

        padding:
          12px 14px;
      }


      .nav-tabs > li > a:hover {

        color:
          #087F8C;

        background:
          #EEF8F9 !important;

        border-radius:
          8px 8px 0 0;
      }


      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {

        color:
          #087F8C !important;

        background:
          transparent !important;

        border:
          none !important;

        border-bottom:
          3px solid #087F8C !important;

        font-weight:
          700;
      }


      /* CARDS */

      .section-card {

        background:
          white;

        border:
          1px solid #E3EAF1;

        border-radius:
          16px;

        padding:
          24px;

        margin-bottom:
          24px;

        box-shadow:
          0 4px 18px
          rgba(
            31,
            50,
            70,
            0.055
          );
      }


      .section-card h3 {

        color:
          #102A43;

        font-size:
          20px;

        font-weight:
          750;

        margin-top:
          0;

        margin-bottom:
          18px;

        padding-bottom:
          12px;

        border-bottom:
          1px solid #EDF1F5;
      }


      .section-card h3::before {

        content:
          '';

        display:
          inline-block;

        width:
          5px;

        height:
          20px;

        background:
          #087F8C;

        border-radius:
          5px;

        margin-right:
          10px;

        vertical-align:
          -3px;
      }


      /* KPI */

      .kpi-card {

        background:
          white;

        border:
          1px solid #E3EAF1;

        border-left:
          5px solid #087F8C;

        border-radius:
          15px;

        padding:
          20px;

        min-height:
          140px;

        margin-bottom:
          22px;

        box-shadow:
          0 4px 18px
          rgba(
            31,
            50,
            70,
            0.06
          );
      }


      .kpi-blue {
        border-left-color:
          #2774C6;
      }


      .kpi-orange {
        border-left-color:
          #E67E22;
      }


      .kpi-green {
        border-left-color:
          #19947A;
      }


      .kpi-purple {
        border-left-color:
          #7657A8;
      }


      .kpi-label {

        color:
          #718096;

        font-size:
          11px;

        font-weight:
          750;

        text-transform:
          uppercase;

        letter-spacing:
          0.7px;
      }


      .kpi-value {

        color:
          #102A43;

        font-size:
          29px;

        font-weight:
          750;

        margin-top:
          8px;
      }


      .kpi-description {

        color:
          #8A98A8;

        font-size:
          12px;

        margin-top:
          5px;
      }


      /* RESEARCH BOX */

      .research-box {

        background:
          linear-gradient(
            135deg,
            #E9F7F8,
            #F3FAFF
          );

        border:
          1px solid #CDE8EB;

        border-left:
          6px solid #087F8C;

        border-radius:
          16px;

        padding:
          25px 28px;

        margin-bottom:
          25px;
      }


      .research-question {

        color:
          #102A43;

        font-size:
          21px;

        font-weight:
          750;

        line-height:
          1.45;

        margin-bottom:
          10px;
      }


      /* METHOD */

      .method-box {

        background:
          white;

        border:
          1px solid #E2E9F0;

        border-left:
          5px solid #2774C6;

        border-radius:
          12px;

        padding:
          20px;

        margin-bottom:
          16px;

        box-shadow:
          0 2px 9px
          rgba(
            31,
            50,
            70,
            0.04
          );
      }


      .method-box h4 {

        color:
          #102A43;

        font-weight:
          750;

        margin-top:
          0;
      }


      /* SIDEBAR */

      .well {

        background:
          white;

        border:
          1px solid #E1E8EF;

        border-radius:
          15px;

        box-shadow:
          0 3px 12px
          rgba(
            31,
            50,
            70,
            0.05
          );
      }


      .control-label {

        color:
          #29445F;

        font-weight:
          700;

        font-size:
          13px;
      }


      .form-control {

        border:
          1px solid #D4DEE8;

        border-radius:
          8px;

        box-shadow:
          none;
      }


      .selectize-input {

        border:
          1px solid #D4DEE8 !important;

        border-radius:
          8px !important;

        box-shadow:
          none !important;
      }


      /* BUTTON */

      .btn-primary {

        background:
          linear-gradient(
            135deg,
            #087F8C,
            #0B9AA5
          );

        border:
          none;

        border-radius:
          8px;

        font-weight:
          650;

        padding:
          9px 17px;
      }


      .btn-primary:hover {

        background:
          linear-gradient(
            135deg,
            #066C76,
            #087F8C
          );
      }


      /* TABLES */

      table.dataTable thead th {

        background:
          #EEF6F8 !important;

        color:
          #12355B !important;

        font-weight:
          700;

        border-bottom:
          2px solid #CFE4E7 !important;
      }


      table.dataTable tbody tr:hover {

        background:
          #F3FAFB !important;
      }


      .dataTables_wrapper {

        color:
          #425466;
      }


      .dataTables_length select,
      .dataTables_filter input {

        border:
          1px solid #CBD7E2;

        border-radius:
          7px;
      }


      .dataTables_paginate .paginate_button.current {

        background:
          #087F8C !important;

        color:
          white !important;

        border:
          none !important;
      }


      /* FOOTER */

      .dashboard-footer {

        margin-top:
          35px;

        padding:
          28px;

        text-align:
          center;

        color:
          #718096;

        font-size:
          12px;

        border-top:
          1px solid #E2E8F0;
      }


      @media(max-width: 900px) {

        .dashboard-header {
          padding: 25px;
        }

        .dashboard-header h1 {
          font-size: 24px;
        }

        .section-card {
          padding: 17px;
        }

        .research-question {
          font-size: 18px;
        }
      }

      ")
    )
  ),


  # ==========================================================
  # HEADER
  # ==========================================================

  div(

    class = "dashboard-header",

    h1(
      "Australian Labour Market Dashboard"
    ),

    p(
      "Labour demand, labour-market slack, tightness and wage pressure"
    ),

    p(
      "ETC-5543 | Australian Labour Market Project"
    )
  ),


  # ==========================================================
  # TABS
  # ==========================================================

  tabsetPanel(

    id = "main_tabs",

    type = "tabs",


    # ========================================================
    # OVERVIEW
    # ========================================================

    tabPanel(

      "Overview",

      br(),

      fluidRow(

        column(
          3,

          div(
            class = "kpi-card kpi-blue",

            div(
              class = "kpi-label",
              "Job Vacancies"
            ),

            div(
              class = "kpi-value",
              textOutput(
                "overview_vacancies",
                inline = TRUE
              )
            ),

            div(
              class = "kpi-description",
              "Latest available quarter"
            )
          )
        ),


        column(
          3,

          div(
            class = "kpi-card kpi-green",

            div(
              class = "kpi-label",
              "Labour-Market Tightness"
            ),

            div(
              class = "kpi-value",
              textOutput(
                "overview_tightness",
                inline = TRUE
              )
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
            class = "kpi-card kpi-orange",

            div(
              class = "kpi-label",
              "Unemployment"
            ),

            div(
              class = "kpi-value",
              textOutput(
                "overview_unemployment",
                inline = TRUE
              )
            ),

            div(
              class = "kpi-description",
              "Latest available observation"
            )
          )
        ),


        column(
          3,

          div(
            class = "kpi-card kpi-purple",

            div(
              class = "kpi-label",
              "Underutilisation"
            ),

            div(
              class = "kpi-value",
              textOutput(
                "overview_underutilisation",
                inline = TRUE
              )
            ),

            div(
              class = "kpi-description",
              "Primary observable slack measure"
            )
          )
        )
      ),


      div(

        class = "research-box",

        div(
          class = "research-question",
          "Research Question"
        ),

        p(
          "To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?"
        ),

        p(
          "The dashboard examines statistical associations between vacancies, labour-market tightness and measures of slack. The relationships should not be interpreted as evidence of causation."
        )
      ),


      div(

        class = "section-card",

        h3(
          "Labour-Market Overview"
        ),

        plotlyOutput(
          "overview_trend",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Dashboard Guide"
        ),

        tags$ul(

          tags$li(
            strong("Labour Demand: "),
            "Explore online job advertisements and labour-demand trends."
          ),

          tags$li(
            strong("Labour-Market Slack: "),
            "Compare unemployment, underemployment and underutilisation."
          ),

          tags$li(
            strong("Tightness & Slack: "),
            "Examine associations between vacancies and labour-market slack."
          ),

          tags$li(
            strong("Research Question: "),
            "Review the main statistical relationships."
          ),

          tags$li(
            strong("Wage Pressure: "),
            "Explore associations with wage growth."
          ),

          tags$li(
            strong("Occupation Demand: "),
            "Explore vacancy demand across occupations."
          ),

          tags$li(
            strong("Data Explorer: "),
            "Search, filter and download project data."
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

      sidebarLayout(

        sidebarPanel(

          h4(
            "Labour Demand Controls"
          ),

          selectInput(

            "demand_period",

            "Time period:",

            choices = c(
              "All available data",
              "Last 10 years",
              "Last 5 years",
              "Last 3 years"
            ),

            selected =
              "All available data"
          ),

          checkboxInput(

            "demand_yoy",

            "Show year-on-year change",

            FALSE
          ),

          width = 3
        ),


        mainPanel(

          div(

            class = "section-card",

            h3(
              "Online Job Advertisements"
            ),

            plotlyOutput(
              "ivi_national_plot",
              height = "500px"
            )
          ),


          div(

            class = "section-card",

            h3(
              "Labour Demand Growth"
            ),

            plotlyOutput(
              "ivi_growth_plot",
              height = "450px"
            )
          )
        )
      )
    ),


    # ========================================================
    # LABOUR-MARKET SLACK
    # ========================================================

    tabPanel(

      "Labour-Market Slack",

      br(),

      sidebarLayout(

        sidebarPanel(

          h4(
            "Slack Controls"
          ),

          checkboxGroupInput(

            "slack_measures",

            "Measures:",

            choices = c(
              "Unemployment" =
                "Unemployment",

              "Underemployment" =
                "Underemployment",

              "Underutilisation" =
                "Underutilisation"
            ),

            selected = c(
              "Unemployment",
              "Underemployment",
              "Underutilisation"
            )
          ),


          selectInput(

            "slack_period",

            "Historical period:",

            choices = c(
              "All periods",
              "1978–1989",
              "1990–1994",
              "1995–2007",
              "2008–2019",
              "2020–2021",
              "2022–present"
            ),

            selected =
              "All periods"
          ),


          uiOutput(
            "slack_date_ui"
          ),


          actionButton(
            "reset_slack",
            "Reset filters",
            class = "btn-primary"
          ),

          br(),
          br(),

          p(
            "Underutilisation is used as the primary observable proxy for labour-market slack, while unemployment and underemployment provide complementary measures."
          ),

          width = 3
        ),


        mainPanel(

          div(

            class = "section-card",

            h3(
              "Labour-Market Slack Over Time"
            ),

            plotlyOutput(
              "slack_trend",
              height = "550px"
            )
          ),


          div(

            class = "section-card",

            h3(
              "Youth vs Overall Slack"
            ),

            plotlyOutput(
              "youth_slack_plot",
              height = "500px"
            )
          ),


          div(

            class = "section-card",

            h3(
              "Historical Period Summary"
            ),

            DTOutput(
              "slack_period_table"
            )
          )
        )
      )
    ),


    # ========================================================
    # TIGHTNESS & SLACK
    # ========================================================

    tabPanel(

      "Tightness & Slack",

      br(),

      div(

        class = "section-card",

        h3(
          "Vacancies and Labour-Market Tightness"
        ),

        plotlyOutput(
          "tightness_plot",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Changes in Vacancies vs Changes in Slack"
        ),

        selectInput(

          "tightness_slack_measure",

          "Slack measure:",

          choices = c(
            "Unemployment",
            "Underemployment",
            "Underutilisation"
          ),

          selected =
            "Underutilisation"
        ),

        plotlyOutput(
          "tightness_slack_scatter",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Correlation Summary"
        ),

        DTOutput(
          "tightness_correlations"
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

        class = "research-box",

        div(
          class = "research-question",

          "To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Main Evidence"
        ),

        p(
          "The analysis compares changes in job vacancies with changes in unemployment, underemployment and underutilisation."
        ),

        p(
          "Negative correlations indicate that increases in vacancies tend to coincide with reductions in measured labour-market slack. These relationships are statistical associations and do not establish causality."
        ),

        plotlyOutput(
          "research_scatter",
          height = "550px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Correlation by Slack Measure"
        ),

        DTOutput(
          "research_correlation_table"
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

        h3(
          "Wage Price Index Growth"
        ),

        plotlyOutput(
          "wpi_plot",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Tightness and Wage Growth"
        ),

        plotlyOutput(
          "tightness_wpi_scatter",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Underutilisation and Wage Growth"
        ),

        plotlyOutput(
          "slack_wpi_scatter",
          height = "500px"
        )
      ),


      div(

        class = "section-card",

        h3(
          "Timing of Associations"
        ),

        DTOutput(
          "wpi_timing_table"
        )
      )
    ),


    # ========================================================
    # OCCUPATION DEMAND
    # ========================================================

    tabPanel(

      "Occupation Demand",

      br(),

      sidebarLayout(

        sidebarPanel(

          h4(
            "Occupation Controls"
          ),

          selectInput(

            "occupation_level",

            "Occupation dataset:",

            choices = c(
              "Broad occupations",
              "Detailed occupations"
            ),

            selected =
              "Broad occupations"
          ),

          width = 3
        ),


        mainPanel(

          div(

            class = "section-card",

            h3(
              "Occupation Vacancy Trends"
            ),

            plotlyOutput(
              "occupation_trend",
              height = "550px"
            )
          ),


          div(

            class = "section-card",

            h3(
              "Occupation Vacancy Table"
            ),

            DTOutput(
              "occupation_table"
            )
          ),


          div(

            class = "section-card",

            h3(
              "Skill-Level Demand"
            ),

            plotlyOutput(
              "skill_plot",
              height = "500px"
            )
          )
        )
      )
    ),


    # ========================================================
    # DATA EXPLORER
    # ========================================================

    tabPanel(

      "Data Explorer",

      br(),

      sidebarLayout(

        sidebarPanel(

          h4(
            "Data Selection"
          ),

          selectInput(

            "explorer_dataset",

            "Dataset:",

            choices = c(
              "Labour-market slack",
              "Vacancy and tightness",
              "Wage Price Index",
              "IVI occupation data"
            ),

            selected =
              "Labour-market slack"
          ),


          numericInput(

            "explorer_rows",

            "Rows to show:",

            value = 25,

            min = 10,

            max = 500,

            step = 10
          ),


          downloadButton(

            "download_explorer",

            "Download current data"
          ),

          br(),
          br(),

          p(
            "Use the Show entries dropdown and table search tools to explore the selected dataset."
          ),

          width = 3
        ),


        mainPanel(

          div(

            class = "section-card",

            h3(
              "Interactive Data Explorer"
            ),

            DTOutput(
              "data_explorer"
            )
          )
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

        h3(
          "Research Design"
        ),

        p(
          "The project investigates the association between labour demand and labour-market slack in Australia."
        ),

        p(
          "The analysis is descriptive and correlational. It does not attempt to identify a causal effect of vacancies on labour-market slack."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Labour-market slack"
        ),

        p(
          "Unemployment, underemployment and underutilisation are examined using ABS Labour Force data. Underutilisation is treated as the primary observable proxy for labour-market slack because it incorporates both unemployment and underemployment."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Labour demand"
        ),

        p(
          "The Jobs and Skills Australia Internet Vacancy Index provides a monthly measure of new online job advertisements. ABS Job Vacancies provide a quarterly measure of vacancies."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Labour-market tightness"
        ),

        p(
          "Tightness is calculated as ABS Job Vacancies divided by the number of unemployed persons."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Wage pressure"
        ),

        p(
          "The Wage Price Index is used as an outcome/context measure to examine associations between labour-market tightness, slack and wage growth."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Historical periods"
        ),

        p(
          "The dashboard compares 1978–1989, 1990–1994, 1995–2007, 2008–2019, 2020–2021 and 2022–present."
        )
      ),


      div(

        class = "method-box",

        h4(
          "Interpretation"
        ),

        p(
          "Correlation measures the strength and direction of statistical association. A negative correlation between vacancies and slack indicates that higher vacancies tend to occur alongside lower measured slack. This does not establish causation."
        )
      )
    )
  ),


  div(

    class = "dashboard-footer",

    "Australian Labour Market Project | ETC-5543 | Monash University"
  )
)


# ============================================================
# 12. SERVER
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # SLACK DATE UI
  # ==========================================================

  output$slack_date_ui <- renderUI({

    if (
      nrow(labour_market_slack) == 0 ||
      !"date" %in% names(labour_market_slack)
    ) {

      return(
        p(
          "Date filter unavailable."
        )
      )
    }


    valid_dates <-
      labour_market_slack$date[
        !is.na(
          labour_market_slack$date
        )
      ]


    if (length(valid_dates) == 0) {

      return(
        p(
          "Date filter unavailable."
        )
      )
    }


    dateRangeInput(

      "slack_dates",

      "Date range:",

      start =
        min(
          valid_dates
        ),

      end =
        max(
          valid_dates
        )
    )
  })


  # ==========================================================
  # OVERVIEW VACANCIES
  # ==========================================================

  output$overview_vacancies <-
    renderText({

      if (
        nrow(combined_data) == 0
      ) {
        return("N/A")
      }


      data <-
        combined_data %>%

        filter(
          !is.na(Job_Vacancies),
          !is.na(Quarter)
        )


      if (
        nrow(data) == 0
      ) {
        return("N/A")
      }


      latest <-
        data |> 
        slice_max(
          Quarter,
          n = 1,
          with_ties = FALSE
        )


      format_number(
        latest$Job_Vacancies[1],
        0
      )
    })


  # ==========================================================
  # OVERVIEW TIGHTNESS
  # ==========================================================

  output$overview_tightness <-
    renderText({

      if (
        nrow(combined_data) == 0
      ) {
        return("N/A")
      }


      data <-
        combined_data |> 

        filter(
          !is.na(Tightness),
          !is.na(Quarter)
        )


      if (
        nrow(data) == 0
      ) {
        return("N/A")
      }


      latest <-
        data |> 
        slice_max(
          Quarter,
          n = 1,
          with_ties = FALSE
        )


      format_number(
        latest$Tightness[1],
        2
      )
    })


  # ==========================================================
  # OVERVIEW UNEMPLOYMENT
  # ==========================================================

  output$overview_unemployment <-
    renderText({

      if (
        nrow(labour_market_slack) == 0
      ) {
        return("N/A")
      }


      data <-
        labour_market_slack |> 

        filter(
          !is.na(unemployment_rate),
          !is.na(date)
        )


      if (
        nrow(data) == 0
      ) {
        return("N/A")
      }


      latest <-
        data |> 
        slice_max(
          date,
          n = 1,
          with_ties = FALSE
        )


      format_percent(
        latest$unemployment_rate[1]
      )
    })


  # ==========================================================
  # OVERVIEW UNDERUTILISATION
  # ==========================================================

  output$overview_underutilisation <-
    renderText({

      if (
        nrow(labour_market_slack) == 0
      ) {
        return("N/A")
      }


      data <-
        labour_market_slack |> 

        filter(
          !is.na(underutilisation_rate),
          !is.na(date)
        )


      if (
        nrow(data) == 0
      ) {
        return("N/A")
      }


      latest <-
        data |> 
        slice_max(
          date,
          n = 1,
          with_ties = FALSE
        )


      format_percent(
        latest$underutilisation_rate[1]
      )
    })


  # ==========================================================
  # OVERVIEW CHART
  # ==========================================================

  output$overview_trend <-
    renderPlotly({

      if (
        nrow(labour_market_slack) == 0
      ) {
        return(
          empty_plot(
            "Labour-market slack data are unavailable."
          )
        )
      }


      data <-
        labour_market_slack |> 

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

          measure =
            case_when(

              measure ==
                "unemployment_rate" ~
                "Unemployment",

              measure ==
                "underemployment_rate" ~
                "Underemployment",

              measure ==
                "underutilisation_rate" ~
                "Underutilisation",

              TRUE ~ measure
            )
        )


      p <-
        ggplot(
          data,
          aes(
            x = date,
            y = rate,
            colour = measure
          )
        ) +

        geom_line(
          linewidth = 1
        ) +

        scale_colour_manual(
          values = dashboard_palette
        ) +

        labs(
          x = NULL,
          y = "Rate (%)",
          colour = "Measure"
        ) +

        theme_minimal(
          base_size = 13
        ) +

        theme(
          legend.position = "top",
          panel.grid.minor =
            element_blank()
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # DEMAND DATA
  # ==========================================================

  demand_data <- reactive({

    if (
      nrow(national_ivi) == 0
    ) {
      return(tibble())
    }


    data <-
      national_ivi |> 

      filter(
        !is.na(Month),
        !is.na(Vacancies)
      ) |> 

      arrange(Month)


    if (
      input$demand_period ==
      "Last 10 years"
    ) {

      max_date <-
        max(
          data$Month,
          na.rm = TRUE
        )

      data <-
        data |> 
        filter(
          Month >=
            max_date -
            years(10)
        )
    }


    if (
      input$demand_period ==
      "Last 5 years"
    ) {

      max_date <-
        max(
          data$Month,
          na.rm = TRUE
        )

      data <-
        data |> 
        filter(
          Month >=
            max_date -
            years(5)
        )
    }


    if (
      input$demand_period ==
      "Last 3 years"
    ) {

      max_date <-
        max(
          data$Month,
          na.rm = TRUE
        )

      data <-
        data |> 
        filter(
          Month >=
            max_date -
            years(3)
        )
    }


    data |> 

      mutate(

        yoy_change =
          (
            Vacancies /
              lag(
                Vacancies,
                12
              ) -
              1
          ) * 100
      )
  })


  # ==========================================================
  # IVI NATIONAL
  # ==========================================================

  output$ivi_national_plot <-
    renderPlotly({

      data <-
        demand_data()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "IVI data are unavailable."
          )
        )
      }


      p <-
        ggplot(

          data,

          aes(
            x = Month,
            y = Vacancies
          )
        ) +

        geom_area(
          fill = "#2774C6",
          alpha = 0.12
        ) +

        geom_line(
          colour = "#2774C6",
          linewidth = 1.1
        ) +

        labs(
          x = NULL,
          y = "Online job advertisements"
        ) +

        theme_minimal(
          base_size = 13
        ) +

        theme(
          panel.grid.minor =
            element_blank()
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # IVI GROWTH
  # ==========================================================

  output$ivi_growth_plot <-
    renderPlotly({

      data <-
        demand_data()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "IVI data are unavailable."
          )
        )
      }


      p <-
        ggplot(

          data,

          aes(
            x = Month,
            y = yoy_change
          )
        ) +

        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          colour = "#9AA5B1"
        ) +

        geom_line(
          colour = "#087F8C",
          linewidth = 1
        ) +

        labs(
          x = NULL,
          y = "Year-on-year change (%)"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # FILTERED SLACK
  # ==========================================================

  filtered_slack <- reactive({

    if (
      nrow(labour_market_slack) == 0
    ) {
      return(tibble())
    }


    data <-
      labour_market_slack


    if (
      !is.null(input$slack_dates) &&
      length(input$slack_dates) == 2
    ) {

      data <-
        data |> 

        filter(

          date >=
            input$slack_dates[1],

          date <=
            input$slack_dates[2]
        )
    }


    if (
      input$slack_period !=
      "All periods"
    ) {

      data <-
        data %>%

        filter(
          period ==
            input$slack_period
        )
    }


    data
  })


  # ==========================================================
  # RESET SLACK
  # ==========================================================

  observeEvent(

    input$reset_slack,

    {

      updateCheckboxGroupInput(

        session,

        "slack_measures",

        selected = c(
          "Unemployment",
          "Underemployment",
          "Underutilisation"
        )
      )


      updateSelectInput(

        session,

        "slack_period",

        selected =
          "All periods"
      )


      if (
        nrow(labour_market_slack) > 0
      ) {

        valid_dates <-
          labour_market_slack$date[
            !is.na(
              labour_market_slack$date
            )
          ]


        if (
          length(valid_dates) > 0
        ) {

          updateDateRangeInput(

            session,

            "slack_dates",

            start =
              min(valid_dates),

            end =
              max(valid_dates)
          )
        }
      }
    }
  )


  # ==========================================================
  # SLACK TREND
  # ==========================================================

  output$slack_trend <-
    renderPlotly({

      data <-
        filtered_slack()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "No slack data match the selected filters."
          )
        )
      }


      if (
        length(input$slack_measures) == 0
      ) {

        return(
          empty_plot(
            "Please select at least one measure."
          )
        )
      }


      plot_data <-
        data |> 

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

          measure =
            case_when(

              measure ==
                "unemployment_rate" ~
                "Unemployment",

              measure ==
                "underemployment_rate" ~
                "Underemployment",

              measure ==
                "underutilisation_rate" ~
                "Underutilisation",

              TRUE ~ measure
            )
        ) |> 

        filter(
          measure %in%
            input$slack_measures
        )


      p <-
        ggplot(

          plot_data,

          aes(
            x = date,
            y = rate,
            colour = measure
          )
        ) +

        geom_line(
          linewidth = 1
        ) +

        scale_colour_manual(
          values = dashboard_palette
        ) +

        labs(
          x = NULL,
          y = "Rate (%)",
          colour = "Measure"
        ) +

        theme_minimal(
          base_size = 13
        ) +

        theme(
          legend.position = "top",
          panel.grid.minor =
            element_blank()
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # YOUTH SLACK
  # ==========================================================

  output$youth_slack_plot <-
    renderPlotly({

      if (
        nrow(labour_market_slack) == 0
      ) {

        return(
          empty_plot(
            "Youth data are unavailable."
          )
        )
      }


      required_columns <- c(
        "date",
        "unemployment_rate",
        "youth_unemployment_rate",
        "underutilisation_rate",
        "youth_underutilisation_rate"
      )


      if (
        !all(
          required_columns %in%
            names(labour_market_slack)
        )
      ) {

        return(
          empty_plot(
            "Youth measures are unavailable."
          )
        )
      }


      data <-
        labour_market_slack |> 

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

          category =
            case_when(

              grepl(
                "underutilisation",
                measure
              ) ~
                "Underutilisation",

              TRUE ~
                "Unemployment"
            ),

          group =
            case_when(

              grepl(
                "youth",
                measure
              ) ~
                "Youth",

              TRUE ~
                "Overall"
            )
        )


      p <-
        ggplot(

          data,

          aes(
            x = date,
            y = rate,
            colour = group
          )
        ) +

        geom_line(
          linewidth = 0.9
        ) +

        facet_wrap(
          ~category,
          scales = "free_y"
        ) +

        scale_colour_manual(

          values = c(
            "Overall" =
              dashboard_palette[
                "Overall"
              ],

            "Youth" =
              dashboard_palette[
                "Youth"
              ]
          )
        ) +

        labs(
          x = NULL,
          y = "Rate (%)",
          colour = NULL
        ) +

        theme_minimal(
          base_size = 13
        ) +

        theme(
          legend.position = "top"
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # SLACK PERIOD TABLE
  # ==========================================================

  output$slack_period_table <-
    renderDT({

      if (
        nrow(labour_market_slack) == 0
      ) {

        return(
          datatable(
            tibble(
              Message =
                "Slack data are unavailable."
            ),
            options =
              datatable_options()
          )
        )
      }


      table_data <-
        labour_market_slack |> 

        group_by(period) |> 

        summarise(

          Observations =
            n(),

          Unemployment =
            mean(
              unemployment_rate,
              na.rm = TRUE
            ),

          Underemployment =
            mean(
              underemployment_rate,
              na.rm = TRUE
            ),

          Underutilisation =
            mean(
              underutilisation_rate,
              na.rm = TRUE
            ),

          .groups = "drop"
        ) |> 

        mutate(

          Unemployment =
            round(
              Unemployment,
              2
            ),

          Underemployment =
            round(
              Underemployment,
              2
            ),

          Underutilisation =
            round(
              Underutilisation,
              2
            )
        )


      datatable(

        table_data,

        rownames = FALSE,

        options =
          datatable_options(
            10
          )
      )
    })


  # ==========================================================
  # TIGHTNESS PLOT
  # ==========================================================

  output$tightness_plot <-
    renderPlotly({

      if (
        nrow(combined_data) == 0
      ) {

        return(
          empty_plot(
            "Vacancy and tightness data are unavailable."
          )
        )
      }


      p1 <-
        ggplot(

          combined_data,

          aes(
            x = Quarter,
            y = Job_Vacancies
          )
        ) +

        geom_line(
          colour = "#2774C6",
          linewidth = 1.1
        ) +

        labs(
          x = NULL,
          y = "Job vacancies"
        ) +

        theme_minimal(
          base_size = 13
        )


      p2 <-
        ggplot(

          combined_data,

          aes(
            x = Quarter,
            y = Tightness
          )
        ) +

        geom_line(
          colour = "#087F8C",
          linewidth = 1.1
        ) +

        labs(
          x = NULL,
          y = "Tightness"
        ) +

        theme_minimal(
          base_size = 13
        )


      plot_ly() |> 

        add_trace(
          x = combined_data$Quarter,
          y = combined_data$Job_Vacancies,
          type = "scatter",
          mode = "lines",
          name = "Job Vacancies",
          line = list(
            color = "#2774C6",
            width = 2
          ),
          yaxis = "y"
        ) |> 

        add_trace(
          x = combined_data$Quarter,
          y = combined_data$Tightness,
          type = "scatter",
          mode = "lines",
          name = "Tightness",
          line = list(
            color = "#087F8C",
            width = 2
          ),
          yaxis = "y2"
        ) |> 

        layout(

          xaxis = list(
            title = ""
          ),

          yaxis = list(
            title = "Job vacancies"
          ),

          yaxis2 = list(
            title = "Tightness",
            overlaying = "y",
            side = "right"
          ),

          legend = list(
            orientation = "h",
            x = 0,
            y = 1.1
          )
        ) |> 

        clean_plotly()
    })


  # ==========================================================
  # TIGHTNESS VS SLACK SCATTER
  # ==========================================================

  output$tightness_slack_scatter <-
    renderPlotly({

      if (
        nrow(combined_data) == 0
      ) {

        return(
          empty_plot(
            "Combined data are unavailable."
          )
        )
      }


      y_column <-
        case_when(

          input$tightness_slack_measure ==
            "Unemployment" ~
            "change_unemployment",

          input$tightness_slack_measure ==
            "Underemployment" ~
            "change_underemployment",

          TRUE ~
            "change_underutilisation"
        )


      plot_data <-
        combined_data |> 

        filter(

          !is.na(change_vacancies),

          !is.na(
            .data[[y_column]]
          )
        )


      if (
        nrow(plot_data) < 3
      ) {

        return(
          empty_plot(
            "Not enough observations."
          )
        )
      }


      p <-
        ggplot(

          plot_data,

          aes(
            x = change_vacancies,
            y = .data[[y_column]]
          )
        ) +

        geom_point(
          colour = "#2774C6",
          alpha = 0.7,
          size = 2.7
        ) +

        geom_smooth(
          method = "lm",
          se = TRUE,
          colour = "#E67E22"
        ) +

        labs(

          x =
            "Change in job vacancies",

          y =
            paste(
              "Change in",
              input$tightness_slack_measure
            )
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # TIGHTNESS CORRELATIONS
  # ==========================================================

  output$tightness_correlations <-
    renderDT({

      if (
        nrow(combined_data) == 0
      ) {

        return(
          datatable(
            tibble(
              Message =
                "Data unavailable."
            ),
            options =
              datatable_options()
          )
        )
      }


      result <-
        tibble(

          Measure = c(
            "Unemployment",
            "Underemployment",
            "Underutilisation"
          ),

          Correlation =
            c(

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_unemployment
              ),

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_underemployment
              ),

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_underutilisation
              )
            )
        ) |> 

        mutate(
          Correlation =
            round(
              Correlation,
              3
            )
        )


      datatable(

        result,

        rownames = FALSE,

        options =
          datatable_options(
            10
          )
      )
    })


  # ==========================================================
  # RESEARCH SCATTER
  # ==========================================================

  output$research_scatter <-
    renderPlotly({

      if (
        nrow(combined_data) == 0
      ) {

        return(
          empty_plot(
            "Combined data are unavailable."
          )
        )
      }


      plot_data <-
        combined_data |> 

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

          names_to =
            "measure",

          values_to =
            "change_slack"
        ) |> 

        mutate(

          measure =
            case_when(

              measure ==
                "change_unemployment" ~
                "Unemployment",

              measure ==
                "change_underemployment" ~
                "Underemployment",

              measure ==
                "change_underutilisation" ~
                "Underutilisation",

              TRUE ~ measure
            )
        ) |> 

        filter(
          !is.na(change_vacancies),
          !is.na(change_slack)
        )


      if (
        nrow(plot_data) < 3
      ) {

        return(
          empty_plot(
            "Not enough observations."
          )
        )
      }


      p <-
        ggplot(

          plot_data,

          aes(
            x = change_vacancies,
            y = change_slack
          )
        ) +

        geom_point(
          colour = "#2774C6",
          alpha = 0.65,
          size = 2.5
        ) +

        geom_smooth(
          method = "lm",
          se = TRUE,
          colour = "#E67E22"
        ) +

        facet_wrap(
          ~measure,
          scales = "free_y"
        ) +

        labs(
          x = "Change in job vacancies",
          y = "Change in labour-market slack"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # RESEARCH CORRELATIONS
  # ==========================================================

  output$research_correlation_table <-
    renderDT({

      if (
        nrow(combined_data) == 0
      ) {

        return(
          datatable(
            tibble(
              Message =
                "Data unavailable."
            ),
            options =
              datatable_options()
          )
        )
      }


      result <-
        tibble(

          Measure = c(
            "Unemployment",
            "Underemployment",
            "Underutilisation"
          ),

          `Correlation with vacancy changes` =
            c(

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_unemployment
              ),

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_underemployment
              ),

              safe_cor(
                combined_data$change_vacancies,
                combined_data$change_underutilisation
              )
            )
        ) |> 

        mutate(

          `Correlation with vacancy changes` =
            round(
              `Correlation with vacancy changes`,
              3
            )
        )


      datatable(

        result,

        rownames = FALSE,

        options =
          datatable_options(
            10
          )
      )
    })


  # ==========================================================
  # WPI PLOT
  # ==========================================================

  output$wpi_plot <-
    renderPlotly({

      if (
        nrow(wpi_data) == 0
      ) {

        return(
          empty_plot(
            "WPI data are unavailable."
          )
        )
      }


      p <-
        ggplot(

          wpi_data,

          aes(
            x = wpi_date,
            y = WPI_growth
          )
        ) +

        geom_hline(

          yintercept = 0,

          linetype =
            "dashed",

          colour =
            "#9AA5B1"
        ) +

        geom_line(

          colour =
            "#E67E22",

          linewidth =
            1.1
        ) +

        labs(

          x = NULL,

          y =
            "WPI growth (%)"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # PREPARE WPI / COMBINED JOIN
  # ==========================================================

  wpi_combined_data <- reactive({

    if (
      nrow(combined_data) == 0 ||
      nrow(wpi_data) == 0
    ) {

      return(tibble())
    }


    combined_data |> 

      mutate(

        year =
          year(Quarter),

        quarter =
          quarter(Quarter)
      ) |> 

      left_join(

        wpi_data |> 

          mutate(

            year =
              year(wpi_date),

            quarter =
              quarter(wpi_date)
          ) |> 

          select(
            year,
            quarter,
            WPI_growth
          ),

        by = c(
          "year",
          "quarter"
        )
      ) |> 

      arrange(Quarter)
  })


  # ==========================================================
  # TIGHTNESS VS WPI
  # ==========================================================

  output$tightness_wpi_scatter <-
    renderPlotly({

      data <-
        wpi_combined_data()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "WPI and tightness data are unavailable."
          )
        )
      }


      data <-
        data |> 

        filter(

          !is.na(Tightness),

          !is.na(WPI_growth)
        )


      if (
        nrow(data) < 3
      ) {

        return(
          empty_plot(
            "Not enough matched observations."
          )
        )
      }


      p <-
        ggplot(

          data,

          aes(
            x = Tightness,
            y = WPI_growth
          )
        ) +

        geom_point(

          colour =
            "#087F8C",

          alpha =
            0.7,

          size =
            2.5
        ) +

        geom_smooth(

          method =
            "lm",

          se =
            TRUE,

          colour =
            "#E67E22"
        ) +

        labs(

          x =
            "Labour-market tightness",

          y =
            "WPI growth (%)"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # UNDERUTILISATION VS WPI
  # ==========================================================

  output$slack_wpi_scatter <-
    renderPlotly({

      data <-
        wpi_combined_data()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "WPI and slack data are unavailable."
          )
        )
      }


      data <-
        data |> 

        filter(

          !is.na(
            underutilisation_rate
          ),

          !is.na(
            WPI_growth
          )
        )


      if (
        nrow(data) < 3
      ) {

        return(
          empty_plot(
            "Not enough matched observations."
          )
        )
      }


      p <-
        ggplot(

          data,

          aes(

            x =
              underutilisation_rate,

            y =
              WPI_growth
          )
        ) +

        geom_point(

          colour =
            "#7657A8",

          alpha =
            0.7,

          size =
            2.5
        ) +

        geom_smooth(

          method =
            "lm",

          se =
            TRUE,

          colour =
            "#E67E22"
        ) +

        labs(

          x =
            "Underutilisation (%)",

          y =
            "WPI growth (%)"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # WPI TIMING
  # ==========================================================

  output$wpi_timing_table <-
    renderDT({

      data <-
        wpi_combined_data()


      if (
        nrow(data) == 0
      ) {

        return(
          datatable(
            tibble(
              Message =
                "WPI timing analysis is unavailable."
            ),
            options =
              datatable_options()
          )
        )
      }


      data <-
        data |> 

        arrange(Quarter) |> 

        mutate(

          WPI_growth_lead1 =
            lead(
              WPI_growth,
              1
            ),

          WPI_growth_lead2 =
            lead(
              WPI_growth,
              2
            )
        )


      result <-
        tibble(

          Measure = c(
            "Tightness",
            "Underutilisation"
          ),

          `Same quarter` = c(

            safe_cor(
              data$Tightness,
              data$WPI_growth
            ),

            safe_cor(
              data$underutilisation_rate,
              data$WPI_growth
            )
          ),

          `One quarter ahead` = c(

            safe_cor(
              data$Tightness,
              data$WPI_growth_lead1
            ),

            safe_cor(
              data$underutilisation_rate,
              data$WPI_growth_lead1
            )
          ),

          `Two quarters ahead` = c(

            safe_cor(
              data$Tightness,
              data$WPI_growth_lead2
            ),

            safe_cor(
              data$underutilisation_rate,
              data$WPI_growth_lead2
            )
          )
        ) |> 

        mutate(

          across(
            where(is.numeric),
            ~ round(.x, 3)
          )
        )


      datatable(

        result,

        rownames = FALSE,

        options =
          datatable_options(
            10
          )
      )
    })


  # ==========================================================
  # OCCUPATION DATA REACTIVE
  # ==========================================================

  selected_occupation_data <- reactive({

    if (
      input$occupation_level ==
      "Detailed occupations"
    ) {

      if (
        nrow(ivi_anzsco4) > 0
      ) {

        return(
          ivi_anzsco4
        )
      }
    }


    occupation_data
  })


  # ==========================================================
  # OCCUPATION TREND
  # ==========================================================

  output$occupation_trend <-
    renderPlotly({

      data <-
        selected_occupation_data()


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "Occupation data are unavailable."
          )
        )
      }


      if (
        !"Title" %in%
        names(data)
      ) {

        return(
          empty_plot(
            "Occupation title information is unavailable."
          )
        )
      }


      data <-
        data |> 

        filter(
          !is.na(Month),
          !is.na(Vacancies),
          !is.na(Title)
        ) |> 

        group_by(
          Month,
          Title
        ) |> 

        summarise(

          Vacancies =
            sum(
              Vacancies,
              na.rm = TRUE
            ),

          .groups = "drop"
        )


      if (
        nrow(data) == 0
      ) {

        return(
          empty_plot(
            "No occupation observations available."
          )
        )
      }


      top_titles <-
        data |> 

        group_by(Title) |> 

        summarise(

          total =
            sum(
              Vacancies,
              na.rm = TRUE
            ),

          .groups = "drop"
        ) |> 

        slice_max(
          total,
          n = 10,
          with_ties = FALSE
        ) |> 

        pull(Title)


      plot_data <-
        data |> 

        filter(
          Title %in%
            top_titles
        )


      p <-
        ggplot(

          plot_data,

          aes(

            x = Month,

            y = Vacancies,

            colour = Title
          )
        ) +

        geom_line(
          linewidth = 0.9
        ) +

        scale_colour_manual(
          values =
            rep(
              occupation_palette,
              length.out =
                length(
                  unique(
                    plot_data$Title
                  )
                )
            )
        ) +

        labs(

          x = NULL,

          y = "Vacancies",

          colour = "Occupation"
        ) +

        theme_minimal(
          base_size = 12
        ) +

        theme(
          legend.position =
            "right"
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # OCCUPATION TABLE
  # ==========================================================

  output$occupation_table <-
    renderDT({

      data <-
        selected_occupation_data()


      if (
        nrow(data) == 0 ||
        !"Title" %in%
        names(data)
      ) {

        return(
          datatable(
            tibble(
              Message =
                "Occupation data are unavailable."
            ),
            options =
              datatable_options()
          )
        )
      }


      result <-
        data |> 

        filter(
          !is.na(Title)
        ) |> 

        group_by(
          Title
        ) |> 

        summarise(

          `Total vacancies` =
            sum(
              Vacancies,
              na.rm = TRUE
            ),

          `Average monthly vacancies` =
            mean(
              Vacancies,
              na.rm = TRUE
            ),

          .groups = "drop"
        ) |> 

        arrange(
          desc(
            `Total vacancies`
          )
        ) |> 

        mutate(

          `Total vacancies` =
            round(
              `Total vacancies`,
              0
            ),

          `Average monthly vacancies` =
            round(
              `Average monthly vacancies`,
              1
            )
        )


      datatable(

        result,

        rownames = FALSE,

        options =
          datatable_options(
            10
          )
      )
    })


  # ==========================================================
  # SKILL PLOT
  # ==========================================================

  output$skill_plot <-
    renderPlotly({

      if (
        nrow(ivi_skill) == 0
      ) {

        return(
          empty_plot(
            "Skill-level IVI data are unavailable."
          )
        )
      }


      # Find a likely skill column

      skill_candidates <-
        names(ivi_skill)[

          grepl(
            "skill",
            names(ivi_skill),
            ignore.case = TRUE
          )
        ]


      if (
        length(skill_candidates) == 0
      ) {

        return(
          empty_plot(
            "No skill category column was found."
          )
        )
      }


      skill_col <-
        skill_candidates[1]


      plot_data <-
        ivi_skill |> 

        filter(
          !is.na(Month),
          !is.na(Vacancies),
          !is.na(.data[[skill_col]])
        ) |> 

        group_by(

          Month,

          Skill =
            .data[[skill_col]]
        ) |> 

        summarise(

          Vacancies =
            sum(
              Vacancies,
              na.rm = TRUE
            ),

          .groups = "drop"
        )


      if (
        nrow(plot_data) == 0
      ) {

        return(
          empty_plot(
            "No skill-level observations available."
          )
        )
      }


      p <-
        ggplot(

          plot_data,

          aes(

            x = Month,

            y = Vacancies,

            colour = Skill
          )
        ) +

        geom_line(
          linewidth = 0.9
        ) +

        labs(

          x = NULL,

          y = "Vacancies",

          colour = "Skill"
        ) +

        theme_minimal(
          base_size = 13
        )


      clean_plotly(
        ggplotly(p)
      )
    })


  # ==========================================================
  # DATA EXPLORER
  # ==========================================================

  explorer_data <- reactive({

    if (
      input$explorer_dataset ==
      "Labour-market slack"
    ) {

      return(
        labour_market_slack
      )
    }


    if (
      input$explorer_dataset ==
      "Vacancy and tightness"
    ) {

      return(
        combined_data
      )
    }


    if (
      input$explorer_dataset ==
      "Wage Price Index"
    ) {

      return(
        wpi_data
      )
    }


    if (
      input$explorer_dataset ==
      "IVI occupation data"
    ) {

      return(
        occupation_data
      )
    }


    labour_market_slack
  })


  # ==========================================================
  # DATA EXPLORER
  # ==========================================================

  output$data_explorer <-
    renderDT({

      data <-
        explorer_data()


      if (
        nrow(data) == 0
      ) {

        return(
          datatable(
            tibble(
              Message =
                "No data available for this dataset."
            ),
            options =
              datatable_options()
          )
        )
      }


      datatable(

        data,

        filter = "top",

        rownames = FALSE,

        options = list(

          pageLength =
            input$explorer_rows,

          lengthMenu = list(

            c(
              10,
              25,
              50,
              100,
              250,
              500,
              -1
            ),

            c(
              "10",
              "25",
              "50",
              "100",
              "250",
              "500",
              "All"
            )
          ),

          lengthChange = TRUE,

          searching = TRUE,

          ordering = TRUE,

          paging = TRUE,

          info = TRUE,

          scrollX = TRUE,

          autoWidth = TRUE,

          language = list(

            lengthMenu =
              "Show _MENU_ entries",

            search =
              "Search:",

            info =
              "Showing _START_ to _END_ of _TOTAL_ entries",

            infoEmpty =
              "Showing 0 to 0 of 0 entries",

            infoFiltered =
              "(filtered from _MAX_ total entries)"
          )
        )
      )
    })


  # ==========================================================
  # DOWNLOAD
  # ==========================================================

  output$download_explorer <-
    downloadHandler(

      filename = function() {

        paste0(

          "australian_labour_market_",

          gsub(
            " ",
            "_",
            tolower(
              input$explorer_dataset
            )
          ),

          ".csv"
        )
      },


      content = function(file) {

        write_csv(
          explorer_data(),
          file
        )
      }
    )
}


# ============================================================
# 13. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)