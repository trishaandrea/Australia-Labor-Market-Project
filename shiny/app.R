# ============================================================
# AUSTRALIAN LABOUR MARKET
# TIGHTNESS AND SLACK
# Interactive Shiny Dashboard
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# ============================================================
# 1. LOAD DATA
# ============================================================

data_paths <- c(
  "../data/processed/combined_tightness_slack.csv",
  "data/processed/combined_tightness_slack.csv"
)

existing_paths <- data_paths[file.exists(data_paths)]

if (length(existing_paths) == 0) {
  stop(
    "Could not find combined_tightness_slack.csv. ",
    "Please check that the file is located in data/processed/."
  )
}

combined_data <- read_csv(
  existing_paths[1],
  show_col_types = FALSE
)

# ============================================================
# 2. CHECK REQUIRED COLUMNS
# ============================================================

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
  names(combined_data)
)

if (length(missing_columns) > 0) {

  stop(
    "The following required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )

}

# ============================================================
# 3. CLEAN DATA
# ============================================================

combined_data <- combined_data |>

  mutate(

    Quarter = as.Date(Quarter),

    Job_Vacancies = as.numeric(Job_Vacancies),

    unemployed = as.numeric(unemployed),

    Tightness = as.numeric(Tightness),

    unemployment_rate =
      as.numeric(unemployment_rate),

    underemployment_rate =
      as.numeric(underemployment_rate),

    underutilisation_rate =
      as.numeric(underutilisation_rate)

  ) |>

  arrange(Quarter)


# ============================================================
# 4. MEASURE DEFINITIONS
# ============================================================

measure_choices <- c(

  "Unemployment" =
    "unemployment_rate",

  "Underemployment" =
    "underemployment_rate",

  "Underutilisation" =
    "underutilisation_rate"

)


measure_descriptions <- c(

  unemployment_rate =
    "The unemployment rate measures the percentage of the labour force that is unemployed.",

  underemployment_rate =
    "The underemployment rate measures employed people who want and are available to work more hours.",

  underutilisation_rate =
    "The underutilisation rate combines unemployment and underemployment to provide a broader measure of unused labour capacity."

)


# ============================================================
# 5. UI
# ============================================================

ui <- fluidPage(

  tags$head(

    tags$style(HTML("

      /* ---------------------------------------------------
         GENERAL
      --------------------------------------------------- */

      body {

        background:
          linear-gradient(
            135deg,
            #f8fafc 0%,
            #eef2f7 100%
          );

        font-family:
          -apple-system,
          BlinkMacSystemFont,
          'Segoe UI',
          sans-serif;

        color: #1f2937;

      }


      .container-fluid {

        max-width: 1500px;

        padding:
          28px 35px 50px 35px;

      }


      /* ---------------------------------------------------
         HEADER
      --------------------------------------------------- */

      .dashboard-header {

        background:
          linear-gradient(
            135deg,
            #111827,
            #26364d
          );

        color: white;

        padding: 30px 34px;

        border-radius: 18px;

        margin-bottom: 24px;

        box-shadow:
          0 8px 25px
          rgba(15, 23, 42, 0.12);

      }


      .dashboard-header h1 {

        margin-top: 0;

        font-size: 30px;

        font-weight: 700;

        letter-spacing: -0.5px;

      }


      .dashboard-header p {

        margin-bottom: 0;

        color: #dbe4ef;

        font-size: 15px;

      }


      /* ---------------------------------------------------
         SIDEBAR
      --------------------------------------------------- */

      .sidebar {

        background: white;

        border:
          1px solid #e5e7eb;

        border-radius: 16px;

        padding: 22px;

        box-shadow:
          0 4px 14px
          rgba(15, 23, 42, 0.05);

      }


      .sidebar h4 {

        font-weight: 700;

        margin-top: 0;

        color: #111827;

      }


      .control-label {

        font-weight: 600;

        color: #374151;

      }


      .form-control {

        border-radius: 9px;

        border:
          1px solid #d1d5db;

      }


      .btn-reset {

        width: 100%;

        border-radius: 9px;

        margin-top: 5px;

      }


      /* ---------------------------------------------------
         CARDS
      --------------------------------------------------- */

      .dashboard-card {

        background: white;

        border:
          1px solid #e5e7eb;

        border-radius: 16px;

        padding: 22px;

        margin-bottom: 20px;

        box-shadow:
          0 4px 14px
          rgba(15, 23, 42, 0.05);

      }


      /* ---------------------------------------------------
         KPI CARDS
      --------------------------------------------------- */

      .kpi-card {

        background: white;

        border:
          1px solid #e5e7eb;

        border-radius: 15px;

        padding: 20px;

        min-height: 135px;

        margin-bottom: 20px;

        box-shadow:
          0 4px 14px
          rgba(15, 23, 42, 0.05);

        transition:
          transform 0.2s ease,
          box-shadow 0.2s ease;

      }


      .kpi-card:hover {

        transform:
          translateY(-2px);

        box-shadow:
          0 8px 20px
          rgba(15, 23, 42, 0.09);

      }


      .kpi-label {

        color: #6b7280;

        font-size: 13px;

        font-weight: 600;

        text-transform: uppercase;

        letter-spacing: 0.5px;

      }


      .kpi-value {

        color: #111827;

        font-size: 30px;

        font-weight: 700;

        margin-top: 8px;

      }


      .kpi-description {

        color: #9ca3af;

        font-size: 12px;

        margin-top: 5px;

      }


      /* ---------------------------------------------------
         SECTION HEADINGS
      --------------------------------------------------- */

      .section-title {

        font-size: 19px;

        font-weight: 700;

        color: #111827;

        margin-top: 0;

      }


      .section-description {

        color: #6b7280;

        font-size: 13px;

        margin-bottom: 15px;

      }


      /* ---------------------------------------------------
         INFO BOX
      --------------------------------------------------- */

      .info-box {

        background: #f8fafc;

        border:
          1px solid #e2e8f0;

        border-radius: 12px;

        padding: 16px;

        font-size: 13px;

        line-height: 1.6;

        color: #475569;

      }


      /* ---------------------------------------------------
         INTERPRETATION
      --------------------------------------------------- */

      .interpretation-box {

        background:
          linear-gradient(
            135deg,
            #f8fafc,
            #eef2f7
          );

        border-radius: 12px;

        padding: 18px;

        border:
          1px solid #e2e8f0;

      }


      .interpretation-value {

        font-size: 17px;

        font-weight: 600;

        color: #111827;

        margin-bottom: 5px;

      }


      /* ---------------------------------------------------
         TABLE
      --------------------------------------------------- */

      table.dataTable thead th {

        background: #f8fafc !important;

        color: #374151 !important;

      }


      /* ---------------------------------------------------
         MOBILE
      --------------------------------------------------- */

      @media(max-width: 768px) {

        .container-fluid {

          padding:
            15px;

        }

        .dashboard-header {

          padding: 22px;

        }

        .dashboard-header h1 {

          font-size: 23px;

        }

      }

    "))

  ),


  # ========================================================
  # PAGE
  # ========================================================

  div(

    class = "container-fluid",


    # ------------------------------------------------------
    # HEADER
    # ------------------------------------------------------

    div(

      class = "dashboard-header",

      h1(
        "Australian Labour Market Tightness and Slack"
      ),

      p(
        "Interactive exploration of labour-market conditions, vacancy tightness and slack in Australia."
      )

    ),


    # ------------------------------------------------------
    # LAYOUT
    # ------------------------------------------------------

    sidebarLayout(


      # ====================================================
      # SIDEBAR
      # ====================================================

      sidebarPanel(

        class = "sidebar",


        h4("Explore the data"),


        selectInput(

          inputId = "measure",

          label =
            "Labour-market slack measure",

          choices =
            measure_choices,

          selected =
            "underutilisation_rate"

        ),


        dateRangeInput(

          inputId = "date_range",

          label =
            "Analysis period",

          start =
            min(
              combined_data$Quarter,
              na.rm = TRUE
            ),

          end =
            max(
              combined_data$Quarter,
              na.rm = TRUE
            ),

          min =
            min(
              combined_data$Quarter,
              na.rm = TRUE
            ),

          max =
            max(
              combined_data$Quarter,
              na.rm = TRUE
            ),

          format = "yyyy-mm-dd",

          separator = " to "

        ),


        actionButton(

          inputId = "reset",

          label = "Reset filters",

          icon =
            icon("refresh"),

          class =
            "btn btn-default btn-reset"

        ),


        br(),
        br(),


        div(

          class = "info-box",

          strong("About the selected measure"),

          br(),
          br(),

          textOutput(
            "measure_description"
          )

        ),


        br(),

        # ------------------------------------------------------
        # LABOUR DEMAND SNAPSHOT
        # ------------------------------------------------------
        
        h4("Labour Demand Snapshot"),
        
        strong("Latest job vacancies"),
        
        br(),
        
        textOutput(
          "latest_vacancies"
        ),
        
        br(),
        br(),
        div(

          class = "info-box",

          strong("Research focus"),

          br(),
          br(),

          "This dashboard explores the association between job vacancies, labour-market tightness and labour-market slack. The analysis describes relationships in the data and does not establish causality."

        )

      ),


      # ====================================================
      # MAIN PANEL
      # ====================================================

      mainPanel(


        # --------------------------------------------------
        # KPI ROW
        # --------------------------------------------------

        fluidRow(

          column(

            width = 4,

            div(

              class = "kpi-card",

              div(
                class = "kpi-label",
                "Average slack"
              ),

              div(
                class = "kpi-value",
                textOutput(
                  "average_slack"
                )
              ),

              div(
                class = "kpi-description",
                "Selected measure across the chosen period"
              )

            )

          ),


          column(

            width = 4,

            div(

              class = "kpi-card",

              div(
                class = "kpi-label",
                "Average tightness"
              ),

              div(
                class = "kpi-value",
                textOutput(
                  "average_tightness"
                )
              ),

              div(
                class = "kpi-description",
                "Job vacancies relative to unemployed persons"
              )

            )

          ),


          column(

            width = 4,

            div(

              class = "kpi-card",

              div(
                class = "kpi-label",
                "Pearson correlation"
              ),

              div(
                class = "kpi-value",
                textOutput(
                  "correlation"
                )
              ),

              div(
                class = "kpi-description",
                "Tightness vs selected slack"
              )

            )

          )

        ),

        # --------------------------------------------------
        # JOB VACANCIES OVER TIME
        # --------------------------------------------------
        
        div(
          
          class = "dashboard-card",
          
          h3(
            class = "section-title",
            "Job Vacancies Over Time"
          ),
          
          p(
            class = "section-description",
            "Quarterly job vacancies across the selected analysis period."
          ),
          
          plotlyOutput(
            "vacancies_plot",
            height = "500px"
          )
          
        ),

        # --------------------------------------------------
        # TIME SERIES
        # --------------------------------------------------

        div(

          class = "dashboard-card",

          h3(

            class = "section-title",

            textOutput(
              "time_title"
            )

          ),

          p(

            class = "section-description",

            "Hover over observations for exact values. Use the Plotly controls to zoom, pan and reset the chart."

          ),

          plotlyOutput(

            "time_plot",

            height = "500px"

          )

        ),


        # --------------------------------------------------
        # RELATIONSHIP
        # --------------------------------------------------

        div(

          class = "dashboard-card",

          h3(

            class = "section-title",

            textOutput(
              "relationship_title"
            )

          ),

          p(

            class = "section-description",

            "The fitted line summarises the linear association within the selected period."

          ),

          plotlyOutput(

            "relationship_plot",

            height = "500px"

          )

        ),


        # --------------------------------------------------
        # INTERPRETATION
        # --------------------------------------------------

        div(

          class = "dashboard-card",

          h3(

            class = "section-title",

            "What does the relationship show?"

          ),

          div(

            class = "interpretation-box",

            div(

              class =
                "interpretation-value",

              textOutput(
                "relationship_summary"
              )

            ),

            p(

              class =
                "section-description",

              "Correlation describes the direction and strength of the linear association between the two variables. It should not be interpreted as evidence of a causal relationship."

            )

          )

        ),


        # --------------------------------------------------
        # DATA TABLE
        # --------------------------------------------------

        div(

          class = "dashboard-card",

          h3(

            class = "section-title",

            "Explore the underlying observations"

          ),

          p(

            class = "section-description",

            "Search, sort and filter the quarterly observations."

          ),

          DTOutput(
            "summary_table"
          ),

          br(),

          downloadButton(

            outputId =
              "download_data",

            label =
              "Download filtered data",

            class =
              "btn btn-default"

          )

        )

      )

    )

  )

)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # SELECTED MEASURE
  # ==========================================================

  selected_measure <- reactive({

    req(input$measure)

    input$measure

  })


  # ==========================================================
  # SELECTED MEASURE LABEL
  # ==========================================================

  selected_measure_label <- reactive({

    measure <-
      selected_measure()

    names(
      measure_choices[
        measure_choices == measure
      ]
    )

  })


  # ==========================================================
  # FILTERED DATA
  # ==========================================================

  filtered_data <- reactive({

    req(
      input$date_range
    )

    combined_data |>

      filter(

        Quarter >=
          input$date_range[1],

        Quarter <=
          input$date_range[2]

      )

  })


  # ==========================================================
  # RESET
  # ==========================================================

  observeEvent(

    input$reset,

    {

      updateSelectInput(

        session,

        "measure",

        selected =
          "underutilisation_rate"

      )


      updateDateRangeInput(

        session,

        "date_range",

        start =
          min(
            combined_data$Quarter,
            na.rm = TRUE
          ),

        end =
          max(
            combined_data$Quarter,
            na.rm = TRUE
          )

      )

    }

  )

  # ==========================================================
  # LATEST JOB VACANCIES
  # ==========================================================
  
  output$latest_vacancies <- renderText({
    
    data <-
      filtered_data()
    
    latest <-
      data |>
      filter(
        !is.na(Job_Vacancies)
      ) |>
      slice_tail(
        n = 1
      )
    
    if (nrow(latest) == 0) {
      
      return("N/A")
      
    }
    
    paste0(
      round(
        latest$Job_Vacancies,
        1
      ),
      " thousand"
    )
    
  })
  
  
  # ==========================================================
  # AVERAGE SLACK
  # ==========================================================

  output$average_slack <- renderText({

    data <-
      filtered_data()

    measure <-
      selected_measure()

    value <-
      mean(
        data[[measure]],
        na.rm = TRUE
      )

    if (is.nan(value)) {

      return("N/A")

    }

    paste0(
      round(
        value,
        2
      ),
      "%"
    )

  })


  # ==========================================================
  # AVERAGE TIGHTNESS
  # ==========================================================

  output$average_tightness <- renderText({

    data <-
      filtered_data()

    value <-
      mean(
        data$Tightness,
        na.rm = TRUE
      )

    if (is.nan(value)) {

      return("N/A")

    }

    round(
      value,
      3
    )

  })


  # ==========================================================
  # CORRELATION
  # ==========================================================

  correlation_value <- reactive({

    data <-
      filtered_data()

    measure <-
      selected_measure()

    valid_data <-
      data |>

      select(
        Tightness,
        all_of(measure)
      ) |>

      drop_na()


    if (
      nrow(valid_data) < 2
    ) {

      return(NA_real_)

    }


    cor(

      valid_data$Tightness,

      valid_data[[measure]],

      method = "pearson"

    )

  })


  output$correlation <- renderText({

    value <-
      correlation_value()

    if (is.na(value)) {

      return("N/A")

    }

    sprintf(
      "%.3f",
      value
    )

  })


  # ==========================================================
  # DESCRIPTION
  # ==========================================================

  output$measure_description <- renderText({

    measure <-
      selected_measure()

    measure_descriptions[[measure]]

  })


  # ==========================================================
  # TITLES
  # ==========================================================

  output$time_title <- renderText({

    paste(
      selected_measure_label(),
      "over time"
    )

  })


  output$relationship_title <- renderText({

    paste(

      "Labour-market tightness vs",

      selected_measure_label()

    )

  })


  # ==========================================================
  # RELATIONSHIP SUMMARY
  # ==========================================================

  output$relationship_summary <- renderText({

    r <-
      correlation_value()

    label <-
      selected_measure_label()


    if (is.na(r)) {

      return(
        "There are insufficient observations for a correlation."
      )

    }


    strength <- case_when(

      abs(r) >= 0.7 ~
        "a strong",

      abs(r) >= 0.4 ~
        "a moderate",

      abs(r) >= 0.2 ~
        "a weak",

      TRUE ~
        "a very weak"

    )


    direction <- if_else(

      r < 0,

      "negative",

      "positive"

    )


    paste0(

      "The selected period shows ",

      strength,

      " ",

      direction,

      " linear association between labour-market tightness and ",

      tolower(label),

      " (r = ",

      round(
        r,
        3
      ),

      ")."

    )

  })

  # ==========================================================
  # JOB VACANCIES PLOT
  # ==========================================================
  
  output$vacancies_plot <- renderPlotly({
    
    data <-
      filtered_data() |>
      filter(
        !is.na(Job_Vacancies)
      )
    
    p <-
      ggplot(
        data,
        aes(
          x = Quarter,
          y = Job_Vacancies,
          text = paste0(
            "<b>", Quarter, "</b>",
            "<br>Job vacancies: ",
            round(Job_Vacancies, 1),
            " thousand"
          )
        )
      ) +
      
      geom_line(
        linewidth = 1.1,
        colour = "#2563eb"
      ) +
      
      geom_point(
        size = 2.5,
        colour = "#2563eb"
      ) +
      
      labs(
        x = NULL,
        y = "Job vacancies ('000)"
      ) +
      
      theme_minimal(
        base_size = 14
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    ggplotly(
      p,
      tooltip = "text"
    ) |>
      
      layout(
        hovermode = "x unified"
      ) |>
      
      config(
        displaylogo = FALSE
      )
    
  })
  
  # ==========================================================
  # TIME SERIES PLOT
  # ==========================================================

  output$time_plot <- renderPlotly({

    data <-
      filtered_data()

    measure <-
      selected_measure()

    label <-
      selected_measure_label()


    plot_data <-
      data |>

      select(

        Quarter,

        value =
          all_of(measure)

      ) |>

      drop_na()


    p <-
      ggplot(

        plot_data,

        aes(

          x =
            Quarter,

          y =
            value,

          text =
            paste0(

              "<b>",
              format(
                Quarter,
                "%Y Q",
              ),
              "</b>",

              "<br>",

              label,

              ": ",

              round(
                value,
                2
              ),

              "%"

            )

        )

      ) +


      geom_line(

        linewidth =
          1.1,

        colour =
          "#2563eb"

      ) +


      geom_point(

        size =
          2.5,

        colour =
          "#2563eb"

      ) +


      labs(

        x =
          NULL,

        y =
          paste0(
            label,
            " (%)"
          )

      ) +


      theme_minimal(

        base_size =
          14

      ) +


      theme(

        panel.grid.minor =
          element_blank(),

        panel.grid.major.x =
          element_blank()

      )


    ggplotly(

      p,

      tooltip =
        "text"

    ) |>

      layout(

        hovermode =
          "x unified",

        margin =
          list(
            l = 60,
            r = 30,
            t = 20,
            b = 50
          ),

        xaxis =
          list(
            rangeslider =
              list(
                visible = TRUE
              ),

            type =
              "date"
          )

      ) |>

      config(

        displaylogo =
          FALSE,

        modeBarButtonsToRemove =
          c(
            "lasso2d",
            "select2d"
          )

      )

  })


  # ==========================================================
  # RELATIONSHIP PLOT
  # ==========================================================

  output$relationship_plot <- renderPlotly({

    data <-
      filtered_data()

    measure <-
      selected_measure()

    label <-
      selected_measure_label()


    plot_data <-
      data |>

      select(

        Quarter,

        Tightness,

        value =
          all_of(measure)

      ) |>

      drop_na()


    p <-
      ggplot(

        plot_data,

        aes(

          x =
            Tightness,

          y =
            value,

          text =
            paste0(

              "<b>",
              format(
                Quarter,
                "%Y Q"
              ),
              "</b>",

              "<br>Tightness: ",

              round(
                Tightness,
                3
              ),

              "<br>",

              label,

              ": ",

              round(
                value,
                2
              ),

              "%"

            )

        )

      ) +


      geom_point(

        size =
          3.5,

        alpha =
          0.75,

        colour =
          "#7c3aed"

      ) +


      geom_smooth(

        method =
          "lm",

        se =
          TRUE,

        colour =
          "#111827",

        fill =
          "#cbd5e1"

      ) +


      labs(

        x =
          "Labour-market tightness",

        y =
          paste0(
            label,
            " (%)"
          )

      ) +


      theme_minimal(

        base_size =
          14

      ) +


      theme(

        panel.grid.minor =
          element_blank()

      )


    ggplotly(

      p,

      tooltip =
        "text"

    ) |>

      layout(

        hovermode =
          "closest",

        margin =
          list(
            l = 60,
            r = 30,
            t = 20,
            b = 50
          )

      ) |>

      config(

        displaylogo =
          FALSE,

        modeBarButtonsToRemove =
          c(
            "lasso2d",
            "select2d"
          )

      )

  })


  # ==========================================================
  # DATA TABLE
  # ==========================================================

  output$summary_table <- renderDT({

    data <-
      filtered_data()

    measure <-
      selected_measure()

    label <-
      selected_measure_label()


    table_data <-
      data |>

      select(

        Quarter,

        Job_Vacancies,

        unemployed,

        Tightness,

        all_of(measure)

      ) |>

      rename(

        Quarter =
          Quarter,

        `Job Vacancies` =
          Job_Vacancies,

        Unemployed =
          unemployed,

        Tightness =
          Tightness

      )


    names(
      table_data
    )[5] <-
      label


    datatable(

      table_data,

      rownames =
        FALSE,

      filter =
        "top",

      options =
        list(

          pageLength =
            10,

          lengthMenu =
            c(
              5,
              10,
              25,
              50
            ),

          scrollX =
            TRUE,

          autoWidth =
            TRUE

        )

    )

  })


  # ==========================================================
  # DOWNLOAD
  # ==========================================================

  output$download_data <-
    downloadHandler(

      filename =
        function() {

          paste0(

            "labour_market_filtered_",

            Sys.Date(),

            ".csv"

          )

        },


      content =
        function(file) {

          write_csv(

            filtered_data(),

            file

          )

        }

    )

}


# ============================================================
# RUN APP
# ============================================================

shinyApp(

  ui =
    ui,

  server =
    server

)