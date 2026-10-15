# ============================================================
# AUSTRALIAN LABOUR MARKET DASHBOARD
# Save this entire script as shiny/app.R
# ============================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)


# ============================================================
# 1. SETTINGS
# ============================================================

slack_columns <- c(
  "Unemployment" = "unemployment_rate",
  "Underemployment" = "underemployment_rate",
  "Underutilisation" = "underutilisation_rate"
)

change_columns <- c(
  "Unemployment" = "change_unemployment",
  "Underemployment" = "change_underemployment",
  "Underutilisation" = "change_underutilisation"
)

slack_palette <- c(
  "Unemployment" = "#2774C6",
  "Underemployment" = "#E67E22",
  "Underutilisation" = "#087F8C"
)

skill_palette <- c(
  "Skill level 1" = "#12355B",
  "Skill level 2" = "#2774C6",
  "Skill level 3" = "#087F8C",
  "Skill level 4" = "#E67E22",
  "Skill level 5" = "#7657A8"
)

state_names <- c(
  "AUST" = "Australia",
  "NSW" = "New South Wales",
  "VIC" = "Victoria",
  "QLD" = "Queensland",
  "SA" = "South Australia",
  "WA" = "Western Australia",
  "TAS" = "Tasmania",
  "NT" = "Northern Territory",
  "ACT" = "Australian Capital Territory"
)

period_labels <- c(
  "1978–1989",
  "1990–1994",
  "1995–2007",
  "2008–2019",
  "2020–2021",
  "2022–present"
)

time_choices <- c(
  "All available data",
  "Last 10 years",
  "Last 5 years",
  "Last 3 years"
)

# Set these to exact CSV column names if automatic detection fails.
WPI_DATE_COLUMN <- NULL
WPI_INDEX_COLUMN <- NULL


# ============================================================
# 2. DATA HELPERS
# ============================================================

find_file <- function(filename) {
  paths <- c(
    file.path("data", "processed", filename),
    file.path("..", "data", "processed", filename),
    file.path("..", "..", "data", "processed", filename)
  )

  found <- paths[file.exists(paths)]
  if (!length(found)) return(NA_character_)
  found[[1]]
}

read_project_data <- function(filename) {
  path <- find_file(filename)

  if (is.na(path)) {
    warning("Missing data file: ", filename)
    return(tibble())
  }

  tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) {
      warning(filename, ": ", conditionMessage(e))
      tibble()
    }
  )
}

parse_dates <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  as.Date(
    suppressWarnings(
      parse_date_time(
        as.character(x),
        orders = c(
          "ymd", "dmy", "mdy",
          "ym", "my", "b Y", "B Y",
          "Y q", "Y-Q"
        )
      )
    )
  )
}

parse_numbers <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(readr::parse_number(as.character(x)))
}

normalise_state <- function(x) {
  key <- str_to_upper(str_squish(as.character(x)))

  lookup <- c(
    "AUSTRALIA" = "AUST",
    "AUSTRALIA TOTAL" = "AUST",
    "AUSTRALIAN TOTAL" = "AUST",
    "NATIONAL" = "AUST",
    "AUS" = "AUST",
    "AUST" = "AUST",
    "NEW SOUTH WALES" = "NSW",
    "VICTORIA" = "VIC",
    "QUEENSLAND" = "QLD",
    "SOUTH AUSTRALIA" = "SA",
    "WESTERN AUSTRALIA" = "WA",
    "TASMANIA" = "TAS",
    "NORTHERN TERRITORY" = "NT",
    "AUSTRALIAN CAPITAL TERRITORY" = "ACT"
  )

  mapped <- unname(lookup[key])
  if_else(is.na(mapped), key, mapped)
}

state_label <- function(code) {
  label <- unname(state_names[code])
  if (length(label) == 1 && !is.na(label)) label else code
}

historical_period <- function(date) {
  factor(
    case_when(
      date < as.Date("1990-01-01") ~ period_labels[1],
      date < as.Date("1995-01-01") ~ period_labels[2],
      date < as.Date("2008-01-01") ~ period_labels[3],
      date < as.Date("2020-01-01") ~ period_labels[4],
      date < as.Date("2022-01-01") ~ period_labels[5],
      TRUE ~ period_labels[6]
    ),
    levels = period_labels
  )
}

quarter_change <- function(x, date) {
  month_id <- year(date) * 12 + month(date)

  if_else(
    month_id - lag(month_id) == 3,
    x - lag(x),
    NA_real_
  )
}

filter_window <- function(data, date_column, window) {
  if (!nrow(data) || window == "All available data") {
    return(data)
  }

  years_back <- switch(
    window,
    "Last 10 years" = 10,
    "Last 5 years" = 5,
    "Last 3 years" = 3,
    NA_real_
  )

  if (is.na(years_back)) return(data)

  latest <- max(data[[date_column]], na.rm = TRUE)

  data |>
    filter(
      .data[[date_column]] >=
        latest %m-% years(years_back)
    )
}

kpi_summary <- function(
  data,
  date_column,
  value_column,
  change_type = "difference",
  digits = 1,
  value_suffix = "",
  change_suffix = ""
) {
  empty <- list(
    value = "Unavailable",
    change = "Year-on-year comparison unavailable",
    date = ""
  )

  if (
    !nrow(data) ||
    !all(c(date_column, value_column) %in% names(data))
  ) {
    return(empty)
  }

  d <- data |>
    transmute(
      date = .data[[date_column]],
      value = .data[[value_column]]
    ) |>
    filter(!is.na(date), is.finite(value)) |>
    arrange(date)

  if (!nrow(d)) return(empty)

  latest_date <- tail(d$date, 1)
  latest <- tail(d$value, 1)

  result <- list(
    value = paste0(
      scales::number(
        latest,
        accuracy = 10^(-digits),
        big.mark = ","
      ),
      value_suffix
    ),
    change = "Year-on-year comparison unavailable",
    date = format(latest_date, "%b %Y")
  )

  month_id <- year(d$date) * 12 + month(d$date)
  target <- year(latest_date) * 12 + month(latest_date) - 12
  prior_values <- d$value[month_id == target]

  if (length(prior_values) != 1) return(result)

  prior <- prior_values[[1]]

  if (change_type == "percent") {
    if (prior == 0) return(result)
    change <- (latest / prior - 1) * 100
    suffix <- "%"
  } else {
    change <- latest - prior
    suffix <- change_suffix
  }

  arrow <- if (change > 0) {
    "▲"
  } else if (change < 0) {
    "▼"
  } else {
    "—"
  }

  result$change <- paste0(
    arrow,
    " ",
    scales::number(
      abs(change),
      accuracy = 10^(-max(digits, 1))
    ),
    suffix,
    " vs a year ago"
  )

  result
}

correlation_summary <- function(data, x_column) {
  map_dfr(names(change_columns), function(measure) {
    y_column <- change_columns[[measure]]

    ok <- is.finite(data[[x_column]]) &
      is.finite(data[[y_column]])

    x <- data[[x_column]][ok]
    y <- data[[y_column]][ok]

    r <- if (
      length(x) >= 3 &&
      sd(x) > 0 &&
      sd(y) > 0
    ) {
      cor(x, y)
    } else {
      NA_real_
    }

    tibble(
      Measure = measure,
      Observations = length(x),
      `Pearson correlation` = round(r, 3),
      Direction = case_when(
        is.na(r) ~ "Unavailable",
        r < 0 ~ "Negative",
        r > 0 ~ "Positive",
        TRUE ~ "Zero"
      )
    )
  })
}


# ============================================================
# 3. PLOT AND UI HELPERS
# ============================================================

chart_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      axis.title = element_text(colour = "#12355B"),
      strip.text = element_text(
        face = "bold",
        colour = "#12355B"
      ),
      plot.margin = margin(10, 15, 10, 15)
    )
}

as_interactive <- function(p) {
  ggplotly(p) |>
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 90, r = 30, t = 75, b = 65),
      font = list(
        family = "Arial",
        size = 13,
        color = "#263648"
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.12,
        xanchor = "left",
        yanchor = "bottom"
      )
    ) |>
    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}

shade_events <- function(p, enabled) {
  if (!isTRUE(enabled)) return(p)

  events <- tibble(
    start = as.Date(c("1990-01-01", "2020-03-01")),
    end = as.Date(c("1992-12-31", "2021-12-31"))
  )

  p +
    geom_rect(
      data = events,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = "#718096",
      alpha = 0.12
    )
}

scatter_chart <- function(data, x, y, x_label, y_label) {
  data <- data |>
    filter(
      is.finite(.data[[x]]),
      is.finite(.data[[y]])
    )

  validate(
    need(nrow(data) >= 3, "Not enough matched observations.")
  )

  p <- ggplot(
    data,
    aes(x = .data[[x]], y = .data[[y]])
  ) +
    geom_point(
      colour = "#2774C6",
      alpha = 0.65,
      size = 2.3
    ) +
    labs(x = x_label, y = y_label) +
    chart_theme()

  if (n_distinct(data[[x]]) > 1) {
    p <- p +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        colour = "#E67E22",
        fill = "#E67E22",
        alpha = 0.15
      )
  }

  as_interactive(p)
}

make_table <- function(data, page_length = 10) {
  datatable(
    data,
    rownames = FALSE,
    options = list(
      pageLength = page_length,
      scrollX = TRUE
    )
  )
}

plot_card <- function(title, id, height = "480px") {
  div(
    class = "dashboard-card",
    h3(title),
    div(
      class = "plotly-frame",
      plotlyOutput(
        outputId = id,
        width = "100%",
        height = height
      )
    )
  )
}

table_card <- function(title, id) {
  div(
    class = "dashboard-card",
    h3(title),
    DTOutput(id)
  )
}


# ============================================================
# 4. LOAD DATA
# ============================================================

slack <- read_project_data("labour_market_slack.csv")
combined <- read_project_data("combined_tightness_slack.csv")
wpi <- read_project_data("labour_market_wpi.csv")
ivi_broad <- read_project_data("ivi_anzsco2_clean.csv")
ivi_detailed <- read_project_data("ivi_anzsco4_clean.csv")
ivi_skill <- read_project_data("ivi_skill_clean.csv")


# ============================================================
# 5. PREPARE SLACK DATA
# ============================================================

required_slack <- c("date", unname(slack_columns))

if (
  nrow(slack) > 0 &&
  all(required_slack %in% names(slack))
) {
  slack <- slack |>
    mutate(
      date = parse_dates(date),
      across(
        all_of(unname(slack_columns)),
        parse_numbers
      )
    ) |>
    filter(!is.na(date)) |>
    arrange(date) |>
    mutate(period = historical_period(date))
} else {
  slack <- tibble()
}


# ============================================================
# 6. PREPARE QUARTERLY DATA
# ============================================================

required_combined <- c(
  "Quarter",
  "Job_Vacancies",
  "Tightness",
  unname(slack_columns)
)

if (
  nrow(combined) > 0 &&
  all(required_combined %in% names(combined))
) {
  combined <- combined |>
    mutate(
      Quarter = parse_dates(Quarter),
      across(
        all_of(setdiff(required_combined, "Quarter")),
        parse_numbers
      )
    ) |>
    filter(!is.na(Quarter)) |>
    arrange(Quarter)

  if (anyDuplicated(combined$Quarter)) {
    warning("Duplicate quarters in the combined dataset.")
    combined <- tibble()
  } else {
    combined <- combined |>
      mutate(
        period = historical_period(Quarter),
        change_vacancies = quarter_change(
          Job_Vacancies, Quarter
        ),
        change_tightness = quarter_change(
          Tightness, Quarter
        ),
        change_unemployment = quarter_change(
          unemployment_rate, Quarter
        ),
        change_underemployment = quarter_change(
          underemployment_rate, Quarter
        ),
        change_underutilisation = quarter_change(
          underutilisation_rate, Quarter
        )
      )
  }
} else {
  combined <- tibble()
}


# ============================================================
# 7. PREPARE IVI DATA
# ============================================================

prepare_ivi <- function(data) {
  if (!nrow(data)) return(tibble())

  if (
    "state" %in% names(data) &&
    !"State" %in% names(data)
  ) {
    data <- rename(data, State = state)
  }

  if (
    "ANZSCO_TITLE" %in% names(data) &&
    !"Title" %in% names(data)
  ) {
    data <- rename(data, Title = ANZSCO_TITLE)
  }

  if (!all(c("Month", "Vacancies") %in% names(data))) {
    return(tibble())
  }

  data <- data |>
    mutate(
      Month = floor_date(parse_dates(Month), "month"),
      Vacancies = parse_numbers(Vacancies)
    ) |>
    filter(!is.na(Month)) |>
    arrange(Month)

  if ("State" %in% names(data)) {
    data <- data |>
      mutate(State = normalise_state(State))
  }

  data
}

ivi_broad <- prepare_ivi(ivi_broad)
ivi_detailed <- prepare_ivi(ivi_detailed)
ivi_skill <- prepare_ivi(ivi_skill)


# National IVI

national_ivi <- tibble()

if (
  nrow(ivi_broad) > 0 &&
  all(
    c("Level", "ANZSCO_CODE", "State") %in%
      names(ivi_broad)
  )
) {
  national_ivi <- ivi_broad |>
    filter(
      as.character(Level) == "1",
      as.character(ANZSCO_CODE) == "0",
      State == "AUST"
    ) |>
    select(Month, Vacancies) |>
    arrange(Month)

  if (anyDuplicated(national_ivi$Month)) {
    warning("Duplicate national IVI months.")
    national_ivi <- tibble()
  } else if (nrow(national_ivi) > 0) {
    national_ivi <- national_ivi |>
      complete(
        Month = seq.Date(
          min(Month),
          max(Month),
          by = "month"
        )
      ) |>
      arrange(Month) |>
      mutate(
        yoy_change = if_else(
          is.finite(lag(Vacancies, 12)) &
            lag(Vacancies, 12) > 0,
          (Vacancies / lag(Vacancies, 12) - 1) * 100,
          NA_real_
        ),
        ma12 = as.numeric(
          stats::filter(
            Vacancies,
            rep(1 / 12, 12),
            sides = 1
          )
        )
      )
  }
}


# State IVI: fixed January 2019 baseline

state_ivi <- tibble()

if (
  nrow(ivi_broad) > 0 &&
  all(
    c("Level", "ANZSCO_CODE", "State") %in%
      names(ivi_broad)
  )
) {
  state_ivi <- ivi_broad |>
    filter(
      as.character(Level) == "1",
      as.character(ANZSCO_CODE) == "0",
      State != "AUST"
    ) |>
    select(State, Month, Vacancies) |>
    arrange(State, Month)

  duplicates <- state_ivi |>
    count(State, Month) |>
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    warning("Duplicate state IVI observations.")
    state_ivi <- tibble()
  } else {
    state_ivi <- state_ivi |>
      group_by(State) |>
      mutate(
        baseline = {
          values <- Vacancies[
            Month == as.Date("2019-01-01")
          ]

          if (length(values) == 1) {
            values[[1]]
          } else {
            NA_real_
          }
        },
        Index = if_else(
          is.finite(baseline) & baseline > 0,
          Vacancies / baseline * 100,
          NA_real_
        )
      ) |>
      ungroup()
  }
}


# Broad occupations

broad_occupations <- tibble()

if (
  nrow(ivi_broad) > 0 &&
  all(c("Level", "Title") %in% names(ivi_broad))
) {
  broad_occupations <- ivi_broad |>
    filter(as.character(Level) == "2")
}


# Detailed occupations

if (
  nrow(ivi_detailed) > 0 &&
  "Title" %in% names(ivi_detailed)
) {
  ivi_detailed <- ivi_detailed |>
    filter(
      !is.na(Title),
      !str_detect(
        Title,
        regex("total", ignore_case = TRUE)
      )
    )

  if ("ANZSCO_CODE" %in% names(ivi_detailed)) {
    ivi_detailed <- ivi_detailed |>
      filter(
        !is.na(ANZSCO_CODE),
        !as.character(ANZSCO_CODE) %in% c(".", "")
      )
  }
}


# Skill levels

skill_data_message <- "Skill-level data are unavailable."

if (
  nrow(ivi_skill) > 0 &&
  "State" %in% names(ivi_skill)
) {
  candidates <- c(
    "Skill_level",
    "Skill_Level",
    "skill_level",
    "Skill",
    "Title"
  )

  candidates <- candidates[
    candidates %in% names(ivi_skill)
  ]

  if (length(candidates)) {
    scores <- vapply(
      candidates,
      function(column) {
        sum(
          !is.na(
            str_extract(
              as.character(ivi_skill[[column]]),
              "(?<![0-9])[1-5](?![0-9])"
            )
          )
        )
      },
      numeric(1)
    )

    skill_column <- candidates[[which.max(scores)]]

    ivi_skill <- ivi_skill |>
      mutate(
        skill_number = as.integer(
          str_extract(
            as.character(.data[[skill_column]]),
            "(?<![0-9])[1-5](?![0-9])"
          )
        ),
        Skill = factor(
          paste("Skill level", skill_number),
          levels = paste("Skill level", 1:5)
        )
      ) |>
      filter(
        !is.na(skill_number),
        !is.na(State)
      ) |>
      arrange(State, skill_number, Month)

    if (!nrow(ivi_skill)) {
      skill_data_message <- paste(
        "No skill levels 1–5 were recognised.",
        "Check the skill-level labels in the CSV."
      )
    }
  } else {
    skill_data_message <- "No skill-level column was found."
    ivi_skill <- tibble()
  }
} else {
  skill_data_message <- paste(
    "Skill data are missing or have no State column.",
    "Check ivi_skill_clean.csv."
  )
  ivi_skill <- tibble()
}


# ============================================================
# 8. MARKET CONDITIONS
# ============================================================

conditions <- tibble()

if (nrow(combined) > 0) {
  valid_conditions <- combined |>
    filter(
      is.finite(Tightness),
      is.finite(underutilisation_rate)
    )

  if (nrow(valid_conditions) > 0) {
    tightness_median <- median(valid_conditions$Tightness)
    slack_median <- median(
      valid_conditions$underutilisation_rate
    )

    conditions <- valid_conditions |>
      mutate(
        tightness_group = if_else(
          Tightness >= tightness_median,
          "High tightness",
          "Low tightness"
        ),
        slack_group = if_else(
          underutilisation_rate >= slack_median,
          "High slack",
          "Low slack"
        ),
        Condition = paste(
          tightness_group,
          slack_group,
          sep = " + "
        )
      )
  }
}


# ============================================================
# 9. WPI DATA
# ============================================================

wpi_prepared <- tibble()

if (nrow(wpi) > 0) {
  date_candidates <- names(wpi)[
    grepl(
      "date|quarter|period",
      names(wpi),
      ignore.case = TRUE
    )
  ]

  index_candidates <- names(wpi)[
    grepl(
      "wpi|wage.*index",
      names(wpi),
      ignore.case = TRUE
    ) &
      !grepl(
        "growth|change",
        names(wpi),
        ignore.case = TRUE
      )
  ]

  index_candidates <- setdiff(
    index_candidates,
    date_candidates
  )

  date_column <- WPI_DATE_COLUMN
  index_column <- WPI_INDEX_COLUMN

  if (is.null(date_column) && length(date_candidates)) {
    date_column <- date_candidates[[1]]
  }

  if (is.null(index_column) && length(index_candidates)) {
    index_column <- index_candidates[[1]]
  }

  if (
    !is.null(date_column) &&
    !is.null(index_column) &&
    all(c(date_column, index_column) %in% names(wpi))
  ) {
    wpi_prepared <- wpi |>
      transmute(
        date = parse_dates(.data[[date_column]]),
        WPI = parse_numbers(.data[[index_column]])
      ) |>
      filter(!is.na(date)) |>
      arrange(date) |>
      mutate(
        month_id = year(date) * 12 + month(date),
        WPI_growth = if_else(
          month_id - lag(month_id) == 3 &
            is.finite(WPI) &
            is.finite(lag(WPI)) &
            lag(WPI) > 0,
          (WPI / lag(WPI) - 1) * 100,
          NA_real_
        ),
        quarter_id = year(date) * 4 + quarter(date)
      ) |>
      select(-month_id)

    if (anyDuplicated(wpi_prepared$quarter_id)) {
      warning(
        "Multiple WPI observations per quarter. ",
        "Select a single WPI index series."
      )
      wpi_prepared <- tibble()
    }
  }
}


# ============================================================
# 10. STATE CHOICES
# ============================================================

states <- unique(
  c(
    if ("State" %in% names(broad_occupations)) {
      broad_occupations$State
    },
    if ("State" %in% names(ivi_detailed)) {
      ivi_detailed$State
    },
    if ("State" %in% names(ivi_skill)) {
      ivi_skill$State
    }
  )
)

states <- sort(states[!is.na(states) & nzchar(states)])
if (!length(states)) states <- "AUST"

states <- c(
  intersect("AUST", states),
  setdiff(states, "AUST")
)

state_choices <- setNames(
  states,
  vapply(states, state_label, character(1))
)


# ============================================================
# 11. USER INTERFACE
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #F4F7FB;
        color: #263648;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 20px 28px;
      }

      .dashboard-header {
        background: linear-gradient(135deg, #12355B, #087F8C);
        color: white;
        padding: 28px;
        border-radius: 14px;
        margin-bottom: 24px;
      }

      .dashboard-header h1 {
        font-size: 30px;
        margin-top: 0;
      }

      .dashboard-card {
        background: white;
        padding: 22px;
        border-radius: 12px;
        border: 1px solid #E2E8F0;
        margin-bottom: 22px;
      }

      .dashboard-card h3 {
        color: #12355B;
        font-size: 20px;
        margin-top: 0;
      }

      .dashboard-card,
      .plotly-frame,
      .tab-content,
      .tab-pane {
        min-width: 0;
      }

      .plotly-frame {
        width: 100%;
      }

      .plotly-frame .html-widget,
      .plotly-frame .plotly {
        width: 100% !important;
        max-width: 100%;
      }

      .kpi {
        background: white;
        padding: 20px;
        border-left: 5px solid #087F8C;
        border-radius: 12px;
        margin-bottom: 20px;
        min-height: 180px;
        box-shadow: 0 3px 12px rgba(0,0,0,0.04);
      }

      .kpi-label {
        font-size: 12px;
        font-weight: 700;
        color: #718096;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .kpi-value {
        font-size: 28px;
        font-weight: bold;
        color: #12355B;
        margin-top: 10px;
      }

      .kpi-change {
        margin-top: 7px;
        color: #087F8C;
        font-size: 13px;
        font-weight: 600;
      }

      .kpi-description {
        margin-top: 7px;
        color: #718096;
        font-size: 12px;
      }

      .well {
        background: white;
        border-radius: 12px;
      }

      .nav-tabs {
        margin-bottom: 20px;
      }

      .note {
        color: #718096;
        font-size: 13px;
      }

      @media (max-width: 768px) {
        .container-fluid { padding: 12px; }
        .dashboard-card { padding: 14px; }
        .dashboard-header h1 { font-size: 24px; }
      }
    ")),

    tags$script(HTML("
      (function() {
        let resizeTimer;

        function resizeVisiblePlots() {
          if (!window.Plotly || !window.jQuery) return;

          $('.js-plotly-plot').each(function() {
            if (this.offsetWidth > 0 && this.offsetHeight > 0) {
              try {
                Plotly.Plots.resize(this);
              } catch (e) {}
            }
          });
        }

        function scheduleResize() {
          clearTimeout(resizeTimer);
          resizeTimer = setTimeout(resizeVisiblePlots, 200);
        }

        $(document).on(
          'shown.bs.tab',
          'a[data-toggle=\"tab\"]',
          function() {
            scheduleResize();
            setTimeout(resizeVisiblePlots, 600);
          }
        );

        $(window).on('resize', scheduleResize);

        $(document).on(
          'shiny:connected shiny:idle',
          scheduleResize
        );
      })();
    "))
  ),

  div(
    class = "dashboard-header",
    h1("Australian Labour Market Dashboard"),
    p("Labour demand, slack, tightness and wage growth"),
    p("Trisha Andrea and Rimlan Alva")
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Overview",
      br(),
      uiOutput("kpis"),

      div(
        class = "dashboard-card",
        h3("Research Question"),
        p(
          "To what extent are changes in job vacancies ",
          "associated with changes in labour-market ",
          "slack in Australia?"
        ),
        p(
          "The analysis is descriptive and correlational. ",
          "Association does not establish causation."
        ),
        p(
          class = "note",
          "pp = percentage points. Cards show the latest ",
          "valid observation for each indicator."
        )
      ),

      plot_card("Labour-Market Overview", "overview_plot")
    ),

    tabPanel(
      "Labour Demand",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Labour Demand Controls"),
          selectInput(
            "demand_window",
            "Time period:",
            choices = time_choices
          ),
          checkboxInput(
            "show_ma",
            "Overlay 12-month moving average",
            FALSE
          ),
          p(
            "State comparisons use an index with ",
            "January 2019 = 100."
          ),
          p(
            class = "note",
            "This tab uses IVI online advertisements. ",
            "The main quarterly analysis uses ABS ",
            "Job Vacancies."
          ),
          width = 3
        ),
        mainPanel(
          plot_card(
            "Online Job Advertisements",
            "ivi_national_plot"
          ),
          plot_card(
            "Year-on-Year Recruitment Growth",
            "ivi_growth_plot"
          ),
          plot_card(
            "State and Territory Comparison",
            "ivi_state_plot",
            "520px"
          ),
          width = 9
        )
      )
    ),

    tabPanel(
      "Labour-Market Slack",
      br(),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "slack_measures",
            "Measures:",
            choices = names(slack_columns),
            selected = names(slack_columns)
          ),
          selectInput(
            "slack_period",
            "Historical period:",
            choices = c("All periods", period_labels)
          ),
          checkboxInput(
            "slack_events",
            "Shade major downturns",
            FALSE
          ),
          p(
            class = "note",
            "Rates are percentages. Changes in rates ",
            "are measured in percentage points."
          ),
          width = 3
        ),
        mainPanel(
          plot_card(
            "Labour-Market Slack Over Time",
            "slack_plot",
            "520px"
          ),
          table_card(
            "Historical Period Averages",
            "slack_table"
          ),
          width = 9
        )
      )
    ),

    tabPanel(
      "Tightness & Slack",
      br(),
      plot_card(
        "Labour-Market Tightness",
        "tightness_plot"
      ),
      fluidRow(
        column(
          6,
          selectInput(
            "scatter_measure",
            "Slack measure:",
            choices = names(change_columns),
            selected = "Underutilisation"
          )
        ),
        column(
          6,
          selectInput(
            "scatter_lag",
            "Vacancy change leads slack by:",
            choices = c(
              "Same quarter" = 0,
              "1 quarter" = 1,
              "2 quarters" = 2,
              "3 quarters" = 3,
              "4 quarters" = 4
            )
          )
        )
      ),
      plot_card(
        "Changes in Vacancies vs Changes in Slack",
        "vacancy_slack_plot"
      ),
      table_card(
        "Same-Quarter Correlations",
        "tightness_table"
      )
    ),

    tabPanel(
      "Market Conditions",
      br(),
      div(
        class = "dashboard-card",
        h3("How Conditions Are Defined"),
        p(
          "High and low conditions are defined relative ",
          "to sample medians. High tightness and high ",
          "slack can coexist, but this alone does not ",
          "prove labour-market mismatch."
        )
      ),
      plot_card(
        "Quarterly Market Conditions",
        "conditions_plot",
        "520px"
      ),
      table_card(
        "Conditions Summary",
        "conditions_table"
      )
    ),

    tabPanel(
      "Research Question",
      br(),
      selectInput(
        "research_demand",
        "Demand measure:",
        choices = c(
          "Job vacancies" = "change_vacancies",
          "Labour-market tightness" = "change_tightness"
        )
      ),
      plot_card(
        "Quarterly Changes: Main Evidence",
        "research_plot",
        "550px"
      ),
      table_card(
        "Correlation by Slack Measure",
        "research_table"
      ),
      p(
        class = "note",
        "Negative correlations indicate opposite ",
        "movements on average, not a causal effect."
      )
    ),

    tabPanel(
      "Wage Pressure",
      br(),
      plot_card(
        "Quarterly Wage Price Index Growth",
        "wpi_plot"
      ),
      plot_card(
        "Tightness and Wage Growth",
        "tightness_wpi_plot"
      ),
      plot_card(
        "Underutilisation and Wage Growth",
        "slack_wpi_plot"
      ),
      p(
        class = "note",
        "This is a complementary association analysis, ",
        "not a forecasting model."
      )
    ),

    tabPanel(
      "Occupation Demand",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Occupation Controls"),
          selectInput(
            "occupation_dataset",
            "Dataset:",
            choices = c(
              "Broad occupations",
              "Detailed occupations"
            )
          ),
          selectInput(
            "occupation_state",
            "Australia / state / territory:",
            choices = state_choices,
            selected = states[[1]]
          ),
          selectInput(
            "occupation_window",
            "Trend period:",
            choices = time_choices,
            selected = "Last 5 years"
          ),
          sliderInput(
            "occupation_n",
            "Number of occupations:",
            min = 3,
            max = 20,
            value = 8,
            step = 1
          ),
          radioButtons(
            "occupation_display",
            "Occupation trend display:",
            choices = c(
              "Vacancies",
              "Indexed (start = 100)"
            )
          ),
          p(
            class = "note",
            "Broad occupations and skill groups use ",
            "seasonally adjusted data. Detailed ",
            "occupations use three-month averages."
          ),
          p(
            class = "note",
            "Skill demand follows the selected state ",
            "and time period independently of the ",
            "occupation dataset selector."
          ),
          width = 3
        ),
        mainPanel(
          plot_card(
            "Largest Occupations: Latest Month",
            "occupation_latest_plot",
            "560px"
          ),
          plot_card(
            "Occupation Trends",
            "occupation_trend_plot",
            "560px"
          ),
          plot_card(
            "Skill-Level Demand",
            "skill_plot",
            "520px"
          ),
          table_card(
            "Occupation Data",
            "occupation_table"
          ),
          width = 9
        )
      )
    ),

    tabPanel(
      "Data Explorer",
      br(),
      selectInput(
        "explorer_dataset",
        "Dataset:",
        choices = c(
          "Labour-market slack",
          "Vacancies and tightness",
          "Wage Price Index",
          "IVI broad occupations",
          "IVI detailed occupations",
          "IVI skill levels"
        )
      ),
      downloadButton(
        "download_data",
        "Download filtered data"
      ),
      br(),
      br(),
      DTOutput("explorer_table")
    ),

    tabPanel(
      "Methodology",
      br(),
      div(
        class = "dashboard-card",

        h3("Research Design"),
        p(
          "The analysis examines statistical ",
          "associations rather than causal effects."
        ),

        h3("Labour-Market Slack"),
        p(
          "ABS unemployment, underemployment and ",
          "underutilisation rates are used. ",
          "Underutilisation is the primary observable ",
          "proxy for labour-market slack."
        ),

        h3("Labour Demand"),
        p(
          "ABS Job Vacancies are used for the main ",
          "quarterly analysis. IVI advertisements ",
          "provide complementary recruitment indicators."
        ),

        h3("Labour-Market Tightness"),
        p(
          "Tightness is vacancies divided by unemployed ",
          "persons. Its relationship with unemployment ",
          "is partly mechanical."
        ),

        h3("Quarterly Changes"),
        p(
          "Changes are calculated only between ",
          "consecutive quarterly observations. ",
          "Missing values are not interpolated."
        ),

        h3("Market Conditions"),
        p(
          "High and low classifications use sample ",
          "medians and depend on the sample period."
        ),

        h3("Skill-Level Demand"),
        p(
          "Groups 1–5 are displayed separately. Missing ",
          "values are not treated as zero. Duplicate ",
          "aggregate observations are flagged."
        ),

        h3("Limitations"),
        p(
          "Economic shocks can affect demand and slack. ",
          "Data contain gaps and may be revised. ",
          "Advertisements are not identical to vacant ",
          "positions."
        )
      )
    )
  )
)


# ============================================================
# 12. SERVER
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # OVERVIEW
  # ----------------------------------------------------------

  output$kpis <- renderUI({
    cards <- list(
      list(
        label = "Job Vacancies",
        colour = "#2774C6",
        description = "ABS quarterly vacancies",
        result = kpi_summary(
          combined, "Quarter", "Job_Vacancies",
          change_type = "percent",
          digits = 0
        )
      ),
      list(
        label = "Labour-Market Tightness",
        colour = "#19947A",
        description = "Vacancies per unemployed person",
        result = kpi_summary(
          combined, "Quarter", "Tightness",
          digits = 2
        )
      ),
      list(
        label = "Unemployment",
        colour = "#E67E22",
        description = "ABS unemployment rate",
        result = kpi_summary(
          slack, "date", "unemployment_rate",
          digits = 1,
          value_suffix = "%",
          change_suffix = " pp"
        )
      ),
      list(
        label = "Underutilisation",
        colour = "#7657A8",
        description = "Primary observable slack measure",
        result = kpi_summary(
          slack, "date", "underutilisation_rate",
          digits = 1,
          value_suffix = "%",
          change_suffix = " pp"
        )
      )
    )

    fluidRow(
      lapply(cards, function(card) {
        column(
          width = 3,
          div(
            class = "kpi",
            style = paste0(
              "border-left-color: ", card$colour, ";"
            ),
            div(class = "kpi-label", card$label),
            div(
              class = "kpi-value",
              card$result$value
            ),
            div(
              class = "kpi-change",
              card$result$change
            ),
            div(
              class = "kpi-description",
              paste(
                card$description,
                card$result$date,
                sep = " | "
              )
            )
          )
        )
      })
    )
  })

  monthly_slack_long <- reactive({
    validate(
      need(nrow(slack) > 0, "Slack data are unavailable.")
    )

    slack |>
      select(
        date,
        period,
        all_of(unname(slack_columns))
      ) |>
      pivot_longer(
        all_of(unname(slack_columns)),
        names_to = "column",
        values_to = "rate"
      ) |>
      mutate(
        Measure = names(slack_columns)[
          match(column, unname(slack_columns))
        ]
      )
  })

  output$overview_plot <- renderPlotly({
    p <- ggplot(
      monthly_slack_long(),
      aes(date, rate, colour = Measure)
    ) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      scale_colour_manual(values = slack_palette) +
      labs(x = NULL, y = "Rate (%)", colour = NULL) +
      chart_theme()

    as_interactive(p)
  })


  # ----------------------------------------------------------
  # LABOUR DEMAND
  # ----------------------------------------------------------

  demand_data <- reactive({
    validate(
      need(
        nrow(national_ivi) > 0,
        "National IVI data are unavailable."
      )
    )

    filter_window(
      national_ivi, "Month", input$demand_window
    )
  })

  output$ivi_national_plot <- renderPlotly({
    data <- demand_data()

    p <- ggplot(data, aes(Month, Vacancies)) +
      geom_area(
        fill = "#2774C6",
        alpha = 0.12,
        na.rm = TRUE
      ) +
      geom_line(
        colour = "#2774C6",
        linewidth = 1,
        na.rm = TRUE
      ) +
      scale_y_continuous(
        labels = scales::label_comma(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Online job advertisements"
      ) +
      chart_theme()

    if (isTRUE(input$show_ma)) {
      p <- p +
        geom_line(
          aes(y = ma12),
          colour = "#E67E22",
          linewidth = 1,
          na.rm = TRUE
        )
    }

    as_interactive(p) |>
      plotly::layout(
        yaxis = list(
          tickformat = ",.0f",
          exponentformat = "none",
          separatethousands = TRUE,
          automargin = TRUE
        )
      )
  })

  output$ivi_growth_plot <- renderPlotly({
    p <- ggplot(
      demand_data(),
      aes(Month, yoy_change)
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = "#718096"
      ) +
      geom_line(
        colour = "#087F8C",
        linewidth = 1,
        na.rm = TRUE
      ) +
      labs(
        x = NULL,
        y = "Year-on-year change (%)"
      ) +
      chart_theme()

    as_interactive(p)
  })

  output$ivi_state_plot <- renderPlotly({
    validate(
      need(
        nrow(state_ivi) > 0,
        "State IVI data are unavailable."
      )
    )

    data <- filter_window(
      state_ivi, "Month", input$demand_window
    )

    available <- data |>
      group_by(State) |>
      summarise(
        valid = any(is.finite(Index)),
        .groups = "drop"
      ) |>
      filter(valid) |>
      pull(State)

    validate(
      need(
        length(available) > 0,
        "No indexed data. Check the January 2019 baseline."
      )
    )

    data <- data |>
      filter(State %in% available)

    p <- ggplot(
      data,
      aes(Month, Index, colour = State)
    ) +
      geom_hline(
        yintercept = 100,
        linetype = "dashed",
        colour = "#9AA5B1"
      ) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_y_continuous(
        labels = scales::label_number(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Index (January 2019 = 100)",
        colour = "State / territory"
      ) +
      chart_theme()

    as_interactive(p)
  })


  # ----------------------------------------------------------
  # LABOUR-MARKET SLACK
  # ----------------------------------------------------------

  output$slack_plot <- renderPlotly({
    data <- monthly_slack_long()

    validate(
      need(
        length(input$slack_measures) > 0,
        "Select at least one measure."
      )
    )

    data <- data |>
      filter(Measure %in% input$slack_measures)

    if (input$slack_period != "All periods") {
      data <- data |>
        filter(
          as.character(period) == input$slack_period
        )
    }

    validate(
      need(nrow(data) > 0, "No observations in this period.")
    )

    p <- ggplot(data, aes(date, rate, colour = Measure)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_colour_manual(values = slack_palette) +
      labs(x = NULL, y = "Rate (%)", colour = NULL) +
      chart_theme()

    as_interactive(
      shade_events(p, input$slack_events)
    )
  })

  output$slack_table <- renderDT({
    validate(
      need(nrow(slack) > 0, "Slack data unavailable.")
    )

    data <- slack |>
      group_by(Period = period) |>
      summarise(
        Observations = n(),
        across(
          all_of(unname(slack_columns)),
          ~ round(mean(.x, na.rm = TRUE), 2)
        ),
        .groups = "drop"
      )

    make_table(data)
  })


  # ----------------------------------------------------------
  # TIGHTNESS & SLACK
  # ----------------------------------------------------------

  output$tightness_plot <- renderPlotly({
    validate(
      need(nrow(combined) > 0, "Quarterly data unavailable.")
    )

    p <- ggplot(combined, aes(Quarter, Tightness)) +
      geom_line(
        colour = "#087F8C",
        linewidth = 1,
        na.rm = TRUE
      ) +
      labs(
        x = NULL,
        y = "Vacancies per unemployed person"
      ) +
      chart_theme()

    as_interactive(p)
  })

  output$vacancy_slack_plot <- renderPlotly({
    validate(
      need(nrow(combined) > 0, "Quarterly data unavailable.")
    )

    k <- as.integer(input$scatter_lag)

    data <- combined |>
      mutate(
        quarter_id = year(Quarter) * 4 + quarter(Quarter)
      )

    earlier <- data |>
      transmute(
        quarter_id = quarter_id + k,
        earlier_vacancy_change = change_vacancies
      )

    data <- data |>
      left_join(earlier, by = "quarter_id")

    scatter_chart(
      data,
      "earlier_vacancy_change",
      change_columns[[input$scatter_measure]],
      if (k == 0) {
        "Change in job vacancies"
      } else {
        paste(
          "Change in job vacancies",
          k,
          "quarter(s) earlier"
        )
      },
      paste(
        "Change in",
        tolower(input$scatter_measure),
        "(percentage points)"
      )
    )
  })

  output$tightness_table <- renderDT({
    validate(
      need(nrow(combined) > 0, "Quarterly data unavailable.")
    )

    make_table(
      correlation_summary(combined, "change_vacancies")
    )
  })


  # ----------------------------------------------------------
  # MARKET CONDITIONS
  # ----------------------------------------------------------

  output$conditions_plot <- renderPlotly({
    validate(
      need(nrow(conditions) > 0, "Conditions data unavailable.")
    )

    p <- ggplot(
      conditions,
      aes(Quarter, underutilisation_rate)
    ) +
      geom_line(
        colour = "#718096",
        linewidth = 0.7
      ) +
      geom_point(
        aes(colour = Condition),
        size = 2.5
      ) +
      labs(
        x = NULL,
        y = "Underutilisation (%)",
        colour = NULL
      ) +
      chart_theme()

    as_interactive(p)
  })

  output$conditions_table <- renderDT({
    validate(
      need(nrow(conditions) > 0, "Conditions data unavailable.")
    )

    data <- conditions |>
      count(Condition, name = "Quarters") |>
      mutate(
        `Share (%)` = round(
          Quarters / sum(Quarters) * 100,
          1
        )
      )

    make_table(data)
  })


  # ----------------------------------------------------------
  # RESEARCH QUESTION
  # ----------------------------------------------------------

  output$research_plot <- renderPlotly({
    validate(
      need(nrow(combined) > 0, "Quarterly data unavailable.")
    )

    x_column <- input$research_demand

    data <- combined |>
      select(
        Quarter,
        all_of(x_column),
        all_of(unname(change_columns))
      ) |>
      pivot_longer(
        all_of(unname(change_columns)),
        names_to = "column",
        values_to = "change_slack"
      ) |>
      mutate(
        Measure = names(change_columns)[
          match(column, unname(change_columns))
        ]
      ) |>
      filter(
        is.finite(.data[[x_column]]),
        is.finite(change_slack)
      )

    validate(
      need(nrow(data) >= 3, "Not enough matched observations.")
    )

    p <- ggplot(
      data,
      aes(x = .data[[x_column]], y = change_slack)
    ) +
      geom_point(colour = "#2774C6", alpha = 0.6) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        colour = "#E67E22"
      ) +
      facet_wrap(~ Measure, scales = "free_y") +
      scale_x_continuous(
        labels = scales::label_comma()
      ) +
      labs(
        x = if (x_column == "change_vacancies") {
          "Change in job vacancies"
        } else {
          "Change in labour-market tightness"
        },
        y = "Change in slack (percentage points)"
      ) +
      chart_theme()

    as_interactive(p)
  })

  output$research_table <- renderDT({
    validate(
      need(nrow(combined) > 0, "Quarterly data unavailable.")
    )

    make_table(
      correlation_summary(
        combined,
        input$research_demand
      )
    )
  })


  # ----------------------------------------------------------
  # WAGE PRESSURE
  # ----------------------------------------------------------

  matched_wpi <- reactive({
    validate(
      need(
        nrow(combined) > 0 && nrow(wpi_prepared) > 0,
        paste(
          "WPI data unavailable.",
          "Check WPI_DATE_COLUMN and WPI_INDEX_COLUMN."
        )
      )
    )

    combined |>
      mutate(
        quarter_id = year(Quarter) * 4 + quarter(Quarter)
      ) |>
      left_join(
        wpi_prepared |>
          select(quarter_id, WPI_growth),
        by = "quarter_id"
      )
  })

  output$wpi_plot <- renderPlotly({
    validate(
      need(
        nrow(wpi_prepared) > 0,
        "WPI data unavailable. Check selected columns."
      )
    )

    p <- ggplot(
      wpi_prepared,
      aes(date, WPI_growth)
    ) +
      geom_line(
        colour = "#E67E22",
        linewidth = 1,
        na.rm = TRUE
      ) +
      labs(x = NULL, y = "Quarterly WPI growth (%)") +
      chart_theme()

    as_interactive(p)
  })

  output$tightness_wpi_plot <- renderPlotly({
    scatter_chart(
      matched_wpi(),
      "Tightness",
      "WPI_growth",
      "Vacancies per unemployed person",
      "Quarterly WPI growth (%)"
    )
  })

  output$slack_wpi_plot <- renderPlotly({
    scatter_chart(
      matched_wpi(),
      "underutilisation_rate",
      "WPI_growth",
      "Underutilisation (%)",
      "Quarterly WPI growth (%)"
    )
  })


  # ----------------------------------------------------------
  # OCCUPATION DEMAND
  # ----------------------------------------------------------

  occupation_data <- reactive({
    data <- if (
      input$occupation_dataset == "Broad occupations"
    ) {
      broad_occupations
    } else {
      ivi_detailed
    }

    validate(
      need(
        nrow(data) > 0 &&
          all(
            c("Month", "Title", "Vacancies", "State") %in%
              names(data)
          ),
        "Occupation data are unavailable."
      )
    )

    data <- data |>
      filter(
        State == input$occupation_state,
        !is.na(Title)
      ) |>
      select(Month, Title, Vacancies) |>
      arrange(Title, Month)

    validate(
      need(nrow(data) > 0, "No data for the selected state.")
    )

    duplicates <- data |>
      count(Month, Title) |>
      filter(n > 1)

    validate(
      need(
        nrow(duplicates) == 0,
        "Duplicate occupation rows. Check the source series."
      )
    )

    data
  })

  latest_occupation_data <- reactive({
    data <- occupation_data() |>
      filter(is.finite(Vacancies))

    validate(
      need(nrow(data) > 0, "No valid vacancy observations.")
    )

    data |>
      filter(Month == max(Month))
  })

  top_occupations <- reactive({
    latest_occupation_data() |>
      slice_max(
        Vacancies,
        n = input$occupation_n,
        with_ties = FALSE
      ) |>
      pull(Title)
  })

  output$occupation_latest_plot <- renderPlotly({
    data <- latest_occupation_data() |>
      filter(Title %in% top_occupations())

    p <- ggplot(
      data,
      aes(
        x = Vacancies,
        y = reorder(Title, Vacancies)
      )
    ) +
      geom_col(fill = "#2774C6") +
      scale_x_continuous(
        labels = scales::label_comma(accuracy = 1)
      ) +
      labs(
        x = "Online job advertisements",
        y = NULL,
        subtitle = paste(
          state_label(input$occupation_state),
          format(max(data$Month), "%B %Y"),
          sep = " | "
        )
      ) +
      chart_theme()

    as_interactive(p) |>
      plotly::layout(
        margin = list(l = 230, r = 30, t = 75, b = 65),
        yaxis = list(automargin = TRUE),
        xaxis = list(
          tickformat = ",.0f",
          automargin = TRUE
        )
      )
  })

  output$occupation_trend_plot <- renderPlotly({
    data <- occupation_data() |>
      filter(Title %in% top_occupations())

    data <- filter_window(
      data, "Month", input$occupation_window
    )

    validate(
      need(nrow(data) > 0, "No data in this period.")
    )

    months <- seq.Date(
      min(data$Month),
      max(data$Month),
      by = "month"
    )

    indexed <- input$occupation_display != "Vacancies"

    data <- data |>
      complete(Month = months, Title) |>
      arrange(Title, Month) |>
      group_by(Title) |>
      mutate(
        baseline = {
          valid <- Vacancies[is.finite(Vacancies)]
          if (length(valid)) valid[[1]] else NA_real_
        },
        value = if (indexed) {
          if_else(
            is.finite(baseline) & baseline > 0,
            Vacancies / baseline * 100,
            NA_real_
          )
        } else {
          Vacancies
        }
      ) |>
      ungroup()

    p <- ggplot(
      data,
      aes(Month, value, colour = Title)
    ) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      labs(
        x = NULL,
        y = if (indexed) {
          "Index (first valid observation = 100)"
        } else {
          "Online job advertisements"
        },
        colour = NULL
      ) +
      chart_theme()

    as_interactive(p)
  })

  output$occupation_table <- renderDT({
    make_table(
      filter_window(
        occupation_data(),
        "Month",
        input$occupation_window
      )
    )
  })


  # ----------------------------------------------------------
  # SKILL-LEVEL DEMAND
  # ----------------------------------------------------------

  output$skill_plot <- renderPlotly({
    validate(
      need(nrow(ivi_skill) > 0, skill_data_message)
    )

    data <- ivi_skill |>
      filter(
        State == normalise_state(input$occupation_state)
      )

    validate(
      need(
        nrow(data) > 0,
        paste(
          "No skill-level observations for",
          state_label(input$occupation_state)
        )
      )
    )

    duplicates <- data |>
      count(Month, Skill) |>
      filter(n > 1)

    validate(
      need(
        nrow(duplicates) == 0,
        paste(
          "Duplicate skill-level rows found.",
          "Check for multiple series types in the CSV."
        )
      )
    )

    data <- filter_window(
      data, "Month", input$occupation_window
    )

    validate(
      need(nrow(data) > 0, "No data in this period."),
      need(
        any(is.finite(data$Vacancies)),
        "No valid skill-level vacancy values."
      )
    )

    months <- seq.Date(
      min(data$Month),
      max(data$Month),
      by = "month"
    )

    data <- data |>
      select(Month, Skill, Vacancies) |>
      complete(
        Month = months,
        Skill,
        fill = list(Vacancies = NA_real_)
      ) |>
      arrange(Skill, Month)

    p <- ggplot(
      data,
      aes(
        Month,
        Vacancies,
        colour = Skill,
        group = Skill
      )
    ) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      scale_colour_manual(
        values = skill_palette,
        drop = FALSE
      ) +
      scale_y_continuous(
        labels = scales::label_comma(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Online job advertisements",
        colour = NULL,
        subtitle = paste(
          "ANZSCO skill groups |",
          state_label(input$occupation_state)
        )
      ) +
      chart_theme()

    as_interactive(p) |>
      plotly::layout(
        yaxis = list(
          tickformat = ",.0f",
          exponentformat = "none",
          automargin = TRUE
        )
      )
  })


  # ----------------------------------------------------------
  # DATA EXPLORER
  # ----------------------------------------------------------

  explorer_data <- reactive({
    switch(
      input$explorer_dataset,
      "Labour-market slack" = slack,
      "Vacancies and tightness" = combined,
      "Wage Price Index" = wpi_prepared,
      "IVI broad occupations" = broad_occupations,
      "IVI detailed occupations" = ivi_detailed,
      "IVI skill levels" = ivi_skill
    )
  })

  output$explorer_table <- renderDT({
    data <- explorer_data()

    validate(
      need(nrow(data) > 0, "Dataset unavailable.")
    )

    datatable(
      data,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  }, server = FALSE)

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "labour_market_",
        gsub(
          "[^a-z0-9]+",
          "_",
          tolower(input$explorer_dataset)
        ),
        ".csv"
      )
    },
    content = function(file) {
      data <- explorer_data()
      rows <- input$explorer_table_rows_all

      if (!is.null(rows)) {
        data <- data[rows, , drop = FALSE]
      }

      write_csv(data, file)
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