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
# 2. COLOURS AND CONSTANTS
# ============================================================

dashboard_palette <- c(
  "Unemployment"     = "#2774C6",
  "Underemployment"  = "#E67E22",
  "Underutilisation" = "#087F8C",
  "Youth"            = "#7657A8",
  "Overall"          = "#12355B",
  "Vacancies"        = "#2774C6",
  "Tightness"        = "#087F8C",
  "WPI Growth"       = "#E67E22"
)

occupation_palette <- c(
  "#12355B", "#2774C6", "#087F8C", "#19947A", "#E67E22",
  "#D95F02", "#7657A8", "#8C6BB1", "#5C7AEA", "#4E9F8A",
  "#B83280", "#718096", "#2F855A", "#C05621", "#2B6CB0"
)

period_levels <- c(
  "1978–1989", "1990–1994", "1995–2007",
  "2008–2019", "2020–2021", "2022–present"
)

event_periods <- tibble(
  event = c("Early-1990s downturn", "COVID-19"),
  start = as.Date(c("1990-01-01", "2020-03-01")),
  end   = as.Date(c("1992-12-31", "2021-12-31"))
)

slack_measure_cols <- c(
  "Unemployment"     = "change_unemployment",
  "Underemployment"  = "change_underemployment",
  "Underutilisation" = "change_underutilisation"
)

# Demand measures that can be compared with slack
demand_cols <- c(
  "Job vacancies"           = "change_vacancies",
  "Labour-market tightness" = "change_tightness"
)

state_labels <- c(
  AUST = "Australia", NSW = "New South Wales", VIC = "Victoria",
  QLD = "Queensland", SA = "South Australia", WA = "Western Australia",
  TAS = "Tasmania", NT = "Northern Territory",
  ACT = "Australian Capital Territory"
)

mismatch_label <- "High tightness + high slack"


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

# ---- Files ---------------------------------------------------

find_project_file <- function(filename) {

  possible_paths <- c(
    file.path("..", "data", "processed", filename),
    file.path("data", "processed", filename),
    file.path("..", "..", "data", "processed", filename)
  )

  existing <- possible_paths[file.exists(possible_paths)]

  if (length(existing) == 0) return(NA_character_)

  existing[1]
}


safe_read_csv <- function(filename) {

  file_path <- find_project_file(filename)

  if (is.na(file_path)) {
    message("File not found: ", filename)
    return(tibble())
  }

  tryCatch(
    read_csv(file_path, show_col_types = FALSE),
    error = function(e) {
      message("Could not read ", filename, ": ", e$message)
      tibble()
    }
  )
}


# ---- Parsing -------------------------------------------------

safe_date <- function(x) {

  if (inherits(x, "Date")) return(x)

  x_chr <- trimws(as.character(x))

  result <- suppressWarnings(
    parse_date_time(
      x_chr,
      orders = c("Y-m-d", "d/m/Y", "m/d/Y", "Y/m/d",
                 "Y-m", "m-Y", "b Y", "B Y", "Y", "Y q", "Y-Q")
    )
  )

  missing <- is.na(result)
  if (any(missing)) {
    parsed_month <- suppressWarnings(as.Date(paste("01", x_chr[missing]), format = "%d %B %Y"))
    still_missing <- is.na(parsed_month)
    if (any(still_missing)) {
      parsed_month[still_missing] <- suppressWarnings(
        as.Date(paste("01", x_chr[missing][still_missing]), format = "%d %b %Y")
      )
    }
    result[missing] <- as.POSIXct(parsed_month)
  }

  as.Date(result)
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}


assign_period <- function(d) {

  factor(
    case_when(
      d < as.Date("1990-01-01") ~ period_levels[1],
      d < as.Date("1995-01-01") ~ period_levels[2],
      d < as.Date("2008-01-01") ~ period_levels[3],
      d < as.Date("2020-01-01") ~ period_levels[4],
      d < as.Date("2022-01-01") ~ period_levels[5],
      TRUE                      ~ period_levels[6]
    ),
    levels = period_levels
  )
}


# Months between consecutive rows (3 = consecutive quarters)
quarter_gap <- function(q) {
  (year(q) - year(lag(q))) * 12 + (month(q) - month(lag(q)))
}

# Quarter-to-quarter change, only when the previous row is the
# previous quarter (otherwise NA, not a misleading multi-quarter jump)
qchange <- function(x, q) {
  if_else(quarter_gap(q) == 3, x - lag(x), NA_real_)
}


# Keep one state / territory
filter_state <- function(data, state_code) {

  if (nrow(data) > 0 && "State" %in% names(data)) {
    data <- data |>
      filter(toupper(as.character(State)) == state_code)
  }

  data
}


# ---- Statistics ----------------------------------------------

safe_cor <- function(x, y) {

  x <- safe_numeric(x)
  y <- safe_numeric(y)
  ok <- complete.cases(x, y)

  if (sum(ok) < 3) return(NA_real_)

  suppressWarnings(cor(x[ok], y[ok]))
}


cor_stats <- function(x, y) {

  x <- safe_numeric(x)
  y <- safe_numeric(y)
  ok <- complete.cases(x, y)

  out <- tibble(
    n = sum(ok), r = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, p_value = NA_real_, spearman = NA_real_
  )

  if (sum(ok) < 5) return(out)

  ct <- tryCatch(
    suppressWarnings(cor.test(x[ok], y[ok])),
    error = function(e) NULL
  )

  if (is.null(ct) || is.null(ct$conf.int)) return(out)

  out$r        <- unname(ct$estimate)
  out$ci_low   <- ct$conf.int[1]
  out$ci_high  <- ct$conf.int[2]
  out$p_value  <- ct$p.value
  out$spearman <- suppressWarnings(
    cor(x[ok], y[ok], method = "spearman")
  )

  out
}


strength_label <- function(r) {
  case_when(
    is.na(r)     ~ "—",
    abs(r) < 0.1 ~ "Negligible",
    abs(r) < 0.3 ~ "Weak",
    abs(r) < 0.5 ~ "Moderate",
    TRUE         ~ "Strong"
  )
}


direction_label <- function(r) {
  case_when(
    is.na(r) ~ "—",
    r < 0    ~ "Negative",
    r > 0    ~ "Positive",
    TRUE     ~ "None"
  )
}


corr_by_measure <- function(data, x = "change_vacancies") {

  map_dfr(
    names(slack_measure_cols),
    function(m) {
      bind_cols(
        tibble(Measure = m),
        cor_stats(data[[x]], data[[slack_measure_cols[[m]]]])
      )
    }
  )
}


format_cor_table <- function(res) {

  res |>
    mutate(
      Direction  = direction_label(r),
      Strength   = strength_label(r),
      `95% CI`   = if_else(
        is.na(r), "—",
        sprintf("[%.2f, %.2f]", ci_low, ci_high)
      ),
      `p-value`  = coalesce(scales::pvalue(p_value), "—"),
      r          = round(r, 3),
      Spearman   = round(spearman, 3)
    ) |>
    select(-ci_low, -ci_high, -p_value, -spearman) |>
    rename(`Pearson r` = r) |>
    relocate(
      any_of(c("Measure", "Period")),
      n, `Pearson r`, `95% CI`, `p-value`, Spearman, Direction, Strength
    )
}


leadlag_table <- function(data, x = "change_vacancies", max_lag = 4) {

  labels <- c("Same quarter", paste0(1:max_lag, "q earlier"))

  map_dfr(
    names(slack_measure_cols),
    function(m) {

      values <- map_dbl(
        0:max_lag,
        function(k) {
          safe_cor(
            dplyr::lag(data[[x]], k),
            data[[slack_measure_cols[[m]]]]
          )
        }
      )

      bind_cols(
        tibble(Measure = m),
        as_tibble(as.list(setNames(round(values, 3), labels)))
      )
    }
  )
}


# ---- Formatting ----------------------------------------------

fmt_num <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) return("—")

  scales::number(x, accuracy = 10^(-digits), big.mark = ",")
}


fmt_pct <- function(x, digits = 1) {

  if (length(x) == 0 || is.na(x)) return("—")

  paste0(scales::number(x, accuracy = 10^(-digits)), "%")
}


delta_text <- function(diff, suffix, digits = 1) {

  if (length(diff) == 0 || is.na(diff)) return(NULL)

  arrow <- if (diff > 0) "▲" else if (diff < 0) "▼" else "■"

  paste0(
    arrow, " ",
    formatC(abs(diff), format = "f", digits = digits),
    suffix, " vs a year ago"
  )
}


kpi_values <- function(data, date_col, value_col, lag_n) {

  empty <- list(latest = NA_real_, diff = NA_real_,
                pct = NA_real_, when = "n/a")

  if (nrow(data) == 0 || !all(c(date_col, value_col) %in% names(data))) {
    return(empty)
  }

  d <- data |>
    filter(!is.na(.data[[date_col]]), !is.na(.data[[value_col]])) |>
    arrange(.data[[date_col]])

  n <- nrow(d)

  if (n == 0) return(empty)

  latest <- d[[value_col]][n]
  prior  <- if (n > lag_n) d[[value_col]][n - lag_n] else NA_real_

  list(
    latest = latest,
    diff   = latest - prior,
    pct    = (latest / prior - 1) * 100,
    when   = format(d[[date_col]][n], "%b %Y")
  )
}


# ---- UI building blocks --------------------------------------

section_card <- function(title, ...) {
  div(class = "section-card", h3(title), ...)
}

plot_card <- function(title, output_id, height = "500px", ...) {
  section_card(
    title,
    ...,
    div(
      class = "plotly-frame",
      style = "width: 100%; min-width: 0; display: block;",
      plotlyOutput(
        outputId = output_id,
        height = height,
        width = "100%"
      )
    )
  )
}

table_card <- function(title, output_id, ...) {
  section_card(title, ..., DTOutput(output_id))
}

method_box <- function(title, ...) {
  div(class = "method-box", h4(title), p(...))
}

note <- function(...) {
  p(class = "note", ...)
}

kpi_card <- function(label, value, delta = NULL, description, colour = "blue") {

  div(
    class = paste("kpi-card", paste0("kpi-", colour)),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(delta)) div(class = "kpi-delta", delta),
    div(class = "kpi-description", description)
  )
}

events_checkbox <- function(id) {
  checkboxInput(id, "Shade major downturns (1990s, COVID-19)", FALSE)
}


# ---- DT options ----------------------------------------------

datatable_options <- function(default = 10) {

  list(
    pageLength = default,
    lengthMenu = list(
      c(10, 25, 50, 100, 250, 500, -1),
      c("10", "25", "50", "100", "250", "500", "All")
    ),
    lengthChange = TRUE,
    searching = TRUE,
    ordering = TRUE,
    paging = TRUE,
    info = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE
  )
}

simple_dt_options <- function() {
  list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
}

message_table <- function(text) {
  datatable(
    tibble(Message = text),
    rownames = FALSE,
    options = simple_dt_options()
  )
}


# ---- Plot helpers --------------------------------------------

empty_plot <- function(message = "No data available") {

  plot_ly() |>
    layout(
      autosize = TRUE,
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      xaxis = list(visible = FALSE, fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE),
      annotations = list(
        list(
          text = message, x = 0.5, y = 0.5,
          xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 16, color = "#718096"),
          align = "center"
        )
      ),
      margin = list(l = 20, r = 20, t = 20, b = 20)
    ) |>
    config(displaylogo = FALSE, responsive = TRUE)
}


clean_plotly <- function(plot, legend_top = TRUE, right = 30) {

  plot <- plotly::layout(
    plot,
    autosize = TRUE,
    font = list(family = "Arial, sans-serif", size = 13, color = "#263648"),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF",
    hoverlabel = list(
      bgcolor = "#FFFFFF",
      bordercolor = "#D9E2EC",
      font = list(color = "#263648", size = 12)
    ),
    margin = list(l = 70, r = right, t = if (legend_top) 75 else 40, b = 65)
  )

  if (legend_top) {
    plot <- plotly::layout(
      plot,
      legend = list(
        orientation = "h",
        x = 0,
        xanchor = "left",
        y = 1.16,
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.85)"
      )
    )
  }

  plotly::config(
    plot,
    displaylogo = FALSE,
    responsive = TRUE,
    displayModeBar = TRUE,
    scrollZoom = TRUE
  )
}


theme_dashboard <- function(base_size = 13) {

  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")
}


add_events <- function(p, show, x_min, x_max) {

  if (!isTRUE(show) || is.na(x_min) || is.na(x_max)) return(p)

  ev <- event_periods |>
    filter(end >= x_min, start <= x_max) |>
    mutate(start = pmax(start, x_min), end = pmin(end, x_max))

  if (nrow(ev) == 0) return(p)

  p +
    geom_rect(
      data = ev,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "#9AA5B1",
      alpha = 0.18
    )
}


slack_long <- function(data) {

  data |>
    select(date, unemployment_rate, underemployment_rate,
           underutilisation_rate) |>
    pivot_longer(-date, names_to = "measure", values_to = "rate") |>
    mutate(
      measure = recode(
        measure,
        unemployment_rate     = "Unemployment",
        underemployment_rate  = "Underemployment",
        underutilisation_rate = "Underutilisation"
      )
    )
}


scatter_plot <- function(data, x, y, x_lab, y_lab,
                         point_colour = "#2774C6",
                         label_col = "Quarter") {

  data <- data |>
    mutate(
      tip = paste0(
        format(.data[[label_col]], "%b %Y"),
        "<br>", x_lab, ": ", round(.data[[x]], 2),
        "<br>", y_lab, ": ", round(.data[[y]], 2)
      )
    )

  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(
      aes(text = tip),
      colour = point_colour, alpha = 0.7, size = 2.5
    ) +
    geom_smooth(
      method = "lm", formula = y ~ x, se = TRUE,
      colour = "#E67E22", fill = "#E67E22", alpha = 0.15
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_dashboard()

  clean_plotly(ggplotly(p, tooltip = "text"))
}


# ============================================================
# 4. LOAD DATA
# ============================================================

labour_market_slack <- safe_read_csv("labour_market_slack.csv")
combined_data       <- safe_read_csv("combined_tightness_slack.csv")
wpi_data            <- safe_read_csv("labour_market_wpi.csv")
ivi_anzsco2         <- safe_read_csv("ivi_anzsco2_clean.csv")
ivi_anzsco4         <- safe_read_csv("ivi_anzsco4_clean.csv")
ivi_skill           <- safe_read_csv("ivi_skill_clean.csv")


# ============================================================
# 5. PREPARE SLACK DATA (monthly)
# ============================================================

add_gap <- function(d, youth, overall, name) {
  if (all(c(youth, overall) %in% names(d))) d[[name]] <- d[[youth]] - d[[overall]]
  d
}

if (nrow(labour_market_slack) > 0 && "date" %in% names(labour_market_slack)) {

  labour_market_slack <- labour_market_slack |>
    mutate(
      date = safe_date(date),
      across(
        any_of(c("unemployment_rate", "underemployment_rate",
                 "underutilisation_rate", "youth_unemployment_rate",
                 "youth_underemployment_rate",
                 "youth_underutilisation_rate")),
        safe_numeric
      )
    ) |>
    filter(!is.na(date)) |>
    arrange(date)

  labour_market_slack <- labour_market_slack |>
    add_gap("youth_unemployment_rate", "unemployment_rate",
            "youth_unemployment_gap") |>
    add_gap("youth_underemployment_rate", "underemployment_rate",
            "youth_underemployment_gap") |>
    add_gap("youth_underutilisation_rate", "underutilisation_rate",
            "youth_underutilisation_gap") |>
    mutate(
      period = assign_period(date),
      unemploy_change     = unemployment_rate     - lag(unemployment_rate),
      underemploy_change  = underemployment_rate  - lag(underemployment_rate),
      underutilise_change = underutilisation_rate - lag(underutilisation_rate),
      unemploy_yoy        = unemployment_rate     - lag(unemployment_rate, 12),
      underemploy_yoy     = underemployment_rate  - lag(underemployment_rate, 12),
      underutilise_yoy    = underutilisation_rate - lag(underutilisation_rate, 12)
    )

} else {
  labour_market_slack <- tibble()
}


# ============================================================
# 6. PREPARE COMBINED DATA (quarterly)
#    Changes are only computed between consecutive quarters.
# ============================================================

if (nrow(combined_data) > 0 && "Quarter" %in% names(combined_data)) {

  combined_data <- combined_data |>
    mutate(
      Quarter = safe_date(Quarter),
      across(
        any_of(c("Job_Vacancies", "unemployed", "Tightness",
                 "unemployment_rate", "underemployment_rate",
                 "underutilisation_rate")),
        safe_numeric
      )
    ) |>
    filter(!is.na(Quarter)) |>
    arrange(Quarter) |>
    mutate(
      period = assign_period(Quarter),
      change_vacancies        = qchange(Job_Vacancies, Quarter),
      change_unemployment     = qchange(unemployment_rate, Quarter),
      change_underemployment  = qchange(underemployment_rate, Quarter),
      change_underutilisation = qchange(underutilisation_rate, Quarter),
      change_tightness        = qchange(Tightness, Quarter)
    )

} else {
  combined_data <- tibble()
}


# ---- High / low tightness and slack (median split) -----------

market_conditions <- tibble()
tightness_median <- NA_real_
underutilisation_median <- NA_real_

if (nrow(combined_data) > 0) {

  cond <- combined_data |>
    filter(!is.na(Tightness), !is.na(underutilisation_rate))

  if (nrow(cond) > 0) {

    tightness_median        <- median(cond$Tightness)
    underutilisation_median <- median(cond$underutilisation_rate)

    market_conditions <- cond |>
      mutate(
        demand_level = if_else(Tightness >= tightness_median,
                               "High tightness", "Low tightness"),
        slack_level  = if_else(underutilisation_rate >= underutilisation_median,
                               "High slack", "Low slack"),
        conditions   = paste(demand_level, slack_level, sep = " + "),
        flag         = if_else(
          demand_level == "High tightness" & slack_level == "High slack",
          mismatch_label, "Other quarter"
        )
      )
  }
}


# ============================================================
# 7. PREPARE WPI DATA
# ============================================================

if (nrow(wpi_data) > 0) {

  wpi_date_candidates <- names(wpi_data)[
    grepl("date|quarter|period", names(wpi_data), ignore.case = TRUE)
  ]

  wpi_value_candidates <- setdiff(
    names(wpi_data)[grepl("wpi", names(wpi_data), ignore.case = TRUE)],
    wpi_date_candidates
  )

  if (length(wpi_date_candidates) > 0 && length(wpi_value_candidates) > 0) {

    wpi_date_col  <- wpi_date_candidates[1]
    wpi_value_col <- wpi_value_candidates[1]

    wpi_data <- wpi_data |>
      mutate(
        wpi_date  = safe_date(.data[[wpi_date_col]]),
        WPI_value = safe_numeric(.data[[wpi_value_col]])
      ) |>
      filter(!is.na(wpi_date)) |>
      arrange(wpi_date) |>
      mutate(WPI_growth = (WPI_value / lag(WPI_value) - 1) * 100)

  } else {
    wpi_data <- tibble()
  }
}


# ============================================================
# 8. PREPARE IVI DATA
#    The three IVI files are not identical:
#     - ANZSCO2 / skill: seasonally adjusted, columns Title, State
#     - ANZSCO4: 3-month average, columns ANZSCO_TITLE, State,
#       plus "." codes and an "Australia Total" row
# ============================================================

standardise_ivi <- function(data) {

  if (nrow(data) == 0) return(data)

  if ("state" %in% names(data) && !"State" %in% names(data)) {
    data <- rename(data, State = state)
  }

  if (!"Title" %in% names(data) && "ANZSCO_TITLE" %in% names(data)) {
    data <- rename(data, Title = ANZSCO_TITLE)
  }

  data
}


prepare_ivi <- function(data) {

  data <- standardise_ivi(data)

  if (nrow(data) == 0 || !all(c("Month", "Vacancies") %in% names(data))) {
    return(tibble())
  }

  data |>
    mutate(
      Month     = safe_date(Month),
      Vacancies = safe_numeric(Vacancies)
    ) |>
    arrange(Month)
}

ivi_anzsco2 <- prepare_ivi(ivi_anzsco2)
ivi_anzsco4 <- prepare_ivi(ivi_anzsco4)
ivi_skill   <- prepare_ivi(ivi_skill)


# ---- ANZSCO4: drop placeholder codes, totals and empty rows ---

if (nrow(ivi_anzsco4) > 0 && "Title" %in% names(ivi_anzsco4)) {

  ivi_anzsco4 <- ivi_anzsco4 |>
    filter(
      !is.na(Title),
      !is.na(Vacancies),
      !str_detect(Title, regex("total", ignore_case = TRUE))
    )

  if ("ANZSCO_CODE" %in% names(ivi_anzsco4)) {
    ivi_anzsco4 <- ivi_anzsco4 |>
      filter(!as.character(ANZSCO_CODE) %in% c(".", "", NA))
  }
}


# ---- Skill levels: keep Skill_level 1-5, create a label -------

skill_order <- character()

if (nrow(ivi_skill) > 0) {

  if ("Skill_level" %in% names(ivi_skill)) {

    kept <- ivi_skill |>
      filter(as.character(Skill_level) %in% as.character(1:5))

    if (nrow(kept) > 0) ivi_skill <- kept

    ivi_skill <- ivi_skill |>
      mutate(
        Skill = if ("Title" %in% names(ivi_skill)) {
          as.character(Title)
        } else {
          paste("Skill level", Skill_level)
        }
      )

    skill_order <- ivi_skill |>
      distinct(Skill, Skill_level) |>
      arrange(Skill_level) |>
      pull(Skill) |>
      unique()

  } else {

    skill_candidates <- names(ivi_skill)[
      grepl("skill", names(ivi_skill), ignore.case = TRUE)
    ]

    if (length(skill_candidates) > 0) {
      ivi_skill <- ivi_skill |>
        mutate(Skill = as.character(.data[[skill_candidates[1]]]))
      skill_order <- sort(unique(ivi_skill$Skill))
    }
  }
}


# ============================================================
# 9. NATIONAL IVI (with YoY and 12-month moving average)
# ============================================================

national_ivi <- tibble()

if (nrow(ivi_anzsco2) > 0 &&
    all(c("Level", "ANZSCO_CODE", "State") %in% names(ivi_anzsco2))) {

  national_ivi <- ivi_anzsco2 |>
    filter(
      as.character(Level) == "1",
      as.character(ANZSCO_CODE) == "0",
      toupper(as.character(State)) == "AUST"
    )
}

if (nrow(national_ivi) == 0 && nrow(ivi_anzsco2) > 0) {

  fallback <- ivi_anzsco2

  if ("Level" %in% names(fallback)) {
    fallback <- fallback |> filter(as.character(Level) == "2")
  }

  national_ivi <- fallback |>
    filter_state("AUST") |>
    group_by(Month) |>
    summarise(Vacancies = sum(Vacancies, na.rm = TRUE), .groups = "drop")
}

if (nrow(national_ivi) > 0) {

  national_ivi <- national_ivi |>
    filter(!is.na(Month), !is.na(Vacancies)) |>
    arrange(Month) |>
    mutate(
      yoy_change = (Vacancies / lag(Vacancies, 12) - 1) * 100,
      ma12 = as.numeric(stats::filter(Vacancies, rep(1 / 12, 12), sides = 1))
    )
}


# ---- State totals, indexed to January 2019 = 100 -------------

state_ivi <- tibble()

if (nrow(ivi_anzsco2) > 0 &&
    all(c("Level", "ANZSCO_CODE", "State") %in% names(ivi_anzsco2))) {

  state_ivi <- ivi_anzsco2 |>
    filter(
      as.character(Level) == "1",
      as.character(ANZSCO_CODE) == "0",
      toupper(as.character(State)) != "AUST",
      !is.na(Month), !is.na(Vacancies)
    ) |>
    mutate(State = toupper(as.character(State))) |>
    group_by(State) |>
    arrange(Month, .by_group = TRUE) |>
    mutate(
      baseline = if (any(Month >= as.Date("2019-01-01"), na.rm = TRUE)) {
        first(Vacancies[Month >= as.Date("2019-01-01")])
      } else {
        first(Vacancies)
      },
      Index = if_else(!is.na(baseline) & baseline != 0,
                      Vacancies / baseline * 100, NA_real_)
    ) |>
    ungroup()
}


# ============================================================
# 10. OCCUPATION DATA AND STATE CHOICES
# ============================================================

occupation_data <- tibble()

if (nrow(ivi_anzsco2) > 0 && "Level" %in% names(ivi_anzsco2)) {

  occupation_data <- ivi_anzsco2 |>
    filter(as.character(Level) == "2")
}

state_codes <- character()

if (nrow(ivi_anzsco2) > 0 && "State" %in% names(ivi_anzsco2)) {
  state_codes <- sort(unique(toupper(as.character(ivi_anzsco2$State))))
  state_codes <- state_codes[!is.na(state_codes)]
}

if (length(state_codes) == 0) state_codes <- "AUST"

state_codes <- c(intersect("AUST", state_codes), setdiff(state_codes, "AUST"))

state_choices <- setNames(
  state_codes,
  ifelse(state_codes %in% names(state_labels),
         state_labels[state_codes], state_codes)
)


# ============================================================
# 11. UI
# ============================================================

ui <- fluidPage(

  tags$head(
    tags$style(HTML("

      body {
        background: linear-gradient(180deg, #F4F7FB 0%, #F8FAFC 100%);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
        color: #263648;
      }

      .container-fluid { padding-left: 28px; padding-right: 28px; }

      .dashboard-header {
        background: linear-gradient(135deg, #102A43 0%, #164E63 50%, #087F8C 100%);
        color: white;
        padding: 34px 38px;
        border-radius: 18px;
        margin-top: 18px;
        margin-bottom: 25px;
        box-shadow: 0 8px 25px rgba(31, 48, 64, 0.15);
      }
      .dashboard-header h1 { margin: 0 0 8px 0; font-size: 30px; font-weight: 750; }
      .dashboard-header p  { margin: 4px 0; color: rgba(255, 255, 255, 0.88); }

      .nav-tabs { border-bottom: 1px solid #DCE5ED; margin-bottom: 20px; }
      .nav-tabs > li > a {
        color: #526579; font-weight: 600; border: none !important; padding: 12px 14px;
      }
      .nav-tabs > li > a:hover {
        color: #087F8C; background: #EEF8F9 !important; border-radius: 8px 8px 0 0;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #087F8C !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 3px solid #087F8C !important;
        font-weight: 700;
      }

      .section-card {
        background: white;
        border: 1px solid #E3EAF1;
        border-radius: 16px;
        padding: 24px;
        margin-bottom: 24px;
        box-shadow: 0 4px 18px rgba(31, 50, 70, 0.055);
      }
      .section-card h3 {
        color: #102A43; font-size: 20px; font-weight: 750;
        margin-top: 0; margin-bottom: 18px; padding-bottom: 12px;
        border-bottom: 1px solid #EDF1F5;
      }
      .section-card h3::before {
        content: ''; display: inline-block; width: 5px; height: 20px;
        background: #087F8C; border-radius: 5px; margin-right: 10px;
        vertical-align: -3px;
      }

      .kpi-card {
        background: white;
        border: 1px solid #E3EAF1;
        border-left: 5px solid #087F8C;
        border-radius: 15px;
        padding: 20px;
        min-height: 150px;
        margin-bottom: 22px;
        box-shadow: 0 4px 18px rgba(31, 50, 70, 0.06);
      }
      .kpi-blue   { border-left-color: #2774C6; }
      .kpi-orange { border-left-color: #E67E22; }
      .kpi-green  { border-left-color: #19947A; }
      .kpi-purple { border-left-color: #7657A8; }
      .kpi-label {
        color: #718096; font-size: 11px; font-weight: 750;
        text-transform: uppercase; letter-spacing: 0.7px;
      }
      .kpi-value  { color: #102A43; font-size: 29px; font-weight: 750; margin-top: 8px; }
      .kpi-delta  { color: #087F8C; font-size: 12px; font-weight: 650; margin-top: 2px; }
      .kpi-description { color: #8A98A8; font-size: 12px; margin-top: 5px; }

      .research-box {
        background: linear-gradient(135deg, #E9F7F8, #F3FAFF);
        border: 1px solid #CDE8EB;
        border-left: 6px solid #087F8C;
        border-radius: 16px;
        padding: 25px 28px;
        margin-bottom: 25px;
      }
      .research-question {
        color: #102A43; font-size: 21px; font-weight: 750;
        line-height: 1.45; margin-bottom: 10px;
      }
      .finding-box {
        background: #FFF8EF;
        border: 1px solid #F5DDBF;
        border-left: 6px solid #E67E22;
        border-radius: 14px;
        padding: 18px 22px;
        margin-bottom: 24px;
        line-height: 1.55;
      }
      .warn-banner {
        background: #FFF5F5; border: 1px solid #FEB2B2; color: #9B2C2C;
        border-radius: 12px; padding: 12px 18px; margin-bottom: 18px;
      }
      .note { color: #718096; font-size: 12.5px; margin-top: 12px; }

      .method-box {
        background: white;
        border: 1px solid #E2E9F0;
        border-left: 5px solid #2774C6;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 16px;
        box-shadow: 0 2px 9px rgba(31, 50, 70, 0.04);
      }
      .method-box h4 { color: #102A43; font-weight: 750; margin-top: 0; }

      .well {
        background: white; border: 1px solid #E1E8EF; border-radius: 15px;
        box-shadow: 0 3px 12px rgba(31, 50, 70, 0.05);
      }
      .control-label { color: #29445F; font-weight: 700; font-size: 13px; }
      .form-control, .selectize-input {
        border: 1px solid #D4DEE8 !important; border-radius: 8px !important;
        box-shadow: none !important;
      }
      .btn-primary {
        background: linear-gradient(135deg, #087F8C, #0B9AA5);
        border: none; border-radius: 8px; font-weight: 650; padding: 9px 17px;
      }
      .btn-primary:hover { background: linear-gradient(135deg, #066C76, #087F8C); }

      table.dataTable thead th {
        background: #EEF6F8 !important; color: #12355B !important;
        font-weight: 700; border-bottom: 2px solid #CFE4E7 !important;
      }
      table.dataTable tbody tr:hover { background: #F3FAFB !important; }
      .dataTables_wrapper { color: #425466; }
      .dataTables_length select, .dataTables_filter input {
        border: 1px solid #CBD7E2; border-radius: 7px;
      }
      .dataTables_paginate .paginate_button.current {
        background: #087F8C !important; color: white !important; border: none !important;
      }

      .dashboard-footer {
        margin-top: 35px; padding: 28px; text-align: center;
        color: #718096; font-size: 12px; border-top: 1px solid #E2E8F0;
      }

      /* Plotly sizing fixes: prevent charts collapsing in cards or tabs */
      .section-card, .plotly-frame, .tab-content, .tab-pane {
        min-width: 0 !important;
      }
      .plotly-frame {
        display: block !important;
        width: 100% !important;
        min-height: 300px;
        overflow: visible !important;
      }
      .plotly-frame .shiny-plot-output,
      .plotly-frame .html-widget,
      .plotly-frame .plotly,
      .plotly-frame .plot-container,
      .plotly-frame .svg-container {
        width: 100% !important;
        max-width: 100% !important;
      }
      .plotly-frame .shiny-plot-output {
        display: block !important;
      }
      .tab-content { overflow: visible !important; }

      @media (max-width: 900px) {
        .dashboard-header { padding: 25px; }
        .dashboard-header h1 { font-size: 24px; }
        .section-card { padding: 17px; }
        .research-question { font-size: 18px; }
      }
    "))
    ,
    tags$script(HTML("
      // Plotly can initialise while its tab is hidden, leaving a blank or
      // incorrectly sized chart. Resize all charts whenever a tab is shown.
      (function() {
        function resizeVisiblePlots() {
          if (window.Plotly && window.jQuery) {
            window.jQuery('.plotly').each(function() {
              try { window.Plotly.Plots.resize(this); } catch (e) {}
            });
          }
        }
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
            window.setTimeout(resizeVisiblePlots, 150);
            window.setTimeout(resizeVisiblePlots, 500);
          });
          window.jQuery(window).on('resize', function() {
            window.setTimeout(resizeVisiblePlots, 100);
          });
          window.jQuery(document).on('shiny:connected', function() {
            window.setTimeout(resizeVisiblePlots, 500);
          });
        }
      })();
    "))
  ),


  div(
    class = "dashboard-header",
    h1("Australian Labour Market Dashboard"),
    p("Labour demand, labour-market slack, tightness and wage pressure"),
    p("ETC-5543 | Australian Labour Market Project")
  ),

  uiOutput("data_warning"),


  tabsetPanel(

    id = "main_tabs",
    type = "tabs",

    # ========================================================
    # OVERVIEW
    # ========================================================

    tabPanel(
      "Overview",
      br(),

      uiOutput("kpi_row"),

      div(
        class = "research-box",
        div(class = "research-question", "Research Question"),
        p("To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?"),
        p("The dashboard examines statistical associations between vacancies, labour-market tightness and measures of slack. The relationships should not be interpreted as evidence of causation.")
      ),

      plot_card(
        "Labour-Market Overview", "overview_trend", "500px",
        events_checkbox("overview_events")
      ),

      section_card(
        "Dashboard Guide",
        tags$ul(
          tags$li(strong("Labour Demand: "), "Online job advertisements nationally and by state."),
          tags$li(strong("Labour-Market Slack: "), "Unemployment, underemployment, underutilisation, youth gaps and shocks."),
          tags$li(strong("Tightness & Slack: "), "Associations between vacancies and slack."),
          tags$li(strong("Market Conditions: "), "Quarters where tightness and slack were both high, and unusual quarters."),
          tags$li(strong("Research Question: "), "Main statistical relationships, by period and by timing."),
          tags$li(strong("Wage Pressure: "), "Associations with wage growth."),
          tags$li(strong("Occupation Demand: "), "Broad and detailed occupations and skill levels, by state."),
          tags$li(strong("Data Explorer: "), "Search, filter and download project data.")
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
          h4("Labour Demand Controls"),

          selectInput(
            "demand_period", "Time period:",
            choices = c("All available data", "Last 10 years",
                        "Last 5 years", "Last 3 years"),
            selected = "All available data"
          ),

          checkboxInput("demand_ma", "Overlay 12-month moving average", FALSE),
          events_checkbox("demand_events"),

          radioButtons(
            "demand_state_measure", "State comparison:",
            choices = c("Indexed (Jan 2019 = 100)", "Vacancies"),
            selected = "Indexed (Jan 2019 = 100)"
          ),

          width = 3
        ),

        mainPanel(
          plot_card("Online Job Advertisements", "ivi_national_plot", "500px"),
          plot_card("Labour Demand Growth (year-on-year)", "ivi_growth_plot", "450px"),
          plot_card("Online Job Advertisements by State and Territory", "ivi_state_plot", "500px"),
          width = 9
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
          h4("Slack Controls"),

          checkboxGroupInput(
            "slack_measures", "Measures:",
            choices = c("Unemployment", "Underemployment", "Underutilisation"),
            selected = c("Unemployment", "Underemployment", "Underutilisation")
          ),

          selectInput(
            "slack_period", "Historical period:",
            choices = c("All periods", period_levels),
            selected = "All periods"
          ),

          uiOutput("slack_date_ui"),
          events_checkbox("slack_events"),

          actionButton("reset_slack", "Reset filters", class = "btn-primary"),

          br(), br(),

          p("Underutilisation is used as the primary observable proxy for labour-market slack, while unemployment and underemployment provide complementary measures."),

          width = 3
        ),

        mainPanel(
          plot_card("Labour-Market Slack Over Time", "slack_trend", "550px"),
          plot_card("Youth vs Overall Slack", "youth_slack_plot", "500px"),
          plot_card(
            "Youth Gap (youth minus overall rate)", "youth_gap_plot", "450px",
            note("Percentage-point gap between the youth rate and the overall rate.")
          ),
          plot_card(
            "Unemployment vs Underemployment: Correlation by Period",
            "period_corr_plot", "450px",
            note("Pearson correlation between monthly unemployment and underemployment rates.")
          ),
          plot_card(
            "Slack Since COVID-19 vs the 2019 Average", "covid_recovery_plot", "450px",
            note("Percentage-point difference from the 2019 average, from March 2020.")
          ),
          table_card("Economic Shocks Compared (average rate, %)", "shock_table"),
          table_card("Historical Period Summary (average rate, %)", "slack_period_table"),
          width = 9
        )
      )
    ),


    # ========================================================
    # TIGHTNESS & SLACK
    # ========================================================

    tabPanel(
      "Tightness & Slack",
      br(),

      plot_card("Vacancies and Labour-Market Tightness", "tightness_plot", "500px"),

      plot_card(
        "Changes in Vacancies vs Changes in Slack", "tightness_slack_scatter", "500px",
        fluidRow(
          column(
            6,
            selectInput(
              "tightness_slack_measure", "Slack measure:",
              choices = c("Unemployment", "Underemployment", "Underutilisation"),
              selected = "Underutilisation"
            )
          ),
          column(
            6,
            selectInput(
              "tightness_lag", "Vacancy change leads slack by:",
              choices = c("Same quarter" = 0, "1 quarter" = 1,
                          "2 quarters" = 2, "3 quarters" = 3, "4 quarters" = 4),
              selected = 0
            )
          )
        )
      ),

      table_card(
        "Correlation Summary", "tightness_correlations",
        note("Correlations are between quarterly changes. Tightness is vacancies divided by unemployed persons, so its link with unemployment is partly mechanical.")
      )
    ),


    # ========================================================
    # MARKET CONDITIONS
    # ========================================================

    tabPanel(
      "Market Conditions",
      br(),

      section_card(
        "How Conditions Are Defined",
        p("Each quarter is classified as high or low tightness and high or low slack by comparing it with the sample median. Quarters where tightness AND underutilisation are both above their medians are flagged as potential mismatch periods: employers have many vacancies, yet labour is still underused."),
        uiOutput("conditions_thresholds")
      ),

      plot_card(
        "Quarters of High Tightness and High Slack", "conditions_plot", "500px"
      ),

      table_card("Conditions Summary", "conditions_summary_table"),

      table_card(
        "Potential Mismatch Quarters vs Other Quarters", "conditions_comparison_table"
      ),

      table_card("Potential Mismatch Quarters", "mismatch_quarters_table"),

      section_card(
        "Unusual Quarters: Demand and Slack Rising Together",
        selectInput(
          "conditions_x", "Demand measure:",
          choices = demand_cols, selected = "change_tightness"
        ),
        uiOutput("unusual_summary"),
        plotlyOutput("unusual_plot", height = "450px"),
        br(),
        DTOutput("unusual_table"),
        note("Descriptive only. A quarter is 'unusual' when the demand measure and underutilisation both rose, the opposite of what a tightening market would predict.")
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

      section_card(
        "Demand Measure",
        selectInput(
          "research_x", "Compare slack changes with changes in:",
          choices = demand_cols, selected = "change_vacancies"
        ),
        note("Job vacancies is the main measure for the research question; tightness (vacancies per unemployed person) is shown as an alternative.")
      ),

      uiOutput("research_finding"),

      plot_card(
        "Main Evidence", "research_scatter", "550px",
        p("The analysis compares quarterly changes in the demand measure with changes in unemployment, underemployment and underutilisation."),
        p("Negative correlations indicate that increases in demand tend to coincide with reductions in measured labour-market slack. These relationships are statistical associations and do not establish causality.")
      ),

      table_card("Correlation by Slack Measure", "research_correlation_table"),

      table_card(
        "Is the Relationship Stable Across Historical Periods?",
        "research_period_table",
        selectInput(
          "research_measure", "Slack measure:",
          choices = c("Unemployment", "Underemployment", "Underutilisation"),
          selected = "Underutilisation"
        ),
        note("Periods with few quarters give wide confidence intervals; interpret those with caution.")
      ),

      table_card(
        "Does Demand Lead Slack? (Lead-Lag Correlations)",
        "research_leadlag_table",
        note("Each cell correlates the change in the demand measure k quarters EARLIER with the change in slack in the current quarter.")
      )
    ),


    # ========================================================
    # WAGE PRESSURE
    # ========================================================

    tabPanel(
      "Wage Pressure",
      br(),

      plot_card("Wage Price Index Growth (quarterly %)", "wpi_plot", "500px"),
      plot_card("Tightness and Wage Growth", "tightness_wpi_scatter", "500px"),
      plot_card("Underutilisation and Wage Growth", "slack_wpi_scatter", "500px"),

      table_card(
        "Timing of Associations", "wpi_timing_table",
        note("Each column correlates the labour-market measure in quarter t with WPI growth in quarter t, t+1 and t+2.")
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
          h4("Occupation Controls"),

          selectInput(
            "occupation_level", "Occupation dataset:",
            choices = c("Broad occupations", "Detailed occupations"),
            selected = "Broad occupations"
          ),

          uiOutput("occupation_note"),

          selectInput(
            "occupation_state", "State / territory:",
            choices = state_choices,
            selected = state_codes[1]
          ),

          selectInput(
            "occupation_window", "Time period:",
            choices = c("All available data", "Last 10 years",
                        "Last 5 years", "Last 3 years"),
            selected = "Last 5 years"
          ),

          sliderInput(
            "occupation_top_n", "Number of occupations:",
            min = 3, max = 20, value = 8, step = 1
          ),

          radioButtons(
            "occupation_measure", "Trend display:",
            choices = c("Vacancies", "Indexed (start of period = 100)"),
            selected = "Vacancies"
          ),

          width = 3
        ),

        mainPanel(
          plot_card("Largest Occupations (latest month)", "occupation_latest_plot", "500px"),
          plot_card("Occupation Vacancy Trends", "occupation_trend", "550px"),
          table_card("Occupation Vacancy Table", "occupation_table"),
          plot_card("Skill-Level Demand", "skill_plot", "500px"),
          width = 9
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
          h4("Data Selection"),

          selectInput(
            "explorer_dataset", "Dataset:",
            choices = c("Labour-market slack", "Vacancy and tightness",
                        "Wage Price Index", "IVI broad occupations",
                        "IVI detailed occupations", "IVI skill level"),
            selected = "Labour-market slack"
          ),

          numericInput(
            "explorer_rows", "Rows to show:",
            value = 25, min = 10, max = 500, step = 10
          ),

          downloadButton("download_explorer", "Download current data"),

          br(), br(),

          p("Use the column filters and search box to narrow the table. The download includes only the rows currently matching your filters."),

          width = 3
        ),

        mainPanel(
          table_card("Interactive Data Explorer", "data_explorer"),
          width = 9
        )
      )
    ),


    # ========================================================
    # METHODOLOGY
    # ========================================================

    tabPanel(
      "Methodology",
      br(),

      section_card(
        "Research Design",
        p("The project investigates the association between labour demand and labour-market slack in Australia."),
        p("The analysis is descriptive and correlational. It does not attempt to identify a causal effect of vacancies on labour-market slack.")
      ),

      method_box(
        "Labour-market slack",
        "Unemployment, underemployment and underutilisation are examined using ABS Labour Force data. Underutilisation is treated as the primary observable proxy for labour-market slack because it incorporates both unemployment and underemployment."
      ),

      method_box(
        "Labour demand",
        "The Jobs and Skills Australia Internet Vacancy Index provides a monthly measure of online job advertisements. Broad occupations (ANZSCO 2-digit) and skill levels are seasonally adjusted; detailed occupations (ANZSCO 4-digit) are three-month averages and some occupation-month values are missing. ABS Job Vacancies provide a quarterly measure of vacancies."
      ),

      method_box(
        "Labour-market tightness",
        "Tightness is calculated as ABS Job Vacancies divided by the number of unemployed persons."
      ),

      method_box(
        "Quarterly changes",
        "Changes are calculated only between consecutive quarters. If a vacancy quarter is missing, the change for that quarter and the next is left blank rather than treated as a one-quarter change."
      ),

      method_box(
        "Market conditions",
        "High and low tightness and slack are defined by comparison with the sample median of tightness and the underutilisation rate. These thresholds are descriptive and depend on the sample period."
      ),

      method_box(
        "Wage pressure",
        "The Wage Price Index is used as an outcome/context measure to examine associations between labour-market tightness, slack and wage growth."
      ),

      method_box(
        "Historical periods",
        "The dashboard compares 1978–1989, 1990–1994, 1995–2007, 2008–2019, 2020–2021 and 2022–present."
      ),

      method_box(
        "Statistics reported",
        "Pearson correlation is reported with a 95% confidence interval and p-value; Spearman rank correlation is shown as a robustness check against outliers. Strength labels: |r| < 0.1 negligible, < 0.3 weak, < 0.5 moderate, otherwise strong."
      ),

      method_box(
        "Interpretation",
        "Correlation measures the strength and direction of statistical association. A negative correlation between vacancies and slack indicates that higher vacancies tend to occur alongside lower measured slack. This does not establish causation."
      ),

      method_box(
        "Limitations",
        "Quarterly changes are noisy and the series contain structural breaks (for example the early 1990s and COVID-19). Tightness uses unemployment in its denominator, so its relationship with unemployment is partly mechanical. Online job advertisements are not the same as ABS job vacancies. Series may be revised by the data providers."
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
  # DATA AVAILABILITY BANNER
  # ==========================================================

  output$data_warning <- renderUI({

    status <- c(
      "labour_market_slack.csv"       = nrow(labour_market_slack) > 0,
      "combined_tightness_slack.csv"  = nrow(combined_data) > 0,
      "labour_market_wpi.csv"         = nrow(wpi_data) > 0,
      "ivi_anzsco2_clean.csv"         = nrow(ivi_anzsco2) > 0,
      "ivi_anzsco4_clean.csv"         = nrow(ivi_anzsco4) > 0,
      "ivi_skill_clean.csv"           = nrow(ivi_skill) > 0
    )

    missing <- names(status)[!status]

    if (length(missing) == 0) return(NULL)

    div(
      class = "warn-banner",
      strong("Some data could not be loaded: "),
      paste(missing, collapse = ", "),
      ". Affected charts will show an empty message."
    )
  })


  # ==========================================================
  # KPI CARDS
  # ==========================================================

  output$kpi_row <- renderUI({

    vac   <- kpi_values(combined_data, "Quarter", "Job_Vacancies", 4)
    tight <- kpi_values(combined_data, "Quarter", "Tightness", 4)
    unemp <- kpi_values(labour_market_slack, "date", "unemployment_rate", 12)
    under <- kpi_values(labour_market_slack, "date", "underutilisation_rate", 12)

    fluidRow(
      column(
        3,
        kpi_card(
          "Job Vacancies", fmt_num(vac$latest, 0),
          delta_text(vac$pct, "%", 1),
          paste("Quarter to", vac$when), "blue"
        )
      ),
      column(
        3,
        kpi_card(
          "Labour-Market Tightness", fmt_num(tight$latest, 2),
          delta_text(tight$diff, "", 2),
          paste("Vacancies per unemployed person,", tight$when), "green"
        )
      ),
      column(
        3,
        kpi_card(
          "Unemployment", fmt_pct(unemp$latest),
          delta_text(unemp$diff, " pp", 1),
          paste("As at", unemp$when), "orange"
        )
      ),
      column(
        3,
        kpi_card(
          "Underutilisation", fmt_pct(under$latest),
          delta_text(under$diff, " pp", 1),
          paste("Primary slack measure,", under$when), "purple"
        )
      )
    )
  })


  # ==========================================================
  # OVERVIEW CHART
  # ==========================================================

  output$overview_trend <- renderPlotly({

    if (nrow(labour_market_slack) == 0) {
      return(empty_plot("Labour-market slack data are unavailable."))
    }

    data <- slack_long(labour_market_slack) |>
      filter(!is.na(date), !is.na(rate))

    if (nrow(data) == 0) return(empty_plot("No valid slack observations are available."))

    p <- ggplot(data, aes(x = date, y = rate, colour = measure)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = dashboard_palette) +
      labs(x = NULL, y = "Rate (%)", colour = "Measure") +
      theme_dashboard()

    p <- add_events(
      p, input$overview_events,
      min(data$date, na.rm = TRUE), max(data$date, na.rm = TRUE)
    )

    clean_plotly(ggplotly(p))
  })


  # ==========================================================
  # LABOUR DEMAND
  # ==========================================================

  years_window <- function(label) {
    switch(
      label,
      "Last 10 years" = 10,
      "Last 5 years"  = 5,
      "Last 3 years"  = 3,
      NA_real_
    )
  }

  demand_data <- reactive({

    if (nrow(national_ivi) == 0) return(tibble())

    years_back <- years_window(input$demand_period)

    data <- national_ivi

    if (!is.na(years_back)) {
      data <- data |> filter(Month >= max(Month) - years(years_back))
    }

    data
  })


  output$ivi_national_plot <- renderPlotly({

    data <- demand_data()

    if (nrow(data) == 0) return(empty_plot("IVI data are unavailable."))

    p <- ggplot(data, aes(x = Month, y = Vacancies)) +
      geom_area(fill = "#2774C6", alpha = 0.12) +
      geom_line(colour = "#2774C6", linewidth = 1.1) +
      labs(x = NULL, y = "Online job advertisements") +
      theme_dashboard()

    if (isTRUE(input$demand_ma)) {
      p <- p + geom_line(
        aes(y = ma12), colour = "#E67E22", linewidth = 1, na.rm = TRUE
      )
    }

    p <- add_events(
      p, input$demand_events,
      min(data$Month, na.rm = TRUE), max(data$Month, na.rm = TRUE)
    )

    clean_plotly(ggplotly(p))
  })


  output$ivi_growth_plot <- renderPlotly({

    data <- demand_data()

    if (nrow(data) == 0) return(empty_plot("IVI data are unavailable."))

    p <- ggplot(data, aes(x = Month, y = yoy_change)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#9AA5B1") +
      geom_line(colour = "#087F8C", linewidth = 1, na.rm = TRUE) +
      labs(x = NULL, y = "Year-on-year change (%)") +
      theme_dashboard()

    p <- add_events(
      p, input$demand_events,
      min(data$Month, na.rm = TRUE), max(data$Month, na.rm = TRUE)
    )

    clean_plotly(ggplotly(p))
  })


  output$ivi_state_plot <- renderPlotly({

    if (nrow(state_ivi) == 0) {
      return(empty_plot("State-level IVI data are unavailable."))
    }

    years_back <- years_window(input$demand_period)

    data <- state_ivi

    if (!is.na(years_back)) {
      data <- data |> filter(Month >= max(Month) - years(years_back))
    }

    use_index <- input$demand_state_measure != "Vacancies"

    data <- data |>
      mutate(value = if (use_index) Index else Vacancies) |>
      filter(!is.na(value))

    if (nrow(data) == 0) {
      return(empty_plot("No state observations in this period."))
    }

    states <- sort(unique(data$State))
    colours <- setNames(rep(occupation_palette, length.out = length(states)), states)

    p <- ggplot(data, aes(x = Month, y = value, colour = State)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = colours) +
      labs(
        x = NULL,
        y = if (use_index) "Index (Jan 2019 = 100)" else "Online job advertisements",
        colour = "State"
      ) +
      theme_dashboard()

    if (use_index) {
      p <- p + geom_hline(yintercept = 100, linetype = "dashed", colour = "#9AA5B1")
    }

    clean_plotly(ggplotly(p))
  })


  # ==========================================================
  # SLACK
  # ==========================================================

  output$slack_date_ui <- renderUI({

    if (nrow(labour_market_slack) == 0) {
      return(p("Date filter unavailable."))
    }

    valid_dates <- labour_market_slack$date[!is.na(labour_market_slack$date)]

    if (length(valid_dates) == 0) return(p("Date filter unavailable."))

    dateRangeInput(
      "slack_dates", "Date range:",
      start = min(valid_dates), end = max(valid_dates)
    )
  })


  filtered_slack <- reactive({

    if (nrow(labour_market_slack) == 0) return(tibble())

    data <- labour_market_slack

    if (!is.null(input$slack_dates) && length(input$slack_dates) == 2) {
      data <- data |>
        filter(date >= input$slack_dates[1], date <= input$slack_dates[2])
    }

    if (input$slack_period != "All periods") {
      data <- data |> filter(period == input$slack_period)
    }

    data
  })


  observeEvent(input$reset_slack, {

    updateCheckboxGroupInput(
      session, "slack_measures",
      selected = c("Unemployment", "Underemployment", "Underutilisation")
    )

    updateSelectInput(session, "slack_period", selected = "All periods")
    updateCheckboxInput(session, "slack_events", value = FALSE)

    valid_dates <- labour_market_slack$date[!is.na(labour_market_slack$date)]

    if (length(valid_dates) > 0) {
      updateDateRangeInput(
        session, "slack_dates",
        start = min(valid_dates), end = max(valid_dates)
      )
    }
  })


  output$slack_trend <- renderPlotly({

    data <- filtered_slack()

    if (nrow(data) == 0) {
      return(empty_plot("No slack data match the selected filters."))
    }

    if (length(input$slack_measures) == 0) {
      return(empty_plot("Please select at least one measure."))
    }

    plot_data <- slack_long(data) |>
      filter(measure %in% input$slack_measures, !is.na(date), !is.na(rate))

    if (nrow(plot_data) == 0) return(empty_plot("No valid observations for the selected measures."))

    p <- ggplot(plot_data, aes(x = date, y = rate, colour = measure)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = dashboard_palette) +
      labs(x = NULL, y = "Rate (%)", colour = "Measure") +
      theme_dashboard()

    p <- add_events(
      p, input$slack_events,
      min(plot_data$date, na.rm = TRUE), max(plot_data$date, na.rm = TRUE)
    )

    clean_plotly(ggplotly(p))
  })


  output$youth_slack_plot <- renderPlotly({

    data <- filtered_slack()

    if (nrow(data) == 0) return(empty_plot("Youth data are unavailable."))

    required_columns <- c(
      "date", "unemployment_rate", "youth_unemployment_rate",
      "underutilisation_rate", "youth_underutilisation_rate"
    )

    if (!all(required_columns %in% names(data))) {
      return(empty_plot("Youth measures are unavailable."))
    }

    plot_data <- data |>
      select(all_of(required_columns)) |>
      pivot_longer(-date, names_to = "measure", values_to = "rate") |>
      mutate(
        category = if_else(
          grepl("underutilisation", measure), "Underutilisation", "Unemployment"
        ),
        group = if_else(grepl("youth", measure), "Youth", "Overall")
      )

    p <- ggplot(plot_data, aes(x = date, y = rate, colour = group)) +
      geom_line(linewidth = 0.9) +
      facet_wrap(~category, scales = "free_y") +
      scale_colour_manual(
        values = c(
          "Overall" = dashboard_palette[["Overall"]],
          "Youth"   = dashboard_palette[["Youth"]]
        )
      ) +
      labs(x = NULL, y = "Rate (%)", colour = NULL) +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  output$youth_gap_plot <- renderPlotly({

    data <- filtered_slack()

    gap_cols <- intersect(
      c("youth_unemployment_gap", "youth_underemployment_gap",
        "youth_underutilisation_gap"),
      names(data)
    )

    if (nrow(data) == 0 || length(gap_cols) == 0) {
      return(empty_plot("Youth gap data are unavailable."))
    }

    plot_data <- data |>
      select(date, all_of(gap_cols)) |>
      pivot_longer(-date, names_to = "measure", values_to = "gap") |>
      mutate(
        measure = recode(
          measure,
          youth_unemployment_gap     = "Unemployment",
          youth_underemployment_gap  = "Underemployment",
          youth_underutilisation_gap = "Underutilisation"
        )
      )

    p <- ggplot(plot_data, aes(x = date, y = gap, colour = measure)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#9AA5B1") +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_colour_manual(values = dashboard_palette) +
      labs(x = NULL, y = "Gap (percentage points)", colour = "Measure") +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  output$period_corr_plot <- renderPlotly({

    if (nrow(labour_market_slack) == 0) {
      return(empty_plot("Slack data are unavailable."))
    }

    res <- labour_market_slack |>
      filter(!is.na(period)) |>
      group_by(Period = period) |>
      summarise(
        corr = safe_cor(unemployment_rate, underemployment_rate),
        obs  = sum(complete.cases(unemployment_rate, underemployment_rate)),
        .groups = "drop"
      ) |>
      mutate(
        label = sprintf("%.2f", corr),
        tip = paste0(
          "<b>", Period, "</b><br>Correlation: ", label,
          "<br>Observations: ", obs
        )
      )

    p <- ggplot(res, aes(x = Period, y = corr, text = tip)) +
      geom_col(fill = "#087F8C", width = 0.65, na.rm = TRUE) +
      geom_text(aes(label = label), vjust = -0.5, na.rm = TRUE) +
      geom_hline(yintercept = 0, colour = "#9AA5B1") +
      labs(x = NULL, y = "Correlation") +
      theme_dashboard()

    clean_plotly(ggplotly(p, tooltip = "text"), legend_top = FALSE)
  })


  output$covid_recovery_plot <- renderPlotly({

    if (nrow(labour_market_slack) == 0) {
      return(empty_plot("Slack data are unavailable."))
    }

    baseline <- labour_market_slack |>
      filter(year(date) == 2019)

    if (nrow(baseline) == 0) {
      return(empty_plot("No 2019 observations for the baseline."))
    }

    base <- c(
      Unemployment     = mean(baseline$unemployment_rate, na.rm = TRUE),
      Underemployment  = mean(baseline$underemployment_rate, na.rm = TRUE),
      Underutilisation = mean(baseline$underutilisation_rate, na.rm = TRUE)
    )

    plot_data <- slack_long(labour_market_slack) |>
      filter(date >= as.Date("2020-03-01")) |>
      mutate(difference = rate - base[measure])

    if (nrow(plot_data) == 0) {
      return(empty_plot("No observations since March 2020."))
    }

    p <- ggplot(plot_data, aes(x = date, y = difference, colour = measure)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#9AA5B1") +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = dashboard_palette) +
      labs(x = NULL, y = "Difference from 2019 average (pp)", colour = "Measure") +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  output$shock_table <- renderDT({

    if (nrow(labour_market_slack) == 0) {
      return(message_table("Slack data are unavailable."))
    }

    shocks <- tibble(
      Period = c("1990s recession (Jul 1990–May 1992)",
                 "Pre-COVID (Jan 2018–Feb 2020)",
                 "COVID-19 (Mar 2020–Nov 2021)"),
      start = as.Date(c("1990-07-01", "2018-01-01", "2020-03-01")),
      end   = as.Date(c("1992-05-31", "2020-02-29", "2021-11-30"))
    )

    result <- map_dfr(
      seq_len(nrow(shocks)),
      function(i) {
        d <- labour_market_slack |>
          filter(date >= shocks$start[i], date <= shocks$end[i])

        tibble(
          Period = shocks$Period[i],
          Months = nrow(d),
          Unemployment     = round(mean(d$unemployment_rate, na.rm = TRUE), 2),
          Underemployment  = round(mean(d$underemployment_rate, na.rm = TRUE), 2),
          Underutilisation = round(mean(d$underutilisation_rate, na.rm = TRUE), 2)
        )
      }
    )

    datatable(result, rownames = FALSE, options = simple_dt_options())
  })


  output$slack_period_table <- renderDT({

    if (nrow(labour_market_slack) == 0) {
      return(message_table("Slack data are unavailable."))
    }

    table_data <- labour_market_slack |>
      group_by(Period = period) |>
      summarise(
        Observations     = n(),
        Unemployment     = round(mean(unemployment_rate, na.rm = TRUE), 2),
        Underemployment  = round(mean(underemployment_rate, na.rm = TRUE), 2),
        Underutilisation = round(mean(underutilisation_rate, na.rm = TRUE), 2),
        .groups = "drop"
      )

    datatable(table_data, rownames = FALSE, options = simple_dt_options())
  })


  # ==========================================================
  # TIGHTNESS & SLACK
  # ==========================================================

  output$tightness_plot <- renderPlotly({

    if (nrow(combined_data) == 0) {
      return(empty_plot("Vacancy and tightness data are unavailable."))
    }

    plot_ly() |>
      add_trace(
        x = combined_data$Quarter, y = combined_data$Job_Vacancies,
        type = "scatter", mode = "lines", name = "Job vacancies",
        line = list(color = dashboard_palette[["Vacancies"]], width = 2),
        yaxis = "y"
      ) |>
      add_trace(
        x = combined_data$Quarter, y = combined_data$Tightness,
        type = "scatter", mode = "lines", name = "Tightness",
        line = list(color = dashboard_palette[["Tightness"]], width = 2),
        yaxis = "y2"
      ) |>
      layout(
        xaxis  = list(title = ""),
        yaxis  = list(title = "Job vacancies"),
        yaxis2 = list(
          title = "Tightness (vacancies per unemployed)",
          overlaying = "y", side = "right", showgrid = FALSE
        )
      ) |>
      clean_plotly(right = 70)
  })


  output$tightness_slack_scatter <- renderPlotly({

    if (nrow(combined_data) == 0) {
      return(empty_plot("Combined data are unavailable."))
    }

    y_column <- slack_measure_cols[[input$tightness_slack_measure]]
    k <- as.integer(input$tightness_lag)

    plot_data <- combined_data |>
      mutate(vacancy_change_x = lag(change_vacancies, k)) |>
      filter(!is.na(vacancy_change_x), !is.na(.data[[y_column]]))

    if (nrow(plot_data) < 3) return(empty_plot("Not enough observations."))

    x_lab <- if (k == 0) {
      "Change in job vacancies"
    } else {
      paste0("Change in job vacancies (", k, "q earlier)")
    }

    scatter_plot(
      plot_data, "vacancy_change_x", y_column,
      x_lab, paste("Change in", tolower(input$tightness_slack_measure))
    )
  })


  output$tightness_correlations <- renderDT({

    if (nrow(combined_data) == 0) return(message_table("Data unavailable."))

    datatable(
      format_cor_table(corr_by_measure(combined_data)),
      rownames = FALSE, options = simple_dt_options()
    )
  })


  # ==========================================================
  # MARKET CONDITIONS
  # ==========================================================

  output$conditions_thresholds <- renderUI({

    if (nrow(market_conditions) == 0) return(NULL)

    note(
      sprintf(
        "Sample medians: tightness = %.2f vacancies per unemployed person; underutilisation = %.1f%%.",
        tightness_median, underutilisation_median
      )
    )
  })


  output$conditions_plot <- renderPlotly({

    if (nrow(market_conditions) == 0) {
      return(empty_plot("Combined tightness and slack data are unavailable."))
    }

    d <- market_conditions |>
      mutate(
        tip = paste0(
          format(Quarter, "%b %Y"),
          "<br>Underutilisation: ", round(underutilisation_rate, 1), "%",
          "<br>Tightness: ", round(Tightness, 2)
        )
      )

    p <- ggplot(d, aes(x = Quarter, y = underutilisation_rate)) +
      geom_line(colour = "#2C7FB8", linewidth = 1) +
      geom_point(aes(colour = flag, text = tip), size = 2.6, alpha = 0.9) +
      geom_hline(
        yintercept = underutilisation_median,
        linetype = "dashed", colour = "#4D4D4D"
      ) +
      scale_colour_manual(
        values = c("Other quarter" = "#8C96A0", "High tightness + high slack" = "#D73027")
      ) +
      labs(x = NULL, y = "Underutilisation rate (%)", colour = NULL) +
      theme_dashboard()

    clean_plotly(ggplotly(p, tooltip = "text"))
  })


  output$conditions_summary_table <- renderDT({

    if (nrow(market_conditions) == 0) return(message_table("Data unavailable."))

    result <- market_conditions |>
      count(Conditions = conditions, name = "Quarters") |>
      mutate(`Share of quarters (%)` = round(Quarters / sum(Quarters) * 100, 1)) |>
      arrange(desc(Quarters))

    datatable(result, rownames = FALSE, options = simple_dt_options())
  })


  output$conditions_comparison_table <- renderDT({

    if (nrow(market_conditions) == 0) return(message_table("Data unavailable."))

    result <- market_conditions |>
      group_by(Group = flag) |>
      summarise(
        Quarters = n(),
        `Avg tightness`        = round(mean(Tightness, na.rm = TRUE), 2),
        `Avg unemployment`     = round(mean(unemployment_rate, na.rm = TRUE), 2),
        `Avg underemployment`  = round(mean(underemployment_rate, na.rm = TRUE), 2),
        `Avg underutilisation` = round(mean(underutilisation_rate, na.rm = TRUE), 2),
        .groups = "drop"
      )

    datatable(result, rownames = FALSE, options = simple_dt_options())
  })


  output$mismatch_quarters_table <- renderDT({

    if (nrow(market_conditions) == 0) return(message_table("Data unavailable."))

    result <- market_conditions |>
      filter(flag == mismatch_label) |>
      select(
        Quarter, any_of(c("Job_Vacancies", "unemployed")), Tightness,
        unemployment_rate, underemployment_rate, underutilisation_rate
      ) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))

    if (nrow(result) == 0) {
      return(message_table("No quarters had both high tightness and high slack."))
    }

    datatable(result, rownames = FALSE, options = datatable_options(10))
  })


  unusual_data <- reactive({

    if (nrow(combined_data) == 0) return(tibble())

    x <- input$conditions_x

    combined_data |>
      filter(!is.na(.data[[x]]), !is.na(change_underutilisation)) |>
      mutate(
        demand_change = .data[[x]],
        direction = case_when(
          demand_change > 0 & change_underutilisation > 0 ~ "Both rose (unusual)",
          (demand_change > 0 & change_underutilisation < 0) |
            (demand_change < 0 & change_underutilisation > 0) ~ "Opposite directions",
          TRUE ~ "Other"
        )
      )
  })


  output$unusual_summary <- renderUI({

    d <- unusual_data()

    if (nrow(d) == 0) return(NULL)

    total    <- nrow(d)
    unusual  <- sum(d$direction == "Both rose (unusual)")
    opposite <- sum(d$direction == "Opposite directions")

    p(
      sprintf(
        "%d of %d quarters (%.0f%%) show demand and underutilisation rising together. %d quarters (%.0f%%) move in opposite directions, as a tightening market would predict.",
        unusual, total, 100 * unusual / total,
        opposite, 100 * opposite / total
      )
    )
  })


  output$unusual_plot <- renderPlotly({

    d <- unusual_data()

    if (nrow(d) == 0) return(empty_plot("Combined data are unavailable."))

    d <- d |>
      mutate(
        tip = paste0(
          format(Quarter, "%b %Y"),
          "<br>Demand change: ", round(demand_change, 2),
          "<br>Underutilisation change: ", round(change_underutilisation, 2)
        )
      )

    p <- ggplot(d, aes(x = Quarter, y = change_underutilisation)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#9AA5B1") +
      geom_point(aes(colour = direction, text = tip), size = 2.6, alpha = 0.85) +
      scale_colour_manual(
        values = c(
          "Both rose (unusual)" = "#D73027",
          "Opposite directions" = "#2774C6",
          "Other"               = "#8C96A0"
        )
      ) +
      labs(x = NULL, y = "Change in underutilisation (pp)", colour = NULL) +
      theme_dashboard()

    clean_plotly(ggplotly(p, tooltip = "text"))
  })


  output$unusual_table <- renderDT({

    d <- unusual_data()

    if (nrow(d) == 0) return(message_table("Data unavailable."))

    result <- d |>
      filter(direction == "Both rose (unusual)") |>
      arrange(desc(change_underutilisation)) |>
      select(
        Quarter, demand_change, change_unemployment,
        change_underemployment, change_underutilisation
      ) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))

    if (nrow(result) == 0) {
      return(message_table("No quarters where both rose."))
    }

    datatable(result, rownames = FALSE, options = datatable_options(10))
  })


  # ==========================================================
  # RESEARCH QUESTION
  # ==========================================================

  research_label <- reactive({
    names(demand_cols)[match(input$research_x, demand_cols)]
  })


  output$research_finding <- renderUI({

    if (nrow(combined_data) == 0) return(NULL)

    res <- corr_by_measure(combined_data, input$research_x) |>
      filter(Measure == "Underutilisation")

    if (nrow(res) == 0 || is.na(res$r)) return(NULL)

    div(
      class = "finding-box",
      strong("Key finding: "),
      sprintf(
        "Quarterly changes in %s show a %s %s correlation with changes in underutilisation (r = %.2f, 95%% CI [%.2f, %.2f], n = %d). The demand measure accounts for roughly %.0f%% of the variation in underutilisation changes. This is an association, not evidence of causation.",
        tolower(research_label()),
        tolower(strength_label(res$r)),
        tolower(direction_label(res$r)),
        res$r, res$ci_low, res$ci_high, res$n,
        100 * res$r^2
      )
    )
  })


  output$research_scatter <- renderPlotly({

    if (nrow(combined_data) == 0) {
      return(empty_plot("Combined data are unavailable."))
    }

    plot_data <- combined_data |>
      select(Quarter, demand_change = all_of(input$research_x),
             all_of(unname(slack_measure_cols))) |>
      pivot_longer(
        all_of(unname(slack_measure_cols)),
        names_to = "measure", values_to = "change_slack"
      ) |>
      mutate(
        measure = names(slack_measure_cols)[match(measure, slack_measure_cols)]
      ) |>
      filter(!is.na(demand_change), !is.na(change_slack))

    if (nrow(plot_data) < 3) return(empty_plot("Not enough observations."))

    p <- ggplot(plot_data, aes(x = demand_change, y = change_slack)) +
      geom_point(colour = "#2774C6", alpha = 0.65, size = 2.5) +
      geom_smooth(
        method = "lm", formula = y ~ x, se = TRUE,
        colour = "#E67E22", fill = "#E67E22", alpha = 0.15
      ) +
      facet_wrap(~measure, scales = "free_y") +
      labs(
        x = paste("Change in", tolower(research_label())),
        y = "Change in labour-market slack (pp)"
      ) +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  output$research_correlation_table <- renderDT({

    if (nrow(combined_data) == 0) return(message_table("Data unavailable."))

    datatable(
      format_cor_table(corr_by_measure(combined_data, input$research_x)),
      rownames = FALSE, options = simple_dt_options()
    )
  })


  output$research_period_table <- renderDT({

    if (nrow(combined_data) == 0) return(message_table("Data unavailable."))

    x <- input$research_x
    y_column <- slack_measure_cols[[input$research_measure]]

    res <- combined_data |>
      filter(!is.na(period)) |>
      group_by(Period = period) |>
      group_modify(~ cor_stats(.x[[x]], .x[[y_column]])) |>
      ungroup() |>
      mutate(Period = as.character(Period))

    datatable(
      format_cor_table(res),
      rownames = FALSE, options = simple_dt_options()
    )
  })


  output$research_leadlag_table <- renderDT({

    if (nrow(combined_data) == 0) return(message_table("Data unavailable."))

    datatable(
      leadlag_table(combined_data, input$research_x),
      rownames = FALSE, options = simple_dt_options()
    )
  })


  # ==========================================================
  # WAGE PRESSURE
  # ==========================================================

  output$wpi_plot <- renderPlotly({

    if (nrow(wpi_data) == 0) return(empty_plot("WPI data are unavailable."))

    p <- ggplot(wpi_data, aes(x = wpi_date, y = WPI_growth)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#9AA5B1") +
      geom_line(colour = "#E67E22", linewidth = 1.1, na.rm = TRUE) +
      labs(x = NULL, y = "WPI growth (%)") +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  wpi_combined_data <- reactive({

    if (nrow(combined_data) == 0 || nrow(wpi_data) == 0) return(tibble())

    wpi_quarterly <- wpi_data |>
      mutate(year = year(wpi_date), quarter = quarter(wpi_date)) |>
      select(year, quarter, WPI_growth) |>
      distinct(year, quarter, .keep_all = TRUE)

    combined_data |>
      mutate(year = year(Quarter), quarter = quarter(Quarter)) |>
      left_join(wpi_quarterly, by = c("year", "quarter")) |>
      arrange(Quarter)
  })


  output$tightness_wpi_scatter <- renderPlotly({

    data <- wpi_combined_data()

    if (nrow(data) == 0) {
      return(empty_plot("WPI and tightness data are unavailable."))
    }

    data <- data |> filter(!is.na(Tightness), !is.na(WPI_growth))

    if (nrow(data) < 3) {
      return(empty_plot("Not enough matched observations."))
    }

    scatter_plot(
      data, "Tightness", "WPI_growth",
      "Labour-market tightness", "WPI growth (%)",
      point_colour = "#087F8C"
    )
  })


  output$slack_wpi_scatter <- renderPlotly({

    data <- wpi_combined_data()

    if (nrow(data) == 0) {
      return(empty_plot("WPI and slack data are unavailable."))
    }

    data <- data |> filter(!is.na(underutilisation_rate), !is.na(WPI_growth))

    if (nrow(data) < 3) {
      return(empty_plot("Not enough matched observations."))
    }

    scatter_plot(
      data, "underutilisation_rate", "WPI_growth",
      "Underutilisation (%)", "WPI growth (%)",
      point_colour = "#7657A8"
    )
  })


  output$wpi_timing_table <- renderDT({

    data <- wpi_combined_data()

    if (nrow(data) == 0) {
      return(message_table("WPI timing analysis is unavailable."))
    }

    data <- data |>
      arrange(Quarter) |>
      mutate(
        WPI_lead1 = lead(WPI_growth, 1),
        WPI_lead2 = lead(WPI_growth, 2)
      )

    result <- tibble(
      Measure = c("Tightness", "Underutilisation"),
      `Same quarter` = c(
        safe_cor(data$Tightness, data$WPI_growth),
        safe_cor(data$underutilisation_rate, data$WPI_growth)
      ),
      `One quarter ahead` = c(
        safe_cor(data$Tightness, data$WPI_lead1),
        safe_cor(data$underutilisation_rate, data$WPI_lead1)
      ),
      `Two quarters ahead` = c(
        safe_cor(data$Tightness, data$WPI_lead2),
        safe_cor(data$underutilisation_rate, data$WPI_lead2)
      )
    ) |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    datatable(result, rownames = FALSE, options = simple_dt_options())
  })


  # ==========================================================
  # OCCUPATION DEMAND
  # ==========================================================

  output$occupation_note <- renderUI({

    if (input$occupation_level == "Detailed occupations") {
      note("Detailed occupations (ANZSCO 4-digit) are three-month averages. Some occupation-months are missing. Use the table search to find a specific occupation.")
    } else {
      note("Broad occupations (ANZSCO 2-digit) are seasonally adjusted.")
    }
  })


  # Chosen dataset, restricted to the chosen state / territory
  selected_occupation_data <- reactive({

    data <- if (input$occupation_level == "Detailed occupations") {
      ivi_anzsco4
    } else {
      occupation_data
    }

    if (nrow(data) == 0 || !"Title" %in% names(data)) return(tibble())

    filter_state(data, input$occupation_state)
  })


  occupation_unavailable <- function() {
    empty_plot("No occupation data for this dataset and state / territory.")
  }


  output$occupation_latest_plot <- renderPlotly({

    data <- selected_occupation_data()

    if (nrow(data) == 0) return(occupation_unavailable())

    data <- data |> filter(!is.na(Vacancies), !is.na(Month))

    if (nrow(data) == 0) return(occupation_unavailable())

    latest <- max(data$Month)

    top <- data |>
      filter(Month == latest) |>
      group_by(Title) |>
      summarise(Vacancies = sum(Vacancies, na.rm = TRUE), .groups = "drop") |>
      slice_max(Vacancies, n = input$occupation_top_n, with_ties = FALSE) |>
      mutate(
        Occupation = str_trunc(as.character(Title), 45),
        tip = paste0(Title, "<br>", format(latest, "%b %Y"), ": ",
                     comma(round(Vacancies)))
      )

    p <- ggplot(top, aes(x = reorder(Occupation, Vacancies), y = Vacancies, text = tip)) +
      geom_col(fill = "#2774C6") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = paste("Vacancies,", format(latest, "%b %Y"))) +
      theme_dashboard()

    clean_plotly(ggplotly(p, tooltip = "text"), legend_top = FALSE)
  })


  occupation_monthly <- reactive({

    data <- selected_occupation_data()

    if (nrow(data) == 0) return(tibble())

    data <- data |>
      filter(!is.na(Month), !is.na(Vacancies), !is.na(Title)) |>
      group_by(Month, Title) |>
      summarise(Vacancies = sum(Vacancies, na.rm = TRUE), .groups = "drop")

    if (nrow(data) == 0) return(tibble())

    years_back <- years_window(input$occupation_window)

    if (!is.na(years_back)) {
      data <- data |> filter(Month >= max(Month) - years(years_back))
    }

    data
  })


  output$occupation_trend <- renderPlotly({

    data <- occupation_monthly()

    if (nrow(data) == 0) return(occupation_unavailable())

    top_titles <- data |>
      group_by(Title) |>
      summarise(total = sum(Vacancies, na.rm = TRUE), .groups = "drop") |>
      slice_max(total, n = input$occupation_top_n, with_ties = FALSE) |>
      pull(Title)

    plot_data <- data |>
      filter(Title %in% top_titles) |>
      group_by(Title) |>
      arrange(Month, .by_group = TRUE) |>
      mutate(
        Index = Vacancies / first(Vacancies[Vacancies > 0]) * 100,
        value = if (input$occupation_measure == "Vacancies") Vacancies else Index,
        Occupation = str_trunc(as.character(Title), 40)
      ) |>
      ungroup()

    occupations <- unique(plot_data$Occupation)
    colours <- setNames(
      rep(occupation_palette, length.out = length(occupations)),
      occupations
    )

    y_lab <- if (input$occupation_measure == "Vacancies") {
      "Vacancies"
    } else {
      "Index (start of period = 100)"
    }

    p <- ggplot(plot_data, aes(x = Month, y = value, colour = Occupation)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_colour_manual(values = colours) +
      labs(x = NULL, y = y_lab, colour = "Occupation") +
      theme_dashboard(12) +
      theme(legend.position = "right")

    if (input$occupation_measure != "Vacancies") {
      p <- p + geom_hline(yintercept = 100, linetype = "dashed", colour = "#9AA5B1")
    }

    clean_plotly(ggplotly(p), legend_top = FALSE)
  })


  output$occupation_table <- renderDT({

    data <- selected_occupation_data()

    if (nrow(data) == 0) {
      return(message_table("No occupation data for this dataset and state / territory."))
    }

    id_cols <- intersect(c("ANZSCO_CODE", "Title"), names(data))

    result <- data |>
      filter(!is.na(Title), !is.na(Month), !is.na(Vacancies)) |>
      group_by(across(all_of(c(id_cols, "Month")))) |>
      summarise(Vacancies = sum(Vacancies, na.rm = TRUE), .groups = "drop") |>
      group_by(across(all_of(id_cols))) |>
      arrange(Month, .by_group = TRUE) |>
      summarise(
        `Average monthly vacancies` = mean(Vacancies),
        `Latest month`              = last(Vacancies),
        prior                       = if (n() > 12) nth(Vacancies, n() - 12) else NA_real_,
        `12-month change (%)`       = (`Latest month` / prior - 1) * 100,
        .groups = "drop"
      ) |>
      select(-prior) |>
      arrange(desc(`Average monthly vacancies`)) |>
      mutate(
        `Average monthly vacancies` = round(`Average monthly vacancies`, 0),
        `Latest month`              = round(`Latest month`, 0),
        `12-month change (%)`       = round(`12-month change (%)`, 1)
      ) |>
      rename(any_of(c(`ANZSCO code` = "ANZSCO_CODE")))

    datatable(result, rownames = FALSE, options = datatable_options(10))
  })


  output$skill_plot <- renderPlotly({

    if (nrow(ivi_skill) == 0 || !"Skill" %in% names(ivi_skill)) {
      return(empty_plot("Skill-level IVI data are unavailable."))
    }

    plot_data <- ivi_skill |>
      filter_state(input$occupation_state) |>
      filter(!is.na(Month), !is.na(Vacancies), !is.na(Skill)) |>
      group_by(Month, Skill) |>
      summarise(Vacancies = sum(Vacancies, na.rm = TRUE), .groups = "drop")

    if (nrow(plot_data) == 0) {
      return(empty_plot("No skill-level data for this state / territory."))
    }

    years_back <- years_window(input$occupation_window)

    if (!is.na(years_back)) {
      plot_data <- plot_data |>
        filter(Month >= max(Month) - years(years_back))
    }

    plot_data <- plot_data |>
      mutate(Skill = factor(Skill, levels = unique(c(skill_order, Skill))))

    skills <- levels(droplevels(plot_data$Skill))
    colours <- setNames(rep(occupation_palette, length.out = length(skills)), skills)

    p <- ggplot(plot_data, aes(x = Month, y = Vacancies, colour = Skill)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = colours) +
      labs(x = NULL, y = "Vacancies", colour = "Skill level") +
      theme_dashboard()

    clean_plotly(ggplotly(p))
  })


  # ==========================================================
  # DATA EXPLORER
  # ==========================================================

  explorer_data <- reactive({

    switch(
      input$explorer_dataset,
      "Labour-market slack"      = labour_market_slack,
      "Vacancy and tightness"    = combined_data,
      "Wage Price Index"         = wpi_data,
      "IVI broad occupations"    = occupation_data,
      "IVI detailed occupations" = ivi_anzsco4,
      "IVI skill level"          = ivi_skill,
      labour_market_slack
    )
  })


  output$data_explorer <- renderDT({

    data <- explorer_data()

    if (nrow(data) == 0) {
      return(message_table("No data available for this dataset."))
    }

    datatable(
      data,
      filter = "top",
      rownames = FALSE,
      options = datatable_options(input$explorer_rows)
    )
  })


  output$download_explorer <- downloadHandler(

    filename = function() {
      paste0(
        "australian_labour_market_",
        gsub(" ", "_", tolower(input$explorer_dataset)),
        ".csv"
      )
    },

    content = function(file) {

      data <- explorer_data()

      rows <- input$data_explorer_rows_all

      if (!is.null(rows) && length(rows) > 0 && nrow(data) > 0) {
        data <- data[rows, , drop = FALSE]
      }

      write_csv(data, file)
    }
  )
}


# ============================================================
# 13. RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)