
# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(readr)
library(rvest)
library(httr2)
library(stringr)
library(purrr)
library(lubridate)


# ------------------------------------------------------------
# 2. SET DATA FOLDERS
# ------------------------------------------------------------

raw_dir <- "data/raw"

processed_dir <- "data/processed"


dir.create(
  raw_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


dir.create(
  processed_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# 3. ABS LATEST RELEASE PAGE
# ============================================================

abs_latest_url <-
  "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release"


# ============================================================
# 4. START MESSAGE
# ============================================================

message("")

message("============================================")

message("ABS LABOUR FORCE DATA IMPORT")

message("============================================")

message("")


message(
  "Checking ABS latest-release page..."
)


# ============================================================
# 5. READ ABS LATEST RELEASE PAGE
# ============================================================

abs_page <- read_html(
  abs_latest_url
)


page_text <- html_text2(
  abs_page
)


# ============================================================
# 6. IDENTIFY REFERENCE PERIOD
# ============================================================

reference_match <- str_match(
  page_text,
  "Reference period\\s+([A-Za-z]+\\s+20[0-9]{2})"
)


if (
  is.na(
    reference_match[1, 2]
  )
) {

  stop(
    "Could not identify the ABS reference period."
  )

}


reference_period <-
  reference_match[1, 2]


message(
  "Reference period: ",
  reference_period
)


# ============================================================
# 7. IDENTIFY RELEASE DATE
# ============================================================

release_match <- str_match(
  page_text,
  "Released\\s+([0-9]{2}/[0-9]{2}/20[0-9]{2})"
)


if (
  !is.na(
    release_match[1, 2]
  )
) {

  release_date <-
    dmy(
      release_match[1, 2]
    )

} else {

  warning(
    "Could not identify the ABS release date. Using today's date."
  )

  release_date <-
    Sys.Date()

}


message(
  "Release date: ",
  release_date
)


# ============================================================
# 8. CONVERT REFERENCE PERIOD TO DATE
# ============================================================

reference_date <- parse_date_time(
  reference_period,
  orders = "B Y"
)


# ============================================================
# 9. CREATE DATA VINTAGE
# ============================================================

vintage <- format(
  release_date,
  "%Y-%m-%d"
)


message(
  "Reference period: ",
  reference_period
)


message(
  "Release date: ",
  release_date
)


message(
  "Data vintage: ",
  vintage
)


# ============================================================
# 10. FIND EXCEL DOWNLOAD LINKS
# ============================================================

excel_links <- abs_page |>

  html_elements("a") |>

  map_dfr(
    function(x) {

      tibble(

        link_text =
          html_text2(x),

        href =
          html_attr(
            x,
            "href"
          )

      )

    }

  ) |>

  filter(
    !is.na(href)
  ) |>

  mutate(

    url =
      url_absolute(
        href,
        abs_latest_url
      )

  ) |>

  filter(

    str_detect(
      url,
      regex(
        "\\.xlsx($|\\?)",
        ignore_case = TRUE
      )

    )

  ) |>

  distinct(
    url,
    .keep_all = TRUE
  )


message("")

message(
  "Excel links found: ",
  nrow(excel_links)
)


if (
  nrow(excel_links) == 0
) {

  stop(
    paste(
      "No Excel links were found.",
      "The ABS webpage structure may have changed."
    )
  )

}


# ============================================================
# 11. IDENTIFY CURRENT ABS RELEASE FOLDER
# ============================================================

release_folder_match <- str_match(
  excel_links$url[1],
  "/labour-force-australia/([^/]+)/"
)


if (
  is.na(
    release_folder_match[1, 2]
  )
) {

  stop(
    "Could not identify the current ABS release folder."
  )

}


release_folder <-
  release_folder_match[1, 2]


message(
  "ABS release folder: ",
  release_folder
)


# ============================================================
# 12. REQUIRED ABS FILES
# ============================================================

table_001_name <-
  "62020001.xlsx"


table_013_name <-
  "62020013.xlsx"


table_x29_name <-
  "62020X29.xlsx"


# ============================================================
# 13. FIND TABLE 001
# ============================================================

table_001_url <- excel_links$url[
  str_detect(
    excel_links$url,
    fixed(table_001_name)
  )
][1]


# ============================================================
# 14. FIND TABLE 013
# ============================================================

table_013_url <- excel_links$url[
  str_detect(
    excel_links$url,
    fixed(table_013_name)
  )
][1]


# ============================================================
# 15. FIND TABLE X29
# ============================================================

table_x29_url <- excel_links$url[
  str_detect(
    excel_links$url,
    fixed(table_x29_name)
  )
][1]


# ============================================================
# 16. CHECK REQUIRED LINKS
# ============================================================

required_links <- tibble(

  table = c(
    "Table 001",
    "Table 013",
    "Table X29"
  ),

  file = c(
    table_001_name,
    table_013_name,
    table_x29_name
  ),

  url = c(
    table_001_url,
    table_013_url,
    table_x29_url
  )

)


message("")

message(
  "Required ABS files:"
)


print(
  required_links
)


if (
  any(
    is.na(
      required_links$url
    )
  )
) {

  missing_files <-
    required_links$file[
      is.na(
        required_links$url
      )
    ]


  stop(
    paste(
      "Could not identify the following ABS files:",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )

}


# ============================================================
# 17. CREATE VINTAGE FOLDER
# ============================================================

vintage_dir <- file.path(
  raw_dir,
  vintage
)


dir.create(
  vintage_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


message("")

message(
  "Vintage folder: ",
  vintage_dir
)


# ============================================================
# 18. DOWNLOAD FUNCTION
# ============================================================

download_abs_file <- function(
    url,
    destination
) {

  if (
    file.exists(
      destination
    )
  ) {

    message(
      "Already downloaded: ",
      basename(destination)
    )

    return(
      invisible(FALSE)
    )

  }


  message(
    "Downloading: ",
    basename(destination)
  )


  request(url) |>

    req_user_agent(
      "Australian Labour Market Slack Project"
    ) |>

    req_perform() |>

    resp_body_raw() |>

    writeBin(
      destination
    )


  if (
    !file.exists(
      destination
    )
  ) {

    stop(
      "Download failed: ",
      basename(destination)
    )

  }


  message(
    "Saved: ",
    destination
  )


  invisible(TRUE)

}


# ============================================================
# 19. CREATE FILE PATHS
# ============================================================

table_001_file <- file.path(
  vintage_dir,
  table_001_name
)


table_013_file <- file.path(
  vintage_dir,
  table_013_name
)


table_x29_file <- file.path(
  vintage_dir,
  table_x29_name
)


# ============================================================
# 20. DOWNLOAD TABLE 001
# ============================================================

download_abs_file(
  table_001_url,
  table_001_file
)


# ============================================================
# 21. DOWNLOAD TABLE 013
# ============================================================

download_abs_file(
  table_013_url,
  table_013_file
)


# ============================================================
# 22. DOWNLOAD TABLE X29
# ============================================================

download_abs_file(
  table_x29_url,
  table_x29_file
)


# ============================================================
# 23. CHECK DOWNLOADED FILES
# ============================================================

required_files <- c(
  table_001_file,
  table_013_file,
  table_x29_file
)


if (
  !all(
    file.exists(
      required_files
    )
  )
) {

  stop(
    "One or more required ABS files were not downloaded."
  )

}


# ============================================================
# 24. SAVE DOWNLOAD METADATA
# ============================================================

abs_metadata <- tibble(

  download_date =
    Sys.Date(),

  reference_period =
    reference_period,

  reference_date =
    reference_date,

  release_date =
    release_date,

  vintage =
    vintage,

  abs_release_folder =
    release_folder,

  source_page =
    abs_latest_url,

  table_001_file =
    table_001_name,

  table_013_file =
    table_013_name,

  table_x29_file =
    table_x29_name,

  table_001_url =
    table_001_url,

  table_013_url =
    table_013_url,

  table_x29_url =
    table_x29_url

)


write_csv(

  abs_metadata,

  file.path(
    vintage_dir,
    "abs_metadata.csv"
  )

)


# ============================================================
# 25. CHECK WORKBOOK SHEETS
# ============================================================

message("")

message(
  "Table 001 sheets:"
)


print(
  excel_sheets(
    table_001_file
  )
)


message("")

message(
  "Table 013 sheets:"
)


print(
  excel_sheets(
    table_013_file
  )
)


message("")

message(
  "Table X29 sheets:"
)


print(
  excel_sheets(
    table_x29_file
  )
)


# ============================================================
# 26. IMPORT TABLE 001
# ============================================================

message("")

message(
  "Reading Table 001..."
)


table_001_raw <- read_excel(

  table_001_file,

  sheet = "Data1",

  skip = 9

)


# ------------------------------------------------------------
# Check column names before selecting variables
# ------------------------------------------------------------

message("")

message(
  "Table 001 columns:"
)


print(
  names(table_001_raw)
)


# ------------------------------------------------------------
# IMPORT REQUIRED SERIES
# ------------------------------------------------------------

labour_force <- table_001_raw |>

  select(

    date =
      `Series ID`,

    employed =
      A84423043C,

    unemployment_rate =
      A84423050A,

    participation_rate =
      A84423051C

  ) |>

  mutate(

    date =
      as.Date(date),

    employed =
      as.numeric(employed),

    unemployment_rate =
      as.numeric(
        unemployment_rate
      ),

    participation_rate =
      as.numeric(
        participation_rate
      )

  ) |>

  arrange(
    date
  )




# ============================================================
# 27. IMPORT TABLE 013
# ============================================================

message("")

message(
  "Reading Table 013..."
)


youth <- read_excel(

  table_013_file,

  sheet = "Data1",

  skip = 9

) |>

  select(

    date =
      `Series ID`,

    youth_unemployment_rate =
      A84424185C

  ) |>

  mutate(

    date =
      as.Date(date),

    youth_unemployment_rate =
      as.numeric(
        youth_unemployment_rate
      )

  ) |>

  arrange(
    date
  )


# ============================================================
# 28. IMPORT X29 UNDEREMPLOYMENT
# ============================================================

message("")

message(
  "Reading X29 underemployment..."
)


underemployment <- read_excel(

  table_x29_file,

  sheet = "Data2",

  skip = 9

) |>

  select(

    date =
      `Series ID`,

    underemployment_rate =
      A85255725J,

    youth_underemployment_rate =
      A85255677A

  ) |>

  mutate(

    date =
      as.Date(date),

    underemployment_rate =
      as.numeric(
        underemployment_rate
      ),

    youth_underemployment_rate =
      as.numeric(
        youth_underemployment_rate
      )

  ) |>

  arrange(
    date
  )


# ============================================================
# 29. IMPORT X29 UNDERUTILISATION
# ============================================================

message("")

message(
  "Reading X29 underutilisation..."
)


underutilisation <- read_excel(

  table_x29_file,

  sheet = "Data4",

  skip = 9

) |>

  select(

    date =
      `Series ID`,

    underutilisation_rate =
      A85255726K,

    youth_underutilisation_rate =
      A85255678C

  ) |>

  mutate(

    date =
      as.Date(date),

    underutilisation_rate =
      as.numeric(
        underutilisation_rate
      ),

    youth_underutilisation_rate =
      as.numeric(
        youth_underutilisation_rate
      )

  ) |>

  arrange(
    date
  )


# ============================================================
# 30. COMBINE ABS DATASETS
# ============================================================

labour_market_slack <- labour_force |>

  left_join(
    youth,
    by = "date"
  ) |>

  left_join(
    underemployment,
    by = "date"
  ) |>

  left_join(
    underutilisation,
    by = "date"
  ) |>

  arrange(
    date
  )


# ============================================================
# 31. ADD DATA VINTAGE METADATA
# ============================================================

labour_market_slack <- labour_market_slack |>

  mutate(

    data_reference_period =
      reference_period,

    data_release_date =
      release_date,

    data_vintage =
      vintage,

    data_download_date =
      Sys.Date(),

    .before = 1

  )


# ============================================================
# 32. CREATE SUPPLEMENTARY MEASURES
# ============================================================

labour_market_slack <- labour_market_slack |>

  mutate(

    youth_unemployment_gap =

      youth_unemployment_rate -
      unemployment_rate,


    youth_underemployment_gap =

      youth_underemployment_rate -
      underemployment_rate,


    youth_underutilisation_gap =

      youth_underutilisation_rate -
      underutilisation_rate,


    calculated_underutilisation_rate =

      unemployment_rate +
      underemployment_rate,


    underutilisation_difference =

      underutilisation_rate -
      calculated_underutilisation_rate

  )


# ============================================================
# 33. BASIC DATASET INFORMATION
# ============================================================

message("")

message("============================================")

message("DATASET CHECKS")

message("============================================")

message("")


obs <- nrow(
  labour_market_slack
)


unique_dates <- n_distinct(
  labour_market_slack$date
)


first_date <- min(
  labour_market_slack$date,
  na.rm = TRUE
)


last_date <- max(
  labour_market_slack$date,
  na.rm = TRUE
)


message(
  "Observations: ",
  obs
)


message(
  "Unique dates: ",
  unique_dates
)


message(
  "First date: ",
  first_date
)


message(
  "Last date: ",
  last_date
)


# ============================================================
# 34. DUPLICATE DATE CHECK
# ============================================================

duplicate_dates <- labour_market_slack |>

  count(
    date
  ) |>

  filter(
    n > 1
  )


if (
  nrow(
    duplicate_dates
  ) > 0
) {

  warning(
    "Duplicate dates detected."
  )


  print(
    duplicate_dates
  )

} else {

  message(
    "Duplicate date check: PASSED"
  )

}


# ============================================================
# 35. MISSING VALUE CHECK
# ============================================================

missing_values <- labour_market_slack |>

  summarise(

    across(
      everything(),
      ~ sum(is.na(.))
    )

  )


message("")

message(
  "Missing values:"
)


print(
  missing_values
)


# ============================================================
# 36. MONTHLY SEQUENCE CHECK
# ============================================================

date_check <- labour_market_slack |>

  arrange(
    date
  ) |>

  mutate(

    month_difference =

      (
        year(date) -
        year(lag(date))
      ) * 12 +

      (
        month(date) -
        month(lag(date))
      )

  )


date_gaps <- date_check |>

  filter(

    !is.na(
      month_difference
    ),

    month_difference != 1

  )


if (
  nrow(
    date_gaps
  ) > 0
) {

  warning(
    "Potential gaps detected in the monthly series."
  )


  print(
    date_gaps
  )

} else {

  message(
    "Monthly sequence check: PASSED"
  )

}


# ============================================================
# 37. UNDERUTILISATION CONSISTENCY CHECK
# ============================================================

underutilisation_check <- labour_market_slack |>

  summarise(

    maximum_absolute_difference =

      max(
        abs(
          underutilisation_difference
        ),
        na.rm = TRUE
      ),


    mean_absolute_difference =

      mean(
        abs(
          underutilisation_difference
        ),
        na.rm = TRUE
      )

  )


message("")

message(
  "Underutilisation consistency check:"
)


print(
  underutilisation_check
)


# ============================================================
# 38. RATE RANGE CHECK
# ============================================================

rate_check <- labour_market_slack |>

  summarise(

    unemployment_min =
      min(
        unemployment_rate,
        na.rm = TRUE
      ),

    unemployment_max =
      max(
        unemployment_rate,
        na.rm = TRUE
      ),


    underemployment_min =
      min(
        underemployment_rate,
        na.rm = TRUE
      ),

    underemployment_max =
      max(
        underemployment_rate,
        na.rm = TRUE
      ),


    underutilisation_min =
      min(
        underutilisation_rate,
        na.rm = TRUE
      ),

    underutilisation_max =
      max(
        underutilisation_rate,
        na.rm = TRUE
      ),


    participation_min =
      min(
        participation_rate,
        na.rm = TRUE
      ),

    participation_max =
      max(
        participation_rate,
        na.rm = TRUE
      )

  )


message("")

message(
  "Rate range check:"
)


print(
  rate_check
)


# ============================================================
# 39. SAVE PROCESSED DATASET
# ============================================================

processed_file <- file.path(

  processed_dir,

  "labour_market_slack.csv"

)


write_csv(

  labour_market_slack,

  processed_file

)


# ============================================================
# 40. FINAL MESSAGE
# ============================================================

message("")

message("============================================")

message("ABS IMPORT COMPLETED")

message("============================================")

message("")


message(
  "Reference period: ",
  reference_period
)


message(
  "Release date: ",
  release_date
)


message(
  "Data vintage: ",
  vintage
)


message(
  "ABS release folder: ",
  release_folder
)


message("")


message(
  "Raw files saved in:"
)


message(
  normalizePath(
    vintage_dir,
    mustWork = FALSE
  )
)


message("")


message(
  "Processed dataset:"
)


message(
  normalizePath(
    processed_file,
    mustWork = FALSE
  )
)


message("")


message(
  "Observations: ",
  obs
)


message(
  "Date range: ",
  first_date,
  " to ",
  last_date
)


message("")

message("============================================")