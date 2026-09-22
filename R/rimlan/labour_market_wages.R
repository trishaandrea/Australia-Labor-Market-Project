# ============================================================ 
# Australian Labour Market: WPI Extension 
# Analysis of labour-market conditions and wage growth 
# ============================================================ 

library(tidyverse) 
library(readxl)

# ============================================================ 
# 1. LOAD AND CLEAN WPI DATA 
# ============================================================ 

wpi_data <- read_excel( 
  "data/raw/ABS_WPI.xlsx", 
  sheet = "Data1", 
  col_names = FALSE 
)

# Extract dates and all-sector seasonally adjusted WPI 
wpi_clean <- tibble( 
  date = as.numeric(wpi_data[[1]][11:126]), 
  WPI = as.numeric(wpi_data[[7]][11:126]) 
) |> 
  mutate( 
    date = as.Date(date, origin = "1899-12-30") )

# Check cleaned WPI data 

wpi_clean 
wpi_clean |> 
  summarise( 
    observations = n(), 
    first_date = min(date), 
    last_date = max(date) 
  )

# Save cleaned WPI data 

write_csv( 
  wpi_clean, 
  "data/processed/labour_market_wpi.csv" 
)

# ============================================================ 
# 2. CREATE QUARTERLY IDENTIFIERS 
# ============================================================ 

wpi_clean <- wpi_clean |>
  mutate( 
    year = year(date), 
    quarter = quarter(date) 
  ) 

wpi_clean |> 
  select(date, WPI, year, quarter) |> 
  head(12)

# ============================================================ 
# 3. LOAD EXISTING LABOUR-MARKET DATA 
# ============================================================ 

vacancy_slack <- read_csv( 
  "data/processed/combined_tightness_slack.csv", 
  show_col_types = FALSE 
) 

vacancy_slack <- vacancy_slack |> 
  mutate( 
    year = year(Quarter), 
    quarter = quarter(Quarter) 
  ) 

vacancy_slack |> 
  select(Quarter, year, quarter) |> 
  head(8)

# ============================================================ 
# 4. MERGE WPI WITH LABOUR-MARKET DATA 
# ============================================================ 

wpi_vacancy_slack <- vacancy_slack |> 
  left_join( 
    wpi_clean |> 
      select(year, quarter, WPI), 
    by = c("year", "quarter") 
  ) 

# Inspect merged data 

wpi_vacancy_slack |> 
  select( 
    Quarter, 
    year, 
    quarter, 
    WPI, 
    Job_Vacancies, 
    unemployed, 
    Tightness, 
    unemployment_rate, 
    underemployment_rate, 
    underutilisation_rate 
  ) |> 
  head(10) 

# Check coverage and missing WPI observations 

wpi_vacancy_slack |> 
  summarise( 
    observations = n(), 
    missing_WPI = sum(is.na(WPI)), 
    first_quarter = min(Quarter), 
    last_quarter = max(Quarter) 
  )

# ============================================================ 
# 5. CALCULATE QUARTERLY WPI GROWTH 
# ============================================================ 

wpi_vacancy_slack <- wpi_vacancy_slack |> 
  arrange(Quarter) |> 
  mutate( 
    WPI_growth = (WPI / lag(WPI) - 1) * 100 
  ) 

# Inspect WPI growth 

wpi_vacancy_slack |> 
  select( 
    Quarter, 
    WPI, 
    WPI_growth, 
    Tightness, 
    underutilisation_rate 
  ) |> 
  head(10) 

# Summary statistics for WPI growth 

wpi_vacancy_slack |> 
  summarise( 
    observations = sum(!is.na(WPI_growth)), 
    mean_WPI_growth = mean(WPI_growth, na.rm = TRUE), 
    median_WPI_growth = median(WPI_growth, na.rm = TRUE), 
    min_WPI_growth = min(WPI_growth, na.rm = TRUE), 
    max_WPI_growth = max(WPI_growth, na.rm = TRUE) 
  )

# ============================================================ 
# 6. LEVEL RELATIONSHIPS 
# ============================================================

# Tightness and WPI growth 

wpi_vacancy_slack |> 
  summarise( 
    correlation = cor( 
      Tightness, 
      WPI_growth, 
      use = "complete.obs" 
    ) 
  ) 

# Underutilisation and WPI growth 

wpi_vacancy_slack |> 
  summarise( 
    correlation = cor( 
      underutilisation_rate, 
      WPI_growth, 
      use = "complete.obs" 
    ) 
  )

# ============================================================
# 7. SCATTERPLOTS 
# ============================================================ 

# Labour-market tightness and wage growth 

ggplot( 
  wpi_vacancy_slack, 
  aes( 
    x = Tightness, 
    y = WPI_growth 
  ) 
) + 
  geom_point() + 
  geom_smooth( 
    method = "lm", 
    se = TRUE 
  ) + 
  labs( 
    title = "Labour-Market Tightness and Wage Growth", 
    x = "Labour-market tightness", 
    y = "Quarterly WPI growth (%)" 
  ) + 
  theme_minimal() 

# Labour-market underutilisation and wage growth 

ggplot( 
  wpi_vacancy_slack, 
  aes( 
    x = underutilisation_rate, 
    y = WPI_growth 
  ) 
) + 
  geom_point() + 
  geom_smooth( 
    method = "lm", 
    se = TRUE 
  ) + 
  labs( 
    title = "Labour-Market Underutilisation and Wage Growth", 
    x = "Underutilisation rate (%)", 
    y = "Quarterly WPI growth (%)" 
  ) + 
  theme_minimal()

# ============================================================ 
# 8. TIMING ANALYSIS 
# ============================================================

wpi_vacancy_slack <- wpi_vacancy_slack |> 
  arrange(Quarter) |>
  mutate( 
    WPI_growth_lead1 = lead(WPI_growth, 1), 
    WPI_growth_lead2 = lead(WPI_growth, 2) 
  )

tightness_timing <- wpi_vacancy_slack |> 
  summarise( 
    same_quarter = cor( 
      Tightness,
      WPI_growth, 
      use = "complete.obs" 
    ), 
    one_quarter_ahead = cor( 
      Tightness, 
      WPI_growth_lead1, 
      use = "complete.obs" 
    ), 
    two_quarters_ahead = cor( 
      Tightness, 
      WPI_growth_lead2, 
      use = "complete.obs" 
    ) 
  ) 

tightness_timing

# Underutilisation and future WPI growth 

underutilisation_timing <- wpi_vacancy_slack |> 
  summarise( 
    same_quarter = cor( 
      underutilisation_rate, 
      WPI_growth, 
      use = "complete.obs" 
    ), 
    one_quarter_ahead = cor( 
      underutilisation_rate, 
      WPI_growth_lead1, 
      use = "complete.obs" 
    ), 
    two_quarters_ahead = cor( 
      underutilisation_rate, 
      WPI_growth_lead2, 
      use = "complete.obs" 
    ) 
  ) 

underutilisation_timing

# ============================================================ 
# 9. CHANGES IN LABOUR-MARKET CONDITIONS 
# ============================================================ 

# Calculate quarter-to-quarter changes in tightness 
# and underutilisation 

wpi_vacancy_slack <- wpi_vacancy_slack |> 
  arrange(Quarter) |> 
  mutate( 
    change_tightness = Tightness - lag(Tightness), 
    change_underutilisation = underutilisation_rate - lag(underutilisation_rate) 
  ) 

# Inspect changes 
wpi_vacancy_slack |> 
  select( 
    Quarter, 
    Tightness, 
    change_tightness, 
    underutilisation_rate, 
    change_underutilisation, 
    WPI_growth 
  ) |> 
  head(10)

# Change in labour-market conditions and WPI growth

wpi_vacancy_slack |>
  summarise(
    change_tightness_correlation = cor(
      change_tightness,
      WPI_growth,
      use = "complete.obs"
    ),
    change_underutilisation_correlation = cor(
      change_underutilisation,
      WPI_growth,
      use = "complete.obs"
    )
  )

# ============================================================
# 10. SAVE FINAL WPI-LABOUR MARKET DATA
# ============================================================

write_csv(
  wpi_vacancy_slack,
  "data/processed/wpi_vacancy_slack.csv"
)