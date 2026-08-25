# ============================================================
# Labour Demand Project - Exploratory Data Analysis
# ============================================================

library(tidyverse)
library(scales)


# ============================================================
# 1. Import Cleaned Data
# ============================================================

ivi_anzsco2 <- read_csv(
  "data/processed/ivi_anzsco2_clean.csv",
  show_col_types = FALSE
)

ivi_skill <- read_csv(
  "data/processed/ivi_skill_clean.csv",
  show_col_types = FALSE
)

ivi_anzsco4 <- read_csv(
  "data/processed/ivi_anzsco4_clean.csv",
  show_col_types = FALSE
)

# Make sure Month is stored as Date
ivi_anzsco2 <- ivi_anzsco2 |>
  mutate(Month = as.Date(Month))

ivi_skill <- ivi_skill |>
  mutate(Month = as.Date(Month))

ivi_anzsco4 <- ivi_anzsco4 |>
  mutate(Month = as.Date(Month))


# Check imported data
glimpse(ivi_anzsco2)
glimpse(ivi_skill)
glimpse(ivi_anzsco4)


# ============================================================
# 2. Overall Australian Vacancy Trend
# ============================================================

# Extract Australian total vacancies
ivi_australia <- ivi_anzsco2 |>
  filter(
    Level == 1,
    ANZSCO_CODE == "0",
    State == "AUST"
  ) |>
  arrange(Month)

# Check
head(ivi_australia)
tail(ivi_australia)

min(ivi_australia$Month)
max(ivi_australia$Month)

dim(ivi_australia)


# ============================================================
# 3. Plot Overall Australian Vacancies
# ============================================================

ggplot(
  ivi_australia,
  aes(x = Month, y = Vacancies)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Internet Job Vacancies in Australia",
    subtitle = "Seasonally adjusted, January 2006 to July 2026",
    x = "Year",
    y = "Number of Internet Vacancies",
    caption = "Source: Jobs and Skills Australia Internet Vacancy Index"
  ) +
  theme_minimal()


# ============================================================
# 4. Calculate Changes in Vacancies
# ============================================================

ivi_australia <- ivi_australia |>
  arrange(Month) |>
  mutate(
    
    # Absolute change from previous month
    Monthly_change =
      Vacancies - lag(Vacancies),
    
    # Percentage change from previous month
    Monthly_pct_change =
      (Vacancies / lag(Vacancies) - 1) * 100,
    
    # Absolute change from same month one year earlier
    YoY_change =
      Vacancies - lag(Vacancies, 12),
    
    # Percentage change from same month one year earlier
    YoY_pct_change =
      (Vacancies / lag(Vacancies, 12) - 1) * 100
  )

# Inspect
ivi_australia |>
  select(
    Month,
    Vacancies,
    Monthly_change,
    Monthly_pct_change,
    YoY_change,
    YoY_pct_change
  ) |>
  head(15)


# ============================================================
# 5. Plot Monthly Percentage Change
# ============================================================

ggplot(
  ivi_australia,
  aes(x = Month, y = Monthly_pct_change)
) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Monthly Change in Australian Internet Vacancies",
    x = "Year",
    y = "Monthly Change (%)"
  ) +
  theme_minimal()


# ============================================================
# 6. Plot Year-on-Year Vacancy Growth
# ============================================================

ggplot(
  ivi_australia,
  aes(x = Month, y = YoY_pct_change)
) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Year-on-Year Growth in Australian Internet Vacancies",
    x = "Year",
    y = "Year-on-Year Change (%)"
  ) +
  theme_minimal()


# ============================================================
# 7. Highest and Lowest Vacancy Levels
# ============================================================

# Five months with highest vacancies
ivi_australia |>
  slice_max(
    order_by = Vacancies,
    n = 5
  ) |>
  select(Month, Vacancies)

# Five months with lowest vacancies
ivi_australia |>
  slice_min(
    order_by = Vacancies,
    n = 5
  ) |>
  select(Month, Vacancies)


# ============================================================
# 8. Largest Monthly Changes
# ============================================================

# Largest monthly increases
ivi_australia |>
  filter(!is.na(Monthly_pct_change)) |>
  slice_max(
    order_by = Monthly_pct_change,
    n = 5
  ) |>
  select(
    Month,
    Vacancies,
    Monthly_pct_change
  )

# Largest monthly decreases
ivi_australia |>
  filter(!is.na(Monthly_pct_change)) |>
  slice_min(
    order_by = Monthly_pct_change,
    n = 5
  ) |>
  select(
    Month,
    Vacancies,
    Monthly_pct_change
  )


# ============================================================
# 9. Largest Year-on-Year Changes
# ============================================================

# Strongest YoY growth
ivi_australia |>
  filter(!is.na(YoY_pct_change)) |>
  slice_max(
    order_by = YoY_pct_change,
    n = 5
  ) |>
  select(
    Month,
    Vacancies,
    YoY_pct_change
  )

# Largest YoY falls
ivi_australia |>
  filter(!is.na(YoY_pct_change)) |>
  slice_min(
    order_by = YoY_pct_change,
    n = 5
  ) |>
  select(
    Month,
    Vacancies,
    YoY_pct_change
  )


# ============================================================
# 10. Important Economic Periods
# ============================================================

# Global Financial Crisis
gfc <- ivi_australia |>
  filter(
    Month >= as.Date("2008-01-01"),
    Month <= as.Date("2010-12-01")
  )

# COVID-19 period
covid <- ivi_australia |>
  filter(
    Month >= as.Date("2020-01-01"),
    Month <= as.Date("2021-12-01")
  )

# Post-COVID / recent period
post_covid <- ivi_australia |>
  filter(
    Month >= as.Date("2022-01-01")
  )


# ============================================================
# 11. Plot GFC Period
# ============================================================

ggplot(
  gfc,
  aes(x = Month, y = Vacancies)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Australian Internet Vacancies During the GFC Period",
    x = "Month",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 12. Plot COVID Period
# ============================================================

ggplot(
  covid,
  aes(x = Month, y = Vacancies)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Australian Internet Vacancies During COVID-19",
    x = "Month",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 13. Plot Post-COVID Period
# ============================================================

ggplot(
  post_covid,
  aes(x = Month, y = Vacancies)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Australian Internet Vacancies Since 2022",
    x = "Month",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 14. Overall Vacancy Summary
# ============================================================

ivi_australia |>
  summarise(
    Average_vacancies =
      mean(Vacancies, na.rm = TRUE),
    
    Minimum_vacancies =
      min(Vacancies, na.rm = TRUE),
    
    Maximum_vacancies =
      max(Vacancies, na.rm = TRUE)
  )


# ============================================================
# 15. Broad Occupation Analysis
# ============================================================

# Level 2 = broad occupation groups
occupation_data <- ivi_anzsco2 |>
  filter(
    Level == 2,
    State == "AUST"
  ) |>
  arrange(Title, Month)

# Check occupation categories
occupation_data |>
  distinct(
    ANZSCO_CODE,
    Title
  )


# ============================================================
# 16. Latest Vacancy Levels by Occupation
# ============================================================

latest_month <- max(
  occupation_data$Month,
  na.rm = TRUE
)

latest_occupations <- occupation_data |>
  filter(Month == latest_month) |>
  arrange(desc(Vacancies))

latest_occupations |>
  select(
    ANZSCO_CODE,
    Title,
    Vacancies
  )


# Plot latest occupation vacancy levels
ggplot(
  latest_occupations,
  aes(
    x = reorder(Title, Vacancies),
    y = Vacancies
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Internet Vacancies by Broad Occupation",
    subtitle = paste("Latest month:", latest_month),
    x = "Occupation",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 17. Occupation Trends Over Time
# ============================================================

ggplot(
  occupation_data,
  aes(
    x = Month,
    y = Vacancies,
    group = Title
  )
) +
  geom_line() +
  facet_wrap(
    ~ Title,
    scales = "free_y"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Internet Vacancy Trends by Broad Occupation",
    x = "Year",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 18. Year-on-Year Growth by Occupation
# ============================================================

occupation_growth <- occupation_data |>
  group_by(
    ANZSCO_CODE,
    Title
  ) |>
  arrange(Month, .by_group = TRUE) |>
  mutate(
    YoY_pct_change =
      (Vacancies / lag(Vacancies, 12) - 1) * 100
  ) |>
  ungroup()

ggplot(
  occupation_growth,
  aes(
    x = Month,
    y = YoY_pct_change
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ Title,
    scales = "free_y"
  ) +
  labs(
    title = "Year-on-Year Vacancy Growth by Broad Occupation",
    x = "Year",
    y = "Year-on-Year Change (%)"
  ) +
  theme_minimal()


# ============================================================
# 19. Indexed Occupation Series
#     January 2019 = 100
# ============================================================

occupation_index <- occupation_data |>
  group_by(
    ANZSCO_CODE,
    Title
  ) |>
  mutate(
    Base =
      Vacancies[Month == as.Date("2019-01-01")],
    
    Vacancy_index =
      (Vacancies / Base) * 100
  ) |>
  ungroup()

ggplot(
  occupation_index,
  aes(
    x = Month,
    y = Vacancy_index
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 100,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ Title
  ) +
  labs(
    title = "Indexed Vacancy Demand by Broad Occupation",
    subtitle = "January 2019 = 100",
    x = "Year",
    y = "Vacancy Index"
  ) +
  theme_minimal()


# ============================================================
# 20. Skill Level Analysis
# ============================================================

# Australia-wide skill levels 1 to 5
skill_australia <- ivi_skill |>
  filter(
    State == "AUST",
    Skill_level %in% 1:5
  ) |>
  arrange(
    Skill_level,
    Month
  )

# Check categories
skill_australia |>
  distinct(
    Skill_level,
    Title
  )


# ============================================================
# 21. Plot Vacancies by Skill Level
# ============================================================

ggplot(
  skill_australia,
  aes(
    x = Month,
    y = Vacancies,
    group = factor(Skill_level),
    linetype = factor(Skill_level)
  )
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Australian Internet Vacancies by Skill Level",
    x = "Year",
    y = "Internet Vacancies",
    linetype = "Skill Level"
  ) +
  theme_minimal()


# ============================================================
# 22. Skill Level Year-on-Year Growth
# ============================================================

skill_growth <- skill_australia |>
  group_by(Skill_level) |>
  arrange(Month, .by_group = TRUE) |>
  mutate(
    YoY_pct_change =
      (Vacancies / lag(Vacancies, 12) - 1) * 100
  ) |>
  ungroup()

ggplot(
  skill_growth,
  aes(
    x = Month,
    y = YoY_pct_change,
    group = factor(Skill_level),
    linetype = factor(Skill_level)
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Year-on-Year Vacancy Growth by Skill Level",
    x = "Year",
    y = "Year-on-Year Change (%)",
    linetype = "Skill Level"
  ) +
  theme_minimal()


# ============================================================
# 23. State Analysis
# ============================================================

# Total vacancy series for each state/territory
state_data <- ivi_anzsco2 |>
  filter(
    Level == 1,
    ANZSCO_CODE == "0"
  ) |>
  arrange(
    State,
    Month
  )

# Check states
unique(state_data$State)


# ============================================================
# 24. Plot Vacancy Trends by State
# ============================================================

ggplot(
  state_data |>
    filter(State != "AUST"),
  aes(
    x = Month,
    y = Vacancies
  )
) +
  geom_line() +
  facet_wrap(
    ~ State,
    scales = "free_y"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Internet Vacancy Trends by State and Territory",
    x = "Year",
    y = "Internet Vacancies"
  ) +
  theme_minimal()


# ============================================================
# 25. Indexed State Vacancy Series
#     January 2019 = 100
# ============================================================

state_index <- state_data |>
  filter(State != "AUST") |>
  group_by(State) |>
  mutate(
    Base =
      Vacancies[Month == as.Date("2019-01-01")],
    
    Vacancy_index =
      (Vacancies / Base) * 100
  ) |>
  ungroup()

ggplot(
  state_index,
  aes(
    x = Month,
    y = Vacancy_index
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 100,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ State
  ) +
  labs(
    title = "Indexed Internet Vacancies by State and Territory",
    subtitle = "January 2019 = 100",
    x = "Year",
    y = "Vacancy Index"
  ) +
  theme_minimal()


# ============================================================
# 26. ANZSCO 4-Digit Occupations
# ============================================================

# Keep genuine occupation codes with available vacancy data
anzsco4_analysis <- ivi_anzsco4 |>
  filter(
    ANZSCO_CODE != ".",
    !is.na(Vacancies)
  )

# Latest available month
latest_anzsco4_month <- max(
  anzsco4_analysis$Month,
  na.rm = TRUE
)

# Highest vacancy 4-digit occupations in Australia
top_anzsco4 <- anzsco4_analysis |>
  filter(
    State == "AUST",
    Month == latest_anzsco4_month,
    ANZSCO_TITLE != "Australia Total"
  ) |>
  arrange(desc(Vacancies)) |>
  slice_head(n = 20)

top_anzsco4 |>
  select(
    ANZSCO_CODE,
    ANZSCO_TITLE,
    Vacancies
  )


# ============================================================
# 27. Plot Top 20 Detailed Occupations
# ============================================================

ggplot(
  top_anzsco4,
  aes(
    x = reorder(ANZSCO_TITLE, Vacancies),
    y = Vacancies
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 20 Detailed Occupations by Internet Vacancies",
    subtitle = paste(
      "Three-month average:",
      latest_anzsco4_month
    ),
    x = "Occupation",
    y = "Internet Vacancies"
  ) +
  theme_minimal()