# Australian Labour Market: Labour Demand and Labour-Market Slack

## Project Overview

This project investigates whether Australian labour-market data can
provide an early indication of changes in economic conditions.

The project examines two connected aspects of the Australian labour
market:

1. **Labour-market slack** — including unemployment, underemployment,
   labour underutilisation and youth labour-market outcomes.

2. **Labour demand** — measured primarily through job vacancies.

The two components are initially analysed separately so that each
dataset can be properly understood, cleaned and validated. They will
then be combined to investigate whether changes in labour demand are
associated with subsequent changes in labour-market slack.

---

## Research Question

To what extent can changes in labour demand, measured through job
vacancies, provide an early indication of changes in labour-market
slack in Australia?

The project does not assume that changes in labour demand cause changes
in labour - market slack. Instead, the relationship will be investigated
empirically using historical Australian labour-market data.

---

# Team Contributions

## Rimlan — Labour-Market Slack

Rimlan's component focuses on understanding the extent to which
available labour is not being fully utilised in Australia.

The main measures include:

- Unemployment rate
- Underemployment rate
- Labour underutilisation rate
- Labour force participation rate
- Employment
- Youth unemployment rate
- Youth underemployment rate
- Youth underutilisation rate

### Research Focus

**How has labour-market slack in Australia changed over time, and how
have unemployment, underemployment and youth labour-market outcomes
responded to changing economic conditions and major shocks?**

The analysis initially uses monthly Australian Bureau of Statistics
(ABS) labour-market data.

The analysis will examine long-term trends, differences between
unemployment and underemployment, changes in labour underutilisation,
youth labour-market outcomes and major periods of economic disruption.

---

## Trisha — Labour Demand

Trisha's component focuses on changes in labour demand using Australian
job vacancy data.

The analysis will investigate:

- Job vacancy levels
- Changes in job vacancies
- Vacancy growth
- Labour demand conditions
- Industry-level vacancy patterns
- Changes in labour demand across different economic periods

The primary source for this component is the Jobs and Skills Australia
Internet Vacancy Index.

---

# Connecting the Two Components

The two components of the project examine different but closely related
aspects of the Australian labour market.

Rimlan's analysis focuses on labour-market slack, measuring the extent to
which available labour is not being fully utilised through unemployment,
underemployment and labour underutilisation.

Trisha's analysis focuses on labour demand, using job vacancy data to
measure changes in employer demand for workers.

After each component has been independently cleaned and validated, the
two datasets will be combined to investigate whether changes in labour
demand are associated with subsequent changes in labour-market slack.

In particular, the analysis will examine whether changes in job
vacancies occur before changes in unemployment, underemployment or
labour underutilisation. This will help determine whether vacancy data
contains useful information about future labour-market conditions.

The analysis will treat this as an empirical relationship to be tested
rather than assuming that a decline in vacancies necessarily causes an
increase in labour-market slack.

---

# Data Sources

## Australian Bureau of Statistics

The Australian Bureau of Statistics (ABS) is the primary source for
the labour-market slack analysis.

Key datasets include:

- Labour Force, Australia
- Labour underutilisation measures
- Youth labour-force measures
- Consumer Price Index, where relevant
- Australian National Accounts, where relevant

The ABS data provides the main measures used to assess labour-market
conditions and changes in labour-market slack over time.

---

## Jobs and Skills Australia

Jobs and Skills Australia's Internet Vacancy Index is used to measure
changes in labour demand through job vacancies.

The vacancy data will be used to investigate changes in employer demand
and its potential relationship with subsequent labour-market outcomes.

---

## Reserve Bank of Australia

Reserve Bank of Australia data may be incorporated to provide
information about monetary-policy conditions.

The main variable of interest is the:

- Cash Rate Target

This information may be used alongside labour-market and vacancy data
to examine broader economic conditions.

---

## Other Economic Indicators

Additional economic variables may be incorporated as the analysis
develops.

Potential variables include:

- Inflation
- Real GDP growth
- Interest rates
- Other relevant economic indicators

These variables will only be included where they contribute directly to
understanding the research question.

---

# Analytical Approach

The project will be developed in several stages.

## 1. Data Understanding and Validation

Each component will first be developed independently.

This includes:

- Identifying the appropriate statistical series
- Understanding variable definitions
- Checking data frequency
- Checking seasonal adjustment
- Identifying missing observations
- Checking for duplicated observations
- Checking units and ranges
- Validating relationships between related measures
- Documenting the data sources and methodology

---

## 2. Labour-Market Slack Analysis

Rimlan's analysis will examine changes in:

- Unemployment
- Underemployment
- Labour underutilisation
- Labour force participation
- Employment
- Youth unemployment
- Youth underemployment
- Youth underutilisation

The analysis will compare these measures over time to determine whether
unemployment alone adequately captures the amount of unused labour
capacity in Australia.

---

## 3. Youth Labour-Market Analysis

Youth labour-market outcomes will be compared with overall
labour-market conditions.

The analysis will include:

- Youth unemployment rate compared with overall unemployment
- Youth unemployment gap
- Youth unemployment relative to overall unemployment
- Youth underemployment
- Youth underutilisation

This will help identify whether young workers experience different
labour-market conditions from the broader working-age population.

---

## 4. Economic Conditions and Major Periods

Changes in labour-market slack will be examined across different
economic periods.

Initial periods of interest include:

- The period before 1990
- The early 1990s recession
- The period from 1994 to 2019
- The COVID-19 period
- The post-COVID recovery
- More recent labour-market conditions

These periods are used as an initial analytical framework and may be
refined as the research develops.

---

## 5. Broader Economic Conditions

Relevant economic indicators will be incorporated to examine how
labour-market slack changes alongside broader economic conditions.

Potential relationships include:

- Inflation and labour-market slack
- Cash rate and labour-market slack
- GDP growth and labour-market slack

The analysis will distinguish between correlation and causation and will
not interpret statistical association as evidence of a causal effect
without appropriate supporting analysis.

---

## 6. Labour Demand Analysis

Trisha's component will examine changes in job vacancies over time.

This will include:

- Vacancy levels
- Vacancy growth rates
- Changes in vacancy conditions
- Industry-level vacancy patterns
- Major changes in labour demand

The aim is to establish a reliable measure of labour demand before
combining it with the labour-market slack dataset.

---

## 7. Combined Analysis

Once both datasets have been independently validated, they will be
combined using compatible time periods.

The combined analysis will investigate:

- Relationships between vacancies and unemployment
- Relationships between vacancies and underemployment
- Relationships between vacancies and labour underutilisation
- Changes in labour demand before changes in labour-market slack
- Lead-lag relationships
- Differences across major economic periods

Particular attention will be given to whether vacancy changes provide
information about subsequent changes in labour-market slack.

---

# Forecasting Approach

After the descriptive and statistical analysis is established,
forecasting models may be developed to determine whether labour-demand
information improves predictions of labour-market slack.

The proposed modelling approach is staged.

### Model 1 — Baseline

Forecast labour-market slack using its own historical behaviour.

### Model 2 — Economic Variables

Incorporate relevant economic indicators such as:

- Inflation
- Cash rate
- GDP

### Model 3 — Labour Demand

Add job vacancy information to determine whether labour-demand
indicators provide additional predictive information.

The models will be evaluated using appropriate out-of-sample
forecasting measures.

The objective is to determine whether vacancy information improves
predictive performance compared with models that rely only on
historical labour-market information.

---

# Model Complexity

More complex statistical and machine-learning methods may be considered
after simpler models have been established.

Potential methods include:

- Regression-based models
- Random Forest
- XGBoost

The project will not use complex models simply because they are more
advanced.

Instead, model complexity will be justified only if it provides a
meaningful improvement in predictive performance.

A simpler model performing as well as or better than a more complex
model would itself be an important finding.

---

# Current Progress

## Rimlan — Labour-Market Slack

The initial labour-market dataset has been constructed using monthly
ABS data.

### Current data coverage

**February 1978 – June 2026**

### Dataset validation

The current dataset contains:

- **581 monthly observations**
- **581 unique dates**
- **No duplicated dates**
- **No missing values in the selected variables**

### Current variables

The dataset currently contains:

- Employment
- Unemployment rate
- Participation rate
- Youth unemployment rate
- Underemployment rate
- Youth underemployment rate
- Labour underutilisation rate
- Youth underutilisation rate
- Youth unemployment gap

Initial validation confirms that the labour underutilisation measure is
consistent with the combination of unemployment and underemployment
rates, with only a negligible difference attributable to numerical
precision and rounding.

### Initial analysis

Initial descriptive analysis has examined:

- Long-term unemployment trends
- Underemployment trends
- Labour underutilisation
- Youth unemployment
- Youth unemployment relative to overall unemployment
- The early 1990s recession
- The COVID-19 period
- Post-COVID labour-market conditions

These results are preliminary and will be developed further as the
project progresses.

---

## Trisha — Labour Demand

The labour-demand component is being developed using Australian job
vacancy data.

The current focus is on identifying, importing and validating the
appropriate vacancy series before combining the data with the
labour-market slack measures.

