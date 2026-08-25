# Australian Labour Market: Labour Demand and Labour-Market Slack

## About the Project

This project investigates the relationship between labour demand and
labour-market slack in Australia.

The project looks at the Australian labour market from two connected
perspectives. Rimlan is focusing on labour-market slack, while Trisha is
focusing on labour demand using job vacancy data.

Labour-market slack describes the amount of available labour that is not
being fully utilised. This includes people who are unemployed, people
who are working fewer hours than they would like, and the broader
measure of labour underutilisation.

Labour demand is measured primarily through job vacancies, which provide
an indication of employers' demand for workers.

The two parts of the project will first be developed and validated
separately. Once the data has been checked and prepared, the datasets
will be combined using a common monthly date.

The main aim is to determine whether changes in job vacancies contain
useful information about changes in labour-market slack and whether
vacancy movements tend to occur before changes in labour-market
conditions.

---

## Research Question

**To what extent do changes in job vacancies help explain changes in
labour-market slack in Australia, and how does this relationship vary
across different economic conditions and major shocks?**

The project focuses on historical relationships between labour demand
and labour-market slack.

We will not assume that changes in job vacancies cause changes in
labour-market slack. Instead, the relationship will be tested using
historical Australian data.

---

# Team Contributions

## Rimlan — Labour-Market Slack

Rimlan's part of the project focuses on measuring and understanding
labour-market slack in Australia.

The analysis uses Australian Bureau of Statistics (ABS) labour-market
data and includes:

- Unemployment rate
- Underemployment rate
- Labour underutilisation rate
- Labour-force participation rate
- Employment
- Youth unemployment rate
- Youth underemployment rate
- Youth underutilisation rate

The main research focus is:

**How has labour-market slack in Australia changed over time, and how
have unemployment, underemployment and youth labour-market outcomes
responded to changing economic conditions and major shocks?**

The analysis will first examine the behaviour of these measures over
time. It will then look at changes in the series rather than relying
only on levels, since long-term movements can make relationships between
variables difficult to interpret.

Underutilisation is particularly important because it provides a broader
measure of unused labour capacity than unemployment alone.

Youth labour-market measures will initially be treated as supplementary
outcomes. They will be compared with overall labour-market conditions
and may be included in the main analysis if the data and research
design support it.

---

## Trisha — Labour Demand

Trisha's part of the project focuses on labour demand in Australia,
using job vacancies as the main indicator.

The analysis will examine:

- Job vacancy levels
- Changes in vacancies over time
- Vacancy growth
- Vacancy rates where appropriate
- Industry-level vacancy patterns
- Changes in labour demand during different economic periods and
  major shocks

The main source for this component is the Jobs and Skills Australia
Internet Vacancy Index.

The vacancy data will be checked carefully before it is combined with
the labour-market slack data. This includes checking the definitions,
frequency, units, date coverage, seasonal adjustment, revisions and
any possible changes in the series.

---

# Bringing the Two Parts Together

The two components will be developed separately at the beginning so
that each dataset can be properly understood and validated.

Once both datasets are ready, they will be merged using a common
monthly date.

The combined analysis will investigate whether changes in job vacancies
are associated with later changes in:

- Unemployment
- Underemployment
- Labour underutilisation

The main focus will be on the timing of the relationship rather than
only comparing the variables in the same month.

For example, we will investigate whether a decline in job vacancies is
followed by an increase in labour-market slack several months later.

This will be examined using pre-specified lags, initially including:

- 0 months
- 1 month
- 3 months
- 6 months

A 12-month lag may also be considered if there is a clear reason for
including it and the available sample remains suitable.

The results will be interpreted as historical associations and possible
leading relationships. A relationship between vacancies and labour
market slack will not automatically be interpreted as evidence of
causation.

---

# Data Sources

## Australian Bureau of Statistics (ABS)

The Australian Bureau of Statistics is the main source for the
labour-market slack analysis.

The current datasets include:

- Labour Force, Australia
- Labour underutilisation measures
- Youth labour-force measures

Before analysis, the relevant definitions, units, frequency, seasonal
adjustment and date coverage will be documented.

Related measures will also be checked for consistency.

For example, labour underutilisation should be consistent with the
combination of unemployment and underemployment, allowing for small
differences caused by rounding and numerical precision.

---

## Jobs and Skills Australia (JSA)

The Jobs and Skills Australia Internet Vacancy Index is the main source
for the labour-demand component.

The project will determine the most appropriate vacancy measure for the
analysis, such as a vacancy count or vacancy rate, based on data
availability, consistency and interpretation.

The selected measure will be decided before the main analysis rather
than choosing the measure based on which produces the strongest
relationship.

---

## Reserve Bank of Australia (RBA)

Reserve Bank of Australia data may be used to provide additional
economic context.

The main variable being considered is the Cash Rate Target.

Other economic variables may also be considered if they are relevant to
the research question and can be obtained consistently over the period
being analysed.

---

# Analysis Approach

The analysis will be developed in stages.

## 1. Understand and Validate the Data

Each dataset will first be examined independently.

This includes checking:

- Variable definitions
- Data frequency
- Seasonal adjustment
- Units and denominators
- Date coverage
- Missing observations
- Duplicate dates
- Unusual or implausible observations
- Consistency between related measures
- Potential structural breaks
- Changes in methodology or measurement

The final combined dataset should contain one observation per month
with a consistent date variable.

---

## 2. Describe Labour-Market Slack

The first stage of the labour-market analysis examines unemployment,
underemployment and labour underutilisation over time.

Time-series plots and summary statistics will be used to understand:

- Long-term movements
- Short-term changes
- Differences between unemployment and underemployment
- Changes in overall labour underutilisation
- Major movements during periods of economic disruption

The analysis will not rely only on unemployment because unemployment
does not capture people who are working fewer hours than they would like.

---

## 3. Examine Youth Labour-Market Outcomes

Youth labour-market outcomes will be compared with overall
labour-market conditions.

The analysis will include:

- Youth unemployment rate
- Overall unemployment rate
- Youth unemployment gap
- Youth underemployment
- Youth underutilisation

The youth unemployment gap will be calculated as:

`Youth unemployment rate - Overall unemployment rate`

This will be used as a descriptive measure of the difference between
youth and overall unemployment.

---

## 4. Examine Changes in the Data

Looking only at levels can sometimes produce misleading relationships
because economic variables can have long-term trends.

For this reason, the analysis will compare levels with measures of
short-term change.

Possible transformations include:

### Monthly changes

For a monthly series:

`Change_t = Value_t - Value_(t-1)`

This measures the change from one month to the next.

### Year-on-year changes

For monthly data:

`Change_YoY_t = Value_t - Value_(t-12)`

This compares a month with the same month in the previous year and can
help account for seasonal patterns.

### Trend or detrended measures

Where appropriate, the analysis may also examine deviations from an
estimated underlying trend.

The final transformation will be selected based on its economic
interpretability, treatment of seasonality, statistical behaviour and
robustness.

The transformation will not be selected simply because it produces the
strongest statistical relationship.

---

# Economic Periods and Major Shocks

The project will examine how labour-market conditions changed during
different periods.

Initial periods of interest include:

- The early 1990s downturn
- The 2008–09 Global Financial Crisis period
- The 2020 COVID-19 shock
- The post-COVID recovery
- The 2022–26 period of inflation and monetary tightening

These periods will be used to provide context for the analysis.

We will not assume that each period represents a comparable recession.
Australia has a relatively limited number of major downturns, so
comparisons across periods will be made carefully.

COVID-19 will receive particular attention because labour-market
conditions and data collection were substantially affected by the
shock.

---

# Vacancy and Labour-Market Slack Analysis

Once the vacancy and labour-market slack datasets have been validated,
the project will examine their relationship.

A starting model will be based on the relationship between changes in
labour-market slack and changes in vacancies at different lags.

For example:

`Change in Slack_t = alpha + beta(Change in Vacancies_(t-k)) + error_t`

where `k` represents the selected lag.

The analysis will examine whether changes in vacancies are associated
with later changes in unemployment, underemployment or
underutilisation.

For each main specification, we will consider:

- Direction of the relationship
- Size of the estimated relationship
- Confidence intervals
- Sample size
- Selected transformation
- Selected lag
- Residual behaviour
- Possible autocorrelation

The results will be interpreted as evidence about association and
timing rather than direct evidence of causation.

---

# Forecasting

Forecasting will only be considered after the underlying
vacancy–slack relationship has been investigated.

The purpose of forecasting is to determine whether vacancy information
provides useful additional information for predicting labour-market
slack.

## Baseline Model

The baseline model will use the historical behaviour of the selected
labour-market slack measure without job vacancy information.

A simple benchmark, such as a naive or autoregressive model, will be
used as the reference point.

## Extended Model

The extended model will include job vacancy information using the
transformation and lag structure selected during the earlier analysis.

The key question is whether including vacancies improves forecasts
compared with the baseline model.

---

# What Counts as an Improvement?

A more complicated model will not automatically be considered better.

An extended model will be considered an improvement only if it performs
better on unseen data than the baseline model using the same forecasting
period and evaluation procedure.

The main evaluation measures will be:

- Mean Absolute Error (MAE)
- Root Mean Squared Error (RMSE)

The models will be evaluated using a time-ordered train/test split or
rolling-origin evaluation.

The observations will not be randomly shuffled because this would
allow information from the future to influence the model.

The main comparison will therefore be:

| Model | Vacancy information | MAE | RMSE |
|---|---|---|---|
| Baseline model | No | To calculate | To calculate |
| Baseline + vacancies | Yes | To calculate | To calculate |
| Optional machine-learning model | Yes | To calculate | To calculate |

A lower MAE or RMSE on the same out-of-sample period would indicate
better forecasting performance.

If adding vacancy information does not improve the forecast, this will
also be treated as an important result.

---

# Machine Learning

Machine-learning methods will only be considered if they provide a
clear purpose within the research question.

Possible methods include:

- Random Forest
- XGBoost
- Other suitable machine-learning methods

These methods will not be used simply because they are more complex.

If machine-learning models are included, they will be compared with
simpler statistical models using:

- The same outcome
- The same forecast horizon
- The same test period
- The same evaluation metrics
- The same information available at the time of forecasting

Given the relatively small number of monthly macroeconomic observations,
model complexity will be treated cautiously.

If a simpler model performs as well as or better than a machine-learning
model, the simpler model will be preferred.

---

# Current Progress

## Rimlan — Labour-Market Slack

The initial ABS labour-market dataset has been imported, combined and
validated.

### Current data coverage

**February 1978 – June 2026**

### Current dataset

The dataset currently contains:

- 581 monthly observations
- 581 unique dates
- No duplicated dates
- No missing values in the selected variables

### Current variables

The dataset includes:

- Employment
- Unemployment rate
- Participation rate
- Youth unemployment rate
- Underemployment rate
- Youth underemployment rate
- Labour underutilisation rate
- Youth underutilisation rate
- Youth unemployment gap

Initial checks also confirm that the labour underutilisation measure is
consistent with unemployment plus underemployment, with only a
negligible difference due to numerical precision and rounding.

Initial analysis has also examined:

- Unemployment trends
- Underemployment trends
- Labour underutilisation
- Youth unemployment
- Youth unemployment relative to overall unemployment
- The early 1990s downturn
- The COVID-19 period
- Post-COVID labour-market conditions

These results are preliminary and will be developed further as the
vacancy data is incorporated.

---

## Trisha — Labour Demand

The labour-demand component is being developed using Australian job
vacancy data from Jobs and Skills Australia.

The current focus is on identifying the most appropriate vacancy
measure and checking its definitions, coverage, frequency and
consistency before combining it with the labour-market slack data.

---

