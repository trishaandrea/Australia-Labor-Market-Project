# Australian Labour Market: Labour Demand and Labour-Market Slack

## About the Project

This project investigates the relationship between labour demand and
labour-market slack in Australia.

The project looks at the Australian labour market from two connected
perspectives. Rimlan is focusing on labour-market slack, while Trisha is
focusing on labour demand using job vacancy data.

Labour-market slack describes the amount of available labour that is not
being fully utilised. This includes people who are unemployed, people
who are working fewer hours than they would like, and broader measures
of labour underutilisation.

Labour demand is measured primarily through job vacancies, which provide
an indication of employers' demand for workers.

The two parts of the project will first be developed and validated
separately. Once the data has been checked and prepared, the datasets
will be aligned over their common sample period at an appropriate
frequency for each analysis.

The main aim is to investigate whether changes in job vacancies are
associated with changes in labour-market slack and whether vacancy
movements may provide useful information about subsequent labour-market
conditions.

---

## Research Question

**To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?**

The project examines the historical relationship between labour demand
and labour-market slack in Australia.

Job vacancies are used as an indicator of labour demand, while
labour-market slack will be measured using observable measures such as
unemployment, underemployment and labour underutilisation.

The analysis focuses on associations rather than assuming that changes
in job vacancies cause changes in labour-market slack.

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

Trisha's part of the project focuses on measuring and understanding
labour demand in Australia using job vacancy data.

The analysis examines:

- Job vacancy levels
- Changes in vacancies over time
- Broad occupation patterns
- Skill-level patterns
- Detailed occupation patterns
- State and territory patterns
- Labour-market tightness

The main source for the monthly labour-demand component is the Jobs and
Skills Australia Internet Vacancy Index (IVI).

The vacancy data is checked carefully before being combined with
labour-market slack measures. This includes examining definitions,
frequency, units, date coverage, seasonal adjustment, revisions and
possible changes in measurement.

### Labour Demand

Labour demand is currently measured using the Jobs and Skills Australia
Internet Vacancy Index. The main measure is the seasonally adjusted
Australian total IVI.

The IVI measures newly lodged online job advertisements during each
month and is used as an indicator of recruitment activity and labour
demand.

Initial EDA shows substantial changes in vacancy activity over time,
including declines around the Global Financial Crisis and COVID-19
periods, followed by a strong post-COVID recovery and a peak around
2022.

An important measurement limitation is that the IVI represents a flow
of newly lodged online advertisements rather than the stock of job
vacancies available at a particular point in time. It also does not
capture all forms of recruitment.

### Labour-Market Tightness

Following further investigation, labour-market tightness is being
considered as an additional measure of labour-market conditions.

A common starting measure is:

**Labour-market tightness = Job Vacancies / Unemployed Persons**

A higher ratio indicates a tighter labour market, meaning that there
are more vacancies relative to the number of unemployed workers.

Using the IVI directly in this ratio raises a measurement issue because
the IVI is a flow of new advertisements, while unemployed persons is a
stock measure.

For this reason, ABS Job Vacancies is being investigated as an
alternative vacancy numerator. ABS Job Vacancies measures the stock of
vacancies at a reference date, providing a more consistent stock-to-stock
comparison with unemployed persons.

The current provisional approach is:

- **Monthly labour demand:** JSA IVI, seasonally adjusted
- **Labour-market tightness:** ABS Job Vacancies / ABS Unemployed Persons,
  seasonally adjusted

The tightness measure is quarterly because ABS Job Vacancies is
available quarterly. This approach remains provisional and will be
discussed with the supervisor.

---

# Bringing the Two Parts Together

The labour-demand and labour-market slack components will initially be
developed separately so that each measure can be properly understood and
validated.

The monthly IVI and labour-market slack measures can be aligned over
their common monthly sample period.

The proposed ABS vacancy-to-unemployment tightness measure is quarterly,
so this analysis will require the relevant labour-market data to be
aligned with the quarterly ABS Job Vacancies observations.

The combined analysis will investigate whether changes in job vacancies
are associated with changes in:

- Unemployment
- Underemployment
- Labour underutilisation

The timing of these relationships may also be investigated rather than
only comparing variables contemporaneously.

Possible lag structures will be considered based on the data,
interpretability and research design rather than being treated as fixed
in advance.

The results will be interpreted as historical associations and possible
leading relationships. An observed relationship between vacancies and
labour-market slack will not automatically be interpreted as evidence
of causation.

---

# Data Sources

## Australian Bureau of Statistics (ABS)

The Australian Bureau of Statistics is the main source for the
labour-market slack analysis.

The current datasets include:

- Labour Force, Australia
- Labour underutilisation measures
- Youth labour-force measures

These data provide measures including unemployment, underemployment,
labour underutilisation, employment and labour-force participation.

Before analysis, the relevant definitions, units, frequency, seasonal
adjustment and date coverage are checked and documented.

Related measures are also checked for consistency. For example, labour
underutilisation should be broadly consistent with the combination of
unemployment and underemployment, allowing for small differences caused
by rounding and numerical precision.

## ABS Job Vacancies, Australia

ABS Job Vacancies provides quarterly estimates of the stock of job
vacancies at a reference date.

The seasonally adjusted Australian total is currently being investigated
as the vacancy numerator for the labour-market tightness measure.

One limitation is its quarterly frequency, which means the resulting
tightness measure cannot be analysed at the same monthly frequency as
the IVI.

## Jobs and Skills Australia (JSA)

The Jobs and Skills Australia Internet Vacancy Index is the main source
for the monthly labour-demand component.

The IVI provides monthly information on newly lodged online job
advertisements. The seasonally adjusted Australian total is currently
the main vacancy measure for the monthly labour-demand analysis.

More detailed IVI data are also used to examine vacancy patterns across
occupations, skill levels, and states and territories.

## Reserve Bank of Australia (RBA)

Reserve Bank of Australia information may be used to provide additional
economic and methodological context.

RBA research on labour-market tightness is particularly relevant when
considering vacancy-to-unemployment and broader vacancy-to-searcher
measures.

Other economic variables, such as the Cash Rate Target, may be
considered later if they are relevant to the research question and can
be incorporated consistently.

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

The datasets will use consistent date variables and will be aligned at
the frequency required for each analysis.

---

## 2. Describe Labour-Market Slack

The first stage of the labour-market slack analysis examines
unemployment, underemployment and labour underutilisation over time.

Time-series plots and summary statistics will be used to understand:

- Long-term movements
- Short-term changes
- Differences between unemployment and underemployment
- Changes in overall labour underutilisation
- Major movements during periods of economic disruption

The analysis will not rely only on unemployment because unemployment
does not capture people who are employed but would like to work
additional hours.

---

## 3. Describe Labour Demand

The labour-demand analysis examines the JSA IVI over time.

The initial EDA considers:

- National vacancy movements
- Broad occupation patterns
- Skill-level patterns
- Detailed occupation patterns
- State and territory patterns

The purpose of this stage is to understand the vacancy data and identify
important measurement characteristics before investigating its
relationship with labour-market slack.

---

## 4. Examine Youth Labour-Market Outcomes

Youth labour-market outcomes will be compared with overall
labour-market conditions.

The analysis includes:

- Youth unemployment rate
- Overall unemployment rate
- Youth unemployment gap
- Youth underemployment
- Youth underutilisation

The youth unemployment gap is calculated as:

`Youth unemployment rate - Overall unemployment rate`

This is used as a descriptive measure of the difference between youth
and overall unemployment.

---

## 5. Examine Changes in the Data

Looking only at levels can sometimes produce misleading relationships
because economic variables can contain persistent trends and other
low-frequency movements.

For this reason, the analysis will compare levels with measures of
change where appropriate.

Possible transformations for monthly series include:

### Monthly changes

`Change_t = Value_t - Value_(t-1)`

This measures the change from one month to the next.

### Year-on-year changes

`Change_YoY_t = Value_t - Value_(t-12)`

This compares a month with the same month in the previous year.

### Trend or detrended measures

Where appropriate, the analysis may also examine deviations from an
estimated underlying trend.

The final transformations will be selected based on economic
interpretability, treatment of seasonality, statistical behaviour and
robustness rather than simply choosing the specification that produces
the strongest relationship.

---

## 6. Examine Labour-Market Tightness

An initial quarterly labour-market tightness measure will be constructed
using:

`ABS Job Vacancies / ABS Unemployed Persons`

Both variables are seasonally adjusted stock measures and are expressed
in thousands, allowing the ratio to be interpreted as vacancies per
unemployed person.

Because ABS Job Vacancies is quarterly, monthly unemployed-person data
will be aligned with the corresponding ABS vacancy reference months.

The resulting series will first be examined descriptively before
deciding how it should be incorporated into the main analysis.

---

# Economic Periods and Major Shocks

Major economic periods may be used as historical context when
interpreting movements in the labour-market series.

Periods of interest include:

- The early 1990s downturn
- The 2008–09 Global Financial Crisis period
- The 2020 COVID-19 shock
- The post-COVID recovery
- The recent period of inflation and monetary tightening

These periods are used primarily to provide context rather than to
assume that each event had the same effect on the labour market.

Australia has a relatively limited number of major downturns, so
comparisons across periods will be made cautiously.

---

# Vacancy and Labour-Market Slack Analysis

Once the labour-demand and labour-market slack measures have been
validated, the project will investigate their relationship over their
common sample period.

A possible starting specification is:

`Change in Slack_t = alpha + beta(Change in Vacancies_(t-k)) + error_t`

where `k` represents a possible lag between vacancy movements and
labour-market slack.

The analysis may examine whether changes in vacancies are associated
with contemporaneous or subsequent changes in unemployment,
underemployment or labour underutilisation.

For each main specification, relevant diagnostics may include:

- Direction of the relationship
- Size of the estimated relationship
- Confidence intervals
- Sample size
- Selected transformation
- Selected lag
- Residual behaviour
- Possible autocorrelation

The exact modelling approach will be selected after the measurement and
data-frequency decisions have been established.

The results will be interpreted as evidence about association and timing
rather than direct evidence of causation.

---

# Forecasting

Forecasting will only be considered after the underlying vacancy–slack
relationship has been investigated and the measurement choices have
been established.

The purpose of forecasting would be to determine whether vacancy
information provides useful additional information for predicting
labour-market slack.

## Baseline Model

A baseline model would use the historical behaviour of the selected
labour-market slack measure without vacancy information.

A simple benchmark, such as a naive or autoregressive model, may be used
as the reference point.

## Extended Model

An extended model would include vacancy information using an appropriate
transformation and lag structure.

The key question would be whether including vacancy information improves
out-of-sample forecasts compared with the baseline model.

---

# What Counts as an Improvement?

A more complicated model will not automatically be considered better.

If forecasting is undertaken, models will be evaluated on unseen data
using the same forecasting period and evaluation procedure.

Possible evaluation measures include:

- Mean Absolute Error (MAE)
- Root Mean Squared Error (RMSE)

A time-ordered train/test split or rolling-origin evaluation would be
used rather than randomly shuffling observations.

A possible comparison is:

| Model | Vacancy information | MAE | RMSE |
|---|---|---|---|
| Baseline model | No | To calculate | To calculate |
| Baseline + vacancies | Yes | To calculate | To calculate |
| Optional machine-learning model | Yes | To calculate | To calculate |

Lower MAE or RMSE over the same out-of-sample period would indicate
better forecasting performance.

If vacancy information does not improve forecasting performance, this
would also be treated as an informative result.

---

# Machine Learning

Machine-learning methods will only be considered if they provide a
clear purpose within the research question and if the available sample
is sufficient.

Possible methods may include:

- Random Forest
- XGBoost
- Other suitable machine-learning methods

If included, machine-learning models would be compared with simpler
statistical models using the same outcome, forecast horizon, test
period, evaluation metrics and information set.

Given the relatively small number of Australian macroeconomic
observations, model complexity will be treated cautiously.

If a simpler model performs as well as or better than a machine-learning
model, the simpler model will be preferred.

---

# Current Progress

## Rimlan — Labour-Market Slack

The initial ABS labour-market dataset has been imported, combined and
validated.

### Current Data Coverage

**February 1978 – June 2026**

### Current Dataset

The dataset currently contains:

- 581 monthly observations
- 581 unique dates
- No duplicated dates
- No missing values in the selected variables

### Current Variables

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

Initial checks confirm that the labour underutilisation measure is
consistent with unemployment plus underemployment, with only small
differences due to numerical precision and rounding.

Initial analysis has examined:

- Unemployment trends
- Underemployment trends
- Labour underutilisation
- Youth unemployment
- Youth unemployment relative to overall unemployment
- The early 1990s downturn
- The COVID-19 period
- Post-COVID labour-market conditions

These results are preliminary and will be developed further as the
labour-demand data is incorporated.

---

## Trisha — Labour Demand

The labour-demand component is being developed using Australian vacancy
data from Jobs and Skills Australia and the Australian Bureau of
Statistics.

### JSA Internet Vacancy Index

The IVI datasets have been cleaned and prepared for analysis, covering:

- Broad occupations
- Skill levels
- Detailed occupations
- States and territories

Initial EDA shows substantial changes in Australian vacancy activity
between 2006 and 2026. Vacancies declined sharply during the Global
Financial Crisis and COVID-19 periods, followed by a strong post-COVID
recovery that peaked around 2022 before declining towards 2026.

Vacancy patterns also differ across occupations, skill levels, and
states and territories. Professionals have the highest vacancies among
broad occupation groups in the most recent period examined, while Skill
Level 1 records the highest vacancy level.

At the detailed occupation level, General Clerks, Sales Assistants
(General), and Registered Nurses are among the occupations with the
highest three-month average vacancies in July 2026.

### ABS Job Vacancies and Labour-Market Tightness

Following further investigation into labour-market tightness, ABS Job
Vacancies has been added as a potential vacancy measure for constructing
a vacancy-to-unemployment ratio.

The seasonally adjusted Australian total ABS Job Vacancies series has
been extracted and cleaned for the period from February 2006 to May
2026.

Unlike the IVI, ABS Job Vacancies is a stock measure. This allows a more
consistent comparison with the stock of unemployed persons.

Seasonally adjusted unemployed persons from ABS Labour Force data will
be used as the denominator.

The proposed quarterly measure is:

`Labour-market tightness = ABS Job Vacancies / ABS Unemployed Persons`

This measure is currently being constructed and assessed and remains
provisional pending further analysis and supervisor feedback.

---

# Next Steps

- Construct and validate the initial quarterly labour-market tightness
  measure.
- Examine the behaviour and interpretation of the tightness series.
- Discuss the vacancy and tightness measurement choices with the
  supervisor.
- Finalise the preferred measures of labour demand and labour-market
  slack.
- Align the selected labour-demand and slack measures over their common
  sample period.
- Investigate the relationship between vacancies, tightness and
  labour-market slack.
- Decide on the modelling approach after the measurement choices have
  been established.