# Australian Labour Market: Labour Demand and Labour-Market Slack

## About the Project

This project investigates the relationship between labour demand and labour-market slack in Australia.

The project looks at the Australian labour market from two connected perspectives. Rimlan is focusing on labour-market slack, while Trisha is focusing on labour demand using job vacancy data.

Labour-market slack describes the amount of available labour that is not being fully utilised. It is an unobservable economic concept rather than a single directly observed ABS variable. It can therefore be quantified using observable labour-market measures such as unemployment, underemployment and broader measures of labour underutilisation.

Labour demand is measured primarily through job vacancies, which provide an indication of employers' demand for workers.

The two parts of the project will first be developed and validated separately. Once the data has been checked and prepared, the datasets will be aligned over their common sample period at an appropriate frequency for each analysis.

The main aim is to investigate whether changes in labour demand are associated with changes in labour-market slack and whether vacancy movements may provide useful information about subsequent labour-market conditions.

---

## Research Question

**To what extent are changes in job vacancies associated with changes in labour-market slack in Australia?**

The project examines the historical relationship between labour demand and labour-market slack in Australia.

Job vacancies are used as an indicator of labour demand, while labour-market slack is measured using observable measures such as unemployment, underemployment and labour underutilisation.

Because labour-market slack is not directly observable as a single variable, the project first examines how different observable measures capture different dimensions of unused labour capacity.

The analysis focuses on associations rather than assuming that changes in job vacancies cause changes in labour-market slack.

---

# Team Contributions

## Rimlan — Labour-Market Slack

Rimlan's part of the project focuses on measuring and understanding labour-market slack in Australia.

The analysis uses Australian Bureau of Statistics (ABS) labour-market data and includes:

* Unemployment rate
* Underemployment rate
* Labour underutilisation rate
* Labour-force participation rate
* Employment
* Youth unemployment rate
* Youth underemployment rate
* Youth underutilisation rate
* Wage Price Index measures for examining wage outcomes

The main research focus is:

**How can labour-market slack in Australia be quantified using observable labour-market measures, how has it changed over time, and how have unemployment, underemployment, wages and youth labour-market outcomes responded to changing economic conditions?**

A key part of the analysis is distinguishing between the **concept of labour-market slack** and the **observable measures used to quantify it**.

Unemployment captures people without employment who meet the ABS criteria for unemployment. Underemployment captures employed people who want and are available for additional hours of work. Underutilisation combines unemployment and underemployment and therefore provides a broader measure of unused labour capacity than unemployment alone.

Underutilisation is therefore being investigated as a primary observable proxy for labour-market slack, while unemployment and underemployment are retained as separate components so that different forms of slack can be examined individually.

The analysis does not assume that underutilisation represents the complete concept of labour-market slack. Other potential sources of unused labour capacity, including people outside the labour force and unused hours, are considered when interpreting the results.

---

## Understanding ABS Labour-Market Measures

A major component of Rimlan's work is understanding how the ABS defines, collects and measures the labour-market variables rather than treating the downloaded series as simple numerical observations.

The analysis documents:

* How the ABS defines employment
* How unemployment is defined
* How underemployment is defined
* How labour underutilisation is defined
* How the labour force is constructed
* How the participation rate is calculated
* How the ABS Labour Force Survey collects the information
* The distinction between stocks, rates and ratios
* Seasonal adjustment
* Data frequency
* Reference periods
* Measurement limitations
* Revisions between releases and data vintages

This is important because labour-market slack is not directly observed. The validity of the analysis therefore depends on understanding what each ABS measure actually represents and what aspects of unused labour capacity it does and does not capture.

---

## Labour-Market Slack as an Unobservable Concept

Labour-market slack is treated as a broader economic concept rather than a single observed variable.

The project considers several observable measures:

### Unemployment

Unemployment captures people who are not employed and meet the ABS criteria relating to job search and availability for work.

It therefore represents one important component of unused labour supply.

### Underemployment

Underemployment captures employed people who want and are available for additional hours of work.

This is important because a person can be classified as employed while still having unused labour capacity.

### Underutilisation

The ABS underutilisation rate combines unemployed and underemployed people relative to the labour force.

It therefore provides a broader measure of labour underutilisation than unemployment alone.

The project uses underutilisation as a strong initial proxy for labour-market slack, while recognising that it may not capture every form of potential unused labour capacity.

---

## Wage Price Index and Labour-Market Slack

The Wage Price Index (WPI) is being incorporated as an additional economic outcome rather than as a direct measure of labour-market slack.

The purpose of including the WPI is to investigate whether periods of different labour-market conditions are associated with different patterns of wage growth.

The analysis will examine questions such as:

* Do periods of lower unemployment coincide with stronger wage growth?
* Does higher underemployment correspond to weaker wage growth?
* Does broader labour underutilisation provide information about wage pressure?
* Does the relationship between labour-market slack and wage growth differ across economic periods?
* Are wage movements different during major shocks compared with more normal labour-market conditions?

The WPI will therefore provide an additional way of assessing whether the observable measures of labour-market slack behave consistently with economic theory.

The WPI is not being treated as proof that a particular level of slack is "good" or "bad". Instead, wage growth is being investigated as one potential indicator of labour-market pressure.

---

# Trisha — Labour Demand

Trisha's part of the project focuses on measuring and understanding labour demand in Australia using job vacancy data.

The analysis examines:

* Job vacancy levels
* Changes in vacancies over time
* Broad occupation patterns
* Skill-level patterns
* Detailed occupation patterns
* State and territory patterns
* Labour-market tightness

The main source for the monthly labour-demand component is the Jobs and Skills Australia Internet Vacancy Index (IVI).

The vacancy data is checked carefully before being combined with labour-market slack measures. This includes examining definitions, frequency, units, date coverage, seasonal adjustment, revisions and possible changes in measurement.

### Labour Demand

Labour demand is currently measured using the Jobs and Skills Australia Internet Vacancy Index. The main measure is the seasonally adjusted Australian total IVI.

The IVI measures newly lodged online job advertisements during each month and is used as an indicator of recruitment activity and labour demand.

Initial EDA shows substantial changes in vacancy activity over time, including declines around the Global Financial Crisis and COVID-19 periods, followed by a strong post-COVID recovery and a peak around 2022.

An important measurement limitation is that the IVI represents a flow of newly lodged online advertisements rather than the stock of job vacancies available at a particular point in time. It also does not capture all forms of recruitment.

### Labour-Market Tightness

Following further investigation, labour-market tightness is being considered as an additional measure of labour-market conditions.

A common starting measure is:

**Labour-market tightness = Job Vacancies / Unemployed Persons**

A higher ratio indicates a tighter labour market, meaning that there are more vacancies relative to the number of unemployed workers.

Using the IVI directly in this ratio raises a measurement issue because the IVI is a flow of new advertisements, while unemployed persons is a stock measure.

For this reason, ABS Job Vacancies is being investigated as an alternative vacancy numerator. ABS Job Vacancies measures the stock of vacancies at a reference date, providing a more consistent stock-to-stock comparison with unemployed persons.

The current provisional approach is:

* **Monthly labour demand:** JSA IVI, seasonally adjusted
* **Labour-market tightness:** ABS Job Vacancies / ABS Unemployed Persons, seasonally adjusted

The tightness measure is quarterly because ABS Job Vacancies is available quarterly. This approach remains provisional and will be discussed with the supervisor.

---

# Bringing the Two Parts Together

The labour-demand and labour-market slack components will initially be developed separately so that each measure can be properly understood and validated.

The monthly IVI and labour-market slack measures can be aligned over their common monthly sample period.

The proposed ABS vacancy-to-unemployment tightness measure is quarterly, so this analysis will require the relevant labour-market data to be aligned with the quarterly ABS Job Vacancies observations.

The combined analysis will investigate whether changes in labour demand are associated with changes in:

* Unemployment
* Underemployment
* Labour underutilisation
* Wage growth
* Labour-market tightness

The timing of these relationships may also be investigated rather than only comparing variables contemporaneously.

Possible lag structures will be considered based on the data, interpretability and research design rather than being treated as fixed in advance.

The results will be interpreted as historical associations and possible leading relationships. An observed relationship between vacancies and labour-market slack will not automatically be interpreted as evidence of causation.

---

# Data Sources

## Australian Bureau of Statistics (ABS)

The Australian Bureau of Statistics is the main source for the labour-market slack analysis.

The current datasets include:

* Labour Force, Australia
* Labour underutilisation measures
* Youth labour-force measures
* Wage Price Index

These data provide measures including unemployment, underemployment, labour underutilisation, employment, labour-force participation and wage growth.

Before analysis, the relevant definitions, units, frequency, seasonal adjustment and date coverage are checked and documented.

Related measures are also checked for consistency. For example, labour underutilisation should be broadly consistent with the combination of unemployment and underemployment, allowing for small differences caused by rounding and numerical precision.

## ABS Job Vacancies, Australia

ABS Job Vacancies provides quarterly estimates of the stock of job vacancies at a reference date.

The seasonally adjusted Australian total is currently being investigated as the vacancy numerator for the labour-market tightness measure.

One limitation is its quarterly frequency, which means the resulting tightness measure cannot be analysed at the same monthly frequency as the IVI.

## Jobs and Skills Australia (JSA)

The Jobs and Skills Australia Internet Vacancy Index is the main source for the monthly labour-demand component.

The IVI provides monthly information on newly lodged online job advertisements. The seasonally adjusted Australian total is currently the main vacancy measure for the monthly labour-demand analysis.

More detailed IVI data are also used to examine vacancy patterns across occupations, skill levels, and states and territories.

## Reserve Bank of Australia (RBA)

Reserve Bank of Australia information may be used to provide additional economic and methodological context.

RBA research on labour-market tightness is particularly relevant when considering vacancy-to-unemployment and broader vacancy-to-searcher measures.

Other economic variables, such as the Cash Rate Target, may be considered later if they are relevant to the research question and can be incorporated consistently.

---

# Analysis Approach

The analysis will be developed in stages.

## 1. Understand and Validate the Data

Each dataset will first be examined independently.

This includes checking:

* Variable definitions
* Data frequency
* Seasonal adjustment
* Units and denominators
* Date coverage
* Missing observations
* Duplicate dates
* Unusual or implausible observations
* Consistency between related measures
* Potential structural breaks
* Changes in methodology or measurement
* Revisions between releases
* Data vintages

The analysis will distinguish between a newly released estimate and later revised estimates where historical vintages are available.

The purpose is to understand whether conclusions about labour-market slack could be affected by revisions to previously published ABS estimates.

The datasets will use consistent date variables and will be aligned at the frequency required for each analysis.

---

## 2. Describe Labour-Market Slack

The first stage of the labour-market slack analysis examines unemployment, underemployment and labour underutilisation over time.

Time-series plots and summary statistics will be used to understand:

* Long-term movements
* Short-term changes
* Differences between unemployment and underemployment
* Changes in overall labour underutilisation
* The relative contribution of unemployment and underemployment
* Major movements during periods of economic disruption
* Labour-market conditions during more typical periods

The analysis will not rely only on unemployment because unemployment does not capture people who are employed but would like to work additional hours.

---

## 3. Examine Normal Labour-Market Conditions

The analysis will not focus only on major economic shocks.

Periods such as the early 1990s recession and COVID-19 provide useful case studies, but they represent unusual conditions.

The analysis will therefore also examine the behaviour of unemployment, underemployment and underutilisation during periods without major economic shocks.

This allows the project to investigate:

* What typical levels of slack look like
* How much labour-market measures normally fluctuate
* Whether unemployment and underemployment move together during normal periods
* Whether underemployment provides additional information when unemployment is relatively low
* Whether wage growth behaves differently when labour-market slack is relatively high or low
* Whether the relationship between slack and wages changes during unusually strong or weak labour-market conditions

The purpose is to avoid defining labour-market slack only through recession periods.

---

## 4. Examine Economic Shocks and Major Periods

Major economic periods will be used as historical context when interpreting movements in the labour-market series.

Periods of interest include:

* The early 1990s downturn
* The 2008–09 Global Financial Crisis period
* The 2020 COVID-19 shock
* The post-COVID recovery
* The recent period of inflation and monetary tightening
* Other periods identified from the data as having unusually large movements in labour-market slack

The analysis will compare these periods with more normal periods rather than treating the shocks as the only relevant observations.

The purpose is to determine whether the behaviour of labour-market slack during major shocks differs from its behaviour during normal labour-market conditions.

---

## 5. Examine Youth Labour-Market Outcomes

Youth labour-market outcomes will be compared with overall labour-market conditions.

The analysis includes:

* Youth unemployment rate
* Overall unemployment rate
* Youth unemployment gap
* Youth underemployment
* Youth underutilisation

The youth unemployment gap is calculated as:

`Youth unemployment rate - Overall unemployment rate`

This is used as a descriptive measure of the difference between youth and overall unemployment.

Youth outcomes will initially be treated as supplementary measures. They may be included in the main analysis if they provide useful information about how different groups experience labour-market slack.

---

## 6. Examine Changes in the Data

Looking only at levels can sometimes produce misleading relationships because economic variables can contain persistent trends and other low-frequency movements.

For this reason, the analysis will compare levels with measures of change where appropriate.

Possible transformations for monthly series include:

### Monthly changes

`Change_t = Value_t - Value_(t-1)`

This measures the change from one month to the next.

### Year-on-year changes

`Change_YoY_t = Value_t - Value_(t-12)`

This compares a month with the same month in the previous year.

### Trend or detrended measures

Where appropriate, the analysis may also examine deviations from an estimated underlying trend.

The final transformations will be selected based on economic interpretability, treatment of seasonality, statistical behaviour and robustness rather than simply choosing the specification that produces the strongest relationship.

---

## 7. Examine the Relationship Between Unemployment and Underemployment

Unemployment and underemployment represent different forms of labour-market slack.

The analysis will therefore examine whether these measures tend to move together or behave differently across economic periods.

This includes:

* Overall correlation
* Correlation by economic period
* Relationship during major shocks
* Relationship during normal periods
* Changes in the relative importance of unemployment and underemployment

This helps determine whether unemployment alone provides an adequate representation of labour-market slack or whether underemployment adds substantial information.

---

## 8. Examine Labour-Market Slack and Wage Growth

The Wage Price Index will be used to examine whether labour-market slack is associated with wage pressure.

The analysis will compare:

* Unemployment and wage growth
* Underemployment and wage growth
* Underutilisation and wage growth
* Labour-market tightness and wage growth where appropriate

The analysis will first examine the relationship descriptively before considering more formal statistical models.

Possible approaches include comparing wage growth across periods of relatively high and low slack and examining whether changes in slack are associated with subsequent changes in wage growth.

The direction and timing of the relationship will be investigated rather than assuming that wage growth responds immediately to changes in slack.

---

## 9. Examine Labour-Market Tightness

An initial quarterly labour-market tightness measure will be constructed using:

`ABS Job Vacancies / ABS Unemployed Persons`

Both variables are seasonally adjusted stock measures and are expressed in thousands, allowing the ratio to be interpreted as vacancies per unemployed person.

Because ABS Job Vacancies is quarterly, monthly unemployed-person data will be aligned with the corresponding ABS vacancy reference months.

The resulting series will first be examined descriptively before deciding how it should be incorporated into the main analysis.

The relationship between labour-market tightness and labour-market slack will also be examined.

A tighter labour market would generally be expected to correspond with lower unemployment and underutilisation, although the relationship may not be one-to-one.

---

# Data Revisions and Vintages

An additional part of the analysis is to investigate how ABS labour-market estimates are revised after their initial release.

ABS labour-market data may be revised as new information becomes available, seasonal factors are updated, or methodological adjustments are made.

The project will therefore investigate:

* Whether historical observations change between releases
* The size of revisions
* Which labour-market measures experience larger revisions
* Whether revisions are concentrated in particular periods
* Whether revisions materially affect measures of labour-market slack
* Whether conclusions drawn from real-time data differ from conclusions based on revised data

Where historical vintages are available, the project will compare earlier published estimates with later revised estimates.

This is important because a relationship that appears strong using revised historical data may not have been equally visible using the information available at the time.

The revision analysis is therefore intended to assess the robustness and real-time usefulness of the labour-market measures.

---

# Economic Interpretation: What Is a "Good" Labour Market?

The project will avoid defining a single unemployment or underutilisation rate as automatically representing a "good" labour market.

Instead, labour-market conditions will be interpreted using multiple indicators.

A stronger labour-market outcome may involve:

* Low unemployment
* Low underemployment
* Low underutilisation
* High participation
* Strong employment growth
* Sustainable wage growth
* A reasonable balance between vacancies and available workers

However, extremely tight labour-market conditions may also create strong wage pressures or recruitment difficulties.

Therefore, the project will investigate the balance between:

**Labour demand**

and

**Available labour supply**

rather than assuming that the lowest possible unemployment or highest possible wage growth is always the optimal outcome.

The Wage Price Index and labour-market tightness measures may provide additional information when interpreting this balance.

---

# Vacancy and Labour-Market Slack Analysis

Once the labour-demand and labour-market slack measures have been validated, the project will investigate their relationship over their common sample period.

A possible starting specification is:

`Change in Slack_t = alpha + beta(Change in Vacancies_(t-k)) + error_t`

where `k` represents a possible lag between vacancy movements and labour-market slack.

The analysis may examine whether changes in vacancies are associated with contemporaneous or subsequent changes in:

* Unemployment
* Underemployment
* Labour underutilisation

The analysis will also consider whether labour-market tightness provides a more informative measure than vacancies alone.

The timing of relationships will be investigated using appropriate lag structures.

For each main specification, relevant diagnostics may include:

* Direction of the relationship
* Size of the estimated relationship
* Confidence intervals
* Sample size
* Selected transformation
* Selected lag
* Residual behaviour
* Possible autocorrelation
* Robustness across different periods

The exact modelling approach will be selected after the measurement and data-frequency decisions have been established.

The results will be interpreted as evidence about association and timing rather than direct evidence of causation.

---

# Forecasting

Forecasting will only be considered after the underlying vacancy–slack relationship has been investigated and the measurement choices have been established.

The purpose of forecasting would be to determine whether vacancy information provides useful additional information for predicting labour-market slack.

## Baseline Model

A baseline model would use the historical behaviour of the selected labour-market slack measure without vacancy information.

A simple benchmark, such as a naive or autoregressive model, may be used as the reference point.

## Extended Model

An extended model would include vacancy information using an appropriate transformation and lag structure.

The key question would be whether including vacancy information improves out-of-sample forecasts compared with the baseline model.

---

# What Counts as an Improvement?

A more complicated model will not automatically be considered better.

If forecasting is undertaken, models will be evaluated on unseen data using the same forecasting period and evaluation procedure.

Possible evaluation measures include:

* Mean Absolute Error (MAE)
* Root Mean Squared Error (RMSE)

A time-ordered train/test split or rolling-origin evaluation would be used rather than randomly shuffling observations.

A possible comparison is:

| Model                           | Vacancy information | MAE          | RMSE         |
| ------------------------------- | ------------------- | ------------ | ------------ |
| Baseline model                  | No                  | To calculate | To calculate |
| Baseline + vacancies            | Yes                 | To calculate | To calculate |
| Optional machine-learning model | Yes                 | To calculate | To calculate |

Lower MAE or RMSE over the same out-of-sample period would indicate better forecasting performance.

If vacancy information does not improve forecasting performance, this would also be treated as an informative result.

---

# Machine Learning

Machine-learning methods will only be considered if they provide a clear purpose within the research question and if the available sample is sufficient.

Possible methods may include:

* Random Forest
* XGBoost
* Other suitable machine-learning methods

If included, machine-learning models would be compared with simpler statistical models using the same outcome, forecast horizon, test period, evaluation metrics and information set.

Given the relatively small number of Australian macroeconomic observations, model complexity will be treated cautiously.

If a simpler model performs as well as or better than a machine-learning model, the simpler model will be preferred.

---

# Current Progress

## Rimlan — Labour-Market Slack

The ABS labour-market dataset has been imported, combined and validated using the latest available Labour Force data.

### Current Data Coverage

**February 1978 – July 2026**

### Current Dataset

The current dataset contains approximately 582 monthly observations covering the period from February 1978 to July 2026.

Initial data checks include:

* Duplicate date checks
* Missing-value checks
* Monthly sequence checks
* Rate-range checks
* Consistency checks between unemployment, underemployment and underutilisation

### Current Variables

The dataset includes:

* Employment
* Unemployment rate
* Participation rate
* Youth unemployment rate
* Underemployment rate
* Youth underemployment rate
* Labour underutilisation rate
* Youth underutilisation rate
* Youth unemployment gap
* Youth underemployment gap
* Youth underutilisation gap
* Calculated underutilisation measures
* ABS release/vintage information

Initial checks confirm that the published labour underutilisation measure is broadly consistent with unemployment plus underemployment, with small differences attributable to rounding and numerical precision.

---

## Current Labour-Market Slack Analysis

Initial analysis has examined:

* Long-term unemployment trends
* Underemployment trends
* Labour underutilisation
* Youth unemployment
* Youth underutilisation
* Youth outcomes relative to overall labour-market conditions
* Period averages
* Changes between economic periods
* The relationship between unemployment and underemployment
* Correlation between unemployment and underemployment
* Correlations across different economic periods
* Month-to-month changes in labour-market slack
* Large monthly movements in underutilisation
* Major economic shocks
* Normal labour-market periods

The early 1990s recession and COVID-19 period were initially used as important case studies.

The analysis is now being expanded beyond major shocks to examine whether the relationships observed during these periods also occur during more typical labour-market conditions.

This is important because major shocks may produce unusual movements that do not represent the normal relationship between labour-market slack, labour demand and wages.

---

## Wage Price Index Analysis

The Wage Price Index is being incorporated as an additional outcome for understanding labour-market conditions.

The current analysis will investigate whether periods of higher or lower labour-market slack are associated with different patterns of wage growth.

The analysis will compare wage growth with:

* Unemployment
* Underemployment
* Underutilisation
* Labour-market tightness where appropriate

The initial focus is descriptive, with formal modelling to be considered after the relationships and appropriate timing have been established.

---

## Data Revision Analysis

The project is also investigating ABS data revisions and vintages.

The objective is to determine:

* How much previously published labour-market estimates change
* Whether revisions differ across variables
* Whether revisions are economically meaningful
* Whether revisions could change conclusions about labour-market slack

This is particularly relevant when considering whether labour-market indicators could have been useful for decision-making in real time.

---

## Trisha — Labour Demand

The labour-demand component is being developed using Australian vacancy data from Jobs and Skills Australia and the Australian Bureau of Statistics.

### JSA Internet Vacancy Index

The IVI datasets have been cleaned and prepared for analysis, covering:

* Broad occupations
* Skill levels
* Detailed occupations
* States and territories

Initial EDA shows substantial changes in Australian vacancy activity between 2006 and 2026. Vacancies declined sharply during the Global Financial Crisis and COVID-19 periods, followed by a strong post-COVID recovery that peaked around 2022 before declining towards 2026.

Vacancy patterns also differ across occupations, skill levels, and states and territories.

### ABS Job Vacancies and Labour-Market Tightness

Following further investigation into labour-market tightness, ABS Job Vacancies has been added as a potential vacancy measure for constructing a vacancy-to-unemployment ratio.

The seasonally adjusted Australian total ABS Job Vacancies series has been extracted and cleaned for the period from February 2006 to May 2026.

Unlike the IVI, ABS Job Vacancies is a stock measure. This allows a more consistent comparison with the stock of unemployed persons.

Seasonally adjusted unemployed persons from ABS Labour Force data will be used as the denominator.

The proposed quarterly measure is:

`Labour-market tightness = ABS Job Vacancies / ABS Unemployed Persons`

This measure is currently being constructed and assessed and remains provisional pending further analysis and supervisor feedback.

---

# Next Steps

## Rimlan — Labour-Market Slack

The next stage of the labour-market slack analysis is to move from descriptive analysis of major shocks towards a broader assessment of normal and unusual labour-market conditions.

The immediate priorities are:

1. **Complete the definition and measurement framework for labour-market slack.**

   * Clearly distinguish the unobservable concept of slack from its observable proxies.
   * Document the ABS definitions of unemployment, underemployment and underutilisation.
   * Identify the strengths and limitations of each measure.

2. **Expand the historical analysis beyond major shocks.**

   * Compare normal periods with the early 1990s recession, GFC, COVID-19 and post-COVID periods.
   * Identify whether patterns observed during shocks also occur during normal conditions.

3. **Investigate the relationship between unemployment and underemployment.**

   * Examine correlations across different periods.
   * Determine whether underemployment provides additional information beyond unemployment.

4. **Continue analysing changes rather than only levels.**

   * Examine monthly changes.
   * Examine year-on-year changes where appropriate.
   * Consider trend or detrended measures if useful.

5. **Incorporate the Wage Price Index.**

   * Compare wage growth with unemployment, underemployment and underutilisation.
   * Examine whether wage pressure changes when labour-market slack is high or low.
   * Investigate possible lagged relationships.

6. **Investigate what constitutes relatively healthy labour-market conditions.**

   * Avoid defining "good" labour-market conditions using a single measure.
   * Consider unemployment, underemployment, participation, wage growth and labour-market tightness together.

7. **Complete the ABS revision and vintage analysis.**

   * Compare initial releases with subsequent revised observations where possible.
   * Quantify the size of revisions.
   * Assess whether revisions could materially affect the interpretation of labour-market slack.

8. **Validate the labour-market slack measures before combining them with vacancy data.**

## Combined Analysis

Once the labour-demand and labour-market slack measures have been validated:

* Construct and validate the quarterly labour-market tightness measure.
* Align the vacancy and slack datasets over their common sample period.
* Investigate the relationship between vacancies and unemployment.
* Investigate the relationship between vacancies and underemployment.
* Investigate the relationship between vacancies and underutilisation.
* Investigate whether labour-market tightness provides more information than vacancy levels alone.
* Examine possible lagged relationships.
* Examine whether relationships differ between normal periods and major economic shocks.
* Investigate whether labour-market tightness and slack are associated with wage growth.

## Final Modelling

After the measurement framework has been established:

* Finalise the preferred measure or combination of measures for labour-market slack.
* Select appropriate transformations.
* Select economically interpretable lag structures.
* Estimate appropriate statistical models.
* Check model diagnostics and robustness.
* Consider forecasting only after the historical relationships have been established.
* Compare baseline and vacancy-augmented forecasting models if forecasting is justified.

The final analysis will prioritise economic interpretation and robustness over model complexity.

---

# Overall Project Objective

The project aims to determine whether observable labour-market indicators can provide a useful measure of labour-market slack and whether changes in labour demand, particularly job vacancies, provide information about subsequent labour-market conditions.

The analysis will therefore connect three related concepts:

**Labour demand**

→ Job vacancies and labour-market tightness

**Labour-market slack**

→ Unemployment, underemployment and underutilisation

**Labour-market pressure**

→ Wage growth and the Wage Price Index

The central objective is not simply to identify whether two variables are correlated, but to understand **what each measure represents, how reliably it measures labour-market conditions, how the relationship behaves during both normal and unusual periods, and whether vacancy information provides useful additional information about future labour-market slack.**
