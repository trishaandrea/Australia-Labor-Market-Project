# Australian Labour Market Project

This project examines the relationship between **job vacancies and labour-market slack in Australia**, with a focus on whether short-run changes in labour demand are associated with changes in unused labour capacity.

## Research Question

> **To what extent are changes in job vacancies associated with short-run changes in labour-market slack in Australia?**

The analysis focuses on **association rather than causation** and compares three measures of labour-market slack:

- **Unemployment** — people without work who are available and actively looking for work.
- **Underemployment** — employed people who want and are available to work more hours.
- **Underutilisation** — a broader measure combining unemployment and underemployment.

Labour-market tightness is also examined as a complementary indicator of labour-market conditions.

## Data

The project combines data from the **Australian Bureau of Statistics (ABS)** and **Jobs and Skills Australia (JSA)**.

### ABS Job Vacancies

ABS Job Vacancies are used as the primary measure of labour demand for the main research question. The data measure the stock of vacant positions at quarterly reference dates.

### ABS Labour Force

Labour Force data provide the measures of labour-market slack used in the analysis:

- unemployment
- underemployment
- underutilisation

Underutilisation is treated as the broadest observable measure of labour-market slack.

### Jobs and Skills Australia Internet Vacancy Index

The **Internet Vacancy Index (IVI)** provides a complementary monthly measure of online recruitment activity. It is used to explore labour-demand trends and differences across occupations and skill levels.

### Wage Price Index

ABS Wage Price Index data are used for supplementary analysis of the relationship between labour-market conditions and wage growth.

## Methodology

The main analysis focuses on **quarter-to-quarter changes** in job vacancies and labour-market slack rather than only comparing their levels.

The project uses:

- exploratory data analysis and time-series visualisation
- quarter-to-quarter changes in vacancies and slack measures
- **Pearson correlation** as the primary correlation measure
- **Spearman correlation** as a sensitivity check
- **LOESS smoothing** to visualise relationships where linearity is not appropriate
- labour-market tightness as a complementary indicator
- supplementary wage and short-horizon forecasting analysis

Labour-market tightness is calculated as:

**Tightness = Job Vacancies / Unemployed Persons**

Because unemployment appears in the denominator, relationships between tightness and unemployment are interpreted cautiously.

## Key Findings

Changes in job vacancies are negatively associated with short-run changes in all three measures of labour-market slack.

The Pearson correlations between vacancy changes and changes in slack are approximately:

| Slack measure | Pearson correlation | Spearman correlation |
|---|---:|---:|
| Unemployment | -0.54 | -0.39 |
| Underemployment | -0.81 | -0.51 |
| Underutilisation | -0.80 | -0.56 |

The strongest relationships are observed for **underemployment and underutilisation**, suggesting that broader measures of unused labour capacity move more closely with changes in labour demand than unemployment alone.

These results are descriptive and should **not be interpreted as causal effects**.

## Additional Analysis

The project also examines:

- historical trends in labour demand and labour-market slack
- labour-market tightness and slack
- changes in tightness and slack
- online recruitment across occupations and skill levels
- wage growth and labour-market conditions
- short-horizon forecasts of labour-market slack

## Interactive Dashboard

### Live Dashboard

[Open the interactive Shiny dashboard](https://2g1bha-rimlan-alva.shinyapps.io/labour-market-dashboard/)

A **Shiny dashboard** accompanies the analysis and provides interactive exploration of:

- labour demand
- labour-market slack
- labour-market tightness
- the main research question
- wage pressure
- occupation and skill-level demand
- short-horizon forecasts
- underlying data and methodology

## Repository Structure

```text
Australia-Labor-Market-Project/
├── R/
│   ├── rimlan/             # Labour-market slack, wages, tightness, and forecasting analysis
│   └── trisha/             # Data cleaning, labour demand, and combined labour-market analysis
├── data/
│   ├── raw/                # Original source data
│   └── processed/          # Cleaned and analysis-ready datasets
├── presentation/
│   ├── Presentation.qmd    # Presentation source
│   ├── Presentation.html   # Rendered presentation
│   └── _extensions/        # Quarto presentation extensions
├── report/
│   ├── Report.qmd          # Final report source
│   ├── Report.html         # Rendered final report
│   └── references.bib      # Bibliography
├── shiny/
│   └── app.R               # Interactive Shiny dashboard
├── .gitignore
├── Australia-Labor-Market-Project.Rproj
├── README.md
└── README.html
```

## Authors

**Trisha Andrea & Rimlan Alva**

Monash University