# Statistical Analysis of the SENIC Hospital Dataset

A Fall 2020 AMS 572 group project using data from the Study on the Efficacy of Nosocomial Infection Control (SENIC) to illustrate exploratory analysis, one-sample inference, logistic regression, and missing-data considerations.

## Questions studied

1. After screening unusually large observations, did the sample's mean hospital length of stay differ from nine days?
2. Could medical-school affiliation be modeled using infection risk, U.S. region, and average daily census?
3. How might different missing-data mechanisms affect the interpretation of the analysis?

## Analysis

The project analyzed 113 hospitals. For the length-of-stay analysis, boxplot-identified outliers were removed before checking normality and applying a two-sided one-sample *t* test. The resulting sample mean was approximately 9.44 days, with *t* = 3.28 and *p* = 0.00138 against a nine-day reference value.

For the second question, an 80% random subsample of the cleaned data was used in a logistic regression. Medical-school affiliation was modeled as a function of infection risk, region, and average daily census. In that fitted model, average daily census was the strongest individual predictor; the joint Wald test for the included predictors had *p* = 0.015.

These are historical coursework results from an observational dataset. They should not be interpreted as causal findings or current clinical guidance.

## Repository contents

- `analysis.R` — R analysis script
- `hospital.xlsx` — copy of the source data
- `main.pdf` — final group report

## Contributors

Kai Li, Yunhan Qi, and Tiange Zhang.
