# Marketing Mix Modeling
A collection of research into marketing mix modeling methods


## 📊 Marketing Mix Modeling (MMM)

This project explores how different marketing channels influence sales using statistical and econometric techniques. MMM is widely used for both explanatory and predictive modeling, helping businesses understand ROI across channels.

## 🎯 Goals

- Explore linear regression–based MMM with attention to challenges like multicollinearity and feature selection.
- Engineer features to capture: Diminishing returns (Hill / exponential functions), Carryover effects (Adstock transformation), Seasonality & trends (Holt-Winters decomposition)

- Generate synthetic time series data with ARIMA + sinusoidal seasonality for testing.

- Evaluate models based on Mean Squared Prediction Error (MSPE).

## 🛠 Methods

- Four models were tested and compared:

1.) Baseline → media spend only

2.) Extended → spend + adstock + exponential (diminishing returns)

3.) Advanced → spend + adstock + Hill function

4.) Extended + Lagged Sales → spend + adstock + Hill + Holt-Winters lagged features

Statistical tests (ANOVA, adjusted R², F-tests) and MSPE on test data assessed model fit and predictive accuracy.

## 📈 Key Findings

- Adding carryover (adstock) and diminishing return curves greatly improved predictive performance.

- Holt-Winters lagged sales terms added only marginal improvements beyond Model 3.

- Even simple regression-based MMMs benefit significantly from non-linear transformations and time-series feature engineering.

## 🚀 Repo Contents

- data/ → synthetic time series data generator (ARIMA + noise)

- notebooks/ → step-by-step exploration of Models 1–4

- src/ → reusable functions for adstock, Hill/exponential transformations, Holt-Winters decomposition

  - results/ → model outputs, ANOVA comparisons, performance plots

## 🔍 References

Jin, Wang, Sun, Chan, & Koehler (2017) — Bayesian MMM & Hill functions

Kübler (2021) — Exponential response functions in MMM
