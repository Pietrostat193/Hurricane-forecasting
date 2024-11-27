## Stepwise Selection on Decomposed Hurricane Time-Series
The repository contains an R file for stepwise variable selection.

To evaluate the predictive power of our variables beyond the structural components of the hurricane time-series, we performed a stepwise selection on the decomposed data. The time-series was decomposed into trend, seasonal, and residual components. Our primary goal was to identify predictors that are informative for forecasting the **non-structural component**—the residuals—which represent the unexplained variability after accounting for trends and seasonality.

This analysis is crucial because trend and seasonal components are generally easier to predict due to their regularity. By focusing on the residuals, we assess whether our predictors contain additional information that can capture the irregular or unexpected fluctuations in hurricane activity.

Stepwise selection was performed using the **Akaike Information Criterion (AIC)** as the selection metric. This approach ensures that the final model balances complexity and goodness of fit, favoring predictors that contribute significantly to the prediction of residuals.

The stepwise selection resulted in the following predictors:
- **PPt**: It is the pseudo-predictor
- **NINA1_std_t_1**: See table in the paper.
- **Obs_t_1**: It is the lag information.
- **nina3_y_anom**: See table in the paper.




