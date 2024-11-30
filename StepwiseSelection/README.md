## Stepwise Selection on Decomposed Hurricane Time-Series
The repository contains an R file for stepwise variable selection.

To evaluate the predictive power of our variables beyond the structural components of the hurricane time-series, we performed a stepwise selection on the decomposed data. The time-series was decomposed into trend, seasonal, and residual components. Our primary goal was to identify predictors that are informative for forecasting the **non-structural component**—the residuals—which represent the unexplained variability after accounting for trends and seasonality.

This analysis is crucial because trend and seasonal components are generally easier to predict due to their regularity. By focusing on the residuals, we assess whether our predictors contain additional information that can capture the irregular or unexpected fluctuations in hurricane activity.

Stepwise selection was performed using the **Akaike Information Criterion (AIC)** as the selection metric. This approach ensures that the final model balances complexity and goodness of fit, favoring predictors that contribute significantly to the prediction of residuals.

The stepwise selection resulted in the following predictors:
- **PPt**: It is the pseudo-predictor

This is a remarkable finding, as it suggests that our designed PPt encapsulates information beyond the seasonal and trend components.

However, we also observed that the lag information other predictors ameliorate the models performance in predictive test such as **NINA1_std_t_1** and **z500_std_t_1**
when added to the models. As shown in the below table these appears to be also some of those predictors that have the highest lag linear correlation.

Some of our predictors choices are heuristic, however  since we tested 10 models (5 montly and 5 yearly) with 22 potential predictors, the potential models predictors combinations would a Total of 10 x 2^22. Potentially, this could be resolved by using penalized regression approches such as lasso, solution that we highly suggest for future work on the topic. However, our results are coherent with the EDA analysis where we found that the NINA variables are negatively correlated with the number of hurrricane occurences. The follwing table gives an overview of the linear correlation of the lagged predictors with the hurricane counts, showing in general low correlations, and justifying why our basedline of predictors is very small.

| Variable                                | Correlation     |
|-----------------------------------------|-----------------|
| cor_lag_time_series1.nina1              | -0.391647458    |
| cor_lag_time_series1.nina3.anom         | -0.391647458    |
| cor_lag_time_series2.z500_avg           | 0.216964985     |
| cor_lag_time_series2.z500_std           | 0.216916984     |
| cor_lag_time_series2.zwdn_500_an        | 0.216831328     |
| cor_lag_time_series1.soi                | -0.152589935    |
| cor_lag_time_series1.pna                | -0.113698679    |
| cor_lag_time_series2.Oni                | -0.106631683    |
| cor_lag_time_series1.tni.had            | -0.102894650    |
| cor_lag_EI                              | 0.097577307     |
| cor_lag_ESPI                            | 0.077302017     |
| cor_lag_time_series1.enso.ts.1mn        | 0.054502423     |
| cor_lag_LI                              | -0.052043547    |
| cor_lag_time_series2.zzwnd200_ori       | -0.048173743    |
| cor_lag_time_series2.zwdn_200_an        | -0.048173743    |
| cor_lag_time_series2.zwnd200_st         | -0.045602989    |
| cor_lag_X130E.80W                       | -0.016291627    |
| cor_lag_time_series2.OLR                | 0.014935114     |
| cor_lag_X160E.80W                       | -0.009993400    |
| cor_lag_X180W.100W                      | -0.008387649    |

Note this table does not include time and PPt.







