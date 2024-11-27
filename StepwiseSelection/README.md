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

However, we also observed that the lag information nina3_y_anom is totally uncorrelated with the output of the hurricane occurence, but that the lag information
from **z500_std_t_1**  is somehow significant. Also some intermediate experiments showed that the lag hurricane information was adding overall noise. 
We end up with  the following set of predictors: **PPt** , **NINA1_std_t_1** and  **z500_std_t_1**. 

Some of our predictors choices are heuristic, however  since we tested 10 models (5 montly and 5 yearly) with 22 potential predictors, the potential models predictors combinations would a Total of 10 x 2^22. Potentially, this could be resolved by using penalized regression approches such as lasso, solution that we highly suggest for future work on the topic. However, our results are coherent with the EDA analysis where we found that the NINA variables are negatively correlated with the number of hurrricane occurences.






