#setwd("H:/Il mio Drive/Hurricane forecasting")
setwd("C:/Users/2692812C/OneDrive - University of Glasgow/Desktop/Hurricane-forecasting-main/Hurricane-forecasting-main")
library(forecast)
library(tscount)
library(partsm)
library(dplyr)
library(car)
library(tscount)#for ingarch
library(quantreg)#for quant reg
library(ProbCast)
library(readr)
library(tidyverse)
library(rio)
data <- rio::import("Monthly-data.csv")
data <- data[1:528,]
train.l <- 396
out.l <- nrow(data)-train.l



forecast_yearly <- function(model, xreg, h = 12, conf_level = 0.95) {
  # Ensure h is a multiple of 12 for yearly aggregation
  if (h %% 12 != 0) stop("Forecast horizon (h) must be a multiple of 12 for yearly aggregation")
  
  # Forecast monthly values
  monthly_forecast <- forecast(model, xreg = xreg, h = h)
  
  # Initialize results for yearly forecasts and intervals
  yearly_forecasts <- c()
  yearly_lower <- c()
  yearly_upper <- c()
  
  # Loop over years (12 months per year)
  for (i in seq(1, h, by = 12)) {
    # Extract 12-month forecasts
    yearly_mean <- sum(monthly_forecast$mean[i:(i + 11)])
    
    # Aggregate variances (upper bound approximation for independence)
    monthly_variances <- (monthly_forecast$upper[i:(i + 11), 2] - 
                            monthly_forecast$mean[i:(i + 11)])^2 / qnorm((1 + conf_level) / 2)^2
    yearly_variance <- sum(monthly_variances)
    yearly_se <- sqrt(yearly_variance)
    
    # Compute confidence intervals
    z <- qnorm((1 + conf_level) / 2)
    yearly_ci_lower <- yearly_mean - z * yearly_se
    yearly_ci_upper <- yearly_mean + z * yearly_se
    
    # Store results
    yearly_forecasts <- c(yearly_forecasts, yearly_mean)
    yearly_lower <- c(yearly_lower, yearly_ci_lower)
    yearly_upper <- c(yearly_upper, yearly_ci_upper)
  }
  
  # Return results as a data frame
  result <- data.frame(
    Year = seq(1, h / 12),
    Forecast = yearly_forecasts,
    Lower_CI = yearly_lower,
    Upper_CI = yearly_upper
  )
  
  return(result)
}
simulate_tsglm <- function(model, nsim = 12, npaths = 1000) {
  # Compute the average lambda from the training data
  avg_lambda <- mean(tail(fitted(model)[12]))
  avg_lambda
  # Simulate multiple paths
  simulations <- matrix(NA, nrow = nsim, ncol = npaths)
  
  for (path in 1:npaths) {
    # Simulate directly from the Poisson distribution with avg_lambda
    simulations[1:12, path] <- rpois(nsim, avg_lambda)
  }
  
  return(simulations)
}




xreg <- data[, c( "time_series1.nina1","time_series2.z500_std")]
xregL <- apply(xreg, 2, function(x){Hmisc::Lag(x,12)})
yt <- ts(data[,4], start=data[1,2], frequency=12)
D <- seasonaldummy(yt)
datasetfinale <- data.frame(data[,c(2,3)],"yt"=yt,xregL,D)
#rio::export(datasetfinale, "datasetfinale.csv")

# Again here

rm(list=ls())

data <- rio::import("Monthly-data.csv")
data <- data[1:528,]
train.l <- 396
out.l <- nrow(data)-train.l



# Initialize objects for storing results
Forecasts <- matrix(NA, nrow = out.l, ncol = 15)  # Aggregated yearly forecasts for all models
IC <- list()  # List to store confidence intervals for each model

# Pre-allocate matrices for each model in IC
for (model_idx in 1:15) {
  IC[[paste0("IC_M", model_idx)]] <- matrix(NA, nrow = out.l / 12, ncol = 4)  # Preallocate for 12-month rolling forecasts
}
iter_idx <- 0
# Loop through rolling forecasts
for (i in seq(1, out.l, by = 12)) {

  iter_idx <- iter_idx + 1
  # Prepare data for the rolling window
  yt <- ts(data[i:(train.l + i - 1), 4], start = data[i, 2], frequency = 12)
  xreg <- data[i:(train.l + i - 1), c("time_series1.nina1", "time_series2.z500_std")]
  xreg <- apply(xreg, 2, function(x) { Hmisc::Lag(x, 12) })
  
  # Ensure xreg is numeric
  if (!is.numeric(xreg)) {
    xreg <- as.matrix(xreg)
  }
  
  # Ensure yt is a ts object
  if (!is.ts(yt)) {
    stop("yt must be a time series object!")
  }
  
  # Train Models
  # ARIMA Models
  model1 <- auto.arima(yt, xreg = seasonaldummy(yt))
  model1b <- auto.arima(yt, xreg = cbind(seasonaldummy(yt), xreg))
  model1c <- auto.arima(yt, xreg = xreg)
  
  # INGARCH Models
  model2 <- tsglm(ts = data[i:(train.l + i - 1), 4], model = list(past_obs = 1, past_mean = 1), 
                  xreg = seasonaldummy(yt), link = "log", distr = "poisson")
  model2b <- tsglm(ts = data[(i + 12):(train.l + i - 1), 4], model = list(past_obs = 1, past_mean = 1), 
                   xreg = cbind(seasonaldummy(yt)[-c(1:12), ], xreg[-c(1:12), ]), link = "log", distr = "poisson")
  model2c <- tsglm(ts = data[(i + 12):(train.l + i - 1), 4], model = list(past_obs = 1, past_mean = 1), 
                   xreg = cbind(xreg[-c(1:12), ]), link = "log", distr = "poisson")
  
  # Neural Networks
  model3 <- nnetar(yt, xreg = seasonaldummy(yt))
  model3b <- nnetar(yt, xreg = cbind(seasonaldummy(yt), xreg))
  model3c <- nnetar(yt, xreg = xreg)
  
  # Quantile Regression
  tau <- seq(0.1, 0.9, 0.1)
  dff <- data.frame(yt = as.numeric(yt), seasonaldummy(yt))
  model4 <- rq(yt ~ ., data = dff, tau = tau)
  
  dff <- data.frame(yt = as.numeric(yt), seasonaldummy(yt), xreg = as.numeric(xreg))
  model4b <- rq(yt ~ ., data = dff, tau = tau)
  
  dff <- data.frame(yt = as.numeric(yt), xreg = as.numeric(xreg))
  model4c <- rq(yt ~ ., data = dff, tau = tau)
  
  # QGBRT Models
  sd <- seasonaldummy(yt)
  sd <- sd[-c(1:12), ]
  Data_t_1 <- as.data.frame(cbind("yt" = as.numeric(yt[1:384]), "dummy" = sd, xreg[-c(1:12), ]))
  
  model5 <- qreg_gbm(
    data = Data_t_1,
    formula = yt ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov,
    interaction.depth = 2,
    n.trees = 1000,
    n.minobsinnode = 3,
    shrinkage = 0.05,
    bag.fraction = 0.5,
    quantiles = tau,
    cores = detectCores()
  )
  
  model5b <- qreg_gbm(
    data = Data_t_1,
    formula = yt ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov +
      time_series1.nina1 + time_series2.z500_std,
    interaction.depth = 2,
    n.trees = 1000,
    n.minobsinnode = 3,
    shrinkage = 0.05,
    bag.fraction = 0.5,
    quantiles = tau,
    cores = detectCores()
  )
  
  model5c <- qreg_gbm(
    data = Data_t_1,
    formula = yt ~ time_series1.nina1 + time_series2.z500_std,
    interaction.depth = 2,
    n.trees = 1000,
    n.minobsinnode = 3,
    shrinkage = 0.05,
    bag.fraction = 0.5,
    quantiles = tau,
    cores = detectCores()
  )
  
  # Process all models
  for (model_idx in 1:15) {

    current_model <- switch(
      model_idx,
      "1" = model1, "2" = model1b, "3" = model1c,
      "4" = model2, "5" = model2b, "6" = model2c,
      "7" = model3, "8" = model3b, "9" = model3c,
      "10" = model4, "11" = model4b, "12" = model4c,
      "13" = model5, "14" = model5b, "15" = model5c
    )
    
    current_xreg <- switch(
      model_idx,
      "1" = seasonaldummy(yt, h = 12),
      "2" = cbind(seasonaldummy(yt, h = 12), tail(xreg, 12)),
      "3" = tail(xreg, 12),
      "4" = seasonaldummy(yt, h = 12),
      "5" = cbind(seasonaldummy(yt, h = 12)[-c(1:12), ], tail(xreg, 12)[-c(1:12), ]),
      "6" = tail(xreg, 12)[-c(1:12), ],
      "7" = seasonaldummy(yt, h = 12),
      "8" = cbind(seasonaldummy(yt, h = 12), tail(xreg, 12)),
      "9" = tail(xreg, 12),
      "10" = seasonaldummy(yt, h = 12),
      "11" = cbind(seasonaldummy(yt, h = 12), tail(xreg, 12)),
      "12" = tail(xreg, 12),
      "13" = as.data.frame(cbind(seasonaldummy(yt, h = 12), tail(xreg, 12))),
      "14" = as.data.frame(cbind(seasonaldummy(yt, h = 12), tail(xreg, 12))),
      "15" = as.data.frame(tail(xreg, 12))
    )
    
    if (model_idx <= 3) {
      yearly_forecast <- forecast_yearly(current_model, xreg = current_xreg, h = 12)
    } else if (model_idx >= 4 && model_idx <= 6) {
      # INGARCH Models
      n_simulations <- 1000
      simulated_paths <- simulate_tsglm(
        model = current_model,
        nsim = 12,
        npaths = n_simulations
      )
      yearly_forecasts <- colSums(simulated_paths)
      yearly_forecast <- list(
        Forecast = mean(yearly_forecasts),
        Lower_CI = quantile(yearly_forecasts, probs = 0.025),
        Upper_CI = quantile(yearly_forecasts, probs = 0.975),
        Year = 1
      )
    } else if (model_idx >= 7 && model_idx <= 9) {
      # NNAR Models
      n_simulations <- 1000
      simulated_paths <- matrix(NA, nrow = 12, ncol = n_simulations)
      for (path in 1:n_simulations) {
        simulated_paths[, path] <- simulate(current_model, nsim = 12, xreg = current_xreg)
      }
      yearly_forecasts <- colSums(simulated_paths)
      yearly_forecast <- list(
        Forecast = mean(yearly_forecasts),
        Lower_CI = quantile(yearly_forecasts, probs = 0.025),
        Upper_CI = quantile(yearly_forecasts, probs = 0.975),
        Year = 1
      )
    } else if (model_idx >= 10 && model_idx <= 12) {
      
      
      # Quantile Regression Models
      n_simulations <- 1000
      quantile_predictions <- tail(quantreg::predict.rq(current_model,xreg = as.data.frame(current_xreg)),12)

      simulated_paths <- matrix(NA, nrow = 12, ncol = n_simulations)
      simulated_paths <- t(apply(quantile_predictions, 1, function(row) {
        sample(row, n_simulations, replace = TRUE)
      }))
      yearly_forecasts <- colSums(simulated_paths)
      yearly_forecast <- list(
        Forecast = mean(yearly_forecasts),
        Lower_CI = quantile(yearly_forecasts, probs = 0.025),
        Upper_CI = quantile(yearly_forecasts, probs = 0.975),
        Year = 1
      )
    } else if (model_idx >= 13 && model_idx <= 15) {
      # QGBRT Models
      quantile_predictions <- predict(current_model, newdata = as.data.frame(current_xreg), pred_ntree = 300, sort = TRUE)
      n_simulations <- 1000
      simulated_paths <- t(apply(quantile_predictions, 1, function(row) {
        sample(row, n_simulations, replace = TRUE)
      }))
      yearly_forecasts <- colSums(simulated_paths)
      yearly_forecast <- list(
        Forecast = mean(yearly_forecasts),
        Lower_CI = quantile(yearly_forecasts, probs = 0.025),
        Upper_CI = quantile(yearly_forecasts, probs = 0.975),
        Year = 1
      )
    }
    
    # Store results in Forecasts and IC
    Forecasts[i, model_idx] <- sum(yearly_forecast$Forecast)
   
 
    IC[[paste0("IC_M", model_idx)]][iter_idx, ] <- c(
      yearly_forecast$Lower_CI,
      yearly_forecast$Forecast,
      yearly_forecast$Upper_CI,
      yearly_forecast$Year
    )
    
  }
 
   print(paste0("Iteration: ", round((i / out.l) * 100, 4), "% completed"))

}

 
# Placeholder for metrics storage
metrics <- list()

# Initialize an empty data frame to store metrics for all models
metrics_df <- data.frame(
  Model = character(),
  MAE = numeric(),
  RMSE = numeric(),
  DR = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each model
for (model_idx in 1:15) {
  # Extract forecasts and confidence intervals for the current model
  model_results <- IC[[paste0("IC_M", model_idx)]]
  
  # Extract forecasts
  forecasts <- model_results[, 2]  # Second column is the forecast
  #lower <- model_results[, 1]
  #upper <- model_results[, 2]
  # Extract actual values corresponding to rolling windows
  actuals <- sapply(1:nrow(model_results), function(idx) {
    start_idx <- train.l + (idx - 1) * 12  # Adjust start index for each rolling window
    sum(data[start_idx:(start_idx + 11), 4])  # Sum of actuals for the year
  })
  
  # Compute metrics
  mae <- mean(abs(forecasts - actuals))
  rmse <- sqrt(mean((forecasts - actuals)^2))
  dr <- sum(sign(diff(forecasts)) == sign(diff(actuals))) / (length(actuals) - 1)
  
  # Append metrics to the data frame
  metrics_df <- rbind(metrics_df, data.frame(
    Model = paste0("Metrics_M", model_idx),
    MAE = mae,
    RMSE = rmse,
    DR = dr
  ))
}

# Print the final metrics data frame
print(metrics_df)
write.csv(metrics_df,"R_Models_metrics_df.csv")
# Load the list back into your R session
save(IC, file = "R_ModelsPredictions.RData")
