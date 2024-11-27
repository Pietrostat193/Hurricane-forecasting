# Load libraries
library(dplyr)
library(car)
library(tscount)  # for Ingarch
library(quantreg) # for Quantile Regression
library(ProbCast)
library(readr)
library(tidyverse)

############################
# Empty object preparation
############################

D <- read_csv("D_python.csv")

# Prediction Objects
f_arima <- p_QR <- F_ingarch <- f_QGBRT_PP <- vector()
f_arima_NOPP <- p_QR_NOPP <- F_ingarch_NOPP <- f_QGBRT_NOPP <- vector()

# Quantile Objects
Ingarch_quantiles_PP <- matrix(nrow = 11, ncol = 9)
Ingarch_quantiles_NOPP <- matrix(nrow = 11, ncol = 9)
Arima_quantiles_PP <- matrix(nrow = 11, ncol = 9)
Arima_quantiles_NOPP <- matrix(nrow = 11, ncol = 9)
p_mQR_matrix_PP <- matrix(nrow = 11, ncol = 9)
p_mQR_matrix_NOPP <- matrix(nrow = 11, ncol = 9)
QGBM_matrix_PP <- matrix(nrow = 11, ncol = 9)
QGBM_matrix_NOPP <- matrix(nrow = 11, ncol = 9)

# Initialize Metrics and Confidence Interval Results
rmse_results <- matrix(NA, nrow = length(2012:2022), ncol = 8)  # RMSE for 8 models
mae_results <- matrix(NA, nrow = length(2012:2022), ncol = 8)   # MAE for 8 models
dr_results <- matrix(NA, nrow = length(2012:2022), ncol = 8)    # DR for 8 models
test_target <- vector()

# Define Directional Accuracy Function
directional_accuracy <- function(forecast, actual) {
  sum(sign(diff(forecast)) == sign(diff(actual))) / (length(actual) - 1)
}

# Quantile Levels
qq <- seq(0.1, 0.9, 0.1)  # Quantiles
start_year <- 1982
i <- 1

############################
# Loop over Test Years
############################
for (test_year in 2012:2022) {
  # Subset the data for training and testing
  train_data <- D[D$YYYY >= start_year & D$YYYY < test_year, ]
  test_data <- D[D$YYYY == test_year, ]
  last <- nrow(test_data)
  
  ##################
  # Predictors 1 + PP
  ##################
  xreg <- train_data[, c("NINA1_std_t_1", "z500_std_t_1", "PPt")]
  xreg_f <- test_data[last, c("NINA1_std_t_1", "z500_std_t_1", "PPt")]
  formula_t <- reformulate(c(names(xreg)), "target")
  
  ### ARIMA + PP ###
  arima <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  forecast_arima <- forecast(arima, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))
  f_arima[i] <- forecast_arima$mean
  # Extract confidence intervals
  Arima_quantiles_PP[i, ] <- forecast_arima$lower[, 2]
  rmse_results[i, 1] <- sqrt(mean((f_arima[i] - test_data$target)^2))  # RMSE
  mae_results[i, 1] <- mean(abs(f_arima[i] - test_data$target))       # MAE
  
  ### Quantile Regression + PP ###
  quantile_model <- rq(formula_t, tau = qq, data = train_data)
  p <- predict(quantile_model, newdata = xreg_f)
  p_QR[i] <- p[5]  # Median prediction
  p_mQR_matrix_PP[i, ] <- p  # All quantiles
  rmse_results[i, 3] <- sqrt(mean((p_QR[i] - test_data$target)^2))  # RMSE
  mae_results[i, 3] <- mean(abs(p_QR[i] - test_data$target))        # MAE
  
  ### Ingarch + PP ###
  in_fit <- tsglm(ts = train_data$target, link = "log", xreg = as.matrix(as.data.frame(xreg)), distr = "poisson")
  Fin_PP <- predict(in_fit, n.ahead = 1, global = TRUE, newxreg = as.matrix(as.data.frame(xreg_f)))
  F_ingarch[i] <- Fin_PP$pred
  Ingarch_quantiles_PP[i, ] <- Fin_PP$interval  # Directly use provided intervals
  rmse_results[i, 5] <- sqrt(mean((F_ingarch[i] - test_data$target)^2))  # RMSE
  mae_results[i, 5] <- mean(abs(F_ingarch[i] - test_data$target))        # MAE
  
  ### QGBRT + PP ###
  QGBRT_PP <- qreg_gbm(
    data = train_data,
    formula = formula_t,
    interaction.depth = 2,
    n.trees = 1000,
    n.minobsinnode = 3,
    shrinkage = 0.05,
    bag.fraction = 0.5,
    quantiles = qq,
    cores = parallel::detectCores()
  )
  f_QGBRT <- predict(QGBRT_PP, newdata = test_data, pred_ntree = 300, sort = TRUE)
  f_QGBRT_PP[i] <- f_QGBRT[5]  # Median prediction
  QGBM_matrix_PP[i, ] <- f_QGBRT[1:9]  # All quantiles
  rmse_results[i, 7] <- sqrt(mean((f_QGBRT_PP[i] - test_data$target)^2))  # RMSE
  mae_results[i, 7] <- mean(abs(f_QGBRT_PP[i] - test_data$target))        # MAE
  
  ##################
  # Predictors 2 - NO PP
  ##################
  xreg <- train_data[, c("NINA1_std_t_1", "z500_std_t_1")]
  xreg_f <- test_data[last, c("NINA1_std_t_1", "z500_std_t_1")]
  formula_t <- reformulate(c(names(xreg)), "target")
  
  ### ARIMA - NO PP ###
  arima_NOPP <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  forecast_arima_NOPP <- forecast(arima_NOPP, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))
  f_arima_NOPP[i] <- forecast_arima_NOPP$mean
  Arima_quantiles_NOPP[i, ] <- forecast_arima_NOPP$lower[, 2]
  rmse_results[i, 2] <- sqrt(mean((f_arima_NOPP[i] - test_data$target)^2))  # RMSE
  mae_results[i, 2] <- mean(abs(f_arima_NOPP[i] - test_data$target))        # MAE
  
  ### Quantile Regression - NO PP ###
  quantile_model_NOPP <- rq(formula_t, tau = qq, data = train_data)
  p2 <- predict(quantile_model_NOPP, newdata = xreg_f)
  p_QR_NOPP[i] <- p2[5]
  p_mQR_matrix_NOPP[i, ] <- p2
  rmse_results[i, 4] <- sqrt(mean((p_QR_NOPP[i] - test_data$target)^2))  # RMSE
  mae_results[i, 4] <- mean(abs(p_QR_NOPP[i] - test_data$target))        # MAE
  
  ### Ingarch - NO PP ###
  in_fit_NOPP <- tsglm(ts = train_data$target, link = "log", xreg = as.matrix(as.data.frame(xreg)), distr = "poisson")
  Fin_NOPP <- predict(in_fit_NOPP, n.ahead = 1, global = TRUE, newxreg = as.matrix(as.data.frame(xreg_f)))
  F_ingarch_NOPP[i] <- Fin_NOPP$pred
  Ingarch_quantiles_NOPP[i, ] <- Fin_NOPP$interval
  rmse_results[i, 6] <- sqrt(mean((F_ingarch_NOPP[i] - test_data$target)^2))  # RMSE
  mae_results[i, 6] <- mean(abs(F_ingarch_NOPP[i] - test_data$target))        # MAE
  
  ### QGBRT - NO PP ###
  QGBRT_NOPP <- qreg_gbm(
    data = train_data,
    formula = formula_t,
    interaction.depth = 2,
    n.trees = 1000,
    n.minobsinnode = 3,
    shrinkage = 0.05,
    bag.fraction = 0.5,
    quantiles = qq,
    cores = parallel::detectCores()
  )
  f_QGBRT_NOPP <- predict(QGBRT_NOPP, newdata = test_data, pred_ntree = 300, sort = TRUE)
  f_QGBRT_NOPP[i] <- f_QGBRT_NOPP[5]
  QGBM_matrix_NOPP[i, ] <- f_QGBRT_NOPP[1:9]
  rmse_results[i, 8] <- sqrt(mean((f_QGBRT_NOPP[i] - test_data$target)^2))  # RMSE
  mae_results[i, 8] <- mean(abs(f_QGBRT_NOPP[i] - test_data$target))        # MAE
  
  # Save target values
  test_target[i] <- test_data$target[last]
  i <- i + 1
}

# Combine CI Results
ci_results <- list(
  ARIMA_PP = Arima_quantiles_PP,
  ARIMA_NOPP = Arima_quantiles_NOPP,
  Ingarch_PP = Ingarch_quantiles_PP,
  Ingarch_NOPP = Ingarch_quantiles_NOPP,
  QR_PP = p_mQR_matrix_PP,
  QR_NOPP = p_mQR_matrix_NOPP,
  QGBRT_PP = QGBM_matrix_PP,
  QGBRT_NOPP = QGBM_matrix_NOPP
)

# Calculate Directional Accuracy (DR)
dr_results <- sapply(
  list(f_arima, f_arima_NOPP, p_QR, p_QR_NOPP, F_ingarch, F_ingarch_NOPP, f_QGBRT_PP, f_QGBRT_NOPP),
  function(forecast) directional_accuracy(forecast, test_target)
)

# Final Summaries
rmse_summary <- data.frame(
  Year = 2012:2022,
  ARIMA_PP_RMSE = rmse_results[, 1],
  ARIMA_NOPP_RMSE = rmse_results[, 2],
  QR_PP_RMSE = rmse_results[, 3],
  QR_NOPP_RMSE = rmse_results[, 4],
  Ingarch_PP_RMSE = rmse_results[, 5],
  Ingarch_NOPP_RMSE = rmse_results[, 6],
  QGBRT_PP_RMSE = rmse_results[, 7],
  QGBRT_NOPP_RMSE = rmse_results[, 8]
)

mae_summary <- data.frame(
  Year = 2012:2022,
  ARIMA_PP_MAE = mae_results[, 1],
  ARIMA_NOPP_MAE = mae_results[, 2],
  QR_PP_MAE = mae_results[, 3],
  QR_NOPP_MAE = mae_results[, 4],
  Ingarch_PP_MAE = mae_results[, 5],
  Ingarch_NOPP_MAE = mae_results[, 6],
  QGBRT_PP_MAE = mae_results[, 7],
  QGBRT_NOPP_MAE = mae_results[, 8]
)

dr_summary <- data.frame(
  Model = c("ARIMA_PP", "ARIMA_NOPP", "QR_PP", "QR_NOPP", "Ingarch_PP", "Ingarch_NOPP", "QGBRT_PP", "QGBRT_NOPP"),
  Directional_Accuracy = dr_results
)

# Print Results
print(rmse_summary)
print(mae_summary)
print(dr_summary)
