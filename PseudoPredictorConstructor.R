#################################
# DATA PREPARATION
################################

# Load libraries
library(ProbCast)
library(readr)
library(tidyverse)
library(forecast)

# Read data
D <- read_csv("D.csv")
D2 <- read_csv("D_var_LSTM.csv")

# Decompose time series
TS <- ts(D$target, frequency = 5)
d <- decompose(TS)
stl <- stl(TS, s.window = 4)

# Extract components
r <- d$random
s <- d$seasonal
t <- d$trend

# Update D dataframe with components
D$seasonal <- s
D$trend <- t
D$remainder <- r
D$stl_r <- stl$time.series[,"remainder"]
D$stl_t <- stl$time.series[,"trend"]
D$stl_s <- stl$time.series[,"seasonal"]

###########################################
# VARIABLE SELECTION
###########################################

# Load libraries
library(dplyr)
library(car)

# Merge dataframes
D2 <- as.data.frame(D2)
md <- left_join(D, D2, by = "year")
df <- md[,-c(1,2)]

# Create lag variable
df1 <- df[2:42,]
df1$z500_std_t_1 <- na.omit(lag(md$z500_std))

# Print summary of the best model
D <- df1

# Creation
DIS <- scale(D$ESPI.x) - scale(D$target)

# Calculate correlations
cor(DIS[1:40],D$target[2:41])
sDIS <- scale(DIS[1:42])

# Forecast using pseudo-P
Pseudo_P <- vector()
for(i in seq(11)){
  a <- auto.arima(scale(D$target[2:29+i]), xreg = DIS[1:28+i] )
  f <- forecast(a, h = 1, xreg = sDIS[(29+i)])
  Pseudo_P[i] <- f$mean
}
cor(Pseudo_P,D$target[30:40])

#################
# Final forecast
#################

a1 <- auto.arima(scale(D$target[2:41]), xreg = sDIS[1:40])
PPt <- append(a1$fitted, NA)
D$PPt <- PPt
PP <- append(rep(NA, 30), Pseudo_P)
D$PPf <- PP


############################
# Empty object preparation
############################

f_arima<- p_QR<- vector()
f_arima_NOPP <- p_QR_NOPP<- vector()

test_year=2011
start_year=1982
mae_results <- matrix(NA, nrow = length(2011:2022), ncol = 4)  
test_target=vector()
i=1
for (test_year in 2011:2022){
  # Subset the data for training and testing
  train_data <- D[D$YYYY >= start_year & D$YYYY < test_year, ]
  test_data <- D[D$YYYY == test_year, ]
  last <- nrow(test_data)
  
  
  ##################
  # Predictors 1 + PP
  ###################
  
  xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
  xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1","PPf")]
  colnames(xreg_f)[3]= "PPt" 
  # xreg_f[3] <- train_data[nrow(train_data),"ESPI.x"]
  formula_t <- reformulate(c(names(xreg)), "target")

  # Model without DecomposD
  # ARIMA on raw data
  arima <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  f_arima[i] <- forecast(arima, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))$mean
  
  # Quantile Regression
  tau <- 0.5 # Quantile level (e.g., median)
  quantile_model <- rq(formula_t, tau = tau, data = train_data)
  p_QR[i] <- round(predict(quantile_model, newdata = test_data[last, ]),3)[[1]]
  

  ##################
  # Predictors 2 + NO PP
  ###################

  xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1")]
  xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1")]
  # xreg_f[3] <- train_data[nrow(train_data),"ESPI.x"]
  formula_t <- reformulate(c(names(xreg)), "target")
  # Model without DecomposD
  # ARIMA on raw data
  arima_NOPP <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  f_arima_NOPP[i] <- forecast(arima_NOPP, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))$mean
  
  # Quantile Regression
  tau <- 0.5 # Quantile level (e.g., median)
  quantile_model_NOPP <- rq(formula_t, tau = tau, data = train_data)
  p_QR_NOPP[i] <- round(predict(quantile_model_NOPP, newdata = test_data[last, ]),3)[[1]]
  

  test_target[i] <- test_data$target[last]
  i <- i + 1
}

# Compute MAE for each model
for (i in seq(12)){
  mae_results[i, 1] <- mean(abs(f_arima[i] - test_target[i]))
  mae_results[i, 2] <- mean(abs(f_arima_NOPP[i] - test_target[i]))
  mae_results[i, 3] <- mean(abs(p_QR[i] - test_target[i]))
  mae_results[i, 4] <- mean(abs(p_QR_NOPP[i] - test_target[i]))
 
}
colnames(mae_results) <- c("ARIMA+PP", "ARIMA", "QR+PP", "QR")
rownames(mae_results) <- 2011:2022
colMeans(mae_results[2:10,], )
