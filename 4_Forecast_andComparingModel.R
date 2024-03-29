#################################
# DATA PREPARATION
################################
library(ProbCast)
library(readr)
library(tidyverse)
D <- read_csv("C:/Users/2692812C/OneDrive - University of Glasgow/Desktop/Projects/Glasgow-Hurricanes-main/Glasgow-Hurricanes-main/D.csv")
D=read_csv("D.csv")
D2=read_csv("D_var_LSTM.csv")
source("FunctionToSource.R", encoding = "UTF-8")
library(forecast)
TS=ts(D$target, frequency = 5)
d=decompose(TS)
stl = stl(TS, s.window = 4)
plot(stl)
#plot(d)
#Acf(na.omit(d$trend))
#auto.arima(d$trend)
r=d$random
s=d$seasonal
t=d$trend
D$seasonal=s
D$trend=t
D$remainder=r

D$stl_r=stl$time.series[,"remainder"]
D$stl_t=stl$time.series[,"trend"]
D$stl_s=stl$time.series[,"seasonal"]


###########################################
# VARIABLE SELECTION
###############################
library(dplyr)
library(car)
D2=as.data.frame(D2)
md <- left_join(D, D2, by = "year")

# Function to perform forward selection
forward_select <- function(data, response) {
  data<- data[, -which(names(data) == "target")]
  data=na.omit(data)
  predictors <- setdiff(names(data), response)
  data1=as.data.frame(apply(data,2,scale))
  selected_predictors <- character(0)
  best_model <- lm(paste(response, "~ 1"), data = data1)
  best_aic <- AIC(best_model)
  
  while (length(predictors) > 0) {
    candidate_aic <- numeric(length(predictors))
    for (i in seq_along(predictors)) {
      formula <- reformulate(c(selected_predictors, predictors[i]), response)
      model <- lm(formula, data = data1)
      candidate_aic[i] <- AIC(model)
    }
    
    best_candidate <- which.min(candidate_aic)
    
    if (candidate_aic[best_candidate] < best_aic) {
      best_model <- lm(reformulate(c(selected_predictors, predictors[best_candidate]), response), data = data)
      best_aic <- candidate_aic[best_candidate]
      selected_predictors <- c(selected_predictors, predictors[best_candidate])
      predictors <- setdiff(predictors, predictors[best_candidate])
    } else {
      break
    }
  }
  
  return(best_model)
}
library(zoo)


df=md[,-c(1,2)]

# Perform forward selection
best_model <- forward_select(df, "stl_r")
best_model
#cor(md$z500_std[2:39], md$stl_r[3:40])
#cor(md$OLR[1:39], md$stl_r[1:39])
#cor(md$ESPI.x[1:39], md$stl_r[1:39])

df1=df[2:42,]
df1$z500_std_t_1=na.omit(lag(md$z500_std))
# Print summary of the best model
summary(best_model)


plot(D$year[1:42],scale(D$target[1:42]), pch=19, col=2)
abline(v=2013)
abline(v=2020)
abline(v=2009)
abline(v=2005)
abline(v=1997)
abline(v=1995)
points(D$year, scale(D$ESPI.x), pch=19, col=4)
points(D$year, scale(D$oni), pch=19, col=5)
points(D$year, scale(D$nina3_y_anom), pch=19, col=6)
points(D$year[1:42], scale(D$OLR[1:42]), pch=19, col=7)
lines(D$year[1:42], scale(D$OLR[1:42]), col=7)


DIS=scale(D$ESPI)-scale(D$target)

cor(DIS[1:40],D$target[2:41])
sDIS=scale(DIS[1:42])

Pseudo_P=vector()
for(i in seq(12)){
  a=auto.arima(scale(D$target[2:29+i]), xreg =sDIS[1:28+i] )
  f=forecast(a, h=1 ,xreg=sDIS[(29+i)])
  Pseudo_P[i]=f$mean
}

cor(Pseudo_P,D$target[29:40])

plot(seq(2010,2021), scale(Pseudo_P))
points(seq(2010,2021), scale(D$target[29:40]), pch=19, col="red")

a1=auto.arima(scale(D$target[2:41]), xreg =sDIS[1:40])
PPt=append(a1$fitted,NA)
PP=append(rep(NA,29),Pseudo_P)
D$PPf=PP

str(PP)
cor(PP,D$target[1:40])

########################################################
# Forecast given the new iNSTRUCTIONS
#########################################################
library(tscount)#for ingarch
library(quantreg)#for quant reg
#Empty objects
test_target<-f_arima<- P_QGB<- F_ingarch <- p_QR<-vector()
Dec_QR <-Dec_QGBRT<-vector()

test_year=2011
start_year=1982
mae_results <- matrix(NA, nrow = length(2011:2022), ncol = 6)  
test_target=vector()
i <- 1
D=as.data.frame(df1)
D <- D[, -which(names(D) == "remainder")]
D <-D[, -which(names(D) == "trend")]
D$PPf=PP
cor(D[1:40,],D$target[2:41])
D$stl_t
i=1
for (test_year in 2011:2022){
  
  # Subset the data for training and testing
  train_data <- D[D$YYYY >= start_year & D$YYYY < test_year, ]
  test_data <- D[D$YYYY == test_year, ]
  last <- nrow(test_data)
  
  xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
  xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1","PPf")]
  colnames(xreg_f)[3]= "PPt" 
  # xreg_f[3] <- train_data[nrow(train_data),"ESPI.x"]
  
  
  
  formula_t <- reformulate(c(names(xreg)), "target")
  formula_r <- reformulate(c(names(xreg)), "stl_r")
  formula_r <- reformulate(c(names(xreg)), "stl_s")
  formula_r <- reformulate(c(names(xreg)), "stl_t")
  # Model without DecomposD
  # ARIMA on raw data
  arima <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  f_arima[i] <- forecast(arima, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))$mean
  
  # INGARCH fit
  # Logarithmic link function with Poisson distribution:
  in_fit <- tsglm(ts = train_data$target, link = "log", xreg =  as.matrix(as.data.frame(xreg)), distr = "nbinom")
  F_ingarch[i] <- predict(in_fit, n.ahead = 1, level = 0.9, global = TRUE, B = 2000, newxreg = as.matrix(as.data.frame(xreg_f)))$p
  
  # QGBRT Train the models
  #QGBRT <- convenient_qreg_gbm(train_data, "Data",xreg)
  #pQ <- predict(QGBRT, newdata = test_data, pred_ntree = 300, sort = TRUE)
  #P_QGB[i] <- pQ[1,1]
  
  # Quantile Regression
  tau <- 0.5 # Quantile level (e.g., median)
  quantile_model <- rq(formula_t, tau = tau, data = train_data)
  p_QR[i] <- round(predict(quantile_model, newdata = test_data[last, ]),3)[[1]]
  
  #######################################
  # With Decomposition
  ######################################
  #1
  #QGBRT_res <- convenient_qreg_gbm(train_data, "Remainder",xreg)
  #QGBRT_sea <- convenient_qreg_gbm(train_data, "Seasonal",xreg)
  #QGBRT_trend <- convenient_qreg_gbm(train_data, "Trend",xreg)
  
  
  #pQ_res <- predict(QGBRT_res, newdata = test_data, pred_ntree = 300, sort= TRUE)   
  #pQ_sea <- predict(QGBRT_sea, newdata = test_data, pred_ntree = 300, sort= TRUE)   
  #pQ_trend <- predict(QGBRT_trend, newdata = test_data, pred_ntree = 300, sort= TRUE)   
  
  #Dec_QGBRT[i]=test_data$stl_s[1]+test_data$stl_t[1]+pQ_res[1,1]
  #Dec_QGBRT[i]=pQ_trend[1,1]+pQ_sea[1,1]+pQ_res[1,1]
  #
  tau=0.5
  quantile_model_res <- rq(formula_r, tau = tau, data = train_data)
  arima_t <- auto.arima(train_data$stl_t, xreg = as.matrix(as.data.frame(xreg)))
  arima_s <- auto.arima(train_data$stl_s, xreg = as.matrix(as.data.frame(xreg)))
  #quantile_model_sea <- rq(stl_s ~ NINA1_avg_t_1 + NINA1_std_t_1 + z500_std_t_1, tau = tau, data = train_data)
  #quantile_model_trend <- rq(stl_t ~ NINA1_avg_t_1 + NINA1_std_t_1 + z500_std_t_1, tau = tau, data = train_data)
  
  # p_QR_res <- predict(quantile_model_res, newdata = test_data[last, ])
  #p_QR_sea <- predict(quantile_model_res, newdata = test_data[last, ])
  # p_QR_trend <- predict(quantile_model_res, newdata = test_data[last, ])
  f_arima_s <- forecast(arima_s, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))$mean
  f_arima_t <- forecast(arima_t, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))$mean
  
  #Dec_QR[i]=test_data$stl_s[1]+test_data$stl_t[1]+ p_QR_res[1]
  Dec_QR[i]=f_arima_s[1]+f_arima_t[1]+ p_QR_res[1]
  
  
  test_target[i] <- test_data$target[last]
  
  
  
  i <- i + 1
}


# Compute MAE for each model
for (i in seq(12)){
  mae_results[i, 1] <- mean(abs(f_arima[i] - test_target[i]))
  mae_results[i, 2] <- mean(abs(F_ingarch[i] - test_target[i]))
  mae_results[i, 3] <- mean(abs(P_QGB[i] - test_target[i]))
  mae_results[i, 4] <- mean(abs(p_QR[i] - test_target[i]))
  mae_results[i, 5] <- mean(abs(Dec_QGBRT[i] - test_target[i]))
  mae_results[i, 6] <- mean(abs(Dec_QR[i] - test_target[i]))
}

ensamble=cbind(F_ingarch,Dec_QR,test_target)
e=rowMeans(ensamble)
mean(abs(e-test_target))

# Print MAE for each model and each iteration
colnames(mae_results) <- c("ARIMA", "INGARCH", "QGBRT", "Quantile Regression", "D QGBRT", "D QR")
rownames(mae_results) <- 2011:2022
print(mae_results)
colMeans(mae_results)

# Assuming you have vectors of forecasted values (forecast) and actual values (actual)
# forecast <- c(1, 2, 3, 4, 5)  # Example forecasted values
# actual <- c(1, 2, 4, 3, 5)    # Example actual values
# Calculate Directional Accuracy
directional_accuracy<-function(forecast,actual){sum(sign(diff(forecast)) == sign(diff(actual))) / (length(actual) - 1)}

DR=c(directional_accuracy(f_arima,test_target),
     directional_accuracy(F_ingarch,test_target),
     directional_accuracy(P_QGB,test_target),
     directional_accuracy(p_QR,test_target),
     directional_accuracy(Dec_QGBRT,test_target),
     directional_accuracy(Dec_QR,test_target))

# Print Directional Accuracy
print(paste("Directional Accuracy:", DR, colnames(mae_results)))


# Load required packages
library(knitr)

# Convert directional accuracy and MAE results to data frames
DR_df <- data.frame(Model = colnames(mae_results), Directional_Accuracy = DR)
mae_results_df <- as.data.frame(mae_results)
apply(mae_results_df,2,mean)
# Print directional accuracy table
cat("Directional Accuracy:\n")
kable(DR_df)
# Print MAE results table
cat("\nMAE Results:\n")
kable(mae_results_df)


library(ggplot2)
library(tidyr)

# Create a data frame with the provided values
data <- data.frame(
  Year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  ARIMA = c(3.4579516, 1.3475037, 8.6684106, 1.0915224, 1.9225489, 0.1776720, 1.0636621, 2.3074646, 0.0813674, 6.0268825, 3.4111994, 1.3782438),
  INGARCH = c(0.5375502, 2.5823754, 6.7719329, 1.5002013, 1.1422231, 0.7456244, 0.5062338, 2.4050784, 0.6625841, 5.5880906, 0.5038368, 2.5706112),
  QGBRT = c(0.3156124, 3.2284311, 8.5893238, 1.8232273, 0.4021211, 0.7382627, 3.8612436, 2.9300716, 1.2730869, 5.7299330, 1.1185956, 3.8303588),
  Quantile_Regression = c(0.594, 2.999, 5.812, 1.514, 2.070, 1.234, 1.178, 3.444, 0.967, 6.344, 0.545, 2.764),
  D_QGBRT = c(0.0735126, 0.1756668, 4.5555228, 2.6961489, 6.166499, 0.880209, 0.2795126, 3.343902, 0.4328697, 1.489915, 0.3256231, 1.0108914),
  D_QR = c(0.5114469, 1.3425525, 2.8246804, 1.8310886, 4.9344909, 0.2864528, 1.0157988, 2.6975838, 0.6742741, 2.2191575, 0.4242184, 1.2341334)
)

# Convert data from wide to long format
data_long <- gather(data, Method, Value, -Year)

# Plot
ggplot(data_long, aes(x = Year, y = Value, color = Method, group = Method)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance Comparison of Methods Over Years",
       x = "Year",
       y = "MAE Error",
       color = "Method") +
  theme_minimal()

