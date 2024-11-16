#
#           Hurricane forecasting
#
##################################################

rm(list=ls())

setwd("H:/Il mio Drive/Hurricane forecasting")
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

data <- rio::import("Monthly-data.csv")
data <- data[1:528,]
train.l <- 396
out.l <- nrow(data)-train.l

# To export

xreg <- data[, c( "time_series1.nina1","time_series2.z500_std")]
xregL <- apply(xreg, 2, function(x){Hmisc::Lag(x,12)})
yt <- ts(data[,4], start=data[1,2], frequency=12)
D <- seasonaldummy(yt)
datasetfinale <- data.frame(data[,c(2,3)],"yt"=yt,xregL,D)
rio::export(datasetfinale, "datasetfinale.csv")

# Again here

rm(list=ls())

data <- rio::import("Monthly-data.csv")
data <- data[1:528,]
train.l <- 396
out.l <- nrow(data)-train.l

#---------- With covariates + seasonal

Forecasts <- matrix(NA, nrow=out.l, ncol=15) # ncol number of models
yt <- ts(data[1:(train.l+1-1),4], start=data[1,2], frequency=12)
xreg <- data[1:(train.l+1-1), c( "time_series1.nina1","time_series2.z500_std")]
xreg <- apply(xreg, 2, function(x){Hmisc::Lag(x,12)})

modelNNAR <- nnetar(yt, xreg=seasonaldummy(yt))
modelNNARb <- nnetar(yt, xreg=cbind(seasonaldummy(yt),xreg))
modelNNARc <- nnetar(yt, xreg=cbind(xreg))

# Yearly rolling

for (i in seq(1, out.l, by=12)){
  
  yt <- ts(data[i:(train.l+i-1),4], start=data[i,2], frequency=12)
  xreg <- data[i:(train.l+i-1), c( "time_series1.nina1","time_series2.z500_std")]
  xreg <- apply(xreg, 2, function(x){Hmisc::Lag(x,12)})
  
  model1 <- auto.arima(yt, xreg=seasonaldummy(yt))
  model1b <- auto.arima(yt, xreg=cbind(seasonaldummy(yt),xreg))
  model1c <- auto.arima(yt, xreg=cbind(xreg))
  
  model2 <- tsglm(ts = data[i:(train.l+i-1),4], model = list(past_obs = 1, past_mean = 1), xreg=seasonaldummy(yt), link = "log", distr = "poisson")
  model2b <- tsglm(ts = data[(i+12):(train.l+i-1),4], model = list(past_obs = 1, past_mean = 1), xreg=cbind(seasonaldummy(yt)[-c(1:12),],xreg[-c(1:12),]), link = "log", distr = "poisson")
  model2c <- tsglm(ts = data[(i+12):(train.l+i-1),4], model = list(past_obs = 1, past_mean = 1), xreg=cbind(xreg[-c(1:12),]), link = "log", distr = "poisson")
  
  model3 <- nnetar(yt, xreg=seasonaldummy(yt), model=modelNNAR)
  model3b <- nnetar(yt, xreg=cbind(seasonaldummy(yt),xreg), model=modelNNARb)
  model3c <- nnetar(yt, xreg=cbind(xreg), model=modelNNARc)
   
  # QR
   tau <- seq(0.1,0.9,0.1)
   dff <-data.frame(yt,seasonaldummy(yt))
   model4 <- rq(yt ~ ., data=dff, tau = tau)
   dff <-data.frame(yt,seasonaldummy(yt),xreg)
   model4b <- rq(yt ~ ., data=dff, tau = tau)
   dff <-data.frame(yt,xreg)
   model4c <- rq(yt ~ ., data=dff, tau = tau)
   
  # QBRT (da qui)
   
   # model5 <- qreg_gbm(formula = yt~seasonaldummy(yt),
   #                        interaction.depth = 2,
   #                        n.trees = 1000,
   #                        n.minobsinnode = 3,
   #                        shrinkage = 0.05,
   #                        bag.fraction = 0.5,
   #                        quantiles =tau,
   #                        cores = detectCores())
   # 
   # model5b <- qreg_gbm(formula = yt~cbind(seasonaldummy(yt),xreg), # yt[-c(1:12)]~seasonaldummy(yt)[-c(1:12),]+xreg[-c(1:12),]
   #                    interaction.depth = 2,
   #                    n.trees = 1000,
   #                    n.minobsinnode = 3,
   #                    shrinkage = 0.05,
   #                    bag.fraction = 0.5,
   #                    quantiles =tau,
   #                    cores = detectCores())
   # 
   # model5c <- qreg_gbm(formula = yt~xreg, # yt[-c(1:12)]~xreg[-c(1:12),]
   #                    interaction.depth = 2,
   #                    n.trees = 1000,
   #                    n.minobsinnode = 3,
   #                    shrinkage = 0.05,
   #                   bag.fraction = 0.5,
   #                    quantiles =tau,
   #                    cores = detectCores())
   
  # Forecasts
  
  Forecasts[i,1] <- sum( round(forecast(model1,xreg=seasonaldummy(yt,h=12), h=12)$mean) )
  # Forecasts[i,2] <- sum( round(forecast(model1b,xreg=cbind(seasonaldummy(yt,h=12),apply(xreg,2,function(x){forecast(ses(x,h=12))$mean})), h=12)$mean) )
  Forecasts[i,2] <- sum( round(forecast(model1b,xreg=cbind(seasonaldummy(yt,h=12),tail(xreg,12)), h=12)$mean) )
  Forecasts[i,3] <- sum( round(forecast(model1c,xreg=tail(xreg,12), h=12)$mean) )
  
  Forecasts[i,4] <- sum( round(predict(model2, newxreg=seasonaldummy(yt,h=12), n.ahead = 12, level = 0.9, global = TRUE, B = 2000)$pred) )
  # Forecasts[i,5] <- sum( round(predict(model2b, xreg=cbind(seasonaldummy(yt,h=1),xreg[,]), n.ahead = 1, level = 0.9, global = TRUE, B = 2000)$pred) )
  Forecasts[i,5] <- sum( round(predict(model2b, newxreg=cbind(seasonaldummy(yt,h=12),tail(xreg,12)), n.ahead = 12, level = 0.9, global = TRUE, B = 2000)$pred) )
  Forecasts[i,6] <- sum( round(predict(model2c, newxreg=tail(xreg,12), n.ahead = 12, level = 0.9, global = TRUE, B = 2000)$pred) )
  
  Forecasts[i,7] <- sum( round(forecast(model3,xreg=seasonaldummy(yt,h=12), h=12)$mean) )
  Forecasts[i,8] <- sum( round(forecast(model3b,xreg=cbind(seasonaldummy(yt,h=12), tail(xreg,12)), h=12)$mean) )
  Forecasts[i,9] <- sum( round(forecast(model3c,xreg=cbind(tail(xreg,12)), h=12)$mean) )
  
  Forecasts[i,10] <- sum( predict(model4, newdata = as.data.frame(seasonaldummy(yt,h=12)))[,5] )
  Forecasts[i,11] <- sum( predict(model4b, newdata = data.frame(seasonaldummy(yt,h=12),tail(xreg,12)))[,5] )
  Forecasts[i,12] <- sum( predict(model4c, newdata = as.data.frame(tail(xreg,12)))[,5] )
  
  # # Forecast QBRT
  # 
  # f_QGBRT <- predict(QGBRT_NOPP, newdata = test_data, pred_ntree = 300, sort= TRUE)
  # f_QGBRT_NOPP[i]=f_QGBRT[5]
  # QGBM_matrix_NOPP[i,1:9]=as.numeric(f_QGBRT[1:9])
  
  print(paste0("Iteration: ",round((i/out.l)*100,4),"% completed"))
}

# Set to zero negative forecasts

Forecasts0 <- Forecasts
Forecasts0[Forecasts0<0] <- 0

# Magnitude accuracy

idx <- rep(1:ceiling(length(data[(train.l+1):nrow(data),4]) / 12), each = 12)[1:length(data[(train.l+1):nrow(data),4])]
ytest <- tapply(data[(train.l+1):nrow(data),4], idx, sum, na.rm = TRUE)

#Error <- data[(train.l+1):nrow(data),4] - Forecasts0
Error <- as.numeric(ytest) - as.matrix(na.omit(Forecasts0[,1:12]))
Acc <- cbind(round(apply(Error,2,function(x){sqrt(mean(x^2, na.rm=T))}),2),
              round(apply(Error,2,function(x){mean(abs(x),na.rm=T)}),2))
rownames(Acc) <- c(paste0("ARMA_",1:3),paste0("INGARCH_",1:3),paste0("ARNN_",1:3),paste0("QR_",1:3))
colnames(Acc) <- c("RMSE","MAE")
Acc

# Directional accuracy

directional_accuracy <- function(forecast,actual){sum(sign(diff(forecast)) == sign(diff(actual))) / (length(actual) - 1)}

# DR <- c(directional_accuracy(Forecasts0[,1],data[(train.l+1):nrow(data),4]),
#         directional_accuracy(Forecasts0[,2],data[(train.l+1):nrow(data),4]),
#         directional_accuracy(Forecasts0[,3],data[(train.l+1):nrow(data),4]),
#         directional_accuracy(Forecasts0[,4],data[(train.l+1):nrow(data),4]),
#         directional_accuracy(Forecasts0[,5],data[(train.l+1):nrow(data),4]))

DAcc <- sapply(1:12, function(i) {
  directional_accuracy(as.matrix(na.omit(Forecasts0[, i])), as.numeric(ytest))
})

names(DAcc) <- c(paste0("ARMA_",1:3),paste0("INGARCH_",1:3),paste0("ARNN_",1:3),paste0("QR_",1:3))
DAcc
