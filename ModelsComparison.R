
###########################################
# VARIABLE SELECTION
###########################################
# Load libraries
library(dplyr)
library(car)
library(tscount)#for ingarch
library(quantreg)#for quant reg
library(ProbCast)
library(readr)
library(tidyverse)

############################
# Empty object preparation
############################

library(readr)
D <- read_csv("D_python.csv")
View(D_python)

#Prediction Objects
f_arima<- p_QR<- F_ingarch<-f_QGBRT_PP<-vector()
f_arima_NOPP <- p_QR_NOPP<- F_ingarch_NOPP<-f_QGBRT_NOPP <-vector()

#Quantiles objects
Ingarch_quantiles_PP=matrix(nrow=11, ncol=9)
Ingarch_quantiles_NOPP=matrix(nrow=11, ncol=9)
Arima_quantiles_PP=matrix(nrow=11, ncol=9)
Arima_quantiles_NOPP=matrix(nrow=11, ncol=9)
p_mQR_matrix_PP=matrix(nrow=11, ncol=9)
p_mQR_matrix_NOPP=matrix(nrow=11, ncol=9)

QGBM_matrix_PP=matrix(nrow=11, ncol=9)
QGBM_matrix_NOPP=matrix(nrow=11, ncol=9)

test_year=2011
start_year=1982
mae_results <- matrix(NA, nrow = length(2012:2022), ncol = 8)  
test_target=vector()
qq=seq(0.1,0.9,0.1)
i=1
#10 Years Experiment;

for (test_year in 2012:2022){
  # Subset the data for training and testing
  train_data <- D[D$YYYY >= start_year & D$YYYY < test_year, ]
  test_data <- D[D$YYYY == test_year, ]
  last <- nrow(test_data)
  

  ##################
  # Predictors 1 + PP
  ###################
  
  xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
  xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
  
  # xreg_f[3] <- train_data[nrow(train_data),"ESPI.x"]
  formula_t <- reformulate(c(names(xreg)), "target")
  
  
  # ARIMA on raw data
  arima <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  forecast_arima <- forecast(arima, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))
  f_arima[i] <-forecast_arima$mean
  # Extract prediction quantiles
  # quantiles <- quantile(c(forecast_arima$lower,forecast_arima$mean,forecast_arima$upper), probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  sd_arima=sd(forecast_arima$model$residuals)
  Arima_quantiles_PP[i,1:9]=qnorm(seq(0.1,0.9,0.1), forecast_arima$mean, sd_arima)
  
  # Quantile Regression
  tau <- qq # Quantile level (e.g., median)
  quantile_model <- rq(formula_t, tau = tau, data = train_data)
  p=predict(quantile_model, newdata = xreg_f)
  p_QR[i] <- round(p,3)[[5]]
  #quantiles
  p_mQR_matrix_PP[i,1:9]=p

  # Ingarch
  in_fit <- tsglm(ts = train_data$target, link = "log", xreg =  as.matrix(as.data.frame(xreg)), distr = "poisson")
  Fin_PP=predict(in_fit, n.ahead = 1, global = TRUE, newxreg = as.matrix(as.data.frame(xreg_f)))
  F_ingarch[i] <- Fin_PP$pred
  #quantiles
  quantile <- qnorm(seq(0.1,0.9,0.1), mean = Fin_PP$pred, sd = sd(in_fit$residuals))
  Ingarch_quantiles_PP[i,1:9]= quantile  
  
  
  #colnames(xreg_f)[3]= "PPt" 
  #QGBRT+PP
  formula <- reformulate(c(names(xreg)), "target")
  QGBRT_PP <- qreg_gbm(data = train_data,
                      formula = formula,
                      interaction.depth = 2,
                      n.trees = 1000,
                      n.minobsinnode = 3,
                      shrinkage = 0.05,
                      bag.fraction = 0.5,
                      quantiles =tau,
                      cores = detectCores())
  f_QGBRT <- predict(QGBRT_PP, newdata = test_data, pred_ntree = 300, sort= TRUE)
  f_QGBRT_PP[i]=f_QGBRT[5]
  QGBM_matrix_PP[i,1:9]=as.numeric(f_QGBRT[1:9])
  
  ##################
  # Predictors 2 + NO PP
  ###################

  xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1")]
  xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1")]
  #xreg_f[3] <- train_data[nrow(train_data),"ESPI.x"]
  formula_t <- reformulate(c(names(xreg)), "target")

  
  # QGBRT 
  formula <- reformulate(c(names(xreg)), "target")
  QGBRT_NOPP <- qreg_gbm(data = train_data,
                       formula = formula,
                       interaction.depth = 2,
                       n.trees = 1000,
                       n.minobsinnode = 3,
                       shrinkage = 0.05,
                       bag.fraction = 0.5,
                       quantiles =tau,
                       cores = detectCores())
  f_QGBRT <- predict(QGBRT_NOPP, newdata = test_data, pred_ntree = 300, sort= TRUE)
  f_QGBRT_NOPP[i]=f_QGBRT[5]
  QGBM_matrix_NOPP[i,1:9]=as.numeric(f_QGBRT[1:9])
  
  # ARIMA 
  arima_NOPP <- auto.arima(train_data$target, xreg = as.matrix(as.data.frame(xreg)))
  forecast_arima <- forecast(arima_NOPP, h = 1, xreg = as.matrix(as.data.frame(xreg_f)))
  f_arima_NOPP[i] <- forecast_arima$mean
  # Extract prediction quantiles
  sd_arima=sd(forecast_arima$model$residuals)
  Arima_quantiles_NOPP[i,1:9]=qnorm(seq(0.1,0.9,0.1), forecast_arima$mean, sd_arima)
  
  
  
  # Quantile Regression
  quantile_model_NOPP <- rq(formula_t, tau = tau, data = train_data)
  p2=predict(quantile_model_NOPP, newdata = xreg_f)
  p_QR_NOPP[i] <-  round(p,3)[[5]]
  p_mQR_matrix_NOPP[i,1:9]=p
  
  # Ingarch
  in_fit <- tsglm(ts = train_data$target, link = "log", xreg =  as.matrix(as.data.frame(xreg)), distr = "poisson")
  Fin<-predict(in_fit, n.ahead = 1, global = TRUE, newxreg = as.matrix(as.data.frame(xreg_f)))
  F_ingarch_NOPP[i] <- Fin$pred
  # Calculate quantiles using inverse CDF
  quantiles <- qnorm(seq(0.1,0.9,0.1), mean = Fin$pred, sd = sd(in_fit$residuals))
  Ingarch_quantiles_NOPP[i,1:9]=quantiles  # For a normal distribution
  

  #test_target[i] <- test_data$target[last]
  i <- i + 1
}


unlist(f_QGBRT_PP)
points(test_target, pch=19, col="red")

test_target <- D[D$YYYY >= 2012 & D$YYYY <= 2022, "target"]
test_target=test_target$target
# Compute MAE for each model
for (i in seq(11)){
  mae_results[i, 1] <- mean(abs(f_arima[i] - test_target[i]))
  mae_results[i, 2] <- mean(abs(f_arima_NOPP[i] - test_target[i]))
  
  mae_results[i, 3] <- mean(abs(p_QR[i] - test_target[i]))
  mae_results[i, 4] <- mean(abs(p_QR_NOPP[i] - test_target[i]))
  
  mae_results[i, 5] <- mean(abs(F_ingarch[i] - test_target[i]))
  mae_results[i, 6] <- mean(abs(F_ingarch_NOPP[i] - test_target[i]))

  mae_results[i, 7] <- mean(abs(unlist(f_QGBRT_PP[i]) - test_target[i]))
  mae_results[i, 8] <- mean(abs(unlist(f_QGBRT_NOPP[i]) - test_target[i]))
}

mae_results
colnames(mae_results) <- c("ARIMA+PP", "ARIMA", "QR+PP", "QR", "Ingarch+PP", "Ingarch","QGBRT+PP","QGBRT")
rownames(mae_results) <- 2012:2022
colMeans(mae_results[1:11,], )

#plot
# Data
# Create a data frame
data <- data.frame(
  Year = 2012:2022,
  Avg_with_PP = rowMeans(cbind(mae_results[1:11, 1], mae_results[1:11, 3], mae_results[1:11, 6])),
  Avg_without_PP = rowMeans(cbind(mae_results[1:11, 2], mae_results[1:11, 4], mae_results[1:11, 4]))
)

# Plot
plot(data$Year, data$Avg_with_PP, type = "o", col = "blue", ylim = range(data[, -1], na.rm = TRUE), xlab = "Year", ylab = "MAE", main = "Comparison of the models performances", lwd = 2)
lines(data$Year, data$Avg_without_PP, type = "o", col = "red", lty = 2, lwd = 2)
abline(v = c(2013, 2018, 2020), lwd = 2, col = "gray")
legend("topleft", legend = c("Models averages with PP", "Models averages without PP"), col = c("blue", "red"), lty = c(1, 2), lwd = 2, pch = 16)

library(ggplot2)

# Create a data frame
data <- data.frame(
  Year = 2012:2022,
  Avg_with_PP = rowMeans(cbind(mae_results[1:11, 1], mae_results[1:11, 3], mae_results[1:11, 6])),
  Avg_without_PP = rowMeans(cbind(mae_results[1:11, 2], mae_results[1:11, 4], mae_results[1:11, 4]))
)

# Plot
# Plot
ggp <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = Avg_with_PP, color = "Models averages with PP"), size = 2) +
  geom_line(aes(y = Avg_without_PP, color = "Models averages without PP"), linetype = "dashed", size = 2) +
  scale_color_manual(values = c("Models averages with PP" = "blue", "Models averages without PP" = "red")) +
  labs(title = "Average Models Performance Comparison", x = "Year", y = "MAE") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_vline(xintercept = c(2013, 2018, 2020), color = "gray", size=2) +
  annotate("text", x = 2013, y = max(data$Avg_with_PP), label = "2013", vjust = -0.5, hjust = -0.5) +
  annotate("text", x = 2018, y = max(data$Avg_with_PP), label = "2018", vjust = -0.5, hjust = -0.5) +
  annotate("text", x = 2020, y = max(data$Avg_with_PP), label = "2020", vjust = -0.5, hjust = -0.5) 

ggp



directional_accuracy<-function(forecast,actual){sum(sign(diff(forecast)) == sign(diff(actual))) / (length(actual) - 1)}

DR=c(directional_accuracy(f_arima[1:11],test_target[1:11]),
     directional_accuracy(f_arima_NOPP[1:11],test_target[1:11]),
     directional_accuracy(p_QR[1:11],test_target[1:11]),
     directional_accuracy(p_QR_NOPP[1:11],test_target[1:11]),
     directional_accuracy(F_ingarch[1:11],test_target[1:11]),
     directional_accuracy(F_ingarch_NOPP[1:11],test_target[1:11]),
     directional_accuracy(unlist(f_QGBRT_PP[1:11]),test_target[1:11]),
     directional_accuracy(unlist(f_QGBRT_NOPP)[1:11],test_target[1:11]))

DR

########################################################################
# AVERAGE PINBALL LOSS
########################################################################


avg_pin=function(test_target, predicted_quantiles){

#output object
avg_pin=vector()
out=list()
#Pinball
pinball_m <- function(y, y_hat, tau) {
  loss <- ifelse(y_hat >= y, (y_hat - y) * tau, (y - y_hat) * (1 - tau))
  return(loss)
}

for(i in seq(11)){
  avg_pin[i]=mean(c(pinball_m(test_target[i], predicted_quantiles[i,1], 0.1),
                    pinball_m(test_target[i], predicted_quantiles[i,2], 0.2),
                    pinball_m(test_target[i], predicted_quantiles[i,3], 0.3),
                    pinball_m(test_target[i], predicted_quantiles[i,4], 0.4),
                    pinball_m(test_target[i], predicted_quantiles[i,5], 0.5),
                    pinball_m(test_target[i], predicted_quantiles[i,6], 0.6),
                    pinball_m(test_target[i], predicted_quantiles[i,7], 0.7),
                    pinball_m(test_target[i], predicted_quantiles[i,8], 0.8),
                    pinball_m(test_target[i], predicted_quantiles[i,9], 0.9)))
}
out[[1]]=mean(avg_pin)
out[[2]]=avg_pin
return(out)
}


# Compute the values
values <- c(
  avg_pin(test_target, Arima_quantiles_PP)[[1]],
  avg_pin(test_target, Arima_quantiles_NOPP)[[1]],
  avg_pin(test_target, Ingarch_quantiles_PP)[[1]],
  avg_pin(test_target, Ingarch_quantiles_NOPP)[[1]],
  avg_pin(test_target, p_mQR_matrix_PP)[[1]],
  avg_pin(test_target, p_mQR_matrix_NOPP)[[1]],
  avg_pin(test_target, QGBM_matrix_PP)[[1]],
  avg_pin(test_target, QGBM_matrix_NOPP)[[1]]
)

# Assign labels to the vector
names(values) <- c(
  "Arima_quantiles_PP",
  "Arima_quantiles_NOPP",
  "Ingarch_quantiles_PP",
  "Ingarch_quantiles_NOPP",
  "p_mQR_matrix_PP",
  "p_mQR_matrix_NOPP",
  "QGBM_matrix_PP",
  "QGBM<_matrix_NOPP",
)

# Print the vector with labels
print(values)

matplot(QGBM_matrix_PP, pch=19, col="red")
points(test_target, pch=19, col="blue")
avg_pin_QR=avg_pin(test_target,p_mQR_matrix_PP)[[2]]
avg_pin_arima=avg_pin(test_target,Arima_quantiles_PP)[[2]]
avg_pin_ingarch=avg_pin(test_target,Ingarch_quantiles_PP)[[2]]
years <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021,2022)


# Entering data 
year <- 2012:2022
# Creating Data Frame 
df <- data.frame(year, avg_pin_QR, p_mQR_matrix_PP[1:11,1:9],D$target[31:41]) 
# Plotting Charts and adding a secondary axis 
library(ggplot2) 


ggp <- ggplot(df) +  
  geom_bar(aes(x = year, y = avg_pin_QR), stat = "identity", fill = "cyan", colour = "#006000") + 
  geom_line(aes(x = year, y = X1, color = "0.1 predicted quantile"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X5, color = "Median"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X9, color = "0.9 predicted quantile"), stat = "identity", size = 2) +
  geom_point(aes(x = year, y = D.target.31.41., color = "Number of Hurricanes"), size = 2) +
  labs(title = "Avg Pinball loss function and quantile forecast of QR(PP)", 
       x = "Year", y = "Number of Hurricanes") +
  scale_color_manual(name = "Legend",  # Change the heading of the legend to "Legend"
                     values = c("red", "orange", "darkred", "blue"),
                     breaks = c("0.1 predicted quantile", "Median", "0.9 predicted quantile", "Number of Hurricanes"),
                     labels = c("X1 is the 0.1 predicted quantiles", "X5 is the median", "X9 is the 0.9 predicted quantiles", "Number of Hurricanes")) +
  scale_fill_manual(values = "cyan")

ggp


df <- data.frame(year, avg_pin_arima, Arima_quantiles_PP[1:11,1:9],D$target[31:41]) 
# Plotting Charts and adding a secondary axis 
ggp2 <- ggplot(df) +  
  geom_bar(aes(x = year, y = avg_pin_arima), stat = "identity", fill = "cyan", colour = "#006000") + 
  geom_line(aes(x = year, y = X1, color = "0.1 predicted quantile"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X5, color = "Median"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X9, color = "0.9 predicted quantile"), stat = "identity", size = 2) +
  geom_point(aes(x = year, y = D.target.31.41., color = "Number of Hurricanes"), size = 2) +
  labs(title = "Avg Pinball loss function and quantile forecast of Arima(PP)", 
       x = "Year", y = "Number of Hurricanes") +
  scale_color_manual(name = "Legend",  # Change the heading of the legend to "Legend"
                     values = c("red", "orange", "darkred", "blue"),
                     breaks = c("0.1 predicted quantile", "Median", "0.9 predicted quantile", "Number of Hurricanes"),
                     labels = c("X1 is the 0.1 predicted quantiles", "X5 is the median", "X9 is the 0.9 predicted quantiles", "Number of Hurricanes")) +
  scale_fill_manual(values = "cyan")

ggp2

df <- data.frame(year, avg_pin_ingarch, Ingarch_quantiles_PP[1:11,1:9],D$target[31:41]) 
# Plotting Charts and adding a secondary axis 
ggp3 <- ggplot(df) +  
  geom_bar(aes(x = year, y = avg_pin_ingarch), stat = "identity", fill = "cyan", colour = "#006000") + 
  geom_line(aes(x = year, y = X1, color = "0.1 predicted quantile"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X5, color = "Median"), stat = "identity", size = 2) + 
  geom_line(aes(x = year, y = X9, color = "0.9 predicted quantile"), stat = "identity", size = 2) +
  geom_point(aes(x = year, y = D.target.31.41., color = "Number of Hurricanes"), size = 2) +
  labs(title = "Avg Pinball loss function and quantile forecast of Ingarch(PP)", 
       x = "Year", y = "Number of Hurricanes") +
  scale_color_manual(name = "Legend",  # Change the heading of the legend to "Legend"
                     values = c("red", "orange", "darkred", "blue"),
                     breaks = c("0.1 predicted quantile", "Median", "0.9 predicted quantile", "Number of Hurricanes"),
                     labels = c("X1 is the 0.1 predicted quantiles", "X5 is the median", "X9 is the 0.9 predicted quantiles", "Number of Hurricanes")) +
  scale_fill_manual(values = "cyan")

library(gridExtra)
grid.arrange(ggp,ggp2,ggp3)
grid.arrange(ggp,ggp2)


# Load the ggplot2 package
library(ggplot2)
library(tidyr)

test_year=2020
train_data <- D[D$YYYY >= start_year & D$YYYY < test_year, ]
test_data <- D[D$YYYY == test_year, ]
xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1","PPt")]
formula_t <- reformulate(c(names(xreg)), "target")
# Quantile Regression
tau <- seq(0.05,0.95,0.01) # Quantile level (e.g., median)
quantile_model <- rq(formula_t, tau = tau, data = train_data)
Q=predict(quantile_model, newdata = xreg_f)
Q
#
xreg <- train_data[, c( "NINA1_std_t_1","z500_std_t_1")]
xreg_f <- test_data[last, c( "NINA1_std_t_1","z500_std_t_1")]
formula_t <- reformulate(c(names(xreg)), "target")
# Quantile Regression
tau <- seq(0.05,0.95,0.01) # Quantile level (e.g., median)
quantile_model <- rq(formula_t, tau = tau, data = train_data)
library(tscount)
## compute and plot pithist
pit(quantile_model)
Q2=predict(quantile_model, newdata = xreg_f)
d=cbind(as.numeric(Q),as.numeric(Q2))

# Load necessary libraries
library(ggplot2)
library(tidyr)
# quantile-based pit
# Assume d is already created with cbind(Q, Q2)
# Convert matrix to data frame for easier handling in ggplot
d <- as.data.frame(d)
names(d) <- c("Q", "Q2")

# Reshape data to long format
d_long <- pivot_longer(d, cols = c(Q, Q2), names_to = "variable", values_to = "value")

# Create the histogram plot
p <- ggplot(d_long, aes(x = value)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  ggtitle("Histograms of the QR+PP predicted quantiles for 2020,") +
  theme_minimal()

# Print the plot
print(p)
############################
#
##########################


test_target <- D[D$YYYY >= 2012 & D$YYYY <= 2022, ]
test_target=test_target$target

# Install plotly if it's not already installed
if (!require(plotly)) install.packages("plotly", dependencies=TRUE)

# Load the plotly package
library(plotly)

# Generate some data
set.seed(42)  # For reproducibility
x <- test_target$NINA1_std_t_1
y <- test_target$z500_std_t_1
z <- test_target$target



# Create a 3D scatter plo
library("scatterplot3d") # load
df=cbind(x,y,z)
# Create a 3D scatter plot
scatterplot3d(df, pch = 16, color="steelblue")
plot(x,z,pch=19, color="stealblue")
plot(y,z,pch=19, color="stealblue")
plot(x,z,pch=19, color="stealblue")
plot(test_target$PPt,z,pch=19, color="stealblue")
cor(train_data$NINA1_std_t_1,train_data$target)
cor(train_data$z500_std_t_1,train_data$target)
cor(z,test_target$PPt)
