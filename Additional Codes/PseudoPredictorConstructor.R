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
cor(DIS[1:40],D$target[1:40])
#sDIS <- scale(DIS[1:42])

# Forecast using pseudo-P
Pseudo_P <- vector()

#note that the Pseudo-Predictor uses lag information  
for(i in seq(11)){
  a <- auto.arima(scale(D$target[2:29+i]), xreg = DIS[1:28+i] )
  f <- forecast(a, h = 1, xreg = DIS[(29+i)])
  Pseudo_P[i] <- f$mean
}

cor(Pseudo_P,D$target[30:40])

directional_accuracy<-function(forecast,actual){sum(sign(diff(forecast)) == sign(diff(actual))) / (length(actual) - 1)}
directional_accuracy(Pseudo_P,D$target[31:41])
# Plot the data

round(mean(abs(DIS)),4)

sH=scale(D$target[1:42])
sESPI=scale(D$ESPI.x)
cor(sH[1:41], sESPI)
# Plot the data
plot(D$year[1:42], sH, pch = 19, col = 2, xlab = "Year", ylab = "Scaled Value", main = "Annual Hurricanes occurence and ESPI index")
points(D$year,sESPI , pch = 19, col = 4)
x2020=c(2019,2020)
y2020=c(sH[38],sH[39])
segments(x2020[1], y2020[1], x2020[2], y2020[2], col= 'red')
x2013=c(2012,2013)
y2013=c(sH[31],sH[32])
segments(x2013[1], y2013[1], x2013[2], y2013[2], col= 'red')
x1997=c(1996,1997)
y1997=c(sH[15],sH[16])
segments(x1997[1], y1997[1], x1997[2], y1997[2], col= 'red')

# Add vertical lines
abline(v = c(1997, 2013, 2020), col = "gray", lty = 2)

x1996=c(1996,1996)
y1996=c(sH[15],sESPI[15])
segments(x1996[1], y1996[1], x1996[2], y1996[2], col= 'blue', lwd=2)

x2012=c(2012,2012)
y2012=c(sH[31],sESPI[31])
segments(x2012[1], y2012[1], x2012[2], y2012[2], col= 'blue', lwd=2)

x2019=c(2019,2019)
y2019=c(sH[38],sESPI[38])
segments(x2019[1], y2019[1], x2019[2], y2019[2], col= 'blue', lwd=2)

# Add legend
legend("topleft", legend = c("Target", "ESPI"), pch = 19, col = c(2, 4))

library(ggplot2)
library(dplyr)

# Data preparation
D <- data.frame(
  year = as.numeric(D$year[1:42]),  # Assuming year data is present and correctly formatted
  sH = scale(D$target[1:42]),
  sESPI = scale(D$ESPI.x[1:42])
)

# Adding segments data for red and blue lines
red_segments <- data.frame(
  xstart = c(2019, 2012, 1996),
  xend = c(2020, 2013, 1997),
  ystart = c(D$sH[38], D$sH[31], D$sH[15]),
  yend = c(D$sH[39], D$sH[32], D$sH[16])
)

blue_segments <- data.frame(
  x = c(1996, 2012, 2019),
  ystart = c(D$sH[15], D$sH[31], D$sH[38]),
  yend = c(D$sESPI[15], D$sESPI[31], D$sESPI[38])
)

# Creating the plot
p <- ggplot(D, aes(x = year)) +
  geom_point(aes(y = sH, color = "Scaled Hurricane Count"), size = 3) +
  geom_point(aes(y = sESPI, color = "ESPI"), size = 3) +
  geom_segment(data = red_segments, aes(x = xstart, xend = xend, y = ystart, yend = yend), color = "red") +
  geom_segment(data = blue_segments, aes(x = x, xend = x, y = ystart, yend = yend), color = "blue", size = 1.2) +
  scale_color_manual(values = c("Scaled Hurricane Count" = "red", "ESPI" = "blue")) +
  geom_vline(xintercept = c(1997, 2013, 2020), size=2, linetype = "dashed", color = "gray") +
  labs(title = "Annual Hurricanes Occurrence and ESPI Index",
       x = "Year",
       y = "Scaled Value",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plot
print(p)

