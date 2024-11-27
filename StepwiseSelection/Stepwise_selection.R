library(readr)
D_used <- read_csv("D_used.csv")
#The D_used contains also the seasonal and trend decomposition, of the hurricane series (using stl, moving averages)
#we removed these components.
D_f<- D_used %>% select(-remainder, -trend, -stl_s, -seasonal, -stl_t)



# The goal is to identify the best predictors for forecasting the residuals 
# of the hurricane time series after removing the trend and seasonal components.
# Specifically, we aim to determine if our predictors provide information beyond 
# what can be captured by the trend and seasonal components, which are generally 
# easier to predict.

# To achieve this, we set the residuals from the seasonal and trend decomposition 
# as the target variable for stepwise selection. This allows us to focus on 
# predictors that contribute unique information to the unexplained variations
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
library(tidyverse)

D_f<- D_used %>% select(-remainder, -trend, -stl_s, -seasonal, -stl_t)
forward_select(D_f, "stl_r")
