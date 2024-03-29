convenient_qreg_gbm <- function(input, case,xreg) {
  result <- switch(case,
                   
                   "Remainder" = {
                     formula <- reformulate(c(names(xreg)), response="stl_r")
                     qreg_gbm(data = train_data,
                              formula = formula,
                              interaction.depth = 2,
                              n.trees = 1000,
                              n.minobsinnode = 2,
                              shrinkage = 0.5,
                              bag.fraction = 0.5,
                              quantiles = c(0.5),
                              cores = detectCores())
                   },
                   "Trend" = {
                     
                     qreg_gbm(data = input,
                              formula = reformulate(c(names(xreg)), "stl_t"),
                              interaction.depth = 2,
                              n.trees = 1000,
                              n.minobsinnode = 2,
                              shrinkage = 0.5,
                              bag.fraction = 0.5,
                              quantiles = seq(0.1, 0.90, by = 0.1),
                              cores = detectCores())
                   },
                   "Seasonal" = {
                     qreg_gbm(data = input,
                              formula = reformulate(c(names(xreg)), "stl_s"),
                              interaction.depth = 2,
                              n.trees = 1000,
                              n.minobsinnode = 2,
                              shrinkage = 0.5,
                              bag.fraction = 0.5,
                              quantiles = seq(0.1, 0.90, by = 0.1),
                              cores = detectCores())
                   },
                   "Data" = {
                     formula <- reformulate(c(names(xreg)), "target")
                     gbm_mqr <- qreg_gbm(data = input,
                                         formula = formula,
                                         interaction.depth = 2,
                                         n.trees = 1000,
                                         n.minobsinnode = 2,
                                         shrinkage = 0.5,
                                         bag.fraction = 0.5,
                                         quantiles =0.5,
                                         cores = detectCores())
                   }
  )
  return(result)
}

