# Load packages 
library(tsModel) 
library(tidyverse) 
library(sandwich) 
library(dplyr) 
library(qcc)
library(MASS)

# Centre time
list2$time_c <- list2$n - 37 # Time centered at the first month of COVID-19
list2$time_e <- list2$n - 48 # Time centered at the end of 2020

# Overdispersion test 
qcc.overdispersion.test(list2$x)

# Negative binominal segmented regression model of HIV cases
model_time <- glm.nb(x ~ lockdown + time_c + lockdown:time_c + harmonic(month, 2, 12), data = list2)
summary(model_time)

# Calculated changes in HIV cases at the end of 2020
model_time_end <- glm.nb(x ~ lockdown + time_e + lockdown:time_e + harmonic(month, 2, 12), data = list2)
summary(model_time_end)

# Look at residuals for autocorrelation 
acf(residuals(model_time, type = "deviance")) 
pacf(residuals(model_time, type = "deviance")) 
acf(residuals(model_time_end, type = "deviance")) 
pacf(residuals(model_time_end, type = "deviance"))

# Calculate Newey-West standard errors, with the lag calculated through the automatic bandwidth selection procedure
est <- exp(c(coef(model_time)["lockdown:time_c"], coef(model_time)["lockdown"], coef(model_time)["time_c"])) 
se1 <- sqrt(diag(NeweyWest(model_time, prewhite = F)))["lockdown:time_c"]
se2 <- sqrt(diag(NeweyWest(model_time, prewhite = F)))["lockdown"]
se3 <- sqrt(diag(NeweyWest(model_time, prewhite = F)))["time_c"] 
lb <- est * exp(-1.96 * c(se1, se2, se3))
ub <- est * exp(1.96 * c(se1, se2, se3))
table <- cbind(round(est, digits = 3), round(lb, digits = 3), round(ub, digits = 3)) 
table
