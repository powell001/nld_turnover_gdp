library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)

# load data
dt1 <- read.csv("data/allDataOmzet.csv", sep=";")

##### Start at 2005-01-01
dt1 <- dt1[-(1:21), ]

# remove first (date) column
dt1 <- dt1[, -1]
head(dt1)

# Number of columns
columns1 <- dim(dt1)

# Columns names
colnm <- colnames(dt1)

# for loop to produce basic figures put 4 figures on a page
# can be modified to account for different sized dataframes, 
# for now I just calculate by hand how many figures to produce
for (j in 0:18){
  png(paste("output/basic/plot_", j, ".png", sep = ""))
  par(mfrow = c(3, 1), mar = c(3, 1, 1, 3))

  begin <- 1 + (j * 3)
  end   <- 3 + (j * 3)

  if (begin <= 70) {

    for (i in begin:end){

      # some columns missing data  
      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(2000, 1)), main = mymain,  width=12, height=4)
    }
    dev.off()
  } else {

    # this is a repeat of the above code, but for two coluns only
    for (i in 71:72){

      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(2000, 1)), main = mymain,  width=12, height=4)
    }
    dev.off()
  }
}

##########################
# HW #####################
##########################

mygray <- "#80808080"

for (j in 0:18){
  png(paste("output/HW/plot_", j, ".png", sep = ""))
  par(mfrow = c(3, 1), mar = c(3, 1, 1, 3))

  begin <- 1 + (j * 3)
  end   <- 3 + (j * 3)

  if (begin <= 70) {

    for (i in begin:end){
      print(i)
      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      test_data <- ts(na.omit(dt1[, i]), frequency = 4, start = c(2005, 1))

      HW_test <- HoltWinters(test_data)
      str(HW_test)

      HW_test_predict <- predict(HW_test, n.ahead = 4)
      str(HW_test_predict)

      # Plot observations along with filtering and prediction values
      plot(HW_test, HW_test_predict, main = mymain,  sub = "HW", width=12, height=4, col = mygray, 
         col.predicted = "red", lty.predicted = "dashed")
    }
    dev.off()
  } else {

    for (i in 71:72){

      print(i)
      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      test_data <- ts(na.omit(dt1[, i]), frequency = 4, start = c(2005, 1))

      HW_test <- HoltWinters(test_data)
      str(HW_test)

      HW_test_predict <- predict(HW_test, n.ahead = 4)
      str(HW_test_predict)

      # Plot observations along with filtering and prediction values
      plot(HW_test, HW_test_predict, main = mymain,  sub = "HW", width=12, height=4, col = mygray, 
         col.predicted = "red", lty.predicted = "dashed")      
    }
    dev.off()
  }
}

##########################
# State Space Kalman #####
##########################

######################################
## Local-trend model
######################################

## Seasonal model
### Approach from the time domain
### Approach from the frequency domain
#### Local-trend model + seasonal model (time-domain approach)

# Preprocessing
library(dlm)

for (j in 0:18){
  png(paste("output/Kalman2/plot_", j, ".png", sep = ""))
  par(mfrow = c(3, 1), mar = c(3, 1, 1, 3))

  begin <- 1 + (j * 3)
  end   <- 3 + (j * 3)

  if (begin <= 70) {

    for (i in begin:end){
      print(i)
      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      # Data
      y <- ts(na.omit(dt1[, i]), frequency = 4, start = c(2005, 1))

      # Model setting: local-trend model + seasonal model (time-domain approach)
      build_dlm_test <- function(par) {
        return(
          dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
          dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
        )
      }

      # Maximum likelihood estimation of parameters and confirmation of the results
      fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
      fit_dlm_test

      # Set the maximum likelihood estimates of parameters in the model
      mod  <- build_dlm_test(fit_dlm_test$par)

      # # Kalman filtering
      dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
      dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

      ######################################
      # Kalman prediction
      ######################################

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 8)

      # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
      f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
      f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
      f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

      # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
      y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

      # Plot results
      plot(y_union, plot.type = "single",
          xlim = c(2005, 2026),
          ylim = c(50,  240), ylab = "Year-Quarter", 
          lty = c("solid", "solid", "dashed", "dashed"),
          col = c("lightgray", "red", "black", "black"),
          main = mymain)

      # Legend
      legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
            lty = c("solid", "solid", "dashed"),
            col = c("lightgray", "black", "black"),
            x = "topleft", cex = 0.6)

      # Grid
      grid(nx = NULL, ny = NULL,
          lty = 2,      # Grid line type
          col = "gray", # Grid line color
          lwd = 1)      # Grid line width
    }

    dev.off()
  
  } else {

    for (i in 71:72){

      print(i)
      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      # Data
      y <- ts(na.omit(dt1[, i]), frequency = 4, start = c(2005, 1))

      # Model setting: local-trend model + seasonal model (time-domain approach)
      build_dlm_test <- function(par) {
        return(
          dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
          dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
        )
      }

      # Maximum likelihood estimation of parameters and confirmation of the results
      fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
      fit_dlm_test

      # Set the maximum likelihood estimates of parameters in the model
      mod  <- build_dlm_test(fit_dlm_test$par)

      # # Kalman filtering
      dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
      dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

      ######################################
      # Kalman prediction
      ######################################

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 8)

      # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
      f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
      f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
      f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

      # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
      y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

      # Ignore the display of following codes

      # Plot results
      plot(y_union, plot.type = "single",
          xlim = c(2005, 2026),
          ylim = c(50,  240), ylab = "Year-Quarter", 
          lty = c("solid", "solid", "dashed", "dashed"),
          col = c("lightgray", "red", "black", "black"),
          main = mymain)

      # Legend
      legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
            lty = c("solid", "solid", "dashed"),
            col = c("lightgray", "red", "black"),
            x = "topleft", cex = 0.6)

      # Grid
      grid(nx = NULL, ny = NULL,
          lty = 2,      # Grid line type
          col = "gray", # Grid line color
          lwd = 1)      # Grid line width

    }
    dev.off()
  }
}


