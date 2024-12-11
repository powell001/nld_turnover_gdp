library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)

##########################
# First smooth, if data is missing from the original "y" series, replace it
# Only take values from 2005 Q1 to 2024Q1

# load data
dt1 <- read.csv("data/allDataOmzet.csv", sep = ";")

##### Start at 2005-01-01, replace missing values with best guess
dt2 <- dt1[-c(1:20), -c(1)]

# number columns
numColumns <- dim(dt2)[2]
numRows <- dim(dt2[1])
colNames <- colnames(dt2)

emptyDF = data.frame(matrix(NA, nrow = numRows, ncol = numColumns))
colnames(emptyDF) <- colnames(dt2)

for (i in 1:numColumns) {
    print(i)
    y <- dt2[, i]

    # ######################################
    # # Kalman smoothing
    # ######################################

    # Setting of local-level model
    W <- 1
    V <- 2
    m0 <- 10
    C0 <- 9
    mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)

    dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

    # Find the mean and standard deviation of the smoothing distribution
    s <- dropFirst(dlmSmoothed_obj$s)

    # which of the original data is missing, for example, sector: 11_Drankenindustrie
    replaceThese <- which(is.na(y))
    y[replaceThese] <- s[replaceThese]

    emptyDF[,i] <- y
}    

write.csv(emptyDF, "tmp.csv")

######################################
## Local-trend model
######################################

localTrend <- emptyDF

## Seasonal model
### Approach from the time domain
### Approach from the frequency domain
#### Local-trend model + seasonal model (time-domain approach)

# Preprocessing
library(dlm)

####
# Save forecasts
####
forecasts <- c()
namesCols <- c()

for (j in 0:18){
  png(paste("output/Kalman2/plot_", j, ".png", sep = ""))
  par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

  begin <- 1 + (j * 4)
  end   <- 4 + (j * 4)

  if (begin <= 70) {

    for (i in begin:end){
      print(colNames[i])      
      mymain <- colNames[i]

      # Data
      y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(2005, 1))
      # if (colNames[i] == "X51_Vervoer_door_de_lucht"){
      #   print(y)
      #   print(!is.na(as.numeric(y)))
      # }

      # Model setting: local-trend model + seasonal model (time-domain approach)
      build_dlm_test <- function(par) {
        return(
          dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
          dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 1)
        )
      }

      # Maximum likelihood estimation of parameters and confirmation of the results
      fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
    
      # Set the maximum likelihood estimates of parameters in the model
      mod  <- build_dlm_test(fit_dlm_test$par)

      # # Kalman filtering
      dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
      dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

      ######################################
      # Kalman prediction
      ######################################

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 8)
      forecasts[i] <- as.data.frame(dlmForecasted_object$f)

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

    for (i in 71:75){

      print(colNames[i])
      if (length(na.omit(localTrend[, i])) < 20) next
      mymain <- colNames[i]

      # Data
      y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(2005, 1))

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

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 4)
      forecasts[i] <-  as.data.frame(dlmForecasted_object$f)

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


######
# combine data create date
######

newforecastDF <-  as.data.frame(do.call(cbind, forecasts))
colnames(newforecastDF) <- colnames(dt2)
allData = rbind(emptyDF, newforecastDF)

quarters <- seq(from = as.Date("2005/01/01"), to = as.Date("2026/07/01"), by ="quarter")

allData['Date'] <- quarters
head(allData)

allData1 <- allData %>% select("Date", everything())

allDataYearly <- apply.yearly(allData1, FUN = colMeans)
allDataYearly
   
prtchangeDF <- sapply(allDataYearly, function(x) (x - lag(x))/lag(x) * 100)
prtchangeDF <- as.data.frame(prtchangeDF)
prtchangeDF['Date'] <- seq(from = as.Date("2005/01/01"), to = as.Date("2026/01/01"), by ="year")

prtchangeDF <- prtchangeDF %>% select("Date", everything())

write.csv(emptyDF, "emptyDF.csv")
write.csv(allDataYearly, "allDataYearly.csv")


colnames(prtchangeDF) <- gsub("X", "", colnames(prtchangeDF))
colnames(prtchangeDF) <- gsub("_", " ", colnames(prtchangeDF))
colnames(prtchangeDF) <- gsub("\\.", " ", colnames(prtchangeDF))
prtchangeDF <- format(prtchangeDF, digits = 2)
write.csv(prtchangeDF, "prtchangeDF.csv")




