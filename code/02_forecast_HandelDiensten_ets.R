library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)
library(dlm)
library(forecast)
library(expsmooth)
library(ggplot2)
#library(ggfortify)
library(changepoint)
library(KFAS)
library(httpgd)
library(funtimes)
library(seastests)
library(car)
library(lmtest)

# Time series plots
# https://cran.rstudio.com/web/packages/ggfortify/vignettes/plot_ts.html


# Model consists of three letters following Hyndman (2008) and here: https://search.r-project.org/CRAN/refmans/forecast/html/ets.html

# First letter is the error type:     A, M or Z
# Second letter is the trend type:    N, A, M, Z
# Third letter is the season type:    N, A, M, Z

# Some of the models have names:
#     ANN is simple exponential smoothing with additive errors.
#     MAM is multiplicative Holt-Winters with multiplicative errors.


##########################
# First smooth, if data is missing from the original "y" series, replace it
# Only take values from 2005 Q1 to 2024Q1

# load data
dt1 <- read.csv("data/HandelDiensten_raw.csv", sep = ",")
colnames(dt1)

series1 <- ts(dt1["Winkels.in.meubels..woninginrichting.alg"], frequency = 12, start=c(2000,1))
series1 <- ts(dt1["X4791.Postorderbedrijven..webwinkels"], frequency = 12, start=c(2000,1))
# series1 <- ts(dt1['Winkels.in.meubels..woninginrichting.alg'], frequency = 12, start=c(2000,1))

plot(series1)

#########################
# Which model to use
#########################

###########################
# Trend or not?
###########################

result <- try(notrend_test(series1)$p.value)
print(result)

if (is.numeric(result) == FALSE) {
  print("assume: Has Trend")
  trendtype <- "A"
} else {
     if (p_value > 0.05) {
      print("Has Trend")
      trendtype <- "A"
    } else {
      print("No Trend")
      trendtype <- "N"
      }
}

print(trendtype)

###########################
# Additive or multiplictive?
###########################

decompose_series1 <- decompose(series1, "multiplicative")
decompose_series1_multiplicative <- decompose_series1$random
muladd_mul <- sqrt(mean(abs(decompose_series1_multiplicative)^2, na.rm=TRUE))

decompose_series1 <- decompose(series1, "additive")
decompose_series1_additive <- decompose_series1$random
muladd_add <- sqrt(mean(abs(decompose_series1_additive)^2, na.rm=TRUE))

if (muladd_mul < muladd_add) {
  print("Use Multiplicative")
  errortype <- "M"
  } else {
    print("Use Additive")
    errortype <- "A"
}

print(errortype)

###########################
# Seasonnal or not
###########################

season_Check <- isSeasonal(series1)

if (season_Check == TRUE) {
  print("Use Seasonal")
  seasontype <- "A"
  } else {
    print ("Use Non-Seasonal")
    seasontype <- "N"
  }

print(seasontype)

# error, trend, season

# First letter is the error type:     A, M or Z
# Second letter is the trend type:    N, A, M, Z
# Third letter is the season type:    N, A, M, Z

modelform <- str_c(c(errortype, trendtype, seasontype), collapse = "")

paste("x","y",sep="")

fit <- ets(series1, model=modelform, damped=FALSE)

h1 <- 12
train <- head(series1, round(length(series1) - h1))
test <- tail(series1, h1)

fit <- ets(train, model="MAM", damped=FALSE)
forecasted1 <- forecast(fit, h=h1)

autoplot(forecasted1, include=h1+24) + autolayer(test)

