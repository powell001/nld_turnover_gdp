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

# load data
dt1 <- read.csv("data/HandelDiensten_raw.csv", sep = ",")
colnames(dt1)

colName <- "X45.Autohandel.en..reparatie"

# connects all the data
Key1 <- paste(Sys.Date(), "_", colName, sep="")

series1 <- ts(dt1[colName], frequency = 12, start=c(2000,1))
series1 <- na.omit(series1)


#########################
# Which model to use
#########################

###########################
# Trend or not?
###########################

p_value <- try(notrend_test(series1)$p.value)
print(p_value)

if (is.numeric(p_value) == FALSE) {
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

fit <- ets(series1, model=modelform, damped=FALSE)

h1 <- 12
train <- head(series1, round(length(series1) - h1))
test <- tail(series1, h1)

fit <- ets(train, model=modelform, damped=FALSE)
forecasted1 <- forecast(fit, h=h1)

png(filename=paste("output/figures/", Key1, "TrainTestForecast.png", sep = "_"))
autoplot(forecasted1, include=h1+24) + autolayer(test)
dev.off()

####################
# final forecast
####################
fit <- ets(series1, model=modelform, damped=FALSE)
forecast_oneMonth <- forecast(fit, h=1)

png(filename=paste("output/figures/", Key1, "final_forecasts.png", sep = "_"))
autoplot(tail(series1, 36)) + autolayer(forecast_oneMonth) + ggtitle(colName)
dev.off()

################################
# Saving
################################

###
# Raw Data
###

data <- data.frame(
  SeriesName   = colName, 
  DateAnalysis = Sys.Date(), 
  ETSmodel = modelform,
  ObservationDate = as.yearmon(time(series1)),
  RawData = series1
)
data$Key1 <- Key1
write.table(data, file = paste("output/forecasts/", Key1, "RawData.csv", sep="_"), sep =",",row.names = FALSE)

###
# TrainTestForecast
###
forecast_tibble <- as.data.frame(forecasted1)
forecast_tibble$Key1 <- Key1 

write.table(forecast_tibble, file = paste("output/forecasts/", Key1, "TrainTestForecast.csv", sep="_"), sep =",",row.names = FALSE)

###
# finalForecast
###
forecast_oneMonth <- forecast(fit, h=1)
finalForecast <- as.data.frame(forecast_oneMonth, row.names = NULL)
finalForecast$Key1 <- Key1
finalForecast <- tibble::rownames_to_column(finalForecast, "Forecast_Period")  

write.table(finalForecast, file = paste("output/forecasts/", Key1, "final_forecasts.csv", sep="_"), sep =",",row.names = FALSE)
