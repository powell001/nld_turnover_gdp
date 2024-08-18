library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)

# load data
dt1 <- read.csv("data/allDataOmzet.csv", sep=";")

st <- as.yearqtr("2000-01-01")
en <- as.yearqtr("2024-02-01")

dt1 <- dt1[, -1]
head(dt1)

# Number of columns
columns1 <- dim(dt1)

# Columns names
colnm <- colnames(dt1)

for (j in 0:18){
  png(paste("output/basic/plot_", j, ".png", sep = ""))
  par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

  begin <- 1 + (j * 4)
  end   <- 4 + (j * 4)

  if (begin <= 70) {

    for (i in begin:end){

      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(2000, 1)), main = mymain)
    }
    dev.off()
  } else {

    for (i in 71:72){

      if (length(na.omit(dt1[, i])) < 20) next
      mymain <- colnm[i]

      plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(2000, 1)), main = mymain)
    }
    dev.off()
  }
}



# par(mfrow=c(5,1))
# sapply(1:5, function(i) plot(ts1[,i]))
# dev.off()

# ###
# # Select column
# ###
# for (i in clns){

#   colNme <- i
  
#   # plot parameters
#   oldpar <- par(no.readonly = TRUE)

#   svglite(paste("figures/", as.character(colNme), "Basic.svg", sep=""), width=8, height=6)
#   par(mfrow = c(2,2), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

#   ##############
#   # negative numbers in change_supply_season
#   if ("change_supply_season" != colNme) {
#     series <- ts(na.omit(dt1[[colNme]]), frequency = 12, start = c(1981, 1)) ### removed log
#    } else {
#     series <- ts(na.omit(dt1[[colNme]]), frequency = 12, start = c(1981, 1))
#    }

#   ##############

#   ###
#   # level
#   ###
#   plot(series)
#   title(sub="(a)", line = 4, family = "mono")

#   ###
#   # hist
#   ###
#   hist(series)
#   title(sub="(b)", line = 4, family = "mono")

#   ###
#   # acf
#   ###
#   acf(series)
#   title(sub="(c)", line = 4, family = "mono")

#   ###
#   # fourier
#   ###
#   plot.spectrum <- function(dat, lab = "", sub = "",
#                             y_max = 1, tick = c(8, 4), unit = 1){
#     # Frequency domain transform of data
#     dat_FFT <- abs(fft(as.vector(dat)))

#     # Preparation for display setting about horizontal axis (frequency)
#     data_len  <- length(dat_FFT)
#     freq_tick <- c(data_len, tick, 2)

#     # Plot data in the frequency domain
#     plot(dat_FFT/max(dat_FFT), type = "l", main = "",
#         ylab = "|Standardized frequency spectrum|", ylim = c(0, y_max),
#         xlab = sprintf("Frequency (1/%s)", lab), xlim = c(1, data_len/2), xaxt = "n")
#     title(sub = sub, line = 4, family = "mono")
#     axis(side = 1, at = data_len/freq_tick * unit + 1, 
#         labels = sprintf("1/%d", freq_tick), cex.axis = 0.7)
#   }

#   plot.spectrum(series, lab ="month", sub = "(d)", y_max = .005, unit = 3)

#   dev.off()

#   ###
#   # Holt-Winters
#   ###

#   svglite(paste("figures/", as.character(colNme), "HW.svg", sep=""), width=8, height=6)
#   par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

#   HW <- HoltWinters(series)
#   par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 3.5, 2, 1), mgp = c(2.5, 1, 0))
#   mygray <- "#80808080"

#   plot(HW, main = "", col = mygray, col.predicted = "black",
#       lty.predicted = "dashed")
#   title(sub = "(a)", line = 4, family = "mono")

#   par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 3.5, 2, 1), mgp = c(2.5, 1, 0))
#   mygray <- "#80808080"

#   HW_out <- HW
#   HW_decomp <- ts.union(Original = HW_out$x,
#                     Level = HW_out$fitted[,"level"] + HW_out$fitted[,"trend"], 
#                     Season = HW_out$fitted[, "season"],
#                 Resisuals = residuals(HW_out))
#   plot(HW_decomp, main = "", cex.lab = 1.3, cex.axis = 1.5, mar = c(0, 5, 1, 1))

#   dev.off()


#   ###
#   # HW Forecast
#   ####

#   svglite(paste("figures/", as.character(colNme), "_HW_Forecast.svg", sep=""), width=8, height=6)
#   par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

#   series_short <- window(series, start=c(1995,1), end=c(2024,1))
#   str(series_short)
#   HW_short <- HoltWinters(series_short)
#   str(HW_short)

#   HW_short_predict <- predict(HW_short, n.ahead = 2)
#   str(HW_short_predict)

#   # Plot observations along with filtering and prediction values
#   plot(HW_short, HW_short_predict, main = as.character(colNme), sub = "Filtering and prediction with Holt-Winters method",
#       col = mygray, col.predicted = "red", lty.predicted = "dashed")

#   # # Plot observations in 2023 as well
#   # series_2023 <- window(series, start = c(2023,1))
#   # lines(series_2023, col = mygray)

#   dev.off()

# }


# ######################################################
# ######################################################
# ######################################################

# # User-defined function for mean absolute percentage error (MAPE)
# MAPE <- function(true, pred){
#   mean(abs(pred - true) / true)


# ######################
# # Read in data
# ######################

# dt1 <- read.csv("data/EU27_NLD_GDP.csv")
# eu27 <- ts(data=dt1['Euro27'], start = c(1995,1), frequency = 4)
# nl <- ts(data=dt1['NL'], start = c(1995,1), frequency = 4)

# dt2 <- read.csv("data/a0_combinedQuarterly.csv")
# nl_exp <- ts(data=dt2['exports_goods_services_season'], start = c(1995,1), frequency = 4)
# nl_invest <- ts(data=drop_na(dt2['gpd_invest_business_households_season']), start = c(1995,1), frequency = 4)

# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(2,2), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

# plot(log(eu27))
# title(sub="(a)", line = 4, family = "mono")
# plot(log(nl))
# title(sub="(b)", line = 4, family = "mono")
# plot(log(nl_exp))
# title(sub="(c)", line = 4, family = "mono")
# plot(log(nl_invest))
# title(sub="(d)", line = 4, family = "mono")

# par(oldpar)
# plot(nl)

# ########################
# # Hist
# ########################

# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(2,2), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

# hist(log(eu27))
# title(sub="(a)", line = 4, family = "mono")
# hist(log(nl))
# title(sub="(b)", line = 4, family = "mono")
# hist(log(nl_exp))
# title(sub="(c)", line = 4, family = "mono")
# hist(log(nl_invest))
# title(sub="(d)", line = 4, family = "mono")

# par(oldpar)
 
# ########################
# # ACF
# ########################

# oldpar <- par(no.readonly = TRUE)

# par(mfrow = c(2,2), oma = c(0,0,0,0), mar = c(5,3.5,2,1), mgp = c(2.5,1,0))

# acf(log(eu27))
# title(sub="(a)", line = 4, family = "mono")
# acf(log(nl))
# title(sub="(b)", line = 4, family = "mono")
# acf(log(nl_exp))
# title(sub="(c)", line = 4, family = "mono")
# acf(log(nl_invest))
# title(sub="(d)", line = 4, family = "mono")

# par(oldpar)
 

# ########################
# # Fourier
# ########################

# oldpar <- par(no.readonly = TRUE)
# split.screen(rbind(c(0, 0.6, 0.75, 1   ),
#                    c(0, 0.6, 0.50, 0.75),
#                    c(0, 0.6, 0.25, 0.50),
#                    c(0, 0.6, 0   , 0.25)))

# # Tick values on the x-axis
# x_tick <- seq(from = 0, to = 2*pi, length.out = 1000)

# # Function drawing rectangular signal
# draw_rectangle <- function(){
#   lines(x = c(   0,    0), y = c( 0,  1))
#   lines(x = c(   0, 1*pi), y = c( 1,  1))
#   lines(x = c(1*pi, 1*pi), y = c( 1, -1))
#   lines(x = c(1*pi, 2*pi), y = c(-1, -1))
#   lines(x = c(2*pi, 2*pi), y = c(-1,  0))
# }

# # Fourier series expansion
# Fourie_series <- 0
# for (n in c(1, 3, 5, 7)){
#   screen((n+1)/2); par(oma = c(0, 0, 0, 0)); par(mar = c(1, 0, 0, 0))
  
#   Fourie_series <- Fourie_series + 4/pi * sin(n * x_tick) / n
#   plot(x = x_tick, y = Fourie_series, ylim = c(-1.3, 1.3), 
#        type = "l", lty = "dashed", ann = FALSE, xaxt="n")
#   draw_rectangle()
#   axis(side = 1, at = 0:6, labels = FALSE, tcl = -0.2)
#   mtext(text = 0:6, at = 0:6, side = 1, line = 0, cex = 0.7)
#   legend("topright", legend = sprintf("Sum of %d sin()s", ceiling(n/2)), cex = 0.6)
# }

# # Post-processing regarding plot
# close.screen(all = TRUE)
# par(oldpar)

# plot.spectrum <- function(dat, lab = "", sub = "",
#                           y_max = 1, tick = c(8, 4), unit = 1){
#   # Frequency domain transform of data
#   dat_FFT <- abs(fft(as.vector(dat)))

#   # Preparation for display setting about horizontal axis (frequency)
#   data_len  <- length(dat_FFT)
#   freq_tick <- c(data_len, tick, 2)

#   # Plot data in the frequency domain
#   plot(dat_FFT/max(dat_FFT), type = "l", main = "",
#        ylab = "|Standardized frequency spectrum|", ylim = c(0, y_max),
#        xlab = sprintf("Frequency (1/%s)", lab), xlim = c(1, data_len/2), xaxt = "n")
#   title(sub = sub, line = 4, family = "mono")
#   axis(side = 1, at = data_len/freq_tick * unit + 1, 
#        labels = sprintf("1/%d", freq_tick), cex.axis = 0.7)
# }

# # Preprocessing regarding plot (saving the default settings for plot, then changing them)
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(5, 3.5, 2, 1), mgp = c(2.5, 1, 0))

# # (a) Annual flow of the Nile
# plot.spectrum(log(eu27), lab =   "quarter", sub = "(a)", unit = 3)

# # (b) CO2 concentration in the atmosphere
# plot.spectrum(log(nl), lab =   "quarter", sub = "(b)", tick = c(12, 6), unit = 3)

# # (c) Quarterly gas consumption in the UK
# plot.spectrum(log(nl_exp), lab =   "quarter", sub = "(c)", tick = c(12, 6), unit = 3)

# # (d) Artificially-generated data from a nonlinear model
# plot.spectrum(log(nl_invest), lab = "quarter", sub = "(d)", unit = 3)

# # Ignore the display of following codes

# # Redraw with changing the scale of the vertical axis

# # (a) Annual flow of the Nile
# plot.spectrum(log(eu27), lab = "quarter", sub = "(a)", y_max = .005)

# # (b) CO2 concentration in the atmosphere
# plot.spectrum(log(nl), lab =   "quarter", sub = "(b)", y_max = .005,
#               tick = c(12, 6))

# # (c) Quarterly gas consumption in the UK
# plot.spectrum(log(nl_exp), lab =   "quarter", sub = "(c)", y_max = .005,
#               tick = c(12, 6))

# # (d) Artificially-generated data from a nonlinear model
# plot.spectrum(log(nl_invest), lab = "quarter", sub = "(d)", y_max = .005, tick = c(12, 6))

# # Post-processing regarding plot
# par(oldpar)

# ########################
# # Holt-Winters
# ########################

# HW_EU27 <- HoltWinters(log(eu27))

# # (b) CO2 concentration in the atmosphere
# HW_NL <- HoltWinters(log(nl))

# # (c) Quarterly gas consumption in the UK
# HW_Exp <- HoltWinters(log(nl_exp))

# # (d) Artificial generated data from a nonlinear model
# HW_NL_Invest <- HoltWinters(log(nl_invest))

# # Plot the filtering value

# # Preprocessing regarding plot (saving the default settings for plot, and then changing them)
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(5, 3.5, 2, 1), mgp = c(2.5, 1, 0))
# mygray <- "#80808080"

# plot(HW_EU27, main = "", col = mygray, col.predicted = "black",
#      lty.predicted = "dashed")
# title(sub = "(a)", line = 4, family = "mono")

# plot(HW_NL, main = "", col = mygray, col.predicted = "black",
#      lty.predicted = "dashed")
# title(sub = "(b)", line = 4, family = "mono")

# plot(HW_Exp, main = "", col = mygray, col.predicted = "black",
#      lty.predicted = "dashed")
# title(sub = "(c)", line = 4, family = "mono")

# plot(HW_NL_Invest, main = "", col = mygray, col.predicted = "black",
#      lty.predicted = "dashed")
# title(sub = "(d)", line = 4, family = "mono")

# # Post-processing regarding plot
# par(oldpar)

# # (a) Annual flow of the Nile
# HW_out <- HW_EU27
# HW_decomp <- ts.union(y = HW_out$x,
#                   Level = HW_out$fitted[,  "level"] + HW_out$fitted[, "trend"], 
#                   Season = HW_out$fitted[, "season"],
#               Resisuals = residuals(HW_out))
# plot(HW_decomp, main = "", cex.lab = 1.3, cex.axis = 1.5, mar = c(0, 5, 1, 1))

# # (b) CO2 concentration in the atmosphere
# HW_out <- HW_NL
# HW_decomp <- ts.union(y = HW_out$x,
#                   Level = HW_out$fitted[,  "level"] + HW_out$fitted[, "trend"], 
#                   Season = HW_out$fitted[, "season"],
#               Resisuals = residuals(HW_out))
# plot(HW_decomp, main = "", cex.lab = 1.3, cex.axis = 1.5, mar = c(0, 5, 1, 1))

# # (c) Quarterly gas consumption in the UK
# HW_out <- HW_Exp
# HW_decomp <- ts.union(y = HW_out$x,
#                   Level = HW_out$fitted[,  "level"] + HW_out$fitted[, "trend"], 
#                   Season = HW_out$fitted[, "season"],
#               Resisuals = residuals(HW_out))
# plot(HW_decomp, main = "", cex.lab = 1.3, cex.axis = 1.5, mar = c(0, 5, 1, 1))

# # (d) Artificially-generated data from a nonlinear model
# HW_out <- HW_NL_Invest
# HW_decomp <- ts.union(y = HW_out$x,
#                   Level = HW_out$fitted[,  "level"] + HW_out$fitted[, "trend"], 
#                   Season = HW_out$fitted[, "season"],
#               Resisuals = residuals(HW_out))
# plot(HW_decomp, main = "", cex.lab = 1.3, cex.axis = 1.5, mar = c(0, 5, 1, 1))


# ########################
# # Forecast
# ########################

# eu27_short <- window(eu27, start=c(1995,1), end=c(2023,1))
# str(eu27_short)
# HW_EU27_short <- HoltWinters(log(eu27_short))
# str(HW_EU27_short)

# HW_EU27_short_predict <- predict(HW_EU27_short, n.ahead = 4)
# str(HW_EU27_short_predict)

# # Plot observations along with filtering and prediction values
# plot(HW_EU27_short, HW_EU27_short_predict, main = "Filtering and prediction with Holt-Winters method",
#      col = mygray, col.predicted = "black", lty.predicted = "dashed")

# # Plot observations in 2023 as well
# EU27_2023 <- window(log(eu27), start = c(2023,1))
# lines(EU27_2023, col = mygray)

# ###########################
# ###########################

# nl_exp_short <- window(nl_exp, start=c(1995,1), end=c(2022,1))
# str(nl_exp_short)
# nl_exp_short <- HoltWinters(log(nl_exp_short))
# str(nl_exp_short)

# nl_exp_short_predict <- predict(nl_exp_short, n.ahead = 8)
# str(nl_exp_short_predict)

# # Plot observations along with filtering and prediction values
# plot(nl_exp_short, nl_exp_short_predict, main = "Filtering and prediction with Holt-Winters method",
#      col = mygray, col.predicted = "black", lty.predicted = "dashed")

# # Plot observations in 2022 as well
# nl_exp_2022 <- window(log(nl_exp), start = c(2022,1))
# lines(nl_exp_2022, col = mygray)


# ###########################
# ###########################

# nl_short <- window(nl, start=c(1995,1), end=c(2022,1))
# str(nl_short)
# nl_short <- HoltWinters(log(nl_short))
# str(nl_short)

# nl_short_predict <- predict(nl_short, n.ahead = 8)
# str(nl_short_predict)

# # Plot observations along with filtering and prediction values
# plot(nl_short, nl_short_predict, main = "Filtering and prediction with Holt-Winters method",
#      col = mygray, col.predicted = "black", lty.predicted = "dashed")

# # Plot observations in 2022 as well
# nl_2022 <- window(log(nl), start = c(2022,1))
# lines(nl_2022, col = mygray)

# ###########################
# ###########################

# nl_invest_short <- window(nl_invest, start=c(1995,1), end=c(2022,1))
# str(nl_invest_short)
# nl_invest_short <- HoltWinters(log(nl_invest_short))
# str(nl_invest_short)

# nl_invest_short_predict <- predict(nl_invest_short, n.ahead = 7)
# str(nl_invest_short_predict)

# # Plot observations along with filtering and prediction values
# plot(nl_invest_short, nl_invest_short_predict, main = "Filtering and prediction with Holt-Winters method",
#      col = mygray, col.predicted = "black", lty.predicted = "dashed")

# # Plot observations in 2022 as well
# nl_invest_2022 <- window(log(nl_invest), start = c(2022,1))
# lines(nl_invest_2022, col = mygray)

# #######################
# #######################
# #######################

# # User-defined function for mean absolute percentage error (MAPE)
# MAPE <- function(true, pred){
#   mean(abs(pred - true) / true)
# }

# # The MAPE of the prediction value
# MAPE(true = EU27_2023, pred = HW_EU27_short_predict)
# MAPE(true = nl_exp_2022, pred = nl_exp_short_predict)
# MAPE(true = nl_2022, pred = nl_invest_short_predict)
# MAPE(true = nl_invest_2022, pred = nl_invest_short_predict)






# # Character width in console output
# options(width = 84)

# # User-defined function for mean absolute percentage error (MAPE)
# MAPE <- function(true, pred){
#   mean(abs((pred - true) / true))
# }

# # Introduction and analysis examples of a well-known component model in the linear Gaussian state-space model
# ## Combination of individual models
# ## Local-level model

# ### Example: artificial local-level model
# set.seed(23)
# library(dlm)

# # Setting of local-level model
# W <- 1
# V <- 2
# m0 <- 10
# C0 <- 9
# mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)

# # Generate observations using Kalman prediction
# t_max <- 200
# sim_data <- dlmForecast(mod = mod, nAhead = t_max, sampleNew = 1)
# y <- sim_data$newObs[[1]]

# # Cast the result to ts class
# y <- ts(as.vector(y))

# # Plot results
# plot(y, ylab = "y")

# # Kalman filtering
# dlmFiltered_obj <- dlmFilter(y = y, mod = mod)

# # Find the mean and standard deviation of the filtering distribution
# m <- dropFirst(dlmFiltered_obj$m)
# m_sdev <- sqrt(
#             dropFirst(as.numeric(
#               dlmSvd2var(dlmFiltered_obj$U.C, dlmFiltered_obj$D.C)
#             ))
#           )

# # Find 2.5% and 97.5% values for 95% intervals of the filtering distribution
# m_quant <- list(m + qnorm(0.025, sd = m_sdev), m + qnorm(0.975, sd = m_sdev))

# # Plot results
# ts.plot(cbind(y, m, do.call("cbind", m_quant)),
#         col = c("lightgray", "red", "black", "black"),
#         lty = c("solid", "solid", "dashed", "dashed"))

# # Legend
# legend(legend = c("Observations", "Mean (filtering distribution)", "95% intervals (filtering distribution)"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("lightgray", "red", "black"),
#        x = "topright", text.width = 70, cex = 0.6)


# ######################################
# # Kalman prediction
# ######################################
# dlmForecasted_obj <- dlmForecast(mod = dlmFiltered_obj, nAhead = 10)

# # Find the mean and standard deviation of the predictive distribution
# a <- ts(data = dlmForecasted_obj$a, start = t_max+1)
# a_sdev <- sqrt(
#             as.numeric(
#               dlmForecasted_obj$R
#             )
#           )

# # Find 2.5% and 97.5% values for 95% intervals of the predictive distribution
# a_quant <- list(a + qnorm(0.025, sd = a_sdev), a + qnorm(0.975, sd = a_sdev))

# # Plot results
# ts.plot(cbind(y, a, do.call("cbind", a_quant)),
#         col = c("lightgray", "red", "black", "black"),
#         lty = c("solid", "solid", "dashed", "dashed"))

# # Legend
# legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("lightgray", "red", "black"),
#        x = "bottomleft", text.width = 70, cex = 0.6)


# ######################################
# # Kalman smoothing
# ######################################

# dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# # Find the mean and standard deviation of the smoothing distribution
# s <- dropFirst(dlmSmoothed_obj$s)
# s_sdev <- sqrt(
#             dropFirst(as.numeric(
#               dlmSvd2var(dlmSmoothed_obj$U.S, dlmSmoothed_obj$D.S)
#             ))
#           )

# # Find 2.5% and 97.5% values for 95% intervals of the smoothing distribution
# s_quant <- list(s + qnorm(0.025, sd = s_sdev), s + qnorm(0.975, sd = s_sdev))

# # Plot results
# ts.plot(cbind(y, s, do.call("cbind", s_quant)),
#         col = c("lightgray", "red", "black", "black"),
#         lty = c("solid", "solid", "dashed", "dashed"))

# # Legend
# legend(legend = c("Observations", "Mean (smoothing distribution)", "95% intervals (smoothing distribution)"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("lightgray", "red", "black"),
#        x = "topright", text.width = 70, cex = 0.6)


# # Save the results
# save(t_max, y, mod, m, m_quant, a, a_quant, s, s_quant, 
#      file = "ArtifitialLocalLevelModel.RData")


# ######################################
# ## Local-trend model
# ######################################

# ## Seasonal model
# ### Approach from the time domain
# ### Approach from the frequency domain
# ### Example: CO2 concentration in the atmosphere
# #### Local-trend model + seasonal model (time-domain approach)

# # Preprocessing
# library(dlm)

# # Load the data
# Ryori <- read.csv("data/CO2.csv")

# # Cast the data to ts class, truncating it by December 2014
# y_all <- ts(data = Ryori$CO2, start = c(1987, 1), frequency = 12)
# y <- window(y_all, end = c(2014, 12))

# # Model setting: local-trend model + seasonal model (time-domain approach)
# build_dlm_CO2a <- function(par) {
#   return(
#     dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
#     dlmModSeas(frequency = 12, dW = c(exp(par[4]), rep(0, times = 10)), dV = 0)
#   )
# }

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_CO2a <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_CO2a)
# fit_dlm_CO2a

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_CO2a(fit_dlm_CO2a$par)

# # Kalman filtering
# dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
# dlmFiltered_obja <- dlmFiltered_obj              # Save under a different name for later comparison of prediction values

# # Mean of the filtering distribution
#    mu <- dropFirst(dlmFiltered_obj$m[, 1])
# gamma <- dropFirst(dlmFiltered_obj$m[, 3])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations")
# ts.plot(   mu, ylab = "Level component", ylim = c(350, 405))
# ts.plot(gamma, ylab = "Seasonal component"  , ylim = c( -9,   6))
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)
# ```

# ######################################
# #### Local-level model + seasonal model (time-domain approach)
# ######################################

# # Model setting: local-level model + seasonal model (time-domain approach)
# build_dlm_CO2b <- function(par) {
#   return(
#     dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2])) +
#     dlmModSeas(frequency = 12, dW = c(exp(par[3]), rep(0, times = 10)), dV = 0)
#   )
# }

# # Ignore the display of following codes

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_CO2b <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm_CO2b)
# fit_dlm_CO2b

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_CO2b(fit_dlm_CO2b$par)

# # Kalman filtering
# dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
# dlmFiltered_objb <- dlmFiltered_obj              # Save under a different name for later comparison of prediction values

# # Mean of the filtering distribution
#    mu <- dropFirst(dlmFiltered_obj$m[, 1])
# gamma <- dropFirst(dlmFiltered_obj$m[, 2])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations")
# ts.plot(   mu, ylab = "Level component", ylim = c(350, 405))
# ts.plot(gamma, ylab = "Seasonal component"  , ylim = c( -9,   6))
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)

# ###################################
# #### Local-trend model + seasonal model (frequency-domain approach)
# ######################################

# # Model setting: local-trend model + seasonal model (frequency domain approach)
# build_dlm_CO2c <- function(par) {
#   return(
#     dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
#     dlmModTrig(s = 12, q = 2, dW = exp(par[4]), dV = 0)
#   )
# }

# # Ignore the display of following codes

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_CO2c <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_CO2c)
# fit_dlm_CO2c

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_CO2c(fit_dlm_CO2c$par)

# # Kalman filtering
# dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
# dlmFiltered_objc <- dlmFiltered_obj              # Save under a different name for later comparison of prediction values

# # Mean of the filtering distribution
#    mu <- dropFirst(dlmFiltered_obj$m[, 1])
# gamma <- dropFirst(dlmFiltered_obj$m[, 3] + dlmFiltered_obj$m[, 5])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations")
# ts.plot(   mu, ylab = "Level component", ylim = c(350, 405))
# ts.plot(gamma, ylab = "Seasonal component"  , ylim = c( -9,   6))
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)

# ######################################
# # Kalman prediction
# ######################################

# dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 12)

# # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
# f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
# f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
# f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

# # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
# y_union <- ts.union(y_all, dlmForecasted_object$f, f_lower, f_upper)

# # Ignore the display of following codes

# # Plot results
# plot(y_union, plot.type = "single",
#      xlim = c(2010, 2016),
#      ylim = c( 385,  410), ylab = "", 
#      lty = c("solid", "solid", "dashed", "dashed"),
#      col = c("lightgray", "black", "black", "black"))

# # Legend
# legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("lightgray", "black", "black"),
#        x = "topleft", cex = 0.6)

# # Find the mean, 2.5%, and 97.5% values of the prediction values for each of models a, b, and c
# f_all <- lapply(list(dlmFiltered_obja, dlmFiltered_objb, dlmFiltered_objc),
#                 function(x){
#   # Kalman prediction
#   dlmForecasted_object <- dlmForecast(mod = x, nAhead = 12)

#   # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
#   f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
#   f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
#   f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)
  
#   # Combine the results
#   return(ts.union(
#      mean = dlmForecasted_object$f,
#     lower = f_lower,
#     upper = f_upper
#   ))
# })

# # Combine the prediction results for each model into ts class
# names(f_all) <- c("a", "b", "c")
# y_pred <- do.call("ts.union", f_all)

# # Extract 2015 data from the entire observation
# y_true <- window(y_all, start = 2015)

# # Ignore the display of following codes


# # Plot results
# plot(y_pred, plot.type = "single", type = "b",
#      xlab = "Time (2015)", xaxt = "n", ylab = "",
#      pch = c(rep("a", 3), rep("b", 3), rep("c", 3)),
#      lty = rep(c("solid", "dashed", "dashed"), 3),
#      col = rep(c("lightgray", "darkgray", "darkgray"), 3))
# lines(y_true)
# axis(side = 1, at = time(y_true), labels = 1:12)

# # Legend
# legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("black", "lightgray", "darkgray"),
#        x = "bottomleft", cex = 0.6)



# # <<Comparison of MAPEs from 2015 among three models>>
# MAPE(true = y_true, pred = y_pred[, "a.mean"])
# MAPE(true = y_true, pred = y_pred[, "b.mean"])
# MAPE(true = y_true, pred = y_pred[, "c.mean"])


# ## ARMA model

# ### Example: Japanese beer production

# ```{r Code 9.8, collapse=TRUE}
# # <<Japanese beer production>>

# # Preprocessing
# library(dlm)

# # Load the data
# beer <- read.csv("BEER.csv")

# # Cast the data to ts class
# y <- ts(beer$Shipping_Volume, frequency = 12, start = c(2003, 1))

# # Plot
# plot(y)

# # Log-transform the data
# y <- log(y)

# # Plot log-transformed data
# plot(y, ylab = "log(y)")
# ```


# #### Local-trend model + seasonal model (time-domain approach)

# ```{r Code 9.9, collapse=TRUE}
# # <<Japanese beer production: local-trend model + seasonal model (time-domain approach)>>

# # Model setting: local-trend model + seasonal model (time-domain approach)
# build_dlm_BEERa <- function(par){
#   return(
#     dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
#     dlmModSeas(frequency = 12, dW = c(exp(par[4]), rep(0, times = 10)), dV = 0)
#   )
# }

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_BEERa <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_BEERa)
# fit_dlm_BEERa

# # Set the maximum likelihood estimates of parameters in the model
# mod <- build_dlm_BEERa(fit_dlm_BEERa$par)

# # Kalman smoothing
# dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# # Mean of the smoothing distribution
#    mu <- dropFirst(dlmSmoothed_obj$s[, 1])
# gamma <- dropFirst(dlmSmoothed_obj$s[, 3])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations (log-transformed)")
# ts.plot(   mu, ylab = "Level component")
# ts.plot(gamma, ylab = "Seasonal component")
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)

# #### Local-level model + seasonal model (time-domain approach)

# ```{r Code 9.10, collapse=TRUE}
# # <<Japanese beer production: local-level model + seasonal model (time-domain approach)>>

# # Model setting: local-level model + seasonal model (time-domain approach)
# build_dlm_BEERb <- function(par){
#   return(
#     dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2])) +
#     dlmModSeas(frequency = 12, dW = c(exp(par[3]), rep(0, times = 10)), dV = 0)
#   )
# }

# # Ignore the display of following codes

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_BEERb <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm_BEERb)
# fit_dlm_BEERb

# # Set the maximum likelihood estimates of parameters in the model
# mod <- build_dlm_BEERb(fit_dlm_BEERb$par)

# # Kalman smoothing
# dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# # Mean of the smoothing distribution
#    mu <- dropFirst(dlmSmoothed_obj$s[, 1])
# gamma <- dropFirst(dlmSmoothed_obj$s[, 2])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations (log-transformed)")
# ts.plot(   mu, ylab = "Level component")
# ts.plot(gamma, ylab = "Seasonal component")
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)

# #### Local-level model + seasonal model (time-domain approach) + ARMA model

# ```{r Code 9.11, collapse=TRUE}
# # <<Japanese beer production: considering AR(1) component>>

# # Model setting: local-level model + seasonal model (time-domain approach) + AR(1) model
# build_dlm_BEERc <- function(par){
#   return(
#     dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2]))           +
#     dlmModSeas(frequency = 12, dW = c(exp(par[3]), rep(0, 10)), dV = 0) +
#     dlmModARMA(ar = ARtransPars(par[4]), sigma2 = exp(par[5]))
#   )
# }

# # Ignore the display of following codes

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_BEERc <- dlmMLE(y = y, parm = rep(0, 5), build = build_dlm_BEERc)
# fit_dlm_BEERc
# ARtransPars(fit_dlm_BEERc$par[4])

# # Set the maximum likelihood estimates of parameters in the model
# mod <- build_dlm_BEERc(fit_dlm_BEERc$par)

# # Kalman smoothing
# dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# # Mean of the smoothing distribution
#    mu <- dropFirst(dlmSmoothed_obj$s[,  1])
# gamma <- dropFirst(dlmSmoothed_obj$s[,  2])
#  arma <- dropFirst(dlmSmoothed_obj$s[, 13])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(4, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "log-Observations")
# ts.plot(   mu, ylab = "Level component")
# ts.plot(gamma, ylab = "Seasonal component")
# ts.plot( arma, ylab = "AR(1) component")
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# # Confirm the log-likelihood
# -dlmLL(y = y, mod = mod)

# ## Regression model
# ### Example: Nintendo's stock price
# ```{r Code 9.12, collapse=TRUE}
# # <<Nintendo's stock price>>

# # Preprocessing
# library(dlm)

# # Load the data
# NINTENDO <- read.csv("data/NINTENDO.csv")
# NINTENDO$Date <- as.Date(NINTENDO$Date)

# NIKKEI225 <- read.csv("data/NIKKEI225.csv")
# NIKKEI225$Date <- as.Date(NIKKEI225$Date)

# # Set observations and explanatory variables
# y      <- NINTENDO$Close
# x_dash <- NIKKEI225$Close

# # Ignore the display of following codes

# # Plot
# plot(x = NINTENDO$Date , y = y     , xlab = ""    , ylab = "",
#      ylim = c(10695, 28220), type = "l", col = "lightgray")
# par(new=T)
# plot(x = NIKKEI225$Date, y = x_dash, xlab = "Time", ylab = "",
#      ylim = c(10695, 28220), type = "l", lty = "dashed"   )

# # Legend
# legend(legend = c("Nintendo's stock price", "Nikkei stock average"),
#        lty = c("solid", "dashed"),
#        col = c("lightgray", "black"),
#        x = "topleft", cex = 0.6)


# ```{r Code 9.13, collapse=TRUE}
# # <<Beta for Nintendo's stock price>>

# ########################################
# # Model setting: regression model
# ########################################

# build_dlm_REG <- function(par) {
#   dlmModReg(X = x_dash, dW = exp(par[1:2]), dV = exp(par[3]))
# }

# # Maximum likelihood estimation of parameters and confirmation of the results
# fit_dlm_REG <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm_REG)
# fit_dlm_REG

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_REG(fit_dlm_REG$par)
# str(mod)

# # Kalman smoothing
# dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# # Ignore the display of following codes
# # Find the mean and standard deviation of the smoothing distribution

# beta <- dropFirst(dlmSmoothed_obj$s[, 2])
# beta_sdev <- sqrt(dropFirst(
#                sapply(dlmSvd2var(dlmSmoothed_obj$U.S, dlmSmoothed_obj$D.S), function(x){
#                  diag(x)[2]
#                })
#              ))

# # Find 2.5% and 97.5% values for 95% intervals of the smoothing distribution
# beta_quant <- list(beta + qnorm(0.025, sd = beta_sdev),
#                    beta + qnorm(0.975, sd = beta_sdev))

# # Plot results
#  plot(x = NINTENDO$Date, y = beta, type="l", ylim = c(0.5, 2.2),
#       xlab = "Time", ylab = "Beta value")
# lines(x = NINTENDO$Date, y = beta_quant[[1]], lty = "dashed")
# lines(x = NINTENDO$Date, y = beta_quant[[2]], lty = "dashed")

# # Legend
# legend(legend = c("Mean (smoothing distribution)", "95% intervals (smoothing distribution)"),
#        lty = c("solid", "dashed"),
#        col = c("black", "black"),
#        x = "topleft", cex = 0.6)

# # Reference event
# mtext("x", at = as.Date("2015/3/17"), side = 1, adj = 0.5, line = -0.5)
# mtext("x", at = as.Date("2016/7/6" ), side = 1, adj = 0.5, line = -0.5)
# mtext("x", at = as.Date("2016/7/22"), side = 1, adj = 0.5, line = -0.5)
# mtext("2015/3/17", at = as.Date("2015/3/17"), side = 1, adj = 0, cex = 0.6)
# mtext("2016/7/6" , at = as.Date("2016/7/6" ), side = 1, adj = 1, cex = 0.6)
# mtext("2016/7/22", at = as.Date("2016/7/22"), side = 1, adj = 0, cex = 0.6)

# ### Example: flow data of the Nile (considering the rapid decrease in 1899)

# ```{r Code 9.14, collapse=TRUE}
# # <<Apply local-level model + regression model (intervention variable) to flow data of the Nile>>

# # Preprocessing
# set.seed(123)
# library(dlm)

# # Flow data of the Nile
# y <- Nile
# t_max <- length(y)

# # Set the explanatory variable (intervention variable)
# x_dash <- rep(0, t_max)                  # All initial value 0s (no dam)
# x_dash[which(1899 <= time(y))] <- 1      # All 1s after 1899 (with dam)

# # Function building local-level model + regression model (intervention variable)
# build_dlm_DAM <- function(par) {
#   return(
#     dlmModPoly(order = 1, dV = exp(par[1]), dW = exp(par[2])) +
#     dlmModReg(X = x_dash, addInt = FALSE, dW = exp(par[3]), dV = 0)
#   )
# }

# # Maximum likelihood estimation of parameters
# fit_dlm_DAM <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm_DAM)
# modtv <- build_dlm_DAM(fit_dlm_DAM$par)

# # Kalman smoothing
# dlmSmoothed_obj <- dlmSmooth(y = y, mod = modtv)

# # Mean and variance of the smoothing distribution
# stv <- dropFirst(dlmSmoothed_obj$s)
# stv_var <- dlmSvd2var(dlmSmoothed_obj$U.S, dlmSmoothed_obj$D.S)
# stv_var <- stv_var[-1]

# # Mean for estimator
# s <- stv[, 1] + x_dash * stv[, 2]                       # Consider also x_dash

# # 95% intervals of level estimator (finding 2.5% and 97.5% values)
# coeff <- cbind(1, x_dash)
# s_sdev <- sqrt(sapply(seq_along(stv_var), function(ct){ # Consider covariance
#             coeff[ct, ] %*% stv_var[[ct]] %*% t(coeff[ct, , drop = FALSE])
#           }))           
# s_quant <- list(s + qnorm(0.025, sd = s_sdev), s + qnorm(0.975, sd = s_sdev))

# # Ignore the display of following codes

# # Plot
# ts.plot(cbind(y, s, do.call("cbind", s_quant)),
#         lty=c("solid", "solid", "dashed", "dashed"),
#         col=c("lightgray", "black", "black", "black"))

# # Legend
# legend(legend = c("Observations", "Mean", "95% intervals"),
#        lty = c("solid", "solid", "dashed"),
#        col = c("lightgray", "black", "black"),
#        x = "topright", cex = 0.6)


# ### Example: family food expenditure (considering effects depending on the days of the week)

# ```{r Code 9.15, collapse=TRUE}
# # <<Family expenditure (food)>>

# # Preprocessing
# library(dlm)

# # Load the data
# food <- read.csv("FOOD.csv")

# # Cast the data to ts class
# y <- ts(food$Expenditure, frequency = 12, start = c(2000, 1))

# # Plot
# plot(y)

# # Log-transform the data
# y <- log(y)

# # Plot log-transformed data
# plot(y, ylab = "log(y)")


# # Model setting: local-trend model + seasonal model (time-domain approach)
# build_dlm_FOODa <- function(par) {
#   return(
#     dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2])) +
#     dlmModSeas(frequency = 12, dW = c(exp(par[3]), rep(0, times = 10)), dV = 0)
#   )
# }

# # Maximum likelihood estimation of parameters
# fit_dlm_FOODa <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm_FOODa)

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_FOODa(fit_dlm_FOODa$par)
# -dlmLL(y = y, mod = mod)

# # Kalman filtering
# dlmSmoothed_obj  <- dlmSmooth(y = y, mod = mod)

# # Mean of the smoothing distribution
#    mu <- dropFirst(dlmSmoothed_obj$s[, 1])
# gamma <- dropFirst(dlmSmoothed_obj$s[, 3])

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "Observations (log-transformed)")
# ts.plot(   mu, ylab = "Level component")
# ts.plot(gamma, ylab = "Seasonal component")
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)

# ```{r Figure 9.19, echo = FALSE, results='hide'}
# # <<Daily food expenditure>>

# # Load daily data (June 2009)
# food_day <- read.csv("data/FOOD_DAY.csv")
# food_day$Date <- as.Date(food_day$Date)

# # Display daily data (June 2009)
# plot(x = food_day$Date, y = food_day$Expenditure,
#      type = "l", xlab = "June 2009", ylab = "Daily food expenditure (yen)", xaxt = "n")
# axis(side = 1, at = food_day$Date, labels = FALSE)
# x_lab <- weekdays(food_day$Date, abbreviate = TRUE)
# x_lab[!(x_lab %in% c("Sat", "Sun"))] <- ""; x_lab <- gsub("([tn])", "\\1.", x_lab)
# par(xpd = TRUE)
# text(labels =  x_lab, x = as.Date(food_day$Date), y = 1850, adj = c(0, 0.5), srt = -45)
# text(labels =  "1st", x = as.Date( "2009-06-01"), y = 1850, adj = c(0, 0.5), srt = -45)
# text(labels = "30th", x = as.Date( "2009-06-30"), y = 1850, adj = c(0, 0.5), srt = -45)
# par(xpd = FALSE)

# ```{r Code 9.16, collapse=TRUE}
# # <<Set explanatory variable (weekday effect)>>
# install.packages("Nippon")

# # User-defined function returning weekdays and holidays in Japan
# jholidays <- function(days){
#   # Use of is.jholiday()
#   library(Nippon)

#   # Obtain the day of the week
#   DOW <- weekdays(days)

#   # Consider Saturdays, Sundays, and other public holidays, including their compensations, as holidays
#   holidays <- (DOW %in% c("Saturday", "Sunday")) | is.jholiday(days)

#   # Overwrite the day of the week with "HOLIDAY" or "WEEKDAY"
#   DOW[ holidays] <- "Holiday"
#   DOW[!holidays] <- "Weekday"

#   return(DOW)
# }

# # Sequence of the date during the examination period
# days <- seq(from = as.Date("2000/1/1"), to = as.Date("2009/12/31"), by = "day")

# # Aggregate the number of weekdays or holidays for every month
# monthly <- table(substr(days, start = 1, stop = 7), jholidays(days))  

# # Explanatory variable (difference between the total number of weekdays and holidays in a month)
# x_dash_weekday <- monthly[, "Weekday"] - monthly[, "Holiday"]




# # Data length
# t_max <- length(y)

# # February in a leap year during the examination period
# LEAPYEAR_FEB <- (c(2000, 2004, 2008) - 2000)*12 + 2

# # Explanatory variable (February in a leap year only corresponds to 1)
# x_dash_leapyear <- rep(0, t_max)          # All initial value 0s
# x_dash_leapyear[LEAPYEAR_FEB] <- 1        # February in leap year corresponds to 1


# # <<Food expenditure analysis with local-level model + seasonal model (time-domain approach) + regression model>>

# # Bind explanatory variables (weekday and leap year effects)
# x_dash <- cbind(x_dash_weekday, x_dash_leapyear)

# # Function building local-level model + seasonal model (time-domain approach) + regression model
# build_dlm_FOODb <- function(par) {
#   return(
#     dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2]))         +
#     dlmModSeas(frequency = 12, dW = c(0, rep(0, times = 10)), dV = 0) +
#     dlmModReg(X = x_dash, addInt = FALSE, dV = 0)
#   )
# }

# # Maximum likelihood estimation of parameters
# fit_dlm_FOODb <- dlmMLE(y = y, parm = rep(0, 2), build = build_dlm_FOODb)

# # Set the maximum likelihood estimates of parameters in the model
# mod  <- build_dlm_FOODb(fit_dlm_FOODb$par)
# -dlmLL(y = y, mod = mod)

# # Kalman filtering
# dlmSmoothed_obj  <- dlmSmooth(y = y, mod = mod)

# # Mean of the smoothing distribution
#     mu <- dropFirst(dlmSmoothed_obj$s[, 1])
#  gamma <- dropFirst(dlmSmoothed_obj$s[, 3])
# beta_w <- dropFirst(dlmSmoothed_obj$s[, 13])[t_max]  # Time-invariant
# beta_l <- dropFirst(dlmSmoothed_obj$s[, 14])[t_max]  # Time-invariant

# # Confirmation of the results
# cat(beta_w, beta_l, "\n")

# # Mean of regression component
# reg <- x_dash %*% c(beta_w, beta_l)
# tsp(reg) <- tsp(y)

# # Plot results
# oldpar <- par(no.readonly = TRUE)
# par(mfrow = c(4, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
# ts.plot(    y, ylab = "log-Observations")
# ts.plot(   mu, ylab = "Level component")
# ts.plot(gamma, ylab = "Seasonal component")
# ts.plot(  reg, ylab = "Regression component")
# mtext(text = "Time", side = 1, line = 1, outer = TRUE)
# par(oldpar)
