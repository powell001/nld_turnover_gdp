# There are 30 potential ETS models from which to choose, how do you choose the best without overfitting?  
# Overfitting in this case is paramount to fishing for the model that fits the given data the best without 
# considering the economic implications of the choices; the repurcussion is that the model fits random data 
# rather than fitting the underlying economic data generating process or processes.  The trick is that you 
# will never know whether the data is random or not.  Ask yourself, does this make economic sense.

# How much should you discount historical data?  This is an economic question.

# The essenential data science skill is the one you need at that moment to answer the question at hand.

#####################################
#####################################
# DLM Package on Cran
# Forecast package: https://cran.rstudio.com/web/packages/forecast/vignettes/JSS2008.pdf 
#####################################
#####################################

library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)
library(dlm)
library(expsmooth)
library(ggplot2)

###########

ets(bonds, model = "AAN", damped = TRUE)
ets(usnetelec, model = "MMN", damped = TRUE)
ets(ukcars, model = "ANA", damped = FALSE)
ets(visitors, model = "MAM", damped = FALSE)


appropriateModels <- c("ANNX", "ANAX", "ANMX",
                       "AANX", "AAAX", "AAMX",
                       "AAND", "AAAD", "ANMD",
                       "AMNX", "AMAX", "AMMX",
                       "AMND", "AMAD", "AMMD",
                       "MNNX", "MNAX", "MNMX",
                       "MANX", "MAAX", "MAMX",
                       "MAND", "MAAD", "MNMD",
                       "MMNX", "MMAX", "MMMX",
                       "MMND", "MMAD", "MMMD"
                       )

bestMod <- function(appropriateModels, data, lowest_aic = Inf){

    for (mod in appropriateModels){

    tryCatch({

        ##############################
        #print(mod)  

        error1 <- str_sub(mod, 1, 1)
        trend2 <- str_sub(mod, 2, 2)
        season3 <- str_sub(mod, 3, 3)
        damp4 <- str_sub(mod, 4, 4)
        ##############################

    if(damp4 == "X"){ #damped is FALSE
            model <- ets(data, model = mod, damped = FALSE)
            #print(paste(mod, model$aic, damped = FALSE, sep = " "))
            out1 <- list(mod, model$aic, damped = FALSE)
            #print(out1[[2]])

            } else { #damped is TRUE
                model <- ets(data, model = mod, damped = TRUE)
                #print(paste(mod, model$aic, damped = TRUE, sep = " "))
                out1 <- list(mod, model$aic, damped = TRUE)
                #print(out1[[2]])
            }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})    

    new_aic <- out1[[2]]

    if (new_aic < lowest_aic){
        lowest_aic <- out1[[2]]
        best_model <- out1
    }
    }
   

    bm_df <- t(as.data.frame(matrix(unlist(best_model)),nrow=1,ncol=3))  

    colnames(bm_df) <- c("Model", "AIC", "Damped")
    rownames(bm_df) <- c("model")

    return(bm_df)
}

bestlist <- list()
datalist <- list(bonds, usnetelec, ukcars, visitors)
for(bm in datalist){
   out1 <- bestMod(appropriateModels, bm)
   bestlist <- rbind(bestlist, out1)
}

rownames(bestlist) <- c("bonds", "usnetelec", "ukcars", "visitors")
as.data.frame(bestlist)

###########
# Load data
dt1 <- read.csv("data/a0_combinedQuarterly_extended_ETS.csv", sep=",", stringsAsFactors = FALSE)
#dt1 <- read.csv("data/cbs_basic_macro_NOT_SEASONCORRECTED_qt.csv", sep=",", stringsAsFactors = FALSE)

dt2 <- dt1[, 2:24]
bestlist <- list()

for(col1 in colnames(dt2)){
    y <- dt2[, col1]
    ts_data <- ts(na.omit(y), frequency = 4, start = c(1995, 1)) 
    out1 <- bestMod(appropriateModels, ts_data)
    bestlist <- rbind(bestlist, out1)
}
rownames(bestlist) <- colnames(dt2)
as.data.frame(bestlist)


######################################################
######################################################


dt1 <- dt1[c('gdp_total_season')]
str(dt1)

###########
# Columns names
colNames <- colnames(dt1)
# Number rows and columns
rows1 <- dim(dt1)[1]
cols1 <- dim(dt1)[2]

#############
# first look

plot(ts(na.omit(dt1[, 'gdp_total_season']), frequency = 4, start = c(1995, 1)), main = "GDP Total Season", col = "blue", lwd = 2)
#dev.off()

###########################
# KalmanSmoothing
###########################

y <- dt1[, 'gdp_total_season']

# ######################################
# # Kalman smoothing
# ######################################

# Setting of local-level model
W <- 1
V <- 2
m0 <- 100000
C0 <- 100

mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)

dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# Find the mean and standard deviation of the smoothing distribution
s <- dropFirst(dlmSmoothed_obj$s)

# which of the original data is missing, for example, sector: 11_Drankenindustrie
replaceThese <- which(is.na(y))
y[replaceThese] <- s[replaceThese]

print(y)

plot(ts(na.omit(y), frequency = 4, start = c(1995, 1)), main = "GDP Total Season", col = "red", lwd = 3)
#dev.off()
