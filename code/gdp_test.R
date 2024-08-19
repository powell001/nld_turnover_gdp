library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)

# load data
dt1 <- read.csv("data/cbs_basic_macro_NOT_SEASONCORRECTED_qt.csv", sep=",")

##### Start at 1995-01-01
#dt1 <- dt1[-(1:21), ]

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
  par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

  begin <- 1 + (j * 4)
  end   <- 4 + (j * 4)

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
  par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

  begin <- 1 + (j * 4)
  end   <- 4 + (j * 4)

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
