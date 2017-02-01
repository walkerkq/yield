# 06_impute_missing.R
# standardize and clean data for analysis
# create indiator variables for factors
# in: non_date_data.csv
# out: data with imputed values, date scaled - non_date_data_imputed.csv

library(mice)
setwd("/Users/kaylinwalker/R/yield")

data <- read.csv("data/non_date_data.csv", stringsAsFactors=F)

# impute missing geographic data
data$HS.Percentile[data$HS.Percentile==0] <- NA
data$ACT[data$ACT==0] <- NA
data <- data[,-c(6,13)] # drop HS size and percentile, too many missing values

tempData <- mice(data, m=1, maxit=10, meth='pmm', seed=500)
data <- complete(tempData, 1)

# ACT, GPA and distance cuts
data$ACT.High <- 0
data$ACT.Low <- 0
data$GPA.High <- 0
data$GPA.Low <- 0
data$Distance.High <- 0
data$Distance.Low <- 0

# create cut vars
data$ACT.High <- ifelse(data$ACT >= 30, 1, 0)
data$ACT.Low <- ifelse(data$ACT <= 24, 1, 0)
data$GPA.High <- ifelse(data$GPA > 3.9, 1, 0)
data$GPA.Low <- ifelse(data$GPA < 3.0, 1, 0)
data$Distance.High <- ifelse(data$Distance.Mhd > 300, 1, 0)
data$Distance.Low<- ifelse(data$Distance.Mhd < 75, 1, 0)

# write.csv(data, "data/non_date_data_imputed.csv", row.names=F)


