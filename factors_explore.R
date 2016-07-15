setwd("/Users/kwalker/git_projects/yield")
data <- read.csv("min_factors.csv", stringsAsFactors=FALSE)

# helpful: http://www.ats.ucla.edu/stat/r/dae/logit.htm

library(reshape2)
library(ggplot2)
library(mice)
library(lattice)

# check for collinearity
collin <- round(cor(data[,2:33], use = "pair"), 2) 
# total gift + FAFSA + award status; summer and made.deposit + final.status

data1fit <- data[ , -c(2,3,4,6,7,9,12,16,18,24,29)] # drop hs size, made deposit, award status, counts, summer o, countdown + act/gpa/hsp/distance

# impute missing values
tempData <- mice(data1fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data1fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(2:28)) data1fit.imp[,j] <- round(data1fit.imp[,j])

# test and train sets
train1 <- data1fit.imp[data1fit.imp$Year!="2016 Fall", ]
test1 <- data1fit.imp[data1fit.imp$Year=="2016 Fall", ]

# fit model
fit1g <- glm(Final.Status ~ ., train1[,-1], family="binomial")
fit1gs <- step(fit1g, direction="both", trace=0)

coef1 <- exp(cbind(Odds.Ratio = coef(fit1gs), confint(fit1gs)))

test1$Predictions_raw <- unlist(predict(fit1gs, test1, type="response"))
test1$Predictions <- sapply(test1$Predictions_raw, function(x) if(x > .3){x <- 1} else {x <- 0})
table(test1$Predictions, test1$Final.Status)

check <- data.frame()
for(cutoff in c(.1,.15,.2,.25,.3,.35,.4,.45,.5,.6,.75,.9)) {
    
    test1$Predictions <- sapply(test1$Predictions_raw, function(x) if(x>cutoff){x<-1}else{x<-0})
    
    results <- table(test1$Predictions, test1$Final.Status)
    accuracy <- round(sum(results[1,1], results[2,2])/sum(results[,2], results[,1]),2)*100
    tp <- round(results[2,2]/sum(results[,2]),4)*100
    fn <- round(results[1,2]/sum(results[,2]),4)*100
    tn <- round(results[1,1]/sum(results[,1]),4)*100
    fp <- round(results[2,1]/sum(results[,1]),4)*100
    
    row <- data.frame(cutoff, accuracy, tp, fp, tn, fn)
    check <- rbind(check,row)
}


library(ROCR)
pred <- prediction(test1$Predictions, test1$Final.Status)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

