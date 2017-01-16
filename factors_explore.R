setwd("/Users/kpavlik/git_projects/yield")
data <- read.csv("min_factors.csv", stringsAsFactors=FALSE)

# helpful: http://www.ats.ucla.edu/stat/r/dae/logit.htm

library(reshape2)
library(ggplot2)
library(mice)
library(lattice)


# handle missing values
data$Year <- factor(data$Year)
missing <- data.frame(var=names(data))
for(u in levels(data$Year)) {
    subset <- data[data$Year==u, ]
    miss <- data.frame(var=names(subset))
    for(g in seq_along(data)) { miss$NAs[g] <- round(sum(is.na(subset[,g]))/length(subset[,g]),4)  }
    missing <- cbind(missing, miss$NAs)
}
colnames(missing) <- c("Variable", levels(data$Year))
missing2 <- melt(missing, id="Variable")
ggplot(missing2, aes(Variable, value)) + geom_bar(stat="identity", position="dodge", aes(fill=variable)) +
    ylab("Percent Missing") + xlab("") + theme_classic() + geom_hline(yintercept=.4) + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=12)) + labs(title="Missingness by Variable")

# check for collinearity
collin <- round(cor(data[,2:32], use = "pair"), 2) 
# no issues

data1fit <- data[ , -c(12)] # drop hs size


# impute missing values
tempData <- mice(data1fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data1fit.imp <- complete(tempData, 1)

# fix binomial issues
#for(j in c(3,7:13,24)) data1fit.imp[,j] <- round(data1fit.imp[,j])

# with cuts
cuts <- c("ACT.Low", "ACT.High", "GPA.Low", "GPA.High", "HSP.Low", "HSP.High", "Distance.Low", "Distance.High")
whole <- c("ACT", "GPA", "HS.Percentile", "Distance.Mhd")
data1fit.imp2 <- data1fit.imp[ ,-c(2:4,6)]
data1fit.imp1 <- data1fit.imp[ ,-c(24:31)]

# test and train sets
train1 <- data1fit.imp1[data1fit.imp1$Year!="2016 Fall", ]
test1 <- data1fit.imp1[data1fit.imp1$Year=="2016 Fall", ]

# fit model
fit1g <- glm(Final.Status ~ ., train1[,-1], family="binomial")
fit1gs <- step(fit1g, direction="both", trace=0)

coef1 <- exp(cbind(Odds.Ratio = coef(fit1gs), confint(fit1gs)))

test1$Predictions_raw <- unlist(predict(fit1gs, test1, type="response"))

test <- data.frame()
for( cutoff in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .8, .9)) {
     test1$Predictions <- ifelse(test1$Predictions_raw>cutoff, 1, 0) # 0.25 cutoff is best
     
     results <- table(test1$Predictions, test1$Final.Status)
     accuracy <- round((results[1,1] + results[2,2])/sum(results[,1], results[,2]),2)
     fn <- round(results[1,2]/sum(results[,2]),2)
     tn <- round(results[1,1]/sum(results[,1]),2)
     fp <- round(results[2,1]/sum(results[,1]),2)
     tp <- round(results[2,2]/sum(results[,2]),2)
     
     row <- data.frame(cutoff, accuracy, tp, tn, fp, fn)
     test <- rbind(test, row)
}


library(ROCR)
test1$Predictions <- ifelse(test1$Predictions_raw>0.2, 1, 0)
pred <- prediction(test1$Predictions, test1$Final.Status)
perf <- performance(pred, "tpr", "fpr")
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    labs(title=paste0("ROC Curve w/ AUC=", auc))

# random Forest
library(caret)
library(randomForest)
rf.fit <- caret::train(Final.Status ~ ., train1[,-1], method="rf")
