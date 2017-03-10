setwd("/Users/kaylinwalker/R/yield/get_and_clean")
source("07_date_filter.R")

# helpful: http://www.ats.ucla.edu/stat/r/dae/logit.htm

library(reshape2)
library(ggplot2)
library(corrplot)

# choosing march 15 for 80% of fafsas in
data <- date_scale(as.Date("2015-03-01"))

# scale data
#1000s
data$Zip.Median.Income <- data$Zip.Median.Income/1000
data$Zip.Population <- data$Zip.Population/1000
# pcts
data$Zip.Pct.White <- data$Zip.Pct.White*100
data$Zip.Alumni.Density <- data$Zip.Alumni.Density*100
data$Other.Admits.Density <- data$Other.Admits.Density*100

# data <- read.csv("enrollment_data.csv", stringsAsFactors=F)
#train1 <- data[data$Year!="2016 Fall" & data$App=="1", -c(18)]
#test1 <- data[data$Year=="2016 Fall" & data$App=="1", -c(18)]

# test and train sets
train1 <- data[data$Year!="2016 Fall" & data$App=="1", -c(6,19:25,28,32,35)] # only use apps+ (not future apps), no cut vars, no countdown/awards
test1 <- data[data$Year=="2016 Fall" & data$App=="1", -c(6,19:25,28,32,35)]

# check for collin
co1 <- round(cor(train1[,-c(2,3)]),4)
co1_concern <- co1[abs(co1)>0.75] # just an identity matrix --> good to go
# make a correlation plot
co1.df <- train1[,-c(2,3)]
colnames(co1.df) <- c("Enrollment Status", "ACT", "GPA", "Distance from Moorhead", "Zip Income", "Zip Population", "Zip White", "Zip Alumni",
                      "Music", "Athletics", "Legacy", "Gender", "Ethnicity", "Lutheran", "Religious", "Deposited", "FAFSA", "CREDO Invite",
                      "CREDO Accept", "Visits", "Perf. Scholarship Event", "Academic Scholarship Event", "Zip Peer Admitted Students")
co1 <- round(cor(co1.df),4)
par(mar=c(rep(5,4)))
corrplot(co1, method="color", type="upper", tl.col="black", tl.cex=.75,mar=c(0,0,1,0), order="FPC")
# independence
plot(predict(fit1gs),residuals(fit1gs))
abline(h=0,lty=2,col="grey")
lines(lowess(predict(fit1gs),residuals(fit1gs)),col="black",lwd=2)

# summary stats of training set
summary(train1)
library(psych)
describe(train1[,-c(2,3)], type=2)


# fit model
fit1g <- glm(Final.Status ~ ., train1[,-c(2,3)], family="binomial") 
fit1gs <- step(fit1g, direction="both", trace=0)

coef1 <- exp(cbind(Odds.Ratio = coef(fit1g), confint(fit1g)))
coef1s <- exp(cbind(Odds.Ratio = coef(fit1gs), confint(fit1gs)))

test1$Predictions_raw <- unlist(predict(fit1gs, test1, type="response"))

test <- data.frame()
for( cutoff in c(.05, .075, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9)) {
     test1$Predictions <- ifelse(test1$Predictions_raw > cutoff, 1, 0) 
     
     results <- table(test1$Predictions, test1$Final.Status)
     accuracy <- round((results[1,1] + results[2,2])/sum(results[,1], results[,2]),2)
     fn <- round(results[1,2]/sum(results[,2]),2)
     tn <- round(results[1,1]/sum(results[,1]),2)
     fp <- round(results[2,1]/sum(results[,1]),2)
     tp <- round(results[2,2]/sum(results[,2]),2)
     
     row <- data.frame(cutoff, accuracy, tp, tn, fp, fn)
     test <- rbind(test, row)
}
testm <- melt(test, id="cutoff")
ggplot(testm[testm$variable %in% c("accuracy", "tp", "fn"),], aes(cutoff, value)) + geom_line(aes(color=variable))

library(ROCR)
test1$Predictions <- ifelse(test1$Predictions_raw>0.20, 1, 0)
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
    labs(title=paste0("ROC Curve w/ AUC=", round(auc,4)))



################

# attempt 2
data2 <- date_scale(as.Date("2015-04-01"))
# scale data
#1000s
data2$Zip.Median.Income <- data2$Zip.Median.Income/1000
data2$Zip.Population <- data2$Zip.Population/1000
# pcts
data2$Zip.Pct.White <- data2$Zip.Pct.White*100
data2$Zip.Alumni.Density <- data2$Zip.Alumni.Density*100
data2$Other.Admits.Density <- data2$Other.Admits.Density*100

# test and train sets
train2 <- data2[data2$Year!="2016 Fall" & data2$App=="1", -c(6,19:25,32)] # only use apps+ (not future apps)
test2 <- data2[data2$Year=="2016 Fall" & data2$App=="1", -c(6,19:25,32)]

# fit model
fit2g <- glm(Final.Status ~ ., train2[,-c(2,3)], family="binomial") # use cut vars 
fit2gs <- step(fit2g, direction="both", trace=0)

coef2 <- exp(cbind(Odds.Ratio = coef(fit2g), confint(fit2g)))
coef2s <- exp(cbind(Odds.Ratio = coef(fit2gs), confint(fit2gs)))

test2$Predictions_raw <- unlist(predict(fit2gs, test2, type="response"))

testp <- data.frame()
for( cutoff in c(.05, .075, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9)) {
     test2$Predictions <- ifelse(test2$Predictions_raw>cutoff, 1, 0) 
     results <- table(test2$Predictions, test2$Final.Status)
     accuracy <- round((results[1,1] + results[2,2])/sum(results[,1], results[,2]),2)
     fn <- round(results[1,2]/sum(results[,2]),2)
     tn <- round(results[1,1]/sum(results[,1]),2)
     fp <- round(results[2,1]/sum(results[,1]),2)
     tp <- round(results[2,2]/sum(results[,2]),2)
     row <- data.frame(cutoff, accuracy, tp, tn, fp, fn)
     testp <- rbind(testp, row)
}
testm2 <- melt(testp, id="cutoff")
ggplot(testm2[testm2$variable %in% c("accuracy", "tp", "fn"),], aes(cutoff, value)) + geom_line(aes(color=variable))

# roc
test2$Predictions <- ifelse(test2$Predictions_raw>0.20, 1, 0)
pred2 <- prediction(test2$Predictions, test2$Final.Status)
perf2 <- performance(pred2, "tnr", "fnr")
auc2 <- performance(pred2, measure="auc")
auc2 <- auc2@y.values[[1]]

roc.data2 <- data.frame(fnr=unlist(perf2@x.values),
                       tnr=unlist(perf2@y.values),
                       model="GLM")

ggplot(roc.data2, aes(x=fnr, ymin=0, ymax=tnr)) +
     geom_ribbon(alpha=0.2) +
     geom_line(aes(y=tnr)) +
     labs(title=paste0("ROC Curve w/ AUC=", auc2))


comp2 <- test2[,c(2,25:38)]
comp <- merge(test1, comp2, by="ID", all=T)
comp <- comp[!is.na(comp$Predictions.x),]
comp_sm <- comp[,c(1,2,37,38,51,52)]
xtabs(~ Predictions.x + Predictions.y, data=comp_sm)


#OTHER
library(randomForest)
library(rpart)
library(gbm)
library(arm)

r1 <- randomForest(Final.Status ~ ., train1)
r2 <- gbm(Final.Status ~ ., data=train1)
r3 <- rpart(y~x)
r4 <- bayesglm(y ~ x, family=binomial)

yy1 <- predict(r1, data.frame(x=test1))
yy2 <- predict(r2, data.frame(x=xx))
yy3 <- predict(r3, data.frame(x=xx))
yy4 <- predict(r4, data.frame(x=xx), type="response")



