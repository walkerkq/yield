library(corrplot)
library(ROCR)
library(ggplot2)

data <- read.csv("enrollment_data.csv", stringsAsFactors=F)

# test and train sets
train1 <- data[data$Year!="2016 Fall" & data$App=="1", -18] 
test1 <- data[data$Year=="2016 Fall" & data$App=="1", -18]

# check for collin
co1 <- round(cor(train1[,-c(2,3)]),4)
co1_concern <- co1[abs(co1)>0.75] # just an identity matrix --> good 

# make a correlation plot
co1.df <- train1[,-c(2,3)]
colnames(co1.df) <- c("Enrollment Status", "ACT", "GPA", "Distance from Moorhead", "Zip Income", "Zip Population", "Zip White", "Zip Alumni",
                      "Music", "Athletics", "Legacy", "Gender", "Ethnicity", "Lutheran", "Religious", "Deposited", "FAFSA", "Honors Invite",
                      "Honors Accept", "Visits", "Perf. Scholarship Event", "Academic Scholarship Event", "Zip Peer Admitted Students")
co1 <- round(cor(co1.df),4)
par(mar=c(rep(5,4)))
corrplot(co1, method="color", type="upper", tl.col="black", tl.cex=.75,mar=c(0,0,1,0), order="FPC")

# summary stats of training set
summary(train1)
library(psych)
describe(train1[,-c(2,3)], type=2)

# fit model
fit1g <- glm(Final.Status ~ ., train1[,-c(2,3)], family="binomial") 
fit1gs <- step(fit1g, direction="both", trace=0) # stepwise selection
coef1s <- exp(cbind(Odds.Ratio = coef(fit1gs), confint(fit1gs))) # get odds ratios

# make predictions and evaluate cutoff options
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

# categorize predictions based on cutoff point
test1$Predictions <- ifelse(test1$Predictions_raw>0.20, 1, 0)

# create ROC plot
pred <- prediction(test1$Predictions, test1$Final.Status)
perf <- performance(pred, "tpr", "fpr")
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) + 
     geom_line(aes(y=tpr)) + labs(title=paste0("ROC Curve w/ AUC=", round(auc,4))) + theme_classic()
