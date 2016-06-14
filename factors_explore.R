setwd("/Users/kwalker/git_projects/yield")
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
ggplot(missing2[missing2$variable!="2017 Fall", ], aes(Variable, value)) + geom_bar(stat="identity", position="dodge", aes(fill=variable)) +
    scale_fill_brewer(palette="Paired") + ylab("Percent Missing") + xlab("") + theme_classic() + geom_hline(yintercept=.4) + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=12)) + labs(title="Missingness by Variable")

# option 1: drop 2010-2012 + impute missing values (+ drop schol. status, hs size and total inst. gift)
# option 2: drop award status, comp. cost, first visit, gross need, group visit, hs size, ind. visit, last visit, schol. visit, scholarship status, total gift, total inst. gift and visit early (13 out of 40 vars)
# do both and compare


################################ OPTION 1
data1 <- data[data$Year %in% c("2014 Fall", "2015 Fall", "2016 Fall"), -c(17,19,26)]
data1fit <- data1[ ,c(28,3,4,7,8,11:21,23:27,30:37)] # drop dates, id, year, deposit, alumni pop

# impute missing values
tempData <- mice(data1fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data1fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(3,16)) data1fit.imp[,j] <- round(data1fit.imp[,j]*100)
data1fit.imp$Zip.Alumni.Density <- round(data1fit.imp$Zip.Alumni.Density*1000) # thousanths
data1fit.imp$Distance.Mhd <- round(data1fit.imp$Distance.Mhd)
data1fit.imp <- data1fit.imp[data1fit.imp$Final.Status!=0.5,]

# test and train sets
smp_size <- floor(0.75 * nrow(data1fit.imp))
set.seed(3333)
train_ind <- sample(seq_len(nrow(data1fit.imp)), size = smp_size)
train1 <- data1fit.imp[train_ind, ]
test1 <- data1fit.imp[-train_ind, ]

# fit model
fit1g <- glm(Final.Status ~ ., train1, family="binomial")
fit1gs <- step(fit1g, direction="both", trace=0)
coef1 <- exp(cbind(Odds.Ratio = coef(fit1gs), confint(fit1gs)))

test1$Predictions_raw <- unlist(predict(fit1gs, test1, type="response"))
test1$Predictions <- unlist(round(test1$Predictions_raw))

for(u in seq_along(test1$Predictions)){
    if(test1$Predictions[u] == test1$Final.Status[u]) { test1$Correct[u] <- "Correct" 
    } else if(test1$Predictions[u]==1) { test1$Correct[u] <- "False Positive"
    } else if(test1$Predictions[u]==0) { test1$Correct[u] <- "False Negative"
    }
}
test1$Correct <- unlist(test1$Correct)
table(test1$Correct) # 94.1% correct / power = 0.962



################################ OPTION 2
data2 <- data[ , c(1:8, 20:25, 27:39)]
data2fit <- data2[ ,c(19,3,4,7:12, 14:18,21:22)] # drop dates, id, year, deposit, alumni pop

# impute missing values
tempData <- mice(data2fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data2fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(3,9)) data2fit.imp[,j] <- round(data2fit.imp[,j]*100)
data2fit.imp$Zip.Alumni.Density <- round(data2fit.imp$Zip.Alumni.Density*1000) # thousanths
data2fit.imp$Distance.Mhd <- round(data2fit.imp$Distance.Mhd)
data2fit.imp <- data2fit.imp[data2fit.imp$Final.Status!=0.5,]

# test and train sets
smp_size <- floor(0.75 * nrow(data2fit.imp))
set.seed(3333)
train_ind <- sample(seq_len(nrow(data2fit.imp)), size = smp_size)
train2 <- data2fit.imp[train_ind, ]
test2 <- data2fit.imp[-train_ind, ]

# fit model
fit2g <- glm(Final.Status ~ ., train2, family="binomial")
fit2gs <- step(fit2g, direction="both", trace=0)
fit2gs2 <- glm(Final.Status ~ ACT + GPA + Number.Campus.Visits + Zip.Median.Income + 
                   Zip.Pct.White + Zip.Alumni.Density + Music.Interest + Athletic.Interest + 
                   Legacy.Relationship + Lutheran.Indicator, train2, family="binomial")

coef2 <- exp(cbind(Odds.Ratio = coef(fit2gs2), confint(fit2gs2)))

test2$Predictions_raw <- unlist(predict(fit2gs2, test2, type="response"))
test2$Predictions <- unlist(round(test2$Predictions_raw))
for(u in seq_along(test2$Predictions)){
    if(test2$Predictions[u] == test2$Final.Status[u]) { test2$Correct[u] <- "Correct" 
    } else if(test2$Predictions[u]==1) { test2$Correct[u] <- "False Positive"
    } else if(test2$Predictions[u]==0) { test2$Correct[u] <- "False Negative"
    }
}
test2$Correct <- unlist(test2$Correct)
table(test2$Correct) # 67.74% correct / power = .807


