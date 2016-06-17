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

# check for collinearity
collin <- round(cor(data[,c(1,4:34)], use = "pair"), 2) 
# don't use made deposit, total.inst.gift, comp.cost
data <- data[ ,-c(12,14,27)]

# option 1: drop 2010-2012 + impute missing values (+ drop schol. status, hs size and total inst. gift)
# option 2: drop award status, comp. cost, first visit, gross need, group visit, hs size, ind. visit, last visit, schol. visit, scholarship status, total gift, total inst. gift and visit early (13 out of 40 vars)
# do both and compare

data$Final.Status <- as.numeric(gsub(0.5, 0, data$Final.Status))

################################ OPTION 1
data1 <- data[data$Year %in% c("2014 Fall", "2015 Fall", "2016 Fall"), -2]
data1fit <- data1[ , -c(7:9,13,19)] # drop ind.visit, schol.visit, group.visit, high school size, inst. gift, schol. status

# impute missing values
tempData <- mice(data1fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data1fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(4,8,9,10,11,12,13,14,24,25)) data1fit.imp[,j] <- round(data1fit.imp[,j])

# test and train sets
smp_size <- floor(0.75 * nrow(data1fit.imp))
set.seed(3333)
train_ind <- sample(seq_len(nrow(data1fit.imp)), size = smp_size)
train1 <- data1fit.imp[train_ind, ]
test1 <- data1fit.imp[-train_ind, ]

# fit model
fit1g <- glm(Final.Status ~ ., train1[,-2], family="binomial")
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
table(test1$Correct) # 92.2% correct / power = 0.954 / 0.031 alpha



################################ OPTION 2
data2 <- data[ , -c(8:14,20,34)]
data2fit <- data2[ ,-c(2,3)] # drop id, year

# impute missing values
tempData <- mice(data2fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data2fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(3,6:10,20,21)) data2fit.imp[,j] <- round(data2fit.imp[,j])

# test and train sets
smp_size <- floor(0.75 * nrow(data2fit.imp))
set.seed(3333)
train_ind <- sample(seq_len(nrow(data2fit.imp)), size = smp_size)
train2 <- data2fit.imp[train_ind, ]
test2 <- data2fit.imp[-train_ind, ]

# fit model
fit2g <- glm(Final.Status ~ ., train2, family="binomial")
fit2gs <- step(fit2g, direction="both", trace=0)

coef2 <- exp(cbind(Odds.Ratio = coef(fit2gs), confint(fit2gs)))

test2$Predictions_raw <- unlist(predict(fit2gs, test2, type="response"))
test2$Predictions <- unlist(round(test2$Predictions_raw))
for(u in seq_along(test2$Predictions)){
    if(test2$Predictions[u] == test2$Final.Status[u]) { test2$Correct[u] <- "Correct" 
    } else if(test2$Predictions[u]==1) { test2$Correct[u] <- "False Positive"
    } else if(test2$Predictions[u]==0) { test2$Correct[u] <- "False Negative"
    }
}
test2$Correct <- unlist(test2$Correct)
table(test2$Correct) # 78.7% correct / power = .837 / alpha = .0496


