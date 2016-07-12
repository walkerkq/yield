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
ggplot(missing2, aes(Variable, value)) + geom_bar(stat="identity", position="dodge", aes(fill=variable)) +
    scale_fill_brewer(palette="Paired") + ylab("Percent Missing") + xlab("") + theme_classic() + geom_hline(yintercept=.4) + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=12)) + labs(title="Missingness by Variable")

# check for collinearity
collin <- round(cor(data[,2:41], use = "pair"), 2) 
# total gift + FAFSA + award status; summer and made.deposit + final.status

data1fit <- data[ , -c(6,9,16,18,24,30)] # drop hs size, made deposit, award status, counts, summer o

# impute missing values
tempData <- mice(data1fit, m=1, maxit=50, meth='pmm', seed=500)
summary(tempData)
densityplot(tempData)
data1fit.imp <- complete(tempData, 1)

# fix binomial issues
for(j in c(2,8:14,25)) data1fit.imp[,j] <- round(data1fit.imp[,j])

# test and train sets
smp_size <- floor(0.75 * nrow(data1fit.imp))
set.seed(3333)
train_ind <- sample(seq_len(nrow(data1fit.imp)), size = smp_size)
#train1 <- data1fit.imp[train_ind, ]
#test1 <- data1fit.imp[-train_ind, ]

train1 <- data1fit.imp[data1fit.imp$Year!="2016 Fall", ]
test1 <- data1fit.imp[data1fit.imp$Year=="2016 Fall", ]

# fit model
fit1g <- glm(Final.Status ~ ., train1[,-1], family="binomial")
fit1gs <- step(fit1g, direction="both", trace=0)

fit1gs2 <- glm(formula = Final.Status ~ ACT + GPA + Royal15 + 
                   Number.Campus.Visits.y + Total.Gift + Gross.Need + Distance.Mhd + 
                   Zip.Median.Income + Zip.Alumni.Density + Legacy.Relationship + 
                   Ethnicity.Indicator + Lutheran.Indicator + 
                   Visit.Before.App + FAFSA + Applied.Summer + 
                   Applied.Fall + Applied.Winter + Applied.Before.Sr + First.Visit.Before.Sr + 
                   First.Visit.Fall + First.Visit.Winter + First.Visit.Spring + 
                   First.Visit.Summer, family = "binomial", data = train1[,-1])

fit1gs3 <- glm(formula = Final.Status ~ ACT + GPA + Royal15 + 
                   Number.Campus.Visits.y + Total.Gift + Gross.Need + Distance.Mhd + 
                   Zip.Median.Income + Zip.Alumni.Density + Legacy.Relationship + 
                   Ethnicity.Indicator + Lutheran.Indicator + 
                   Visit.Before.App + FAFSA, family = "binomial", data = train1[,-1])

coef1 <- exp(cbind(Odds.Ratio = coef(fit1gs2), confint(fit1gs2)))

test1$Predictions_raw <- unlist(predict(fit1gs2, test1, type="response"))
test1$Predictions <- unlist(round(test1$Predictions_raw))

table(test1$Predictions, test1$Final.Status)

for(u in seq_along(test1$Predictions)){
    if(test1$Predictions[u] == test1$Final.Status[u]) { test1$Correct[u] <- "Correct" 
    } else if(test1$Predictions[u]==1) { test1$Correct[u] <- "False Positive"
    } else if(test1$Predictions[u]==0) { test1$Correct[u] <- "False Negative"
    }
}
test1$Correct <- unlist(test1$Correct)
table(test1$Correct) 



