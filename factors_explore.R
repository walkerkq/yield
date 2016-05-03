setwd("/Users/kwalker/git_projects/yield")
data <- read.csv("min_factors.csv", stringsAsFactors=FALSE)
library(reshape2)
library(ggplot2)

get_prop <- function(x, y){
    t <- prop.table(table(x, y), 2)
    t <- melt(t)
    ggplot(t, aes(y, value)) + geom_bar(stat="identity", aes(fill=x)) + geom_label(aes(label=round(value,2)))
}


a <- aggregate(Gender.Indicator ~ Year + Final.Status, data, sum)
a$Prop <- 0
for(r in seq_along(a[,1])){
    tote <- sum(a[a[,1]==a[r,1],3])
    a[r,4] <- a[r,3]/tote
}

ggplot(a, aes(Year, Prop)) + geom_bar(stat="identity", position="dodge", aes(fill=as.factor(Final.Status)))

fit <- glm(Final.Status ~ GPA + Number.Visits + Award.Status + Total.Discount.Rate, data, family="binomial")
summary(fit)
exp(cbind(Odds.Ratio = coef(fit), confint(fit)))
cbind(Odds.Ratio = coef(fit), confint(fit))
