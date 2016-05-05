setwd("/Users/kwalker/git_projects/yield")
data <- read.csv("min_factors.csv", stringsAsFactors=FALSE)
library(reshape2)
library(ggplot2)

get_prop <- function(x, y){
    t <- prop.table(table(x, y), 2)
    t <- melt(t)
    ggplot(t, aes(y, value)) + geom_bar(stat="identity", aes(fill=x)) + geom_label(aes(label=round(value,2)))
}
get_prop(data$Final.Status, data$Number.Campus.Visits)

a <- aggregate(Gender.Indicator ~ Year + Final.Status, data, sum)
a$Prop <- 0
for(r in seq_along(a[,1])){
    tote <- sum(a[a[,1]==a[r,1],3])
    a[r,4] <- a[r,3]/tote
}

ggplot(a, aes(Year, Prop)) + geom_bar(stat="identity", position="dodge", aes(fill=as.factor(Final.Status)))


ivs <- data[data$Year=="2015 Fall" | data$Year=="2014 Fall" , c(3,4,7,11:14,19:31,33:40)]
ivs <- ivs[complete.cases(ivs),]
fit <- lm(Final.Status ~ ., ivs)
summary(fit)
step2 <- step(fit)
summary(step2)

exp(cbind(Odds.Ratio = coef(stepm), confint(stepm)))

exp(cbind(Odds.Ratio = coef(fit), confint(fit)))
cbind(Odds.Ratio = coef(fit), confint(fit))





