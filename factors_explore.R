data <- read.csv("factors.csv", stringsAsFactors=FALSE)
data <- data[,2:43]

# FORMAT
for(j in c(5,6,42,43)) data[,j] <- as.Date(data[,j], format="%Y-%m-%d")
for(j in c(2,3,4,7,8,9,21)) data[,j] <- as.factor(data[,j])

# EXPLORE 



