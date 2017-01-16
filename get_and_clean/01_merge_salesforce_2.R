# 01_merge_salesforce.R
# read in raw data from financial aid, 
# visit and application objects from Salesforce
# combine into data set with one observation per applicant
# in: FinAidFactors.csv, AppFactors.csv (source Salesforce)
# out: salesforce_combined.csv

# read in raw data from Salesforce
setwd("/Users/kpavlik/git_projects/yield/data")

####################### FINANCIAL AID OBJECT ####################### 
finaid <- read.csv("FinAidFactors2.csv", stringsAsFactors=FALSE) 
for(o in 7:10) finaid[,o] <- as.Date(finaid[,o], format="%m/%d/%Y")
## Duplicates found. Keep the larger award.
finaid_condense <- NULL
for(fa in unique(finaid$Application.ID)){
    sub <- finaid[finaid$Application.ID==fa, ]
    sub <- sub[order(-sub$Total.Gift), ]
    keep <- sub[1,]
    finaid_condense <- rbind(finaid_condense, keep)
}
## GOOD.

####################### APPLICATION OBJECT ####################### 
app <- read.csv("AppFactors.csv", stringsAsFactors=FALSE)
for(i in 9:11) app[,i] <- as.Date(app[,i], format="%m/%d/%Y")
colnames(app) <- c("ID", "Year", "Contact.Status", "Admission.Substatus", "Major.Interest", 
                   "ACT", "GPA", "HS.Percentile", "Date.App.Submitted", "Date.App.Completed", "Date.Deposited", "Legacy", "Co.Curricular", "Gender", 
                   "Ethnicity", "Religious.Preference", "HS.Name", "City", "State", "Zip", "Number.Campus.Visits", 
                   "Royal15", "Royal.ID")
## GOOD.

####################### MERGE ####################### 
merge1 <- merge(app, finaid_condense, by.x="ID", by.y="Application.ID", all.x=TRUE)
# write.csv(merge1, "salesforce_combined1.csv", row.names=FALSE)



