# 01_merge_salesforce.R
# read in raw data from financial aid, 
# visit and application objects from Salesforce
# combine into data set with one observation per applicant
# in: FinAidFactors.csv, VisitFactors.csv, AppFactors.csv (source Salesforce)
# out: salesforce_combined.csv

# read in raw data from Salesforce
setwd("/Users/kaylinwalker/R/yield/data")

####################### FINANCIAL AID OBJECT ####################### 
finaid <- read.csv("FinAidFactors.csv", stringsAsFactors=FALSE) 
## Duplicates found. Keep the larger award.
finaid_condense <- NULL
for(fa in unique(finaid$Application.ID)){
    sub <- finaid[finaid$Application.ID==fa, ]
    sub <- sub[order(-sub$Total.Gift), ]
    keep <- sub[1,]
    finaid_condense <- rbind(finaid_condense, keep)
}
## GOOD.


####################### VISIT OBJECT ####################### 
visit <- read.csv("VisitFactors.csv", stringsAsFactors=FALSE)
visit$Visit.Date <- as.Date(visit$Visit.Date, format="%m/%d/%Y")
visit_condense <- NULL
for(w in unique(visit$Active.Application.Record)) {
    sub <- visit[visit$Active.Application.Record==w, ]
    sub <- sub[order(sub$Visit.Date), ]
    Summer <- 0
    Countdown <- 0
    # recording up to 4 visits accounts for 99% of apps
    First.Visit <- NA
    Second.Visit <- NA
    Third.Visit <- NA
    Last.Visit <- NA
    Number.Campus.Visits <- 0 
    if("Summer Orientation" %in% sub$Campus.Visit..Record.Type) Summer <-1
    if("Cobber Countdown" %in% sub$Campus.Visit..Record.Type) Countdown <- 1
    sub2 <- sub[!(sub$Campus.Visit..Record.Type %in% c("Cobber Countdown", "Summer Orientation")), ]
    if(nrow(sub2)>0) { 
      Number.Campus.Visits <- nrow(sub2)  
      First.Visit <- sub2$Visit.Date[1]
      Last.Visit <- sub2$Visit.Date[length(sub2[,1])]
      if(Number.Campus.Visits > 2){
           Second.Visit <- sub2$Visit.Date[2]
           if(Number.Campus.Visits > 3){
                Third.Visit <- sub2$Visit.Date[3]
           }
      }
    }
    row <- data.frame(ID=w, First.Visit, Second.Visit, Third.Visit, Last.Visit, Summer, Countdown, Number.Campus.Visits)
    for(i in c(2:5)) row[,i] <- as.Date(row[,i], format="%Y-%m-%d")
    visit_condense <- rbind(visit_condense, row)
    for(i in c(2:5)) visit_condense[,i] <- as.Date(visit_condense[,i], format="%Y-%m-%d")
}

## GOOD. 

####################### APPLICATION OBJECT ####################### 
app <- read.csv("AppFactors.csv", stringsAsFactors=FALSE)
colnames(app) <- c("ID", "Year", "Contact.Status", "Admission.Substatus", "Major.Interest", 
                   "ACT", "GPA", "HS.Percentile", "Date.App.Submitted", "Date.App.Completed", "Date.Deposited", "Legacy", "Co.Curricular", "Gender", 
                   "Ethnicity", "Religious.Preference", "HS.Name", "City", "State", "Zip", "Number.Campus.Visits", 
                   "Royal15", "Royal.ID")
## GOOD.

####################### MERGE ####################### 
merge1 <- merge(app, visit_condense, by="ID", all.x=TRUE)
merge2 <- merge(merge1, finaid_condense, by.x="ID", by.y="Application.ID", all.x=TRUE)
# write.csv(merge2, "salesforce_combined.csv", row.names=FALSE)



