# 01_merge_salesforce.R
# read in raw data from financial aid, scholarship, 
# visit and application objects from Salesforce
# combine into data set with one observation per applicant
# in: FinAidFactors.csv, ScholFactors.csv, VisitFactors.csv, AppFactors.csv (source Salesforce)
# out: salesforce_combined.csv

# read in raw data from Salesforce
setwd("/Users/kwalker/git_projects/yield/data")

####################### FINANCIAL AID OBJECT ####################### 
finaid <- read.csv("FinAidFactors.csv", stringsAsFactors=FALSE) 
## DUPES!!!!
finaid_condense <- NULL
for(fa in unique(finaid$Application.ID)){
    sub <- finaid[finaid$Application.ID==fa, ]
    sub <- sub[order(-sub$Total.Gift), ]
    keep <- sub[1,]
    finaid_condense <- rbind(finaid_condense, keep)
}
# GOOD.
####################### SCHOLARSHIP OBJECT ####################### 
schol <- read.csv("ScholFactors.csv", stringsAsFactors=FALSE)
# create one variable from all statuses where 3 = winner, 2 = non-winner, 1 = non-qualifed/cancel
schol$Status.Placeholder <- paste(schol$Banner.MPS.Status, schol$Banner.PDS.Status, 
                            schol$Banner.Theatre.Scholarship.Status,
                            schol$Banner.SpeechDebate.Scholarship.Status, 
                            schol$Banner.VisualArts.Scholarship.Status)
for(i in seq_along(schol[,1])) {
    if (grepl("W", schol$Status.Placeholder[i])) { 
        schol$Status[i] <- "W" 
      } else if (grepl("C", schol$Status.Placeholder[i])) { 
          schol$Status[i] <- "C" 
      } else if (grepl("N", schol$Status.Placeholder[i])) { 
          schol$Status[i] <- "N" 
      } else { schol$Status[i] <- NA }                                               
}
schol <- schol[,c(1,2,10)]
schol <- schol[!is.na(schol$Status),]
schol_condense <- NULL
for(j in unique(schol$Application..Application.ID)){
    student <- schol[schol$Application..Application.ID==j, ]
    status <- data.frame(table(student[,3]))
    if(status$Var1[1]=="W") stat <- 3
    if(status$Var1[1]=="N") stat <- 2
    if(status$Var1[1]=="C") stat <- 1
    row <- data.frame(ID=j, Scholarship.Status=stat)
    schol_condense <- rbind(schol_condense, row)
}
## GOOD.
####################### VISIT OBJECT ####################### 
visit <- read.csv("VisitFactors.csv", stringsAsFactors=FALSE)
# condense visit type
visit$Campus.Visit..Record.Type <- gsub("April Junior Visit Day|August Visit Day|EDMN/NDEA|July Visit Day|May Visit Day|MPCW|Veterans Day|Visit Day: August Visit Day|Visit Day: MLK Jr|Visit Day: Presidents' Day|Cobber Celebration|Football Visit|Road Trip|Ensemble Tour|Cobber Countdown|BrewU|Special Event|Christmas Concerts", "Group Visit" ,visit$Campus.Visit..Record.Type)
visit$Campus.Visit..Record.Type <- gsub("Music Performance Scholarship|Presidential Distinction Scholarship|Regents Scholarship Event|Speech and Debate Scholarship|Theatre Performance Scholarship|Theatre Peformance Scholarship|Visual Arts Scholarship", "Scholarship Competition" ,visit$Campus.Visit..Record.Type)
visit$Campus.Visit..Record.Type <- gsub("Coffee Conversations", "Other", visit$Campus.Visit..Record.Type)
visit$Visit.Date <- as.Date(visit$Visit.Date, format="%m/%d/%Y")
visit_condense <- NULL
for(w in unique(visit$Active.Application.Record)) {
    sub <- visit[visit$Active.Application.Record==w, ]
    sub <- sub[order(sub$Visit.Date), ]
    first <- sub$Visit.Date[1]
    last <- sub$Visit.Date[length(sub[,1])]
    type <- data.frame(table(sub$Campus.Visit..Record.Type))
    vt1 <- 0; vt2 <- 0; vt3 <- 0
    for(g in seq_along(type[,1])) {
        if(type$Var1[g]=="Daily Campus Visit") vt1 <- type$Freq[g]
        if(type$Var1[g]=="Group Visit") vt2 <- type$Freq[g]
        if(type$Var1[g]=="Scholarship Competition") vt3 <- type$Freq[g]
    }
    row <- data.frame(ID=w, First.Visit=first, Last.Visit=last, Ind.Visit=vt1, Group.Visit=vt2, Schol.Visit=vt3)
    visit_condense <- rbind(visit_condense, row)
}
# GOOD. 
####################### APPLICATION OBJECT ####################### 
app <- read.csv("AppFactors.csv", stringsAsFactors=FALSE)
colnames(app) <- c("ID", "Type", "International", "Year", "Contact.Status", "Admission.Substatus", "Major.Interest", 
                   "ACT", "GPA", "Date.App.Submitted", "Date.App.Completed", "Legacy", "Co.Curricular", "Gender", 
                   "Ethnicity", "Religious.Preference", "HS.Name", "City", "State", "Zip", "Number.Campus.Visits", 
                   "Royal15", "Royal.ID")
# remove anyone who isn't a domestic first year
app <- app[app$Type=="First Year" & app$International==0, ]
app <- app[,c(1,4:23)]

####################### MERGE ####################### 
merge1 <- merge(app, visit_condense, by="ID", all.x=TRUE)
merge2 <- merge(merge1, finaid_condense, by.x="ID", by.y="Application.ID", all.x=TRUE)
salesforce <- merge(merge2, schol_condense, by="ID", all.x=TRUE)
# write.csv(salesforce, "salesforce_combined.csv", row.names=FALSE)


