setwd("/Users/kwalker/git_projects/yield")
app <- read.csv("AppFactors.csv", stringsAsFactors=FALSE)
finaid <- read.csv("FinAidFactors.csv", stringsAsFactors=FALSE)
visit <- read.csv("VisitFactors.csv", stringsAsFactors=FALSE)
data <- merge(finaid, visit, by.x="Application.ID", by.y="Active.Application.Record", all=TRUE)
data <- merge(data, app, by.x="Application.ID", by.y="Active.Application.Record..Application.ID", all=TRUE)
rm(app); rm(finaid); rm(visit)

colnames(data) <- c("ID", "Award.Status", "Total.Discount.Rate", "Total.Institutional.Gift",
                    "Gross.Need", "Cobber.Edge.Award", "Priority.Cobber.Award", "Presidential.Total", "Regents.Total",
                    "Speech.Total", "Theatre.Total", "Arts.Total", "Music.Total", "Visit.Date", 
                    "Year", "Contact.Status", "Admission.Substatus", "Academic.Programs",
                    "ACT", "GPA", "Date.App.Submitted", "Date.App.Completed", "Legacy.Relationships", "Co.Curriculars", 
                    "Gender", "Ethnicity", "Religion", "HS.Name", "City", "State", "Zip", "Number.Visits", 
                    "Royall", "Royall.ID")
data <- data[ , c(1,15:17,21,22,25:31,23,19:20,33,34,18,24,32,14,2:13)]
# remove anyone w/o an ID
data <- data[data$ID!="", ]
data$Contact.Status <- gsub("\\: ", "", data$Contact.Status)
# clean up a few items
for(j in seq_along(data[,1])){
    # clean legacy
    if(grepl("Father|Mother|Parent|Parents", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Parent" 
    } else if(grepl("Aunt|Uncle|Cousin|Grandparent|Other", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Ext. Family"
    } else if(grepl("Brother|Sister", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Sibling"
    }
    # clean award status
    if(is.na(data$Award.Status[j])) data$Award.Status[j] <- "No Award" 
    # clean finaid
    for(u in c(26:36)) { 
        if(is.na(data[j,u])) data[j,u] <- 0
    }
    # clean Royall
    if(!is.na(data$Royall.ID[j])) {
        data$Royall[j] <- 1
    }
    
}
data2 <- NULL
for(u in unique(data$ID)){
    subset <- data[grep(u, data$ID), ]
    num <- length(subset[,1])
    subset$First.Visit.Date <- subset$Visit.Date[1]
    subset$Last.Visit.Date <- subset$Visit.Date[num]
    subset <- subset[1,-24]
    data2 <- rbind(data2, subset)
}
data <- data2
rm(data2)

# format 
for(j in c(7,8,36,37)) data[,j] <- as.Date(data[,j], format="%m/%d/%Y")
for(j in c(4:6,9:11,24)) data[,j] <- as.factor(data[,j])
