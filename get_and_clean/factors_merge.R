library(stringr)

setwd("/Users/kwalker/git_projects/yield")
app <- read.csv("data/AppFactors.csv", stringsAsFactors=FALSE)
finaid <- read.csv("data/FinAidFactors.csv", stringsAsFactors=FALSE)
visit <- read.csv("data/VisitFactors.csv", stringsAsFactors=FALSE)

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

# keep only fall entrances
data <- data[grepl("Fall", data$Year), ]

# clean up a few items
data$Contact.Status <- gsub("\\: ", "", data$Contact.Status)
data$Co.Curriculars <- tolower(data$Co.Curriculars)
data$Academic.Programs <- tolower(data$Academic.Programs)
data$Zip <- substring(data$Zip, 1, 5)


# create new variables
# look at all co-curriculars available
all <- str_c(data$Co.Curriculars, collapse=" ")
all <- gsub("\\;", " ", all)
all <- factor(unique(unlist(strsplit(all, " "))))
data$Music.Interest <- 0
data$Athletic.Interest <- 0
data$Legacy <- 0 

# add hs size
hs <- read.csv("data/factors_schoolsize.csv", stringsAsFactors=FALSE)
data$HS.Name <- gsub("HS", "High School", data$HS.Name)
data$HS.Size <- NA

# add zip code features
data$Distance.Mhd <- NA
data$Zip.Median.Income <- NA
data$Zip.Population <- NA
data$Zip.Pct.White <- NA
# save out .csv
zips <- unique(data$Zip)
# write.csv(zips, "data/factor_zips.csv", row.names=FALSE)
zip_data <- read.csv("data/factors_zip_data.csv", stringsAsFactors=FALSE)
zip_data <- zip_data[zip_data$zip %in% zips, ]

for(j in seq_along(data[,1])){
    # clean legacy
    if(grepl("Father|Mother|Parent|Parents|Stepfather|Stepmother", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Parent" 
    } else if(grepl("Aunt|Uncle|Cousin|Grandparent|Other", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Ext. Family"
    } else if(grepl("Brother|Sister", data$Legacy.Relationships[j])) { 
        data$Legacy.Relationships[j] <- "Sibling"
    }
    if(data$Legacy.Relationships[j]!="") data$Legacy[j] <- 1
    
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
    
    # mark music or athl interest
    if(grepl("music|band|orchestra|choir|choral", data$Co.Curriculars[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("music|band|orchestra|choir|choral", data$Academic.Programs[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("baseball|basketball|hockey|track|field|wrestling|cross country|golf|
             softball|diving|tennis|volleyball", data$Co.Curriculars[j])) { data$Athletic.Interest[j] <- 1 } 
    
    if(data$HS.Name[j] %in% hs$school) {
        data$HS.Size[j] <- hs[hs$school==data$HS.Name[j], 2]
    }
    
    if(data$Zip[j] %in% zip_data$zip){
        data$Distance.Mhd[j] <- zip_data[zip_data$zip==data$Zip[j], 2]
        data$Zip.Median.Income[j] <- zip_data[zip_data$zip==data$Zip[j], 3]
        data$Zip.Population[j] <- zip_data[zip_data$zip==data$Zip[j], 4]
        data$Zip.Pct.White[j] <- zip_data[zip_data$zip==data$Zip[j], 6]
    }
    
}
# new variable
for(j in c(5,6)) data[,j] <- as.Date(data[,j], format="%m/%d/%Y")
data$Visit.Date <- as.Date(data$Visit.Date, format="%m/%d/%y")
data$Time.to.Complete <- as.numeric(data$Date.App.Completed - data$Date.App.Submitted)
data <- data[ , -18] # remove the extrz Royall var

# keep only the first and last visit dates
data2 <- NULL
for(u in unique(data$ID)){
    subset <- data[grep(u, data$ID), ]
    num <- length(subset[,1])
    subset$First.Visit.Date <- subset$Visit.Date[1]
    subset$Last.Visit.Date <- subset$Visit.Date[num]
    subset <- subset[1,-21]
    data2 <- rbind(data2, subset)
}
data <- data2
rm(data2)
#write.csv(data, "factors.csv", row.names=FALSE)
