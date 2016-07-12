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
data$Gender.Indicator <- 0
data$Final.Status <- 0
data$Made.Deposit <- 0
data$Ethnicity.Indicator <- 0
data$Lutheran.Indicator <- 0
data$Religious.Indicator <- 0

# clean up aid variables
data$Total.Scholarship <- data$Cobber.Edge.Award + data$Priority.Cobber.Award + data$Presidential.Total + 
    data$Regents.Total + data$Speech.Total + data$Theatre.Total + data$Arts.Total + data$Music.Total
data$Aid <- data$Total.Institutional.Gift - data$Total.Scholarship


# add hs size
hs <- read.csv("data/factors_schoolsize.csv", stringsAsFactors=FALSE)
data$HS.Name <- gsub("HS", "High School", data$HS.Name)
data$HS.Size <- NA

# add zip code features
data$Distance.Mhd <- NA
data$Zip.Median.Income <- NA
data$Zip.Population <- NA
data$Zip.Pct.White <- NA
data$Zip.Alumni.Pop <- NA
data$Zip.Alumni.Density <- NA

zips <- unique(data$Zip)
zip_data <- read.csv("data/factors_zip_data2.csv", stringsAsFactors=FALSE)
zip_data <- zip_data[zip_data$zip %in% zips, ]

# create indicator variables line by line

for(j in seq_along(data[,1])){
    
    # clean finaid
    for(u in c(26:36)) { 
        if(is.na(data[j,u])) data[j,u] <- 0
    }
 
    #### combine HS info
    if(data$HS.Name[j] %in% hs$school) {
        data$HS.Size[j] <- hs[hs$school==data$HS.Name[j], 2]
    }
    
    #### combine zip info
    if(data$Zip[j] %in% zip_data$zip){
        data$Distance.Mhd[j] <- zip_data[zip_data$zip==data$Zip[j], 2]
        data$Zip.Median.Income[j] <- zip_data[zip_data$zip==data$Zip[j], 3]
        data$Zip.Population[j] <- zip_data[zip_data$zip==data$Zip[j], 4]
        data$Zip.Pct.White[j] <- zip_data[zip_data$zip==data$Zip[j], 6]
        data$Zip.Alumni.Pop[j] <- zip_data[zip_data$zip==data$Zip[j], 7]
        data$Zip.Alumni.Density[j] <- zip_data[zip_data$zip==data$Zip[j], 8]
    }
    
    ######### create indicator variables
    if(data$Legacy.Relationships[j]!="") data$Legacy[j] <- 1
    
    if(grepl("music|band|orchestra|choir|choral", data$Co.Curriculars[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("music|band|orchestra|choir|choral", data$Academic.Programs[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("baseball|basketball|hockey|track|field|wrestling|cross country|golf|
             softball|diving|tennis|volleyball", data$Co.Curriculars[j])) { data$Athletic.Interest[j] <- 1 } 

    if(data$Gender[j] == "Male") { 
        data$Gender.Indicator[j] <- 1 
    } else if (data$Gender[j] == "Female") {
        data$Gender.Indicator[j] <- 0 
    } else { 
        data$Gender.Indicator[j] <- NA 
    }

    if(data$Contact.Status[j]=="Deposit") {
        data$Final.Status[j] <- 1
    } else if(data$Contact.Status[j]=="Admit") {
        data$Final.Status[j] <- 0.5
    }
   
    if(data$Admission.Substatus[j]=="Cancel: Deposit"){
        data$Made.Deposit[j] <- 1
    } else if(data$Contact.Status[j]=="Deposit"){
        data$Made.Deposit[j] <- 1
    }
    
    if(is.na(data$Award.Status[j])) { 
        data$Award.Status[j] <- 0 
    } else if(data$Award.Status[j]=="No Award") {
        data$Award.Status[j] <- 0 
    } else if(data$Award.Status[j]=="Award Complete") {
        data$Award.Status[j] <- 1 
    }
    
    if(grepl("Indian|Native|Black|African|Pacific|Asian|Two|Hispanic|Multiracial", data$Ethnicity[j])){
        data$Ethnicity.Indicator[j] <- 1
    }
    
    if(grepl("Lutheran|ELCA|Luthern|Lutheren", data$Religion[j])) {
        data$Lutheran.Indicator[j] <- 1
    }
    if(!grepl("Unknown|None|No Affiliation|No religious affiliation|Not Reported|Prefer not to respond", data$Religion[j])) {
        data$Religious.Indicator[j] <- 1
    }
    if(data$Religion[j]=="") data$Religious.Indicator[j] <- 0
    
    if(!is.na(data$Royall.ID[j])) {
        data$Royall[j] <- 1
    }

}
# new variable
for(j in c(5,6)) data[,j] <- as.Date(data[,j], format="%m/%d/%Y")
data$Visit.Date <- as.Date(data$Visit.Date, format="%m/%d/%y")
data$Time.to.Complete <- as.numeric(data$Date.App.Completed - data$Date.App.Submitted)
data$Date.App.Submitted.Month <- format(data$Date.App.Submitted, origin="%Y-%m-%d", format="%m")
data$Date.App.Completed.Month <- format(data$Date.App.Completed, origin="%Y-%m-%d", format="%m")
data <- data[ , -18] # remove the extrz Royall var

# keep only the first and last visit dates
data2 <- NULL
for(u in unique(data$ID)){
    subset <- data[grep(u, data$ID), ]
    subset <- subset[order(subset$Visit.Date), ]
    num <- length(subset[,1])
    subset$First.Visit.Date <- subset$Visit.Date[1]
    subset$Last.Visit.Date <- subset$Visit.Date[num]
    subset$First.Visit.Date.Month <- format(subset$First.Visit.Date, origin="%Y-%m-%d", format="%m")
    subset$Last.Visit.Date.Month <- format(subset$Last.Visit.Date, origin="%Y-%m-%d", format="%m")
    subset <- subset[1,-21]
    data2 <- rbind(data2, subset)
}


data <- data2
rm(data2)
data <- data[,-1]
#write.csv(data, "full_factors.csv", row.names=FALSE)

keeps <- c(1,12,14:16,19:23,32:52,55:56)
data2 <- data[,keeps]
keeps_ordered <- c(1,15,16,30,31,29,6,32,33,10,7,9,8,20,21,13,3,4,11,12,5,14,17,19,18,2,23,22,24,25,26,27,28)
data2 <- data2[,keeps_ordered]

#write.csv(data2, "min_factors.csv", row.names=FALSE)
