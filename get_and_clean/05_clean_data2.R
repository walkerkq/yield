# 05_clean_data2.R
# standardize and clean data for analysis
# create indiator variables for factors
# in: all_combined1.csv
# out: date_data.csv, non_date_data.csv

setwd("/Users/kpavlik/git_projects/yield/")
data <- read.csv("data/all_combined1.csv", stringsAsFactors=FALSE)

# static items only
# clean up
data <- data[data$ID!="", ] # remove anyone w/o an ID
data <- data[grepl("Fall", data$Year), ] # keep only fall entrances
data$Contact.Status <- gsub("\\: ", "", data$Contact.Status)
data$Co.Curricular <- tolower(data$Co.Curricular)
data$Major.Interest <- tolower(data$Major.Interest)
data <- data[!is.na(data$Date.App.Submitted), ]

# create indicator variables
data$Music.Interest <- 0
data$Athletic.Interest <- 0
data$Legacy.Relationship <- 0 
data$Gender.Indicator <- 0
data$Final.Status <- 0
data$Ethnicity.Indicator <- 0
data$Lutheran.Indicator <- 0
data$Religious.Indicator <- 0

# cycle thru
data$Legacy.Relationship <- ifelse(data$Legacy!="", 1, 0)
data$Final.Status <- ifelse(data$Contact.Status %in% "Deposit", 1, 0)
data$Gender.Indicator <- ifelse(data$Gender %in% "Male", 1, 0)
data$Music.Interest <- ifelse(grepl("music|band|orchestra|choir|choral", data$Co.Curricular), 1, 
                              ifelse(grepl("music|band|orchestra|choir|choral", data$Major.Interest), 1, 0))
data$Athletic.Interest <- ifelse(grepl("baseball|basketball|hockey|track|field|wrestling|cross country|golf|
         softball|diving|tennis|volleyball", data$Co.Curricular), 1, 0)
data$Total.Gift[is.na(data$Total.Gift)] <- 0
data$Total.Institutional.Gift[is.na(data$Total.Institutional.Gift)] <- 0
data$Gross.Need[is.na(data$Gross.Need)] <- 0
data$Ethnicity.Indicator <- ifelse(grepl("Indian|Native|Black|African|Pacific|Asian|Two|Hispanic|Multiracial", data$Ethnicity), 1, 0)
data$Lutheran.Indicator <- ifelse(grepl("Lutheran|ELCA|Luthern|Lutheren", data$Religious.Preference), 1, 0)
data$Religious.Indicator <- ifelse(!grepl("Unknown|None|No Affiliation|No religious affiliation|Not Reported|Prefer not to respond", data$Religious.Preference), 1, 0)
data$Religious.Indicator[data$Religious.Preference==""] <- 0
data$Royal15[!is.na(data$Royal.ID)] <- 1

remove <- c("Major.Interest", "Admission.Substatus", "Contact.Status", "Legacy", "Co.Curricular", "Gender", "Award.Status",
            "Gender", "Ethnicity", "Religious.Preference", "City", "State", "Number.Campus.Visits", "Royal.ID", "HS.Name", "Zip",
            "Zip.Alumni.Pop", "Cost.of.Attendance", "Total.Gift", "Total.Institutional.Gift", "Gross.Need")

data <- data[ , which(!(names(data) %in% remove))]

date_nums <- c(6:8, 10:13)
non_date_data <- data[ ,-date_nums]
non_date_data <- non_date_data[,c(17, 1:16, 18:20)]
# write.csv(non_date_data, "data/non_date_data.csv", row.names=F)

# date-dependent data
date_data <- data[ , c(1, date_nums)]
# write.csv(date_data, "data/date_data.csv", row.names=F)
