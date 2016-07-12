# 05_clean_data.R
# standardize and clean data for analysis
# create indiator variables for factors
# in: all_combined.csv
# out: full_factors.csv, min_factors.csv

setwd("/Users/kwalker/git_projects/yield/")
data <- read.csv("data/all_combined.csv", stringsAsFactors=FALSE)

# clean up
data <- data[data$ID!="", ] # remove anyone w/o an ID
data <- data[grepl("Fall", data$Year), ] # keep only fall entrances
data$Contact.Status <- gsub("\\: ", "", data$Contact.Status)
data$Co.Curricular <- tolower(data$Co.Curricular)
data$Major.Interest <- tolower(data$Major.Interest)
data$Date.App.Submitted <- as.Date(data$Date.App.Submitted, format="%m/%d/%Y")
data$First.Visit <- as.Date(data$First.Visit, format="%Y-%m-%d")

data <- data[!is.na(data$Date.App.Submitted), ]

# create indicator variables
data$Music.Interest <- 0
data$Athletic.Interest <- 0
data$Legacy.Relationship <- 0 
data$Gender.Indicator <- 0
data$Final.Status <- 0
data$Made.Deposit <- 0
data$Ethnicity.Indicator <- 0
data$Lutheran.Indicator <- 0
data$Religious.Indicator <- 0
data$Visit.Before.App <- 0
data$FAFSA <- 0
data$Zip.Admits.TY <- 0
data$Zip.Admits.TY.Pct <- 0
data$Applied.Summer <- 0
data$Applied.Fall <- 0
data$Applied.Winter <- 0
data$Applied.Spring <- 0
data$Applied.Before.Sr <- 0
data$First.Visit.Before.Sr <- 0
data$First.Visit.Fall <- 0
data$First.Visit.Winter <- 0
data$First.Visit.Spring <- 0
data$First.Visit.Summer <- 0

zip.admits <- data.frame(table(data$Year, data$Zip))
colnames(zip.admits) <- c("Year", "Zip", "Count")
zip.deposits <- data.frame(table(data$Year[data$Contact.Status=="Deposit"], data$Zip[data$Contact.Status=="Deposit"]))
colnames(zip.deposits) <- c("Year", "Zip", "Count")
year.ref <- data.frame(TY=c("2015 Fall", "2014 Fall", "2013 Fall", "2012 Fall", "2011 Fall", "2010 Fall"), LY=c("2014 Fall", "2013 Fall", "2012 Fall", "2011 Fall", "2010 Fall", NA), stringsAsFactors=FALSE)

for(j in seq_along(data[,1])){
    
    if(data$Legacy[j]!="") data$Legacy.Relationship[j] <- 1
    
    if(grepl("music|band|orchestra|choir|choral", data$Co.Curricular[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("music|band|orchestra|choir|choral", data$Major.Interest[j])) { data$Music.Interest[j] <- 1 }
    if(grepl("baseball|basketball|hockey|track|field|wrestling|cross country|golf|
             softball|diving|tennis|volleyball", data$Co.Curricular[j])) { data$Athletic.Interest[j] <- 1 } 
    
    if(data$Gender[j] == "Male") { 
        data$Gender.Indicator[j] <- 1 
    } else if (data$Gender[j] == "Female") {
        data$Gender.Indicator[j] <- 0 
    } else { 
        data$Gender.Indicator[j] <- NA 
    }
    
    if(data$Contact.Status[j]=="Deposit") {
        data$Final.Status[j] <- 1
    } 
    
    if(data$Admission.Substatus[j]=="Cancel: Deposit"){
        data$Made.Deposit[j] <- 1
    } else if(data$Contact.Status[j]=="Deposit"){
        data$Made.Deposit[j] <- 1
    }
    
    if(!is.na(data$Award.Status[j])){
        data$FAFSA[j] <- 1
        
        if(data$Award.Status[j]=="No Award") {
            data$Award.Status[j] <- 0 
        } else if(data$Award.Status[j]=="Award Complete") {
            data$Award.Status[j] <- 1 
        } 
    } else {
        data$Award.Status[j] <- 0 
    }
    
    if(is.na(data$Total.Gift[j])) data$Total.Gift[j] <- 0
    if(is.na(data$Total.Institutional.Gift[j])) data$Total.Institutional.Gift[j] <- 0
    if(is.na(data$Gross.Need[j])) data$Gross.Need[j] <- 0
    
    if(grepl("Indian|Native|Black|African|Pacific|Asian|Two|Hispanic|Multiracial", data$Ethnicity[j])){
        data$Ethnicity.Indicator[j] <- 1
    }
    
    if(grepl("Lutheran|ELCA|Luthern|Lutheren", data$Religious.Preference[j])) {
        data$Lutheran.Indicator[j] <- 1
    }
    
    if(!grepl("Unknown|None|No Affiliation|No religious affiliation|Not Reported|Prefer not to respond", data$Religious.Preference[j])) {
        data$Religious.Indicator[j] <- 1
    }
    if(data$Religious.Preference[j]=="") data$Religious.Indicator[j] <- 0
    
    if(!is.na(data$Royal.ID[j])) {
        data$Royal15[j] <- 1
    }
    
    # season of first visit
    if(!is.na(data$First.Visit[j])) { 
        # if the student visited before applying, assign a 1
        if(data$First.Visit[j] < data$Date.App.Submitted[j]) {
            data$Visit.Before.App[j] <- 1
        }
        # season first visit
        month <- format(data$First.Visit[j], format="%m")
       if (month %in% c("06", "07", "08")) { 
           data$First.Visit.Summer[j] <- 1 
       } else if (month %in% c("09", "10", "11")) { 
           data$First.Visit.Fall[j] <- 1 
       } else if (month %in% c("12", "01", "02")) { 
           data$First.Visit.Winter[j] <- 1 
       } else if (month %in% c("03", "04", "05")) { 
           data$First.Visit.Spring[j] <- 1 
       }
        
       if(data$Year[j]=="2014 Fall"){
            if(data$First.Visit[j] < as.Date("2013-08-01")) { data$First.Visit.Before.Sr[j] <- 1 }
        } else if(data$Year[j]=="2015 Fall"){
            if(data$First.Visit[j] < as.Date("2014-08-01")) { data$First.Visit.Before.Sr[j] <- 1 }
        } else if (data$Year[j]=="2016 Fall"){
            if(data$First.Visit[j] < as.Date("2015-08-01")) { data$First.Visit.Before.Sr[j] <- 1 }
        } 
        
    }
    
    if(is.na(data$Countdown[j])) data$Countdown[j] <- 0
    if(is.na(data$Summer[j])) data$Summer[j] <- 0
    if(is.na(data$Number.Campus.Visits.y[j])) data$Number.Campus.Visits.y[j] <- 0
    
    # season of app
    app.month <- format(data$Date.App.Submitted[j], format="%m")
    if (app.month %in% c("06", "07", "08")) { 
        data$Applied.Summer[j] <- 1 
    } else if (app.month %in% c("09", "10", "11")) { 
        data$Applied.Fall[j] <- 1 
    } else if (app.month %in% c("12", "01", "02")) { 
        data$Applied.Winter[j] <- 1 
    } else if (app.month %in% c("03", "04", "05")) { 
        data$Applied.Spring[j] <- 1 
    } 
    
    if(data$Year[j] %in% "2014 Fall"){
        if(data$Date.App.Submitted[j] < as.Date("2013-09-01")) data$Applied.Before.Sr[j] <- 1
    } else if(data$Year[j] %in% "2015 Fall"){
        if(data$Date.App.Submitted[j] < as.Date("2014-09-01")) data$Applied.Before.Sr[j] <- 1
    } else if(data$Year[j] %in% "2016 Fall"){
        if(data$Date.App.Submitted[j] < as.Date("2015-09-01")) data$Applied.Before.Sr[j] <- 1
    }
    
    # calculate # of admits from zip code currently
    zip.ad <- zip.admits[zip.admits$Year==data$Year[j] & zip.admits$Zip==data$Zip[j], 3]
    if(length(zip.ad)>0) {
        data$Zip.Admits.TY[j] <- zip.ad
        data$Zip.Admits.TY.Pct[j] <- (zip.ad/data$Zip.Population[j])
    }
    
}

# write.csv(data, "full_factors.csv", row.names=FALSE)
# data <-  read.csv("full_factors.csv", stringsAsFactors=FALSE)
# remove excess
remove <- c("ID", "Contact.Status", "Admission.Substatus", "Major.Interest", "Date.App.Submitted", 
            "Date.App.Completed", "Date.Deposited", "Legacy", "Co.Curricular", "Gender", "Ethnicity", "Religious.Preference",
            "HS.Name", "City", "State", "Zip", "Number.Campus.Visits.x", "Royal.ID", "First.Visit", "Last.Visit",
            "Comprehensive.Cost", "Total.Institutional.Gift")

data.min <- data[,-which(names(data) %in% c(remove))]

# change some units
# thousands of dollars / pop
for(i in c(10,11,13,14)) data.min[,i] <- data.min[,i]/1000
# make pos. numbers 
for(k in c(17,31)) data.min[,k] <- data.min[,k]*10000
data.min$Zip.Admits.TY.Pct <- gsub("Inf", NA, data.min$Zip.Admits.TY.Pct)
# make decimals into percents
data.min$Zip.Pct.White <- data.min$Zip.Pct.White*100
data.min$GPA <- data.min$GPA*10

# write.csv(data.min, "min_factors.csv", row.names=FALSE)

