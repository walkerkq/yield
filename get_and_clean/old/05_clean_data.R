# 05_clean_data.R
# standardize and clean data for analysis
# create indiator variables for factors
# in: all_combined.csv
# out: full_factors.csv, min_factors.csv

setwd("/Users/kpavlik/git_projects/yield/")
data <- read.csv("data/all_combined.csv", stringsAsFactors=FALSE)

# clean up
data <- data[data$ID!="", ] # remove anyone w/o an ID
data <- data[grepl("Fall", data$Year), ] # keep only fall entrances
data$Contact.Status <- gsub("\\: ", "", data$Contact.Status)
data$Co.Curricular <- tolower(data$Co.Curricular)
data$Major.Interest <- tolower(data$Major.Interest)
data$Date.App.Submitted <- as.Date(data$Date.App.Submitted, format="%m/%d/%Y")
data$Date.App.Completed <- as.Date(data$Date.App.Completed, format="%m/%d/%Y")
data$Date.Deposited <- as.Date(data$Date.Deposited, format="%m/%d/%Y")
data$First.Visit <- as.Date(data$First.Visit, format="%Y-%m-%d")
data$Second.Visit <- as.Date(data$Second.Visit, format="%Y-%m-%d")
data$Third.Visit <- as.Date(data$Third.Visit, format="%Y-%m-%d")
data$Last.Visit <- as.Date(data$Last.Visit, format="%Y-%m-%d")

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
data$FAFSA <- 0
data$Zip.Admits.TY <- 0
data$Zip.Admits.TY.Pct <- 0
data$App.Before.Nov <- 0
data$Visits.by.Jan1 <- 0

### scale dates to be all oriented around fall 2015
for(j in seq_along(data[,1])){
   if(data$Year[j]=="2014 Fall"){
        edit <- 365
   } else if(data$Year[j]=="2016 Fall"){
        edit <- -365
   } else { edit <- 0 }
   for(b in c(9:11,24:27)) {
        data[j,b] <- data[j,b] + edit
   }
}

data$ACT.Low <- 0
data$ACT.High <- 0
data$GPA.Low <- 0
data$GPA.High <- 0
data$HSP.Low <- 0
data$HSP.High <- 0
data$Distance.Low <- 0
data$Distance.High <- 0

zip.admits <- data.frame(table(data$Year, data$Zip))
colnames(zip.admits) <- c("Year", "Zip", "Count")
zip.deposits <- data.frame(table(data$Year[data$Contact.Status=="Deposit"], data$Zip[data$Contact.Status=="Deposit"]))
colnames(zip.deposits) <- c("Year", "Zip", "Count")

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
    
    if(is.na(data$Countdown[j])) data$Countdown[j] <- 0
    if(is.na(data$Summer[j])) data$Summer[j] <- 0
    if(is.na(data$Number.Campus.Visits.y[j])) data$Number.Campus.Visits.y[j] <- 0
    
    # applied before nov 1 
   if(data$Date.App.Submitted[j] < as.Date("2014-11-01")) data$App.Before.Nov[j] <- 1

   # number visits by jan 1
   visits <- data[j,c("First.Visit", "Second.Visit", "Third.Visit", "Last.Visit")]
   if(!is.na(visits$First.Visit) & !is.na(visits$Last.Visit) & visits$First.Visit == visits$Last.Visit){
        visits <- visits[,-1]
   }
   data$Visits.by.Jan1[j] <- suppressWarnings(length(which(visits < as.Date("2015-01-01"))))
   
    # calculate # of admits from zip code currently
    zip.ad <- zip.admits[zip.admits$Year==data$Year[j] & zip.admits$Zip==data$Zip[j], 3]
    if(length(zip.ad)>0) {
        data$Zip.Admits.TY[j] <- zip.ad
        data$Zip.Admits.TY.Pct[j] <- (zip.ad/data$Zip.Population[j])
    }
   
    #ACT, GPA, HS Percentile cutoffs
   if(data$ACT[j] <= 24) {
       data$ACT.Low[j] <- 1
   } else if(data$ACT[j] >= 33) {
       data$ACT.High[j] <- 1
   }
   if(!is.na(data$GPA[j])){ 
       if(data$GPA[j] < 3.0) {
           data$GPA.Low[j] <- 1
       } else if(data$GPA[j] > 3.9) {
           data$GPA.High[j] <- 1
       }
   }
   if(!is.na(data$HS.Percentile[j])) { 
       if(data$HS.Percentile[j] < 75) {
           data$HSP.Low[j] <- 1
       } else if(data$HS.Percentile[j] > 90) {
           data$HSP.High[j] <- 1
       }
   }
   if(!is.na(data$Distance.Mhd[j])){
       if(data$Distance.Mhd[j] < 75){
           data$Distance.Low[j] <- 1
       } else if(data$Distance.Mhd[j] > 300){
           data$Distance.High[j] <- 1
       } 
   }
    
}

# write.csv(data, "full_factors.csv", row.names=FALSE)
# data <-  read.csv("full_factors.csv", stringsAsFactors=FALSE)
# cut down as if we are running the model on Jan. 1
data$Date.App.Submitted <- as.Date(data$Date.App.Submitted, format="%Y-%m-%d")
data2 <- data[data$Date.App.Submitted <= as.Date("2015-01-01"), ]

# admits this year needs to be by 1/1
data2$Zip.Admits.TY <- 0
data2$Zip.Admits.TY.Pct <- 0
for(j in seq_along(data2[,1])){
     # calculate # of admits from zip code currently
     zip.ad <- zip.admits[zip.admits$Year==data2$Year[j] & zip.admits$Zip==data2$Zip[j], 3]
     if(length(zip.ad)>0) {
          data2$Zip.Admits.TY[j] <- zip.ad
          data2$Zip.Admits.TY.Pct[j] <- (zip.ad/data2$Zip.Population[j])
     }
}

# remove excess
remove <- c("ID", "Contact.Status", "Admission.Substatus", "Major.Interest", "Date.App.Submitted", 
            "Date.App.Completed", "Date.Deposited", "Legacy", "Co.Curricular", "Gender", "Ethnicity", "Religious.Preference",
            "HS.Name", "City", "State", "Zip", "Number.Campus.Visits.x", "Royal.ID", "First.Visit", "Second.Visit", "Third.Visit", "Last.Visit",
            "Summer", "Countdown", "Comprehensive.Cost", "Total.Institutional.Gift", 
            # removed due to time concerns (post-1/1)
            "FAFSA", "Number.Campus.Visits.y", "Award.Status", "Made.Deposit",
            "Cost.of.Attendance", "Total.Gift", "Gross.Need")

data.min <- data[,-which(names(data) %in% c(remove))]

# change some units
# thousands of dollars / pop
for(i in c(7,8)) data.min[,i] <- data.min[,i]/1000
# make pos. numbers 
for(k in c(11,22)) data.min[,k] <- data.min[,k]*10000
data.min$Zip.Admits.TY.Pct <- gsub("Inf", NA, data.min$Zip.Admits.TY.Pct)
# make decimals into percents
data.min$Zip.Pct.White <- data.min$Zip.Pct.White*100

# write.csv(data.min, "min_factors.csv", row.names=FALSE)

