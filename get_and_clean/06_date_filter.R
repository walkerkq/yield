# 06_date_filter.R
# standardize and clean data for analysis
# create indiator variables for factors
# in: date_data.csv, non_date_data.csv

# choose date (scaled to Fall 2015 dates)
my_date <- as.Date("2015-03-01")

##---------
  
setwd("/Users/kpavlik/git_projects/yield/data")
data <- read.csv("date_data.csv", stringsAsFactors=F)

# very last: visit
visit <- read.csv("VisitFactors.csv", stringsAsFactors=F)
colnames(visit) <- c("ID", "Date.Time", "Visit.Date", "Visit.Type")
visit$Visit.Date <- as.Date(visit$Visit.Date, format="%m/%d/%Y")

##### get data by that date
## DATE DATA
date_nums <- c(6:8, 15:18)
for(u in date_nums) data[,u] <- as.Date(data[,u], format="%Y-%m-%d")

for(o in seq_along(data$ID)){
  if(data$Year[o] %in% "2014 Fall"){ adj <- 365 } 
  else if(data$Year[o] %in% "2016 Fall") { adj <- -365 }
  else{ adj <- 0 }
  for(j in date_nums){
    data[o,j] <- data[o,j] + adj
  }
}

# indicators
data$Deposited <- ifelse(data$Date.Deposited <= my_date, 1, 0)
data$FAFSA.Received <- ifelse(data$Date.FAFSA.Received <= my_date, 1, 0)
data$Award.Packaged <- ifelse(data$Date.Award.Packaged <= my_date, 1, 0)
data$CREDO.Invited <- ifelse(data$Date.CREDO.Invitation.Sent <= my_date, 1, 0)
data$CREDO.Accepted <- ifelse(data$Date.CREDO.Contract.Received <= my_date, 1, 0)
data$Deposited[is.na(data$Deposited)] <- 0
data$FAFSA.Received[is.na(data$FAFSA.Received)] <- 0
data$Award.Packaged[is.na(data$Award.Packaged)] <- 0
data$CREDO.Invited[is.na(data$CREDO.Invited)] <- 0
data$CREDO.Accepted[is.na(data$CREDO.Accepted)] <- 0

# remove award package info if not awarded yet
for(g in seq_along(data$ID)){
  if(data$Award.Packaged[g] == 0){
    data$Total.Gift[g] <- NA
    data$Total.Institutional.Gift[g] <- NA
    data$Gross.Need[g] <- NA
  }
}

## VISITS
data$Regular.Visits.by.Date <- 0
data$Music.Scholarship.Event <- 0
data$Performance.Scholarship.Event <- 0
data$Academic.Scholarship.Event <- 0
data$Countdown <- 0

for(i in seq_along(data$ID)){
  id <- data$ID[i]
  id_visits <- visit[visit$ID %in% id, ]
  year <- data$Year[i]
  if(year %in% "2014 Fall") {
    id_visits$Visit.Date <- id_visits$Visit.Date + 365
  } else if (year %in% "2016 Fall"){
    id_visits$Visit.Date <- id_visits$Visit.Date - 365
  }
  id_visits <- id_visits[id_visits$Visit.Date <= my_date & !is.na(id_visits$ID), ]
  id_visits <- id_visits[!is.na(id_visits$ID), ]
  if(length(id_visits[,1])>0){
    for(g in seq_along(id_visits$ID)){
      if(id_visits$Visit.Type[g] %in% c("Regents Scholarship Event", "Presidential Distinction Scholarship")) data$Academic.Scholarship.Event[i] <- 1
      if(id_visits$Visit.Type[g] %in% c("Cobber Countdown", "Cobber Celebration")) data$Countdown[i] <- 1
      if(id_visits$Visit.Type[g] %in% "Music Performance Scholarship") data$Music.Scholarship.Event[i] <- 1
      if(id_visits$Visit.Type[g] %in% c("Music Performance Scholarship", "Speech and Debate Scholarship", "Theatre Performance Scholarship", "Visual Arts Scholarship")) data$Performance.Scholarship.Event[i] <- 1
      if(!grepl("Scholarship|Countdown|Celebration", id_visits$Visit.Type[g])) data$Regular.Visits.by.Date[i] <- data$Regular.Visits.by.Date[i] + 1
      }
  }
}

### final file with info up to the date




