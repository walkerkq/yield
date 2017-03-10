# 07_date_filter.R
# filter data by date, combine
# in: date_data.csv, non_date_data_imputed.csv
# out: data filtered by date

setwd("/Users/kaylinwalker/R/yield")

data <- read.csv("data/non_date_data_imputed.csv", stringsAsFactors=F)
date_data <- read.csv("data/date_data.csv", stringsAsFactors=F)
visit <- read.csv("data/VisitFactors.csv", stringsAsFactors=F)

# scale date data
date_data <- merge(date_data, data[,c(2,3)], by="ID", all.x=TRUE, all.y=FALSE)
for(g in 2:8) {
     date_data[,g] <- as.Date(date_data[,g], format="%Y-%m-%d")
}
for(i in seq_along(date_data$ID)){
     if(date_data$Year[i] %in% "2014 Fall") {
          for(g in 2:8) date_data[i,g] <- date_data[i,g] + 365
     } else if (date_data$Year[i] %in% "2016 Fall"){
          for(g in 2:8) date_data[i,g] <- date_data[i,g] - 365
     }
}

##-FUNCTION--------
date_scale <- function(my_date) {
     
     # indicators
     data$App <- ifelse(date_data$Date.App.Submitted <= my_date, 1, 0) 
     data$Deposited <- ifelse(date_data$Date.Deposited <= my_date, 1, 0)
     data$FAFSA.Received <- ifelse(date_data$Date.FAFSA.Received <= my_date, 1, 0)
     data$Award.Packaged <- ifelse(date_data$Date.Award.Packaged <= my_date, 1, 0)
     data$CREDO.Invited <- ifelse(date_data$Date.CREDO.Invitation.Sent <= my_date, 1, 0)
     data$CREDO.Accepted <- ifelse(date_data$Date.CREDO.Contract.Received <= my_date, 1, 0)
     data$Deposited[is.na(data$Deposited)] <- 0
     data$FAFSA.Received[is.na(data$FAFSA.Received)] <- 0
     data$Award.Packaged[is.na(data$Award.Packaged)] <- 0
     data$CREDO.Invited[is.na(data$CREDO.Invited)] <- 0
     data$CREDO.Accepted[is.na(data$CREDO.Accepted)] <- 0

     ## zip code
     zips <- aggregate(App ~ Distance.Mhd + Year, data, sum) # added in visit loop
          
     ## VISITS
     visit$Visit.Date <- as.Date(visit$Visit.Date, format = "%m/%d/%Y")
     colnames(visit) <- c("ID", "Time", "Visit.Date", "Visit.Type")
     
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
           if(id_visits$Visit.Type[g] %in% c("Music Performance Scholarship", "Speech and Debate Scholarship", "Theatre Peformance Scholarship", "Visual Arts Scholarship")) data$Performance.Scholarship.Event[i] <- 1
           if(!grepl("Scholarship|Countdown|Celebration", id_visits$Visit.Type[g])) data$Regular.Visits.by.Date[i] <- data$Regular.Visits.by.Date[i] + 1
           }
       }
       ## zip code admits too
       data$Other.Admits.Density[i] <- (zips$App[zips$Distance.Mhd %in% data$Distance.Mhd[i] & zips$Year %in% data$Year[i]])/data$Zip.Population[i]
     }
     
     return(data)
}

### final file with info up to the date


# choosing march 15 for 80% of fafsas in
data <- date_scale(as.Date("2015-03-01"))

# scale data
#1000s
data$Zip.Median.Income <- data$Zip.Median.Income/1000
data$Zip.Population <- data$Zip.Population/1000
# pcts
data$Zip.Pct.White <- data$Zip.Pct.White*100
data$Zip.Alumni.Density <- data$Zip.Alumni.Density*100
data$Other.Admits.Density <- data$Other.Admits.Density*100

data <- data[ , -c(6,19:24,28,32,35)]

#write.csv(data, "enrollment_data.csv", row.names=F)

