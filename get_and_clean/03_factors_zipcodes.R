# 03_factors_zipcodes.R
# collect distance to Moorhead data from distancecheck.com
# pull relevant census data from census.gov downloads
# in: factor_zips.csv, factors_alumni_zipcodes.csv (source Banner)
#     ACS_14_5YR_income.csv, ACS_14_5YR_race.csv (source Census.gov)
# out: factors_zipdistance.csv, factors_zip_data.csv, factors_zip_data2.csv

setwd("/Users/kwalker/git_project/yield")
library(XML)
library(RCurl)

### get zip code distances
zips <- read.csv("data/factor_zips.csv", stringsAsFactors=FALSE)
distances <- NULL
for(x in seq_along(zips)){
    zip <- zips[x,1]
    URL <- paste("http://distancecheck.com/zipcode-distance.php?start=", zip,"&end=56562&key=94e59342-9574-4790-9de2-d0692b809c59&submit=Calculate+Distance", sep="")
    dist <- htmlTreeParse(getURL(URL), useInternal=TRUE)
    dist.raw <- xpathApply(dist, "//h2//strong[@id='miles']", xmlValue)
    dist.raw <- unlist(dist.raw)
    dist.raw <- gsub(" miles ", "", dist.raw)
    if(length(dist.raw) > 0){
        row <- data.frame(zip=zip, distance=dist.raw)
    } else { row <- data.frame(zip=zip, distance="NA") }
    distances <- rbind(distances, row)
}
#write.csv(distances, "data/factors_zipdistance.csv", row.names=FALSE)
#distances <- read.csv("data/factors_zipdistance.csv", stringsAsFactors=FALSE)

# merge with zip code MEDIAN INCOME
# census.gov MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2014 INFLATION-ADJUSTED DOLLARS)

inc <- read.csv("data/ACS_14_5YR_income.csv", stringsAsFactors=FALSE)

# keep HC02_EST_VC02
inc <- inc[2:length(inc[,1]),c(3,6)]
for(u in seq_along(inc[,1])) inc[u,1] <- strsplit(inc[u,1], " ")[[1]][2]
colnames(inc) <- c("Zip", "Median.Income")

di <- merge(distances, inc, by.x="zip", by.y="Zip", all=TRUE)

# merge with zip code % WHITE
# census.gov RACE 2010-2014 American Community Survey 5-Year Estimates B02001

race <- read.csv("data/ACS_14_5YR_race.csv", stringsAsFactors=FALSE)

# keep zip code, HD01_VD01 (total estimate) and HD01_VD02 (white alone)
race <- race[2:length(race[,1]) ,c(3,4,6)]
for(u in seq_along(race[,1])) race[u,1] <- strsplit(race[u,1], " ")[[1]][2]
colnames(race) <- c("Zip", "Total", "White")
race$Pct.White <- round(as.numeric(race$White)/as.numeric(race$Total), 4)

dir <- merge(di, race, by.x="zip", by.y="Zip", all=TRUE)
for(h in 2:6) dir[,h] <- as.numeric(dir[,h])

# save out at this point
# write.csv(dir, "data/factors_zip_data.csv", row.names=FALSE)

# merge with # alumni in zip code
# calculate alumni density per zip code
alumni <- read.csv("data/factors_alumni_zipcodes.csv", stringsAsFactors=FALSE)
alumni$Zip <- substr(alumni$Zip, 1, 5)
alumni_zips <- data.frame(table(alumni$Zip))
alumni_zips <- alumni_zips[order(-alumni_zips$Freq), ]
colnames(alumni_zips) <- c("Zip", "Alumni.Pop")
dira <- merge(dir, alumni_zips, by.x="zip", by.y="Zip", all=TRUE)
dira$Alumni.Density <- round(dira$Alumni.Pop/dira$Total, 4)

#write.csv(dira, "data/factors_zip_data2.csv", row.names=FALSE)
