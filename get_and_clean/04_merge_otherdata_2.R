# 03_merge_otherdata.R
# read in hs size and zip code geographic data,
# merge with salesforce data
# in: salesforce_combined.csv, factors_schoolsize.csv, factors_zip_data2.csv
# out: all_combined.csv


setwd("/Users/kpavlik/git_projects/yield")
data <- read.csv("data/salesforce_combined1.csv", stringsAsFactors=FALSE)

# clean zip code feature
data$Zip <- sapply(data$Zip, function(x) x <- strsplit(x, "-")[[1]][1])
zips <- unique(data$Zip)

# read in zip code data, dependent on 03_factors_zipcodes.R
zip_data <- read.csv("data/factors_zip_data2.csv", stringsAsFactors=FALSE)
zip_data <- zip_data[zip_data$zip %in% zips, ]
# add zip code features
data$Distance.Mhd <- NA
data$Zip.Median.Income <- NA
data$Zip.Population <- NA
data$Zip.Pct.White <- NA
data$Zip.Alumni.Pop <- NA
data$Zip.Alumni.Density <- NA

# read in high school data, dependent on 02_factors_hs_size.R
hs <- read.csv("data/factors_schoolsize.csv", stringsAsFactors=FALSE)
# clean HS name feature
data$HS.Name <- gsub("HS", "High School", data$HS.Name)
# add HS size feature
data$HS.Size <- NA

for(j in seq_along(data[,1])){
    
    #### combine zip info
    if(data$Zip[j] %in% zip_data$zip){
        data$Distance.Mhd[j] <- zip_data[zip_data$zip==data$Zip[j], 2]
        data$Zip.Median.Income[j] <- zip_data[zip_data$zip==data$Zip[j], 3]
        data$Zip.Population[j] <- zip_data[zip_data$zip==data$Zip[j], 4]
        data$Zip.Pct.White[j] <- zip_data[zip_data$zip==data$Zip[j], 6]
        data$Zip.Alumni.Pop[j] <- zip_data[zip_data$zip==data$Zip[j], 7]
        data$Zip.Alumni.Density[j] <- zip_data[zip_data$zip==data$Zip[j], 8]
    }
    
    #### combine HS info
    if(data$HS.Name[j] %in% hs$school) {
        data$HS.Size[j] <- hs[hs$school==data$HS.Name[j], 2]
    }
    
}
# write.csv(data, "data/all_combined1.csv", row.names=FALSE)
