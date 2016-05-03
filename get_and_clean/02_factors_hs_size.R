# 02_factors_hs_size.R
# scrape names and sizes of MN and ND high schools from web
# in: none
# out: factors_schoolsize.csv

# size of HS
# MN: http://www.mshsl.org/mshsl/enrollments.asp
library(XML)
library(RCurl)

### MN HS sizes
URL <- "http://www.mshsl.org/mshsl/enrollments.asp"
mnhs <- htmlTreeParse(getURL(URL), useInternal=TRUE)
mnhs.raw <- xpathApply(mnhs, "//table//table//table//tr", xmlValue)
mnhs.raw <- unlist(mnhs.raw)
mnhs.raw <- mnhs.raw[nchar(mnhs.raw) < 60]
mnhs.split <- strsplit(mnhs.raw, "\r\n")
mnhs <- data.frame()
for(j in seq_along(mnhs.split)) {
    school <- mnhs.split[[j]][1]
    if(length( mnhs.split[[j]][2]) > 0) size <- mnhs.split[[j]][2]
    row <- data.frame(school, size)
    mnhs <- rbind(mnhs, row)
}
mnhs$size <- as.numeric(as.character(mnhs$size))
mnhs <- mnhs[!is.na(mnhs$size), ]

### ND HS Sizes
URL2 <- "http://www.localschooldirectory.com/state-schools/ND"
ndhs <- htmlTreeParse(getURL(URL2), useInternal=TRUE)
ndhs.raw <- xpathApply(ndhs, "//div[@class='indent_2']//table//tr//td", xmlValue)
ndhs.raw <- unlist(ndhs.raw)
chunk <- 1:8
ndhs <- ndhs.raw[chunk]
for(x in 1:round(length(ndhs.raw)/8)){
    chunk <- chunk + 8
    row <- ndhs.raw[chunk]
    ndhs <- rbind(ndhs, row)
}
ndhs <- data.frame(ndhs)
colnames(ndhs) <- c("one", "city", "schools", "size", "s.t.ratio", "median.income", "rating", "grade")
ndhs$school <- paste(ndhs$city, "High School")
ndhs <- ndhs[,c(9,4)]
ndhs$size <- as.character(ndhs$size)
ndhs$size <- as.numeric(gsub("\\,", "", ndhs$size))

hs_size <- rbind(mnhs, ndhs)

#write.csv(hs_size, "factors_schoolsize.csv", row.names=FALSE)
