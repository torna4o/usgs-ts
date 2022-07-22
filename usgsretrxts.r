### Part 0: Loading library to manage data download from USGS databases

library(dataRetrieval) # the main package to retrieve USGS data
#vignette("dataRetrieval",package = "dataRetrieval") # a quick good tutorial
library(xts) # for time series operaitons


### Part 1: Retrieving No3 data from USGS database online

siteNumber <- "11447650" # This example is Sacramento Freeport CA
parameterCd <- "00618" # This parameter code is for nitrate mg/l
beginDate <- "1996-01-01"
endDate <- "2022-07-20"


# The following code retrieves data, you have to have internet connection here
rawData <- readNWISqw(siteNumber,parameterCd,
                     beginDate ,endDate, reshape=TRUE) #reshape to have wide data

### Part 2: Readying data to xts operation

poi2 <- as.data.frame(rawData$sample_dt)
colnames(poi2)[1] <- "date"
poi2$date<-as.POSIXct(poi2$date,format="%Y-%m-%d") # USGS date format
head(poi2) # Sanity check
poi2$nitrate <- rawData$result_va_00618 # specific column of interest for nitrate

### Part 3: xts - daily time-series data first

# Here there will be a continuous date, and the data will be inserted in corresponding
# dates, the rest will be NA

df1.zoo <- zoo(poi2[,2],poi2[,1])
head(df1.zoo)
df1x <- as.xts(df1.zoo)
df.mid <- as.POSIXct(zoo(,seq(start(df1.zoo),end(df1.zoo), by="1 day")), format="%Y-%m-%d")
df2 <- merge(df1x, df.mid)
head(df2,34) # Sanity check


### Part 4: Filling the gaps with linear interpolation
dfnonna <- na.approx(df2,  maxgap=85) # maxgap is the longest gap to be interpolated


### Part 5: Aggregating the data to monthly values with mean aggregation


dfnasagg <- period.apply(dfnonna[, "df1x"], 
                   INDEX = endpoints(dfnonna, on = "months", k = 1), 
                   FUN = mean)

### Part 6: Visualizing

plot(df2, type="p")
#lines(dfnonna, col="red")
lines(dfnasagg, col ="green")

