# Construct dataset:
# get: http://stat-computing.org/dataexpo/2009/2007.csv.bz2
x <- read.csv(bzfile("2007.csv.bz2"), as.is=TRUE)
air <- read.csv("airports.csv", as.is=TRUE)
x <- x[!is.na(x$DepTime) & !is.na(x$ActualElapsedTime),]

# Only take flights between top 100 airports
topArr <- names(sort(table(c(x$Origin,x$Dest)),decreasing=TRUE))
x <- x[x$Origin %in% topArr[1:100] & x$Dest %in% topArr[1:100],]

# Parse local minutes and hours for departure:
min <- substr(x$DepTime, nchar(x$DepTime) - 1L, nchar(x$DepTime))
hour <- substr(x$DepTime, 1L , nchar(x$DepTime) - 2L)
hour[nchar(x$DepTime) < 3L] <- 0
hour[hour == "24"] <- 0

# Calculate offsets for each airport with and without daylight savings
library(timezone) # github.com/statsmaths/timezone
offset01 <- getOffset(air$lat, air$lon, rep(1167609600, nrow(air)))
offset02 <- getOffset(air$lat, air$lon, rep(1185926400, nrow(air)))
offset01[2825] <- 3600 * -5 # manual correction

# Determine whether daylight savings is in effect
dstFlag <- (x$Month == 3 & x$DayofMonth == 11 & hour >= 3) |
           (x$Month == 3 & x$DayofMonth > 11) |
           (x$Month == 11 & x$DayofMonth == 4 & hour < 2) |
           (x$Month == 11 & x$DayofMonth < 4) |
           (x$Month %in% 4:10)

# Get departure time of each flight as a POSIX object;
# needs to be correct for timezone still
depP <- sprintf("2007-%02d-%02d %02d:%02d",
    x$Month, x$DayofMonth, as.numeric(hour), as.numeric(min))
depP <- as.POSIXct(depP)

# Calculate local offset of each departure
index <- match(x$Origin, air$iata)
dOffset <- offset01[index]
dOffset[dstFlag] <- offset02[index[dstFlag]]
dTime <- depP - dOffset

# Calculate local offset of arrival and arrival time UTC
aTime <- dTime + x$ActualElapsedTime * 60
index <- match(x$Dest, air$iata)
dstFlag <- (as.numeric(aTime) >= 1173596400) &
           (as.numeric(aTime) <= 1194246000)
aOffset <- offset01[index]
aOffset[dstFlag] <- offset02[index[dstFlag]]

# Compute all quantities in km and seconds
dist <- 1.60934 * x$Dist
depDelay <- x$DepDelay * 60
arrDelay <- x$ArrDelay * 60

# Save these here; may use later
schedTotTime <- x$CRSElapsedTime * 60
totTime <- x$ActualElapsedTime * 60
airTime <- x$AirTime * 60
taxiInTime <- x$TaxiIn * 60
taxiOutTime <- x$TaxiOut * 60

# Sanity check a few things
quantile((aTime - dTime))
table(dOffset / 3600)
table(aOffset / 3600)

# Construct the final dataset
df <- data.frame(dTime=dTime, aTime=aTime, dOffset=dOffset, aOffset=aOffset,
                 origin=x$Origin, dest=x$Dest, carrier=x$UniqueCarrier,
                 depDelay=depDelay, arrDelay=arrDelay, dist=dist,
                 stringsAsFactors=FALSE)
saveRDS(df, "airline2007_clean.Rds")
