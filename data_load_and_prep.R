source("./function_lib.R")

## Data processing libraries
load_libraries(c("gdata", "reshape2", "mice"))

loginfo("-- Initiating data checks --")

##############
## Data checks
##############

data.sources <- list (
  weekly.fuel = "https://www.eia.gov/dnav/pet/hist_xls/EMM_EPM0_PTE_Y48SE_DPGw.xls",
  monthly.fuel = "https://www.eia.gov/dnav/pet/hist_xls/EMM_EPM0_PTE_Y48SE_DPGm.xls",
  rail.link = NULL,
  bike.broadway = "https://data.seattle.gov/api/views/j4vh-b42a/rows.csv?accessType=DOWNLOAD",
  bike.burke = "https://data.seattle.gov/api/views/2z5v-ecg8/rows.csv?accessType=DOWNLOAD",
  bike.fremont = "https://data.seattle.gov/api/views/65db-xm6k/rows.csv?accessType=DOWNLOAD",
  bike.spokane = "https://data.seattle.gov/api/views/upms-nr8w/rows.csv?accessType=DOWNLOAD",
  road.wsdot = "http://www.wsdot.wa.gov/mapsdata/travel/docs/StateHighwayVMT.xlsx",
  road.sdot = NULL,
  bus.kcmetro = NULL
)

data.files <- list(
  weekly.fuel = "./data/fuel/EMM_EPM0_PTE_Y48SE_DPGw.xls",
  monthly.fuel = "./data/fuel/EMM_EPM0_PTE_Y48SE_DPGm.xls",
  rail.link = "./data/rail/Link_2014_Daily_Ridership.xlsx",
  bike.broadway = "./data/bike/Broadway_Cycle_Track_North_Of_E_Union_St.csv",
  bike.burke = "./data/bike/Burke_Gilman_Trail_north_of_NE_70th_St_Bike_and_Ped_Counter.csv",
  bike.fremont = "./data/bike/Fremont_Bridge_Hourly_Bicycle_Counts_by_Month_October_2012_to_present.csv",
  bike.spokane = "./data/bike/Spokane_St_Bridge_Counter.csv",
  road.wsdot = "./data/road/StateHighwayVMT.xlsx",
  road.sdot = "./data/road/Combined_Control_Counts-cleaned.csv",
  bus.kcmetro = "./data/bus/kcmetro-ridership.xls"
)

## Check for data and download if missing
for(n in names(data.files)){
  if(!file.exists(data.files[[n]])){
    logwarn("Data file %s not found; attempting download", data.files[[n]])
    if(is.null(data.sources[[n]])) {
      logerror("No data source for %s", n)
      stop("No data source for ", n)
    }
    try(if(download.file(data.sources[[n]], data.files[[n]], mode="wb", method="curl") != 0) {
      logerror("Download of data source failed for %s", n)
      stop("Download of data source failed for ", n)
    })
  }
}

loginfo("-- Data files confirmed; beginning data load --")

##########################################
## Seattle Fuel Prices - 2003-2016, weekly
## Weeks starting on Mondays
##########################################
fuel.prices.weekly <- read.xls(data.files[["weekly.fuel"]], sheet=2, skip=2, blank.lines.skip=T, stringsAsFactors=F, col.names=c("Date","Avg.Price"))
# convert dates
fuel.prices.weekly$Date <- as.Date(fuel.prices.weekly$Date, format="%b %d, %Y")
fuel.prices.weekly$Week <- format(fuel.prices.weekly$Date, format="%V")
fuel.prices.weekly$WkYear <- format(fuel.prices.weekly$Date, format="%G")
# Order by date
fuel.prices.weekly <- fuel.prices.weekly[order(fuel.prices.weekly[,1]),]

loginfo("Weekly fuel prices loaded; %i records", nrow(fuel.prices.weekly))
if(nrow(fuel.prices.weekly) == 0) logwarn("Weekly fuel prices have no data!")


##########################################
## Seattle Fuel Prices - 2003-2016, monthly
##########################################
fuel.prices.monthly <- read.xls(data.files[["monthly.fuel"]], sheet=2, skip=2, blank.lines.skip=T, stringsAsFactors=F, col.names=c("Date","Avg.Price"))
# convert dates
fuel.prices.monthly$Date <- as.Date(paste("01 ",fuel.prices.monthly$Date), format="%d %b-%Y")
fuel.prices.monthly$Month <- format(fuel.prices.monthly$Date, format="%m")
fuel.prices.monthly$Year <- format(fuel.prices.monthly$Date, format="%Y")
# Order by date
fuel.prices.monthly <- fuel.prices.monthly[order(fuel.prices.monthly[,1]),]
# First month has no price data; whack it
fuel.prices.monthly <- fuel.prices.monthly[-1,]

loginfo("Monthly fuel prices loaded; %i records", nrow(fuel.prices.monthly))
if(nrow(fuel.prices.monthly) == 0) logwarn("Monthly fuel prices have no data!")


###############################
## LINK Ridership - 2014, daily
###############################
rail.link.2014 <- read.xls(data.files[["rail.link"]], stringsAsFactors=F, colClasses=c(NA, "num.with.commas"))
# convert dates
rail.link.2014$Date <- as.Date(rail.link.2014$Date)
# There is an obvious outlier in February - presumably due to a special event or an error in measurement - let's fix it by linear interpolation
rl2014.max <- which(rail.link.2014$Daily.Ridership == max(rail.link.2014$Daily.Ridership))
rail.link.2014[rl2014.max,]$Daily.Ridership <- as.integer(approx(
  c(rail.link.2014[rl2014.max - 7,]$Daily.Ridership,
    rail.link.2014[rl2014.max + 7,]$Daily.Ridership),
  n=3
)$y[2])
rm(rl2014.max)
# Restore Missing Dates
rail.link.2014 <- rbind(rail.link.2014, data.frame(Date = generateMissingDates(rail.link.2014$Date, "day"), Daily.Ridership=NA))
# Order by date
rail.link.2014 <- rail.link.2014[order(rail.link.2014[,1]),]
# Impute missing data at 2014-11-15
rl2014.na <- which(is.na(rail.link.2014$Daily.Ridership))
rail.link.2014[rl2014.na,]$Daily.Ridership <- as.integer(approx(
  c(rail.link.2014[rl2014.na - 7,]$Daily.Ridership,
    rail.link.2014[rl2014.na + 7,]$Daily.Ridership),
  n=3
)$y[2])
rm(rl2014.na)

loginfo("LINK Light Rail data loaded; %i records", nrow(rail.link.2014))
if(nrow(rail.link.2014) == 0) logwarn("LINK has no data!")


###################################################
## Bike - Broadway hourly
###################################################
bike.broadway.hourly <- read.csv(data.files[["bike.broadway"]], stringsAsFactors=F, colClasses=c(NA, "integer", "integer", "integer"), col.names=c("Date","Riders","NB","SB"))
# convert dates
bike.broadway.hourly$Date <- as.Date(bike.broadway.hourly$Date, format="%m/%d/%Y %H:%M:%S %p")
# Order by date
bike.broadway.hourly <- bike.broadway.hourly[order(bike.broadway.hourly[,1]),]
# simplify to daily numbers
bike.broadway.daily <- aggregate(Riders ~ Date, data=bike.broadway.hourly, FUN=sum)
rm(bike.broadway.hourly)
# Restore missing dates
bike.broadway.daily <- rbind(bike.broadway.daily, data.frame(Date=generateMissingDates(bike.broadway.daily$Date, "day"), Riders=NA))
bike.broadway.daily <- bike.broadway.daily[order(bike.broadway.daily[,1]),]

loginfo("Broadway bike data loaded; %i records", nrow(bike.broadway.daily))
if(nrow(bike.broadway.daily) == 0) logwarn("Broadway has no data!")


###################################################
## Bike - Burke Gilman hourly
###################################################
bike.burke.hourly <- read.csv(data.files[["bike.burke"]], stringsAsFactors=F, colClasses=c(NA, "integer", "integer", "integer", "integer", "integer"), col.names=c("Date","Traffic","Ped.SB","Ped.NB","Bike.NB","Bike.SB"))
# convert dates
bike.burke.hourly$Date <- as.Date(bike.burke.hourly$Date, format="%m/%d/%Y %H:%M:%S %p")
# Order by date
bike.burke.hourly <- bike.burke.hourly[order(bike.burke.hourly[,1]),]
# simplify to daily numbers
bike.burke.daily <- aggregate(Bike.SB + Bike.NB ~ Date, data=bike.burke.hourly, FUN=sum)
names(bike.burke.daily) <- c("Date","Riders")
rm(bike.burke.hourly)
# Restore missing dates
bike.burke.daily <- rbind(bike.burke.daily, data.frame(Date = generateMissingDates(bike.burke.daily$Date, "day"), Riders=NA))
bike.burke.daily <- bike.burke.daily[order(bike.burke.daily[,1]),]
# Set series of 0's to NA
bike.burke.daily[which(bike.burke.daily$Riders == 0),]$Riders <- NA

loginfo("Burke Gilman bike data loaded; %i records", nrow(bike.burke.daily))
if(nrow(bike.burke.daily) == 0) logwarn("Burke Gilman has no data!")


###################################################
## Bike - Fremont Bridge hourly
###################################################
bike.fremont.hourly <- read.csv(data.files[["bike.fremont"]], stringsAsFactors=F, colClasses=c(NA, "integer", "integer"), col.names=c("Date","East","West"))
# convert dates
bike.fremont.hourly$Date <- as.Date(bike.fremont.hourly$Date, format="%m/%d/%Y %H:%M:%S %p")
# Order by date
bike.fremont.hourly <- bike.fremont.hourly[order(bike.fremont.hourly[,1]),]
# simplify to daily numbers
bike.fremont.daily <- aggregate(East + West ~ Date, data=bike.fremont.hourly, FUN=sum)
names(bike.fremont.daily) <- c("Date","Riders")
rm(bike.fremont.hourly)

loginfo("Fremont Bridge bike data loaded; %i records", nrow(bike.fremont.daily))
if(nrow(bike.fremont.daily) == 0) logwarn("Fremont Bridge has no data!")


###################################################
## Bike - Spokane Bridge hourly
###################################################
bike.spokane.hourly <- read.csv(data.files[["bike.spokane"]], stringsAsFactors=F, colClasses=c(NA, "integer", "integer", "integer"), col.names=c("Date","Riders","West","East"))
# convert dates
bike.spokane.hourly$Date <- as.Date(bike.spokane.hourly$Date, format="%m/%d/%Y %H:%M:%S %p")
# Order by date
bike.spokane.hourly <- bike.spokane.hourly[order(bike.spokane.hourly[,1]),]
# simplify to daily numbers
bike.spokane.daily <- aggregate(Riders ~ Date, data=bike.spokane.hourly, FUN=sum)
rm(bike.spokane.hourly)

loginfo("Spokane Bridge bike data loaded; %i records", nrow(bike.spokane.daily))
if(nrow(bike.spokane.daily) == 0) logwarn("Spokane Bridge has no data!")


###################################################
## Road - WSDOT Monthly Vehicle Miles by County
###################################################
road.wsdot.mvmt <- read.xls(data.files[["road.wsdot"]], sheet=4, pattern="Jan MVMT", blank.lines.skip=T, stringsAsFactors=F, colClasses=c(NA, NA, "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas", "num.with.commas"))
road.wsdot.mvmt <- road.wsdot.mvmt[road.wsdot.mvmt$County == "King", 1:14]
# convert columns to date observations
road.wsdot.mvmt <- melt(road.wsdot.mvmt, id.vars=c("Year","County"), variable.name="Month", value.name="Monthly.Miles")
# Fix dates
road.wsdot.mvmt$Month <- sub(".MVMT", "", road.wsdot.mvmt$Month, fixed=T)
road.wsdot.mvmt$Date <- as.Date(paste("1", road.wsdot.mvmt$Month, road.wsdot.mvmt$Year), format="%d %b %Y")
# Cleanup
road.wsdot.mvmt <- subset(road.wsdot.mvmt, select=c("Date","Monthly.Miles"))
# We are missing an entire year's worth of observations; let's kill everything before that
road.wsdot.mvmt <- road.wsdot.mvmt[road.wsdot.mvmt$Date >= "2008-01-01",]
# Order by date
road.wsdot.mvmt <- road.wsdot.mvmt[order(road.wsdot.mvmt[,1]),]

loginfo("WSDOT Traffic data loaded; %i records", nrow(road.wsdot.mvmt))
if(nrow(road.wsdot.mvmt) == 0) logwarn("WSDOT Traffic has no data!")


###################################################
## Road - SDOT Automatic Vehicle Survey by Intersection
###################################################
## AMPK/PMPK = AM Peak / PM Peak
## AWDT = Average Weekday Traffic
## ADT = Average Daily Traffic
###################################################
road.sdot.adt <- read.csv(data.files[["road.sdot"]], stringsAsFactors=F, colClasses=c(NA, "factor", NA, "factor", "factor", "factor", "character", NA, NA, NA, NA, NA, NA), col.names=c("Id","Location","Comp.Key","Unit.Id","Direction","Lane","Date","AMPK","PMPK","AWDT","ADT","Weekdays","Days"))
# convert dates
road.sdot.adt$Date <- as.Date(road.sdot.adt$Date, format="%m/%d/%y")
# Order by date
road.sdot.adt <- road.sdot.adt[order(road.sdot.adt[,7]),]
# Impute '0' ADT values using AWDT
road.sdot.adt[road.sdot.adt$ADT == 0,]$ADT <- road.sdot.adt[road.sdot.adt$ADT == 0,]$AWDT
# Remove all rows with non-STANDARD Lane values
road.sdot.adt <- road.sdot.adt[road.sdot.adt$Lane == "STANDARD",]

# use ADT for measurement

loginfo("SDOT Traffic data loaded; %i records", nrow(road.sdot.adt))
if(nrow(road.sdot.adt) == 0) logwarn("SDOT traffic has no data!")


###################################################
## Bus - KC Metro VanPool monthly average boardings
###################################################
road.vanpool.monthly <- read.xls(data.files[["bus.kcmetro"]], sheet=3, skip=1, blank.lines.skip=T, stringsAsFactors=F, nrows=5)
# convert this into monthly observations
road.vanpool.monthly <- melt(road.vanpool.monthly, c("Year"))
road.vanpool.monthly$Date <- as.Date(paste("01",road.vanpool.monthly$variable, road.vanpool.monthly$Year), format="%d %b %Y")
road.vanpool.monthly <- road.vanpool.monthly[,c("Date", "value")]
names(road.vanpool.monthly) <- c("Date", "Boardings")
# sort
road.vanpool.monthly <- road.vanpool.monthly[order(road.vanpool.monthly[,1]),]

loginfo("KC Metro Vanpool loaded; %i records", nrow(road.vanpool.monthly))
if(nrow(road.vanpool.monthly) == 0) logwarn("KC Metro Vanpool has no data!")


###################################################
## Bus - KC Metro monthly average boardings
###################################################
bus.kcmetro.monthly <- read.xls(data.files[["bus.kcmetro"]], sheet=1, skip=1, blank.lines.skip=T, stringsAsFactors=F, nrows=5)
# convert this into monthly observations
bus.kcmetro.monthly <- melt(bus.kcmetro.monthly, c("Year"))
bus.kcmetro.monthly$Date <- as.Date(paste("01",bus.kcmetro.monthly$variable, bus.kcmetro.monthly$Year), format="%d %b %Y")
bus.kcmetro.monthly <- bus.kcmetro.monthly[,c("Date", "value")]
names(bus.kcmetro.monthly) <- c("Date", "Boardings")
# sort
bus.kcmetro.monthly <- bus.kcmetro.monthly[order(bus.kcmetro.monthly[,1]),]

loginfo("KC Metro bus data loaded; %i records", nrow(bus.kcmetro.monthly))
if(nrow(bus.kcmetro.monthly) == 0) logwarn("KC Metro Bus no data!")


###################################################
## Bus - KC Metro RapidRide monthly average boardings
###################################################
bus.rapidride.monthly <- read.xls(data.files[["bus.kcmetro"]], sheet=2, skip=1, blank.lines.skip=T, stringsAsFactors=F, nrows=10)
# Remove "baseline" rows
bus.rapidride.monthly <- bus.rapidride.monthly[bus.rapidride.monthly$Year != "Baseline",]
# convert this into monthly observations
bus.rapidride.monthly <- melt(bus.rapidride.monthly, c("Year"))
bus.rapidride.monthly$Date <- as.Date(paste("01",bus.rapidride.monthly$variable, bus.rapidride.monthly$Year), format="%d %b %Y")
bus.rapidride.monthly <- bus.rapidride.monthly[,c("Date", "value")]
names(bus.rapidride.monthly) <- c("Date", "Boardings")
# sort
bus.rapidride.monthly <- bus.rapidride.monthly[order(bus.rapidride.monthly[,1]),]

loginfo("Rapid Ride data loaded; %i records", nrow(bus.rapidride.monthly))
if(nrow(bus.rapidride.monthly) == 0) logwarn("Rapid Ride has no data!")


################################
# Combine into sets for analysis
################################

## Set 1: Bike lanes 2014-2016
loginfo("-- Aggregating bike lane data into weekly series --")

bike.min.common <- max(min(bike.broadway.daily$Date),min(bike.burke.daily$Date),min(bike.fremont.daily$Date),min(bike.spokane.daily$Date))
bike.max.common <- min(max(bike.broadway.daily$Date),max(bike.burke.daily$Date),max(bike.fremont.daily$Date),max(bike.spokane.daily$Date))

loginfo("Bike series date range: %s - %s", bike.min.common, bike.max.common)

bike.all.daily <- cbind(
  data.frame(Date = seq.Date(bike.min.common, bike.max.common, by="day")),
  data.frame(Broadway = bike.broadway.daily[bike.broadway.daily$Date >= bike.min.common & bike.broadway.daily$Date <= bike.max.common,]$Riders),
  data.frame(Burke = bike.burke.daily[bike.burke.daily$Date >= bike.min.common & bike.burke.daily$Date <= bike.max.common,]$Riders),
  data.frame(Fremont = bike.fremont.daily[bike.fremont.daily$Date >= bike.min.common & bike.fremont.daily$Date <= bike.max.common,]$Riders),
  data.frame(Spokane = bike.spokane.daily[bike.spokane.daily$Date >= bike.min.common & bike.spokane.daily$Date <= bike.max.common,]$Riders)
)

loginfo("Attempting imputation of missing bike data; %i missing values", sum(is.na(bike.all.daily)))

bike.all.daily.imp <- mice(bike.all.daily[,-1], m=5, maxit=50, meth="norm.nob", seed=1234)
#densityplot(bike.all.daily.imp) # lattice
# increase m for model verification
# modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
# summary(pool(modelFit1))

#t <- amelia(bike.all.daily, ts="Date", lags="Riders", leads="Riders", polytime=2)
#tmp <- mice(bike.all.daily, m=5, maxit=50, meth="rf", seed=1234)

## Temp: take one imputation and run with it
loginfo("Aggregating bike data using imputation series 1; May introduce artifacts")
bike.all.daily <- data.frame(Date = bike.all.daily$Date, complete(bike.all.daily.imp, 1))
# Fix an anomaly of imputation
bike.all.daily[which(bike.all.daily$Burke < 0),]$Burke <- 0

bike.all.daily <- data.frame(Date=bike.all.daily$Date, Riders=rowSums(bike.all.daily[,-1]))
bike.all.daily$Week <- format(bike.all.daily$Date, format="%V")
bike.all.daily$WkYear <- format(bike.all.daily$Date, format="%G")

set.bike.14.16 <- aggregate(Riders ~ Week + WkYear, data=bike.all.daily, FUN=sum)
set.bike.14.16 <- merge(set.bike.14.16, fuel.prices.weekly, sort=F)
set.bike.14.16 <- set.bike.14.16[-1,c("Date","Riders","Avg.Price")] # drop incomplete first week and reorder

set.bike.14.16[,-1] <- sapply(set.bike.14.16[,-1], FUN=function(x){
  scale(x, center=F, scale=max(x, na.rm=T))
})

loginfo("Bike series data; %i records", nrow(set.bike.14.16))
if(nrow(set.bike.14.16) == 0) logwarn("Bike series has no data!")


## Set 2: roads & Metro 2011-2014

loginfo("-- Aggregating roads & Metro data into monthly series --")

road.min.common <- max(min(road.wsdot.mvmt$Date),min(road.vanpool.monthly$Date),min(bus.kcmetro.monthly$Date),min(bus.rapidride.monthly$Date))
road.max.common <- min(max(road.wsdot.mvmt$Date),max(road.vanpool.monthly$Date),max(bus.kcmetro.monthly$Date),max(bus.rapidride.monthly$Date))

loginfo("Road & Metro series date range: %s - %s", road.min.common, road.max.common)

set.roads.11.14 <- cbind(
  data.frame(Date = seq.Date(road.min.common, road.max.common, by="month")),
  data.frame(Traffic = road.wsdot.mvmt[road.wsdot.mvmt$Date >= road.min.common & road.wsdot.mvmt$Date <= road.max.common,]$Monthly.Miles),
  data.frame(Vanpool = road.vanpool.monthly[road.vanpool.monthly$Date >= road.min.common & road.vanpool.monthly$Date <= road.max.common,]$Boardings),
  data.frame(Bus = bus.kcmetro.monthly[bus.kcmetro.monthly$Date >= road.min.common & bus.kcmetro.monthly$Date <= road.max.common,]$Boardings),
  data.frame(Rapidride = bus.rapidride.monthly[bus.rapidride.monthly$Date >= road.min.common & bus.rapidride.monthly$Date <= road.max.common,]$Boardings)
)

set.roads.11.14 <- merge(set.roads.11.14, fuel.prices.monthly[,c("Date","Avg.Price")], sort=F)

set.roads.11.14[,-1] <- sapply(set.roads.11.14[,-1], FUN=function(x){
  scale(x, center=F, scale=max(x, na.rm=T))
})

loginfo("Roads & Metro series data; %i records", nrow(set.roads.11.14))
if(nrow(set.roads.11.14) == 0) logwarn("Roads & Metro series has no data!")


## Set 3: all 2014

loginfo("-- Aggregating all 2014 data into monthly series --")

set.2014.min <- as.Date("2014-01-01")
set.2014.max <- as.Date("2014-12-31")

loginfo("2014 series date range: %s - %s", set.2014.min, set.2014.max)

# aggregate rail into months
bike.all.daily$Month <- format(bike.all.daily$Date, format="%m")
bike.all.daily$Year <- format(bike.all.daily$Date, format="%Y")

rail.link.2014$Month <- format(rail.link.2014$Date, format="%m")
rail.link.2014$Year <- format(rail.link.2014$Date, format="%Y")

rail.link.monthly <- aggregate(Daily.Ridership ~ Month + Year, data=rail.link.2014, FUN=sum)

set.2014 <- aggregate(Riders ~ Month + Year, data=bike.all.daily[bike.all.daily$Date >= set.2014.min & bike.all.daily$Date <= set.2014.max,], FUN=sum)
set.2014 <- merge(set.2014, fuel.prices.monthly, sort=F)
set.2014 <- set.2014[,c("Date","Riders","Avg.Price")] # reorder
names(set.2014) <- c("Date","Bike","Avg.Price")

set.2014 <- cbind(
  set.2014,
  data.frame(Traffic = road.wsdot.mvmt[road.wsdot.mvmt$Date >= set.2014.min & road.wsdot.mvmt$Date <= set.2014.max,]$Monthly.Miles),
  data.frame(Vanpool = road.vanpool.monthly[road.vanpool.monthly$Date >= set.2014.min & road.vanpool.monthly$Date <= set.2014.max,]$Boardings),
  data.frame(Bus = bus.kcmetro.monthly[bus.kcmetro.monthly$Date >= set.2014.min & bus.kcmetro.monthly$Date <= set.2014.max,]$Boardings),
  data.frame(Rapidride = bus.rapidride.monthly[bus.rapidride.monthly$Date >= set.2014.min & bus.rapidride.monthly$Date <= set.2014.max,]$Boardings),
  data.frame(Light.Rail = rail.link.monthly$Daily.Ridership)
)

set.2014[,-1] <- sapply(set.2014[,-1], FUN=function(x){
  scale(x, center=F, scale=max(x, na.rm=T))
})

loginfo("2014 series data; %i records", nrow(set.2014))
if(nrow(set.2014) == 0) logwarn("2014 series has no data!")

## Set #4: roads only

loginfo("-- Aggregating roads data into monthly series --")

roads.min <- min(road.wsdot.mvmt$Date)
roads.max <- max(road.wsdot.mvmt$Date)

loginfo("Roads series date range: %s - %s", roads.min, roads.max)

set.roads.only <- cbind(
  data.frame(Date = seq.Date(roads.min, roads.max, by="month")),
  data.frame(Traffic = road.wsdot.mvmt[road.wsdot.mvmt$Date >= roads.min & road.wsdot.mvmt$Date <= roads.max,]$Monthly.Miles)
)

set.roads.only <- merge(set.roads.only, fuel.prices.monthly[,c("Date","Avg.Price")], sort=F)

set.roads.only[,-1] <- sapply(set.roads.only[,-1], FUN=function(x){
  scale(x, center=F, scale=max(x, na.rm=T))
})

loginfo("Road series data; %i records", nrow(set.roads.only))
if(nrow(set.roads.only) == 0) logwarn("Road series has no data!")


loginfo("-- Data load complete --")
