
# Set WD relative to this script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

.funct.loaded = F
source("./function_lib.R")
source("./data_load_and_prep.R")

## Analysis libraries
load_libraries(c("TSclust","dtw","forecast","lmtest","vars","tseries","dynlm"))

# set original pars for reset
opar=par(no.readonly=T)

if(interactive()){
  
#################
## Data Discovery
#################

## Fuel prices
generateExploratoryGraphs(fuel.prices.weekly, "Avg.Price", "Seattle Weekly Average Fuel Prices")

generateExploratoryGraphs(fuel.prices.monthly, "Avg.Price", "Seattle Monthly Average Fuel Prices")

generateExploratoryGraphs(fuel.prices.weekly[fuel.prices.weekly$Date >= set.2014.min & fuel.prices.weekly$Date <= set.2014.max,], "Avg.Price", "Seattle 2014 Weekly Average Fuel Prices")

## Link rail
generateExploratoryGraphs(rail.link.2014, "Daily.Ridership", "Link Light Rail Ridership, 2014")

## Broadway bike lane
generateExploratoryGraphs(bike.broadway.daily, "Riders", "Broadway Bike Track Riders")

## Burke Gilman trail
generateExploratoryGraphs(bike.burke.daily, "Riders", "Burke Gilman Trail Riders")

## Fremont Bridge bike lanes
generateExploratoryGraphs(bike.fremont.daily, "Riders", "Fremont Bridge Bike Crossings")

## Spokane Bridge bike lanes
generateExploratoryGraphs(bike.spokane.daily, "Riders", "Spokane Bridge Bike Crossings")

## WSDOT highway traffic estimates
generateExploratoryGraphs(road.wsdot.mvmt, "Monthly.Miles", "WSDOT Vehicle Miles for King County")

## KC Metro VanPool
generateExploratoryGraphs(road.vanpool.monthly, "Boardings", "Van Pool Boardings")

## KC Metro Transit
generateExploratoryGraphs(bus.kcmetro.monthly, "Boardings", "King County Metro Monthly Bus Boardings")

## KC Metro RapidRide
generateExploratoryGraphs(bus.rapidride.monthly, "Boardings", "Rapid Ride Monthly Bus Boardings")


#################
## Analysis
#################


loginfo("-- Analyzing Average Price versus Traffic --")

###################################################
## Test: Fuel vs Traffic - is there a relationship?
##

if(nrow(set.roads.only) == 0) {
  logerror("Roads data series has no data!")
  stop("Roads data series has no data!")
} 

## Generate Timeseries
t.roads <- ts(set.roads.only[,-1], start=c(as.numeric(format(set.roads.only$Date[1], "%Y")), as.numeric(format(set.roads.only$Date[1], "%m"))), end=c(as.numeric(format(set.roads.only$Date[nrow(set.roads.only)], "%Y")), as.numeric(format(set.roads.only$Date[nrow(set.roads.only)], "%m"))), frequency=12)


##-------------- Normality Checks --------------------##

loginfo("Performing normality check on data")

loginfo("Reject skewness=zero and excess kurtosis=zero for Traffic: %s", jarque.bera.test(t.roads[,1])$p.value < 0.05)
# jb null not rejected; no evidence of normality

loginfo("Reject skewness=zero and excess kurtosis=zero for Avg.Price: %s", jarque.bera.test(t.roads[,2])$p.value < 0.05)
# jb null rejected; no evidence of normality


##-------------- Autocorrelation Checks --------------------##

loginfo("Checking for autocorrelation disturbances between Traffic ~ Avg.Price")

# Check for autocorrelation disturbances
set.roads.only.dw <- dwtest(Traffic ~ Avg.Price, data=t.roads)
loginfo("Reject autocorrelation = 0 without differences: %s", set.roads.only.dw$p.value < 0.05)


##--------------Stationarity Checks --------------------##

loginfo("Performing Stationarity checks & differencing")

t.roads.stat <- getRequiredDiffs(t.roads)

for(var in dimnames(t.roads)[[2]]) {
  if(t.roads.stat[var] > 0){
    loginfo("Performing diff(%g) on %s", t.roads.stat[var], var)
    t.roads[,var] <- c(rep(0, times=t.roads.stat[[var]]), diff(t.roads[,var], differences=t.roads.stat[[var]]))
  }
}


##-------------- Model Generation --------------------##

loginfo("Proceeding with linear model at one difference: diff(Traffic) ~ diff(Avg.Price)")
# lm at lag=1
set.roads.only.lm <- lm(Traffic ~ Avg.Price, data=t.roads)
summary(set.roads.only.lm)
loginfo("Significant variables: %s", names(summary(set.roads.only.lm)$coefficients[1:2,4])[summary(set.roads.only.lm)$coefficients[1:2,4] < 0.05])
loginfo("Coefficients:")
loginfo("%s", names(coef(set.roads.only.lm)))
loginfo("%s", coef(set.roads.only.lm))
# slope of Avg.Price is +0.3561


##-------------- Normality Checks --------------------##

loginfo("Performing normality check on linear model residuals")

# test null normality in lm at lag=1
loginfo("Reject skewness=zero and excess kurtosis=zero in model: %s", jarque.bera.test(resid(set.roads.only.lm))$p.value < 0.05)
# jb null not rejected; no evidence of normality


##-------------- Model Confirmation --------------------##

loginfo("Performing dynamic linear model with variations of Avg.Price")

# check lagged price
loginfo("Significant variables in model: Avg.Price + L(Avg.Price): %s", names(summary(dynlm(Traffic ~ Avg.Price + L(Avg.Price), data=t.roads))$coefficients[1:2,4])[summary(dynlm(Traffic ~ Avg.Price + L(Avg.Price), data=t.roads))$coefficients[1:2,4] < 0.05])
# price is significant, but not lagged price

loginfo("Significant variables in model: Avg.Price + trend(Avg.Price): %s", names(summary(dynlm(Traffic ~ Avg.Price + trend(Avg.Price), data=t.roads))$coefficients[1:2,4])[summary(dynlm(Traffic ~ Avg.Price + trend(Avg.Price), data=t.roads))$coefficients[1:2,4] < 0.05])
# again, price but not trend(price)

loginfo("Significant variables in model: Avg.Price + harmon(Avg.Price): %s", names(summary(dynlm(Traffic ~ Avg.Price + harmon(Avg.Price), data=t.roads))$coefficients[1:4,4])[summary(dynlm(Traffic ~ Avg.Price + harmon(Avg.Price), data=t.roads))$coefficients[1:4,4] < 0.05])
# slight significance in harmon(Avg.Price)sin


##-------------- Causality Checks --------------------##

loginfo("Proceeding with Granger Causality at diff(1) and lm Traffic ~ Avg.Price")

# We have stationary series (at 1 diff) that have evidence of a correlation
# When a cointegrating relationship exists between two 
# series, Granger causality must exist as well

# Granger Causality tests at order = 1 lag
loginfo("Granger test shows Avg.Price effect on Traffic at lag=1: %s", grangertest(Traffic ~ Avg.Price, data=t.roads, order=1)[["Pr(>F)"]][2] < 0.05)
# ANOVA says p = 0.0021; Price affects traffic

loginfo("Granger test shows Traffic effect on Avg.Price at lag=1: %s", grangertest(Avg.Price ~ Traffic, data=t.roads, order=1)[["Pr(>F)"]][2] < 0.05)
# ANOVA says p = 0.938 - Traffic definitely does not affect Price

# let's iterate through lags to see how far out the lags go
iter <- 10
par(opar)
par(mfrow=c(1,2))
plot(1:iter, sapply(1:iter, FUN=function(i){grangertest(Traffic ~ Avg.Price, data=t.roads, order=i)[["Pr(>F)"]] })[2,], type="l", xlab="Order", ylab="Pr>F", ylim=c(0,1), main="Lags Traffic ~ Avg.Price")
abline(h=0.05, col="green")

plot(1:iter, sapply(1:iter, FUN=function(i){grangertest(Avg.Price ~ Traffic, data=t.roads, order=i)[["Pr(>F)"]] })[2,], type="l", xlab="Order", ylab="Pr>F", ylim=c(0,1), main="Lags Avg.Price ~ Traffic")
abline(h=0.05, col="red")
par(opar)
# Avg.Price is highly predictive below lag 3; falls off beyond that
# 2-3 months of previous price data is predictive of current traffic values


##-------------- DTW Confirmation --------------------##

# Dynamic Time Warping
# makes pretty pictures
plot(dtw(t.roads[,"Traffic"], t.roads[,"Avg.Price"], keep=T, step=rabinerJuangStepPattern(6,"c")), type="threeway", main="Timeseries Alignment", xlab="Traffic Index", ylab="Price Index")



loginfo("-- Analyzing Average Price versus Bike Transportation --")

#######################################################
## Test: Bike lanes 2014-2016 - is there a relationship 
##       between fuel price and bike use?

if(nrow(set.bike.14.16) == 0) {
  logerror("Bike data series has no data!")
  stop("Bike data series has no data!")
}

## Generate Timeseries
t.bikes <- ts(set.bike.14.16[,-1], start=c(as.numeric(format(set.bike.14.16$Date[1], "%G")), as.numeric(format(set.bike.14.16$Date[1], "%V"))), frequency=52)


##-------------- Normality Checks --------------------##

loginfo("Performing normality check on bike data")

loginfo("Reject skewness=zero and excess kurtosis=zero for Riders: %s", jarque.bera.test(t.bikes[,1])$p.value < 0.05)
# jb null not rejected; no evidence of normality

loginfo("Reject skewness=zero and excess kurtosis=zero for Avg.Price: %s", jarque.bera.test(t.bikes[,2])$p.value < 0.05)
# jb null rejected; not normal


##-------------- Autocorrelation Checks --------------------##

loginfo("Checking for autocorrelation disturbances between Riders ~ Avg.Price")

# Check for autocorrelation disturbances
set.bike.14.16.dw <- dwtest(Riders ~ Avg.Price, data=t.bikes)
loginfo("Reject autocorrelation = 0 without differences: %s", set.bike.14.16.dw$p.value < 0.05)


##--------------Stationarity Checks --------------------##

loginfo("Performing Stationarity checks & differencing")

t.bikes.stat <- getRequiredDiffs(t.bikes)

for(var in dimnames(t.bikes)[[2]]) {
  if(t.bikes.stat[var] > 0){
    loginfo("Performing diff(%g) on %s", t.bikes.stat[var], var)
    t.bikes[,var] <- c(rep(0, times=t.bikes.stat[[var]]), diff(t.bikes[,var], differences=t.bikes.stat[[var]]))
  }
}


##-------------- Model Generation --------------------##

loginfo("Proceeding with linear model at one difference: Riders ~ diff2(Avg.Price)")
# lm at lag=1
set.bike.14.16.lm <- lm(Riders ~ Avg.Price, data=t.bikes)
summary(set.bike.14.16.lm)
loginfo("Significant variables: %s", names(summary(set.bike.14.16.lm)$coefficients[1:2,4])[summary(set.bike.14.16.lm)$coefficients[1:2,4] < 0.05])
loginfo("Coefficients:")
loginfo("%s", names(coef(set.bike.14.16.lm)))
loginfo("%s", coef(set.bike.14.16.lm))


##-------------- Normality Checks --------------------##

loginfo("Performing normality check on linear model residuals")

# test null normality in lm at lag=1
loginfo("Reject skewness=zero and excess kurtosis=zero in model: %s", jarque.bera.test(resid(set.bike.14.16.lm))$p.value < 0.05)
# jb null not rejected; no evidence of normality


##-------------- Model Confirmation --------------------##

loginfo("Performing dynamic linear model with variations of Avg.Price")

# check lagged price
loginfo("Significant variables in model: Avg.Price + L(Avg.Price): %s", names(summary(dynlm(Riders ~ Avg.Price + L(Avg.Price), data=t.bikes))$coefficients[1:2,4])[summary(dynlm(Riders ~ Avg.Price + L(Avg.Price), data=t.bikes))$coefficients[1:2,4] < 0.05])
# only intercept is significant

loginfo("Significant variables in model: Avg.Price + trend(Avg.Price): %s", names(summary(dynlm(Riders ~ Avg.Price + trend(Avg.Price), data=t.bikes))$coefficients[1:2,4])[summary(dynlm(Riders ~ Avg.Price + trend(Avg.Price), data=t.bikes))$coefficients[1:2,4] < 0.05])
# only intercept is significant

loginfo("Significant variables in model: Avg.Price + harmon(Avg.Price): %s", names(summary(dynlm(Riders ~ Avg.Price + harmon(Avg.Price), data=t.bikes))$coefficients[1:4,4])[summary(dynlm(Riders ~ Avg.Price + harmon(Avg.Price), data=t.bikes))$coefficients[1:4,4] < 0.05])
# significance in intercept and harmon(Avg.Price)cos


##-------------- Causality Checks --------------------##

loginfo("Proceeding with Granger Causality with lm Riders ~ diff2(Avg.Price)")

# No correlation detected, but let's verify anyway

# Granger Causality tests at order = 1 lag
loginfo("Granger test shows Avg.Price effect on Riders at lag=1: %s", grangertest(Riders ~ Avg.Price, data=t.bikes, order=1)[["Pr(>F)"]][2] < 0.05)
# ANOVA says p = 0.775; Price does not affect Riders

loginfo("Granger test shows Riders effect on Avg.Price at lag=1: %s", grangertest(Avg.Price ~ Riders, data=t.bikes, order=1)[["Pr(>F)"]][2] < 0.05)
# ANOVA says p = 0.298 - Riders does not affect Price though it is more significant than the reverse

# let's iterate through lags to see how far out the lags go
iter <- 10
par(opar)
par(mfrow=c(1,2))
plot(1:iter, sapply(1:iter, FUN=function(i){grangertest(Riders ~ Avg.Price, data=t.bikes, order=i)[["Pr(>F)"]] })[2,], type="l", xlab="Order", ylab="Pr>F", ylim=c(0,1), main="Lags Riders ~ Avg.Price")
abline(h=0.05, col="green")

plot(1:iter, sapply(1:iter, FUN=function(i){grangertest(Avg.Price ~ Riders, data=t.bikes, order=i)[["Pr(>F)"]] })[2,], type="l", xlab="Order", ylab="Pr>F", ylim=c(0,1), main="Lags Avg.Price ~ Riders")
abline(h=0.05, col="red")
par(opar)
# Avg.Price is not predictive


##-------------- DTW Confirmation --------------------##

# Dynamic Time Warping
# makes pretty pictures
plot(dtw(t.bikes[,"Riders"], t.bikes[,"Avg.Price"], keep=T, step=rabinerJuangStepPattern(6,"c")), type="threeway", main="Timeseries Alignment", xlab="Riders Index", ylab="Price Index")



loginfo("-- Analyzing Cointegration of Price, Traffic, and Transit --")


#########################################################################
## Test: Roads & Transit 2011-2014 - is there a symetric effect on transit?

if(nrow(set.roads.11.14) == 0) {
  logerror("Roads & Metro data series has no data!")
  stop("Roads & Metro data series has no data!")
}

# Generate monthly time series
t <- ts(set.roads.11.14[,-1], start=c(as.numeric(format(set.roads.11.14$Date[1], "%Y")), as.numeric(format(set.roads.11.14$Date[1], "%m"))), end=c(as.numeric(format(set.roads.11.14$Date[nrow(set.roads.11.14)], "%Y")), as.numeric(format(set.roads.11.14$Date[nrow(set.roads.11.14)], "%m"))), frequency=12)
t <- t[,c(5,1,2,3,4)] # Reorder - presumed cause first

##--------------Stationarity Checks --------------------##

loginfo("Performing Stationarity checks & differencing")

t.stat <- getRequiredDiffs(t)

for(var in dimnames(t)[[2]]) {
  if(t.stat[var] > 0){
    loginfo("Performing diff(%g) on %s", t.stat[var], var)
    t[,var] <- c(rep(0, times=t.stat[[var]]), diff(t[,var], differences=t.stat[[var]]))
  }
}


##--------------Vector Auto-Regression -----------------##

loginfo("Performing Vector Auto-Regression")
loginfo("K = %g", dim(t)[2])

# determine lag order
set.roads.p <- VARselect(t, lag.max=5, season=12, type="both")$selection
loginfo("Optimal p value (lag) %g selected by criterion: %s", min(set.roads.p), names(set.roads.p[set.roads.p == min(set.roads.p)]))
set.roads.p <- min(set.roads.p) 

loginfo("Estimating VAR of Roads & Transit with deterministic trend regressors at lag %g", set.roads.p)
set.roads.var <- VAR(t, p=set.roads.p, type="trend", season=12)

summary(set.roads.var)
#par(opar)
#par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#plot(set.roads.var, nc=2)
#par(opar)

loginfo("Validating VAR(%g) model", set.roads.p)


##--------------VAR Stationarity Checks -----------------##

loginfo("Checking Eigenvalue components of VAR(%g) model:", set.roads.p)
set.roads.eigval <- roots(set.roads.var)
if(length(set.roads.eigval[set.roads.eigval == 1]) > 0){
  logerror("Unit root detected in Eigenvalues; VECM required but not implemented!")
  stop("VECM required but not implemented!")
} else if(length(set.roads.eigval[set.roads.eigval >= 1]) == 0){
  loginfo("VAR is stationary; proceeding with analysis")
} else if(length(set.roads.eigval[set.roads.eigval <= 1]) == 0){
  logwarn("VAR is not stationary; co-integration check and possible data transformation required! Proceeding with analysis anyway")
} else if(quantile(set.roads.eigval)[["75%"]] < 1){
  loginfo("Most VAR Eigenvalues are below 1; proceeding with analysis presuming stationary VAR")
} else {
  logerror("VAR model's Eigenvalues are ambiguous; further inspection necessary!")
  stop("VAR model's Eigenvalues are ambiguous!")
}


##--------------Re-estimate VAR if too few significant terms -----------------##

# Re-estimate VAR based on low significance
# *** need criteria
if(FALSE){
  set.roads.res <- restrict(set.roads.var, method="ser", thresh=2)
  set.roads.res$restrictions
  Acoef(set.roads.res)
  set.roads.var <- set.roads.res
}


##--------------VAR Serial Correlation Test -----------------##

loginfo("Asymptotic Portmanteau test of serial correlation in the residuals")
# test lack of serial correlation in the residuals
# that would mean that incorrect conclusions would be drawn from other tests, or that sub-optimal estimates of model parameters are obtained
set.roads.ser <- serial.test(set.roads.var, lags.pt=set.roads.p, type = "PT.asymptotic") 
set.roads.ser$serial
#plot(set.roads.ser)
loginfo("Autocorrelation of residuals in VAR model detected: %s", as.numeric(set.roads.ser$serial$p.value) >= 0.05)
if(as.numeric(set.roads.ser$serial$p.value) >= 0.05) {
  logwarn("WARNING: autocorrelation in residuals found; analysis results may be sub-optimal!")
}


##--------------VAR Normality Test -----------------##

loginfo("Multivariate Jarque-Bera tests and multivariate skewness and kurtosis tests for the residuals")
set.roads.norm <- normality.test(set.roads.var)
set.roads.norm$jb.mul 
loginfo("Skewness and the excess kurtosis of residuals are zero: %s", set.roads.norm$jb.mul$JB$p.value[1,1] >= 0.05)
if(set.roads.norm$jb.mul$JB$p.value[1,1] < 0.05) {
  logwarn("WARNING: residuals are not normally distributed; analysis results may be sub-optimal!")
}


##--------------VAR ARCH Test -----------------##

loginfo("Multivariate ARCH-LM test")
# test for autoregressive conditional heteroskedasticity
set.roads.arch <- arch.test(set.roads.var, lags.multi=set.roads.p) 
set.roads.arch$arch.mul
#plot(set.roads.arch, names = "Traffic")
loginfo("Serial dependence autoregressive conditional heteroskedasticity effects found: %s", as.numeric(set.roads.arch$arch.mul$p.value) <= 0.05)


##--------------VAR Stability Test -----------------##

loginfo("Testing stability of VAR(%g)", set.roads.p)
set.roads.stab <- stability(set.roads.var)
par(opar)
plot(set.roads.stab, nc=3)
for(var in set.roads.stab$names) {
  # estimating cutoff at 1.35 - would be nice to know the actual value
  if(sum(abs(set.roads.stab$stability[[var]]$process) > 1.35)) {
    logwarn("Unstable empirical fluctuation in OLS CUSUM found in %s", var)
  }
}

loginfo("VAR(%g) tests complete...", set.roads.p)


##--------------Granger and Instantaneous Causality -----------------##

loginfo("Testing Granger and Instantaneous Causality")
set.roads.cause <- causality(set.roads.var, cause=c("Avg.Price"))
loginfo("Fuel price Granger-causes road and transit: %s", set.roads.cause$Granger$p.value[1,1] <= 0.05)
loginfo("Instantaneous causality between fuel price and road / transit: %s", set.roads.cause$Instant$p.value[1,1] <= 0.05)


##--------------Impulse Response Function -----------------##

loginfo("Computing Impulse Response Coefficients and Plot")
set.roads.irf <- irf(set.roads.var, impulse="Avg.Price", response=c("Traffic", "Vanpool", "Bus", "Rapidride"), boot=T, ortho=T, cumulative=F, n.ahead=6, seed=1337)
plot(set.roads.irf)





stop("Stopping execution here", call=F) #/###/####/###/####/###/####/###/####/###/####/###/###

loginfo("-- Analyzing Cointegration of Price and Transportation Modes in 2014 --")


#######################################################
## Test: All in 2014 - How does Price affect all modes?

if(nrow(set.2014) == 0) {
  logerror("2014 data series has no data!")
  stop("2014 data series has no data!")
}

# First, let's have some fun and classify like series by clustering
pred <- diss(set.2014[,-1], "DTWARP", step=rabinerJuangStepPattern(6,"c"))
hc.pred <- hclust(pred)
plot(hc.pred)

# Generate monthly time series
t.2014 <- ts(set.2014[,-1], start=c(as.numeric(format(set.2014$Date[1], "%Y")), as.numeric(format(set.2014$Date[1], "%m"))), end=c(as.numeric(format(set.2014$Date[nrow(set.2014)], "%Y")), as.numeric(format(set.2014$Date[nrow(set.2014)], "%m"))), frequency=12)
t.2014 <- t.2014[,c(2,3,4,5,6,7,1)] # Reorder - presumed cause first

##--------------Stationarity Checks --------------------##

loginfo("Performing Stationarity checks & differencing for 2014")

t.2014.stat <- getRequiredDiffs(t.2014)

for(var in dimnames(t.2014)[[2]]) {
  if(t.2014.stat[var] > 0){
    loginfo("Performing diff(%g) on %s", t.2014.stat[var], var)
    t.2014[,var] <- c(rep(0, times=t.2014.stat[[var]]), diff(t.2014[,var], differences=t.2014.stat[[var]]))
  }
}


##--------------Vector Auto-Regression -----------------##

loginfo("Performing Vector Auto-Regression")
loginfo("K = %g", dim(t.2014)[2])

# determine lag order
set.2014.p <- VARselect(t.2014, lag.max=4, type="both", season=12)$selection
loginfo("Optimal p value (lag) %g selected by criterion: %s", min(set.2014.p), names(set.2014.p[set.2014.p == min(set.2014.p)]))
set.2014.p <- min(set.2014.p) 

loginfo("Estimating VAR of Roads & Transit with deterministic trend regressors at lag %g", set.2014.p)
set.2014.var <- VAR(t.2014, p=set.2014.p, type="trend", season=12)

summary(set.2014.var)
#par(opar)
#par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#plot(set.2014.var, nc=2)
#par(opar)

loginfo("Validating VAR(%g) model", set.2014.p)


##--------------VAR Stationarity Checks -----------------##

loginfo("Checking Eigenvalue components of VAR(%g) model:", set.2014.p)
set.2014.eigval <- roots(set.2014.var)
if(length(set.2014.eigval[set.2014.eigval == 1]) > 0){
  logerror("Unit root detected in Eigenvalues; VECM required but not implemented!")
  stop("VECM required but not implemented!")
} else if(length(set.2014.eigval[set.2014.eigval >= 1]) == 0){
  loginfo("VAR is stationary; proceeding with analysis")
} else if(length(set.2014.eigval[set.2014.eigval <= 1]) == 0){
  logwarn("VAR is not stationary; co-integration check and possible data transformation required! Proceeding with analysis anyway")
} else if(quantile(set.2014.eigval)[["75%"]] < 1){
  loginfo("Most VAR Eigenvalues are below 1; proceeding with analysis presuming stationary VAR")
} else {
  logerror("VAR model's Eigenvalues are ambiguous; further inspection necessary!")
  stop("VAR model's Eigenvalues are ambiguous!")
}


##--------------Re-estimate VAR if too few significant terms -----------------##

# Re-estimate VAR based on low significance
# *** need criteria
if(FALSE){
  set.2014.res <- restrict(set.2014.var, method="ser", thresh=2)
  set.2014.res$restrictions
  Acoef(set.2014.res)
  set.2014.var <- set.2014.res
}


##--------------VAR Serial Correlation Test -----------------##

loginfo("Asymptotic Portmanteau test of serial correlation in the residuals")
# test lack of serial correlation in the residuals
# that would mean that incorrect conclusions would be drawn from other tests, or that sub-optimal estimates of model parameters are obtained
set.2014.ser <- serial.test(set.2014.var, lags.pt=set.2014.p, type = "PT.asymptotic") 
set.2014.ser$serial
#plot(set.roads.ser)
loginfo("Autocorrelation of residuals in VAR model detected: %s", as.numeric(set.2014.ser$serial$p.value) >= 0.05)
if(as.numeric(set.2014.ser$serial$p.value) >= 0.05) {
  logwarn("WARNING: autocorrelation in residuals found; analysis results may be sub-optimal!")
}


##--------------VAR Normality Test -----------------##

loginfo("Multivariate Jarque-Bera tests and multivariate skewness and kurtosis tests for the residuals")
set.2014.norm <- normality.test(set.2014.var)
set.2014.norm$jb.mul 
loginfo("Skewness and the excess kurtosis of residuals are zero: %s", set.2014.norm$jb.mul$JB$p.value[1,1] >= 0.05)
if(set.2014.norm$jb.mul$JB$p.value[1,1] < 0.05) {
  logwarn("WARNING: residuals are not normally distributed; analysis results may be sub-optimal!")
}


##--------------VAR ARCH Test -----------------##

loginfo("Multivariate ARCH-LM test")
# test for autoregressive conditional heteroskedasticity
set.2014.arch <- arch.test(set.2014.var, lags.multi=set.2014.p) 
set.2014.arch$arch.mul
#plot(set.roads.arch, names = "Traffic")
loginfo("Serial dependence autoregressive conditional heteroskedasticity effects found: %s", as.numeric(set.2014.arch$arch.mul$p.value) <= 0.05)


##--------------VAR Stability Test -----------------##

loginfo("Testing stability of VAR(%g)", set.2014.p)
set.2014.stab <- stability(set.2014.var)
par(opar)
plot(set.2014.stab, nc=2)
for(var in set.2014.stab$names) {
  # estimating cutoff at 1.35 - would be nice to know the actual value
  if(sum(abs(set.2014.stab$stability[[var]]$process) > 1.35)) {
    logwarn("Unstable empirical fluctuation found in %s", var)
  }
}

loginfo("VAR(%g) tests complete...", set.2014.p)


##--------------Granger and Instantaneous Causality -----------------##

loginfo("Testing Granger and Instantaneous Causality")
set.2014.cause <- causality(set.2014.var, cause=c("Avg.Price"))
loginfo("Fuel price Granger-causes transportation: %s", set.2014.cause$Granger$p.value[1,1] <= 0.05)
loginfo("Instantaneous causality between fuel price and transportation: %s", set.2014.cause$Instant$p.value[1,1] <= 0.05)


##--------------Impulse Response Function -----------------##

loginfo("Computing Impulse Response Coefficients and Plot")
set.2014.irf <- irf(set.2014.var, impulse="Avg.Price", response=c("Traffic", "Vanpool", "Bus", "Rapidride", "Light.Rail", "Bike"), boot=T, ortho=T, cumulative=F, n.ahead=6, seed=1337)
plot(set.2014.irf)



loginfo("Test run complete at %s", Sys.time())
} # End interactive()
