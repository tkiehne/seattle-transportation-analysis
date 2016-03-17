# Check if we've already been sourced
if(exists(".funct.loaded") & .funct.loaded==F){
  .funct.loaded <- T

###################
## Helper functions
###################

#' Load required packages and install if necessary
#' 
#' @param pkg string or character vector of package(s) to load
#' @return void
#' 
load_libraries <- function(pkgs) {
  for(pkg in pkgs){
    if (!require(pkg,character.only = TRUE)) {
      install.packages(pkg,dep=TRUE)
      if(!require(pkg,character.only = TRUE)) stop("Required package could not be installed")
    }
  }
}

# Global / utility libraries
load_libraries(c("logging"))

# Logging setup
basicConfig()
addHandler(writeToFile, file=paste0("./logs/analysis-", as.numeric(as.POSIXct(Sys.time())), ".log"))
if(interactive()){
  setLevel("DEBUG")
} else {
  setLevel("ERROR")
}
loginfo("Starting test run at %s", Sys.time())

## Set a class to process string numerics containing commas
setClass("num.with.commas")
setAs("character", "num.with.commas", function(from) as.numeric(gsub(",", "", from)))


#' Helper to impute a time series to ensure evenly spaced dates
#' 
#' @param dates vector of Date objects
#' @param interval string name of time interval (e.g.: day, week) 
#' @return vector of Date objects missing from the provided data
#' 
generateMissingDates <- function(dates, interval) {
  dateseq = seq.Date(dates[1], dates[length(dates)], by=interval)
  dateseq[!(dateseq %in% dates)]
}


#' Helper to generate exploratory plot and hist for data
#' 
#' @param df data frame containing data to plot
#' @param var variable in data frame to plot
#' @param title main title for plots 
#' @return void
#' 
generateExploratoryGraphs <- function(df, var, title) {
  opar=par(no.readonly=T)
  par(mfrow=c(1,2), mar=c(5.1, 4.1, 1, 2.1), oma=c(0,0,1,0))
  plot(df$Date, df[[var]], type="l", xlab="Date", ylab=var)
  hist(df[[var]], breaks=30, xlab=var, main="", freq=F)
  abline(v=mean(df[[var]]), col="blue")
  abline(v=median(df[[var]]), col="red")
  curve(dnorm(x, mean=mean(df[[var]]), sd=sd(df[[var]])), add=TRUE, col="darkblue", lty=2)
  mtext(title, outer=T, cex=1.2)
  par(opar)
}


#' Calculate ndiffs and nsdiffs for time series
#' 
#' @param ts time series as data frame or ts/mts
#' @return list containing number of diffs required for each variable
#' 
getRequiredDiffs <- function(ts){
  ret = vector("list", dim(ts)[2])
  for(var in dimnames(ts)[[2]]) {
    ret[var] = ndiffs(ts[,var], alpha=0.05, test=c("kpss")) + nsdiffs(ts[,var], m=12, test="ocsb", max.D=1)
  }
  ret
}

} # close load check
