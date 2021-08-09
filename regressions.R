library(plm)  

data.folder <- "data"

crime.data <- readRDS(file.path(data.folder, "crime_data.rds"))
clearup.data <- readRDS(file.path(data.folder, "clearup_data.rds"))

clearups.vector <- unlist(clearup.data, use.names = F)
crimes.vector <- unlist(crime.data, use.names = F)
lsoas.vector <- rep(colnames(crime.data), each=nrow(crime.data))
dates.vector <- rep(rownames(crime.data), times=ncol(crime.data))

make.lag <- function(vec, n, date.vec, dates) {
  lagged.vec <- c(rep(NA, n), head(vec, -n))
  remove.dates <- head(dates, n)
  lagged.vec[date.vec %in% remove.dates] <- NA
  return(lagged.vec)
}

 qmake.n.lags <- function(vec, n, date.vec, dates) {
  lags.vec <- c()
  for (i in 1:n) {
    new.lag <- make.lag(vec, n, date.vec, dates)
    lags.vec <- append(lags.vec, new.lag)
  }
  return(lags.vec)
}

make.lag.names <- function(name, n) {
  names <- c()
  for (i in 1:n) {
    names <- append(names, paste("l", name, toString(i), sep=""))
  }
  return(names)
}

crime.lags <- make.n.lags(crimes.vector, 4, dates.vector, rownames(crime.data))
clearup.lags <- make.n.lags(clearups.vector, 4, dates.vector, rownames(crime.data))

panel.data <- data.frame(lsoas.vector, dates.vector, crimes.vector, 
                         cr,
                         clearups.lag.1, clearups.lag.2, clearups.lag.3, clearups.lag.4)



colnames(panel.data) <- c("lsoa", "date", "crimes", 
                          "lcrimes1", "lcrimes2", "lcrimes3", "lcrimes4",
                          "lclearups1", "lclearups2", "lclearups3", "lclearups4")


fixed.effects.reg <- plm(crimes ~ lcrimes1 + lcrimes2 + lcrimes3 + lcrimes4 + 
                           lclearups1 + lclearups2 + lclearups3 + lclearups4, 
                         data=panel.data, index=c("lsoa", "date"), model="within")

summary(fixed.effects.reg)
