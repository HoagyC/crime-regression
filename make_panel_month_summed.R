library(dplyr)

setwd("~/crime_regression/")
data.folder <- "data/prepared_datasets"

crime.data <- readRDS(file.path(data.folder, "robbery_by_month_pf.RDS"))
clearup.data <- readRDS(file.path(data.folder, "robbery_clearup_by_month_pf.RDS"))
pop.data <- readRDS("useful_outputs/police_force_populations.RDS")

n.months.clearups <- 12
n.crime.lags <- 12
  
for (n.months in 1:20) {
  n.months.clearups <- n.months
  
  clearups.vector <- clearup.data %>% unlist(use.names = F) %>% as.numeric
  crimes.vector <- crime.data %>% unlist(use.names = F) %>% as.numeric
  clearup.rate.vector <- clearups.vector / crimes.vector
  clearup.rate.vector[is.na(clearup.rate.vector)] <- mean(clearup.rate.vector[!is.na(clearup.rate.vector)])
  
  pfs.vector <- rep(colnames(crime.data), each=nrow(crime.data))
  dates.vector <- rep(rownames(crime.data), times=ncol(crime.data))
  
  pop.vector <- c(0)
  length(pop.vector) <- length(colnames(crime.data))
  for (i in 1:length(pop.vector)) {
    pop.vector[i] <- pop.data[colnames(crime.data)[i], 2]
  }
  pop.vector <- unlist(pop.vector)
  pop.vector <- rep(pop.vector, each=nrow(crime.data))
  if (!length(pop.vector) == length(pfs.vector)) {
    print(paste(length(pop.vector), length(pfs.vector)))
  }
  
  crime.rate.vector <- crimes.vector / pop.vector
  
  print('made primary data vectors')
  
  make.lag <- function(vec, n, date.vec, dates) {
    lagged.vec <- c(rep(NA, n), head(vec, -n))
    remove.dates <- head(dates, n)
    lagged.vec[date.vec %in% remove.dates] <- NA
    return(lagged.vec)
  }
  
  make.summed.lag <- function(crime.vec, clearup.vec, n, date.vec, dates) {
    cumulative.crime <- cumsum(crime.vec)
    cumulative.clearups <- cumsum(clearup.vec)
    last.n.crime <- c(rep_len(NA, n), tail(cumulative.crime, -n) - c(0, head(cumulative.crime, -(n + 1))))
    last.n.clearups <- c(rep_len(NA, n), tail(cumulative.clearups, -n) - c(0, head(cumulative.clearups, -(n + 1))))
    remove.dates <- head(dates, n)
    last.n.crime[date.vec %in% remove.dates] <- NA
    last.n.clearups[date.vec %in% remove.dates] <- NA
    lagged.vec <- last.n.clearups / last.n.crime
    return(lagged.vec)
  }
  
  make.average.lag <- function(vec, n) {
    return(c(NA, head(rollmeanr(vec, k=n, fill=NA), -1)))
  }
  
  make.n.lags <- function(vec, n, date.vec, dates) {
    lags.vec <- list()
    for (i in 1:n) {
      new.lag <- make.lag(vec, i, date.vec, dates)
      lags.vec[i] <- list(new.lag)
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
  
  crime.lags <- make.n.lags(crime.rate.vector, n.crime.lags, dates.vector, rownames(crime.data))
  clearup.rate.lag <- make.summed.lag(crimes.vector, clearups.vector, n.months.clearups, dates.vector, rownames(crime.data))
  
  print('made lag vectors')
  
  
  panel.data <- data.frame(pfs.vector, dates.vector, crime.rate.vector)
  
  for (crime.lag in crime.lags) {
    panel.data <- cbind(panel.data, unlist(crime.lag))
  }
  
  panel.data <- cbind(panel.data, clearup.rate.lag)
  
  colnames(panel.data) <- c("pf", "date", "crimerate", 
                            make.lag.names("crimes", n.crime.lags),
                            paste("clearup", n.months.clearups, "m", sep="")
  )
  
  save.name <- paste("robbery_by_month_", n.months, ".RDS", sep="")
  saveRDS(panel.data, file.path(data.folder, save.name))
  print(paste("saved data as", save.name))
}
