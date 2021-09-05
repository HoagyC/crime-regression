library(dplyr)

setwd("~/crime_regression/")
data.folder <- "data/prepared_datasets"

crime.data <- readRDS(file.path(data.folder, "burglary_by_month_pf.RDS"))
clearup.data <- readRDS(file.path(data.folder, "burglary_clearup_by_month_pf.RDS"))
pop.data <- readRDS("useful_outputs/police_force_populations.RDS")

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
print(pop.vector)
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

n.crime.lags <- 4
n.clearup.lags <- 12
crime.lags <- make.n.lags(crime.rate.vector, n.crime.lags, dates.vector, rownames(crime.data))
clearup.rate.lags <- make.n.lags(clearup.rate.vector, n.clearup.lags, dates.vector, rownames(crime.data))

print('made lag vectors')


panel.data <- data.frame(pfs.vector, dates.vector, crime.rate.vector)

for (crime.lag in crime.lags) {
  panel.data <- cbind(panel.data, unlist(crime.lag))
}

for (clearup.rate.lag in clearup.rate.lags) {
  panel.data <- cbind(panel.data, unlist(clearup.rate.lag))
}

colnames(panel.data) <- c("pf", "date", "crimerate", 
                          make.lag.names("crimes", n.crime.lags),
                          make.lag.names("clearups", n.clearup.lags)
)

View(head(panel.data))
print('made panel dataframe')
current.time <- Sys.time()
saveRDS(panel.data, file.path(data.folder, current.time))
print(paste("saved data as ", current.time))
