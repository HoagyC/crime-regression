library(plm)  

crime.data <- readRDS("crime_data.rds")
clearup.data <- readRDS("clearup_data.rds")

clearups.vector <- unlist(clearup.data, use.names = F)
crimes.vector <- unlist(crime.data, use.names = F)
lsoas.vector <- rep(colnames(crime.data), each=nrow(crime.data))
dates.vector <- rep(rownames(crime.data), times=ncol(crime.data))

panel.data <- data.frame(lsoas.vector, dates.vector, crimes.vector, clearups.vector)
colnames(panel.data) <- c("lsoa", "date", "crimes", "clearups")

current.month.ols <- lm(crimes ~ clearups, data=panel.data)
summary(current.month.ols)

fixed.effects.reg <- plm(crimes ~ clearups, data=panel.data, index=c("lsoa", "date"), model="within")
summary(fixed.effects.reg)
