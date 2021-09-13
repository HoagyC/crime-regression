setwd("/home/hoagy/crime_regression/")

regression.coefs_burglary <- c()
standard.errors <- c()
for (i in 1:20) {
  panel.data <- readRDS(paste("data/prepared_datasets/burglary_by_month_av_", i, ".RDS", sep=""))
  panel.data <- panel.data[!is.na(panel.data[, ncol(panel.data)]), ]
  
  fixed.effects.reg <- lm(as.formula(paste("crimerate ~ ", paste(tail(names(panel.data), -3), collapse=" + "))), 
                           data=panel.data)
  
  print(paste("completed coefficient estimation for", i, "months"))
  regression.coefs_burglary[i] <- tail(fixed.effects.reg$coefficients, 1)
}