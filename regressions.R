library(plm)

setwd("/home/hoagy/crime_regression/")

regression.coefs <- c()
standard.errors <- c()
for (i in 1:20) {
  panel.data <- readRDS(paste("data/prepared_datasets/robbery_by_month_", i, ".RDS", sep=""))
  panel.data <- panel.data[!is.na(panel.data[, ncol(panel.data)]), ]
  
  fixed.effects.reg <- plm(as.formula(paste("crimerate ~ ", paste(tail(names(panel.data), -3), collapse=" + "))), 
                           data=panel.data, index=c("pf", "date"), model="within")
  
  print(paste("completed coefficient estimation for", i, "months"))
  regression.coefs[i] <- tail(fixed.effects.reg$coefficients, 1)
  
}
