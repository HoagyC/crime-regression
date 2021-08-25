library(plm)

setwd("/home/hoagy/crime_regression/")
panel.data <- readRDS("data/prepared_datasets/2021-08-24 16:48:53")

fixed.effects.reg <- plm(as.formula(paste("crimerate ~ ", paste(tail(names(panel.data), -3), collapse=" + "))), 
                         data=panel.data, index=c("lsoa", "date"), model="within")

print('completed coefficient estimation')
print(summary(fixed.effects.reg))
