library(readxl)
lsoa.pop.2019.unformatted <- read_excel("data/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx", 
                                                                 sheet = "Mid-2019 Persons", skip = 3)
lsoa.codes <- lsoa.pop.2019.unformatted$`LSOA Code`
lsoa.pop.2019 <- data.frame(lsoa.pop.2019.unformatted$"All Ages")
rownames(lsoa.pop.2019) <- lsoa.codes

saveRDS(lsoa.pop.2019, file="lsoa_pop_2019.rds")
