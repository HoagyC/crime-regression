library(readxl)
lsoa.pop.2019.unformatted <- read_excel("data/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx", 
                                                                 sheet = "Mid-2019 Persons", skip = 3)
lsoa.pop.2019 <- lsoa.pop.2019.unformatted %>% select("LA Code (2019 boundaries)", "All Ages")
  
saveRDS(lsoa.pop.2019, file="lsoa_pop_2019.rds")