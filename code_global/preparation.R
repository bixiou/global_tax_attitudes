qinc <- read.csv("../data/EU_income_deciles.tsv", sep = "\t") # equivalised disposable income in LCU
inc_quantiles <- matrix(NA, nrow = 5, ncol = 11, dimnames = list(c("FR", "DE", "ES", "UK", "CH"), c("D1", "D2", "D3", "D4","Q2", "D6", "D7", "D8", "D9", "Q1", "Q3")))
for (c in c("FR", "DE", "ES", "UK", "CH")) {
  for (i in 1:9) inc_quantiles[c,i] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("D", i, ",TC,NAC,", c)])) # euro_countries / year_countries[c]
  inc_quantiles[c,10] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("Q", 1, ",TC,NAC,", c)])) 
  inc_quantiles[c,11] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("Q", 3, ",TC,NAC,", c)])) 
}
inc_quantiles <- inc_quantiles[,c(1,2,10,3:7,11,8,9)]
inc_quantiles["UK",] <- round(inc_quantiles["UK",]*2317/2170) # inflate to 2021 by nominal GDP growth in LCU https://data.worldbank.org/indicator/NY.GDP.MKTP.CN?end=2021&locations=GB&start=1960&view=chart
inc_quantiles["CH",] <- round(inc_quantiles["CH",]*742.84/727.21) # Now in 2021 LCU for all
inc_quantiles # Now in 2021 LCU for each
50*round(inc_quantiles/12/50) # Per month, rounded
rm(qinc)

