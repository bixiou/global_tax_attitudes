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

zipcodes <- read.csv("../data/PC_DGURBA_2018.csv") # 19048 GISCO: Geog Info System of the COmmission (NUTS, LAU), 17650 NSI: National Statistical Institute, 28715 postal/outward codes
zipcodes <- zipcodes[zipcodes$CNTR_ID %in% c("FR", "DE", "ES", "UK"),-c(5,6)]
decrit("CNTR_ID", zipcodes) # Close to true numbers (ES: 11752, FR: 6328, DE: 8203, UK: 1.7 million) according to Wikipedia or for DE: https://energizedanalytics.com/en/what-you-always-wanted-to-know-about-german-postal-codes/
uk_zipcodes <- zipcodes[zipcodes$CNTR_ID == "UK",]
zipcodes_EU <- zipcodes[zipcodes$CNTR_ID != "UK",]
uk_outward <- uk_zipcodes[,c(2,5)]
uk_outward$PC_CNTR <- sub("UK_", "", sub(" .*", "", uk_outward$PC_CNTR), fixed = T)
duplicate.urbanity <- uk_outward %>%  group_by(PC_CNTR) %>%  summarise(dup.urbanity = length(unique(DGURBA_FINAL_2018)))
# duplicate.urbanity <- zipcodes %>%  group_by(GISCO_ID) %>%  summarise(dup.urbanity = length(unique(DGURBA_FINAL_2018)))
decrit("dup.urbanity", duplicate.urbanity) # 30% of outcodes with several urbanity :-/ But GISCO_ID have unique urbanity
length(unique(zipcodes$GISCO_CODE))
length(unique(zipcodes_EU$PC_CNTR)) + length(unique(uk_outward$PC_CNTR))
