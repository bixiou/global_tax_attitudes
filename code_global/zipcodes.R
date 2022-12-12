##### EU #####
zipcodes <- read.csv("../data/PC_DGURBA_2018.csv") # 19048 GISCO: Geog Info System of the COmmission (NUTS, LAU), 17650 NSI: National Statistical Institute, 28715 postal/outward codes
zipcodes <- zipcodes[zipcodes$CNTR_ID %in% c("FR", "DE", "ES", "UK"),-c(5,6)]
decrit("CNTR_ID", zipcodes) # Close to true numbers (ES: 11752, FR: 6328, DE: 8203, UK: 1.7 million) according to Wikipedia or for DE: https://energizedanalytics.com/en/what-you-always-wanted-to-know-about-german-postal-codes/
zipcodes_UK <- zipcodes[zipcodes$CNTR_ID == "UK",]
zipcodes_EU <- zipcodes[zipcodes$CNTR_ID != "UK",]

# uk_outward <- zipcodes_UK[,c(2,5)]
# uk_outward$PC_CNTR <- sub("UK_", "", sub(" .*", "", uk_outward$PC_CNTR), fixed = T)
# duplicate.urbanity <- uk_outward %>%  group_by(PC_CNTR) %>%  summarise(dup.urbanity = length(unique(DGURBA_FINAL_2018)))
# # duplicate.urbanity <- zipcodes %>%  group_by(GISCO_ID) %>%  summarise(dup.urbanity = length(unique(DGURBA_FINAL_2018)))
# decrit("dup.urbanity", duplicate.urbanity) # 30% of outcodes with several urbanity :-/ But GISCO_ID have unique urbanity
# length(unique(zipcodes$GISCO_CODE))
# length(unique(zipcodes_EU$PC_CNTR)) + length(unique(uk_outward$PC_CNTR))
# duplicate.urbanity_EU <- zipcodes_EU %>%  group_by(PC_CNTR) %>%  summarise(dup.urbanity = length(unique(DGURBA_FINAL_2018)))
# decrit("dup.urbanity", duplicate.urbanity_EU) # All have unique urbanity


##### UK #####
# Source: oecd_climate/code_oecd/zipcodes/UK_rural.R (cleaned, also contained region coding); https://www.doogal.co.uk/postcodedownloads.php
data <- read.csv2("../data/zipcodes_UK.csv", sep=',')

rural <- c("Rural village", "Rural hamlet and isolated dwellings", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural hamlet and isolated dwellings in a sparse setting", "Rural village in a sparse setting")
rural <- c(rural,"", "Accessible rural area", "Remote rural area", "Very remote rural area", "Very remote small town", "Accessible small town", "Remote small town")
urban_town <- c("Urban city and town", "Urban city and town in a sparse setting")
urban_large <- c("Urban major conurbation", "Urban minor conurbation", "Large urban area", "Other urban area")

data$Rural.urban.quota[data$Rural.urban %in% urban_town] <-"City_Town"
data$Rural.urban.quota[data$Rural.urban %in% rural] <-"Rural"
data$Rural.urban.quota[data$Rural.urban %in% urban_large] <-"Large_urban"

data.outcode <- data %>%
  select(Population, Postcode, Region.quota, Rural.urban.quota) # may bug because dplyr::select is masked by MASS

data.outcode <- data.outcode %>%
  subset(Region.quota != "")
# Get the outcode
data.outcode$outcode <- sub(" .*", "", data.outcode$Postcode)

# Then since there are more then 2Mio postcodes, we want info at the outcode levels
# When there are multiples regions or rural/urban areas for the same outcode,
# we assign the one with the greatest share of population


# Step 1: Get total pop by outcode
data.outcode.pop <- data.outcode %>%
  group_by(outcode) %>%
  summarise(pop.outcode = sum(Population, na.rm=T)) %>%
  ungroup()
data.outcode <- merge(data.outcode, data.outcode.pop, by="outcode")


## Step 2: Urbanity
# Focus on subsample of duplicates
duplicate.urbanity <- data.outcode %>%
  group_by(outcode) %>%
  summarise(dup.urbanity = length(unique(Rural.urban.quota))) %>%
  ungroup()

# Assign region if no duplicates
data.outcode$Urbanity.outcode <- ""
data.outcode <- merge(data.outcode, duplicate.urbanity, by="outcode")
data.outcode$Urbanity.outcode[data.outcode$dup.urbanity == 1] <- data.outcode$Rural.urban.quota[data.outcode$dup.urbanity == 1]

# Get share of each rural/urban category for each outcode
data.outcode.urbanity <- data.outcode[data.outcode$dup.urbanity > 1,] %>%
  group_by(outcode, Rural.urban.quota) %>%
  summarise(outcode, Rural.urban.quota, share = sum(Population, na.rm = T)/pop.outcode) %>%
  ungroup()

# Transform data
data.outcode.urbanity <- data.outcode.urbanity[!duplicated(data.outcode.urbanity),]
data.outcode.urbanity <- data.outcode.urbanity %>%
  pivot_wider(names_from = Rural.urban.quota, values_from = share)

data.outcode.urbanity$Urbanity.outcode <- ""
# If no population at all, we assign it to rural, except for 3 exceptions (not related to rural in original data)
data.outcode.urbanity$Urbanity.outcode[(is.na(data.outcode.urbanity$Large_urban) & is.na(data.outcode.urbanity$Rural) & is.na(data.outcode.urbanity$City_Town)) == T] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[data.outcode.urbanity$outcode %in% c("DH98", "NG80", "WV98")] <- "City_Town"

# Replace NA with 0
data.outcode.urbanity$Large_urban[is.na(data.outcode.urbanity$Large_urban)] <- 0
data.outcode.urbanity$Rural[is.na(data.outcode.urbanity$Rural)] <- 0
data.outcode.urbanity$City_Town[is.na(data.outcode.urbanity$City_Town)] <- 0

# Assign value with greatest share
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Large_urban > data.outcode.urbanity$Rural & data.outcode.urbanity$Large_urban > data.outcode.urbanity$City_Town)] <- "Large_urban"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Rural > data.outcode.urbanity$Large_urban & data.outcode.urbanity$Rural > data.outcode.urbanity$City_Town)] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$City_Town > data.outcode.urbanity$Rural & data.outcode.urbanity$City_Town > data.outcode.urbanity$Large_urban)] <- "City_Town"

# Merging
data.outcode.urbanity <- data.outcode.urbanity %>%
  select(outcode, Urbanity.outcode)

data.outcode <- merge(x=data.outcode, y=data.outcode.urbanity, by="outcode", all.x = T)

# Assign values of non-duplicates
data.outcode$Urbanity.outcode.y[is.na(data.outcode$Urbanity.outcode.y)] <- data.outcode$Urbanity.outcode.x[is.na(data.outcode$Urbanity.outcode.y)]
data.outcode <- data.outcode %>%
  rename(Urbanity.outcode = Urbanity.outcode.y)

decrit(data.outcode$Urbanity.outcode, weights = data.outcode$Population, data=data.outcode) # 401/423/176
decrit(data$Rural.urban.quota, weights = data$Population, data=data) # 396/386/218 By assigning outcodes to a single urbanity, we lose some 4 pp of rurals.

# Merge with Eurostat's DEGURBA
# zipcodes_UK <- zipcodes[zipcodes$CNTR_ID == "UK",]
# zipcodes_UK$outcode <- sub("UK_", "", sub(" .*", "", zipcodes_UK$PC_CNTR), fixed = T)
zipcodes_UK$Postcode <- sapply(zipcodes_UK$PC_CNTR, function(x) { substr(x, 4, nchar(x)) })
data_uk <- merge(data[,c("Population", "Postcode", "Rural.urban.quota")], zipcodes_UK[,c("Postcode", "DGURBA_FINAL_2018")], by="Postcode") # Rural.urban has 18 levels
decrit("Rural.urban.quota", data = data_uk, weights = data_uk$Population) # 444/229/327
length(unique(zipcodes_UK$Postcode))
length(unique(data$Postcode))
fre(data_uk$Rural.urban, weight = data_uk$Population)
fre(data$Rural.urban, weight = data$Population)
CrossTable(data_uk$Rural.urban.quota, data_uk$DGURBA_FINAL_2018)
CrossTable(data_uk$Rural.urban.quota[!is.na(data$Population)], data_uk$DGURBA_FINAL_2018[!is.na(data$Population)], weights = data$Population[!is.na(data$Population)])
wtable(data_uk$Rural.urban.quota[!is.na(data$Population)], data_uk$DGURBA_FINAL_2018[!is.na(data$Population)], w = data$Population[!is.na(data$Population)])/20891767
# Degurba 1 is half in Large_urban, half in City_Town; 2 is half in City_Town, half in Rural; 3 is mostly in Rural. 
# Conversely, Large_urban is mostly in 1; City_Town is 60% in 2 and 40% in 1; and Rural is 70% in 2 and 30% in 3.
# In other words, Eurostat's definitions tend to view a postcode as more urban than UK's: all UK rural are Degurba=3, while all Degurba=1 are in UK Large_urban.
# /!\ But, the previous analysis is made only on 28M people, as half of Postcodes are unknown to Eurostat (and we lose half of Large_urban population and some City_Town in the meantime). 


## Export
zipcodes_UK <- data[,c("Rural.urban.quota", "Postcode")]
write.csv(zipcodes_UK,"zipcodes_UK.csv", row.names=F)
zipcodes_UK$Rural.urban.quota[zipcodes_UK$Rural.urban.quota == "Large_urban"] <- 1
zipcodes_UK$Rural.urban.quota[zipcodes_UK$Rural.urban.quota == "City_Town"] <- 2
zipcodes_UK$Rural.urban.quota[zipcodes_UK$Rural.urban.quota == "Rural"] <- 3
rm(data_uk, duplicate.urbanity, data.outcode, data.outcode.pop, data.outcode.urbanity, data)


##### Merge EU and UK, export #####
zipcodes_ES <- zipcodes_EU[zipcodes$CNTR_ID == "ES", c(2,5)]
zipcodes_ES$PC_CNTR <- substr(zipcodes_ES$PC_CNTR, 4, 8)
write.csv(zipcodes_ES, "../data/zipcodes_ES.csv")
zipcodes_DE <- zipcodes_EU[zipcodes$CNTR_ID == "DE", c(2,5)]
zipcodes_DE$PC_CNTR <- substr(zipcodes_DE$PC_CNTR, 4, 8)
write.csv(zipcodes_DE, "../data/zipcodes_DE.csv")
zipcodes_FR <- zipcodes_EU[zipcodes$CNTR_ID == "FR", c(2,5)]
zipcodes_FR$PC_CNTR <- substr(zipcodes_FR$PC_CNTR, 4, 8)
write.csv(zipcodes_FR, "../data/zipcodes_FR.csv")
zipcodes_FR$PC_CNTR <- paste0("F", zipcodes_FR$PC_CNTR)
zipcodes_ES$PC_CNTR <- paste0("E", zipcodes_ES$PC_CNTR)
zipcodes_DE$PC_CNTR <- paste0("D", zipcodes_DE$PC_CNTR)
zipcodes_UK$Postcode <- paste0("U", zipcodes_DE$Postcode)
zipcodes <- rbind(zipcodes_FR, zipcodes_DE, zipcodes_ES)
names(zipcodes) <- c("Postcode", "degurba")
zipcodes <- rbind(zipcodes_UK, zipcodes)
write.csv(zipcodes_EU, "../data/zipcodes.csv")
rm(zipcodes, zipcodes_FR, zipcodes_EU, zipcodes_DE, zipcodes_ES, zipcodes_UK, uk_outward)
