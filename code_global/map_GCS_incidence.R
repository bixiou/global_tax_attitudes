# TODO? share of winners per country: Ivanova & Wood (20) show that 2020 World average of 6t pc is at ~50 percentile in FR, ~65 in ES, ~50 in UK, ~20 in DE; Fremstad & Paul (19) show it's at ~20p in the U.S.
# TODO: more accurate assumption/computations (e.g. based on NDCs)

##### Population data #####
# pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
# pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
# pop <- pop[pop$Time %in% c(2015, 2019, 2023, 2030),]
# pop <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
# names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
# pop_iso3 <- aggregate(adult ~ year + code, data = pop, FUN = sum)
years <- c(2005, seq(2010, 2100, 10))
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop <- pop[pop$Time %in% c(2015, 2019, 2023, 2030, years),]
pop_adult <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
names(pop_adult) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
pop_adult_iso3 <- aggregate(adult ~ year + code, data = pop_adult, FUN = sum)
names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "pop")
pop_iso3 <- aggregate(pop ~ year + code, data = pop, FUN = sum)
pop_iso3 <- merge(pop_iso3, pop_adult_iso3)

##### CO2 emissions data #####
# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions
co2 <- read.csv("../data/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2019),]
temp <- co2[co2$Year == 2019,]
temp$Year <- 2023
co2 <- rbind(co2, temp)
names(co2) <- c("country", "code", "year", "territorial", "footprint")


##### GDP pc data #####
GDPpc <- read.csv("../data/GDPpc_2015$_nominal.csv") # GDP per capita (constant 2015 US$) https://data.worldbank.org/indicator/NY.GDP.PCAP.KD March 1, 2023 
GDPpc$code <- GDPpc$Country.Code
GDPpc <- rbind(data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2015, "year" = 2015), data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2019, "year" = 2019))
co2 <- merge(co2, GDPpc)


##### Merge datasets #####
# co2$year[co2$year == 2016] <- 2030 # Beware, before we estimate 2030 emissions, they will be equal to 2016 ones!
for (y in c(years)) {
  temp_y <- co2[co2$year == 2015,]
  temp_y$year <- y
  for (v in c("territorial", "footprint")) temp_y[[v]] <- NA
  co2 <- rbind(co2, temp_y)
}
co2_pop <- merge(pop_iso3[pop_iso3$code != "",], co2[co2$code != "",], all = T)
country_code <- setNames(co2$country[co2$year == 2019], co2$code[co2$year == 2019])
co2_pop$country <- country_code[co2_pop$code]
co2_pop$adult <- 1000 * co2_pop$adult
co2_pop$pop <- 1000 * co2_pop$pop
co2_pop$emissions <- co2_pop$footprint
co2_pop$missing_footprint <- is.na(co2_pop$footprint)
(sum(co2_pop$adult_2019[is.na(co2_pop$footprint_2019)])/adult_pop_2019) # 7.4% of global footprint data missing 
co2_pop$emissions[is.na(co2_pop$footprint)] <- co2_pop$territorial[is.na(co2_pop$footprint)] # imputing territorial emissions for those countries
co2_pop <- co2_pop %>% group_by(code) %>% pivot_wider(id_cols = c("code", "country"),  names_from = year, 
           values_from = c("territorial", "footprint", "emissions", "adult", "pop", "gdp_pc", "missing_footprint"), names_glue = "{.value}_{year}") %>% ungroup()
# /!\ Beware, data is valid only in 2015 and 2019 for gdp_pc (and it is in nominal)
co2_pop$missing_footprint <- co2_pop$missing_footprint_2019
co2_pop <- co2_pop[, !colnames(co2_pop) %in% paste0("missing_footprint_", c(2015, 2019, 2023, years))]
co2_pop <- co2_pop[!co2_pop$country %in% c("World", "Kosovo", NA), sapply(names(co2_pop), function(v) any(!is.na(co2_pop[[v]])))]
rm(pop, co2)  
decrit(co2_pop$missing_footprint) # 98 out of 218 countries missing (while no missing for territorial)
# co2_pop$country[co2_pop$missing_footprint]


##### Constants #####
euro_per_dollar <- 0.94
pound_per_dollar <- .82
LCU_per_dollar <- c(rep(euro_per_dollar, 3), pound_per_dollar, 1)
co2_emissions_2030 <- 26.3e9
(R <- carbon_tax_revenues_2030 <- 2367e9) # in $, Based on $90/tCO2 
(A <- adult_pop_2015 <- sum(co2_pop$adult_2015)) # 5.45G (entire pop (not just 15+) 7.42G)
(adult_pop_2019 <- sum(co2_pop$adult_2019)) # 5.75G (entire pop (not just 15+) 7.76G)
(F_ <- adult_pop_2030 <- sum(co2_pop$adult_2030)) # 6.57G (entire pop (not just 15+) 8.54G)
(E <- co2_emissions_2015 <- sum(co2_pop$territorial_2015)) # 34.4G (footprint: 33.4G + NA)
(E <- co2_emissions_2019 <- sum(co2_pop$territorial_2019)) # 35.8G
# countries_survey_oecd <- c("AUS", "CAN", "DNK", "FRA", "DEU", "ITA", "JPN", "MEX", "POL", "KOR", "ESP", "TUR", "GBR", "USA", "BRA", "CHN", "IND", "IDN", "ZAF", "UKR")
# revenues_pa_oecd <- c(134, 105, 68, 46, 61, 42, 60, 34, 42, 72, 39, 40, 59, 128, 18, 38, 13, 16, 50, 32) # cf. oecd_climate/questionnairesboard row 63 and oecd_climate/questionnaires/net_gain_global_tax
# mean_gain_oecd <- 30 - revenues_pa_oecd
# emission_share_2015_oecd <- c(426.4, 547.9, 59.4, 445, 853.4, 423, 1361, 485.5, 273.8, 584.8, 293.8, 374.9, 575.8, 5794.5, 475.4, 7977.9, 1918.8, 484.6, 313.5, 262)/32276 # OECD bit.ly/37kSVUx EU28 12.3%, G20 82.7% 2015 latest date available
# names(mean_gain_oecd) <- names(emission_share_2015_oecd) <- names(revenues_pa_oecd) <- countries_survey_oecd
#             2020   2030   2040   2050                                Past/global basic income/estimate of a global basic income.pdf
# Emissions   34.3   26.3   18.8   13.1 GtCO2                          2DS (66% chance) scenario in Energy Technology Perspectives 2017 http://www.iea.org/etp2017/summary/
# Price         40     90    120    145 $/tCO2                         Stern & Stiglitz (2017), Table 3
# Revenues    1372   2367   2256   1900 G$/year
# Basic income  20     30     26     21 $/month                        Median UN ≥15 pop
# PPP          1.4    2.1    1.8    1.5 Sub-Saharan Africa PPP $/day   2.1: ratio of GDP pc of Sub-Saharan Africa in PPP and value (World Bank)
# Share income 1.4    1.7    1.2    0.7 % of Gross World Product       2020: 98T$, then assuming 3.5% growth
# Emissions pa   6      4    2.6    1.7 tCO2/year per adult above 15
# Adult pop    5.7    6.6    7.2    7.5 G adult above 15
# /!\ The 2020 Gross World Product (84.7T$) and future growth may well be lower than assumed (98T$, 3.5%), hence higher Share income.
# /!\ Units of price are not specified in Stern & Stiglitz (2017), but probably constant 2017 dollar.


##### Compute net gain per capita #####
# Assumption: emissions per adult (>15) will evolve in the same way in all countries
# There are discrepancies between OECD data (used in OECD survey) and Global Carbon Project data (used here as it covers all countries, cf. Peters al. (2012)). Discrepancies are always within +/- 20%.
# The results also slightly change if the baseline year 2019 is used instead of 2015, and if we compute the net gain for (i.e. divide by population of) year = 2030 vs. 2015, cf. deprecated/draft_map_GCS_incidence.R for an analysis
# For consistency with the OECD survey, we use 2015 as a baseline.
compute_gain <- function(year = 2030, base_year = 2019, type = "mean", df = co2_pop, return_data = T) {
  df[[paste0("demographic_evolution_", base_year)]] <- (df$adult_2030/df[[paste0("adult_", base_year)]]) * (sum(df[[paste0("adult_", base_year)]])/adult_pop_2030)
  df[[paste0("emissions_pa_", base_year)]] <- df[[paste0("emissions_", base_year)]]/df[[paste0("adult_", base_year)]]
  df[[paste0("share_emissions_", base_year)]] <- df[[paste0("emissions_", base_year)]]/sum(df[[paste0("emissions_", base_year)]])
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_", base_year)]] * df[[paste0("demographic_evolution_", base_year)]]
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]]/sum(df[[paste0("share_emissions_2030_base_", base_year)]])
  df$emissions_2030 <- df[[paste0("share_emissions_2030_base_", base_year)]] * co2_emissions_2030
  df$emissions_pa_2030 <- df$emissions_2030 / df$adult_2030
  df[[paste0("revenues_pa_", year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]] * carbon_tax_revenues_2030 / df[[paste0("adult_", year)]]/12
  df[[paste0(type, "_gain_", year)]] <- 30 - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pa_", year)]]
  if (return_data) return(df) else return(df[[paste0(type, "_gain_", year)]])
}
co2_pop <- compute_gain(year = 2015, base_year = 2015, type = "median") # creates median_gain_2015
co2_pop <- compute_gain(year = 2019, base_year = 2019, type = "mean") 
co2_pop <- compute_gain(year = 2030, base_year = 2019, type = "mean") # creates mean_gain_2030

# co2_pop <- compute_gain(year = 2030, base_year = 2019, type = "median") # creates median_gain_2015
# setNames(c(10, 25, 5, 20, 85), c("FRA", "DEU", "ESP", "GBR", "USA"))
# setNames(LCU_per_dollar*co2_pop$median_gain_2030[sapply(c("FRA", "DEU", "ESP", "GBR", "USA"), function(c) which(co2_pop$code == c))], c("FRA", "DEU", "ESP", "GBR", "USA"))

# Net median gain in our 5 countries of interest
(median_gain_2015_LCU <- LCU_per_dollar*co2_pop$median_gain_2015[sapply(c("FRA", "DEU", "ESP", "GBR", "USA"), function(c) which(co2_pop$code == c))])
# Appendix table
min_pop_table_gain_gcs <- 20e6
sum(co2_pop$adult_2019[co2_pop$adult_2019 > min_pop_table_gain_gcs])/adult_pop_2019 # 94% (89%) of global population lives in one of the 80 (57) countries > 10M
# TODO? remove? change Mean to Median and mean_gain_2030 to median_gain_2015? With median_gain_2015, the ranking with emissions_pa_2015 is not preserved because we divide by adult_2015 instead of adult_2030
table_gain_gcs <- sort(setNames(co2_pop$mean_gain_2030[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs]))
temp <- sort(setNames(co2_pop$emissions_pa_2019[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs])) # China has larger footprint than France!
table_gain_gcs <- cbind(table_gain_gcs, temp[names(table_gain_gcs)])
row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% co2_pop$country[co2_pop$missing_footprint]] <- paste0(row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% co2_pop$country[co2_pop$missing_footprint]], "*")
row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% c("Democratic Republic of Congo*", "Democratic Republic of the Congo*")] <- "DRC*"
cat(paste(kbl(table_gain_gcs, "latex", caption = "Estimated net gain from the GCS in 2030 and carbon footprint by country.", position = "b", escape = F, booktabs = T, digits = c(0, 1), linesep = rep("", nrow(table_gain_gcs)-1), longtable = T, label = "gain_gcs.tex",
              col.names = c("\\makecell{Mean\\\\net gain\\\\from\\\\the GCS\\\\(\\$/month)}", "\\makecell{CO$_\\text{2}$\\\\footprint\\\\per adult\\\\in 2019\\\\(tCO$_\\text{2}$/y)}")), collapse="\n"), file = "../tables/gain_gcs.tex") 


##### Plot map #####
co2_pop$country[co2_pop$country == "United States"] <- "USA"
co2_pop$country[co2_pop$country == "United Kingdom"] <- "UK"
co2_pop$country[co2_pop$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
co2_pop$country[co2_pop$country == "Congo"] <- "Republic of Congo"
co2_pop$country[co2_pop$country == "Cote d'Ivoire"] <- "Ivory Coast"
co2_pop$country[co2_pop$country == "Czechia"] <- "Czech Republic"
wtd.quantile(co2_pop$mean_gain_2030, weights = co2_pop$adult_2019, probs = seq(0, 1, 1/6)) 
thresholds_map <- c(-Inf, -70, -30, -20, -10, 0, 10, 15, 20, 25, Inf)
plot_world_map("mean_gain_2030", breaks = thresholds_map, format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
# If bug, first use save = F then again save = T
# Looks nice with width: 1160, height: 560 (one needs to adjust manually for PDF)

plot_world_map("median_gain_2015", breaks = thresholds_map, format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Median net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T, width = 1160, height = 560) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) limits = c(-30, 30), 
# If needed run  cd .\Documents\www\global_tax_attitudes\figures\maps\
#      and  pdfcrop --margins '-20 0 70 -7' mean_gain_2030 (or simply sh crop_pdf.sh to treat all PDFs)

co2_pop$mean_gain_over_gdp_2019 <- 100*12*co2_pop$mean_gain_2019/co2_pop$gdp_pc_2019 # TODO! mean_gain is computed per adult, not p.c. => abs gain/loss are lower than shown, especially in young countries
sum((co2_pop$mean_gain_over_gdp_2019 * co2_pop$gdp_pc_2019 * co2_pop$adult_2019 / 100)[co2_pop$mean_gain_over_gdp_2019 > 0], na.rm = T)
-sum((co2_pop$mean_gain_over_gdp_2019 * co2_pop$gdp_pc_2019 * co2_pop$adult_2019 / 100)[co2_pop$mean_gain_over_gdp_2019 < 0], na.rm = T)
# /!\ Discrepancy between transfers given (.6T) and received (.9T), due to the adult vs. pop issue raised above. Indeed, computing it without the GDPpc has no problem (.85T in both cases):
sum((12 * co2_pop$mean_gain_2030 * co2_pop$adult_2030)[co2_pop$mean_gain_2030 > 0], na.rm = T) # .85T
-sum((12 * co2_pop$mean_gain_2030 * co2_pop$adult_2030)[co2_pop$mean_gain_2030 < 0], na.rm = T)
sum((12 * co2_pop$mean_gain_2030 * co2_pop$adult_2030)[co2_pop$mean_gain_2030 > 0], na.rm = T)/(sum(co2_pop$gdp_pc_2030 * co2_pop$pop_2030, na.rm = T)) # 1% of GDP redistributed
sort(setNames(co2_pop$mean_gain_over_gdp_2019, co2_pop$country))
plot_world_map("mean_gain_over_gdp_2019", breaks = c(-Inf, -5, -2, -1, 0, 1, 5, 20, 50, Inf), format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -5, -2, -1, 0, 1, 5, 20, 50, Inf), sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in % of GDP)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
co2_pop$country[co2_pop$country == "USA"] <- "United States"
co2_pop$country[co2_pop$country == "UK"] <- "United Kingdom"
co2_pop$country[co2_pop$country == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
co2_pop$country[co2_pop$country == "Republic of Congo"] <- "Congo"
co2_pop$country[co2_pop$country == "Ivory Coast"] <- "Cote d'Ivoire"
co2_pop$country[co2_pop$country == "Czech Republic"] <- "Czechia"


##### Poverty gaps #####
extract_last_year <- function(df = pg, cols = names(df), var_name = NULL, pattern_gsub = "[^0-9]", keep = names(df)) {
  df$last_year <- NA
  for (j in cols) df$last_year <- ifelse(is.na(df[[j]]), df$last_year, rep(j, nrow(df))) 
  if (!is.null(var_name)) df[[var_name]] <- NA
  if (!is.null(var_name)) df[[var_name]][!is.na(df$last_year)] <- sapply(which(!is.na(df$last_year)), function(i) df[[df$last_year[i]]][i])
  df$last_year <- as.numeric(gsub(pattern_gsub, "", df$last_year))
  if (!is.null(var_name)) return(df[, unique(c(keep, "last_year", var_name))])
  else return(df$last_year)
}

# Problem: U.S. have a poverty gap at $2 of 0.8%, not much lower than India's 1.8%
pg2 <- read.csv("../data/poverty_gap_2-15.csv") # Poverty gap at $2.15 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.GAPS March 1, 2023. World average: 2.6% i.e. (less than - bc PPP) 163G$ = 2.15*365*8e9*.026
pg2$code <- pg2$Country.Code
pg2 <- extract_last_year(df = pg2, cols = paste0("X", 1960:2021), var_name = "pg2", keep = c("Country.Name", "code"))
pg2$Country.Name[pg2$code %in% co2_pop$code][is.na(pg2$pg2[pg2$code %in% co2_pop$code])] # No data (for any year) for Afghanistan, Cambodia, Cuba, Dominica, Eritrea, Equatorial Guinea, Libya, North Korea. 

pg4 <- read.csv("../data/poverty_gap_3-65.csv") # Poverty gap at $3.65 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.LMIC.GP March 1, 2023. World average: 8% i.e. (less than - bc PPP) .85T$ = 3.65*365*8e9*.08
pg4$code <- pg4$Country.Code
pg4 <- extract_last_year(df = pg4, cols = paste0("X", 1960:2021), var_name = "pg4", keep = c("Country.Name", "code"))

pg7 <- read.csv("../data/poverty_gap_6-85.csv") # Poverty gap at $6.85 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.UMIC.GP March 1, 2023. World average: 21% i.e. (less than - bc PPP) 4T$ = 6.85*365*8e9*.21
pg7$code <- pg7$Country.Code
pg7 <- extract_last_year(df = pg7, cols = paste0("X", 1960:2021), var_name = "pg7", keep = c("Country.Name", "code"))

GDPpcPPP <- read.csv("../data/GDPpc_2017$_PPP.csv") # GDP per capita, PPP (constant 2017 international $) https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD March 1, 2023 
GDPpcPPP$code <- GDPpcPPP$Country.Code
# all(GDPpcPPP$country == pg$country)
GDPpcPPP$GDPpcPPP[!is.na(pg7$last_year)] <- sapply(which(!is.na(pg7$last_year)), function(i) GDPpcPPP[[paste0("X", pg7$last_year[i])]][i])
# We don't replace missing values for GDPpc in gap's last year (while other years are available for GDPpc) as they always coincide with missing values for poverty gaps
# GDPpcPPP <- extract_last_year(df = GDPpcPPP, cols = paste0("X", 1960:2021), var_name = "gdp_last_year")
# sum(is.na(GDPpcPPP$GDPpcPPP[GDPpcPPP$code %in% co2_pop$code])) # 41 missing values for GDP pc for the year at which we have the last poverty gap estimate, we use instead the last available data for GDP (in 2017 constant $)
# GDPpcPPP$GDPpcPPP[is.na(GDPpcPPP$GDPpcPPP)] <- GDPpcPPP$gdp_last_year[is.na(GDPpcPPP$GDPpcPPP)]
# GDPpcPPP$Country.Name[GDPpcPPP$code %in% co2_pop$code][is.na(GDPpcPPP$GDPpcPPP[GDPpcPPP$code %in% co2_pop$code])] # No data (for any year) for Cuba, Eritrea, North Korea, South Sudan, Syria, Venezuela, Yemen. TODO: complete from other sources

pg <- merge(merge(pg2, merge(pg4, pg7)), GDPpcPPP[, c("Country.Name", "code", "GDPpcPPP")]) # TODO: do not remove region groupings (or recreate them, once we've done the share computations)

# co2_pop$country[co2_pop$code %in% setdiff(co2_pop$code, pg$code)]
# pg$Country.Name[pg$code %in% setdiff(pg$code, co2_pop$code)]
pg <- merge(pg, co2_pop)
pg$gdp_2019 <- pg$gdp_pc_2019 * pg$pop_2019

# reg_pg_gdp <- lm(gap ~ log10(GDPpcPPP), data = pg, weights = adult_2023) 
# summary(reg_pg_gdp)

# We exclude countries without extreme poor people and weight by adult population. R^2 = .50
reg_pg7_gdp_log <- lm(log10(pg7) ~ log10(GDPpcPPP), data = pg, weights = adult_2023, subset = pg7 > 0)
summary(reg_pg7_gdp_log) # 7.36 - 1.6 gdp, R^2: .78
pg$predicted_gap7[as.numeric(names(reg_pg7_gdp_log$fitted.values))] <- 10^reg_pg7_gdp_log$fitted.values
reg_pg4_gdp_log <- lm(log10(pg4) ~ log10(GDPpcPPP), data = pg, weights = adult_2023, subset = pg4 > 0)
summary(reg_pg4_gdp_log) # 6.6 - 1.52 gdp, R^2: .66
pg$predicted_gap4[as.numeric(names(reg_pg4_gdp_log$fitted.values))] <- 10^reg_pg4_gdp_log$fitted.values
reg_pg2_gdp_log <- lm(log10(pg2) ~ log10(GDPpcPPP), data = pg, weights = adult_2023, subset = pg2 > 0)
summary(reg_pg2_gdp_log) # 3.96 - .97, R^2: .50
pg$predicted_gap2[as.numeric(names(reg_pg2_gdp_log$fitted.values))] <- 10^reg_pg2_gdp_log$fitted.values
# pg$predicted_gap_simplfied <- 10^(4 - log10(pg7$GDPpcPPP))
# reg_pg_gdp_predicted <- predict(reg_pg_gdp, type = "response")

key_gap_gdp <- function(return_var = "gap", # gap, global_share, gdp_share, rev_pc
                        return_type = "list", # var, seq, function, list
                        poverty_line = 4,  # 2, 4, 7 $/day
                        max_reg = phase_out_start, # Inf, phase_out_start
                        PPP = F, list_month = T, df = pg, global_revenues = 4.08e+11, min_pop = 30e6,
                        phase_out_start = NULL, phase_out_end = 2*phase_out_start) { # wtd.mean(pg$GDPpcPPP, pg$pop_2019) wtd.mean(pg$gdp_pc_2019, pg$pop_2019)
  pg <- df
  pg$gap <- pg[[paste0("pg", poverty_line)]]
  pg$gdp <- if (PPP) pg$GDPpcPPP else pg$gdp_pc_2019
  if (is.null(phase_out_start)) phase_out_start <- wtd.mean(pg$gdp, pg$pop_2019)
  reg_gap_gdp_log <- lm(log10(gap) ~ log10(gdp), data = pg, weights = pop_2023, subset = gap > 0 & gdp < max_reg)
  # pg$predicted_gap[as.numeric(names(reg_gap_gdp_log$fitted.values))] <- 10^reg_gap_gdp_log$fitted.values
  print(paste("R^2: ", round(summary(reg_gap_gdp_log)$r.squared, 3)))
  key_gap <- function(gdp) {
    gap <- (gdp < phase_out_end)*10^((gdp <= phase_out_start) * (reg_gap_gdp_log$coefficients[1] + reg_gap_gdp_log$coefficients[2] * log10(gdp)) +
             (gdp > phase_out_start) * (-1 + (reg_gap_gdp_log$coefficients[1] + reg_gap_gdp_log$coefficients[2] * log10(phase_out_start) + 1) * (log10(phase_out_end) - log10(gdp))/(log10(phase_out_end) - log10(phase_out_start))))
    gap[gdp > phase_out_end] <- 0
    return(gap)
    # if (gdp <= phase_out_start) gap <- reg_gap_gdp_log$coefficients[1] + reg_gap_gdp_log$coefficients[2] * log10(gdp)
    # else if (gdp > phase_out_end) gap <- 0
    # else gap <- log10(key_gap(phase_out_start)) * (log10(phase_out_end) - log10(gdp))/(log10(phase_out_end) - log10(phase_out_start))
    # return(10^gap)
  }
  
  global_share <- function(gdp, pop) {
    share <- key_gap(gdp) * pop
    return(100*share/sum(share, na.rm = T))
  }
  
  global_share_pc <- function(gdp, pop) return(global_share(gdp, pop) / (100*pop/sum(pop, na.rm = T)))
  
  gdp_share <- function(gdp, pop, revenues = global_revenues) {
    return(global_share(gdp, pop) * revenues / (gdp*pop))
  }
  
  rev_pc <- function(gdp, pop, revenues = global_revenues) return(global_share(gdp, pop) * revenues / pop / 100 / ifelse(list_month, 12, 1))
  
  if (return_type == "var") {
    if (return_var == "gap") return(key_gap(pg$gdp))
    if (return_var == "global_share") return(global_share(pg$gdp, pg$pop_2023))
    if (return_var == "global_share_pc") return(global_share_pc(pg$gdp, pg$pop_2023))
    if (return_var == "gdp_share") return(gdp_share(pg$gdp_pc_2019, pg$pop_2023))
    if (return_var == "rev_pc") return(rev_pc(pg$gdp, pg$pop_2023))
  } else if (return_type == "function") {
    if (return_var == "gap") return(key_gap)
    if (return_var == "global_share") return(global_share)
    if (return_var == "global_share") return(global_share_pc)
    if (return_var == "gdp_share") return(gdp_share)
    if (return_var == "rev_pc") return(rev_pc)
  } else if (return_type == "seq") {
    if (return_var == "gap") return(key_gap(seq(250, 120000, 250)))
    # if (return_var == "rev_pc") return(rev_pc(seq(250, 120000, 250)))
    else warning("Impossible combination of return_var and return_type")
  } else if (return_type == "list") {
    if (return_var == "gap") return(sort(setNames(key_gap(pg$gdp)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
    if (return_var == "global_share") return(sort(setNames(global_share(pg$gdp, pg$pop_2023)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
    if (return_var == "global_share_pc") return(sort(setNames(global_share_pc(pg$gdp, pg$pop_2023)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
    if (return_var == "gdp_share") return(sort(setNames(gdp_share(pg$gdp_pc_2019, pg$pop_2023)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
    if (return_var == "rev_pc") return(sort(setNames(rev_pc(pg$gdp, pg$pop_2023)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
  }
}
# TODO: run the regression only for countries below lower threshold

# These lines are the ones used
pg$predicted_gap <- key_gap_gdp(poverty_line = 4, PPP = F, phase_out_start = wtd.mean(pg$gdp_pc_2019, pg$pop_2019), return_var = "gap", return_type = "var")
mean_gdp_pc <- wtd.mean(pg$gdp_pc_2019, pg$pop_2019)
qplot(log10(gdp_pc_2019), log10(pg4), data = pg, size = pop_2019, xlab = "log10 of 2019 GDP per capita (constant 2015 $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  # geom_smooth(method = "lm",  mapping = aes(weight = pop_2019 * (gdp_pc_2019 < mean_gdp_pc)), color = "black", show.legend = FALSE, se = F) +
  geom_line(aes(y = log10(predicted_gap)), size = 2, color = "red", show.legend = FALSE) + theme_bw()
# save_plot(filename = "poverty_gap_gdp", folder = "../figures/policies/", format = "png", width = 538, height = 413) # Renders much better by hand

table_pg <- key_gap_gdp(return_var = "gdp_share", return_type = "list")
table_pg <- cbind("gdp_share" = table_pg, "rev_pc" = key_gap_gdp("rev_pc")[names(table_pg)], "global_share_pc" = key_gap_gdp("global_share_pc")[names(table_pg)], "global_share" = key_gap_gdp("global_share")[names(table_pg)])
row.names(table_pg)[row.names(table_pg) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
cat(paste(kbl(table_pg[table_pg[,1] > 0.03,], "latex", caption = "Allocation of the global wealth tax revenues.", position = "b", escape = F, booktabs = T, digits = c(2, 0, 2, 2), linesep = rep("", nrow(table_pg)-1), longtable = T, label = "allocation",
              col.names = c("\\makecell{Revenues\\\\over GDP\\\\(in percent)}", "\\makecell{Revenues\\\\per capita\\\\(in \\$ per month)}", "\\makecell{Revenues per capita\\\\over average\\\\revenues p.c.}", "\\makecell{Global\\\\share of\\\\revenues}")), collapse="\n"), file = "../tables/allocation.tex") 
  # TODO: do total, re-order all by GDP pc

mean_GDPpcPPP <- wtd.mean(pg$GDPpcPPP, pg$pop_2019)
qplot(log10(GDPpcPPP), log10(pg7), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $6.85 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "black", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "black", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg2), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $2.15 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "black", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE, se = F) + theme_bw() +
  geom_line(aes(y = log10(predicted_gap)), size = 2, color = "red", show.legend = FALSE)

# SSA gets more when using PPP. Gets most with line at 4. For lines at 2 and 4, gets more if max_reg = mean; for line at 7, gets more if max_reg = Inf.
# Three most credible are l=4, not PPP, max_reg mean; l=7, PPP, max_reg mean; l=4, PPP, max_reg Inf; l=4, not PPP, max_reg Inf
# LIC <- pg$code[pg$gdp_pc_2019 < 1085] # closer to the 2021 classification available on World Bank data for which LIC: 700M people
HIC <- pg$code[pg$gdp_pc_2019 > 13205] # approximately right (63 countries instead of 81 but differences are due to small islands)
LIC <- c("AFG", "BFA", "BDI", "TCD", "COG", "ERI", "ETH", "GMB", "GIN", "GNB", "PRK", "LBR", "MDG", "MWI", "MLI", "MOZ", "NER", "RWA", "SOM", "SRE", "SDN", "SSD", "SYR", "TGO", "UGA", "YEM", "ZMB") # 2023 official classification. LIC: 650M people
SSA <- c("SDN", "AGO", "GIN", "GMB", "GNB", "GNQ", "BDI", "BEN", "BFA", "SEN", "BWA", "CAF", "SLE", "SOM", "SSD", "CIV", "CMR", "COD", "COG", "COM", "LBR", "LSO", "SWZ", "TCD", "TGO", "MLI", "MDG", "DJI", "ERI", "ESH", "ETH", "MWI", "MUS", "MRT", "MOZ", "TZA", "UGA", "ZMB", "ZWE", "NGA", "NER", "NAM", "GHA", "GAB")
SSA_max_reg_inf <- SSA_max_reg_mean <- LIC_max_reg_inf <- LIC_max_reg_mean <- India_max_reg_inf <- India_max_reg_mean <- array(dimnames = list("line" = c(2, 4, 7), "PPP" = c(T, F)), dim = c(3, 2))
for (l in c(2, 4, 7)) for (p in c(TRUE, FALSE)) {
  SSA_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T))
  SSA_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T))
  LIC_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% LIC], na.rm = T))
  LIC_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% LIC], na.rm = T))
  India_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% "IND"], na.rm = T))
  India_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gap_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% "IND"], na.rm = T))
}
SSA_max_reg_inf
SSA_max_reg_mean
LIC_max_reg_inf
LIC_max_reg_mean
India_max_reg_inf
India_max_reg_mean
key_gap_gdp(poverty_line = 4, PPP = F, return_var = "global_share")
key_gap_gdp(poverty_line = 4, PPP = F, max_reg = Inf, return_var = "global_share_pc")
sum(key_gap_gdp(poverty_line = 4, PPP = T, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T)
pg$global_share <- key_gap_gdp(return_var = "global_share", return_type = "var")
pg$global_share_pc <- key_gap_gdp(return_var = "global_share_pc", return_type = "var")
pg$rev_pc <- key_gap_gdp(return_var = "rev_pc", return_type = "var")
pg$gdp_share <- key_gap_gdp(return_var = "gdp_share", return_type = "var")
sum(pg$global_share[pg$code %in% SSA], na.rm = T)
wtd.mean(pg$rev_pc[pg$code %in% SSA], pg$pop_2019[pg$code %in% SSA], na.rm = T)
wtd.mean(pg$global_share_pc[pg$code %in% SSA], pg$pop_2019[pg$code %in% SSA], na.rm = T)
pooled_revenues * sum(pg$global_share[pg$code %in% SSA], na.rm = T) / sum((pg$gdp_pc_2019 * pg$pop_2019)[pg$code %in% SSA], na.rm = T)
sum(pg$global_share[pg$code %in% LIC], na.rm = T)
wtd.mean(pg$rev_pc[pg$code %in% LIC], pg$pop_2019[pg$code %in% LIC], na.rm = T)
wtd.mean(pg$global_share_pc[pg$code %in% LIC], pg$pop_2019[pg$code %in% LIC], na.rm = T)
pooled_revenues * sum(pg$global_share[pg$code %in% LIC], na.rm = T) / sum((pg$gdp_pc_2019 * pg$pop_2019)[pg$code %in% LIC], na.rm = T)
pg$gdp_share[pg$code == "BDI"]
pg$rev_pc[pg$code == "BDI"]
pg$global_share_pc[pg$code == "BDI"]

# qplot(log10(GDPpcPPP), log10(gap), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $2.15 a day (2017 PPP) (%)", show.legend = FALSE) + 
#   geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw() +
#   geom_line(aes(y = log10(predicted_gap_simplfied)), color = "red", show.legend = FALSE) + labs(fill = "Adult population (2023)")
# with(pg, symbols(x=GDPpcPPP, y=gap, circles=adult_2023, inches=1/3,  ann=F, bg="steelblue2", fg=NULL))
# qplot(GDPpcPPP, gap, data = pg, size = adult_2023) + geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw() + coord_trans(x="log10")
# qplot(log10(GDPpcPPP), gap, data = pg, size = adult_2023) + geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw()
# qplot(GDPpcPPP, gap, data = pg, size = adult_2023) + geom_line(aes(y = predicted_gap), color = "red", show.legend = FALSE) + theme_bw() + coord_trans(x="log10")

# TODO: set it to 0 for high-income countries
# TODO: different colors for different income categories. # Thresholds for lower/upper middle-income: $1085/4255/13205 in GDP pc (nominal)
pg$share_revenues7 <- pg$predicted_gap7 * pg$adult_2023 # TODO fill missing values and use full pop instead of adults (also below)
pg$share_revenues7 <- pg$share_revenues7/sum(pg$share_revenues7, na.rm = T)
pg$share_revenues4 <- pg$predicted_gap4 * pg$adult_2023
pg$share_revenues4 <- pg$share_revenues4/sum(pg$share_revenues4, na.rm = T)
pg$share_revenues2 <- pg$predicted_gap2 * pg$adult_2023
pg$share_revenues2 <- pg$share_revenues2/sum(pg$share_revenues2, na.rm = T)
sort(setNames(pg$share_revenues7, pg$country))
sort(setNames(pg$share_revenues4, pg$country))
sort(setNames(pg$share_revenues2, pg$country))
cor(pg$share_revenues7, pg$share_revenues4, use = "complete.obs") # .9996
cor(pg$share_revenues7, pg$share_revenues2, use = "complete.obs") # .959

wealth_tax_revenues <- 0.0085*96e12 # From a 2% tax above $5 million, cf. Chancel et al. (2022) https://wid.world/world-wealth-tax-simulator/
pooled_revenues <- 0.5 * wealth_tax_revenues
pg$wealth_tax_rev_pc <- pooled_revenues * pg$share_revenues4 / pg$adult_2023 # per year
sort(setNames(pg$wealth_tax_rev_pc/12, pg$country)) # per month: $4.6 in India, 2.8 in China, 14.6 in RDC, 1.3 in U.S.


##### SSPs #####
ssp_countries <- read.xlsx("../data/SSP_cmip6_iam_model_region_mapping.xlsx")
names(ssp_countries)[1] <- "code"
ssp_countries$R5_region <- sub("R5", "R5.2", ssp_countries$R5_region)
co2_pop <- merge(ssp_countries[,c("code", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION", "R5_region")], co2_pop)
pop_un <- list()
pop.R5 <- co2_pop %>% group_by(R5_region) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_", names(co2_pop))], sum, na.rm = T)
pop_un[["IMAGE"]] <- co2_pop %>% group_by(IMAGE.REGION) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_", names(co2_pop))], sum, na.rm = T)
pop_un[["MESSAGE-GLOBIOM"]] <- co2_pop %>% group_by(`MESSAGE-GLOBIOM.REGION`) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_", names(co2_pop))], sum, na.rm = T)
names(pop.R5)[1] <- names(pop_un[["IMAGE"]])[1] <- names(pop_un[["MESSAGE-GLOBIOM"]])[1] <- "region"
pop_un[["IMAGE"]] <- rbind(pop_un[["IMAGE"]], pop.R5)
pop_un[["MESSAGE-GLOBIOM"]] <- rbind(pop_un[["MESSAGE-GLOBIOM"]], pop.R5)


SSPs <- read.csv("../data/SSPs.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/iam_v2/SSP_IAM_V2_201811.csv.zip
SSPs_countries <- read.csv("../data/SSP_CMIP6.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/cmip6/SSP_CMIP6_201811.csv.zip
# poverty, Gini: https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/additional/NRao_et_al_GiniProjections_2018.zip
# region mapping: https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/cmip6/cmip6_iam_model_region_mapping.xlsx
# In SSPs_countries, IMAGE has only SCENARIO: SSP1-19, SSP1-26; MESSAGE-GLOBIOM: SSP2-45; AIM/CGE: SSP3-70 (Baseline), SSP3-LowNTCF; GCAM4: SSP4-34, SSP4-60; REMIND-MAGPIE: SSP5-34-OS, SSP5-85 (Baseline); WITCH-GLOBIOM is absent
#   while in SSPs, each MODEL has many SCENARIOs
# I think that emissions of SSPs are first defined in CMIP6 (i.e. SSPs_countries) and then passed as inputs to IAMs, which yield consistent bud modified emissions, in SSPs
# /!\ World emissions are not equal to the sum of the 5 R5.2 regions TODO! why? check regions definition
# Nb regions (excluding the 5 R5.2 ones): AIM/CGE: 18 (3 letters), IMAGE: 27, GCAM4: 33, MESSAGE-GLOBIOM: 12, REMIND-MAGPIE: 12 (different), WITCH-GLOBIOM: absent from SSPs_countries

# In ssp, data of the 6 lower-case macro-regions (incl. 'world') is given by IAM results (except for adult_*) while the rest is given by CMIP6 (emissions) or UN median projection (population). *adult_ is estimated by shrinking pop_ by the median UN adult/pop ratio in each region.
#         emissions_pc (resp. emissions_pa) is emissions divided by total (resp. adult) population
vars_SSPs <- c("emissions_pc" = "Emissions|CO2", "fossil_emissions" = "Emissions|CO2|Fossil Fuels and Industry", # Mt CO2/yr => tCO2/yr/pers
               "energy_pc" = "Final Energy", # EJ/yr => GJ/yr/pers
               "gdp_pc" = "GDP|PPP", # billion US$2005/yr => US$2005/yr/pers, "conso" = "Consumption"
               "pop" = "Population", # million => pers
               "carbon_price" = "Price|Carbon") # US$2005/t CO2
ssp <- list() # 2 min
for (i in unique(SSPs$SCENARIO)) { # unique(SSPs$MODEL)
  ssp[[i]] <- list()
  for (j in c("IMAGE", "MESSAGE-GLOBIOM")) {
    ssp[[i]][[j]] <- data.frame(region = c(paste0("iam_", unique(SSPs$REGION)), unique(SSPs_countries$REGION[SSPs_countries$SCENARIO %in% c("SSP1-19", "SSP2-45")]))) # unique(SSPs_countries$REGION)
    for (y in years) {
      for (v in names(vars_SSPs)) {
        # print(paste(i, j, v, y))
        if (sum(SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]) > 0 && all(SSPs$REGION[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]] == unique(SSPs$REGION))) {
          ssp[[i]][[j]][[paste0(v, "_", y)]] <- c(SSPs[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v], paste0("X", y)], rep(NA, 42))
        }
      }
      # print(paste(i, j, v, y))
      if (sum(SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2" & SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & !is.na(SSPs_countries[[paste0("X", y)]])) > 0) {
        temp <- SSPs_countries[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", paste0("X", y)]
        names(temp) <- SSPs_countries$REGION[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2"]
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]][7:48] <- temp[ssp[[i]][[j]]$region][7:48]
      }
      if (paste0("emissions_pc_", y) %in% names(ssp[[i]][[j]])) {
        ssp[[i]][[j]][[paste0("emissions_", y)]] <- ssp[[i]][[j]][[paste0("emissions_pc_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("pop_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        ssp[[i]][[j]][[paste0("adult_", y)]] <- NA
        ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)] <- (pop_un[[j]][[paste0("adult_", y)]]/pop_un[[j]][[paste0("pop_", y)]])[match(unique(SSPs$REGION)[-6], pop_un[[j]]$region)] * ssp[[i]][[j]][[paste0("pop_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)]
        ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == "iam_World"] <- sum(ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)])
        ssp[[i]][[j]][[paste0("adult_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("adult_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region]
        # TODO? Adjust pop_ and adult_ of CMIP6 to match the sum of IAM?
        if (i %in% c("SSP1-19", "SSP1-26")) {
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "AFR"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("WAF", "EAF", "SAF", "RSAF")])
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "LAM"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("MEX", "RCAM", "RSAM")])
        }
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("emissions_pa_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
        for (var in c("energy", "gdp")) ssp[[i]][[j]][[paste0(var, "_pc_", y)]] <- 1e3 * ssp[[i]][[j]][[paste0(var, "_pc_", y)]]
      }
    }
    ssp[[i]][[j]]$region[1:6] <- c("asia", "lam", "maf", "oecd", "ref", "world")
  }
}

ssp1_19 <- ssp$`SSP1-19`$IMAGE
ssp1_26 <- ssp$`SSP1-26`$IMAGE
ssp2_26 <- ssp$`SSP2-26`$IMAGE
ssp2_45 <- ssp$`SSP2-45`$IMAGE
ssp2 <- ssp$`SSP2-45`$`MESSAGE-GLOBIOM`
# TODO: SSP5-Baseline is SSP5-8.5 but what is SSP1-Baseline? SSP2-Baseline? SSP2-4.5 
rm(SSPs, SSPs_countries)
rm(ssp)

# TODO! use Greenpeace, Global Energy Assessment

ssps <- c("ssp1_19", "ssp1_26", "ssp2_26", "ssp2_45", "ssp2")
names(ssps) <- c("SSP1-1.9 (1.4 °C)", "SSP1-2.6 (1.8 °C)", "SSP2-2.6 (1.8 °C)", "SSP2-4.5 (2.7 °C)", "SSP2 (baseline)")
# In all SSPs considered, carbon prices converge in 2035 at the latest. We use Asian carbon price, which are very close to the others except for ssp2 in 2020 and 2030
# for (s in ssps) for (y in years) if (gap(d(s)[[paste0("carbon_price_", y)]]) > .1) print(paste("Non unique carbon price for", s, y, "gap: ", round(gap(d(s)[[paste0("carbon_price_", y)]]), 3)))
# ssp1_19$carbon_price_2020[1:6] # c("asia", "lam", "maf", "oecd", "ref", "world")
# ssp2_26$carbon_price_2020[1:6]
# ssp2_26$carbon_price_2030[1:6]
# ssp2_45$carbon_price_2020[1:6]
# ssp2_45$carbon_price_2030[1:6]
# ssp2$carbon_price_2020[1:6]
# ssp2$carbon_price_2030[1:6]

carbon_price <- revenues_pc <- world_emissions <- world_population <- revenues_over_gdp <- list()
for (s in ssps) {
  carbon_price[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), years)
  world_emissions[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("emissions_", y)]][d(s)$region == "world"]), years)
  world_population[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("pop_", y)]][d(s)$region == "world"]), years)
  revenues_over_gdp[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"]) / sapply(years, function(y) d(s)[[paste0("gdp_pc_", y)]][d(s)$region == "world"])
  revenues_pc[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"])
  for (y in years) eval(str2expression(paste0(s, "$gain_pc_", y, " <- (revenues_pc$", s, "['", y, "'] - carbon_price$", s, "['", y, "'] * ", s, "$emissions_pc_", y, ") /12"))) # Average gain in $/month
}

mar <- par()$mar
par(mar = c(2.1, 4.1, .1, .1))
plot(years, sapply(years, function(y) ssp1_19[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "green", lwd = 2, type = 'l', xlab = "", ylab = "Carbon price (US$2005/tCO2)", xlim = c(2010, 2100), ylim = c(0, 900))
lines(years, sapply(years, function(y) ssp1_26[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "blue", lwd = 2, type = 'l')
lines(years, sapply(years, function(y) ssp2_26[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "purple", lwd = 2, type = 'l')
lines(years, sapply(years, function(y) ssp2_45[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "red", lwd = 2, type = 'l')
# lines(years, sapply(years, function(y) ssp2_45[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "orange", lwd = 2, type = 'l')
# lines(years, sapply(years, function(y) ssp2[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "red", lwd = 2, type = 'l')
abline(h = seq(0, 900, 100), lty = 3, col = "gray") + abline(v = years, lty = 3, col = "gray")
# legend("topleft", legend = c("SSP1-1.9 (1.4 °C)", "SSP1-2.6 (1.8 °C)", "SSP2-2.6 (1.8 °C)", "SSP2-4.5 (2.7 °C)", "SSP2 (baseline)"), col = c("green", "blue", "purple", "orange", "red"), lwd = 2)
legend("topleft", legend = c("SSP1-1.9 (1.4 °C)", "SSP1-2.6 (1.8 °C)", "SSP2-2.6 (1.8 °C)", "SSP2-4.5 (2.7 °C)"), col = c("green", "blue", "purple", "red"), lwd = 2)
save_plot(filename = "SSP_carbon_price", folder = "../figures/policies/", format = "pdf", trim = FALSE)
save_plot(filename = "SSP_carbon_price", folder = "../figures/policies/", format = "png")

par(mar = c(4.1, 4.1, .1, .1))
plot_ssp <- function(ssp, var = "emissions", save = T, ylim = NULL, regions = c("OECD" = "oecd", "Middle East & Africa" = "maf", "Asia" = "asia", "Former Soviet Union" = "ref", "Latin America" = "lam")) {
  colors <- c("blue", "orange", "green", "red", "purple", "cyan", "gray", "yellow")
  plot(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == regions[1]]), col = colors[1], lwd = 2, type = 'l', xlab = paste("Evolution of ", var, " p.c. from the Global Climate Scheme in", names(ssps)[which(ssps == ssp)]), 
       ylab = if (var == "gain") "Average net gain per capita ($/month)" else "Emissions per capita (tCO2/yr)", xlim = c(if ("maf" %in% regions) 2010 else 2020, 2100), ylim = if (!is.null(ylim)) ylim else c(-6, 12))
  for (i in 1:length(regions)) lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == regions[i]]), col = colors[i], lwd = 2, type = 'l')
  # lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == "lam"]), col = "purple", lwd = 2, type = 'l')
  # lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == "maf"]), col = "orange", lwd = 2, type = 'l')
  # lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == "oecd"]), col = "blue", lwd = 2, type = 'l')
  # lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == "ref"]), col = "red", lwd = 2, type = 'l')
  lines(years, sapply(years, function(y) d(ssp)[[paste0(var, "_pc_", y)]][d(ssp)$region == "world"]), col = "black", lwd = if (var == "emissions") 4 else 2, type = 'l')
  if (is.null(ylim)) abline(h = seq(-5, 15, 2.5), lty = 3, col = "gray") + abline(v = years, lty = 3, col = "gray") else grid()
  legend(if (var == "emissions") { if (ssp == "ssp2") "bottomleft" else "topright" } else { if (ssp == "ssp2_45") "bottomleft" else "topleft"}, 
         legend = c(names(regions), if (var == "emissions") "World" else NULL), col = c(colors[1:length(regions)], if (var == "emissions") "black" else NULL), lwd = c(rep(2, length(regions)), if (var == "emissions") 4 else NULL))
  if (save) {
    save_plot(filename = paste0(var, "_pc_", toupper(ssp), if (!"maf" %in% regions) "_many" else ""), folder = "../figures/policies/", format = "png")
    save_plot(filename = paste0(var, "_pc_", toupper(ssp), if (!"maf" %in% regions) "_many" else ""), folder = "../figures/policies/", format = "pdf", trim = FALSE)
  }
}
plot_ssp("ssp1_19") # OECD = Africa in 2038, OECD < Africa <= 0 from 2050 onwards
plot_ssp("ssp1_26") # OECD = Africa in 2060, OECD < 0 < Africa from 2070 onwards
plot_ssp("ssp2_26") # OECD < 0 < Africa from 2065 onwards
plot_ssp("ssp2_45") # OECD > Africa, with convergence between regions
# plot_ssp("ssp2") # OECD > Africa, with divergence between regions

# regions_IMAGE <- c("Brazil" = "BRA", "Canada" = "CAN", "Central Europe" = "CEU", "East Africa" = "EAF", "India" = "INDIA", "Indonesia" = "INDO", "Japan" = "JAP", "Korea" = "KOR",
#   "Middle East" = "ME", "Mexico" = "ME", "North Africa" = "NAF", "Oceania" = "OCE", "Rest Central America" = "RCAM", "Rest Southern Africa" = "RSAF", "Rest South America" = "RSAM", 
#   "Rest South Asia" = "RSAS", "South Africa" = "SAF", "South East Asia" = "SEA", "Kazakhstan region" = "STAN", "Turkey" = "TUR", "Ukraine region" = "UKR", "USA" = "USA", 
#   "West Africa" = "WAF", "Western Europe" = "WEU", "Russia" = "RUS")
# regions_MESSAGE <- c("Sub-saharan Africa" = "AFR", "China" = "CPA", "Eastern Europe" = "EEU", "Former Soviet Union" = "FSU", "Latin America" = "LAM",
#                      "Middle East and North Africa" = "MEA", "North America" = "NAM", "Pacific OECD" = "PAO", "South East Asia" = "PAS", "South Asia" = "SAS", "Western Europe" = "WEU")
regions_IMAGE <- c("USA" = "USA", "West Africa" = "WAF", "India" = "INDIA", "Western Europe" = "WEU", "Brazil" = "BRA", "China" = "CHN", "Middle East" = "ME")
regions_IMAGE_modif <- c("USA" = "USA", "Sub-Saharan Africa" = "AFR", "India" = "INDIA", "Western Europe" = "WEU", "Latin America" = "LAM", "China" = "CHN", "Middle East" = "ME")
regions_MESSAGE <- c("North America" = "NAM", "Sub-saharan Africa" = "AFR", "South Asia" = "SAS", "Western Europe" = "WEU", "Latin America" = "LAM", "China" = "CPA", "Middle East and North Africa" = "MEA")
plot_ssp("ssp1_19", regions = regions_IMAGE_modif) 
plot_ssp("ssp1_26", regions = regions_IMAGE_modif) # No data for ssp2_26, ssp2_45
plot_ssp("ssp2", regions = regions_MESSAGE, ylim = c(-10, 15))

# /!\ Huge variations in gains of Former Soviet Union, questioning the data quality
plot_ssp("ssp1_19", var = "gain", ylim = c(-70, 160)) # from 2040 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp1_26", var = "gain", ylim = c(-30, 50)) # from 2060 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp2_26", var = "gain", ylim = c(-70, 360)) # from 2060 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp2_45", var = "gain", ylim = c(-22, 6)) # OECD, lam, FSU always lose and Africa, Asia always win
# plot_ssp("ssp2", var = "gain", ylim = c(-20, 60)) # Very low amounts, lam wins and OECD, FSU lose

plot_ssp("ssp1_19", var = "gain", ylim = c(-100, 190), regions = regions_IMAGE_modif) # 
plot_ssp("ssp1_26", var = "gain", ylim = c(-30, 80), regions = regions_IMAGE_modif) # 
plot_ssp("ssp2", var = "gain", ylim = c(-30, 40), regions = regions_MESSAGE) #

# average world emissions by SSP throughout 21st century: 1 for 1.9; 2 for 2.6; 4 for 4.5.
# mean(sapply(years, function(y) ssp1_19[[paste0("emissions_pc_", y)]][ssp1_19$region == "world"])) # 1.1
# mean(sapply(years, function(y) ssp1_26[[paste0("emissions_pc_", y)]][ssp1_26$region == "world"])) # 2.2
# mean(sapply(years, function(y) ssp2_26[[paste0("emissions_pc_", y)]][ssp2_26$region == "world"])) # 1.9
# mean(sapply(years, function(y) ssp2_45[[paste0("emissions_pc_", y)]][ssp2_45$region == "world"])) # 4.1
# mean(sapply(years, function(y) ssp2[[paste0("emissions_pc_", y)]][ssp2$region == "world"])) # 4.1


##### Global Energy Assessment #####
gea_emissions <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_emissions_regions.xlsx")
gea_pop <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_emissions_regions.xlsx")
gea <- list() # GEA model is done with MESSAGE
for (i in c("IMAGE", "GEA")) {
  gea[[i]] <- data.frame(region = unique(gea_pop$Region))
  for (y in years) {
    gea[[i]][[paste0("emissions_", y)]] <- gea_emissions[[as.character(y)]][gea_emissions$Model == i]
    gea[[i]][[paste0("pop_", y)]] <- gea_pop[[as.character(y)]][gea_emissions$Model == i]
    gea[[i]][[paste0("emissions_pc_", y)]] <- gea[[i]][[paste0("emissions_", y)]]/gea[[i]][[paste0("pop_", y)]]
  }
}
# TODO! finir ça


##### NDCs #####
# Assuming 2030 emissions will be like NDCs and global carbon price of $90, emissions and basic income would be 54% higher than target.
# China would lose: -25 $/month per person, India win +8, EU +4, US -52, Vietnam +28, Brazil +38, Australia -113
ndc <- read.xlsx("../data/NDCs_Gao.xlsx")
EU28_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
EU27_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
temp <- rbind(co2_pop, c("EU28", "EU-28", colSums(co2_pop[co2_pop$code %in% EU28_countries, 3:ncol(co2_pop)])))
for (v in setdiff(names(temp), c("country", "code"))) temp[[v]] <- as.numeric(temp[[v]])
temp$mean_gain_2030[temp$country == "EU-28"] <- temp$mean_gain_2030[temp$code %in% EU28_countries] %*% temp$adult_2030[temp$code %in% EU28_countries] / sum(temp$adult_2030[temp$code %in% EU28_countries])
temp$emissions_pa_2030 <- temp$emissions_2030/temp$adult_2030
ndc <- merge(ndc, temp, by = "country")
ndc <- ndc[, c("country", "emissions_2030", "adult_2030", "pop_2030", "mean_gain_2030", "emissions_pa_2030", "emissions_ndc_2030")]
for (v in setdiff(names(ndc), c("country", "Abbreviation"))) ndc[[v]] <- as.numeric(ndc[[v]])
ndc <- rbind(ndc, c("Total", "total", colSums(ndc[,3:ncol(ndc)])))
for (v in setdiff(names(ndc), c("country", "Abbreviation"))) ndc[[v]] <- as.numeric(ndc[[v]])
share_covered_gao <- sum(ndc$emissions_2030, na.rm = T)/co2_emissions_2030 # 85%
excess_emissions_ndc <- ndc$emissions_ndc_2030[ndc$country == "Total"]/co2_emissions_2030/share_covered_gao # 1.54
ndc$emissions_ndc_2030 <- 1e6 * ndc$emissions_ndc_2030
ndc$emissions_pa_ndc_2030 <- ndc$emissions_ndc_2030/ndc$adult_2030
ndc$mean_gain_ndc_2030 <- 30*excess_emissions_ndc - (90 * ndc$emissions_pa_ndc_2030) /12
ndc$gain_ndc_over_estimate_2030 <- ndc$mean_gain_ndc_2030/ndc$mean_gain_2030
ndc$emissions_ndc_over_estimate_2030 <- ndc$emissions_ndc_2030/ndc$emissions_2030
View(ndc[, c("country", "mean_gain_2030", "mean_gain_ndc_2030", "gain_ndc_over_estimate_2030", "emissions_pa_2030", "emissions_pa_ndc_2030", "emissions_ndc_over_estimate_2030")])


##### Sandbox #####
beep()
View(ssp)
SSPs_countries[SSPs_countries$MODEL == "MESSAGE-GLOBIOM" & SSPs_countries$SCENARIO == "SSP1-19" & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", c("REGION", "X2020")]
SSPs_countries[SSPs_countries$MODEL == "MESSAGE-GLOBIOM" & SSPs_countries$SCENARIO == "SSP2-45" & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", c("REGION", "X2020")]
setNames(ssp$`SSP2-45`$`MESSAGE-GLOBIOM`$emissions_2020, ssp$`SSP2-45`$`MESSAGE-GLOBIOM`$region)

unique(SSPs_countries$REGION[SSPs_countries$SCENARIO %in% c("SSP1-19", "SSP2-45")])
SSPs_countries$REGION[SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2" & SSPs_countries$MODEL == "IMAGE" & !is.na(SSPs_countries$X2020)]

unique(SSPs_countries$SCENARIO[SSPs_countries$MODEL == "MESSAGE-GLOBIOM"])
unique(SSPs$MODEL)
unique(SSPs_countries$SCENARIO)

SSP1/2/5-2.6 (or -1.9)
SSP2-45



SSPs_countries[SSPs_countries$MODEL == "MESSAGE-GLOBIOM" & SSPs_countries$SCENARIO == "SSP2-45" & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", c("REGION", "X2020")]
setNames(ssp$`SSP2-45`$`MESSAGE-GLOBIOM`$emissions_2020, ssp$`SSP2-45`$`MESSAGE-GLOBIOM`$region)


SSPs[SSPs$MODEL == "MESSAGE-GLOBIOM" & SSPs$SCENARIO == "SSP2-45" & SSPs$VARIABLE == "GDP|PPP", c("REGION", "X2050")]
SSPs[SSPs$MODEL == "IMAGE" & SSPs$SCENARIO == "SSP2-45" & SSPs$VARIABLE == "GDP|PPP", c("REGION", "X2050")]
SSPs[SSPs$MODEL == "GCAM4" & SSPs$SCENARIO == "SSP2-45" & SSPs$VARIABLE == "GDP|PPP", c("REGION", "X2050")]


##### 60% of emissions #####
co2_pop$share_territorial_2019 <- co2_pop$territorial_2019/sum(co2_pop$territorial_2019)
cumulative_emissions <- function(variable_gain = "mean_gain_over_gdp_2019", variable_share = "share_emissions_2019") {
  order_by_gain <- order(-co2_pop[[variable_gain]]) 
  emissions_ordered_by_gain <- setNames(co2_pop[[variable_share]], co2_pop$country)[order_by_gain]
  (cumul_emissions <- setNames(sapply(1:204, function(i) sum(emissions_ordered_by_gain[1:i])), names(emissions_ordered_by_gain)))
  return(round(cbind(share = co2_pop[[variable_share]][order_by_gain], cumul = cumul_emissions, gain = co2_pop$mean_gain_2030[order_by_gain]), 3))
}

cumulative_emissions("mean_gain_over_gdp_2019")
cumulative_emissions("mean_gain_2030") 
cumulative_emissions("mean_gain_over_gdp_2019", "territorial_2019")
cumulative_emissions("mean_gain_2030", "territorial_2019") 
# footprint
sum(co2_pop$share_emissions_2019[co2_pop$mean_gain_2030 > 0 | co2_pop$code %in% c("CHN", EU28_countries)]) # exactly 60%
sum(co2_pop$share_emissions_2019[co2_pop$code %in% c("CHN", "IND", "USA", EU28_countries)]) # 62.4%
sum(co2_pop$share_emissions_2019[co2_pop$mean_gain_2030 > 0]) # 20.8% of global emissions in countries with gain > 0
co2_pop$share_emissions_2019[co2_pop$code == "USA"] # 16.0%
co2_pop$share_emissions_2019[co2_pop$code == "IND"] # 7.0%
co2_pop$share_emissions_2019[co2_pop$code == "CHN"] # 28.1%
sum(co2_pop$share_emissions_2019[co2_pop$code %in% EU28_countries]) # 11.4%
# territorial
sum(co2_pop$share_territorial_2019[co2_pop$mean_gain_2030 > 0 | co2_pop$code %in% c("CHN", EU28_countries)]) # exactly 60%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("CHN", "IND", "USA", EU28_countries)]) # 61.6%
sum(co2_pop$share_territorial_2019[co2_pop$mean_gain_2030 > 0]) # 20.9% of global emissions in countries with gain > 0
co2_pop$share_territorial_2019[co2_pop$code == "USA"] # 14.8%
co2_pop$share_territorial_2019[co2_pop$code == "IND"] # 7.4%
co2_pop$share_territorial_2019[co2_pop$code == "CHN"] # 30.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("JPN", "KOR", "NOR", "SWZ", "NLD", "CAN")]) # 7%, JP + SK: 5%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("AUS", "SAU", "QAT", "KWT", "ARE")]) # 4%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% EU28_countries]) # 9.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% EU28_countries & co2_pop$code != "GBR"]) # 8.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("JPN", "KOR")]) # 5%

# Countries with income lower than 2* world average and losers from GCS: 
# China, Russia, Turkey, Iran, Iraq, Algeria, Kazakhstan, Malaysia, Chile, South Africa, Lybia, Botswana, Namibia, Turkmenistan, Mongolia, and some EU countries
co2_pop$country[co2_pop$gdp_pc_2020 < 2*wtd.mean(co2_pop$gdp_pc_2020, co2_pop$pop_2020) & co2_pop$mean_gain_2030 < 0]
sum(co2_pop$share_territorial_2019[co2_pop$gdp_pc_2020 < 2*wtd.mean(co2_pop$gdp_pc_2020, co2_pop$pop_2020) & co2_pop$mean_gain_2030 < 0], na.rm = T) # 45.6%

# USA: 15 / EU+UK: 9 / CHN: 30 / other middle inc, high em: 15 / gain: 21 / other OECD Asia, America or Europe: 7 / other high inc: 4

cor(co2_pop$emissions_pa_2019, co2_pop$gdp_pc_2019, use = "complete.obs") # .64
sort(setNames(co2_pop$emissions_pa_2019/co2_pop$gdp_pc_2019, co2_pop$country))
wtd.mean(co2_pop$emissions_pa_2019[co2_pop$code %in% EU27_countries], co2_pop$adult_2019[co2_pop$code %in% EU27_countries])/co2_pop$emissions_pa_2019[co2_pop$country == "India"]

# Revenues retained relative to the world average revenues, in function of y (where GNI pc = 1+y * world average)
unlist(setNames(lapply(seq(0, 1, 0.1), function(x) 1+x-x*(1+x)), seq(0, 1, 0.1)))

# Adult population. In the world: 5.2G, incl. 54.7M (1%) >1M; 4.56M (.09%) >5M
sum(co2_pop$adult_2023[co2_pop$code %in% c("CAN", "USA")]) # North America: 312M incl. 22.1M (7.1%) >1M; 2.14M >5M (0.7%)
sum(co2_pop$adult_2023[co2_pop$code %in% c(EU28_countries, "UKR", "NOR", "BEL", "SWZ", "SRB", "ALB", "MAC")]) # 485M incl. 15.6M >1M (3.2%); 1.05M >5M (0.2%)
