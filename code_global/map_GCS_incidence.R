# TODO? share of winners per country: Ivanova & Wood (20) show that 2020 World average of 6t pc is at ~50 percentile in FR, ~65 in ES, ~50 in UK, ~20 in DE; Fremstad & Paul (19) show it's at ~20p in the U.S.
# TODO: more accurate assumption/computations (e.g. based on NDCs)

# plot maps and compare distributive effects of equal pc, contraction & convergence, greenhouse dvlpt rights, historical respo, and each country retaining its revenues
# equal pc
# historical respo, 1990
# grandfathering / each country retaining its revenues
# contraction & convergence, 2035
# greenhouse dvlpt rights

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
# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions CO2 emissions from fossil fuels and industry. Land use change is not included.
co2 <- read.csv("../data/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2019),]
temp <- co2[co2$Year == 2019,]
temp$Year <- 2023
co2 <- rbind(co2, temp)
names(co2) <- c("country", "code", "year", "territorial", "footprint")
# TODO impute data for Taiwan, small islands, Eswatini... in co2 and GDPpc


##### GDP pc data #####
GDPpc <- read.csv("../data/GDPpc_2015$_nominal.csv") # GDP per capita (constant 2015 US$) https://data.worldbank.org/indicator/NY.GDP.PCAP.KD March 1, 2023 (last year available is 2021)
GDPpc$code <- GDPpc$Country.Code
GDPpc <- rbind(data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2015, "year" = 2015), data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2019, "year" = 2019))
GDPpc$gdp_pc[GDPpc$code %in% c("ERI", "PRK", "SSD", "VEN", "YEM")] <- c(715, 654, 467, 3640, 702) # Impute data from other sources for Eritrea (IMF, 2023), North Korea (IMF, 2021), South Sudan (IMF, 2023), Venezuela (IMF, 2023), Yemen (WB, 2018)
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
co2_pop <- co2_pop[, !colnames(co2_pop) %in% c(paste0("missing_footprint_", c(2015, 2019, 2023, years)), paste0("gdp_pc_", years))]
co2_pop <- co2_pop[!co2_pop$country %in% c("World", "Kosovo", NA), sapply(names(co2_pop), function(v) any(!is.na(co2_pop[[v]])))]
co2_pop$gdp_2019 <- co2_pop$gdp_pc_2019 * co2_pop$pop_2019
co2_pop$country_map <- co2_pop$country
co2_pop$country_map[co2_pop$country == "United States"] <- "USA"
co2_pop$country_map[co2_pop$country == "United Kingdom"] <- "UK"
co2_pop$country_map[co2_pop$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
co2_pop$country_map[co2_pop$country == "Congo"] <- "Republic of Congo"
co2_pop$country_map[co2_pop$country == "Cote d'Ivoire"] <- "Ivory Coast"
co2_pop$country_map[co2_pop$country == "Czechia"] <- "Czech Republic"
co2_pop$share_territorial_2019 <- co2_pop$territorial_2019/sum(co2_pop$territorial_2019)
# rm(pop, co2)  
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
  # Computes revenues_pa
  # reduction_factor <- co2_emissions_2030/sum(df[[paste0("emissions_pa_", base_year)]] * df$adult_2030)
  # df$emissions_pa_2030 <- reduction_factor * df[[paste0("emissions_pa_", base_year)]]
  # df[[paste0("revenues_pa_", year)]] <- df$emissions_pa_2030 * 90 /12
  # df[[paste0("revenues_pa_", year)]] <- df[[paste0("revenues_pa_", year)]] * df$adult_2030 / df[[paste0("adult_", year)]]
  # Below computations are equivalent to the above lines, they just make additional steps through share_emissions
  df[[paste0("demographic_evolution_", base_year)]] <- (df$adult_2030/df[[paste0("adult_", base_year)]]) * (sum(df[[paste0("adult_", base_year)]])/adult_pop_2030)
  df[[paste0("emissions_pa_", base_year)]] <- df[[paste0("emissions_", base_year)]]/df[[paste0("adult_", base_year)]]
  df[[paste0("share_emissions_", base_year)]] <- df[[paste0("emissions_", base_year)]]/sum(df[[paste0("emissions_", base_year)]])
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_", base_year)]] * df[[paste0("demographic_evolution_", base_year)]]
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]]/sum(df[[paste0("share_emissions_2030_base_", base_year)]])
  df$emissions_2030 <- df[[paste0("share_emissions_2030_base_", base_year)]] * co2_emissions_2030
  df$emissions_pa_2030 <- df$emissions_2030 / df$adult_2030
  df[[paste0("revenues_pa_", year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]] * carbon_tax_revenues_2030 / df[[paste0("adult_", year)]]/12
  
  df[[paste0(type, "_gain_", year)]] <- 30 - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pa_", year)]]
  # Accounts for opting out (using 2019 GDP pc)
  df[[paste0("optout_right_", year)]] <- 2 - pmax(1, pmin(2, df$gdp_pc_2019 / wtd.mean(df$gdp_pc_2019, df$pop_2019)))
  temp <- rep(T, nrow(df)) 
  basic_income <- wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("adult_", year)]])
  df[[paste0("large_footprint_", year)]] <- (df[[paste0("revenues_pa_", year)]] > basic_income)
  df[[paste0("participation_rate_", year)]] <- 1 - df[[paste0("large_footprint_", year)]] * df[[paste0("optout_right_", year)]]
  while (any(temp != df[[paste0("large_footprint_", year)]])) {
    temp <- df[[paste0("large_footprint_", year)]]
    basic_income <- wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("participation_rate_", year)]] * df[[paste0("adult_", year)]])
    df[[paste0("large_footprint_", year)]] <- (df[[paste0("revenues_pa_", year)]] > basic_income)
    df[[paste0("participation_rate_", year)]] <- 1 - df[[paste0("large_footprint_", year)]] * df[[paste0("optout_right_", year)]]
  }
  # basic_income <- 30 * wtd.mean(df[[paste0("participation_rate_", year)]], df[[paste0("revenues_pa_", year)]] * df[[paste0("adult_", year)]])
  print(paste0("Basic income adjusted for opting-out countries: ", round(basic_income, 1), " (down from ", round(wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("adult_", year)]]), 1), ")"))
  df[[paste0(type, "_gain_adj_", year)]] <- df[[paste0("participation_rate_", year)]] * (basic_income - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pa_", year)]])
  if (return_data) return(df) else return(df[[paste0(type, "_gain_", year)]])
}
co2_pop <- compute_gain(year = 2015, base_year = 2015, type = "median") # creates median_gain_2015. Adj: basic income 36 -> 32
co2_pop <- compute_gain(year = 2019, base_year = 2019, type = "mean") # Adj: 34 -> 29
co2_pop <- compute_gain(year = 2030, base_year = 2019, type = "mean") # creates mean_gain_2030. Adj: 30 -> 25

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
wtd.quantile(co2_pop$mean_gain_2030, weights = co2_pop$adult_2019, probs = seq(0, 1, 1/6)) 
thresholds_map <- c(-Inf, -70, -30, -20, -10, 0, 10, 15, 20, 25, Inf)
plot_world_map("mean_gain_2030", breaks = thresholds_map, format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in $/month)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
# If bug, first use save = F then again save = T (or reinstall ggalt)
# Looks nice with width: 1160, height: 560 (one needs to adjust manually for PDF)

plot_world_map("median_gain_2015", breaks = thresholds_map, format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Median net\ngain per capita\nfrom the GCS\n(in $/month)", #fill_na = T,
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
               legend = "Average net\ngain per capita\nfrom the GCS\n(in % of GDP)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 

plot_world_map("mean_gain_adj_2030", breaks = c(-Inf, -70, -30, -20, -10, -.1, .1, 5, 10, 15, 20, Inf), format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -70, -30, -20, -10, 0, 0, 5, 10, 15, 20, Inf), sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in $/month)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 

co2_pop$mean_gain_adj_over_gdp_2019 <- 100*12*co2_pop$mean_gain_adj_2019/co2_pop$gdp_pc_2019 
sum(-(co2_pop$mean_gain_adj_over_gdp_2019 * co2_pop$gdp_pc_2019 * co2_pop$adult_2019 / 100)[co2_pop$mean_gain_adj_over_gdp_2019 < 0], na.rm = T)/sum(co2_pop$gdp_2019, na.rm = T) # 0.7% of GWP redistributed from high- to low-emitters
sort(setNames(co2_pop$mean_gain_adj_over_gdp_2019, co2_pop$country), decreasing = T)
plot_world_map("mean_gain_adj_over_gdp_2019", breaks = c(-Inf, -5, -2, -1, -.5, -.01, .01, 1, 5, 20, 50, Inf), format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -5, -2, -1, -.5, 0, 0, 1, 5, 20, 50, Inf), sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in % of GDP)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 


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

GDPpcPPP <- read.csv("../data/GDPpc_2017$_PPP.csv") # GDP per capita, PPP (constant 2017 international $) https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD March 1, 2023  (last year available is 2021)
GDPpcPPP$code <- GDPpcPPP$Country.Code
# all(GDPpcPPP$country == pg$country)
GDPpcPPP$GDPpcPPP[!is.na(pg7$last_year)] <- sapply(which(!is.na(pg7$last_year)), function(i) GDPpcPPP[[paste0("X", pg7$last_year[i])]][i])
# TODO! replace last_year (2021) by 2019 to avoid covid effects
# We don't replace missing values for GDPpc in gap's last year (while other years are available for GDPpc) as they always coincide with missing values for poverty gaps
# GDPpcPPP <- extract_last_year(df = GDPpcPPP, cols = paste0("X", 1960:2021), var_name = "gdp_last_year")
# sum(is.na(GDPpcPPP$GDPpcPPP[GDPpcPPP$code %in% co2_pop$code])) # 41 missing values for GDP pc for the year at which we have the last poverty gap estimate, we use instead the last available data for GDP (in 2017 constant $)
# GDPpcPPP$GDPpcPPP[is.na(GDPpcPPP$GDPpcPPP)] <- GDPpcPPP$gdp_last_year[is.na(GDPpcPPP$GDPpcPPP)]
# GDPpcPPP$Country.Name[GDPpcPPP$code %in% co2_pop$code][is.na(GDPpcPPP$GDPpcPPP[GDPpcPPP$code %in% co2_pop$code])] # No data (for any year) for Cuba, Eritrea, North Korea, South Sudan, Syria, Venezuela, Yemen. TODO: complete from other sources

pg <- merge(merge(pg2, merge(pg4, pg7)), GDPpcPPP[, c("Country.Name", "code", "GDPpcPPP")]) # TODO: do not remove region groupings (or recreate them, once we've done the share computations)

# co2_pop$country[co2_pop$code %in% setdiff(co2_pop$code, pg$code)]
# pg$Country.Name[pg$code %in% setdiff(pg$code, co2_pop$code)]
pg <- merge(pg, co2_pop)

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
                        PPP = F, list_month = T, df = pg, global_revenues = 6.87e+11, min_pop = 30e6, # 4.08e+11
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
cat(paste(kbl(table_pg[table_pg[,1] > 0.04,], "latex", caption = "Allocation of the global wealth tax revenues.", position = "b", escape = F, booktabs = T, digits = c(2, 0, 2, 2), linesep = rep("", nrow(table_pg)-1), longtable = T, label = "allocation",
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
wealth_tax_revenues <- 0.013*96e12 # From a 2% tax above $5 million / 4% above $100M / 6% above $1G, cf. Chancel et al. (2022) https://wid.world/world-wealth-tax-simulator/ # 0.0085
pooled_revenues <- 0.5 * wealth_tax_revenues
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

wealth_tax_revenues <- 1.3*96e12 # From a 2% tax above $5 million / 4% above $100M / 6% above $1G, cf. Chancel et al. (2022) https://wid.world/world-wealth-tax-simulator/ # 0.0085
pooled_revenues <- 0.5 * wealth_tax_revenues
pg$wealth_tax_rev_pc <- pooled_revenues * pg$share_revenues4 / pg$adult_2023 # per year
sort(setNames(pg$wealth_tax_rev_pc/12, pg$country)) # per month: $4.6 in India, 2.8 in China, 14.6 in RDC, 1.3 in U.S.


##### SSPs #####
ssp_countries <- read.xlsx("../data/SSP_cmip6_iam_model_region_mapping.xlsx")
names(ssp_countries)[1] <- "code"
ssp_countries$R5_region <- sub("R5", "R5.2", ssp_countries$R5_region)
co2_pop <- merge(pg[,c("code", "GDPpcPPP")], co2_pop)
co2_pop$gdp_ppp_now <- co2_pop$GDPpcPPP * co2_pop$pop_2020
co2_pop <- merge(ssp_countries[,c("code", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION", "R5_region")], co2_pop)
pop_un <- list()
pop.R5 <- co2_pop %>% group_by(R5_region) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
pop_un[["IMAGE"]] <- co2_pop %>% group_by(IMAGE.REGION) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
pop_un[["MESSAGE-GLOBIOM"]] <- co2_pop %>% group_by(`MESSAGE-GLOBIOM.REGION`) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
names(pop.R5)[1] <- names(pop_un[["IMAGE"]])[1] <- names(pop_un[["MESSAGE-GLOBIOM"]])[1] <- "region"
pop_un[["IMAGE"]] <- rbind(pop_un[["IMAGE"]], pop.R5)
pop_un[["MESSAGE-GLOBIOM"]] <- rbind(pop_un[["MESSAGE-GLOBIOM"]], pop.R5)

View(co2_pop[, c("code", "country", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION")])
co2_pop$`MESSAGE-GLOBIOM.REGION`[is.na(co2_pop$`MESSAGE-GLOBIOM.REGION`)] <- "LAM"
regions <- unique(co2_pop$`MESSAGE-GLOBIOM.REGION`)
message_region_by_image <- c("BRA" = "LAM", "CAN" = "NAM", "CEU" = "EEU", "CHN" = "CPA", "EAF" = "AFR", "INDIA" = "SAS", "INDO" = "PAS", "JAP" = "PAO", "KOR" = "PAS", "ME" = "MEA", "MEX" = "LAM", "NAF" = "MEA", "OCE" = "PAO", "RCAM" = "LAM", "RSAF" = "AFR", "RSAM" = "LAM", "RSAS" = "SAS", "RUS" = "FSU", "SAF" = "AFR", "SEAS" = "PAS", "STAN" = "FSU", "TUR" = "WEU", "UKR" = "FSU", "USA" = "NAM", "WAF" = "AFR", "WEU" = "WEU")
image_regions <- names(message_region_by_image)
message_regions <- unique(message_region_by_image) # this is regions sorted differently
message_region_by_code <- setNames(co2_pop$`MESSAGE-GLOBIOM.REGION`, co2_pop$code)
image_region_by_code <- setNames(co2_pop$IMAGE.REGION, co2_pop$code)
big_region_by_message <- c("LAM" = "lam", "NAM" = "oecd", "EEU" = "oecd", "CPA" = "asia", "AFR" = "maf", "SAS" = "asia", "PAS" = "asia", "PAO" = "oecd", "MEA" = "maf", "FSU" = "ref", "WEU" = "oecd")
# big_region_by_image <- c("BRA" = "lam", "CAN" = "oecd", "CEU" = "oecd", "CHN" = "asia", "EAF" = "maf", "INDIA" = "asia", "INDO" = "asia", "JAP" = "oecd", "KOR" = "asia", "ME" = "maf", "MEX" = "lam", "NAF" = "maf", "OCE" = "oecd", "RCAM" = "lam", "RSAF" = "maf", "RSAM" = "lam", "RSAS" = "asia", "RUS" = "ref", "SAF" = "maf", "SEAS" = "asia", "STAN" = "ref", "TUR" = "oecd", "UKR" = "ref", "USA" = "oecd", "WAF" = "maf", "WEU" = "oecd")
big_region_by_image <- setNames(big_region_by_message[message_region_by_image[image_regions]], image_regions)
big_region_by_code <- setNames(big_region_by_message[message_region_by_code[co2_pop$code]], co2_pop$code)
big_regions <- c("asia", "lam", "maf", "oecd", "ref")
co2_pop.bak <- co2_pop

SSPs <- read.csv("../data/SSPs.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/iam_v2/SSP_IAM_V2_201811.csv.zip
SSPs_countries <- read.csv("../data/SSP_CMIP6.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/cmip6/SSP_CMIP6_201811.csv.zip
# poverty, Gini: https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/additional/NRao_et_al_GiniProjections_2018.zip
# region mapping: https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/cmip6/cmip6_iam_model_region_mapping.xlsx
# In SSPs_countries, IMAGE has only SCENARIO: SSP1-19, SSP1-26; MESSAGE-GLOBIOM: SSP2-45; AIM/CGE: SSP3-70 (Baseline), SSP3-LowNTCF; GCAM4: SSP4-34, SSP4-60; REMIND-MAGPIE: SSP5-34-OS, SSP5-85 (Baseline); WITCH-GLOBIOM is absent
#   while in SSPs, each MODEL has many SCENARIOs
# SSPs_countries is disaggregated by 11-26 regions but has only emissions; SSPs has only 5 regions but many variables incl. pop, gdp ppp, carbon price, emissions
# I think that emissions of SSPs are first defined in CMIP6 (i.e. SSPs_countries) and then passed as inputs to IAMs, which yield consistent bud modified emissions, in SSPs
# /!\ World emissions are not equal to the sum of the 5 R5.2 regions TODO! why? check regions definition
# Nb regions (excluding the 5 R5.2 ones): AIM/CGE: 18 (3 letters), IMAGE: 27, GCAM4: 33, MESSAGE-GLOBIOM: 12, REMIND-MAGPIE: 12 (different), WITCH-GLOBIOM: absent from SSPs_countries

# In ssp, data of the 6 lower-case macro-regions (incl. 'world') is given by IAM results (except for adult_*) while the rest is given by CMIP6 (emissions) or UN median projection (population). *adult_ is estimated by shrinking pop_ by the median UN adult/pop ratio in each region.
#         emissions_pc (resp. emissions_pa) is emissions divided by total (resp. adult) population
vars_SSPs <- c("emissions_pc" = "Emissions|CO2", "fossil_emissions" = "Emissions|CO2|Fossil Fuels and Industry", # Mt CO2/yr => tCO2/yr/pers
               "energy_pc" = "Final Energy", # EJ/yr => GJ/yr/pers
               "gdp_ppp" = "GDP|PPP", # billion US$2005/yr => US$2005/yr, "conso" = "Consumption"
               "pop" = "Population", # million => pers
               "carbon_price" = "Price|Carbon") # US$2005/t CO2
ssp <- list() # 2 min
for (i in unique(SSPs$SCENARIO)) { # unique(SSPs$MODEL)
  ssp[[i]] <- list()
  for (j in c("IMAGE", "MESSAGE-GLOBIOM")) {
    # puts the database in the right form
    ssp[[i]][[j]] <- data.frame(region = c(paste0("iam_", unique(SSPs$REGION)), unique(SSPs_countries$REGION[SSPs_countries$SCENARIO %in% c("SSP1-19", "SSP2-45")]))) # unique(SSPs_countries$REGION)
    # loads GDP data (current one, not projections)
    for (y in years) {
      for (v in names(vars_SSPs)) {
        # print(paste(i, j, v, y))
        if (sum(SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]) > 0 && all(SSPs$REGION[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]] == unique(SSPs$REGION))) {
          ssp[[i]][[j]][[paste0(v, "_", y)]] <- c(SSPs[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v], paste0("X", y)], rep(NA, 42))
        }
      }
      # merge SSPs (aggregated into 5 regions) with SSPs_countries
      if (sum(SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2" & SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & !is.na(SSPs_countries[[paste0("X", y)]])) > 0) {
        temp <- SSPs_countries[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", paste0("X", y)]
        names(temp) <- SSPs_countries$REGION[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2"]
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]][7:48] <- temp[ssp[[i]][[j]]$region][7:48]
      }
      if (y == years[1]) {
        ssp[[i]][[j]]$gdp_ppp_now <- ssp[[i]][[j]]$gdp_2019 <- rep(NA, 48)
        ssp[[i]][[j]]$gdp_ppp_now[match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]]$gdp_ppp_now[pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        # ssp[[i]][[j]]$gdp_2019[match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]]$gdp_2019[pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        for (r in c(big_regions)) ssp[[i]][[j]]$gdp_ppp_now[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] <- sum(ssp[[i]][[j]]$gdp_ppp_now[ssp[[i]][[j]]$region %in% c(image_regions[big_region_by_image == r], message_regions[big_region_by_message == r])], na.rm = T)
        # for (r in c(big_regions)) ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] <- sum(ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region %in% c(image_regions[big_region_by_image == r], message_regions[big_region_by_message == r])], na.rm = T)
      }
      if (paste0("emissions_pc_", y) %in% names(ssp[[i]][[j]])) {
        ctries <- !multi_grepl(c(toupper(big_regions), "World"), ssp[[i]][[j]]$region)
        # Defines population, adult and gdp (using external pop_un coming from co2_pop) for disaggregated countries (it was only given for the 5 regions)
        ssp[[i]][[j]][[paste0("emissions_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("emissions_pc_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("pop_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        # Adjust pop_ of CMIP6 to match the region sum of IAM
        for (r in c(big_regions)) ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
          ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region == paste0("R5.2", toupper(r))] # sum(ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])], na.rm = T)
        # Fill
        ssp[[i]][[j]][[paste0("adult_", y)]] <- NA
        ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)] <- (pop_un[[j]][[paste0("adult_", y)]]/pop_un[[j]][[paste0("pop_", y)]])[match(unique(SSPs$REGION)[-6], pop_un[[j]]$region)] * ssp[[i]][[j]][[paste0("pop_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)]
        ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == "iam_World"] <- sum(ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)], na.rm = T)
        ssp[[i]][[j]][[paste0("adult_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("adult_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region]
        # Adjust to IAM
        for (r in c(big_regions)) ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
          ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / sum(ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])], na.rm = T)
        # ssp[[i]][[j]][[paste0("gdp_ppp_", y)]] <- 1e9 * ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]
        for (r in c(big_regions)) {
          region_r <- ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])
          growth_factor_ry <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))]/sum(((ssp[[i]][[j]]$gdp_ppp_now/ssp[[i]][[j]]$pop_2020) * ssp[[i]][[j]][[paste0("pop_", y)]])[region_r], na.rm = T)
          ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]][region_r] <- growth_factor_ry * (ssp[[i]][[j]]$gdp_ppp_now/ssp[[i]][[j]]$pop_2020)[region_r]
          ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][region_r] <- (ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]] * ssp[[i]][[j]][[paste0("pop_", y)]])[region_r]
        } 
        # ssp[[i]][[j]][[paste0("gdp_", y)]] <- rep(NA, 48)
        # for (r in c(big_regions)) ssp[[i]][[j]][[paste0("gdp_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
        #   ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))]
        if (i %in% c("SSP1-19", "SSP1-26")) { # extends to all MESSAGE regions
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "AFR"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("WAF", "EAF", "SAF", "RSAF")])
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "LAM"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("MEX", "RCAM", "RSAM")])
        }
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]] <- ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("emissions_pa_", y)]] <- ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
        for (var in c("energy_pc_", "gdp_ppp_")) ssp[[i]][[j]][[paste0(var, y)]] <- 1e9 * ssp[[i]][[j]][[paste0(var, y)]]
        # ssp[[i]][[j]][[paste0("gdp_pc_", y)]] <- ssp[[i]][[j]][[paste0("gdp_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        # ssp[[i]][[j]][[paste0("gdp_pa_", y)]] <- ssp[[i]][[j]][[paste0("gdp_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
        ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]] <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("gdp_ppp_pa_", y)]] <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
      }
    }
    ssp[[i]][[j]]$region[1:6] <- c("asia", "lam", "maf", "oecd", "ref", "world")
  }
}
# /!\ ssp1_26$emissions_2030[6] != sum(ssp1_26$emissions_2030[ssp1_26$region %in% image_regions]) (and it's true for every year)
# View(ssp[[i]][[j]])

ssp1_19 <- ssp$`SSP1-19`$IMAGE
ssp1_26 <- ssp$`SSP1-26`$IMAGE
ssp2_26 <- ssp$`SSP2-26`$IMAGE
ssp2_45 <- ssp$`SSP2-45`$IMAGE
ssp2 <- ssp$`SSP2-45`$`MESSAGE-GLOBIOM`
# TODO: SSP5-Baseline is SSP5-8.5 but what is SSP1-Baseline? SSP2-Baseline? SSP2-4.5 
rm(SSPs, SSPs_countries)
rm(ssp)
# j <- "IMAGE"
# i <- unique(SSPs$SCENARIO)[1]
# y <- 2030


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

carbon_price <- revenues_pc <- world_emissions <- world_emissions_pc <- world_population <- revenues_over_gdp <- list()
for (s in ssps) {
  carbon_price[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), years)
  world_emissions[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("emissions_", y)]][d(s)$region == "world"]), years)
  world_population[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("pop_", y)]][d(s)$region == "world"]), years)
  world_emissions_pc[[s]] <- world_emissions[[s]]/world_population[[s]]
  revenues_over_gdp[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"]) / sapply(years, function(y) d(s)[[paste0("gdp_ppp_pc_", y)]][d(s)$region == "world"])
  revenues_pc[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"])
  for (y in years) eval(str2expression(paste0(s, "$gain_pc_", y, " <- (revenues_pc$", s, "['", y, "'] - carbon_price$", s, "['", y, "'] * ", s, "$emissions_pc_", y, ") /12"))) # Average gain in $/month
}



##### Global Energy Assessment #####
# This is the one we use (because SSPs I had are not disaggregated enough). /!\ It's territorial, not footprint.
# TODO try also Kriegler et al. (13)'s LIMITS, not Greenpeace (as it contains population, emissions but no GDP and only until 2050)/
# GEA: Emissions (2°C >50% chance), population and GDP pc until 2100 disaggregated on ~50 countries, but without a price trajectory.
gea_emissions <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_emissions_regions.xlsx")
gea_pop <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_pop_regions.xlsx")
gea_gdp_ppp <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_GDP_PPP_regions.xlsx")
gea_gdp_mer <- read.xlsx("../data/GEA_efficiency/GEA_efficiency_GDP_MER_regions.xlsx")

gea <- list() # GEA model is done with MESSAGE. It contains three additional regions: ASIA (CPA+SAS+PAS), MAF (AFR+MEA), REF (FSU+EEU), OECD90
for (i in c("IMAGE", "GEA")) {
  gea[[i]] <- data.frame(region = unique(gea_pop$Region))
  temp <- ssp2_26 # I make it temp to avoid problems with intersect(image_regions, message_regions) == "WEU"
  for (y in years) {
    for (v in c("gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_", y)]] <- 1e9 * d(paste0("gea_", v))[[as.character(y)]][d(paste0("gea_", v))$Model == i]
    for (v in c("emissions", "pop")) gea[[i]][[paste0(v, "_", y)]] <- 1e6 * d(paste0("gea_", v))[[as.character(y)]][d(paste0("gea_", v))$Model == i]
    for (r in message_regions) temp[[paste0("adult_", y)]][temp$region == r] <- sum(temp[[paste0("adult_", y)]][message_region_by_image[temp$region] == r], na.rm = T)
    gea[[i]][[paste0("adult_", y)]][match.nona(temp$region, gea[[i]]$region)] <- temp[[paste0("adult_", y)]][temp$region %in% gea[[i]]$region]
    # gea[[i]][[paste0("emissions_", y)]] <- gea_emissions[[as.character(y)]][gea_emissions$Model == i]
    # gea[[i]][[paste0("pop_", y)]] <- gea_pop[[as.character(y)]][gea_pop$Model == i]
    # gea[[i]][[paste0("gea_gdp_ppp_", y)]] <- gea_gdp_ppp[[as.character(y)]][gea_gdp_ppp$Model == i]
    # gea[[i]][[paste0("gea_gdp_mer_", y)]] <- gea_gdp_mer[[as.character(y)]][gea_gdp_mer$Model == i]
    for (v in c("emissions", "gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_pc_", y)]] <- gea[[i]][[paste0(v, "_", y)]]/gea[[i]][[paste0("pop_", y)]]
    for (v in c("emissions", "gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_pa_", y)]] <- gea[[i]][[paste0(v, "_", y)]]/gea[[i]][[paste0("adult_", y)]]
    for (v in c("gdp_ppp_pc_", "gdp_mer_pc_")) gea[[i]][paste0(v, "over_mean_", y)] <- gea[[i]][paste0(v, y)]/wtd.mean(gea[[i]][paste0(v, y)], gea[[i]][[paste0("pop_", y)]])
  }
  gea[[i]]$region[gea[[i]]$region == "World"] <- "world"
  gea[[i]] <- gea[[i]][!gea[[i]]$region %in% c("ASIA", "MAF", "REF", "OECD90", "North", "South"), ]
  for (y in years) for (v in c("adult")) gea[[i]][[paste0(v, "_", y)]][gea[[i]]$region == "world"] <- sum(gea[[i]][[paste0(v, "_", y)]][gea[[i]]$region != "world"], na.rm = T) # , "emissions", "pop", "gdp_ppp", "gdp_mer"
}
world_emissions_pc$gea_gea <- setNames(gea$GEA[gea$GEA$region == "world", grepl("emissions_pc", names(gea$GEA))], years)
world_emissions_pc$gea_image <- setNames(gea$IMAGE[gea$IMAGE$region == "world", grepl("emissions_pc", names(gea$IMAGE))], years)
# View(rbind("gea_gea" = world_emissions_pc$gea_gea, "gea_image" = world_emissions_pc$gea_image, "SSP1-2.6" = world_emissions_pc$ssp1_26, "SSP2-2.6" = world_emissions_pc$ssp2_26, "SSP2" = world_emissions_pc$ssp2))
# GEA scenario is closest to ssp1_26 (higher emissions than ssp2_26 before 2050, larger negative emissions after), gea_image has almost no negative emissions => use gea_gea
# I impute the price trajectory of ssp2_26 (at least three times higher than ssp1_26) to be conservative although emissions_pc are much closer to ssp1_26
carbon_price$gea_gea <- carbon_price$ssp2_26

world_population$gea_gea <- setNames(gea$GEA[gea$GEA$region == "world", grepl("pop_", names(gea$GEA))], years)
world_population$gea_image <- setNames(gea$IMAGE[gea$IMAGE$region == "world", grepl("pop_", names(gea$IMAGE))], years)
world_emissions$gea_gea <- setNames(world_emissions_pc$gea_gea * world_population$gea_gea, years)
world_emissions$gea_image <- setNames(world_emissions_pc$gea_image * world_population$gea_image, years)
# View(rbind("gea_gea" = world_population$gea_gea, "gea_image" = world_population$gea_image, "SSP1-2.6" = world_population$ssp1_26, "SSP2-2.6" = world_population$ssp2_26, "SSP2" = world_population$ssp2))
# World population closest to that of ssp2_26 (maximum 1% difference before 2070) so we can take adult_ from ssp2_26 one as first approximation. TODO: improve the imputation of adult_ in GEA using the commented lines below
# ssp[[i]][[j]][[paste0("adult_", y)]] <- NA
# ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)] <- (pop_un[[j]][[paste0("adult_", y)]]/pop_un[[j]][[paste0("pop_", y)]])[match(unique(SSPs$REGION)[-6], pop_un[[j]]$region)] * ssp[[i]][[j]][[paste0("pop_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)]
# ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == "iam_World"] <- sum(ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)], na.rm = T)
# ssp[[i]][[j]][[paste0("adult_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("adult_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region]
# # Adjust to IAM
# for (r in c(big_regions)) ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
#   ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / sum(ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])], na.rm = T)
gea_gea <- gea$GEA
# for (y in years) gea_gea[[paste0("gdp_", y)]] <- gea_gea[[paste0("gdp_ppp_", y)]] # by default it's _mer
# co2_pop <- create_var_ssp(gea_gea)


##### GDRs / CERc #####
# Two methods to compare GDRs to equal pc (with the common limitation that we have to use a point estimate in 2030 as they don't provide figures beyond that date):
# 1. Use "our" gea_gea figures, and redo the computations of GDR allocation with the updated world_emissions$gea_gea["2030"] = 30.9Gt instead of their 1°5 29.4Gt (our closest figure is ssp2_26: 28.7Gt), LED 24.4Gt (our closest is ssp2_26: 28.7Gt in 2030) or their 2DS 35.95Gt (closest gea_image: 35Gt)
# 2. Use their LED 1.5° scenario (which is the NGOs baseline), gdp and pop figures for 2030 and compute our net gains in their setting. The issue with this method is that their LED 1.5°C trajectory is only disaggregated into North vs. South (and not sure it results from a uniform global price) so I don't know how to impute emissions per country.
# 3. Use "our" gea_gea figures for emissions, and theirs for gdp and pop. It shouldn't change much from ours, so let's simply do method 1 and use data from their scenario 1.5°C, closest to our gea_gea.
cerc_1d5 <- read.xlsx("../data/cerc_1D5.xlsx")
cerc_1d5 <- cerc_1d5[cerc_1d5$year == 2030, c("code", "emissions_baseline", "rci", "allocation_MtCO2")] # , "pop_mln", "gdp_blnUSDMER", "gdp_blnUSDPPP"
names(cerc_1d5)[!names(cerc_1d5) %in% c("code", "country", "year")] <- paste0(names(cerc_1d5)[!names(cerc_1d5) %in% c("code", "country", "year")], "_2030")
cerc_1d5$emissions_baseline_2030 <- 1e6 * cerc_1d5$emissions_baseline_2030
world_emissions_reduction_2030_cerc <- ((cerc_1d5$emissions_baseline_2030 - 1e6 * cerc_1d5$allocation_MtCO2_2030)/cerc_1d5$rci_2030)[1] # It's a constant vector
world_emissions_baseline_2030_cerc <- cerc_1d5$emissions_baseline_2030[cerc_1d5$code == "WORLD"]
world_emissions_1d5_2030_cerc <- world_emissions_baseline_2030_cerc - world_emissions_reduction_2030_cerc
world_emissions_1d5_2030 <- world_emissions$gea_gea[["2030"]]
world_emissions_reduction_2030 <- world_emissions_baseline_2030_cerc - world_emissions_1d5_2030
co2_pop <- merge(cerc_1d5[, 1:3], co2_pop) # This discards Taiwan, which is in cerc_1d5
co2_pop$gdr_pa_2030_cerc <- (co2_pop$emissions_baseline_2030 - co2_pop$rci_2030 * world_emissions_reduction_2030_cerc)/co2_pop$adult_2030
co2_pop$gdr_pa_2030 <- (co2_pop$emissions_baseline_2030 - co2_pop$rci_2030 * world_emissions_reduction_2030)/co2_pop$adult_2030
# wtd.mean(co2_pop$gdr_pa_2030_cerc, co2_pop$adult_2030)

# TODO!! instead of SSPs/GEA, use AR6/ country-disaggregated data. Also, I should look for a model/scenario with a uniform carbon price and carbon footprints (pb: Ctrl+F "footprint", "embodied", "consumption-based" returns nothing).

# Disaggregate by country emissions, gdp, gdp_pc
# On top of partition in R5_region, IMAGE (ssp1_19, ssp1_26) has 26 regions (co2_pop$IMAGE.REGION), MESSAGE (ssp2) has 11 MESSAGE-GLOBIOM.REGION. No dissagregated data for ssp2_26, ssp2_45. 
# IMAGE regions are not refinement of MESSAGE's: North Korea, Vietnam (CPA in MESSAGE instead of PAS, SEAS in IMAGE), Non-Australia Oceania (PAS instead of OCE) or rather Australia, NZ (PAO (which is these 2 + Japan) instead of OCE), Cyprus (WEU instead of CEU), Sudan, South Sudan (MEA instead of EAF)

total_revenues <- average_revenues <- average_revenues_bis <- basic_income <- basic_income_adj <- list()
create_var_ssp <- function(ssp, df = co2_pop, base_year = 2019, CC_convergence = 2040, discount = .04, opt_out_threshold = 1.5, full_part_threshold = 2) { # message is only for ssp2 , region = message_region_by_code
  ssp_name <- deparse(substitute(ssp))
  if (grepl("ssp1", ssp_name)) model <- "IMAGE"
  else if (ssp_name == "ssp2" | grepl("gea", ssp_name)) model <- "MESSAGE"
  else model <- "big"
  # Dirty fix for unrealistically high projections of GDP pc for middle-income African countries: we assign them to China region, which has a comparable GDP pc, so the projection of GDP pc are more credible
  # TODO? Make our own projections for all countries, grouping countries based on GDP pc and carbon footprint rather than geography, and deriving projections by group from SSPs or GEA macro-regions.
  recoded_countries <- c("BWA", "GAB", "GNQ", "ZAF", "NAM")
  if (grepl("gea", ssp_name)) message_region_by_code_original <- message_region_by_code
  if (grepl("gea", ssp_name)) message_region_by_code[recoded_countries] <- "MEA" # c("Botswana", "Gabon", "Equatorial Guinea", "South Africa", "Namibia)
  region <- if (model == "big") big_region_by_code else { if (model == "IMAGE") image_region_by_code else message_region_by_code }
  regions <- if (model == "big") big_regions else { if (model == "IMAGE") image_regions else message_regions }
  total_revenues[[ssp_name]] <- average_revenues[[ssp_name]] <- average_revenues_bis[[ssp_name]] <- basic_income[[ssp_name]] <- basic_income_adj[[ssp_name]] <- c()
  
  for (y in years) {
    yr <- as.character(y)
    for (v in paste0(c("pop_", "adult_", "emissions_", "gdp_"), y)) if (!v %in% names(df)) df[[v]] <- NA
    if (!paste0("gdp_", y) %in% names(ssp)) {
      if (paste0("gdp_mer_", y) %in% names(ssp)) {
        ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_mer_", y)]] 
        df$gdp_pc_base_year <- df$gdp_pc_2019 # manage missing values (Venezuela, Yemen, South Sudan, North Korea, Eritrea, fix Western Sahara)
      } else { 
        ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_ppp_", y)]]
        df$gdp_pc_base_year <- df$GDPpcPPP  # TODO: manage missing values (Saudi Arabia, Afghanistan, New Zealand, Cambodia...)
      }
    } else df$gdp_pc_base_year <- df$GDPpcPPP
    if (grepl("gea", ssp_name)) {
      for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "MEA"] <- ssp[[v]][ssp$region == "MEA"] * (1 + sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
      for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "AFR"] <- ssp[[v]][ssp$region == "AFR"] * (1 - sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
      for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "MEA"] <- ssp[[paste0(v, y)]][ssp$region == "MEA"] * (1 + sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
      for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "AFR"] <- ssp[[paste0(v, y)]][ssp$region == "AFR"] * (1 - sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
    }
    for (r in regions) {
      region_r <- region[df$code] == r # df$code %in% region[r] 
      # /!\ The line below overwrites UN's pop projections
      for (v in paste0(c("pop_", "adult_"), y)) df[[v]][region_r] <- df[[v]][region_r] * ssp[[v]][ssp$region == r] / sum(df[[v]][region_r], na.rm = T)
      reduction_factor_ry <- ssp[[paste0("emissions_", y)]][ssp$region == r]/sum((df[[paste0("emissions_pa_", base_year)]] * df[[paste0("adult_", y)]])[region_r])
      df[[paste0("emissions_pa_", y)]][region_r] <- reduction_factor_ry * df[[paste0("emissions_pa_", base_year)]][region_r] # df$emissions_2019[region_r] * ssp[[paste0("emissions_", y)]][ssp$region == r] / sum(df$emissions_2019[region_r], na.rm = T)
      df[[paste0("emissions_", y)]][region_r] <- (df[[paste0("emissions_pa_", y)]] * df[[paste0("adult_", y)]])[region_r]
      growth_factor_ry <- ssp[[paste0("gdp_", y)]][ssp$region == r]/sum((df$gdp_pc_base_year * df[[paste0("pop_", y)]])[region_r], na.rm = T)
      df[[paste0("gdp_pc_", y)]][region_r] <- growth_factor_ry * df$gdp_pc_base_year[region_r]
      df[[paste0("gdp_", y)]][region_r] <- (df[[paste0("gdp_pc_", y)]] * df[[paste0("pop_", y)]])[region_r] # df$gdp_ppp_now[region_r] * ssp[[paste0("gdp_ppp_", y)]][ssp$region == r] / sum(df$gdp_ppp_now[region_r], na.rm = T)
    } 
    df[[paste0("gdp_pc_over_mean_", y)]] <- df[[paste0("gdp_pc_", y)]]/wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]])
    df[[paste0("gdp_pa_", y)]] <- df[[paste0("gdp_", y)]]/df[[paste0("adult_", y)]]
    df[[paste0("emissions_pc_", y)]] <- df[[paste0("emissions_", y)]]/df[[paste0("pop_", y)]]
    # Unadjusted mean gain pa
    if (y > 2015) {
      df[[paste0("revenues_pa_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pa_", y)]]) # /12
      total_revenues[[ssp_name]][yr] <- carbon_price[[ssp_name]][yr] * sum(df[[paste0("emissions_", y)]], na.rm = T) # ssp[[paste0("emissions_", y)]][ssp$region == "world"]
      if (total_revenues[[ssp_name]][yr] < 0) df[[paste0("revenues_pa_", y)]] <- 0
      
      # GCS
      df[[paste0("gain_pa_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0("adult_", y)]], na.rm = T) - df[[paste0("revenues_pa_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
      # Adjusted for opt out
      df[[paste0("optout_right_", y)]] <- (full_part_threshold - pmax(opt_out_threshold, pmin(full_part_threshold, df[[paste0("gdp_pc_", y)]] / wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]]))))/(full_part_threshold - opt_out_threshold)
      temp <- rep(T, nrow(df)) # average_revenues is average emissions_pa * carbon_price while basic_income is adjusted for participation_rate due to opt-out and anti-regressive mechanism
      average_revenues_bis[[ssp_name]][yr] <- total_revenues[[ssp_name]][yr]/ssp[[paste0("adult_", y)]][ssp$region == "world"] # a bit different from average_revenues because in ssp, world emissions is not the sum of regional emissions (be it image_ or big_ regions)
      basic_income[[ssp_name]][yr] <- average_revenues[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("adult_", y)]])
      df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > basic_income[[ssp_name]][yr])
      df[[paste0("participation_rate_", y)]] <- 1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]
      while (any(temp != df[[paste0("large_footprint_", y)]])) {
        temp <- df[[paste0("large_footprint_", y)]]
        basic_income[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
        df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > basic_income[[ssp_name]][yr])
        df[[paste0("participation_rate_", y)]] <- 1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]
        df[[paste0("gain_optout_", y)]] <- df[[paste0("participation_rate_", y)]] * (basic_income[[ssp_name]][yr] - df[[paste0("revenues_pa_", y)]])
      } 
      # Adjusted to avoid high-income receiving money. Pb: GDP in PPP of Europe is not more than twice the world average 2050-2070.
      # /!\ Pb, 2070 GDP pc PPP of China is larger that Western Europe in View(ssp1_26[,c("region", "gdp_pc_2020", "gdp_pc_2070")]) co2_pop$gdp_pc_2070[co2_pop$country %in% c("China", "Spain", "France", "Nigeria", "Namibia")]
      # To estimate future emissions and GDP, I make the assumption that emissions_pc/GDPpc evolve in the same way in all big regions. Pb: this assumption is at odd with SSP1, where GDPpc converge across regions. 
      # => Either I should drop the country-by-country analysis, or I should find better projections of GDP.
      y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]])
      e_bar <- wtd.mean(df[[paste0("emissions_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
      lambda <- pmax(0, pmin(1, (2.2*y_bar - df[[paste0("gdp_pc_", y)]])/((2.2-2)*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
      lambda[is.na(lambda)] <- 1
      df[[paste0("share_basic_income_", y)]] <- df[[paste0("participation_rate_", y)]] * (lambda + pmin(1, df[[paste0("emissions_pa_", y)]]/(1.3*e_bar))*(1-lambda))
      df[[paste0("gain_adj_", y)]][df[[paste0("emissions_pa_", y)]] < 1.3*e_bar] <- (basic_income[[ssp_name]][yr] * df[[paste0("share_basic_income_", y)]] - 
              df[[paste0("participation_rate_", y)]] * df[[paste0("revenues_pa_", y)]])[df[[paste0("emissions_pa_", y)]] < 1.3*e_bar]
      basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + wtd.mean(df[[paste0("participation_rate_", y)]] - df[[paste0("share_basic_income_", y)]], df[[paste0("adult_", y)]]))
      df[[paste0("gain_adj_", y)]][lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[[ssp_name]][yr] - df[[paste0("revenues_pa_", y)]]))[lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar]
      df[[paste0("gain_adj_over_gdp_", y)]] <- df[[paste0("gain_adj_", y)]]/df[[paste0("gdp_pc_", y)]]
      df[[paste0("gain_over_gdp_", y)]] <- df[[paste0("gain_pa_", y)]]/df[[paste0("gdp_pc_", y)]]
      
      # C&C: define climate debt/credit until convergence date TODO!
      
    }        
  }
  
  compute_npv <- function(var = "gain_pa_", discount_rate = discount, data = df) {
    rate <- (1+discount_rate)^10
    return(rowSums(sapply(2:10, function(i) { return(10*data[[paste0(var, 2000+10*i)]]/rate^(i-2)) })))
  }
  # GDR: find emissions allocations on website and allocate total_revenues[[ssp_name]][yr]. They go only until 2030. Either I recover the GDRs from them (or their code) and apply them here, or I add the per-capita allocation to their code.
  df$gain_gdr_2030 <- (carbon_price[[ssp_name]][["2030"]] * df$gdr_pa_2030  - df$revenues_pa_2030)
  df$gain_gdr_over_gdp_2030 <- df$gain_gdr_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_2030 <- df$gain_gdr_2030 - df$gain_pa_2030
  df$diff_gain_gdr_gcs_over_gdp_2030 <- df$diff_gain_gdr_gcs_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_adj_2030 <- df$gain_gdr_2030 - df$gain_adj_2030
  df$diff_gain_gdr_gcs_adj_over_gdp_2030 <- df$diff_gain_gdr_gcs_adj_2030/df$gdp_pa_2030
  
  df$npv_pa_gcs <- compute_npv("gain_pa_", discount)
  df$npv_pa_gcs_adj <- compute_npv("gain_adj_", discount)
  df$npv_over_gdp_gcs <- df$npv_pa_gcs/compute_npv("gdp_pa_", discount) # this formula corresponds to the % loss in consumption computed in Balanced Growth Equivalent of Stern et al. (07)
  df$npv_over_gdp_gcs_adj <- df$npv_pa_gcs_adj/compute_npv("gdp_pa_", discount)
  
  total_revenues[[ssp_name]] <<- total_revenues[[ssp_name]]
  average_revenues[[ssp_name]] <<- average_revenues[[ssp_name]]
  average_revenues_bis[[ssp_name]] <<- average_revenues_bis[[ssp_name]] 
  basic_income[[ssp_name]] <<- basic_income[[ssp_name]]
  basic_income_adj[[ssp_name]] <<- basic_income_adj[[ssp_name]]
  return(df)
}
# Disaggregated data not available for ssp2_19 or ssp2_26. 
co2_pop <- create_var_ssp(gea_gea, opt_out_threshold = 1.5)
co2_pop <- create_var_ssp(ssp2_26)
View(co2_pop[,grepl("2040", names(co2_pop))])
View(co2_pop[,grepl("over_mean|country|pop_2020", names(co2_pop))])
View(gea_gea[,grepl("over_mean|region|pop_2020", names(gea_gea))])
for (r in message_regions) co2_pop$gdp_over_region[message_region_by_code[co2_pop$code] == r] <- co2_pop$gdp_pc_2020[message_region_by_code[co2_pop$code] == r]/gea_gea$gdp_mer_pc_2020[gea_gea$region == r]
# threshold gdp_pc that keep following countries full opt out: 1.1: China, (Turkey), 1.3: Malaysia, 1.5: (Mexico), Botswana, 1.53: Russia, Gabon, 1.6: Lybia, 1.65: Kazakhstan, 1.75: South Africa, 1.83: Argentina, Equatorial Guinea
# threshold gdp_pc that keep following countries full opt out: 
# To manage co2_pop$country %in% c("Botswana", "China", "Gabon", "Equatorial Guinea", "Namibia", "South Africa"), I could attach them to SAS (they are 35% above its GDP average vs. +75% for AFR)
# TODO manage small countries (Eswatini, Montenegro, Western Sahara, Antillas, Taiwan, Cape Verde) for which there is a bug
# Define rights to emit i.e. allocation key, for equal pc C&C and GDR
# co2_pop$gain_gdr_over_gdp_2030[co2_pop$country == "China"] # 7%

# TODO Historical resp: NPV equal pc + climate debt until today. Use function cumulative_emissions 

##### GCS Policy brief #####
# Total revenues in % of global GDP
total_revenues$gea_gea/sapply(years[3:11], function(y) sum(co2_pop[[paste0("gdp_", y)]], na.rm = T))
# International transfers in % of global GDP
setNames(sapply(years[3:11], function(y) sum(co2_pop[[paste0("gain_adj_", y)]] * co2_pop[[paste0("adult_", y)]] * (co2_pop[[paste0("gain_adj_", y)]] > 0), na.rm = T)/sum(co2_pop[[paste0("gdp_", y)]], na.rm = T)), years[3:11])
carbon_price$ssp2_26
basic_income_adj$gea_gea/12
# Median gain in 2030 in complementary survey countries
setNames(0.9*co2_pop$gain_adj_2030[match.nona(countries_names, co2_pop$country)]/12, countries_names) # vs. 12 25 5 23 86 in the survey. Different only in DE, ES. Why not higher with +50% price (and +20% emissions)? Because in this co2 data, carbon footprint is lower by a comparable amount. E.g. France's 2020 is 6.7 setNames(co2_pop$emissions_pa_2020[match.nona(countries_names, co2_pop$country)], countries_names)
# Share of emissions of countries which gain from the (adjusted) GCS
co2_pop$share_territorial_2019[co2_pop$code == "CHN"] # 30.2%
co2_pop$share_territorial_2019[co2_pop$code == "USA"] # 14.8%
co2_pop$share_territorial_2019[co2_pop$code == "IND"] # 7.4%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% EU28_countries]) # 9.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("CHN", "IND", "USA", EU28_countries)]) # 61.7%
sum(co2_pop$share_territorial_2019[co2_pop$npv_pa_gcs_adj > 0], na.rm = T) # 22.7% of global emissions in countries with gain > 0
sum(co2_pop$share_territorial_2019[co2_pop$npv_pa_gcs_adj == 0], na.rm = T) # 57.8% of global emissions in countries with gain >= 0
sum(co2_pop$share_territorial_2019[(co2_pop$code %in% EU28_countries & co2_pop$code != "GBR") | co2_pop$npv_pa_gcs_adj >= 0], na.rm = T) # 65%
# TODO! redo the computations with partial participation (e.g. without the U.S.)
cor(co2_pop$emissions_pa_2019, co2_pop$gdp_pc_2019, use = "complete.obs") # .69
average_revenues$gea_gea/12
basic_income_adj$gea_gea/12


# GCS_adj_trajectories
mar <- par()$mar
par(mar = c(2.1, 4.1, 0.1, 4.1))
plot(years[3:9], basic_income_adj$gea_gea[1:7]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(-5, 53))
lines(years[3:9], world_emissions$gea_gea[3:9]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(years[3:9], carbon_price$gea_gea[3:9], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5) 
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
legend("topright", legend = c("CO2 emissions", "Basic income", "Carbon price (right axis)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))




##### Plots #####
# Net gains are closer to zero than for Stern-Stiglitz due to lower carbon_price$ssp1_26. In PPP, China is not below average GDPpc, hence its (small) cost.
for (y in years[3:9]) plot_world_map(paste0("gain_adj_", y), breaks = c(-Inf, -1000, -500, -200, -100, -1e-10, 0, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, 0, 0, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gain per adult\nfrom the GCS\nin ", y, " (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[3:9]) plot_world_map(paste0("gain_over_gdp_", y), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gain per adult\nfrom the GCS\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[3:9]) plot_world_map(paste0("gain_adj_over_gdp_", y), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gain per adult\nfrom the GCS\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years) plot_world_map(paste0("emissions_pc_", y), breaks = c(-Inf, 0, 1, 2, 4, 5, 7, 10, 15, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, rev_color = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, 1, 2, 4, 5, 7, 10, 15, Inf), sep = " to ", return = "levels")), 
               legend = paste0("tCO2 emissions\nper capita\nin ", y, "\nin GEA Efficiency"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("gain_gdr_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gain per adult\nfrom GDRs\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("gain_gdr_over_gdp_2030", breaks = c(-Inf, -.04, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.04, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gain per adult\nfrom GDRs\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_adj_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngain per adult:\nGDRs - GCS\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_adj_over_gdp_2030", breaks = c(-Inf, -.3, -.1, -.05, -.02, -1e-10, 0, .005, .01, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.3, -.1, -.05, -.02, 0, 0, .005, .01, .05, .1, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngain per adult:\nGDRs - GCS\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngain per adult:\nGDRs - GCS\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_over_gdp_2030", breaks = c(-Inf, -.3, -.1, -.05, -.02, -1e-10, 0, .005, .01, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.3, -.1, -.05, -.02, 0, 0, .005, .01, .05, .1, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\nnet gain:\nGDRs - equal pc\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_pa_gcs", breaks = c(-Inf, -30000, -10000, -1000, -1e-10, 0, 1000, 5000, 10000, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -30000, -10000, -1000, 0, 0, 1000, 5000, 10000, Inf), sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gain per adult\nfrom the GCS\n(with 4% discount rate)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_pa_gcs_adj", breaks = c(-Inf, 30000, -10000, -1000, -1e-10, 0, 1000, 5000, 10000, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -30000, -10000, -1000, 0, 0, 1000, 5000, 10000, Inf), sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gain per adult\nfrom the adjusted GCS\n(with 4% discount rate)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs", breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf # -.003, -.001, -.0005, 0, .0005, .01, .02
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gain per adult\nfrom the GCS (in % of GDP)\n(with 4% discount rate)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs_adj", breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
               legend = "Net present value\nof gain per adult\n(in % of GDP)\nfrom the adjusted GCS\n(with 4% discount rate)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 


# Old plots SSP
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
