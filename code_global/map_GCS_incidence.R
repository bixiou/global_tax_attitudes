# TODO? share of winners per country: Ivanova & Wood (20) show that 2020 World average of 6t pc is at ~50 percentile in FR, ~65 in ES, ~50 in UK, ~20 in DE; Fremstad & Paul (19) show it's at ~20p in the U.S.
# TODO: more accurate assumption/computations (e.g. based on NDCs)
# TODO! ajuster PPP => MER moi-même

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
iso2to3 <- setNames(pop$code, pop$ISO2_code)
iso2to3["NA"] <- "NAM"


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
co2_pop$country_map[co2_pop$country == "Eswatini"] <- "Swaziland"
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

co2_pop$mean_gain_over_gdp_2019 <- 100*12*co2_pop$mean_gain_2019/co2_pop$gdp_pc_2019 # TODO mean_gain is computed per adult, not p.c. => abs gain/loss are lower than shown, especially in young countries
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

pg7 <- read.csv("../data/poverty_gap_6-85.csv") # Poverty gap at $6.85 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.UMIC.GP March 1, 2023. World average: 21% i.e. (less than - bc PPP) 4.2T$ = 6.85*365*8e9*.21
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

# /!\ Big issue with this allocation: it doesn't preserve GDP rankings, cf. plot below.
key_gap_gdp <- function(return_var = "gap", # gap, global_share, gdp_share, rev_pc, new_gni
                        return_type = "list", # var, seq, function, list
                        poverty_line = 4,  # 2, 4, 7 $/day
                        max_reg = phase_out_start, # Inf, phase_out_start
                        type_reg = "quadratic", # piecewise linear quadratic
                        cap_fit = Inf, # Unfortunetaly, cap_fit doesn't suffice to preserve GDP rankings
                        PPP = F, monthly = T, df = pg, global_revenues = 8.8e+11, min_pop = 30e6, # 1: 4.08e+11, 1-2-3: 6.87e+11, default: 1-3-5 0.926e+12
                        phase_out_start = NULL, phase_out_end = 2*phase_out_start) { # wtd.mean(pg$GDPpcPPP, pg$pop_2019) wtd.mean(pg$gdp_pc_2019, pg$pop_2019)
  pg <- df
  pg$gap <- pg[[paste0("pg", poverty_line)]]
  pg$gdp <- if (PPP) pg$GDPpcPPP else pg$gdp_pc_2019
  pg$log_gdp <- log10(pg$gdp)
  if (is.null(phase_out_start)) phase_out_start <- wtd.mean(pg$gdp, pg$pop_2019) #  + I(log10(gdp)^3)
  if (type_reg == "piecewise") {
    reg_gap_gdp_log <- lm(log10(gap) ~ log_gdp, data = pg, weights = pop_2023, subset = gap > 0 & gdp < max_reg)
    reg_gap_gdp_log <- segmented(reg_gap_gdp_log, seg.Z = ~log_gdp, weights = pop_2023)
  } else if (type_reg == "quadratic") {
    reg_gap_gdp_log <- lm(log10(gap) ~ log10(gdp) + I(log10(gdp)^2), data = pg, weights = pop_2023, subset = gap > 0 & gdp < max_reg)
  } else if (type_reg == "linear") {
    reg_gap_gdp_log <- lm(log10(gap) ~ log10(gdp), data = pg, weights = pop_2023, subset = gap > 0 & gdp < max_reg)
  }
  # pg$predicted_gap[as.numeric(names(reg_gap_gdp_log$fitted.values))] <- 10^reg_gap_gdp_log$fitted.values
  print(paste("R^2: ", round(summary(reg_gap_gdp_log)$r.squared, 3)))

  key_gap <- function(gdp) {
    predicted <- data.frame(log_gdp = pg$log_gdp, gdp = pg$gdp, pop_2023 = pg$pop_2023) # [pg$gap > 0 & pg$gdp < max_reg]
    predicted$gap <- 10^predict(reg_gap_gdp_log, newdata = predicted)
    predicted_gap_phase_out_start <- predict(reg_gap_gdp_log, newdata = data.frame(log_gdp = log10(phase_out_start), gdp = phase_out_start))
    predicted$gap[!is.na(predicted$gdp) & predicted$gdp > phase_out_start] <- 10^(-1 + (predicted_gap_phase_out_start + 1) * (log10(phase_out_end) - log10(predicted$gdp[!is.na(predicted$gdp) & predicted$gdp > phase_out_start]))/(log10(phase_out_end) - log10(phase_out_start)))
    predicted$gap <- pmin(cap_fit, predicted$gap)
    predicted$gap[predicted$gdp > phase_out_end] <- 0
    return(predicted$gap)
  }

  global_share <- function(gdp, pop) {
    share <- key_gap(gdp) * pop
    return(100*share/sum(share, na.rm = T))
  }

  global_share_pc <- function(gdp, pop) return(global_share(gdp, pop) / (100*pop/sum(pop, na.rm = T)))

  gdp_share <- function(gdp, pop, revenues = global_revenues) {
    return(global_share(gdp, pop) * revenues / (gdp*pop))
  }

  rev_pc <- function(gdp, pop, revenues = global_revenues) return(global_share(gdp, pop) * revenues / pop / 100 / ifelse(monthly, 12, 1))

  new_gni <- function(gdp, pop, revenues = global_revenues) return(gdp / ifelse(monthly, 12, 1) + rev_pc(gdp, pop, revenues = global_revenues))

  if (any(new_gni(pg$gdp, pg$pop_2023)[order(pg$gdp)] != sort(new_gni(pg$gdp, pg$pop_2023), na.last = T), na.rm = T)) warning("new_gni is not increasing")
  if (any(rev_pc(pg$gdp, pg$pop_2023)[order(pg$gdp)] != sort(rev_pc(pg$gdp, pg$pop_2023), decreasing = T, na.last = T), na.rm = T)) warning("new_gni is not decreasing")
  
  if (return_type == "var") {
    if (return_var == "gap") return(key_gap(pg$gdp))
    if (return_var == "global_share") return(global_share(pg$gdp, pg$pop_2023))
    if (return_var == "global_share_pc") return(global_share_pc(pg$gdp, pg$pop_2023))
    if (return_var == "gdp_share") return(gdp_share(pg$gdp_pc_2019, pg$pop_2023))
    if (return_var == "rev_pc") return(rev_pc(pg$gdp, pg$pop_2023))
    if (return_var == "new_gni") return(new_gni(pg$gdp, pg$pop_2023))
    if (return_var == "gdp") return(pg$gdp / ifelse(monthly, 12, 1))
  } else if (return_type == "function") {
    if (return_var == "gap") return(key_gap)
    if (return_var == "global_share") return(global_share)
    if (return_var == "global_share") return(global_share_pc)
    if (return_var == "gdp_share") return(gdp_share)
    if (return_var == "rev_pc") return(rev_pc)
    if (return_var == "new_gni") return(new_gni)
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
    if (return_var == "new_gni") return(sort(setNames(new_gni(pg$gdp, pg$pop_2023)[pg$pop_2023 > min_pop], pg$country[pg$pop_2023 > min_pop]), decreasing = T))
    if (return_var == "gdp") return(sort(setNames(pg$gdp[pg$pop_2023 > min_pop] / ifelse(monthly, 12, 1), pg$country[pg$pop_2023 > min_pop]), decreasing = T))
  }
}
plot(pg$gdp_pc_2019, key_gap_gdp("new_gni", return_type = 'var', monthly = F, poverty_line = 4), xlim = c(0,3000), ylim = c(0,3000), xlab = "GDP pc 2019", ylab = "GNI pc after transfers", lwd = 2)
lines(c(0,1e5), c(0,1e5), type = 'l') + grid() # /!\ Big issue with this allocation: it doesn't preserve GDP rankings.
plot(pg$gdp_pc_2019, key_gap_gdp("rev_pc", return_type = 'var', monthly = F, poverty_line = 4), xlim = c(0,3000), ylim = c(0,1000), xlab = "GDP pc 2019", ylab = "GNI pc after transfers", lwd = 2)
# /!\ Two conditions to check to select an allocation key: rev_pc(gdp) must be decreasing and new_gni(gdp) increasing.

# These lines are the ones used
pg$predicted_gap <- key_gap_gdp(poverty_line = 4, PPP = F, phase_out_start = wtd.mean(pg$gdp_pc_2019, pg$pop_2019), return_var = "gap", return_type = "var")
mean_gdp_pc <- wtd.mean(pg$gdp_pc_2019, pg$pop_2019)
qplot(log10(gdp_pc_2019), log10(pg4), data = pg, size = pop_2019, xlab = "log10 of 2019 GDP per capita (constant 2015 $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  # geom_smooth(method = "lm",  mapping = aes(weight = pop_2019 * (gdp_pc_2019 < mean_gdp_pc)), color = "black", show.legend = FALSE, se = F) +
  geom_line(aes(y = log10(predicted_gap)), size = 2, color = "red", show.legend = FALSE) + theme_bw()
# save_plot(filename = "poverty_gap_gdp", folder = "../figures/policies/", format = "png", width = 538, height = 413) # Renders much better by hand

table_pg <- key_gap_gdp(return_var = "gdp_share", return_type = "list")
(table_pg <- cbind("gdp_share" = table_pg, "rev_pc" = key_gap_gdp("rev_pc")[names(table_pg)], "global_share_pc" = key_gap_gdp("global_share_pc")[names(table_pg)], "global_share" = key_gap_gdp("global_share")[names(table_pg)]))
row.names(table_pg)[row.names(table_pg) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
cat(paste(kbl(table_pg[table_pg[,1] > 0.07,], "latex", caption = "Allocation of the global wealth tax revenues.", position = "b", escape = F, booktabs = T, digits = c(2, 0, 2, 2), linesep = rep("", nrow(table_pg)-1), longtable = T, label = "allocation",
              col.names = c("\\makecell{Revenues\\\\over GDP\\\\(in percent)}", "\\makecell{Revenues\\\\per capita\\\\(in \\$ per month)}", "\\makecell{Revenues per capita\\\\over average\\\\revenues p.c.}", "\\makecell{Global\\\\share of\\\\revenues}")), collapse="\n"), file = "../tables/allocation.tex") 

# Linear line from demogrant to phase_out (where GDPpc after = before)
key_gdp <- function(return_var = "rev_pc", # gap, global_share, gdp_share, rev_pc
                    return_type = "list", # var,function, list, params
                    PPP = F, monthly = T, df = pg, global_revenues = 8.8e+11, min_pop = 30e6, country = NULL, # 1: 4.08e+11, 1-2-3: 6.87e+11, default: 1-3-5
                    phase_out = "mean_gdp_pc") { # "mean_gdp_pc" 0.5*wtd.mean(pg$gdp_pc_2019, pg$pop_2019) 4045 $ GNI pc (nominal: Atlas method, cf. Vaggi 17) is the threshold between lower and upper MI
  pg <- df
  pg$gdp <- if (PPP) pg$GDPpcPPP else pg$gdp_pc_2019
  pg$pop <- pg$pop_2019 # TODO? use pop_2023?
  if (phase_out == "mean_gdp_pc") phase_out <- wtd.mean(pg$gdp, pg$pop)
  gdp_below_phase_out <- sum(pg$gdp * pg$pop * (pg$gdp < phase_out), na.rm = T)
  pop_below_phase_out <- sum(pg$pop * (pg$gdp < phase_out), na.rm = T)
  gap_to_phase_out <- pop_below_phase_out * phase_out - gdp_below_phase_out
  
  a <- 1 - global_revenues/gap_to_phase_out
  m <- global_revenues * phase_out/gap_to_phase_out
  # Other methods than linear line (e.g. power law line: y = m + b*a^x or regressions: polynomial, loess, or piecewise linear continuous) either require extra parameters (power, loess) or do not necessarily fulfill the conditions (regressions). I tried to make power line work (either m+b*(a^gdp-1) fixing m, or m+a^gdp), but there was no solution (we'd need another specification with more parameters).
  # The issue with linear line is the spread of transfers - though this may be geopolitically convenient (which reduces over time as countries converge). Other methods concentrate more the transfers on poorest countries (though less so over time as countries converge).

  new_gni <- function(gdp = pg$gdp, coef = a, demogrant = m, by_month = monthly) return(((gdp >= phase_out) * gdp + (gdp < phase_out) * (demogrant + a*gdp)) / ifelse(by_month, 12, 1))
  rev_pc <- function(gdp = pg$gdp, coef = a, demogrant = m, by_month = monthly) return((new_gni(gdp, by_month = F) - gdp) / ifelse(by_month, 12, 1))
  gdp_share <- function(gdp = pg$gdp, coef = a, demogrant = m) return(rev_pc(gdp, by_month = F)/gdp)
  global_share_pc <- function(gdp = pg$gdp, pop = pg$pop, revenues = global_revenues, coef = a, demogrant = m) return(rev_pc(gdp, coef = a, demogrant = m, by_month = F)/(revenues/sum(pop)))
  global_share <- function(gdp = pg$gdp, pop = pg$pop, revenues = global_revenues) {  return(rev_pc(gdp, by_month = F) * pop / revenues)  }

  if (any(new_gni()[order(pg$gdp)] != sort(new_gni(), na.last = T), na.rm = T)) warning("new_gni is not increasing")
  if (any(rev_pc()[order(pg$gdp)] != sort(rev_pc(), decreasing = T, na.last = T), na.rm = T)) warning("new_gni is not decreasing")
  
  if (!is.null(country)) return(key_gdp(return_var = return_var, return_type = "var", PPP = PPP, monthly = monthly, df = df, global_revenues = global_revenues, phase_out = phase_out)[df$country == country])
  else if (return_type == "var") {
    if (return_var == "global_share") return(global_share())
    if (return_var == "global_share_pc") return(global_share_pc())
    if (return_var == "gdp_share") return(gdp_share())
    if (return_var == "rev_pc") return(rev_pc())
    if (return_var == "new_gni") return(new_gni())
    if (return_var == "gdp") return(pg$gdp / ifelse(monthly, 12, 1))
  } else if (return_type == "function") {
    if (return_var == "global_share") return(global_share)
    if (return_var == "global_share") return(global_share_pc)
    if (return_var == "gdp_share") return(gdp_share)
    if (return_var == "rev_pc") return(rev_pc)
    if (return_var == "new_gni") return(new_gni)
  } else if (return_type == "list") {
    if (return_var == "global_share") return(sort(setNames(global_share()[pg$pop > min_pop], pg$country[pg$pop > min_pop]), decreasing = T))
    if (return_var == "global_share_pc") return(sort(setNames(global_share_pc()[pg$pop > min_pop], pg$country[pg$pop > min_pop]), decreasing = T))
    if (return_var == "gdp_share") return(sort(setNames(gdp_share()[pg$pop > min_pop], pg$country[pg$pop > min_pop]), decreasing = T))
    if (return_var == "rev_pc") return(sort(setNames(rev_pc()[pg$pop > min_pop], pg$country[pg$pop > min_pop]), decreasing = T))
    if (return_var == "new_gni") return(sort(setNames(new_gni()[pg$pop > min_pop], pg$country[pg$pop > min_pop]), decreasing = T))
    if (return_var == "gdp") return(sort(setNames(pg$gdp[pg$pop > min_pop] / ifelse(monthly, 12, 1), pg$country[pg$pop > min_pop]), decreasing = T))
  } else if (return_type == "params") return(c("coef" = a, "demogrant" = m))
}
# sum(key_gdp("gdp", return_type = 'var', monthly = F) * pg$pop_2019, na.rm=T)/(sum(key_gdp("new_gni", return_type = 'var', monthly = F) * pg$pop_2019, na.rm=T)-global_revenues)
key_gdp(return_type = 'params', monthly = F)
plot(key_gdp("gdp", return_type = 'var', monthly = F), key_gdp("new_gni", return_type = 'var', monthly = F), xlim = c(0,3000), ylim = c(0,3000), xlab = "GDP pc 2019", ylab = "GNI pc after transfers", lwd = 2)
lines(c(0,1e5), c(0,1e5), type = 'l') + grid() # /!\ Big issue with this allocation: it doesn't preserve GDP rankings.
# If a country with gdp=600 gets new_gni=y=1080 (like Mozambique), to preserve GDP rankings around x=600, we need pg(x+dx)>p(x)*(1-dx/(y-x)). [This comes from: y = x + b*p(x) = x+dx + b.p(x+dx) where p is the predicted pg, and y-x(-dx) is the transfer rev_pc at x(+dx), and the equality of botwh new_gni (with y) is the limit where GDP rankings are preserved]
x <- key_gap_gdp("gdp", return_type = 'var', PPP = T, monthly = F, poverty_line = 4)[pg$country == "Madagascar"] # 600 # log10(600)=2.78 key_gdp(return_var = "gdp", country = "Mozambique", monthly = F)
y <- key_gap_gdp("new_gni", return_type = 'var', PPP = T, monthly = F, poverty_line = 4)[pg$country == "Madagascar"] # Use Madagascar, Tanzania or Niger
p <- pg$pg4[pg$country == "Madagascar"]
x <- key_gdp(return_var = "gdp", country = "Niger", monthly = F) # 600
y <- key_gdp(return_var = "new_gni", country = "Niger", monthly = F) # 1080
p <- pg$pg4[pg$country == "Niger"] # 50
iso_y <- data.frame(z = seq(2.4, 4, 0.01))
iso_y$pz <- log10(p*(1-(10^iso_y$z-x)/(y-x)))

# Best solutions: 0. PMA, 1. quadratic fit of pg (pb: meets monotonicity conditions by chance), 2. affine line up to 4k (pb: nothing for 4-10k), 3. piecewise affine continuous with breakpoints and transfers share for each income bin decided in advance (pb: arbitrariness).
# TODO! Poverty-Minimizing (Allocation) Revenue Sharing Between Countries (PMA): Define a number (or-and locations) of breakpoints; Compute the distances between what a country receives and its poverty gap, for each parametrization of the piecewise affine continuous allocation; Pick the parameters that minimize the sum of (squared?) distances.
# Unused:
table_pg <- key_gdp(return_var = "gdp_share", return_type = "list")
(table_pg <- cbind("gdp_share" = 100*table_pg, "rev_pc" = key_gdp("rev_pc")[names(table_pg)], "global_share_pc" = key_gdp("global_share_pc")[names(table_pg)], "global_share" = 100*key_gdp("global_share")[names(table_pg)]))
row.names(table_pg)[row.names(table_pg) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
cat(paste(kbl(table_pg[table_pg[,1] > 0,], "latex", caption = "Allocation of the global wealth tax revenues.", position = "b", escape = F, booktabs = T, digits = c(2, 0, 2, 2), linesep = rep("", nrow(table_pg)-1), longtable = T, label = "allocation",
              col.names = c("\\makecell{Revenues\\\\over GDP\\\\(in percent)}", "\\makecell{Revenues\\\\per capita\\\\(in \\$ per month)}", "\\makecell{Revenues per capita\\\\over average\\\\revenues p.c.}", "\\makecell{Global\\\\share of\\\\revenues}")), collapse="\n"), file = "../tables/allocation.tex") 
  # TODO: do total, re-order all by GDP pc

pg$log_gdp <- log10(pg$gdp_pc_2019)
fit <- lm(log10(pg4) ~ log_gdp, data = pg[pg$pg4 > 0 & pg$gdp_pc_2019 < wtd.mean(pg$gdp_pc_2019, pg$pop_2019),], weights = pop_2023) # 
segmented.fit <- segmented(fit, seg.Z = ~log_gdp, weights = pop_2023) # npsi: number of breakpoints, fixed.psi: location of breakpoints (if not automatically found)
# segmented.fit <- segmented(fit, seg.Z = ~log_gdp, psi=3, fixed.psi = c(3, 3.5), weights = pop_2023)
# plot(pg$x, log10(pg$pg4), pch=16, col='steelblue')
# plot(segmented.fit, add=T)
predicted_data <- data.frame(log_gdp = pg$log_gdp, pop_2023 = pg$pop_2023, gdp_pc_2019 = pg$gdp_pc_2019, GDPpcPPP = pg$GDPpcPPP, pg4 = pg$pg4)
predicted_data$predicted_pg <- predict(segmented.fit, newdata = predicted_data)

mean_GDPpcPPP <- wtd.mean(pg$GDPpcPPP, pg$pop_2019)
mean_gdp_pc_2019 <- wtd.mean(pg$gdp_pc_2019, pg$pop_2019)
qplot(log10(gdp_pc_2019), log10(pg4), data = pg[pg$pg4 > 0 & pg$gdp_pc_2019 < mean_gdp_pc_2019,], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),  mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()

qplot(log10(GDPpcPPP), log10(pg4), data = pg[pg$pg4 > 0 & pg$GDPpcPPP < mean_GDPpcPPP,], size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "loess",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(gdp_pc_2019), log10(pg4), data = pg[pg$pg4 > 0 & pg$gdp_pc_2019 < mean_gdp_pc_2019,], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "loess", mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(GDPpcPPP), log10(pg4), data = pg[pg$pg4 > 0 & pg$GDPpcPPP < mean_GDPpcPPP,], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(gdp_pc_2019), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),  mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(gdp_pc_2019), log10(pg4), data = pg[pg$pg4 > 0 & pg$gdp_pc_2019 < mean_gdp_pc_2019,], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),  mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() + 
  geom_line(data = iso_y, mapping = aes(x = z, y = pz), inherit.aes = F, size = 1.3) # If the red curve decreases faster that the black one, GDP rankings won't be preserved (use Niger to define iso_y)
qplot(log10(gdp_pc_2019), log10(pg7), data = pg, size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $6.85 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3),  mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(gdp_pc_2019), log10(pg7), data = pg[pg$pg7 > 0 & pg$gdp_pc_2019 < wtd.mean(pg$gdp_pc_2019, pg$pop_2019),], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $6.85 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),  mapping = aes(weight = pop_2023 * (gdp_pc_2019 < mean_gdp_pc_2019)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(gdp_pc_2019), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_line(data = predicted_data, mapping = aes(log_gdp, predicted_pg), size = 2, color = "red", show.legend = FALSE) + theme_bw()
qplot(log10(gdp_pc_2019), log10(pg4), data = pg[pg$pg4 > 0 & pg$gdp_pc_2019 < mean_gdp_pc_2019,], size = pop_2023, xlab = "log10 of GDP per capita in 2019 (nominal $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_line(data = predicted_data[predicted_data$pg4 > 0 & predicted_data$gdp_pc_2019 < mean_gdp_pc_2019,], mapping = aes(log_gdp, predicted_pg), size = 2, color = "red", show.legend = FALSE) + theme_bw()
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita , PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_line(data = predicted_data, mapping = aes(log_gdp, predicted_pg), size = 2, color = "red", show.legend = FALSE) + theme_bw()
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "loess",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", formula = y ~ I(exp(x)) + I(exp(x)^2), mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw()
qplot(log10(GDPpcPPP), log10(pg7), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $6.85 a day (2017 PPP) (%)", show.legend = FALSE, ylim = c(-1, 2)) + 
  geom_smooth(method = "lm", mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", size = 2, show.legend = FALSE, se = F) + theme_bw() +
  geom_line(data = iso_y, mapping = aes(x = z, y = pz), inherit.aes = F, size = 1.3) # If the red curve decreases faster that the black one, GDP rankings won't be preserved (which is the case here on the left) (use Madagascar to define iso_y)
qplot(log10(GDPpcPPP), log10(pg2), data = pg, size = pop_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $2.15 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = pop_2023 * (GDPpcPPP < mean_GDPpcPPP)), color = "red", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE, se = F) + theme_bw() +
  geom_line(aes(y = log10(predicted_gap)), size = 2, color = "red", show.legend = FALSE)

key_gdp("gdp")
key_gdp("new_gni")
key_gdp("rev_pc")

# SSA gets more when using PPP. Gets most with line at 4. For lines at 2 and 4, gets more if max_reg = mean; for line at 7, gets more if max_reg = Inf.
# Three most credible are l=4, not PPP, max_reg mean; l=7, PPP, max_reg mean; l=4, PPP, max_reg Inf; l=4, not PPP, max_reg Inf
# LIC <- pg$code[pg$gdp_pc_2019 < 1085] # closer to the 2021 classification available on World Bank data for which LIC: 700M people
HIC <- pg$code[pg$gdp_pc_2019 > 13205] # approximately right (63 countries instead of 81 but differences are due to small islands)  
LIC <- c("AFG", "BFA", "BDI", "TCD", "COD", "ERI", "ETH", "GMB", "GIN", "GNB", "PRK", "LBR", "MDG", "MWI", "MLI", "MOZ", "NER", "RWA", "SOM", "SRE", "SDN", "SSD", "SYR", "TGO", "UGA", "YEM") # 2023 official classification. LIC: 650M people " # New: ZMB no longer LIC!
# SSA <- c("SDN", "AGO", "GIN", "GMB", "GNB", "GNQ", "BDI", "BEN", "BFA", "SEN", "BWA", "CAF", "SLE", "SOM", "SSD", "CIV", "CMR", "COD", "COG", "COM", "LBR", "LSO", "SWZ", "TCD", "TGO", "MLI", "MDG", "DJI", "ERI", "ESH", "ETH", "MWI", "MUS", "MRT", "MOZ", "TZA", "UGA", "ZMB", "ZWE", "NGA", "NER", "NAM", "GHA", "GAB") # old
SSA <- c("SDN", "AGO", "GIN", "GMB", "GNB", "GNQ", "BDI", "BEN", "BFA", "SEN", "BWA", "CAF", "SLE", "SOM", "SSD", "CIV", "CMR", "COD", "COG", "COM", "LBR", "LSO", "SWZ", "TCD", "TGO", "MLI", "MDG", "DJI", "ERI", "ESH", "ETH", "MWI", "MUS", "MRT", "MOZ", "TZA", "UGA", "ZMB", "ZWE", "NGA", "NER", "NAM", "GHA", "GAB", "RWA", "ZAF")
SSA_max_reg_inf <- SSA_max_reg_mean <- LIC_max_reg_inf <- LIC_max_reg_mean <- India_max_reg_inf <- India_max_reg_mean <- array(dimnames = list("line" = c(2, 4, 7), "PPP" = c(T, F)), dim = c(3, 2))
for (l in c(2, 4, 7)) for (p in c(TRUE, FALSE)) {
  SSA_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T))
  SSA_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T))
  LIC_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% LIC], na.rm = T))
  LIC_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% LIC], na.rm = T))
  India_max_reg_inf[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% "IND"], na.rm = T))
  India_max_reg_mean[as.character(l), as.character(p)] <- round(sum(key_gdp(poverty_line = l, PPP = p, return_var = "global_share", return_type = "var")[pg$code %in% "IND"], na.rm = T))
}
SSA_max_reg_inf
SSA_max_reg_mean
LIC_max_reg_inf
LIC_max_reg_mean
India_max_reg_inf
India_max_reg_mean
key_gdp(poverty_line = 4, PPP = F, return_var = "global_share")
key_gdp(poverty_line = 4, PPP = F, max_reg = Inf, return_var = "global_share_pc")
sum(key_gdp(poverty_line = 4, PPP = T, max_reg = Inf, return_var = "global_share", return_type = "var")[pg$code %in% SSA], na.rm = T)
pg$global_share <- key_gap_gdp(return_var = "global_share", return_type = "var")
pg$global_share_pc <- key_gap_gdp(return_var = "global_share_pc", return_type = "var")
pg$rev_pc <- key_gap_gdp(return_var = "rev_pc", return_type = "var")
pg$gdp_share <- key_gap_gdp(return_var = "gdp_share", return_type = "var")
sum(pg$global_share[pg$code %in% SSA], na.rm = T) # 39%, linear: 53%
wtd.mean(pg$rev_pc[pg$code %in% SSA], pg$pop_2019[pg$code %in% SSA], na.rm = T) # $26, linear: $36
wtd.mean(pg$rev_pc[pg$code %in% SSA], pg$pop_2019[pg$code %in% SSA], na.rm = T) / wtd.mean(pg$rev_pc, pg$pop_2019, na.rm = T) # 2.9, linear: 4
wtd.mean(pg$global_share_pc[pg$code %in% SSA], pg$pop_2019[pg$code %in% SSA], na.rm = T)
wealth_tax_revenues <- 0.0176*1e14  # 0.013*96e12 (2/4/6) # From a 2% tax above $5 million / 6% above $100M / 10% above $1G, cf. Chancel et al. (2022) https://wid.world/world-wealth-tax-simulator/ # 0.0085
pooled_revenues <- 0.5 * wealth_tax_revenues
pooled_revenues * sum(pg$global_share[pg$code %in% SSA], na.rm = T) / sum((pg$gdp_pc_2019 * pg$pop_2019)[pg$code %in% SSA], na.rm = T) # 25%, linear: 32%
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

# View(co2_pop[, c("code", "country", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION")])
co2_pop$`MESSAGE-GLOBIOM.REGION`[is.na(co2_pop$`MESSAGE-GLOBIOM.REGION`)] <- "LAM"
regions <- unique(co2_pop$`MESSAGE-GLOBIOM.REGION`)
message_region_by_image <- c("BRA" = "LAM", "CAN" = "NAM", "CEU" = "EEU", "CHN" = "CPA", "EAF" = "AFR", "INDIA" = "SAS", "INDO" = "PAS", "JAP" = "PAO", "KOR" = "PAS", "ME" = "MEA", "MEX" = "LAM", "NAF" = "MEA", "OCE" = "PAO", "RCAM" = "LAM", "RSAF" = "AFR", "RSAM" = "LAM", "RSAS" = "SAS", "RUS" = "FSU", "SAF" = "AFR", "SEAS" = "PAS", "STAN" = "FSU", "TUR" = "WEU", "UKR" = "FSU", "USA" = "NAM", "WAF" = "AFR", "WEU" = "WEU")
image_regions <- names(message_region_by_image)
message_regions <- unique(message_region_by_image) # this is regions sorted differently
message_region_by_code <- setNames(co2_pop$`MESSAGE-GLOBIOM.REGION`, co2_pop$code)
message_region_by_code[c("Dem USA", "Non-Dem USA")] <- "NAM"
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
# /!\ World emissions are not equal to the sum of the 5 R5.2 regions TODO why? check regions definition
# Nb regions (excluding the 5 R5.2 ones): AIM/CGE: 18 (3 letters), IMAGE: 27, GCAM4: 33, MESSAGE-GLOBIOM: 12, REMIND-MAGPIE: 12 (different), WITCH-GLOBIOM: absent from SSPs_countries

# On top of partition in R5_region, IMAGE (ssp1_19, ssp1_26) has 26 regions (co2_pop$IMAGE.REGION), MESSAGE (ssp2) has 11 MESSAGE-GLOBIOM.REGION. No dissagregated data for ssp2_26, ssp2_45. 
# IMAGE regions are not refinement of MESSAGE's: North Korea, Vietnam (CPA in MESSAGE instead of PAS, SEAS in IMAGE), Non-Australia Oceania (PAS instead of OCE) or rather Australia, NZ (PAO (which is these 2 + Japan) instead of OCE), Cyprus (WEU instead of CEU), Sudan, South Sudan (MEA instead of EAF)

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
ssp2_26msg <- ssp$`SSP2-26`$`MESSAGE-GLOBIOM`
ssp2_45 <- ssp$`SSP2-45`$IMAGE
ssp2_ref <- ssp$`SSP2-45`$`MESSAGE-GLOBIOM`
# TODO: SSP5-Baseline is SSP5-8.5 but what is SSP1-Baseline? SSP2-Baseline? SSP2-4.5 
rm(SSPs, SSPs_countries)
rm(ssp)
# j <- "IMAGE"
# i <- unique(SSPs$SCENARIO)[1]
# y <- 2030

ssps <- c("ssp1_19", "ssp1_26", "ssp2_26", "ssp2_26msg", "ssp2_45", "ssp2_ref")
names(ssps) <- c("SSP1-1.9 (1.4 °C)", "SSP1-2.6 (1.8 °C)", "SSP2-2.6 (1.8 °C)", "SSP2-2.6  (1.8 °C)", "SSP2-4.5 (2.7 °C)", "SSP2 (baseline)")
# In all SSPs considered, carbon prices converge in 2035 at the latest. We use Asian carbon price, which are very close to the others except for ssp2 in 2020 and 2030
# for (s in ssps) for (y in years) if (gap(d(s)[[paste0("carbon_price_", y)]]) > .1) print(paste("Non unique carbon price for", s, y, "gap: ", round(gap(d(s)[[paste0("carbon_price_", y)]]), 3)))
# ssp1_19$carbon_price_2020[1:6] # c("asia", "lam", "maf", "oecd", "ref", "world")
# ssp2_26$carbon_price_2020[1:6]
# ssp2_26$carbon_price_2030[1:6]
# ssp2_45$carbon_price_2020[1:6]
# ssp2_45$carbon_price_2030[1:6]
# ssp2_ref$carbon_price_2020[1:6]
# ssp2_ref$carbon_price_2030[1:6]

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
# GEA: Emissions (2°C >50% chance), population and GDP pc until 2100 disaggregated on 11 countries (unlike SSP where only emissions are disaggregated in more than 5 regions), but without a price trajectory.
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
# View(rbind("gea_gea" = world_emissions_pc$gea_gea, "gea_image" = world_emissions_pc$gea_image, "SSP1-2.6" = world_emissions_pc$ssp1_26, "SSP2-2.6" = world_emissions_pc$ssp2_26, "SSP2" = world_emissions_pc$ssp2_ref))
# GEA scenario is closest to ssp1_26 (higher emissions than ssp2_26 before 2050, larger negative emissions after), gea_image has almost no negative emissions => use gea_gea
# I impute the price trajectory of ssp2_26 (at least three times higher than ssp1_26) to be conservative although emissions_pc are much closer to ssp1_26
carbon_price$gea_gea <- carbon_price$ssp2_26

world_population$gea_gea <- setNames(gea$GEA[gea$GEA$region == "world", grepl("pop_", names(gea$GEA))], years)
world_population$gea_image <- setNames(gea$IMAGE[gea$IMAGE$region == "world", grepl("pop_", names(gea$IMAGE))], years)
world_emissions$gea_gea <- setNames(world_emissions_pc$gea_gea * world_population$gea_gea, years)
world_emissions$gea_image <- setNames(world_emissions_pc$gea_image * world_population$gea_image, years)
# View(rbind("gea_gea" = world_population$gea_gea, "gea_image" = world_population$gea_image, "SSP1-2.6" = world_population$ssp1_26, "SSP2-2.6" = world_population$ssp2_26, "SSP2" = world_population$ssp2_ref))
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


##### GDR / CERF #####
# Two methods to compare GDRs to equal pc (with the common limitation that we have to use a point estimate in 2030 as they don't provide figures beyond that date):
#>1. Use "our" gea_gea figures, and adjust the GDR allocation to the updated world_emissions$gea_gea["2030"] = 30.9Gt instead of their 1°5 29.4Gt (our closest figure is ssp2_26: 28.7Gt), LED 24.4Gt (our closest is ssp2_26: 28.7Gt in 2030) or their 2DS 35.95Gt (closest gea_image: 35Gt)
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

# Disaggregate by country emissions, gdp, gdp_pc

# compute_npv <- function(var = "gain_pa_", discount_rate = discount, data = co2_pop) {
#   rate <- (1+discount_rate)^10
#   # /!\ NPV is computed on 2020-2100. TODO? Compute it on 2030-2080 (this would make India neutral in Generous EU as the positive part comes from 2020).
#   return(rowSums(sapply(2:10, function(i) { return(10*data[[paste0(var, 2000+10*i)]]/rate^(i-2)) })))
# }
# 
# compute_gain_given_parties <- function(parties = df$code, df = co2_pop, return = "df", discount = .03, ssp_name = "gea_gea") {
#   if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
#   basic_income <- basic_income_adj <- c()
#   for (y in seq(2020, 2100, 10)) {
#     yr <- as.character(y)
#     df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
#     temp <- rep(T, nrow(df)) # df$code %in% parties # average_revenues is average emissions_pa * carbon_price while basic_income is adjusted for participation_rate due to opt-out and anti-regressive mechanism
#     while (any(temp != df[[paste0("large_footprint_", y)]])) {
#       temp <- df[[paste0("large_footprint_", y)]]
#       basic_income[yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
#       df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > basic_income[yr])
#       df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
#     }
#     df[[paste0("gain_optout_", y)]] <- df[[paste0("participation_rate_", y)]] * (basic_income[yr] - df[[paste0("revenues_pa_", y)]])
#     # Adjusted to avoid high-income receiving money. Pb: GDP in PPP of Europe is not more than twice the world average 2050-2070.
#     # /!\ Pb, 2070 GDP pc PPP of China is larger that Western Europe in View(ssp1_26[,c("region", "gdp_pc_2020", "gdp_pc_2070")]) co2_pop$gdp_pc_2070[co2_pop$country %in% c("China", "Spain", "France", "Nigeria", "Namibia")]
#     # To estimate future emissions and GDP, I make the assumption that emissions_pc/GDPpc evolve in the same way in all big regions. Pb: this assumption is at odd with SSP1, where GDPpc converge across regions.
#     # => Either I should drop the country-by-country analysis, or I should find better projections of GDP.
#     y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]])
#     e_bar <- wtd.mean(df[[paste0("emissions_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
#     lambda <- pmax(0, pmin(1, (2.2*y_bar - df[[paste0("gdp_pc_", y)]])/((2.2-2)*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
#     lambda[is.na(lambda)] <- 1
#     df[[paste0("share_basic_income_", y)]] <- df[[paste0("participation_rate_", y)]] * (lambda + pmin(1, df[[paste0("emissions_pa_", y)]]/(1.3*e_bar))*(1-lambda))
#     df[[paste0("gain_adj_", y)]][df[[paste0("emissions_pa_", y)]] < 1.3*e_bar] <- (basic_income[yr] * df[[paste0("share_basic_income_", y)]] -
#                                                                                      df[[paste0("participation_rate_", y)]] * df[[paste0("revenues_pa_", y)]])[df[[paste0("emissions_pa_", y)]] < 1.3*e_bar]
#     basic_income_adj[yr] <- basic_income[yr] * (1 + wtd.mean(df[[paste0("participation_rate_", y)]] - df[[paste0("share_basic_income_", y)]], df[[paste0("adult_", y)]]))
#     df[[paste0("gain_adj_", y)]][lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pa_", y)]]))[lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar]
#     df[[paste0("gain_adj_over_gdp_", y)]] <- df[[paste0("gain_adj_", y)]]/df[[paste0("gdp_pc_", y)]]
#   }
#
#   # GDR: find emissions allocations on website and allocate total_revenues[[ssp_name]][yr]. They go only until 2030. Either I recover the GDRs from them (or their code) and apply them here, or I add the per-capita allocation to their code.
#   df$gain_gdr_2030 <- (carbon_price[[ssp_name]][["2030"]] * df$gdr_pa_2030  - df$revenues_pa_2030)
#   df$gain_gdr_over_gdp_2030 <- df$gain_gdr_2030/df$gdp_pa_2030
#   df$diff_gain_gdr_gcs_2030 <- df$gain_gdr_2030 - df$gain_pa_2030
#   df$diff_gain_gdr_gcs_over_gdp_2030 <- df$diff_gain_gdr_gcs_2030/df$gdp_pa_2030
#   df$diff_gain_gdr_gcs_adj_2030 <- df$gain_gdr_2030 - df$gain_adj_2030
#   df$diff_gain_gdr_gcs_adj_over_gdp_2030 <- df$diff_gain_gdr_gcs_adj_2030/df$gdp_pa_2030
#
#   df$npv_pa_gcs <- compute_npv("gain_pa_", discount = discount, data = df)
#   df$npv_pa_gcs_adj <- compute_npv("gain_adj_", discount = discount, data = df)
#   df$npv_over_gdp_gcs <- df$npv_pa_gcs/compute_npv("gdp_pa_", discount = discount, data = df) # this formula corresponds to the % loss in consumption computed in Balanced Growth Equivalent of Stern et al. (07)
#   df$npv_over_gdp_gcs_adj <- df$npv_pa_gcs_adj/compute_npv("gdp_pa_", discount = discount, data = df)
#
#   return(df)
# }
#
# total_revenues <- average_revenues <- average_revenues_bis <- basic_income <- basic_income_adj <- list()
# create_var_ssp <- function(ssp, df = co2_pop, base_year = 2019, CC_convergence = 2040, discount = .03, opt_out_threshold = 1.5, full_part_threshold = 2, scenario = "all_countries") { # message is only for ssp2_ref , region = message_region_by_code
#   ssp_name <- deparse(substitute(ssp))
#   if (grepl("ssp1", ssp_name)) model <- "IMAGE"
#   else if (ssp_name %in% c("ssp2_ref", "ssp2_26_country") | grepl("gea", ssp_name)) model <- "MESSAGE"
#   else model <- "big"
#   # Dirty fix for unrealistically high projections of GDP pc for middle-income African countries: we assign them to China region, which has a comparable GDP pc, so the projection of GDP pc are more credible
#   # TODO? Make our own projections for all countries, grouping countries based on GDP pc and carbon footprint rather than geography, and deriving projections by group from SSPs or GEA macro-regions.
#   recoded_countries <- c("BWA", "GAB", "GNQ", "ZAF", "NAM")
#   if (grepl("gea", ssp_name)) message_region_by_code_original <- message_region_by_code
#   if (grepl("gea", ssp_name)) message_region_by_code[recoded_countries] <- "MEA" # c("Botswana", "Gabon", "Equatorial Guinea", "South Africa", "Namibia)
#   region <- if (model == "big") big_region_by_code else { if (model == "IMAGE") image_region_by_code else message_region_by_code }
#   regions <- if (model == "big") big_regions else { if (model == "IMAGE") image_regions else message_regions }
#   total_revenues[[ssp_name]] <- average_revenues[[ssp_name]] <- average_revenues_bis[[ssp_name]] <- basic_income[[ssp_name]] <- basic_income_adj[[ssp_name]] <- c()
#   if (!exists("scenarios_parties") & scenario == "all_countries") parties <- df$code
#   else parties <- scenarios_parties[[scenario]]
#   if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
#
#   if ("Dem USA" %in% parties) { # split USA for scenario == "optimistic" into Dem USA (the 12 States + DC with Democratic lead > 10 pp) and Non-Dem USA
#     # all_us <- dem_us <- non_dem_us <- as.data.frame(df[df$code == "USA",])
#     # dem_us$country_map <- dem_us$code <- "Dem USA" # dem_us$country <-
#     # # non_dem_us$country_map <- non_dem_us$code <- "Non-Dem USA" # non_dem_us$country <-
#     # dem_us$gdp_pc_2019 <- (.3897/.3295) * all_us$gdp_pc_2019
#     # dem_us$GDPpcPPP <- (.3897/.3295) * all_us$GDPpcPPP
#     # # non_dem_us$gdp_pc_2019 <- ((1 - .3897)/(1 - .3295)) * all_us$gdp_pc_2019
#     # # non_dem_us$GDPpcPPP <- ((1 - .3897)/(1 - .3295)) * all_us$GDPpcPPP
#     # for (y in years) for (v in paste0(c("pop_", "adult_"), y)) {
#     #   dem_us[[v]] <- .3295 * all_us[[v]]
#     #   # non_dem_us[[v]] <- (1 - .3295) * all_us[[v]]
#     # } # TODO (though not necessary) territorial_, footprint_ emissions_n gdp_2019, share_territorial
#     # dem_us[[paste0("emissions_pa_", base_year)]] <- (.2033/.3295) * all_us[[paste0("emissions_pa_", base_year)]]
#     # # non_dem_us[[paste0("emissions_pa_", base_year)]] <- ((1 - .2033)/(1 - .3295)) * all_us[[paste0("emissions_pa_", base_year)]]
#     # # df <- rbind(df[!df$code %in% c("USA", "Dem USA", "Non-Dem USA"),], dem_us, non_dem_us)
#     # df <- rbind(df[!df$code %in% c("USA", "Dem USA", "Non-Dem USA"),], dem_us)
#
#     for (y in years) for (v in paste0(c("pop_", "adult_"), y)) df[[v]][df$code == "USA"] <- .3429 * df[[v]][df$code == "USA"]
#     for (v in c("gdp_pc_2019", "GDPpcPPP")) df[[v]][df$code == "USA"] <- (.4082/.3429) * df[[v]][df$code == "USA"]
#     df[[paste0("emissions_pa_", base_year)]][df$code == "USA"] <- (.2149/.3429) * df[[paste0("emissions_pa_", base_year)]][df$code == "USA"]
#   }
#
#   for (y in years) {
#     yr <- as.character(y)
#     for (v in paste0(c("pop_", "adult_", "emissions_", "gdp_"), y)) if (!v %in% names(df)) df[[v]] <- NA
#     if (!paste0("gdp_", y) %in% names(ssp)) {
#       if (paste0("gdp_mer_", y) %in% names(ssp)) {
#         ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_mer_", y)]]
#         df$gdp_pc_base_year <- df$gdp_pc_2019 # manage missing values (Venezuela, Yemen, South Sudan, North Korea, Eritrea, fix Western Sahara)
#       } else {
#         ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_ppp_", y)]]
#         df$gdp_pc_base_year <- df$GDPpcPPP  # TODO: manage missing values (Saudi Arabia, Afghanistan, New Zealand, Cambodia...)
#       }
#     } else df$gdp_pc_base_year <- df$GDPpcPPP
#     if (grepl("gea", ssp_name)) {
#       for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "MEA"] <- ssp[[v]][ssp$region == "MEA"] * (1 + sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
#       for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "AFR"] <- ssp[[v]][ssp$region == "AFR"] * (1 - sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
#       for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "MEA"] <- ssp[[paste0(v, y)]][ssp$region == "MEA"] * (1 + sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
#       for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "AFR"] <- ssp[[paste0(v, y)]][ssp$region == "AFR"] * (1 - sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
#     }
#     for (r in regions) { # Country downscaling
#       region_r <- region[df$code] == r # df$code %in% region[r]
#       # /!\ The line below overwrites UN's pop projections
#       for (v in paste0(c("pop_", "adult_"), y)) df[[v]][region_r] <- df[[v]][region_r] * ssp[[v]][ssp$region == r] / sum(df[[v]][region_r], na.rm = T)
#       reduction_factor_ry <- ssp[[paste0("emissions_", y)]][ssp$region == r]/sum((df[[paste0("emissions_pa_", base_year)]] * df[[paste0("adult_", y)]])[region_r])
#       df[[paste0("emissions_pa_", y)]][region_r] <- reduction_factor_ry * df[[paste0("emissions_pa_", base_year)]][region_r] # df$emissions_2019[region_r] * ssp[[paste0("emissions_", y)]][ssp$region == r] / sum(df$emissions_2019[region_r], na.rm = T)
#       df[[paste0("emissions_", y)]][region_r] <- (df[[paste0("emissions_pa_", y)]] * df[[paste0("adult_", y)]])[region_r]
#       growth_factor_ry <- ssp[[paste0("gdp_", y)]][ssp$region == r]/sum((df$gdp_pc_base_year * df[[paste0("pop_", y)]])[region_r], na.rm = T)
#       df[[paste0("gdp_pc_", y)]][region_r] <- growth_factor_ry * df$gdp_pc_base_year[region_r]
#       df[[paste0("gdp_", y)]][region_r] <- (df[[paste0("gdp_pc_", y)]] * df[[paste0("pop_", y)]])[region_r] # df$gdp_ppp_now[region_r] * ssp[[paste0("gdp_ppp_", y)]][ssp$region == r] / sum(df$gdp_ppp_now[region_r], na.rm = T)
#     }
#     df[[paste0("gdp_pc_over_mean_", y)]] <- df[[paste0("gdp_pc_", y)]]/wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]])
#     df[[paste0("gdp_pa_", y)]] <- df[[paste0("gdp_", y)]]/df[[paste0("adult_", y)]]
#     df[[paste0("emissions_pc_", y)]] <- df[[paste0("emissions_", y)]]/df[[paste0("pop_", y)]]
#
#     # Unadjusted mean gain pa
#     if (y > 2015) {
#       df[[paste0("revenues_pa_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pa_", y)]]) # /12
#       total_revenues[[ssp_name]][yr] <- carbon_price[[ssp_name]][yr] * sum(df[[paste0("emissions_", y)]], na.rm = T) # ssp[[paste0("emissions_", y)]][ssp$region == "world"]
#       if (total_revenues[[ssp_name]][yr] < 0) df[[paste0("revenues_pa_", y)]] <- 0
#
#       # GCS
#       df[[paste0("gain_pa_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0("adult_", y)]], na.rm = T) - df[[paste0("revenues_pa_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
#       df[[paste0("gain_over_gdp_", y)]] <- df[[paste0("gain_pa_", y)]]/df[[paste0("gdp_pc_", y)]]
#       # Adjusted for opt out
#       df[[paste0("optout_right_", y)]] <- (full_part_threshold - pmax(opt_out_threshold, pmin(full_part_threshold, df[[paste0("gdp_pc_", y)]] / wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]]))))/(full_part_threshold - opt_out_threshold)
#       # Accounts for non-universal participation
#       average_revenues_bis[[ssp_name]][yr] <- total_revenues[[ssp_name]][yr]/ssp[[paste0("adult_", y)]][ssp$region == "world"] # a bit different from average_revenues because in ssp, world emissions is not the sum of regional emissions (be it image_ or big_ regions)
#       average_revenues[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("adult_", y)]])
#       df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > average_revenues[[ssp_name]][yr])
#
#       # C&C: define climate debt/credit until convergence date
#     }
#   }
#
#   df_parties <- compute_gain_given_parties(parties, df = df, return = "df", ssp_name = ssp_name)
#   for (y in seq(2020, 2100, 10)) {
#     yr <- as.character(y)
#     basic_income[[ssp_name]][yr] <- wtd.mean(df_parties[[paste0("revenues_pa_", y)]], df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("adult_", y)]])
#     basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + wtd.mean(df_parties[[paste0("participation_rate_", y)]] - df_parties[[paste0("share_basic_income_", y)]], df_parties[[paste0("adult_", y)]]))
#   }
#
#   total_revenues[[ssp_name]] <<- total_revenues[[ssp_name]]
#   average_revenues[[ssp_name]] <<- average_revenues[[ssp_name]]
#   average_revenues_bis[[ssp_name]] <<- average_revenues_bis[[ssp_name]]
#
#   if (length(setdiff(df$code, parties)) == 0) {
#     basic_income[[ssp_name]] <<- basic_income[[ssp_name]]
#     basic_income_adj[[ssp_name]] <<- basic_income_adj[[ssp_name]]
#     df <- df_parties
#   } else {
#     # if ("Dem USA" %in% parties) {
#     #   df <- rbind(df, all_us)
#     #   all_us <- as.data.frame(df_parties[df_parties$code == "Non-Dem USA",])
#     #   all_us$country_map <- all_us$code <- all_us$country <- "USA"
#     #   # for (v in names(all_us)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj|^diff_gain_gdr_gcs_adj"), names(all_us))]) all_us[[v]] <- 0
#     #   df_parties <- rbind(df_parties, all_us) # TODO: check that it doesn't raise an issue to duplicate USA (with both USA and Dem/Non-Dem USA)
#     # }
#     for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj|^diff_gain_gdr_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]] <- df_parties[[v]]
#     for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]][!df$code %in% parties] <- NA
#
#     # if ("Dem USA" %in% parties) for (v in names(df)) if (is.numeric(df[[v]]) & !grepl(paste0("S", scenario), v)) df[[v]][df$code %in% c("Dem USA", "Non-Dem USA")] <- 0
#   }
#   basic_income[[scenario]] <<- basic_income[[ssp_name]]
#   basic_income_adj[[scenario]] <<- basic_income_adj[[ssp_name]]
#   return(df)
# }
# Disaggregated data not available for ssp2_19 or ssp2_26 (only 5 regions instead of 11) and ssp1 is at odd with the assumption that GDPpc or emissions evolve in the same way in all countries (cf. comment in compute_gain_given_parties). 
# co2_pop <- create_var_ssp(ssp2_26)



co2_pop <- create_var_ssp(gea_gea, opt_out_threshold = 1.5) # /!\ necessary




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

# TODO! Historical resp: NPV equal pc + climate debt until today. Use function cumulative_emissions 

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

cor(co2_pop$emissions_pa_2019, co2_pop$gdp_pc_2019, use = "complete.obs") # .69
average_revenues$gea_gea/12
basic_income_adj$gea_gea/12


# GCS_adj_trajectories
mar <- par()$mar
par(mar = c(2.1, 4.1, 0.1, 4.1))
plot(years[3:9], basic_income_adj$gea_gea[1:7]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(-5, 53))
lines(years[3:9], world_emissions$gea_gea[3:9]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(years[3:9], carbon_price$ssp2_26[3:9], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5) 
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
legend("topright", legend = c("CO2 emissions", "Basic income", "Carbon price (right axis)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))

# FR GCP_trajectoires
plot(years[3:9], basic_income_adj$ssp2_26[as.character(years[3:9])]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Revenu de base ($ par mois); Émissions de CO2 (Gt par an)", ylim = c(-5, 53))
lines(years[3:9], world_emissions$gea_gea[3:9]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(years[3:9], carbon_price$ssp2_26[3:9], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
mtext("Prix du carbone ($/tCO2)", side=4, col="blue", line=2.5) 
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
legend("topright", legend = c("Émissions de CO2", "Revenu de base", "Prix du carbone (axe de droite)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))

(10*sum(world_emissions$gea_gea[4:8])+5*sum(world_emissions$gea_gea[c(3,9)]))/10^9 # total positive emissions 2020-80: 963 GtCO2
(10*sum(world_emissions$gea_gea[4:10])+5*sum(world_emissions$gea_gea[c(3,11)]))/10^9 # total 2020-2100 emissions (incl. net negative in 2080-2100): 756 GtCO2

# mar <- par()$mar
# par(mar = c(2.1, 4.1, 0.1, 4.1))
# plot(years[3:9], world_emissions$gea_gea[3:9]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(-5, 53))
# lines(years[3:9], basic_income_adj$gea_gea[1:7]/12, type = 'b', col = 'darkgreen', lwd = 2)
# par(new = T)
# plot(years[3:9], carbon_price$gea_gea[3:9], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
# mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5) 
# axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
# grid()
# legend("topright", legend = c("CO2 emissions", "Carbon price (right axis)", "Basic income"), col = c("red", "blue", "darkgreen"), lwd = 2, lty = c(1,2,1), pch = c(16, 17, 15))
# legend("topright", legend = c("CO2 emissions", "Carbon price (right axis)", "                                         "), col = c("red", "blue", "white"), lwd = 2, lty = c(1,2,1), pch = c(16, 17, 15))
# legend("topright", legend = c("CO2 emissions", "                                         ", ""), col = c("red", "white", "white"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))




##### Plots #####
# Net gains are closer to zero than for Stern-Stiglitz due to lower carbon_price$ssp1_26. In PPP, China is not below average GDPpc, hence its (small) cost.
plot_world_map("npv_over_gdp_gcs_adj", breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[3:9]) plot_world_map(paste0("gain_adj_over_gdp_", y), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[3:9]) plot_world_map(paste0("gain_adj_", y), breaks = c(-Inf, -1000, -500, -200, -100, -1e-10, 0, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, 0, 0, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[3:9]) plot_world_map(paste0("gain_over_gdp_", y), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCS\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years) plot_world_map(paste0("emissions_pc_", y), breaks = c(-Inf, 0, 1, 2, 4, 5, 7, 10, 15, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, rev_color = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, 1, 2, 4, 5, 7, 10, 15, Inf), sep = " to ", return = "levels")), 
               legend = paste0("tCO2 emissions\nper capita\nin ", y, "\nin GEA Efficiency"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("gain_gdr_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom GDRs\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("gain_gdr_over_gdp_2030", breaks = c(-Inf, -.04, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.04, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom GDRs\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_adj_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngains per adult:\nGDRs - GCP\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_adj_over_gdp_2030", breaks = c(-Inf, -.3, -.1, -.05, -.02, -1e-10, 0, .005, .01, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.3, -.1, -.05, -.02, 0, 0, .005, .01, .05, .1, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngains per adult:\nGDRs - GCP\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_2030", breaks = c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, -.1, .1, 50, 100, 400, 800, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\ngains per adult:\nGDRs - GCS\nin 2030 (in $ per year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_gain_gdr_gcs_over_gdp_2030", breaks = c(-Inf, -.3, -.1, -.05, -.02, -1e-10, 0, .005, .01, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.3, -.1, -.05, -.02, 0, 0, .005, .01, .05, .1, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Difference in\nnet gains:\nGDRs - equal pc\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_pa_gcs", breaks = c(-Inf, -30000, -10000, -1000, -1e-10, 0, 1000, 5000, 10000, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -30000, -10000, -1000, 0, 0, 1000, 5000, 10000, Inf), sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gains per adult\nfrom the GCS", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_pa_gcs_adj", breaks = c(-Inf, -30000, -10000, -1000, -1e-10, 0, 1000, 5000, 10000, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -30000, -10000, -1000, 0, 0, 1000, 5000, 10000, Inf), sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gains per adult\nfrom the GCP", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs", breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf # -.003, -.001, -.0005, 0, .0005, .01, .02
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gains per adult\nfrom the GCS (in % of GDP)", #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (s in scenarios_names[3]) {
  plot_world_map(paste0("S", s, "_npv_over_gdp_gcs_adj"), breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
                 labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
                 legend = paste0("Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan\nScenario: ", capitalize(gsub("_", " ", s))), #fill_na = T, \n(with 4% discount rate)
                 save = T, parties = scenarios_parties[[s]])
  y <- 2030 # Non parties in black
  plot_world_map(paste0("S", s, "_gain_adj_over_gdp_", y), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
                 labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")),
                 legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)\nScenario: ", capitalize(gsub("_", " ", s))), #fill_na = T,
                 save = T, parties = scenarios_parties[[s]])
}
# FR
for (y in years[4]) plot_world_map(paste0("gain_adj_", y), breaks = c(-Inf, -1000, -500, -200, -100, -1e-10, 0, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
                                     labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, 0, 0, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), filename = paste0("gain_adj_", y, "_fr"),
                                     legend = paste0("Gain net\npar adulte au\nPlan mondial pour le climat\nen ", y, " (en $ par an)"), #fill_na = T,
                                     save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs_adj", breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), filename = "npv_over_gdp_gcs_adj_fr",
               legend = "Gains nets au\nPlan mondial pour le Climat\nagrégés sur le siècle\n(en % of GDP)", #fill_na = T, \n(with 4% discount rate)
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 


##### NPV of the basic income #####
discount_rate <- .03
rate <- (1+discount_rate)^10
(monthly_annualized_basic_income <- discount_rate*sum(sapply(2:10, function(i) { return(10*basic_income_adj$gea_gea[[as.character(2000+10*i)]]/rate^(i-2)) }))/12)
(monthly_annualized_gdp_pa <- discount_rate*sum(sapply(2:10, function(i) { return(10*wtd.mean(co2_pop[[paste0("gdp_pa_", 2000+10*i)]], co2_pop[[paste0("adult_", 2000+10*i)]])/rate^(i-2)) }))/12)
39.4/1754 # 2.2%


# Old plots SSP
mar <- par()$mar
par(mar = c(2.1, 4.1, .1, .1))
plot(years, sapply(years, function(y) ssp1_19[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "green", lwd = 2, type = 'l', xlab = "", ylab = "Carbon price (US$2005/tCO2)", xlim = c(2010, 2100), ylim = c(0, 900))
lines(years, sapply(years, function(y) ssp1_26[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "blue", lwd = 2, type = 'l')
lines(years, sapply(years, function(y) ssp2_26[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "purple", lwd = 2, type = 'l')
lines(years, sapply(years, function(y) ssp2_45[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "red", lwd = 2, type = 'l')
# lines(years, sapply(years, function(y) ssp2_45[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "orange", lwd = 2, type = 'l')
# lines(years, sapply(years, function(y) ssp2_ref[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), col = "red", lwd = 2, type = 'l')
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
  legend(if (var == "emissions") { if (ssp == "ssp2_ref") "bottomleft" else "topright" } else { if (ssp == "ssp2_45") "bottomleft" else "topleft"}, 
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
# plot_ssp("ssp2_ref") # OECD > Africa, with divergence between regions

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
plot_ssp("ssp2_ref", regions = regions_MESSAGE, ylim = c(-10, 15))

# /!\ Huge variations in gains of Former Soviet Union, questioning the data quality
plot_ssp("ssp1_19", var = "gain", ylim = c(-70, 160)) # from 2040 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp1_26", var = "gain", ylim = c(-30, 50)) # from 2060 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp2_26", var = "gain", ylim = c(-70, 360)) # from 2060 onwards, OECD wins and Africa, Asia lose
plot_ssp("ssp2_45", var = "gain", ylim = c(-22, 6)) # OECD, lam, FSU always lose and Africa, Asia always win
# plot_ssp("ssp2_ref", var = "gain", ylim = c(-20, 60)) # Very low amounts, lam wins and OECD, FSU lose

plot_ssp("ssp1_19", var = "gain", ylim = c(-100, 190), regions = regions_IMAGE_modif) # 
plot_ssp("ssp1_26", var = "gain", ylim = c(-30, 80), regions = regions_IMAGE_modif) # 
plot_ssp("ssp2_ref", var = "gain", ylim = c(-30, 40), regions = regions_MESSAGE) #

# average world emissions by SSP throughout 21st century: 1 for 1.9; 2 for 2.6; 4 for 4.5.
# mean(sapply(years, function(y) ssp1_19[[paste0("emissions_pc_", y)]][ssp1_19$region == "world"])) # 1.1
# mean(sapply(years, function(y) ssp1_26[[paste0("emissions_pc_", y)]][ssp1_26$region == "world"])) # 2.2
# mean(sapply(years, function(y) ssp2_26[[paste0("emissions_pc_", y)]][ssp2_26$region == "world"])) # 1.9
# mean(sapply(years, function(y) ssp2_45[[paste0("emissions_pc_", y)]][ssp2_45$region == "world"])) # 4.1
# mean(sapply(years, function(y) ssp2_ref[[paste0("emissions_pc_", y)]][ssp2_ref$region == "world"])) # 4.1


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

co2_pop$npv_over_gdp_gcs_adj[co2_pop$country == "Colombia"]


##### 60% of emissions #####
co2_pop$share_territorial_2019 <- co2_pop$territorial_2019/sum(co2_pop$territorial_2019)
co2_pop$emissions_pc_2019 <- co2_pop$emissions_2019/co2_pop$pop_2019
cumulative_emissions <- function(var = "cumulative_average_pc", variable_order = "emissions_pc_2019", decreasing_order = T, variable_share = "share_emissions_2019", variable_pop = "pop_2019", data = co2_pop) {
  order_by_var <- order(data[[variable_order]], decreasing = decreasing_order) 
  emissions_ordered <- setNames(data[[variable_share]], data$country)[order_by_var]
  (cumul_emissions <- setNames(sapply(1:204, function(i) sum(emissions_ordered[1:i])), names(emissions_ordered)))
  data$cumulative_average_pc <- data$cumul_pop <- NA
  if (var == "cumulative_average_pc") for (i in 1:length(order_by_var)) data$cumulative_average_pc[order_by_var[i]] <- sum((data[[variable_order]] * data[[variable_pop]])[order_by_var[1:i]])/sum(data[[variable_pop]][order_by_var[1:i]])
  for (i in 1:length(order_by_var)) data$cumul_pop[order_by_var[i]] <- sum(data[[variable_pop]][order_by_var[1:i]])/sum(data[[variable_pop]])
  return(round(cbind(share = data[[variable_share]][order_by_var], cumul = cumul_emissions, var = data[[var]][order_by_var], var_over_max = data[[var]][order_by_var]/max(data[[var]][order_by_var]), cumul_pop = data$cumul_pop[order_by_var]), 3))
}

cumulative_emissions(var = "cumulative_average_pc", variable_order = "emissions_pc_2019", decreasing_order = FALSE)
emissions_tot/emissions_tot["2020"]
footprint_pc <- c()
for (s in scenarios_names) footprint_pc[s] <- sum(co2_pop$emissions_2019[co2_pop$code %in% scenarios_parties[[s]]])/sum(co2_pop$pop_2019[co2_pop$code %in% scenarios_parties[[s]]])
footprint_pc/wtd.mean(co2_pop$emissions_pc_2019, co2_pop$pop_2019)

cumulative_emissions(var = "mean_gain_2030", variable_order = "mean_gain_2030", variable_share = "share_emissions_2019") 
cumulative_emissions(var = "mean_gain_2030", variable_order = "mean_gain_over_gdp_2019", variable_share = "share_emissions_2019")
cumulative_emissions(var = "mean_gain_2030", variable_order = "mean_gain_2030", variable_share = "territorial_2019") 
cumulative_emissions(var = "mean_gain_2030", variable_order = "mean_gain_over_gdp_2019", variable_share = "territorial_2019")
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
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("JPN", "KOR", "NOR", "CHE", "NZL", "CAN")]) # 7%, JP + SK: 5%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("RUS", "KAZ", "SAU", "QAT", "KWT", "ARE", "OMN")]) # 9%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("IRN", "IRQ")]) # 2.5%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("AUS", "ISR", "MYS", "CHL", "URY", "PAN", "TKM")]) # 2.6%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% EU28_countries]) # 9.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% EU28_countries & co2_pop$code != "GBR"]) # 8.2%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("JPN", "KOR")]) # 5%

# Book scenarios
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("JPN", "KOR", "NOR", "CHE", "NZL", "CAN", EU28_countries) | co2_pop$npv_pa_gcs_adj >= 0], na.rm = T) # 73%
sum(co2_pop$share_territorial_2019[co2_pop$code %in% c("USA", "AUS", "RUS", "KAZ", "SAU", "QAT", "KWT", "ARE", "OMN", "BHR", "SGP", "MYS", "ISR", "CHL", "URY", "PAN", "TKM", "TTO")], na.rm = T) # 27%
# States with Democratic margin (e.g. 57%-41%) by >15pp: California + Illinois + New York + New Jersey + Washington + Massachusetts + Oregon + Connecticut + Delaware + Hawaii + Rhose Island + DC + Vermont + Maryland
(303.7+170.2+143.7+ 83.9+68.3+52.3+37.4+33.8+12.4+15+9.8+2.4+5.4+48.1)/4591 # 21.49% CO2 emissions, 3.1% of World total (incl. 2% for CA+IL+NY) # 2022 https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_carbon_dioxide_emissions
(39.1+12.6+19.7+9.3+7.8+7+4.2+3.6+1+1.4+1.1+.7+.6+6.2)/333.29 # 114M = 34.29% population # 2022 https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population
(3598+2053+1033+745+726+688+322+299+162+98+88+71+41+470)/25463 # 40.82% GDP # 2022 https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP
(109*1e6+sum(co2_pop$pop_2023[co2_pop$code %in% c("JPN", "KOR", "NOR", "CHE", "NZL", "CAN", EU28_countries) | co2_pop$npv_pa_gcs_adj >= 0], na.rm = T))/sum(co2_pop$pop_2023, na.rm = T) # 93%


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
sum(co2_pop$adult_2023[co2_pop$code %in% c(EU28_countries, "UKR", "NOR", "BEL", "CHE", "SRB", "ALB", "MAC")]) # 485M incl. 15.6M >1M (3.2%); 1.05M >5M (0.2%)

setNames(co2_pop$adult_2023, co2_pop$country)/sum(co2_pop$adult_2023, na.rm = T)
setNames(ssp2_26$adult_2020, ssp2_26$region)/sum(co2_pop$adult_2020, na.rm = T)
sum(c(ssp2_26$adult_2020[ssp2_26$region %in% c("NAF", "WAF", "EAF", "SAF", "RSAF", "MEX", "BRA", "RCAM", "RSAM", "SEAS", "INDIA", "INDO")]),  + co2_pop$adult_2020[co2_pop$code %in% EU28_countries])/sum(co2_pop$adult_2023, na.rm = T)
# EU28+LAM+SSA+IA+ID+SEAS: 53.6%
# China: 30%, Saudi Arabia + UAE + Afghanistan: 1%, Russia + Iran: 3%

##### Figure GDP pc PPP #####
# Data 2021: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?contextual=default&end=2022&locations=EU-ZG-XD-XM-1W-IN-US-CD-BI-LU-CN-FR&start=2021&view=bar
fig_gdp <- c("Luxembourg" = 133330, "United States" = 69288, "High income" = 54758, "European Union" = 48767, "China" = 19338, "World" = 18605, "India" = 7242, "Sub-Saharan Africa" = 4070, "Low income" = 2124, "Congo Dem. Rep." = 1180, "Burundi" = 775) # 2021
# fig_gdp_w_fr <- c("Luxembourg" = 140614, "United States" = 76327, "High income" = 60806, "European Union" = 54626, "China" = 21483, "World" = 20694, "India" = 8400, "Sub-Saharan Africa" = 4435, "Low income" = 2256, "Congo Dem. Rep." = 1338, "Burundi" = 837) # 2022
names_fig_gdp <- names(fig_gdp)
fig_gdp <- array(fig_gdp/18605, dim = c(1, length(fig_gdp)))
barres(data = fig_gdp, labels = names_fig_gdp, legend = c("GDP"), sort = FALSE, show_ticks = FALSE, showLegend = FALSE, save = T, file = "../figures/policies/GDP_pc_PPP")
barres(data = array(fig_gdp[1,2:10], dim = c(1, 9)), labels = names_fig_gdp[2:10], legend = c("GDP"), sort = FALSE, show_ticks = FALSE, save = T, showLegend = FALSE, file = "../figures/policies/GDP_pc_PPP_few")
fig_gdp_fr <- c("États-Unis" = 76327, "Pays à hauts revenus" = 60806, "France" = 55388, "Union Européenne" = 54626, "Chine" = 21483, "Monde" = 20694, "Inde" = 8400, "Afrique subsaharienne" = 4435, "Pays à bas revenus" = 2256, "Rép. Dém. Congo" = 1338, "Burundi" = 837) # 2022
names_fig_gdp_fr <- names(fig_gdp_fr)
fig_gdp_fr <- array(rev(fig_gdp_fr)/20694, dim = c(1, length(fig_gdp_fr)))
barres(data = fig_gdp_fr, labels = names_fig_gdp_fr, legend = c("PIB"), sort = FALSE, show_ticks = FALSE, showLegend = FALSE, save = T, file = "../figures/policies/GDP_pc_PPP_fr") # 330*344 px
barres(data = array(fig_gdp_fr[1,2:10], dim = c(1, 9)), labels = names_fig_gdp_fr[2:10], legend = c("PIB"), sort = FALSE, show_ticks = FALSE, save = T, showLegend = FALSE, file = "../figures/policies/GDP_pc_PPP_few_fr")

##### Sandbox #####
co2_pop$gain_adj_over_gdp_2030[co2_pop$country == "South Africa"]
co2_pop$gain_adj_2030[co2_pop$country == "South Africa"]/12
co2_pop$npv_over_gdp_gcs_adj[co2_pop$country == "South Africa"]
plot_world_map(paste0("Scautious_gain_adj_over_gdp_2080"), breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", 2080, " (in % of GDP)\nScenario: ", capitalize(gsub("_", " ", "cautious"))), #fill_na = T,
               save = T, na_label = "Non Parties")

for (s in scenarios_names[3]) {
  plot_world_map(paste0("S", s, "_npv_over_gdp_gcs_adj"), breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
                 labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), 
                 legend = paste0("Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan\nScenario: ", capitalize(gsub("_", " ", s))), #fill_na = T, \n(with 4% discount rate)
                 save = F, parties = scenarios_parties[[3]])
}
# setNames(co2_pop$Soptimistic_npv_over_gdp_gcs_adj, co2_pop$code)[grepl("USA", co2_pop$code)]


##### AR6 data #####
ar <- read.csv("../data/AR6/AR6_Scenarios_Database_ISO3_v1.1.csv")
names(ar)
# unique(ar$Variable) # No data on Adult population. We use MER rather than PPP.
ar <- ar[ar$Variable %in% c("Temperature|Global Mean", "Price|Carbon", "Emissions|CO2", "Emissions|CO2|AFOLU", "Population", "GDP|MER", "GDP|PPP"),] # "Emissions|CO2|Energy", "Emissions|CO2|Industrial Processes", "Emissions|CH4", "Emissions|N2O", "Emissions|PFC", "Emissions|SF6"
unique(ar$Model) # /!\ MESSAGE is absent! Why?
unique(ar$Scenario)
length(which(ar$Variable == "Temperature|Global Mean")) # 32
runs <- good_runs <- list()
for (ms in unique(lapply(1:nrow(ar), function(i) {c(ar$Model[i], ar$Scenario[i])}))) {
  name_ms <- paste(ms[1], ms[2])
  runs[[name_ms]] <- c("nb_regions" = length(unique(ar$Region[ar$Model == ms[1] & ar$Scenario == ms[2]])),
                      "nb_years" = sum(!is.na(ar[ar$Variable == "Emissions|CO2" & ar$Model == ms[1] & ar$Scenario == ms[2] & ar$Region == unique(ar$Region[ar$Model == ms[1] & ar$Scenario == ms[2]])[1], which(grepl("X", names(ar)))])),
                      "has_2030_2100" = sum(is.na(ar[ar$Variable == "Emissions|CO2" & ar$Model == ms[1] & ar$Scenario == ms[2], paste0("X", seq(2030, 2100, 10))])) == 0,
                      "has_price" = "Price|Carbon" %in% ar$Variable[ar$Model == ms[1] & ar$Scenario == ms[2]],
                      "has_population" = "Population" %in% ar$Variable[ar$Model == ms[1] & ar$Scenario == ms[2]],
                      "has_gdp" = "GDP|MER" %in% ar$Variable[ar$Model == ms[1] & ar$Scenario == ms[2]],
                      "temperature_2100" = mean(ar$X2100[ar$Variable == "Temperature|Global Mean" & ar$Model == ms[1] & ar$Scenario == ms[2]], na.rm = T))
  runs[[name_ms]]["has_2030_2100"] <- runs[[name_ms]]["has_2030_2100"] * (runs[[name_ms]]["nb_years"] > 0)
  runs[[name_ms]]["has_all"] <- runs[[name_ms]]["has_price"] & runs[[name_ms]]["has_population"] & runs[[name_ms]]["has_gdp"] & runs[[name_ms]]["has_2030_2100"]
  runs[[name_ms]]["has_needed"] <- runs[[name_ms]]["has_price"] & runs[[name_ms]]["has_gdp"] & runs[[name_ms]]["has_2030_2100"]
  if (runs[[name_ms]]["has_needed"]) good_runs[[name_ms]] <- runs[[name_ms]]
}
length(good_runs)
nb_regions <- c()
for (i in names(runs)) nb_regions[i] <- runs[[i]]["nb_regions"]
sort(nb_regions, decreasing = T)[1:100] # IIASAPOP 2.0 (SSP2): 199, ICES-XPS 1.0: 38, GENeSYS-MOD 2.9: 30, NEMESIS 5.0: 30, TIAM-ECN 1.1: 25, GCAM 5.3: 16
runs <- runs[order(nb_regions, decreasing = T)]
runs[1:300] # IIASAPOP: only pop; ICES-XPS: no emissions; GENeSYS-MOD: only EU, no price nor GDP; NEMESIS: only EU until 2050, no pop
# Possibilities:
# TIAM-ECN: 25 regions but no pop nor GDP nor temp => Similar to the SSPs above
# POLES, GEM-E3_V2021: 16 regions, no temp, uniform price, no global coverage (e.g. SSA is missing) => can be used to improve estimates of single country's emissions like >SAU, TUR, >AUS and those of GCAM (though this can be done with the SSP above also, except for the ">" countries)
# GCAM: 16 regions, no price nor temp, no global coverage (e.g. SSA is missing) => can be used to improve estimates of single country's emissions like Korea: >ARG, BRA, CAN, >COL, IDN, IND, JPN, KOR, MEX, >PAK, >TWN unique(ar$Region[ar$Model == "GCAM 5.3" & ar$Scenario == "SSP_SSP2" & ar$Variable == "Emissions|CO2"])
# IMAGE, GEM: 12 regions, no temp
# TIAM-UCL: 8 regions, only with temp, no GDP
names(runs)[grepl("POLES|GEM", names(runs))]
View(ar[ar$Model == "GCAM 5.3" & ar$Scenario == "SSP_SSP2" & ar$Variable == "Emissions|CO2",])


##### Downscaled data from Gütschow et al. (21) #####
# Emissions data (already downscaled) comes from Gütschow et al. (21) while price comes from SSP_CMIP6
# /!\ Problem with this (and all other available) data: emissions are territorial, not footprint.
# /!\ Problem specific with this data: GDP is in PPP, not nominal (though carbon price is nominal)
# Gütschow et al. (21) exclude LULUCF emissions
# TODO!! Run Gidden's R routine for country-downscaling (cf. email, he doesn't want to share the output) https://github.com/iiasa/emissions_downscaling
# TODO! Use Battiston et al. (22) data: it goes only until 2050 but it includes a global carbon price
# Scenario sm should be chosen by default
# TODO! Find nominal GDP estimates => rob.dellink@oecd.org will produce MER GDP by country for the SSPs in August/September
# TODO! Find carbon footprint instead of territorial emissions
# TODO! merge U.S. Dem vs. Non-Dem
# TODO smoothen the carbon price trajectory
# - estimating costs per country of the wealth tax
# - estimating effects on the global income distribution of the GCP (I know how to do these first two points, I just need data that Lucas Chancel will give me once he's back from holidays)
# - making some geopolitical scenarios/simulations linking the cartelization (or not) of oil exporters, the price of oil, long-term purchase agreements of fossil fuels with climate-ambitious countries (to try to make some oil exporters deviate from a potential cartel), and the net gains by country from the GCP. I'll prioritize writing the book to this latter point though.

# ssp_country <- read.csv("../data/PMSSPIE_05Feb20.csv") # Gütschow et al. (21) https://zenodo.org/record/3638137
# unique(ssp_country$scenario) # SSP119IMAGE and SSP226MESGB are the best as they correspond to illustrative marker scenario of respective SSP1 and 2
# unique(paste(ssp_country$entity, ssp_country$unit)) # GDPPPP (M 2011 $ (International, Geary–Khamis)) POP (k) CO2 (Gg = kt OR Mt) CH4 (Gg OR Mt) N2O FGASES FGASESAR4 KYOTOGHG KYOTOGHGAR4
# names(ssp_country)
# ssp_country <- ssp_country %>% .[.$scenario %in% c("SSP119IMAGE", "SSP226MESGB", "SSP119GCAM4", "SSP226AIMCGE", "SSP245MESGB") & .$source != "SSPIAMIE",]
# ssp_country <- write.csv(ssp_country, "../data/PMSSPIE.csv", row.names = FALSE)
ssp_country <- read.csv("../data/PMSSPIE.csv")
# unique(ssp_country$source) # PMSSPIEMISC: gdp, pop; PMSSPIE: emissions; SSPIAMIE: unharmonized (useless)
# unique(ssp_country$country) # 179 + ANNEXI AOSIS BASIC EARTH EU28 LDC NONANNEXI UMBRELLA MAC
# unique(ssp_country$category) # ECO DEMOGR IPCM0EL
# unique(paste(ssp_country$entity, ssp_country$unit)) # GDPPPP (M 2011 $ (International, Geary–Khamis)) POP (k) CO2 (Gg = kt CO2(eq), same for all gases) CH4 N2O FGASES FGASESAR4 KYOTOGHG KYOTOGHGAR4
ssp_country <- ssp_country %>% .[!.$country %in% c("EARTH", "ANNEXI", "AOSIS", "BASIC", "EU28", "LDC", "NONANNEXI", "UMBRELLA", "MAC"),]
# Missing countries: present in co2_pop: SWZ (Eswatini), PSE (Palestine); not in co2_pop: XXK (Kosovo), ESH (Western Sahara)

# possible_scenarios <- ssp_country[grepl("SSP119GCAM4|SSP226MESGB|SSP119IMAGE|SSP226AIMCGE", ssp_country$scenario) & ssp_country$source != "SSPIAMIE" & ssp_country$entity == "CO2" & ssp_country$country == "EARTH", c("scenario", paste0("X", years))]
# possible_scenarios <- rbind(possible_scenarios, c("ssp2_26", world_emissions$ssp2_26/1e3), c("ssp1_26", world_emissions$ssp1_26/1e3), c("ssp1_19", world_emissions$ssp1_19/1e3), c("ssp2_26msg", world_emissions$ssp2_26msg/1e3))
# rownames(possible_scenarios) <- possible_scenarios$scenario
# possible_scenarios <- as.matrix(possible_scenarios[,-1])
# class(possible_scenarios) <- "numeric"
# matplot(x = years, y = t(possible_scenarios/1e6),  type = c("l"), lty = 1, lwd = c(rep(1, nrow(possible_scenarios)-4), rep(3,4)), col = 1:nrow(possible_scenarios))
# grid()
# legend("bottomleft", legend = row.names(possible_scenarios), lty = 1, lwd = c(rep(1, nrow(possible_scenarios)-4), rep(3,4)), col=1:nrow(possible_scenarios))
# # best matches (1st best 1st): ssp1_19,SSP119IMAGE > ssp2_26msg,SSP119GCAM4 > ssp1_26,SSP226MESGB > ssp1_26,SSP119GCAM4 > ssp2_26,SSP226MESGB > ssp2_26,SSP226AIMCGE
# # highest prices (taking 2040 as ex): ssp1_19 550 > ssp2_26 190 > ssp2_26msg 50 ~ ssp1_26 70
# # 3 scenarios: high prices - high ambition: ssp1_19,SSP119IMAGE; medium price - medium ambition: ssp2_26,SSP226AIMCGE; low price - medium ambition: ssp2_26msg,SSP119GCAM4 (or ssp1_26,SSP226MESGB but worse match, or ssp1_26,SSP126REMMP to get same SSP but even worse match) 
# # Why same SSP don't have same emissions trajectories? e.g. ssp2_26 (image) always has lower emissions than ssp2_26msg (which has emissions similar to SSP119GCAM4). 
# # => See WA conv with Thomas Bossy: The different IAMs don't agree on absorption/decay of CO2 (hence divergences in emisssions for a given concentration pathway) + differ on LULUCF emissions.

prepare_ssp_country <- function(scenario = "SSP226MESGB", ssps = ssp_country, df = co2_pop, keep_from_df = copy_from_co2_pop) { # GDP is in PPP
  # TODO! Pb: scenarios of GDP pc are unrealistic: 14% annual growth in DRC over 2019-30 (25% over 2019-23 and 8% over 2023-30), 7% over 2030-40, 6% 2040-50. => Underestimation of gains over GDP in low-income countries.
  # TODO! It's because 2019 is in nominal and >=2020 in PPP, no?
  # Uses country, country_map and adult_ from co2_pop
  # TODO!!: streamline creation of co2_pop for this purpose, perhaps also keeping emissions_baseline_2030, rci_2030, territorial_2019, footprint_2019, missing_footprint, gdp_pc_2019 (not PPP), share_territorial_2019, median_gain_2015, mean_gain_2030, gdp_ppp_now,gdr_pa_2030_cerc, gdr_pa_2030 
  # TODO: streamline fetching of carbon_price
  # sum(is.na(ssp2_26_country)) # 1134
  # sum(is.na(ssp2_26_country[!ssp2_26_country$code %in% c("SSD", "TWN", "PRK", "FSM"),])) # 0
  # setdiff(co2_pop$code, ssp2_26_country$code) # small islands and small countries, Palestine
  # setdiff(ssp2_26_country$code, co2_pop$code) # Taiwan, Hong Kong
  # Add Taiwan data
  if (exists("pop_iso3") & !"TWN" %in% df$code) {
    twn <- df[df$code == "KOR",]
    twn$code <- "TWN"
    twn$country <- twn$country_map <- "Taiwan"
    for (y in c(2023, seq(2020, 2100, 10))) { 
      twn[[paste0("pop_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$pop[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$pop[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
      twn[[paste0("adult_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$adult[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$adult[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
    } 
    df <- rbind(df, twn)
  }
  ssps <- ssps[ssps$country %in% df$code & !ssps$country %in% c("FSM", "GRD"),] # "SSD", "TWN", "PRK", 
  ssp <- data.frame(code = unique(ssps$country))
  for (y in 2020:2100) { # Years span 1850:2100
    ssp[[paste0("pop_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "POP"], ssps$country[ssps$scenario == scenario & ssps$entity == "POP"])[ssp$code]
    ssp[[paste0("gdp_", y)]] <- 1e6 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "GDPPPP"], ssps$country[ssps$scenario == scenario & ssps$entity == "GDPPPP"])[ssp$code]  # /!\ It is in PPP (contrary to old code with IIASA SSPs)
    ssp[[paste0("emissions_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "CO2"], ssps$country[ssps$scenario == scenario & ssps$entity == "CO2"])[ssp$code]
    # Add North Korea data
    ssp[[paste0("emissions_", y)]][ssp$code == "PRK"] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("emissions_", y - y %% 10)]][df$code == "PRK"], df[[paste0("emissions_", 10*ceiling(y/10))]][df$code == "PRK"])
    ssp[[paste0("gdp_", y)]][ssp$code == "PRK"] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("gdp_", y - y %% 10)]][df$code == "PRK"], df[[paste0("gdp_", 10*ceiling(y/10))]][df$code == "PRK"])
    # Add South Sudan and Taiwan data
    for (c in c("TWN", "SSD")) ssp[[paste0("pop_", y)]][ssp$code == c] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("pop_", y - y %% 10)]][df$code == c], df[[paste0("pop_", 10*ceiling(y/10))]][df$code == c])
  } # Scales up df$adult by ssp$pop/df$pop
  for (y in c(2023, seq(2020, 2100, 10))) ssp[[paste0("adult_", y)]][match.nona(df$code, ssp$code)] <- ssp[[paste0("pop_", y)]][match.nona(df$code, ssp$code)] * (df[[paste0("adult_", y)]]/df[[paste0("pop_", y)]])[df$code %in% ssp$code]
  for (y in 2020:2100) { # Interpolate adult_ from pop_ and df$adult/df$pop
    y_prev <- 10*floor(y/10)
    y_next <- 10*ceiling(y/10)
    if (y > 2023 & y < 2030) y_prev <- 2023
    if (y %in% c(2021, 2022)) y_next <- 2023
    lambda <- (y - y_prev)/10
    ssp[[paste0("adult_", y)]] <- ssp[[paste0("pop_", y)]] * ((1 - lambda) * (ssp[[paste0("adult_", y_prev)]]/ssp[[paste0("pop_", y_prev)]]) + lambda * (ssp[[paste0("adult_", y_next)]]/ssp[[paste0("pop_", y_next)]]))
    ssp[[paste0("emissions_pa_", y)]] <- ssp[[paste0("emissions_", y)]]/ssp[[paste0("adult_", y)]]
    ssp[[paste0("emissions_pc_", y)]] <- ssp[[paste0("emissions_", y)]]/ ssp[[paste0("pop_", y)]]
    ssp[[paste0("gdp_pa_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("adult_", y)]] # /!\ It is in PPP (contrary to old code with IIASA SSPs)
    ssp[[paste0("gdp_pc_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("pop_", y)]] # /!\ It is in PPP (contrary to old code with IIASA SSPs)
    ssp[[paste0("gdp_pc_over_mean_", y)]] <- ssp[[paste0("gdp_pc_", y)]]/wtd.mean(ssp[[paste0("gdp_pc_", y)]], ssp[[paste0("pop_", y)]], na.rm = T)
  }
  ssp$gdp_ppp_now <- ssp$gdp_ppp_2023 # TODO? add carbon_price_?
  ssp$gdp_pc_base_year <- ssp$gdp_ppp_pc_2023
  for (v in intersect(keep_from_df, names(df))) ssp[[v]][match.nona(df$code, ssp$code)] <- df[[v]][df$code %in% ssp$code]
  return(ssp)
}

compute_npv <- function(var = "gain_pa_", discount_rate = .03, start = 2030, end = 2100, data = co2_pop, decadal = FALSE) {
  # /!\ If decadal == T, NPV is computed on 2020-2100. TODO!? Compute it on 2030-2080 (this would make India neutral in Generous EU as the positive part comes from 2020).
  if (decadal) return(rowSums(sapply(2:10, function(i) { return(10*data[[paste0(var, 2000+10*i)]]/((1+discount_rate)^10)^(i-2)) })))
  else return(rowSums(sapply(start:end, function(i) { return(data[[paste0(var, i)]]/(1+discount_rate)^(i-start)) })))
}
compute_gain_given_parties <- function(parties = df$code, df = sm, return = "df", discount = .03, ssp_name = "ssp2_26_country", start = 2025, end = 2100, linear_downscaling = FALSE, opt_out_at_start = F, antiredistr_prevention = T, middle_losing_prevention = T) {
  # Uses large_footprint_, optout_right_, revenues_pa_, adult_, gdp_pc_, pop_, pop_, emissions_pa_, carbon_price[[ssp_name]]
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
  basic_income <- basic_income_adj <- share_pooled <- total_revenues <- mean_revenues <- basic_income_over_mean_revenues <- c()
  for (y in start:end) { # Iteratively find out how much each country opts out
    yr <- as.character(y)
    # Countries whose GDP pc is below the mean can choose to get the basic income or the mean_revenues multiplied by footprint_over_mean_start, they take whichever is higher
    # More generally, countries that are not high-income have an income equal to the maximum between the basic_income and a barycenter of their optout_right * mean_revenues * footprint_over_mean_start and the basic_income
    total_revenues[yr] <- sum(df[[paste0("revenues_pa_", y)]] * df[[paste0("adult_", y)]] * (df$code %in% parties), na.rm = T)
    mean_revenues[yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("adult_", y)]] * (df$code %in% parties), na.rm = T)
    # df$footprint_over_mean_start <- (df$footprint_2019/df$adult_2020)/(sum(df$footprint_2019, na.rm = T)/sum(df$adult_2020))
    # df$footprint_over_mean_start[is.na(df$footprint_over_mean_start)] <- (df$emissions_pa_2020/(sum(df$emissions_2020, na.rm = T)/sum(df$adult_2020)))[is.na(df$footprint_over_mean_start)]
    df$footprint_over_mean_start <- (df[[paste0("emissions_pa_", start)]]/(sum(df[[paste0("emissions_", start)]], na.rm = T)/sum(df[[paste0("adult_", start)]]))) # TODO this hypothesis or the above?
    # df$intensity_over_mean_start <- ((df$emissions_2025/df$gdp_2025)/(sum(df$emissions_2025, na.rm = T)/sum(df$gdp_2025))) # TODO this hypothesis or the above?
    if (opt_out_at_start) { # Opt-out defined in function of excess emissions at t=0
      if (antiredistr_prevention) {
        y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], (df$code %in% parties) * df[[paste0("pop_", y)]])
        e_bar <- wtd.mean(df[[paste0("emissions_pa_", y)]], df$code %in% parties * df[[paste0("adult_", y)]])
        lambda <- pmax(0, pmin(1, (2.2*y_bar - df[[paste0("gdp_pc_", y)]])/((2.2-2)*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
        lambda[is.na(lambda)] <- 1
        df[[paste0("share_basic_income_", y)]] <- (df$code %in% parties) * (lambda + pmin(1, df[[paste0("emissions_pa_", y)]]/(1.3*e_bar))*(1-lambda))
      } else df[[paste0("share_basic_income_", y)]] <- 1

      if (mean_revenues[yr] > 0) {
        temp <- mean_revenues[yr] 
        df[[paste0("income_pa_", y)]] <- pmax(temp, (df[[paste0("optout_right_", y)]] * mean_revenues[yr] * df$footprint_over_mean_start + (1 - df[[paste0("optout_right_", y)]]) * temp * df[[paste0("share_basic_income_", y)]]) * (df$code %in% parties)) # TODO? shouldn't df$code %in% parties apply to the whole?
        # df[[paste0("income_pa_", y)]] <- pmax(temp, (df[[paste0("optout_right_", y)]] * mean_revenues[yr] * df$intensity_over_mean_start * (sum(df[[paste0("emissions_", y)]], na.rm = T)/sum(df[[paste0("gdp_", y)]])) * df[[paste0("gdp_pa_", y)]] + (1 - df[[paste0("optout_right_", y)]]) * temp * df[[paste0("share_basic_income_", y)]]) * (df$code %in% parties))
        basic_income[yr] <- mean_revenues[yr] / wtd.mean(df[[paste0("income_pa_", y)]]/mean_revenues[yr], df[[paste0("adult_", y)]], na.rm = T)
        while (max_gap(temp, basic_income[yr]) > 1e-6) {# TODO! include the mechanism preventing anti-redistribution at this stage
          df[[paste0("income_pa_", y)]] <- pmax(basic_income[yr], (df[[paste0("optout_right_", y)]] * mean_revenues[yr] * df$footprint_over_mean_start + (1 - df[[paste0("optout_right_", y)]]) * basic_income[yr]) * (df$code %in% parties))
          temp <- basic_income[yr]
          basic_income[yr] <- mean_revenues[yr] / wtd.mean(df[[paste0("income_pa_", y)]]/mean_revenues[yr], df[[paste0("adult_", y)]], na.rm = T)
        }
      } else {
        df[[paste0("income_pa_", y)]] <- 0
        basic_income[yr] <- 0
      }
      basic_income_adj[yr] <- basic_income[yr]
      # df[[paste0("participation_rate_", y)]] <- 1 
      if (!middle_losing_prevention) df[[paste0("income_pa_", y)]] <- df[[paste0("share_basic_income_", y)]] * basic_income[yr]
      df[[paste0("gain_adj_", y)]] <- df[[paste0("income_pa_", y)]] - df[[paste0("revenues_pa_", y)]]
    } else {
      df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
      large_footprint_previous_iter <- rep(T, nrow(df)) # df$code %in% parties # average_revenues is average emissions_pa * carbon_price while basic_income is adjusted for participation_rate due to opt-out and anti-regressive mechanism
      while (any((large_footprint_previous_iter != df[[paste0("large_footprint_", y)]])[!is.na(df[[paste0("large_footprint_", y)]])])) {
        large_footprint_previous_iter <- df[[paste0("large_footprint_", y)]]
        basic_income[yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
        df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > basic_income[yr])
        df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
      }
      if (!middle_losing_prevention) df[[paste0("participation_rate_", y)]] <- 1
      df[[paste0("gain_optout_", y)]] <- df[[paste0("participation_rate_", y)]] * (basic_income[yr] - df[[paste0("revenues_pa_", y)]]) 
      
      if (antiredistr_prevention) { # Adjusted to avoid high-income receiving money. Pb: GDP in PPP of Europe is not more than twice the world average 2050-2070.
        # This has only minor effects, with only Sweden, Switerland and France being concerned at some point, and they always receive >60% of the basic income.
        y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]])
        e_bar <- wtd.mean(df[[paste0("emissions_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
        lambda <- pmax(0, pmin(1, (2.2*y_bar - df[[paste0("gdp_pc_", y)]])/((2.2-2)*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
        lambda[is.na(lambda)] <- 1
        df[[paste0("share_basic_income_", y)]] <- (lambda + pmin(1, df[[paste0("emissions_pa_", y)]]/(1.3*e_bar))*(1-lambda))
        lower_basic_income_ctries <- lambda < 1 & no.na(df[[paste0("emissions_pa_", y)]] < 1.3*e_bar, rep = FALSE, num_as_char = FALSE)
        if (y %% 10 == 0) df[[paste0("lower_basic_income_", y)]] <- 1*lower_basic_income_ctries
        basic_income_adj[yr] <- basic_income[yr] * (1 + sum((1 - df[[paste0("share_basic_income_", y)]]) * df[[paste0("adult_", y)]] * df[[paste0("participation_rate_", y)]] * lower_basic_income_ctries) / sum(df[[paste0("adult_", y)]] * df[[paste0("participation_rate_", y)]] * !lower_basic_income_ctries))
        df[[paste0("gain_adj_", y)]][lower_basic_income_ctries] <- (df[[paste0("participation_rate_", y)]] * (basic_income[yr] * df[[paste0("share_basic_income_", y)]] - df[[paste0("revenues_pa_", y)]]))[lower_basic_income_ctries]
        df[[paste0("gain_adj_", y)]][!lower_basic_income_ctries] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pa_", y)]]))[!lower_basic_income_ctries]
      } else df[[paste0("gain_adj_", y)]] <- (basic_income[yr] - df[[paste0("revenues_pa_", y)]]) * df[[paste0("participation_rate_", y)]] # TODO!
    }
    
    df[[paste0("gain_adj_over_gdp_", y)]] <- df[[paste0("gain_adj_", y)]]/df[[paste0("gdp_pa_", y)]]
    
    df[[paste0("share_revenues_lost_", y)]] <- ifelse(df[[paste0("revenues_pa_", y)]] > 0, pmax(0, (df[[paste0("revenues_pa_", y)]] - basic_income_adj[yr])/df[[paste0("revenues_pa_", y)]]), 0)
    df[[paste0("share_basic_income_collected_", y)]] <- df[[paste0("revenues_pa_", y)]]/basic_income_adj[yr]
    df[[paste0("basic_income_over_revenues_", y)]] <- basic_income_adj[yr]/df[[paste0("revenues_pa_", y)]]
    # Alternative policy, called "gcs_pool", where all losers keep the same proportion of revenues they collect
    share_pooled[yr] <- sum(((basic_income_adj[yr] - df[[paste0("revenues_pa_", y)]]) * df[[paste0("adult_", y)]])[df[[paste0("gain_adj_", y)]] > 0])/(sum((df[[paste0("revenues_pa_", y)]] * df[[paste0("adult_", y)]])[df[[paste0("gain_adj_", y)]] < 0]) + 1e10) # wtd.mean(df[[paste0("share_revenues_lost_", y)]], df[[paste0("adult_", y)]])
    df[[paste0("gain_pool_", y)]] <- (df[[paste0("gain_adj_", y)]] >= 0) * df[[paste0("gain_adj_", y)]] - (df[[paste0("gain_adj_", y)]] < 0) * share_pooled[yr] * df[[paste0("revenues_pa_", y)]]
    df[[paste0("gain_pool_over_gdp_", y)]] <- df[[paste0("gain_pool_", y)]]/df[[paste0("gdp_pa_", y)]]
  }

  # GDR: find emissions allocations on website and allocate total_revenues[[ssp_name]][yr]. They go only until 2030. Either I recover the GDRs from them (or their code) and apply them here, or I add the per-capita allocation to their code.
  df$gain_gdr_2030 <- (carbon_price[[ssp_name]][["2030"]] * df$gdr_pa_2030  - df$revenues_pa_2030)
  df$gain_gdr_over_gdp_2030 <- df$gain_gdr_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_2030 <- df$gain_gdr_2030 - df$gain_pa_2030
  df$diff_gain_gdr_gcs_over_gdp_2030 <- df$diff_gain_gdr_gcs_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_adj_2030 <- df$gain_gdr_2030 - df$gain_adj_2030
  df$diff_gain_gdr_gcs_adj_over_gdp_2030 <- df$diff_gain_gdr_gcs_adj_2030/df$gdp_pa_2030
  
  df$npv_pa_gcs <- compute_npv("gain_pa_", discount = discount, data = df, decadal = linear_downscaling)
  df$npv_pa_gcs_adj <- compute_npv("gain_adj_", discount = discount, data = df, decadal = linear_downscaling)
  df$npv_over_gdp_gcs <- df$npv_pa_gcs/compute_npv("gdp_pa_", discount = discount, data = df, decadal = linear_downscaling) # this formula corresponds to the % loss in consumption computed in Balanced Growth Equivalent of Stern et al. (07)
  df$npv_over_gdp_gcs_adj <- df$npv_pa_gcs_adj/compute_npv("gdp_pa_", discount = discount, data = df, decadal = linear_downscaling)
  df$npv_over_gdp_gcs_pool <- compute_npv("gain_pool_", discount = discount, data = df, decadal = linear_downscaling)/compute_npv("gdp_pa_", discount = discount, data = df, decadal = linear_downscaling)
  df$diff_npv_over_gdp_pool__adj <- df$npv_over_gdp_gcs_pool - df$npv_over_gdp_gcs_adj
  
  return(df)
}

total_revenues <- average_revenues <- average_revenues_bis <- basic_income <- basic_income_adj <- share_pooled <- list()
create_var_ssp <- function(ssp = NULL, df = sm, CC_convergence = 2040, discount = .03, opt_out_threshold = 1.5, full_part_threshold = 2, scenario = "all_countries", base_year_downscaling = NULL) { # message is only for ssp2 , region = message_region_by_code
  linear_downscaling <- !is.null(base_year_downscaling)
  name_df <- deparse(substitute(df))
  years <- if (linear_downscaling) c(2005, seq(2010, 2100, 10)) else 2020:2100
  if (is.null(ssp)) { 
    ssp_name <- if (name_df %in% c("s1", "sh")) "ssp1_19" else { if (name_df %in% c("s3", "sl")) "ssp2_26msg" else "ssp2_26" } 
  } else ssp_name <- deparse(substitute(ssp))
  if (!name_df %in% c("co2_pop", "s1", "s2", "s3", "sh", "sm", "sl") & is.null(ssp_name)) warning("ssp is not given, ssp2_26 assumed.")
  total_revenues[[ssp_name]] <- average_revenues[[ssp_name]] <- basic_income[[ssp_name]] <- basic_income_adj[[ssp_name]] <- c()
  
  if (!exists("scenarios_parties") & scenario == "all_countries") parties <- df$code
  else parties <- scenarios_parties[[scenario]]
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
  # split USA for scenario == "optimistic" into Dem USA (the 12 States + DC with Democratic lead > 10 pp) and Non-Dem USA
  if ("Dem USA" %in% parties) for (y in years) for (v in paste0(c("pop_", "adult_"), y)) df[[v]][df$code == "USA"] <- .3429 * df[[v]][df$code == "USA"]

  # Country downscaling (Legacy from old code using IIASA SSPs instead of downscaled dataset from Gütschow et al. 21)
  if (linear_downscaling) {
    if ("Dem USA" %in% parties) for (v in c("gdp_pc_2019", "GDPpcPPP")) df[[v]][df$code == "USA"] <- (.4082/.3429) * df[[v]][df$code == "USA"]
    if ("Dem USA" %in% parties) df[[paste0("emissions_pa_", base_year_downscaling)]][df$code == "USA"] <- (.2149/.3429) * df[[paste0("emissions_pa_", base_year_downscaling)]][df$code == "USA"]
    
    if (grepl("ssp1", ssp_name)) model <- "IMAGE"
    else if (ssp_name %in% c("ssp2", "ssp2_26_country") | grepl("gea", ssp_name)) model <- "MESSAGE"
    else model <- "big"
    # Dirty fix for unrealistically high projections of GDP pc for middle-income African countries: we assign them to China region, which has a comparable GDP pc, so the projection of GDP pc are more credible
    # TODO? Make our own projections for all countries, grouping countries based on GDP pc and carbon footprint rather than geography, and deriving projections by group from SSPs or GEA macro-regions.
    recoded_countries <- c("BWA", "GAB", "GNQ", "ZAF", "NAM")
    if (grepl("gea", ssp_name)) message_region_by_code_original <- message_region_by_code
    if (grepl("gea", ssp_name)) message_region_by_code[recoded_countries] <- "MEA" # c("Botswana", "Gabon", "Equatorial Guinea", "South Africa", "Namibia)
    region <- if (model == "big") big_region_by_code else { if (model == "IMAGE") image_region_by_code else message_region_by_code }
    regions <- if (model == "big") big_regions else { if (model == "IMAGE") image_regions else message_regions }
    
    for (y in years) {
      yr <- as.character(y)
      for (v in paste0(c("pop_", "adult_", "emissions_", "gdp_"), y)) if (!v %in% names(df)) df[[v]] <- NA
      if (!paste0("gdp_", y) %in% names(ssp)) { # Outdated: gdp_ is in ssp (in PPP)
        if (paste0("gdp_mer_", y) %in% names(ssp)) { # MER used in gea_gea (deprecated)
          ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_mer_", y)]] 
          df$gdp_pc_base_year_downscaling <- df$gdp_pc_2019 # manage missing values (Venezuela, Yemen, South Sudan, North Korea, Eritrea, fix Western Sahara)
        } else { 
          ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_ppp_", y)]] # PPP used in Gütschow (the default)
          df$gdp_pc_base_year_downscaling <- df$GDPpcPPP  # TODO: manage missing values (Saudi Arabia, Afghanistan, New Zealand, Cambodia...)
        }
      } else df$gdp_pc_base_year_downscaling <- df$GDPpcPPP
      if (grepl("gea", ssp_name)) {
        for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "MEA"] <- ssp[[v]][ssp$region == "MEA"] * (1 + sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
        for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "AFR"] <- ssp[[v]][ssp$region == "AFR"] * (1 - sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
        for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "MEA"] <- ssp[[paste0(v, y)]][ssp$region == "MEA"] * (1 + sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
        for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "AFR"] <- ssp[[paste0(v, y)]][ssp$region == "AFR"] * (1 - sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
      }
      for (r in regions) { # Country downscaling
        region_r <- region[df$code] == r # df$code %in% region[r] 
        # /!\ The line below overwrites UN's pop projections
        for (v in paste0(c("pop_", "adult_"), y)) df[[v]][region_r] <- df[[v]][region_r] * ssp[[v]][ssp$region == r] / sum(df[[v]][region_r], na.rm = T)
        reduction_factor_ry <- ssp[[paste0("emissions_", y)]][ssp$region == r]/sum((df[[paste0("emissions_pa_", base_year_downscaling)]] * df[[paste0("adult_", y)]])[region_r])
        df[[paste0("emissions_pa_", y)]][region_r] <- reduction_factor_ry * df[[paste0("emissions_pa_", base_year_downscaling)]][region_r] # df$emissions_2019[region_r] * ssp[[paste0("emissions_", y)]][ssp$region == r] / sum(df$emissions_2019[region_r], na.rm = T)
        df[[paste0("emissions_", y)]][region_r] <- (df[[paste0("emissions_pa_", y)]] * df[[paste0("adult_", y)]])[region_r]
        growth_factor_ry <- ssp[[paste0("gdp_", y)]][ssp$region == r]/sum((df$gdp_pc_base_year_downscaling * df[[paste0("pop_", y)]])[region_r], na.rm = T)
        df[[paste0("gdp_pc_", y)]][region_r] <- growth_factor_ry * df$gdp_pc_base_year_downscaling[region_r]
        df[[paste0("gdp_", y)]][region_r] <- (df[[paste0("gdp_pc_", y)]] * df[[paste0("pop_", y)]])[region_r] # df$gdp_ppp_now[region_r] * ssp[[paste0("gdp_ppp_", y)]][ssp$region == r] / sum(df$gdp_ppp_now[region_r], na.rm = T)
      } 
      df[[paste0("gdp_pc_over_mean_", y)]] <- df[[paste0("gdp_pc_", y)]]/wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]])
      df[[paste0("gdp_pa_", y)]] <- df[[paste0("gdp_", y)]]/df[[paste0("adult_", y)]]
      df[[paste0("emissions_pc_", y)]] <- df[[paste0("emissions_", y)]]/df[[paste0("pop_", y)]]
    }
  } 
  
  for (y in years) { # Unadjusted mean gain pa
    yr <- as.character(y)
    if (!linear_downscaling | y %% 10 == 0) {
      y_prev <- as.character(10*floor(y/10))
      y_next <- as.character(10*ceiling(y/10))
      lambda <- (y - 10*floor(y/10))/10
      carbon_price[[ssp_name]][yr] <- (1 - lambda) * carbon_price[[ssp_name]][y_prev] + lambda * carbon_price[[ssp_name]][y_next]
      df[[paste0("revenues_pa_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pa_", y)]]) # /12
      total_revenues[[ssp_name]][yr] <- carbon_price[[ssp_name]][yr] * sum(df[[paste0("emissions_", y)]], na.rm = T) # ssp[[paste0("emissions_", y)]][ssp$region == "world"]
      if (total_revenues[[ssp_name]][yr] < 0) df[[paste0("revenues_pa_", y)]] <- 0
      
      # GCS
      df[[paste0("gain_pa_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0("adult_", y)]], na.rm = T) - df[[paste0("revenues_pa_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
      df[[paste0("gain_over_gdp_", y)]] <- df[[paste0("gain_pa_", y)]]/df[[paste0("gdp_pc_", y)]]    
      # Adjusted for opt out
      df[[paste0("optout_right_", y)]] <- (full_part_threshold - pmax(opt_out_threshold, pmin(full_part_threshold, df[[paste0("gdp_pc_", y)]] / wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]]))))/(full_part_threshold - opt_out_threshold)
      # Accounts for non-universal participation
      average_revenues[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("adult_", y)]])
      df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > average_revenues[[ssp_name]][yr])
      
      # C&C: define climate debt/credit until convergence date TODO!
    }   
  }
  
  df_parties <- compute_gain_given_parties(parties, df = df, return = "df", ssp_name = ssp_name, discount = discount, linear_downscaling = linear_downscaling)
  for (y in years[years >= 2020]) {
    yr <- as.character(y) # TODO!! in some cases, participation_rate not defined
    basic_income[[ssp_name]][yr] <- wtd.mean(df_parties[[paste0("revenues_pa_", y)]], df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("adult_", y)]])
    # basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + wtd.mean(df_parties[[paste0("participation_rate_", y)]] - df_parties[[paste0("share_basic_income_", y)]], df_parties[[paste0("adult_", y)]]))
    if (paste0("lower_basic_income_", y) %in% names(df_parties)) basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + sum((1 - df_parties[[paste0("share_basic_income_", y)]]) * df_parties[[paste0("adult_", y)]] * df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("lower_basic_income_", y)]]) / sum(df_parties[[paste0("adult_", y)]] * df_parties[[paste0("participation_rate_", y)]] * (1 - df_parties[[paste0("lower_basic_income_", y)]]))) 
    share_pooled[[ssp_name]][yr] <- sum(((basic_income_adj[[ssp_name]][yr] - df_parties[[paste0("revenues_pa_", y)]]) * df_parties[[paste0("adult_", y)]])[df_parties[[paste0("gain_adj_", y)]] > 0])/sum((df_parties[[paste0("revenues_pa_", y)]] * df_parties[[paste0("adult_", y)]])[df_parties[[paste0("gain_adj_", y)]] < 0]) # wtd.mean(df_parties[[paste0("share_revenues_lost_", y)]], df_parties[[paste0("adult_", y)]])
  }
  # TODO!! remplace ssp_name by df name; streamline fetch of carbon_price; check trajectories with sf
  
  total_revenues[[ssp_name]] <<- total_revenues[[ssp_name]]
  average_revenues[[ssp_name]] <<- average_revenues[[ssp_name]]
  carbon_price[[ssp_name]] <<- carbon_price[[ssp_name]] # carbon_price is just completed between decadal years by interpolation
  
  if (length(setdiff(df$code[!df$code %in% c("ABW", "HKG", "MDV", "MUS")], parties)) == 0) { # , "TWN
    basic_income[[name_df]] <<- basic_income[[ssp_name]] <<- basic_income[[ssp_name]] 
    basic_income_adj[[name_df]] <<- basic_income_adj[[ssp_name]] <<- basic_income_adj[[ssp_name]]
    share_pooled[[name_df]] <<- share_pooled[[ssp_name]] <<- share_pooled[[ssp_name]]
    df <- df_parties
  } else {
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj|^diff_gain_gdr_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]] <- df_parties[[v]]
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]][!df$code %in% parties] <- NA
  }
  basic_income[[scenario]] <<- basic_income[[ssp_name]]
  basic_income_adj[[scenario]] <<- basic_income_adj[[ssp_name]]
  share_pooled[[scenario]] <<- share_pooled[[ssp_name]]
  return(df)
}
# sm <- create_var_ssp(df = sm) 

## DEPRECATED: use GCP_gain_by_country ##
# /!\ There are small inconsistencies with pop_ from GCP_gain_by_country because here we call gea_gea to define pop_
co2_pop.bak2 <- co2_pop
copy_from_co2_pop <- c("country", "country_map", "gdr_pa_2030", # These three are absolutely needed 
                       "emissions_baseline_2030", "rci_2030", "territorial_2019", "footprint_2019", "missing_footprint", "gdp_pc_2019", "share_territorial_2019", "median_gain_2015", "mean_gain_2030", "gdp_ppp_now", "gdr_pa_2030_cerc")
sh <- prepare_ssp_country("SSP119IMAGE") # SSP1-1.9, sh, temp max: 1.6°C, temp 2100: 1.4°C https://www.carbone4.com/publication-scenarios-ssp-adaptation
sf <- prepare_ssp_country("SSP226AIMCGE") # best fit for high prices (incidentally, China wins in this scenario)
sl <- prepare_ssp_country("SSP119GCAM4")  # SSP1-1.9, sl: scenario low price
sm <- prepare_ssp_country("SSP226MESGB") # SSP2-2.6, sm, temp max: 1.8°C, temp 2100: 1.8°C
# sm <- prepare_ssp_country("SSP226MESGB", df = df) 
sm <- create_var_ssp(df = sm) # medium price - medium ambition. Illustrative pathway ssp2_26, SSP226MESGB
sh <- create_var_ssp(df = sh) # high prices - high ambition: ssp1_19 (price), SSP119IMAGE (emissions)
sf <- create_var_ssp(df = sf) # medium price - medium ambition. ssp2_26, SSP226AIMCGE best match for emissions with medium price trajectory ssp2_26
sl <- create_var_ssp(df = sl) # low price - medium ambition: ssp2_26msg, SSP119GCAM4 (alternative: ssp1_26,SSP226MESGB but worse match for emissions, or ssp1_26,SSP126REMMP to get same SSP but even worse match) 
# sm: >China neutral<, increasing basic income ~50$ (until 2060) / sf: China winner, plateau of emissions ~40$ (until 2060), lower gains/losses, better fit with price and no problem
# 
# sum(sm$gain_adj_2030 * sm$adult_2030, na.rm = T)
# sum(df$gain_adj_2030 * df$adult_2030, na.rm = T)
# sum(df$Scentral_gain_adj_2030 * df$adult_2030, na.rm = T)
sm$gain_adj_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]/12
df$gain_adj_2030[df$code %in% c("CHN", "FRA", "IND", "USA")]/12
sm$gain_adj_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]*euro_per_dollar/12
df$Scentral_gain_adj_2030[df$code %in% c("CHN", "FRA", "IND", "USA")]/12
df$Sprudent_gain_adj_2030[df$code %in% c("CHN", "FRA", "IND", "USA")]/12
# df$emissions_pa_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# wtd.mean(df$emissions_pa_2030, df$adult_2030 * df$participation_rate_2030 * df$code %in% central)
# # df$Scentral_npv_pa_gcs_adj[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# # df$Scentral_npv_over_gdp_gcs_adj[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# sm$participation_rate_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# sm$share_basic_income_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# sm$gdp_pc_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]/y_bar
# lambda[sm$code %in% c("CHN", "FRA", "IND", "USA")]
# wtd.mean(sm$emissions_pa_2030, sm$adult_2030 * sm$code %in% central)

sm$income_pa_2030[sm$code %in% c("CHN", "FRA", "IND", "USA")]

share_pooled # 50-60%
sort(setNames(sm$npv_over_gdp_gcs_adj, sm$code))
sort(setNames(sm$npv_over_gdp_gcs_pool, sm$code))
# Total revenues: 2.5% world GDP; International transfers: 0.65% world GDP
total_revenues$ssp2_26[["2030"]]/sum(sm$gdp_2030, na.rm = T) 
sum((sm$gain_adj_2030 * sm$adult_2030)[sm$gain_adj_2030 > 0], na.rm = T)/sum(sm$gdp_2030, na.rm = T)
sum((sf$gain_adj_2030 * sf$adult_2030)[sf$gain_adj_2030 > 0], na.rm = T)/sum(sf$gdp_2030, na.rm = T)


plot_world_map("npv_over_gdp_gcs_adj", df = sm, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), # .003, .01, .03
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs_pool", df = sm, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), # .003, .01, .03
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("diff_npv_over_gdp_pool__adj", df = sm, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), # .003, .01, .03
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Pool", #fill_na = T, \n(with 4% discount rate)
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_pa_gcs_adj", df = sm, breaks = c(-Inf, -30000, -10000, -1000, -1e-4, 0, 1000, 5000, 10000, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -30000, -10000, -1000, 0, 0, 1000, 5000, 10000, Inf), sep = " to ", return = "levels")), 
               legend = "Net present value\nof net gains per adult\nfrom the GCP", #fill_na = T,
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[4]) plot_world_map(paste0("gain_adj_over_gdp_", y), df = sm, breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[5]) plot_world_map(paste0("gain_adj_", y), df = sm, breaks = c(-Inf, -2000, -800, -300, -100, -0.1, 0.1, 50, 100, 200, 400, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -2000, -800, -300, -100, 0, 0, 50, 100, 200, 400, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in $/year)"), #fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (df in c("sl", "sm", "sh", "sf")) plot_world_map("npv_over_gdp_gcs_adj", df = d(df), breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), filename = paste0("npv_over_gdp_gcs_adj_", df), # .003, .01, .03
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 

for (y in years[4]) plot_world_map(paste0("gain_adj_over_gdp_", y), df = sm, breaks = c(-Inf, -.03, -.02, -.01, -0.005, -.002, 0.02, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -0.005, -.002, 0.02, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), 
               legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)"), #fill_na = T,
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
plot_world_map("npv_over_gdp_gcs_adj", df = sm, breaks = c(-Inf, -.02, -.01, -.003, -.001, 0.002, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, -.001, 0.002, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), # .003, .01, .03
               legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
               save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 
for (y in years[5]) plot_world_map(paste0("share_basic_income_", y), df = sm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
                 labels =  sub("≤", "<", agg_thresholds(c(0), c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1), sep = " to ", return = "levels")), 
                 legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)"), #fill_na = T,
                 save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) 



##### Scenarios #####
# /!\ Pb: the price used is the global carbon price, which would induce stronger emission reductions than the proportional share of the carbon budget when participation is not universal (and involves lower average emission than the world).
# In other words, the scenarios 2-6 are best seen as a carbon tax than an ETS.
df <- sm # cs
# 1. All: Whole World
all_countries <- setNames(df$code, df$country)
# 2. All against OPEC+: World except OPEC+ losers
all_but_OPEC <- all_countries[!df$code %in% c("RUS", "KAZ", "SAU", "QAT", "KWT", "ARE", "OMN", "BHR", "MYS")] # NB: Qatar has left OPEC
# 3. Optimistic scenario: not losers + EU28 + Norway + Switzerland + Canada + Japan + Korea + NZ + U.S. Democratic states 
optimistic <- c(all_countries[df$npv_pa_gcs_adj >= 0 | df$code %in% c("CHN", EU28_countries, "NOR", "CHE", "CAN", "JPN", "KOR", "NZL")], "Dem USA" = "Dem USA")
# 4. Central scenario: winners + China + EU28 + Norway + Switzerland + Japan + NZ (Before, was 2030 instead of NPV winners)
central <- all_countries[df$npv_over_gdp_gcs_adj > 0 | df$code %in% c("CHN", EU28_countries, "NOR", "CHE", "JPN", "NZL")]
# 5. Generous EU: EU27 + China + winners. (Before, was EU27 + China + African non-losers + Asia non-losers)
cautious <- all_countries[(df$npv_over_gdp_gcs_adj > 0) | df$code %in% c("CHN", EU27_countries)]
# 6. Africa-EU partnership: EU27 + African winners
africa_EU <- all_countries[(df$npv_over_gdp_gcs_adj >= 0 & image_region_by_code[df$code] %in% c("WAF", "SAF", "RSAF", "NAF", "EAF")) | df$code %in% c(EU27_countries)]
scenarios_names <- c("all_countries", "all_but_OPEC", "optimistic", "central", "cautious", "africa_EU")
scenarios_parties <- setNames(lapply(scenarios_names, function(name) eval(str2expression(name))), scenarios_names) 

for (s in scenarios_names) df <- create_var_ssp(df = df, scenario = s)

emissions_tot <- emissions_pc <- c()
for (y in as.character(2025:2100)) {
  emissions_tot[y] <- sum(df[[paste0("emissions_", y)]], na.rm = T)
  emissions_pc[y] <- wtd.mean(df[[paste0("emissions_pc_", y)]], weights = df[[paste0("pop_", y)]], na.rm = T) }
EU_gain_adj <- EU_gain_adj_over_gdp <- EU_npv_gain_adj_over_gdp <- list()
for (s in scenarios_names) {
  emissions_pa[[s]] <- EU_gain_adj[[s]] <- EU_gain_adj_over_gdp[[s]] <- c()
  for (y in as.character(2025:2100)) { # seq(2020, 2100, 10)
    EU_gain_adj[[s]][y] <- wtd.mean(df[[paste0(if (s == "all_countries") "" else paste0("S", s, "_"), "gain_adj_", y)]], weights = df[[paste0("adult_", y)]] * (df$code %in% EU27_countries))
    EU_gain_adj_over_gdp[[s]][y] <- EU_gain_adj[[s]][y]/wtd.mean(df[[paste0("gdp_pc_", y)]], weights = df[[paste0("pop_", y)]] * (df$code %in% EU27_countries)) }
  EU_npv_gain_adj_over_gdp[[s]] <- sum(sapply(2025:2100, function(y) { return(EU_gain_adj[[s]][as.character(y)]/(1+discount_rate)^(y-2025)) }))/sum(sapply(2025:2100, function(y) { return(wtd.mean(df[[paste0("gdp_pa_", y)]], weights = df[[paste0("adult_", y)]] * (df$code %in% EU27_countries))/(1+discount_rate)^(y-2025)) })) 
}
basic_income_adj
emissions_pc
EU_gain_adj
EU_gain_adj_over_gdp
EU_npv_gain_adj_over_gdp

world_emissions_pc

scenarios_features <- data.frame(scenario = capitalize(gsub("_", " ", scenarios_names)), row.names = scenarios_names)
for (s in scenarios_names) {
  scenarios_features[s, "emissions_covered"] <- sum(df$share_territorial_2019[df$code %in% eval(str2expression(s))]) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 0.0318
  scenarios_features[s, "pop_covered"] <- (sum(df$pop_2023[df$code %in% eval(str2expression(s))], na.rm = T) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 117*1e6)/sum(df$pop_2023, na.rm = T)
  scenarios_features[s, "basic_income_2040"] <- basic_income_adj[[s]]["2040"]/12
  scenarios_features[s, "EU_loss_adj_over_gdp_2040"] <- -EU_gain_adj_over_gdp[[s]]["2040"]
}
(scenarios_table <- scenarios_features)

for (col in names(scenarios_features)[2:length(names(scenarios_features))]) scenarios_table[, col] <- paste0(sprintf(paste0("%.", if (grepl("EU_", col)) 1 else 0, "f"), if (grepl("basic_income", col)) scenarios_features[, col] else 100*scenarios_features[, col]), if (grepl("basic_income", col)) "" else "\\%")
# write.table(scenarios_table, file = "scenarios_table.tex", sep = "\t", row.names = capitalize(gsub("_", " ", scenarios_names)), 
#             col.names = c("Scenario", "\\makecell{Emissions\\\\covered}", "\\makecell{Population\\\\covered}", "\\makecell{Basic income\\\\in 2040 ($/month)}", "\\makecell{EU loss in 2040\\\\(in share of GDP)}"))
cat(paste(kbl(scenarios_table, "latex", caption = "Main features of the different scenarios.", position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1), label = "scenarios_table.tex", row.names = FALSE,  
              col.names = c("Scenario", "\\makecell{Emissions\\\\covered}", "\\makecell{Population\\\\covered}", "\\makecell{Basic income\\\\in 2040 (\\$/month)}", "\\makecell{EU loss in 2040\\\\(share of its GDP)}")), collapse="\n"), file = "../tables/scenarios_table.tex") 
scenarios_table_fr <- scenarios_table
scenarios_table_fr$scenario <- c("Tous les pays", "Tous sauf OPEP+", "Optimiste", "Prudent", "UE + Chine + gagnants", "UE + Afrique")
cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", paste(kbl(scenarios_table_fr, "latex", caption = "Principales caractéristiques des différents scénarios de club climatique.", caption.short = "Différents scénarios de club climatique", position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1), label = "scenarios_table_fr", row.names = FALSE,  format.args = list(decimal = ","),
              col.names = c("\\makecell{Scenario\\\\de club}", "\\makecell{Émissions\\\\mondiales\\\\couvertes}", "\\makecell{Population\\\\mondiale\\\\couverte}", "\\makecell{Revenu de base\\\\en 2040\\\\(\\$/mois)}", "\\makecell{Contribution de l'UE\\\\en 2040\\\\(fraction de son PIB)}")), collapse="\n"), fixed = T), fixed = T), file = "../tables/scenarios_table_fr.tex") 


##### Global trajectories #####
agg_png("../figures/policies/trajectories_sm_ZH.png", width = 715,  height= 433)
mar <- par()$mar
par(mar = c(2.1, 4.1, 0.1, 4.1))
# plot(c(2025, seq(2030, 2080, 10)), basic_income_adj$all_countries[as.character(c(2025, seq(2030, 2080, 10)))]/12, type = 'b', pch = 16, col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(-5, 70))
plot(c(2025, seq(2030, 2080, 10)), basic_income_adj$all_countries[as.character(c(2025, seq(2030, 2080, 10)))]/12, type = 'b', pch = 16, col = 'darkgreen', lwd = 2, xlab = "", ylab = "基本收入（每月美元）；二氧化碳排放量（每年千兆吨）", ylim = c(-5, 53), family="Microsoft JhengHei")
# plot(2025:2080, basic_income_adj$all_countries[as.character(2025:2080)]/12, type = 'l', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(-5, 70))
# lines(2025:2080, basic_income_adj$optimistic[as.character(2025:2080)]/12, type = 'l', col = 'red', lwd = 2)
# lines(2025:2080, basic_income_adj$central[as.character(2025:2080)]/12, type = 'l', col = 'red', lwd = 2)
# lines(2025:2080, basic_income_adj$all_but_OPEC[as.character(2025:2080)]/12, type = 'l', col = 'red', lwd = 2)
# lines(2025:2080, basic_income_adj$cautious[as.character(2025:2080)]/12, type = 'l', col = 'red', lwd = 2)
# lines(2025:2080, basic_income_adj$africa_EU[as.character(2025:2080)]/12, type = 'l', col = 'red', lwd = 2)
lines(c(2025, seq(2030, 2080, 10)), emissions_tot[as.character(c(2025, seq(2030, 2080, 10)))]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
# lines(2025:2080, emissions_tot[as.character(2025:2080)]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(c(2025, seq(2030, 2080, 10)), carbon_price$ssp2_26[as.character(c(2025, seq(2030, 2080, 10)))], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
# plot(2025:2080, carbon_price$ssp2_26[as.character(2025:2080)], type = 'b', pch = 17, axes = FALSE, ylim = c(-100, 1060), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
# mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5) 
mtext("碳价格（美元/吨 CO2）", side=4, col="blue", line=2.5, family="Microsoft JhengHei") 
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
# legend("topleft", legend = c("CO2 emissions", "Basic income", "Carbon price (right axis)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))
op <- par(family = "Microsoft JhengHei")
legend("topleft", legend = c("二氧化碳排放量", "基本收入", "碳价格（右轴）"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))
par(op)
dev.off()


##### Chancel data #####
wid <- read.dta13("../data/WID_world-pct-em-income.dta") # /!\ PPP €19 estimates, from Chancel email Nov 21, 2023m cd. /data/deprecated/Chancel_read_me
wid <- rbind(t(sapply(1:5, function(i) c(pctile = i, wid[1, 2:4]/5))), wid[2:96,]) # In 2019 1$ = 0.89€, i.e. 2.15$ = 1.91€
for (i in 1:4) wid[[i]] <- unlist(wid[[i]])
mean(wid$emissions)

# ## Hypothesis that matches our main model:
# wid$emissions <- wid$emissions * emissions_pc["2025"]/mean(wid$emissions) # Rescaling total emissions as WID also includes non-CO2 gases.
# emissions_reduction_factor <- emissions_tot["2030"]/emissions_tot["2025"] # 0.91
# price <- carbon_price$ssp2_26["2030"]*euro_per_dollar # - carbon_price$ssp2_26["2025"]*euro_per_dollar # 134€/t
# # wid$income <- wid$income*wtd.mean(df$gdp_pc_2030, df$pop_2030)/mean(wid$income) # Unused

## Simplified hypothesis that yields virtually identical result (unless we rescale GDP or emissions):
emissions_reduction_factor <- emissions_tot["2030"]/emissions_tot["2025"] # 0.91
price <- 100*euro_per_dollar
iter <- 0
# emissions_reduction_factor <- 0.7
# price <- 200
wid$post_emissions <- wid$emissions
post_emissions <- wid$emissions * emissions_reduction_factor
(revenues_pa <- sum(price * post_emissions)/1200) # 42€
wid$post_income <- wid$income + revenues_pa*12 - price * post_emissions # basic income, carbon price
while (max_gap(wid$post_emissions, post_emissions) > 1e-5 & iter < 100) { # Takes into account the rebound effect: simply remove the loop to neglect it.
  iter <- iter + 1
  wid$post_emissions <- post_emissions
  post_emissions <- interpolate(wid$post_income, wid$income, wid$emissions)
  post_emissions <- post_emissions * emissions_reduction_factor * sum(wid$emissions)/sum(post_emissions)
  (revenues_pa <- sum(price * post_emissions)/1200) 
  wid$post_income <- wid$income + revenues_pa*12 - price * post_emissions
}
wid$post_emissions <- post_emissions
# wid$post_income[1]/wid$income[9]

wid$diff_income <- stats::filter(sort(wid$post_income - wid$income, decreasing = T), filter = c(.1, .2, .4, .2, .1))
wid$diff_income[is.na(wid$diff_income)] <- sort(wid$post_income - wid$income, decreasing = T)[is.na(wid$diff_income)]
wid$diff_income[98:100] <- (wid$post_income - wid$income)[98:100]
wid$variation_income <- stats::filter(sort((wid$post_income - wid$income)/wid$income, decreasing = T), filter = c(.1, .2, .4, .2, .1))
wid$variation_income[is.na(wid$variation_income)] <- sort((wid$post_income - wid$income)/wid$income, decreasing = T)[is.na(wid$variation_income)]

revenues_pa*12/mean(wid$income) # Revenues as share of world GDP

# Three different dataset: WID, PIP, Gütschow
# Poverty gap is computed from WID (Table ineq) and PIP (7.1 note 3) => best to combine PIP with GDP data as we do => 2% PG in 2030
# Poverty rate is computed from WID (6.1, used to compute transfer from rich to poor) and PIP (7.1 note 3) => PIP more accurate to measure poverty rate => 40% PR in 2030
# Revenue raised is computed from WID (revenues_pa) and Gütschow (5.2)
# NEED at least 40€ of RDB to eradicate poverty
# => We necessarily have an inconsistency between ineq Table (where it is by chance that we find PG identical to PIP),
#    rich->poor transfer (where we should rely on PIP percentiles) and total revenue raised (which relies on Gütschow)


# BOOK Figure
# mar <- par()$mar
# mgp <- par()$mgp
# par(mar = c(3.1, 3.1, 0.3, 0.2), mgp = c(2.2, 1, 0)) # width: 342, height: 312
# plot(1:100, wid$income/12, col = "red", lwd = 2, type = 'l', ylim = c(0, 8e4/12), xlab = "Percentile de niveau de vie", ylab = "Niveau de vie (en €/mois)")
# lines(1:100, wid$post_income/12, col = "darkgreen", lwd = 2, type = 'l', lty = 2) + grid()
# legend("topleft", legend = list("actuel", "suite au Plan"), col = c("red", "darkgreen"), title = "Niveau de vie", lwd = 2, lty = c(1,2))
# plot(1:100, wid$diff_income, col = "purple", lwd = 2, ylim = c(-2000, 500), type = 'l', xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en €/an)") + grid() + abline(h = 0)
# plot(1:100, 100*pmin(6, wid$variation_income), col = "blue", lwd = 2, type = 'l', ylim = 100*c(0, 2.2), xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en %)") + grid() + abline(h = 0)
# plot(40:100, 100*wid$variation_income[40:100], col = "blue", lwd = 2, type = 'l', ylim = 100*c(-0.024, 0.048), xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en %)") + grid() + abline(h = 0)

sum(wid$post_income > wid$income) # 71% of winners
sum((wid$post_income - wid$income)[wid$post_income > wid$income])/sum(wid$income) # 1.3% redistributed
Gini(wid$income) # .675
Gini(wid$post_income) # .657
wid$income[90]/wid$income[10] # 54
wid$post_income[90]/wid$post_income[10] # 32
wid$income[90]/wid$income[50] # 5.55
wid$post_income[90]/wid$post_income[50] # 5.27
sum(wid$income[91:100])/sum(wid$income) # 53%
sum(wid$post_income[91:100])/sum(wid$post_income) # 52%
sum(wid$income[100])/sum(wid$income) # 19.6%
sum(wid$post_income[100])/sum(wid$post_income) # 19.1%
sum(wid$income[1:50])/sum(wid$income) # 8.5%
sum(wid$post_income[1:50])/sum(wid$post_income) # 9.6%
sum(wid$income[91:100])/sum(wid$income[1:40]) # 10.4
sum(wid$post_income[91:100])/sum(wid$post_income[1:40]) # 8.5
# Poverty
sum(wid$income < 1.35*365) # 8%
sum(wid$post_income < 1.35*365) # 0%, 1.35€ = 1.5$
sum(wid$post_income[wid$income < 7.5*365])/sum(wid$income) # 2.4%
sum(wid$post_income[wid$post_income < 7.5*365])/sum(wid$post_income) # 1.7%, i.e. -29%

(table_ineq <- cbind("poverty_gap" = 100*c(sum(wid$post_income[wid$income < 7.5*365])/sum(wid$income), sum(wid$post_income[wid$post_income < 7.5*365])/sum(wid$post_income)), "top10" = 100*c(sum(wid$income[91:100])/sum(wid$income), sum(wid$post_income[91:100])/sum(wid$post_income)), "bottom50" = 100*c(sum(wid$income[1:50])/sum(wid$income), sum(wid$post_income[1:50])/sum(wid$post_income)),
                     "Gini" = 100*c(Gini(wid$income), Gini(wid$post_income)), "D9/D1" = c(wid$income[90]/wid$income[10], wid$post_income[90]/wid$post_income[10])))
row.names(table_ineq) <- c("Avant", "Après")
cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", paste(kbl(table_ineq, "latex", caption = "Évolution de l'inégalité mondiale suite au Plan.", position = "b", escape = F, booktabs = T, digits = 1, label = "gcp_ineq", align = 'c', format.args = list(decimal = ","),
               col.names = c("\\makecell{Étendue de\\\\la pauvreté\\\\à 7,5~\\euro{}/jour\\\\(en \\% du PIB)}", "\\makecell{Top 10~\\%\\\\(part en \\%)}", "\\makecell{Bottom 50~\\%\\\\(part en \\%)}", "\\makecell{Gini\\\\(en \\%)}", "\\makecell{D9/D1\\\\Ratio\\\\inter-décile}")), collapse="\n"), fixed = T), fixed = T), file = "../tables/gcp_ineq.tex") 
# (table_ineq <- cbind("top10" = 100*c(sum(wid$income[91:100])/sum(wid$income), sum(wid$post_income[91:100])/sum(wid$post_income)), "bottom50" = 100*c(sum(wid$income[1:50])/sum(wid$income), sum(wid$post_income[1:50])/sum(wid$post_income)),
#                     "Gini" = 100*c(Gini(wid$income), Gini(wid$post_income)), "D9/D1" = c(wid$income[90]/wid$income[10], wid$post_income[90]/wid$post_income[10]),
#                     "Palma" = c(sum(wid$income[91:100])/sum(wid$income[1:40]), sum(wid$post_income[91:100])/sum(wid$post_income[1:40]))))
# row.names(table_ineq) <- c("Avant", "Après")
# cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", paste(kbl(table_ineq, "latex", caption = "Évolution de l'inégalité mondiale suite au Plan.", position = "b", escape = F, booktabs = T, digits = 1, label = "gcp_ineq", align = 'c', format.args = list(decimal = ","),
#               col.names = c("\\makecell{Top 10~\\%\\\\(part en \\%)}", "\\makecell{Bottom 50~\\%\\\\(part en \\%)}", "\\makecell{Gini (en \\%)}", "\\makecell{D9/D1\\\\Ratio\\\\inter-décile}", "\\makecell{Top 10~\\% sur\\\\Bottom 40~\\%\\\\(ratio des parts)}")), collapse="\n"), fixed = T), fixed = T), file = "../tables/gcp_ineq.tex") 

# plot(stats::density(log10(wid$income)), col = "red")
# lines(stats::density(log10(wid$post_income)), col = "darkgreen")

# plot(1:100, 100*wid$variation_income, col = "blue", lwd = 2, type = 'l', ylim = 100*c(-0.024, 0.048), xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie suite au Plan (en %)") + grid() + abline(h = 0)

# plot(1:100, 100*sort(pmin(6, (wid$post_income - wid$income)/wid$income), decreasing = T)[1:100], col = "blue", lwd = 2, type = 'l', ylim = 100*c(0, 5), xlab = "Percentile de revenus", ylab = "Variation de niveau de vie suite au Plan (en %)") + grid() + abline(h = 0)
# plot(1:99, sort(wid$post_income - wid$income, decreasing = T)[1:99], col = "blue", lwd = 2, type = 'l', xlab = "Percentile de revenus", ylab = "Variation de niveau de vie suite au Plan (en $/an)") + grid() + abline(h = 0)
# plot(1:100, 100*sort((wid$post_income - wid$income)/wid$income, decreasing = T), col = "blue", lwd = 2, type = 'l', ylim = 100*c(-0.02, 0.05), xlab = "Percentile de revenus", ylab = "Variation de niveau de vie suite au Plan (en %)") + grid() + abline(h = 0)


##### 1% of richest billion triples poorest billion #####

# Check: 1% from top 1G => triple bottom 1G.
mean(wid$income[1:12]) # 306€/year: average income of bottom billion
mean(wid$income[89:100]) # 78k€/year: average income of top billion
mean(wid$income[1:12])/mean(wid$income[89:100]) # 0.4%: 1% from top 1G => triple bottom 1G
mean(wid$income[1:20])/mean(wid$income[91:100]) # 1% of the top 10% would double the bottom 20%
sum(wid$income[1:12])/sum(wid$income[100]) # 1% of the top 1% would double the income of the bottom billion



##### Bruckner et al. (22) #####
country_distr <- read.xlsx("../data/bruckner.xlsx") # /!\ Elasticities are generally > 1, i.e. larger carbon intensities for richer households within a country
deciles <- data.frame("country" = unique(country_distr$ctry_iso3))
for (i in 1:10) for (c in deciles$country) deciles[[paste0("footprint_d", i)]][deciles$country == c] <- country_distr$CF_decile[country_distr$ctry_iso3 == c & country_distr$decile == i]
deciles$winning <- rowSums(deciles[, 2:11] < 3.2) # World average carbon footprint is 3.2 tCO2 according to their 2014 data
country_names <- setNames(co2_pop$country_map, co2_pop$code)
deciles$country_map <- country_names[deciles$country]

plot_world_map("winning", df = deciles,  breaks = seq(-0.5, 10.5, 1), format = c('png', 'pdf'), legend_x = .045, trim = T, # svg, pdf
               labels = paste0(seq(0, 100, 10), " %"), legend = "Part de gagnants", 
               save = T) 


##### World Inequality Database #####
# # 2019 World average carbon footprint (all gases): 5.97237587 tCO2eq (from wid.world/data)
# # All WID data downloaded from https://wid.world/data/ on Nov 22, 2023
# wid_countries <- read.csv2("../data/wid_all_data/WID_countries.csv", na.strings = c())$alpha2
# percentiles <- data.frame("iso2" = wid_countries[!grepl("-", wid_countries)])
# for (c in percentiles$iso2) {
#   dataC <- read.csv2(paste0("../data/wid_all_data/WID_data_", c, ".csv"))
#   for (i in unique(dataC$percentile[dataC$variable == "lpfghgi999" & dataC$year == 2019])) percentiles[[i]][percentiles$iso2 == c] <- as.numeric(dataC$value[dataC$variable == "lpfghgi999" & dataC$year == 2019 & dataC$percentile == i])
# }
# percentiles$code <- iso2to3[percentiles$iso2]
# percentiles$country_map <- country_names[percentiles$code]
# percentiles$share_below_global_mean <- rowSums(percentiles[, sapply(0:99, function(i) paste0("p", i, "p", i+1))] < 5.97237587) # percentiles$p0p100[percentiles$iso2 == "WO"]
# write.csv(percentiles, "../data/wid_emissions_percentiles.csv")

percentiles <- read.csv("../data/wid_emissions_percentiles.csv")
percentiles$share_below_global_mean[no.na(percentiles$code) == "IND"]
percentiles$share_below_global_mean[no.na(percentiles$code) == "FRA"]
revenues_pa - price * percentiles$p50p51[no.na(percentiles$code) == "FRA"]/12 # -18

plot_world_map("share_below_global_mean", df = percentiles[!is.na(percentiles$country_map),],  breaks = c(-Inf, 1, 15, 30, 50, 70, 90, 99, Inf), format = c('png', 'pdf'), legend_x = .09, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(1), c(-Inf, 1, 15, 30, 50, 70, 90, 99, Inf), sep = "% à ", end = "%", return = "levels")), legend = "Part de gagnants", 
               save = T) 
plot_world_map("share_below_global_mean", df = percentiles[!is.na(percentiles$country_map),], continuous = T, format = c('png', 'pdf'), legend_x = .09, trim = T, # svg, pdf
               legend = "Part de gagnants", limits = c(0, 100), breaks = seq(-0.5, 100.5, 1), labels = paste0(seq(0, 100, 1), " %"), 
               save = T) 


# plot(2025:2080, sapply(2025:2080, function(y) sum(df[[paste0("adult_", y)]], na.rm = T)), type = 'l', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)")


##### Downscaled data from Richters et al. (23) #####
# Goes until 2100 (contrary to Richters et al. (22)). Fully downscaled, with all variables needed. This is NGFS v4.1 https://data.ece.iiasa.ac.at/ngfs/#/downloads
# Pb: CO2 prices are not uniform (but check, they may not differ that much between countries.)
# temp <- read.xlsx("../data/NGFS/Downscaled_MESSAGEix-GLOBIOM 1.1-M-R12_data.xlsx") # max gap: MESSAGE: 44% (Delayed transition & Below 2°C - but Low demand also interesting as the peak gap of 85% is in 2080 and it's < 10% before 2050, and same for Net  2050 (with peak 42% and <25%)); REMIND 90%; GCAM 70%
# temp <- temp[temp$Variable %in% c("Price|Carbon", "Emissions|CO2", "GDP|PPP|Counterfactual without damage", "Net GDP|PPP|median damage|KW panel population-weighted GMT AR6 climate diagnostics Surface Temperature (GSAT) MAGICCv7.5.3|50.0th Percentile", 
#                                   "Revenue|Government|Tax|Carbon", "Carbon Sequestration|CCS", "Emissions|Total Non-CO2", "Emissions|CO2|Energy and Industrial Processes", "Revenue|Government|Tax|Carbon"), 
#              c("Scenario", "Region", "Variable", 2000 + seq(20, 100, 10))] # IAM_data contains Price|Carbon, Temperature (peaking at 1.64° in Net Zero 2050), GDP|MER per world region and the whole world, etc.
# test <- temp[temp$Variable == "Price|Carbon", c("Scenario", "Region", 2000 + seq(20, 100, 10))]
# unique(test$Scenario) # Best scenarios is MESSAGE Net Zero 2050 as it's where prices are close to uniform until 2050, BUT prices are very high (700$ in 2030). (Pb: with Low demand which is also in this situation: there is no GDP data for it)
# for (s in unique(test$Scenario)) for (y in c(2000 + seq(20, 100, 10))) print(paste("max_gap", s, y, gap(test[[as.character(y)]][test$Scenario == s])))
# sort(setNames(test$`2040`[test$Scenario == "Below 2°C"], test$Region[test$Scenario == "Below 2°C"])) # Price is lowest in Australia-Japan and highest in EU
# sort(setNames(test$`2050`[test$Scenario == "Delayed transition"], test$Region[test$Scenario == "Delayed transition"])) # Price is lowest in Indonesia-Korea and highest in Latin America
# sort(setNames(test$`2080`[test$Scenario == "Net Zero 2050"], test$Region[test$Scenario == "Net Zero 2050"])) # Price is lowest in Australia-Japan and highest in EU


##### Downscaled data from Richters (et al.) (22) #####
# ngfs <- read.xlsx("../data/NGFS/downscaled_data.xlsx") # Covers only 2010-2050 (but contains all variables I need)
# ngfs <- read.xlsx("../data/NGFS/IAM_data.xlsx") # Only GDP PPP is downscaled, Pop is absent. Richters et al. (22) [https://zenodo.org/record/7198430](https://zenodo.org/record/7198430)
# names(ngfs)[1:5] <- c("model", "scenario", "country", "entity", "unit")
# unique(ngfs$scenario) # Below 2°C (1.6 °C in 2100 as >67% chance below 2°C throughout century), Current Policies, Delayed transition (2°C but late), Divergent Net Zero (2°C but disorderly), Nationally Determined Contributions (NDCs), Net Zero 2050 (1.4 °C)
# unique(ngfs$entity) # "net GDP|PPP|median damage|KW panel population-weighted GMT AR6 climate diagnostics Surface Temperature (GSAT) MAGICCv7.5.3|50.0th Percentile", "GDP|PPP|including medium chronic physical risk damage estimate", "Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CO2|Energy and Industrial Processes", "Emissions|Kyoto Gases", "Emissions|Kyoto Gases|AFOLU", "Emissions|N2O",
# # "Emissions|N2O|AFOLU", SF6, PFC, NOx, CH4, HFC, "GDP|PPP|Counterfactual without damage", "Population", "Price|Carbon", "Revenue|Government|Tax|Carbon", "Concentration|CO2", "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile", "Temperature|Global Mean", "Price|Carbon|SCC", "GDP|PPP|including chronic physical risk damage estimate"
# # sum(nchar(unique(ngfs$Region)) == 3)
# unique(ngfs$country) # 180 + World + each model regions (243 regions in total)
# unique(ngfs$model) # GCAM, MESSAGEix-GLOBIOM, REMIND-MAgPIE, REMIND-MAgPIE 3.0-4.4 IntegratedPhysicalDamages (95th-high), REMIND-MAgPIE 3.0-4.4 IntegratedPhysicalDamages (median)
# ngfs <- ngfs %>% .[.$scenario %in% c("Below 2°C", "Net Zero 2050") & nchar(.$country) == 3 & .$entity %in% c("net GDP|PPP|median damage|KW panel population-weighted GMT AR6 climate diagnostics Surface Temperature (GSAT) MAGICCv7.5.3|50.0th Percentile", "GDP|PPP|including medium chronic physical risk damage estimate", "Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", "GDP|PPP|Counterfactual without damage",
#                                                                                        "Population", "Price|Carbon", "Revenue|Government|Tax|Carbon", "Concentration|CO2", "Temperature|Global Mean", "Price|Carbon|SCC", "GDP|PPP|including chronic physical risk damage estimate"),] #  & !grepl("IntegratedPhysicalDamages", .$model)
# ngfs <- ngfs.bak
# # Kyoto gases: CO2, CH4, N2O, SF6, HFC, PFC. EU ETS: CO2, N2O, PFC.
# unique(paste(ngfs$entity, ngfs$unit)) # GDP in billion US$2010, Pop in million, Emissions in Mt CO2
# ngfs$entity[ngfs$entity == "Emissions|CO2"] <- "CO2"
# ngfs$entity[ngfs$entity == "Population"] <- "POP" # /!\ This is not downscaled by country! Population does not depend on the scenario (only on the model) and GCAM is closest to co2_pop==sm (<.5% gap)
# # if GCAM, MESSAGE or REMIND (not IntegratedPhysicalDamages)
# ngfs$entity[ngfs$entity == "GDP|PPP|including medium chronic physical risk damage estimate"] <- "GDPPPP" # could also be net GDP|PPP|median damage|KW panel population-weighted GMT AR6 climate diagnostics Surface Temperature (GSAT) MAGICCv7.5.3|50.0th Percentile
# # # if REMIND-MAgPIE 3.0-4.4 IntegratedPhysicalDamages (median)
# # ngfs$entity[ngfs$entity == "GDP|PPP|Counterfactual without damage"] <- "GDPPPP"
# write.csv(ngfs, "../data/NGFS.csv", row.names = FALSE)
# ngfs <- read.csv("../data/NGFS.csv")
# 
# copy_from_co2_pop <- c("country", "country_map", "gdr_pa_2030", # These three are absolutely needed
#                        "emissions_baseline_2030", "rci_2030", "territorial_2019", "footprint_2019", "missing_footprint", "gdp_pc_2019", "share_territorial_2019", "median_gain_2015", "mean_gain_2030", "gdp_ppp_now", "gdr_pa_2030_cerc")
# 
# prepare_ngfs_country <- function(scenario = "Below 2°C", ssps = ngfs, df = co2_pop, keep_from_df = copy_from_co2_pop) {
#   # Uses country, country_map and adult_ from co2_pop
#   # TODO: streamline creation of co2_pop for this purpose, perhaps also keeping emissions_baseline_2030, rci_2030, territorial_2019, footprint_2019, missing_footprint, gdp_pc_2019 (not PPP), share_territorial_2019, median_gain_2015, mean_gain_2030, gdp_ppp_now,gdr_pa_2030_cerc, gdr_pa_2030
#   # sum(is.na(ssp2_26_country)) # 1134
#   # sum(is.na(ssp2_26_country[!ssp2_26_country$code %in% c("SSD", "TWN", "PRK", "FSM"),])) # 0
#   # setdiff(co2_pop$code, ssp2_26_country$code) # small islands and small countries, Palestine
#   # setdiff(ssp2_26_country$code, co2_pop$code) # Taiwan, Hong Kong
#   # Add Taiwan data
#   if (exists("pop_iso3") & !"TWN" %in% df$code) {
#     twn <- df[df$code == "KOR",]
#     twn$code <- "TWN"
#     twn$country <- twn$country_map <- "Taiwan"
#     for (y in c(2023, seq(2020, 2100, 10))) {
#       twn[[paste0("pop_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$pop[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$pop[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
#       twn[[paste0("adult_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$adult[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$adult[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
#     }
#     df <- rbind(df, twn)
#   }
#   ssps <- ssps[ssps$country %in% df$code & !ssps$country %in% c("FSM", "GRD"),] # "SSD", "TWN", "PRK",
#   ssp <- data.frame(code = c(unique(ssps$country), "SSD", "TWN", "PRK"))
#   for (y in 2020:2100) { # Years span 1850:2100
#     if (sum(ssps$entity == "POP") != 0) ssp[[paste0("pop_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "POP"], ssps$country[ssps$scenario == scenario & ssps$entity == "POP"])[ssp$code]
#     ssp[[paste0("gdp_", y)]] <- 1e6 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "GDPPPP"], ssps$country[ssps$scenario == scenario & ssps$entity == "GDPPPP"])[ssp$code]  # /!\ It is in PPP (contrary to old code with IIASA ssps)
#     ssp[[paste0("emissions_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "CO2"], ssps$country[ssps$scenario == scenario & ssps$entity == "CO2"])[ssp$code]
#     for (v in c("emissions_", "gdp_", "pop_")) ssp[[paste0(v, y)]] <- 1e3 * ssp[[paste0(v, y)]]
#     # Add PRK, TWN, SSD data
#     # for (c in c("PRK", "TWN", "SSD")) for (v in c("emissions_", "gdp_", "pop_")) ssp[[paste0(v, y)]][ssp$code == c] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0(v, y - y %% 10)]][df$code == c], df[[paste0(v, 10*ceiling(y/10))]][df$code == c])
#   }
#   if (sum(ssps$entity == "POP") == 0) for (y in c(2023, seq(2020, 2100, 10))) ssp[[paste0("pop_", y)]][match.nona(df$code, ssp$code)] <- df[[paste0("pop_", y)]][df$code %in% ssp$code]
#   for (y in c(2023, seq(2020, 2100, 10))) ssp[[paste0("adult_", y)]][match.nona(df$code, ssp$code)] <- ssp[[paste0("pop_", y)]][match.nona(df$code, ssp$code)] * (df[[paste0("adult_", y)]]/df[[paste0("pop_", y)]])[df$code %in% ssp$code]
#   for (y in 2020:2100) { # Interpolate adult_ from pop_ and df$adult/df$pop
#     y_prev <- 10*floor(y/10)
#     y_next <- 10*ceiling(y/10)
#     if (y > 2023 & y < 2030) y_prev <- 2023
#     if (y %in% c(2021, 2022)) y_next <- 2023
#     lambda <- (y - y_prev)/10
#     ssp[[paste0("adult_", y)]] <- ssp[[paste0("pop_", y)]] * ((1 - lambda) * (ssp[[paste0("adult_", y_prev)]]/ssp[[paste0("pop_", y_prev)]]) + lambda * (ssp[[paste0("adult_", y_next)]]/ssp[[paste0("pop_", y_next)]]))
#     ssp[[paste0("emissions_pa_", y)]] <- ssp[[paste0("emissions_", y)]]/ssp[[paste0("adult_", y)]]
#     ssp[[paste0("emissions_pc_", y)]] <- ssp[[paste0("emissions_", y)]]/ ssp[[paste0("pop_", y)]]
#     ssp[[paste0("gdp_pa_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("adult_", y)]] # /!\ It is in PPP (contrary to old code with IIASA ssps)
#     ssp[[paste0("gdp_pc_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("pop_", y)]] # /!\ It is in PPP (contrary to old code with IIASA ssps)
#     ssp[[paste0("gdp_pc_over_mean_", y)]] <- ssp[[paste0("gdp_pc_", y)]]/wtd.mean(ssp[[paste0("gdp_pc_", y)]], ssp[[paste0("pop_", y)]], na.rm = T)
#   }
#   ssp$gdp_ppp_now <- ssp$gdp_ppp_2023 # TODO? add carbon_price_?
#   ssp$gdp_pc_base_year <- ssp$gdp_ppp_pc_2023
#   for (v in intersect(keep_from_df, names(df))) ssp[[v]][match.nona(df$code, ssp$code)] <- df[[v]][df$code %in% ssp$code]
#   return(ssp)
# }
# 
# cs <- prepare_ngfs_country(scenario = "Below 2°C", ssps = ngfs[grepl("GCAM", ngfs$model),])
# cs <- create_var_ssp(df = cs)
# 
# for (y in years[4]) plot_world_map(paste0("gain_adj_over_gdp_", y), df = cs, breaks = c(-Inf, -.03, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
#          labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.03, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")),
#          legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in % of GDP)"), #fill_na = T,
#          save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))
# for (df in c("cs")) plot_world_map("npv_over_gdp_gcs_adj", df = d(df), breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .03, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf
#          labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .03, .1, Inf)*100, sep = " to ", return = "levels")), filename = paste0("npv_over_gdp_gcs_adj_", df), # .003, .01, .03
#          legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
#          save = F) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))
# 
# 
# 
# 
# 


##### Book #####
## Own calculations: need references / methodological note
# Ch 2: Domestic Poverty Eradication - PIP
# Ch 5: moitié de l'humanité < 7$/jour (contredit 8.1)
# Ch 6.2: recettes = 5% du PIB mondial
# Fig 6.1: trajectoires - gea_gea
# Ch 6.4: scénarios - cf. 7.3
# Ch 7.1: effet sur la distribution mondiale - Chancel
# Ch 7.2: effet sur les distributions nationales - WID
# Ch 7.3: effet sur la distribution internationale - sm
# Ch 8.1: 39% de la population mondiale vit avec moins de 7,5$/j
# Ch 8.2: chiffrage redistribution nationale

# 7.3: Emissions data (already downscaled) comes from Gütschow et al. (21) while price comes from SSP_CMIP6

# Figures
# 1: GDP_pc_PPP_few_fr
# 7.1: gcp_rev_distr, gcp_diff_rev, gcp_var_rev, gcp_var_rev_rich_only
# 7.2: share_below_global_mean
# 7.3: gain_adj_2030_fr, npv_over_gdp_gcs_adj_fr, Soptimistic_npv_over_gdp_gcs_adj, Scentral_npv_over_gdp_gcs_adj

plot(2025:2080, basic_income_adj$ssp2_26[as.character(2025:2080)]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(0, 100))
lines(2025:2080, emissions_tot[as.character(2025:2080)]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(2025:2080, carbon_price$ssp2_26[as.character(2025:2080)], type = 'b', pch = 17, axes = FALSE, ylim = c(0, 750), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5) 
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
legend("topleft", legend = c("CO2 emissions", "Basic income", "Carbon price (right axis)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))

mar <- par()$mar
par(mar = c(2.1, 4.1, 0.1, 4.1))
plot(2025:2080, df[df$country == "China", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'red', lwd = 2, lty = 2, xlab = "", ylab = "CO2 emissions per adult (tCO2/year)", ylim = c(0, 17.5))
lines(2025:2080, df[df$code == "USA", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'blue', lwd = 2, lty = 3)
lines(2025:2080, colSums(df[df$code %in% EU27_countries, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% EU27_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'darkgreen', lwd = 2, lty = 4, xlab = "", ylab = "")
lines(2025:2080, df[df$code == "IND", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'orange', lwd = 2, lty = 5, xlab = "", ylab = "")
lines(2025:2080, colSums(df[df$code %in% SSA, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% African_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'purple', lwd = 2, lty = 6, xlab = "", ylab = "")
lines(2025:2080, colSums(df[, paste0("emissions_", 2025:2080)], na.rm = T)/colSums(df[, paste0("adult_", 2025:2080)], na.rm = T), type = 'l', col = 'black', lwd = 2, lty = 1, xlab = "", ylab = "")
grid() + abline(h = 1:18, col = "gray", lty = 3)
legend("topright", legend = c("World", "China", "U.S.", "EU", "India", "Sub-Saharan Africa"), col = c("black", "red", "blue", "darkgreen", "orange", "purple"), lwd = 2, lty = 1:6)

plot(2025:2080, df[df$country == "China", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'red', lwd = 2, lty = 2, xlab = "", ylab = "Émissions de CO2 par adulte (tCO2/an)", ylim = c(0, 17.5))
lines(2025:2080, df[df$code == "USA", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'blue', lwd = 2, lty = 3)
lines(2025:2080, colSums(df[df$code %in% EU27_countries, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% EU27_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'darkgreen', lwd = 2, lty = 4, xlab = "", ylab = "")
lines(2025:2080, df[df$code == "IND", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'orange', lwd = 2, lty = 5, xlab = "", ylab = "")
lines(2025:2080, colSums(df[df$code %in% SSA, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% African_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'purple', lwd = 2, lty = 6, xlab = "", ylab = "")
lines(2025:2080, colSums(df[, paste0("emissions_", 2025:2080)], na.rm = T)/colSums(df[, paste0("adult_", 2025:2080)], na.rm = T), type = 'l', col = 'black', lwd = 2, lty = 1, xlab = "", ylab = "")
grid() + abline(h = 1:18, col = "gray", lty = 3)
legend("topright", legend = c("Monde", "Chine", "États-Unis", "Union Européenne", "Inde", "Afrique subsaharienne"), col = c("black", "red", "blue", "darkgreen", "orange", "purple"), lwd = 2, lty = 1:6)


##### Course Tsinghua equal pc 2030 target #####
# Taking -20% over 2019-2030 as 2°C >66% chance, cf. e.g. Robiou du Pont et al. (17)
target_2030 <- 0.85*sum(co2_pop$emissions_2019)/sum(sm$pop_2030) # 3.37 t p.c.
(co2_pop$pop_2030*target_2030/co2_pop$emissions_2019)[co2_pop$code == "CHN"]-1 # -47%
sum(sm$pop_2030[sm$code == "USA"]*target_2030)/co2_pop$emissions_2019[co2_pop$code == "USA"]-1 # -77%
sum(co2_pop$pop_2030[co2_pop$code %in% EU27_countries])*target_2030/sum(co2_pop$emissions_2019[co2_pop$code %in% EU27_countries])-1 # -55%
(co2_pop$pop_2030*target_2030/co2_pop$emissions_2019)[co2_pop$code == "IND"]-1 # +118%


## Où le montant RDB! matter: 
# over 2030-60
# 40€/an à chaque Haïtien
# 44€ pour gain par personne
# (indirectement) -10€ par Français


# (df$gain_adj_2030*df$adult_2022)[df$country == "United States"]*0.97 # 460G$ to be raised to offset 
# df$gain_adj_2030[df$country == "United States"]*0.97/12 # -137$/month: loss to average (compensated) American from GCP
# # Increasing income tax rates 32->33% >315k, 35->40% >400k, 37->50% >1M, 37->60% >5M, 37->70% >50M, integrate with corporate tax and tax capital gains fully
# # => collects 459.4 G$ in 2019. Source: taxjusticenow.org (consulted on 29/04/2024)
# # The site also shows that taxes would barely increase for the bottom 97%, those with less than $312k/year (tax rate of percentile 96: 31.6->31.9%)
# 315000/12 # 26250$/month: threshold of top 3% according to taxjusticenow.org (FYI top 1% threshold: 567k/year = 47k/month)

# (df$gain_adj_2030*df$adult_2022)[df$country == "United States"] # 474G$ to be raised to offset 
# df$gain_adj_2030[df$country == "United States"]/12 # -141$/month: loss to average (compensated) American from GCP
# # Increasing income tax rates 35->40% >400k, 35->45% >600k, 37->50% >1M, 37->60% >5M, 37->70% >50M, integrate with corporate tax and tax capital gains fully
# # => collects 473 G$ in 2019. Source: taxjusticenow.org (consulted on 29/04/2024)
# # /!\ The site also shows that taxes would not only increase for the top 1%, e.g. for percentile 98: 30.1->30.9% 


##### Realistic scenarios #####