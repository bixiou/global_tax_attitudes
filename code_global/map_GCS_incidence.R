# TODO? share of winners per country: Ivanova & Wood (20) show that 2020 World average of 6t pc is at ~50 percentile in FR, ~65 in ES, ~50 in UK, ~20 in DE; Fremstad & Paul (19) show it's at ~20p in the U.S.
# TODO: more accurate assumption/computations (e.g. based on NDCs)

##### Population data #####
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop <- pop[pop$Time %in% c(2015, 2019, 2023, 2030),]
pop <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
pop_iso3 <- aggregate(adult ~ year + code, data = pop, FUN = sum)

##### CO2 emissions data #####
# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions
co2 <- read.csv("../data/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2016, 2019),]
temp <- co2[co2$Year == 2019,]
temp$Year <- 2023
co2 <- rbind(co2, temp)
names(co2) <- c("country", "code", "year", "territorial", "footprint")

##### Merge datasets #####
co2$year[co2$year == 2016] <- 2030 # Beware, before we estimate 2030 emissions, they will be equal to 2016 ones!
co2_pop <- merge(pop_iso3[pop_iso3$code != "",], co2[co2$code != "",])
co2_pop$adult <- 1000 * co2_pop$adult
co2_pop$emissions <- co2_pop$footprint
co2_pop$missing_footprint <- is.na(co2_pop$footprint)
(sum(co2_pop$adult_2019[is.na(co2_pop$footprint_2019)])/adult_pop_2019) # 7.4% of global footprint data missing 
co2_pop$emissions[is.na(co2_pop$footprint)] <- co2_pop$territorial[is.na(co2_pop$footprint)] # imputing territorial emissions for those countries
co2_pop <- co2_pop %>% group_by(code) %>% pivot_wider(id_cols = c("code", "country"),  names_from = year, 
           values_from = c("territorial", "footprint", "emissions", "adult", "missing_footprint"), names_glue = "{.value}_{year}") %>% ungroup()
co2_pop$missing_footprint <- co2_pop$missing_footprint_2019
co2_pop <- co2_pop[, !colnames(co2_pop) %in% paste0("missing_footprint_", c(2015, 2019, 2023, 2030))]
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
# revenues_pc_oecd <- c(134, 105, 68, 46, 61, 42, 60, 34, 42, 72, 39, 40, 59, 128, 18, 38, 13, 16, 50, 32) # cf. oecd_climate/questionnairesboard row 63 and oecd_climate/questionnaires/net_gain_global_tax
# mean_gain_oecd <- 30 - revenues_pc_oecd
# emission_share_2015_oecd <- c(426.4, 547.9, 59.4, 445, 853.4, 423, 1361, 485.5, 273.8, 584.8, 293.8, 374.9, 575.8, 5794.5, 475.4, 7977.9, 1918.8, 484.6, 313.5, 262)/32276 # OECD bit.ly/37kSVUx EU28 12.3%, G20 82.7% 2015 latest date available
# names(mean_gain_oecd) <- names(emission_share_2015_oecd) <- names(revenues_pc_oecd) <- countries_survey_oecd
#             2020   2030   2040   2050                                Past/global basic income/estimate of a global basic income.pdf
# Emissions   34.3   26.3   18.8   13.1 GtCO2                          2DS (66% chance) scenario in Energy Technology Perspectives 2017 http://www.iea.org/etp2017/summary/
# Price         40     90    120    145 $/tCO2                         Stern & Stiglitz (2017), Table 3
# Revenues    1372   2367   2256   1900 G$/year
# Basic income  20     30     26     21 $/month                        Median UN ≥15 pop
# Basic income 1.4    2.1    1.8    1.5 Sub-Saharan Africa PPP $/day   2.1: ratio of GDP pc of Sub-Saharan Africa in PPP and value (World Bank)
# Share income 1.4    1.7    1.2    0.7 % of Gross World Product       2020: 98T$, then assuming 3.5% growth
# Emissions pa   6      4    2.6    1.7 tCO2/year per adult above 15
# Adult pop    5.7    6.6    7.2    7.5 G adult above 15
# /!\ The 2020 Gross World Product (84.7T$) and future growth may well be lower than assumed (98T$, 3.5%), hence higher Share income.
# /!\ Units of price are not specified in Stern & Stiglitz (2017), but probably constant 2017 dollar.


##### Compute net gain per capita #####
# Assumption: emissions per adult (>15) will evolve in the same way in all countries
# There are discrepancies between OECD data (used in OECD survey) and Global Carbon Project data (used here as it covers all countries, cf. Peters al. (2012)). Discrepancies are always within +/- 20%.
# The results also slightly change if the baseline year 2019 is used instead of 2015, and if we compute the net gain for (i.e. divide by population of) 2030 vs. 2015, cf. deprecated/draft_map_GCS_incidence.R for an analysis
# For consistency with the OECD survey, we use 2015 as a baseline.
compute_gain <- function(year = 2030, base_year = 2019, type = "mean", df = co2_pop, return_data = T) {
  df[[paste0("demographic_evolution_", base_year)]] <- (df$adult_2030/df[[paste0("adult_", base_year)]]) * (sum(df[[paste0("adult_", base_year)]])/adult_pop_2030)
  df[[paste0("emissions_pc_", base_year)]] <- df[[paste0("emissions_", base_year)]]/df[[paste0("adult_", base_year)]]
  df[[paste0("share_emissions_", base_year)]] <- df[[paste0("emissions_", base_year)]]/sum(df[[paste0("emissions_", base_year)]])
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_", base_year)]] * df[[paste0("demographic_evolution_", base_year)]]
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]]/sum(df[[paste0("share_emissions_2030_base_", base_year)]])
  df$emissions_pc_2030 <- df[[paste0("emissions_pc_", base_year)]] * co2_emissions_2030 / df$adult_2030
  df[[paste0("revenues_pc_", year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]] * carbon_tax_revenues_2030 / df[[paste0("adult_", year)]]/12
  df[[paste0(type, "_gain_", year)]] <- 30 - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pc_", year)]]
  if (return_data) return(df) else return(df[[paste0(type, "_gain_", year)]])
}
co2_pop <- compute_gain(year = 2015, base_year = 2015, type = "median") # creates median_gain_2015
co2_pop <- compute_gain(year = 2030, base_year = 2019, type = "mean") # creates mean_gain_2030


# Net median gain in our 5 countries of interest
(median_gain_2015_LCU <- LCU_per_dollar*co2_pop$median_gain_2015[sapply(c("FRA", "DEU", "ESP", "GBR", "USA"), function(c) which(co2_pop$code == c))])
# Appendix table
min_pop_table_gain_gcs <- 20e6
sum(co2_pop$adult_2019[co2_pop$adult_2019 > min_pop_table_gain_gcs])/adult_pop_2019 # 94% (89%) of global population lives in one of the 80 (57) countries > 10M
# TODO? remove? change Mean to Median and mean_gain_2030 to median_gain_2015? With median_gain_2015, the ranking with emissions_pc_2015 is not preserved because we divide by adult_2015 instead of adult_2030
table_gain_gcs <- sort(setNames(co2_pop$mean_gain_2030[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs]))
temp <- sort(setNames(co2_pop$emissions_pc_2019[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs])) # China has larger footprint than France!
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