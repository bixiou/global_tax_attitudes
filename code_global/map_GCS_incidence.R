pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop <- pop[pop$Time %in% c(2015, 2019, 2030),]
pop <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
pop_location <- aggregate(adult ~ year + country, data = pop, FUN = sum)
# pop_iso2 <- aggregate(adult ~ year + ISO2_code, data = pop, FUN = sum)
pop_iso3 <- aggregate(adult ~ year + code, data = pop, FUN = sum)

# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions
co2 <- read.csv("../data/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2016, 2019),]
names(co2) <- c("country", "code", "year", "territorial", "footprint")

co2$year[co2$year == 2016] <- 2030 # Beware, before we estimate 2030 emissions, they will be equal to 2016 ones!
co2_pop <- merge(pop_iso3[pop_iso3$code != "",], co2[co2$code != "",])
co2_pop$adult <- 1000 * co2_pop$adult
co2_pop$emissions <- co2_pop$footprint
co2_pop$emissions[is.na(co2_pop$footprint)] <- co2_pop$territorial[is.na(co2_pop$footprint)]
co2_pop <- co2_pop %>% group_by(code) %>% pivot_wider(id_cols = c("code", "country"),  names_from = year, 
           values_from = c("territorial", "footprint", "emissions", "adult"), names_glue = "{.value}_{year}") %>% ungroup()
  
(A <- adult_pop_2015 <- sum(co2_pop$adult_2015)) # 5.45G (entire pop (not just 15+) 7.42G)
(adult_pop_2019 <- sum(co2_pop$adult_2019)) # 5.75G (entire pop (not just 15+) 7.76G)
(F_ <- adult_pop_2030 <- sum(co2_pop$adult_2030)) # 6.57G (entire pop (not just 15+) 8.54G)
(E <- co2_emissions_2015 <- sum(co2_pop$territorial_2015)) # 34.4G (footprint: 33.4G + NA)
(E <- co2_emissions_2019 <- sum(co2_pop$territorial_2019)) # 35.8G
co2_emissions_2030 <- 26.3e9
(R <- carbon_tax_revenues_2030 <- 2367e9) # in $, Based on $90/tCO2 TODO: global emissions in 2030
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
# /!\ Units of price are unknown, but probably constant 2017 dollar.

decrit(co2_pop$footprint_2015) # 98 out of 218 countries missing (while no missing for territorial)
all(co2_pop$country[is.na(co2_pop$footprint_2015)] == co2_pop$country[is.na(co2_pop$footprint_2019)])
co2_pop$country[is.na(co2_pop$footprint_2015)]
(sum(co2_pop$adult_2019[is.na(co2_pop$footprint_2019)])/adult_pop_2019) # 7.4%

# Assumption: emissions per adult (>15) will evolve in the same way in all countries
# base_year <- 2015
# co2_pop$demographic_evolution <- (co2_pop$adult_2030/co2_pop[[paste0("adult_", base_year)]]) * (sum(co2_pop[[paste0("adult_", base_year)]])/adult_pop_2030)
co2_pop$demographic_evolution_2015 <- (co2_pop$adult_2030/co2_pop$adult_2015) * (adult_pop_2015/adult_pop_2030)
co2_pop$demographic_evolution_2019 <- (co2_pop$adult_2030/co2_pop$adult_2019) * (adult_pop_2019/adult_pop_2030)
co2_pop$emissions_pc_2015 <- co2_pop$emissions_2015/co2_pop$adult_2015
co2_pop$emissions_pc_2019 <- co2_pop$emissions_2019/co2_pop$adult_2019
co2_pop$share_emissions_2015 <- co2_pop$emissions_2015/sum(co2_pop$emissions_2015)
co2_pop$share_emissions_2019 <- co2_pop$emissions_2019/sum(co2_pop$emissions_2019)
co2_pop$share_emissions_2030_wrong <- co2_pop$share_emissions_2015 * co2_pop$demographic_evolution_2015
co2_pop$share_emissions_2030_inadjusted <- co2_pop$share_emissions_2019 * co2_pop$demographic_evolution_2019
co2_pop$share_emissions_2030 <- co2_pop$share_emissions_2030_inadjusted/sum(co2_pop$share_emissions_2030_inadjusted)
co2_pop$share_emissions_2030_wrong_adjusted <- co2_pop$share_emissions_2030_wrong/sum(co2_pop$share_emissions_2030_wrong)
co2_pop$emissions_pc_2030_wrong <- co2_pop$share_emissions_2030 * co2_emissions_2030 / co2_pop$adult_2015
co2_pop$emissions_pc_2030 <- co2_pop$share_emissions_2030 * co2_emissions_2030 / co2_pop$adult_2030
# for (v in c("territorial", "footprint", "emissions")) {
#   co2_pop[[paste0(v, "_pc_2015")]] <- co2_pop[[paste0(v, "_2015")]]/co2_pop$adult_2015
#   co2_pop[[paste0(v, "_pc_2019")]] <- co2_pop[[paste0(v, "_2019")]]/co2_pop$adult_2019
#   co2_pop[[paste0("share_", v, "_2015")]] <- co2_pop[[paste0(v, "_2015")]]/sum(co2_pop[[paste0(v, "_2015")]])
#   co2_pop[[paste0("share_", v, "_2019")]] <- co2_pop[[paste0(v, "_2019")]]/sum(co2_pop[[paste0(v, "_2019")]])
#   co2_pop[[paste0("share_", v, "_2030_wrong")]] <- co2_pop[[paste0("share_", v, "_", base_year)]] * co2_pop$demographic_evolution
#   co2_pop[[paste0("share_", v, "_2030")]] <- co2_pop[[paste0("share_", v, "_2030_wrong")]]/sum(co2_pop[[paste0("share_", v, "_2030_wrong")]])
#   co2_pop[[paste0(v, "_pc_2030_wrong")]] <- co2_pop[[paste0("share_", v, "_2030")]] * co2_emissions_2030 / co2_pop$adult_2015
#   co2_pop[[paste0(v, "_pc_2030")]] <- co2_pop[[paste0("share_", v, "_2030")]] * co2_emissions_2030 / co2_pop$adult_2030
# }
co2_pop$revenues_pc_2030_wrong <- co2_pop$share_emissions_2030_wrong * carbon_tax_revenues_2030 / co2_pop$adult_2015/12 # TODO problem with the formula?
co2_pop$revenues_pc_2030_wrong_adjusted <- co2_pop$share_emissions_2030_wrong_adjusted * carbon_tax_revenues_2030 / co2_pop$adult_2015/12 # TODO problem with the formula?
co2_pop$revenues_pc_2030 <- co2_pop$share_emissions_2030 * carbon_tax_revenues_2030 / co2_pop$adult_2019/12
# sum(co2_pop$revenues_pc_2030 * co2_pop$adult_2030)
co2_pop$mean_gain_2030 <- 30 - co2_pop$revenues_pc_2030
co2_pop$median_gain_2030 <- 30 - 0.9*co2_pop$revenues_pc_2030
sum(co2_pop$share_emissions_2030)

countries_survey_oecd <- c("AUS", "CAN", "DNK", "FRA", "DEU", "ITA", "JPN", "MEX", "POL", "KOR", "ESP", "TUR", "GBR", "USA", "BRA", "CHN", "IND", "IDN", "ZAF", "UKR")
revenues_pc_oecd <- c(134, 105, 68, 46, 61, 42, 60, 34, 42, 72, 39, 40, 59, 128, 18, 38, 13, 16, 50, 32) # cf. oecd_climate/questionnairesboard row 63 and oecd_climate/questionnaires/net_gain_global_tax
mean_gain_oecd <- 30 - revenues_pc_oecd
emission_share_2015_oecd <- c(426.4, 547.9, 59.4, 445, 853.4, 423, 1361, 485.5, 273.8, 584.8, 293.8, 374.9, 575.8, 5794.5, 475.4, 7977.9, 1918.8, 484.6, 313.5, 262)/32276 # OECD bit.ly/37kSVUx EU28 12.3%, G20 82.7%
names(mean_gain_oecd) <- names(emission_share_2015_oecd) <- names(revenues_pc_oecd) <- countries_survey_oecd
euro_per_dollar <- 0.94
pound_per_dollar <- .82
median_loss_used <- c(15, 25, 5, 20, 85) # LCU/month
LCU_per_dollar <- c(rep(euro_per_dollar, 3), pound_per_dollar, 1)
revenues_pc <- (median_loss_used/LCU_per_dollar + 30)/0.9
names(median_loss_used) <- names(LCU_per_dollar) <- names(revenues_pc) <- countries

# Below is shown the discrepancy between GCP and OECD data, using the same formula
co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
# Below is shown the discrepancy between true GCP estimates and OECD survey values
co2_pop$revenues_pc_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
# Below is shown the difference between correct formula (using 2019) and wrong formula (using 2015 and not adjusting): less than 3% for DE, ES, FR, US, 11% for UK, max 23% for UA, 18% for ID, 15% for CN, 11% for AU
setNames(co2_pop$revenues_pc_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
# /!\ Below is shown the error due to the mistake in the formula used in the OECD survey (and for the U.S.): all revenues_pc are underestimated by 7% in OECD
setNames(co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]/revenues_pc
(median_loss_true <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]*0.9))
(mean_loss_true <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]))
median_loss_true <- c(5, 20, 5, 15, 75) # LCU/month FR (7) can be 10
mean_loss_true <- c(10, 25, 5, 15, 85) # LCU/month UK (17) can be 20

co2_pop[co2_pop$code %in% c("USA", "FRA", "DEU", "ESP", "GBR", "IND", "CHN"), # countries_survey_oecd
        c("code", "mean_gain_2030", "median_gain_2030", "emissions_pc_2015", "emissions_pc_2019", "emissions_pc_2030")]
co2_pop$mean_gain_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/mean_gain_oecd
co2_pop$share_emissions_2015[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/emission_share_2015_oecd
# /!\ Discrepancies in mean gain due to different 2015 emissions data (OECD in OECD survey vs. Global carbon project (GCP) / Peters et al. (2012) here).
#     Above all for Ukraine (GCP data is 35% lower than OECD's), Denmark (21%), Australia (14%) and France (11%).
# OECD data/survey features higher carbon footprint for high-income countries and lower for middle-income, compared to current data.
# TODO: why is there a discrepancy? Is LULUCF accounted for? None of them includes deforestation, only fossil fuels.
# TODO: share of winners per country
# TODO: more accurate assumption/computations

co2_pop$country[co2_pop$country == "United States"] <- "USA"
co2_pop$country[co2_pop$country == "United Kingdom"] <- "UK"
co2_pop$country[co2_pop$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
co2_pop$country[co2_pop$country == "Congo"] <- "Republic of Congo"
co2_pop$country[co2_pop$country == "Cote d'Ivoire"] <- "Ivory Coast"
co2_pop$country[co2_pop$country == "Czechia"] <- "Czech Republic"
wtd.quantile(co2_pop$mean_gain_2030, weights = co2_pop$adult_2019, probs = seq(0, 1, 1/6)) # c(-Inf, -40, -20, -10, 0, 10, 20, Inf)
thresholds_map <- c(-Inf, -80, -40, -20, -10, 0, 10, 15, 20, 25, Inf)
plot_world_map("mean_gain_2030", breaks = thresholds_map, format = 'pdf', trim = F, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Average net gain\nper capita\nfrom the GCS\n(in $/month)", fill_na = T,
               limits = c(-30, 30), save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))
