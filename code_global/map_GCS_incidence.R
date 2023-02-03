pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop <- pop[pop$Time %in% c(2015, 2019, 2023, 2030),]
pop <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
pop_location <- aggregate(adult ~ year + country, data = pop, FUN = sum)
# pop_iso2 <- aggregate(adult ~ year + ISO2_code, data = pop, FUN = sum)
pop_iso3 <- aggregate(adult ~ year + code, data = pop, FUN = sum)

# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions
co2 <- read.csv("../data/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2016, 2019),]
temp <- co2[co2$Year == 2019,]
temp$Year <- 2023
co2 <- rbind(co2, temp)
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
co2_pop$revenues_pc_2030_wrong_adjusted_alt <- co2_pop$share_emissions_2030_wrong_adjusted * carbon_tax_revenues_2030 / co2_pop$adult_2019/12 # TODO problem with the formula?
co2_pop$revenues_pc_2030 <- co2_pop$share_emissions_2030 * carbon_tax_revenues_2030 / co2_pop$adult_2019/12
co2_pop$revenues_pc_2030_alt <- co2_pop$share_emissions_2030 * carbon_tax_revenues_2030 / co2_pop$adult_2030/12
co2_pop$revenues_pc_2030_23 <- co2_pop$share_emissions_2030 * carbon_tax_revenues_2030 / co2_pop$adult_2023/12
# sum(co2_pop$revenues_pc_2030 * co2_pop$adult_2030)
co2_pop$mean_gain_2030 <- 30 - co2_pop$revenues_pc_2030_alt
co2_pop$mean_gain_2023 <- 30 - co2_pop$revenues_pc_2030_23
co2_pop$median_gain_2030 <- 30 - 0.9*co2_pop$revenues_pc_2030_alt
co2_pop$median_gain_2023 <- 30 - 0.9*co2_pop$revenues_pc_2030_23
co2_pop$median_gain_2030 <- 30 - 0.9*co2_pop$revenues_pc_2030
co2_pop$median_gain_2015 <- 30 - 0.9*co2_pop$revenues_pc_2030_wrong_adjusted
co2_pop$mean_gain_2015 <- 30 - co2_pop$revenues_pc_2030_wrong_adjusted
sum(co2_pop$share_emissions_2030)

countries_survey_oecd <- c("AUS", "CAN", "DNK", "FRA", "DEU", "ITA", "JPN", "MEX", "POL", "KOR", "ESP", "TUR", "GBR", "USA", "BRA", "CHN", "IND", "IDN", "ZAF", "UKR")
revenues_pc_oecd <- c(134, 105, 68, 46, 61, 42, 60, 34, 42, 72, 39, 40, 59, 128, 18, 38, 13, 16, 50, 32) # cf. oecd_climate/questionnairesboard row 63 and oecd_climate/questionnaires/net_gain_global_tax
mean_gain_oecd <- 30 - revenues_pc_oecd
emission_share_2015_oecd <- c(426.4, 547.9, 59.4, 445, 853.4, 423, 1361, 485.5, 273.8, 584.8, 293.8, 374.9, 575.8, 5794.5, 475.4, 7977.9, 1918.8, 484.6, 313.5, 262)/32276 # OECD bit.ly/37kSVUx EU28 12.3%, G20 82.7% 2015 latest date available
names(mean_gain_oecd) <- names(emission_share_2015_oecd) <- names(revenues_pc_oecd) <- countries_survey_oecd
euro_per_dollar <- 0.94
pound_per_dollar <- .82
median_loss_used <- c(15, 25, 5, 20, 85) # LCU/month
LCU_per_dollar <- c(rep(euro_per_dollar, 3), pound_per_dollar, 1)
revenues_pc <- (median_loss_used/LCU_per_dollar + 30)/0.9
names(median_loss_used) <- names(LCU_per_dollar) <- names(revenues_pc) <- countries

co2_pop$share_emissions_2015_oecd[co2_pop$code %in% countries_survey_oecd] <- emission_share_2015_oecd[co2_pop$code[co2_pop$code %in% rev(countries_survey_oecd)]]
co2_pop$share_emissions_2015_oecd[co2_pop$code == "REU"] <- 1 - sum(emission_share_2015_oecd, na.rm = T) # Allocates remaining shares to Reunion so that all shares sum to 1 (which will be used later on)
co2_pop$share_emissions_2030_oecd <- co2_pop$share_emissions_2015_oecd * co2_pop$demographic_evolution_2015
co2_pop$share_emissions_2030_oecd_corrected <- co2_pop$share_emissions_2030_oecd / sum(co2_pop$share_emissions_2030_oecd, na.rm = T)
co2_pop$revenues_pc_oecd <- co2_pop$share_emissions_2030_oecd * carbon_tax_revenues_2030 / co2_pop$adult_2015/12
co2_pop$revenues_pc_oecd_corrected <- co2_pop$share_emissions_2030_oecd_corrected * carbon_tax_revenues_2030 / co2_pop$adult_2015/12

# In OECD survey, due to a mistake, we underestimate the revenues pc by 7% everywhere.
# With GCP data instead of OECD, we find lower carbon footprint for EU/US, up to 11% for France. This kind of compensate the OECD survey mistake.
# Taking 2019 rather than 2015 as a baseline reduces revenues pc by 5-17%, especially in the UK (17%)
# Estimating the revenues pc in 2030 rather than 2019 reduces it by 0% (DE) to 8% (US) in our 5 countries.
# Below is shown the discrepancy between the correct formula using GCP data with 2015 baseline vs. OECD survey figures (wrong formula, OECD survey with 2019 baseline): error is limited to -5, +4% in our 5 countries
co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
co2_pop$revenues_pc_2030_wrong_adjusted_alt[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
# Below is shown the discrepancy between revenues pc used in OECD survey and recomputed, probably due to updated estimates of demographic evolution. Small except for Turkey (7%)
co2_pop$revenues_pc_oecd[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
# Below is shown the discrepancy between GCP and OECD data, using the same formula
co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
# Below is shown the discrepancy between using 2019 and 2015 as a baseline: revenues pc is 5% (ES) to 17% (UK) lower when using 2019
setNames(co2_pop$revenues_pc_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
# Below is shown the discrepancy between true GCP estimates and OECD survey values
co2_pop$revenues_pc_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]/revenues_pc
# Below is shown the difference between correct formula (using 2019) and wrong formula (using 2015 and not adjusting): less than 3% for DE, ES, FR, US, 11% for UK, max 23% for UA, 18% for ID, 15% for CN, 11% for AU
setNames(co2_pop$revenues_pc_2030[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
# /!\ Below is shown the error due to the mistake in the formula used in the OECD survey (and for the U.S.): all revenues_pc are underestimated by 7% in OECD
setNames(co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
co2_pop$revenues_pc_oecd_corrected[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_oecd[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]
# Below is shown the discrepancy in revenues pc if we estimate the scheme in 2030 rather than 2019 (i.e. divide by 2030 population)
setNames((co2_pop$revenues_pc_2030_alt/co2_pop$revenues_pc_2030)[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
# Below is shown the discrepancy between revenues pc as estimated in the paper (for 2015) and as truly in effect in 2030
co2_pop$revenues_pc_2030_alt[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/revenues_pc_oecd
setNames(co2_pop$revenues_pc_2030_alt[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))]/co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_survey_oecd, function(c) which(co2_pop$code == c))], countries_survey_oecd)
(median_loss <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030_wrong_adjusted_alt[sapply(countries_names, function(c) which(co2_pop$country == c))]*0.9))
(median_loss_true <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]*0.9))
(mean_loss_true <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030[sapply(countries_names, function(c) which(co2_pop$country == c))]))
(mean_loss_4 <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030_alt[sapply(countries_names, function(c) which(co2_pop$country == c))]))
(mean_loss_1 <- LCU_per_dollar*(30 - co2_pop$revenues_pc_2030_wrong_adjusted[sapply(countries_names, function(c) which(co2_pop$country == c))]))
median_loss_true <- c(5, 20, 5, 15, 75) # LCU/month FR (7) can be 10
mean_loss_true <- c(10, 25, 5, 15, 85) # LCU/month UK (17) can be 20
# Used: 15 => 10, 25, 5, 20, 85
mean_loss_4/LCU_per_dollar
mean_loss_1/LCU_per_dollar

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
thresholds_map <- c(-Inf, -70, -30, -20, -10, 0, 10, 15, 20, 25, Inf)
# thresholds_map <- c(-Inf, -100, -70, -40, -20, -10, 0, 5, 10, 15, 20, 25, Inf)
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

# Also
plot_world_map("mean_gain_2023", breaks = thresholds_map, format = c('pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))

plot_world_map("median_gain_2023", breaks = thresholds_map, format = c('pdf', 'png', 'svg'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Median net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))

plot_world_map("mean_gain_2015", breaks = thresholds_map, format = c('pdf', 'png', 'svg'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Average net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))

plot_world_map("median_gain_2030", breaks = thresholds_map, format = c('pdf', 'png', 'svg'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Median net\ngain per capita\nfrom the GCS\n(in $/month)", fill_na = T,
               save = T) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030))
