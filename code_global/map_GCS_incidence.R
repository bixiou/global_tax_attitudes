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
pg2 <- read.csv("../data/poverty_gap_2-15.csv") # Poverty gap at $2.15 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.GAPS March 1, 2023 
pg2$code <- pg2$Country.Code
pg2 <- extract_last_year(df = pg2, cols = paste0("X", 1960:2021), var_name = "pg2", keep = c("Country.Name", "code"))
pg2$Country.Name[pg2$code %in% co2_pop$code][is.na(pg2$pg2[pg2$code %in% co2_pop$code])] # No data (for any year) for Afghanistan, Cambodia, Cuba, Dominica, Eritrea, Equatorial Guinea, Libya, North Korea. 

pg4 <- read.csv("../data/poverty_gap_3-65.csv") # Poverty gap at $3.65 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.LMIC.GP March 1, 2023 
pg4$code <- pg4$Country.Code
pg4 <- extract_last_year(df = pg4, cols = paste0("X", 1960:2021), var_name = "pg4", keep = c("Country.Name", "code"))

pg7 <- read.csv("../data/poverty_gap_6-85.csv") # Poverty gap at $6.85 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.UMIC.GP March 1, 2023 
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

pg <- merge(merge(pg2, merge(pg4, pg7)), GDPpcPPP[, c("Country.Name", "code", "GDPpcPPP")])

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

qplot(log10(GDPpcPPP), log10(pg7), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $6.85 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg4), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $3.65 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE, se = F) + theme_bw() 
qplot(log10(GDPpcPPP), log10(pg2), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $2.15 a day (2017 PPP) (%)", show.legend = FALSE) + 
  geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE, se = F) + theme_bw() 

# qplot(log10(GDPpcPPP), log10(gap), data = pg, size = adult_2023, xlab = "log10 of GDP per capita, PPP (constant 2017 international $)", ylab = "log10 of Poverty gap at $2.15 a day (2017 PPP) (%)", show.legend = FALSE) + 
#   geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw() +
#   geom_line(aes(y = log10(predicted_gap_simplfied)), color = "red", show.legend = FALSE) + labs(fill = "Adult population (2023)")
# with(pg, symbols(x=GDPpcPPP, y=gap, circles=adult_2023, inches=1/3,  ann=F, bg="steelblue2", fg=NULL))
# qplot(GDPpcPPP, gap, data = pg, size = adult_2023) + geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw() + coord_trans(x="log10")
# qplot(log10(GDPpcPPP), gap, data = pg, size = adult_2023) + geom_smooth(method = "lm",  mapping = aes(weight = adult_2023), color = "black", show.legend = FALSE) + theme_bw()
# qplot(GDPpcPPP, gap, data = pg, size = adult_2023) + geom_line(aes(y = predicted_gap), color = "red", show.legend = FALSE) + theme_bw() + coord_trans(x="log10")

# TODO: set it to 0 for high-income countries
pg$share_revenues7 <- pg$predicted_gap7 * pg$adult_2023 * pg$GDPpcPPP # TODO fill missing values and use full pop instead of adults (also below)
pg$share_revenues7 <- pg$share_revenues7/sum(pg$share_revenues7, na.rm = T)
pg$share_revenues4 <- pg$predicted_gap4 * pg$adult_2023 * pg$GDPpcPPP 
pg$share_revenues4 <- pg$share_revenues4/sum(pg$share_revenues4, na.rm = T)
pg$share_revenues2 <- pg$predicted_gap2 * pg$adult_2023 * pg$GDPpcPPP 
pg$share_revenues2 <- pg$share_revenues2/sum(pg$share_revenues2, na.rm = T)
sort(setNames(pg$share_revenues7, pg$country))
sort(setNames(pg$share_revenues4, pg$country))
sort(setNames(pg$share_revenues2, pg$country))
cor(pg$share_revenues7, pg$share_revenues4, use = "complete.obs") # .9996
cor(pg$share_revenues7, pg$share_revenues2, use = "complete.obs") # .959

wealth_tax_revenues <- 0.0085*96e12 # From a 2% tax above $5 million, cf. Chancel et al. (2022) https://wid.world/world-wealth-tax-simulator/
pooled_revenues <- 0.33
pg$wealth_tax_rev_pc <- pooled_revenues * wealth_tax_revenues * pg$share_revenues7 / pg$adult_2023 # per year
sort(setNames(pg$wealth_tax_rev_pc/12, pg$country)) # per month: $4.6 in India, 2.8 in China, 14.6 in RDC, 1.3 in U.S.
