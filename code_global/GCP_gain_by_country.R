##### Population data #####
years <- c(2005, seq(2010, 2100, 10))
EU28_countries <- countries_names <- countries_EU <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
EU27_countries <- countries_names_fr <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
African_countries <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "SWZ", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
discount_rate <- .03
euro_per_dollar <- 0.94
yrs <- c(2025, seq(2030, 2080, 10))

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


##### GDP pc data #####
GDPpc <- read.csv("../data/GDPpc_2015$_nominal.csv") # GDP per capita (constant 2015 US$) https://data.worldbank.org/indicator/NY.GDP.PCAP.KD March 1, 2023 (last year available is 2021)
GDPpc$code <- GDPpc$Country.Code
GDPpc <- rbind(data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2015, "year" = 2015), data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2019, "year" = 2019))
GDPpc$gdp_pc[GDPpc$code %in% c("ERI", "PRK", "SSD", "VEN", "YEM")] <- c(715, 654, 467, 3640, 702) # Impute data from other sources for Eritrea (IMF, 2023), North Korea (IMF, 2021), South Sudan (IMF, 2023), Venezuela (IMF, 2023), Yemen (WB, 2018)
co2 <- merge(co2, GDPpc)
# /!\ Beware, gdp_pc_2019 is nominal while gdp_pc_2020:2100 is in PPP!!


##### Merge datasets #####
# co2$year[co2$year == 2016] <- 2030 # Beware, before we estimate 2030 emissions, they will be equal to 2016 ones!
for (y in c(years)) {
  temp_y <- co2[co2$year == 2015,]
  temp_y$year <- y
  for (v in c("territorial", "footprint")) temp_y[[v]] <- NA
  co2 <- rbind(co2, temp_y)
}
cp <- merge(pop_iso3[pop_iso3$code != "",], co2[co2$code != "",], all = T)
country_code <- setNames(co2$country[co2$year == 2019], co2$code[co2$year == 2019])
cp$country <- country_code[cp$code]
cp$adult <- 1000 * cp$adult
cp$pop <- 1000 * cp$pop
cp$recipient <- cp$adult + (cp$pop - cp$adult)/2  
cp$emissions <- cp$footprint
cp$missing_footprint <- is.na(cp$footprint)
# (sum(cp$adult_2019[is.na(cp$footprint_2019)])/adult_pop_2019) # 7.4% of global footprint data missing
cp$emissions[is.na(cp$footprint)] <- cp$territorial[is.na(cp$footprint)] # imputing territorial emissions for those countries
cp <- cp %>% group_by(code) %>% pivot_wider(id_cols = c("code", "country"),  names_from = year,
                                                      values_from = c("territorial", "footprint", "emissions", "adult", "pop", "recipient", "gdp_pc", "missing_footprint"), names_glue = "{.value}_{year}") %>% ungroup()
# /!\ Beware, data is valid only in 2015 and 2019 for gdp_pc (and it is in nominal)
cp$missing_footprint <- cp$missing_footprint_2019
cp <- cp[, !colnames(cp) %in% c(paste0("missing_footprint_", c(2015, 2019, 2023, years)), paste0("gdp_pc_", years))]
cp <- cp[!cp$country %in% c("World", "Kosovo", "Aruba", NA), sapply(names(cp), function(v) any(!is.na(cp[[v]])))]
cp$gdp_2019 <- cp$gdp_pc_2019 * cp$pop_2019 # nominal
cp$country_map <- cp$country
cp$country_map[cp$country == "United States"] <- "USA"
cp$country_map[cp$country == "United Kingdom"] <- "UK"
cp$country_map[cp$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
cp$country_map[cp$country == "Congo"] <- "Republic of Congo"
cp$country_map[cp$country == "Cote d'Ivoire"] <- "Ivory Coast"
cp$country_map[cp$country == "Czechia"] <- "Czech Republic"
cp$country_map[cp$country == "Eswatini"] <- "Swaziland"
cp$share_territorial_2019 <- cp$territorial_2019/sum(cp$territorial_2019)
# cp <- merge(pg[,c("code", "GDPpcPPP")], cp)

# write.csv(cp[,c("code", "country", "country_map")], "../data/country_code_mapping.csv", row.names = FALSE)
# countries_code <- read.csv("../data/country_code_mapping.csv")

##### Loads Carbon price from SSP2-2.6 #####
SSPs <- read.csv("../data/SSPs.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/iam_v2/SSP_IAM_V2_201811.csv.zip
if (!exists("carbon_price")) carbon_price <- list() # Prices coincide in all regions after 2030, is between 119 (REF) - 150 (OECD) for 2030 (Asia: 144) and 0 (LAM, REF) - 38 (MAF) - 47 (Asia) - 55 (OECD) for 2020
carbon_price$ssp2_26 <- setNames(sapply(years, function(y) SSPs[SSPs$MODEL == "IMAGE" & SSPs$SCENARIO == "SSP2-26" & SSPs$VARIABLE == "Price|Carbon" & SSPs$REGION == "R5.2ASIA", paste0("X", y)]), years) # "US$2005/t CO2"
rm(SSPs)

##### Country-downscaled trajectories #####
ssp_country <- read.csv("../data/PMSSPIE.csv") # Gütschow et al. (2021) extracted from PMSSPIE_05Feb20.csv, https://zenodo.org/record/3638137 # Territorial CO2 emissions from non-LULUCF sectors
ssp_country <- ssp_country %>% .[!.$country %in% c("EARTH", "ANNEXI", "AOSIS", "BASIC", "EU28", "LDC", "NONANNEXI", "UMBRELLA", "MAC"),]

compute_carbon_debt <- function(start = 1990, end = 2029, df = sm, discounted = FALSE) {
  # When discounted, we discount emissions at t by world GDP at t over world GDP at end. But it makes no sense to discount, one should use the current year.
  # /!\ We replace NA by 0
  if (start != end) df[[paste0("emissions_", start, "_", end)]] <- rowSums(df[, paste0("emissions_", start:end)])
  name_var <- paste0("carbon_debt_", if (discounted) "discounted_" else "", start, "_", end)
  df[[name_var]] <- 0
  for (y in start:end) {
    df[[name_var]] <- df[[name_var]] + (sum(df[[paste0("gdp_", y)]], na.rm = T)/sum(df[[paste0("gdp_", end)]], na.rm = T))^discounted * (df[[paste0("emissions_", y)]] - 
           df[[paste0("pop_", y)]]*sum(df[[paste0("emissions_", y)]][!is.na(df[[paste0("pop_", y)]])], na.rm = T)/sum(df[[paste0("pop_", y)]][!is.na(df[[paste0("emissions_", y)]])], na.rm = T))
    df[[name_var]][is.na(df[[name_var]])] <- 0
  }
  return(df)
}

prepare_ssp_country <- function(scenario = "SSP226MESGB", ssps = ssp_country, df = cp, keep_from_df = copy_from_cp) { # GDP is in PPP (no nominal data available)
  if (exists("pop_iso3") & !"TWN" %in% df$code) {
    twn <- df[df$code == "KOR",]
    twn$code <- "TWN"
    twn$country <- twn$country_map <- "Taiwan"
    for (y in c(2023, seq(2020, 2100, 10))) { 
      twn[[paste0("pop_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$pop[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$pop[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
      twn[[paste0("adult_", y)]] <- 1e3 * barycenter(y, y - y %% 10, 10*ceiling(y/10), pop_iso3$adult[pop_iso3$year == y - y %% 10 & pop_iso3$code == "TWN"], pop_iso3$adult[pop_iso3$year == 10*ceiling(y/10) & pop_iso3$code == "TWN"])
      twn[[paste0("recipient_", y)]] <- twn[[paste0("adult_", y)]] + (twn[[paste0("pop_", y)]] - twn[[paste0("adult_", y)]])/2
    } 
    df <- rbind(df, twn)
  }
  ssps <- ssps[ssps$country %in% df$code & !ssps$country %in% c("FSM", "GRD"),] # "SSD", "TWN", "PRK", 
  ssp <- data.frame(code = unique(ssps$country))
  for (y in 1850:2100) { # Years span 1850:2100
    ssp[[paste0("pop_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "POP"], ssps$country[ssps$scenario == scenario & ssps$entity == "POP"])[ssp$code]
    ssp[[paste0("gdp_", y)]] <- 1e6 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "GDPPPP"], ssps$country[ssps$scenario == scenario & ssps$entity == "GDPPPP"])[ssp$code]  # /!\ It is in PPP (contrary to old code with IIASA SSPs)
    # Territorial CO2 emissions from non-LULUCF sectors
    ssp[[paste0("emissions_", y)]] <- 1e3 * setNames(ssps[[paste0("X", y)]][ssps$scenario == scenario & ssps$entity == "CO2"], ssps$country[ssps$scenario == scenario & ssps$entity == "CO2"])[ssp$code]
    # Add North Korea data
    if (y >= 2020) ssp[[paste0("emissions_", y)]][ssp$code == "PRK"] <- ssp[[paste0("emissions_", y)]][ssp$code == "BLR"] # Using Belarus, a country with similar 2022 fossil emissions from https://en.wikipedia.org/wiki/List_of_countries_by_carbon_dioxide_emissions
    if (y >= 2020) ssp[[paste0("gdp_", y)]][ssp$code == "PRK"] <- ssp[[paste0("gdp_", y)]][ssp$code == "MDG"] # Using Madagascar, a country with similar GDP PPP https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)
    # ssp[[paste0("emissions_", y)]][ssp$code == "PRK"] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("emissions_", y - y %% 10)]][df$code == "PRK"], df[[paste0("emissions_", 10*ceiling(y/10))]][df$code == "PRK"])
    # ssp[[paste0("gdp_", y)]][ssp$code == "PRK"] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("gdp_", y - y %% 10)]][df$code == "PRK"], df[[paste0("gdp_", 10*ceiling(y/10))]][df$code == "PRK"])
    # Add South Sudan and Taiwan data
    if (y >= 2020) for (c in c("TWN", "SSD")) ssp[[paste0("pop_", y)]][ssp$code == c] <- barycenter(y, y - y %% 10, 10*ceiling(y/10), df[[paste0("pop_", y - y %% 10)]][df$code == c], df[[paste0("pop_", 10*ceiling(y/10))]][df$code == c])
  } # Scales up df$adult by ssp$pop/df$pop
  for (y in c(2023, seq(2020, 2100, 10))) ssp[[paste0("adult_", y)]][match.nona(df$code, ssp$code)] <- ssp[[paste0("pop_", y)]][match.nona(df$code, ssp$code)] * (df[[paste0("adult_", y)]]/df[[paste0("pop_", y)]])[df$code %in% ssp$code]
  for (y in c(2023, seq(2020, 2100, 10))) ssp[[paste0("recipient_", y)]][match.nona(df$code, ssp$code)] <- ssp[[paste0("pop_", y)]][match.nona(df$code, ssp$code)] * (df[[paste0("recipient_", y)]]/df[[paste0("pop_", y)]])[df$code %in% ssp$code]
  for (y in 2020:2100) { # Interpolate adult_ from pop_ and df$adult/df$pop
    y_prev <- 10*floor(y/10)
    y_next <- 10*ceiling(y/10)
    if (y > 2023 & y < 2030) y_prev <- 2023
    if (y %in% c(2021, 2022)) y_next <- 2023
    lambda <- (y - y_prev)/10
    ssp[[paste0("adult_", y)]] <- ssp[[paste0("pop_", y)]] * ((1 - lambda) * (ssp[[paste0("adult_", y_prev)]]/ssp[[paste0("pop_", y_prev)]]) + lambda * (ssp[[paste0("adult_", y_next)]]/ssp[[paste0("pop_", y_next)]]))
    ssp[[paste0("recipient_", y)]] <- ssp[[paste0("adult_", y)]] + (ssp[[paste0("pop_", y)]] - ssp[[paste0("adult_", y)]])/2
    ssp[[paste0("emissions_pa_", y)]] <- ssp[[paste0("emissions_", y)]]/ssp[[paste0("adult_", y)]]
    ssp[[paste0("emissions_pc_", y)]] <- ssp[[paste0("emissions_", y)]]/ssp[[paste0("pop_", y)]]
    ssp[[paste0("gdp_pa_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("adult_", y)]] # /!\ It is in PPP (contrary to old code with IIASA SSPs), same below
    ssp[[paste0("gdp_pc_", y)]] <- ssp[[paste0("gdp_", y)]]/ssp[[paste0("pop_", y)]] 
    ssp[[paste0("gdp_pc_over_mean_", y)]] <- ssp[[paste0("gdp_pc_", y)]]/wtd.mean(ssp[[paste0("gdp_pc_", y)]], ssp[[paste0("pop_", y)]], na.rm = T)
  }
  ssp$gdp_ppp_now <- ssp$gdp_ppp_2023 
  ssp$gdp_pc_base_year <- ssp$gdp_ppp_pc_2023
  for (v in intersect(keep_from_df, names(df))) ssp[[v]][match.nona(df$code, ssp$code)] <- df[[v]][df$code %in% ssp$code]
  ssp$share_emissions_2023 <- ssp$emissions_2023/sum(ssp$emissions_2023, na.rm = T)
  ssp$share_territorial_2019[ssp$code == "TWN"] <- ssp$share_emissions_2023[ssp$code == "TWN"]
  ssp$share_territorial_2019 <- ssp$share_territorial_2019/sum(ssp$share_territorial_2019, na.rm = T)
  ssp <- compute_carbon_debt(start = 1990, end = 2029, df = ssp)
  ssp <- compute_carbon_debt(start = 1990, end = 2024, df = ssp)
  ssp <- compute_carbon_debt(start = 1850, end = 2024, df = ssp)
  ssp <- compute_carbon_debt(start = 1960, end = 2024, df = ssp)
  return(ssp)
}

compute_npv <- function(var = "gain_pa_", discount_rate = .03, start = 2030, end = 2100, data = cp, decadal = FALSE) {
  # /!\ If decadal == T, NPV is computed on 2020-2100. 
  if (decadal) return(rowSums(sapply(2:10, function(i) { return(10*data[[paste0(var, 2000+10*i)]]/((1+discount_rate)^10)^(i-2)) })))
  else return(rowSums(sapply(start:end, function(i) { return(data[[paste0(var, i)]]/(1+discount_rate)^(i-start)) })))
}
compute_gain_given_parties <- function(parties = df$code, df = sm, return = "df", beneficiary = "adult_", discount = .03, 
                                       ssp_name = "ssp2_26_country", start = 2025, end = 2100, max_gain = Inf, full_part_threshold = 2, opt_out_threshold=1.5) {
  # Uses large_footprint_, optout_right_, revenues_pa_, adult_, gdp_pc_, pop_, pop_, emissions_pa_, carbon_price[[ssp_name]]
  max_gain_as_fraction <- max_gain
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
  basic_income <- basic_income_adj <- c()
  if (!any(df[[paste0("large_footprint_", start)]] * (df[[paste0("optout_right_", y)]] < 1) * (df$code %in% parties))) warning("/!\\ Error: All countries opt out, there is no international transfer.")
  for (y in start:end) { 
    if (max_gain_as_fraction < 1) max_gain <- max_gain_as_fraction*df[[paste0("gdp_pb_", y)]]
    else max_gain <- rep(max_gain_as_fraction, nrow(df))
    yr <- as.character(y)
    df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
    temp <- rep(T, nrow(df)) # df$code %in% parties # average_revenues is average emissions_pa * carbon_price while basic_income is adjusted for participation_rate due to opt-out and anti-regressive mechanism
    while (any((temp != df[[paste0("large_footprint_", y)]])[!is.na(df[[paste0("large_footprint_", y)]])])) { # TODO? move after capping (to have consistent basic_income)
      temp <- df[[paste0("large_footprint_", y)]]
      basic_income[yr] <- wtd.mean(df[[paste0("revenues_pb_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0(beneficiary, y)]])
      basic_income[yr] <- basic_income[yr] + wtd.mean(pmax(0, basic_income[yr] - df[[paste0("revenues_pb_", y)]] - max_gain), df[[paste0("participation_rate_", y)]] * df[[paste0(beneficiary, y)]])
      df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pb_", y)]] > basic_income[yr])
      df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
    } 
    df[[paste0("gain_optout_", y)]] <- df[[paste0("participation_rate_", y)]] * (basic_income[yr] - df[[paste0("revenues_pb_", y)]])
    
    # Adjusted to avoid high-income receiving money. 
    y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]])
    e_bar <- wtd.mean(df[[paste0("emissions_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]]) 
    lambda <- pmax(0, pmin(1, ((full_part_threshold+.2)*y_bar - df[[paste0("gdp_pc_", y)]])/(.2*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
    lambda[is.na(lambda)] <- 1
    df[[paste0("share_basic_income_", y)]] <- lambda + pmin(1, df[[paste0("emissions_pc_", y)]]/(1.3*e_bar))*(1-lambda)
    lower_basic_income_ctries <- lambda < 1 & no.na(df[[paste0("emissions_pc_", y)]] < 1.3*e_bar, rep = FALSE, num_as_char = FALSE)
    if (y %in% yrs) df[[paste0("lower_basic_income_", y)]] <- 1*lower_basic_income_ctries
    # /!\ Basic income is expressed in pb (per beneficiary) but gain_adj in pa (per adult).
    basic_income[yr] <- wtd.mean(df[[paste0("revenues_pb_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0(beneficiary, y)]])
    basic_income_adjusted <- basic_income[yr] * (1 + sum((1 - df[[paste0("share_basic_income_", y)]]) * df[[paste0(beneficiary, y)]] * df[[paste0("participation_rate_", y)]] * lower_basic_income_ctries) / sum(df[[paste0(beneficiary, y)]] * df[[paste0("participation_rate_", y)]] * !lower_basic_income_ctries))
    
    # Capping basic_income_adj at max_gain TODO: make sure that high-income countries still do not receive money
    high_income <- df[[paste0("gdp_pc_", y)]] > opt_out_threshold*y_bar
    excess_revenue <- sum(pmax(0, basic_income_adjusted - df[[paste0("revenues_pb_", y)]] - max_gain) * df[[paste0(beneficiary, y)]])
    temp <- rep(F, nrow(df))
    above_max_gain_ctries <- df[[paste0("participation_rate_", y)]] * (basic_income_adjusted - df[[paste0("revenues_pb_", y)]]) > max_gain
    basic_income_adj[yr] <- basic_income_adjusted + excess_revenue/sum(df[[paste0(beneficiary, y)]] * df[[paste0("participation_rate_", y)]] * (!lower_basic_income_ctries) * (!above_max_gain_ctries) * (!high_income))
    while (any(temp != above_max_gain_ctries)) { # Prevents the increased (due to capping) basic income to make some marginal countries' gain_adj exceeding the cap
      temp <- above_max_gain_ctries
      above_max_gain_ctries <- (df[[paste0("participation_rate_", y)]] * (basic_income_adjusted - df[[paste0("revenues_pb_", y)]]) > max_gain)
      excess_revenue <- excess_revenue - sum(pmax(0, max_gain - (basic_income_adjusted - df[[paste0("revenues_pb_", y)]])) * df[[paste0(beneficiary, y)]] * above_max_gain_ctries * (!temp))
      basic_income_adj[yr] <- basic_income_adjusted + excess_revenue/sum(df[[paste0(beneficiary, y)]] * df[[paste0("participation_rate_", y)]] * (!lower_basic_income_ctries) * (!above_max_gain_ctries) * (!high_income))
    }
    
    df[[paste0("gain_adj_", y)]][lower_basic_income_ctries] <- (df[[paste0("participation_rate_", y)]] * (basic_income[yr] * df[[paste0("share_basic_income_", y)]] - df[[paste0("revenues_pb_", y)]]))[lower_basic_income_ctries]
    df[[paste0("gain_adj_", y)]][!lower_basic_income_ctries] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adjusted - df[[paste0("revenues_pb_", y)]]))[!lower_basic_income_ctries]
    df[[paste0("gain_adj_", y)]][!high_income] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pb_", y)]]))[!high_income]
    # df[[paste0("gain_adj_", y)]][!lower_basic_income_ctries] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pb_", y)]]))[!lower_basic_income_ctries]
    if (all(max_gain < Inf)) df[[paste0("gain_adj_", y)]][above_max_gain_ctries] <- max_gain[above_max_gain_ctries]
    # df[[paste0("gain_adj_", y)]] <- pmin(max_gain, df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pb_", y)]]))
    df[[paste0("gain_adj_", y)]] <- df[[paste0("gain_adj_", y)]] * df[[paste0(beneficiary, y)]]/df[[paste0("adult_", y)]] 
    df[[paste0("gain_adj_over_gdp_", y)]] <- df[[paste0("gain_adj_", y)]]/df[[paste0("gdp_pa_", y)]]
    # df[[paste0("gain_euro_", y)]] <- df[[paste0("gain_adj_", y)]]*euro_per_dollar
    
    df[[paste0("share_revenues_lost_", y)]] <- ifelse(df[[paste0("revenues_pb_", y)]] > 0, pmax(0, (df[[paste0("revenues_pb_", y)]] - basic_income_adj[yr])/df[[paste0("revenues_pb_", y)]]), 0)
    df[[paste0("share_basic_income_collected_", y)]] <- df[[paste0("revenues_pb_", y)]]/basic_income_adj[yr]
    df[[paste0("basic_income_over_revenues_", y)]] <- basic_income_adj[yr]/df[[paste0("revenues_pb_", y)]]
    df[[paste0("rights_equivalent_", y)]] <- (df[[paste0("gain_adj_", y)]] + df[[paste0("revenues_pa_", y)]])*df[[paste0("adult_", y)]]/carbon_price[[ssp_name]][yr]
  }
  
  df$npv_pa_gcs <- compute_npv("gain_pa_", discount = discount, data = df, decadal = FALSE) # /!\ Beware, this is only valid for universal participation scenario
  df$npv_pa_gcs_adj <- compute_npv("gain_adj_", discount = discount, data = df, decadal = FALSE)
  df$npv_over_gdp_gcs <- df$npv_pa_gcs/compute_npv("gdp_pa_", discount = discount, data = df, decadal = FALSE) # this formula corresponds to the % loss in consumption computed in Balanced Growth Equivalent of Stern et al. (07)
  df$npv_over_gdp_gcs_adj <- df$npv_pa_gcs_adj/compute_npv("gdp_pa_", discount = discount, data = df, decadal = FALSE)

  return(df)
}

create_var_ssp <- function(ssp = NULL, df = sm, CC_convergence = 2040, discount = .03, opt_out_threshold = 1.5, full_part_threshold = 2, scenario = "all_countries", 
                           beneficiary = "adult_", BAU = bau, max_gain = Inf) { # message is only for ssp2 , region = message_region_by_code
  # beneficiary (of the basic income) can be: adult_ (age >= 15) or recipient_ (where children receive half the amount of adult_)
  name_df <- deparse(substitute(df)) 
  years <- 2020:2100
  if (is.null(ssp)) { 
    ssp_name <- if (name_df %in% c("s1", "sh")) "ssp1_19" else { if (name_df %in% c("s3", "sl")) "ssp2_26msg" else "ssp2_26" } 
  } else ssp_name <- deparse(substitute(ssp))
  if (!name_df %in% c("cp", "s1", "s2", "s3", "sh", "sm", "sl") & is.null(ssp_name)) warning("ssp is not given, ssp2_26 assumed.")
  total_revenues[[ssp_name]] <- average_revenues[[ssp_name]] <- basic_income[[ssp_name]] <- basic_income_adj[[ssp_name]] <- c()

  if (!exists("scenarios_parties") & scenario == "all_countries") parties <- df$code
  else parties <- scenarios_parties[[scenario]]
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
  # if ("Dem USA" %in% parties) for (y in years) for (v in paste0(c("pop_", "adult_", "recipient_"), y)) df[[v]][df$code == "USA"] <- .3429 * df[[v]][df$code == "USA"]
      
  for (y in years) { # Unadjusted mean gain pa
    yr <- as.character(y)
    y_prev <- as.character(10*floor(y/10))
    y_next <- as.character(10*ceiling(y/10))
    df[[paste0("emissions_pb_", y)]] <- df[[paste0("emissions_", y)]]/df[[paste0(beneficiary, y)]]
    df[[paste0("gdp_pb_", y)]] <- df[[paste0("gdp_", y)]]/df[[paste0(beneficiary, y)]]
    # split USA for scenario == "optimistic" into Dem USA (the 12 States + DC with Democratic lead > 10 pp) and Non-Dem USA
    if ("Dem USA" %in% parties) df[[paste0("emissions_pb_", y)]][df$code == "USA"] <- (.2149/.3429) * df[[paste0("emissions_pb_", y)]][df$code == "USA"]
    lambda <- (y - 10*floor(y/10))/10
    carbon_price[[ssp_name]][yr] <- (1 - lambda) * carbon_price[[ssp_name]][y_prev] + lambda * carbon_price[[ssp_name]][y_next]
    # df[[paste0("revenues_pa_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pa_", y)]]) # /12
    df[[paste0("revenues_pb_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pb_", y)]]) # /12
    total_revenues[[ssp_name]][yr] <- carbon_price[[ssp_name]][yr] * sum(df[[paste0("emissions_", y)]], na.rm = T) # ssp[[paste0("emissions_", y)]][ssp$region == "world"]
    if (total_revenues[[ssp_name]][yr] < 0) df[[paste0("revenues_pb_", y)]] <- 0
    df[[paste0("revenues_pa_", y)]] <- df[[paste0("revenues_pb_", y)]] * df[[paste0(beneficiary, y)]]/df[[paste0("adult_", y)]]
    
    # GCS 
    # df[[paste0("gain_pa_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0("adult_", y)]], na.rm = T) - df[[paste0("revenues_pa_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
    df[[paste0("gain_pb_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0(beneficiary, y)]], na.rm = T) - df[[paste0("revenues_pb_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
    df[[paste0("gain_pa_", y)]] <- df[[paste0("gain_pb_", y)]] * df[[paste0(beneficiary, y)]]/df[[paste0("adult_", y)]] 
    df[[paste0("gain_over_gdp_", y)]] <- df[[paste0("gain_pb_", y)]]/df[[paste0("gdp_pb_", y)]]    
    # Adjusted for opt out
    df[[paste0("optout_right_", y)]] <- (full_part_threshold - pmax(opt_out_threshold, pmin(full_part_threshold, df[[paste0("gdp_pc_", y)]] / wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]]))))/(full_part_threshold - opt_out_threshold)
    # Accounts for non-universal participation
    average_revenues[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pb_", y)]], df[[paste0(beneficiary, y)]])
    df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pb_", y)]] > average_revenues[[ssp_name]][yr])

    # Complete the handling of "Dem USA" (cannot be done at once as we need USA number of beneficiary but Dem USA revenues_pb in #GCS)
    if ("Dem USA" %in% parties) for (v in paste0(c("pop_", "adult_", "recipient_"), y)) df[[v]][df$code == "USA"] <- .3429 * df[[v]][df$code == "USA"]
    if ("Dem USA" %in% parties) for (v in paste0(c("gdp_pb_", "gdp_pa_"), y)) df[[v]][df$code == "USA"] <- (.4082/.3429) * df[[v]][df$code == "USA"] # TODO source
  }
  
  df_parties <- compute_gain_given_parties(parties, df = df, return = "df", beneficiary = beneficiary, ssp_name = ssp_name, 
                                           discount = discount, max_gain = max_gain, full_part_threshold = full_part_threshold, opt_out_threshold = opt_out_threshold)
  for (y in years[years >= 2020]) {
    yr <- as.character(y)
    basic_income[[ssp_name]][yr] <- wtd.mean(df_parties[[paste0("revenues_pb_", y)]], df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0(beneficiary, y)]])
    basic_income_pa[[ssp_name]][yr] <- wtd.mean(df_parties[[paste0("revenues_pa_", y)]], df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("adult_", y)]])
    if (paste0("lower_basic_income_", y) %in% names(df_parties)) basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + sum((1 - df_parties[[paste0("share_basic_income_", y)]]) * df_parties[[paste0(beneficiary, y)]] * df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("lower_basic_income_", y)]]) / sum(df_parties[[paste0(beneficiary, y)]] * df_parties[[paste0("participation_rate_", y)]] * (1 - df_parties[[paste0("lower_basic_income_", y)]]))) 
    if (paste0("lower_basic_income_", y) %in% names(df_parties)) basic_income_adj_pa[[ssp_name]][yr] <- basic_income_adj[[ssp_name]][yr] * sum(df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0(beneficiary, y)]]) / sum(df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("adult_", y)]])
  }
  
  # Put back total USA figures
  if ("Dem USA" %in% parties) for (y in years) {
    df[[paste0("emissions_pb_", y)]][df$code == "USA"] <- df[[paste0("emissions_pb_", y)]][df$code == "USA"]/(.2149/.3429)
    for (v in paste0(c("pop_", "adult_", "recipient_"), y)) df[[v]][df$code == "USA"] <- df[[v]][df$code == "USA"]/.3429
    for (v in paste0(c("gdp_pb_", "gdp_pa_"), y)) df[[v]][df$code == "USA"] <- df[[v]][df$code == "USA"] / (.4082/.3429) }
  
  total_revenues[[ssp_name]] <<- total_revenues[[ssp_name]]
  average_revenues[[ssp_name]] <<- average_revenues[[ssp_name]]
  carbon_price[[ssp_name]] <<- carbon_price[[ssp_name]] # carbon_price is just completed between decadal years by interpolation
  
  if (length(setdiff(df$code[!df$code %in% c("ABW", "HKG", "MDV", "MUS")], parties)) == 0) { # If universal participation , "TWN"
    basic_income[[name_df]] <<- basic_income[[ssp_name]] 
    basic_income_adj[[name_df]] <<- basic_income_adj[[ssp_name]] 
    basic_income_pa[[name_df]] <<- basic_income_pa[[ssp_name]] 
    basic_income_adj_pa[[name_df]] <<- basic_income_adj_pa[[ssp_name]] 
    df <- df_parties
  } else {
    if (!is.null(BAU)) {
      for (t in c("", "pa_", "pc_")) for (y in 2020:2100) {
        v <- paste0("emissions_", t, y)
        if (v %in% intersect(names(df), names(BAU))) {
          df[[paste0("S", scenario, "_", v)]] <- df[[v]]
          df[[paste0("S", scenario, "_", v)]][!df$code %in% parties] <- BAU[[v]][!df$code %in% parties] 
          if ("Dem USA" %in% parties) {
            if (t == "") df[[paste0("S", scenario, "_", v)]][df$code == "USA"] <- .2149 * df[[v]][df$code == "USA"] + (1-.2149) * BAU[[v]][df$code == "USA"]
            else if (t == "pa_") df[[paste0("S", scenario, "_", v)]][df$code == "USA"] <- df[[paste0("S", scenario, "_emissions_", y)]][df$code == "USA"]/df[[paste0("adult_", y)]][df$code == "USA"]
            else if (t == "pc_") df[[paste0("S", scenario, "_", v)]][df$code == "USA"] <- df[[paste0("S", scenario, "_emissions_", y)]][df$code == "USA"]/df[[paste0("pop_", y)]][df$code == "USA"]
          }
        }
      }
    }
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj|^diff_gain_gdr_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]] <- df_parties[[v]]
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]][!df$code %in% parties] <- NA
  }
  basic_income[[scenario]] <<- basic_income[[ssp_name]]
  basic_income_adj[[scenario]] <<- basic_income_adj[[ssp_name]]
  basic_income_pa[[scenario]] <<- basic_income_pa[[ssp_name]]
  basic_income_adj_pa[[scenario]] <<- basic_income_adj_pa[[ssp_name]]
  return(df)
}

##### Instantiation #####
copy_from_cp <- c("country", "country_map", # These two are absolutely needed 
                       "gdr_pa_2030", "emissions_baseline_2030", "rci_2030", "territorial_2019", "footprint_2019", "missing_footprint", "gdp_pc_2019", "share_territorial_2019", "median_gain_2015", "mean_gain_2030", "gdp_ppp_now", "gdr_pa_2030_cerc")
# total_revenues (and average_revenues) do not depend on the scenario: they simply multiply total world emissions by the carbon price (and divide by number of beneficiaries if participation is universal and there is no special provisions like opt out).
# basic_income takes into account the opt out, basic_income_adj also takes into account the provision to avoid anti-redistributive effects. They are expressed per beneficiary. By default, beneficiary "adult_" so it coincides with basic_income_(adj_)pa.
total_revenues <- average_revenues <- basic_income <- basic_income_adj <- basic_income_pa <- basic_income_adj_pa <- list()
bau <- prepare_ssp_country("SSP245MESGB") # SSP2-4.5, temp 2100: 2.7°C, AR6 WGI SPM https://www.carbone4.com/publication-scenarios-ssp-adaptation
bau <- create_var_ssp(df = bau) # 
df <- prepare_ssp_country("SSP226MESGB") # sm, SSP2-2.6, temp max: 1.8°C, temp 2100: 1.8°C
df <- create_var_ssp(df = df) # medium price - medium ambition. Illustrative pathway ssp2_26, SSP226MESGB  # , beneficiary = "recipient_"

df$gain_euro_2030 <- df$gain_adj_2030*euro_per_dollar/12
revenues_over_gdp <- total_revenues$ssp2_26/sapply(2020:2100, function(y) sum(df[[paste0("gdp_", y)]]))
transfer_over_gdp <- setNames(sapply(2020:2100, function(y) sum((df[[paste0("gain_adj_", y)]] * df[[paste0("adult_", y)]])[df[[paste0("gain_adj_", y)]] > 0])/sum(df[[paste0("gdp_", y)]])), 2020:2100)


##### Scenarios #####
# 1. All: Whole World
all_countries <- setNames(df$code, df$country)
# 2. All against OPEC+: World except OPEC+ losers
all_but_OPEC <- all_countries[!df$code %in% c("RUS", "KAZ", "SAU", "QAT", "KWT", "ARE", "OMN", "BHR", "MYS")] # NB: Qatar has left OPEC
# 3. Optimistic scenario: not losers + EU28 + Norway + Switzerland + Canada + Japan + Korea + NZ + U.S. Democratic states 
optimistic <- c(all_countries[df$npv_pa_gcs_adj >= 0 | df$code %in% c("CHN", EU28_countries, "NOR", "CHE", "CAN", "JPN", "KOR", "NZL")], "Dem USA" = "Dem USA")
# 4. Central scenario: winners + China + EU28 + Norway + Switzerland + Japan + NZ 
central <- all_countries[df$npv_over_gdp_gcs_adj >= 0 | df$code %in% c("CHN", EU28_countries, "NOR", "CHE", "JPN", "NZL")]
# 5. Cautious scenario: EU27 + non-losers. 
prudent <- all_countries[(df$npv_over_gdp_gcs_adj >= 0) | df$code %in% c("CHN", EU27_countries)]
# 6. Africa-EU partnership: EU27 + African winners 
africa_EU <- all_countries[(df$npv_over_gdp_gcs_adj >= 0 & df$code %in% African_countries) | df$code %in% EU27_countries] # image_region_by_code[df$code] %in% c("WAF", "SAF", "RSAF", "NAF", "EAF")
# South <- all_countries[!df$contributing & df$npv_over_gdp_gcs_adj > 0]
scenarios_names <- c("all_countries", "all_but_OPEC", "optimistic", "central", "prudent", "africa_EU") # manage , "South"
scenarios_parties <- setNames(lapply(scenarios_names, function(name) eval(str2expression(name))), scenarios_names) 

for (s in scenarios_names) df <- create_var_ssp(df = df, scenario = s)

# df$Scentral_gain_adj_2030[df$code %in% c("CHN", "FRA", "IND", "USA")]/12

emissions_tot <- emissions_pc <- c()
for (y in as.character(2020:2100)) {
  emissions_tot[y] <- sum(df[[paste0("emissions_", y)]], na.rm = T)
  emissions_pc[y] <- wtd.mean(df[[paste0("emissions_pc_", y)]], weights = df[[paste0("pop_", y)]], na.rm = T) }
emissions_pa <- EU_gain_adj <- EU_gain_adj_over_gdp <- EU_npv_gain_adj_over_gdp <- list()
for (s in scenarios_names) {
  emissions_pa[[s]] <- EU_gain_adj[[s]] <- EU_gain_adj_over_gdp[[s]] <- c()
  for (y in as.character(2025:2100)) { # seq(2020, 2100, 10)
    EU_gain_adj[[s]][y] <- wtd.mean(df[[paste0(if (s == "all_countries") "" else paste0("S", s, "_"), "gain_adj_", y)]], weights = df[[paste0("adult_", y)]] * (df$code %in% EU27_countries))
    EU_gain_adj_over_gdp[[s]][y] <- EU_gain_adj[[s]][y]/wtd.mean(df[[paste0("gdp_pa_", y)]], weights = df[[paste0("adult_", y)]] * (df$code %in% EU27_countries)) }
  EU_npv_gain_adj_over_gdp[[s]] <- sum(sapply(2025:2100, function(y) { return(EU_gain_adj[[s]][as.character(y)]/(1+discount_rate)^(y-2025)) }))/sum(sapply(2025:2100, function(y) { return(wtd.mean(df[[paste0("gdp_pa_", y)]], weights = df[[paste0("adult_", y)]] * (df$code %in% EU27_countries))/(1+discount_rate)^(y-2025)) })) 
}


# scenarios_features <- data.frame(scenario = capitalize(gsub("_", " ", scenarios_names)), row.names = scenarios_names)
# for (s in scenarios_names) { 
#   scenarios_features[s, "emissions_covered"] <- sum(df$share_territorial_2019[df$code %in% eval(str2expression(s))], na.rm = T) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 0.0318
#   scenarios_features[s, "pop_covered"] <- (sum(df$pop_2023[df$code %in% eval(str2expression(s))], na.rm = T) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 117*1e6)/sum(df$pop_2023, na.rm = T)
#   scenarios_features[s, "basic_income_2040"] <- basic_income_adj[[s]]["2040"]/12
#   scenarios_features[s, "EU_loss_adj_over_gdp_2040"] <- -EU_gain_adj_over_gdp[[s]]["2040"]
# }
# (scenarios_table <- scenarios_features)
# 
# for (col in names(scenarios_features)[2:length(names(scenarios_features))]) scenarios_table[, col] <- paste0(sprintf(paste0("%.", if (grepl("EU_", col)) 1 else 0, "f"), if (grepl("basic_income", col)) scenarios_features[, col] else 100*scenarios_features[, col]), if (grepl("basic_income", col)) "" else "\\%")
# # write.table(scenarios_table, file = "scenarios_table.tex", sep = "\t", row.names = capitalize(gsub("_", " ", scenarios_names)), 
# #             col.names = c("Scenario", "\\makecell{Emissions\\\\covered}", "\\makecell{Population\\\\covered}", "\\makecell{Basic income\\\\in 2040 ($/month)}", "\\makecell{EU loss in 2040\\\\(in share of GDP)}"))
# cat(paste(kbl(scenarios_table, "latex", caption = "Main features of the different scenarios.", position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1), label = "scenarios_table.tex", row.names = FALSE,  
#               col.names = c("Scenario", "\\makecell{Emissions\\\\covered}", "\\makecell{Population\\\\covered}", "\\makecell{Basic income\\\\in 2040 (\\$/month)}", "\\makecell{EU loss in 2040\\\\(share of its GDP)}")), collapse="\n"), file = "../tables/scenarios_table.tex") 
# scenarios_table_fr <- scenarios_table
# scenarios_table_fr$scenario <- c("Tous les pays", "Tous sauf OPEP+", "Optimiste", "Central", "Prudent", "UE + Afrique")
# cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", paste(kbl(scenarios_table_fr, "latex", caption = "Principales caractéristiques des différents scénarios de club climatique.", position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1), label = "scenarios_table_fr", row.names = FALSE,  format.args = list(decimal = ","),
#           col.names = c("\\makecell{Scenario\\\\de club}", "\\makecell{Émissions\\\\mondiales\\\\couvertes}", "\\makecell{Population\\\\mondiale\\\\couverte}", "\\makecell{Revenu de base\\\\en 2040\\\\(\\$/mois)}", "\\makecell{Contribution de l'UE\\\\en 2040\\\\(fraction de son PIB)}")), collapse="\n"), fixed = T), fixed = T), file = "../tables/scenarios_table_fr.tex") 


##### WID data #####
wid <- read.dta13("../data/WID_world-pct-em-income.dta") # /!\ PPP €19 estimates, cf. /data/deprecated/Chancel_read_me. File given by Lucas Chancel email Nov 21, 2023
wid <- rbind(t(sapply(1:5, function(i) c(pctile = i, wid[1, 2:4]/5))), wid[2:96,]) # In 2019 1$ = 0.89€, i.e. 2.15$ = 1.91€
for (i in 1:4) wid[[i]] <- unlist(wid[[i]])

emissions_reduction_factor <- emissions_tot["2030"]/emissions_tot["2025"] # 0.91
price <- 100*euro_per_dollar
iter <- 0
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

wid$diff_income <- stats::filter(sort(wid$post_income - wid$income, decreasing = T), filter = c(.1, .2, .4, .2, .1))
wid$diff_income[is.na(wid$diff_income)] <- sort(wid$post_income - wid$income, decreasing = T)[is.na(wid$diff_income)]
wid$diff_income[98:100] <- (wid$post_income - wid$income)[98:100]
wid$variation_income <- stats::filter(sort((wid$post_income - wid$income)/wid$income, decreasing = T), filter = c(.1, .2, .4, .2, .1))
wid$variation_income[is.na(wid$variation_income)] <- sort((wid$post_income - wid$income)/wid$income, decreasing = T)[is.na(wid$variation_income)]


##### Percentiles of emissions by country #####
# 2019 World average carbon footprint (all gases): 5.97237587 tCO2eq (from wid.world/data)
# All WID data downloaded from https://wid.world/data/ on Nov 22, 2023
# The data file can't be uploaded to github as it is too heavy => I cleanse it and export only the required data: wid_emissions_percentiles.csv
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


##### Figures #####
# 7.1: gcp_rev_distr, gcp_diff_rev, gcp_var_rev, gcp_var_rev_rich_only
# 7.2: share_below_global_mean
# 7.3: gain_adj_2030_fr, npv_over_gdp_gcs_adj_fr, Soptimistic_npv_over_gdp_gcs_adj, Scentral_npv_over_gdp_gcs_adj

# ##### Plots #####
# # Ch 6, 7.3: gain_adj_2030_fr, npv_over_gdp_gcs_adj_fr, Soptimistic_npv_over_gdp_gcs_adj, Scautious_npv_over_gdp_gcs_adj
# # EN GCS_adj_trajectories
# yrs <- c(2025, seq(2030, 2080, 10))
# mar <- par()$mar
# par(mar = c(2.1, 4.1, 0.1, 4.1))
# plot(yrs, basic_income_adj$ssp2_26[as.character(yrs)]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Basic income ($ per month); CO2 emissions (Gt per year)", ylim = c(0, 75))
# lines(yrs, emissions_tot[as.character(yrs)]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
# par(new = T)
# plot(yrs, carbon_price$ssp2_26[as.character(yrs)], type = 'b', pch = 17, axes = FALSE, ylim = c(0, 750), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
# mtext("Carbon price ($/tCO2)", side=4, col="blue", line=2.5)
# axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
# grid()
# legend("topleft", legend = c("CO2 emissions", "Basic income", "Carbon price (right axis)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))
# 
# for (y in years[4]) plot_world_map(paste0("gain_adj_", y), df = df, breaks = c(-Inf, -1000, -500, -200, -100, -1e-10, 0, 100, 200, 300, 500, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
#                                    labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, 0, 0, 100, 200, 300, 500, Inf), sep = " to ", return = "levels")),
#                                    legend = paste0("Gains per adult\nfrom the GCP\nin ", y, " (in $ per year)"), #fill_na = T,
#                                    save = T) # c(min(df$mean_gain_2030), max(df$mean_gain_2030))
# plot_world_map("npv_over_gdp_gcs_adj", df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .09, trim = T, # svg, pdf
#                labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), # .003, .01, .03
#                legend = "Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan", #fill_na = T, \n(with 4% discount rate)
#                save = T) # c(min(df$mean_gain_2030), max(df$mean_gain_2030))
# for (s in scenarios_names[2:6]) {
#   plot_world_map(paste0("S", s, "_npv_over_gdp_gcs_adj"), df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .09, trim = T, # svg, pdf
#                  labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")),
#                  legend = paste0("Net present value\nof gains per adult\n(in % of GDP)\nfrom the Global Climate Plan\nScenario: ", capitalize(gsub("_", " ", s))), #fill_na = T, \n(with 4% discount rate)
#                  save = T, parties = scenarios_parties[[s]])
# }
# 
# # FR GCP_trajectoires
# plot(yrs, basic_income_adj$ssp2_26[as.character(yrs)]/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Revenu de base ($ par mois); Émissions de CO2 (Gt par an)", ylim = c(0, 75))
# lines(yrs, emissions_tot[as.character(yrs)]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
# par(new = T)
# plot(yrs, carbon_price$ssp2_26[as.character(yrs)], type = 'b', pch = 17, axes = FALSE, ylim = c(0, 750), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
# mtext("Prix du carbone ($/tCO2)", side=4, col="blue", line=2.5)
# axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
# grid()
# legend("topleft", legend = c("Émissions de CO2", "Revenu de base", "Prix du carbone (axe de droite)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))
# 
# for (y in years[4]) plot_world_map(paste0("gain_adj_", y), df = df, breaks = c(-Inf, -1000, -500, -200, -100, -1e-10, 0, 100, 200, 300, 500, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
#                                    labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -1000, -500, -200, -100, 0, 0, 100, 200, 300, 500, Inf), sep = " to ", return = "levels")), filename = paste0("gain_adj_", y, "_fr"),
#                                    legend = paste0("Gain net\npar adulte au\nPlan mondial pour le climat\nen ", y, " (en $ par an)"), #fill_na = T,
#                                    save = T) # c(min(df$mean_gain_2030), max(df$mean_gain_2030))
# plot_world_map("npv_over_gdp_gcs_adj", df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
#                labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), filename = "npv_over_gdp_gcs_adj_fr",
#                legend = "Gains nets au\nPlan mondial pour le climat\nagrégés sur le siècle\n(en % du PIB)", #fill_na = T, \n(with 4% discount rate)
#                save = T) # c(min(df$mean_gain_2030), max(df$mean_gain_2030))
# for (i in 2:6) {
#   plot_world_map(paste0("S", scenarios_names[i], "_npv_over_gdp_gcs_adj"), df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .075, trim = T, # svg, pdf
#                  labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.003, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), filename = paste0("S", scenarios_names[i], "_npv_over_gdp_gcs_adj_fr"),
#                  legend = paste0("Gains nets au\nPlan mondial pour le climat\nagrégés sur le siècle\n(en % du PIB)\nScénario: ", capitalize(gsub("_", " ", scenarios_table_fr$scenario[i]))), #fill_na = T, \n(with 4% discount rate)
#                  save = T, parties = scenarios_parties[[scenarios_names[i]]])
# }
# 
# 
# ##### Book figures #####
# # sum(df$share_territorial_2019[df$code == "CHN"]) # 30%
# # sum(df$share_territorial_2019[df$code == "USA"]) # 15%
# # sum(df$share_territorial_2019[df$code == "IND"]) # 7%
# # sum(df$share_territorial_2019[df$code %in% EU28_countries]) # 9%
# # sum(df$share_territorial_2019[df$npv_pa_gcs_adj > 0], na.rm = T) # 19% of global emissions in countries with gain > 0
# # sum(df$share_territorial_2019[df$npv_pa_gcs_adj == 0], na.rm = T) # 36% of global emissions in countries with gain == 0
# # TODO! why basic_income_adj spikes in 2074 (and not basic_income)?
# # TODO!? base the opt out on nominal rather than PPP? So that Russia gets it
# # TODO? replace opt out provision by increase in basic income proportional to excess emissions to global mean at t=0?
# 
# # 6.2: Recettes en proportion du PIB mondial
# revenues_over_gdp <- total_revenues$ssp2_26/sapply(2020:2100, function(y) sum(df[[paste0("gdp_", y)]]))
# transfer_over_gdp <- setNames(sapply(2020:2100, function(y) sum((df[[paste0("gain_adj_", y)]] * df[[paste0("adult_", y)]])[df[[paste0("gain_adj_", y)]] > 0])/sum(df[[paste0("gdp_", y)]])), 2020:2100)
# # max(revenues_over_gdp) # 2.3%
# # revenues_over_gdp["2030"]
# # mean(transfer_over_gdp[as.character(2025:2060)]) # .6%
# # mean((transfer_over_gdp/revenues_over_gdp)[as.character(2025:2060)]) # 65%
# # plot(2020:2100, revenues_over_gdp, type = 'l')
# # plot(2020:2100, transfer_over_gdp, type = 'l')
# 
# # 8.1: Effet sur le Burundi
# (df$pop_2030/df$adult_2030)[df$code == "BDI"] # 1.66
# # 262$ pc => 262*1.66 = 435 pa => 435+600 = 1035 pa suite au Plan => 1035/1.66/12 = 52$ pc (*2.38 suite au Plan) => 52*2.7 = 140 $PPP pc
# # 2.7 PPP conversion factor: 262 GDP pc MER https://data.worldbank.org/indicator/NY.GDP.PCAP.KD?locations=BI-CD&most_recent_value_desc=false
# # 708 GDP pc PPP https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?locations=BI-CD&most_recent_value_desc=false
# 
# # A Cadre
# sum(emissions_tot[emissions_tot > 0]) # 933 Mt
# sum(emissions_tot) # 865 Mt
