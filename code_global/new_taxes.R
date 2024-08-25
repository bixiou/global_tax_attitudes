##### Parameters #####
wealth_tax_rate <- .03 # Marginal wealth tax rate above wealth_tax_threshold
wealth_tax_threshold <- 1e8
wealth_tax_evasion_depreciation <- (1-.2)*(1-.1)
carbon_tax <- 10 
maritime_carbon_price <- 100
aviation_carbon_price <- 300
factor_gdp_tax <- .01 # Share of GNI reallocated in international transfers
share_fund <- .5
threshold_recipient_world_average <- 2 


##### Data preparation #####
# source(".Rprofile")
# source("GCP_gain_by_country.R") # For iso2to3, pop, adult, emissions_2023 and emissions_pc_2023

## GNI per capita (nominal, Atlas method)
# gni_pc <- WDI(indicator = "NY.GNP.PCAP.CD", latest = 1) # 26/07/2024
# write.csv(gni_pc, "../data/gni_pc.csv")
gni_pc <- read.csv("../data/gni_pc.csv")
gni_pc <- data.frame("code" = gni_pc$iso3c, "gni_pc_nom_2023" = gni_pc$NY.GNP.PCAP.CD)
df <- merge(df, gni_pc, all.x = T)
df$gni_nom_2023 <- df$gni_pc_nom_2023 * df$pop_2023

## Fund allocation
df$recipient_share <- df$adult_2022 * pmax(0, threshold_recipient_world_average * wtd.mean(df$gni_pc_nom_2023, df$pop_2023) - df$gni_pc_nom_2023) 
df$recipient_share <- df$recipient_share/sum(df$recipient_share, na.rm = T)


## Population and CO2 (from GCP_gain_by_country.R)
# Source for adult population: https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
# Source for population and CO2 (territorial) emissions: scenario SSP2-2.6 https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/iam_v2/SSP_IAM_V2_201811.csv.zip


## Billionaire tax
# Data from WID (bajard.felix@laposte.net), in current USD. TODO: update data once it's online and stabilized.
wealth <- read.csv("../data/wealth_tax_wid.csv") # /!\ wealth_above_threshold is total, not marginal wealth. E.g. someone with 150M will have wealth_above_threshold = 150M, not 50M, for threshold = 100M.
names(wealth) <- c("n", "iso2", "year", "threshold", "gdp", "national_wealth", "wealth_above_threshold", "headcount_above_threshold", "threshold_constant_2023", "headcount_at_bracket", "wealth_at_bracket")
wealth$code <- iso2to3[wealth$iso2]
for (c in setdiff(unique(wealth$code), NA)) df$wealth_100M_2022[df$code == c] <- wealth$wealth_above_threshold[wealth$threshold == wealth_tax_threshold & wealth$year == 2022 & no.na(wealth$code) == c] - 
  wealth_tax_threshold * wealth$headcount_above_threshold[wealth$threshold == wealth_tax_threshold & wealth$year == 2022 & no.na(wealth$code) == c]
# df$country[no.na(df$wealth_100M_2022) < 0] # /!\ pb with Egypt data: negative wealth (problem signaled to Félix Bajard)
df$wealth_100M_2022[df$wealth_100M_2022 < 0] <- NA # Remove Egypt data
df$wealth_tax_revenue <- wealth_tax_rate*wealth_tax_evasion_depreciation*df$wealth_100M_2022
df$wealth_tax_revenue_pc <- df$wealth_tax_revenue/df$pop_2022

# Impute missing wealth data using GNI pc
reg_wealth <- lm(log10(wealth_tax_revenue_pc) ~ log10(gni_pc_nom_2023), data = df, weights = pop_2022)
df$wealth_tax_revenue_pc[is.na(df$wealth_tax_revenue_pc)] <- 10^predict(reg_wealth, data.frame(gni_pc_nom_2023 = df$gni_pc_nom_2023[is.na(df$wealth_tax_revenue_pc)]))
billionaire_tax_revenue <- sum(df$wealth_tax_revenue_pc * df$pop_2022, na.rm = T) # 765G
sum(df$wealth_tax_revenue, na.rm = T)/billionaire_tax_revenue # 95% from non-missing data


## Financial Transactions Tax
# Source: Pekanov & Schratzenstaller (2019) p. 47. Total FTT revenue: 326887 M$
ftt_revenue <- 326887e6
df$ftt <- NA
df$ftt[df$code == "USA"] <- 72570
df$ftt[df$code == "AUT"] <- 1280
df$ftt[df$code == "BEL"] <- 1586
df$ftt[df$code == "BGR"] <- 154
df$ftt[df$code == "CZE"] <- 496
df$ftt[df$code == "DNK"] <- 3468
df$ftt[df$code == "FIN"] <- 838
df$ftt[df$code == "FRA"] <- 9994
df$ftt[df$code == "DEU"] <- 10002
df$ftt[df$code == "GRC"] <- 391
df$ftt[df$code == "HUN"] <- 344
df$ftt[df$code == "IRL"] <- 655
df$ftt[df$code == "ITA"] <- 4000
df$ftt[df$code == "LVA"] <- 71
df$ftt[df$code == "LTU"] <- 92
df$ftt[df$code == "LUX"] <- 1155
df$ftt[df$code == "NLD"] <- 3962
df$ftt[df$code == "POL"] <- 1206
df$ftt[df$code == "PRT"] <- 461
df$ftt[df$code == "ROU"] <- 458
df$ftt[df$code == "SLV"] <- 234
df$ftt[df$code == "ESP"] <- 3280
df$ftt[df$code == "SWE"] <- 2208
df$ftt[df$code == "GBR"] <- 75657
df$ftt[df$code == "JPN"] <- 19988
df$ftt[df$code == "AUS"] <- 5940
df$ftt[df$code == "HKG"] <- 13008 
df$ftt[df$code == "SGP"] <- 15272
df$ftt[df$code == "CHE"] <- 5659
df$na_ftt <- is.na(df$ftt)
# For country with missing data, we assume the same GDP PPP share of FTT revenue
df$ftt[is.na(df$ftt)] <- (ftt_revenue/1e6 - sum(df$ftt, na.rm = T)) * df$gdp_2017[is.na(df$ftt)]/sum(df$gdp_2017[is.na(df$ftt)], na.rm = T)
df$ftt <- df$ftt*1e6 
# wtd.mean(df$ftt/df$gdp_2017, df$gdp_2017) # .3%
# wtd.mean(df$ftt/df$gdp_2017, df$gdp_2017 * df$na_ftt) # .1%
# wtd.mean(df$ftt/df$gdp_2017, df$gdp_2017 * !df$na_ftt) # .56%
df$ftt_pc <- df$ftt/df$pop_2017


## Aviation tax
# Data used: Graver et al. (ICCT, 2018) https://theicct.org/publication/co2-emissions-from-commercial-aviation-2018/ recovered from https://ourworldindata.org/grapher/per-capita-co2-aviation 26/07/2024
# Based on GWP*100, global warming potential of aviation over 100 years, accounting for non-CO2 effects like contrails, is 3 times the warming caused by its CO2 emissions only (Lee et al., 21).
# Hence, aviation should be taxed at 3 times the rate of other sectors, i.e. $300/t. Given total emissions of 747 Mt (in 2018), this yields $223G.
aviation <- read.csv("../data/per-capita-co2-aviation.csv")[,c(2,4)] # 2018 data. World average: 97.86
names(aviation) <- c("code", "emissions_pc_aviation")
df <- merge(df[, names(df) != "emissions_pc_aviation"], aviation, all.x = T)
df$emissions_pc_aviation[is.na(df$emissions_pc_aviation)] <- (sum(df$pop_2018, na.rm = T)*aviation$emissions_pc_aviation[aviation$code == "OWID_WRL"] - 
                                                                sum(df$emissions_pc_aviation * df$pop_2018, na.rm = T)) / sum(df$pop_2018[is.na(df$emissions_pc_aviation)], na.rm = T)
df$emissions_pc_aviation <- df$emissions_pc_aviation/1e3


## Maritime levy
# Data from Dequiedt (mail from edouard.mien@ferdi.fr): 2018 CO2 emissions from shipping (departure country) after a $40/t tax (in their model, emissions are only reduced by 2% after the tax). 
# I take the average of min and max value they provide.
maritime <- read.xlsx("../data/emissions_shipping_Dequiedt_2024.xlsx")
df <- merge(df[, names(df) != "emissions_maritime_mean"], maritime[c(1,5)], all.x = T)
df$net_gain_maritime_pc <- maritime_carbon_price*((sum(df$emissions_maritime_mean, na.rm = T)/sum(df$adult_2020[!is.na(df$emissions_maritime_mean)], na.rm = T))*df$adult_2020/df$pop_2018 - df$emissions_maritime_mean/df$pop_2018)


## Corporate income tax
# Change minimum rate from 15% to 21% in Pillar 2 with no carve-out 
# Source: https://www.taxobservatory.eu/fr/base-de-donn%C3%A9es/the-tax-deficit-simulator/ 26/07/24 (cited in https://www.parisschoolofeconomics.eu/IMG/pdf/duflo_developmentinthexxicentury_pse.pdf)
cit <- read.csv2("../data/cit.csv")
df <- merge(df, cit, all.x = T)
df$cit_revenue_pc <- 1e6*df$revenue_cit_21p_extra_15p/df$pop_2023
# Impute data for the only HIC with missing data, with data from a country with same GNI p.c.
df$cit_revenue_pc[df$country == "Israel"] <- df$cit_revenue_pc[df$country == "Belgium"] 
df$cit_revenue_pc[df$country == "New Zealand"] <- df$cit_revenue_pc[df$country == "Finland"] 
df$cit_revenue_pc[df$country == "Iceland"] <- df$cit_revenue_pc[df$country == "Denmark"] 
df$cit_revenue_pc[is.na(df$cit_revenue_pc)] <- 0


##### Combined revenues and international transfers #####
# Combined revenues
df$revenues_all_taxes_pc <- df$wealth_tax_revenue_pc + df$ftt_pc + df$cit_revenue_pc + carbon_tax*df$emissions_pc_2023 + 
  maritime_carbon_price*df$emissions_maritime_mean/df$pop_2018 + aviation_carbon_price*df$emissions_pc_aviation
df$revenues_all_taxes_over_gdp <- df$revenues_all_taxes_pc/df$gni_pc_nom_2023
  
# Reallocation of a fraction of GNI
world_transfers_gdp_tax_pa <- factor_gdp_tax*sum(df$gni_nom_2023, na.rm = T)/sum(df$adult_2023[!is.na(df$gni_pc_nom_2023)])
df$net_gain_gdp_tax_pc <- world_transfers_gdp_tax_pa*df$adult_2023/df$pop_2023 - factor_gdp_tax*df$gni_pc_nom_2023
df$net_gain_over_gdp_gdp_tax <- df$net_gain_gdp_tax_pc/df$gni_pc_nom_2023

# International transfers
df$net_gain_both_taxes_pc <- share_fund*(billionaire_tax_revenue*df$recipient_share/df$pop_2022 - df$wealth_tax_revenue_pc) + df$net_gain_gdp_tax_pc
df$net_gain_over_gdp_both_taxes <- df$net_gain_both_taxes_pc/df$gni_pc_nom_2023

# Gain for government budgets (revenues + net transfers)
df$budget_gain_both_taxes_pc <- df$revenues_all_taxes_pc + df$net_gain_both_taxes_pc
df$budget_gain_over_gdp_both_taxes <- df$budget_gain_both_taxes_pc/df$gni_pc_nom_2023

# Entitlements in function of population rather than adult population
df$recipient_share_pop <- df$pop_2022 * pmax(0, threshold_recipient_world_average * wtd.mean(df$gni_pc_nom_2023, df$pop_2023) - df$gni_pc_nom_2023) 
df$recipient_share_pop <- df$recipient_share_pop/sum(df$recipient_share_pop, na.rm = T)
world_transfers_gdp_tax_pc<- factor_gdp_tax*sum(df$gni_nom_2023, na.rm = T)/sum(df$pop_2023[!is.na(df$gni_pc_nom_2023)])
df$net_gain_gdp_tax_pc_pop <- world_transfers_gdp_tax_pc - factor_gdp_tax*df$gni_pc_nom_2023
df$net_gain_over_gdp_gdp_tax_pop <- df$net_gain_gdp_tax_pc_pop/df$gni_pc_nom_2023
df$net_gain_both_taxes_pc_pop <- share_fund*(billionaire_tax_revenue*df$recipient_share_pop/df$pop_2022 - df$wealth_tax_revenue_pc) + df$net_gain_gdp_tax_pc
df$net_gain_over_gdp_both_taxes_pop <- df$net_gain_both_taxes_pc_pop/df$gni_pc_nom_2023
df$budget_gain_both_taxes_pc_pop <- df$revenues_all_taxes_pc + df$net_gain_both_taxes_pc_pop
df$budget_gain_over_gdp_both_taxes_pop <- df$budget_gain_both_taxes_pc_pop/df$gni_pc_nom_2023


##### Figures #####
# Revenue by tax
(revenue <- c("wealth" = billionaire_tax_revenue, # 765G (Higher than Zucman's 511G because he accounts for taxes already paid - here, we simulate an additional rather than a top-up tax. 
                                                                      # Also, Zucman considers a 3% over all wealth, while I consider 3% marginal over threshold. And we account for missing data (it's 726G without).)
             "ftt" = ftt_revenue, # 326G
             "carbon" = carbon_tax*sum(df$emissions_2023), # 356G
             "maritime" = maritime_carbon_price*sum(df$emissions_maritime_mean, na.rm = T), # 104G
             "aviation" = aviation_carbon_price * sum(df$emissions_pc_aviation * df$pop_2018, na.rm = T), # 223G
             "cit" = sum(df$cit_revenue_pc * df$pop_2023, na.rm = T))/1e9) # 299G
sum(revenue) # Combined revenue 2074G
sum(revenue)*1e9/sum(df$gdp_nom_2023, na.rm = T) # i.e. 2.0% of world GDP
carbon_tax*sum(df$emissions_2023, na.rm = T)/sum(df$gni_nom_2023, na.rm = T) # 0.33% Carbon tax as world GNI share 
(sum((df$net_gain_both_taxes_pc * df$pop_2030)[df$net_gain_both_taxes_pc > 0], na.rm = T)/1e9) # North-South transfer: 766G
df$country[df$budget_gain_both_taxes_pc < 0 & !is.na(df$budget_gain_both_taxes_pc)] # no country with budget loss


##### Maps and Table #####
plot_world_map("net_gain_over_gdp_both_taxes", df = df, breaks = c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")),
               legend = "International\ntransfers\nfrom new taxes\n(in % of GNI)", colors = color(13)[1:11],
               save = T)

plot_world_map("budget_gain_over_gdp_both_taxes", df = df, breaks = c(-Inf, 0, .01, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, .01, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")), colors = color(11)[c(1:5,7,8)], 
               legend = "Budget gain\nfrom new taxes\n(in % of GNI)", 
               save = T)

table_new_taxes <- cbind("transfer" = df$net_gain_both_taxes_pc, "budget_gain" = df$budget_gain_both_taxes_pc, "wealth" = df$wealth_tax_revenue_pc, "ftt" = df$ftt_pc, 
                         "carbon" = carbon_tax*df$emissions_pc_2023, "maritime" = maritime_carbon_price*df$emissions_maritime_mean/df$pop_2018, "aviation" = aviation_carbon_price*df$emissions_pc_aviation, 
                        "cit" = df$cit_revenue_pc, "pop" = df$pop_2023*df$gni_pc_nom_2023) / df$gni_pc_nom_2023
row.names(table_new_taxes) <- df$country
row.names(table_new_taxes)[row.names(table_new_taxes) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
(table_new_taxes <- rbind("World" = colSums(sweep(table_new_taxes, 1, df$pop_2023*df$gni_pc_nom_2023/(sum(df$pop_2023*df$gni_pc_nom_2023, na.rm = T)), `*`), na.rm = TRUE),
                      table_new_taxes[order(-table_new_taxes[,1]),]))
cat(paste(kbl(100*table_new_taxes[no.na(table_new_taxes[,9] > 35e6, F, F), 1:8], "latex", #caption = "Net tax gain and revenues collected from global taxes (in \\% of GDP).",
              position = "h", escape = F, booktabs = T, table.envir = NULL,  digits = c(1, 1, rep(2, 6)), linesep = rep("", nrow(table_new_taxes)-1), longtable = F, label = "transfers_gain", align = 'c',
              col.names = c("\\makecell{Int'l\\\\transfers}", "\\makecell{Budget\\\\gain}", "\\makecell{Wealth\\\\Tax(3\\%\\\\$>$100M)}", "\\makecell{Financ.\\\\Transac.\\\\Tax}", "\\makecell{Carbon\\\\Tax\\\\(10\\$/t)}",
                            "\\makecell{Maritime\\\\fuel tax\\\\(100\\$/t)}", "\\makecell{Aviation\\\\fuel tax\\\\(300\\$/t)}", "\\makecell{Corporate\\\\inc. tax\\\\(min 21\\%)}")), collapse="\n"), file = "../tables/transfers_gain.tex")


# Entitlements in function of population rather than adult population
plot_world_map("net_gain_over_gdp_both_taxes_pop", df = df, breaks = c(-.015, -.005, -.0015, 0.0015, .005, .02, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-.015, -.005, -.0015, 0.0015, .005, .02, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")),
               legend = "International\ntransfers\nfrom new taxes\n(in % of GNI)", colors = color(13)[1:11],
               save = T)

plot_world_map("budget_gain_over_gdp_both_taxes_pop", df = df, breaks = c(-Inf, 0, .01, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, .01, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")), colors = color(11)[c(1:5,7,8)], 
               legend = "Budget gain\nfrom new taxes\n(in % of GNI)", 
               save = T)

table_new_taxes_pop <- cbind("transfer" = df$net_gain_both_taxes_pc_pop, "budget_gain" = df$budget_gain_both_taxes_pc_pop, "wealth" = df$wealth_tax_revenue_pc, "ftt" = df$ftt_pc, 
                         "carbon" = carbon_tax*df$emissions_pc_2023, "maritime" = maritime_carbon_price*df$emissions_maritime_mean/df$pop_2018, "aviation" = aviation_carbon_price*df$emissions_pc_aviation, 
                         "cit" = df$cit_revenue_pc, "pop" = df$pop_2023*df$gni_pc_nom_2023) / df$gni_pc_nom_2023
row.names(table_new_taxes_pop) <- df$country
row.names(table_new_taxes_pop)[row.names(table_new_taxes_pop) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
(table_new_taxes_pop <- rbind("World" = colSums(sweep(table_new_taxes_pop, 1, df$pop_2023*df$gni_pc_nom_2023/(sum(df$pop_2023*df$gni_pc_nom_2023, na.rm = T)), `*`), na.rm = TRUE),
                          table_new_taxes_pop[order(-table_new_taxes_pop[,1]),]))
cat(paste(kbl(100*table_new_taxes_pop[no.na(table_new_taxes_pop[,9] > 35e6, F, F), 1:8], "latex", #caption = "Net tax gain and revenues collected from global taxes (in \\% of GDP).",
              position = "h", escape = F, booktabs = T, table.envir = NULL,  digits = c(1, 1, rep(2, 6)), linesep = rep("", nrow(table_new_taxes_pop)-1), longtable = F, label = "transfers_gain_pop", align = 'c',
              col.names = c("\\makecell{Int'l\\\\transfers}", "\\makecell{Budget\\\\gain}", "\\makecell{Wealth\\\\Tax(3\\%\\\\$>$100M)}", "\\makecell{Financ.\\\\Transac.\\\\Tax}", "\\makecell{Carbon\\\\Tax\\\\(10\\$/t)}",
                            "\\makecell{Maritime\\\\fuel tax\\\\(100\\$/t)}", "\\makecell{Aviation\\\\fuel tax\\\\(300\\$/t)}", "\\makecell{Corporate\\\\inc. tax\\\\(min 21\\%)}")), collapse="\n"), file = "../tables/transfers_gain_pop.tex")


table_pop <- cbind("transfer_pop" = df$net_gain_both_taxes_pc_pop, "transfer" = df$net_gain_both_taxes_pc, "budget_gain_pop" = df$budget_gain_both_taxes_pc_pop, "budget_gain" = df$budget_gain_both_taxes_pc, "pop" = df$pop_2023*df$gni_pc_nom_2023) / df$gni_pc_nom_2023
row.names(table_pop) <- df$country
row.names(table_pop)[row.names(table_pop) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
(table_pop <- rbind("World" = colSums(sweep(table_pop, 1, df$pop_2023*df$gni_pc_nom_2023/(sum(df$pop_2023*df$gni_pc_nom_2023, na.rm = T)), `*`), na.rm = TRUE),
                    table_pop[order(-table_pop[,1]),]))
cat(paste(kbl(100*table_pop[no.na(table_pop[,5] > 35e6, F, F), 1:4], "latex", #caption = "Net tax gain and revenues collected from global taxes (in \\% of GDP).",
              position = "h", escape = F, booktabs = T, table.envir = NULL,  digits = 1, linesep = rep("", nrow(table_pop)-1), longtable = F, label = "transfers_gain_pop_adult", align = 'c',
              col.names = c("\\makecell{Int'l\\\\transfers\\\\(population)}", "\\makecell{Int'l\\\\transfers\\\\(adult)}", "\\makecell{Budget\\\\gain\\\\(population)}", "\\makecell{Budget\\\\gain\\\\(adult)}")), collapse="\n"), file = "../tables/transfers_gain_pop_adult.tex")


##### With carbon debt #####
sort(setNames(df$carbon_debt_1990_2024 * 10 / df$gdp_2025, df$country))
sort(setNames(df$carbon_debt_1990_2024 * 10 / df$gni_nom_2023, df$country))
price_past_emissions <- 100

table_pop_balance <- cbind("transfer_pop" = df$net_gain_both_taxes_pc_pop, "transfer" = df$net_gain_both_taxes_pc, "budget_gain_pop" = df$budget_gain_both_taxes_pc_pop, 
                   "budget_gain" = df$budget_gain_both_taxes_pc, "carbon_balance" = -df$carbon_debt_1990_2024 * price_past_emissions / df$pop_2025, "pop" = df$pop_2023*df$gni_pc_nom_2023) / df$gni_pc_nom_2023
row.names(table_pop_balance) <- df$country
row.names(table_pop_balance)[row.names(table_pop_balance) %in% c("Democratic Republic of Congo", "Democratic Republic of the Congo")] <- "DRC"
table_pop_balance <- table_pop_balance[order(-table_pop_balance[,1]),]
cat(paste(kbl(100*table_pop_balance[no.na(table_pop_balance[,6] > 35e6, F, F), 1:5], "latex", #caption = "Net tax gain and revenues collected from global taxes (in \\% of GDP).",
              position = "h", escape = F, booktabs = T, table.envir = NULL,  digits = c(rep(1, 4), 0), linesep = rep("", nrow(table_pop_balance)-1), longtable = F, label = "transfers_gain_pop_adult", align = 'c',
              col.names = c("\\makecell{Int'l\\\\transfers\\\\(population)}", "\\makecell{Int'l\\\\transfers\\\\(adult)}", "\\makecell{Budget\\\\gain\\\\(population)}", 
                            "\\makecell{Budget\\\\gain\\\\(adult)}", "\\makecell{Carbon balance\\\\at \\$100/tCO$_\\text{2}$\\\\1990-2024}")), collapse="\n"), 
    file = "../tables/transfers_gain_pop_adult_balance.tex")


