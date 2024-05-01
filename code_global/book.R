####                  REPLICATION FILE FOR                 ####
# Un Plan mondial pour le climat et contre l'extrême pauvreté #
#                       Adrien Fabre                          #
#                 2024, adrien.fabre@cnrs.fr                  #

##### Preparing the data #####
source(".Rprofile")
source("GCP_gain_by_country.R") # TODO expliquer df
percentiles <- read.csv("../data/wid_emissions_percentiles.csv")
ppp_conversion <- read.xlsx("../data/poverty/PPP_conversion.xlsx")
source("domestic_poverty.R") # TODO expliquer p17, w
euro_per_dollar <- 0.94 # Consulted on 01/05/2024 https://www.xe.com/currencyconverter/convert/?Amount=1&From=USD&To=EUR

# NB: GCP = Global Climate Plan = Plan mondial pour le climat et contre l'extrême pauvreté.

##### Ch 1. Un statu quo insupportable #####
# Note 12
US_GDP_nominal <- 76330 # https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=IN-US-CN&start=2022&view=bar
US_GDP_PPP <- 64623 # https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?end=2022&locations=IN-US&start=2022&view=bar
India_GDP_nominal <- 2410 # https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=IN-US&start=2022&view=bar
India_GDP_PPP <- 7112 # https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?end=2022&locations=IN-US&start=2022&view=bar
2.15*India_GDP_nominal/India_GDP_PPP # => 2.15$ in the U.S. in 2017 can buy the same as 0.73$ in India in 2022.
2.15*(US_GDP_nominal/US_GDP_PPP)*India_GDP_nominal/India_GDP_PPP # In 2022, 2.15$ in the U.S. can buy the same as 0.86$ in India.
2.15*euro_per_dollar # 2.15$ = 2.02€

# Note 14
# FAO (2023) estimates at 735M the number of undernourished people in 2022.
# World Bank (consulted 28/04/2024) estimates at 716M the number of people with less than $2.15/day in 2022: 
# 9% is the extreme poverty rate: https://data.worldbank.org/indicator/SI.POV.DDAY?end=2022&locations=1W&start=2022&view=bar
world_pop <- 7.95*1e9 # 2022 World population https://data.worldbank.org/indicator/SP.POP.TOTL?end=2022&locations=1W&start=2022&view=bar
0.09*world_pop # 715.5M

# GDP p.c. of China (12720) and World (12688) in 2022 https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=1W-CN&start=2022&view=bar
euro_per_dollar*12720/12 # 996€/month 

# Figure 1.1 ../figures/policies/GDP_pc_PPP_fr.pdf
# GDP pc PPP 2022: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?contextual=default&end=2022&locations=EU-ZG-XD-XM-1W-IN-US-CD-BI-LU-CN-FR&start=2022&view=bar (consulted 29/04/2024)
fig_gdp_fr <- c("États-Unis" = 76330, "Pays à hauts revenus" = 62326, "France" = 57594, "Union Européenne" = 57286, "Chine" = 21483, 
                "Monde" = 20964, "Inde" = 8400, "Afrique subsaharienne" = 4435, "Pays à bas revenus" = 2256, "Rép. Dém. Congo" = 1338, "Burundi" = 837) # "Luxembourg" = 146457, 
names_fig_gdp_fr <- names(fig_gdp_fr)
fig_gdp_fr <- array(rev(fig_gdp_fr)/fig_gdp_fr["Monde"], dim = c(1, length(fig_gdp_fr)))
barres(data = fig_gdp_fr, labels = names_fig_gdp_fr, legend = c("PIB"), sort = FALSE, show_ticks = FALSE, showLegend = FALSE, save = F, file = "../figures/policies/GDP_pc_PPP_fr") # 330*344 px


# Note 16
# 2022 GDP p.c. (consulted on 28/04/2024) https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=EU-ZG-XD-XM-1W-IN-US-CD-BI-LU-CN&start=2022&view=bar
HIC_GDP_pc_nominal <- 49607
LIC_GDP_pc_nominal <- 751
# 1.24G in High-Income Countries and 704M in Low-Income Countries in 2022, https://data.worldbank.org/indicator/SP.POP.TOTL?end=2022&locations=XM-XD&start=2022&view=bar
(HIC_pop <- ppp_conversion$pop_2022[ppp_conversion$code == "HIC"]) # 1.24G
(LIC_pop <- ppp_conversion$pop_2022[ppp_conversion$code == "LIC"]) # 704M
0.01*HIC_GDP_pc_nominal*HIC_pop/(LIC_GDP_pc_nominal*LIC_pop) # 1% of HIC income corresponds to 116% of the combined GDP of LIC.
# Classification of HIC/LIC consulted on 28/04/2024: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
LIC <- c("AFG", "BFA", "BDI", "TCD", "COD", "ERI", "ETH", "GMB", "GIN", "GNB", "PRK", "LBR", "MDG", "MWI", "MLI", "MOZ", "NER", "RWA", "SOM", "SRE", "SDN", "SSD", "SYR", "TGO", "UGA", "YEM") # 2023 official classification. LIC: 650M people "ZMB"
HIC <- df$code[df$gdp_pc_2019 > 13205] # Classification approximately right (63 countries instead of 83 but differences are due to small islands)
sum(df$pop_2023[df$gdp_pc_2019 > 13205], na.rm = T) # 1.2G 


##### Ch. 2 La nécessité de redistribution mondiale #####
# Note 2
euro_per_dollar*6.7 # 6.30€/day: income above which all should be expropriated to close the extreme poverty gap in D.R. Congo. It is even lower in Burundi and Malawi. Cf. Table A3, Fabre (2024).

# Note 3
# The global pet food market size was estimated at USD 103.3 billion in 2023. https://www.grandviewresearch.com/industry-analysis/pet-food-industry
# Global GDP is $100.88 trillion. https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2022&locations=1W&start=2022&view=bar (consulted on 28/04/2024)
world_GDP_nominal <- 100.88e12

# Note 4
euro_per_dollar*7.5 # 7.5$ = 7.05€
sum(df$pop_2023[df$gdp_pc_2023 < 365*7.5], na.rm = T) # 624.7M live in a country with GDP p.c. below $7.5/day


# Note 5 
# 46% of world population live with less than $6.85/day. (consulted on 28/04/2024) https://data.worldbank.org/indicator/SI.POV.UMIC?end=2022&locations=1W&start=2022&view=bar
compute_poverty_rate(w, threshold = 7.5, growth = "now") # 48% live with less than 7€/day according to PIP

# Note 6
world_GDP_PPP <- 139.36e12 # $140T: 2022 World GDP in PPP https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD?end=2022&locations=1W&start=2022&view=bar (consulted on 30/04/2024)
compute_poverty_gap(df = w, threshold = 7.5, growth = "now") # $4.73T: World 7.5$ = 7€ poverty gap.
compute_poverty_gap(df = w, threshold = 7.5, growth = "now")/world_GDP_PPP # 3.4% of world GDP: World 7€ poverty gap. 
# World growth over 2002-2022: 3.4% per year
(139.36/71.9)^(1/20)-1 # https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD?end=2022&locations=1W&start=1990&view=chart (consulted on 01/05/2024)
# 2.1% of world GDP: world poverty gap in 2030 after 3.5% annual growth. Note that even if growth is lower, the poverty gap can be reduced to this level through domestic redistribution.
compute_poverty_gap(df = w, threshold = 7.5, growth = "average")/(world_GDP_PPP*1.035^8) # 2.1% of world GDP

# Note 7
# OECD (2023) indicates "aid flows from DAC countries to the group of least developed countries were USD 32 billion" in 2022.
# Table 1 (in OECD, 2023) shows DAC total aid flows are $204G = 0.36 of their GNI => 0.06% of their GNI is for aid to LDCs. *DAC: Development Assistance Committee
0.36*32/204 # 0.056% of DAC countries' GNI to LDCs

# Note 8 TODO 2 à 5% de leur PIB

# Note 9 Source: https://ourworldindata.org/contributed-most-global-co2


##### Ch. 4 Un Plan largement soutenu #####
# Global GDP is $100.88 trillion. https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2022&locations=1W&start=2022&view=bar (consulted on 28/04/2024)

# Notes 19 & 20
# Les montants ont été recalculés plus précisément depuis la préparation du sondage, 
# ce qui explique que le montant du revenu de base a été ajusté à la hausse par exemple, de même que la perte pour les États-uniens (cf. book, Ch. 5, Note 10).

# The Figures of this Chapter are translated from Fabre, Douenne & Mattauch (2023).
# Figure 4.1 ../figures/OECD/Heatplot_global_tax_attitudes_share_fr.pdf
# Figure 4.2 ../figures/FR/gcs_support_positive.pdf
# Figure 4.3 ../figures/FR/conjoint_left_ag_b_binary_positive.pdf


##### Interlude: Déroulé du futur politique rêvé #####
# GDP p.c. of Burundi in 2022: $259 => 38€/month per adult https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=BI&start=2022&view=bar (consulted on 29/04/2024)
Burundi_GDP_pc_nominal <- 259
Burundi_GDP_pc_nominal*(df$pop_2022/df$adult_2022)[df$country == "Burundi"]*euro_per_dollar/12 # 38€/month per adult


##### Ch. 5 Les grands éléments du Plan #####
## 5.1
# Figure 5.1 ../figures/policies/emissions_par_region_sm.pdf
par(mar = c(2.1, 3.1, 0.1, 0.1)) 
plot(2025:2080, df[df$country == "China", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'red', lwd = 2, lty = 2, xlab = "", ylab = "Émissions de CO2 par adulte (tCO2/an)", ylim = c(0, 17.5))
lines(2025:2080, df[df$code == "USA", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'blue', lwd = 2, lty = 3)
lines(2025:2080, colSums(df[df$code %in% EU27_countries, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% EU27_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'darkgreen', lwd = 2, lty = 4, xlab = "", ylab = "")
lines(2025:2080, df[df$code == "IND", paste0("emissions_pa_", 2025:2080)], type = 'l', col = 'orange', lwd = 2, lty = 5, xlab = "", ylab = "")
lines(2025:2080, colSums(df[df$code %in% SSA, paste0("emissions_", 2025:2080)])/colSums(df[df$code %in% African_countries, paste0("adult_", 2025:2080)]), type = 'l', col = 'purple', lwd = 2, lty = 6, xlab = "", ylab = "")
lines(2025:2080, colSums(df[, paste0("emissions_", 2025:2080)], na.rm = T)/colSums(df[, paste0("adult_", 2025:2080)], na.rm = T), type = 'l', col = 'black', lwd = 2, lty = 1, xlab = "", ylab = "")
grid() + abline(h = 1:18, col = "gray", lty = 3)
legend("topright", legend = c("Monde", "Chine", "États-Unis", "Union Européenne", "Inde", "Afrique subsaharienne"), col = c("black", "red", "blue", "darkgreen", "orange", "purple"), lwd = 2, lty = 1:6)


## 5.2 
basic_income_adj$df["2030"]*euro_per_dollar/12 # 44€/mois = 47$/month in 2030
basic_income_adj$df["2040"]*euro_per_dollar/12 # 44€/mois = 47$/month in 2040
mean(basic_income_adj$df[as.character(seq(2030, 2060, 10))])*euro_per_dollar/12 # 53€/mois en moyenne sur 2030-2060 (56$/m)

# Figure 5.2 ../figures/policies/GCP_trajectoires.pdf TODO
par(mar = c(2.1, 3.1, 0.3, 4.1), mgp = c(2.2, 1, 0)) 
plot(yrs, basic_income_adj$df[as.character(yrs)]*euro_per_dollar/12, type = 'b', col = 'darkgreen', lwd = 2, xlab = "", ylab = "Revenu de base (€ par mois); Émissions de CO2 (Gt par an)", ylim = c(0, 70))
lines(yrs, emissions_tot[as.character(yrs)]/1e9, type = 'b', pch = 15, col = 'red', lwd = 2)
par(new = T)
plot(yrs, carbon_price$ssp2_26[as.character(yrs)]*euro_per_dollar, type = 'b', pch = 17, axes = FALSE, ylim = c(0, 700), col = 'blue', lwd = 2, lty = 2, xlab = "", ylab = "")
mtext("Prix du carbone (€/tCO2)", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 750), col="blue", col.axis="blue")
grid()
legend("topleft", legend = c("Émissions de CO2", "Revenu de base", "Prix du carbone (axe de droite)"), col = c("red", "darkgreen", "blue"), lwd = 2, lty = c(1,1,2), pch = c(16, 15, 17))


# 715M of people live with less than 2€/day (cf. above, Ch. 1, Note 14)

# Revenues from the GCP: 2% of world GDP in 2030
max(revenues_over_gdp) # 2.52%
revenues_over_gdp["2030"] # 2.52% 
plot(2020:2100, revenues_over_gdp, type = 'l')
plot(2020:2100, transfer_over_gdp, type = 'l')

# International transfers from the GCP: .6% of world GDP in 2030
mean(transfer_over_gdp[as.character(2025:2060)]) # .58% 

# Average French loses less than 10€/month, average Indian wins 25€/month in 2030.
df$gain_euro_2030[df$country == "France"] # -9.40€/month
df$gain_euro_2030[df$country == "India"] # +24.9/month

## 5.3 
# Emission share by region:
sum(df$share_territorial_2019[df$code == "CHN"]) # 30%
sum(df$share_territorial_2019[df$code == "USA"]) # 15%
sum(df$share_territorial_2019[df$code == "IND"]) # 7.4%
sum(df$share_territorial_2019[df$code %in% EU28_countries]) # 9%
sum(df$share_territorial_2019[df$npv_pa_gcs_adj > 0], na.rm = T) # 19% of global emissions in countries with gain > 0
sum(df$share_territorial_2019[df$npv_pa_gcs_adj == 0], na.rm = T) # 36% of global emissions in countries with gain == 0

# Table 5.1 Principales caractéristiques des différents scénarios de club climatique
scenarios_features <- data.frame(scenario = capitalize(gsub("_", " ", scenarios_names)), row.names = scenarios_names)
for (s in scenarios_names) { 
  scenarios_features[s, "emissions_covered"] <- sum(df$share_territorial_2019[df$code %in% eval(str2expression(s))], na.rm = T) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 0.0318
  scenarios_features[s, "pop_covered"] <- (sum(df$pop_2023[df$code %in% eval(str2expression(s))], na.rm = T) + ("Dem USA" %in% eval(str2expression(s)) & !"USA" %in% eval(str2expression(s))) * 117*1e6)/sum(df$pop_2023, na.rm = T)
  scenarios_features[s, "basic_income_2040"] <- basic_income_adj[[s]]["2040"]*euro_per_dollar/12
  scenarios_features[s, "EU_loss_adj_over_gdp_2040"] <- -EU_gain_adj_over_gdp[[s]]["2040"]
}
(scenarios_table <- scenarios_features)

for (col in names(scenarios_features)[2:length(names(scenarios_features))]) scenarios_table[, col] <- paste0(sprintf(paste0("%.", if (grepl("EU_", col)) 1 else 0, "f"), 
                            if (grepl("basic_income", col)) scenarios_features[, col] else 100*scenarios_features[, col]), if (grepl("basic_income", col)) "" else "\\%")
# cat(paste(kbl(scenarios_table, "latex", caption = "Main features of the different scenarios.", position = "h", escape = F, booktabs = T, align = "c", 
#               linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1), label = "scenarios_table.tex", row.names = FALSE,  
#               col.names = c("Scenario", "\\makecell{Emissions\\\\covered}", "\\makecell{Population\\\\covered}", "\\makecell{Basic income\\\\in 2040 (\\$/month)}", 
#                             "\\makecell{EU loss in 2040\\\\(share of its GDP)}")), collapse="\n"), file = "../tables/scenarios_table.tex") 
scenarios_table_fr <- scenarios_table
scenarios_table_fr$scenario <- c("Tous les pays", "Tous sauf OPEP+", "Optimiste", "Central", "Prudent", "UE + Afrique")
cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", 
    paste(kbl(scenarios_table_fr, "latex", caption = "Principales caractéristiques des différents scénarios de club climatique.", 
              position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(scenarios_table)-1), digits = c(0, 0, 0, 1),
              label = "scenarios_table_fr", row.names = FALSE,  format.args = list(decimal = ","),
        col.names = c("\\makecell{Scenario\\\\de club}", "\\makecell{Émissions\\\\mondiales\\\\couvertes}", "\\makecell{Population\\\\mondiale\\\\couverte}", 
                      "\\makecell{Revenu de base\\\\en 2040\\\\(\\euro{}/mois)}", "\\makecell{Contribution de l'UE\\\\en 2040\\\\(fraction de son PIB)}")), 
        collapse="\n"), fixed = T), fixed = T), file = "../tables/scenarios_table_fr.tex") 


# Note 14 
NGN_per_euro <- 0.0007026 # Consulted on 28/04/2024 https://www.xe.com/currencyconverter/convert/?Amount=1&From=NGN&To=EUR
269130*NGN_per_euro # 189€ Hardware Cost + Shipping: NGN 269,130 Cost for January 2023. Consulted on 28/04/2024 https://starlinkinsider.com/starlink-price/
24175*NGN_per_euro # 17€ Monthly Price (Roam Regional): NGN 24,175 Cost for January 2023. Consulted on 28/04/2024 https://starlinkinsider.com/starlink-price/

# The government of Haiti estimates at €17 billion the illegitimate debt that France imposed upon it at the independence. 
# Source: http://preferences-pol.fr/Documents/Haïti.pdf, cf. aussi https://www.cadtm.org/spip.php?page=imprimer&id_article=339
17e9/(12*4*df$adult_2025[df$country == "Haiti"]) # 44€/month: cost of a basic income to all Haitian adults for 4 years to pay back the €17G debt. 


##### Ch 6. Un transfert important vers les pays du Sud ##### 
## 6.1
sum(wid$post_income > wid$income) # 71% of winners
sum((wid$post_income - wid$income)[wid$post_income > wid$income])/sum(wid$income) # 1.2% redistributed from rich to poor people
(sum(wid$income[91:100]) - sum(wid$post_income[91:100]))/sum((wid$post_income - wid$income)[wid$post_income > wid$income]) # 82% of transfer comes from top 10%
(sum(wid$post_income[1:50]) - sum(wid$income[1:50]))/sum((wid$post_income - wid$income)[wid$post_income > wid$income]) # 86% of transfer goes to top 50%

# Table 6.1
(table_ineq <- cbind("poverty_gap" = 100*c((7*365 - mean(wid$income[wid$income < 7*365]))*sum(wid$income < 7*365)/sum(wid$income), 
                                           (7*365 - mean(wid$post_income[wid$post_income < 7*365]))*sum(wid$post_income < 7*365)/sum(wid$post_income)), 
                     "top10" = 100*c(sum(wid$income[91:100])/sum(wid$income), sum(wid$post_income[91:100])/sum(wid$post_income)), 
                     "bottom50" = 100*c(sum(wid$income[1:50])/sum(wid$income), sum(wid$post_income[1:50])/sum(wid$post_income)),
                     "Gini" = 100*c(Gini(wid$income), Gini(wid$post_income)), 
                     "D9/D1" = c(wid$income[90]/wid$income[10], wid$post_income[90]/wid$post_income[10])))
row.names(table_ineq) <- c("Avant", "Après")
cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", 
  paste(kbl(table_ineq, "latex", caption = "Évolution de l'inégalité mondiale suite au Plan.", 
        position = "b", escape = F, booktabs = T, digits = 1, label = "gcp_ineq", align = 'c', format.args = list(decimal = ","),
        col.names = c("\\makecell{Étendue de\\\\la pauvreté\\\\à 7~\\textit{\\texteuro{}}/jour\\\\(en \\% du PIB)}", "\\makecell{Top 10~\\%\\\\(part en \\%)}", "\\makecell{Bottom 50~\\%\\\\(part en \\%)}", 
                      "\\makecell{Gini\\\\(en \\%)}", "\\makecell{D9/D1\\\\Ratio\\\\inter-décile}")), 
        collapse="\n"), fixed = T), fixed = T), file = "../tables/gcp_ineq.tex") # TODO

# # price*mean(wid$post_emissions)/mean(wid$income) # 2.8% of world GDP raised
# (share_redistributed <- sum((wid$post_income - wid$income)[wid$post_income > wid$income]/100)/(revenues_pa*12)) # 40% of GCP revenues redistributed from rich to poor
# (rich_to_poor <- share_redistributed*revenues_over_gdp["2030"]) # 1% of world GDP redistributed from rich to poor people (rescaled to perfectly match the other model)
# 
basic_income_adj$df["2030"]*euro_per_dollar/12 # 44€/month: amount of the basic income under universal participation
revenues_pa # 44€/month We match the basic income of our other model. TODO doesn't match
min(wid$diff_income[1:99])/12 # -168€/month: maximum average loss for the 99th percentile (only the 100th loses more)
max(wid$diff_income)/12 # 36€/month: maximum gain.* 
min(wid$variation_income) # -2.3%: maximum average loss per percentile.
Gini(wid$post_income)/Gini(wid$income)-1 # -2.49%: reduction in Gini
sum(wid$post_income[1:12])/sum(wid$income[1:12]) # 2.4: factor by which the income of the poorest billion is multiplied.*
# *Gains for the poor are underestimated because the carbon price is nominal but the income data is in PPP. 
# WID overestimates extreme poverty, because it relies on tax administration data that does not account for informal work and auto-production (cf. Prydz et al. 2022).
# On the contrary, it is the purpose of PIP (World Bank) data to accurately measure poverty, and PIP finds a 9% extreme poverty rate. 
# According to the World Bank, 9% of the world population live in extreme poverty. 
# The gain in nominal terms of the GCP is at least 1.15€/day for the first 9 percentiles (the extreme poor):
(min_gain_for_extreme_poor <- (revenues_pa*12 - min(wid$post_emissions[1:9]) * 100)/365) # 1.15 € (nominal)
# To the extent that 1.15€ in nominal terms equals at least 2€ in PPP, even someone with 0 income would be lifted out of extreme poverty (defined at 2€/day) by the GCP. TODO PPP conversion
# As shown below, 1.15€ in nominal terms equals 3€ in PPP over Low-Income Countries and 2 €PPP in D.R.C. 
# => ERADICATION OF EXTREME POVERTY
LIC_GDP_nominal <- 751 # https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=XM&start=2022&view=bar (consulted on 30/04/2024)
LIC_GDP_PPP <- 1949 # https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?end=2022&locations=XM&start=2022&view=bar (consulted on 30/04/2024)
min_gain_for_extreme_poor*LIC_GDP_PPP/LIC_GDP_nominal # 3.00 €PPP: minimum gain from the GCP in LIC (assuming the same PPP conversion factor in all LIC, average of LIC PPP factors) 
ppp_conversion$country[ppp_conversion$code %in% LIC][which.min(no.na(ppp_conversion$PPP_conversion_2022[ppp_conversion$code %in% LIC]))] # LIC with highest prices: DRC
(min_gain_for_extreme_poor)*min(ppp_conversion$PPP_conversion_2022[ppp_conversion$code %in% LIC], na.rm = T) # 2.001 €PPP: minimum gain from the GCP in D.R.C., the LIC with the highest prices. 

# Fall in poverty gap of 25%
(sum(wid$post_income[wid$post_income < 7*365])/sum(wid$post_income))/(sum(wid$post_income[wid$income < 7*365])/sum(wid$income))-1 # -25%

sum(wid$variation_income > 0.03) # 54% experience more than 3% increase in income
sum(wid$variation_income > 0.1) # 31% experience more than 10% increase in income

# Figure 6.1
revenues_pa # 43€/month basic income
# mar <- par()$mar
# mgp <- par()$mgp
par(mar = c(3.1, 3.1, 0.3, 0.2), mgp = c(2.2, 1, 0)) # width: 338, height: 322
# 6.1a ../figures/policies/gcp_rev_distr.pdf 
plot(1:100, wid$income/12, col = "red", lwd = 2, type = 'l', ylim = c(0, 8e4/12), xlab = "Percentile de niveau de vie", ylab = "Niveau de vie (en €/mois)")
lines(1:100, wid$post_income/12, col = "darkgreen", lwd = 2, type = 'l', lty = 2) + grid()
legend("topleft", legend = list("actuel", "suite au Plan"), col = c("red", "darkgreen"), title = "Niveau de vie", lwd = 2, lty = c(1,2))
# 6.1b ../figures/policies/gcp_diff_rev.pdf
plot(1:100, wid$diff_income/12, col = "purple", lwd = 2, ylim = c(-200, 50), type = 'l', xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en €/mois)") + grid() + abline(h = 0)
# plot(1:100, wid$diff_income, col = "purple", lwd = 2, ylim = c(-2000, 500), type = 'l', xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en €/an)") + grid() + abline(h = 0)
# 6.1c ../figures/policies/gcp_var_rev.pdf
plot(1:100, 100*pmin(6, wid$variation_income), col = "blue", lwd = 2, type = 'l', ylim = 100*c(0, 2.2), xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en %)") + grid() + abline(h = 0)
# 6.1d ../figures/policies/gcp_var_rev_rich_only.pdf
plot(40:100, 100*wid$variation_income[40:100], col = "blue", lwd = 2, type = 'l', ylim = 100*c(-0.024, 0.048), xlab = "Percentile de niveau de vie", ylab = "Variation de niveau de vie (en %)") + grid() + abline(h = 0)

percentiles$share_below_global_mean[no.na(percentiles$code) == "IND"] # 94% of winners in India
percentiles$share_below_global_mean[no.na(percentiles$code) == "FRA"] # 23% of winners in France
df$gain_euro_2030[df$code == "FRA"] # -9.4€/month for the average French 

# Note 2
# States with Democratic margin (e.g. 57%-41%) >15pp at the 2024 presidential election: 13 states + DC
# California, Illinois, New York, New Jersey, Washington, Massachusetts, Oregon, Connecticut, Delaware, Hawaii, Rhose Island, DC, Vermont, Maryland
# sources: https://en.wikipedia.org/wiki/2020_United_States_presidential_election#Results_by_state

# Figure 6.2 ../figures/maps/share_below_global_mean.pdf 1297x626, then cropped using https://pdfresizer.com/crop
plot_world_map("share_below_global_mean", df = percentiles[!is.na(percentiles$country_map),],  breaks = c(-Inf, 1, 15, 30, 50, 70, 90, 99, Inf), format = c('png', 'pdf'), legend_x = .09, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(1), c(-Inf, 1, 15, 30, 50, 70, 90, 99, Inf), sep = "% à ", end = "%", return = "levels")), legend = "Part de gagnants\nsuite au\nPlan mondial pour le climat", 
               save = T) 

# Figure 6.3 ../figures/maps/gain_adj_2030_fr.pdf
plot_world_map("gain_euro_2030", df = df, breaks = c(-Inf, -150, -100, -50, -10, -1e-10, 0, 10, 20, 30, 40, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), c(-Inf, -150, -100, -50, -10, 0, 0, 10, 20, 30, 40, Inf), sep = " to ", return = "levels")), filename = paste0("gain_adj_2030_fr"),
               legend = paste0("Gain net\npar adulte au\nPlan mondial pour le climat\nen 2030 (en € par mois)"), #fill_na = T,
               save = T) 

# Figure 6.4 ../figures/maps/npv_over_gdp_gcs_adj_fr.pdf
plot_world_map("npv_over_gdp_gcs_adj", df = df, breaks = c(-Inf, -.02, -.01, -.005, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.005, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), filename = "npv_over_gdp_gcs_adj_fr",
               legend = "Gains nets au\nPlan mondial pour le climat\nagrégés sur le siècle\n(en % du PIB)", #fill_na = T, \n(with 4% discount rate)
               save = T)

# Figure 6.5 ../figures/maps/Soptimistic_npv_over_gdp_gcs_adj.pdf
# Figure 6.6 ../figures/maps/Scentral_npv_over_gdp_gcs_adj.pdf
for (i in 3:4) {
  plot_world_map(paste0("S", scenarios_names[i], "_npv_over_gdp_gcs_adj"), df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .075, trim = T, # svg, pdf
                 labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.005, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), filename = paste0("S", scenarios_names[i], "_npv_over_gdp_gcs_adj_fr"),
                 legend = paste0("Gains nets au\nPlan mondial pour le climat\nagrégés sur le siècle\n(en % du PIB)\nScénario: ", capitalize(gsub("_", " ", scenarios_table_fr$scenario[i]))), #fill_na = T, \n(with 3% discount rate)
                 save = T, parties = scenarios_parties[[scenarios_names[i]]])
}


##### Ch. 7 Un pas vers un monde soutenable #####
## 7.1 
euro_per_dollar*Burundi_GDP_pc_nominal*(df$pop_2022/df$adult_2022)[df$country == "Burundi"] # 450€/year: PIB per adult Burundi, cf. Interlude for the source
df$emissions_pa_2022[df$country == "Burundi"] # 0.1 tCO2/year
Burundi_GDP_nominal <- 259 # https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=BI-US&start=2022&view=bar
Burundi_GDP_PPP <- 708 # https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?end=2022&locations=BI-US&start=2022&view=bar
(Burundi_new_GDP_pc <- 259*1.03^6 + (df$gain_adj_2028*df$adult_2030/df$pop_2030)[df$country == "Burundi"]) # 610$
Burundi_new_GDP_pc/Burundi_GDP_nominal # x2.4: factor by which Burundi's GDP pc would be multiplied
Burundi_new_GDP_pc*euro_per_dollar/12 # 48 €/mois: new GDP pa
Burundi_new_GDP_pc*euro_per_dollar*Burundi_GDP_PPP/Burundi_GDP_nominal/12 # => 131€PPP in Burundi.

# Note 3
compute_poverty_rate(df = w, threshold = 7.5, growth = "average") # 40%: world poverty rate in 2030 after 3.5% annual growth TODO
compute_poverty_gap(df = w, threshold = 7.5, growth = "average")/(world_GDP_PPP*1.035^8) # 2.1% of world GDP: world poverty gap in 2030 after 3.5% annual growth

# Note 4
(sum(wid$post_income[1:40])/sum(wid$post_income) - sum(wid$income[1:40])/sum(wid$income)) # 0.8% transferred to (originally) poor
sum(wid$income[1:40])/sum(wid$income) # 5.1% Share of bottom 40% in world income TODO
sum(wid$post_income[1:40])/sum(wid$post_income) # 5.9% Share of bottom 40% in world income after the GCP 

# Note 5
# GDP p.c. PPP of Ukraine https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD?locations=UA (consulted on 29/04/2024)
Ukraine_GDP_2022 <- 10731
Ukraine_GDP_2021 <- 12949
Ukraine_GDP_1990 <- 16428
Ukraine_GDP_2022/Ukraine_GDP_1990-1 # -35%
Ukraine_GDP_2021/Ukraine_GDP_1990-1 # -21%

# Note 6, Own computations based on "Compensation for atmospheric appropriation", Fanning & Hickel (2023)
# Cf.  github.com/bixiou/compensation-atmospheric-appropriation. TODO

# Note 8
# With depreciation of 15% and no tax evasion, a 2% tax above $5 million would collect 1% of the world income. Source: https://wid.world/world-wealth-tax-simulator/

df$gain_adj_2030[df$country == "France"]*euro_per_dollar # 113€ per French adult to be raised
df$gain_adj_2030[df$country == "France"]*euro_per_dollar/12 # Average loss per French adult: 9.35€/month
# 3.5% tax on income > 16342€/month (top 1%): raises 120€ per French adult (without behavioral effect). Source: wid.world/data (consulted on 29/04/2024), cf. own computations on ../data/poverty/WID_income.xlsx

(df$gain_adj_2030*df$adult_2022)[df$country == "United States"] # 474G$ to be raised to offset 
df$gain_adj_2030[df$country == "United States"]/12 # -141$/month: loss to average (compensated) American from GCP
# Increasing income tax rates 32->33% >315k, 35->40% >400k, 37->50% >600k, 37->60% >5M, integrate with corporate tax and tax capital gains fully
# => collects 472.6 G$ in 2019. Source: taxjusticenow.org (consulted on 29/04/2024)
# The site also shows that taxes would barely increase for the bottom 97%, those with less than $312k/year (tax rate of percentile 96: 31.6->31.9%)
315000/12 # 26250$/month: threshold of top 3% according to taxjusticenow.org (FYI top 1% threshold: 567k/year = 47k/month)

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

# Note 13
# 16.3%: world top 1% share of post-tax income in 2022. Source: wid.world/data (consulted on 29/04/2024)

# Table 7.1 
# Revenue of each row:
sum((wid$post_income - wid$income)[wid$post_income > wid$income])/sum(wid$income) # 1.2% of world GDP redistributed from rich to poor people, cf. above Ch. 6.1
# 2% tax on wealth > 5M€: 1% of world GDP. Cf. above, Ch. 7, Note 8 https://wid.world/world-wealth-tax-simulator/
# 6% tax on wealth > 100 M€, 10% > 1 G€: 1.02% of world GDP (based on depreciation of 25% and tax evasion of 20%). Source: https://wid.world/world-wealth-tax-simulator/
# 12.3% tax on income > 21191€/month (top 0.4%): 1% of 2022 world PPP income (without behavioral effect). Source: wid.world/data (consulted on 29/04/2024), cf. own computations on ../data/poverty/WID_income.xlsx
# 8.4% tax on income > 11960€/month (top 1%): 1% of 2022 world income. See above. => An additional tax of 15% on income > 10k€ would collect 2% of world income. 
# 5% tax on income > 6080€/month (top 4%): 1% of 2022 world income. See above.
# => An additional tax schedule of 5% > 6k€, 12.5% > 10k€, and 25% > 20k€ would collect 3% of world income.

# Cost of each row:
# GCP: revenue = cost, extreme poverty ended as shown in Ch. 6.1
# Climate debt: Ch. 7, Note 6.
# Fossil fuel tax losses: at most / eventually Fuel excise + carbon pricing revenues: 0.8% GDP (world), 1% (OECD), 1.4% (France), 1.7% (Germany) 0.4% (U.S., China), 2.9% (Poland), 
#                         up to 3.5% in Greece in 2021. Source (sum of three first columns): https://stats.oecd.org/Index.aspx?DataSetCode=REVPOT (consulted on 29/04/2024)
# Compensation of High-Income Countries' middle class and after decarbonization, maintaining ~1% of world GDP North-South transfer:
sum((df$gain_adj_2025*df$adult_2025)[df$code %in% HIC])/sum(df$gdp_2025[df$code %in% HIC]) # 1.2%: average loss of GDP across high-income countries from the GCP
# Green investments and negative emissions. Green investments TODO 
# 0.3-3% of world GDP needed in second half of century for negative emissions, according to Edenhofer et al. (2023), "On the Governance of Carbon Dioxide Removal – A Public Economics Perspective"
# Improved public services: it is not necessarily 1% of world GDP, but the tax potential is equally discretionary

##### Ch 8. L’appel pour la redistribution mondiale #####
# Note 5
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2022&locations=EU-ZG-XD-XM-1W-IN-US-CD-BI-LU-CN&start=2022&view=bar
HIC_GDP_pc_nominal/LIC_GDP_pc_nominal # 66
# 1% from HIC would double LIC: cf. above, Ch 1, Note 16.

# Note 6: cf. above, Ch. 7, Note 8 Source: https://wid.world/world-wealth-tax-simulator/
4.56e6/sum(df$adult_2023) # 0.08% of people would be taxed, according to https://wid.world/world-wealth-tax-simulator/

# Global GDP is $100.88 trillion. https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2022&locations=1W&start=2022&view=bar (consulted on 28/04/2024)

# Day for eradicating poverty is on October 17 each year. Source: https://en.wikipedia.org/wiki/International_Day_for_the_Eradication_of_Poverty


##### FAQ #####
# La population ne va-t-elle pas s'opposer au Plan lorsqu'elle réalisera l'ampleur des efforts nécessaires ?
# 10% soutenaient une taxe carbone avec transferts vs. 70% s'y opposaient. Par cohérence avec le Ch. 4, le soutien est présenté en termes relatifs plutôt qu'absolu.
10/(10+70) # 13% (fév 2019)
# Le chiffre de 38% (oct 2020) est lui aussi en termes relatifs. Cf. p. 3 https://adrien-fabre.com/Documents/Les%20Fran%C3%A7ais%20et%20la%20taxe%20carbone%20-%20Pr%C3%A9sentation%20ADEME%2020%27.pdf
26/(26+42) # 38%

# Quelles seront les conséquences macroéconomiques du Plan (croissance, inflation, chômage) ?
# Note 18
# Pays avec le plus de destructions nettes d’emploi: (Source labor force 2023: https://tradingeconomics.com/[country]/labor-force-total-wb-data.html; source job losses: Jacobson et al., 2017, p. 167)
# Brunei (20k, 8.9% of labor force), Libye (168k, 7.3%), Qatar (117k, 5.8%), Norway (150k, 5%), Kuwait (114k, 4.8%), Arabie Saoudite (444k, 2.8%), Irak (189k, 1.7%), Angola (227k, 1.5%) 

# Global labor force TODO
# luxe et tourisme représentent à eux deux 6 % du PIB français TODO


##### Annexe A #####
## Mécanismes de participation
# Les dérogations (ou opt out) à la mutualisation des recettes réduiraient le revenu de base de 56 à 47 dollars par mois en 2030 (dans les pays qui n’en bénéficient pas). TODO
average_revenues$ssp2_26["2030"]*euro_per_dollar/12 # 54€
basic_income$df["2030"]*euro_per_dollar/12 # 44€

# Note 5
# Cf. https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?end=2022&locations=1W-RU-CN&start=2022&view=bar Consulted on 28/04/2024


##### Annexe B #####
## B.2
price # 94€/t
100*euro_per_dollar # 94
emissions_reduction_factor # -9%
emissions_tot["2030"]/emissions_tot["2025"] # 0.91
revenues_pa # 42€/month

## B.4
sum(emissions_tot[emissions_tot > 0 & as.numeric(names(emissions_tot)) >= 2025]) # 934 Gt starting in 2025. Adding 35 Gt from 2024, this corresponds to 969 Gt, i.e. ~69% chance of meeting the 2°C target.
sum(emissions_tot[emissions_tot > 0]/1e9) # 1113 Gt from 2020.
barycenter(sum(emissions_tot[emissions_tot > 0]/1e9), 900, 1150, 83, 67) # 69% chance. Indeed, 1113 Gt from 2020 => 67% chance; 900 Gt => 83%, according to IPCC (2021), SPM.2. 
names(emissions_tot)[emissions_tot <= 0][1] # 2079: first year with negative emissions
sum(emissions_tot[as.numeric(names(emissions_tot)) >= 2025]) # 866 Mt: Total emissions over 2025-2100.

# Note 8
((df$gdp_pc_2030/df$gdp_pc_2020)[df$country == "Democratic Republic of Congo"])^(1/10)-1 # 7.7% growth over 2020-2030

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
