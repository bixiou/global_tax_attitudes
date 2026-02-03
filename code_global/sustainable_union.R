##### Parameters #####
ultra_wealth_tax_rate <- .03 # Marginal wealth tax rate above global_wealth_tax_threshold
ultra_wealth_tax_threshold <- 1e8
wealth_tax_rate <- .02 # Marginal wealth tax rate above national_wealth_tax_threshold
wealth_tax_threshold <- 5e6
wealth_tax_evasion_depreciation <- (1-.2)*(1-.1)
carbon_price_floor <- 10 
pooled <- .01 # Share of GNI reallocated in international transfers
participation_incentive <- function(r) { return(max(1/3, sqrt(r))) }

# scenarios_names <- c("all_countries", "all_but_OPEC", "optimistic", "central", "prudent", "africa_EU")
union_parties <- scenarios_parties$central # cf. GCP_gain_by_country
union_taxes <- c("global_wealth_tax_revenue", "national_wealth_tax_revenue", "ftt", "carbon_price_revenues")
union_waivers <- c() # c("CHN")

##### Data preparation #####
# source(".Rprofile")
# source("GCP_gain_by_country.R") # For iso2to3, pop, adult, emissions_2023 and emissions_pc_2023
# source("new_taxes.R")

## Other taxes (TODO)
df$global_wealth_tax_revenue <- df$national_wealth_tax_revenue <- df$ftt <- 0

## Carbon tax
df$carbon_price_revenues <- carbon_price_floor*df$emissions_2023
df$carbon_price_revenues_pc <- carbon_price_floor*df$emissions_pc_2023

## Transfer allocation
df$key <- df$pop_2023 * (df$gni_pc_nom_2023 - wtd.mean(df$gni_pc_nom_2023, df$pop_2023)) 
df$contributing <- no.na(df$key > 0, F, num_as_char = F)

participation_factor <- function(parties = union_parties, waivers = union_waivers, data = df) {
  contributing <- data$contributing & !data$code %in% waivers
  return(participation_incentive(sum(data$key[data$code %in% parties & contributing], na.rm = T)/sum(data$key[contributing], na.rm = T)))
}
compute_union_revenues <- function(taxes = union_taxes, parties = union_parties, data = df, return_total = F, unit = 1) {
  if (unit %in% c("GDP", "GNI", "gni", "gdp")) unit <- sum(data$gni_nom_2023[data$code %in% parties], na.rm = T)
  revenues <- c()
  for (t in taxes) revenues[t] <- sum(data[[t]][data$code %in% parties], na.rm = T)*participation_factor(parties, data)/unit
  if (return_total) {
    return(sum(revenues))
    print(paste("particiation factor:", particiation_factor))
  } else return(revenues)
}
compute_union_revenues(parties = scenarios_parties$central, return_total = F, unit = 1e9)
compute_union_revenues(parties = scenarios_parties$central, return_total = T, unit = "gdp")

compute_transfers <- function(taxes = union_taxes, parties = union_parties, waivers = union_waivers, data = df) {
  parties <- data$code %in% parties & !data$code %in% union_waivers # /!\ Waiver and deviation from benchmark emissions rights are coded at once
  data$transfer[data$contributing & parties] <- -pooled * participation_factor(parties, data) * df$key[data$contributing & parties]
  total_transfer <- -sum(data$transfer, na.rm = T)
  data$transfer[!data$contributing & parties] <- total_transfer * data$key[!data$contributing & parties]/sum(data$key[!data$contributing & parties], na.rm = T)
  return(data$transfer)
} # TODO: conditional cooperation on carbon pricing, relevant if carbon pricing is separated from other taxes 

compute_budget_gain <- function(taxes = union_taxes, parties = union_parties, data = df) {
  data$budget_gain <- 0
  for (t in taxes) data$budget_gain <- data$budget_gain + data[[t]]
  data$budget_gain <- data$budget_gain + compute_transfers(taxes, parties, data)
  data$budget_gain[!data$code %in% parties] <- NA
  return(data$budget_gain)
}

compute_union <- function(taxes = union_taxes, scenario = "central", parties = NULL, waivers = union_waivers, data = df) {
  if (is.null(parties)) parties <- scenarios_parties[[scenario]]
  data[[paste0("transfer_union_", scenario)]] <- compute_transfers(taxes, parties, data)
  data[[paste0("budget_gain_union_", scenario)]] <- compute_budget_gain(taxes, parties, data) 
  data[[paste0("transfer_union_", scenario, "_over_gdp")]] <- data[[paste0("transfer_union_", scenario)]] / data$gni_nom_2023
  data[[paste0("budget_gain_union_", scenario, "_over_gdp")]] <- data[[paste0("budget_gain_union_", scenario)]] / data$gni_nom_2023
  data[[paste0("transfer_union_", scenario, "_pc")]] <- data[[paste0("transfer_union_", scenario)]] / data$pop_2023
  data[[paste0("budget_gain_union_", scenario, "_pc")]] <- data[[paste0("budget_gain_union_", scenario)]] / data$pop_2023
  return(data)
}

scenarios_names <- c("all_countries", "all_but_OPEC", "optimistic", "central", "prudent", "africa_EU")
for (s in scenarios_names) df <- compute_union(taxes = union_taxes, scenario = s, data = df)
df <- compute_union(taxes = union_taxes, scenario = "South", parties = df$code[!df$contributing], data = df)
df <- compute_union(taxes = union_taxes, scenario = "all_countries", parties = df$code, data = df)


##### Figures #####
plot_world_map("transfer_union_central_over_gdp", df = df, breaks = c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, Inf)*100, sep = "% to ", end = "%", return = "levels")),
               legend = "International\ntransfers\nfrom new taxes\n(in % of GNI)\nScenario: Central", colors = color(13)[2:11], # color(13)[1:11]
               save = T)

plot_world_map("budget_gain_union_central_over_gdp", df = df, breaks = c(-Inf, -.0001, 0, .025, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, 0, .025, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")), colors = color(11)[c(1:7,8)], 
               legend = "Budget gain\nfrom new taxes\n(in % of GNI)\nScenario: Central", 
               save = T)

plot_world_map("transfer_union_South_over_gdp", df = df, breaks = c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-.015, -.005, -.001, 0.001, .005, .02, .05, .1, Inf)*100, sep = "% to ", end = "%", return = "levels")),
               legend = "International\ntransfers\nfrom new taxes\n(in % of GNI)\nScenario: South", colors = color(13)[2:11], # color(13)[1:11]
               save = T)

plot_world_map("budget_gain_union_South_over_gdp", df = df, breaks = c(-Inf, -.0001, 0, .025, .05, .1, .2, Inf), format = c('png', 'pdf'), legend_x = .08, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, 0, 0, .025, .05, .1, .2, Inf)*100, sep = "% to ", end = "%", return = "levels")), colors = color(11)[c(1:7,8)], 
               legend = "Budget gain\nfrom new taxes\n(in % of GNI)\nScenario: South", 
               save = T)

df$transfer_union_central_over_gdp[df$code == "FRA"]
df$transfer_union_all_countries_over_gdp[df$code == "FRA"]
sum(df$transfer_union_central, na.rm = T)
-sum(df$transfer_union_central[df$contributing], na.rm = T)/1e9
-sum(df$transfer_union_all_countries[df$contributing], na.rm = T)/1e9 # /!\ Only 491G because 766G included 50% of billionaire tax for L&D on top of the 1% 
df$transfer_union_central[df$code=="CHN"]

plot_world_map("SSouth_npv_over_gdp_gcs_adj", df = df, breaks = c(-Inf, -.02, -.01, -.003, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .075, trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.01, -.005, 0, 0, .005, .02, .05, Inf)*100, sep = " à ", return = "levels")), filename = "SSouth_npv_over_gdp_gcs_adj_fr",
               legend = "Gains nets suite au\nPlan mondial pour le climat\nagrégés sur le siècle\n(en % du PIB)\nScénario: South", #fill_na = T, \n(with 3% discount rate)
               save = T) # , parties = scenarios_parties["South"]





















