# North-South transfer C&S
cstra <- read.csv(paste0(nice_path, "global_cap_share_18_rampup/country_output/transfer.csv"))
csgdp <- read.csv(paste0(nice_path, "global_cap_share_18_rampup/global_output/global_gross_output.csv"))
(cs_global_transfers <- round(setNames(sapply(2030:2080, function(y) sum(cstra$transfer[cstra$time == y & cstra$transfer > 0])/1e9), 2030:2080)))
(cs_global_transfers_over_gdp <- round(100*setNames(sapply(2030:2080, function(y) sum(cstra$transfer[cstra$time == y & cstra$transfer > 0])/(csgdp$YGROSS_global[csgdp$time == y]*1e12)), 2030:2080), 2))
mean(cs_global_transfers_over_gdp) # 0.40%

# TODO If oil exporting countries, representing 25% of current emissions, do not join the coalition, temperature in 2100 would be about 0.3°C higher than with a universal participation to decarbonization effort

# Current trend Union:
sum(v$emissions_bau[v$region %in% regions_union]) + sum(df[df$participate_union & !df$region_tiam %in% regions_union, paste0("bau_nice_emissions_", 2030:2080)])*sum(v$emissions_bau[!v$region %in% c(regions_union, "World", "union")])/sum(df[!df$region_tiam %in% regions_union, paste0("bau_nice_emissions_", 2030:2080)]) # 924
sum(v$emissions_ndc30[v$region %in% regions_union]) + sum(df[df$participate_union & !df$region_tiam %in% regions_union, paste0("emissions_", 2030:2080)])*sum(v$emissions_ndc30[!v$region %in% c(regions_union, "World", "union")])/sum(df[!df$region_tiam %in% regions_union, paste0("emissions_", 2030:2080)]) # 788
emissions_bau_union_2030_2100 + sum(df[df$participate_union & !df$region_tiam %in% regions_union, paste0("bau_nice_emissions_", 2030:2100)])*sum(v$emissions_bau[!v$region %in% c(regions_union, "World", "union")])/sum(df[!df$region_tiam %in% regions_union, paste0("bau_nice_emissions_", 2030:2100)]) # 1134
sum(df$rights[df$participate_union])/1e9 # 691

(sum((df$rights_proposed - df$rights)[df$region_tiam %in% c("CHI", "WEU")]))/1e9 # 11
sum(df$emissions_2025[df$code %in% names(manual_adjust[manual_adjust > 1.1])])/sum(df$emissions_2025) # 4.6% EXPORT
sum(df$pop_2025[df$code %in% names(manual_adjust[manual_adjust > 1.1])])/sum(df$pop_2025) # 4.0%
sum(df$rights[df$code %in% names(manual_adjust[manual_adjust > 1.1])])/1e9 # 21

print(max(colSums(df[df$participate_union, paste0("rights_proposed_", 2031:2080)])/colSums(df[df$participate_union, paste0("rights_proposed_", 2030:2079)])))
# colSums(df[df$participate_union, paste0("rights_proposed_", 2031:2080)])/colSums(df[df$participate_union, paste0("rights_proposed_", 2030:2079)])
which(colSums(df[df$participate_union, paste0("rights_proposed_", 2031:2080)])/colSums(df[df$participate_union, paste0("rights_proposed_", 2030:2079)]) > .98)
1-max(sapply(2035:2080, function(y) (sum(df[df$participate_union, paste0("rights_proposed_", y)])/sum(df[df$participate_union, paste0("rights_proposed_", y-5)]))^.2)) # EXPORT (must be >2%)
