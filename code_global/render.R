##### labels_vars #####
labels_vars <- c(
  "finished" = "Finished",
  "excluded" = "Excluded",
  "click_reminder" = "Clicked on reminder", 
  "country" = "Country",
  "urban_category" = "Urban category",
  "region" = "Region",
  "gender" = "Gender",
  "age_exact" = "Age",
  "country" = "Country",
  "couple" = "Lives with partner",
  "hh_size" = "Household size",
  "zipcode" = "Zipcode",
  "Nb_children__14" = "Number of children below 14",
  "income" = "Income",
  "education" = "Education",
  "employment_status" = "Employment status",
  "race" = "Race",
  "race_white" = "Race: White",
  "race_black" = "Race: Black",
  "race_hispanic" = "Race: Hispanic",
  "race_asian" = "Race: Asian",
  "home_tenant" = "Home: tenant",
  "home_owner" = "Home: owner",
  "home_landlord" = "Home: landlord",
  "home_hosted" = "Home: hosted",
  "wealth_couple" = "Wealth quintile (couple)",
  "wealth_single" = "Wealth quintile (single)", 
  "gcs_win_lose" = "Win/lose to G",
  "nr_win_lose" = "Win/lose to R",
  "both_win_lose" = "Win/lose to G+R",
  "gcs_support" = "Global climate scheme (G)",
  "nr_support" = "National redistribution scheme (R)",
  "cgr_support" = "Coal exit + G + R", # "Support for C+G+R",
  "gcs_belief" = "Belief about G",
  "nr_belief" = "Belief about N",
  "list_exp_gl" = "List exp.: G/C/O",
  "list_exp_rgl" = "List exp.: R/G/C/O", 
  "list_exp_l" = "List exp.: C/O",
  "list_exp_rl" = "List exp.: R/C/O",   
  "conjoint_crg_cr" = "Conjoint: C+R+G vs. C+R",
  "conjoint_cr_gr" = "Conjoint: C+R vs. G+R",
  "conjoint_r_rcg" = "Conjoint: R vs. R+C+G",
  "conjoint_rg_r" = "Conjoint: R+G vs. R",
  "conjoint_rc_r" = "Conjoint: R+C vs. R",
  "conjoint_left_right" = "Conjoint: Left vs. Right",
  "conjoint_leftg_right" = "Conjoint: Left+G vs. Right",
  "conjoint_left_a_b" = "Conjoint: random programs A vs. B",
  "conjoint_left_ag_b" = "Conjoint: random programs A+G vs. B",
  "conjoint_crg_cr_binary" = "Conjoint: >>C+R+G<< vs. C+R",
  "conjoint_cr_gr_binary" = "Conjoint: C+R vs. >>G+R<<",
  "conjoint_r_rcg_binary" = "Conjoint: R vs. >>R+C+G<<",
  "conjoint_rg_r_binary" = "Conjoint: >>R+G<< vs. R",
  "conjoint_rc_r_binary" = "Conjoint: >>R+C<< vs. R",
  "conjoint_left_right_binary" = "Conjoint: >>Left<< vs. Right",
  "conjoint_leftg_right_binary" = "Conjoint: >>Left+G<< vs. Right",
  "conjoint_left_a_b_binary" = "Conjoint: random programs >>A<< vs. B",
  "conjoint_left_ag_b_binary" = "Conjoint: random programs >>A+G<< vs. B",
  "gcs_important_limit_CC" = "It would succeed in limiting climate change",
  "gcs_important_hurt_economy" = "It would hurt the [Country] economy",
  "gcs_important_hurt_me" = "It would penalize my household",
  "gcs_important_change_lifestyles" = "It would make people change their lifestyle",
  "gcs_important_reduce_poverty" = "It would reduce poverty in low-income countries",
  "gcs_important_hurt_poor" = "It might be detrimental to some poor countries",
  "gcs_important_foster_cooperation" = "It could foster global cooperation",
  "gcs_important_fuel_corruption" = "It could fuel corruption in low-income countries",
  "gcs_important_fuel_fraud" = "It could be subject to fraud",
  "gcs_important_difficult_enact" = "It would be technically difficult to put in place",
  "gcs_important_having_info" = "Having enough information on this scheme and its consequences",
  "negotiation" = "What [Country] should defend in climate negotiations",
  "climate_compensation_support" = "Payments from high-income countries to compensate low-income countries for climate damages",
  "climate_mitigation_support" = "High-income countries funding renewable energy in low-income countries",
  "climate_adaptation_support" = "High-income countries contributing $100 billion per year to help low-income countries adapt to climate change",
  "debt_cancellation_support" = "Cancellation of low-income countries' public debt",
  "democratise_un_imf_support" = "Democratise international institutions (UN, IMF) by making a country's voting right proportional to its population",
  "remove_tariffs_support" = "Removing tariffs on imports from low-income countries",
  "global_min_wage_support" = "A minimum wage in all countries at 50% of local median wage",
  "global_register_support" = "Fight tax evasion by creating a global financial register to record ownership of all assets",
  "cap_wealth_support" = "A maximum wealth limit of $10 billion for each human", #(US) / €100 million (EU)",
  "attention_test" = "Attention test",
  "donation_nation" = "Donation to own country",
  "donation_africa" = "Donation to Africa",
  "donation" = "Donation (any)",
  "global_tax_support" = "Global tax on millionaires",
  "national_tax_support" = "National tax on millionaires",
  "global_tax_global_share" = "Preferred share of global tax for low-income",
  "global_tax_sharing" = "Sharing half of global tax with low-income",
  "foreign_aid_belief" = "Belief about foreign aid / public spending",
  "foreign_aid_preferred_no_info" = "Preferred foreign aid (no info)",
  "foreign_aid_preferred_info" = "Preferred foreign aid (with info)",
  "foreign_aid_preferred" = "Preferred foreign aid (any branch)",
  "foreign_aid_raise_how_defense" = "Lower spending on defense",
  "foreign_aid_raise_how_pensions" = "Lower spending on retirement pensions",
  "foreign_aid_raise_how_healthcare" = "Lower spending on healthcare",
  "foreign_aid_raise_how_welfare" = "Lower spending on welfare benefits",
  "foreign_aid_raise_how_education" = "Lower spending on education",
  "foreign_aid_raise_how_other" = "Lower spending on other programs",
  "foreign_aid_raise_how_wealthy" = "Higher taxes on the wealthiest",
  "foreign_aid_raise_how_corporations" = "Higher corporate income tax rate",
  "foreign_aid_raise_how_income_tax" = "Higher personal income tax rates",
  "foreign_aid_raise_how_deficit" = "Higher public deficit",
  "foreign_aid_reduce_how_defense" = "Higher spending on defense",
  "foreign_aid_reduce_how_pensions" = "Higher spending on retirement pensions",
  "foreign_aid_reduce_how_healthcare" = "Higher spending on healthcare",
  "foreign_aid_reduce_how_welfare" = "Higher spending on welfare benefits",
  "foreign_aid_reduce_how_education" = "Higher spending on education",
  "foreign_aid_reduce_how_other" = "Higher spending on other programs",
  "foreign_aid_reduce_how_wealthy" = "Lower taxes on the wealthiest",
  "foreign_aid_reduce_how_corporations" = "Lower corporate income tax rate",
  "foreign_aid_reduce_how_income_tax" = "Lower personal income tax rates",
  "foreign_aid_reduce_how_deficit" = "Lower public deficit",
  "foreign_aid_raise_support" = "Preferred evolution of [Country]'s foreign aid",
  "foreign_aid_condition_human_rights" = "That recipient countries comply with climate targets and human rights",
  "foreign_aid_condition_fight_migration" = "That recipient countries cooperate to fight illegal migrations",
  "foreign_aid_condition_all_high_income" = "That other high-income countries also increase their foreign aid",
  "foreign_aid_condition_tax_rich" = "That this is financed by increased taxes on millionaires",
  "foreign_aid_condition_no_diversion" = "That we can be sure the aid reaches people in need and money is not diverted",
  "foreign_aid_condition_other_choice" = "Other",
  "foreign_aid_no_ineffective" = "Aid perpetuates poverty as it makes people feel less responsible for themselves",
  "foreign_aid_no_diversion" = "Aid is not effective as most of it is diverted",
  "foreign_aid_no_pressure" = "Aid is a pressure tactic for high-income countries that prevents low-income countries from developing freely",
  "foreign_aid_no_not_our_role" = "[Country] is not responsible for what happens in other countries",
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already a lot to do to support the American people in need",
  "foreign_aid_no_other_choice" = "Other",
  "petition_gcs" = "Petition for G",
  "petition_nr" = "Petition for R",
  "petition" = "Petition (any)",
  "donation_charities" = "Donation to charities",
  "interested_politics" = "Interest in politics",
  "group_defended" = "Group defended when voting",
  "involvement_govt" = "Govt involvement",
  "political_affiliation" = "Political affiliation",
  "left_right" = "Left - right on economics",
  "vote_participation" = "Vote participation",
  "voted" = "Voted at last election",
  "vote_fr_voters" = "Vote (voters)",
  "vote_fr_non_voters" = "Vote (non voters)",
  "vote_de_voters" = "Vote (voters)",
  "vote_de_non_voters" = "Vote (non voters)",
  "vote_es_voters" = "Vote (voters)",
  "vote_es_non_voters" = "Vote (non voters)",
  "vote_uk_voters" = "Vote (voters)",
  "vote_uk_non_voters" = "Vote (non voters)",
  "problem_inequality" = "Income inequality in [Country]",
  "problem_climate" = "Climate change",
  "problem_poverty" = "Global poverty",
  "points_econ1" = "econ1",
  "points_econ2" = "econ2",
  "points_econ3" = "econ3",
  "points_econ4" = "econ4",
  "points_soc1" = "soc1",
  "points_soc2" = "soc2",
  "points_soc3" = "Making abortion a right at the federal level",
  "points_climate1" = "climate1",
  "points_climate2" = "climate2",
  "points_climate3" = "climate3",
  "points_tax1_nr" = "tax1: National redistribution scheme",
  "points_tax2_wealth_tax" = "tax2: Wealth tax",
  "points_tax3" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_foreign1_gcs" = "foreign1: Global climate scheme",
  "points_foreign2_tax_rich" = "foreign2: Global tax on millionaires",
  "points_foreign3_assembly" = "foreign3: Global democratic assembly on climate change",
  "points_foreign4_aid" = "foreign4: Doubling foreign aid",
  "survey_biased" = "Survey biased", 
  "survey_biased_yes" = "Survey is biased", 
  "survey_biased_left" = "Survey is left-wing biased", 
  "survey_biased_right" = "Survey is right-wing biased", 
  "survey_biased_no" = "Survey is not biased", 
  "interview" = "Agrees for interview",
  "duration" = "Duration",
  "duration_gcs" = "Duration: G comprehension",
  "duration_nr" = "Duration: R comprehension",
  "duration_both" = "Duration: G+R comprehension",
  "duration_gcs" = "Duration: G questions",
  "duration_conjoint_a" = "Duration: conjoint (a)",
  "duration_conjoint_b" = "Duration: conjoint (b)",
  "duration_conjoint_c" = "Duration: conjoint (c)",
  "duration_conjoint_d" = "Duration: conjoint (d)",
  "duration_gcs_perception" = "Duration: G perceptions",
  "duration_other_policies" = "Duration: other policies",
  "duration_feedback" = "Duration: feedback",
  "duration_points" = "Duration: 100 points",
  "score_understood" = "Number of correct answers to understanding questions",
  "gcs_understood" = "With G, typical [country] people lose and poorest humans win",
  "nr_understood" = "With R, typical [country] people win and richest win",
  "both_understood" = "With G+R, typical [country] people neither win nor lose",
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "petition_matches_support" = "Petition and support answers match",
  "conjoint_a_matches_support" = "Conjoint (a) and support answers match"
)
for (v in c(variables_donation, variables_points, variables_belief, variables_foreign_aid_amount, "share_policies_supported")) labels_vars[paste0(v, "_agg")] <- labels_vars[v]

##### labels_vars_short_html #####
labels_vars_short_html <- c(
  "list_exp_gl" = "G/C/O",
  "list_exp_rgl" = "R/G/C/O", 
  "list_exp_l" = "C/O",
  "list_exp_rl" = "R/C/O",   
  "conjoint_crg_cr_binary" = "<b>C+R+G</b> vs. C+R",
  "conjoint_cr_gr_binary" = "C+R vs. <b>G+R</b>",
  "conjoint_r_rcg_binary" = "R vs. <b>R+C+G</b>",
  "conjoint_rg_r_binary" = "<b>R+G</b> vs. R",
  "conjoint_rc_r_binary" = "<b>R+C</b> vs. R",
  "conjoint_left_right_binary" = "<b>Left</b> vs. Right",
  "conjoint_leftg_right_binary" = "<b>Left+G</b> vs. Right",
  "conjoint_left_a_b_binary" = "Random program <b>A</b> vs. B",
  "conjoint_left_ag_b_binary" = "Random program <b>A+G</b> vs. B",
  "gcs_important_limit_CC" = "It would succeed in limiting climate change",
  "gcs_important_hurt_economy" = "It would hurt the [Country] economy",
  "gcs_important_hurt_me" = "It would penalize my household",
  "gcs_important_change_lifestyles" = "It would make people change their lifestyle",
  "gcs_important_reduce_poverty" = "It would reduce poverty in low-income countries",
  "gcs_important_hurt_poor" = "It might be detrimental to some poor countries",
  "gcs_important_foster_cooperation" = "It could foster global cooperation",
  "gcs_important_fuel_corruption" = "It could fuel corruption<br>in low-income countries",
  "gcs_important_fuel_fraud" = "It could be subject to fraud",
  "gcs_important_difficult_enact" = "It would be technically difficult<br>to put in place",
  "gcs_important_having_info" = "Having enough information<br>on this scheme and its consequences",
  "negotiation" = "What [Country] should defend in climate negotiations",
  "climate_compensation_support" = "Payments from high-income countries to compensate<br>low-income countries for climate damages",
  "climate_mitigation_support" = "High-income countries funding renewable<br>energy in low-income countries",
  "climate_adaptation_support" = "High-income countries contributing $100 billion per year<br>to help low-income countries adapt to climate change",
  "debt_cancellation_support" = "Cancellation of low-income countries' public debt",
  "democratise_un_imf_support" = "Democratise international institutions (UN, IMF) by making<br>a country's voting right proportional to its population",
  "remove_tariffs_support" = "Removing tariffs on imports from low-income countries",
  "global_min_wage_support" = "A minimum wage in all countries<br>at 50% of local median wage",
  "global_register_support" = "Fight tax evasion by creating a global financial register<br>to record ownership of all assets",
  "cap_wealth_support" = "A maximum wealth limit of<br>$10 billion (US) / €100 million (EU)",
  "foreign_aid_condition_human_rights" = "That recipient countries comply<br>with climate targets and human rights",
  "foreign_aid_condition_fight_migration" = "That recipient countries cooperate<br>to fight illegal migrations",
  "foreign_aid_condition_all_high_income" = "That other high-income countries<br>also increase their foreign aid",
  "foreign_aid_condition_tax_rich" = "That this is financed by<br>increased taxes on millionaires",
  "foreign_aid_condition_no_diversion" = "That we can be sure the aid reaches<br>people in need and money is not diverted",
  "foreign_aid_condition_other_choice" = "Other",
  "foreign_aid_no_ineffective" = "Aid perpetuates poverty as it makes<br>people feel less responsible for themselves",
  "foreign_aid_no_diversion" = "Aid is not effective<br>as most of it is diverted",
  "foreign_aid_no_pressure" = "Aid is a pressure tactic for high-income countries that<br>prevents low-income countries from developing freely",
  "foreign_aid_no_not_our_role" = "[Country] is not responsible<br>for what happens in other countries",
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already<br>a lot to do to support the American people in need"
)

##### labels_vars_US #####
labels_vars_us <- c(
  "points_econ1_agg" = "Student loan forgiveness",
  "points_econ2_agg" = "$15 minimum wage",
  "points_econ3_agg" = "Universal childcare/pre-K",
  "points_econ4_agg" = "Funding affordable housing",
  "points_soc1_agg" = "Expanding the Supreme Court",
  "points_soc2_agg" = "Handgun ban",
  "points_soc3_agg" = "Making abortion a right at the federal level", # TODO
  "points_climate1_agg" = "Coal exit",
  "points_climate2_agg" = "Trillion dollar investment in clean transportation<br>infrastructure and building insulation",
  "points_climate3_agg" = "Ban the sale of new<br>combustion-engine cars by 2030",
  "points_tax1_nr_agg" = "National redistribution scheme",
  "points_tax2_wealth_tax_agg" = "Wealth tax",
  "points_tax3_agg" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_foreign1_gcs_agg" = "Global climate scheme",
  "points_foreign2_tax_rich_agg" = "Global tax on millionaires",
  "points_foreign3_assembly_agg" = "Global democratic assembly on climate change",
  "points_foreign4_aid_agg" = "Doubling foreign aid"
)

fill_heatmaps <- function(list_var_list = NULL, heatmaps = heatmaps_defs, conditions = c("", ">= 1", "/"), sort = FALSE) {
  # list_var_list can be NULL, a named list of vectors of variables, a named list of type heatmaps_defs, or a list of names of (existing) vectors of variables (with or without the prefix 'variables_')
  # /!\ Bug if an object named 'heatmaps' exists in the environment.
  if (missing(list_var_list)) list_var_list <- list()
  if (is.character(list_var_list)) {
    vec_vars <- list_var_list
    list_var_list <- list()
    for (vars in vec_vars) {
      vars <- sub("variables_", "", vars)
      if (!vars %in% names(heatmaps)) list_var_list[[vars]]$vars <- eval(str2expression(paste0("variables_", vars))) # do not override an already defined variable vec with this name
    } 
  }  
  if (length(list_var_list) != length(names(list_var_list))) warning("'list_var_list' cannot be an unnamed list.")
  # We fill heatmaps with the entries given in input
  for (name in names(list_var_list)) {
    if (!is.list(list_var_list[[name]])) list_var_list[[name]] <- list(vars = list_var_list[[name]])
    var_list <- list_var_list[[name]]
    if (!name %in% names(heatmaps)) heatmaps[[name]] <- var_list
    else for (key in names(var_list)) heatmaps[[name]][[key]] <- var_list[[key]] # TODO? if (!key %in% names(heatmaps[[name]])) ?
  }
  # We complete the missing fields of heatmaps 
  for (name in names(heatmaps)) {
    if (!"name" %in% names(heatmaps[[name]])) heatmaps[[name]]$name <- name
    if (!"labels" %in% names(heatmaps[[name]])) {
      if (!"vars" %in% names(heatmaps[[name]])) { warning(paste("'vars' must be specified for", name)) }
      heatmaps[[name]]$labels <- c()
      for (var in heatmaps[[name]]$vars) heatmaps[[name]]$labels <- c(heatmaps[[name]]$labels, ifelse(var %in% names(labels_vars), labels_vars[var], var))
    }
    if (!"conditions" %in% names(heatmaps[[name]])) heatmaps[[name]]$conditions <- conditions
    if (!"sort" %in% names(heatmaps[[name]])) heatmaps[[name]]$sort <- sort
  }
  return(heatmaps)
}
# (heatmaps_defs <- fill_heatmaps(c("other_policies"), list()))

##### heatmaps_defs #####
heatmaps_defs <- list(
  "global_tax_global_share" = list(vars = c("global_tax_global_share"), conditions = c("", ">= 1")),
  "global_tax_sharing" = list(vars = c("global_tax_sharing"), conditions = c(">= 1")),
  "list_exp" = list(vars = variables_list_exp, conditions = c("")),
  "understood_all" = list(vars = variables_understood, conditions = c("")),
  "understood_each" = list(vars = variables_understood[1:3], conditions = c(">= 1")),
  "understood_score" = list(vars = variables_understood[4], conditions = c("")),
  "gcs_important" = list(vars = variables_gcs_important, conditions = c("", ">= 1")),
  "support_binary" = list(vars = variables_support_binary, conditions = ">= 1"),
  "petition" = list(vars = variables_petition, conditions = ">= 1"),
  "conjoint" = list(vars = variables_conjoint_binary, conditions = ">= 1"),
  "conjoint_a" = list(vars = variables_conjoint_a_binary, conditions = ">= 1"),
  "conjoint_b" = list(vars = variables_conjoint_b_binary, conditions = ">= 1"),
  "conjoint_c" = list(vars = variables_conjoint_c_binary, conditions = ">= 1"),
  "conjoint_d" = list(vars = variables_conjoint_d_binary, conditions = ">= 1"),
  "duration" = list(vars = variables_duration, conditions = ""),
  "donation" = list(vars = c("donation_nation", "donation_africa"), conditions = c(""), percent = FALSE), # removes 'donation'
  "belief" = list(vars = variables_belief, conditions = "", percent = FALSE), 
  "points" = list(vars = variables_points, conditions = c("", ">= 1"), percent = FALSE), # TODO: 0 digit
  "foreign_aid_amount" = list(vars = variables_foreign_aid_amount, conditions = c(""), percent = FALSE),
  "foreign_aid_raise" = list(vars = variables_foreign_aid_raise, conditions = ">= 1"),
  "foreign_aid_reduce" = list(vars = variables_foreign_aid_reduce, conditions = ">= 1"),
  "foreign_aid_no" = list(vars = variables_foreign_aid_no[!grepl("other", variables_foreign_aid_no)]),
  "foreign_aid_condition" = list(vars = variables_foreign_aid_condition[!grepl("other", variables_foreign_aid_condition)]),
  "share_policies_supported" = list(vars = c("share_policies_supported"), conditions = c("")),
  "support_match" = list(vars = c("petition_matches_support", "conjoint_a_matches_support"), conditions = c(">= 1"))
)

##### vars_heatmaps #####
vars_heatmaps <- c("support", "other_policies", "climate_policies", "global_policies", "support_binary", "support_likert", "petition", "gcs_important", "problem", 
                        "foreign_aid_amount", "duration", "donation", "belief", "points", "foreign_aid_raise", "foreign_aid_reduce", "foreign_aid_no", "foreign_aid_condition", 
                        "conjoint", "conjoint_a", "conjoint_b", "conjoint_c", "list_exp", "understood") # misses socio-demos, politics

heatmaps_defs <- fill_heatmaps(vars_heatmaps, heatmaps_defs)
# heatmaps_defs$foreign_aid_no

heatmap_multiple <- function(heatmaps = heatmaps_defs, data = e, trim = FALSE, weights = T) {
  for (heatmap in heatmaps) {
    vars_present <- heatmap$vars %in% names(data)
    heatmap_wrapper(vars = heatmap$vars[vars_present], data = data, labels = heatmap$labels[vars_present], name = heatmap$name, conditions = heatmap$conditions, sort = heatmap$sort, trim = trim, weights = weights) 
  }
}


##### Barres #####
barres_multiple <- function(barres = barres_defs, df = e, folder = NULL, print = T, export_xls = FALSE, trim = FALSE, method = 'orca', format = 'pdf', weights = T) {
  if (missing(folder)) folder <- automatic_folder(along = "country", data = df, several = "all")
  for (def in barres) {
    tryCatch({
      vars_present <- def$vars %in% names(df)
      plot <- barres(vars = def$vars[vars_present], df = df, export_xls = export_xls, labels = def$labels[vars_present], miss = def$miss, sort = def$sort, rev = def$rev, rev_color = def$rev_color, legend = def$legend, showLegend = def$showLegend, thin = def$thin, weights = weights)
      if (print) print(plot)
      save_plotly(plot, filename = def$name, folder = folder, width = def$width, height = def$height, method = method, trim = trim, format = format)
      print(paste0(def$name, ": success"))
    }, error = function(cond) { print(paste0(def$name, ": failed.")) } )
  }
}

fill_barres <- function(list_var_list = NULL, plots = barres_defs, df = e, miss = FALSE, sort = T, thin = T, rev = FALSE, rev_color = T, rev_legend = FALSE,
                        short_labels = T, width = dev.size('px')[1]) { # width/height could be NULL by default as well, so plotly decides the size , height = dev.size('px')[2]
  # list_var_list can be NULL, a named list of vectors of variables, a named list of type plots_defs, or a list of names of (existing) vectors of variables (with or without the prefix 'variables_')
  # If df$var and variables_var both exist, giving 'var' (resp. 'variables_var') will yield var (resp. variables_var)
  # /!\ Bug if an object named 'plots' exists in the environment.
  if (!exists("labels_vars")) warning("'labels_vars' should exist but does not.")
  labels <- labels_vars
  if (exists("labels_vars_short_html") & short_labels) labels[names(labels_vars_short_html)] <- labels_vars_short_html
  if (grepl("us", deparse(substitute(df)))) labels[names(labels_vars_us)] <- labels_vars_us
  if (missing(list_var_list)) list_var_list <- list()
  if (is.character(list_var_list)) {
    vec_vars <- list_var_list
    list_var_list <- list()
    for (vars in vec_vars) {
      multi <- grepl("^variables_", vars) | (exists(paste0("variables_", vars)) & !vars %in% names(df))
      variables <- ifelse(multi, paste0("variables_", sub("variables_", "", vars)), sub("variables_", "", vars))
      if (!vars %in% names(plots)) list_var_list[[vars]]$vars <- if (multi) eval(str2expression(variables)) else variables # do not override an already defined variable vec with this name
    } 
  }  
  if (length(list_var_list) != length(names(list_var_list))) warning("'list_var_list' cannot be an unnamed list.")
  # We fill plots with the entries given in input
  for (name in names(list_var_list)) {
    if (!is.list(list_var_list[[name]])) list_var_list[[name]] <- list(vars = list_var_list[[name]])
    var_list <- list_var_list[[name]]
    if (!name %in% names(plots)) plots[[name]] <- var_list
    else for (key in names(var_list)) plots[[name]][[key]] <- var_list[[key]] # TODO? if (!key %in% names(plots[[name]])) ?
  }
  # We complete the missing fields of plots 
  for (name in names(plots)) {
    if (!"vars" %in% names(plots[[name]])) plots[[name]]$vars <- if (name %in% names(df)) name else eval(str2expression(paste0("variables_", sub("variables_", "", name))))
    if (!"name" %in% names(plots[[name]])) plots[[name]]$name <- name
    if (!"labels" %in% names(plots[[name]])) {
      plots[[name]]$labels <- c()
      for (var in plots[[name]]$vars) plots[[name]]$labels <- c(plots[[name]]$labels, ifelse(var %in% names(labels), labels[var], var))
    }
    if (!"miss" %in% names(plots[[name]])) plots[[name]]$miss <- miss
    if (!"sort" %in% names(plots[[name]])) plots[[name]]$sort <- sort
    if (!"rev" %in% names(plots[[name]])) plots[[name]]$rev <- rev
    if (!"rev_color" %in% names(plots[[name]])) plots[[name]]$rev_color <- rev_color
    if (!"rev_legend" %in% names(plots[[name]])) plots[[name]]$rev_legend <- rev_legend
    vars_in <- plots[[name]]$vars[plots[[name]]$vars %in% names(df)]
    var_example <- vars_in[1]
    if (!"legend" %in% names(plots[[name]]) & !is.na(var_example)) plots[[name]]$legend <- dataN(var_example, data=df, miss=plots[[name]]$miss, return = "legend", fr=F, rev_legend = plots[[name]]$rev_legend)
    # yes_no <- setequal(plots[[name]]$legend, c('Yes', 'No', 'PNR')) | setequal(plots[[name]]$legend, c('Oui', 'Non', 'NSP')) | setequal(plots[[name]]$legend, c('Yes', 'No')) | setequal(plots[[name]]$legend, c('Oui', 'Non'))
    # if (!"showLegend" %in% names(plots[[name]])) plots[[name]]$showLegend <- if (is.na(var_example))  T else (!is.binary(df[[var_example]]) | yes_no)
    if (!"showLegend" %in% names(plots[[name]])) plots[[name]]$showLegend <- if (is.na(var_example))  T else (!is.logical(df[[var_example]]))
    if (!"thin" %in% names(plots[[name]])) plots[[name]]$thin <- thin #& !yes_no
    if (!"width" %in% names(plots[[name]])) plots[[name]]$width <- width
    if (!"height" %in% names(plots[[name]]) & "heigth" %in% names(plots[[name]])) plots[[name]]$height <- plots[[name]]$heigth
    if (!"height" %in% names(plots[[name]])) plots[[name]]$height <- fig_height(nb_bars = length(plots[[name]]$labels), large = any(grepl("<br>", plots[[name]]$labels))) # height
  }
  return(plots)
}
# TODO automatize miss, width, thin params

##### barres_defs #####
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  "understood_each" = list(vars = variables_understood[1:3], width = 1380),
  "problem" = list(width = 1050),
  "support_binary" = list(width = 770),
  "support_likert" = list(width = 1275),
  "climate_policies" = list(width = 1221),
  "global_policies" = list(width = 1275),
  "other_policies" = list(width = 1270),
  "climate_policies" = list(width = 1221),
  "variables_list_exp" = list(width = 500), # TODO manage the different branches e.g. as heterogeneity
  "variables_petition" = list(vars = c("petition_gcs", "petition_nr"), width = 500),
  "variables_donation" = list(vars = c("donation_africa_agg", "donation_nation_agg"), width = 835),
  "foreign_aid_amount" = list(vars = variables_foreign_aid_amount_agg, width = 1080),
  "belief" = list(vars = variables_belief_agg, width = 750),
  "points" = list(vars = variables_points_agg, width = 1080, sort = FALSE), # TODO: average
  "points_us" = list(vars = variables_points_us_agg, width = 1080, sort = FALSE),
  "share_policies_supported" = list(vars = "share_policies_supported_agg", width = 950),
  # # "understood_score" = list(vars = variables_understood[4]), # TODO
  # "gcs_important" = list(vars = variables_gcs_important, conditions = c("", ">= 1")),
  # "support_binary" = list(vars = variables_support_binary, conditions = ">= 1"),
  # "petition" = list(vars = variables_petition, conditions = ">= 1"),
  "conjoint" = list(vars = variables_conjoint_binary, width = 900, sort = FALSE),
  # "conjoint_a" = list(vars = variables_conjoint_a_binary, conditions = ">= 1"),
  # "conjoint_b" = list(vars = variables_conjoint_b_binary, conditions = ">= 1"),
  # "conjoint_c" = list(vars = variables_conjoint_c_binary, conditions = ">= 1"),
  # "conjoint_d" = list(vars = variables_conjoint_d_binary, conditions = ">= 1"),
  # "duration" = list(vars = variables_duration, conditions = ""),
  # "donation" = list(vars = c("donation_nation", "donation_africa"), conditions = c(""), percent = FALSE), # removes 'donation'
  # "belief" = list(vars = variables_belief, conditions = "", percent = FALSE), 
  # "points" = list(vars = variables_points, conditions = c("", ">= 1"), percent = FALSE), # TODO: 0 digit
  # "foreign_aid_amount" = list(vars = variables_foreign_aid_amount, conditions = c(""), percent = FALSE),
  # "foreign_aid_raise" = list(vars = variables_foreign_aid_raise, conditions = ">= 1"),
  # "foreign_aid_reduce" = list(vars = variables_foreign_aid_reduce, conditions = ">= 1"),
  "foreign_aid_no" = list(vars = variables_foreign_aid_no[!grepl("other", variables_foreign_aid_no)], width = 1125),
  "foreign_aid_condition" = list(vars = variables_foreign_aid_condition[!grepl("other", variables_foreign_aid_condition)], width = 955),
  "support_match" = list(vars = c("petition_matches_support", "conjoint_a_matches_support"), width = 950)
)

# TODO "duration", socio-demos, politics, survey_biased, individual variables
vars_barres <- c("other_policies", "climate_policies", "global_policies", "support_binary", "support_likert", "variables_petition", "gcs_important", "problem", 
                  "foreign_aid_raise", "foreign_aid_reduce", "foreign_aid_no", "foreign_aid_condition", "global_tax_global_share", "global_tax_sharing", "conjoint", "variables_list_exp") 

barres_defs <- fill_barres(vars_barres, barres_defs, df = us1)
# barres_defs$foreign_aid_no


##### Run #####
barres_multiple(barres = barres_defs[c("points_us", "belief", "variables_donation", "share_policies_supported")], df = us1, folder = "../figures/US1/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'
barres_multiple(barres = barres_defs[c("foreign_aid_amount")], df = usp, folder = "../figures/USp/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'
(temp <- barres(vars = variables_support_binary[1:3], rev = F, rev_color = T, export_xls = F, df = usp, sort = T, thin = F, miss=F, showLegend = T, legend = c("No", "Yes"), labels=unname(labels_vars[variables_support_binary[1:3]])))
(test <- barres(vars = c("cap_wealth_support", "remove_tariffs_support"), rev = F, rev_color = T, export_xls = F, df = usp, sort = T, thin = T, miss=F, labels=unname(labels_vars[c("cap_wealth_support", "remove_tariffs_support")])))
save_plotly(test, filename = "cap_wealth_support", folder = "../figures/USp/", width = NULL, height = NULL, trim = FALSE)

heatmap_multiple() # Doesn't work if data contains a single country (by design, to avoid overwriting files)
heatmap_multiple(heatmaps_defs[c("support_match", "share_policies_supported", "understood_all", "understood_each", "understood_score")])
# heatmaps_defs <- fill_heatmaps(c("conjoint_a_binary"), list())
# heatmap_multiple(heatmaps = heatmaps_defs)
# TODO trim, break labels automatically
# TODO? Arial or Computer modern (Times)?

# x heatmap OECD
# v support (HEAT + ctry)
# - conjoint (heat + ctry + (r) + by party)
# - prioritization (distr + heterog distr + PLOT_ALONG)
# v list exp (TAB + heat + ctry)
# - foreign aid evolution (ctry + heterog)
# - foreign aid why (ctry)
# - petition (ctry + heterog + tab)
# - belief (CDF + ctry + heterog)
# - negotiation (ctry + heterog)
# - group defended (ctry + heterog)
# - problem (ctry + heat)