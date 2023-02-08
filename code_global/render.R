# TODO! add G to OECD heatmap, remove Dependence on what other countries do, change label titles to make it clear that the first one was multiple answers while the others were likert
# TODO! "duration", socio-demos, politics compare
# TODO list_exp, group_defended_agg2
# TODO trim
# TODO refresh Viewer with laptop (i.e. automatic rstudioapi::executeCommand('viewerRefresh'))
# TODO automatically set share_labels and margin_l
# TODO? Arial or Computer modern (Times)?
# TODO automatize miss, width, thin params

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
  "urbanity" = "Degree of urbanization",
  "age" = "Age",
  "Nb_children__14" = "Number of children below 14",
  "income" = "Income",
  "education" = "Highest diploma",
  "diploma" = "Highest diploma",
  "diploma_25_64" = "Highest diploma among 25-64",
  "education" = "Highest diploma (ISCED class)",
  "education_original" = "Highest diploma",
  "employment_agg" = "Employment status",
  "employment_status" = "Employment status",
  "employment_18_64" = "Employment among 18-64",
  "race" = "Race",
  "race_white" = "Race: White",
  "race_black" = "Race: Black",
  "race_hispanic" = "Race: Hispanic",
  "race_asian" = "Race: Asian",
  "home_tenant" = "Home: tenant",
  "home_owner" = "Home: owner",
  "home_landlord" = "Home: landlord",
  "home_hosted" = "Home: hosted",
  "income_decile" = "Income decile",
  "income_quartile" = "Income quartile",
  "owner" = "Owner",
  "wealth" = "Wealth quintile",
  "vote" = "Vote",
  "vote_all" = "Vote (actual and hypothetical)",
  "vote_us" = "Vote",
  "vote_agg" = "Vote (actual and hypothetical)",
  "vote_us_voters" = "Vote (voters)",
  "vote_us_non_voters" = "Vote intention (non voters)",
  "wealth_couple" = "Wealth quintile (couple)",
  "wealth_single" = "Wealth quintile (single)", 
  "gcs_win_lose" = "Win/lose to GCS",
  "nr_win_lose" = "Win/lose to NR",
  "both_win_lose" = "Win/lose to GCS+NR",
  "gcs_support" = "Global climate scheme (GCS)",
  "nr_support" = "National redistribution scheme (NR)",
  "cgr_support" = "Coal exit + GCS + NR", # "Support for C+G+R",
  "gcs_belief" = "Belief about GCS",
  "nr_belief" = "Belief about NR",
  "list_exp_gl" = "List exp.: GCS/C/O",
  "list_exp_rgl" = "List exp.: NR/GCS/C/O", 
  "list_exp_l" = "List exp.: C/O",
  "list_exp_rl" = "List exp.: NR/C/O",   
  "conjoint_crg_cr" = "Conjoint: C+NR+GCS vs. C+NR",
  "conjoint_cr_gr" = "Conjoint: C+NR vs. GCS+NR",
  "conjoint_r_rcg" = "Conjoint: NR vs. NR+C+GCS",
  "conjoint_rg_r" = "Conjoint: NR+GCS vs. NR",
  "conjoint_rc_r" = "Conjoint: NR+C vs. NR",
  "conjoint_left_right" = "Conjoint: Left vs. Right",
  "conjoint_leftg_right" = "Conjoint: Left+GCS vs. Right",
  "conjoint_left_a_b" = "Conjoint: random programs A vs. B",
  "conjoint_left_ag_b" = "Conjoint: random programs A+GCS vs. B",
  "conjoint_crg_cr_binary" = "Conjoint: >>C+NR+GCS<< vs. C+NR",
  "conjoint_cr_gr_binary" = "Conjoint: C+NR vs. >>GCS+NR<<",
  "conjoint_r_rcg_binary" = "Conjoint: NR vs. >>NR+C+GCS<<",
  "conjoint_rg_r_binary" = "Conjoint: >>NR+GCS<< vs. NR",
  "conjoint_rc_r_binary" = "Conjoint: >>NR+C<< vs. NR",
  "conjoint_left_right_binary" = "Conjoint: >>Left<< vs. Right",
  "conjoint_leftg_right_binary" = "Conjoint: >>Left+GCS<< vs. Right",
  "conjoint_left_a_b_binary" = "Conjoint: random programs >>A<< vs. B",
  "conjoint_left_ag_b_binary" = "Conjoint: random programs >>A+GCS<< vs. B",
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
  "foreign_aid_raise_support" = "Should [Country]'s foreign aid increase?",
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
  "petition_gcs" = "Petition for GCS",
  "petition_nr" = "Petition for NR",
  "petition" = "Petition (any)",
  "donation_charities" = "Donation to charities",
  "interested_politics" = "Interested in politics",
  "group_defended" = "Group defended when voting",
  "group_defended_agg" = "Group defended when voting",
  "involvement_govt" = "Desired level of involvement of government",
  "political_affiliation" = "Political affiliation",
  "left_right" = "Left - right on economics",
  "vote_participation" = "Voted at last election",
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
  "points_tax3_corporate_tax_agg" = "Increase corporate income tax<br>rate from 21% to 28%",
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
  "duration_gcs" = "Duration: GCS comprehension",
  "duration_nr" = "Duration: NR comprehension",
  "duration_both" = "Duration: GCS+NR comprehension",
  "duration_gcs" = "Duration: GCS questions",
  "duration_conjoint_a" = "Duration: conjoint (a)",
  "duration_conjoint_b" = "Duration: conjoint (b)",
  "duration_conjoint_c" = "Duration: conjoint (c)",
  "duration_conjoint_d" = "Duration: conjoint (d)",
  "duration_gcs_perception" = "Duration: G perceptions",
  "duration_other_policies" = "Duration: other policies",
  "duration_feedback" = "Duration: feedback",
  "duration_points" = "Duration: 100 points",
  "score_understood" = "Number of correct answers<br>to understanding questions",
  "gcs_understood" = "With GCS,<br>typical [country] people lose and poorest humans win",
  "nr_understood" = "With NR,<br>typical [country] people win and richest lose",
  "both_understood" = "With GCS+NR,<br>typical [country] people neither win nor lose",
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "petition_matches_support" = "Petition and support answers match",
  "conjoint_a_matches_support" = "Conjoint (a) and support answers match",
  "nationalist" = "Nationalist",
  "universalist" = "Universalist",
  "conjoint_a_matches_support" = "Conjoint (a) and support answers match",
  "woman" = "Gender: Woman",
  "man" = "Gender: Man"
)
for (v in c(variables_donation, variables_points, variables_belief, variables_foreign_aid_amount, "share_policies_supported")) labels_vars[paste0(v, "_agg")] <- labels_vars[v]

##### labels_vars_short_html #####
labels_vars_short_html <- c(
  "list_exp_gl" = "GCS/C/O",
  "list_exp_rgl" = "NR/GCS/C/O", 
  "list_exp_l" = "C/O",
  "list_exp_rl" = "NR/C/O",   
  "conjoint_crg_cr_binary" = "<b>C+NR+GCS</b> vs. C+NR",
  "conjoint_cr_gr_binary" = "C+NR vs. <b>GCS+NR</b>",
  "conjoint_r_rcg_binary" = "NR vs. <b>NR+C+GCS</b>",
  "conjoint_rg_r_binary" = "<b>NR+GCS</b> vs. NR",
  "conjoint_rc_r_binary" = "<b>NR+C</b> vs. NR",
  "conjoint_left_right_binary" = "<b>Left</b> vs. Right",
  "conjoint_leftg_right_binary" = "<b>Left+GCS</b> vs. Right",
  "conjoint_left_a_b_binary" = "Random program <b>A</b> vs. B",
  "conjoint_left_ag_b_binary" = "Random program <b>A+GCS</b> vs. B",
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
  "points_soc3_agg" = "Making abortion a right at the federal level",
  "points_climate1_agg" = "Coal exit",
  "points_climate2_agg" = "Trillion dollar investment in clean transportation<br>infrastructure and building insulation",
  "points_climate3_agg" = "Ban the sale of new<br>combustion-engine cars by 2030",
  "points_tax1_nr_agg" = "National redistribution scheme",
  "points_tax2_wealth_tax_agg" = "Wealth tax",
  "points_tax3_agg" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_tax3_corporate_tax_agg" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_foreign1_gcs_agg" = "Global climate scheme",
  "points_foreign2_tax_rich_agg" = "Global tax on millionaires",
  "points_foreign3_assembly_agg" = "Global democratic assembly on climate change",
  "points_foreign4_aid_agg" = "Doubling foreign aid"
)

fill_heatmaps <- function(list_var_list = NULL, heatmaps = heatmaps_defs, conditions = c("", ">= 1", "/"), sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL) {
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
    if (!"percent" %in% names(heatmaps[[name]])) heatmaps[[name]]$percent <- percent
    if (!"proportion" %in% names(heatmaps[[name]])) heatmaps[[name]]$proportion <- proportion
    if (!"nb_digits" %in% names(heatmaps[[name]])) heatmaps[[name]]$nb_digits <- nb_digits
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
  "donation" = list(vars = c("donation_nation", "donation_africa"), conditions = c(""), nb_digits = 0), # removes 'donation'
  "belief" = list(vars = variables_belief, conditions = "", nb_digits = 0), 
  "points" = list(vars = variables_points, conditions = c("", ">= 1"), nb_digits = 0),
  "foreign_aid_amount" = list(vars = variables_foreign_aid_amount, conditions = c(""), nb_digits = 1),
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
    heatmap_wrapper(vars = heatmap$vars[vars_present], data = data, labels = heatmap$labels[vars_present], name = heatmap$name, conditions = heatmap$conditions, sort = heatmap$sort, percent = heatmap$percent, proportion = heatmap$proportion, nb_digits = heatmap$nb_digits, trim = trim, weights = weights) 
  }
}


##### Barres #####
data_list_exp <- function(data) return(cbind(t(t(c(0, 0, dataN("list_exp_l", data, miss = F)))), t(t(c(0, dataN("list_exp_gl", data, miss = F)))), t(t(c(0, dataN("list_exp_rl", data, miss = F)))), dataN("list_exp_rgl", data, miss = F)))

barres_multiple <- function(barres = barres_defs, df = e, folder = NULL, print = T, export_xls = FALSE, trim = T, method = 'orca', format = 'pdf', weights = T) {
  if (missing(folder)) folder <- automatic_folder(along = "country", data = df, several = "all")
  for (def in barres) {
    tryCatch({
      vars_present <- def$vars %in% names(df)
      plot <- barres(vars = def$vars[vars_present], df = df, export_xls = export_xls, labels = def$labels[vars_present], share_labels = def$share_labels, margin_l = def$margin_l, 
                     miss = def$miss, sort = def$sort, rev = def$rev, rev_color = def$rev_color, legend = def$legend, showLegend = def$showLegend, thin = def$thin, title = def$title, weights = weights)
      if (print) print(plot)
      save_plotly(plot, filename = def$name, folder = folder, width = def$width, height = def$height, method = method, trim = trim, format = format)
      print(paste0(def$name, ": success"))
    }
  , error = function(cond) { print(paste0(def$name, ": failed.")) } )
  }
}

fill_barres <- function(list_var_list = NULL, plots = barres_defs, df = e, miss = FALSE, sort = T, thin = T, rev = FALSE, rev_color = T, 
                        short_labels = T, width = 850, labels_max_length = 57) { # width/height could be NULL by default as well, so plotly decides the size , height = dev.size('px')[2], width = dev.size('px')[1]
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
    else for (key in names(var_list)) plots[[name]][[key]] <- var_list[[key]] 
  }
  # We complete the missing fields of plots 
  for (name in names(plots)) {
    if (!"vars" %in% names(plots[[name]])) plots[[name]]$vars <- if (name %in% names(df)) name else eval(str2expression(paste0("variables_", sub("variables_", "", name))))
    if (!"name" %in% names(plots[[name]])) plots[[name]]$name <- name
    if (!"labels" %in% names(plots[[name]])) {
      plots[[name]]$labels <- c()
      for (var in plots[[name]]$vars) plots[[name]]$labels <- c(plots[[name]]$labels, break_strings(ifelse(var %in% names(labels), labels[var], var), max_length = labels_max_length))
    }
    # if (!"share_labels" %in% names(plots[[name]])) plots[[name]]$share_labels <- NA
    # if (!"margin_l" %in% names(plots[[name]])) plots[[name]]$margin_l <- NA
    if (!"miss" %in% names(plots[[name]])) plots[[name]]$miss <- miss
    if (!"sort" %in% names(plots[[name]])) plots[[name]]$sort <- sort
    if (!"rev" %in% names(plots[[name]])) plots[[name]]$rev <- rev
    if (!"rev_color" %in% names(plots[[name]])) plots[[name]]$rev_color <- rev_color
    if (!"fr" %in% names(plots[[name]])) plots[[name]]$fr <- FALSE
    if (!"title" %in% names(plots[[name]])) plots[[name]]$title <- ""
    vars_in <- plots[[name]]$vars[plots[[name]]$vars %in% names(df)]
    var_example <- vars_in[1]
    if (!"legend" %in% names(plots[[name]]) & !is.na(var_example)) plots[[name]]$legend <- dataN(var_example, data=df, miss=plots[[name]]$miss, return = "legend", fr = plots[[name]]$fr, rev = plots[[name]]$rev, rev_legend = plots[[name]]$rev)
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

##### barres_defs #####
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  "understood_each" = list(vars = variables_understood[1:3], width = 850), # 1480 
  # "problem" = list(width = 850), # 1335
  # "support_binary" = list(width = 850), # 770
  # "support_likert" = list(width = 850), # 1275
  "negotiation" = list(width = 940), # TODO! 1200
  # "group_defended" = list(width = 850), # 1250
  # "group_defended_agg" = list(width = 850), # TODO! 1150
  "foreign_aid_raise_support" = list(width = 940), # TODO! 1425
  # "global_policies" = list(width = 850), # 1275
  # "other_policies" = list(width = 850), # 1270
  # "climate_policies" = list(width = 850), # 1221
  # "variables_list_exp" = list(width = 500),
  "variables_petition" = list(vars = c("petition_gcs", "petition_nr"), width = 850), # 500
  "variables_donation" = list(vars = c("donation_africa_agg", "donation_nation_agg"), width = 850), # 835
  "foreign_aid_amount" = list(vars = variables_foreign_aid_amount_agg, width = 850), # 1080
  "belief" = list(vars = variables_belief_agg, width = 850), # 750
  "points" = list(vars = variables_points_agg, width = 850, sort = FALSE), # 750 TODO! average
  "points_us" = list(vars = variables_points_us_agg, width = 750, sort = FALSE), # 1080
  "share_policies_supported" = list(vars = "share_policies_supported_agg", width = 850), # 950
  "understood_score" = list(vars = variables_understood[4], width = 850), # 650
  # "gcs_important" = list(vars = variables_gcs_important, conditions = c("", ">= 1")),
  # "support_binary" = list(vars = variables_support_binary, conditions = ">= 1"),
  # "petition" = list(vars = variables_petition, conditions = ">= 1"),
  "conjoint" = list(vars = variables_conjoint_binary, width = 850, sort = FALSE), # 900
  # "conjoint_a" = list(vars = variables_conjoint_a_binary, conditions = ">= 1"),
  # "conjoint_b" = list(vars = variables_conjoint_b_binary, conditions = ">= 1"),
  # "conjoint_c" = list(vars = variables_conjoint_c_binary, conditions = ">= 1"),
  # "conjoint_d" = list(vars = variables_conjoint_d_binary, conditions = ">= 1"),
  # "duration" = list(vars = variables_duration, conditions = ""),
  # "foreign_aid_raise" = list(vars = variables_foreign_aid_raise, conditions = ">= 1"),
  # "foreign_aid_reduce" = list(vars = variables_foreign_aid_reduce, conditions = ">= 1"),
  "vote"= list(rev = T, miss = T, fr = "PNR/Non-voter"), # non_voters as such, aggregating candidates into 3 categories
  "vote_all"= list(rev = T), # hypothetical votes for non_voters
  "vote_agg"= list(rev = T), # hypothetical votes for non_voters, aggregating small candidates
  "vote_us"= list(rev = T), # non_voters as such
  "vote_us_voters"= list(rev = T),
  "vote_us_non_voters"= list(rev = T),
  "political_affiliation"= list(rev = T),
  "survey_biased"= list(rev = T),
  "foreign_aid_no" = list(vars = variables_foreign_aid_no[!grepl("other", variables_foreign_aid_no)], width = 850), # 1125
  "foreign_aid_condition" = list(vars = variables_foreign_aid_condition[!grepl("other", variables_foreign_aid_condition)], width = 850), # 955
  "support_match" = list(vars = c("petition_matches_support", "conjoint_a_matches_support"), width = 850) # 950
)

vars_barres <- c("other_policies", "climate_policies", "global_policies", "support_binary", "support_likert", "variables_petition", "gcs_important", "problem", 
                  "foreign_aid_raise", "foreign_aid_reduce", "foreign_aid_no", "foreign_aid_condition", "global_tax_global_share", "global_tax_sharing", "conjoint", "group_defended", "group_defended_agg",
                 "country", "urbanity", "region", "gender", "age", "age_exact", "couple", "hh_size", "income_decile", "income_quartile", "education", "diploma", "diploma_15_64", "employment_agg", "employment_status", "employment_18_64", "race", "owner", "wealth", "survey_biased", 
                 "interested_politics", "donation_charities", "involvement_govt", "left_right") 

barres_defs <- fill_barres(vars_barres, barres_defs, df = us1)
# barres_defs$foreign_aid_no


##### Run #####
# Bars
barres_multiple(barres = barres_defs[c("survey_biased")], df = us1, folder = "../figures/US1/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'

# country, urban_cateogry, region, gender, age, couple, hh_size, income, education, employment, race, owner, wealth, survey_biased, interested_politics, donation_charities, political_affiliation, involvement_govt, left_right, vote_participation, vote, duration
barres_multiple(barres = barres_defs[c("understood_each")], df = usp, folder = "../figures/USp/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'
(test <- barres(vars = c("score_understood"), rev = F, rev_color = T, export_xls = F, df = us1, sort = T, thin = T, miss=F, labels=unname(labels_vars[c("score_understood")])))
save_plotly(test, filename = "cap_wealth_support", folder = "../figures/USp/", width = NULL, height = NULL, trim = FALSE)

# list_exp
(temp <- barres(data = data_list_exp(us1), rev = F, rev_color = T, export_xls = F, sort = F, thin = T, miss=F, showLegend = T, legend = c(0:4), labels=labels_vars[variables_list_exp]))
save_plotly(temp, filename = "list_exp", folder = "../figures/US1/", width = 850, height = fig_height(4), trim = T)

# Heatmaps
heatmap_multiple() # Doesn't work if data contains a single country (by design, to avoid overwriting files)

heatmap_multiple(heatmaps_defs[c("points", "donation", "belief", "foreign_aid_amount")])

(nb_vars_heatmaps <- sort(sapply(heatmaps_defs, function(heatmap) return(setNames(length(heatmap$vars), heatmap[1]$name)))))
# Regroup heatmaps by nb of variables to change the size of the Viewer before each run and have nice saved plots
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 2]])
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 4 & nb_vars_heatmaps >= 2]])
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 9 & nb_vars_heatmaps >= 4]])
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps >= 9]])
# heatmaps_defs <- fill_heatmaps(c("conjoint_a_binary"), list())
# heatmap_multiple(heatmaps = heatmaps_defs)

# x: done; v: done for US1, waiting for EU; ~: needs to be improved; -: needs to be done
# x heatmap OECD
# v support (HEAT + ctry)
# ~ conjoint (heat + ctry + (r) + by party)
# ~ prioritization (distr + heterog distr + PLOT_ALONG)
# v list exp (TAB + heat + ctry)
# - foreign aid evolution (ctry + heterog)
# v foreign aid why (ctry)
# v petition (ctry + heterog + tab)
# v belief (CDF + ctry + heterog)
# v negotiation (ctry + heterog)
# v group defended (ctry + heterog)
# v problem (ctry + heat)

temp <- c("Aid is a pressure tactic", " for high-income countries", "Aid is xa tactic for high-income countries", " is a pressure tactic for high-inc")
(bars <- plot_ly(x = c(1,1,1,1), y = break_strings(temp),
                type = 'bar', orientation = 'h', textposition = 'auto',
                name=c("red")) %>%
    add_annotations(xref = 'paper', yref = 'y', x = c(0.5, 0.5,.5,.5)-0.01, y = break_strings(temp),
             xanchor = 'right',
             text = break_strings(temp),
             font = list(family = "Arial", size = 14+2, color = 'black', type = if (length(labels) == 2) 'bold' else ''), 
             showarrow = FALSE, align = 'right'
             )  %>%
    plotly::layout(font = list(size = 16),
                          margin = list(
                            # l = 0, r = 0, t = 0, b = 0,
                                        autoexpand = T),
                          yaxis = list(automargin = T, showticklabels = FALSE),
                          xaxis = list(
                            automargin = T,
                                       domain = c(0.5, 1)))
  )
strwrap(temp, 20)

plot_ly(x = c(1), y = "Test", type = 'bar')
