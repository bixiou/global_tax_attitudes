# TODO! petition comparable sample
# TODO! add G to OECD heatmap, remove Dependence on what other countries do, change label titles to make it clear that the first one was multiple answers while the others were likert
# TODO list_exp, all_same heatmaps, 
# TODO full results global_tax_global_share 
# TODO global_tax_support using OECD

# TODO refresh Viewer with laptop (i.e. automatic rstudioapi::executeCommand('viewerRefresh'))
# TODO automatically set share_labels/margin_l/width, miss, thin 
# TODO? Arial or Computer modern (Times)?
# TODO? barresN with also Eu

# We use weight_country for country average and weight for EU/US average.

##### labels_vars #####
labels_vars <- c(
  "(Intercept)" = "Constant",
  "finished" = "Finished",
  "excluded" = "Excluded",
  "click_reminder" = "Clicked on reminder", 
  "country" = "Country",
  "urban_category" = "Urban category",
  "region" = "Region",
  "gender" = "Gender",
  "age_exact" = "Age",
  "country_name" = "Country",
  "country_nameOther" = "Country: Other",
  "couple" = "Lives with partner",
  "hh_size" = "Household size",
  "zipcode" = "Zipcode",
  "urbanity" = "Degree of urbanization",
  "urban" = "Urban",
  "age" = "Age",
  "age_factor" = "Age",
  "Nb_children__14" = "Number of children below 14",
  "income" = "Income",
  "income_factor" = "Income quartile",
  # "education" = "Highest diploma",
  "diploma" = "Highest diploma",
  "post_secondary" = "Diploma: Post secondary",
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
  "wealth_factor" = "Wealth quintile",
  "swing_state" = "Swing State",
  "swing_state_5pp" = "Swing State (at 5%)",
  "swing_state_3pp" = "Swing State (at 3%)",
  "vote" = "Vote",
  "vote_factor" = "Vote",
  "vote3" = "Vote",
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
  "gcs_support_branch_petition_gcs" = "(Comparable) support for the GCS",
  "nr_support_branch_petition_nr" = "(Comparable) support for NR",
  "gcs_support_100" = "Support for the GCS",
  "gcs_support_neg" = "Support for the GCS",
  "nr_support" = "National redistribution scheme (NR)",
  "nr_support_100" = "Support for NR",
  "cgr_support" = "National climate policy + GCS + NR", # "Support for C+G+R",
  "gcs_belief" = "Belief about GCS",
  "nr_belief" = "Belief about NR",
  "list_exp_gl" = "List exp.: GCS/C/O",
  "list_exp_rgl" = "List exp.: NR/GCS/C/O", 
  "list_exp_l" = "List exp.: C/O",
  "list_exp_rl" = "List exp.: NR/C/O",  
  "list_exp" = "Number of supported policies", 
  "conjoint_crg_cr" = "Conjoint: C+NR+GCS vs. C+NR",
  "conjoint_cr_gr" = "Conjoint: C+NR vs. GCS+NR",
  "conjoint_r_rcg" = "Conjoint: NR vs. NR+C+GCS",
  "conjoint_rg_r" = "Conjoint: NR+GCS vs. NR",
  "conjoint_rc_r" = "Conjoint: NR+C vs. NR",
  "conjoint_left_right" = "Conjoint: Left vs. Right",
  "conjoint_leftg_right" = "Conjoint: Left+GCS vs. Right",
  "conjoint_left_a_b" = "Conjoint: random programs A vs. B",
  "conjoint_left_ag_b" = "Conjoint: random programs A+GCS vs. B",
  "conjoint_crg_cr_binary" = "C+NR+GCS preferred to C+NR", # "Conjoint: >>C+NR+GCS<< vs. C+NR",
  "conjoint_cr_gr_binary" = "GCS+NR preferred to C+NR", # "Conjoint: C+NR vs. >>GCS+NR<<",
  "conjoint_r_rcg_binary" = "NR+C+GCS preferred to NR", # "Conjoint: NR vs. >>NR+C+GCS<<",
  "conjoint_rg_r_binary" = "NR+GCS preferred to NR", # "Conjoint: >>NR+GCS<< vs. NR",
  "conjoint_rc_r_binary" = "NR+C preferred to NR", # "Conjoint: >>NR+C<< vs. NR",
  "conjoint_left_right_binary" = "Left preferred to Right", # "Conjoint: >>Left<< vs. Right",
  "conjoint_leftg_right_binary" = "Left+GCS preferred to Right", # "Conjoint: >>Left+GCS<< vs. Right",
  "conjoint_left_a_b_binary" = "Random programs: A preferred to B", # "Conjoint: random programs >>A<< vs. B",
  "conjoint_left_ag_b_binary" = "Random programs: A+GCS preferred to B", # "Conjoint: random programs >>A+GCS<< vs. B",
  "conjoint_r" = "Random programs: with GCS preferred to without",
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
  "cap_wealth_support" = "A maximum wealth limit of $10 billion (US) / €100 million (Eu) for each human",
  "attention_test" = "Attention test",
  "donation_nation" = "Donation to own country",
  "donation_africa" = "Donation to Africa",
  "donation" = "Donation (any)",
  "global_tax_support" = "Global tax on millionaires",
  "national_tax_support" = "National tax on millionaires",
  "global_tax_global_share" = "Preferred share of global tax for low-income (in %)",
  "global_tax_sharing" = "Sharing half of global tax with low-income countries",
  "foreign_aid_belief" = "Belief about foreign aid", # / public spending",
  "foreign_aid_actual" = "Actual foreign aid (in % of public spending)",
  "foreign_aid_preferred_no_info" = "Preferred foreign aid (no info)",
  "foreign_aid_preferred_info" = "Preferred foreign aid (with info)",
  "foreign_aid_preferred" = "Preferred foreign aid (any branch)",
  "foreign_aid_preferred_info_agg" = "Preferred foreign aid (with info)",
  "foreign_aid_preferred_no_info_agg" = "Preferred foreign aid (no info)",
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
  "foreign_aid_raise_support" = "[Country]'s foreign aid should be increased",
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
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already a lot to do to support the [country] people in need",
  "foreign_aid_no_other_choice" = "Other",
  "petition_gcs" = "Petition for the GCS",
  "petition_nr" = "Petition for NR",
  "petition" = "Petition (any)",
  "negotiation_country_respecting" = "Favors [Country]'s interests restrained by global justice",
  "negotiation_global_before" = "Favors global justice, restrained or not by [Country]'s interests",
  "negotiation_only_country" = "Favors [Country]'s interests even against global justice",
  "donation_charities" = "Donation to charities",
  "interested_politics" = "Interested in politics",
  "group_defended" = "Group defended when voting",
  "group_defended_agg" = "Group defended when voting",
  "group_defended_agg2" = "Group defended when voting",
  "group_defended_agg5" = "Group defended when voting",
  "group_defended_agg6" = "Group defended when voting",
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
  "problem_inequality" = "Income inequality in [Country] is a problem",
  "problem_climate" = "Climate change is a problem",
  "problem_poverty" = "Global poverty is a problem",
  "points_econ1" = "econ1",
  "points_econ2" = "econ2: [Higher minimum wage] (DE: Bürgerversicherung)",
  "points_econ3" = "econ3",
  "points_econ4" = "econ4",
  "points_soc1" = "soc1",
  "points_soc2" = "soc2",
  "points_soc3" = "Making abortion a right at the federal level",
  "points_climate1" = "climate1",
  "points_climate2" = "climate2: Thermal insulation plan (US: also transport)",
  "points_climate3" = "climate3: Ban the sale of new combustion-engine cars by 2030",
  "points_tax1_nr" = "tax1: National redistribution scheme",
  "points_tax2_wealth_tax" = "tax2: Wealth tax (ES: raise tax on top incomes)",
  "points_tax3" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_tax3_corporate_tax" = "Increase corporate income tax<br>rate from 21% to 28%",
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
  "duration_agg" = "Duration",
  "duration_gcs" = "Duration: GCS comprehension",
  "duration_nr" = "Duration: NR comprehension",
  "duration_both" = "Duration: GCS+NR comprehension",
  # "duration_gcs" = "Duration: GCS questions",
  "duration_conjoint_a" = "Duration: conjoint (a)",
  "duration_conjoint_b" = "Duration: conjoint (b)",
  "duration_conjoint_c" = "Duration: conjoint (c)",
  "duration_conjoint_d" = "Duration: conjoint (d)",
  "duration_gcs_perception" = "Duration: G perceptions",
  "duration_other_policies" = "Duration: other policies",
  "duration_feedback" = "Duration: feedback",
  "duration_points" = "Duration: 100 points",
  "score_understood" = "Number of correct answers to understanding questions",
  "gcs_understood" = "With GCS, typical [country] people lose and poorest humans win",
  "nr_understood" = "With NR, typical [country] people win and richest lose",
  "both_understood" = "With GCS+NR, typical [country] people neither win nor lose",
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "petition_matches_support" = "Petition and support answers match",
  "conjoint_a_matches_support" = "Conjoint (a) and support answers match",
  "nationalist" = "Nationalist",
  "universalist" = "Universalist",
  "individualist" = "Individualist",
  "egoistic" = "Individualist",
  "woman" = "Gender: Woman",
  "man" = "Gender: Man",
  "ets2_equal_cash_support" = "ETS2 with equal cash transfer (105€/year for each European)",
  "ets2_country_cash_support" = "ETS2 with cash transfer in proportion to country's emissions",
  "ets2_investments_support" = "ETS2 with low-carbon investments",
  "ets2_vulnerable_investments_support" = "ETS2 with transfers to vulnerable and low-carbon investments",
  "ets2_no_european" = "Policies should be at national level", # Does not support ETS2 because<br>
  "ets2_no_revenue_use" = "Would prefer other revenue use",
  "ets2_no_pricing" = "Opposes carbon pricing",
  "ets2_no_climate_action" = "Opposes more climate action",
  "ets2_no_understanding" = "Does not understand",
  "ets2_no_dont_know" = "Does not know",
  "ets2_oppose" = "Does not support any of the ETS2 variants",
  "global_tax_more_half" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 50%",
  "global_tax_more_30p" = "Preferred share of global wealth tax for low-income countries: ≥ 30%",
  "global_tax_more_10p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 10%",
  "branch_list_exp_g" = "List contains: GCS",
  "branch_list_exp_r" = "List contains: NR",
  "branch_list_exp_g:branch_list_exp_r" = "List contains: GCS $\\times$ NR",
  "branch_list_exp_gTRUE:branch_list_exp_rTRUE" = "List contains: GCS $\\times$ NR",
  "branch_c_gcs" = "GCS in Progressive platform",
  "branch_donationOwn nation" = "Poor is in own country",
  "vote_factorPNR/Non-voter" = "Vote: PNR/Non-voter",
  "branch_donationOwn nation:vote_factorCenter-right or Right" = "Poor in own country $\\times$ Vote: Center-right or Right",
  "branch_donationOwn nation:vote_factorLeft" = "Poor in own country $\\times$ Vote: Left",
  "branch_donationOwn nation:vote_factorPNR/Non-voter" = "Poor in own country $\\times$ Vote: PNR/Non-voter",
  "branch_donationOwn nation:vote_factorFar right" = "Poor in own country $\\times$ Vote: Far right",
  "branch_gcs" = "Treatment",
  "branch_gcsfield" = "Treatment: Open-ended field on GCS pros & cons",
  "branch_gcsimportant" = "Treatment: Closed questions on GCS pros & cons", 
  "branch_gcsinfo" = "Treatment: Info on actual support for GCS and NR",
  "(Intercept)" = "Constant"
)
for (v in c(variables_gcs_field_names, variables_poverty_field_names, "gcs_field_empty", "poverty_field_empty")) labels_vars[v] <-sub("dont", "don't", gsub("_", " ", gsub(".*_field_", "", v)))
for (v in c(variables_gcs_field_contains, variables_poverty_field_contains)) labels_vars[v] <-  paste0(gsub(".*_", "", v), ": ", gsub(".*contains: ", "", Label(all[[v]])))
for (v in c(variables_donation, variables_points, variables_belief, variables_foreign_aid_amount, "share_policies_supported")) labels_vars[paste0(v, "_agg")] <- labels_vars[v]
for (v in intersect(names(all), names(labels_vars))) { # intersect(c(socio_demos, socio_demos_us), names(all)), 
  if (grepl("TRUE / FALSE", Levels(all[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(all[[v]]), NA)) {
    if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
  }
}

  
##### labels_vars_short_html #####
labels_vars_short_html <- c(
  "score_understood" = "Number of correct answers<br>to understanding questions",
  "gcs_understood" = "With GCS,<br>typical [country] people lose and poorest humans win",
  "nr_understood" = "With NR,<br>typical [country] people win and richest lose",
  "both_understood" = "With GCS+NR,<br>typical [country] people neither win nor lose",
  "list_exp_gl" = "GCS/C/O",
  "list_exp_rgl" = "NR/GCS/C/O", 
  "list_exp_l" = "C/O",
  "list_exp_rl" = "NR/C/O",   
  "global_tax_more_30p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 30%",
  "conjoint_crg_cr_binary" = "<b>C+NR+GCS</b> vs. C+NR",
  "conjoint_cr_gr_binary" = "C+NR vs. <b>GCS+NR</b>",
  "conjoint_r_rcg_binary" = "NR vs. <b>NR+C+GCS</b>",
  "conjoint_rg_r_binary" = "<b>NR+GCS</b> vs. NR",
  "conjoint_rc_r_binary" = "<b>NR+C</b> vs. NR",
  "conjoint_left_right_binary" = "<b>Left</b> vs. Right",
  "conjoint_leftg_right_binary" = "<b>Left+GCS</b> vs. Right",
  "conjoint_left_a_b_binary" = "Random program <b>A</b> vs. B",
  "conjoint_left_ag_b_binary" = "Random program <b>A+GCS</b> vs. B",
  "conjoint_r" = "Random program <b>with GCS</b> vs. without",
  "foreign_aid_raise_support" = "Should [Country]'s foreign aid be increased?",
  "problem_inequality" = "Income inequality in [Country]",
  "problem_climate" = "Climate change",
  "problem_poverty" = "Global poverty",
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
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already<br>a lot to do to support the [country] people in need",
  "ets2_equal_cash_support" = "ETS2 with equal cash transfer<br>(105€/year for each European)",
  "ets2_country_cash_support" = "ETS2 with cash transfer<br>in proportion to country's emissions",
  "ets2_investments_support" = "ETS2 with low-carbon investments",
  "ets2_vulnerable_investments_support" = "ETS2 with transfers to vulnerable<br>and low-carbon investments",
  "ets2_no_european" = "Policies should be at national level",
  "ets2_no_revenue_use" = "Would prefer other revenue use",
  "ets2_no_pricing" = "Opposes carbon pricing",
  "ets2_no_climate_action" = "Opposes more climate action",
  "ets2_no_understanding" = "Does not understand",
  "ets2_no_dont_know" = "Does not know"
)

##### labels_vars_country #####
labels_vars_country <- list() #"US" = c(), "DE" = c(), "FR" = c(), "ES" = c(), "UK" = c())
for (c in countries) {
  names_policies_names <- paste0("points_", row.names(policies.names))
  for (v in intersect(c(variables_points), names_policies_names)) {
    labels_vars_country[[c]][v] <- policies.names[sub("points_", "", v), c]
    labels_vars_country[[c]][paste0(v, "_agg")] <- policies.names[sub("points_", "", v), c] # common ones in English
  }
  for (v in setdiff(names_policies_names[grepl("[0-9]", names_policies_names)], variables_points)) { # common ones in local language
    var <- variables_points[grepl(v, variables_points)]
    if (length(var) == 1) labels_vars_country[[c]][paste0(var, "_agg")] <- policies.names[sub("points_", "", v), c]
  }
}
labels_vars_country$US["foreign_aid_no_nation_first"] <- "Charity begins at home: there is already a lot to do to support the American people in need"

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
      for (var in heatmaps[[name]]$vars) heatmaps[[name]]$labels <- c(heatmaps[[name]]$labels, break_strings(ifelse(var %in% names(labels_vars), labels_vars[var], var), sep = "\n"))
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
  "conjoint_r" = list(vars = "conjoint_r"), 
  "ets2_oppose" = list(vars = "ets2_oppose", conditions = ">= 1"),
  "conjoint_left_ag_b_binary" = list(vars = "conjoint_left_ag_b_binary", conditions = c(">= 1")), 
  "global_tax_global_share" = list(vars = c("global_tax_global_share"), conditions = c("", ">= 1"), nb_digits = 0),
  "global_tax_sharing" = list(vars = c("global_tax_sharing"), conditions = c(">= 1")),
  "list_exp" = list(vars = variables_list_exp, conditions = c("")),
  "understood_all" = list(vars = variables_understood, conditions = c("")),
  "understood_each" = list(vars = variables_understood[1:3], conditions = c(">= 1")),
  "understood_score" = list(vars = variables_understood[4], conditions = c("")),
  "gcs_important" = list(vars = variables_gcs_important, conditions = c("", ">= 1")),
  "support_binary" = list(vars = variables_support_binary, conditions = ">= 1"),
  "gcs_field_contains" = list(vars = variables_gcs_field_contains[1:10], conditions = ">= 1", sort = T),
  "gcs_field" = list(vars = c(variables_gcs_field_names, "gcs_field_empty"), conditions = ">= 1", sort = T),
  "poverty_field_contains" = list(vars = variables_poverty_field_contains[1:9], conditions = ">= 1", sort = T),
  "poverty_field" = list(vars = c(variables_poverty_field_names, "poverty_field_empty"), conditions = ">= 1", sort = T),
  "petition_only" = list(vars = variables_petition[1:2], conditions = ">= 1"),
  "petition" = list(vars = c("petition_gcs", "gcs_support", "petition_nr", "nr_support"), conditions = ">= 1"),
  "petition_gcs" = list(vars = c("petition_gcs", "gcs_support"), conditions = ">= 1"),
  "petition_nr" = list(vars = c("petition_nr", "nr_support"), conditions = ">= 1"),
  "petition" = list(vars = c("petition_gcs", "gcs_support", "petition_nr", "nr_support"), conditions = ">= 1"),
  "petition_comparable" = list(vars = c("petition_gcs", "gcs_support_branch_petition_gcs", "petition_nr", "nr_support_branch_petition_nr"), conditions = ">= 1"),
  "conjoint_all" = list(vars = c("gcs_support", variables_conjoint_a_binary, variables_conjoint_b_binary, variables_conjoint_c_binary, "conjoint_left_ag_b_binary", "conjoint_r"), conditions = ">= 1"),
  "conjoint" = list(vars = c("gcs_support", variables_conjoint_a_binary, variables_conjoint_b_binary, "conjoint_left_ag_b_binary", "conjoint_r"), conditions = ">= 1"), 
  "conjoint_ab" = list(vars = c("gcs_support", "conjoint_rg_r_binary", "conjoint_crg_cr_binary", "conjoint_rc_r_binary", "conjoint_cr_gr_binary"), conditions = ">= 1"),
  "conjoint_ab_all" = list(vars = c("gcs_support", "conjoint_crg_cr_binary", "conjoint_rg_r_binary", "conjoint_rc_r_binary", "conjoint_cr_gr_binary", "conjoint_r_rcg_binary"), conditions = ">= 1"), 
  "conjoint_a" = list(vars = variables_conjoint_a_binary, conditions = ">= 1"),
  "conjoint_b" = list(vars = variables_conjoint_b_binary, conditions = ">= 1"),
  "conjoint_c" = list(vars = variables_conjoint_c_binary, conditions = ">= 1"),
  "conjoint_d" = list(vars = variables_conjoint_d_binary, conditions = ">= 1"),
  "duration" = list(vars = variables_duration, conditions = ""),
  "donation" = list(vars = c("donation_nation", "donation_africa"), conditions = c(""), nb_digits = 0), # removes 'donation'
  "belief" = list(vars = variables_belief, conditions = "", nb_digits = 0), 
  "belief_all" = list(vars = c("gcs_belief", "gcs_support_100", "nr_belief", "nr_support_100"), conditions = "", nb_digits = 0), 
  "points" = list(vars = variables_points, conditions = c("", ">= 1"), nb_digits = 0),
  "foreign_aid_amount" = list(vars = c("foreign_aid_actual", "foreign_aid_belief", "foreign_aid_preferred_info", "foreign_aid_preferred_no_info"), conditions = c("", "median"), nb_digits = 1),
  "foreign_aid_more" = list(vars = c("foreign_aid_more_less_info", "foreign_aid_more_less_no_info", "foreign_aid_raise_support"), conditions = c("> 0"), 
              labels = c("Preferred foreign aid is higher than current", "Preferred foreign aid is higher than perceived", "Supports increasing foreign aid (incl. with conditions)")),
  "foreign_aid_no_less" = list(vars = c("foreign_aid_no_less_info", "foreign_aid_no_less_no_info", "foreign_aid_raise_support"), conditions = c("> 0"), 
              labels = c("Preferred foreign aid is at least as high as current", "Preferred foreign aid is at least as high as perceived", "Supports increasing foreign aid (incl. with conditions)")),
  "foreign_aid_more_all" = list(vars = c("foreign_aid_more_less_info", "foreign_aid_less_more_info", "foreign_aid_more_less_no_info", "foreign_aid_less_more_no_info", "foreign_aid_raise_support", "foreign_aid_reduce_support"), conditions = c("> 0"), 
              labels = c("Preferred foreign aid is higher than current", "Preferred foreign aid is lower than current", "Preferred foreign aid is higher than perceived", "Preferred foreign aid is lower than perceived", "Supports increased foreign aid (incl. with conditions)", "Supports reduced foreign aid")),
  "foreign_aid_raise" = list(vars = variables_foreign_aid_raise, conditions = ">= 1"),
  "foreign_aid_reduce" = list(vars = variables_foreign_aid_reduce, conditions = ">= 1"),
  "foreign_aid_no" = list(vars = variables_foreign_aid_no[!grepl("other", variables_foreign_aid_no)]),
  "foreign_aid_condition" = list(vars = variables_foreign_aid_condition[!grepl("other", variables_foreign_aid_condition)]),
  "share_policies_supported" = list(vars = c("share_policies_supported"), conditions = c("")),
  "support_match" = list(vars = c("petition_matches_support", "conjoint_a_matches_support"), conditions = c(">= 1")),
  "universalism" = list(vars = c("universalist", "nationalist", "egoistic", "negotiation_only_country", "negotiation_country_respecting", "negotiation_global_before", "problem_climate", "problem_poverty", "problem_inequality"), conditions = c(">= 1")),
  "main" = list(vars = c("gcs_support_neg", "global_tax_support", "cap_wealth_support", "climate_mitigation_support", "foreign_aid_raise_support", "universalist"), conditions = c("/")),
  "main_all" = list(vars = c("gcs_support_neg", "global_tax_support", "global_tax_sharing", "cap_wealth_support", "climate_mitigation_support", "foreign_aid_raise_support", "universalist"), conditions = c("/"))
)

##### vars_heatmaps #####
vars_heatmaps <- c("support", "other_policies", "climate_policies", "global_policies", "support_binary", "support_likert", "petition", "gcs_important", "problem", 
                        "foreign_aid_amount", "duration", "donation", "belief", "points", "foreign_aid_raise", "foreign_aid_reduce", "foreign_aid_no", "foreign_aid_condition", 
                        "conjoint", "conjoint_a", "conjoint_b", "conjoint_c", "conjoint_d", "list_exp", "understood", "ets2_support", "ets2_no") # misses socio-demos, politics

heatmaps_defs <- fill_heatmaps(vars_heatmaps, heatmaps_defs)
# heatmaps_defs$foreign_aid_no

heatmap_multiple <- function(heatmaps = heatmaps_defs, data = e, trim = FALSE, weights = T, folder = NULL, name = NULL) {
  for (heatmap in heatmaps) {
    vars_present <- heatmap$vars %in% names(data)
    if (any(c("gcs_support", "nr_support", "gcs_support_100") %in% heatmap$vars)) data <- data[data$wave != "US2",]
    heatmap_wrapper(vars = heatmap$vars[vars_present], special = "Europe", data = data, labels = heatmap$labels[vars_present], name = if (is.null(name)) heatmap$name else name, conditions = heatmap$conditions, sort = heatmap$sort, 
                    percent = heatmap$percent, proportion = heatmap$proportion, nb_digits = heatmap$nb_digits, trim = trim, weights = weights, folder = folder)   
  }
}


##### Barres #####
data_list_exp <- function(data) return(cbind(t(t(c(0, 0, dataN("list_exp_l", data, miss = F)))), t(t(c(0, dataN("list_exp_gl", data, miss = F)))), t(t(c(0, dataN("list_exp_rl", data, miss = F)))), dataN("list_exp_rgl", data, miss = F)))

barres_multiple <- function(barres = barres_defs, df = e, folder = NULL, print = T, export_xls = FALSE, trim = T, method = 'orca', format = 'pdf', weights = T) {
  if (missing(folder)) folder <- automatic_folder(along = "country", data = df, several = "all")
  for (def in barres) {
    tryCatch({
      vars_present <- def$vars %in% names(df)
      if (!"along" %in% names(def)) plot <- barres(vars = def$vars[vars_present], df = df, export_xls = export_xls, labels = def$labels[vars_present], share_labels = def$share_labels, margin_l = def$margin_l, add_means = def$add_means, show_legend_means = def$show_legend_means, transform_mean = def$transform_mean,
                                                 miss = def$miss, sort = def$sort, rev = def$rev, rev_color = def$rev_color, legend = def$legend, showLegend = def$showLegend, thin = def$thin, title = def$title, weights = weights)
      else plot <- barresN(vars = def$vars[vars_present], df = df, along = def$along, export_xls = export_xls, labels = def$labels[vars_present], share_labels = def$share_labels, margin_l = def$margin_l,
                           miss = def$miss, sort = def$sort, rev = def$rev, rev_color = def$rev_color, legend = def$legend, showLegend = def$showLegend, thin = def$thin, weights = weights)
      if (print) print(plot)
      save_plotly(plot, filename = def$name, folder = folder, width = def$width, height = def$height, method = method, trim = trim, format = format)
      print(paste0(def$name, ": success"))
    }
  , error = function(cond) { print(paste0(def$name, ": failed.")) } )
  }
}

fill_barres <- function(list_var_list = NULL, plots = barres_defs, df = e, country = NULL, miss = FALSE, sort = T, thin = T, rev = FALSE, rev_color = T, along = NULL,
                        short_labels = T, width = 850, labels_max_length = 57) { # width/height could be NULL by default as well, so plotly decides the size , height = dev.size('px')[2], width = dev.size('px')[1]
  # list_var_list can be NULL, a named list of vectors of variables, a named list of type plots_defs, or a list of names of (existing) vectors of variables (with or without the prefix 'variables_')
  # If df$var and variables_var both exist, giving 'var' (resp. 'variables_var') will yield var (resp. variables_var)
  # /!\ Bug if an object named 'plots' exists in the environment.
  if (!exists("labels_vars")) warning("'labels_vars' should exist but does not.")
  labels <- labels_vars
  if (exists("labels_vars_short_html") & short_labels) labels[names(labels_vars_short_html)] <- labels_vars_short_html
  # if (grepl("us", deparse(substitute(df)))) labels[names(labels_vars_us)] <- labels_vars_us
  # c <- if (deparse(substitute(df)) != "e") gsub("[0-9p]*", "",  deparse(substitute(df))) else if (length(unique(df$country)) == 1) unique(df$country)[1] else NULL
  c <- if (length(unique(df$country)) == 1) unique(df$country)[1] else NULL
  if (!is.null(c)) labels[names(labels_vars_country[[c]])] <- labels_vars_country[[c]]
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
    if (!"vars" %in% names(plots[[name]])) plots[[name]]$vars <- if (name %in% names(df)) name else if (exists(paste0("variables_", sub("variables_", "", name)))) eval(str2expression(paste0("variables_", sub("variables_", "", name)))) else warning(paste(name, "not found"))
    if (!"name" %in% names(plots[[name]])) plots[[name]]$name <- name
    if (!"labels" %in% names(plots[[name]])) {
      plots[[name]]$labels <- c()
      for (var in plots[[name]]$vars) plots[[name]]$labels <- c(plots[[name]]$labels, break_strings(ifelse(var %in% names(labels), labels[var], var), max_length = labels_max_length))
    }
    # if (!"share_labels" %in% names(plots[[name]])) plots[[name]]$share_labels <- NA
    # if (!"margin_l" %in% names(plots[[name]])) plots[[name]]$margin_l <- NA
    if (!"miss" %in% names(plots[[name]])) plots[[name]]$miss <- miss
    if (!"sort" %in% names(plots[[name]])) plots[[name]]$sort <- sort
    if (!"along" %in% names(plots[[name]])) plots[[name]]$along <- along
    if (!"rev" %in% names(plots[[name]])) plots[[name]]$rev <- rev
    if (!"rev_color" %in% names(plots[[name]])) plots[[name]]$rev_color <- rev_color
    if (!"fr" %in% names(plots[[name]])) plots[[name]]$fr <- FALSE
    if (!"title" %in% names(plots[[name]])) plots[[name]]$title <- ""
    vars_in <- plots[[name]]$vars[plots[[name]]$vars %in% names(df)]
    var_example <- vars_in[1]
    if (!"legend" %in% names(plots[[name]]) & !is.na(var_example)) plots[[name]]$legend <- dataKN(vars = vars_in, data=df, miss=plots[[name]]$miss, return = "legend", fr = plots[[name]]$fr, rev = plots[[name]]$rev, rev_legend = plots[[name]]$rev)
    # yes_no <- setequal(plots[[name]]$legend, c('Yes', 'No', 'PNR')) | setequal(plots[[name]]$legend, c('Oui', 'Non', 'NSP')) | setequal(plots[[name]]$legend, c('Yes', 'No')) | setequal(plots[[name]]$legend, c('Oui', 'Non'))
    # if (!"showLegend" %in% names(plots[[name]])) plots[[name]]$showLegend <- if (is.na(var_example))  T else (!is.binary(df[[var_example]]) | yes_no)
    if (!"showLegend" %in% names(plots[[name]])) plots[[name]]$showLegend <- if (is.na(var_example)) T else (!is.logical(df[[var_example]]))
    if (!"thin" %in% names(plots[[name]])) plots[[name]]$thin <- thin #& !yes_no
    if (!"width" %in% names(plots[[name]])) plots[[name]]$width <- width
    if (!"height" %in% names(plots[[name]]) & "heigth" %in% names(plots[[name]])) plots[[name]]$height <- plots[[name]]$heigth
    if (!"height" %in% names(plots[[name]])) plots[[name]]$height <- fig_height(nb_bars = if (!is.null(along)) length(Levels(df[[along]])) else length(plots[[name]]$labels), large = any(grepl("<br>", plots[[name]]$labels))) # height
  }
  return(plots)
}


##### barres_defs #####
# define_barres_defs <- function(df = e) {
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  "understood_each" = list(vars = variables_understood[1:3], width = 850), # 1480 
  # "problem" = list(width = 850), # 1335
  # "support_binary" = list(width = 850), # 770
  # "support_likert" = list(width = 850), # 1275
  "negotiation" = list(width = 940), # TODO! 1200
  "group_defended" = list(width = 1000), # 1250
  "group_defended_agg" = list(width = 900), # TODO! 1150
  "foreign_aid_raise_support" = list(width = 940), # TODO! 1425
  # "global_policies" = list(width = 850), # 1275
  # "other_policies" = list(width = 850), # 1270
  # "climate_policies" = list(width = 850), # 1221
  # "variables_list_exp" = list(width = 500),
  "variables_petition" = list(vars = c("petition_gcs", "petition_nr"), width = 850), # 500
  "variables_donation" = list(vars = c("donation_africa_agg", "donation_nation_agg"), width = 850), # 835
  "foreign_aid_amount" = list(vars = variables_foreign_aid_amount_agg[1:3], width = 850), # 1080
  "belief" = list(vars = variables_belief_agg, width = 850), # 750
  # "points" = list(vars = variables_points_agg, width = 850, sort = FALSE), # 750 TODO! average
  "points_mean" = list(vars = variables_points_us_agg, width = 850, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)), # 1080 points_us
  "points" = list(vars = variables_points_us_agg, width = 850, sort = FALSE), # 1080 points_us
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
  "support_binary_all" = list(showLegend = FALSE), 
  "global_national_tax" = list(vars = c("national_tax_support", "global_tax_support"), sort = FALSE),
  "global_tax_share" = list(vars = c("global_tax_sharing", "global_tax_more_half", "global_tax_more_30p", "global_tax_more_10p"), sort = FALSE), # TODO make it also a heatmap
  "vote"= list(miss = T, fr = "PNR/Non-voter"), # non_voters as such, aggregating candidates into 3 categories
  "vote_all"= list(rev = T), # hypothetical votes for non_voters
  "vote_agg"= list(rev = T), # hypothetical votes for non_voters, aggregating small candidates
  "vote3"= list(rev = T), # non_voters as such, pooled with PNR and those voting for small candidates
  "vote_us"= list(rev = T), # non_voters as such
  "vote_us_voters"= list(rev = T),
  "vote_us_non_voters"= list(rev = T),
  "political_affiliation"= list(rev = T),
  "survey_biased"= list(rev = T),
  "foreign_aid_no" = list(vars = variables_foreign_aid_no[!grepl("other", variables_foreign_aid_no)], width = 850), # 1125
  "foreign_aid_condition" = list(vars = variables_foreign_aid_condition[!grepl("other", variables_foreign_aid_condition)], width = 850), # 955
  "support_match" = list(vars = c("petition_matches_support", "conjoint_a_matches_support"), width = 850) # 950
)

vars_barres <- c("ets2_support", "ets2_no", "other_policies", "climate_policies", "global_policies", "support_binary", "support_likert", "variables_petition", 
                 "gcs_important", "problem", "foreign_aid_raise", "foreign_aid_reduce", "foreign_aid_no", "foreign_aid_condition", "global_tax_global_share", 
                 "global_tax_sharing", "global_tax_support", "national_tax_support", "conjoint", "group_defended", "group_defended_agg", "group_defended_agg2", 
                 "group_defended_agg5", "group_defended_agg6", "country_name", "urbanity", "region", "gender", "age", "age_exact", "couple", "hh_size", "income_decile", 
                 "income_quartile", "education", "diploma", "diploma_25_64", "employment_agg", "employment_status", "employment_18_64", "race", "owner", "wealth", "survey_biased", 
                 "vote_agg", "vote_participation", "interested_politics", "donation_charities", "involvement_govt", "left_right", "duration_agg") 

barres_defs <- fill_barres(vars_barres, barres_defs) # , df = us1
# return(barres_defs) }
# barres_defs$foreign_aid_no

vars_barresN <- c("group_defended_agg2", "foreign_aid_raise_support", "global_tax_support", "national_tax_support", "global_tax_global_share", "global_tax_sharing",
                  "foreign_aid_belief_agg", "foreign_aid_preferred_info_agg", "foreign_aid_preferred_no_info_agg", "donation_charities", "interested_politics", 
                  "involvement_govt",  "vote_participation", "survey_biased", "interview", "left_right") 
barresN_defs <- fill_barres(vars_barresN, list("negotiation" = list(width = 940), "vote" = list(miss = T)), along = "country_name")
barresN_continent_defs <- fill_barres(vars_barresN, list("negotiation" = list(width = 940), "vote" = list(miss = T)), along = "continent")
main_outcomes <- c("gcs_support", "nr_support", "global_tax_support", "national_tax_support", "cap_wealth_support", "group_defended_agg2", "negotiation", "democratise_un_imf_support", "climate_mitigation_support")
barresN_vote3_defs <- fill_barres(main_outcomes, list(), along = "vote3")
barresN_vote_defs <- fill_barres(c(main_outcomes, "foreign_aid_raise_support"), list(), along = "vote_factor")
barresN_age_defs <- fill_barres(c(main_outcomes, "foreign_aid_raise_support"), list(), along = "age_factor")
barresN_income_defs <- fill_barres(c(main_outcomes, "foreign_aid_raise_support"), list(), along = "income_character")

barres_multiple(barres = barresN_defs[c("global_tax_support")], df = all, folder = "../figures/country_comparison/")
barres_multiple(barres = barresN_continent_defs[c("vote")], df = all, folder = "../figures/continents/")


##### Run #####
# Bars
barres_multiple(barres = barres_defs[c("points_mean", "points")], df = us1, folder = "../figures/US1/") 
barres_multiple(barres = barres_defs[c("group_defended")], df = us1, folder = "../figures/US1/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'

barres_multiple(barres = barres_defs[c("income_quartile")], df = eu, folder = "../figures/USp/") # , folder = NULL, export_xls = T, trim = FALSE, method = 'orca', format = 'pdf'
(test <- barres(vars = c("score_understood"), rev = F, rev_color = T, export_xls = F, df = us1, sort = T, thin = T, miss=F, labels=unname(labels_vars[c("score_understood")])))
save_plotly(test, filename = "cap_wealth_support", folder = "../figures/USp/", width = NULL, height = NULL, trim = FALSE)

# barresN_defs <- fill_barres(c("group_defended_agg2"), list("negotiation" = list(width = 940)), along = "country_name")
barres_multiple(barres = barresN_continent_defs["foreign_aid_raise_support"], df = all, folder = "../figures/continents/") 

# Heatmaps
heatmap_multiple() # Doesn't work if data contains a single country (by design, to avoid overwriting files)
# US2
heatmap_multiple(heatmaps_defs[c("foreign_aid_amount", "foreign_aid_more")])
heatmap_multiple(heatmaps_defs[c("petition_comparable")])
heatmap_multiple(heatmaps_defs[c("foreign_aid_more_all")])
heatmap_multiple(heatmaps_defs[c("global_tax_global_share", "global_tax_sharing")])

heatmap_multiple(heatmaps_defs[c("foreign_aid_no", "foreign_aid_condition")], weights = T)
heatmap_multiple(heatmaps_defs[c("petition", "foreign_aid_amount", "foreign_aid_more")], weights = T)
heatmap_multiple(heatmaps_defs[c("petition_only", "petition_gcs", "petition_nr", "global_tax_global_share")], weights = T)

# points TODO: define e
(plot_points <- barres(vars = variables_points_us_agg[variables_points_us_agg %in% names(e)], df = e, export_xls = F, labels = barres_defs[["points"]]$labels[variables_points_us_agg %in% names(e)], sort = FALSE, weights = T, miss = FALSE, rev_color = T, rev = FALSE, add_means = T, transform_mean = function(x) return(x/100)))
plot_points %>% layout(shapes = list(x0 = 0.167, x1 = 0.167, line = list(color = "grey"), type = "line", y0 = 0, y1 = 1, yref = "paper"))
save_plotly(plot_points, filename = "points_line", folder = automatic_folder(along = "country", data = e, several = "all"), width = 850, height = fig_height(length(which(variables_points_us_agg %in% names(e)),  any(grepl("<br>", barres_defs[["points"]]$labels)))), trim = T, format = 'pdf')


##### Bars #####
barres_multiple(barres = barres_defs, df = eu, folder = "../figures/EU/") 
barres_multiple(barres = barres_defs, df = us1, folder = "../figures/US1/") 
barres_multiple(barres = barres_defs, df = us2, folder = "../figures/US2/") 
barres_multiple(barres = barres_defs, df = us, folder = "../figures/US/") 
barres_multiple(barres = barres_defs, df = eu[eu$country == 'FR',], folder = "../figures/FR/") 
barres_multiple(barres = barres_defs, df = eu[eu$country == 'DE',], folder = "../figures/DE/") 
barres_multiple(barres = barres_defs, df = eu[eu$country == 'ES',], folder = "../figures/ES/") 
barres_multiple(barres = barres_defs, df = eu[eu$country == 'UK',], folder = "../figures/UK/") 
barres_multiple(barres = barres_defs, df = all, folder = "../figures/all/") 

# list_exp
(temp <- barres(data = data_list_exp(us1), rev = F, rev_color = T, export_xls = F, sort = F, thin = T, miss=F, showLegend = T, legend = c(0:4), labels=labels_vars[variables_list_exp]))
save_plotly(temp, filename = "list_exp", folder = "../figures/US1/", width = 850, height = fig_height(4), trim = T)
(temp <- barres(data = data_list_exp(eu), rev = F, rev_color = T, export_xls = F, sort = F, thin = T, miss=F, showLegend = T, legend = c(0:4), labels=labels_vars[variables_list_exp]))
save_plotly(temp, filename = "list_exp", folder = "../figures/EU/", width = 850, height = fig_height(4), trim = T)
for (c in countries_EU) {
  (temp <- barres(data = data_list_exp(d(c)), rev = F, rev_color = T, export_xls = F, sort = F, thin = T, miss=F, showLegend = T, legend = c(0:4), labels=labels_vars[variables_list_exp]))
  save_plotly(temp, filename = "list_exp", folder = paste0("../figures/", c, "/"), width = 850, height = fig_height(4), trim = T)
}

# Handle exceptions
for (c in countries) {
  e <- d(c)
  barres_multiple(barres = fill_barres(c("vote_agg", "points_agg"), 
            list("points_mean" = list(vars = variables_points_us_agg, width = 850, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)), # 1080 points_us
                 "points" = list(vars = variables_points_us_agg, width = 850, sort = FALSE) 
  )), df = d(c), folder = paste0("../figures/", c, "/")) 
}
e <- d("ES")
barres_multiple(barres = fill_barres(c("global_tax_global_share"), list()), df = d("ES"), folder = paste0("../figures/ES/")) 
e <- all


##### Heatmaps #####
# TODO define e
nb_vars_heatmaps <- sort(sapply(heatmaps_defs, function(heatmap) return(setNames(length(heatmap$vars), heatmap[1]$name))))
(nb_vars_heatmaps <- nb_vars_heatmaps[!grepl("ets2", names(nb_vars_heatmaps))])
# Regroup heatmaps by nb of variables to change the size of the Viewer before each run and have nice saved plots
heatmap_multiple(heatmaps_defs[c("foreign_aid_more")], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 2]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps == 2][1]], weights = T, trim = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps == 2]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps == 3][1]], weights = T, trim = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps == 3]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 5 & nb_vars_heatmaps >= 4][1]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 5 & nb_vars_heatmaps >= 4]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 8 & nb_vars_heatmaps >= 5][1]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps < 8 & nb_vars_heatmaps >= 5]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps >= 8][1]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps >= 8 & nb_vars_heatmaps < 14]], weights = T)
heatmap_multiple(heatmaps_defs[names(nb_vars_heatmaps)[nb_vars_heatmaps >= 14]], weights = T)
heatmap_multiple(heatmaps_defs[c("petition", "belief_all")], weights = T)
heatmap_multiple(heatmaps_defs[c("petition_gcs", "petition_nr")], weights = T)
heatmap_multiple(heatmaps_defs[c("conjoint_all", "conjoint")], weights = T) # TODO! crop conjoint_ab, conjoint_ab_all, support binary, etc.
heatmap_multiple(heatmaps_defs[c("gcs_field")], weights = T) 
heatmap_multiple(heatmaps_defs[c("gcs_field_contains", "poverty_field_contains", "poverty_field")], weights = T) 
heatmap_multiple(heatmaps_defs[c("universalism")], weights = T) 
heatmap_multiple(heatmaps_defs[c("main")], weights = T) 
heatmap_multiple(heatmaps_defs[c("main_all")], weights = T) 
heatmap_multiple(heatmaps_defs[c("support_binary")], weights = T) 
heatmap_multiple(heatmaps_defs[c("conjoint_ab", "conjoint_ab_all")], weights = T) 
heatmap_wrapper(vars = heatmaps_defs$main$vars, data = all, labels = heatmaps_defs$main$labels, name = "main_by_vote", along = "continent_vote", conditions = "/", folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = T, weights = T) 
heatmap_wrapper(vars = heatmaps_defs$main_all$vars, data = all, labels = heatmaps_defs$main_all$labels, name = "main_all_by_vote", along = "continent_vote", conditions = "/", folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = F, weights = T) 

# heatmaps_defs <- fill_heatmaps(c("conjoint_a_binary"), list())
# heatmap_multiple(heatmaps = heatmaps_defs)

temp <- read.xlsx("../../oecd_climate/tables/country_comparison/global_tax_attitudes_GCS_positive.xlsx")
temp2 <- read.xlsx("../../oecd_climate/tables/country_comparison/global_tax_attitudes_GCS_share.xlsx")
temp[13,6] <- temp2[13,6] <- wtd.mean(eu$gcs_support[eu$country == "FR"] == "Yes", weights = eu$weight_country[eu$country == "FR"])
temp[13,7] <- temp2[13,7] <- wtd.mean(eu$gcs_support[eu$country == "DE"] == "Yes", weights = eu$weight_country[eu$country == "DE"])
temp[13,12] <- temp2[13,12] <- wtd.mean(eu$gcs_support[eu$country == "ES"] == "Yes", weights = eu$weight_country[eu$country == "ES"])
temp[13,13] <- temp2[13,13] <- wtd.mean(eu$gcs_support[eu$country == "UK"] == "Yes", weights = eu$weight_country[eu$country == "UK"])
temp[13,14] <- temp2[13,14] <- wtd.mean(us1$gcs_support == "Yes", weights = us1$weight)
temp[13,2] <- temp2[13,2] <- mean(c(temp2[13,14], mean(temp2[13, c(6,7,12,13)]))) # as.numeric(temp[13,])
write.xlsx(temp, "../../oecd_climate/tables/country_comparison/global_tax_attitudes_GCS_positive.xlsx")
write.xlsx(temp2, "../../oecd_climate/tables/country_comparison/global_tax_attitudes_GCS_share.xlsx")

heatmap_wrapper(vars = heatmaps_defs$donation$vars, data = e, labels = heatmaps_defs$donation$labels, name = "donation", conditions = "", folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = T, weights = F) 

heatmap_multiple(heatmaps_defs[c("ets2_support")], weights = T, data = eu[eu$country != 'UK',])
heatmap_multiple(heatmaps_defs[c("ets2_no")], weights = T, data = eu[eu$country != 'UK',])
heatmap_multiple(heatmaps_defs[c("ets2_oppose")], weights = T, data = eu[eu$country != 'UK',])
heatmap_multiple(heatmaps_defs[c("gcs_important")], weights = T, data = all[all$vote >= 0,], name = "gcs_important_voteRight")
heatmap_multiple(heatmaps_defs[c("gcs_important")], weights = T, data = all[all$vote == -1,], name = "gcs_important_voteLeft")
heatmap_multiple(heatmaps_defs[c("gcs_important")], weights = T, data = all[all$gcs_support == "Yes",], name = "gcs_important_gcs_support")
heatmap_multiple(heatmaps_defs[c("gcs_important")], weights = T, data = all[all$gcs_support == "No",], name = "gcs_important_gcs_oppose")


##### Heterogeneity #####
barres_multiple(barres = barresN_defs[c("foreign_aid_preferred_info_agg", "foreign_aid_preferred_no_info_agg")], df = all, folder = "../figures/country_comparison/") 
barres_multiple(barres = barresN_continent_defs[c("foreign_aid_preferred_info_agg", "foreign_aid_preferred_no_info_agg")], df = all, folder = "../figures/continents/") 
barres_multiple(barres = barresN_defs["global_tax_global_share"], df = all, folder = "../figures/country_comparison/") 

barres_multiple(barres = barresN_defs, df = all, folder = "../figures/country_comparison/") 
barres_multiple(barres = barresN_continent_defs, df = all, folder = "../figures/continents/") 

barres_multiple(barres = barresN_vote_defs, df = eu, folder = "../figures/EU/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("FR"), folder = "../figures/FR/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("DE"), folder = "../figures/DE/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("ES"), folder = "../figures/ES/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("UK"), folder = "../figures/UK/vote/") 
barres_multiple(barres = barresN_vote3_defs, df = us, folder = "../figures/US/vote/") 
barres_multiple(barres = barresN_vote3_defs[c("gcs_support", "nr_support", "foreign_aid_raise_support")], df = us1, folder = "../figures/US1/vote/") 

barres_multiple(barres = barresN_age_defs, df = eu, folder = "../figures/EU/age/") 
barres_multiple(barres = barresN_age_defs, df = d("FR"), folder = "../figures/FR/age/") 
barres_multiple(barres = barresN_age_defs, df = d("DE"), folder = "../figures/DE/age/") 
barres_multiple(barres = barresN_age_defs, df = d("ES"), folder = "../figures/ES/age/") 
barres_multiple(barres = barresN_age_defs, df = d("UK"), folder = "../figures/UK/age/") 
barres_multiple(barres = barresN_age_defs, df = us, folder = "../figures/US/age/") 
barres_multiple(barres = barresN_age_defs[c("gcs_support", "nr_support", "foreign_aid_raise_support")], df = us1, folder = "../figures/US1/age/") 

barres_multiple(barres = barresN_income_defs, df = eu, folder = "../figures/EU/income/") 
barres_multiple(barres = barresN_income_defs, df = d("FR"), folder = "../figures/FR/income/") 
barres_multiple(barres = barresN_income_defs, df = d("DE"), folder = "../figures/DE/income/") 
barres_multiple(barres = barresN_income_defs, df = d("ES"), folder = "../figures/ES/income/") 
barres_multiple(barres = barresN_income_defs, df = d("UK"), folder = "../figures/UK/income/") 
barres_multiple(barres = barresN_income_defs, df = us, folder = "../figures/US/income/") 
barres_multiple(barres = barresN_income_defs[c("gcs_support", "nr_support", "foreign_aid_raise_support")], df = us1, folder = "../figures/US1/income/") 


##### Word clouds and vote #####
stopwords <- unlist(sapply(c("french", "english", "german", "spanish"), function(v) stopwords(v)))

for (country in c("US1", "US2", "EU", "FR", "DE", "ES", "UK")) {
  if (exists(tolower(country)) & is.data.frame(d(tolower(country)))) e <- d(country) else e <- eu[eu$country == country,]
  barres_defs <- define_barres_defs(e) # TODO update define_barres_defs, uncomment below, check it works for FR, etc.
  if (country != "US2") barres_multiple(barres = barres_defs[c("points")], df = e, folder = paste0("../figures/", country, "/")) 
  
  if ("comment_field" %in% names(e)) {
    rquery.wordcloud(paste(e$comment_field, collapse=" \n "), max.words = 70, colorPalette = "Blues", excludeWords = c(stopwords)) # , "climate", "change", "government"
    save_plot(filename = paste0("../figures/", country, "/comment_field"), height = 400, width = 400)
  }

  if ("gcs_field" %in% names(e)) {
    rquery.wordcloud(paste(e$gcs_field, collapse=" \n "), max.words = 70, colorPalette = "Blues", excludeWords = c(stopwords)) # , "climate", "change", "government"
    save_plot(filename = paste0("../figures/", country, "/gcs_field"), height = 400, width = 400)
  }

  if ("poverty_field" %in% names(e)) {
    rquery.wordcloud(paste(e$poverty_field, collapse=" \n "), max.words = 70, colorPalette = "Blues", excludeWords = c(stopwords)) # , "climate", "change", "government"
    save_plot(filename = paste0("../figures/", country, "/poverty_field"), height = 400, width = 400)
  }

  barres_multiple(barres = barres_defs[c("points", "vote_agg", "vote3", "vote", "vote_all", "vote_agg", paste0("vote_", tolower(country)), paste0("vote_", tolower(country), "_non_voters"), paste0("vote_", tolower(country), "_voters"))], df = e, folder = paste0("../figures/", country, "/"))
}


##### Sanbox #####
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


##### Comparison representativeness #####
labels_comp <- c("Population", "Sample: weighted", "Sample: non-weighted")

plot_comp <- function(var, df = e, country = "EU", miss = NULL, pop = NULL, export_xls = FALSE, trim = T, rev = T, rev_color = FALSE, width = 850, return = "plot") {
  if (is.null(pop)) {
    name_var <- if (paste0(country, "_", var) %in% names(levels_quotas)) paste0(country, "_", var) else var
    pop <- if (name_var %in% names(pop_freq[[country]])) pop_freq[[country]][[name_var]] else pop_freq[[country]][[var]]
    pop <- setNames(pop, gsub(".", " ", levels_quotas[[name_var]], fixed = T)) }

  missing_values <- as.character(unique(df[[var]][is.pnr(df[[var]])]))
  missing_values <- missing_values[!is.na(missing_values)]
  fr <- if (length(missing_values) == 1 & is.null(miss)) missing_values else FALSE
  miss <- if (length(missing_values) == 1 & is.null(miss)) T else FALSE
  if (length(missing_values) > 1) warning("More than one missing value.")

  if (abs(sum(pop) - 1) < 0.03 & fr != FALSE) pop <- pop / sum(pop[names(pop)!= fr])

  data <- cbind(dataN(var, data = df, miss = miss, weights = F, fr = fr), dataN(var, data = df, miss = miss, weights = F, fr = fr))
  row.names(data) <- dataN(var, data = df, miss = miss, fr = fr, return = "legend") 

  pop <- pop[row.names(data)]
  data <- cbind(data, pop)
  data <- data[sapply(1:nrow(data), function(i) (!all(data[i,]) <= 0.00001)),]
  if (return == "data") return(data)

  plot <- barres(data = data, export_xls = export_xls, labels = labels_comp, legend = row.names(data), miss = miss, sort = FALSE, rev = rev, rev_color = rev_color, showLegend = T, thin = T, weights = F)
  if (return == "plot") print(plot)
  save_plotly(plot, filename = paste0(var, "_comp"), folder = paste0("../figures/", ifelse(grepl("[0-9]", deparse(substitute(df))), toupper(deparse(substitute(df))), country), "/"), width = width, height = fig_height(nb_bars = 3, large = any(grepl("<br>", row.names(data)))), method = 'orca', trim = trim, format = 'pdf')

}

plot_all_comp <- function(df = eu, country = "EU") {
  for (v in names(pop_freq[[country]])) {
    tryCatch({plot_comp(var = sub(paste0(country, "_"), "", v), df = df, country = country)  
    print(paste0(v, ": success"))}, error = function(cond) { print(paste0(v, ": failed.")) } )
  } 
}

plot_all_comp(df = eu, country = "EU")
plot_all_comp(df = us1, country = "US")

plot_all_comp(df = us2, country = "US")
plot_all_comp(df = us, country = "US")

# plot_comp("gender", df = eu, country = "EU")
# plot_comp("income_quartile", df = e, country = "EU")
# plot_comp("age", df = eu, country = "EU")
# plot_comp("urbanity", df = eu, country = "EU")
# plot_comp("diploma_25_64", df = e, country = "EU") 
# plot_comp("employment_18_64", df = eu, country = "EU")
# plot_comp("vote", df = eu, country = "EU")
# plot_comp("country", df = eu, country = "EU")
# 
# plot_comp("gender", df = us1, country = "US")
# plot_comp("income_quartile", df = us1, country = "US")
# plot_comp("age", df = us1, country = "US")
# plot_comp("urban", df = us1, country = "US")
# plot_comp("diploma_25_64", df = us1, country = "US") 
# plot_comp("employment_18_64", df = us1, country = "US")
# plot_comp("region", df = us1, country = "US")
# plot_comp("race", df = us1, country = "US")
# plot_comp("vote", df = us1, country = "US") 
# plot_comp("vote_us", df = us1, country = "US")




##### Heterogeneity #####
plot_along(vars = c("gcs_support", "petition_gcs", "cap_wealth_support", "global_tax_more_half", "share_policies_supported", "points_foreign1_gcs"), 
           along = "age", conditions = c("", "", "> 0", "", "", "> 16.67"), covariates = c(), df = eu, save = FALSE)

plot_along(vars = c("gcs_support", "petition_gcs", "cap_wealth_support", "global_tax_more_half", "share_policies_supported", "points_foreign1_gcs"), 
           along = "income_factor", conditions = c("", "", "> 0", "", "", "> 16.67"), covariates = c(), df = eu, save = FALSE)

plot_along(vars = c("gcs_support", "petition_gcs", "cap_wealth_support", "global_tax_more_half", "share_policies_supported", "points_foreign1_gcs"), 
           along = "country", conditions = c("", "", "> 0", "", "", "> 16.67"), covariates = c(), df = eu, save = FALSE)

plot_along(vars = c("gcs_support", "petition_gcs", "cap_wealth_support", "global_tax_more_half", "share_policies_supported", "points_foreign1_gcs"), 
           along = "country", conditions = c("", "", "> 0", "", "", "> 16.67"), covariates = c(), df = all, save = FALSE)

barresN(vars = def$vars[vars_present], df = df, along = along, export_xls = export_xls, labels = def$labels[vars_present], share_labels = def$share_labels, margin_l = def$margin_l,
        miss = def$miss, sort = def$sort, rev = def$rev, rev_color = def$rev_color, legend = def$legend, showLegend = def$showLegend, thin = def$thin, weights = weights)

(temp <- barresN(vars = "negotiation", rev = F, rev_color = T,  export_xls = F, df = all, along = "country_name", miss=F, labels = paste0(labels_vars[vars], "<br>")))
save_plotly(temp, folder = "../figures/country_comparison/", filename = "negotiation", width= 850, height=fig_height(5), format = "pdf", trim = T)
  
(temp <- barresN(vars = "group_defended_agg2", rev = F, rev_color = T,  export_xls = F, df = all, along = "country_name", miss=F, labels = paste0("brl", "<br>")))
save_plotly(temp, folder = "../figures/country_comparison/", filename = "group_defended_agg2", width= 850, height=fig_height(5), format = "pdf", trim = T)

modelplot(lm(reg_formula("gcs_support", quotas_eu), data = eu), coef_map = rev(labels_vars), coef_omit = "Intercept", background = list(geom_vline(xintercept = 0, color = 'black')))
modelplot(lm(reg_formula("gcs_support", socio_demos), data = eu), coef_map = rev(labels_vars), coef_omit = "Intercept", background = list(geom_vline(xintercept = 0, color = 'black')))

modelplot(lm(reg_formula("gcs_support", quotas_us), data = us1), coef_map = rev(labels_vars), coef_omit = "Intercept", background = list(geom_vline(xintercept = 0, color = 'black')))
modelplot(lm(reg_formula("gcs_support", socio_demos_us), data = us1), coef_map = rev(labels_vars), coef_omit = "Intercept", background = list(geom_vline(xintercept = 0, color = 'black')))

reg_gcs_support_eu <- lm(reg_formula("gcs_support", socio_demos), data = eu)
dat <- map_dfr(c(.5, .9, .99), function(x) { modelplot(reg_gcs_support_eu, conf_level = x, draw = FALSE) %>% mutate(.width = x) })
dat$term <- as.character(dat$term)
dat$term[dat$term %in% names(labels_vars)] <-labels_vars[dat$term[dat$term %in% names(labels_vars)]]
ggplot(dat, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +# , color = model
  ggdist::geom_pointinterval(position = "dodge", interval_size_range = c(1, 3), fatten_point = .1) + xlab("") + ylab("") + theme_bw() # theme_classic()
