e <- us1p
e <- eup
# TODO! Spanish questionnaire, update .js, quotas, socio-demos

##### Duration #####
decrit("duration", data = e) # US1p: 9.5 / EU: 15 
decrit("duration_gcs", data = e) # US1p:  2.7/ EU: 2.5
decrit("duration_conjoint_a", data = e) # same
decrit("duration_conjoint_b", data = e) # US1p: .5 / EU: 
decrit("duration_conjoint_c", data = e) # NA
decrit("duration_conjoint_d", data = e) # US1p: .3 / EU: .4
decrit("duration_gcs_perception", data = e) # US2p:  / EU:  NA
decrit("duration_other_policies", data = e) # US2p:  / EU: 1.35
decrit("duration_points", data = e) # US2p: .73 / EU: .71
decrit("duration_feedback", data = e) # US1p: .35 / EU: .3


##### Attention #####
decrit("attention_test", data = e) # NAs because I removed donation question
decrit("score_understood) # 1.6 (random: 1.33", data = e)
decrit("nr_understood", data = e)
decrit("gcs_understood", data = e)
decrit("both_understood", data = e)
decrit("nr_win_lose", data = e)
decrit("gcs_win_lose", data = e)
decrit("both_win_lose", data = e)
decrit("click_details", data = e)
decrit("dropout", data = e)


##### Other #####
decrit("survey_biased", data = e) 
e$comment_field[!is.na(e$comment_field)]
decrit("interview", data = e)


##### Socio-demos #####
decrit("gender", data = e)
decrit("age", data = e) # TODO 
decrit("diploma", data = e)
decrit("income", data = e) # TODO 
decrit("urbanitry", data = e) # TODO 
decrit("urban_category", data = e)
decrit("race", data = e) # TODO 
decrit("region", data = e)
decrit("country", data = e)
decrit("employment_status", data = e)# TODO 
decrit("age_exact", data = e)
decrit("education", data = e) # ISCED
decrit("education_original", data = e)# TODO
decrit("couple", data = e)
decrit("hh_size", data = e)
decrit("home_tenant", data = e)
decrit("owner", data = e)
decrit("wealth", data = e)# TODO 
decrit("language", data = e)
CrossTable(e$language, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = F, total.c = F, total.r = F, cell.layout = F)
decrit("number_same_ip", data = e)


##### Support #####
decrit("support_igr", data = e)
decrit("nr_support", data = e)
decrit("gcs_support", data = e)
decrit("climate_compensation_support", data = e)
decrit("climate_mitigation_support", data = e)
decrit("climate_adaptation_support", data = e)
decrit("debt_cancellation_support", data = e)
decrit("democratise_un_imf_support", data = e)
decrit("remove_tariffs_support", data = e)
decrit("global_min_wage_support", data = e)
decrit("global_register_support", data = e)
decrit("cap_wealth_100m_support", data = e) # TODO: reword?
decrit("foreign_aid_raise_support", data = e)


##### GCS #####
decrit("gcs_support", data = e)
decrit("gcs_belief", data = e) 
decrit("petition_gcs", data = e)
decrit("points_foreign1_gcs", data = e)
decrit("support_igr", data = e)
decrit("branch_gcs_perception", data = e)
decrit("gcs_important_limit_CC", data = e)
decrit("gcs_important_hurt_economy", data = e)
decrit("gcs_important_hurt_me", data = e) 
decrit("gcs_important_change_lifestyles", data = e)
decrit("gcs_important_hurt_poor", data = e)
decrit("gcs_important_foster_cooperation", data = e)
decrit("gcs_important_fuel_corruption", data = e)
decrit("gcs_important_fuel_fraud", data = e)
decrit("gcs_important_difficult_enact", data = e)
decrit("gcs_important_having_info", data = e)
e$gcs_field[!is.na(e$gcs_field)]


##### NR ######
decrit("nr_support", data = e)
decrit("nr_belief", data = e)
decrit("petition_nr", data = e)
decrit("points_tax1_nr", data = e)


##### Petition #####
decrit("petition_matches_support", data = e) # 77%
summary(lm(petition_matches_support ~ branch_petition, data = e))
decrit("petition_yes_support_no", data = e)
decrit("petition_no_support_yes", data = e)


##### List experiment #####
decrit("list_exp_ir", data = e) # TODO!
decrit("list_exp_igr", data = e)
decrit("list_exp_i", data = e)
mean(e$list_exp_igr, na.rm = T) - mean(e$list_exp_ir, na.rm = T)
mean(e$list_exp_ir, na.rm = T) - mean(e$list_exp_i, na.rm = T)


##### Conjoint analysis #####
# a
decrit("conjoint_a", data = e)
decrit("conjoint_irg_ir", data = e)
decrit("conjoint_a_matches_support", data = e) # 84%
decrit("conjoint_a_irg_support_no", data = e) 
decrit("conjoint_a_ir_support_yes", data = e) 
# b
decrit("conjoint_b", data = e) # TODO!
decrit("conjoint_b_na", data = e)
decrit("conjoint_ir_gr", data = e)
decrit("conjoint_r_igr", data = e)
decrit("conjoint_gr_r", data = e)
decrit("conjoint_ir_r", data = e)
# c
decrit("conjoint_c", data = e)
decrit("conjoint_left_right", data = e)
decrit("conjoint_leftg_right", data = e)
summary(lm(conjoint_c ~ branch_c_gcs, data = e)) # -.15***
# d
decrit("conjoint_d", data = e)
decrit("conjoint_left_a_b", data = e)
decrit("conjoint_left_ag_b", data = e)


##### Donation #####
decrit("donation_nation", data = e) # TODO! put back
decrit("donation_africa", data = e) 
summary(lm(donation ~ branch_donation, data = e))
decrit("negotiation", data = e)


##### Wealth tax #####
decrit("global_tax_support", data = e)
decrit("national_tax_support", data = e)
decrit("global_tax_global_share", data = e) 
table(e$global_tax_sharing)


##### Foreign aid #####
decrit("foreign_aid_belief, numbers = T", data = e) # TODO raise/reduce
decrit("foreign_aid_preferred_no_info, numbers = T", data = e)
decrit("foreign_aid_preferred_info, numbers = T", data = e)
# decrit("foreign_aid_condition_", data = e)
# decrit("foreign_aid_raise_", data = e)
# decrit("foreign_aid_reduce_", data = e)
e$foreign_aid_condition_other[!is.na(e$foreign_aid_condition_other)]
e$foreign_aid_no_other[!is.na(e$foreign_aid_no_other)]
e$foreign_aid_raise_how_other[!is.na(e$foreign_aid_raise_how_other)]
e$foreign_aid_reduce_how_other[!is.na(e$foreign_aid_reduce_how_other)]
summary(lm(foreign_aid_preferred ~ info_foreign_aid, data = e))


##### 100 points #####
policies_names
decrit("points_econ1", data = e) 
decrit("points_econ2", data = e) # 
decrit("points_econ3", data = e)
decrit("points_econ4", data = e) # 
decrit("points_soc1", data = e)
decrit("points_soc2", data = e) #
decrit("points_soc3", data = e) # US1p
decrit("points_climate1", data = e)
decrit("points_climate2", data = e)
decrit("points_climate3", data = e)
decrit("points_tax1_nr", data = e)
decrit("points_tax2_wealth_tax", data = e)
decrit("points_tax3", data = e) # US1p
decrit("points_foreign1_gcs", data = e)
decrit("points_foreign2_tax_rich", data = e) # 
decrit("points_foreign3_assembly", data = e)
decrit("points_foreign4_aid", data = e)


##### Politics #####
decrit("political_affiliation", data = e)
decrit("left_right", data = e)
table(e$group_defended)
decrit("problem_inequality", data = e)
decrit("problem_climate", data = e)
decrit("problem_poverty", data = e)

