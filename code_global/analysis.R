e <- us1p
e <- eup


##### Duration #####
decrit(e$duration) # US1p:  / EU:  num
decrit(e$duration_gcs) # US1p:  / EU: 
decrit(e$duration_gcs) # US1p:  / EU: 
decrit(e$duration_conjoint_a) # US1p:  / EU: 
decrit(e$duration_conjoint_b) # US1p:  / EU: 
decrit(e$duration_conjoint_c) # US1p:  / EU: 
decrit(e$duration_conjoint_d) # US1p:  / EU: 
decrit(e$duration_gcs_perception) # US1p:  / EU: 
decrit(e$duration_other_policies) # US1p:  / EU: 
decrit(e$duration_100_points) # US1p:  / EU: 
decrit(e$duration_feedback) # US1p:  / EU: 


##### Attention #####
decrit(e$attention_test)
decrit(e$nr_win_lose)
decrit(e$gcs_win_lose)
decrit(e$both_win_lose)
decrit(e$click_details)
decrit(e$dropout)


##### Other #####
decrit(e$survey_biased)
e$comment_field[!is.na(e$comment_field)]
decrit(e$interview)


##### Socio-demos #####
# TODO
decrit(e$language)


##### Support #####
decrit(e$support_igr)
decrit(e$nr_support)
decrit(e$gcs_support)
decrit(e$climate_compensation_support)
decrit(e$climate_mitigation_support)
decrit(e$climate_adaptation_support)
decrit(e$debt_cancellation_support)
decrit(e$democratise_un_imf_support)
decrit(e$remove_tariffs_support)
decrit(e$global_min_wage_support)
decrit(e$global_register_support)
decrit(e$cap_wealth_100m_support)
decrit(e$foreign_aid_raise_support)


##### GCS #####
decrit(e$gcs_support)
decrit(e$gcs_belief) # num
decrit(e$petition_gcs)
decrit(e$points_foreign1_gcs)
decrit(e$support_igr)
decrit(e$gcs_important_limit_CC)
decrit(e$gcs_important_hurt_economy)
decrit(e$gcs_important_hurt_me)
decrit(e$gcs_important_change_lifestyles)
decrit(e$gcs_important_hurt_poor)
decrit(e$gcs_important_foster_cooperation)
decrit(e$gcs_important_fuel_corruption)
decrit(e$gcs_important_fuel_fraud)
decrit(e$gcs_important_difficult_enact)
decrit(e$gcs_important_having_info)
e$gcs_field[!is.na(e$gcs_field)]


##### NR ######
decrit(e$nr_support)
decrit(e$nr_belief)
decrit(e$petition_nr)
decrit(e$points_tax1_nr)


##### List experiment #####
decrit(e$list_exp_ir)
decrit(e$list_exp_igr)
decrit(e$list_exp_i)


##### Conjoint analysis #####
# a
decrit(e$conjoint_irg_ir)
# b
decrit(e$conjoint_ir_gr)
decrit(e$conjoint_r_igr)
decrit(e$conjoint_gr_r)
decrit(e$conjoint_ir_r)
# c
decrit(e$conjoint_left_right)
decrit(e$conjoint_leftg_right)
# d
decrit(e$conjoint_left_a_b)
decrit(e$conjoint_left_ag_b)


##### Donation #####
decrit(e$donation_nation) # TODO
decrit(e$donation_africa) # num
decrit(e$negotiation)


##### Wealth tax #####
decrit(e$global_tax_support)
decrit(e$national_tax_support)
decrit(e$global_tax_global_share) # num
decrit(e$global_tax_sharing)


##### Foreign aid #####
decrit(e$foreign_aid_belief) # TODO
decrit(e$foreign_aid_preferred_no_info)
decrit(e$foreign_aid_preferred_info)
# decrit(e$foreign_aid_condition_)
# decrit(e$foreign_aid_raise_)
# decrit(e$foreign_aid_reduce_)
e$foreign_aid_condition_other[!is.na(e$foreign_aid_condition_other)]
e$foreign_aid_no_other[!is.na(e$foreign_aid_no_other)]
e$foreign_aid_raise_how_other[!is.na(e$foreign_aid_raise_how_other)]
e$foreign_aid_reduce_how_other[!is.na(e$foreign_aid_reduce_how_other)]


##### 100 points #####
decrit(e$points_econ1) # num
decrit(e$points_econ2)
decrit(e$points_econ3)
decrit(e$points_econ4)
decrit(e$points_soc1)
decrit(e$points_soc2)
decrit(e$points_soc3) # US1p
decrit(e$points_climate1)
decrit(e$points_climate2)
decrit(e$points_climate3)
decrit(e$points_tax1_nr)
decrit(e$points_tax2_wealth_tax)
decrit(e$points_tax3) # US1p
decrit(e$points_foreign1_gcs)
decrit(e$points_foreign2_tax_rich)
decrit(e$points_foreign3_assembly)
decrit(e$points_foreign4_aid)


##### Politics #####
decrit(e$political_affiliation)
decrit(e$left_right)
table(e$group_defended)
decrit(e$problem_inequality)
decrit(e$problem_climate)
decrit(e$problem_poverty)

