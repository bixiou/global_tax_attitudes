e <- us1p
e <- us2p
e <- eup
e <- eup[eup$country %in% c("DE", "ES"),]
e <- ep
# TODO! slides
# TODO! literature review
# TODO! appendix sources, calcul net gain
# TODO US/EU: put back email?, welcome: amount incentives
# TODO? mettre soutien/Croyances GCS+NR dans le bloc d'avant? bof, faudrait refaire des blocs pck on donne la réponse aux questions aussi
# TODO? US/EU: correct => expected for DE/ES questions? (already in answers)
# TODO read about Norway and foreign aid
# TODO? Ask 200 more respondents to rebalance for age-UK


##### Duration #####
print(paste0(round(100*sum(us1a$finished == 1 & is.na(us1a$excluded), na.rm = T)/sum(us1a$finished == 1 | us1pa$excluded=="Screened", na.rm = T)), "% IR in US1")) # 92% % incidence rate
print(paste0(round(100*sum(us1a$dropout)/sum(is.na(us1a$excluded))), "% dropout in US1")) # 11% US1
print(paste0(round(100*sum(eupa$finished == 1 & is.na(eupa$excluded), na.rm = T)/sum(eupa$finished == 1 | eupa$excluded=="Screened", na.rm = T)), "% IR in EUp")) # 86%
print(paste0(round(100*sum(us1pa$excluded=="QuotaMet", na.rm = T)/nrow(us1pa)), "% QuotaMet")) # 4%
print(paste0(round(100*sum(us1pa$excluded=="Screened", na.rm = T)/nrow(us1pa)), "% Screened")) # 0%
print(paste0(round(100*sum(us1pa$dropout)/sum(is.na(us1pa$excluded))), "% dropout in US1p")) # 1%
print(paste0(round(100*sum(eupa$excluded=="QuotaMet", na.rm = T)/nrow(eupa)), "% QuotaMet")) # 7%
print(paste0(round(100*sum(eupa$excluded=="Screened", na.rm = T)/nrow(eupa)), "% Screened")) # 11%
print(paste0(round(100*sum(eupa$dropout)/sum(is.na(eupa$excluded))), "% dropout in EUp")) # 17% EU
print(paste0(round(100*sum(us1p$dropout)/sum(is.na(us1p$excluded))), "% dropout in US1p")) # 17% EU
print(paste0(round(100*sum(us2p$dropout)/sum(is.na(us2p$excluded))), "% dropout in US2p")) # 17% EU
print(paste0(round(100*sum(eupa$dropout & as.numeric(eupa$progress > 15))/sum(is.na(eupa$excluded))), "% dropout excluding sociodemos")) # 13% 
print(paste0(round(100*sum(eupa$dropout & as.numeric(eupa$progress == 16))/sum(is.na(eupa$excluded))), "% dropout at policy description")) # 7% (progress = 16 at policy description *for EUp*)
decrit("duration", data = us1p) # US1p: 8.44 / US2p: 16.5 / EUp: 15.75
decrit("duration", data = us2p) 
decrit("duration", data = us2p, which = is.na(us2p$branch_iat)) # 12.8
decrit("duration", data = us2p, which = !is.na(us2p$branch_iat)) # 18.5 => duration IAT: 5.6
decrit("duration", data = eup)
decrit("duration", data = e) 
decrit("duration", data = us1p, which = us1p$duration > 4) # US1p: 9 / US2p: 12.9 / EU: 17
decrit("duration", data = us2p, which = us2p$duration > 6) 
decrit("duration", data = eup, which = eup$duration > 6)
decrit("duration_gcs", data = e) # US1p: 2.7 / EU: 2.5 / US2p: 1.7
decrit("duration_nr", data = e) # US2p: .73
decrit("duration_both", data = e) # US2p: .66
decrit("duration_conjoint_a", data = e) # same
decrit("duration_conjoint_b", data = e) # US1p: .5 / EU: 
decrit("duration_conjoint_c", data = e) # NA
decrit("duration_conjoint_d", data = e) # US1p: .3 / EU: .4
decrit("duration_gcs_perception", data = e) # US2p: .7 / EU: .8 NA
decrit("duration_other_policies", data = e) # US2p: 1.25 / EU: 1.35
decrit("duration_points", data = e) # US1p: .9 / EU: .9
decrit("duration_donation", data = e) # US2p: .5
decrit("duration_list_exp", data = e) # US2p: .6
decrit("duration_feedback", data = e) # US1p: .35 / EU: .3 / US2p: .5
# US2p: 16.5 = 6.64 + socio-demos/politics + wealth + foreign aid + IAT


##### Attention #####
decrit("attention_test", data = e) # NAs because I removed donation question
decrit("score_understood", data = e) # 1.5 (random: .83). US2p: 1.7
decrit("nr_understood", data = e)
decrit("gcs_understood", data = e)
decrit("both_understood", data = e)
decrit("nr_win_lose", data = e)
decrit("gcs_win_lose", data = e)
decrit("both_win_lose", data = e)
decrit("click_details", data = e) # 10%
decrit("click_reminder", data = e) # 13%
decrit("dropout", data = e) # TODO!


##### Other #####
decrit("survey_biased", data = e) 
e$comment_field[!is.na(e$comment_field)] # US1p/EUp1 all good except two people who complain about the layout, one doesn't give details and one say there is too much space between questions.
decrit("interview", data = e)


##### Socio-demos #####
decrit("gender", data = e)
decrit("age", data = e) 
decrit("diploma_25_64", data = e, miss = F)
decrit("diploma_25_64", data = e, miss = T)
decrit("diploma", data = e)
decrit("income_decile", data = e)
decrit("income_quartile", data = e)
decrit("urbanity", data = e) 
decrit("race", data = e) 
decrit("region", data = e)
decrit("country", data = e)
decrit("employment_status", data = e) 
decrit("employment_agg", data = e) 
decrit("age", data = e, numbers = T) 
decrit("age_exact", data = e)
decrit("education", data = e) 
decrit("education_original", data = e)
decrit("couple", data = e)
decrit("hh_size", data = e)
decrit("home_tenant", data = e)
decrit("owner", data = e)
decrit("wealth", data = e, miss = F)
decrit("language", data = e, weight = F)
CrossTable(e$language, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = F, total.c = F, total.r = F, cell.layout = F)
decrit("number_same_ip", data = e)
representativeness_table(c("US1"), return_table = F)


##### Support #####
# 'positive' gives the share of positive answers and 'majority' the share of positive answers among non-indifferent.
datasummary(eval(str2expression(paste(paste(c(variables_support, "petition_gcs", "petition_nr"), collapse = ' + '), " ~ positive + majority + majority * country"))), data = e, fmt = 0, output = "markdown")
majority(e$climate_mitigation_support)
decrit("cgr_support", data = e)
decrit("gcs_support", data = e)
decrit("nr_support", data = e)
CrossTable(e$gcs_support, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)
CrossTable(e$nr_support, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)
CrossTable(e$cgr_support, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)
decrit("climate_compensation_support", data = e)
decrit("climate_mitigation_support", data = e)
decrit("climate_adaptation_support", data = e)
decrit("debt_cancellation_support", data = e)
decrit("democratise_un_imf_support", data = e)
decrit("remove_tariffs_support", data = e)
decrit("global_min_wage_support", data = e)
decrit("global_register_support", data = e)
decrit("cap_wealth_100m_support", data = e) 
decrit("foreign_aid_raise_support", data = e) 
means_support <- indiferrents_support <- relative_majority_support <- c()
for (v in variables_support) means_support[v] <- mean(e[[v]] > 0, na.rm = T)
for (v in variables_other_policies) indiferrents_support[v] <- mean(e[[v]] == 0, na.rm = T)
for (v in variables_other_policies) relative_majority_support[v] <- mean(e[[v]][e[[v]] != 0] > 0, na.rm = T)
-sort(-means_support)
-sort(-indiferrents_support)
-sort(-relative_majority_support)


##### GCS #####
decrit("gcs_support", data = e)
decrit("gcs_support", data = e, which = e$diploma_25_64 == "Below upper secondary", weight = F)
decrit("gcs_support", data = e, which = e$income_quartile == 4, weight = F)
decrit("gcs_support", data = e, which = e$race_hispanic == T, weight = F)
decrit("gcs_support", data = e, which = e$region == "West", weight = F)
decrit("gcs_support", data = e, which = e$man == T, weight = F)
decrit("weight", data = e, which = e$diploma_25_64 == "Below upper secondary", weight = F)
decrit("gcs_belief", data = e) 
decrit("petition_gcs", data = e)
decrit("points_foreign1_gcs", data = e)
decrit("cgr_support", data = e)
decrit("branch_gcs_perception", data = e)
decrit("gcs_important_limit_CC", data = e) #
decrit("gcs_important_hurt_economy", data = e)
decrit("gcs_important_hurt_me", data = e) #
decrit("gcs_important_change_lifestyles", data = e) #
decrit("gcs_important_reduce_poverty", data = e) #
decrit("gcs_important_hurt_poor", data = e)
decrit("gcs_important_foster_cooperation", data = e)
decrit("gcs_important_fuel_corruption", data = e) # 
decrit("gcs_important_fuel_fraud", data = e)
decrit("gcs_important_difficult_enact", data = e)
decrit("gcs_important_having_info", data = e)
decrit("order_wealth_separate", data = e)
means_gcs_important <- c()
for (v in variables_gcs_important) means_gcs_important[v] <- mean(e[[v]], na.rm = T)
-sort(-means_gcs_important)
e$gcs_field[!is.na(e$gcs_field)]
summary(lm(gcs_support ~ branch_gcs, data = e))
summary(lm(nr_support ~ branch_gcs, data = e))
summary(lm(gcs_support ~ z_score_understood, data = e)) # +7pp
summary(lm(gcs_support ~ z_score_understood + education + income_factor + as.factor(age_exact), data = e))
summary(lm(gcs_support ~ gcs_understood + nr_understood + both_understood, data = e))

e$post_secondary_25_64 <- replace_na(e$diploma_25_64 == 'Post secondary', F)
e$upper_secondary_25_64 <- replace_na(e$diploma_25_64 == 'Upper secondary', F)
summary(lm(reg_formula("gcs_support", socio_demos_us), data = us1)) # ***: age, urbanity, diploma, urban
summary(lm(reg_formula("gcs_support", c("as.factor(age)")), data = us1))
datasummary(gcs_support  ~ Mean*as.factor(age), data = us1, output = 'markdown')
datasummary(gcs_support  ~ Mean*(woman + upper_secondary_25_64 + post_secondary_25_64), data = us1, output = 'markdown')
datasummary(gcs_support  ~ Mean*(race + region), data = us1, output = 'markdown')
datasummary(gcs_support  ~ Mean*as.factor(urbanity), data = us1, output = 'markdown')
datasummary(gcs_support  ~ Mean*as.factor(income_quartile), data = us1, output = 'markdown')


##### NR ######
decrit("nr_support", data = e)
decrit("nr_belief", data = e)
decrit("petition_nr", data = e)
decrit("points_tax1_nr", data = e)
summary(lm(nr_support ~ z_score_understood, data = e)) # +12pp
summary(lm(nr_support ~ gcs_understood + nr_understood + both_understood, data = e))


##### Petition #####
decrit("petition_gcs", data = e)
decrit("petition_nr", data = e)
decrit("petition_matches_support", data = e) # 77%
summary(lm(petition_matches_support ~ branch_petition, data = e))
decrit("petition_yes_support_no", data = e)
decrit("petition_no_support_yes", data = e)


##### List experiment #####
decrit("branch_list_exp", data = e)
decrit("list_exp_rl", data = e) # ir
decrit("list_exp_gl", data = e) # gr
decrit("list_exp_rgl", data = e) # igr
decrit("list_exp_l", data = e) # i
mean(e$list_exp_rgl, na.rm = T) - mean(e$list_exp_rl, na.rm = T) # 67%
mean(e$gcs_support[e$branch_list_exp == "rgl"], na.rm = T) # 67%
mean(e$gcs_support[e$branch_list_exp == "rgl"], na.rm = T) + mean(e$nr_support[e$branch_list_exp == "rgl"], na.rm = T) - mean(e$nr_support[e$branch_list_exp == "rl"], na.rm = T) # 74%
mean(e$list_exp_rgl, na.rm = T) - mean(e$list_exp_gl, na.rm = T) # 92%
mean(e$gcs_support[e$branch_list_exp == "rgl"], na.rm = T) + mean(e$nr_support[e$branch_list_exp == "rgl"], na.rm = T) - mean(e$nr_support[e$branch_list_exp == "rl"], na.rm = T) # 79%
mean(e$list_exp_rl, na.rm = T) - mean(e$list_exp_l, na.rm = T) # 57%
mean(e$nr_support[e$branch_list_exp == "rl"], na.rm = T) # 53%
mean(e$list_exp_gl, na.rm = T) - mean(e$list_exp_rl, na.rm = T) # -26%
summary(lm(list_exp ~ (branch_list_exp == "gl") + (branch_list_exp == "l") + (branch_list_exp == "rgl"), data = e))
summary(lm(list_exp ~ branch_list_exp_g * branch_list_exp_r, data = e, weights = NULL))
desc_table(dep_vars = "list_exp", filename = "US1/reg_list_exp", dep.var.labels = "Number of supported policies", weights = NULL, omit = c(), 
           indep_vars = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), nolabel = F,
           indep_labels = c("List contains: G", "List contains: R", "List contains: G $\\times$ R"))

# Done: switched to i, r, ir, gr, igr
# 1 nr_support: ir - i / igr - ig / gr - g => nr - n
# 2 gcs_support: igr - ir / gr - r / ig - i => ng - n
# 0 gcs_support en double
# 3 nb of options: igr - ir, gr - r / igr - ig, ir - i => okl - ok, ol - o
# 4 prefer i or g: gr - ir / conjoint analysis (ir vs. gr)
# 5 i_support: igr - ir / ig - g / ir - r / conjoint analysis (ir vs. r)
# 6 l_support: gr - r - g / ir - i - r / igr - gr - i 
# interaction r and g: impossible, conjoint analysis for that
# i, ir, gr, igr: 12456
# r, ir, gr, igr: 02345
# g, ir, gr, igr: 12456
# i, ig, ir, igr: 0123 pre-registered
# i, r, ir, gr, igr: 0123456
# g, r, ir, gr, igr: 0123456
# i, g, ir, gr, igr: 123456
# i, ig, ir, gr, igr: 123456


##### Conjoint analysis #####
# a
decrit("conjoint_a", data = e)
decrit("conjoint_crg_cr", data = e)
decrit("conjoint_a_matches_support", data = e) # 84%
decrit("conjoint_a_irg_support_no", data = e) 
decrit("conjoint_a_ir_support_yes", data = e) 
# b
decrit("conjoint_b", data = e)
decrit("conjoint_b_na", data = e)
decrit("conjoint_cr_gr", data = e)
decrit("gcs_support", data = e, which = e$branch_conjoint_b == "ir_gr")
decrit("conjoint_r_rcg", data = e)
decrit("conjoint_rg_r", data = e)
decrit("conjoint_rc_r", data = e)
decrit("branch_conjoint_b", data = e)
# summary(lm(conjoint_b ~ branch_conjoint_b, data = e))
# c
decrit("conjoint_c", data = e) # TODO! non-right
decrit("conjoint_left_right", data = e)
decrit("conjoint_leftg_right", data = e)
summary(lm(conjoint_c ~ branch_c_gcs, data = e[e$country == "FR",])) # -.1
decrit("gcs_support", data = e, which = e$branch_conjoint_c == "leftg_right")
# d
decrit("conjoint_d", data = e) # 59%
decrit("conjoint_left_ag_b", data = e)
# r
decrit("conjoint_r", data = e) # 65%
decrit("conjoint_left_a_b", data = e)


##### Donation #####
decrit("donation_nation", data = e) 
decrit("donation_africa", data = e) 
summary(lm(donation ~ branch_donation, data = e))
decrit(!(e$donation %in% c(0, 100))) # 88%
decrit("negotiation", data = e)
CrossTable(e$negotiation, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)


##### Wealth tax #####
decrit("global_tax_support", data = e)
decrit("national_tax_support", data = e)
decrit("global_tax_global_share", data = e) 
median(e$global_tax_global_share, na.rm = T) 
decrit("global_tax_sharing", data = e) 
CrossTable(e$global_tax_sharing, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)
summary(lm(global_tax_support > 0 ~ branch_global_tax, data = e))
summary(lm(national_tax_support > 0 ~ branch_global_tax, data = e))
summary(lm(global_tax_support - national_tax_support ~ branch_global_tax, data = e))


##### Foreign aid #####
decrit("foreign_aid_belief", numbers = T, data = e) 
decrit("foreign_aid_preferred_no_info", numbers = T, data = e)
decrit("foreign_aid_preferred_info", numbers = T, data = e)
decrit("foreign_aid_more_less", data = e)
decrit("foreign_aid_more_less", data = e, which = e$info_foreign_aid == T)
decrit("foreign_aid_more_less", data = e, which = e$info_foreign_aid == FALSE)
# decrit("foreign_aid_condition_", data = e)
# decrit("foreign_aid_raise_", data = e)
# decrit("foreign_aid_reduce_", data = e)
e$foreign_aid_condition_other[!is.na(e$foreign_aid_condition_other)]
e$foreign_aid_no_other[!is.na(e$foreign_aid_no_other)]
summary(lm(foreign_aid_preferred ~ info_foreign_aid, data = e))
means_foreign_aid_condition <- means_variables_foreign_aid_no_ <- means_variables_foreign_aid_raise <- means_variables_foreign_aid_reduce <- c()
for (v in variables_foreign_aid_condition) means_foreign_aid_condition[v] <- mean(e[[v]], na.rm = T)
for (v in variables_foreign_aid_no_) means_variables_foreign_aid_no_[v] <- mean(e[[v]], na.rm = T)
for (v in variables_foreign_aid_raise) means_variables_foreign_aid_raise[v] <- mean(e[[v]], na.rm = T)
for (v in variables_foreign_aid_reduce) means_variables_foreign_aid_reduce[v] <- mean(e[[v]], na.rm = T)
-sort(-means_foreign_aid_condition)
-sort(-means_variables_foreign_aid_no_)
-sort(-means_variables_foreign_aid_raise)
-sort(-means_variables_foreign_aid_reduce)


##### 100 points #####
policies_names
decrit("points_econ1", data = e) 
decrit("points_econ2", data = e) # 
decrit("points_econ3", data = e)
decrit("points_econ4", data = e) # 
decrit("points_soc1", data = e)
decrit("points_soc2", data = e) 
decrit("points_soc3", data = e) # US1p
decrit("points_climate1", data = e)
decrit("points_climate2", data = e)
decrit("points_climate3", data = e) #-
decrit("points_tax1_nr", data = e)
decrit("points_tax2_wealth_tax", data = e) #
decrit("points_tax3_corporate_tax", data = e) # US1p
decrit("points_foreign1_gcs", data = e)
decrit("points_foreign2_tax_rich", data = e) # 
decrit("points_foreign3_assembly", data = e)
decrit("points_foreign4_aid", data = e) #-
(mean_points <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(e[[v]], na.rm = T, weights = NULL), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"]))))
(positive_points <- sort(setNames(sapply(variables_points_us, function(v) round(100*wtd.mean(e[[v]] > 0, na.rm = T, weights = NULL))), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"]))))


##### IAT #####
decrit("branch_iat", data = e) # 59% enter IAT


##### Politics #####
decrit("political_affiliation", data = e)
decrit("left_right", data = e)
decrit("group_defended", data = e) 
major_candidates
minor_candidates
decrit("vote", data = e, which = e$country == "US")
decrit("vote_agg", data = e, which = e$country == "FR")
table(e$vote[e$country == "DE",])
decrit("vote", data = e, which = e$country == "UK")
decrit("vote", data = e, which = e$country == "DE")
decrit("vote", data = e, which = e$country == "ES")
decrit("vote_us_voters", data = e)
decrit("vote_fr_voters", data = e)
decrit("vote_uk_voters", data = e)
decrit("vote_de_voters", data = e)
decrit("vote_es_voters", data = e)
decrit("problem_inequality", data = e)
decrit("problem_climate", data = e)
decrit("problem_poverty", data = e)
means_variables_problem <- c()
for (v in variables_problem) means_variables_problem[v] <- mean(e[[v]], na.rm = T)
-sort(-means_variables_problem)


##### Swing States #####
decrit(e$swing_state)
summary(lm(gcs_support == 'Yes' ~ swing_state + swing_state_5pp, data = e, weights = e$weight)) # .56 - 0.018 - 0.008
summary(lm(conjoint_c ~ branch_c_gcs * swing_state, data = e, weights = e$weight))
