e <- us1p
e <- us2p
e <- eup
e <- eup[eup$country %in% c("DE", "ES"),]
e <- ep
# TODO! appendix sources
# TODO! plot maps and compare distributive effects of equal pc, contraction & convergence, greenhouse dvlpt rights, historical respo, and each country retaining its revenues
# TODO map % winners/losers
# TODO regress poverty gap on GDPpc and find a formula to allocate wealth tax revenues based on GDPpc (to not incentivize having large poverty gaps). Then estimate valuation of carbon date it approximates in $/tCO2 from historical respo.
# TODO! map: in % of GDP, with NDCs, at each date with 2°C + SDG trajectories => Gore (21) (I sent an email)
# TODO! reweighted estimate dans papier
# TODO? Use donation to measure altruism?
# TODO! decrit(us2$interview) decrit(us1$interview)

##### Duration #####
print(paste0(round(100*sum(us1a$finished == 1 & is.na(us1a$excluded), na.rm = T)/sum(us1a$finished == 1 | us1pa$excluded=="Screened", na.rm = T)), "% IR in US1")) # 92% % incidence rate
print(paste0(round(100*sum(us1a$dropout)/sum(is.na(us1a$excluded))), "% dropout in US1")) # 17% US1
print(paste0(round(100*sum(us2a$dropout)/sum(is.na(us2a$excluded))), "% dropout in US2")) # 2% EU
print(paste0(round(100*sum(eua$dropout)/sum(is.na(eua$excluded))), "% dropout in EU")) # 2% EU
print(paste0(round(100*sum(eupa$finished == 1 & is.na(eupa$excluded), na.rm = T)/sum(eupa$finished == 1 | eupa$excluded=="Screened", na.rm = T)), "% IR in EUp")) # 86%
print(paste0(round(100*sum(us1pa$excluded=="QuotaMet", na.rm = T)/nrow(us1pa)), "% QuotaMet")) # 4%
print(paste0(round(100*sum(us1pa$excluded=="Screened", na.rm = T)/nrow(us1pa)), "% Screened")) # 0%
print(paste0(round(100*sum(us1pa$dropout)/sum(is.na(us1pa$excluded))), "% dropout in US1p")) # 1%
print(paste0(round(100*sum(eupa$excluded=="QuotaMet", na.rm = T)/nrow(eupa)), "% QuotaMet")) # 7%
print(paste0(round(100*sum(eupa$excluded=="Screened", na.rm = T)/nrow(eupa)), "% Screened")) # 11%
print(paste0(round(100*sum(us1p$dropout)/sum(is.na(us1p$excluded))), "% dropout in US1p")) # 17% EU
print(paste0(round(100*sum(us2p$dropout)/sum(is.na(us2p$excluded))), "% dropout in US2p")) # 17% EU
print(paste0(round(100*sum(eupa$dropout & as.numeric(eupa$progress > 15))/sum(is.na(eupa$excluded))), "% dropout excluding sociodemos")) # 13% 
print(paste0(round(100*sum(eupa$dropout & as.numeric(eupa$progress == 16))/sum(is.na(eupa$excluded))), "% dropout at policy description")) # 7% (progress = 16 at policy description *for EUp*)
decrit("duration", data = us1) # US1p: 8.44 / US2p: 16.5 / EUp: 15.75
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
CrossTable(e$language, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = F, total.c = F, total.r = F, cell.layout = F) # TODO! remove mismatch
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
decrit("foreign_aid_raise_support", data = e) # TODO! 
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


##### Second-order beliefs #####
plot_along(vars = "gcs_belief", subsamples = "country", along = "income_factor", labels = Levels(all$country), conditions = c(""), df = all[!is.pnr(all$gcs_belief),], save = FALSE)
plot_along(vars = "gcs_belief", subsamples = "country", along = "diploma", labels = Levels(all$country), conditions = c(""), df = all[!is.pnr(all$gcs_belief),], save = FALSE)
all$


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
petition_petition <- petition_direct <- all[, c("petition", "branch_petition", "gcs_support", "nr_support", "country", "weight")]
petition_petition$support <- petition_petition$petition
petition_petition$type <- "petition"
petition_direct$support <- petition_direct$gcs_support
petition_direct$support[no.na(petition_direct$branch_petition) == 'nr'] <- petition_direct$nr_support[no.na(petition_direct$branch_petition) == 'nr']
petition_direct$type <- "simple"
petition <- rbind(petition_petition, petition_direct)
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition") + country, data = petition, weights = weight)) # Interaction not significant
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition") * country, data = petition, weights = weight))
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition"), data = petition, subset = country == "UK", weights = weight))
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition"), data = petition, subset = country == "US", weights = weight))
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition"), data = petition, subset = country == "FR", weights = weight))
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition"), data = petition, subset = country == "DE", weights = weight)) # Only country with significant interaction
summary(lm(support == "Yes" ~ (branch_petition == "gcs") * (type == "petition"), data = petition, subset = country == "ES", weights = weight))


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

# TODO!! t, p

# /!\ Weighted list experiment regression is not supported (yet) for the multi-item design.
# fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, family = binomial("logit"))
# fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_ict', J = 2, data = all, method = "lm", multi.condition = "level")
# (avg.pred.social.desirability <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)

# Code copied from https://github.com/SensitiveQuestions/list/blob/master/R/ictreg.R, line ~5030
# logistic <- function(object) exp(object)/(1+exp(object))
# fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 2 + wtd.mean(all$branch_list_exp_r == T, all$weight), data = all, weights = 'weight', method = "lm")
# fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all[all$wave != "US2",], weights = weight, family = binomial("logit"))
# # summary(lm(gcs_support == 'Yes' ~ 1, data = all[all$wave != "US2",], weights = weight))
# # fit.list <- fit.list_eu
# # fit.direct <- fit.direct_eu
# 
# beta <- fit.list$par.treat ## was coef(object)[1:nPar]
# var.beta <- vcov(fit.list)[1:length(fit.list$coef.names), 1:length(fit.list$coef.names)]
# xvar.direct <- model.matrix(as.formula(paste("~", c(fit.list$call$formula[[3]]))), fit.direct$data)
# n.draws <- 1000
# draws.list <- mvrnorm(n = n.draws, mu = beta, Sigma = var.beta/1.5^2) # /1.5^2 simulates a higher efficiency, as if ML estimator was used. Equality cannot be rejected with 90% confidence.
# draws.direct <- mvrnorm(n = n.draws, mu = coef(fit.direct), Sigma = vcov(fit.direct))
# pred.list.mean <- pred.direct.mean <- pred.diff.mean <- rep(NA, n.draws)
# for (d in 1:n.draws) {
#   par.g <- draws.list[d, ]
#   # if (object$method == "lm")
#     pred.list <- fit.list$x %*% par.g
#   # else
#   #   pred.list <- logistic(fit.list$x %*% par.g)
# 
#   pred.direct <- logistic(fit.list$x %*% draws.direct[d,])
# 
#   # pred.list.mean[d] <- mean(pred.list) # This is the original code, it doesn't include weights
#   # pred.direct.mean[d] <- mean(pred.direct)
#   pred.list.mean[d] <- wtd.mean(pred.list, weights = fit.list$weights)
#   pred.direct.mean[d] <- wtd.mean(pred.direct, weights = fit.list$weights)
#   pred.diff.mean[d] <- pred.list.mean[d] - pred.direct.mean[d]
# 
# }
# (est.diff <- mean(pred.diff.mean))
# (se.diff <- sd(pred.diff.mean))
# mean(pred.list.mean)
# mean(pred.direct.mean)
# level <- .9
# (ci.upper.diff <- est.diff + qt(1-(1-level)/2, df = nrow(fit.list$x)) * se.diff) # .007 (EU: .01)
# (ci.lower.diff <- est.diff - qt(1-(1-level)/2, df = nrow(fit.list$x)) * se.diff) # -.06 (EU: -.075)


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
summary(lm(conjoint_c ~ branch_c_gcs, data = us1, weights = weight, subset = us1$conjoint_c != "None")) # -.1
decrit("gcs_support", data = e, which = e$branch_conjoint_c == "leftg_right")
# d
decrit("conjoint_d", data = e) # 59%
decrit("conjoint_left_ag_b", data = e)
# r
decrit("conjoint_r", data = e) # 65%
decrit("conjoint_left_a_b", data = e)

same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_left", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "conjoint_c_right", dep.var.caption = "Prefers the Conservative platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_right", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

decrit("conjoint_c_none", all)
same_reg_subsamples(dep.var = "conjoint_c_none", dep.var.caption = "Prefers None of the platforms", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

# Good one (p-values instead of SEs)
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none_p", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, p_instead_SE = T)



##### Donation #####
decrit("donation_nation", data = e) 
decrit("donation_africa", data = e) 
summary(lm(donation ~ branch_donation, data = e))
decrit(!(e$donation %in% c(0, 100))) # 88%
decrit("negotiation", data = e)
CrossTable(e$negotiation, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F)
desc_table(dep_vars = "donation", filename = "donation_interaction", data = list(all, us1, us1, eu), 
           indep_vars = c("branch_donation", "vote3_factor", "branch_donation:vote3_factor", "branch_donation:vote3_factor"), model.numbers = F,
           indep_vars_included = list(c(T,F,F,F), c(T,F,F,F), c(T,T,T,T), c(T,F,F,F)), weights = "weight", save_folder = "../tables/continents/", robust_SE = FALSE, omit = c("Constant", "^  Vote", "^  vote3"))



##### GCS ~ info / pros_cons #####
decrit("nr_support", data = e)
decrit("gcs_support", data = e)
summary(lm(gcs_support ~ branch_gcs, data = us2, weights = weight))
summary(lm(reg_formula("gcs_support", c("branch_gcs", socio_demos_us)), data = us2, weights = weight))
summary(lm(reg_formula("gcs_support", c("branch_gcs", covariates[-1])), data = us2, weights = weight))
summary(lm(gcs_support ~ branch_gcs, data = usp))
summary(lm(gcs_support ~ branch_gcs, data = merge(us2, us2p, all = T)))
summary(lm(nr_support ~ branch_gcs, data = e, weights = weight))
summary(lm(gcs_support ~ branch_gcs * political_affiliation, data = e))
summary(lm(gcs_support ~ branch_gcs * (political_affiliation == "Republican"), data = e))
summary(lm(gcs_support ~ branch_gcs * (political_affiliation == "Democrat"), data = e))
.desc_table(c("gcs_support", "gcs_support", "nr_support", "nr_support"), filename = "branch_gcs", data = us2, indep_vars = c("branch_gcs", covariates), indep_vars_included = list("branch_gcs", c("branch_gcs", covariates), "branch_gcs", c("branch_gcs", covariates)), mean_control = T, model.numbers = T, #!mean_above,
              dep.var.labels = c("Global Climate Scheme", "National Redistribution"), dep.var.caption = c("Support"), digits= 3, robust_SE = T, omit = c("Constant", "Race: Other"), mean_above = T, only_mean = F, keep = "branch_gcs", save_folder = "../tables/US2/", nolabel = F, 
              add_lines = list(c(18, "Includes controls &  & \\checkmark &  & \\checkmark \\\\")))

desc_table("gcs_support", filename = "swing_state", data = us1, indep_vars = c("swing_state", "swing_state_5pp"), indep_vars_included = list("swing_state", "swing_state_5pp"), model.numbers = T, keep = c("Constant", "swing_state", "swing_state_5pp"), omit = c(), mean_above = F, #!mean_above,
           dep.var.labels = "Global Climate Scheme", dep.var.caption = c("Support"), digits= 3, robust_SE = T, only_mean = F, save_folder = "../tables/US1/", nolabel = F)


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
decrit(c(us2$global_tax_sharing, us2p$global_tax_sharing))
decrit(c(us2$global_tax_global_share, us2p$global_tax_global_share))
decrit(c(us2$global_tax_support, us2p$global_tax_support) > 0)
decrit(c(us2$global_tax_support, us2p$global_tax_support)[c(us2$global_tax_support, us2p$global_tax_support) != 0] > 0)
decrit(us2$vote)
decrit(us2p$vote)


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
summary(lm(foreign_aid_preferred ~ income_factor + country + foreign_aid_belief, all))

# a healthcare, education
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education), d(c)$weight), 3)))
# b healthcare, education, pensions
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education | d(c)$foreign_aid_reduce_how_pensions), d(c)$weight), 3)))
# c spending except defense
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education | d(c)$foreign_aid_reduce_how_pensions | d(c)$foreign_aid_reduce_how_welfare), d(c)$weight), 3)))
# d spending
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education | d(c)$foreign_aid_reduce_how_pensions | d(c)$foreign_aid_reduce_how_welfare) | d(c)$foreign_aid_reduce_how_defense | d(c)$foreign_aid_reduce_how_other, d(c)$weight), 3)))
# e taxes & deficit
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_income_tax | d(c)$foreign_aid_reduce_how_wealthy | d(c)$foreign_aid_reduce_how_corporations | d(c)$foreign_aid_reduce_how_deficit), d(c)$weight), 3)))
# f taxes
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_income_tax | d(c)$foreign_aid_reduce_how_wealthy | d(c)$foreign_aid_reduce_how_corporations), d(c)$weight), 3)))
# e << d
# f << a


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


##### Swing States & Democratic States #####
decrit(e$swing_state)
decrit("gcs_support", data = e, weights = e$weight, which = e$swing_state)
decrit("gcs_support", data = e, weights = e$weight, which = e$swing_state_5pp)
summary(lm(gcs_support == 'Yes' ~ swing_state_5pp, data = e, weights = e$weight)) 
summary(lm(gcs_support == 'Yes' ~ swing_state, data = e, weights = e$weight)) # .ht - 0.018 - 0.008
summary(lm(gcs_support == 'Yes' ~ swing_state + swing_state_5pp, data = e, weights = e$weight)) # .ht - 0.018 - 0.008
summary(lm(conjoint_c ~ branch_c_gcs * swing_state, data = e, weights = e$weight))
decrit("gcs_support", which = us1$democratic_state == T) # 62%


##### Reweighted estimate #####
reweighted_estimate("gcs_support", "EU") # 76
reweighted_estimate("gcs_support", "US1") # 53 Assigns a weight 0 to vote_us = PNR/No right
reweighted_estimate("gcs_support", "US1", omit = "vote_us") # 52 Uses all observations and still reweight for vote using e$vote
# reweighted_estimate("gcs_support", "US2") # Assigns a weight 0 to vote_us = PNR/No right => TODO only for branch_gcs == 'nothing'
# reweighted_estimate("gcs_support", "US2", omit = "vote_us") # Uses all observations and still reweight for vote using e$vote
reweighted_estimate("gcs_support", "EU", weights = T)
reweighted_estimate("gcs_support", "US1", weights = T) 
reweighted_estimate("gcs_support", "US2", verbose = T)
decrit("gcs_support", us1, weights = us1$weight_vote)
reweighted_estimate(NULL, "EU")
reweighted_estimate(NULL, "US1")
decrit("gcs_support", eu)
decrit("gcs_support", eu, weights = eu$weight_vote)
decrit("gcs_support", us1)
decrit("gcs_support", us1, weights = us1$weight_vote)


##### CI support ####
binconf(sum(us1$weight[us1$gcs_support == 'Yes']), nrow(us1))
# Significantly lower than 50% for branch_gcs == 'important' or branch_gcs %in% c('field', 'important'
binconf(sum(us2$weight[us2$gcs_support == 'Yes' & us2$branch_gcs == 'important']), sum(us2$weight[us2$branch_gcs == 'important']))
binconf(sum(us2$weight[us2$gcs_support == 'Yes' & us2$branch_gcs == 'field']), sum(us2$weight[us2$branch_gcs == 'field']))
binconf(sum(us2$weight[us2$gcs_support == 'Yes' & us2$branch_gcs %in% c('field', 'important')]), sum(us2$weight[us2$branch_gcs %in% c('field', 'important')]))
us3 <- us2[us2$branch_gcs == 'important', ]
reweighted_estimate("gcs_support", "US3", verbose = T)
rm(us3)
usa <- merge(us2, us2p, all = T)
binconf(sum(us2$gcs_support == 'Yes' & us2$branch_gcs == 'important'), sum(us2$branch_gcs == 'important'))
binconf(sum(usa$gcs_support == 'Yes' & usa$branch_gcs == 'important'), sum(usa$branch_gcs == 'important'))
binconf(sum(usa$gcs_support == 'Yes' & usa$branch_gcs %in% c('important', 'field')), sum(usa$branch_gcs %in% c('important', 'field')))
rm(usa)


##### Attrition without vote #####
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), 
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis", save_folder = "../tables/US1/", data = c(list(us1a), list(us1a), list(us1a[us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,])), 
           indep_vars = quotas_us) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis", save_folder = "../tables/US2/", data = c(list(us2a), list(us2a), list(us2a[us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,])), 
           indep_vars = quotas_us) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 6"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition_analysis", save_folder = "../tables/EU/", data = c(list(eua), list(eua), list(eua[eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,])), 
           indep_vars = quotas_eu) 


























