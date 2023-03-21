##### Stated support #####
# TODO complete
# support_binary_positive
# support_likert_positive, support_likert_share

# Foreign aid
decrit(all$foreign_aid_raise_support)
# all/foreign_aid_condition

##### Petition ##### 
t.test(us1$gcs_support[us1$branch_petition == "gcs"], us1$petition_gcs[us1$branch_petition == "gcs"]) # cannot reject equality (p=.11)
t.test(eu$gcs_support[eu$branch_petition == "gcs"], eu$petition_gcs[eu$branch_petition == "gcs"]) # cannot reject equality (p=.11)
wtd.t.test(us1$gcs_support[us1$branch_petition == "gcs"], us1$petition_gcs[us1$branch_petition == "gcs"], weight=us1$weight[us1$branch_petition == "gcs"]) # cannot reject equality (p=.30)
wtd.t.test(eu$gcs_support[eu$branch_petition == "gcs"], eu$petition_gcs[eu$branch_petition == "gcs"], weight=eu$weight[eu$branch_petition == "gcs"]) # rejects equality (p=1e-5)


##### Second-order beliefs ##### 
decrit(us1$gcs_belief)
decrit(eu$gcs_belief)
decrit(eu$gcs_belief > 50, weights = eu$weight)


##### Universalism #####
# group_defended_agg, group_defended_agg2
decrit("group_defended_agg", data = all, which = all$vote != 0)
decrit("group_defended_agg", data = eu, which = eu$vote != 0)
decrit("group_defended_agg", data = us1, which = us1$vote != 0)
decrit("group_defended_agg", data = all, which = all$vote == -1)
decrit("group_defended_agg", data = eu, which = eu$vote == -1)
decrit("group_defended_agg", data = us1, which = us1$vote == -1)
# all/negotiation
wtd.mean(all$problem_climate, weights = all$weight)
wtd.mean(all$problem_poverty, weights = all$weight)
wtd.mean(all$problem_inequality, weights = all$weight)


##### Donation #####
same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation", "vote_factor", "branch_donation:vote_factor"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_covariates", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation_above_25 ", dep.var.caption = "More than 25\\% donated to poor people", covariates = c("branch_donation"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_above_25 ", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation_above_25 ", dep.var.caption = "More than 25\\% donated to poor people", covariates = c("branch_donation"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_above_25 ", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### Global millionaire tax #####
same_reg_subsamples(dep.var = "global_tax_sharing", dep.var.caption = "Prefers to share half of global tax with low-income countries", covariates = c("vote_factor"), 
                    data = all[all$country != "US",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "global_tax_sharing_vote", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### Conjoint analysis: Left/Right vote #####
# TODO: weight_country or even weight_vote
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_left", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "conjoint_c_right", dep.var.caption = "Prefers the Conservative platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_right", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "conjoint_c_none", dep.var.caption = "Prefers None of the platforms", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F,], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_wo_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

for (i in c("left", "right", "none", "wo_none")) {
  temp <- readLines(paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
  writeLines(sub("United Kingdom", "UK", temp), paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
}


##### List experiment #####
same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
                    data = all, along = "continent", nolabel = F, include.total = FALSE, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
                    filename = "reg_list_exp", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
                                     c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))

same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "reg_list_exp_all", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(all$gcs_support, weights = all$weight), 3), "} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
                                     c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(all$nr_support, weights = all$weight), 3), "} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))

all$list_exp_0 <- all$list_exp == 0
same_reg_subsamples(dep.var = "list_exp_0", dep.var.caption = "No supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
                    data = all, along = "continent", nolabel = F, include.total = FALSE, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "reg_list_exp_0", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
                                     c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))

# TODO! Find a test that works for the combined list experiment branch_list_exp_g:branch_list_exp_r. Mail sent to Blair & Imai 
# From Blair & Imai (12) /!\ TODO! Read the paper to understand whether "lm" method (yielding the same as lm regression) or the default "ml" is preferable
fit.list_no_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
fit.list_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
fit.direct <- glm(gcs_support == 'Yes' ~ 1, data = all, family = binomial("logit"))
(avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
(avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)

fit.list_no_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "US" & all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
fit.list_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "US" & all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
fit.direct_US <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "US",], family = binomial("logit"))
(avg.pred.social.desirability_no_r_US <- predict(fit.list_no_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # -.06 95% CI: (-.15, .03)
(avg.pred.social.desirability_r_US <- predict(fit.list_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # .004 95% CI: (-.12, .13)

fit.list_no_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "Eu" & all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
fit.list_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "Eu" & all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
fit.direct_Eu <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "Eu",], family = binomial("logit"))
(avg.pred.social.desirability_no_r_Eu <- predict(fit.list_no_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # .01 95% CI: (-.07, .09)
(avg.pred.social.desirability_r_Eu <- predict(fit.list_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # -.10 95% CI: (-.17, -.03)

fit.list_no_r <- ictreg(list_exp ~ continent, treat = c('branch_list_exp_g'), J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
fit.list_r <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, family = binomial("logit"))
(avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
(avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
plot(avg.pred.social.desirability_no_r)
plot(avg.pred.social.desirability_r)

# Good ones here
summary(lm(list_exp ~ branch_list_exp_g*continent, data = all, weights = all$weight))
fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 2 + wtd.mean(all$branch_list_exp_r == T, all$weight), data = all, weights = 'weight', method = "lm")
fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, weights = weight, family = binomial("logit"))
(avg.pred.social.desirability <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE)) # -.014 95% CI: (-.07, -.04)

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "Eu",], weights = weight))
fit.list_eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(eu$branch_list_exp_r == T, eu$weight), data = all[all$continent == "Eu",], weights = 'weight', method = "lm")
fit.direct_eu <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "Eu",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_eu <- predict(fit.list_eu, direct.glm = fit.direct_eu, se.fit = TRUE)) # -.026 95% CI: (-.10, .05)

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "US",], weights = weight))
fit.list_us <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(us1$branch_list_exp_r == T, us1$weight), data = all[all$continent == "US",], weights = 'weight', method = "lm")
fit.direct_us <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "US",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_us <- predict(fit.list_us, direct.glm = fit.direct_us, se.fit = TRUE)) # -.026 95% CI: (-.11, -.06)

same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
                    filename = "reg_list_exp_g", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(all$gcs_support, weights = all$weight), 3), "} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\"))))

# /!\ Weighted list experiment regression is not supported (yet) for the multi-item design.
# fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, family = binomial("logit"))
# fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_ict', J = 2, data = all, method = "lm", multi.condition = "level")
# (avg.pred.social.desirability <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)


##### App Representativeness #####
representativeness_table(c("US1", "US2", "EU"), return_table = F, all = T) 
representativeness_table(countries_EU, return_table = F, all = T, weight_var = "weight_country") 


##### App Attrition analysis #####
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
           indep_vars = quotas_us) 
