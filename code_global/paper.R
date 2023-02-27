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

# TODO! Find a test that works for the combined list experiment branch_list_exp_g:branch_list_exp_r, cf. Blair & Imai (12) sections 2.2, 2.3, 2.5.2
# From Blair & Imai (12)
fit.list_no_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "ml")
fit.list_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
fit.direct <- glm(gcs_support == 'Yes' ~ 1, data = all, family = binomial("logit"))
(avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
(avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)

fit.list_no_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "US" & all$branch_list_exp_r == F,], weights = 'weight', method = "ml")
fit.list_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "US" & all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
fit.direct_US <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "US",], family = binomial("logit"))
(avg.pred.social.desirability_no_r_US <- predict(fit.list_no_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # -.064 95% CI: (-.13, .007)
(avg.pred.social.desirability_r_US <- predict(fit.list_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # -.02 95% CI: (-.09, .05)

fit.list_no_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "Eu" & all$branch_list_exp_r == F,], weights = 'weight', method = "ml")
fit.list_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "Eu" & all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
fit.direct_Eu <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "Eu",], family = binomial("logit"))
(avg.pred.social.desirability_no_r_Eu <- predict(fit.list_no_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # -.058 95% CI: (-.13, .01)
(avg.pred.social.desirability_r_Eu <- predict(fit.list_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # -.126 95% CI: (-.20, -.05)

fit.list_no_r <- ictreg(list_exp ~ continent, treat = c('branch_list_exp_g'), J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "ml")
fit.list_r <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, family = binomial("logit"))
(avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
(avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
plot(avg.pred.social.desirability_no_r)
plot(avg.pred.social.desirability_r)


