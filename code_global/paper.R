##### Stated support #####
# TODO complete
# support_binary_positive
decrit("gcs_support", us1)
binconf(sum(us1$weight[us1$gcs_support == T]), nrow(us1), alpha = 0.05)
binconf(sum(eu$weight[eu$gcs_support == T]), nrow(eu), alpha = 0.05)
decrit("gcs_support", eu)
decrit("nr_support", us1)
decrit("nr_support", eu)
reweighted_estimate("gcs_support", "EU") # 76
reweighted_estimate("gcs_support", "US1") # 53 Assigns a weight 0 to vote_us = PNR/No right
reweighted_estimate("gcs_support", "US1", omit = "vote_us") # 52 Uses all observations and still reweight for vote using e$vote
decrit("gcs_support", us1, which = us1$voted)
decrit("gcs_understood", all)
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "Supports the Global Climate Scheme", covariates = c("gcs_understood"), covariate.labels = "\\makecell{With GCS, typical\\\\~[country] people lose\\\\and poorest humans win}",
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T, 
                    filename = "gcs_support_understood", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "Supports the Global Climate Scheme", covariates = variables_understood[c(2,1,3)], covariate.labels = "\\makecell{With GCS, typical\\\\~[country] people lose\\\\and poorest humans win}",# c("gcs_understood"),
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T, 
                    filename = "gcs_support_understood_all", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


# Global wealth tax
# global_tax_sharing_positive, global_tax_global_share_positive, global_tax_global_share_share TODO! combine them
decrit(us2$global_tax_support > 0, us2)
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'FR')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'DE')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'ES')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'UK') 
decrit(all$global_tax_support > 0, all) 
decrit(all$national_tax_support > 0, all)
decrit(us2$global_tax_support > 0, us2, which = us2$global_tax_support != 0) 
decrit(us2$national_tax_support > 0, us2, which = us2$national_tax_support != 0) 
decrit("global_tax_support", us2)
decrit("national_tax_support", eu)
decrit("global_tax_global_share", all, weight = F)
decrit("global_tax_global_share", us2) # 41
decrit(us2$global_tax_global_share > 0)
wtd.mean(eu$global_tax_global_share[eu$country == 'UK'] > 0, weights = eu$weight_country[eu$country == 'UK'])
decrit("global_tax_sharing", all, weight = F)
decrit("global_tax_sharing", us2) # 65% Yes
decrit("global_tax_support", eu, which = eu$country == "FR", weights = eu$weight_country)

# Other global policies
# support_likert_positive, support_likert_share
# support_likert_positive, global_policies_mean, global_policies_positive, global_policies_share

# Foreign aid 
decrit("foreign_aid_raise_support", all)
# healthcare, education
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education), d(c)$weight), 3)))
# taxes
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_income_tax | d(c)$foreign_aid_reduce_how_wealthy | d(c)$foreign_aid_reduce_how_corporations), d(c)$weight), 3)))

min_indifferent <- 1
max_indifferent <- 0
for (v in variables_support_likert) for (c in countries) {
  share_indifferent_vc <- wtd.mean(d(c)[["climate_compensation_support"]] == 0, d(c)$weight)
  if (share_indifferent_vc < min_indifferent) min_indifferent <- share_indifferent_vc
  if (share_indifferent_vc > max_indifferent) max_indifferent <- share_indifferent_vc
}
min_indifferent # 21%
max_indifferent # 27%

share_indifferent <- matrix(NA, dimnames = list(variables_support_likert, countries), nrow = 11, ncol = 5)
for (v in variables_support_likert) for (c in countries) share_indifferent[v,c] <- wtd.mean(d(c)[[v]] == 0, d(c)$weight)
round(quantile(share_indifferent, c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%  25%   50%  75%  95%  100% 
# 0.10 0.11 0.19 0.25 0.32 0.37 0.40

share_indifferent_oecd <- readRDS("../data/share_indifferent_oecd.rds")
round(quantile(share_indifferent_oecd, c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%   25%  50%  75%  95%  100% 
# 0.11 0.15 0.21 0.27 0.33 0.40 0.48 

round(quantile(c(share_indifferent_oecd[c(1,2,4),], share_indifferent[10:11,]), c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%   25%  50%  75%  95%  100% 
# 0.10 0.11 0.15 0.20 0.26 0.32 0.37 

# all/foreign_aid_condition

##### List experiment #####
# same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
#                     data = all, along = "continent", nolabel = F, include.total = FALSE, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
#                     filename = "reg_list_exp", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
#                     add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
#                                      c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))
# 
# same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
#                     data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
#                     filename = "reg_list_exp_all", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
#                     add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(all$gcs_support, weights = all$weight), 3), "} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
#                                      c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(all$nr_support, weights = all$weight), 3), "} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))
# 
# all$list_exp_0 <- all$list_exp == 0
# same_reg_subsamples(dep.var = "list_exp_0", dep.var.caption = "No supported policies", covariates = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), 
#                     data = all, along = "continent", nolabel = F, include.total = FALSE, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
#                     filename = "reg_list_exp_0", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
#                     add_lines = list(c(11, paste("\\textit{(Support for GCS)} & \\textit{", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "}\\\\")),
#                                      c(14, paste("\\textit{(Support for NR)} & \\textit{", round(wtd.mean(us1$nr_support, weights = us1$weight), 3), "} & \\textit{", round(wtd.mean(eu$nr_support, weights = eu$weight), 3), "}\\\\"))))
# 
# # TODO! Find a test that works for the combined list experiment branch_list_exp_g:branch_list_exp_r. Mail sent to Blair & Imai 
# # From Blair & Imai (12) /!\ TODO! Read the paper to understand whether "lm" method (yielding the same as lm regression) or the default "ml" is preferable
# fit.list_no_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
# fit.list_r <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
# fit.direct <- glm(gcs_support == 'Yes' ~ 1, data = all, family = binomial("logit"))
# (avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
# (avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
# 
# fit.list_no_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "U.S." & all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
# fit.list_r_US <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "U.S." & all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
# fit.direct_US <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "U.S.",], family = binomial("logit"))
# (avg.pred.social.desirability_no_r_US <- predict(fit.list_no_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # -.06 95% CI: (-.15, .03)
# (avg.pred.social.desirability_r_US <- predict(fit.list_r_US, direct.glm = fit.direct_US, se.fit = TRUE)) # .004 95% CI: (-.12, .13)
# 
# fit.list_no_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2, data = all[all$continent == "Europe" & all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
# fit.list_r_Eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 3, data = all[all$continent == "Europe" & all$branch_list_exp_r == T,], weights = 'weight', method = "ml")
# fit.direct_Eu <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "Europe",], family = binomial("logit"))
# (avg.pred.social.desirability_no_r_Eu <- predict(fit.list_no_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # .01 95% CI: (-.07, .09)
# (avg.pred.social.desirability_r_Eu <- predict(fit.list_r_Eu, direct.glm = fit.direct_Eu, se.fit = TRUE)) # -.10 95% CI: (-.17, -.03)
# 
# fit.list_no_r <- ictreg(list_exp ~ continent, treat = c('branch_list_exp_g'), J = 2, data = all[all$branch_list_exp_r == F,], weights = 'weight', method = "lm")
# fit.list_r <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 3, data = all[all$branch_list_exp_r == T,], weights = 'weight', method = "lm")
# fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all, family = binomial("logit"))
# (avg.pred.social.desirability_no_r <- predict(fit.list_no_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
# (avg.pred.social.desirability_r <- predict(fit.list_r, direct.glm = fit.direct, se.fit = TRUE)) # -.066 95% CI: (-.12, -.02)
# plot(avg.pred.social.desirability_no_r)
# plot(avg.pred.social.desirability_r)

# Good ones here
# Using ml is advised because the estimator is more efficient, i.e. has tighter CIs. But Graeme Blair wrote in an email "you can use the linear model".
# Using 80% CIs instead of the more common 95% may be a way to address the lower efficiency (which increases the SE by ~46% according to Blair (11, Figure 2), vs. 1.96/1.28=1+53% for taking 80% instead of 95% CIs)
# The estimates, SE, CIs and test (list == direct) do not depend on J if lm is used (I've checked the R code). 
# On the contrary, ml depends on J and cannot be used for our design (with a varying J).
# The lm model applying to a varying J is unbiased to the extent that the four branches are balanced (which they are almost perfectly).
# Also, contrary to the other models, the linear model (coefficient) has a clear interpretation.
# The difference test works by Monte Carlo (cf. below): by computing the difference of 10k draws of direct & indirect support (using their respective normal distributions) and computing the differences' mean and standard deviation.
summary(lm(list_exp ~ branch_list_exp_g*continent, data = all, weights = all$weight))
fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 2 + wtd.mean(all$branch_list_exp_r == T, all$weight), data = all, weights = 'weight', method = "lm")
fit.direct <- glm(as.character(gcs_support) == 'Yes' ~ continent, data = all[all$wave != "US2",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE, level = .8))

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "Europe",], weights = weight))
fit.list_eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(eu$branch_list_exp_r == T, eu$weight), data = all[all$continent == "Europe",], weights = 'weight', method = "lm")
fit.direct_eu <- glm(as.character(gcs_support) == 'Yes' ~ 1, data = all[all$continent == "Europe",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_eu <- predict(fit.list_eu, direct.glm = fit.direct_eu, se.fit = TRUE, level = .8)) 

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "U.S.",], weights = weight))
fit.list_us <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(us1$branch_list_exp_r == T, us1$weight), data = all[all$continent == "U.S.",], weights = 'weight', method = "lm")
fit.direct_us <- glm(as.character(gcs_support) == 'Yes' ~ 1, data = all[all$wave == "US1",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_us <- predict(fit.list_us, direct.glm = fit.direct_us, se.fit = TRUE, level = .8)) 

same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g"), share_na_remove = 0.5,
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
                    filename = "reg_list_exp_g", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\hline  \\\\[-1.8ex] \\textit{Support for GCS} &", round(wtd.mean(all$gcs_support[all$wave != "US2"], weights = all$weight[all$wave != "US2"]), 3), " & ", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), " & ", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "\\\\")),
                                     c(12, paste("\\textit{Social desirability bias} & \\textit{$", round(avg.pred.social.desirability$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_us$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_eu$fit[3,1], 3),  "$}\\\\")),
                                     c(13, paste("\\textit{80\\% C.I. for the bias} & \\textit{ $[", round(avg.pred.social.desirability$fit[3,2], 2), ";", round(avg.pred.social.desirability$fit[3,3], 2), "]$ } & \\textit{ $[", round(avg.pred.social.desirability_us$fit[3,2], 2), ";", round(avg.pred.social.desirability_us$fit[3,3], 2), "]$} & \\textit{ $[", round(avg.pred.social.desirability_eu$fit[3,2], 2), ";", round(avg.pred.social.desirability_eu$fit[3,3], 2), "]$}\\\\"))))
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


##### Petition ##### 
wtd.t.test(us1$gcs_support, us1$petition_gcs, weight=us1$weight, drops = "") # rejects equality (p=.046)
t.test(us1$gcs_support, us1$petition_gcs, paired = T) # rejects equality (p=.016)
t.test(us1$gcs_support, us1$petition_gcs, paired = F) # rejects equality (p=.019)
decrit("petition_gcs", us1) # TODO!!
(temp <- wtd.t.test(us1$petition_gcs[us1$branch_petition == "gcs"], us1$gcs_support[us1$branch_petition == "gcs"], weight=us1$weight[us1$branch_petition == "gcs"])) # cannot reject equality (p=.30)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2])
binconf(sum(us1$weight[us1$petition_gcs & us1$branch_petition == "gcs"]), sum(us1$weight[us1$branch_petition == "gcs"]), alpha = 0.05)
(temp <- wtd.t.test(us1$petition_nr[us1$branch_petition == "nr"], us1$nr_support[us1$branch_petition == "nr"], weight=us1$weight[us1$branch_petition == "nr"])) # cannot reject equality (p=.76)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2])
binconf(sum(us1$weight[us1$petition_nr & us1$branch_petition == "nr"]), sum(us1$weight[us1$branch_petition == "nr"]), alpha = 0.05)
(temp <- wtd.t.test(eu$petition_gcs[eu$branch_petition == "gcs"], eu$gcs_support[eu$branch_petition == "gcs"], weight=eu$weight[eu$branch_petition == "gcs"])) # rejects equality (p=1e-5)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2])
binconf(sum(eu$weight[eu$petition_gcs & eu$branch_petition == "gcs"]), sum(eu$weight[eu$branch_petition == "gcs"]), alpha = 0.05)
(temp <- wtd.t.test(eu$petition_nr[eu$branch_petition == "nr"], eu$nr_support[eu$branch_petition == "nr"], weight=eu$weight[eu$branch_petition == "nr"])) # rejects equality (p=.01)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2])
binconf(sum(eu$weight[eu$petition_nr & eu$branch_petition == "nr"]), sum(eu$weight[eu$branch_petition == "nr"]), alpha = 0.05)
decrit("petition_gcs", eu)
decrit("petition_nr", eu)


##### Conjoint analyses #####
# conjoint_ab_all_positive
decrit(us1$branch_conjoint_b)
decrit(eu$branch_conjoint_b)
decrit(eu$conjoint_crg_cr, eu)
wtd.t.test(eu$conjoint_rg_r_binary, eu$gcs_support, weight = eu$weight)
wtd.t.test(eu$conjoint_rg_r_binary, eu$gcs_support, weight = eu$weight, drops = "")
wtd.t.test(us1$conjoint_rg_r_binary, us1$gcs_support, weight = us1$weight)
wtd.t.test(us1$conjoint_rg_r_binary, us1$gcs_support, weight = us1$weight, drops = "")
wtd.t.test(us1$conjoint_cr_gr_binary, .5, weight = us1$weight)
wtd.t.test(eu$conjoint_cr_gr_binary, .5, weight = eu$weight)
wtd.t.test(us1$conjoint_cr_gr_binary, .5, weight = us1$weight)


##### Conjoint analysis: Left/Right vote #####
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

# Good one
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none_p", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, p_instead_SE = T)

summary(lm(conjoint_c ~ branch_c_gcs, all[all$conjoint_c_none == F ,], weights = weight))
CI(0.02756, 0.01333, 5200)
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F,], weights = weight)) # p-value: .13
CI(0.02860, 0.01900, 2617)
summary(lm(conjoint_c ~ branch_c_gcs, eu[eu$conjoint_c_none == F & eu$country == "FR",], weights = weight))
CI(0.11175, 0.03956, 603)
summary(lm(conjoint_c ~ branch_c_gcs, eu[eu$conjoint_c_none == F & eu$country == "DE",], weights = weight))
CI(0.01504, 0.03203, 811)
summary(lm(conjoint_c ~ branch_c_gcs, eu[eu$conjoint_c_none == F & eu$country == "UK",], weights = weight))
CI(0.007872, 0.038831, 659)
summary(lm(conjoint_c ~ branch_c_gcs, eu[eu$conjoint_c_none == F & eu$country == "ES",], weights = weight))
CI(-0.01477, 0.03772, 502)
summary(lm(gcs_support ~ swing_state, us1, weights = weight)) # .012, n=693
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state == T,], weights = weight)) # .012, n=693
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state_3pp == T,], weights = weight)) # .006, n=509
summary(lm(conjoint_c_right ~ branch_c_gcs, us1, weights = weight)) # p-value: .0504

# Interaction with political leaning:
summary(lm(conjoint_c ~ branch_c_gcs * vote_factor, d("FR")[d("FR")$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ branch_c_gcs * vote3, us1[us1$conjoint_c_none == F,], weights = weight))

same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), weights = "weight_vote", omit.note = T,
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_wo_none_weight_vote", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

for (i in c("left", "right", "none", "wo_none", "wo_none_weight_vote")) {
  temp <- readLines(paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
  writeLines(sub("United Kingdom", "UK", temp), paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
}

# The effect tends to be driven by right-wing voters
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = all, weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = all[all$conjoint_c_none == F & all$wave != "US2",], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = us1[us1$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$country=="FR" & eu$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$country=="FR" & eu$conjoint_c_none == F,], weights = weight))

# amce is created in conjoint_analysis.R
amce$UK$user.levels # same in all EU. 1: GCS, 2: tax, 3: assembly, 4: aid
amce$FR$estimates$foreignpolicy # 1: .13, 2: .11, 3: .12
amce$UK$estimates$foreignpolicy # 1: .09, 2: .13, 3: .07
amce$ES$estimates$foreignpolicy # 1: .04, 2: .05, 3:-.01
amce$DE$estimates$foreignpolicy # 1: .09, 2: .09, 3: .10
amce$us1$estimates$Foreignpolicy #1: .01, 2: .09, 3: .08

table_effects_amce <- matrix(NA, nrow = 15, ncol = 5, dimnames = list(paste0(countries, "; ", c(rep("Global Climate Plan", 5), rep("Global Millionaire Tax", 5), rep("Global Democratic Assembly on Climate Change", 5))), 
                              c("Effect", "N", "t", "p", "CI")))
for (i in 1:5) {
  indices <- if (i == 5) c(9, 11, 10) else 8:10
  temp <- summary(amce[[if (i == 5) "us1" else countries[i]]])
  table_effects_amce[c(i, i+5, i+10), "Effect"] <- paste0(round(temp$amce$Estimate[indices], 2), temp$amce$` `[indices])
  table_effects_amce[c(i, i+5, i+10), "N"] <- rep(temp$samplesize_estimates, 3)
  table_effects_amce[c(i, i+5, i+10), "t"] <- round(temp$amce$`z value`[indices], 2)
  table_effects_amce[c(i, i+5, i+10), "p"] <- round(temp$amce$`Pr(>|z|)`[indices], 3)
  table_effects_amce[c(i, i+5, i+10), "CI"] <- CI(temp$amce$Estimate[indices], temp$amce$`Std. Err`[indices], rep(temp$samplesize_estimates, 3), print = T)
} 
summary(amce$FR)$amce$`Pr(>|z|)`[8] # 0.00047
table_effects_amce[1, "p"] <- "$5\\cdot 10^{-4}$"
cat(sub("\\end{tabular}", "\\end{tabular}}", sub("\\centering", "\\makebox[\\textwidth][c]{", 
   paste(kbl(table_effects_amce, "latex", caption = "Average Marginal Component Effects of global policies.", 
             position = "h", escape = F, booktabs = T, align = "c", linesep = rep("", nrow(table_effects_amce)-1), 
             label = "amce", row.names = T,  format.args = list(decimal = ","),
             col.names = c("Effect", "Obs.", "t", "P-value", "95\\% C.I.")), # "Country; Policy", 
         collapse="\n"), fixed = T), fixed = T), file = "../tables/amce.tex") 



# Multiple hypotheses testing
pvalues_treatment <- list() # adjusted for multiple testing
for (c in c(countries, "all")) {
  pvalues_treatment[[c]] <- list()
  temp <- lm(conjoint_c ~ branch_c_gcs, data = d(c)[d(c)$conjoint_c_none == F & d(c)$wave != "US2",], weights = weight)
  pvalues_treatment[[c]] <- summary(temp)$coefficients[(length(summary(temp)$coefficients[,4])-2):length(summary(temp)$coefficients[,4]), 4]
  }
pvalues_treatment
sort(unlist(pvalues_treatment))
(adjusted_pvalues <- sort(p.adjust(unlist(pvalues_treatment), method = 'fdr')))


largest_effect_amce <- function(model) {
  max <- p_max <- -1
  for (d in names(model$estimates)) for (p in colnames(model$estimates[[d]])) if (model$estimates[[d]][1, p] > max) {
    max <- model$estimates[[d]][1, p]
    p_max <- p
  }
  names(max) <- model$user.levels[[p_max]]
  return(round(max, 3))
}

for (i in c("us1", countries_EU)) {
  print(i)
  print(largest_effect_amce(amce[[i]])) }

# conjoint_left_ag_b_binary_positive
decrit("conjoint_left_ag_b_binary", us1)
decrit("conjoint_left_ag_b_binary", us1, weight = F)
wtd.t.test(eu$conjoint_left_ag_b_binary[eu$country=='UK'], .5, weight = eu$weight_country[eu$country=='UK'])
wtd.t.test(eu$conjoint_left_ag_b_binary[eu$country=='ES'], .5, weight = eu$weight_country[eu$country=='ES'])


##### Prioritization #####
(mean_points <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(all[[v]], na.rm = T, weights = all$weight), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"])))) # GCS 9th, tax 4th, assembly 10, trillion inv 5, coal 13, ban ICE 15
(mean_points_us <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(us1[[v]], na.rm = T, weights = us1$weight), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"])))) # GCS 9th, tax 4th, assembly 10, trillion inv 5, coal 13, ban ICE 15
(mean_points_uk <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "UK"], na.rm = T, weights = eu$weight_country[eu$country == "UK"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "UK"])))) # tax 5, GCS 8, ass 10, insul 7, LEZ 11, ban 13
(mean_points_de <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "DE"], na.rm = T, weights = eu$weight_country[eu$country == "DE"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "DE"])))) # GCS 1, tax 2, ass 8, insul 5, solar 7, ban 14
(mean_points_fr <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "FR"], na.rm = T, weights = eu$weight_country[eu$country == "FR"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "FR"])))) # insul 3, GCS 4, tax 6, ass 8, LEZ 13, ban 15
(mean_points_es <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "ES"], na.rm = T, weights = eu$weight_country[eu$country == "ES"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "ES"])))) # 100% renew 4, tax 5, GCS 8, insul 10, ass 11, ban 15
decrit("points_foreign1_gcs", data = all) # mean: 17.7 / median: 20
decrit("points_foreign1_gcs", data = eu) # mean: 19.75 / median: 19
decrit("points_foreign1_gcs", data = us1) # mean: 15.4 / median: 11
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'DE') # mean: 23.3 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'FR') # mean: 20.9 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'ES') # mean: 15.7 / median: 13
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'UK') # mean: 17 / median: 16
decrit("points_foreign2_tax_rich", data = us1) # mean: 20.6 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'DE') # mean: 22.6 / median: 20
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'FR') # mean: 20.1 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'ES') # mean: 19.4 / median: 16
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'UK') # mean: 19.6 / median: 16


##### Pros and cons #####
decrit(all$gcs_field_pro | all$gcs_field_con, all)
reg_pros_cons <- lm(gcs_support ~ branch_gcs, us2, weights = weight)
summary(reg_pros_cons)
confint(reg_pros_cons)
summary(lm(nr_support ~ branch_gcs, us2, weights = weight))
desc_table(c("gcs_support", "gcs_support", "nr_support", "nr_support"), filename = "branch_gcs", data = us2, indep_vars = c("branch_gcs", covariates), indep_vars_included = list("branch_gcs", c("branch_gcs", covariates), "branch_gcs", c("branch_gcs", covariates)), mean_control = T, model.numbers = T, #!mean_above,
           dep.var.labels = c("Global Climate Scheme", "National Redistribution"), dep.var.caption = c("Support"), digits= 3, robust_SE = T, omit = c("Constant", "Race: Other"), mean_above = T, only_mean = F, keep = "branch_gcs", save_folder = "../tables/US2/", nolabel = F, 
           add_lines = list(c(18, "Includes controls &  & \\checkmark &  & \\checkmark \\\\")))
reg_info_support <- lm(gcs_support ~ info_support, data = us2, weights = weight)
summary(reg_info_support)
confint(reg_info_support)


##### Second-order beliefs ##### 
decrit("gcs_belief", data = us1)
decrit("gcs_belief", data = eu)
decrit(eu$gcs_belief > 50, weights = eu$weight)
decrit(eu$gcs_belief < 76, data = eu) # 78%
decrit(us1$gcs_belief < 54, data = us1) # 53%


##### Universalism #####
# group_defended_agg, group_defended_agg2
decrit("group_defended_agg5", data = all)
decrit("group_defended_agg", data = all, which = all$vote != 0)
decrit("group_defended_agg", data = eu, which = eu$vote != 0)
decrit("group_defended_agg", data = us1, which = us1$vote != 0)
decrit("group_defended_agg", data = all, which = all$vote == -1)
decrit("group_defended_agg", data = eu, which = eu$vote == -1)
decrit("group_defended_agg", data = us1, which = us1$vote == -1)
# all/negotiation
decrit("negotiation", data = all)
wtd.mean(all$problem_climate, weights = all$weight)
wtd.mean(all$problem_poverty, weights = all$weight)
wtd.mean(all$problem_inequality, weights = all$weight)


##### Donation #####
decrit("donation", all)
same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation", "vote_factor", "branch_donation:vote_factor"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_covariates", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation", "vote_factor", "branch_donation:vote_factor"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, keep = c("branch_donation"),
                    filename = "donation_interaction", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

desc_table(dep_vars = "donation", filename = "donation_interaction", data = list(all, us1, us1, eu), dep.var.labels = c("All", "US", "US", "Eu"), dep.var.caption = "Donation to poor people (in \\%)",
           indep_vars = c("branch_donation", "vote_Biden", "branch_donation:vote_Biden"), model.numbers = F, multicolumn = F, mean_above = F, 
           indep_vars_included = list(c(T,F,F), c(T,F,F), c(T,T,T), c(T,F,F)), weights = "weight", save_folder = "../tables/continents/", robust_SE = FALSE, omit = c("Constant", "^  Vote", "^  vote"))

  desc_table(dep_vars = "donation", filename = "donation_interaction", data = list(all, us1, us1, eu), dep.var.labels = c("All", "US", "US", "Eu"), dep.var.caption = "Donation to poor people (in \\%)",
           indep_vars = c("branch_donation", "vote_not_Biden", "branch_donation:vote_not_Biden"), model.numbers = F, multicolumn = F,  mean_above = F, 
           indep_vars_included = list(c(T,F,F), c(T,F,F), c(T,T,T), c(T,F,F)), weights = "weight", save_folder = "../tables/continents/", robust_SE = FALSE, omit = c("Constant", "^  Vote", "^  vote"))

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


##### Methods #####
decrit("survey_biased", data = all)


##### App Determinants #####
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "\\makecell{Supports the Global Climate Scheme}", covariates = covariates, 
                    data_list = list(all, us, eu, d("DE"), d("FR"), d("UK"), d("ES")), dep_var_labels = c("All", "United States", "Europe", countries_names), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "gcs_support", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### App Representativeness #####
representativeness_table(c("US1", "US2", "Eu"), return_table = F, all = T) 
representativeness_table(countries_EU, return_table = F, all = T, weight_var = "weight_country") 


##### App Attrition analysis #####
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US1/", data = c(list(us1a), list(us1a), list(us1a[us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US2/", data = c(list(us2a), list(us2a), list(us2a[us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 6"),weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/EU/", data = c(list(eua), list(eua), list(eua[eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,])), 
           indep_vars = c(quotas_eu, "vote_factor")) 


##### App Placebo tests #####
# desc_table(dep_vars = c("conjoint_a", "cgr_support", "petition", "share_policies_supported", "conjoint_left_ag_b_binary"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
#            dep.var.labels = c("\\makecell{G+R+C > R+C}", "\\makecell{Support\\G+R+C}", "\\makecell{Signs\\petition}", "\\makecell{Share policies\\supported}", "\\makecell{Conjoint 5\\A+CGS > B}"),
#            filename = "placebo_tests_eu", save_folder = "../tables/", data = eu, indep_vars = c("branch_list_exp", "branch_petition", "branch_donation", "branch_foreign_aid_preferred", "branch_gcs_field")) 
# 
# desc_table(dep_vars = c("conjoint_a", "cgr_support", "petition", "share_policies_supported", "conjoint_left_ag_b_binary"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
#            dep.var.labels = c("\\makecell{G+R+C > R+C}", "\\makecell{Support\\G+R+C}", "\\makecell{Signs\\petition}", "\\makecell{Share policies\\supported}", "\\makecell{Conjoint 5\\A+CGS > B}"),
#            filename = "placebo_tests_us1", save_folder = "../tables/", data = us1, indep_vars = c("branch_list_exp", "branch_petition", "branch_donation", "branch_foreign_aid_preferred", "branch_gcs_field")) 

desc_table(dep_vars = c("conjoint_a", "cgr_support", "petition", "share_policies_supported", "conjoint_left_ag_b_binary"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
           dep.var.labels = c("\\makecell{G+R+C\\\\preferred to\\\\R+C}", "\\makecell{Supports\\\\G+R+C}", "\\makecell{Signs\\\\petition}", "\\makecell{Share of\\\\policies\\\\supported}", "\\makecell{Conjoint 5\\\\A+CGS\\\\preferred to B}"),
           filename = "placebo_tests", save_folder = "../tables/", data = all, indep_vars = c("branch_list_exp", "branch_petition", "branch_donation")) 


##### App Balance analysis #####
desc_table(dep_vars = c("branch_list_exp_g", "branch_petition == 'nr'", "branch_donation == 'Own nation'", "branch_conjoint_c == 'leftg_right'"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{List contains: G}", "\\makecell{Branch petition: NR}", "\\makecell{Branch donation: Own nation}", "\\makecell{Branch conjoint 3: with GCS}"),
           filename = "balance_analysis", save_folder = "../tables/", data = all, indep_vars = c(socio_demos)) 


##### App Extended sample #####
# Preparation
e <- merge(merge(us1a, us2a, all = T), eua, all = T)
e <- e[e$stayed == T,]
nrow(e)
1-nrow(all)/nrow(e) # 14%
e$gcs_support_neg <- 2*e$gcs_support - 1
for (v in variables_support_likert) {
  if (v %in% names(e)) {
    temp <-  temp <- 2 * (e[[v]] %in% text_support[5]) + (e[[v]] %in% text_support[4]) - (e[[v]] %in% text_support[2]) - 2 * (e[[v]] %in% text_support[1])
    temp[is.na(e[[v]])] <- NA
    e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")), missing.values=c(NA), annotation=Label(e[[v]])) 
  } }
e$global_tax_sharing_original <- e$global_tax_sharing
e$global_tax_sharing <- NA
e$global_tax_sharing[grepl("whole wealth tax financing national budgets", e$global_tax_sharing_original)] <- FALSE
e$global_tax_sharing[grepl("half of it financing low-income countries", e$global_tax_sharing_original)] <- T
e$foreign_aid_raise_support_original <- e$foreign_aid_raise_support
temp <- -1 * grepl("reduce", e$foreign_aid_raise_support) + 1*grepl("condition", e$foreign_aid_raise_support) + 2*grepl("increase", e$foreign_aid_raise_support)
e$foreign_aid_raise_support <- as.item(temp, labels = structure(-1:2, names = c("No, should be reduced", "No, should remain stable", "Yes, but at some conditions", "Yes, should be increased")), missing.values = NA, annotation = Label(e$foreign_aid_raise_support))     
e$group_defended_original <- e$group_defended
temp <- 0*grepl("myself", e$group_defended) + 1*grepl("relatives", e$group_defended) + 2*grepl("town|State", e$group_defended) + 3*grepl("religion", e$group_defended) + 4*grepl("Americans", e$group_defended) + 5*grepl("European", e$group_defended) + 6*grepl("Humans", e$group_defended) + 7*grepl("animals", e$group_defended)
e$group_defended <- as.item(temp, labels = structure(0:7, names = c("Family and self", "Relatives", "Region, U.S. State or town", "Culture or religion", "Fellow citizens", "Europeans", "Humans", "Sentient beings")), annotation = Label(e$group_defended))
e$group_defended[is.na(e$group_defended_original)] <- NA
e$universalist <- e$group_defended > 5
e$conjoint_left_ag_b_binary <- e$conjoint_left_ag_b == "A"

for (v in intersect(names(e), c(variables_conjoint))) {
  e[[v]] <- sub(".* (.*)", "\\1", e[[v]])
  e[[v]][e[[v]] == "them"] <- "None"
  if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Democrat", "A")] <- "Left"
  if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Republican", "B")] <- "Right"
  if (v %in% variables_conjoint_c) e$conjoint_c[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "Left" 
  if (v %in% variables_conjoint_c) e$conjoint_c_none[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "None"
  if (v %in% variables_conjoint_c) e$branch_conjoint_c[!is.na(e[[v]])] <- sub("conjoint_", "", v)
}
e$branch_c_gcs <- grepl("g_", e$branch_conjoint_c)
e$weight <- 1

e$branch_list_exp[!is.na(e$list_exp_l)] <- "l"
e$branch_list_exp[!is.na(e$list_exp_rgl)] <- "rgl"
e$branch_list_exp[!is.na(e$list_exp_gl)] <- "gl"
e$branch_list_exp[!is.na(e$list_exp_rl)] <- "rl"
e$branch_list_exp_g <- grepl("g", e$branch_list_exp)
e$branch_list_exp_r <- grepl("r", e$branch_list_exp)
for (v in c("l", "rl", "gl", "rgl")) e$list_exp[no.na(e$branch_list_exp) == v] <- e[[paste0("list_exp_", v)]][no.na(e$branch_list_exp) == v]

# Plot of main results
heatmap_multiple(heatmaps_defs[c("main_all")], weights = F, data = e, name = "main_alla") 
heatmap_multiple(heatmaps_defs[c("conjoint_left_ag_b_binary")], weights = F, data = e, name = "conjoint_left_ag_b_binary_alla") 

same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), along.levels = c("United States", names(countries_eu)[1:4]), share_na_remove = 1,
                    data = e[e$conjoint_c_none == F & e$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none_alla", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

summary(lm(list_exp ~ branch_list_exp_g*continent, data = e))
fit.list_ <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 2 + mean(e$branch_list_exp_r == T), data = e, method = "lm")
fit.direct_ <- glm(as.character(gcs_support) == 'Yes' ~ continent, data = e[e$wave != "US2",], family = binomial("logit"))
(avg.pred.social.desirability_ <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE, level = .8))

summary(lm(list_exp ~ branch_list_exp_g, data = e[e$continent == "Europe",], weights = weight))
fit.list_eu_ <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(e$branch_list_exp_r == T, e$wave == "EU"), data = e[e$continent == "Europe",], method = "lm")
fit.direct_eu_ <- glm(as.character(gcs_support) == 'Yes' ~ 1, data = e[e$continent == "Europe",], family = binomial("logit"))
(avg.pred.social.desirability_eu_ <- predict(fit.list_eu, direct.glm = fit.direct_eu, se.fit = TRUE, level = .8)) 

summary(lm(list_exp ~ branch_list_exp_g, data = e[e$continent == "U.S.",], weights = weight))
fit.list_us_ <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(e$branch_list_exp_r == T, e$wave == "US1"), data = e[e$continent == "U.S.",], method = "lm")
fit.direct_us_ <- glm(as.character(gcs_support) == 'Yes' ~ 1, data = e[e$wave == "US1",], family = binomial("logit"))
(avg.pred.social.desirability_us_ <- predict(fit.list_us, direct.glm = fit.direct_us, se.fit = TRUE, level = .8)) 

same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g"), share_na_remove = 0.5,
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
                    filename = "reg_list_exp_g_alla", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\hline  \\\\[-1.8ex] \\textit{Support for GCS} &", round(mean(e$gcs_support[e$wave != "US2"]), 3), " & ", round(wtd.mean(e$gcs_support, weights = e$wave == "US1"), 3), " & ", round(wtd.mean(e$gcs_support, weights = e$wave == "EU"), 3), "\\\\")),
                                     c(12, paste("\\textit{Social desirability bias} & \\textit{$", round(avg.pred.social.desirability_$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_us_$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_eu_$fit[3,1], 3),  "$}\\\\")),
                                     c(13, paste("\\textit{80\\% C.I. for the bias} & \\textit{ $[", round(avg.pred.social.desirability_$fit[3,2], 2), ";", round(avg.pred.social.desirability_$fit[3,3], 2), "]$ } & \\textit{ $[", round(avg.pred.social.desirability_us_$fit[3,2], 2), ";", round(avg.pred.social.desirability_us_$fit[3,3], 2), "]$} & \\textit{ $[", round(avg.pred.social.desirability_eu_$fit[3,2], 2), ";", round(avg.pred.social.desirability_eu_$fit[3,3], 2), "]$}\\\\"))))


##### App questionnaire framing #####

desc_table(dep_vars = c("universalist", "nationalist", "egoistic"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
          dep.var.caption = "Group defended when voting",
           dep.var.labels = c("Humans \\textit{or} Sentient beings", "Fellow citizens", "Family and self"),
           filename = "ordering_us", save_folder = "../tables/", data = all[all$wave != "EU",], indep_vars = c("wave")) 














