# TODO explain: figures in render.R, conjoint in ..., variables Label()
start <- Sys.time()

##### Prepare data and figures #####
# source("preparation.R")
# source("render.R")
# save.image("after_preparation.RData")


##### Load prepared data #####
load("after_preparation.RData")

# Figure 1: questionnaire/survey_flow-simple.pdf

share_indifferent_oecd <- readRDS("../data/share_indifferent_oecd.rds")
round(quantile(share_indifferent_oecd, c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%   25%  50%  75%  95%  100%
# 0.11 0.15 0.21 0.27 0.33 0.40 0.48

##### Global support #####
# Figure 2: figures/OECD/Heatplot_global_tax_attitudes_share.pdf, cf. code at https://doi.org/10.3886/E208254V1

##### Stated support for the Global Climate Scheme #####
# box The Global Climate Scheme
# median loss from GCS in 2030 (LCU): cf. questionnaire/specificities.xlsx:Figures (line 6)

decrit("gcs_support", us1) # 54% of Americans support the GCS
decrit("gcs_support", eu) # 76% of Europeans support the GCS
decrit("nr_support", eu) # 73%

# Figure 3: figures/country_comparison/support_likert_all_share.pdf
heatmap_multiple(heatmaps_defs["support_likert_all"])


##### List experiment #####

# Table 1: tables/continents/reg_list_exp_g.tex
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


##### Petition ##### 
(temp <- wtd.t.test(us1$petition_gcs[us1$branch_petition == "gcs"], us1$gcs_support[us1$branch_petition == "gcs"], 
                    weight=us1$weight[us1$branch_petition == "gcs"])) # cannot reject equality (p=.30)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2]) # -.02 [-.05, .02], t(3,044)=1.0
(temp <- wtd.t.test(us1$petition_nr[us1$branch_petition == "nr"], us1$nr_support[us1$branch_petition == "nr"], 
                    weight=us1$weight[us1$branch_petition == "nr"])) # cannot reject equality (p=.76)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2]) # -.1 [-.04, .03],  t(2952)=.76
(temp <- wtd.t.test(eu$petition_gcs[eu$branch_petition == "gcs"], eu$gcs_support[eu$branch_petition == "gcs"], 
                    weight=eu$weight[eu$branch_petition == "gcs"])) # rejects equality (p=1e-5)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2]) # -.07 [-.10, -.04], t(3018)= -4.4
(temp <- wtd.t.test(eu$petition_nr[eu$branch_petition == "nr"], eu$nr_support[eu$branch_petition == "nr"], 
                    weight=eu$weight[eu$branch_petition == "nr"])) # rejects equality (p=.008)
CI(temp$additional[1], temp$additional[4], temp$coefficients[2]) # -.04 [.08, .01], t(2953)= -2.6

decrit("petition_gcs", eu) # 69%
decrit("petition_nr", eu) # 67%


##### Conjoint analyses #####

# 3rd conjoint analysis
# Table 2: tables/country_comparison/conjoint_c_wo_none_stats.tex
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

# P-values, t, and C.I. are added manually to the table exported above
reg_left_gcs_p_values <- "\textit{P-value}"
reg_left_gcs_t <- "\textit{t}"
reg_left_gcs_ci <- "\textit{95% C.I.}"
for (k in c("all", "us1", countries_EU[c(1,2,4,3)])) {
  reg_left_gcs_k <- lm(conjoint_c ~ branch_c_gcs, d(k)[d(k)$conjoint_c_none == F ,], weights = weight)
  reg_left_gcs_p_values <- paste0(reg_left_gcs_p_values, " & \textit{", sprintf("%.3f", coeftest(reg_left_gcs_k , vcov = vcovHC(reg_left_gcs_k , "HC2"))[2,4]), "}")
  reg_left_gcs_t <- paste0(reg_left_gcs_t, " & \textit{", sprintf("%.2f", coeftest(reg_left_gcs_k , vcov = vcovHC(reg_left_gcs_k , "HC2"))[2,3]), "}")
  reg_left_gcs_ci <- paste0(reg_left_gcs_ci, " & \textit{[", paste(sprintf("%.2f", CI(coeftest(reg_left_gcs_k , vcov = vcovHC(reg_left_gcs_k , "HC2"))[2,1], coeftest(reg_left_gcs_k , vcov = vcovHC(reg_left_gcs_k , "HC2"))[2,2], reg_left_gcs_k$df.residual)), collapse = '; '), "]}")
}
reg_left_gcs_ci <- gsub("0\\.", ".", gsub("-", "$-$", reg_left_gcs_ci))
paste(c(reg_left_gcs_p_values, reg_left_gcs_t, reg_left_gcs_ci), collapse = "\\")

# 4th conjoint analysis
# amce is created in code_global/conjoint_analysis.R
amce$UK$user.levels # same in all EU. 1: GCS, 2: tax, 3: assembly, 4: aid
amce$FR$estimates$foreignpolicy # 1: .13, 2: .11, 3: .12
amce$UK$estimates$foreignpolicy # 1: .09, 2: .13, 3: .07
amce$ES$estimates$foreignpolicy # 1: .04, 2: .05, 3:-.01
amce$DE$estimates$foreignpolicy # 1: .09, 2: .09, 3: .10
amce$us1$estimates$Foreignpolicy #1: .01, 2: .09, 3: .08
# max between .15-.18 except in Spain (.27)
for (c in c("us1", countries_EU)) print(paste(c, round(max(sapply(names(amce[[c]]$estimates), function(k) max(amce[[c]]$estimates[[k]][1,]))), 2)))

# 5th conjoint analysis
decrit("conjoint_left_ag_b_binary", us1) # 58%
decrit("conjoint_left_ag_b_binary", eu) # 60%


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
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'DE', weights = eu$weight_country) # mean: 22.9 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'FR', weights = eu$weight_country) # mean: 20.2 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'ES', weights = eu$weight_country) # mean: 15.8 / median: 13
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'UK', weights = eu$weight_country) # mean: 17.4 / median: 16
decrit("points_foreign2_tax_rich", data = us1) # mean: 20.6 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'DE', weights = eu$weight_country) # mean: 22.92 / median: 20
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'FR', weights = eu$weight_country) # mean: 20.2 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'ES', weights = eu$weight_country) # mean: 18.9 / median: 16
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'UK', weights = eu$weight_country) # mean: 19.9 / median: 16


##### Pros and cons #####
round(sort(sapply(variables_gcs_field_contains, function(v) wtd.mean(all[[v]], all$weight))), 2) # world: 23%, envi: 22%, poorest: 10%; cost: 8%
round(sort(sapply(variables_gcs_field_names, function(v) wtd.mean(all[[v]], all$weight))), 2) # envi: 30%, obstacles to get agreement: 9%, obstacles implementation: 5%
reg_pros_cons <- lm(gcs_support ~ branch_gcs, us2, weights = weight)
summary(reg_pros_cons) # -.11, t(1996)=-3.5, p=5e4=-1 when facing question on important pros and cons
confint(reg_pros_cons) # [-.17, .05]


##### box Second-order beliefs ##### 
decrit("gcs_belief", data = us1) # 52% (36, 52, 68)
decrit("gcs_belief", data = eu) # 59% (43, 61, 74)
decrit(eu$gcs_belief > 50, weights = eu$weight) # 65%
decrit(eu$gcs_belief < 76, data = eu) # 78%
decrit(us1$gcs_belief < 54, data = us1) # 53%
reg_info_support <- lm(gcs_support ~ info_support, data = us2, weights = weight)
summary(reg_info_support) # .025 t(1998)=1.1
confint(reg_info_support) # [-.02, .07]


##### Stated support for global redistribution #####

# Figure 3: figures/country_comparison/support_likert_all_share.pdf
share_indifferent <- matrix(NA, dimnames = list(variables_support_likert, countries), nrow = 11, ncol = 5)
for (v in variables_support_likert) for (c in countries) share_indifferent[v,c] <- wtd.mean(d(c)[[v]] == 0, d(c)$weight)
round(quantile(share_indifferent, c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%  25%   50%  75%  95%  100%
# 0.10 0.11 0.19 0.25 0.32 0.37 0.40

decrit(all$global_tax_global_share > 0, weights = F)
decrit(all$global_tax_sharing, weights = F)


# Foreign aid
decrit("foreign_aid_raise_support", all) # 16%, 45%+17%=62%
for (v in variables_foreign_aid_condition) print(paste(v, round(wtd.mean(all[[v]], weights = all$weight), 2)))
for (v in variables_foreign_aid_no) print(paste(v, round(wtd.mean(all[[v]], weights = all$weight), 2)))

round(wtd.mean(all$poverty_field_give_money | all$poverty_field_education | all$poverty_field_food | all$poverty_field_jobs | all$poverty_field_unspecified_aid, weights = all$weight), 2) # 52% if Americans express more aid is needed
for (v in c(variables_poverty_field_names, "poverty_field_empty")) print(paste(v, round(wtd.mean(all[[v]], weights = all$weight), 2)))
for (v in variables_poverty_field_contains) print(paste(v, round(wtd.mean(all[[v]], weights = all$weight), 2)))


##### Universalistic values #####
# group_defended_agg, group_defended_agg2
decrit("group_defended_agg5", data = all) # 20%, 22%, 33%, 15%


##### Methods: Data collection #####
decrit(us1$date) # n=3000, from 2023-01-11 to 2023-03-10
decrit(us2$date) # n=200, from 2023-03-10 to 2023-04-12
decrit(eu$date) # n=3000, from 2023-02-10 to 2023-03-24


##### Methods: Data quality #####
decrit(us1$duration) # 14'
decrit(us2$duration) # 11'
decrit(eu$duration) # 20'
decrit("survey_biased", data = all) # 69% No, 24% left / 8% right


##### Methods: Questionnaires and raw results #####
# cf. questionnaire/ for questionnaire files (in .qsf or exported to .docx) and figures/sources used in the questionnaires in specificities.xlsx
# cf. code_global/preparation.R (lines ~1300) for the export of country-specific raw results


##### Methods: Support for the GCS #####
binconf(sum(us1$weight[us1$gcs_support == T]), nrow(us1), alpha = 0.05) # Margin of error is +/- 2pp
binconf(sum(eu$weight[eu$gcs_support == T]), nrow(eu), alpha = 0.05)
reweighted_estimate("gcs_support", "EU") # 76%
reweighted_estimate("gcs_support", "US1") # 53% Assigns a weight 0 to vote_us = PNR/No right
reweighted_estimate("gcs_support", "US1", omit = "vote_us") # 52% Uses all observations and still reweight for vote using e$vote
decrit("gcs_support", us1, which = us1$voted) # 54%
decrit("gcs_support", us1, which = us1$voted & us1$vote_agg == "Biden") # 74%
decrit("gcs_support", us1, which = us1$voted & us1$vote_agg == "Trump") # 26%
decrit("gcs_support", us1, which = us1$swing_state) # 51%
summary(lm(gcs_support ~ swing_state, us1, weights = weight)) # -.04* p: .05
summary(lm(gcs_support ~ swing_state_3pp, us1, weights = weight)) # -.06*** p: .01
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state == T,], weights = weight)) # .012, p: .74, n=693
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state_3pp == T,], weights = weight)) # .006, p: .089, n=509


##### Methods: Petition #####
t.test(us1$gcs_support, us1$petition_gcs, paired = T) # rejects equality (p=.016)
wtd.t.test(us1$gcs_support, us1$petition_gcs, weight=us1$weight, drops = "") # rejects equality (p=.046)
decrit("petition_gcs", us1) # 51%


##### Methods: Conjoint analyses #####
# 1st conjoint analysis
# conjoint_ab_all_positive
decrit("conjoint_crg_cr", us) # 55%
decrit("conjoint_crg_cr", eu) # 74%

# 2nd conjoint analysis
# Number of obs. by branch
decrit(us1$branch_conjoint_b) # rg_r: n=757, rc_r: 751, cr_gr: 721, r_rcg: 771
decrit(eu$branch_conjoint_b) # rg_r: n=746, rc_r: 747, cr_gr: 741, r_rcg: 766
# 1st branch: NR+GCS vs. NR
decrit("conjoint_rg_r", us) # 55%
decrit("conjoint_rg_r", eu) # 77%
wtd.t.test(all$conjoint_rg_r_binary, all$gcs_support, weight = all$weight) # p: .24
wtd.t.test(eu$conjoint_rg_r_binary, eu$gcs_support, weight = eu$weight) # p: .44
wtd.t.test(us1$conjoint_rg_r_binary, us1$gcs_support, weight = us1$weight) # p: .36
# 2nd branch: NR+C vs. NR
decrit("conjoint_rc_r", us) # 62%
decrit("conjoint_rc_r", eu) # 84%
wtd.t.test(all$conjoint_rc_r_binary, all$gcs_support, weight = all$weight) # .06*** p: 6e-4
wtd.t.test(eu$conjoint_rc_r_binary, eu$gcs_support, weight = eu$weight) # .08*** p: 2e-4
wtd.t.test(us1$conjoint_rc_r_binary, us1$gcs_support, weight = us1$weight) # .04 p: .14
# 3rd branch: C+NR vs. GCS+NR
decrit("conjoint_cr_gr", us) # C>GCS:   53%
decrit("conjoint_cr_gr", eu) # GCS>C: 52%
wtd.t.test(us1$conjoint_cr_gr_binary, .5, weight = us1$weight) # p: .14
wtd.t.test(eu$conjoint_cr_gr_binary, .5, weight = eu$weight) # p; .40
# 4th branch: NR vs. GCS+C+NR
decrit("conjoint_r_rcg", us) # 55%
decrit("conjoint_r_rcg", eu) # 77%

# 4th conjoint analysis: prepared in code_global/conjoint_analysis.R


##### Methods: Prioritization #####
mean_points_us # Ban combustion-engine cars: 10.6 (3rd to last)
mean_points_fr # Ban combustion-engine cars: 7.8 (last)
mean_points_de # Ban combustion-engine cars: 7.9 (last)
mean_points_es # Ban combustion-engine cars: 8.9 last)
mean_points_uk # Ban combustion-engine cars: 11.4 (3rd to last)


##### Methods: Open-ended question on the GCS #####
# Open-ended fields are manually recoded in data/fields/[Country].xlsm
decrit(all$gcs_field_pro | all$gcs_field_con, all) # 27% cites pros or cons
round(sort(sapply(variables_gcs_field_names, function(v) wtd.mean(all[[v]], all$weight))), 2) # 7% support, 4% oppose, 24% unclassifiable
decrit(all$gcs_field_empty | all$gcs_field_dont_know, all) # 11% empty or don't know


##### Methods: Pros and cons #####
decrit(us$branch_gcs_important) # n=493
decrit(eu$branch_gcs_perception) # n=1505
round(sort(sapply(variables_gcs_important, function(v) wtd.mean(eu[[v]] > 0, eu$weight))), 2) # 60% hurts me to 85% limit CC; reduce poverty in LICs
round(sort(sapply(variables_gcs_important, function(v) wtd.mean(us[[v]] > 0, us$weight))), 2) # 75% hurts me to 89% having info; 82% foster global cooperation

summary(lm(nr_support ~ branch_gcs, us2, weights = weight)) # -.07** 
desc_table(c("gcs_support", "gcs_support", "nr_support", "nr_support"), filename = "branch_gcs", data = us2, indep_vars = c("branch_gcs", covariates), indep_vars_included = list("branch_gcs", c("branch_gcs", covariates), "branch_gcs", c("branch_gcs", covariates)), mean_control = T, model.numbers = T, #!mean_above,
           dep.var.labels = c("Global Climate Scheme", "National Redistribution"), dep.var.caption = c("Support"), digits= 3, robust_SE = T, omit = c("Constant", "Race: Other"), mean_above = T, only_mean = F, keep = "branch_gcs", save_folder = "../tables/US2/", nolabel = F,
           add_lines = list(c(18, "Includes controls &  & \\checkmark &  & \\checkmark \\\\")))


##### Methods: Universalistic values #####
decrit("negotiation", data = all) # 11% only countr's interest, 24+6=30% favor global justice, 38% [Country] then global
wtd.mean(all$problem_climate, weights = all$weight) # .58
wtd.mean(all$problem_poverty, weights = all$weight) # .40
wtd.mean(all$problem_inequality, weights = all$weight) # 35
summary(lm(donation ~ branch_donation, data = us, weights = weight)) # .03**
summary(lm(donation ~ (branch_donation=='Africa') + (vote3_factor!='Biden') + (branch_donation=='Africa'):(vote3_factor!='Biden'), data = us, weights = weight)) # entirely driven by non-Biden voters


##### Methods: Global wealth tax estimates #####
# cf. questionnaire/specificities.xlsx:Wealth tax (column G) for computations of wealth tax revenues (for LICs, we apply revenue over GDP of SSA; for U.S. we apply North America)


##### Extended data #####
# Table S1: tables/US2/branch_gcs.tex
desc_table(c("gcs_support", "gcs_support", "nr_support", "nr_support"), filename = "branch_gcs", data = us2, indep_vars = c("branch_gcs", covariates), indep_vars_included = list("branch_gcs", c("branch_gcs", covariates), "branch_gcs", c("branch_gcs", covariates)), mean_control = T, model.numbers = T, #!mean_above,
           dep.var.labels = c("Global Climate Scheme", "National Redistribution"), dep.var.caption = c("Support"), digits= 3, robust_SE = T, omit = c("Constant", "Race: Other"), mean_above = T, only_mean = F, keep = "branch_gcs", save_folder = "../tables/US2/", nolabel = F, 
           add_lines = list(c(18, "Includes controls &  & \\checkmark &  & \\checkmark \\\\")))

# Figure S1: figures/country_comparison/support_binary_positive.pdf
heatmap_multiple(heatmaps_defs[c("support_binary")]) 

# Table S3: tables/amce.tex# Table S3: tables/amce.tex
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

# Figure S2: figures/[US1, FR, DE, UK, ES]/ca_r.png
# Cf. also code_global/conjoint_analysis.R to reproduce/code the conjoint analysis
plot(amce$FR_en, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r_en", folder = '../figures/FR/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$DE_en, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r_en", folder = '../figures/DE/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$ES_en, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r_en", folder = '../figures/ES/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$UK, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/UK/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$us1, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/US1/', width = 1100, height = 500, method='dev', trim = T, format = 'png') 

# Figure S3: figures/country_comparison/conjoint_left_ag_b_binary_positive.pdf
heatmap_multiple(heatmaps_defs[c("conjoint_left_ag_b_binary")]) 

# Figure S4: figures/country_comparison/belief_all_mean.pdf
heatmap_multiple(heatmaps_defs[c("belief_all")]) 

# Figure S5: figures/country_comparison/global_tax_global_share_mean.pdf
heatmap_multiple(heatmaps_defs[c("global_tax_global_share")]) 

# Figure S6: figures/country_comparison/foreign_aid_raise_support.pdf
barres_multiple(barresN_defs[c("foreign_aid_raise_support")]) 

# Figure S7: figures/country_comparison/foreign_aid_condition_positive.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_condition")]) 

# Figure S8: figures/country_comparison/foreign_aid_no_positive.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_no")]) 


##### App Literature review #####
# Figure S9: figures/maps/gain_gdr_over_gdp_2030.pdf, cf. preparation in code_global/map_GCS_incidence.R
# Figure S10: figures/maps/diff_gain_gdr_gcs_over_gdp_2030.pdf, cf. preparation in code_global/map_GCS_incidence.R
time_map_gcs <- Sys.time()
source("map_GCS_incidence.R")
time_map_gcs <- Sys.time() - time_map_gcs


##### App Raw results #####
# Figure S11: figures/OECD/Heatplot_global_tax_attitudes_positive.pdf, cf. code at https://doi.org/10.3886/E208254V1

# Figure S12: figures/country_comparison/understood_each_positive.pdf
heatmap_multiple(heatmaps_defs[c("understood_each")]) 

# Figure S13: figures/country_comparison/understood_score_mean.pdf
heatmap_multiple(heatmaps_defs[c("understood_score")]) 

# Figure S14: figures/country_comparison/list_exp_mean.pdf
heatmap_multiple(heatmaps_defs[c("list_exp")]) 

# Figure S15: figures/country_comparison/conjoint_ab_all_positive.pdf
heatmap_multiple(heatmaps_defs[c("conjoint_ab_all")]) 

# Figure S16: figures/[country]/ca_r.png
plot(amce$FR, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/FR/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$DE, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/DE/', width = 1100, height = 500, method='dev', trim = T, format = 'png')
plot(amce$ES, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/ES/', width = 1100, height = 500, method='dev', trim = T, format = 'png')

# Figure S17: figures/country_comparison/gcs_important_positive.pdf
heatmap_multiple(heatmaps_defs[c("gcs_important")]) 

# Figure S18: figures/country_comparison/gcs_field_positive.pdf
heatmap_multiple(heatmaps_defs[c("gcs_field")]) 

# Figure S19: figures/country_comparison/gcs_field_contains_positive.pdf
heatmap_multiple(heatmaps_defs[c("gcs_field_contains")]) 

# Figure S20: figures/country_comparison/donation_mean.pdf
heatmap_multiple(heatmaps_defs[c("donation")]) 

# Figure S21: figures/country_comparison/global_tax_support.pdf
barres_multiple(barresN_defs[c("global_tax_support")]) 

# Figure S22: figures/country_comparison/national_tax_support.pdf
barres_multiple(barresN_defs[c("national_tax_support")]) 

# Figure S23: figures/country_comparison/global_tax_global_share.pdf
barres_multiple(barresN_defs[c("global_tax_global_share")]) 

# Figure S24: figures/country_comparison/global_tax_sharing_positive.pdf
heatmap_multiple(heatmaps_defs[c("global_tax_sharing")]) 

# Figure S25: figures/country_comparison/foreign_aid_belief_agg.pdf
barres_multiple(barresN_defs[c("foreign_aid_belief_agg")]) 

# Figure S26: figures/country_comparison/foreign_aid_preferred_no_info_agg.pdf
barres_multiple(barresN_defs[c("foreign_aid_preferred_no_info_agg")]) 

# Figure S27: figures/country_comparison/foreign_aid_preferred_info_agg.pdf
barres_multiple(barresN_defs[c("foreign_aid_preferred_info_agg")]) 

# Figure S28: figures/country_comparison/global_tax_global_share_mean.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_amount")]) 

# Figure S29: figures/country_comparison/foreign_aid_no_less_all_positive.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_no_less_all")]) 

# Figure S30: figures/country_comparison/foreign_aid_raise_positive.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_raise")]) 

# Figure S31: figures/country_comparison/foreign_aid_reduce_positive.pdf
heatmap_multiple(heatmaps_defs[c("foreign_aid_reduce")]) 

# Figure S32: figures/country_comparison/petition_comparable_positive.pdf
heatmap_multiple(heatmaps_defs[c("petition_comparable")]) 

# Figure S33: figures/country_comparison/support_likert_positive.pdf
heatmap_multiple(heatmaps_defs[c("support_likert")]) 

# Figure S34: figures/country_comparison/negotiation.pdf
barres_multiple(barresN_defs[c("negotiation")]) 

# Figure S35: figures/country_comparison/problem_positive.pdf
heatmap_multiple(heatmaps_defs[c("problem")]) 

# Figure S36: figures/country_comparison/group_defended_agg2.pdf
barres_multiple(barresN_defs[c("group_defended_agg2")]) 

# Figure S37: figures/country_comparison/points_mean.pdf
# Figure S38: figures/country_comparison/points_positive.pdf
heatmap_multiple(heatmaps_defs[c("points")]) 

# Figure S39: figures/country_comparison/donation_charities.pdf
barres_multiple(barresN_defs[c("donation_charities")]) 

# Figure S40: figures/country_comparison/interested_politics.pdf
barres_multiple(barresN_defs[c("interested_politics")]) 

# Figure S41: figures/country_comparison/involvement_govt.pdf
barres_multiple(barresN_defs[c("involvement_govt")]) 

# Figure S42: figures/country_comparison/left_right.pdf
barres_multiple(barresN_defs[c("left_right")]) 

# Figure S43: figures/country_comparison/vote_participation.pdf
barres_multiple(barresN_defs[c("vote_participation")]) 

# Figure S44: figures/country_comparison/vote.pdf
barres_multiple(barresN_defs[c("vote")]) 

# Figure S45: figures/country_comparison/survey_biased.pdf
barres_multiple(barresN_defs[c("survey_biased")]) 

# Figure S46a: figures/country_comparison/poverty_field_positive.pdf
heatmap_multiple(heatmaps_defs[c("poverty_field")]) 
# Figure S46b: figures/country_comparison/poverty_field_contains_positive.pdf
heatmap_multiple(heatmaps_defs[c("poverty_field_contains")]) 

# Figure S47: figures/country_comparison/main_all_by_vote_share.pdf
heatmap_wrapper(vars = heatmaps_defs$main_all$vars, data = all, labels = heatmaps_defs$main_all$labels, name = "main_all_by_vote", along = "continent_vote", conditions = "/", 
                folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = F, weights = T) 


##### App Questionnaire #####
# Figure S47: questionnaire/survey_flow-combined.pdf, created on questionnaire/survey_flow.pptx


##### Net gains from the Global Climate Scheme #####
# cf. questionnaire/specificities.xlsx:Figures (line 6)

# Figure S48: figures/maps/median_gain_2015.pdf, created in code_global/map_GCS_incidence.R

# Table S4: tables/gain_gcs.tex, created in code_global/map_GCS_incidence.R


##### App Determinants of support #####
# Table S5: tables/country_comparison/gcs_support.tex
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "\\makecell{Supports the Global Climate Scheme}", covariates = covariates, 
                    data_list = list(all, us, eu, d("DE"), d("FR"), d("UK"), d("ES")), dep_var_labels = c("All", "United States", "Europe", countries_names), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "gcs_support", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

# Table S6: tables/country_comparison/gcs_support_understood.tex
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "Supports the Global Climate Scheme", covariates = c("gcs_understood"), covariate.labels = "\\makecell{With GCS, typical\\\\~[country] people lose\\\\and poorest humans win}",
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "gcs_support_understood", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

# Table S7: tables/global_tax_support_pos_AtC_keepC_hi.tex, cf. code at https://doi.org/10.3886/E208254V1
# Table S8: tables/global_tax_support_pos_AtC_keepC_mi.tex, cf. code at https://doi.org/10.3886/E208254V1


##### App Representativeness of the surveys #####

# Table S9: tables/sample_composition/US1_US2_EU_all.tex
representativeness_table(c("US1", "US2", "Eu"), return_table = F, all = T) 

# Table S10: tables/sample_composition/FR_DE_ES_UK_all.tex
representativeness_table(countries_EU, return_table = F, all = T, weight_var = "weight_country") 


##### App Attrition analysis #####

# Table S11: tables/US1/attrition_analysis_vote.tex
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US1/", data = c(list(us1a), list(us1a), list(us1a[us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

# Table S12: tables/US2/attrition_analysis_vote.tex
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US2/", data = c(list(us2a), list(us2a), list(us2a[us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

# Table S13: tables/EU/attrition_analysis_vote.tex
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 6"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/EU/", data = c(list(eua), list(eua), list(eua[eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,])), 
           indep_vars = c(quotas_eu, "vote_factor")) 


##### App Balance analysis #####

# Table S14: tables/balance_analysis.tex 
desc_table(dep_vars = c("branch_list_exp_g", "branch_petition == 'nr'", "branch_donation == 'Own nation'", "branch_conjoint_c == 'leftg_right'"), omit = c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{List contains: G}", "\\makecell{Branch petition: NR}", "\\makecell{Branch donation: Own nation}", "\\makecell{Branch conjoint 3: with GCS}"),
           filename = "balance_analysis", save_folder = "../tables/", data = all, indep_vars = c(socio_demos)) 


##### App Placebo tests #####

# Table S16: tables/placebo_tests.tex 
desc_table(dep_vars = c("conjoint_a", "cgr_support", "petition", "share_policies_supported", "conjoint_left_ag_b_binary"), omit = c("Constant", "Race: Other", "factorNA"),
           dep.var.labels = c("\\makecell{G+R+C\\\\preferred to\\\\R+C}", "\\makecell{Supports\\\\G+R+C}", "\\makecell{Signs\\\\petition}", "\\makecell{Share of\\\\policies\\\\supported}", "\\makecell{Conjoint 5\\\\A+CGS\\\\preferred to B}"),
           filename = "placebo_tests", save_folder = "../tables/", data = all, indep_vars = c("branch_list_exp", "branch_petition", "branch_donation")) 


##### App Main results on the extended sample #####
# Preparation
e <- merge(merge(us1a, us2a, all = T), eua, all = T)
e <- e[e$stayed == T,]
nrow(e) # n=9,318
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
e$foreign_aid_raise_support_no_null <- as.item(2*(temp-.5), labels = structure(2*(-1:2-.5), names = c("No, should be reduced", "No, should remain stable", "Yes, but at some conditions", "Yes, should be increased")), missing.values = NA, annotation = Label(e$foreign_aid_raise_support))     
e$group_defended_original <- e$group_defended
temp <- 0*grepl("myself", e$group_defended) + 1*grepl("relatives", e$group_defended) + 2*grepl("town|State", e$group_defended) + 3*grepl("religion", e$group_defended) + 4*grepl("Americans", e$group_defended) + 5*grepl("European", e$group_defended) + 6*grepl("Humans", e$group_defended) + 7*grepl("animals", e$group_defended)
e$group_defended <- as.item(temp, labels = structure(0:7, names = c("Family and self", "Relatives", "Region, U.S. State or town", "Culture or religion", "Fellow citizens", "Europeans", "Humans", "Sentient beings")), annotation = Label(e$group_defended))
e$group_defended[is.na(e$group_defended_original)] <- NA
e$universalist <- e$group_defended > 5
e$conjoint_left_ag_b_binary <- as.factor(e$conjoint_left_ag_b) == "A"
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

# Figure S50: figures/country_comparison/main_alla_share.pdf
heatmap_multiple(heatmaps_defs[c("main_all")], weights = F, data = e, name = "main_alla") 

# Figure S51: figures/country_comparison/conjoint_left_ag_b_binary_alla_positive.pdf
heatmap_multiple(heatmaps_defs[c("conjoint_left_ag_b_binary")], weights = F, data = e, name = "conjoint_left_ag_b_binary_alla") 

# Table S16: tables/continents/reg_list_exp_g_alla.tex
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
                    add_lines = list(c(11, paste("\\hline  \\\\[-1.8ex] \\textit{Support for GCS} &", round(mean(e$gcs_support[e$wave != "US2"], na.rm = T), 3), " & ", round(wtd.mean(e$gcs_support, weights = e$wave == "US1"), 3), " & ", round(wtd.mean(e$gcs_support, weights = e$wave == "EU"), 3), "\\\\")),
                                     c(12, paste("\\textit{Social desirability bias} & \\textit{$", round(avg.pred.social.desirability_$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_us_$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_eu_$fit[3,1], 3),  "$}\\\\")),
                                     c(13, paste("\\textit{80\\% C.I. for the bias} & \\textit{ $[", round(avg.pred.social.desirability_$fit[3,2], 2), ";", round(avg.pred.social.desirability_$fit[3,3], 2), "]$ } & \\textit{ $[", round(avg.pred.social.desirability_us_$fit[3,2], 2), ";", round(avg.pred.social.desirability_us_$fit[3,3], 2), "]$} & \\textit{ $[", round(avg.pred.social.desirability_eu_$fit[3,2], 2), ";", round(avg.pred.social.desirability_eu_$fit[3,3], 2), "]$}\\\\"))))

# Table S17: tables/country_comparison/conjoint_c_wo_none_alla.tex
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), along.levels = c("United States", names(countries_eu)[1:4]), share_na_remove = 1,
                    data = e[e$conjoint_c_none == F & e$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none_alla", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### App Effect of questionnaire framing #####

# Table S18: tables/ordering_us.tex
desc_table(dep_vars = c("universalist", "nationalist", "egoistic"), weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
          dep.var.caption = "Group defended when voting", mean_above = F,
           dep.var.labels = c("Humans \\textit{or} Sentient beings", "Fellow citizens", "Family and self"),
           filename = "ordering_us", save_folder = "../tables/", data = all[all$wave != "EU",], indep_vars = c("wave")) 


##### Figure Research Briefing #####
# Crop version of Figures 2 and 3. Shares of indifferent among the rows presented:
round(quantile(c(share_indifferent_oecd[c(1,2,4),], share_indifferent[10:11,]), c(0, .05, .25, .5, .75, .95, 1), na.rm = T), 2)
# 0%   5%   25%  50%  75%  95%  100%
# 0.10 0.11 0.15 0.20 0.26 0.32 0.37

(total_duration <- Sys.time() - start)
beep()
time_map_gcs