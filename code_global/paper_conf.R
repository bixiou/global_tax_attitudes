# TODO: check literature List experiment
# TODO! table list_exp in each country

# H0 stated support: OECD (heatmap), G, other_policies (plot), foreign aid evolution, why (plot)
# H1, H2 sincerity: list exp (table), petition (plot + table), conjoint analysis (plot), prioritization (plot)
# H3 pluralistic ignorance: belief (plot)
# H4 weak opinion: universalistic values (plot nego, group defended, problem)
# In US2/EU: donation, foreign aid in public spending, how (plot + table), perceptions G (plot), field on poverty reduction, global tax share/ing (plot), bandwagon effect (table)

# 4 most important figures: heatmap OECD, heatmap support, prioritization or conjoint (r), list exp (table)
# 2 essential figures: heatmap OECD, heatmap support
# => write a 2-3 pager (1kw with 2 figures) on Word w Science's template, send it as submission enquiry to Nature (as one can't send full paper); then for Policy forum in Science (peer-reviewed at editor's discretion).
# => write full paper as a 6-page (2500w) in LaTeX or Word so it can fit in PNAS, even for NCC it shouldn't exceed 8-9-page. (It's already too long for Science's max 5 page).
# => Submission order: 1. Nature, 2. Science, 3. NCC, 4. Nature Sust, 5. PNAS, 6. Science Advances, 7. GEC, 8. ERL or JEEM. cf. Relectures/journal_requirements.xlsx
# Impact Factors: Nature, Science: 50-70, Nature Sus: 27, NCC: 20, PNAS: 13, Sci Adv: 14, Global env change: 10, ERL: 7, JEEM: 6.

##### H0 #####
# Majority support for each global policies except maximum wealth and debt cancellation
# OECD
# G
decrit("gcs_support", data = e) # 54%
decrit("nr_support", data = e) # 54%
# Other policies
means_support <- indiferrents_support <- relative_majority_support <- c()
for (v in variables_support) means_support[v] <- wtd.mean(e[[v]] > 0, na.rm = T, weights = e$weight)
for (v in variables_other_policies) relative_majority_support[v] <- wtd.mean(e[[v]][e[[v]] != 0] > 0, na.rm = T, weights = e$weight[e[[v]] != 0])
-sort(-means_support)
-sort(-relative_majority_support)
# Foreign aid
decrit("foreign_aid_raise_support", data = e) 
means_foreign_aid_condition <- means_variables_foreign_aid_no_ <- means_variables_foreign_aid_raise <- means_variables_foreign_aid_reduce <- c()
for (v in variables_foreign_aid_condition) means_foreign_aid_condition[v] <- wtd.mean(e[[v]], na.rm = T, weights = e$weight)
for (v in variables_foreign_aid_no_) means_variables_foreign_aid_no_[v] <- wtd.mean(e[[v]], na.rm = T, weights = e$weight)
-sort(-means_foreign_aid_condition)
-sort(-means_variables_foreign_aid_no_)
barres_multiple(barres = barres_defs[c("support_likert", "support_binary", "foreign_aid_raise_support")], df = us1, folder = "../figures/US1/")


##### H1, H2 #####
# List experiment 
# There seems to be a 8pp social norm (differential of 3pp with NR). No effect of the number of options.
summary(lm(list_exp ~ branch_list_exp_g * branch_list_exp_r, data = e, weights = e$weight))
mean(e$gcs_support[e$branch_list_exp == "rgl"], na.rm = T) # 54%
mean(e$gcs_support[e$branch_list_exp == "rgl"], na.rm = T) + mean(e$nr_support[e$branch_list_exp == "rgl"], na.rm = T) - mean(e$nr_support[e$branch_list_exp == "rl"], na.rm = T) # 55%
mean(e$gcs_support[e$branch_list_exp == "gl"], na.rm = T) # 52%
mean(e$gcs_support[e$branch_list_exp == "gl"], na.rm = T) + mean(e$nr_support[e$branch_list_exp == "gl"], na.rm = T) - mean(e$nr_support[e$branch_list_exp == "l"], na.rm = T) # 56%
desc_table(dep_vars = "list_exp", filename = "all/reg_list_exp", dep.var.labels = "Number of supported policies", weights = e$weight, omit = c(), 
           indep_vars = c("branch_list_exp_g", "branch_list_exp_r", "branch_list_exp_g:branch_list_exp_r"), nolabel = F,
           indep_labels = c("List contains: GCS", "List contains: NR", "List contains: GCS $\\times$ NR"))

# Petition
# Small effect against GCS: -4pp
decrit("petition_gcs", data = e) # 49%
decrit("gcs_support", data = e, which = e$branch_petition == "gcs") # 53%
decrit("petition_nr", data = e) # 57%
decrit("nr_support", data = e, which = e$branch_petition == "nr") # 56%
summary(lm(petition_matches_support ~ branch_petition, data = e))

# Conjoint analysis: G|C+R 56%, G|R 59%, G 48% ~ C (|R), G+C|R 56%, C|R 64%, Left+G - Left = -3pp, A+G vs. B 59%
# => G is supported for itself, rather independently from R or C, with similar support to both, and it doesn't significantly penalize the Left, and would help a Democratic candidate
decrit("cgr_support", data = e) # 53%
barres_multiple(barres = barres_defs[c("conjoint", "points")], df = us1, folder = "../figures/US1/")
summary(lm(conjoint_c ~ branch_c_gcs, data = e, weights = e$weight)) # TODO! share of None increases, explaining decline in Left/conjoint_c
summary(lm(conjoint_r ~ conjoint_r_type, data = e, weights = e$weight)) # conjoint_left_ag_b == conjoint_d
# Prioritization: G has mean only slightly lower than average, makes better than ban of cars and coal exit; global tax on millionaires does as well as wealth tax and almost as good as $15 minimum wage
(mean_points <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(e[[v]], na.rm = T, weights = NULL), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"]))))
decrit("points_foreign1_gcs", data = e) # mean: 15.4


##### H3 #####
# No pluralistic ignorance
decrit("gcs_belief", data = e) # 51.3-52%
decrit("nr_belief", data = e) # 53.7-54%
barres_multiple(barres = barres_defs[c("belief")], df = us1, folder = "../figures/US1/")


##### H4 #####
# A strong majority is universalist/cosmopolitan (TODO: which word?), even a majority for non-Republican
summary(lm(donation ~ branch_donation, data = e, weights = e$weight)) # +2.5% to nation (p=0.06)
decrit("group_defended", data = e) # 44% universalist
decrit("group_defended", data = e, which = e$political_affiliation != "Republican") # 51% universalist
decrit("negotiation", data = e) # Median for Country's interest to the extent it respects global justice
decrit("negotiation", data = e, which = e$political_affiliation == "Democrat") # same
means_variables_problem <- c()
for (v in variables_problem) means_variables_problem[v] <- mean(e[[v]][e$political_affiliation == "Democrat"], na.rm = T)
for (v in variables_problem) means_variables_problem[v] <- mean(e[[v]], na.rm = T)
-sort(-means_variables_problem) # CC n°1, global poverty ~ country inequality
barres_multiple(barres = barres_defs[c("negotiation", "group_defended_agg", "problem")], df = us1, folder = "../figures/US1/")


##### App Representativeness #####
representativeness_table(c("US1"), return_table = F) # TODO custom rows/style
representativeness_table(c("EU"), return_table = T, all = T) # too many Q1 (+4) and cities (+7)
representativeness_table(c("ES"), return_table = T, all = T) 
representativeness_table(c("US1"), return_table = F, all = T) 
representativeness_table(c("US1", "US2", "EU"), return_table = F, all = T) 
temp <- representativeness_table(countries_EU, return_table = T, all = T, weighted = F) 
representativeness_table(countries_EU, return_table = T, all = T, weight_var = "weight_country") 
representativeness_table(countries_EU, return_table = T, all = T, weight_var = "weight", filename = "FR_DE_ES_UK_all_EU_weight") 


##### App Attrition analysis #####
print(paste(nrow(us1a), "start"))
print(paste(sum(no.na(us1a$excluded)=="QuotaMet"), "are excluded for their quota is met"))
print(paste(sum(us1a$dropout), "dropout"))
# print(us1a[us1a$dropout==T, c("progress", "employment_status", "political_affiliation", "gcs_support")], n = 20)
print(paste(sum(us1a$dropout & us1a$progress == 19), "drop out at GCS description"))
print(paste(sum(us1a$dropout_late), "drop out after socio-demos"))
print(paste(sum(us1a$stayed), "are allowed and do not drop out"))
print(paste(sum(us1a$failed_test & us1a$stayed), "fail the attention test"))
print(paste(sum(us1a$duration < 240/60 & !us1a$failed_test & us1a$stayed), "complete in less than 4 min"))
# print(paste(sum((us1a$failed_test | us1a$duration < 240/60) & !us1a$dropout & no.na(us1a$excluded)!="QuotaMet"), "are excluded"))
print(paste(sum(no.na(us1a$excluded) == "Screened" & !us1a$dropout & no.na(us1a$excluded)!="QuotaMet"), "are excluded"))
print(paste(nrow(us1), "in final sample"))
print(paste0(round(100*sum(us1a$dropout)/sum(no.na(us1a$excluded) != "QuotaMet")), "% dropout"))
print(paste0(round(100*sum(us1a$dropout_late)/sum(no.na(us1a$excluded) != "QuotaMet")), "% dropout excluding sociodemos"))
# summary(lm(dropout ~ treatment, data = us1a, weights = us1a$weight))
# summary(lm(dropout ~ country, data = us1a, weights = us1a$weight))
summary(lm(as.formula(paste("dropout ~ ", paste(quotas_us, collapse = ' + '))), data = us1a, weights = us1a$weight))
summary(lm(as.formula(paste("(dropout & as.numeric(alla$progress >= 19)) ~ ", paste(quotas_us, collapse = ' + '))), data = us1a, weights = us1a$weight))

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis", save_folder = "../tables/US1/", data = c(list(us1a), list(us1a), list(us1a[us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,])), 
           indep_vars = quotas_us) 

decrit(us1a$gcs_support)
decrit(us1$gcs_support)


##### App Determinants of GCS support #####
desc_table(dep_vars = c("gcs_support"),
           dep.var.labels = c("\\makecell{Supports the\\\\Global\\\\Climate Scheme}"),
           filename = "gcs_support", save_folder = "../tables/US1/", data = us1, 
           indep_vars = socio_demos_us) # omit: Race: Other

same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "\\makecell{Supports the Global Climate Scheme}", covariates = covariates, 
                    data_list = list(all, us, eu, d("DE"), d("FR"), d("UK"), d("ES")), dep_var_labels = c("All", "United States", "Europe", countries_names), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "gcs_support", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)





































