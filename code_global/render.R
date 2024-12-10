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
heatmap_multiple(heatmaps_defs[c("foreign_aid_no_less_all")])
heatmap_multiple(heatmaps_defs[c("support_likert_gcs")])
# heatmaps_defs$support_likert_plus$labels[4] <- expression("Preferred share of global wealth tax for low-income countries: ">=" 30%*")
heatmaps_defs$support_likert_plus$labels[4] <- heatmaps_defs$support_likert_all$labels[4] <- "Preferred share of global wealth tax\nfor low-income countries: 30% or more"
heatmaps_defs$support_likert_all$labels[5] <- "[Country]'s foreign aid should be increased*"
heatmap_multiple(heatmaps_defs[c("support_likert_plus", "support_likert_all")])
# text(-5, 10, expression("Preferred share of global wealth tax\nfor low-income countries: ">=" 30%*"))
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
barres_multiple(barres = barres_defs["gcs_support"], df = us1, folder = "../figures/US1/") 
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

# Plots in French
# barres_multiple(barres = fill_barres("conjoint_left_ag_b_binary", list("gcs_support" = list(rev = T, rev_color = F)), country= "FR", along = "country_name"),  folder = paste0("../figures/FR/")) 
e$country_name_fr <- case_when(e$country_name == "United States" ~ "États-Unis", e$country_name == "France"  ~ "France", e$country_name == "Germany" ~ "Allemagne", e$country_name == "Spain" ~ "Espagne", e$country_name == "United Kingdom" ~ "Royaume-Uni")
heatmaps_defs_fr <- fill_heatmaps(c(), list(#"conjoint_left_ag_b_binary" = list(vars = "conjoint_left_ag_b_binary", conditions = c(">= 1")), 
                                            #"gcs_support" = list(vars = "gcs_support", conditions = ">= 1"),
                                            #"support_likert" = list(vars = variables_support_likert, conditions = "/")#,
                                            "support_likert_few" = list(vars = variables_support_likert[c(1:4,9:11)], conditions = "/")#,
                                            #"global_tax_global_share" = list(vars = c("global_tax_global_share"), conditions = c("", ">= 1"), nb_digits = 0)
                                            ), labels = labels_vars_country$FR)
heatmap_multiple(heatmaps_defs_fr, along = "country_name_fr", folder = "../figures/FR/")



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
heatmap_multiple(heatmaps_defs[c("main_all")], weights = F, data = alla, name = "main_alla") 
heatmap_multiple(heatmaps_defs[c("support_binary")], weights = T) 
heatmap_multiple(heatmaps_defs[c("conjoint_ab", "conjoint_ab_all")], weights = T) 
heatmap_wrapper(vars = heatmaps_defs$main$vars, data = all, labels = heatmaps_defs$main$labels, name = "main_by_vote", along = "continent_vote", conditions = "/", folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = T, weights = T) 
heatmap_wrapper(vars = heatmaps_defs$main_all$vars, data = all, labels = heatmaps_defs$main_all$labels, name = "main_all_by_vote", along = "continent_vote", conditions = "/", folder = "../figures/country_comparison/", sort = FALSE, percent = FALSE, proportion = NULL, nb_digits = NULL, trim = F, weights = T) 
heatmap_multiple(heatmaps_defs[c("few_main")], weights = T) 

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
barres_multiple(barres = barresN_defs[c("gcs_support")], df = eu, folder = "../figures/EU/") 
barres_multiple(barres = barresN_defs[c("foreign_aid_preferred_info_agg", "foreign_aid_preferred_no_info_agg")], df = all, folder = "../figures/country_comparison/") 
barres_multiple(barres = barresN_continent_defs[c("foreign_aid_preferred_info_agg", "foreign_aid_preferred_no_info_agg")], df = all, folder = "../figures/continents/") 
barres_multiple(barres = barresN_defs["global_tax_global_share"], df = all, folder = "../figures/country_comparison/") 

barres_multiple(barres = barresN_defs, df = all, folder = "../figures/country_comparison/") 
barres_multiple(barres = barresN_continent_defs, df = all, folder = "../figures/continents/") 

barres_multiple(barres = barresN_vote_defs["gl"], df = eu, folder = "../figures/EU/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("FR"), folder = "../figures/FR/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("DE"), folder = "../figures/DE/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("ES"), folder = "../figures/ES/vote/") 
barres_multiple(barres = barresN_vote_defs, df = d("UK"), folder = "../figures/UK/vote/") 
barres_multiple(barres = barresN_vote3_defs, df = us, folder = "../figures/US/vote/") 
barres_multiple(barres = barresN_vote3_defs[c("gcs_support")], df = us1, folder = "../figures/US1/vote/") 

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
