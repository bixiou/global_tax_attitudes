# TODO! merge D and F (by defining GCS/- as the levels of foreign).
# TODO? put in preparation
# TODO! graph PDF

# Design of the programs:
# Policies are specific to each country. Here is the number of policies in US/EU (to which one should add "-" to each set except for tax_system in D which is {nR; nR + Wealth tax}): 
# nb policies US/EU: econ_issues 4/4, society_issues 3/2, climate_pol 3/3, tax_system 3/2 (except for D), foreign_policy 5/5 (0 for D)
# 9d_F: randomly picks the policies, with a weight of 3/7 for the Global climate scheme.
# 9d_F_EU: randomly picks the policies, with a weight of 3/7 for the Global climate scheme and weights .35/.4/.25 for nR/Wealth tax/- (so that more profiles are funded or with the combination nR+GCS).
# 9d_D/9d_D_EU: randomly picks the policies with uniform weighting but doesn't contain Foreign policy attribute, and for the tax_system it only contains nR or nR+Wealth tax. 

# Restrictions: [in the end, no restriction]
# /!\ The randomization algo is basic: it draws a profile using the weights until it finds one that it is allowed. So levels with many restrictions will appear rarely (more rarely than their defined weights). 
#     In case of restrictions, we'd need to adjust weights to re-establish targeted frequencies (to that end, makeDesign is useful as it gives the frequencies of each profile). 
# Initially we set up restrictions so that costly measures are funded (i.e. are not allowed without a revenue-generating one) and there isn't both a global tax on millionaires and a (national) wealth tax.
# We finally removed these restrictions because they would have required to include many interaction terms in the regression (e.g. all tax * econ interactions) which would have made the analysis hard to present.
# Also, they would have made some pairs of policies very correlated (like nR and $15 minimum wage (the only costless econ policy)) and appropriate weights complicated to compute.
# Share of programs that are fully funded: 9d_F: 63% (1/2+1/2*(2/5*3/4*6/7)), 9d_F_EU: 63% (.4+.6*(3/5*3/4*6/7)), 9d_D: 65% (1/2+1/2*(2/5*3/4)), 9d_D_EU: 72.5% (1/2+1/2*(3/5*3/4)).

# How to:
# 1. Open the .exe: define settings, attributes, levels, weights then restrictions
# /!\ When setting both custom weights and restrictions, it's important to set the weights before (otherwise it bugs), and to not edit the labels afterwards.
# 2. Save the .sdt, export the .dat, .js and .html. 
# /!\ For _D, manually need to modify the .js and .html to replace F- => D-. Also, need to add a last row to the HTML column so it contains Global climate scheme in Bundle A and - in Bundle B.
# 3. Directly edit the .js and .dat if needed. This is what I need, so the .sdt are obsolete and the .js are now tailored to our questionnaire (e.g. finding and adapting to the respondent's country).
# /!\ So now, the .js should not be overridden from the .exe (i.e. steps 1 and 2 should not be redone).
# 4. I have replaced policies by generic names in ...EU_template.js so that we can fill the names automatically using specificities.xlsx from the code below "Prepare EU JavaScript" (useful if we change the wording or policies).
# /!\ I think ..UK.dat will bug because comas are used both within policies names and to separate policies. This problem is avoided in ..EU.dat files, which should thus be used instead in R.
# 5. Import to R the .dat using makeDesign and the raw .csv data using read.qualtrics, cf. "Import to R" below
# 6. Conduct the analysis


###### Prepare EU JavaScript #####
policies_names <- as.matrix(read.xlsx("../questionnaire/specificities.xlsx", sheet = "Policies", rowNames = T, rows = c(1, 16:41), cols = 1:6))
policies_names <- policies_names[is.na(as.numeric(row.names(policies_names))),] # NAs by coercion normal
js_D <- readLines("../conjoint_analysis/9d_D_EU_template.js")
js_F <- readLines("../conjoint_analysis/9d_F_EU_template.js")
for (name in row.names(policies_names)) for (c in colnames(policies_names)) js_D <- gsub(paste0(c, '_', name), policies_names[name, c], js_D)
for (name in row.names(policies_names)) for (c in colnames(policies_names)) js_F <- gsub(paste0(c, '_', name), policies_names[name, c], js_F)
writeLines(js_D, "../conjoint_analysis/9d_D_EU.js") # (d): no foreign policy in it, GCS vs. nothing instead
writeLines(js_F, "../conjoint_analysis/9d_F_EU.js") # (r) Directly paste-able into Qualtrics


##### Import to R #####
# The CSV should be the raw CSV exported by Qualtrics, with answers should shown as "coded values" and not as choice text
# Useful only in case of restrictions or weighted randomization. If it bugs, add a blank line after Restrictions in the .dat or check above "/!\" below 4. in How to.

formula_cjoint_generic <- as.formula("selected ~ econ_issues + society_issues + climate_pol + tax_system + foreign_policy")
formula_cjoint_specific <- as.formula("selected ~ `Economic issues` + `Societal issues` + `Climate policy` + `Tax system` + `Foreign policy`")
# /!\ If Error in if (any(as.vector(r_1) - as.vector(cross_tab_std[m,... add a (second) blank line at the end of the .dat
design_cjoint_US <- makeDesign(filename = "../conjoint_analysis/9d_F.dat") # gives the probability that a profile appears: sum(design_cjoint$J)=1, dim(design_cjoint$J) = 5 4 4 4 5. 
design_cjoint_EU <- makeDesign(filename = "../conjoint_analysis/9d_F_EU.dat") 
design_cjoint_both <- makeDesign(filename = "../conjoint_analysis/9d_F_both.dat") # The weighted are the average of the US and EU weights
amce <- ca <- list() # We should have "Old qualtrics format detected." (otherwise it would assume new format and delete the first observation).
for (df in c("us1", "eu", "all")) { # "usp", "eup", "ep"
  print(df)
  csv.path <- paste0("../conjoint_analysis/ca_", df, ".csv")
  write.csv(d(df)[!is.na(d(df)$conjoint_r_number), c(variables_conjoint_r, 'conjoint_r_number', 'n')], csv.path, row.names = FALSE)
  temp <- readLines(csv.path)
  writeLines(c(temp[1], temp), csv.path)
  ca[[df]] <- read.qualtrics(csv.path, responses = 'conjoint_r_number', covariates = c(variables_conjoint_r_levels), respondentID = "n") # names(d(n))[cols_conjoint]
  names(ca[[df]])[1] <- "n"
  ca[[df]] <- merge(d(df)[, c("country", "n")], ca[[df]])
  for (i in 1:5) {
    ca[[df]][[conjoint.attributes[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]])
    for (c in countries) {
      temp <- which(ca[[df]]$country == c & !(ca[[df]][[conjoint_attributes[i]]] %in% c("soc3", "tax3", "-")))
      ca[[df]][[conjoint.attributes[i]]][temp] <- as.character(policies.names[as.character(ca[[df]][[conjoint_attributes[i]]][temp]), c])
    }
    ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "-"] <- "-"
    ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "soc3"] <- "Making abortion a right at the federal level"
    ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "tax3"] <- "Increase corporate income tax rate from 21% to 28%"
    ca[[df]][[conjoint.attributes[i]]] <- as.factor(ca[[df]][[conjoint.attributes[i]]])
    ca[[df]][[paste0(conjoint.attributes[i], ".rowpos")]] <- ca[[df]][[paste0(conjoint_attributes[i], ".rowpos")]]
  }
  formula_cjoint <- if (grepl("us", df)) formula_cjoint_specific else formula_cjoint_generic
  design_cjoint <- if (grepl("us", df)) design_cjoint_US else { if (df %in% c("e", "ep", "all")) design_cjoint_both else design_cjoint_EU }
  # amce[[n]] <- amce(formula_cjoint, ca[[df]], cluster = FALSE, weights= NULL)
  amce[[df]] <- amce(formula_cjoint, ca[[df]], design = design_cjoint, cluster = FALSE, weights= NULL)
}
# ca_e <- read.qualtrics("../data/EUn.csv", responses = "Q30", covariates = variables_conjoint_r_levels, ranks = NULL, new.format = T) 
for (c in countries_EU) {
  print(c)  # TODO break long string
  amce[[c]] <- amce(formula_cjoint_generic, ca$eu[ca$eu$country == c,], design = design_cjoint_EU, cluster = FALSE, weights= NULL)
  for (i in names(amce[[c]]$user.levels)) if (amce[[c]]$user.levels[[i]] %in% row.names(policies.names)) amce[[c]]$user.levels[[i]] <- policies.names[amce[[c]]$user.levels[[i]], c]
  for (i in names(amce[[c]]$user.names)) if (amce[[c]]$user.names[[i]] %in% row.names(policies.names)) amce[[c]]$user.names[[i]] <- policies.names[amce[[c]]$user.names[[i]], c]
}
for (c in c("all", "eu")) {
  for (i in names(amce[[c]]$user.names)) if (amce[[c]]$user.names[[i]] %in% row.names(policies.names)) amce[[c]]$user.names[[i]] <- policies.names[amce[[c]]$user.names[[i]], "US"]
  for (i in c("climatepolclimate3", "taxsystemtax1", "foreignpolicyforeign1", "foreignpolicyforeign2", "foreignpolicyforeign3", "foreignpolicyforeign4")) amce[[c]]$user.levels[[i]] <- policies.names[amce[[c]]$user.levels[[i]], "US"]
  amce[[c]]$user.levels[["econissuesecon2"]] <- "[Higher minimum wage] (DE: Bürgerversicherung)" # TODO! switch econ 1 with 4 in UK, ES and 3 with 4 in US and write "[Higher spending on public services like health]"
  amce[[c]]$user.levels[["climatepolclimate2"]] <- if (c == "eu") "Thermal insulation plan" else "Thermal insulation plan (except US: trillion investment in transport and insulation)"
  amce[[c]]$user.levels[["taxsystemtax2"]] <- "[Wealth tax] (ES: raise income tax above 100k€/year)"
}


##### Analysis #####
# baselines <- list()
# baselines$attribute <- "level"
# Amce <- amce(formula_cjoint_specific, ca[["usp"]], cluster = FALSE, weights= NULL, design = design_cjoint_US)
# Amce <- amce(formula_cjoint_generic, ca[["eup"]], cluster = FALSE, weights= NULL, design = design_cjoint_EU)
# Amce <- amce(formula_cjoint, ca[[df]], cluster = FALSE, weights= NULL)
# # Amce <- amce(formula_cjoint, ca_e[!is.na(ca_e$selected),], cluster = FALSE, weights= NULL)
# summary(Amce)http://127.0.0.1:11371/graphics/09d65b49-3a15-4047-bd65-2542b724f185.png
# plot(Amce)

plot(amce$all, xlab = "Average Marginal Component Effect", text.size = 18) # TODO! good labels
save_plot (filename = "ca_r", folder = '../figures/all/', width = 700, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$us1, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/US1/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$eu, xlab = "Average Marginal Component Effect", text.size = 18)# TODO! good labels
save_plot (filename = "ca_r", folder = '../figures/EU/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$FR, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/FR/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$DE, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/DE/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$ES, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/ES/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$UK, xlab = "Average Marginal Component Effect", text.size = 18)
save_plot (filename = "ca_r", folder = '../figures/UK/', width = 1100, height = 500, method='dev', trim = T, format = 'png') # TODO! solve bug PDF
plot(amce$usp) # GCS is foreign1
plot(amce$eup)
plot(amce$ep)
plot(amce$FR, text.size = 12)
plot(amce$DE)
plot(amce$ES)
plot(amce$UK)
summary(amce$usp)
