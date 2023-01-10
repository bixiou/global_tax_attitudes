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
amce <- ca <- list()
for (n in c("usp", "eup", "ep")) {
  print(n)
  csv.path <- paste0("../conjoint_analysis/ca_", n, ".csv")
  write.csv(d(n)[!is.na(d(n)$conjoint_r_number), c(variables_conjoint_r, 'conjoint_r_number', 'n')], csv.path, row.names = FALSE)
  temp <- readLines(csv.path)
  writeLines(c(temp[1], temp), csv.path)
# Pb? the function below might only works with PHP export (but JS export is better)
  ca[[n]] <- read.qualtrics(csv.path, responses = 'conjoint_r_number', covariates = c(variables_conjoint_r_levels), respondentID = "n") # names(d(n))[cols_conjoint]
  names(ca[[n]])[1] <- "n"
  ca[[n]] <- merge(d(n)[, c("country", "n")], ca[[n]])
  for (i in 1:5) {
    ca[[n]][[conjoint.attributes[i]]] <- as.character(ca[[n]][[conjoint_attributes[i]]])
    for (c in countries) {
      temp <- which(ca[[n]]$country == c & !(ca[[n]][[conjoint_attributes[i]]] %in% c("soc3", "tax3", "-")))
      ca[[n]][[conjoint.attributes[i]]][temp] <- as.character(policies.names[as.character(ca[[n]][[conjoint_attributes[i]]][temp]), c])
    }
    ca[[n]][[conjoint.attributes[i]]][ca[[n]][[conjoint_attributes[i]]] == "-"] <- "-"
    ca[[n]][[conjoint.attributes[i]]][ca[[n]][[conjoint_attributes[i]]] == "soc3"] <- "Making abortion a right at the federal level"
    ca[[n]][[conjoint.attributes[i]]][ca[[n]][[conjoint_attributes[i]]] == "tax3"] <- "Increase corporate income tax rate from 21% to 28%"
    ca[[n]][[conjoint.attributes[i]]] <- as.factor(ca[[n]][[conjoint.attributes[i]]])
    ca[[n]][[paste0(conjoint.attributes[i], ".rowpos")]] <- ca[[n]][[paste0(conjoint_attributes[i], ".rowpos")]]
  }
  formula_cjoint <- if (grepl("us", n)) formula_cjoint_specific else formula_cjoint_generic
  amce[[n]] <- amce(formula_cjoint, ca[[n]], cluster = FALSE, weights= NULL)
}
# ca_e <- read.qualtrics("../data/EUn.csv", responses = "Q30", covariates = variables_conjoint_r_levels, ranks = NULL, new.format = T) # TODO new or old?

design_cjoint <- makeDesign(filename = "../conjoint_analysis/9d_F.dat") # gives the probability that a profile appears: sum(design_cjoint$J)=1, dim(design_cjoint$J) = 5 4 4 4 5. # TODO

# TODO: UK EconomicS issues => Economic issues
##### Analysis #####
baselines <- list()
baselines$attribute <- "level"
Amce <- amce(formula_cjoint, ca[[n]], cluster = FALSE, weights= NULL)
# Amce <- amce(formula_cjoint, ca_e[!is.na(ca_e$selected),], cluster = FALSE, weights= NULL)
summary(Amce)
plot(Amce)

plot(amce[["usp"]]) # GCS is foreign1
plot(amce[["eup"]])
plot(amce[["ep"]])
summary(amce[["usp"]])
