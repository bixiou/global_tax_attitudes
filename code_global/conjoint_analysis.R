# 9d_F: randomly picks the policies, with a weight of 3/7 for the Global climate scheme and restrictions such that costly measures are funded and there isn't both a global and national tax on millionaires
# 9d_D: randomly picks the policies but doesn't contain Foreign policy attribute, and for the Tax it only contains nR or nR+Wealth tax. It also includes restrictions such that costly measures are funded 
# /!\ Manually, need to modify the .js and .html to replace F- => D-. Also, need to add a last row to the HTML column so it contains Global climate scheme in Bundle A and - in Bundle B.
# /!\ When setting both custom weights and restrictions, it's important to set the weights before (otherwise it bugs), and to not edit the labels afterwards.
# /!\ The randomization algo is basic: it draws a profile using the weights until it finds one that it is allowed. So levels with many restrictions will appear rarely (more rarely than their defined weights). 
#     => Need to adjust weights to re-establish targeted frequencies IN ALL countries and in .dat
# 9d_D_UK and other EU countries: like 9d_D which is for the U.S.
# 9d_F_UK and other EU countries: like 9d_F except that the policies are different and Wealth tax has a weight 1/2 (to compensate the lact of other revenue-generating measure)
# TODO: manage conjoint analysis by pooling 9d_D and 9d_F

# Prepare EU JavaScript from EU template
policies_names <- as.matrix(read.xlsx("../questionnaire/specificities.xlsx", sheet = "Policies", rowNames = T, rows = c(1, 16:41), cols = 1:5))
policies_names <- policies_names[is.na(as.numeric(row.names(policies_names))),]
js_D <- readLines("../conjoint_analysis/9d_D_EU_template.js")
js_F <- readLines("../conjoint_analysis/9d_F_EU_template.js")
for (name in row.names(policies_names)) for (c in colnames(policies_names)) js_D <- gsub(paste0(c, '_', name), policies_names[name, c], js_D)
for (name in row.names(policies_names)) for (c in colnames(policies_names)) js_F <- gsub(paste0(c, '_', name), policies_names[name, c], js_F)
writeLines(js_D, "../conjoint_analysis/9d_D_EU.js")
writeLines(js_F, "../conjoint_analysis/9d_F_EU.js")


# Useful only in case of restrictions or weighted randomization. If it bugs, add a blank line after Restrictions in the .dat.
design_cjoint <- makeDesign(filename = "../conjoint_analysis/9d_F.dat") # gives the probability that a program appears: sum(design_cjoint$J)=1, dim(design_cjoint$J) = 5 4 4 4 5

# The CSV should be the raw CSV exported by Qualtrics, with answers should shown as "coded values" and not as choice text
cols_conjoint <- c(12:13) # number (or name) of names(data) corresponding to the conjoint analysis' tasks
covariates_conjoint <- c() # names of the covariates to be included
cols_rankings <- c(10:11) # An integer vector with the identifiers of the CSV columns that contain the conjoint rankings or ratings. (?)
# Pb? the function below might only works with PHP export (but JS export is better)
ca <- read.qualtrics("../data/raw.csv", responses = cols_conjoint, covariates = covariates_conjoint, ranks = cols_rankings)

formula_cjoint <- as.formula("")
baselines <- list()
baselines$attribute <- "level"
amce <- amce(formula_cjoint, ca, weights= NULL, design = design_cjoint) # , baselines = baselines
summary(amce)
plot(amce)

