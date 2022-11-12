# 9d_random: randomly picks the policies, with a weight of 3/7 for the Global climate scheme and restrictions such that costly measures are funded
# 9d_r: randomly picks the policies but doesn't contain Foreign policy attribute, and for the Tax it only contains nR or nR+Wealth tax. /!\ Manually, need to modify the .js and .html to replace F- => D-. Also, need to add a last row to the HTML column so it contains Global climate scheme in Bundle A and - in Bundle B.
# TODO: manage conjoint analysis by pooling 9d_r and 9d_random

# Useful only in case of restrictions or weighted randomization. If it bugs, add a blank line after Restrictions in the .dat.
design_cjoint <- makeDesign(filename = "../conjoint_analysis/9a.dat")

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

