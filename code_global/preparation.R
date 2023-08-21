# Consistency: increase/reduce aid for those with the info + later question; petition_yes_support_no; duplicate_ip
# TODO order_
# TODO sources quotas, national quotas incl. vote
# TODO check vote

source(".Rprofile")
source("relabel_rename.R")
# source("conjoint_analysis.R")
# Our panelist is Bilendi/Respondi. Their partner panelist in the U.S. is Prodedge. 

##### Income quantiles and policies names #####
qinc <- read.csv("../data/EU_income_deciles.tsv", sep = "\t") # equivalised disposable income in LCU
inc_quantiles <- matrix(NA, nrow = 5, ncol = 11, dimnames = list(c("FR", "DE", "ES", "UK", "CH"), c("D1", "D2", "D3", "D4","Q2", "D6", "D7", "D8", "D9", "Q1", "Q3")))
for (c in c("FR", "DE", "ES", "UK", "CH")) {
  for (i in 1:9) inc_quantiles[c,i] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("D", i, ",TC,NAC,", c)])) # euro_countries / year_countries[c]
  inc_quantiles[c,10] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("Q", 1, ",TC,NAC,", c)])) 
  inc_quantiles[c,11] <- as.numeric(gsub(" b", "", qinc[[paste0("X", if (c == "UK") 2018 else if (c == "CH") 2019 else 2021)]][qinc[[1]]==paste0("Q", 3, ",TC,NAC,", c)])) 
}
inc_quantiles <- inc_quantiles[,c(1,2,10,3:7,11,8,9)]
inc_quantiles["UK",] <- round(inc_quantiles["UK",]*2317/2170) # inflate to 2021 by nominal GDP growth in LCU https://data.worldbank.org/indicator/NY.GDP.MKTP.CN?end=2021&locations=GB&start=1960&view=chart
inc_quantiles["CH",] <- round(inc_quantiles["CH",]*742.84/727.21) # Now in 2021 LCU for all
inc_quantiles # Now in 2021 LCU for each
50*round(inc_quantiles/12/50) # Per month, rounded
rm(qinc)

policies.names <- as.matrix(read.xlsx("../questionnaire/specificities.xlsx", sheet = "Policies", rowNames = T, rows = c(1, 16:41), cols = 1:6))
policies.names <- policies.names[is.na(as.numeric(row.names(policies.names))),] # NAs by coercion normal
policies.names.en <- as.matrix(read.xlsx("../questionnaire/specificities.xlsx", sheet = "Policies_EN", rowNames = T, rows = c(1, 16:41), cols = 1:6))
policies.names.en <- policies.names.en[is.na(as.numeric(row.names(policies.names.en))),] # NAs by coercion normal
policies.names.us <- rbind(policies.names, "tax3" = c(rep(NA, 4), "Increase corporate income tax rate from 21% to 28%"), "soc3" = c(rep(NA, 4), "Making abortion a right at the federal level"))
# write.csv(policies.names, "../data/policies_names.csv") # to recover it in case specificities.xlsx is modified
conjoint_attributes <- c("econ_issues", "society_issues", "climate_pol", "tax_system", "foreign_policy")
conjoint.attributes <- c("Economic issues", "Societal issues", "Climate policy", "Tax system", "Foreign policy")
names(conjoint.attributes) <- conjoint_attributes # c("Economic.issues", "Societal.issues", "Climate.policy", "Tax.system", "Foreign.policy")
# row.names(policies.names)[unique(which(policies.names %in% conjoint_attributes) %% 20)] <- conjoint_attributes

countries_names <- c("France", "Germany", "Spain", "United Kingdom", "United States")
foreign_aid_actual <- c(.8, 1.3, .5, 1.7, .4)
names(countries_names) <- names(foreign_aid_actual) <- countries <- c("FR", "DE", "ES", "UK", "US")
names(countries) <- countries_names
countries_EU <- countries[1:4]
major_candidates <- minor_candidates <- list()


##### Quotas #####
{
  levels_quotas <- list(
  "gender" = c("Woman", "Other", "Man"), # we could add: urbanity, education, wealth, occupation, employment_agg, marital_status, Nb_children, HH_size, home (ownership)
  "income_quartile" = 1:4, #c("Q1", "Q2", "Q3", "Q4"),
  "age" = c("18-24", "25-34", "35-49", "50-64", "65+"),
  "urbanity" = c("Cities", "Towns and suburbs", "Rural"),
  "diploma_25_64" = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64"), # "Not 25-64"
  "employment_18_64" = c("Inactive", "Unemployed", "Employed", "65+"),
  "vote" = c("Left", "Center-right or Right", 'Far right', "PNR/Non-voter"),
  "EU_country" = c("FR", "DE", "ES", "UK"),
  "US_region" = c("Northeast", "Midwest", "South", "West"),
  "US_race" = c("White only", "Hispanic", "Black", "Other"),
  "US_vote_us" = c("Biden", "Trump", "Other/Non-voter", "PNR/no right"),
  "EU_urbanity" = c("Cities", "Towns and suburbs", "Rural"),
  "wealth" = paste0("Q", 1:5),
  "US_urban" = c(TRUE, FALSE)#,
  # "college_OECD" = c("College Degree", "No college"),
  # "employment" = c(TRUE, FALSE),
  # "diploma" = c("No secondary", "Vocational", "High school", "College"), # "FR_education" = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"),
  # "US_core_metropolitan" = c(FALSE, TRUE),
  # "FR_region" = c("autre", "IDF", "Nord-Est", "Nord-Ouest", "Sud-Est", "Sud-Ouest"),
  # "FR_diploma" = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"),
  # "FR_CSP" = c("Inactif", "Ouvrier", "Cadre", "Indépendant", "Intermédiaire", "Retraité", "Employé", "Agriculteur"),
  # "FR_region9" = c("autre","ARA", "Est", "Nord", "IDF", "Ouest", "SO", "Occ", "Centre", "PACA"),
  # "UK_urban_category" = c("Rural", "City_Town", "Large_urban"),
  # "ES_region" = c("East", "Center",  "South", "North", "North-West"),
  # "DE_region" = c("Northern", "Western", "Central", "Eastern", "Southern"),
  # "DE_urban_category" = c("Rural", "Towns_and_Suburbs", "Cities"),
)
  
  quotas <- list("EU" = c("gender", "income_quartile", "age", "diploma_25_64", "country", "urbanity"),
                 "EU_vote" = c("gender", "income_quartile", "age", "diploma_25_64", "country", "urbanity", "vote"),
                 "EU_all" = c("gender", "income_quartile", "age", "diploma_25_64", "country", "urbanity", "employment_18_64", "vote"), 
                 "US" = c("gender", "income_quartile", "age", "diploma_25_64", "race", "region", "urban"), 
                 "US_vote" = c("gender", "income_quartile", "age", "diploma_25_64", "race", "region", "urban", "vote_us"),
                 "US_all" = c("gender", "income_quartile", "age", "diploma_25_64", "race", "region", "urban", "employment_18_64", "vote"),
                 "FR" = c("gender", "income_quartile", "age", "diploma_25_64", "urbanity"), #, "urban_category") From oecd_climate: Pb sur cette variable car il y a des codes postaux à cheval sur plusieurs types d'aires urbaines. Ça doit fausser le type d'aire urbaine sur un peu moins de 10% des répondants. Plus souvent que l'inverse, ça les alloue au rural alors qu'ils sont urbains.
                 # Au final ça rajoute plus du bruit qu'autre chose, et ça gène pas tant que ça la représentativité de l'échantillon (surtout par rapport à d'autres variables type age ou diplôme). Mais ça justifie de pas repondérer par rapport à cette variable je pense. cf. FR_communes.R pour les détails.
                 "DE" = c("gender", "income_quartile", "age", "diploma_25_64", "urbanity"),
                 "ES" = c("gender", "income_quartile", "age", "diploma_25_64", "urbanity"),
                 "UK" = c("gender", "income_quartile", "age", "diploma_25_64", "urbanity")
  )
  for (c in countries_EU) quotas[[paste0(c, "_all")]] <- c(quotas[[c]], "employment_18_64", "vote")

  qs <- read.xlsx("../questionnaire/specificities.xlsx", sheet = "Quotas", rowNames = T, rows = c(1:6, 8), cols = 1:43)

  pop_freq <- list(
    "EU" = list( 
      "EU_country" = unlist(qs["EU", c("FR", "DE", "ES", "UK")]/1000)
    ),
    "US" = list(
      "urbanity" = c(qs["US", "Cities"], 0.001, qs["US","Rural"])/1000,
      "US_urban" = c(qs["US", "Cities"], qs["US","Rural"])/1000,
      "US_region" = unlist(qs["US", c("Region.1", "Region.2", "Region.3", "Region.4")]/1000),
      "US_race" = unlist(qs["US", c("White.non.Hispanic", "Hispanic", "Black", "Other")]/1000),
      "US_vote_us" = c(0.342171, 0.312823, 0.345006, 0.000001)
    ))
  for (c in c("EU", countries)) {
    pop_freq[[c]]$gender <- c(qs[c,"women"], 0.001, qs[c,"men"])/1000
    pop_freq[[c]]$income_quartile <- rep(.25, 4)
    pop_freq[[c]]$age <- unlist(qs[c, c("18-24", "25-34", "35-49", "50-64", ">65")]/1000)
    pop_freq[[c]]$diploma_25_64 <- unlist(c(qs[c, c("Below.upper.secondary.25-64.0-2", "Upper.secondary.25-64.3", "Above.Upper.secondary.25-64.4-8")]/1000, "Not 25-64" = sum(unlist(qs[c, c("18-24", ">65")]/1000))))
    pop_freq[[c]]$employment_18_64 <- unlist(c(c("Inactive" = qs[c, "Inactivity"], "Unemployed" = qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000, "Employed" =  1000-qs[c, "Inactivity"]-qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000)*(1000-qs[c, c(">65")])/1000, "65+" = qs[c, c(">65")])/1000)
    pop_freq[[c]]$vote <- unlist(c(c(qs[c, "Left"], qs[c, "Center-right.or.Right"], qs[c, "Far.right"])*(1000-qs[c, "Abstention"])/sum(qs[c, c("Left", "Center-right.or.Right", "Far.right")]), qs[c, "Abstention"])/1000)
    pop_freq[[c]]$wealth <- rep(.2, 5)
    if (c != "US") pop_freq[[c]]$urbanity <- unlist(qs[c, c("Cities", "Towns.and.suburbs", "Rural")]/1000)
  }
}


##### Functions #####
remove_id <- function(file, folder = "../data/") {
  filename <- paste(folder, file, ".csv", sep = "")
  
  filename_copy <- paste("./deprecated/", file, sample.int(10^5, 1), ".csv", sep = "") # in case the three last lines don't work
  # file.copy(filename, "./deprecated/")
  file.copy(filename, filename_copy)
  data <- read_csv(filename_copy)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic")))]
  write_csv(data, filename, na = "")
  file.remove(filename_copy)
  # data <- read_csv(filename) # the three commented lines worked well for all .csv except SK, don't know why. Maybe retry later to put them back
  # data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID")))]
  # write_csv(data, filename, na = "")
} # for (file in c("US_pilot", "US_pilot2", "US_pilot3", "US", "DK", 'FR')) remove_id(file)

relabel_and_rename <- function(e, country, wave = NULL) {
  # Notation: ~ means that it's a random variant / * that the question is only displayed under certain condition
  
  # The commented lines below should be executed before creating relabel_and_rename, to ease the filling of each name and label
  # remove_id("UA")
  # e <- read_csv("../data/EUn.csv")
  # for (i in 1:length(e)) {
  #   label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
  #   print(paste(i, label(e[[i]])))
  # }
  # 
  if (missing(wave) || wave == "full") {
    e <- match.fun(paste0("relabel_and_rename_", country))(e)
    # e <- e[,-c((which(names(e) %in% c("clicked_petition", "positive_treatment"))+1):length(names(e)))]
  }  else e <- match.fun(paste0("relabel_and_rename_", country, wave))(e)
  
  for (i in 1:length(e)) {
    label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="")
    # print(paste(i, label(e[[i]])))
  }
  e <- e[-c(1:2),]
  
  return(e)
}

# 26% Les pays pauvres s'en sortiront mieux par eux-mêmes qu'avec notre aide
# 37% Je serais favorable si l'aide allait directement aux plus pauvres, et pas aux États
# 26% Je serais favorable si tous les pays riches contribuaient autant que la France
# 33% La dépense publique doit servir en priorité aux services publics et aux Français
# 10% Je serais favorable avec un montant plus faible : 2% c'est trop
weighting <- function(e, country, printWeights = T, variant = NULL, min_weight_for_missing_level = F, combine_age_50 = FALSE, trim = T) {
  if (!missing(variant)) print(variant)
  vars <- quotas[[paste0(c(country, variant), collapse = "_")]]
  freqs <- list()
  for (v in vars) {
    if (!(v %in% names(e))) warning(paste(v, "not in data"))
    e[[v]] <- as.character(e[[v]])
    e[[v]][is.na(e[[v]])] <- "NA"
    var <- ifelse(v %in% names(levels_quotas), v, paste(country, v, sep="_"))
    if (!(var %in% names(levels_quotas))) warning(paste(var, "not in levels_quotas"))
    levels_v <- as.character(levels_quotas[[var]])
    missing_levels <- setdiff(levels(as.factor(e[[v]])), levels_v)
    present_levels <- which(levels_v %in% levels(as.factor(e[[v]])))
    if (length(present_levels) != length(levels_v)) warning(paste0("Following levels are missing from data: ", var, ": ", paste(levels_v[!1:length(levels_v) %in% present_levels], collapse = ', '), " (for ", country, "). Weights are still computed, neglecting this category."))
    prop_v <- pop_freq[[country]][[var]][present_levels]
    if (v == "age" & combine_age_50) {
      e$age_50 <- e$age
      e$age_50[e$age == "65+"] <- "50-64"
      prop_v[4] <- prop_v[4] + prop_v[5]
      prop_v <- prop_v[1:4]
      levels_v <- levels_v[1:4]
      present_levels <- present_levels[1:4]
      v <- "age_50"
      vars[vars == "age"] <- "age_50"
    }
    if (min_weight_for_missing_level) freq_missing <- rep(0.000001, length(missing_levels)) # imputes 0 weight for levels present in data but not in the weight's definitio
    else freq_missing <- vapply(missing_levels, function(x) sum(e[[v]]==x), FUN.VALUE = c(0))
    freq_v <- c(prop_v*(nrow(e)-sum(freq_missing)), freq_missing)
    df <- data.frame(c(levels_v[present_levels], missing_levels), freq_v)
    # df <- data.frame(c(levels_v, missing_levels), nrow(e)*c(pop_freq[[country]][[var]], rep(0.0001, length(missing_levels))))
    names(df) <- c(v, "Freq")
    freqs <- c(freqs, list(df))
  }
  # print(freqs)
  unweigthed <- svydesign(ids=~1, data=e)
  raked <- rake(design= unweigthed, sample.margins = lapply(vars, function(x) return(as.formula(paste("~", x)))), population.margins = freqs)
  
  if (printWeights) {    print(summary(weights(raked))  )
    print(paste("(mean w)^2 / (n * mean w^2): ", representativity_index(weights(raked)), " (pb if < 0.5)")) # <0.5 : problématique
    print(paste("proportion not in [0.25; 4]: ", round(length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)), 3), "Nb obs. in sample: ", nrow(e)))
  }
  if (trim) return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE)))
  else return(weights(raked, lower=0.25, upper=4, strict=TRUE))
}

prepare <- function(incl_quality_fail = FALSE, exclude_speeder=TRUE, exclude_screened=TRUE, only_finished=TRUE, only_known_agglo=T, duration_min=0, country = "US", wave = NULL, weighting = TRUE, replace_brackets = FALSE, zscores = T, zscores_dummies = FALSE, remove_id = FALSE, efa = FALSE, combine_age_50 = T, define_var_lists = T) { #(country!="DK") # , exclude_quotas_full=TRUE
  # if (country == "US") {
  #   if (wave == "pilot1") e <- read_csv("../data/US_pilot.csv") 
  #   else if (wave == "pilot2") e <- read_csv("../data/US_pilot2.csv") 
  #   else if (wave == "pilot3") e <- read_csv("../data/US_pilot3.csv") 
  #   else if (wave == "full") e <- read_csv("../data/US.csv") 
  # } else if (country == "DK") e <- read_csv("../data/DK.csv") 
  filename <- paste0(c(country, wave), collapse="_")
  file <- paste0("../data/", filename, ".csv")
  # if (filename != "SA") remove_id(filename)
  # if (missing(remove_id)) {
  #   e <- read_csv(file)
  #   remove_id <- "PSID" %in% names(e)
  # }
  if (remove_id) remove_id(filename)
  if (replace_brackets) {
    data <- readLines(file)
    data <- gsub("[Country]", Country_names[country], data, fixed = T)
    data <- gsub("[country]", country_names[country], data, fixed = T)
    writeLines(data, con=file)  }
  e <- read_csv(file)
  # e <- read_csv("../data/IT.csv")
  
  if (missing(wave)) wave <- "full"
  e <- relabel_and_rename(e, country = country, wave = wave)
  all_na <- c()
  for (v in 1:ncol(e)) if (all(is.na(e[[v]])) & is.na(names(e)[[v]])) all_na <- c(all_na, v)
  e <- e[, setdiff(1:ncol(e), all_na)]
  
  print(paste(length(which(e$excluded=="QuotaMet")), "QuotaMet"))
  e$finished[e$excluded=="QuotaMet"] <- "False" # To check the number of QuotaMet that shouldn't have incremented the quota, comment this line and: decrit(e$each_strate[e$exclu=="QuotaMet" & e$csp=="Employé" & !grepl("2019-03-04 07", e$date)])
  if (incl_quality_fail & "attention_test" %in% names(e)) e <- e[replace_na(e$excluded, "na") != "QuotaMet" & !is.na(e$attention_test) & !(replace_na(e$excluded, "na") == "Screened" & replace_na(e$attention_test) == "A little"),] # allqa: e[e$finished == 1
  if (exclude_screened & !incl_quality_fail) { e <- e[is.na(e$excluded),] }
  if (exclude_speeder) e <- e[as.numeric(as.vector(e$duration)) > duration_min,] # & !incl_quality_fail
  if (only_finished & !incl_quality_fail) e <- e[e$finished==1,] 
  # if (only_finished | incl_quality_fail) { # TODO: le faire marcher même pour les autres
    e <- convert(e, country = country, wave = wave, weighting = weighting, zscores = zscores, zscores_dummies = zscores_dummies, efa = efa, combine_age_50 = combine_age_50, only_finished = only_finished, define_var_lists = define_var_lists)
    e <- e[,!duplicated(names(e))]
    # if (!incl_quality_fail) e <- e[e$attention_test == T, ] # TODO!
    if (weighting) {
      e$weight <- weighting(e, sub("[0-9p]+", "", country))
      e$weight_all <- weighting(e, sub("[0-9p]+", "", country), variant = "all")
      if (("vote_us" %in% names(e) & (sum(e$vote_us=="PNR/no right")!=0)) | ("vote" %in% names(e))) e$weight_vote <- weighting(e, sub("[0-9]+[a-z]*", "", country), variant = "vote")
      if (country == "EU") { for (c in countries_EU) e$weight_country[e$country == c] <- weighting(e[e$country == c,], c) } else e$weight_country <- e$weight
    }
    
  # e$left_right_na <- as.numeric(e$left_right)
  # e$left_right_na[e$indeterminate == T] <- wtd.mean(e$left_right, weights = e$weight)
  # } else e <- create_education(e, country, only = TRUE)
  
  if ("attention_test" %in% names(e)) {
    e$failed_test <- no.na(e$attention_test) != "A little"
    label(e$failed_test) <- "failed_test: Failed the attention_test"
    e$valid <- (as.numeric(e$progress) > 1) & (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded)
    label(e$valid) <- "valid: Respondents that has not been screened out due to speed or failure to the attention test."
    e$dropout <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded) & e$finished != "1"
    label(e$dropout) <- "dropout: Respondent who did not complete the survey though was not excluded."
    if (country %in% c("US1", "US2", "EU")) {
      progress_socio <- if (country == "US1") 19 else { if (country == "US2") 14 else 15 }
      # max(as.numeric(e$progress[is.na(e$gcs_win_lose) & (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded) & e$finished != "1"]))
      e$dropout_late <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded) & e$finished != "1" & n(e$progress) >= progress_socio
      label(e$dropout_late) <- "dropout: Respondent who did not complete the survey though was not excluded, and who dropped out after the socio-demographic questions." }
    e$finished_attentive <- (e$valid | (e$duration <= duration_min & e$attention_test=="A little")) & e$finished==1
    label(e$finished_attentive) <- "finished_attentive: Respondent completed the survey and did not fail the attention test."
    e$stayed <- !e$dropout & no.na(e$excluded) != "QuotaMet"
    label(e$stayed) <- "stayed: T/F quotas are allowed and do not drop out"
  }
  # e$sample <- "a"
  # e$sample[e$finished=="True"] <- "e"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min] <- "p"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min & e$excluded==""] <- "r"
  
  return(e)
}

convert <- function(e, country, wave = NULL, weighting = T, zscores = T, zscores_dummies = FALSE, efa = FALSE, combine_age_50 = T, only_finished = T, define_var_lists = T) {
  text_pnr <<- c("US" = "I don't know", "US" = "Prefer not to say",  "US" = "Don't know, or prefer not to say",  "US" = "Don't know",  "US" = "Don't know or prefer not to say", "US" = "I don't know",
                "US" = "Don't know, prefer not to say",  "US" = "Don't know, or prefer not to say.",  "US" = "Don't know,  or prefer not to say", "US" = "I am not in charge of paying for heating; utilities are included in my rent", "PNR",
                "FR" = "Je ne sais pas", "FR" = "Ne sais pas, ne souhaite pas répondre", "FR" = "NSP (Ne sais pas, ne se prononce pas)", "FR" = "NSP (Ne sait pas, ne se prononce pas)", "FR" = "Préfère ne pas le dire",
                "UK" = "I don't know", "DE" = "Ich weiß es nicht")
  text_yes <<- c("US" = "Yes", 
                "FR" = "Oui")
  text_no <<- c("No")
  text_intensity <<- c("Not at all", "A little", "Moderately", "A lot", "A great deal")
  text_support <<- c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")
  text_importance <<- c("Not at all important", "Not so important", "Quite important", "Very important")
  text_problem <<- c("Not an important issue for me", "An issue but there are other priorities", "An issue but we already do what we can", "An important issue, we should do more", "One of the most pressing issue of our time")
  if (wave == "pilot") text_problem[3] <<- "An issue but we do already what we can"
  text_negotiation <<- c("U.S. interests, even if it goes against global justice", "U.S. interests, to the extent it respects global justice", "Indifferent or don't know", "Global justice, to the extent it respects U.S. interests", "Global justice, even if it goes against U.S. interests")
  foreign_aid_amounts <<- c(.1, .2, .5, 1.0, 1.7, 2.6, 4, 6, 9, 13, 25)
  foreign_aid_means <<- c(0, .15, .4, .8, 1.4, 2.2, 3.35, 5, 7.5, 11, 19, 30)
  foreign_aid_actual_amounts <<- c("FR" = .8, "DE" = 1.3, "ES" = .5, "UK" = 1.7, "US" = .4)
  foreign_aid_actual_amounts_min <<- c("FR" = .6, "DE" = 1.1, "ES" = .3, "UK" = 1.1, "US" = .3)
  foreign_aid_actual_amounts_max <<- c("FR" = 1, "DE" = 1.7, "ES" = .5, "UK" = 1.7, "US" = .5)
  names(foreign_aid_means) <- c(foreign_aid_amounts, 30) # sub(".0", "", temp)
  
  if (only_finished) {
    if ("petition_gcs" %in% names(e)) {
      e$branch_petition[!is.na(e$petition_gcs)] <- "gcs"
      e$branch_petition[!is.na(e$petition_nr)] <- "nr"
      for (m in c("gcs", "nr")) e$petition[e$branch_petition == m] <- e[[paste0("petition_", m)]][e$branch_petition == m]
      label(e$branch_petition) <- "branch_petition: gcs/nr Whether the petition question is on the global climate scheme or national redistribution."
      label(e$petition) <- "petition: Yes/No Willing to sign a petition on gcs/nr (depends on branch_petition)."
      e$gcs_support_branch_petition_gcs <- 1*(e$gcs_support == "Yes")
      e$nr_support_branch_petition_nr <- 1*(e$nr_support == "Yes")
      e$gcs_support_branch_petition_gcs[e$branch_petition == "nr"] <- NA
      e$nr_support_branch_petition_nr[e$branch_petition == "gcs"] <- NA
    }
    
    if ("global_tax_sharing" %in% names(e)) {
      e$global_tax_sharing_original <- e$global_tax_sharing
      e$global_tax_sharing <- NA
      e$global_tax_sharing[grepl("whole wealth tax financing national budgets", e$global_tax_sharing_original)] <- FALSE
      e$global_tax_sharing[grepl("half of it financing low-income countries", e$global_tax_sharing_original)] <- T
      label(e$global_tax_sharing) <- "global_tax_sharing: T/F/NA Prefers to allocate half of global wealth tax to low-income countries rather than keeping all in collector country's national budgets. NA if the question is not asked (cf. branch_global_tax)."
      e$branch_global_tax[!is.na(e$global_tax_sharing)] <- "sharing"
      e$branch_global_tax[!is.na(e$global_tax_global_share)] <- "global_share"
      e$branch_global_tax[e$order_global_tax == 1] <- "global_first"
      e$branch_global_tax[e$order_national_tax == 1] <- "national_first"
      label(e$branch_global_tax) <- "branch_global_tax: global_first/national_first/sharing/global_share/NA Way to ask the preference for funding low-income countries through a global tax on the rich: either separately the support for a national and a global tax on millionaires (with either the global or national question asked first); whether to allocate half or none of the global tax to low-income countries; the 'global_share' in 0 to 100%."
      e$global_tax_more_30p <- e$global_tax_global_share >= 30
      label(e$global_tax_more_30p) <- "global_tax_more_30p: Wants at least 30% of global wealth tax revenues to fund low-income countries (defined from global_tax_global_share)."
      e$global_tax_more_10p <- e$global_tax_global_share >= 10
      label(e$global_tax_more_10p) <- "global_tax_more_10p: Wants at least 10% of global wealth tax revenues to fund low-income countries (defined from global_tax_global_share)."
      e$global_tax_more_half <- e$global_tax_global_share >= 50
      label(e$global_tax_more_half) <- "global_tax_more_half: Wants at least half of global wealth tax revenues to fund low-income countries (defined from global_tax_global_share)."
    }
  }

  if (define_var_lists) {
    variables_support <<- names(e)[grepl('support', names(e)) & !grepl("foreign_aid_raise_support|order_|ets2", names(e))]
    variables_other_policies <<- names(e)[grepl('_support', names(e)) & !grepl("nr|gcs|cgr|foreign_aid|_tax_|order_|ets2", names(e))]
    variables_climate_policies <<- variables_other_policies[grepl('climate', variables_other_policies)]
    variables_global_policies <<- variables_other_policies[!grepl('climate', variables_other_policies)]
    variables_support_binary_all <<- c("gcs_support", "nr_support", "cgr_support", "global_tax_sharing")
    variables_support_likert <<- c(variables_other_policies, "national_tax_support", "global_tax_support")
    variables_support_ets2_support <<- names(e)[grepl('ets2', names(e)) & grepl('support', names(e))]
    variables_support_ets2_no <<- names(e)[grepl('ets2_no_', names(e))]
    variables_petition <<- names(e)[grepl('petition', names(e)) & !grepl('branch_petition|order_', names(e))]
    variables_gcs_important <<- names(e)[grepl('gcs_important', names(e))]
    variables_problem <<- names(e)[grepl('problem_', names(e))]
    variables_win_lose <<- names(e)[grepl('win_lose', names(e))]
    variables_foreign_aid_amount <<- c("foreign_aid_belief", "foreign_aid_preferred_no_info", "foreign_aid_preferred_info", "foreign_aid_preferred")
    variables_duration <<- names(e)[grepl('duration', names(e))]
    variables_donation <<- c("donation_nation", "donation_africa", "donation")
    variables_list_exp <<- names(e)[grepl('list_exp', names(e))]
    if (wave != "pilot") variables_list_exp <<- c("list_exp_rgl", "list_exp_rl", "list_exp_gl", "list_exp_l")
    variables_belief <<- c("gcs_belief", "nr_belief")
    variables_points <<- names(e)[grepl("points", names(e)) & !grepl("order|duration", names(e))]
    variables_understood <<- paste0(c("nr", "gcs", "both", "score"), "_understood")
    
    variables_foreign_aid_raise <<- names(e)[grepl('foreign_aid_raise_how', names(e))]
    variables_foreign_aid_reduce <<- names(e)[grepl('foreign_aid_reduce_how', names(e))]
    variables_foreign_aid_no <<- names(e)[grepl('foreign_aid_no_', names(e)) & names(e) != "foreign_aid_no_other"]
    variables_foreign_aid_condition <<- names(e)[grepl('foreign_aid_condition', names(e)) & names(e) != "foreign_aid_condition_other"]
    
    variables_conjoint <<- names(e)[grepl('conjoint_', names(e)) & !grepl("order|duration", names(e))]
    variables_conjoint_a <<- c("conjoint_crg_cr")
    variables_conjoint_b <<- c("conjoint_cr_gr", "conjoint_r_rcg", "conjoint_rg_r", "conjoint_rc_r")
    variables_conjoint_c <<- c("conjoint_left_right", "conjoint_leftg_right")
    variables_conjoint_d <<- c("conjoint_left_a_b", "conjoint_left_ag_b")
    variables_conjoint_a_binary <<- c("conjoint_crg_cr_binary")
    variables_conjoint_b_binary <<- paste0(variables_conjoint_b, "_binary")
    variables_conjoint_c_binary <<- paste0(variables_conjoint_c, "_binary")
    variables_conjoint_d_binary <<- paste0(variables_conjoint_d, "_binary")
    variables_conjoint_binary <<- c(variables_conjoint_a_binary, variables_conjoint_b_binary, variables_conjoint_c_binary, variables_conjoint_d_binary)
    variables_conjoint_attr <<- paste0("F-1-", 1:5)
    variables_conjoint_r_levels <<- names(e)[grepl("F-1-1-|F-1-2-", names(e))]
    variables_conjoint_d_levels <<- names(e)[grepl("D-1-1-|D-1-2-", names(e))]
    variables_conjoint_r <<- c(variables_conjoint_attr, variables_conjoint_r_levels)
    variables_conjoint_d <<- c(variables_conjoint_attr, variables_conjoint_d_levels)
    variables_conjoint_levels <<- c(variables_conjoint_d_levels, variables_conjoint_r_levels)
    conjoint_position_g <<- c("A", "B", "B", "A", "None", "None", "A", "Random", "A")
    conjoint_position_1 <<- c("A", "B", "B", "A", "A", "Left", "Left", "A", "A")
    names(conjoint_position_g) <<- names(conjoint_position_1) <<- variables_conjoint
    
    variables_foreign_aid_amount_agg <<- paste0(variables_foreign_aid_amount, "_agg")
    variables_belief_agg <<- paste0(variables_belief, "_agg")
    variables_donation_agg <<- paste0(variables_donation, "_agg")
    variables_points_agg <<- paste0(variables_points, "_agg")
    
    variables_matrices <<- list("global_policies" = variables_global_policies,
                                "climate_policies" = variables_climate_policies,
                                "problem" = variables_problem,
                                "gcs_important" = variables_gcs_important)
    
  }
  if (country %in% c("US1", "US1p")) variables_points <- variables_points_us <<- names(e)[grepl("points", names(e)) & !grepl("order|duration", names(e))]
  if (country %in% c("US1", "US1p")) variables_points_us_agg <<- paste0(variables_points_us, "_agg")
  if (country == "EU") variables_ets2_support <<- names(e)[grepl("ets2", names(e)) & grepl("support", names(e))]
  if (country == "EU") variables_ets2_no <<- names(e)[grepl("ets2_no_", names(e))]
  variables_support_binary <<- c("gcs_support", "nr_support", "cgr_support") #, "global_tax_sharing")
  variables_belief_mep <<- c("belief_eu", "belief_us")
  variables_belief_mep_agg <<- paste0(variables_belief_mep, "_agg")
  
  for (i in intersect(c(variables_duration, "hh_size", "Nb_children__14", "zipcode", variables_donation, variables_belief, variables_belief_mep, variables_list_exp, variables_points, "global_tax_global_share" #, "age"
  ), names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector( gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in intersect(variables_duration, names(e))) e[[v]] <- e[[v]]/60
  
  for (j in intersect(c("couple", variables_petition, variables_support_binary), names(e))) {
    temp <- 1*(e[j][[1]] %in% text_yes) - 0.1*(e[j][[1]] %in% text_pnr) # - (e[j][[1]] %in% text_no)
    temp[is.na(e[j][[1]])] <- NA
    e[j][[1]] <- as.item(temp, labels = structure(c(0,-0.1,1), names = c("No","PNR","Yes")),
                         missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
    # e[j][[1]] <- as.item(as.character(e[j][[1]]), labels = structure(yes_no_names, names = c("NA","No","PNR","Yes")),
    #             missing.values = c("","PNR"), annotation=attr(e[j][[1]], "label"))
  }
  
  # for (j in intersect(c("region", "education", "employment_status", "vote" 
  # ), names(e))) {
  #   e[j][[1]] <- as.item(as.factor(e[j][[1]]), missing.values = c("PNR", "", NA), annotation=paste(attr(e[j][[1]], "label")))
  # }
  
  for (j in names(e)) {
    if ((grepl('race_|home_|foreign_aid_raise_how|foreign_aid_reduce_how|foreign_aid_condition|foreign_aid_no_|ets2_no_', j) & !(grepl('_other$|order_', j))) | grepl('how_other', j)) {
      temp <- label(e[[j]])
      e[[j]] <- e[[j]]!="" # e[[j]][e[[j]]!=""] <- TRUE
      e[[j]][is.na(e[[j]])] <- FALSE
      label(e[[j]]) <- temp
    }
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% c("A little")
  
  if (country != "MEP") {
  e$wave <- paste0(country, ifelse(wave == "pilot", "p", ""))
  e$country_name <- e$country
  if (grepl("US", country)) e$country_name <- "United States"
  e$country <- countries[e$country_name]
  # e$continent <- sub("[0-9p]+", "", e$wave)
  e$continent <- if (country == "EU") "Europe" else "U.S."
  
  e$woman <- e$gender == "Woman"
  e$man <- e$gender == "Man"
  temp <- as.numeric(as.vector(gsub("[^0-9\\.]", "", gsub(".*to", "", e$age_exact))))
  temp <- temp - 1.5
  temp[temp == 18.5] <- 19.5
  temp[temp == 22.5] <- 23
  temp[temp == 97.5] <- 95
  temp[temp == 98.5] <- 101
  e$age_exact <- as.item(temp, labels = structure(c(16.5, 19.5, 23, seq(27.5, 87.5, 5), 95, 101), names = c("< 18", "18-20", "21-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-99", "100+")), missing.values=c(NA), annotation=Label(e$age_exact))
  temp <- 21.5*(e$age_exact < 25) + 30*(e$age_exact < 35 & e$age_exact > 25) + 42.5*(e$age_exact < 50 & e$age_exact > 35) + 57.5*(e$age_exact < 65 & e$age_exact > 50) + 71*(e$age_exact > 65)
  e$age <- as.item(temp, labels = structure(c(21.5, 30, 42.5, 57.5, 71), names = c("18-24", "25-34", "35-49", "50-64", "65+")), missing.values=c(NA), annotation=Label(e$age_exact))
  e$age_factor <- as.factor(e$age) 
  
  if ("race_black" %in% names(e)) {
    e$race <- "Other"
    e$race[e$race_white==T & e$race_asian == FALSE & e$race_native == FALSE] <- "White only"
    e$race[e$race_hispanic==T] <- "Hispanic"
    e$race[e$race_black==T] <- "Black"
    if (any(e$race == "White only")) e$race <- relevel(as.factor(e$race), "White only")
    label(e$race) <- "race: White only/Hispanic/Black/Other. True proportions: .601/.185/.134/.08"
  }

  e$income_original <- e$income
  if (grepl("US", country)) {
    income_string <- c("< 20k", "20-35k", "35-42k", "42-50k", "50-65k", "65-82k", "82-103k", "103-130k", "130-145k", "145-165k", "165-250k", "> 250k")
    income_number <- c(15, 28, 39, 46, 58, 74, 92, 116, 142, 155, 205, 285)
    decile <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8, 9, 10)
    quartile <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
    names(income_number) <- names(decile) <- names(quartile) <- income_string
    temp <- as.numeric(income_number[as.vector(gsub("less than \\$", "< ", gsub("between \\$", "", gsub(",000", "k", gsub(",001 and \\$", "-", gsub("165,000 a", "165,001 a", gsub("more than \\$", "> ", e$income_original)))))))])
    e$income <- as.item(temp, labels = structure(income_number, names = income_string), missing.values=c(NA), annotation=Label(e$income))
    e$income_decile <- as.numeric(decile[as.character(e$income)])
    e$income_quartile <- as.numeric(quartile[as.character(e$income)])
  } else {
    e$income_quartile <- ceiling(as.numeric(as.vector(gsub("[^0-9\\.]", "", gsub("and.*", "", e$income_original))))/250)
    e$income_decile <- ceiling(as.numeric(as.vector(gsub("900", "901", gsub("[^0-9\\.]", "", gsub("and.*", "", e$income_original)))))/100)
  }
  e$income_factor <- as.factor(e$income_quartile)
  e$income_character <- paste0("Q", e$income_factor)
  label(e$income_decile) <- "income_decile: [1-10] Decile of income. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  label(e$income_quartile) <- "income_quartile: [1-4] Quartile of income. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  label(e$income_character) <- label(e$income_factor) <- "income_factor: 1/2/3/4 Quartile of income, as a factor rather than a numeric vector. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  
  if ("Nb_children__14" %in% names(e)) {
    e$children <- e$Nb_children__14 > 0
    label(e$children) <- "children: Lives with child(ren) below 14." }
  
  e$urban_category <- as.numeric(e$urban_category)
  e$urban_category[e$urban_category == 0] <- NA
  label(e$urban_category) <- "urban_category: [1-4] Computed from the zipcode. NA indicates an unrecognized zipcode. For FR/DE/ES, Eurostat's degree of urbanization (1: Cities, 2: Towns and suburbs, 3: Rural). For the UK we use another classification that tends to classify zipcodes as more rural than Eurostat (cf. zipcodes.R). For the US, recoded from RUCA codes (1: Metropolitan core (RUCA 1, 73% pop), 2: Metro non-core (2-3), 3: Micropolitan or Small town (< 50k, 4-9), 4: Rural (10))."
  temp <- as.numeric(as.vector(e$urban_category))
  if (grepl("US", country)) temp <- as.numeric(as.vector(e$urban_category - 1 * (e$urban_category > 2)))
  e$urbanity <- as.item(temp, labels = structure(1:3, names = c("Cities", "Towns and suburbs", "Rural")), missing.values=c(NA), annotation="urbanity: 1: Cities / 2: Towns and suburbs / 3: Rural. Computed from the zipcode. For EU, equals urban_category; for the U.S., urbanity = 1; 2; 3 (resp.) corresponds to urban_category = 1; 2 or 3; 4.")
  e$urban <- e$urban_category == 1
  e$urbanity <- as.factor(e$urbanity) # new
  label(e$urban) <- "urban: T/F urban_category == 1: Cities (EU) or Core metropolitan (US)."
  
  e$education_original <- e$education
  # ISCED_EU <- c("0-1", "2", "3 pro basic", "3 pro advanced", "3 general", "4-5", "6", "7-8")
  ISCED <- c("0-1", "2", "3.1", "3.2", "3.3", "4-5", "6", "7-8")
  names(ISCED) <- c("Primary school or less", "Eigth grade", "Some high school", "Regular high school diploma/GED or alternative credential", "Some college, no degree", "2-year college degree or associates degree (for example: AA, AS)", "Bachelor's degree (for example: BA, BS)", "Master’s degree or above (MA, MS, MEng, MEd, MSW, MBA, MD, DDS, DVM, LLB, JD, PhD)")
  e$education <- ISCED[e$education_original] # TODO create variable with shorter names and plot it
  label(e$education) <- "education: What is the highest level of education you have completed? /!\ For EU, the values don't correspond to the responses. To see the correspondence between values and responses in each country, cf. specificities.xlsx$Education"
  e$diploma[e$education %in% c("0-1", "2")] <- 1 
  e$diploma[grepl("3", e$education)] <- 2 
  e$diploma[e$education %in% c("4-5", "6", "7-8")] <- 3 
  e$diploma <- as.item(e$diploma, labels = structure(1:3, names = c("Below upper secondary", "Upper secondary", "Post secondary")), missing.values=c(NA, "Not 25-64"), annotation="diploma: 1: Below upper secondary (ISCED 0-2) / 2: Upper secondary (ISCED 3) / 3: Post secondary (ISCED 4-8), recoded from education.")
  e$post_secondary <- e$diploma > 2
  label(e$post_secondary) <- "post_secondary: Has a post-secondary degree (at least two years after highschool, i.e. Associate degree, BA, BS, etc.)."
  e$diploma_25_64 <- e$diploma
  e$diploma_25_64[e$age < 25 | e$age > 65] <- 0 # "Not 25-64"
  e$diploma_25_64 <- as.item(as.numeric(as.vector(e$diploma_25_64)), labels = structure(c(1:3, 0), names = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64")), missing.values=c(NA, 0), 
                             annotation="diploma_25_64: 0: Not 25-64 if age is not within 25-64 (missing value) / 1: Below upper secondary (ISCED 0-2) / 2: Upper secondary (ISCED 3) / 3: Post secondary (ISCED 4-8), recoded from education.")
  
  e$employment_status <- gsub(" \\(.*\\)", "", e$employment_status)
  
  e$employment_agg <-  "Not working"
  e$employment_agg[e$employment_status == "Student"] <- "Student"
  e$employment_agg[e$employment_status == "Retired"] <- "Retired"
  e$employment_agg[e$employment_status == "Self-employed" | e$employment_status == "Full-time employed" | e$employment_status == "Part-time employed"] <- "Working"
  e$employment_agg <- as.factor(e$employment_agg)
  label(e$employment_agg) <- "employment_agg: Not working (Inactive or Unemployed) / Student / Retired / Employed (full-time, part-time, or self-employed). Built from employment_status."
  
  e$inactive <- e$employment_agg %in% c("Retired", "Not working")
  e$employment <- e$employment_agg == "Working"
  e$employment[e$age == "65+"] <- NA
  label(e$employment) <- "employment: T/F/NA indicator that the respondent is employed (employment_agg == Working), NA if s-he is above 65."

  e$employment_18_64 <- "Employed"
  e$employment_18_64[e$employment_status %in% c("Unemployed")] <- "Unemployed"
  e$employment_18_64[e$employment_status %in% c("Inactive", "Student", "Retired")] <- "Inactive"
  e$employment_18_64[e$age > 64] <- "65+"
  label(e$employment_18_64) <- "employment_18_64: 65+ / Inactive (Inactive, Student or Retired) / Unemployed / Employed (full-time, part-time, or self-employed). Built from employment_status."
  
  if ("wealth_couple" %in% names(e)) {
    e$wealth[!is.na(e$wealth_couple)] <- e$wealth_couple[!is.na(e$wealth_couple)]
    e$wealth[!is.na(e$wealth_single)] <- e$wealth_single[!is.na(e$wealth_single)]
    temp <-  grepl("Less than \\$0", e$wealth) + 2 * grepl("Close to \\$0", e$wealth) + 3 * grepl("Between \\$4,000", e$wealth) + 4 * (e$wealth %in% c("Between $120,000 and $380,000", "Between $60,000 and $190,000")) + 5 * grepl("More than", e$wealth)
    e$wealth <- as.item(temp, labels = structure(c(1:5, 0), names = c("Q1","Q2","Q3","Q4","Q5", "PNR")), missing.values = c(NA, 0), annotation="wealth: Quintile of wealth (from wealth_couple and wealth_single).")
    e$wealth_factor <- as.factor(e$wealth)
  }
  
  e$owner <- e$home_owner == T | e$home_landlord == T
  label(e$owner) <- "owner: Owner or Landlord renting out property to: Are you a homeowner or a tenant?"
  
  thresholds_duration <- c(0, if (country == "EU") 6 else 4, 10, 15, 20, 25, 30, Inf) 
  labels_duration <- agg_thresholds(e$duration, thresholds_duration, return = "levels")
  if (sum(e$duration < if (country == "EU") 6 else 4) > 0) {
    labels_duration[c(1,2)] <- c("< min", "min - 10")
    e$duration_agg <- agg_thresholds(e$duration, thresholds_duration, labels = labels_duration)
  } else {
    labels_duration[c(1,2)] <- c("< min", "< 10")
    e$duration_agg <- agg_thresholds(e$duration, thresholds_duration[2:length(thresholds_duration)], labels = labels_duration[2:length(labels_duration)])
  }
  
  if (only_finished) {
    if ("list_exp_rgl" %in% names(e)) {
      e$branch_list_exp[!is.na(e$list_exp_l)] <- "l"
      e$branch_list_exp[!is.na(e$list_exp_rgl)] <- "rgl"
      e$branch_list_exp[!is.na(e$list_exp_gl)] <- "gl"
      e$branch_list_exp[!is.na(e$list_exp_rl)] <- "rl"
      if (wave == "pilot" & country != "US2") e$list_exp_rl[e$branch_list_exp == "gl" & e$country %in% c("US", "FR", "UK")] <- e$list_exp_gl[e$branch_list_exp == "gl" & e$country %in% c("US", "FR", "UK")]
      if (wave == "pilot" & country != "US2") e$list_exp_gl[e$branch_list_exp == "gl" & e$country %in% c("US", "FR", "UK")] <- NA
      if (wave == "pilot" & country != "US2") e$branch_list_exp[e$branch_list_exp == "gl" & e$country %in% c("US", "FR", "UK")] <- "rl"
      label(e$branch_list_exp) <- "branch_list_exp: control/rl/gl/rgl Variant of the list experiment faced, where l denotes coal exit (US) or the buildings' insulation plan (EU), r the national redistribution, and g the global climate scheme. Marriage only for opposite-sex couples (US) and death penalty for major crimes (EU) were also systematically included."
      e$branch_list_exp_g <- grepl("g", e$branch_list_exp)
      e$branch_list_exp_r <- grepl("r", e$branch_list_exp)
      label(e$branch_list_exp_r) <- "branch_list_exp_r: T/F r (national redistribution) is present in the list experiment."
      label(e$branch_list_exp_g) <- "branch_list_exp_g: T/F g (global climate scheme) is present in the list experiment."
      for (v in c("l", "rl", "gl", "rgl")) e$list_exp[e$branch_list_exp == v] <- e[[paste0("list_exp_", v)]][e$branch_list_exp == v]
      label(e$list_exp) <- "list_exp: [0-4] Number of supported policies in the list experiment (combining all branches, cf. branch_list_exp and variables_list_exp)."
      e$branch_list_exp_ict <- e$branch_list_exp
      e$branch_list_exp_ict[e$branch_list_exp == "l"] <- "control"
      e$branch_list_exp_ict <- as.factor(e$branch_list_exp_ict)
      e$branch_list_exp_ict <-  1*(e$branch_list_exp == "gl") + 2*(e$branch_list_exp == "rl") + 3*(e$branch_list_exp == "rgl")
    }
    
    for (v in c(variables_support_likert, variables_support_ets2_support)) {
      if (v %in% names(e)) {
        temp <-  temp <- 2 * (e[[v]] %in% text_support[5]) + (e[[v]] %in% text_support[4]) - (e[[v]] %in% text_support[2]) - 2 * (e[[v]] %in% text_support[1])
        temp[is.na(e[[v]])] <- NA
        e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")), missing.values=c(NA), annotation=Label(e[[v]])) 
      } }
    
    for (v in intersect(variables_gcs_important, names(e))) {
      temp <-  temp <- 2 * (e[[v]] %in% text_importance[4]) + (e[[v]] %in% text_importance[3]) - (e[[v]] %in% text_importance[2]) - 2 * (e[[v]] %in% text_importance[1])
      temp[is.na(e[[v]])] <- NA
      e[[v]] <- as.item(temp, labels = structure(c(-2,-1,1,2), names = text_importance), #sub(" important", "", text_importance)), 
                        missing.values=c(NA), annotation=Label(e[[v]]))    
      e$branch_gcs_perception <- "field"
      e$branch_gcs_perception[!is.na(e[[v]])] <- "gcs_important"
      if (country == "EU") e$branch_gcs_field <- e$branch_gcs_perception == "field"
      label(e$branch_gcs_perception) <- "branch_gcs_perception: field/gcs_important/NA Whether the perception of the global climate scheme is asked as a matrix 'gcs_important' or an entry field 'field'"
    }
    
    for (v in intersect(variables_problem, names(e))) {
      temp <- 2 * (e[[v]] %in% text_problem[5]) + (e[[v]] %in% text_problem[4]) - (e[[v]] %in% text_problem[2]) - 2 * (e[[v]] %in% text_problem[1])
      temp[is.na(e[[v]])] <- NA
      e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Not an issue", "Not a priority", "Already addressed", "Important, should do more", "Most pressing issue")), missing.values=c(NA), annotation=Label(e[[v]]))    
    }
    
    if ("negotiation" %in% names(e) & wave != "pilot") {
      e$negotiation_original <- e$negotiation
      temp <- 2 * (e$negotiation_original %in% text_negotiation[5]) + (e$negotiation_original %in% text_negotiation[4]) - (e$negotiation_original %in% text_negotiation[2]) - 2 * (e$negotiation_original %in% text_negotiation[1])
      e$negotiation <- as.item(temp, labels = structure(c(-2:2), names = c("Only [Country] interest", "[Country] then global", "Indifferent or don't know", "Global then [Country]", "Only global justice")
                                                          # works: c("[Country] interest, not global justice", "[Country] interest, with global justice", "Indifferent or don't know", "Global justice, with [Country] interest", "Global justice, not [Country] interest") 
                                                        # too long: c("[Country] interest, even against global justice", "[Country] interest, respecting global justice", "Indifferent or don't know", "Global justice, respecting [Country] interest", "Global justice, even against [Country] interest")
                                                        ), missing.values=c(NA), annotation=Label(e$negotiation_original))  
      e$negotiation_only_country <- e$negotiation == -2
      e$negotiation_country_respecting <- e$negotiation == -1
      e$negotiation_global_before <- e$negotiation > 0
      label(e$negotiation_only_country) <- "negotiation_only_country: Respondents favors Only [Country] interest in negotiation."
      label(e$negotiation_country_respecting) <- "negotiation_country_respecting: Respondents favors [Country] then global in negotiation."
      label(e$negotiation_global_before) <- "negotiation_global_before: Respondents favors Global then [Country] or Only global justice in negotiation."
    }
    
    if ("foreign_aid_preferred_info" %in% names(e)) {
      e$branch_foreign_aid_preferred[!is.na(e$foreign_aid_preferred_info)] <- "Info"
      e$branch_foreign_aid_preferred[!is.na(e$foreign_aid_preferred_no_info)] <- "No info"
      e$info_foreign_aid <- e$branch_foreign_aid_preferred == "Info"
      e$foreign_aid_preferred[e$info_foreign_aid == T] <- e$foreign_aid_preferred_info[e$info_foreign_aid == T]
      e$foreign_aid_preferred[e$info_foreign_aid == FALSE] <- e$foreign_aid_preferred_no_info[e$info_foreign_aid == FALSE]
      label(e$branch_foreign_aid_preferred) <- "branch_foreign_aid_preferred: Info/No info Whether the info on the actual amount of foreign aid was given before asking the preferred amount."
      label(e$info_foreign_aid) <- "info_foreign_aid: T/F Whether the info on the actual amount of foreign aid was given before asking the preferred amount."
      label(e$foreign_aid_preferred) <- "foreign_aid_preferred: 0/.15/.4/.8/1.4/2.2/3.35/5/7.5/11/19/30 Amount of preferred foreign aid (mean of interval in % of nation's public spending). Depending on info_foreign_aid, info on actual amount is randomly given or not beforehand."
    }
    
    for (v in intersect(variables_foreign_aid_amount, names(e))) {
      e[[paste0(v, "_original")]] <- e[[v]]
      temp <- as.numeric(gsub("[^0-9\\.]", "", gsub(".*to", "", e[[v]])))
      temp[grepl("More", e[[v]])] <- 30
      temp <- foreign_aid_means[sub(".0", "", temp)]
      e[[v]] <- as.item(temp, labels = structure(c(foreign_aid_means), names = c("< 0.1%", "0.1 to 0.2", "0.3 to 0.5", "0.6 to 1", "1.1 to 1.7", "1.8 to 2.6", "2.7 to 4", "4.1 to 6", "6.1 to 9", "9.1 to 13", "13.1 to 25", "> 25%")# foreign_aid_amounts
      ), annotation = Label(e[[v]])) 
    }
    
    if ("foreign_aid_preferred" %in% names(e)) {
      e$foreign_aid_more_less[e$info_foreign_aid == T] <- ((e$foreign_aid_preferred > foreign_aid_actual_amounts_max[e$country]) - (e$foreign_aid_preferred < foreign_aid_actual_amounts_min[e$country]))[e$info_foreign_aid == T]
      e$foreign_aid_more_less[e$info_foreign_aid == FALSE] <- ((e$foreign_aid_preferred > e$foreign_aid_belief) - (e$foreign_aid_preferred < e$foreign_aid_belief))[e$info_foreign_aid == FALSE]
      e$foreign_aid_more_less <- as.item(e$foreign_aid_more_less, labels = structure(-1:1, names = c("Less", "Same", "More")), missing.values = NA, annotation = "foreign_aid_more_less: -1: Less / 0: Same / 1: More. Whether the respondent wants more or less foreign aid than now. Depending on info_foreign_aid = T or F, current aid is taken as the actual or the believed one.")
      e$foreign_aid_more_less_info <- e$foreign_aid_more_less_no_info <- e$foreign_aid_more_less
      e$foreign_aid_more_less_info[e$info_foreign_aid == FALSE] <- NA 
      e$foreign_aid_more_less_no_info[e$info_foreign_aid == T] <- NA
      e$foreign_aid_no_less_info <- e$foreign_aid_more_less_info >= 0
      e$foreign_aid_no_less_no_info <- e$foreign_aid_more_less_no_info >= 0
      e$foreign_aid_less_more_info <- as.item(-e$foreign_aid_more_less_info, labels = structure(-1:1, names = c("More", "Same", "Less")), missing.values = NA, annotation = "foreign_aid_more_less: 1: Less / 0: Same / -1: More. Whether the respondent wants more or less foreign aid than now. Depending on info_foreign_aid = T or F, current aid is taken as the actual or the believed one.")
      e$foreign_aid_less_more_no_info <- as.item(-e$foreign_aid_more_less_no_info, labels = structure(-1:1, names = c("More", "Same", "Less")), missing.values = NA, annotation = "foreign_aid_more_less: 1: Less / 0: Same / -1: More. Whether the respondent wants more or less foreign aid than now. Depending on info_foreign_aid = T or F, current aid is taken as the actual or the believed one.")
    }
    for (v in intersect(names(e), variables_foreign_aid_reduce)) e[[v]][e$info_foreign_aid == FALSE | e$foreign_aid_more_less >= 0] <- NA
    for (v in intersect(names(e), variables_foreign_aid_raise)) e[[v]][e$info_foreign_aid == FALSE | e$foreign_aid_more_less <= 0] <- NA
    
    e$foreign_aid_actual <- foreign_aid_actual[e$country]
    label(e$foreign_aid_actual) <- "foreign_aid_actual: [Constant] Actual amount of foreign aid in the country, in proportion of public spending."
    
    if ("foreign_aid_raise_support" %in% names(e)) {
      e$foreign_aid_raise_support_original <- e$foreign_aid_raise_support
      temp <- -1 * grepl("reduce", e$foreign_aid_raise_support) + 1*grepl("condition", e$foreign_aid_raise_support) + 2*grepl("increase", e$foreign_aid_raise_support)
      e$foreign_aid_raise_support <- as.item(temp, labels = structure(-1:2, names = c("No, should be reduced", "No, should remain stable", "Yes, but at some conditions", "Yes, should be increased")), missing.values = NA, annotation = Label(e$foreign_aid_raise_support))     
      e$foreign_aid_reduce_support <- as.item(-temp, labels = structure(-2:1, names = rev(c("No, should be reduced", "No, should remain stable", "Yes, but at some conditions", "Yes, should be increased"))), missing.values = NA, annotation = Label(e$foreign_aid_raise_support))
    }
    for (v in intersect(names(e), variables_foreign_aid_no)) e[[v]][e$foreign_aid_raise_support > 0] <- NA
    for (v in intersect(names(e), variables_foreign_aid_condition)) e[[v]][e$foreign_aid_raise_support != 1] <- NA
    
    for (v in intersect(variables_win_lose, names(e))) e[[paste0(v, "_original")]] <- e[[v]]
    if ("nr_win_lose" %in% names(e)) {
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would lose and the richest Americans would lose."] <- "Typical lose, richest lose"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would lose and the richest Americans would win."] <- "Typical lose, richest win"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would win and the richest Americans would lose."] <- "Typical win, richest lose"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would win and the richest Americans would win."] <- "Typical win, richest win"
      e$nr_understood <- e$nr_win_lose == "Typical win, richest lose"
      label(e$nr_understood) <- "nr_understood: T/F Correct answer to nr_win_lose (in national redistribution, typical people win, richest lose)."
    }
    
    if ("gcs_support" %in% names(e) & country != "US2") e$gcs_support_neg <- 2*e$gcs_support - 1
    
    if ("gcs_win_lose" %in% names(e)) {
      e$gcs_win_lose[e$gcs_win_lose == "Typical Americans would lose and the 700 million poorest humans would lose."] <- "Typical lose, poorest lose"
      e$gcs_win_lose[e$gcs_win_lose == "Typical Americans would lose and the 700 million poorest humans would win."] <- "Typical lose, poorest win"
      e$gcs_win_lose[e$gcs_win_lose == "Typical Americans would win and the 700 million poorest humans would lose."] <- "Typical win, poorest lose"
      e$gcs_win_lose[e$gcs_win_lose == "Typical Americans would win and the 700 million poorest humans would win."] <- "Typical win, poorest win"
      e$gcs_understood <- e$gcs_win_lose == "Typical lose, poorest win"
      label(e$gcs_understood) <- "gcs_understood: T/F Correct answer to gcs_win_lose (in global climate scheme, typical high-income win, 700M poorest lose)."
    }
    
    if ("both_win_lose" %in% names(e)) {
      e$both_win_lose[e$both_win_lose == "A typical American would gain financially."] <- "Win"
      e$both_win_lose[e$both_win_lose == "A typical American would lose out financially."] <- "Lose"
      e$both_win_lose[e$both_win_lose == "A typical American would neither gain nor lose."] <- "Unaffected"
      e$both_understood <- e$both_win_lose == "Unaffected"
      label(e$both_understood) <- "both_understood: T/F Correct answer to both_win_lose (in national redistribution + global climate scheme combined, typical people in high-income countries are unaffected)."
    }
    
    e$score_understood <- as.numeric(e$nr_understood + e$gcs_understood + e$both_understood)
    label(e$score_understood) <- "score_understood: [0-3] Number correct answers to understanding questions (nr/gcs/both_understood)."
    e$z_score_understood <- (e$score_understood - mean(e$score_understood)) / sd(e$score_understood) # TODO weighted
    label(e$z_score_understood) <- "z_score_understood: Normalized score_understood."
    
    if ("donation_nation" %in% names(e) & sum(!is.na(e$donation_nation)) != 0) {
      e$branch_donation[!is.na(e$donation_africa)] <- "Africa"
      e$branch_donation[!is.na(e$donation_nation)] <- "Own nation"
      e$donation[replace_na(e$branch_donation, "na") == "Africa"] <- e$donation_africa[replace_na(e$branch_donation, "na") == "Africa"]
      e$donation[replace_na(e$branch_donation, "na") == "Own nation"] <- e$donation_nation[replace_na(e$branch_donation, "na") == "Own nation"]
      label(e$branch_donation) <- "branch_donation: Africa/Own nation Whether the donation is for poor people in Africa or in one's nation."
      label(e$donation) <- "donation: [0-100] Percentage of potential lottery gain donated to poor people in Africa or in one's nation (depending on branch_donation)."
      e$donation_above_25 <- e$donation > 25
      label(e$donation_above_25) <- "donation_above_25: T/F Percentage of potential lottery gain donated to poor people in Africa or in one's nation is above 25% (depending on branch_donation)."
    }
    
    for (v in intersect(c("gcs_support", "nr_support"), names(e))) e[[paste0(v, "_100")]] <- 100 * e[[v]]
    
    for (v in intersect(names(e), c(variables_conjoint))) {
      e[[v]] <- sub(".* (.*)", "\\1", e[[v]])
      e[[v]][e[[v]] == "them"] <- "None"
      if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Democrat", "A")] <- "Left"
      if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Republican", "B")] <- "Right"
      if (v %in% variables_conjoint_a) e$conjoint_a <- e[[v]] == conjoint_position_g[v]
      if (v %in% variables_conjoint_b) e$conjoint_b[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == conjoint_position_g[v]
      if (v %in% variables_conjoint_b) e$branch_conjoint_b[!is.na(e[[v]])] <- sub("conjoint_", "", v)
      if (v %in% variables_conjoint_c) e$conjoint_c[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "Left" #conjoint_position_g[v]
      if (v %in% variables_conjoint_c) e$conjoint_c_right[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "Right"
      if (v %in% variables_conjoint_c) e$conjoint_c_none[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "None"
      if (v %in% variables_conjoint_c) e$branch_conjoint_c[!is.na(e[[v]])] <- sub("conjoint_", "", v)
      if (v %in% "conjoint_left_ag_b") e$conjoint_d[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == conjoint_position_g[v]
      if (v == "conjoint_rc_r") e$conjoint_b[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "A"
      e[[paste0(v, "_binary")]] <- e[[v]] == conjoint_position_1[v]
      label(e[[paste0(v, "_binary")]]) <- paste0(v, "binary: 0/1 Binary indicator of the variable, cf. conjoint_position_1 to see which among A or B is indicated.")
    }
    if ("conjoint_a" %in% names(e)) {
      label(e$branch_conjoint_b) <- "branch_conjoint_b: cr_gr/r_rcg/rc_r/rg_r Random branch faced by the respondent in conjoint analysis (b)."
      label(e$branch_conjoint_c) <- "branch_conjoint_c: left_right/leftg_right Random branch faced by the respondent in conjoint analysis (c), used in branch_c_gcs := branch_conjoint_c == 'leftg_right.'"
      label(e$conjoint_a) <- "conjoint_a: T/F Bundle with GCS is chosen in conjoint analysis (a), i.e. rcg > cr."
      label(e$conjoint_b) <- "conjoint_b: T/F Bundle with GCS is chosen in conjoint analysis (b) when GCS is in one Bundle, or cr > r in case GCS is in no bundle."
      label(e$conjoint_c) <- "conjoint_c: T/F Left-wing candidate is chosen in conjoint_c (cf. branch_c_gcs to know whether the Left candidate includes GCS in their platform)."
      label(e$conjoint_c_right) <- "conjoint_c: T/F Right-wing candidate is chosen in conjoint_c (cf. branch_c_gcs to know whether the Left candidate includes GCS in their platform)."
      label(e$conjoint_c_none) <- "conjoint_c: T/F None candidate is chosen in conjoint_c (cf. branch_c_gcs to know whether the Left candidate includes GCS in their platform)."
      e$conjoint_d[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "A"
      label(e$conjoint_d) <- "conjoint_d: T/F Candidate with GCS is chosen in conjoint analysis (d). In US1, question asked only to non-Republican."
      e$branch_b_gcs <- grepl("g", e$branch_conjoint_b)
      label(e$branch_b_gcs) <- "branch_b_gcs: T/F Whether GCS is in one Bundle in conjoint_b, i.e. =F iff branch_conjoint_b == 'conjoint_rc_r'."
      e$conjoint_b_na <- e$conjoint_b
      e$conjoint_b_na[e$branch_b_gcs == FALSE] <- NA
      label(e$conjoint_b_na) <- "conjoint_b_na: T/F/NA Bundle with GCS is chosen in conjoint analysis (b) when GCS is in one Bundle, NA in case GCS is in no bundle (i.e. branch_b_gcs == F i.e. branch_conjoint_b == 'conjoint_rc_r'."
      e$branch_c_gcs <- grepl("g_", e$branch_conjoint_c)
      label(e$branch_c_gcs) <- "branch_c_gcs: T/F Whether GCS is in the Left-wing platform in conjoint_c, i.e. =T iff branch_conjoint_c == 'conjoint_leftg_right'."
      e$conjoint_r_type <- temp <- "None"
      for (i in 1:5) {
        e$conjoint_r_type[e[[paste0("F-1-1-", i)]] %in% policies.names["foreign1",]] <- temp[e[[paste0("F-1-1-", i)]] %in% policies.names["foreign1",]] <- "A"
        e$conjoint_r_type[e[[paste0("F-1-2-", i)]] %in% policies.names["foreign1",]] <- "B" }
      e$conjoint_r_type[e$conjoint_r_type == "B" & temp == A] <- "Both"
      label(e$conjoint_r_type) <- "conjoint_r_type: None/A/B/Both Which candidate includes GCS in their program in conjoint_left_a_b"
      e$conjoint_r <- e$conjoint_left_a_b == e$conjoint_r_type
      e$conjoint_r[e$conjoint_r_type %in% c("None", "Both")] <- NA
      label(e$conjoint_r) <- "conjoint_r: T/F/NA Whether the candidate who includes GCS in their program is preferred (NA if both or none include GCS). In US1, question asked only to non-Republican."
      
      # conjoint_attributes <- c("Economic issues", "Societal issues", "Climate policy", "Tax system", "Foreign policy")
      # if (!grepl("EU", country)) for (v in variables_conjoint_attr) e[[v]] <- conjoint_attributes[e[[v]]]
      for (v in c(variables_conjoint_r, variables_conjoint_d_levels)) {
        e[[paste0(v, "_original")]] <- e[[v]]
        temp <- sapply(e[[v]], function(i) { which(policies.names==i)[1] %% 20 })
        temp[temp == 0] <- 20
        e[[v]] <- row.names(policies.names)[temp]
        e[[v]][e[[paste0(v, "_original")]] == "-"] <- "-"
        e[[v]][e[[paste0(v, "_original")]] == "Increase corporate income tax rate from 21% to 28%"] <- "tax3"
        e[[v]][e[[paste0(v, "_original")]] == "Making abortion a right at the federal level"] <- "soc3"
      } 
      
      e$conjoint_r_wrong_level <- FALSE
      unrecognized_levels <- c()
      for (v in c(variables_conjoint_r_levels)) {
        e$conjoint_r_wrong_level[is.na(e[[v]]) & !is.na(e[[paste0(v, "_original")]])] <- T
        # respondents_with_wrong_levels <- c(respondents_with_wrong_levels, which(is.na(e[[v]]) & !is.na(e[[paste0(v, "_original")]])))
        unrecognized_levels <- c(unrecognized_levels, e[[paste0(v, "_original")]][is.na(e[[v]]) & !is.na(e[[paste0(v, "_original")]])])
      }
      # respondents_with_wrong_levels <- unique(respondents_with_wrong_levels)
      unrecognized_levels <- unique(unrecognized_levels) # In EUp there are 58 respondents affected: 55 in UK, 1 in DE, 2 in ES. This is because we updated the policies names in the meantime.
      if (length(unrecognized_levels) > 0) print(paste("There are", length(unrecognized_levels), "unrecognized levels in conjoint analysis (r).", sum(e$conjoint_r_wrong_level), "respondents are affected."))
      e$conjoint_r_number <- 1*(e$conjoint_left_a_b == "A") + 2*(e$conjoint_left_a_b == "B")
      e$conjoint_r_number[e$conjoint_r_wrong_level==T | is.na(e$`F-1-1-1`)] <- NA
      label(e$conjoint_r_number) <- "conjoint_r_number: 1/2/NA (instead of A/B) Candidate with GCS is chosen in conjoint analysis (r). In US1, question asked only to non-Republican. NA if question not asked or there is some unrecognized level (conjoint_r_wrong_level == T)."
    }
    
    if ("ets2_no_revenue_use" %in% names(e)) {
      e$ets2_oppose <- pmax(e$ets2_equal_cash_support, e$ets2_country_cash_support, e$ets2_investments_support, e$ets2_vulnerable_investments_support) <= 0
      label(e$ets2_oppose) <- "ets2_oppose: T/F/NA Does not support (somewhat or strongly) any of the four ETS2 variants proposed. NA if ETS2 support is not asked (i.e. if country == UK)"
      for (v in variables_ets2_no) e[[v]][e$ets2_oppose == F] <- NA
    }
    
    if ("iat_lp5" %in% names(e)) {
      e$branch_iat <- NA
      e$branch_iat[!is.na(e$iat_ln1)] <- "LN"
      e$branch_iat[!is.na(e$iat_lp1)] <- "LP"
      e$branch_iat[!is.na(e$iat_rn1)] <- "RN"
      e$branch_iat[!is.na(e$iat_rp1)] <- "RP"
      label(e$branch_iat) <- "branch_iat: LN/LP/RN/RP/NA Branch of the Implicit Association Test (Left vs. Right & Positive vs. Negative). NA for those without IAT (because their device is a phone or their language Spanish)."
    }
    
    if ("branch_gcs_info" %in% names(e)) {
      for (m in c("info", "nothing", "field", "important")) e$branch_gcs[!is.na(e[[paste0("branch_gcs_", m)]])] <- m
      for (m in c("info", "nothing", "field", "important")) e[[paste0("branch_gcs_", m)]] <- !is.na(e[[paste0("branch_gcs_", m)]])
      e$branch_gcs_perception <- e$branch_gcs <- relevel(as.factor(e$branch_gcs), "nothing")
      label(e$branch_gcs) <- "branch_gcs: info/nothing/field/important Whether gcs/nr_support is preceded by the info on the actual support, nothing, gcs_field or variables_gcs_important."
    }
    
    if ("interested_politics" %in% names(e)) temp <- 2 * (e$interested_politics %in% text_intensity[5]) + (e$interested_politics %in% text_intensity[4]) - (e$interested_politics %in% text_intensity[2]) - 2 * (e$interested_politics %in% text_intensity[1])
    if ("interested_politics" %in% names(e)) e$interested_politics <- as.item(temp, labels = structure(c(-2:2),  names = c(text_intensity)), annotation=Label(e$interested_politics))
    
    if ("group_defended" %in% names(e)) {
      e$group_defended_original <- e$group_defended # TODO! assign all but one "My town" to "My State" in US.
      temp <- 0*grepl("myself", e$group_defended) + 1*grepl("relatives", e$group_defended) + 2*grepl("town|State", e$group_defended) + 3*grepl("religion", e$group_defended) + 4*grepl("Americans", e$group_defended) + 5*grepl("European", e$group_defended) + 6*grepl("Humans", e$group_defended) + 7*grepl("animals", e$group_defended)
      e$group_defended <- as.item(temp, labels = structure(0:7, names = c("Family and self", "Relatives", "Region, U.S. State or town", "Culture or religion", "Fellow citizens", "Europeans", "Humans", "Sentient beings")), annotation = Label(e$group_defended))
      e$nationalist <- e$group_defended == 4
      e$egoistic <- e$group_defended == 0
      e$universalist <- e$group_defended > 5
      label(e$nationalist) <- "nationalist: T/F Defends one's nation / fellow citizens when one votes (cf. group_defended)."
      label(e$universalist) <- "universalist: T/F Defends humans or sentient beings (humans and animals) when one votes (cf. group_defended)."
      label(e$egoistic) <- "egoistic: T/F Defends one's self and family when one votes (cf. group_defended)."
      e$group_defended_agg <- sign(e$group_defended - 4)
      e$group_defended_agg[e$group_defended == 0] <- -2
      e$group_defended_agg[e$group_defended == 5] <- 0
      e$group_defended_agg <- as.item(e$group_defended_agg, labels = structure(-2:1, names = c("Family and self", "Group of related people", "Fellow citizens or Europeans", "Humans or sentient beings") #c("Egoistic", "Tribalist", "Nationalist", "Universalist")
                                                                             ), annotation = "group_defended_agg: Group defended when one votes, where 'Group of related people' gathers My relatives and/or colleagues, My town, My State/region, People sharing my culture or religion; where 'Fellow citizens or Europeans' gathers [Country] and Europeans; and where 'Humans or sentient beings' gathers Humans and Sentient beings (humans or animals)")
      e$group_defended_agg5 <- as.numeric(e$group_defended_agg)
      e$group_defended_agg5[e$group_defended == 7] <- 2
      e$group_defended_agg5 <- as.item(e$group_defended_agg5, labels = structure(-2:2, names = c("Family and self", "Group of related people", "Fellow citizens or Europeans", "Humans", "Sentient beings") #c("Egoistic", "Tribalist", "Nationalist", "Universalist")
            ), annotation = "group_defended_agg5: Group defended when one votes, where 'Group of related people' gathers My relatives and/or colleagues, My town, My State/region, People sharing my culture or religion; where 'Fellow citizens or Europeans' gathers [Country] and Europeans")
      e$group_defended_agg6 <- as.numeric(e$group_defended_agg5)
      e$group_defended_agg6[e$group_defended == 5] <- 0.5
      e$group_defended_agg6 <- as.item(e$group_defended_agg6, labels = structure(c(-2:0,0.5,1,2), names = c("Family and self", "Group of related people", "Fellow citizens", "Europeans", "Humans", "Sentient beings") #c("Egoistic", "Tribalist", "Nationalist", "Universalist")
      ), annotation = "group_defended_agg6: Group defended when one votes, where 'Group of related people' gathers My relatives and/or colleagues, My town, My State/region, People sharing my culture or religion.")
      e$group_defended_agg2 <- as.numeric(e$group_defended_agg5)
      e$group_defended_agg2[e$group_defended == 5] <- -1
      e$group_defended_agg2 <- as.item(e$group_defended_agg2, labels = structure(-2:2, names = c("Family and self", "Group of related people", "Fellow citizens", "Humans", "Sentient beings") #c("Egoistic", "Tribalist", "Nationalist", "Universalist")
      ), annotation = "group_defended_agg2: Group defended when one votes, where 'Group of related people' gathers My relatives and/or colleagues, My town, My State/region, People sharing my culture or religion, and Europeans.")
    }
    
    if ("donation_charities" %in% names(e)) {
      e$donation_charities_original <- e$donation_charities
      temp <- 50*grepl("100", e$donation_charities) + 300*grepl("101", e$donation_charities) + 750*grepl("501", e$donation_charities) + 3000*grepl("001", e$donation_charities) + 7000*grepl("More", e$donation_charities) + 0*grepl("did not", e$donation_charities)
      e$donation_charities <- as.item(temp, structure(c(0, 50, 300, 750, 3000, 7000), names = agg_thresholds(c(1), thresholds = c(0, 0, 100, 500, 1000, 5000, Inf), return = "levels", shift = 1)), annotation = Label(e$donation_charities))
    }
    
    if ("vote_participation" %in% names(e)) {
      e$vote_participation[grepl("right to vote", e$vote_participation)] <- "No right to vote"

      major_threshold <- 5 # 5% is the treshold to be considered a major candidate
      e$vote <- -0.1 # "PNR/Non-voter"
      for (c in tolower(countries)) {
        if (paste0("vote_", c, "_voters") %in% names(e)) {
          e$vote_all[!is.na(e[[paste0("vote_", c, "_voters")]]) & e$vote_participation=="Yes"] <- e[[paste0("vote_", c, "_voters")]][!is.na(e[[paste0("vote_", c, "_voters")]]) & e$vote_participation=="Yes"]
          e$vote_all[!is.na(e[[paste0("vote_", c, "_non_voters")]]) & e$vote_participation!="Yes"] <- e[[paste0("vote_", c, "_non_voters")]][!is.na(e[[paste0("vote_", c, "_non_voters")]]) & e$vote_participation!="Yes"]
          major_candidates[[c]] <<- setdiff(names(table(e$vote_all[e$country == toupper(c)]))[table(e$vote_all[e$country == toupper(c)]) > major_threshold * sum(e$country == toupper(c)) / 100], text_pnr)
          minor_candidates[[c]] <<- setdiff(names(table(e$vote_all[e$country == toupper(c)]))[table(e$vote_all[e$country == toupper(c)]) <= .05 * sum(e$country == toupper(c))], text_pnr)
          e$vote_agg[e$country == toupper(c) & e$vote_all %in% c(major_candidates[[c]], text_pnr)] <- e$vote_all[e$country == toupper(c) & e$vote_all %in% c(major_candidates[[c]], text_pnr)]
          e$vote_agg[e$country == toupper(c) & e$vote_all %in% minor_candidates[[c]]] <- "Other"
          e$vote[e[[paste0("vote_", c, "_voters")]] %in% c("Biden", "Hawkins", "Jean-Luc Mélenchon", "Yannick Jadot", "Fabien Roussel", "Anne Hidalgo", "Philippe Poutou", "Nathalie Arthaud", 
                                                           "PSOE", "Unidas Podemos", "Esquerra Republicana", "Más País", "JxCat–Junts", "Euskal Herria Bildu (EHB)", "Candidatura d'Unitat Popular-Per la Ruptura (CUP–PR)", "Partido Animalista (PACMA)", 
                                                           "SPD", "Grüne", "Die Linke", "Tierschutzpartei", "dieBasis", "Die PARTEI", "Labour", "SNP", "Green", "Sinn Féin")] <- -1 # "Left"
          e$vote[e[[paste0("vote_", c, "_voters")]] %in% c("Trump", "Jorgensen", "Emmanuel Macron", "Valérie Pécresse", "Jean Lassalle", "CDU/CSU", "Freie Wähler", "FDP", 
                                                           "PP", "Ciudadanos", "Partido Nacionalista Vasco (EAJ-PNV)", "Conservative", "Liberal Democrats", "DUP")] <- 0 #"Center-right or Right"
          e$vote[e[[paste0("vote_", c, "_voters")]] %in% c("Marine Le Pen", "Éric Zemmour", "Nicolas Dupont-Aignan", "AfD", "Vox", "Brexit Party")] <- 1 #"Far right"
          e$vote <- as.item(e$vote, labels = structure(c(-1:1, -0.1), names = c("Left", "Center-right or Right", "Far right", "PNR/Non-voter")), missing.values = c(-0.1, NA), annotation = "vote: Left / Center-right or Right / Far right / PNR/Non-voter Classification of vote_[country]_voters into three blocs.")
          e$vote_factor <- as.factor(as.character(e$vote))
          e$vote_factor <- relevel(e$vote_factor, "Left")
          # e$vote_factor <- relevel(e$vote_factor, "PNR/Non-voter")
          label(e$vote_factor) <- Label(e$vote)
        }
      }
      e$vote_participation <- as.item(as.character(e$vote_participation), missing.values = 'PNR', annotation=Label(e$vote_participation))
      label(e$vote_all) <- "vote_all: What the respondent has voted or would have voted in the last election, combining vote_[country]_voters and vote_[country]_non_voters."
      label(e$vote_agg) <- paste0("vote_agg: What the respondent has voted or would have voted in the last election, lumping minor candidates (with less than ", major_threshold, "% of $vote) into 'Other'. Build from $vote that combines $vote_[country]_voters and $vote_[country]_non_voters.")
      # e$vote_all <- as.item(as.character(e$vote_all), missing.values = 'PNR', annotation="vote_all: What the respondent has voted or would have voted in the last election, combining vote_[country]_voters and vote_[country]_non_voters.")
      # e$vote_agg <- as.item(as.character(e$vote_agg), missing.values = 'PNR', annotation=paste0("vote_agg: What the respondent has voted or would have voted in the last election, lumping minor candidates (with less than ", major_threshold, "% of $vote) into 'Other'. Build from $vote that combines $vote_[country]_voters and $vote_[country]_non_voters."))
      e$voted <- e$vote_participation == 'Yes'
      label(e$voted) <- "voted: Has voted in last election: Yes to vote_participation."
      major_candidates <<- major_candidates
      minor_candidates <<- minor_candidates
      
      temp <- as.character(e$vote)
      temp[grepl("ight", temp)] <- "Right"
      e$continent_vote <- paste(e$continent, temp)
    }

    if ("vote_us_voters" %in% names(e)) {
      e$vote_us <- "Other/Non-voter" # What respondent voted in 2020.
      e$vote_us[e$vote_participation %in% c("No right to vote", "Prefer not to say") | e$vote_us_voters %in% c("PNR", "Prefer not to say")] <- "PNR/no right"
      e$vote_us[e$vote_us_voters == "Biden"] <- "Biden"
      e$vote_us[e$vote_us_voters == "Trump"] <- "Trump"
      e$vote_us <- as.item(e$vote_us, annotation = "vote_us: Biden / Trump / Other/Non-voter / PNR/No right. True proportions: .342/.313/.333/.0")
      missing.values(e$vote_us) <- "PNR/no right"
      e$vote3 <- as.character(e$vote_us)
      e$vote3[e$vote3 %in% c("PNR/no right", "Other/Non-voter")] <- "Abstention/PNR/Other"
      label(e$vote3) <- "vote3: Abstention/PNR/Other / Biden / Trump Vote at 2020 presidential election"
      e$vote3_factor <- relevel(as.factor(e$vote3), "Biden")
      e$vote_Biden <- e$vote3 == 'Biden'
      e$vote_not_Biden <- e$vote3 != 'Biden'
      
      e$swing_state_3pp <- n(e$zipcode/1000) %between% c(48, 50) | n(e$zipcode/1000) %between% c(88.9, 90.0) | n(e$zipcode/1000) %between% c(15.0, 19.7) | n(e$zipcode/1000) %between% c(53, 55) | n(e$zipcode/1000) %between% c(85, 87) | n(e$zipcode/1000) %between% c(30, 32) | n(e$zipcode/1000) %between% c(39.8, 40.0) | n(e$zipcode/1000) %between% c(27, 29) 
      e$swing_state_5pp <- e$swing_state_3pp | n(e$zipcode/1000) %between% c(32, 35)
      label(e$swing_state_3pp) <- "swing_state: T/F Lives in one of the 7 States with less than 3 p.p. margin from the tipping point at the 2020 Presidential election (MI, NV, PA, WI, AZ, GA, NC)."
      label(e$swing_state_5pp) <- "swing_state_5pp: T/F Lives in one of the 8 States with less than 5 p.p. margin from the tipping point at the 2020 Presidential election (MI, NV, PA, WI, AZ, GA, NC, FL)."
      e$swing_state <- e$swing_state_5pp
        # 7 States with less than 3pp margin from tipping-point (or from 50%, both yield the same) in 2020:
        # Michigan MI (zipcodes: 48-49, 2.8%D margin, 15 electoral votes), Nevada NV (889-899, 2.4%D, 6), Pennsylvania PA (150-196, 1.2%D, 19), Wisconsin WI (53-54, .6%D, 10, tipping-point state), Arizona AZ (85-86, .3%D, 11), Georgia GA (30-31/398-399, .2%D, 16), North Carolina NC (27-28, 1.4%R, 16). [If instead we use 5pp, it adds Florida FL (32-34, 3.4%R, 30)] Apart from these, Dems should secure 228 electoral votes, i.e. need 42 more to win (out of 538 electoral votes or 83 swing ones).
        # sources: https://en.wikipedia.org/wiki/Swing_state#Swing_states_by_results, https://www.270towin.com/, consistent with analyses from https://www.businessinsider.com/battleground-states-2024-presidential-election-road-white-house-2022-12?r=US&IR=T, https://edition.cnn.com/2022/11/22/politics/2022-preview-2024-presidential-election/index.html
    }
    
    e$survey_biased[e$survey_biased %in% c("Yes, left-wing biased")] <- "Yes, left"
    e$survey_biased[e$survey_biased %in% c("Yes, right-wing biased")] <- "Yes, right"
    e$survey_biased[e$survey_biased %in% c("No, I do not feel it was biased")] <- "No"
    if ("Yes, right" %in% levels(as.factor(e$survey_biased))) e$survey_biased <- relevel(relevel(as.factor(e$survey_biased), "Yes, right"), "No")
    e$survey_biased_yes <- e$survey_biased != 'No'
    e$survey_biased_left <- e$survey_biased == "Yes, left"
    e$survey_biased_right <- e$survey_biased == "Yes, right"
    label(e$survey_biased_yes) <- "survey_biased_yes: T/F Finds the survey biased (survey_biased != No)"
    label(e$survey_biased_left) <- "survey_biased_left: T/F Finds the survey left-wing biased (survey_biased == Yes, left)"
    label(e$survey_biased_right) <- "survey_biased_right: T/F Finds the survey right-wing biased (survey_biased == Yes, right)"
    
    if ("feedback" %in% names(e)) e$branch_poverty_field <- e$feedback == 0
    # 1321 gcs_field in EU (branch_gcs_perception == 'field'), 477 in US2 (branch_gcs == 'field'), 662 poverty_field in US2 (branch_poverty_field == T)
    # 737 non-NA comment fields in eu, 700 in US2 (=> 1/2), 1453 in US1 (=> 1/3). ~4k fields will be read and ~1350 comment_field not read
    
    # To recode gcs_field/comment_field/poverty_field (pre-treatment necessary so the following code works): ~2h/country
    # 1. use lines below export CSV. 
    # 2. Create country.xlsm: if language has special characters, from 'template - no wrap'; if not, from 'template' and jump to step 5 (a posteriori, don't understand this last instruction)
    # 3. Data>Import from text>file_path>UTF8 + Delimited>Semicolon. For language without latin characters, run Sys.setlocale("LC_CTYPE","chinese") (didn't succeed with hindi and beware, it works just one at a time). Then it doesn't always works: closing and reopening Excel sometimes works, as well as replacing remaining latin characters (e.g. "NA"), after many attempts of basically the same thing it ended up working (mystery).
    # 4. If needed, translate to English: rename .xlsm into .xslx, translate on https://www.onlinedoctranslator.com/de/translationform, rename back to .xlsm
    # 5. Widen first row until below lifestyle. 6. Home>Wrap text on first row + Format>Column width>60 7. Click on appropriate cells. 
    # automatic translation: https://www.onlinedoctranslator.com/de/translationform
    # for (c in c(countries_EU, "US2")) for (v in intersect(names(d(c)), c("gcs_field", "comment_field", "poverty_field"))) write.table(paste(c('"', paste(gsub("\n", "\\\\\\n ", gsub("\r", " ", gsub('\"', "\\\\\\'", d(c)[[v]]))), collapse = '";"'), '"'), collapse=""),
    #                            paste0("../data/fields/csv/", v, "_", c, ".csv"), row.names = F, quote = F, col.names = F, fileEncoding = "UTF-8")
    # for (i in 1:2) write.table(paste(c('"', paste(gsub("\n", "\\\\\\n ", gsub("\r", " ", gsub('\"', "\\\\\\'", us2$comment_field[seq(i,nrow(us2),2)]))), collapse = '";"'), '"'), collapse=""),
    #                            paste0("../data/fields/csv/comment_field_US2_", i, ".csv"), row.names = F, quote = F, col.names = F, fileEncoding = "UTF-8")
    # for (i in 1:3) write.table(paste(c('"', paste(gsub("\n", "\\\\\\n ", gsub("\r", " ", gsub('\"', "\\\\\\'", us1$comment_field[seq(i,nrow(us1),3)]))), collapse = '";"'), '"'), collapse=""),
    #                            paste0("../data/fields/csv/comment_field_US1_", i, ".csv"), row.names = F, quote = F, col.names = F, fileEncoding = "UTF-8")
    # To make pre-treatment, open VBA (Alt+F11) then for each sheet needed, save:
    # Private Sub Worksheet_SelectionChange(ByVal Target As Excel.Range)
    #   Application.EnableEvents = False
    #   If Target.Cells.Count = 1 Then
    #     If Not Intersect(Target, Range("B2:ZZ50")) Is Nothing Then
    #       Select Case Target.Value
    #       Case ""
    #         Target.Value = "1"
    #       Case "1"
    #         Target.Value = ""
    #       End Select
    #       Cells(1, ActiveCell.Column).Select
    #     End If
    #   End If
    #   Application.EnableEvents = True
    # End Sub
    
    if ("gcs_field" %in% names(e)) {
      gcs_field_names <<- c("pro", "con", "support", "oppose", "dont_know", "unclassifiable", "misunderstands_gcs", "environment", "tax_redistribution", "misunderstands_question", "difficult_agreement", "difficult_implement", "cost",  "poorest_humans") # "climate_denier", "copy_paste", 
      # gcs_field_names_names <<- names(gcs_field_names)
      # names(gcs_field_names_names) <<- gcs_field_names
      variables_gcs_field_names <<- paste0("gcs_field_", gcs_field_names)
      e$gcs_field_english <- e$gcs_field
      recode_gcs_field <- list()
      split <- if (country %in% c("US1", "US2")) country else countries_EU
      for (i in split) {
        if (file.exists(paste0("../data/fields/", i, ".xlsm"))) recode_gcs_field[[i]] <- read.xlsx(paste0("../data/fields/", i, ".xlsm"), sheet = "GCS", rowNames = T, sep.names = " ", na.strings = c(), skipEmptyCols = F)
        else if (file.exists(paste0("../data/fields/", i, "en.xlsm"))) recode_gcs_field[[i]] <- read.xlsx(paste0("../data/fields/", i, "en.xlsm"), sheet = "GCS", rowNames = T, sep.names = " ", na.strings = c(), skipEmptyCols = F)
        else print("No file found for recoding of gcs_field.")
        indices_i <- if (country %in% c("US1", "US2")) 1:nrow(e) else which(e$country == i)
        if (file.exists(paste0("../data/fields/", i, "en.xlsm"))) e$gcs_field_english[indices_i] <- as.character(read.xlsx(paste0("../data/fields/", i, "en.xlsm"), sheet = "GCS", rowNames = T, colNames = F, sep.names = " ", na.strings = c(), skipEmptyCols = F)[1,])
        # row.names(recode_gcs_field[[i]]) <- gcs_field_names[row.names(recode_gcs_field[[i]])]
        recode_gcs_field[[i]] <- as.data.frame(t(recode_gcs_field[[i]]), row.names = indices_i)
        if (i == split[1]) for (v in names(recode_gcs_field[[i]])) e[[paste0("gcs_field_", v)]] <- NA # /!\ There may be a bug if there are NA in gcs_field_names[names(recode_gcs_field[[i]])], which happens when the variable/column names are unknown in gcs_field_names
        for (v in names(recode_gcs_field[[i]])) e[[paste0("gcs_field_", v)]][indices_i] <- recode_gcs_field[[i]][[v]]==1
        # e[[paste0("gcs_field_empty")]][indices_i][recode_gcs_field[[i]][["empty"]]==2] <- 2
      }
      for (v in names(recode_gcs_field[[i]])) e[[paste0("gcs_field_", v)]][e$branch_gcs_field == T & is.na(e[[paste0("gcs_field_", v)]])] <- FALSE
      
      label(e$gcs_field_english) <- "gcs_field_english: gcs_field translated to English."
      e$length_gcs_field_english <- nchar(e$gcs_field_english)
      e$length_gcs_field_english[is.na(e$length_gcs_field_english) & e$branch_gcs_field == T] <- 0
      label(e$length_gcs_field_english) <- "length_gcs_field_english: Number of characters in gcs_field_english"
      e$gcs_field_empty <- e$length_gcs_field_english == 0
      e$gcs_field_empty[e$branch_gcs_field == FALSE] <- NA
      
      variables_gcs_field_contains <<- paste0("gcs_field_contains_", c("pro", "con", "world", "environment", "cost", "poorest", "tax", "redistribution", "implementation", "agreement", "justice", "awareness", "socialism", "bureaucracy", "migration", "children"))
      grep_variables_gcs_field_contains <<- c("pro |pro:|pros |pros:", "con |con: |cons |cons: ", "international|world|countr|global", "climat|environment|animal|emission|natur", "cost|expensive|higher price|85|inflation", "poor|low-income|700|poverty", "tax", "rich|redistribu", 
                                              "implement|enforce|polic|monitor", "agree|accept|participat", "justice|equal|fair|low-income", "aware|educat", "socialis|communis", "bureaucra|administr", "migra", "child")
      names(grep_variables_gcs_field_contains) <<- variables_gcs_field_contains
      for (v in variables_gcs_field_contains) {
        e[[v]] <- grepl(grep_variables_gcs_field_contains[v], e$gcs_field_english, ignore.case = T)
        e[[v]][e$branch_gcs_field == FALSE] <- NA
        label(e[[v]]) <- paste0(v, ": T/F gcs_field_english contains: ", grep_variables_gcs_field_contains[v])  }
      
      # Impressions:
      # Most people mention environmental benefits from the GCS or environmental impacts of CC; the majority seems to understand the trade-off between climate protection and costs, though many answers are very short or out-of-topic; effects on the poorest humans are less mentioned
      # US: 
      # DE: 
      # FR: 
      # UK: 
      
      # Recurrent topics that don't have a variable:
      # US: people confusing GCS and GCS+NR, people thinking poor Americans would win from GCS (forgetting it's global or thinking they are in the bottom 700M), plan will help raising awareness, selfish reasoning, accusation of socialism/communism, 
      # DE: pessimism (e.g. politicians all corrupt, or GCS would be good but won't happen), that global (not national) action is needed, that (any) climate action is needed, will increase bureaucracy
      # FR: middle class always pays (squeezed middle), we'll need to consume less, avoids migration, future for our children, 
      # ES: better world, better for biodiversity or nature
      # UK: 
      # TODO! (add cost, poorest humans, change doctrine): US2 A-ZZ, UK A-GH
      
      # Pépites:
      # US: HI, GI, JX, OL, 3E, 3EG, 3GM, 3GU (!), 3IT, 3JR, 2ED, 2NJ
      # I'm probably one of the 700 million in poverty and although the numbers on paper look good , no body wins in the end. The rich pay more and for what I'm still gonna be poor . \n  \n The only good I can think of is the worls itself. The life span of our world would be a little greater without all his gas and pollution.
      # It sounds like this would be a win for typical Americans.
      # I think it sounds like a good idea to limit climate change, I like the idea that it would benefit the poorest on this planet but it would come at a cost for Americans. They would lose out financially or wouldn't gain anything.  \n  \n I do think that the national redistribution scheme sounds great, but the Global Climate scheme would make it unbalanced.  It would need to be tweaked to the point where both the poorest and Americans equally gain from these schemes.
      # I think its a good idea to combat climate. However, as an american myself, i do not like this idea.
      # the scheme is good but the people who would run it would not be reliable
      # pros are the poorest will benefit and the cons are the rich will be taxed more to pay for it
      # I feel like the typical american would not benefit so I don't see any pros
      # I think anything is better then what we're doing now and if the calculations have been done by the properly suited people then it is what we must to to improve the earth for future generations and more importantly the animals.  \n  \n Pros: World doesn't die, people don't die, animals don't die  \n  \n Cons: I might not be able to drive my V8 anymore \n  \n Well .. actually now that I think about that, no I don't approve it. People should be allowed to drive what they want. You can't race or have good memories in an electric car. There's no sentimental value to a electric car, they're not customizable and they're tiny and powerless. Plus there would not be energy stations in the country which means country and rural people couldn't drive.
      # I don't think there are any pros. You should not force me to suffer in order to help those who cannot help themselves.
      # I definitely do not support this. It's nice that it helps those in extreme poverty but why should Americans have to suffer. I obviously support working towards reversing climate change but there is no excuse why anyone should have to pay for it. Especially not Americans who are already dealing with horrible inflation. If there was a way to change this where Americans would benefit as well then I'd definitely support it.
      # I think it’s a good plan, but has no chance of working because no American politician would go for it.
      # Not sure of all the pros and cons of global climate change - but it is real and needs to be dealt with      # 

      # DE: FM, HM, LP, MZ, QH, AKN
      # Habe nicht soviel Ahnung davon aber die leite die sich mit Politik beschäftigen und beim Bundesrat reden haben auch keine Ahnung
      # es ist längst überfällig dies zu unternehmen
      # Ist mir alles zu fremd. Ich kann die Vor- oder Nachteile nicht verstehen. Ich bin für einfacherer Sachen und nicht so was \'weltfremdes\'. Hier werden Arme gegen Reiche ausgespielt. Ich finde, das alles gleich behandelt werden sollen.
      # Versucht das Klima zu retten, aber dies interessiert mich nicht wirklich, sorry!
      # Gewinner sind die Menschen (vor allem zukünftige Generationen) da das Klima erhalten bleibt, ebenso alldiejenige auf Nachhaltigkeit setzen. Verlierer sind alldiejenige, die auf fossile Brennstoffe oder hohen CO2-Verbrauch setzen.
      # Wenn wirklich alle Nationen mitmachen würden   \n Dann wäre es für das Klimaprogramm vorteilhaft. Leider ist das nicht so. Jede Nation sucht ihren Vorteil.
      # Translated:
      # Don't have so much idea about it but the leaders who deal with politics and talk at the Bundesrat don't have a clue either
      # it is long overdue to undertake this
      # It's all too foreign to me. I can't understand the pros or cons. I am for simpler things and not so what \'weltfremdes\'. Here the poor are pitted against the rich. I think that everything should be treated equally.
      # Try to save the climate, but this does not really interest me, sorry!
      # Winners are the people (especially future generations) because the climate is preserved, as well as all those who rely on sustainability. Losers are all those who rely on fossil fuels or high CO2 consumption.
      # If all nations would really join in \n then it would be beneficial for the climate program. Unfortunately, it is not. Every nation is looking for its advantage.
      
      # FR: KP, MN, OI, OM, OS, OU, YA,
      # Pas d'autres alternatives.
      # C'est important que tout le monde agisse contre le réchauffement climatique
      # Sauver la planete ça urge
      # plus de justice sociale mais difficile à mettre en oeuvre
      # Il aurait supposément l'avantage de limiter l'émission des gaz à effet de serre et l'utilisation des énergies fossiles et d'aider à sortir de l'extrême pauvreté un grand nombre de personnes. Ses inconvénients seraient son caractère contraignant, sanctions économiques pour les pays réfractaires notamment, et la perte financière qu'il représenterait pour des Français comme moi dont le revenu est somme toute très moyen !
      # Arnaque pour détourner des fond
      # Il faut convaincre les plus grands pays et les plus grandes industries
      # Translated:
      # No other alternatives.
      # It's important that everyone takes action against global warming
      # Saving the planet is urgent
      # More social justice but difficult to implement
      # It would supposedly have the advantage of limiting greenhouse gas emissions and the use of fossil fuels and of helping to lift a large number of people out of extreme poverty. Its disadvantages would be its constraining character, economic sanctions for the refractory countries in particular, and the financial loss that it would represent for French people like me whose income is quite average!
      # Scam to divert funds
      # It is necessary to convince the biggest countries and the biggest industries
      
      # ES: JD, KA, KD, KU, LO, MW
      # Pro: intento de que el entorno global no se vaya degradando o por lo menos no tan rapido como lo hace actualmente.\n Contra: muchas personas con ingresos muy altos se opondrian
      # Que tenemos que colaborar todos los países del mundo  , si no es asi nuestro futuro va a ser complicado
      # Los fondos se distorsionan en tareas no prioritarias.\n La lucha contra el cambio climático no la pueden pagar los ciudadanos
      # me parece bien tal como esta espresado en el enunciado
      # No me gusta la GLOBALIZACION. Siempre ganan los ricos y pierdes los pobres.
      # Tenemos que luchar contra el cambio climático sin ni siquiera pensar en los contras que pueda haber.
      # Translated:
      # Pro: attempt to keep the global environment from degrading or at least not as fast as it is currently degrading.
      # That we have to collaborate with all the countries of the world, otherwise our future will be complicated.
      # Funds are distorted in non-priority tasks. The fight against climate change cannot be paid by the citizens.
      # I'm fine with it as it is expressed in the statement.
      # I don't like GLOBALIZATION. It's always a win-win for the rich and a lose-lose for the poor.
      # We have to fight climate change without even thinking about the cons.
      
      # UK: M, GX, KC, KD, LE, LL, OI, PW, ZR  
      # The normal person will see no benefit from the scheme
      # I think we could do alot more than we are doing but if the education structure is not there how is it possible, also alot of people don't really care about our climate change problems or care to think about it and just live there lives everyday.
      # Pro: this is a matter of extreme urgency; action needed now!\n Con: idiot climate change deniers will make a noise!
      # In an ideal world it sounds fine and would be a massive boost for some of the poorest people in the world. \n In reality however it's a pipe dream and stands zero chance of even coming close to achieving. \n There is always going to be a large part of the world cut off from the rest and in abject poverty.
      # It is of benefit to many and might well encourage people to switch from fossil fuels.\n However it is a great excuse for the fossil fuel producers to hike prices
      # We need to do something and we have to suffer short term pain
      # Pros be good for the world, cons cost
      # Assisting 700 million of the world's poorest must be a aim worth striving for. Those who earn the most should pay the most tax. Majority of British people would not lose out financially so all good
      # Will never get implemented as too many rich and or powerful people will stop it for their own interests
    }
    
    if (country == "US2") { # poverty_field
      poverty_field_names <<- c("give_money", "education", "food", "jobs", "unspecified_aid", "against_help", "tax", "other", "dont_know")
      # poverty_field_names_names <<- names(poverty_field_names)
      # names(poverty_field_names_names) <<- poverty_field_names
      variables_poverty_field_names <<- paste0("poverty_field_", poverty_field_names)
      if (file.exists(paste0("../data/fields/US2.xlsm"))) recode_poverty_field <- read.xlsx(paste0("../data/fields/US2.xlsm"), sheet = "poverty", rowNames = T, sep.names = " ", na.strings = c(), skipEmptyCols = F)
      else print("No file found for recoding of poverty_field.")
      # row.names(recode_poverty_field) <- poverty_field_names[row.names(recode_poverty_field)]
      recode_poverty_field <- as.data.frame(t(recode_poverty_field), row.names = 1:nrow(e))
      # for (v in names(recode_poverty_field)) e[[paste0("poverty_field_", v)]] <- NA # /!\ There may be a bug if there are NA in poverty_field_names[names(recode_poverty_field)], which happens when the variable/column names are unknown in poverty_field_names
      for (v in names(recode_poverty_field)) e[[paste0("poverty_field_", v)]] <- recode_poverty_field[[v]]==1
      for (v in names(recode_poverty_field)) e[[paste0("poverty_field_", v)]][e$branch_poverty_field == T & is.na(e[[paste0("poverty_field_", v)]])] <- FALSE
      # e[[paste0("poverty_field_empty")]][indices_i][recode_poverty_field[["empty"]]==2] <- 2
      
      e$length_poverty_field <- nchar(e$poverty_field)
      e$length_poverty_field[is.na(e$length_poverty_field) & e$branch_poverty_field == T] <- 0
      label(e$length_poverty_field) <- "length_poverty_field_english: Number of characters in poverty_field_english"
      e$poverty_field_empty <- e$length_poverty_field == 0
      e$poverty_field_empty[e$branch_poverty_field == FALSE] <- NA
      
      variables_poverty_field_contains <<- paste0("poverty_field_contains_", c("justice", "education", "corruption", "money", "help", "job", "tax", "health", "food", "world", "wage"))
      grep_variables_poverty_field_contains <<- c("justice|poor|equalit|fair|low-income", "train|teach|educat", "ruler|leader|democracy|corrupt", "money|financ", "help|aid", "job", "tax", "health|medic", "food", "international|world|global", "wage")
      names(grep_variables_poverty_field_contains) <<- variables_poverty_field_contains
      for (v in variables_poverty_field_contains) {
        e[[v]] <- grepl(grep_variables_poverty_field_contains[v], e$poverty_field, ignore.case = T)
        e[[v]][e$branch_poverty_field == FALSE] <- NA
        label(e[[v]]) <- paste0(v, ": T/F poverty_field_english contains: ", grep_variables_poverty_field_contains[v])  }
      # Impressions:
      # Give more money is the most common answered, followed by "teach them how to fish", i.e. providing education, then comes providing in-king assistance (food, healthcare...), and finally: help poor Americans first
      
      # Recurrent topics that don't have a variable:
      # Increase wages in low-income countries (e.g. by generalizing fair trade); provide loans (e.g. for infrastructure); promote democracy / have them remove their authoritarian leaders
      
      # Pépites:
      # They should figure out what are the most important basic necessities that poor people need and go from there.
      # program/should help: Assist low-income countries as much as they can.
      # give money: Donate money to a fund run by the UN
      # Contribute more
      # Spend more in foreign aid
      # Give more aid but watch where it goes
      # program/should help: Help the poor
      # against helping them: Stop helping other countries
      # against helping them: Nothing
      # against helping them: help usa first
      # against helping them: first we should focus on ourselves. we have many people living in poverty in america
      # jobs: create jobs and opportunity in their country
      # education (& give money): I think teaching the country to work for themselves, teach them how to better their country. We would of course need to provide monetary care as well
      # other: Help pay for infrastructure, welcome refugees into the US, pay higher wages to factory workers worldwide
      # tax: Raise taxes on the rich.
      # I believe that there should be a higher tax on the wealthy individuals and this should go to support lower income countries
    }
    
    if ("comment_field" %in% names(e)) {
      comment_field_names <<- c("good", "bad", "bias", "problem")
      var_comment_field_names <<- paste0("comment_field_", comment_field_names)
      e$comment_field_english <- e$comment_field
      recode_comment_field <- list()
      if (country == "US1") split <- 1:3 
      else if (country == "US2") split <- 1:2
      else split <- countries_EU
      for (i in split) {
        filename <- if (country %in% c("US1", "US2")) country else i
        sheet_i <- if (country %in% c("US1", "US2")) i else ""
        if (file.exists(paste0("../data/fields/", filename, ".xlsm"))) recode_comment_field[[i]] <- read.xlsx(paste0("../data/fields/", filename, ".xlsm"), sheet = paste0("comment", sheet_i), rowNames = T, sep.names = " ", na.strings = c(), skipEmptyCols = F)
        else if (file.exists(paste0("../data/fields/", filename, "en.xlsm"))) recode_comment_field[[i]] <- read.xlsx(paste0("../data/fields/", filename, "en.xlsm"), sheet = paste0("comment", sheet_i), rowNames = T, sep.names = " ", na.strings = c(), skipEmptyCols = F)
        else print("No file found for recoding of comment_field.")
        if (country == "US1") indices_i <- i+3*((1:ncol(recode_comment_field[[i]])-1)) 
        else if (country == "US2") indices_i <- i+2*((1:ncol(recode_comment_field[[i]])-1)) 
        else indices_i <- which(e$country == i)
        if (file.exists(paste0("../data/fields/", filename, "en.xlsm"))) e$comment_field_english[indices_i] <- names(recode_comment_field[[i]])
        recode_comment_field[[i]] <- as.data.frame(t(recode_comment_field[[i]]), row.names = indices_i)
        if (i == split[1]) for (v in names(recode_comment_field[[i]])) e[[paste0("comment_field_", v)]] <- NA
        for (v in names(recode_comment_field[[i]])) e[[paste0("comment_field_", v)]][indices_i] <- recode_comment_field[[i]][[v]]==1
      }
      label(e$comment_field_english) <- "comment_field_english: comment_field translated to English."
      e$dislike_comment_field <- (e$comment_field_bad | e$comment_field_bias) %in% T
      label(e$dislike_comment_field) <- "dislike_comment_field: T/F The respondent didn't sur survey: comment_field either says the survey is bad or biased."
      e$critic_comment_field <- (e$comment_field_bad | e$comment_field_problem | e$comment_field_bias) %in% T
      label(e$critic_comment_field) <- "critic_comment_field: T/F The answer to comment_field is critical: either mentions an issue, says that the survey is bad or biased."
      e$treated_comment_field <- T
      if (country == "US1") non_treated <- c(2+3*((1:nrow(recode_comment_field[[i]])-1)), 3+3*((1:nrow(recode_comment_field[[i]])-1)))
      else if (country == "US2") non_treated <- c(2+2*((1:nrow(recode_comment_field[[i]])-1)))
      else non_treated <- c()
      e$treated_comment_field[non_treated] <- FALSE
      label(e$treated_comment_field) <- "treated_comment_field: T/F comment_field has been treated/recoded. N"
      for (v in names(recode_comment_field[[1]])) {
        e[[paste0("comment_field_", v)]][e$treated_comment_field & is.na(e[[paste0("comment_field_", v)]])] <- FALSE
        label(e[[paste0("comment_field_", v)]]) <- paste0("comment_field_", v, ": ", v, " - Feeling or opinion the respondent expressed about the survey in comment_field.") }
      variables_comment_field_contains <<- paste0("comment_field_contains_", c("long", "good", "thanks", "learned", "bias"))
      grep_variables_comment_field_contains <<- c(" long", "good|Good|excellent|enjoy|interesting", "thank|Thank", "learnt|learn", "bias")
      names(grep_variables_comment_field_contains) <<- variables_comment_field_contains
      for (v in variables_comment_field_contains) {
        e[[v]] <- grepl(grep_variables_comment_field_contains[v], e$comment_field_english)
        label(e[[v]]) <- paste0(v, ": T/F comment_field_english contains: ", grep_variables_comment_field_contains[v])  }
      e$non_empty_comment_field <- !is.na(e$comment_field)
      label(e$non_empty_comment_field) <- "non_empty_comment_field: The respondent left a feedback comment, i.e. comment_field is not NA."
    }

    if ("branch_petition" %in% names(e)) {
      e$petition_matches_support[e$branch_petition == "nr"] <- (e$petition == e$nr_support)[e$branch_petition == "nr"]
      e$petition_matches_support[e$branch_petition == "gcs"] <- (e$petition == e$gcs_support)[e$branch_petition == "gcs"]
      e$petition_yes_support_no[e$branch_petition == "nr"] <- (e$petition == "Yes" & e$nr_support == "No")[e$branch_petition == "nr"]
      e$petition_yes_support_no[e$branch_petition == "gcs"] <- (e$petition == "Yes" & e$gcs_support == "No")[e$branch_petition == "gcs"]
      e$petition_no_support_yes[e$branch_petition == "nr"] <- (e$petition == "No" & e$nr_support == "Yes")[e$branch_petition == "nr"]
      e$petition_no_support_yes[e$branch_petition == "gcs"] <- (e$petition == "No" & e$gcs_support == "Yes")[e$branch_petition == "gcs"]
      label(e$petition_matches_support) <- "petition_matches_support: T/F The answer to the petition coincides with the answer to the support (cf. branch_petition to see which measure: gcs/nr)." 
      label(e$petition_yes_support_no) <- "petition_yes_support_no: T/F Willing to sign the petition but does not support (cf. branch_petition to see which measure: gcs/nr)."
      label(e$petition_no_support_yes) <- "petition_no_support_yes: T/F Not willing to sign the petition but supports (cf. branch_petition to see which measure: gcs/nr)." 
    }
    
    if ("conjoint_a" %in% names(e)) {
      e$conjoint_a_matches_support <- e$conjoint_a == e$gcs_support
      e$conjoint_a_rcg_support_no <- e$conjoint_a == T & e$gcs_support == "No"
      e$conjoint_a_rc_support_yes <- e$conjoint_a == F & e$gcs_support == "Yes"
      label(e$conjoint_a_matches_support) <- "conjoint_a_matches_support: T/F The answer to conjoint analysis (a) (irg vs. ir) corresponds to the answer to the support of GCS."
      label(e$conjoint_a_rcg_support_no) <- "conjoint_a_rcg_support_no: T/F Prefers rcg to rc in conjoint analysis (a) but does not support GCS."
      label(e$conjoint_a_rc_support_yes) <- "conjoint_a_rc_support_yes: T/F Prefers rc to rcg in conjoint analysis (a) but supports GCS."
    }
    
    e$share_policies_supported <- rowMeans(e[, intersect(variables_support, names(e))] > 0, na.rm = T)
    label(e$share_policies_supported) <- "share_policies_supported: Share of all policies supported (strongly or somewhat) among all policies asked to the respondent."
    
    for (i in seq_along(variables_matrices)) if (length(intersect(variables_matrices[[i]], names(e))) > 0) {
      e[[paste0("spread_", names(variables_matrices)[i])]] <- apply(X = e[, intersect(variables_matrices[[i]], names(e))], MARGIN = 1, FUN = function(v) return(max(v, na.rm = T) - min(v, na.rm = T)))
      e[[paste0("all_same_", names(variables_matrices)[i])]] <- e[[paste0("spread_", names(variables_matrices)[i])]] == 0
      label(e[[paste0("spread_", names(variables_matrices)[i])]]) <- paste0("spread_", names(variables_matrices)[i], ": Spread between max and min value in the respondent's answers to the matrix ", names(variables_matrices)[i])
      label(e[[paste0("all_same_", names(variables_matrices)[i])]]) <- paste0("all_same_", names(variables_matrices)[i], ": T/F Indicator that all answers to the matrix ", names(variables_matrices)[i], " are identical.")
    }
    variables_spread <<- intersect(paste0("spread_", names(variables_matrices)), names(e))
    variables_all_same <<- intersect(paste0("all_same_", names(variables_matrices)), names(e))
    names_matrices <<- gsub("all_same_", "", variables_all_same)
    e$mean_spread <- rowMeans(e[, variables_spread], na.rm = T)
    e$share_all_same <- rowMeans(e[, variables_all_same], na.rm = T)
    label(e$mean_spread) <- paste0("mean_spread: Mean spread between max and min value in the respondent's answers to the matrices (averaged over the matrices: ", paste(names_matrices, collapse = ", "), "). -Inf indicates that all answers were NA.")
    label(e$share_all_same) <- paste0("mean_spread: Share of matrices to which all respondent's answers are identical (among matrices: ",  paste(names_matrices, collapse = ", "), ").")
  }
  
  for (v in intersect(variables_donation, names(e))) e[[paste0(v, "_agg")]] <- agg_thresholds(e[[v]], c(0, 0, 25, 50, 90, 100), shift = 1)
  for (v in intersect(variables_points, names(e))) e[[paste0(v, "_agg")]] <- agg_thresholds(e[[v]], c(0, 0, 8, 14, 18, 25, 50, 100), shift = 1)
  for (v in intersect(c("share_policies_supported"), names(e))) e[[paste0(v, "_agg")]] <- agg_thresholds(e[[v]], c(0, 0, 0.25, 0.5, 0.75, 1, 1), shift = 0.01)
  for (v in intersect(variables_foreign_aid_amount, names(e))) e[[paste0(v, "_agg")]] <- agg_thresholds(e[[v]], c(-Inf, 0.2, 0.5, 1, 1.7, 2.6, 6, Inf), shift = 0.1)
  for (v in intersect(variables_belief, names(e))) e[[paste0(v, "_agg")]] <- agg_thresholds(e[[v]], c(0, 20, 40, 60, 80, 100), shift = 1) # TODO add actual value
  
  if (country == "EU") label(e$foreign_aid_no_nation_first) <- gsub("American", "[country]", Label(e$foreign_aid_no_nation_first))
  
  e$wrong_language <- (e$country == "US" & e$language != "EN") | (e$country == "DE" & e$language != "DE") | (e$country == "FR" & e$language != "FR") | (e$country == "ES" & e$language != "ES-ES") | (e$country == "UK" & e$language != "EN-GB")
  label(e$wrong_language) <- "wrong_language: T/F The language does not correspond to the respondent's country (including Spanish in the U.S.)."
  }
  
  # count_IP <- rle(as.vector(sort(e$ip)))
  # e$number_same_ip <- count_IP$lengths[match(e$ip, count_IP$values)]
  # e$duplicate_ip <- e$number_same_ip > 1
  # label(e$number_same_ip) <- "number_same_ip: Number of respondents with the same IP."
  # label(e$duplicate_ip) <- "duplicate_ip: T/F The respondent's IP is used by other respondents."
  
  e$n <- paste0(country, ifelse(wave == "pilot", "p_", "_"), 1:nrow(e))
  
  print(paste("convert: success", country))
  return(e)
}


##### Run #####

e <- us1 <- prepare(country = "US1", weighting = T, define_var_lists = FALSE)
e <- eu <- prepare(country = "EU", weighting = T)
e <- us2 <- prepare(country = "US2", weighting = T, define_var_lists = FALSE)

us <- merge(us1, us2, all = T)
e <- all <- merge(us, eu, all = T)

# variables_include <- c("finished", "excluded", "duration", "attention_test", "progress", "dropout", "valid", "finished_attentive", "education_original", "gender", "age", "income", "owner", "female", "income_factor", "treatment", "urban_category", "region") 
us1a <- prepare(country = "US1", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
us2a <- prepare(country = "US2", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
eua <- prepare(country = "EU", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]

e <- mep <- prepare(country = "MEP", only_finished = FALSE, exclude_speeder = FALSE, incl_quality_fail = T, weighting = FALSE, define_var_lists = FALSE)

us1p <- prepare(country = "US1", wave = "pilot", weighting = FALSE)
us2p <- prepare(country = "US2", wave = "pilot", weighting = FALSE)
eup <- prepare(country = "EU", wave = "pilot", weighting = FALSE)

usp <- merge(us1p, us2p, all = T)
e <- ep <- merge(usp, eup, all = T)

eupa <- prepare(country = "EU", wave = "pilot", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
us1pa <- prepare(country = "US1", wave = "pilot", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
us2pa <- prepare(country = "US2", wave = "pilot", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]

export_codebook(us1p, "../data/codebook_us1p.csv", stata = FALSE, omit = which(names(us1p) %in% c("list_exp_gl", "donation_nation", "donation_africa")))
export_codebook(us2p, "../data/codebook_us2p.csv", stata = FALSE)
export_codebook(eup, "../data/codebook_eup.csv", stata = FALSE)
export_codebook(eup, "../data/codebook_ep.csv", stata = FALSE)

variables_list_exp <- c("list_exp_l", "list_exp_gl", "list_exp_rl", "list_exp_rgl")
quotas_us <- c("income_factor", "post_secondary", "age_factor", "man", "urban", "race", "region") # race_white instead of race? not for representativeness
socio_demos_us <- c(quotas_us, "swing_state", "couple", "employment_agg", "vote3") # , "wealth_factor"
quotas_eu <- c("country_name", "income_factor", "post_secondary", "age_factor", "man", "urbanity") # diploma instead of post_secondary? as.factor(urbanity) instead of urban?
socio_demos <- c(quotas_eu, "couple", "employment_agg", "vote_factor") # add "hh_size", "owner", "wealth_factor", "donation_charities"?
politics <- c("political_affiliation", "interested_politics", "involvement_govt", "left_right", "vote_participation", "vote_us", "group_defended")
covariates <- c("country_name", "income_factor", "post_secondary", "age_factor", "man", "couple", "employment_agg", "vote_factor", "urban", "race_white", "region", "swing_state")

##### Create Raw results appendices #####
# # (don't forget instructions in comments)
# countries_eu <- c(countries_EU, "Eu" = "EU")
# names_countries_eu <- setNames(names(countries_eu), countries_eu)
# country_eu <- setNames(c("French", "German", "Spanish", "British", "[own country]"), countries_eu)
# c <- "EU"
# temp <- readLines("../paper/app_desc_stats_US.tex")
# temp <- gsub("the U.S.", names_countries_eu[c], temp, fixed = T)
# temp <- gsub("U.S.", country_eu[c], temp, fixed = T)
# temp <- gsub("s US1 and US2", " Eu", temp, fixed = T)
# temp <- gsub(" \\(US1\\)| \\(US2\\)", "", temp)
# temp <- gsub("US|US1|US2", c, temp)
# temp <- gsub("[Asked only to non-Republicans] ", "", temp, fixed = T)
# temp <- gsub("Democratic", "political", temp, fixed = T)
# writeLines(temp, con = paste0("../paper/app_desc_stats_", c, ".tex")) 
# # check TODO: Eu and US2 (manually remove duplicates at the end for vote, etc.)
# for (c in countries_EU) {
#   temp <- readLines("../paper/app_desc_stats_EU.tex")
#   temp <- gsub("Eu", names_countries_eu[c], temp, fixed = T)
#   temp <- gsub("[own country]", country_eu[c], temp, fixed = T)
#   temp <- gsub("EU", c, temp)
#   temp <- gsub(paste("Complementary survey", c), "Complementary survey Eu", temp, fixed = T)
#   writeLines(temp, con = paste0("../paper/app_desc_stats_", c, ".tex")) 
# }
# # comment vote_agg in EU


##### Random draws #####
# set.seed(42)
# winners <- data.frame(ID = character(), gain = numeric(), country = character(), wave = character(), question = character())
# # understanding
# for (v in variables_understood[1:3]) {
#   understanding_winners <- sample(all$ID[all[[v]] == T], 3)
#   winners <- rbind(winners, data.frame(ID = understanding_winners, gain = 50, country = all$country[match(understanding_winners, all$ID)], wave = all$wave[match(understanding_winners, all$ID)], question = v))
# }
# # second-order beliefs
# for (v in variables_belief) {
#   mean_belief <- c()
#   for (c in countries_EU) mean_belief[c] <- wtd.mean(eu[[v]][eu$country == c], eu$weight_country[eu$country == c])
#   mean_belief["US"] <- wtd.mean(us1[[v]], us1$weight)
#   all[[paste0(v, "_distance_truth")]] <- abs(mean_belief[all$country] - all[[v]])
#   closest <- all[[paste0(v, "_distance_truth")]] == min(all[[paste0(v, "_distance_truth")]], na.rm = T)
#   if (sum(closest, na.rm = T) > 2) belief_winners <- sample(all$ID[which(closest)], 3)
#   else print(paste("Less than 3 closest for", v))
#   print(paste("Number of closest for", v, ": ", sum(closest, na.rm = T)))
#   winners <- rbind(winners, data.frame(ID = belief_winners, gain = 50, country = all$country[match(belief_winners, all$ID)], wave = all$wave[match(belief_winners, all$ID)], question = v))
# }
# # donation lottery
# donation_winner <- sample(all$ID, 1)
# winners <- rbind(winners, data.frame(ID = donation_winner, gain = 100 - all$donation[all$ID == donation_winner], country = all$country[all$ID == donation_winner], wave = all$wave[all$ID == donation_winner], question = paste0("donation_", all$branch_donation[all$ID == donation_winner])))
# write.xlsx(winners, "../data/winners.xlsx")
# winners




















