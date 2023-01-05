# Consistency: increase/reduce aid for those with the info + later question; petition_yes_support_no; duplicate_ip

source(".Rprofile")
source("relabel_rename.R")
# source("conjoint_analysis.R")

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

policies_names <- as.matrix(read.xlsx("../questionnaire/specificities.xlsx", sheet = "Policies", rowNames = T, rows = c(1, 16:41), cols = 1:5))
policies_names <- policies_names[is.na(as.numeric(row.names(policies_names))),] # NAs by coercion normal
countries_names <- c("France", "Germany", "Spain", "United Kingdom", "United States")
names(countries_names) <- countries <- c("FR", "DE", "ES", "UK", "US")
names(countries) <- countries_names
major_candidates <- minor_candidates <- list()


##### Quotas #####
{
  levels_quotas <- list(
  "gender" = c("Woman", "Other", "Man"), # we could add: urbanity, education, wealth, occupation, employment_agg, marital_status, Nb_children, HH_size, home (ownership)
  "income" = c("Q1", "Q2", "Q3", "Q4"),
  "age" = c("18-24", "25-34", "35-49", "50-64", "65+"),
  "urbanity" = c("Cities", "Towns and suburbs", "Rural"),
  "diploma" = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64"), # "Not 25-64"
  "EU_country" = c("FR", "DE", "ES", "UK"),
  "US_region" = c("Northeast", "Midwest", "West","South"),
  "US_race" = c("White only", "Hispanic", "Black", "Other"),
  "US_vote_2020" = c("Biden", "Trump", "Other/Non-voter"), #, "PNR/no right"),
  "EU_urbanity" = c("Cities", "Towns and suburbs", "Rural"),
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
  
  quotas <- list("EU" = c("gender", "income", "age", "urbanity", "diploma", "country"),
                 "US" = c("gender", "income", "age", "urbanity", "diploma", "region", "race"), 
                 "US_vote" = c("gender", "income", "age", "urbanity", "diploma", "region", "race", "vote_2020"),
                 "FR" = c("gender", "income", "age", "urbanity", "diploma"), #, "urban_category") From oecd_climate: Pb sur cette variable car il y a des codes postaux à cheval sur plusieurs types d'aires urbaines. Ça doit fausser le type d'aire urbaine sur un peu moins de 10% des répondants. Plus souvent que l'inverse, ça les alloue au rural alors qu'ils sont urbains.
                 # Au final ça rajoute plus du bruit qu'autre chose, et ça gène pas tant que ça la représentativité de l'échantillon (surtout par rapport à d'autres variables type age ou diplôme). Mais ça justifie de pas repondérer par rapport à cette variable je pense. cf. FR_communes.R pour les détails.
                 "DE" = c("gender", "income", "age", "urbanity", "diploma"),
                 "ES" = c("gender", "income", "age", "urbanity", "diploma"),
                 "UK" = c("gender", "income", "age", "urbanity", "diploma")
  )
  
  qs <- read.xlsx("../questionnaire/specificities.xlsx", sheet = "Quotas", rowNames = T, rows = c(1, 6, 8), cols = 1:34)
  pop_freq <- list(
    "EU" = list(
      "gender" = c(qs["EU","women"], 0.001, qs["EU","men"])/1000,
      "income" = rep(.25, 4),
      "age" = qs["EU", c("18-24", "25-34", "35-49", "50-64", ">65")]/1000,
      "urbanity" = qs["EU", c("Cities", "Towns.and.suburbs", "Rural")]/1000,
      "diploma" = qs["EU", c("Below.upper.secondary.25-64.0-2", "Upper.secondary.25-64.3", "Above.Upper.secondary.25-64.4-8")]/1000, 
      "EU_country" = qs["EU", c("FR", "DE", "ES", "UK")]/1000
    ),
    "US" = list(
      "gender" = c(qs["US","women"], 0.001, qs["US","men"])/1000,
      "income" = rep(.25, 4),
      "age" = qs["US", c("18-24", "25-34", "35-49", "50-64", ">65")]/1000,
      "urbanity" = c(qs["US", "Cities"], 0.001, qs["US","Rural"])/1000,
      "diploma" = qs["US", c("Below.upper.secondary.25-64.0-2", "Upper.secondary.25-64.3", "Above.Upper.secondary.25-64.4-8")]/1000, 
      "US_region" = qs["US", c("Region.1", "Region.2", "Region.3", "Region.4")]/1000,
      "US_race" = qs["US", c("White.non.Hispanic", "Hispanic", "Black", "Other")]/1000
    ))
  for (c in c("EU", "US")) pop_freq[[c]]$diploma <- c(unlist(pop_freq[[c]]$diploma), "Not 25-64" = 1-sum(pop_freq[[c]]$diploma))
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
  # e <- read_csv("../data/US1_pilot.csv")
  # for (i in 1:length(e)) {
  #   label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
  #   print(paste(i, label(e[[i]])))
  # }
  # 
  if (missing(wave) | wave == "full") {
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

prepare <- function(incl_quality_fail = FALSE, exclude_speeder=TRUE, exclude_screened=TRUE, only_finished=TRUE, only_known_agglo=T, duration_min=0, country = "US", wave = NULL, weighting = TRUE, replace_brackets = FALSE, zscores = T, zscores_dummies = FALSE, remove_id = FALSE, efa = FALSE, combine_age_50 = T) { #(country!="DK") # , exclude_quotas_full=TRUE
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
  
  print(paste(length(which(e$excluded=="QuotaMet")), "QuotaMet"))
  e$finished[e$excluded=="QuotaMet"] <- "False" # To check the number of QuotaMet that shouldn't have incremented the quota, comment this line and: decrit(e$each_strate[e$exclu=="QuotaMet" & e$csp=="Employé" & !grepl("2019-03-04 07", e$date)])
  if (incl_quality_fail) e <- e[replace_na(e$excluded, "na") != "QuotaMet" & !is.na(e$attention_test) & !(replace_na(e$excluded, "na") == "Screened" & replace_na(e$attention_test) == "A little"),] # allqa: e[e$finished == 1
  if (exclude_screened & !incl_quality_fail) { e <- e[is.na(e$excluded),] }
  if (exclude_speeder) e <- e[as.numeric(as.vector(e$duration)) > duration_min,] # & !incl_quality_fail
  if (only_finished & !incl_quality_fail) e <- e[e$finished==1,] 
  # if (only_finished | incl_quality_fail) { # TODO: le faire marcher même pour les autres
    e <- convert(e, country = country, wave = wave, weighting = weighting, zscores = zscores, zscores_dummies = zscores_dummies, efa = efa, combine_age_50 = combine_age_50, only_finished = only_finished)
    e <- e[,!duplicated(names(e))]
    # if (!incl_quality_fail) e <- e[e$attention_test == T, ] # TODO!
    # if (weighting) {
    #   e$weight <- weighting(e)
    #   if ("vote_2020" %in% names(e) & (sum(e$vote_2020=="PNR/no right")!=0)) e$weight_vote <- weighting(e, vote = T)  }
    
    # e$left_right_na <- as.numeric(e$left_right)
    # e$left_right_na[e$indeterminate == T] <- wtd.mean(e$left_right, weights = e$weight)
  # } else e <- create_education(e, country, only = TRUE)
  
  
  e$valid <- (as.numeric(e$progress) > 1) & (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded)
  label(e$valid) <- "valid: Respondents that has not been screened out due to speed or failure to the attention test."
  e$dropout <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded) & e$finished != "1"
  label(e$dropout) <- "dropout: Respondent who did not complete the survey though was not excluded."
  e$finished_attentive <- (e$valid | (e$duration <= duration_min & e$attention_test=="A little")) & e$finished==1
  label(e$finished_attentive) <- "finished_attentive: Respondent completed the survey and did not fail the attention test."
  
  # e$sample <- "a"
  # e$sample[e$finished=="True"] <- "e"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min] <- "p"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min & e$excluded==""] <- "r"
  
  return(e)
}

convert <- function(e, country, wave = NULL, weighting = T, zscores = T, zscores_dummies = FALSE, efa = FALSE, combine_age_50 = T, only_finished = T) {
  text_pnr <<- c("US" = "I don't know", "US" = "Prefer not to say",  "US" = "Don't know, or prefer not to say",  "US" = "Don't know",  "US" = "Don't know or prefer not to say", "US" = "I don't know",
                "US" = "Don't know, prefer not to say",  "US" = "Don't know, or prefer not to say.",  "US" = "Don't know,  or prefer not to say", "US" = "I am not in charge of paying for heating; utilities are included in my rent", "PNR",
                "FR" = "Je ne sais pas", "FR" = "Ne sais pas, ne souhaite pas répondre", "FR" = "NSP (Ne sais pas, ne se prononce pas)", "FR" = "NSP (Ne sait pas, ne se prononce pas)", "FR" = "Préfère ne pas le dire",
                "UK" = "I don't know", "DE" = "Ich weiß es nicht")
  text_yes <<- c("US" = "Yes", 
                "FR" = "Oui")
  text_no <<- c("No")
  text_intensity <<- c("Not at all", "A little", "Moderately", "A little", "A great deal")
  text_support <<- c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")
  text_importance <<- c("Not at all important", "Not so important", "Quite important", "Very important")
  text_problem <<- c("Not an important issue for me", "An issue but there are other priorities", "An issue but we do already what we can", "An important issue, we should do more", "One of the most pressing issue of our time")
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
    }
    
    if ("global_tax_sharing" %in% names(e)) {
      e$global_tax_sharing_original <- e$global_tax_sharing
      e$global_tax_sharing[grepl("whole wealth tax financing national budgets", e$global_tax_sharing)] <- "No"
      e$global_tax_sharing[grepl("half of it financing low-income countries", e$global_tax_sharing)] <- "Yes"
      e$global_tax_sharing[is.na(e$global_tax_sharing_original)] <- NA
      label(e$global_tax_sharing) <- "global_tax_sharing: T/F/NA Prefers to allocate half of global wealth tax to low-income countries rather than keeping all in collector country's national budgets. NA if the question is not asked (cf. branch_global_tax)."
      e$branch_global_tax[!is.na(e$global_tax_sharing)] <- "sharing"
      e$branch_global_tax[!is.na(e$global_tax_global_share)] <- "global_share"
      e$branch_global_tax[!is.na(e$global_tax_support)] <- "separate"
      label(e$branch_global_tax) <- "branch_global_tax: separate/sharing/global_share/NA Way to ask the preference for funding low-income countries through a global tax on the rich: either 'separate'ly the support for a national and a global tax on millionaires; whether to allocate half or none of the global tax to low-income countries; the 'global_share' in 0 to 100%."
    }
  }

  variables_support <<- names(e)[grepl('support', names(e))]
  variables_other_policies <<- names(e)[grepl('_support', names(e)) & !grepl("nr|gcs|foreign_aid|_tax_", names(e))]
  variables_climate_policies <<- variables_other_policies[grepl('climate', variables_other_policies)]
  variables_global_policies <<- variables_other_policies[!grepl('climate', variables_other_policies)]
  variables_support_binary <<- c("gcs_support", "nr_support", "support_igr", "global_tax_sharing")
  variables_support_likert <<- c("global_tax_support", "national_tax_support", variables_other_policies)
  variables_petition <<- names(e)[grepl('petition', names(e)) & !grepl('branch_petition', names(e))]
  variables_gcs_important <<- names(e)[grepl('gcs_important', names(e))]
  variables_problem <<- names(e)[grepl('problem_', names(e))]
  variables_win_lose <<- names(e)[grepl('win_lose', names(e))]
  variables_foreign_aid_amount <<- c("foreign_aid_belief", "foreign_aid_preferred_no_info", "foreign_aid_preferred_info", "foreign_aid_preferred")
  variables_duration <<- names(e)[grepl('duration', names(e))]
  variables_donation <<- c("donation_nation", "donation_africa", "donation")
  variables_list_exp <<- names(e)[grepl('list_exp', names(e))]
  variables_belief <<- c("gcs_belief", "nr_belief")
  variables_points <<- names(e)[grepl("points", names(e)) & !grepl("order|duration", names(e))]
  
  variables_foreign_aid_raise <<- names(e)[grepl('foreign_aid_raise_how', names(e))]
  variables_foreign_aid_reduce <<- names(e)[grepl('foreign_aid_reduce_how', names(e))]
  variables_foreign_aid_no_ <<- names(e)[grepl('foreign_aid_no_', names(e))]
  variables_foreign_aid_condition <<- names(e)[grepl('foreign_aid_condition', names(e))]
  
  variables_conjoint <<- names(e)[grepl('conjoint_', names(e)) & !grepl("order|duration", names(e))]
  variables_conjoint_a <<- c("conjoint_irg_ir")
  variables_conjoint_b <<- c("conjoint_ir_gr", "conjoint_r_igr", "conjoint_gr_r", "conjoint_ir_r")
  variables_conjoint_c <<- c("conjoint_left_right", "conjoint_leftg_right")
  variables_conjoint_d <<- c("conjoint_left_a_b", "conjoint_left_ag_b")
  conjoint_position_g <<- c("A", "B", "B", "A", "None", "None", "A", "Random", "A")
  names(conjoint_position_g) <<- variables_conjoint
  
  variables_matrices <<- list("global_policies" = variables_global_policies,
                              "climate_policies" = variables_climate_policies,
                              "problem" = variables_problem,
                              "gcs_important" = variables_gcs_important)
  
  for (i in intersect(c(variables_duration, "hh_size", "Nb_children__14", "zipcode", variables_donation, variables_belief, variables_list_exp, variables_points, "global_tax_global_share" #, "age"
  ), names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector( gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in variables_duration) e[[v]] <- e[[v]]/60
  
  for (j in intersect(c("couple", variables_petition, variables_support_binary), names(e))) {
    temp <- 1*(e[j][[1]] %in% text_yes) - 0.1*(e[j][[1]] %in% text_pnr) # - (e[j][[1]] %in% text_no)
    temp[is.na(e[j][[1]])] <- NA
    e[j][[1]] <- as.item(temp, labels = structure(c(0,-0.1,1), names = c("No","PNR","Yes")),
                         missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
    # e[j][[1]] <- as.item(as.character(e[j][[1]]), labels = structure(yes_no_names, names = c("NA","No","PNR","Yes")),
    #             missing.values = c("","PNR"), annotation=attr(e[j][[1]], "label"))
  }
  
  # for (j in intersect(c("region", "education", "employment_status", "vote" # TODO
  # ), names(e))) {
  #   e[j][[1]] <- as.item(as.factor(e[j][[1]]), missing.values = c("PNR", "", NA), annotation=paste(attr(e[j][[1]], "label")))
  # }
  
  for (j in names(e)) {
    if ((grepl('race_|home_|foreign_aid_raise_how|foreign_aid_reduce_how|foreign_aid_condition|foreign_aid_no_', j)) & !(grepl('_other$|order_', j))) {
      temp <- label(e[[j]])
      e[[j]] <- e[[j]]!="" # e[[j]][e[[j]]!=""] <- TRUE
      e[[j]][is.na(e[[j]])] <- FALSE
      label(e[[j]]) <- temp
    }
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% c("A little")
  
  e$country_name <- e$country
  if (grepl("US", country)) e$country_name <- "United States"
  e$country <- countries[e$country_name]
  
  temp <- as.numeric(as.vector(gsub("[^0-9\\.]", "", gsub(".*to", "", e$age_exact))))
  temp <- temp - 1.5
  temp[temp == 18.5] <- 19.5
  temp[temp == 22.5] <- 23
  temp[temp == 97.5] <- 95
  temp[temp == 98.5] <- 101
  e$age_exact <- as.item(temp, labels = structure(c(16.5, 19.5, 23, seq(27.5, 87.5, 5), 95, 101), names = c("< 18", "18-20", "21-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-99", "100+")), missing.values=c(NA), annotation=Label(e$age_exact))
  temp <- 21.5*(e$age_exact < 25) + 30*(e$age_exact < 35 & e$age_exact > 25) + 42.5*(e$age_exact < 50 & e$age_exact > 35) + 57.5*(e$age_exact < 65 & e$age_exact > 50) + 71*(e$age_exact > 65)
  e$age <- as.item(temp, labels = structure(c(21.5, 30, 42.5, 57.5, 71), names = c("18-24", "25-34", "35-49", "50-64", "65+")), missing.values=c(NA), annotation=Label(e$age_exact))
  
  if ("race_black" %in% names(e)) {
    e$race <- "Other"
    e$race[e$race_white==T & e$race_asian == FALSE & e$race_native == FALSE] <- "White only"
    e$race[e$race_hispanic==T] <- "Hispanic"
    e$race[e$race_black==T] <- "Black"
    label(e$race) <- "race: White only/Hispanic/Black/Other. True proportions: .601/.185/.134/.08"
  }

  e$income_original <- e$income
  if (grepl("US", country)) {
    income_string <- c("< 20k", "20-35k", "35-42k", "42-50k", "50-65k", "65-82k", "82-103k", "103-130k", "130-145k", "145-165k", "165-250k", "> 250k")
    income_number <- c(15, 28, 39, 46, 58, 74, 92, 116, 142, 155, 205, 285)
    decile <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8, 9, 10)
    quartile <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
    names(income_number) <- names(decile) <- names(quartile) <- income_string
    temp <- as.numeric(income_number[as.vector(gsub("less than \\$", "< ", gsub("between \\$", "", gsub(",000", "k", gsub(",001 and \\$", "-", gsub("165,000 a", "165,001 a", e$income_original))))))])
    e$income <- as.item(temp, labels = structure(income_number, names = income_string), missing.values=c(NA), annotation=Label(e$income))
    e$income_decile <- as.numeric(decile[as.character(e$income)])
    e$income_quartile <- as.numeric(quartile[as.character(e$income)])
  } else {
    e$income_quartile <- ceiling(as.numeric(as.vector(gsub("[^0-9\\.]", "", gsub("and.*", "", e$income_original))))/250)
    e$income_decile <- ceiling(as.numeric(as.vector(gsub("900", "901", gsub("[^0-9\\.]", "", gsub("and.*", "", e$income_original)))))/100)
  }
  e$income_factor <- as.factor(e$income_quartile)
  label(e$income_decile) <- "income_decile: [1-10] Decile of income. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  label(e$income_quartile) <- "income_quartile: [1-4] Quartile of income. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  label(e$income_factor) <- "income_factor: 1/2/3/4 Quartile of income, as a factor rather than a numeric vector. For US, this is total household income and for EU, equivalised disposable income (i.e. total income divided by the household's number of consumption units)."
  
  e$urban_category <- as.numeric(e$urban_category)
  e$urban_category[e$urban_category == 0] <- NA
  label(e$urban_category) <- "urban_category: [1-4] Computed from the zipcode. NA indicates an unrecognized zipcode. For FR/DE/ES, Eurostat's degree of urbanization (1: Cities, 2: Towns and suburbs, 3: Rural). For the UK we use another classification that tends to classify zipcodes as more rural than Eurostat (cf. zipcodes.R). For the US, recoded from RUCA codes (1: Metropolitan core (RUCA 1, 73% pop), 2: Metro non-core (2-3), 3: Micropolitan or Small town (< 50k, 4-9), 4: Rural (10))."
  temp <- as.numeric(as.vector(e$urban_category))
  if (grepl("US", country)) temp <- as.numeric(as.vector(e$urban_category - 1 * (e$urban_category > 2)))
  e$urbanity <- as.item(temp, labels = structure(1:3, names = c("Cities", "Towns and suburbs", "Rural")), missing.values=c(NA), annotation="urbanity: 1: Cities / 2: Towns and suburbs / 3: Rural. Computed from the zipcode. For EU, equals urban_category; for the U.S., urbanity = 1; 2; 3 (resp.) corresponds to urban_category = 1; 2 or 3; 4.")
  e$urban <- e$urban_category == 1
  label(e$urban) <- "urban: T/F urban_category == 1: Cities (EU) or Core metropolitan (US)."
  
  e$education_original <- e$education
  # ISCED_EU <- c("0-1", "2", "3 pro basic", "3 pro advanced", "3 general", "4-5", "6", "7-8")
  ISCED <- c("0-1", "2", "3.1", "3.2", "3.3", "4-5", "6", "7-8")
  names(ISCED) <- c("Primary school or less", "Eigth grade", "Some high school", "Regular high school diploma/GED or alternative credential", "Some college, no degree", "2-year college degree or associates degree (for example: AA, AS)", "Bachelor's degree (for example: BA, BS)", "Master’s degree or above (MA, MS, MEng, MEd, MSW, MBA, MD, DDS, DVM, LLB, JD, PhD)")
  e$education <- ISCED[e$education_original]
  label(e$education) <- "education: What is the highest level of education you have completed? /!\ For EU, the values don't correspond to the responses. To see the correspondence between values and responses in each country, cf. specificities.xlsx$Education"
  e$diploma[e$education %in% c("0-1", "2")] <- 1 
  e$diploma[grepl("3", e$education)] <- 2 
  e$diploma[e$education %in% c("4-5", "6", "7-8")] <- 3 
  e$diploma <- as.item(e$diploma, labels = structure(1:3, names = c("Below upper secondary", "Upper secondary", "Post secondary")), missing.values=c(NA, "Not 25-64"), annotation="diploma: 1: Below upper secondary (ISCED 0-2) / 2: Upper secondary (ISCED 3) / 3: Post secondary (ISCED 4-8), recoded from education.")
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
  
  e$inactive <- e$employment_agg %in% c("Retired", "Not working")
  e$employment <- e$employment_agg == "Working"
  e$employment[e$age == "65+"] <- NA
  label(e$employment) <- "employment: T/F/NA indicator that the respondent is employed (employment_agg == Working), NA if s-he is above 65."
  
  if ("wealth_couple" %in% names(e)) {
    e$wealth[!is.na(e$wealth_couple)] <- e$wealth_couple[!is.na(e$wealth_couple)]
    e$wealth[!is.na(e$wealth_single)] <- e$wealth_single[!is.na(e$wealth_single)]
    temp <-  grepl("Less than \\$0", e$wealth) + 2 * grepl("Close to \\$0", e$wealth) + 3 * grepl("Between \\$4,000", e$wealth) + 4 * (e$wealth %in% c("Between $120,000 and $380,000", "Between $60,000 and $190,000")) + 5 * grepl("More than", e$wealth)
    e$wealth <- as.item(temp, labels = structure(c(1:5, 0), names = c("Q1","Q2","Q3","Q4","Q5", "PNR")), missing.values = c(NA, 0), annotation="wealth: Quintile of wealth (from wealth_couple and wealth_single).")
  }
  
  e$owner <- e$home_owner == T | e$home_landlord == T
  label(e$owner) <- "owner: Owner or Landlord renting out property to: Are you a homeowner or a tenant?"
  
  if (only_finished) {
    if ("list_exp_igr" %in% names(e)) {
      e$branch_list_exp[!is.na(e$list_exp_i)] <- "i"
      e$branch_list_exp[!is.na(e$list_exp_igr)] <- "igr"
      e$branch_list_exp[!is.na(e$list_exp_gr)] <- "gr"
      e$branch_list_exp[!is.na(e$list_exp_ir)] <- "ir"
      if (wave == "pilot" & country != "US2") e$list_exp_ir[e$branch_list_exp == "gr" & e$country %in% c("US", "FR", "UK")] <- e$list_exp_gr[e$branch_list_exp == "gr" & e$country %in% c("US", "FR", "UK")]
      if (wave == "pilot" & country != "US2") e$branch_list_exp[!is.na(e$list_exp_gr) & e$country %in% c("US", "FR", "UK")] <- "ir"
      label(e$branch_list_exp) <- "branch_list_exp: i/ir/gr/igr Variant of the list experiment faced, where i denotes coal exit (US) or the buildings' insulation plan (EU), r the national redistribution, and g the global climate scheme. Marriage only for opposite-sex couples (US) and death penalty for major crimes (EU) were also systematically included."
      e$branch_list_exp_g <- grepl("g", e$branch_list_exp)
      e$branch_list_exp_r <- grepl("r", e$branch_list_exp)
      label(e$branch_list_exp_r) <- "branch_list_exp_r: T/F r (national redistribution) is present in the list experiment."
      label(e$branch_list_exp_g) <- "branch_list_exp_g: T/F g (global climate scheme) is present in the list experiment."
      for (v in c("i", "ir", "gr", "igr")) e$list_exp[e$branch_list_exp == v] <- e[[paste0("list_exp_", v)]][e$branch_list_exp == v]
      label(e$list_exp) <- "list_exp: [0-4] Number of supported policies in the list experiment (combining all branches, cf. branch_list_exp and variables_list_exp)."
    }
    
    for (v in c(variables_support_likert)) {
      if (v %in% names(e)) {
        temp <-  temp <- 2 * (e[[v]] %in% text_support[5]) + (e[[v]] %in% text_support[4]) - (e[[v]] %in% text_support[2]) - 2 * (e[[v]] %in% text_support[1])
        temp[is.na(e[[v]])] <- NA
        e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")), missing.values=c(NA), annotation=Label(e[[v]])) 
      } }
    
    for (v in intersect(variables_gcs_important, names(e))) {
      temp <-  temp <- 2 * (e[[v]] %in% text_importance[4]) + (e[[v]] %in% text_importance[3]) - (e[[v]] %in% text_importance[2]) - 2 * (e[[v]] %in% text_importance[1])
      temp[is.na(e[[v]])] <- NA
      e[[v]] <- as.item(temp, labels = structure(c(-2,-1,1,2), names = sub(" important", "", text_importance)), missing.values=c(NA), annotation=Label(e[[v]]))    
      e$branch_gcs_perception[!is.na(e$gcs_field)] <- "field"
      e$branch_gcs_perception[!is.na(e[[v]])] <- "gcs_important"
      label(e$branch_gcs_perception) <- "branch_gcs_perception: field/gcs_important/NA Whether the perception of the global climate scheme is asked as a matrix 'gcs_important' or an entry field 'field'"
    }
    
    for (v in intersect(variables_problem, names(e))) {
      temp <-  temp <- 2 * (e[[v]] %in% text_problem[5]) + (e[[v]] %in% text_problem[4]) - (e[[v]] %in% text_problem[2]) - 2 * (e[[v]] %in% text_problem[1])
      temp[is.na(e[[v]])] <- NA
      e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Not an issue", "Not a priority", "Already addressed", "Important, should do more", "Most pressing issue")), missing.values=c(NA), annotation=Label(e[[v]]))    
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
      e$foreign_aid_more_less <- as.item(e$foreign_aid_more_less, labels = structure(-1:1, names = c("Less", "Same"," More")), missing.values = NA, 
                                         annotation = "foreign_aid_more_less: -1: Less / 0: Same / 1: More. Whether the respondent wants more or less foreign aid than now. Depending on info_foreign_aid = T or F, current aid is taken as the actual or the believed one.")
    }
    
    for (v in intersect(variables_win_lose, names(e))) e[[paste0(v, "_original")]] <- e[[v]]
    if ("nr_win_lose" %in% names(e)) {
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would lose and the richest Americans would lose."] <- "Typical lose, richest lose"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would lose and the richest Americans would win."] <- "Typical lose, richest win"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would win and the richest Americans would lose."] <- "Typical win, richest lose"
      e$nr_win_lose[e$nr_win_lose == "Typical Americans would win and the richest Americans would win."] <- "Typical win, richest win"
      e$nr_understood <- e$nr_win_lose == "Typical win, richest lose"
      label(e$nr_understood) <- "nr_understood: T/F Correct answer to nr_win_lose (in national redistribution, typical people win, richest lose)."
    }
    
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
    
    e$score_understood <- e$nr_understood + e$gcs_understood + e$both_understood
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
    }
    
    for (v in intersect(names(e), c(variables_conjoint))) {
      e[[v]] <- sub(".* (.*)", "\\1", e[[v]])
      e[[v]][e[[v]] == "them"] <- "None"
      if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Democrat", "A")] <- "Left"
      if (v %in% variables_conjoint_c) e[[v]][e[[v]] %in% c("Republican", "B")] <- "Right"
      if (v %in% variables_conjoint_a) e$conjoint_a <- e[[v]] == conjoint_position_g[v]
      if (v %in% variables_conjoint_b) e$conjoint_b[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == conjoint_position_g[v]
      if (v %in% variables_conjoint_b) e$branch_conjoint_b[!is.na(e[[v]])] <- sub("conjoint_", "", v)
      if (v %in% variables_conjoint_c) e$conjoint_c[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "Left" #conjoint_position_g[v]
      if (v %in% variables_conjoint_c) e$branch_conjoint_c[!is.na(e[[v]])] <- sub("conjoint_", "", v)
      if (v %in% "conjoint_left_ag_b") e$conjoint_d[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == conjoint_position_g[v]
      if (v == "conjoint_ir_r") e$conjoint_b[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "A"
    }
    label(e$branch_conjoint_b) <- "branch_conjoint_b: ir_gr/r_igr/gr_r/ir_r Random branch faced by the respondent in conjoint analysis (b)."
    label(e$branch_conjoint_c) <- "branch_conjoint_c: left_right/leftg_right Random branch faced by the respondent in conjoint analysis (c), used in branch_c_gcs := branch_conjoint_c == 'leftg_right.'"
    label(e$conjoint_a) <- "conjoint_a: T/F Bundle with GCS is chosen in conjoint analysis (a), i.e. irg > ir."
    label(e$conjoint_b) <- "conjoint_b: T/F Bundle with GCS is chosen in conjoint analysis (b) when GCS is in one Bundle, or ir > r in case GCS is in no bundle."
    label(e$conjoint_c) <- "conjoint_c: T/F Left-wing candidate is chosen in conjoint_c (cf. branch_c_gcs to know whether the Left candidate includes GCS in their platform)."
    e$conjoint_d[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] == "A"
    label(e$conjoint_d) <- "conjoint_d: T/F Candidate with GCS is chosen in conjoint analysis (d). In US1, question asked only to non-Republican."
    label(e$conjoint_a) <- "conjoint_a: T/F Bundle with GCS is chosen in conjoint analysis (a), i.e. irg > ir."
    label(e$conjoint_b) <- "conjoint_b: T/F Bundle with GCS is chosen in conjoint analysis (b) when GCS is in one Bundle, or ir > r in case GCS is in no bundle."
    label(e$conjoint_c) <- "conjoint_c: T/F Left-wing candidate is chosen in conjoint_c (cf. branch_c_gcs to know whether the Left candidate includes GCS in their platform)."
    e$branch_b_gcs <- grepl("g", e$branch_conjoint_b)
    label(e$branch_b_gcs) <- "branch_b_gcs: T/F Whether GCS is in one Bundle in conjoint_b, i.e. =F iff branch_conjoint_b == 'conjoint_ir_r'."
    e$conjoint_b_na <- e$conjoint_b
    e$conjoint_b_na[e$branch_b_gcs == FALSE] <- NA
    label(e$conjoint_b_na) <- "conjoint_b_na: T/F/NA Bundle with GCS is chosen in conjoint analysis (b) when GCS is in one Bundle, NA in case GCS is in no bundle (i.e. branch_b_gcs == F i.e. branch_conjoint_b == 'conjoint_ir_r'."
    e$branch_c_gcs <- grepl("g_", e$branch_conjoint_c)
    label(e$branch_c_gcs) <- "branch_c_gcs: T/F Whether GCS is in the Left-wing platform in conjoint_c, i.e. =T iff branch_conjoint_c == 'conjoint_leftg_right'."
    e$conjoint_r_type <- "None"
    e$conjoint_r_type[e$`F-1-1-5` %in% policies_names["foreign1",]] <- "A"
    e$conjoint_r_type[e$`F-1-2-5` %in% policies_names["foreign1",]] <- "B"
    e$conjoint_r_type[e$`F-1-1-5` %in% policies_names["foreign1",] & e$`F-1-2-5` %in% policies_names["foreign1",]] <- "Both"
    label(e$conjoint_r_type) <- "conjoint_r_type: None/A/B/Both Which candidate includes GCS in their program in conjoint_left_a_b"
    e$conjoint_r <- e$conjoint_left_a_b == e$conjoint_r_type
    e$conjoint_r[e$conjoint_r_type %in% c("None", "Both")] <- NA
    label(e$conjoint_r) <- "conjoint_r: T/F/NA Whether the candidate who includes GCS in their program is preferred (NA if both or none include GCS). In US1, question asked only to non-Republican."
    
    if ("interest_politics" %in% names(e)) temp <- 2 * (e[[v]] %in% text_intensity[5]) + (e[[v]] %in% text_intensity[4]) - (e[[v]] %in% text_intensity[2]) - 2 * (e[[v]] %in% text_intensity[1])
    if ("interest_politics" %in% names(e)) e$interest_politics <- as.item(temp, labels = structure(c(-2:2),  names = c(text_intensity)), annotation=Label(e$interest_politics))
    
    if ("vote_participation" %in% names(e)) { # TODO
      e$vote_participation[grepl("right to vote", e$vote_participation)] <- "No right to vote"

      major_threshold <- 5 # 5% is the treshold to be considered a major candidate
      for (c in tolower(countries)) {
        if (paste0("vote_", c, "_voters") %in% names(e)) {
          e$vote[!is.na(e[[paste0("vote_", c, "_voters")]]) & e$vote_participation=="Yes"] <- e[[paste0("vote_", c, "_voters")]][!is.na(e[[paste0("vote_", c, "_voters")]]) & e$vote_participation=="Yes"]
          e$vote[!is.na(e[[paste0("vote_", c, "_non_voters")]]) & e$vote_participation!="Yes"] <- e[[paste0("vote_", c, "_non_voters")]][!is.na(e[[paste0("vote_", c, "_non_voters")]]) & e$vote_participation!="Yes"]
          major_candidates[[c]] <<- setdiff(names(table(e$vote[e$country == toupper(c)]))[table(e$vote[e$country == toupper(c)]) > major_threshold * sum(e$country == toupper(c)) / 100], text_pnr)
          minor_candidates[[c]] <<- setdiff(names(table(e$vote[e$country == toupper(c)]))[table(e$vote[e$country == toupper(c)]) <= .05 * sum(e$country == toupper(c))], text_pnr) 
          e$vote_agg[e$country == toupper(c) & e$vote %in% c(major_candidates[[c]], text_pnr)] <- e$vote[e$country == toupper(c) & e$vote %in% c(major_candidates[[c]], text_pnr)]
          e$vote_agg[e$country == toupper(c) & e$vote %in% minor_candidates[[c]]] <- "Other"
        }
      }
      e$vote_participation <- as.item(as.character(e$vote_participation), missing.values = 'PNR', annotation=Label(e$vote_participation))
      e$vote <- as.item(as.character(e$vote), missing.values = 'PNR', annotation="vote: What the respondent has voted or would have voted in the last election, combining vote_[country]_voters and vote_[country]_non_voters.")
      e$vote_agg <- as.item(as.character(e$vote_agg), missing.values = 'PNR', annotation=paste0("vote_agg: What the respondent has voted or would have voted in the last election, lumping minor candidates (with less than ", major_threshold, "% of $vote) into 'Other'. Build from $vote that combines $vote_[country]_voters and $vote_[country]_non_voters."))
      e$voted <- e$vote_participation == 'Yes'
      label(e$voted) <- "voted: Has voted in last election: Yes to vote_participation."
      major_candidates <<- major_candidates
      minor_candidates <<- minor_candidates
    }
    
    e$survey_biased[e$survey_biased %in% c("Yes, left-wing biased")] <- "Yes, left"
    e$survey_biased[e$survey_biased %in% c("Yes, right-wing biased")] <- "Yes, right"
    e$survey_biased[e$survey_biased %in% c("No, I do not feel it was biased")] <- "No"
    if ("Yes, right" %in% levels(as.factor(e$survey_biased))) e$survey_biased <- relevel(relevel(as.factor(e$survey_biased), "Yes, left"), "No")
    e$survey_biased_yes <- e$survey_biased != 'No'
    e$survey_biased_left <- e$survey_biased == "Yes, left"
    e$survey_biased_right <- e$survey_biased == "Yes, right"
    label(e$survey_biased_yes) <- "survey_biased_yes: T/F Finds the survey biased (survey_biased != No)"
    label(e$survey_biased_left) <- "survey_biased_left: T/F Finds the survey left-wing biased (survey_biased == Yes, left)"
    label(e$survey_biased_right) <- "survey_biased_right: T/F Finds the survey right-wing biased (survey_biased == Yes, right)"
    
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
      e$conjoint_a_irg_support_no <- e$conjoint_a == T & e$gcs_support == "No"
      e$conjoint_a_ir_support_yes <- e$conjoint_a == F & e$gcs_support == "Yes"
      label(e$conjoint_a_matches_support) <- "conjoint_a_matches_support: T/F The answer to conjoint analysis (a) (irg vs. ir) corresponds to the answer to the support of GCS."
      label(e$conjoint_a_irg_support_no) <- "conjoint_a_irg_support_no: T/F Prefers irg to ir in conjoint analysis (a) but does not support GCS."
      label(e$conjoint_a_ir_support_yes) <- "conjoint_a_ir_support_yes: T/F Prefers ir to irg in conjoint analysis (a) but supports GCS."
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
  
  e$wrong_language <- (e$country == "US" & e$language != "EN") | (e$country == "DE" & e$language != "DE") | (e$country == "FR" & e$language != "FR") | (e$country == "ES" & e$language != "ES-ES") | (e$country == "UK" & e$language != "EN-GB")
  label(e$wrong_language) <- "wrong_language: T/F The language does not correspond to the respondent's country (including Spanish in the U.S.)."
  
  count_IP <- rle(as.vector(sort(e$ip)))
  e$number_same_ip <- count_IP[[1]][match(e$ip, count_IP[[2]])]
  e$duplicate_ip <- e$number_same_ip > 1
  label(e$number_same_ip) <- "number_same_ip: Number of respondents with the same IP."
  label(e$duplicate_ip) <- "duplicate_ip: T/F The respondent's IP is used by other respondents."
  
  print(paste("convert: success", country))
  return(e)
}

us1p <- prepare(country = "US1", wave = "pilot", weighting = FALSE)
eup <- prepare(country = "EU", wave = "pilot", weighting = FALSE)

e <- ep <- merge(us1p, eup, all = T)

# variables_include <- c("finished", "excluded", "duration", "attention_test", "progress", "dropout", "valid", "finished_attentive", "education_original", "gender", "age", "income", "owner", "female", "income_factor", "treatment", "urban_category", "region") 
eupa <- prepare(country = "EU", wave = "pilot", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
us1pa <- prepare(country = "US1", wave = "pilot", weighting = FALSE, exclude_speeder = F, only_finished = F, exclude_screened = F)#[, variables_include]
