# Consistency: increase/reduce aid for those with the info + later question

source(".Rprofile")
source("relabel_rename.R")

##### Income quantiles #####
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


##### Quotas #####
{
  levels_quotas <- list(
  "gender" = c("Woman", "Other", "Man"), # we could add: urbanity, education, wealth, occupation, employment_agg, marital_status, Nb_children, HH_size, home (ownership)
  "income" = c("Q1", "Q2", "Q3", "Q4"),
  "age" = c("18-24", "25-34", "35-49", "50-64", "65+"),
  "urbanity" = c("Cities", "Towns and suburbs", "Rural"),
  "diploma" = c("Below upper secondary", "Upper secondary", "Above upper secondary", "Not 25-64"),
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

convert <- function(e, country, wave = NULL, weighting = T, zscores = T, zscores_dummies = FALSE, efa = FALSE, combine_age_50 = T) {
  text_pnr <- c("US" = "I don't know", "US" = "Prefer not to say",  "US" = "Don't know, or prefer not to say",  "US" = "Don't know",  "US" = "Don't know or prefer not to say", "US" = "I don't know",
                "US" = "Don't know, prefer not to say",  "US" = "Don't know, or prefer not to say.",  "US" = "Don't know,  or prefer not to say", "US" = "I am not in charge of paying for heating; utilities are included in my rent", "PNR",
                "FR" = "Je ne sais pas", "FR" = "Ne sais pas, ne souhaite pas répondre", "FR" = "NSP (Ne sais pas, ne se prononce pas)", "FR" = "NSP (Ne sait pas, ne se prononce pas)", "FR" = "Préfère ne pas le dire",
                "UK" = "I don't know", "CN" = "我不知道", "DE" = "Ich weiß es nicht")
  text_yes <- c("US" = "Yes", 
                "FR" = "Oui")
  text_no <- c("No")
  
  variables_duration <<- names(e)[grepl('duration', names(e))]
  for (i in intersect(c(variables_duration, "hh_size", "Nb_children__14", "zipcode", "donation"#, "age"
  ), names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector( gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in variables_duration) e[[v]] <- e[[v]]/60

  for (j in intersect(c("couple"), names(e))) {
    temp <- 1*(e[j][[1]] %in% text_yes) - 0.1*(e[j][[1]] %in% text_pnr) # - (e[j][[1]] %in% text_no)
    temp[is.na(e[j][[1]])] <- NA
    e[j][[1]] <- as.item(temp, labels = structure(c(0,-0.1,1), names = c("No","PNR","Yes")),
                         missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
    # e[j][[1]] <- as.item(as.character(e[j][[1]]), labels = structure(yes_no_names, names = c("NA","No","PNR","Yes")),
    #             missing.values = c("","PNR"), annotation=attr(e[j][[1]], "label"))
  }
  
  for (j in intersect(c(#"gender", "region", "education", "employment_status", "income", "wealth", "survey_biased", "vote", 
    ), names(e))) {
    e[j][[1]] <- as.item(as.factor(e[j][[1]]), missing.values = c("PNR", "", NA), annotation=paste(attr(e[j][[1]], "label"))) 
  } 
  
  for (j in names(e)) {
    if ((grepl('race_|home_', j)) & !(grepl('_other$|order_', j))) {
      temp <- label(e[[j]])
      e[[j]] <- e[[j]]!="" # e[[j]][e[[j]]!=""] <- TRUE
      e[[j]][is.na(e[[j]])] <- FALSE
      label(e[[j]]) <- temp
    }
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% text_a_little
  
  for (v in intersect(names(e), c(variables_burden_sharing, variables_burden_share, variables_policies_effect, variables_policies_fair, "should_fight_CC", "can_trust_people", "can_trust_govt", "trust_public_spending", "CC_problem"))) { 
    temp <-  2 * (e[[v]] %in% text_strongly_agree) + (e[[v]] %in% text_somewhat_agree) - (e[[v]] %in% text_somewhat_disagree) - 2 * (e[[v]] %in% text_strongly_disagree) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]]))
    e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                               names = c("Strongly disagree","Somewhat disagree","Neither agree or disagree","Somewhat agree","Strongly agree","PNR")),
                      missing.values=-0.1, annotation=Label(e[[v]]))
  }
  
  e$owner <- e$home_owner == T | e$home_landlord == T
  label(e$owner) <- "owner: Owner or Landlord renting out property to: Are you a homeowner or a tenant?"
  
  for (v in c(variables_policy , variables_tax, variables_support, "insulation_support", "global_quota", variables_gas_spike, variables_fine_support)) {
    if (v %in% names(e)) {
      temp <-  2 * (e[[v]] %in% text_support_strongly) + (e[[v]] %in% text_support_somewhat) - (e[[v]] %in% text_support_not_really) - 2 * (e[[v]] %in% text_support_not_at_all) - 0.1 * (e[[v]] %in% text_pnr) # | is.na(e[[v]]))
      temp[is.na(e[[v]])] <- NA
      e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                 names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support","PNR")),
                        missing.values=c(-0.1,NA), annotation=Label(e[[v]])) 
    } }
  
  if ("interest_politics" %in% names(e)) temp <-  (e$interest_politics %in% text_interest_politics_lot) - (e$interest_politics %in% text_interest_politics_no) - 0.1 * (e$interest_politics %in% text_pnr)
  if ("interest_politics" %in% names(e)) e$interest_politics <- as.item(temp, labels = structure(c(-1:1,-0.1),
                                                                                                 names = c("Not really or not at all","A little", "A lot","PNR")),
                                                                        missing.values=-0.1, annotation=Label(e$interest_politics))
  
  if ("vote_participation" %in% names(e)) {
    e$vote_participation[grepl("right to vote", e$vote_participation)] <- "No right to vote"
    if ("vote_voters_2016" %in% names(e)) {
      e$vote_participation_2016[e$vote_participation_2016 %in% text_vote_participation_no_right] <- "No right to vote"
      e$vote_2016[!is.na(e$vote_voters_2016) & e$vote_participation_2016=="Yes"] <- e$vote_voters_2016[!is.na(e$vote_voters_2016) & e$vote_participation_2016=="Yes"]
      e$vote_2016[!is.na(e$vote_non_voters_2016) & e$vote_participation_2016!="Yes"] <- e$vote_non_voters_2016[!is.na(e$vote_non_voters_2016) & e$vote_participation_2016!="Yes"]
      e$vote_participation_2016 <- as.item(as.character(e$vote_participation_2016), missing.values = 'PNR', annotation=Label(e$vote_participation_2016))
      e$vote_2016 <- as.item(as.character(e$vote_2016), missing.values = 'PNR', annotation=Label(e$vote_2016))
      e$vote_2016_factor <- as.factor(e$vote_2016)
      e$vote_2016_factor <- relevel(relevel(e$vote_2016_factor, "Stein"), "Clinton")
    }
    e$vote[!is.na(e$vote_voters) & e$vote_participation=="Yes"] <- e$vote_voters[!is.na(e$vote_voters) & e$vote_participation=="Yes"]
    e$vote[!is.na(e$vote_non_voters) & e$vote_participation!="Yes"] <- e$vote_non_voters[!is.na(e$vote_non_voters) & e$vote_participation!="Yes"]
    e$vote_participation <- as.item(as.character(e$vote_participation), missing.values = 'PNR', annotation=Label(e$vote_participation))
    e$vote <- as.item(as.character(e$vote), missing.values = 'PNR', annotation=Label(e$vote))
    e$voted <- e$vote_participation == 'Yes'
    label(e$voted) <- "voted: Has voted in last election: Yes to vote_participation."
  }
  
  e$survey_biased[e$survey_biased %in% text_survey_biased_pro_envi] <- "Yes, pro environment"
  e$survey_biased[e$survey_biased %in% text_survey_biased_anti_envi] <- "Yes, anti environment"
  e$survey_biased[e$survey_biased %in% text_survey_biased_left] <- "Yes, left"
  e$survey_biased[e$survey_biased %in% text_survey_biased_right] <- "Yes, right"
  e$survey_biased[e$survey_biased %in% text_survey_biased_no] <- "No" 
  if ("Yes, right" %in% levels(as.factor(e$survey_biased))) e$survey_biased <- relevel(relevel(as.factor(e$survey_biased), "Yes, right"), "No")
  e$survey_biased_yes <- e$survey_biased != 'No'
  e$survey_biased_left <- e$survey_biased == "Yes, left"
  e$survey_biased_right <- e$survey_biased == "Yes, right"
  label(e$survey_biased_yes) <- "survey_biased_yes: T/F Finds the survey biased (survey_biased != No)"
  label(e$survey_biased_left) <- "survey_biased_left: T/F Finds the survey left-wing biased (survey_biased == Yes, left)"
  label(e$survey_biased_right) <- "survey_biased_right: T/F Finds the survey right-wing biased (survey_biased == Yes, right)"
  
  print("all_policies undefined")
  all_policies <- c(variables_policies_support, "standard_public_transport_support", "tax_transfers_progressive_support", variables_fine_support, variables_policy, variables_tax, "global_quota", variables_global_policies, "insulation_support", variables_beef, variables_policy_additional) # include also should_fight_CC, burden_share, if_other_do_less/more, variables_flight_quota ?
  
  e$share_policies_supported <- rowMeans(e[, intersect(all_policies, names(e))] > 0, na.rm = T)
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
  
  print(paste("convert: success", country))
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
  if (only_finished | incl_quality_fail) { # TODO: le faire marcher même pour les autres
    e <- convert(e, country = country, wave = wave, weighting = weighting, zscores = zscores, zscores_dummies = zscores_dummies, efa = efa, combine_age_50 = combine_age_50)
    e <- e[,!duplicated(names(e))]
    # if (!incl_quality_fail) e <- e[e$attention_test == T, ] # TODO!
    # if (weighting) {
    #   e$weight <- weighting(e)
    #   if ("vote_2020" %in% names(e) & (sum(e$vote_2020=="PNR/no right")!=0)) e$weight_vote <- weighting(e, vote = T)  }
    
    # e$left_right_na <- as.numeric(e$left_right)
    # e$left_right_na[e$indeterminate == T] <- wtd.mean(e$left_right, weights = e$weight)
  } else e <- create_education(e, country, only = TRUE)
  
  
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

us1p <- prepare(country = "US1", wave = "pilot", weighting = FALSE)
eup <- prepare(country = "EU", wave = "pilot", weighting = FALSE)

