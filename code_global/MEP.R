mep_list <- read.xlsx("../data/MEPs.xlsx", sheet = 1) # /!\ The group/party is not always accurate as some MEPs have changed group/party

table(mep_list$country_name)
