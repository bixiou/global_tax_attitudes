mep_list <- read.xlsx("../data/MEPs.xlsx", sheet = 1) # /!\ The group/party is not always accurate as some MEPs have changed group/party

table(mep_list$country_name)
paste(sort(mep_list$email), collapse = ", ")
# 8-10/02 emails sent to MEPs, ordered alphabetically by firstname, from Thomas' address, 50 per email, in Bcc.