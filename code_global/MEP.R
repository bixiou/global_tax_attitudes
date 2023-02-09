mep_list <- read.xlsx("../data/MEPs.xlsx", sheet = 1) # /!\ The group/party is not always accurate as some MEPs have changed group/party

table(mep_list$country_name)
paste(sort(mep_list$email), collapse = ", ")
# 8-10/02 emails sent to MEPs, ordered alphabetically by firstname, from Thomas' address, 50 per email, in Bcc.

decrit("gcs_support", mep, weight = F)
decrit("belief_eu", mep, weight = F) # TODO prepare belief_eu/us_agg
decrit("belief_confidence", mep, weight = F) # TODO prepare
decrit("belief_us", mep, weight = F)
decrit("belief_vote", mep, weight = F) # TODO prepare
decrit("position", mep, weight = F)
decrit("country", mep, weight = F)
decrit("group", mep, weight = F)
mep$comment_field[mep$comment_field != ""]

View(mep[,18:27])
