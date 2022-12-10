##### Total income quantiles #####
# figures from https://www.lustat.ch/files_ftp/daten/gd/1061/w185_043t_gd1061_ss_d_2020.html
quantiles <- list()
quantiles[["married_without_kind"]] <- c(33049, 53477, 77557, 111252, 168188, 223026)
quantiles[["married_with_kind"]] <- c(48814, 69477, 94410, 132282, 187637, 244330)
quantiles[["single_without_kind"]] <- c(2413, 20136, 41700, 60441, 84436, 107235)
quantiles[["single_with_kind"]] <- c(9074, 31121, 51009, 73845, 107283, 136585)

types <- c("married_without_kind", "married_with_kind", "single_without_kind", "single_with_kind")
nb_adults <- c(7850*2, 5345*2, 35586, 3294)
names(nb_adults) <- types
simulated_incomes <- c()
simulated_incomes_by_type <- list()
for (t in types) {
  names(quantiles[[t]]) <- c(10, 25, 50, 75, 90, 95)
  simulated_incomes_by_type[[t]] <- c(rep(quantiles[[t]]["10"], floor(0.15*nb_adults[t])), rep(quantiles[[t]]["25"], floor(0.23*nb_adults[t])), rep(quantiles[[t]]["50"], floor(0.24*nb_adults[t])), rep(quantiles[[t]]["75"], floor(0.23*nb_adults[t])), rep(quantiles[[t]]["90"], floor(0.08*nb_adults[t])), rep(quantiles[[t]]["95"], floor(0.07*nb_adults[t])))
  simulated_incomes <- c(simulated_incomes, simulated_incomes_by_type[[t]])
}
quantile(simulated_incomes, c(10, 25, 50, 75, 90, 95)/100)
decrit(simulated_incomes)
plot(Ecdf(simulated_incomes), type='s') + grid()


##### Individual income quantiles #####
quantiles_indiv <- quantiles
quantiles_indiv[["married_with_kind"]] <- quantiles[["married_with_kind"]]/2
quantiles_indiv[["married_without_kind"]] <- quantiles[["married_without_kind"]]/2
simulated_indiv_incomes <- c()
simulated_indiv_incomes_by_type <- list()
for (t in types) {
  names(quantiles_indiv[[t]]) <- c(10, 25, 50, 75, 90, 95)
  simulated_indiv_incomes_by_type[[t]] <- c(rep(quantiles_indiv[[t]]["10"], floor(0.15*nb_adults[t])), rep(quantiles_indiv[[t]]["25"], floor(0.23*nb_adults[t])), rep(quantiles_indiv[[t]]["50"], floor(0.24*nb_adults[t])), rep(quantiles_indiv[[t]]["75"], floor(0.23*nb_adults[t])), rep(quantiles_indiv[[t]]["90"], floor(0.08*nb_adults[t])), rep(quantiles_indiv[[t]]["95"], floor(0.07*nb_adults[t])))
  simulated_indiv_incomes <- c(simulated_indiv_incomes, simulated_indiv_incomes_by_type[[t]])
}
decrit(simulated_indiv_incomes)
plot(Ecdf(simulated_indiv_incomes), type='s') + grid()


##### Individual wealth quantiles #####
# figures from https://www.lustat.ch/files_ftp/daten/gd/1061/w185_023t_gd1061_ss_d_2020.html
quantiles2 <- list()
quantiles2[["married"]] <- c(0, 17795, 100000, 100000, 120000, 125000)
quantiles2[["single"]] <- c(0, 3969, 32450, 50000, 50000, 50000)
types2 <- c("married", "single")
nb_adults2 <- c(13068*2, 39007)
names(nb_adults2) <- types2
simulated_wealths <- c()
simulated_wealths_by_type <- list()
for (t in types2) {
  names(quantiles2[[t]]) <- c(10, 25, 50, 75, 90, 95)
  simulated_wealths_by_type[[t]] <- c(rep(quantiles2[[t]]["10"], floor(0.15*nb_adults2[t])), rep(quantiles2[[t]]["25"], floor(0.23*nb_adults2[t])), rep(quantiles2[[t]]["50"], floor(0.24*nb_adults2[t])), rep(quantiles2[[t]]["75"], floor(0.23*nb_adults2[t])), rep(quantiles2[[t]]["90"], floor(0.08*nb_adults2[t])), rep(quantiles2[[t]]["95"], floor(0.07*nb_adults2[t])))
  simulated_wealths <- c(simulated_wealths, simulated_wealths_by_type[[t]])
}
decrit(simulated_wealths)
plot(Ecdf(simulated_wealths), type='s') + grid()


