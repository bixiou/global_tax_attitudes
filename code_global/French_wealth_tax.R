##### Revenue from a wealth tax in FR #####
# There is no data on wealth distribution in WID website
# Data from WID (bajard.felix@laposte.net), in current USD. TODO: update data once it's online and stabilized.
wealth <- read.csv("../data/wealth_tax_wid.csv") # /!\ wealth_above_threshold is total, not marginal wealth. E.g. someone with 150M will have wealth_above_threshold = 150M, not 50M, for threshold = 100M.
names(wealth) <- c("n", "iso2", "year", "threshold", "gdp", "national_wealth", "wealth_above_threshold", "headcount_above_threshold", "threshold_constant_2023", "headcount_at_bracket", "wealth_at_bracket")
wealth$code <- iso2to3[wealth$iso2]
# Remplacer DMTO et taxe foncière actuelle par taxe foncière sur la valeur vénale nette d'emprunt (au taux de 0.6% s'il est flat): France Stratégie 2016
usd22_eur26 <- .96
usd23_eur24 <- .95
# mean(wfr$national_wealth)*usd22_eur26/50e6 # Average wealth per adult: 276k. national_wealth = 1.42357e13. median: 124k https://www.insee.fr/fr/statistiques/2388851
# 43.5G€ from taxe foncière on households (26.1) + DMTO (14.7) + IFI (2.7) in 2024 https://www.vie-publique.fr/en-bref/301204-faut-il-reformer-la-fiscalite-du-patrimoine
# Patrimoine immobilier net des français ~ 7000G https://www.insee.fr/fr/statistiques/7941417?sommaire=7941491#tableau-figure2
# Patrimoine immobilier net moyen: 140k; taux d'imposition moyen: 0.6%. Patrimoine brut moyen: 200k (André & Meslin 25). Taxe foncière en % du brut: 0.34%. (note that they say there is 35G revenue from taxe foncière)
wfr <- wealth[wealth$iso2 == "FR" & wealth$year == 2022, c("threshold", "wealth_above_threshold", "headcount_above_threshold", "threshold_constant_2023")]
wfr$wealth_above_threshold <- wfr$wealth_above_threshold - wfr$headcount_above_threshold * wfr$threshold_constant_2023

wealth_tax_revenue <- function(df = wfr, thresholds = c(2e5,     5e5,   1e6,  2e6,   5e6,  1e7, 1e8), 
                               tax_rates = c(0.001, 0.002, 0.005, 0.01, 0.015, 0.02, 0.025)) {
  match_idx <- vapply(thresholds, function(t) {
    rel_diff <- abs(df$threshold - t) / pmax(abs(t), 1)
    best <- which.min(rel_diff)
    if (rel_diff[best] > 1e-6) stop(sprintf("Threshold %.6g not found in df$threshold. ",t )) 
    best}, integer(1))
  return(round(usd23_eur24*sum(diff(c(0, tax_rates)) * df$wealth_above_threshold[match_idx])/1e9, 1))
}
wealth_tax_revenue(thresholds = c(2e5,     5e5,   1e6,  2e6,   5e6,  1e7, 1e8), 
                   tax_rates = c(0.001, 0.002, 0.005, 0.01, 0.015, 0.02, 0.025)) # 95G (i.e. +50G)
wealth_tax_revenue(thresholds = c(2e5,     5e5,   1e6,  2e6), 
                   tax_rates = c(0.001, 0.002, 0.005, 0.01)) # 63G (i.e. +20G)
wealth_tax_revenue(thresholds = c(2e5,   1e6), 
                   tax_rates = c(0.003, 0.01)) # 78G (i.e. +34G)
wealth_tax_revenue(thresholds = c(2e5,   1e6,   1e7, 1e8), 
                   tax_rates = c(0.003, 0.01, 0.015, 0.02)) # 95G (i.e. +50G)
wealth_tax_revenue(thresholds = c(2e5,     5e5,   1e6,  2e6,   5e6,  1e7), 
                   tax_rates = c(0.0005, 0.001, 0.003, .008, 0.012, .017)) # 71G (i.e. +27G)
wealth_tax_revenue(thresholds = c(1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9), 
                   tax_rates = effective_to_marginal(thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9))[-1]) # 1490G One implementation of Piketty's schedule (in %): 0 .3 1.4 3.7 10 11 85 96
wealth_tax_revenue(thresholds = c(1e5, 2e5, 1e6, 2e6, 5e6, 5e7, 5e8), 
                   tax_rates = effective_to_marginal(thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 5e6, 5e7, 5e8))[-1]) # 1560G Another implementation of Piketty's schedule: 0 .3 1.4 3.7 10 11 70 95
wealth_tax_revenue(thresholds = c(2e5,    5e5,  1e6,   2e6,  5e6, 2e7, 2e8), # 0% below average wealth (taken as 200k), 0.1% up to 2× average, 0.5% up to 5×, 1% up to 10×, 2% up to 20×, 5% up to 100×, 10% up to 1000×, and 90% above that
                   tax_rates = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.9)) # 903G 
# With this schedule, wealth converges at 20M (4.5% return), 47M (7.5%), 200M (10%), 300M (35%) in less than 5 years for multi-billionaires, up to 30 years for self-made people (starting at 10k)
wealth_tax_revenue(thresholds = c(2e5,    1e6,  2e6,   5e6,  5e7, 5e8, 5e9), 
                   tax_rates = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.9)) # 464G
# Piketty: Multiple of average net wealth (300k€ in FR): .5 2 5 10 100 1k 10k, i.e. 150k 600k 1.5M 3M 30M 300M 3G
#          Effect tax rate on net wealth (in %):         .1 1 2  5  10 60 90
write.csv(wfr, "wfr.csv")
View(wfr)


##### Wealth tax given schedule #####
thresholds <-      c(2e5,  5e5,  1e6,  2e6,  5e6,  2e7, 2e8)
tax_rates  <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.9)

wealth_tax <- function(W, thresholds = c(2e5,  5e5,  1e6,  2e6,  5e6,  2e7, 2e8), tax_rates = c(0, 0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.9), rate = FALSE) {
  tax <- 0
  lower <- 0
  for (i in seq_along(thresholds)) {
    upper <- thresholds[i]
    taxable <- min(W, upper) - lower
    if (taxable > 0) tax <- tax + taxable * c(0, tax_rates)[i]
    lower <- upper
  }
  if (W > thresholds[length(thresholds)]) {
    tax <- tax + (W - thresholds[length(thresholds)]) *
      tax_rates[length(tax_rates)]
  }
  if (rate) return(tax/W)
  else return(tax)
}

wealth_tax(1e6, thresholds = c(2e5,     5e5,   1e6,  2e6), 
           tax_rates = c(0.002, 0.004, 0.01, 0.01))/1e4


##### Stationary wealth #####
stationary_wealth <- function(r, thresholds = c(2e5,  5e5,  1e6,  2e6,  5e6,  2e7, 2e8), tax_rates = c(0, 0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.9)) { # around 200M whatever the rate
# stationary_wealth <- function(r, thresholds = c(1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9), tax_rates = effective_to_marginal(thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9))[-1]) { # Piketty: 5/7.5/10%: 3/6/30M
  f <- function(W) r*W - wealth_tax(W, thresholds = thresholds, tax_rates = tax_rates)
  uniroot(f, interval = c(1e3, 1e15))$root
}

rates_test <- c(0.04, 0.05, 0.068, 0.075, 0.10, 0.15, 0.20, 0.35)
stationary_levels <- sapply(rates_test, stationary_wealth)
data.frame(r = rates_test, stationary_wealth = stationary_levels/1e6)


##### Evolution of wealth following tax schedule #####
simulate <- function(W0, r, years = 80) {
  W <- numeric(years + 1)
  W[1] <- W0
  for (t in 1:years) {
    W[t+1] <- W[t] * (1 + r) - wealth_tax(W[t])
  }
  W
}
years <- 80
time  <- 0:years
# Individus
W1 <- simulate(1e4,   0.30, years)
# W1 <- simulate(1e4,   0.10, years)
W2 <- simulate(1e12,  0.10, years)
W3 <- simulate(5e7,   0.075, years)
plot(time, W1, type="l", log="y", lwd=2,ylim = range(c(W1,W2,W3)),col="blue",xlab="Années",ylab="Patrimoine (log)")
lines(time, W2, col="red", lwd=2)
lines(time, W3, col="darkgreen", lwd=2)

legend("topright",legend=c("Individu 1 (1M, r=30%)","Individu 2 (1000 Md, r=10%)","Individu 3 (50M, r=7.5%)"),col=c("blue","red","darkgreen"), lwd=2)


##### Effective to marginal rates #####
effective_to_marginal <- function(effective_rates = c(0, .1, 1, 2, 5, 10, 60, 90), at_values = c(1e4, 150e3, 600e3, 1.5e6, 3e6, 3e7, 3e8, 3e9), thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9)) {
  stopifnot(length(effective_rates) == length(at_values))
  stopifnot(all(effective_rates >= 0) && all(effective_rates <= 100))
  stopifnot(all(diff(thresholds) > 0), thresholds[1] == 0)
  stopifnot(all(at_values > 0))
  e <- effective_rates / 100
  y <- at_values
  n_brackets <- length(thresholds) 
  t <- c(thresholds, max(at_values)+1e9)
  bracket_of <- findInterval(y, t, rightmost.closed = FALSE)
  if (any(bracket_of < 1 | bracket_of > n_brackets)) stop("Some at_values fall outside the range defined by thresholds.")
  bracket_counts <- tabulate(bracket_of, nbins = n_brackets)
  if (any(bracket_counts[unique(bracket_of)] != 1)) stop("Each bracket must contain exactly one at_value.")
  ord <- order(y)
  y   <- y[ord]
  e   <- e[ord]
  bracket_of <- bracket_of[ord]
  tax_at_threshold <- numeric(length(t))  # initialized to 0
  marginal_rates <- numeric(n_brackets)
  for (i in seq_along(y)) {
    k  <- bracket_of[i]          # bracket index (1-indexed)
    lo <- t[k]                   # lower threshold of this bracket
    T_lo <- tax_at_threshold[k]
    T_yi <- e[i] * y[i]
    if (y[i] <= lo) stop(sprintf("at_value %g is <= its bracket's lower threshold %g", y[i], lo))
    m_k <- (T_yi - T_lo) / (y[i] - lo)
    if (m_k < 0 || m_k > 1) warning(sprintf("Marginal rate in bracket %d (covering [%g, %g)) is %.4f (%.2f%%) — outside [0,1].",  k, t[k], t[k + 1], m_k, m_k * 100))
    marginal_rates[k] <- m_k
    if (k < length(t)) {
      tax_at_threshold[k + 1] <- T_lo + m_k * (t[k + 1] - lo)
    }
  }
  
  missing_brackets <- which(bracket_counts == 0)
  if (length(missing_brackets) > 0) warning(sprintf("No at_value provided for bracket(s) %s — marginal rate set to 0.",paste(missing_brackets, collapse = ", ")))
  return(marginal_rates)
}
effective_to_marginal(thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9)) # One implementation of Piketty's Capital & Ideology rates
effective_to_marginal(thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 5e6, 5e7, 5e8)) # Another implementation of Piketty's Capital & Ideology rates
sapply(c(1e4, 1.5e5, 6e5, 1.5e6, 3e6, 3e7, 3e8, 3e9), function(w) wealth_tax(w, thresholds = c(0, 1e5, 2e5, 1e6, 2e6, 1e7, 1e8, 1e9), tax_rates = effective_to_marginal(), rate = T))


##### INSEE Data 2020-21 #####
df_menage <- read.csv2("C:/Users/fabre/Documents/www/Données/Patrimoine_2020_2021/MENAGE.csv")
df_produit <- read.csv2("C:/Users/fabre/Documents/www/Données/Patrimoine_2020_2021/PRODUIT.csv")
# Individualiser le patrimoine
# Pour chaque tranche de patrimoine net, calculer les agrégats de patrimoine net ainsi que patrimoine immo net des dettes.
# Prendre un barème; calculer pour chaque tranche la taxe payée et la part due à l'immo net de dettes (vs. le reste); vérifier que le total de l'impôt immo est > 44G (valeur actulle TF+DMTO+IFI)

library(dplyr)
library(tidyr)

# --- 1. Préparation et Individualisation ---
df_menage <- df_menage %>%  mutate(PATPRO = coalesce(PATPROFHENT, 0) + coalesce(PATPROFENT, 0))

# Ratios de détention (Table PRODUIT)
parts_couple <- df_produit %>%
  group_by(IDENT) %>%
  summarise(
    v_PR = sum(MONTCLA * (PARPR / 100), na.rm = TRUE),
    v_CJ = sum(MONTCLA * (PARCJ / 100), na.rm = TRUE), .groups = "drop"
  ) %>%
  mutate(total = v_PR + v_CJ,
         r_PR = ifelse(total > 0, v_PR / total, 0.5),
         r_CJ = ifelse(total > 0, v_CJ / total, 0.5))

# Création de la table Individus
df_indiv <- df_menage %>%
  left_join(parts_couple, by = "IDENT") %>%
  mutate(rat_PR = ifelse(COUPLEPR == 1, coalesce(r_PR, 0.5), 1),
         rat_CJ = ifelse(COUPLEPR == 1, coalesce(r_CJ, 0.5), 0),
         pat_immo_net_m = pmax(0, PATIMM - MTDETPRIV)) %>%
  {
    bind_rows(
      dplyr::select(., IDENT, POND_TRANS, ratio = rat_PR, PATRI_NET, PATRI_BRUT, pat_immo_net_m, PATIMM, PATPRO, PATFISOM) %>% mutate(type="PR"),
      filter(., COUPLEPR == 1) %>% dplyr::select(IDENT, POND_TRANS, ratio = rat_CJ, PATRI_NET, PATRI_BRUT, pat_immo_net_m, PATIMM, PATPRO, PATFISOM) %>% mutate(type="CJ")
    )
  } %>%
  mutate(across(c(PATRI_NET, PATRI_BRUT, pat_immo_net_m, PATIMM, PATPRO, PATFISOM), ~ . * ratio)) %>%
  # AJOUT : Part de l'immobilier net dans le patrimoine net individuel
  mutate(share_immo_indiv = ifelse(PATRI_NET > 0, pat_immo_net_m / PATRI_NET, 0))

# --- 2. Construction de la table 'patr' avec surplus ---
thresholds_final <- c(-Inf, 0, 2.5e4, 5e4, 7.5e4, 1e5, 1.5e5, 2e5, 2.5e5, 3e5, 4e5, 5e5, 
                      6e5, 7e5, 8e5, 9e5, 1e6, 2e6, 3e6, 5e6, 1e7, 3e7, 5e7, 
                      1e8, 3e8, 5e8, 1e9, 3e9, 5e9, Inf)

patr <- do.call(rbind, lapply(thresholds_final, function(t) {
  df_indiv %>%
    filter(PATRI_NET > t) %>%
    summarise(
      threshold      = t,
      headcount      = sum(POND_TRANS, na.rm = TRUE),
      patr_net       = sum((PATRI_NET - t) * POND_TRANS, na.rm = TRUE),
      patr_brut      = sum(pmax(0, PATRI_BRUT - t) * POND_TRANS, na.rm = TRUE),
      patr_immo_net  = sum(pmax(0, pat_immo_net_m - t) * POND_TRANS, na.rm = TRUE),
      patr_immo_brut = sum(pmax(0, PATIMM - t) * POND_TRANS, na.rm = TRUE),
      patr_pro       = sum(pmax(0, PATPRO - t) * POND_TRANS, na.rm = TRUE),
      patr_fin       = sum(pmax(0, PATFISOM - t) * POND_TRANS, na.rm = TRUE),
      # AJOUT : Part immo moyenne pondérée au-delà du seuil
      share_immo     = sum(share_immo_indiv * (PATRI_NET - t) * POND_TRANS, na.rm = TRUE) / 
        sum((PATRI_NET - t) * POND_TRANS, na.rm = TRUE)
    )
}))

# --- 3. Fonction d'estimation fiscale ---

estimate_tax_revenue <- function(tax_thresholds, tax_rates, data_patr) {
  # On s'assure que les seuils du barème existent dans la table patr
  # Sinon, on interpole ou on utilise les seuils les plus proches
  
  total_revenue <- 0
  immo_revenue <- 0
  
  for (i in 1:length(tax_thresholds)) {
    curr_threshold <- tax_thresholds[i]
    curr_rate <- tax_rates[i]
    
    # Extraire la ligne correspondant au seuil dans patr
    row <- data_patr %>% filter(threshold == curr_threshold)
    
    if (nrow(row) == 1) {
      # La base taxable pour ce taux marginal est précisément patr_net à ce seuil
      # (car patr_net est déjà calculé comme le surplus au-dessus du seuil)
      
      # Si c'est un barème progressif classique, on doit calculer par tranche
      # Mais ici, avec la structure 'surplus', la recette d'un taux marginal i 
      # s'applique sur tout le patrimoine net au-dessus du seuil i.
      # Pour obtenir le rendement par tranche, on multiplie le surplus par la différence de taux.
      
      rate_increment <- ifelse(i == 1, tax_rates[i], tax_rates[i] - tax_rates[i-1])
      
      segment_revenue <- row$patr_net * rate_increment
      segment_immo    <- segment_revenue * row$share_immo
      
      total_revenue <- total_revenue + segment_revenue
      immo_revenue <- immo_revenue + segment_immo
    } else {
      warning(paste("Le seuil", curr_threshold, "n'est pas présent dans la table patr."))
    }
  }
  
  return(c(
    total_tax_revenue = total_revenue,
    immo_tax_revenue = immo_revenue,
    immo_share_percent = (immo_revenue / total_revenue) * 100
  ))
}

(result <- round(estimate_tax_revenue(c(1e5, 2e5, 1e6), c(0.003, 0.014, 0.037), patr)/c(1e9, 1e9, 1), 1))
(result <- round(estimate_tax_revenue(c(1e5, 2e5, 1e6), c(0.003, 0.01, 0.015), patr)/c(1e9, 1e9, 1), 1))
(result <- round(estimate_tax_revenue(c(1e5, 2e5, 5e5), c(0.005, 0.01, 0.02), patr)/c(1e9, 1e9, 1), 1))
View(patr) # aggregate value is 80-90% of National Accounts for real estate and ~50% for financial assets (prudent estimates, missing rich in the sample)
