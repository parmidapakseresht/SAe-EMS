# Chargement des données
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep=";")

# Filtrer les données
filtre <- data$X96..DATE_ENREG <= "03/12/2025 15:30:00"
data_filtre <- subset(data, filtre)

# Charger les packages
library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# ANALYSE DES USAGES, RAISONS ET LIMITES
# =============================================================================

# Fonction simple pour compter les modalités
compter_modalites <- function(vecteur, sep = ";") {
  # Créer un vecteur vide pour stocker tous les éléments
  tous_elements <- c()
  
  # Boucler sur chaque réponse
  for (i in 1:length(vecteur)) {
    if (!is.na(vecteur[i]) & vecteur[i] != "") {
      # Séparer les éléments par le séparateur
      elements <- strsplit(vecteur[i], sep)[[1]]
      # Enlever les espaces
      elements <- trimws(elements)
      # Ajouter au vecteur global
      tous_elements <- c(tous_elements, elements)
    }
  }
  
  # Compter les occurrences
  table_resultats <- table(tous_elements)
  
  # Créer un dataframe avec les résultats
  resultat <- data.frame(
    modalite = names(table_resultats),
    effectif = as.numeric(table_resultats)
  )
  
  return(resultat)
}

# Fonction pour calculer la proportion et l'IC
calculer_stats <- function(effectifs, n_repondants) {
  # Calculer la proportion
  p <- effectifs / n_repondants
  
  # Calculer la variance
  var_p <- p * (1 - p) / n_repondants
  
  # Calculer l'écart-type
  et_p <- sqrt(var_p)
  
  # Intervalle de confiance à 95%
  z <- 1.96
  ic_inf <- p - z * et_p
  ic_sup <- p + z * et_p
  
  # S'assurer que l'IC est entre 0 et 1
  ic_inf <- pmax(0, ic_inf)
  ic_sup <- pmin(1, ic_sup)
  
  # Créer un dataframe avec les résultats
  resultat <- data.frame(
    proportion = p,
    pourcentage = p * 100,
    variance = var_p,
    ecart_type = et_p,
    IC_inf = ic_inf,
    IC_sup = ic_sup
  )
  
  return(resultat)
}

# Extraire les colonnes d'intérêt
usages <- data_filtre$X45..RP_PN_IA_usages
raisons <- data_filtre$X47..RP_PN_IA_raisons
limites <- data_filtre$X53..RP_PN_IALimites

# Nombre de répondants
n <- nrow(data_filtre)

# ===== USAGES =====
print("===== USAGES =====")
usages_count <- compter_modalites(usages)
usages_count <- usages_count[order(-usages_count$effectif), ]
usages_stats <- calculer_stats(usages_count$effectif, n)
usages_results <- cbind(usages_count, usages_stats)
print(usages_results)

# ===== RAISONS =====
print("===== RAISONS =====")
raisons_count <- compter_modalites(raisons)
raisons_count <- raisons_count[order(-raisons_count$effectif), ]
raisons_stats <- calculer_stats(raisons_count$effectif, n)
raisons_results <- cbind(raisons_count, raisons_stats)
print(raisons_results)

# ===== LIMITES =====
print("===== LIMITES =====")
limites_count <- compter_modalites(limites)
limites_count <- limites_count[order(-limites_count$effectif), ]
limites_stats <- calculer_stats(limites_count$effectif, n)
limites_results <- cbind(limites_count, limites_stats)
print(limites_results)

# Sauvegarder les résultats
estim_prop_usages <- usages_results
estim_prop_raisons <- raisons_results
estim_prop_limites <- limites_results 