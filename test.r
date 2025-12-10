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
# ANALYSE DES FREQUENCES D'UTILISATION DES IA GENERATIVES
# =============================================================================

freq_util_chatgpt <- data_filtre$X36..ChatGPT
freq_util_deepl <- data_filtre$X37..DeepL
freq_util_copilot <- data_filtre$X38..Copilot
freq_util_grammarly <- data_filtre$X39..Grammarly
freq_util_perplexity <- data_filtre$X40..Perplexity

# =============================================================================
# ANALYSE DES USAGES, RAISONS ET LIMITES
# =============================================================================

# Fonction simple pour compter les modalités (y compris les non-réponses)
compter_modalites <- function(vecteur, sep = ";") {
  # Créer un vecteur vide pour stocker tous les éléments
  tous_elements <- c()
  n_non_repondants <- 0
  
  # Boucler sur chaque réponse
  for (i in 1:length(vecteur)) {
    if (is.na(vecteur[i]) | vecteur[i] == "") {
      # Compter les non-réponses
      n_non_repondants <- n_non_repondants + 1
    } else {
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
  
  # Ajouter une ligne pour les non-répondants
  if (n_non_repondants > 0) {
    resultat <- rbind(resultat, data.frame(
      modalite = "Non-réponse",
      effectif = n_non_repondants
    ))
  }
  
  return(resultat)
}

# Fonction pour calculer la proportion et l'IC
calculer_stats <- function(effectifs, n_repondants, N_population) {
  # Calculer la proportion
  p <- effectifs / n_repondants
  
  # Taux de sondage
  f <- n_repondants / N_population  # Taux de sondage
  
  # Variance avec correction pour population finie
  # Var(p) = p(1-p)/n * (1-f)
  var_p <- (p * (1 - p) / n_repondants) * (1 - f)
  
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

# Fonction pour afficher les résultats de manière claire
afficher_resultats <- function(resultats, titre) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(titre, "\n")
  cat(strrep("=", 80), "\n\n")
  
  for (i in 1:nrow(resultats)) {
    cat("Modalité :", resultats$modalite[i], "\n")
    cat("  Effectif                     :", resultats$effectif[i], "\n")
    cat("  Estimateur de proportion (p) :", round(resultats$proportion[i], 4), 
        "(", round(resultats$pourcentage[i], 2), "% )\n")
    cat("  Variance de l'estimateur     :", round(resultats$variance[i], 6), "\n")
    cat("  Écart-type de l'estimateur   :", round(resultats$ecart_type[i], 4), "\n")
    cat("  Intervalle de confiance 95%  : [", 
        round(resultats$IC_inf[i], 4), " ; ", 
        round(resultats$IC_sup[i], 4), "]\n")
    cat("                                 [", 
        round(resultats$IC_inf[i]*100, 2), "% ; ", 
        round(resultats$IC_sup[i]*100, 2), "%]\n")
    cat("\n")
  }
}

N_population <- 5354

# ===== USAGES =====
print("===== USAGES =====")
usages_count <- compter_modalites(usages)
usages_count <- usages_count[order(-usages_count$effectif), ]
usages_stats <- calculer_stats(usages_count$effectif, n, N_population)
usages_results <- cbind(usages_count, usages_stats)
afficher_resultats(usages_results, "ANALYSE DES USAGES")

# ===== RAISONS =====
print("===== RAISONS =====")
raisons_count <- compter_modalites(raisons)
raisons_count <- raisons_count[order(-raisons_count$effectif), ]
raisons_stats <- calculer_stats(raisons_count$effectif, n, N_population)
raisons_results <- cbind(raisons_count, raisons_stats)
afficher_resultats(raisons_results, "ANALYSE DES RAISONS")

# ===== LIMITES =====
print("===== LIMITES =====")
limites_count <- compter_modalites(limites)
limites_count <- limites_count[order(-limites_count$effectif), ]
limites_stats <- calculer_stats(limites_count$effectif, n, N_population)
limites_results <- cbind(limites_count, limites_stats)
afficher_resultats(limites_results, "ANALYSE DES LIMITES")

# Sauvegarder les résultats
estim_prop_usages <- usages_results
estim_prop_raisons <- raisons_results
estim_prop_limites <- limites_results

# ==========================
# FRÉQUENCES D'UTILISATION
# ==========================

# Variables de fréquence (colonnes individuelles)
freq_vars <- list(
  ChatGPT = data_filtre$X36..ChatGPT,
  DeepL = data_filtre$X37..DeepL,
  Copilot = data_filtre$X38..Copilot,
  Grammarly = data_filtre$X39..Grammarly,
  Perplexity = data_filtre$X40..Perplexity,
  Autre = data_filtre$X41..Autre
)

freq_results_list <- list()
cat('\n', strrep('=', 80), '\n')
cat('ANALYSE DES FRÉQUENCES D\'UTILISATION (VARIABLES INDIVIDUELLES)', '\n')
cat(strrep('=', 80), '\n')

for (name in names(freq_vars)) {
  vec <- freq_vars[[name]]
  # Compter toutes les modalités y compris les NA
  tab <- table(factor(vec, exclude = NULL), useNA = 'ifany')
  modalites <- names(tab)
  eff <- as.numeric(tab)
  df <- data.frame(modalite = modalites, effectif = eff, stringsAsFactors = FALSE)
  # Remplacer la modalité NA par "Non-réponse"
  df$modalite[is.na(df$modalite)] <- 'Non-réponse'

  # Calculer stats (proportion, variance corrigée, IC)
  stats <- calculer_stats(df$effectif, n, N_population)
  df2 <- cbind(df, stats)

  cat('\n-- Variable :', name, '--\n')
  afficher_resultats(df2, paste('Fréquences :', name))

  freq_results_list[[name]] <- df2
}

# Sauvegarder les résultats des fréquences
estim_freqs <- freq_results_list