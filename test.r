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
# VERSION TRÈS SIMPLE — pour débutant
#  - compte les modalités (y compris non-réponses)
#  - calcule proportion p = effectif / n, variance p(1-p)/n et IC95

# charger et filtrer (si besoin)
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";", stringsAsFactors = FALSE)
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")

n <- nrow(data) # taille de l'échantillon

# fonction minimale : analyse une colonne à choix multiple
analyse_col <- function(col_name) {
  v <- data[[col_name]]
  # compter modalités
  counts <- table(unlist(strsplit(paste(v[!is.na(v) & v != ""], collapse = ";"), ";")))
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  names(counts) <- c("modalite", "effectif")
  # ajouter non-réponses
  n_non <- sum(is.na(v) | v == "")
  if (n_non > 0) counts <- rbind(counts, data.frame(modalite = "Non-reponse", effectif = n_non))
  # calculs simples
  counts$proportion <- counts$effectif / n
  counts$variance <- counts$proportion * (1 - counts$proportion) / n
  counts$se <- sqrt(counts$variance)
  counts$IC_low <- pmax(0, counts$proportion - 1.96 * counts$se)
  counts$IC_high <- pmin(1, counts$proportion + 1.96 * counts$se)
  counts$pourcent <- counts$proportion * 100
  counts[order(-counts$effectif), ]
}

# colonnes d'intérêt
cols <- c("X45..RP_PN_IA_usages", "X47..RP_PN_IA_raisons", "X53..RP_PN_IALimites")

for (cname in cols) {
  cat('\n', '--- Analyse simple :', cname, '---\n')
  print(analyse_col(cname), row.names = FALSE)
}

# === Analyse des fréquences d'utilisation (variables individuelles) ===
# colonnes individuelles de fréquence
freq_cols <- c("X36..ChatGPT", "X37..DeepL", "X38..Copilot", "X39..Grammarly", "X40..Perplexity", "X41..Autre")

for (fc in freq_cols) {
  if (!fc %in% names(data)) next
  cat('\n', '--- Fréquences :', fc, '---\n')
  vec <- data[[fc]]
  tab <- table(factor(vec, exclude = NULL), useNA = 'ifany')
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df) <- c('modalite', 'effectif')
  # remplacer NA par "Non-reponse"
  df$modalite[is.na(df$modalite)] <- 'Non-reponse'
  df$proportion <- df$effectif / n
  df$variance <- df$proportion * (1 - df$proportion) / n
  df$se <- sqrt(df$variance)
  df$IC_low <- pmax(0, df$proportion - 1.96 * df$se)
  df$IC_high <- pmin(1, df$proportion + 1.96 * df$se)
  df$pourcent <- df$proportion * 100
  df <- df[order(-df$effectif), ]
  print(df, row.names = FALSE)
}

# Fin (version courte)
cat('\nPost-stratifié - USAGES (par année d\'étude)\n')

print(post_usages_annee)
