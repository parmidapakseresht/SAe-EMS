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
# ULTRA-SIMPLE : code basique pour débutant
# Charger données
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";")
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")
n <- nrow(data)

# ============= USAGES =============
cat("\n===== USAGES =====\n")
v <- data$X45..RP_PN_IA_usages
elements <- c()
for (i in 1:length(v)) {
  if (!is.na(v[i]) && v[i] != "") {
    parts <- strsplit(v[i], ";")[[1]]
    parts <- trimws(parts)
    elements <- c(elements, parts)
  }
}
modalites <- unique(elements)
tab_usages <- data.frame(modalite = character(), effectif = numeric())
for (m in modalites) {
  eff <- sum(elements == m)
  tab_usages <- rbind(tab_usages, data.frame(modalite = m, effectif = eff))
}
n_non <- sum(is.na(v) | v == "")
tab_usages <- rbind(tab_usages, data.frame(modalite = "Non-reponse", effectif = n_non))
tab_usages <- tab_usages[order(-tab_usages$effectif), ]
tab_usages$proportion <- tab_usages$effectif / n
tab_usages$variance <- tab_usages$proportion * (1 - tab_usages$proportion) / n
tab_usages$se <- sqrt(tab_usages$variance)
tab_usages$IC_low <- pmax(0, tab_usages$proportion - 1.96 * tab_usages$se)
tab_usages$IC_high <- pmin(1, tab_usages$proportion + 1.96 * tab_usages$se)
tab_usages$pct <- tab_usages$proportion * 100
print(tab_usages, row.names = FALSE)

# ============= RAISONS =============
cat("\n===== RAISONS =====\n")
v <- data$X47..RP_PN_IA_raisons
elements <- c()
for (i in 1:length(v)) {
  if (!is.na(v[i]) && v[i] != "") {
    parts <- strsplit(v[i], ";")[[1]]
    parts <- trimws(parts)
    elements <- c(elements, parts)
  }
}
modalites <- unique(elements)
tab_raisons <- data.frame(modalite = character(), effectif = numeric())
for (m in modalites) {
  eff <- sum(elements == m)
  tab_raisons <- rbind(tab_raisons, data.frame(modalite = m, effectif = eff))
}
n_non <- sum(is.na(v) | v == "")
tab_raisons <- rbind(tab_raisons, data.frame(modalite = "Non-reponse", effectif = n_non))
tab_raisons <- tab_raisons[order(-tab_raisons$effectif), ]
tab_raisons$proportion <- tab_raisons$effectif / n
tab_raisons$variance <- tab_raisons$proportion * (1 - tab_raisons$proportion) / n
tab_raisons$se <- sqrt(tab_raisons$variance)
tab_raisons$IC_low <- pmax(0, tab_raisons$proportion - 1.96 * tab_raisons$se)
tab_raisons$IC_high <- pmin(1, tab_raisons$proportion + 1.96 * tab_raisons$se)
tab_raisons$pct <- tab_raisons$proportion * 100
print(tab_raisons, row.names = FALSE)

# ============= LIMITES =============
cat("\n===== LIMITES =====\n")
v <- data$X53..RP_PN_IALimites
elements <- c()
for (i in 1:length(v)) {
  if (!is.na(v[i]) && v[i] != "") {
    parts <- strsplit(v[i], ";")[[1]]
    parts <- trimws(parts)
    elements <- c(elements, parts)
  }
}
modalites <- unique(elements)
tab_limites <- data.frame(modalite = character(), effectif = numeric())
for (m in modalites) {
  eff <- sum(elements == m)
  tab_limites <- rbind(tab_limites, data.frame(modalite = m, effectif = eff))
}
n_non <- sum(is.na(v) | v == "")
tab_limites <- rbind(tab_limites, data.frame(modalite = "Non-reponse", effectif = n_non))
tab_limites <- tab_limites[order(-tab_limites$effectif), ]
tab_limites$proportion <- tab_limites$effectif / n
tab_limites$variance <- tab_limites$proportion * (1 - tab_limites$proportion) / n
tab_limites$se <- sqrt(tab_limites$variance)
tab_limites$IC_low <- pmax(0, tab_limites$proportion - 1.96 * tab_limites$se)
tab_limites$IC_high <- pmin(1, tab_limites$proportion + 1.96 * tab_limites$se)
tab_limites$pct <- tab_limites$proportion * 100
print(tab_limites, row.names = FALSE)

# ============= FRÉQUENCES =============
cat("\n===== CHATGPT =====\n")
v <- data$X36..ChatGPT
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)

cat("\n===== DEEPL =====\n")
v <- data$X37..DeepL
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)

cat("\n===== COPILOT =====\n")
v <- data$X38..Copilot
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)

cat("\n===== GRAMMARLY =====\n")
v <- data$X39..Grammarly
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)

cat("\n===== PERPLEXITY =====\n")
v <- data$X40..Perplexity
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)

cat("\n===== AUTRE =====\n")
v <- data$X41..Autre
tab <- data.frame(modalite = unique(v), effectif = NA)
for (i in 1:nrow(tab)) { tab$effectif[i] <- sum(v == tab$modalite[i]) }
tab <- tab[order(-tab$effectif), ]
tab$proportion <- tab$effectif / n
tab$variance <- tab$proportion * (1 - tab$proportion) / n
tab$se <- sqrt(tab$variance)
tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
tab$pct <- tab$proportion * 100
print(tab, row.names = FALSE)
cat('\nPost-stratifié - USAGES (par année d\'étude)\n')

print(post_usages_annee)
