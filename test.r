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

# ==================================================
# POST-STRATIFICATION SIMPLE (à remplacer par vos valeurs)
# ==================================================
# Exposé :
# - Remplacez `VAR_STRATE` par le nom de la colonne de strate dans `data_filtre`.
# - Complétez `pop_counts` par les effectifs connus de la population par strate.
# - Le code calcule, pour chaque modalité d'une variable à choix multiple,
#   l'estimateur post-stratifié de la proportion, sa variance approchée
#   (formule post-stratifiée) et l'IC 95%.

# Nom de la variable de stratification (remplacer par le nom réel)
VAR_STRATE <- "NOM_VARIABLE_POUR_STRATIFICATION"  # <- remplacer

# Totaux de population par strate (remplacer par les valeurs réelles)
# Exemple : pop_counts <- c("StrateA" = 1000, "StrateB" = 2000)
pop_counts <- c( "STRATE_1" = 1234, "STRATE_2" = 2345 )  # <- remplacer

# Fonction post-stratifiée pour une variable à choix multiple
poststrat_proportions <- function(df, var_q, strata_var, pop_counts, sep = ";") {
  # N total population
  N_total <- sum(pop_counts)

  # Modalités observées (inclut "Non-réponse" si présent)
  all_vals <- df[[var_q]]
  # Extraire toutes les modalités pour constituer la liste
  mods <- c()
  for (v in all_vals) {
    if (is.na(v) || v == "") next
    elems <- trimws(unlist(strsplit(v, sep)))
    mods <- c(mods, elems)
  }
  mods <- unique(mods)
  mods <- sort(mods)
  mods <- c(mods, "Non-réponse")

  # Préparer résultat
  res <- data.frame(modalite = mods, p_hat = NA, variance = NA, IC_inf = NA, IC_sup = NA, stringsAsFactors = FALSE)

  # Itérer sur chaque modalité
  for (m in mods) {
    # Variance accumulateur et numérateur pour p_hat
    num_p <- 0
    var_sum <- 0

    for (h in names(pop_counts)) {
      N_h <- pop_counts[[h]]
      # Sous-échantillon de la strate h
      sub_h <- df[!is.na(df[[strata_var]]) & df[[strata_var]] == h, ]
      n_h <- nrow(sub_h)
      if (n_h == 0) {
        p_h <- 0
        # si aucun échantillon dans la strate, on saute la contribution
        next
      }

      # compter le nombre d'unités dans la strate qui ont la modalité m
      if (m == "Non-réponse") {
        count_m_h <- sum(is.na(sub_h[[var_q]]) | sub_h[[var_q]] == "")
      } else {
        # nombre d'unités dont la réponse contient la modalité m
        present <- sapply(sub_h[[var_q]], function(x) {
          if (is.na(x) || x == "") return(FALSE)
          m %in% trimws(unlist(strsplit(x, sep)))
        })
        count_m_h <- sum(present)
      }

      # proportion dans la strate (parmi l'échantillon de la strate)
      p_h <- count_m_h / n_h

      # contribution au numérateur pondéré
      num_p <- num_p + N_h * p_h

      # taux de sondage dans la strate
      f_h <- n_h / N_h

      # variance contribution (formule post-stratifiée)
      var_h <- (1 - f_h) * (N_h^2) * (p_h * (1 - p_h) / n_h)
      var_sum <- var_sum + var_h
    }

    # estimateur global et variance
    p_hat <- num_p / N_total
    var_p_hat <- var_sum / (N_total^2)
    se <- sqrt(var_p_hat)
    z <- 1.96
    ic_inf <- p_hat - z * se
    ic_sup <- p_hat + z * se
    ic_inf <- max(0, ic_inf)
    ic_sup <- min(1, ic_sup)

    # stocker
    idx <- which(res$modalite == m)
    res$p_hat[idx] <- p_hat
    res$variance[idx] <- var_p_hat
    res$IC_inf[idx] <- ic_inf
    res$IC_sup[idx] <- ic_sup
  }

  # ajouter colonnes en pourcentage
  res$pourcent <- res$p_hat * 100
  return(res)
}

# Application du post-stratification (exemples pour usages/raisons/limites)
cat('\n', strrep('-', 80), '\n')
cat('POST-STRATIFICATION : remplacez VAR_STRATE et pop_counts par vos valeurs', '\n')
cat(strrep('-', 80), '\n')

# Vérifier que la variable de strate existe
if (!VAR_STRATE %in% names(data_filtre)) {
  cat('ATTENTION : la variable de stratification', VAR_STRATE, 'n\'existe pas dans data_filtre.\n')
  cat('Remplacez VAR_STRATE par le nom réel et complétez pop_counts.\n')
} else {
  post_usages <- poststrat_proportions(data_filtre, 'X45..RP_PN_IA_usages', VAR_STRATE, pop_counts)
  cat('\nPost-stratifié - USAGES\n')
  print(post_usages)

  post_raisons <- poststrat_proportions(data_filtre, 'X47..RP_PN_IA_raisons', VAR_STRATE, pop_counts)
  cat('\nPost-stratifié - RAISONS\n')
  print(post_raisons)

  post_limites <- poststrat_proportions(data_filtre, 'X53..RP_PN_IALimites', VAR_STRATE, pop_counts)
  cat('\nPost-stratifié - LIMITES\n')
  print(post_limites)
}

# =============================================================================
# ANALYSE DES FRÉQUENCES D'UTILISATION DES IA GENERATIVES
# =============================================================================

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

