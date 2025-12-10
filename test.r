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
# Script simplifié d'analyse des sondages
# - compte des modalités (y compris non-réponses)
# - estimateur de proportion, variance avec correction finie (si N connu), IC95
# - post-stratification simple par une variable de strate
# Le code est volontairement compact et commenté pour un débutant.

# === Chargement des données ===
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";", stringsAsFactors = FALSE)
# Filtre temporel (garder si utile)
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")

# === Fonctions simples ===
# Compter modalités dans une colonne de réponses multiples (séparateur ';')
compter_modalites <- function(col, sep = ";") {
  n_non <- 0
  all_el <- c()
  for (v in col) {
    if (is.na(v) || v == "") {
      n_non <- n_non + 1
    } else {
      parts <- trimws(unlist(strsplit(v, sep)))
      all_el <- c(all_el, parts)
    }
  }
  tab <- as.data.frame(table(all_el), stringsAsFactors = FALSE)
  if (n_non > 0) tab <- rbind(tab, data.frame(all_el = "Non-reponse", Freq = n_non))
  names(tab) <- c("modalite", "effectif")
  tab[order(-tab$effectif), , drop = FALSE]
}

# Calculer proportion, variance (avec facteur 1-f si N_population fourni), IC95
calc_stats <- function(effectifs, n_sample, N_population = NULL) {
  p <- effectifs / n_sample
  if (is.null(N_population)) {
    f <- 0  # pas de correction finie si N inconnu
  } else {
    f <- n_sample / N_population
  }
  var_p <- (p * (1 - p) / n_sample) * (1 - f)
  se <- sqrt(var_p)
  z <- 1.96
  ic_low <- p - z * se
  ic_high <- p + z * se
  ic_low <- pmax(0, ic_low)
  ic_high <- pmin(1, ic_high)
  data.frame(proportion = p, pct = p * 100, variance = var_p, se = se, IC_low = ic_low, IC_high = ic_high)
}

# Post-stratification simple
# - df: dataframe
# - var_q: colonne question (choix multiples)
# - strata: colonne de stratification (factor-like)
# - pop_counts: vecteur nommé des effectifs population par strate. Si NULL -> on utilise les effectifs d'echantillon (à remplacer par vrais totaux si disponibles)
poststrat_simple <- function(df, var_q, strata, pop_counts = NULL, sep = ";") {
  if (!strata %in% names(df)) stop("La variable de strate n'existe pas dans df")
  if (is.null(pop_counts)) {
    pop_tab <- table(df[[strata]], useNA = "no")
    pop_counts <- as.numeric(pop_tab)
    names(pop_counts) <- names(pop_tab)
  }
  Ntot <- sum(pop_counts)
  tab_all <- compter_modalites(df[[var_q]], sep = sep)
  mods <- tab_all$modalite
  res <- data.frame(modalite = mods, p_post = NA, variance = NA, IC_low = NA, IC_high = NA, stringsAsFactors = FALSE)

  for (m in mods) {
    num <- 0
    varsum <- 0
    for (h in names(pop_counts)) {
      N_h <- pop_counts[[h]]
      sub <- df[df[[strata]] == h, ]
      n_h <- nrow(sub)
      if (n_h == 0) next
      if (m == "Non-reponse") {
        x_h <- sum(is.na(sub[[var_q]]) | sub[[var_q]] == "")
      } else {
        presente <- sapply(sub[[var_q]], function(x) { if (is.na(x) || x == "") return(FALSE); m %in% trimws(unlist(strsplit(x, sep))) })
        x_h <- sum(presente)
      }
      p_h <- x_h / n_h
      num <- num + N_h * p_h
      f_h <- n_h / N_h
      var_h <- (1 - f_h) * (N_h^2) * (p_h * (1 - p_h) / n_h)
      varsum <- varsum + var_h
    }
    p_hat <- num / Ntot
    var_hat <- varsum / (Ntot^2)
    se <- sqrt(var_hat)
    z <- 1.96
    il <- max(0, p_hat - z * se)
    ih <- min(1, p_hat + z * se)
    idx <- which(res$modalite == m)
    res$p_post[idx] <- p_hat
    res$variance[idx] <- var_hat
    res$IC_low[idx] <- il
    res$IC_high[idx] <- ih
  }
  res$pourcent <- res$p_post * 100
  res
}

# === Variables d'interet ===
usages_col <- "X45..RP_PN_IA_usages"
raisons_col <- "X47..RP_PN_IA_raisons"
limites_col <- "X53..RP_PN_IALimites"

# colonnes de stratification disponibles dans le fichier (adapter si noms differents)
strata_vars <- c("mention", "DIP...Annee", "Individu...Sexe")

# nombre echantillon
n_sample <- nrow(data)

# Si vous connaissez N population totales, mettez la valeur ci-dessous, sinon laissez NULL
N_population <- NULL

# affichage simple
afficher_table <- function(df) print(df, row.names = FALSE)

# === Proportions simples (par modalité) ===
for (col in c(usages_col, raisons_col, limites_col)) {
  cat('\n', strrep('=', 60), '\n')
  cat('Analyse de :', col, '\n')
  cat(strrep('=', 60), '\n')
  t <- compter_modalites(data[[col]])
  stats <- calc_stats(t$effectif, n_sample, N_population)
  out <- cbind(t, stats)
  afficher_table(out)
}

# === Post-stratification par chaque variable de strate ===
for (sname in strata_vars) {
  if (!sname %in% names(data)) next
  cat('\n', strrep('-', 60), '\n')
  cat('Post-stratification par :', sname, '\n')
  cat(strrep('-', 60), '\n')
  pop_here <- NULL # Remplacer par vecteur nommé si vous avez les totaux réels
  post_u <- poststrat_simple(data, usages_col, sname, pop_counts = pop_here)
  cat('\nUsages (post-stratifié):\n')
  afficher_table(post_u)
  post_r <- poststrat_simple(data, raisons_col, sname, pop_counts = pop_here)
  cat('\nRaisons (post-stratifié):\n')
  afficher_table(post_r)
  post_l <- poststrat_simple(data, limites_col, sname, pop_counts = pop_here)
  cat('\nLimites (post-stratifié):\n')
  afficher_table(post_l)
}

# Fin
cat('\nPost-stratifié - USAGES (par année d\'étude)\n')

print(post_usages_annee)
