# analyses.r
# Script d'analyse d'un sondage PESR avec redressement par raking
# - proportions pour variables simples et choix multiple
# - redressement par raking (méthode calib() du package survey, comme Activité 4)
# - intervalles de confiance et variances estimées

# --- 0) Chargement du package survey ---
# install.packages("survey")  # Décommentez si le package n'est pas installé
library(survey)

# --- 1) Chargement et préparation des données ---
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";", stringsAsFactors = FALSE)
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")
n <- nrow(data)
data$poids_initial <- rep(1, nrow(data))

# --- 2) Analyses non redressées ---

# Proportions pour variable simple
prop_simple <- function(col_name, titre) {
  cat("\n--- ", titre, " (non redressé) ---\n")
  v <- data[[col_name]]
  tab_effectifs <- table(v, useNA = "ifany")
  tab <- as.data.frame(tab_effectifs, stringsAsFactors = FALSE)
  names(tab) <- c("modalite", "effectif")
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- round(tab$proportion * 100, 2)
  print(tab[order(-tab$effectif), ], row.names = FALSE)
}

# Proportions pour variable à choix multiple
prop_multiple <- function(col_name, titre) {
  cat("\n--- ", titre, " (choix multiple, non redressé) ---\n")
  v <- data[[col_name]]
  toutes_modalites <- c()
  for (i in seq_along(v)) {
    if (!is.na(v[i]) && v[i] != "") {
      parts <- strsplit(v[i], ";")[[1]]
      parts <- trimws(parts)
      toutes_modalites <- c(toutes_modalites, parts)
    }
  }
  if (length(toutes_modalites) == 0) {
    cat("Aucune réponse renseignée pour ", col_name, "\n")
    return(invisible(NULL))
  }
  tab_effectifs <- table(toutes_modalites)
  tab <- as.data.frame(tab_effectifs, stringsAsFactors = FALSE)
  names(tab) <- c("modalite", "effectif")
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- round(tab$proportion * 100, 2)
  print(tab[order(-tab$effectif), ], row.names = FALSE)
}

# --- 3) Redressement par raking (Activité 4) ---
# 
# Fonction redressement_raking : applique calib() du package survey
# avec method="raking" pour calculer des poids redressés
redressement_raking <- function(col_strate, totaux_population) {
  cat("\n=== REDRESSEMENT PAR RAKING (calib avec method='raking') ===\n")
  
  strate <- as.character(data[[col_strate]])
  strate <- trimws(strate)
  strate[is.na(strate)] <- ""
  
  modalites_attendues <- names(totaux_population)
  
  # Créer variables indicatrices pour chaque modalité attendue
  X <- data.frame()
  for (m in modalites_attendues) {
    X[[paste0(col_strate, "_", m)]] <- as.numeric(strate == m)
  }
  
  # Appliquer calib() avec method="raking"
  cat("Application de calib() avec method='raking'...\n")
  poids_redresses <- calib(
    X = X,
    d = data$poids_initial,
    total = totaux_population,
    method = "raking",
    description = TRUE
  )
  
  cat("\nSomme des poids redressés :", round(sum(poids_redresses), 2), "\n")
  cat("Statistiques des poids redressés :\n")
  print(summary(poids_redresses))
  
  return(poids_redresses)
}

# --- 4) Affichage proportions redressées avec package survey ---
prop_redressee <- function(poids, col_name, titre) {
  cat("\n--- ", titre, " (redressé par raking) ---\n")
  
  data$poids_redresse <- poids
  
  design <- svydesign(
    ids = ~1,
    weights = ~poids_redresse,
    data = data
  )
  
  v <- data[[col_name]]
  modalites <- unique(v)
  
  resultats <- data.frame(
    modalite = modalites,
    effectif_pond = 0,
    proportion = 0,
    se = 0,
    IC_low = 0,
    IC_high = 0,
    pct = 0,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(resultats))) {
    modal <- resultats$modalite[i]
    if (is.na(modal)) {
      data$temp_var <- as.numeric(is.na(v))
    } else {
      data$temp_var <- as.numeric(v == modal)
    }
    
    design$variables$temp_var <- data$temp_var
    est <- svymean(~temp_var, design)
    prop <- as.numeric(est[1])
    se <- sqrt(as.numeric(attr(est, "var")[1,1]))
    
    resultats$proportion[i] <- prop
    resultats$se[i] <- se
    resultats$IC_low[i] <- pmax(0, prop - 1.96 * se)
    resultats$IC_high[i] <- pmin(1, prop + 1.96 * se)
    resultats$pct[i] <- round(prop * 100, 2)
    
    if (is.na(modal)) {
      idx <- which(is.na(v))
    } else {
      idx <- which(v == modal)
    }
    resultats$effectif_pond[i] <- sum(poids[idx], na.rm = TRUE)
  }
  
  print(resultats[order(-resultats$effectif_pond), ], row.names = FALSE)
}

# --- 5) Variables d'intérêt et exécution ---
vars_multiples <- c("X45..RP_PN_IA_usages", "X47..RP_PN_IA_raisons", "X53..RP_PN_IALimites")
vars_simples <- c("X36..ChatGPT", "X37..DeepL", "X38..Copilot", "X39..Grammarly", "X40..Perplexity", "X41..Autre")

cat("\n========== ANALYSES SIMPLES (PESR, non redressées) ==========\n")
for (col in vars_multiples) prop_multiple(col, col)
for (col in vars_simples) prop_simple(col, col)

# --- 6) Exemple d'utilisation du redressement ---

# Vous devez remplacer les valeurs ci-dessous par vos vrais totaux de population
# Décommentez et adaptez les lignes suivantes :

# Redressement par année d'étude
col_strate <- "DIP...Annee"
totaux_pop <- c("BUT1" = 2199, "BUT2" = 1674, "BUT3" = 1481)

# Appliquer le redressement par raking
poids_redresses <- redressement_raking(col_strate, totaux_pop)

# Afficher les analyses redressées
cat("\n========== ANALYSES REDRESSÉES PAR RAKING ==========\n")
for (col in vars_multiples) prop_redressee(poids_redresses, col, col)
for (col in vars_simples) prop_redressee(poids_redresses, col, col)

# Redressement par domaine
col_strate <- "Mention"
totaux_pop <- c(
  "Gestion et Commerce" = 986 + 794 + 695,
  "Informatique et Data" = 250 + 205 + 171,
  "Réseaux et Télécoms" = 90 + 69 + 75,
  "Sciences et Technologies" = 410 + 269 + 260
)

# Appliquer le redressement par raking
poids_redresses <- redressement_raking(col_strate, totaux_pop)

# Afficher les analyses redressées
cat("\n========== ANALYSES REDRESSÉES PAR RAKING ==========\n")
for (col in vars_multiples) prop_redressee(poids_redresses, col, col)
for (col in vars_simples) prop_redressee(poids_redresses, col, col)

# Redressement par sexe
col_strate <- "Individu...Sexe"
totaux_pop <- c("M" = 3205, "F" = 2149)

# Appliquer le redressement par raking
poids_redresses <- redressement_raking(col_strate, totaux_pop)

# Afficher les analyses redressées
cat("\n========== ANALYSES REDRESSÉES PAR RAKING ==========\n")
for (col in vars_multiples) prop_redressee(poids_redresses, col, col)
for (col in vars_simples) prop_redressee(poids_redresses, col, col)

# FIN
