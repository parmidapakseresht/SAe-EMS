# === CHARGEMENT DES DONNÉES (une seule fois) ===
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";")
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")
n <- nrow(data)

# === FONCTION 1 : Analyser réponses multiples (avec séparateur) ===
analyser_multiples <- function(col_name, titre) {
  cat("\n")
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | EFFECTIF | PROPORTION | VARIANCE   | SE        | IC 95%%", "MODALITÉ"))
  cat("\n")
  cat(strrep("-", 100), "\n")
  
  v <- data[[col_name]]
  
  # Extraire toutes les modalités
  elements <- c()
  for (i in 1:length(v)) {
    if (!is.na(v[i]) && v[i] != "") {
      parts <- strsplit(v[i], ";")[[1]]
      parts <- trimws(parts)
      elements <- c(elements, parts)
    }
  }
  
  # Compter les modalités
  modalites <- unique(elements)
  tab <- data.frame(modalite = modalites, effectif = 0)
  for (i in 1:nrow(tab)) {
    tab$effectif[i] <- sum(elements == tab$modalite[i])
  }
  
  # Ajouter non-réponses
  n_non <- sum(is.na(v) | v == "")
  tab <- rbind(tab, data.frame(modalite = "Non-reponse", effectif = n_non))
  
  # Trier et calculer stats
  tab <- tab[order(-tab$effectif), ]
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- tab$proportion * 100
  
  # Affichage formaté
  for (i in 1:nrow(tab)) {
    ic_str <- sprintf("[%.4f ; %.4f]", tab$IC_low[i], tab$IC_high[i])
    cat(sprintf("%-50s | %8d | %10.2f%% | %.8f | %.8f | %s\n", 
                substr(tab$modalite[i], 1, 49), 
                tab$effectif[i], 
                tab$pct[i], 
                tab$variance[i], 
                tab$se[i], 
                ic_str))
  }
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | %8d |\n", "TOTAL", sum(tab$effectif)))
  cat(strrep("=", 100), "\n")
}

# === FONCTION 2 : Analyser réponses simples (pas de séparateur) ===
analyser_simple <- function(col_name, titre) {
  cat("\n")
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | EFFECTIF | PROPORTION | VARIANCE   | SE        | IC 95%%", "MODALITÉ"))
  cat("\n")
  cat(strrep("-", 100), "\n")
  
  v <- data[[col_name]]
  
  # Compter les modalités
  tab <- data.frame(modalite = unique(v), effectif = 0)
  for (i in 1:nrow(tab)) {
    tab$effectif[i] <- sum(v == tab$modalite[i], na.rm = FALSE)
  }
  
  # Trier et calculer stats
  tab <- tab[order(-tab$effectif), ]
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- tab$proportion * 100
  
  # Affichage formaté
  for (i in 1:nrow(tab)) {
    ic_str <- sprintf("[%.4f ; %.4f]", tab$IC_low[i], tab$IC_high[i])
    cat(sprintf("%-50s | %8d | %10.2f%% | %.8f | %.8f | %s\n", 
                substr(tab$modalite[i], 1, 49), 
                tab$effectif[i], 
                tab$pct[i], 
                tab$variance[i], 
                tab$se[i], 
                ic_str))
  }
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | %8d |\n", "TOTAL", sum(tab$effectif)))
  cat(strrep("=", 100), "\n")
}

# === Variables d'intérêt ===
usages <- "X45..RP_PN_IA_usages"
raisons <- "X47..RP_PN_IA_raisons"
limites <- "X53..RP_PN_IALimites"
ChatGPT <- "X36..ChatGPT"
DeepL <- "X37..DeepL"
Copilot <- "X38..Copilot"
Grammarly <- "X39..Grammarly"
Perplexity <- "X40..Perplexity"
Autre <- "X41..Autre"

# === ANALYSES DES RÉPONSES MULTIPLES ===
analyser_multiples(usages, "USAGES")
analyser_multiples(raisons, "RAISONS")
analyser_multiples(limites, "LIMITES")

# === ANALYSES DES FRÉQUENCES (réponses simples) ===
analyser_simple(ChatGPT, "CHATGPT")
analyser_simple(DeepL, "DEEPL")
analyser_simple(Copilot, "COPILOT")
analyser_simple(Grammarly, "GRAMMARLY")
analyser_simple(Perplexity, "PERPLEXITY")
analyser_simple(Autre, "AUTRE")


# === REDRESSEMENT PAR POST-STRATIFICATION (TRÈS SIMPLE) ===
# Calcule des poids par strate puis résume une variable avec ces poids.
# Remplacez `COL_STRATE` et `totaux_population` par vos valeurs réelles (expliqué ci-dessous).
post_stratification <- function(data, col_strate, totaux_population) {
  # Fonction interne de normalisation
  norm <- function(x) {
    s <- trimws(as.character(x))
    s[is.na(s)] <- ""
    s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT")
    s <- gsub("[^A-Za-z0-9 ]+", " ", s)
    s <- tolower(s)
    s <- gsub("[[:space:]]+", " ", s)
    trimws(s)
  }

  strate_raw <- as.character(data[[col_strate]])
  strate_norm <- norm(strate_raw)
  # normaliser les noms des totaux
  noms_totaux_raw <- names(totaux_population)
  noms_totaux_norm <- norm(noms_totaux_raw)

  comptes_echantillon <- table(strate_norm, useNA = "no")
  noms_echantillon <- names(comptes_echantillon)

  # Pour chaque modalité attendue, on cherche la meilleure correspondance
  matched_sample_name <- rep(NA_character_, length(noms_totaux_raw))
  for (i in seq_along(noms_totaux_raw)) {
    key_norm <- noms_totaux_norm[i]
    if (key_norm == "") next
    # exact match
    exact <- which(noms_echantillon == key_norm)
    if (length(exact) == 1) {
      matched_sample_name[i] <- noms_echantillon[exact]
      next
    }
    # partial match: key contained in sample name
    partial1 <- which(grepl(key_norm, noms_echantillon, fixed = TRUE))
    if (length(partial1) >= 1) {
      matched_sample_name[i] <- noms_echantillon[partial1[1]]
      next
    }
    # reverse partial: sample name contained in key
    partial2 <- which(grepl(noms_echantillon, key_norm, fixed = TRUE))
    if (length(partial2) >= 1) {
      matched_sample_name[i] <- noms_echantillon[partial2[1]]
      next
    }
    # else leave NA
  }

  # Construire vecteur d'effectifs pour chaque modalité attendue
  effectifs_sample <- sapply(matched_sample_name, function(x) {
    if (is.na(x)) return(0)
    as.numeric(comptes_echantillon[x])
  })

  # Avertissements pour modalités non appariées
  manquantes <- noms_totaux_raw[is.na(matched_sample_name)]
  if (length(manquantes) > 0) {
    warning(paste("Modalités absentes de l'échantillon (poids mis à 0):", paste(manquantes, collapse = ", ")))
  }

  # Calcul des poids par modalité attendue
  poids_par_modalite <- numeric(length(effectifs_sample))
  for (i in seq_along(effectifs_sample)) {
    if (effectifs_sample[i] > 0) {
      poids_par_modalite[i] <- totaux_population[i] / effectifs_sample[i]
    } else {
      poids_par_modalite[i] <- 0
    }
  }
  names(poids_par_modalite) <- noms_totaux_raw

  # Construire table poids pour chaque modalité observée (norm)
  poids_par_echantillon <- setNames(rep(0, length(noms_echantillon)), noms_echantillon)
  for (i in seq_along(matched_sample_name)) {
    ms <- matched_sample_name[i]
    if (!is.na(ms)) poids_par_echantillon[ms] <- poids_par_modalite[i]
  }

  # Affecter poids aux individus selon leur strate normalisée
  poids <- rep(0, length(strate_norm))
  idx_valides <- which(strate_norm %in% names(poids_par_echantillon) & strate_norm != "")
  if (length(idx_valides) > 0) {
    poids[idx_valides] <- poids_par_echantillon[strate_norm[idx_valides]]
  }
  return(poids)
}

resume_pondere <- function(poids, nom_colonne, titre, somme_totaux_population) {
  cat("\n")
  cat(strrep("=", 100), "\n")
  cat(sprintf("REDRESSEMENT PAR DOMAINE - %-50s (POST-STRATIFIÉ)\n", titre))
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | EFF. POND | PROPORTION | VARIANCE   | SE        | IC 95%%", "MODALITÉ"))
  cat("\n")
  cat(strrep("-", 100), "\n")
  
  v <- data[[nom_colonne]]
  modalites <- unique(v)
  tab <- data.frame(modalite = modalites, effectif_pondere = 0)
  for (i in 1:nrow(tab)) {
    tab$effectif_pondere[i] <- sum(poids[which(v == tab$modalite[i])], na.rm = TRUE)
  }
  non_pondere <- sum(poids[which(is.na(v) | v == "")], na.rm = TRUE)
  tab <- rbind(tab, data.frame(modalite = "Non-reponse", effectif_pondere = non_pondere))

  # Proportions relatives à la population totale (somme des totaux fournis)
  tab$proportion <- tab$effectif_pondere / somme_totaux_population

  # Taille effective d'échantillon pondérée (approximation)
  n_eff <- if (sum(poids^2) > 0) (sum(poids)^2) / sum(poids^2) else NA

  # Variance approximative et IC (utilise n_eff)
  tab$variance <- tab$proportion * (1 - tab$proportion) / n_eff
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- tab$proportion * 100
  
  # Trier par effectif pondéré
  tab <- tab[order(-tab$effectif_pondere), ]
  
  # Affichage formaté
  for (i in 1:nrow(tab)) {
    ic_str <- sprintf("[%.4f ; %.4f]", tab$IC_low[i], tab$IC_high[i])
    cat(sprintf("%-50s | %9.1f | %10.2f%% | %.8f | %.8f | %s\n", 
                substr(tab$modalite[i], 1, 49), 
                tab$effectif_pondere[i], 
                tab$pct[i], 
                tab$variance[i], 
                tab$se[i], 
                ic_str))
  }
  cat(strrep("=", 100), "\n")
  cat(sprintf("%-50s | %9.1f |\n", "TOTAL PONDÉRÉ", sum(tab$effectif_pondere)))
  cat(strrep("=", 100), "\n")
}

# --- Redressement en fonction de l'année d'étude ---
COL_STRATE <- "DIP...Annee"    # <-- remplacer par le nom de la colonne de strate
totaux_population <- c("BUT1" = 2199, "BUT2" = 1674, "BUT3" = 1481) # <-- remplacer
poids <- post_stratification(data, COL_STRATE, totaux_population)
somme_totaux_population <- sum(totaux_population)
resume_pondere(poids, usages, "CHATGPT", somme_totaux_population)
resume_pondere(poids, raisons, "CHATGPT", somme_totaux_population)
resume_pondere(poids, limites, "CHATGPT", somme_totaux_population)
resume_pondere(poids, ChatGPT, "CHATGPT", somme_totaux_population)
resume_pondere(poids, DeepL, "DEEPL", somme_totaux_population)
resume_pondere(poids, Copilot, "COPILOT", somme_totaux_population)
resume_pondere(poids, Grammarly, "GRAMMARLY", somme_totaux_population)
resume_pondere(poids, Perplexity, "PERPLEXITY", somme_totaux_population)
resume_pondere(poids, Autre, "AUTRE", somme_totaux_population)

# --- Redressement en fonction du domaine (regroupement de mentions) ---
# Dictionnaire : mention -> domaine
mention_to_domaine <- c(
  # Gestion et Commerce
  "Carrières Juridiques (IUT2)" = "Gestion et Commerce",
  "Carrières Sociales (IUT2)" = "Gestion et Commerce",
  "Gestion des Entreprises et des Administrations (IUT Valence)" = "Gestion et Commerce",
  "Gestion des Entreprises et des Administrations (IUT2)" = "Gestion et Commerce",
  "Information - Communication (IUT2)" = "Gestion et Commerce",
  "Techniques de Commercialisation (IUT Valence)" = "Gestion et Commerce",
  "Techniques de Commercialisation (IUT2)" = "Gestion et Commerce",
  # Informatique et Data
  "Informatique (IUT Valence)" = "Informatique et Data",
  "Informatique (IUT2)" = "Informatique et Data",
  "Science des Données (IUT2)" = "Informatique et Data",
  # Réseaux et Télécoms
  "Réseaux et Télécommunications (IUT1)" = "Réseaux et Télécoms",
  # Sciences et Technologies
  "Génie Civil Construction Durable (IUT1)" = "Sciences et Technologies",
  "Génie Electrique et Informatique Industrielle (IUT1)" = "Sciences et Technologies",
  "Génie Mécanique et Productique (IUT1)" = "Sciences et Technologies"
)

# Totaux par domaine (pour BUT1, BUT2, BUT3)
domaine_totaux <- c(
  "Gestion et Commerce" = 986 + 794 + 695,
  "Informatique et Data" = 250 + 205 + 171,
  "Réseaux et Télécoms" = 90 + 69 + 75,
  "Sciences et Technologies" = 410 + 269 + 260
)

# Ajouter colonne domaine à data (basée sur la colonne Mention)
# Normalisation et mapping plus robustes : on supprime accents, ponctuation
normaliser <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(s)] <- ""
  # enlever accents
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT")
  # garder lettres et chiffres et espaces
  s <- gsub("[^A-Za-z0-9 ]+", " ", s)
  s <- tolower(s)
  s <- gsub("[[:space:]]+", " ", s)
  trimws(s)
}

mentions_obs_raw <- as.character(data$Mention)
mentions_obs_norm <- normaliser(mentions_obs_raw)

# Préparer clé normalisée pour le dictionnaire
keys_raw <- names(mention_to_domaine)
keys_norm <- normaliser(keys_raw)

data$domaine <- NA
for (i in seq_along(mentions_obs_norm)) {
  m <- mentions_obs_norm[i]
  if (m == "") next
  # 1) correspondance exacte sur version normalisée
  match_idx <- which(keys_norm == m)
  if (length(match_idx) == 1) {
    data$domaine[i] <- mention_to_domaine[[keys_raw[match_idx]]]
    next
  }
  # 2) correspondance partielle : clé contenue dans mention ou inverse
  found <- FALSE
  for (j in seq_along(keys_norm)) {
    if (keys_norm[j] == "") next
    if (grepl(keys_norm[j], m, fixed = TRUE) || grepl(m, keys_norm[j], fixed = TRUE)) {
      data$domaine[i] <- mention_to_domaine[[keys_raw[j]]]
      found <- TRUE
      break
    }
  }
  if (!found) data$domaine[i] <- NA
}

# signaler quelques mentions non appariées pour aide au debug
unmatched <- unique(mentions_obs_raw[is.na(data$domaine) & mentions_obs_raw != ""]) 
if (length(unmatched) > 0) {
  warning(paste0("Mentions non appariées (domaine NA) — exemples: ", paste(head(unmatched, 10), collapse = ", ")))
}

COL_STRATE <- "domaine"
totaux_population <- domaine_totaux
poids <- post_stratification(data, COL_STRATE, totaux_population)
somme_totaux_population <- sum(totaux_population)
resume_pondere(poids, usages, "USAGES", somme_totaux_population)
resume_pondere(poids, raisons, "RAISONS", somme_totaux_population)
resume_pondere(poids, limites, "LIMITES", somme_totaux_population)
resume_pondere(poids, ChatGPT, "CHATGPT", somme_totaux_population)
resume_pondere(poids, DeepL, "DEEPL", somme_totaux_population)
resume_pondere(poids, Copilot, "COPILOT", somme_totaux_population)
resume_pondere(poids, Grammarly, "GRAMMARLY", somme_totaux_population)
resume_pondere(poids, Perplexity, "PERPLEXITY", somme_totaux_population)
resume_pondere(poids, Autre, "AUTRE", somme_totaux_population)

# --- Redressement en fonction du sexe ---
COL_STRATE <- "Individu...Sexe"    # <-- remplacer par le nom de la colonne de strate
totaux_population <- c("M" = 3205, "F" = 2149) # <-- remplacer
poids <- post_stratification(data, COL_STRATE, totaux_population)
somme_totaux_population <- sum(totaux_population)
resume_pondere(poids, usages, "CHATGPT", somme_totaux_population)
resume_pondere(poids, raisons, "CHATGPT", somme_totaux_population)
resume_pondere(poids, limites, "CHATGPT", somme_totaux_population)
resume_pondere(poids, ChatGPT, "CHATGPT", somme_totaux_population)
resume_pondere(poids, DeepL, "DEEPL", somme_totaux_population)
resume_pondere(poids, Copilot, "COPILOT", somme_totaux_population)
resume_pondere(poids, Grammarly, "GRAMMARLY", somme_totaux_population)
resume_pondere(poids, Perplexity, "PERPLEXITY", somme_totaux_population)
resume_pondere(poids, Autre, "AUTRE", somme_totaux_population)

# --- Redressement en fonction de la bourse ---
COL_STRATE <- "Type.bourse..lib.."    # <-- remplacer par le nom de la colonne de strate
totaux_population <- c("Boursier" = 1544, "Non Boursier" = 3810) # <-- remplacer
poids <- post_stratification(data, COL_STRATE, totaux_population)
somme_totaux_population <- sum(totaux_population)
resume_pondere(poids, usages, "CHATGPT", somme_totaux_population)
resume_pondere(poids, raisons, "CHATGPT", somme_totaux_population)
resume_pondere(poids, limites, "CHATGPT", somme_totaux_population)
resume_pondere(poids, ChatGPT, "CHATGPT", somme_totaux_population)
resume_pondere(poids, DeepL, "DEEPL", somme_totaux_population)
resume_pondere(poids, Copilot, "COPILOT", somme_totaux_population)
resume_pondere(poids, Grammarly, "GRAMMARLY", somme_totaux_population)
resume_pondere(poids, Perplexity, "PERPLEXITY", somme_totaux_population)
resume_pondere(poids, Autre, "AUTRE", somme_totaux_population)