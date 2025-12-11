# === CHARGEMENT DES DONNÉES (une seule fois) ===
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";")
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")
n <- nrow(data)

# === FONCTION SIMPLE POUR ANALYSER UNE COLONNE ===
analyser_col <- function(col_name, titre, sep = ";") {
  cat("\n===== ", titre, " =====\n")
  
  v <- data[[col_name]]
  
  # Extraire toutes les modalités (réponses multiples)
  elements <- c()
  for (i in 1:length(v)) {
    if (!is.na(v[i]) && v[i] != "") {
      parts <- strsplit(v[i], sep)[[1]]
      parts <- trimws(parts)
      elements <- c(elements, parts)
    }
  }
  
  # Compter les modalités
  modalites <- unique(elements)
  tab <- data.frame(modalite = character(), effectif = numeric())
  for (m in modalites) {
    eff <- sum(elements == m)
    tab <- rbind(tab, data.frame(modalite = m, effectif = eff))
  }
  
  # Ajouter non-réponses
  n_non <- sum(is.na(v) | v == "")
  tab <- rbind(tab, data.frame(modalite = "Non-reponse", effectif = n_non))
  
  # Trier par effectif décroissant
  tab <- tab[order(-tab$effectif), ]
  
  # Calculer stats simples
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- tab$proportion * 100
  
  print(tab, row.names = FALSE)
}

# === ANALYSES DES RÉPONSES MULTIPLES ===
analyser_col("X45..RP_PN_IA_usages", "USAGES")
analyser_col("X47..RP_PN_IA_raisons", "RAISONS")
analyser_col("X53..RP_PN_IALimites", "LIMITES")

# === ANALYSES DES FRÉQUENCES (réponses simples) ===
analyser_freq <- function(col_name, titre) {
  cat("\n===== ", titre, " =====\n")
  
  v <- data[[col_name]]
  
  # Compter les modalités (pas de séparation)
  tab <- data.frame(modalite = unique(v), effectif = NA)
  for (i in 1:nrow(tab)) {
    tab$effectif[i] <- sum(v == tab$modalite[i])
  }
  
  # Trier par effectif décroissant
  tab <- tab[order(-tab$effectif), ]
  
  # Calculer stats
  tab$proportion <- tab$effectif / n
  tab$variance <- tab$proportion * (1 - tab$proportion) / n
  tab$se <- sqrt(tab$variance)
  tab$IC_low <- pmax(0, tab$proportion - 1.96 * tab$se)
  tab$IC_high <- pmin(1, tab$proportion + 1.96 * tab$se)
  tab$pct <- tab$proportion * 100
  
  print(tab, row.names = FALSE)
}

# Appliquer à chaque fréquence
analyser_freq("X36..ChatGPT", "CHATGPT")
analyser_freq("X37..DeepL", "DEEPL")
analyser_freq("X38..Copilot", "COPILOT")
analyser_freq("X39..Grammarly", "GRAMMARLY")
analyser_freq("X40..Perplexity", "PERPLEXITY")
analyser_freq("X41..Autre", "AUTRE")
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
