# === CHARGEMENT DES DONNÉES (une seule fois) ===
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep = ";")
data <- subset(data, data$X96..DATE_ENREG <= "03/12/2025 15:30:00")
n <- nrow(data)

# === FONCTION 1 : Analyser réponses multiples (avec séparateur) ===
analyser_multiples <- function(col_name, titre) {
  cat("\n===== ", titre, " =====\n")
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
  
  print(tab, row.names = FALSE)
}

# === FONCTION 2 : Analyser réponses simples (pas de séparateur) ===
analyser_simple <- function(col_name, titre) {
  cat("\n===== ", titre, " =====\n")
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
  
  print(tab, row.names = FALSE)
}

# === ANALYSES DES RÉPONSES MULTIPLES ===
analyser_multiples("X45..RP_PN_IA_usages", "USAGES")
analyser_multiples("X47..RP_PN_IA_raisons", "RAISONS")
analyser_multiples("X53..RP_PN_IALimites", "LIMITES")

# === ANALYSES DES FRÉQUENCES (réponses simples) ===
analyser_simple("X36..ChatGPT", "CHATGPT")
analyser_simple("X37..DeepL", "DEEPL")
analyser_simple("X38..Copilot", "COPILOT")
analyser_simple("X39..Grammarly", "GRAMMARLY")
analyser_simple("X40..Perplexity", "PERPLEXITY")
analyser_simple("X41..Autre", "AUTRE")


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
