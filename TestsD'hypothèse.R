data <- read.csv("data_filtre.csv", sep=",", stringsAsFactors = TRUE)

# Rename columns to standard names
names(data)[grep("Sexe", names(data))[1]] <- "Sexe"
names(data)[grep("Annee", names(data))[1]] <- "Annee"
names(data)[grep("bourse", names(data))[1]] <- "Bourse"

# ==============================================================================
# TEST 1: SEXE (Gender)
# ==============================================================================
print("--- TEST SEXE ---")
sexe_table <- table(data$Sexe)
prop_sexe <- c("F" = 0.4082, "M" = 0.5918)
prop_sexe <- prop_sexe / sum(prop_sexe)
test_sexe <- chisq.test(sexe_table, p = prop_sexe[names(sexe_table)])
print(test_sexe)

# ==============================================================================
# TEST 2: ANNÉE (Academic Year)
# ==============================================================================
print("--- TEST ANNÉE ---")
annee_table <- table(data$Annee)
prop_annee <- c("BUT1" = 0.4109, "BUT2" = 0.3122, "BUT3" = 0.2768)
prop_annee <- prop_annee / sum(prop_annee)
test_annee <- chisq.test(annee_table, p = prop_annee[names(annee_table)])
print(test_annee)

# ==============================================================================
# TEST 3: BOURSE (Scholarship)
# ==============================================================================
print("--- TEST BOURSE ---")
data_bourse <- subset(data, Bourse != "" & !is.na(Bourse))
bourse_table <- table(data_bourse$Bourse)
prop_bourse <- c("Boursier" = 0.2810, "Non boursier" = 0.7190)
prop_bourse <- prop_bourse / sum(prop_bourse)
test_bourse <- chisq.test(bourse_table, p = prop_bourse[names(bourse_table)])
print(test_bourse)

# ==============================================================================
# TEST 4: MENTION (Field of Study)
# ==============================================================================
print(">>> TEST MENTION <<<")

mention_table <- table(data$Mention)
print(mention_table)

# Population targets for each mention (2025-26)
targets_mention <- c(
  "Carrières Juridiques"                           = 142 + 116 + 101,
  "Carrières Sociales"                             = 84 + 63 + 64,
  "Chimie"                                          = 87 + 57 + 60,
  "Génie Civil Construction Durable"               = 131 + 74 + 86,
  "Génie Electrique et Informatique Industrielle"  = 157 + 102 + 81,
  "Génie Mécanique et Productique"                 = 122 + 93 + 93,
  "Gestion des Entreprises et des Administrations" = 169 + 125 + 107 + 228 + 206 + 155,
  "Information - Communication"                    = 122 + 87 + 79,
  "Informatique"                                   = 61 + 56 + 43 + 132 + 107 + 90,
  "Mesures Physiques"                              = 105 + 86 + 51,
  "Métiers de la Transition et de l'Efficacité Energétiques" = 93 + 63 + 56,
  "Métiers du Multimédia et de l'Internet"         = 109 + 92 + 87,
  "Réseaux et Télécommunications"                  = 59 + 44 + 42 + 90 + 69 + 75,
  "Science des Données"                            = 57 + 42 + 38,
  "Techniques de Commercialisation"                = 157 + 120 + 118 + 84 + 77 + 71
)

# Find which mentions are in both the sample and targets
mention_names <- names(mention_table)
target_values <- targets_mention[mention_names]

# Run chi-square test
test_mention <- chisq.test(as.numeric(mention_table), p = target_values, rescale.p = TRUE)
print(test_mention)

# Print conclusion
if(test_mention$p.value < 0.05) {
  print("ALERTE : Échantillon BIAISÉ sur les Mentions (p < 0.05).")
} else {
  print("OK : Échantillon représentatif sur les Mentions.")
}

# Show details: observed vs expected for each mention
expected_mention <- (target_values / sum(target_values)) * sum(mention_table)
residuals <- (as.numeric(mention_table) - expected_mention) / sqrt(expected_mention)

details <- data.frame(
  Mention = mention_names,
  Observed = as.numeric(mention_table),
  Expected = round(expected_mention, 2),
  Residual = round(residuals, 2)
)

print("--- DÉTAILS PAR MENTION ---")
print(details)

# Identify over and under-represented mentions
print("--- MENTIONS SIGNIFICATIVEMENT DIFFÉRENTES (|Residual| > 1.96) ---")
significant <- details[abs(details$Residual) > 1.96, ]
if(nrow(significant) > 0) {
  print(significant)
} else {
  print("Aucune mention significativement différente.")
}

# ==============================================================================
# CALCULATE WEIGHTS (Redressement)
# ==============================================================================

print(">>> CALCUL DES POIDS <<<")

# Step 1: Calculate weight for SEXE
freq_sexe <- table(data$Sexe) / nrow(data)
weight_sexe <- prop_sexe[names(freq_sexe)] / freq_sexe
sexe_df <- data.frame(
  Sexe = names(weight_sexe),
  w_sexe = as.numeric(weight_sexe)
)

# Step 2: Calculate weight for ANNEE
freq_annee <- table(data$Annee) / nrow(data)
weight_annee <- prop_annee[names(freq_annee)] / freq_annee
annee_df <- data.frame(
  Annee = names(weight_annee),
  w_annee = as.numeric(weight_annee)
)

# Step 3: Calculate weight for MENTION
freq_mention <- table(data$Mention) / nrow(data)
weight_mention <- (targets_mention[names(freq_mention)] / sum(targets_mention)) / freq_mention
mention_df <- data.frame(
  Mention = names(weight_mention),
  w_mention = as.numeric(weight_mention)
)

# Step 4: Merge all weights into the data
data <- merge(data, sexe_df, by = "Sexe")
data <- merge(data, annee_df, by = "Annee")
data <- merge(data, mention_df, by = "Mention", all.x = TRUE)

# Step 5: Calculate final weight (product of all weights)
data$Poids_Final <- data$w_sexe * data$w_annee * data$w_mention

# Export weighted data
write.csv(data, "data_redressee.csv", row.names = FALSE)

# Show sample of results
print("--- SAMPLE OF WEIGHTED DATA ---")
print(head(data[, c("Sexe", "Annee", "Mention", "w_sexe", "w_annee", "w_mention", "Poids_Final")]))