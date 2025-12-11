install.packages("sampling")
library(sampling)

data <- read.csv("data_filtre.csv", sep=",", stringsAsFactors = FALSE)

names(data)[grep("Sexe", names(data))[1]] <- "Sexe"
names(data)[grep("Annee", names(data))[1]] <- "Annee"
names(data)[grep("bourse", names(data))[1]] <- "Bourse"
names(data)[grep("Mention", names(data))[1]] <- "Mention"

print(">>> CRÉATION DES GROUPES <<<")

assign_group <- function(mention) {
  if (mention %in% c("Gestion des Entreprises et des Administrations", 
                     "Techniques de Commercialisation", 
                     "Carrières Juridiques", 
                     "Carrières Sociales", 
                     "Information - Communication")) {
    return("1) Gestion et Commerce")
    
  } else if (mention %in% c("Informatique", 
                            "Science des Données")) {
    return("2) Informatique et Data")
    
  } else if (mention %in% c("Réseaux et Télécommunications")) {
    return("3) Réseaux et Télécoms")
    
  } else if (mention %in% c("Génie Civil Construction Durable", 
                            "Génie Electrique et Informatique Industrielle", 
                            "Génie Mécanique et Productique")) {
    return("4) Sciences et Technologies")
    
  } else {
    return(NA)
  }
}

data$Groupe <- sapply(data$Mention, assign_group)

data <- subset(data, !is.na(Groupe))

pop_BUT1 <- 1736
pop_BUT2 <- 1337
pop_BUT3 <- 1201

pop_G1_Gestion <- 2475
pop_G2_Info    <- 626
pop_G3_Reseaux <- 234
pop_G4_Science <- 939

pop_F <- round(4274 * 0.4082)
pop_M <- 4274 - pop_F

print("--- TEST CHI2 : SEXE ---")
sexe_obs <- table(data$Sexe)
sexe_theo_prob <- c("F" = 0.4082, "M" = 0.5918)
test_sexe <- chisq.test(sexe_obs, p = sexe_theo_prob[names(sexe_obs)])
print(test_sexe)
if (test_sexe$p.value < 0.05) {
  print("CONCLUSION: Sexe -> Échantillon BIAISÉ (p < 0.05) : redressement recommandé")
} else {
  print("CONCLUSION: Sexe -> Échantillon REPRÉSENTATIF (p >= 0.05) : redressement non nécessaire")
}



print("--- TEST CHI2 : GROUPES ---")
groupe_obs <- table(data$Groupe)
groupe_theo_counts <- c(
  "1) Gestion et Commerce"      = pop_G1_Gestion,
  "2) Informatique et Data"     = pop_G2_Info,
  "3) Réseaux et Télécoms"      = pop_G3_Reseaux,
  "4) Sciences et Technologies" = pop_G4_Science
)
groupe_theo_prob <- groupe_theo_counts / sum(groupe_theo_counts)

common_groups <- intersect(names(groupe_obs), names(groupe_theo_prob))
test_groupe <- chisq.test(groupe_obs[common_groups], p = groupe_theo_prob[common_groups])
print(test_groupe)
if (test_groupe$p.value < 0.05) {
  print("CONCLUSION: Groupes -> Échantillon BIAISÉ (p < 0.05) : redressement recommandé")
} else {
  print("CONCLUSION: Groupes -> Échantillon REPRÉSENTATIF (p >= 0.05) : redressement non nécessaire")
}

print("--- TEST CHI2 : ANNÉE ---")
annee_obs <- table(data$Annee)
annee_theo_counts <- c("BUT1" = pop_BUT1, "BUT2" = pop_BUT2, "BUT3" = pop_BUT3)
annee_theo_prob <- annee_theo_counts / sum(annee_theo_counts)
common_annees <- intersect(names(annee_obs), names(annee_theo_prob))
test_annee <- chisq.test(annee_obs[common_annees], p = annee_theo_prob[common_annees])
print(test_annee)
if (test_annee$p.value < 0.05) {
  print("CONCLUSION: Année -> Échantillon BIAISÉ (p < 0.05) : redressement recommandé")
} else {
  print("CONCLUSION: Année -> Échantillon REPRÉSENTATIF (p >= 0.05) : redressement non nécessaire")
}

print("--- TEST CHI2 : BOURSE ---")
bourse_obs <- table(data$Bourse)
bourse_obs_prop <- bourse_obs / sum(bourse_obs)
bourse_theo_prob <- bourse_obs_prop
test_bourse <- chisq.test(bourse_obs, p = bourse_theo_prob[names(bourse_obs)])
print(test_bourse)
if (test_bourse$p.value < 0.05) {
  print("CONCLUSION: Bourse -> Échantillon BIAISÉ (p < 0.05) : redressement recommandé")
} else {
  print("CONCLUSION: Bourse -> Échantillon REPRÉSENTATIF (p >= 0.05) : redressement non nécessaire")
}

print(">>> CALAGE <<<")

Sexe_F <- as.numeric(data$Sexe == "F")
Sexe_M <- as.numeric(data$Sexe == "M")

Annee_1 <- as.numeric(data$Annee == "BUT1")
Annee_2 <- as.numeric(data$Annee == "BUT2")
Annee_3 <- as.numeric(data$Annee == "BUT3")

Groupe_1 <- as.numeric(data$Groupe == "1) Gestion et Commerce")
Groupe_2 <- as.numeric(data$Groupe == "2) Informatique et Data")
Groupe_3 <- as.numeric(data$Groupe == "3) Réseaux et Télécoms")
Groupe_4 <- as.numeric(data$Groupe == "4) Sciences et Technologies")

X <- data.frame(Sexe_F, Sexe_M, 
                Annee_1, Annee_2, Annee_3, 
                Groupe_1, Groupe_2, Groupe_3, Groupe_4)

popTotals <- c(
  pop_F, pop_M,              
  pop_BUT1, pop_BUT2, pop_BUT3, 
  pop_G1_Gestion, pop_G2_Info, pop_G3_Reseaux, pop_G4_Science
)

N_total <- 4274
n_sample <- nrow(data)
d_weights <- rep(N_total / n_sample, n_sample)

g_weights <- calib(Xs = X, 
                   d = d_weights, 
                   total = popTotals, 
                   method = "raking",
                   description = TRUE)

data$Poids_Calib <- g_weights * d_weights

print("--- Vérification des totaux après redressement ---")
check_sexe <- tapply(data$Poids_Calib, data$Sexe, sum)
check_annee <- tapply(data$Poids_Calib, data$Annee, sum)
check_groupe <- tapply(data$Poids_Calib, data$Groupe, sum)

print("Sexe (Cible vs Calé) :")
print(check_sexe)
print("Année (Cible vs Calé) :")
print(check_annee)
print("Groupe (Cible vs Calé) :")
print(check_groupe)

write.csv(data, "data_redressee_calib.csv", row.names = FALSE)
print("Fichier 'data_redressee_calib.csv' généré avec succès.")