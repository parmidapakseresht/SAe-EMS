data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep=";")

# Filtrer les données pour garder que les réponses en face à face
filtre <- data$X96..DATE_ENREG <= "03/12/2025 15:30:00"
data_filtre <- subset(data, filtre)

# Variables d'intérêt
freq_util_chatgpt <- data_filtre$X36..ChatGPT
freq_util_deepl <- data_filtre$X37..DeepL
freq_util_copilot <- data_filtre$X38..Copilot
freq_util_grammarly <- data_filtre$X39..Grammarly
freq_util_perplexity <- data_filtre$X40..Perplexity
freq_util_autre <- data_filtre$X41..Autre
usages <- data_filtre$X45..RP_PN_IA_usages
raisons <- data_filtre$X47..RP_PN_IA_raisons
limites <- data_filtre$X53..RP_PN_IALimites

# Définition des tailles
n <- nrow(data_filtre)
N <- 5365

# Fonction simple : calcule les proportions par modalité avec variance et IC
proportions_modalites <- function(var_data, sep = ";") {
  # Séparer toutes les réponses multiples
  toutes_modalites <- character(0)
  
  for(i in 1:length(var_data)) {
    if(!is.na(var_data[i]) && var_data[i] != "") {
      modalites <- strsplit(var_data[i], sep)[[1]]
      modalites <- trimws(modalites)
      modalites <- modalites[modalites != ""]
      toutes_modalites <- c(toutes_modalites, modalites)
    }
  }
  
  # Compter les effectifs par modalité
  modalites_uniques <- unique(toutes_modalites)
  effectifs <- numeric(length(modalites_uniques))
  
  for(i in 1:length(modalites_uniques)) {
    effectifs[i] <- sum(toutes_modalites == modalites_uniques[i])
  }
  
  # Calculer les proportions
  proportions <- effectifs / n
  
  # Calculer les variances : Var(p) = (1-f)*p(1-p)/n
  f <- n / N
  variances <- (1 - f) * (proportions * (1 - proportions) / n)
  
  # Calculer les écarts-types
  ecarts_types <- sqrt(variances)
  
  # Calculer les intervalles de confiance à 95% : IC = p ± 1.96 * SE
  ic_inf <- proportions - 1.96 * ecarts_types
  ic_sup <- proportions + 1.96 * ecarts_types
  
  # S'assurer que les bornes sont entre 0 et 1
  for(i in 1:length(ic_inf)) {
    if(ic_inf[i] < 0) ic_inf[i] <- 0
    if(ic_sup[i] > 1) ic_sup[i] <- 1
  }
  
  # Créer le résultat et trier par proportion décroissante
  resultat <- data.frame(
    reponse = modalites_uniques,
    effectif = effectifs,
    proportion = proportions,
    variance = variances,
    ecart_type = ecarts_types,
    ic_inf = ic_inf,
    ic_sup = ic_sup
  )
  
  # Trier par proportion décroissante
  indices_tri <- order(proportions, decreasing = TRUE)
  resultat <- resultat[indices_tri, ]
  
  return(resultat)
}

# Séparateur utilisé dans les fichiers d'export
sep_choix_multiple <- ";"

# Calcul des proportions pour chaque variable
proportions_usages  <- proportions_modalites(usages, sep_choix_multiple)
proportions_raisons <- proportions_modalites(raisons, sep_choix_multiple)
proportions_limites <- proportions_modalites(limites, sep_choix_multiple)

# Aperçu des résultats avec variance et intervalles de confiance
cat("\n=== PROPORTIONS USAGES ===\n")
print(proportions_usages)
cat("\n=== PROPORTIONS RAISONS ===\n")
print(proportions_raisons)
cat("\n=== PROPORTIONS LIMITES ===\n")
print(proportions_limites)

# ============================================================================
# REDRESSEMENT PAR POST-STRATIFICATION - CALCULS SIMPLES
# ============================================================================

# Variables auxiliaires pour le redressement
data_filtre$Annee <- data_filtre$DIP...Annee
data_filtre$Sexe <- data_filtre$Individu...Sexe
data_filtre$Bourse <- data_filtre$Type.bourse..lib..\ndata_filtre$Mention <- data_filtre$Mention

Annee <- data_filtre$Annee
Mention <- data_filtre$Mention
Sexe <- data_filtre$Sexe
Bourse <- data_filtre$Bourse

Annee <- as.character(Annee)
Annee[is.na(Annee)] <- "NA"

Mention <- as.character(Mention)
Mention[is.na(Mention)] <- "NA"

Sexe <- as.character(Sexe)
Sexe[is.na(Sexe)] <- "NA"

Bourse <- as.character(Bourse)
Bourse[is.na(Bourse)] <- "NA"

# Marges de population
pop_BUT1 <- 1736
pop_BUT2 <- 1337
pop_BUT3 <- 1201

pop_G1_Gestion <- 2475
pop_G2_Info    <- 626
pop_G3_Reseaux <- 379
pop_G4_Science <- 939

pop_F <- round(4419 * 0.4082)
pop_M <- 4419 - pop_F

# ============================================================================
# CALCUL DES POIDS POUR CHAQUE VARIABLE DE REDRESSEMENT SÉPARÉMENT
# ============================================================================

# Fonction pour calculer les poids par strate pour une variable donnée
calcul_poids_par_variable <- function(var_auxiliaire, var_nom, Nh_marges, noms_modalites) {
  # Créer les strates basées uniquement sur cette variable
  strates_var <- var_auxiliaire
  strates_uniques_var <- unique(strates_var)
  
  # Calculer nh par strate
  nh_var <- numeric(length(strates_uniques_var))
  for(i in 1:length(strates_uniques_var)) {
    nh_var[i] <- sum(strates_var == strates_uniques_var[i])
  }
  
  # Calculer Nh par strate : Nh = N * (nh / n)
  # Utiliser n (taille totale de l'échantillon) pour garantir la cohérence
  Nh_var <- numeric(length(strates_uniques_var))
  for(i in 1:length(strates_uniques_var)) {
    Nh_var[i] <- N * (nh_var[i] / n)
  }
  
  # Ajuster pour respecter les marges
  for(i in 1:length(strates_uniques_var)) {
    modalite <- strates_uniques_var[i]
    # Trouver l'index dans les marges
    idx_marge <- which(noms_modalites == modalite)
    if(length(idx_marge) > 0) {
      Nh_cible <- Nh_marges[idx_marge]
      # Calculer la somme des Nh pour cette modalité
      somme_Nh <- sum(Nh_var[strates_uniques_var == modalite])
      if(somme_Nh > 0) {
        facteur <- Nh_cible / somme_Nh
        Nh_var[strates_uniques_var == modalite] <- Nh_var[strates_uniques_var == modalite] * facteur
      }
    }
  }
  
  # Normalisation
  somme_Nh_var <- sum(Nh_var)
  Nh_var <- Nh_var * N / somme_Nh_var
  
  # Calculer les poids
  poids_var <- Nh_var / N
  
  # Créer un vecteur de poids pour chaque individu
  poids_individus <- numeric(length(var_auxiliaire))
  for(i in 1:length(var_auxiliaire)) {
    idx_strate <- which(strates_uniques_var == var_auxiliaire[i])
    if(length(idx_strate) > 0) {
      poids_individus[i] <- poids_var[idx_strate]
    } else {
      poids_individus[i] <- 1
    }
  }
  
  return(poids_individus)
}

# Calcul des poids pour chaque variable
poids_annee <- calcul_poids_par_variable(Annee, "Annee", 
                                         c(pop_BUT1, pop_BUT2, pop_BUT3),
                                         c("BUT1", "BUT2", "BUT3"))

poids_mention <- calcul_poids_par_variable(Mention, "Mention", 
                                         c(pop_G1_Gestion, pop_G2_Info, pop_G3_Reseaux, pop_G4_Science),
                                         c("1) Gestion et Commerce", "2) Informatique et Data", 
                                           "3) Réseaux et Télécoms", "4) Sciences et Technologies"))

poids_sexe <- calcul_poids_par_variable(Sexe, "Sexe", 
                                        c(pop_M, pop_F),
                                        c("M", "F"))

poids_bourse <- calcul_poids_par_variable(Bourse, "Bourse",
                                          c(0, 0),
                                          c("Boursier", "Non Boursier"))

# ============================================================================
# CALCUL DES PROPORTIONS REDRESSÉES
# Formule : estimateur_redressé = Σ (Nh/N) × ph
# où ph = nh_modalite / nh_total dans chaque strate
# ============================================================================

# Fonction pour calculer les proportions redressées avec des poids donnés
calcul_proportions_redressees <- function(var_data, poids_var, var_auxiliaire, nom_redressement) {
  # Créer les strates basées sur la variable auxiliaire
  strates_var <- var_auxiliaire
  strates_uniques_var <- unique(strates_var)
  
  # Calculer nh_total par strate (nombre ayant répondu à la question)
  nh_total_par_strate <- numeric(length(strates_uniques_var))
  for(i in 1:length(strates_uniques_var)) {
    s <- strates_uniques_var[i]
    nh_total_par_strate[i] <- 0
    for(j in 1:length(var_data)) {
      if(strates_var[j] == s && !is.na(var_data[j]) && var_data[j] != "") {
        nh_total_par_strate[i] <- nh_total_par_strate[i] + 1
      }
    }
  }
  
  # Séparer les réponses multiples
  toutes_modalites <- character(0)
  indices_individus <- numeric(0)
  
  for(i in 1:length(var_data)) {
    if(!is.na(var_data[i]) && var_data[i] != "") {
      modalites <- strsplit(var_data[i], sep_choix_multiple)[[1]]
      modalites <- trimws(modalites)
      modalites <- modalites[modalites != ""]
      
      for(m in modalites) {
        toutes_modalites <- c(toutes_modalites, m)
        indices_individus <- c(indices_individus, i)
      }
    }
  }
  
  # Calculer les proportions redressées par modalité
  modalites_uniques <- unique(toutes_modalites)
  resultat <- data.frame(
    reponse = character(0),
    effectif_redresse = numeric(0),
    proportion_redressee = numeric(0)
  )
  
  for(m in modalites_uniques) {
    # Trouver tous les individus ayant sélectionné cette modalité
    indices_modalite <- toutes_modalites == m
    indices_ind_modalite <- indices_individus[indices_modalite]
    
    # Calculer la proportion redressée
    proportion_redressee <- 0
    effectif_redresse <- 0
    
    # Pour chaque strate
    for(i in 1:length(strates_uniques_var)) {
      s <- strates_uniques_var[i]
      
      # Compter nh_modalite dans cette strate
      nh_modalite_strate <- 0
      poids_total_strate <- 0
      count_total_strate <- 0
      
      for(j in indices_ind_modalite) {
        if(strates_var[j] == s) {
          nh_modalite_strate <- nh_modalite_strate + 1
        }
      }
      
      # Calculer le poids moyen de la strate
      for(j in 1:length(strates_var)) {
        if(strates_var[j] == s) {
          poids_total_strate <- poids_total_strate + poids_var[j]
          count_total_strate <- count_total_strate + 1
        }
      }
      
      # nh_total dans cette strate
      nh_total_strate <- nh_total_par_strate[i]
      
      # ph = nh_modalite / nh_total
      if(nh_total_strate > 0 && count_total_strate > 0) {
        ph <- nh_modalite_strate / nh_total_strate
        poids_moyen <- poids_total_strate / count_total_strate
        # Ajouter (Nh/N) * ph
        proportion_redressee <- proportion_redressee + poids_moyen * ph
      }
    }
    
    # Effectif redressé : proportion × n (pour cohérence avec la taille de l'échantillon)
    effectif_redresse <- proportion_redressee * n
    
    resultat <- rbind(resultat, data.frame(
      reponse = m,
      effectif_redresse = effectif_redresse,
      proportion_redressee = proportion_redressee
    ))
  }
  
  # Trier par proportion décroissante
  resultat <- resultat[order(resultat$proportion_redressee, decreasing = TRUE), ]
  return(resultat)
}

# Calcul des proportions redressées pour chaque variable de redressement
proportions_usages_redressees_annee <- calcul_proportions_redressees(usages, poids_annee, Annee, "Année")
proportions_raisons_redressees_annee <- calcul_proportions_redressees(raisons, poids_annee, Annee, "Année")
proportions_limites_redressees_annee <- calcul_proportions_redressees(limites, poids_annee, Annee, "Année")

proportions_usages_redressees_mention <- calcul_proportions_redressees(usages, poids_mention, Mention, "Mention")
proportions_raisons_redressees_mention <- calcul_proportions_redressees(raisons, poids_mention, Mention, "Mention")
proportions_limites_redressees_mention <- calcul_proportions_redressees(limites, poids_mention, Mention, "Mention")

proportions_usages_redressees_sexe <- calcul_proportions_redressees(usages, poids_sexe, Sexe, "Sexe")
proportions_raisons_redressees_sexe <- calcul_proportions_redressees(raisons, poids_sexe, Sexe, "Sexe")
proportions_limites_redressees_sexe <- calcul_proportions_redressees(limites, poids_sexe, Sexe, "Sexe")

proportions_usages_redressees_bourse <- calcul_proportions_redressees(usages, poids_bourse, Bourse, "Bourse")
proportions_raisons_redressees_bourse <- calcul_proportions_redressees(raisons, poids_bourse, Bourse, "Bourse")
proportions_limites_redressees_bourse <- calcul_proportions_redressees(limites, poids_bourse, Bourse, "Bourse")

# Affichage des résultats
cat("\n=== PROPORTIONS USAGES REDRESSÉES PAR ANNÉE ===\n")
print(proportions_usages_redressees_annee)
cat("\n=== PROPORTIONS USAGES REDRESSÉES PAR MENTION ===\n")
print(proportions_usages_redressees_mention)
cat("\n=== PROPORTIONS USAGES REDRESSÉES PAR SEXE ===\n")
print(proportions_usages_redressees_sexe)
cat("\n=== PROPORTIONS USAGES REDRESSÉES PAR BOURSE ===\n")
print(proportions_usages_redressees_bourse)

cat("\n=== PROPORTIONS RAISONS REDRESSÉES PAR ANNÉE ===\n")
print(proportions_raisons_redressees_annee)
cat("\n=== PROPORTIONS RAISONS REDRESSÉES PAR MENTION ===\n")
print(proportions_raisons_redressees_mention)
cat("\n=== PROPORTIONS RAISONS REDRESSÉES PAR SEXE ===\n")
print(proportions_raisons_redressees_sexe)
cat("\n=== PROPORTIONS RAISONS REDRESSÉES PAR BOURSE ===\n")
print(proportions_raisons_redressees_bourse)

cat("\n=== PROPORTIONS LIMITES REDRESSÉES PAR ANNÉE ===\n")
print(proportions_limites_redressees_annee)
cat("\n=== PROPORTIONS LIMITES REDRESSÉES PAR MENTION ===\n")
print(proportions_limites_redressees_mention)
cat("\n=== PROPORTIONS LIMITES REDRESSÉES PAR SEXE ===\n")
print(proportions_limites_redressees_sexe)
cat("\n=== PROPORTIONS LIMITES REDRESSÉES PAR BOURSE ===\n")
print(proportions_limites_redressees_bourse)