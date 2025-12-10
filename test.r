# Chargement des données
data <- read.csv("Extractionfusion_pour_CB_Termine.csv", sep=";")

# Filtrer les données pour garder que les réponses en face à face
filtre <- data$X96..DATE_ENREG <= "03/12/2025 15:30:00"
data_filtre <- subset(data, filtre)
# Il faut charger le package si ce n'est pas déjà fait
# install.packages("writexl")
# library(writexl)
# 
# write_xlsx(data_filtre, 
#            path = "C:/Users/aladi/OneDrive/Documents/data_filtre.xlsx")

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

# Calcul des proportions par modalité pour les questions à choix multiples
# On sépare chaque modalité avant de compter afin d'éviter les combinaisons complètes
library(dplyr)
library(tidyr)
library(stringr)

# Fonction utilitaire : découpe les réponses multiples, compte et calcule les proportions
proportions_modalites <- function(df, col, sep = ";") {
  df %>%
    select(reponse = {{ col }}) %>%
    filter(!is.na(reponse), reponse != "") %>%
    separate_rows(reponse, sep = sep) %>%
    mutate(reponse = str_trim(reponse)) %>%
    count(reponse, name = "effectif") %>%
    mutate(proportion = effectif / sum(effectif)) %>%
    arrange(desc(proportion))
}

# Séparateur utilisé dans les fichiers d'export (ajuster si besoin, ex. "," ou "|")
sep_choix_multiple <- ";"

proportions_usages  <- proportions_modalites(data_filtre, X45..RP_PN_IA_usages, sep_choix_multiple)
proportions_raisons <- proportions_modalites(data_filtre, X47..RP_PN_IA_raisons, sep_choix_multiple)
proportions_limites <- proportions_modalites(data_filtre, X53..RP_PN_IALimites, sep_choix_multiple)

# Aperçu des résultats
print(proportions_usages)
print(proportions_raisons)
print(proportions_limites)
