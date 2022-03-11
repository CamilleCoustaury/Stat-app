library(ggplot2)
library(haven)
library(tidyverse)
library(dplyr)
library(readr)

# ------------------- PREPARATION BDD --------------------------
StartData_long <- read.csv("StartData_long_without_NA.csv")

StartData_long$wave <- as.factor(StartData_long$wave)
name_column <- c("sclddr", "eqtotinc_bu_s", "nettotnhw_bu_s", "srh_hrs")


# -------------------- FONCTIONS UTILES ---------------------------------
# Diagrammes en barres :
diagramme_en_barres <- function(BDD, variable, titre) {
  ggplot(data = BDD, aes_string(variable)) +
    geom_bar(fill="steelblue") +
    theme_minimal() +
    labs(title= titre)
}

# Histogramme :
histogramme <- function(bdd, variable) {
  ggplot(data = bdd, aes_string(variable)) + geom_histogram()
}

# Boxplot :
boxplot <- function(BDD, variable_x, variable_y){
  ggplot(BDD, aes_string(x= variable_x, y=variable_y)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=1,
                 outlier.size=1)
}


# ------------------ MISSING VALUES ----------------------
# Créer des tables avec les valeurs manquantes : 
for (i in seq_len(length(name_column))){
  assign(x = paste0("missing_value_", name_column[i]), value = StartData_long %>% rename(wave = wave, var = name_column[i]) %>% filter(var < 0 | var == NA))
}
# Tracer les diagrammes en barres pour toutes les variables :
for (i in seq_len(length(name_column))){
  titre = paste0("Missing values of : ", name_column[i])
  print(diagramme_en_barres(get(paste0("missing_value_", name_column[i])), "wave", titre))
}


# -------------- Transformation des variables ----------------------------------
StartData_long[StartData_long$eqtotinc_bu_s < 0,] <- NA
StartData_long$log_revenu <- log(StartData_long$eqtotinc_bu_s + 0.000000001)
StartData_long[StartData_long$nettotnhw_bu_s < 0,] <- NA
#StartData_long$ihs_wealth <- sinh(StartData_long$nettotnhw_bu_s)
StartData_long$log_wealth <- log(StartData_long$nettotnhw_bu_s + 0.000000001)


# -------------------- HISTOGRAMME -----------------------------
histogramme(StartData_long, "sclddr") #répartition normale centrée en 60
histogramme(StartData_long, "srh_hrs") #répartition normale centrée en 2.5
histogramme(StartData_long, "eqtotinc_bu_s")
histogramme(StartData_long, "log_revenu")
histogramme(StartData_long, "nettotnhw_bu_s")
histogramme(StartData_long, "log_wealth")


#--------------------- BOXPLOT -----------------------------------

# Préparation BDD : la table s'appelle int
int <- StartData_long %>% filter(srh_hrs>0)
int <- int %>% filter(sclddr >=0) 
int$srh_hrs <- as.factor(int$srh_hrs)


  # Différence entre les vagues
boxplot(int, "wave", "sclddr")
boxplot(int, "wave", "log_revenu")
boxplot(int, "wave", "log_wealth")

# Les outliers -20 correspondent aux personnes ayant un revenu nul
# Quand on retire ces valeurs : 
boxplot(int %>% filter(log_revenu > -20), "wave", "log_revenu")
boxplot(int %>% filter(log_revenu > -20), "wave", "log_wealth")

# Les personnes sont de plus en plus riches au fil des vagues


  # Différence entre les Self Rated Health
boxplot(int, "srh_hrs", "sclddr") + facet_grid(. ~ wave)
boxplot(int %>% filter(log_revenu > -20), "srh_hrs", "log_revenu")+ facet_grid(. ~ wave)
boxplot(int %>% filter(log_revenu > -20), "srh_hrs", "log_wealth")+ facet_grid(. ~ wave)


# Pour l'éducation :
tab <- table(int$srh_hrs, int$edqual)[,c(5:10)]
chisq.test(tab)
mosaicplot(tab, las = 3, shade = TRUE)
