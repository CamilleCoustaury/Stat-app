#Stat des

library(ggplot2)
library(haven)
library(tidyverse)

StartData_wide <- read.csv("StartData_wide.csv")
StartData_long <- read.csv("StartData_long_without_NA.csv")

sum(is.na(StartData_wide$sclddr1)) # 7708 valeurs manquantes
sum(is.na(StartData_wide$sclddr2)) # 10375 valeurs manquantes
sum(is.na(StartData_wide$sclddr3)) # 10036 valeurs manquantes
sum(is.na(StartData_wide$sclddr4)) # 8757 valeurs manquantes
sum(is.na(StartData_wide$sclddr5)) # 9533 valeurs manquantes
sum(is.na(StartData_wide$sclddr6)) # 9206 valeurs manquantes
sum(is.na(StartData_wide$sclddr7)) # 10141 valeurs manquantes
sum(is.na(StartData_wide$sclddr8)) # 11362 valeurs manquantes
sum(is.na(StartData_wide$sclddr9)) # 11071 valeurs manquantes (du au renouvellement de la BDD)

#Moyenne de la variable de bien-être perçue (sur 100, sans compter les valeurs manquantes et les chiffres<0)
mean_withoutnanandnegative<-function(column){
  compteur=0
  s=0
  for (i in column){
    if (is.na(i)==FALSE){
      if (i>0){
        s=s+i
        compteur=compteur+1}}}
  return(s/compteur)}

mean_sclddr <- c(mean_withoutnanandnegative(StartData_wide$sclddr1),mean_withoutnanandnegative(StartData_wide$sclddr2),
                 mean_withoutnanandnegative(StartData_wide$sclddr3),mean_withoutnanandnegative(StartData_wide$sclddr4),
                 mean_withoutnanandnegative(StartData_wide$sclddr5),mean_withoutnanandnegative(StartData_wide$sclddr6),
                 mean_withoutnanandnegative(StartData_wide$sclddr7),mean_withoutnanandnegative(StartData_wide$sclddr8),
                 mean_withoutnanandnegative(StartData_wide$sclddr9))
mean_sclddr_tot <- mean(mean_sclddr)

#Moyenne de la variable de santé (sur 5, sans compter les valeurs manquantes et les chiffres<0)
#Attention on enlève wave 3 car pas mesurée
mean_srh_hrs <- c(mean_withoutnanandnegative(StartData_wide$srh_hrs1),mean_withoutnanandnegative(StartData_wide$srh_hrs2),
                 mean_withoutnanandnegative(StartData_wide$srh_hrs4),
                 mean_withoutnanandnegative(StartData_wide$srh_hrs5),mean_withoutnanandnegative(StartData_wide$srh_hrs6),
                 mean_withoutnanandnegative(StartData_wide$srh_hrs7),mean_withoutnanandnegative(StartData_wide$srh_hrs8),
                 mean_withoutnanandnegative(StartData_wide$srh_hrs9))
mean_srh_hrs_tot <- mean(mean_srh_hrs)


# Pour réaliser des histogrammes :
histogramme <- function(bdd, variable) {
  ggplot(data = bdd, aes_string(variable)) + geom_histogram()
}

histogramme(StartData_wide, "sclddr1")
histogramme(StartData_wide, "srh_hrs9")

#travail pour le 9/03
install.packages(c("readr", "dplyr"))
library(dplyr)
library(readr)
StartData_long = read_csv(file = "/Users/adelemoreau/Desktop/ENSAE/Stat'App/BDD/StartData_long_without_NA.csv")

#histogramme des données
histogramme(StartData_long, "sclddr") #répartition normale centrée en 60
histogramme(StartData_long, "srh_hrs") #répartition normale centrée en 2.5
histogramme(StartData_long, "totinc_bu_s")

#listes de sclddr et srh_hrs
social_status = StartData_long$sclddr
health = StartData_long$srh_hrs

#enlever quand valeurs négatives ds une des 2 listes
social_status_positive <- list()

for (i in social_status){
  if (i>=0){
    social_status_positive <- append(social_status_positive,i)
  }
}

social_status_positive


# -------------------- FONCTIONS UTILES ---------------------------------
# Diagrammes en barres :
diagramme_en_barres <- function(BDD, variable, titre) {
  ggplot(data = BDD, aes_string(variable)) +
    geom_bar(fill="steelblue") +
    theme_minimal() +
    labs(title= titre)
}

# Preparation BDD
StartData_long$wave <- as.factor(StartData_long$wave)
name_column <- c("sclddr", "eqtotinc_bu_s", "nettotnhw_bu_s", "srh_hrs")

# ------------------ MISSING VALUES ----------------------
# Créer des tables avec les valeurs manquantes : 
for (i in seq_len(length(name_column))){
  assign(x = paste0("missing_value_", name_column[i]), value = StartData_long %>% rename(wave = wave, var = name_column[i]) %>% filter(var < 0))
}
# Tracer les diagrammes en barres pour toutes les variables :
for (i in seq_len(length(name_column))){
  titre = paste0("Missing values of : ", name_column[i])
  print(diagramme_en_barres(get(paste0("missing_value_", name_column[i])), "wave", titre))
}

# Pas de valeurs manquantes


#--------------------- BOXPLOT -----------------------------------
library(ggplot2)

StartData_long$sclddr <- as.factor(StartData_long$sclddr)
head(StartData_long)

#Box plots basiques
p <- ggplot(StartData_long, aes(x=sclddr, y=srh_hrs)) + geom_boxplot()
p

# Tourner le box plot
p + coord_flip()

# Box plot de type notch
ggplot(StartData_long, aes(x=sclddr, y=srh_hrs)) + geom_boxplot(notch=TRUE)
# Changer la couleur, la forme et la taille des 
# valeurs atypiques (outliers)
ggplot(StartData_long, aes(x=sclddr, y=srh_hrs)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,
               outlier.size=1)


# Statut social
int <- StartData_long %>% filter(srh_hrs>0 & sclddr >=0) 
ggplot(int, aes(x=as.factor(srh_hrs), y=sclddr)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,
               outlier.size=1)


