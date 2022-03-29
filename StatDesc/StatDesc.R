library(ggplot2)
library(haven)
library(tidyverse)
library(dplyr)
library(readr)

# ------------------- PREPARATION BDD --------------------------
#StartData_long <- read.csv("Data/StartData_long_without_NA.csv")
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
histogramme <- function(bdd, variable, titre) {
  ggplot(data = bdd, aes_string(variable)) +
    geom_histogram()+
    labs(title= titre)
}

# Boxplot :
boxplot <- function(BDD, variable_x, variable_y, titre){
  ggplot(BDD, aes_string(x= variable_x, y=variable_y)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=1,
                 outlier.size=1) +
    labs(title= titre)
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

# -------------------- HISTOGRAMME -----------------------------
pdf("StatDesc/Histograms.pdf")
histogramme(StartData_long, "sclddr", "Social Status") +
  facet_wrap(. ~ wave, ncol = 3)
histogramme(StartData_long, "srh_hrs", "Self Rated Health") +
  facet_wrap(. ~ wave, ncol = 3)
histogramme(StartData_long%>% filter(log_revenu > 0.01), "log_revenu", "Log(income)") +
  facet_wrap(. ~ wave, ncol = 3)
histogramme(StartData_long , "ihs_wealth", "Ifs(wealth)") +
  facet_wrap(. ~ wave, ncol = 3)
dev.off()


#--------------------- BOXPLOT -----------------------------------

# Préparation BDD : la table s'appelle int
int <- StartData_long %>% filter(srh_hrs>0)
int <- int %>% filter(sclddr >=0) 
int$srh_hrs <- as.factor(int$srh_hrs)


  # Différence entre les vagues
pdf("StatDesc/Boxplots_Wave.pdf")
boxplot(int, "wave", "sclddr", "Sclddr vs Wave")
boxplot(int, "wave", "log_revenu", "Log(Income) vs Wave")

# Les outliers -20 correspondent aux personnes ayant un revenu nul
# Quand on retire ces valeurs : 
boxplot(int %>% filter(log_revenu > 0), "wave", "log_revenu", "Log(Income) (without null income) vs Wave")

boxplot(int, "wave", "ihs_wealth", "Ihs(Wealth) vs Wave")
boxplot(int %>% filter(ihs_wealth >=0 ), "wave", "ihs_wealth", "Ihs(Wealth) (without negative wealth) vs Wave")
dev.off()
# Les personnes sont de plus en plus riches au fil des vagues


  # Différence entre les Self Rated Health
pdf(file = "StatDesc/Boxplots_health.pdf", height=10,width=10)
boxplot(int, "srh_hrs", "sclddr", "Social status vs Self Rated Health (by wave)") +
  facet_wrap(. ~ wave, ncol = 3)
boxplot(int %>% filter(log_revenu > -20), "srh_hrs", "log_revenu", "Log(Income) vs Self Rated Health (by wave)") +
  facet_wrap(. ~ wave, ncol = 3)
boxplot(int %>% filter(log_revenu > -20), "srh_hrs", "ihs_wealth", "Ihs(Wealth) vs Self Rated Health (by wave)")+
  facet_wrap(. ~ wave, ncol = 3)
#on ferme le graphique
dev.off()

# Pour l'éducation :
tab <- table(int$srh_hrs, int$edqual)[,c(5:10)]
chisq.test(tab)
mosaicplot(tab, las = 3, shade = TRUE)
