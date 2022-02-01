# Indiquer le chemin du répertoire
setwd("~/Desktop/Stat'App/Data/Data_Wave1")

# Pour la première utilisation : faire :
#install.packages("haven")
library(haven)

# Mettre le nom de la table entre les guillemets
data_wave1_ifs <- read_dta("wave_1_ifs_derived_variables.dta")
data_wave1_core <- read_dta("wave_1_core_data_v3.dta")

# Variable a garder
variable_id <- c("idauniq", "idahhw1", "perid")
variable_a_garder_core <- c(
                      "hegenh", "heill",
                      "heeye", "hefunc", "hehear",
                      "hesmk", "heska",
                      "heala", "heacta",
                      "sclddr",
                      "wpactw")
variable_a_garder_ifs <- c("sex", "age", "elsa")

data_wave1_ifs <- data_wave1_ifs[ ,c(variable_id, variable_a_garder_ifs)]
data_wave1 <- data_wave1_core[,c(variable_id, variable_a_garder_core)]

# Fusionner les 2 tables :
data_wave1 <- merge(data_wave1, data_wave1_ifs, by = variable_id)




# Mettre les variables en variables catégorielles :
data_wave1$hegenh <- as.factor(data_wave1$hegenh)
data_wave1$heill <- as.factor(data_wave1$heill)
data_wave1$heeye <- as.factor(data_wave1$heeye)
data_wave1$hefunc <- as.factor(data_wave1$hefunc)
data_wave1$hehear <- as.factor(data_wave1$hehear)
data_wave1$hesmk <- as.factor(data_wave1$hesmk)
data_wave1$heska <- as.factor(data_wave1$heska)
data_wave1$sclddr <- as.factor(data_wave1$sclddr)
data_wave1$wpactw <- as.factor(data_wave1$wpactw)
data_wave1$heala <- as.factor(data_wave1$heala)
data_wave1$heacta <- as.factor(data_wave1$heacta)
data_wave1$sex <- as.factor(data_wave1$sex)

summary(data_wave1)
