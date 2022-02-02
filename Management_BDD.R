# Indiquer le chemin du répertoire
setwd("~/Desktop/Stat'App/Data/Data_Wave1")

# Pour la première utilisation : faire :
#install.packages("haven")
library(haven)

# Mettre le nom de la table entre les guillemets
data_wave1_ifs <- read_dta("wave_1_ifs_derived_variables.dta")
data_wave1_core <- read_dta("wave_1_core_data_v3.dta")
data_wave1_finance <- read_dta("wave_1_financial_derived_variables.dta")
data_wave1_pension <- read_dta("wave_1_pension_wealth_v2.dta")
data_wave1_pension_grid <- read_dta("wave_1_pension_grid.dta")

# Variable a garder
var_id <- c("idauniq", "idahhw1", "perid")
var_ifs <- c("sex", "age", "elsa")
var_finance <- c("sinc_bu_i", "ppen_bu_i", "spen_r_i")
var_core <- c("hegenh", "sclddr", "wpactw", "iawork", "iasinc", "iaspen", "iapam")


data_wave1_ifs <- data_wave1_ifs[ ,c(var_id, var_ifs)]
data_wave1_finance <- data_wave1_finance[ ,c(var_id, var_obj_ses_finance)]
data_wave1 <- data_wave1_core[,c(var_id, var_core)]

# Fusionner les 2 tables :
data_wave1 <- merge(data_wave1, data_wave1_ifs, by = var_id)
data_wave1 <- merge(data_wave1, data_wave1_fin, by = var_id)


# Mettre les variables en variables catégorielles :
var_discrete <- c("hegenh", "sex", "elsa")
data_wave1[c(var_discrete)] <- lapply(data_wave1[c(var_discrete)], as.factor)

summary(data_wave1)
