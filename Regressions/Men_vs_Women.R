setwd("~/Desktop/Stat-App_git/Stat-app")
df_long <- read.csv("Data/StartData_long_without_NA.csv")

# Dummies pour l'éducation 
df_long <- dummy_cols(df_long, select_columns = 'edqual')
df_long <- df_long[, - c(1, length(df_long), length(df_long)-1)]
colnames(df_long)[c((length(df_long)-4):length(df_long))] <- c("edqual0", "edqual1", "edqual2", "edqual3", "edqual4")

# On enlève les individus avec des données manquantes ou avec un salaire négatif
keep <- which(df_long$wave != 3 & df_long$sclddr >= 0)
df_kept <- df_long[keep,]

# Créer les 2 dataframes Hommes/ Femmes
femme <- df_kept[df_kept$sex == 0,]
homme <- df_kept[df_kept$sex == 1,]


fixed_femme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                     edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                     age + marital_status + wpactive +
                     wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                   data=femme, index=c("idauniq", "wave"), model="within")
summary(fixed_femme)

fixed_homme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                     edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                     age + marital_status + wpactive +
                     wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                   data=homme, index=c("idauniq", "wave"), model="within")
summary(fixed_homme)


random_femme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                age + marital_status + wpactive +
                wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
              data=femme, index=c("idauniq", "wave"), model="random")
summary(random_femme)

random_homme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                      edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                      age + marital_status + wpactive +
                      wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                    data=homme, index=c("idauniq", "wave"), model="random")
summary(random_homme)



fd_femme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                      edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                      age + marital_status + wpactive +
                      wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                    data=femme, index=c("idauniq", "wave"), model="fd")
summary(fd_femme)

fd_homme <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                      edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                      age + marital_status + wpactive +
                      wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                    data=homme, index=c("idauniq", "wave"), model="fd")
summary(fd_homme)

# on applique le test de Haussman pour comparer les modèles
phtest(random_femme, fixed_femme)
phtest(random_homme, fixed_homme)


