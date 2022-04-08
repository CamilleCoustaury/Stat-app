library(ggplot2)
library(haven)
library(tidyverse)
library(dplyr)
library(readr)
library(fastDummies)

missing_value_sclddr <- StartData_long %>% rename(wave = wave, var = sclddr) %>% filter(is.na(var))

df <- StartData_long
df$wave <- as.numeric(df$wave)


# On crée une variable missing value (D du cours) :
# 1 si on observe la variable
# 0 sinon
df$missing_value <-  0
df[is.na(df$sclddr),]$missing_value <- 1
df$missing_value <- as.factor(df$missing_value)

# Montrer le lien entre le fait que la variable soit manquante et la santé
tab <- table(df$sex, df$missing_value)
chisq.test(tab)
mosaicplot(tab, las = 3, shade = TRUE)

# Parmi les gens en mauvaise santé, il y a une surréprésentation de valeurs manquantes
variable_test <- c("edqual", "sex", "nonwhite", "marital_status", "wpactive",
                   "wave1", "wave2", "wave4", "wave5", "wave6", "wave7", "wave8", "wave9")
variable_test2 <- c("srh_hrs", variable_test)

# Créer une liste de p_valeur du test d'indépendance
p_value_chi_test <- c()
for (i in 1:length(variable_test2)){
  tab <- table(df[, c(variable_test2[i])], df$missing_value)
  p_value <- chisq.test(tab)$p.value
  p_value_chi_test[i] <- p_value
  names(p_value_chi_test)[i] <- variable_test2[i]
}
p_value_chi_test

# ------------------- METHODE DES ARBRES CART -------------------------------
library(rpart)
library(rpart.plot)

# Creation d'un df sans valeur manquante :
df_without_missing <- df[df$missing_value == 0, c(variable_test, "age", "log_income_inflation", "ihs_wealth_inflation", "sclddr")]

# Creation d'un df avec seulement les valeurs manquantes
df_missing <- df[df$missing_value == 1, c(variable_test, "age", "log_income_inflation", "ihs_wealth_inflation", "idauniq", "wave")]

# On créé un échantillon de test et un autre d'apprentissage :
alea <- sample(1:nrow(df_without_missing), size = floor(nrow(df_without_missing)*0.2), replace = FALSE)

df_without_missing$ech <- 0
df_without_missing[alea,]$ech <- 1

train=df_without_missing[df_without_missing$ech== 0, -c(length(df_without_missing))]
test=df_without_missing[df_without_missing$ech==1,-c(length(df_without_missing))]

# On fixe les paramètres
parametres=rpart.control(minsplit=200 , # nb min qu'il faut dans un noeud pr pvr fr une div
                         minbucket= 150, # une div peut pas générer un noeud enfants avec moins  de __ (minbucket) ind
                         xval= 10, # nb de blocs que l'on peut utiliser dans la validation croisée
                         maxdepth = 30)

# On apprend le modèle sur la base d'apprentissage :
model_missing=rpart(sclddr ~ .,
                  data=train, 
                  control=parametres,
                  method="anova", # régression ou classification
                  parms=list( split='gini')) # par défaut, c'est Gini

rpart.plot(model_missing, type = 2)


# Faire les prédications sur l'échantillon de test:
pred <- (predict(model_missing, test))
test <- cbind(test, pred)
test$err_cart <- (test$pred - test$sclddr)**2

mean(test$err_cart)


# On fait les prédictions sur les valeurs manquantes :
sclddr_new <- (predict(model_missing, df_missing))
df_missing <- cbind(df_missing, sclddr_new)

# On met les nouvelles valeurs sclddr dans une base de donnée finale :
df_missing_selection <- df_missing[, c("idauniq", "sclddr_new", "wave")]
df_completed <- merge( x=df, y=df_missing_selection, by=c("idauniq", "wave"), all.x=TRUE)

df_completed[is.na(df_completed$sclddr_new),]$sclddr_new <- df_completed[!is.na(df_completed$sclddr),]$sclddr

write_csv(df_completed, "Regressions/BDD_sclddr_modifie.csv")


# ----------------------- REGRESSIONS ------------------------------------------
library(plm)
library(fastDummies)
library(lmtest)

df <- read_csv("Regressions/BDD_sclddr_modifie.csv")
prep_data_reg <- function(df_long){
  df_long <- dummy_cols(df_long, select_columns = 'edqual')
  df_long <- df_long[, - c(length(df_long), length(df_long)-1)]
  colnames(df_long)[c((length(df_long)-4):length(df_long))] <- c("edqual0", "edqual1", "edqual2", "edqual3", "edqual4")
  return(df_long)
}

df <- prep_data_reg(df)


ols <-lm(srh_hrs ~ sclddr_new+log_income+ihs_wealth+edqual0+edqual1+edqual2+edqual3+edqual4+sex+age+marital_status+wpactive,
         data=df)
summary(ols)

# OLS sans les contrôles
ols_without_controls <-lm(srh_hrs ~ sclddr_new, data=df)
summary(ols_without_controls)

# Effets fixes
within <- plm(srh_hrs ~ sclddr_new + log_revenu + ihs_wealth +age+marital_status+wpactive, data=df, index=c("idauniq", 'wave'), model="within")
summary(within)
fd <- plm(srh_hrs ~ sclddr_new + log_revenu + ihs_wealth + edqual0 + edqual1 + edqual2 + edqual3 + edqual4,
          data=df, index=c("idauniq", "wave"), model="fd")
summary(fd)




