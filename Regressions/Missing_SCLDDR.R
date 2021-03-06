library(ggplot2)
library(haven)
library(tidyverse)
library(dplyr)
library(readr)
library(fastDummies)
library(sqldf)

StartData_long <- read.csv("Data/StartData_long_without_NA.csv")
StartData_long$wave <- as.numeric(StartData_long$wave)
df <- StartData_long[StartData_long$wave != 3,]

missing_value_sclddr <- df %>% filter(is.na(sclddr))
not_missing_value_sclddr <- df %>% filter(sclddr >=0)


# On crée une variable missing value (D du cours) :
# 1 si on observe la variable
# 0 sinon
df$missing_value <-  0
df[is.na(df$sclddr),]$missing_value <- 1
df$missing_value <- as.factor(df$missing_value)

# Montrer le lien entre le fait que la variable soit manquante et la santé
tab <- table(df$srh_hrs, df$missing_value)
chisq.test(tab)
mosaicplot(tab, las = 3, shade = TRUE, color = FALSE)

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

# On créé un échantillon de test et un autre d'apprentissage :
alea <- sample(1:nrow(not_missing_value_sclddr), size = floor(nrow(not_missing_value_sclddr)*0.2), replace = FALSE)

not_missing_value_sclddr$ech <- 0
not_missing_value_sclddr[alea,]$ech <- 1

train=not_missing_value_sclddr[not_missing_value_sclddr$ech== 0, -c(length(not_missing_value_sclddr))]
test=not_missing_value_sclddr[not_missing_value_sclddr$ech==1,-c(length(not_missing_value_sclddr))]

# On fixe les paramètres
parametres=rpart.control(minsplit=200 , # nb min qu'il faut dans un noeud pr pvr fr une div
                         minbucket= 150, # une div peut pas générer un noeud enfants avec moins  de __ (minbucket) ind
                         xval= 10, # nb de blocs que l'on peut utiliser dans la validation croisée
                         maxdepth = 30)

# On apprend le modèle sur la base d'apprentissage :
#levels(train$edqual) <- c("0", "1", "2", "3", '4', '5')
model_missing=rpart(sclddr ~ wave + edqual + sex + nonwhite + marital_status +
                      age + wpactive + log_income_inflation + income_inflation +
                      wealth_inflation + ihs_wealth_inflation,
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



# Entrainer le modèle sur TOUTES les observations:
levels(not_missing_value_sclddr$edqual) <- c("0", "1", "2", "3", '4', '5')
model_missing=rpart(sclddr ~ wave + edqual + sex + nonwhite + marital_status +
                      age + wpactive + log_income_inflation + income_inflation +
                      wealth_inflation + ihs_wealth_inflation,
                    data=not_missing_value_sclddr, 
                    control=parametres,
                    method="anova", # régression ou classification
                    parms=list( split='gini'))
rpart.plot(model_missing, type = 2)

# On fait les prédictions sur les valeurs manquantes :
sclddr_new <- (predict(model_missing, missing_value_sclddr))
missing_value_sclddr <- cbind(missing_value_sclddr, sclddr_new)


missing_value_sclddr$sclddr_new <- round(missing_value_sclddr$sclddr_new , 2)
tab <- table(missing_value_sclddr$srh_hrs, missing_value_sclddr$sclddr_new)
chisq.test(tab)
mosaicplot(tab, las = 1, shade = TRUE, xlab = "SRH", ylab = "SSS")
#
missing_value_sclddr$srh_hrs <- as.factor(missing_value_sclddr$srh_hrs)
boxplot(missing_value_sclddr, "srh_hrs", "sclddr", "Social status vs Self Rated Health (by wave)") +
  facet_wrap(. ~ srh_hrs, ncol = 3, labeller=label_both) +
  labs(title="Social status vs Self Rated Health (by wave)",
       x ="Self Rated Health", y = "Subjective Social Status")

# On met les nouvelles valeurs sclddr dans une base de donnée finale :
df_missing_selection <- missing_value_sclddr[, c("idauniq", "sclddr_new", "wave")]
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


ols <-lm(srh_hrs ~ sclddr_new + log_income_inflation + ihs_wealth_inflation + 
           edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
           sex + age + marital_status + wpactive +
           wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
         data=df)
summary(ols)

m1coeffs_ols <- coeftest(ols, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_ols

# OLS sans les contrôles
ols_without_controls <-lm(srh_hrs ~ sclddr_new, data=df)
summary(ols_without_controls)

m1coeffs_ols_without_controls <- coeftest(ols_without_controls, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_ols_without_controls

# Effets fixes
fixed <- plm(srh_hrs ~ sclddr_new + log_income_inflation + ihs_wealth_inflation + 
               edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
               sex + age + marital_status + wpactive +
               wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
             data=df, index=c("idauniq", "wave"), model="within")
summary(fixed)

m1coeffs_random <- coeftest(random, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_random

fd <- plm(srh_hrs ~ sclddr_new + log_income_inflation + ihs_wealth_inflation + 
               edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
               sex + age + marital_status + wpactive +
               wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
             data=df, index=c("idauniq", "wave"), model="fd")
summary(fd)

# Effets aléatoires
random <- plm(srh_hrs ~ sclddr_new + log_income_inflation + ihs_wealth_inflation + 
               edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
               sex + age + marital_status + wpactive +
               wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
             data=df, index=c("idauniq", "wave"), model="random")
summary(random)

# Haussman test
phtest(random, fixed)



