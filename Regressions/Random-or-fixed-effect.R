# install.packages("fastDummies")
#install.packages('Rcpp')
library(Rcpp)

library(modelr)
library(ggplot2)
library(plm)
library(fastDummies)

library(sandwich)
library(lmtest)


setwd("~/Documents/statApp/git/Stat-app/git")
#setwd("~/Desktop/Stat-App_git/Stat-app/Data")
df_long <- read.csv("StartData_long_without_NA.csv")

# Dummies pour l'éducation 
df_long <- dummy_cols(df_long, select_columns = 'edqual')
df_long <- df_long[, - c(1, length(df_long), length(df_long)-1)]
colnames(df_long)[c((length(df_long)-4):length(df_long))] <- c("edqual0", "edqual1", "edqual2", "edqual3", "edqual4")

# On enlève les individus avec des données manquantes ou avec un salaire négatif
keep <- which(df_long$sclddr != 3 & df_long$sclddr >= 0)
df_kept <- df_long[keep,]


# regression de la santé percue sur les autres variables
# OLS
ols <-lm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
           edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
           sex + age + marital_status + wpactive +
           wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
         data=df_kept)
summary(ols)

# Pour avoir les standard errors clusterisées
m1coeffs_ols <- coeftest(ols, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_ols

# OLS sans les contrôles
ols_without_controls <-lm(srh_hrs ~ sclddr, data=df_kept)
summary(ols_without_controls)

# Pour avoir les standard errors clusterisées
m1coeffs_ols_without_controls <- coeftest(ols_without_controls, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_ols_without_controls



# on calcule la regression avec le modèle des fixed effects et des random effects
fixed <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
               edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
               sex + age + marital_status + wpactive +
               wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
             data=df_kept, index=c("idauniq", 'wave'), model="within")
summary(fixed)

# Certaines variables de contrôles ne sont plus significatives
# log_income_inflation - marital_status - wpactive
# Coef : 0.019

random <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
                edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
                sex + age + marital_status + wpactive +
                wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
              data=df_kept, index=c("idauniq", "wave"), model="random")
summary(random)
# Coef : 0.06

fd <- plm(srh_hrs ~ sclddr + log_income_inflation + ihs_wealth_inflation + 
            edqual0 + edqual1 + edqual2 + edqual3 + edqual4 + 
            sex + age + marital_status + wpactive +
            wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
          data=df_kept, index=c("idauniq"), model="fd")
summary(fd)
# Coef : 0.015


# on applique le test de Haussman pour comparer les modèles
phtest(random, fixed)

# p-value is consistent then the random effect model is inconsistent and 
# we have to choose the fixed effect model