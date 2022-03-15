# install.packages("fastDummies")
install.packages('Rcpp')
library(Rcpp)

library(modelr)
library(ggplot2)
library(plm)
library(fastDummies)

library(sandwich)
library(lmtest)


setwd("~/Documents/statApp/git/Stat-app/git")
#setwd("~/Desktop/Stat-App_git/Stat-app/Data")
df <- read.csv("StartData_wide.csv")
df_long <- read.csv("StartData_long_without_NA.csv")

# Dummies pour l'éducation 
df_long <- dummy_cols(df_long, select_columns = 'edqual')
df_long <- df_long[, - c(1, length(df_long), length(df_long)-1)]
colnames(df_long)[c(10:14)] <- c("edqual0", "edqual1", "edqual2", "edqual3", "edqual4")

# On enlève les individus avec des données manquantes ou avec un salaire négatif
keep <- which(df_long$sclddr >= 0 & df_long$srh_hrs >= 0 & df_long$eqtotinc_bu_s >= 0)
df_kept <- df_long[keep,]


# regression de la santé percue sur les autres variables
# OLS
ols <-lm(srh_hrs ~ sclddr + log_revenu + ihs_wealth + edqual0 + edqual1 + edqual2 + edqual3 + edqual4, data=df_kept)
summary(ols)

# OLS sans les contrôles
ols_without_controls <-lm(srh_hrs ~ sclddr+ log_revenu, data=df_kept)
summary(ols_without_controls)

# Pour avoir les standard errors clusterisées
m1coeffs_cl <- coeftest(ols_without_controls, vcov. = vcovHC, cluster = ~idauniq)
m1coeffs_cl



# on calcule la regression avec le modèle des fixed effects et des random effects
fixed <- plm(srh_hrs ~ sclddr + log_revenu + ihs_wealth + edqual0 + edqual1 + edqual2 + edqual3 + edqual4, data=df_kept, index=c("idauniq", 'wave'), model="within")
summary(fixed)

random <- plm(srh_hrs ~ sclddr + log_revenu + ihs_wealth + edqual0 + edqual1 + edqual2 + edqual3 + edqual4,data=df_kept, index=c("idauniq", "wave"), model="random")
summary(random)


fd <- plm(srh_hrs ~ sclddr + log_revenu + ihs_wealth + edqual0 + edqual1 + edqual2 + edqual3 + edqual4,
          data=df_kept, index=c("idauniq"), model="fd")


# on applique le test de Haussman pour comparer les modèles
phtest(random, fixed)

# p-value is consistent then the random effect model is inconsistent and 
# we have to choose the fixed effect model