# install.packages("plm")

library(modelr)
library(ggplot2)
library(plm)


setwd("~/Documents/statApp/git/Stat-app/git")
df <- read.csv("StartData_wide.csv")
df_long <- read.csv("StartData_long_without_NA.csv")

# On enlève les individus avec des données manquantes ou avec un salaire négatif
keep <- which(df_long$sclddr >= 0 & df_long$srh_hrs >= 0 & df_long$edqual >= 0 & df_long$eqtotinc_bu_s >= 0)
df_kept <- df_long[keep,]

# la fonction Inverse Hyperbolic Sine
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

# On applique les transformations aux variables de revenu et de richesse
df_kept$log_inc <- log(df_kept$eqtotinc_bu_s + 1)
df_kept$ihs_wealth <- ihs(df_kept$nettotnhw_bu_s)

# regression de la santé percue sur les autres variables
ols <-lm(srh_hrs ~ sclddr + log_inc + ihs_wealth + edqual, data=df_kept)

# on calcule la regression avec le modèle des fixed effects et des random effects
fixed <- plm(srh_hrs ~ sclddr + log_inc + ihs_wealth + edqual, data=df_kept, index=c("idauniq"), model="within")
random <- plm(srh_hrs ~ sclddr + log_inc + ihs_wealth + edqual, data=df_kept, index=c("idauniq"), model="random")

# on applique le test de Haussman pour comparer les modèles
phtest(random, fixed)

# p-value is consistent then the random effect model is inconsistent and 
# we have to choose the fixed effect model