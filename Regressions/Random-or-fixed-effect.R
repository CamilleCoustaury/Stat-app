# install.packages("plm")

library(modelr)
library(ggplot2)
library(plm)

# library(tidyverse)
# library(foreign)
# library(plm)

setwd("~/Documents/statApp/git/Stat-app/data")
df <- read.csv("StartData_wide.csv")
df_long <- read.csv("StartData_long_with_NA.csv")

# test <- function(x) {
#   if (is.na(x) | x<0) {
#     x<- NA
#   }
#   x
# }

keep <- which(df_long$sclddr >= 0 & df_long$srh_hrs >= 0)

df_kept <- df_long[keep,]

ols <-lm(srh_hrs ~ sclddr, data=df_kept)

grid <- data.frame(Intercept=1, sclddr=seq_range(df_kept$sclddr, 10))
grid$pred <- predict(ols,grid)
ggplot(df_kept, aes(sclddr)) +
  geom_point(aes(y = srh_hrs)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

HRS_mean = c()

for (i in 1:20){
  HRS_mean <- c(HRS_mean,mean(df_long[which(df_long$sclddr == 5*i & df_long$srh_hrs >= 0),]$srh_hrs))
}


plot(x = 1:20 * 5, y = HRS_mean, xlab = "sclddr")

fixed <- plm(srh_hrs ~ sclddr, data=df_kept, index=c("wave"), model="within")
random <- plm(srh_hrs ~ sclddr, data=df_kept, index=c("wave"), model="random")
phtest(random, fixed)
# p-value is consistent then the random effect model is inconsistent and we have to choose the fixed effect model
