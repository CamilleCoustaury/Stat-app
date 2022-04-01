
#### This file is used to create a file for analyses ####
#--------------------------------------------------------
#IMPORTATIONS + FILE DIRECTORY

#install.packages("haven") only for first use
library(haven)
library(Hmisc)
library(dplyr)
library(sqldf)
library(tidyr)
library(tidyverse)

#set the working directory, namely to the folder where we keep the data set.
# WARNING : must be change according to your own directory
#setwd("~/Documents/statApp/git/Stat-app/data")
#setwd("~/Desktop/Stat-App_git/Data")
setwd("/Users/camille/Desktop/COURS ENSAE/Stat-app/data")


#********
##STEP 1 : create an index file, a file that only contains the unique ID's 
#of everyone who has ever participated in the survey
#********

BDD_names_core = c("wave_1_core_data_v3.dta", "wave_2_core_data_v4.dta", "wave_3_elsa_data_v4.dta", "wave_4_elsa_data_v3.dta",
                   "wave_5_elsa_data_v4.dta", "wave_6_elsa_data_v2.dta", "wave_7_elsa_data.dta",
                   "wave_8_elsa_data_eul_v2.dta", "wave_9_elsa_data_eul_v1.dta")

# It takes time as data sets are big
for (i in seq_len(9)){
  assign(x = paste0("wave", i, "_core"), value = read_dta(BDD_names_core[i]))
}

for (i in seq_len(9)){
  assign(x = paste0("index", i), value = get(paste0("wave", i, "_core"))["idauniq"])
}

#create an index file, a file that only contains the unique ID's 
#of everyone who has ever participated in the survey
index = wave1_core["idauniq"]
for (i in 2:9){
  index = bind_rows(index, get(paste0("index", i)))
}

#Drop duplicates 
index = distinct(index)


#--------------------------------------------------------
# ADD SCLDDR

# # ajout de la variable maritale (1 si en couple, 0 sinon)
temporaire <- as.integer(wave1_core[,c("dimar")] %in% c(2,3))
wave1_core$marital_status <- temporaire

for (i in 2:9){
  col <- data.frame(as.integer(get(paste0("wave", i, "_core"))$couple %in% c(1,2)))
  names(col) <- "marital_status"
  assign(x = paste0("wave", i, "_core"), value = cbind(get(paste0("wave", i, "_core")), col))
}


# We start with the first wave:
df = merge( x=index, y=wave1_core[c("idauniq","sclddr", "marital_status")], by="idauniq",all.x=TRUE)

#The merge command here adds the variable sclddr uniquely to each 
#participant in the data set, drawing on the wave 2 core file. We should keep
#in mind that the sclddr variable only comes from wave 2, and so we will
#rename it accordingly:
names(df)[names(df) == 'sclddr'] <- 'sclddr1'
names(df)[names(df) == 'marital_status'] <- 'marital_status1'


#The data set now contains two variables, the ID variable idauniq and 
#the subjective SES variable from wave 1 sclddr1.
#We'll add the remaining waves of sclddr accordingly:
for (i in 2:9){
  df = merge( x=df, y=get(paste0("wave", i, "_core"))[c("idauniq","sclddr", "marital_status")], by="idauniq",all.x=TRUE)
  names(df)[names(df) == 'sclddr'] <- paste0('sclddr', i)
  names(df)[names(df) == 'marital_status'] <- paste0('marital_status', i)
}



#--------------------------------------------------------
# ADD SELF-RATED HEALTH VARIABLE
# Next, we'll add the self-rated health variable from the set of files containing variables
# that have been prepared by the IFS.

BDD_names_IFS = numeric()

# A part la wave 8, les noms des table sont les mêmes, on peut faire une boucle :
for (i in seq_len(9)){
  BDD_names_IFS[i] <- paste0("wave_", i, "_ifs_derived_variables.dta")
}
# Change for wave 8 :
BDD_names_IFS[8] <- "wave_8_elsa_ifs_dvs_eul_v1.dta"

# Importations of the data sets :
for (i in seq_len(9)){
  assign(x = paste0("wave", i, "_IFS"), value = read_dta(BDD_names_IFS[i]))
}

# --- Pour la vague 3 : 
# Prendre la variable srh_hse
wave3_IFS <- wave3_IFS[c("idauniq", "srh_hse", "edqual", "sex", "age", "wpactive")]
wave3_IFS$srh_hrs <- wave3_IFS$srh_hse


# Keep the variable srh_hrs and education
var_educ_health <- c("idauniq","edqual", "sex", "age", "srh_hrs", "wpactive")

for (i in 1:9){
  df = merge( x=df, y=get(paste0("wave", i, "_IFS"))[c("idauniq","srh_hrs", "edqual", "age", "sex", "wpactive")], by="idauniq",all.x=TRUE)
  # For each vairable in var_educ_health, rename the variable
  for (elem in var_educ_health[2:length(var_educ_health)]){
    names(df)[names(df) == elem] <- paste0(elem, i)
  }
}

# CHANGER LES MODALITES DANS LES VAGUES 
# #on regroupe 4 et 5 pour la vague 3 
# df[df$srh_hrs3 < 0 & !is.na(df$srh_hrs3),]$srh_hrs3 <- NA
# df$srh_hrs3 <- as.factor(df$srh_hrs3)
# df$srh_hrs3<- fct_collapse(df$srh_hrs3, "1" = "1",
#                            "2" = "2",
#                            "3" = "3",
#                            "4" = c("4", "5"))
# df$srh_hrs3 <- as.numeric(as.character(df$srh_hrs3))
# 
# #on regroupe 1 et 2 pour les autres vagues et on décale vers le haut 
# names<-c('srh_hrs1','srh_hrs2','srh_hrs4','srh_hrs5','srh_hrs6','srh_hrs7','srh_hrs8','srh_hrs9')
# for (i in names){
#   df[,i][df[,i]<0 & !is.na(df[,i])]<-NA
#   df[,i] <- as.factor(df[,i])
#   df[,i]<- fct_collapse(df[,i], "1" = c("1","2"),
#                         "2" = "3",
#                         "3" = "4",
#                         "4" = "5")
#   df[,i] <- as.numeric(as.character(df[,i]))
# }

#on renverse les modalités 
names<-c('srh_hrs1','srh_hrs2','srh_hrs3','srh_hrs4','srh_hrs5','srh_hrs6','srh_hrs7','srh_hrs8','srh_hrs9')
for (i in names){
  df[,i][df[,i]<0 & !is.na(df[,i])]<-NA
  df[,i] <- as.factor(df[,i])
  df[,i]<- fct_collapse(df[,i], "1" = "5",
                        "2" = "4",
                        "3" = "3",
                        "4" = "2",
                        "5" = "1")
  df[,i] <- as.numeric(as.character(df[,i]))
}

#--------------------------------------------------------
#CREAT A SINGLE EDUCATION VARIABLE 

df<-cbind(df,df$edqual1)
names(df)[names(df)=="edqual1"] <-"edqual"
names<-c('edqual2','edqual3','edqual4','edqual5','edqual6','edqual7','edqual8','edqual9')
for (i in names){
  df[,'edqual'][df[,'edqual']<0 | is.na(df[,'edqual'])]<-df[,i][df[,'edqual']<0 | is.na(df[,'edqual'])]
}
df <- subset(df, select = -c(edqual2,edqual3,edqual4,edqual5,edqual6,edqual7,edqual8,edqual9))

# We do the same for sex :
df<-cbind(df,df$sex1)
names(df)[names(df)=="sex1"] <-"sex"
names<-c('sex2','sex3','sex4','sex5','sex6','sex7','sex8','sex9')
for (i in names){
  df[,'sex'][df[,'sex']<0 | is.na(df[,'sex'])]<-df[,i][df[,'sex']<0 | is.na(df[,'sex'])]
}
df <- subset(df, select = -c(sex2,sex3,sex4,sex5,sex6,sex7,sex8,sex9))

#--------------------------------------------------------
# ADD FINANCIAL VARIABLES

BDD_names_financial <- numeric()
# A part la wave 8, les noms des table sont les mêmes, on peut faire une boucle :
for (i in seq_len(9)){
  BDD_names_financial[i] <- paste0("wave_", i, "_financial_derived_variables.dta")
}
# Change for wave 8 :
BDD_names_financial[8] <- "wave_8_elsa_financial_dvs_eul_v1.dta"

# Import financial data for each wave
for (i in seq_len(9)){
  assign(x = paste0("wave", i, "_financial"), value = read_dta(BDD_names_financial[i]))
}


# States the Objective SES variables :
var_SES_objective <- c("idauniq","eqtotinc_bu_s", "nettotnhw_bu_s")

# Keep only the variables we want (var_SES_objective)
for (i in seq_len(9)){
  assign(x = paste0("wave", i, "_financial"), value = get(paste0("wave", i, "_financial"))[var_SES_objective])
}


# Merge the financial data with the dataframe data

for (i in 1:9){
  df = merge( x=df, y=get(paste0("wave", i, "_financial")), by="idauniq",all.x=TRUE)
  # For each vairable in var_SES_objective, rename the variable
  for (elem in var_SES_objective[2:length(var_SES_objective)]){
    names(df)[names(df) == elem] <- paste0(elem, i)
  }
}


# SAVE FILE :
#save(df, file="StartData_wide.dta")
write.csv(df, file = "StartData_wide.csv")


#--------------------------------------------------------
# CHANGE INTO LONG DATA SET
df <- read.csv(file = "StartData_wide.csv")

#columns_names = c(var_SES_objective[c(2, 3)], var_educ_health[c(2, 3)])
columns_names = c("marital_status", var_SES_objective[c(2, 3)], var_educ_health[c(4, 5, 6)])

df_temp <- select(df, c(idauniq, edqual, sex), starts_with("sclddr"))
df_long <- pivot_longer(df_temp, !idauniq & !edqual & !sex, names_to = "wave", names_prefix = "sclddr", values_to = "sclddr")

for (i in columns_names){
  df_temp <- select(df, c(idauniq, edqual, sex), starts_with(i))
  df_temp_long <- pivot_longer(df_temp, !idauniq & !edqual & !sex, names_to = "wave", names_prefix = i, values_to = i, values_drop_na = TRUE)
  df_long <- merge(df_long, df_temp_long, by = c('idauniq', 'wave', 'edqual', 'sex'))
}


#------------------------------------------------------
# TRANSFORMATION DES VARIABLES
# Changer les modalités de la variable education
df_long[df_long$edqual < 0 & !is.na(df_long$edqual),]$edqual <- NA
df_long$edqual <- as.factor(df_long$edqual)
df_long$edqual <- fct_collapse(df_long$edqual, "0 - No qualification" = "7",
                               "1 - Foreign / Others" = "6",
                               "2 - Lower secondary" = c("4", "5"),
                               "3 - Upper secondary" = "3",
                               "4 - Some tertiary" = "2",
                               "5 - Tertiary" = "1")

# Les variables de revenus
df_long[df_long$eqtotinc_bu_s < 0,]$eqtotinc_bu_s <- 0
df_long$log_revenu <- log(df_long$eqtotinc_bu_s + 0.000000001)
df_long$log_income <- log(df_long$eqtotinc_bu_s + 0.000000001)
df_long$ihs_wealth <- asinh(df_long$nettotnhw_bu_s)

# Sclddr divisé par 10 :
df_long[df_long$sclddr < 0,]$sclddr <- NA
df_long$sclddr <- df_long$sclddr/10

# Sex : 1 pour les hommes et 0 pour les femmes
df_long$sex <- as.integer(df_long[,c("sex")] == 1)

# Age : ne garder que les personnes de plus de 50 ans
df_long <- df_long %>% filter(age >= 50)

write.csv(df_long, file = 'StartData_long_without_NA.csv')


# Inspection of the self-rated health variables yields various insights,
# including: In wave 2, self-rated health has a lot of missing information, and
# in wave 3 it was not measured at all.

#A look at the data set from wave 2 shows that self-rated health has been
#measured in two different ways
# "HSE version:"                              "HRS version:"
#                                             - Excellent
# - Very good                                 - Very good
# - Good                                      - Good
# - Fair                                      - Fair
#                                             - Poor
# - Bad                                       
# - Very bad

# HSE stands for Health Survey for England, HRS stands for Health and
# Retirement Study, the two sources from which the two question wording were drawn.
# I assume that wave 2 included a so-called split ballot experiment where one
# random half of the sample answered the HSE version and the other half the 
# HRS version and researchers could thus examine the effects of question  wording.

#It is probably worth looking at the documentation and searching for articles
# that analyze self-rated health using ELSA data from 20-25 years ago to get a
# better understanding of what happened, but as a bottom line we probably just
# need to accept that we are missing a bit of information at the beginning of the data.
  
  
  
