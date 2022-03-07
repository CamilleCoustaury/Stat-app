#### This file is used to create a file for analyses ####
#--------------------------------------------------------
#IMPORTATIONS + FILE DIRECTORY

#install.packages("haven") only for first use
library(haven)
library(Hmisc)
library(dplyr)
library(sqldf)
library(tidyr)

#set the working directory, namely to the folder where we keep the data set.
# WARNING : must be change according to your own directory
setwd("~/Documents/statApp/git/Stat-app/data")


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
# ADD SUBJECTIVE SES VARIABLE
# In a second step, we will now add the subjective SES variable sclddr to the 
#data set we have created. We start with the first wave:
df = merge( x=index, y=wave1_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)

#The merge command here adds the variable sclddr uniquely to each 
#participant in the data set, drawing on the wave 2 core file. We should keep
#in mind that the sclddr variable only comes from wave 2, and so we will
#rename it accordingly:
names(df)[names(df) == 'sclddr'] <- 'sclddr1'

#The data set now contains two variables, the ID variable idauniq and 
#the subjective SES variable from wave 1 sclddr1.
#We'll add the remaining waves of sclddr accordingly:
for (i in 2:9){
  df = merge( x=df, y=get(paste0("wave", i, "_core"))[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
  names(df)[names(df) == 'sclddr'] <- paste0('sclddr', i)
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


# Keep the variable srh_hrs
for (i in 1:9){
  df = merge( x=df, y=get(paste0("wave", i, "_IFS"))[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
  names(df)[names(df) == 'srh_hrs'] <- paste0('srh_hrs', i)
}


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
var_SES_objective <- c("idauniq","totinc_bu_s", "empinc_bu_s", "seinc_bu_s", "spinc_bu_s", 
                       "nethw_bu_s", "netfw_bu_s")

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

save(df, file="StartData_wide.dta")
write.csv(df, file = "StartData_wide.csv")


#--------------------------------------------------------
# CHANGE INTO LONG DATA SET
df <- read.csv(file = "StartData_wide.csv")

columns_names = c("srh_hrs", "totinc_bu_s", "empinc_bu_s", "seinc_bu_s", "spinc_bu_s", "nethw_bu_s", "netfw_bu_s")

df_temp <- select(df, idauniq, starts_with("sclddr"))
df_long <- pivot_longer(df_temp, !idauniq, names_to = "wave", names_prefix = "sclddr", values_to = "sclddr")

for (i in columns_names){
  df_temp <- select(df, idauniq, starts_with(i))
  df_temp_long <- pivot_longer(df_temp, !idauniq, names_to = "wave", names_prefix = i, values_to = i, values_drop_na = FALSE)
  df_long <- merge(df_long, df_temp_long, by = c('idauniq', 'wave'))
}

write.csv(df_long, file = 'StartData_long_with_NA.csv')

# Name the variables :
# df1=df[c("idauniq","sclddr1", "sclddr2", "sclddr3","sclddr4","sclddr5","sclddr6","sclddr7","sclddr8","sclddr9")]
# df2=df[c("idauniq","srh_hrs1", "srh_hrs2", "srh_hrs3","srh_hrs4","srh_hrs5","srh_hrs6","srh_hrs7","srh_hrs8","srh_hrs9")]
# 
# gathercols1 <- c("sclddr1", "sclddr2", "sclddr3","sclddr4","sclddr5","sclddr6","sclddr7","sclddr8","sclddr9")
# gathercols2 <- c("srh_hrs1", "srh_hrs2", "srh_hrs3","srh_hrs4","srh_hrs5","srh_hrs6","srh_hrs7","srh_hrs8","srh_hrs9")
# 
# df_long1=gather(df1, condition1, sclddr, all_of(gathercols1))
# df_long2=gather(df2, condition2, srh_hsr, all_of(gathercols2))
# df_long = merge( x=df_long1, y=df_long2, by="idauniq",all.x=TRUE)
# 
# save(df_long, file="StartData_long.dta")



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




