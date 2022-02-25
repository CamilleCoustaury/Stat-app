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
setwd("/Users/camille/Desktop/COURS ENSAE/Stat-app/data")
setwd("~/Desktop/Stat-App_git/Data")

#--------------------------------------------------------
#INDEX CREATION

#********
##STEP 1 : create an index file, a file that only contains the unique ID's 
#of everyone who has ever participated in the survey
#********

#This command here opens the first wave core data, but only one variable,idauniq. 
#idauniq is a unique number assigned to everyone who has ever participated in ELSA:
wave1_core = read_dta("wave_1_core_data_v3.dta")
index = wave1_core["idauniq"]
describe(index)
# we have 22,099 participants in the first wave.


#********
##STEP 2 : append all other observations from the core files of wave 2 to 8
#********
wave2_core = read_dta("wave_2_core_data_v4.dta")
wave3_core = read_dta("wave_3_elsa_data_v4.dta")
wave4_core = read_dta("wave_4_elsa_data_v3.dta")
wave5_core = read_dta("wave_5_elsa_data_v4.dta")
wave6_core = read_dta("wave_6_elsa_data_v2.dta")
wave7_core = read_dta("wave_7_elsa_data.dta")
wave8_core = read_dta("wave_8_elsa_data_eul_v2.dta")
wave9_core = read_dta("wave_9_elsa_data_eul_v1.dta")

index= bind_rows(index,wave2_core["idauniq"], wave3_core["idauniq"], wave4_core["idauniq"],
                 wave5_core["idauniq"], wave6_core["idauniq"], wave7_core["idauniq"],
                 wave8_core["idauniq"], wave9_core["idauniq"]) 


BDD_names_core = c("wave_1_core_data_v3.dta", "wave_2_core_data_v4.dta", "wave_3_elsa_data_v4.dta", "wave_4_elsa_data_v3.dta",
              "wave_5_elsa_data_v4.dta", "wave_6_elsa_data_v2.dta", "wave_7_elsa_data.dta",
              "wave_8_elsa_data_eul_v2.dta", "wave_9_elsa_data_eul_v2.dta")

#Drop duplicates 
index = distinct(index)

#This data set now contains the unique ID of each of the 19,807 participants



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
df = merge( x=df, y=wave2_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr2'
df = merge( x=df, y=wave3_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr3'
df = merge( x=df, y=wave4_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr4'
df = merge( x=df, y=wave5_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr5'
df = merge( x=df, y=wave6_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr6'
df = merge( x=df, y=wave7_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr7'
df = merge( x=df, y=wave8_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr8'
df = merge( x=df, y=wave9_core[c("idauniq","sclddr")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'sclddr'] <- 'sclddr9'


#We now have what is called a "wide" data set, each row corresponds to one participant.
#We can now inspect the sclddr variables to see if they all have the same 
  #response format from wave to wave by looking at descriptive statistics.


#--------------------------------------------------------
# ADD SELF-RATED HEALTH VARIABLE

# Next, we'll add the self-rated health variable from the set of files containing variables
# that have been prepared by the IFS.

wave1_IFS = read_dta("wave_1_ifs_derived_variables.dta")
wave2_IFS = read_dta("wave_2_ifs_derived_variables.dta")
wave3_IFS = read_dta("wave_3_ifs_derived_variables.dta")
wave4_IFS = read_dta("wave_4_ifs_derived_variables.dta")
wave5_IFS = read_dta("wave_5_ifs_derived_variables.dta")
wave6_IFS = read_dta("wave_6_ifs_derived_variables.dta")
wave7_IFS = read_dta("wave_7_ifs_derived_variables.dta")
wave8_IFS = read_dta("wave_8_elsa_ifs_dvs_eul_v1.dta")
wave9_IFS = read_dta("wave_9_ifs_derived_variables.dta")


df = merge( x=df, y=wave1_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs1'
df = merge( x=df, y=wave2_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs2'
df = merge( x=df, y=wave3_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs3'
df = merge( x=df, y=wave4_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs4'
df = merge( x=df, y=wave5_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs5'
df = merge( x=df, y=wave6_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs6'
df = merge( x=df, y=wave7_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs7'
df = merge( x=df, y=wave8_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs8'
df = merge( x=df, y=wave9_IFS[c("idauniq","srh_hrs")], by="idauniq",all.x=TRUE)
names(df)[names(df) == 'srh_hrs'] <- 'srh_hrs9'


#--------------------------------------------------------
# ADD FINANCIAL VARIABLES

# Import data
wave1_Financial = read_dta("wave_1_financial_derived_variables.dta")
wave2_Financial = read_dta("wave_2_financial_derived_variables.dta")
wave3_Financial = read_dta("wave_3_financial_derived_variables.dta")
wave4_Financial = read_dta("wave_4_financial_derived_variables.dta")
wave5_Financial = read_dta("wave_5_financial_derived_variables.dta")
wave6_Financial = read_dta("wave_6_financial_derived_variables.dta")
wave7_Financial = read_dta("wave_7_financial_derived_variables.dta")
wave8_Financial = read_dta("wave_8_elsa_financial_dvs_eul_v1.dta")
wave9_Financial = read_dta("wave_9_financial_derived_variables.dta")

var_SES_objective <- c("idauniq","totinc_bu_s", "empinc_bu_s", "seinc_bu_s", "spinc_bu_s", 
                       "nethw_bu_s", "netfw_bu_s")

wave1_Financial <- wave1_Financial[var_SES_objective]
wave2_Financial <- wave2_Financial[var_SES_objective]
wave3_Financial <- wave3_Financial[var_SES_objective]
wave4_Financial <- wave4_Financial[var_SES_objective]
wave5_Financial <- wave5_Financial[var_SES_objective]
wave6_Financial <- wave6_Financial[var_SES_objective]
wave7_Financial <- wave7_Financial[var_SES_objective]
wave8_Financial <- wave8_Financial[var_SES_objective]
wave9_Financial <- wave9_Financial[var_SES_objective]


df = merge( x=df, y=wave1_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s1'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_1'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_1'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_1'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_1'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_1'
df = merge( x=df, y=wave2_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s2'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_2'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_2'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_2'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_2'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_2'
df = merge( x=df, y=wave3_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s3'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_3'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_3'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_3'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_3'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_3'
df = merge( x=df, y=wave4_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s4'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_4'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_4'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_4'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_4'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_4'
df = merge( x=df, y=wave5_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s5'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_5'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_5'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_5'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_5'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_5'
df = merge( x=df, y=wave6_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s6'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_6'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_6'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_6'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_6'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_6'
df = merge( x=df, y=wave7_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s7'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_7'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_7'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_7'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_7'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_7'
df = merge( x=df, y=wave8_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s8'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_8'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_8'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_8'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_8'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_8'
df = merge( x=df, y=wave9_Financial, by="idauniq",all.x=TRUE)
names(df)[names(df) == 'totinc_bu_s'] <- 'totinc_bu_s9'
names(df)[names(df) == 'empinc_bu_s'] <- 'empinc_bu_s_9'
names(df)[names(df) == 'seinc_bu_s'] <- 'seinc_bu_s_9'
names(df)[names(df) == 'spinc_bu_s'] <- 'spinc_bu_s_9'
names(df)[names(df) == 'nethw_bu_s'] <- 'nethw_bu_s_9'
names(df)[names(df) == 'netfw_bu_s'] <- 'netfw_bu_s_9'

#--------------------------------------------------------
# SAVE FILE


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


#Save file
save(df, file="StartData_wide.dta")
write.csv(df, file = "StartData_wide.csv")



#--------------------------------------------------------
# CHANGE INTO LONG DATA SET

df1=df[c("idauniq","sclddr1", "sclddr2", "sclddr3","sclddr4","sclddr5","sclddr6","sclddr7","sclddr8","sclddr9")]
df2=df[c("idauniq","srh_hrs1", "srh_hrs2", "srh_hrs3","srh_hrs4","srh_hrs5","srh_hrs6","srh_hrs7","srh_hrs8","srh_hrs9")]

gathercols1 <- c("sclddr1", "sclddr2", "sclddr3","sclddr4","sclddr5","sclddr6","sclddr7","sclddr8","sclddr9")
gathercols2 <- c("srh_hrs1", "srh_hrs2", "srh_hrs3","srh_hrs4","srh_hrs5","srh_hrs6","srh_hrs7","srh_hrs8","srh_hrs9")

df_long1=gather(df1, condition1, sclddr, all_of(gathercols1))
df_long2=gather(df2, condition2, srh_hsr, all_of(gathercols2))
df_long = merge( x=df_long1, y=df_long2, by="idauniq",all.x=TRUE)

save(df_long, file="StartData_long.dta")
