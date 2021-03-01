#DATA IMPORTING/CLEANING/TIDYING

#This script demonstrates how I collected all of the data I need to conduct
#analysis on grid-scale batteries

#This involves directly interacting with AEMO's databases to download all relevant
#energy, raise FCAS and lower FCAS data.

#Because there's so much data I coud only access it all by looping through each
#of the generation and load DUIDs for each 1-2 years

#I ended having to extract most of it on an annual basis because my queries
#kept timing out if I tried to extract data over longer timeframes where all 
#DUIDs were involved



library(tidyverse)
getwd()
setwd('C:/Users/joshua.copeland/Documents/SEA_projects/battery_case_studies/data/energy')


hornsdale_17_18 <- read_csv('hornsdale_17-18.csv',
                          col_types = cols(
                            DUID = col_factor(levels = NULL)
                          )
)

hornsdale_19_20 <- read_csv('hornsdale_19-20.csv',
                            col_types = cols(
                              DUID = col_factor(levels = NULL)
                            )
)

hornsdale <- rbind(hornsdale_17_18,hornsdale_19_20)

write_csv(hornsdale,'hornsdale.csv')

#GETTING FCAS DATA FOR BATTERIES

#--------------------LOWER FCAS----------------------------------------------------

# ------------------------ 2020 ----------------

rm(list = ls())

library(httr)

run_query <- function(query) {
  requ <- list(
    query = query,
    key = "AEMC16",
    format="csv"
  )
  res <- POST("http://www.neopoint.com.au/data/query", body = requ, encode = "form", verbose())
  tabtext <- content(res, "text")
  df <- read.csv(text = tabtext, header = TRUE)
}

gen_duids = c('BALBG1', 'DALNTH01', 'GANNBG1', 'HRPG1', 'LBBG1')
load_duids = c('BALBL1', 'DALNTH1', 'GANNBL1', 'HPRL1', 'LBBL1')

df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, LOWERREG, LOWER6SEC, LOWER60SEC, LOWER5MIN
               from DISPATCHLOAD
               where SETTLEMENTDATE between '2020-01-01 00:00:00' and '2020-11-20 00:00:00' AND DUID = '%s'", d)
  
  lower_fcas_2020 <- run_query(q)
  
  df_list <- append(df_list, list(lower_fcas_2020))
}

lower_fcas_2020 <- do.call(rbind, df_list)

#---------2019------------------


df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, LOWERREG, LOWER6SEC, LOWER60SEC, LOWER5MIN
               from DISPATCHLOAD
               where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-01-01 00:00:00' AND DUID = '%s'", d)
  
  lower_fcas_2019 <- run_query(q)
  
  df_list <- append(df_list, list(lower_fcas_2019))
}

lower_fcas_2019 <- do.call(rbind, df_list)

#-----------2018----------------

df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, LOWERREG, LOWER6SEC, LOWER60SEC, LOWER5MIN
               from DISPATCHLOAD
               where SETTLEMENTDATE between '2018-01-01 00:00:00' and '2019-01-01 00:00:00' AND DUID = '%s'", d)
  
  lower_fcas_2018 <- run_query(q)
  
  df_list <- append(df_list, list(lower_fcas_2018))
}

lower_fcas_2018 <- do.call(rbind, df_list)


#-------2017---------------

df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, LOWERREG, LOWER6SEC, LOWER60SEC, LOWER5MIN
               from DISPATCHLOAD
               where SETTLEMENTDATE between '2017-01-01 00:00:00' and '2018-01-01 00:00:00' AND DUID = '%s'", d)
  
  lower_fcas_2017 <- run_query(q)
  
  df_list <- append(df_list, list(lower_fcas_2017))
}

lower_fcas_2017 <- do.call(rbind, df_list)



#-------------COMBINE----------

lower_fcas <- rbind(lower_fcas_2020,lower_fcas_2019,lower_fcas_2018,lower_fcas_2017)

remove(lower_fcas_2020,lower_fcas_2019,lower_fcas_2018,lower_fcas_2017)

setwd("C:/Users/joshua.copeland/Documents/SEA_projects/battery_case_studies/data/fcas")

write_csv(lower_fcas, 'lower_fcas.csv')




#---------RAISE FCAS--------------------------------------------------------



df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, RAISEREG, RAISE6SEC, RAISE60SEC, RAISE5MIN
                from DISPATCHLOAD where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-01-01 00:00:00' AND DUID = '%s'", d)
  
  raise_fcas_2020 <- run_query(q)
  
  df_list <- append(df_list, list(raise_fcas_2020))
}

raise_fcas_2020 <- do.call(rbind, df_list)

#---------2019------------------


df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, RAISEREG, RAISE6SEC, RAISE60SEC, RAISE5MIN
                from DISPATCHLOAD where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-01-01 00:00:00' AND DUID = '%s'", d)
  
  raise_fcas_2019 <- run_query(q)
  
  df_list <- append(df_list, list(raise_fcas_2019))
}

raise_fcas_2019 <- do.call(rbind, df_list)

#-----------2018----------------

df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, RAISEREG, RAISE6SEC, RAISE60SEC, RAISE5MIN
                from DISPATCHLOAD where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-01-01 00:00:00' AND DUID = '%s'", d)
  
  raise_fcas_2018 <- run_query(q)
  
  df_list <- append(df_list, list(raise_fcas_2018))
}

raise_fcas_2018 <- do.call(rbind, df_list)


#-------2017---------------

df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, RAISEREG, RAISE6SEC, RAISE60SEC, RAISE5MIN
                from DISPATCHLOAD where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-01-01 00:00:00' AND DUID = '%s'", d)
  
  raise_fcas_2017 <- run_query(q)
  
  df_list <- append(df_list, list(raise_fcas_2017))
}

raise_fcas_2017 <- do.call(rbind, df_list)




#----------------COMBINE---------------

raise_fcas <- rbind(raise_fcas_2020, raise_fcas_2019, raise_fcas_2018, raise_fcas_2017)

write_csv(raise_fcas, 'raise_fcas.csv')

remove(raise_fcas_2020, raise_fcas_2019, raise_fcas_2018, raise_fcas_2017)








#ENERGY DATA-----------------------

#GENERATION ------------ 19-20 ---------------

df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, SCADAVALUE
                from MMS.DISPATCH_UNIT_SCADA where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-11-20 00:00:00' AND DUID = '%s'", d)
  
  energy_gen_19_20 <- run_query(q)
  
  df_list <- append(df_list, list(energy_gen_19_20))
}

energy_gen_19_20 <- do.call(rbind, df_list)

##GENERATION ------------ 17-18 ---------------

df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, SCADAVALUE
                from MMS.DISPATCH_UNIT_SCADA where SETTLEMENTDATE between '2017-11-01 00:00:00' and '2018-12-31 23:55:00' AND DUID = '%s'", d)
  
  energy_gen_17_18 <- run_query(q)
  
  df_list <- append(df_list, list(energy_gen_17_18))
}

energy_gen_17_18 <- do.call(rbind, df_list)


#LOAD ------------ 19-20 ---------------

df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, SCADAVALUE
                from MMS.DISPATCH_UNIT_SCADA where SETTLEMENTDATE between '2019-01-01 00:00:00' and '2020-11-20 00:00:00' AND DUID = '%s'", d)
  
  energy_load_19_20 <- run_query(q)
  
  df_list <- append(df_list, list(energy_load_19_20))
}

energy_load_19_20 <- do.call(rbind, df_list)

##GENERATION ------------ 17-18 ---------------

df_list = list()

for (d in load_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, SCADAVALUE
                from MMS.DISPATCH_UNIT_SCADA where SETTLEMENTDATE between '2017-11-01 00:00:00' and '2018-12-31 23:55:00' AND DUID = '%s'", d)
  
  energy_load_17_18 <- run_query(q)
  
  df_list <- append(df_list, list(energy_load_17_18))
}

energy_load_17_18 <- do.call(rbind, df_list)


# merge all energy

energy <- rbind(energy_gen_17_18,energy_gen_19_20,energy_load_17_18,energy_load_19_20)

getwd()
write_csv(energy, 'energy.csv')
remove(energy_gen_17_18,energy_gen_19_20,energy_load_17_18,energy_load_19_20)

remove(df,df_list, ballarat)

#----------------------------------


