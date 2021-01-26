#This script shows how I retrieved all of the market data for all grid-scale batteries
#in the NEM

#This requires retrieving all of the spot market, lower FCAS and raise FCAS data
#for these batteries

#The function below is how I will be interacting with AEMO's Market Management System
#which is where all of the NEM's market data is recorded and stored

#I interact with this database by writing SQL queries

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

#These are the Dispatch Unit Identifiers (DUIDs) for the batteries, there
#are 5 batteries but each battery is required to have two DUIDs to operate in the
#NEM currently (one for its conusmption and one for its export of electricity)

gen_duids = c('BALBG1', 'DALNTH01', 'GANNBG1', 'HPRG1', 'LBBG1')
load_duids = c('BALBL1', 'DALNTHL1', 'GANNBL1', 'HPRL1', 'LBBL1')
duids = c('BALBG1', 'DALNTH01', 'GANNBG1', 'HPRG1', 'LBBG1','BALBL1', 'DALNTHL1', 'GANNBL1', 'HPRL1', 'LBBL1')

#Using the "q" query below will retrieve all of the spot market data for each DUID
#I am using a for-loop here because the output from this query is too large to be
#downloaded all at once for all batteries

df_list = list()

for (d in duids) {
  print(d)
  
  q <- sprintf("select `SETTLEMENTDATE`,`DUID`,`SCADAVALUE` 
  from mms.dispatch_unit_scada where SETTLEMENTDATE 
  between '2017-01-01 00:00:00' and '2020-12-31 23:55:00' and
  DUID = '%s'", d)
  df_2017 <- run_query(q)
  
  df_list <- append(df_list, list(df_2017))
}

#The output of this query is then saved to the "energy" object dataframe

energy <- do.call(rbind, df_list)

#we now need to do the same for lower and raise FCAS data

#RAISE FCAS DATA

df_list = list()

for (d in gen_duids) {
  print(d)
  
  q <- sprintf("select SETTLEMENTDATE, DUID, RAISEREG, RAISE6SEC, RAISE60SEC, RAISE5MIN
                from DISPATCHLOAD where SETTLEMENTDATE between '2019-01-01 00:00:00' and 
               '2020-01-01 00:00:00' AND DUID = '%s'", d)
    df <- run_query(q)
  
  df_list <- append(df_list, list(df_2017))
}

raise_fcas <- do.call(rbind, df_list)

#LOWER FCAS DATA

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



#Now we have all the data we need to do our RRO analysis




