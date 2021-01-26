#AEMO has asked the AEMc to consider if it is appropriate for batteries to be
#exempt from the Retailer Reliability Obligation in the National Electricity Rules
#as part of developing a policy framework for grid-scale batteries in the Integrating
#Energy Storage into the NEM rule change.

#The RRO was designed to encourage new investments in dispatchable energy to improve 
#system reliability. It requires liable entities to contract with generation, storage 
#or demand response so that there is a minimum amount of dispatchable energy available 
#to meet consumer and system needs.

#Market customers are liable entities under the RRO if its annual aggregate annual 
#load is over 10Gwh for a particular NEM region. This would energy storage assets 
#liable entities given that they currently register as a Market Customer to operate in the NEM.


#AEMO argues in its rule change request that this is not appropriate because these #
#assets improve system reliability as they are likely to consumer and store electricity 
#when demand is low, and produce electricity in periods of high demand.

#Therefore, assessing whether these assets should not qualify as liable entities 
# relies on the following:
#  1. Identifying that energy storage are liable entities under the RRO
#  2. Determining if they are a net asset to system reliability by observing their 
#     activity during high price periods




library(tidyverse)
library(lubridate)

getwd()
setwd("C:/Users/joshua.copeland/Documents/SEA_projects/battery_case_studies/data")

prices <- read_csv('prices.csv')

energy <- read_csv('energy.csv')

#Are batteries liable under the RRO? Only if their aggregate annual load >10GWh


energy <- energy %>% 
  mutate(MWh = MW/12)

annual_load_sum <- energy %>% 
  filter(DUID == "BALBL1"| DUID =="DALNTHL1" |DUID =="GANNBL1" |DUID == "HPRL1" |DUID == "LBBL1") %>% 
  group_by(DUID,year(datetime)) %>% 
  summarise(load_sum_GWh = (sum(MWh))/1000)
  

#yes, batteries are liable under the RRO. Hornesdale has been every year and so has 
#Gannawarra (2019 & 2020)/Lake Bonney (2020)

#Now was need to asset if they are a net asset to system reliability by observing
#their activity during high price periods (i.e. during reliability gaps)

high_price <- prices %>% 
  select(settlementdate, regionid, RRP) %>% 
  filter(RRP >= 5000)

#Now we want to find when energy loads != 0 during these time frames

#separate high_prices datetime from everything and then innerjoin with energy
#Remember to differentiate between VIC & SA (Gann/Ball = VIC)

high_price_activity_VIC <- high_price %>% 
  select(settlementdate, regionid) %>% 
  filter(regionid == "VIC1") %>% 
  inner_join(energy, by = c("settlementdate" = 'datetime')) %>% 
  filter(DUID == "BALBL1"|DUID == "BALBG1" | DUID =="GANNBL1"| DUID == "GANNBG1")%>% 
  group_by(DUID) %>% 
  summarise(high_price_load_volume = sum(MWh))

high_price_activity_SA <- high_price %>% 
  select(settlementdate, regionid) %>% 
  filter(regionid == "SA1") %>% 
  inner_join(energy, by = c("settlementdate" = 'datetime')) %>% 
  filter(DUID =="DALNTHL1" | DUID == "DALNTH01" |DUID == "HPRL1" | DUID == "HPRG1" |DUID == "LBBL1" | DUID == "LBBG1")%>% 
  group_by(DUID) %>% 
  summarise(high_price_load_volume = sum(MWh))

high_price_activity_energy <- rbind(high_price_activity_SA,high_price_activity_VIC)

remove(high_price_activity_SA,high_price_activity_VIC)

#There is a small amount of energy which can be attributed to load during high price
#events. This data was presented in the August consultation paper and AEMO said
#that activity in the lower regulation FCAS market could likely reconcile this
#activity. Is this the case?


lower_fcas <- read_csv('lower_fcas.csv')

high_price_intervals <- high_price %>% 
  select(settlementdate,regionid)

high_price_lower_reg_activity_VIC <- lower_fcas %>% 
  select(datetime,DUID,lower_reg) %>% 
  mutate(lower_reg_MWh = lower_reg/12) %>% 
  inner_join(high_price_intervals, by = c("datetime" = 'settlementdate')) %>% 
  filter(lower_reg_MWh !=0) %>% 
  filter(regionid == "VIC1", DUID == 'BALBL1' | DUID == 'GANNBL1') %>% 
  group_by(DUID) %>% 
  summarise(high_price_lower_reg_volume = sum(lower_reg_MWh))

high_price_lower_reg_activity_SA <- lower_fcas %>% 
  select(datetime,DUID,lower_reg) %>% 
  mutate(lower_reg_MWh = lower_reg/12) %>% 
  inner_join(high_price_intervals, by = c("datetime" = 'settlementdate')) %>% 
  filter(lower_reg_MWh !=0) %>% 
  filter(regionid == "SA1", DUID == 'DALNTHL1' | DUID == 'HPRL1' | DUID == 'LBBL1') %>% 
  group_by(DUID) %>% 
  summarise(high_price_lower_reg_volume = sum(lower_reg_MWh))

high_price_lower_reg_activity <- rbind(high_price_lower_reg_activity_SA,high_price_lower_reg_activity_VIC)

remove(high_price_lower_reg_activity_SA,high_price_lower_reg_activity_VIC)

high_price_activity <- high_price_activity_energy %>% 
  inner_join(high_price_lower_reg_activity, by = "DUID")


#Yes, AEMO was correct. This spot market load activity is at least compensated 
#by activity in lower_regulation by almost all batteries (except one by a couple
#of Mwh)

#therefore, it seems that these assets tend to be net reliability assets.


#writing to csv for slides...

getwd()
setwd("C:/Users/joshua.copeland/Documents/SEA_projects/battery_case_studies/data/csv_dump")

write_csv(annual_load_sum, "annual_load_sum.csv")
write_csv(high_price_activity_energy, "high_price_activity_energy.csv")
write_csv(high_price_activity, "high_price_activity.csv")

