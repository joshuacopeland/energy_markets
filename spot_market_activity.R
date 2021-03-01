#Battery activity in the spot market

#There are 5 grid scale batteries in the NEM: Hornesdale, Lake Bonney, Gannawarra,
# Ballarat and Dalrymple North.

#I am interested in learning about how these batteries participate in the market

#Objectives of this analysis:
# 1. Assess how active grid-scale batteries are in the spot market
# 2. Assess to what extent this activity is driven by spot market prices


#Import the spot market data required

library(tidyverse)
library(lubridate)

getwd()
setwd('C:/Users/joshua.copeland/Documents/SEA_projects/battery_case_studies/data')

energy <- read_csv('energy.csv',
                   col_types = cols(DUID = col_factor(levels = NULL),
                                    datetime = col_datetime(format = "")))

str(energy)

#Convert MW to MWh

energy <- energy %>% 
  mutate(MWh = MW/12)


#check to see if all the relevant DUIDs are present and check timestamps
#for future analysis it will also be important to understand how many dispatch
#intervals this all encompasses

battery_timelines <- energy %>% 
  group_by(DUID) %>% 
  summarise(start_date = min(datetime), end_date = max(datetime)) %>% 
  mutate(total_days = end_date - start_date) %>%
  mutate(total_intervals = round(total_days*288, 2))

#there's some mismatching on the end dates but all start dates are consistent
#with commissioning dates

#1. HOW ACTIVE ARE GRID-SCALE BATTERIES IN THE SPOT MARKET?

#What is the most common activities these batteries carry out in the spot market?

battery_top_n <- energy %>% 
  group_by(DUID) %>% 
  count(MW) %>% 
  arrange(desc(n)) %>% 
  top_n(1, n) 

#It seems that batteries are most often idle in the spot market

#What proportion of the time in the spot market have batteries been idle?



activity_proportion <- battery_top_n %>% 
  inner_join(battery_timelines, by = 'DUID') %>% 
  select(DUID, MW, freq = n, total_intervals)

str(activity_proportion)

#Need to convert total_intervals to an integer and then find proportion

activity_proportion <- activity_proportion %>% 
  mutate(total_intervals_int = as.integer(total_intervals)) %>% 
  select(DUID, MW, idle_intervals = freq, total_intervals = total_intervals_int) 

activity_proportion <- activity_proportion %>% 
  mutate(idle_prop = (idle_intervals / total_intervals)*100) 
  

#create battery groupings and order appropriately

activity_proportion <- activity_proportion %>% 
  mutate(battery = if_else(DUID == "BALBG1"|DUID == "BALBL1", "Ballarat",
                           if_else(DUID == "DALNTH01"|DUID == "DALNTHL1", "Dalrymple",
                                   if_else(DUID == "GANNBG1"|DUID == "GANNBL1", "Gannawarra",
                                           if_else(DUID == "HPRG1"|DUID == "HPRL1", "Hornesdale", "Lake Bonney")))))


activity_proportion$DUID <- factor(activity_proportion$DUID, levels = c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1", "LBBG1", "LBBL1"))
activity_proportion$battery <- factor(activity_proportion$battery, levels = c("Ballarat", "Dalrymple", "Gannawarra", "Hornesdale", "Lake Bonney"))

activity_proportion <- activity_proportion %>% 
  select(battery, DUID, MW, idle_intervals, total_intervals, idle_prop) %>% 
  arrange(battery)

         
         
ggplot(activity_proportion, aes(x = DUID, y = idle_prop, fill = battery)) + geom_bar(stat = "identity") + theme(legend.position = "none")


#Conclusion:

#Batteries don't seem to be very active in the spot market, only Dalrymple's load component 
#has been active in the market > 50% of the time.

#A good follow-up exercise would be to compare these idleness metrics with other conventinal generators (coal, OCGT, etc)


#Before looking at the relationship of activity with price, it is worth understanding the the volume of activity that does occur

#To do this we first need to exclude all idle intervals

energy_active <- energy %>% 
  filter(MW != 0)

#Note that the number of observations has reduce by approximately two thirds!

#What are the top 3 active actions for batteries and how often do they occcur?

energy_active_top_n_3 <- energy_active %>% 
  group_by(DUID) %>% 
  count(MW) %>% 
  arrange(desc(n)) %>% 
  top_n(3,n)

energy_active_top_n_3 <- energy_active_top_n_3 %>% 
  inner_join(battery_timelines, by = 'DUID') %>% 
  select(DUID,MW, n, total_intervals)

energy_active_top_n_3$total_intervals <- as.integer(energy_active_top_n_3$total_intervals)

str(energy_active_top_n_3)

energy_active_top_n_3 <- energy_active_top_n_3 %>% 
  mutate(prop_total_intervals = round((n/total_intervals)*100,2)) %>% 
  group_by(DUID) %>% 
  summarise(top_n3_total_prop = sum(prop_total_intervals))

#Data formatting so I graph properly
#lesson learnt: assign factor levels/other attributes upfront to the actual dataset to minimise mess later on

energy_active_top_n_3$DUID <- factor(energy_active_top_n_3$DUID, levels = c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1", "LBBG1", "LBBL1"))
energy_active_top_n_3 <- energy_active_top_n_3 %>% 
    mutate(battery = if_else(DUID == "BALBG1"|DUID == "BALBL1", "Ballarat",
                           if_else(DUID == "DALNTH01"|DUID == "DALNTHL1", "Dalrymple",
                                   if_else(DUID == "GANNBG1"|DUID == "GANNBL1", "Gannawarra",
                                           if_else(DUID == "HPRG1"|DUID == "HPRL1", "Hornesdale", "Lake Bonney")))))


ggplot(energy_active_top_n_3, aes(x = DUID, y = top_n3_total_prop , fill = battery)) + geom_bar(stat = "identity") + theme(legend.position = "none") 

#This graph shows the proportion the top 3 active activity values represent of total battery activity

#What does this tell us?

#It tells us that batteries are very flexible in the way they interact with the market 
#as their top 3 active spot market dispatch inputs represent an extremely small proportion
#of overall activity - except for Dalrymple's load

#This is likely because what they draw/export to the grid is highly dependant on FCAS markets

#This could be indicated by demonstrating a gap between what batteries aggregately charge/discharge in the spot market

energy_active_volumes <- energy_active %>% 
  group_by(DUID) %>% 
  summarise(energy_volumes = sum(MWh)) %>% 
  mutate(battery = if_else(DUID == "BALBG1"|DUID == "BALBL1", "Ballarat",
                           if_else(DUID == "DALNTH01"|DUID == "DALNTHL1", "Dalrymple",
                                   if_else(DUID == "GANNBG1"|DUID == "GANNBL1", "Gannawarra",
                                           if_else(DUID == "HPRG1"|DUID == "HPRL1", "Hornesdale", "Lake Bonney")))))
energy_active_volumes$DUID <- factor(energy_active_volumes$DUID, levels = c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1", "LBBG1", "LBBL1"))



ggplot(energy_active_volumes, aes(x = DUID, y = energy_volumes , fill = battery)) + geom_bar(stat = "identity") + theme(legend.position = "none") 

#There doesn't seem to be too much of a gap - the gaps between load and generation 
#could likely be explained by efficiency losses

#Except for Dalrymple and Hornesdale, their gaps likely indicate excess FCAS market activity

#To verify this I would need to collect all of the efficiency ratings for these batteries and measure
#any difference between load and generation beyond what their efficiency ratings would imply


#2. IS THIS ACTIVITY DRIVEN BY SPOT MARKET PRICES

#first we need to import prices

prices <- read_csv("prices.csv")

#This tibble has all price data for all markets for SA and VIC

#We need to make sure the right regional prices join with the right batteries
#To do this, we need to label each energy_active observation with a region id

energy_active <- energy_active %>% 
  mutate(regionid = if_else(DUID == "BALBG1"|DUID == "BALBL1"|DUID == "GANNBG1"|DUID == "GANNBL1", "VIC1", "SA1"))

#Might be easier to separate everything into the two state and then bring it back together

prices_SA <- prices %>% 
  filter(regionid == "SA1") %>% 
  select(settlementdate, regionid, RRP)

prices_VIC <- prices %>% 
  filter(regionid == "VIC1") %>% 
  select(settlementdate, regionid, RRP)


energy_active_SA <- energy_active %>% 
  filter(regionid == "SA1") 

energy_active_VIC <- energy_active %>% 
  filter(regionid == "VIC1")

#Now join prices onto each region of battery observations

energy_active_SA <- energy_active_SA %>% 
  inner_join(prices_SA, by = c("datetime" = "settlementdate")) %>% 
  select(datetime, DUID,MW,MWh,regionid = regionid.x,RRP)

energy_active_VIC <- energy_active_VIC %>% 
  inner_join(prices_VIC, by = c("datetime" = "settlementdate")) %>% 
  select(datetime, DUID,MW,MWh,regionid = regionid.x,RRP)


energy_active <- rbind(energy_active_SA,energy_active_VIC)

remove(energy_active_SA,energy_active_VIC)

#important things to keep separate when looking at aggregate activity:
#the region of batteries and whether activity of load or generation

#We've already got a region indicator so we need to insert a load/gen indicator

energy_active <- energy_active %>% 
  mutate(type = if_else(DUID == "BALBG1"|DUID == "DALNTH01"|DUID == "GANNBG1"|DUID == "HPRG1"|DUID == "LBBG1", "gen", "load")) %>% 
  mutate(battery = if_else(DUID == "BALBG1"|DUID == "BALBL1", "Ballarat",
                           if_else(DUID == "DALNTH01"|DUID == "DALNTHL1", "Dalrymple",
                                   if_else(DUID == "GANNBG1"|DUID == "GANNBL1", "Gannawarra",
                                           if_else(DUID == "HPRG1"|DUID == "HPRL1", "Hornesdale", "Lake Bonney")))))


#Now we can make some graphs to get a feel for price/activity relationships

ggplot(energy_active, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter() + facet_wrap(~regionid + type)


#My computer can't handle graphing to much at once so lets try breaking down this relationship by price brackets

energy_active_minus1000_minus50 <- energy_active %>% 
  filter(RRP >= -1000 & RRP <= -50)

energy_active_minus50_100 <- energy_active %>% 
  filter(RRP >= -50 & RRP <= 100)
  
energy_active_100_500 <- energy_active %>% 
  filter(RRP >= 100 & RRP <= 500)

energy_active_500_1000 <- energy_active %>% 
  filter(RRP >= 100 & RRP <= 1000)

energy_active_1000_15000 <- energy_active %>% 
  filter(RRP >= 1000 & RRP <= 15000)

ggplot(energy_active_minus1000_minus50, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter(alpha = 0.2) + facet_wrap(~regionid + type)
ggplot(energy_active_minus50_100, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter(alpha = 0.2) + facet_wrap(~regionid + type)
ggplot(energy_active_100_500, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter(alpha = 0.2) + facet_wrap(~regionid + type)
ggplot(energy_active_500_1000, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter(alpha = 0.2) + facet_wrap(~regionid + type)
ggplot(energy_active_1000_15000, aes(x = MWh, y = RRP, colour = battery)) + geom_jitter(alpha = 0.2) + facet_wrap(~regionid + type)

#There doesn't seem to be any real noticeable relationship

#Battery activity is so great and variable between ~0-3MWh that there are just massive walls of plot points

#There isn't any more analysis I can conduct with spot market data, I will need to bring in FCAS
#data to try and wring out price and activity relationships


       