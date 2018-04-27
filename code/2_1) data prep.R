# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rmsfuns)
library(lubridate)
library(broom)
library(rugarch)
library(rmgarch)
library(tbl2xts)


# Load & Prep data --------------------------------------------------------

#NOTES:
# Make sure to remove NAs and weekends. -> No weekends in Yahoo finance data -> some not. See rows of NAs
# Drop stocks which haven't traded for long enough
#calculate returna

data <- 
  readRDS("data/data.rds")

# neat col names
colnames(data) <- gsub(x = colnames(data), pattern = ".JO.Adjusted|.X.Adjusted", replacement = "")

# Removing weekends & public holidays
data <- 
  data %>% xts_tbl() %>% 
  mutate(Days = format(date, "%A")) %>% filter(!Days %in% c("Saturday", "Sunday") ) %>% select(-Days)

data <-
  data[!grepl("-12-25|-12-26|-01-01|-03-21|-03-30|-04-02|-04-27|-05-01|-06-16|-08-09|-09-24|-12-16|-12-17", data$date),]
  

# Long format
data <-
  reshape2::melt(data, id="date", variable_name="symbol")

#****
 
# Trimming the dataset ----------------------------------------------------
Pct_Valid <- 0.8 # This can change of course. 70% valid data over period at least
StartDate <- ymd(20031201)
EndDate <- ymd(20180404) #	2018-04-04

# Shorten timeframe
data <- 
  data %>% filter(date >= StartDate & date <= EndDate)

NDates <- length(unique(data %>% pull(date)) ) 

# Only used shares still active:
ActiveTickers <-
  data %>% 
  filter(date >= ymd(20180404)) %>%
  group_by(variable) %>% 
  mutate(N_Valid = ifelse(is.na(value) == TRUE, 0, 1)) %>% 
  summarise(S = sum(N_Valid)) %>% 
  filter(S >0) %>% pull(variable)

HoldTickers <-
  data %>% 
  filter(variable %in% ActiveTickers) %>% 
  group_by(variable) %>% 
  mutate(N_Valid = ifelse(is.na(value) == TRUE, 0, 1) ) %>% summarise(N_Valid_Pct = sum(N_Valid)/NDates) %>% 
  filter(N_Valid_Pct >= Pct_Valid) %>% pull(variable) %>% unique()

rtn <- 
  data %>% 
  filter(variable %in% HoldTickers | variable == "ZAR") 


# Calculate returns -------------------------------------------------------

rtnseries <-
  rtn %>% group_by(variable) %>% 
  mutate(Return = value/lag(value)-1) 

rtnseries[is.na(rtnseries)] <- 0
rtnseries[is.infinite(rtnseries$value)] <- 0

#*** At this point it is in long format.. 4 cols: date, value, variable, Return (simple)

rtn.final <- 
  rtnseries %>% arrange(date) %>% group_by(variable) %>% mutate(Return = coalesce(Return, 0)) %>% 
  mutate(Index = cumprod(1+Return) ) %>% 
  mutate(DlogReturn =  log(Index) - log( lag(Index))) %>% ungroup() %>% 
  mutate(DlogReturn = coalesce(DlogReturn, 0)) %>% select(-Index)

#*** At this point it is in long format.. 5 cols: date, value, variable, Return (simple), DlogReturn

# Stratification ----------------------------------------------------------

# Stratify for high vol
# Ramaphosa rally

usdzar <- 
  rtn.final %>% filter(variable == "ZAR")

Df <- 
  usdzar %>% select(date, Return) %>% filter(date > first(date))

StratValue1 <- 0.2 # Change threshold
StratValue2 <- 0.8 # Change threshold

df_Strat <- 
  Df %>% 
  mutate(Q1 = quantile(Return, StratValue1, na.rm = TRUE), Q2 = quantile(Return, StratValue2, na.rm = TRUE)) %>% 
  mutate(ID = ifelse(Return <= Q1, "Low", 
                     ifelse(Return > Q1 & Return <= Q2 , "Medium",
                            ifelse(Return > Q2 , "High", "NA")) )) %>% ungroup() 



HighDates <- df_Strat %>% filter(ID == "High") %>% pull(date)
LowDates <- df_Strat %>% filter(ID == "Low") %>% pull(date)

  # And now you can do the following e.g.:
  # rtn.final %>% filter(date %in% HighDates)

# RAMAPHOSA RALLY - defined as 11 Dec 2017 (cyclical high) to now.
  # Ramaphosa sworn in on 15 Feb
  # Zuma resigned 14 Feb

RRstartDate <- ymd(20171211)

  # And now you can do the following
  # rtn.final %>% filter(date >= RRstartDate)

