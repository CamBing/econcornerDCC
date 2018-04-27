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

