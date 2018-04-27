#Regression approach

Pct_Valid.reg <- 0.8
NDates.reg <- length(unique(data %>% pull(date)) ) 

# Only used shares still active:
ActiveTickers.reg <-
  data %>% 
  filter(date >= ymd(20180404)) %>%
  group_by(variable) %>% 
  mutate(N_Valid = ifelse(is.na(value) == TRUE, 0, 1)) %>% 
  summarise(S = sum(N_Valid)) %>% 
  filter(S >0) %>% pull(variable)

HoldTickers.reg <-
  data %>% 
  filter(variable %in% ActiveTickers.reg) %>% 
  group_by(variable) %>% 
  mutate(N_Valid = ifelse(is.na(value) == TRUE, 0, 1) ) %>% summarise(N_Valid_Pct = sum(N_Valid)/NDates.reg) %>% 
  filter(N_Valid_Pct >= Pct_Valid.reg) %>% pull(variable) %>% unique()

rtn.reg <- 
  data %>% 
  filter(variable %in% HoldTickers.reg | variable == "ZAR") 


# Calculate returns -------------------------------------------------------

rtnseries.reg <-
  rtn.reg %>% group_by(variable) %>% 
  mutate(Return = value/lag(value)-1) 

rtnseries.reg[is.na(rtnseries.reg)] <- 0
rtnseries.reg[is.infinite(rtnseries.reg$value)] <- 0

#*** At this point it is in long format.. 4 cols: date, value, variable, Return (simple)

rtn.final.reg <- 
  rtnseries.reg %>% arrange(date) %>% group_by(variable) %>% mutate(Return = coalesce(Return, 0)) %>% 
  mutate(Index = cumprod(1+Return) ) %>% 
  mutate(DlogReturn =  log(Index) - log( lag(Index))) %>% ungroup() %>% 
  mutate(DlogReturn = coalesce(DlogReturn, 0)) %>% select(-Index)


# Prep data ---------------------------------------------------------------

Regression_data <-
  rtn.final.reg %>% select("date", "variable", "DlogReturn")

zar <-
  Regression_data %>%  filter(variable == "ZAR") %>% 
  select("date", "DlogReturn") %>% 
  rename("usdzar_spot" = DlogReturn)

Regression_data <- 
  right_join(Regression_data, zar, by = "date") %>% 
  filter(variable != "ZAR") %>% 
  filter(!is.na(DlogReturn))






# Execute -----------------------------------------------------------------

Regressions <-
  Regression_data %>% 
  group_by(variable) %>% 
  do(reg = lm(usdzar_spot ~ (DlogReturn), data = .))

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

#head(RegressionCoeffs)

# BEST HEDGES::
hedges <- RegressionCoeffs %>%  
  filter(., term == "DlogReturn") %>% 
  select(., variable, estimate) %>% 
  arrange(., desc(estimate))
    
  # Top 10 hedges::
top.10.reg <-
  (RegressionCoeffs %>%  
     filter(., term == "DlogReturn") %>% 
     select(., variable, estimate) %>% 
     arrange(., desc(estimate)))[1:10,]

# Rand Plays
worst.10.reg <-
  (RegressionCoeffs %>%  
     filter(., term == "DlogReturn") %>% 
     select(., variable, estimate) %>% 
     arrange(., estimate))[1:10,]

