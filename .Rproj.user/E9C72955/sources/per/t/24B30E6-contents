#Regression approach


# Prep data ---------------------------------------------------------------

Regression_data <-
  rtn.final #%>% select("date", "variable", "DlogReturn")

zar <-
  Regression_data %>%  filter(variable == "ZAR") %>% 
  select("date", "DlogReturn") %>% 
  rename("usdzar_spot" = DlogReturn)

Regression_data <- 
  right_join(Regression_data, zar, by = "date") %>% 
  filter(variable != "ZAR") %>% 
  filter(!is.na(DlogReturn))

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

