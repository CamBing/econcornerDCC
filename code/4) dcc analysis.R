# DCC Approach

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(rmsfuns)
library(lubridate)
library(broom)
library(rugarch)
library(rmgarch)
library(tbl2xts)
library(MTS)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)

# Prep data ---------------------------------------------------------------

# Data must be in wide xts format

dcc.data <- rtn.final %>% 
  select(date, variable, DlogReturn) %>% 
  tbl_xts(., spread_by = "variable")

dcc.data.clean <- rtn.final %>% 
  select(date, variable, DlogReturn)



# Trimming dataset --------------------------------------------------------

Pct_Valid <- 0.9 # works with 0.9 # This can change of course. 70% valid data over period at least

NDates.dcc <- length(unique(dcc.data.clean %>% pull(date)) ) 

HoldTickers.dcc <-
  dcc.data.clean %>% 
  group_by(variable) %>% 
  mutate(N_Valid = ifelse(DlogReturn == 0, 0, 1) ) %>% summarise(N_Valid_Pct = sum(N_Valid)/NDates.dcc) %>% 
  filter(N_Valid_Pct >= Pct_Valid) %>% pull(variable) %>% unique()



dcc.data.rtn <- 
  dcc.data.clean %>% 
  filter(variable %in% HoldTickers.dcc | variable == "ZAR") %>% 
  tbl_xts(., spread_by = "variable")


# Build DCC ---------------------------------------------------------------
# NOTE: data needs to be in wide format (tbl_xts() works)

uspec <-
  ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
             mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
             distribution.model = "sstd")

multi_univ_garch_spec <- multispec(replicate(ncol(dcc.data.rtn), uspec))

spec.dcc = dccspec(multi_univ_garch_spec, 
                   dccOrder = c(1, 1), 
                   distribution = 'mvnorm',
                   lag.criterion = c("AIC", "HQ", "SC", "FPE")[1],
                   model = c("DCC", "aDCC")[1])


cl = makePSOCKcluster(10)

# This takes a while to run (5 mins)
multf = multifit(multi_univ_garch_spec, dcc.data.rtn, cluster = cl)  

fit.dcc = dccfit(spec.dcc, 
                 data = dcc.data.rtn, 
                 solver = 'solnp', 
                 cluster = cl, 
                 fit.control = list(eval.se = FALSE), 
                 fit = multf)


# We can now test the model's fit as follows:
#   Let's use the covariance matrices to test the adequacy of MV model in fitting mean residual processes:
RcovList <- rcov(fit.dcc) # This is now a list of the monthly covariances of our DCC model series.
covmat = matrix(RcovList,nrow(dcc.data.rtn),ncol(dcc.data.rtn)*ncol(dcc.data.rtn),byrow=TRUE)
mc1 = MTS::MCHdiag(dcc.data.rtn,covmat) # NEED TO INTERPRET RESULTS OF TEST

dcc.time.var.cor <- rcor(fit.dcc)
#print(dcc.time.var.cor)

# Plotting
dcc.time.var.cor <- aperm(dcc.time.var.cor,c(3,2,1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)

# Renaming function ====
renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {
  
  ncolrtn <- ncol(ReturnSeries)
  namesrtn <- colnames(ReturnSeries)
  paste(namesrtn, collapse = "_")
  
  nam <- c()
  xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
  nam <- c()
  for (j in 1:(ncolrtn)) {
    for (i in 1:(ncolrtn)) {
      nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
    }
  }
  
  colnames(DCC.TV.Cor) <- nam
  
  # So to plot all the time-varying correlations wrt SBK:
  # First append the date column that has (again) been removed...
  DCC.TV.Cor <- 
    data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
    mutate(date = as.Date(date)) %>%  tbl_df() 
  
  DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)
  
  DCC.TV.Cor
  
}

#end of function

dcc.time.var.cor <-
  renamingdcc(ReturnSeries = dcc.data.rtn, DCC.TV.Cor = dcc.time.var.cor)

all.correl <- 
  ggplot(dcc.time.var.cor %>% filter(grepl("ZAR_", Pairs ), !grepl("_ZAR", Pairs)) ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Dynamic Conditional Correlations: ZAR")
all.correl

#=== === === ===
# Top 10 graph
#=== === === ===
# Top 10 by average Rho
zarDCC <- dcc.time.var.cor %>% filter(grepl("ZAR_", Pairs ), !grepl("_ZAR", Pairs))
top.10.reg.list.avg <-
  (zarDCC %>% group_by(Pairs) %>% 
  summarise(avg = mean(Rho)) %>% 
  arrange(., desc(avg)))[1:10,]

top.10.graph.avg <- zarDCC %>% filter(Pairs %in% c(top.10.reg.list.avg$Pairs)) %>% 
  ggplot( ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Top 10 Dynamic Conditional Correlations: ZAR") 
top.10.graph.avg

#From regression approach

top.10.reg.list <- top.10.reg[,1] 
top.10.reg.list$variable <- paste0('ZAR_', top.10.reg.list$variable)
colnames(top.10.reg.list) <- "Pairs"
#top.10.reg.list <- top.10.reg.list$Pairs

top.10.graph <- zarDCC %>% filter(Pairs %in% top.10.reg.list$Pairs) %>% 
  ggplot( ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Top 10 Dynamic Conditional Correlations: ZAR") 

top.10.graph

#interactive graph
library(plotly)
plot.10 <- zarDCC %>% filter(Pairs %in% c(top.10.reg.list.avg$Pairs))
plot.10$Pairs <- gsub(plot.10$Pairs, pattern = "ZAR_", replacement = "")

p <- ggplot(data=plot.10, aes(x=date, y=Rho, colour = Pairs)) +
  geom_line() + 
  theme_hc() +
  ggtitle("Top 10 Dynamic Conditional Correlations: ZAR") +
  xlab("Date")


ggplotly(p, tooltip = "Pairs")

         