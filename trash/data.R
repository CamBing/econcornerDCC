library(quantmod)
library(dplyr)

# Ex- Small cap -----------------------------------------------------------

# Isolating JSE tickers ---------------------------------------------------

jse <- read.csv("data/tickers_exsmallcap.csv", header = F)

remove <- c("CATP.JO", "FVT.JO", "DCP.JO", "HET.JO", "TOPTR2.JO", "GFI.JO", "DIB.JO", "COH.JO", "SCD.JO")

jse <- 
  jse %>% filter(!(V1 %in% remove))

# Grabbing the data -------------------------------------------------------

symbol1 <- as.character(jse[1:20, 1])
symbol2 <- as.character(jse[21:40, 1])
symbol3 <- as.character(jse[41:60, 1])
symbol4 <- as.character(jse[61:80, 1])
symbol5 <- as.character(jse[81:100, 1])
symbol6 <- as.character(jse[101:120, 1])
symbol7 <- as.character(jse[121:140, 1])
symbol8 <- as.character(jse[141:175, 1])

data <- new.env()

getSymbols.yahoo(symbol1,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)


getSymbols.yahoo(symbol2,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol3,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol4,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol5,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol6,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol7,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

getSymbols.yahoo(symbol8,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

plist <- eapply(data, Ad)
pframe <- do.call(merge, plist)


saveRDS(pframe, file = "data/data.rds")
