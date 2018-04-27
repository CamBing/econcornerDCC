
# Data download -----------------------------------------------------------


# Smaller sample test run

jse_test <- read.csv("data/tickers_test.csv", header = F)
Symbols <- as.character(jse_test$V1)
data <- new.env()

getSymbols.yahoo(Symbols,
                 return.class = 'xts',
                 from = "2007-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

plist <- eapply(data, Ad)
pframe <- do.call(merge, plist)


# Ex- Small cap -----------------------------------------------------------

# Isolating JSE tickers ---------------------------------------------------

jse <- read.csv("data/tickers_exsmallcap.csv", header = F)

# remove stocks giving errors
#remove <- c("TFGP.JO", "BAWP.JO", "IBLUSD.JO", "SWXTR2.JO", "TOPTR2.JO", "FSRP.JO")
#remove <- c("AMIB50.JO", "AOO.JO", "ASHWGB.JO", "BFS.JO", "AIP.JO", "BIK.JO", "AFX.JO")
remove <- c("CATP.JO", "FVT.JO", "DCP.JO", "HET.JO")

jse <- 
  jse %>% filter(!(V1 %in% remove))
# Back to the data grabbing:


# Grabbing the data -------------------------------------------------------


Symbols <- as.character(jse$V1)
data <- new.env()

getSymbols.yahoo(Symbols,
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)

plist <- eapply(data, Ad)
pframe <- do.call(merge, plist)

#Test
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