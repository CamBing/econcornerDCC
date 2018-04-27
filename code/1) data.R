library(quantmod)
library(dplyr)


# To get data from Yahoo finance: -----------------------------------------

# Isolating JSE tickers ---------------------------------------------------


jse <- read.csv("data/tickers_exsmallcap.csv", header = F)

remove <- c("CATP.JO", "FVT.JO", "DCP.JO", "HET.JO", "TOPTR2.JO", "GFI.JO", "DIB.JO", "COH.JO", "SCD.JO")

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

getSymbols.yahoo("ZAR=X",
                 return.class = 'xts',
                 from = "2000-01-01",
                 to = Sys.Date(),
                 periodicity = "daily",
                 env = data)


plist <- eapply(data, Ad)
pframe <- do.call(merge, plist)

saveRDS(pframe, file = "data/data.rds")


