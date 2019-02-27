# Library install lines commented out in case you need them
# install.packages('finreportr')
# install.packages('dplyr')
# install.packages('quantmod')

library(finreportr)
library(dplyr)
library(quantmod)
setDefaults(getSymbols.av, api.key="HQXQHIFN548EZELL")


# Looks like I have to build the data frame, let just pull columns from Amazon's balance sheet.
# Due to Amazon's high stock price, it's doubtful the algorithm will buy Amazon stock..
amazon_2017 <- data.frame(GetBalanceSheet("AMZN", 2017))
# Next line extrancts datasheet column names and removes duplicates
features <- distinct(data.frame(amazon_2017$Metric))
# Next line transposes features rows into one row
features_transposed <- as.data.frame(t(features$amazon_2017.Metric))[1 ,]
# Next line creates NA row with arbitrary column names
all_stocks <- data.frame(matrix(ncol=nrow(features)))
# This creates our empty all-column, 1 row data frame. The row is all NA's
colnames(all_stocks) <- features_transposed[1,]
# The next line removes the NA row
all_stocks <- all_stocks[-1,]
# Cleaning up data I don't need anymore
rm(features_transposed, amazon_2017, features)

# Now that we have all the column names from the company's balance 
# sheet, let's grab a list of all stocks listed on NASDAQ.
# Thank you to link below for this csv file
# https://datahub.io/core/nasdaq-listings#resource-nasdaq-listed-symbols
companySymbolsAndNames <- read.csv('nasdaq-listed-symbols_csv.csv')
# The quoantmod package can give us stock price data from Yahoo Finance/Alpha Vantage
# example below
rm(lastQuote)
# Lets start off with just 10 companies
masterDataFrame <- data.frame(companySymbolsAndNames)
rm(companySymbolsAndNames)
# Prof Shukla, importing this next csv file takes about 2.5 minutes
historicalStockPrices <- read.csv('historical_stock_prices.csv')
historicalStockPrices$date <- as.Date(historicalStockPrices$date, format= "%Y-%m-%d")

# Let's try filtering out dates that are NOT January 19th, 2018 ('2018-01-19')
historicalStockPrices$dateInteger <- ifelse(historicalStockPrices$date == as.Date('2018-01-19'), as.Date(historicalStockPrices$date), NA)
historicalStockPrices$date <- as.Date(historicalStockPrices$dateInteger)
historicalStockPrices <- historicalStockPrices[ , -which(names(historicalStockPrices) %in% c("dateInteger"))]
# Checking if there's any missing data.
January19_2018 <- historicalStockPrices[complete.cases(historicalStockPrices), ]
# rm(i, lastQuote, lastSharePrice, ptm)


masterDataFrame$Jan19.2018ClosingPrice <- NA

for (i in 1:nrow(masterDataFrame)){
  currentRowInMaster <- i
  currentCompanyTicker <- masterDataFrame[currentRowInMaster,1]
  regexTicker <- paste0('^', currentCompanyTicker, '$')
  # print(regexTicker)
  indexOfMatchingRow <- grep(regexTicker, January19_2018$ticker)
  if (length(indexOfMatchingRow) == 1){
    relevantHistoricalData <- January19_2018[indexOfMatchingRow,]
    closingPriceThatDay <- round(as.numeric(relevantHistoricalData$close), digits = 2)
    masterDataFrame[currentRowInMaster, which(names(masterDataFrame) %in% c("Jan19.2018ClosingPrice"))] <- closingPriceThatDay
  }
}

masterDataFrame <- masterDataFrame[complete.cases(masterDataFrame), ]
# rm(January19_2018)
# rm(closingPriceThatDay, currentRowInMaster, currentCompanyTicker, indexOfMatchingRow)
# rm(i, regexTicker)

# install.packages("Quandl")
library(Quandl)


xgGJbUuCWrY81whTDs8n
tempEnviorment <- new.env()
getFinancials("AAPL", env = tempEnviorment, src = "google",
              auto.assign = TRUE)
# 
# 
# for (i in 1:nrow(allStockPrices)){
#   stockPriceEnv <- new.env()
#   
#   # latestTradingMetrics <- getQuote(masterDataFrame[i,1], src='yahoo')
#   # lastSharePrice <- latestTradingMetrics$Last
#   # print(paste(masterDataFrame[i,1], lastSharePrice))
#   # allStockPrices[i,1] <- getQuote(masterDataFrame[i,1])
# }
# 
# 
# 
# getSymbols.av("AMZN", sp500,
#               return.class = "xts",
#               periodicity = "daily",
#               adjusted = FALSE,
#               interval = "1min",
#               output.size = "compact",
#               data.type = "json")
# lastClosePrice <- sp500$AMZN$AMZN.Close[nrow(sp500$AMZN$AMZN.Close)]
# logRegData <- logRegData[ , -which(names(logRegData) %in% c("Country"))]
# 
# ibm <- data.frame(getSymbols("IBM", src="av", output.size="compact"))
# sp500$AMZN
# 
# alphavantage <- function(sym, 
#                          datatype=c("intraday", "daily", "adjdaily", "weekly", "monthly"), 
#                          outputsize=c("compact", "full"))
# c <-getSymbols.av("AMZN", globalenv(), api.key="HQXQHIFN548EZELL", output.size="full", periodicity="intraday")
# for (i in 1:nrow(allStockPrices)){
#   latestTradingMetrics <- getQuote(masterDataFrame[i,1], src='yahoo')
#   lastSharePrice <- latestTradingMetrics$Last
#   print(paste(masterDataFrame[i,1], lastSharePrice))
#   # allStockPrices[i,1] <- getQuote(masterDataFrame[i,1])
# }
