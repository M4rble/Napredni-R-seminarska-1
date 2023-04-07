# testing file

library(tidyquant)
library(xts)
library(dplyr)

end_date <- Sys.Date()
start_date <- end_date - 5*365

sp500_data <- tq_get("^GSPC",
                     from = start_date,
                     to = end_date,
                     get = "stock.prices")

##################################################################################

get_period_data <- function(ticker, start_date, end_date, period) {
  # Get historical daily price data
  daily_data <- tq_get(ticker,
                       from = start_date,
                       to = end_date,
                       get = "stock.prices")
  
  # Convert daily data to weekly
  xts_daily <- xts(daily_data$adjusted, order.by = daily_data$date)
  xts_period <- to.period(xts_daily, period, indexAt = "firstof")
  colnames(xts_period) <- c("open", "high", "low", "close")
  
  # Convert the xts object back to a data frame
  period_data <- data.frame(date = index(xts_period), coredata(xts_period))
  
  return(period_data)
}

a <- get_period_data("^GSPC", start_date, end_date, "weeks")

library(TTR)
a$MA_50 <- SMA(a$open, n=50)

macd <- MACD(a$open)

################################################################################


tickers <- c("^GSPC", "^IXIC", "^DJI")

get_historical_data <- function(ticker) {
  tq_get(ticker,
         from = start_date,
         to = end_date,
         get = "stock.prices")
}

# Fetch historical price data for the indices
historical_data <- lapply(tickers, get_historical_data)
names(historical_data) <- c("S&P_500", "NASDAQ", "Dow_Jones")


historical_data$`S&P_500`




#library(PortfolioAnalytics)
#
#data(edhec)
#asset_returns <- edhec
#
## Get the column names of the returns data
#asset_names <- colnames(asset_returns)
#
## Create a portfolio specification object using asset_names
#port_spec <- portfolio.spec(assets = asset_names)
#
## Get the class of the portfolio specification object
#class(port_spec)
#
#print(port_spec)

