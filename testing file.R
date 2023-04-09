# testing file

library(tidyquant)
library(xts)
library(dplyr)

end_date <- Sys.Date()
start_date <- end_date - 20*365

sp500_data <- tq_get("^GSPC",
                     from = start_date,
                     to = end_date,
                     get = "stock.prices")

b <- tq_transmute(sp500_data,
                  select     = sp500_data$high, 
                  mutate_fun = to.period, 
                  period     = "days")

dax_data <- tq_get("^GDAXI",
                     from = start_date,
                     to = end_date,
                     get = "stock.prices")
summary(dax_data)

###########################################################################

get_period_data <- function(ticker, start_date, end_date, price, period) {
  # Get historical daily price data
  data <- tq_get(ticker,
                 from = start_date,
                 to = end_date,
                 get = "stock.prices") %>% 
    tq_transmute(select = price, 
                 mutate_fun = to.period, 
                 period = period)
  return(data)
}

get_period_data("^GSPC",
                start_date,
                end_date,
                "open",
                "days")

##################################################################################

get_period_data <- function(ticker, start_date, end_date, period) {
  # Get historical daily price data
  daily_data <- tq_get(ticker,
                       from = start_date,
                       to = end_date,
                       get = "stock.prices")
  
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

