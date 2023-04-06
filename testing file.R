# testing file

library(tidyquant)

end_date <- Sys.Date()
start_date <- end_date - 365

sp500_data <- tq_get("^GSPC",
                     from = start_date,
                     to = end_date,
                     get = "stock.prices",
                     frequency = "daily")

sp500_2 <- tq_get("SPY",
                  from = start_date,
                  to = end_date,
                  get = "stock.prices",
                  frequency = "daily")














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

