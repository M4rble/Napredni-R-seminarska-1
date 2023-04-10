# testing file

library(tidyquant)
library(xts)
library(dplyr)
library(tidyr)

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

###########################################################################

asset_tickers <- c("S&P500" = "^GSPC",
                   "NASDAQ" = "^IXIC",
                   "DowJones Index"= "^DJI",
                   "STOXX Europe 50 Index"= "^STOXX50E",
                   "DAX" = "^GDAXI",
                   "crude oil" = "CL=F",
                   "gold" = "GC=F",
                   "silver" = "SI=F",
                   "natural gas" = "NG=F",
                   "wheat" = "ZW=F",
                   "US 10-Year Treasury Bond Yield" = "^TNX",
                   "US 30-Year Treasury Bond Yield" = "^TYX",
                   "US 5-Year Treasury Bond Yield" = "^FVX",
                   "Bitcoin" = "BTC-USD",
                   "Ethereum" = "ETH-USD",
                   "XRP" = "XRP-USD",
                   "Solana" = "SOL-USD",
                   "Dogecoin" = "DOGE-USD")

asset_list <- c("S&P500",
                "NASDAQ",
                "DowJones Index",
                "STOXX Europe 50 Index",
                "DAX",
                "crude oil",
                "gold",
                "silver",
                "natural gas",
                "wheat",
                "US 10-Year Treasury Bond Yield",
                "US 30-Year Treasury Bond Yield",
                "US 5-Year Treasury Bond Yield",
                "Bitcoin",
                "Ethereum",
                "XRP",
                "Solana",
                "Dogecoin")

assets <- asset_tickers[match(asset_list, names(asset_tickers))]

end_date <- Sys.Date()
start_date <- end_date - 365 * 10 # Use 5 years of historical data

# Get historical data for selected assets
prices <- tq_get(assets,
                 from = start_date,
                 to = end_date,
                 get = "stock.prices") 

colnames(prices)[1] <- "symbol"

# Calculate daily returns
log_ret_tidy <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns',
               type = 'arithmetic')

log_ret_tidy <- na.omit(log_ret_tidy)

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = returns) %>%
  tk_xts()

mean_ret <- colMeans(log_ret_xts, na.rm=TRUE)
print(round(mean_ret, 5))

cov_mat <- cov(na.omit(log_ret_xts)) * 252


# random weights - normalizirane na 1
wts <- runif(n = length(assets))
print(wts)

print(sum(wts))

wts <- wts/sum(wts)
print(wts)

sum(wts)

# letni donos
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# letno tveganje
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)


# Since Risk free rate is 0% 

sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)

####################### gor je osnovna - ponovimo za 5000 portfeljev ##############

num_port <- 10000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(assets))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)


for (i in seq_along(port_returns)) {
  
  wts <- runif(length(assets))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

v <- 1:(length(portfolio_values)-3)

colnames(portfolio_values)[v] <- names(asset_tickers)[match(colnames(portfolio_values)[v], asset_tickers)]

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

p <- min_var %>%
  gather(colnames(min_var)[v], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_bw() + theme(axis.text = element_blank()) +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


p <- max_sr %>%
  gather(colnames(min_var)[v], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_bw() + theme(axis.text = element_blank()) +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')


ggplotly(p)

library(plotly)


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

