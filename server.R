# server

library(shiny)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(TTR)
library(gridExtra)
library(grid)
library(ggpubr)
library(plotly)
library(tidyverse)
library(timetk)

###############################################################################
# uporabljene funkcije


function(input, output,session) {

  # iz dnevnih v različne podatke
  
          get_period_data <- function(ticker, start_date, end_date, period) {
            # dobimo zgodovinske cene iz spleta
            daily_data <- tq_get(ticker,
                                 from = start_date,
                                 to = end_date,
                                 get = "stock.prices")
            
            # z uporabo paketa xts pretvorimo dnevne podatke v željeno periodo
            xts_daily <- xts(daily_data$adjusted, order.by = daily_data$date)
            xts_period <- to.period(xts_daily, period, indexAt = "firstof")
            colnames(xts_period) <- c("open", "high", "low", "close")
            
            # xts objekt pretvorimo nazaj v dataframe
            period_data <- data.frame(date = index(xts_period), coredata(xts_period))
            
            return(period_data)
          }
          
          generate_plot <- function(index, price_type, frequency, datum, 
                                    indikatorji, ma_period, rsi_period,
                                    nFast, nSlow, nSig) {
            ticker <- switch(index,
                             "S&P500" = "^GSPC",
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
            
            data <- na.omit(switch(frequency,
                                   "daily" = tq_get(ticker,
                                                    from = datum[1],
                                                    to = datum[2],
                                                    get = "stock.prices"),
                                   "weekly" = get_period_data(ticker,
                                                              datum[1],
                                                              datum[2],
                                                              "week"),
                                   "monthly" = get_period_data(ticker,
                                                               datum[1],
                                                               datum[2],
                                                               "month"),
                                   "quarterly" = get_period_data(ticker,
                                                                 datum[1],
                                                                 datum[2],
                                                                 "quarter"),
                                   "yearly" = get_period_data(ticker,
                                                              datum[1],
                                                              datum[2],
                                                              "year")))
            
            price <- switch(price_type,
                            "Opening" = data$open,
                            "Closing" = data$close,
                            "Highest" = data$high,
                            "Lowest" = data$low)
            
            indeksi <-  ggplot(data, aes(x=date)) + geom_line(aes(y=price)) +
              ggtitle(paste(price_type, "price of", index, "for", frequency, "data from",
                            format(datum[1], "%d-%m-%Y"), "to", 
                            format(datum[2], "%d-%m-%Y"))) +
              theme_bw() + xlab("Date") + ylab(paste(price_type, "price"))
            
            if ("Moving average" %in% indikatorji) {
              
              ma_period <- switch(ma_period,
                                  "10 units" = 10,
                                  "20 units" = 20,
                                  "50 units" = 50,
                                  "100 units" = 100,
                                  "200 units" = 200)
              
              ma <- SMA(price, n = ma_period)
              indeksi <- indeksi + geom_line(aes(x = date, y = ma, color="ma_period")) +
                scale_color_manual(name = "Moving average", values = c(ma_period = "green"),
                                   labels = c(ma_period))
            }
            
            
            rsi_plot <- ggplot() + theme_void()
            if ("RSI" %in% indikatorji) {
              
              rsi <- RSI(price, n = rsi_period)
              rsi_data <- data.frame(date = data$date, rsi = rsi)
              rsi_plot <- rsi_plot + geom_line(data=rsi_data, aes(x = date, y = rsi, color = "rsi_period")) +
                theme_bw() + xlab("Date") + geom_hline(aes(yintercept=70)) + 
                geom_hline(aes(yintercept=30)) + ylab("RSI") + 
                scale_color_manual(name = "RSI", values = c(rsi_period = "red"),
                                   labels = c(rsi_period))
            }
            
            macd_plot <- ggplot() + theme_void()
            if ("MACD" %in% indikatorji) {
              macd_df <- as.data.frame(MACD(price, nFast = nFast, nSlow = nSlow,
                                            nSig = nSig, maType = "SMA"))
              macd <- macd_df$macd
              signal <- macd_df$signal
              macd_data <- data.frame(date = data$date, macd = macd, signal = signal)
              macd_plot <- macd_plot + geom_line(data=macd_data, aes(x = date, y = macd, color = "macd")) +
                geom_line(data=macd_data, aes(x = date, y = signal, color="signal")) +
                theme_bw() + xlab("Date") + ylab("MACD") + 
                scale_color_manual(name = "MACD", values = c(macd = "blue", signal = "purple"),
                                   labels = c("MACD", "Signal"))
            }
            
            indeksi <- grid.arrange(indeksi, rsi_plot, macd_plot, nrow = 3, heights = c(3,1,1))
            return(indeksi)
            
            #id1 <- ggplotly(indeksi)
            
            #return(list(id1,rsi_plot, macd_plot))
          }
          
        output$plotIndex <- renderPlot({
          generate_plot(input$index, input$price, input$frequency, input$datum, 
                        input$indikatorji, input$ma_period, input$rsi_period, 
                        input$nFast, input$nSlow, input$nSig)
          }, width = 1100, height = 800)
  
        #output$plotIndex1 <- renderPlotly({
        #  generate_plot(input$index, input$price, input$frequency, input$datum, 
        #                input$indikatorji, input$ma_period, input$rsi_period, 
        #                input$nFast, input$nSlow, input$nSig)[[1]]
        #})#, width = 1100, height = 800)
        #output$plotIndex2 <- renderPlotly({
        #  generate_plot(input$index, input$price, input$frequency, input$datum, 
        #                input$indikatorji, input$ma_period, input$rsi_period, 
        #                input$nFast, input$nSlow, input$nSig)[[2]]
        #})#, width = 1100, height = 800)
        #output$plotIndex3 <- renderPlotly({
        #  generate_plot(input$index, input$price, input$frequency, input$datum, 
        #                input$indikatorji, input$ma_period, input$rsi_period, 
        #                input$nFast, input$nSlow, input$nSig)[[3]]
        #})#, width = 1100, height = 800)
        
        
        output$plotCommodities <- renderPlot({
          generate_plot(input$commodities, input$price_c, input$frequency_c, input$datum_c, 
                        input$indikatorji_c, input$ma_period_c, input$rsi_period_c, 
                        input$nFast_c, input$nSlow_c, input$nSig_c)
        }, width = 1100, height = 800)
        
        output$plotBonds <- renderPlot({
          generate_plot(input$bonds, input$price_b, input$frequency_b, input$datum_b, 
                        input$indikatorji_b, input$ma_period_b, input$rsi_period_b, 
                        input$nFast_b, input$nSlow_b, input$nSig_b)
        }, width = 1100, height = 800)
        
        output$plotCryptocurrencies <- renderPlot({
          generate_plot(input$cryptocurrencies, input$price_crypto, input$frequency_crypto, input$datum_crypto, 
                        input$indikatorji_crypto, input$ma_period_crypto, input$rsi_period_crypto, 
                        input$nFast_crypto, input$nSlow_crypto, input$nSig_crypto)
        }, width = 1100, height = 800)
        
        
        
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
        
        observe({
          if(input$selectall == 0) return(NULL) 
          else if (input$selectall%%2 == 0)
          {
            updateCheckboxGroupInput(session,"assets","Select Assets:",choices=asset_list)
          }
          else
          {
            updateCheckboxGroupInput(session,"assets","Select Assets:",choices=asset_list,selected=asset_list)
          }
        })
        
        optimal_weights <- function(assets){
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
          
          assets <- asset_tickers[match(assets, names(asset_tickers))]
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
          
          cov_mat <- cov(na.omit(log_ret_xts)) * 252
          
          
          # random weights - normalizirane na 1
          wts <- runif(n = length(assets))
          
          wts <- wts/sum(wts)
          
          # letni donos
          port_returns <- (sum(wts * mean_ret) + 1)^252 - 1
          
          # letno tveganje
          port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
          
          
          # Since Risk free rate is 0% 
          
          sharpe_ratio <- port_returns/port_risk
          
          
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
          
          p1 <- min_var %>%
            gather(colnames(min_var)[v], key = Asset,
                   value = Weights) %>%
            mutate(Asset = as.factor(Asset)) %>%
            ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
            geom_bar(stat = 'identity') +
            theme_bw() + theme(axis.text = element_blank()) +
            labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
            scale_y_continuous(labels = scales::percent) 
          
          p2 <- max_sr %>%
            gather(colnames(min_var)[v], key = Asset,
                   value = Weights) %>%
            mutate(Asset = as.factor(Asset)) %>%
            ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
            geom_bar(stat = 'identity') +
            theme_bw() + theme(axis.text = element_blank()) +
            labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
            scale_y_continuous(labels = scales::percent) 
          
          p3 <- portfolio_values %>%
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
          
          
          fig1 <- ggplotly(p1)
          fig2 <- ggplotly(p2)
          fig3 <- ggplotly(p3)
          #fig <- subplot(ggplotly(p1), ggplotly(p2), ggplotly(p3), nrows=3, margin = 0.1)
          #annotations = list(
          #  list(x=0.2, y=1, text = "Minimum Variance Portfolio Weights"),
          #  list(x=0.2, y=0.66, text = "Tangency Portfolio Weights"),
          #  list(x=0.2, y=0.33, text = "Portfolio Optimization & Efficient Frontier")
          #)
          #fig <- fig %>% layout(annotations = annotations)
          return(list(fig1, fig2, fig3))
            
        } 
        
        plot_weights <- eventReactive(input$calculate, {
          optimal_weights(input$assets)
        })
        
        output$optimal_weights1 <- renderPlotly({plot_weights()[[1]]})
        output$optimal_weights2 <- renderPlotly({plot_weights()[[2]]})
        output$optimal_weights3 <- renderPlotly({plot_weights()[[3]]})
        
        
}

  
