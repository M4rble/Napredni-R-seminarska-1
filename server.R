# server

library(shiny)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(TTR)
library(gridExtra)
library(grid)
library(ggpubr)

###############################################################################
# uporabljene funkcije


function(input, output) {

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
                             "Ethereum" = "ETH_USD",
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
          }
          

  
        output$plotIndex <- renderPlot({
          generate_plot(input$index, input$price, input$frequency, input$datum, 
                        input$indikatorji, input$ma_period, input$rsi_period, 
                        input$nFast, input$nSlow, input$nSig)
        }, width = 1100, height = 800)
        
        
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
}


  
  #output$tabela <- renderTable({
  #  price <- switch(input$price,
  #                  "Opening" = "open",
  #                  "Closing" = "close",
  #                  "Highest" = "high",
  #                  "Lowest" ="low",
  #  )
  #  
  #  sp500_data <- tq_get("^GSPC",
  #                       from = input$datum[1],
  #                       to = input$datum[2],
  #                       get = "stock.prices",
  #                       frequency = "daily")
  #  
  #  print(sp500_data)
  #})
  
  #if (input$plotMean) {
  #  abline(v = input$mean, col = "green")
  #}
  
  #output$tableDensity <- renderTable({
  #  x = seq(-4,4,0.1)
  #  y = dnorm(x, mean = input$mean, sd = input$sd)
  #  df = data.frame(x, y)
  #  colnames(df) <- c("Values", "Probability density")
  #  df
  #})
  
