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

# iz dnevnih v različne podatke

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

# iz dnevnih v mesečne podatke

function(input, output) {
  
        plot <- reactive({
          
        ticker <- switch(input$index,
                         "S&P500" = "^GSPC",
                         "NASDAQ" = "^IXIC",
                         "DowJones Index"= "^DJI",
                         "STOXX Europe 50 Index"= "^STOXX50E",
                         "DAX" = "^GDAXI")
    
        data <- na.omit(switch(input$frequency,
                             "daily" = tq_get(ticker,
                                              from = input$datum[1],
                                              to = input$datum[2],
                                              get = "stock.prices"),
                             "weekly" = get_period_data(ticker,
                                                        input$datum[1],
                                                        input$datum[2],
                                                        "week"),
                             "monthly" = get_period_data(ticker,
                                                         input$datum[1],
                                                         input$datum[2],
                                                         "month"),
                             "quarterly" = get_period_data(ticker,
                                                           input$datum[1],
                                                           input$datum[2],
                                                           "quarter"),
                             "yearly" = get_period_data(ticker,
                                                        input$datum[1],
                                                        input$datum[2],
                                                        "year")))
          
        price <- switch(input$price,
                        "Opening" = data$open,
                        "Closing" = data$close,
                        "Highest" = data$high,
                        "Lowest" = data$low)
        
        indeksi <-  ggplot(data, aes(x=date)) + geom_line(aes(y=price)) +
                            ggtitle(paste(input$price, "price of", input$index, "for", input$frequency, "data from",
                                          format(input$datum[1], "%d-%m-%Y"), "to", 
                                          format(input$datum[2], "%d-%m-%Y"))) +
                            theme_bw() + xlab("Date") + ylab(paste(input$price, "price"))
        
        if ("Moving average" %in% input$indikatorji) {
          
          ma_period <- switch(input$ma_period,
                              "10 units" = 10,
                              "20 units" = 20,
                              "50 units" = 50,
                              "100 units" = 100,
                              "200 units" = 200)

          ma <- SMA(price, n = ma_period)
          indeksi <- indeksi + geom_line(aes(x = data$date, y = ma, color="ma_period")) +
                               scale_color_manual(name = "Moving average", values = c(ma_period = "green"),
                               labels = c(input$ma_period))
        }
        
        
        rsi_plot <- ggplot() + theme_void()
        if ("RSI" %in% input$indikatorji) {
          
          rsi <- RSI(price, n = input$rsi_period)
          rsi_data <- data.frame(date = data$date, rsi = rsi)
          rsi_plot <- rsi_plot + geom_line(data=rsi_data, aes(x = date, y = rsi, color = "rsi_period")) +
            theme_bw() + xlab("Date") + geom_hline(aes(yintercept=70)) + 
            geom_hline(aes(yintercept=30)) + ylab("RSI") + 
            scale_color_manual(name = "RSI", values = c(rsi_period = "red"),
                               labels = c(input$rsi_period))
        }
        
        macd_plot <- ggplot() + theme_void()
        if ("MACD" %in% input$indikatorji) {
          macd_df <- as.data.frame(MACD(price, nFast = input$nFast, nSlow = input$nSlow,
                                        nSig = input$nSig, maType = "SMA"))
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
        
        })
  
  
  output$plotIndex <- renderPlot({
    
    plot()

  }, width= 1100, height=800)
  
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
  
