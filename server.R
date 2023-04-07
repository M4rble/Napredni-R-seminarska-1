# server

library(shiny)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(TTR)

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
    
        sp500_data <- switch(input$frequency,
                             "daily" = sp500_data <- tq_get("^GSPC",
                                                            from = input$datum[1],
                                                            to = input$datum[2],
                                                            get = "stock.prices"),
                             "weekly" = get_period_data("^GSPC",
                                                        input$datum[1],
                                                        input$datum[2],
                                                        "week"),
                             "monthly" = get_period_data("^GSPC",
                                                         input$datum[1],
                                                         input$datum[2],
                                                         "month"),
                             "quarterly" = get_period_data("^GSPC",
                                                           input$datum[1],
                                                           input$datum[2],
                                                           "quarter"),
                             "yearly" = get_period_data("^GSPC",
                                                        input$datum[1],
                                                        input$datum[2],
                                                        "year"))
          
        price <- switch(input$price,
                        "Opening" = sp500_data$open,
                        "Closing" = sp500_data$close,
                        "Highest" = sp500_data$high,
                        "Lowest" = sp500_data$low)
        
        indeksi <- switch(input$index,
                          "S&P500" = ggplot(sp500_data, aes(x=date)) + geom_line(aes(y=price)) +
                            ggtitle(paste(input$price, "price of SP500 for", input$frequency ,"data from",
                                          format(input$datum[1], "%d-%m-%Y"), "to", 
                                          format(input$datum[2], "%d-%m-%Y"))) +
                            theme_bw() + xlab("Date") +  
                            scale_y_continuous(name = paste(input$price, "price"), 
                                               sec.axis = sec_axis(~., name = "RSI")),
                          "NASDAQ" = "ni še grafa",
                          "DowJones Index"= "ni še grafa",
                          "STOXX Europe 50 Index"= "ni še grafa",
                          "DAX"= "ni še grafa")
        
        if (!is.null(input$indikatorji) && input$indikatorji == "Moving average"){
          indeksi <- indeksi + geom_line(aes(y=SMA(price, n=50)), col = "green")
        }
          return(indeksi)
        
        
        if (!is.null(input$indikatorji) && input$indikatorji == "RSI"){
          indeksi <- indeksi + geom_line(aes(y=RSI(price, n=14)), col = "red")
          return(indeksi)
        }
        #if (!is.null(input$indikatorji) && input$indikatorji == "MACD"){
        #  macd = MACD(price)
        #  indeksi <- indeksi + geom_line(aes(x=date, y=macd[,1]), col = "blue") + 
        #    geom_line(aes(x=date, y=macd[,2]), col = "yellow")
        #}
        
        })
  
  
  output$plotIndex <- renderPlot({
    
    plot()

  })
  
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
  
