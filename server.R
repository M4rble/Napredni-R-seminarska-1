# server

library(shiny)
library(tidyquant)
library(ggplot2)

function(input, output) {
  
  output$sp500 <- renderPlot({
    end_date <- Sys.Date()
    start_date <- end_date - 365
    
    sp500_data <- tq_get("^GSPC",
                         from = start_date,
                         to = end_date,
                         get = "stock.prices",
                         frequency = "daily")
    
    ggplot(sp500_data, aes(x=date, y=close)) + geom_line() + geom_point() +
      ggtitle(paste("Closing price of SP500 for each day from", start_date)) +
      theme_bw() + xlab("date") + ylab("closing price")
    #if (input$plotMean) {
    #  abline(v = input$mean, col = "green")
    #}
  })
  
  #output$tableDensity <- renderTable({
  #  x = seq(-4,4,0.1)
  #  y = dnorm(x, mean = input$mean, sd = input$sd)
  #  df = data.frame(x, y)
  #  colnames(df) <- c("Values", "Probability density")
  #  df
  #})
  
}