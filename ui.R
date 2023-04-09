# ui

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
                  
                  titlePanel("NV Investments"),
                  navbarPage("NV Investments - people will eNVy you your investments",
                             navbarMenu("Overview",
                                        tabPanel("How to use the app?", 
                                                 h2("How to use the app?")),
                                        tabPanel("Data description", h2("Data description")),
                                        tabPanel("Optimisation model description",
                                                 h2("Optimisation model description"))
                             ),
                             navbarMenu("Financial instruments analysis",
                                        
    ################################# Indices #############################################################                                    
                                        
                                        tabPanel("Indices", 
                                                 h2("Indices"), 
                                                 
                                                 sidebarPanel(
                                                   
                                                   selectInput("index", label = "Choose index:",
                                                               choices = c("S&P500",
                                                                           "NASDAQ",
                                                                           "DowJones Index",
                                                                           "STOXX Europe 50 Index",
                                                                           "DAX")),
                                                 hr(),
                                                   
                                                 radioButtons("price", label = "Choose what price to show:",
                                                              choices = c("Opening",
                                                                          "Closing",
                                                                          "Highest",
                                                                          "Lowest"),
                                                              inline = TRUE),
                                                 
                                                 radioButtons("frequency", label = "Choose frequency:",
                                                              choices = c("daily",
                                                                          "weekly",
                                                                          "monthly",
                                                                          "quarterly",
                                                                          "yearly"),
                                                              inline = TRUE),
                                                 
                                                 dateRangeInput("datum", label = "Choose date range:",
                                                                start = Sys.Date() - 365,
                                                                end = Sys.Date(),
                                                                format="dd-mm-yyyy",
                                                                max = Sys.Date()
                                                 ),
                                                 hr(),
                                                 
                                                 checkboxGroupInput("indikatorji", label = "Add indicator:",
                                                                    choices = c("Moving average",
                                                                               "RSI",
                                                                               "MACD"),
                                                                    inline = TRUE),
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('Moving average') > -1",
                                                   radioButtons("ma_period", label = "Choose moving average period:",
                                                                choices = c("10 units",
                                                                            "20 units",
                                                                            "50 units",
                                                                            "100 units",
                                                                            "200 units"),
                                                                selected = "10 units",
                                                                inline = TRUE)
                                                 ),
                                                 
                                                 
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('RSI') > -1",
                                                   sliderInput("rsi_period", label = "Choose RSI period:",
                                                                min = 3, max = 21,
                                                                value = 14)
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('MACD') > -1",
                                                   numericInput("nFast", label = "Input short-term MACD period:",
                                                                value = 12, min = 1, max = 18),
                                                   numericInput("nSlow", label = "Input long-term MACD period:",
                                                                value = 26, min = 19, max = 52),
                                                   numericInput("nSig", label = "Input period for moving average smoothing:",
                                                                value = 9, min = 1, max = 24)
                                                 )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotIndex")),
                                        
    #############################Commodities######################################################                                    
                                        
                                        tabPanel("Commodities", h2("Commodities"),
                                                 sidebarPanel(
                                                   
                                                   selectInput("commodities", label = "Choose commodity:",
                                                               choices = c("crude oil",
                                                                           "gold",
                                                                           "silver",
                                                                           "natural gas",
                                                                           "wheat")),
                                                   hr(),
                                                   
                                                   radioButtons("price_c", label = "Choose what price to show:",
                                                                choices = c("Opening",
                                                                            "Closing",
                                                                            "Highest",
                                                                            "Lowest"),
                                                                inline = TRUE),
                                                   
                                                   radioButtons("frequency_c", label = "Choose frequency:",
                                                                choices = c("daily",
                                                                            "weekly",
                                                                            "monthly",
                                                                            "quarterly",
                                                                            "yearly"),
                                                                inline = TRUE),
                                                   
                                                   dateRangeInput("datum_c", label = "Choose date range:",
                                                                  start = Sys.Date() - 365,
                                                                  end = Sys.Date(),
                                                                  format="dd-mm-yyyy",
                                                                  max = Sys.Date()
                                                   ),
                                                   hr(),
                                                   
                                                   checkboxGroupInput("indikatorji_c", label = "Add indicator:",
                                                                      choices = c("Moving average",
                                                                                  "RSI",
                                                                                  "MACD"),
                                                                      inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('Moving average') > -1",
                                                     radioButtons("ma_period_c", label = "Choose moving average period:",
                                                                  choices = c("10 units",
                                                                              "20 units",
                                                                              "50 units",
                                                                              "100 units",
                                                                              "200 units"),
                                                                  selected = "10 units",
                                                                  inline = TRUE)
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('RSI') > -1",
                                                     sliderInput("rsi_period_c", label = "Choose RSI period:",
                                                                 min = 3, max = 21,
                                                                 value = 14)
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('MACD') > -1",
                                                     numericInput("nFast_c", label = "Input short-term MACD period:",
                                                                  value = 12, min = 1, max = 18),
                                                     numericInput("nSlow_c", label = "Input long-term MACD period:",
                                                                  value = 26, min = 19, max = 52),
                                                     numericInput("nSig_c", label = "Input period for moving average smoothing:",
                                                                  value = 9, min = 1, max = 24)
                                                   )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotCommodities")),
    
    ############################## Bonds ################################################################
    
                                        tabPanel("Bonds", h2("Bonds"),
                                                 sidebarPanel(
                                                   
                                                   selectInput("bonds", label = "Choose bond:",
                                                               choices = c("US 5-Year Treasury Bond Yield",
                                                                           "US 10-Year Treasury Bond Yield",
                                                                           "US 30-Year Treasury Bond Yield")),
                                                   hr(),
                                                   
                                                   radioButtons("price_b", label = "Choose what price to show:",
                                                                choices = c("Opening",
                                                                            "Closing",
                                                                            "Highest",
                                                                            "Lowest"),
                                                                inline = TRUE),
                                                   
                                                   radioButtons("frequency_b", label = "Choose frequency:",
                                                                choices = c("daily",
                                                                            "weekly",
                                                                            "monthly",
                                                                            "quarterly",
                                                                            "yearly"),
                                                                inline = TRUE),
                                                   
                                                   dateRangeInput("datum_b", label = "Choose date range:",
                                                                  start = Sys.Date() - 365,
                                                                  end = Sys.Date(),
                                                                  format="dd-mm-yyyy",
                                                                  max = Sys.Date()
                                                   ),
                                                   hr(),
                                                   
                                                   checkboxGroupInput("indikatorji_b", label = "Add indicator:",
                                                                      choices = c("Moving average",
                                                                                  "RSI",
                                                                                  "MACD"),
                                                                      inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('Moving average') > -1",
                                                     radioButtons("ma_period_b", label = "Choose moving average period:",
                                                                  choices = c("10 units",
                                                                              "20 units",
                                                                              "50 units",
                                                                              "100 units",
                                                                              "200 units"),
                                                                  selected = "10 units",
                                                                  inline = TRUE)
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('RSI') > -1",
                                                     sliderInput("rsi_period_b", label = "Choose RSI period:",
                                                                 min = 3, max = 21,
                                                                 value = 14)
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('MACD') > -1",
                                                     numericInput("nFast_b", label = "Input short-term MACD period:",
                                                                  value = 12, min = 1, max = 18),
                                                     numericInput("nSlow_b", label = "Input long-term MACD period:",
                                                                  value = 26, min = 19, max = 52),
                                                     numericInput("nSig_b", label = "Input period for moving average smoothing:",
                                                                  value = 9, min = 1, max = 24)
                                                   )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotBonds")),
                                        tabPanel("Cryptocurrencies", h2("Cryptocurrencies"))
                             ),
                             navbarMenu("Portfolio optimisation",
                                        tabPanel("Mean-variance model", h2("Mean-variance model")),
                                        tabPanel("Black-Litterman model", h2("Black-Litterman model"))
                             )
      


)
)
)



#sidebarLayout(
#  sidebarPanel(
#    numericInput("mean", "Mean", min = -2, max = 2, value = 0, step = 0.1),
#    sliderInput("sd", "Standard deviation", min = 0, max = 2, value = 1, step = 0.1,
#                animate=TRUE),
#    checkboxInput("plotMean", "Plot vertical line at mean?", value = FALSE),
#    hr(),
#    selectInput("curveColor", "Choose color of curve:",
#                choices = c("black", "red", "blue"), selected = "black"),
#    hr(),
#    textInput("plotTitle", "Specify title plot", value="A plot"),
#    textInput("verticalLabel", "Specify vertical label", value="probability density"),
#    hr(),
#    submitButton("Update View", icon("refresh"))
#  ),