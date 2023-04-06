# ui

library(shiny)

navbarPage(
  title = "NV Investments, where people will envy you your investments",
      tabPanel("Pregled"),
      tabPanel("Analiza finančnih instrumentov", plotOutput("sp500")),
      tabPanel("Optimizacija portfelja")
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

)