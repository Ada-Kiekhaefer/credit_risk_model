#
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

load('./data/data_and_log_model.RData')

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput('stock_symbol', 'Enter stock symbol: ', 'AAPL')),
    mainPanel(
      textOutput("symbol"),
      plotOutput('trend'),
      plotOutput('volume'))
  )
)
server <- function(input, output){
  output$symbol <- renderText({
    paste(toupper(input$stock_symbol), 'stock time-series plot')
  })
  output$trend <- renderPlot({
    # ggplot(subset(all_stocks, Name == toupper(input$stock_symbol))) +
    #   geom_line(aes(x = date, y = close)) +
    #   labs(y = 'stock price (dollars)')
    s_name <- all_stocks %>% 
      filter(Name == toupper(input$stock_symbol))
    ggplot(s_name) +
      geom_line(aes(x = date, y = close)) +
      labs(y = 'stock price (dollars)')
  }) 
  output$volume <- renderPlot({
    ggplot(subset(all_stocks, Name == toupper(input$stock_symbol))) +
      geom_line(aes(x = date, y = volume)) +
      labs(y = 'stock volume')
  })
}
shinyApp(ui = ui, server = server)