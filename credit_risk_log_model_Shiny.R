#
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

load('./data/loan_data_no_outliers.RData')
#log_model<- glm(loan_status ~ age + annual_inc, family='binomial', data = loan_data_no_outliers)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
  textInput('borrower_age', 'Enter age: '),
  textInput('borrower_annual_income', 'Enter annual income: '),
  selectInput("indepvar", label = h3("Explanatory variable: "), 
              choices = list("annual income" = "annual_inc",
                             "age" = "age"), selected = 1)),
    mainPanel(
  textOutput('age'),
  textOutput('annual_income'),
  plotOutput('distribution1')),

  )
)
server <- function(input, output){
  output$age <- renderText({
    paste('Age : ', input$borrower_age, '\n')
  })
  output$annual_income <- renderText({
    paste('Annual income : ', input$borrower_annual_income)
  })

  # # Regression output
  # output$summary <- renderPrint({
  #   fit <-lm(loan_data_no_outliers[,loan_status] ~ loan_data_no_outliers[,annual_inc])
  #   #names(fit$coefficients) <- c("Intercept", annual_inc)
  #   summary(fit)
  # })
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(loan_data_no_outliers[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  # output$model_result <- renderText({
  #   # newdata <- as.data.frame(age=input$borrower_age, annual_inc = input$borrower_annual_income)
  #   # p <- predict(log_model, newdata = newdata, type = 'response')
  #   # paste('model prob = ', as.character(p))
  # })

}
shinyApp(ui = ui, server = server)