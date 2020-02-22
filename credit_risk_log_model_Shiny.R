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
  selectInput("indepvar", label = "Explanatory variables histrogram: ", 
              choices = list("annual income" = "annual_inc",
                             "loan amount" = "loan_amnt",
                             "age" = "age"), selected = 1),
  selectInput("indepvar2", label = "Explanatory variables plot: ", 
              choices = list("grade" = "grade",
                             "home ownership" = "home_ownership",
                             "interest rate" = "ir_cat",
                             "employment length" = "emp_cat"), selected = 1)),  
    mainPanel(
  textOutput('age'),
  textOutput('annual_income'),
  plotOutput('distribution1'),
  plotOutput('distribution2')),
  
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
    hist(loan_data_no_outliers[,input$indepvar], 
         main="Histogram of independent variable", xlab=input$indepvar)
  }, height=300, width=300)

  #Plot of other independent variables
  output$distribution2 <- renderPlot({
    plot(loan_data_no_outliers[,input$indepvar2], 
         main="Plot of independent variable", xlab=input$indepvar2)
  }, height=300, width=300)
    
  # output$model_result <- renderText({
  #   # newdata <- as.data.frame(age=input$borrower_age, annual_inc = input$borrower_annual_income)
  #   # p <- predict(log_model, newdata = newdata, type = 'response')
  #   # paste('model prob = ', as.character(p))
  # })

}
shinyApp(ui = ui, server = server)