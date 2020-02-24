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
  plotOutput('distribution2'),
  verbatimTextOutput("summary")),
  
  )
)
server <- function(input, output){
  output$age <- renderText({
    paste('Age : ', input$borrower_age, '\n')
  })
  output$annual_income <- renderText({
    paste('Annual income : ', input$borrower_annual_income)
  })

  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(loan_data_no_outliers[,input$indepvar], 
         main="Histogram of independent variable", xlab=input$indepvar)
  }, height=400, width=400)

  #Plot of other independent variables
  output$distribution2 <- renderPlot({
    plot(loan_data_no_outliers[,input$indepvar2], 
         main="Plot of independent variable", xlab=input$indepvar2)
  }, height=400, width=400)

  output$summary <- renderPrint({
    fit <- glm(loan_status ~ age + annual_inc, family = 'binomial', data = loan_data_no_outliers)
    summary(fit)
  })
  
  output$prediction <- renderText({
    # newdata <- as.data.frame(age=input$borrower_age, annual_inc = input$borrower_annual_income)
    # p <- predict(log_model, newdata = newdata, type = 'response')
    # paste('model prob = ', as.character(p))
  })

}
shinyApp(ui = ui, server = server)

# ### Test in regular R command
# newdata <- data.frame('age'=33, 'annual_inc' = 100000)
# p <- predict(log_model, newdata = newdata, type = 'response')
# paste('model prob = ', as.character(p))
# # Make a binary predictions-vector using a cut-off of 15%
# p_cutoff_15 <- ifelse(p > 0.15, 1, 0)
# p_cutoff_15 <- ifelse(p > 0.15, 'Default', 'Non default')
# approval <- ifelse(p > 0.15, 'Not approved', 'Approved')
# print(p_cutoff_15)
# print(approval)
# 
# log_model<- glm(loan_status ~ age + annual_inc + loan_amnt + grade + home_ownership + emp_cat, 
#                 family='binomial', data = loan_data_no_outliers)
# newdata <- data.frame('age'=33, 
#                       'annual_inc' = 100000,
#                       'loan_amnt' = 200000,
#                       'grade' = 'G',
#                       'home_ownership' = 'RENT',
#                       'emp_cat' = '0-15')
# p <- predict(log_model, newdata = newdata, type = 'response')
# # Make a binary predictions-vector using a cut-off of 15%
# p_cutoff_15 <- ifelse(p > 0.15, 1, 0)
# p_cutoff_15 <- ifelse(p > 0.15, 'Default', 'Non default')
# approval <- ifelse(p > 0.15, 'Not approved', 'Approved')
# print(p_cutoff_15)
# print(approval)

