library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)


data_A <- data.frame(
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
summary(data_A)

# Fit the multiple linear regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data_A)

# Display the summary of the regression model
summary(model)
r_squared <- summary(model)$r.squared * 100  

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "E-Commerce Sales"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Model", tabName = "menu_1", icon = icon("cogs")),
      menuItem(text = "Prediction", tabName = "menu_2", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab for Model Regresi
      tabItem(
        tabName = "menu_1",
        fluidPage(
          h2(tags$b("Multiple Linear Regression Model")),
          fluidRow(
            column(
              width = 6,
              h4("Model Coefficients"),
              tableOutput("model_coefficients")
            ),
            column(
              width = 6,
              h4("Model Accuracy"),
              uiOutput("model_accuracy"),
              h4("Interpretation"),
              textOutput("model_interpretation")
            )
          ),
          fluidRow(
            column(
              width = 12,
              h4("Regression Result"),
              tableOutput("regression_result"),
              h4("Regression Result Variabel"),
              textOutput("var_interpretation")
            )
          )
        )
      ),
      
      # Tab for Prediction
      tabItem(
        tabName = "menu_2",
        fluidRow(
          column(
            width = 6,
            h4("Input Predictor Values"),
            numericInput("x1", "Number of Website Visitors:", value = 140000),
            numericInput("x2", "Number of Monthly Transactions:", value = 15000),
            numericInput("x3", "Average Number of Items per Transaction:", value = 3),
            sliderInput("x4", "Customer Satisfaction Rating:", min = 1, max = 9, value = 8.3),
            numericInput("x5", "Number of Online Advertisements:", value = 32000),
            actionButton("predictButton", "Predict Sales", style = "background-color: #FF69B4; color: white; margin-top: 20px;")
          ),
          column(
            width = 6,
            h4("Prediction Result"),
            textOutput("predictedSales")
          )
        )
      )
    )
  )
)

# Define the server for the Shiny app
server <- function(input, output) {
  output$model_coefficients <- renderTable({
    coefficients <- summary(model)$coefficients
    coefficients
  })
  
  output$regression_result <- renderTable({
    predicted <- predict(model, newdata = data_A)
    actual_predicted <- cbind(data_A, Predicted = predicted)
    actual_predicted
  })
  
  output$var_interpretation <- renderText({c(
      " x1 = Number of Website Visitors represents the total monthly visitors to the e-commerce website.",
      " 
      x2 = Number of Monthly Transactions represents the total number of transactions in a month.",
      "
      x3 = Average Number of Items per Transaction represents the mean items purchased in a single transaction.",
      " 
      x4 = Customer Satisfaction Rating represents the rating given by customers.",
      " 
      x5 = Number of Online Advertisements represents the total online advertisements.",
      " 
      y = Sales represents the total monthly sales of the e-commerce business." )
  })
  
  output$model_accuracy <- renderUI({
    r_squared <- summary(model)$r.squared
    accuracy_text <- paste("Model Accuracy: ", round(r_squared * 100, 2), "%")
    HTML(accuracy_text)
  })
  
  output$model_interpretation <- renderText({
    "An increase of one unit in 'Number of Monthly Transactions' is estimated to lead to an increase of approximately 11.12 units in sales, and an increase of one unit in 'Number of Online Advertisements' is estimated to lead to an increase of about 0.0052 units in sales. Other variables do not have a significant impact on sales."
  })
  
  observeEvent(input$predictButton, {
    user_input <- data.frame(
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5
    )
    
    predicted_sales <- predict(model, newdata = user_input)
    
    output$predictedSales <- renderText(paste("Predicted Sales: $", round(predicted_sales, 3)))
  })
}

# Run the Shiny app
shinyApp(ui, server)
