library(shiny)
library(dplyr)

data_2022 <- read.csv("www/ResaleFlatPrices2022.csv")

average_prices <- data_2022 %>%
  group_by(flat_type, town) %>%
  summarize(average_price = mean(resale_price, na.rm = TRUE))

# Define UI
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      numericInput("income", "Your Total Annual Income in SGD$:", value = 50000),
      numericInput("spouse_income", "Spouse's Total Annual Income in SGD$:", value = 50000),
      selectInput("flat_type", "Preferred Flat Type:", choices = unique(data_2022$flat_type)),
      selectInput("town", "Preferred Town:", choices = unique(data_2022$town)),
      numericInput("interest_rate", "Annual Interest Rate (%):", value = 2.6),
      numericInput("loan_tenure", "Loan Tenure (Years):", value = 25, min = 1, max = 30),
      actionButton("calculate", "Calculate"),
      tags$hr(),  # Horizontal line to separate the inputs from the output
      div(style = "text-align: left;", textOutput("downpayment")), br(),
      div(style = "text-align: left;", textOutput("monthly_repayment")), br(),
      div(style = "text-align: left;", textOutput("advice"))
    ),
    mainPanel()
  )
)

# Define server logic
server <- function(input, output) {
  
  # eventReactive for calculate button - this basically consistantly watches the input from the user. 
  result <- eventReactive(input$calculate, {
    selected_price <- average_prices %>%
      filter(flat_type == input$flat_type, town == input$town) %>%
      .$average_price
    
    inflation_rate <- 0.0246  # 2.46% inflation rate
    future_price <- selected_price * ((1 + inflation_rate) ^ input$loan_tenure)
    
    downpayment <- future_price * 0.2
    loan_principal <- future_price * 0.8
    
    r <- input$interest_rate / 100 / 12
    n <- input$loan_tenure * 12
    monthly_repayment <- loan_principal * (r * (1 + r)^n) / ((1 + r)^n - 1)
    
    list(downpayment = downpayment, monthly_repayment = monthly_repayment)
  })
  
  # calculate button
  observeEvent(input$calculate, {
    calc_result <- result()
    
    output$downpayment <- renderText({
      paste("Downpayment: Minimum downpayment (20%) for a", input$flat_type, "in", input$town, 
            "with an expected inflation rate is $", round(calc_result$downpayment, 2))
    })
    
    output$monthly_repayment <- renderText({
      paste("Monthly Repayment: Estimated monthly repayment over", input$loan_tenure, 
            "years with an expected inflation rate of 2.46% is $", round(calc_result$monthly_repayment, 2))
    })
    
    # Calculate advice
    total_income <- input$income + input$spouse_income
    monthly_income_limit <- total_income / 12 * 0.3  # 30% of monthly income
    
    advice <- ifelse(calc_result$monthly_repayment > monthly_income_limit,
                     "Advice: Please consider a lower priced resale flat! You should spend less than 30% of your total annual income for your monthly payment!",
                     "Advice: You are good to go! Remember you should only allocate 30% of your total annual income for your housing needs!")
    
    output$advice <- renderText({
      advice
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

