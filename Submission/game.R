library(shiny)
library(dplyr)

# Assuming the flat type and town are in columns named 'flat_type' and 'town', 
# and the price is in a column named 'resale_price'.

# Read the dataset
data_2022 <- read.csv("/mnt/data/ResaleFlatPrices2022.csv")

# Pre-calculate the average prices for each flat type and town
average_prices <- data_2022 %>%
  group_by(flat_type, town) %>%
  summarize(average_price = mean(resale_price, na.rm = TRUE))

# Define UI
ui <- fluidPage(
  titlePanel("HDB Resale Price Estimator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("income", "Your Annual Income:", value = 50000),
      numericInput("spouse_income", "Spouse's Annual Income:", value = 50000),
      selectInput("flat_type", "Preferred Flat Type:", choices = unique(data_2022$flat_type)),
      selectInput("town", "Preferred Town:", choices = unique(data_2022$town)),
      actionButton("estimate", "Estimate Price")
    ),
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$estimate, {
    # Filter the pre-calculated average prices based on user selection
    selected_price <- average_prices %>%
      filter(flat_type == input$flat_type, town == input$town) %>%
      .$average_price
    
    # Display the result
    output$result <- renderText({
      if (length(selected_price) > 0) {
        paste("The average price for a", input$flat_type, "in", input$town, "is $", round(selected_price, 2))
      } else {
        paste("No data available for a", input$flat_type, "in", input$town)
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
