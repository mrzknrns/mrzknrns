library(shiny)
library(ggplot2)
library(scales)

#server
server <- function(input, output) {
  
  data_1990 <- read.csv("www/ResaleFlatPrices1999.csv")
  data_2022 <- read.csv("www/ResaleFlatPrices2022.csv")
  
  data_2022$remaining_lease <- as.numeric(sub(" years.*", "", data_2022$remaining_lease))
  
  selected_data <- reactive({
    if(input$year == "1990") {
      return(data_1990)
    } else {
      return(data_2022)
    }
  })
  
  # render
  output$pricePlot <- renderPlot({
    data_to_plot <- selected_data()
    
    # FINALLY FIGURED HOW TO DO REGRESSION PLS
    lm_model <- lm(resale_price ~ remaining_lease, data = data_to_plot)
    equation <- paste0("y = ", round(coef(lm_model)[1], 2), 
                       " + ", round(coef(lm_model)[2], 2), "x")
    
    ggplot(data_to_plot, aes(x = remaining_lease, y = resale_price)) +
      geom_point(alpha = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Resale Prices vs. Remaining Lease in", input$year),
           x = "Remaining Lease (Years)",
           y = "Resale Price") +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "")) +
      annotate("text", x = max(data_to_plot$remaining_lease, na.rm = TRUE), 
               y = min(data_to_plot$resale_price, na.rm = TRUE), 
               label = equation, hjust = 2.8, vjust = -40) +
      theme_minimal()
  })
}

# ui things
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = c("1990", "2022"))
    ),
    mainPanel(
      plotOutput("pricePlot")
    )
  )
)

# run app
shinyApp(ui = ui, server = server)
