library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

data_1999 <- read.csv("www/ResaleFlatPrices1999.csv")
data_2022 <- read.csv("www/ResaleFlatPrices2022.csv")

#combine here
data_1999$year <- 1990
data_2022$year <- 2022
combined_data <- rbind(data_1999, data_2022)

#define
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("flatType", "Select Flat Type:", 
                  choices = unique(combined_data$flat_type))
    ),
    mainPanel(
      plotOutput("pricePlot")
    )
  )
)

#server
server <- function(input, output) {
  output$pricePlot <- renderPlot({
    filtered_data <- combined_data %>% 
      filter(flat_type == input$flatType) %>%
      group_by(year) %>%
      summarize(average_price = mean(resale_price, na.rm = TRUE))
    
    ggplot(filtered_data, aes(x = as.factor(year), y = average_price, fill = as.factor(year))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::dollar(average_price)), 
                vjust = -0.5, color = "black") +
      scale_fill_manual(values = c("1990" = "green", "2022" = "orange"), name = "Year") +
      scale_y_continuous(labels = scales::label_dollar()) + 
      labs(title = paste("Average Prices for", input$flatType),
           x = "Year",
           y = "Average Price ($)",
           fill = "Year") +  
      theme_minimal()
  })
}

#run
shinyApp(ui = ui, server = server)
