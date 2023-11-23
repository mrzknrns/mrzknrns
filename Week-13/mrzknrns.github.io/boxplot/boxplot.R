library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

#combining two files to for comparison
combine_data <- function(data1990_path, data2022_path) {
  data1990 <- read.csv(data1990_path)
  data2022 <- read.csv(data2022_path)
  
  #change format from yyyy-mm to yyyy
  data1990$Year <- as.numeric(substr(data1990$month, 1, 4))
  data2022$Year <- as.numeric(substr(data2022$month, 1, 4))
  combinedData <- rbind(data1990, data2022)
  
  return(combinedData)
}

# Define UI
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("flatType", "Select Flat Type:", 
                  choices = c("3 ROOM", "4 ROOM", "5 ROOM", "EXECUTIVE"), 
                  selected = "3 ROOM",
                  multiple = TRUE), #this defaults to 3 room
      checkboxGroupInput("year", "Select Year(s):",
                         choices = c('1990', '2022'),
                         selected = c('1990', '2022'))
    ),
    mainPanel(
      plotOutput("boxPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  combinedData <- combine_data("www/ResaleFlatPrices1999.csv", "www/ResaleFlatPrices2022.csv")
  
  updateSelectInput(session, "flatType", 
                    choices = unique(combinedData$flat_type),
                    selected = unique(combinedData$flat_type)[1])
  
  output$boxPlot <- renderPlot({
    filteredData <- filter(combinedData, 
                           flat_type %in% input$flatType,
                           Year %in% as.numeric(input$year))
    
#boxplot creation
    ggplot(filteredData, aes(x = flat_type, y = resale_price, fill = as.factor(Year))) +
      geom_boxplot() +
      scale_y_continuous(labels = label_dollar()) + 
      theme_minimal() +
      labs(title = "Box Plot of Resale Prices", 
           y = "Resale Price", x = "Flat Type") +
      scale_fill_brewer(palette = "Set2", name = "Year") 
  })
}

shinyApp(ui, server)

