library(shiny)
library(ggplot2)
library(dplyr)
library(scales)  

# convert
convert_remaining_lease <- function(lease) {
  if (is.na(lease)) {
    return(NA_real_)
  }
  parts <- strsplit(lease, " years| months")[[1]]
  years <- as.numeric(parts[1])
  if (length(parts) > 2) {
    months <- as.numeric(parts[2]) / 12
  } else {
    months <- 0
  }
  return(years + months)
}

# Server logic
server <- function(input, output, session) {
  
  data_1990 <- read.csv("www/ResaleFlatPrices1999.csv")
  data_2022 <- read.csv("www/ResaleFlatPrices2022.csv")
  
  data_2022$remaining_lease <- sapply(data_2022$remaining_lease, convert_remaining_lease)
  
  combined_data <- bind_rows(data_1990, data_2022, .id = "year")
  combined_data$year <- as.factor(combined_data$year)
  
  # Send the unique list of towns to the UI
  observe({
    updateSelectizeInput(session, "towns", choices = unique(combined_data$town), selected = c("BUKIT TIMAH", "PUNGGOL"))
  })
  
  # Plot
  output$violinPlot <- renderPlot({

    filtered_data <- combined_data %>% 
      filter(year == input$year, town %in% input$towns) %>%
      mutate(town = as.factor(town),
             flat_type = as.factor(flat_type))
    
    # Create the violin plot
    ggplot(filtered_data, aes(x = town, y = resale_price, fill = flat_type)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white") +
      facet_wrap(~flat_type, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Distribution of Resale Flat Prices", 
           x = "Estate", 
           y = "Resale Price (SGD)") +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = ""))
  })
}

# UI layout
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = c("1990" = "1", "2022" = "2"), selected = "2"),
      selectizeInput("towns", "Select Estate(s):", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select one or more estate', selected = c("ANG MO KIO", "BEDOK")))
    ),
    mainPanel(
      plotOutput("violinPlot")
    )
  )
)

# Run the Shiny app
shinyApp(ui = ui, server = server)
