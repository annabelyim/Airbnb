library(shiny)
library(tidyverse)
library(maps)
library(shinycssloaders)

# import data
listings_la <- read_csv("~/Airbnb/UpdatedLAlistings.csv")
listings_nyc <- read_csv("~/Airbnb/UpdatedNYClistings.csv")

# map variables
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
nyc_county <- subset(counties, region == "new york")

# neighborhood variables 
neighborhoods_la <- listings_la %>% distinct(neighborhood) %>% arrange(neighborhood)
neighborhoods_nyc <- listings_nyc %>% distinct(neighborhood) %>% arrange(neighborhood)
neighborhoods_all <- neighborhoods_la %>% 
  rbind(neighborhoods_nyc)


# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Airbnb"),
   
   sidebarLayout(
      sidebarPanel(
        
        titlePanel("Airbnb Filters"),
        radioButtons(inputId = "city",
                            label = "Select City",
                            choices = c("Los Angeles", "New York City"),
                            selected = "Los Angeles"
                     ),
        
        selectInput(inputId = "neighborhood",
                    label = "Select Neighborhood",
                    choices = neighborhoods_la,
                    selected = "", 
                    multiple = TRUE
        ),
        uiOutput("slider_price"),
        uiOutput("slider_rating"),
        uiOutput("slider_reviews")
      
        # check boxes
      	checkboxInput(inputId = "entire house",
                    label = "Entire house",
                    value = FALSE, 
                    width = '100%'
        ),
        
        checkboxInput(inputId = "washing machine",
                    label = "Has a washer & dryer",
                    value = FALSE, 
                    width = '100%'
        ),
        
        checkboxInput(inputId = "cooking utilities",
                    label = "Home cooking utilities",
                    value = FALSE, 
                    width = '100%'
        ),
        
        checkboxInput(inputId = "parking",
                    label = "Free parking",
                    value = FALSE, 
                    width = '100%'
        ),
      ),
      

      mainPanel(
        withSpinner(plotOutput(outputId = "map"))
        
      )
   )
)

# Define server 
server <- function(input, output, session) {
  
  # neighborhoods to dynamically changed based on city selected
  selected_city <- reactive({
    req(input$city)
    if (input$city == "New York City") {
      neighborhoods_nyc
    } else {
      neighborhoods_la
    }
  })
  
  observeEvent(input$city, {
    updateSelectInput(session, 
                      inputId = "neighborhood",
                      label = "Select Neighborhood",
                      choices = selected_city())
  })
  
  output$slider_price <- renderUI({
    sliderInput("avgprice", "Average Price",
                min = 0, max = 500,
                value = c(50,150))
  })
  
  output$slider_rating <- renderUI({
    sliderInput("rating", "Average Rating",
                min = 0, max = 10,
                value = 8)
  })
  
  output$slider_reviews <- renderUI({
    sliderInput("reviews", "Number of Reviews",
                min = 0, max = 200,
                value = 10)
  })
  
  selected_neighborhoods <- reactive({
    req(input$city)
    req(input$neighborhood)
    if (input$city == "New York City") {
      filter(listings_nyc, neighborhood %in% input$neighborhood) 
    } else {
      filter(listings_la, neighborhood %in% input$neighborhood) 
    }
    
  })
  # map 
  output$map <- renderPlot ({
    if (input$city == "New York City") {
    ggplot() +
      geom_polygon(data = nyc_county, aes(x = long, y = lat, group = group), color = "white", fill = "gray") +
      geom_point(data = selected_neighborhoods(), aes(x = longitude, y = latitude, alpha = 0.05)) +
      coord_quickmap(xlim = c(-75, -73), ylim = c(40, 41)) +
      theme_void() +
      guides(alpha = FALSE) 
      
      
      
    } else {
      ggplot() +
        geom_polygon(data = ca_county, aes(x = long, y = lat, group = group), color = "white", fill = "gray") +
        geom_point(data = selected_neighborhoods(), aes(x = longitude, y = latitude, alpha = 0.5)) +
        coord_quickmap(xlim = c(-119, -117), ylim = c(33, 35)) +
        theme_void() +
        guides(alpha = FALSE) 
    }
    
  })

}


# Run the application 
shinyApp(ui = ui, server = server)

