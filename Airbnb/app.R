library(shiny)
library(tidyverse)
library(maps)
library(shinycssloaders)

# import data
listings_la <- read_csv("~/Airbnb/UpdatedLAlistings.csv")
listings_nyc <- read_csv("~/Airbnb/UpdatedNYClistings.csv")

# map variables
states <- map_data("state")
counties <- map_data("county")
ca <- subset(states, region == "california")
ca_county <- subset(counties, region == "california")
nyc <- subset(states, region == "new york")
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
      
      ),
      

      mainPanel(
        withSpinner(plotOutput(outputId = "map"))
        
      )
   )
)

# Define server 
server <- function(input, output, session) {
  
  # neighborhoods to dynamically changed based on city selected
  selected_neighborhood <- reactive({
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
                      choices = selected_neighborhood())
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
  
  # map 
  output$map <- renderPlot ({
    if (input$city == "New York City") {
    ggplot(data = nyc, aes(x = long, y = lat, group = group)) +
      geom_polygon(color = "white", fill = "gray") +
      coord_quickmap() +
      theme_void() +
      geom_polygon(data = nyc_county, fill = NA, color = "white") +
      geom_polygon(color = "white", fill = NA)
      
    } else {
      ggplot(data = ca, aes(x = long, y = lat, group = group)) +
        geom_polygon(color = "white", fill = "gray") +
        coord_quickmap() +
        theme_void() +
        geom_polygon(data = ca_county, fill = NA, color = "white") +
        geom_polygon(color = "white", fill = NA)
    }
    
  })

}


# Run the application 
shinyApp(ui = ui, server = server)

