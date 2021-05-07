library(shiny)
library(tidyverse)

# import data
listings_la <- read_csv("~/Airbnb/listings-la.csv")
listings_nyc <- read_csv("~/Airbnb/listings-nyc.csv")

# neighborhoods
neighborhoods_la <- listings_la %>% distinct(neighbourhood) %>% arrange(neighbourhood)
neighborhoods_nyc <- listings_nyc %>% distinct(neighbourhood) %>% arrange(neighbourhood)
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
        
      )
   )
)

# Define server 
server <- function(input, output, session) {
  
  # neighborhoods to dynamically changed based on city selected
  selected_neighborhood <- reactive({
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

}


# Run the application 
shinyApp(ui = ui, server = server)

