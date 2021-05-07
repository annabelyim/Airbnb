library(shiny)
library(tidyverse)

# import data
listings_la <- read_csv("~/Airbnb/listings-la.csv")
listings_nyc <- read_csv("~/Airbnb/listings-nyc.csv")

# neighbourhoods
neighbourhoods_la <- listings_la %>% distinct(neighbourhood) %>% arrange(neighbourhood)
neighbourhoods_nyc <- listings_nyc %>% distinct(neighbourhood) %>% arrange(neighbourhood)

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
        
        selectInput(inputId = "neighbourhood",
                    label = "Select Neighbourhood",
                    choices = neighbourhoods_la,
                    selected = "", 
                    multiple = TRUE
        )
      
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
      neighbourhoods_nyc
    } else {
      neighbourhoods_la
    }
  })
  
  observeEvent(input$city, {
    updateSelectInput(session, 
                      inputId = "neighbourhood",
                      label = "Select Neighbourhood",
                      choices = selected_neighborhood())
  })

  }


# Run the application 
shinyApp(ui = ui, server = server)

