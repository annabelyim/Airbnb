library(shiny)
library(tidyverse)
``
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
                    choices = "",
                    selected = ""
        )
      
      ),
      

      mainPanel(
        
      )
   )
)

# Define server 
server <- function(input, output, session) {

  }


# Run the application 
shinyApp(ui = ui, server = server)

