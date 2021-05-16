library(shiny)
library(tidyverse)
library(maps)
library(shinycssloaders)
library(leaflet)
library(GGally)

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

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Airbnb"),
  
  # Filtering Panel 
  sidebarLayout(
    sidebarPanel(
      
      # selecting city and neighborhoods 
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
      
      # sliders
      sliderInput(inputId = "price_range", 
                  label = "Average Price",
                  min = 0, max = 500,
                  value = c(50,150)),
      sliderInput(inputId = "num_reviews", 
                  label = "Number of Reviews",
                  min = 0, max = 200,
                  value = 10),
      sliderInput(inputId = "num_guests", 
                  label = "Number of Guests Allowed",
                  min = 0, max = 16,
                  value = 2),
      
      # check boxes
      checkboxGroupInput(inputId = "room_types",
                         label = "Accomodation Type",
                         choices = c("Private room", "Shared room", "Hotel room", 
                                     "Entire home/apt"),
                         selected = c("Private room", "Shared room", "Hotel room", 
                                      "Entire home/apt"),
                         width = '100%'
      )
      
    ),
    
    
    mainPanel(
      withSpinner(leafletOutput(("map2"))),
      plotOutput(outputId = "avgprice_graph"),
      plotOutput(outputId= "avgratings_graph")
    )
  )
)


# Define server 
server <- function(input, output, session) {
  
  # filters for all attributes selected
  selected_attributes <- reactive({
    req(input$city)
    req(input$neighborhood)
    df 
    if (input$city == "New York City") {
      df <- filter(listings_nyc, neighborhood %in% input$neighborhood)
    } else {
      df <- filter(listings_la, neighborhood %in% input$neighborhood)
    }
    df %>% 
      filter(price <= input$price_range[2] & price >= input$price_range[1],
             number_of_reviews >= input$num_reviews[1],
             accommodates >= input$num_guests[1],
             room_type %in% input$room_types)
  })
  
  # neighborhoods to dynamically changed based on city selected for map 
  selected_city_neighborhoods <- reactive({
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
                      choices = selected_city_neighborhoods())
  })
  
  # map: 
  bins <- c(0,20,40,60, 80, 100)
  
  output$map2 <- renderLeaflet({
    leaflet(selected_attributes()) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) 
  })
  
  # dynamic view based on selected filters
  observe({
    leafletProxy("map2", data = selected_attributes()) %>%
      setView(mean(selected_attributes()$longitude),mean(selected_attributes()$latitude), zoom = 10)
  })
  
  # dynamic circle marks based on selected filters
  observe({
    pal <- colorBin(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"), selected_attributes() %>% select(review_scores_rating), bins = bins)
    
    leafletProxy("map2", data = selected_attributes()) %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        stroke=FALSE,
        fillColor = ~pal(review_scores_rating),
        color = "white",
        fillOpacity=0.7,
        labelOptions = labelOptions(noHide = FALSE),
        popup = paste0( '<p><strong>', selected_attributes()$name, '</strong> <p/>',
                        "<strong> Room Type: </strong>",
                        selected_attributes()$room_type, '<br/>',
                        "<strong> Price Per Night </strong>",
                        selected_attributes()$price))
    
  })
  
  # dynamic legends based on selected filters 
  observe({
    leafletProxy("map2", data = selected_attributes()) %>%
      clearShapes() %>%
      addLegend(
        "bottomright", pal = pal, values = ~review_scores_rating,
        title = "Listings",
        opacity = 0.7,
        labels = labels
      )
    
  }) # end of map 
  

  # histogram of avg price graph:
  output$avgprice_graph <- renderPlot ({
    ggplot(selected_attributes(), aes(x = price, fill = room_type)) +
      geom_histogram(binwidth = 50) +
      labs(x = "average price", y = "count", title = "Airbnb Average Prices") +
      theme(legend.title = element_blank())
  }) # end of histogram 
  
  # avg ratings:
  selected_neighborhood_avg_ratings <- reactive({
    req(input$city)
    req(input$neighborhood)
    avg_ratings <- selected_attributes() %>% 
      group_by(neighborhood) %>%
      summarize(avg_rating = round(mean(review_scores_rating/10, na.rm = TRUE), 2), 
                avg_cleanliness = round(mean(review_scores_cleanliness, na.rm = TRUE), 2),
                avg_communication = round(mean(review_scores_communication, na.rm = TRUE), 2)) 
    avg_ratings
  }) 
  # parallel coordinate plot of avg ratings
  output$avgratings_graph <- renderPlot ({
    ggparcoord(selected_neighborhood_avg_ratings(), 
               columns = 2:4, groupColumn = 1, scale = "globalminmax", title = "Average Ratings by Neighborhood")
  }) # end of avg ratings 
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)
