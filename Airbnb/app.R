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
      withSpinner(plotOutput(outputId = "map")),
      plotOutput(outputId = "avgprice_graph"),
      plotOutput(outputId= "avgratings_graph")
      # leafletOutput(("map2"))
    )
  )
)


# Define server 
server <- function(input, output, session) {
  
  # filters for all attributes selected
  selected_attributes <- reactive({
    req(input$city)
    df 
    if (input$city == "New York City") {
      df <- listings_nyc
    } else {
      df <- listings_la
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
  
  selected_neighborhoods <- reactive({
    req(input$city)
    req(input$neighborhood)
    if (input$city == "New York City") {
      filter(listings_nyc, neighborhood %in% input$neighborhood) 
    } else {
      filter(listings_la, neighborhood %in% input$neighborhood) 
    }
  })
  
  selected_neighborhood_avg_ratings <- reactive({
    req(input$city)
    req(input$neighborhood)
    avg_ratings <- selected_neighborhoods() %>% 
      group_by(neighborhood) %>%
      summarize(avg_rating = round(mean(review_scores_rating/10, na.rm = TRUE), 2), 
                avg_cleanliness = round(mean(review_scores_cleanliness, na.rm = TRUE), 2),
                avg_communication = round(mean(review_scores_communication, na.rm = TRUE), 2)) 
    avg_ratings
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
  
  # histogram of avg price graph 
  output$avgprice_graph <- renderPlot ({
    ggplot(selected_attributes(), aes(x = price, fill = room_type)) +
      geom_histogram(binwidth = 50) +
      labs(x = "average price", y = "count", title = "Airbnb Average Prices") +
      theme(legend.title = element_blank())
  })
  
  # parallel coordinate plot of avg ratings
  output$avgratings_graph <- renderPlot ({
    ggparcoord(selected_neighborhood_avg_ratings(), 
               columns = 2:4, groupColumn = 1, scale = "globalminmax", title = "Average Ratings by Neighborhood")
  })
  
  
  # map 
  # Other type of map
  # Create our colors with a categorical color function
  bins <- c(0,20,40,60, 80, 100)
  
  output$map2 <- renderLeaflet({
    if (input$city == "New York City") {
      pal <- colorBin(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"), domain = listings_nyc$review_scores_rating, bins = bins)
      map2 <- leaflet(listings_nyc) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius=~price,
          stroke=FALSE,
          fillColor = ~pal(review_scores_rating),
          color = "white",
          fillOpacity=0.5,
          labelOptions = labelOptions(noHide = FALSE),
          popup = paste0( '<p><strong>', listings_nyc$name, '</strong> <p/>', 
                          "<strong> Room Type: </strong>", 
                          listings_nyc$room_type, '<br/>',
                          "<strong> Price Per Night </strong>", 
                          listings_nyc$price)) %>%
        
        addLegend(
          "bottomright", pal = pal, values = ~review_scores_rating,
          title = "Listings",
          opacity = 0.7,
          labels = labels
        ) 
      
    } else {
      pal <- colorBin(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"), domain = listings_la$review_scores_rating, bins = bins)
      map2 <- leaflet(listings_la) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius=~price,
          stroke=FALSE,
          fillColor = ~pal(review_scores_rating),
          color = "white",
          fillOpacity=0.5,
          popup = paste0( '<p><strong>', listings_la$name, '</strong> <p/>', 
                          "<strong> Room Type: </strong>", 
                          listings_la$room_type, '<br/>',
                          "<strong> Price Per Night </strong>", 
                          listings_la$price)) %>%
        
        addLegend(
          "bottomright", pal = pal, values = ~review_scores_rating,
          title = "Listings",
          opacity = 0.7,
          labels = labels
        ) }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
