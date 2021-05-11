library(shinycssloaders)
library(leaflet)

# import data
listings_la <- read_csv("/Users/vibhagogu/Airbnb/Airbnb/UpdatedLAlistings.csv")
listings_nyc <- read_csv("/Users/vibhagogu/Airbnb/Airbnb/UpdatedNYClistings.csv") 
listings_la$price <- as.numeric(gsub("[$,]", "", listings_la$price))
listings_nyc$price <- as.numeric(gsub("[$,]", "", listings_nyc$price))

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
      uiOutput("slider_reviews"),
      
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
      withSpinner(plotOutput(outputId = "map")),
      plotOutput(outputId = "avgprice_graph"), 
      plotOutput(outputId = "map2"),
    )
  )
)

# Define server 
server <- function(input, output, session) {
  
  # city selected
  selected_city <- reactive({
    req(input$city)
    if (input$city == "New York City") {
      listings_nyc
    } else {
      listings_la
    }
  })
  
  # neighborhoods to dynamically changed based on city selected
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
  
  output$slider_price <- renderUI({
    sliderInput("avgprice", "Average Price",
                min = 0, max = 500,
                value = c(50,150))
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
  
  selected_city_prices <- reactive({
    prices <- 2 * sd(selected_city()$price)
    listings_prices <- selected_city() %>% 
      filter(price <= prices)
    listings_prices
  })
  
  # map 
  #Other type of map
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
        ) }
      if (input$city == "Los Angeles") {
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
  
  output$avgprice_graph <- renderPlot ({
    ggplot(selected_city_prices(), aes(x = price, fill = room_type)) +
      geom_histogram(binwidth = 50) +
      labs(x = "average price", y = "count", title = "Airbnb Average Prices") +
      theme(legend.title = element_blank())
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
