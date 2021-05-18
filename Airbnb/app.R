library(shiny)
library(tidyverse)
library(maps)
library(shinycssloaders)
library(leaflet)
library(GGally)
library(shinydashboard)
library(tm)
library(tidytext)
library(stopwords)
library(wordcloud)
library(SnowballC)


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
  titlePanel("Airbnb Data Dashboard"),
  
  # Filtering Panel 
  sidebarLayout(
    sidebarPanel(
      
      # selecting city and neighborhoods 
      titlePanel("Search Filters"),
      radioButtons(inputId = "city",
                   label = "Select City",
                   choices = c("Los Angeles", "New York City"),
                   selected = "Los Angeles"
      ),
      
      selectInput(inputId = "neighborhood",
                  label = "Select Neighborhood",
                  choices = neighborhoods_la,
                  selected = NULL, 
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
      wellPanel(
        withSpinner(leafletOutput(("map2"))),
        style = "margin: 10px;"
      ),
      br(),
      wellPanel(
        h3("Average Prices by Accomodation Type"),
        plotOutput(outputId = "avgprice_graph"),
        style = "margin: 10px;"
      ),
      br(),
      conditionalPanel(
        condition = "input.neighborhood != ''",
        wellPanel(
          h3("Average Ratings by Neighborhood"),
          plotOutput(outputId = "avgratings_graph"),
          style = "margin: 10px;"
        )
      ),
      br(),
      wellPanel(
        h3("Amenities Wordcloud"),
        plotOutput("wordcloud")
      ),
      br(),
      plotOutput(outputId= "hostStats_graph"),
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
    if (!is.null(input$neighborhood)) {
      df <- df %>% 
        filter(neighborhood %in% input$neighborhood)
    }
    df %>% 
      filter(price <= input$price_range[2] & price >= input$price_range[1],
             number_of_reviews >= input$num_reviews[1],
             accommodates >= input$num_guests[1],
             room_type %in% input$room_types) %>%
      mutate(review_scores_rating = review_scores_rating / 10 ) #to be in the same scale as cleanliness/communication ratings 
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
    pal2 <- colorFactor("RdYlBu", selected_attributes()$room_type)
    leafletProxy("map2", data = selected_attributes()) %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        stroke=FALSE,
        fillColor = ~pal2(room_type),
        color = "white",
        fillOpacity=0.7,
        labelOptions = labelOptions(noHide = FALSE),
        popup = paste0( '<p><strong>', selected_attributes()$name, '</strong> <p/>',
                        "<strong> Rating: </strong>",
                        selected_attributes()$review_scores_rating, '<br/>',
                        "<strong> Price Per Night: </strong>",
                        selected_attributes()$price))
  })
  # dynamic legends based on selected filters 
  observe({
    pal2 <- colorFactor("RdYlBu", selected_attributes()$room_type)
    leafletProxy("map2", data = selected_attributes()) %>%
      clearShapes() %>%
      addLegend(
        "bottomleft", pal = pal2, values = ~room_type,
        title = "Listings",
        opacity = 0.7,
        labels = labels
      )
  }) # end of map 
  
  
  # histogram of avg price graph:
  output$avgprice_graph <- renderPlot ({
    ggplot(selected_attributes(), aes(x = price, fill = room_type)) +
      geom_histogram(binwidth = 50) +
      labs(x = "average price", y = "count") +
      theme(legend.title = element_blank())
  }) # end of histogram 
  
  # avg ratings:
  selected_neighborhood_avg_ratings <- reactive({
    req(input$city)
    req(input$neighborhood)
    avg_ratings <- selected_attributes() %>% 
      group_by(neighborhood) %>%
      summarize(avg_rating = round(mean(review_scores_rating, na.rm = TRUE), 2), 
                avg_cleanliness = round(mean(review_scores_cleanliness, na.rm = TRUE), 2),
                avg_communication = round(mean(review_scores_communication, na.rm = TRUE), 2)) 
    avg_ratings
  }) 
  # parallel coordinate plot of avg ratings
  output$avgratings_graph <- renderPlot ({
    ggparcoord(selected_neighborhood_avg_ratings(), 
               columns = 2:4, groupColumn = 1, scale = "globalminmax")
  }) # end of avg ratings 
  
  # word cloud:
  selected_amenities <- reactive({
    req(input$city)
    
    split_amenities <- selected_attributes() %>% 
      transform(amenities = strsplit(amenities, ",")) %>%
      unnest(amenities)
    
    keywords <- removeWords(split_amenities$amenities, stopwords("en", source="smart")) 
    keywords <- removeNumbers(keywords)
    keywords <- removePunctuation(keywords)
    keywords <- gsub( " ", "", keywords) 
    
    keyword_counts <- read.table(text=keywords, col.names=c('amenities')) %>%
      group_by(amenities) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    keyword_counts
  })
  
  output$wordcloud <- renderPlot({
    wordcloud(words = selected_amenities()$amenities, freq = selected_amenities()$count, scale=c(1.5, 0.5), 
              max.words=100, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  # % of hosts w/ verified identities, % who are 'superhosts', avg % acceptance of guests
  selected_neighborhood_host_stats <- reactive({
	    req(input$city)
	    req(input$neighborhood)
	    req(listings_la)
	    req(listings_nyc)
    
    
    
       host_stats <- selected_attributes() %>% 
       group_by(neighborhood) %>%
       summarize(
      			
                percent_hosts_verified = sum(host_identity_verified == "TRUE", na.rm = TRUE) / 
                					   sum(host_identity_verified != "", na.rm = TRUE),
                percent_superhost = sum(host_is_superhost == "TRUE", na.rm = TRUE) / 
                					   sum(host_is_superhost != "", na.rm = TRUE),
                avg_percent_acceptance = mean(as.numeric(sub("%", "", host_acceptance_rate))/100, na.rm = TRUE)
                )
             
    host_stats
  })
  
  # host stats plot
  output$hostStats_graph <- renderPlot ({
    ggparcoord(selected_neighborhood_host_stats(), 
               columns = 2:4, groupColumn = 1, scale = "globalminmax", title = "Host Stats by Neighborhood")
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
