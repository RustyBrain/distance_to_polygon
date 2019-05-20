
library(shiny)
library(rgdal)
library(geojsonio)
library(geosphere)
library(dplyr)
library(leaflet)
library(jsonlite)
library(readr)
library(ggmap)
library(purrr)

school <- geojson_read('school_poly.geojson',  what = 'sp')
register_google(key = KEY)
get_coords_from_api <- function(addresses) {
  addresses <- addresses %>%
    cbind(as.data.frame(geocode(addresses$address)))
  
  distances <- data.frame()
  for (row in 1:nrow(addresses)) {
    print(row)
    point <- c(addresses[row, 'lon'], addresses[row, 'lat'])
    dist <- dist2Line(p=point, line = school)
    distances <- rbind(distances, as.data.frame(dist))
  }
  
  addresses <- cbind(addresses, distances)
  colnames(addresses)[2] <- 'lng'
  colnames(addresses)[3] <- 'lt'
  write.csv(addresses, 'most_recent_addresses.csv')
  return(addresses)
}

get_most_recent_addresses <- function() {
  if (file.exists('most_recent_addresses.csv')){
    return(read_csv('most_recent_addresses.csv'))
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Distance Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("addresses", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        downloadButton('downloadData', 'Download')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("distMap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distMap <- renderLeaflet({
    inFile <- input$addresses
    if (is.null(inFile)) {
      return(NULL)
    }
    addresses <- read_csv(inFile$datapath)
    addresses <- get_coords_from_api(addresses)
    map1 <- leaflet(school) %>%
       addTiles(group = "OSM (default)") %>%
       addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                   opacity = 1.0) %>%
       addMarkers(data = addresses, lng = ~lng, lat = ~lt)

     for (i in 1:nrow(addresses)) {
       map1 <- addPolylines(map1,
                            lat = c(addresses[i, 'lt'], addresses[i, 'lat']),
                            lng = c(addresses[i, 'lng'], addresses[i, 'lon']),
                            label = toString(addresses[i, 'distance']))
     }
     map1
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("addresses_with_distance", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- read_csv('most_recent_addresses.csv')
      df <- df %>%
        select(address, distance, lng, lt)
      write.csv(df, file, row.names = FALSE)
    },
    unlink('most_recent_addresses.csv')
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


