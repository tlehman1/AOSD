library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Standortabfrage mit R Shiny"),
  sidebarLayout(
    sidebarPanel(
      actionButton("get_location", "Standort abfragen")
    ),
    mainPanel(
      verbatimTextOutput("location_output")
    )
  ),
  tags$script(HTML("
    // JavaScript zum Abrufen des Standorts
    document.getElementById('get_location').addEventListener('click', function() {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(function(position) {
          var lat = position.coords.latitude;
          var lon = position.coords.longitude;
          Shiny.onInputChange('user_location', {lat: lat, lon: lon});
        }, function(error) {
          Shiny.onInputChange('user_location', {error: error.message});
        });
      } else {
        Shiny.onInputChange('user_location', {error: 'Geolocation ist nicht unterstützt.'});
      }
    });
  "))
)

# Server
server <- function(input, output, session) {
  output$location_output <- renderPrint({
    req(input$user_location)
    if (!is.null(input$user_location$error)) {
      return(paste("Fehler:", input$user_location$error))
    }
    paste("Breitengrad:", input$user_location$lat, 
          "Längengrad:", input$user_location$lon)
  })
}

# App
shinyApp(ui = ui, server = server)
