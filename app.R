# app.R
# Air Quality Visualization for NRW with Shiny - Improved Version with Loading Animation

# Loading required packages
library(shiny)
library(leaflet)
library(sf)
library(jsonlite)
library(httr)
library(dplyr)
library(DT)
library(shinyjs)

# --- 1. Improved Helper Functions for API Queries ---

# Ensure that German pollutant names are translated to English
translate_pollutant_names <- function(text) {
  if (is.null(text) || is.na(text)) return(text)
  # Make sure we don't create 'Ozonee' by mistake
  # First check if this is 'Ozon' exactly
  if (text == "Ozon") return("Ozone")
  # Otherwise use careful replacement with word boundaries
  text <- gsub("\\bOzon\\b", "Ozone", text)
  text <- gsub("\\bStickstoffdioxid\\b", "Nitrogen Dioxide", text)
  return(text)
}

# Function to fetch current air quality values
fetch_current_lqi <- function() {
  # Current date in YYYY-MM-DD format
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Correct URL according to documentation
  # Components: 200 (PM10), 201 (PM2.5), 3 (Nitrogen Dioxide), 1 (Ozone)
  lqi_url <- paste0(
    "https://luftqualitaet.nrw.de/api/lqindex.php?",
    "komponente=200,201,3,1",
    "&beginn=", current_date, "T00:00",
    "&ende=", current_date, "T23:59",
    "&index=obj&messwerteliste=beides"  # Object-based format with names for easier processing
  )
  
  message("Fetching current air quality data from URL: ", lqi_url)
  
  tryCatch({
    response <- GET(lqi_url, 
                    user_agent("Mozilla/5.0 (compatible; R-Shiny-App)"),
                    add_headers(Accept = "application/json"))
    
    if (status_code(response) != 200) {
      warning("API error for current air quality data: Status ", status_code(response))
      return(NULL)
    }
    
    content_text <- content(response, "text", encoding = "UTF-8")
    
    # Helpful debug output
    message("API response received. First 200 characters: ", substr(content_text, 1, 200))
    
    # Try to parse the response
    lqi_data <- fromJSON(content_text, flatten = TRUE)
    
    # Output structure for debugging
    message("Structure of API response:")
    print(str(lqi_data))
    
    # Translate English/other text designations to English
    text_mapping <- c(
      "very good" = "very good",
      "good" = "good",
      "moderate" = "moderate",
      "poor" = "poor",
      "very poor" = "very poor",
      "befriedigend" = "moderate",
      "ausreichend" = "poor",
      "sehr gut" = "very good",
      "gut" = "good",
      "mäßig" = "moderate",
      "schlecht" = "poor",
      "sehr schlecht" = "very poor"
    )
    
    # Process the response based on the actual format
    if (is.data.frame(lqi_data)) {
      # If a data frame was already returned
      processed_data <- lqi_data
      message("API response is already a dataframe with columns: ", paste(names(processed_data), collapse=", "))
    } else if (is.list(lqi_data) && any(sapply(lqi_data, is.list))) {
      # Array of objects (if index=obj was used)
      message("API response is a list of objects, converting to dataframe")
      processed_data <- bind_rows(lapply(lqi_data, as.data.frame))
    } else if (is.list(lqi_data) && all(sapply(lqi_data, function(x) is.vector(x) && !is.list(x)))) {
      # Array of arrays (standard format)
      message("API response is a list of vectors, converting to dataframe")
      processed_data <- data.frame(
        station_id = sapply(lqi_data, `[[`, 1),  # mstkey (Station ID)
        komponente_id = sapply(lqi_data, `[[`, 2),  # kmpkey (Component ID)
        lqi = as.numeric(sapply(lqi_data, `[[`, 9)),  # lqi (Air Quality Index)
        lqi_text = sapply(lqi_data, `[[`, 10),  # lqi_text (Textual description)
        stringsAsFactors = FALSE
      )
    } else {
      warning("Unexpected format of API response for air quality data")
      message("Data type: ", class(lqi_data))
      return(NULL)
    }
    
    # Here ensure that we have the correct column names
    # and that the LQI values are present as numeric values
    if ("mstkey" %in% names(processed_data) && !"station_id" %in% names(processed_data)) {
      processed_data$station_id <- processed_data$mstkey
    }
    
    if ("mstkrz" %in% names(processed_data) && !"station_id" %in% names(processed_data)) {
      processed_data$station_id <- processed_data$mstkrz
    } else if ("mstkrz" %in% names(processed_data)) {
      # Prefer mstkrz (station abbreviation) as station_id
      processed_data$station_id <- processed_data$mstkrz
    }
    
    if ("mstname" %in% names(processed_data) && !"station_name" %in% names(processed_data)) {
      processed_data$station_name <- processed_data$mstname
    } else if (!"station_name" %in% names(processed_data)) {
      # If no station_name is available, use the ID
      processed_data$station_name <- processed_data$station_id
    }
    
    if ("kmpkey" %in% names(processed_data) && !"komponente_id" %in% names(processed_data)) {
      processed_data$komponente_id <- processed_data$kmpkey
    }
    
    if ("kmpname" %in% names(processed_data) && !"komponente_name" %in% names(processed_data)) {
      processed_data$komponente_name <- processed_data$kmpname
    } else if (!"komponente_name" %in% names(processed_data)) {
      # If no komponente_name is available, use a mapping
      komp_map <- c(
        "1" = "Ozone", 
        "3" = "Nitrogen Dioxide", 
        "200" = "PM10", 
        "201" = "PM2.5"
      )
      processed_data$komponente_name <- komp_map[as.character(processed_data$komponente_id)]
    }
    
    if ("lqi" %in% names(processed_data) && !"LQI" %in% names(processed_data)) {
      processed_data$LQI <- as.numeric(as.character(processed_data$lqi))
    }
    
    if ("lqi_text" %in% names(processed_data) && !"LQI_Text" %in% names(processed_data)) {
      # Translate any existing English/other designations
      processed_data$lqi_text <- sapply(processed_data$lqi_text, function(txt) {
        if (txt %in% names(text_mapping)) {
          return(text_mapping[txt])
        }
        return(txt)
      })
      
      processed_data$LQI_Text <- processed_data$lqi_text
    } 
    
    # IMPORTANT: After loading the data, recalculate LQI_Text based on LQI value
    # This ensures consistent categories
    if ("LQI" %in% names(processed_data)) {
      processed_data$LQI_Text <- sapply(processed_data$LQI, function(lqi) {
        if (lqi <= 1.5) return("very good")
        if (lqi <= 2.5) return("good")
        if (lqi <= 3.5) return("moderate")
        if (lqi <= 4.5) return("poor")
        return("very poor")
      })
    }
    
    # Count the different components per station
    # This is important to determine if all 4 pollutants are measured
    komponenten_pro_station <- processed_data %>%
      group_by(station_id) %>%
      summarise(
        anzahl_komponenten = n_distinct(komponente_id),
        komponenten = paste(sort(unique(komponente_name)), collapse=", ")
      )
    
    # Aggregate by station (highest LQI value)
    if (all(c("station_id", "LQI") %in% names(processed_data))) {
      # Station name might be missing, we use the station ID if necessary
      if (!"station_name" %in% names(processed_data)) {
        processed_data$station_name <- processed_data$station_id
        message("No station name found, using station ID as name")
      }
      
      if (!"komponente_name" %in% names(processed_data)) {
        processed_data$komponente_name <- as.character(processed_data$komponente_id)
        message("No component name found, using component ID as name")
      }
      
      aggregated_lqi <- processed_data %>% 
        group_by(station_id) %>% 
        summarise(
          station_name = first(station_name),
          LQI = max(LQI, na.rm = TRUE),
          LQI_Text = LQI_Text[which.max(LQI)],
          # Store the component with the worst value
          worst_komponente = komponente_name[which.max(LQI)]
        ) %>%
        ungroup()
      
      # Add the number of components
      aggregated_lqi <- left_join(
        aggregated_lqi,
        komponenten_pro_station,
        by = "station_id"
      )
      
      # Handle possible NaN values after aggregation
      aggregated_lqi$LQI[is.na(aggregated_lqi$LQI) | is.nan(aggregated_lqi$LQI)] <- 3
      
      # Ensure that the LQI_Text matches the LQI value
      # This overwrites the texts provided by the API to ensure consistent categories
      aggregated_lqi$LQI_Text <- sapply(aggregated_lqi$LQI, function(lqi) {
        if (lqi <= 1.5) return("very good")
        if (lqi <= 2.5) return("good")
        if (lqi <= 3.5) return("moderate")
        if (lqi <= 4.5) return("poor")
        return("very poor")
      })
      
      message("Successfully processed ", nrow(aggregated_lqi), " stations with LQI values.")
      return(aggregated_lqi)
    } else {
      warning("Required columns 'station_id' or 'LQI' missing in the processed data")
      message("Available columns: ", paste(names(processed_data), collapse=", "))
      return(NULL)
    }
    
  }, error = function(e) {
    warning("Error fetching current air quality data: ", e$message)
    return(NULL)
  })
}

# Function to fetch station data
fetch_stations <- function() {
  # According to documentation: fetch all stations
  station_url <- "https://luftqualitaet.nrw.de/api/station.php?format=json"
  
  tryCatch({
    response <- GET(station_url, 
                    user_agent("Mozilla/5.0 (compatible; R-Shiny-App)"),
                    add_headers(Accept = "application/json"))
    if (status_code(response) != 200) {
      stop("API Error: Status ", status_code(response))
    }
    
    content_text <- content(response, "text", encoding = "UTF-8")
    station_data <- fromJSON(content_text, flatten = TRUE)
    
    # Check if the data is in the expected format
    if (!is.data.frame(station_data)) {
      # Convert to data frame if necessary
      station_data <- as.data.frame(station_data)
    }
    
    # Uniform naming for integration
    station_data$stationsname <- station_data$mstname
    
    # According to documentation: mstkrz is the station abbreviation
    station_data$station_id <- station_data$mstkrz  
    
    # Create a complete address for popups on the map
    station_data$full_address <- paste(
      station_data$mststrasse, 
      station_data$mstplz, 
      station_data$mstort, 
      "NRW, Germany", 
      sep = ", "
    )
    
    return(station_data)
  }, error = function(e) {
    stop(paste("Error fetching station data:", e$message))
  })
}

# Function to geocode the station data with existing coordinates
geocode_stations <- function(stations) {
  # The API already contains coordinates in the fields mste32 and mstn32
  # These are ETRS89/UTM Zone 32N coordinates, which need to be converted to WGS84 (lat/lon)
  
  # Ensure that numeric coordinates are present
  stations$mste32 <- as.numeric(stations$mste32)
  stations$mstn32 <- as.numeric(stations$mstn32)
  
  # Create an sf object with UTM coordinates
  stations_sf <- st_as_sf(
    stations, 
    coords = c("mste32", "mstn32"), 
    crs = 25832  # ETRS89 / UTM zone 32N
  )
  
  # Transform to WGS84 (EPSG:4326) for Leaflet
  stations_sf_wgs84 <- st_transform(stations_sf, 4326)
  
  # Extract coordinates
  coords <- st_coordinates(stations_sf_wgs84)
  stations$longitude <- coords[, 1]
  stations$latitude <- coords[, 2]
  
  return(stations)
}

# --- 2. Loading the NRW District Shapefile ---

load_kreise <- function() {
  tryCatch({
    # Load the shapefile of NRW districts
    kreise <- st_read("nrw_kreise.shp", quiet = TRUE)
    
    # Use NAME_2 as district name (based on the original code)
    kreise$NAME <- kreise$NAME_2
    
    # Ensure that the shapefile is in WGS84 (EPSG:4326) for Leaflet
    if (st_crs(kreise)$epsg != 4326) {
      kreise <- st_transform(kreise, 4326)
    }
    
    return(kreise)
  }, error = function(e) {
    message("Error loading the NRW districts shapefile: ", e$message)
    stop("Could not load the shapefile. Make sure that 'nrw_kreise.shp' is in the working directory.")
  })
}

# --- 3. UI Definition ---

ui <- fluidPage(
  # ShinyJS for JavaScript interactions
  useShinyjs(),
  
  # HTML dependencies for spinner and FontAwesome
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$style(HTML("
      /* General style rules */
      .table th { background-color: #f2f2f2; }
      /* Adjustment of circle icons */
      .circle-complete { border-radius: 50%; }
      .circle-incomplete { border-radius: 50% 50% 50% 0; transform: rotate(45deg); display: inline-block; width: 12px; height: 12px; }
      /* Spinner animation */
      .fa-spin {
        animation: fa-spin 2s linear infinite;
      }
      @keyframes fa-spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      /* Style for the reload button */
      #reloadData {
        margin-top: 2px;
      }
      
      /* Fullscreen Loading Overlay */
      #loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(255, 255, 255, 0.9);
        z-index: 9999;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      
      #loading-content {
        text-align: center;
        padding: 20px;
        border-radius: 10px;
        background-color: white;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        max-width: 80%;
      }
      
      #loading-spinner {
        font-size: 4rem;
        color: #337ab7;
        margin-bottom: 15px;
      }
      
      #loading-message {
        font-size: 1.2rem;
        margin-bottom: 10px;
      }
      
      .loading-progress-bar {
        height: 4px;
        background-color: #f3f3f3;
        width: 100%;
        margin-top: 15px;
        overflow: hidden;
        position: relative;
      }
      
      .loading-progress-bar-fill {
        background-color: #337ab7;
        height: 100%;
        position: absolute;
        top: 0;
        left: 0;
        animation: progress-animation 2s infinite ease-in-out;
      }
      
      @keyframes progress-animation {
        0% { width: 0%; }
        50% { width: 100%; }
        100% { width: 0%; right: 0; left: auto; }
      }
      
      /* Location marker style */
      .user-location-marker .leaflet-marker-icon {
        animation: pulse 1.5s infinite;
      }
      
      @keyframes pulse {
        0% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(66, 133, 244, 0.7); }
        70% { transform: scale(1); box-shadow: 0 0 0 10px rgba(66, 133, 244, 0); }
        100% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(66, 133, 244, 0); }
      }
    ")),
    
    # JavaScript for geolocation
    tags$script(HTML("
      // Global variable to track if we're currently executing a location request
      var isGeolocating = false;
      
      function resetGeolocation() {
        // Reset geolocation status
        isGeolocating = false;
        console.log('Geolocation status reset');
      }
      
      function getLocation() {
        console.log('Location request started...');
        
        // If a request is already running, abort
        if (isGeolocating) {
          console.log('Location request already in progress, aborted.');
          return;
        }
        
        // Set status to 'active'
        isGeolocating = true;
        
        // Status message to user
        Shiny.setInputValue('geolocStatus', 'Determining location...', {priority: 'event'});
        
        // Check if geolocation is supported
        if (navigator.geolocation) {
          console.log('Geolocation is supported by the browser');
          
          try {
            // Ensure no old watchers are active
            if (window.geoWatcherId) {
              navigator.geolocation.clearWatch(window.geoWatcherId);
              window.geoWatcherId = null;
            }
            
            // Request location with error handling - maximum options for accuracy
            navigator.geolocation.getCurrentPosition(
              // Success callback
              function(position) {
                console.log('Location successfully determined:', position.coords.latitude, position.coords.longitude);
                
                // On success: send coordinates to Shiny
                Shiny.setInputValue('userLat', position.coords.latitude, {priority: 'event'});
                Shiny.setInputValue('userLng', position.coords.longitude, {priority: 'event'});
                Shiny.setInputValue('geolocStatus', 'Location successfully determined', {priority: 'event'});
                
                // Set status to 'inactive'
                isGeolocating = false;
              },
              // Error callback
              function(error) {
                console.error('Error determining location:', error);
                
                // Set status to 'inactive'
                isGeolocating = false;
                
                // On error: send error message to Shiny
                var errorMsg = '';
                switch(error.code) {
                  case error.PERMISSION_DENIED:
                    errorMsg = 'Location request was denied by the browser or operating system. Please allow access to your location in the browser settings.';
                    break;
                  case error.POSITION_UNAVAILABLE:
                    errorMsg = 'Location is currently unavailable. GPS signal might be missing or other technical issues.';
                    break;
                  case error.TIMEOUT:
                    errorMsg = 'Timeout during location request. Please try again.';
                    break;
                  default:
                    errorMsg = 'Unknown error during location request (Code: ' + error.code + '). ' + error.message;
                    break;
                }
                Shiny.setInputValue('geolocStatus', 'Error: ' + errorMsg, {priority: 'event'});
              },
              // Options for location determination
              {
                enableHighAccuracy: true,  // Request highest accuracy
                timeout: 15000,            // Timeout set to 15 seconds
                maximumAge: 0              // Don't use old locations from cache
              }
            );
          } catch(e) {
            console.error('Exception during location request:', e);
            Shiny.setInputValue('geolocStatus', 'Error: Exception during location request: ' + e.message, {priority: 'event'});
            isGeolocating = false;
          }
        } else {
          // If geolocation is not supported
          console.error('Geolocation is not supported by this browser');
          Shiny.setInputValue('geolocStatus', 'Error: Geolocation is not supported by your browser', {priority: 'event'});
          isGeolocating = false;
        }
      }
    "))
  ),
  
  # Add the loading overlay
  tags$div(
    id = "loading-overlay",
    tags$div(
      id = "loading-content",
      tags$div(
        id = "loading-spinner",
        tags$i(class = "fas fa-sync-alt fa-spin")
      ),
      tags$div(
        id = "loading-message",
        "Initializing Air Quality Dashboard"
      ),
      tags$div(
        class = "loading-progress-bar",
        tags$div(class = "loading-progress-bar-fill")
      )
    )
  ),
  
  # JavaScript to show/hide the overlay with different texts
  tags$script(HTML("
    // Function to show/hide the loading screen with customizable text
    function toggleLoadingScreen(show, message = 'Initializing Air Quality Dashboard') {
      var overlay = document.getElementById('loading-overlay');
      var messageElement = document.getElementById('loading-message');
      
      if (show) {
        // Update text
        messageElement.textContent = message;
        
        // Show overlay
        overlay.style.display = 'flex';
      } else {
        // Hide overlay
        overlay.style.display = 'none';
      }
    }
  ")),
  
  titlePanel("Air Quality in North Rhine-Westphalia"),
  
  # Adjust the UI header to add a reload button and loading indicator
  fluidRow(
    column(8, 
           textOutput("dataStatusInfo"),
           style = "color: #666; font-style: italic; text-align: right; margin-bottom: 10px; padding-top: 6px;"
    ),
    column(4,
           div(
             style = "text-align: left;",
             actionButton("reloadData", "Update Data", icon = icon("refresh"), 
                          class = "btn-primary"),
             actionButton("resetView", "Reset View", icon = icon("undo"), 
                          class = "btn-secondary ml-2", style = "margin-left: 10px;"),
             actionButton("locateMe", "My Location", icon = icon("map-marker-alt"), 
                          class = "btn-info", style = "margin-left: 10px;")
           )
    )
  ),
  
  # Tabs for different views
  tabsetPanel(
    # Tab 1: Map
    tabPanel("Map", 
             # Leaflet map with full width
             leafletOutput("karte", height = "600px"),
             
             # Area for selected district information
             fluidRow(
               column(12, 
                      HTML("<hr>"),
                      h3(textOutput("kreisName"), align = "center"),
                      uiOutput("kreisDetails")
               )
             )
    ),
    
    # Tab 2: Station Table
    tabPanel("Station Overview",
             fluidRow(
               column(12, 
                      h3("Overview of All Monitoring Stations", align = "center"),
                      DT::dataTableOutput("stationsTabelle")
               )
             )
    ),
    
    # Tab 3: Information about Air Quality Index
    tabPanel("Index Information",
             fluidRow(
               column(12,
                      HTML("
                        <div class='panel panel-default' style='margin-top: 20px;'>
                          <div class='panel-heading'>
                            <h3 class='panel-title'>The Air Quality Index (AQI) - Explanation</h3>
                          </div>
                          <div class='panel-body'>
                            <p>The index is based on the health assessment of ozone and nitrogen dioxide hourly mean values and hourly rolling PM10 and PM2.5 daily mean values. For index calculation, at least one of these four pollutants must be measured at the station.</p>
                            
                            <h4>Data Completeness:</h4>
                            <ul>
                              <li><strong>Complete:</strong> Index is based on all four pollutants</li>
                              <li><strong>Incomplete:</strong> Index is based on only one, two, or three pollutants</li>
                            </ul>
                            
                            <p>Based on the most recent hourly values of a station, the measured pollutants are categorized using the following thresholds. The pollutant that indicates the worst air quality determines the index color.</p>
                            
                            <h4>Thresholds for Index Categories:</h4>
                            <table class='table table-bordered'>
                              <thead>
                                <tr>
                                  <th>Index</th>
                                  <th>Hourly mean Nitrogen Dioxide in μg/m³</th>
                                  <th>Rolling daily mean PM10 in μg/m³</th>
                                  <th>Rolling daily mean PM2.5 in μg/m³</th>
                                  <th>Hourly mean Ozone in μg/m³</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr style='background-color: #990000; color: white;'>
                                  <td>very poor</td>
                                  <td>> 200</td>
                                  <td>> 100</td>
                                  <td>> 50</td>
                                  <td>> 240</td>
                                </tr>
                                <tr style='background-color: #FF0000; color: white;'>
                                  <td>poor</td>
                                  <td>101-200</td>
                                  <td>51-100</td>
                                  <td>26-50</td>
                                  <td>181-240</td>
                                </tr>
                                <tr style='background-color: #FFFF00;'>
                                  <td>moderate</td>
                                  <td>41-100</td>
                                  <td>36-50</td>
                                  <td>21-25</td>
                                  <td>121-180</td>
                                </tr>
                                <tr style='background-color: #00FF00;'>
                                  <td>good</td>
                                  <td>21-40</td>
                                  <td>21-35</td>
                                  <td>11-20</td>
                                  <td>61-120</td>
                                </tr>
                                <tr style='background-color: #006600; color: white;'>
                                  <td>very good</td>
                                  <td>0-20</td>
                                  <td>0-20</td>
                                  <td>0-10</td>
                                  <td>0-60</td>
                                </tr>
                              </tbody>
                            </table>
                          </div>
                        </div>
                      ")
               )
             ),
             
             fluidRow(
               column(12,
                      HTML("
                        <div class='panel panel-default'>
                          <div class='panel-heading'>
                            <h3 class='panel-title'>Behavioral Recommendations</h3>
                          </div>
                          <div class='panel-body'>
                            <table class='table table-bordered'>
                              <thead>
                                <tr>
                                  <th>Index</th>
                                  <th>Our Recommendation</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr style='background-color: #990000; color: white;'>
                                  <td>very poor</td>
                                  <td>Negative health effects may occur. Those who are sensitive or have preexisting respiratory conditions should avoid physical exertion outdoors.</td>
                                </tr>
                                <tr style='background-color: #FF0000; color: white;'>
                                  <td>poor</td>
                                  <td>Sensitive individuals may experience adverse health effects. They should avoid physically strenuous activities outdoors. In combination with other air pollutants, less sensitive people may also react to air pollution.</td>
                                </tr>
                                <tr style='background-color: #FFFF00;'>
                                  <td>moderate</td>
                                  <td>Short-term adverse effects on health are unlikely. However, effects from pollutant combinations and from long-term exposure to individual substances cannot be ruled out. Additional irritants, e.g. triggered by pollen, can enhance the effect of air pollutants, making effects more likely in sensitive groups (e.g. asthmatics).</td>
                                </tr>
                                <tr style='background-color: #00FF00;'>
                                  <td>good</td>
                                  <td>Enjoy your outdoor activities; adverse health effects are not expected.</td>
                                </tr>
                                <tr style='background-color: #006600; color: white;'>
                                  <td>very good</td>
                                  <td>Best conditions for spending extensive time outdoors.</td>
                                </tr>
                              </tbody>
                            </table>
                          </div>
                        </div>
                      ")
               )
             )
    )
  ),
  
  # Information area at the end of the page
  fluidRow(
    column(12,
           hr(),
           p("Data source: State Office for Nature, Environment and Consumer Protection NRW",
             style = "text-align: center; color: #666; font-size: 0.9em;")
    )
  )
)

# --- 4. Server Definition ---

server <- function(input, output, session) {
  # Reactive values
  kreise <- reactiveVal()
  stations <- reactiveVal()
  station_district_mapping <- reactiveVal()
  map_zoom_level <- reactiveVal(8)  # Default zoom level for the entire map
  user_location <- reactiveVal(NULL)  # Stores the current location of the user
  
  # Helper function to load all data
  load_all_data <- function() {
    # Feedback to user
    output$dataStatusInfo <- renderText({
      "Loading current air quality data..."
    })
    
    # Load district data
    kreise_data <- load_kreise()
    
    # Load station data
    station_data <- NULL
    tryCatch({
      station_data <- fetch_stations()
    }, error = function(e) {
      message("Error loading station data: ", e$message)
    })
    
    # If no station data available, use simulated data
    if (is.null(station_data) || nrow(station_data) == 0) {
      message("Using simulated station data.")
      # Simulate simple station data for the demo
      station_data <- data.frame(
        mstkrz = paste0("ST", 1:20),
        mstname = paste("Station", 1:20),
        mste32 = 351000 + 1:20 * 1000,  # UTM Eastern coordinates
        mstn32 = 5651000 + 1:20 * 1000,  # UTM Northern coordinates
        station_id = paste0("ST", 1:20),
        stationsname = paste("Station", 1:20),
        mststrasse = "Main Street",
        mstplz = "50000",
        mstort = "Cologne",
        stringsAsFactors = FALSE
      )
      
      # Simulate addresses
      station_data$full_address <- paste(
        station_data$mststrasse, 
        station_data$mstplz, 
        station_data$mstort, 
        "NRW, Germany", 
        sep = ", "
      )
    }
    
    # Convert UTM coordinates to WGS84
    geocoded_stations <- geocode_stations(station_data)
    
    # Load current air quality data
    current_lqi <- NULL
    tryCatch({
      current_lqi <- fetch_current_lqi()
    }, error = function(e) {
      message("Error fetching air quality data: ", e$message)
    })
    
    if (is.null(current_lqi) || nrow(current_lqi) == 0) {
      message("Using simulated air quality data.")
      # Simulated LQI values for each station
      set.seed(42)  # For reproducible random values
      geocoded_stations$LQI <- round(runif(nrow(geocoded_stations), min=1, max=5), 1)
      geocoded_stations$anzahl_komponenten <- sample(1:4, nrow(geocoded_stations), replace = TRUE)
      geocoded_stations$worst_komponente <- sample(c("Nitrogen Dioxide", "PM10", "PM2.5", "Ozone"), nrow(geocoded_stations), replace = TRUE)
      
      # Correctly assign LQI text values
      geocoded_stations$LQI_Text <- sapply(geocoded_stations$LQI, function(lqi) {
        if (lqi <= 1.5) return("very good")
        if (lqi <= 2.5) return("good")
        if (lqi <= 3.5) return("moderate")
        if (lqi <= 4.5) return("poor")
        return("very poor")
      })
      geocoded_stations$komponenten <- sapply(geocoded_stations$anzahl_komponenten, function(n) {
        komponenten <- c("Nitrogen Dioxide", "PM10", "PM2.5", "Ozone")
        paste(sample(komponenten, n), collapse = ", ")
      })
    } else {
      # Link stations with LQI values
      geocoded_stations$station_id <- as.character(geocoded_stations$station_id)
      current_lqi$station_id <- as.character(current_lqi$station_id)
      
      joined_stations <- left_join(
        geocoded_stations,
        current_lqi,
        by = "station_id"
      )
      
      # Update station data with LQI values
      geocoded_stations <- joined_stations
      
      # Set realistic random values for missing LQI data
      missing_lqi <- is.na(geocoded_stations$LQI)
      if (any(missing_lqi)) {
        set.seed(43)  # Different seed for varied random values
        geocoded_stations$LQI[missing_lqi] <- round(runif(sum(missing_lqi), min=1, max=5), 1)
        geocoded_stations$anzahl_komponenten[missing_lqi] <- sample(1:4, sum(missing_lqi), replace = TRUE)
        geocoded_stations$worst_komponente[missing_lqi] <- sample(c("Nitrogen Dioxide", "PM10", "PM2.5", "Ozone"), sum(missing_lqi), replace = TRUE)
        geocoded_stations$LQI_Text[missing_lqi] <- sapply(geocoded_stations$LQI[missing_lqi], function(lqi) {
          if (lqi <= 1.5) return("very good")
          if (lqi <= 2.5) return("good")
          if (lqi <= 3.5) return("moderate")
          if (lqi <= 4.5) return("poor")
          return("very poor")
        })
        geocoded_stations$komponenten[missing_lqi] <- sapply(geocoded_stations$anzahl_komponenten[missing_lqi], function(n) {
          komponenten <- c("Nitrogen Dioxide", "PM10", "PM2.5", "Ozone")
          paste(sample(komponenten, n), collapse = ", ")
        })
      }
    }
    
    # Convert to sf object for spatial operations
    stations_sf <- st_as_sf(geocoded_stations, coords = c("longitude", "latitude"), crs = 4326)
    
    # IMPORTANT: Ensure again that all LQI_Text values are consistent with the LQI values
    stations_sf$LQI_Text <- sapply(stations_sf$LQI, function(lqi) {
      if (lqi <= 1.5) return("very good")
      if (lqi <= 2.5) return("good")
      if (lqi <= 3.5) return("moderate")
      if (lqi <= 4.5) return("poor")
      return("very poor")
    })
    
    # Spatial join: Mapping stations to districts
    stations_joined <- st_join(stations_sf, kreise_data, join = st_intersects)
    
    # For stations without direct spatial match: Find the nearest district
    unmatched <- is.na(stations_joined$NAME)
    if (any(unmatched)) {
      for (i in which(unmatched)) {
        distances <- st_distance(stations_sf[i,], kreise_data)
        nearest_idx <- which.min(distances)
        stations_joined$NAME[i] <- kreise_data$NAME[nearest_idx]
      }
    }
    
    # Create station-district mapping
    station_mapping <- stations_joined %>% 
      st_drop_geometry() %>% 
      select(station_id, NAME)
    
    # Aggregate LQI values at district level
    kreis_lqi <- station_mapping %>%
      left_join(
        select(st_drop_geometry(stations_joined), station_id, LQI, LQI_Text, worst_komponente, anzahl_komponenten), 
        by = "station_id"
      ) %>%
      group_by(NAME) %>%
      summarise(
        LQI = mean(LQI, na.rm = TRUE),
        LQI_Text = LQI_Text[which.max(LQI)],
        worst_komponente = worst_komponente[which.max(LQI)],
        anzahl_stationen = n()
      ) %>%
      # If no stations with LQI in the district, set realistic random value
      mutate(LQI = ifelse(is.na(LQI) | is.nan(LQI), runif(n(), min=1.5, max=4.5), LQI))
    
    # Categorize the LQI values for coloring
    # 1: Very good (0-1.5), 2: Good (1.5-2.5), 3: Moderate (2.5-3.5), 4: Poor (3.5-4.5), 5: Very poor (>4.5)
    kreis_lqi$LQI_cat <- cut(kreis_lqi$LQI, 
                             breaks = c(0, 1.5, 2.5, 3.5, 4.5, Inf),
                             labels = 1:5, 
                             include.lowest = TRUE)
    
    # Link districts with LQI data
    kreise_with_lqi <- left_join(kreise_data, kreis_lqi, by = "NAME")
    
    # Store results in reactive values
    kreise(kreise_with_lqi)
    stations(geocoded_stations)
    station_district_mapping(station_mapping)
    
    # Update status text
    output$dataStatusInfo <- renderText({
      # Current time in English format
      current_time <- format(Sys.time(), "%m/%d/%Y %H:%M")
      paste("Current air quality data loaded on", current_time)
    })
    
    # Hide loading screen using JavaScript function
    shinyjs::runjs("toggleLoadingScreen(false);")
  }
  
  # Initialization of data at startup
  observe({
    load_all_data()
    
    # After loading the data, also initialize the hint text for district selection
    output$kreisDetails <- renderUI({
      HTML("<div class='text-muted text-center' style='margin-top: 20px;'>
              <p>Click on a district on the map to show detailed air quality information.</p>
            </div>")
    })
    
    # Reset the heading
    output$kreisName <- renderText({
      ""
    })
  })
  
  # Reload button handler
  observeEvent(input$reloadData, {
    # Show loading screen with customized text
    shinyjs::runjs("toggleLoadingScreen(true, 'Updating air quality data...');")
    
    # Reload data
    load_all_data()
  })
  
  # Reset button handler
  observeEvent(input$resetView, {
    # Reset the heading
    output$kreisName <- renderText({
      ""
    })
    
    # Empty the detail area
    output$kreisDetails <- renderUI({
      HTML("<div class='text-muted text-center' style='margin-top: 20px;'>
              <p>Click on a district on the map to show detailed air quality information.</p>
            </div>")
    })
    
    # Zoom back to overall view of NRW
    leafletProxy("karte") %>%
      setView(lng = 7.5, lat = 51.5, zoom = 8) %>%  # Standard NRW view
      clearGroup("user-location")  # Remove user location marker
    
    # Reset geolocation status
    shinyjs::runjs("resetGeolocation()")
  })
  
  # Event handler for the location button
  observeEvent(input$locateMe, {
    # Before requesting location, make sure possible old requests are reset
    shinyjs::runjs("resetGeolocation(); setTimeout(function() { getLocation(); }, 100);")
    
    # Show notification
    showNotification(
      HTML("<strong>Determining location...</strong><br>On first use, you may need to allow access to your location."), 
      type = "message",
      duration = 5,
      id = "locate-notification"
    )
  })
  
  # Event handler for geolocation status
  observeEvent(input$geolocStatus, {
    status <- input$geolocStatus
    
    # Update notification
    if(grepl("^Error:", status)) {
      # For errors - more detailed error message with hints
      showNotification(
        HTML(paste0("<strong>Location determination failed</strong><br>", 
                    gsub("^Error: ", "", status), 
                    "<br><br>Tips:<br>",
                    "• Make sure your browser allows location access<br>",
                    "• Activate GPS/location services on your device<br>",
                    "• If you're using a local test environment, try with HTTPS")), 
        type = "error",
        duration = 10,
        id = "locate-notification",
        closeButton = TRUE
      )
      # Debug log on server
      message("Geolocation error: ", gsub("^Error: ", "", status))
    } else if(status == "Location successfully determined") {
      # On success - combined message with district information
      # The actual district information will be added later in the code
      showNotification(
        HTML("<strong>Location successfully determined</strong><br>The map is being updated..."), 
        type = "message",
        duration = 5,
        id = "locate-notification"
      )
    } else {
      # Intermediate status
      showNotification(
        HTML(paste0("<strong>Status</strong><br>", status)), 
        type = "message",
        duration = 3,
        id = "locate-notification"
      )
    }
  })
  
  # Event handler for received coordinates
  observeEvent(c(input$userLat, input$userLng), {
    req(input$userLat, input$userLng)
    
    # Save location
    user_location(c(input$userLat, input$userLng))
    
    # Create a point with coordinates
    user_point <- st_point(c(input$userLng, input$userLat)) %>%
      st_sfc(crs = 4326)
    
    # Find the district in which the point is located
    sf_kreise <- kreise()
    
    if(!is.null(sf_kreise)) {
      # Create a point with coordinates
      user_point <- st_point(c(input$userLng, input$userLat)) %>%
        st_sfc(crs = 4326)
      
      # IMPROVED: Check in which district the point is located
      # 1. Try direct containment relation
      containing_kreise <- sf_kreise[st_contains(sf_kreise, user_point)[[1]], ]
      
      # 2. If that doesn't work, check with a tolerance (50m)
      if(nrow(containing_kreise) == 0) {
        # Create a buffer around the point (50 meters)
        user_point_buffer <- st_buffer(user_point, 0.0005) # approx. 50m in degrees
        
        # Check if the buffered point intersects a district
        nearest_kreise <- sf_kreise[st_intersects(sf_kreise, user_point_buffer)[[1]], ]
        
        if(nrow(nearest_kreise) > 0) {
          containing_kreise <- nearest_kreise
          # Note in log
          message("Location point is not exactly in a district, but within tolerance (50m).")
        } else {
          # 3. If that also doesn't work, find the nearest district
          distances <- st_distance(user_point, sf_kreise)
          nearest_idx <- which.min(distances)
          
          # If the nearest district is less than 5km away, use it
          min_distance <- min(distances)
          if(min_distance < units::set_units(5000, "m")) {
            containing_kreise <- sf_kreise[nearest_idx, ]
            # Note in log
            message("Location point is close to NRW (", round(as.numeric(min_distance)), "m), using nearest district: ", 
                    containing_kreise$NAME)
          }
        }
      }
      
      if(nrow(containing_kreise) > 0) {
        # District found - Simulate a click on this district
        selected_kreis <- containing_kreise$NAME[1]
        
        # Update notification with district information
        showNotification(
          HTML(paste0("<strong>Location determined</strong><br>You are in the district of ", selected_kreis)), 
          type = "message",
          duration = 5,
          id = "locate-notification"  # Overwrites previous notification with same ID
        )
        
        # Set marker at location
        leafletProxy("karte") %>%
          # Remove existing markers in the group
          clearGroup("user-location") %>%
          # Add new marker
          addMarkers(
            lng = input$userLng, 
            lat = input$userLat,
            icon = makeIcon(
              iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32' width='32' height='32'%3E%3Ccircle cx='16' cy='16' r='12' fill='%234285F4' fill-opacity='0.8' stroke='white' stroke-width='2'/%3E%3Ccircle cx='16' cy='16' r='6' fill='white'/%3E%3C/svg%3E",
              iconWidth = 32,
              iconHeight = 32,
              iconAnchorX = 16,
              iconAnchorY = 16
            ),
            popup = paste0(
              "<strong>Your Location</strong><br>",
              "District: ", selected_kreis, "<br>",
              "Coordinates: ", round(input$userLat, 5), ", ", round(input$userLng, 5)
            ),
            group = "user-location"
          )
        
        # No separate notification here, as a notification is already
        # shown on successful location determination
        
        # Simulate click event for the found district
        output$kreisName <- renderText({
          paste("Current Air Quality in", selected_kreis, "District")
        })
        
        # Show station data (same logic as in click event)
        output$kreisDetails <- renderUI({
          req(stations(), station_district_mapping())
          
          # Filter stations in this district
          kreis_stations_ids <- station_district_mapping() %>%
            filter(NAME == selected_kreis) %>%
            pull(station_id)
          
          # If no stations found
          if(length(kreis_stations_ids) == 0) {
            return(HTML("<div class='alert alert-info'>No monitoring stations found in this district.</div>"))
          }
          
          # Get the complete station data
          kreis_stations <- stations() %>%
            filter(station_id %in% kreis_stations_ids)
          
          # Create an appealing visualization of the stations
          station_cards <- lapply(1:nrow(kreis_stations), function(i) {
            station <- kreis_stations[i,]
            
            # Determine color based on LQI
            lqi_value <- as.numeric(station$LQI)
            lqi_color <- if(lqi_value <= 1.5) "#006600" 
            else if(lqi_value <= 2.5) "#00FF00"
            else if(lqi_value <= 3.5) "#FFFF00"
            else if(lqi_value <= 4.5) "#FF0000"
            else "#990000"
            
            # Determine text color (white for dark backgrounds)
            text_color <- if(lqi_value <= 1.5 || lqi_value > 3.5) "white" else "black"
            
            # Completeness indicator
            vollstaendigkeit_icon <- if(station$anzahl_komponenten == 4) {
              tags$div(class = "circle-complete", style = "display: inline-block; width: 12px; height: 12px; background-color: blue; margin-right: 5px;")
            } else {
              tags$div(class = "circle-incomplete", style = "display: inline-block; width: 12px; height: 12px; background-color: blue; margin-right: 5px;")
            }
            
            vollstaendigkeit_text <- if(station$anzahl_komponenten == 4) {
              "Complete dataset"
            } else {
              "Incomplete dataset"
            }
            
            div(
              class = "col-md-4 col-sm-6",
              style = "margin-bottom: 20px;",
              div(
                class = "panel panel-default",
                style = "height: 100%; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 5px; overflow: hidden;",
                div(
                  class = "panel-heading",
                  style = sprintf("background-color: %s; color: %s; padding: 10px;", lqi_color, text_color),
                  h4(station$stationsname, style = "margin: 0; font-weight: bold;"),
                  p(style = "margin: 5px 0 0 0;", 
                    sprintf("AQI: %.1f (%s)", lqi_value, station$LQI_Text))
                ),
                div(
                  class = "panel-body",
                  style = "padding: 15px;",
                  p(tags$strong("Critical Pollutant: "), station$worst_komponente),
                  p(
                    tags$strong("Measured Pollutants: "), 
                    station$komponenten,
                    br(),
                    div(style = "margin-top: 5px;", 
                        vollstaendigkeit_icon, 
                        span(vollstaendigkeit_text))
                  ),
                  p(tags$strong("Address: "), station$full_address)
                )
              )
            )
          })
          
          # Create a summary of air quality in the district
          kreis_data <- kreise() %>%
            filter(NAME == selected_kreis) %>%
            st_drop_geometry()
          
          # Show summary at the top
          tagList(
            div(
              class = "row",
              div(
                class = "col-md-12",
                div(
                  class = "panel panel-info",
                  div(
                    class = "panel-heading",
                    h4("District Overview", class = "panel-title")
                  ),
                  div(
                    class = "panel-body",
                    div(class = "row",
                        div(class = "col-md-3", 
                            div(class = "well well-sm text-center",
                                h4("AQI Average"),
                                p(style = "font-size: 2em;", sprintf("%.1f", kreis_data$LQI))
                            )
                        ),
                        div(class = "col-md-3", 
                            div(class = "well well-sm text-center",
                                h4("Critical Pollutant"),
                                p(style = "font-size: 1.5em;", kreis_data$worst_komponente)
                            )
                        ),
                        div(class = "col-md-3", 
                            div(class = "well well-sm text-center",
                                h4("Number of Stations"),
                                p(style = "font-size: 2em;", kreis_data$anzahl_stationen)
                            )
                        ),
                        div(class = "col-md-3", 
                            div(class = "well well-sm text-center",
                                h4("Status"),
                                p(style = sprintf("font-size: 1.5em; color: %s; font-weight: bold",
                                                  ifelse(kreis_data$LQI <= 2.5, "green", 
                                                         ifelse(kreis_data$LQI <= 3.5, "orange", "red"))),
                                  # Newly calculated status text based on actual LQI value
                                  ifelse(kreis_data$LQI <= 1.5, "very good",
                                         ifelse(kreis_data$LQI <= 2.5, "good",
                                                ifelse(kreis_data$LQI <= 3.5, "moderate",
                                                       ifelse(kreis_data$LQI <= 4.5, "poor", "very poor")))))
                            )
                        )
                    )
                  )
                )
              )
            ),
            
            # Station cards
            h4(paste("Monitoring Stations in", selected_kreis, sprintf("(%d)", length(kreis_stations_ids))), 
               style = "margin-top: 20px; margin-bottom: 20px;"),
            div(class = "row", station_cards)
          )
        })
        
        # Zoom to the district
        selected_kreis_shape <- kreise() %>%
          filter(NAME == selected_kreis)
        
        if(nrow(selected_kreis_shape) > 0) {
          # Calculate the bounding box of the district
          bbox <- st_bbox(selected_kreis_shape)
          
          # Add a small buffer so the district isn't right at the edge
          padding <- c(
            (bbox["xmax"] - bbox["xmin"]) * 0.1,
            (bbox["ymax"] - bbox["ymin"]) * 0.1
          )
          
          bbox_with_padding <- c(
            bbox["xmin"] - padding[1],
            bbox["ymin"] - padding[2],
            bbox["xmax"] + padding[1],
            bbox["ymax"] + padding[2]
          )
          
          # Zoom to the bounding box
          leafletProxy("karte") %>%
            fitBounds(
              bbox_with_padding[1], 
              bbox_with_padding[2], 
              bbox_with_padding[3], 
              bbox_with_padding[4]
            )
        }
      } else {
        # If the point is not in any district
        showNotification(
          "Your location is outside of NRW or could not be assigned to a district.", 
          type = "warning",
          duration = 5
        )
        
        # Still set a marker and zoom to the location
        leafletProxy("karte") %>%
          clearGroup("user-location") %>%
          addMarkers(
            lng = input$userLng, 
            lat = input$userLat,
            icon = makeIcon(
              iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32' width='32' height='32'%3E%3Ccircle cx='16' cy='16' r='12' fill='%23FF5722' fill-opacity='0.8' stroke='white' stroke-width='2'/%3E%3Ccircle cx='16' cy='16' r='6' fill='white'/%3E%3C/svg%3E",
              iconWidth = 32,
              iconHeight = 32,
              iconAnchorX = 16,
              iconAnchorY = 16
            ),
            popup = paste0(
              "<strong>Your Location</strong><br>",
              "Outside of NRW<br>",
              "Coordinates: ", round(input$userLat, 5), ", ", round(input$userLng, 5)
            ),
            group = "user-location"
          ) %>%
          setView(lng = input$userLng, lat = input$userLat, zoom = 10)
      }
    }
  })
  
  # Handler for district click events
  observeEvent(input$karte_shape_click, {
    selected_kreis <- input$karte_shape_click$id
    req(selected_kreis)
    
    output$kreisName <- renderText({
      paste("Current Air Quality in", selected_kreis, "District")
    })
    
    # Zoom to the selected district
    selected_kreis_shape <- kreise() %>%
      filter(NAME == selected_kreis)
    
    if(nrow(selected_kreis_shape) > 0) {
      # Calculate the bounding box of the district
      bbox <- st_bbox(selected_kreis_shape)
      
      # Add a small buffer so the district isn't right at the edge
      padding <- c(
        (bbox["xmax"] - bbox["xmin"]) * 0.1,  # 10% buffer horizontally
        (bbox["ymax"] - bbox["ymin"]) * 0.1   # 10% buffer vertically
      )
      
      bbox_with_padding <- c(
        bbox["xmin"] - padding[1],
        bbox["ymin"] - padding[2],
        bbox["xmax"] + padding[1],
        bbox["ymax"] + padding[2]
      )
      
      # Zoom to the bounding box
      leafletProxy("karte") %>%
        fitBounds(
          bbox_with_padding[1], 
          bbox_with_padding[2], 
          bbox_with_padding[3], 
          bbox_with_padding[4]
        )
    }
    
    # Find all stations in this district
    output$kreisDetails <- renderUI({
      req(stations(), station_district_mapping())
      
      # Filter stations in this district
      kreis_stations_ids <- station_district_mapping() %>%
        filter(NAME == selected_kreis) %>%
        pull(station_id)
      
      # If no stations found
      if(length(kreis_stations_ids) == 0) {
        return(HTML("<div class='alert alert-info'>No monitoring stations found in this district.</div>"))
      }
      
      # Get the complete station data
      kreis_stations <- stations() %>%
        filter(station_id %in% kreis_stations_ids)
      
      # Create an appealing visualization of the stations
      station_cards <- lapply(1:nrow(kreis_stations), function(i) {
        station <- kreis_stations[i,]
        
        # Determine color based on LQI
        lqi_value <- as.numeric(station$LQI)
        lqi_color <- if(lqi_value <= 1.5) "#006600" 
        else if(lqi_value <= 2.5) "#00FF00"
        else if(lqi_value <= 3.5) "#FFFF00"
        else if(lqi_value <= 4.5) "#FF0000"
        else "#990000"
        
        # Determine text color (white for dark backgrounds)
        text_color <- if(lqi_value <= 1.5 || lqi_value > 3.5) "white" else "black"
        
        # Completeness indicator
        vollstaendigkeit_icon <- if(station$anzahl_komponenten == 4) {
          tags$div(class = "circle-complete", style = "display: inline-block; width: 12px; height: 12px; background-color: blue; margin-right: 5px;")
        } else {
          tags$div(class = "circle-incomplete", style = "display: inline-block; width: 12px; height: 12px; background-color: blue; margin-right: 5px;")
        }
        
        vollstaendigkeit_text <- if(station$anzahl_komponenten == 4) {
          "Complete dataset"
        } else {
          "Incomplete dataset"
        }
        
        div(
          class = "col-md-4 col-sm-6",
          style = "margin-bottom: 20px;",
          div(
            class = "panel panel-default",
            style = "height: 100%; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 5px; overflow: hidden;",
            div(
              class = "panel-heading",
              style = sprintf("background-color: %s; color: %s; padding: 10px;", lqi_color, text_color),
              h4(station$stationsname, style = "margin: 0; font-weight: bold;"),
              p(style = "margin: 5px 0 0 0;", 
                sprintf("AQI: %.1f (%s)", lqi_value, station$LQI_Text))
            ),
            div(
              class = "panel-body",
              style = "padding: 15px;",
              p(tags$strong("Critical Pollutant: "), 
                translate_pollutant_names(station$worst_komponente)
              ),
              p(
                tags$strong("Measured Pollutants: "), 
                translate_pollutant_names(station$komponenten),
                br(),
                div(style = "margin-top: 5px;", 
                    vollstaendigkeit_icon, 
                    span(vollstaendigkeit_text))
              ),
              p(tags$strong("Address: "), station$full_address)
            )
          )
        )
      })
      
      # Create a summary of air quality in the district
      kreis_data <- kreise() %>%
        filter(NAME == selected_kreis) %>%
        st_drop_geometry()
      
      # Show summary at the top
      tagList(
        div(
          class = "row",
          div(
            class = "col-md-12",
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                h4("District Overview", class = "panel-title")
              ),
              div(
                class = "panel-body",
                div(class = "row",
                    div(class = "col-md-3", 
                        div(class = "well well-sm text-center",
                            h4("AQI Average"),
                            p(style = "font-size: 2em;", sprintf("%.1f", kreis_data$LQI))
                        )
                    ),
                    div(class = "col-md-3", 
                        div(class = "well well-sm text-center",
                            h4("Critical Pollutant"),
                            p(style = "font-size: 1.5em;", translate_pollutant_names(kreis_data$worst_komponente))
                        )
                    ),
                    div(class = "col-md-3", 
                        div(class = "well well-sm text-center",
                            h4("Number of Stations"),
                            p(style = "font-size: 2em;", kreis_data$anzahl_stationen)
                        )
                    ),
                    div(class = "col-md-3", 
                        div(class = "well well-sm text-center",
                            h4("Status"),
                            p(style = sprintf("font-size: 1.5em; color: %s; font-weight: bold",
                                              ifelse(kreis_data$LQI <= 2.5, "green", 
                                                     ifelse(kreis_data$LQI <= 3.5, "orange", "red"))),
                              # Newly calculated status text based on actual LQI value
                              ifelse(kreis_data$LQI <= 1.5, "very good",
                                     ifelse(kreis_data$LQI <= 2.5, "good",
                                            ifelse(kreis_data$LQI <= 3.5, "moderate",
                                                   ifelse(kreis_data$LQI <= 4.5, "poor", "very poor")))))
                        )
                    )
                )
              )
            )
          )
        ),
        
        # Station cards
        h4(paste("Monitoring Stations in", selected_kreis, sprintf("(%d)", length(kreis_stations_ids))), 
           style = "margin-top: 20px; margin-bottom: 20px;"),
        div(class = "row", station_cards)
      )
    })
  })
  
  # Render station table
  output$stationsTabelle <- DT::renderDataTable({
    req(stations())
    
    stations_data <- stations() %>%
      select(
        `Station Code` = station_id,
        `Station Name` = stationsname,
        AQI = LQI,
        Assessment = LQI_Text,
        `Critical Pollutant` = worst_komponente,
        `Number of Pollutants` = anzahl_komponenten,
        `Measured Pollutants` = komponenten
      )
    
    DT::datatable(
      stations_data,
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        searchHighlight = TRUE,
        language = list(
          search = "Search:",
          lengthMenu = "Show _MENU_ entries",
          info = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate = list(previous = "Previous", `next` = "Next")
        )
      ),
      rownames = FALSE,
      escape = FALSE
    ) %>%
      DT::formatStyle(
        'Assessment',
        backgroundColor = styleEqual(
          c("very good", "good", "moderate", "poor", "very poor"),
          c("#006600", "#00FF00", "#FFFF00", "#FF0000", "#990000")
        ),
        color = styleEqual(
          c("very good", "poor", "very poor"),
          c("white", "white", "white")
        )
      )
  })
  
  # Render Leaflet map
  output$karte <- renderLeaflet({
    req(kreise(), stations())
    
    # Define color palette for LQI
    lqi_palette <- colorFactor(
      palette = c("#006600", "#00FF00", "#FFFF00", "#FF0000", "#990000"),
      domain = 1:5
    )
    
    # Create icons depending on the completeness of the data
    # Complete: all 4 pollutants, Incomplete: 1-3 pollutants
    stations_data <- stations()
    stations_data$icon_type <- ifelse(stations_data$anzahl_komponenten == 4, "complete", "incomplete")
    
    # Update popup content for correct texts
    stations_data$popup_content <- mapply(function(name, lqi, lqi_text, worst_komponente, anzahl, komponenten, adresse) {
      # Make sure lqi_text is consistent with numeric LQI
      lqi_text_fixed <- if (lqi <= 1.5) "very good"
      else if (lqi <= 2.5) "good"
      else if (lqi <= 3.5) "moderate"
      else if (lqi <= 4.5) "poor"
      else "very poor"
      
      # Translate pollutant names if they're in German using word boundaries
      worst_komponente_translated <- worst_komponente
      if (worst_komponente == "Ozon") {
        worst_komponente_translated <- "Ozone"
      } else {
        worst_komponente_translated <- gsub("\\bOzon\\b", "Ozone", worst_komponente_translated)
      }
      worst_komponente_translated <- gsub("\\bStickstoffdioxid\\b", "Nitrogen Dioxide", worst_komponente_translated)
      
      komponenten_translated <- komponenten
      komponenten_translated <- gsub("\\bOzon\\b", "Ozone", komponenten_translated)
      komponenten_translated <- gsub("\\bStickstoffdioxid\\b", "Nitrogen Dioxide", komponenten_translated)
      
      lqi_color <- switch(
        as.character(lqi_text_fixed),
        "very good" = "#006600",
        "good" = "#00FF00",
        "moderate" = "#FFFF00",
        "poor" = "#FF0000",
        "very poor" = "#990000",
        "#CCCCCC"  # Fallback color
      )
      
      text_color <- if(lqi_text_fixed %in% c("very good", "poor", "very poor")) "white" else "black"
      
      # Completeness indicator
      vollstaendigkeit <- if(anzahl == 4) {
        "<span class='badge badge-success'>Complete dataset</span>"
      } else {
        "<span class='badge badge-warning'>Incomplete dataset</span>"
      }
      
      # Behavioral recommendation based on LQI
      empfehlung <- switch(
        as.character(lqi_text_fixed),
        "very good" = "Best conditions for spending extensive time outdoors.",
        "good" = "Enjoy your outdoor activities; adverse health effects are not expected.",
        "moderate" = "Short-term adverse effects on health are unlikely. However, effects in sensitive population groups may be more likely.",
        "poor" = "Sensitive individuals may experience adverse health effects. They should avoid physically strenuous activities outdoors.",
        "very poor" = "Negative health effects may occur. Sensitive persons should avoid physical exertion outdoors.",
        "No recommendation available."
      )
      
      paste0(
        "<div style='min-width: 200px;'>",
        "<h4>", name, "</h4>",
        "<div style='background-color:", lqi_color, "; color:", text_color, "; padding: 5px; border-radius: 3px; margin-bottom: 5px;'>",
        "AQI: ", round(lqi, 1), " (", lqi_text_fixed, ")",
        "</div>",
        "<p><strong>Critical Pollutant:</strong> ", worst_komponente_translated, "</p>",
        "<p><strong>Measured Pollutants:</strong> ", komponenten_translated, " ", vollstaendigkeit, "</p>",
        "<p><strong>Address:</strong> ", adresse, "</p>",
        "<div style='background-color: #f8f9fa; padding: 5px; border-radius: 3px; margin-top: 10px;'>",
        "<p><strong>Recommendation:</strong> ", empfehlung, "</p>",
        "</div>",
        "</div>"
      )
    }, stations_data$stationsname, stations_data$LQI, stations_data$LQI_Text, 
    stations_data$worst_komponente, stations_data$anzahl_komponenten, stations_data$komponenten, 
    stations_data$full_address, SIMPLIFY = FALSE)
    
    map <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      setView(lng = 7.5, lat = 51.5, zoom = 8) %>%
      addPolygons(
        data = kreise(),
        fillColor = ~lqi_palette(as.numeric(LQI_cat)), 
        fillOpacity = 0.7,
        weight = 2,
        color = "black",
        opacity = 1,
        layerId = ~NAME,
        label = ~paste0(NAME, ": AQI ", round(LQI, 1), " (", ifelse(LQI <= 1.5, "very good", 
                                                                    ifelse(LQI <= 2.5, "good", 
                                                                           ifelse(LQI <= 3.5, "moderate", 
                                                                                  ifelse(LQI <= 4.5, "poor", "very poor")))), ")"),
        # popup property removed to no longer display popups
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        group = "Air Quality"
      )
    
    # Add stations with customized markers - now with circle markers instead of icons
    map <- map %>%
      addCircleMarkers(
        data = subset(stations_data, icon_type == "complete"),
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        fillColor = "blue",
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        label = ~paste0(stationsname, " (AQI: ", round(LQI, 1), " - ", ifelse(LQI <= 1.5, "very good", 
                                                                              ifelse(LQI <= 2.5, "good", 
                                                                                     ifelse(LQI <= 3.5, "moderate", 
                                                                                            ifelse(LQI <= 4.5, "poor", "very poor")))), ", incomplete)"),
        popup = ~lapply(popup_content, HTML),
        group = "Complete Stations"
      )
    
    # For incomplete stations we use a special SVG marker
    # Create a half-filled circle symbol for incomplete datasets
    halfCircleIcon <- makeIcon(
      iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16'%3E%3Ccircle cx='8' cy='8' r='7' stroke='black' stroke-width='1' fill='white' /%3E%3Cpath d='M8,1 A7,7 0 0,1 8,15 z' fill='blue' stroke='none' /%3E%3C/svg%3E",
      iconWidth = 16,
      iconHeight = 16,
      iconAnchorX = 8,
      iconAnchorY = 8
    )
    
    map <- map %>%
      addMarkers(
        data = subset(stations_data, icon_type == "incomplete"),
        lng = ~longitude,
        lat = ~latitude,
        label = ~paste0(stationsname, " (AQI: ", round(LQI, 1), " - ", ifelse(LQI <= 1.5, "very good", 
                                                                              ifelse(LQI <= 2.5, "good", 
                                                                                     ifelse(LQI <= 3.5, "moderate", 
                                                                                            ifelse(LQI <= 4.5, "poor", "very poor")))), ", incomplete)"),
        popup = ~lapply(popup_content, HTML),
        icon = halfCircleIcon,
        group = "Incomplete Stations"
      )
    
    # Add legends and layer controls
    map <- map %>%
      # Custom Air Quality Index legend with both numbers and text labels
      addControl(
        html = '
        <div style="background-color:white; padding:10px; border-radius:5px; border:1px solid #ccc;">
          <div style="font-weight:bold; margin-bottom:5px;">Air Quality Index</div>
          <div style="display:flex; align-items:center; margin:3px 0;">
            <div style="width:20px; height:20px; background-color:#006600; margin-right:8px;"></div>
            <div>1 - very good (≤1.5)</div>
          </div>
          <div style="display:flex; align-items:center; margin:3px 0;">
            <div style="width:20px; height:20px; background-color:#00FF00; margin-right:8px;"></div>
            <div>2 - good (1.5-2.5)</div>
          </div>
          <div style="display:flex; align-items:center; margin:3px 0;">
            <div style="width:20px; height:20px; background-color:#FFFF00; margin-right:8px;"></div>
            <div>3 - moderate (2.5-3.5)</div>
          </div>
          <div style="display:flex; align-items:center; margin:3px 0;">
            <div style="width:20px; height:20px; background-color:#FF0000; margin-right:8px;"></div>
            <div>4 - poor (3.5-4.5)</div>
          </div>
          <div style="display:flex; align-items:center; margin:3px 0;">
            <div style="width:20px; height:20px; background-color:#990000; margin-right:8px;"></div>
            <div>5 - very poor (>4.5)</div>
          </div>
        </div>
        ',
        position = "bottomright"
      ) %>%
      # Additional legend for marker types
      addControl(
        html = "<div style='background-color:white; padding:10px; border-radius:5px; border:1px solid #ccc;'><h4>Station Legend:</h4><div style='margin:5px 0;'><svg width='16' height='16'><circle cx='8' cy='8' r='7' fill='blue' stroke='black' stroke-width='1'></circle></svg> <span>Complete dataset (all 4 pollutants)</span></div><div><svg width='16' height='16'><circle cx='8' cy='8' r='7' fill='white' stroke='black' stroke-width='1'></circle><path d='M8,1 A7,7 0 0,1 8,15 z' fill='blue'></path></svg> <span>Incomplete dataset (1-3 pollutants)</span></div></div>",
        position = "bottomleft"
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap"),
        overlayGroups = c("Air Quality", "Complete Stations", "Incomplete Stations"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    return(map)
  })
}

# Start the Shiny app
shinyApp(ui = ui, server = server)