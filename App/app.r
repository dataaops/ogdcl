library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(dplyr)

# Your site data
map <- data.frame(
  Site = c("Rig N-2","Rig N-2","Rig N-2", "Rig N-3", "Rig N-4", "Rig N-5","Rig N-6", "Rig N-55","SK-750","SP-1", "SP-2" ,"SP-3", "SP-3", "SP-4", "SP-5","SP-5"),
  Source_Name = c("Tando Alam", "Uch", "Tando Allahyar", "Karak", "Kohati", "Sanghar", "Sadiqabad", "Tando Jam","Tando Muhammad Khan",
                  "Sehwan", "Loralai", "Qilla Saifullah", "Kot Addu", "Kohat", "Jati", "Chotiari"),
  Dest_Name = c( "Uch","Tando Allahyar",  "Tando Jam","Pindi Gheb", "Chakwal", "Tando Jam", "Tando Jam",NA, "Tando Jam", NA,
                 "Larkana", "Kot Addu"," Jhal Magsi",NA, "Chotiari", "Uch"),
  Source_Lat = c(25.30462784, 31.5204, 30.1575, 24.9136, 28.3949, 33.607163, 28.13067129, 25.438634,25.25683282, 26.38927294 , 30.48252189, 30.70595739, 30.4598,33.607163 , 24.3565174,26.07643379),
  Source_Lon = c(68.50163408, 74.3587, 67.0021, 67.0599, 70.2998, 71.286263, 69.96926704, 68.571107,68.54551339, 67.84868522, 69.44548962, 68.28947656, 70.9660 , 71.286263,68.2741903, 69.20025348),
  Dest_Lat = c(28.60965314, 25.3960, 33.6844, 30.1575, 32.9310991,25.438634,25.438634,NA,  25.38396283,NA, 27.55898,30.4598, 28.2816 ,NA,26.05673379, 28.5826),
  Dest_Lon = c(68.62583841, 68.3578, 73.0479, 67.0021, 72.8550863,68.571107,68.571107, NA, 68.57481627, NA, 68.21204 , 70.9660, 67.4576,NA,69.1939, 68.1715)
)

# Process site location data
site_location <- map %>%
  mutate(Location = ifelse(is.na(Dest_Name), Source_Name, Dest_Name)) %>%
  mutate(Lat = ifelse(is.na(Dest_Lat), Source_Lat, Dest_Lat)) %>%
  mutate(Long = ifelse(is.na(Dest_Lon), Source_Lon, Dest_Lon)) %>%
  group_by(Site) %>%
  slice_tail(n = 1) %>%   # Keep the last row for each group
  ungroup() %>%
  select(Site, Location, Lat, Long)

# Pakistan hospital data
hospitals_data <- data.frame(
  name = c("Aga Khan University Hospital", "Shaukat Khanum Memorial Hospital", 
           "Pakistan Institute of Medical Sciences", "Holy Family Hospital",
           "Allied Hospital", "Nishtar Medical University Hospital", 
           "Hayatabad Medical Complex", "Sandeman Provincial Hospital",
           "Liaquat University Hospital", "DHQ Hospital Gujranwala",
           "Sialkot International Hospital", "DHQ Hospital Sargodha",
           "Jinnah Postgraduate Medical Centre", "Services Hospital Lahore",
           "Combined Military Hospital Rawalpindi", "Mayo Hospital Lahore"),
  lat = c(25.051820, 33.558310, 33.6973, 33.5651,
          31.4504, 30.1575, 34.0151, 30.1798,
          25.3960, 32.1877, 32.4972, 32.0836,
          24.8607, 31.5804, 33.5651, 31.5804),
  lng = c(68.610000, 73.226480, 72.9734, 73.0169,
          73.1350, 71.5249, 71.5249, 66.9750,
          68.3578, 74.1945, 74.5361, 72.6711,
          67.0011, 74.3587, 73.0169, 74.3587),
  city = c("Karachi", "Lahore", "Islamabad", "Rawalpindi",
           "Faisalabad", "Multan", "Peshawar", "Quetta",
           "Hyderabad", "Gujranwala", "Sialkot", "Sargodha",
           "Karachi", "Lahore", "Rawalpindi", "Lahore"),
  stringsAsFactors = FALSE
)

# Function to calculate distance between two points (Haversine formula)
calculate_distance <- function(lat1, lng1, lat2, lng2) {
  R <- 6371  # Earth's radius in kilometers
  
  lat1_rad <- lat1 * pi / 180
  lng1_rad <- lng1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lng2_rad <- lng2 * pi / 180
  
  dlat <- lat2_rad - lat1_rad
  dlng <- lng2_rad - lng1_rad
  
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlng/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  distance <- R * c
  return(round(distance, 2))
}

ui <- page_sidebar(
  title = "Hospital Finder",
  sidebar = sidebar(
    selectInput(
      "selected_site",
      "Select Site:",
      choices = setNames(site_location$Site, site_location$Site),
      selected = site_location$Site[1]
    ),
    numericInput(
      "max_hospitals",
      "Number of nearest hospitals to show:",
      value = 5,
      min = 1,
      max = nrow(hospitals_data),
      step = 1
    ),
    numericInput(
      "max_distance",
      "Maximum distance (km):",
      value = 10,
      min = 1,
      max = 50,
      step = 1
    )
  ),
  
  card(
    card_header("Interactive Map"),
    leafletOutput("map", height = "500px")
  ),
  
  card(
    card_header("Nearest Hospitals Details"),
    DTOutput("hospitals_table")
  ),
  
  card(
    card_header("Summary Statistics"),
    verbatimTextOutput("summary_stats")
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to get selected site coordinates
  selected_site_coords <- reactive({
    site_row <- site_location[site_location$Site == input$selected_site, ]
    return(list(lat = site_row$Lat, lng = site_row$Long))
  })
  
  # Reactive expression to calculate distances and filter hospitals
  nearest_hospitals <- reactive({
    site_coords <- selected_site_coords()
    
    # Filter hospitals by type
    #filtered_hospitals <- hospitals_data[hospitals_data$type %in% input$hospital_types, ]
    filtered_hospitals<-hospitals_data
    # Calculate distances
    filtered_hospitals$distance_km <- mapply(
      calculate_distance,
      site_coords$lat, site_coords$lng,
      filtered_hospitals$lat, filtered_hospitals$lng
    )
    
    # Filter by maximum distance
    filtered_hospitals <- filtered_hospitals[filtered_hospitals$distance_km <= input$max_distance, ]
    
    # Sort by distance and limit to requested number
    filtered_hospitals <- filtered_hospitals[order(filtered_hospitals$distance_km), ]
    filtered_hospitals <- head(filtered_hospitals, input$max_hospitals)
    
    return(filtered_hospitals)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    site_coords <- selected_site_coords()
    hospitals <- nearest_hospitals()
    
    # Create the base map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = site_coords$lng, lat = site_coords$lat, zoom = 12)
    
    # Add selected site marker
    map <- map %>%
      addMarkers(
        lng = site_coords$lng,
        lat = site_coords$lat,
        popup = paste("<b>Selected Site:</b><br>", input$selected_site),
        icon = makeIcon(
          iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
          iconWidth = 25, iconHeight = 41,
          iconAnchorX = 12, iconAnchorY = 41
        )
      )
    
    # Add hospital markers
    if (nrow(hospitals) > 0) {
      map <- map %>%
        addMarkers(
          lng = hospitals$lng,
          lat = hospitals$lat,
          popup = paste(
            "<b>", hospitals$name, "</b><br>",
            "Distance: ", hospitals$distance_km, " km<br>"
          ),
          icon = makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
            iconWidth = 25, iconHeight = 41,
            iconAnchorX = 12, iconAnchorY = 41
          )
        )
    }
    
    map
  })
  
  # Render the hospitals table
  output$hospitals_table <- renderDT({
    hospitals <- nearest_hospitals()
    
    if (nrow(hospitals) > 0) {
      # Select and rename columns for display
      display_data <- hospitals %>%
        select(
          Hospital = name,
          `Distance (km)` = distance_km
        )
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = c("Distance (km)"), digits = 2)
    } else {
      datatable(
        data.frame(Message = "No hospitals found within the specified criteria"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Render summary statistics
  output$summary_stats <- renderText({
    hospitals <- nearest_hospitals()
    
    if (nrow(hospitals) > 0) {
      nearest_distance <- min(hospitals$distance_km)
      avg_distance <- mean(hospitals$distance_km)
      max_distance <- max(hospitals$distance_km)
      
      paste(
        "Selected Site:", input$selected_site, "\n",
        "Number of hospitals found:", nrow(hospitals), "\n",
        "Nearest hospital distance:", nearest_distance, "km\n",
        "Average distance:", round(avg_distance, 2), "km\n",
        "Furthest hospital distance:", max_distance, "km\n"
      )
    } else {
      paste("No hospitals found within the specified criteria for", input$selected_site)
    }
  })
}

shinyApp(ui = ui, server = server)


