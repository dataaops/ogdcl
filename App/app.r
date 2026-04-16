# app.R - Complete Employee Management System (Fixed Map Filter)
# Combines: (1) Rotation Status + (2) Location Map

library(shiny)
library(DT)
library(leaflet)
library(dplyr)
library(lubridate)
library(sf)
library(readr)

# ============================================================
# DATA LOADING
# ============================================================

# Read attendance/rotation data
read_attendance_data <- function() {
  if (!file.exists("data/attendance.csv")) {
    stop("attendance.csv file not found!")
  }
  
  data <- read.csv("data/attendance.csv", stringsAsFactors = FALSE)
  
  if (nrow(data) == 0) {
    stop("attendance.csv is empty!")
  }
  
  data$in_date <- as.Date(data$in_date)
  return(data)
}

# Read location data
read_location_data <- function() {
  employees <- read.csv("data/employees.csv", stringsAsFactors = FALSE)
  city_coords <- read.csv("data/city_coords.csv", stringsAsFactors = FALSE)
  
  # Merge coordinates
  employees$lat <- city_coords$lat[match(employees$city, city_coords$city)]
  employees$lng <- city_coords$lng[match(employees$city, city_coords$city)]
  employees <- employees[!is.na(employees$lat), ]
  
  return(employees)
}

# Calculate rotation status
calculate_status <- function(data, current_date = Sys.Date()) {
  data %>%
    mutate(
      days_since_start = as.numeric(difftime(current_date, in_date, units = "days")),
      cycle_position = ifelse(days_since_start >= 0, days_since_start %% 42, 0),
      status = case_when(
        days_since_start < 0 ~ "OFF DUTY",
        cycle_position < 21 ~ "ON DUTY",
        TRUE ~ "OFF DUTY"
      ),
      days_remaining = case_when(
        status == "ON DUTY" & days_since_start >= 0 ~ 21 - cycle_position,
        status == "OFF DUTY" & days_since_start >= 0 ~ 42 - cycle_position,
        days_since_start < 0 ~ as.numeric(difftime(in_date, current_date, units = "days")),
        TRUE ~ 0
      ),
      next_change = case_when(
        status == "ON DUTY" & days_since_start >= 0 ~ current_date + days_remaining,
        status == "OFF DUTY" & days_since_start >= 0 ~ current_date + days_remaining,
        days_since_start < 0 ~ in_date,
        TRUE ~ current_date
      ),
      status_display = ifelse(status == "ON DUTY", "🟢 ON DUTY", "🔴 OFF DUTY")
    )
}

# Add province information to location data
add_province_info <- function(employees) {
  provinces <- st_read("data/pak_admin_boundaries.shp/pak_admin1.shp")
  
  employees_sf <- st_as_sf(employees, coords = c("lng", "lat"), crs = 4326)
  employees_sf <- st_join(employees_sf, provinces)
  employees_sf$province <- employees_sf$adm1_name
  
  coords <- st_coordinates(employees_sf)
  employees_sf$lng <- coords[,1]
  employees_sf$lat <- coords[,2]
  
  return(as.data.frame(employees_sf))
}

# ============================================================
# LOAD ALL DATA
# ============================================================

# Load attendance data
attendance_data <- read_attendance_data()
attendance_teams <- unique(attendance_data$team)

# Load location data
location_data <- read_location_data()
location_data <- add_province_info(location_data)

# Get unique values for filters
location_sites <- unique(location_data$site)
location_provinces <- unique(location_data$province)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tab-content { padding: 20px 0; }
    "))
  ),
  
  titlePanel("📊 My App"),
  
  tabsetPanel(
    # ============================================================
    # TAB 1: ATTENDANCE / ROTATION STATUS
    # ============================================================
    tabPanel(
      "📅 Rotation Status",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Today's Date"),
          h3(textOutput("current_date")),
          br(),
          selectInput("team_filter", "Filter by Team",
                      choices = c("All Teams", attendance_teams),
                      selected = "All Teams"),
          br(),
          p("📌 Rotation Rule:"),
          p("21 days ON → 21 days OFF → Repeat"),
          hr(),
          p("🟢 = On Duty"),
          p("🔴 = Off Duty"),
          hr(),
          p(em(paste("Total Employees:", nrow(attendance_data)))),
          p(em(paste("Last updated:", format(Sys.Date(), "%Y-%m-%d"))))
        ),
        
        mainPanel(
          width = 9,
          h3("Current Status"),
          DTOutput("status_table"),
          br(),
          h4("📋 Upcoming Changes (Next 10)"),
          DTOutput("change_table")
        )
      )
    ),
    
    # ============================================================
    # TAB 2: EMPLOYEE LOCATION MAP
    # ============================================================
    tabPanel(
      "📍 Location Map",
      br(),
      fluidRow(
        column(3,
               selectInput("site_filter", "Select Site:",
                           choices = c("All Sites", unique(location_sites)),
                           selected = "All Sites")
        ),
        column(3,
               selectInput("province_filter", "Select Province:",
                           choices = c("All Provinces", unique(location_provinces)),
                           selected = "All Provinces")
        ),
        column(3,
               br(),
               textOutput("location_count")
        ),
        column(3,
               br(),
               actionButton("reset_filters", "Reset Filters", class = "btn-sm")
        )
      ),
      br(),
      leafletOutput("location_map", height = "70vh")
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ============================================================
  # ATTENDANCE TAB SERVER
  # ============================================================
  
  # Auto-refresh every minute
  autoInvalidate <- reactiveTimer(60000)
  
  # Current date
  output$current_date <- renderText({
    autoInvalidate()
    format(Sys.Date(), "%B %d, %Y")
  })
  
  # Status data
  status_data <- reactive({
    current_status <- calculate_status(attendance_data)
    
    if (input$team_filter != "All Teams") {
      current_status <- current_status[current_status$team == input$team_filter, ]
    }
    
    current_status
  })
  
  # Status table
  output$status_table <- renderDT({
    data <- status_data()
    
    display_data <- data[, c("name", "team", "pair", "role", "status_display", 
                             "days_remaining", "next_change", "in_date")]
    colnames(display_data) <- c("Name", "Team", "Pair", "Role", "Status", 
                                "Days Remaining", "Next Change", "Start Date")
    
    datatable(display_data,
              options = list(
                pageLength = 50,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE
              ),
              rownames = FALSE,
              filter = 'top') %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("🟢 ON DUTY", "🔴 OFF DUTY"),
                    c("#d4edda", "#f8d7da")
                  ),
                  fontWeight = 'bold')
  })
  
  # Upcoming changes table
  output$change_table <- renderDT({
    data <- status_data()
    
    upcoming <- data %>%
      arrange(next_change) %>%
      select(name, team, pair, status_display, next_change, days_remaining) %>%
      head(10)
    
    colnames(upcoming) <- c("Name", "Team", "Pair", "Current Status", "Next Change", "Days Left")
    
    datatable(upcoming,
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE) %>%
      formatStyle("Current Status",
                  backgroundColor = styleEqual(
                    c("🟢 ON DUTY", "🔴 OFF DUTY"),
                    c("#d4edda", "#f8d7da")
                  ))
  })
  
  # ============================================================
  # LOCATION MAP TAB SERVER (FIXED)
  # ============================================================
  
  # Create base map once
  output$location_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 70, lat = 30, zoom = 6)
  })
  
  # Filter location data based on selections
  filtered_location <- reactive({
    data <- location_data
    
    # Apply site filter
    if (!is.null(input$site_filter) && input$site_filter != "All Sites") {
      data <- data %>% filter(site == input$site_filter)
    }
    
    # Apply province filter
    if (!is.null(input$province_filter) && input$province_filter != "All Provinces") {
      data <- data %>% filter(province == input$province_filter)
    }
    
    return(data)
  })
  
  # Update province choices when site changes
  observeEvent(input$site_filter, {
    if (input$site_filter == "All Sites") {
      province_choices <- c("All Provinces", unique(location_data$province))
    } else {
      provinces_in_site <- unique(location_data$province[location_data$site == input$site_filter])
      # Remove NA if any
      provinces_in_site <- provinces_in_site[!is.na(provinces_in_site)]
      province_choices <- c("All Provinces", provinces_in_site)
    }
    
    updateSelectInput(session, "province_filter", choices = province_choices, selected = "All Provinces")
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "site_filter", selected = "All Sites")
    updateSelectInput(session, "province_filter", selected = "All Provinces")
  })
  
  # Employee count display
  output$location_count <- renderText({
    data <- filtered_location()
    paste("📍 Employees:", nrow(data))
  })
  
  # Update map markers when filters change
  observeEvent(list(input$site_filter, input$province_filter), {
    data <- filtered_location()
    
    # Clear all existing markers and add new ones
    leafletProxy("location_map") %>%
      clearMarkerClusters() %>%   # <-- IMPORTANT FIX
      clearMarkers() %>%          # optional but safe
      addCircleMarkers(
        data = data,
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        color = "blue",
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>", name, "</b><br>",
          "Code:", code, "<br>",
          "City:", city, "<br>",
          "Site:", site, "<br>",
          "Province:", province
        ),
        label = ~name,
      )
  })
}

# ============================================================
# RUN THE APP
# ============================================================

shinyApp(ui = ui, server = server)