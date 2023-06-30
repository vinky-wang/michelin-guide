########################################
# Michelin Restaurant Guide            #
# Vinky Wang                           #
# ui.R file                            #
########################################



# Load packages and data -------------------------------------------------------
# Load packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)


# Load data
restaurants <- read.csv("https://raw.githubusercontent.com/ngshiheng/michelin-my-maps/main/data/michelin_my_maps.csv")



# Data preprocessing  ----------------------------------------------------------
restaurants <- restaurants %>%
  separate(Location, into = c("City", "Country_Region"), sep=", ", fill="right") %>%
  mutate(Country_Region = case_when(City == "Abu Dhabi" ~ "United Arab Emirates",
                                    City == "Dubai" ~ "United Arab Emirates",
                                    City == "Hong Kong" ~ "Hong Kong",
                                    City == "Luxembourg" ~ "Luxembourg",
                                    City == "Macau" ~ "Macau",
                                    City == "Singapore" ~ "Singapore",
                                    TRUE~Country_Region
                                    ),
         Price = ifelse(nchar(Price) == 0, "Other", strrep("$", nchar(Price))))



# Building the Shiny app -------------------------------------------------------
# UI
ui <- navbarPage(  
  
  # Title
  title = "",                                          
  
  # Home tab
  tabPanel("Home",
           icon = icon("house"),
           fluidRow(tags$head(tags$style(HTML(".navbar-static-top               
                                              {background-color:#BD2333}",
                                              ".navbar-default .navbar-nav > li > a { color: white;}
                                              ul {
                                              list-style-image: url(logo.png);
                                              }"
                    ))),
                    tags$h1("The Michelin Restaurant Guide"),
                    tags$br(),
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "logo.png",
                      ))
                    ,
                    tags$br(),
                    tags$h2("Welcome!"),
                    tags$p(tags$em("Michelin Restaurant Guide"), 
                           "is an app that lets you explore, search, and visualize restaurants recognized by the Michelin Guides."),
                    tags$br(),
                    tags$p("Navigate to the"),
                    tags$ul(
                      tags$li("Restaurant Map to visualize Michelin awarded restaurants across the world"),
                      tags$li("Restaurant Finder to search for restaurants matching your selection criteria and download the results as a .csv file"),
                      tags$li("Restaurant Look Up to find more information about a restaurant and download as a .csv file")
                    ),
                    tags$br(),
                    tags$h2("Links"),
                    tags$p("The dataset is scraped from the", tags$a("Michelin Guide", href = "https://guide.michelin.com/en/restaurants"),
                           "by", tags$a("Jerry Ng", href = "https://jerrynsh.com/author/jerry/"),
                           "which can be found", tags$a("here", href="https://github.com/ngshiheng/michelin-my-maps")), 
                    tags$p("The code for this Shiny app can be found",
                           tags$a("here", href="https://github.com/vinky-wang/michelin-guide"))
           )),
  
  
  
  
  
  
  
  # Restaurant Map tab
  tabPanel("Restaurant Map", icon = icon("map"),                                # restaurant map output
           div(
             id = "map_container",
             leafletOutput(height = "600px", "map"),
             absolutePanel(
               top = 20,
               right = 20,
               style = "color: #FFF",
               h1("", style = "color:white"),
             )
           ))
  ,               
  
  
  
  # Restaurant Finder tab               
  tabPanel("Restaurant Finder", icon = icon("magnifying-glass"),
           fluidRow(fluidPage(downloadButton("download_restaurant_finder",      # download filtered restaurant table 
                                             "Download"),
                              sidebarLayout(
                                sidebarPanel(                                   
                                  checkboxGroupButtons("award_select",          # checkbox group for award         
                                                       "Award", 
                                                       choices = sort(unique(restaurants$Award))),
                                  
                                  selectizeInput(                               # text input for cuisine
                                    inputId = "cuisine_search",
                                    label = "Search Cuisine",
                                    multiple = TRUE,
                                    choices = sort(unique(restaurants$Cuisine))
                                  ),
                                  
                                  checkboxGroupButtons("price_range",           # checkbox for price
                                                       "Price",
                                                       choices = sort(unique(restaurants$Price))), 
                                ),
                                
                                
                                mainPanel(
                                  h2(textOutput("no_of_results")),              # number of results found
                                  br(), br(),
                                  leafletOutput("city_plot"),                   # map of filtered restaurant
                                  DT::dataTableOutput("restaurant_table")       # reactive filtered restaurant table
                                )
                              ),
           )
           )
  ),
  
  # Restaurant Info tab
  tabPanel("Restaurant Look Up", icon = icon("info"),
           fluidRow(fluidPage(downloadButton("download_restaurant_info", "Download"),
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput(
                                    inputId = "restaurant_search",
                                    label = "Search Restaurant",
                                    choices = NULL, 
                                    multiple = TRUE
                                  )
                                ),
                                mainPanel(
                                  DT::dataTableOutput("info_table")
                                )
                              )
           ))
  ))








# Server
server <- function(input, output, session){

  # Map of all restaurants
  output$map <- renderLeaflet({
    leaflet(data=restaurants) %>%                                               # reactive basemap layer
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "Open Street Map", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, 
                       group = "Esri World Imagery", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, 
                       group = "Nasa Earth at Night", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(                                                               # add marker
        ~Longitude,
        ~Latitude,
        icon = makeIcon(                                                        # use Michelin logo as marker
          iconUrl = "star.png",
          iconWidth = 30, iconHeight = 30
        ),
        
        clusterOptions = markerClusterOptions(),                                # add marker cluster
        popup = ~paste(Name,                                                    # add popup info
                       "<br>Award:", Award,
                       "<br>Address:", Address,
                       "<br>Phone Number:", PhoneNumber,
                       paste('<br><a href =',                                   # hyperlink website
                             restaurants$WebsiteUrl, 
                             '> Visit Website </a>'))
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map", "Esri World Imagery", "Nasa Earth at Night"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })
  
  
  
  
  # Filter restaurants based on selection from restaurant finder tab
  restaurant_filtered <- reactive({
      restaurants %>%
      filter(Award %in% input$award_select|
               Cuisine %in% input$cuisine_search|
               Country_Region %in% input$country_region_search|
               Price %in% input$price_range)
  })
  
  
  
  # Table for filtered restaurants based on selection from restaurant finder tab
  output$restaurant_table <- DT::renderDataTable({                              # reactive table
    restaurant_filtered() %>%
      dplyr::select(Name, Address, Cuisine, Award, Price) 
  })
  

  
  # Plot of restaurants based on filtered criteria
  output$city_plot <- renderLeaflet({
    restaurant_filtered() %>%
    leaflet() %>%                                                               # reactive basemap layer
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "Open Street Map", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, 
                       group = "Esri World Imagery", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, 
                       group = "Nasa Earth at Night", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(                                                               # add marker
        ~Longitude,
        ~Latitude,
        icon = makeIcon(                                                        # use Michelin logo as marker
          iconUrl = "star.png",
          iconWidth = 30, iconHeight = 30
        ),
        
        clusterOptions = markerClusterOptions(),                                # add marker cluster
        popup = ~paste(Name,                                                    # add popup info
                       "<br>Award:", Award,
                       "<br>Address:", Address,
                       "<br>Phone Number:", PhoneNumber,
                       paste('<br><a href =',                                   # hyperlink website
                             restaurants$WebsiteUrl, 
                             '> Visit Website </a>'))
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map", "Esri World Imagery", "Nasa Earth at Night"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })
  

  
  
  
  
  # Filter restaurants based on selection from restaurant info tab
  updateSelectizeInput(session, "restaurant_search",                            # change value of select input on the client since too many restaurant names to list
                       choices = restaurants$Name, server = TRUE)
  info_filtered <- reactive({
    restaurants %>%
      filter(Name %in% input$restaurant_search)
  })
  
  
  
  # Table for filtered restaurants based on selection from info tab
  output$info_table <- DT::renderDataTable(DT::datatable({                      # reactive table with DT::datatable to hyperlink websites
    data <- info_filtered() %>%
      dplyr::select(Name, Address, PhoneNumber, WebsiteUrl, Url) %>%
      rename("Phone Number" = PhoneNumber) %>%
      mutate(Website = paste0("<a href='", WebsiteUrl, "' target='_blank'>", WebsiteUrl, "</a>"),
             "Michelin Guide Article" = paste0("<a href='", Url, "' target='_blank'>", Url, "</a>")) %>%
      select(-c("Url", "WebsiteUrl"))
    data
  },
  escape=FALSE)
  )
  
  
  
  # Download filtered restaurant data
  output$download_restaurant_finder <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(restaurant_filtered(), file)
    }
  )
  
  
  
  # Download filtered restaurant info data
  output$download_restaurant_info <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(info_filtered(), file)
    }
  )
  
  
  
  # Print number of results found matching selection from restaurant finder tab
  output$no_of_results <- renderText({
    paste("We found", print(nrow(restaurant_filtered())), "options for you")
  })
  
}





# Run Shiny app
shinyApp(ui = ui, server = server)
