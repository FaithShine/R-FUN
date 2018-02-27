
# This is the server logic for a Shiny web application.
# @Author: Feixiang Li
# @Copyright: Monash MIT
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(rgdal) #polygon map
library(geojsonio)
library(rmapshaper)
library(dplyr)
library(plotly)

print("success")
shpData <- readOGR(dsn="../SDM370702/ll_gda94/sde_shape/whole/VIC/VMADMIN/layer", layer="postcode_polygon")
proj4string(shpData) 
vic_suburbs <- geojsonio::geojson_read("../aus_ste.geojson",
                                       what = "sp")
#load data
pop_staticsData <- read.csv("../data/data4.csv")
crime_data <- read.csv("../data/crime_data.csv")
property_data <- read.csv("../data/property_data2.csv")
pop_densityData <- read.csv("../data/pop_density.csv")


shinyServer(function(input, output) {

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 144.989807, lat = -37.836873, zoom = 10)
  })
  
  
  # hint the select top birth country information
  output$caption_birth_country <- renderText({ 
    paste("You have selected this", input$si_tbc)
  })
  
  filterdata <- reactive({
  byBirthCountry <- input$si_tbc
  # fliter data by select input birth country
  filter_data1 <- pop_staticsData[tolower(pop_staticsData$Top.country.of.birth)==byBirthCountry,]
  # clean the duplicated data
  filter_data2 <- filter_data1[!duplicated(filter_data1[, c("POSTCODE")]),]
  
  # merge data from crime data, propterty data and population density data
  filter_data3 <- merge(x=filter_data2, y=crime_data, by="POSTCODE", all.x=TRUE)
  # print(nrow(filter_data3))
  filter_data3 <- merge(x=filter_data3, y=pop_densityData, by="Suburb", all.x=TRUE)
  # print(nrow(filter_data3))
  filter_data3 <- merge(x=filter_data3, y=property_data, by="Suburb", all.x=TRUE)
  # print(nrow(filter_data3))
  
  # clean the duplicated data
  filter_data2 <- filter_data3[!duplicated(filter_data3[, c("POSTCODE")]),]
  # print(nrow(filter_data2))
  # merge data into suburb polygon data
  data_byBirthCountry <- merge(x=shpData, y=filter_data2, by="POSTCODE", all.x=TRUE, duplicateGeoms=TRUE)
  
  # clean the NULL data
  data_byBirthCountry <- data_byBirthCountry[!is.na(data_byBirthCountry$Suburb), ]
  # data_byBirthCountry[is.na(data_byBirthCountry)] <- 0
  
  # ====germany na problems======
  # write.csv(data_byBirthCountry, file = "../data/fliterdata.csv")
  # print(nrow(data_byBirthCountry[is.na(data_byBirthCountry$Suburb), ]))
  })  

  observe({
    data_byBirthCountry <- filterdata()
    # print(names(filterdata()))
    # render map by leafletProxy
    colorData <- data_byBirthCountry$Population.density.2015
    # print(colorData)
    pal <- colorBin("viridis", data_byBirthCountry$Population.density.2015, 7, pretty = FALSE)
    
    leafletProxy("map", data = data_byBirthCountry) %>%
      clearShapes() %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6, 
                  fillColor=pal(colorData), layerId=~POSTCODE) %>%
                  # fillColor = "lightblue") %>%
                  # label = ~paste0(POSTCODE, ": ", Suburb)) %>%
      addLegend("bottomleft", pal = pal, values=~log10(colorData), opacity = 0.6, layerId="colorLegend")

  })

  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   leafletProxy("map") %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomleft",
  #                         pal = pal, values = ~mag)
  #   }
  # })
    
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
    
  # Show a popup at the given location
  showZipcodePopup <- function(postcode, lat, lng) {
    crimes <- crime_data[crime_data$POSTCODE==postcode, ]
    popStatics <- pop_staticsData[pop_staticsData$POSTCODE==postcode, ]
    
    # click to tag-show the suburb information
    content <- as.character(tagList(
      tags$h4("PostCode:", popStatics$POSTCODE),
      tags$h5("Suburb:", popStatics$Suburb)
      # tags$strong(HTML(sprintf("%s, %s %s",
                               # popStatics$price, property$state.x, property$zipcode
      # ))), tags$br(),
      # sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      # sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = postcode)
    
    # line trend of the property
    # print("======")
    # print(popStatics$Suburb)
    # print(popStatics)
    property <- property_data[property_data$Suburb==toupper(popStatics$Suburb),]
    # print(property)
    output$lineTrendPlot<- renderPlotly({
      p1 <- plot_ly() %>%
        add_lines(data=property, x = ~year, y = ~as.numeric(as.character(property$price)), color=~Suburb) %>%
        add_lines(data=crimes, x=~year, y= ~as.numeric(gsub('\\,', '', as.character(crimes$occurence))), yaxis ="y2", name="crime info") %>%
        layout(
          yaxis=list(
            title="property price"
          ),
          yaxis2 = list(
            side="right",
            overlaying="y",
            title="crime occurence"
          )
        )
      # p2 <- popStatics %>%
      #   group_by(AgeGroup) %>%
      #   plot_ly(labels=~AgeGroup, values= ~AG_percent, textinfo="label+percent") %>%
      #   add_pie(hole=0.5)
      # subplot(p1, p2, nrows = 2, heights = c(0.7, 0.3))
      p1
    })

     output$ageGroupPieChart <- renderPlotly({
      p2 <- popStatics %>%
        group_by(AgeGroup) %>%
        plot_ly(labels=~AgeGroup, values= ~AG_percent) %>%
        add_pie(hole=0.5)
      p2
    })
  }#end of show postcode popup  
  
  
  #filter data by sliders input
  agegroup_census <- reactive({
    #get sliders input
    Minpercent_child <- input$children[1]
    Minpercent_adult <- input$adult[1]
    Minpercent_elderly <- input$elderly[1]
    Maxpercent_child <- input$children[2]
    Maxpercent_adult <- input$adult[2]
    Maxpercent_elderly <- input$elderly[2]
    
    #get census dataframe from selection of specific columns in pop_staticsData
    census <- pop_staticsData[, c("POSTCODE", "Suburb", "AgeGroup", "AG_percent")]
    #fliter data
    census_child <- census %>%
      filter(
        AgeGroup == "Children",
        AG_percent >= Minpercent_child,
        AG_percent <= Maxpercent_child
      ) %>%
      arrange(POSTCODE)
    census_adult <- census %>%
      filter(
        AgeGroup == "Adult",
        AG_percent >= Minpercent_adult,
        AG_percent <= Maxpercent_adult
      ) %>%
      arrange(POSTCODE)
    census_elderly <- census %>%
      filter(
        AgeGroup == "Elderly",
        AG_percent >= Minpercent_elderly,
        AG_percent <= Maxpercent_elderly
      ) %>%
      arrange(POSTCODE)
    #delete the duplicated records
    census_child <- distinct(census_child)
    census_adult <- distinct(census_adult)
    census_elderly <- distinct(census_elderly)
    
    #filtered data
    census_ag <- rbind(census_child, census_adult, census_elderly)
    # census_ag <- distinct(census_ag)
    # print("=====census======")
    # census_ag <- Reduce(function(x, y) merge(x, y, all=TRUE), list(census_child, census_adult, census_elderly))
    # print(names(census))
    # print(nrow(census_ag))
    # print(nrow(distinct(census_ag)))
    return(census_ag)
  })
  
  # census distribution map
  output$Map_Census <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 144.989807, lat = -37.836873, zoom = 10)
  })
  
  # interactive with census distribution map
  observe({
    #get flitered data
    ag_census <- agegroup_census()
    #get property data in 2015
    property2015 <- property_data %>%
      filter(
        year == "2015"
      ) %>%
      arrange(Suburb)
    #merge census data and property data
    temp_df <- merge(ag_census, property2015, by.x="Suburb", all.x=TRUE)
    data4map <- merge(x=shpData, y=temp_df, by="POSTCODE", all.x=TRUE, duplicateGeoms=TRUE)
    #data used for map without NA 
    data4map <- data4map[!is.na(data4map$price),]
    
    colorData <- as.numeric(as.character(data4map$price))
    pal <- colorBin("viridis", as.numeric(as.character(data4map$price)), 7, pretty = FALSE)
    
    #repolygon leaflet map
    leafletProxy("Map_Census", data = data4map) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.6,
                  fillColor=pal(colorData), layerId=~POSTCODE,
                  label = ~paste0(POSTCODE, ": ", Suburb)) %>%
      # fillColor = "lightblue") %>%
      # label = ~paste0(POSTCODE, ": ", Suburb)) %>%
      addLegend("bottomleft", pal = pal, values=~log10(colorData), opacity = 0.6, layerId="colorLegend")
  })
})


