
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
# @Author: Feixiang Li
# @Copyright: Monash University
#

library(shiny)
library(leaflet)
library(plotly)

# >>>>>@load data
pop_staticsData <- read.csv("../data/data4.csv")
topBirthCountry_str <- unique(pop_staticsData$Top.country.of.birth__1)
topBirthCountry_val <- tolower(topBirthCountry_str)
ageGroup <- c(
  "Children" = "children",
  "Adult" = "adult",
  "Elderly" = "elderly"
)
topBirthCountry = setNames(topBirthCountry_val, topBirthCountry_str)

shinyUI(
  navbarPage("VIC Property", id="nav",

    # ========tab panel explore suburb property trend========                          
    tabPanel("explore suburb property trend",
        div(class="outer",
            tags$head(
              # Include our custom CSS
              includeCSS("styles.css"),
              includeScript("gomap.js")
            ),
            
            # =======the leaflet map=========
            leafletOutput("map", width="100%", height="100%"),
            
            # =======floating panel==========
            # the interactive panel for inputting the condition to filter data
            # and show the relating information by click the area at the map
            # Shiny versions prior to 0.11 should use class="modal" instead.
            absolutePanel(id = "controls", class = "panel panel-default", 
                          fixed = TRUE, draggable = TRUE, 
                          top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 760, height = "auto",
                          
                          h2("explorer"),
                          
                          # >>>>>>@ selectinput to filter the top birth country
                          selectInput("si_tbc", "top birth country", topBirthCountry),
                          # >>>>>>@ check the select input top birth country
                          h3(textOutput("caption_birth_country")),
                          # >>>>>>@ select to show the map legend, or not.
                          checkboxInput("legend", "Show legend", TRUE),
                          # >>>>>>@ output the line-trend of property by clicking the suburb at map
                          # plotlyOutput("lineTrendPlot"),
                          # plotlyOutput("ageGroupPieChart")
                          htmltools::div(style = "display:inline-block", plotlyOutput("lineTrendPlot", height = "260px")),
                          htmltools::div(style = "display:inline-block", plotlyOutput("ageGroupPieChart", width="50%"))
            ),
            
            # ===========bottom tag to show the copyright information===============
            tags$div(id="cite",
                     'Map show for ', 
                     tags$em('Coming Apart: The population distribution of Victoria state in Australia'), ' by Feixiang Li (2017).'
                     )
        )
     ),
    
    #=======tab panel explore census and property======
    tabPanel("explore census and property",
             div(class="outer",
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css"),
                   includeScript("gomap.js")
                 ),
                 # leaflet output the map of census distribution
                 leafletOutput("Map_Census", width="100%", height="100%"),
                 
                 #filter panel
                 absolutePanel(id = "controls", class = "panel panel-default",
                               fixed = TRUE, draggable = TRUE,
                               top = 60, left = "auto", right = 20, bottom = "auto",
                               width = 390, height = "auto",

                 h4("Filter"),
                 #sliders of percentage of children, adult and elderly
                 sliderInput("children", "The percentage of children in each suburb",
                             0, 30, value = c(10, 30)),
                 sliderInput("adult", "The percentage of adult in each suburb", 0, 100, value = c(60, 100)),
                 sliderInput("elderly", "The percentage of elderly in each suburb",
                             0, 60, value = c(30, 60))
                 )
                 )


    )
))
