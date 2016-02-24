library(shiny)
library(leaflet)
library(DT)

# Choices for drop-downs
CrimeType <- c(
  "All Crime" = "",
  "Grand Larceny" = "GRAND LARCENY",
  "Grand Larceny of Motor Vehicle" = "GRAND LARCENY OF MOTOR VEHICLE",
  "Rape" = "RAPE",
  "Murder" = "MURDER",
  "Robbery" = "ROBBERY",
  "Burglary" = "BURGLARY",
  "Felony Assault" = "FELONY ASSAULT"
)

Month <- c(
  "All" = "",
  "January" = "Jan", "February" = "Feb", "March" = "Mar", 
  "April" = "Apr", "May" = "May", "June" = "Jun",
  "July" = "Jul", "August" = "Aug", "September" = "Sep",
  "October" = "Oct", "November" = "Nov", "December" = "Dec"
)

Hour <- c(
  "All" = "",
  "0",  "1" = "100",  "2" = "200",  "3" = "300",  "4" = "400",
  "5" = "500", "6" = "600",  "7" = "700",  "8" = "800",  "9" = "900",
  "10" = "1000", "11" = "1100", "12" = "1200", "13" = "1300",
  "14" = "1400", "15" = "1500", "16" = "1600", "17" = "1700",
  "18" = "1800", "19" = "1900", "20" = "2000", "21" = "2100",
  "22" = '2200', "23" = "2300"
)

Day <- c(
  "All" = "",
  "1",  "2",  "3",  "4",  "5", "6",
  "7",  "8",  "9",  "10", "11", "12",
  "13", "14", "15", "16", "17", "18",
  "19", "20", "21", "22", "23", "24",
  "25", "26", "27", "28", "29", "30", "31"
)

shinyUI(navbarPage("DIDI Toilets", id="nav",
                   
                   tabPanel("Toilet Map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h3("Public Toilets"),
                                              
                                              textInput("address", "Your Address", value = "350 5th Ave, New Yock, NY", width = NULL, placeholder = NULL),
                                              actionButton("submit","Mark"),
                                              helpText("Select toilet types (leave them blank if no specification)"), 
                                              checkboxInput("handicap", "Handicap Accessible"),
                                              checkboxInput("yearround", "Open Year Round"),
                                              
                                          
                                              
                                              helpText("Check to show crime data"),
                                              #actionButton("submit","Add Crime Data"),
                                              checkboxInput("addcrime", "Add Crime Data"),
                                              numericInput("circleR", label = "Customize mouseover circle radius (meters):", value = 400, min = 1),
                                              selectInput("crime", "Crime Type", CrimeType, selected = "MURDER"),
                                              selectInput("month", "Month", Month),
                                              selectInput("day", "Day", Day),
                                              selectInput("hour", "Hour", Hour),
                                              numericInput("icon", label = "Customize toilet markers: enter an integer between 1 to 18", value = 18, min = 1, max = 18)
                                ),
                                absolutePanel(id="graphstuff",class = "panel panel-default", fixed=TRUE,
                                              draggable = TRUE, top=60,left=10,right="auto", bottom="auto",width=300,
                                              height=200, style="opacity:0.85",
                                              # div(style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;"),
                                              # h2("Crime around this Restroom"),
                                              h6(textOutput("totalcrime")),
                                              plotOutput("circ_plot",height=200)
                                              
                            )
                                              
                            )
                            
                            
                            
                   ),
                   
                   tabPanel("Dynamic Map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map2", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("NYPD 7 Major Felony Incidents"),
                                              
                                              radioButtons("offense", "Show Just One Crime", CrimeType, selected = "FELONY ASSAULT"),
                                              
                                              # Simple integer interval
                                              sliderInput("minute", "Minute of Day:", 
                                                          min = time.start, max = time.end, value = 0, step = 1,
                                                          animate=animationOptions(interval = 500)),
                                              helpText("Click to see dynamic crime data")
                                )
                            )
                   ),
                   tabPanel("About Our Project", includeMarkdown("docs/About.md")),
                   conditionalPanel("false", icon("crosshair"))
))