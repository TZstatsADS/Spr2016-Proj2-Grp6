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

DayofWeek <- c(
  "All" = "",
  "Monday", "Tuesday", "Wednesday", "Thursday", 
  "Friday", "Saturday", "Sunday"
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

shinyUI(navbarPage("NYC Public Toilets Map", id="nav",
                   
                   tabPanel("Interactive map",
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
                                              
                                              h2("Public Toilets"),
                                              
                                              helpText("Please select toilet types (leave them blank if no specification)"), 
                                              checkboxInput("handicap", "Handicap Accessible"),
                                              checkboxInput("yearround", "Open Year Round"),
                                              helpText("Check to show crime data"),
                                              checkboxInput("addcrime", "Add Crime Data"),
                                              selectInput("crime", "Crime Type", CrimeType),
                                              selectInput("month", "Month", Month),
                                              selectInput("day", "Day", Day),
                                              selectInput("hour", "Hour", Hour)
                                              #radioButtons("vehicle", "Show Just One Vehicle", vars3, selected = '')
                                )
                             )
                                
                                
          
                   ),
                   
                   tabPanel("Most Dangerous Intersections",
                            h2("TOP 10 Intersections With The Most Accidents"),
                            helpText("Click ACTION BUTTON to view the intersection on the map"),
                            helpText("Choose ALL VEHICLES in interactive map to ensure the right popup info"),
                            hr(),
                            DT::dataTableOutput("toptable")
                   ),   
                   
                   tabPanel("About",
                            h4("User Manual: ", a("Click Here", href=
                                    "http://nbviewer.ipython.org/github/funjo/NYPD_accidents_shiny/blob/master/User%20Manual.pdf")),
                            br(),
                            h4("Data Source"),
                            p("Source: ",a("NYPD Motor Vehicle Collisions | NYC Open Data.",href=
                                    "https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95")),
                            p("Description: ","Data Details of Motor Vehicle Collisions in 
                              New York City provided by the Police Department (NYPD)."),
                            p("Usage: ","Original dataset was downloaded on 07/07/2015, 
                              containing 618,358 accident records from 07/01/2012 to 07/05/2015. 
                              Because of the loading speed concern, this app uses only 10,000 random records 
                              from the original dataset."),
                            br(),
                            h4("Author Information"),
                            p("Fangzhou Cheng"),
                            p("Email: fc982@nyu.edu"),
                            p("Website:", a("http://www.fangzhoucheng.com",href="http://www.fangzhoucheng.com")),
                            p("Github:", a("http://www.github.com/funjo",href="http://www.github.com/funjo")),
                            p("LinkedIn:", a("http://www.linkedin.com/in/fangzhoucheng",href="http://www.linkedin.com/in/fangzhoucheng")),
                            br(),
                            br(),
                            p("Fangzhou Cheng - Copyright @ 2015, All Rights Reserved")
                   ),
                   
                   conditionalPanel("false", icon("crosshair"))
))
