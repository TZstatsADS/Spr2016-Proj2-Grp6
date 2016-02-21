library(shiny)
library(leaflet)
library(DT)

# Choices for drop-downs
vars <- c(
  'All Types' = 'type1',
  'Open All Year Round' = 'type2',
  'Handicap Accessible' = 'type3'
  )

vars2 <- c(
  "Injuries" = "NUMBER.OF.PERSONS.INJURED",
  "Deaths" = "NUMBER.OF.PERSONS.KILLED"
)

vars3 <- c(
  "All Toilets" = "",
  "handicap" = "Handicap",
  "yearround" = "Yearround"
)

vars4 <- c("All boroughs"="",
  'Manhattan'='MANHATTAN',
  'Brooklyn'='BROOKLYN',
  'Queens'='QUEENS','Bronx'='BRONX',
  'Staten Island'='STATEN ISLAND')

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
                                              
                                              #selectInput("type", "Type", vars),
                                              #selectInput("size", "Size", vars2, selected = "NUMBER.OF.PERSONS.INJURED"),
                                              helpText("Please select toilet types (leave them blank if no specification)"), 
                                              checkboxInput("handicap", "Handicap Accessible"),
                                              checkboxInput("yearround", "Open Year Round"),
                                              helpText("Check to show crime data"),
                                              checkboxInput("crime", "Add Crime Data")
                                              #radioButtons("vehicle", "Show Just One Vehicle", vars3, selected = '')
                                ),
                                
                                tags$div(id="cite",
                                         'Data from: ', tags$em('NYPD Motor Vehicle Collisions'), '  | NYC Open Data. 
                                         Details of Motor Vehicle Collisions in New York City provided by the 
                                         Police Department (NYPD).'
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
                   
                   tabPanel("See Your Neighbourhood",
                            fluidRow(
                              column(3,
                                     selectInput("boroughs", "Boroughs", vars4, multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.boroughs",
                                                      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            helpText("Click ACTION BUTTON to view the intersection on the map"),
                            helpText("Choose ALL VEHICLES in interactive map to ensure the right popup info"),
                            hr(),
                            DT::dataTableOutput("vctable")
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
