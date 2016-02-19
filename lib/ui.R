suppressMessages(library(shiny))
suppressMessages(library(rCharts))
suppressMessages(library(doSNOW))
suppressMessages(library(foreach))
# load(file = "./data/cleandata.RDS")
a <- readRDS("cleandata.RDS")
b <- as.character(levels(a$`Complaint Type`))


# googleAnalytics <- function(account="UA-53239073-3"){
#   HTML(paste("<script type=\"text/javascript\">
#              
#              var _gaq = _gaq || [];
#              _gaq.push(['_setAccount', '",account,"']);
#              _gaq.push(['_setDomainName', 'miningchi2.shinyapps.io']);
#              _gaq.push(['_trackPageview']);
#              
#              (function() {
#              var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
#              ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
#              var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
#              })();
#              
#              </script>", sep=""))
# }

shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("New York City Complaint Data Visualization"),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sidebar Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sidebarPanel(
    
    wellPanel(
      helpText(HTML("<b>READY?</b>")),
      HTML("Scroll down to modify the settings. Click this when you are ready to render new plots."),
      submitButton("Update Graphs and Tables")
    ),
    
    wellPanel(
      helpText(HTML("<b>BASIC SETTINGS</b>")),
      selectInput("comptype", "Choose Complaint Type:", choice = b),
      helpText("Examples: Trans Fat, Unlicesed Dog etc."),
      
      dateInput("startdate", "Start Date of Data Collection:", value = "2010-01-01", format = "mm-dd-yyyy",
                min = "2010-01-01", max = "2016-02-16"),
      
      dateInput("enddate", "End Date of Data Collection:", value = "2016-02-16", format = "mm-dd-yyyy",
                min = "startdate", max = "2016-02-16"),
      ##Need some validation that enddate is after start date
      helpText("MM-DD-YEAR as Date Format")),
    
    wellPanel(
      selectInput('borough', 'Borough', c(levels(a$Borough),"NYC-ALL"), selected = "NYC-ALL",selectize=TRUE),
      helpText("Applies to Crime Map, Analysis, and Weather sections")),
    
    wellPanel(
      selectInput("period", "Choose period for analysis:", choice = c("Monthly","Weekly","Daily","Yearly")),
      helpText("Applies to Analysis and Weather sections"))
        
    
   ),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#just need to find the right HTML formatting

  mainPanel(
    tabsetPanel(
      tabPanel("Introduction", includeMarkdown("docs/introduction.md")),
      tabPanel("Basic Stats",showOutput("analysis","highcharts")),
      tabPanel("Crime Map", uiOutput("mapcenter"), div(class="span6",uiOutput("mapzoom")),
               div(class="span8", plotOutput("map",height=600,width=600)),div(class="span4",uiOutput("maptype")),div(class="span2",uiOutput("mapres")),
               div(class="span2",uiOutput("mapbw"))),
      tabPanel("Crime Heat Map", uiOutput("hmapcenter"), div(class="span6",uiOutput("hmapzoom")),
               div(class="span8", plotOutput("heatmap",height=600,width=600)),div(class="span4",uiOutput("hmaptype")),div(class="span2",uiOutput("hmapres")),
               div(class="span2",uiOutput("hmapbw")), div(class="span2",uiOutput("halpharange")), div(class="span2",uiOutput("hbins")),
               div(class="span2",uiOutput("hboundwidth")), div(class="span2",uiOutput("hboundcolor")), div(class="span2",uiOutput("hlow")),
               div(class="span2",uiOutput("hhigh"))
               ),
#       tabPanel("Traffic", uiOutput("tmaptitle"),uiOutput("tmapcenter"), div(class="span6",uiOutput("tmapzoom")),
#                div(class="span8", plotOutput("tmap",height=600,width=600)),div(class="span4",uiOutput("tmaptype")),div(class="span2",uiOutput("tmapres")),
#                div(class="span2",uiOutput("tmapbw"))),
      tabPanel("Time Series",uiOutput("decomintro"), plotOutput("decomplot"),plotOutput("trends")), #textOutput("bfastmonitor")
      tabPanel("Weather", plotOutput("weather")),
      tabPanel("Data", dataTableOutput("datatable")),
      tabPanel("Credits", includeMarkdown("docs/credits.md"))
    )#, 
# googleAnalytics()  
  )

))
