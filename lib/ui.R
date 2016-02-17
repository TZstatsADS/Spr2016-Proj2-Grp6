suppressMessages(library(shiny))
suppressMessages(library(rCharts))
suppressMessages(library(doSNOW))
suppressMessages(library(foreach))
load(file = "./data/community.RDA")

googleAnalytics <- function(account="UA-53239073-3"){
  HTML(paste("<script type=\"text/javascript\">
             
             var _gaq = _gaq || [];
             _gaq.push(['_setAccount', '",account,"']);
             _gaq.push(['_setDomainName', 'miningchi2.shinyapps.io']);
             _gaq.push(['_trackPageview']);
             
             (function() {
             var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
             ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
             var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
             })();
             
             </script>", sep=""))
}

shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("MiningChi - Chicago Crime Data Visualization"),
  
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
      selectInput("crimetype", "Choose Crime Type:", choice = c("HOMICIDE","VIOLENCE","PROPERTYCRIME","THEFT","CRIM SEXUAL ASSAULT","BURGLARY","BATTERY","ROBBERY",
                                            "INTERFERENCE WITH PUBLIC OFFICER","DECEPTIVE PRACTICE","ARSON","CRIMINAL DAMAGE",
                                            "ASSAULT","NARCOTICS","CRIMINAL TRESPASS","OTHER OFFENSE","PUBLIC PEACE VIOLATION",
                                            "SEX OFFENSE","OFFENSE INVOLVING CHILDREN","PROSTITUTION","WEAPONS VIOLATION","KIDNAPPING",
                                            "LIQUOR LAW VIOLATION","STALKING","NON-CRIMINAL","INTIMIDATION","OBSCENITY",
                                            "PUBLIC INDECENCY","OTHER NARCOTIC VIOLATION","GAMBLING","OTHER OFFENSE ","NON - CRIMINAL",
                                            "NON-CRIMINAL (SUBJECT SPECIFIED)","INTERFERE WITH PUBLIC OFFICER","OFFENSES INVOLVING CHILDREN","RITUALISM")),
      helpText("Examples: BATTERY, THEFT etc."),
      
      dateInput("startdate", "Start Date of Data Collection:", value = "2000-01-01", format = "mm-dd-yyyy",
                min = "2000-01-01", max = "2014-09-29"),
      
      dateInput("enddate", "End Date of Data Collection:", value = "2015-01-02", format = "mm-dd-yyyy",
                min = "startdate", max = "2014-09-30"),
      ##Need some validation that enddate is after start date
      helpText("MM-DD-YEAR as Date Format")),
    
    wellPanel(
      selectInput('community', 'Community Area', community$Community.Area, selected = "Chicago-All",selectize=TRUE),
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
    ), 
googleAnalytics()  
  )

))
