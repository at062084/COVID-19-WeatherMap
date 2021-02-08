options(error = function() traceback(2))
options(shiny.reactlog = TRUE)
library(lubridate)
library(httr)

# -----------------------------------------------------------
# Define Cron, Logging and Monitoring
# -----------------------------------------------------------
logDir = "./log"
logFile <- "cwm.rshiny.log"
logMsg <- function(msg, sessionID="_global_") {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=stderr())
}

hostSystem <- system("hostname", intern=TRUE)
slackMsg <- function (title, msg, hostName = hostSystem) {
  url <- as.character(read.csv("../secrets/slack.txt",header=FALSE)[1,1])
  body <- list(text = paste(paste0(now()," *",title,"*: "), paste0(hostName,": ",msg)))
  r <- POST(url, content_type_json(), body = body, encode = "json")
  invisible(r)
}
slackMsg(title="COVID-19-WeatherMap",msg=paste("Start shiny global section in app.R"))

if(0==1) {
  logMsg("Starting service cron")
  system2("sudo","service cron start")
}

# -----------------------------------------------------------
# Libraries, functions and globals
# -----------------------------------------------------------
logMsg("Loading libraries")
library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(stringr)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(geojsonsf)
library(spdplyr)
library(readr)

logMsg("Sourcing fun.R")
source("fun.R", local=TRUE)
logMsg("Sourcing hlp.R")
source("hlp.R", local=TRUE)
logMsg("Sourcing ages.R")
source("ages.R", local=TRUE)


# --------------------------------------------------------------------------------------------------------------------------
# Global section. Data available to all sessions
# --------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# Map Austria data structures
# --------------------------------------------------------------------------------------------
logMsg("Constructing geo data structures")
# Poligon geoJson structure
mapNUTSAT <- funNUTSAT() 
mapATRegions <- funATRegions()
mapATCounties <- funATCounties()
datATCounties <- mapATCounties@data %>%
  dplyr::arrange(Region, County)

# ---------------------------------------------------------------------------------
# States: Reactive File Poller: Monitor for new files created by cron
# ---------------------------------------------------------------------------------
cwmStatesFile <- "./data/COVID-19-CWM-AGES-States-Curated.rda"
df.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmStatesFile,
  readFunc=readRDS
)

# complete timeframe
df <- eventReactive(df.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:df", cwmStatesFile)) 

  df.rfr() %>%
    dplyr::mutate(RegionID=as.character(RegionID)) %>%
    dplyr::select(Date,Region,RegionID,Population, starts_with("rma"), starts_with("rm7"), newConfPop, dt7rm7NewConfPop) 
})

# Data for log-linear model for weathermap predictions
df.model <- eventReactive(df(), {
  logMsg(paste("eventReactive reactiveFileReader:df.model", cwmPredictionFile)) 
  
  dx <- df() %>% dplyr::filter(Date>max(Date)-days(nModelDaysPrediction)) %>% dplyr::select(Date, Region, rm7NewConfPop, rmaNewConfPop)
  dy <- cwm.model(dx=dx, dg=datATRegions, locID="Region", colID="RegionS")
  return(dy)
})

# enriched geomap data for WeatherMap of Bundesländer leaflet plot
df.map <- eventReactive(df.model(), {
  logMsg(paste("eventReactive reactiveFileReader:map", cwmPredictionFile)) 

  # Conjugate model data frame and add some geoRegion attributes
  dm <- data.frame(t(df.model() %>% dplyr::select(-Inzidenz)), stringsAsFactors=FALSE)  %>% 
    dplyr::mutate(RegionS=rownames(.)) %>%
    dplyr::mutate(Date=as.Date(Date)) %>%
    dplyr::left_join(datATRegions %>% dplyr::select(RegionS,RegionID), by="RegionS")
  
  # Add prediction data to geoMap data
  pMapNUTS <- mapNUTSAT %>% 
    dplyr::left_join(dm, by="RegionID") %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.0,binForeCast), 
                  idxForConfPop=.bincode(rm7NewConfPop.7,binForeCast),
                  idxMonConfPop=.bincode(rm7NewConfPop.28,binForeCast),
                  idxDblConfPop=.bincode(dtDay,binDblDays))
  
  # need to plot 'Österreich' last
  pMapNUTS <- pMapNUTS[c(1:4,6:10,5),]
  return(pMapNUTS)
})

# timeframe for '2020 history'
de <- eventReactive(df.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:de", cwmStatesFile)) 

  df.rfr() %>% 
    dplyr::filter(Date>=as.Date("2020-07-27"),Date<=as.Date("2020-11-16")) %>%
    dplyr::select(Date,Region,RegionID,Population, starts_with("rma"), starts_with("rm7"), newConfPop, dt7rm7NewConfPop, modrm7NewConfPop) 
})


# ---------------------------------------------------------------------------------
# Counties: Reactiv File Poller: Monitor for new files created by cron
# ---------------------------------------------------------------------------------
cwmCountiesFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"
dg.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmCountiesFile,
  readFunc=readRDS
)
dg <- eventReactive(dg.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:dg", cwmCountiesFile)) 

    # Select only features currently needed
  dg.rfr() %>% 
    dplyr::select(Date, Region, RegionID, County, CountyID, CountyNR, Population, starts_with("rm7"),starts_with("rma")) %>%
    dplyr::mutate(rmaNewConfPop=rmaNewConfPop, rmaNewConfirmed=rmaNewConfirmed) 
})

dg.model <- eventReactive(dg(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.model", cwmPredictionFile)) 
  
  dx <- dg() %>% dplyr::filter(Date>max(Date)-days(nModelDaysPrediction)) %>% dplyr::select(Date, Region, RegionID, County, CountyID, CountyNR, Population, rm7NewConfPop, rmaNewConfPop)
  dy <- cwm.model(dx=dx, dg=datATCounties, locID="CountyID", colID="CountyID")
  return(dy)
})

# enriched geomap data for WeatherMap of Bundesländer leaflet plot
dg.map <- eventReactive(dg.model(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.map", cwmPredictionFile)) 
  
  # Conjugate model data frame and add some geoRegion attributes
  dm <- data.frame(t(dg.model() %>% dplyr::select(-Inzidenz)), stringsAsFactors=FALSE)  %>% 
    dplyr::mutate(CountyID=rownames(.)) %>%
    dplyr::mutate(Date=as.Date(Date)) %>%
    dplyr::select(Date, CountyID, starts_with("rm"), dtDay, ends_with("Days"))
  
  # Add prediction data to geoMap data
  pMapATCounties <- mapATCounties %>% 
    dplyr::left_join(dm, by="CountyID") %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.0,binForeCast), 
                  idxForConfPop=.bincode(rm7NewConfPop.7,binForeCast),
                  idxMonConfPop=.bincode(rm7NewConfPop.28,binForeCast),
                  idxDblConfPop=.bincode(dtDay,binDblDays))
  
  return(pMapATCounties)
})

dg.mapx <- eventReactive(dg(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.map", cwmCountiesFile)) 
  dg() %>% dplyr::filter(Date>max(Date)-days(nWeatherForeCastDaysCountyMonth)) })

dg.today <- eventReactive(dg(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.today", cwmCountiesFile)) 
  dg() %>% dplyr::filter(Date==max(Date)) %>% 
    dplyr::mutate(rm7NewConfPop=round(rm7NewConfPop,1), TagesInzidenzAGES=round(rmaNewConfPop,1)) %>%
    dplyr::select(Date, Region, County, Population, TagesInzidenz=rm7NewConfPop, TagesInzidenzAGES)})

# Prediction ???
cwmPredictionFile <- "./data/COVID-19-CWM-AGES-Prediction.rda"
wm.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmPredictionFile,
  readFunc=readRDS
)





# Weather Icons
iconSizeWeather=40
iconsWeather <- iconList (
  Sun =       makeIcon(iconUrl="./www/iconWeather-Sun.png",       iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Cloud = makeIcon(iconUrl="./www/iconWeather-Sun-Cloud.png", iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Rain =  makeIcon(iconUrl="./www/iconWeather-Sun-Rain.png",  iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Rain =      makeIcon(iconUrl="./www/iconWeather-Rain.png",      iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Thunder =   makeIcon(iconUrl="./www/iconWeather-Thunder.png",   iconWidth=iconSizeWeather, iconHeight=iconSizeWeather)
)

# Direction Icons
iconSizeDirection=24
iconsDirection <- iconList (
  dirS =   makeIcon(iconUrl="./www/iconDirection-S.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirESE = makeIcon(iconUrl="./www/iconDirection-SSE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirSSE = makeIcon(iconUrl="./www/iconDirection-ESE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirE =   makeIcon(iconUrl="./www/iconDirection-E.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirENE = makeIcon(iconUrl="./www/iconDirection-ENE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirNNE = makeIcon(iconUrl="./www/iconDirection-NNE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirN =   makeIcon(iconUrl="./www/iconDirection-N.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection)
)


# --------------------------------------------------------------------------------------------------------------------------
# GUI definition
# --------------------------------------------------------------------------------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19-WeatherMap"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
      p("CWM-V0.9.13-20210203"),
      
      fluidRow(
        column(6,
          checkboxGroupInput("cbgRegion",
            width="220px",
            label="Region",
            choices = list("Österreich", 
                           "Wien", 
                           "Niederösterreich", 
                           "Burgenland", 
                           "Oberösterreich",
                           "Steiermark", 
                           "Kärnten", 
                           "Salzburg", 
                           "Tirol", 
                           "Vorarlberg"), 
            selected = c("Österreich","Wien","Niederösterreich","Burgenland","Kärnten"))),
        column(6,
          actionButton("abUpdate", "Anzeigen"))),

        fluidRow(
          checkboxInput("cbLogScale", label="LogScale", value=TRUE, width="220px")),
        
      fluidRow( 
        sliderInput("sldPastTime",
                    width="220px",
                    label="ZeitRaum (Monate)",
                    min=1, max=12, step=1, value=6)),
      
        fluidRow( 
          hr(style = "border-top: 3px solid #777777;"),
          sliderInput("sldModelDays",
                       width="220px",
                       label="Prognose: BerechnungsTage",
                       min=7, max=28, step=7, value=14)),
      fluidRow(        
        radioButtons("rbsModelOrder",
                     width="220px",
                     label="Prognose: BerechnungsModel",
                     choices = list("Linear (Gerade)" = "1",
                                    "Quadratisch (Parabel)" = "2"),
                     selected="1")),
  ),


    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",
 
       tabPanel("Karte Bundesländer",
                 h4("Lage und Aussichten Bundesländer", align = "left", style="color:gray"),
                 p("[Menüauswahl: keine]", align = "left", style="color:green"),
                fluidRow(column(width=9,  leafletOutput(outputId = "lftWeatherMap", height="60vh"),
                                          DT::dataTableOutput(outputId = "dtoWeatherMap")),
                                    column(width=3, htmlOutput(outputId="hlpWeatherMap")))),
 
       tabPanel("Karte Bezirke",
                h4("Lage und Aussichten Bezirke", align = "left", style="color:gray"),
                p("[Menüauswahl: keine]", align = "left", style="color:green"),
                fluidRow(column(width=9,  leafletOutput(outputId = "lftWeatherMapCounties", height="60vh"),
                                DT::dataTableOutput(outputId = "dtoWeatherMapCounties")),
                         column(width=3, htmlOutput(outputId="hlpWeatherMapCounties")))),
       
                                  
        tabPanel("Prognose Bundesländer",
                 h4("Prognose TagesInzidenz", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,LogScale,BerechnungsTage,BerechnungsModell]", align = "left", style="color:green"),
                 fluidRow(column(width=8, plotOutput(outputId = "ggpIncidencePrediciton", height="75vh")),
                          column(width=4, htmlOutput(outputId="hlpIncidencePrediction")))),

        tabPanel("Inzidenz Bundesländer",
                  h4("TagesInzidenz Bundesländer", align = "left", style="color:gray"),
                  p("[Menüauswahl: Region,Zeitbereich,LogScale]", align = "left", style="color:green"),
                  fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceStates", height="75vh")),
                           column(width=3, htmlOutput(outputId="hlpIncidenceStates")))),

          tabPanel("Inzidenz Bezirke",
                 h4("TagesInzidenz Bezirke", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,Zeitbereich,LogScale]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceCounties", height="75vh"),
                                          DT::dataTableOutput(outputId = "dtoIncidenceCounties")),
                          column(width=3, htmlOutput(outputId="hlpIncidenceCounties")))),
        

        tabPanel("Ausbreitungsgeschwindigkeit",
                 h4("Änderung der TagesInzidenz in % vom Vortag", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,Zeitbereich]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpChangeRateStates", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpChangeRateStates")))),

        tabPanel("Rückblick 2020",
                 h4("Exponentielles Wachstum in zweiten Halbjahr 2020", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,Zeitbereich,LogScale]", align = "left", style="color:green"),
                 fluidRow(column(width=9, 
                                 plotOutput(outputId = "ggpExpDateConfPop", height="60vh"),
                                 plotOutput(outputId = "ggpExpDatedt7ConfPop", height="60vh"),
                                 plotOutput(outputId = "ggpExpConfPopdt7ConfPop", height="60vh")),
                          column(width=3, htmlOutput(outputId="hlpExponential")))),

#        tabPanel("Rohdaten Bundesländer",
#          h4("Rohdaten Bundesländer", align = "left", style="color:black"),
#          p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
#          DT::dataTableOutput(outputId = "dtoRawDataRegion")),
        
#        tabPanel("Rohdaten Bezirke",
#                 h4("Rohdaten Bezirke", align = "left", style="color:black"),
#                 p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
#                 DT::dataTableOutput(outputId = "dtoRawDataCounty")),
        
        tabPanel("Erläuterungen",
                 h4("Beschreibung der Graphiken und Hintergrund zu Berechnungen", align = "left", style="color:black"),
                 p("[Menüauswahl: keine]", align = "left", style="color:green"),
                 htmlOutput(outputId="manDescription"))
      )
    )
  )
)


# --------------------------------------------------------------------------------------------------------------------------
# GUI definition
# --------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  # identify session
  sessionID = substr(session$token,1,8)
  logMsg("Server WeatherMap app.R", sessionID)
  
  # states by Time
  df.past <- reactive({
    logMsg(" reactive df.past sldPastTime", sessionID)
    return(df() %>% dplyr::filter(Date > max(Date)-months(as.integer(input$sldPastTime))))
  })
  
  # counties by Time
  dg.past <- reactive({
    logMsg(" reactive dg.past sldPastTime", sessionID)
    return(dg() %>% dplyr::filter(Date > max(Date)-months(as.integer(input$sldPastTime))))
  })

  # state history by regions
  de.regions <- reactive({
    logMsg(" reactive de.regions cbgRegion", sessionID)
    input$abUpdate
    return(de() %>% dplyr::filter(Region %in% isolate(input$cbgRegion)))
  })
  

  
  # -------------------------------------------
  # Weather Map BundesLänder
  # -------------------------------------------
  output$hlpWeatherMap <- renderText({ htmlWeatherMap })

  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoWeatherMap <- DT::renderDataTable({ df.model() %>% dplyr::filter(! Inzidenz %in% c("Date","dtDay","dblDays")) })
  # options=list(pageLength=8, lengthChange=FALSE) 
  
  # WeatherMap
  output$lftWeatherMap <- renderLeaflet({
    logMsg("  output renderPlot lftWeatherMap", sessionID)
    options(warn=-1)
    
    # Depend on reactive file reader
    pMapNUTS <- df.map()
    
    # hoover 
    labWeatherMap <- sprintf(
      "<table>
          <tr><td><strong>%s</strong></td><td align='right'> %s </td></tr>
          <tr><td>TagesInzidenz AGES: </td><td align='right'> %g</td></tr>
          <tr><td><small><i><b>Modell aus den letzten %s Tagen</b></i></small></td> </tr>
          <tr><td>Inzidenz heute: </td><td align='right'> %g</td></tr>
          <tr><td>Inzidenz kommende Woche: </td><td align='right'>  %g </td></tr>
          <tr><td>Inzidenz nächstes Monat: </td><td align='right'>  %g </td></tr>
          <tr><td>Tage bis Inzidenz %s:  </td><td align='right'> %g </td></tr>
        </table>",
      pMapNUTS$Region, format(pMapNUTS$Date,"%a, %d.%m"),
      round(pMapNUTS$rmaNewConfPop,1),  nModelDaysWeek, 
      round(pMapNUTS$rm7NewConfPop.0,1), round(pMapNUTS$rm7NewConfPop.7,1), round(pMapNUTS$rm7NewConfPop.28,1),
      ifelse(pMapNUTS$dblDays>0,"Verdoppelung","Halbierung"), round(abs(pMapNUTS$dblDays))) %>% lapply(htmltools::HTML)
  
    leaflet(pMapNUTS, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)) %>%
      #addTiles(group="DefaultMap", options=providerTileOptions(minZoom=6, maxZoom=8)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.0,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Stand Heute") %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.7,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommende Woche") %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.28,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommendes Monat") %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), options=layersControlOptions(collapsed=FALSE)) %>%
      addMarkers(lng=~cxNUTS-.35, lat=~cyNUTS, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addMarkers(lng=~cxNUTS, lat=~cyNUTS, icon=~iconsDirection[idxDblConfPop], group="Trend",
                 label=labWeatherMap, labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px")) %>%
      addMarkers(lng=~cxNUTS+.35, lat=~cyNUTS, icon=~iconsWeather[idxForConfPop], group="ForeCast") %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.0, position="bottomright", opacity=1, title="Incidence") %>%
      setView(lng=pMapNUTS$cxNUTS[1]-3, lat=pMapNUTS$cyNUTS[1], zoom=7)
  })

  # -------------------------------------------
  # Weather Map Bezirke
  # -------------------------------------------
  output$hlpWeatherMapCounties <- renderText({ htmlWeatherMapCounties })
  
  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoWeatherMapCounties <- DT::renderDataTable({ dg.map()@data %>% 
                                      dplyr::rename(AGES=rmaNewConfPop,
                                                    Heute=rm7NewConfPop.0, Woche=rm7NewConfPop.7, Monat=rm7NewConfPop.28,
                                                    Änderung=dtDay, TageDoppelt=DblDays, TageHälfte=HalfDays, 
                                                    TageEndeLockDown=rm7NewConfPop8, TageBeginLockDown=rm7NewConfPop32) %>%
                                      dplyr::select(Region,County,AGES,
                                                    Heute, Woche, Monat,
                                                    Änderung, TageDoppelt, TageHälfte, 
                                                    TageEndeLockDown, TageBeginLockDown) })
  # options=list(pageLength=8, lengthChange=FALSE) 
  
  # WeatherMap
  output$lftWeatherMapCounties <- renderLeaflet({
    logMsg("  output renderPlot lftWeatherMapCounties", sessionID)
    options(warn=-1)
    
    # Depend on reactive file reader
    pMapCounties <- dg.map()
    
    labWeatherMapCounties <- sprintf(
      "<table>
          <tr><td><strong>%s</strong></td><td align='right'> %s </td></tr>
          <tr><td>TagesInzidenz AGES: </td><td align='right'> %g</td></tr>
          <tr><td><small><i><b>Modell aus den letzten %s Tagen</b></i></small></td> </tr>
          <tr><td>Inzidenz heute: </td><td align='right'> %g</td></tr>
          <tr><td>Inzidenz kommende Woche: </td><td align='right'>  %g </td></tr>
          <tr><td>Inzidenz nächstes Monat: </td><td align='right'>  %g </td></tr>
          <tr><td>Tage bis Inzidenz %s:  </td><td align='right'> %g </td></tr>
        </table>",
      pMapCounties$County, format(pMapCounties$Date,"%a, %d.%m"),
      round(pMapCounties$rmaNewConfPop,1), 
      nModelDaysCountyWeek,
      round(pMapCounties$rm7NewConfPop.0,1), round(pMapCounties$rm7NewConfPop.7,1), round(pMapCounties$rm7NewConfPop.28,1),
      ifelse(pMapCounties$dblDays>0,"Verdoppelung","Halbierung"), abs(round(pMapCounties$dblDays)))  %>% 
      lapply(htmltools::HTML)
    
    #, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)
    leaflet(pMapCounties, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE)) %>%
      #addTiles(group="DefaultMap", options=providerTileOptions(minZoom=6, maxZoom=8)) %>%
      #addMapPane("Counties", zIndex=410) %>% 
      #addMapPane("States", zIndex=420) %>%
      # options(pathOptions(pane="Counties")),
      # overlayGroups="Bundesländer",
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.0,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Stand Heute") %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.7,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommende Woche") %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.28,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommendes Monat") %>%
      addPolygons(data=mapATRegions, stroke = TRUE,  weight=3, color="black", smoothFactor = 0, fillOpacity = 0, fillColor="none", group="Bundesländer") %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), 
                        options=layersControlOptions(collapsed=FALSE)) %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.0, position="bottomright", opacity=1, title="Incidence") 
  })
 
  # -------------------------------------------
  # Prediction TagesInzidenz
  # -------------------------------------------
  output$hlpIncidencePrediction <- renderText({ htmlIncidencePrediction })
  
  output$ggpIncidencePrediciton <- renderPlot({
    logMsg("  output renderPlot ggpIncidencePrediciton", sessionID)
    options(warn=-1)
    
    # react on Update button
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dk <- df.past() %>% dplyr::filter(Region %in% inRegions) %>%
      dplyr::mutate(locID=RegionID) # added so cwmAgesRm7EstimatePoly know what to look for (RegionID or CountyID)
    
    dp <- cwmAgesRm7EstimatePoly(dk, nModelDays=input$sldModelDays+3, nPoly=as.integer(input$rbsModelOrder), nPredDays=28)

    ggplot(data=dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region)) + 
      cwmConfPopStyle(sldPastTime=1, cbLogScale=input$cbLogScale, inRegions=inRegions, xLimits=c(max(dk$Date)-weeks(6), max(dp$Date)+days(1))) +
      geom_line(linetype=2, size=1) + 
      geom_line(data=dk, aes(x=Date, y=1), size=1.0, color="green") +
      geom_line(data=dk, aes(x=Date, y=2), size=1.0, color="orange") +
      geom_line(data=dk, aes(x=Date, y=4), size=.8, color="magenta") +
      geom_line(data=dk, aes(x=Date, y=8), size=.8, color="red") +
      geom_line(data=dk, aes(x=Date, y=16), size=.8, color="darkred") +
      geom_line(data=dk, aes(x=Date, y=32), size=.8, color="black") +
      geom_line(data=dk, aes(x=Date, y=64), size=1.0, color="black") +
      geom_line(data=dk, aes(x=Date, y=128), size=1.5, color="black") +
      geom_point(data=dk%>%dplyr::filter(Date==max(Date)),size=5) + 
      geom_point(data=dk%>%dplyr::filter(Date==max(Date)),size=2) + 
      geom_point(data=dp%>%dplyr::filter(Date %in% c(max(Date), max(Date)-weeks(3))),size=5) + 
      geom_point(data=dk,aes(x=Date,y=rm7NewConfPop), size=2) + 
      geom_line(data=dk,aes(x=Date,y=rm7NewConfPop), size=.5) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Prognose TagesInzidenz. Stand ", max(dk$Date), ".  Basisdaten: AGES"))
  })
  
  
  # -------------------------------------------
  # TagesInzidenz BundesLänder
  # -------------------------------------------
  output$hlpIncidenceStates <- renderText({ htmlIncidenceStates })

  output$ggpIncidenceStates <- renderPlot({
    logMsg("  output renderPlot ggpIncidenceStates", sessionID)
    options(warn=-1)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- df.past() %>% dplyr::filter(Region %in% inRegions)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(sldPastTime=input$sldPastTime, cbLogScale=input$cbLogScale, inRegions=inRegions) +
      geom_point(size=2)+geom_line()+
      geom_point(data=dp %>% dplyr::filter(Date==max(Date)), size=4)+
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # TagesInzidenz Bezirke
  # -------------------------------------------
  output$hlpIncidenceCounties <- renderText({ htmlIncidenceCounties })

  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoIncidenceCounties <- DT::renderDataTable({ dg.today() })
    
  output$ggpIncidenceCounties <- renderPlot({
    logMsg("  output renderPlot ggpIncidenceCounties", sessionID)
    options(warn=-1)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- dg.past() %>% dplyr::filter(rm7NewConfPop>0, Region %in% inRegions)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, group=CountyID))+
      cwmConfPopStyle(sldPastTime=input$sldPastTime, cbLogScale=input$cbLogScale, inRegions=inRegions[inRegions!="Österreich"], yLimits=c(.5,256)) +
      geom_line(size=.25, aes(color=Region))+
      ggtitle(paste0("COVID-19 Österreich, Bundesländer und Bezirke: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # ChangeRateStates
  # -------------------------------------------
  output$hlpChangeRateStates <- renderText({ htmlChangeRateStates })
  
  output$ggpChangeRateStates <- renderPlot({
    logMsg("  output renderPlot ggpChangeRateStates", sessionID)
    options(warn=-1)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- df.past() %>% dplyr::filter(Region %in% inRegions) %>% dplyr::filter(dt7rm7NewConfPop<1.19, dt7rm7NewConfPop>.84)
    #dp <- df %>% dplyr::filter(dt7rm7NewConfPop<1.20, dt7rm7NewConfPop>.85)
    
    ggplot(dp, aes(x=Date, y=dt7rm7NewConfPop, color=Region, shape=Region))+
      cwmSpreadStyle(sldPastTime=input$sldPastTime, inRegions=inRegions) +
      geom_line(size=.75) +
      geom_point(size=2) + 
      geom_point(data=dp %>% dplyr::filter(Date==max(Date)), size=4)+
      ggtitle(paste0("COVID-19 Österreich und Bundesländer: Ausbreitungsgeschwindigkeit in % pro Tag, seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })
  
  # -------------------------------------------
  # 2020
  # -------------------------------------------
  output$hlpExponential <- renderText({ htmlExponential })

  output$ggpExpDateConfPop <- renderPlot({
    logMsg("  output renderPlot ggpExpDateConfPop", sessionID)
    options(warn=-1)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    ggplot(de.regions(), aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(sldPastTime=1, cbLogScale=input$cbLogScale, inRegions=inRegions) +
      geom_point(size=2)+geom_line()+
      geom_line(aes(y=modrm7NewConfPop)) +
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: TagesInzidenz: Positiv getestete pro Tag pro 100.000 Einwohner.  Basisdaten: AGES"))
  })

  output$ggpExpDatedt7ConfPop <- renderPlot({
    logMsg("  output renderPlot ggpExpDatedt7ConfPop", sessionID)
    options(warn=-1)
    
    xLimMin <- .9
    xLimMax <- 100
    yLimMin <- 0.84
    yLimMax <- 1.19
    dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)

    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- de.regions()  %>% dplyr::filter(dt7rm7NewConfPop<1.19, dt7rm7NewConfPop>.84)
    
    ggplot(dp, aes(x=Date, y=dt7rm7NewConfPop, color=Region, shape=Region))+
      cwmSpreadStyle(sldPastTime=1, inRegions=inRegions) +
      #scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right") +
      geom_line(size=.75) +
      geom_point(size=2) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Ausbreitungsgeschwindigkeit in % pro Tag.  Basisdaten: AGES"))
  })
  
  output$ggpExpConfPopdt7ConfPop <- renderPlot({
    logMsg("  output  renderPlot ggpExpConfPopdt7ConfPop", sessionID)
    options(warn=-1)
    
    xLimMin <- 1
    xLimMax <- 128
    yLimMin <- 0.84
    yLimMax <- 1.21
    dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    idxRegions <- sort(match(inRegions,atRegions))
    regPalette <- cbPalette[idxRegions]
    regShapes <- atShapes[idxRegions]
    trans <- ifelse(input$cbLogScale, "log10", "identity")
    sSize=0.5
    
    dl <- data.frame(x=c(xLimMin,xLimMax), y=c(yLimMin,yLimMax))
    
    dp <- de.regions()  %>% 
      dplyr::filter(dt7rm7NewConfPop<1.21, dt7rm7NewConfPop>.84) %>%
      dplyr::mutate(Month=month(Date, label=TRUE, abbr=FALSE)) %>%
      dplyr::arrange(Region, Date)
    
    ggplot(dp, aes(x=rm7NewConfPop, y=dt7rm7NewConfPop, color=Region, shape=Month))+
      geom_line(data=dl, aes(x=x,y=1.104), size=sSize, color="#b00000", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=1.051), size=sSize, color="#ff0000", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=1.025), size=sSize, color="#fe7f00", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=1.012), size=sSize, color="#e3e300", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=1.00), size=2.0, color="white", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=0.988), size=sSize, color="#7ffe00", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=0.976), size=sSize, color="#72e400", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=0.952), size=sSize, color="#62c400", inherit.aes=FALSE) +
      geom_line(data=dl, aes(x=x,y=0.906), size=sSize, color="#4f9e00", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=1), size=1.0*sSize, color="green", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=2), size=1.0*sSize, color="orange", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=4), size=.8*sSize, color="magenta", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=8), size=.8*sSize, color="red", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=16), size=.8*sSize, color="darkred", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=32), size=.8*sSize, color="black", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=64), size=1.0*sSize, color="black", inherit.aes=FALSE) +
      geom_line(data=dl, aes(y=y,x=128), size=1.5*sSize, color="black", inherit.aes=FALSE) +
      
      theme(panel.grid.major = element_line(color = "darkgray", linetype=3), 
            panel.grid.minor=element_line(color = "gray90", linetype=1),
            axis.text = element_text(size=12), axis.title.x=element_blank()) +
      scale_shape_manual(values=atShapes) +
      scale_fill_manual(values=regPalette) +
      scale_color_manual(values=regPalette) +
      scale_x_continuous(limits=c(xLimMin,xLimMax), breaks=2^(0:7), trans=trans, sec.axis=dup_axis()) + 
#      scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.025), trans=trans, name="yLabel", sec.axis=dup_axis()) +
      scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblXDays), labels=dblXDays, position="right",
                         sec.axis=dup_axis(labels=as.character(round((exp(log(2)/dblXDays)-1)*100,1)), name="Tägliche Steigerungsrate [%]")) +
      geom_path() + 
      geom_point(size=3) +
      ggtitle(paste0("COVID-19 Österreich und Bundesländer: Ausbreitungsgeschwindigkeit gegen TagesInzidenz.  Basisdaten: AGES"))
  })

  
  #      scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right", sec.axis=dup_axis()) +
  #                         sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100))) +
  #scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  #geom_line(data=dp, aes(x=rm7NewConfPop, y=1)) +
  
  # -------------------------------------------
  # Raw Data
  # -------------------------------------------

  output$dtoRawDataRegion <- DT::renderDataTable({
    df.past() %>% dplyr::filter(Date==max(Date)) %>%
      dplyr::arrange(desc(Date),Region)})

  output$dtoRawDataCounty <- DT::renderDataTable({
    dg.past() %>% dplyr::filter(Date==max(Date)) %>%
      dplyr::mutate(newConfPop=round(newConfPop,2)) %>%
      dplyr::arrange(desc(Date),Region)})
  
  
  # -------------------------------------------
  # Erläuterungen
  # -------------------------------------------
  output$manDescription <- renderText({ htmlDescription })
}


# start shiny app
shinyApp(ui = ui, server = server, options=list(launch.browser = TRUE, width=1650, height=1024))


