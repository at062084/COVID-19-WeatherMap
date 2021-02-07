options(error = function() traceback(2))
options(shiny.reactlog = TRUE)
library(lubridate)
library(httr)

# do some logging
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
library(cronR)

logMsg("Sourcing fun.R")
source("fun.R", local=TRUE)
logMsg("Sourcing hlp.R")
source("hlp.R", local=TRUE)
logMsg("Sourcing ages.R")
source("ages.R", local=TRUE)


# --------------------------------------------------------------------------------------------------------------------------
# Global section. Data available to all sessions
# --------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------
# Define cron job to retrieve new data from AGES
# -----------------------------------------------------------
if(0==1) {
  logMsg("Starting service cron")
  system2("sudo","service cron start")
}

# -----------------------------------------------------------
# Reactiv File Poller: Monitor for new files created by cron
# -----------------------------------------------------------
# States
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
    dplyr::select(Date,Region,RegionID,Population,newConfPop,rm7NewConfirmed,rm7NewConfPop,rm7NewTested,rm7NewConfTest,dt7rm7NewConfPop,modrm7NewConfPop) })
# timeframe for '2020 history'
de <- eventReactive(df.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:de", cwmStatesFile)) 
  df.rfr() %>% 
    dplyr::filter(Date>=as.Date("2020-07-27"),Date<=as.Date("2020-11-16")) })

# Counties
cwmCountiesFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"
dg.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmCountiesFile,
  readFunc=readRDS
)
dg <- eventReactive(dg.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:dg", cwmCountiesFile)) 
  dg.rfr() })
dg.today <- eventReactive(dg(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.today", cwmCountiesFile)) 
  dg() %>% dplyr::filter(Date==max(Date)) %>% 
    dplyr::mutate(rm7NewConfPop=round(rm7NewConfPop,1)) %>%
    dplyr::select(Date, Region, County, Population, TagesInzidenz=rm7NewConfPop)})

# Prediction
cwmPredictionFile <- "./data/COVID-19-CWM-AGES-Prediction.rda"
wm.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmPredictionFile,
  readFunc=readRDS
)

df.pred <- eventReactive(df(), {
  logMsg(paste("eventReactive reactiveFileReader:df.pred", cwmPredictionFile)) 
  
  dx <- df() %>% dplyr::filter(Date>max(Date)-days(nWeatherForeCastDays)) %>% dplyr::select(Date, Region, rm7NewConfPop)
  
  maxDate=max(dx$Date)
  n <- nWeatherForeCastDays + c(0,7,30,92)
  t <- c(4,8,16)
  rowNames <- c("Heute","In einer Woche","In vier Wochen","In drei Monaten","Tage bis Verdoppelung","Tage bis Inzidenz=4", "Tage bis Inzidenz=8", "Tage bis Inzidenz=16")
  dy <- data.frame(Inzidenz=rowNames, stringsAsFactors=FALSE)
  for (r in 1:length(atRegions)) {
    y <- dx$rm7NewConfPop[dx$Region==atRegions[r]]
    p <- rm7PolyLog(y, nPoly=1, nModelDays=length(y), nNewData=n, nTransData=t, bDblDays=TRUE)
    q <- c(round(p$pNewData,1),round(p$pDblDays),round(p$pTransData))
    cn <- colnames(dy)
    dy <- cbind(dy,(q))
    colnames(dy) <- c(cn,atRegionsShort[r])
  } 
  rownames(dy) <- NULL
  dy[,c(1,6,2:5,7:11)]
})

# -----------------------------------------------------
# Map Austria data structures
# -----------------------------------------------------
logMsg("Constructing global data structures")
# Poligon geoJson structure
mapNUTS <- mapNUTSAT()
mapCounties <- mapATCounties()

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
  output$dtoWeatherMap <- DT::renderDataTable({ df.pred() })
  # options=list(pageLength=8, lengthChange=FALSE) 
  
  # WeatherMap
  output$lftWeatherMap <- renderLeaflet({
    logMsg("  output renderPlot lftWeatherMap", sessionID)
    options(warn=-1)
    
    # days from today back nWeatherForeCastDays days
    dh <- df.past() %>% 
      dplyr::filter(Date>max(Date)-days(nWeatherForeCastDays)) %>% 
      dplyr::select(Date, Region, RegionID, dt7rm7NewConfPop,starts_with("rm7")) %>% 
      dplyr::mutate(locID=RegionID) # added so cwmAgesRm7EstimatePoly know what to look for (RegionID or CountyID)
    # Pick data for AGES (rm7 three days ago) 
    da <- dh %>% dplyr::filter(Date==max(Date)-days(3)) %>% dplyr::select(starts_with("Region"), rm7AGESNewConfPop=rm7NewConfPop)
    # model prediction for today
    dq <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=0, nPoly=1) %>%
      dplyr::filter(Date==max(dh$Date))
    # model predicition for next week
    dm <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=7, nPoly=1) %>%
      dplyr::filter(Date==max(Date))
    # model predicition for next month
    do <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=28, nPoly=1) %>%
      dplyr::filter(Date==max(Date))
    # number of days till inzidenz doubles
    dt2 <- dh %>% 
      dplyr::group_by(RegionID) %>% 
      dplyr::summarise(dblDays=rm7PolyLog(rm7NewConfPop, nPoly=1, nModelDays=nWeatherForeCastDays, nNewData=1, bDblDays=TRUE)$pDblDays) %>%
      dplyr::ungroup()
    
    pMapNUTS <- mapNUTS %>% 
      # construct single row with all required fields. merges data for today and forecast
      dplyr::left_join(dm, by="RegionID") %>%
      dplyr::left_join(da, by="RegionID") %>%
      dplyr::left_join(dt2, by="RegionID") %>%
      dplyr::left_join(dq, by="RegionID", suffix = c(".f", ".c")) %>%
      dplyr::left_join(do, by="RegionID") %>%
      dplyr::left_join(dh %>% dplyr::select(Date, RegionID,dt7rm7NewConfPop) %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(-Date), by="RegionID") %>%
      dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.c,binForeCast), 
                    idxDblConfPop=.bincode((rm7NewConfPop.c+(rm7NewConfPop.f-rm7NewConfPop.c)/7)/rm7NewConfPop.c,binDblDays),
                    idxMonConfPop=.bincode(rm7NewConfPop,binForeCast),
                    idxForConfPop=.bincode(rm7NewConfPop.f,binForeCast))
    # need to plot 'Österreich' last
    pMapNUTS <- rbind(pMapNUTS[1,],pMapNUTS[2,],pMapNUTS[3,],pMapNUTS[4,],pMapNUTS[6,],pMapNUTS[7,],pMapNUTS[8,],pMapNUTS[9,],pMapNUTS[10,],pMapNUTS[5,])
                     

    labWeatherMap <- sprintf(
      "<table>
          <tr><td><strong>%s</strong></td><td align='right'> %s </td></tr>
          <tr><td>TagesInzidenz AGES (ca.): </td><td align='right'> %g</td></tr>
          <tr><td>TagesInzidenz heute: </td><td align='right'> %g</td></tr>
          <tr><td>TagesInzidenz nächste Woche: </td><td align='right'>  %g </td></tr>
          <tr><td>Tage bis Inzidenz %s:  </td><td align='right'> %g </td></tr>
        </table>
        <small><i>Alle Werte berechnet auf Basis der letzten 10 Tage</i></small>",
        pMapNUTS$Region, format(pMapNUTS$Date.c,"%a, %d.%m"),
        round(pMapNUTS$rm7AGESNewConfPop,1),
        round(pMapNUTS$rm7NewConfPop.c,1), round(pMapNUTS$rm7NewConfPop.f,1), 
        ifelse(pMapNUTS$dblDays>0,"Verdoppelung","Halbierung"),round(abs(pMapNUTS$dblDays))) %>% lapply(htmltools::HTML)
  
    leaflet(pMapNUTS, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7, )) %>%
      #addTiles(group="DefaultMap", options=providerTileOptions(minZoom=6, maxZoom=8)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.c,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Stand Heute") %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.f,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommende Woche") %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommendes Monat") %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), options=layersControlOptions(collapsed=FALSE)) %>%
      addMarkers(lng=~cxNUTS-.35, lat=~cyNUTS, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addMarkers(lng=~cxNUTS, lat=~cyNUTS, icon=~iconsDirection[idxDblConfPop], group="Trend",
                 label=labWeatherMap, labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px")) %>%
      addMarkers(lng=~cxNUTS+.35, lat=~cyNUTS, icon=~iconsWeather[idxForConfPop], group="ForeCast") %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.c, position="bottomright", opacity=1, title="Incidence") %>%
      setView(lng=pMapNUTS$cxNUTS[1]-3, lat=pMapNUTS$cyNUTS[1], zoom=7)
  })

  # -------------------------------------------
  # Weather Map Bezirke
  # -------------------------------------------
  output$hlpWeatherMapCounties <- renderText({ htmlWeatherMapCounties })
  
  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoWeatherMapCounties <- DT::renderDataTable({ dg.today() })
  # options=list(pageLength=8, lengthChange=FALSE) 
  
  # WeatherMap
  output$lftWeatherMapCounties <- renderLeaflet({
    logMsg("  output renderPlot lftWeatherMapCounties", sessionID)
    options(warn=-1)
    
    # today's data for weathermap
    # nWeatherForeCastDays=nWeatherForeCastDays # same as slider=7 for incidence prediction (3 is automatically added to slider reading)
    # days from today back nWeatherForeCastDays days
    dh <- dg.past() %>% 
      dplyr::filter(Date>max(Date)-days(nWeatherForeCastDays)) %>% 
      dplyr::mutate(locID=CountyID) # added so cwmAgesRm7EstimatePoly know what to look for (RegionID or CountyID)
    
    # Pick data for AGES (rm7 three days ago) 
    #da <- dh %>% dplyr::filter(Date==max(Date)-days(3)) %>% dplyr::select(Region, rm7AGESNewConfPop=rm7NewConfPop)
    # model prediction for today
    dq <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=0, nPoly=1) %>%
      dplyr::filter(Date==max(dh$Date)) %>% dplyr::select(CountyID, rm7NewConfPop)
    # model predicition for next week
    dm <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=7, nPoly=1) %>%
      dplyr::filter(Date==max(Date))  %>% dplyr::select(CountyID, rm7NewConfPop)
    # model predicition for next month
    do <- cwmAgesRm7EstimatePoly(dh, nModelDays=nWeatherForeCastDays, nPredDays=28, nPoly=1) %>%
      dplyr::filter(Date==max(Date)) %>% dplyr::select(CountyID, rm7NewConfPop)
    
    # AGES compliant
    pMapCounties <- mapCounties %>% 
      dplyr::left_join(dq, by="CountyID") %>%
      dplyr::left_join(dm, by="CountyID", suffix = c(".c", ".f")) %>%
      dplyr::left_join(do, by="CountyID") #%>%
    
    labWeatherMapCounties <- sprintf(
      "<table>
          <tr><td><strong>%s</strong></td><td align='right'> %s </td></tr>
          <tr><td>TagesInzidenz heute: </td><td align='right'> %g</td></tr>
          <tr><td>Prognose kommende Woche: </td><td align='right'>  %g </td></tr>
          <tr><td>Prognose nächstes Monat: </td><td align='right'>  %g </td></tr>
        </table>,
        <small><i>Alle Werte berechnet auf Basis der letzten 10 Tage</i></small>",
      pMapCounties$County, format(max(dh$Date),"%a, %d.%m"),
      round(pMapCounties$rm7NewConfPop.c,1), 
      round(pMapCounties$rm7NewConfPop.f,1), 
      round(pMapCounties$rm7NewConfPop,1))  %>% lapply(htmltools::HTML)
    
    #, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)
    leaflet(pMapCounties, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE)) %>%
      #addTiles(group="DefaultMap", options=providerTileOptions(minZoom=6, maxZoom=8)) %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.c,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Stand Heute") %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.f,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommende Woche") %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"), group="Prognose kommendes Monat") %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), options=layersControlOptions(collapsed=FALSE)) %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.f, position="bottomright", opacity=1, title="Incidence") 
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


