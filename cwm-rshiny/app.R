options(error = function() traceback(2))
options(shiny.reactlog = TRUE)

# do some logging
logDir = "./log"
logFile <- "cwm.rshiny.log"
logMsg <- function(msg, sessionID="_global_") {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=stderr())
}

logMsg("Loading libraries")
library(shiny)
library(lubridate)
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
# Global constants
#dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
#popBreaks <- c(0,1,2,5,10,15,20,25,seq(30,150,by=10))
#popLogBreaks <- (c(.1,.2,.5,1,2,5,10,20,50,100))
#logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000))
#logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000),seq(10000,100000,by=10000))
#nModelDays=28
#nPredDays=14
#yLimMax <- 128
#julDate <- as.Date("2020-07-01")
#trans="log10"

# -----------------------------------------------------------
# Define cron job to retrieve new data from AGES
# -----------------------------------------------------------
#if(0==1) {
logMsg("Define cron job for data retrieval from AGES")
cronJobDir <- "/srv/shiny-server/COVID-19-WeatherMap"
#cronJobDir <- "/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny"
cronJobFile <- paste0(cronJobDir,"/cron.R")
cronJobLog <-paste0(cronJobDir,"/log/cwm.cron.log")  
cmd <- cron_rscript(rscript=cronJobFile, rscript_log=cronJobLog, log_timestamp=TRUE, workdir=cronJobDir)
cmd
cron_clear(ask=FALSE)
cron_add(cmd, id='AGES-14', at = '14:14')
cron_add(cmd, id='AGES-22', at = '22:22')
logMsg("Starting service cron")
system2("sudo","service cron start")
#}
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
  logMsg(paste("eventReactive: reactiveFileReader df for", cwmStatesFile)) 
  df.rfr()} )
# timeframe for '2020 history'
de <- eventReactive(df.rfr(), {
  logMsg(paste("eventReactive: reactiveFileReader de for", cwmStatesFile)) 
  df.rfr() %>% 
    dplyr::filter(Date>=as.Date("2020-07-27"),Date<=as.Date("2020-11-16")) %>% 
    dplyr::select(Date,Region,rm7NewConfPop,dt7rm7NewConfPop, modrm7NewConfPop)} )

# Counties
cwmCountiesFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"
dg.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmCountiesFile,
  readFunc=readRDS
)
dg <- eventReactive(dg.rfr(), {
  logMsg(paste("eventReactive: reactiveFileReader dg for", cwmStatesFile)) 
  dg.rfr()} )


#cwmTestFile <- "./data/COVID-19-CWM-AGES-Test-Curated.rda"
#cwmTestFile <- "./data/test.csv"
#dt.rfr <- reactiveFileReader(
#  session=NULL,
#  intervalMillis=1000,
#  filePath=cwmTestFile,
#  readFunc=read.csv
#)
#dt <- eventReactive(dt.rfr(), {
#  logMsg(paste("eventReactive: reactiveFileReader for", cwmTestFile)) 
#  dt.rfr()} )

# -----------------------------------------------------
# AGES data files
# -----------------------------------------------------
#logMsg("Loading data files")
#df <- readRDS(file="./data/COVID-19-CWM-AGES-States-Curated.rda")
#dg <- readRDS(file="./data/COVID-19-CWM-AGES-Counties-Curated.rda")

# -----------------------------------------------------
# Map Austria data structures
# -----------------------------------------------------
logMsg("Constructing global data structures")
# Poligon geoJson structure
mapNUTS <- mapNUTSAT()

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
      #p("COVID-19-WeatherMap", align = "left", style="color:darkred"),
      p("CWM-V0.5.2-20210128"),
#      tableOutput("secTime"),
      
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
            selected = c("Österreich","Wien","Kärnten"))),
        column(6,
          actionButton("abUpdate", "Update Region"))),

        fluidRow(        
             radioButtons("rbsPastTime",
              width="220px",
              label="Zeitaum",
            choices = list("1 Monat" = "4",
                           "2 Monate" = "9",
                           "3 Monate" = "13",
                           "4 Monate" = "18",
                           "5 Monate" = "22",
                           "6 Monate" = "26",
                           "12 Monate"= "53"),
            selected="13")),

      fluidRow(
        checkboxInput("cbLogScale", label="LogScale", value=TRUE, width="220px")),
      
      
        fluidRow( 
          hr(style = "border-top: 1px solid #777777;"),
          sliderInput("sldModelDays",
                       width="220px",
                       label="BerechnungsTage",
                       min=7, max=28, step=7, value=14)),
      fluidRow(        
        radioButtons("rbsModelOrder",
                     width="220px",
                     label="BerechnungsModel",
                     choices = list("Linear" = "1",
                                    "Quadratisch" = "2"),
                     selected="1")),
      

  ),

    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",
 
       tabPanel("COVID Lage und Aussichten",
                 h4("Lage und Aussichten Bundesländer", align = "left", style="color:gray"),
                 p("[Menüauswahl: keine]", align = "left", style="color:green"),
                 fluidRow(column(width=9, leafletOutput(outputId = "lftWeatherMap", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpWeatherMap")))),
                  
        tabPanel("Inzidenz Prognose",
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
                 fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceCounties", height="75vh")),
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

  oldWarn <- getOption("warn")
  options(warn=-1)
  
  # identify session
  sessionID = substr(session$token,1,8)
  logMsg("Server called ...", sessionID)
  
  # states by Time
  df.past <- reactive({
    logMsg("  Reactive: df.past: Filtering for rbsPastTime", sessionID)
    return(df() %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })
  
  # counties by Time
  dg.past <- reactive({
    logMsg("  Reactive: dg.past: Filtering for rbsPastTime", sessionID)
    return(dg() %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })

  # state history by regions
  de.regions <- reactive({
    logMsg("  Reactive: dg.past: Filtering for cbgRegion", sessionID)
    input$abUpdate
    return(de() %>% dplyr::filter(Region %in% isolate(input$cbgRegion)))
  })
  

  
  # -------------------------------------------
  # Weather Map
  # -------------------------------------------
  output$hlpWeatherMap <- renderText({ htmlWeatherMap })
  
  output$lftWeatherMap <- renderLeaflet({
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)
    
  #levConfPop <- round(c(0,10^seq(0,2,by=0.2),1000),1)
  # bins for rm7ConfPop
  binConfPop <- c(0,1,1.4,2,2.8,4,5.6,8,11,16,22,32,45,64,90,128,512)
  palConfPop <- c(brewer.pal(9,"Greens")[c(7,6,5,4)], brewer.pal(9,"YlOrRd"), "#404040", brewer.pal(9,"Greys")[c(8,9)])
  colConfPop <- colorBin(palette=palConfPop, domain=0:256, bins=binConfPop)
  
  # 7 icons: bins for dt* 
  dblDays <- c(1,14,28,56,-56,-28,-14,-1)
  binDblDays <- sort(round(exp(log(2)/dblDays),3))
  
  # 5 Icons
  binForeCast <- c(0,4,8,16,32,Inf)
  
  # today's data for weathermap
  nWeatherForeCastDays=10 # same as slider=7 for incidence prediction (3 is automatically added to slider reading)
  #dp <- df.past() %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7"))# Today
  # days from today back nWeatherForeCastDays days
  de <- df.past() %>% dplyr::filter(Date>=max(Date)-days(nWeatherForeCastDays)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7")) # Past days for forecast
  # model prediction for today
  dq <- cwmAgesRm7EstimatePoly(de, nModelDays=nWeatherForeCastDays, nPredDays=0, nPoly=1) %>%
    dplyr::filter(Date==max(de$Date))
  # model predicition for next week
  dm <- cwmAgesRm7EstimatePoly(de, nModelDays=nWeatherForeCastDays, nPredDays=7, nPoly=1) %>%
    dplyr::filter(Date==max(Date))
  

  pMapNUTS <- mapNUTS %>% 
    # construct single row with all required fields. merges data for today and forecast
    dplyr::left_join(dm, by="Region") %>%
    dplyr::left_join(dq, by="Region", suffix = c(".f", ".c")) %>%
    dplyr::left_join(de %>% dplyr::select(Date, Region,dt7rm7NewConfPop) %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(-Date), by="Region") %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.c,binForeCast), 
                  idxDblConfPop=.bincode((rm7NewConfPop.c+(rm7NewConfPop.f-rm7NewConfPop.c)/7)/rm7NewConfPop.c,binDblDays), 
                  idxForConfPop=.bincode(rm7NewConfPop.f,binForeCast))
  # idxDblConfPop=.bincode(dt7rm7NewConfPop,binDblDays), 
  
  labWeatherMap <- sprintf(
    "<strong>%s</strong><br/>TagesInzidenz: %g pro 100000<br>Änderung seit letzer Woche: %g%%<br>Prognose nächste Woche: %g",
    pMapNUTS$Region, round(pMapNUTS$rm7NewConfPop.c,2), round(pMapNUTS$dt7rm7NewConfPop,2), round(pMapNUTS$rm7NewConfPop.f,2)) %>% lapply(htmltools::HTML)
  
    leaflet(pMapNUTS) %>%
      addTiles(group="DefaultMap",options = providerTileOptions(minZoom=6, maxZoom=8)) %>%
      setView(lng=pMapNUTS$cxNUTS[1]-3, lat=pMapNUTS$cyNUTS[1], zoom=7) %>%
      #addPolygons(data=mapNUTS1, stroke = TRUE, smoothFactor = 0, color="black", fillOpacity = 0, fillColor="None", weight=10, group="AT1") %>%
      #addPolygons(data=mapNUTS3, stroke = TRUE, smoothFactor = 0, fillOpacity = 0, fillColor="none", weight=1, group="AT3") %>%
      addPolygons(stroke = TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.c,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"),
                  group="AT2") %>%
      addMarkers(lng=~cxNUTS-.35, lat=~cyNUTS, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addMarkers(lng=~cxNUTS, lat=~cyNUTS, icon=~iconsDirection[idxDblConfPop], group="Trend",
                 label=labWeatherMap, labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px")) %>%
      addMarkers(lng=~cxNUTS+.35, lat=~cyNUTS, icon=~iconsWeather[idxForConfPop], group="ForeCast") %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.c, position="bottomright", opacity=1, title="Incidence") %>%
      #addMarkers(lng=~cxNUTS, lat=~cyNUTS, group="Trend", label=atRegions, popup=~Region) %>%
      #addLayersControl(overlayGroups=c("AT1","AT3"), options=layersControlOptions(collapsed=FALSE)) %>%
      hideGroup(c("AT1","AT3","Markers"))
  })
  
  # -------------------------------------------
  # TagesInzidenz BundesLänder
  # -------------------------------------------
  output$hlpIncidencePrediction <- renderText({ htmlIncidencePrediction })
  
  output$ggpIncidencePrediciton <- renderPlot({
    logMsg("  output$ggpIncidencePrediciton: renderPlot", sessionID)
    
    # react on Update button
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dk <- df.past() %>% dplyr::filter(Region %in% inRegions)
    dp <- cwmAgesRm7EstimatePoly(dk, nModelDays=input$sldModelDays+3, nPoly=as.integer(input$rbsModelOrder), nPredDays=28)

    ggplot(data=dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region)) + 
      cwmConfPopStyle(rbsPastTime=5, cbLogScale=input$cbLogScale, inRegions=inRegions, xLimits=c(max(dk$Date)-weeks(6), max(dp$Date)+days(1))) +
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
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)

    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- df.past() %>% dplyr::filter(Region %in% inRegions)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(rbsPastTime=input$rbsPastTime, cbLogScale=input$cbLogScale, inRegions=inRegions) +
      geom_point(size=2)+geom_line()+
      geom_point(data=dp %>% dplyr::filter(Date==max(Date)), size=4)+
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # TagesInzidenz Bezirke
  # -------------------------------------------
  output$hlpIncidenceCounties <- renderText({ htmlIncidenceCounties })
  
  output$ggpIncidenceCounties <- renderPlot({
    logMsg("  output$ggpIncidenceCounties: renderPlot", sessionID)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- dg.past() %>% dplyr::filter(newConfPop>0, Region %in% inRegions)
    
    ggplot(dp, aes(x=Date, y=newConfPop, group=CountyID))+
      cwmConfPopStyle(rbsPastTime=input$rbsPastTime, cbLogScale=input$cbLogScale, inRegions=inRegions[inRegions!="Österreich"], yLimits=c(.5,256)) +
      geom_line(size=.25, aes(color=Region))+
      ggtitle(paste0("COVID-19 Österreich, Bundesländer und Bezirke: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # ChangeRateStates
  # -------------------------------------------
  output$hlpChangeRateStates <- renderText({ htmlChangeRateStates })
  
  output$ggpChangeRateStates <- renderPlot({
    logMsg("  output$ggpChangeRateStates: renderPlot", sessionID)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- df.past() %>% dplyr::filter(Region %in% inRegions) %>% dplyr::filter(dt7rm7NewConfPop<1.19, dt7rm7NewConfPop>.84)
    #dp <- df %>% dplyr::filter(dt7rm7NewConfPop<1.20, dt7rm7NewConfPop>.85)
    
    ggplot(dp, aes(x=Date, y=dt7rm7NewConfPop, color=Region, shape=Region))+
      cwmSpreadStyle(rbsPastTime=input$rbsPastTime, inRegions=inRegions) +
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
    logMsg("  output$ggpExpDateConfPop: renderPlot", sessionID)
    
    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    ggplot(de.regions(), aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(rbsPastTime=5, cbLogScale=input$cbLogScale, inRegions=inRegions) +
      geom_point(size=2)+geom_line()+
      geom_line(aes(y=modrm7NewConfPop)) +
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: TagesInzidenz: Positiv getestete pro Tag pro 100.000 Einwohner.  Basisdaten: AGES"))
  })

  output$ggpExpDatedt7ConfPop <- renderPlot({
    logMsg("  output$ggpExpDatedt7ConfPop: renderPlot", sessionID)
    
    xLimMin <- .9
    xLimMax <- 100
    yLimMin <- 0.84
    yLimMax <- 1.19
    dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)

    input$abUpdate
    inRegions <- isolate(input$cbgRegion)
    dp <- de.regions()  %>% dplyr::filter(dt7rm7NewConfPop<1.19, dt7rm7NewConfPop>.84)
    
    ggplot(dp, aes(x=Date, y=dt7rm7NewConfPop, color=Region, shape=Region))+
      cwmSpreadStyle(rbsPastTime=5, inRegions=inRegions) +
      #scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right") +
      geom_line(size=.75) +
      geom_point(size=2) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Ausbreitungsgeschwindigkeit in % pro Tag.  Basisdaten: AGES"))
  })
  
  output$ggpExpConfPopdt7ConfPop <- renderPlot({
    logMsg("  output$ggpExpConfPopdt7ConfPop: renderPlot", sessionID)
    
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
  
  options(warn=oldWarn)
}


# start shiny app
shinyApp(ui = ui, server = server, options=list(launch.browser = TRUE, width=1650, height=1024))


