options(error = function() traceback(2))
options(shiny.reactlog = TRUE)

# do some logging
logDir = "./log"
logFile <- "cwm.rshiny.log"
logMsg <- function(msg, sessionID="_global_") {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"))
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
logMsg("Define cron job for data retrieval from AGES")
cronJobDir <- "/srv/shiny-server/COVID-19-WeatherMap"
#cronJobDir <- "/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny"
cronJobFile <- paste0(cronJobDir,"/cron.R")
cronJobLog <-paste0(cronJobDir,"/log/cwm.cron.log")  
cmd <- cron_rscript(rscript=cronJobFile, rscript_log=cronJobLog, log_timestamp=TRUE, workdir=cronJobDir)
cmd
cron_clear(ask=FALSE)
cron_add(cmd, frequency='daily', id='AGES-15', at = '14:41')
cron_add(cmd, frequency='daily', id='AGES-21', at = '22:22')

# -----------------------------------------------------------
# Reactiv File Poller: Monitor for new files created by cron
# -----------------------------------------------------------
cwmStatesFile <- "./data/COVID-19-CWM-AGES-States-Curated.rda"
df.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmStatesFile,
  readFunc=readRDS
)
df <- eventReactive(df.rfr(), {
  logMsg(paste("eventReactive: reactiveFileReader for", cwmStatesFile)) 
  df.rfr()} )

cwmCountiesFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"
dg.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmCountiesFile,
  readFunc=readRDS
)
dg <- eventReactive(dg.rfr(), {
  logMsg(paste("eventReactive: reactiveFileReader for", cwmStatesFile)) 
  dg.rfr()} )


cwmTestFile <- "./data/COVID-19-CWM-AGES-Test-Curated.rda"
cwmTestFile <- "./data/test.csv"
dt.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=1000,
  filePath=cwmTestFile,
  readFunc=read.csv
)
dt <- eventReactive(dt.rfr(), {
  logMsg(paste("eventReactive: reactiveFileReader for", cwmTestFile)) 
  dt.rfr()} )

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
      
      p("CWM-V0.4.0-20210122"),
#      tableOutput("secTime"),
      
      fluidRow(
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
            selected = c("Österreich","Wien","Oberösterreich","Kärnten"))),

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
            selected="18")),

      fluidRow(
        checkboxInput("cbLogScale", label="LogScale", value=TRUE, width="220px")),
      
      
        fluidRow(        
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
                     selected="2")),
      

  ),

        #checkboxInput("cbLines", label="PointsOnly", value=FALSE, width="220px"),
        #checkboxInput("cbRegression", label="Regression", value=FALSE, width="220px")),

    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",
 
#        tabPanel("Test",
#                 fluidRow(plotOutput(outputId = "testID"))),

        tabPanel("COVID Lage und Aussichten",
                 h4("Lage und Aussichten Bundesländer", align = "left", style="color:gray"),
                 p("[Menüauswahl: NA]", align = "left", style="color:green"),
                 fluidRow(column(width=9, leafletOutput(outputId = "lftWeatherMap", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpWeatherMap")))),
                  
        tabPanel("Inzidenz Prognose",
                 h4("Progno  se TagesInzidenz", align = "left", style="color:gray"),
                 p("[Menüauswahl: NA]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpIncidencePrediciton", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpIncidencePrediction")))),

        tabPanel("Inzidenz Bundesländer",
                  h4("TagesInzidenz Bundesländer", align = "left", style="color:gray"),
                  p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                  fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceStates", height="75vh")),
                           column(width=3, htmlOutput(outputId="hlpIncidenceStates")))),

          tabPanel("Inzidenz Bezirke",
                 h4("TagesInzidenz Bezirke", align = "left", style="color:gray"),
                 p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceCounties", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpIncidenceCounties")))),
        

        tabPanel("Ausbreitungsgeschwindigkeit",
                 h4("Änderung der TagesInzidenz in % vom Vortag", align = "left", style="color:gray"),
                 p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpChangeRateStates", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpChangeRateStates")))),
        
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
                 p("[Menüauswahl: NA]", align = "left", style="color:green"),
                 htmlOutput(outputId="manDescription"))
      )
    )
  )
)

#        tabPanel("Messwerte Verlauf",
#                 h4("Zeitreihe der Messwerte einer Sensorart von allen Messstationen", align = "left", style="color:gray"),
#                 p("[Menüauswahl: Zeitbereich,Sensor]", align = "left", style="color:green"),
#                 plotOutput(outputId = "CovidPlot")),
#                 plotOutput("sensorTimeSeriesPO", height = "75vh")),


# --------------------------------------------------------------------------------------------------------------------------
# GUI definition
# --------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  # identify session
  sessionID = substr(session$token,1,8)
  logMsg("Server called ...", sessionID)
  
  # logMsg("Defining reactive sensorID Filter", sessionID)
  df.past <- reactive({
    logMsg("  Reactive: df.past: Filtering for rbsPastTime", sessionID)
    return(df() %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })
  
  dg.past <- reactive({
    logMsg("  Reactive: dg.past: Filtering for rbsPastTime", sessionID)
    return(dg() %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })
  
  # -------------------------------------------
  # Test
  # -------------------------------------------
#  output$testID <- renderPlot({
#    logMsg("  output$testID: renderPlot", sessionID)
#    ggplot(dt(), aes(x=x,y=y))+geom_point(size=5)
#  })
  
#  st <- reactive({ invalidateLater(1000)
#    as.character(format(Sys.time(), "%H:%H:%S")) })
#  output$secTime <- renderText({ st() }) 
  
  
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
  nWeatherForeCastDays=14
  dp <- df.past() %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7"))# Today
  de <- df.past() %>% dplyr::filter(Date>=max(Date)-days(nWeatherForeCastDays)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7")) # Past days for forecast
  dm <- cwmAgesRm7EstimatePoly(de,nModelDays=nWeatherForeCastDays,nPredDays=7) %>%
    dplyr::filter(Date==max(Date))

  pMapNUTS <- mapNUTS %>% 
    dplyr::left_join(dm, by="Region") %>%
    dplyr::left_join(dp, by="Region", suffix = c(".f", ".c")) %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.c,binForeCast), 
                  idxDblConfPop=.bincode(dt7rm7NewConfPop,binDblDays), 
                  idxForConfPop=.bincode(rm7NewConfPop.f,binForeCast))
  
  labWeatherMap <- sprintf(
    "<strong>%s</strong><br/>TagesInzidenz: %g pro 100000<br>Änderung seit letzer Woche: %g%%<br>Prognose nächste Woche: %g<br>Tage bis LockDown: %g",
    pMapNUTS$Region, round(pMapNUTS$rm7NewConfPop.c,2), round(pMapNUTS$dt7rm7NewConfPop,2), round(pMapNUTS$rm7NewConfPop.f,2), 0) %>% lapply(htmltools::HTML)
  
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
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)
    
    # dk <- df.past()
    dk <- df.past() %>% dplyr::filter(Region %in% input$cbgRegion)
    dp <- cwmAgesRm7EstimatePoly(dk, nModelDays=input$sldModelDays, nPoly=as.integer(input$rbsModelOrder), nPredDays=14)

    trans <- ifelse(input$cbLogScale, "log10", "identity")
    if(as.integer(input$rbsPastTime)<26) {
      rvBreaks="1 weeks"
      rvLabels="%d.%m"
    } else {
      rvBreaks="1 months"
      rvLabels="%B"
    }
    
    popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,100,by=10),120,150,200,300,400,500)
    
    ggplot(data=dp, aes(x=Date, y=rm7NewConfPop,color=Region,shape=Region)) + 
      scale_shape_manual(values=c(1:10)) + 
      scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, limits=c(min(dk$Date), max(dp$Date)), expand=expand_scale(mult=0.02)) +
      scale_y_continuous(limits=c(1,yLimMax), breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.01), trans=trans, 
                         name="TagesInzidenz: PositiveGetestete/100.000 Einwohnern.") + 
      geom_line(data=dk, aes(x=Date, y=1), size=1.0, color="green") +
      geom_line(data=dk, aes(x=Date, y=2), size=1.0, color="orange") +
      geom_line(data=dk, aes(x=Date, y=4), size=.8, color="magenta") +
      geom_line(data=dk, aes(x=Date, y=8), size=.8, color="red") +
      geom_line(data=dk, aes(x=Date, y=16), size=.8, color="darkred") +
      geom_line(data=dk, aes(x=Date, y=32), size=.8, color="black") +
      geom_line(data=dk, aes(x=Date, y=64), size=1.0, color="black") +
      geom_line(data=dk, aes(x=Date, y=128), size=1.5, color="black") +
      geom_line(data=dp, aes(x=Date, y=1), size=1.0, color="green") +
      geom_line(data=dp, aes(x=Date, y=2), size=1.0, color="orange") +
      geom_line(data=dp, aes(x=Date, y=4), size=.8, color="magenta") +
      geom_line(data=dp, aes(x=Date, y=8), size=.8, color="red") +
      geom_line(data=dp, aes(x=Date, y=16), size=.8, color="darkred") +
      geom_line(data=dp, aes(x=Date, y=32), size=.8, color="black") +
      geom_line(data=dp, aes(x=Date, y=64), size=1.0, color="black") +
      geom_line(data=dp, aes(x=Date, y=128), size=1.5, color="black") +
      geom_line(linetype=2, size=1) + 
      geom_point(data=dk%>%dplyr::filter(Date==max(Date)),size=6) + 
      geom_point(data=dp%>%dplyr::filter(Date==max(Date)),size=6) + 
      geom_point(data=dk,aes(x=Date,y=rm7NewConfPop), size=3) + 
      geom_line(data=dk,aes(x=Date,y=rm7NewConfPop), size=.5) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Prognose TagesInzidenz. Model ab ", min(dp$Date), ".  Basisdaten: AGES"))
  })
  
  
  # -------------------------------------------
  # TagesInzidenz BundesLänder
  # -------------------------------------------
  output$hlpIncidenceStates <- renderText({ htmlIncidenceStates })

  output$ggpIncidenceStates <- renderPlot({
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)

    dp <- df.past() %>% dplyr::filter(Region %in% input$cbgRegion)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(input$rbsPastTime, input$cbLogScale) +
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
    
    dp <- dg.past() %>% dplyr::filter(newConfPop>0, Region %in% input$cbgRegion)
    
    ggplot(dp, aes(x=Date, y=newConfPop, group=CountyID))+
      cwmConfPopStyle(input$rbsPastTime, input$cbLogScale, yLimits=c(.5,256)) +
      geom_line(size=.25, aes(color=Region))+
      ggtitle(paste0("COVID-19 Österreich, Bundesländer und Bezirke: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # ChangeRateStates
  # -------------------------------------------
  output$hlpChangeRateStates <- renderText({ htmlChangeRateStates })
  
  output$ggpChangeRateStates <- renderPlot({
    logMsg("  output$ggpChangeRateStates: renderPlot", sessionID)
    
    dp <- df.past() %>% dplyr::filter(Region %in% input$cbgRegion) %>% dplyr::filter(dt7rm7NewConfPop<1.19, dt7rm7NewConfPop>.84)
    #dp <- df %>% dplyr::filter(dt7rm7NewConfPop<1.20, dt7rm7NewConfPop>.85)
    
    ggplot(dp, aes(x=Date, y=dt7rm7NewConfPop, color=Region, shape=Region))+
      cwmSpreadStyle(input$rbsPastTime) +
      geom_line(size=.75) +
      geom_point(size=2) + 
      geom_point(data=dp %>% dplyr::filter(Date==max(Date)), size=4)+
      ggtitle(paste0("COVID-19 Österreich und Bundesländer: Ausbreitungsgeschwindigkeit in % pro Tag, seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })
  
  
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


