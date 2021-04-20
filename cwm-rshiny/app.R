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

#if(0==1) {
#  logMsg("Starting service cron")
#  system2("sudo","service cron start")
#}

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

  return(df.rfr() %>%
    dplyr::mutate(RegionID=as.character(RegionID)) %>%
    dplyr::select(Date,Region,RegionID,Population, starts_with("rma"), starts_with("rm7NewConf"), rm7NewTested, newConfPop, dt7rm7NewConfPop))
})

# Data for log-linear model for weathermap predictions
df.predict <- eventReactive(df(), {
  logMsg(paste("eventReactive reactiveFileReader:df.predict", cwmStatesFile)) 
  
  return(df() %>% 
           dplyr::select(Date, Region, RegionID, rm7NewConfPop, rm7NewConfirmed, rmaNewConfPop, rm7NewTested, rm7NewConfTest) %>% 
           dplyr::filter(Date>max(Date)-days(nModelDaysPredictionPast)))
})

# Data for log-linear model for weathermap predictions
df.model <- eventReactive(df(), {
  logMsg(paste("eventReactive reactiveFileReader:df.model", cwmStatesFile)) 
  
  dx <- df() %>% 
    dplyr::filter(Date>max(Date)-days(nModelDaysPrediction)) %>% 
    dplyr::select(Date, Region, rm7NewConfPop, rmaNewConfPop)
  dy <- cwm.model(dx=dx, nPoly=nModelPolyGrade, nModelDays=nModelDaysPrediction, dg=datATRegions, locID="Region", colID="RegionS")
  return(dy)
})

# enriched geomap data for WeatherMap of Bundesländer leaflet plot
df.map <- eventReactive(df.model(), {
  logMsg(paste("eventReactive reactiveFileReader:map", cwmStatesFile)) 

  # Conjugate model data frame and add some geoRegion attributes
  dm <- data.frame(t(df.model() %>% dplyr::select(-Inzidenz)), stringsAsFactors=FALSE) %>% 
    dplyr::mutate(RegionS=rownames(.)) %>%
    dplyr::mutate(Date=as.Date(as.numeric(Date))) %>%
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
  return(dg.rfr() %>% 
    dplyr::select(Date, Region, RegionID, County, CountyID, CountyNR, Population, starts_with("rm7"),starts_with("rma")) %>%
    dplyr::mutate(rmaNewConfPop=rmaNewConfPop, rmaNewConfirmed=rmaNewConfirmed))
})

dg.model <- eventReactive(dg(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.model", cwmCountiesFile)) 
  
  dx <- dg() %>% 
    dplyr::filter(Date>max(Date)-days(nModelDaysPrediction)) %>% 
    dplyr::select(Date, Region, RegionID, County, CountyID, CountyNR, Population, rm7NewConfPop, rmaNewConfPop)
  dy <- cwm.model(dx=dx, nPoly=1, nModelDays=nModelDaysPredictionCounties, dg=datATCounties, locID="CountyID", colID="CountyID")
  return(dy)
})

# enriched geomap data for WeatherMap of Bundesländer leaflet plot
dg.map <- eventReactive(dg.model(), {
  logMsg(paste("eventReactive reactiveFileReader:dg.map", cwmCountiesFile)) 
  
  # Conjugate model data frame and add some geoRegion attributes
  dm <- data.frame(t(dg.model() %>% dplyr::select(-Inzidenz)), stringsAsFactors=FALSE)  %>% 
    dplyr::mutate(CountyID=rownames(.)) %>%
    dplyr::mutate(Date=as.Date(Date)) %>%
    dplyr::select(Date, CountyID, rmaNewConfPop, starts_with("rm7NewConfPop"), dtDay, ends_with("Days"))
  
  # Add prediction data to geoMap data
  pMapATCounties <- mapATCounties %>% 
    dplyr::left_join(dm, by="CountyID") %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.0,binForeCast), 
                  idxForConfPop=.bincode(rm7NewConfPop.7,binForeCast),
                  idxMonConfPop=.bincode(rm7NewConfPop.28,binForeCast),
                  idxDblConfPop=.bincode(dtDay,binDblDays))
  
  return(pMapATCounties)
})


# ---------------------------------------------------------------------------------
# AGES-TestedEvaluated: Reactive File Poller: Monitor for new files created by cron
# ---------------------------------------------------------------------------------
cwmTestedEvaluatedFile <- "./data/COVID-19-CWM-AGES-TestedEvaluated.rda"
do.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmTestedEvaluatedFile,
  readFunc=readRDS
)
# complete timeframe
do <- eventReactive(do.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:do", cwmTestedEvaluatedFile)) 
  
  return(do.rfr() %>%
    select(DateEvaluated, Region, newConfirmed, daysDayN) %>% 
    dplyr::mutate(NachTragTag=factor(daysDayN, levels=(nSettleDays:1)-1)) %>%
    dplyr::group_by(Region, DateEvaluated) %>%
    dplyr::mutate(diffConfirmed=newConfirmed-lag(newConfirmed, default=0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(DateEvaluated>(max(DateEvaluated,na.rm=TRUE)-days(nSettleDays)-weeks(nCalcWeeks))))
})


# ---------------------------------------------------------------------------------
# BMSGPK-Dashboard: Reactive File Poller: Monitor for new files created by cron
# ---------------------------------------------------------------------------------
cwmBMSGPKDashboardFile <- "./data/COVID-19-CWM-BMSGPK-Dashboard.curated.rda"
dk.rfr <- reactiveFileReader(
  session=NULL,
  intervalMillis=10000,
  filePath=cwmBMSGPKDashboardFile,
  readFunc=readRDS
)
# complete timeframe
dk <- eventReactive(dk.rfr(), {
  logMsg(paste("eventReactive reactiveFileReader:dk", cwmBMSGPKDashboardFile)) 
  
  return(dk.rfr())
})

#dg.mapx <- eventReactive(dg(), {
#  logMsg(paste("eventReactive reactiveFileReader:dg.map", cwmCountiesFile)) 
#  dg() %>% dplyr::filter(Date>max(Date)-days(nWeatherForeCastDaysCountyMonth)) })

#dg.today <- eventReactive(dg(), {
#  logMsg(paste("eventReactive reactiveFileReader:dg.today", cwmCountiesFile)) 
#  dg() %>% dplyr::filter(Date==max(Date)) %>% 
#    dplyr::mutate(rm7NewConfPop=round(rm7NewConfPop,1), TagesInzidenzAGES=round(rmaNewConfPop,1)) %>%
#    dplyr::select(Date, Region, County, Population, TagesInzidenz=rm7NewConfPop, TagesInzidenzAGES)})

# ---------------------------------------------------------------------------------
# Prediction
# ---------------------------------------------------------------------------------
#cwmPredictionFile <- "./data/COVID-19-CWM-AGES-Prediction.rda"
#wm.rfr <- reactiveFileReader(
#  session=NULL,
#  intervalMillis=10000,
#  filePath=cwmPredictionFile,
#  readFunc=readRDS
#)

# ---------------------------------------------------------------------------------
# Mutations
# ---------------------------------------------------------------------------------
#cwmStatesMutations <- "./data/COVID-19-CWM-AGES-States-Mutations.rda"
#du.rfr <- reactiveFileReader(
#  session=NULL,
#  intervalMillis=10000,
#  filePath=cwmStatesMutations,
#  readFunc=readRDS
#)

# complete timeframe
#du <- eventReactive(du.rfr(), {
#  logMsg(paste("eventReactive reactiveFileReader:du", cwmStatesMutations)) 
#  
#  du.rfr()
#})



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
    sidebarPanel(
      p("CWM-V1.1.0-20210404"),
      
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
            selected = c("Wien","Burgenland","Vorarlberg"))),
        column(6,
          actionButton("abUpdate", "Anzeigen"))),

        fluidRow(
          checkboxInput("cbLogScale", label="StufenModell", value=TRUE, width="220px")),
        
      fluidRow( 
        sliderInput("sldPastTime",
                    width="220px",
                    label="ZeitRaum (letzte n Monate)",
                    min=1, max=12, step=1, value=8)),
      
        fluidRow( 
          hr(style = "border-top: 3px solid #777777;"),
          sliderInput("sldModelDays",
                       width="220px",
                       label="Prognose: BerechnungsTage",
                       min=7, max=63, step=7, value=42)),
      fluidRow(        
        radioButtons("rbsModelOrder",
                     width="220px",
                     label="Prognose: BerechnungsModel",
                     choices = list("Linear (Gerade)" = "1",
                                    "Quadratisch (Parabel)" = "2"),
                     selected="2")),
      width=2
  ),

#                  fluidRow(column(width=12, htmlOutput(outputId="htmlFrontPageNews"))),

    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",

        tabPanel("FrontPage",
                 h4("COVID-19 Wetterkarte und StufenModell", align = "left", style="color:gray"),
                 p("[Menüauswahl: StufenModell]", align = "left", style="color:green"),
                 fluidRow(column(width=6, htmlOutput(outputId="htmlFrontPageTop")),
                          column(width=6, leafletOutput(outputId = "lftFrontPage", height="50vh"))),
                 p("", align = "center", style="color:green"),
                 fluidRow(column(width=6, plotOutput(outputId="ggpFrontPage", height="50vh")),
                          column(width=6, htmlOutput(outputId = "htmlFrontPageBot")))),
                 
       tabPanel("Bundesländer",
                 h4("Lage und Aussichten Bundesländer", align = "left", style="color:gray"),
                 p("[Menüauswahl: keine]", align = "left", style="color:green"),
                 fluidRow(column(width=9,  leafletOutput(outputId = "lftWeatherMap", height="60vh"),
                                          DT::dataTableOutput(outputId = "dtoWeatherMap")),
                         column(width=3, htmlOutput(outputId="hlpWeatherMap")))),
 
       tabPanel("Bezirke",
                h4("Lage und Aussichten Bezirke", align = "left", style="color:gray"),
                p("[Menüauswahl: keine]", align = "left", style="color:green"),
                fluidRow(column(width=9,  leafletOutput(outputId = "lftWeatherMapCounties", height="60vh"),
                                          DT::dataTableOutput(outputId = "dtoWeatherMapCounties")),
                         column(width=3, htmlOutput(outputId="hlpWeatherMapCounties")))),
                                  
        tabPanel("Prognose",
                 h4("Prognose TagesInzidenz", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,StufenModell, BerechnungsTage,BerechnungsModell]", align = "left", style="color:green"),
                 fluidRow(column(width=8, plotOutput(outputId = "ggpIncidencePrediciton", height="75vh")),
                          column(width=4, htmlOutput(outputId="hlpIncidencePrediction")))),

        tabPanel("Inzidenz Bundesländer",
                  h4("TagesInzidenz Bundesländer", align = "left", style="color:gray"),
                  p("[Menüauswahl: Region,Zeitbereich,StufenModell]", align = "left", style="color:green"),
                  fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceStates", height="75vh")),
                           column(width=3, htmlOutput(outputId="hlpIncidenceStates")))),

          tabPanel("Inzidenz Bezirke",
                 h4("TagesInzidenz Bezirke", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,Zeitbereich,StufenModell]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpIncidenceCounties", height="75vh"),
                                          DT::dataTableOutput(outputId = "dtoIncidenceCounties")),
                          column(width=3, htmlOutput(outputId="hlpIncidenceCounties")))),
        
        tabPanel("Geschwindigkeit",
                 h4("Änderung der TagesInzidenz in % vom Vortag", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region,Zeitbereich]", align = "left", style="color:green"),
                 fluidRow(column(width=9, plotOutput(outputId = "ggpChangeRateStates", height="75vh")),
                          column(width=3, htmlOutput(outputId="hlpChangeRateStates")))),
       
       tabPanel("Einmeldungen",
                h4("Anzahl der täglichen Einmeldungen und späteren Nachträge für den Tag der Testung", align = "left", style="color:gray"),
                p("[Menüauswahl: keine]", align = "left", style="color:green"),
                fluidRow(column(width=12, plotOutput(outputId = "ggpTestedEvaluated", height="75vh")))),

       tabPanel("Gesundheitsministerium",
                h4("Historische Daten aus dem tagesaktuellen Dashboard des Gesundheitsministeriums", align = "left", style="color:gray"),
                p("[Menüauswahl: Österreich Testet: Region. Spitalsbelegung: StufenModell]", align = "left", style="color:green"),
                fluidRow(column(width=12, 
                                plotOutput(outputId = "ggpBmsgpkCTAP", height="60vh"),
                                plotOutput(outputId = "ggpBmsgpkCHIR", height="60vh")))),
       
              
#        tabPanel("Mutationen",
#                 h4("Britische (B.1.1.7, N501Y-V1), Afrikanische (B.1.351, N501Y-V2) und deren Stamm Mutation (N501Y)", align = "left", style="color:gray"),
#                 p("[Menüauswahl: keine]", align = "left", style="color:green"),
#                 fluidRow(column(width=12, plotOutput(outputId = "ggpMutations", height="75vh")))),
                          
        tabPanel("Rückblick 2020",
                 h4("Exponentielles Wachstum in zweiten Halbjahr 2020", align = "left", style="color:gray"),
                 p("[Menüauswahl: Region, StufenModell]", align = "left", style="color:green"),
                 fluidRow(column(width=11, 
                                 plotOutput(outputId = "ggpExpDateConfPop", height="60vh"),
                                 plotOutput(outputId = "ggpExpDatedt7ConfPop", height="60vh"),
                                 plotOutput(outputId = "ggpExpConfPopdt7ConfPop", height="60vh")),
                          column(width=1, htmlOutput(outputId="hlpExponential")))),

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

#        tabPanel("SessionData",
#                 h3("Parsed query string"),
#                 verbatimTextOutput("queryText"),
#                 h3("URL components"),
#                 verbatimTextOutput("sessionText"),
#                 h3("EnvVars"),
#                 verbatimTextOutput("envvarText"))
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
    return(df() %>% dplyr::filter(Date > (max(Date, na.rm=TRUE)-months(as.integer(input$sldPastTime)))))
  })
  
  
  # counties by Time
  dg.past <- reactive({
    logMsg(" reactive dg.past sldPastTime", sessionID)
    return(dg() %>% dplyr::filter(Date > (max(Date, na.rm=TRUE)-months(as.integer(input$sldPastTime)))))
  })

  # state history by regions
  de.regions <- reactive({
    logMsg(" reactive de.regions cbgRegion", sessionID)
    input$abUpdate
    return(de() %>% dplyr::filter(Region %in% isolate(input$cbgRegion)))
  })
 
 
  # -------------------------------------------
  # FrontPage
  # -------------------------------------------
  output$htmlFrontPageNews <- renderText({ htmlFrontPageNews })
  output$htmlFrontPageTop <- renderText({ htmlFrontPageTop })
  output$htmlFrontPageBot <- renderText({ htmlFrontPageBot })
  
  output$lftFrontPage <- renderLeaflet({
    logMsg("  output renderPlot lftFrontPage", sessionID)
    options(warn=-1)

    # Depend on reactive file reader
    pMapNUTS <- df.map()
    
    curZoom=6
    # Depend on reactive file reader
    leaflet(pMapNUTS, options=leafletOptions(minZoom=curZoom, maxZoom=curZoom, zoomControl=FALSE, dragging=FALSE, zoom=curZoom)) %>%
      addTiles(group="DefaultMap", options=providerTileOptions(minZoom=curZoom, maxZoom=curZoom)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.0,binConfPop)]) %>%
      addMarkers(lng=~cxNUTS, lat=~cyNUTS, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addLabelOnlyMarkers(lng=11, lat=48.5, label=paste("Daten mit Stand von",format(pMapNUTS$Date[1],"%a., %d. %b %Y")), 
                          labelOptions = labelOptions(noHide=T, direction='top', textsize='9pt', style=list('color'='white', 'background'='#666666'))) %>%
      setView(lng=pMapNUTS$cxNUTS[1]-3, lat=pMapNUTS$cyNUTS[1], zoom=curZoom)
  })
  
  output$ggpFrontPage <- renderPlot({
    logMsg("  output renderPlot ggpFrontPage", sessionID)
    options(warn=-1)
    
    popSteps=c(1,2,4,8,16,32,64,128)
    dp <- df() %>% dplyr::filter(Date>as.Date("2020-07-04"), Region=="Österreich") %>% dplyr::select(Date, Region, rm7NewConfPop)
    trans <- ifelse(input$cbLogScale, "log10", "identity")
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(sldPastTime=6, cbLogScale=TRUE, inRegions="Österreich") +
      scale_x_date(date_breaks="1 months", date_labels="%b") +
      scale_y_continuous(limits=c(1,128), breaks=popSteps, position="right",  trans=trans, name="TagesInzidenz",
                         sec.axis=dup_axis(name="WochenInzidenz", labels=as.character(popSteps*7))) +
      theme(legend.position="none") +
      geom_point(size=1, col="red")+geom_line(col="red", size=1)+
      ggtitle(paste0("COVID-19 Österreich:  Inzidenz: Stufenmodell seit ", min(dp$Date), ".   Basisdaten: AGES"))
  })
  
  # -------------------------------------------
  # Weather Map BundesLänder
  # -------------------------------------------
  output$hlpWeatherMap <- renderText({ htmlWeatherMap })

  flts <- c("Date", "ÄnderungVortag", "dblDays")
  #flts <- "foo"
  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoWeatherMap <- DT::renderDataTable({ df.model() %>% 
      dplyr::mutate(ID=(1:n())-1) %>% 
      dplyr::filter(! Inzidenz %in% flts) }, options=list(pageLength=20, dom='t'))
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
          <tr><td>Tage Inzidenz %s (linear):  </td><td align='right'> %g </td></tr>
        </table>",
      pMapNUTS$Region, format(pMapNUTS$Date,"%a, %d.%m"),
      round(pMapNUTS$rmaNewConfPop,1),  nModelDaysPrediction, 
      round(pMapNUTS$rm7NewConfPop.0,1), round(pMapNUTS$rm7NewConfPop.7,1), round(pMapNUTS$rm7NewConfPop.28,1),
      ifelse(pMapNUTS$dblDays>0,"Verdoppelung","Halbierung"), round(abs(pMapNUTS$dblDays))) %>% lapply(htmltools::HTML)
  
    leaflet(pMapNUTS, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)) %>%
      addTiles(group="DefaultMap", options=providerTileOptions(minZoom=7, maxZoom=7)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.0,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Stand Heute",
                  highlight = highlightOptions(weight=5, bringToFront=FALSE)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.7,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Prognose kommende Woche",
                  highlight = highlightOptions(weight=5, bringToFront=FALSE)) %>%
      addPolygons(stroke=TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapNUTS$rm7NewConfPop.28,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Prognose kommendes Monat",
                  highlight = highlightOptions(weight=5, bringToFront=FALSE)) %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), options=layersControlOptions(collapsed=FALSE)) %>%
      addMarkers(lng=~cxNUTS-.35, lat=~cyNUTS, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addMarkers(lng=~cxNUTS, lat=~cyNUTS, icon=~iconsDirection[idxDblConfPop], group="Trend",
                 label=labWeatherMap, labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px")) %>%
      addMarkers(lng=~cxNUTS+.35, lat=~cyNUTS, icon=~iconsWeather[idxForConfPop], group="ForeCast") %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.0, position="bottomright", opacity=1, title="Incidence") %>%
      addLabelOnlyMarkers(lng=11, lat=48.5, label=paste("Daten mit Stand von",format(pMapNUTS$Date[1],"%a., %d. %b %Y")), 
                          labelOptions = labelOptions(noHide=T, direction='top', textsize='10pt', style=list('color'='white', 'background'='#444444'))) %>%
      setView(lng=pMapNUTS$cxNUTS[1]-3, lat=pMapNUTS$cyNUTS[1], zoom=7)
  })

  # -------------------------------------------
  # Weather Map Bezirke
  # -------------------------------------------
  output$hlpWeatherMapCounties <- renderText({ htmlWeatherMapCounties })
  
  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoWeatherMapCounties <- DT::renderDataTable({ dg.map()@data %>% 
                                      dplyr::mutate(ID=1:n()) %>%
                                      dplyr::mutate(TageDplt =ifelse(dblDays>0,dblDays,NA)) %>%
                                      dplyr::mutate(TageHalb =ifelse(dblDays<0,-dblDays,NA)) %>%
                                      dplyr::mutate(Heute=round(rm7NewConfPop.0,1), Woche=round(rm7NewConfPop.7,1), Monat=round(rm7NewConfPop.28,1), ProzVortag=round((dtDay-1)*100,2)) %>%
                                      dplyr::rename(AGES=rmaNewConfPop) %>%
                                      dplyr::select(Region,County,AGES, Heute, Woche, Monat,
                                                    ProzVortag, TageDplt, TageHalb) }, options=list(pageLength=94, dom='t'))
  
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
          <tr><td><small><i><b>Lineares Modell aus den letzten %s Tagen</b></i></small></td> </tr>
          <tr><td>Inzidenz heute: </td><td align='right'> %g</td></tr>
          <tr><td>Inzidenz kommende Woche: </td><td align='right'>  %g </td></tr>
          <tr><td>Inzidenz nächstes Monat: </td><td align='right'>  %g </td></tr>
          <tr><td>Tage bis Inzidenz %s:  </td><td align='right'> %g </td></tr>
        </table>",
      pMapCounties$County, format(pMapCounties$Date,"%a, %d.%m"),
      round(pMapCounties$rmaNewConfPop,1), 
      nModelDaysPredictionCounties,
      round(pMapCounties$rm7NewConfPop.0,1), round(pMapCounties$rm7NewConfPop.7,1), round(pMapCounties$rm7NewConfPop.28),
      ifelse(pMapCounties$dblDays>0,"Verdoppelung","Halbierung"), abs(round(pMapCounties$dblDays)))  %>% 
      lapply(htmltools::HTML)
    
    #, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)
    leaflet(pMapCounties, options=leafletOptions(minZoom=7, maxZoom=7, zoomControl=FALSE, dragging=FALSE, zoom=7)) %>%
      addTiles(group="DefaultMap", options=providerTileOptions(minZoom=7, maxZoom=7)) %>%
      #addMapPane("Counties", zIndex=410) %>% 
      #addMapPane("States", zIndex=420) %>%
      # options(pathOptions(pane="Counties")),
      # overlayGroups="Bundesländer",
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.0,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Stand Heute",
                  highlight = highlightOptions(weight=3, bringToFront=FALSE)) %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.7,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Prognose kommende Woche",
                  highlight = highlightOptions(weight=3, bringToFront=FALSE)) %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(pMapCounties$rm7NewConfPop.28,binConfPop)],
                  label=labWeatherMapCounties, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="12px"), group="Prognose kommendes Monat",
                  highlight = highlightOptions(weight=3, bringToFront=FALSE)) %>%
      addPolygons(data=mapATRegions, stroke = TRUE,  weight=3, color="black", smoothFactor = 0, fillOpacity = 0, fillColor="none", group="Bundesländer") %>%
      addLayersControl(baseGroups=c("Stand Heute","Prognose kommende Woche","Prognose kommendes Monat"), 
                        options=layersControlOptions(collapsed=FALSE)) %>%
      addLabelOnlyMarkers(lng=11, lat=48.5, label=paste("Daten mit Stand von",format(pMapCounties$Date[1],"%a., %d. %b %Y")), 
                          labelOptions = labelOptions(noHide=T, direction='top', textsize='10pt', style=list('color'='white', 'background'='#444444'))) %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop.0, position="bottomright", opacity=1, title="Incidence") %>%
      setView(lng=mapNUTSAT$cxNUTS[1]-3, lat=mapNUTSAT$cyNUTS[1], zoom=7)
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
    dk <- df.predict() %>% dplyr::filter(Region %in% inRegions) %>%
      dplyr::mutate(locID=RegionID) # added so cwmAgesRm7EstimatePoly know what to look for (RegionID or CountyID)
    
    # dp <- cwmAgesRm7EstimatePoly(dk, nModelDays=input$sldModelDays, nPoly=as.integer(input$rbsModelOrder), nPredDays=28)
    dp <- cwmAgesRm7EstimatePoly(dk, nPoly=as.integer(input$rbsModelOrder), nModelDays=input$sldModelDays, nPredDays=28)
    
    ggplot(data=dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region)) + 
      cwmConfPopStyle(sldPastTime=1, cbLogScale=input$cbLogScale, inRegions=inRegions, xLimits=c(max(dk$Date)-weeks(8), max(dp$Date)+days(1))) +
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
    
    if (bDebug) logMsg(paste("sldPastTime=",input$sldPastTime, "cbLogScale=",input$cbLogScale, "inRegions=",inRegions))
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region))+
      cwmConfPopStyle(sldPastTime=as.integer(input$sldPastTime), cbLogScale=input$cbLogScale, inRegions=inRegions) +
      geom_point(size=2)+geom_line()+
      geom_point(data=dp %>% dplyr::filter(Date==max(Date)), size=4)+
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # TagesInzidenz Bezirke
  # -------------------------------------------
  output$hlpIncidenceCounties <- renderText({ htmlIncidenceCounties })

  # Values, and Prediction of Incodence, Time to/outof Lockdown
  output$dtoIncidenceCounties <- DT::renderDataTable({ dg.map()@data %>% 
      dplyr::arrange(Region, County) %>%
      dplyr::mutate(ID=(1:n())-1) %>%
      dplyr::rename(AGES=rmaNewConfPop,
                    Heute=rm7NewConfPop.0, Woche=rm7NewConfPop.7, Monat=rm7NewConfPop.28,
                    Änderung=dtDay, TageDoppelt=DblDays, TageHälfte=HalfDays, 
                    Inzidenz8=rm7NewConfPop8, Inzidenz32=rm7NewConfPop32) %>%
      dplyr::select(ID,Region,County,AGES,
                    Heute, Woche, Monat,
                    Änderung, TageDoppelt, TageHälfte, 
                    Inzidenz8, Inzidenz32) }, options=list(pageLength=94, dom='t'))
    
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
  # TestedEvaluated
  # -------------------------------------------
  output$ggpTestedEvaluated <- renderPlot({
    
    colPal <- c("#DDDDDD", "#56B4E9","#E69F00",  "#009E73", "#0072B2", "#D55E00",  "#C40000")[7:1]
    ggplot(data=do(), aes(x=DateEvaluated, y=diffConfirmed)) + 
      theme(panel.grid.major.x = element_line(color = "darkgray", linetype=2), 
            axis.text = element_text(size=10), axis.title.x=element_blank()) +
      geom_col(aes(fill=NachTragTag), width=.95 , color="grey70")+
      scale_fill_manual(values=colPal) +
      scale_color_manual(values=colPal) +
      scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") + 
      facet_wrap(Region~., scales="free_y", nrow=2) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Rückwirkende Verortung der täglichen Fallzahlen (BMSGPK)  zum TestDatum (AGES).  Basisdaten: AGES"))
  })  
 
  # -------------------------------------------
  # bmsgpk Data
  # -------------------------------------------
  output$ggpBmsgpkCHIR <- renderPlot({

    setStatus100k <- c("newConfirmed","curHospital","curICU","newRecovered")
    setFacet <- "Region"
    
    yTrans <- ifelse(input$cbLogScale, "log2","identity")
    
    ggplot(data=dk() %>% dplyr::filter(Date>as.Date("2021-02-01"), Status %in% setStatus100k), aes(x=Date, y=Count, color=Status, shape=Status)) +
      geom_line(aes(y=1), size=0.25, color="white") +
      geom_line(aes(y=2), size=0.25, color="green") +
      geom_line(aes(y=4), size=0.25, color="orange") +
      geom_line(aes(y=8), size=0.25, color="magenta") +
      geom_line(aes(y=16), size=0.5, color="red") +
      geom_line(aes(y=32), size=0.5, color="darkred") +
      geom_line(aes(y=64), size=0.5, color="black") +
      geom_line(size=.5) +
      geom_point(size=1) +
      facet_wrap(as.formula(paste0(setFacet,"~.")), nrow=2)+
      scale_fill_manual(values=cbPalette) +
      scale_color_manual(values=cbPalette) +
      scale_x_date(date_breaks="2 weeks", date_labels="%d.%m") +
      scale_y_continuous(limits=c(1/2,NA), trans=yTrans, breaks=2^(-5:10)) +
      ggtitle("COIVD-19 Österreich: Positive, Spitalsbelegung, Intensivstation und Genesene pro 100.000. Wochenmittel.       Daten: bmsgpk. Datenqualität: schlecht.  Bearbeitung: manuelle 'Korrektur'")
  })  
  
  
  output$ggpBmsgpkCTAP <- renderPlot({
    
    setStatusTesting100k <- c("newTested", "newTested_AG", "newTested_PCR", "relConfTest", "relConfTest_AG", "relConfTest_PCR")
    #setRegionSelect <- c("Burgenland","Vorarlberg","Wien")
    #setRegionSelect <- c("Tirol","Kärnten","Salzburg")
    setRegionSelect <- input$cbgRegion
    setFacet <- "Status"
    
    ggplot(data=dk() %>% dplyr::filter(Date>as.Date("2021-02-01"), Status %in% setStatusTesting100k, Region %in% setRegionSelect), 
           aes(x=Date, y=Count, color=Region, shape=Region)) +
      geom_line(size=.5) +
      geom_point(size=1) +
      facet_wrap(as.formula(paste0(setFacet,"~.")), nrow=2, scales="free_y")+
      scale_fill_manual(values=cbPalette) +
      scale_color_manual(values=cbPalette) +
      scale_x_date(date_breaks="2 weeks", date_labels="%d.%m") +
      scale_y_continuous(limits=c(0,NA), trans="identity") +
      ggtitle("COIVD-19 Österreich Testet. Oben: Anzahl Gesamt, AG und PCR pro 100.000 (1000=1%).  Unten: Prozent Positive Tests. Wochenmittel.     Daten: bmsgpk. Datenqualität: schlecht.  Bearbeitung: manuelle 'Korrektur'")
  })  
   
  # -------------------------------------------
  # Mutationen
  # -------------------------------------------
  output$ggpMutations <- renderPlot({

    dp <- du()
    # dynamic selection of mutations
    mutStates <- levels(factor(dp$Status))
    idx <-  !is.na(str_match(mutStates, "positiv")) | 
      !is.na(str_match(mutStates, "bestätigt"))| 
      !is.na(str_match(mutStates, "gesamt")) |
      !is.na(str_match(mutStates, "Mutation")) 
    
    fltStatus <- mutStates[idx]
    ggplot(data=dp %>% dplyr::filter(Status %in% fltStatus), aes(x=Date, y=Count, color=Status, shape=Status)) + 
      scale_shape_manual(values=atShapes) +
      scale_fill_manual(values=cbPalette) +
      scale_color_manual(values=cbPalette) +
      geom_line() + 
      geom_point(size=3) +
      facet_wrap(.~Region, nrow=2, scales="free_y") +
      scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
      scale_y_continuous()
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
      cwmConfPopStyle(sldPastTime=1, cbLogScale=input$cbLogScale, inRegions=inRegions, stepDate=max(de.regions()$Date)) +
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
  
  
  
  # -------------------------------------------
  # Client Session
  # -------------------------------------------  
  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
  # Return the components of the URL in a string:
  output$sessionText <- renderText({
    cls <- sapply(session, function(a) class(a)[1])
    nms <- names(cls[ cls %in% c("list", "character", "numeric", "integer",
                                 "NULL", "logical", "environment", "reactivevalues" ) ])
    nms <- setdiff(nms, ".__enclos_env__")
    paste(
      capture.output(
        str(
          sapply(nms,
                 function(sessnm) {
                   if (inherits(session[[sessnm]], c("environment", "reactivevalues"))) {
                     sapply(names(session[[sessnm]]), function(nm) session[[sessnm]][[nm]], simplify = FALSE)
                   } else if (inherits(session[[sessnm]], c("character", "numeric", "integer"))) {
                     session[[sessnm]]
                   } else class(session[[sessnm]])
                 }, simplify = FALSE),
          nchar.max = 1e5,
          vec.len = 1e5
        )
      ),
      collapse = "\n"
    )
  })
  # Dump the environment variables
  output$envvarText <- renderText({
    paste(
      capture.output(
        str(as.list(Sys.getenv()))
      ),
      collapse = "\n"
    )
  })

}


# start shiny app
shinyApp(ui = ui, server = server, options=list(launch.browser = TRUE, width=1650, height=1024))


