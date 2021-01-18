options(error = function() traceback(2))

logDir <- Sys.getenv("RSHINY_LOGDIR")
if (logDir=="") logDir = "."

logFile <- Sys.getenv("RSHINY_LOGFILE")
if (logFile=="") logFile <- "itg.rshiny.log"

# The above daoes not work -> hard code log path for now
logDir = "/var/log/itg"
logFile <- "itg.rshiny.log"

# do some logging
logMsg <- function(msg, sessionID="_global_") {
  #cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste(logDir,"/",logFile, sep=""), append=TRUE)
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

source("fun.R")


# --------------------------------------------------------------------------------------------------------------------------
# Global section. Data available to all sessions
# --------------------------------------------------------------------------------------------------------------------------
# Global constants
dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
popBreaks <- c(0,1,2,5,10,15,20,25,seq(30,150,by=10))
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,150,by=10))
popLogBreaks <- (c(.1,.2,.5,1,2,5,10,20,50,100))
logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000))
logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000),seq(10000,100000,by=10000))
#nModelDays=28
#nPredDays=14
yLimMax <- 128
julDate <- as.Date("2020-07-01")
maxDate <- max(df$Date)
trans="log10"


# -----------------------------------------------------
# AGES data files
# -----------------------------------------------------
df <- read.csv("./data/COVID-19-AGES-Curated.csv", sep=",", dec=".") %>%
  dplyr::mutate(Date=as.Date(Date,"%Y-%m-%d")) %>%
  dplyr::select(-Stamp)

di <- data.frame(RegionID=as.character(1:9), Region=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"), stringsAsFactors=FALSE)
dg <- read.csv("./data/COVID-19-AGES-GKZ.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(Date=as.Date(Date,"%Y-%m-%d")) %>%
  dplyr::select(-Stamp) %>%
  dplyr::rename(County=Region, CountyID=RegionID) %>%
  dplyr::mutate(RegionID=(str_sub(as.character(CountyID),1,1)), CountyNR=(str_sub(as.character(CountyID),2,3))) %>%
  dplyr::left_join(di, by="RegionID") %>% dplyr::select(1,13,15,2,14,3,4,5:12)

# -----------------------------------------------------
# Map Austria data structures
# -----------------------------------------------------
iconSizeWeather=25
iconsWeather <- iconList (
  Thunder =   makeIcon(iconUrl="./www/iconWeather-Thunder.png",   iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Rain =      makeIcon(iconUrl="./www/iconWeather-Rain.png",      iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Rain =  makeIcon(iconUrl="./www/iconWeather-Sun-Rain.png",  iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Cloud = makeIcon(iconUrl="./www/iconWeather-Sun-Cloud.png", iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun =       makeIcon(iconUrl="./www/iconWeather-Sun.png",       iconWidth=iconSizeWeather, iconHeight=iconSizeWeather)
)

iconSizeDirection=25
iconsDirection <- iconList (
  dirS =   makeIcon(iconUrl="./www/iconDirection-S.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirESE = makeIcon(iconUrl="./www/iconDirection-SSE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirSSE = makeIcon(iconUrl="./www/iconDirection-ESE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirE =   makeIcon(iconUrl="./www/iconDirection-E.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirENE = makeIcon(iconUrl="./www/iconDirection-ENE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirNNE = makeIcon(iconUrl="./www/iconDirection-NNE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirN =   makeIcon(iconUrl="./www/iconDirection-N.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection)
)

# Austria NUTS poligons for NUTS1, NUTS2 and NUTS3
mapNUTS1 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_1.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
mapNUTS2 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_2.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
mapNUTS3 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_3.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))

# Center of NUTSn boxes
cxNUTS1 = mean(bbox(mapNUTS1)[1,])
cyNUTS1 = mean(bbox(mapNUTS1)[2,])

cxNUTS2 <- vector()
cyNUTS2 <- vector()
for(i in 1:length(mapNUTS2)) {
  cxNUTS2[i] <- mean(bbox(mapNUTS2[i,])[1,])
  cyNUTS2[i] <- mean(bbox(mapNUTS2[i,])[2,])
}



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
      
      p("COVID-19-WeatherMap-0.1.1 IBM@20210115"),
      
#      fluidRow(
#        dateRangeInput(
#          inputId = "DateRange",
#          label = "DateRange",
#          start=as.Date("2020-07-01"),
#          end=as.Date("2021-01-13"),
#          min = as.Date("2020-07-01"),
#          max = as.Date("2021-01-13"),
#          weekstart=1)),
      
      fluidRow(
#        column(width=6,
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
            selected = c("Österreich","Wien"))),

        fluidRow(        
#        column(width=6,
             radioButtons("rbsPastTime",
              width="220px",
              label="Zeitaum",
            choices = list("2 Monate" = "9",
                           "3 Monate" = "13",
                           "4 Monate" = "18",
                           "5 Monate" = "22",
                           "6 Monate" = "26",
                           "12 Monate"= "53"),
            selected="18")),
              
      fluidRow(
        "Options", 
        checkboxInput("cbLines", label="PointsOnly", value=FALSE, width="220px"),
        checkboxInput("cbLogScale", label="LogScale", value=TRUE, width="220px"),
        checkboxInput("cbRegression", label="Regression", value=FALSE, width="220px"))
  ),
  
    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",
 
        tabPanel("COVID Lage und Aussichten",
                 h4("Lage und Aussichten Bundesländer", align = "left", style="color:gray"),
                 p("[Menüauswahl: NA]", align = "left", style="color:green"),
                 fluidRow(column(width=10, leafletOutput(outputId = "lftWeatherMap", height="75vh")),
                          column(width=2, htmlOutput(outputId="hlpWeatherMap")))),
                  
        tabPanel("Prognose TagesInzidenz",
                 h4("Prognose TagesInzidenz", align = "left", style="color:gray"),
                 p("[Menüauswahl: NA]", align = "left", style="color:green"),
                 fluidRow(column(width=10, plotOutput(outputId = "ggpIncidencePrediciton", height="75vh")),
                          column(width=2, htmlOutput(outputId="hlpIncidencePrediciton")))),

        tabPanel("TagesInzidenz Bundesländer",
                  h4("TagesInzidenz Bundesländer", align = "left", style="color:gray"),
                  p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                  fluidRow(column(width=10, plotOutput(outputId = "ggpIncidenceStates")),
                           column(width=2, htmlOutput(outputId="hlpIncidenceStates")))),

          tabPanel("TagesInzidenz Bezirke",
                 h4("TagesInzidenz Bezirke", align = "left", style="color:gray"),
                 p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                 fluidRow(column(width=10, plotOutput(outputId = "ggpIncidenceCounties")),
                          column(width=2, htmlOutput(outputId="hlpIncidenceCounties")))),
        
                
        tabPanel("Rohdaten Bundesländer",
          h4("Rohdaten Bundesländer", align = "left", style="color:black"),
          p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
          DT::dataTableOutput(outputId = "dtoRawDataRegion")),
        
        tabPanel("Rohdaten Bezirke",
                 h4("Rohdaten Bezirke", align = "left", style="color:black"),
                 p("[Menüauswahl: Zeitbereich,Region]", align = "left", style="color:green"),
                 DT::dataTableOutput(outputId = "dtoRawDataCounty"))
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
    return(df %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })
  
  dg.past <- reactive({
    logMsg("  Reactive: dg.past: Filtering for rbsPastTime", sessionID)
    return(dg %>% dplyr::filter(Date > max(Date)-weeks(as.integer(input$rbsPastTime))))
  })
  
  # -------------------------------------------
  # Weather Map
  # -------------------------------------------
  output$hlpWeatherMap <- renderText({
    "<p><b>Help</b></p><p>this is some help text that shall explain the plot</p><p><li>itime1</li><li>listitem 2<</li></p>"
  })
  
  
  #levConfPop <- round(c(0,10^seq(0,2,by=0.2),1000),1)
  # bins for rm7ConfPop
  binConfPop <- c(0,1,1.4,2,2.8,4,5.6,8,11,16,22,32,45,64,90,128,Inf)
  palConfPop <- c("#FFFFFF", brewer.pal(9,"YlOrRd"), "#600026", "#400020", "#200010", "#000000", "#000000","#000000","#000000")
  colConfPop <- colorBin(palette=palConfPop, domain=binConfPop, bins=binConfPop)

  # 7 icons: bins for dt* 
  dblDays <- c(1,14,28,56,-56,-28,-14,-1)
  binDblDays <- sort(round(exp(log(2)/dblDays),3))
  
  # 5 Icons
  binForeCast <- c(0,4,8,16,32,Inf)
  
  # TEST: add some properties to the geojson data structure
  NUTS2_AT <- data.frame(
    NUTS_ID=c("AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34"),
    Region= c("Burgenland","Niederösterreich","Wien","Kärnten", "Steiermark","Oberösterreich","Salzburg","Tirol","Vorarlberg"),
    stringsAsFactors=FALSE)
  
  # today's data for weathermap
  dp <- df %>% dplyr::filter(Date==max(Date))
  
  mapNUTS2 <- mapNUTS2 %>% 
    dplyr::left_join(NUTS2_AT, by="NUTS_ID") %>% 
    dplyr::left_join(dp, by="Region") %>%
    dplyr::mutate(cxNUTS2=cxNUTS2, cyNUTS2=cyNUTS2, 
                  idxCurConfPop=.bincode(rm7NewConfPop,binForeCast), 
                  idxDblConfPop=.bincode(dt7rm7NewConfPop,binDblDays), 
                  idxForConfPop=.bincode(modrm7NewConfPop,binForeCast))
  
  labWeatherMap <- sprintf(
    "<strong>%s</strong><br/>TagesInzidenz: %g pro 100000<br>Änderung seit letzer Woche: %g%%<br>Prognose nächste Woche: %g<br>Tage bis LockDown: %g",
    mapNUTS2$Region, round(mapNUTS2$rm7NewConfPop,2), round(mapNUTS2$dt7rm7NewConfPop,2), round(mapNUTS2$modrm7NewConfPop,2), 0) %>% lapply(htmltools::HTML)
  
  
  output$lftWeatherMap <- renderLeaflet({
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)
    
    leaflet(mapNUTS2) %>%
      addTiles(group="DefaultMap",options = providerTileOptions(minZoom=6, maxZoom=8)) %>%
      setView(lng=cxNUTS1, lat=cyNUTS1, zoom=7) %>%
      addPolygons(data=mapNUTS1, stroke = TRUE, smoothFactor = 0, color="black", fillOpacity = 0, fillColor="None", weight=10, group="AT1") %>%
      addPolygons(data=mapNUTS3, stroke = TRUE, smoothFactor = 0, fillOpacity = 0, fillColor="none", weight=1, group="AT3") %>%
      addPolygons(stroke = TRUE, weight=3, color="black",
                  fill=TRUE, fillOpacity = 1, fillColor=palConfPop[.bincode(mapNUTS2$rm7NewConfPop,binConfPop)],
                  label=labWeatherMap, 
                  labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"),
                  popup=~paste(Region,round(rm7NewConfPop,2),round(dt7rm7NewConfPop,2),round(modrm7NewConfPop,2),sep="\n\r<br>"),
                  group="AT2") %>%
      addMarkers(lng=~cxNUTS2-.25, lat=~cyNUTS2, icon=~iconsWeather[idxCurConfPop], group="Incidence") %>%
      addMarkers(lng=~cxNUTS2, lat=~cyNUTS2, icon=~iconsDirection[idxDblConfPop], group="Trend") %>%
      addMarkers(lng=~cxNUTS2+.25, lat=~cyNUTS2, icon=~iconsWeather[idxForConfPop], group="ForeCast") %>%
      addLegend(pal=colConfPop, values=~rm7NewConfPop, position="bottomright") %>%
      addLayersControl(overlayGroups=c("AT1","AT3"), options=layersControlOptions(collapsed=FALSE)) %>%
      hideGroup(c("AT1","AT3","Markers"))
  })
  
  # -------------------------------------------
  # TagesInzidenz BundesLänder
  # -------------------------------------------
  output$hlpIncidencePrediciton <- renderText({
    "<p><b>IncidencePrediciton</b></p><p>this is some help text that shall explain the plot</p><p><li>itime1</li><li>listitem 2<</li></p>"
  })
  
  output$ggpIncidencePrediciton <- renderPlot({
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)

    dk <- df.past()
    dp <- caAgesRm7EstimatePoly(dk,nModelDays=20,nPredDays=14)

    trans <- ifelse(input$cbLogScale, "log10", "identity")
    if(as.integer(input$rbsPastTime)<26) {
      rvBreaks="1 weeks"
      rvLabels="%d.%m"
    } else {
      rvBreaks="1 months"
      rvLabels="%B"
    }
    
    ggplot(data=dp, aes(x=Date, y=rm7NewConfPop,color=Region,shape=Region)) + 
      scale_shape_manual(values=c(1:10)) + 
      scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, limits=c(min(dk$Date), max(dp$Date)), expand=expand_scale(mult=0.01)) +
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
      geom_point(data=dk,aes(x=Date,y=rm7NewConfPop)) + 
      geom_line(data=dk,aes(x=Date,y=rm7NewConfPop), size=.5) + 
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Prognose TagesInzidenz. Model ab ", min(dp$Date), ".  Basisdaten: AGES"))
  })
  
  
  # -------------------------------------------
  # TagesInzidenz BundesLänder
  # -------------------------------------------
  output$hlpTagesInzidenz <- renderText({
    "<p><b>Help</b></p><p>this is some help text that shall explain the plot</p><p><li>itime1</li><li>listitem 2<</li></p>"
    })

  output$ggpIncidenceStates <- renderPlot({
    logMsg("  output$ggpIncidenceStates: renderPlot", sessionID)

    Regions <- input$cbgRegion
    bSmooth <- input$cbRegression
    bLines <- input$cbRegression
    trans <- ifelse(input$cbLogScale, "log10", "identity")
    if(as.integer(input$rbsPastTime)<26) {
      rvBreaks="1 weeks"
      rvLabels="%d.%m"
    } else {
      rvBreaks="1 months"
      rvLabels="%B"
    }
    
    dp <- df.past() %>% dplyr::filter(Region %in% Regions)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region))+
      theme(panel.grid.major = element_line(color = "darkgray", linetype=3), panel.grid.minor=element_line(color = "gray90", linetype=1)) +
      scale_shape_manual(values=c(1:10)) +
      scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, limits=c(min(dp$Date), maxDate+days(nPredDays*0+2)), expand=expand_scale(mult=0.01)) +
      scale_y_continuous(limits=c(1,yLimMax), breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.01), trans=trans, 
                         name="Positive/100.000 Einwohnern. \nAmpelfarben entlang ECDC (European Centre for Disease Prevention and Control) ") + 
      geom_line(aes(x=Date, y=1), size=1.0, color="green") +
      geom_line(aes(x=Date, y=2), size=1.0, color="orange") +
      geom_line(aes(x=Date, y=4), size=.8, color="magenta") +
      geom_line(aes(x=Date, y=8), size=.8, color="red") +
      geom_line(aes(x=Date, y=16), size=.8, color="darkred") +
      geom_line(aes(x=Date, y=32), size=.8, color="black") +
      geom_line(aes(x=Date, y=64), size=1.0, color="black") +
      geom_line(aes(x=Date, y=128), size=1.5, color="black") +
      geom_point(size=1)+geom_line()+
      #geom_smooth(data=dp%>%dplyr::filter(Date<as.Date("2020-12-01")), aes(x=Date, y=rm7NewConfPop),method="lm", se=FALSE) +
      ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
  })

  
  # -------------------------------------------
  # TagesInzidenz Bezirke
  # -------------------------------------------
  output$hlpIncidenceCounties <- renderText({
    "<p><b>Help</b></p><p>this is some help text that shall explain the plot</p><p><li>itime1</li><li>listitem 2<</li></p>"
  })
  
  output$ggpIncidenceCounties <- renderPlot({
    logMsg("  output$ggpIncidenceCounties: renderPlot", sessionID)
    
    Regions <- input$cbgRegion
    trans <- ifelse(input$cbLogScale, "log10", "identity")
    if(as.integer(input$rbsPastTime)<26) {
        rvBreaks="1 weeks"
        rvLabels="%d.%m"
    } else {
      rvBreaks="1 months"
      rvLabels="%B"
    }
    
    # dp <- dg %>% dplyr::filter(!is.na(newConfPop), newConfPop>0)
    dp <- dg.past() %>% dplyr::filter(newConfPop>0, Region %in% Regions)
    
    ggplot(dp, aes(x=Date, y=newConfPop, group=CountyID))+
      theme(panel.grid.major = element_line(color = "darkgray", linetype=3), panel.grid.minor=element_line(color = "gray90", linetype=1)) +
      # scale_color_manual(values = colorRampPalette(brewer.pal(11, "RdYlBu"))(nColors)) +
      scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, limits=c(min(dp$Date), maxDate+days(nPredDays*0+2)), expand=expand_scale(mult=0.01)) +
      scale_y_continuous(limits=c(.5,yLimMax), breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.01), trans=trans, 
                         name="Positive/100.000 Einwohnern. \nAmpelfarben entlang ECDC (European Centre for Disease Prevention and Control) ") + 
      geom_line(aes(x=Date, y=1), size=1.0, color="green") +
      geom_line(aes(x=Date, y=2), size=1.0, color="orange") +
      geom_line(aes(x=Date, y=4), size=.8, color="magenta") +
      geom_line(aes(x=Date, y=8), size=.8, color="red") +
      geom_line(aes(x=Date, y=16), size=.8, color="darkred") +
      geom_line(aes(x=Date, y=32), size=.8, color="black") +
      geom_line(aes(x=Date, y=64), size=1.0, color="black") +
      geom_line(aes(x=Date, y=128), size=1.5, color="black") +
      geom_line(size=.25, aes(color=Region))+
      #theme(legend.position = "none") + 
      #geom_smooth(data=dp%>%dplyr::filter(Date<as.Date("2020-12-01")), aes(x=Date, y=rm7NewConfPop),method="lm", se=FALSE) +
      ggtitle(paste0("COVID-19 Österreich, Bundesländer und Bezirke: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date), ".  Basisdaten: AGES"))
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
  
}


# start shiny app
shinyApp(ui = ui, server = server, options=list(launch.browser = TRUE, width=1650, height=1024))


