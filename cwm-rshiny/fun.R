options(error = function() traceback(2))

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

# Regions
atRegions=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Österreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")

# Settings for all Region Plots: Color Blind Palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FF0000")

# Settings for cwmConfPopStyle
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,100,by=10),120,150,200,300,400,500)
yLimMin <- 1
yLimMax <- 128

# Setings for cwmSpreadStyle
dblXDays <- c(1:7,10,14,21,28,56,Inf,-56,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)




mapNUTSAT <- function () {
  # Austria NUTS poligons for NUTS1, NUTS2 and NUTS3
  mapNUTS1 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_1.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
  mapNUTS2 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_2.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
  mapNUTS3 <- geojsonio::geojson_read(x="./maps/nuts_rg_60m_2013_lvl_3.geojson", what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
  
  # Box Centers.  Center of AT2: will be used as Center for AT
  cxNUTS <- vector()
  cyNUTS <- vector()
  cxNUTS[1] <- mean(bbox(mapNUTS1)[1,])
  cyNUTS[1] <- mean(bbox(mapNUTS1)[2,])
  for(i in 1:length(mapNUTS2)) {
    cxNUTS[i+1] <- mean(bbox(mapNUTS2[i,])[1,])
    cyNUTS[i+1] <- mean(bbox(mapNUTS2[i,])[2,])
  }
  
  # Mapping from NUTS2 to Region
  NUTS_AT <- data.frame(
    NUTS_ID=c("AT2","AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34"),
    Region= c("Österreich", "Burgenland","Niederösterreich","Wien", "Kärnten","Steiermark", "Oberösterreich","Salzburg","Tirol","Vorarlberg"),
    stringsAsFactors=FALSE)
  
  # Add features Region and Center 
  mapNUTS <- rbind(mapNUTS1[2,],mapNUTS2) %>% 
    dplyr::left_join(NUTS_AT, by="NUTS_ID") %>% 
    dplyr::mutate(cxNUTS=cxNUTS, cyNUTS=cyNUTS)
  
  # Patch Wien, Niederösterreich and Burgenland center coords for better position of WeatherMap icons
  mapNUTS$cxNUTS[mapNUTS$Region=="Österreich"]       = mapNUTS$cxNUTS[mapNUTS$Region=="Österreich"] +1.5
  mapNUTS$cyNUTS[mapNUTS$Region=="Österreich"]       = mapNUTS$cyNUTS[mapNUTS$Region=="Österreich"] +.05
  mapNUTS$cxNUTS[mapNUTS$Region=="Wien"]             = mapNUTS$cxNUTS[mapNUTS$Region=="Wien"]
  mapNUTS$cyNUTS[mapNUTS$Region=="Wien"]             = mapNUTS$cyNUTS[mapNUTS$Region=="Wien"] +.15
  mapNUTS$cxNUTS[mapNUTS$Region=="Niederösterreich"] = mapNUTS$cxNUTS[mapNUTS$Region=="Niederösterreich"] -.5
  mapNUTS$cyNUTS[mapNUTS$Region=="Niederösterreich"] = mapNUTS$cyNUTS[mapNUTS$Region=="Niederösterreich"] + .5
  mapNUTS$cxNUTS[mapNUTS$Region=="Burgenland"]       = mapNUTS$cxNUTS[mapNUTS$Region=="Burgenland"] +.15
  mapNUTS$cyNUTS[mapNUTS$Region=="Burgenland"]       = mapNUTS$cyNUTS[mapNUTS$Region=="Burgenland"] +.35
  mapNUTS$cxNUTS[mapNUTS$Region=="Salzburg"]         = mapNUTS$cxNUTS[mapNUTS$Region=="Salzburg"] -.1
  mapNUTS$cyNUTS[mapNUTS$Region=="Salzburg"]         = mapNUTS$cyNUTS[mapNUTS$Region=="Salzburg"] -.1
  mapNUTS$cxNUTS[mapNUTS$Region=="Oberösterreich"]   = mapNUTS$cxNUTS[mapNUTS$Region=="Oberösterreich"] +.15
  mapNUTS$cyNUTS[mapNUTS$Region=="Oberösterreich"]   = mapNUTS$cyNUTS[mapNUTS$Region=="Oberösterreich"] +.2
  mapNUTS$cxNUTS[mapNUTS$Region=="Steiermark"]       = mapNUTS$cxNUTS[mapNUTS$Region=="Steiermark"] + .6
  mapNUTS$cyNUTS[mapNUTS$Region=="Steiermark"]       = mapNUTS$cyNUTS[mapNUTS$Region=="Steiermark"] - .15
  mapNUTS$cxNUTS[mapNUTS$Region=="Tirol"]            = mapNUTS$cxNUTS[mapNUTS$Region=="Tirol"] -.25
  
  # Sort mapNUT Regions along ggplot conventions for strings (lexical)
  #atRegions=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Österreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")
  mapNUTS <- rbind(mapNUTS[2,],mapNUTS[5,],mapNUTS[3,],mapNUTS[7,],mapNUTS[1,],mapNUTS[8,],mapNUTS[6,],mapNUTS[9,],mapNUTS[10,],mapNUTS[4,])

  return(mapNUTS)  
}


cwmConfPopStyle <- function(rbsPastTime=25, cbLogScale=TRUE, yLimits=c(yLimMin, yLimMax), yLabel="Positive/100.000 Einwohnern") {
  
  # Process left side menu user interactions
  trans <- ifelse(cbLogScale, "log10", "identity")
  if(as.integer(rbsPastTime)<26) {
    rvBreaks="1 weeks"
    rvLabels="%d.%m"
  } else {
    rvBreaks="1 months"
    rvLabels="%B"
  }
  
  list(
    theme(panel.grid.major = element_line(color = "darkgray", linetype=3), panel.grid.minor=element_line(color = "gray90", linetype=1)),
      scale_shape_manual(values=c(1:10)),
      scale_fill_manual(values=cbPalette),
      scale_color_manual(values=cbPalette),
      scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, expand=expand_scale(mult=0.01)),
      scale_y_continuous(limits=yLimits, breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.01), trans=trans, name=yLabel),
      geom_line(aes(y=1), size=1.0, color="green"),
      geom_line(aes(y=2), size=1.0, color="orange"),
      geom_line(aes(y=4), size=.8, color="magenta"),
      geom_line(aes(y=8), size=.8, color="red"),
      geom_line(aes(y=16), size=.8, color="darkred"),
      geom_line(aes(y=32), size=.8, color="black"),
      geom_line(aes(y=64), size=1.0, color="black"),
      geom_line(aes(y=128), size=1.5, color="black")
  )
}

cwmSpreadStyle <- function(rbsPastTime=25, yLimits=c(0.84, 1.19)) {

  sSize=.5
  
  # Process left side menu user interactions
   if(as.integer(rbsPastTime)<26) {
    rvBreaks="1 weeks"
    rvLabels="%d.%m"
  } else {
    rvBreaks="1 months"
    rvLabels="%B"
  }

  list(
    scale_shape_manual(values=c(1:10)),
    scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels, expand=expand_scale(mult=0.01)),
    scale_y_continuous(limits=yLimits, breaks=exp(log(2)/dblXDays), labels=dblXDays, position="right",
                       sec.axis=dup_axis(labels=as.character(round((exp(log(2)/dblXDays)-1)*100,1)), name="Tägliche Steigerungsrate [%]")),
    scale_fill_manual(values=cbPalette),
    scale_color_manual(values=cbPalette),
    geom_line(aes(y=1.104), size=sSize, color="black"),
    geom_line(aes(y=1.051), size=sSize, color="darkred"),
    geom_line(aes(y=1.025), size=sSize, color="red"),
    geom_line(aes(y=1.012), size=sSize, color="orange"),
    geom_line(aes(y=1.00), size=1.0, color="black"),
    geom_line(aes(y=0.988), size=sSize, color="lightgreen"),
    geom_line(aes(y=0.976), size=sSize, color="green"),
    geom_line(aes(y=0.952), size=sSize, color="blue"),
    geom_line(aes(y=0.906), size=sSize, color="darkblue")
  )
}



cwmAgesRm7EstimatePoly <- function(df, nPoly=2, nModelDays=10, nPredDays=7) {
  
  curDate <- max(df$Date)                      
  minDate <- curDate - days(nModelDays)+1 # Prediction interval: first day
  maxDate <- curDate + days(nPredDays) # Prediction interval: last day

  # construct dataframe of Regions and Dates relevant to modelling
  dd <- df %>% dplyr::filter(Date>=minDate) %>% dplyr::arrange(Region,Date)
  regions <- dd %>% dplyr::filter(Date==curDate) %>% dplyr::select(Region)
  predRegions=rep(regions$Region,each=(nModelDays+nPredDays))
  predDate=seq.Date(minDate,maxDate,1)
  predDates=rep(predDate,nrow(regions))
  predDF <- data.frame(Date=predDates, Region=predRegions, stringsAsFactors=FALSE) %>% dplyr::arrange(Region,Date)
  
  # add days to predict for to dd
  dd <- predDF %>% dplyr::left_join(dd, by=c("Region","Date"))

  # Linear model of order nPoly on log data
  rm7PolyLog <- function(y, nPoly=2, nModelDays=10) {
    nx=1:length(y)
    x <- 1:nModelDays
    y <- y[x]
    pm <- lm(formula = log(y) ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=x)
    exp(predict(pm, newdata=data.frame(x=nx)))
  }
  
  # Linear model of order nPoly on lin data
  rm7PolyLin <- function(y, nPoly=2, nModelDays=10) {
    nx=1:length(y)
    x <- 1:nModelDays
    y <- y[x]
    pm <- lm(formula = y ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=x)
    predict(pm, newdata=data.frame(x=nx))
  }
  
  # Calc order nPoly estimate for each Region and each rm7 feature for next nPredDays from past nModelDays
  dp <- dd %>%
    dplyr::select(Date, Region, starts_with("rm7")) %>%
    dplyr::group_by(Region) %>%
    # Log poly model for potentially exponentially growing items
    dplyr::mutate_at(vars(c(starts_with("rm7"),-rm7NewTested,-rm7NewConfTest)), rm7PolyLog, nPoly, nModelDays) %>%
    # nonLog linear model for newTested
    dplyr::mutate_at(vars(rm7NewTested), rm7PolyLin, nPoly, nModelDays) %>%
    # Calc newConfProp from estimated Confirmed and Tested
    dplyr::mutate(rm7NewConfTest = rm7NewConfirmed/rm7NewTested) %>%
    dplyr::ungroup() 
  
  #dp %>% dplyr::filter(Region=="Wien", Date > max(Date)-days(10)) %>% dplyr::select(Date, Region, rm7NewConfPop)
  
  return(dp)
}
