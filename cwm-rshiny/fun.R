options(error = function() traceback(2))
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



# Settings for cwmConfPopStyle
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,100,by=10),120,150,200,300,400,500)
yLimMin <- 1
yLimMax <- 128

# Setings for cwmSpreadStyle
dblXDays <- c(1:7,10,14,21,28,56,Inf,-56,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)

# WeatherMaps
nModelDaysPrediction = 10
nModelDaysWeek = 14
nModelDaysMonth = 14
nModelDaysQuater = 56
nForeCastDaysWeek =  7 
nForeCastDaysMonth = 28 
nForeCastDaysQuater = 91 
nModelDaysCountyWeek = 14
nModelDaysCountyMonth = 14
nModelDaysCountyQuater = 56
nForeCastDaysCountyWeek =  7
nForeCastDaysCountyMonth = 28
nForeCastDaysCountyQuater = 91
binConfPop <- c(0,1,1.4,2,2.8,4,5.6,8,11,16,22,32,45,64,90,128,512)
palConfPop <- c(brewer.pal(9,"Greens")[c(7,6,5,4)], brewer.pal(9,"YlOrRd"), "#404040", brewer.pal(9,"Greys")[c(8,9)])
colConfPop <- colorBin(palette=palConfPop, domain=0:256, bins=binConfPop)
dblDays <- c(1,14,28,56,-56,-28,-14,-1)
binDblDays <- sort(round(exp(log(2)/dblDays),3))
binForeCast <- c(0,4,8,16,32,Inf)

# ----------------------------------------------------------------------------------
# Geo data (from OpenData*)

# OBSOLETE: Regions characteristics. Matched. Don't mess around
atRegions=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Österreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")
atRegionsShort=c("B","K","NOe","OOe","AT","Szbg","Stmk","T","V","W")
atShapes <- c(10,6,7,2,11,5,12,22,1,9)
# Settings for all Region Plots: Color Blind Palette
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FF0000")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#444444", "#F0D042", "#0072B2", "#D55E00", "#CC79A7", "#C40000")
# ----------------------------------------------------------------------------------

datATRegions <- data.frame(
  NUTS_ID=c("AT0","AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34"),
  Region= c("Österreich", "Burgenland","Niederösterreich","Wien", "Kärnten","Steiermark", "Oberösterreich","Salzburg","Tirol","Vorarlberg"),
  RegionS=c("AT","B","NOe","W","K","Stmk","OOe","Szbg","T","V"),
  RegionID=as.character(c(10,1,3,9,2,6,4,5,7,8)), stringsAsFactors=FALSE) %>% 
  dplyr::arrange(Region) %>%
  dplyr::mutate(Shape=c(10,6,7,2,11,5,12,22,1,9)) %>% 
  dplyr::mutate(Palette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#444444", "#F0D042", "#0072B2", "#D55E00", "#CC79A7", "#C40000")
)

# Bundesländer
funATRegions <- function() {
  geo <- geojsonio::geojson_read(x="./maps/laender_999_geo.json", what="sp") %>%
    dplyr::rename(Region=name, RegionID=iso)
  return(geo)
}

# Bezirke
funATCounties <- function() {
  di <- datATRegions %>% dplyr::select(RegionID, Region, Palette)
  geo <- geojsonio::geojson_read(x="./maps/bezirke_999_geo.json", what="sp") %>%
    # remove Bezirke Wien
    dplyr::filter(as.integer(iso)<=900) %>%
    dplyr::rename(County=name, CountyID=iso) %>%
    dplyr::mutate(RegionID=(str_sub(as.character(CountyID),1,1)), CountyNR=(str_sub(as.character(CountyID),2,3))) %>%
    dplyr::left_join(di, by="RegionID") %>% 
    dplyr::mutate(CountyID=as.character(CountyID)) %>%
    dplyr::select("Region","RegionID","County","CountyID","CountyNR")
  return(geo)
}


# Bundsländer für die aktuelle Wetterkarte, mit geoLocations der Icons
funNUTSAT <- function () {
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

  # Construct NUTS0 as a micropoligon in the center of NUTS1
  mapNUTS0 <- mapNUTS1 %>% dplyr::filter(NUTS_ID=="AT2")
  mapNUTS0$NUTS_ID <- "AT0"
  mapNUTS0@polygons[[1]]@Polygons[[1]]@coords <- matrix(c(cxNUTS[1]+1.5+c(-.75,+.75,+.75,-.75,-.75),cyNUTS[1]+.05+c(-.25,-.25,+.25,+.25,-.25)),ncol=2)
  mapNUTS0@polygons[[1]]@plotOrder <- as.integer(10)

  # Mapping from NUTS2 to Region
  NUTS_AT <- data.frame(
    NUTS_ID=c("AT0","AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34"),
    Region= c("Österreich", "Burgenland","Niederösterreich","Wien", "Kärnten","Steiermark", "Oberösterreich","Salzburg","Tirol","Vorarlberg"),
    RegionID=as.character(c(10,1,3,9,2,6,4,5,7,8)),
    stringsAsFactors=FALSE)
  
  # Add features Region and Center 
  mapNUTS <- rbind(mapNUTS0,mapNUTS2) %>% 
    dplyr::left_join(NUTS_AT, by="NUTS_ID") %>% 
    dplyr::mutate(cxNUTS=cxNUTS, cyNUTS=cyNUTS)
  
  # Patch center coords for better position of WeatherMap icons
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

cwm.model <- function(dx, dg=datATRegions, locID="Region", colID="RegionS") {
  maxDate=max(dx$Date)
  n <- nModelDaysPrediction + c(0,nForeCastDaysWeek,nForeCastDaysMonth)
  t <- c(32,16,8,4)
  rowNames <- c("Date", "AGES", "Heute","In einer Woche","In vier Wochen", "ÄnderungVortag","dblDays","Tage bis Verdoppelung","Tage bis   Halbierung", paste0("Tage bis Inzidenz=",t))
  rowIDs <- c("Date","rmaNewConfPop","rm7NewConfPop.0","rm7NewConfPop.7","rm7NewConfPop.28","dtDay", "dblDays", "DblDays","HalfDays",paste0("rm7NewConfPop",t))
  dy <- data.frame(Inzidenz=rowNames, stringsAsFactors=FALSE)
  rownames(dy) <- rowIDs
  for (r in 1:(dim(dg)[1])) {
    # idx <- dx$Region==dg$Region[r]
    idx <- dx[,locID]==as.character(dg[r,locID])
    y <- dx$rm7NewConfPop[idx]
    p <- rm7PolyLog(y, nPoly=1, nNewData=n, nTransData=t, bDblDays=TRUE)
    p$pTransData[p$pTransData<0] <- NA
    q <- c(as.numeric(maxDate),
           round(dx$rmaNewConfPop[idx & dx$Date==maxDate]/7,1),
           round(p$pNewData,1),
           round(exp(log(2)/p$pDblDays),3),
           round(p$pDblDays),
           ifelse(p$pDblDays>=0,round(p$pDblDays),NA), 
           ifelse(p$pDblDays<=0,-round(p$pDblDays),NA), 
           round(p$pTransData))
    cn <- colnames(dy)
    dy <- cbind(dy,(q))
    colnames(dy) <- c(cn,dg[r,colID])
  } 
  return (dy)
}


# ----------------------------------------------------------------------------------------------
# Standard ggplot style for newConfPop~Date
# ----------------------------------------------------------------------------------------------
cwmConfPopStyle <- function(sldPastTime=3, cbLogScale=TRUE, inRegions=1:10, xLimits=c(NULL,NULL), yLimits=c(yLimMin, yLimMax), yLabel="Positive/100.000 Einwohnern") {
  
  # Process left side menu user interactions
  trans <- ifelse(cbLogScale, "log10", "identity")
  if(as.integer(sldPastTime)<=3) {
    rvBreaks="1 weeks"
    rvLabels="%d.%m"
  } else {
    rvBreaks="1 months"
    rvLabels="%B"
  }

  idxRegions <- sort(match(inRegions,atRegions))
  regPalette <- cbPalette[idxRegions]
  regShapes <- atShapes[idxRegions]
  # expand=expand_scale(mult=0.025), expand=expand_scale(add=1)
  
  list(
    theme(panel.grid.major = element_line(color = "darkgray", linetype=3), 
          panel.grid.minor=element_line(color = "gray90", linetype=1),
          axis.text = element_text(size=12), axis.title.x=element_blank()),
      scale_shape_manual(values=regShapes),
      scale_fill_manual(values=regPalette),
      scale_color_manual(values=regPalette),
      scale_x_date(limits=xLimits, date_breaks=rvBreaks, date_labels=rvLabels, sec.axis=dup_axis()),
      scale_y_continuous(limits=yLimits, breaks=popBreaksAll, position="right",  trans=trans, name=yLabel, sec.axis=dup_axis()),
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

# ----------------------------------------------------------------------------------------------
# Standard ggplot style for dt7ConfPop~Date
# ----------------------------------------------------------------------------------------------
cwmSpreadStyle <- function(sldPastTime=3, inRegions=1:10, yLimits=c(0.84, 1.19)) {

  sSize=.5
  
  # Process left side menu user interactions
   if(as.integer(sldPastTime)<=3) {
    rvBreaks="1 weeks"
    rvLabels="%d.%m"
  } else {
    rvBreaks="1 months"
    rvLabels="%B"
  }

  idxRegions <- sort(match(inRegions,atRegions))
  regPalette <- cbPalette[idxRegions]
  regShapes <- atShapes[idxRegions]
  
  # expand=expand_scale(add=1),
  list(
    theme(panel.grid.major = element_line(color = "darkgray", linetype=3), 
          panel.grid.minor=element_line(color = "gray90", linetype=1),
          axis.text = element_text(size=12), axis.title.x=element_blank()),
    scale_shape_manual(values=regShapes),
    scale_x_date(date_breaks=rvBreaks, date_labels=rvLabels,  sec.axis=dup_axis()),
    scale_y_continuous(limits=yLimits, breaks=exp(log(2)/dblXDays), labels=dblXDays, position="right",
                       sec.axis=dup_axis(labels=as.character(round((exp(log(2)/dblXDays)-1)*100,1)), name="Tägliche Steigerungsrate [%]")),
    scale_fill_manual(values=regPalette),
    scale_color_manual(values=regPalette),
    geom_line(aes(y=1.104), size=sSize, color="#b00000"),
    geom_line(aes(y=1.051), size=sSize, color="#ff0000"),
    geom_line(aes(y=1.025), size=sSize, color="#fe7f00"),
    geom_line(aes(y=1.012), size=sSize, color="#e3e300"),
    geom_line(aes(y=1.00), size=2.0, color="white"),
    geom_line(aes(y=0.988), size=sSize, color="#7ffe00"),
    geom_line(aes(y=0.976), size=sSize, color="#72e400"),
    geom_line(aes(y=0.952), size=sSize, color="#62c400"),
    geom_line(aes(y=0.906), size=sSize, color="#4f9e00")
  )
}


# ------------------------------------------------------------------------------------------
# Fit order 1 or 2 linear model using last nModelDays and calc estimate for one week ahead
# Given 
# 1. the weakness of the data reported for the past few days and
# 2. the 'shorted week' weekly means strategy used to calc the weekly means for the last 3 days
# this function is used to get a grasp of the current situation 
# It uses a order 1 or 2 LS linear model of the log'ed data, with weights
# weights are decreasing linearly from current day till the begin of model period
# then add to these weights the number of days that have been used to calculate their value (7 for weekly means, 5,3,1 for shorted weekly means)
# then subtract the minimum resulting weight from all weights (to decrease effective weight from days further in the past)
# ------------------------------------------------------------------------------------------
# Linear model of order nPoly on log data
rm7PolyLog <- function(y, nPoly=2, nModelDays=length(y), modWeights=NULL, nNewData=NULL, nTransData=NULL, bDblDays=FALSE) {
  nx=1:length(y)
  x <- 1:nModelDays
  y <- y[x]+0.001
  if(is.null(modWeights)) {
    modWeights <- c(1:(nModelDays-3),nModelDays-1,nModelDays-2,nModelDays-3) + c(rep(7,nModelDays-3), 5,3,1)
    # make different weights more pronounced
    modWeights <- modWeights-min(modWeights)+1
  }
  
  pm <- lm(formula = log(y) ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=modWeights)

  if(is.null(nNewData)) dn <- data.frame(x=nx) else dn <- data.frame(x=nNewData)
  pNewData <- exp(predict(pm, newdata=dn))
  if(bDblDays & nPoly==1) pDblDays <- log(2)/(coef(pm)[2]) else pDblDays <- NA
  if(is.null(nTransData)) pTransData <- NA else pTransData <- (log(nTransData)-coef(pm)[1])/coef(pm)[2]
  
  if (!bDblDays & is.null(nTransData)) r <- pNewData else r <- list(pNewData=pNewData, pDblDays=pDblDays, pTransData=pTransData)
  return(r)
}

# Linear model of order nPoly on lin data
rm7PolyLin <- function(y, nPoly=2, nModelDays=length(y), modWeights=NULL, nNewData=NULL, nTransData=NULL, bDblDays=FALSE) {
  nx=1:length(y)
  x <- 1:nModelDays
  y <- y[x]+0.001
  if(is.null(modWeights)) {
    modWeights <- c(1:(nModelDays-3),nModelDays-1,nModelDays-2,nModelDays-3) + c(rep(7,nModelDays-3), 5,3,1)
    # make different weights more pronounced
    modWeights <- modWeights-min(modWeights)+1
  }

  pm <- lm(formula = y ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=modWeights)
  
  if(is.null(nNewData)) dn <- data.frame(x=nx) else dn <- data.frame(x=nNewData)
  pNewData <- (predict(pm, newdata=dn))
  if(bDblDays & nPoly==1) pDblDays <- (2)/(coef(pm)[2]) else pDblDays <- NA
  if(is.null(nTransData)) pTransData <- NA else pTransData <- ((nTransData)-coef(pm)[1])/coef(pm)[2]
  
  if (!bDblDays & is.null(nTransData)) r <- pNewData else r <- list(pNewData=pNewData, pDblDays=pDblDays, pTransData=pTransData)
  return(r)
}

# Prediction on rm7* features group by "locID": this field MUST be provided in df (usually a copy of RegionID or CountyID)
cwmAgesRm7EstimatePoly <- function(df, nPoly=2, nModelDays=10, nPredDays=7, modWeights=NULL) {
  
  curDate <- max(df$Date)                 # last day in dataset                   
  minDate <- curDate - days(nModelDays)+1 # go back nModelDays for Model
  #maxDate <- curDate + days(nPredDays)    # Number of days to predict from last day in dataset

  # construct dataframe of Regions and Dates relevant to modelling
  dd <- df %>% 
    #dplyr::mutate(locID=RegionID) %>%
    dplyr::filter(Date>=minDate) %>%
    dplyr::select(Date, locID, starts_with("Region"), starts_with("County"), starts_with("rm7")) %>% 
    dplyr::arrange(locID,Date)

  # Append rows for each locID for every prediction day (fill features with copy of last day)
  if(nPredDays > 0) {
    # MUST set future data to NA, so that rm7PolyLog,rm7PolyLin will ignore when modelling across the complete data frame !!!
    dd.append <- dd %>% 
      dplyr::filter(Date==curDate) %>% 
      dplyr::mutate_at(vars(c(starts_with("rm7"),starts_with("rma"))), function(x){NA})
    for (k in 1:nPredDays) {
      dd.append$Date=curDate+days(k)
      dd <- rbind(dd,dd.append)
    }
  }
  
  # Calc order nPoly estimate for each locID and each rm7 feature for next nPredDays from past nModelDays
  dd <- dd %>%
    dplyr::group_by(locID) %>%
    # Log poly model for potentially exponentially growing items
    dplyr::mutate_at(vars(c(starts_with("rm7"),-rm7NewTested,-rm7NewConfTest)), rm7PolyLog, nPoly=nPoly, nModelDays=nModelDays, modWeights=modWeights) %>%
    #dplyr::mutate_at(vars(c(starts_with("rma"))), rm7PolyLog, nPoly=nPoly, nModelDays=nModelDays) %>%
    # nonLog linear model for newTested
    dplyr::mutate_at(vars(rm7NewTested), rm7PolyLin, nPoly=nPoly, nModelDays=nModelDays, modWeights=modWeights) %>%
    # Calc newConfProp from estimated Confirmed and Tested
    dplyr::mutate(rm7NewConfTest = rm7NewConfirmed/rm7NewTested) %>%
    dplyr::ungroup() 
  
  #dp %>% dplyr::filter(Region=="Wien", Date > max(Date)-days(10)) %>% dplyr::select(Date, Region, rm7NewConfPop)
  
  return(dd)
}




# --------------------------------------------------------------------------------------------------------
# AGES Estimate of rm7 data for past three days based on estimate of over/under reports depending on day of week
# Currently implemented for newConfirmed only. TODO: Calculate for all rm7* features
# --------------------------------------------------------------------------------------------------------
cwmAgesRm7DOWCorrection <- function(df, nWeeks=4, bPlot=FALSE) {
  # estimate the weekly rolling mean for today and the past two days by 
  # compensating the over/under estimation as the mean in the past three weeks
  begDate <- max(df$Date) - weeks(nWeeks) - days(3)
  endDate <- max(df$Date) - days(3)

  # Calculate over/under estimation factor
  dft <- df %>%
    dplyr::filter(Date > begDate) %>%
    dplyr::mutate(WeekDay=wday(Date, week_start=getOption("lubridate.week.start",1))) %>%
    dplyr::mutate(WeekNo=as.character(isoweek(Date)))

  # --> kCorr = rm7NewConfirmed/newConfirmed
  # --> rm7NewConfirmed = newConfirmed*kCorr
  
  dff <- dft %>% dplyr::filter(Date<=endDate) %>%
    dplyr::mutate(proNewConfirmed=rm7NewConfirmed/newConfirmed)
 
  # through away one outlyer if more than three weeks
  dowFactorCalc <- function(n){
    if(length(n)<=3) return(mean(n))
    d <- n-mean(n)
    n <- n[which (abs(d)!=max(abs(d)))]
    return(mean(n))
  }
  
  # extract correction factor for the last three days from the same week days on the  nWeeks before
  estWeekDays <- dft %>% dplyr::filter(Date>endDate) %>% dplyr::select(WeekDay) %>% dplyr::distinct()
  estWeekDays <- as.data.frame(estWeekDays)[,1]
  dof <- dff %>% 
    dplyr::filter(Date<=endDate) %>%
    dplyr::filter(WeekDay %in% estWeekDays) %>%
    dplyr::group_by(Region,WeekDay) %>% 
    dplyr::summarize(dowFactor=dowFactorCalc(proNewConfirmed)) %>%
    dplyr::ungroup()
  
  dow <- dof %>%
    # add Date colum back
    dplyr::inner_join (dft %>% dplyr::filter(Date>endDate) %>% dplyr::select(Date,Region,WeekDay,newConfirmed,rm7NewConfirmed), by=c("WeekDay","Region")) %>%
    dplyr::select(Date, Region, WeekDay, rm7NewConfirmed, newConfirmed, dowFactor) %>%
    dplyr::mutate(dowNewConfirmed=round(newConfirmed*dowFactor)) %>%
    dplyr::select(Date, Region, newConfirmed, dowNewConfirmed, WeekDay, dowFactor)
  
  
  if (bPlot) {
    ggplot(data=dft, aes(x=Date, y=newConfirmed, group=Region, color=Region)) + geom_point() + geom_line(size=0.5) +
      geom_line(aes(y=rm7NewConfirmed), size=2) +
      facet_wrap(.~Region, nrow=2, scales="free_y")

    dfq <- dft %>% 
      dplyr::filter(Date<=endDate) %>%
      dplyr::group_by(Region,WeekDay) %>% 
      dplyr::summarize(meanProNewConfirmed=mean(proNewConfirmed, trim=0.25)) %>%
      dplyr::ungroup()
    
    ggplot(data=dft, aes(x=WeekDay, y=proNewConfirmed, shape=WeekNo)) +
      scale_x_continuous(breaks=1:7)+
      scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
      geom_point(size=5) +
      geom_line(aes(y=1)) +
      geom_point(data=dfq, aes(x=WeekDay, y=meanProNewConfirmed, size=5, col="red"), inherit.aes=FALSE)+
      facet_wrap(.~Region, nrow=2)
  }
  
  # let the caller handle the estimates
  return(dow)    
}
