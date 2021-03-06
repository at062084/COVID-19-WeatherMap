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

bDebug <- FALSE

# Settings for cwmConfPopStyle
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,100,by=10),120,150,200,300,400,500)
popSteps=c(1,2,4,8,16,32,64,128)
yLimMin <- 1
yLimMax <- 128

# Setings for cwmSpreadStyle
dblXDays <- c(1:7,10,14,21,28,56,Inf,-56,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)

# WeatherMaps
nModelDaysPredictionCounties = 14
nModelDaysPrediction = 42
nModelDaysPredictionPast = 70
nModelPolyGrade = 2

#nModelDaysWeek = 14
#nModelDaysMonth = 14
#nModelDaysQuater = 56
nForeCastDaysWeek =  7 
nForeCastDaysMonth = 28 
nForeCastDaysQuater = 91 
#nModelDaysCountyWeek = 14
#nModelDaysCountyMonth = 14
#nModelDaysCountyQuater = 56
nForeCastDaysCountyWeek =  7
nForeCastDaysCountyMonth = 28
nForeCastDaysCountyQuater = 91
binConfPop <- c(-Inf,1,1.4,2,2.8,4,5.6,8,11,16,22,32,45,64,90,128,Inf)
palConfPop <- c(brewer.pal(9,"Greens")[c(7,6,5,4)], brewer.pal(9,"YlOrRd"), "#404040", brewer.pal(9,"Greys")[c(8,9)])
colConfPop <- colorBin(palette=palConfPop, domain=0:1024, bins=binConfPop)
dblDays <- c(1,14,28,56,-56,-28,-14,-1)
binDblDays <- sort(round(exp(log(2)/dblDays),3))
binForeCast <- c(0,4,8,16,32,Inf)

nSettleDays <- 7 # Number of days to wait until status 'all tests returned' reached (may or not actually be the case) 
nCalcWeeks <- 4 # Number of past weeks to use for estimation of fraction of under-reporting before nSettleDays reached

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
    RegionID=as.character(c(10,1,3,9,2,6,4,5,7,8)), 
    Region= c("Österreich", "Burgenland", "Niederösterreich", "Wien", "Kärnten", "Steiermark", "Oberösterreich", "Salzburg", "Tirol", "Vorarlberg"),
    Population=c(8901064,    294436,      1684287,           1911191, 561293,    1246395,      1490279,          558410,      757634,  397139), 
    RegionS=c(       "AT",   "B",         "NOe",             "W",     "K",       "Stmk",       "OOe",            "Szbg",     "T",      "V"),
    Regions=c(       "AT",   "B",         "Noe",             "W",     "K",       "Stmk",       "Ooe",            "Szbg",     "T",      "V"),
    stringsAsFactors=FALSE) %>% 
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

# Calculate prediction for today, next week (nForeCastDaysWeek) and next month (nForeCastDaysMonth) using 
# function rm7PolyLog() based on previous nModelDaysPrediction days
# rm7PolyLog 
cwm.model <- function(dx, nPoly=2, nModelDays=nModelDaysPrediction, dg=datATRegions, locID="Region", colID="RegionS") {
  
  if (bDebug) logMsg(paste(" cwm.model", capture.output(print(sys.call(1))), nPoly, nModelDays, locID, colID))
  
  # dx: Date, Region, rm7NewConfPop and rmaNewConfPop range(Date)
  maxDate=max(dx$Date)
  n <- c(0,nForeCastDaysWeek,nForeCastDaysMonth) # Days to be predicted in nNewData (today=0)
  t <- c(128,64,32,16,8)
  
  #rowNames <- c("Date", "AGES", "Heute","In einer Woche","In vier Wochen", "ÄnderungVortag","dblDays","Tage bis Verdoppelung","Tage bis   Halbierung", paste0("Tage bis Inzidenz=",t))
  #rowIDs <- c("Date","rmaNewConfPop","rm7NewConfPop.0","rm7NewConfPop.7","rm7NewConfPop.28","dtDay", "dblDays", "DblDays","HalfDays",paste0("rm7NewConfPop",t))

  rowNames <- c("Date", "AGES (3 Tage zurück)",         "Heute",          "In einer Woche", "In vier Wochen",   "ÄnderungVortag", "dblDays", "Tage bis Verdoppelung","Tage bis   Halbierung", 
                "Tage bis Minimum", "Minimum", "Tage bis Maximum", "Maximum",  paste0("Tage bis Inzidenz=",t))
  rowIDs <-   c("Date", "rmaNewConfPop","rm7NewConfPop.0","rm7NewConfPop.7","rm7NewConfPop.28", "dtDay",          "dblDays", "DblDays",               "HalfDays",               
                "MinDays",          "MinInz",  "MaxDays",          "MaxInz",   paste0("rm7NewConfPop",t))
  
  # dy: Data frame with feature predicted by rm7PolyLog. Regions in Cols, Features in Rows
  dy <- data.frame(Inzidenz=rowNames, stringsAsFactors=FALSE, row.names=rowIDs)
  # rownames(dy) <- rowIDs
  #print(rowIDs)
  
  # Loop over Regions in dg
  for (r in 1:(dim(dg)[1])) {
  
    # select rows in dx for Region r: idx <- dx$Region==dg$Region[r]
    idx <- dx[,locID]==as.character(dg[r,locID])
    
    # inzidence data of current Region
    y <- dx$rm7NewConfPop[idx]
    
    # Fit poly to y: list of features from model in p
    p <- rm7PolyLog(y, nPoly=nPoly, nModelDays=nModelDays, nNewData=n, nTransData=t, bDblDays=TRUE, bMinMax=(nPoly==2))
    # p <- list(pNewData=pNewData, pDblDays=pDblDays, pMMX=pMMX, pTransData=pTransData, pCoefs=pc)
    
    p$pTransData[p$pTransData<0] <- NA
    
    # construct vector of features
    q <- c(as.numeric(maxDate),
           round(dx$rmaNewConfPop[idx & dx$Date==maxDate]/7,1),
           round(p$pNewData,1),
           round(exp(log(2)/p$pDblDays),3),
           round(p$pDblDays),
           round(p$pHflDblDays[2]), round(p$pHflDblDays[1]),
           #ifelse(round(p$pDblDays)>0,round(p$pDblDays),NA), 
           #ifelse(round(p$pDblDays)<0,-round(p$pDblDays),NA),
           round(p$pMMX$xMin),round(p$pMMX$yMin), round(p$pMMX$xMax),round(p$pMMX$yMax),
           round(p$pTransData))
    cn <- colnames(dy)
    dy <- cbind(dy,(q))
    colnames(dy) <- c(cn,dg[r,colID])
  } 
  #rownames(dy) <- rowIDs
  #print(colnames(dy))
  #print(rownames(dy))
  #print(str(dy))
  #print(summary(dy))
  return (dy)
}

# not needed
cwm.predict <- function(df, nPoly=2, nModelDays=nModelDaysPrediction, nPredDays=7, modWeights=NULL) {
}

cwmFacetTheme.TextScale <- 1.5
cwmFacetTheme <- theme(
  axis.title.x = element_blank(),
  plot.title   = element_text(size=rel(cwmFacetTheme.TextScale)),
  strip.text.x = element_text(size=rel(cwmFacetTheme.TextScale)),
  strip.text.y = element_text(size=rel(cwmFacetTheme.TextScale)),
  axis.text    = element_text(size=rel(cwmFacetTheme.TextScale *.75)),
  legend.text  = element_text(size=rel(cwmFacetTheme.TextScale *.75)),
  legend.title = element_text(size=rel(cwmFacetTheme.TextScale *.75))
)
# text = element_text(size=rel(cwmFacetTheme.TextScale)

# ----------------------------------------------------------------------------------------------
# Standard ggplot style for newConfPop~Date
# ----------------------------------------------------------------------------------------------
cwmConfPopStyle <- function(sldPastTime=3, cbLogScale=TRUE, inRegions="Österreich", xLimits=c(NULL,NULL), yLimits=c(yLimMin, yLimMax), yLabel="TagesInzidenz", stepDate=as.Date(now())) {
  
  # Process left side menu user interactions
  trans <- ifelse(cbLogScale, "log10", "identity")
  if(as.integer(sldPastTime)<=3) {
    rvBreaks="1 weeks"
    rvLabels="%d.%m"
  } else {
    rvBreaks="1 months"
    rvLabels="%b"
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
      scale_y_continuous(limits=yLimits, breaks=popSteps, name=yLabel, labels=as.character(popSteps), position="right",  trans=trans,
                         sec.axis=dup_axis(name="WochenInzidenz", labels=as.character(popSteps*7))),
      annotate("text", x=stepDate, y=2^(0:7)*1.5, label=as.character(1:8), colour="grey", size=15),
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
    scale_y_continuous(limits=yLimits, breaks=exp(log(2)/dblXDays), labels=dblXDays, position="right", name="Tage bis Verdoppelung/Halbierung der Inzidenz",
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
# !!! CAVEAT: y MUST start with the first day to use for the model, and nModelDays MUST depict the 'time zero' day starting from the first day !!!
rm7PolyLog <- function(y, nPoly=2, nModelDays=length(y), modWeights=NULL, nNewData=NULL, nTransData=NULL, bDblDays=FALSE, bMinMax=FALSE) {
  
  #  capture.output(print(sys.call(1))),
  if (bDebug) logMsg(paste(" rm7PolyLog:", "length(y)=", length(y), y[1], y[length(y)], nPoly, nModelDays, length(modWeights), length(nNewData), length(nTransData), bDblDays, bMinMax))
  
  # --------------------------------------
  # TODO: This is very unstable and actually wrong if length(y)!=nModelDays, at least for 'Countie' weathermap
  # --------------------------------------
  Ny <- length(y)                   # may be different from nModelDays if nNewData is not given (such that a complete df is returned)
  Yy=y+0.001                        # adjust zero values (cannot be log'ed)
  Yx=(1:Ny)-nModelDays      # ???        # time values for model days (partly in the past, so <0. today: Yx==0)
  Mx <- (1:nModelDays)-nModelDays
  My <- Yy[1:nModelDays]
  if(is.null(modWeights)) {
    modWeights <- c(1:(nModelDays-3),nModelDays*.8,nModelDays*.7,nModelDays*.5) + c(rep(7,nModelDays-3), 5,3,1)
  }
  
  # calculate model of degree nPoly (should be one or two) and extract cofficients
  pm <- lm(formula = log(My) ~ poly(Mx, degree=nPoly, raw=TRUE), na.action="na.omit", weights=modWeights)
  pc <- coefficients(pm)
  names(pc) <- c("d","k","a")[1:length(pc)]

  # make prediction for required days --> this works for nPoly==1 and nPoly==2
  if(is.null(nNewData)) dn <- data.frame(Mx=Yx) else dn <- data.frame(Mx=nNewData)
  pNewData <- exp(predict(pm, newdata=dn))
  
  # calculate dblDays for nPoly==1
  pDblDays <- NA
  pHflDblDays <- c(NA,NA)
  
  if(bDblDays) {
  
    # same for fst and scnd order model    
    pDblDays <- log(2)/pc[2]

    if(nPoly==2) {

      # HflDblDays: For nPoly=2 defined by intersection of prediction model with half/double value of prediction for today
      pHflDblDays <- c(NA,NA)
      
      # halfdays (negativ double days)
      pd <- c(pc[1]-log(0.5*pNewData[1]),pc[2],pc[3])
      roots <- polyroot(pd)
      Roots <- Re(roots[abs(Im(roots))<1e-10 & Re(roots)>0])
      if(length(Roots)>0) pHflDblDays[1]=Roots[1]
      
      # dblDays
      pd <- c(pc[1]-log(2*pNewData[1]),pc[2],pc[3])
      roots <- polyroot(pd)
      Roots <- Re(roots[abs(Im(roots))<1e-10 & Re(roots)>0])
      if(length(Roots)>0) pHflDblDays[2]=Roots[1]
    }
  } 
  
  # Calculate Min/Max date and value from coefs for nPoly=2 model
  # ! must calc number of days relative to today (day=ny !!!, not start of data where day=1)
  pMMX <- list(mmX=NA, xMin=NA, yMin=NA, xMax=NA, yMax=NA)
  if(bMinMax & nPoly==2) {
    # calculate roots of first derivative
    t0 <- -pc[2]/2/pc[3]
    m0 <- pc[1]+pc[2]*t0+pc[3]*t0^2
    # max
    if(pc[3]<0) {
      pMMX$mmx <- "max"
      if (round(t0)>0) {
        pMMX$xMax <- round(t0)
        pMMX$yMax <- round(exp(round(m0)))
      }
    }
    if(pc[3]>0) {
      pMMX$mmx <- "min"
      if (round(t0)>0) {
        pMMX$xMin <- round(t0)
        pMMX$yMin <- round(exp(round(m0)))
      }
    }
  }
  
  # calculate number of days until inzidenz levels nTransData reached
  pTransData <- NULL
  if(!is.null(nTransData)) {
    n = length(nTransData)
    pTransData <- rep(NA,n)
    
    if (nPoly==1) {
      pTransData <- (log(nTransData)-coef(pm)[1])/coef(pm)[2]
    }
    if (nPoly==2) {
      for(i in 1:n) {
        # intersection of estimated poly line with nTransData 
        pn <- c((pc[1]-log(nTransData[i])),pc[2],pc[3])
        roots <- polyroot(pn)
        j <- abs(Im(roots))<1e-14 & Re(roots)>0
        # at least one root real and positive
        if (sum(j)>0) {
          Roots <- Re(roots[j])
          Roots <- Roots[round(Roots)>0]
          if (length(Roots)>0) pTransData[i] <- round(Roots[1])    
        }
      }
    }
  }
  
  # add calcuated estimates for critical events to list of return values
  if (!bDblDays & !bMinMax & is.null(nTransData)) r <- pNewData else r <- list(pNewData=pNewData, pDblDays=pDblDays, pHflDblDays=pHflDblDays, pMMX=pMMX, pTransData=pTransData, pCoefs=pc)
  return(r)
}

# Linear model of order nPoly on lin data
rm7PolyLin <- function(y, nPoly=2, nModelDays=length(y), modWeights=NULL, nNewData=NULL, nTransData=NULL, bDblDays=FALSE) {

  if (bDebug)  logMsg(paste(" rm7PolyLin:", capture.output(print(sys.call(1))), " length(y)=", length(y), nPoly, nModelDays, bDblDays))
  
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
cwmAgesRm7EstimatePoly <- function(df, nPoly=2, nModelDays=14, nPredDays=7, modWeights=NULL) {
  
  if (bDebug)  logMsg(paste(" cwmAgesRm7EstimatePoly:", capture.output(print(sys.call(1))), nPoly, nModelDays, nPredDays))
  
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
      if (bDebug) logMsg(paste(nPredDays, curDate, curDate+days(k)))
      dd.append$Date=curDate+days(k)
      dd <- rbind(dd,dd.append)
    }
  }
  
  if (bDebug) logMsg(table(dd$locID))
  
  # Calc order nPoly estimate for each locID and each rm7 feature for next nPredDays from past nModelDays
  dd <- dd %>%
    dplyr::group_by(locID) %>%
    # Log poly model for potentially exponentially growing items
    dplyr::mutate_at(vars(starts_with("rm7"),-rm7NewTested,-rm7NewConfTest), rm7PolyLog, nPoly=nPoly, nModelDays=nModelDays, modWeights=modWeights) %>%
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
    n <- n[which (abs(d)<max(abs(d)))]
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



