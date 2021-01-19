
# Settings for cwmConfPopStyle
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,100,by=10),120,150,200,300,400,500)
yLimMin <- 1
yLimMax <- 128


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

  dblXDays <- c(1:7,10,14,21,28,56,Inf,-56,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
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
