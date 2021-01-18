
caAgesRm7EstimatePoly <- function(df, nPoly=2, nModelDays=10, nPredDays=7) {
  
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
