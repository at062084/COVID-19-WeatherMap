nModelDays=28
nPredDays=14
regions <- df %>% group_by(Region) %>% summarize(Date=first(Date)) %>% dplyr::ungroup() %>% dplyr::select(Region)
predRegions=rep(regions$Region,each=(nPredDays+nModelDays))
predDate=seq.Date(maxDate-days(nModelDays-1),maxDate+days(nPredDays),1)
predDates=rep(predDate,nrow(regions))
predDF <- data.frame(Date=predDates, Region=predRegions, stringsAsFactors=FALSE)
dplm <- df %>% 
  dplyr::filter(Date>maxDate-days(nModelDays)) %>%
  dplyr::select(Date, Region, rm7NewConfPop) %>%
  dplyr::full_join(predDF, by=c("Date","Region")) %>%
  dplyr::arrange(Region, Date) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(predNewConfPop=exp(predict(lm(log(rm7NewConfPop)~Date, na.action=na.exclude), newdata=data.frame(Date=predDate)))) %>%
  dplyr::ungroup()




# -----------------------------------------------------
# Short Term prediction of newConfPop
# lm(formula = log(y) ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=x)
# -----------------------------------------------------

nModelDaysST=10
nPredDaysST=7

de <- caAgesRm7EstimatePoly(df, nPolyDays=7, nPoly=2, nRm7Days=7)

maxDate <- max(df$Date)
regions <- df %>% group_by(Region) %>% summarize(Date=first(Date)) %>% dplyr::ungroup() %>% dplyr::select(Region)
predRegions=rep(regions$Region,each=nPredDaysST+nModelDaysST)
predDate=seq.Date(maxDate-days(nModelDaysST-1),maxDate+days(nPredDaysST),1)
predDates=rep(predDate,nrow(regions))
predDF <- data.frame(Date=predDates, Region=predRegions, stringsAsFactors=FALSE)
dplmST <- df %>% 
  dplyr::filter(Date>maxDate-days(nModelDaysST)) %>%
  dplyr::select(Date, Region, rm7NewConfPop) %>%
  dplyr::full_join(predDF, by=c("Date","Region")) %>%
  dplyr::arrange(Region, Date) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(nDate=1:n()) %>%
  #  dplyr::mutate(predNewConfPop=exp(predict(lm(log(rm7NewConfPop)~Date, na.action=na.exclude), newdata=data.frame(Date=predDate)))) %>%
  dplyr::mutate(predNewConfPop=exp(predict(lm(log(rm7NewConfPop)~nDate+I(nDate^2), na.action=na.exclude), newdata=data.frame(nDate=nDate)))) %>%
  dplyr::ungroup()
ggplot(data=dplmST %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=predNewConfPop)) + geom_point() + geom_line(aes(y=rm7NewConfPop))
ggplot(data=dplmST %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=predNewConfPop)) + geom_point() + geom_line(aes(y=rm7NewConfPop))

