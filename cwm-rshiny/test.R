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

de <- cwmAgesRm7EstimatePoly(df, nPolyDays=7, nPoly=2, nRm7Days=7)

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






#lm.x <- 1:nWeatherForeCastDays
d <- 1:nWeatherForeCastDays
#nd.d <- data.frame(Date=c(maxDate+weeks(c(0,1,4))))
#nd.c <- data.frame(rm7NewConfPop=1:128) # Inzidenz levels 1,2,4,8,16,32,64,128
#lm.w <- c(1:(nWeatherForeCastDays-3),6,4,2)
lm.w <- rep(1,nWeatherForeCastDays)
#rowNames <- c("Heute","In einer Woche","In vier Wochen","Tage bis Verdoppelung","Tage bis/noch LockDown")
#colNames <- (dx %>% dplyr::filter(Date==maxDate) %>% dplyr::select(Region))$Region

maxDate=max(dx$Date)
rowNames <- c("Heute","In einer Woche","In vier Wochen","Tage bis Verdoppelung")
d <- 1:nWeatherForeCastDays
w <- c(1:(nWeatherForeCastDays-3),6,4,2)
nd.d <- c(0,7,28) + nWeatherForeCastDays
nd.c <- c(8)
dy <- data.frame(Inzidenz=rowNames, stringsAsFactors=FALSE)
for (r in 1:length(atRegions)) {
  c <- dx$rm7NewConfPop[dx$Region==atRegions[r]]
  lm.d <- lm(log(c)~d, weights=w)
  p.d <- round(exp(predict (lm.d, newdata=data.frame(d=nd.d))),1)
  dblDays <- (round(log(2)/coef(lm.d)[2]))
  
  p <- c(p.d,dblDays)  
  cn <- colnames(dy)
  dy <- cbind(dy,p)
  colnames(dy) <- c(cn,atRegionsShort[r])
}
rownames(dy) <- NULL
dy[,c(1,6,2:5,7:11)]



