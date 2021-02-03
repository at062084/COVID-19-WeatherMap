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




# Generate a test data frame for handling of last three days

# daily under/over reporting (independent of absolute numbers)
p <- c(0.8,1,1.5,1.4,1.0,0.8,0.5) 
mean(p)
# four weeks plus three days
d <- as.Date("2021-01-04") + days(c(0:30)) 
# true test results
e <- exp((0:30)*.25) 
# reported test results
c <- e * p 
# data frame
dc <- data.frame(Date=d, epiConfirmed=e, newConfirmed=c) %>%
  dplyr::mutate(k=c(rep(p,4),p[1:3])) %>%
  dplyr::mutate(rm7NewConfirmed = rollmean(newConfirmed, 7, align="center", na.pad=TRUE)) %>%
  dplyr::mutate(k7 = rm7NewConfirmed/newConfirmed)

m <- lm(formula=log(newConfirmed)~Date, data=dc)
ggplot(data=dc, aes(x=Date, y=epiConfirmed)) + 
  geom_line() + 
  geom_point(aes(y=newConfirmed)) + 
  geom_line(aes(y=newConfirmed),size=.25) + 
  scale_x_date(date_labels="%a", date_breaks="1 day") +
  scale_y_continuous(trans="log10")  + 
  geom_line(aes(y=rm7NewConfirmed),size=.25, col="red") + 
  geom_point(aes(y=rm7NewConfirmed),col="red") +
  geom_line(aes(y=exp(predict(m))), col="green") +
  geom_line(aes(y=newConfirmed/k), col="magenta") + 
  geom_line(aes(y=newConfirmed*k7), col="cyan") 

ggplot(data=dc, aes(x=Date, y=rm7NewConfirmed/epiConfirmed)) +
  geom_line() + 
  scale_y_continuous(trans="log10")  

ggplot(data=dc, aes(x=Date, y=newConfirmed/rm7NewConfirmed)) + 
  geom_line() + geom_point() + scale_y_continuous(limits=c(0,2)) +
  geom_point(aes(y=k), colour="red")  

# --> k7 = rm7NewConfirmed/newConfirmed
# --> rm7NewConfirmed = newConfirmed*k7


