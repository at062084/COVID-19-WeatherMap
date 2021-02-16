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


# create 2d color palette
n=16
x=1:n
y=1:n
mc <- matrix(rep(x,n), nrow=n)*n-1-matrix(rep(seq(n-1,0,by=-1),n),nrow=n)
mr <- t(mc)
as.vector(mc)


cp <- rgb(as.vector(255-mc),as.vector(mr),0,maxColorValue=255)
g <- expand.grid(x,y)

hcl <- hcl(0:255)
ts <- cbind(g,i=1:256,hcl)



ggplot(data=ts, aes(x=Var1, y=Var2, color=i)) + 
  scale_color_manual(values=cp) +
  geom_point(aes(color=hcl), size=5) +  
  theme(legend.position = "none")

nx <- 19
ny <- 11
g <- expand.grid(x=seq(0,100, length.out=nx),y=seq(0,240,length.out=ny))
c <- hcl(h=g$y,c=50,l=sqrt(g$x*100))
xyz <- data.frame(g,i=as.factor(1:(nx*ny)))
ggplot(data=xyz, aes(x=100-x, y=240-y, color=i)) + 
  scale_color_manual(values=c) +
  geom_point(aes(color=i), size=10) +  
  theme(legend.position = "none")


pal <- function(col, border = "light gray", ...) {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
  axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}



#Capitals=c("Wien","Eisenstadt","Sankt Pölten(Land)","Linz(Stadt)","Klagenfurth Stadt","Graz(Stadt)","Salzburg(Stadt)","Innsbruck-Stadt","Feldkirch")
#Big10Cities=c("Wien","Graz(Stadt)","Linz(Stadt)","Baden","Vöcklabruck","Bregenz","Innsbruck-Stadt","Mödling","Amstetten","Kufstein")
#dc <- df %>%
#  dplyr::mutate(regionBigCity=Region %in% Big10Cities, regionCapital=Region %in% Capitals) %>%
#  dplyr::group_by(regionBigCity, Date) %>%
#  dplyr::summarize(cityConfPop = sum(newConfirmed)/sum(Population)*100000) %>%
#  dplyr::ungroup()


#ggplot(data=dfrm, aes(x=Date, y=newConfirmed/Population*1000000, color=Region)) + 
#  geom_line() + 
#  scale_x_date(limits=c(as.Date(strptime("2020-08-01",format="%Y-%m-%d")),NA), 
#               date_breaks="1 weeks", date_labels="%a.%d.%m") +
#  scale_y_continuous(limits=c(0,500)) + 
#  ggtitle("AGES Bezirke Timeline: Wien")


#cronJobDir <- "/srv/shiny-server/COVID-19-WeatherMap"
#cronJobDir <- "/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny"
#cronJobFile <- paste0(cronJobDir,"/cron.R")
#cronJobLog <-paste0(cronJobDir,"/log/cwm.cron.log")  
#cmd <- cron_rscript(rscript=cronJobFile, rscript_log=cronJobLog, log_timestamp=TRUE, workdir=cronJobDir)
#cmd
#cron_clear(ask=FALSE)
#cron_add(cmd, id='AGES-14', at = '14:14')
#cron_add(cmd, id='AGES-22', at = '22:22')
#logMsg("Define cron job for data retrieval from AGES")
#cronCMD14="14 14 * * * shiny cd /srv/shiny-server/COVID-19-WeatherMap %% /usr/local/bin/Rscript ./cron.R"
#cronCMD23="23 23 * * * shiny cd /srv/shiny-server/COVID-19-WeatherMap %% /usr/local/bin/Rscript ./cron.R"
#system2("sudo",paste("Echo",cronCMD14,">> /etc/crontab"))
#system2("sudo",paste("Echo",cronCMD23,">> /etc/crontab"))


#<tr><td>WochenInzidenz heute:  </td><td align='right'> %g</td></tr>
#<tr><td>WochenInzidenz nächste Woche:  </td><td align='right'> %g </td></tr>
#round(pMapNUTS$rm7NewConfPop.c*7), round(pMapNUTS$rm7NewConfPop.f*7), 

#addPolygons(data=mapNUTS1, stroke = TRUE, smoothFactor = 0, color="black", fillOpacity = 0, fillColor="None", weight=10, group="AT1") %>%
#addPolygons(data=mapNUTS3, stroke = TRUE, smoothFactor = 0, fillOpacity = 0, fillColor="none", weight=1, group="AT3") %>%
#addMarkers(lng=~cxNUTS, lat=~cyNUTS, group="Trend", label=atRegions, popup=~Region) %>%
#addLayersControl(overlayGroups=c("AT1","AT3"), options=layersControlOptions(collapsed=FALSE)) %>%
#hideGroup(c("AT1","AT3","Markers"))
# idxDblConfPop=.bincode(dt7rm7NewConfPop,binDblDays), 
#dp <- df.past() %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7"))# Today

#dplyr::left_join(dh %>% dplyr::select(Date, Region,dt7rm7NewConfPop) %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(-Date), by=c("iso"="CountyID")) %>%
# dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.c,binForeCast), 
#              idxMonConfPop=.bincode(rm7NewConfPop,binForeCast),
#              idxForConfPop=.bincode(rm7NewConfPop.f,binForeCast)) #%>%
#     #               idxDblConfPop=.bincode((rm7NewConfPop.c+(rm7NewConfPop.f-rm7NewConfPop.c)/7)/rm7NewConfPop.c,binDblDays), 
#dplyr::rename(County=Region, Region=State) # undo renames for above calculations and joins

# number of days till inzidenz doubles
#dt2 <- dh %>% 
#  dplyr::group_by(CountyID) %>% 
#  dplyr::summarise(dblDays=rm7PolyLog(rm7NewConfPop, nPoly=1, nModelDays=nWeatherForeCastDays, nNewData=1, bDblDays=TRUE)$pDblDays) %>%
#  dplyr::ungroup()

#addPolygons(data=mapNUTS1, stroke = TRUE, smoothFactor = 0, color="black", fillOpacity = 0, fillColor="None", weight=10, group="AT1") %>%
#addPolygons(data=mapNUTS3, stroke = TRUE, smoothFactor = 0, fillOpacity = 0, fillColor="none", weight=1, group="AT3") %>%
#addMarkers(lng=~cxNUTS, lat=~cyNUTS, group="Trend", label=atRegions, popup=~Region) %>%
#addLayersControl(overlayGroups=c("AT1","AT3"), options=layersControlOptions(collapsed=FALSE)) %>%
#hideGroup(c("AT1","AT3","Markers"))
# idxDblConfPop=.bincode(dt7rm7NewConfPop,binDblDays), 
#dp <- df.past() %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(Date, Region, dt7rm7NewConfPop,starts_with("rm7"))# Today





# Global constants
#dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
#popBreaks <- c(0,1,2,5,10,15,20,25,seq(30,150,by=10))
#popLogBreaks <- (c(.1,.2,.5,1,2,5,10,20,50,100))
#logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000))
#logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000),seq(10000,100000,by=10000))
#nModelDays=28
#nPredDays=14
#yLimMax <- 128
#julDate <- as.Date("2020-07-01")
#trans="log10"



# http://data.opendataportal.at/dataset/geojson-daten-osterreich
# https://github.com/ginseng666/GeoJSON-TopoJSON-Austria
# mapBezirke <- geojsonio::geojson_read(x="./maps/bezirke_95_geo.json", what="sp")



#locIDs <- dd %>% dplyr::filter(Date==curDate) %>% dplyr::select(locID)
#predRegions=rep(locIDs$locID,each=(nModelDays+nPredDays))
#predDate=seq.Date(minDate,maxDate,1)
#predDates=rep(predDate,nrow(locIDs))
#predDF <- data.frame(Date=predDates, locID=predRegions, stringsAsFactors=FALSE) %>% dplyr::arrange(locID,Date)

# add days to predict for to dd
#dd <- predDF %>% dplyr::left_join(dd, by=c("locID","Date"))


df.modelx <- eventReactive(df(), {
  logMsg(paste("eventReactive reactiveFileReader:modelx", cwmPredictionFile)) 
  
  # days from today back nWeatherForeCastDays days
  dh <- df() %>% 
    dplyr::filter(Date>max(Date-3-nModelDaysMonth)) %>%
    dplyr::mutate(locID=RegionID) # added so cwmAgesRm7EstimatePoly know what to look for (RegionID or CountyID)
  # Pick data for AGES (rm7 three days ago) 
  da <- dh %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(starts_with("Region"), rmaNewConfPop)
  # model prediction for today
  dq <- cwmAgesRm7EstimatePoly(dh, nModelDays=nModelDaysWeek, nPredDays=0, nPoly=1) %>%
    dplyr::filter(Date==max(dh$Date))
  # model predicition for next week
  dm <- cwmAgesRm7EstimatePoly(dh, nModelDays=nModelDaysWeek, nPredDays=nForeCastDaysWeek, nPoly=1) %>%
    dplyr::filter(Date==max(Date))
  # model predicition for next month
  do <- cwmAgesRm7EstimatePoly(dh, nModelDays=nModelDaysMonth, nPredDays=nForeCastDaysMonth, nPoly=1) %>%
    dplyr::filter(Date==max(Date))
  # number of days till inzidenz doubles
  dt2 <- dh %>% 
    dplyr::arrange(RegionID, Date) %>%
    dplyr::group_by(RegionID) %>% 
    dplyr::summarise(dblDays=rm7PolyLog(rm7NewConfPop, nPoly=1, nModelDays=nModelDaysWeek, bDblDays=TRUE)$pDblDays) %>%
    dplyr::ungroup()
  
  dz <- da %>%
    dplyr::left_join(dq, by="RegionID") %>%
    dplyr::left_join(dm, by="RegionID", suffix = c(".c", ".f")) %>%
    dplyr::left_join(do, by="RegionID") %>%
    dplyr::left_join(dt2, by="RegionID")
  
  # Add to geojson structure
  pMapNUTS <- mapNUTS %>% 
    dplyr::left_join(dz, by="RegionID") %>%
    #    dplyr::left_join(dh %>% dplyr::select(Date, RegionID,dt7rm7NewConfPop) %>% dplyr::filter(Date==max(Date)) %>% dplyr::select(-Date), by="RegionID") %>%
    dplyr::mutate(idxCurConfPop=.bincode(rm7NewConfPop.c,binForeCast), 
                  idxDblConfPop=.bincode((rm7NewConfPop.c+(rm7NewConfPop.f-rm7NewConfPop.c)/7)/rm7NewConfPop.c,binDblDays),
                  idxMonConfPop=.bincode(rm7NewConfPop,binForeCast),
                  idxForConfPop=.bincode(rm7NewConfPop.f,binForeCast))
  # need to plot 'Österreich' last
  pMapNUTS <- rbind(pMapNUTS[1,],pMapNUTS[2,],pMapNUTS[3,],pMapNUTS[4,],pMapNUTS[6,],pMapNUTS[7,],pMapNUTS[8,],pMapNUTS[9,],pMapNUTS[10,],pMapNUTS[5,])
  return(pMapNUTS)
})


ggplot(data=df %>% dplyr::filter(rm7NewConfTest>0.001, Date>as.Date("2020-10-01"), Date<as.Date("2021-02-15")) %>%
         dplyr::filter(Region=="Salzburg"), 
       aes(x=Date, y=rm7NewTested/10, color=Region)) + 
  geom_line(col="blue", size=.25) +
  geom_point(col="blue", size=.25) +
  geom_point(aes(y=rm7NewConfirmed), size=0.5, col="red") +
  geom_line(aes(y=rm7NewConfirmed), size=0.5, col="red") +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(trans="log10")


ggplot(data=dt %>% dplyr::filter(Date>as.Date("2020-12-14"), Date<as.Date("2021-02-15")),
       aes(x=Date, y=sumTested, color=Region)) + 
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  geom_line(size=.25) +
  geom_point(size=.25)
  
  
ggplot(data=df %>% dplyr::filter(rm7NewConfTest>0.001, Date>as.Date("2020-08-01"), Date<as.Date("2021-02-15")) %>% 
         dplyr::filter(Region=="Wien"),
       aes(x=rm7NewTested, y=rm7NewConfTest*100, color=Region)) + geom_path() + geom_point() +
  scale_x_continuous(limits=c(0,NA), trans="identity") +
  scale_y_continuous(limits=c(0,NA), trans="identity")

bmsgpk <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.csv")
ggplot (data=bmsgpk %>% dplyr::mutate(Date=as.Date(Stamp)) %>% dplyr::filter(Status=="Tested"), aes(x=Date, y=AT)) + 
  geom_line(col="red") + 
  geom_line(data=bmsgpk %>% dplyr::mutate(Date=as.Date(Stamp)) %>% dplyr::filter(Status=="Tested_PCR"),aes(y=AT), col="blue") + 
  geom_line(data=bmsgpk %>% dplyr::mutate(Date=as.Date(Stamp)) %>% dplyr::filter(Status=="Tested_AG"), aes(y=AT), col="green")

ggplot (data=bmsgpk %>% dplyr::mutate(Date=as.Date(Stamp)), aes(x=Date, y=AT, group=Status, col=Status)) + geom_line()
  
bm <- bmsgpk %>% 
  dplyr::mutate(Date=as.Date(Stamp)) %>%
  dplyr::select(-Stamp) %>%
  group_by(Date,Status) %>% 
  summarize_all(first) %>% 
  dplyr::ungroup() %>% 
  tidyr::gather(key=Region, value=Count, "AT":"W") %>%
  tidyr::spread(key=Status, value=Count, fill=NA, drop=FALSE) %>%
  dplyr::arrange(Region, Date) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(newConfirmed=rollmean(Confirmed-lag(Confirmed),7, align="center", fill=NA)) %>%
  dplyr::mutate(newTested=rollmean(Tested-lag(Tested),7, align="center", fill=NA)) %>%
  dplyr::mutate(newTested_PCR=rollmean(Tested_PCR-lag(Tested_PCR),7, align="center", fill=NA)) %>%
  dplyr::mutate(newTested_AG=rollmean(Tested_AG-lag(Tested_AG),7, align="center", fill=NA)) %>%
  dplyr::ungroup()
  
ggplot(data=bm %>% dplyr::filter(Region=="AT", Date > as.Date("2020-10-01")), aes(x=Date, y=newTested)) + geom_line(col="blue") +
  geom_line(aes(y=newTested_PCR), col="cyan") + 
  geom_line(aes(y=newTested_AG), col="magenta")  +
  #geom_line(aes(y=Confirmed*10), col="red") +
  geom_line(aes(y=newConfirmed/newTested*1000000), col="green") +
  geom_line(aes(y=newConfirmed*10), col="black")


# Mutations
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(xml2)
library(rvest)
library(ggplot2)

# get html page from bmsgpk
ts=format(now(),"%Y%m%d-%H%M")
url <- "https://www.ages.at/themen/krankheitserreger/coronavirus/sars-cov-2-varianten-in-oesterreich"
mutFile <- paste0("./html/COVID-19-austria.mutations.",ts,".html")
logMsg(paste("Scraping", url))
logMsg(paste("Dumping page to", mutFile))
cmd <- paste(url, "-O", mutFile)
system2("wget", cmd)

#xpathTable <- "/html/body/div[3]/div/div/div/div[2]/main/div[2]/table"
#xt <- xml2::xml_find_all(html, xpathTable)

logMsg(paste("Parsing dump in", mutFile))
html <- xml2::read_html(mutFile)

logMsg(paste("Extracting Status table in Bundesländer"))
tables <- rvest::html_table(html, dec=",", fill=TRUE)

mutRegionS <- c("AT","B","K","NOe","OOe","Szbg","Stmk","T","V","W")
mutRegion <- c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")

# initialize
df <- as.data.frame(tables[[2]]) %>% mutate(Region=mutRegion[2])
str(df)
for(t in 3:10) {
  dt <- as.data.frame(tables[[t]]) %>% mutate(Region=mutRegion[t])
  df <- rbind(df,dt)
}
#df <- df %>% dplyr::select(Region, Fälle, starts_with("KW"))
dg <- df %>% 
  tidyr::gather(key=Week, value=Count, starts_with("KW")) %>% 
  dplyr::mutate(Date=as.Date("2021-01-04") + weeks(as.integer(substr(Week,3,4)))) %>%
  dplyr::select(Date, Region, Status=Fälle, Count, -Week)


# Add sum over all Regions as 'Österreich'
ds <- dg %>% 
  dplyr::group_by(Date, Status) %>% 
  summarize(Count=sum(Count)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Region="Österreich") %>%
  dplyr::select(Date, Region, Status, Count)
str(ds)  
dg  <- rbind(dg,ds)
str(dg)  


mutStates <- levels(factor(dg$Status))
dk <- dg
dk$Status <- factor(dk$Status, levels=mutStates[c(3,6,1,2,5,4,7,8)])
ggplot(data=dk, aes(x=Date, y=Count, color=Region, shape=Region)) + geom_line() + geom_point() +
  facet_wrap(.~Status, nrow=2, scales="free_y") +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous()

# dynamic selection of mutations
idx <-  !is.na(str_match(mutStates, "positiv")) | !is.na(str_match(mutStates, "bestätigt"))| !is.na(str_match(mutStates, "gesamt"))
fltStatus <- mutStates[idx]
ggplot(data=dg %>% dplyr::filter(Status %in% fltStatus), aes(x=Date, y=Count, color=Status, shape=Status)) + geom_line() + geom_point() +
  facet_wrap(.~Region, nrow=2, scales="free_y") +
    scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
    scale_y_continuous()


  
  
