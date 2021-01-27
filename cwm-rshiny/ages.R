options(error = function() traceback(2))

# do some logging
#logDir = "./log"
#logFile <- "cwm.rshiny.log"
#logMsg <- function(msg, sessionID="_global_") {
#  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
#  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=stderr())
#}

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(tibbletime)
library(scales)
library(forcats)
options(error = function() traceback(2))


cfGKZtl <- "CovidFaelle_Timeline_GKZ.csv"  # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv    # caAgesRead_cfGKZtl()
cftl <- "CovidFaelle_Timeline.csv"         # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv        # caAgesRead_cftl()
cfz <- "CovidFallzahlen.csv"               # https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv                                                                      # caAgesRead_cfz
#tftl <- "TodesfaelleTimeline.csv"                                                                                   # caAgesRead_tftl()
#cfGKZ <- "CovidFaelle_GKZ.csv"            # https://covid19-dashboard.ages.at/data/CovidFaelle_GKZ.csv             


# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline_GKZ.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfGKZtl <- function(csvFile="CovidFaelle_Timeline_GKZ.csv", bSave=TRUE) {
  
  # "http://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv"
  webAGES <- "https://covid19-dashboard.ages.at/data"
  webFile <- paste0(webAGES,"/",csvFile) 
  logMsg(paste("Download AGES data from", webFile))
  dskFile <- paste0("./data/",csvFile)
  logMsg(paste("Storing AGES data to", dskFile))
  cmd <- paste(webFile, "-O", dskFile)
  system2("wget", cmd)
  
  df <- read.csv(dskFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=GKZ, Region=Bezirk, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rmaConfirmed=AnzahlFaelle7Tage) %>%
    dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
    dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
    dplyr::mutate(newConfPop = newConfirmed/Population*100000) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time) %>% dplyr::select(11,12,2,1,3,4,6,13,9,7,5,10,8) 

  rdaFile <- "./data/CovidFaelle_Timeline_GKZ.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(df, file=rdaFile)  
  
  #dplyr::mutate(SiebenTageInzidenzFaelle=as.integer(SiebenTageInzidenzFaelle))
  #str(df)
  #summary(df)

  # apply rolling mean to 'new*' cols
  db <- df %>%
    dplyr::arrange(Date, Region) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, fill=NA, align="right") %>%
    dplyr::ungroup()

  di <- data.frame(RegionID=as.character(1:9), Region=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"), stringsAsFactors=FALSE)
  db <- db %>%
    dplyr::rename(County=Region, CountyID=RegionID) %>%
    dplyr::mutate(RegionID=(str_sub(as.character(CountyID),1,1)), CountyNR=(str_sub(as.character(CountyID),2,3))) %>%
    dplyr::left_join(di, by="RegionID") %>% dplyr::select(2,14,16,3,15,4,5,7,6,8:13)

  rdaFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(db, file=rdaFile)  
  
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
}

# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline.csv: TimeLine BundesLänder (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cftl <- function(csvFile="CovidFaelle_Timeline.csv", bSave=TRUE) {
  
  # "http://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv"
  webAGES <- "https://covid19-dashboard.ages.at/data"
  webFile <- paste0(webAGES,"/",csvFile) 
  logMsg(paste("Download AGES data from", webFile))
  dskFile <- paste0("./data/",csvFile)
  logMsg(paste("Storing AGES data to", dskFile))
  cmd <- paste(webFile, "-O", dskFile)
  system2("wget", cmd)
  
  dc <- read.csv(dskFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, newRecovered=AnzahlGeheiltTaeglich, newDeaths=AnzahlTotTaeglich) %>%
    dplyr::rename(sumConfirmed=AnzahlFaelleSum, sumRecovered=AnzahlGeheiltSum, sumDeaths=AnzahlTotSum) %>%
    dplyr::mutate(curConfirmed=sumConfirmed-sumRecovered-sumDeaths) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time, -AnzahlFaelle7Tage) %>% 
    dplyr::select(11,10,2,1,3,4,8,6,5,9,7,12)
  #str(dc)
  #summary(dc)
  
  rdaFile <- "./data/CovidFaelle_Timeline.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(dc, file=rdaFile)  
  
  return(dc)
}

# -------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: TimeLine Bezirke (Tested, Hospital, ICU)
# -------------------------------------------------------------------------------------------
caAgesRead_cfz <- function(csvFile="CovidFallzahlen.csv", bSave=TRUE) {

  # prepare imputation of some weird problems in AGES data. TODO: check and enhance !!!
  cfzImpute <- function(newTested) {
    newTested[newTested<0]=0
    newTested[newTested==0]=mean(newTested, na.rm=TRUE)
    newTested[is.na(newTested)]=mean(newTested, na.rm=TRUE)
    newTested=round(newTested)
    return(newTested)
  }    

  # "http://covid19-dashboard.ages.at/data/CovidFallzahlen.csv"
  webAGES <- "https://covid19-dashboard.ages.at/data"
  webFile <- paste0(webAGES,"/",csvFile) 
  logMsg(paste("Download AGES data from", webFile))
  dskFile <- paste0("./data/",csvFile)
  logMsg(paste("Storing AGES data to", dskFile))
  cmd <- paste(webFile, "-O", dskFile)
  system2("wget", cmd)
  
  dt <- read.csv(dskFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(MeldeDatum, format="%d.%m.%Y %H:%M:%S"), Date=date(as.POSIXct(Meldedat, format="%d.%m.%Y"))) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, sumTested=TestGesamt) %>%
    dplyr::rename(curHospital=FZHosp, freeHospital=FZHospFree, curICU=FZICU, freeICU=FZICUFree) %>%
    dplyr::arrange(Date) %>% 
    dplyr::group_by(Region) %>% 
    dplyr::mutate(newTested=sumTested-lag(sumTested)) %>% 
    dplyr::mutate(newTested=cfzImpute(newTested)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Meldedat, -MeldeDatum) %>% 
    dplyr::select(8,9,6,7,10,1,2,3,4,5)
  #str(dt)
  #summary(dt)
  dt$Region[dt$Region=="Alle"] <- "Österreich"
  
  rdaFile <- "./data/CovidFallzahlen.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(dt, file=rdaFile)  
  
  return(dt)
}


# --------------------------------------------------------------------------------------------------------------------
# Join cftl and cfz AGES data files. Add several derived features
# --------------------------------------------------------------------------------------------------------------------
caAgesRead_tlrm <- function(cftlFile="./data/CovidFaelle_Timeline.rda", cfzFile="./data/CovidFallzahlen.rda", bPlot=FALSE, bSave=TRUE,
                            nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                            bResiduals=TRUE, dResFirst=as.Date("2020-07-27"), dResLast=as.Date("2020-11-16"), bShiftDown=TRUE,
                            bDOWCorrection=TRUE, nDOWCorrectionWeeks=5,
                            bPredict=TRUE, nPolyDays=7, nPoly=2,
                            bEstimate=FALSE, bCompleteCases=FALSE) {
  
  # Read timeline of confirmed, hospitalized, deaths
  dc <- readRDS(cftlFile)

  # Read timeline of tested
  dt <- readRDS(cfzFile)

  # need to check if both datasets are available up to the same date
  jointDate <- min(max(dc$Date), max(dt$Date))
  
  # Join 'Confirmed' and 'Tested' datasets
  dj <- dc %>% left_join(dt, by=c("Date","RegionID", "Stamp","Region")) 
  
  # Add col for Month and Week
  df <- dj %>% 
    dplyr::mutate(Month=as.character(month(Date, label=TRUE))) %>%
    dplyr::mutate(Week=week(Date))
  
  # Add col for record grouped record ID
  df <- df %>% dplyr::arrange(Date, Region) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(ID=1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(1,2,19,20,3,4,21,5,6:18)
  
  # Exclude data before 2020-04-02, these have NA's vor newTested and sumTested
  df <- df %>% 
    dplyr:: filter(Date <=jointDate) %>%
    dplyr::filter(Date>as.Date("2020-04-01")) %>% 
    ### DIRTY: upcoming bad AGES data imputation depends on data to be sorted !!!
    dplyr::arrange(Region, Date)   
  
  # impute wrong newTested. Heuristic :(
  idx <- which(df$newTested==0 | is.na(df$newTested))
  df$newTested[idx] <- round((df$newTested[idx-1]+df$newTested[idx+1])/2)
  
  # impute wrong newTested. Heuristic :(
  idx <- which(is.na(df$curHospital))
  df$curHospital[idx] <- round((df$curHospital[idx-1]+df$curHospital[idx+1])/2)
  idx <- which(is.na(df$curICU))
  df$curICU[idx] <- round((df$curICU[idx-1]+df$curICU[idx+1])/2)
  idx <- which(is.na(df$freeHospital))
  df$freeHospital[idx] <- round((df$freeHospital[idx-1]+df$freeHospital[idx+1])/2)
  idx <- which(is.na(df$freeICU))
  df$freeICU[idx] <- round((df$freeICU[idx-1]+df$freeICU[idx+1])/2)
  
  
  # add derived properties
  df <- df %>%
    dplyr::mutate(newConfPop=newConfirmed/Population*100000) %>%
    dplyr::mutate(newConfTest=newConfirmed/newTested)
  
  # impute wrong newConfTest TODO --> impute complete dataset upfront
  idx <- which(df$newConfTest >.7) # Heuristic :(
  df$newConfTest[idx] <- round((df$newConfTest[idx-1]+df$newConfTest[idx+1])/2)
  
  # apply rolling mean to 'new*' cols. Leaves the last three days with NA's
  df <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(rm7NewTested=(rollmean(newTested, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfirmed=(rollmean(newConfirmed, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewRecovered=(rollmean(newRecovered, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewDeaths=(rollmean(newDeaths, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfPop=(rollmean(newConfPop, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfTest=rollmean(newConfTest, k=nRm7Days, align="center", fill=NA)) %>%
    dplyr::mutate(rm7CurConfirmed=(rollmean(curConfirmed, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7CurHospital=(rollmean(curHospital, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7CurICU=(rollmean(curICU, k=nRm7Days, align="center", fill=NA))) %>%
    #dplyr::mutate_at(vars(starts_with("new")), rollmean, k=nRm7Days, align="center", fill=NA) %>%
    dplyr::ungroup() 
  # str(df)
  
  # apply 'Dow of Week Correction' for newConfirmed values for last three days of data
  if (bDOWCorrection) {
    # Estimate actual newConfirmed based on over/under reports of last nWeeks 
    dq <- cwmAgesRm7DOWCorrection(df, nWeeks=nDOWCorrectionWeeks) %>%
      dplyr::arrange(Date, Region) %>%
      dplyr::filter(Date>max(Date)-days(3)) %>%
      dplyr::arrange(Region,Date)
    
    # patch newConfirmed with dowNewConfirmed. This relies on same sort order: Region,Date    
    df$newConfirmed[df$Date>max(df$Date)-days(3)] <- dq$dowNewConfirmed
  }

  # Calculate 'shorted' week means for last three days:
  # This is provided as 'raw' data for downstream processing.
  # e.g. calculation of today's incidence rate may need to do more postprocessing, e.g. fitting a second order LS model 
  # d-3 = mean(last 7)
  # d-2 = mean(last 5)
  # d-1 = mean(last 3)
  # d-0 = last
  
  # work last three days
  maxDate <- max(df$Date)
  # work all cols with rolling means by week
  rm7s <- colnames(df)[startsWith(colnames(df),"rm7")]
  for(d in 0:2) {
    # work all rm7 features
    for (rm7 in rm7s) {
      # select item for all regions
      df[df$Date==maxDate-days(d),rm7] <- df %>% 
                                             dplyr::filter(Date>=maxDate-days(d*2)) %>% 
                                             dplyr::select(Region,x=paste0(tolower(substr(!!rm7, 4,4)),substr(!!rm7,5,100))) %>% 
                                             dplyr::group_by(Region)  %>%
                                             dplyr::summarize(rm7=mean(x)) %>%
                                             dplyr::ungroup() %>% 
                                             dplyr::select(rm7)
    }
  }
    
  # Update 2020-01-24: This is obsoleted by above strategy of 'shorted week weekly means'
  # patch rm7* NA's with predicts from lm poly model
  if(bPredict==Inf) {
    # Estimate actual newConfirmed based on linear model estimates of order 2.
    # this should work as the last three days have NA's in rm7*, which are omitted from modeling here:
    dp <- cwmAgesRm7EstimatePoly(df, nPoly=nPoly, nModelDays=nPolyDays+3, nPredDays=0) %>%
      dplyr::arrange(Date, Region) 

    
    ggplot(dp, aes(x=Date, y=rm7NewConfirmed))+geom_line() +
      geom_point(data=df%>%dplyr::filter(Date>=min(dp$Date)), aes(x=Date, y=newConfirmed), color="red") +
      geom_line(data=df%>%dplyr::filter(Date>=min(dp$Date)), aes(x=Date, y=rm7NewConfirmed), color="black", size=.5) +
      geom_line(data=df%>%dplyr::filter(Date>=min(dp$Date)), aes(x=Date, y=newConfirmed), color="red", size=.5) +
      geom_point(data=dq, aes(x=Date, y=dwfNewConfirmed), color="blue") +
      facet_wrap(.~Region, nrow=2, scales="free_y")
    
    # Same order as in dp
    df <- df %>% dplyr::arrange(Date, Region)
    rm7Cols=c("rm7NewTested", "rm7NewConfirmed","rm7NewRecovered","rm7NewDeaths", "rm7NewConfPop", "rm7NewConfTest", 
              "rm7CurConfirmed", "rm7CurHospital", "rm7CurICU")
    # Last three days have been estimated above, so can now be patched 
    patchDates <- unique(dp$Date)[(nPolyDays+1):(nPolyDays+3)]
    # TODO: some weir results for rm7NewTested ans rm7NewConfTest. Maybe others
    df[df$Date %in% patchDates,rm7Cols] <- dp[dp$Date %in% patchDates,rm7Cols]
  }
  
  # df %>% dplyr::filter(Region=="Wien", Date > max(Date)-days(10)) %>% dplyr::select(Date, Region, newConfPop,rm7NewConfPop)
  
  # Calculate speed of spread in percent change of new* per day. 
  # TODO: better handling of zeros in data 
  # TODO: Window should be center, as with rm7, plus special handling of last 3 days
  rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals+0.001)~Date), na.action=na.ignore, singular.ok=TRUE)[2])}, window=nDt7Days)
  
  # Add spread 
  if (bDt7) {
    # currently only looking at derivative of weekly means
    df <- df %>% 
      dplyr::arrange(Region,Date) %>% 
      dplyr::group_by(Region) %>% 
      dplyr::mutate(dt7rm7NewTested=rolm(Date,rm7NewTested)) %>%
      dplyr::mutate(dt7rm7NewConfirmed=rolm(Date,rm7NewConfirmed)) %>%
      dplyr::mutate(dt7rm7NewRecovered=rolm(Date,rm7NewRecovered)) %>%
      dplyr::mutate(dt7rm7NewDeaths=rolm(Date,rm7NewDeaths)) %>%
      dplyr::mutate(dt7rm7NewConfPop=rolm(Date,rm7NewConfPop)) %>%
      dplyr::mutate(dt7rm7NewConfTest=rolm(Date,rm7NewConfTest)) %>%    # produces some NA's
      dplyr::mutate(dt7rm7CurConfirmed=rolm(Date,rm7CurConfirmed)) %>%
      dplyr::mutate(dt7rm7CurHospital=rolm(Date,rm7CurHospital)) %>%
      dplyr::mutate(dt7rm7CurICU=rolm(Date,rm7CurICU)) %>%
      dplyr::ungroup() 
    #dplyr::mutate(dt7NewConfirmed=rolm(Date,newConfirmed)) %>%
    #dplyr::mutate(dt7CurConfirmed=rolm(Date,curConfirmed)) %>%
    #dplyr::mutate(dt7CurHospital=rolm(Date,curHospital)) %>%
    #dplyr::mutate(dt7NewDeaths=rolm(Date,newDeaths)) %>%
  }
  
  # add lpreds
  if(bLpr==Inf) {
    # Span adjusted manually to 19 after visual inspection of several options. maybe a bit too lpr. Smaller number for closer match to rm7
    span <- nLprDays/(dim(df)[1]/10) # Heuristic :(
    df <- df %>%
      dplyr::arrange(Region, Date) %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate(lprNewTested=round(predict(loess(newTested~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewConfirmed=round(predict(loess(newConfirmed~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewRecovered=round(predict(loess(newRecovered~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewDeaths=round(predict(loess(newDeaths~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewConfPop=lprNewConfirmed/Population*100000) %>%
      dplyr::mutate(lprŃewConfTest=lprNewConfirmed/lprNewTested) %>% 
      dplyr::mutate(dt7lprNewConfirmed=rolm(Date,lprNewConfirmed)) %>%
      dplyr::mutate(dt7lprNewDeaths=rolm(Date,lprNewDeaths)) %>%
      dplyr::ungroup()
  } 
  
  # Calculate robust regression on log transformed 
  if (bResiduals) {
    
    modLogLM <- function(Date, Count, Speed, dResFirst=as.Date("2020-07-27"), dResLast=as.Date("2020-10-19"), bPlot=FALSE, bShiftDown=FALSE) {
      # Complete log(Count)~Date dataframe d, with index into dateRange s considered for regression
      d <- data.frame(Date=Date, logCount=log(Count+0.001))
      # s <- Date>=dResFirst & Date<=dResLast & Speed > 0.99 & Speed <1.01
      s <- Date>=dResFirst & Date<=dResLast
      # m <- MASS::rlm(logCount~Date, data=d[s,], method="MM", maxit=100)
      # model and prediction of log(Count)
      m <- lm(logCount~Date, data=d[s,])
      p <- predict(m, newdata=d)
      
      # Shift down regression line just below all data points
      if(bShiftDown) {
        i <- min(residuals(m))
        m$coefficients[1] <- m$coefficients[1]+i-0.01
      }
      q <- predict(m, newdata=d)
      
      if(bPlot) {
        gg <- ggplot(data=d, aes(x=Date, y=logCount)) + geom_point(size=.5) + 
          geom_line(data=data.frame(Date=Date,logPred=p), mapping=aes(x=Date, y=logPred), color="red", size=.25) +
          geom_line(data=data.frame(Date=Date,parPred=q), mapping=aes(x=Date, y=parPred), color="blue", size=.25)
        options(digits.secs=9)
        ggsave(filename=format(now(),"resNew-%H:%M:%OS9.png"), plot=gg, device="png")
      }
      
      # return prediction as Count (not log(Count))
      r <- exp(q)
      return(r)
    }
    
    df <- df %>%
      dplyr::arrange(Region, Date) %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate(modrm7NewTested=modLogLM(Date, rm7NewTested, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfirmed=modLogLM(Date, rm7NewConfirmed, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfPop=modLogLM(Date, rm7NewConfPop, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfTest=modLogLM(Date, rm7NewConfTest, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%    # produces some NA's
      dplyr::mutate(modrm7CurConfirmed=modLogLM(Date, rm7CurConfirmed, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7CurHospital=modLogLM(Date, rm7CurHospital, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7CurICU=modLogLM(Date, rm7CurICU, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewRecovered=modLogLM(Date, rm7NewRecovered, dt7rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewDeaths=modLogLM(Date, rm7NewDeaths, dt7rm7NewConfirmed, as.Date("2020-09-21"), as.Date("2020-12-07"))) %>%
      
      dplyr::mutate(resrm7NewTested=rm7NewTested-modrm7NewTested) %>%
      dplyr::mutate(resrm7NewConfirmed=rm7NewConfirmed-modrm7NewConfirmed) %>%
      dplyr::mutate(resrm7NewConfPop=rm7NewConfPop-modrm7NewConfPop) %>%
      dplyr::mutate(resrm7NewConfTest=rm7NewConfTest-modrm7NewConfTest) %>%    # produces some NA's
      dplyr::mutate(resrm7CurConfirmed=rm7CurConfirmed-modrm7CurConfirmed) %>%
      dplyr::mutate(resrm7CurHospital=rm7CurHospital-modrm7CurHospital) %>%
      dplyr::mutate(resrm7CurICU=rm7CurICU-modrm7CurICU) %>%
      dplyr::mutate(resrm7NewRecovered=rm7NewRecovered-modrm7NewRecovered) %>%
      dplyr::mutate(resrm7NewDeaths=rm7NewDeaths-modrm7NewDeaths) %>%
      dplyr::ungroup()
  }
  
  # patch rm7NewConfirmed data. DISABLED !
  if(bEstimate == Inf) {
    dx <- caAgesRM7Estimate(df)
    for (i in 1:dim(dx)[1]) {
      idxf <- which(df$Date==dx$Date[i] & df$Region==dx$Region[i])
      idxx <- which(dx$Date==dx$Date[i] & dx$Region==dx$Region[i])
      df[idxf,"rm7NewConfirmed"] <- round(df[idxf,"newConfirmed"]/dx[idxx,"meanProConfirmed"])
      df[idxf,"rm7NewConfPop"] <- round(df[idxf,"rm7NewConfirmed"]/df[idxf,"Population"]*100000)
      df[idxf,"newConfirmed"] <- round(df[idxf,"newConfirmed"]/dx[idxx,"meanProConfirmed"])
      # cat(dx$WeekDay[i],dx$Region[i],as.numeric(dx[idxx,"meanProConfirmed"]),as.numeric(df[idxf,"newConfirmed"]),as.numeric(df[idxf,"rm7NewConfirmed"]),'\n')
    }
  }
  
  # remove NA's
  if(bCompleteCases) {
    df <- df[complete.cases(df),]
  }
  
  # Sort cols
  df <- df %>% dplyr::select(1:8,sort(colnames(df)[c(9:dim(df)[2])]))

  rdaFile <- "./data/COVID-19-CWM-AGES-States-Curated.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(df, file=rdaFile)  
  
  return(df)
}

