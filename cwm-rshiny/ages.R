options(error = function() traceback(2))
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(tibbletime)
library(scales)
library(forcats)


# Mutations
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(xml2)
library(rvest)
library(ggplot2)


# -------------------------------------------------------------------------------------------
# AGES Data files
# -------------------------------------------------------------------------------------------

# caAgesRead_cfz       <- function(csvFile="CovidFallzahlen.csv")
# caAgesRead_cftl      <- function(csvFile="CovidFaelle_Timeline.csv")
# caAgesRead_cfGKZtl   <- function(csvFile="CovidFaelle_Timeline_GKZ.csv")
# caAgesRead_Mutations <- function(csvFile="COVID-19-AGES-Mutations.csv")

### Non-Timeline
### CovidFaelle_Altersgruppe.csv
### CovidFaelle_GKZ.csv

### Unhandled
### Mortalitätsmonitoring
### Gestorbene --> 6 files
### Corona Ampel --> 2 Files
### Checkboxen Standorte --> many files

### Obsolete
#### tftl   <- "TodesfaelleTimeline.csv"                                                                                  # caAgesRead_tftl()



# -------------------------------------------------------------------------------------------
# AGES Web Pages
# -------------------------------------------------------------------------------------------
# Mutations
caAgesRead_Mutations <- function(csvFile="COVID-19-AGES-Mutations.csv", bSave=TRUE) {
  
  ts=format(now(),"%Y%m%d")
  url <- "https://www.ages.at/themen/krankheitserreger/coronavirus/sars-cov-2-varianten-in-oesterreich"
  mutFile <- paste0("./html/COVID-19-austria.mutations.",ts,".html")
  logMsg(paste("Scraping", url))
  logMsg(paste("Dumping page to", mutFile))
  cmd <- paste(url, "-O", mutFile)
  system2("wget", cmd)
  
  logMsg(paste("Parsing dump in", mutFile))
  html <- xml2::read_html(mutFile)
  
  logMsg(paste("Extracting tables"))
  tables <- rvest::html_table(html, dec=",", fill=TRUE)
  idx <- seq(4,19,by=2)
  # mutRegionS <- c("AT","B","K","NOe","OOe","Szbg","Stmk","T","V","W")
  mutRegion <- c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")
  
  # initialize
  df <- as.data.frame(tables[[2]]) %>% mutate(Region=mutRegion[2])
  
  for(t in 1:length(idx)) {
    dt <- as.data.frame(tables[[idx[t]]]) %>% mutate(Region=mutRegion[t+2])
    df <- rbind(df,dt)
  }

  logMsg(paste("Writing", csvFile))
  if (bSave) write.csv(df, file=csvFile)  
  
  # Convert KW* string to Date
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
  dg  <- rbind(dg,ds)
  
  rdaFile <- "./data/COVID-19-CWM-AGES-States-Mutations.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(dg, file=rdaFile)  

  return(dg)
}




# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline_GKZ.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfGKZtl <- function(csvFile="CovidFaelle_Timeline_GKZ.csv", nRm7Days=7, bSave=TRUE) {
  
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
    dplyr::rename(Region=Bezirk, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rmaNewConfirmed=AnzahlFaelle7Tage) %>%
    dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
    dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
    dplyr::mutate(RegionID=as.character(GKZ), newConfPop = newConfirmed/Population*100000, rmaNewConfPop=as.numeric(str_replace(SiebenTageInzidenzFaelle,",","."))) %>%
    dplyr::select(-Time, -SiebenTageInzidenzFaelle, -GKZ)

  rdaFile <- "./data/CovidFaelle_Timeline_GKZ.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(df, file=rdaFile)  

  # Add Region,RegionID to County
  di <- data.frame(RegionID=as.character(1:9), 
                   Region=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"), stringsAsFactors=FALSE)
  
  # apply rolling mean to 'new*' cols. Leaves the last three days with NA's --> right align, so we get closer to AGES figures
  # some dummy cols so downstream filter don't get confused
  db <- df %>%
    dplyr::rename(County=Region, CountyID=RegionID) %>%
    dplyr::mutate(RegionID=str_sub(CountyID,1,1), CountyNR=str_sub(CountyID,2,3)) %>%
    dplyr::left_join(di, by="RegionID") %>% 
    dplyr::arrange(CountyID, Date) %>%
    dplyr::group_by(CountyID) %>%
    dplyr::mutate(rm7NewConfirmed=(rollmean(newConfirmed, k=nRm7Days, align="right", fill=NA))) %>%
    dplyr::mutate(rm7NewRecovered=(rollmean(newRecovered, k=nRm7Days, align="right", fill=NA))) %>%
    dplyr::mutate(rm7NewDeaths=(rollmean(newDeaths, k=nRm7Days, align="right", fill=NA))) %>%
    dplyr::mutate(rm7NewConfPop=(rollmean(newConfPop, k=nRm7Days, align="right", fill=NA))) %>%
    dplyr::mutate(rm7NewTested=rm7NewConfirmed) %>% 
    dplyr::mutate(rm7NewConfTest=1) %>%
    dplyr::ungroup()%>%
    dplyr::select(Date,Region,RegionID,County,CountyID,CountyNR,Population, starts_with("rma"), starts_with("new"), starts_with("rm7"), starts_with("sum"))
  

  rdaFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"   
  logMsg(paste("Writing", rdaFile))
  if (bSave) saveRDS(db, file=rdaFile)  
  
  return(db)
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
    dplyr::rename(newConfirmed=AnzahlFaelle, newRecovered=AnzahlGeheiltTaeglich, newDeaths=AnzahlTotTaeglich) %>%
    dplyr::rename(sumConfirmed=AnzahlFaelleSum, sumRecovered=AnzahlGeheiltSum, sumDeaths=AnzahlTotSum, rmaNewConfirmed=AnzahlFaelle7Tage) %>%
    dplyr::rename(Region=Bundesland, RegionID=BundeslandID, Population=AnzEinwohner) %>%
    dplyr::mutate(curConfirmed=sumConfirmed-sumRecovered-sumDeaths, rmaNewConfPop=as.numeric(str_replace(SiebenTageInzidenzFaelle,",","."))) %>%
    dplyr::select(Date,Region,RegionID,Population, starts_with("cur"), starts_with("new"), starts_with("rma"), starts_with("sum"))

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


# ----------------------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: History of confirmed cases by dates as reported by AGES (Date of evaluation in lab)
# ----------------------------------------------------------------------------------------------------------
caAgesConfHistory <- function (df, nCalcWeeks=4, nSettleDays=7, dataPath="./data",
                               agesProcessedFile="COVID-19-CWM-AGES-TestedProcessed.rda", 
                               agesEvaluatedFile="COVID-19-CWM-AGES-TestedEvaluated.rda") {
  
  # df as read by caAgesRead_cftl(). i.e. ages data preprocessed into cwm naming conventions (prefiltered by cron.R)
  df <- df %>%
    dplyr::filter(Date>(max(Date)-days(nSettleDays))) %>%
    dplyr::mutate(DateReported = max(Date)) %>%
    dplyr::mutate(DateEvaluated = Date) %>%
    dplyr::mutate(RegionID=as.character(RegionID)) %>%
    dplyr::select(DateReported, DateEvaluated, RegionID, Region, newConfirmed)
  
  # read history accumulated so far (same data format as above)
  dp <- readRDS(paste0(dataPath,"/",agesProcessedFile))
  
  # make sure no double entries and save with today's additions
  dp <- rbind(dp, df)
  dp <- dp %>% 
    dplyr::arrange(RegionID, Region, DateEvaluated, DateReported) %>%
    dplyr::group_by(RegionID, Region, DateEvaluated, DateReported) %>% 
    dplyr::summarize(newConfirmed=max(newConfirmed)) %>%
    dplyr::ungroup()
  
  rdaFile <- paste0(dataPath,"/",agesProcessedFile)
  logMsg(paste("Writing", rdaFile))
  saveRDS(dp,file=rdaFile)
  
  # Enrich data and write to disk
  dq <- dp %>%
    dplyr::mutate(WdayReported=as.character(wday(DateReported, week_start=1, label=TRUE, abbr=TRUE))) %>%
    dplyr::mutate(WdayEvaluated=as.character(wday(DateEvaluated, week_start=1, label=TRUE, abbr=TRUE))) %>%
    dplyr::mutate(MonthEvaluated=as.character(month(DateEvaluated, label=TRUE, abbr=TRUE))) %>%
    dplyr::select(DateEvaluated, MonthEvaluated, WdayEvaluated, DateReported, WdayReported, RegionID, Region, newConfirmed) %>%
    dplyr::arrange(RegionID, Region, DateEvaluated, DateReported) %>%
    dplyr::group_by(DateEvaluated, Region) %>%
    dplyr::mutate(daysDayN=as.integer(DateReported-DateEvaluated)) %>%
    dplyr::mutate(propDayN = newConfirmed/last(newConfirmed)) %>%  
    dplyr::ungroup()
  
  rdaFile <- paste0(dataPath,"/",agesEvaluatedFile)
  logMsg(paste("Writing", rdaFile))
  saveRDS(dq,file=rdaFile)
  
  return(dq %>% dplyr::filter(DateEvaluated>max(DateReported)-weeks(nCalcWeeks)-days(nSettleDays)))
}




# --------------------------------------------------------------------------------------------------------------------
# Join cftl and cfz AGES data files. Add several derived features
# --------------------------------------------------------------------------------------------------------------------
caAgesRead_tlrm <- function(cftlFile="./data/CovidFaelle_Timeline.rda", cfzFile="./data/CovidFallzahlen.rda", bPlot=FALSE, bSave=TRUE,
                            nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                            bResiduals=TRUE, dResFirst=as.Date("2020-07-27"), dResLast=as.Date("2020-11-16"), bShiftDown=TRUE,
                            bDOWCorrection=TRUE, nDOWCorrectionWeeks=4,
                            bAGESTestEvaluationCorrection=TRUE, agesEvaluatedFile=paste0("./data/COVID-19-CWM-AGES-TestedEvaluated.rda"),
                            bPredict=TRUE, nPolyDays=7, nPoly=2,
                            bEstimate=FALSE, bCompleteCases=FALSE) {
  
  # Read timeline of confirmed, hospitalized, deaths
  dc <- readRDS(cftlFile)

  # Read timeline of tested
  dt <- readRDS(cfzFile)

  # need to check if both datasets are available up to the same date
  jointDate <- min(max(dc$Date), max(dt$Date))
  
  # Join 'Confirmed' and 'Tested' datasets
  dj <- dc %>% left_join(dt, by=c("Date","RegionID", "Region")) 
  
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
  
  # remove one potential outlier
  trimMean <- function(x) {
    res <- abs(x-mean(x))
    idx <- which(res!=max(res))
    if(length(idx)==0) idx=1
    mean(x[idx])
  }
  
  # Correct for late reports of newConfirmed counts
  if (bAGESTestEvaluationCorrection) {
    
    # this file is initially generated from past ZIP files and updated by the daily cron job.
    dr <- readRDS(agesEvaluatedFile)
    
    # select those days within nCalcWeeks range that hold converged data as defined by nSettleDays
    dr <- dr %>% 
      # consider nCalcWeeks timerange for correction
      dplyr::filter(DateEvaluated > max(DateReported)-weeks(nCalcWeeks) - days(nSettleDays)) %>%
      # make sure only settled timeranges are considered
      dplyr::filter(DateEvaluated <(max(DateReported)-days(nSettleDays))) %>%
      # Calculate correction by Region, DayOfWeek of Evaluation, for each Evaluation-Report interval 
      dplyr::group_by(Region, WdayReported, daysDayN) %>%
      dplyr::summarize(countPropDayN = n(), meanPropDayN = round(trimMean(propDayN),2)) %>%
      dplyr::ungroup() 
    
    # correct last three days including today
    for (k in 0:2)  {
      toDay <- max(df$Date)-days(k)
      weekDay <- wday(toDay, label=TRUE, abbr=TRUE)
      meanProp <- dr %>% filter(daysDayN==k, WdayReported==weekDay) %>% dplyr::select(meanPropDayN)
      df$newConfirmed[df$Date==toDay] <- round(df$newConfirmed[df$Date==toDay]/meanProp$meanPropDayN)
    }
  }
  
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
      dplyr::arrange(Region,Date)
    
    # patch newConfirmed with dowNewConfirmed. This relies on same sort order: Region,Date    
    df$newConfirmed[df$Date>(max(df$Date)-days(3))] <- dq$dowNewConfirmed
  }

  # Calculate 'shorted' week means for last three days:
  # This is provided as 'raw' data for downstream processing.
  # e.g. calculation of today's incidence rate may need to do more postprocessing, e.g. fitting a second order LS model 
  # d-3 = mean(last 7)
  # d-2 = mean(last 5)
  # d-1 = mean(last 3)
  # d-0 = last
  
  # New variant as of 2020-02-03:
  # 1.) patch NA's of rm7* cols of last three days with new* features
  # 2.) calc rm7 (day-2) as mean of rm7 of last 5 days (includes two actural rm7)
  # 3.) calc rm7 (day-1) as mean of rm7 of last 3 days (include one actual and the previously calculated)
  # 4.) leave rm7 od today as is (just DOW corrected new) or original value for other features
  
  patch531 <- function(new,rm7) {
    if (length(new)==5) return(mean(c(rm7[1:2],new[3:5])))
    if (length(new)==3) return(mean(c(rm7[1],new[2:3])))
    if (length(new)==1) return(new[1])
    return(NULL)
  }
  
  # Calculate rm7 of last three days
  maxDate <- max(df$Date)
  # work all cols with rolling means by week
  rm7s <- colnames(df)[startsWith(colnames(df),"rm7")]
  # calc mean of last 5 days, last 3 days and last day as estimate of day before yesterday, yesterday, today (in this order)
  for(d in c(2,1,0)) {
    for (rm7 in rm7s) {
      # select item for all regions
      #r <- df %>% 
      #  dplyr::filter(Date>=maxDate-days(d*2)) %>% 
      #  dplyr::select(Region,x=paste0(tolower(substr(!!rm7, 4,4)),substr(!!rm7,5,100))) %>% 
      #  dplyr::group_by(Region)  %>%
      #  dplyr::summarize(rm7=mean(x)) %>%
      #  dplyr::ungroup() %>% 
      #  dplyr::select(rm7)
      
      # incremental build of rm7 from available rm7 and new
      s <- df %>% 
        dplyr::filter(Date>=(maxDate-days(d*2))) %>% 
        dplyr::select(Region,x=paste0(tolower(substr(!!rm7, 4,4)),substr(!!rm7,5,100)),y=!!rm7) %>% 
        dplyr::group_by(Region)  %>%
        dplyr::summarise(rm7=patch531(x,y))  %>%
        dplyr::ungroup() %>% 
        dplyr::select(rm7)

      df[df$Date==(maxDate-days(d)),rm7] <- s 
    }
  }
  
  # recalc rm7ConfPop
  df <- df %>% dplyr::mutate(rm7NewConfPop=rm7NewConfirmed/Population*100000)
  
    
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
  
  # Calculate regression on log transformed 
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

