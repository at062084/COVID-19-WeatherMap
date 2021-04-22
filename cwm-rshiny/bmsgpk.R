library(imputeTS)
library(zoo)

options(error = function() traceback(2))

#setwd("/srv/shiny-server/COVID-19-WeatherMap")
#setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny")
#setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny")
source("fun.R")

BL <- data.frame(ID=c("AT","B","K","Noe","Ooe","Szbg","Stmk","T","V","W"),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)


# Download CoronaAmpel file and Scrape Dashboard data
caBmsgpkScrapeDashBoard <- function(ts=format(now(),"%Y%m%d-%H%M"), htmlPath="./html", ampelPath="./ampel") {
  
  # wget -O CoronaAmpel-20200904.js https://corona-ampel.gv.at/sites/corona-ampel.gv.at/files/assets/Warnstufen_Corona_Ampel_aktuell.json
  logMsg("Running caBmsgpkScrapeDashBoard ...")
  
  ampelFile <- paste0(ampelPath,"/CoronaAmpel.",ts,".js")
  url="\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\""
  logMsg(paste("Download Ampel data from", url))
  logMsg(paste("Storing Ampel data to", ampelFile))
  cmd <- paste(url, "-O", ampelFile)
  system2("wget", cmd)
  
  # get html page from bmsgpk
  bmsgpkFile <- paste0(htmlPath,"/COVID-19-austria.bmsgpk.",ts,".html")
  url="\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\""
  logMsg(paste("Scraping", url))
  logMsg(paste("Dumping page to", bmsgpkFile))
  cmd <- paste(url, "-O", bmsgpkFile)
  system2("wget", cmd)
  
  logMsg(paste("Parsing dump in", bmsgpkFile))
  html <- xml2::read_html(bmsgpkFile)
  
  logMsg(paste("Extracting Status table for Bundesländer"))
  tables <- rvest::html_table(html, dec=",", fill=TRUE)
  dx <- tables[[1]][,1:11]
  
  # Extract Stamp and Status from first col
  S0 <- dx[,1]
  S2 <- str_split_fixed(S0, "\\(", n=2)[,1]
  Status <- str_split_fixed(S2, "\\*",n=2)[,1]
  Status <- str_match(Status,"[a-zA-Z0-9äöüÄÖÜß ]*")
  Status <- trimws(Status)
  # Must watchout for invisible blank characters that are encoded as '&nbsp;' in original html
  Stamp <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[,2],"[^0-9:.,]",""),format="%d.%m.%Y,%H:%M")
  # Be tolerant to time stamp format changes on website
  idx <- which(is.na(Stamp))
  if (length(idx)>0) {
    Stamp[idx] <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[idx,2],"[^0-9:.,]",""),format="%d.%m.%Y,%H.%M")
  }
  # Be tolerant to time stamp format changes on website
  idx <- which(is.na(Stamp))
  if (length(idx)>0) {
    Stamp[idx] <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[idx,2],"[^0-9:.,]",""),format="%d.%m.%Y.%H:%M")
  }
  
  df <- dx %>%
    dplyr::select(11,2:10) %>% 
    mutate_all(funs(str_replace_all(., "[\\.\\*, ◊kA]", ""))) %>% 
    mutate_all(funs(as.integer(.))) %>%
    # need this one (!)
    dplyr::mutate(Stamp=Stamp,Status=Status)  %>%
    dplyr::select(Stamp,Status,1:10) 
  colnames(df) <- c("Stamp","Status",BL$ID)
  
  # Rename Status to previous Labels
  StatusMap <- data.frame(
    from=c("Bestätigte Fälle","Todesfälle","Genesen","Hospitalisierung","Intensivstation","Testungen","Davon PCR","Davon Antigen"),
    to=c("Confirmed","Deaths","Recovered","Hospitalisierung","Intensivstation","Tested","Tested_PCR","Tested_AG"), stringsAsFactors=FALSE)
  
  for (s in 1:nrow(df)) {
    n = which(df$Status[s]==StatusMap$from)
    df$Status[s] <- StatusMap$to[n]
  }

  return(df) 
}


caBmsgpkCurateDashboard <- function(dataPath="./data",
                               bmsgkpScrapedFile="COVID-19-CWM-BMSGPK-Dashboard.scraped.rda", 
                               bmsgkpCuratedFile="COVID-19-CWM-BMSGPK-Dashboard.curated.rda") {

  logMsg("Running caBmsgpkCurateDashboard ...")
  
  # Weird data in orignal data. Replace by NA's. Manually maintained list of visual spikes
  dp <- data.frame(t(data.frame(
    c("2021-03-26","V","Tested",NA),
    c("2021-03-26","V","Tested_AG",NA),
    c("2021-03-18","V","Tested_PCR",NA),
    c("2021-03-24","V","Tested_PCR",NA),
    c("2021-03-25","V","Tested_PCR",NA),
    c("2021-03-24","W","Tested_PCR",NA),
    c("2021-03-25","W","Tested_PCR",NA),
    c("2021-03-27","W","Tested_PCR",NA),
    c("2021-03-11","W","Tested_AG",NA),
    c("2021-04-05","W","Tested_AG",NA),
    c("2021-03-26","W","Tested_AG",2129738)
  )), stringsAsFactors=FALSE)
  colnames(dp) <- c("Date","Region","Status","Count")
  rownames(dp) <- NULL
  dp <- dp %>% dplyr::mutate(Date=as.Date(Date), Count=as.numeric(Count))
  
  
  # Weird data in orignal data. Replace by NA's. Manually maintained list of visual spikes
  de <- data.frame(t(data.frame(
    #  c("2021-03-26","Wien",	"newTested"),
    #  c("2021-03-26","Wien",	"newTested_AG"),
    #  c("2021-03-27","Wien","newTested_AG"),
    c("2021-01-18","Burgenland","newTested_AG"),
    c("2021-01-18","Burgenland","newTested"),
    c("2021-02-25","Burgenland","newTested_PCR"),
    #  c("2021-01-28","Kärnten","newConfirmed"),
    #  c("2021-01-14","Kärnten","newRecovered"),
    #  c("2021-01-20","Niederösterreich","newTested"),
    #  c("2021-01-20","Niederösterreich","newTested_AG"),
    #  c("2021-01-14","Oberösterreich","newTested"),
    #  c("2021-01-14","Oberösterreich","newTested_AG"),
    #  c("2021-01-14","Oberösterreich","newRecovered"),
    #  c("2021-01-10","Steiermark","newRecovered"),
    #  c("2021-01-22","Salzburg","newRecovered"),
    #  c("2021-01-28","Tirol","newConfirmed"),
    #  c("2021-01-28","Österreich","newConfirmed"),
    c("2021-01-28","Vorarlberg","newConfirmed"))), stringsAsFactors=FALSE)
  colnames(de) <- c("Date","Region","newStatus")
  rownames(de) <- NULL
  de <- de %>% dplyr::mutate(Date=as.Date(Date))
  
  # df must have cols Date and Count
  smoothCumSumStep <- function(df, Date, numDays=7) {
    
    d <- df[df$Date==Date, "Count"] # Count at top of step
    s <- d - df[df$Date==(Date-days(1)),"Count"] # Step Size to preious day
    n <- df[df$Date==(Date+days(1)),"Count"] - d  # Step size to next day
    dc <- round((s -n)/numDays) # Add calculated step (cumsum to propagate from previous day)
    ds <- cumsum(rep(dc,numDays))
    df[df$Date>=(Date-days(numDays)) & df$Date<Date,"Count"] <- df[df$Date>=(Date-days(numDays)) & df$Date<Date,"Count"] + ds 
    return(df)
  }
  
  # Process raw scraped data
  rdaFile=paste0(dataPath,"/",bmsgkpScrapedFile)
  logMsg(paste(" Reading raw timeline dashboard data from", rdaFile))
  da <- readRDS(rdaFile)
  
  # reduce to one record per day
  da <- da %>%
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp,Date,Status) %>%
    dplyr::group_by(Date,Status) %>%
    dplyr::summarize_all(first) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Status, AT:W, -Stamp) %>%
    dplyr::filter(Date>=as.Date("2021-01-04"))
  
  # Correct Testing Data in V (PCR and AG counts mistaken from 2021-03-17 till 2021-03-26)
  # Correction seems to be more or less OK (just anomalies corrected)
  idx_PCR <- da$Date>=as.Date("2021-03-17") & da$Date<=as.Date("2021-03-26") & da$Status=="Tested_PCR"
  idx_AG  <- da$Date>=as.Date("2021-03-16") & da$Date<=as.Date("2021-03-25") & da$Status=="Tested_AG"
  # AG were counted instead of PCR for PCR
  da$V[idx_PCR] <- da$V[idx_PCR]-(da$V[idx_AG]-da$V[idx_AG][1])
  # but new PCR tests was not added, presumably even subtracted. So add fake data taken from previous week twice
  idx_fake_PCR <- da$Date>=as.Date("2021-03-08") & da$Date<=as.Date("2021-03-17") & da$Status=="Tested_PCR"
  fake_PCR <- da$V[idx_fake_PCR] - da$V[idx_fake_PCR][1]
  da$V[idx_PCR] <- da$V[idx_PCR] + 2* fake_PCR 
  d_PCR <- da$V[da$Status=="Tested_PCR" & da$Date==as.Date("2021-03-26")] - da$V[da$Status=="Tested_PCR" & da$Date==as.Date("2021-03-28")]
  da$V[da$Status=="Tested_PCR" & da$Date>=as.Date("2021-03-28")] <- da$V[da$Status=="Tested_PCR" & da$Date>=as.Date("2021-03-28")] + d_PCR
  # Shift Tested_AG the same amount Tested_PCR was shifted, and add the same fake_PCR numbers
  da$V[da$Status=="Tested_AG" & da$Date>=as.Date("2021-03-28")] <- da$V[da$Status=="Tested_AG" & da$Date>=as.Date("2021-03-28")] - d_PCR + max(fake_PCR)*2
  da$V[da$Status=="Tested"] <- da$V[da$Status=="Tested_PCR"] + da$V[da$Status=="Tested_AG"]
  
  
  # Patch NA's into obvious data problem points. These will be imputated by linear interpolation
  for (i in 1:dim(dp)[1]) {
    da[da$Date==dp$Date[i] & da$Status==dp$Status[i],as.character(dp$Region[i])] <- dp$Count[i]
  }
  
  # Smooth steps in cumsum data reports
  db <- da %>% 
    tidyr::gather(key=Region, val=Count, AT:W)
  
  # Work steps to smooth
  db[db$Region=="B" & db$Status=="Tested",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested"), Date=as.Date("2021-01-09"), numDays=5) 
  db[db$Region=="B" & db$Status=="Tested",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested"), Date=as.Date("2021-01-18")) 
  db[db$Region=="B" & db$Status=="Tested",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested"), Date=as.Date("2021-02-20")) 
  db[db$Region=="B" & db$Status=="Tested_AG",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested_AG"), Date=as.Date("2021-01-09"), numDays=5) 
  db[db$Region=="B" & db$Status=="Tested_AG",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested_AG"), Date=as.Date("2021-01-18")) 
  db[db$Region=="B" & db$Status=="Tested_AG",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested_AG"), Date=as.Date("2021-02-20")) 
  db[db$Region=="B" & db$Status=="Tested_PCR",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested_PCR"), Date=as.Date("2021-01-19")) 
  db[db$Region=="B" & db$Status=="Tested_PCR",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="B",Status=="Tested_PCR"), Date=as.Date("2021-02-25")) 
  db[db$Region=="V" & db$Status=="Tested_PCR",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="V",Status=="Tested_PCR"), Date=as.Date("2021-04-07"), numDays=14) 
  db[db$Region=="W" & db$Status=="Tested",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="W",Status=="Tested"), Date=as.Date("2021-03-26"), numDays=35) 
  db[db$Region=="W" & db$Status=="Tested_AG",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="W",Status=="Tested_AG"), Date=as.Date("2021-03-27"), numDays=35) 
  
  db <- db %>% tidyr::spread(key=Region, val=Count)

  # Run linear imputation of NA's in time series
  dg <- db %>%
    tidyr::gather(key=Regions, value=Count, AT:W) %>%
    dplyr::arrange(Date, Status, Regions) %>%
    dplyr::group_by(Status, Regions) %>%
    dplyr::mutate(Count=imputeTS::na_interpolation(Count,option="linear")) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(datATRegions %>% dplyr::select(RegionID, Region, Regions, Population), by="Regions") %>%
    dplyr::select(Date, RegionID, Region, Population, Status, Count, -Regions)
  
  # Remove weird data points where data cannot be zero or less than zero
  mutZle2NA <- function(x) {ifelse(x<=0,NA,x)}
  mutZlt2NA <- function(x) {ifelse(x<0,NA,x)}
  
  # Calculate daily differences and normalize to 100.000 inhabitants
  dd <- dg %>%
    tidyr::spread(key=Status, value=Count) %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(newTested=(Tested-lag(Tested))/Population*100000) %>%
    dplyr::mutate(newTested_AG=(Tested_AG-lag(Tested_AG))/Population*100000) %>%
    dplyr::mutate(newTested_PCR=(Tested_PCR-lag(Tested_PCR))/Population*100000) %>%
    # Impute any negative or Zero entries
    dplyr::mutate_at(vars(starts_with("newTested")), mutZle2NA) %>%
    dplyr::mutate(newConfirmed=(Confirmed-lag(Confirmed))/Population*100000) %>%
    dplyr::mutate(newDeaths=(Deaths-lag(Deaths))/Population*100000) %>%
    dplyr::mutate(newRecovered=(Recovered-lag(Recovered))/Population*100000) %>%
    dplyr::mutate(curHospital=Hospitalisierung/Population*100000) %>%
    dplyr::mutate(curICU=Intensivstation/Population*100000) %>%
    # Impute any negative entries
    dplyr::mutate_at(vars(starts_with("new")), mutZlt2NA) %>%
    dplyr::ungroup()
  
  
  dn <- dd %>%
    dplyr::select(Date, Region, starts_with("cur"), starts_with("new")) %>%
    tidyr::gather(key=Status, value=Count, starts_with("cur"), starts_with("new"))
  
  # Some more NA's for linear imputation
  for (i in 1:dim(de)[1]) {
    dn$Count[dn$Date==de$Date[i] & dn$Region==de$Region[i] & dn$Status==de$Status[i]] <- NA
  }
  
  # Calculate Confirmed/Tested ratios
  dm <- dn %>%
    dplyr::group_by(Region, Status) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(Count=imputeTS::na_interpolation(Count,option="linear")) %>%
    dplyr::mutate(Count=rollmean(Count,k=7,align="center",na.pad=TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key=Status, value=Count) %>%
    dplyr::mutate(relConfTest=newConfirmed/newTested) %>%
    dplyr::mutate(relConfTest_AG=newConfirmed/newTested_AG) %>%
    dplyr::mutate(relConfTest_PCR=newConfirmed/newTested_PCR) %>%
    tidyr::gather(key=Status, value=Count, starts_with("cur"), starts_with("new"), starts_with("rel")) %>%
    dplyr::mutate(Type=substr(Status,1,3)) %>%
    dplyr::filter(Date>min(Date)+days(7))

  
  rdaFile <- paste0(dataPath,"/", bmsgkpCuratedFile)
  logMsg(paste(" Writing curated data to", rdaFile))
  saveRDS(dm, rdaFile)
    
  return(dm)
}  


# Scrape bmsgpk Dashboard and append data to raw data history file
caBmsgpkUpdateDashboard <- function(dataPath="./data",
                                    bmsgkpScrapedFile="COVID-19-CWM-BMSGPK-Dashboard.scraped.rda") {
  
  logMsg("Running caBmsgpkUpdateDashboard ...")
  logMsg(paste(" Updating bmsgpk dashboard data"))
  dm <- caBmsgpkScrapeDashBoard()
  
  df <- readRDS(paste0(dataPath,"/",bmsgkpScrapedFile))
  df <- rbind(df, dm) %>% unique()
  
  rdaFile <- paste0(dataPath,"/",bmsgkpScrapedFile)
  logMsg(paste(" Appending new data to", rdaFile))
  saveRDS(df, rdaFile)
  
  logMsg(paste(" Curating data"))
  dm <- caBmsgpkCurateDashboard()
}



