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
library(xml2)
library(rvest)
library(ggplot2)


# -------------------------------------------------------------------------------------------
# CWM: GIT commit and push
# -------------------------------------------------------------------------------------------
caGitCommitPush <- function() {
  
  cmd <- paste0("\"git add ./data/*.csv\"")
  system2("git", cmd)
  
  cmd <- paste0("\"git push\"")
  system2("git", cmd)
  
  return(0)
}



# -------------------------------------------------------------------------------------------
# StatA: General Mortility Österreich
# -------------------------------------------------------------------------------------------
caStatAData_p_ag <- function() {
  
  # "Alter in 5-Jahresgruppen","Values","Time section","Gender <2>","Number","Annotations"
  fName <- "./data/StatA-Population-2019_AgeGroup_Gender.csv"
  col.names=c("AgeGroup20","Values","Year","Gender","Population","Annotation")
  colClasses=c("character","NULL","character", "character","character","NULL")
  
  df <- read.csv(fName, header=FALSE, skip=7, sep=",", stringsAsFactors=FALSE, 
                 col.names=col.names, colClasses=colClasses, na.strings="not applicable") %>%
    dplyr::filter(Gender=="male" | Gender=="female") %>%
    dplyr::mutate(Population=as.integer(Population)) 
  
  # remove additional AgeGroups
  df <- df[1:40,]
  
  df <- df %>%
    dplyr::mutate(AgeGroup20=factor(AgeGroup20, levels=unique(df$AgeGroup20), ordered=TRUE),
                  AgeGroupID20=as.integer(AgeGroup20), 
                  AgeGroup2=factor(ifelse(AgeGroupID20>=14,"Über65","Unter65"), levels=c("Unter65","Über65")),
                  Gender = factor(Gender, levels=c("male","female"), labels=c("M","W")))
  
  return(df)
}

# -------------------------------------------------------------------------------------------
# StatA: General Mortility Österreich
# -------------------------------------------------------------------------------------------
caStatAData_m_ag <- function() {
  
  fName <- "./data/StatA-Sterbefaelle-Year_AgeGroup_Gender.csv"
  col.names=c("Year","AgeGroup20","xxx","Gender","Deceased","y")
  colClasses=c("character","character","NULL", "character","character","NULL")
  df <- read.csv(fName, header=TRUE, skip=7, sep=",", stringsAsFactors=FALSE, 
                 col.names=col.names, colClasses=colClasses, na.strings="not applicable") %>%
    dplyr::filter(Gender=="male" | Gender=="female") %>%
    dplyr::filter(!is.na(AgeGroup20))
  
  df <- df %>%
    dplyr::mutate(Deceased=as.integer(Deceased)) %>%
    dplyr::mutate(Gender = factor(Gender, levels=c("male","female"), labels=c("M","W"))) %>%
    dplyr::mutate(AgeGroup20=factor(dd$AgeGroup20, levels=df$AgeGroup20 %>% unique(), ordered=TRUE)) %>%
    dplyr::mutate(AgeGroupID20=as.integer(AgeGroup20)) %>%
    dplyr::mutate(AgeGroup2=factor(ifelse(AgeGroupID20>=14,"Über65","Unter65"), levels=c("Unter65","Über65"))) %>%
    dplyr::arrange(Year, AgeGroupID20, Gender)
  
  return(df)
}



# -------------------------------------------------------------------------------------------
# BMSGPK: timeline-eimpfpass.csv: TimeLine Impfungen (Region, AgeGroup, Gender, Shot1, Shot2)
# -------------------------------------------------------------------------------------------
caBmsgpkData_v_rag <- function(rollMeanDays=7) {
  
  url <- "https://info.gesundheitsministerium.gv.at/data/timeline-eimpfpass.csv"
  df<- read.csv(url, header=TRUE, sep=";", stringsAsFactors=FALSE) %>%
    dplyr::mutate(Date=as.POSIXct(Datum))
  # str(df)
  
  df <- df %>%
    dplyr::rename(RegionID=BundeslandID, Region=Name, Population=Bevölkerung, AgeX_X_X=Gruppe_NichtZuordenbar) %>%
    dplyr::select(Date, RegionID, Region, starts_with("Gruppe_")) %>%
    dplyr::rename_at(vars(starts_with("Gruppe_")), list(~ paste0("Age",substring(.,8)))) %>%
    tidyr::pivot_longer(starts_with("Age"), names_sep="_", names_to=c("AgeGroup","Gender","ShotNo"), values_to="sumVaccinated") %>%
    dplyr::filter(Gender!="D", Region!="KeineZuordnung") %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age.15"), "5-14")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age.84"), ">84")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age"), "")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("."), "-")) %>%
    dplyr::mutate(AgeGroup =factor(AgeGroup, levels=levels(dcrd$AgeGroup), ordered=TRUE)) %>%
    tidyr::pivot_wider(names_from=ShotNo, names_prefix="sumVaccinated_", values_from=sumVaccinated)
  
  di <- df %>%
    dplyr::mutate(AgeGroup3=factor(AgeGroup, 
                                   levels=levels(df$AgeGroup), 
                                   labels=c(rep("Jugendliche",3), rep("Erwachsene",4), rep("Senioren",3)), ordered=TRUE)) %>%
    dplyr::arrange(Region, RegionID, AgeGroup, Gender, Date) %>%
    dplyr::group_by(Region, RegionID, AgeGroup, Gender) %>%
    dplyr::mutate(sumVaccinated_1=rollmean(sumVaccinated_1, k=rollMeanDays, align="center", fill=0)) %>%
    dplyr::mutate(sumVaccinated_2=rollmean(sumVaccinated_2, k=rollMeanDays, align="center", fill=0)) %>%
    dplyr::mutate(newVaccinated_1=sumVaccinated_1-dplyr::lag(sumVaccinated_1)) %>%
    dplyr::mutate(newVaccinated_2=sumVaccinated_2-dplyr::lag(sumVaccinated_2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Region, RegionID, AgeGroup, AgeGroup3, Gender, starts_with("new"), starts_with("sum"))
  
  idx <- is.na(di$newVaccinated_1)
  di$newVaccinated_1[idx] <- 0
  idx <- is.na(di$newVaccinated_2)
  di$newVaccinated_2[idx] <- 0
  
  # str(di)
  return(di)
}

# -------------------------------------------------------------------------------------------
# BMSGPK: timeline-eimpfpass.csv: TimeLine Impfungen (Region, AgeGroup, Gender, Shot1, Shot2)
# -------------------------------------------------------------------------------------------
caBmsgpkData_crdhit_r0 <- function(rollMeanDays=7) {
  
  atRegionsShort=c("B","K","Noe","Ooe","AT","Szbg","Stmk","T","V","W")
  atRegions=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Österreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien")
  
  url <- "https://raw.githubusercontent.com/at062084/COVID-19-Austria/master/bmsgpk/data/COVID-19-austria.csv"
  df<- read.csv(url, header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    tidyr::pivot_longer(cols=c("AT","B","K","Noe","Ooe","Szbg","Stmk","T","V","W"), names_to="Region", values_to="Count") %>%
    dplyr::mutate(Region=factor(Region, levels=atRegionsShort, labels=atRegions)) %>%
    tidyr::pivot_wider(id_cols=c("Stamp", "Status","Region"), values_fn = list(Count=mean), names_from="Status", values_from="Count") %>%
    dplyr::rename(sumConfirmed=Confirmed, sumDeath=Deaths, sumRecovered=Recovered, 
                  sumTested=Tested, sumTestedPCR=Tested_PCR, sumTestedAG=Tested_AG,
                  curHospital=Hospitalisierung, curICU=Intensivstation) %>%
    # handle NA's
    dplyr::mutate(Date=as.POSIXct(format(Stamp,"%Y-%m-%d"))) %>%
    dplyr::group_by(Date, Region) %>%
    dplyr::summarize_all(.groups="drop", .funs=mean, na.rm=TRUE) %>%
    # tidy cols
    dplyr::select(Date, Region, starts_with("cur"), starts_with("sum"))
  str(df)
  
  crdhit_r0 <-df  %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(dplyr::across(starts_with("sum"), ~ rollmean(.x, k=rollMeanDays, align="center", fill=NA))) %>%
    dplyr::mutate(newTested=sumTested-dplyr::lag(sumTested),
                  newConfirmed=sumConfirmed-dplyr::lag(sumConfirmed), 
                  newRecovered=sumRecovered-dplyr::lag(sumRecovered), 
                  newDeath=sumDeath-dplyr::lag(sumDeath)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Region, starts_with("new"), starts_with("cur") , starts_with("sum"))
  str(crdhit_r0)
  
  return(crdhit_r0)
}



# -------------------------------------------------------------------------------------------
# AGES: zipFile
# -------------------------------------------------------------------------------------------
caAgesData_zipFile <- function() {
  
  zipFile=paste0("./data/data.zip")
  unzipDir="./data"
  
  url="https://covid19-dashboard.ages.at/data/data.zip"
  cmd <- paste0("\"",url,"\"", " -O ", zipFile)
  system2("wget", cmd)
  
  cmd <- paste("-fo", zipFile, "-d", unzipDir)
  system2("unzip",cmd)
  
  return(0)
}

# -------------------------------------------------------------------------------------------
# AGES: CovidFaelle_Altersgruppe.csv: Confirmed, Recovered, Deaths by  Region, AgeGroup10, Gender
# -------------------------------------------------------------------------------------------
caAgesData_crd_rag <- function(rollMeanDays=7, bSave=TRUE) {
  
  # df <- read.csv(".//AgeGroup/CovidFaelle_Altersgruppe.csv", sep=";", stringsAsFactors=FALSE)
  
  url <- "https://covid19-dashboard.ages.at/data/CovidFaelle_Altersgruppe.csv"
  df<- read.csv(url, header=TRUE, sep=";", stringsAsFactors=FALSE)
  
  dcrd <- df %>%
    # Adjust data types, add groups and confirm to naming conventions
    dplyr::mutate(Date=as.POSIXct(Time, format="%d.%m.%Y")) %>%
    dplyr::mutate(Altersgruppe=fct_reorder(factor(df$Altersgruppe, ordered=TRUE),df$AltersgruppeID,sum)) %>%
    dplyr::rename(AgeGroup=Altersgruppe, AgeGroupID=AltersgruppeID, 
                  Region=Bundesland, RegionID=BundeslandID, Population=AnzEinwohner) %>%
    dplyr::mutate(AgeGroup2=ifelse(AgeGroupID<=7,"Unter65","Über65")) %>%
    dplyr::mutate(AgeGroup2 = factor(AgeGroup2, levels=c("Unter65","Über65"), ordered=TRUE)) %>%
    dplyr::rename(sumConfirmed=Anzahl, sumRecovered=AnzahlGeheilt, sumDeath=AnzahlTot, Gender=Geschlecht) %>%
    dplyr::mutate(Gender=factor(Gender)) 
    
  
  dcrd <- dcrd %>%
    dplyr::mutate(AgeGroup3=factor(AgeGroup, levels=levels(dcrd$AgeGroup), labels=c(rep("Jugendliche",3), rep("Erwachsene",4), rep("Senioren",3)), ordered=TRUE)) %>%
    # smooth for weekly means
    dplyr::arrange(Region, AgeGroup, Gender, Date) %>%
    dplyr::group_by(Region, AgeGroup, Gender) %>%
    dplyr::mutate(dplyr::across(starts_with("sum"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=0))) %>%
    # calc new* from sum*
    #dplyr::mutate(dplyr::across(starts_with("new"), ~ .x - dplyr::lag(.x))) %>%
    dplyr::mutate(newConfirmed=sumConfirmed-dplyr::lag(sumConfirmed), 
                  newRecovered=sumRecovered-dplyr::lag(sumRecovered), 
                  newDeath=sumDeath-dplyr::lag(sumDeath)) %>%
    dplyr::ungroup() %>%
    
    # Beautify data frame
    dplyr::select(Date, Region, RegionID, AgeGroup, AgeGroupID, AgeGroup2, AgeGroup3, Gender, Population, starts_with("new"), starts_with("sum"))
  
  idx <- is.na(dcrd$newConfirmed)
  dcrd$newConfirmed[idx] <- 0
  idx <- is.na(dcrd$newRecovered)
  dcrd$newRecovered[idx] <- 0
  idx <- is.na(dcrd$newDeath)
  dcrd$newDeath[idx] <- 0
  
  # str(dcrd)
  return(dcrd)
}

# -------------------------------------------------------------------------------------------
# AGES: Inzidenz_vollstaendig_unvollstaendig_geimpft_2021-08-20.csv: Confirmed, Vaccinated by AgeGroup3
# -------------------------------------------------------------------------------------------
caAgesData_cv_a3 <- function(rollMeanDays=7, bSave=TRUE) {
  
  # new files as of 2021-08-27
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_12bis17Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_18bis59Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_60plus.csv
  
  # Data seem to be published with a timestamp in the filename
  yesterday = format(now() -days(1), "%Y-%m-%d")
  yesterday = "2021-08-20"
  
  baseurl <- "https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus"
  url <- paste0(baseurl, "/Inzidenz_vollstaendig_unvollstaendig_geimpft_", yesterday,".csv")
  colnames=c("Datum","Date","AgeGroup3", "Immunized", "newInzidenz7")
  df<- read.csv(url, header=FALSE, skip=1, sep=";", dec=",", stringsAsFactors=FALSE, col.names=colnames) 
  
  dcv <- df %>% 
    dplyr::select(-1) %>%
    dplyr::mutate(Date=as.POSIXct((Date))) %>%
    dplyr::mutate(Immunized=factor(Immunized, levels=c("Ja","Nein"), labels=c("Ja","Nein"))) %>%
    dplyr::mutate(AgeGroup3=factor(AgeGroup3, 
                                   levels=c("12-17 Jahre","18-59 Jahre", "60+ Jahre"), 
                                   labels=c("Jugendliche","Erwachsene","Senioren"), ordered=TRUE)) %>%
    tidyr::pivot_wider(names_from="Immunized", values_from="newInzidenz7", names_prefix="inz7Immunized")
  # str(dcv)
  
  if (bSave) {
    yesterday = format(now() -days(1), "%Y%m%d")
    rdaFile <- paste0("./data/COVID-19-CWM-AGES-ConfVacc-",yesterday,".rda")   
    # logMsg(paste("Writing", rdaFile))
    saveRDS(dcv, file=rdaFile)  
  }
  return(dcv)
}

# -------------------------------------------------------------------------------------------
# AGES: Inzidenz_vollstaendig_unvollstaendig_geimpft_2021-08-20.csv: Confirmed, Vaccinated by AgeGroup3
# -------------------------------------------------------------------------------------------
caAgesData_z_a3 <- function(rollMeanDays=7, bSave=FALSE) {
  
  # new files as of 2021-08-27
  Urls=c(
  "https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_12bis17Jahre.csv",
  "https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_18bis59Jahre.csv",
  "https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_60plus.csv")
  AgeGroups3=c("Jugendliche","Erwachsene","Senioren")
  
  col.names=c("Date","newInzImmuYes","newInzImmuNo")
  colClasses = c("character", rep("numeric",2))
  for (i in 1:length(Urls)) {
    df <- read.csv(Urls[i], header=FALSE, skip=1, sep=";", dec=",", stringsAsFactors=FALSE, col.names=col.names, colClasses=colClasses) 
    df$AgeGroup3 <- AgeGroups3[i]
    # Lots of empty rows
    df <- df[complete.cases(df),]
    if(i==1)
      dfs <- df
    else
      dfs <- rbind(dfs,df)
  }
  za3 <- dfs %>%
    dplyr::mutate(Date=as.POSIXct(Date, format="%d.%m.%Y")) %>%
    dplyr::mutate(AgeGroup3=factor(AgeGroup3, levels=AgeGroups3, labels=AgeGroups3, ordered=TRUE)) %>%
    # scale from 7-TagesInzidenz to Inzidenz
    dplyr::mutate(newInzImmuYes=newInzImmuYes/7, newInzImmuNo=newInzImmuNo/7) %>%
    dplyr::select(Date, AgeGroup3, starts_with("new"))
  #str(za3)
  #summary(za3)

  if (bSave) {
    rdaFile <- paste0("./data/COVID-19-CWM-AGES-ConfVacc-AgeGroup3.rda")   
    # logMsg(paste("Writing", rdaFile))
    saveRDS(za3, file=rdaFile)  
  }
  return(za3)
}

# -------------------------------------------------------------------------------------------
# AGES: Inzidenz_vollstaendig_unvollstaendig_geimpft_2021-08-20.csv: Confirmed, Vaccinated by AgeGroup3
# -------------------------------------------------------------------------------------------
caAgesData_hi_r <- function(rollMeanDays=7, bSave=TRUE) {

  url <- "https://covid19-dashboard.ages.at/data/Hospitalisierung.csv"
  df<- read.csv(url, header=TRUE, sep=";", stringsAsFactors=FALSE) %>%
    dplyr::mutate(Date=as.POSIXct(Meldedatum, format="%d.%m.%Y %H:%M:%S")) %>%
    dplyr::select(c(Date,-1,-2,3,4,6)) %>%
    dplyr::rename(Region=Bundesland, curHospital=NormalBettenBelCovid19, curICU=IntensivBettenBelCovid19) %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(dplyr::across(starts_with("cur"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=NA))) %>%
    dplyr::ungroup()
  # str(df)

  return(df)
}
  
# -------------------------------------------------------------------------------------------
# AGES: Inzidenz_vollstaendig_unvollstaendig_geimpft_2021-08-20.csv: Confirmed, Vaccinated by AgeGroup3
# -------------------------------------------------------------------------------------------
caAgesData_crdhit_r <- function(rollMeanDays=7, bSave=TRUE) {
  
  url <- "https://info.gesundheitsministerium.gv.at/data/timeline-faelle-bundeslaender.csv"
  df<- read.csv(url, header=TRUE, sep=";", stringsAsFactors=FALSE) %>%
    dplyr::mutate(Date=as.POSIXct(Datum)) %>%
    dplyr::select(-1) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Name, 
                  sumConfirmed=BestaetigteFaelleBundeslaender, sumDeath=Todesfaelle, sumRecovered=Genesen, 
                  sumTested=Testungen, sumTestedPCR=TestungenPCR, sumTestedAG=TestungenAntigen,
                  curHospital=Hospitalisierung, curICU=Intensivstation) %>%
    dplyr::select(Date, RegionID, Region, starts_with("cur"), starts_with("sum"))
  # str(df)
  
  crdhit <- df %>% 
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(
                  # test: don't smooth the hospitaö and icu current readings 
                  # dplyr::across(starts_with("cur"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=NA)),
                  dplyr::across(starts_with("sum"), ~ rollmean(.x, k=rollMeanDays, align="center", fill=NA))) %>%
    # calc new* from sum*
    #dplyr::mutate(dplyr::across(starts_with("new"), ~ .x - dplyr::lag(.x))) %>%
    dplyr::mutate(newTested=sumTested-dplyr::lag(sumTested),
                  newConfirmed=sumConfirmed-dplyr::lag(sumConfirmed), 
                  newRecovered=sumRecovered-dplyr::lag(sumRecovered), 
                  newDeath=sumDeath-dplyr::lag(sumDeath)) %>%
    dplyr::ungroup() %>%
    
    # Beautify data frame
    dplyr::select(Date, Region, RegionID, starts_with("new"), starts_with("cur") , starts_with("sum"))
  # str(crdhit)
  
  return(crdhit)
}

# -------------------------------------------------------------------------------------------
# AGES: curHospital, curICU by Region
# -------------------------------------------------------------------------------------------
caAgesData_thi_r <- function(rollMeanDays=7, bSave=TRUE) {
  
  # "Alter in 5-Jahresgruppen","Values","Time section","Gender <2>","Number","Annotations"
  fName <- "./data/CovidFallzahlen.csv"
  col.names=c("Date","sumTested","NULL", "curHospital", "curICU", "NULL", "NULL", "RegionID", "Region")
  colClasses=c("character","numeric","NULL","numeric", "numeric","NULL","NULL","character","character")
  
  df <- read.csv(fName, header=TRUE,  sep=";", stringsAsFactors=FALSE, 
                 col.names=col.names, colClasses=colClasses) %>%
    dplyr::mutate(Date=as.POSIXct(Date, format="%d.%m.%Y"))
  
  df$Region[df$Region=="Alle"] <- "Österreich"
 
  thi_r <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(dplyr::across(starts_with("sum"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=NA))) %>%
    dplyr::mutate(dplyr::across(starts_with("cur"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=NA))) %>%
    dplyr::mutate(newTested=sumTested-dplyr::lag(sumTested)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Region, starts_with("new"), starts_with("cur"), starts_with("sum"))
   
  return(thi_r)
}

# -------------------------------------------------------------------------------------------
# AGES: sumConfirmed, sumRecovered,  sumDeath by Region
# -------------------------------------------------------------------------------------------
caAgesData_crd_r <- function(rollMeanDays=7, bSave=TRUE) {
  
  # "Alter in 5-Jahresgruppen","Values","Time section","Gender <2>","Number","Annotations"
  fName <- "./data/CovidFaelle_Timeline.csv"
  col.names=c("Date", "Region", "RegionID", "Population", "newConfirmed","sumConfirmed", "newConfirmed7","newInzidenz7", "newDeath","sumDeath", "newRecovered","sumRecovered")
  colClasses=c(rep("character",2),rep("numeric",10))
  df <- read.csv(fName, header=TRUE,  sep=";", dec=",", stringsAsFactors=FALSE, col.names=col.names, colClasses=colClasses) %>% 
    dplyr::mutate(Date=as.POSIXct(Date, format="%d.%m.%Y %H:%M:%S")) %>%
    dplyr::mutate(newInzidenz=newInzidenz7/7) %>%
    dplyr::select(-newConfirmed7, -newInzidenz7, -RegionID)
    #str(df)
  
  crd_r <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    # align right to fit with AGES way of calculating Inzidenz
    dplyr::mutate(dplyr::across(starts_with("sum"), ~ rollmean(.x, k=rollMeanDays, align="right", fill=0))) %>%
    dplyr::mutate(newConfirmed=sumConfirmed-dplyr::lag(sumConfirmed)) %>%
    dplyr::mutate(newRecovered=sumRecovered-dplyr::lag(sumRecovered)) %>%
    dplyr::mutate(newDeath=sumDeath-dplyr::lag(sumDeath)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Region, Population, starts_with("new"), starts_with("cur"), starts_with("sum"))
  #str(crd_r)
  
  return(crd_r)
}




