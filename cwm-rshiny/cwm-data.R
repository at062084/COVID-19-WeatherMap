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
# StatA: General Mortility Österreich
# -------------------------------------------------------------------------------------------
caStatAData_m_ag <- function() {
  
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
# AGES: CovidFaelle_Altersgruppe.csv: Confirmed, Recovered, Deaths by  Region, AgeGroup10, Gender
# -------------------------------------------------------------------------------------------
caAgesData_crd_rag <- function(rollMeanDays=7) {
  
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
    dplyr::mutate(Gender=factor(Gender)) %>%
    
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
    dplyr::select(Date, Region, RegionID, AgeGroup, AgeGroupID, AgeGroup2, Gender, Population, starts_with("new"), starts_with("sum"))
  
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
caAgesData_cv_a <- function(rollMeanDays=7, bSave=TRUE) {
  
  # Data seem to be published with a timestamp in the filename
  yesterday = format(now() -days(1), "%Y-%m-%d")
  yesterday = "2021-08-20"
  
  baseurl <- "https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus"
  url <- paste0(baseurl, "/Inzidenz_vollstaendig_unvollstaendig_geimpft_", yesterday,".csv")
  colnames=c("Datum","Date","AgeGroup3", "bVaccinated_2", "newConfirmed")
  df<- read.csv(url, header=FALSE, skip=1, sep=";", dec=",", stringsAsFactors=FALSE, col.names=colnames) 
  
  dcv <- df %>% 
    dplyr::select(-1) %>%
    dplyr::mutate(Date=as.POSIXct((Date)))
  str(df)
  
  if (bSave) {
    yesterday = format(now() -days(1), "%Y%m%d")
    rdaFile <- paste0("./data/COVID-19-CWM-AGES-ConfVacc-",yesterday,".rda")   
    logMsg(paste("Writing", rdaFile))
    saveRDS(dcv, file=rdaFile)  
  }
  return(dcv)
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
    dplyr::select(Date, RegionID, Region, Population, starts_with("Gruppe_")) %>%
    dplyr::rename_at(vars(starts_with("Gruppe_")), list(~ paste0("Age",substring(.,8)))) %>%
    tidyr::pivot_longer(starts_with("Age"), names_sep="_", names_to=c("AgeGroup","Gender","ShotNo"), values_to="sumVaccinated") %>%
    dplyr::filter(Gender!="D", Region!="KeineZuordnung") %>%
    dplyr::select(-Population) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age.15"), "5-14")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age.84"), ">84")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("Age"), "")) %>%
    dplyr::mutate(AgeGroup=stringr::str_replace(AgeGroup, fixed("."), "-")) %>%
    dplyr::mutate(AgeGroup =factor(AgeGroup, levels=levels(dcrd$AgeGroup), ordered=TRUE)) %>%
    tidyr::pivot_wider(names_from=ShotNo, names_prefix="sumVaccinated_", values_from=sumVaccinated)
  
  di <- df %>%
    dplyr::arrange(Region, RegionID, AgeGroup, Gender, Date) %>%
    dplyr::group_by(Region, RegionID, AgeGroup, Gender) %>%
    dplyr::mutate(sumVaccinated_1=rollmean(sumVaccinated_1, k=rollMeanDays, align="right", fill=0)) %>%
    dplyr::mutate(sumVaccinated_2=rollmean(sumVaccinated_2, k=rollMeanDays, align="right", fill=0)) %>%
    dplyr::mutate(newVaccinated_1=sumVaccinated_1-dplyr::lag(sumVaccinated_1)) %>%
    dplyr::mutate(newVaccinated_2=sumVaccinated_2-dplyr::lag(sumVaccinated_2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Region, RegionID, AgeGroup, Gender, starts_with("new"), starts_with("sum"))
  
  idx <- is.na(di$newVaccinated_1)
  di$newVaccinated_1[idx] <- 0
  idx <- is.na(di$newVaccinated_2)
  di$newVaccinated_2[idx] <- 0
  
  # str(di)
  return(di)
}

