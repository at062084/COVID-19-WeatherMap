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

# -------------------------------------------------------------------------------------------
# Globals
# prefix 'ca' is akkronym CovidAustria
# -------------------------------------------------------------------------------------------
# Mandatory smoothing of raw data (sum* -> new*, cur*)
rollMeanDays <- 7

# Optimized heuristically for AgeGroups 65-84 (most populated endagered groups)
rollMeanTimeShift <- 14
leadDaysRecovered=21
leadDaysDeath=20

# Days until Immunization
lagDaysVaccinated_1 <- 21
lagDaysVaccinated_2 <- 14

# Definition of Infection waves
caWaves <- data.frame(stringsAsFactors=FALSE,
  Wave=(c("Wave1","Wave2","Wave3","Wave4","Wave5")),
  Beg=as.POSIXct(c("2020-03-01","2020-10-15","2021-02-15","2021-10-15","2021-12-26")),
  End=as.POSIXct(c("2020-05-01","2020-12-15","2021-05-01","2021-12-26","2022-02-28")),
  Variant=(c("Original","Alpha","Beta","Delta","Omicron"))
)


# ===========================================================================================
# Data Ingestion
# ===========================================================================================
caDataIngestionPipeline <- function() {

  logMsg("Running caDataIngestionPipeline ...")
  
  caDataDownload()
  caDataPrepare()
  caDataCurate()
  
  logMsg("Done Running caDataIngestionPipeline")
  # caGitCommitPush()
  return(TRUE)
}

# -------------------------------------------------------------------------------------------
# CWM: GIT commit and push
# Only push ascii csv files to repo. All other file types are derived and can be reconstructed 
# -------------------------------------------------------------------------------------------
caGitCommitPush <- function() {

  logMsg("Executing caGitCommitPush")
  
  LogMsg("Adding files to git commit: *.csv")
  cmd <- paste0("\"add ./data/*/*.csv\"")
  system2("git", cmd)
  
  LogMsg("Commiting files to git: *.csv")
  cmd <- paste0("\"commit -m AutoCommit\"")
  system2("git", cmd)
  
  LogMsg("Pushing files to git: *.csv")
  cmd <- paste0("\"push\"")
  system2("git", cmd)
  
  return(TRUE)
}



# ===========================================================================================
# CWM: DATA CURATION
# ===========================================================================================
caDataCurate <- function() {
  
  logMsg("Running caDataCurate ...")
  
  caDataCurate_crdv_rag()
  caDataCurate_crdv_rag_ts()
  caDataCurate_tcrdzhi_r()
  caDataCurate_yz_a3()
  
  caDataStatic_CFR_2nd()
  caDataStatic_POP_2021()
  
  logMsg("Done Running caDataCurate")
  return(TRUE)
}

# -------------------------------------------------------------------------------------------
# Confirmed, Recovered, Death, Vaccinated by Region, AgeGroup, Gender
# -------------------------------------------------------------------------------------------
caDataCurate_crdv_rag <- function(bSave=TRUE) {
  
  logMsg("Executing caDataCurate_crdv_rag")

    # dv <- caPrepareDataBmsgpk_v_rag(rollMeanDays=rollMeanDays)
  rdaFile <- paste0("./data/prepared/Bmsgpk/v_rag.rda")
  logMsg(paste("Reading", rdaFile))
  dv <- readRDS(file=rdaFile)  
  
  # dcrd <- caPrepareDataAges_crd_rag(rollMeanDays=rollMeanDays)
  rdaFile <- paste0("./data/prepared/Ages/crd_rag.rda")
  logMsg(paste("Reading", rdaFile))
  dcrd <- readRDS(file=rdaFile)  
  
  dcrdv <- dcrd %>%
    dplyr::left_join(dv, by=c("Date","RegionID","Region","AgeGroup","AgeGroup3","Gender")) %>%
    dplyr::select(Date, Region, RegionID, AgeGroup, AgeGroupID, AgeGroup3, AgeGroup2, Gender, Population, starts_with("new"), starts_with("sum")) %>%
    dplyr::mutate(Gender=factor(Gender))
  
  idx <- is.na(dcrdv$newVaccinated_1)
  dcrdv$newVaccinated_1[idx] <- 0
  idx <- is.na(dcrdv$sumVaccinated_1)
  dcrdv$sumVaccinated_1[idx] <- 0
  
  idx <- is.na(dcrdv$newVaccinated_2)
  dcrdv$newVaccinated_2[idx] <- 0
  idx <- is.na(dcrdv$sumVaccinated_2)
  dcrdv$sumVaccinated_2[idx] <- 0
  
  # Add caWaves
  dcrdv$Wave=NA
  dcrdv$Variant=NA
  for (w in 1:dim(caWaves)[1]) {
    idx <- dcrdv$Date >= caWaves$Beg[w] & dcrdv$Date < caWaves$End[w]
    dcrdv$Wave[idx] <- caWaves$Wave[w]
    dcrdv$Variant[idx] <- caWaves$Variant[w]
  }
  dcrdv$Wave <- factor(dcrdv$Wave)
  dcrdv$Variant <- factor(dcrdv$Variant)
  
  if (bSave) {
    rdaFile <- "./data/curated/crdv_rag.rda"   
    logMsg(paste("Writing", rdaFile))
    saveRDS(dcrdv, file=rdaFile)  
  }
  
  return(dcrdv) 
}

# -------------------------------------------------------------------------------------------
# Confirmed, Recovered, Death, Vaccinated by Region, AgeGroup, Gender
# time shiftd for best match of newDeath with newConfirmed
# -------------------------------------------------------------------------------------------
caDataCurate_crdv_rag_ts <- function(bSave=TRUE) {
  
  logMsg("Executing caDataCurate_crdv_rag_ts")

  # Read non-timeshifted data
  rdaFile <- "./data/curated/crdv_rag.rda"  
  logMsg(paste("Reading", rdaFile))
  dcrdv <- readRDS(file=rdaFile)  
  
  dcrdv.ts <- dcrdv %>%
    dplyr::arrange(Region, AgeGroup, Gender, Date) %>%
    dplyr::group_by(Region, AgeGroup, Gender) %>%
    dplyr::mutate(newRecovered=dplyr::lead(newRecovered,leadDaysRecovered), 
                  newDeath=dplyr::lead(newDeath,leadDaysDeath)) %>%
    dplyr::ungroup()
  
  if (bSave) {
    rdaFile <- "./data/curated/crdv_rag_ts.rda" 
    logMsg(paste("Writing", rdaFile))
    saveRDS(dcrdv.ts, file=rdaFile)  
  }
  
  return(dcrdv.ts) 
}

# -------------------------------------------------------------------------------------------
# Confirmed, Recovered, Death, Vaccinated, Inzidenz, Hospital, ICU by Region
# -------------------------------------------------------------------------------------------
caDataCurate_tcrdzhi_r <- function(bSave=TRUE, rollMeanDays=7) {
  
  logMsg("Executing caDataCurate_tcrdzhi_r")

  #dcrdz <- caAgesData_crd_r(rollMeanDays=7). 
  # Also contains z = Inzidence as reported by Ages
  rdaFile <- "./data/prepared/Ages/crd_r.rda"
  logMsg(paste("Reading", rdaFile))
  dcrdz <- readRDS(file=rdaFile)  
  # str(dcrdz)
  
  #dthi <- caAgesData_thi_r(rollMeanDays=7)
  rdaFile <- "./data/prepared/Ages/thi_r.rda"
  logMsg(paste("Reading", rdaFile))
  dthi <- readRDS(file=rdaFile)  
  # str(dthi)
  
  dtcrdzhi <- dcrdz %>%
    dplyr::left_join(dthi, by=c("Date","Region")) %>%
    dplyr::select(Date, Region, Population, starts_with("new"), starts_with("cur") , starts_with("sum"))
  # str(dtcrdzhi)
  
  # Add caWaves
  dtcrdzhi$Wave=NA
  dtcrdzhi$Variant=NA
  for (w in 1:dim(caWaves)[1]) {
    idx <- dtcrdzhi$Date >= caWaves$Beg[w] & dtcrdzhi$Date < caWaves$End[w]
    dtcrdzhi$Wave[idx] <- caWaves$Wave[w]
    dtcrdzhi$Variant[idx] <- caWaves$Variant[w]
  }
  dtcrdzhi$Wave <- factor(dtcrdzhi$Wave)
  dtcrdzhi$Variant <- factor(dtcrdzhi$Variant)
  
  if (bSave) {
    rdaFile <- "./data/curated/tcrdzhi_r.rda"
    logMsg(paste("Writing", rdaFile))
    saveRDS(dtcrdzhi, file=rdaFile)  
  }
  
  return(dtcrdzhi) 
}



# -------------------------------------------------------------------------------------------
#  Inzidenz General and Symptomatic by AgeGroup3
# -------------------------------------------------------------------------------------------
caDataCurate_yz_a3 <- function (bSave=TRUE) {
  yi <- readRDS("./data/prepared/Ages/yi_a3.rda")
  zi <- readRDS("./data/prepared/Ages/zi_a3.rda")
  
  df <- rbind(yi,zi) 
  # str(df)
  
  yzi_a3 <- df %>%
    tidyr::pivot_longer(cols=starts_with("new"),  names_to=c("tmpNewConfPop", "tmpVacc", "Vaccinated"), values_to="newConfPop", names_sep="_") %>%
    dplyr::mutate(Vaccinated=factor(Vaccinated), Symptomatic=factor(Symptomatic)) %>%
    dplyr::select(-starts_with("tmp"))
  # str(yzi_a3)
  
  if (bSave) {
    rdaFile <- "./data/curated/yzi_a3.rda"
    logMsg(paste("Writing", rdaFile))
    saveRDS(yzi_a3, file=rdaFile)  
  }
  
  return(yzi_a3)
}



# -------------------------------------------------------------------------------------------
#  CFR.2nd
# -------------------------------------------------------------------------------------------
caDataStatic_CFR_2nd <- function(bSave=TRUE) {
  
  rdaFile <- "./data/curated/crdv_rag_ts.rda"
  logMsg(paste("Reading", rdaFile))
  df <- readRDS(rdaFile) %>% 
    # COVID-19 Österreich: Zweite Welle
    dplyr::filter(Date>as.POSIXct("2020-10-15"), Date<as.POSIXct("2020-12-31")) %>%
    dplyr::select(Date, Region,Population, AgeGroup, Gender, newConfirmed, newDeath) # %>%
  # dplyr::group_by(AgeGroup, Gender) %>%
  # dplyr::mutate(newConf100 = newConfirmed/max(newConfirmed), newDeath100 = newDeath/max(newDeath)) %>%
  # dplyr::ungroup()
  # ggplot(data=df%>%dplyr::filter(as.integer(AgeGroup)>5), aes(x=Date)) + 
  # geom_line(aes(y=newConf100), color="red") +  
  # geom_line(aes(y=newDeath100), color="blue") +
  # facet_grid(Gender~AgeGroup, scales="free_y") 
  # str(df)
  CFR.2nd <- df %>% 
    dplyr::group_by(Region, AgeGroup, Gender) %>%
    dplyr::summarize(.groups="drop", CFR = sum(newDeath)/sum(newConfirmed))
  # str(CFR.2nd)
  
  if (bSave) {
    rdaFile <- "./data/curated/CFR_2nd.rda"
    logMsg(paste("Writing", rdaFile))
    saveRDS(CFR.2nd, file=rdaFile)  
  }
  
  return(CFR.2nd)
}

# -------------------------------------------------------------------------------------------
#  POP
# -------------------------------------------------------------------------------------------
caDataStatic_POP_2021 <- function(bSave=TRUE) {
  
  rdaFile <- "./data/curated/crdv_rag.rda"
  logMsg(paste("Reading", rdaFile))
  df <- readRDS(rdaFile) %>% 
    dplyr::filter(Date==as.POSIXct("2021-01-01")) %>%
    dplyr::select(Region, Population, AgeGroup, AgeGroup2, AgeGroup3, Gender)
  # str(df)
  
  if (bSave) {
    rdaFile <- "./data/curated/POP_2021.rda"
    logMsg(paste("Writing", rdaFile))
    saveRDS(df, file=rdaFile)  
  }
  return(df)
}





