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
# -------------------------------------------------------------------------------------------
# Mandatory smoothing of raw data (sum* -> new*, cur*)
rollMeanDays <- 7
rollMeanTimeShift <- 14

# Optimized heuristically for AgeGroups 65-84 (most populated endagered groups)
leadDaysRecovered=21
leadDaysDeath=20

# Days until Immunization
lagDaysVaccinated_1 <- 21
lagDaysVaccinated_2 <- 14



# -------------------------------------------------------------------------------------------
# CWM: GIT commit and push
# -------------------------------------------------------------------------------------------
caGitCommitPush <- function() {
  
  LogMsg("Adding files to git commit: *.csv")
  cmd <- paste0("\"add ./data/*/*.csv\"")
  system2("git", cmd)
  
  LogMsg("Adding files to git commit: *.rda")
  cmd <- paste0("\"add ./data/*/*.rda\"")
  system2("git", cmd)
  
  LogMsg("Commiting files to git: *.csv")
  cmd <- paste0("\"commit -m AutoCommit\"")
  system2("git", cmd)
  
  LogMsg("Pushing files to git: *.csv")
  cmd <- paste0("\"push\"")
  system2("git", cmd)
  
  return(0)
}


# ===========================================================================================
# CWM: DATA CURATION
# ===========================================================================================

caDataCurate <- function() {
  
  caDataCurate_crdv_rag()
  caDataCurate_crdv_rag_ts()
  caDataCurate_tcrdzhi_r()
  
  return(TRUE)
}


# -------------------------------------------------------------------------------------------
# Confirmed, Recovered, Death, Vaccinated by Region, AgeGroup, Gender
# -------------------------------------------------------------------------------------------
caDataCurate_crdv_rag <- function(bSave=TRUE) {
  
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
caDataCurate_crdv_rag_ts <- function(bSave=TRUE, rollMeanTimeShift=14) {
  
  # Read non-timeshifted data
  rdaFile <- "./data/curated/crdv_rag.rda"  
  logMsg(paste("Reading", rdaFile))
  dcrdv <- readRDS(file=rdaFile)  
  
  dcrdv.ts <- dcrdv %>%
    dplyr::arrange(Region, AgeGroup, Gender, Date) %>%
    dplyr::group_by(Region, AgeGroup, Gender) %>%
    dplyr::mutate(dplyr::across(starts_with("new"), ~ rollmean(.x, k=rollMeanTimeShift, align="center", fill=0))) %>%
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
  
  # interactive download from original websites
  #dcrdz <- caAgesData_crd_r(rollMeanDays=7)
  rdaFile <- "./data/prepared/Ages/crd_r.rda"
  logMsg(paste("Reading", rdaFile))
  dcrdz <- readRDS(file=rdaFile)  
  
  #dthi <- caAgesData_thi_r(rollMeanDays=7)
  rdaFile <- "./data/prepared/Ages/thi_r.rda"
  logMsg(paste("Reading", rdaFile))
  dthi <- readRDS(file=rdaFile)  
  
  
  dtcrdzhi <- dcrdz %>%
    dplyr::left_join(dthi, by=c("Date","Region")) %>%
    dplyr::select(Date, Region, Population, starts_with("new"), starts_with("cur") , starts_with("sum"))
  
  if (bSave) {
    rdaFile <- "./data/curated/tcrdzhi_r.rda"
    logMsg(paste("Writing", rdaFile))
    saveRDS(dtcrdzhi, file=rdaFile)  
  }
  
  return(dtcrdzhi) 
}
