#!/usr/lib64/R/bin/Rscript

library(httr)
library(lubridate)

options(error = function() traceback(2))
#setwd("/srv/shiny-server/COVID-19-WeatherMap")
#setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny")

logDir = "./log"
logFile <- "cwm.cron.log"
logMsg <- function(msg, sessionID="__cron__") {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=stderr())
}

hostSystem <- system("hostname", intern=TRUE)
slackMsg <- function (title, msg, hostName = hostSystem) {
  url <- as.character(read.csv("../secrets/slack.txt",header=FALSE)[1,1])
  body <- list(text = paste(paste0(now()," *",title,"*: "), paste0(hostName,": ",msg)))
  r <- POST(url, content_type_json(), body = body, encode = "json")
  invisible(r)
}

  


# ===================================================================================
slackMsg(title="COVID-19-WeatherMap",msg=paste("Start cron job cron.R"))
logMsg(paste("cron: Start Running script cron.R in ", getwd()),"__cron__")
# ===================================================================================


# -----------------------------------------------------------------------------------
# Run Data Ingestion Pipeline: 
# This Reads/Writes to the new ./data/[download|prepared|curated] directory tructure
# -----------------------------------------------------------------------------------
logMsg("Start Running new style DataIngestionPipeline ...","__cron__")
# Functions for new data structure (download, prepare, curate)
logMsg("Sourcing cwmDataDownload.R","__cron__")
source("./cwmDataDownload.R")
logMsg("Sourcing cwmDataPrepare.R","__cron__")
source("./cwmDataPrepare.R")
logMsg("Sourcing cwmDataCurate.R","__cron__")
source("./cwmDataCurate.R")

caDataIngestionPipeline()
logMsg("Done Running new style DataIngestionPipeline","__cron__")

return(TRUE)
# -----------------------------------------------------------------------------------
# Old style. leave intact for now
# DownLoad data from BMSGPK
# -----------------------------------------------------------------------------------

logMsg("Start Running old style data preperation ...","__cron__")

logMsg("Sourcing fun.R","__cron__")
source("./fun.R")
logMsg("Sourcing ages.R","__cron__")
source("./ages.R")
logMsg("Sourcing bmsgpk.R","__cron__")
source("./bmsgpk.R")


# DownLoad list of 6 data files:
# "timeline-eimpfpass", "timeline-bbg", "timeline-faelle-ems", "timeline-faelle-bundeslaender", 
# "timeline-testungen-apotheken-betriebe", "timeline-testungen-schulen"
logMsg("DISABLED: Downloading data from BMSGPK Website ...","__cron__")
#bd <- caBmsgpkDownload ()

# https://info.gesundheitsministerium.gv.at/data/timeline-faelle-bundeslaender.csv
logMsg("Downloading timeline-faelle-bundeslaender data from BMSGPK ...","__cron__")
dd <- caBmsgpkRead_tfb()

# "https://info.gesundheitsministerium.gv.at/data/timeline-faelle-ems.csv"
logMsg("Downloading timeline-faelle-ems data from BMSGPK ...","__cron__")
de <- caBmsgpkRead_tfe()

# bmsgpk Webpage scraper
logMsg("DISABLED: Scraping BMSGPK Website ...","__cron__")
#dm <- caBmsgpkUpdateDashboard()


# -----------------------------------------------------------------------------------
# Download data from AGES
# -----------------------------------------------------------------------------------

# "https://www.ages.at/themen/krankheitserreger/coronavirus/sars-cov-2-varianten-in-oesterreich"
logMsg("DISABLED: Scraping Mutations data from AGES ...","__cron__")
# dm <-caAgesRead_Mutations()


# Download zip File from AGES. Contains 7 Files 
logMsg("DISABLED: DownLoading zip File from AGES ...","__cron__")
# dz <- caAgesData_zipFile()



# Source: http://covid19-dashboard.ages.at/data/CovidFallzahlen.csv
# Input:  http://covid19-dashboard.ages.at/data/CovidFallzahlen.csv
# Output: ./data/CovidFallzahlen.csv
# Output: ./data/CovidFallzahlen.rda
logMsg("DownLoading CovidFallzahlen data from AGES ...","__cron__")
dt <- caAgesRead_cfz()

# Source: http://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv
# Input:  ./data/download/Ages/CovidFaelle_Timeline.csv
# Output: ./data/CovidFaelle_Timeline.rda
logMsg("Downloading CovidFaelle_Timeline data from AGES ...","__cron__")
dc <- caAgesRead_cftl()

# Source: http://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv
# Input:  ./data/CovidFaelle_Timeline_GKZ.csv
# Output: ./data/CovidFaelle_Timeline_GKZ.rda
# Output: ./data/COVID-19-CWM-AGES-Counties-Curated.rda
# >>> cwmCountiesFile <- "./data/COVID-19-CWM-AGES-Counties-Curated.rda"
logMsg("Downloading CovidFaelle_Timeline_GKZ data from AGES ...","__cron__")
db <- caAgesRead_cfGKZtl()

# Input: dc
# Input:  ./data/COVID-19-CWM-AGES-TestedProcessed.rda 
# Output: ./data/COVID-19-CWM-AGES-TestedProcessed.rda (incremental, ads daily data !!!)
# Output: ./data/COVID-19-CWM-AGES-TestedEvaluated.rda
# >>> cwmTestedEvaluatedFile <- "./data/COVID-19-CWM-AGES-TestedEvaluated.rda"
logMsg("Creating history of AGES reports of Confirmed cases ...", "__cron__")
dq <- caAgesConfHistory(dc %>% 
                          dplyr::select(Date, RegionID, Region, newConfirmed) %>% 
                          dplyr::filter(Date> max(Date)-days(nSettleDays)))

# Construct working data frame 
# Input:  ./data/CovidFaelle_Timeline.rda
# Input:  ./data/CovidFallzahlen.rda
# Input:  ./data/COVID-19-CWM-AGES-TestedEvaluated.rda
# Output: ./data/COVID-19-CWM-AGES-States-Curated.rda
# >>> cwmStatesFile <- "./data/COVID-19-CWM-AGES-States-Curated.rda"
logMsg("Start Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features ...","__cron__")
df <- caAgesRead_tlrm(cftlFile="./data/CovidFaelle_Timeline.rda", cfzFile="./data/CovidFallzahlen.rda", bPlot=FALSE, 
                      nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                      bResiduals=TRUE, dResFirst=as.Date("2020-07-27"), dResLast=as.Date("2020-11-16"), bShiftDown=TRUE,
                      bPredict=TRUE, nPolyDays=7, nPoly=2,
                      bEstimate=FALSE, bCompleteCases=FALSE)
logMsg("Done Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features","__cron__")

logMsg("Done Running old style data preperation ...","__cron__")


# ===================================================================================
slackMsg(title="COVID-19-WeatherMap",msg=paste("Complete cron job cron.R"))
logMsg(paste("cron: Done Running script cron.R in ", getwd()),"__cron__")
# ===================================================================================

